{$mode objfpc}
{$H+}
program avrsim;

uses {$ifdef UNIX}
  cthreads, {$endif UNIX}
  Classes,
  SysUtils,
  syncobjs,
  avr,
  gdbserver;

const
  VMA_FLASH:longword	 =  $00000000;
  VMA_RAM:longword		 =  $00800000;
  VMA_EEPROM:longword  =  $00810000;

type
  TRunnerState = (rsBreak, rsWatchBreak, rsRunning);

  TAVRRunner = class;

  { TBreakableAVR }

  TBreakableAVR = class(TAVR)
  private
    FOnBreak: TNotification;
    FOnHalt: TNotification;
    fRunner: TAVRRunner;
  protected
    // reverse communication channel to gdb server
    procedure Notify;
  public
    constructor Create();

    property Runner: TAVRRunner read fRunner write fRunner;
    property OnNotify: TNotification read FOnBreak write FOnBreak;
    property OnHalt: TNotification read FOnHalt write FOnHalt;
  end;

  { TAVRRunner }

  TAVRRunner = class(TThread)
  private
    fAvr: TAvr;
    fState: TRunnerState;
    fLock: TCriticalSection;
    fBreakpoints: array of longword;
  protected
    procedure Execute; override;
  public
    procedure AddBreak(typ: TBreakpointType; AAddr: int64; AKind: longint);
    procedure RemoveBreak(typ: TBreakpointType; AAddr: int64; AKind: longint);

    function DoBreak: TRunnerState;
    procedure UnBreak(AOldState: TRunnerState);
    procedure SingleStep;
    procedure Continue;

    constructor Create(AAvr: TAvr);
    destructor Destroy; override;
  end;

  { TDebugAVR }

  TDebugAVR = class(TInterfacedObject, IGDBHandler)
  private
    fAVR: TAvr;
    fRunner: TAVRRunner;
    fMemoryMap: string;

    function ReadByte(AAddr: longword): byte;
    procedure WriteByte(AAddr: longword; val: byte);
  public
    function Continue: TStopReply;
    procedure DoBreak;
    function GetRegisterString: string;
    function GetStatus: TStopReply;
    function GetStatusStr: string;
    function Read(var ABuffer; AAddr, ALen: int64): boolean;
    function ReadReg(AAddr: int64; var AVal: int64): boolean;
    procedure RemoveBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    procedure Reset;
    procedure SetBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    function SingleStep: TStopReply;
    procedure StepCycles(ACycles: longint);
    function Write(const ABuffer; AAddr, ALen: int64): boolean;
    function WriteReg(AAddr, AVal: int64): boolean;

    // Reverse communication with gdb server
    procedure SendNotification(AEvent: TNotification);

    function SupportedOptions: string;
    function BreakpointHit: boolean;
    function DoHalt: boolean;
    function memoryMap(offset, len: dword): string;
    procedure eraseFlash(addr, len: dword);
    constructor Create;
    destructor Destroy; override;

    property AVR: TAvr read fAVR;
  end;

var
  fserv: TGDBServerListener;
  fHandler: TDebugAVR;
  x: TAvr;
  port: String;

  procedure TBreakableAVR.Notify;
    begin
      writeln('Hit breakpoint at ',hexstr(fRunner.fAvr.PC,4));

      fRunner.DoBreak;
      if assigned(OnNotify) then
        OnNotify;
    end;

  constructor TBreakableAVR.Create();
    begin
      inherited Create;
      FOnBreak:=nil;
      fRunner:=nil;
    end;

  procedure TAVRRunner.Execute;
    var
      i: integer;
      tmpState: TRunnerState;
    begin
      while not Terminated do
      begin
        fLock.Enter;
        tmpState := fState;
        fLock.Leave;
        if tmpState <> rsRunning then
          Sleep(1)
        else
        begin
          fAvr.Step(1);
          if fAvr.DataWatchBreak then
          begin
            tmpState := rsWatchBreak;
            fAvr.clearDataWatchBreak;
          end
          else
          begin
            i := high(fBreakpoints);
            while i > -1 do
            begin
              if fBreakpoints[i] = fAvr.PC then
              begin
                i := -1;
                tmpState := rsBreak;
              end
              else
                dec(i);
            end;
          end;

          if fAvr.DoExit then
          begin
            tmpState := rsBreak;
          end;

          if tmpState <> rsRunning then
          begin
            fLock.Enter;
            fState := tmpState;
            fLock.Leave;
            TBreakableAVR(fAvr).Notify;
          end;
        end;
      end;
    end;

    procedure TAVRRunner.AddBreak(typ: TBreakpointType; AAddr: int64;
      AKind: longint);
    var
      i: integer;
    begin
      if typ in [btMemBreak, btHWBreak] then
      begin
        for i := low(fBreakpoints) to high(fBreakpoints) do
          if fBreakpoints[i] = AAddr then exit; // keep only one copy of a BP address
        SetLength(fBreakpoints, Length(fBreakpoints)+1);
        fBreakpoints[Length(fBreakpoints)-1] := AAddr;
      end
      else
      begin
        AAddr := AAddr and $FFFFF;  // mask out data space address
        if (typ = btRWatch) or (typ = btAWatch) then
          fAvr.addDataWatch(LongWord(AAddr), LongWord(AKind), true, ord(typ));

        if (typ = btWWatch) or (typ = btAWatch) then
          fAvr.addDataWatch(LongWord(AAddr), LongWord(AKind), false, ord(typ));
      end;
    end;

    procedure TAVRRunner.RemoveBreak(typ: TBreakpointType; AAddr: int64;
      AKind: longint);
    var
      i: integer;
    begin
      if typ in [btMemBreak, btHWBreak] then
      begin
        i := low(fBreakpoints);
        if i = 0 then exit;

        while (fBreakpoints[i] <> AAddr) and (i < high(fBreakpoints)) do
          inc(i);

        if (AAddr = fBreakpoints[i]) then
        begin
          if i < high(fBreakpoints) then
          begin
            while i < high(fBreakpoints)-1 do
            begin
              fBreakpoints[i] := fBreakpoints[i+1];
              inc(i);
            end;
          end;
          SetLength(fBreakpoints, Length(fBreakpoints)-1);
        end;
      end
      else
      begin
        AAddr := AAddr and $FFFFF;  // mask out data space address
        if (typ = btRWatch) or (typ = btAWatch) then
          fAvr.removeDataWatch(LongWord(AAddr), LongWord(AKind), true, ord(typ));

        if (typ = btWWatch) or (typ = btAWatch) then
          fAvr.removeDataWatch(LongWord(AAddr), LongWord(AKind), false, ord(typ));
      end;
    end;

  function TAVRRunner.DoBreak: TRunnerState;
    begin
      fLock.Enter;

      Result := fState;

      if fState = rsRunning then
        fState := rsBreak;
      fLock.Leave;
    end;

  procedure TAVRRunner.UnBreak(AOldState: TRunnerState);
    begin
      fLock.Enter;
      fState := AOldState;
      fLock.Leave;
    end;

  procedure TAVRRunner.SingleStep;
    begin
      fLock.Enter;
      DoBreak;
      fAvr.Step(1);
      fLock.Leave;
    end;

  procedure TAVRRunner.Continue;
    begin
      fLock.Enter;
      if fState <> rsRunning then
        fState := rsRunning;
      fLock.Leave;
    end;

  constructor TAVRRunner.Create(AAvr: TAvr);
    begin
      inherited Create(True);
      fState := rsBreak;
      fAvr := AAvr;

      fLock := TCriticalSection.Create;
    end;

  destructor TAVRRunner.Destroy;
  begin
    fLock.Free;
    inherited;
  end;

  function TDebugAVR.ReadByte(AAddr: longword): byte;
    begin
      // Assume $800000+ indicates RAM/IOREGs
      if (AAddr <= $FFFF) then
        result := fAVR.Flash[AAddr]
      else if (AAddr >= VMA_RAM) and (AAddr <= (VMA_RAM + $FFFF)) then
        result := fAVR.RAM[(AAddr-VMA_RAM)]
      else if (AAddr >= VMA_EEPROM) and (AAddr < (VMA_EEPROM + $FFFF)) then
        result := fAVR.readEEPROM(AAddr - VMA_EEPROM)
      else
        result:=0;
    end;

  procedure TDebugAVR.WriteByte(AAddr: longword; val: byte);
    begin
      if (AAddr <= $FFFF) then
        fAVR.Flash[AAddr-VMA_FLASH]:=val
      else if (AAddr >= VMA_RAM) and (AAddr < (VMA_RAM + $FFFF)) then
        fAVR.RAM[AAddr-VMA_RAM]:=val
      else if (AAddr >= VMA_EEPROM) and (AAddr < (VMA_EEPROM + $FFFF)) then
        fAVR.writeEEPROM(val, AAddr-VMA_EEPROM)
      else
        WriteLn('Writing byte beyond RAM');
    end;

  function TDebugAVR.Continue: TStopReply;
    begin
      result:=srOK;
      fRunner.Continue;
    end;

  procedure TDebugAVR.DoBreak;
    begin
      fRunner.DoBreak;
    end;

  function TDebugAVR.GetRegisterString: string;
    var
      old: TRunnerState;
      i: Integer;
    begin
      old:=fRunner.DoBreak;

      result:='';
      for i := 0 to 31 do
        result:=result+hexstr(fAVR.RAM[i],2);

      // Convert hex representation to lowercase, else gdb thinks reply starting with E is error
      result := LowerCase(result);
      result:=result+hexstr(fAVR.SREG,2); // SReg

      result:=result+hexstr(fAVR.StackPointer and $FF,2); // SP
      result:=result+hexstr((fAVR.StackPointer shr 8) and $FF,2);

      result:=result+hexstr(fAVR.PC and $FF,2); // SP
      result:=result+hexstr((fAVR.PC shr 8) and $FF,2);
      result:=result+hexstr((fAVR.PC shr 16) and $FF,2);
      result:=result+hexstr((fAVR.PC shr 24) and $FF,2);

      fRunner.UnBreak(old);
    end;

  function TDebugAVR.GetStatus: TStopReply;
    var
      old: TRunnerState;
    begin
      result:=srOK;

      old:=fRunner.DoBreak;

      if old=rsBreak then
        result:=srSigInt;

      fRunner.UnBreak(old);
    end;

  function TDebugAVR.GetStatusStr: string;
  var
    old: TRunnerState;
    i: Integer;
  begin
    result := 'T05';
    old := fRunner.DoBreak;
    if old = rsBreak then
      result := result + 'hwbreak:;'
    else if old = rsWatchBreak then
    begin
      case TBreakpointType(fAVR.DataWatchType) of
        btWWatch: result := result + 'watch';
        btRWatch: result := result + 'rwatch';
        btAWatch: result := result + 'awatch';
        else
          ;
      end;
      result := result + ':' + hexStr(fAVR.DataWatchAddress + $800000, 6) + ';';
    end;

    // Register file
    for i := 0 to 31 do
      result := result + hexstr(i, 2) + ':' + hexstr(fAVR.RAM[i],2) + ';';

    // SREG
    result := result + '20:' + hexstr(fAVR.SREG,2) + ';';
    // SP
    result := result + '21:' + hexstr(fAVR.StackPointer and $FF,2)
              + hexstr((fAVR.StackPointer shr 8) and $FF,2) + ';';
    // PC
    result := result + '22:' + hexstr(fAVR.PC and $FF,2)
              + hexstr((fAVR.PC shr 8) and $FF,2)
              + hexstr((fAVR.PC shr 16) and $FF,2)
              + hexstr((fAVR.PC shr 24) and $FF,2)
              + ';';
    fRunner.UnBreak(old);
  end;

  function TDebugAVR.Read(var ABuffer; AAddr, ALen: int64): boolean;
    var
      old: TRunnerState;
      i: Integer;
    begin
      old:=fRunner.DoBreak;

      result:=true;
      for i := 0 to ALen-1 do
        pbyte(@Abuffer)[i]:=ReadByte(AAddr+i);

      fRunner.UnBreak(old);
    end;

  function TDebugAVR.ReadReg(AAddr: int64; var AVal: int64): boolean;
    begin
      result := true;
      if (AAddr > 0) and (AAddr < 32) then // CPU registers
        AVal := fAVR.RAM[AAddr]
      else if AAddr = 32 then              // SREG
        AVal := fAVR.SREG
      // Anything larger than a byte is swapped to big endian
      // so that the resulting IntToHex conversion will give a little endian hex string
      else if AAddr = 33 then              // SP
        AVal := NtoBE(fAVR.StackPointer)
      else if AAddr = 34 then              // PC
        AVal := NtoBE(fAVR.PC)
      else
        result := false;
    end;

  procedure TDebugAVR.RemoveBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    begin
      fRunner.RemoveBreak(AType, AAddr, AKind);
    end;

  procedure TDebugAVR.Reset;
    begin
    end;

  procedure TDebugAVR.SetBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    begin
      fRunner.AddBreak(AType, AAddr, AKind);
    end;

  function TDebugAVR.SingleStep: TStopReply;
    begin
      fRunner.SingleStep;
      Result:=srSigInt;
    end;

  procedure TDebugAVR.StepCycles(ACycles: longint);
    var
      old: TRunnerState;
    begin
      old:=fRunner.DoBreak;

      fAVR.Step(ACycles);

      fRunner.UnBreak(old);
    end;

  function TDebugAVR.Write(const ABuffer; AAddr, ALen: int64): boolean;
    var
      old: TRunnerState;
      i: Integer;
    begin
      old:=fRunner.DoBreak;

      writeln('Writing ', alen, ' bytes to ',hexstr(AAddr,4));

      result:=true;
      for i := 0 to ALen-1 do
        WriteByte(AAddr+i, pbyte(@Abuffer)[i]);

      fRunner.UnBreak(old);
    end;

  function TDebugAVR.WriteReg(AAddr, AVal: int64): boolean;
    var
      tmp: qword;
    begin
      result := true;
      if (AAddr > 0) and (AAddr < 32) then // CPU registers
        fAVR.RAM[AAddr] := byte(AVal)
      else if AAddr = 32 then              // SREG
        fAVR.SREG := byte(AVal)
      else if AAddr = 33 then              // SPL, only modify low byte of SP
      begin
        tmp := fAVR.StackPointer;
        tmp := (tmp and $FF00) + byte(AVal);
        fAVR.StackPointer := tmp;
      end
      else if AAddr = 34 then              // SPH
      begin
        tmp := fAVR.StackPointer;
        tmp := (tmp and $00FF) + (byte(AVal) shl 8);
        fAVR.StackPointer := tmp;
      end
      else if AAddr = 35 then             // PC0
      begin
        tmp := fAVR.PC;
        tmp := (tmp and $FFFFFF00) + byte(AVal);
        fAVR.PC := tmp;
      end
      else if AAddr = 36 then             // PC1
      begin
        tmp := fAVR.PC;
        tmp := (tmp and $FFFF00FF) + (byte(AVal) shl 8);
        fAVR.PC := tmp;
      end
      else if AAddr = 37 then             // PC2
      begin
        tmp := fAVR.PC;
        tmp := (tmp and $FF00FFFF) + (byte(AVal) shl 16);
        fAVR.PC := tmp;
      end
      else if AAddr = 38 then             // PC3
      begin
        tmp := fAVR.PC;
        tmp := (tmp and $00FFFFFF) + (byte(AVal) shl 24);
        fAVR.PC := tmp;
      end
      else
        result := false;
    end;

  procedure TDebugAVR.SendNotification(AEvent: TNotification);
    begin
      TBreakableAVR(fAVR).OnNotify:=AEvent;
    end;

  function TDebugAVR.SupportedOptions: string;
  begin
    result := 'hwbreak+;swbreak+;qXfer:memory-map:read+;PacketSize=256;';
  end;

  function TDebugAVR.BreakpointHit: boolean;
  begin
    result := fRunner.fState <> rsRunning;
  end;

  function TDebugAVR.DoHalt: boolean;
  begin
    result := fAVR.DoExit;
  end;

  function TDebugAVR.memoryMap(offset, len: dword): string;
  begin
    // If requested length < memory map size, prepend "m", else "l"
    if (offset + len) < length(FMemoryMap) then
      result := 'm' + copy(FMemoryMap, 1+offset, len)
    else
      result := 'l' + copy(FMemoryMap, 1+offset, len);
  end;

  procedure TDebugAVR.eraseFlash(addr, len: dword);
  var
    buf: TBytes;
  begin
    SetLength(buf, len);
    FillChar(buf[0], length(buf), $FF);
    fAVR.WriteFlash(buf[0], length(buf), addr);
  end;

  constructor TDebugAVR.Create;
    begin
      inherited Create;

      fAVR := TBreakableAVR.Create();

      fRunner := TAVRRunner.Create(fAVR);
      TBreakableAVR(fAVR).Runner:=fRunner;

      // Assume avrsim type layout - 32 registers & 224 IO registers, so first RAM address starts at $800100
      FMemoryMap := format('<memory-map> <memory type="ram" start="0x800000" length="0x%.4x"/> <memory type="flash" start="0" length="0x%.4x">  <property name="blocksize">0x100</property> </memory></memory-map>',
                      [256 + fAVR.ramSize, fAVR.flashSize]);

      fRunner.DoBreak;
      fRunner.Start;
    end;

  destructor TDebugAVR.Destroy;
    begin
      fRunner.DoBreak;
      fRunner.Terminate;
      fRunner.WaitFor;
      fRunner.Free;

      fAVR.Free;

      inherited Destroy;
    end;

var
  Filename : String;
  RunInDebugger,
  SimulateAVR6, Verbose: Boolean;
  i : Integer;
  setRamStart: boolean;
  RamStart: string;

procedure InvalidCommandline;
  begin
    writeln('Simulator: Invalid command line');
    writeln('Usage: avrsim [-d<port>] [-6] [-v] [-s<startAddress>] <bin-file>');
    halt(-100001);
  end;

begin
  RunInDebugger:=false;
  SimulateAVR6:=false;
  setRamStart := false;
  for i:=1 to ParamCount do
    case Copy(ParamStr(i),1,2) of
      '-d':
        begin
          RunInDebugger:=true;
          port:=ParamStr(i);
          delete(port,1,2);
        end;
      '-6':
        SimulateAVR6:=true;
      '-v':
        Verbose:=true;
      '-s':
        begin
          setRamStart := true;
          RamStart := ParamStr(i);
          delete(RamStart, 1, 2);
        end
      else
        begin
          if i=Paramcount then
            begin
              if not FileExists(ParamStr(i)) then
                begin
                  if not FileExists(ParamStr(i)+'.bin') then
                    begin
                      writeln('Simulator: File not found: ',ParamStr(Paramcount));
                      halt(-100000);
                    end
                  else
                    Filename:=ParamStr(Paramcount)+'.bin';
                end
              else
                Filename:=ParamStr(Paramcount);
            end
          else
            InvalidCommandline;
        end;
    end;

  try
    if RunInDebugger then
      begin
        fHandler:=TDebugAVR.Create;
        if setRamStart then
          fHandler.AVR.ramStart := StrToInt(RamStart);
        fHandler.AVR.AVR6:=SimulateAVR6;

        try
          fHandler.AVR.LoadFlashBinary(Filename);

          fserv := TGDBServerListener.Create(strtoint(port), fHandler);
          try
            fserv.Start;

            fserv.Waitfor;

            ExitCode := fHandler.AVR.ExitCode;
            WriteLn('Exit code: ', ExitCode);
          finally
            fserv.Free;
          end;
        finally;
          //fHandler.Free;  // This is automatically freed when fserv is destroyed because fserv hold an interface reference which gets decremented
        end;
      end
    else
      begin
        x := TAvr.Create;
        if setRamStart then
          x.ramStart := StrToInt(RamStart);
        x.AVR6:=SimulateAVR6;

        try
          x.LoadFlashBinary(Filename);

          while not x.DoExit do
             x.Step(10);

          ExitCode := x.ExitCode;
          if Verbose then
            WriteLn('Exit code: ', ExitCode);

        finally
          x.Free;
        end;
      end;
  except
    on e: Exception do
      begin
        writeln('Simulator: Exception happened!');
        writeln(e.Message);
        halt(-100002);
      end;
  end;
end.
