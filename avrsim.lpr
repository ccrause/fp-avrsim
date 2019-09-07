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

type
  TRunnerState = (rsBreak, rsRunning);

  TAVRRunner = class;

  { TBreakableAVR }

  TBreakableAVR = class(TAVR)
  private
    FOnBreak: TBreakNotify;
    FOnHalt: TBreakNotify;
    fRunner: TAVRRunner;
  protected
    // reverse communication channel to gdb server
    procedure BreakHit; override;
    procedure SignalHalt;
  public
    constructor Create();

    property Runner: TAVRRunner read fRunner write fRunner;
    property OnBreak: TBreakNotify read FOnBreak write FOnBreak;
    property OnHalt: TBreakNotify read FOnHalt write FOnHalt;
  end;

  TAVRRunner = class(TThread)
  private
    fAvr: TAvr;
    fState: TRunnerState;
    fLock: TCriticalSection;
    fBreakpoints: array of longword;
  protected
    procedure Execute; override;
  public
    procedure AddBreak(AAddr: longword);
    procedure RemoveBreak(AAddr: longword);

    function DoBreak: TRunnerState;
    procedure UnBreak(AOldState: TRunnerState);
    procedure SingleStep;
    procedure Continue;

    constructor Create(AAvr: TAvr);
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
    procedure SetBreakHit(AEvent: TBreakNotify);
    procedure SetHaltProc(AEvent: TBreakNotify);

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

  procedure TBreakableAVR.BreakHit;
    begin
      inherited BreakHit;

      writeln('Hit breakpoint at ',hexstr(fRunner.fAvr.PC,4));

      fRunner.DoBreak;
      if assigned(OnBreak) then
        OnBreak;
    end;

  procedure TBreakableAVR.SignalHalt;
    begin
      if Assigned(FOnHalt) then
        OnHalt;
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
    begin
      while not Terminated do
      begin
        fLock.Enter;
        if fState = rsBreak then
          Sleep(1)
        else
        begin
          fAvr.Step(1);
          i := high(fBreakpoints);
          while i > -1 do
          begin
            if fBreakpoints[i] = fAvr.PC then
            begin
              TBreakableAVR(fAvr).BreakHit;
              i := -1;
              fState := rsBreak;
            end
            else
              dec(i);
          end;
          if fAvr.DoExit then
            TBreakableAVR(fAvr).SignalHalt;
        end;
        fLock.Leave;
      end;
    end;

  procedure TAVRRunner.AddBreak(AAddr: longword);
    var
      i: integer;
    begin
      for i := low(fBreakpoints) to high(fBreakpoints) do
        if fBreakpoints[i] = AAddr then exit; // keep only one copy of a BP address
      SetLength(fBreakpoints, Length(fBreakpoints)+1);
      fBreakpoints[Length(fBreakpoints)-1] := AAddr;
    end;

  procedure TAVRRunner.RemoveBreak(AAddr: longword);
    var
      i: integer;
    begin
      i := low(fBreakpoints);
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
    end;

  function TAVRRunner.DoBreak: TRunnerState;
    begin
      fLock.Enter;

      Result := fState;

      if fState <> rsBreak then
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

  function TDebugAVR.ReadByte(AAddr: longword): byte;
    begin
      // Assume $800000+ indicates RAM/IOREGs
      if (AAddr <= $FFFF) then
        result := fAVR.Flash[AAddr]
      else if (AAddr > VMA_RAM) and (AAddr < (VMA_RAM + $FFFF)) then
        result := fAVR.RAM[(AAddr-VMA_RAM)]
      else
        result:=0;
    end;

  procedure TDebugAVR.WriteByte(AAddr: longword; val: byte);
    begin
      if (AAddr <= $FFFF) then
        fAVR.Flash[AAddr-VMA_FLASH]:=val
      else if (AAddr > VMA_RAM) and (AAddr < (VMA_RAM + $FFFF)) then
        fAVR.RAM[AAddr-VMA_RAM]:=val
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
    result := 'T';
    old := fRunner.DoBreak;
    case old of
      rsBreak: result := result + '05hwbreak';
      //srBreakPoint: s := result + '05swbreak';
    end;

    //// Register file
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
      //AVal:=fAVR.get;
      result:=false;
    end;

  procedure TDebugAVR.RemoveBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    begin
      fRunner.RemoveBreak(AAddr);
    end;

  procedure TDebugAVR.Reset;
    begin
    end;

  procedure TDebugAVR.SetBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    begin
      fRunner.AddBreak(AAddr);
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
    begin
      result:=false;
    end;

  procedure TDebugAVR.SetBreakHit(AEvent: TBreakNotify);
    begin
      TBreakableAVR(fAVR).OnBreak:=AEvent;
    end;

  procedure TDebugAVR.SetHaltProc(AEvent: TBreakNotify);
    begin
      TBreakableAVR(fAVR).OnHalt:=AEvent;
    end;

  function TDebugAVR.SupportedOptions: string;
  begin
    result := 'hwbreak+;swbreak+;qXfer:memory-map:read+;PacketSize=256;';
  end;

  function TDebugAVR.BreakpointHit: boolean;
  begin
    result := fRunner.fState = rsBreak;
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

procedure InvalidCommandline;
  begin
    writeln('Simulator: Invalid command line');
    writeln('Usage: avrsim [-d<port>] [-6] [-v] <bin-file>');
    halt(-100001);
  end;

begin
  RunInDebugger:=false;
  SimulateAVR6:=false;
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

        fHandler.AVR.AVR6:=SimulateAVR6;

        try
          fHandler.AVR.LoadFlashBinary(Filename);

          fserv := TGDBServerListener.Create(strtoint(port), fHandler);
          try
            fserv.Start;

            fserv.Waitfor;

            ExitCode := fHandler.AVR.ExitCode;
            if Verbose then
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

        x.AVR6:=SimulateAVR6;

        try
          x.LoadFlashBinary(Filename);

          while not x.DoExit do
             x.Step(10);

          ExitCode := x.ExitCode;
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
