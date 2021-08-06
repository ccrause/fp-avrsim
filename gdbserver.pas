unit gdbserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, ssockets;

type
  TGDBServerListener = class;

  TBreakpointType = (btMemBreak, btHWBreak, btWWatch, btRWatch, btAWatch);
  TStopReply = (srOK, srSigInt, srBreakPoint);

  TNotification = procedure of object;

  IGDBHandler = interface
    function GetStatus: TStopReply;
    function GetStatusStr: string;
    function Continue: TStopReply;
    function SingleStep: TStopReply;
    procedure DoBreak;
    procedure DoCtrlC;
    procedure StepCycles(ACycles: longint);

    function Read(var ABuffer; AAddr,ALen: int64): boolean;
    function Write(const ABuffer; AAddr,ALen: int64): boolean;
    function ReadReg(AAddr: int64; var AVal: int64): boolean;
    function WriteReg(AAddr, AVal: int64): boolean;
    function GetRegisterString: string;

    procedure SendNotification(AEvent: TNotification);
    function SupportedOptions: string;

    procedure Reset;
    procedure SetBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    procedure RemoveBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    function BreakpointHit: boolean;
    function DoHalt: boolean;
    function MemoryMap(offset, len: dword): string;
    procedure eraseFlash(addr, len: dword);
  end;

  { TGDBServer }

  TGDBServer = class(TThread)
  private
    fOwner: TGDBServerListener;
    fReadBuffer: TStream;
    fHandler: IGDBHandler;
    fSock: TSocketStream;
    fNewNotification: boolean;
    fRunning: boolean;

    procedure fNotifyEvent;
    procedure BreakHit;
    procedure ReadPacket;

    function CalcChecksum(const AStr: string): byte;
    procedure Escape(var AStr: string);
    procedure Respond(AStr: string);
    procedure Respond(ATyp: TStopReply);
    procedure HexEncodeRespond(AStr: string);

    function HandlePacket(APacket: string): boolean;

    function fTcpReadChar: char;
    function HexDecode(hexCode: string): string;
    procedure HandleRcmd(req: string);
  protected
    procedure Execute; override;
  public
    procedure DoExit;
    constructor Create(AOwner: TGDBServerListener; ASock: TSocketStream; AHandler: IGDBHandler);
  end;

  TGDBServerListener = class(TThread)
  private
    fPort: longint;
    fServer: TInetServer;
    fHandler: IGDBHandler;
    fClients: TList;

    procedure Connect(Sender: TObject; Data: TSocketStream);
    procedure Idle(Sender: TObject);

    procedure FQueryConnect(Sender: TObject; ASocket: LongInt; var doaccept: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create(APort: longint; AHandler: IGDBHandler);
    destructor Destroy; override;
  end;

var
  debugPrint: boolean = false;

implementation

uses
  {$IFNDEF WINDOWS}BaseUnix, sockets;
  {$ELSE}winsock2, windows;
  {$ENDIF}



procedure dbgPrint(const c: char);
begin
  if debugPrint then write(c);
end;

procedure dbgPrintLn(const s: string);
var
  i: integer;
begin
  if debugPrint then
  begin
    i := 1;
    while i <= length(s) do
    begin
      if (ord(s[i]) > 31) and (ord(s[i]) < 127) then
        write(s[i])
      else
        write('.');

      inc(i);
    end;
    WriteLn();
  end;
end;

function AddrToString(Addr: TSockAddr): String;
begin
  {$IFNDEF WINDOWS}
  Result := NetAddrToStr(Addr.sin_addr);
  {$ELSE}
  Result := inet_ntoa(Addr.sin_addr);
  {$ENDIF}

  Result := Result  + ':' + IntToStr(Addr.sin_port);
end;

procedure TGDBServer.ReadPacket;
  var
    c: char;
    cksum, calcSum: byte;
    s: String;
  begin
    repeat
      // Check if handler needs servicing before new TCP data request starts
      if fNewNotification then exit;
      c := fTcpReadChar;

      if c=#$3 then
        begin
          dbgPrintLn('-> <Ctrl-C>');
          fHandler.DoCtrlC;
          Respond(fHandler.GetStatusStr);
        end;
    until (c='$') or terminated;

    if terminated then exit;

    c:= fTcpReadChar;
    s:='';
    calcSum:=0;
    while c<>'#' do
      begin
        calcSum := byte(calcSum+byte(c));

        if c=#$7D then
          begin
            c:= fTcpReadChar;

            // Something weird happened
            if c='#' then
              break;

            calcSum:=byte(calcSum+byte(c));

            c:=char(byte(c) xor $20);
          end;

        s:=s+c;
        c:=fTcpReadChar;
      end;
    dbgPrintLn('-> ' + s);
    cksum:=strtoint('$'+char(fTcpReadChar)+char(fTcpReadChar));

    if calcSum=cksum then
      begin
        fSock.WriteByte(byte('+'));  dbgPrintLn('<- +');
        if not HandlePacket(s) then
        begin
          Respond('');
        end;
      end
    else
    begin
      fSock.WriteByte(byte('-'));
      dbgPrintLn('<- -');
    end;
  end;

function TGDBServer.CalcChecksum(const AStr: string): byte;
  var
    c: byte;
    i: longint;
  begin
    c:=0;
    for i := 1 to length(astr) do
      c:=byte(c+byte(astr[i]));
    result:=c;
  end;

procedure TGDBServer.Escape(var AStr: string);
  const
    EscapeChars = [#$7D,#$23,#$24,#$2A];
  var
    res: String;
    i: longint;
    c: Char;
  begin
    res:='';
    for i := 1 to length(astr) do
      begin
        c:=astr[i];
        if c in EscapeChars then
          res:=res+#$7D+char(byte(c) xor $20)
        else
          res:=res+c;
      end;
  end;

procedure TGDBServer.Respond(AStr: string);
  var
    ckSum: Byte;
  begin
    Escape(AStr);
    ckSum:=CalcChecksum(AStr);

    AStr:='$'+AStr+'#'+hexStr(cksum,2);

    fSock.Write(astr[1], length(astr));
    dbgPrintLn('<- ' + AStr);
  end;

procedure TGDBServer.Respond(ATyp: TStopReply);
  begin
    case ATyp of
      srOK: Respond('S05');
      srBreakPoint: Respond('S03');
      srSigInt: Respond('S02');
    end;
  end;

procedure TGDBServer.HexEncodeRespond(AStr: string);
var
  enc: string;
  i, v: integer;
begin
  enc := '';
  for i := 1 to length(AStr) do
  begin
    v := ord(AStr[i]);
    enc := enc + HexStr(v, 2);
  end;
  Respond(enc);
end;

function TGDBServer.HandlePacket(APacket: string): boolean;
  var
    addr, len, val: Int64;
    buffer: array of byte;
    s: string;
    i: longint;
    typ: TBreakpointType;
    kind: Int64;
  begin
    result:=true;

    if APacket='' then
      exit(false);

    case APacket[1] of
      '?':
        begin
          Respond(fHandler.GetStatusStr);
        end;
      'c':
        begin
          fHandler.Continue;
          fRunning := true;
        end;
      's':
        begin
          fHandler.SingleStep;
          Respond(fHandler.GetStatusStr);
        end;
      'i':
        begin
          if length(APacket)>1 then
            exit(false);

          fHandler.StepCycles(1);

          Respond('');
        end;
      'm':
        begin
          delete(APacket,1,1);
          addr:=strtoint64('$'+Copy2SymbDel(APacket,','));
          len:=strtoint64('$'+APacket);

          setlength(buffer, len);

          if fHandler.Read(buffer[0], addr, len) then
            begin
              s:='';

              for i := 0 to len-1 do
                s:=s+hexstr(buffer[i],2);

              respond(s);
            end
          else
            respond('E00');
        end;
      'g':
        begin
          Respond(fHandler.GetRegisterString);
        end;
      'H':
        begin
          Respond('OK');
        end;
      'D', 'k':
        begin
          if APacket[1] = 'D' then
          begin
            dbgPrintLn('Client detached - terminating.');
            Respond('OK');
          end
          else
            dbgPrintLn('Client killed program - terminating.');
          Terminate;
        end;
      'M':
        begin
          delete(APacket,1,1);
          addr:=strtoint64('$'+Copy2SymbDel(APacket,','));
          len:=strtoint64('$'+Copy2SymbDel(APacket,':'));

          if length(APacket)<>len*2 then
            begin
              Respond('E00');
              exit;
            end;

          setlength(buffer, len);
          for i := 0 to len-1 do
            buffer[i]:=strtoint('$'+copy(APacket,i*2+1,2));

          if fHandler.Write(buffer[0], addr, len) then
            respond('OK')
          else
            respond('E00');
        end;
      'p':
        begin
          delete(APacket,1,1);
          addr:=strtoint64('$'+APacket);

          val:=0;
          if fHandler.ReadReg(addr,val) then
            Respond(inttohex(val,1))
          else
            Respond('E00');
        end;
      'P':
        begin
          delete(APacket,1,1);
          addr:=strtoint64('$'+Copy2SymbDel(APacket,'='));
          val:=strtoint64('$'+APacket);

          if fHandler.WriteReg(addr,val) then
            Respond('OK')
          else
            Respond('E00');
        end;
     'q':
        if pos('Supported', APacket) > 0 then
          Respond(fHandler.SupportedOptions)
        else if pos('Xfer:memory-map:read', APacket) > 0 then  // qXfer:memory-map:read::0,18a
        begin
          delete(APacket, 1, pos('::', APacket)+1);
          addr:=strtoint64('$'+Copy2SymbDel(APacket,','));
          val:=strtoint64('$'+APacket);
          respond(fHandler.MemoryMap(addr, val));
        end
        else if pos('Rcmd', APacket) > 0 then   // mon help = qRcmd,68656c70
        begin
          delete(APacket, 1, pos(',', APacket));
          if length(APacket) > 0 then
            HandleRcmd(APacket)
          else
           exit(false);
        end
        else
          exit(false);
     'R':
        begin
          fHandler.Reset;
        end;

     'v':
        begin
          if pos('FlashErase', APacket) > 0 then
          begin
            delete(APacket, 1, pos(':', APacket));
            addr:=strtoint64('$'+Copy2SymbDel(APacket,','));
            len:= StrToInt64('$'+APacket);
            fHandler.eraseFlash(addr, len);
            Respond('OK')
          end
          else if pos('FlashWrite', APacket) > 0 then
          begin
            // ‘vFlashWrite:addr:XX...’
            delete(APacket, 1, pos(':', APacket));
            addr:=strtoint64('$'+Copy2SymbDel(APacket,':'));
            setlength(buffer, length(APacket));

            for i := 1 to length(APacket) do
              buffer[i-1] := ord(APacket[i]);

            if fHandler.Write(buffer[0], addr, length(buffer)) then
              Respond('OK')
            else
              Respond('E00');
          end
          else if pos('FlashDone', APacket) > 0 then
            Respond('OK')
          else
            exit(false);
        end;

        'Z':     // Z0,9e,2
        begin
          delete(APacket,1,1);
          if pos(':',APacket)>0 then
            exit(false);

          typ:=TBreakpointType(strtoint('$'+Copy2SymbDel(APacket,',')));
          addr:=strtoint64('$'+Copy2SymbDel(APacket,','));
          kind:=strtoint64('$'+APacket);

          fHandler.SetBreakpoint(typ,addr,kind);

          Respond('OK');
        end;
      'z':
        begin
          delete(APacket,1,1);
          if pos(':',APacket)>0 then
            exit(false);

          typ:=TBreakpointType(strtoint('$'+Copy2SymbDel(APacket,',')));
          addr:=strtoint64('$'+Copy2SymbDel(APacket,','));
          kind:=strtoint64('$'+APacket);

          fHandler.RemoveBreakpoint(typ,addr,kind);

          Respond('OK');
        end
    else
      exit(false);
    end;
  end;

function TGDBServer.fTcpReadChar: char;
{$if defined(unix) or defined(windows)}
var
  FDS: TFDSet;
  TimeV: TTimeVal;
  dataAvailable: boolean;
{$endif}
begin
  Result:=#0;
{$if defined(unix) or defined(windows)}
  TimeV.tv_usec := 1 * 1000;  // 1 msec
  TimeV.tv_sec := 0;
{$endif}
{$ifdef unix}
  FDS := Default(TFDSet);
  fpFD_Zero(FDS);
  fpFD_Set(self.fSock.Handle, FDS);
  dataAvailable := fpSelect(self.fSock.Handle + 1, @FDS, nil, nil, @TimeV) > 0;
{$else}
{$ifdef windows}
  FDS := Default(TFDSet);
  FD_Zero(FDS);
  FD_Set(self.fSock.Handle, FDS);
  dataAvailable := Select(self.fSock.Handle + 1, @FDS, nil, nil, @TimeV) > 0;
{$endif}
{$endif}
  if dataAvailable then
    result := char(fReadBuffer.ReadByte)
  else
    result := #0;
end;

function TGDBServer.HexDecode(hexCode: string): string;
var
  i: integer;
  s: string;
begin
  SetLength(Result, length(hexCode) div 2);
  for i := 1 to length(Result) do
  begin
    s := '$' + hexCode[2*i-1] + hexCode[2*i];
    result[i] := char(StrToInt(s));
  end;
end;

procedure TGDBServer.HandleRcmd(req: string);
var
  resp: string;
  cmds: TStringList;
begin
  if length(req) = 0 then exit;

  cmds := TStringList.Create;
  try
    cmds.Delimiter := ' ';
    cmds.DelimitedText := lowercase(HexDecode(req));
    dbgPrintLn('Rcmd received: ' + cmds.DelimitedText);

    resp := 'OK';  // Only override on actual error or alternative output
    case cmds[0] of
      'help':
        begin
          resp := '  help - List of commands supported.'+LineEnding +
                  '  set remote-debug [1 or 0] - Enable or disable remote protocol debug message'+LineEnding;
        end;
      'set':  // Options: remote-debug
        begin
          if cmds.Count = 3 then
          begin
            case cmds[1] of
              'remote-debug':
                begin
                  if cmds[2] = '1' then
                    debugPrint := true
                  else if cmds[2] = '0' then
                    debugPrint := false
                  else
                    resp := 'E00';
                end
              else
                resp := 'E00';
            end
          end
          else
            resp := 'E00';
        end;
      else
        resp := 'E00';
    end;
  finally
    cmds.Free;
  end;

  // Monitor responses should be encoded,
  // but normal protocol responses (OK, Enn) should be returned plain text
  if (resp = '') or (resp = 'OK') or ((length(resp) = 3) and (resp[1] = 'E')) then
    Respond(resp)
  else
    HexEncodeRespond(resp);
end;

procedure TGDBServer.Execute;
  begin
    fRunning := false;  // sim starts paused
    fNewNotification := false; // no new break notifications
    while not terminated do
      begin
        ReadPacket;
        if fRunning and fNewNotification then
        begin
          if fHandler.DoHalt then
          begin
            WriteLn('Simulation exited...');
            DoExit;
            fRunning := false;
          end
          else if fHandler.BreakpointHit then
          begin
            BreakHit;
            fRunning := false;
          end;
          fNewNotification := false;
        end;
      end;
    fSock.Free;
    fOwner.Terminate;
  end;

procedure TGDBServer.DoExit;
  var
    tmp: TSocketStream;
  begin
    Terminate;
    tmp:=fSock;
    fSock:=nil;
    tmp.Free;
  end;

procedure TGDBServer.fNotifyEvent;
begin
  fNewNotification := true;
end;

procedure TGDBServer.BreakHit;
  begin
    Respond(fHandler.GetStatusStr);
  end;

constructor TGDBServer.Create(AOwner: TGDBServerListener; ASock: TSocketStream; AHandler: IGDBHandler);
  begin
    fHandler:=AHandler;
    fOwner:=AOwner;
    fSock:=ASock;
    fReadBuffer:=fSock;//TReadBufStream.Create(fSock);
    fNewNotification := false;
    fHandler.SendNotification(@fNotifyEvent);

    inherited Create(false);
  end;

procedure TGDBServerListener.Connect(Sender: TObject; Data: TSocketStream);
  begin
    dbgPrintLn('Incoming connection from ' + AddrToString(Data.RemoteAddress));
    fClients.Add(TGDBServer.Create(self,data, fHandler));
  end;

procedure TGDBServerListener.Idle(Sender: TObject);
  begin
    if terminated then
      fServer.StopAccepting;
  end;

procedure TGDBServerListener.FQueryConnect(Sender: TObject; ASocket: LongInt;
  var doaccept: Boolean);
begin
  if fClients.Count > 0 then
  begin
    doaccept := false;
    dbgPrintLn('Refusing new connection - already connected');
  end
  else
  begin
    doaccept := true;
    dbgPrintLn('Accepting new connection');
  end;
end;

procedure TGDBServerListener.Execute;
  begin
    fServer := TInetServer.Create(fPort);
    fServer.AcceptIdleTimeOut := 100;
    try
      fServer.MaxConnections := 1;
      fServer.OnConnectQuery := @FQueryConnect;
      fServer.OnIdle:=@Idle;
      fServer.OnConnect:=@Connect;
      WriteLn('Listening for GDB connection on port ', fPort);

      while not Terminated do
        fServer.StartAccepting;

      fServer.StopAccepting();
    finally
      fServer.Destroy;
    end;
  end;

constructor TGDBServerListener.Create(APort: longint; AHandler: IGDBHandler);
  begin
    inherited Create(true);
    fClients:=tlist.Create;
    fHandler:=AHandler;
    fPort:=APort;
  end;

destructor TGDBServerListener.Destroy;
  var
    cl: TGDBServer;
    i: longint;
  begin
    Terminate;
    //fServer.StopAccepting;
    WaitFor;

    for i := fClients.count-1 downto 0 do
      begin
        cl:=TGDBServer(fClients[i]);
        cl.Terminate;
        cl.WaitFor;
        cl.free;
      end;
    fClients.Free;

    inherited Destroy;
  end;

end.

