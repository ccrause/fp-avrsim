unit gdbserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, bufstream, SysUtils, strutils, ssockets;

type
  TGDBServerListener = class;

  TBreakpointType = (btMemBreak,btHWBreak, btWWatch, btRWatch, btAWatch);
  TStopReply = (srOK, srSigInt, srBreakPoint);

  TBreakNotify = procedure of object;

  IGDBHandler = interface
    function GetStatus: TStopReply;
    function Continue: TStopReply;
    function SingleStep: TStopReply;

    procedure DoBreak;

    procedure StepCycles(ACycles: longint);

    function Read(var ABuffer; AAddr,ALen: int64): boolean;
    function Write(const ABuffer; AAddr,ALen: int64): boolean;

    function ReadReg(AAddr: int64; var AVal: int64): boolean;
    function WriteReg(AAddr, AVal: int64): boolean;

    function GetRegisterString: string;

    procedure SetBreakHit(AEvent: TBreakNotify);

    function SupportedOptions: string;

    procedure Reset;

    procedure SetBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);
    procedure RemoveBreakpoint(AType: TBreakpointType; AAddr: int64; AKind: longint);

    function BreakpointHit: boolean;
  end;

  TGDBServer = class(TThread)
  private
    fOwner: TGDBServerListener;
    fReadBuffer: TStream;
    fHandler: IGDBHandler;
    fSock: TSocketStream;
    fOldBreak: boolean;

    procedure BreakHit;
    procedure ReadPacket;

    function CalcChecksum(const AStr: string): byte;
    procedure Escape(var AStr: string);
    procedure Respond(AStr: string);
    procedure Respond(ATyp: TStopReply);

    function HandlePacket(APacket: string): boolean;
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

implementation

uses sockets;

var
  debugPrint: boolean = true;

procedure dbgPrint(const c: char);
begin
  if debugPrint then write(c);
end;

procedure dbgPrintLn(const s: string);
begin
  if debugPrint then WriteLn(s);
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
      c:=char(fReadBuffer.ReadByte);

      if c=#$3 then
        begin
          dbgPrintLn('-> <Ctrl-C>');
          fHandler.DoBreak;
          Respond(fHandler.GetStatus);
          fOldBreak := true;  // prevent additional break notifications
        end;
    until (c='$') or terminated;

    if terminated then exit;

    c:=char(fReadBuffer.ReadByte);
    s:='';
    calcSum:=0;
    while c<>'#' do
      begin
        calcSum := byte(calcSum+byte(c));

        if c=#$7D then
          begin
            c:=char(fReadBuffer.ReadByte);

            // Something weird happened
            if c='#' then
              break;

            calcSum:=calcSum+byte(c);

            c:=char(byte(c) xor $20);
          end;

        s:=s+c;
        c:=char(fReadBuffer.ReadByte);
      end;
    dbgPrintLn('-> ' + s);
    cksum:=strtoint('$'+char(fReadBuffer.ReadByte)+char(fReadBuffer.ReadByte));

    if calcSum=cksum then
      begin
        fSock.WriteByte(byte('+'));  dbgPrintLn('<- +');
        if not HandlePacket(s) then
        begin
          Respond('');
          dbgPrintLn('<- ''''');
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
          Respond(fHandler.GetStatus);
        end;
      'c':
        begin
          fHandler.Continue;
        end;
      's':
        begin
          fHandler.SingleStep;
          Respond(srOK);
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
            dbgPrintLn('Client detached - terminating.')
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
        else
          exit(false);
     'R':
        begin
          fHandler.Reset;
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

procedure TGDBServer.Execute;
  var
    newBreak: boolean;
  begin
    fOldBreak := fHandler.BreakpointHit;  // Start in rsBreak state by default.  No need to send this through since gdb starts with a status request (?)
    while not terminated do
      begin
        ReadPacket;
        newBreak := fHandler.BreakpointHit;
        if newBreak and not fOldBreak then
        begin
          BreakHit;
        end;
        fOldBreak := newBreak;
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

procedure TGDBServer.BreakHit;
  begin
    Respond(srOK);
  end;

constructor TGDBServer.Create(AOwner: TGDBServerListener; ASock: TSocketStream; AHandler: IGDBHandler);
  begin
    fHandler:=AHandler;
    fOwner:=AOwner;
    fSock:=ASock;
    fReadBuffer:=fSock;//TReadBufStream.Create(fSock);
    fOldBreak := false;
    fHandler.SetBreakHit(@BreakHit);

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
    fServer.StopAccepting;
    WaitFor;

    for i := fClients.count-1 downto 0 do
      begin
        cl:=TGDBServer(fClients[i]);
        cl.Terminate;
        cl.WaitFor;
        cl.free;
      end;

    inherited Destroy;
  end;

end.

