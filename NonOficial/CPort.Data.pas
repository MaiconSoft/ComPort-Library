unit CPort.Data;

interface

uses
  System.SysUtils, System.Classes, CPort, CPort.Types,
  System.Generics.Collections, CPort.Protocol;

type

  TComDataBytePacket = class(TComponent)
  private
    FComLink: TComLink;
    FComPort: TCustomComPort;
    FStartString: string;
    FStopString: string;
    FMaxBufferSize: Integer;
    FSize: Integer;
    FIncludeStrings: Boolean;
    FCaseInsensitive: Boolean;
    FInPacket: Boolean;
    FFoundLength: Integer;
    FBuffer: TComDataBuffer;
    FOnPacket: TComDataEvent;
    FOnDiscard: TComDataEvent;
    FOnCustomStart: TCustBytePacketEvent;
    FOnCustomStop: TCustBytePacketEvent;
    FStartBytes: TComDataBuffer;
    FStopBytes: TComDataBuffer;
    FProtocol: TComProtocol;
    FFixedSize: Integer;
    FFixedPosition: Integer;
    procedure SetComPort(const Value: TCustomComPort);
    procedure SetCaseInsensitive(const Value: Boolean);
    procedure SetSize(const Value: Integer);
    procedure EncodeParamString(const str: string; List: TComDataBuffer);
    procedure DecodeParamString(var str: string; List: TComDataBuffer);
    procedure SetStartString(const Value: string);
    procedure SetStopString(const Value: string);
    function GetStartString: string;
    function GetStopString: string;
    procedure RxBuf(Sender: TObject; const Buffer; Count: Integer);
    procedure CheckIncludeStrings(var str: string);
    function Upper(const str: string): string;
    procedure EmptyBuffer;
    function ValidStop: Boolean;
    procedure AddBuffer(const Value: byte); virtual;
    procedure ProtocolStartEnd(const Value: byte);
    procedure ProtocolCustom(const Value: byte); virtual;
    procedure ProtocolLine(const Value: byte);
    procedure ProtocolStartFixed(const Value: byte);
    procedure ProtocolStartSize(const Value: byte);

  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DoDiscard(const Data: TComDataBuffer); dynamic;
    procedure DoPacket(const Data: TComDataBuffer); dynamic;
    procedure DoCustomStart(const Data: TComDataBuffer;
      var Pos: Integer); dynamic;
    procedure DoCustomStop(const Data: TComDataBuffer;
      var Pos: Integer); dynamic;
    procedure HandleBuffer; virtual;

    property Buffer: TComDataBuffer read FBuffer write FBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddData(const Data: array of byte);virtual;
    procedure ResetBuffer;
  published  
    property ComPort: TCustomComPort read FComPort write SetComPort;
    property IncludeStrings: Boolean read FIncludeStrings write FIncludeStrings
      default False;
    property MaxBufferSize: Integer read FMaxBufferSize write FMaxBufferSize
      default 1024;
    property StartString: string read GetStartString write SetStartString;
    property StopString: string read GetStopString write SetStopString;
    property Size: Integer read FSize write SetSize default 0;
    property OnDiscard: TComDataEvent read FOnDiscard write FOnDiscard;
    property OnPacket: TComDataEvent read FOnPacket write FOnPacket;
    property OnCustomStart: TCustBytePacketEvent read FOnCustomStart
      write FOnCustomStart;
    property OnCustomStop: TCustBytePacketEvent read FOnCustomStop
      write FOnCustomStop;
    property Protocol: TComProtocol read FProtocol write FProtocol
      default cptlStartEnd;
    property FixedSize: Integer read FFixedSize write FFixedSize default -1;
    property FixedPosition: Integer read FFixedPosition write FFixedPosition
      default -1;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CPortLib', [TComDataBytePacket]);
end;

(* ****************************************
  * TComDataPacket component              *
  **************************************** *)

// create component
constructor TComDataBytePacket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFoundLength := 0;
  FInPacket := False;
  FBuffer := TComDataBuffer.Create;
  FStartBytes := TComDataBuffer.Create;
  FStopBytes := TComDataBuffer.Create;
  FComLink := TComLink.Create;
  FComLink.OnRxBuf := RxBuf;
  FMaxBufferSize := 1024;
  FFixedSize := -1;
  FFixedPosition := -1;
  FProtocol := cptlStartEnd;
end;

procedure TComDataBytePacket.DecodeParamString(var str: string;
  List: TComDataBuffer);
var
  Value: byte;
begin
  str := '';
  for Value in List do
  begin
    str := str + format(' %.2x', [Value]);
  end;
  str := Trim(str);
end;

// destroy component
destructor TComDataBytePacket.Destroy;
begin
  ComPort := nil;
  FComLink.Free;
  FBuffer.Free;
  FStartBytes.Free;
  FStopBytes.Free;
  inherited Destroy;
end;

procedure TComDataBytePacket.ProtocolCustom(const Value: byte);
begin

end;

procedure TComDataBytePacket.ProtocolLine(const Value: byte);
const
  LineTerminator: array [0 .. 1] of byte = ($13, $10);
begin
  Buffer.Add(Value);

  if LineTerminator[FFoundLength] = Value then
    Inc(FFoundLength)
  else
    FFoundLength := 0;

  // terminator is found
  if FFoundLength = 2 then
  begin
    if not IncludeStrings then
      Buffer.DeleteRange(Buffer.Count - 2, 2);
    DoPacket(Buffer);
    FFoundLength := 0;
    Buffer.Clear;
  end;
end;

procedure TComDataBytePacket.ProtocolStartFixed(const Value: byte);
begin
  if (FStartBytes.Count = 0) then
  begin
    raise Exception.Create
      ('For this protocol, you need especify "start" string.');
  end;

  if (FFixedSize < 1) then
  begin
    raise Exception.Create('For this protocol, "FixedSize" must be > 0.');
  end;

  Buffer.Add(Value);

  if FInPacket then
  begin
    Inc(FFoundLength);
    if FFoundLength = FFixedSize then
    begin
      FInPacket := False;
      DoPacket(Buffer);
      FFoundLength := 0;
      Buffer.Clear;
    end;

    if Buffer.Count = FMaxBufferSize then
    begin
      EmptyBuffer;
      FFoundLength := 0;
      FInPacket := False;
    end;
  end
  else
  begin
    if Value = FStartBytes[FFoundLength] then
      Inc(FFoundLength)
    else
      FFoundLength := 0;

    // Start string OK, open packet
    if (FFoundLength = FStartBytes.Count) then
    begin
      EmptyBuffer;
      if IncludeStrings then
        Buffer.AddRange(FStartBytes.ToArray);
      FInPacket := True;
      FFoundLength := 0;
    end;

    if Buffer.Count = FMaxBufferSize then
    begin
      EmptyBuffer;
      FFoundLength := 0;
    end;
  end;

end;

procedure TComDataBytePacket.ProtocolStartSize(const Value: byte);
begin
  if (FStartBytes.Count = 0) then
  begin
    raise Exception.Create
      ('For this protocol, you need especify "start" string.');
  end;

  if (FFixedPosition < 0) then
  begin
    raise Exception.Create('For this protocol, "FixedPosition" must be >= 0.');
  end;

  Buffer.Add(Value);

  if FInPacket then
  begin
    if FFoundLength < 0 then
    begin
      Inc(FFoundLength);
      if FFoundLength = 0 then
      begin
        FFoundLength := Value;
      end;
    end
    else
    begin
      Dec(FFoundLength);
      if FFoundLength <= 0 then
      begin
        FInPacket := False;
        DoPacket(Buffer);
        FFoundLength := 0;
        Buffer.Clear;
      end;
    end;

    if Buffer.Count = FMaxBufferSize then
    begin
      EmptyBuffer;
      FFoundLength := 0;
      FInPacket := False;
    end;
  end
  else
  begin
    if Value = FStartBytes[FFoundLength] then
      Inc(FFoundLength)
    else
      FFoundLength := 0;

    // Start string OK, open packet
    if (FFoundLength = FStartBytes.Count) then
    begin
      EmptyBuffer;
      if IncludeStrings then
        Buffer.AddRange(FStartBytes.ToArray);
      FInPacket := True;
      FFoundLength := -FixedPosition;
    end;

    if Buffer.Count = FMaxBufferSize then
    begin
      EmptyBuffer;
      FFoundLength := 0;
    end;
  end;

end;

procedure TComDataBytePacket.ProtocolStartEnd(const Value: byte);
begin
  if (FStartBytes.Count = 0) or (FStopBytes.Count = 0) then
  begin
    raise Exception.Create
      ('For this protocol, you need especify "start" and "stop" strings.');
  end;

  Buffer.Add(Value);
  if FInPacket then
  begin
    if Value = FStopBytes[FFoundLength] then
      Inc(FFoundLength)
    else
    begin
      FFoundLength := 0;
    end;

    // Stop string OK, close packet
    if (FFoundLength = FStopBytes.Count) then
    begin
      FInPacket := False;
      if not IncludeStrings then
        Buffer.DeleteRange(Buffer.Count - FStopBytes.Count, FStopBytes.Count);
      DoPacket(Buffer);
      FFoundLength := 0;
      Buffer.Clear;
    end;

    if Buffer.Count = FMaxBufferSize then
    begin
      EmptyBuffer;
      FFoundLength := 0;
      FInPacket := False;
    end;
  end
  else
  begin
    if Value = FStartBytes[FFoundLength] then
      Inc(FFoundLength)
    else
      FFoundLength := 0;

    // Start string OK, open packet
    if (FFoundLength = FStartBytes.Count) then
    begin
      EmptyBuffer;
      if IncludeStrings then
        Buffer.AddRange(FStartBytes.ToArray);
      FInPacket := True;
      FFoundLength := 0;
    end;

    if Buffer.Count = FMaxBufferSize then
    begin
      EmptyBuffer;
      FFoundLength := 0;
    end;
  end;
end;

procedure TComDataBytePacket.AddBuffer(const Value: byte);
begin
  case Protocol of
    cptlCustom:
      ProtocolCustom(Value);
    cptlLine:
      ProtocolLine(Value);
    cptlStartEnd:
      ProtocolStartEnd(Value);
    cptlStartSize:
      ProtocolStartSize(Value);
    cptlStartFixedSize:
      ProtocolStartFixed(Value);
  end;
end;

// add custom data to packet buffer
procedure TComDataBytePacket.AddData(const Data: array of byte);
var
  Value: byte;
begin
  if Length(Data) = 0 then
    Exit;

  for Value in Data do
  begin
    AddBuffer(Value);
  end;
end;

// remove ComPort property if being destroyed
procedure TComDataBytePacket.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FComPort) and (Operation = opRemove) then
    ComPort := nil;
end;

// call OnDiscard
procedure TComDataBytePacket.DoDiscard(const Data: TComDataBuffer);
begin
  if Assigned(FOnDiscard) then
    FOnDiscard(Self, Data);
end;

// call OnPacket
procedure TComDataBytePacket.DoPacket(const Data: TComDataBuffer);
begin
  if Assigned(FOnPacket) then
    FOnPacket(Self, Data);
end;

// call OnCustomStart
procedure TComDataBytePacket.DoCustomStart(const Data: TComDataBuffer;
  var Pos: Integer);
begin
  if Assigned(FOnCustomStart) then
    FOnCustomStart(Self, Data, Pos);
end;

// call OnCustomStop
procedure TComDataBytePacket.DoCustomStop(const Data: TComDataBuffer;
  var Pos: Integer);
begin
  if Assigned(FOnCustomStop) then
    FOnCustomStop(Self, Data, Pos);
end;

// discard start and stop strings
procedure TComDataBytePacket.CheckIncludeStrings(var str: string);
var
  LenStart, LenStop: Integer;
begin
  if FIncludeStrings then
    Exit;
  LenStart := Length(FStartString);
  LenStop := Length(FStopString);
  // remove start string
  if Pos(Upper(FStartString), Upper(str)) = 1 then
    str := Copy(str, LenStart + 1, Length(str) - LenStart);
  // remove stop string
  if Pos(Upper(FStopString), Upper(str)) = (Length(str) - LenStop + 1) then
    str := Copy(str, 1, Length(str) - LenStop);
end;

// upper case
function TComDataBytePacket.Upper(const str: string): string;
begin
  if FCaseInsensitive then
    Result := UpperCase(str)
  else
    Result := str;
end;

// split buffer in packets
procedure TComDataBytePacket.HandleBuffer;
begin

end;

// is stop condition valid?
function TComDataBytePacket.ValidStop: Boolean;
begin
  Result := (FSize > 0) or (Length(FStopString) > 0) or
    (Assigned(FOnCustomStop));
end;

// receive data
procedure TComDataBytePacket.ResetBuffer;
begin
  EmptyBuffer;
end;

procedure TComDataBytePacket.RxBuf(Sender: TObject; const Buffer;
  Count: Integer);
var
  Data: array of byte;
  i: Integer;
begin
  SetLength(Data, Count);
  Move(Buffer, Data[0], Count);
  AddData(Data);
end;

// empty buffer
procedure TComDataBytePacket.EmptyBuffer;
begin
  if Buffer.Count > 0 then
  begin
    try
      DoDiscard(Buffer);
    finally
      Buffer.Clear;
      FInPacket := False;
    end;
  end;
end;

procedure TComDataBytePacket.EncodeParamString(const str: string;
  List: TComDataBuffer);
var
  buf: TStringList;
  s: string;
  Value: Integer;
begin
  buf := TStringList.Create;
  buf.Delimiter := ' ';
  buf.DelimitedText := str;
  List.Clear;
  for s in buf do
  begin
    if TryStrToInt('$' + s, Value) then
      List.Add(Value);
  end;
  buf.Free;
end;

function TComDataBytePacket.GetStartString: string;
begin
  DecodeParamString(Result, FStartBytes);
end;

function TComDataBytePacket.GetStopString: string;
begin
  DecodeParamString(Result, FStopBytes);
end;

// set com port
procedure TComDataBytePacket.SetComPort(const Value: TCustomComPort);
begin
  if Value <> FComPort then
  begin
    if FComPort <> nil then
      FComPort.UnRegisterLink(FComLink);
    FComPort := Value;
    if FComPort <> nil then
    begin
      FComPort.FreeNotification(Self);
      FComPort.RegisterLink(FComLink);
    end;
  end;
end;

// set case sensitivity
procedure TComDataBytePacket.SetCaseInsensitive(const Value: Boolean);
begin
  if FCaseInsensitive <> Value then
  begin
    FCaseInsensitive := Value;
    if not(csLoading in ComponentState) then
      EmptyBuffer;
  end;
end;

// set packet size
procedure TComDataBytePacket.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    if not(csLoading in ComponentState) then
      EmptyBuffer;
  end;
end;

// set start string
procedure TComDataBytePacket.SetStartString(const Value: string);
begin
  EncodeParamString(Value, FStartBytes);

  if not(csLoading in ComponentState) then
    EmptyBuffer;
end;

// set stop string
procedure TComDataBytePacket.SetStopString(const Value: string);
begin
  EncodeParamString(Value, FStopBytes);

  if not(csLoading in ComponentState) then
    EmptyBuffer;
end;

end.
