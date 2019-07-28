unit CPort.Protocol;

interface

uses
  System.SysUtils, System.Classes, CPort, CPort.Types, Vcl.ActnList,
  Generics.Legacy, Dialogs;

type
  TCustomProtocolItem = class(TCollectionItem)
  private
    FBuffer: TComDataBuffer;
    FOnDiscard: TComDataEvent;
    FOnPacket: TComDataEvent;
    FMaxBufferSize: Cardinal;
    FTag: string;
    FEnable: Boolean;
  protected
    procedure DoDiscard; virtual;
    procedure DoPacket; virtual;
    procedure EmptyBuffer; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure AddBuffer(const Value: byte); virtual;
    property Buffer: TComDataBuffer read FBuffer write FBuffer;
  published
    property OnPacket: TComDataEvent read FOnPacket write FOnPacket;
    property OnDiscard: TComDataEvent read FOnDiscard write FOnDiscard;
    property MaxBufferSize: Cardinal read FMaxBufferSize write FMaxBufferSize;
    property Tag: string read FTag write FTag;
    property Enable: Boolean read FEnable write FEnable;
  end;

  TCustomProtocolCollection = class(TOwnedCollection<TCustomProtocolItem>)
  private

  public

  end;

  TCustomComProtocol = class(TComponent)
  private
    FComPort: TComPort;
    procedure RxBuf(Sender: TObject; const Buffer; Count: Integer);
    procedure SetComPort(const Value: TComPort);
    procedure AddData(const Data: array of byte);
  protected
    procedure AddBuffer(const Value: byte); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ComPort: TComPort read FComPort write SetComPort;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CPortLib', [TCustomComProtocol]);
end;

{ TCustomProtocolItem }

procedure TCustomProtocolItem.AddBuffer(const Value: byte);
begin

end;

constructor TCustomProtocolItem.Create(Collection: TCollection);
begin
  inherited;
  Buffer := TComDataBuffer.Create;
  FMaxBufferSize := 1024;
  FEnable := True;
end;

destructor TCustomProtocolItem.Destroy;
begin
  Buffer.Free;
  inherited;
end;

procedure TCustomProtocolItem.DoDiscard;
begin
  if Assigned(FOnDiscard) then
    FOnDiscard(Self, Buffer);
end;

procedure TCustomProtocolItem.DoPacket;
begin
  if Assigned(FOnPacket) then
  begin
    FOnPacket(Self, Buffer);;
  end;
end;

procedure TCustomProtocolItem.EmptyBuffer;
begin
  Buffer.Clear;
end;

{ TCustomComProtocol }

constructor TCustomComProtocol.Create(AOwner: TComponent);
begin
  inherited;
  FComPort := nil;
end;

destructor TCustomComProtocol.Destroy;
begin

  inherited;
end;

procedure TCustomComProtocol.RxBuf(Sender: TObject; const Buffer;
  Count: Integer);
var
  Data: array of byte;
  i: Integer;
begin
  SetLength(Data, Count);
  Move(Buffer, Data[0], Count);
  AddData(Data);
end;

procedure TCustomComProtocol.AddBuffer(const Value: byte);
begin

end;

procedure TCustomComProtocol.AddData(const Data: array of byte);
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

procedure TCustomComProtocol.SetComPort(const Value: TComPort);
begin
  FComPort := Value;
  if Assigned(FComPort) then
    FComPort.OnRxBuf := RxBuf;
end;

end.
