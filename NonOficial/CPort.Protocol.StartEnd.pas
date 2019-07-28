unit CPort.Protocol.StartEnd;

interface

uses System.SysUtils, System.Classes, CPort, CPort.Types, Vcl.ActnList,
  Generics.Legacy, CPort.Protocol, Dialogs;

type
  TComProtocolSartEndItem = class(TCustomProtocolItem)
  private
    FStart, FStop: TComDataBuffer;
    FFoundLength: integer;
    FInPacket: Boolean;
    FIncludeDelimiters: Boolean;
    function GetStrFooter: string;
    function GetStrHeader: string;
    procedure SetStrFooter(const Value: string);
    procedure SetStrHeader(const Value: string);
    procedure DoDiscard; override;
    procedure DoPacket; override;
  public
    procedure AddBuffer(const Value: byte); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property StrStart: string read GetStrHeader write SetStrHeader;
    property StrStop: string read GetStrFooter write SetStrFooter;
    property IncludeDelimiters: Boolean read FIncludeDelimiters
      write FIncludeDelimiters;
  end;

  TComProtocolStartEndCollection = class
    (TOwnedCollection<TComProtocolSartEndItem>)
  private

  public
    destructor Destroy; override;
  end;

  TComProtocolStartEnd = class(TCustomComProtocol)
  private
    FItems: TComProtocolStartEndCollection;
  public
    procedure AddBuffer(const Value: byte); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TComProtocolStartEndCollection read FItems write FItems;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CPortLib', [TComProtocolStartEnd]);
end;

{ TComProtocolStartEnd }

procedure TComProtocolStartEnd.AddBuffer(const Value: byte);
var
  i: integer;
begin
  inherited;
  if Items.Count > 0 then
    for i := 0 to Items.Count - 1 do
    begin
      Items.Items[i].AddBuffer(Value);

    end;
end;

constructor TComProtocolStartEnd.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TComProtocolStartEndCollection.Create(Self);
end;

destructor TComProtocolStartEnd.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TComProtocolSartEndItem }

procedure TComProtocolSartEndItem.AddBuffer(const Value: byte);
begin
  inherited;
  if not Enable then
    Exit;

  if (FStart.Count = 0) or (FStop.Count = 0) then
  begin
    raise Exception.Create
      ('For this protocol, you need especify "start" and "stop" strings.');
  end;

  Buffer.Add(Value);

  if FInPacket then
  begin
    if Value = FStop[FFoundLength] then
      Inc(FFoundLength)
    else
    begin
      FFoundLength := 0;
    end;

    // Stop string OK, close packet
    if (FFoundLength = FStop.Count) then
    begin
      FInPacket := False;
      if not FIncludeDelimiters then
        Buffer.DeleteRange(Buffer.Count - FStop.Count, FStop.Count);
      DoPacket;
      FFoundLength := 0;
      Buffer.Clear;
    end;

    if Buffer.Count = MaxBufferSize then
    begin
      EmptyBuffer;
      FFoundLength := 0;
      FInPacket := False;
    end;
  end
  else
  begin
    if Value = FStart[FFoundLength] then
      Inc(FFoundLength)
    else
      FFoundLength := 0;

    // Start string OK, open packet
    if (FFoundLength = FStart.Count) then
    begin

      EmptyBuffer;
      if IncludeDelimiters then
        Buffer.AddRange(FStart.ToArray);
      FInPacket := True;
      FFoundLength := 0;
    end;

    if Buffer.Count = MaxBufferSize then
    begin
      EmptyBuffer;
      FFoundLength := 0;
    end;
  end;
end;

constructor TComProtocolSartEndItem.Create(Collection: TCollection);
begin
  inherited;
  FStart := TComDataBuffer.Create;
  FStop := TComDataBuffer.Create;
  FFoundLength := 0;
  FInPacket := False;
end;

destructor TComProtocolSartEndItem.Destroy;
begin
  FStart.Free;
  FStop.Free;
  inherited;
end;

procedure TComProtocolSartEndItem.DoDiscard;
begin
  inherited;

end;

procedure TComProtocolSartEndItem.DoPacket;
begin
  inherited;

end;

function TComProtocolSartEndItem.GetStrFooter: string;
begin
  result := FStop.ToHexString;
end;

function TComProtocolSartEndItem.GetStrHeader: string;
begin
  result := FStart.ToHexString;
end;

procedure TComProtocolSartEndItem.SetStrFooter(const Value: string);
begin
  FStop.ToHexString := Value;
end;

procedure TComProtocolSartEndItem.SetStrHeader(const Value: string);
begin
  FStart.ToHexString := Value;
end;

{ TComProtocolStartEndCollection }

destructor TComProtocolStartEndCollection.Destroy;
var
  i: integer;
begin
  inherited;
end;

end.
