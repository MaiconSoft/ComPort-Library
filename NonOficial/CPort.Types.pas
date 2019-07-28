unit CPort.Types;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections;

type
  T7bByte = class;

  TComDataBuffer = class(TList<byte>)
  private
    FLittleEndianMode: Boolean;
    FAs7bits: T7bByte;
    procedure DecodeParamString(var str: string; List: TComDataBuffer);
    procedure EncodeParamString(const str: string; List: TComDataBuffer);
    function GetString: string;
    procedure SetString(const Value: string);
    function GetAsWord(Index: Integer): Word;
    procedure SetAsWord(Index: Integer; const Value: Word);
    function ReadBytes(Index: Integer; const Buffer; const Size: Integer;
      LittleEndianMode: Boolean = True): Integer;
    function WriteBytes(Index: Integer; const Buffer; const Size: Integer;
      LittleEndianMode: Boolean = True): Integer;
    function GetAsDWord(Index: Integer): Cardinal;
    procedure SetDAsWord(Index: Integer; const Value: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    property AsWord[Index: Integer]: Word read GetAsWord write SetAsWord;
    property AsDWord[Index: Integer]: Cardinal read GetAsDWord write SetDAsWord;
    function Read(Index: Integer; const Buffer; const Size: Integer): Integer;
    function Write(Index: Integer; const Buffer; const Size: Integer): Integer;
    property ToHexString: string read GetString write SetString;
    property LittleEndianMode: Boolean read FLittleEndianMode
      write FLittleEndianMode default True;
    property As7bits: T7bByte read FAs7bits;
  end;

  T7bNible = record
    low, high: byte;
  end;

  T7bByte = class
  private
    FParent: TComDataBuffer;
    function get7bByte(Index: Integer): byte;
    procedure set7bByte(Index: Integer; const Value: byte);
    function getByte(Index: Integer): Word;
    procedure setByte(Index: Integer; const Value: Word);
    function getWord(Index: Integer): Word;
    procedure setWord(Index: Integer; const Value: Word);
    function get7bGeneric(Index, Size: Integer): Cardinal;
    procedure set7bGeneric(Index: Integer; Value: Cardinal; Size: Integer);
    function GetNible(Index: Integer): T7bNible;
    procedure setNible(Index: Integer; const Value: T7bNible);
    function getBit(Index, bIndex: Integer): Boolean;
    procedure setBit(Index, bIndex: Integer; const Value: Boolean);
    function getGeneric(Index, bIndex, bSize: Integer): byte;
    procedure setGeneric(Index, bIndex, bSize: Integer; const Value: byte);
  public
    constructor Create(AOwner: TComDataBuffer);
    property Parent: TComDataBuffer read FParent write FParent;
    property As7bByte[Index: Integer]: byte read get7bByte write set7bByte;
    property AsByte[Index: Integer]: Word read getByte write setByte;
    property AsWord[Index: Integer]: Word read getWord write setWord;
    property AsNible[Index: Integer]: T7bNible read GetNible write setNible;
    property AsBit[Index, bIndex: Integer]: Boolean read getBit write setBit;
    property AsGeneric[Index, bIndex, bSize: Integer]: byte read getGeneric
      write setGeneric;
  end;

  TCustBytePacketEvent = procedure(Sender: TObject; const Data: TComDataBuffer;
    var Pos: Integer) of object;
  TComDataEvent = procedure(Sender: TObject; const Data: TComDataBuffer)
    of object;
  TComProtocol = (cptlCustom, cptlLine, cptlStartEnd, cptlStartSize,
    cptlStartFixedSize);

procedure FixBigEndian(const Buffer; Sizes: array of byte);

implementation

uses
  Winapi.Windows;

procedure ReverseBytes(Source: Pointer; Size: Integer);
var
  Index: Integer;
  procedure swap(var a; var b);
  var
    p: byte;
  begin
    Move(a, p, 1);
    Move(b, a, 1);
    Move(p, b, 1);
  end;

begin
  for Index := 0 to (Size div 2) - 1 do
  begin
    swap(Pointer(LongInt(Source) + Index)^,
      Pointer(LongInt(Source) + (Size - Index - 1))^);
  end;
end;

procedure FixBigEndian(const Buffer; Sizes: array of byte);
var
  offset: Integer;
  Size: byte;
begin
  // make sure you using packed record
  offset := 0;
  for Size in Sizes do
  begin
    if Size > 1 then
      ReverseBytes(Pointer(LongInt(@Buffer) + offset), Size);
    Inc(offset, Size);
  end;

end;

destructor TComDataBuffer.Destroy;
begin
  FAs7bits.Free;
  inherited;
end;

procedure TComDataBuffer.EncodeParamString(const str: string;
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

function T7bByte.get7bByte(Index: Integer): byte;
begin
  Result := get7bGeneric(Index, 1);
end;

function T7bByte.getBit(Index, bIndex: Integer): Boolean;
var
  mask: byte;
begin
  mask := 1 shl bIndex;
  Result := FParent.Items[Index] and mask > 0;
end;

function T7bByte.getByte(Index: Integer): Word;
begin
  Result := get7bGeneric(Index, 2);
end;

function T7bByte.getGeneric(Index, bIndex, bSize: Integer): byte;
var
  mask: byte;
  i: Integer;
begin
  mask := 0;
  for i := 0 to bSize - 1 do
    mask := mask or (1 shl (bIndex + i));

  Result := ((FParent.Items[Index] and mask) shr bIndex);
end;

function T7bByte.GetNible(Index: Integer): T7bNible;
begin
  with FParent do
  begin
    Result.low := Items[Index] and $0F;
    Result.high := (Items[Index] shr 4) and $0F;
  end;
end;

function T7bByte.get7bGeneric(Index: Integer; Size: Integer): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  if Size < 1 then
    Exit;
  for i := 0 to Size - 1 do
  begin
    Result := Result or ((FParent.Items[Index] and $7F) shl (7 * i));
  end;
end;

procedure T7bByte.set7bGeneric(Index: Integer; Value: Cardinal; Size: Integer);
var
  i: Integer;
begin
  if Size < 1 then
    Exit;

  with FParent do
  begin
    while Index + Size > Count do
      Add(0);

    for i := 0 to Size - 1 do
    begin
      Items[Index + i] := ((Value shr (7 * i)) and $7F);
    end;
  end;
end;

function T7bByte.getWord(Index: Integer): Word;
begin
  Result := get7bGeneric(Index, 3);
end;

function TComDataBuffer.GetAsDWord(Index: Integer): Cardinal;
begin
  ReadBytes(Index, Result, SizeOf(Result));
end;

function TComDataBuffer.GetAsWord(Index: Integer): Word;
begin
  ReadBytes(Index, Result, SizeOf(Result));
end;

function TComDataBuffer.GetString: string;
begin
  DecodeParamString(Result, Self);
end;

function TComDataBuffer.Read(Index: Integer; const Buffer;
  const Size: Integer): Integer;
begin
  // This function can return wrong values for big endian system data
  CopyMemory(@Buffer, @ToArray[Index], Size);
end;

function TComDataBuffer.ReadBytes(Index: Integer; const Buffer;
  const Size: Integer; LittleEndianMode: Boolean): Integer;
begin
  Result := Read(Index, Buffer, Size);
  if LittleEndianMode then
    ReverseBytes(@Buffer, Size);
end;

procedure T7bByte.set7bByte(Index: Integer; const Value: byte);
begin
  set7bGeneric(Index, Value, 1);
end;

procedure T7bByte.setBit(Index, bIndex: Integer; const Value: Boolean);
var
  mask: byte;
begin
  mask := 1 shl bIndex;
  if Value then
  begin
    FParent.Items[Index] := FParent.Items[Index] or mask;
  end
  else
  begin
    mask := not mask;
    FParent.Items[Index] := FParent.Items[Index] and mask;
  end;
end;

procedure T7bByte.setByte(Index: Integer; const Value: Word);
begin
  set7bGeneric(Index, Value, 2);
end;

procedure T7bByte.setGeneric(Index, bIndex, bSize: Integer; const Value: byte);
var
  mask, val: byte;
  i: Integer;
begin
  mask := 0;
  for i := 0 to bSize - 1 do
    mask := mask or (1 shl (i + bIndex));

  val := (Value shl bIndex) and mask;

  FParent.Items[Index] := FParent.Items[Index] and (not mask); // clear bits
  FParent.Items[Index] := FParent.Items[Index] or (val shl bIndex);
end;

procedure T7bByte.setNible(Index: Integer; const Value: T7bNible);
begin
  with FParent do
  begin
    while Index > Count do
      Add(0);
    Items[Index] := ((Value.high and $0F) shl 4) or (Value.low and $0F);
  end;
end;

procedure T7bByte.setWord(Index: Integer; const Value: Word);
begin
  set7bGeneric(Index, Value, 3);
end;

function TComDataBuffer.Write(Index: Integer; const Buffer;
  const Size: Integer): Integer;
begin
  // This function can return wrong values for big endian system data
  CopyMemory(@ToArray[Index], @Buffer, Size);
end;

function TComDataBuffer.WriteBytes(Index: Integer; const Buffer;
  const Size: Integer; LittleEndianMode: Boolean): Integer;
begin
  if LittleEndianMode then
    ReverseBytes(@Buffer, Size);
  Result := Write(Index, Buffer, Size);
end;

procedure TComDataBuffer.SetAsWord(Index: Integer; const Value: Word);
begin
  WriteBytes(Index, Value, SizeOf(Value));
end;

procedure TComDataBuffer.SetDAsWord(Index: Integer; const Value: Cardinal);
begin
  WriteBytes(Index, Value, SizeOf(Value));
end;

procedure TComDataBuffer.SetString(const Value: string);
begin
  EncodeParamString(Value, Self);
end;

constructor TComDataBuffer.Create;
begin
  inherited Create;
  FAs7bits := T7bByte.Create(Self);
end;

procedure TComDataBuffer.DecodeParamString(var str: string;
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

{ T7bByte }

constructor T7bByte.Create(AOwner: TComDataBuffer);
begin
  FParent := AOwner;
end;

end.
