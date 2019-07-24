(* *****************************************************
  * This is a non-oficial part of ComPort Library      *
  *   ver. 4.11 for Delphi 5, 6, 7, 2007-2010,XE       *                                 *
  *   and  C++ Builder 3, 4, 5, 6                      *
  *                                                    *
  * Written by MaicoSoft Jul 2019                      *
  * Note:                                              *
  *      The envent OnAfterClose and OnAfterOpen of    *
  *      TComPort is used by TComButton, if you need   *
  *      this envents you can use the TComButton       *
  *      events instead.                               *
  *                                                    *
  **************************************************** *)

unit CPortButton;

interface

uses SysUtils, Classes, Cport, Vcl.StdCtrls;

const
  strConection: array [boolean] of string = ('Connect', 'Disconect');

Type
  TComButton = class(TButton)
  private
    FPort: TComPort;
    FAfterClose: TNotifyEvent;
    FAfterOpen: TNotifyEvent;
    procedure SetPort(const Value: TComPort);
    function IsPortAssigned: boolean;
    procedure DoAfterClose(Sender: TObject);
    procedure DoAfterOpen(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Port: TComPort read FPort write SetPort;
    property OnAfterClose: TNotifyEvent read FAfterClose write FAfterClose;
    property OnAfterOpen: TNotifyEvent read FAfterOpen write FAfterOpen;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CPortLib', [TComButton]);
end;

{ TComButton }

procedure TComButton.Click;
begin
  inherited;
  if IsPortAssigned then
  begin
    Port.TryToggle;
    Caption := strConection[Port.Connected];
  end
  else
    Caption := '[unsigned]';
end;

constructor TComButton.Create(AOwner: TComponent);
begin
  inherited;
  if IsPortAssigned then
    Caption := strConection[Port.Connected]
  else
    Caption := '[unsigned]';
end;

destructor TComButton.Destroy;
begin
  if IsPortAssigned then
  begin
    Port.OnAfterOpen := nil;
    Port.OnAfterClose := nil;
    Port.TryDisconect;
  end;
  inherited;
end;

procedure TComButton.DoAfterClose(Sender: TObject);
begin
  Caption := strConection[Port.Connected];

  if Assigned(FAfterClose) then
    FAfterClose(Self);
end;

procedure TComButton.DoAfterOpen(Sender: TObject);
begin
  Caption := strConection[Port.Connected];
  if Assigned(FAfterOpen) then
    FAfterOpen(Self);
end;

function TComButton.IsPortAssigned: boolean;
begin
  Result := Assigned(Port);
end;

procedure TComButton.SetPort(const Value: TComPort);
begin
  FPort := Value;
  if IsPortAssigned then
  begin
    Port.OnAfterOpen := DoAfterOpen;
    Port.OnAfterClose := DoAfterClose;
    Caption := strConection[Port.Connected];
  end;
end;

end.
