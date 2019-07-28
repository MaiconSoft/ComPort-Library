(* *****************************************************
  * This file is a part of ComPort Library ver. 4.11   *
  *                                                    *
  * Written by Dejan Crnila, 1998 - 2002               *
  * Homepage: http://comport.sf.net/                   *
  *                                                    *
  * Maintained by MaicoSoft Jul 2019                   *
  *     Changed visual for terminal like (black-green) *
  *     Make resposive resize form                     *
  *     Add error handling for connect invalid port    *
  *     Add auto update list of port names             *
  *     Add suport to save and restoure settings       *
  *     automatically on startup                       *
  *     Add buffer screen for view last messages sended*
  *     Add control of enable in buttons               *
  *     Add Packet bytes manager                       *
  *                                                    *
  **************************************************** *)

unit ComMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CPort, CPortCtl, System.IniFiles, CPort.Protocol,
  CPort.Protocol.StartEnd, CPort.types;

type
  TForm1 = class(TForm)
    ComPort: TComPort;
    grp1: TGroupBox;
    grpSettings: TGroupBox;
    btnOpen: TButton;
    btnSettings: TButton;
    btnBt_Store: TButton;
    btnLoad: TButton;
    grpData: TGroupBox;
    edtData: TEdit;
    btnSend: TButton;
    chkNewLine: TCheckBox;
    grp2: TGroupBox;
    mmoView: TMemo;
    pnl1: TPanel;
    ledCTS: TComLed;
    ledDSR: TComLed;
    ledRLSD: TComLed;
    ledRing: TComLed;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    ledTX: TComLed;
    ledRX: TComLed;
    lbl5: TLabel;
    lbl6: TLabel;
    cmbPort: TComComboBox;
    lblPort: TLabel;
    chkStayConnected: TCheckBox;
    grp3: TGroupBox;
    mmoSendView: TMemo;
    cpsnProtocol: TComProtocolStartEnd;
    grp4: TGroupBox;
    mmoPackets: TMemo;
    procedure btnOpenClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure ComPortOpen(Sender: TObject);
    procedure ComPortClose(Sender: TObject);
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure cmbPortChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnBt_StoreClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure cmbPortDropDown(Sender: TObject);
    procedure cpsnProtocolItems0Packet(Sender: TObject;
      const Data: TComDataBuffer);
  private
    FIniFileName: TFileName;
    procedure SaveSettings;
    procedure LoadSettings;
    { Private declarations }
  public
    { Public declarations }
    property IniFileName: TFileName read FIniFileName write FIniFileName;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.btnBt_StoreClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  LoadSettings;
end;

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  ComPort.TryToggle;
end;

procedure TForm1.btnSettingsClick(Sender: TObject);
begin
  ComPort.ShowSetupDialog;
end;

procedure TForm1.cmbPortChange(Sender: TObject);
begin
  ComPort.Port := cmbPort.Text;
end;

procedure TForm1.cmbPortDropDown(Sender: TObject);
begin
  cmbPort.ComProperty := cpPort; // Force load new ports available
  cmbPort.Text := ComPort.Port; // Fix correct port name
end;

procedure TForm1.btnSendClick(Sender: TObject);
var
  Str: String;
begin
  Str := edtData.Text;
  mmoSendView.Lines.Add(Str);
  edtData.Clear;
  if chkNewLine.Checked then
    Str := Str + #13#10;
  ComPort.WriteStr(Str);
end;

procedure TForm1.ComPortOpen(Sender: TObject);
begin
  btnOpen.Caption := 'Close';
  SaveSettings;
  btnSettings.Enabled := false;
  btnSend.Enabled := true;
end;

procedure TForm1.ComPortClose(Sender: TObject);
begin
  if btnOpen <> nil then
  begin
    btnOpen.Caption := 'Open';
    btnSettings.Enabled := true;
    btnSend.Enabled := false;
  end;
end;

procedure TForm1.ComPortRxChar(Sender: TObject; Count: Integer);
var
  Str: String;
begin
  ComPort.ReadStr(Str, Count);
  mmoView.Text := mmoView.Text + Str;
end;

procedure TForm1.cpsnProtocolItems0Packet(Sender: TObject;
  const Data: TComDataBuffer);
var
  Tag, Bytes: string;
begin
  Bytes := Data.ToHexString;
  Tag := (Sender as TComProtocolSartEndItem).Tag;
  mmoPackets.Lines.Add(Format('%s >> %s', [Tag, Bytes]));
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  LoadSettings;
  if chkStayConnected.Checked then
    ComPort.TryConect;
  ComPort.TriggersOnRxChar := false;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
  if ComPort.Connected then
  begin
    ComPort.TryDisconect;
    Sleep(100); // wait thread terminate
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  IniFileName := ChangeFileExt(ParamStr(0), '.ini');
end;

procedure TForm1.LoadSettings;
begin
  ComPort.LoadSettings(stIniFile, IniFileName);
  cmbPort.Text := ComPort.Port;
  with TIniFile.Create(IniFileName) do
  begin
    chkStayConnected.Checked := ReadBool(ComPort.Name, 'StayConnected',
      chkStayConnected.Checked);
    chkNewLine.Checked := ReadBool(ComPort.Name, 'NewLine', chkNewLine.Checked);
    Free;
  end;
  // ComPort.LoadSettings(stRegistry, 'HKEY_LOCAL_MACHINE\Software\Dejan');
end;

procedure TForm1.SaveSettings;
begin
  ComPort.StoreSettings(stIniFile, IniFileName);
  with TIniFile.Create(IniFileName) do
  begin
    WriteBool(ComPort.Name, 'StayConnected', chkStayConnected.Checked);
    WriteBool(ComPort.Name, 'NewLine', chkNewLine.Checked);
    Free;
  end;
  // ComPort.StoreSettings(stRegistry, 'HKEY_LOCAL_MACHINE\Software\Dejan');
end;

end.
