�
 TFORM1 0�  TPF0TForm1Form1Left� TopkCaptionComPort Library example - ModClientHeightClientWidth7Color	clBtnFaceFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameConsolas
Font.StylefsBold OldCreateOrder	
OnActivateFormActivateOnClose	FormCloseOnCreate
FormCreatePixelsPerInch`
TextHeight 	TGroupBoxgrp1LeftTop Width/HeightAlignalRightTabOrder  	TGroupBoxgrpSettingsLeftTopWidth+Height� AlignalClientCaptionSettingsTabOrder  TLabellblPortLeftTop%Width-HeightCaptionPort:  TButtonbtnOpenLeftTop� WidthKHeightCaptionOpenTabOrder OnClickbtnOpenClick  TButtonbtnSettingsLeft� Top8Width*HeightHintSettingsCaption...ParentShowHintShowHint	TabOrderOnClickbtnSettingsClick  TButtonbtnBt_StoreLeftgTop� WidthKHeightHintSave settings on diskCaptionStoreParentShowHintShowHint	TabOrderOnClickbtnBt_StoreClick  TButtonbtnLoadLeft� Top� WidthKHeightHintLoad settings from diskCaptionLoadParentShowHintShowHint	TabOrderOnClickbtnLoadClick  TComComboBoxcmbPortLeftTop8Width� HeightHintPort serial to connectComPortComPortComPropertycpPortText    StylecsDropDownList	ItemIndex�ParentShowHintShowHint	TabOrderTabStopOnChangecmbPortChange
OnDropDowncmbPortDropDown  	TCheckBoxchkStayConnectedLeftTopYWidth� HeightHintAuto connect on startupCaptionStay ConnectedParentShowHintShowHint	TabOrder   	TGroupBoxgrpDataLeftTop� Width+Height)AlignalBottomCaptionDataTabOrder TEditedtDataLeftTop0Width� HeightTabOrder   TButtonbtnSendLeft� Top0WidthKHeight!HintWrite a msg in serial portCaptionSendDefault	EnabledParentShowHintShowHint	TabOrderOnClickbtnSendClick  	TCheckBox
chkNewLineLeftTopQWidth� HeightHintAppend LN and LR char to msgCaptionSend new lineChecked	ParentShowHintShowHint	State	cbCheckedTabOrder    	TGroupBoxgrp2Left Top WidthHeightAlignalClientCaptionViewTabOrder TMemommoViewLeftTopWidthHeight� AlignalClientColorclBlackFont.CharsetANSI_CHARSET
Font.ColorclLimeFont.Height�	Font.NameConsolas
Font.StylefsBold 
ParentFontReadOnly	
ScrollBarsssBothTabOrder   TPanelpnl1LeftTop�WidthHeight9AlignalBottom
BevelInnerbvRaised
BevelOuter	bvLoweredTabOrder TComLedledCTSLeftpTopWidthHeightComPortComPort	LedSignallsCTSKindlkPurpleLight  TComLedledDSRLeft� TopWidthHeightComPortComPort	LedSignallsDSRKindlkPurpleLight  TComLedledRLSDLeft� TopWidthHeightComPortComPort	LedSignallsRLSDKindlkPurpleLight  TComLedledRingLeft TopWidthHeightComPortComPort	LedSignallsRingKindlkYellowLight  TLabellbl1LeftpTop WidthHeightCaptionCTS  TLabellbl2Left� Top WidthHeightCaptionDSR  TLabellbl3Left� Top Width$HeightCaptionRLSD  TLabellbl4Left Top Width$HeightCaptionRing  TComLedledTXLeftXTopWidthHeightComPortComPort	LedSignallsTxKind
lkRedLight  TComLedledRXLeftxTopWidthHeightComPortComPort	LedSignallsRxKind
lkRedLight  TLabellbl5Left^Top WidthHeightCaptionTx  TLabellbl6Left~Top WidthHeightCaptionRx   	TGroupBoxgrp3LeftTop� WidthHeight� AlignalBottomTabOrder TMemommoSendViewLeftTopWidth Height� AlignalClientColorclBlackFont.CharsetANSI_CHARSET
Font.ColorclLimeFont.Height�	Font.NameConsolas
Font.StylefsBold 
ParentFontReadOnly	
ScrollBarsssBothTabOrder     TComPortComPortBaudRatebr1200PortCOM1Parity.BitsprNoneStopBitssbOneStopBitDataBitsdbEightDiscardNull	EventsevRxChar	evTxEmptyevRxFlagevRingevBreakevCTSevDSRevErrorevRLSD
evRx80Full FlowControl.OutCTSFlowFlowControl.OutDSRFlowFlowControl.ControlDTR	dtrEnableFlowControl.ControlRTS
rtsDisableFlowControl.XonXoffOutFlowControl.XonXoffInStoredPropsspBasic TriggersOnRxChar	OnAfterOpenComPortOpenOnAfterCloseComPortCloseOnRxCharComPortRxCharLeftToph   