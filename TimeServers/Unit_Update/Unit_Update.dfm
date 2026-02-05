object Form10: TForm10
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1054#1073#1085#1086#1074#1083#1077#1085#1080#1077
  ClientHeight = 411
  ClientWidth = 484
  Color = clBtnFace
  Constraints.MinHeight = 450
  Constraints.MinWidth = 500
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesktopCenter
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object MemoUpdateLog: TMemo
    Left = 0
    Top = 0
    Width = 484
    Height = 313
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object TabControlButtons: TTabControl
    Left = 0
    Top = 374
    Width = 484
    Height = 37
    Align = alBottom
    TabOrder = 3
    object ButtonDownload: TButton
      Left = 261
      Top = 7
      Width = 95
      Height = 25
      Cursor = crHandPoint
      Caption = #1057#1082#1072#1095#1072#1090#1100
      Default = True
      TabOrder = 0
      OnClick = ButtonDownloadClick
    end
    object ButtonClose: TButton
      Left = 370
      Top = 7
      Width = 95
      Height = 25
      Cursor = crHandPoint
      Caption = #1047#1072#1082#1088#1099#1090#1100
      TabOrder = 1
      OnClick = ButtonCloseClick
    end
  end
  object ProgressBarUpdate: TProgressBar
    Left = 0
    Top = 313
    Width = 484
    Height = 21
    Align = alBottom
    TabOrder = 1
    Visible = False
  end
  object GroupBoxOption: TGroupBox
    Left = 0
    Top = 334
    Width = 484
    Height = 40
    Align = alBottom
    Caption = #1054#1087#1094#1080#1080
    TabOrder = 2
    object CheckBoxQuickUpdate: TCheckBox
      Left = 13
      Top = 17
      Width = 210
      Height = 17
      Cursor = crHandPoint
      Caption = #1041#1099#1089#1090#1088#1086#1077' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1077
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBoxForceUpdate: TCheckBox
      Left = 261
      Top = 17
      Width = 210
      Height = 17
      Cursor = crHandPoint
      Caption = #1055#1088#1080#1085#1091#1076#1080#1090#1077#1083#1100#1085#1086#1077' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1077
      TabOrder = 0
      OnClick = CheckBoxForceUpdateClick
    end
  end
  object PopupMenuLanguage: TPopupMenu
    Left = 260
    Top = 13
    object LangUpdateCancel: TMenuItem
      Caption = #1054#1073#1085#1086#1074#1083#1077#1085#1080#1077' '#1086#1090#1084#1077#1085#1077#1085#1086' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1084'!'
    end
    object LangNoDownload: TMenuItem
      Caption = #1060#1072#1081#1083#1099' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103' '#1085#1077' '#1079#1072#1075#1088#1091#1078#1077#1085#1099'!'
    end
    object LangUpdateСomplet: TMenuItem
      Caption = #1054#1073#1085#1086#1074#1083#1077#1085#1080#1077' '#1079#1072#1074#1077#1088#1096#1077#1085#1086'!'
    end
    object LangDownloadComplet: TMenuItem
      Caption = #1060#1072#1081#1083#1099' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103' '#1079#1072#1075#1088#1091#1078#1077#1085#1099'!'
    end
    object LangNoUpdate: TMenuItem
      Caption = #1053#1077#1090' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103
    end
    object LangServerVersion: TMenuItem
      Caption = #1042#1077#1088#1089#1080#1103' '#1085#1072' '#1089#1077#1088#1074#1077#1088#1077':'
    end
    object LangVersionComputer: TMenuItem
      Caption = #1042#1077#1088#1089#1080#1103' '#1085#1072' '#1082#1086#1084#1087#1100#1102#1090#1077#1088#1077':'
    end
    object LangUpdateAvailable: TMenuItem
      Caption = #1044#1086#1089#1090#1091#1087#1085#1086' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1077
    end
    object LangSize: TMenuItem
      Caption = #1056#1072#1079#1084#1077#1088': '
    end
    object LangLaunchApp: TMenuItem
      Caption = #1047#1072#1087#1091#1089#1082' '#1087#1088#1080#1083#1086#1078#1077#1085#1080#1103':'
    end
    object LangCloseApp: TMenuItem
      Caption = #1047#1072#1082#1088#1099#1090#1080#1077' '#1087#1088#1086#1075#1088#1072#1084#1084#1099':'
    end
    object LangDownload: TMenuItem
      Caption = #1057#1082#1072#1095#1072#1090#1100
    end
    object LangCancel: TMenuItem
      Caption = #1054#1090#1084#1077#1085#1072
    end
    object LangServerError: TMenuItem
      Caption = #1054#1096#1080#1073#1082#1072' '#1087#1086#1076#1082#1083#1102#1095#1077#1085#1080#1103' '#1082' '#1089#1077#1088#1074#1077#1088#1091':'
    end
    object LangVersionChanges: TMenuItem
      Caption = #1063#1090#1086' '#1085#1086#1074#1086#1075#1086' '#1074' '#1087#1086#1089#1083#1077#1076#1085#1077#1081' '#1074#1077#1088#1089#1080#1080':'
    end
  end
  object IdHTTP1: TIdHTTP
    OnWork = IdHTTP1Work
    OnWorkBegin = IdHTTP1WorkBegin
    OnWorkEnd = IdHTTP1WorkEnd
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 112
    Top = 16
  end
  object IdHTTP2: TIdHTTP
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 40
    Top = 16
  end
  object TaskbarUpdate: TTaskbar
    TaskBarButtons = <>
    ProgressState = Normal
    TabProperties = []
    Left = 176
    Top = 16
  end
end
