object frmFindSubControlWorkerMain: TfrmFindSubControlWorkerMain
  Left = 373
  Height = 312
  Top = 185
  Width = 1254
  Caption = 'FindSubControl Worker'
  ClientHeight = 312
  ClientWidth = 1254
  Constraints.MinHeight = 312
  Constraints.MinWidth = 1200
  LCLVersion = '8.4'
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  object memLog: TMemo
    Left = 8
    Height = 168
    Top = 8
    Width = 881
    Anchors = [akTop, akLeft, akBottom]
    Lines.Strings = (
      'memLog'
    )
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object imgFindSubControlBackground: TImage
    Left = 896
    Height = 90
    Top = 8
    Width = 90
    AutoSize = True
  end
  object grpMQTT: TGroupBox
    Left = 8
    Height = 70
    Top = 184
    Width = 480
    Anchors = [akLeft, akBottom]
    Caption = 'MQTT'
    ClientHeight = 50
    ClientWidth = 476
    TabOrder = 1
    object lbeAddress: TLabeledEdit
      Left = 8
      Height = 23
      Top = 19
      Width = 80
      Anchors = [akLeft, akBottom]
      EditLabel.Height = 15
      EditLabel.Width = 80
      EditLabel.Caption = 'Address'
      EditLabel.ParentFont = False
      ParentFont = False
      TabOrder = 0
      Text = '127.0.0.1'
    end
    object lbePort: TLabeledEdit
      Left = 96
      Height = 23
      Top = 19
      Width = 45
      Anchors = [akLeft, akBottom]
      EditLabel.Height = 15
      EditLabel.Width = 45
      EditLabel.Caption = 'Port'
      EditLabel.ParentFont = False
      ParentFont = False
      TabOrder = 1
      Text = '1883'
    end
    object lbeClientID: TLabeledEdit
      Left = 144
      Height = 23
      Top = 19
      Width = 240
      Anchors = [akLeft, akBottom]
      Color = 16640708
      EditLabel.Height = 15
      EditLabel.Width = 240
      EditLabel.Caption = 'ClientID'
      ReadOnly = True
      TabOrder = 2
    end
    object btnConnection: TButton
      Left = 392
      Height = 25
      Top = 17
      Width = 77
      Caption = 'Disconnect'
      TabOrder = 3
      OnClick = btnConnectionClick
    end
    object lblBrokerConnectionStatus: TLabel
      Left = 344
      Height = 13
      Top = 0
      Width = 120
      Caption = 'Status: disconnected'
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'DejaVu Sans'
      Font.Quality = fqNonAntialiased
      ParentFont = False
    end
  end
  object grpExtServer: TGroupBox
    Left = 488
    Height = 70
    Top = 184
    Width = 321
    Anchors = [akLeft, akBottom]
    Caption = 'ExtServer'
    ClientHeight = 50
    ClientWidth = 317
    Enabled = False
    TabOrder = 2
    object lbeExtServerPort: TLabeledEdit
      Left = 8
      Height = 23
      Top = 19
      Width = 45
      Anchors = [akLeft, akBottom]
      EditLabel.Height = 15
      EditLabel.Width = 45
      EditLabel.Caption = 'Port'
      EditLabel.ParentFont = False
      ParentFont = False
      TabOrder = 0
      Text = '43444'
    end
    object chkExtServerActive: TCheckBox
      Left = 64
      Height = 19
      Top = 19
      Width = 51
      Caption = 'Active'
      TabOrder = 1
      OnChange = chkExtServerActiveChange
    end
    object lblExtServerInfo: TLabel
      Left = 64
      Height = 15
      Top = 0
      Width = 249
      Caption = 'Local UIClicker requests BMPs from  this server.'
    end
    object chkExtServerKeepAlive: TCheckBox
      Left = 120
      Height = 19
      Top = 19
      Width = 71
      Caption = 'Keep alive'
      TabOrder = 2
    end
    object lblServerInfo: TLabel
      Left = 80
      Height = 13
      Top = 37
      Width = 143
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Server module is inactive'
      Font.CharSet = ANSI_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Pitch = fpVariable
      Font.Quality = fqDraft
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
  end
  object lbeUIClickerPort: TLabeledEdit
    Left = 817
    Height = 23
    Top = 231
    Width = 72
    Anchors = [akLeft, akBottom]
    EditLabel.Height = 15
    EditLabel.Width = 72
    EditLabel.Caption = 'UIClickerPort'
    EditLabel.ParentFont = False
    ParentFont = False
    TabOrder = 3
    Text = '33444'
    OnChange = lbeUIClickerPortChange
  end
  object lbeLatestWork: TLabeledEdit
    Left = 8
    Height = 23
    Top = 280
    Width = 480
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    EditLabel.Height = 15
    EditLabel.Width = 480
    EditLabel.Caption = 'Latest work'
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 4
  end
  object btnGetListOfFonts: TButton
    Left = 496
    Height = 25
    Hint = 'For testing only.'
    Top = 278
    Width = 128
    Anchors = [akLeft, akBottom]
    Caption = 'Get ListOfFonts'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = btnGetListOfFontsClick
  end
  object lbeUIClickerPath: TLabeledEdit
    Left = 648
    Height = 23
    Hint = 'Required by FileProvider.'#13#10'If not set, bmp, png and pmtv files, used by FindSubControl, which depend on $AppDir$ replacement, won''t be sent to UIClicker.'
    Top = 280
    Width = 208
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    EditLabel.Height = 15
    EditLabel.Width = 208
    EditLabel.Caption = 'UIClicker path'
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 6
  end
  object btnBrowseUIClickerPath: TButton
    Left = 864
    Height = 25
    Top = 278
    Width = 25
    Anchors = [akLeft, akBottom]
    Caption = '...'
    TabOrder = 7
    OnClick = btnBrowseUIClickerPathClick
  end
  object chkCacheFonts: TCheckBox
    Left = 813
    Height = 19
    Top = 184
    Width = 81
    Anchors = [akLeft, akBottom]
    Caption = 'Cache fonts'
    TabOrder = 8
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 268
    Top = 124
  end
  object tmrProcessLog: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrProcessLogTimer
    Left = 348
    Top = 124
  end
  object tmrProcessRecData: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrProcessRecDataTimer
    Left = 452
    Top = 124
  end
  object IdTCPClient1: TIdTCPClient
    ConnectTimeout = 0
    Port = 0
    ReadTimeout = -1
    UseNagle = False
    Left = 84
    Top = 124
  end
  object tmrSubscribe: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmrSubscribeTimer
    Left = 744
    Top = 124
  end
  object tmrConnect: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = tmrConnectTimer
    Left = 640
    Top = 124
  end
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    DefaultPort = 43444
    ListenQueue = 30
    OnConnect = IdHTTPServer1Connect
    OnException = IdHTTPServer1Exception
    ReuseSocket = rsFalse
    UseNagle = False
    Scheduler = IdSchedulerOfThreadPool1
    KeepAlive = True
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 744
    Top = 80
  end
  object IdDecoderMIME1: TIdDecoderMIME
    FillChar = '='
    Left = 744
    Top = 24
  end
  object IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool
    MaxThreads = 30
    PoolSize = 10
    Left = 600
    Top = 80
  end
  object IdHTTP1: TIdHTTP
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
    Left = 904
    Top = 144
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Executables (*.exe;*)|*.exe;*|All files (*.*)|*.*'
    Left = 940
    Top = 262
  end
  object tmrReconnectFileProvider: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = tmrReconnectFileProviderTimer
    Left = 911
    Top = 221
  end
  object IdHTTPServer2: TIdHTTPServer
    Bindings = <>
    DefaultPort = 8880
    OnConnect = IdHTTPServer2Connect
    OnException = IdHTTPServer2Exception
    Scheduler = IdSchedulerOfThreadPool2
    KeepAlive = True
    OnCommandGet = IdHTTPServer2CommandGet
    Left = 984
    Top = 144
  end
  object tmrPing: TTimer
    Enabled = False
    Interval = 20000
    OnTimer = tmrPingTimer
    Left = 452
    Top = 56
  end
  object IdSchedulerOfThreadPool2: TIdSchedulerOfThreadPool
    MaxThreads = 30
    PoolSize = 10
    Left = 984
    Top = 96
  end
end
