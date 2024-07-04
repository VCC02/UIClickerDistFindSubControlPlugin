object frmFindSubControlWorkerMain: TfrmFindSubControlWorkerMain
  Left = 373
  Height = 251
  Top = 185
  Width = 1254
  Caption = 'FindSubControl Worker'
  ClientHeight = 251
  ClientWidth = 1254
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
    Top = 180
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
      Width = 256
      Anchors = [akLeft, akBottom]
      Color = 16640708
      EditLabel.Height = 15
      EditLabel.Width = 256
      EditLabel.Caption = 'ClientID'
      ReadOnly = True
      TabOrder = 2
    end
    object btnDisconnect: TButton
      Left = 408
      Height = 25
      Top = 17
      Width = 69
      Caption = 'Disconnect'
      TabOrder = 3
      OnClick = btnDisconnectClick
    end
  end
  object grpExtServer: TGroupBox
    Left = 488
    Height = 70
    Top = 181
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
    Top = 216
    Width = 72
    Anchors = [akLeft, akBottom]
    EditLabel.Height = 15
    EditLabel.Width = 72
    EditLabel.Caption = 'UIClickerPort'
    EditLabel.ParentFont = False
    ParentFont = False
    TabOrder = 3
    Text = '33444'
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
    Interval = 10
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
end
