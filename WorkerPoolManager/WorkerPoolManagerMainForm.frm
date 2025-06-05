object frmWorkerPoolManagerMain: TfrmWorkerPoolManagerMain
  Left = 373
  Height = 531
  Top = 185
  Width = 744
  Caption = 'WorkerPoolManager'
  ClientHeight = 531
  ClientWidth = 744
  LCLVersion = '8.4'
  OnCreate = FormCreate
  object memInfo: TMemo
    Left = 0
    Height = 97
    Top = 0
    Width = 737
    Anchors = [akTop, akLeft, akRight]
    Lines.Strings = (
      'This manager sends "remote execution" commands to UIClickers, running automatically (on startup) on machines, to start MQTT brokers, including this machine.'
      'As a rule of thumb, every plugin will be part of a worker pool (with its own unique PoolID), which connects to one broker.'
      'Multiple brokers can be started on a machine. To avoid routing congestion, multiple machines with brokers can be used.'
      'If every user has its own allocated broker, then there is no need for pool IDs.'
      'Setting/updating a machine to online state, from PS:  Invoke-WebRequest -Uri http://127.0.0.1:11884/MachineOnline?MachineOS=Win'
    )
    TabOrder = 0
    WordWrap = False
  end
  object vstMachines: TVirtualStringTree
    Left = 0
    Height = 197
    Top = 104
    Width = 737
    Anchors = [akTop, akLeft, akRight]
    DefaultText = 'Node'
    Header.AutoSizeIndex = 0
    Header.Columns = <    
      item
        MinWidth = 30
        Position = 0
        Text = '#'
      end    
      item
        MinWidth = 100
        Position = 1
        Text = 'Address'
        Width = 100
      end    
      item
        MinWidth = 150
        Position = 2
        Text = 'Resource type'
        Width = 150
      end    
      item
        MinWidth = 100
        Position = 3
        Text = 'OS'
        Width = 100
      end    
      item
        MinWidth = 100
        Position = 4
        Text = 'State'
        Width = 100
      end>
    Header.Height = 23
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.Style = hsFlatButtons
    TabOrder = 1
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnGetText = vstMachinesGetText
  end
  object grpSettings: TGroupBox
    Left = 0
    Height = 105
    Top = 304
    Width = 408
    Caption = 'Settings'
    ClientHeight = 85
    ClientWidth = 404
    TabOrder = 2
    object spnedtBrokerCountPerMachine: TSpinEdit
      Left = 8
      Height = 23
      Top = 23
      Width = 154
      EditorEnabled = False
      MaxValue = 40
      MinValue = 1
      TabOrder = 0
      Value = 2
    end
    object lblBrokerCountPerMachine: TLabel
      Left = 8
      Height = 15
      Top = 8
      Width = 137
      Caption = 'Broker count per machine'
    end
    object spnedtMinBrokerPort: TSpinEdit
      Left = 176
      Height = 23
      Hint = 'Broker port allocation starts at this value and it ends, depending on count.'
      Top = 23
      Width = 104
      EditorEnabled = False
      MaxValue = 65534
      MinValue = 20000
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Value = 20000
    end
    object lblMinBrokerPort: TLabel
      Left = 176
      Height = 15
      Top = 7
      Width = 83
      Caption = 'Min broker port'
    end
  end
  object btnAddMachine: TButton
    Left = 531
    Height = 25
    Top = 368
    Width = 136
    Caption = 'btnAddMachine'
    TabOrder = 3
    OnClick = btnAddMachineClick
  end
  object btnStartTwoBrokers: TButton
    Left = 528
    Height = 25
    Top = 397
    Width = 139
    Caption = 'btnStartTwoBrokers'
    TabOrder = 4
    OnClick = btnStartTwoBrokersClick
  end
  object memLog: TMemo
    Left = 0
    Height = 97
    Top = 432
    Width = 744
    Anchors = [akLeft, akRight, akBottom]
    Font.Height = -13
    Font.Name = 'Courier New'
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 5
    WordWrap = False
  end
  object btnSendPoolCredentialsToLocal: TButton
    Left = 544
    Height = 25
    Top = 328
    Width = 192
    Caption = 'Send Pool credentials to local'
    TabOrder = 6
    OnClick = btnSendPoolCredentialsToLocalClick
  end
  object IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool
    MaxThreads = 30
    PoolSize = 10
    Left = 320
    Top = 168
  end
  object IdHTTPServerPlugins: TIdHTTPServer
    Bindings = <>
    DefaultPort = 11883
    ListenQueue = 30
    OnConnect = IdHTTPServerPluginsConnect
    OnException = IdHTTPServerPluginsException
    ReuseSocket = rsFalse
    UseNagle = False
    Scheduler = IdSchedulerOfThreadPool1
    KeepAlive = True
    OnCommandGet = IdHTTPServerPluginsCommandGet
    Left = 320
    Top = 216
  end
  object IdHTTPServerResources: TIdHTTPServer
    Bindings = <>
    DefaultPort = 11884
    ListenQueue = 30
    OnConnect = IdHTTPServerResourcesConnect
    OnException = IdHTTPServerResourcesException
    ReuseSocket = rsFalse
    UseNagle = False
    Scheduler = IdSchedulerOfThreadPool2
    KeepAlive = True
    OnCommandGet = IdHTTPServerResourcesCommandGet
    Left = 496
    Top = 216
  end
  object IdSchedulerOfThreadPool2: TIdSchedulerOfThreadPool
    MaxThreads = 30
    PoolSize = 10
    Left = 496
    Top = 168
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 420
    Top = 355
  end
  object tmrFSM: TTimer
    OnTimer = tmrFSMTimer
    Left = 420
    Top = 304
  end
end
