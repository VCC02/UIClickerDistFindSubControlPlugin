object frmWorkerPoolManagerMain: TfrmWorkerPoolManagerMain
  Left = 373
  Height = 531
  Top = 185
  Width = 744
  Caption = 'WorkerPoolManager'
  ClientHeight = 531
  ClientWidth = 744
  LCLVersion = '8.4'
  OnClose = FormClose
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
        MinWidth = 150
        Position = 4
        Text = 'Dist Address(es)'
        Width = 150
      end    
      item
        MinWidth = 300
        Position = 5
        Text = 'State'
        Width = 300
      end>
    Header.Height = 23
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.Style = hsFlatButtons
    TabOrder = 1
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnGetText = vstMachinesGetText
  end
  object grpSettings: TGroupBox
    Left = 0
    Height = 120
    Top = 304
    Width = 424
    Caption = 'Settings'
    ClientHeight = 100
    ClientWidth = 420
    TabOrder = 2
    object spnedtBrokerCountPerMachine: TSpinEdit
      Left = 8
      Height = 23
      Hint = 'This also means "Dist" machines count per worker machine.'
      Top = 24
      Width = 146
      EditorEnabled = False
      MaxValue = 40
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Value = 1
    end
    object lblBrokerCountPerMachine: TLabel
      Left = 8
      Height = 15
      Top = 8
      Width = 137
      Caption = 'Broker count per machine'
    end
    object spnedtMinBrokerPort: TSpinEdit
      Left = 168
      Height = 23
      Hint = 'Broker port allocation starts at this value and it ends, depending on count.'
      Top = 24
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
      Left = 168
      Height = 15
      Top = 8
      Width = 83
      Caption = 'Min broker port'
    end
    object lblServiceUIClickerPort: TLabel
      Left = 288
      Height = 15
      Top = 8
      Width = 112
      Caption = 'Service UIClicker port'
    end
    object spnedtServiceUIClickerPort: TSpinEdit
      Left = 288
      Height = 23
      Hint = 'Service UIClicker port.'#13#10'This is theUIClicker instance, running on the broker/worker machine,'#13#10'which is used for starting brokers, workers and other UIClickers.'
      Top = 24
      Width = 120
      MaxValue = 65534
      MinValue = 500
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Value = 55444
      OnChange = spnedtServiceUIClickerPortChange
    end
    object spnedtDistUIClickerPort: TSpinEdit
      Left = 288
      Height = 23
      Hint = 'Dist UIClicker port.'#13#10'This is theUIClicker instance, running the UIClickerDistFindSubControl plugin and the PoolClient plugin.'#13#10'It is not running on the same machine as the brokers and workers.'
      Top = 72
      Width = 120
      MaxValue = 65534
      MinValue = 500
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Value = 5444
      OnChange = spnedtDistUIClickerPortChange
    end
    object lblDistUIClickerPort: TLabel
      Left = 288
      Height = 15
      Top = 56
      Width = 95
      Caption = 'Dist UIClicker port'
    end
    object spnedtMaxWorkerMachineCount: TSpinEdit
      Left = 8
      Height = 23
      Hint = 'This applies to machines with both brokers and workers and also on worker-only machines.'#13#10'It is the maximum number of allowed items in the above table.'
      Top = 72
      Width = 146
      EditorEnabled = False
      MaxValue = 200
      MinValue = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Value = 2
    end
    object lblMaxWorkerMachineCount: TLabel
      Left = 8
      Height = 15
      Top = 56
      Width = 144
      Caption = 'Max worker machine count'
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
    TabOrder = 4
    WordWrap = False
  end
  object btnSendPoolCredentialsToLocal: TButton
    Left = 544
    Height = 25
    Hint = 'ToDo: Move feature to API.'#13#10'The address of this ("Dist") UIClicker is not in this list of machines.'
    Top = 328
    Width = 192
    Caption = 'Send Pool credentials to local'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = btnSendPoolCredentialsToLocalClick
  end
  object btnGetListeningProcesses: TButton
    Left = 556
    Height = 25
    Hint = 'Get processes from selected machine.'
    Top = 399
    Width = 148
    Caption = 'Get listening processes'
    TabOrder = 6
    OnClick = btnGetListeningProcessesClick
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
