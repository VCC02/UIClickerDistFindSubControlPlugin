{
    Copyright (C) 2025 VCC
    creation date: 18 May 2024
    initial release date: 19 May 2024

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit DistFindSubControlPluginProperties;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ClickerUtils, DistFindSubControlCommonConsts;


const
  CMaxRequiredSubControlActions = 1;
  CAdditionalPropertiesCount = 24;
  CPropertiesCount = CMaxRequiredSubControlActions + CAdditionalPropertiesCount;

  CFindSubControlActionPropertyIndex = 0;
  CCredentialsFullFileNamePropertyIndex = 1;
  CAddressPropertyIndex = 2;
  CPortPropertyIndex = 3;
  CWorkerQoSPropertyIndex = 4;
  CGetWorkerCapabilitiesTimeoutPropertyIndex = 5;
  CFindSubControlWorkerTimeoutPropertyIndex = 6;
  CFindSubControlTimeoutDiffPropertyIndex = 7;
  CWorkerCapabilitiesSourcePropertyIndex = 8;
  CLoadWorkerCapabilitiesCacheActionPropertyIndex = 9;
  CSaveWorkerCapabilitiesCacheActionPropertyIndex = 10;

  CTextRenderingOSPropertyIndex = 11;
  CListOfMultiValuePropertyNamesPropertyIndex = 12;
  CUseCompressionPropertyIndex = 13;
  CCompressionAlgorithmPropertyIndex = 14;

  CLzmaEndOfStreamPropertyIndex = 15; //EOS
  CLzmaAlgorithmPropertyIndex = 16;
  CLzmaNumBenchMarkPassesPropertyIndex = 17;
  CLzmaDictionarySizePropertyIndex = 18;
  CLzmaMatchFinderPropertyIndex = 19;
  CLzmaLiteralContextPropertyIndex = 20;
  CLzmaLiteralPosBitsPropertyIndex = 21;
  CLzmaPosBitsPropertyIndex = 22;
  CLzmaFastBytesPropertyIndex = 23;

  CVariablesForWorkersPropertyIndex = 24;

  CFindSubControlActionPropertyName = 'FindSubControlAction';
  CCredentialsFullFileNamePropertyName = 'CredentialsFullFileName';  //for connection to broker
  CAddressPropertyName = 'Address';
  CPortPropertyName = 'Port';
  CWorkerQoSPropertyName = 'WorkerQoS';  //QoS between plugin and workers
  CGetWorkerCapabilitiesTimeoutPropertyName = 'GetWorkerCapabilitiesTimeout';  //waiting timeout for every worker to present its capabilites
  CFindSubControlWorkerTimeoutPropertyName = 'FindSubControlWorkerTimeout'; //waiting timeout for every worker processing
  CFindSubControlTimeoutDiffPropertyName = 'FindSubControlTimeoutDiff';
  CWorkerCapabilitiesSourcePropertyName = 'WorkerCapabilitiesSource';
  CLoadWorkerCapabilitiesCacheActionPropertyName = 'LoadWorkerCapabilitiesCacheAction';  //can be a "LoadSetVarFromFile", a "CallTemplate" etc.
  CSaveWorkerCapabilitiesCacheActionPropertyName = 'SaveWorkerCapabilitiesCacheAction';  //can be a "SaveSetVarToFile", a "CallTemplate" etc.

  CTextRenderingOSPropertyName = 'TextRenderingOS'; //Enum-like property, which selects betwen 'Win', 'Lin' and 'Win+Lin'.
  CListOfMultiValuePropertyNamesPropertyName = 'ListOfMultiValuePropertyNames';  //Used in case CFindSubControlActionPropertyName points to a plugin action. It tells this plugin what properties to use, to split the action.
  CUseCompressionPropertyName = 'UseCompression';
  CCompressionAlgorithmPropertyName = 'CompressionAlgorithm';

  CLzmaEndOfStreamPropertyName = 'LzmaEndOfStream'; //EOS
  CLzmaAlgorithmPropertyName = 'LzmaAlgorithm';
  CLzmaNumBenchMarkPassesPropertyName = 'LzmaNumBenchMarkPasses';
  CLzmaDictionarySizePropertyName = 'LzmaDictionarySize';
  CLzmaMatchFinderPropertyName = 'LzmaMatchFinder';
  CLzmaLiteralContextPropertyName = 'LzmaLiteralContext';
  CLzmaLiteralPosBitsPropertyName = 'LzmaLiteralPosBits';
  CLzmaPosBitsPropertyName = 'LzmaPosBits';
  CLzmaFastBytesPropertyName = 'LzmaFastBytes';

  CVariablesForWorkersPropertyName = 'VariablesForWorkers';

  CReqCapOperation_wcsReqCapAndFindSubControl = 'wcsReqCapAndFindSubControl';
  CReqCapOperation_wcsReqCapAndGetFonts = 'wcsReqCapAndGetFonts';
  CReqCapOperation_wcsReqCapAndGetFontsAndFindSubControl = 'wcsReqCapAndGetFontsAndFindSubControl';  //this is the slowest, but can do all 3 operations in a single action execution
  CReqCapOperation_wcsReqCapAndUpdateCache = 'wcsReqCapAndUpdateCache';
  CReqCapOperation_wcsLoadCacheAndFindSubControl = 'wcsLoadCacheAndFindSubControl';


  CRequiredSubControlPropertyNames: array[0..CPropertiesCount - 1] of string = (  //these are the expected FindSubControl property names, configured in plugin properties
    CFindSubControlActionPropertyName,

    CCredentialsFullFileNamePropertyName,
    CAddressPropertyName,
    CPortPropertyName,
    CWorkerQoSPropertyName,
    CGetWorkerCapabilitiesTimeoutPropertyName,
    CFindSubControlWorkerTimeoutPropertyName,
    CFindSubControlTimeoutDiffPropertyName,
    CWorkerCapabilitiesSourcePropertyName,
    CLoadWorkerCapabilitiesCacheActionPropertyName,
    CSaveWorkerCapabilitiesCacheActionPropertyName,

    CTextRenderingOSPropertyName,
    CListOfMultiValuePropertyNamesPropertyName,
    CUseCompressionPropertyName,
    CCompressionAlgorithmPropertyName,

    CLzmaEndOfStreamPropertyName,
    CLzmaAlgorithmPropertyName,
    CLzmaNumBenchMarkPassesPropertyName,
    CLzmaDictionarySizePropertyName,
    CLzmaMatchFinderPropertyName,
    CLzmaLiteralContextPropertyName,
    CLzmaLiteralPosBitsPropertyName,
    CLzmaPosBitsPropertyName,
    CLzmaFastBytesPropertyName,

    CVariablesForWorkersPropertyName
  );

  //property details: (e.g. enum options, hints, icons, menus, min..max spin intervals etc)

  //See TOIEditorType datatype from ObjectInspectorFrame.pas, for valid values
  CRequiredSubControlPropertyTypes: array[0..CPropertiesCount - 1] of string = (
    'TextWithArrow', //FindSubControlActionPropertyName

    'FilePath', //CredentialsFullFileName
    'TextWithArrow', //Address
    'SpinText',      //Port
    'SpinText',      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    'TextWithArrow', //GetWorkerCapabilitiesTimeout
    'TextWithArrow', //FindSubControlWorkerTimeout
    'SpinText',      //FindSubControlTimeoutDiff
    'EnumCombo',     //WorkerCapabilitiesSource
    'TextWithArrow', //LoadWorkerCapabilitiesCacheAction
    'TextWithArrow', //SaveWorkerCapabilitiesCacheAction

    'EnumCombo',
    'TextWithArrow', //ListOfMultiValue
    'BooleanCombo',  //UseCompression
    'EnumCombo',     //CompressionAlgorithm

    'BooleanCombo',  //LzmaEndOfStream,
    'EnumCombo',     //LzmaAlgorithm,
    'SpinText',      //LzmaNumBenchMarkPasses,
    'TextWithArrow', //LzmaDictionarySize,
    'EnumCombo',     //LzmaMatchFinder,
    'SpinText',      //LzmaLiteralContext,
    'EnumCombo',     //LzmaLiteralPosBits,
    'EnumCombo',     //LzmaPosBits,
    'SpinText',      //LzmaFastBytes
    'TextWithArrow'  //VariablesForWorkers
  );

  CRequiredSubControlPropertyDataTypes: array[0..CPropertiesCount - 1] of string = (
    CDTString, //FindSubControlActionPropertyName

    CDTString, //CredentialsFullFileName
    CDTString, //Address
    CDTInteger,      //Port
    CDTInteger,      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    CDTInteger, //GetWorkerCapabilitiesTimeout
    CDTInteger, //FindSubControlWorkerTimeout
    CDTInteger, //FindSubControlTimeoutDiff
    CDTEnum,    //WorkerCapabilitiesSource
    CDTString,  //LoadWorkerCapabilitiesCacheAction
    CDTString,  //SaveWorkerCapabilitiesCacheAction

    CDTEnum, //'EnumCombo',     //TextRenderingOS    - property details (e.g. enum options, hints, icons, menus, min..max spin intervals etc)
    CDTString, //ListOfMultiValue
    CDTBool,   //UseCompression
    CDTEnum,   //CompressionAlgorithm

    CDTBool,    //LzmaEndOfStreamPropertyName,
    CDTEnum,    //LzmaAlgorithmPropertyName,
    CDTInteger, //LzmaNumBenchMarkPassesPropertyName,
    CDTInteger, //LzmaDictionarySizePropertyName,
    CDTEnum,    //LzmaMatchFinderPropertyName,
    CDTInteger, //LzmaLiteralContextPropertyName,
    CDTEnum,    //LzmaLiteralPosBitsPropertyName,
    CDTEnum,    //LzmaPosBitsPropertyName,
    CDTInteger, //LzmaFastBytesPropertyName

    CDTString   //VariablesForWorkers
  );

  CPluginEnumCounts: array[0..CPropertiesCount - 1] of Integer = (
    0, //FindSubControlActionPropertyName

    0, //CredentialsFullFileName
    0, //Address
    0,      //Port
    0,      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    0, //GetWorkerCapabilitiesTimeout
    0, //FindSubControlWorkerTimeout
    0, //FindSubControlTimeoutDiff
    5, //'EnumCombo',     //WorkerCapabilitiesSource
    0, //LoadWorkerCapabilitiesCacheAction
    0, //SaveWorkerCapabilitiesCacheAction

    3, //'EnumCombo',     //TextRenderingOS    - property details (e.g. enum options, hints, icons, menus, min..max spin intervals etc)
    0, //ListOfMultiValue
    0, //UseCompression
    2, //'EnumCombo',     CompressionAlgorithm

    0, //LzmaEndOfStreamPropertyName,
    3, //LzmaAlgorithmPropertyName,
    0, //LzmaNumBenchMarkPassesPropertyName,
    0, //LzmaDictionarySizePropertyName,
    2, //LzmaMatchFinderPropertyName,
    0, //LzmaLiteralContextPropertyName,
    5, //LzmaLiteralPosBitsPropertyName,
    5, //LzmaPosBitsPropertyName,
    0, //LzmaFastBytesPropertyName

    0  //VariablesForWorkers
  );

  CPluginEnumStrings: array[0..CPropertiesCount - 1] of string = (
    '', //FindSubControlActionPropertyName

    '', //CredentialsFullFileName
    '', //Address
    '',      //Port
    '',      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    '', //GetWorkerCapabilitiesTimeout
    '', //FindSubControlWorkerTimeout
    '', //FindSubControlTimeoutDiff
    CReqCapOperation_wcsReqCapAndFindSubControl + #4#5 +
      CReqCapOperation_wcsReqCapAndGetFonts + #4#5 +
      CReqCapOperation_wcsReqCapAndGetFontsAndFindSubControl + #4#5 +
      CReqCapOperation_wcsReqCapAndUpdateCache + #4#5 +
      CReqCapOperation_wcsLoadCacheAndFindSubControl, //WorkerCapabilitiesSource
    '', //LoadWorkerCapabilitiesCacheAction
    '', //SaveWorkerCapabilitiesCacheAction

    CReportedOS_Win + #4#5 + CReportedOS_Lin + #4#5 + CReportedOS_Win + '+' + CReportedOS_Lin + #4#5, //'EnumCombo',     //TextRenderingOS    - property details (e.g. enum options, hints, icons, menus, min..max spin intervals etc)
    '', //ListOfMultiValue
    '',  //UseCompression
    'Zlib' + #4#5 + 'Lzma' + #4#5, //CompressionAlgorithm  'EnumCombo'

    '0', //LzmaEndOfStreamPropertyName,
    '0' + #4#5 + '1' + #4#5 + '2' + #4#5, //LzmaAlgorithmPropertyName,
    '0', //LzmaNumBenchMarkPassesPropertyName,
    '0', //LzmaDictionarySizePropertyName,
    '0' + #4#5 + '1' + #4#5, //LzmaMatchFinderPropertyName,
    '0', //LzmaLiteralContextPropertyName,
    '0' + #4#5 + '1' + #4#5 + '2' + #4#5 + '3' + #4#5 + '4' + #4#5, //LzmaLiteralPosBitsPropertyName,
    '0' + #4#5 + '1' + #4#5 + '2' + #4#5 + '3' + #4#5 + '4' + #4#5, //LzmaPosBitsPropertyName,
    '0', //LzmaFastBytesPropertyName

    ''  //VariablesForWorkers
  );

  CPluginHints: array[0..CPropertiesCount - 1] of string = (
    'Name of a FindSubControl action, from the same template as this plugin, which will be sent and executed remotely.', //FindSubControlActionPropertyName

    'Full file path of a ini file, containing MQTT credentials. The file has a plugin specific format.' + #4#5 + 'Example:' + #4#5#4#5 + '[Credentials]' + #4#5 + 'Username=<MyUsername>' + #4#5 + 'Password=<MyPassword>' + #4#5#4#5 + 'If the path starts with "Mem:\", the file is searched in the In-Mem FS for plugins.', //CredentialsFullFileName
    'Hostname or IP address of the MQTT broker, where this plugin connects to.', //Address
    'Port number of the MQTT broker, where this plugin connects to.',      //Port
    'Quality of service, used by MQTT communication. Can be 1 or 2.',      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    'Timeout in ms, until the plugin no longer waits for all workers to present their capabilities.' + #4#5 + 'Workers which miss this timeout won''t receive work.',
    'Timeout in ms, until the plugin no longer waits for a remote worker to return the execution results of FindSubControl action.' + #4#5 + 'The actual FindSubControl action is configured to have a timeout, e.g. 1500ms less than this worker waiting timeout.' + #4#5 + 'For example, on a 2000ms FindSubControlWorkerTimeout, the FindSubControl action has only 500ms.' + #4#5 + 'To prevent negative values, the FindSubControl action will have a minimum of 100ms.', //FindSubControlWorkerTimeout
    'Difference between plugin timeout and the actual FindSubControl execution timeout.' + #4#5 + 'Default value: 2500.', //FindSubControlTimeoutDiff
    '- When set to ' + CReqCapOperation_wcsReqCapAndFindSubControl + ', the plugin requests capabilities from workers, then it requests the FindSubControl operation.' + #4#5 +
      '- When set to ' + CReqCapOperation_wcsReqCapAndGetFonts + ', the plugin requests the list of fonts, updates back some variables and exits.' + #4#5 +
      '- When set to ' + CReqCapOperation_wcsReqCapAndGetFontsAndFindSubControl + ', the plugin requests capabilities and the list of fonts, updates back some variables and then it requests the FindSubControl operation.' + #4#5 +
      '- When set to ' + CReqCapOperation_wcsReqCapAndUpdateCache + ', the plugin requests capabilities from workers, then it executes the action configured to SaveWorkerCapabilitiesCacheAction.' + #4#5 +
      '- When set to ' + CReqCapOperation_wcsLoadCacheAndFindSubControl + ', the plugin executes the action configured to LoadWorkerCapabilitiesCacheAction, then it requests the FindSubControl operation.', //WorkerCapabilitiesSource
    'Name of a "LoadSetVarFromFile", "CallTemplate" or "Plugin" action, which loads the worker capabilities into specific variables.', //LoadWorkerCapabilitiesCacheAction
    'Name of a "SaveSetVarToFile", "CallTemplate" or "Plugin" action, which saves the worker capabilities into specific variables.', //SaveWorkerCapabilitiesCacheAction

    'Target operating system where this action should be executed.' + #4#5 + 'If this setting matches the existing worker OS, then the FindSubControl action is executed there.' + #4#5 + 'This is useful, because of different rendering settings or different lists of font types.',
    'Used in case the configured FindSubControl action points to a plugin action.' + #4#5 + 'It tells this plugin what properties to use, to distribute the action between multiple workers.' + #4#5 + 'If a FindSubControl action is configured, this property is ignored and the action is distributed by Txt profiles, Bmp files and Pmtv files.',
    'Enables compression of transferred bitmaps.', //UseCompression
    'Compression algorithm',

    'Default value: False.', //EndOfStream
    'Default value: 2.',     //Algorithm
    'Default value: 10. Valid range: [0..10].',    //LzmaNumBenchMarkPassesPropertyName,
    'Default value: 23.' + #4#5 + 'E.g.: A value of 1048576 results in both a high compression ratio and a fast compression.',    //LzmaDictionarySizePropertyName,
    'Default value: 1.',     //LzmaMatchFinderPropertyName,
    'Default value: 3. Valid range: [0..8].',     //LzmaLiteralContextPropertyName,
    'Default value: 0.',     //LzmaLiteralPosBitsPropertyName,
    'Default value: 2.',     //LzmaPosBitsPropertyName,
    'Default value: 128. Valid range: [5, 273].' + #4#5 + 'E.g.: A value of 273 gives the best compression. A value of 5 results in the fastest compression.',   //LzmaFastBytesPropertyName

    'Comma-separated list of variables, which will be sent to workers, before the actual action FindSubControl execution.'   //VariablesForWorkers
  );

  CPropertyEnabled: array[0..CPropertiesCount - 1] of string = (  // The 'PropertyValue[<index>]' replacement uses indexes from the following array only. It doesn't count fixed properties.
    '', //FindSubControlActionPropertyName                        // If empty string, the property is unconditionally enabled. For available operators, see CComp constans in ClickerUtils.pas.

    '', //CredentialsFullFileName
    '', //Address
    '',      //Port
    '',      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    '', //GetWorkerCapabilitiesTimeout
    '', //FindSubControlWorkerTimeout
    '', //FindSubControlTimeoutDiff                                  [7]
    '', //WorkerCapabilitiesSource                                   [8]
    'PropertyValue[8]==wcsLoadCacheAndFindSubControl', //LoadWorkerCapabilitiesCacheAction   [9]
    'PropertyValue[8]==wcsReqCapAndUpdateCache', //SaveWorkerCapabilitiesCacheAction                          [10]

    '', //'EnumCombo',     //TextRenderingOS    - EnumCombo cannot be used until the plugin API allows defining property details (e.g. enum options, hints, icons, menus, min..max spin intervals etc)
    '', //ListOfMultiValue
    '', //UseCompression    //this is  [13]
    'PropertyValue[13]==True',  //CompressionAlgorithm    //this is  [14]

    'PropertyValue[13]==True' + #5#6 + 'PropertyValue[14]==Lzma',  //LzmaEndOfStreamPropertyName,
    'PropertyValue[13]==True' + #5#6 + 'PropertyValue[14]==Lzma',  //LzmaAlgorithmPropertyName,
    'PropertyValue[13]==True' + #5#6 + 'PropertyValue[14]==Lzma',  //LzmaNumBenchMarkPassesPropertyName,
    'PropertyValue[13]==True' + #5#6 + 'PropertyValue[14]==Lzma',  //LzmaDictionarySizePropertyName,
    'PropertyValue[13]==True' + #5#6 + 'PropertyValue[14]==Lzma',  //LzmaMatchFinderPropertyName,
    'PropertyValue[13]==True' + #5#6 + 'PropertyValue[14]==Lzma',  //LzmaLiteralContextPropertyName,
    'PropertyValue[13]==True' + #5#6 + 'PropertyValue[14]==Lzma',  //LzmaLiteralPosBitsPropertyName,
    'PropertyValue[13]==True' + #5#6 + 'PropertyValue[14]==Lzma',  //LzmaPosBitsPropertyName,
    'PropertyValue[13]==True' + #5#6 + 'PropertyValue[14]==Lzma',  //LzmaFastBytesPropertyName

    ''  //VariablesForWorkers
  );

  CPluginDefaultValues: array[0..CPropertiesCount - 1] of string = (
    '', //FindSubControlActionPropertyName

    '', //CredentialsFullFileName
    '127.0.0.1', //Address
    '1883',      //Port
    '1',      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    '500',  //GetWorkerCapabilitiesTimeout
    '2000', //FindSubControlWorkerTimeout
    '2500', //FindSubControlTimeoutDiff
    CReqCapOperation_wcsReqCapAndGetFontsAndFindSubControl, //WorkerCapabilitiesSource
    '', //LoadWorkerCapabilitiesCacheAction
    '', //SaveWorkerCapabilitiesCacheAction

    CReportedOS_Win + '+' + CReportedOS_Lin, //TextRenderingOS    - property details
    '', //ListOfMultiValue
    'True',  //UseCompression
    'Lzma', //CompressionAlgorithm  'EnumCombo'

    'False', //LzmaEndOfStreamPropertyName,
    '2', //LzmaAlgorithmPropertyName,
    '10', //LzmaNumBenchMarkPassesPropertyName,
    '1048576', //LzmaDictionarySizePropertyName,
    '1', //LzmaMatchFinderPropertyName,
    '3', //LzmaLiteralContextPropertyName,
    '0', //LzmaLiteralPosBitsPropertyName,
    '0', //LzmaPosBitsPropertyName,
    '5', //LzmaFastBytesPropertyName
    '$Control_Handle$,$Control_Left$,$Control_Top$,$Control_Right$,$Control_Bottom$,$Control_Width$,$Control_Height$'   //VariablesForWorkers
  );


function FillInPropertyDetails: string;


implementation


uses
  ClickerActionPlugins;


function FillInPropertyDetails: string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to CPropertiesCount - 1 do
    Result := Result + CRequiredSubControlPropertyNames[i] + '=' + CRequiredSubControlPropertyTypes[i] + #8#7 +
                       CPluginPropertyAttr_DataType + '=' + CRequiredSubControlPropertyDataTypes[i] + #8#7 +
                       CPluginPropertyAttr_EnumCounts + '=' + IntToStr(CPluginEnumCounts[i]) + #8#7 +
                       CPluginPropertyAttr_EnumStrings + '=' + CPluginEnumStrings[i] + #8#7 +
                       CPluginPropertyAttr_Hint + '=' + CPluginHints[i] + #8#7 +
                       CPluginPropertyAttr_Enabled + '=' + CPropertyEnabled[i] + #8#7 +
                       CPluginPropertyAttr_DefaultValue + '=' + CPluginDefaultValues[i] + #8#7 +
                       #13#10;
end;

end.

