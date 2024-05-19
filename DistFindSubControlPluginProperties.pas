{
    Copyright (C) 2024 VCC
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
  Classes, SysUtils, ClickerUtils;


const
  CMaxRequiredSubControlActions = 1;
  CAdditionalPropertiesCount = 18;
  CPropertiesCount = CMaxRequiredSubControlActions + CAdditionalPropertiesCount;

  CFindSubControlActionPropertyIndex = 0;
  CCredentialsFullFileNamePropertyIndex = 1;
  CAddressPropertyIndex = 2;
  CPortPropertyIndex = 3;
  CWorkerQoSPropertyIndex = 4;
  CFindSubControlWorkerTimeoutPropertyIndex = 5;
  CTextRenderingOSPropertyIndex = 6;
  CListOfMultiValuePropertyNamesPropertyIndex = 7;
  CUseCompressionPropertyIndex = 8;
  CCompressionAlgorithmPropertyIndex = 9;

  CLzmaEndOfStreamPropertyIndex = 10; //EOS
  CLzmaAlgorithmPropertyIndex = 11;
  CLzmaNumBenchMarkPassesPropertyIndex = 12;
  CLzmaDictionarySizePropertyIndex = 13;
  CLzmaMatchFinderPropertyIndex = 14;
  CLzmaLiteralContextPropertyIndex = 15;
  CLzmaLiteralPosBitsPropertyIndex = 16;
  CLzmaPosBitsPropertyIndex = 17;
  CLzmaFastBytesPropertyIndex = 18;

  CFindSubControlActionPropertyName = 'FindSubControlAction';
  CCredentialsFullFileNamePropertyName = 'CredentialsFullFileName';  //for connection to broker
  CAddressPropertyName = 'Address';
  CPortPropertyName = 'Port';
  CWorkerQoSPropertyName = 'WorkerQoS';  //QoS between plugin and workers
  CFindSubControlWorkerTimeoutPropertyName = 'FindSubControlWorkerTimeout'; //waiting timeout for every worker processing
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


  CRequiredSubControlPropertyNames: array[0..CPropertiesCount - 1] of string = (  //these are the expected FindSubControl property names, configured in plugin properties
    CFindSubControlActionPropertyName,

    CCredentialsFullFileNamePropertyName,
    CAddressPropertyName,
    CPortPropertyName,
    CWorkerQoSPropertyName,
    CFindSubControlWorkerTimeoutPropertyName,
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
    CLzmaFastBytesPropertyName
  );

  //property details: (e.g. enum options, hints, icons, menus, min..max spin intervals etc)

  //See TOIEditorType datatype from ObjectInspectorFrame.pas, for valid values
  CRequiredSubControlPropertyTypes: array[0..CPropertiesCount - 1] of string = (
    'TextWithArrow', //FindSubControlActionPropertyName

    'TextWithArrow', //CredentialsFullFileName
    'TextWithArrow', //Address
    'SpinText',      //Port
    'SpinText',      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    'TextWithArrow', //FindSubControlWorkerTimeout
    'EnumCombo',
    'TextWithArrow', //ListOfMultiValue
    'BooleanCombo',  //UseCompression
    'EnumCombo',     //CompressionAlgorithm

    'BooleanCombo',  //LzmaEndOfStreamPropertyName,
    'EnumCombo',     //LzmaAlgorithmPropertyName,
    'SpinText',      //LzmaNumBenchMarkPassesPropertyName,
    'TextWithArrow', //LzmaDictionarySizePropertyName,
    'EnumCombo',     //LzmaMatchFinderPropertyName,
    'SpinText',      //LzmaLiteralContextPropertyName,
    'EnumCombo',     //LzmaLiteralPosBitsPropertyName,
    'EnumCombo',     //LzmaPosBitsPropertyName,
    'SpinText'       //LzmaFastBytesPropertyName
  );

  CRequiredSubControlPropertyDataTypes: array[0..CPropertiesCount - 1] of string = (
    CDTString, //FindSubControlActionPropertyName

    CDTString, //CredentialsFullFileName
    CDTString, //Address
    CDTInteger,      //Port
    CDTInteger,      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    CDTInteger, //FindSubControlWorkerTimeout
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
    CDTInteger  //LzmaFastBytesPropertyName
  );

  CPluginEnumCounts: array[0..CPropertiesCount - 1] of Integer = (
    0, //FindSubControlActionPropertyName

    0, //CredentialsFullFileName
    0, //Address
    0,      //Port
    0,      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    0, //FindSubControlWorkerTimeout
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
    0  //LzmaFastBytesPropertyName
  );

  CPluginEnumStrings: array[0..CPropertiesCount - 1] of string = (
    '', //FindSubControlActionPropertyName

    '', //CredentialsFullFileName
    '', //Address
    '',      //Port
    '',      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    '', //FindSubControlWorkerTimeout
    'Win' + #4#5 + 'Lin' + #4#5 + 'Win+Lin' + #4#5, //'EnumCombo',     //TextRenderingOS    - property details (e.g. enum options, hints, icons, menus, min..max spin intervals etc)
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
    '0'  //LzmaFastBytesPropertyName
  );

  CPluginHints: array[0..CPropertiesCount - 1] of string = (
    'Name of a FindSubControl action, from the same template as this plugin, which will be sent and executed remotely.', //FindSubControlActionPropertyName

    'Full file path of a ini file, containing MQTT credentials. It has a plugin specific format.', //CredentialsFullFileName
    'Hostname or IP address of the MQTT broker, where this plugin conntects to.', //Address
    'Port of the MQTT broker, where this plugin conntects to.',      //Port
    'Quality of service, used by MQTT communication. Can be 1 or 2.',      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    'Timeout until the plugin no longer waits for a remote worker to return the execution results of FindSubControl action.', //FindSubControlWorkerTimeout
    'Target operating system where this action should be executed.' + #4#5 + 'If this setting matches the existing worker OS, then the FindSubControl action is executed there.' + #4#5 + 'This is useful, because of different rendering settings or different lists of font types.',
    'Used in case the configured FindSubControl action points to a plugin action.' + #4#5 + 'It tells this plugin what properties to use, to distribute the action between multiple workers.' + #4#5 + 'If a FindSubControl action is configured, this property is ignored and the action is distributed by Txt profiles, Bmp files and Pmtv files.',
    'Enables compression of transferred bitmaps.', //UseCompression
    'Compression algorithm',

    'Default value: False.', //EndOfStream
    'Default value: 2.',     //Algorithm
    'Default value: 10. Valid range: [0..10].',    //LzmaNumBenchMarkPassesPropertyName,
    'Default value: 23.' + #4#5 + 'E.g.: A value of 73000 gives 2503 bytes from a 307KB BMP.',    //LzmaDictionarySizePropertyName,
    'Default value: 1.',     //LzmaMatchFinderPropertyName,
    'Default value: 3. Valid range: [0..8].',     //LzmaLiteralContextPropertyName,
    'Default value: 0.',     //LzmaLiteralPosBitsPropertyName,
    'Default value: 2.',     //LzmaPosBitsPropertyName,
    'Default value: 128. Valid range: [5, 273].' + #4#5 + 'E.g.: A value of 273 gives the best compression.'   //LzmaFastBytesPropertyName
  );

  CPropertyEnabled: array[0..CPropertiesCount - 1] of string = (  // The 'PropertyValue[<index>]' replacement uses indexes from the following array only. It doesn't count fixed properties.
    '', //FindSubControlActionPropertyName                        // If empty string, the property is unconditionally enabled. For available operators, see CComp constans in ClickerUtils.pas.

    '', //CredentialsFullFileName
    '', //Address
    '',      //Port
    '',      //WorkerQoS      (somehow, this should be limited to 1..2    (cannot use 0, because it expects a response)
    '', //FindSubControlWorkerTimeout
    '', //'EnumCombo',     //TextRenderingOS    - EnumCombo cannot be used until the plugin API allows defining property details (e.g. enum options, hints, icons, menus, min..max spin intervals etc)
    '', //ListOfMultiValue
    '', //UseCompression    //this is  [8]
    'PropertyValue[8]==True',  //CompressionAlgorithm    //this is  [9]

    'PropertyValue[8]==True' + #5#6 + 'PropertyValue[9]==Lzma',  //LzmaEndOfStreamPropertyName,
    'PropertyValue[8]==True' + #5#6 + 'PropertyValue[9]==Lzma',  //LzmaAlgorithmPropertyName,
    'PropertyValue[8]==True' + #5#6 + 'PropertyValue[9]==Lzma',  //LzmaNumBenchMarkPassesPropertyName,
    'PropertyValue[8]==True' + #5#6 + 'PropertyValue[9]==Lzma',  //LzmaDictionarySizePropertyName,
    'PropertyValue[8]==True' + #5#6 + 'PropertyValue[9]==Lzma',  //LzmaMatchFinderPropertyName,
    'PropertyValue[8]==True' + #5#6 + 'PropertyValue[9]==Lzma',  //LzmaLiteralContextPropertyName,
    'PropertyValue[8]==True' + #5#6 + 'PropertyValue[9]==Lzma',  //LzmaLiteralPosBitsPropertyName,
    'PropertyValue[8]==True' + #5#6 + 'PropertyValue[9]==Lzma',  //LzmaPosBitsPropertyName,
    'PropertyValue[8]==True' + #5#6 + 'PropertyValue[9]==Lzma'   //LzmaFastBytesPropertyName
  );


implementation

end.

