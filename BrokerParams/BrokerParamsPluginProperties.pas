{
    Copyright (C) 2025 VCC
    creation date: 06 Jun 2025
    initial release date: 06 Jun 2025

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


unit BrokerParamsPluginProperties;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ClickerUtils;


const
  CPropertiesCount = 5;                  //ToDo: Update file. So far, no properties are needed.

  CPoolManagerAddressPropertyIndex = 0;
  CPoolManagerPortPropertyIndex = 1;
  COutputCredentialsFileNamePropertyIndex = 2;
  CBrokerAddressOutputVarNamePropertyIndex = 3;
  CBrokerPortOutputVarNamePropertyIndex = 4;

  CPoolManagerAddressPropertyName = 'PoolManagerAddress';
  CPoolManagerPortPropertyName = 'PoolManagerPort';
  COutputCredentialsFileNamePropertyName = 'OutputCredentialsFileName';
  CBrokerAddressOutputVarNamePropertyName = 'BrokerAddressOutputVarName';
  CBrokerPortOutputVarNamePropertyName = 'BrokerPortOutputVarName';


  CRequiredPropertyNames: array[0..CPropertiesCount - 1] of string = (  //these are the expected property names, configured in plugin properties
    CPoolManagerAddressPropertyName,
    CPoolManagerPortPropertyName,
    COutputCredentialsFileNamePropertyName,
    CBrokerAddressOutputVarNamePropertyName,
    CBrokerPortOutputVarNamePropertyName
  );

  //property details: (e.g. enum options, hints, icons, menus, min..max spin intervals etc)

  //See TOIEditorType datatype from ObjectInspectorFrame.pas, for valid values
  CRequiredPropertyTypes: array[0..CPropertiesCount - 1] of string = (
    'TextWithArrow', //PoolManagerAddress
    'TextWithArrow', //PoolManagerPort
    'TextWithArrow', //OutputCredentialsFileName
    'TextWithArrow', //BrokerAddressOutputVarName
    'TextWithArrow'  //BrokerPortOutputVarName
  );

  CRequiredPropertyDataTypes: array[0..CPropertiesCount - 1] of string = (
    CDTString, //PoolManagerAddress
    CDTString, //PoolManagerPort
    CDTString, //OutputCredentialsFileName
    CDTString, //BrokerAddressOutputVarName
    CDTString  //BrokerPortOutputVarName
  );

  CPluginEnumCounts: array[0..CPropertiesCount - 1] of Integer = (
    0, //PoolManagerAddress
    0, //PoolManagerPort
    0, //OutputCredentialsFileName
    0, //BrokerAddressOutputVarName
    0  //BrokerPortOutputVarName
  );

  CPluginEnumStrings: array[0..CPropertiesCount - 1] of string = (
    '', //PoolManagerAddress
    '', //PoolManagerPort
    '', //OutputCredentialsFileName
    '', //BrokerAddressOutputVarName
    ''  //BrokerPortOutputVarName
  );

  CPluginHints: array[0..CPropertiesCount - 1] of string = (
    'Address of the machine, where WorkerPoolManager is running.', //PoolManagerAddress
    'WorkerPoolManager port number. This is usually 11883.', //PoolManagerPort
    'Name of the filename, which will be used as "Credentials File" by a UIClickerDistFindSubControl plugin.' + #4#5 + 'If the path starts with "Mem:\", the file is save in the In-Mem file system.' + #4#5 + 'Otherwise, this has to be a valid path on disk.' + #4#5 + 'If this value is not set (i.e. empty string), the credentials file is not requested from server and no file is saved by the plugin.', //OutputCredentialsFileName
    'Name of a variable, which will be updated by this plugin, with the address of the MQTT broker, which can be used by a UIClickerDistFindSubControl plugin.', //BrokerAddressOutputVarName
    'Name of a variable, which will be updated by this plugin, with the port number of the MQTT broker, which can be used by a UIClickerDistFindSubControl plugin.'  //BrokerPortOutputVarName
  );

  CPropertyEnabled: array[0..CPropertiesCount - 1] of string = (  // The 'PropertyValue[<index>]' replacement uses indexes from the following array only. It doesn't count fixed properties.
    '', //PoolManagerAddress                        // If empty string, the property is unconditionally enabled. For available operators, see CComp constans in ClickerUtils.pas.
    '', //PoolManagerPort
    '', //OutputCredentialsFileName
    '', //BrokerAddressOutputVarName
    ''  //BrokerPortOutputVarName
  );

  CPluginDefaultValues: array[0..CPropertiesCount - 1] of string = (
    '127.0.0.1', //PoolManagerAddress
    '11883', //PoolManagerPort
    'Mem:\CredentialsFileName.txt', //OutputCredentialsFileName
    '$MQTTBrokerAdress$', //BrokerAddressOutputVarName
    '$MQTTBrokerPort$'  //BrokerPortOutputVarName
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
    Result := Result + CRequiredPropertyNames[i] + '=' + CRequiredPropertyTypes[i] + #8#7 +
                       CPluginPropertyAttr_DataType + '=' + CRequiredPropertyDataTypes[i] + #8#7 +
                       CPluginPropertyAttr_EnumCounts + '=' + IntToStr(CPluginEnumCounts[i]) + #8#7 +
                       CPluginPropertyAttr_EnumStrings + '=' + CPluginEnumStrings[i] + #8#7 +
                       CPluginPropertyAttr_Hint + '=' + CPluginHints[i] + #8#7 +
                       CPluginPropertyAttr_Enabled + '=' + CPropertyEnabled[i] + #8#7 +
                       CPluginPropertyAttr_DefaultValue + '=' + CPluginDefaultValues[i] + #8#7 +
                       #13#10;
end;

end.

