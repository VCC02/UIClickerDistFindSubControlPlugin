UIClickerDistFindSubControlPlugin

A plugin for UIClicker, which executes a FindSubControl action on multiple remote machines, via MQTT.

#DistInitialEnc and DistEnc usage example:
.
#Sending (unencrypted) the decryption plugin: DistInitialDec.dll (the key and IV, specified here, end up in the txt files, near the plugin, then used on subsequent plugins)
.\DistInitialEnc.exe --ClickerClient C:\UIClicker\ClickerClient\ClickerClient.dll --PluginToBeSent C:\UIClickerDistFindSubControlPlugin\DistInitialDec\lib\i386-win32\DistInitialDec.dll --PluginToBeSentDestName DistInitialDec.dll --UIClickerAddress 127.0.0.1 --UIClickerPort 5444

#Sending (encrypted) DistDec.dll  (Always sent as a DecDecHash plugin by this tool, decrypted by DistInitialDec.dll)
.\DistInitialEnc.exe --ClickerClient C:\UIClicker\ClickerClient\ClickerClient.dll --PluginToBeSent C:\UIClickerDistFindSubControlPlugin\DistDec\lib\i386-win32\DistDec.dll --PluginToBeSentDestName DistDec.dll --UIClickerAddress 127.0.0.1 --UIClickerPort 5444

#Sending (encrypted) UIClickerDecompressionExample.dll (must be manually specified that it is a DecDecHash plugin)
.\DistEnc.exe --ClickerClient C:\UIClicker\ClickerClient\ClickerClient.dll --PluginToBeSent C:\UIClickerDecompressionExamplePlugin\lib\i386-win32\UIClickerDecompressionExample.dll --PluginToBeSentDestName UIClickerDecompressionExample.dll --UIClickerAddress 127.0.0.1 --UIClickerPort 5444 --DecryptionPluginName "DistDec.dllarc|Mem:\DistDec.dll" --IsDecDecHash True


Dependencies:

https://github.com/VCC02/MiscUtils

https://github.com/VCC02/UIClicker

https://github.com/VCC02/MQTTClient

![alt text](https://github.com/VCC02/UIClickerDistFindSubControlPlugin/blob/master/Doc/DistSch.png?raw=true)
