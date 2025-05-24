This is a plugin, with the typical UIClicker Action plugin API, which connects to WorkerPoolManager, to get connection info for UIClickerDistFindSubControl plugin.
It outputs a connection address and port as variables, and optionally, a Credentials file (which can be In-Mem (Mem:\) or saved on disk).
A UIClickerDistFindSubControl plugin can be configured to used these variables and the Credentials file.
If required (and implemented) a PoolID can be saved in the Credentials file and used by the UIClickerDistFindSubControl plugin.
Thus, the whole "PoolID" infrastructure becomes optional and if used, does not require any changes in how a UIClickerDistFindSubControl plugin is implemented.
The PoolClient plugin can be called once per UIClicker session and its output info can be used in multiple calls to UIClickerDistFindSubControl plugin.