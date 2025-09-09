{
    Copyright (C) 2025 VCC
    creation date: 23 May 2025
    initial release date: 24 May 2025

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


unit WorkerPoolCommonConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  CBrokerAddressKeyName = 'BrokerAddress';
  CBrokerPortKeyName = 'BrokerPort';
  CCredentialsFileKeyName = 'CredentialsFile';

  //Commands
  CGetConfigCmd = 'GetConfig'; //the PoolClient plugin requests broker address and port

  CMachineOnline = 'MachineOnline'; //a machine, with broker(s) and workers, is online. Also, a dist machine is online.
  CRemoveWorkerMachine = 'RemoveWorkerMachine';
  CRemoveDistMachine = 'RemoveDistMachine';
  CGetAppsStatus = 'GetAppsStatus';
  CSendPoolCredentials = 'SendPoolCredentials'; //resend on demand, in case of a UIClicker crash

  //Command parameters
  CIncludeCredentialsParam = 'IncludeCredentials'; //When set to 1, the Pool manager returs an additional content, which is the "Credentials File" for connecting to broker.
                                                   //This file is returned as a ini file with #4#5 line breaks.
  CPoolClientUserNameParam = 'PoolClientUserName';
  CPoolClientPassWordParam = 'PoolClientPassWord';
  CPoolCredentialsFileName = 'Mem:\PoolCredentials.txt';

  //broker/worker params
  CMachineOSParam = 'MachineOS';
  CWorkerMachineAddress = 'WorkerMachineAddress';   //Where the "service" UIClicker is running, together with broker and workers.
  CDistPluginMachineAddress = 'DistPluginMachineAddress';  //Where the "Dist" plugin is running.

  //Responses
  CErrPrefix = 'Err:';
  CUserNotFound = 'UserNotFound';
  CWrongPassword = 'WrongPassword';

  CMachineSet = 'MachineSet';
  CWorkerMachineNotFound = 'WorkerMachineNotFound';
  CDistMachineNotFound = 'DistMachineNotFound';
  CMachineRemoved = 'MachineRemoved';
  CTooManyWorkerMachines = 'TooManyWorkerMachines';
  CTooManyDistMachines = 'TooManyDistMachines';

  CNoAppRunning = 'NoAppRunning';
  CAllAppsRunning = 'AllAppsRunning';
  CSomeAppsRunning = 'SomeAppsRunning';  //not all

  //Other:
  CWinParam = 'Win';
  CLinParam = 'Lin';
  CBrokerProcessName = 'mosquitto'; //the only supported broker :)
  CWorkerProcessName = 'FindSubControlWorker';

implementation

end.

