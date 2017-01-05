::////////////////////////////////////////////////////////////////////////
::
::  Dos Navigator Open Source 1.51.08
::  Based on Dos Navigator (C) 1991-99 RIT Research Labs
::
::  This programs is free for commercial and non-commercial use as long as
::  the following conditions are aheared to.
::
::  Copyright remains RIT Research Labs, and as such any Copyright notices
::  in the code are not to be removed. If this package is used in a
::  product, RIT Research Labs should be given attribution as the RIT Research
::  Labs of the parts of the library used. This can be in the form of a textual
::  message at program startup or in documentation (online or textual)
::  provided with the package.
::
::  Redistribution and use in source and binary forms, with or without
::  modification, are permitted provided that the following conditions are
::  met:
::
::  1. Redistributions of source code must retain the copyright
::     notice, this list of conditions and the following disclaimer.
::  2. Redistributions in binary form must reproduce the above copyright
::     notice, this list of conditions and the following disclaimer in the
::     documentation and/or other materials provided with the distribution.
::  3. All advertising materials mentioning features or use of this software
::     must display the following acknowledgement:
::     "Based on Dos Navigator by RIT Research Labs."
::
::  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
::  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
::  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
::  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
::  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
::  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
::  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
::  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
::  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
::  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
::  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
::
::  The licence and distribution terms for any publically available
::  version or derivative of this code cannot be changed. i.e. this code
::  cannot simply be copied and put under another distribution licence
::  (including the GNU Public Licence).
::
:://///////////////////////////////////////////////////////////////////////
@set pause=rem
@Echo off
@rem set pause=pause
if [%1]==[P] goto plugin
if [%1]==[p] goto plugin
if not [%1]==[] goto help
goto end_plugin
:plugin
set plugin=/dPLUGIN
set rplugin=PLUGIN
:end_plugin

set H=O
if [%Host%]==[OS2] goto end_host
set H=W
if [%Host%]==[W32] goto end_host
goto Help
:end_host

if [%target%]==[] set target=%Host%
set T=O
if [%Target%]==[OS2] goto end_Target
set T=W
if [%Target%]==[W32] goto end_Target
goto Help
:end_Target

echo Host="%Host%" Target="%Target%" %rplugin%
%pause%

Echo        Compiling System.pas
del exe.%Target%\system.lib
vpc /b /q /c%T% -dDEBUGMEM lib.%Target%\vpsyslow
vpc /b /q /c%T% -dDEBUGMEM -Usys sys\system
if [%Target%]==[OS2] lib "exe.%Target%\system.lib" +sys\io +sys\thunk "+exe.%Target%\vpsyslow.lib" "+exe.%Target%\strings.lib"
if [%Target%]==[W32] lib "exe.%Target%\system.lib" +sys\iodos "+exe.%Target%\vpsyslow.lib"
if errorlevel 1 pause Error
%pause%

if not exist EXE.%Target% md EXE.%Target%

Echo        Compiling VERSION.EXE for %Host%
if exist EXE.%Host%\version.exe goto dover
vpc version /b /q /c%T%
if errorlevel 1 pause Error
if exist EXE.%Host%\tvhc.exe del EXE.%Host%\thvc.exe
:dover
EXE.%Host%\version.exe EXE.%Target%\version.inc %Target%  %rplugin%
%pause%

if exist EXE.%Host%\tvhc.exe goto comphelp
Echo        Compiling TVHC.EXE for %Host%
vpc tvhc /m  /q /c%T%
if errorlevel 1 pause Error

:comphelp
Echo   Compiling help files for %Target%
EXE.%Host%\tvhc resource\english\dnhelp.htx EXE.%Target%\english.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
EXE.%Host%\tvhc resource\russian\dnhelp.htx EXE.%Target%\russian.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
rem EXE.%Target%\tvhc resource\hungary\dnhelp.htx EXE.%Target%\hungary.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
%pause%

if exist EXE.%Host%\rcp.exe goto dores
Echo        Compiling RCP.EXE for %Host%
vpc rcp /b /dRCP /q %plugin% /c%H%
if errorlevel 1 pause Error
:dores

Echo        Compiling resource files for %Target% %rplugin%
set RCP_Target=%Target%
if [%RCP_Target%]==[W32] set RCP_Target=Win32
EXE.%Host%\rcp %T% %RCP_Target% %rplugin%
:endres
%pause%

if [%plugin%]==[] goto end_dn2cat
Echo        Compiling DNcat.EXE for %Target%
vpc dn2CAT /b /dDN /dDNPRG /dPLUGIN /q /c%T%
if not errorlevel 1 goto end_dn2cat
@echo Это что-то переполняется в VP, со второго раза получится.
vpc dn2CAT /m /dDN /dDNPRG /dPLUGIN /q /c%T%
if errorlevel 1 pause Error
:end_dn2cat
%pause%

if not %Target%==OS2 goto end_dnpmapil
Echo        Compiling dnpmapil.dll for %Target%
vpc dnpmapil /q /b %plugin% /c%T%
if errorlevel 1 pause Error
%pause%
:end_dnpmapil

Echo        Compiling DN.EXE for %Target%  %rplugin%
del exe.%Target%\advance.*
del exe.%Target%\drivers.*
vpc dn /m /dDN /dDNPRG /q %plugin% /c%T%
if not errorlevel 1 goto end_dn
@echo Это что-то переполняется в VP, со второго раза получится.
vpc dn /m /dDN /dDNPRG /q %plugin% /c%T%
if errorlevel 1 pause Error
:end_dn
if [%Target%]==[OS2] if [%Host%]==[OS2] rc -p -x2 exe.os2\dn.res exe.os2\dn.exe

%pause% Финальное удаление vpi и.т.п.
del EXE.%Target%\*.vpi
del EXE.%Target%\*.lib
del EXE.%Target%\*.map
del EXE.%Target%\*.bak
del EXE.%Target%\*.lnk
del EXE.%Target%\*.obj
exit

:Help
@echo Переменная Host должна указывать текущую платформу (OS2, W32);
@echo Переменная Target должна указывать целевую платформу (OS2, W32),
@echo если она отлична от текущей.
@echo Параметр должен отсутствовать или иметь значение P
@echo (для компиляции плагинной версии).
