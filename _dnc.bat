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
set T=D
if [%Target%]==[D32] goto end_Target
goto Help
:end_Target

echo Host="%Host%" Target="%Target%" %rplugin%
%pause%

if not exist EXE.%Target% md EXE.%Target%

Echo        Compiling VERSION.EXE for %Host%
if exist EXE.%Host%\version.exe goto dover
vpc version /b /q /c%H%
@if errorlevel 1 pause Error
if exist EXE.%Host%\tvhc.exe del EXE.%Host%\thvc.exe
:dover
EXE.%Host%\version.exe EXE.%Target%\version.inc %Target%  %rplugin%
%pause%

if exist EXE.%Host%\tvhc.exe goto comphelp
Echo        Compiling TVHC.EXE for %Host%
vpc tvhc /m  /q /c%H%
@if errorlevel 1 pause Error

:comphelp
Echo   Compiling help files for %Target%
EXE.%Host%\tvhc resource\english\dnhelp.htx EXE.%Target%\english.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
EXE.%Host%\tvhc resource\russian\dnhelp.htx EXE.%Target%\russian.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
EXE.%Host%\tvhc resource\ukrain\dnhelp.htx EXE.%Target%\ukrain.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
rem EXE.%Target%\tvhc resource\hungary\dnhelp.htx EXE.%Target%\hungary.hlp EXE.%Target%\dnhelp.pas /4DN_OSP
%pause%

if exist EXE.%Host%\rcp.exe goto dores
Echo        Compiling RCP.EXE for %Host%
vpc rcp /b /dRCP /q %plugin% /c%H%
@if errorlevel 1 pause Error
:dores

Echo        Compiling resource files for %Target% %rplugin%
set RCP_Target=%Target%
if [%RCP_Target%]==[W32] set RCP_Target=Win32
EXE.%Host%\rcp %T% %RCP_Target% %rplugin%
:endres
%pause%

rem if [%plugin%]==[] goto end_dn2cat
rem Echo        Compiling DNcat.EXE for %Target%
rem vpc dn2CAT /b /dDN /dDNPRG /dPLUGIN /q /c%T%
rem if not errorlevel 1 goto end_dn2cat
rem @echo Это что-то переполняется в VP, со второго раза получится.
rem vpc dn2CAT /m /dDN /dDNPRG /dPLUGIN /q /c%T%
rem @if errorlevel 1 pause Error
rem :end_dn2cat
rem %pause%

if not %Target%==OS2 goto end_dnpmapil
Echo        Compiling dnpmapil.dll for %Target%
vpc dnpmapil /q /b %plugin% /c%T%
@if errorlevel 1 pause Error
%pause%
:end_dnpmapil

Echo        Compiling DN.EXE for %Target%  %rplugin%
del exe.%Target%\advance.*
del exe.%Target%\drivers.*

if not [%Target%]==[D32] goto OS2W32
vpc dn /dDN /dDNPRG -b -q -CW:d32:DPMI32
@if not errorlevel 1 goto pe2LE
@echo Это что-то переполняется в VP, со второго раза получится.
vpc dn /dDN /dDNPRG -m -q -CW:d32:DPMI32
@if errorlevel 1 pause Error
:pe2LE
PE2LE.EXE EXE.D32\dn.EXE /* /S:WDOSXLE.EXE /Q'
@if errorlevel 1 pause Error
del EXE.D32\dn.exe
goto end_dn

:OS2W32
if [%Target%]==[W32] vpc LIB.W32\vpkbdw32.pas /m /dDN /dDNPRG /q %plugin% /c%T%
vpc dn /b /dDN /dDNPRG /q %plugin% /c%T%
if not errorlevel 1 goto end_dn
@echo Это что-то переполняется в VP, со второго раза получится.
vpc dn /m /dDN /dDNPRG /q %plugin% /c%T%
@if errorlevel 1 pause Error
:end_dn
if [%Target%]==[OS2] if [%Host%]==[OS2] rc -p -x2 exe.os2\dn.res exe.os2\dn.exe

if [%plugin%]==[] goto end_plgman
Echo        Compiling plugman.dll for %Target%
vpc plugman /m /q /c%T%
if not errorlevel 1 goto end_plgman
@echo Это что-то переполняется в VP, со второго раза получится.
vpc plugman /m /q /c%T%
@if errorlevel 1 pause Error
:end_plgman
%pause%

rem lxlite exe.os2\dn.exe

%pause% Финальное удаление vpi и.т.п.
del EXE.%Target%\*.vpi
del EXE.%Target%\*.lib
del EXE.%Target%\*.map
del EXE.%Target%\*.bak
del EXE.%Target%\*.lnk
del EXE.%Target%\*.obj
goto ret

:Help
@echo Переменная Host должна указывать текущую платформу (OS2, W32);
@echo Переменная Target должна указывать целевую платформу (OS2, W32),
@echo если она отлична от текущей.
@echo Параметр должен отсутствовать или иметь значение P
@echo (для компиляции плагинной версии).

:ret
