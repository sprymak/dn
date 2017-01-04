::////////////////////////////////////////////////////////////////////////
::
::  Dos Navigator Open Source
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
::
::  Version history:
::
::  4.9.0
::
:://///////////////////////////////////////////////////////////////////////
@Echo off

set vppath=c:\vp21
if %Host% == .. Set Host=w32

if [%target%]==[] set target=%Host%
set T=O
if [%Target%]==[OS2] goto end_Target
set T=W
if [%Target%]==[W32] goto end_Target
set T=D
if [%Target%]==[D32] goto end_Target
goto Help
:end_Target


if not exist exe.%Host%\nul md exe.%Host%
if not exist out.%Host%\nul md out.%Host%

Echo -------- Compiling VERSION.EXE
if exist exe.%Host%\version.exe goto dover
%vppath%\bin.%Host%\vpc version /b /q
if errorlevel 1 goto ex
if exist exe.%Host%\tvhc.exe del exe.%Host%\thvc.exe
:dover
exe.%Host%\version.exe exe.%Host%\version.inc

if exist exe.%Host%\tvhc.exe goto comphelp
Echo -------- Compiling TVHC.EXE
%vppath%\bin.%Host%\vpc tvhc /b /q
if errorlevel 1 goto ex
:comphelp
if exist out.%Host%\*.hlp goto endcomp
Echo -------- Compiling help files
exe.%Host%\tvhc resource\english\dnhelp.htx out.%Host%\english.hlp exe.%Host%\dnhelp.pas /4DN_OSP
exe.%Host%\tvhc resource\russian\dnhelp.htx out.%Host%\russian.hlp exe.%Host%\dnhelp.pas /4DN_OSP
exe.%Host%\tvhc resource\hungary\dnhelp.htx out.%Host%\hungary.hlp exe.%Host%\dnhelp.pas /4DN_OSP
:endcomp

if exist exe.%Host%\rcp.exe goto dores
Echo -------- Compiling RCP.EXE
%vppath%\bin.%Host%\vpc rcp /b /dRCP /q
if errorlevel 1 goto ex
:dores
if exist out.%Host%\*.dlg goto endres
exe.%Host%\rcp
:endres

Echo -------- Compiling DN.EXE
%vppath%\bin.%Host%\vpc dn /b /dDN;DNPRG /q /c%T% /eOut.%Host%
if errorlevel 1 goto ex

Goto Ex1
:ex
Echo Error was founded!
:Ex1
Exit

:Help
@echo Переменная Host должна указывать текущую платформу (OS2, W32),
@echo Переменная Target должна указывать целевую платформу (OS2, W32).
