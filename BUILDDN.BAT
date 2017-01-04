::////////////////////////////////////////////////////////////////////////
::
::  Dos Navigator Open Source 1.51.04/DOS
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

@echo off
set tvar=
set multi=Y
set _Q_opt=/Q
set bppath=c:\pascal\
set outpath=out
md exe >nul
md out >nul
cls

rem ------------------------------------------------------------------
rem BUILDDN [help] [resource] [asm] [nobpc] [nolite] [ver] [verbose]
rem ------------------------------------------------------------------
rem ctty con
if .%1.==.. builddn help resource nolite

:Loop
if .%1.==.asm. goto asm
if .%1.==.ver. goto cmpver
if .%1.==.resource. goto resource
if .%1.==.help. goto help
if .%1.==.nolite. set lite=off
if .%1.==.nolite. shift
if .%0.==.nolite. goto Loop
if .%1.==.verbose. set _Q_opt=
if .%1.==.verbose. shift
if .%0.==.verbose. goto Loop

:Done
if .%1.==.nobpc. goto SkipBPC
%bppath%bpc dn /dDN /dDNPRG /GP %_Q_opt% /B %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto Quit

if .%lite%.==.off. goto SkipLite
pklite exe\dn.exe
if errorlevel 1 goto Quit
:SkipLite
copy exe\dn.ovr %outpath%
copy exe\dn.exe %outpath%\*.PRG

:SkipBPC
goto Quit

:resource
if not .%Q%.==.. %bppath%bpc rcp %_Q_opt%
if not .%Q%.==.. goto :r
if not exist exe\rcp.exe %bppath%bpc rcp %_Q_opt%
:r
if errorlevel 1 goto Quit
if not exist exe\rcp.exe goto Quit
shift
exe\rcp
if errorlevel 1 goto Quit

copy exe\*.lng %outpath%
copy exe\*.dlg %outpath%
set Q=
goto Loop

:help
shift
%bppath%bpc tvhc %_Q_opt%
if errorlevel 1 goto Quit
if not exist exe\tvhc.exe goto Quit

exe\tvhc resource\english\dnhelp.htx exe\english.hlp dnhelp.pas
if errorlevel 1 goto Quit
copy exe\english.hlp %outpath%

exe\tvhc resource\russian\dnhelp.htx exe\russian.hlp dnhelp.pas
if errorlevel 1 goto Quit
copy exe\russian.hlp %outpath%

set Q=Y
goto Loop

:asm
shift
%bppath%bin\tasm /t /x /m /q *.asm
if errorlevel 1 goto Quit
%bppath%bin\tlink /t /x dn.obj, exe\dn.com
if errorlevel 1 goto Quit
del dn.obj
copy exe\dn.com %outpath%
goto Loop

:cmpver
shift
%bppath%bpc version %_Q_opt%
exe\version
goto Loop

:Quit
