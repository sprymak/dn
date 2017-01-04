::////////////////////////////////////////////////////////////////////////
::
::  Dos Navigator Open Source 1.51.05/DOS
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
set SWPOPT=
set ASMOPT=
set bppath=c:\bp\
set outpath=out
set GGT=
set GGB=
set GGS=
set GGD=
set packer=upx -9
set ovrl=
md exe >nul
md out >nul
cls

echo {} >overlay.def
if .%ovrl%.==.yes. echo {$define packoverlay} >overlay.def

rem ------------------------------------------------------------------
rem BUILDDN [swapdisk] [dpmi] [help] [resource] [asm] [nobpc] [nolite] [ver] [verbose]
rem ------------------------------------------------------------------
rem ctty con
if .%1.==.. builddn help resource nolite

:Loop
if .%0.==.nolite.   set GGT=GGT
if .%0.==.verbose.  set GGB=GGB
if .%0.==.swapdisk. set GGS=GGS
if .%0.==.dpmi.     set GGD=GGD
if .%1.==.asm. goto asm
if .%1.==.ver. goto cmpver
if .%1.==.resource. goto resource
if .%1.==.help. goto help
::::
if .%1.==.nolite. set lite=off
if .%1.==.nolite. shift
if .%GGT.==.GGT. goto GGT
if .%0.==.nolite. goto Loop
:GGT
if .%1.==.verbose. set _Q_opt=
if .%1.==.verbose. shift
if .%GGB%==.GGB. goto GGB
if .%0.==.verbose. goto Loop
:GGB
if .%1.==.swapdisk. set SWPOPT=%SWPOPT% /l
if .%1.==.swapdisk. shift
if .%GGS%==.GGS. goto GGS
if .%0.==.swapdisk. goto Loop
:GGS
if .%1.==.dpmi. set SWPOPT=%SWPOPT% /cp
if .%1.==.dpmi. set ASMOPT=*.obp -d_DPMI_
if .%1.==.dpmi. shift
if .%GGD%==.GGD. goto GGD
if .%0.==.dpmi. goto Loop
:GGD

:Done
if .%1.==.nobpc. goto SkipBPC
%bppath%bin\bpc %SWPOPT% dn /dDN /dDNPRG /GD %_Q_opt% /B %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto Quit

if .%ovrl%.==.yes. %bppath%bin\bpc %SWPOPT% PFile /dDN /dDNPRG /GD %_Q_opt% /B %1 %2 %3 %4 %5 %6 %7 %8 %9
if .%ovrl%.==.yes. if errorlevel 1 goto Quit

cd Map
%bppath%bin\bpc %SWPOPT% PackMap /GD %_Q_opt% /B %1 %2 %3 %4 %5 %6 %7 %8 %9
cd ..
if errorlevel 1 goto Quit
Map\PackMap Exe\dn

if .%lite%.==.off. goto SkipLite
%packer% exe\dn.exe
::if errorlevel 1 goto Quit
:SkipLite
if .%ovrl%.==.. goto ovrcopy
exe\PFile exe\dn.ovr %outpath%\dn.ovr
goto copyanother
:ovrcopy
copy exe\dn.ovr %outpath%
:copyanother
copy exe\dn.exe %outpath%\*.PRG
copy exe\dn.mpp %outpath%
copy history.txt %outpath%
:SkipBPC
goto Quit

:resource
if not .%Q%.==.. %bppath%bin\bpc %SWPOPT% rcp %_Q_opt%
if not .%Q%.==.. goto :r
if not exist exe\rcp.exe %bppath%bin\bpc %SWPOPT% rcp %_Q_opt%
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
%bppath%bin\bpc %SWPOPT% tvhc %_Q_opt%
if errorlevel 1 goto Quit
if not exist exe\tvhc.exe goto Quit

exe\tvhc resource\english\dnhelp.htx exe\english.hlp dnhelp.pas /4DN_OSP
if errorlevel 1 goto Quit
copy exe\english.hlp %outpath%

exe\tvhc resource\russian\dnhelp.htx exe\russian.hlp dnhelp.pas /4DN_OSP
if errorlevel 1 goto Quit
copy exe\russian.hlp %outpath%

exe\tvhc resource\hungary\dnhelp.htx exe\hungary.hlp dnhelp.pas /4DN_OSP
if errorlevel 1 goto Quit
copy exe\hungary.hlp %outpath%

set Q=Y
goto Loop

:asm
shift
%bppath%bin\tasm /t /x /m /zi *.asm
if errorlevel 1 goto Quit
if not .%ASMOPT%.==.. %bppath%bin\tasm /t /x /m /zi *.asm %ASMOPT%
if errorlevel 1 goto Quit
%bppath%bin\tlink /t /x dn.obj, exe\dn.com
if errorlevel 1 goto Quit
del dn.obj
copy exe\dn.com %outpath%
goto Loop

:cmpver
shift
%bppath%bin\bpc %SWPOPT% version %_Q_opt%
exe\version
goto Loop

:Quit
set tvar=
set multi=
set _Q_opt=
set SWPOPT=
set ASMOPT=
set bppath=
set outpath=
set GGT=
set GGB=
set GGS=
set GGD=
set packer=
set ovrl=