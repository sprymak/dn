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
::  1.6.RC1
::  dn16rc1-Fix_rare_build_error_diff137byMV.patch
::
::  2.0.0
::  dn200-version_autonumbering.patch
::  dn200-builddn_batch_fix.patch
::  dn200-version_upgrade.patch
::
::  2.3.0
::  dn230-version_upgrade.patch
::
::  2.7.0
::  dn270-version_upgrade.patch
::
::  3.7.0
::  dn31029-Compile_by_VP.patch
::  dn370-version_upgrade.patch
::
::  4.9.0
::  dn50308-polish_resources.patch
::  dn40900-version_upgrade.patch
::
::  5.9.0
::
:://///////////////////////////////////////////////////////////////////////
@echo off
:: Set up the paths to your Borland tools or leave blank if they are in PATH
set binpath=c:\bp\bin\
set bpcpath=c:\bp\bin\
:: Set up optional executable packing utilities (recommended)
set real_packer=
set dpmi_packer=
set com_packer=
:: Set to Y to enable compression of the overlay file (decreases performance)
set PackOverlay=

:: No user-configurable parameters beyond this line
:://///////////////////////////////////////////////////////////////////////
set outpath=out
set _Q_opt=/Q
set SWPOPT=
set ASMOPT=exe\*.obj
set GGT=
set GGB=
set GGS=
set GGD=
set GGDB=
if not exist exe\nul mkdir exe >nul
if not exist %outpath%\nul mkdir %outpath% >nul

rem ------------------------------------------------------------------
rem BUILDDN [debug] [swapdisk] [dpmi] [help] [resource] [asm] [nobpc]
rem         [nolite] [ver] [verbose]
rem ------------------------------------------------------------------
rem ctty con
if .%1.==.. builddn asm help resource nolite

set prg_packer=%real_packer%

set dn_version=5.9.0
set diz_title=Real Mode Binary Package

:Loop
if .%0.==.nolite.   set GGT=GGT
if .%0.==.verbose.  set GGB=GGB
if .%0.==.swapdisk. set GGS=GGS
if .%0.==.dpmi.     set GGD=GGD
if .%0.==.debug.    set GGDB=GGDB
if .%1.==.asm.      goto asm
if .%1.==.ver.      goto cmpver
if .%1.==.resource. goto resource
if .%1.==.help. goto help
::::
if .%1.==.nolite. set lite=off
if .%1.==.nolite. shift
if .%GGT%.==.GGT. goto GGT
if .%0.==.nolite. goto Loop
:GGT
if .%1.==.verbose. set _Q_opt=
if .%1.==.verbose. shift
if .%GGB%.==.GGB. goto GGB
if .%0.==.verbose. goto Loop
:GGB
if .%1.==.swapdisk. set SWPOPT=%SWPOPT% /l
if .%1.==.swapdisk. shift
if .%GGS%.==.GGS. goto GGS
if .%0.==.swapdisk. goto Loop
:GGS
if .%1.==.dpmi. set SWPOPT=%SWPOPT% /cp
if .%1.==.dpmi. set ASMOPT=exe\*.obp -d_DPMI_ -d__DPMI__
if .%1.==.dpmi. set prg_packer=%dpmi_packer%
if .%1.==.dpmi. set diz_title=DPMI Binary Package
if .%1.==.dpmi. shift
if .%GGD%.==.GGD. goto GGD
if .%0.==.dpmi. goto Loop
:GGD
if .%GGDB%.==.GGDB. goto GGDB
if .%1.==.debug. shift
if .%0.==.debug. goto Loop
:GGDB

:Done
if .%1.==.nobpc. goto SkipBPC
%bpcpath%bpc %SWPOPT% dn /dDN /dDNPRG /GD %_Q_opt% /B %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto Quit

%bpcpath%bpc %SWPOPT% PackMap /GD %_Q_opt% %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto Quit
Exe\PackMap Exe\dn

if .%lite%.==.off. goto SkipLite
if NOT ."%prg_packer%".==."". %prg_packer% exe\dn.exe
::if errorlevel 1 goto Quit
:SkipLite

if .%PackOverlay%.==.. goto ovrcopy
%bpcpath%bpc %SWPOPT% packfile /GD %_Q_opt% %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto Quit
%outpath%\packfile exe\dn.ovr exe\dn.ovp
if exist exe\dn.ovp copy exe\dn.ovp %outpath%\dn.ovr >nul
goto copyanother

:ovrcopy
if exist exe\dn.ovr copy exe\dn.ovr %outpath% >nul

:copyanother
copy exe\dn.exe %outpath%\*.PRG >nul
copy exe\dn.mpp %outpath% >nul
if not exist doc\whatsnew.* goto CopyDocDir
copy doc\whatsnew.* %outpath% >nul
goto SkipBPC
:CopyDocDir
if not exist %outpath%\doc\nul mkdir %outpath%\doc >nul
if not exist %outpath%\doc\english\nul mkdir %outpath%\doc\english >nul
if not exist %outpath%\doc\russian\nul mkdir %outpath%\doc\russian >nul
copy doc\english\*.* %outpath%\doc\english >nul
copy doc\russian\*.* %outpath%\doc\russian >nul
copy doc\*.* %outpath%\doc >nul
echo Dos Navigator Open Source Project >%outpath%\file_id.diz
echo Version %dn_version% >>%outpath%\file_id.diz
echo %diz_title% >>%outpath%\file_id.diz
:SkipBPC

if not .%GGDB%.==.GGDB. goto Quit
copy *.* exe\*.* >nul
cd exe
bp dn
goto Quit

:resource
if not exist exe\rcp.exe %bpcpath%bpc %SWPOPT% RegAll /DRCP %_Q_opt%
if errorlevel 1 goto Quit
if not exist exe\rcp.exe %bpcpath%bpc %SWPOPT% rcp %_Q_opt%
if errorlevel 1 goto Quit
if not exist exe\rcp.exe goto Quit
shift
exe\rcp
if errorlevel 1 goto Quit

set Q=
goto Loop

:help
shift
%bpcpath%bpc %SWPOPT% tvhc %_Q_opt%
if errorlevel 1 goto Quit
if not exist exe\tvhc.exe goto Quit

exe\tvhc resource\english\dnhelp.htx exe\ENGLISH.HLP dnhelp.pas /4DN_OSP
if errorlevel 1 goto Quit
copy exe\english.hlp %outpath% >nul

exe\tvhc resource\russian\dnhelp.htx exe\RUSSIAN.HLP dnhelp.pas /4DN_OSP
if errorlevel 1 goto Quit
copy exe\russian.hlp %outpath% >nul

exe\tvhc resource\polish\dnhelp.htx exe\POLISH.HLP dnhelp.pas /4DN_OSP
if errorlevel 1 goto Quit
copy exe\polish.hlp %outpath% >nul

exe\tvhc resource\hungary\dnhelp.htx exe\HUNGARY.HLP dnhelp.pas /4DN_OSP
if errorlevel 1 goto Quit
copy exe\hungary.hlp %outpath% >nul

set Q=Y
goto Loop

:asm
shift
%binpath%tasm /t /x /m3 /zi *.asm,exe\*.obj
if .%GGD%.==.GGD. %binpath%tasm /t /x /m3 /zi dn.asm,exe\dn.obj -d_DPMI_
if errorlevel 1 goto Quit
%binpath%tasm -t -m3 -q -d__LARGE__ *.asl %ASMOPT%
if errorlevel 1 goto Quit
if not .%GGD%.==.GGD. goto SkipObp2Obj
if exist exe\cpuspeed.obp copy exe\cpuspeed.obp exe\*.obj >nul
if exist exe\p5speed.obp  copy exe\p5speed.obp  exe\*.obj >nul
if exist exe\cachetst.obp copy exe\cachetst.obp exe\*.obj >nul
:SkipObp2Obj
if not ."%ASMOPT%".==."exe\*.obj". %binpath%tasm /t /x /m3 /zi *.asm %ASMOPT%
if errorlevel 1 goto Quit
%binpath%tlink /t /x exe\dn.obj, exe\dn.com
if errorlevel 1 goto Quit
if exist exe\dn.obj del exe\dn.obj >nul
if ."%com_packer%".==."". goto copydncom
%com_packer% exe\dn.com
echo.
:copydncom
copy exe\dn.com %outpath% >nul
if .%GGD%.==.. goto realcode
%binpath%tasm -t -m3 -q -d__LARGE__ -d__DPMI__ dpmicode.adp exe\dpmicode.obp
%binpath%tasm -t -m3 -x -zi -d__DPMI__ -d_DPMI_ s95tfio.adp exe\s95tfio.obp
if errorlevel 1 goto Quit
goto Loop
:realcode
%binpath%tasm -t -m3 -q -d__LARGE__ realcode.arl exe\realcode.obj
if errorlevel 1 goto Quit
:loop
goto Loop

:cmpver
shift
%bpcpath%bpc %SWPOPT% version %_Q_opt%
if errorlevel 1 goto Quit
exe\version exe\version.inc
if errorlevel 1 goto Quit
goto Loop

:Quit
set _Q_opt=
set SWPOPT=
set ASMOPT=
set bpcpath=
set binpath=
set outpath=
set GGT=
set GGB=
set GGS=
set GGD=
set GGDB=
set prg_packer=
set real_packer=
set dpmi_packer=
set com_packer=
set diz_title=
set dn_version=
set PackOverlay=
