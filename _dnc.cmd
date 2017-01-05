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
@Echo off
if not exist exeo md exeo

Echo        Compiling VERSION.EXE
if exist exeo\version.exe goto dover
vpc version /m /dOS2 /q
if errorlevel 1 goto ex
if exist exeo\tvhc.exe del exeo\thvc.exe
:dover
exeo\version.exe exeo\version.inc

if exist exeo\tvhc.exe goto comphelp
Echo        Compiling TVHC.EXE
vpc tvhc /m /dOS2 /q
if errorlevel 1 goto ex
del exeo\*.vpi
del exeo\*.lib
:comphelp
if exist exeo\*.hlp goto endcomp
exeo\tvhc resource\english\dnhelp.htx exeo\english.hlp exeo\dnhelp.pas /4DN_OSP
exeo\tvhc resource\russian\dnhelp.htx exeo\russian.hlp exeo\dnhelp.pas /4DN_OSP
rem exeo\tvhc resource\hungary\dnhelp.htx exeo\hungary.hlp exeo\dnhelp.pas /4DN_OSP
:endcomp

if exist exeo\rcp.exe goto dores
Echo        Compiling RCP.EXE
vpc rcp /m /dRCP /dOS2 /q
if errorlevel 1 goto ex
del exeo\*.vpi
del exeo\*.lib
:dores
if exist exeo\*.dlg goto endres
exeo\rcp
:endres


Echo        Compiling DN.EXE
vpc dn /m /dDN /dDNPRG /dOS2 /q
vpc dnpmapil /m /dOS2 /q
call resmgr -a exeo\dn.exe exeo\dn.res
lxlite exeo\dn.exe
if not %1.==debug. goto ex
copy *.* exeo\*.*
cd exe
vp

:ex
del exeo\*.vpi
del exeo\*.lib
del exeo\*.map
del exeo\*.bak
del exeo\*.lnk
del exeo\*.obj
