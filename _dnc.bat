::////////////////////////////////////////////////////////////////////////
::
::  Dos Navigator Open Source 1.51.10
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
if not exist exe md exe

Echo        Compiling VERSION.EXE
if exist exe\version.exe goto dover
vpc version /m /q
if errorlevel 1 goto ex
if exist exe\tvhc.exe del exe\thvc.exe
:dover
exe\version.exe exe\version.inc

if exist exe\tvhc.exe goto comphelp
Echo        Compiling TVHC.EXE
vpc tvhc /m /q
if errorlevel 1 goto ex
del exe\*.vpi
del exe\*.lib
:comphelp
if exist exe\*.hlp goto endcomp
exe\tvhc resource\english\dnhelp.htx exe\english.hlp exe\dnhelp.pas /4DN_OSP
exe\tvhc resource\russian\dnhelp.htx exe\russian.hlp exe\dnhelp.pas /4DN_OSP
exe\tvhc resource\hungary\dnhelp.htx exe\hungary.hlp exe\dnhelp.pas /4DN_OSP
:endcomp

if exist exe\rcp.exe goto dores
Echo        Compiling RCP.EXE
vpc rcp /m /dRCP /q
if errorlevel 1 goto ex
del exe\*.vpi
del exe\*.lib
:dores
if exist exe\*.dlg goto endres
exe\rcp
:endres


Echo        Compiling DN.EXE
vpc dn /m /dDN;DNPRG /q
if not %1.==debug. goto ex
copy *.* exe\*.*
cd exe
vp

:ex
