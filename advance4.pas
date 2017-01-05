{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
//  Based on Dos Navigator (C) 1991-99 RIT Research Labs
//
//  This programs is free for commercial and non-commercial use as long as
//  the following conditions are aheared to.
//
//  Copyright remains RIT Research Labs, and as such any Copyright notices
//  in the code are not to be removed. If this package is used in a
//  product, RIT Research Labs should be given attribution as the RIT Research
//  Labs of the parts of the library used. This can be in the form of a textual
//  message at program startup or in documentation (online or textual)
//  provided with the package.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  1. Redistributions of source code must retain the copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. All advertising materials mentioning features or use of this software
//     must display the following acknowledgement:
//     "Based on Dos Navigator by RIT Research Labs."
//
//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  The licence and distribution terms for any publically available
//  version or derivative of this code cannot be changed. i.e. this code
//  cannot simply be copied and put under another distribution licence
//  (including the GNU Public Licence).
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit advance4; {OS/2 support}

interface

type
  SessionType = (stOS2SYS, stOS2FullScreen, stOS2Windowed,
    stPMSession,
  stVDMFullScreen, stWinFullScreen, stWinWindow, stVDMWindow);

procedure RunOS2Command(Command: String; Bckg: boolean; Session:
    SessionType);

implementation

uses
  advance, Lfn, advance1, advance3, Drivers
  , Dos, DnExec
  ;
{ ------------------------------------ OS/2 API --------------------------- }

{-DataCompBoy-}
procedure RunOS2Command;
  var
    t: lText;
    i: integer;
    s, M, ex: String;
  begin
    if not OS2exec then
      exit;
    i := 1;
    repeat
      ClrIO;
      ex := SwpDir+'$DN'+ItoS(i)+'$.CMD';
      lAssignText(t, ex);
      FileMode := $40;
      lResetText(t);
      if IOResult <> 0 then
        break;
      Close(t.t);
      if InOutRes = 0 then
        Inc(i);
    until IOResult <> 0;
    ClrIO;
    lAssignText(t, ex);
    lRewriteText(t);
    lGetDir(0, s);
    Writeln(t.t, '@'+Copy(s, 1, 2));
    Writeln(t.t, '@cd "'+s+'"');
    s := Command;
    if PosChar(';', s) > 0 then
      begin
        Replace(';;', #0, s);
        while (s <> '') and (PosChar(';', s) <> 0) do
          begin
            i := PosChar(';', s);
            M := Copy(s, 1, i-1);
            Replace(#0, ';', M);
            Writeln(t.t, M);
            Delete(s, 1, i);
          end;
        Replace(#0, ';', s);
        Writeln(t.t, s);
      end
    else
      Writeln(t.t, Command);
    if not Bckg and (ShiftState and $20 = 0) then
      Writeln(t.t, '@pause');
    Write(t.t, '@del "'+ex+'" & exit'^Z);
    Close(t.t);
    if Session = stOS2FullScreen then
      M := 'START "'+Command+'" /FS '+ex
    else
      M := 'START "'+Command+'" /WIN '+ex;
    ExecString(@M, '');
  end { RunOS2Command };
{DataCompBoy}
end.
