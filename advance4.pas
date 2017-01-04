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

unit Advance4; {OS/2 support}
interface

type SessionType = (stOS2SYS, stOS2FullScreen, stOS2Windowed, stPMSession,
                    stVDMFullScreen, stWinFullScreen, stWinWindow, stVDMWindow);

procedure StartOS2Session(DataLen: Word; Session: Sessiontype; Background: Boolean;
                          Title, Name, Args, IcoName: string);
procedure RunOS2Command(Command: string; Bckg: Boolean; Session: SessionType);


implementation
uses advance, lfn, advance1, advance3, drivers
     {$IFDEF VIRTUALPASCAL}, dos{$ENDIF}
     ;
{ ------------------------------------ OS/2 API --------------------------- }

procedure StartOS2Session(DataLen: Word; Session: Sessiontype; Background: Boolean;
                          Title, Name, Args, IcoName: string);
 var R: Record
         Size, Relation, FBground, Trace: Word;
         Title: Pointer;
         Name: Pointer;
         Args: Pointer;
         TERMQ: LongInt;
         Env: Pointer;
         Inheritance: Word;
         Session: Word;
         IcoN: Pointer;
    end;

    P: Pointer;
begin
   if not OS2exec then Exit;
   R.Size := DataLen;
   R.Relation := 1;
   R.FBground := Byte(Background);
   R.Trace := 0;
   Title := Title + #0;
   Name := Name + #0;
   Args := Args + #0;
   IcoName := IcoName + #0;
   R.Title := @Title[1];
   R.Name := @Name[1];
   R.Args := @Args[1];
   R.Inheritance := 0;
   R.Session := Word(Session);
   R.IcoN := @IcoName[1];
   R.TermQ := 0;
   LongInt(R.Env) := 0;
   P := @R;

   asm
     push ds
     mov ax, $6400
     mov cx, $636C
     mov bx, $0025
     lds si, P
     int 21h
     pop ds
   end;

end;

function GetBootDrive: Byte; assembler;
{ requires DOS 4.0+ or OS/2 Warp 3+ }
{ return: 1=A:, 2=B:, ...           }
asm
    mov ax, 3305h
    int 21h
    mov al, dl
end;

        {-DataCompBoy-}
procedure RunOS2Command;
 var T: lText;
     I: Integer;
     S, M, EX, OS2comspec: string;
begin
  if not OS2exec then Exit;
  I := 1;
  repeat
    ClrIO;
    EX := SwpDir+'$DN'+Itos(I)+'$.CMD';
    lAssignText(T, EX);
    Filemode := $40;
    lResetText(T);
    if IOResult <> 0 then Break;
    Close(T.T);
    if InOutRes = 0 then Inc(I);
  until IOResult <> 0;
  ClrIO;
  lAssignText(T, EX);
  lRewriteText(T);
  lGetDir(0, S);
  WriteLn(T.T, '@'+Copy(S,1,2));
  WriteLn(T.T, '@cd "'+S+'"');
  S := Command;
  if PosChar(';', S) > 0 then
   begin
     Replace(';;', #0, S);
     While (S <> '') and (PosChar(';', S) <> 0) do
      begin
        I := PosChar(';', S);
        M := Copy(S, 1, I-1); Replace(#0, ';', M);
        WriteLn(T.T, M);
        Delete(S, 1, I);
      end;
     Replace(#0, ';', S);
     WriteLn(T.T, S);
   end else WriteLn(T.T, Command);
  if not Bckg and (ShiftState and $20 = 0) then WriteLn(T.T, '@pause');
  Write(T.T, '@del "'+EX+'" & exit'^Z);
  Close(T.T);
  OS2comspec := GetEnv('OS2COMSPEC');
  if OS2comspec = ''
      then OS2comspec := Chr(96+GetBootDrive) + ':\os2\cmd.exe';
  StartOS2Session($20, Session, Bckg, Command, OS2comspec,
                     '/c '+EX, '');
end;
        {DataCompBoy}

end.
