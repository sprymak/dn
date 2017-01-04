{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.07/DOS
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

unit DNExec;

interface
uses UserMenu, Startup, Objects, FilesCol, Commands;

procedure SaveDsk;
procedure ExecString(S: PString);
function  SearchExt(FileRec: PFileRec; var HS: String): Boolean; {DataCompBoy}
function  ExecExtFile(const ExtFName: string; UserParams: PUserParams; SIdx: TStrIdx): Boolean; {DataCompBoy}
procedure ExecFile(const FileName: string); {DataCompBoy}

implementation
uses DnUtil, Advance, DnApp, Advance1, Lfn, LfnCol, Dos, Advance3, FlPanelX,
     CmdLine, Views, Advance2, Drivers, Advance4;

procedure SaveDsk;
begin
 ClrIO;
 if StartupData.Unload and osuAutosave = 0 then
  PDNApplication(Application)^.SaveDesktop(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
end;


        {-DataCompBoy-}
procedure ExecString(S: PString);
 var F1: lText;
     I: Integer;
     M: String;
     DT: DateTime;
 label 1;
begin
 M:=S^;
 DelRight(M);
 if not Chk4Dos and (Pos('||', M) <> 0) then
  begin
    lAssignText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT'); ClrIO;
    lRewriteText(F1); if IOResult <> 0 then begin Close(F1.T); Exit; end;
    I := 0;
    repeat
      I := Pos('||', M);
      if I = 0 then I := Length(M)+1;
      WriteLn(F1.T, MakeCMDParams(Copy(M, 1, I-1),
                                CnvString(CurFileActive),
                                CnvString(CurFilePassive)));
      Delete(M, 1, I+1);
    until (M = '');
    Close(F1.T);
    M := SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT ';
  end else M := MakeCMDParams(M, CnvString(CurFileActive), CnvString(CurFilePassive));
1:
 M := ' '+M+#13; Dec(M[0]);
{$IFDEF BIT_16}
 if (LoaderSeg <> 0) then Move(M, mem[LoaderSeg:CommandOfs], Length(M)+2);
{$ELSE}
 Move(M, Ptr(LoaderSeg,CommandOfs)^, Length(M)+2);
{$ENDIF}
 if (SystemData.Options and ossFastExec <> 0) or RunFrom2E then asm
                                               mov ax, 9903h
                                               mov cl, 1
                                               int 2Fh
                                            end;
 if TimerMark then DDTimer := Get100s
              else DDTimer := 0;
 asm
    mov ax, 9904h
    mov  dx, word ptr DDTimer
    mov  cx, word ptr DDTimer+2
    int 2Fh
    mov ax, 9902h
    mov cl, 1
    int 2Fh
 end;
 Halt(1);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function SearchExt(FileRec: PFileRec; var HS: String): Boolean;
var
  AllRight : Boolean;
  f        : PTextReader;
  F1       : lText;
  s,s1     : String;
  BgCh,EnCh: Char;
  EF, First: Boolean;
  I        : Integer;
  Local    : Boolean;
  FName,LFN: string;
  UserParam: TUserParams;
label RL;

begin
  First := True;
  Message(Desktop, evBroadcast, cmGetUserParams, @UserParam);
  UserParam.Active:=FileRec;
  FName:=FileRec^.Name;
  LFN  :=GetLFN      (FileRec^.LFN);
  lGetDir(0, ActiveDir);
  SearchExt:=False;
  Local := On;
  f := New(PTextReader, Init('DN.EXT'));
  if f=nil then
   begin
  RL:
     Local := Off;
     f := New(PTextReader, Init(SourceDir+'DN.EXT'));
   end;
  if f=nil then exit; AllRight:=False;
  BgCh:='{';EnCh:='}'; Abort := Off; EF:=Off;
  if PShootState and 8 > 0 then begin BgCh:='[';EnCh:=']';end else
  if PShootState and 3 > 0 then begin BgCh:='(';EnCh:=')';end;
  While (not f^.EOF) and (not AllRight) do begin
    S := f^.GetStr;
    if S[1] <> ' ' then
     begin
      I := PosChar(BgCh, S); if (I = 0) or (S[I+1]=BgCh) then Continue;
      S1 := Copy(S, 1, I-1);
      if InExtFilter(FName, S1) or InExtFilter(LFN, S1) then
       begin
        lAssignText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT'); ClrIO;
        lRewriteText(F1); if IOResult <> 0 then begin Dispose(F,Done); exit; end;
        Writeln(F1.T, '@echo off');
        System.Delete(S, 1, PosChar(BgCh, S));
        repeat
         Replace(']]', #0, S);
         Replace('))', #1, S);
         Replace('}}', #2, S);
         DelLeft(S); DelRight(S);
         if S[Length(S)] = EnCh then
          begin Dec(S[0]); EF := On; if S <> '' then
           begin
             Replace(#0, ']', S);
             Replace(#1, ')', S);
             Replace(#2, '}', S);
             S := MakeString(S, @UserParam, off, nil);
             HS := S;
             WriteLn(F1.T, S); Break
           end;
          end;
         if S <> '' then
          begin
           Replace(#0, ']', S);  Replace(#1, ')', S);  Replace(#2, '}', S);
           if (BgCh <> '[') then S := MakeString(S, @UserParam, off, nil);
           if First and (BgCh <> '[') then HS := S;
           WriteLn(F1.T, S);
           First := False;
          end;
         if (F^.EOF) then Break;
         if not EF then S := F^.GetStr;
        until (IOResult <> 0) or Abort or EF;
        Close(F1.T);
        AllRight := On;
       end;
      end;
   end;
  Dispose(F,Done);
  if not EF and not Abort and Local then Goto RL;
  if EF and (BgCh = '[') then
   begin
    EraseFile( SwpDir+'$DN'+ItoS(DNNumber)+'$.MNU' );
    lRenameText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$.MNU');
    EF := ExecUserMenu(Off);
    if not EF then lEraseText(F1);
   end;
  SearchExt:=not Abort and EF;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function ExecExtFile(const ExtFName: string; UserParams: PUserParams; SIdx: TStrIdx): Boolean;
 var F: PTextReader;
     S,S1: String;
     FName, LFN: String;
     Event: TEvent;
     I,J: Integer;
     PP: PView;
     Success, CD: Boolean;
     Local: Boolean;
 label 1,1111, RepeatLocal;

begin
 ExecExtFile := Off;
 FileMode := $40;
 Local := On;
 LFN:=GetLFN(UserParams^.Active^.LFN);
 FName:=UserParams^.Active^.Name;

 F := New(PTextReader, Init(ExtFName));

 if F = nil then
  begin
RepeatLocal:
    Local := Off;
    F := New(PTextReader, Init(SourceDir+ExtFName));
  end;
 if F = nil then Exit;
 While not F^.EOF do
   begin
      S := F^.GetStr;
      DelLeft(S);
      S1:=Copy(S, 1, pred(PosChar(':', S)));
      if S1='' then continue;
      if InExtFilter(FName, S1) or InExtFilter(LFN, S1) then goto 1111;
   end;
   ExecExtFile := Off;
   Dispose(F, Done);
   if Local then Goto RepeatLocal;
   Exit;
1111:
   Delete(S, 1, succ(Length(S1)));
   Dispose(F, Done);
   if not Application^.Valid(cmQuit) then Exit;
   ClrIO;
   S1:='';
   S:=MakeString(S, UserParams, off, @S1);
   if S1<>''
    then TempFile:='!'+S1+'|'+MakeNormName(UserParams^.Active^.Owner^, LFN)
    else if TempFile<>''
          then TempFile:=MakeNormName(UserParams^.Active^.Owner^, LFN);
   if Abort then Exit;
   if S[1] = '*' then DelFC(S);
   PP := _WriteMsg(' '+GetString(SIdx));
   if not CheckExit then Begin ExecExtFile:=false; Exit; end;
   lGetDir(0, S1);
   if UpStrg(MakeNormName(lfGetLongFileName(UserParams^.Active^.Owner^),'.'))<>
      UpStrg(MakeNormName(S1,'.'))
    then begin
          DirToChange:=S1;
          lChDir(lfGetLongFileName(UserParams^.Active^.Owner^));
         end;
   ExecExtFile := On;
   Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
   SaveDsk;
   Exiting:=true;
   Application^.Done;
   ExecString(@S);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure ExecFile(const FileName: string);
 var S, L, M: String;
     fr: PFileRec;

 procedure PutHistory(B: Boolean);
 begin
  if M = '' then Exit;
  CmdLine.Str := M;
  CmdLine.StrModified := On;
  CmdDisabled := B;
  Message(CommandLine, evKeyDown, kbDown, nil);
  Message(CommandLine, evKeyDown, kbUp, nil);
 end;

 procedure RunCommand(B: Boolean);
   var ST: SessionType;
 begin
    if (PCommandLine(CommandLine)^.LineType in [ltOS2Window,ltOS2FullScreen]) then
     begin
       if PCommandLine(CommandLine)^.LineType = ltOS2FullScreen then ST := stOS2FullScreen
         else ST := stOS2Windowed;
       RunOS2Command(M, Off, ST);
       CmdLine.StrModified := On;
       Message(CommandLine, evKeyDown, kbDown, nil);
       Exit;
     end;
    SaveDsk;
    Exiting:=True;
    Application^.Done;
    if B then begin WriteLn(#13#10, ActiveDir+'>', M); M := M + #13 end;
    ExecString(@M);
 end;

label ex;
begin
 fr:= CreateFileRec(FileName);
 S := MakeFileName(fr^.Name);
 L := GetLFN(fr^.LFN);
 FreeStr := '';
 M := '';
 if (ShiftState and (3 or kbAltShift) <> 0) or
    (not InExtFilter(S, Executables) and
     not InExtFilter(L, Executables)) then
  begin
   if SearchExt(fr, M) then
   begin
    PutHistory(On);
    M := SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT ' + FreeStr;
    RunCommand(Off);
   end else goto ex;
  end;
 M := S;
 PutHistory(Off);
 M := S;
 RunCommand(On);
ex:
 DelFileRec(fr);
end;

end.
