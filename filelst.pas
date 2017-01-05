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
unit Filelst;

interface

uses
  Collect;

procedure MakeListFile(APP: Pointer; Files: PCollection);
function ParseAddress(Address: String; var Zone, Net, Node, Point:
    word): boolean;

implementation
uses
  Startup, Lfn, Messages, Objects, FilesCol, advance2, advance1,
    UserMenu,
  advance, HistList, Commands, DNApp, DNUtil, Tree, Views, Drivers,
    Drives
  {, dnfuncs} {надо вставлять до Dos}
  , Dos
  , ErrMess, FlPanelX
  ;

function ParseAddress(Address: String; var Zone, Net, Node, Point:
    word): boolean;
  var
    i, j: integer;
  begin
    ParseAddress := False;
    Point := 0;
    i := PosChar('@', Address);
    if i > 0 then
      Delete(Address, i, 255);
    i := PosChar(':', Address);
    if i > 0 then
      begin
        Val(Copy(Address, 1, i-1), Zone, j);
        if j <> 0 then
          exit;
        Delete(Address, 1, i);
      end;
    i := PosChar('/', Address);
    if i > 0 then
      begin
        Val(Copy(Address, 1, i-1), Net, j);
        if j <> 0 then
          exit;
        Delete(Address, 1, i);
      end;
    i := PosChar('.', Address);
    if i > 0 then
      begin
        Val(Copy(Address, i+1, MaxStringLength), Point, j);
        if j <> 0 then
          exit;
        Delete(Address, i, MaxStringLength);
      end;
    if Length(Address) > 0 then
      begin
        Val(Address, Node, j);
        if j <> 0 then
          exit;
      end;
    ParseAddress := True;
  end { ParseAddress };

procedure MakeListFile;
  label AddrError, Retry;
  var
    i, j, k: integer;
    D, dr, SR, SN: String;
    P: PFileRec;
    s: TMakeListRec;
    t, HF: lText;
    Nm: String;
    XT: String;
    FidoMode: boolean;
    SS: String;
    PP: PString;
    zz, NN, ND, PT: word;
    BB: boolean;
    FLD, Dr_: String;
    CurPos: integer;
    UPr: tUserParams;
    rc: longInt;

  function GetNextName(var TheString, TheName: String): boolean;
    var
      j: boolean;
    begin
      while (CurPos <= Length(TheString)) and (TheString[CurPos] in [
          ';', ' '])
      do
        Inc(CurPos);
      TheName := '';
      j := False;
      while (CurPos <= Length(TheString)) and ((TheString[CurPos] <>
          ';') or j)
      do
        begin
          if TheString[CurPos] = '"' then
            j := not j;
          TheName := TheName+TheString[CurPos];
          Inc(CurPos)
        end;
      while TheName[Length(TheName)] = ' ' do
        SetLength(TheName, Length(TheName)-1);
      GetNextName := TheName <> ''
    end { GetNextName };

  function SomeFilesExist(var NameList: String): boolean;
    var
      TheName: String;
    begin
      SomeFilesExist := False;
      CurPos := 1;
      while GetNextName(NameList, TheName) do
        if ExistFile(TheName)
        then
          begin
            SomeFilesExist := True;
            exit
          end
    end;

  procedure MakeStr(D: String);
    label Fail;
    var
      Drr: boolean;
    begin
      { BB means "Is filename occurs in Action?" }
      Replace('!!', #1, D);
      Replace('##', #2, D);
      Replace('$$', #3, D);
      Replace('&&', #4, D);
      if (PosChar('!', D) = 0) and (PosChar('#', D) = 0) then
        if (D[Length(D)] in [#8, ' ']) or ((Length(D) = 1) and (D[1] in
            ['^', #2]))
        then
          D := D+'!.!'
        else
          D := D+' !.!';
      if ((s.Options and cmlAutoDetermine = cmlAutoDetermine) and
        {$IFNDEF OS2}
        (lfGetShortFileName(SR) <> lfGetShortFileName(dr)) and
        {$ELSE}
        (SR <> dr) and
        {$ENDIF}
        (Pos('!\', D) = 0) and
        (Pos('!/', D) = 0) and
        (Pos('!:', D) = 0) and
        (Pos('#\', D) = 0) and
        (Pos('#/', D) = 0) and
        (Pos('#:', D) = 0))
        or ((s.Options and cmlPathNames <> 0) and
        (s.Options and cmlAutoDetermine = 0))
      then
        begin{ Need to force insert !:!\ }
          k := Pos('.!', D);
          if k = 0 then
            begin
              k := PosChar('!', D);
              if k = 0 then
                begin
                  k := Pos('.#', D);
                  if k = 0 then
                    begin
                      k := PosChar('#', D);
                      if k = 0 then
                        goto Fail;
                    end
                  else if (k <> 1) and (D[k-1] in ['!', '#']) then
                      Dec(k)
                  else
                    goto Fail;
                end
              else if (k <> 1) and (D[k-1] in ['!', '#']) then
                Dec(k)
              else
                goto Fail;
            end
          else if (k <> 1) and (D[k-1] in ['!', '#']) then
            Dec(k)
          else
            goto Fail;
          Insert('!:!\', D, k);
        end;
Fail: { Cannot find place to insert !\ }
      Replace(#1, '!!', D);
      Replace(#2, '##', D);
      Replace(#3, '$$', D);
      Replace(#4, '&&', D);
      D := MakeString(D, @UPr, False, nil);
      Writeln(t.t, D);
    end { MakeStr };

  begin { MakeListFile }
    if Files^.Count = 0 then
      exit;
    Message(APP, evBroadcast, cmGetUserParams, @UPr);
    FillChar(s, SizeOf(s), 0);
    s.FileName := HistoryStr(hsMakeList, 0);
    if s.FileName = '' then
      s.FileName := 'DNLIST'+CmdExt;
    s.Action := HistoryStr(hsExecDOSCmd, 0);
    s.Header := HistoryStr(hsMakeListHeader, 0); {JO}
    {S.Header := '';}s.HeaderMode := hfmAuto;
    s.Footer := HistoryStr(hsMakeListFooter, 0); {JO}
    {S.Footer := '';}s.FooterMode := hfmAuto;
    s.Options := MakeListFileOptions;
    if s.Options and cmlPathNames <> 0 then
      begin
        BB := False;
        PP := PFileRec(Files^.At(0))^.Owner;
        for i := 1 to Files^.Count-1 do
          begin
            BB := BB or (PFileRec(Files^.At(i))^.Owner <> PP);
            if BB then
              break;
          end;
        if BB then
          s.Options := s.Options or cmlPathNames
        else
          s.Options := s.Options and not cmlPathNames;
      end;
    if (ExecResource(dlgMakeList, s) <> cmOK) then
      exit;
    MakeListFileOptions := s.Options;
    while s.Action[Length(s.Action)] = ' ' do
      SetLength(s.Action, Length(s.Action)-1);
    while s.Header[Length(s.Header)] = ' ' do
      SetLength(s.Header, Length(s.Header)-1);
    while s.Footer[Length(s.Footer)] = ' ' do
      SetLength(s.Footer, Length(s.Footer)-1);
    if s.Action <> '' then
      s.Action := s.Action+' ';
    FileMode := 2;
    Abort := False;
    if s.FileName[1] in ['+', '%', '/'] then
      begin
Retry:
        FidoMode := True;
        SS := FMSetup.DIZ;
        i := Pos('/FIDO=', SS);
        if i = 0 then
          begin
            SS := '';
AddrError:
            if ExecResource(dlgFTNInfo, SS) = cmCancel then
              exit;
            if PosChar(',', SS) > 0 then
              i := Pos('/FIDO=', FMSetup.DIZ);
            if i > 0 then
              begin
                for j := i to 255 do
                  if FMSetup.DIZ[j] = ';' then
                    break;
                Delete(FMSetup.DIZ, i, j-i+1);
              end;
            if FMSetup.DIZ[Length(FMSetup.DIZ)] <> ';' then
              FMSetup.DIZ := FMSetup.DIZ+';';
            FMSetup.DIZ := FMSetup.DIZ+'/FIDO='+SS;
            Message(Application, evCommand, cmUpdateConfig, nil);
            goto Retry;
          end;
        Delete(SS, 1, i+5);
        i := PosChar(';', SS);
        if i = 0 then
          i := Length(SS)+1;
        SetLength(SS, i-1);
        i := PosChar(',', SS);
        if i = 0 then
          goto AddrError;
        ParseAddress(Copy(SS, 1, i-1), zz, NN, ND, PT);
        k := zz;
        ParseAddress(Copy(s.FileName, 2, MaxStringLength), zz, NN, ND,
          PT);
        Delete(SS, 1, i);
        if SS[Length(SS)] = '\' then
          SetLength(SS, Length(SS)-1);
        lFSplit(SS, dr, Nm, XT);
        if k <> zz then
          XT := '.'+Copy(Hex4(zz), 2, 3);
        SS := dr+Nm+XT;
        if PT = 0 then
          Nm := Hex4(NN)+Hex4(ND)
        else
          begin
            SS := MakeNormName(SS, Hex4(NN)+Hex4(ND)+'.PNT\');
            Nm := Hex8(PT);
          end;
        XT := '.hlo';
        case s.FileName[1] of
          '+':
            XT[2] := 'c';
          '%':
            XT[2] := 'f';
        end {case};
        SS := MakeNormName(SS, Nm+XT);
        s.FileName := SS;
        s.Options := s.Options or cmlPathNames;
      end
    else
      FidoMode := False;
    D := lFExpand(s.FileName);
    lFSplit(D, dr, Nm, XT);
    ClrIO;
    FLD := dr;
    CreateDirInheritance(dr, False);
    if Abort then
      exit;
    lAssignText(t, D);
    ClrIO;
    lResetText(t);
    if IOResult = 0 then
      begin
        Close(t.t);
        PP := @SS;
        SS := Cut(D, 40);
        if FidoMode then
          i := cmOK
        else
          i := MessageBox(GetString(dlED_OverQuery)
          , @PP, mfYesButton+mfCancelButton+mfAppendButton+mfWarning);
        case i of
          cmOK:
            lAppendText(t);
          cmYes:
            lRewriteText(t);
          else
            exit;
        end {case};
      end
    else
      lRewriteText(t);
    if Abort then
      begin
        Close(t.t);
        exit;
      end;
    rc := IOResult;
    if rc <> 0 then
      begin
        MessFileNotOpen(Cut(s.FileName, 40), rc);
        exit;
      end;
    if (s.Header <> '') and not FidoMode then
      begin
        if (s.HeaderMode = hfmInsertText) or
          ((s.HeaderMode = hfmAuto) and not SomeFilesExist(s.Header))
        then
          Writeln(t.t, s.Header)
        else
          begin
            CurPos := 1;
            while GetNextName(s.Header, SS) do
              begin
                lAssignText(HF, SS);
                ClrIO;
                lResetText(HF);
                rc := IOResult;
                if (rc <> 0) and (s.HeaderMode = hfmInsertFiles)
                then
                  begin
                    MessFileNotOpen(Cut(SS, 40), rc);
                  end;
                while (IOResult = 0) and not Eof(HF.t) do
                  begin
                    readln(HF.t, SS);
                    Writeln(t.t, SS);
                  end;
                Close(HF.t)
              end;
            ClrIO;
          end;
      end;
    Message(Desktop, evBroadcast, cmGetUserParams, @UPr);
    for i := 1 to Files^.Count do
      begin
        P := Files^.At(i-1);
        UPr.active := P;
        {AK155 23-09-2003: разотметка по одному файлу тормозит страшно при
большом числе файлов
    Message(APP, evCommand, cmCopyUnselect, P);
/AK155}
        BB := False;
        SR := P^.Owner^;
        Replace('!', #0, SR);
        if SR[Length(SR)] <> '\' then
          SR := SR+'\';
        SS := s.Action;
        if SS <> '' then
          begin
            Replace(';;', #1, SS);
            while SS <> '' do
              begin
                j := PosChar(';', SS);
                if j = 0 then
                  j := Length(SS)+1;
                MakeStr(fReplace(#1, ';', Copy(SS, 1, j-1)));
                Delete(SS, 1, j);
              end;
          end
        else if (s.Options and cmlPathNames <> 0) or (s.Options and
            cmlAutoDetermine <> 0) and (SR <> FLD)
        then
          MakeStr('!:!\!.!')
        else
          MakeStr('!.!');
      end;
    if (s.Footer <> '') and not FidoMode then
      begin
        if (s.FooterMode = hfmInsertText) or
          ((s.FooterMode = hfmAuto) and not SomeFilesExist(s.Footer))
        then
          Writeln(t.t, s.Footer)
        else
          begin
            CurPos := 1;
            while GetNextName(s.Footer, SS) do
              begin
                lAssignText(HF, SS);
                ClrIO;
                lResetText(HF);
                rc := IOResult;
                if (rc <> 0) and (s.FooterMode = hfmInsertFiles)
                then
                  begin
                    MessFileNotOpen(Cut(SS, 40), rc);
                  end;
                while (IOResult = 0) and not Eof(HF.t) do
                  begin
                    readln(HF.t, SS);
                    Writeln(t.t, SS);
                  end;
                Close(HF.t)
              end;
            ClrIO;
          end;
      end;
    Close(t.t);
    { AK155 23-09-2003 Теперь скопом снимаем всю отметку. Делать это надо
обязательно до RereadDirectory, так как она страшно тормзит при большом
числе отмеченных файлов. }
    ClearSelection(APP, PFilePanelRoot(APP)^.Files);
    {/AK155}
    RereadDirectory(dr);
    GlobalMessage(evCommand, cmRereadInfo, nil);
    GlobalMessage(evCommand, cmRereadTree, @Dr);
  end { MakeListFile };
{-DataCompBoy-}

end.
