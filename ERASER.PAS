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

unit Eraser;

interface

uses
  Objects, Files, Filediz, Collect;

procedure EraseFiles(Files: PCollection);
procedure SetVLabel;
function ValidErase(Files: PCollection): boolean;

implementation
uses
  Dos, Lfn {DataCompBoy}, FilesCol, Commands, advance, advance1,
    advance2,
  advance3, Startup, Messages, xTime, Drivers, Tree, Memory,
  DNApp, Gauge, Views, RStrings, Dialogs, Drives, FileCopy
  , fnotify
  {JO} {$IFDEF OS2}, VPUtils {$ENDIF}, FlTl

  ;

{-DataCompBoy-}
function ValidErase;
  var
    PF: PFileRec;
    s: String;
    i: integer;
  begin
    ValidErase := False;
    if Files = nil then
      exit;
    if Files^.Count = 0 then
      exit;
    if Files^.Count = 1 then
      begin
        PF := Files^.At(0);
        if PF^.Attr and Directory <> 0 then
          s := GetString(dlEraseConfirmDir)
        else
          s := GetString(dlEraseConfirm1);
        s := s+'"'+ {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Cut(PF^
          .FlName[True], 40))+'"'+' ?';

      end
    else
      s := GetString(dlEraseConfirms1);
    if (Files^.Count = 1) and (Confirms and cfSingleErase = 0) then
      i := cmYes
    else
      i := MessageBox(s, nil, mfConfirmation+mfYesButton+mfNoButton);
    if (i <> cmYes) then
      exit;
    if (Files^.Count > 1) and (Confirms and cfMultiErase <> 0) then
      begin
        s := GetString(dlEraseConfirm2)+ItoS(Files^.Count)+' '+
          GetString(dlDIFiles)+' ?';
        if MessageBox(s, nil, mfConfirmation+mfYesButton+mfNoButton) <>
            cmYes
        then
          exit;
      end;
    ValidErase := True;
  end { ValidErase };
{-DataCompBoy-}

{-DataCompBoy-}
procedure EraseFiles;
  var
    PInfo: PWhileView;
    R: TRect;
    EraseCancel: boolean;
    tmr: TEventTimer;
    i, j: integer;
    s, dr: String;
    Ch: Char;
    F: lFile;
    DOSMode, DirEnd, DirModified: boolean;
    PF: PFileRec;
    //    DrivesSet: Set of Char;
    RereadCollection: PStringCollection; {Cat}
    PS: PString; {Cat}
    DeleteAllFiles: boolean;
    Params: record{AK155}
      FName: PString;
      rc: longInt;
      end;

  procedure InfoWrite(n: integer; const s: Str40);
    begin
      PInfo^.Write(n, s);
    end;

  procedure CalcCancel;
    begin
      if EraseCancel then
        exit;
      if TimerExpired(tmr) then
        begin
          DispatchEvents(PInfo, EraseCancel);
          EraseCancel := (EraseCancel or CtrlBreakHit) and (
            MessageBox(GetString(dlQueryAbort), nil, mfYesNoConfirm) =
            cmYes);
          CtrlBreakHit := False;
          NewTimer(tmr, 1);
        end;
    end;

  function Over(s: String): word;
    var
      i: integer;
    begin
      if Confirms and cfEraseReadonly = 0 then
        begin
          Over := cmYes;
          exit
        end;
      i := MessageBox(^C+GetString(dlFile)+' "'+Cut(s, 40)+'"'+
        GetString(dlEraseRO), nil, mfYesNoConfirm+mfAllButton);
      if i = cmOK then
        DeleteAllFiles := True;
      Over := i;
      if i = cmOK then
        Over := cmYes;
    end;

  {AK155 При отказе (DOSDelDir=false) Params.RC возвращает код
завершения неудачной операции, если причина отказа - ошибка
операции с файлом или каталогом. При посторонних причинах отказа
(например, Abort) будет Params.RC=0}
  function DOSDelDir: boolean;
    var
      DC: PDirCol;
      TD: PDirCol;
      SR: lSearchRec;
      s: String;
    label TryDel;
    begin
      DOSDelDir := False;
      Abort := False;
      Params.rc := 0;
      New(DC, Init($10, $10, False));
      New(TD, Init($10, $10, False));
      DC^.Insert(NewStr(FreeStr));

      while DC^.Count > 0 do
        begin
          FreeStr := PString(DC^.At(0))^;
          DC^.AtFree(0);
          PInfo^.Write(2, Cut(GetName(FreeStr), 40));

          if word(Length(FreeStr))+word(Length(x_x)) > MaxPathLen-1
          then
            begin
              Abort := True;
              break
            end;

          lFindFirst(MakeNormName(FreeStr, x_x), AnyFile, SR); {JO}
          while DOSError = 0 do
            begin
              if Abort then
                break;
              if (SR.FullName = '.') or (SR.FullName = '..') then
                begin
                  lFindNext(SR);
                  continue
                end;
              if (word(Length(FreeStr))+word(Length(SR.FullName))) >
                  MaxPathLen-1
              then
                begin
                  Abort := True;
                  break;
                end;
              if (SR.SR.Attr and Directory) <> 0 then
                begin
                  s := MakeNormName(FreeStr, SR.FullName);
                  {$IFDEF Win32}
                  if SR.SR.FindData.dwFileAttributes and
                    FILE_ATTRIBUTE_REPARSE_POINT <> 0
                  then
                    begin{ Симлинк: удаляем сам симлинк }
                      lRmDir(s);
                      Params.rc := IOResult;
                      if Params.rc <> 0 then
                        begin
                          Abort := True;
                          break;
                        end;
                    end
                  else
                    {$ENDIF}
                    begin
                      if MaxAvail < Length(s)+$40
                      then
                        begin
                          Abort := True;
                          break;
                        end;
                      DC^.AtInsert(0, NewStr(s));
                    end;
                end
              else
                begin
                  if (word(Length(FreeStr))+word(Length(SR.FullName))) >
                      MaxPathLen-1
                  then
                    begin
                      Abort := True;
                      break;
                    end;
                  ClrIO;
                  lAssignFile(F, MakeNormName(FreeStr, SR.FullName));
                  lSetFAttr(F, 0);
                  lEraseFile(F);
                  Params.rc := IOResult;
                  if Params.rc <> 0 then
                    begin
                      Abort := True;
                      break;
                    end;
                end;
              lFindNext(SR);
            end;
          lFindClose(SR);
          if not Abort then
            begin
              if MaxAvail < Length(FreeStr)+$40 then
                begin
                  Abort := True;
                  break
                end;
              if FreeStr[Length(FreeStr)] = '\'
              then
                TD^.AtInsert(0, NewStr(Copy(FreeStr, 1, Length(
                  FreeStr)-1)))
              else
                TD^.AtInsert(0, NewStr(FreeStr));
            end
          else
            break;
        end;
      DC^.FreeAll;
      Dispose(DC, Done);
      DOSDelDir := not Abort;
      if not Abort then
        while TD^.Count > 0 do
          begin
            ClrIO;
            s := PString(TD^.At(0))^;
TryDel:
            lRmDir(s);
            Params.rc := IOResult;
            if Params.rc <> 0 then
              if SysErrorFunc(Params.rc, byte(s[1])-byte('A')) = 1
              then
                goto TryDel;
            if Params.rc <> 0 then
              begin
                DOSDelDir := False;
                FreeStr := PString(TD^.At(0))^;
                break;
              end;
            TD^.AtFree(0);
          end;
      TD^.FreeAll;
      Dispose(TD, Done);
    end { DOSDelDir: };

  function DeleteDirectory(Dir: String; Cluster: word): boolean;
    var
      s: String;
      dr: word;
      j123qwe: boolean;
      SR: lSearchRec;
    begin
      DeleteDirectory := False;
      Dir := lFExpand(Dir);
      if Dir[Length(Dir)] = '.' then
        SetLength(Dir, Length(Dir)-1);
      if not DeleteAllFiles then
        begin
          s := MakeNormName(Dir, x_x);
          DOSError := 0;
          lFindFirst(s, AnyFile, SR); {JO}
          if IsDummyDir( {$IFDEF OS_DOS}SR.SR.Name {$ELSE}SR.
              FullName {$ENDIF})
          then
            lFindNext(SR);
          if IsDummyDir( {$IFDEF OS_DOS}SR.SR.Name {$ELSE}SR.
              FullName {$ENDIF})
          then
            lFindNext(SR);
          lFindClose(SR);
          if Abort then
            exit;
          dr := cmOK;
          if (DOSError = 0) and (not DeleteAllFiles) then
            begin
              s := Dir;
              Dec(SkyEnabled);
              if Confirms and cfEraseSubDir = 0 then
                dr := cmYes
              else
                dr := MessageBox(^C+GetString(dlDirectory)+' '+Cut(s, 40
                  )+GetString(dlEraseDirNotEmpty),
                nil, mfConfirmation+mfNoButton+mfAllButton+
                  mf2YesButton+mfCancelButton);
              Inc(SkyEnabled);
              DeleteAllFiles := dr = cmOK;
              Abort := dr = cmCancel;
            end;
          if not (dr in [cmYes, cmOK]) then
            exit;
        end;
      FreeStr := Dir;
      j123qwe := not DOSDelDir;
      if j123qwe then
        begin
          FreeStr := Cut(FreeStr, 40);
          Params.FName := @FreeStr;
          s := GetString(dlEraseCantDelDir);
          if Params.rc <> 0 then
            s := s+^M^C'(RC=%d)';
          MessageBox(s, @Params, mfError+mfOKButton);
          Abort := True;
        end;
      PInfo^.DrawView;
      CreateDirectory(Dir, True);
      DeleteDirectory := True;
    end { DeleteDirectory };

  var
    Fls: PCollection;
    {$IFDEF OS_DOS}
    Flush: boolean;
    {$ENDIF}
    Ask: integer;
    iLFN: TUseLFN;
  label LLL, DeleteDirDIZ;

  begin { EraseFiles }
    if not ValidErase(Files) then
      exit;
    NotifySuspend; {Cat}
    DeleteAllFiles := False;
    Abort := False;
    ClrIO;
    CtrlBreakHit := False; {JO}
    R.Assign(1, 1, 26, 9);
    New(PInfo, Init(R));
    PInfo^.Top := GetString(dlErase);
    DOSMode := True;

    EraseCancel := False;
    Abort := False;
    NewTimer(tmr, 0);
    CalcCancel;

    if Abort then
      begin
        Dispose(PInfo, Done);
        NotifyResume; {Cat}
        exit;
      end;
    Inc(SkyEnabled);
    Desktop^.Insert(PInfo);

    for i := 1 to Files^.Count do
      begin
        PF := Files^.At(i-1);
        {JO: файлы, найденные в архивах нельзя удалить из панели поиска}
        if PathFoundInArc(PF^.Owner^) then
          continue;
        {/JO}
        s := PF^.FlName[True];
        CalcCancel;
        if (PF <> nil) and not EraseCancel and not Abort then
          if (PF^.Attr and Directory = 0) then
            begin
              InfoWrite(1, GetString(dlErasingFile));
              InfoWrite(2, Cut(s, 40));
              j := cmYes;
              if not DeleteAllFiles and (PF^.Attr and ReadOnly <> 0)
              then
                j := Over(s);
              EraseCancel := EraseCancel or (j = cmCancel);
              if j = cmYes then
                begin
                  s := MakeNormName(PF^.Owner^, s);
                  lAssignFile(F, s);
                  ClrIO;
                  if PF^.Attr and ReadOnly <> 0 then
                    begin
LLL:
                      lSetFAttr(F, Archive);
                      ClrIO;
                      lEraseFile(F);
                    end
                  else
                    begin
                      lEraseFile(F);
                      if IOResult <> 0 then
                        goto LLL;
                    end;
                  if IOResult <> 0 then
                    begin
                      s := lFExpand(s);
                      GlobalMessage(evBroadcast, cmReleaseFile, @S);
                      ClrIO;
                      lEraseFile(F);
                    end;
                  Ask := IOResult;
                  {if IOResult = 0 then}
                  if Ask = 0 then
                    begin
                      if (PF^.DIZ <> nil) and
                        (FMSetup.Options and fmoPreserveDesc = 0)
                      then
                        begin
                          InfoWrite(1, GetString(dlDeletingDIZ));
                          for iLFN := Low(TUseLFN) to High(TUseLFN) do
                            DeleteDiz(CalcDPath(PF^.DIZ, PF^.Owner), PF^
                              .FlName[iLFN]);
                        end;
                    end
                  else
                    begin
                      if Ask in [2, 3, 110] then
                        MessageBox(GetString(dlErasingNoFile)+s
                          {+ '; RC = %d'}, nil {@Ask}, mfError+
                          mfOKButton)
                      else
                        begin
                          Ask := SysErrorFunc(Ask, GetDrive);
                          if (Ask = 1) then
                            goto LLL;
                        end;
                    end
                    ;
                end;
            end
          else
            begin
              InfoWrite(1, GetString(dlErasingDir));
              InfoWrite(2, Cut(s, 40));
              {$IFDEF OS_DOS}
              s := lfGetLongFileName(MakeNormName(PF^.Owner^, s));
              {$ELSE}
              s := MakeNormName(PF^.Owner^, s);
              {$ENDIF}
              {$IFDEF WIN32}
              if PF^.Attr and FILE_ATTRIBUTE_REPARSE_POINT <> 0 then
                begin
                    { Симлинк: удаляем без подтверждений сам симлинк}
                  lRmDir(s);
                  if IOResult = 0 then
                    goto DeleteDirDIZ;
                end
              else
                {$ENDIF} if DeleteDirectory(s, 0) then
                begin
DeleteDirDIZ:
                  for iLFN := Low(TUseLFN) to High(TUseLFN) do
                    DeleteDiz(CalcDPath(PF^.DIZ, PF^.Owner), PF^.
                      FlName[iLFN]);
                end;
            end
        else
          break;
      end;
    {$IFDEF OS_DOS}
    Flush := ((SystemData.Options shl 3) and ossFlushDsk <> 0);

    if Flush then
      begin
        PInfo^.ClearInterior;
        InfoWrite(1, GetString(dlFlushingBuffers));
      end
    else
      {$ENDIF}
      Dispose(PInfo, Done);

    {Cat: Во-первых, совершенно непонятно, зачем перечитывать _все_ диски
      Во-вторых, такой способ не работает с сетевыми путями}
    (*
  DrivesSet := [];
  for I := 0 to Files^.Count-1 do
      DrivesSet := [UpCase(PFileRec(Files^.At(I))^.Owner^[1])] + DrivesSet;
  S := 'A:\';
  for ch := 'A' to 'Z' do
    if ch in DrivesSet then
      begin
        S[1]:=ch;
        GlobalMessage(evCommand, cmRereadTree, @S);
        RereadDirectory(S);
      end;
*)
    RereadCollection := New(PStringCollection, Init(32, 32, False));
      {сортированная, без повторов}
    for i := 0 to Files^.Count-1 do
      RereadCollection^.Insert(PFileRec(Files^.At(i))^.Owner);
    s := #0;
    for i := 0 to RereadCollection^.Count-1 do
      begin
        PS := RereadCollection^.At(i);
        if s <> Copy(PS^, 1, Length(s)) then
            {если уже перечитали вышележащий каталог, то этот перечитывать не надо}
          begin
            s := PS^;
            GlobalMessage(evCommand, cmRereadTree, PS);
            RereadDirectory(s);
          end;
      end;
    RereadCollection^.DeleteAll;
    Dispose(RereadCollection, Done);
    {/Cat}
    GlobalMessage(evCommand, cmRereadInfo, nil);
    {$IFDEF OS_DOS}
    if Flush then
      begin
        asm
     mov ah, 0dh
     int 21h
    end;
        Dispose(PInfo, Done);
      end;
    {$ENDIF}
    Dec(SkyEnabled);
    NotifyResume; {Cat}
  end { EraseFiles };
{-DataCompBoy-}

procedure SetVLabel;
  {$IFNDEF OS2}
  begin
  end;
  {$ELSE}
  var
    s: String;
    rc: longInt;
  begin
    s := {$IFDEF WIN32}CharToOemStr {$ENDIF}(GetVolumeLabel(
      GetCurDrive)); {Cat}
    if ExecResource(dlgVolumeLabel, s) = cmOK then
      begin
        rc := SetVolume(GetDrive+1, s);
        if rc <> 0 then
          MessageBox(GetString(dl_Failed_to_set_volume_label)+
            ^M^C', (RC=%d)', @rc, mfOKButton);
      end;
  end;
{$ENDIF}

end.
