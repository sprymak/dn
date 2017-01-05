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

UNIT ERASER;

interface

uses
  Objects, {$IFNDEF OS2}LFNCol,{$ENDIF} filediz, Collect;

 procedure EraseFiles(Files: PCollection);
 procedure SetVLabel;
 function  ValidErase(Files: PCollection): Boolean;

implementation
uses Dos, LFN{DataCompBoy}, FilesCol, Commands, advance, advance1, advance2,
     advance3, Startup, Messages, xTime, Drivers, Tree, Memory,
     DNApp, Gauge, Views, RStrings, Dialogs, Drives, FileCopy
     {$IFNDEF NONBP},DiskTool{$ENDIF}
{JO} {$IFDEF OS2}, VPUtils, FlOS2Tl{$ENDIF}

     ;

        {-DataCompBoy-}
function ValidErase;
var
  PF: PFileRec;
  S: string;
  I: Integer;
begin
  ValidErase := False;
  if Files = nil then Exit;
  if Files^.Count = 0 then Exit;
  if Files^.Count = 1 then
   begin PF := Files^.At(0);
         if PF^.Attr and Directory <> 0 then S := GetString(dlEraseConfirmDir)
                                        else S := GetString(dlEraseConfirm1);
{$IFNDEF OS2}
         S := S + '"' + {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(Cut(GetLFN(PF^.LFN), 40))  + '"' + ' ?';
{$ELSE}
         S := S + '"' + Cut(PF^.Name, 40)  + '"' + ' ?';
{$ENDIF}

   end
  else S := GetString(dlEraseConfirms1);
  if (Files^.Count = 1) and (Confirms and cfSingleErase = 0) then I := cmYes else
  I := MessageBox(S,nil,mfConfirmation+mfYesButton+mfNoButton);
  if (I <> cmYes) then Exit;
  if (Files^.Count > 1) and (Confirms and cfMultiErase <> 0) then
   begin
    S := GetString(dlEraseConfirm2)+ItoS(Files^.Count)+' '+GetString(dlDIFiles)+ ' ?';
    if MessageBox(S,nil,mfConfirmation+mfYesButton+mfNoButton) <> cmYes then Exit;
   end;
  ValidErase := True;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure EraseFiles;
  var PInfo: PWhileView;
      R: TRect;
      EraseCancel: Boolean;
      Tmr: TEventTimer;
      I, J: Integer;
      S, Dr : string;
      ch: char;
      F: lFile;
      DOSMode, DirEnd, DirModified: Boolean;
      PF: PFileRec;
      {$IFDEF DiskEdit}
      Drv: PDiskDriver;
      DirCluster, DirSize: Word;
      Dir: ^DirPtr;
      {$ENDIF}
      DrivesSet: Set of Char;
      DeleteAllFiles: Boolean;
      Params: record {AK155}
        FName: PString;
        RC: longint;
        end;

  procedure InfoWrite(N: Integer; const S: Str40);
  begin
    PInfo^.Write(N, S);
  end;

  procedure CalcCancel;
  begin
    if EraseCancel then Exit;
    if TimerExpired(Tmr) then
    begin
      DispatchEvents(PInfo, EraseCancel);
      EraseCancel := (EraseCancel or CtrlBreakHit) and (MessageBox(GetString(dlQueryAbort), nil, mfYesNoConfirm) = cmYes);
      CtrlBreakHit := Off;
      NewTimer(Tmr, 1);
    end;
  end;

  function Over(S: String): Word;
   var I: Integer;
  begin
    if Confirms and cfEraseReadonly = 0 then
      begin Over := cmYes; Exit end;
    I := MessageBox(^C+GetString(dlFile)+' "'+Cut(S, 40)+'"'+GetString(dlEraseRO), nil,mfYesNoConfirm+mfAllButton);
    if I = cmOK then DeleteAllFiles := True;
    Over := I; if I = cmOK then Over := cmYes;
  end;

  {$IFDEF DiskEdit}
  procedure EraseChain(Cl: Word);
   var Next, CCL: Word;
  begin
   if CL < 2 then Exit;
   CCL := Cl;
   repeat
    Next := Drv^.GetFAT(CCL);
    if (CCL >= 2) then Drv^.SetFAT(CCL, 0);
    CCL := Next;
   until (CCL < 2) or (CCL >= Drv^.LastEl);
  end;
  {$ENDIF}

{AK155 При отказе (DOSDelDir=false) Params.RC возвращает код
завершения неудачной операции, если причина отказа - ошибка
операции с файлом или каталогом. При посторонних причинах отказа
(например, Abort) будет Params.RC=0}
  function DOSDelDir: Boolean;
   var
    DC: PDirCol;
    TD: PDirCol;
    SR: lSearchRec;
    s: string;
  Label TryDel;
  begin
   DOSDelDir := false;
   Abort:=false;
   Params.RC := 0;
   new(DC, Init($10, $10, False));
   new(TD, Init($10, $10, False));
   DC^.Insert(NewStr(FreeStr));

   While DC^.Count>0 do
    begin
     FreeStr:=PString(DC^.At(0))^;
     DC^.AtFree(0);
     PInfo^.Write(2, Cut(GetName(FreeStr), 40));

     if Word(Length(FreeStr))+Word(Length(x_x)) > MaxPathLen-1 then
      begin Abort:=true; Break end;

    lFindFirst(MakeNormName(FreeStr, x_x),{$IFDEF OS2}Anyfile{$ELSE}$FF XOR VolumeID{$ENDIF}, SR); {JO}
     while DosError=0 do
      begin
       If Abort then break;
       if (SR.FullName='.') or (SR.FullName='..') then
        begin lFindNext(SR); continue end;
       if (Word(Length(FreeStr))+Word(Length(SR.FullName))) > MaxPathLen-1
        then begin Abort:=true; break; end;
       if (SR.SR.Attr and Directory)<>0 then
        begin
         If MaxAvail<Length(MakeNormName(FreeStr, SR.FullName))+$40
          then begin Abort:=True; Break; end;
         DC^.AtInsert(0, NewStr(MakeNormName(FreeStr, SR.FullName)));
        end
       else
        begin
         if (Word(Length(FreeStr))+Word(Length(SR.FullName))) > MaxPathLen-1
          then begin Abort:=true; break; end;
         ClrIO;
         lAssignFile(F, MakeNormName(FreeStr, SR.FullName));
         lSetFAttr(F, 0);
         lEraseFile(F);
         Params.RC := IOResult;
         if Params.RC<>0 then begin Abort:=true; break; end;
        end;
       lFindNext(SR);
      end;
     lFindClose(SR);
     if not Abort then
      begin
       if MaxAvail<Length(FreeStr)+$40 then begin Abort:=true; break end;
       If FreeStr[Length(FreeStr)]='\'
        then TD^.AtInsert(0, NewStr(Copy(FreeStr,1, length(FreeStr)-1)))
        else TD^.AtInsert(0, NewStr(FreeStr));
      end else Break;
    end;
   DC^.FreeAll;
   Dispose(DC,Done);
   DOSDelDir:=not Abort;
   If not Abort then
    while TD^.Count>0 do
     begin
      ClrIO;
      s := PString(tD^.At(0))^;
TryDel:
      lRmDir(s);
      Params.RC := IOResult;
      if Params.rc <> 0 then
         if SysErrorFunc(Params.rc, byte(s[1]) - byte('A')) = 1 then
           goto TryDel;
      if Params.RC<>0 then
       begin DOSDelDir:=False; FreeStr:=PString(tD^.At(0))^; break; end;
      TD^.AtFree(0);
     end;
   TD^.FreeAll;
   Dispose(TD,Done);
  end;

  {$IFDEF DiskEdit}
  var  nxt,fs : Word;
       w      : String;

  procedure DirDel(cl : Word);
   var DirCl,fcl,tcl,i : Word;
       DirEnd          : Boolean;
   label     1;

  begin
   if cl = 0 then Exit;
   DirCl:=Cl; DirEnd := False;
   1:
    Drv^.ClusterRead(Dir^, DirCL);
    for i:=0 to ((Drv^.SectLen div 32) * Drv^.SectPerClust)-1 do
     begin
      if DirEnd or (Dir^[I].Name[0] = #0) then begin DirEnd := On; Break end;
      if (Dir^[i].Name[0]>#32) and (Dir^[i].Name[0]<>#$E5) and
         (Dir^[i].Name[0]<>'.') then
      begin
       if (Dir^[i].Attr and Directory <> 0) and
          (Dir^[i].Attr and VolumeID = 0) then
          begin
           Drv^.ClusterWrite(Dir^, DirCl);
           fcl:=Dir^[i].Clus; DirDel(FCL);
           Drv^.ClusterRead(Dir^, DirCl);
           EraseChain(FCL);
           Dir^[I].Name[0] := #$E5;
          end
       else
        if (Dir^[i].Attr and VolumeID = 0) then
         begin
          EraseChain(Dir^[i].Clus);
          Dir^[I].Name[0] := #$E5;
         end;
      end;
     end;
    Drv^.ClusterWrite(Dir^, DirCL);
    CL := Drv^.GetFAT(DirCL);
    if not DirEnd and (CL >= 2) and (cl <= Drv^.TotalClu + 1) then
     begin DirCL := CL; GoTo 1; end;
   end;
   {$ENDIF}

  function DeleteDirectory(Dir: String; Cluster: Word): Boolean;
   var S: String;
       dr: Word; j123qwe:boolean;
       SR: lSearchRec;
  begin
    DeleteDirectory := Off;
    Dir:=lFExpand(Dir);
    if Dir[Length(Dir)]='.' then SetLength(Dir, Length(Dir)-1);
    if not DeleteAllFiles then
    begin
       S := MakeNormName(Dir, x_x);
       DosError:=0;
       lFindFirst(S ,{$IFDEF OS2}Anyfile{$ELSE}$FF XOR VolumeID{$ENDIF}, SR);  {JO}
       if IsDummyDir({$IFDEF OS_DOS}SR.SR.Name{$ELSE}SR.Fullname{$ENDIF}) then lFindNext(SR);
       if IsDummyDir({$IFDEF OS_DOS}SR.SR.Name{$ELSE}SR.Fullname{$ENDIF}) then lFindNext(SR);
       lFindClose(SR);
       if Abort then Exit;
       dr := cmOK;
       if (DOSError = 0) and (not DeleteAllFiles) then
        begin
         S:=Dir;
         Dec(SkyEnabled);
         if Confirms and cfEraseSubDir = 0 then dr := cmYes else
         dr:=MessageBox(^C+GetString(dlDirectory)+' '+Cut(S, 40)+GetString(dlEraseDirNotEmpty),
                        nil, mfConfirmation + mfNoButton + mfAllButton+ mf2YesButton + mfCancelButton);
         Inc(SkyEnabled);
         DeleteAllFiles := DR = cmOK;
         Abort:= dr = cmCancel;
        end;
       if not (dr in [cmYes, cmOK]) then Exit;
    end;
    {$IFDEF DiskEdit}
    if DOSMode then
       begin
    {$ENDIF}
         FreeStr:=Dir;
         j123qwe:=not DOSDelDir;
         if j123qwe then
           begin
              FreeStr := Cut(FreeStr,40);
              Params.FName := @FreeStr;
              S := GetString(dlEraseCantDelDir);
              {$IFDEF SHOWRC}
              if Params.RC <> 0 then
                S := S + ^M^C'(RC=%d)';
              {$ENDIF}
              MessageBox(S, @Params, mfError+mfOKButton);
              Abort := On;
           end;
         PInfo^.DrawView;
       {$IFDEF DiskEdit}
       end else begin DirDel(Cluster); Drv^.WriteFAT; end;
       {$ENDIF}
    CreateDirectory(Dir, On);
    DeleteDirectory := On;
    {$IFDEF DiskEdit}
    if not DosMode then
    begin
      asm
       mov ah, 0dh
       int 21h
      end;
      Drv^.ReadFAT;
    end;
    {$ENDIF}
  end;

  var Fls: PCollection;
    {$IFDEF OS_DOS}
      Flush: Boolean;
    {$ENDIF}
    {$IFDEF VIRTUALPASCAL}
      Ask: Integer;
    {$ENDIF}
  label LLL;

begin
  if not ValidErase(Files) then Exit;
  DeleteAllFiles := Off; Abort := Off; ClrIO;
  CtrlBreakHit := Off; {JO}
  R.Assign(1,1,26,9);
  New(PInfo, Init(R));
  PInfo^.Top := GetString(dlErase);
  {$IFDEF DiskEdit}
  DOSMode := LFNPresent; {DataCompBoy}
  {$ELSE}
  DOSMode := True;
  {$ENDIF}

  {$IFDEF DiskEdit}
  if not DOSMode then
   begin
    PF := Files^.At(0); S := PF^.Owner^;
    for I := 1 to Files^.Count-1 do
      if PFileRec(Files^.At(I))^.Owner^ <> S then
       begin DOSMode := On; Break end;
   end;
  {$ENDIF}

  EraseCancel := Off; Abort := Off;
  NewTimer(Tmr, 0); CalcCancel;

  {$IFDEF DiskEdit}
  if not DOSMode then
   begin
    if (Length(S) > 3) and (S[Length(S)] = '\') then SetLength(S, Length(S)-1);
    New(Drv, Init(Byte(S[1])-64, On));
    if Drv = nil then begin Abort := Off; DOSMode := On; end
     else
      begin
       DirCluster := Drv^.GetPath(S);
       DirSize := Max(Drv^.ClusterSize(DirCluster), Drv^.ClusterSize(2));
       if DirCluster = $FFFF then begin Dispose(Drv,Done); DOSMode := On; end
        else
         begin
          Drv^.FreeFAT; Dir := MemAlloc(DirSize);
          Drv^.SeekFAT(0);
          if (Drv^.FAT = nil) or Abort then
           begin Abort := Off; FreeMem(Dir, DirSize);
                 Dispose(Drv,Done); DOSMode := On; end else
           begin
            Drv^.ClusterRead(Dir^, DirCluster);
            if Abort then begin Abort := Off; FreeMem(Dir, DirSize);
                                Dispose(Drv,Done); DOSMode := On; end
           end;
         end;
    end;
   end;
  {$ENDIF}
  if Abort then begin Dispose(PInfo,Done); Exit end;
  Inc(SkyEnabled);
  Desktop^.Insert(PInfo);

  {$IFDEF DiskEdit}
  if DOSMode then
   begin
  {$ENDIF}
    for I := 1 to Files^.Count do
     begin
      PF := Files^.At(I-1);
{$IFNDEF OS2}
      S := GetLFN(PF^.LFN);
{$ELSE}
      S := PF^.Name;
{$ENDIF}
      CalcCancel;
      if (PF <> nil) and not EraseCancel and not Abort then
       if (PF^.Attr and Directory = 0) then
       begin
        InfoWrite(1,GetString(dlErasingFile));
        InfoWrite(2,Cut(S,40));
        J := cmYes;
        if not DeleteAllFiles and (PF^.Attr and ReadOnly <> 0) then J := Over(S);
        EraseCancel := EraseCancel or (J = cmCancel);
        if J = cmYes then
         begin
               S := MakeNormName(PF^.Owner^, S);
               lAssignFile(F, S); ClrIO;
               if PF^.Attr and ReadOnly <> 0 then
               begin
LLL:             lSetFAttr(F, Archive); ClrIO; lEraseFile(F);
               end else
               begin
                 lEraseFile(F); if IOResult <> 0 then goto LLL;
               end;
               if IOResult <> 0 then
                 begin
                   S := lFExpand(S);
                   GlobalMessage(evBroadcast, cmReleaseFile, @S);
                   ClrIO; lEraseFile(F);
                 end;
               Ask := IOResult;
              {if IOResult = 0 then}
               if Ask = 0 then
               begin
                 if (PF^.DIZ <> nil) and
                    (FMSetup.Options and fmoPreserveDesc = 0) then
                 begin
                   InfoWrite(1,GetString(dlDeletingDIZ));
                   {$IFNDEF OS2}
                   DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), MakeFileName(PF^.Name));
                   DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), GetLFN(PF^.LFN));
                   {$ELSE}
                   DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), PF^.Name);
                   {$ENDIF}
                 end;
               end else
                   {$IFDEF VIRTUALPASCAL}
                    begin
                     if Ask in [2,3,110] then
                   {$ENDIF}
                       MessageBox(GetString(dlErasingNoFile)+S {+ '; RC = %d'}, nil{@Ask}, mfError+mfOKButton)
                   {$IFDEF VIRTUALPASCAL}
                     else
                       begin
                         Ask := SysErrorFunc(Ask, GetDrive);
                         if (Ask = 1) then goto LLL;
                       end;
                    end
                   {$ENDIF}
                    ;
         end;
        end else begin
                       InfoWrite(1,GetString(dlErasingDir));
                       InfoWrite(2,Cut(S,40));
                       {$IFNDEF OS2}
                       DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), MakeFileName(PF^.Name));
                       DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), GetLFN(PF^.LFN));
                       {$ELSE}
                       DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), PF^.Name);
                       {$ENDIF}
                       {$IFDEF OS_DOS}
                       S := lfGetLongFileName(MakeNormName(PF^.Owner^, S));
                       {$ELSE}
                       S := MakeNormName(PF^.Owner^, S);
                       {$ENDIF}
                       DeleteDirectory(S,0);
                 end
       else Break;
     end;
  {$IFDEF DiskEdit}
  end else
  begin
   DirEnd := Off;
   while (Files^.Count > 0) and not DirEnd and not Abort do
    begin
     DirModified := Off;
     for I := 0 to (DirSize div 32) - 1 do
      begin
       if Abort then Break;
       if (Dir^[I].Name[0] < #32) then
        begin DirEnd := On; break; end;
       for J := 1 to Files^.Count do
        begin
         if Abort then Break;
         PF := Files^.At(J-1); S := UpStrg(PF^.Name); Delete(S, 9, 1);
         if StrEq(S[1], Dir^[I].Name, 11) then
          begin
           {Files^.AtDelete(J-1);}
          {$IFNDEF OS2} {на самом деле под многозадачками не актуально}
           S := MakeFileName(PF^.Name);
          {$ELSE}
           S := PF^.Name;
          {$ENDIF}
           if Dir^[I].Attr and Directory = 0 then
            begin
             J := cmYes;
             if not DeleteAllFiles and (Dir^[I].Attr and ReadOnly <> 0) then J := Over(S);
             if J = cmYes then
              begin
               InfoWrite(1,GetString(dlErasingFile));
               InfoWrite(2,Cut(S,40));
               EraseChain(Dir^[I].Clus);
               Dir^[I].Name[0] := #$E5;
               DirModified := On;
              end;
            end else
             begin
               InfoWrite(1, GetString(dlErasingDir));
               PInfo^.Write(2, Cut(S,40));
               S := MakeNormName(PF^.Owner^, S);
               Drv^.ClusterWrite(Dir^, DirCluster);
               if DeleteDirectory(S, Dir^[I].Clus) and not Abort then
                begin
                 Drv^.ClusterRead(Dir^, DirCluster);
                 EraseChain(Dir^[I].Clus);
                 Dir^[I].Name[0] := #$E5;
                 Drv^.ClusterWrite(Dir^, DirCluster);
                end else Drv^.ClusterRead(Dir^, DirCluster);
             end;
           Break;
          end;
        end;
      end;
      if DirModified and not Abort then Drv^.ClusterWrite(Dir^, DirCluster);
      DirEnd := (DirCluster = 0) or DirEnd or Abort;
      if not DirEnd then
       begin
        DirCluster := Drv^.GetFAT(DirCluster);
        if (DirCluster >= Drv^.LastEl)
         then DirEnd := On
         else Drv^.ClusterRead(Dir^, DirCluster);
       end;
    end;
   Dispose(Drv,Done);
   FreeMem(Dir, DirSize);
  end;
  {$ENDIF}
  DrivesSet := [];
{$IFDEF OS_DOS}
  Flush := ((SystemData.Options{$IFDEF VIRTUALPASCAL}shl 3{$ENDIF}) and ossFlushDsk <> 0);

  if Flush then
  begin
    PInfo^.ClearInterior;
    InfoWrite(1, GetString(dlFlushingBuffers));
  end else
{$ENDIF}
           Dispose(PInfo,Done);

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
  GlobalMessage(evCommand, cmRereadInfo, nil);
{$IFDEF OS_DOS}
  if Flush then
  begin
    asm
     mov ah, 0dh
     int 21h
    end;
    Dispose(PInfo,Done);
  end;
{$ENDIF}
  Dec(SkyEnabled);
end;
        {-DataCompBoy-}

procedure SetVLabel;
{$IFNDEF OS2}
  {$IFDEF DiskEdit}
 var Drv: PDiskDriver;
     S, S1: String;
     SR: SearchRec;
     D: PDialog;
     P: PView;
     R: TRect;
     W,I: Word;
     L: Byte Absolute S;
begin
  Abort := Off; DOSError := 0; ClrIO;
  FindFirst('\*.*', VolumeID, SR);
  if Abort then Exit;
  lGetDir(0, S1); {GetDir(0, S1);} {Cat}
  if Abort then Exit;
  if DOSError = 0 then S := SR.Name else S := '';
  If ExecResource( dlgVolumeLabel, S ) = cmOK then begin
    New(Drv, Init(Byte(S1[1])-64, False));
    if Drv = nil then Exit;
    Drv^.SetVLabel(S);
    Dispose(Drv,Done);
    GlobalMessage(evCommand, cmRereadInfo, nil);
  end
  {$ELSE}
begin
  {$ENDIF}
end;
{$ELSE}
 Var
    S: String;
    rc: Longint;
begin
  S := {$IFDEF WIN32}CharToOemStr{$ENDIF}(GetVolumeLabel(GetCurDrive)); {Cat}
  if ExecResource(dlgVolumeLabel, S ) = cmOK then
  begin
    rc :=  SetVolume(GetDrive+1, S);
    if rc <> 0 then messagebox(GetString(dl_Failed_to_set_volume_label)+{$IFDEF SHOWRC}^M^C', (RC=%d)'{$ENDIF}, @rc, mfOKButton);
  end;
end;
{$ENDIF}

END.
