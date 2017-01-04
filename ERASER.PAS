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
{$I STDEFINE.INC}

UNIT ERASER;

interface
uses Objects, LFNCol, filediz, Collect;

 procedure EraseFiles(Files: PCollection);
 procedure SetVLabel;
 function  ValidErase(Files: PCollection): Boolean;

implementation
uses Dos, LFN{DataCompBoy}, FilesCol, Commands, advance, advance1, advance2,
     advance3, Startup, DiskTool, Messages, xTime, Drivers, Tree, Memory,
     DNApp, Gauge, Views, RStrings, Dialogs, Drives, FileCopy;

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
         S := S + '"' + Cut(GetLFN(PF^.LFN), 40)  + '"' + ' ?';
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
      Drv: PDiskDriver;
      DirCluster, DirSize, EndM: Word;
      Dir: ^DirPtr;
      DrivesSet: Set of Char;
      DeleteAllFiles: Boolean;

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

  procedure EraseChain(Cl: Word);
   var Next, CCL: Word;
  begin
   if CL < 2 then Exit;
   CCL := Cl; if Drv^.FAT12 then ENDM := $FF0 else ENDM := $FFF0;
   repeat
    Next := Drv^.GetFAT(CCL);
    if (CCL >= 2) then Drv^.SetFAT(CCL, 0);
    CCL := Next;
   until (CCL < 2) or (CCL >= ENDM);
  end;

  function DOSDelDir: Boolean;
   var
    DC: PDirCol;
    TD: PDirCol;
    SR: lSearchRec;
  begin
   DOSDelDir := false;
   Abort:=false;
   new(DC, Init($10, $10));
   new(TD, Init($10, $10));
   DC^.Insert(NewStr(FreeStr));

   While DC^.Count>0 do
    begin
     FreeStr:=PString(DC^.At(0))^;
     DC^.AtFree(0);
     PInfo^.Write(2, Cut(GetName(FreeStr), 40));

     if Word(Length(FreeStr))+Word(Length(x_x)) > MaxPathLen-1 then
      begin Abort:=true; Break end;

     lFindFirst(MakeNormName(FreeStr, x_x), $FF xor VolumeID, SR);
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
         if IOResult<>0 then begin Abort:=true; break; end;
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
      lRmDir(PString(tD^.At(0))^);
      if IOResult<>0 then
       begin DOSDelDir:=False; FreeStr:=PString(tD^.At(0))^; break; end;
      TD^.AtFree(0);
     end;
   TD^.FreeAll;
   Dispose(TD,Done);
  end;

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

  function DeleteDirectory(Dir: String; Cluster: Word): Boolean;
   var S: String;
       dr: Word; j123qwe:boolean;
       SR: lSearchRec;
  begin
    DeleteDirectory := Off;
    Dir:=lFExpand(Dir);
    if Dir[Length(Dir)]='.' then Dec(Dir[0]);
    if not DeleteAllFiles then
    begin
       S := MakeNormName(Dir, x_x);
       DosError:=0;
       lFindFirst(S ,$FF xor VolumeID, SR);
       if (SR.SR.Name[1] = '.') then lFindNext(SR);
       if (SR.SR.Name[1] = '.') then lFindNext(SR);
       if Abort then begin lFindClose(SR); Exit; end;
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
    if DOSMode then
       begin
         FreeStr:=Dir;
         j123qwe:=not DOSDelDir;
         if j123qwe then
           begin
              MessageBox(GetString(dlEraseCantDelDir)+Cut(FreeStr,40), nil, mfError+mfOKButton);
              Abort := On;
           end;
         PInfo^.DrawView;
       end else begin DirDel(Cluster); Drv^.WriteFAT; end;
    CreateDirectory(Dir, On);
    DeleteDirectory := On;
    if not DosMode then
    begin
      asm
       mov ah, 0dh
       int 21h
      end;
      Drv^.ReadFAT;
    end;
  end;

  var Fls: PCollection;
      Flush: Boolean;
  label LLL;

begin
  if not ValidErase(Files) then Exit;
  DeleteAllFiles := Off; Abort := Off; ClrIO;

  R.Assign(1,1,26,9);
  New(PInfo, Init(R));
  PInfo^.Top := GetString(dlErase);
  DOSMode := LFNPresent; {DataCompBoy}

  if not DOSMode then
   begin
    PF := Files^.At(0); S := PF^.Owner^;
    for I := 1 to Files^.Count-1 do
      if PFileRec(Files^.At(I))^.Owner^ <> S then
       begin DOSMode := On; Break end;
   end;

  EraseCancel := Off; Abort := Off;
  NewTimer(Tmr, 0); CalcCancel;

  if not DOSMode then
   begin
    if (S[0] > #3) and (S[Length(S)] = '\') then Dec(S[0]);
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

  if Abort then begin Dispose(PInfo,Done); Exit end;
  Inc(SkyEnabled);
  Desktop^.Insert(PInfo);

  if DOSMode then
   begin
    for I := 1 to Files^.Count do
     begin
      PF := Files^.At(I-1);
      S := GetLFN(PF^.LFN);
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
               if IOResult = 0 then
               begin
                 if (PF^.DIZ <> nil) and
                    (FMSetup.Options and fmoPreserveDesc = 0) then
                 begin
                   InfoWrite(1,GetString(dlDeletingDIZ));
                   DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), MakeFileName(PF^.Name));
                   DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), GetLFN(PF^.LFN));
                 end;
               end else MessageBox(GetString(dlErasingNoFile)+S, nil, mfError+mfOKButton);
         end;
        end else begin
                       InfoWrite(1,GetString(dlErasingDir));
                       InfoWrite(2,Cut(S,40));
                       DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), MakeFileName(PF^.Name));
                       DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), GetLFN(PF^.LFN));
                       S := lfGetLongFileName(MakeNormName(PF^.Owner^, S));
                       DeleteDirectory(S,0);
                 end
       else Break;
     end;
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
         PF := Files^.At(J-1); S := UpStrg(MakeFileName(PF^.Name)); Delete(S, 9, 1);
{$IFNDEF FPC}
         if StrEq(S[1], Dir^[I].Name, 11) then
{$ELSE}
         if S=Dir^[I].Name then
{$ENDIF}
          begin
           {Files^.AtDelete(J-1);}
           S := MakeFileName(PF^.Name);
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
      if Drv^.FAT12 then ENDM := $FF0 else ENDM := $FFF0;
      DirEnd := (DirCluster = 0) or DirEnd or Abort;
      if not DirEnd then
       begin
        DirCluster := Drv^.GetFAT(DirCluster);
        if (DirCluster >= EndM) then DirEnd := On else Drv^.ClusterRead(Dir^, DirCluster);
       end;
    end;
   Dispose(Drv,Done);
   FreeMem(Dir, DirSize);
  end;

  DrivesSet := [];

  Flush := (SystemData.Options and ossFlushDsk <> 0);

  if Flush then
  begin
    PInfo^.ClearInterior;
    InfoWrite(1, GetString(dlFlushingBuffers));
  end else Dispose(PInfo,Done);

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
  if Flush then
  begin
    asm
     mov ah, 0dh
     int 21h
    end;
    Dispose(PInfo,Done);
  end;
  Dec(SkyEnabled);
end;
        {-DataCompBoy-}

procedure SetVLabel;
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
  GetDir(0, S1);
  if Abort then Exit;
  if DOSError = 0 then S := SR.Name else S := '';
  If ExecResource( dlgVolumeLabel, S ) = cmOK then begin
    New(Drv, Init(Byte(S1[1])-64, False));
    if Drv = nil then Exit;
    Drv^.SetVLabel(S);
    Dispose(Drv,Done);
    GlobalMessage(evCommand, cmRereadInfo, nil);
  end
end;

END.
