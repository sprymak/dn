{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  1.6.RC1
//  dn16rc1-Archives_without_extension_fix_diff131byMV.patch
//  dn16rc1-move_COMBINE_MIXED_from_sources_to_Options_menu_diff140byMV.patch
//  dn16rc1-deleting_temporary_files_safety_fix.patch
//  dn16rc1-Archivers_Optimization-diff154byMV.patch
//  dn16rc1-Archive_total_size_2GB_fix-diff155byMV.patch
//
//  2.0.0
//  dn200-delete_file_from_temp_fix-diff161byMV.patch
//
//  2.3.0
//  dn230-files_panel_bugs_fix_1.patch
//  dn230-remove_GZip_compression_and_change_external_filter_view.patch
//
//  2.7.0
//  dn270-search_in_archives.patch
//  dn270-password_with_spaces_warning.patch
//  dn270-nameless_files_are_extension.patch
//  dn270-view_file_with_2_spaces_in_filename_from_archive.patch
//  dn2922-show_size_before_name_if_it_longer_250.patch
//  dn270-arc_in_arc_fix.patch
//  dn21029-highlight_250_fix-jo21104a.patch
//  dn21116-Ctrl_Slash_Up_Down_fix.patch
//  dn21201-copy_arcpanel_to_temppanel.patch
//  dn21128-archive_name_depends_on_Use_LFN-jo20604a.patch
//  dn21202-Find_file_and_branch_in_archive.patch
//  dn21225-fpanel(if)-copy_files_from_archives_in_find_panel.patch
//  dn21225-FilePanel(i)-change_SFN_look_in_archives.patch
//  dn328-FilePanel(f)_jo30224a_crash_fix.patch
//  dn3216-archives(f)-unselect_files_only_when_needed.patch
//  dn3216-fplanel(i)-copy_to_temp_with_dialogs.patch
//  dn368-find_LFN_inside_of_archive.patch
//
//  3.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

Unit ArcView;

Interface uses Dos, LFN, LFNCol, tree, Objects, Views, Drivers, FViewer,
               FlPanel, FilesCol, DiskInfo, DblWnd, HistList, HideView,
               RStrings, Drives, Commands, Archiver, FStorage, Histries,
               advance, advance1, advance2, advance3, archdet, collect
               ;
type
    PArcDrive = ^TArcDrive;
    TArcDrive = object(TDrive)
     ArcName:     String; {DataCompBoy}
     VArcName:    String; {JO's idea moved from DN/2 by piwamoto}
     AType:       PARJArchive;
     Files:       PDirStorage;
     KillAfterUse:Boolean;
     ArcDate:     LongInt;
     ForceRescan: Boolean;
     Password:    String;
     constructor Init(const AName, VAName: String; viewmode: byte);
     constructor InitCol(PC: PDirStorage; const AName, VAName: String);
     constructor Load(var S: TStream);
     procedure   Store(var S: TStream); virtual;
     procedure   RereadDirectory(S: String); virtual; {DataCompBoy}
     procedure   KillUse; virtual;
     function    ReadArchive: Boolean;
     procedure   lChDir(ADir: String); virtual; {DataCompBoy}
     function    GetDir: String; virtual;
     function    GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String;
                              var FreeSpace, TotalInfo: String ): PCollection; virtual;
     function    Exec(Prg, Cmd: String): Boolean;
     procedure   UseFile(P: PFileRec; Command: Word); virtual; {DataCompBoy}
     function    MakeListFile(PC: PCollection): String;
     procedure   CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
     procedure   CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
     procedure   EraseFiles(AFiles: PCollection); virtual;
     procedure   GetDown(var B; C: Word; P: PFileRec); virtual;
     procedure   GetFull(var B; P:PFileRec; C, SC:Word); virtual;{DataCompBoy}
     procedure   GetEmpty(var B; SC: Word); virtual;
     function    CalcLengthWithoutName: Integer; virtual;
     function    CalcLength: Integer; virtual;
     function    GetRealName: String; virtual;
     procedure   MakeTop(var S: String); virtual;
     procedure   HandleCommand(Command: Word; InfoPtr: Pointer); virtual;
     procedure   MakeDir; virtual;
     function    isUp: Boolean; virtual;
     procedure   ChangeUp(var S: String); virtual;
     procedure   ChangeRoot; virtual;
     procedure   ExtractFiles(AFiles: PCollection; ExtrDir: String; Own: PView; Options: Byte); {DataCompBoy}
     procedure   GetFreeSpace(var S: String); virtual;
     procedure   GetDirInfo(var B: TDiskInfoRec); virtual;
     function    GetFullFlags: Word; virtual;
     procedure   GetDirLength(PF: PFileRec); virtual; {DataCompBoy}
     procedure   GetParam(n: byte); virtual; {DataCompBoy}
     destructor  Done; virtual;
     procedure   StdMsg4;
     function    OpenDirectory(const Dir: String): PDrive; virtual;
     procedure   FindFile;
    end;

    function ArcViewer(AName, VAName : String; num: byte): Boolean; {DataCompBoy}
    procedure StdMsg(MsgNo: Byte);

const
    ArcPasw    : String[32] = '';

type
    PFInfo     = ^TFInfo;
    TFInfo     = Record
                  FName   : String;
                  USize   : LongInt;
                  PSize   : LongInt;
                  Date    : LongInt;
                  Attr    : Byte;
                  Last    : Byte;
                  { 0 - not last    }
                  { 1 - archive end }
                  { 2 - broken arc  }
                 end;

const
  Uncompleted: Boolean = False; {JO}
  StopPoint:   Integer = 0; {JO}

Implementation
 uses Menus, DnApp, Messages, Dialogs, Gauge, FileCopy, Memory, Startup,
      Arvid, xTime, VideoMan, Filefind
      ;

const
  LowMemSize = $4000;  {Local setting}

function MaxAvail: LongInt;
begin
  MaxAvail := MemAdjust(System.MaxAvail);
end;

Procedure StdMsg(MsgNo: Byte);
var
  si: TStrIdx;
begin
 Application^.Redraw;
 case MsgNo of
  4 : MessageBox(GetString(dlArcMsg4)+''''+Cut(ArcFileName,40)+'''',NIL,mfOkButton or mfError);
  5 : Msg(dlArcMsg5,NIL,mfOkButton or mfInformation);
  1, 6, 7:
   begin
     case MsgNo of
       1: si := dlArcMsg1;
       6: si := dlArcMsg6;
       7: si := dlArcMsg7;
     end;
     ErrMsg(si);
   end;
 end;
end;

procedure TArcDrive.StdMsg4;
begin
  if TempFile <> '' then TempFile := ''; StdMsg(4); Abort := On;
end;

        {-DataCompBoy-}
procedure TArcDrive.GetParam;
begin
 Param:=n;
 with ColumnsDefaultsArch do
  begin
   Flags := Params[n].Param;
   LFNLen:=StoI(Params[n].LFNLen);
   if (LFNLen = 0) or (LFNLen > 252) then LFNLen := 252;
   EXTLen:=StoI(Params[n].EXTLen);
   if EXTLen > 252 then EXTLen := 252;
   case FileCase of
    0: FilFLP := 0;
    1: FilFLP := flnLowCase;
    2: FilFLP := flnCapitalCase;
    3: FilFLP := flnUpCase;
    4: FilFLP := 65535;
    5: FilFLP := 65534;
   end;
   case DirsCase of
    0: DirFLP := 0;
    1: DirFLP := flnLowCase;
    2: DirFLP := flnCapitalCase;
    3: DirFLP := flnUpCase;
    4: DirFLP := 65535;
    5: DirFLP := 65534;
   end;
  end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
constructor TArcDrive.Init;
 var SR: lSearchRec;
     I: Integer;
     xt,Q: String;
begin
 TObject.Init;
 if viewmode<1 then GetParam(1)
               else GetParam(viewmode);
 innum:=viewmode;

 VArcName := lFExpand(VAName);

 I:=PosChar(':', Copy(AName,3,255))+2;
 If I>2 then begin
  Q := Copy(AName, I+1, 255);
  if Q[Length(Q)] in ['\','/'] then Dec(Q[0]);
  ArcName := lFExpand(Copy(AName,1,I-1));
 end else begin
  ArcName := lFExpand(AName);
  Q:='\';
 end;

 xt:=GetExt(ArcName);
 If ((xt='') or (xt='.')) and (ArcName[length(ArcName)]<>'.')
  then ArcName:=ArcName+'.';
 lFindFirst(ArcName, AnyFile, SR); {JO}
 if DosError <> 0 then
 begin
   ArcFileName := ArcName;
   VArcFileName := VArcName;
   StdMsg4;
   lFindClose(SR);
   Fail
 end;
 lFindClose(SR);
 ArcDate := SR.SR.Time;
 DriveType := dtArc;
 if not ReadArchive or (Files = nil) then begin Done; Fail; end;
 KillAfterUse := TempFile <> '';
 TempFile := '';
 Password := '';
 lChDir(Q);
 AddToDirectoryHistory(ArcName+':'+CurDir, integer(DriveType));
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
constructor TArcDrive.InitCol;
 var SR: lSearchRec;
begin
 TObject.Init;
 GetParam(1);
 ArcName := lFExpand(AName);
 lFindFirst(ArcName, AnyFile, SR); {JO}
 ArcDate := SR.SR.Time;
 lFindClose(SR);
 DriveType := dtArc;
 Files := PC;
 if (Files = nil) then Fail;
 KillAfterUse := TempFile <> '';
 TempFile := '';
 Password := '';
 if ExistFile(ArcName) then
  New(ArcFile,Init(ArcName, stOpenRead, 512))
 else ArcFile:=nil;
 ArcFileName := ArcName;
 VArcFileName := VArcName;
 if (ArcFile=nil) or (ArcFile^.Status <> stOK) then begin
   StdMsg(4);
   Dispose(ArcFile, Done); ArcFile:=nil;
   if Files <> nil then begin Dispose(Files, Done); Files:=nil end;
   Fail;
 end;
 ArcPos := 0;
 SkipSFX;
 AType := DetectArchive;
 FreeObject(ArcFile);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
constructor TArcDrive.Load;
 var
      SR: lSearchRec;
label
  Failure;
begin
 inherited Load(S);
 S.Read(ArcName[0],1);S.Read(ArcName[1],Length(ArcName));
 S.Read(VArcName[0],1);S.Read(VArcName[1],Length(VArcName));
 S.Read(KillAfterUse, 1);
 S.Read(Password[0],1);S.Read(Password[1],Length(Password));
 Files := PDirStorage(S.Get);
 ForceRescan := Off;
 DriveType := dtArc;
 ArcFileName := ArcName;
 VArcFileName := VArcName;
 lFindFirst(ArcName, AnyFile, SR); {JO}
 if DosError <> 0 then goto Failure;
 lFindClose(SR);
 ArcDate := SR.SR.Time;
 New(ArcFile,Init(ArcName, stOpenRead, 512));
 if (ArcFile=nil) or (ArcFile^.Status <> stOK) then begin
   Dispose(ArcFile, Done); ArcFile:=nil;
Failure:
   lFindClose(SR);
   StdMsg(4);
   if Files <> nil then Dispose(Files, Done); Files:=nil;
   S.Read(ForceRescan, 1);
   Drives.DriveLoadingError:=True;
   Fail;
 end;
 ArcPos := 0;
 SkipSFX;
 AType := DetectArchive;
 FreeObject(ArcFile);
 if AType = nil then goto Failure;
 S.Read(ForceRescan, 1);
end;
        {-DataCompBoy-}

procedure TArcDrive.KillUse;
begin
 if Prev <> nil then Prev^.KillUse;
 if KillAfterUse then EraseTempFile(ArcName);
end;

procedure TArcDrive.Store;
begin
 inherited Store(S);
 S.Write(ArcName[0],1 + Length(ArcName));
 S.Write(VArcName[0],1 + Length(VArcName));
 S.Write(KillAfterUse, 1);
 S.Write(Password[0],1 + Length(Password));
 S.Put(Files);
 S.Write(ForceRescan, 1);
end;

destructor TArcDrive.Done;
begin
 if Files <> nil then Dispose(Files, Done); Files:=nil;
 If AType <> nil then Dispose(AType, Done); AType:=nil;
 inherited Done;
end;

        {-DataCompBoy-}
Function TArcDrive.ReadArchive;
var PF : PArcFile;
    P  : PWhileView;
    R  : TRect;
    Ln : LongInt;
    Cancel: Boolean;
    T: TEventTimer;
    olf: String;
begin
 ReadArchive:=Off;
 New(ArcFile,Init(ArcName, stOpenRead, 512));
 ArcFileName := ArcName;
 VArcFileName := VArcName;
 if (ArcFile=nil) or (ArcFile^.Status <> stOK) then
  begin
    Dispose(ArcFile, Done); ArcFile:=nil;
    StdMsg4;
    Exit;
  end;
 if Files <> nil then Dispose(Files, Done);
 Files := nil;
 ArcPos := 0;
 SkipSFX;
 If AType <> nil then Dispose(AType, Done); AType:=nil; {DataCompBoy}
 AType := DetectArchive;
 if AType = nil then begin FreeObject(ArcFile); Exit; end;
 New(Files, Init);
 if Files = nil then Exit; P := nil;
 R.Assign(1,1,30,10);
 {P := WriteMsg(GetString(dlArcReadArc));}
 LN := ArcFile^.GetSize + 1;
 Cancel := Off; PReader := nil;
 Inc(SkyEnabled);
 NewTimer(T, 3);
 if StartupData.Slice2 and osuReleaseArch = 0 then Application^.BFSpeed;
 Repeat
  if TimerExpired(T) then
    begin
      if P = nil then
        begin
          New(P, Init(R));
          PReader := P;
          P^.Top := GetString(dlArcReadArc);
          P^.Write(1, GetString(dlPercentComplete));
          Desktop^.Insert(P);
        end;
      P^.Write(2, Copy(Strg(#219,25 div (LN div (ArcFile^.GetPos+1)))+Strg(#177, 25), 1, 25));
      P^.Write(3, ItoS(Files^.Files)+GetString(dlFilesFound));
      NewTimer(T, 3);
    end;
  AType^.GetFile;
  if FileInfo.Last=0 then
     begin
      Replace('/', '\', FileInfo.FName);
{piwamoto: it willn't work due to bugs in FName processing in flpanelx
      if (FileInfo.FName[1] = '.') and (FileInfo.FName[2] = '\') then
        begin
(*         System.Delete(FileInfo.FName,1,2); *)
         Files^.AddFile('\.\..', '\.\..', 0, 0, FileInfo.Date, 0);
        end;
}
      if FileInfo.FName[1] <> '\' then FileInfo.FName := '\' + FileInfo.FName;
      if FileInfo.Attr and Directory <> 0 then FileInfo.FName := FileInfo.FName + '\';
      olf := FileInfo.FName;
      Replace('\.','\',FileInfo.FName);
      UpStr(FileInfo.FName);
{attribute "Hidden" means "with password"}
      Files^.AddFile(olf,  FileInfo.FName, FileInfo.USize,
                     FileInfo.PSize,FileInfo.Date,  FileInfo.Attr);
      if (FileInfo.Attr and Directory<>0) or (olf[length(olf)]='\') then
       Files^.AddFile(MakeNormName(olf,'..'),  MakeNormName(FileInfo.FName, '..'),
                      FileInfo.USize,FileInfo.PSize,FileInfo.Date, 0);
      if (P <> nil) and  TimerExpired(T) then
        begin
          DispatchEvents(P, Cancel);
          if Cancel then begin StdMsg(5);FileInfo.Last:=1; end;
        end;
     end;
 until (FileInfo.Last>0) or LowMemory;
 if StartupData.Slice2 and osuReleaseArch = 0 then Application^.EFSpeed;
 Dec(SkyEnabled);
 if P <> nil then P^.Free;
 FreeObject(ArcFile);
 CDir := '';
 if FileInfo.Last=2 then StdMsg(6);
 ReadArchive:=On;
 if Files^.Files=0 then begin StdMsg(7);ReadArchive:=Off;end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.lChDir;
 var Dr: String;
     Nm: String;
     Xt: String;
begin
 if ADir = #0 then Exit;
 While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do Dec(CurDir[0]);
 if ADir[1] = '.' then
  begin
   if CurDir <> '' then While (CurDir <> '') and (not(CurDir[Length(CurDir)] in ['\','/'])) do Dec(CurDir[0])
                   else Advance2.lChDir(GetPath(ArcName));
   Exit;
  end;
 lFSplit(ADir, Dr, Nm, Xt);
 if Xt = '..' then
  begin
   CurDir := Dr;
   while (CurDir <> '') and not (CurDir[Length(CurDir)] in ['\','/']) do
    Dec(CurDir[0]);
  end else CurDir := ADir;
 While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do Dec(CurDir[0]);
 If CurDir='' then CurDir:='\';
 If CurDir[1]<>'\' then CurDir:='\'+CurDir;
 AddToDirectoryHistory(ArcName+':'+CurDir, integer(DriveType));
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TArcDrive.GetDir;
 var Dr: String;
     Nm: String;
     Xt: String;
begin
 While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do Dec(CurDir[0]);
 if (Length(CurDir)>0) and (not (CurDir[1] in ['\','/'])) then CurDir := '\' + CurDir;
 lFSplit(ArcName, Dr, Nm, Xt);
 GetDir := AType^.GetSign+Nm+Xt+CurDir;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TArcDrive.GetDirectory;
 var F: PFileRec;
     FA: PArcFile;
     I,si: LongInt;
     AllFiles: Boolean;
     AFiles, FD: PFilesCollection;
     TTL, TPL: TSize;
     _USize, _PSize: TSize;
     FR: TFileRec;
     OW: Pointer;
     Dr: String;
begin
 AFiles := New(PFilesCollection, Init($10, $10));
 PFilesCollection(AFiles)^.Owner := Owner;
 PFilesCollection(AFiles)^.SortMode := Sortmode;
 GetDirectory := AFiles;
 While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do Dec(CurDir[0]);
 FD := New(PFilesCollection, Init($40, $10));
 FreeSpace := ''; TotalInfo := ''; TTL := 0; TPL := 0;
 AllFiles := (FileMask = x_x) or (FileMask='*');
 FD^.SortMode := 141;
 FD^.Duplicates := False;
 AFiles^.Duplicates := False;
 Files^.ResetPointer('');
 While not Files^.Last and Files^.GetNextFile and (MaxAvail > LowMemSize) do
  begin
    _USize := Files^.CurFile.Size;
    _PSize := Files^.CurFile.CSize;
    if (UpStrg(CurDir + '\') = UpStrg(Files^.LastDir)) and
       (AllFiles or InFilter(Files^.CurFile.LFN, FileMask)
                 or InFilter(Files^.CurFile.Name, FileMask)) then
      begin
        if Files^.CurFile.Name = '' then Continue;
        if Files^.CurFile.Name = '..' then Continue;
        with Files^.CurFile do
         F := NewFileRec(LFN, {$IFNDEF OS2}GetURZ(Name), {$ENDIF}_USize, Date, Attr, @CurDir);
        F^.PSize := _PSize;
        TTL := TTL + _USize;
        TPL := TPL + _PSize;
      end else if (UpStrg(CurDir+'\') = UpStrg(Copy(Files^.LastDir, 1, Length(CurDir)+1))) then
              begin
                Dr := Copy(Files^.LastDir, Length(CurDir)+2, 255);
                I := PosChar('\', Dr); if I = 0 then I := Length(Dr)+1;
                Dr[0] := Char(I-1);
                if Dr = '' then Continue;
                FillChar(FR, SizeOf(FR), 0);
                {$IFNDEF OS2}
                FR.Name := Norm12(GetURZ(Dr)); FR.Name[9] := ' ';
                {$ENDIF}
                FR.LFN := AddLFN(Dr);
                FR.Attr := Directory;
                I := FD^.IndexOf(@FR);
                DelLFN(FR.LFN);
                if I >= 0 then
                 with PFileRec(FD^.At(I))^ do
                  begin
                    Size:=Size+_USize;
                    PSize:=PSize+_PSize;
                    TTL := TTL + _USize;
                    TPL := TPL + _PSize;
                    Continue;
                  end;
                F := NewFileRec(Dr, {$IFNDEF OS2}GetURZ(Dr), {$ENDIF}_USize, ArcDate, $80 or Directory, @CurDir);
                F^.PSize := _PSize;
                TTL := TTL + _USize;
                TPL := TPL + _PSize;
                if FD^.Search(F,si) then begin DelFileRec(F); continue; end
                                    else FD^.AtInsert(si, F);
              end else Continue;
      if AFiles^.Search(F,si) then DelFileRec(F)
                              else AFiles^.AtInsert(si, F);
  end;
 if MaxAvail <= LowMemSize then
 begin
   Application^.OutOfMemory;
 end;
 FreeSpace := '';
 TotalInfo := CalcTotalInfo(AFiles^.Count, AFiles^.Count, TTL);

 if CurDir = '' then OW := @ArcName else OW := @CurDir;

 F := NewFileRec('..',{$IFNDEF OS2}'..'{$ENDIF},TTL{Round(TTL)}{2gb fix by piwamoto}, ArcDate, Directory, OW);
 F^.Attr := $8000 or F^.Attr;
 F^.PSize := TPL{Round(TPL)}{2gb fix by piwamoto};
 AFiles^.AtInsert(0, F);
 FD^.DeleteAll; Dispose(FD, Done);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.UseFile;
 var
     C: Char;
     ExecOK: Boolean;
     SS, S, S2, Q: String;
label TryAgain;
begin
   TempFile:=''; {-$VOL}
   if (Command = cmEditFile) or (Command = cmFileEdit) or
      (Command = cmIntEditFile) or (Command = cmIntFileEdit) then Exit;
   case Command of
    cmDBFView: C := '=';
    cmWKZView: C := '>';
    cmTextView: C := '<';
    cmHexView: C := '|';
    cmIntFileView: C := '-';
    else C := '+';
   end;
   {if not CheckPassword(AF) then Exit;}
   S := ' ';
   if P^.Attr and Hidden <> 0 then
    begin
     S := '';
TryAgain:
     if ExecResource( dlgSetPassword, S ) <> cmOK then Exit;
   { Flash >>> }
    if CheckForSpaces(S) then
     S := ' ' + CnvString(AType^.Garble) + S + ' '
    else
    if AType^.UseLFN then
     S := ' ' + CnvString(AType^.Garble) + '"' + S + '"' + ' '
    else
     begin
     MessageBox(GetString(dlSpacesInPassword), nil, mfWarning + mfOkButton);
     goto TryAgain;
     end;
   { Flash <<< }
    end;
   SS := MakeNormName(P^.Owner^, GetLFN(P^.LFN));
   if SS[1] = '\' then DelFC(SS);
{$IFNDEF OS2}
   if not AType^.UseLFN then ArcName := lfGetShortFileName(ArcName);
{$ENDIF}
   S := CnvString(AType^.Extract)+' '+s+SquashesName(ArcName);
   if (GetExt(ArcName) = '.') and (S[length(S)] <> '.')
      then S := S + '.';{piwamoto: extracting from extensionless archives}
   S := S +' '+SquashesName(SS)+' ';
{   DelDoubles('  ',S);}{piwamoto: files can have 2 spaces in names}
   TempFile := C + MakeNormName(TempDir , GetLFN(P^.LFN));
   Q:='|' + GetRealName+':'+MakeNormName(CurDir, GetLFN(P^.LFN));
   if C in ['<','-','|','+'] then TempFile := TempFile + Q;
   S2:=Copy(TempFile,2,255); {ExecOK:=True;}
   if not ExistFile(S2) or (PackedDate(P)<>FileTime(S2)) then
   begin
    lGetDir(0, DirToChange);
    Advance2.lChDir(TempDir);
    {ExecOK :=} Exec(CnvString(AType^.unPacker),s);
    Advance2.lChDir(DirToChange); DirToChange := '';
   end;
(* {JO: убрал, т.к. без этого всё работает, а с этим куском - глючит просмотр в архивах с фильтром}
   if ExecOK then
   begin
     TempFile := MakeNormName(TempDir, GetLFN(P^.LFN));
     if Q<>'' then TempFile:=TempFile+Q;
     While TempFile[Length(TempFile)] = ' ' do Dec(TempFile[0]);
     Message(Application, evCommand, Command, @TempFile);
   end;
*)
{$IFDEF VIRTUALPASCAL}
   TempFileSWP := TempFile; {JO}
{$ENDIF}
   TempFile:=''; {-$VOL}
{$IFDEF VIRTUALPASCAL}
  Message(Application, evCommand, cmRetrieveSwp, nil); {JO}
{$ENDIF}
end;
        {-DataCompBoy-}

function TArcDrive.Exec;
 var S: String;
     SM: Word;
     DE: Word;

procedure StdMsg8;
var
  L: array[0..1] of LongInt;
begin
  Application^.Redraw;
  Pointer(L[0]) := @S; L[1] := DE;
  Msg(dlArcMsg8, @L, mfOkButton or mfError);
end;

begin
  Exec := True;
  S := Prg+' '+Cmd;
  if AType^.Swap then
  begin
    Exec := Message(Application, evCommand, cmExecString, @S) = nil;
    Exit;
  end;
  DoneSysError;
  DoneEvents;
  DoneVideo;
  FreeMem(UserScreen, UserScreenSize);
  UserScreen := nil;
  ScreenSaved := Off;
  DoneDOSMem;
  DoneMemory;
  {$IFDEF OS_DOS}asm cld; mov ax,3; int $10; end;{$ENDIF}
  S := GetEnv('PATH');
  S := FSearch(Prg,S);
  SwapVectors;
  if InFilter(S, '*.EXE;*.COM')
   then DOS.Exec(S,Cmd)
   else DOS.Exec(GetEnv('COMSPEC'),'/c '+S+' '+Cmd);
  DE := DosError; ClrIO;
  SwapVectors;
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.LST'); {DataCompBoy}
  InitDOSMem;
  InitMemory;
  SetVideoMode(ScreenMode); SM := ScreenMode;
  SetBlink(On);
  InitVideo; ScreenMode := SM;
  InitEvents;
  InitSysError;
  if (StartupData.Load and osuResetPalette <> 0) and VGASystem { dagoon }
     then SetPalette(vga_palette);
  case DE of
     0 : Application^.Redraw;
     8 : StdMsg(1);
    else StdMsg8;
  end;
  GlobalMessage(evCommand, cmPanelReread, nil);
  GlobalMessage(evCommand, cmRereadInfo, nil);
end;

function TArcDrive.isUp;
begin
 isUp := {CurDir = ''}True;
end;

procedure TArcDrive.ChangeUp;
begin
  if CurDir <> '' then
   begin
     S := GetName(CurDir);
     lChDir('..');
     Exit
   end;
  if Owner = nil then Exit;
  if Prev = nil then
   begin
     New(Prev, Init(0, Owner, innum));
     if Prev = nil then Exit;
     {Prev^.Owner := Owner;}
   end;
  PFilePanel(Owner)^.Drive := Prev;
  if TypeOf(Prev^)=TypeOf(TDrive) then Prev^.lChDir(GetPath(ArcName));
  if (Prev^.DriveType = dtDisk) and
     (PView(Owner)^.GetState(sfSelected+sfActive)) then
      ActivePanel := Owner;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  Prev := nil;
  S := GetName(ArcName);
  if KillAfterUse then EraseTempFile(ArcName);
  Dispose(PDrive(@Self), Done);
end;

procedure TArcDrive.ChangeRoot;
begin
 CurDir := '';
end;

        {-DataCompBoy-}
function TArcDrive.MakeListFile;
 var F: lText;
     PF: PFileRec;
     PA: PArcFile;
     I: Integer;
     S, S1: String;
     B: Boolean;

  function GetArcOwn(S: String): String;
  begin
    if Length(S) < 2 then begin GetArcOwn := ''; Exit; end;
    S[2] := ';'; {JO: реально не важно на что меняем, лишь бы не ':'}
    GetArcOwn := Copy(S, PosChar(':', S) + 1, 255);
  end;

  procedure PutDir(SS: String);
   var I: Integer;
       S1: String;
  begin
   if not (SS[Length(SS)] in ['\','/']) then AddStr(SS, '\');
   Files^.ResetPointer('');
   while not Files^.Last and Files^.GetNextFile do
     if (SS = Copy(Files^.LastDir, 1, Length(SS))) and (Files^.CurFile.Name <> '') then
      begin
        S1 := Files^.LastDir + Files^.CurFile.LFN;
        if S1[1] in ['\','/'] then DelFC(S1);
        if B then S := S + ' ' + SquashesName(S1) else WriteLn(F.T, S1);
      end;
  end;

begin
 B := AType^.ListChar = ' ';
 if B then S := '' else
  begin
   S := SwpDir + '$DN'+ItoS(DNNumber)+'$.LST';
   lAssignText(F, S); ClrIO;
   lRewriteText(F);
   B := IOResult <> 0;
   if B then S := '' else S := AType^.ListChar + S;
  end;
 for I := StopPoint to PC^.Count - 1 do
  begin
   PF := PC^.At(I);
{JO: проверка для разархивирования из панели поиска в архивах}
   if PathFoundInArc(PF^.Owner^) then
     S1 := MakeNormName(GetArcOwn(PF^.Owner^), GetLFN(PF^.LFN))
    else
     S1 := MakeNormName(PF^.Owner^, GetLFN(PF^.LFN));
   if S1[1] in ['\','/'] then DelFC(S1);
   if PF^.Attr and Directory = 0
      then if B then S := S + ' ' + SquashesName(S1) else WriteLn(F.T, S1)
      else PutDir('\'+S1+'\');
   MakeListFile := S;
{$IFDEF OS_DOS}
   if S[0] > #70 then
{$ELSE}
   if S[0] > #200 then
{$ENDIF}
     begin
       StopPoint := I+1;
       if StopPoint < (PC^.Count - 1) then Uncompleted := True
         else Uncompleted := False;
       Exit;
     end;
  end;
 Uncompleted := False;
 MakeListFile := S;
 if not B then Close(F.T);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.ExtractFiles (AFiles: PCollection; ExtrDir: String;
                                  Own: PView; Options: Byte);
 var SS, S: String;
     DT: Record S: String; W: Word; Psw: String[30]; end;
     ExtrChar: String;
     Nm: String;
     Xt: String;
     Pswd: Boolean;

 procedure UnSelect(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
 begin
  Message(Owner, evCommand, cmCopyUnselect, P);
 end;

 procedure IsPswd(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF} {JO}
 begin
  Pswd := Pswd or (P^.Attr and Hidden <> 0);
 end;

begin
  While ExtrDir[Length(ExtrDir)] = ' ' do Dec(ExtrDir[0]);
  if (ExtrDir = '') or (ExtrDir = '..') then
{$IFNDEF OS2}
  if AType^.UseLFN then
{$ENDIF}
              lFSplit(VArcName, ExtrDir, Nm, Xt) {JO: для распаковки по F4 архивов, просмотренных через фильтр}
{$IFNDEF OS2}
         else lFSplit(lfGetShortFileName(ArcName), ExtrDir, Nm, Xt)
{$ENDIF}
  ;

  if not (ExtrDir[Length(ExtrDir)] in ['\','/']) then ExtrDir := ExtrDir + '\';
  repeat   {JO}
    SS := MakeListFile(AFiles);
    S := ' '; Pswd := Off;
    AFiles^.ForEach(@IsPswd);
    if not Uncompleted
       and (PFilePanel(ActivePanel)^.SelNum > 0) then { Flash 17-02-2003 }
      AFiles^.ForEach(@Unselect);
    ExtrChar := CnvString(AType^.ExtractWP);
    if Options and 1 = 0 then ExtrChar := CnvString(AType^.Extract);
    if Options and 2 <> 0 then ExtrChar := CnvString(AType^.Test);
    if (Password = '') and Pswd then
      if ExecResource(dlgSetPassword, Password) <> cmOK then Exit;
    if Password <> '' then S := ' ' + DelSpaces(CnvString(AType^.Garble) + Password) + ' ';
{$IFNDEF OS2}
   if not AType^.UseLFN then ArcName := lfGetShortFileName(ArcName);
{$ENDIF}
    S := ExtrChar + ' ' +s+ SquashesName(ArcName);
    if (Xt = '.') and (S[length(S)] <> '.')
       then S := S + '.';{piwamoto: extracting from extensionless archives}
    S := S +' '+SS+' ';
    lGetDir(0, DirToChange);
    CreateDirInheritance(ExtrDir, On);
    Advance2.lChDir(ExtrDir);
    DelDoubles('  ', S);
    Exec(CnvString(AType^.unPacker),s);
  until not Uncompleted; {JO}
  StopPoint := 0; {JO}
  Advance2.lChDir(DirToChange); DirToChange := '';
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.CopyFiles;
 var
     DT: Record S: String; W: Word; Psw: String[30]; end;
     ExtrDir: String;
     ExtrChar: String;
begin
  ExtrDir := '';
  DT.S := '';
  DT.Psw := Password;
  DT.W := byte(ExtractWithPathNames);
  Message(Application, evCommand, cmPushFirstName, @DT.S);
  if CopyDirName <> '' then DT.S := CopyDirName;
{  if DT.S = cTEMP_ then DT.S := '';}
  if DT.S = '' then GlobalMessageL(evCommand, cmPushName, hsExtract);
  if DT.S = '' then DT.S := HistoryStr(hsExtract, 0);
{  if DT.S = cTEMP_ then DT.S := '';}
  CopyDirName := '';
  if not SkipCopyDialog then
     if ExecResource( dlgExtract, DT) <> cmOK then Exit;
  if ((DT.W and 1)<>0)<>ExtractWithPathNames then ConfigModified:=True;
  ExtractWithPathNames:=(DT.W and 1) <> 0;
  SkipCopyDialog := Off;
  if DT.S = cTEMP_ then
    begin
     CopyToTempDrive(AFiles, Own, ArcName);
     Exit;
    end;
  ExtrDir := DT.S;
  Password := DT.Psw;
  ExtractFiles(AFiles, ExtrDir, Own, DT.W);
end;
        {-DataCompBoy-}

procedure TArcDrive.MakeDir;
begin
end;

        {-DataCompBoy-}
procedure TArcDrive.EraseFiles;
 var SS, S: String;
     PF: PFileRec;
     J, I: Word;
     O: PView;
     P: PString;
begin
  if AFiles^.Count = 0 then Exit;
  if AFiles^.Count = 1 then
   begin PF := AFiles^.At(0);
         S := GetString(dlEraseConfirm1) + GetLFN(PF^.LFN) + ' ?'; end
  else S := GetString(dlEraseConfirms1);
  I := MessageBox(S,nil,mfConfirmation+mfYesButton+mfNoButton{+mfFastButton});
  if (I <> cmYes) then Exit;
  if AFiles^.Count > 1 then
   begin
    S := GetString(dlEraseConfirm2)+ItoS(AFiles^.Count)+' '+GetString(dlDIFiles)+ ' ?';
    J := MessageBox(S,nil,mfConfirmation+mfYesButton+mfNoButton);
    if (J <> cmYes) then Exit;
   end;
  SS := MakeListFile(AFiles);
{$IFNDEF OS2}
  if not AType^.UseLFN then ArcName := lfGetShortFileName(ArcName);
{$ENDIF}
  S := CnvString(AType^.Delete)+' '+SquashesName(ArcName);
  if (GetExt(ArcName) = '.') and (S[length(S)] <> '.')
     then S := S + '.';{piwamoto: deleting from extensionless archives}
  S := S +' '+SS;
  ForceRescan := True;
  Exec(CnvString(AType^.Packer),s);
  ForceRescan := Off;
  O := Owner;
  if not ReadArchive then
    begin
      CurDir := '';
      S := Cut(ArcName, 40);
      MessageBox(GetString(dlArcMsg4) + S, nil, mfError + mfOKButton);
      ChangeUp(CurDir);
      PFilePanel(O)^.ReadDirectory;
    end else
      PFilePanel(O)^.RereadDir;
end;
        {-DataCompBoy-}

function TArcDrive.CalcLengthWithoutName;
begin
  CalcLengthWithoutName := (10 + CountryInfo.TimeFmt) * Byte(Flags and psShowSize <> 0) +
                11 * Byte(Flags and psShowPacked <> 0) +
                 9 * Byte(Flags and psShowDate <> 0) +
                (7 - CountryInfo.TimeFmt) * Byte(Flags and psShowTime <> 0) +
                 4 * Byte(Flags and psShowRatio <> 0);
end;

function TArcDrive.CalcLength;
 var B: Word;
begin
{$IFNDEF OS2}
 if Flags and psShowLongName <> 0 then
{$ENDIF}
           B := LFNLen + 1 + CalcLengthWithoutName
{$IFNDEF OS2}
 else
           B := 13 + CalcLengthWithoutName
{$ENDIF}
 ;
 if B > MaxViewWidth then B := MaxViewWidth;
 CalcLength := B;
end;

procedure TArcDrive.MakeTop;
begin
if (LFNLen < 250) {$IFNDEF OS2} or (Flags and psShowLongName = 0){$ENDIF}then
begin
{$IFNDEF OS2}
 if Flags and psShowLongName <> 0 then
     begin
{$ENDIF}
       if LFNLen <= SizeX then
       S := CenterStr(GetString(dlTopLFN), LFNLen)+GetString(dlTopSplit)
         else S := AddSpace(CenterStr(GetString(dlTopLFN), SizeX), LFNLen)
{$IFNDEF OS2}
       ;
     end
   else S := GetString(dlTopName)
{$ENDIF}
end else S := '';
 if Flags and psShowSize     <> 0 then S := S + Copy(GetString(dlTopOriginal),2-CountryInfo.TimeFmt,255);
 if Flags and psShowPacked   <> 0 then S := S + GetString(dlTopPacked);
 if Flags and psShowRatio    <> 0 then S := S + GetString(dlTopRatio);
 if Flags and psShowDate     <> 0 then S := S + GetString(dlTopDate);
 if Flags and psShowTime     <> 0 then S := S + Copy(' '+GetString(dlTopTime),1+CountryInfo.TimeFmt,255);
 if (LFNLen >= 250) {$IFNDEF OS2} and (Flags and psShowLongName <> 0){$ENDIF}
         then S := S  + AddSpace(CenterStr(GetString(dlTopLFN), SizeX-Length(S)), LFNLen);
end;

        {-DataCompBoy-}
procedure TArcDrive.GetDown;
 var FR: TFileRec;
     S,S1 {$IFDEF OS2} ,S2 {$ENDIF}: String;
     {$IFNDEF OS2} S3,S4: String; {$ENDIF} { Flash 09-02-2003 }
begin
    if P=nil then exit;
    FR := P^; FR.Name[9] := ' ';
    if (FR.Attr and Directory = 0) or (FR.Attr and $8000 <> 0) and (FR.Size >= 0)
    or (PFilePanel(Owner)^.PanelFlags and fmiDirLen <> 0)
     {or (FR.Size >= 0)}
      then S := FileSizeStr( FR.Size )
      else if FR.TType = ttUpDir
        then S := GetString( dlUpDir )
        else S := GetString( dlSubDir );
    MakeDate(DateMode, FR.Day, FR.Month, FR.Yr, FR.Hour, FR.Minute, S1);
{$IFNDEF OS2} { Flash 09-02-2003 >>> }
    S3 := GetExt(GetLFN(FR.LFN));
    S3 := ' '+Copy(S3,2,Length(S3)-1);
    if Length(S3)>4 then S3 := Copy(S3,1,3)+FMSetup.RestChar[1];
    S4 := GetSName(GetLFN(FR.LFN));
    if Length(S4)>8 then S4 := Copy(S4,1,7)+FMSetup.RestChar[1];
    while Length(S4)<8 do S4 := S4+' ';
    S3 := MakeFileName(S4+S3);
    if FR.Attr and Directory = 0 then S3 := LowStrg(S3) else S3 := UpStrg(S3);
    MoveStr(B, S3, C);
{$ELSE}       { Flash 09-02-2003 <<< }
    S2 := GetLFN(FR.LFN);
    if Byte(S2[0]) > 14 then
      begin
        S2[0] := #14;
        S2[14] := FMSetup.RestChar[1];
      end;
    MoveStr(B,S2,C);
{$ENDIF}
    MoveStr(TWordArray(B)[22-Length(S)+CountryInfo.TimeFmt],S , C);
    MoveStr(TWordArray(B)[23+CountryInfo.TimeFmt],S1, C);
end;
        {-DataCompBoy-}

procedure TArcDrive.GetFull;
var
  S1: String[40];
  X: AWord; LS: Byte;
  NFM: TNameFormatMode;
  OPT,CO : Word;
  S, Ext: String;
  label Longer250, Longer250_1;
begin
X := 0;
CO := C;
if LFNLen < 250 then
{JO: если ширина колонки имени больше 250 символов, её показываем после остальных колонок}
Longer250:
 begin
{$IFNDEF OS2}
  if Flags and psShowLongName <> 0 then
   begin
    s:=GetLFN(P^.LFN);
{$ELSE}
    s:=P^.Name;
{$ENDIF}
    Ext := GetExt(S);
    NFM:=nfmNull;
    if (EXTLen=0) or ((Flags and psVariableExtDA<>0) and
                      ((Length(Ext)=1) or (Length(S) = Length(Ext))))
     then OPT:=flnPreferName or flnUseCutChar or flnPadRight or flnHighlight or flnHandleTildes
     else OPT:=flnHardExtArea or flnAutoHideDot or flnUseCutChar or flnPadRight or flnHighlight or flnHandleTildes;
    if P^.Selected and (FMSetup.Show and fmsTagChar<>0) then OPT:=OPT or flnSelected;

    case P^.Attr and (Hidden+SysFile) of               {Pavel Anufrikov -> }
     Hidden  :        NFM:=nfmHidden;
     SysFile :        NFM:=nfmSystem;
     Hidden+SysFile : NFM:=nfmHiddenSystem;
    end{case};                                         { <- Pavel Anufrikov}

    if P^.Attr and Directory <> 0
      then if DirFLP < 65534
             then OPT:=OPT or DirFLP
             else if DirFLP = 65535
                    then if (Length(GetSName(S))<=8) and
                            (Length(Ext)<=4)
                           then OPT:=OPT or flnUpCase
                           else
                    else if (not IsMixedCase(S))
                           then OPT:=OPT or flnUpCase
                           else
      else if FilFLP < 65534
             then OPT:=OPT or FilFLP
             else if FilFLP = 65535
                    then if (Length(GetSName(S))<=8) and
                            (Length(Ext)<=4)
                           then OPT:=OPT or flnLowCase
                           else
                    else if (not IsMixedCase(S))
                           then OPT:=OPT or flnLowCase
                           else ;

    S:=FormatLongName(S, LFNLen, EXTLen, OPT, NFM, nil);
    if P^.Selected and (FMSetup.Show and fmsTagChar=0) then S:='~'+S+'~';
{$IFNDEF OS2}
   end
  else
   begin
    NFM:=nfmNull;
    OPT:=flnHardExtArea or flnAutoHideDot or flnPadRight or flnHighlight or flnHandleTildes;
    if P^.Selected and (FMSetup.Show and fmsTagChar<>0) then OPT:=OPT+flnSelected;

    case P^.Attr and (Hidden+SysFile) of               {Pavel Anufrikov -> }
     Hidden  :        NFM:=nfmHidden;
     SysFile :        NFM:=nfmSystem;
     Hidden+SysFile : NFM:=nfmHiddenSystem;
    end{case};                                         { <- Pavel Anufrikov}
    if P^.Attr and SysFile<>0 then OPT:=OPT or flnCapitalCase;


    if P^.Attr and Directory <>0 then OPT:=OPT or flnUpCase
                                 else OPT:=OPT or flnLowCase;
    S:=FormatLongName(MakeFileName(P^.Name), 12, 3, OPT, NFM, nil);
    if P^.Selected and (FMSetup.Show and fmsTagChar=0) then S:='~'+S+'~';
   end;

{$ENDIF}

{!!!!!!!!!!!}

  MoveCStr(TWordArray(B)[X], S, C);
  {$IFNDEF OS2}if Flags and psShowLongName <> 0 then {$ENDIF} Inc(X, LFNLen)
  {$IFNDEF OS2}  else Inc(X, 12) {$ENDIF}
  ;
{-----------}
  if X >= 255 then Exit;
  if LFNLen >= 250 then goto Longer250_1;
{JO: не понял, зачем DataCompBoy ввёл условие                                }
{    PView(Owner)^.GetState(sfFocused), приводившее к исчезновению выделения }
{    цветом всего, что идёт после имени у помеченных файлов при переходе     }
{    на противоположную панель; без этого условия всё работает нормально     }
  if P^.Selected {and PView(Owner)^.GetState(sfFocused)} then
    begin
      C:=C shr 8;
      SC:=SC and $00FF + C shl 8;
    end;
  Inc(X);
  if X >= 255 then Exit;
  TWordArray(B)[X-1]:=SC;
 end {условие 250 символов}
  else if P^.Selected then
    begin
      C := C shr 8;
      SC := SC and $00FF + C shl 8;
    end;

  MakeDate(DateMode, P^.Day, P^.Month, P^.Yr, P^.Hour, P^.Minute, S1);
  if (P^.Size > 0) or (P^.Attr and Directory = 0)
    then S := FileSizeStr( P^.Size ){!}
    else if P^.TType = ttUpDir
      then S := GetString( dlUpDir )
      else S := GetString( dlSubDir );
  if Flags and psShowSize <> 0 then
   begin
    for LS := 1 to (9 - Length(S) + CountryInfo.TimeFmt) do S := ' ' + S; {JO}
    MoveStr(TWordArray(B)[X], S, C);
    X := X+10+CountryInfo.TimeFmt;
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowPacked <> 0 then
   begin
    if PosChar('<',S) = 0 then
      begin
        S := FStr(P^.PSize);
        for LS := 1 to (10 - Length(S)) do S := ' ' + S; {JO}
      end
     else if (CountryInfo.TimeFmt = 0) then S := ' ' + S;
    MoveStr(TWordArray(B)[X], S, C);
    Inc(X, 11);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowRatio <> 0 then
   begin
    Inc(X, 4);
    if X >= 255 then Exit;
    if PosChar('<',S) = 0 then
     begin
      if P^.Size <> 0 then S := ZtoS(Percent(P^.Size, P^.PSize))
                      else S := '100';
      S := PredSpace(S, 3);
     end else S := '   ';
    MoveStr(TWordArray(B)[X-Length(S)-1], S, C);
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowDate <> 0 then
   begin
    MoveStr(TWordArray(B)[X], Copy(S1,1,8), C);
    Inc(X, 9);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowTime <> 0 then
   begin
    MoveStr(TWordArray(B)[X], Copy(S1,10,6), C);
    Inc(X, 7+CountryInfo.TimeFmt);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if LFNLen >= 250 then begin C := CO; goto Longer250; end;
Longer250_1:
end;

procedure TArcDrive.GetEmpty;
 var
  X: AWord;
begin
  if (LFNLen < 250) {$IFNDEF OS2} or (Flags and psShowLongName = 0){$ENDIF} then
    begin
{$IFNDEF OS2}
      if Flags and psShowLongName <> 0 then
{$ENDIF}
         X:=LFNLen+1
{$IFNDEF OS2}
        else X:=13
{$ENDIF}
        ;
      if X >= 255 then Exit;
      TWordArray(B)[X-1]:=SC;
    end else X := 0;
  if Flags and psShowSize <> 0 then
   begin
    X := X+10+CountryInfo.TimeFmt;
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowPacked <> 0 then
   begin
    Inc(X, 11);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowRatio <> 0 then
   begin
    Inc(X, 4);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowDate <> 0 then
   begin
    Inc(X, 9);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowTime <> 0 then
   begin
    Inc(X, 7+CountryInfo.TimeFmt);
    if X >= 255 then Exit;
    TWordArray(B)[X-1]:=SC;
   end;
end;

function TArcDrive.GetRealName;
 var S: String;
begin
 S := GetDir;
 GetRealName := Copy(S, 1, PosChar(':', S)) + ArcName;
end;

procedure TArcDrive.CopyFilesInto;
begin
 ForceRescan := On;
 ArchiveFiles(GetRealName, AFiles, MoveMode, nil);
 ForceRescan := Off;
end;

procedure TArcDrive.HandleCommand;
 var C: PCollection absolute InfoPtr;

 procedure SetPassword;
  var S: String;
 begin
   S := Password;
   if ExecResource( dlgSetPassword, S) <> cmOK then Exit;
   Password := S;
 end;

 procedure TestFiles;
 begin
  ExtractFiles(C, '', Owner, 2);
 end;

 procedure Extract;
 begin
  ExtractFiles(C, '', Owner, 1);
 end;

 var PDr: PDrive;
     S: String;
     O: PView;

begin
 case Command of
   cmSetPassword: SetPassword;
   cmArcTest: TestFiles;
   cmExtractTo: Extract;
   cmMakeForced: ForceRescan := On;
   cmRereadForced: if ForceRescan then
                    begin
                      ForceRescan := Off;
{$IFNDEF MINARCH}
                      if (AType^.GetID = arcUC2) or (AType^.GetID = arcAIN) then
                        begin
                          PFilePanel(Owner)^.ForceReading := On;
                        end;
{$ENDIF}
                      O := Owner;
                      if not ReadArchive then
                       begin
                         CurDir := '';
                         ChangeUp(CurDir);
                       end;
                       PFilePanel(O)^.RereadDir;
                    end;
   cmFindFile: FindFile;
 end;
end;

procedure TArcDrive.GetFreeSpace;
begin
  S := '';
end;

function ArcViewer;
 var P: PDrive;
     E: TEvent;
     I: Byte;
     Xt: String;
     PathInside: String;
begin
{JO: дабы перейти к найденному файлу в архиве из панели поиска}
  PathInside := AName;
  if PathInside[2] = ':' then PathInside[2] := ';'; {JO: меняем двоеточие не важно на что }
    I := PosChar(':', PathInside);
    if I > 0 then
      begin
        PathInside := Copy(AName, I+1, 255);
        AName := Copy(AName, 1, I-1);
      end
     else PathInside :=  '';
{/JO}
 ArcViewer := False;
 P := New(PArcDrive, Init(AName, VAname, num));
 if Abort then Exit;
 if P = nil then
   begin
     Xt:=UpStrg(GetExt(AName));
     if (Xt = '.TDR') or (Xt = '.AVT')
      then P := New(PArvidDrive, Init(AName, num));
     if P = nil then Exit;
   end;
 E.What := evCommand;
 E.Command := cmInsertDrive;
 E.InfoPtr := P;
 Desktop^.HandleEvent(E);
 if E.What <> evNothing then
   begin
     Dispose(P, Done);
     Exit;
   end
    else ArcViewer := True;
{JO: переходим к найденному файлу в архиве}
 if PathInside <> '' then
   begin
     if Copy(PathInside, Length(PathInside)-1, 2) = '\.'
       then PathInside[0] := Char(Length(PathInside)-2);
     if (GetPath(PathInside) <> '\') then
       begin
         P^.lChDir(Copy(GetPath(PathInside), 2, 255));
         Message(Application, evCommand, cmPanelReread, nil);
       end;
    end;
{/JO}
end;

function TArcDrive.GetFullFlags;
begin
 GetFullFlags := psShowSize + psShowDate + psShowTime + psShowPacked + psShowRatio;
end;

procedure TArcDrive.RereadDirectory;
begin
 if Prev <> nil then Prev^.RereadDirectory(S);
end;

procedure TArcDrive.GetDirInfo;
  var Fl: Integer;
      PSz, USz: TSize;

  procedure DoCount(P: PArcFile); {$IFDEF BIT_16}far;{$ENDIF}
  begin
    if (P <> nil) and (not(P^.FName^[Length(P^.FName^)] in ['\','/'])) then
      begin
        Inc(Fl);
        USz:=USz+P^.USize;
        PSz:=PSz+P^.PSize;
      end;
  end;

begin
  B.Title := NewStr( GetString( dlDICurArchive ));
  B.Dir := NewStr(ArcName);

  Fl := Files^.Files; PSz := Files^.TotalCLength; USz := Files^.TotalLength;
  {
  if Files <> nil then Files^.ForEach(@DoCount);
  }
  B.Files := NewStr( GetString( dlDIArcTotalFiles )+ItoS(Fl)+'~');

  B.Total := NewStr( GetString( dlDIPackedSize )+FStr(PSz)+'~');
  B.Free  := NewStr( GetString( dlDIUnpackedSize )+FStr(USz)+'~');

end;

procedure TArcDrive.GetDirLength(PF: PFileRec);
begin
end;

function ESC_Pressed: Boolean; var E: TEvent;
begin
  Application^.Idle;
  GetKeyEvent(E); ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbEsc)
end;

function TArcDrive.OpenDirectory(const Dir: String): PDrive;
  var PDrv: PDrive;
      Dirs: PStringCollection;
      Fils:PFilesCollection;
      FR: PFileRec;
      tmr: TEventTimer;
      _USize, _PSize: TSize;
      l: integer;
      PDir: PString;
      PI: PView;
      Root: string;
begin
  NewTimer(Tmr,0);
  Dirs := New(PStringCollection, Init($10,$10));
  PI := WriteMsg(GetString(dlReadingList));
  New(Fils, Init($10, $10)); Fils^.SortMode := psmOrdered;
  Files^.ResetPointer('');
  Root := UpStrg(CurDir) + '\'; l := length(Root);
  While not Files^.Last and Files^.GetNextFile and (MaxAvail > LowMemSize)
  do
   begin
     if (Root = Copy(UpStrg(Files^.LastDir), 1, l)) then
       begin
       with Files^.CurFile do
         begin
         if (Name = '') or (Name = '..') then Continue;
         _USize := Size;
         _PSize := CSize;
         PDir := NewStr(Files^.LastDir);
         FR := NewFileRec(LFN, {$IFNDEF OS2}GetURZ(Name), {$ENDIF}_USize, Date, Attr, PDir);
         end;
         if Dirs^.IndexOf(PDir) = -1 then Dirs^.Insert(PDir);
       Fils^.AtInsert(Fils^.Count, FR);
       if TimerExpired(Tmr) then
         begin
           NewTimer(Tmr, 1);
           if ESC_Pressed then Break;
         end;
       end;
   end;
  PI^.Free;
  PDrv := New(PFindDrive, Init(GetString( dlBranch )+Dir, Dirs, Fils, FreeByte));
  OpenDirectory := PDrv;
end;

type
    TArcFindRec = record
     Mask: String; {DataCompBoy}
     Options: Word;
     AddChar: String[3];
    end;

const
     ArcFindRec: TArcFindRec = ( Mask : '*.*';
                                 Options : 2; {рекурсивный поиск}
                                 AddChar : ''
                                 );

procedure TArcDrive.FindFile;
  var
      PDrv: PDrive;
      Dirs: PStringCollection;
      Fils:PFilesCollection;
      FR: PFileRec;
      tmr: TEventTimer;
      _USize, _PSize: TSize;
      L: Integer;
      PDir: PString;
      I, DateAfter, DateBefore, SizeGreat, SizeLess: LongInt;
      Attr: Byte;
      PI: PView;
      LDir, DrName: String;
      Root: String;

begin
  NewTimer(Tmr,0);
  ArcFindRec.AddChar := '';
  ArcFindRec.Options := (FindRec.Options and 1) +
                         ((FindRec.Options and 4) shr 1);
  if ExecResource(dlgArcFileFind, ArcFindRec) = cmCancel then Exit;
  FindRec.Options := (FindRec.Options and $FFFA) or (ArcFindRec.Options and 1)
                                        or ((ArcFindRec.Options and 2) shl 1);
  ConfigModified := true;
  DelLeft(ArcFindRec.Mask); DelRight(ArcFindRec.Mask);
  if ArcFindRec.Mask = '' then ArcFindRec.Mask := x_x;
  if (Pos('*', ArcFindRec.Mask)=0) and
     (Pos('.', ArcFindRec.Mask)=0) and
     (Pos(';', ArcFindRec.Mask)=0) and
     (Pos('?', ArcFindRec.Mask)=0) then ArcFindRec.AddChar := '*.*'
                                else ArcFindRec.AddChar := '';

  if ArcFindRec.Options and ffoAdvanced <> 0 then
   begin
     DateAfter := ParseTime(AdvanceSearchData.After);
     DateBefore := ParseTime(AdvanceSearchData.Before);
     if DateBefore = 0 then DateBefore := $7FFFFFFF;
     SizeGreat := Stoi(AdvanceSearchData.Greater);
     SizeLess := Stoi(AdvanceSearchData.Less);
     if SizeLess = 0 then SizeLess := $7FFFFFFF;
     Attr := 0;
     if AdvanceSearchData.Attr and 1 <> 0 then Attr := Archive;
     if AdvanceSearchData.Attr and 2 <> 0 then Attr := Attr or SysFile;
     if AdvanceSearchData.Attr and 4 <> 0 then Attr := Attr or Hidden;
     if AdvanceSearchData.Attr and 8 <> 0 then Attr := Attr or ReadOnly;
   end;

  Dirs := New(PStringCollection, Init($10,$10));
  PI := WriteMsg(^M^M^C + GetString(dlSearching) + '...');
  New(Fils, Init($10, $10)); Fils^.SortMode := psmOrdered;
  Files^.ResetPointer('');
  Root := UpStrg(CurDir) + '\'; l := length(Root);
  While not Files^.Last and Files^.GetNextFile and (MaxAvail > LowMemSize)
  do
   begin
     if (Root = Copy(UpStrg(Files^.LastDir), 1, l)) then
       begin
        with Files^.CurFile do
         begin
          if ((ArcFindRec.Options and ffoAdvanced = 0) or
                 (Date >= DateAfter) and (Date <= DateBefore)
           and (Size >= SizeGreat)  and (Size <= SizeLess)
           and ((Attr = 0) or (FileInfo.Attr and Attr <> 0)))
           and ((ArcFindRec.Options and 2 <> 0) or
                (Root = UpStrg(Files^.LastDir)))
           and (Name <> '') and (Name <> '..')
           and InFilter(LFN, ArcFindRec.Mask + ArcFindRec.AddChar) then
            begin
              _USize := Size;
              _PSize := CSize;
              PDir := NewStr(Files^.LastDir);
              FR := NewFileRec(LFN, {$IFNDEF OS2}GetURZ(Name), {$ENDIF}_USize, Date, Attr, PDir);
              if Dirs^.IndexOf(PDir) = -1 then Dirs^.Insert(PDir);
              Fils^.AtInsert(Fils^.Count, FR);
            end;
         {JO: добавляем каталоги}
          if Length(Files^.LastDir) > L then
            begin
              LDir := Files^.LastDir;
              repeat
                Dec(LDir[0]);
                for I := Length(LDir) downto L do
                  if LDir[I] = '\' then Break;
                DrName := Copy(LDir, I+1, 255);
                LDir[0] := Char(I);
                if (DrName <> '')
{
             //JO: всё равно для каталогов дата и атрибуты показываются
             //    весьма условно, размер равен нулю, так что при расширенном
             //    поиске каталоги только мешают
}
                  and (ArcFindRec.Options and ffoAdvanced = 0)
                  and ((ArcFindRec.Options and 2 <> 0) or
                       (Root = UpStrg(LDir)))
                  and InFilter(DrName, ArcFindRec.Mask+ ArcFindRec.AddChar)
                  then
                   begin
                     PDir := NewStr(Copy(LDir, 1, I));
                     FR := NewFileRec(DrName,{$IFNDEF OS2}GetURZ(DrName),{$ENDIF} 0, ArcDate, $80 or Directory, PDir);
                     if Dirs^.IndexOf(PDir) = -1 then Dirs^.Insert(PDir);
                     if Fils^.Search(FR,I) then DelFileRec(FR)
                                else Fils^.AtInsert(I, FR);
                   end;
              until Length(LDir) <= L;
            end; {конец добавления каталогов}
          if TimerExpired(Tmr) then
            begin
              NewTimer(Tmr, 1);
              if ESC_Pressed then Break;
            end;
         end;
       end;
   end;
  PI^.Free;
  PDrv := New(PFindDrive, Init(GetString(dlFindPanel) + ArcFindRec.Mask, Dirs, Fils, FreeByte));
  Message(Owner, evCommand, cmInsertDrive, PDrv);
end;

end.
