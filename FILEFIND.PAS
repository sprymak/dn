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
//  dn16rc1-move_COMBINE_MIXED_from_sources_to_Options_menu_diff140byMV.patch
//  dn16rc1-pkview_some_dialogs_resources_fix.patch
//
//  2.0.0
//  dn200-files_count_in_FindFiles_panel.patch
//  dn200-files_panel_bugs_fix.patch
//
//  2.3.0
//  dn230-file_find_terminate_more_quick_and_smart.patch
//  dn247-text_and_file_find_improve.patch
//  dn2628_reading_file_list_with_LFN_fix.patch
//
//  2.7.0
//  dn270-search_files_window_title_update_fix.patch
//  dn270-search_in_archives.patch
//  dn270-search_files_mask_with_directory.patch
//  dn281-SearchInArchives_will_NOT_add_extensionless_files_to_archives_list.patch
//  dn281-DN_Lite_is_compilable_again.patch
//  dn2922-show_size_before_name_if_it_longer_250.patch
//  dn281-warn_about_searching_text_inside_of_archives.patch
//  dn21029-highlight_250_fix-jo21104a.patch
//  dn270-ChangeRoot_on_TEMP_drive-jo21115c.patch
//  dn21116-Ctrl_Slash_Up_Down_fix.patch
//  dn21128-read_file_list.patch
//  dn21201-copy_arcpanel_to_temppanel.patch
//  dn21202-Find_file_and_branch_in_archive.patch
//  dn21202-Find_folder_in_archive-jo21207a.patch
//  dn21225-fpanel(if)-copy_files_from_archives_in_find_panel.patch
//  dn328-FilePanel(f)_jo30224a_crash_fix.patch
//  dn3216-TempDrive(f)-unselect_when_return_fix.patch
//  dn3216-fplanel(i)-copy_to_temp_with_dialogs.patch
//  dn3315-FileFind(f)-crash_when_empty_and_run_programm_fix.patch
//  dn3323-temp(f)-copy_files_from_archives.patch
//  dn3421-FileList(i)-read_and_write_files_list_improve.patch
//
//  3.7.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//  dn31220-kernel(if)-work_with_temporary_files.patch
//  dn40328-7Zip.patch
//
//  4.9.0
//  dn40900-search(f)-directory_in_mask.patch
//  dn50115-search(f)-directory_in_mask.patch
//  dn50208-cleanup.patch
//  dn40900-temp_drive_in_directories_history.patch
//
//  5.9.0
//  dn50900-SFX_detection.patch
//
//  6.4.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
unit FileFind;

interface
uses Objects, Views, Dialogs, Drivers, Menus, FileCopy, FilesCol, Eraser,
     Drives, HideView, DOS, LFN, Gauge, DiskInfo, FStorage, LFNCol, DNIni,
     collect;

type
    TFindRec = record
     Mask: String; {DataCompBoy}
     What: String;
     Options: Word;
     Where: Word;
     AddChar: String[3];
    end;

    PListItem  = ^TListItem;
    TListItem = record
     Next: PListItem;
     Item: Pointer;
    end;

    TAdvanceSearchData = record
                    After: String[17]; {Inputline}
                    Before: String[17]; {Inputline}
                    Greater : String[12]; {Inputline}
                    Less : String[12]; {Inputline}
                    Attr : Word; {Checkbox}
                  end;

const
     CFindWindow = #126#127#128#129#130#131#132#133#134#135#136#137#138;
     CFileFinder = #6#7#8#9;
     CFindInfo   = #10#11#12#13;

     ffSeFnd    = $01; {Found a file}           {-$VOL}
     ffSeD2Lng  = $02; {Search string overflow} {-$VOL}
     ffSeNotFnd = $FE; {No files found}         {-$VOL}

     ffoAdvanced    = 1;
     ffoCaseSens    = 2;
     ffoRecursive   = 4;
     ffoWholeWords  = 8; {-$VIV 14.05.99}
     ffoAllCP       = 16; {-$VIV 14.05.99}
     ffoInArch      = 32; {JO 17.06.02}{piwamoto}

     FindRec: TFindRec = ( Mask : '*.*';
                           What : '';
                           Options : 4;
                           Where : 1;
                           AddChar : ''
                         );


    AdvanceSearchData : TAdvanceSearchData =
                    (After: '';
                     Before: '';
                     Greater: '';
                     Less: '';
                     Attr: 0
                     );

{    TempStored: Boolean = False;}


procedure FindFile;
function  FindFiles(var Files: PFilesCollection; var Directories: PCollection;var FindRec: TFindRec; PInfo: PWhileView): byte;
function  ReadList(const AName: String; var DC: PSortedCollection; var FC: PFilesCollection): Boolean; {DataCompBoy}
function  ParseTime(S: String): LongInt;

type

  PFindDrive = ^TFindDrive;
  TFindDrive = object(TDrive)
   isDisposable: Boolean;
   Files: PFilesCollection;
   Dirs: PSortedCollection;
   ListFile: PString;
   UpFile: PFileRec; {DataCompBoy}
   AllowPack: Boolean;
   AMask, AWhat: PString;
   constructor Load(var S: TStream);
   procedure Store(var S: TStream); virtual;
   procedure NewUpFile;
   constructor Init(const AName: String; ADirs: PCollection; AFiles: PFilesCollection; num: byte); {DataCompBoy}
   constructor InitList(const AName: String); {DataCompBoy}
   procedure lChDir(ADir: String); virtual; {DataCompBoy}
   function GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String;
                         var FreeSpace, TotalInfo: String ): PCollection; virtual;
   procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
   procedure CopyFromArc(AFiles: PFilesCollection; Own: PView); {JO}
   procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
   procedure EraseFiles(AFiles: PCollection); virtual;
   procedure UseFile(P: PFileRec; Command: Word); virtual; {JO}
   function Disposable: Boolean; virtual;
   function GetRealName: String; virtual;
   function GetDir: string; virtual;
   procedure MakeDir; virtual;
   destructor Done; virtual;
   procedure GetFull(var B; P:PFileRec; C, SC:Word); virtual; {DataCompBoy}
   procedure GetEmpty(var B; SC: Word); virtual;
   procedure GetFreeSpace(var S: String); virtual;
   function CalcLengthWithoutName: Integer; virtual;
   function CalcLength: Integer; virtual;
   procedure MakeTop(var S: String); virtual;
   function isUp: Boolean; virtual;
   procedure ChangeUp(var S: String); virtual;
   procedure ChangeRoot; virtual;
   procedure RereadDirectory(S: String); virtual;
   function GetFullFlags: Word; virtual;
   procedure GetDirInfo(var B: TDiskInfoRec); virtual;
   procedure GetParam(n: byte); virtual; {DataCompBoy}
   procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;
   function OpenDirectory(const Dir: String): PDrive; virtual;
  end;

  PTempDrive = ^TTempDrive;
  TTempDrive = object(TFindDrive)
   constructor Init(num: byte);
   constructor Load(var S: TStream);
   procedure   Store(var S: TStream); virtual;
   procedure   CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
   function    GetRealName: String; virtual;
   procedure   CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
   procedure   EraseFiles(AFiles: PCollection); virtual;
   procedure   ChangeRoot; virtual;
   procedure   GetDirInfo(var B: TDiskInfoRec); virtual;
   procedure   GetParam(n: byte); virtual; {DataCompBoy}
   destructor  Done; virtual;
  end;

  procedure CopyToTempDrive(AFiles: PCollection; Own: PView; ArchiveName: String);

implementation
uses DNApp, advance, advance1, advance2, advance3, Startup, Memory, Messages,
     HistList, Commands, RStrings, FlPanel, FViewer, MicroEd, Tree, xTime,
     DnUtil, u_keymap, {!!}CmdLine, Histries, TitleSet, DNExec, DNHelp,
     Archiver, ArchDet{JO}{search in archive}
     , ArcView {JO: для разархивирования файлов найденных в архивах}
;


const
  LowMemSize = $6000;  {Local setting}

var  FAdr   : Word;
     UsM    : Word;
     Rd,Ofs : Word;

const ExtrDir: String = '';

function ESC_Pressed: Boolean; var E: TEvent;
begin
  Application^.Idle;
  GetKeyEvent(E); ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbEsc)
end;

function ReadingListMsg: PView; begin ReadingListMsg := WriteMsg(GetString(dlReadingList)) end;

        {-DataCompBoy-}
procedure FindFile;
 var
     TempWord: Word;
     PInfo: PWhileView;
     Files: PFilesCollection;
     Directories: PCollection;
     BB: byte; {-$VOL}
     R: TRect;
     P: PView;
begin
 FindRec.AddChar:='';
 TempWord := FindRec.Options and ffoInArch;

 if ExecResource(dlgFileFind, FindRec) = cmCancel then Exit;
 if (TempWord = 0) and (FindRec.Options and ffoInArch <> 0) then
   begin
    TempWord := MessageBox(GetString(dlWarnIfSearchTextInsideOfArc), nil, mfYesNoConfirm);
    if TempWord <> cmYes then Exit;
   end;
 ConfigModified := True;
 DelLeft(FindRec.Mask); DelRight(FindRec.Mask);
 if FindRec.Mask = '' then FindRec.Mask := x_x;
 if (Pos('*', FindRec.Mask)=0) and
    (Pos('.', FindRec.Mask)=0) and
    (Pos(';', FindRec.Mask)=0) and
    (Pos('?', FindRec.Mask)=0) then FindRec.AddChar := '*.*'
                               else FindRec.AddChar := '';
 New(Files, Init($10, $10)); Files^.SortMode:=psmOrdered;
 Directories := New(PStringCollection, Init(30,30));
 R.Assign(1, 1, 40, 10); Inc(SkyEnabled);
 New(PInfo, Init(R)); PInfo^.Options := PInfo^.Options or ofSelectable or ofCentered;
 PInfo^.HelpCtx := hcFileFind; { Flash 03-02-2003 }
 if FindRec.What=''
  then PInfo^.Top := GetString(dlDBViewSearch)+Cut(FindRec.Mask, 50)
  else PInfo^.Top := GetString(dlDBViewSearch)+Cut(FindRec.Mask, 30)+' | '+Cut(FindRec.What, 17);
 PInfo^.Bottom := GetString(dlNoFilesFound);
 PInfo^.Write(1, GetString(dlDBViewSearchingIn));
 Desktop^.Insert(PInfo);
 if StartupData.Slice2 and osuReleaseFind = 0 then Application^.BFSpeed;
 BB := FindFiles(Files, Directories, FindRec, PInfo);
 if StartupData.Slice2 and osuReleaseFind = 0 then Application^.EFSpeed;
 Desktop^.Delete(PInfo); Dec(SkyEnabled);
 Dispose(PInfo,Done);
 if (BB and ffSeD2Lng) <> 0 then MessageBox( GetString(dlSE_Dir2Long), nil, mfWarning + mfOkButton);
 if (BB and ffSeNotFnd) = BB then MessageBox(^C+GetString(dlNoFilesFound), nil, mfError + mfOkButton);
end;
        {-DataCompBoy-}

function ParseTime(S: String): LongInt;
 label l1, l2;
 var
     I, J: Integer;
     M: LongInt;
     DT: DateTime;
     S1,S2,S3: String[5];
     TM: String[10];
begin
  ParseTime := 0;
  Dt.Sec := 0; Dt.Hour := 0; Dt.Min := 0;
  I := PosChar(' ', S);
  if I > 0 then
    begin
      Tm := DelSpaces(Copy(S, I+1, 10));
      if Tm <> '' then
        begin
          I := Pos(CountryInfo.TimeSep, Tm);
          if I = 0 then Exit;
          DT.Hour := StoI(Copy(Tm, 1, I-1));
          J := 0;
          l2:
          case UpCase(Tm[Length(Tm)]) of
            'M': begin J := 12; Goto l2 end;
            'A': Dec(Tm[0]);
            'P': begin J := 12; Dec(Tm[0]); end;
            '0'..'9':;
            else Goto l1;
          end;
          Inc(DT.Hour, J);
          DT.Min := StoI(Copy(Tm, I+Length(CountryInfo.TimeSep), 10));
        end;
 l1: Byte(S[0]) := PosChar(' ', S) - 1;
    end else Tm := '';
  I := Pos(CountryInfo.DateSep, S);
  if I = 0 then Exit;
  S1 := Copy(S, 1, I-1);
  Delete(S, 1, I+Length(CountryInfo.DateSep)-1);
  I := Pos(CountryInfo.DateSep, S);
  if I = 0 then Exit;
  S2 := Copy(S, 1, I-1);
  Delete(S, 1, I+Length(CountryInfo.DateSep)-1);
  case CountryInfo.DateFmt of
    0: begin
         DT.Year := Stoi(S); if DT.Year < 80 then Inc(DT.Year, 2000)
                     else if DT.Year < 100 then Inc(DT.Year, 1900);
         DT.Day := Stoi(S2); if DT.Day > 31 then Exit;
         DT.Month := Stoi(S1); if DT.Month > 12 then Exit;
       end;
    1: begin
         DT.Year := Stoi(S); if DT.Year < 80 then Inc(DT.Year, 2000)
                             else if DT.Year < 100 then Inc(DT.Year, 1900);
         DT.Day := Stoi(S1); if DT.Day > 31 then Exit;
         DT.Month := Stoi(S2); if DT.Month > 12 then Exit;
       end;
    2: begin
         DT.Year := Stoi(S1); if DT.Year < 80 then Inc(DT.Year, 2000)
                             else if DT.Year < 100 then Inc(DT.Year, 1900);
         DT.Day := Stoi(S); if DT.Day > 31 then Exit;
         DT.Month := Stoi(S2); if DT.Month > 12 then Exit;
       end;
  end;
  PackTime(DT, M);
  ParseTime := M;
end;

        {-DataCompBoy-}
function FindFiles;
 var
     DateAfter, DateBefore, SizeGreat, SizeLess: LongInt;
     TotalNum: Longint; {-$VIV}
     CurP: Longint; {JO}
     C: Char;
     i: byte;
     Attr: Byte;
     FFResult: byte; {-$VOL}
     CancelSearch: Boolean;
     MaskSearch: Boolean;
     Drv: PFindDrive;
     Pnl: PView;
     T: TEventTimer;
     AType: PARJArchive; {JO}
     CurFileRec: PFileRec; {JO}
     FN: String;
     Dr: String;
     Nm: String;
     Xt: String;
     Mask: String;
     FF: string;

 procedure InitPanel;
 begin
  Pnl := Message(Desktop, evBroadcast, cmInsertDrive, Drv);
  if Pnl <> nil then PFilePanel(Pnl)^.ChangeLocked := True;
 end;

 procedure DispatchEvents;
  var Event: TEvent;
 begin
  if PInfo = nil then Exit;
  Application^.GetEvent(Event);
  if (Event.What = evCommand) and (Event.Command = cmCancel) and
     (Event.InfoPtr = PInfo^.But) then
      begin
       PInfo^.ClearEvent(Event);
       CancelSearch := MessageBox(GetString(dlQueryCancelSearch),nil,mfYesNoConfirm)=cmYes;
       SetTitle(PInfo^.Top); { Flash }
      end;
  if (Event.What <> evNothing) and not ((Event.What = evCommand) and (Event.Command = cmQuit))
     then Application^.HandleEvent(Event);
 end;

 procedure SearchData(Path: string);
  label Skip,
        NotArchive; {JO}
  var
      ArcTime: Longint;
      D: DateTime;
      PDir: PString;
      P: PFileRec;
      I: Byte;
      DirCol: PDirCol;
      XL: TXLat;
      LookInArchives: Boolean;
      ArcDirs: PCollection;
      PArcLastDir: PString;
      SR: lSearchRec;
      ArcPath: String;
      LDir, DrName: String;

 function SearchF: Boolean;
   var
       i: Longint;
       S: TBUFStream;
 begin
   Inc(TotalNum); {-$VIV}
   if PInfo <> nil then
    begin
     PInfo^.Write(2, Cut( Path+SR.FullName, 50 ));
     PInfo^.Write(3, GetString(dlLookedFiles) + ItoS(TotalNum)); {-$VIV}
    end;
   S.Init(Path+SR.FullName, stOpenRead, $4000);
{--- start -------- Eugeny Zvyagintzev ---------}
{Now DN will terminate File Find process more quick}
{when performing text search in files}
   If S.Status = stOK Then
    i:=FViewer.SearchFileStr(@S, XL, FindRec.What, 0,
                             FindRec.Options and ffoCaseSens <> 0,
                             False,
                             FindRec.Options and ffoWholeWords <> 0, False,
                             FindRec.Options and ffoAllCP <> 0,PInfo)
   Else
    i:=-1;

   CancelSearch:=(i = -2);
   SearchF:=(i >= 0 );

   {SearchF := (S.Status = stOK) and
              ( FViewer.SearchFileStr(@S, XL, FindRec.What, 0,
                                          FindRec.Options and ffoCaseSens <> 0,} {-$VIV 14.05.99}
                                          {Off,
                                          FindRec.Options and ffoWholeWords <> 0, False,
                                          FindRec.Options and ffoAllCP <> 0) >= 0 );} {-$VIV 14.05.99}
{--- finish -------- Eugeny Zvyagintzev ---------}
   S.Done;
 end;

 begin
  LookInArchives := (FindRec.Options and ffoInArch <> 0) and (FindRec.What = ''); {JO}
  NullXLAT(XL);
  if (length(Path) + length(Mask)) > (MaxPathLen) then {-$VOL} {DataCompBoy}
    begin FFResult := FFResult or ffSeD2Lng; Exit; end;
  if CancelSearch or not MemOK then Exit;
  New(DirCol,Init($10,$10));
  DirCol^.Insert(NewStr(Path));
  while DirCol^.Count>0 do
   begin
    if CancelSearch then break;
    Path:=PString(DirCol^.At(0))^;
    DirCol^.AtFree(0);
    PDir := nil;
    DosError := 0;
    if PInfo <> nil then PInfo^.Write(2, Cut(Path, 50));
    if (FindRec.What = '') then
    begin
      Inc(TotalNum); {-$VIV}
      if PInfo <> nil then
       PInfo^.Write(3, GetString(dlLookedDirs) + ItoS(TotalNum)); {-$VIV}
    end;

    if Path[Length(Path)] <> '|' then {JO}
      begin {начало поиска не в архиве}
       lFindFirst(Path + Mask, AnyFile, SR); {JO}

       While (DosError = 0) {and not LowMemory} and not CancelSearch and MemOK do
        begin
         if {$IFNDEF OS2}(SR.SR.Attr and VolumeID = 0) and {$ENDIF}
            (SR.SR.Name[1] <> '.') and
            (not Security or (SR.SR.Attr and Hidden = 0)) and
            (MaskSearch or InFilter(SR.FullName, FindRec.Mask+FindRec.AddChar)) and
            ((FindRec.Options and ffoAdvanced = 0) or (SR.SR.Time >= DateAfter)
              and (SR.SR.Time <= DateBefore) and (SR.FullSize >= SizeGreat) and
              (SR.FullSize <= SizeLess) and
              ((Attr = 0) or (SR.SR.Attr and Attr <> 0))) and
            ((FindRec.What='') or (SR.SR.Attr and Directory <> 0) and
             (FindRec.What='') or (FindRec.What<>'') and SearchF) then
          begin
           if Pos('\', Mask) > 0 then DrName := Path+GetPath(Mask) else DrName := Path; { Flash 21.01.2005 }
           if PDir = nil then PDir := NewStr(DrName);
           P := NewFileRec(SR.FullName, {$IFNDEF OS2}SR.SR.Name,{$ENDIF} SR.FullSize, SR.SR.Time, SR.SR.Attr, PDir);
           if Pnl = nil then InitPanel;
           if Pnl <> nil then Message(Pnl, evCommand, cmInsertFile, CopyFileRec(P));
           Files^.AtInsert(Files^.Count, P);
           if PInfo <> nil then
           PInfo^.Bottom := ItoS(Files^.Count) + FF;
           if TimerExpired(T) then
             begin
               DispatchEvents;
               if PInfo <> nil then PInfo^.DrawView;
               NewTimer(T, 1);
             end;
          end;
         if TimerExpired(T) then
           begin
             DispatchEvents;
             NewTimer(T, 1);
           end;
         DosError := 0;
         lFindNext(SR);
        end;
       lFindClose(SR);
      end {конец поиска не в архиве}
     else
{JO}
      begin {начало поиска в архиве}
        CtrlBreakHit := False;
        ArcPath := Copy(Path, 1, Length(Path) - 1);
        ArcTime := FileTime(ArcPath);
        New(ArcFile,Init(ArcPath, stOpenRead, 512));
        if (ArcFile = nil) or (ArcFile^.Status <> stOK) then Goto NotArchive;
        SkipSFX;
        AType := DetectArchive;
        if (AType = nil)
{$IFNDEF MINARCH}
          or (AType^.GetID in [arcAIN, arcUC2, arc7Z])
{$ENDIF}
          {временно! - надо решить проблему с лочкой dndosout.bat}
          {piwamoto: это у JO временно. а мы в DOSe}
            then Goto NotArchive;
        ArcDirs := New(PStringCollection, Init(30,30));
        repeat
          AType^.GetFile;
          if FileInfo.Last = 0 then
            begin
              Replace('/', '\', FileInfo.FName);
              with FileInfo do
                begin
                  if FName[1] <> '\' then FName := '\' + FName;
                  if FName[Length(FName)] = '\' then
                    begin
                      Dec(FName[0]);
                      Attr := Attr or Directory;
                    end;
                end;
              if (GetName(FileInfo.FName) <> '')
                and InFilter(GetName(FileInfo.FName), FindRec.Mask+FindRec.AddChar)
                and ((FindRec.Options and ffoAdvanced = 0) or (FileInfo.Date >= DateAfter)
                and (FileInfo.Date <= DateBefore) and (FileInfo.USize >= SizeGreat)
                and (FileInfo.USize <= SizeLess) and ((Attr = 0) or (FileInfo.Attr and Attr <> 0))) then
                  begin
                    if (FileInfo.Attr and Directory) <> 0 then
                     begin
                      if FileInfo.FName[Length(FileInfo.FName)] = '\' then
                       PArcLastDir := NewStr(UpStrg(FileInfo.FName))
                      else
                       PArcLastDir := NewStr(UpStrg(FileInfo.FName+'\'));
                     end;
                    if (((FileInfo.Attr and Directory) = 0) or
                        (ArcDirs^.IndexOf(PArcLastDir) = -1)) then
                      begin
                        PDir := NewStr(ArcPath+':'+GetPath(FileInfo.FName));
                        P := NewFileRec(GetName(FileInfo.FName),
                           {$IFNDEF OS2}GetName(FileInfo.FName),{$ENDIF}
                                        FileInfo.USize,
                                        FileInfo.Date,
                                        FileInfo.Attr,
                                        PDir);
                        if Directories^.IndexOf(PDir) = -1 then
                          Directories^.Insert(PDir);
                        if Pnl = nil then InitPanel;
                        if Pnl <> nil then Message(Pnl, evCommand,
                                              cmInsertFile, CopyFileRec(P));
                        Files^.AtInsert(Files^.Count, P);
                        if (FileInfo.Attr and Directory) <> 0 then
                          ArcDirs^.Insert(PArcLastDir);
                        if PInfo <> nil then
                        PInfo^.Bottom := ItoS(Files^.Count) + FF;
                        if TimerExpired(T) then
                          begin
                            DispatchEvents;
                            if PInfo <> nil then PInfo^.DrawView;
                            NewTimer(T, 1);
                          end;
                      end else DisposeStr(PArcLastDir);
                  end;
{добавляем каталоги которые присутствуют только в виде путей к файлам }
              LDir := GetPath(FileInfo.FName);
              if Length(LDir) > 2 then
                repeat
                  PArcLastDir := NewStr(UpStrg(LDir));
                  Dec(LDir[0]);
                  for I := Length(LDir) downto 1 do
                    if LDir[I] = '\' then Break;
                  DrName := Copy(LDir, I+1, Length(LDir));
                  LDir[0] := Char(I);
                  if (DrName <> '') and
                     (((FindRec.Options and ffoAdvanced) = 0) or
                      (Attr = 0) or ((Attr and Directory) <> 0)) and
                     InFilter(DrName, FindRec.Mask+ FindRec.AddChar) and
                     (ArcDirs^.IndexOf(PArcLastDir) = -1) then
                    begin
                      ArcDirs^.Insert(PArcLastDir);
                      PDir := NewStr(ArcPath+':'+Copy(LDir, 1, I));
                      P := NewFileRec(DrName,{$IFNDEF OS2}DrName,{$ENDIF}
                                       0,
                                       ArcTime,
                                       $80 or Directory,
                                       PDir);
                      if Directories^.IndexOf(PDir) = -1 then
                        Directories^.Insert(PDir);
                      if Pnl = nil then InitPanel;
                      if Pnl <> nil then Message(Pnl, evCommand,
                                cmInsertFile, CopyFileRec(P));
                      Files^.AtInsert(Files^.Count, P);
                      if PInfo <> nil then
                        PInfo^.Bottom := ItoS(Files^.Count) + FF;
                      if TimerExpired(T) then
                        begin
                          DispatchEvents;
                          if PInfo <> nil then PInfo^.DrawView;
                          NewTimer(T, 1);
                        end;
                    end else DisposeStr(PArcLastDir);
                until Length(LDir) <= 2;
{конец добавления каталогов которые присутствуют только в виде путей к файлам}
            end;
            if TimerExpired(T) then
              begin
                DispatchEvents;
                NewTimer(T, 1);
              end;
        until (FileInfo.Last>0) or CtrlBreakHit or CancelSearch or not MemOK;
        ArcDirs^.DeleteAll;
        Dispose(ArcDirs, Done);
        CtrlBreakHit := False;
NotArchive:
        FreeObject(ArcFile);
      end; {конец поиска в архиве}
{/JO}

    if not MemOK then Drv^.NoMemory := True;

    if TimerExpired(T) then
    begin
      DispatchEvents;
      NewTimer(T, 1);
    end;
    if ((FindRec.Options and ffoRecursive <> ffoRecursive) and
        not LookInArchives) or
        (Path[Length(Path)] = '|') then goto Skip;
    DosError := 0;
    lFindFirst(Path + x_x, AnyFile, SR); {JO}
    While (DosError = 0) and not LowMemory and not CancelSearch do
     begin
      if {$IFNDEF OS2}(SR.SR.Attr and VolumeID = 0) and{$ENDIF}
         (SR.SR.Name[1] <> '.') then
        if (SR.SR.Attr and Directory <> 0) then
          begin
           if (FindRec.Options and ffoRecursive <> 0) then
             DirCol^.Insert(NewStr(Path + SR.FullName + '\'))
          end
         else
{JO}
          if LookInArchives and (InExtFilter(SR.Fullname, Archives^)
             or InExtFilter(SR.Fullname, AddArchives))
{             or ((PosChar('.', SR.Fullname) = 0) and (SR.SR.Attr and SysFile = 0))}
               then DirCol^.Insert(NewStr(Path + SR.FullName + '|'));
{/JO}
      lFindNext(SR);
     end;
    lFindClose(SR);
Skip:
    if PDir <> nil then Directories^.Insert(PDir);
    DosError := 0;
    if PInfo <> nil then PInfo^.DrawView;
   end;
  Dispose(DirCol,Done); DirCol:=nil;
 end;

function SlowDrive(C: Char): Boolean;
begin
  case C of
     'C'..'Z' : SlowDrive := IsDriveCDROM(C);
    else SlowDrive := True;
  end;
end;

begin
 TotalNum := 0; {-$VIV}
 NewTimer(T, 0);
 FF := GetString(dlFilesFound);
 if FindRec.Options and ffoAdvanced <> 0 then
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
 CancelSearch := False; FFResult := ffSeFnd;
 SearchString.What := FindRec.What;
 SearchString.Opts := ((FindRec.Options and ffoCaseSens) shr 1) or
                      ((FindRec.Options and ffoWholeWords) shr 2) or
                      ((FindRec.Options and ffoAllCP) shr 2); {-$VIV 14.05.99}
 Microed.SearchData.Line := FindRec.What;
 Microed.SearchData.What := #0;
 Microed.SearchData.Options := SearchString.Opts;
 Microed.SearchData.Scope := 0;

 { Flash >>> }
 FN:=GetCurrentDir;
 if FN[Length(FN)] <> '\' then FN := FN + '\';
 if ExistDir(FN+GetPath(FindRec.Mask)) and (Pos('\',FindRec.Mask)<>0) then
  begin
  FN:=FN+GetPath(FindRec.Mask);
  while Pos('\',FindRec.Mask)<>0 do
   Delete(FindRec.Mask,1,Pos('\',FindRec.Mask));
  end;
 if FN[Length(FN)] <> '\' then FN := FN + '\';
 { Flash <<< }

 MaskSearch := not PackMask(FindRec.Mask+FindRec.AddChar, Mask, true);

 New(Drv, Init(GetString(dlFindPanel) + FindRec.Mask, Directories, Files, FreeByte));
 if FindRec.What<>'' then Drv^.AWhat:=NewStr(FindRec.What);
 Drv^.AMask := NewStr(FindRec.Mask);
 Drv^.isDisposable := False;
 Pnl := nil;

 case FindRec.Where of
  1: SearchData(FN);
  2: for C := 'A' to 'Z' do
         if ValidDrive(C) then
           if (C = UpCase(FN[1])) or (not SlowDrive(C)) then SearchData(C + ':\');
  else SearchData(GetRoot(FN));
 end;
 if not MemOK then Drv^.NoMemory := True;
 if Pnl <> nil then
  begin
    if Pnl <> nil then PFilePanel(Pnl)^.ChangeLocked := False;
    Drv^.isDisposable := True;
    CurFileRec :=
         PFilePanel(Pnl)^.Files^.At(PFilePanel(Pnl)^.ScrollBar^.Value); {JO}
    Dispose(PFilePanel(Pnl)^.Files, Done);
    PFilePanel(Pnl)^.Files := New(PFilesCollection, Init($10,$10));
    Application^.Redraw;
    Message(Pnl, evCommand, cmPanelReread, nil);
{JO: позиционируем фокус на файле, на котором он был до перечитывания панели}
    with PFilePanel(Pnl)^.Files^ do
      for CurP := 0 to Count - 1 do
       if (UpStrg(GetLFN(PFileRec(At(CurP))^.LFN)) = UpStrg(GetLFN(CurFileRec^.LFN))) and
          (PFileRec(At(CurP))^.Owner^ = CurFileRec^.Owner^)
         then PFilePanel(Pnl)^.ScrollBar^.SetValue(CurP);
{/JO}
  end;
 if (Files^.Count = 0) or (Pnl = nil) then
  begin
   Dispose(Drv,Done); Drv:=nil;
   FFResult := FFResult and ffSeNotFnd; {-$VOL}
  end
{ JO: здесь сортировка не нужна, т.к. она делается в TFindDrive.GetDirectory}
{     и в результате мы получаем сортировку дважды}
{else Files^.Sort}
  ;
 FindFiles := FFResult;
end;
        {-DataCompBoy-}

{AK155}
function InsertFile(const S: String; var DC: PSortedCollection;
          var FC: PFilesCollection): Boolean;
  var
    I: LongInt;
    Dr: PString;
    Duped: Boolean;
    SR: lSearchRec;
    Dir: String;
    Nm: String;
    Xt: String;
  begin
  InsertFile := false;
  if (S = '')
    or (S = '*') or (S = '*.*')   {JO: иначе в список попадёт весь каталог}
       then exit;
  ClrIO;
  lFindFirst(S, AnyFile, SR); {JO}
  if (DOSError = 0) and not Abort then
   begin
     InsertFile := true;
     lFSplit(lFExpand(S), Dir, Nm, Xt);
     if DC = nil then DC := New(PStringCollection, Init($10,$10));
     I := DC^.IndexOf(@Dir);
     if I < 0 then
       begin
         if Dir[Length(Dir)] <> '\' then AddStr(Dir, '\');
         Dr := NewStr(Dir);
         DC^.Insert(Dr);
       end else Dr := DC^.At(I);
   end;
  while (DOSError = 0) and not Abort do
   begin
     if{$IFNDEF OS2}(SR.SR.Name[1] <> '.') and (SR.FullName <> '.') and (SR.FullName <> '..'){$ELSE}(SR.SR.Name <> '..')
                                                              and (SR.SR.Name <> '.'){$ENDIF}then
      begin
        if FC = nil then New(FC, Init($10, $10));
        Duped := False;
        for I := 0 to FC^.Count - 1 do
          if (SR.FullName = GetLFN(PFileRec(FC^.At(I))^.LFN))
            and (Dr^ = PFileRec(FC^.At(I))^.Owner^) then
             Duped := True;
        if not Duped then
          FC^.AtInsert(FC^.Count, NewFileRec(SR.FullName,
                                             {$IFNDEF OS2}
                                             SR.SR.Name,
                                             {$ENDIF}
                                             SR.FullSize,
                                             SR.SR.Time,
                                             SR.SR.Attr,
                                             Dr
                                             )
                      )
         else Break;
      end;
     ClrIO;
     lFindNext(SR);
   end;
  lFindClose(SR);
{$IFDEF OS2}
  if DosError = 49 then messagebox(GetString(dl_CodePage_FS_Error), nil, mfError+mfOKButton);
{$ENDIF}
end;

        {-DataCompBoy-, AK155}
function ReadList(const AName: String; var DC: PSortedCollection;
       var FC: PFilesCollection): Boolean;
  var
      I,J,C: LongInt;
      D: DateTime;
      PInfo: PWhileView;
      TT: TEventTimer;
      R: TRect;
      P: PFileRec;
      F: PTextReader;
      S,FF: String;
begin
  ReadList := false;

  F := New(PTextReader, Init(AName));
  if F = nil then Exit;

  FC := nil; DC := nil;
{--- start -------- Eugeny Zvyagintzev ---- 11-05-2003 ----}
  R.Assign(1, 1, 30, 8); Inc(SkyEnabled);
  New(PInfo, Init(R)); PInfo^.Options := PInfo^.Options or ofSelectable or ofCentered;
  PInfo^.HelpCtx := hcMakeListFile;
  PInfo^.Write(1, Copy(GetString(dlReadingList), 4, 255));
  PInfo^.Bottom := GetString(dlNoFilesFound);
  FF := GetString(dlFilesFound);
  Desktop^.Insert(PInfo);
  NewTimer(TT, 0);
{--- finish -------- Eugeny Zvyagintzev ---- 11-05-2003 ----}
  while not F^.EOF and (IOResult = 0) and not Abort do
   begin
{--- start -------- Eugeny Zvyagintzev ---- 11-05-2003 ----}
     C:=0;
     If (FC <> Nil) And (FC^.Count <> 0) Then C:=FC^.Count;
     If (DC <> Nil) And (DC^.Count <> 0) Then C:=C+DC^.Count;
     If C <> 0 Then
      Begin
       PInfo^.Bottom:=ItoS(C)+FF;
       PInfo^.DrawView;
      End;
     if TimerExpired(TT) then
      begin
        DispatchEvents(PInfo,Abort);
        NewTimer(TT, 1);
      end;
     If Abort And
      (MessageBox(GetString(dlQueryAbort),Nil,mfYesNoConfirm)=cmYes) Then Break
     Else Abort:=False;
{--- finish -------- Eugeny Zvyagintzev ---- 11-05-2003 ----}
     S := F^.GetStr;
     if S <> '' then
       case S[1] of
        ' ', #9, '>':
         begin {игнорируем последующие строки многострочного описания}
         end;
        '"':
         begin { длинное имя в кавычках }
         system.delete (S, 1, 1);
         S[0] := Char(PosChar('"', S)-1);
         InsertFile(S, DC, FC);
         end;
        else {попробуем целиком, или без первого символа (BSO),
              а если не вышло - то до первого пробела или Tab}
         begin
         if not InsertFile(S, DC, FC) and
            not ((S[1] in ['#', '^']) and
                 InsertFile(Copy(S, 2, length(S)-1), DC, FC)) {BSO}
         then
           begin
           I := PosChar(' ',S); J := PosChar(#9,S);
           if (I=0) or ((J <> 0) and (J < I)) then
             I := J;
           if I <> 0 then
             InsertFile(Copy(S, 1, I-1), DC, FC);
           end;
         end;
       end {case};
   end;

  Dispose(F,Done);
{--- start -------- Eugeny Zvyagintzev ---- 11-05-2003 ----}
  Desktop^.Delete(PInfo); Dec(SkyEnabled);
  Dispose(PInfo,Done);
{--- finish -------- Eugeny Zvyagintzev ---- 11-05-2003 ----}
  if (FC = nil) or (FC^.Count = 0) or (DC = nil) or (DC^.Count = 0) then
    begin
      if FC <> nil then Dispose(FC,Done);
      if DC <> nil then Dispose(DC,Done);
      MessageBox(^C+GetString(dlNoFilesFound), nil, mfInformation+mfOKButton);
      Exit
    end;
  ReadList := true;
end;
        {-DataCompBoy-}


{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! TFindDrive }
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

{JO}
function GetArcName(S: String): String;
   Var C: Char;
begin
  if Length(S) < 2 then begin GetArcName := ''; Exit; end;
  C := S[2];
  S[2] := ';'; {JO: реально не важно на что меняем, лишь бы не ':'}
  S := Copy(S, 1, PosChar(':', S) - 1);
  if Length(S) > 1 then S[2] := C;
  GetArcName := S;
end;
{/JO}

        {-DataCompBoy-}
procedure TFindDrive.GetParam;
begin
 Param:=n;
 with ColumnsDefaultsFind do
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
constructor TFindDrive.Init;
 var
     I: LongInt;
     S: PString;
     SS: String;
begin
 TObject.Init;
 if num<1 then GetParam(1)
          else GetParam(num);
 innum:=num;
 AllowPack := True;
 ListFile := nil;
 isDisposable := True;
 DriveType := dtFind;
 CurDir := AName;
 Dirs := PSortedCollection(ADirs);
 Files := AFiles;
 lGetDir(0, SS); ClrIO;
 S := NewStr(SS);
 I := Dirs^.IndexOf(S);
 if I >= 0 then begin DisposeStr(S); S := Dirs^.At(I); end
           else Dirs^.Insert(S);
 NewUpFile;
 UpFile^.Owner := S;
end;
        {-DataCompBoy-}

procedure TFindDrive.NewUpFile;
begin
  UpFile := NewFileRec('..',{$IFNDEF OS2}'..',{$ENDIF}0,0,Directory,nil); {DataCompBoy}
end;

constructor TFindDrive.Load(var S: TStream);
 var I: LongInt;
     Q, Q2: LongInt;
begin
 inherited Load(S);
 AllowPack := True;
 isDisposable := True;
 S.Read(DriveType,SizeOf(DriveType));
 AMask:=S.ReadStr;
 AWhat:=S.ReadStr;
 Dirs := PSortedCollection(S.Get);
 S.Read(I, SizeOf(I)); if I < 0 then I := 0;
 if Dirs = nil then Fail;

 S.Read(Q, SizeOf(Q));
 If Q >= 0 then begin
  Files := New(PFilesCollection, Init(Q+1, $10));
  Files^.SortMode := psmOrdered;
  Files^.Duplicates := TypeOf(Self)=TypeOf(TFindDrive);
  for Q2:=0 to Q do
    begin
      Files^.AtInsert(Q2, LoadFileRecOwn(S, Dirs));
{     if PFileRec(Files^.At(Q2))^.Owner = nil then
        PFileRec(Files^.At(Q2))^.Owner := NewStr('---:---'); }
    end;
 end;

 ListFile := S.ReadStr;
 NewUpFile;
 UpFile^.Owner := Dirs^.At(I);
end;

procedure TFindDrive.Store(var S: TStream);
 var I: LongInt;
     Q: LongInt;
begin
  inherited Store(S);
  S.Write(DriveType,SizeOf(DriveType));
  S.WriteStr(AMask);
  S.WriteStr(AWhat);
  S.Put(Dirs);
  I := Dirs^.IndexOf(UpFile^.Owner);
  S.Write(I, SizeOf(I));
  If Files = Nil Then Q:=-1  {John_SW  22-03-2003}
  Else Q:=Files^.Count - 1;
  S.Write(Q, SizeOf(Q));

  for I := 0 to Q do StoreFileRecOwn(S, Files^.At(I), Dirs);
  S.WriteStr(ListFile);
end;

destructor TFindDrive.Done;
begin
 DisposeStr(AMask); DisposeStr(AWhat);
 if Files  <> nil then Dispose(Files, Done); Files:=nil;
 if UpFile <> nil then DelFileRec(UpFile);
 if Dirs   <> nil then Dispose(Dirs, Done);  Dirs:=nil;
 DisposeStr(ListFile);
 inherited Done;
end;

procedure TFindDrive.MakeDir;
begin
end;

procedure TFindDrive.lChDir;
begin
end;

        {-DataCompBoy-}
function TFindDrive.GetDirectory;
 label LLL;
 var
     I, N: LongInt;
     TotalLen: TSize;
     OW: Pointer;
     D: DateTime;
     PD: PString;
     AFiles: PFilesCollection;
     P, CR: PFileRec;
     AllFiles: Boolean;
     OOM: Boolean;
     SR: lSearchRec;
     DrNm: String;
     S: String;
{$IFDEF CHDIRDEBUG}
  ev: TEvent;
 const NumCal: word = 0;
       States:Array[0..3] of char = '|/-\';
       PanelState: PChar = Ptr($B800, 0);
       StartChar: Char = ' ';
{$ENDIF}

function CMatches: Boolean;
begin
  OOM := OOM or (MaxAvail < (AFiles^.Count+$200)*4);
  CMatches := not OOM;
end;

{$IFDEF CHDIRDEBUG}
begin
 PanelState:=Ptr(SegB800,0);
 StartChar:=PanelState^;
{$ELSE}
begin
{$ENDIF}
{$IFDEF CHDIRTIMER}
 if ShiftState and (kbLeftShift+kbRightShift) <> 0 then DDTimer:=Get100s;
{$ENDIF}
LLL:

 OOM := False;
 DOSError := 0;
 Abort := False; AllFiles := (FileMask = x_x) or (FileMask='*'); ClrIO;
 FreeSpace := ''; TotalInfo := ''; TotalLen := 0;

 AFiles := New(PFilesCollection, Init($10, $10));
 PFilesCollection(AFiles)^.Owner := Owner;
 AFiles^.Duplicates := TypeOf(Self)=TypeOf(TFindDrive);

 PFilesCollection(AFiles)^.SortMode := SortMode; S := ''; PD := nil;
 ClrIO;
 if Files <> nil then
  for N := 0 to Files^.Count-1 do if MemOK and CMatches then begin
{$IFDEF CHDIRDEBUG}
   PanelState^:=States[NumCal];
   inc(NumCal);
   if NumCal=4 then NumCal:=0;
{$ENDIF}
    CR := Files^.At(N);
    if cr=nil then continue;
    if (Cr^.Owner=nil)
     then OW := Dirs^.At(0)
     else if (S = CR^.Owner^)
           then OW := PD
           else if Dirs^.Search(CR^.Owner, I) then begin
                 PD := Dirs^.At(I);
                 OW := PD;
                 S := PD^
                end else OW := Dirs^.At(0);

   P := CopyFileRec(CR);

   if AllFiles or
      (P^.Attr and Directory <> 0) or InFilter(GetLFN(P^.LFN), FileMask)
    then with AFiles^ do AtInsert(Count, P)
    else begin DelFileRec(P); Continue end;
   TotalLen := TotalLen + P^.Size;
   if P^.Attr and Directory <> 0 then
    begin
       P^.Attr := P^.Attr and $7FFF;
       P^.Size := 0;
       if PanelFlags and fmiDirLen <> 0 then
        begin
          if SR.SR.Name[1] <> '.' then
             P^.Size := GetDirLen(MakeNormName(P^.Owner^, GetLFN(P^.LFN)));
          if Abort then begin PanelFlags := PanelFlags and (not fmiDirLen); Abort := False; end;
        end;
    end;
  end;

  if AFiles^.Count > 0 then
  begin
    if (OOM) or (not MemOK) then
    begin
      NoMemory := True;
      if AllowPack then
      begin
        I:=Max(AFiles^.Count-100, 0);
        Dispose(AFiles,Done);
        while I < Files^.Count do Files^.AtFree(I);
        goto LLL;
      end;
    end;
    AFiles^.Sort;
 end else
 begin
   if (OOM) or (not MemOK) then NoMemory := True;
 end;

 TotalInfo := CalcTotalInfo(AFiles^.Count, AFiles^.Count, TotalLen);

 if DriveType <> dtTemp then AFiles^.AtInsert(0, CopyFileRec(UpFile));

 GetDirectory := AFiles;
{$IFDEF CHDIRDEBUG}
 PanelState^:=StartChar;
{$ENDIF}
{$IFDEF CHDIRTIMER}
 if ShiftState and (kbLeftShift+kbRightShift) <> 0 then begin
  DDTimer:=Get100s-DDTimer;
  ev.what:=evCommand;
  ev.command:=cmShowTimeInfo;
  ev.infoptr:=nil;
  Application^.PutEvent(ev);
 end;
{$ENDIF}
end;
        {-DataCompBoy-}

function TFindDrive.CalcLengthWithoutName;
begin
 CalcLengthWithoutName := (10 + CountryInfo.TimeFmt) * Byte(Flags and psShowSize <> 0) +
                 9 * Byte(Flags and psShowDate <> 0) +
                (7 - CountryInfo.TimeFmt) * Byte(Flags and psShowTime <> 0);
end;

function TFindDrive.CalcLength;
 var B: Word;
begin
{$IFNDEF OS2}
 if Flags and psShowLongName <> 0 then
{$ENDIF}
           B := LFNLen + 1 + CalcLengthWithoutName +
                      252 * Byte(Flags and psShowDir <> 0)
{$IFNDEF OS2}
 else
           B := 13 + CalcLengthWithoutName +
                      252 * Byte(Flags and psShowDir <> 0)
{$ENDIF}
 ;
 if B > MaxViewWidth then B := MaxViewWidth;
 CalcLength := B;
end;

procedure TFindDrive.MakeTop;
 var Q: String;
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
 if Flags and psShowSize     <> 0 then S := S + Copy(GetString(dlTopSize),2-CountryInfo.TimeFmt, 255);
 if Flags and psShowDate     <> 0 then S := S + GetString(dlTopDate);
 if Flags and psShowTime     <> 0 then S := S + Copy(' '+GetString(dlTopTime), 1+CountryInfo.TimeFmt, 255);
 if (LFNLen >= 250) {$IFNDEF OS2} and (Flags and psShowLongName <> 0){$ENDIF}
         then S := S  + AddSpace(CenterStr(GetString(dlTopLFN), SizeX-Length(S)), LFNLen)
   else if Flags and psShowDir      <> 0 then
  begin
   Q := GetString(dlTopPath);
   S := S + Q + Strg(' ', 252-Length(Q)) {+ '~'#179'~'};
  end;
end;

procedure TFindDrive.GetFull;
 var X: AWord;
begin
  inherited GetFull(B, P, C, SC);
  if (LFNLen < 250){$IFNDEF OS2} or (Flags and psShowLongName = 0){$ENDIF} then
    begin
  {$IFNDEF OS2}if Flags and psShowLongName <> 0 then
  {$ENDIF} X := LFNLen+1 {$IFNDEF OS2} else X := 13{$ENDIF};
    end else X := 0;
  if Flags and psShowSize <> 0 then X := X + 11;
  if Flags and psShowDate <> 0 then Inc(X, 9);
  if Flags and psShowTime <> 0 then Inc(X, 6);
  if X >= 255 then Exit;
  if (Flags and psShowDir <> 0) and ((LFNLen < 250){$IFNDEF OS2} or (Flags and psShowLongName = 0){$ENDIF}) then
   begin
      C := C shr 8;
      MoveStr(TWordArray(B)[X], AddSpace({$IFDEF RecodeWhenDraw}
        CharToOemStr{$ENDIF} (P^.Owner^), MaxViewWidth - X - 1), C);
   {Inc(X, 80);
    TWordArray(B)[X-1]:=SC;}
   end;
end;

procedure TFindDrive.GetEmpty;
 var
  X: AWord;
begin
  inherited GetEmpty(B, SC);
  if (LFNLen < 250){$IFNDEF OS2} or (Flags and psShowLongName = 0){$ENDIF} then
    begin
  {$IFNDEF OS2}if Flags and psShowLongName <> 0 then
  {$ENDIF} X:=LFNLen+1 {$IFNDEF OS2}else X:=13{$ENDIF};
    end else X := 0;
  if Flags and psShowSize <> 0 then X := X + 11;
  if Flags and psShowDate <> 0 then Inc(X, 9);
  if Flags and psShowTime <> 0 then Inc(X, 6);
  if (X > 0) and (X < 255) then TWordArray(B)[X-1] := SC;
end;

procedure TFindDrive.ChangeUp;
 var P: PDrive;
begin
  if ListFile = nil then S := '' else S := GetName(ListFile^);
  if Owner = nil then Exit;
  if Prev = nil then
   begin
     New(Prev, Init(0, Owner, innum));
     if Prev = nil then Exit;
     GlobalMessage(evCommand, cmRereadInfo, nil);
   end;
  PDrive(PFilePanel(Owner)^.Drive) := PDrive(Prev);
  Prev^.lChDir(Prev^.CurDir);
  if (Prev^.DriveType = dtDisk) and
     (PView(Owner)^.GetState(sfSelected+sfActive)) then
      ActivePanel := Owner;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  Prev := nil;
  Dispose(PDrive(@Self), Done);
end;

procedure TFindDrive.ChangeRoot;
  var P: PDrive;
begin
  if DriveType = dtList then
    begin
      CurDir := '';
      Exit;
    end;
  if Prev <> nil then
    begin
      PDrive(PFilePanel(Owner)^.Drive) := PDrive(Prev);
      Prev^.ChangeRoot;
      GlobalMessage(evCommand, cmRereadInfo, nil);
      Prev := nil;
      Dispose(PDrive(@Self), Done);
      Exit;
    end;
  New(Prev, Init(0, Owner, innum));
  if Prev = nil then Exit;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  {Prev^.Owner := Owner;}
  {Prev^.Flags := PFilePanel(Owner)^.Drive^.Flags;}
  PDrive(PFilePanel(Owner)^.Drive) := PDrive(Prev);
  Prev^.ChangeRoot;
  if (PView(Owner)^.GetState(sfSelected+sfActive)) then
      ActivePanel := Owner;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  Prev := nil;
end;

function TFindDrive.isUp;
begin
 isUp := True;
end;

function TFindDrive.Disposable;
begin
 Disposable := isDisposable;
end;

procedure TFindDrive.RereadDirectory;
var PV: PView;
begin
{ if (Prev <> nil) or (Prev^.Prev = nil) then Exit;}
 PV := ReadingListMsg;
 ForceWriteShow(PV);
 if Prev <> nil then Prev^.RereadDirectory(S);
 if (DriveType <> dtArcFind) or not ExistFile(GetArcName(UpFile^.Owner^))
   then DosReread(Files);
 if PV<>nil then Dispose(PV,Done);
end;


function TFindDrive.GetRealName;
begin
 GetRealName := '';
end;

function TFindDrive.GetDir: string;
var
    SX: LongInt;
    S: String;
begin
 if ListFile<>nil then GetDir := GetString(dlListPanel)+ListFile^
 else if (Pos(GetString( dlBranch ), CurDir) = 1) or (CurDir=cTEMP_)
       then GetDir:=CurDir
 else begin
  S:=GetString(dlFindPanel);
  SX:=SizeX - Length(S) - 12;
  if (AMask<>nil) and (AWhat<>nil) and (AMask^<>x_x)
   then GetDir := S +
                  Cut(AMask^, ((SX div 3) shl 1)) + '|' +
                  Cut(AWhat^,  (SX - Min(((SX div 3) shl 1), Length(AMask^))))
   else
    if (AWhat<>nil) and ((AMask=nil) or (AMask^=x_x))
     then GetDir := S + Cut('*.*|' + AWhat^, SX)
     else if AMask<>nil then GetDir := S + Cut(AMask^, SX) else GetDir := S + '*.*';
 end;
end;

{JO: 20.06.2002 - возможен просмотр файла найденного в архиве}
procedure TFindDrive.UseFile;
 var
     C: Char;
     I: Byte;
     AType: PARJArchive;
     SS, S, S2, Q: String;
     Unp: String;
     OwnArc: String;
     PathInside: String;

begin
 if (DriveType in [dtFind, dtTemp]) and (P^.Owner <> nil) and
   PathFoundInArc(P^.Owner^) then
    begin {просмотр файла найденного в архиве}
      TempFile:='';
      if (Command = cmEditFile) or (Command = cmFileEdit) or
         (Command = cmIntEditFile) or (Command = cmIntFileEdit) then Exit;
    { определяем имя архиватора и путь внутри архива}
      OwnArc := P^.Owner^;
      OwnArc[2] := ';'; {JO: реально не важно, какой символ взять, лишь бы не ':'}
      I := PosChar(':', OwnArc);
      OwnArc :=  Copy(P^.Owner^, 1, I-1);
      PathInside := Copy(P^.Owner^, I+1, 255);
      if PathInside[1] = '\' then Delete(PathInside, 1, 1);
    { детектим тип архива}
      New(ArcFile,Init(OwnArc, stOpenRead, 512));
      if (ArcFile=nil) or (ArcFile^.Status <> stOK) then
        begin
          FreeObject(ArcFile); {Abort := True;} Exit;
        end;
      SkipSFX;
      AType := DetectArchive;
      FreeObject(ArcFile);
      if AType = nil then Exit;

      case Command of
{$IFDEF DBView}
       cmDBFView: C := '=';
{$ENDIF}
{$IFDEF SpreadSheet}
       cmWKZView: C := '>';
{$ENDIF}
       cmTextView: C := '<';
       cmHexView: C := '|';
       cmIntFileView: C := '-';
       else C := '+';
      end;
      S := ' ';
      if P^.Attr and Hidden <> 0 then
       begin
        S := '';
        if ExecResource( dlgSetPassword, S ) <> cmOK then Exit;
        S := ' ' + DelSpaces(CnvString(AType^.Garble) + S) + ' ';
       end;
     {$IFNDEF OS2}
      SS := MakeNormName(PathInside, GetLFN(P^.LFN));
      if SS[1] = '\' then Delete(SS, 1, 1);
      if AType^.UseLFN then S2 := OwnArc
        else S2 := lfGetShortFileName(OwnArc);
      if OwnArc[Length(OwnArc)]='.' then S2 := S2 + '.';
      S := CnvString(AType^.Extract)+' '+ s +
         SquashesName(S2) + ' ' + SquashesName(SS)+' ';
      DelDoubles('  ',S);
      TempFile := C + MakeNormName(TempDir , GetLFN(P^.LFN));
      Q:='|' + MakeNormName(P^.Owner^, GetLFN(P^.LFN));
     {$ELSE}
      SS := MakeNormName(PathInside, P^.Name);
      if SS[1] = '\' then Delete(SS, 1, 1);
       S := CnvString(AType^.Extract)+' '+s+SquashesName(OwnArc)+' '+SquashesName(SS)+' ';
      DelDoubles('  ',S);
      TempFile := C + MakeNormName(TempDir , P^.Name);
      Q:='|' + MakeNormName(P^.Owner^, P^.Name);
     {$ENDIF}
      if C in ['<','-','|','+'] then TempFile := TempFile + Q;
      S2:=Copy(TempFile, 2, 255);
      if not ExistFile(S2) or (PackedDate(P)<>FileTime(S2)) then
      begin
       Unp := CnvString(AType^.unPacker);
       S := Unp + ' ' + S;
       lGetDir(0, DirToChange);
       Advance2.lChDir(TempDir);
       Message(Application, evCommand, cmExecString, @S);
       Advance2.lChDir(DirToChange); DirToChange := '';
      end;
   {$IFDEF VIRTUALPASCAL}
      TempFileSWP := {$IFDEF RecodeWhenDraw}OemToCharStr{$ENDIF}(TempFile); {JO}
   {$ENDIF}
      TempFile:=''; {-$VOL}
   {$IFDEF VIRTUALPASCAL}
      Message(Application, evCommand, cmRetrieveSwp, nil); {JO}
   {$ENDIF}
    end {конец просмотра файла найденного в архиве}
  else
    begin
      if (Prev <> nil) and (Prev^.DriveType = dtArc) then
        Prev^.UseFile(P, Command)
       else
         begin
           if P^.Owner<>nil then S := MakeNormName(P^.Owner^, {$IFNDEF OS2}GetLFN(P^.LFN){$ELSE}P^.Name{$ENDIF});
           Message(Application, evCommand, Command, @S);
         end;
    end;


end;
{/JO}

procedure NewTemp;
begin
 if TempFiles = nil then New(TempFiles, Init($10, $10));
 TempFiles^.SortMode := psmOrdered;
 TempFiles^.Duplicates := False;
end;

        {-DataCompBoy-}
procedure CopyToTempDrive;
 var Info: PView;

 procedure AddRec(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
   var
       DT: DateTime;
       I, J: LongInt;
       l: Pointer;
       Nmm: String;
 begin
   UpdateWriteView(Info);
   if TempFiles^.Search(P, J) then exit;
   Nmm:= GetLFN(P^.LFN);
   I  := TempDirs^.IndexOf(P^.Owner);
   if I >= 0 then L := TempDirs^.At(I)
     else
       begin
        if ArchiveName <> '' then
          begin
            if (P^.Owner^ = '') then L := NewStr(ArchiveName + ':\')
              else L := NewStr(ArchiveName + ':' + P^.Owner^);
          end
         else
          L := NewStr(P^.Owner^);
         TempDirs^.Insert(L);
       end;
   TempFiles^.AtInsert(J, NewFileRec(Nmm,
                                     {$IFNDEF OS2}MakeFileName(P^.Name),{$ENDIF}
                                     P^.Size,
                                     PackedDate(P),
                                     P^.Attr,
                                     L
                                    )
                      );
 end;

begin
 if TempDirs = nil then TempDirs := New(PStringCollection, Init(10, 10));
 if TempFiles = nil then NewTemp;
 Info := WriteMsg(GetString(dlPleaseStandBy));
 AFiles^.ForEach(@AddRec);
 Drives.RereadDirectory(cTEMP_);
 Dispose(Info,Done);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TTempDrive.GetParam;
begin
 Param:=n;
 with ColumnsDefaultsTemp do
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

constructor TTempDrive.Init;
  var S: PString;
{John_SW 02-03-2003}
{We have to clear slected flag when returning to TempDrive}
 Procedure UnSelect(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  Begin
   P^.Selected:=False;
  End;
begin
 TObject.Init;
 if num<1 then GetParam(1)
          else GetParam(num);
 innum:=num;
 if TempDirs = nil then TempDirs := New(PStringCollection, Init(10, 10));
 if TempFiles = nil then NewTemp;
 TempFiles^.ForEach(@Unselect); {John_SW 02-03-2003}
 isDisposable := True;
 Dirs := TempDirs;
 Files := TempFiles;
 CurDir := cTEMP_;
 DriveType := dtTemp;
 ListFile := nil;
 System.GetDir(0, FreeStr); ClrIO;
 S := NewStr(FreeStr);
 NewUpFile;
 UpFile^.Owner := S; {DataCompBoy}
 UpFile^.OwnerDisposible:=true;
end;

constructor TTempDrive.Load(var S: TStream);
begin
 TDrive.Load(S);
 if TempDirs = nil then TempDirs := New(PStringCollection, Init(10, 10));
 if TempFiles = nil then NewTemp;
 isDisposable := True;
 Dirs  := TempDirs;
 Files := TempFiles;
 CurDir := cTEMP_;
 DriveType := dtTemp;
 ListFile := nil;
 NewUpFile;
 UpFile^.Owner := S.ReadStr;
 UpFile^.OwnerDisposible:=true;
end;

procedure TTempDrive.Store;
 var I: LongInt;
     Q: LongInt;
begin
  TDrive.Store(S);
  S.WriteStr(UpFile^.Owner)
end;

procedure TTempDrive.CopyFilesInto;
begin
 CopyToTempDrive(AFiles, Own, '');
end;

procedure TTempDrive.EraseFiles;
 procedure DoErase(P: PFileRec); {$IFDEF BIT_16}Far;{$ENDIF}
 var I: LongInt;
 begin
  I := 0;
  if (P<>nil) and
     (Files^.Search(P, I))
   then Files^.AtFree(I);
 end;
begin
 AFiles^.ForEach(@DoErase);
 Drives.RereadDirectory(cTEMP_);
end;

function TTempDrive.GetRealName;
begin
 GetRealName := cTEMP_;
end;

destructor TTempDrive.Done;
begin
 if UpFile <> nil then DelFileRec(UpFile);
 TDrive.Done;
end;

procedure TFindDrive.GetFreeSpace;
begin
  S := '';
end;

function TFindDrive.GetFullFlags;
begin
 GetFullFlags := psShowSize + psShowDate + psShowTime + psShowDir;
end;

        {-DataCompBoy-}
constructor TFindDrive.InitList;
  var
      FC: PSortedCollection;
      DC: PFilesCollection;
begin
  if not ReadList(AName, FC, DC) then Fail;
  Init(AName, PCollection(FC), DC, FreeByte);
  ListFile := NewStr(AName);
  DriveType := dtList;
  AddToDirectoryHistory(ListFile^, DriveType);
end;
        {-DataCompBoy-}

procedure TTempDrive.CopyFiles;
var B: Boolean;
begin
 if ReflectCopyDirection
  then RevertBar:=Message(DeskTop, evBroadCast, cmIsRightPanel, Own) <> Nil
  else RevertBar:=false;
 inherited CopyFiles(AFiles, Own, MoveMode);
end;

{JO}  { Flash 03-02-2003 >>> }
procedure TFindDrive.CopyFromArc;
 Var
     I, ListCounter: LongInt;
     FCCur: PFilesCollection;
     FR: PFileRec;
     Drv: PDrive;
     PI: PView;
     Pswd: Boolean;
     DT: Record S: String; W: Word; Psw: String[30]; end;
     F1,F2: lText;
     CurArcName,ExtrChar: String;
     BatchFile,NewName: String;
     S,SS,Nm,Xt: String;

  function GetArcOwn(S: String): String;
  begin
    if Length(S) < 2 then begin GetArcOwn := ''; Exit; end;
    S[2] := ';'; {JO: реально не важно на что меняем, лишь бы не ':'}
    GetArcOwn := Copy(S, PosChar(':', S) + 1, 255);
  end;

  procedure IsPswd(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF} {JO}
  begin
   Pswd := Pswd or (P^.Attr and Hidden <> 0);
  end;

  procedure UnSelect(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  begin
   Message(Owner, evCommand, cmCopyUnselect, P);
  end;

begin
{JO: выводим диалог разархивирования, общий для всех архивов}
   {ExtrDir := '';}
   DT.S := '';
   DT.Psw := '';
   ListCounter:=0;
{   DT.W := UnarchiveOpt and not 2;} {JO}
   DT.W := 0;{piwamoto: don't use paths}
   Message(Application, evCommand, cmPushFirstName, @DT.S);
   if CopyDirName <> '' then DT.S := CopyDirName else
   if ExtrDir <> '' then DT.S := ExtrDir;
   if DT.S = '' then GlobalMessageL(evCommand, cmPushName, hsExtract);
   if DT.S = '' then DT.S := HistoryStr(hsExtract, 0);
   CopyDirName := '';
   if not SkipCopyDialog then
      if ExecResource( dlgExtract, DT) <> cmOK then Exit;
{
   if ((DT.W and 1) <> (UnarchiveOpt and 1)) or
      ((DT.W and 4) <> (UnarchiveOpt and 4)) then ConfigModified := True;
   UnarchiveOpt := DT.W and not 2;
}
   ConfigModified := True;
{}
   SkipCopyDialog := false;
   if DT.S = cTEMP_ then
     begin
      CopyToTempDrive(AFiles, Own, '');
      Exit;
     end;
   ExtrDir := DT.S;

    PI := WriteMsg(GetString(dlReadingList));
    BatchFile := SwpDir+'$DN'+ItoS(DNNumber)+'$'+BatchExtension;
    lAssignText(F1, BatchFile);
    lRewriteText(F1);

{JO: формируем файловые коллекции для каждого архива и разархивируем файлы из архивов}
   repeat
     I := 0;
     FR := AFiles^.At(0);
{
   // для файлов с разными путями внутри архива запускаем архиватор отдельно,
   // иначе они будут распакованы с созданием подкаталогов, а нам это не надо
}
     S := FR^.Owner^;
     CurArcName := UpStrg(S);
     New(FCCur, Init($10,$10));
     repeat
       FR := AFiles^.At(I);
       if (FR <> nil) and (FR^.Owner <> nil) and
          (UpStrg(FR^.Owner^) = CurArcName) then
         begin
           FCCur^.AtInsert(FCCur^.Count, FR);
           AFiles^.AtDelete(I);
         end
        else Inc(I);
     until I >= AFiles^.Count;
     CurArcName := S;
     if FCCur^.Count > 0 then
       begin
        Drv := New(PArcDrive,Init(GetArcName(CurArcName),
                                  GetArcName(CurArcName), 1));
        if Drv <> nil then
          begin
            Drv^.Owner := Owner; {дабы обеспечить снятие выделения в панели}
            Drv^.lChDir(GetArcOwn(CurArcName));
            PArcDrive(Drv)^.Password := DT.Psw;

            While ExtrDir[Length(ExtrDir)] = ' ' do Dec(ExtrDir[0]);
            if (ExtrDir = '') or (ExtrDir = '..') then
            {$IFNDEF OS2}
             if PArcDrive(Drv)^.AType^.UseLFN then
            {$ENDIF}
              lFSplit(PArcDrive(Drv)^.VArcName, ExtrDir, Nm, Xt) {JO: для распаковки по F4 архивов, просмотренных через фильтр}
            {$IFNDEF OS2}
             else lFSplit(lfGetShortFileName(PArcDrive(Drv)^.ArcName), ExtrDir, Nm, Xt)
            {$ENDIF}
             ;

            if not (ExtrDir[Length(ExtrDir)] in ['\','/']) then ExtrDir := ExtrDir + '\';
            repeat   {JO}
             SS := PArcDrive(Drv)^.MakeListFile(FCCur); S := ' '; Pswd := False;
             NewName := SwpDir + Hex2(DNNumber) + Copy(Hex8(ListCounter),3,6) + '.$DN';
             lAssignText(F2, Copy(SS,2,Length(SS)-1));
             lRenameText(F2, NewName);
             Inc(ListCounter);
             SS := '@'+NewName;
             FCCur^.ForEach(@IsPswd);
             if not Uncompleted then FCCur^.ForEach(@Unselect);
             ExtrChar := CnvString(PArcDrive(Drv)^.AType^.ExtractWP);
             if DT.W and 1 = 0 then ExtrChar := CnvString(PArcDrive(Drv)^.AType^.Extract);
             if DT.W and 2 <> 0 then ExtrChar := CnvString(PArcDrive(Drv)^.AType^.Test);
             if (PArcDrive(Drv)^.Password = '') and Pswd then
             if ExecResource(dlgSetPassword, PArcDrive(Drv)^.Password) <> cmOK then Exit;
             if PArcDrive(Drv)^.Password <> '' then
             S := ' ' + DelSpaces(CnvString(PArcDrive(Drv)^.AType^.Garble) + PArcDrive(Drv)^.Password) + ' ';
             {$IFNDEF OS2}
             if not PArcDrive(Drv)^.AType^.UseLFN then
              PArcDrive(Drv)^.ArcName := lfGetShortFileName(PArcDrive(Drv)^.ArcName);
             {$ENDIF}
             S := ExtrChar + ' ' +s+ SquashesName(PArcDrive(Drv)^.ArcName);
             if (Xt = '.') and (S[length(S)] <> '.')
              then S := S + '.';{piwamoto: extracting from extensionless archives}
             S := S +' '+SS+' ';

             lGetDir(0, DirToChange);
             CreateDirInheritance(ExtrDir, True);
             Advance2.lChDir(ExtrDir);
             DelDoubles('  ', S);

             WriteLn(F1.T, '@'+CnvString(PArcDrive(Drv)^.AType^.unPacker)+' '+S);
             WriteLn(F1.T, '@del '+NewName);
            until not Uncompleted; {JO}
            StopPoint := 0; {JO}
            Dispose(Drv, Done);
          end;
       end;
     FCCur^.DeleteAll;
     Dispose(FCCur, Done);
   until AFiles^.Count = 0;
   Write(F1.T, '@');
   Close(F1.T);
   PI^.Free;
   ExecString(@BatchFile,'');
end;
{/JO} { Flash 03-02-2003 <<< }

{JO}
procedure TFindDrive.CopyFiles;
  Var FC_Disk, FC_Arc: PFilesCollection;
      PI: PView;

{
// JO: разделяем коллекцию файлов в панели поиска на две: в одну помещаем
//     файлы, которые лежат на диске, в другую - которые лежет в архивах
}
  procedure SeparateCollections(FC_Comm: PFilesCollection;
                          Var FC_Disk, FC_Arc: PFilesCollection);
    Var I: Longint;
        FR: PFileRec;
  begin
   FC_Disk := nil; FC_Arc := nil;
   if (FC_Comm = nil) or (FC_Comm^.Count = 0) then Exit;
   New(FC_Disk, Init($10,$10)); New(FC_Arc,  Init($10,$10));
   for I := 0 to FC_Comm^.Count-1 do
     begin
       FR := FC_Comm^.At(I);
       if (FR^.Owner <> nil) and PathFoundInArc(FR^.Owner^) then
         FC_Arc^.AtInsert(FC_Arc^.Count, FR)
        else
         FC_Disk^.AtInsert(FC_Disk^.Count, FR);
     end;
   if FC_Arc^.Count = 0 then begin Dispose(FC_Arc, Done); FC_Arc := nil; end;
   if FC_Disk^.Count = 0 then begin Dispose(FC_Disk, Done); FC_Disk := nil; end;
  end;

begin
 PI := WriteMsg(GetString(dlReadingList)); { Flash 03-02-2003 }
 SeparateCollections(PFilesCollection(AFiles), FC_Disk, FC_Arc);
{ Flash 03-02-2003 >>> }
 if FC_Arc <> nil then
   begin
     Message(Application, evCommand, cmPushFirstName, @ExtrDir);
     if ExtrDir = '' then GlobalMessageL(evCommand, cmPushName, hsExtract);
     if ExtrDir = '' then ExtrDir := HistoryStr(hsExtract, 0);
   end;
 PI^.Free;
{ Flash 03-02-2003 <<< }
 if FC_Disk <> nil then
   begin
     if not MoveMode then CopyFromTemp := True;
     if Prev <> nil then Prev^.CopyFiles(FC_Disk, Own, MoveMode);
     CopyFromTemp := False;
     FC_Disk^.DeleteAll;
     Dispose(FC_Disk,Done);
   end;
 if FC_Arc <> nil then
   begin
     CopyFromArc(FC_Arc, Own);
     FC_Arc^.DeleteAll;
     Dispose(FC_Arc,Done);
   end;
end;
{/JO}

procedure TFindDrive.CopyFilesInto;
begin
end;

procedure TFindDrive.EraseFiles;
begin
 if Prev <> nil then Prev^.EraseFiles(AFiles);
end;

procedure TFindDrive.GetDirInfo;
  var Fl: Integer;
      Sz: TSize;
      S1,S2: String[40];

        {-DataCompBoy-}
  procedure DoCount(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  begin
    if (P <> nil) and (P^.Attr and Directory = 0) then
      begin
        Inc(Fl);
        Sz:=Sz+P^.Size;
      end;
  end;
        {-DataCompBoy-}

begin
  B.Title := NewStr( GetString( dlDIFileFind ));
  B.Dir := NewStr(GetString( dlFileMask )+Copy(CurDir, PosChar(':', CurDir)+1, 255)); {DataCompBoy}
  Fl := 0; Sz := 0;
  if Files <> nil then Files^.ForEach(@DoCount);

  if Fl = 0 then B.Files := NewStr(GetString(dlDINoFiles))
   else
    begin
     if Fl = 1 then S1 := GetString(dlDIFile) else S1 := GetString(dlDIFiles);
     if Sz = 1 then S2 := GetString(dlDIByte) else S2 := GetString(dlDIBytes);
     B.Files := NewStr('~'+FStr(Fl)+'~ '+S1+GetString(dlDIWith)+'~'+FStr(Sz)+'~ '+S2);
    end;

end;


procedure TFindDrive.HandleCommand(Command: Word; InfoPtr: Pointer);
begin
 if (Prev <> nil) and (Prev^.DriveType = dtArc) and
    ((Command = cmSetPassword) or
     (Command = cmArcTest) or
     (Command = cmExtractTo)) then
     Prev^.HandleCommand(Command, InfoPtr);
end;


function TFindDrive.OpenDirectory(const Dir: String): PDrive;
  begin OpenDirectory := nil; end;


procedure TTempDrive.GetDirInfo;
  var Fl: LongInt;
      Sz: TSize;
      S1,S2: String[40];

        {-DataCompBoy-}
  procedure DoCount(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  begin
    if (P <> nil) and (P^.Attr and Directory = 0) then
      begin
        Inc(Fl);
        Sz:=Sz+P^.Size;
      end;
  end;
        {-DataCompBoy-}

begin
  B.Title := NewStr( GetString( dlDITemporary ));
  B.Dir := NewStr(cTEMP_);
  Fl := 0; Sz := 0;

  if Files <> nil then Files^.ForEach(@DoCount);

  if Fl = 0 then B.Files := NewStr(GetString(dlDINoFiles))
   else
    begin
     if Fl = 1 then S1 := GetString(dlDIFile) else S1 := GetString(dlDIFiles);
     if Sz = 1 then S2 := GetString(dlDIByte) else S2 := GetString(dlDIBytes);
     B.Files := NewStr('~'+FStr(Fl)+'~ '+S1+GetString(dlDIWith)+'~'+FStr(Sz)+'~ '+S2);
    end;
end;

procedure TTempDrive.ChangeRoot;
begin
end;

end.
