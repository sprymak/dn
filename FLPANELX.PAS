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
//  dn16rc1-Shift_Del_erases_file_only_if_DEL_erases_files_switched_on.patch
//  dn16rc1-sort_by_description_and_archives_before_exe.patch
//  dn16rc1-Ctrl_Alt_Shift_Enter_bugfix-diff149byMV.patch
//  dn16rc1-Ctrl-N_hotkey_used_for_long-short_names_switching.patch
//  dn16rc1-Don't_remove_root_by_Ctrl-Del.patch
//  dn16rc1-vp_noasm_compatible.patch
//
//  2.0.0
//  dn200-files_panel_bugs_fix.patch
//
//  2.3.0
//  dn230-file_panel_left_right_moving_fix.patch
//  dn230-files_panel_bugs_fix.patch
//  dn230-esc_works_when_cmdline_disabled.patch
//  dn230-files_panel_bugs_fix_1.patch
//  dn230-remove_GZip_compression_and_change_external_filter_view.patch
//  dn230-ctrl_ins_fix_incorrect_filename_case.patch
//  dn2522_manual_save_desktop.patch
//  dn230_force_mouse_scroll.patch
//
//  2.7.0
//  dn270-quicksearch_fix.patch
//  dn270-search_in_archives.patch
//  dn281-DirHistoryMenu_default_item.patch
//  dn2825-new_logic_for_viewers.patch
//  dn270-UUdecode_fix-ak20912a.patch
//  dn2911-change_drive_fix.patch
//  dn2911-command_line_improve.patch
//  dn270-directory_changing_from_archive_panel-jo21107a.patch
//  dn21029-Goto_File_from_Find_Drive-jo21113b.patch
//  dn21116-directory_changing_from_archive_panel-jo21121a.patch
//  dn21116-Ctrl_Slash_Up_Down_fix.patch
//  dn21116-histories_fix-jo21112a.patch
//  dn21128-show_LFN_and_desc_in_bottom.patch
//  dn21128-Ctrl-R_fix-jo21116b.patch
//  dn21128-archive_name_depends_on_Use_LFN-jo20604a.patch
//  dn21202-fix_for_dn21116-Ctrl_Slash_Up_Down_fix.patch
//  dn21202-Find_file_and_branch_in_archive.patch
//  dn21225-file-panel(f)-change_drive_fix.patch
//  dn21225-FilePanel(fi)-lfn_and_description_show_on_divider.patch
//  dn328-FilePanel(f)-reread_directory_fix.patch
//  dn328-FilePanel(f)-CD_command_on_network_drives_fix.patch
//  dn3216-fpanel(n)-use_Ctrl-Shift-Del_to_hide_one_file.patch
//  dn3216-FilePanel(f)-copy_file_from_panel_to_clipboard_fix.patch
//  dn3216-FilePanel(f)-change_folder_fix.patch
//  dn3331-Arvid_bugfix_and_TDR_detection.patch
//  dn3331-FileFindPanel(f)-reread_directory_fix.patch
//  dn3331-fpanel(f)-reread_find_panel_after_exiting_archive.patch
//  dn3421-FilePanel(f)_change_drive_fix.patch
//  dn3421-FilePanel(f)_loading_current_file_fix.patch
//  dn3421-Macros(f)-empty_or_network_drive_fix.patch
//
//  3.7.0
//  dn370-ChangeDrive(f)-russian_language_fix.patch
//  dn370-change_dirname_started_with_a_dot.patch
//  dn31005-bp_to_vp_on_off_true_false.patch
//  dn31220-Kernel(f)-panel_setups_works_in_VP.patch
//  dn40307-define_DN_Micro.patch
//
//  4.9.0
//  dn40900-save_cursor_position.patch
//  dn50208-cleanup.patch
//  dn40900-temp_drive_in_directories_history.patch
//
//  5.9.0
//  dn50900-GotoSingle-AK60213C.patch
//
//  6.4.0
//
//////////////////////////////////////////////////////////////////////////}

{$I STDEFINE.INC}

UNIT
  FLPanelX;


INTERFACE

USES
  Dos, LFN, LFNCol, {DataCompBoy} advance, advance1, advance2, advance3,
  Objects, Views, Drivers, FilesCol, HideView, Drives, filediz, collect, Arvid;

TYPE
  PFilePanelRoot = ^TFilePanelRoot;
  TFilePanelRoot = object(THideView)
   isValid, MSelect, SelectFlag, Loaded, ChangeLocked: Boolean;
   InfoView, DirView, DriveLine: PHideView;
   Delta, OldDelta, OldPos, DeltaX: LongInt;
   Files: PFilesCollection;
   SortMode: Byte;
   DirectoryName, OldDirectory: String;
   FileMask: String;
   SearchParam: TQuickSearchData; {DataCompBoy}
   ScrollBar: PMyScrollBar;
   DrawDisableLvl, SelNum, LineLength: LongInt;
   SelectedLen, PackedLen: TSize;
   WasActive, PosChanged, CommandEnabling,
   {QuickSearch,} ViewEnabled: Boolean;
   TotalInfo, FreeSpace: String[50];
   PanelFlags: Word;
   LastDriveFlags: Word;
   Drive: PDrive;
   ForceReading: Boolean;
   DriveState: Word;
   LastCurPos: TPoint;
   BadDrv: Boolean;

   constructor Init(var Bounds: TRect; ADrive: Integer; AScrBar: PMyScrollBar);
   constructor Load(var S: TStream);
   procedure Store(var S: TStream);
   destructor Done; virtual;
   procedure Awaken; virtual;
   procedure ChDirName;
   procedure ShowView; virtual;
   procedure HideView; virtual;
   procedure CommandHandle(var Event: TEvent);
   procedure ChangeBounds(var Bounds: TRect); virtual;
   function  Valid(Command: Word): Boolean; virtual;
   procedure GetUserParams(var FileRec: PFileRec; var List: String; BuildList: boolean);
   procedure ReadDirectory;
   procedure RereadDir;
   procedure SetDirName; virtual;
   procedure SendLocated;
   procedure IncDrawDisabled;
   procedure DecDrawDisabled;
   procedure ChkNoMem;
   procedure ChDir(Dir: string);
  end;

{                                                                        }
{ WARNING: The following vars are mirrored in FLPANEL.PAS via ABSOLUTEs! }
{                                                                        }

CONST
  ActivePanel: Pointer = NIL;
  CtrlWas: Boolean = False;
  DirsToChange: Array [0..9] of PString = (nil,nil,nil,nil,nil,nil,nil,nil,nil,nil);
  CurFileActive: PString = nil;
  CurFilePassive: PString = nil;
  QuickSearch: boolean = false; {AK155 5-01-2002. До этого было такое поле
в TFilePanelRoot и в Tree.TTreeView. Поскольку по логике использования
QuickSearch может быть только один, естественным решением является
глобальная переменная, а от ее размазывания по объектам ничего, кроме
глюков быть не может. Поводом для превращения этих поле в глобальную
переменную послужило желание проанализировать ее в TCommandLine.Update }


VAR
  PShootState: Word;
  CurrentDirectory: String;


IMPLEMENTATION

uses
  Messages, DnApp, DnHelp, Startup, Commands, Histries, HistList, FlTools,
  FileFind, CmdLine, ArcView, Archiver, DiskImg, DiskInfo, FileCopy, DnUtil,
  dnini {-$VIV}
  {$IFDEF MODEM}{$IFDEF LINK}, NavyLink{$ENDIF}{$ENDIF}
{$IFDEF UUENCODE},UUCode{$ELSE}
{$IFDEF UUDECODE},UUCode{$ENDIF}{$ENDIF}
  , flpanel;

procedure TFilePanelRoot.IncDrawDisabled;
begin
  Inc(DrawDisableLvl);
end;

procedure TFilePanelRoot.DecDrawDisabled;
begin
  Dec(DrawDisableLvl);
  ChkNoMem;
end;

procedure TFilePanelRoot.ChkNoMem;
begin
  if Drive^.NoMemory and (DrawDisableLvl = 0) then
  begin
    Application^.OutOfMemory;
    Drive^.NoMemory := False;
  end;
end;

constructor TFilePanelRoot.Init;
begin
 inherited Init(Bounds);
 ForceReading := False;
 CommandEnabling := True;
 ChangeLocked := False;
 Abort := False;
 HelpCtx := hcFilePanel;
 GrowMode := gfGrowHiY + gfGrowHiX;
 Options := Options or ofSelectable or ofTopSelect or ofFirstClick;
 PanelFlags := PanelDefaults.Show;

 EventMask := $FFFF;

 ScrollBar := AScrBar; Loaded := False; OldDelta := -1; PosChanged := False;
 isValid := True;

 if (ADrive <= 0) or not ValidDrive(Char(ADrive+64)) then ADrive := 0;
 Drive := New(PDrive, Init(ADrive, @Self, 0));
 DeltaX := 0;

 if Abort then begin IsValid := False; Exit end;
 {GetDir(ADrive, DirectoryName);}

 {Drive^.Owner := @Self;}
 FileMask := x_x; Files := nil;

 if PanelDefaults.Sort < NumSortModes-1 then SortMode := PanelDefaults.Sort+1 else
 if PanelDefaults.Sort = NumSortModes-1 then SortMode := psmOrdered
                           else SortMode := 0;
 IncDrawDisabled; MSelect := False;
 QuickSearch := False; ViewEnabled := False;
 if not Abort then ReadDirectory;
 isValid := isValid and not Abort; DecDrawDisabled;
end;

constructor TFilePanelRoot.Load;
 var I: LongInt;
     dumm: array[1..20] of byte;
begin
 inherited Load(S);
 ChangeLocked := False;
 GetPeerViewPtr(S, ScrollBar);
 GetPeerViewPtr(S, DirView);
 GetPeerViewPtr(S, InfoView);
 GetPeerViewPtr(S, DriveLine);

{--- start -------- Eugeny Zvyagintzev ---- 27-03-2002 ----}
 Drives.DriveLoadingError:=False;
 Drive := PDrive(S.Get);
 BadDrv:=Drives.DriveLoadingError;
{--- finish -------- Eugeny Zvyagintzev ---------}

 if Drive = nil then New(Drive, Init(0, @Self, 0)) else Drive^.Owner:=@Self;
 DeltaX := 0;
 Drive^.RereadDirectory('');
 if Drive <> nil then begin
  Drive^.SizeX := Size.X;
  DirectoryName := Drive^.GetDir;
 end;
 S.Read(FileMask[0], 1); S.Read(FileMask[1], Length(FileMask));
 S.Read(SortMode, 1);
 S.Read(PanelFlags, 2);
{--- start -------- Eugeny Zvyagintzev ---- 20-01-2003 ----}
 If (CfgVer <> 0) And (CfgVer < 30200) Then
  Begin
   If ShowLongName > 0 Then
    PanelFlags:=(PanelFlags Or fmiFullNameInBottom) Or
                (PanelFlags And (Not fmiFullNameInDivider))
   Else
    PanelFlags:=PanelFlags Or
                (PanelFlags And (Not fmiFullNameInBottom)) Or
                (PanelFlags And (Not fmiFullNameInDivider));
   If (PanelFlags And fmiSelectedInBottom) <> fmiSelectedInBottom Then
   begin
    PanelFlags:=PanelFlags Or fmiSelectedInDivider; end;
  End;
{--- finish -------- Eugeny Zvyagintzev ---- 20-01-2003 ----}
 S.Read(Delta, 2); OldDelta := -1; PosChanged := False;
 S.Read(ForceReading, 1);
 If (Drive <> Nil) And (Not BadDrv) Then Begin
  S.Read(Drive^.LFNLen,    SizeOf(Drive^.LFNLen   ));
  S.Read(Drive^.EXTLen,    SizeOf(Drive^.EXTLen   ));
  S.Read(Drive^.DirFLP,    SizeOf(Drive^.DirFLP   ));
  S.Read(Drive^.FilFLP,    SizeOf(Drive^.FilFLP   ));
  S.Read(Drive^.Param,     SizeOf(Drive^.Param    ));
  S.Read(Drive^.Flags,     SizeOf(Drive^.Flags    ));
 end else
  S.Read(dumm,SizeOf(Drive^.LFNLen)+SizeOf(Drive^.EXTLen)+
              SizeOf(Drive^.DirFLP)+SizeOf(Drive^.FilFLP)+
              SizeOf(Drive^.Param) +SizeOf(Drive^.Flags));

 If BadDrv Then
  S.Get
 Else
  Files := PFilesCollection(S.Get);
 if (Files <> nil) and (Drive^.DriveType=dtDisk) then
   for I := 0 to Files^.Count-1
     do PFileRec(Files^.At(I))^.Owner := @Drive^.CurDir;
 Loaded := Not BadDrv; MSelect := False; isValid := True;
 QuickSearch := False; ViewEnabled := False;
 CommandEnabling := True;
end;

procedure TFilePanelRoot.Awaken;
begin
 RereadDir;
 if (DriveLine <> nil) then
   if (FMSetup.Show and fmsDriveLine <> 0) then
     begin
       if not DriveLine^.GetState(sfVisible) then DriveLine^.Show;
     end else
          if DriveLine^.GetState(sfVisible) then DriveLine^.Hide;
end;

procedure TFilePanelRoot.Store;
var I: Longint;
begin
 inherited Store(S);
 PutPeerViewPtr(S, ScrollBar);
 PutPeerViewPtr(S, DirView);
 PutPeerViewPtr(S, InfoView);
 PutPeerViewPtr(S, DriveLine);
 if (ActivePanel <> @Self) or (Drive^.DriveType <> dtDisk)
    or (StartupData.UnLoad and osuPreserveDir <> 0)
    or (word((@DirectoryName[1])^)=$2F2F{//})
    or (word((@DirectoryName[1])^)=$5C5C{\\})
    or (ManualSave) {John_SW}
   then S.Put(Drive) else S.Put(nil);
 S.Write(FileMask, 1 + Length(FileMask));
 S.Write(SortMode, 1);
 S.Write(PanelFlags, 2);
 S.Write(Delta, 2);
 S.Write(ForceReading, 1);
 S.Write(Drive^.LFNLen,    SizeOf(Drive^.LFNLen   ));
 S.Write(Drive^.EXTLen,    SizeOf(Drive^.EXTLen   ));
 S.Write(Drive^.DirFLP,    SizeOf(Drive^.DirFLP   ));
 S.Write(Drive^.FilFLP,    SizeOf(Drive^.FilFLP   ));
 S.Write(Drive^.Param,     SizeOf(Drive^.Param    ));
 S.Write(Drive^.Flags,     SizeOf(Drive^.Flags    ));
 PFilesCollection(Files)^.Selected := ScrollBar^.Value;
 if (Files <> nil) and (Drive^.DriveType<>dtDisk) then begin
   for I := 0 to Files^.Count-1
     do PFileRec(Files^.At(I))^.OwnerDisposible:=true;
   S.Put(Files);
   for I := 0 to Files^.Count-1
     do PFileRec(Files^.At(I))^.OwnerDisposible:=false;
 end else S.Put(Files);
end;

function TFilePanelRoot.Valid;
begin
 Valid := isValid;
 if (Command = cmClose) then Valid := Drive^.Disposable;
end;

destructor TFilePanelRoot.Done;
begin
 If ActivePanel = @Self Then ActivePanel:=Nil; {John_SW}
 if Files <> nil then Dispose(Files,Done); Files:=nil;
 if Drive <> nil then Dispose(Drive,Done); Drive:=nil;
 inherited Done;
end;

procedure TFilePanelRoot.ShowView;
begin
 if InfoView <> nil then InfoView^.ShowView;
 if DirView <> nil then DirView^.ShowView;
 if (ScrollBar <> nil) and GetState(sfActive+sfSelected) then
   ScrollBar^.Show;
 if Loaded then RereadDir;
 inherited ShowView;
end;

procedure TFilePanelRoot.HideView;
begin
 if InfoView <> nil then InfoView^.HideView;
 if DirView <> nil then DirView^.HideView;
 if (ScrollBar <> nil) then ScrollBar^.Hide;
 inherited HideView;
end;

procedure TFilePanelRoot.ChangeBounds;
 var
     I: LongInt;
     R: TRect;
begin
 I := Byte(Drive^.DriveType in [dtFind,dtTemp,dtArc,dtArcFind]);
 I := Byte(FMSetup.Show and fmsDivider <> 0) +
      Byte(PanelFlags and fmiCurrent <> 0)*(1 + I) +
      Byte(PanelFlags and (fmiSelectedInBottom or fmiFullNameInBottom) <> 0) +
      Byte(PanelFlags and fmiTotals <> 0) +
      Byte(PanelFlags and (fmiFree or fmiDescriptionInBottom) <> 0)*Byte(I=0);
 Dec(Bounds.B.Y, I);
 SetBounds(Bounds);
 R := Bounds; R.B.Y := R.A.Y; Dec(R.A.Y);
 if DirView <> nil then DirView^.SetBounds(R);
 R := Bounds; R.A.Y := R.B.Y; Inc(R.B.Y, I);
 if InfoView <> nil then InfoView^.SetBounds(R);
 if DriveLine <> nil then
   begin
     R.A.Y := R.B.Y + Byte(FMSetup.Show and fmsDriveLine = 0);
     Inc(R.B.Y);
     DriveLine^.SetBounds(R);
     if (FMSetup.Show and fmsDriveLine <> 0) then
       begin
         if not DriveLine^.GetState(sfVisible) then DriveLine^.Show;
       end else
            if DriveLine^.GetState(sfVisible) then DriveLine^.Hide;
   end;
 if ScrollBar <> nil then
  begin R :=Bounds; R.A.X := R.B.X; Inc(R.B.X); ScrollBar^.SetBounds(R) end;
 If Drive <> nil then begin
  Drive^.SizeX := Size.X;
  DirectoryName := Drive^.GetDir;
 end;
end;

procedure TFilePanelRoot.SendLocated;
 var
     R: LongInt;
     S: String;
begin
 if (Files^.Count = 0) or (Drive^.DriveType = dtArc) or
    (State and sfVisible = 0) then Exit;
 R := ScrollBar^.Value;
 if R >= Files^.Count then Exit;
 if not ViewEnabled and (Drive^.Flags and psShowLongDesc = 0) then Exit;
 S := MakeNormName(PFileRec(Files^.At(R))^.Owner^,
                   GetLFN(PFileRec(Files^.At(R))^.LFN));
 if ViewEnabled or (Drive^.Drivetype < dtArc)
    then Message(Owner, evCommand, cmLoadViewFile, @S);
end;


        {-DataCompBoy-}
procedure TFilePanelRoot.RereadDir;
 var
     I, J, Pos: LongInt;
     P,PP,CurP: PFileRec;
     FC {,FCC}: PFilesCollection;
     BB: Boolean;
     B: Boolean;
     TF: TFileRec;
     NewDir: String;
begin
 Abort := False; OldDelta := Delta; PosChanged := False;
 If Files = Nil Then
  New(Files, Init(10, 10));
 if Loaded then begin
  LineLength := Drive^.CalcLength;
  ScrollBar^.SetParams(PFilesCollection(Files)^.Selected, 0, Files^.Count - 1,
                      (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0)) *
                      ((Size.X+1) div LineLength), 1);
  CurrentDirectory := ActiveDir;
  Loaded := False;
{  if WasActive and (UpStrg(ActiveDir) <> UpStrg(DirectoryName)) then
   begin DirectoryName := ActiveDir; ReadDirectory;
         ScrollBar^.SetValue(0); Exit end;}
 end;
 if (Files = nil) or (Files^.Count = 0) then begin OldDelta := -1; Delta := 0 end;
 ClrIO; Loaded := False; ChDirName;
 if Abort then begin Dispose(Files,Done); New(Files, Init(10, 10)); Abort := False; Exit; end;
 NewDir := Drive^.GetRealDir;
 if Abort then begin Dispose(Files,Done); New(Files, Init(10, 10)); Abort := False; Exit; end;
 if GetState(sfFocused) then CurrentDirectory := DirectoryName;
 if Files^.Count = 0 then
  begin  DirectoryName := NewDir; ReadDirectory; ChDirName; Exit end;
 FC := PFilesCollection(Files); Files := nil; IncDrawDisabled;
 Pos := ScrollBar^.Value;
 if UpStrg(NewDir) <> UpStrg(DirectoryName) then
  begin Pos := 0; DirectoryName := NewDir end;
 I := 0; CurP := FC^.At(Pos);
 while (I < FC^.Count) do
   begin
     P := FC^.At(I); {-$VOL}
     if (P <> CurP) and (P <> nil) and not P^.Selected then
       FC^.AtReplace(I,nil); {-$VOL}
     Inc(I);
   end;
 ReadDirectory;
 B := not (Drive^.DriveType in [dtTemp, dtFind, dtList, dtArcFind]);
 if (FC^.Count > 0) then
  begin
   SelNum := 0; SelectedLen := 0; PackedLen := 0; BB := False;
   for I := 0 to FC^.Count-1 do
    begin
      PP := FC^.At(I);
      if (PP <> nil) and PP^.Selected or (PP = CurP) then
        for J := 0 to Files^.Count - 1 do
         begin
           P := Files^.At(J); {-$VOL}
           if (UpStrg(GetLFN(P^.LFN)) = UpStrg(GetLFN(PP^.LFN)))
{Commented by John_SW 27-04-2003}
{Why "Or"? WasLoaded are not needed any more!}
              {and (WasLoaded or (UpStrg(P^.Owner^) = UpStrg(PP^.Owner^))) then}
              And (UpStrg(CnvString(P^.Owner)) = UpStrg(cnvString(PP^.Owner))) then
             begin
              if B then begin
               TF := PP^;
               PP^ := P^;
               P^ := TF;
               PP^.Selected := TF.Selected;
              end;
              FC^.AtPut(I,P); {-$VOL}
              Files^.AtPut(J,PP); {-$VOL}
              if PP^.TType = ttUpDir then PP^.Selected := false;
              if PP^.Selected then
              begin
                SelectedLen := SelectedLen + PP^.Size;
                PackedLen := PackedLen + PP^.PSize;
                Inc(SelNum);
              end;
              Break;
           end;
         end;
    end;
   if not BB then
    begin
     for I := 0 to Files^.Count-1 do
      begin
        P := Files^.At(I);
        if (UpStrg(GetLFN(CurP^.LFN)) = UpStrg(GetLFN(P^.LFN))) and (UpStrg(CurP^.Owner^) = UpStrg(P^.Owner^)) then
           begin Pos := I; break; end;
      end;
    end;
  end else Pos := 0;
 Dispose(FC,Done);
 ScrollBar^.SetValue(Pos);
 DecDrawDisabled;
 DirectoryName := NewDir;
 SetDirName;
 if GetState(sfFocused) and (Drive^.DriveType = dtDisk) then
   GlobalMessage(evCommand, cmRereadInfo, nil);
 SendLocated;
end;
        {-DataCompBoy-}

procedure TFilePanelRoot.SetDirName;
begin
{ if (ActivePanel = @Self) then
  begin Drive^.lChDir(DirectoryName); if TypeOf(Drive^) = TypeOf(TDrive) then CurrentDirectory := DirectoryName; end;}
 DrawView;
 if InfoView <> nil then InfoView^.DrawView;
 if DirView <> nil then DirView^.DrawView;
end;

procedure TFilePanelRoot.ChDirName;
begin
 SetDirName;
end;

        {-DataCompBoy-}
procedure TFilePanelRoot.ReadDirectory;
begin
 LineLength := Drive^.CalcLength;
 Drive^.Owner := @Self;
 case Drive^.DriveType of
   dtArc, dtArcFind: HelpCtx := hcArchives;
   dtLink: HelpCtx := hcLinkPanel;
    else HelpCtx := hcFilePanel;
 end;
 DOSError := 0;
 PosChanged := False; Loaded := False;
 Abort := False; ClrIO;
 FreeSpace := ''; TotalInfo := '';
 if Files <> nil then Dispose(Files,Done);
 Files := PFilesCollection(Drive^.GetDirectory(SortMode, PanelFlags, FileMask, FreeSpace, TotalInfo));
 if Files = nil then Files := New(PFilesCollection, Init($10,$10));
 PFilesCollection(Files)^.Owner := @Self;
 SelNum := 0; SelectedLen := 0; PackedLen := 0;
 PFilesCollection(Files)^.SortMode := SortMode;
 if Abort then Exit;

 if DriveState and dsInvalid > 0 then Exit ;

 Drive^.SizeX := Size.X;
 if Drive^.DriveType = dtDisk
  then DirectoryName := Drive^.GetRealDir
  else DirectoryName := Drive^.GetDir;
 if GetState(sfSelected) then Message(Owner, evCommand, cmChangeTree, @DirectoryName);

 ScrollBar^.SetParams(ScrollBar^.Value, 0, Files^.Count - 1, (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0))
                                        * ((Size.X+1) div LineLength), 1);
 if (ActivePanel = @Self) and (Drive^.DriveType = dtDisk)
    then CurrentDirectory := DirectoryName
    else LFN.lChDir(CurrentDirectory);
 if Drive^.DriveType = dtDisk then Message(CommandLine, evCommand, cmRereadInfo, nil);
 Message(Owner, evCommand, cmRereadInfo, nil);
 if DriveLine <> nil then DriveLine^.DrawView;
 ChkNoMem;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TFilePanelRoot.GetUserParams;
  var PF: PFileRec;
      S: String[1];

 procedure MakeTempList;
  var
      I: LongInt;
      T: lText;
 begin
  lAssignText(T, List); ClrIO;
  lRewriteText(T); if Abort then Exit;
{--- start -------- Eugeny Zvyagintzev ---- 15-05-2003 ----}
{For macros !: #: $: &: when drive is empty}
  If (Files = Nil) or (Files^.Count = 0) Then
   Begin
    Close(T.T);
    Exit;
   End;
{--- finish -------- Eugeny Zvyagintzev ---- 15-05-2003 ----}
  if (SelNum=0) then begin
   PF := Files^.At(ScrollBar^.Value);
   {$IFNDEF OS2}
   WriteLn(T.T, MakeFileName(PF^.Name));
   {$ELSE}
   WriteLn(T.T, GetLFN(PF^.LFN));
   {$ENDIF}

  end else
  for I := 1 to Files^.Count do begin
   PF := Files^.At(I-1);
   if (PF^.Selected) then
   {$IFNDEF OS2}
                     WriteLn(T.T, MakeFileName(PF^.Name));
   {$ELSE}
                     WriteLn(T.T, GetLFN(PF^.LFN));
   {$ENDIF}
  end;
  Close(T.T);
 end;

begin
  FileRec := nil; List := '';
  if not GetState(sfVisible) then Exit;
{--- start -------- Eugeny Zvyagintzev ---- 15-05-2003 ----}
{For macros !: #: $: &: when drive is empty}
  if (Files = nil) or (Files^.Count = 0) Then
   FileRec:=NewFileRec('','',0,0,Directory,@DirectoryName)
{--- finish -------- Eugeny Zvyagintzev ---- 15-05-2003 ----}
  Else
   FileRec := Files^.At(ScrollBar^.Value);
  if BuildList then begin
   if GetState(sfSelected) then S := '$' else S := '';
   List := SwpDir+'$dn'+ItoS(DNNumber)+S+'.lst';
   MakeTempList;
  end else List := '';
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
PROCEDURE TFilePanelRoot.CommandHandle;
var
  CurPos: LongInt;
  LastRDelay: Word;
  PF: PFileRec;
  MPos: TPoint;

 procedure CE;begin ClearEvent(Event) end;
 procedure CED;begin ClearEvent(Event); DrawView end;

 procedure Rebound;
  var R: TRect;
 begin
  GetBounds(R);
  R.A.Y := 1; R.B.Y := Owner^.Size.Y - 1;
  ChangeBounds(R);
  Owner^.Redraw;
 end;

 procedure GotoSingle(Name: String);
  var A: LongInt;
 begin
  A := Files^.Count;
  UpStr(Name);
  while A <> 0 do
    begin
    Dec(A);
    if UpStrg(GetLFN(PFileRec(Files^.At(A))^.LFN)) = Name then
      Break;
    {AK155 странно, почему тут сравнивается только длинное имя.}
    end;
  IncDrawDisabled;
  OldDirectory := DirectoryName;
  ScrollBar^.SetValue(A);
  DecDrawDisabled;
  Rebound;
 end;

 function ChangeUp: Boolean;
  var S: String;
 begin
  if ChangeLocked then Exit;
  ChangeUp := Drive^.IsUp;
  DeltaX := 0;
  if Drive^.IsUp then Drive^.ChangeUp(S) else Exit;
  IncDrawDisabled; ReadDirectory;
  AddToDirectoryHistory(DirectoryName, Drive^.DriveType);
  if S <> '' then GotoSingle(S);
  RereadDir;
  DecDrawDisabled;
  Rebound;
 end;

 function ReplaceDrive(C: char): Boolean;
 var
   PDr: PDrive;
   NeedRebound: Boolean; {John_SW  01-05-2003}
 begin
    ReplaceDrive := false;
    New(PDr, Init(Byte(C)-64, @Self, Drive^.Param));
    if Abort then
     begin Dispose(PDr,Done); Exit; end;
    NeedRebound:=Drive^.DriveType <> PDr^.DriveType; {John_SW  01-05-2003}
    Dispose(Drive,Done);
    Drive := PDr;
    DeltaX := 0;
    {if WasFull then Drive^.Flags := Drive^.GetFullFlags
               else Drive^.Flags := 0;}
    Drive^.SizeX := Size.X;
    DirectoryName := Drive^.GetDir;
{Commented By John_SW 27-12-2002}
{Fix change drive}
    {if GetState(sfActive+sfSelected) then
       SetState(sfActive+sfSelected, true);}
{John_SW  01-05-2003 Drive's type differs - need to rebound!}
    If NeedRebound Then Rebound;
    ReplaceDrive := true;
 end;

 procedure GotoFile(FileName: String);
  var
      Drv: PDrive; {JO}
      I: Byte;
      Dr: String;
      Nm: String;
      Xt: String;
      Name: String;
      PathInside: String; {JO}
 label WrongArc;
 begin
  if ChangeLocked then Exit;
{JO: дабы перейти к найденному файлу в архиве из панели поиска}
  PathInside := FileName;
  if PathInside[2] = ':' then PathInside[2] := ';'; {JO: меняем двоеточие не важно на что }
    I := PosChar(':', PathInside);
    if I > 0 then
      begin
        PathInside := Copy(FileName, I+1, 255);
        FileName := Copy(FileName, 1, I-1);
      end
     else PathInside :=  '';
{/JO}
  lFSplit(FileName, Dr, Nm, Xt);
  {$IFDEF MODEM}
  {$IFDEF LINK}
  if Copy(Dr, 1, 5) = cLINK_ then  Delete(Dr, 1, 5);
  {$ENDIF}
  {$ENDIF}
  if (Dr[Length(Dr)] = '\') and ((Dr[2] <> ':') or (Dr[0] > #3)) then Dec(Dr[0]);
  if (Dr[2] = ':') and not(Drive^.DriveType in [dtDisk,dtLink,dtArc]) then
    begin
     Dispose(Drive,Done);
     New(Drive, Init(Byte(Upcase(Dr[1]))-64, @Self, Drive^.Param));
    end;
  if Drive^.DriveType <> dtDisk {fixed by piwamoto}
    then DirectoryName := Dr else DirectoryName := GetNormPath(Dr);
  IncDrawDisabled;
  OldDirectory := Dr;
  DeltaX := 0;
  Drive^.lChDir(DirectoryName);
  Drive^.SizeX := Size.X;
  DirectoryName := Drive^.GetDir;
  OldDelta := -1; Delta := 0;
  ReadDirectory;
  AddToDirectoryHistory(DirectoryName, Drive^.DriveType);
  DecDrawDisabled;

{JO: переходим к найденному файлу в архиве}
  if (PathInside <> '') then
    begin
      Drv := nil;
      Drv := New(PArcDrive,Init(FileName, FileName, FreeByte));
       if Drv = nil then goto WrongArc;
       if Drive^.DriveType <> dtDisk then ReplaceDrive(FileName[1]);
       Message(@Self, evCommand, cmInsertDrive, Drv);
       if (GetPath(PathInside) <> '\') and (Drive^.DriveType = dtArc) then
         begin
           Drive^.lChDir(Copy(GetPath(PathInside), 2, 255));
           ReadDirectory;
         end;
       GotoSingle(GetName(PathInside));
       Exit;
    end;
WrongArc:
{/JO}

  if (Nm = '') and (Xt='') then Name := '..'
                           else Name := Nm+Xt;
  if Name = '.' then Name := '';{directory name '.' bugfix by piwamoto}
  GotoSingle(Name);
 end;

 function  isSeldir : boolean ;
   begin
      isSelDir := False ;
      if Files^.Count = 0 then Exit;
      if CurPos >= Files^.Count then Exit;
      isSelDir := PFileRec(Files^.At(CurPos))^.Attr and Directory <> 0;
   end;

 procedure ViewFile(Command: Word);
  var P: PFileRec;
 begin
  if Files^.Count = 0 then Exit;
  CE; if CurPos >= Files^.Count then Exit;
  P := Files^.At(CurPos);
  if P^.Attr and Directory <> 0 then Exit;
  {$IFNDEF BIT_32}
  asm
   mov ax,Command
   cmp ax,cmIntEditFile
   jnz @@1
   mov ax,cmIntFileEdit;
   jmp @@0
@@1:
   cmp ax,cmIntViewFile
   jnz @@2
   mov ax,cmIntFileView;
   jmp @@0
@@2:
   cmp ax,cmEditFile
   jnz @@3
   mov ax,cmFileEdit;
   jmp @@0
@@3:
   mov ax,cmFileView;
@@0:
   mov Command, ax
  end;
  {$ELSE BIT_32}{&Frame-}
  asm
   mov eax,Command
   cmp eax,cmIntEditFile
   jnz @@1
   mov eax,cmIntFileEdit;
   jmp @@0
@@1:
   cmp eax,cmIntViewFile
   jnz @@2
   mov eax,cmIntFileView;
   jmp @@0
@@2:
   cmp eax,cmEditFile
   jnz @@3
   mov eax,cmFileEdit;
   jmp @@0
@@3:
   mov eax,cmFileView;
@@0:
   mov Command, eax
  end;
  {$ENDIF}
  Drive^.UseFile(P, Command);
 end;

 procedure Recount;
  var I: LongInt;
      PF: PFileRec;
 begin
  SelectedLen := 0; PackedLen := 0; SelNum := 0;
  for I := 1 to Files^.Count do
   begin
    PF := Files^.At(I-1);
    if PF^.Selected then
    begin
      SelectedLen := SelectedLen + PF^.Size;
      PackedLen := PackedLen + PF^.PSize;
      Inc(SelNum);
    end;
   end;
   DrawView;
   if InfoView <> nil then InfoView^.DrawView;
 end;

 procedure Setup;
  var R: record Sort,Show,Info,Sel,LFN: AWord; S: String; end;
 begin
  R.Sort := SortMode;
  if R.Sort = 0 then R.Sort := NumSortModes else
    if R.Sort = psmOrdered then R.Sort := NumSortModes-1 else Dec(R.Sort);

{--- start -------- Eugeny Zvyagintzev ---- 18-01-2003 -----}
  R.Show := (PanelFlags And
             (fmiHiliteFiles+fmiExeFirst+fmiArchivesFirst+fmiExeBeforeArc)
            ) Shr 5;
  R.Info := PanelFlags And (fmiDirLen+fmiCurrent)+
            ((PanelFlags And (fmiTotals+fmiFree)) Shr 1)+
            ((PanelFlags And fmiDescriptionInBottom) Shr 6);
  R.Sel:= ((PanelFlags And fmiSelectedInBottom) Shr 2)+
          ((PanelFlags And fmiSelectedInDivider) Shr 10);
  R.LFN:= ((PanelFlags And fmiFullNameInBottom) Shr 9)+
          ((PanelFlags And fmiFullNameInDivider) Shr 11);
{--- finish -------- Eugeny Zvyagintzev ---- 18-01-2003 -----}
  R.S := FileMask;
  if ExecResource(dlgPanelSetup, R) <> cmOK then Exit;

  if R.Sort = NumSortModes then R.Sort := 0 else
   if R.Sort = NumSortModes-1 then R.Sort := psmOrdered else Inc(R.Sort);

  if (ActivePanel = @Self) and (Drive^.DriveType = dtDisk)
    then CurrentDirectory := DirectoryName;
  SortMode := R.Sort;

{--- start -------- Eugeny Zvyagintzev ---- 18-01-2003 -----}
  PanelFlags:=R.Show Shl 5;
  PanelFlags:=PanelFlags+(R.Info And $3)+((R.Info And $0C) Shl 1)+
              ((R.Info And $10) Shl 6);
  PanelFlags:=PanelFlags+((R.Sel And $1) Shl 2)+((R.Sel And $2) Shl 10);
  PanelFlags:=PanelFlags+((R.LFN And $1) Shl 9)+((R.LFN And $2) Shl 11);
{--- finish -------- Eugeny Zvyagintzev ---- 18-01-2003 -----}
  FileMask := R.S; RereadDir;
  Rebound;
  ChDirName;
 end;

 procedure CountLen;

  procedure DoCount(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  begin
    if (P^.Attr and Directory <> 0) and (P^.Selected) and (not Abort) then Drive^.GetDirLength(P);
  end;

 begin
  CE; if (PF = nil) or ((PF^.Attr and Directory = 0) and (SelNum = 0)) then Exit;
  if PF^.Attr and Directory <> 0 then Drive^.GetDirLength(PF);
  if SelNum > 0 then Files^.ForEach(@DoCount); Abort := False;
  Recount;
  DrawView;
  if InfoView <> nil then InfoView^.DrawView;
 end;

 var WasFull: Boolean;

 procedure InsertDrive;
  var PDir: PString;
 begin
  if ChangeLocked or (Event.InfoPtr = nil) then Exit;
  {if (PDrive(Event.InfoPtr)^.DriveType = dtTemp) and
     (Drive^.DriveType = dtTemp) then begin CE; Exit end;} {This is nimious check. Persistor, Mar 2005}
  if PDrive(Event.InfoPtr)^.DriveType in [dtDisk, dtLink] then
      begin if Drive <> nil then Dispose(Drive,Done); Drive:=nil end;
  PDrive(Event.InfoPtr)^.Prev := Drive;
  Drive := Event.InfoPtr;
  if (Drive^.DriveType = dtFind) and
     (Drive^.Prev <> nil) and (Drive^.Prev^.DriveType = dtArc) then
   begin
     with PArcDrive(Drive^.Prev)^ do
       begin
       if CurDir = '' then
         PDir := NewStr(ArcName+':\')
       else
         PDir := NewStr(ArcName+':'+CurDir)
       end;
     with PFindDrive(Drive)^ do
       begin
       DriveType := dtArcFind;
       UpFile^.Owner := PDir;
       Dirs^.Insert(PDir);
       end;
   end;
  if (Drive^.DriveType = dtArc) or (Drive^.DriveType = dtArvid) then Drive^.lChDir(#0);
  {if WasFull then Drive^.Flags := Drive^.GetFullFlags else Drive^.Flags := 0;}
  Drive^.Owner := @Self;
  CE;
  DeltaX := 0;
  ReadDirectory;
  AddToDirectoryHistory(DirectoryName, Drive^.DriveType);
  ScrollBar^.SetValue(0);
  Rebound;
 end;

 procedure EraseGroup;
 begin
   Drive^.EraseFiles(Event.InfoPtr);
   CE;
 end;

 procedure SelecType(S: Boolean);
  procedure Sel(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  begin
   if (P^.TType = PF^.TType) and (P^.TType <> ttUpDir) then
     P^.Selected := S;
  end;
 procedure SelAll(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
 begin
  P^.Selected:=S;
 end;
 begin
  if (CurPos >= Files^.Count) {or (PanelFlags and fmiHiliteFiles = 0)} then Exit;
  PF := Files^.At(CurPos);
  if PF^.TType <> ttUpDir then
    Files^.ForEach(@Sel) else
  begin
    Files^.ForEach(@SelAll);
    PF^.Selected:=False;
  end;
  Recount; Owner^.Redraw;
 end;

 procedure SelectExt(S: Boolean; G1, G2: LongInt; Invert: Boolean);
  var
      a: boolean;
      SS: String;

  procedure Sel(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  begin
   if a then
    begin
     if ((SS = UpStrg(GetExt (GetLFN(P^.LFN)))) xor Invert)
       and (P^.TType <> ttUpDir) then P^.Selected := S;
    end else
    begin
     if ((SS = UpStrg(GetSName(GetLFN(P^.LFN)))) xor Invert)
       and (P^.TType <> ttUpDir) then P^.Selected := S;
    end
  end;

 begin
  if (CurPos >= Files^.Count) then Exit;
  PF := Files^.At(CurPos);
  if g1=10 then a:=true else a:=false;
  if a then SS := UpStrg(GetExt  (GetLFN(PF^.LFN)))
       else SS := UpStrg(GetSName(GetLFN(PF^.LFN)));
  Files^.ForEach(@Sel);
  Recount; Owner^.Redraw;
 end;

 procedure HandleCommand;
  var
      W: Word;
      FC: PCollection;
 begin
   W := Event.Command; CE; FC := nil;
   case W of
    cmExtractTo, cmArcTest: begin FC := GetSelection(@Self, False); if FC = nil then Exit; end;
   end;
   Drive^.HandleCommand(W, FC);
   if FC <> nil then
    begin
      FC^.DeleteAll;
      Dispose(FC,Done);
    end;
 end;

 function MakeFName(Idx: LongInt; Mode: Boolean; Sq: Boolean): String;
 begin
   PF := Files^.At(Idx);
   FreeStr := GetLFN(PF^.LFN);
   if (ShiftState and kbAltShift = 0)
     And (FreeStr <> '..') {John_SW  05-03-2003}
     then FreeStr := GetName(lfGetShortFileName(FreeStr));
   if PosChar('.', FreeStr) = 0 then AddStr(FreeStr, '.');
   if (Drive^.DriveType = dtDisk) then
    If (ShiftState and (kbLeftShift+kbRightShift) <> 0) then
     {$IFNDEF OS2}
     if (ShiftState and kbAltShift = 0)
      then FreeStr := MakeNormName(lfGetShortFileName(PF^.Owner^), FreeStr)
      else FreeStr := MakeNormName(lfGetLongFileName(PF^.Owner^), FreeStr)
     {$ELSE}
     FreeStr := MakeNormName(PF^.Owner^, FreeStr)
     {$ENDIF}
    else
   else FreeStr := MakeNormName(PF^.Owner^, FreeStr);
   if Sq then
    if Mode
     then MakeFName := ' '+SquashesName(FreeStr)
     else MakeFName := SquashesName(FreeStr)
   else
    if Mode
     then MakeFName := ' '+FreeStr
     else MakeFName := FreeStr;
 end;

 procedure _CtrlPgUp; forward;
 procedure DoChange(SS: String);
 begin
{--- start -------- Eugeny Zvyagintzev ---- 01-03-2003 ----}
{If we handle CD command then we must do it correctly!}
   if SS = '' then Exit;
   If HandleChDirCommand Then
{If we in network path and new folder name contains no drive or network path}
{specifications we have to change dir by full path.}
{So, it's quite correct do it not only in network path!}
    If (SS[2] <> ':') And (SS[1] <> '\') Then SS:=MakeNormName(DirectoryName,SS)
    Else
{If new folder name contains is Root Folder and we in network path we have to}
{use root of current network folder.}
{So, it's quite correct do it not only in network path!}
     If (SS[1] = '\') And (SS[2] <> '\') Then
      Begin
       SS:=MakeNormName(GetRoot(DirectoryName),Copy(SS,2,Length(SS)));
       If Not PathExist(SS) Then Exit;
{Before change folder to root we have to change Drive to dtDisk}
       If Drive^.DriveType <> dtDisk Then
        Begin
         IncDrawDisabled; {No need to redraw yet!}
         Drive^.ChangeRoot;
         _CtrlPgUp;
         DecDrawDisabled;
        End;
      End;
  If Not PathExist(SS) Then Exit;
{Before change folder to root we have to change Drive to dtDisk}
  IncDrawDisabled; {No need to redraw yet!}
  if Drive^.DriveType <> dtDisk then
   if not ReplaceDrive(SS[1]) then
    Begin
     DecDrawDisabled;
     Exit;
    End;
  DecDrawDisabled;
  ChDir(SS);
{--- finish -------- Eugeny Zvyagintzev ---- 01-03-2003 ----}
 end;

(*DataCompBoy
 procedure DoExcept(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
 begin
   if (P <> nil) and (P^.Attr and Directory = 0) then
    if not InFilter(GetLFN(P^.LFN), FileMask) then P^.Size := -100;
 end;
*)
 procedure _GetFName;
 var
   dum: PFileRec;
   S: string;
 begin
   GetUserParams(dum, PString(Event.InfoPtr)^, False);
   {$IFNDEF OS2}
   S := MakeFileName(PF^.Name);
   {$ELSE}
   S := GetLFN(PF^.LFN);
   {$ENDIF}
   PString(Event.InfoPtr)^ := S + ' ' + CnvString(Event.InfoPtr);
 end;

 procedure _DoPush(a: Boolean);
 var
   S: string;
 begin
   if not GetState(sfVisible) then Exit;
   if a and (Drive^.DriveType<>dtDisk) then PString(Event.InfoPtr)^ := CurrentDirectory
   else
   begin
     S := Drive^.GetRealName;
     if (S <> '') and (S <> CurrentDirectory) then PString(Event.InfoPtr)^ := S;
   end;
 end;
        {-DataCompBoy-}
 procedure _DoRereadDir;
 var
   I: LongInt;
   S: string;
 begin
   if DrawDisableLvl > 0 then Exit; {John_SW 10-02-2003}
   if DrawDisableLvl < 0 then DrawDisableLvl := 0; {John_SW 10-02-2003}
   S := UpStrg(CnvString(Event.InfoPtr));
   if (S[0] > #3) and (S[Length(S)] = '\') then Dec(S[0]);
   I := Length(S);
   if (Drive^.DriveType <> dtDisk) or
      (S = Copy(UpStrg({$IFNDEF OS2}lfGetLongFileName{$ENDIF}(DirectoryName)), 1, Length(s)) ) then
       begin
        Drive^.RereadDirectory(S);
        RereadDir;
       end
   else
        if (Drive^.DriveType = dtDisk) and (UpStrg(S[1]) = UpStrg(DirectoryName[1])) then
         begin
           if PanelFlags and fmiFree <> 0 then
              Drive^.GetFreeSpace(FreeSpace);
           if InfoView <> nil then InfoView^.DrawView;
         end;
 end;
        {-DataCompBoy-}

 procedure _PushName;
 var
   S: string;
 begin
   S := Drive^.GetRealName;
   if S <> '' then HistoryAdd(Event.InfoByte, S);
 end;

 procedure _ChangeDrive;
 var
   S: string;
 begin
   if ChangeLocked then Exit;
   CE;
   Abort := False; OldDelta := -1;
   MPos.X := Origin.X + Size.X div 2;
   if DriveSelectVCenter then                {-$X-Man}
   MPos.Y := Origin.Y + Size.Y div 2 {-$VIV} {-$X-Man}
   else MPos.Y := Origin.Y+1;                {-$X-Man}
   Owner^.MakeGlobal(MPos, MPos);
   if (DirectoryName[2] = ':') then S[1] := DirectoryName[1]
       else if (Drive^.DriveType = dtLink) then S[1] := '+'
            else S[1] := CurrentDirectory[1];
   S := SelectDrive(MPos.X, MPos.Y, S[1], True);
   if S = cTEMP_ then
   begin
     Message(@Self, evCommand, cmChangeDrv, @S);
     Exit;
   end else
   {$IFDEF MODEM}
   {$IFDEF LINK}
   if S[1] = '+' then
   begin
     Event.InfoPtr := NewLinkDrive(s[2],@Self, Drive^.Param);
     InsertDrive; Exit;
   end else
   {$ENDIF}
   {$ENDIF}
   if S <> '' then
    begin
     ClrIO;
     Abort:=False;
     if Drive^.DriveType <> dtDisk then
      begin
       if not ReplaceDrive(S[1]) then Exit;
      end else
      begin
        Drive^.lChDir(S);
        if Abort then
         begin  ClrIO; SetDirName; LFN.lChDir(CurrentDirectory); Exit; end;
        Drive^.SizeX := Size.X;
        S := Drive^.GetDir;
      end;
     IncDrawDisabled; DirectoryName := S; ReadDirectory;
     AddToDirectoryHistory(DirectoryName, Drive^.DriveType);
     DecDrawDisabled; SetDirName;
    end;
   ChDirName;
 end;

 procedure _ChangeDrv;
 var
   S: string;
 begin
   if ChangeLocked then Exit;
   QuickSearch := false;
   S := CnvString(Event.InfoPtr); CE;
   if S = cTEMP_ then begin
    if Drive^.DriveType<>dtTemp then begin { <= Persistor, Mar 2005}
      Event.InfoPtr := New(PTempDrive, Init(Drive^.Param));
      InsertDrive;
    end;
    Exit;
   end
   {$IFDEF MODEM}
   {$IFDEF LINK}
   else if S[1] = '+' then
   begin
     Event.InfoPtr := NewLinkDrive(S[2],@Self, Drive^.Param);
     InsertDrive; Exit;
   end
   {$ENDIF};
   {$ENDIF};
   lGetDir(Byte(S[1])-64, S);
   if not Abort then
   begin
     S:=GetNormPath(S);
     If (S[Length(S)]='\') and (S[Length(S)-1]<>':') then Dec(S[0]);
     DoChange(S);
   end;
 end;

 procedure _DoDirHistory;
 var
   Dr: PDrive;
   S: string;
{   Q: string;}
 begin
   CE;
   S := DirHistoryMenu(Drive^.DriveType); { Flash }
   if S <> '' then
   begin
     FreeByte:=PosChar(':', Copy(S, 3, 255))+2;
     if FreeByte>2 then begin
(* {JO: закоментированный кусок реально никогда не работал, т.к. перед }
   {    ним стояло ReplaceDrive, после которого ни одно из условий не  }
   {    выполнялось. После того, как я перенёс ReplaceDrive ниже, этот }
   {    кусок стал подключаться и выяснилось, что он не работает       }
      Q:=Copy(S, 1, FreeByte-1);
      if ((Drive^.DriveType = dtArc) and
         (UpStrg(PArcDrive(Drive)^.ArcName)=UpStrg(Q)))
         or
         ((Drive^.DriveType = dtArvid) and
         (UpStrg(PArvidDrive(Drive)^.Name^)=UpStrg(Q))) then begin
       Q:=Copy(S, FreeByte+1, 255);
       Message(@Self, evCommand, cmChangeDirectory, @Q)
      end else begin
*)
       if s=cTEMP_ then begin Message(@Self, evCommand, cmChangeDrv, @S); { <= Persistor, Mar 2005}
                              Exit;                                       { <= Persistor, Mar 2005}
                        end;                                              { <= Persistor, Mar 2005}
       Dr:=nil;
       Dr:=New(PArcDrive,Init(S, S, Drive^.innum));
{$IFDEF ARVID}
       if Dr=nil then Dr:=New(PArvidDrive,Init(S, Drive^.innum));
{$ENDIF}
       if Dr=nil then exit;
       if Drive^.DriveType <> dtDisk then ReplaceDrive(S[1]);
       Message(@Self, evCommand, cmInsertDrive, Dr);
     {end}
     end
     else
      if S[Length(S)]='\'
        then
          begin
            if not PathExist(S) then
              begin
                messagebox(^C + GetString(dlDirectory) + ' '+  S
                 + ^M^C + GetString(dlDoes_Not_Exist), nil, mfError+mfOKButton);
                Exit;
              end;
            if Drive^.DriveType <> dtDisk then ReplaceDrive(S[1]);
            Message(@Self, evCommand, cmChangeDirectory, @S);
          end
         else
          begin
            if not ExistFile(S) then
              begin
                messagebox(^C + GetString(dlFile) + ' '+  S
                 + ^M^C + GetString(dlDoes_Not_Exist), nil, mfError+mfOKButton);
                Exit;
              end;
             FreeByte := Drive^.innum;
             if Drive^.DriveType <> dtDisk then ReplaceDrive(S[1]);
            {JO: раньше при переходе к списку из истории каталогов текущий }
            {    каталог не менялся и, соответственно, список не находил   }
            {    файлов, находящихся в его каталоге. Для исправления этого }
            {    нижележащая строка                                        }
             Drive^.lChDir(GetPath(S));

             Message(@Self, evCommand, cmInsertDrive, New(PFindDrive,InitList(S)))
          end;
   end;
 end;

 procedure _CloseLinked;
 var
   S: string;
 begin
   if Drive^.DriveType = dtLink then
   begin
     lGetDir(0, S);
     ReplaceDrive(S[1]);
     RereadDir;
   end;
 end;

 procedure _DoCtrl;
 var
   DriveLetter: Char;
 begin
   if ((ShiftState and 3 <> 0) or (CmdLine.Str = ''))
      and (FMSetup.Show and fmsDriveLine <> 0) then
   begin
     if (DirectoryName = cTEMP_) then DirectoryName[1] := '*';
     if Drive^.DriveType = dtArc
       then DirectoryName[1] := PArcDrive(Drive)^.ArcName[1];
{$IFDEF ARVID}
     if Drive^.DriveType = dtArvid
       then DirectoryName[1] := PArvidDrive(Drive)^.Name^[1];
{$ENDIF}
     DriveLetter := DirectoryName[1];
     DirectoryName[2] := ':';
     repeat
       if Event.What = evKeyDown then
        begin
          case Event.KeyCode of
            kbCtrlLeft: repeat
                          if DirectoryName[1] in ['B'..'Z'] then Dec(DirectoryName[1])
                           else if DirectoryName[1] = 'A' then DirectoryName[1] := '*'
                                                    else DirectoryName[1] := 'Z';
                         until ValidDrive(DirectoryName[1]) or (DirectoryName[1] = '*');
            kbCtrlRight: repeat
                           if DirectoryName[1] in ['A'..'Y'] then INC(DirectoryName[1])
                            else if DirectoryName[1] = 'Z' then DirectoryName[1] := '*'
                                                     else DirectoryName[1] := 'A';
                          until ValidDrive(DirectoryName[1]) or (DirectoryName[1] = '*');
          end;
          DriveLine^.DrawView;
        end;
       GetEvent(Event);
     until ShiftState and kbCtrlShift = 0;
     if DriveLetter <> DirectoryName[1] then
       begin
         if DirectoryName[1] = '*' then DirectoryName := cTEMP_;
         Message(@Self, evCommand, cmChangeDrv, @DirectoryName);
       end;
     Drive^.SizeX := Size.X;
     DirectoryName := Drive^.GetDir;
     DriveLine^.DrawView;
     Abort := False;
{     DriveLine^.DrawView;}
     CE;
   end;
 end;

 procedure _CtrlIns;
 var
   I: LongInt;
   b: boolean;
   S: string;
 begin
   S := ''; CE;
   b := false;
   if (Files = nil) or (Files^.Count <= 0) then Exit;
   if SelNum = 0
    then S := MakeFName(CurPos, False, False)
    else
     for I := 0 to Files^.Count-1 do
      if PFileRec(Files^.At(I))^.Selected then begin
       S := S + MakeFName(I, b, True);
       b:=true;
      end;
    Message(Application, evCommand, cmPutInClipboard, @S);
 end;

 function _AltEnter: boolean; forward;
 procedure _CtrlEnter;
 var
   PF: PFileRec;
   S: string;
 begin
   CE;
   if Files^.Count = 0 then Exit;
   PF := Files^.At(CurPos);
   if (ShiftState and
      (kbAltShift+kbCtrlShift+kbLeftShift+kbRightShift)=kbAltShift+kbCtrlShift
      ) and OS2exec then begin _AltEnter; exit end;
   if (PF <> nil) then
   begin
    S := GetLFN(PF^.LFN);
    if (PF^.Attr and Directory = 0) and
       (PosChar('.', S) = 0) then AddStr(S, '.');
    if (S <> '..') and (ShiftState and kbAltShift = 0)
     {piwamoto: get real short name instead of PF^.Name}
     then S := GetName(lfGetShortFileName(GetLFN(PF^.LFN)));
    if S = '..'  then
     {$IFNDEF OS2}
     if (ShiftState and kbAltShift = 0)
      then S := lfGetShortFileName(PF^.Owner^)+'\'
      else{$ENDIF} S := {$IFNDEF OS2}lfGetLongFileName{$ENDIF}(PF^.Owner^)+'\'
    else
     if ShiftState and (kbRightShift + kbLeftShift) <> 0 then
      {$IFNDEF OS2}
      if (ShiftState and kbAltShift <> 0)
       then S := MakeNormName(lfGetLongFileName(PF^.Owner^), S)
       else{$ENDIF} S := MakeNormName({$IFNDEF OS2}lfGetShortFileName{$ENDIF}(PF^.Owner^), S);
    S:=SquashesName(S);
    Message(CommandLine, evCommand, cmInsertName, @S);
   end;
 end;

 function _AltEnter: Boolean;
 var
   S: string;
 begin
   if OS2exec and (Drive^.DriveType = dtDisk) and
      InExtFilter(MakeFileName(PF^.Name), Executables) and
      (ShiftState and (kbCtrlShift or 3) <> 0) then
   begin
     PShootState := ShiftState;
     S := MakeFileName(PF^.Name);
     if ShiftState and kbCtrlShift = 0 then S := '>'+S
                                         else S := '<'+S;
     Message(Application, evCommand, cmExecString, @S);
     CE;
     _AltEnter := True;
   end else
   begin
     PShootState := ShiftState;
     CE;
     _AltEnter := False;
   end;
 end;

 procedure _GotoExt;
 var
   S: string;
 begin
   if (Drive^.DriveType <> dtArvid) and (Drive^.DriveType <> dtArc) then
   begin
      if Drive^.DriveType = dtDisk then S := MakeFileName(PF^.Name)
        else
             begin CE; Exit; end;
      if OS2exec and (Copy(PF^.Name,10,3) = 'cmd') then
       begin
          S := 'call "'+S+'"';
          if ShiftState and 3 <> 0 then S := '<'+S else S := '>'+S;
          Message(Application, evCommand, cmExecString, @S);
          Exit;
       end;
      if (Word((@DirectoryName[1])^) = $2F2F{//}) or
         (Word((@DirectoryName[1])^) = $5C5C{\\})
         then S:=MakeNormName(DirectoryName, S);
      Message(Application, evCommand, cmExecFile, @S);
   end else ViewFile(cmViewFile)
   {else Message(Application, evCommand, cmExecCommandLine, nil);}
 end;

 procedure _CtrlPgUp;
 begin
   CE;
   if ChangeLocked then Exit;
   if not ChangeUp then
     if Drive^.DriveType in [dtDisk, dtLink] then GotoFile(DirectoryName)
 end;

 procedure _Enter;
 var S: string;
 begin
   if GetLFN(PF^.LFN) = '..' then
     begin _CtrlPgUp; exit; end;
   if not ( Drive^.DriveType in [ dtFind, dtTemp, dtList, dtArcFind ] ) then
     exit;
   if Drive^.DriveType <= dtArcFind then
    begin
      S := MakeNormName(PF^.Owner^, {$IFNDEF OS2}GetLFN(PF^.LFN){$ELSE}PF^.Name{$ENDIF});
      if Drive^.DriveType = dtArcFind
        then S := MakeNormName(PArcDrive(Drive^.Prev)^.ArcName + ':'+
                  PF^.Owner^, {$IFNDEF OS2}GetLFN(PF^.LFN){$ELSE}PF^.Name{$ENDIF})
       else  S := MakeNormName(PF^.Owner^, {$IFNDEF OS2}GetLFN(PF^.LFN){$ELSE}PF^.Name{$ENDIF});
(*
{JO: переходим к найденному файлу в архиве}
      if PathFoundInArc(PF^.Owner^) and
          ArcViewer(S, S, FreeByte) then
            begin
            {$IFNDEF OS2}
              GotoSingle(GetLFN(PF^.LFN));
            {$ELSE}
              GotoSingle(PF^.Name);
            {$ENDIF}
              Exit;
            end;
{/JO}
*)
      if ShiftState and 3 <> 0 then
        begin
          Message(Owner, evCommand, cmChangeInactive, @S);
          Exit;
        end;
      FreeByte := Drive^.Param;
      Dispose(Drive,Done);
      New(Drive, Init(Byte(S[1])-64, @Self, FreeByte));
      {Drive^.Owner := @Self;}
      DeltaX := 0;
      {if WasFull then Drive^.Flags := Drive^.GetFullFlags
                 else Drive^.Flags := 0;}
      GotoFile(S);
      Rebound;
    end;
 end;

 procedure _CtrlPgDn;
 var
   S: string;
 begin
   CE; if ChangeLocked then Exit;
   if (Files^.Count = 0) then Exit;
   PF := Files^.At(CurPos);
   if PF^.TType = ttUpDir then
   begin
     _CtrlPgUp;
     Exit;
   end;
   if Drive^.DriveType = dtArcFind then
     begin
       _Enter;
       Exit;
     end;
   S := MakeNormName(MakeNormName(PF^.Owner^,{$IFNDEF OS2}GetLFN(PF^.LFN)){$ELSE}PF^.Name{$ENDIF},'.');
   if (PF^.Attr and Directory = 0) then
     begin
       if not ArcViewer(S, S, FreeByte) {AK155: по CtrlPgDn входим в архив}
         then _Enter
{JO: переходим к найденному файлу в архиве    }
        else if PathFoundInArc(S)
{$IFNDEF OS2}
         then GotoSingle(GetLFN(PF^.LFN));
{$ELSE}
         then GotoSingle(PF^.Name);
{$ENDIF}
     end
   else
     GotoFile(S);
 end;

 procedure FindNextSelFile(pNext: Boolean); {-$VIV,VOL 19.05.99---}
 var I: LongInt;

    function CheckIt(I: LongInt): Boolean;
    begin
      CheckIt := False;
      if PFileRec(Files^.At(I))^.Selected then
      begin
        ScrollBar^.SetValue( I );
        CED;
        CheckIt := True;
      end;
    end;

 begin
   if Files = nil then Exit;
   if pNext then begin for I := CurPos + 1 to Files^.Count - 1 do if CheckIt(I) then Break; end
     else begin for I := CurPos - 1 downto 0 do if CheckIt(I) then Break; end;
 end;

 procedure _CheckKB;
 var
   aaa: word;
   I, L, line: longint;
   s, s2: string;
 begin
   case Event.KeyCode of
    { Flash >>> }
    kbAltF3: if ShiftState and 3 <> 0 then
              Message(@Self, evCommand, cmViewText, nil);
    { Flash <<< }
    kbAlt1..kbAlt9: begin
                      I := (Event.KeyCode shr 8)-(kbAlt1 shr 8);
                      CE;
                      if ShiftState and 7 <> 0 then
                        begin
                          if Drive^.DriveType <> dtDisk then Exit;
                          if Msg( dlPromptForQDir, nil, mfQuery+mfYesButton+mfNoButton) <> cmYes then Exit;
                          DisposeStr(DirsToChange[I]);
                          DirsToChange[I] := NewStr(DirectoryName);
                          Message(Application, evCommand, cmUpdateConfig, nil);
                        end else begin
                      if DirsToChange[I] <> nil then
                           DoChange(CnvString(DirsToChange[I]));
                      end;
                    end;
    kbCtrl1..kbCtrl0: Begin
                       Drive^.GetParam((Event.KeyCode shr 8)-(kbCtrl1 shr 8)+1);
                       Owner^.ReDraw;
                      End;
{$IFDEF COLUMNSMENU}
          kbAltMinus: Begin                            {JO}
                       CM_SelectColumn(@Self);
                      End;                             {JO}
{$ENDIF}
    kbAltIns: if ShiftState and kbCtrlShift <>0 then _CtrlIns else
              begin
               if (Drive^.Flags and psShowDescript = 0) and
                 (FMSetup.Options and fmoAlwaysCopyDesc = 0) and (PF^.DIZ = nil) then
                begin
                 if (Drive^.DriveType = dtDisk) then
                 begin
                  s2:='';
                  i:=1;
                  PF:=Files^.At(CurPos);
                  repeat
                   s:=GetPossibleDizOwner(i);
                   Inc(i);
                   if s<>'' then S2:=GetDIZ(s, Line, MakeFileName(PF^.Name), GetLFN(PF^.LFN));
                  until (s='') or (s2<>'');
                  if s2<>'' then begin
                   New(PF^.DIZ);
                   PF^.DIZ^.Owner:=NewStr(S);
                   PF^.DIZ^.DIZ:=NewStr(S2);
                   PF^.DIZ^.Line:= Line;
                   PF^.DIZ^.isDisposable:=True;
                  end;
                  Drive^.EditDescription(PF);
                 end;
                end
                else Drive^.EditDescription(PF);
                CE;
              end;
    kbBack: if (FMSetup.Options and fmoBackGoesBack <> 0) and
               ((ShiftState and 3 <> 0) or ((CmdLine.Str = '') and
                (not CmdLine.StrCleared) and (not QuickSearch))) then
               begin
                 _CtrlPgUp;
                 Exit;
               end;
    kbUp, kbDown, kbUpUp, kbDownUp: begin
                     CtrlWas := ShiftState and kbCtrlShift <> 0;
                   end;
    kbCtrlR: begin Message(@Self, evCommand, cmForceRescan, nil); CE end;
    kbGrayAst, kbCtrlGAst: begin QuickSearch:=False; InvertSelection(@Self, Event.KeyCode = kbCtrlGAst); CE end; { Flash }
    kbAltGPlus, kbAltGMinus: begin SelectExt(Event.KeyCode = kbAltGPlus, 1, 8,
                                             ShiftState and 3 <> 0); CE end;
    kbCtrlGPlus, kbCtrlGMinus:
       begin
         if ShiftState and 3 = 0 then SelecType(Event.KeyCode = kbCtrlGPlus)
                                   else SelectExt(Event.KeyCode = kbCtrlGPlus, 10, 3, False); CE
       end;
    kbCtrlBSlash:begin
                 Drive^.ChangeRoot;
                 ReadDirectory;
                 AddToDirectoryHistory(DirectoryName, Drive^.DriveType);
                 ScrollBar^.SetValue(0);
                 DecDrawDisabled;
                 if Drive^.DriveType in [dtArc, dtArvid, dtList] then {-$VOL}
                 begin _CtrlPgUp; Exit; end;{-$VOL}
                 Owner^.Redraw; CE end;
kbCtrlRight, kbCtrlLeft: _DoCtrl;
kbCtrlDel: if Files^.Count > 0 then
            begin
             if ShiftState and 3 = 0 then { Flash 20-02-2003 }
              begin
              I := 0;
              if SelNum = 0 then
                begin
                 if PFileRec(Files^.At(CurPos))^.TType <> ttUpDir then
                    Files^.AtFree(CurPos);
                end
               else
                while I < Files^.Count do
                 if PFileRec(Files^.At(I))^.Selected then
                  begin
                    Files^.AtFree(I);
                    if I < CurPos then Dec(CurPos);
                  end else Inc(I);
              end else
              { Flash 20-02-2003 >>> }
              begin
              if PFileRec(Files^.At(CurPos))^.TType <> ttUpDir then
                 Files^.AtFree(CurPos);
              end;
              { Flash 20-02-2003 <<< }
              ScrollBar^.SetParams(CurPos, 0, Files^.Count - 1,
                                 (Size.Y-Byte(FMSetup.Show and fmsColumnTitles<> 0)) *
                                 ((Size.X+1) div LineLength), 1);
              Recount;
              CE;
            end;
kbCtrlIns,kbShiftIns: if ShiftState and kbCtrlShift<>0 then _CtrlIns;
kbCtrlEnter: _CtrlEnter;
    kbLeft: if (Size.X + 2 > LineLength) then
             if (Delta>0) or
                (ScrollBar^.Value >
                 Size.Y - Byte(FMSetup.Show and fmsColumnTitles<>0)) then begin
              if ScrollBar^.Value - Delta < Size.Y - Byte(FMSetup.Show and fmsColumnTitles<>0)
               then Dec(Delta, Size.Y - Byte(FMSetup.Show and fmsColumnTitles<>0));
              ScrollBar^.SetValue(ScrollBar^.Value - Size.Y + Byte(FMSetup.Show and fmsColumnTitles<> 0));
              CE;
             end else begin
              if ScrollBar^.Value > 0 then
              ScrollBar^.SetValue(0);
              CE;
             end
            else if DeltaX > 0 then begin
                  Dec(DeltaX);
                  DrawView;
                  if InfoView <> nil then InfoView^.DrawView;
                  CE;
                 end;
    kbRight: if Size.X + 2 > LineLength then begin
              aaa := Delta;
              Owner^.Lock;
              ScrollBar^.SetValue(ScrollBar^.Value + Size.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0));
              if aaa<>delta then
               Delta := aaa + Size.Y -
                        Byte(FMSetup.Show and fmsColumnTitles<>0);
               CED;
               Owner^.Unlock;
              end else if DeltaX < LineLength - Size.X - 1 then begin
                        Inc(DeltaX);
                        DrawView;
                        if InfoView <> nil then InfoView^.DrawView;
                        CE;
                       end;
    kbHome: begin CE; DeltaX := 0; OldDelta := -1; ScrollBar^.SetValue(0); Owner^.ReDraw end;
    kbEnd:  begin CE; OldDelta := -1; ScrollBar^.SetValue(Files^.Count-1) end;
    kbAltEnter,kbMenu: { Michael Averbukh } { X-Man }
    begin
      ShiftState := ShiftState or kbAltShift; { Michael Averbukh }
      if _AltEnter then Exit else
      begin
        _GotoExt;
        Exit;
      end;
    end;
    kbEnter: if CurPos < Files^.Count then
             begin
               PShootState := ShiftState;
               PF := Files^.At(CurPos);
               if ChangeLocked or (Message(Application, evCommand, cmExecCommandLine, nil) <> nil) then
               begin CE; Exit; end;
               CE;
               FreeStr := 'AUTOEXEC BAT'; FreeStr[9] := PF^.Name[9];
               if (Drive^.DriveType = dtDisk) and
                  (PF^.Attr and Directory = 0) and {-$VOL}
                  (UpStrg(PF^.Name) = FreeStr) and
                  (Msg(dlAutoexecWarning,nil,mfYesNoConfirm) <> cmYes) then Exit;
(* piwamoto
Оригинальный код из ДН/2 не работает - где-то я накосячил при перетаскивании?
Или в ДН/2 было четное количество багов, а перетащено нечетное ? ;-)
На правой панели ищем все файлы.
Далее нажимаем Tab, Tab, Alt-F1, Enter, Enter, Alt-F1, Enter.
После этого левая панель напрочь залипает, и по Alt-F1 перейти уже никуда не
получится. Меняется только заголовок панели.

               if (Drive^.DriveType in [dtFind, dtTemp, dtArcFind]) or
                 ((Drive^.DriveType = dtList) and (PF^.TType <> ttUpDir)) then
                   begin _Enter; Exit; end;
*)
               if (Drive^.DriveType in [dtFind, dtArcFind, dtTemp, dtList]) then
               begin
                if PF^.TType = ttUpDir then _CtrlPgUp else _Enter;
                Exit;
               end;

               if (FMSetup.Options and fmoEnterArchives <> 0) and
                  (Event.KeyCode = kbEnter) and (ShiftState and 3 = 0)
                  and (PF^.Attr and Directory = 0) and
                  (not (Drive^.DriveType in [dtArc,dtFind,dtArvid,dtArcFind]))
                  and (ArcViewer(MakeNormName(PF^.Owner^, GetLFN(PF^.LFN)),
                                 MakeNormName(PF^.Owner^, GetLFN(PF^.LFN)),
                                 Drive^.Param))
                  then begin CE; Exit; end;
               if (PF <> nil) then
               if (PF^.Attr and Directory <> 0) then
               begin
                if PF^.TType = ttUpDir then
                    begin
                     if not ChangeUp then GotoFile(PF^.Owner^);
                     end
                   else begin
                         PF^.Name := MakeFileName(PF^.Name);
                         GotoFile(MakeNormName(MakeNormName(PF^.Owner^,GetLFN(PF^.LFN)),'.'));
                        end;
                Exit;
               end else
               _GotoExt;
             end;
    kbCtrlPgUp: _CtrlPgUp;
    kbCtrlPgDn: _CtrlPgDn;
    kbDel: if ((CmdLine.Str = '') and (FMSetup.Options and fmoDelErase <> 0))
              and (not CmdLine.StrCleared) then
            Message(@Self, evCommand, cmPanelErase, nil);
    kbShiftDel: if ((CmdLine.Str = '') and (FMSetup.Options and fmoDelErase <> 0)) then
              Message(@Self, evCommand, cmSingleDel, nil);
    kbIns,kbSpace: if CurPos < Files^.Count then
           begin
            QuickSearch := False;
            if (Event.CharCode = ' ') and ((CmdLine.Str <> '') or
               (FMSetup.Options and fmoSpaceToggle = 0)) then Exit;
            CE; if Files^.Count = 0 then Exit;
            PF := Files^.At(CurPos);
            if PF^.TType <> ttUpDir then
            begin
             PF^.Selected := not PF^.Selected;
             SelectedLen := SelectedLen - (1-2*Integer(PF^.Selected))*PF^.Size;
             PackedLen := PackedLen - (1-2*Integer(PF^.Selected))*PF^.PSize;
             Dec(SelNum, 1-2*Integer(PF^.Selected));
            end;
            ScrollBar^.SetValue(CurPos + 1);
            if CurPos = ScrollBar^.Value then DrawView;
            if InfoView <> nil then InfoView^.DrawView;
           end;
    kbGrayPlus: begin SelectFiles(@Self, True, ShiftState and 3 <> 0); CE end;
    kbGrayMinus: begin SelectFiles(@Self, False, ShiftState and 3 <> 0); CE end;
    kbAltUp, kbAltDown: FindNextSelFile( Event.KeyCode = kbAltDown ); {-$VIV}
{--- start -------- Eugeny Zvyagintzev ---------}
    kbEsc: if (InterfaceData.Options and ouiHideCmdline <> 0) and
              (InterfaceData.Options and ouiEsc <> 0) then
            Message(Application, evCommand, cmShowUserScreen, nil);
{--- finish -------- Eugeny Zvyagintzev ---------}
    else if ShowKeyCode and 1>0 then
          begin
           l:=Event.KeyCode;
           MessageBox('%x',@l,0);
          end;
   end;
 end;

procedure _GetCurrentPosFiles;
var
  S: string;
begin
  if (Drive^.DriveType < dtArcFind) and (PF <> nil) then
    S := MakeNormName(PF^.Owner^, MakeFileName(PF^.Name))
    else S := '';
  if ActivePanel = @Self then CurFileActive := NewStr(S) else
  begin
    CurFilePassive := NewStr(S);
    CE;
  end;
end;

 procedure _TagUntag;
 var
   I: LongInt;
 begin
   if (PF <> nil) and (PF^.Selected xor (Event.Command = cmSingleTag))
    then I := kbIns else I := kbDown;
   Message(Owner, evKeyDown, I, nil);CE;
 end;

 procedure _ForceRescan;
 var
   PDr: PDrive;
 begin
   if Drive^.DriveType = dtArc then
   begin
     if PArcDrive(Drive)^.ReadArchive then RereadDir else
     begin
       PDr := Drive;
       Drive := Drive^.Prev;
       PDr^.Prev := nil;
       Dispose(PDr,Done);
       DeltaX := 0;
       Drive^.SizeX := Size.X;
       DirectoryName := Drive^.GetDir;
       ReadDirectory;
       AddToDirectoryHistory(DirectoryName, Drive^.DriveType);
       Rebound;
     end;
   end
   else
     begin
       if Drive^.DriveType in [dtTemp, dtFind, dtList, dtArcFind] then
         Drive^.RereadDirectory('');
       RereadDir;
     end;
   CE;
 end;

 procedure _FindForced;
 var
   PDr: PDrive;
 begin
   ForceReading := False;
   DeltaX := 0;
   PDr := Drive;
   Drive := Event.InfoPtr;
   Drive^.Prev := PDr^.Prev;
   PDr^.Prev := nil;
   Dispose(PDr,Done);
   RereadDir;
   CE;
 end;

 {$IFDEF UUDECODE}
 procedure CallUuDecode;
 var
   FC: PCollection;
 begin
   FC:=GetSelection(@Self, False);
   if FC = nil then Exit;
   if FC^.Count <> 0 then
     begin UuDecode(FC); FC^.DeleteAll; end;
   Dispose(FC,Done);
 end;
 {$ENDIF}

var
  I: LongInt;
  FC: PFilesCollection;
  PSDEL: byte;
begin
 if Drive<>nil
  then WasFull := Drive^.GetFullFlags = Drive^.Flags
  else WasFull := false;
 if ScrollBar<>nil
  then CurPos := ScrollBar^.Value
  else CurPos := 0;
 if Files<>nil
  then if Files^.Count > CurPos
        then PF := Files^.At(CurPos)
        else PF := nil
  else PF := nil;
 case Event.What of
  evCommand: case Event.Command of
              cmToggleDescriptions: begin CM_ToggleDescriptions(@Self); CE end;
              cmToggleLongNames: begin CM_ToggleLongNames(@Self); CE end;
              cmToggleShowMode: begin CM_ToggleShowMode(@Self); CE end;
              cmFastRename: begin
                             if QuickRenameInDialog xor
                                (ShiftState and (kbLeftShift+kbRightShift)<>0)
                              then CM_RenameSingleDialog(@Self, @Event)
                             else
                              if Drive^.Flags and psShowLongName>0
                               then CM_RenameSingleL(@Self, @Event)
                               else CM_RenameSingleS(@Self, @Event);
                              CE
                            end;
              cmSingleTag, cmSingleUnTag : _TagUntag;
              cmQuickChange1..cmQuickChange9:
                               begin
                                 DoChange(CnvString(DirsToChange[Event.Command-cmQuickChange1]));
                                 CE;
                               end;
              cmPanelMakeList: begin
                                 Message(@Self, evCommand, cmInsertDrive,
   {$IFNDEF OS2}
                                        New(PFindDrive, InitList(MakeNormName(PF^.Owner^,MakeFileName(PF^.Name)))));
   {$ELSE}
                                        New(PFindDrive, InitList(MakeNormName(PF^.Owner^,GetLFN(PF^.LFN)))));
   {$ENDIF}
                                 CE;
                               end;
              cmPanelArcFiles:  begin CM_ArchiveFiles(@Self); CE end;
              cmExtractArchive: begin
                                   if (Drive^.DriveType <> dtArc) and
                                      (Drive^.DriveType <> dtArvid) and
                                      (Drive^.DriveType <> dtArcFind) and
                                      (PF <> nil) and
                                      (PF^.Attr and Directory = 0) then
                                     UnarchiveFiles(MakeNormName(PF^.Owner^, GetLFN(PF^.LFN)));
                                   CE
                                end;
              cmReboundPanel: Rebound;
{$IFDEF PRINTMANAGER}
              cmPrintFile: begin CM_Print(@Self); CE end;
{$ENDIF}
              cmSetPassword, cmExtractTo,
              cmArcTest, cmRereadForced,
              cmMakeForced
              : HandleCommand;
              cmEraseGroup: EraseGroup;
              cmFindTree: if (Drive^.DriveType = dtDisk) and
                             (PanelFlags and fmiDirLen <> 0) and
                             (Char(Event.InfoPtr^) = DirectoryName[1])
                              then Char(Event.InfoPtr^) := #0;
              cmInsertFile: if Files<>nil then begin
                             Files^.AtInsert(Files^.Count, Event.InfoPtr);
                             ScrollBar^.SetParams(ScrollBar^.Value, 0, Files^.Count - 1,
                                                  (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0)) *
                                                  ((Size.X+1) div LineLength), 1);
                             DrawView;
                             CE;
                            end;
              cmViewText, cmViewHex, cmViewDBF, cmViewWKZ:
               begin
                 if Files<>nil then
                  if Files^.Count >= CurPos then Drive^.UseFile(Files^.At(CurPos), 22000 + Event.Command);
                 CE;
               end;
              cmInsertDrive: if Event.InfoPtr <> nil then InsertDrive else CE;

              cmEditFile, cmIntViewFile, cmIntEditFile, cmViewFile:
                         begin
                           FreeByte:=Drive^.Param;
                           if isSelDir then
                             case Event.Command of
                              cmViewFile,cmIntViewFile :  if Drive^.DriveType <> dtArc then CountLen else CE;
                              cmEditFile,cmIntEditFile :  begin CM_SetAttributes(@Self, True, CurPos); CE end;
                             end { case }
                           else
                           begin
                             ViewFile(Event.Command); CE;
                           end;
                         end;
              cmCountLen: if Drive^.DriveType <> dtArc then CountLen else CE;
              cmSingleDel: CM_EraseFiles(@Self, True);
              cmGetFileName: begin _GetFName; CE end;
              cmTempCopyFiles: begin CM_CopyTemp(@Self); CE end;
              cmSortBy: begin CM_SortBy(@Self); CE end;
              cmPanelSetup: begin Setup; CE end;
              cmSetupColumns: begin CM_SetShowParms(@Self); CE end;
              cmTouchFile: begin
                            if (PF <> nil) then
                              if (PF^.Attr and Directory <> 0) then
                                  Message(@Self, evKeyDown, kbCtrlPgDn, nil) else
                                  if InExtFilter(MakeFileName(PF^.Name), Executables) then
                                      Message(@Self, evKeyDown, kbEnter, nil) else
                                      ViewFile(cmViewFile);
                            CE;
                           end;
              cmCompareDir: begin
                             Message(Owner, evCommand, cmPanelCompare, Files);
                             Recount; CE;
                            end;
              cmPushName: if UpStrg(CurrentDirectory) <> UpStrg(Drive^.GetRealName) then _PushName;
              cmPushFirstName: _DoPush(False);
              cmPushFullName: _DoPush(True);
              cmPanelCompare: begin CM_CompareDirs(@Self, Event.InfoPtr); Recount; CE end;
              cmPanelSelect: begin SelectFiles(@Self, True,False); CE end;
              cmPanelUnselect: begin SelectFiles(@Self, False,False); CE end;
              cmPanelXSelect: begin SelectFiles(@Self, True,True); CE end;
              cmPanelXUnselect: begin SelectFiles(@Self, False,True); CE end;
              cmCopyFiles,cmMoveFiles,cmSingleCopy,cmSingleRename:
                begin
                  CM_CopyFiles(@Self,
                    (Event.Command = cmMoveFiles) or
                     (Event.Command = cmSingleRename),
                    (Event.Command = cmSingleCopy) or
                     (Event.Command = cmSingleRename));
                  CE
                end;
              cmPanelInvertSel: begin InvertSelection(@Self, False); CE end;
              cmPanelErase: begin CM_EraseFiles(@Self, False); CE end;
              cmPanelMkDir: begin CM_MakeDir(@Self); CE end;
              cmRereadDir: if GetState(sfVisible) then _DoRereadDir;
              cmTotalReread: if GetState(sfVisible) or (Drive^.DriveType <> dtDisk) then
                                begin
                                  Drive^.RereadDirectory('');
                                  RereadDir;
                                end;
              cmForceRescan: _ForceRescan;
              cmPanelReread: begin RereadDir; CE end;
              cmMakeList: begin CM_MakeList(@Self); CE end;
              cmChangeDrive: _ChangeDrive;
              cmChangeDrv: _ChangeDrv;

              cmDirBranch: begin
                           Message(@self, evCommand, cmInsertDrive,
                               Drive^.OpenDirectory(DirectoryName));
                           CE
                           end;
{$IFDEF UUDECODE}
              cmUUDecodeFile:
                begin
                  if Drive^.DriveType < dtArcFind then CallUuDecode;
                  CE;
                end;
{$ENDIF}
{$IFDEF UUENCODE}
              cmUUEncodeFile: begin
                          if (PF <> nil) and (PF^.Attr and Directory = 0)
                             and (Drive^.DriveType < dtArcFind) then
                              UUEncode(MakeNormName(PF^.Owner^, MakeFileName(PF^.Name)));
                          CE;
                        end;
{$ENDIF}
           cmChangeNameCase: if (PF <> nil) and (Drive^.DriveType < dtArcFind)
                              then CM_ChangeCase(@Self);
{$IFDEF DiskImg}
               cmUnpDiskImg: begin
                              if (Drive^.DriveType < dtArcFind) then
                               UnpackDiskImages(@Self,
                                                GetSelection(@Self, False));
                              CE;
                             end;
{$ENDIF}
              cmChangeDirectory: begin DoChange(PString(Event.InfoPtr)^); CE; end;
              cmChangeDir: begin
                            FreeStr := CM_ChangeDirectory(@Self);
                            if FreeStr <> '' then
                              begin
                                if Drive^.DriveType = dtArc then
                                  ReplaceDrive(FreeStr[1]);
                                GotoFile(MakeNormName(FreeStr,'.'));
                              end;
                            CE
                           end;
              cmGetDirInfo: if Drive <> nil then
                              begin
                                Drive^.GetDirInfo(PDiskInfoRec(Event.InfoPtr)^);
                                CE;
                              end;
                  cmDirHistory: _DoDirHistory;
{$IFDEF MODEM}
{$IFDEF LINK}
              cmCloseLinked: _CloseLinked;
{$ENDIF}
{$ENDIF}
              cmSetFAttr,cmSingleAttr: begin CM_SetAttributes(@Self, Event.Command=cmSingleAttr,CurPos); CE end;
{$IFDEF LongCopy}
              cmPanelLongCopy: begin CM_LongCopy(@Self); CE end;
{$ENDIF}
              cmEnableView: begin ViewEnabled := True; SendLocated; end;
              cmDisableView: begin ViewEnabled := False; end;
              cmLViewFile: begin SendLocated; CE end;
              cmFindFile: if Drive^.DriveType in [dtArc,dtArvid] then
                            begin Drive^.HandleCommand(cmFindFile, nil); CE end;
              cmFindGotoFile: begin FileMask := x_x;
                                    GotoFile(PString(Event.InfoPtr)^); CE;
                                    Owner^.Select; Select;
                                    GlobalMessage(evCommand, cmRereadInfo, nil);
                              end;
              cmAdvFilter: begin CM_AdvancedFilter(@Self); CE end;
{--- start -------- Eugeny Zvyagintzev ---- 10-02-2003 ----}
              cmEnableFileFindPanels:
               If Drive^.DriveType In [dtFind,dtTemp,dtArcFind ] Then
                DecDrawDisabled;
              cmDisableFileFindPanels:
               If Drive^.DriveType In [dtFind,dtTemp,dtArcFind ] Then
                IncDrawDisabled;
              cmReReadFileFindPanels:
               If Drive^.DriveType In [dtFind,dtTemp,dtArcFind ] Then
                _ForceRescan;
{--- finish -------- Eugeny Zvyagintzev ---- 10-02-2003 ----}
             end;
  evKeyDown: _CheckKB;
  evBroadcast: case Event.Command of
                 cmGetCurrentPosFiles: _GetCurrentPosFiles;
                 cmFindForced: if ForceReading then _FindForced;
                 cmInsertDrive: InsertDrive;
                 cmUnarchive: if Drive^.DriveType = dtArc then begin
                                Drive^.CopyFiles(PCopyRec(Event.InfoPtr)^.FC, @Self, True);
                                CE;
                              end;
                 cmCopyCollection: if Drive^.DriveType < dtArcFind then begin
                                Drive^.CopyFiles(Event.InfoPtr, @Self, ShiftState and 7 <> 0);
                                CE;
                              end;
                 cmDropped: if MouseInView(PCopyRec(Event.InfoPtr)^.Where) then
                            begin
                              case Drive^.DriveType of
                                dtFind,dtArcFind, dtList :;
                              else CM_Dropped(@Self, Event.InfoPtr);
                              end;
                              CE;
                            end;
(*AK155 19-06-2002. Такого, вроде, не бывает никогда. Почти идентичный
кусок есть в flpanel, так он действительно работает. И если тот кусок
закоментарить, то DN глючит, но сюда управление все равно не попадает.
                 cmScrollBarChanged: if ScrollBar = Event.InfoPtr then begin
                                      if MSelect then
                                        begin
                                         if Files<>nil then begin
                                          CE; if Files^.Count = 0 then Exit;
                                          PF := Files^.At(ScrollBar^.Value);
                                          if not PF^.IsUpDir
                                                  and (PF^.Selected xor SelectFlag) then
                                           begin
                                            PF^.Selected := SelectFlag;
                                            if SelectFlag then begin Inc(SelNum); SelectedLen := SelectedLen + PF^.Size;
                                                                     PackedLen := PackedLen + PF^.PSize end
                                                          else begin Dec(SelNum); SelectedLen := SelectedLen - PF^.Size;
                                                                     PackedLen := PackedLen - PF^.PSize end;
                                           end;
                                         end;
                                        end;
                                      PosChanged := True;
                                      if InfoView <> nil then InfoView^.DrawView;
                                      CED; PosChanged := False;
                                      if (RepeatDelay <> 0) and ViewEnabled then
                                        NeedLocated := GetSTime;
                                     end; AK155*)
               end;
  evMouseDown: begin
                if Files^.Count = 0 then Exit;
                QuickSearch := false; {AK155}
                MSelect := Event.Buttons and mbRightButton <> 0;
                MakeLocal(Event.Where, MPos);
                if (Drive^.Flags and psShowLongName = 0) or ((MPos.X mod LineLength) <> Drive^.LFNLen) then
(*              if (Drive^.Flags and psShowLongName = 0) or ((MPos.X mod LineLength) <> Drive^.LFNLen-Drive^.EXTLen-1) then*)
                 begin
                  if (MPos.Y = 0) and (FMSetup.Show and fmsColumnTitles <> 0) then
                   begin
                    LastRDelay := RepeatDelay; RepeatDelay := 0;
                    if MSelect then
                     begin
                      PF := Files^.At(CurPos);
                      SelectFlag := not PF^.Selected;
                     end;
                    repeat
                     PSpecScroll(ScrollBar)^.ForceScroll := not MSelect; {AK155}
                     Message(Owner, evKeyDown, kbUp, nil)
                    until not MouseEvent(Event, evMouseMove + evMouseAuto);
                    RepeatDelay := LastRDelay; CE; MSelect := False;
                    Exit;
                   end;
                  MakeLocal(Event.Where, MPos);
                  CurPos := Delta + (MPos.X div LineLength)* (Size.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0))
                            + MPos.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0);
                  if Event.Double then
                   begin
                    MSelect := False;
                    CE; if Files^.Count = 0 then Exit;
                    if CurPos < Files^.Count then
                      begin
                       if ScrollBar^.Value <> CurPos then ScrollBar^.SetValue(CurPos)
                       else
                         begin
                            PF := Files^.At(ScrollBar^.Value);
                            if PF^.Attr and Directory <> 0 then
                             begin Message(@Self, evKeyDown, kbCtrlPgDn, nil); CE; Exit end;
                            if ShiftState and kbCtrlShift <> 0 then Message(@Self, evKeyDown, kbCtrlEnter, nil)
                                else begin
                                        Message(CommandLine, evKeyDown, kbDown, nil);
                                        Message(@Self, evKeyDown, kbEnter, nil);
                                        CE
                                     end;
                         end;
                      end
                   end
                  else
                  begin
                   LastRDelay := RepeatDelay; RepeatDelay := 0;
                   if MSelect and (CurPos < Files^.Count) then
                    begin
                     CurPos := Delta + (MPos.X div LineLength)* (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0))
                               + MPos.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0);
                     if (CurPos >= Files^.Count) then CurPos := Files^.Count - 1;
                     if (CurPos < 0) then CurPos := 0;
                     PF := Files^.At(CurPos);
                     SelectFlag := not PF^.Selected;
                     ScrollBar^.SetValue(CurPos);
                     Message(@Self, evBroadcast, cmScrollBarChanged, ScrollBar);
                     if PF^.TType = ttUpDir then PF^.Selected := False;
                    end;
                  if not MSelect and (FMSetup.Options and fmoDragAndDrop <> 0)
                    then begin
                           MSelect := False; RepeatDelay := LastRDelay;
                           CurPos := Delta + (MPos.X div LineLength) * (Size.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0))
                                     + MPos.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0);
                           if CurPos < Files^.Count then
                            begin
                             ScrollBar^.SetValue(CurPos);
                             if MouseEvent(Event, evMouseMove) then CM_DragDropper(@Self, CurPos, @Event);
                            end;
                           CE; Exit;
                         end;
                   repeat
                    MakeLocal(Event.Where, MPos);
                    if MouseInView(Event.Where) then
                     begin
                      CurPos := Delta + (MPos.X div LineLength) * (Size.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0))
                                + MPos.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0);
                      if CurPos < Files^.Count then ScrollBar^.SetValue(CurPos);
                     end else
                     begin
                      if MPos.Y < 0 then ScrollBar^.SetValue(ScrollBar^.Value - 1)
                       else if MPos.Y >= (Size.Y - 1) then ScrollBar^.SetValue(ScrollBar^.Value + 1);
                     end;
                   until not MouseEvent(Event, evMouseAuto + evMouseMove + evMouseDown);
                   RepeatDelay := LastRDelay; SendLocated;
                  end; CE; MSelect := False;
                 end
(*               else
                 begin
                  psdel:=(MPos.X div LineLength) + 1;
                  Repeat
                   if (Drive^.LFNLen - (LineLength*psdel - MPos.X) > 5) and
                      (LineLength*psdel - MPos.X >=0) then
                    Drive^.EXTLen:=LineLength*psdel - MPos.X;
                   {Owner^.ReDraw;}
                   DrawView;
                   MouseEvent(Event, evMouseMove+evMouseUp);
                   MakeLocal(Event.Where, MPos);
                  Until Event.What=evMouseUp;
                  CE;
                 end*)
                 else
                 begin
                  psdel:=MPos.X div LineLength;
                  Repeat
                   if MPos.X - LineLength*psdel - Drive^.EXTLen >= 5 then
                    begin
                     Drive^.LFNLen:=MPos.X div (psdel+1);
                     LineLength:=Drive^.CalcLength;
                    end;
                   Owner^.ReDraw;
                   MouseEvent(Event, evMouseMove+evMouseUp);
                   MakeLocal(Event.Where, MPos);
                  Until Event.What=evMouseUp;
                  CE;
                 end;
              end;
 end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
 procedure TFilePanelRoot.ChDir;
 begin
   if Drive^.DriveType <> dtDisk then Exit;
   if (Dir[0] > #3) and (Dir[Length(Dir)] = '\') then Dec(Dir[0]);
   Drive^.lChDir(Dir);
   IncDrawDisabled;
   ReadDirectory;
   AddToDirectoryHistory(DirectoryName, Drive^.DriveType);
   ScrollBar^.SetValue(0);
   DecDrawDisabled; DrawView;
   GlobalMessage(evCommand, cmRereadInfo, nil);
   Owner^.Redraw;
 end;
        {-DataCompBoy-}

END.
