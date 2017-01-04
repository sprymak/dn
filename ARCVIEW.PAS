{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.12
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

Unit ArcView;

Interface uses Dos, LFN, LFNCol, tree, Objects, Views, Drivers, FViewer,
               FlPanel, FilesCol, DiskInfo, DblWnd, HistList, HideView,
               RStrings, Drives, Commands, Archiver, FStorage, Histries,
               advance, advance1, advance2, advance3, archdet, collect
               {$IFNDEF MINARCH}, Arc_tgz, gzio{$ENDIF}
               ;
type
    PArcDrive = ^TArcDrive;
    TArcDrive = object(TDrive)
     ArcName:     String; {DataCompBoy}
     AType:       PARJArchive;
     Files:       PDirStorage;
     KillAfterUse:Boolean;
     ArcDate:     LongInt;
     ForceRescan: Boolean;
     Password:    String;
     constructor Init(const AName: String; viewmode: byte);
     constructor InitCol(PC: PDirStorage; const AName: String);
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
     {procedure  GetDown(var B; C: Word; P: PFileRec); virtual;}
     procedure   GetFull(var B; P:PFileRec; C, SC:Word); virtual;{DataCompBoy}
     procedure   GetEmpty(var B; SC: Word); virtual;
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
    end;

    function ArcViewer(AName : String; num: byte): Boolean; {DataCompBoy}
    procedure StdMsg(MsgNo: Byte);

const
    ArcPasw    : String[32] = '';

type
    PFInfo     = ^TFInfo;
    TFInfo     = Record
                  LFN     : String; {DataCompBoy}
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

Implementation
 uses Menus, DnApp, Messages, Dialogs, Gauge, FileCopy, Memory, Startup,
      Arvid, xTime, VideoMan;

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
   EXTLen:=StoI(Params[n].EXTLen);
   case FileCase of
    0: FilFLP := 0;
    1: FilFLP := flnLowCase;
    2: FilFLP := flnCapitalCase;
    3: FilFLP := flnUpCase;
    4: FilFLP := 65535;
   end;
   case DirsCase of
    0: DirFLP := 0;
    1: DirFLP := flnLowCase;
    2: DirFLP := flnCapitalCase;
    3: DirFLP := flnUpCase;
    4: DirFLP := 65535;
   end;
  end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
constructor TArcDrive.Init;
 var SR: lSearchRec;
     xt: string;
     I: Integer;
     Q: String;
begin
 TObject.Init;
 if viewmode<1 then GetParam(1)
               else GetParam(viewmode);
 innum:=viewmode;
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
 var  Dr: String;
      Nm: String;
      Xt: String;
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
 lFSplit(ArcName, Dr, Nm, Xt);
 if ExistFile(ArcName) then
  New(ArcFile,Init(ArcName, stOpenRead, 512))
 else ArcFile:=nil;
 ArcFileName := ArcName;
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
 var  Dr: String;
      Nm: String;
      Xt: String;
      SR: lSearchRec;
label
  Failure;
begin
 inherited Load(S);
 S.Read(ArcName[0],1);
 S.Read(ArcName[1],Length(ArcName));
 S.Read(KillAfterUse, 1);
 S.Read(Password[0],1);
 S.Read(Password[1],Length(Password));
 Files := PDirStorage(S.Get);
 ForceRescan := Off;
 DriveType := dtArc;
 lFSplit(ArcName, Dr, Nm, Xt);
 ArcFileName := ArcName;
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
 if KillAfterUse then EraseFile(ArcName);
end;

procedure TArcDrive.Store;
begin
 inherited Store(S);
 S.Write(ArcName[0],1 + Length(ArcName));
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
    i  : Integer;
    ev : TEvent;
    P  : PWhileView;
    R  : TRect;
    Dr : String;
    Nm : String;
    Xt : String;
    tmp: String;
    Ln : LongInt;
    PLn: LongInt;
    Cancel: Boolean;
    T: TEventTimer;
    olf: String;
begin
 ReadArchive:=Off;
 lFSplit(ArcName, Dr, Nm, Xt);
 New(ArcFile,Init(ArcName, stOpenRead, 512));
 ArcFileName := ArcName;
 if (ArcFile=nil) or (ArcFile^.Status <> stOK) then
  begin
    Dispose(ArcFile, Done); ArcFile:=nil;
    StdMsg4;
    Exit;
  end;
 if Files <> nil then Dispose(Files, Done); Files:=nil;
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
 If ArcFile<>nil then LN := ArcFile^.GetSize + 1 else begin
  LN:=0;
  {$IFNDEF MINARCH}
  If TypeOf(AType^)=TypeOf(TTGZArchive)
   then PLn:=FileSize(PTGZArchive(AType)^.gzf^.gzFile.F);
  {$ENDIF}
 end;
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
      If LN<>0 then
       P^.Write(2, Copy(Strg(#219,((ArcFile^.GetPos+1)*25) div LN)+Strg(#177, 25), 1, 25))
      {$IFNDEF MINARCH}
      else if PLn<>0 then
            P^.Write(2,Copy(Strg(#219,((FilePos(PTGZArchive(AType)^.gzf^.gzFile.F)+1)*25) div PLn )+Strg(#177, 25),
                            1, 25))
      {$ENDIF}
      ;
      P^.Write(3, ItoS(Files^.Files)+GetString(dlFilesFound));
      NewTimer(T, 3);
    end;
  AType^.GetFile;
  if FileInfo.Last=0 then
     begin
      DelLeft(FileInfo.FName);
      if not (FileInfo.FName[1] in ['\','/']) then FileInfo.FName := '\' + FileInfo.FName;
      Replace('/', '\', FileInfo.FName); UpStr(FileInfo.FName);
      olf:=GetLFN(FileInfo.LFN);
      DelLFN(FileInfo.LFN);
      if not (olf[1] in ['\','/']) then olf:='\'+olf; Replace('/', '\', olf);
      Replace('\.','\',FileInfo.FName);
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
 GetDir := AType^.GetSign+Copy(Dr,1,2)+Nm+Xt+CurDir;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TArcDrive.GetDirectory;
 var F: PFileRec;
     FA: PArcFile;
     I: LongInt;
     si:LongInt;
     Dr: String;
     Nm: String;
     Xt: String;
     DT: DateTime;
     AllFiles: Boolean;
     AFiles, FD: PFilesCollection;
     TTL, TPL: TSize;
     _USize, _PSize: TSize;
     FR: TFileRec;
     OW: Pointer;

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

 F := NewFileRec('..',{$IFNDEF OS2}'..'{$ENDIF},Round(TTL), ArcDate, Directory, OW);
 F^.Attr := $8000 or F^.Attr;
 F^.PSize := Round(TPL);
 AFiles^.AtInsert(0, F);
 FD^.DeleteAll; Dispose(FD, Done);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.UseFile;
 var SS, S, S2, Q: String;
     C: Char;
     ExecOK: Boolean;
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
     if ExecResource( dlgSetPassword, S ) <> cmOK then Exit;
     S := ' ' + DelSpaces(CnvString(AType^.Garble) + S) + ' ';
    end;
   SS := MakeNormName(P^.Owner^, GetLFN(P^.LFN));
   if SS[1] = '\' then DelFC(SS);
   {$IFNDEF OS2}
   if ArcName[Length(ArcName)]='.' then
    S := CnvString(AType^.Extract)+' '+s+lfGetShortFileName(ArcName)+'. '+SquashesName(SS)+' '
   else
    S := CnvString(AType^.Extract)+' '+s+lfGetShortFileName(ArcName)+' '+SquashesName(SS)+' ';
   {$ELSE}
   S := CnvString(AType^.Extract)+' '+s+SquashesName(ArcName)+' '+SquashesName(SS)+' ';
   {$ENDIF}
   DelDoubles('  ',S);
   TempFile := C + MakeNormName(TempDir , GetLFN(P^.LFN));
   Q:='|' + GetRealName+':'+MakeNormName(CurDir, GetLFN(P^.LFN));
   if C in ['<','-','|','+'] then TempFile := TempFile + Q;
   S2:=Copy(TempFile,2,255); ExecOK:=True;
   if not ExistFile(S2) or (PackedDate(P)<>FileTime(S2)) then
   begin
    lGetDir(0, DirToChange);
    Advance2.lChDir(TempDir);
    ExecOK := Exec(CnvString(AType^.unPacker),s);
    Advance2.lChDir(DirToChange); DirToChange := '';
   end;
   if ExecOK then
   begin
     TempFile := MakeNormName(TempDir, GetLFN(P^.LFN));
     if Q<>'' then TempFile:=TempFile+Q;
     While TempFile[Length(TempFile)] = ' ' do Dec(TempFile[0]);
     Message(Application, evCommand, Command, @TempFile);
   end;
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
  if KillAfterUse then EraseFile(ArcName);
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
 for I := 0 to PC^.Count - 1 do
  begin
   PF := PC^.At(I);
   S1 := MakeNormName(PF^.Owner^, GetLFN(PF^.LFN));
   if S1[1] in ['\','/'] then DelFC(S1);
   if PF^.Attr and Directory = 0
      then if B then S := S + ' ' + SquashesName(S1) else WriteLn(F.T, S1)
      else PutDir('\'+S1+'\');
   MakeListFile := S;
   if S[0] > #100 then Exit;
  end;
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
  Pswd := Pswd or (P^.Attr and Hidden <> 0);
 end;

begin
  While ExtrDir[Length(ExtrDir)] = ' ' do Dec(ExtrDir[0]);
  {$IFNDEF OS2}
  if (ExtrDir = '') or (ExtrDir = '..') then lFSplit(lfGetShortFileName(ArcName), ExtrDir, Nm, Xt);
  {$ELSE}
  if (ExtrDir = '') or (ExtrDir = '..') then lFSplit(ArcName, ExtrDir, Nm, Xt);
  {$ENDIF}
  if not (ExtrDir[Length(ExtrDir)] in ['\','/']) then ExtrDir := ExtrDir + '\';

  SS := MakeListFile(AFiles);
  S := ' '; Pswd := Off;
  AFiles^.ForEach(@Unselect);
  ExtrChar := CnvString(AType^.ExtractWP);
  if Options and 1 = 0 then ExtrChar := CnvString(AType^.Extract);
  if Options and 2 <> 0 then ExtrChar := CnvString(AType^.Test);
  if (Password = '') and Pswd then
    if ExecResource(dlgSetPassword, Password) <> cmOK then Exit;
  if Password <> '' then S := ' ' + DelSpaces(CnvString(AType^.Garble) + Password) + ' ';
  {$IFNDEF OS2}
  S := ExtrChar+' '+s+lfGetShortFileName(ArcName)+' '+SS+' ';
  {$ELSE}
  S := ExtrChar+' '+s+SquashesName(ArcName)+' '+SS+' ';
  {$ENDIF}
  lGetDir(0, DirToChange);
  CreateDirInheritance(ExtrDir, On);
  Advance2.lChDir(ExtrDir);
  DelDoubles('  ', S);
  Exec(CnvString(AType^.unPacker),s);
  Advance2.lChDir(DirToChange); DirToChange := '';
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.CopyFiles;
 var SS, S: String;
     DT: Record S: String; W: Word; Psw: String[30]; end;
     ExtrDir: String;
     ExtrChar: String;
     Nm: String;
     Xt: String;
begin
  S := ' ';
  ExtrDir := '';
  DT.S := '';
  DT.Psw := Password;
  DT.W := byte(ExtractWithPathNames);
  Message(Application, evCommand, cmPushFirstName, @DT.S);
  if CopyDirName <> '' then DT.S := CopyDirName;
  if DT.S = cTEMP_ then DT.S := '';
  if DT.S = '' then GlobalMessageL(evCommand, cmPushName, hsExtract);
  if DT.S = '' then DT.S := HistoryStr(hsExtract, 0);
  if DT.S = cTEMP_ then DT.S := '';
  CopyDirName := '';
  if not SkipCopyDialog then
     if ExecResource( dlgExtract, DT) <> cmOK then Exit;
  if ((DT.W and 1)<>0)<>ExtractWithPathNames then ConfigModified:=True;
  ExtractWithPathNames:=(DT.W and 1) <> 0;
  SkipCopyDialog := Off;
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
  S := CnvString(AType^.Delete)+' '+lfGetShortFileName(ArcName)+' '+SS;
  {$ELSE}
  S := CnvString(AType^.Delete)+' '+SquashesName(ArcName)+' '+SS;
  {$ENDIF}
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

function TArcDrive.CalcLength;
begin
 if Flags and psShowLongName <> 0 then
  CalcLength := LFNLen + 1 +
                (10 + CountryInfo.TimeFmt) * Byte(Flags and psShowSize <> 0) +
                11 * Byte(Flags and psShowPacked <> 0) +
                 9 * Byte(Flags and psShowDate <> 0) +
                (7 - CountryInfo.TimeFmt) * Byte(Flags and psShowTime <> 0) +
                 4 * Byte(Flags and psShowRatio <> 0)
 else
  CalcLength := 13 +
                (10 + CountryInfo.TimeFmt) * Byte(Flags and psShowSize <> 0) +
                11 * Byte(Flags and psShowPacked <> 0) +
                 9 * Byte(Flags and psShowDate <> 0) +
                (7 - CountryInfo.TimeFmt) * Byte(Flags and psShowTime <> 0) +
                 4 * Byte(Flags and psShowRatio <> 0)
end;

procedure TArcDrive.MakeTop;
begin
 if Flags and psShowLongName <> 0 then S := CenterStr(GetString(dlTopLFN), LFNLen)+GetString(dlTopSplit)
                                  else S:=GetString(dlTopName);
 if Flags and psShowSize     <> 0 then S := S + Copy(GetString(dlTopOriginal),2-CountryInfo.TimeFmt,255);
 if Flags and psShowPacked   <> 0 then S := S + GetString(dlTopPacked);
 if Flags and psShowRatio    <> 0 then S := S + GetString(dlTopRatio);
 if Flags and psShowDate     <> 0 then S := S + GetString(dlTopDate);
 if Flags and psShowTime     <> 0 then S := S + Copy(' '+GetString(dlTopTime),1+CountryInfo.TimeFmt,255);
end;

procedure TArcDrive.GetFull;
var
  S: String;
  S1: String[40];
  X: Byte;
  NFM: TNameFormatMode;
  OPT: Word;
begin
  if Flags and psShowLongName <> 0 then
   begin
    s:=GetLFN(P^.LFN);
    NFM:=nfmNull;
    if (EXTLen=0) or ((Flags and psVariableExtDA<>0) and (Length(GetExt(s))=1))
     then OPT:=flnPreferName or flnUseCutChar or flnPadRight or flnHighlight or flnHandleTildes
     else OPT:=flnHardExtArea or flnAutoHideDot or flnUseCutChar or flnPadRight or flnHighlight or flnHandleTildes;
    if P^.Selected and (FMSetup.Show and fmsTagChar<>0) then OPT:=OPT or flnSelected;
    if P^.Attr and SysFile   <>0 then NFM:=nfmSystem;
    if P^.Attr and Hidden    <>0 then NFM:=nfmHidden;
    if P^.Attr and Directory <>0 then if DirFLP<>65535
                                       then OPT:=OPT or DirFLP
                                       else {$IFNDEF COMBINE_MIXED}
                                            if (Length(GetSName(S))<=8) and
                                               (Length(GetExt(S))<=4)
                                            {$ELSE}
                                            if (not IsMixedCase(S)) {JO}
                                            {$ENDIF}
                                             then OPT:=OPT or flnUpCase
                                             else
                                 else if FilFLP<>65535
                                       then OPT:=OPT or FilFLP
                                       else {$IFNDEF COMBINE_MIXED}
                                            if (Length(GetSName(S))<=8) and
                                               (Length(GetExt(S))<=4)
                                            {$ELSE}
                                            if (not IsMixedCase(S)) {JO}
                                            {$ENDIF}
                                             then OPT:=OPT or flnLowCase
                                             else ;
    S:=FormatLongName(S, LFNLen, EXTLen, OPT, NFM, nil);
    if P^.Selected and (FMSetup.Show and fmsTagChar=0) then S:='~'+S+'~';
   end
  else
   begin
    NFM:=nfmNull;
    OPT:=flnHardExtArea or flnAutoHideDot or flnPadRight or flnHighlight or flnHandleTildes;
    if P^.Selected and (FMSetup.Show and fmsTagChar<>0) then OPT:=OPT+flnSelected;
    if P^.Attr and SysFile<>0 then begin NFM:=nfmSystem; OPT:=OPT or flnCapitalCase end;
    if P^.Attr and Hidden <>0 then NFM:=nfmHidden;
    if P^.Attr and Directory <>0 then OPT:=OPT or flnUpCase
                                 else OPT:=OPT or flnLowCase;
    S:=FormatLongName(MakeFileName(P^.Name), 12, 3, OPT, NFM, nil);
    if P^.Selected and (FMSetup.Show and fmsTagChar=0) then S:='~'+S+'~';
   end;

  X:=LFNLen;
  MoveCStr(TWordArray(B), S, C);
{!!!!!!!!!!!}
  if P^.Selected and PView(Owner)^.GetState(sfFocused) then begin
   C:=C shr 8;
   SC:=SC and $00FF + C shl 8;
  end;
  Inc(X);
  TWordArray(B)[X-1]:=SC;
  MakeDate(DateMode, P^.Day, P^.Month, P^.Yr, P^.Hour, P^.Minute, S1);
  if (P^.Size > 0) or (P^.Attr and Directory = 0)
    then S := FileSizeStr( P^.Size ){!}
    else if P^.Name[1]= '.'
      then S := GetString( dlUpDir )
      else S := GetString( dlSubDir );
  if Flags and psShowSize <> 0 then
   begin
    MoveStr(TWordArray(B)[X+9-Length(S)+CountryInfo.TimeFmt], S, C);
    X := X+10+CountryInfo.TimeFmt;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowPacked <> 0 then
   begin
    Inc(X, 11);
    if PosChar('<',S) = 0 then S := FileSizeStr(P^.PSize);
    MoveStr(TWordArray(B)[X-Length(S)-1], S, C);
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowRatio <> 0 then
   begin
    Inc(X, 4);
    if PosChar('<',S) = 0 then
     begin
      if P^.Size <> 0 then S := ZtoS(Percent(P^.Size, P^.PSize))
                      else S := '100';
      S := PredSpace(S, 3);
     end else S := '';
    MoveStr(TWordArray(B)[X-Length(S)-1], S, C);
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowDate <> 0 then
   begin
    MoveStr(TWordArray(B)[X], Copy(S1,1,8), C);
    Inc(X, 9);
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowTime <> 0 then
   begin
    MoveStr(TWordArray(B)[X], Copy(S1,10,6), C);
    Inc(X, 7+CountryInfo.TimeFmt);
    TWordArray(B)[X-1]:=SC;
   end;
end;

procedure TArcDrive.GetEmpty;
 var
  X: Byte;
begin
  if Flags and psShowLongName <> 0 then X:=LFNLen+1 else X:=13;
  TWordArray(B)[X-1]:=SC;
  if Flags and psShowSize <> 0 then
   begin
    X := X+10+CountryInfo.TimeFmt;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowPacked <> 0 then
   begin
    Inc(X, 11);
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowRatio <> 0 then
   begin
    Inc(X, 4);
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowDate <> 0 then
   begin
    Inc(X, 9);
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowTime <> 0 then
   begin
    Inc(X, 7+CountryInfo.TimeFmt);
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
 end;
end;

procedure TArcDrive.GetFreeSpace;
begin
  S := '';
end;

function ArcViewer;
 var P: PDrive;
     E: TEvent;
     Dr: String;
     Nm: String;
     Xt: String;
begin
 ArcViewer := False;
 P := New(PArcDrive, Init(AName, num));
 if Abort then Exit;
 if P = nil then
   begin
     lFSplit(AName, Dr, Nm, Xt);
     if (UpStrg(XT) = '.TDR') or (UpStrg(XT) = '.AVT')
      then P := New(PArvidDrive, Init(AName, num));
     if P = nil then Exit;
   end;
 E.What := evCommand;
 E.Command := cmInsertDrive;
 E.InfoPtr := P;
 Desktop^.HandleEvent(E);
 if E.What <> evNothing then Dispose(P, Done) else ArcViewer := True;
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


end.
