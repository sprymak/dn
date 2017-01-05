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

Unit ArcView;

Interface

uses
  LFN, {DataCompBoy} {$IFNDEF OS2}LFNCol,{$ENDIF} tree, Dos, Objects, Views, Drivers,
  FViewer, FlPanel, FilesCol, DiskInfo, DblWnd, HistList, HideView,
  RStrings, Drives, Commands, Archiver, FStorage, Histries,
  advance, advance1, advance2, advance3, archdet, collect
  , Arc_RAR, Arc_ACE
  ;

type
    PArcDrive = ^TArcDrive;
    TArcDrive = object(TDrive)
     ArcName:     String; {DataCompBoy}
     VArcName:    String; {JO}
     AType:       PARJArchive;
     Files:       PDirStorage;
     KillAfterUse:Boolean;
     FakeKillAfterUse:Boolean; {временная пустышка}
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
     function    Exec(Prg, Cmd: String; Lst: AnsiString; B: Boolean): Boolean;
                 {JO:  выделил список файлов в командной строке или путь к               }
                 {     файлу-списку в отдельный параметр Lst;                            }
                 {     параметр B должен быть False, если используем                     }
                 {     файл-список или разархивируем одиночный файл не прибегая к списку }
                 {     и True, если используем список в командной строке                 }

     procedure   UseFile(P: PFileRec; Command: Word); virtual; {DataCompBoy}
     function    MakeListFile(PC: PCollection; UseUnp: Boolean; Var B: Boolean): Ansistring;
                 {JO:  параметр UseUnp указывает, будем использовать полученный      }
                 {     список файлов для распаковщика (True), или паковщика (False); }
                 {     переменная В возвращает, был ли создан фписок в               }
                 {     командной строке (True) или в файле-списке (False)            }

     procedure   CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
     procedure   CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
     procedure   EraseFiles(AFiles: PCollection); virtual;
     {procedure  GetDown(var B; C: Word; P: PFileRec); virtual;}
     procedure   GetFull(var B; P:PFileRec; C, SC:Word); virtual;{DataCompBoy}
     procedure   GetEmpty(var B; SC: Word); virtual;
     function    CalcLengthWithoutName: Integer; virtual;
     function    CalcLength: Integer; virtual;
     function    GetRealName: String; virtual;
     function    GetInternalName: String; virtual;
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

Implementation

uses
  {$IFDEF VIRTUALPASCAL} VpSysLow, {$ENDIF}
  Menus, DnApp, Messages, Dialogs, Gauge, FileCopy, Memory, Startup,
  {$IFDEF ARVID} Arvid, {$ENDIF} xTime, VideoMan, DNExec;

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
  4 : MessageBox(GetString(dlArcMsg4)+''''+{$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(Cut(ArcFileName,40))+'''',NIL,mfOkButton or mfError);
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
     I: Integer;
     xt,Q: String;
begin
 TObject.Init;
 if viewmode<1 then GetParam(1)
               else GetParam(viewmode);
 innum:=viewmode;

 I:=PosChar(':', Copy(VAName,3,MaxStringLength))+2;
 If I>2 then begin
  Q := Copy(VAName, I+1, MaxStringLength);
  if Q[Length(Q)] in ['\','/'] then SetLength(Q, Length(Q)-1);
  VArcName := lFExpand(Copy(VAName,1,I-1));
 end else begin
  VArcName := lFExpand(VAName);
  Q:='\';
 end;

 I:=PosChar(':', Copy(AName,3,MaxStringLength))+2;
 If I>2 then begin
  Q := Copy(AName, I+1, MaxStringLength);
  if Q[Length(Q)] in ['\','/'] then SetLength(Q, Length(Q)-1);
  ArcName := lFExpand(Copy(AName,1,I-1));
 end else begin
  ArcName := lFExpand(AName);
  Q:='\';
 end;

{lFSplit(ArcName, FreeStr, Nm, Xt);
 if Xt = '' then AddStr(ArcName, '.');}
 xt:=GetExt(ArcName);
 If ((xt='') or (xt='.')) and (ArcName[length(ArcName)]<>'.')
  then ArcName:=ArcName+'.';
 lFindFirst(ArcName, Anyfile {$IFNDEF OS2}XOR VolumeID{$ENDIF}, SR); {JO}
 lFindClose(SR);
 if DosError <> 0 then
 begin
   ArcFileName := ArcName;
   VArcFileName := VArcName;
   StdMsg4;
   lFindClose(SR);
   Fail
 end;
 ArcDate := SR.SR.Time;
 DriveType := dtArc;
 if not ReadArchive or (Files = nil) then begin Done; Fail; end;
 KillAfterUse := TempFile <> '';
 TempFile := '';
 Password := '';
{lFindClose(SR);}
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
{VArcName := lFExpand(VAName);}
 lFindFirst(ArcName, Anyfile {$IFNDEF OS2}XOR VolumeID{$ENDIF}, SR); {JO}
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
 S.ReadStrV(ArcName); {S.Read(ArcName[0],1); S.Read(ArcName[1],Length(ArcName));}
{Cat}
 S.ReadStrV(VArcName); {S.Read(VArcName[0],1); S.Read(VArcName[1],Length(VArcName));}
{/Cat}
 S.Read(FakeKillAfterUse, 1);
{$IFDEF VIRTUALPASCAL} {временно}
 KillAfterUse := False;
{$ENDIF}
 S.ReadStrV(Password); {S.Read(Password[0],1); S.Read(Password[1],Length(Password));}
 Files := PDirStorage(S.Get);
 ForceRescan := Off;
 DriveType := dtArc;
 ArcFileName := ArcName;
 VArcFileName := VArcName;
 lFindFirst(ArcName, Anyfile {$IFNDEF OS2}XOR VolumeID{$ENDIF}, SR); {JO}
 lFindClose(SR);
 if DosError <> 0 then goto Failure;
 ArcDate := SR.SR.Time;
 New(ArcFile,Init(ArcName, stOpenRead, 512));
 if (ArcFile=nil) or (ArcFile^.Status <> stOK) then begin
   Dispose(ArcFile, Done); ArcFile:=nil;
Failure:
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
 if KillAfterUse then EraseTempFile(ArcName);
end;

procedure TArcDrive.Store;
begin
 inherited Store(S);
 S.WriteStr(@ArcName); {S.Write(ArcName[0],1 + Length(ArcName));}
 S.WriteStr(@VArcName); {S.Write(VArcName[0],1 + Length(VArcName));} {Cat}
 S.Write(KillAfterUse, 1);
 S.WriteStr(@Password); {S.Write(Password[0],1 + Length(Password));}
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
 CtrlBreakHit := False;
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
 LN := ArcFile^.GetSize + 1;
 Cancel := Off; PReader := nil;
 Inc(SkyEnabled);
 NewTimer(T, 3);
 if (StartupData.Slice2 and osuReleaseArch = 0) then Application^.BFSpeed;
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
{$IFNDEF OS2}
      Replace('\.','\',FileInfo.FName);
      UpStr(FileInfo.FName);
{$ENDIF}

{attribute "Hidden" means "with password"}
      Files^.AddFile(olf, {$IFNDEF OS2}FileInfo.FName, {$ENDIF}FileInfo.USize,
                     FileInfo.PSize,FileInfo.Date,  FileInfo.Attr);
      if (FileInfo.Attr and Directory<>0) or (olf[length(olf)]='\') then
       Files^.AddFile(MakeNormName(olf,'..'), {$IFNDEF OS2}MakeNormName(FileInfo.FName, '..'),{$ENDIF}
                      FileInfo.USize,FileInfo.PSize,FileInfo.Date, 0);

      if (P <> nil) and  TimerExpired(T) then
        begin
          DispatchEvents(P, Cancel);
          if Cancel then begin StdMsg(5);FileInfo.Last:=1; end;
        end;
     end;
 until (FileInfo.Last>0) or LowMemory or CtrlBreakHit;
 if CtrlBreakHit then StdMsg(5);
 CtrlBreakHit := False;
 if (StartupData.Slice2 and osuReleaseArch = 0) then Application^.EFSpeed;
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
 While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do SetLength(CurDir, Length(CurDir)-1);
{$IFNDEF OS2}
 if ADir[1] = '.' then
{$ELSE}
 if ADir = '..' then
{$ENDIF}
  begin
   if CurDir <> '' then While (CurDir <> '') and (not(CurDir[Length(CurDir)] in ['\','/'])) do SetLength(CurDir, Length(CurDir)-1)
                   else Advance2.lChDir(GetPath(ArcName));
   Exit;
  end;
 lFSplit(ADir, Dr, Nm, Xt);
 if Xt = '..' then
  begin
   CurDir := Dr;
   while (CurDir <> '') and not (CurDir[Length(CurDir)] in ['\','/']) do
    SetLength(CurDir, Length(CurDir)-1);
  end else CurDir := ADir;
 While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do SetLength(CurDir, Length(CurDir)-1);
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
 While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do SetLength(CurDir, Length(CurDir)-1);
 if (Length(CurDir)>0) and (not (CurDir[1] in ['\','/'])) then CurDir := '\' + CurDir;
 lFSplit(VArcName, Dr, Nm, Xt); {JO}
 GetDir := AType^.GetSign+Copy(Dr,1,2)+Nm+Xt+CurDir;
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
{While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do SetLength(CurDir, Length(CurDir)-1);}
 AFiles := New(PFilesCollection, Init($10, $10));
{FD := New(PFilesCollection, Init($40, $10));}
 PFilesCollection(AFiles)^.Owner := Owner;
 PFilesCollection(AFiles)^.SortMode := Sortmode;
{$IFDEF Win32}
 if PFilesCollection(AFiles)^.SortMode = psmName then PFilesCollection(AFiles)^.SortMode := psmLongName;
 if PFilesCollection(AFiles)^.SortMode = psmExtension then PFilesCollection(AFiles)^.SortMode := psmLongExt;
 PFilesCollection(AFiles)^.LFNActive := True; {JO}
{$ENDIF}
 GetDirectory := AFiles;
 While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do SetLength(CurDir, Length(CurDir)-1);
 FD := New(PFilesCollection, Init($40, $10));
 FreeSpace := ''; TotalInfo := ''; TTL := 0; TPL := 0;
{GetDirectory := AFiles;} AllFiles := (FileMask = x_x) or (FileMask='*');
 FD^.SortMode := 141;
 FD^.Duplicates := False;
{$IFDEF Win32}
 FD^.LFNActive := True; {JO}
{$ENDIF}
 AFiles^.Duplicates := False;
 Files^.ResetPointer('');
 While not Files^.Last and Files^.GetNextFile and (MaxAvail > LowMemSize) do
  begin
    _USize := Files^.CurFile.Size;
    _PSize := Files^.CurFile.CSize;
    if (UpStrg(CurDir + '\') = UpStrg(Files^.LastDir)) and
       (AllFiles {$IFNDEF OS2}or InFilter(Files^.CurFile.LFN, FileMask){$ENDIF}
               {$IFNDEF Win32}or InFilter(Files^.CurFile.Name, FileMask){$ENDIF}) then
      begin
{$IFNDEF Win32}
        if Files^.CurFile.Name = '' then Continue;
        if Files^.CurFile.Name = '..' then Continue;
{$ELSE}
        if Files^.CurFile.LFN = '' then Continue;
        if Files^.CurFile.LFN = '..' then Continue;
{$ENDIF}
        with Files^.CurFile do
{$IFNDEF OS2}
         F := NewFileRec(LFN, GetURZ(Name), _USize, Date, 0, 0, Attr, @CurDir);
{$ELSE}
         F := NewFileRec(Name, _USize, Date, 0, 0, Attr, @CurDir);
{$ENDIF}
        F^.PSize := _PSize;
        TTL := TTL + _USize;
        TPL := TPL + _PSize;
      end else if (UpStrg(CurDir+'\') = UpStrg(Copy(Files^.LastDir, 1, Length(CurDir)+1))) then
              begin
                Dr := Copy(Files^.LastDir, Length(CurDir)+2, MaxStringLength);
                I := PosChar('\', Dr); if I = 0 then I := Length(Dr)+1;
                SetLength(Dr, I-1);
                if Dr = '' then Continue;
                FillChar(FR, SizeOf(FR), 0);
{$IFNDEF OS2}
                FR.Name := Norm12(GetURZ(Dr)); FR.Name[9] := ' '; {??? относит. OS/2}
                FR.LFN := AddLFN(Dr);
{$ELSE}
                FR.Name := Dr; {???}
{$ENDIF}
                FR.Attr := Directory;
                I := FD^.IndexOf(@FR);
{$IFNDEF OS2}
 {$IFNDEF STRINGLFN}
                DelLFN(FR.LFN);
 {$ENDIF}
{$ENDIF}
                if I >= 0 then
                 with PFileRec(FD^.At(I))^ do
                  begin
                    Size:=Size+_USize;
                    PSize:=PSize+_PSize;
                    TTL := TTL + _USize;
                    TPL := TPL + _PSize;
                    Continue;
                  end;
                F := NewFileRec(Dr, {$IFNDEF OS2}GetURZ(Dr),{$ENDIF} _USize, ArcDate, 0, 0, $80 or Directory, @CurDir);
                F^.PSize := _PSize;
                TTL := TTL + _USize;
                TPL := TPL + _PSize;
                if FD^.Search(F,si) then begin DelFileRec(F); continue; end
                                   {else FD^.Insert(F);}
                                    else FD^.AtInsert(si, F);
              end else Continue;
      if AFiles^.Search(F,si) then DelFileRec(F)
                             {else AFiles^.Insert(F);}
                              else AFiles^.AtInsert(si, F);
  end;
 if MaxAvail <= LowMemSize then
 begin
   Application^.OutOfMemory;
 end;
 FreeSpace := '';
 TotalInfo := CalcTotalInfo(AFiles^.Count, AFiles^.Count, TTL);

 if CurDir = '' then OW := @ArcName else OW := @CurDir;
(* if TTL > MaxLongInt then F := NewFileRec('..', '..',0, ArcDate, Directory, OW) else
 begin
   F := NewFileRec({$IFNDEF OS2}'..',{$ENDIF} '..',Round(TTL), ArcDate, Directory, OW);
   F^.Attr := $8000 or F^.Attr;
 end;
 if TPL < MaxLongInt then F^.PSize := Round(TPL); *)

 F := NewFileRec('..',{$IFNDEF OS2}'..',{$ENDIF} {Round}(TTL), ArcDate, 0, 0, Directory, OW);
 F^.Attr := $8000 or F^.Attr;
 F^.PSize := {Round}(TPL);
 AFiles^.AtInsert(0, F);
 FD^.DeleteAll; Dispose(FD, Done);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.UseFile;
 var SS, S, S2, Q: String;
     C: Char;
     Unp: String;
    {ExecOK: Boolean;}
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
  {$IFNDEF OS2}
   SS := MakeNormName(P^.Owner^, GetLFN(P^.LFN));
   if SS[1] = '\' then Delete(SS, 1, 1); {DelFC(SS);}
   if AType^.UseLFN then S2 := ArcName
     else S2 := lfGetShortFileName(ArcName);
   if ArcName[Length(ArcName)]='.' then S2 := S2 + '.';
   S := CnvString(AType^.Extract)+' '+ s +
      SquashesName(S2) + ' ' + SquashesName(SS)+' ';
   DelDoubles('  ',S);
   TempFile := C + MakeNormName(TempDir , GetLFN(P^.LFN));
   Q:='|' + GetRealName+':'+MakeNormName(CurDir, GetLFN(P^.LFN));
  {$ELSE}
   SS := MakeNormName(P^.Owner^, P^.Name);
   if SS[1] = '\' then Delete(SS, 1, 1); {DelFC(SS);}
    S := CnvString(AType^.Extract)+' '+s+SquashesName(ArcName)+' '+SquashesName(SS)+' ';
   DelDoubles('  ',S);
   TempFile := C + MakeNormName(TempDir , P^.Name);
   Q:='|' + GetRealName+':'+MakeNormName(CurDir, P^.Name);
  {$ENDIF}
   if C in ['<','-','|','+'] then TempFile := TempFile + Q;
   S2:=Copy(TempFile,2,MaxStringLength); {ExecOK:=True;}
   if not ExistFile(S2) or (PackedDate(P)<>FileTime(S2)) then
   begin
    Unp := CnvString(AType^.unPacker);
    if (AType^.GetID = arcRAR) and (PosChar(';', Unp) > 0) then
      begin
        if PRARArchive(AType)^.VersionToExtr > 20 then
              Unp := Copy(Unp, PosChar(';', Unp)+1, 255)
         else Unp := Copy(Unp, 1, PosChar(';', Unp)-1);
      end;
    lGetDir(0, DirToChange);
    Advance2.lChDir(TempDir);
   {ExecOK :=} Exec(Unp , {$IFDEF RecodeWhenDraw}OemToCharStr{$ENDIF}(s), '', False);
    Advance2.lChDir(DirToChange); DirToChange := '';
   end;
(* {JO: убрал, т.к. без этого всё работает, а с этим куском - глючит просмотр в архивах с фильтром}
   if ExecOK then
   begin
 {$IFNDEF OS2}
     TempFile := MakeNormName(TempDir, GetLFN(P^.LFN));
 {$ELSE}
     TempFile := MakeNormName(TempDir, P^.Name);
 {$ENDIF}
     if Q<>'' then TempFile:=TempFile+Q;
     While TempFile[Length(TempFile)] = ' ' do SetLength(TempFile, Length(TempFile)-1);
     Message(Application, evCommand, Command, @TempFile);
   end;
*)
{$IFDEF VIRTUALPASCAL}
   TempFileSWP := {$IFDEF RecodeWhenDraw}OemToCharStr{$ENDIF}(TempFile); {JO}
{$ENDIF}
   TempFile:=''; {-$VOL}
{$IFDEF VIRTUALPASCAL}
  Message(Application, evCommand, cmRetrieveSwp, nil); {JO}
{$ENDIF}
end;
        {-DataCompBoy-}

function TArcDrive.Exec;
 var S: String;
     SS1: AnsiString;
     SM: Word;
     DE: Word;
     CmdLineLim, ListLineLim: AWord;
     CmdLineOK: Boolean;
     I, J: Longint;

procedure StdMsg8;
var
  L: array[0..1] of LongInt;
  ST: String;
begin
  Application^.Redraw;
  ST := S;
  Pointer(L[0]) := @ST; L[1] := DE;
  Msg(dlArcMsg8, @L, mfOkButton or mfError);
end;

{AK155 20/12/2001 Если под Win32 пытаться в отладчике прошагать
эту функцию, то получается полная блокировка клавиатуры и мыши
сразу на входе (даже с begin сойти не получается).
Этот эффект исчезает, если параметр AnsiString заменить на String.
Под OS/2 все шагается без проблем. Интересно, чей это глюк -
виндового отладчика или виндовой RTL? Хорошо, если первое. }
begin
  Exec := True;
  S := Prg+' '+Cmd;
{$IFDEF OS_DOS}
  if AType^.Swap then
  begin
    Exec := Message(Application, evCommand, cmExecString, @S) = nil;
    Exit;
  end;
{$ENDIF}
  DoneSysError;
  DoneEvents;
  DoneVideo;
  DoneDOSMem;
  DoneMemory;
  {$IFDEF OS_DOS}asm cld; mov ax,3; int $10; end;{$ENDIF}
{$IFDEF OS_DOS} //AK155
  {AK155, дописано позже, чем комментарий к OS/2.
Под Win32 тоже не следует заниматься самодеятельностью.
Во-первых, мы отдаем консоль в каком-то не таком состоянии,
так что консольный rar не может вводить с клавиатуры.
Во-вторых, стОило ли работать с ansistring, чтобы потом вызвать
Dos.Exec?}
  S := GetEnv('PATH');
  S := FSearch(Prg,S);
  SwapVectors;
  if InFilter(S, '*.EXE;*.COM')
   then DOS.Exec(S,Cmd)
   else DOS.Exec(GetEnv('COMSPEC'),'/c '+S+' '+Cmd);
{$ELSE}
   {AK155 Под OS/2, во-первых, PATH обычно не умещается
    в 255 символов, во-вторых, нет проблем с памятью,
    в третьих архиватор может оказаться ДОСовым.
    Так что пускай PATH просматривает cmd.exe, а мы не
    будем заниматься самодеятельностью }
  if B then
    begin
{JO: разбираем ту часть командной строки, которая содержит список файлов    }
{    на куски удобоваримой для командного процессора длины                  }
      if AType^.ShortCmdLine then CmdLineLim := {$IFDEF OS2}95{$ELSE}127{$ENDIF} else CmdLineLim := {$IFDEF OS2}1000{$ELSE}250{$ENDIF};
      ListLineLim := CmdLineLim - Length(Prg+Cmd) - 7;
      CmdLineOK := False;
      SS1 := Lst; {для перестраховки}
      repeat
        if Length(Lst) >= ListLineLim then
          begin
            for I := ListLineLim downto 1 do
              if Lst[I] = #$14 then
                begin
                  SS1 := Copy(Lst, 1, I-1);
                  Delete (Lst, 1, I);
                  Break;
                end;
          end
         else
          begin
            SS1 := Lst;
            CmdLineOK := True;
          end;
        for J := 1 to Length(SS1) do
          if SS1[J] = #$14 then SS1[J] := #$20; {JO: заменяем временный символ на пробелы}
        DelDoubles('  ', S);
        AnsiDelDoubles('  ', SS1);
{JO: AnsiExec - процедура из модуля DNExec , которая }
{    используется вместо DOS.Exec и в качестве       }
{    коммандлайна использует строку типа Ansistring  }
        SwapVectors;
        AnsiExec(GetEnv('COMSPEC'),'/c ' + S + ' ' + SS1 + ' ');
        DE := DosError; ClrIO;
        SwapVectors;
      until CmdLineOK;
    end
   else
    begin
      DelDoubles('  ', S);
      AnsiDelDoubles('  ', Lst);
      SwapVectors;
      AnsiExec(GetEnv('COMSPEC'),'/c ' + S + ' ' + Lst + ' ');
      DE := DosError; ClrIO;
      SwapVectors;
      EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.LST'); {DataCompBoy}
    end;
{$ENDIF}
  InitDOSMem;
  InitMemory;
  SetVideoMode(ScreenMode); SM := ScreenMode;
  SetBlink(On);
  InitVideo; ScreenMode := SM;
  {$IFDEF Win32}SysTVInitCursor; {$ENDIF} {AK155, см.комментарий к SysTVInitCursor}
  if (StartupData.Load and osuResetPalette <> 0) then SetPalette(VGA_Palette);
  InitEvents;
  InitSysError;
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
  if TypeOf(Prev^)=TypeOf(TDrive) then Prev^.lChDir(GetPath(VArcName)); {???}
  if (Prev^.DriveType = dtDisk) and
     (PView(Owner)^.GetState(sfSelected+sfActive)) then
      ActivePanel := Owner;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  Prev := nil;
  S := GetName(VArcName); {???}
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
     S: Ansistring;
     S1: String;
{    B: Boolean;}

  procedure PutDir(SS: String);
   var I: Integer;
       S1: String;
  begin
   if not (SS[Length(SS)] in ['\','/']) then AddStr(SS, '\');
   Files^.ResetPointer('');
   while not Files^.Last and Files^.GetNextFile do
     if (SS = Copy(Files^.LastDir, 1, Length(SS)))
{$IFNDEF Win32}
          and (Files^.CurFile.Name <> '') then
{$ELSE}
          and (Files^.CurFile.LFN <> '') then
{$ENDIF}
      begin
{$IFNDEF OS2}
        S1 := Files^.LastDir + Files^.CurFile.LFN;
{$ELSE}
        S1 := Files^.LastDir + Files^.CurFile.Name;
{$ENDIF}
        if S1[1] in ['\','/'] then Delete(S1, 1, 1); {DelFC(S1);}
        if (copy(S1, Length(S1) - 2, 3) = '\..') or (copy(S1, Length(S1) - 2, 3) = '/..') then
           SetLength(S1, Length(S1)-{2}3); {JO}
           {JO: нужен ли слэш в конце каталогов - вопрос спорный,          }
           {    но похоже его отсутствие нигде не мешает, а наличие        }
           {    вводит RAR в заблуждение, если не использовать файл-список;}
           {    в дальейшем не исключено, что для каких-то архиваторов     }
           {    потребуется сделать соотв. опцию;                          }
           {    zip работает нормально и с тем, и с другим                 }

       {JO:  используем символ #$14 для временного разделения имён файлов}
        if B then S := S + #$14 + SquashesName(S1) else WriteLn(F.T, S1);
      end;
  end;

begin
 if UseUnp then B := (CnvString(AType^.ExtrListChar) = ' ') or (CnvString(AType^.ExtrListChar) = '')
   else B := (CnvString(AType^.ComprListChar) = ' ') or (CnvString(AType^.ComprListChar) = '');
 if B then S := '' else
  begin
   S := SwpDir + '$DN'+ItoS(DNNumber)+'$.LST';
   lAssignText(F, S); ClrIO;
   lRewriteText(F);
   B := IOResult <> 0;
   if B then S := '' else
     begin
       if UseUnp then S := CnvString(AType^.ExtrListChar) + S
         else S := CnvString(AType^.ComprListChar) + S;
     end;
  end;
 for I := 0 to PC^.Count - 1 do
  begin
   PF := PC^.At(I);
{$IFNDEF OS2}
   S1 := MakeNormName(PF^.Owner^, GetLFN(PF^.LFN));
{$ELSE}
   S1 := MakeNormName(PF^.Owner^, PF^.Name);
{$ENDIF}
   if S1[1] in ['\','/'] then Delete(S1, 1, 1); {DelFC(S1);}

{JO:  используем символ #$14 для временного разделения имён файлов}
   if PF^.Attr and Directory = 0
      then if B then S := S + #$14 + SquashesName(S1) else WriteLn(F.T, S1)
      else PutDir('\'+S1+'\');
   MakeListFile := S;
  end;
 MakeListFile := S;
 if not B then Close(F.T);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.ExtractFiles (AFiles: PCollection; ExtrDir: String;
                                  Own: PView; Options: Byte);
 var SS, ArchiveName: Ansistring;
     S: String;
     DT: Record S: String; W: Word; Psw: String[30]; end;
     ExtrChar: String;
     Nm: String;
     Xt: String;
     Pswd: Boolean;
     B: Boolean;
     SCurDir: String;
     Unp: String;

 procedure UnSelect(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
 begin
  Message(Owner, evCommand, cmCopyUnselect, P);
  Pswd := Pswd or (P^.Attr and Hidden <> 0);
 end;

begin
  While ExtrDir[Length(ExtrDir)] = ' ' do SetLength(ExtrDir, Length(ExtrDir)-1);
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

  SS := MakeListFile(AFiles, True, B);
  S := ' '; Pswd := Off;
  AFiles^.ForEach(@Unselect);
  ExtrChar := CnvString(AType^.ExtractWP);
  if Options and 1 = 0 then ExtrChar := CnvString(AType^.Extract);
  if Options and 2 <> 0 then ExtrChar := CnvString(AType^.Test);
  if (Password = '') and Pswd then
    if ExecResource(dlgSetPassword, Password) <> cmOK then Exit;
  if Password <> '' then S := ' ' + DelSpaces(CnvString(AType^.Garble) + Password) + ' ';
{$IFNDEF OS2}
  if AType^.UseLFN then
{$ENDIF}
    ArchiveName := SquashesName(ArcName)
{$IFNDEF OS2}
    else ArchiveName := SquashesName(lfGetShortFileName(ArcName))
{$ENDIF}
  ;
  if (CnvString(AType^.SetPathInside) <> '') and (CurDir <> '') then
    begin
      SCurDir := CurDir;
      While (SCurDir[Length(SCurDir)] = '.') do SetLength(SCurDir, Length(SCurDir)-1);
      While (SCurDir[1] = '\') do Delete(SCurdir, 1, 1);
      if (SCurDir[Length(SCurDir)] <> '\') then SCurdir := SCurdir + '\';
      S := S + ' ' + CnvString(AType^.SetPathInside) + SCurDir + ' ';
    end;
  S := ExtrChar + ' ' + S + ArchiveName;
  Unp := CnvString(AType^.unPacker);
  if (AType^.GetID = arcRAR) and (PosChar(';', Unp) > 0) then
    begin
     if PRARArchive(AType)^.VersionToExtr > 20 then
           Unp := Copy(Unp, PosChar(';', Unp)+1, 255)
      else Unp := Copy(Unp, 1, PosChar(';', Unp)-1);
    end;
  lGetDir(0, DirToChange);
  CreateDirInheritance(ExtrDir, On);
  Advance2.lChDir(ExtrDir);
  Exec(Unp ,{$IFDEF RecodeWhenDraw}OemToCharStr{$ENDIF}(s), SS, B);
  Advance2.lChDir(DirToChange); DirToChange := '';
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.CopyFiles;
 var SS, S: String;
     DT: Record S: String; W: Word; Psw: String[30]; end;
     ExtrDir: String;
     ExtrChar: String;
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
 var SS, S: Ansistring;
     PF: PFileRec;
     J, I: Word;
     O: PView;
     P: PString;
     B: Boolean;
begin
  if AFiles^.Count = 0 then Exit;
  if AFiles^.Count = 1 then
   begin PF := AFiles^.At(0);
{$IFNDEF OS2}
         S := GetString(dlEraseConfirm1) + GetLFN(PF^.LFN) + ' ?';
{$ELSE}
         S := GetString(dlEraseConfirm1) + PF^.Name + ' ?';
{$ENDIF}
   end
  else S := GetString(dlEraseConfirms1);
  I := MessageBox(S,nil,mfConfirmation+mfYesButton+mfNoButton{+mfFastButton});
  if (I <> cmYes) then Exit;
  if AFiles^.Count > 1 then
   begin
    S := GetString(dlEraseConfirm2)+ItoS(AFiles^.Count)+' '+GetString(dlDIFiles)+ ' ?';
    J := MessageBox(S,nil,mfConfirmation+mfYesButton+mfNoButton);
    if (J <> cmYes) then Exit;
   end;
  SS := MakeListFile(AFiles, False, B);
{$IFNDEF OS2}
  if AType^.UseLFN then
{$ENDIF}
    S := CnvString(AType^.Delete) + ' ' + ArcName
{$IFNDEF OS2}
   else S := CnvString(AType^.Delete) + ' ' + lfGetShortFileName(ArcName)
{$ENDIF}
  ;

  ForceRescan := True;
  Exec(CnvString(AType^.Packer),s, SS, B);
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

procedure TArcDrive.GetFull;
var
  S: String;
{$IFDEF USEANSISTRING}
  S1: String;
{$ELSE}
  S1: String[40];
{$ENDIF}
  X: Byte;
  NFM: TNameFormatMode;
  OPT: Word;
  label Longer250, Longer250_1;
begin
X:= 0;
if (LFNLen < 250){$IFNDEF OS2} or (Flags and psShowLongName = 0){$ENDIF} then {JO: если ширина колонки имени больше 250 символов, её показываем после остальных колонок}
Longer250:
 begin
{$IFNDEF OS2}
  if Flags and psShowLongName <> 0 then
   begin
    s:=GetLFN(P^.LFN);
{$ELSE}
    s:=P^.Name;
{$ENDIF}
    NFM:=nfmNull;
    if EXTLen=0
     then OPT:=flnPreferName or flnUseCutChar or flnPadRight or flnHighlight or flnHandleTildes
     else OPT:=flnHardExtArea or flnAutoHideDot or flnUseCutChar or flnPadRight or flnHighlight or flnHandleTildes;
    if P^.Selected and (FMSetup.Show and fmsTagChar<>0) then OPT:=OPT or flnSelected;

    case P^.Attr and (Hidden+SysFile) of               {Pavel Anufrikov -> }
     Hidden  :        NFM:=nfmHidden;
     SysFile :        NFM:=nfmSystem;
     Hidden+SysFile : NFM:=nfmHiddenSystem;
    end{case};                                         { <- Pavel Anufrikov}

    if P^.Attr and Directory <>0 then if DirFLP<>65535
                                       then OPT:=OPT or DirFLP
                                       else if (not IsMixedCase(S)) {JO}
                                             then OPT:=OPT or flnUpCase
                                             else
                                 else if FilFLP<>65535
                                       then OPT:=OPT or FilFLP
                                       else if (not IsMixedCase(S)) {JO}
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
  MoveCStr(TWordArray(B)[X], S, C);
  {$IFNDEF OS2}if Flags and psShowLongName <> 0 then {$ENDIF} Inc(X, LFNLen)
  {$IFNDEF OS2}  else Inc(X, 12) {$ENDIF}
  ;
{-----------}
  if P^.Selected and PView(Owner)^.GetState(sfFocused) then begin
   C:=C shr 8;
   SC:=SC and $00FF + C shl 8;
  end;
  if (LFNLen >= 250){$IFNDEF OS2} and (Flags and psShowLongName <> 0){$ENDIF} then goto Longer250_1;
  Inc(X);
  TWordArray(B)[X-1]:=SC;
 end; {условие 250 символов}
  MakeDate(DateMode, P^.Day, P^.Month, P^.Yr, P^.Hour, P^.Minute, S1);
  if (P^.Size > 0) or (P^.Attr and Directory = 0)
    then S := FileSizeStr( P^.Size ){!}
{$IFNDEF OS2}
  {$IFDEF OS_DOS}
    else if P^.Name[1]= '.'
  {$ELSE}
    else if P^.LFN= '..'
  {$ENDIF}
{$ELSE}
    else if P^.Name= '..'
{$ENDIF}
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
    if PosChar('<',S) = 0 then S := FStr(P^.PSize);
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
  if (LFNLen >= 250) {$IFNDEF OS2} and (Flags and psShowLongName <> 0){$ENDIF} then goto Longer250;
Longer250_1:
end;

procedure TArcDrive.GetEmpty;
 var
  X: Byte;
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
  TWordArray(B)[X-1]:=SC;
    end else X := 0;
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

function TArcDrive.GetInternalName;
 var IntPath: String;
begin
 IntPath := GetDir;
 if PosChar('\', IntPath) >0 then
   GetInternalName := Copy(IntPath, PosChar('\', IntPath), 255)
   else GetInternalName := '';
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
     Xt: String;
begin
 ArcViewer := False;
 P := New(PArcDrive, Init(AName, VAName, num));
 if Abort then Exit;
 if P = nil then
   begin
     {$IFDEF ARVID}
     Xt:=UpStrg(GetExt(AName));
     if (Xt = '.TDR') or (Xt = '.AVT')
      then P := New(PArvidDrive, Init(AName, num));
     if P = nil then
     {$ENDIF}
     Exit;
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

  if AType^.GetID = arcRAR then B.VolumeID := NewStr(GetString(dlDIVersionToExtract)+RtoS(PRARArchive(AType)^.VersionToExtr / 10, 4, 2)+'~');
  if AType^.GetID = arcACE then B.VolumeID := NewStr(GetString(dlDIVersionToExtract)+RtoS(ACEVerToExtr / 10, 4, 2)+'~');

end;

procedure TArcDrive.GetDirLength(PF: PFileRec);
begin
end;


end.
