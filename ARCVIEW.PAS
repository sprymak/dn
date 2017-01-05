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
{JO, AK155: 27.11.2002 - добавили раскрытие архивов в ветвь по Ctrl-H}
{JO:  1.12.2002 - добавил поиск файлов внутри архива}
{$I STDEFINE.INC}

Unit ArcView;

Interface

uses
  LFN, files, tree, Dos, Objects, Views, Drivers,
  FViewer, FlPanel, FilesCol, DiskInfo, DblWnd, HistList, HideView,
  RStrings, Drives, Commands, Archiver, FStorage, Histries,
  advance, advance1, advance2, advance3, archdet, collect
  , Arc_RAR, Arc_ACE
  ;

type
    PArcDrive = ^TArcDrive;
    TArcDrive = object(TDrive)
  {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
     ArcName:     String; {DataCompBoy}
     VArcName:    String; {JO}
     AType:       PARJArchive;
     Files:       PDirStorage;
     KillAfterUse:Boolean;
     FakeKillAfterUse:Boolean; {временная пустышка}
     ArcDate: LongInt;     ArcSize: Comp; {сохраняются вместе}
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
                    var FreeSpace: String; var TotalInfo: TSize): PCollection; virtual;
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
     function OpenDirectory(const Dir: String): PDrive; virtual;
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

Implementation

uses
  {$IFDEF VIRTUALPASCAL} VpSysLow, {$ENDIF}
  Menus, DnApp, Messages, Dialogs, Gauge, FileCopy, Memory, Startup,
  {$IFDEF ARVID} Arvid, {$ENDIF} xTime, VideoMan, DNExec, Filefind
  {$IFDEF CHDIRTIMER}
  , CmdLine, DNIni
  {$ENDIF}
  , UserMenu {JO: для скрывания панелей при разархивировании }
  , Arc_Zip {JO: для CentralDirRecPresent}
  ;

const
  LowMemSize = $4000;  {Local setting}

function MaxAvail: LongInt;
begin
  MaxAvail := MemAdjust(System.MaxAvail);
end;

Procedure StdMsg(MsgNo: Byte);
begin
 Application^.Redraw;
 case MsgNo of
  1 : ErrMsg(dlArcMsg1);
  4 : MessageBox(GetString(dlArcMsg4)+''''+{$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(Cut(ArcFileName,40))+'''',NIL,mfOkButton or mfError);
  5 : Msg(dlArcMsg5,NIL,mfOkButton or mfInformation);
  6 : if MsgHelpCtx <> 0 then
        MessageBox(GetString(dlArcMsg6)+GetString(dlPressF1),
                   nil,mfOkButton or mfError)
        else MessageBox(GetString(dlArcMsg6),nil,mfOkButton or mfError);
  7 : Msg(dlArcMsg7,NIL,mfOkButton or mfInformation);
 end;
end;

procedure TArcDrive.StdMsg4;
begin
  if TempFile <> '' then TempFile := ''; StdMsg(4); Abort := true;
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

 I := PosChar(':', Copy(VAName,3,MaxStringLength))+2;
 if I > 2 then VArcName := lFExpand(Copy(VAName,1,I-1))
  else VArcName := lFExpand(VAName);

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
 lFindFirst(ArcName, AnyFile, SR); {JO}
 lFindClose(SR);
 if DosError <> 0 then
 begin
   ArcFileName := ArcName;
   VArcFileName := VArcName;
   StdMsg4;
   lFindClose(SR);
   Fail
 end;
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
 lFindFirst(ArcName, AnyFile, SR); {JO}
 ArcDate := SR.SR.Time; ArcSize := SR.SR.Size;
 lFindClose(SR);
 DriveType := dtArc;
 Files := PC;
 if (Files = nil) then Fail;
 KillAfterUse := TempFile <> '';
 TempFile := '';
 Password := '';
 if ExistFile(ArcName) then
  New(ArcFile,Init(ArcName, stOpenRead, ArcBufSize))
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
 S.Read(ArcDate, SizeOf(ArcDate)+SizeOf(ArcSize));
 ForceRescan := false;
 DriveType := dtArc;
 ArcFileName := ArcName;
 VArcFileName := VArcName;
 lFindFirst(ArcName, AnyFile, SR); {JO}
 lFindClose(SR);
 if DosError <> 0 then goto Failure;
 if (ArcDate <> SR.SR.Time) or (ArcSize <> SR.SR.Size) then
   begin {архив изменился, надо его перечитывать заново}
   Files := PDirStorage(S.Get); {AK155 28-11-2002 Тут лучше бы не читать,
     а просто пропустить кусок потока, но я не умею это делать}
   CurDir:='\'; ReadArchive;
   end
 else
   begin
   Files := PDirStorage(S.Get);
   New(ArcFile,Init(ArcName, stOpenRead, ArcBufSize));
   if (ArcFile=nil) or (ArcFile^.Status <> stOK) then
     begin
     Dispose(ArcFile, Done); ArcFile:=nil;
     goto Failure;
     end;
   ArcPos := 0;
   SkipSFX;
   AType := DetectArchive;
   FreeObject(ArcFile);
   if AType = nil then
     begin
Failure:
     StdMsg(4);
     if Files <> nil then Dispose(Files, Done); Files:=nil;
     S.Read(ForceRescan, 1);
     Fail;
     end;
   end;
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
 S.Write(ArcDate, SizeOf(ArcDate)+SizeOf(ArcSize));
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
    SR: lSearchRec;
begin
{$IFDEF CHDIRTIMER}
 if ChDirTimer and (ShiftState and (kbLeftShift+kbRightShift) <> 0) then
   DDTimer := Get100s;
{$ENDIF}
{AK155 26-11-2002 Перечитываем архив тогда и только тогда, когда
 у него изменилась дата/время или длина }
 lFindFirst(ArcName, AnyFile, SR);
 lFindClose(SR);
 if (ArcDate = SR.SR.Time) and (ArcSize = SR.SR.Size) then
    begin ReadArchive := true; exit; end;
 ArcDate := SR.SR.Time; ArcSize := SR.SR.Size;
{/AK155}
 CtrlBreakHit := False;
 ReadArchive:=false;
 New(ArcFile,Init(ArcName, stOpenRead, ArcBufSize));
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
 Cancel := false; PReader := nil;
 Inc(SkyEnabled);
 NewTimer(T, 6);
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
      NewTimer(T, 6);
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
      Files^.AddFile(olf, FileInfo.USize,
                     FileInfo.PSize,FileInfo.Date,  FileInfo.Attr);
      if (FileInfo.Attr and Directory<>0) or (olf[length(olf)]='\') then
       Files^.AddFile(MakeNormName(olf,'..'),
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
 if (FileInfo.Last=2) or
   ((AType^.GetID = arcZIP) and not CentralDirRecPresent) then StdMsg(6);
 ReadArchive:=true;
 if Files^.Files=0 then begin StdMsg(7);ReadArchive:=false;end;
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
 if IsDummyDir(ADir) then
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
 if (Prev <> nil) and (Prev^.DriveType = dtDisk) then
   lFSplit(VArcName, Dr, Nm, Xt) {JO}
  else lFSplit(ArcName, Dr, Nm, Xt);
 GetDir := AType^.GetSign+Nm+Xt+CurDir;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TArcDrive.GetDirectory;
 var F: PFileRec;
     I,si: LongInt;
     AllFiles: Boolean;
     AFiles, FD: PFilesCollection;
     TTL, TPL: TSize;
     _USize, _PSize: TSize;
     FR: TFileRec;
     OW: Pointer;
     Dr: String;
     MemReq: LongInt;
     MAvail: LongInt;
   {$IFDEF CHDIRTIMER}
     ev: TEvent;
   {$ENDIF}
begin
{$IFDEF CHDIRTIMER}
 if ChDirTimer and (ShiftState and (kbLeftShift+kbRightShift) <> 0) and
   (DDTimer = 0) then DDTimer := Get100s;
{$ENDIF}
 ReadArchive; {AK155 26-11-2002}
{While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do SetLength(CurDir, Length(CurDir)-1);}
 AFiles := New(PFilesCollection, Init($10, $10));
{FD := New(PFilesCollection, Init($40, $10));}
 PFilesCollection(AFiles)^.Owner := Owner;
 PFilesCollection(AFiles)^.SortMode := Sortmode;
{$IFDEF Win32}
 PFilesCollection(AFiles)^.LFNActive := True; {JO}
{$ENDIF}
 GetDirectory := AFiles;
 While (PosChar(CurDir[Length(CurDir)], '.\') > 0) do SetLength(CurDir, Length(CurDir)-1);
 FD := New(PFilesCollection, Init($40, $10));
 FreeSpace := ''; TTL := 0; TPL := 0;
{GetDirectory := AFiles;} AllFiles := (FileMask = x_x) or (FileMask='*');
 FD^.SortMode := 141;
 FD^.Duplicates := False;
{$IFDEF Win32}
 FD^.LFNActive := True; {JO}
{$ENDIF}
 AFiles^.Duplicates := False;
 Files^.ResetPointer('');
{JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
{    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
{    доступный изначально объём                                              }
 MemReq := LowMemSize;
 MAvail := MaxAvail;
 While not Files^.Last and Files^.GetNextFile and (MAvail > MemReq) do
  begin
    _USize := Files^.CurFile.Size;
    _PSize := Files^.CurFile.CSize;
    if (UpStrg(CurDir + '\') = UpStrg(Files^.LastDir)) and
       (AllFiles or InFilter(Files^.CurFile.Name, FileMask)) then
      begin
        if Files^.CurFile.Name = '' then Continue;
        if Files^.CurFile.Name = '..' then Continue;
        with Files^.CurFile do
         begin
          F := NewFileRec(Name,{$IFNDEF OS2}GetURZ(Name),{$ENDIF}
                          _USize, Date, 0, 0, Attr, @CurDir);
          Inc(MemReq, SizeOf(TFileRec));
          Inc(MemReq, Length(CurDir+Name)+2);
         end;
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
                CopyShortString(Dr, FR.FlName[true]);
                FR.Attr := Directory;
                I := FD^.IndexOf(@FR);
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
                Inc(MemReq, SizeOf(TFileRec));
                Inc(MemReq, Length(CurDir+Dr)+2);
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

 NoMemory := MAvail <= MemReq;
 FreeSpace := '';
 TotalInfo := TTL;

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
{$IFDEF CHDIRTIMER}
 if ChDirTimer and (ShiftState and (kbLeftShift+kbRightShift) <> 0) then
  begin
   DDTimer := Get100s-DDTimer;
   ev.what := evCommand;
   ev.command := cmShowTimeInfo;
   ev.infoptr := nil;
   Application^.PutEvent(ev);
  end;
{$ENDIF}
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.UseFile;
 var SS, S, S2, Q: String;
     C: Char;
     Unp: String;
     ATime: Longint;
     ASize: TSize;
     AAttr: Word;
{$IFNDEF OS2}
label TryAgain;
{$ENDIF}
begin
   TempFile:=''; {-$VOL}
   if (Command = cmEditFile) or (Command = cmFileEdit) or
      (Command = cmIntEditFile) or (Command = cmIntFileEdit) then Exit;
   case Command of
    cmDBFView: C := '=';
    cmWKZView: C := '>';
    cmTextView, cmViewText: C := '<';
    cmHexView: C := '|';
    cmIntFileView: C := '-';
    else C := '+';
   end;
   {if not CheckPassword(AF) then Exit;}
   S := ' ';
   if P^.Attr and Hidden <> 0 then
    begin
     S := '';
{$IFNDEF OS2}
TryAgain:
{$ENDIF}
     if ExecResource( dlgSetPassword, S ) <> cmOK then Exit;
   { Flash >>> }
    if CheckForSpaces(S) then
      S := ' ' + CnvString(AType^.Garble) + S + ' '
     else
     {$IFNDEF OS2}
      if AType^.UseLFN then
     {$ENDIF}
        S := ' ' + CnvString(AType^.Garble) + '"' + S + '"' + ' '
     {$IFNDEF OS2}
       else
        begin
          MessageBox(GetString(dlSpacesInPassword), nil, mfWarning + mfOkButton);
          goto TryAgain;
        end
     {$ENDIF}
        ;
   { Flash <<< }
    end;
   SS := MakeNormName(P^.Owner^, P^.FlName[true]);
   if SS[1] = '\' then Delete(SS, 1, 1); {DelFC(SS);}
  {$IFNDEF OS2}
   if AType^.UseLFN then S2 := ArcName
     else S2 := lfGetShortFileName(ArcName);
   if ArcName[Length(ArcName)]='.' then S2 := S2 + '.';
   S := CnvString(AType^.Extract) + ' ' + s +
      CnvString(AType^.ForceMode) + ' ' +
      SquashesName(S2) + ' ' + SquashesName(SS) + ' ';
  {$ELSE}
   S := CnvString(AType^.Extract) + ' ' + s +
      CnvString(AType^.ForceMode) + ' ' +
      SquashesName(ArcName) + ' ' + SquashesName(SS) + ' ';
  {$ENDIF}
{   DelDoubles('  ',S);}{piwamoto: files can have 2 spaces in names}
   TempFile := C + MakeNormName(TempDir , P^.FlName[true]);
   Q := '|' + GetRealName+':'+MakeNormName(CurDir, P^.FlName[true]);

   S2 := Copy(TempFile,2,MaxStringLength);

   if C in ['<','-','|','+'] then TempFile := TempFile + Q;

   GetFTimeSizeAttr(S2, ATime, ASize, AAttr);
   if not ExistFile(S2) or (PackedDate(P)<> ATime) or (P^.Size <> ASize) then

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
    Exec(Unp , {$IFDEF RecodeWhenDraw}OemToCharStr{$ENDIF}(s), '', False);
    Advance2.lChDir(DirToChange); DirToChange := '';
   end;
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
       // DelDoubles('  ', S);{files can have 2 spaces in names}
       // AnsiDelDoubles('  ', SS1);
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
      // DelDoubles('  ', S);{files can have 2 spaces in names}
      // AnsiDelDoubles('  ', Lst);
      SwapVectors;
      AnsiExec(GetEnv('COMSPEC'),'/c ' + S + ' ' + Lst + ' ');
      DE := DosError; ClrIO;
      SwapVectors;
      EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.LST'); {DataCompBoy}
    end;
{$ENDIF}
  InitDOSMem;
  InitMemory;
  InitVideo;
  InitEvents;
  InitSysError;
  case DE of
     0 : Application^.Redraw;
     8 : StdMsg(1);
    else StdMsg8;
  end;
// JO: закомментарил, т.к. теперь после разархивирования идёт копирование
//     через временный каталог и после его удаления панель перечитывается
{ GlobalMessage(evCommand, cmPanelReread, nil);
  GlobalMessage(evCommand, cmRereadInfo, nil);}
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
  if TypeOf(Prev^)=TypeOf(TDrive) then Prev^.lChDir(GetPath(VArcName)); {piwamoto: VArcName is a feature, not a bug :-)}
  if (Prev^.DriveType = dtDisk) and
     (PView(Owner)^.GetState(sfSelected+sfActive)) then
      ActivePanel := Owner;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  if (Prev^.DriveType = dtDisk) then S := GetName(VArcName)
    else S := GetName(ArcName);
  Prev := nil;
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

  function GetArcOwn(S: String): String;
  begin
    if Length(S) < 2 then begin Result := ''; Exit; end;
    S[2] := ';'; {JO: реально не важно на что меняем, лишь бы не ':'}
    Result := Copy(S, PosChar(':', S) + 1, MaxStringLength);
  end;

  procedure PutDir(SS: String);
   var I: Integer;
       S1: String;
  begin
   if not (SS[Length(SS)] in ['\','/']) then AddStr(SS, '\');
   Files^.ResetPointer('');
   while not Files^.Last and Files^.GetNextFile do
     if (SS = Copy(Files^.LastDir, 1, Length(SS)))
          and (Files^.CurFile.Name <> '') then
      begin
        S1 := Files^.LastDir + Files^.CurFile.Name;
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
{JO: проверка для разархивирования из панели поиска в архивах}
   if PathFoundInArc(PF^.Owner^) then
     S1 := MakeNormName(GetArcOwn(PF^.Owner^), PF^.FlName[true])
    else
     S1 := MakeNormName(PF^.Owner^, PF^.FlName[true]);
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
     S, SCr: String;
     DT: Record S: String; W: Word; Psw: String[30]; end;
     ExtrChar: String;
     Nm: String;
     Xt: String;
     Pswd: Boolean;
     B: Boolean;
     SCurDir: String;
     Unp: String;
     DNN: Byte;
     TempExtrDir: String;
     TempDirUsed: Boolean;
     FCT: PFilesCollection;
     FRT: PFileRec;
     OldConfirms: Word;
     PV: PView;
     Inhr: Byte;
     SR: lSearchRec;

{$IFNDEF OS2}
label TryAgain;
{$ENDIF}

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

  SCurDir := CurDir;
  SCr := '';
  if (SCurDir <> '') then
    begin
      While (SCurDir[Length(SCurDir)] = '.') do SetLength(SCurDir, Length(SCurDir)-1);
      While (SCurDir <> '') and (SCurDir[1] = '\') do Delete(SCurdir, 1, 1);
      if (SCurDir[Length(SCurDir)] <> '\') then SCurdir := SCurdir + '\';
      if (CnvString(AType^.SetPathInside) <> '') then
        begin
          SCr := ' ' + CnvString(AType^.SetPathInside) + SCurDir + ' ';
          SCurDir := '';
        end;
    end;

{JO}
 // проверяем, содержит ли каталог назначения файлы
  DosError := 0;
  lFindFirst(MakeNormName(ExtrDir, x_x), Anyfile, SR);  {JO}
  if IsDummyDir(SR.Fullname) then lFindNext(SR);
  if IsDummyDir(SR.Fullname) then lFindNext(SR);
  lFindClose(SR);
 // для разархивирования на дискеты и тестирования не используем
 // временный подкаталог
  if ((Options and 8) = 0) or ((Options and 2) <> 0) or
      ((DosError <> 0) and (SCurDir = '')) then
    begin
      TempExtrDir := ExtrDir;
      TempDirUsed := False;
    end
   else
    begin
   { даём имя временному подкаталогу в каталоге назначения}
      DNN := DNNumber;
      while true do
        begin
          TempExtrDir := ExtrDir + '$DN'+ItoS(DNN)+'$.EDR';
          ClrIO;
          if PathExist(TempExtrDir) then Inc(DNN) else Break;
        end;
      ClrIO;
      TempExtrDir := TempExtrDir + '\';
      TempDirUsed := True;
    end;
{/JO}

  SS := MakeListFile(AFiles, True, B);
  S := ' '; Pswd := false;
  AFiles^.ForEach(@Unselect);
  ExtrChar := CnvString(AType^.ExtractWP);
  if Options and 1 = 0 then ExtrChar := CnvString(AType^.Extract);
  if Options and 2 <> 0 then ExtrChar := CnvString(AType^.Test);
  if Pswd then
   begin
    if Password = '' then
{$IFNDEF OS2}
TryAgain:
{$ENDIF}
      if ExecResource(dlgSetPassword, Password) <> cmOK then Exit;
   { Flash >>> }{JO: взял код Flash из Arcview.TArcDrive.UseFile }
    if CheckForSpaces(Password) then
      S := ' ' + CnvString(AType^.Garble) + Password + ' '
     else
     {$IFNDEF OS2}
      if AType^.UseLFN then
     {$ENDIF}
        S := ' ' + CnvString(AType^.Garble) + '"' + Password + '"' + ' '
     {$IFNDEF OS2}
       else
        begin
          MessageBox(GetString(dlSpacesInPassword), nil, mfWarning +
                                                          mfOkButton);
          goto TryAgain;
        end
     {$ENDIF}
        ;
   { Flash <<< }
   end;
{$IFNDEF OS2}
  if AType^.UseLFN then
{$ENDIF}
    ArchiveName := SquashesName(ArcName)
{$IFNDEF OS2}
    else ArchiveName := SquashesName(lfGetShortFileName(ArcName))
{$ENDIF}
  ;
  if ((Options and 4 <> 0) or TempDirUsed) and
      (CnvString(AType^.ForceMode) <> '') then
    S := S + CnvString(AType^.ForceMode) + ' ';
  S := S + SCr; {установка пути внутpи аpхива}
  S := ExtrChar + ' ' + S + ArchiveName;
  Unp := CnvString(AType^.unPacker);
  if (AType^.GetID = arcRAR) and (PosChar(';', Unp) > 0) then
    begin
     if PRARArchive(AType)^.VersionToExtr > 20 then
           Unp := Copy(Unp, PosChar(';', Unp)+1, 255)
      else Unp := Copy(Unp, 1, PosChar(';', Unp)-1);
    end;
  lGetDir(0, DirToChange);
  Inhr := CreateDirInheritance(ExtrDir, True);
  CreateDirInheritance(TempExtrDir, False);
//JO: если каталог назначения не создался (напpимеp, если диск доступен
//    только на чтение), то нет смысла и вызывать аpхиватоp
  if not PathExist(TempExtrDir) then Exit;
  Advance2.lChDir(TempExtrDir);
  Exec(Unp ,{$IFDEF RecodeWhenDraw}OemToCharStr{$ENDIF}(s), SS, B);
{JO}
  if not TempDirUsed then
    begin
      Advance2.lChDir(DirToChange); DirToChange := '';
      GlobalMessage(evCommand, cmPanelReread, nil);
      GlobalMessage(evCommand, cmRereadInfo, nil);
      Exit;
    end
   else
    begin
    { перекидываем файлы из временного подкаталога в каталог назначения}
      PV := New(PUserWindow, Init);
      Desktop^.Insert(PV);
      CopyDirContent(TempExtrDir+SCurDir, ExtrDir, True,(Options and 4 <> 0));
      PV^.Free;
    { удаляем временный каталог со всем, что в нём осталось}
      SetLength(TempExtrDir, Length(TempExtrDir) - 1);
      S := GetPath(TempExtrDir);
      FRT := NewFileRec(GetName(TempExtrDir),
                        {$IFNDEF OS2}
                        GetName(TempExtrDir),
                        {$ENDIF}
                        0,0,0,0, Directory,
                        @S);
      New(FCT, Init(1,1));
      FCT^.AtInsert(0,FRT);
      OldConfirms := Confirms;
      Confirms := 0;
      Advance2.lChDir(S);
      Eraser.EraseFiles(FCT);
      Advance2.lChDir(DirToChange); DirToChange := '';
      Confirms := OldConfirms;
      FCT^.DeleteAll;
      Dispose(FCT, Done);
      if Inhr > 0 then
        begin
          GlobalMessage(evCommand, cmPanelReread, nil);
          GlobalMessage(evCommand, cmRereadInfo, nil);
        end;
    end;
{/JO}
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TArcDrive.CopyFiles;
 var DT: Record S: String; W: Word; Psw: String[30]; end;
     ExtrDir: String;
     DDr: Char;
begin
  ExtrDir := '';
  DT.S := '';
  DT.Psw := Password;
  DT.W := UnarchiveOpt and not 2; {JO}
  Message(Application, evCommand, cmPushFirstName, @DT.S);
  if CopyDirName <> '' then DT.S := CopyDirName;
 {if DT.S = cTEMP_ then DT.S := '';}
  if DT.S = '' then GlobalMessageL(evCommand, cmPushName, hsExtract);
  if DT.S = '' then DT.S := HistoryStr(hsExtract, 0);
 {if DT.S = cTEMP_ then DT.S := '';}
  CopyDirName := '';
  if DT.S = cTEMP_ then
    begin
     CopyToTempDrive(AFiles, Own, ArcName);
     Exit;
    end;
{JO}
// пpовеpяем, находится ли диск в списке дисков, на котоpые надо
// pазаpхивиpовать не чеpез вpеменный подкаталог (по умолчанию A: и B:)
  if (DT.S <> '') and (Length(DT.S) >= 2) then
   begin
    if DT.S[2] = ':' then DDr := UpCase(DT.S[1])
      else DDr := #1; {любой символ не входящий в 'A'..'Z'}
   end
  else
   begin
    lGetDir(0, ExtrDir);
    DDr := UpCase(ExtrDir[1]);
    ExtrDir := '';
   end;
  if (DDr in ['A'..'Z']) and
     ((SystemData.Drives[DDr]{$IFNDEF OS_DOS}shl 1{$ENDIF}) and
       ossUnarcToDirectly <> 0) then
       DT.W := DT.W and not 8
  else DT.W := DT.W or 8;
{/JO}
  if not SkipCopyDialog then
     if ExecResource( dlgExtract, DT) <> cmOK then Exit;
{JO}
  if ((DT.W and 1) <> (UnarchiveOpt and 1)) or
     ((DT.W and 4) <> (UnarchiveOpt and 4)) then ConfigModified := True;
  UnarchiveOpt := (DT.W and not 2) or 8;
{/JO}
  SkipCopyDialog := false;
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
         S := GetString(dlEraseConfirm1) + PF^.FlName[true] + ' ?';
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
  ForceRescan := false;
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
 B := LFNLen + 1 + CalcLengthWithoutName;
 if B > MaxViewWidth then B := MaxViewWidth;
 CalcLength := B;
end;

procedure TArcDrive.MakeTop;
begin
if (LFNLen < 250) then
  begin
    if LFNLen <= SizeX then
      S := CenterStr(GetString(dlTopLFN), LFNLen)+GetString(dlTopSplit)
     else S := AddSpace(CenterStr(GetString(dlTopLFN), SizeX), LFNLen)
  end else S := '';
 if Flags and psShowSize     <> 0 then S := S + Copy(GetString(dlTopOriginal),2-CountryInfo.TimeFmt,255);
 if Flags and psShowPacked   <> 0 then S := S + GetString(dlTopPacked);
 if Flags and psShowRatio    <> 0 then S := S + GetString(dlTopRatio);
 if Flags and psShowDate     <> 0 then S := S + GetString(dlTopDate);
 if Flags and psShowTime     <> 0 then S := S + Copy(' '+GetString(dlTopTime),1+CountryInfo.TimeFmt,255);
 if (LFNLen >= 250) then
   S := S  + AddSpace(CenterStr(GetString(dlTopLFN), SizeX-Length(S)), LFNLen);
end;

procedure TArcDrive.GetFull;
var
  S: String;
  S1: String[40];
  X: AWord;
  FD: TDate4;
  NFM: TNameFormatMode;
  OPT: Word;
  LS: Byte;
  CO: Word;
  label Longer250, Longer250_1;
begin
X := 0;
CO := C;
{JO: если ширина колонки имени больше 250 символов, её показываем после     }
{    остальных колонок                                                      }
if (LFNLen < 250) then
Longer250:
 begin
    s:=P^.FlName[true];
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
  MoveCStr(TWordArray(B)[X], S, C);
  Inc(X, LFNLen);
  if X >= 255 then Exit;
{-----------}
  if (LFNLen >= 250) then goto Longer250_1;
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

  FD := TDate4(P^.FDate);
  MakeDate(DateMode, FD.Day, FD.Month, P^.Yr, FD.Hour, FD.Minute, S1);
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
      S := ZtoS(Percent(P^.Size, P^.PSize));
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
  if (LFNLen >= 250) then begin C := CO; goto Longer250; end;
Longer250_1:
end;

procedure TArcDrive.GetEmpty;
 var
  X: AWord;
begin
  if (LFNLen < 250) then
    begin
      X:=LFNLen+1;
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
 ForceRescan := true;
 ArchiveFiles(GetRealName, AFiles, MoveMode, nil);
 ForceRescan := false;
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
  Var CDir: String;
      Opts: Byte;
 begin
// JO: пpовеpяем, находится ли диск в списке дисков, на котоpые надо
//     pазаpхивиpовать не чеpез вpеменный подкаталог (по умолчанию A: и B:)
  lGetDir(0, CDir);
  if (UpCase(CDir[1]) in ['A'..'Z']) and
     ((SystemData.Drives[UpCase(CDir[1])]{$IFNDEF OS_DOS}shl 1{$ENDIF}) and
       ossUnarcToDirectly <> 0) then Opts := 1
  else Opts := 9;
  ExtractFiles(C, '', Owner, Opts);
 end;

 var PDr: PDrive;
     S: String;
     O: PView;

begin
 case Command of
   cmSetPassword: SetPassword;
   cmArcTest: TestFiles;
   cmExtractTo: Extract;
   cmMakeForced: ForceRescan := true;
   cmRereadForced: if ForceRescan then
                    begin
                      ForceRescan := false;
{$IFNDEF MINARCH}
                      if (AType^.GetID = arcUC2) or (AType^.GetID = arcAIN) then
                        begin
                          PFilePanel(Owner)^.ForceReading := true;
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
     Xt: String;
     I: Byte;
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
 P := New(PArcDrive, Init(AName, VAName, num));
 if Abort then
   begin ArcViewer := True; Exit; end; { AK155 21-06-2002
    Если файл не прочитался в качестве архива, то он и любым другим
    спосбом не прочитается, так что ArcViewer берет ответственность
    на себя, чтобы во вьювере не продолжать бессмысленные попытки. }
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
 if E.What <> evNothing then
   begin
     Dispose(P, Done);
     Exit;
   end
    else ArcViewer := True;
{JO: переходим к найденному файлу в архиве}
 if PathInside <> '' then
   begin
     if Copy(PathInside, Length(PathInside)-1, 2) = '\.' then SetLength(PathInside, Length(PathInside)-2);
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

  if AType^.GetID = arcRAR then B.VolumeID := NewStr(GetString(dlDIVersionToExtract)+RtoS(PRARArchive(AType)^.VersionToExtr / 10, 4, 2)+'~');
  if AType^.GetID = arcACE then B.VolumeID := NewStr(GetString(dlDIVersionToExtract)+RtoS(ACEVerToExtr / 10, 4, 2)+'~');

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
      Root: string;
      PDir: PString;
      PI: PView;
      MemReq: LongInt;
      MAvail: LongInt;
begin
  NewTimer(Tmr,0);
  Dirs := New(PStringCollection, Init($10,$10,False));
  PI := WriteMsg(GetString(dlReadingList));
  New(Fils, Init($10, $10)); Fils^.SortMode := psmOrdered;
  Files^.ResetPointer('');
  Root := UpStrg(CurDir) + '\'; l := length(Root);
{JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
{    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
{    доступный изначально объём                                              }
  MemReq := LowMemSize;
  MAvail := MaxAvail;
  While not Files^.Last and Files^.GetNextFile and (MAvail > MemReq)
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
         Inc(MemReq, SizeOf(ShortString));
         FR := NewFileRec(Name,{$IFNDEF OS2}GetURZ(Name),{$ENDIF}
                         _USize, Date, 0, 0, Attr, PDir);
         Inc(MemReq, SizeOf(TFileRec));
         Inc(MemReq, Length(PDir^+Name)+2);
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
  PDrv := New(PFindDrive, Init(GetString( dlBranch )+Dir, Dirs, Fils,
                                                                  FreeByte));
  PDrv^.NoMemory := MAvail <= MemReq;
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
  var I: LongInt;
      PDrv: PDrive;
      Dirs: PStringCollection;
      Fils:PFilesCollection;
      FR: PFileRec;
      tmr: TEventTimer;
      _USize, _PSize: TSize;
      L: Integer;
      Root: String;
      PDir: PString;
      LDir, DrName: String;
      DateAfter, DateBefore,
      SizeGreat, SizeLess: LongInt;
      Attr: Byte;
      PI: PView;
      MemReq: LongInt;
      MAvail: LongInt;

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

  Dirs := New(PStringCollection, Init($10,$10,False));
  PI := WriteMsg(^M^M^C + GetString(dlSearching) + '...');
  New(Fils, Init($10, $10)); Fils^.SortMode := psmOrdered;
  Files^.ResetPointer('');
  Root := UpStrg(CurDir) + '\'; l := length(Root);
{JO: сначала один pаз опpеделяем объём доступной памяти, а затем по ходу дела}
{    подсчтитываем насколько тpебования памяти pастут и не пpевысили ли они  }
{    доступный изначально объём                                              }
  MemReq := LowMemSize;
  MAvail := MaxAvail;
  While not Files^.Last and Files^.GetNextFile and (MAvail > MemReq)
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
           and InFilter(Name, ArcFindRec.Mask+ArcFindRec.AddChar) then
            begin
              _USize := Size;
              _PSize := CSize;
              PDir := NewStr(Files^.LastDir);
              Inc(MemReq, SizeOf(ShortString));
              FR := NewFileRec(Name,{$IFNDEF OS2}GetURZ(Name),{$ENDIF}
                              _USize, Date, 0, 0, Attr, PDir);
              Inc(MemReq, SizeOf(TFileRec));
              Inc(MemReq, Length(PDir^+Name)+2);
              if Dirs^.IndexOf(PDir) = -1 then Dirs^.Insert(PDir);
              Fils^.AtInsert(Fils^.Count, FR);
            end;
         {JO: добавляем каталоги}
          if Length(Files^.LastDir) > L then
            begin
              LDir := Files^.LastDir;
              repeat
                SetLength(LDir, Length(LDir)-1);
                for I := Length(LDir) downto L do
                  if LDir[I] = '\' then Break;
                DrName := Copy(LDir, I+1, MaxStringLength);
                SetLength(LDir, I);
                if (DrName <> '')
             //JO: всё равно для каталогов дата и атрибуты показываются
             //    весьма условно, размер равен нулю, так что при расширенном
             //    поиске каталоги только мешают
                  and (ArcFindRec.Options and ffoAdvanced = 0)
                  and ((ArcFindRec.Options and 2 <> 0) or
                       (Root = UpStrg(LDir)))
                  and InFilter(DrName, ArcFindRec.Mask+ ArcFindRec.AddChar)
                  then
                   begin
                     PDir := NewStr(Copy(LDir, 1, I));
                     Inc(MemReq, SizeOf(ShortString));
                     FR := NewFileRec(DrName,{$IFNDEF OS2}GetURZ(DrName),{$ENDIF}
                                0, ArcDate, 0, 0, $80 or Directory, PDir);
                     Inc(MemReq, SizeOf(TFileRec));
                     Inc(MemReq, Length(PDir^+DrName)+2);
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
  PDrv := New(PFindDrive, Init(GetString(dlFindPanel) + ArcFindRec.Mask,
                                                     Dirs, Fils, FreeByte));
  PDrv^.NoMemory := MAvail <= MemReq;
  Message(Owner, evCommand, cmInsertDrive, PDrv);
end;

end.
