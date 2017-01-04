{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.09
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

UNIT Arvid;

INTERFACE
uses
  Collect, Objects, DOS, Drives, FilesCol, Views, DiskInfo, Tree, Histries,
  Lfn, LfnCol, FindObj;

type
   TTdrHeader = record
      FileTableOfs: LongInt;
      DirTableOfs:  LongInt;
      PosTableOfs:  LongInt;

      FileTableLen: LongInt;
      DirTableLen:  LongInt;
      PosTableLen:  LongInt;

      TapeFmt:      AWord;
      TapeID:       AWord;
      TapeLen:      AWord;
      RecordLen:    AWord;
      NewRecordSector: LongInt;

      DescTableOfs: LongInt;
      Res01: Array [1..16] of Byte;
      DescTableLen: LongInt;
      Res02: Array [1..16] of Byte;
      LastNewRecordSector: LongInt;
      Res03: Array [1..36] of Byte;
   end;

   PTdrFileCell = ^TTdrFileCell;
   TTdrFileCell = record
      Name: Array[1..11] of Char;
      Attr: Byte;
      StartSector: LongInt;
      Description: LongInt;
      Res: AWord;
      Time: LongInt;
      Cluster: AWord;
      Size: LongInt;
   end;

   TTdrDirCell = record
      Level: AWord;
      Name: Array[1..11] of Char;
      Attr: Byte;
      StartSector: LongInt;
      Description: LongInt;
      Res: AWord;
      Time: LongInt;
      Cluster: AWord;
      Size: LongInt;

      Files: AWord;
      LastFile: AWord;
      NumFiles: AWord;

   end;

Type
   TAvtHeader = record
      Signature:     Array[1..4] of Char;        {AVTP}
      AvtFmt:        LongInt;
      CheckSum:      LongInt;
      AfterLastCell: LongInt;
      FreeCell:      LongInt;
      RootDirCell:   LongInt;
      NewSector:     LongInt;
      LastNewSector: LongInt;
      AvtMediaCell:  LongInt;
      Undefined1:    LongInt;
   end;

   TAvtMediaCell = record
      NextMediaCell:           LongInt;
      TapeFmt:                 AWord;            {16,32}
      TapeLen:                 AWord;
      TapeID:                  LongInt;
      Undefined1:              LongInt;
      Undefined2:              LongInt;
      Undefined3:              LongInt;
      FirstSectorNum:          LongInt;
      Sectors:                 LongInt;
      PositionTable:           LongInt;
      PositionTableSize:       LongInt;
   end;

   TAvtFileCell = record
      LeftFileCell:  LongInt;
      RightFileCell: LongInt;
      ChildOrSize:   LongInt;
      Time:          LongInt;
      StartSector:   LongInt;
      Flags:         AWord;
      Attr:          AWord;
      case Char of
        0: (Name0: Array[1..16] of Char);
        1: (Name1: Array[1..12] of Char; Next1: LongInt);
        2: (Name2: Array[1..12] of Char; DescPtr2: LongInt);
        3: (NamePtr, NonUsed1, NonUsed2, DescPtr3: LongInt);
   end;

   TAvtTextCell = record
      NextTextCell: LongInt;
      Data: Array[1..36] of Char;
   end;

const
      avtLogSectors = $00FF;  { Mask's for TAvtFileCell.Flags }
      avtBalance    = $0300;
      avtCellFormat = $0C00;
      avtIsDir      = $1000;

      avtCell0      = $0000;
      avtCell1      = $0400;
      avtCell2      = $0800;
      avtCell3      = $0C00;

type
   TAvdType = (avdTdr, avdAvt);

   PArvidDrive = ^TArvidDrive;
   TArvidDrive = object(TDrive)
      Name: PString;
      Stream: PStream;
      CurFile: LongInt;
      CurDirPos: LongInt;
      PosTableOfs: LongInt;
      CurFileNum: AWord;
      CurLevel: AWord;
      CurDate: LongInt;
      KillAfterUse: Boolean;
      FileType: TAvdType;
      D: TTdrHeader;
      AVT: TAvtHeader;
      TapeFmt: AWord;
      TapeTotalTime: AWord;
      TapeRecordedTime: AWord;
      TotFiles: LongInt;      {in directory}
      TotLen: LongInt;
      CurDirCellPos: LongInt; {this for AVT: location of directory cell.
                               CurDirPos for AVT is position of a root node}

      constructor Init(const AName: String; num: byte);
      procedure lChDir(ADir: String); virtual;
      function GetDir: String; virtual;
      function GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String;
                            var FreeSpace, TotalInfo: String ): PCollection; virtual;
      constructor Load(var S: TStream);
      procedure Store(var S: TStream); virtual;
      procedure RereadDirectory(S: String); virtual;
      procedure KillUse; virtual;
      procedure UseFile(P: PFileRec; Command: Word); virtual;
      procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
      procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
      procedure EraseFiles(AFiles: PCollection); virtual;
      function  GetRealName: String; virtual;
      procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;
      procedure MakeDir; virtual;
      function  isUp: Boolean; virtual;
      procedure ChangeUp(var S: String); virtual;
      procedure ChangeRoot; virtual;
      procedure GetFreeSpace(var S: String); virtual;
      procedure GetDirInfo(var B: TDiskInfoRec); virtual;
      procedure EditDescription(PF: PFileRec); virtual;
      procedure GetDirLength(PF: PFileRec); virtual;
      procedure GetParam(n: byte); virtual; {DataCompBoy}
      destructor Done; virtual;
      procedure SeekDirectory;
    private
      procedure Kill;
   end;


const
  ArvidDrives: PCollection = nil;
  ArvidWithDN: Boolean     = True;

var
  ArvidDeleteAllFiles: Boolean;

IMPLEMENTATION
uses
  Advance, Advance2, Advance1, Advance3, FlPanel, Commands, Startup, DnApp,
  Drivers, Messages, Dialogs, Memory, FileFind, DnUtil, FileCopy, u_KeyMap,
  ArvidAvt,ArvidTdr;

        {-DataCompBoy-}
procedure TArvidDrive.GetParam;
begin
 Param:=n;
 with ColumnsDefaultsArvd do
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

constructor TArvidDrive.Init;
var Attrb: Word;
    i: integer;
    q: string;

  procedure GetAttr;
  var F: lfile; begin lAssignFile(F, AName); lGetFAttr(F, Attrb) end;

  procedure SetAttr(A: Word);
  var F: lfile; begin lAssignFile(F, AName); lSetFAttr(F, A) end;

begin
  TObject.Init;
  if num<1 then GetParam(1)
           else GetParam(num);
  innum:=num;

  i:=PosChar(':', Copy(AName, 3, 255))+2;
  if i>2 then begin
   q:=Copy(AName,I+1,255);
   if Q[Length(Q)] in ['\','/'] then Dec(Q[0]);
   Stream:=New(PBufStream, Init(Copy(AName, 1, I-1), stOpen, 2048));
  end else
   Stream:=New(PBufStream, Init(AName, stOpen, 2048));

  if Stream^.Status <> stOK then begin
    GetAttr; if Attrb and ReadOnly <> 0 then
    begin
      SetAttr(Archive);
      if (IOResult <> 0 ) then begin Dispose(Stream, Done); Stream:=nil; Fail; end;
    end;
    if i>0
     then Stream:=New(PBufStream, Init(Copy(AName, 1, I-1), stOpen, 2048))
     else Stream:=New(PBufStream, Init(AName, stOpen, 2048));
    if Stream^.Status <> stOK then begin Dispose(Stream, Done); Stream:=nil; Fail end;
  end;
  FileType := avdAvt;
  Stream^.Read(AVT, SizeOf(AVT));
  if Stream^.Status <> stOK then begin Dispose(Stream,Done); Fail end;
  if AVT.Signature <> 'AVTP'
   then if not TdrInit(@Self) then fail else
   else if not AvtInit(@Self) then fail;
  Name:=NewStr(lFExpand(AName));
  DriveType:=dtArvid;
  KillAfterUse:=TempFile <> '';
  TempFile:='';
  if I>2 then CurDir:=Q else CurDir:='\';
  SeekDirectory;
  if ArvidDrives = nil then New(ArvidDrives, Init($100, $100));
  ArvidDrives^.Insert(@Self);
  AddToDirectoryHistory(Name^+':'+CurDir,Integer(DriveType));
end;

procedure TArvidDrive.SeekDirectory;
begin
  case FileType of
    avdTdr: TdrSeekDirectory(@Self);
    avdAvt: AvtSeekDirectory(@Self);
  end;
end;

procedure TArvidDrive.Kill;
begin
  if not KillAfterUse then Exit;
  if Stream <> nil then Dispose(Stream, Done); Stream := nil;
  EraseFile(Name^);
end;

function TArvidDrive.GetDirectory;
var
  FC: PFilesCollection;
  P: PString;
  DT: DateTime;
  AllFiles, ShowD: Boolean;
  TAttr: Word;
begin
  New(FC, Init($80, $40));
  FC^.SortMode := SortMode;

  if ArvidWithDN then GetFreeSpace(FreeSpace);
  TotalInfo := ''; TotFiles := 0; TotLen := 0;

  GetDirectory := FC;
  ShowD := (Flags and psShowDescript <> 0);
  AllFiles := (FileMask = x_x) or (FileMask='*');

  case FileType of
    avdTdr: TdrGetDirectory(@Self, CurDirPos, FC, ShowD, FileMask);
    avdAvt: AvtGetDirectory(@Self, CurDirPos, FC, ShowD, FileMask);
  end;

  if CurDir = '' then P := Name else P := @CurDir;

  if ArvidWithDN then TotalInfo:=CalcTotalInfo(FC^.Count, TotFiles, TotLen);

  FC^.AtInsert(0, NewFileRec('..', '..', 0, CurDate, Directory, P));
end;

destructor TArvidDrive.Done;
begin
  if ArvidDrives<>nil then begin
    ArvidDrives^.Delete(@Self);
    if ArvidDrives^.Count = 0 then Dispose(ArvidDrives, Done);
    ArvidDrives:=nil;
  end;
  if Stream <> nil then Dispose(Stream, Done); Stream:=nil;
  DisposeStr(Name);
  inherited Done;
end;

procedure TArvidDrive.lChDir;
 var Dr: String;
     Nm: String;
     Xt: String;
begin
 if ADir = #0 then Exit;
 lFSplit(ADir, Dr, Nm, Xt);
 if (Nm = '.') and (Xt = '.') then begin
   if Dr <> '' then CurDir := Dr;
   if CurDir[1] <> '\' then Insert('\', CurDir, 1);
   repeat Dec(CurDir[0]) until (CurDir = '') or (CurDir[Length(CurDir)] = '\');
   if CurDir <> '' then Dec(CurDir[0]);
 end else CurDir := ADir;
 SeekDirectory;
 While (PosChar(CurDir[Length(CurDir)], ' .\') > 0) do Dec(CurDir[0]);
 AddToDirectoryHistory(Name^+':'+CurDir,Integer(DriveType));
end;

function TArvidDrive.GetDir;
 var Dr: String;
     Nm: String;
     Xt: String;
begin
 lFSplit(Name^, Dr, Nm, Xt);
 if FileType = avdTdr
  then GetDir:= 'TDR:'+Nm+'\'+CurDir
  else GetDir:= 'AVT:'+Nm+'\'+CurDir;
end;

constructor TArvidDrive.Load;
  label 1;
begin
  TObject.Init;
  inherited Load(S);
  DriveType := dtArvid;
  S.Read(KillAfterUse, 1);
  Name := S.ReadStr;
  Stream := New(PBufStream, Init(Name^, stOpen, 2048));
  if Stream^.Status <> stOK then
    begin
1:
      Dispose(Stream, Done);
      Stream:=nil;
      Done;
      Fail;
    end;
  FileType := avdAvt;
  Stream^.Read(AVT, SizeOf(AVT));
  if Stream^.Status <> stOK then begin Dispose(Stream,Done); Done; Fail end;
  if AVT.Signature <> 'AVTP'
   then if not TdrLoad(@Self, S) then goto 1
   else if not AvtLoad(@Self, S) then goto 1;
  if Stream^.Status <> stOK then Goto 1;
  SeekDirectory;
  if ArvidDrives = nil then New(ArvidDrives, Init($100, $100));
  ArvidDrives^.Insert(@Self);
end;

procedure TArvidDrive.Store;
begin
  inherited Store(S);
  S.Write(KillAfterUse, 1);
  S.WriteStr(Name);
end;

procedure TArvidDrive.RereadDirectory;
begin
  if Prev <> nil then Prev^.RereadDirectory(S);
  if FileType = avdAvt then begin
   Dispose(Stream,Done);
   Stream:=New(PBufStream, Init(Name^, stOpen, 2048));
   Stream^.Seek(0);
   Stream^.Read(AVT, SizeOf(AVT));
   SeekDirectory;
  end;
end;

procedure TArvidDrive.KillUse;
begin
  if Prev <> nil then Prev^.KillUse;
  Kill;
end;

procedure TArvidDrive.UseFile;
begin
{  MessageBox('TArvidDrive.UseFile', nil, mfError + mfOKButton);}
end;

{ CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual; }
procedure TArvidDrive.CopyFiles;
var
    CopyDir: String;
    Mask: String;
    CopyOpt: Word;
    CopyMode: Word;
    CopyPrn: Boolean;
    OwnerPtr: PView;
    I,J:  Integer;
    PF: PFileRec;
    S1, S2, S3:  String;
    CmdFileNam: String;
    Dr: String;
    Nm: String;
    Xt: String;
    FC, FC2: TAvtFileCell;
    Nam, Desc: String;
    P:  PView;
    T:  lText;
    CmdFileCreated: Boolean;

   procedure CopyTree(Pos: LongInt);
   begin
     if Pos = 0 then Exit;
     Stream^.Seek(Pos); Stream^.Read(FC, SizeOf(FC));
     if (FC.Flags and avtIsDir) <> 0 then begin
       FC2:=FC;
       FC.ChildOrSize:=0;
     end;
     Nam:=AvtCellName(FC, Stream^);
     Desc:=AvtCellDesc(FC, Stream^);
     if AvtNewFile(@Self,S2+Nam, Desc, (FC.Flags and avtIsDir) <> 0, FC.ChildOrSize,
       FC.Time, FC.StartSector, FC.Attr) = 0 then
     begin
       MessageBox(GetString(dlFBBOver1)+S2+Nam, nil, mfError + mfOKButton);
     end else begin
       if (FC.Flags and avtIsDir) <> 0 then begin
         S1:=S1+Nam+'\';
         S2:=S2+Nam+'\';
         CopyTree(FC2.ChildOrSize);
         Dec(S1[0]); Dec(S2[0]);
         while (S1<>'') and (S1[Length(S1)] <> '\') do begin
           Dec(S1[0]); Dec(S2[0]);
         end;
         Stream^.Seek(Pos); Stream^.Read(FC, SizeOf(FC));
       end;
       Stream^.Seek(Pos); Stream^.Read(FC, SizeOf(FC));
       CopyTree(FC.LeftFileCell);
       Stream^.Seek(Pos); Stream^.Read(FC, SizeOf(FC));
       CopyTree(FC.RightFileCell);
     end;
   end;


{ CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual; }
begin
   CtrlBreakHit:=Off;
   AFiles^.Pack; if AFiles^.Count <= 0 then Exit;
   lFSplit(Name^, Dr, Nm, Xt);
   if FileType = avdTdr then
     S1:= Dr+'TDR:'+Nm+'\' else
     S1:= Dr+'AVT:'+Nm+'\';
   CopyDirName:=S1+CurDir;
   if CopyDirName[Length(CopyDirName)]<>'\' then CopyDirName:=CopyDirName+'\';
   if not CopyDialog(CopyDir, Mask, CopyOpt, CopyMode, CopyPrn,
                     MoveMode, AFiles, 0, Owner, On) then Exit;

   if CopyDir[Length(CopyDir)] <> '\' then CopyDir:=CopyDir+'\';
   if Pos(S1, CopyDir) = 1 then begin
     if FileType = avdTdr then begin
       MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil, mfInformation + mfOKButton);
       Exit;
     end;
     P:=WriteMsg(GetString(dlPleaseStandBy));
     while Length(S1) <> 1 do begin DelFC(S1); DelFC(CopyDir); end;
     S2:=CurDir;
     for I:=0 to AFiles^.Count - 1 do begin
       PF:=AFiles^.at(i);
       Stream^.Seek(Round(PF^.PSize));
       Stream^.Read(FC, SizeOf(FC));
       Desc:=AvtCellDesc(FC, Stream^);
       S1:=MakeNormName('\'+CurDir, GetLFN(PF^.LFN));
       S2:=MakeNormName(CopyDir, MkName(GetLFN(PF^.LFN), Mask));
       if Pos(S1,S2)=1 then begin
         MessageBox(GetString(erIntoItself), nil, mfError + mfOKButton);
       end
       else begin
         if MoveMode then begin {move}
           FC2:=FC; FC2.ChildOrSize:=0;
           Stream^.Seek(Round(PF^.PSize)); Stream^.Write(FC2, SizeOf(FC2));
           AvtDelFile(@Self, S1);
           if AvtNewFile(@Self,S2, Desc, (FC.Flags and avtIsDir) <> 0,
             FC.ChildOrSize, FC.Time, FC.StartSector, FC.Attr) = 0 then
           begin
             AvtNewFile(@Self,S1, Desc, (FC.Flags and avtIsDir) <> 0,
               FC.ChildOrSize, FC.Time, FC.StartSector, FC.Attr);
             MessageBox(GetString(dlFBBOver1)+S2, nil, mfError + mfOKButton);
           end;
         end else begin {copy}
           if (PF^.Attr and Directory <> 0) then begin
             FC2:=FC;
             FC.ChildOrSize:=0;
           end;
           if AvtNewFile(@Self,S2, Desc, (FC.Flags and avtIsDir) <> 0,
             FC.ChildOrSize, FC.Time, FC.StartSector, FC.Attr) = 0 then
           begin
             MessageBox(GetString(dlFBBOver1)+S2, nil, mfError + mfOKButton);
           end else if (PF^.Attr and Directory <> 0) then begin
             S1:=S1+'\';
             S2:=S2+'\';
             CopyTree(FC2.ChildOrSize);
           end;
         end;
       end;
     end;
     P^.Free;
     Stream^.Seek(0); Stream^.Write(AVT, SizeOf(AVT));
     Dispose(Stream,Done);
     Stream:=New(PBufStream, Init(Name^, stOpen, 2048));
     GlobalMessage(evCommand, cmPanelReread, nil);
     Exit;
   end else begin
     S2:=CopyDir;
     J:=0;
     I:=Pos(':',S2);
     while I>0 do begin Inc(J); S2[I]:=' '; I:=Pos(':',S2); end;
     if (J>1) or MoveMode then begin
       MessageBox(GetString(dlArvidNeedDisk), nil, mfError + mfOKButton);
       Exit;
     end;
     if TapeFmt = 0 then begin
       MessageBox(GetString(dlArvidVolumeIsNotTape), nil, mfError + mfOKButton);
       Exit;
     end;
     S2:=UpStrg(CopyDir);
     I:=Pos(':',S2);
     if (I<>2) or (S2[1]<'A') or (S2[1]>'Z') then begin
       MessageBox(GetString(dlArvidNeedDisk), nil, mfError + mfOKButton);
       Exit;
     end;
     CmdFileNam:=MakeNormName(Dr, Nm+'.RD');
     lAssignText(T, CmdFileNam);
     lAppendText(T);
     CmdFileCreated:=False;
     if IOResult <> 0 then begin
       lRewriteText(T);
       WriteLn(T.T, 'IDENT');
       CmdFileCreated:=True;
     end;
     WriteLn(T.T, '');
     P:=WriteMsg(GetString(dlPleaseStandBy));
     for I:=0 to AFiles^.Count - 1 do begin
       PF:=AFiles^.at(i);
       S1:=MakeNormName('\'+CurDir, GetLFN(PF^.LFN));
       S2:=MakeNormName(CopyDir, MkName(Mask, PF^.Name));
       if (PF^.Attr and Directory) = 0 then
         WriteLn(T.T, 'COPY TP:'+SquashesName(S1)+' '+SquashesName(S2)+' /O/R/C/H')
       else begin
         MkDir(S2);
         WriteLn(T.T, 'COPYDIR TP:'+SquashesName(S1)+' '+SquashesName(S2)+' /I/O/R/C/H')
       end;
     end;
     P^.Free;
     if CmdFileCreated then
       MessageBox(GetString(dlArvidCmdFileCreated)+CmdFileNam,
       nil, mfInformation + mfOKButton)
     else
       MessageBox(GetString(dlArvidCmdFileAppended)+CmdFileNam,
       nil, mfInformation + mfOKButton);
     Close(T.T);
     GlobalMessage(evCommand, cmPanelReread, nil);
     Exit;
   end;
end;

{ CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
procedure TArvidDrive.CopyFilesInto;
begin AvtCopyFilesInto(@Self, AFiles, Own, MoveMode); end;

procedure TArvidDrive.EraseFiles;
begin AvtEraseFiles(@Self, AFiles); end;

procedure TArvidDrive.MakeDir;
begin AvtMakeDir(@Self) end;

function TArvidDrive.isUp;
begin
  isUp:=True;
end;

procedure TArvidDrive.ChangeUp;
begin
  if CurDir <> '' then begin
   S := GetName(CurDir);
   lChDir('..');
   Exit
  end;
  if Owner = nil then Exit;
  if Prev = nil then begin
   New(Prev, Init(0, Owner, Flags));
   if Prev = nil then Exit;
  end;
  PFilePanel(Owner)^.Drive := Prev;
  Prev^.lChDir(Prev^.CurDir);
  if (Prev^.DriveType = dtDisk) and
     (PView(Owner)^.GetState(sfSelected+sfActive)) then
      ActivePanel := Owner;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  Prev := nil;
  S := GetName(Name^);
  Kill;
  Dispose(PDrive(@Self), Done);
end;

procedure TArvidDrive.ChangeRoot;
begin
  CurDir := '';
  SeekDirectory;
end;

procedure TArvidDrive.GetFreeSpace;
  var M, R: LongInt;
      L: Array [1..2] of LongInt;
      A: TAvtMediaCell;
begin
  case TapeFmt of
      0:   M:=0;
      2,4: M:=100;
      8:   M:=200;
      16:  M:=200;
      32:  M:=325;
      else M:=325;
  end;

  if FileType = avdTdr
   then L[1] := Max(0, (LongInt(D.TapeLen) - D.RecordLen) div 60)
   else L[1] := Max(0, (LongInt(TapeTotalTime) - TapeRecordedTime) div 60);
  L[2] := (L[1] * 60 * M) div 1024;
  FormatStr(S, GetString(dlArvid_TimeLeft), L);
end;

function TArvidDrive.GetRealName;
var
 Dr: String;
 Nm: String;
 Xt: String;
begin
 lFSplit(Name^, Dr, Nm, Xt);
 GetRealName:=Dr+GetDir;
end;

procedure TArvidDrive.GetDirInfo;
  var L: Array [1..2] of LongInt;
      S,NF,Sz: LongInt;
      S1, S2: String;
      A: TAvtMediaCell;
begin
  B.Title := NewStr( GetString( dlArvid_Title ));
  FreeStr := GetName(Name^); FreeStr[0] := Char(PosChar('.', FreeStr)-1);
  B.Dir := NewStr(FreeStr);

  case TapeFmt of
      0:   begin S:=000; B.VolumeID:=NewStr(GetString(dlArvid_Type)+GetString(dlArvidNoReal)) end;
      2:   begin S:=100; B.VolumeID:=NewStr(GetString(dlArvid_Type)+'~100~ Kb/s, CRC-16') end;
      4:   begin S:=100; B.VolumeID:=NewStr(GetString(dlArvid_Type)+'~100~ Kb/s, CRC-32') end;
      8:   begin S:=200; B.VolumeID:=NewStr(GetString(dlArvid_Type)+'~200~ Kb/s, CRC-32') end;
      16:  begin S:=200; B.VolumeID:=NewStr(GetString(dlArvid_Type)+'~200..300~ Kb/s') end;
      32:  begin S:=325; B.VolumeID:=NewStr(GetString(dlArvid_Type)+'~325..500~ Kb/s') end;
      else begin S:=325; B.VolumeID:=NewStr(GetString(dlArvid_Type)+'~325..500~ Kb/s') end;
  end;

  if FileType = avdTDR then begin
   Sz := D.RecordLen * S;
   Nf := D.FileTableLen div SizeOf(TTdrFileCell);
  end else begin
   Sz:=TotLen div 1024;
   Nf:=TotFiles;
  end;

  if NF = 0 then B.Files := NewStr(GetString(dlDINoFiles))
   else
    begin
     if NF = 1 then S1 := GetString(dlDIFile) else S1 := GetString(dlDIFiles);
     S2 := GetString(dlDIBytes);
     B.Files := NewStr('~'+FStr(NF)+'~ '+S1+GetString(dlDIWith)+'~'+FStr(Sz)+'~K '+S2);
    end;

  if FileType = avdTDR then begin
   L[1] := LongInt(TapeTotalTime) div 60; L[2] := (TapeTotalTime * S) div 1024;
   FormatStr(FreeStr, GetString(dlArvid_TimeUsed), L);
   B.Total := NewStr( FreeStr );
   L[1] := Max(0, (LongInt(TapeTotalTime) - TapeRecordedTime) div 60); L[2] := (L[1] * 60 * S) div 1024;
   FormatStr(FreeStr, GetString(dlArvid_TimeLeft), L);
   B.Free  := NewStr( FreeStr );
  end else begin
   L[1] := LongInt(D.TapeLen) div 60; L[2] := (D.TapeLen * S) div 1024;
   FormatStr(FreeStr, GetString(dlArvid_TimeUsed), L);
   B.Total := NewStr( FreeStr );
   L[1] := Max(0, (LongInt(D.TapeLen) - D.RecordLen) div 60); L[2] := (L[1] * 60 * S) div 1024;
   FormatStr(FreeStr, GetString(dlArvid_TimeLeft), L);
   B.Free  := NewStr( FreeStr );
  end;
end;

procedure TArvidDrive.EditDescription;
var S: String;
    Nam: String;

label 1;

var
   RealAttr, Attrb: Word;

   procedure GetAttr;
   var F: lfile; begin lAssignFile(F, Name^); lGetFAttr(F, Attrb) end;

   procedure SetAttr(A: Word);
   var F: lfile; begin lAssignFile(F, Name^); lSetFAttr(F, A) end;

   procedure Err;
   begin CantWrite(Name^) end;

{ procedure TArvidDrive.EditDescription; }
begin
  if (PF^.DIZ = nil) then Exit;
  S:=CnvString(PF^.Diz^.Diz);
  if BigInputBox(GetString(dlEditDesc),GetString(dl_D_escription), S, 255, hsEditDesc) <> cmOK then Exit;
  Dispose(Stream,Done); Stream:=nil; RealAttr:=$FFFF;
  ClrIO; Abort:=Off;
  GetAttr; if Attrb and ReadOnly <> 0 then
  begin
    SetAttr(Archive);
    if Abort or (IOResult <> 0 ) then begin Err; Goto 1 end;
    RealAttr:=Attrb or Archive;
  end;
  Stream:=New(PBufStream, Init(Name^, stOpen, 2048));
  if Abort or (Stream^.Status <> stOK) then begin Err; Goto 1 end;
  Stream^.Seek(PF^.DIZ^.Line);
  case FileType of
    avdTdr: TdrEditDescription(@Self, S, Nam, PF);
    avdAvt: AvtEditDescription(@Self, S, Nam);
  end;
1: ClrIO; Abort:=False;
  Stream^.Seek(0);
  if FileType = avdAvt then Stream^.Write(AVT, SizeOf(AVT))
  else                      Stream^.Write(D, SizeOf(D));
  Dispose(Stream,Done);
  if RealAttr <> $FFFF then SetAttr(RealAttr);
  Stream:=New(PBufStream, Init(Name^, stOpen, 2048));
  GlobalMessage(evCommand, cmPanelReread, nil);
end;


procedure TArvidDrive.HandleCommand;
  var DT: record
            Mask: String;
            Text: String;
            O: Word;
          end;
      SDesc: Boolean;
      P: PView;
      DateAfter,
      DateBefore,
      SizeGreat,
      SizeLess: LongInt;
      Attr: Byte;
      OOM: Boolean;
      WasTape: Boolean;
      WasDir: Boolean;
      Dr: String;
      LP: LongInt;
      St0: PStream;
      NName: String;
      AA0: TAvtFileCell;
      S0: String;


  label 1;

  procedure Add(P: Pointer; const Name: String);
  begin
    if FindList = nil then New(FindList, Init($100, $100));
    if OOM or (not MemOK) or (MaxAvail < (FindList^.Count+$200)*4) then
    begin
      Dispose(PObject(P),Done);
      OOM:=True;
      Exit;
    end;
    if not WasTape then FindList^.Insert(New(PFindObject, Init(Name)));
    if not WasDir then FindList^.Insert(New(PFindDir, Init(Dr, LP)));
    WasTape:=On; WasDir:=On;
    FindList^.Insert(P);
  end;

  procedure TdrSearchInStream(St: PStream; var D: TTdrHeader; const Name: String);
    var
        DD: TTdrDirCell;
        FF: TTdrFileCell;
        I, J: LongInt;
        Lv0: Boolean;
        LastLv: Integer;
        SS: String[12];
  begin
    Lv0:=Off; LastLv:=0;
    LP:=D.DirTableOfs;
    repeat
      St^.Seek(LP);
      St^.Read(DD, SizeOf(DD));
      if DD.Level = 0 then
         if Lv0 then Break
                else Lv0:=On;
      SS:=DD.Name; Insert('.', SS, 9);
      if DD.Level > LastLv then Dr:=MakeNormName(Dr, MakeFileName(SS))
         else if DD.Level <= LastLv then
                begin
                  repeat
                    while (Dr[Length(Dr)] <> '\') and (Dr <> '') do Dec(Dr[0]);
                    if Dr <> '' then Dec(Dr[0]);
                    Dec(LastLv);
                  until DD.Level > LastLv;
                  if Dr = '' then Dr:='\';
                  Dr:=MakeNormName(Dr, MakeFileName(SS))
                end;
      LastLv:=DD.Level;
      WasDir:=Off;
      if InSpaceFilter(SS, DT.Mask) then
        begin
          if SDesc and (DD.Description <> 0) then
             begin
               St^.Seek(D.DescTableOfs+DD.Description-1);
               St^.Read(FreeStr, 2);
               St^.Read(FreeStr[1], Length(FreeStr));
               UpStr(FreeStr);
             end else FreeStr:='';
          if (not SDesc or (Pos(DT.Text, FreeStr) > 0)) and
                 ((DT.O and 1 =0) or
                  (FF.Time <= DateBefore) and  (FF.Time >= DateAfter) and
                  (FF.Size >= SizeGreat) and (FF.Size <=SizeLess) and
                  ((Attr = 0) or (FF.Attr and Attr <> 0))) then
              begin
                WasDir:=On;
                Add(New(PFindDir, Init(Dr, LP)), Name);
              end;
        end;
      if not OOM then
      begin
        St^.Seek(D.FileTableOfs + LongInt(DD.Files)*SizeOf(TTdrFileCell));
        for I:=1 to DD.NumFiles do
          begin
            St^.Read(FF, SizeOf(FF));
            SS:=FF.Name; Insert('.', SS, 9);
            if InSpaceFilter(SS, DT.Mask) then
              begin
                if SDesc and (FF.Description <> 0) then
                   begin
                     J:=St^.GetPos;
                     St^.Seek(D.DescTableOfs+FF.Description-1);
                     St^.Read(FreeStr, 2);
                     St^.Read(FreeStr[1], Length(FreeStr));
                     St^.Seek(J);
                     UpStr(FreeStr);
                   end else FreeStr:='';
                if (not SDesc or (Pos(DT.Text, FreeStr) > 0)) and
                   ((DT.O and 1 =0) or
                    (FF.Time <= DateBefore) and  (FF.Time >= DateAfter) and
                    (FF.Size >= SizeGreat) and (FF.Size <=SizeLess) and
                    ((Attr = 0) or (FF.Attr and Attr <> 0)))
                 then
                    begin
                      Add(New(PFindFile, Init(MakeFileName(SS), FF.Size, FF.Time)),Name);
                    end;
              end;
          end;
      end;
      Inc(LP, SizeOf(DD));
    until (LP > D.DirTableOfs + D.DirTableLen) or OOM;
    if OOM then Application^.OutOfMemory;
  end;

  procedure AvtSearchInStream(L: LongInt);
  var
    SaveWasDir: Boolean;
    SaveLP: LongInt;
  begin
    if L = 0 then Exit;
    St0^.Seek(L);
    St0^.Read(AA0, SizeOf(AA0));
    if St0^.Status <> stOK then Exit;
    AvtSearchInStream(AA0.LeftFileCell);
    St0^.Seek(L);
    St0^.Read(AA0, SizeOf(AA0));
    if St0^.Status <> stOK then Exit;
    if AA0.Flags and avtIsDir <> 0 then begin
      SaveWasDir:=WasDir; WasDir:=False;
      SaveLP:=LP; LP:=L;
      Dr:=Dr + AvtCellName(AA0, St0^) + '\';
      AvtSearchInStream(AA0.ChildOrSize);
      WasDir:=SaveWasDir; LP:=SaveLP;
      Dec(Dr[0]); while (Dr[Length(Dr)] <> '\') and (Dr <> '') do Dec(Dr[0]);
    end else
    begin
      S0:=AvtCellName(AA0, St0^);
      if InSpaceFilter(S0, DT.Mask) then
      begin
        if SDesc then FreeStr:=UpStrg(AvtCellDesc(AA0, St0^));
        if (not SDesc or (Pos(DT.Text, FreeStr) > 0)) and
           ((DT.O and 1 =0) or
            (AA0.Time <= DateBefore) and  (AA0.Time >= DateAfter) and
            (AA0.ChildOrSize >= SizeGreat) and (AA0.ChildOrSize <=SizeLess))
        then begin
          if WasDir = False then begin
            WasDir:=True;
            Add(New(PFindDir, Init(Dr, LP)), NName);
          end;
          Add(New(PFindFile, Init(S0, AA0.ChildOrSize, AA0.Time)),NName);
        end;
      end;
    end;
    St0^.Seek(L);
    St0^.Read(AA0, SizeOf(AA0));
    if St0^.Status <> stOK then Exit;
    AvtSearchInStream(AA0.RightFileCell);
  end;

  procedure SearchInStream(St: PStream; const Name: String);
    var
        DD: TTdrHeader;
        AA: TAvtHeader;
  begin
    WasTape:=Off; Dr:='\';
    St^.Seek(0);
    St^.Read(AA, SizeOf(AA));
    if AA.Signature = 'AVTP' then begin
      St0:=St;
      NName:=Name;
      AvtSearchInStream(AA.RootDirCell);
    end else
    begin
      St^.Seek(0);
      St^.Read(DD, SizeOf(DD));
      if DD.PosTableLen <> 4656 then Exit;
      if DD.FileTableOfs <> sizeof(TTdrHeader) then Exit;
      if DD.DirTableOfs <> (DD.FileTableOfs+DD.FileTableLen) then Exit;
      if DD.DescTableOfs < (DD.DirTableOfs+DD.DirTableLen) then Exit;
      if DD.PosTableOfs <>
        ((DD.DescTableOfs+DD.DescTableLen+511) div 512 * 512) then Exit;
      if DD.TapeLen < DD.RecordLen then Exit;
      if DD.FileTableLen <> (DD.FileTableLen div sizeof(TTdrFileCell)
        * sizeof(TTdrFileCell)) then Exit;
      TdrSearchInStream(St, DD, Name);
    end;
  end;

  procedure SearchInAllFiles;
    var SR: SearchRec;
        St: PBufStream;
        D: TTdrHeader;
  begin
    ClrIO;
    FindFirst(MakeNormName(GetPath(Name^), '*.TDR'), Archive+Byte(Security)*Hidden+ReadOnly+SysFile, SR);
    while (DOSError = 0) and not Abort and not LowMemory do
      begin
        New(St, Init(MakeNormName(GetPath(Name^), SR.Name), stOpenRead, 2048));
        if St^.Status = stOK then
          begin
            SearchInStream(St, SR.Name);
          end;
        Dispose(St,Done);
        ClrIO;
        FindNext(SR);
      end;
    FindFirst(MakeNormName(GetPath(Name^), '*.AVT'), Archive+Byte(Security)*Hidden+ReadOnly+SysFile, SR);
    while (DOSError = 0) and not Abort and not LowMemory do
      begin
        New(St, Init(MakeNormName(GetPath(Name^), SR.Name), stOpenRead, 2048));
        if St^.Status = stOK then
          begin
            SearchInStream(St, SR.Name);
          end;
        Dispose(St,Done);
        ClrIO;
        FindNext(SR);
      end;
  end;

  procedure ExecuteFindDialog;
    label 1;
    var D: PDialog;
        R: TRect;
        PL: PFindBox;
        P: PView;
        F: PFindObject;

      procedure DoCount(P: PFindObject); {$IFDEF BIT_16}far;{$ENDIF}
      begin
        Inc(R.A.X, Byte(P^.TT = ttFile));
      end;

  begin

     D:=PDialog(LoadResource(dlgArvidFindResults));
     if D = nil then Exit;

     R.Assign(58, 1, 59, 13);
     P:=New(PScrollBar, Init(R));
     D^.Insert(P);

     R.Assign(2,1,58,13);
     New(PL, Init(R, 1, PScrollBar(P)));
     PL^.NewLisT(FindList);
     D^.Insert(PL);

     R.A.X:=0;
     if (FindList <> nil) then FindList^.ForEach(@DoCount);

     FreeStr:=FStr(R.A.X) + GetString(dlFilesFound);
     R.Assign(1, 13, 1+Length(FreeStr), 14);
     P:=New(PStaticText, Init(R, FreeStr));
     P^.Options:=P^.Options or ofCenterX;
     D^.Insert(P);

     PL^.Select;
     R.A.X:=Desktop^.ExecView(D);
     R.A.Y:=PL^.Focused;

     PL^.List:=nil;

     Dispose(D,Done);

     if R.A.X = cmNo then FreeObject(FindList);

     if (R.A.X = cmOK) and (FindList <> nil) and (FindList^.Count > 0) then
       begin
         F:=FindList^.At(R.A.Y);
         if F = nil then Exit;
         FreeStr:='';
         for R.B.X:=R.A.Y downto 0 do
           begin
             F:=FindList^.At(R.B.X);
             if (F^.TT = ttDir) and (FreeStr = '') then
                   FreeStr:=CnvString(F^.Text);
             if (F^.TT = ttTape) then Break;
           end;
         if UpStrg(F^.Text^) <> UpStrg(GetName(Name^)) then
            begin
              FreeObject(Stream);
              CurDir:=FreeStr;
              FreeStr:=MakeNormName(GetPath(Name^), F^.Text^);
              DisposeStr(Name);
              Name:=NewStr(FreeStr);
              Stream:=New(PBufStream, Init(FreeStr, stOpenRead, 2048));
              FreeStr:=CurDir;
              CurDir:='';
              if Stream^.Status <> stOK then
                begin
1:
                  ChangeUp(FreeStr);
                  Exit;
                end;
              Stream^.Read(Self.D, SizeOf(Self.D));
              if Stream^.Status <> stOK then Goto 1;
            end;
         CurDir:=FreeStr;
         if CurDir[1] = '\' then DelFC(CurDir);
         SeekDirectory;
         F:=FindList^.At(R.A.Y);
         if F^.TT = ttFile then FreeStr:=MakeNormName(FreeStr, CnvString(PFindFile(F)^.Name));
         Message(Owner, evCommand, cmFindGotoFile, @FreeStr);
       end;

  end;

begin
   OOM:=False;
   if Command = cmFindFile then
     begin
       DT.Mask:='';
       DT.Text:='';
       DT.O:=0;
       if ExecResource(dlgArvidFileFind, DT) <> cmOK then Goto 1;
       if DT.O and 1 <> 0 then
         begin
           DateAfter:=ParseTime(AdvanceSearchData.After);
           DateBefore:=ParseTime(AdvanceSearchData.Before);
           if DateBefore = 0 then DateBefore:=$7FFFFFFF;
           SizeGreat:=Stoi(AdvanceSearchData.Greater);
           SizeLess:=Stoi(AdvanceSearchData.Less);
           if SizeLess = 0 then SizeLess:=$7FFFFFFF;
           Attr:=0;
           if AdvanceSearchData.Attr and 1 <> 0 then Attr:=Archive;
           if AdvanceSearchData.Attr and 2 <> 0 then Attr:=Attr or SysFile;
           if AdvanceSearchData.Attr and 4 <> 0 then Attr:=Attr or Hidden;
           if AdvanceSearchData.Attr and 8 <> 0 then Attr:=Attr or ReadOnly;
         end;
       if DT.Mask = '' then DT.Mask:=x_x;
       SDesc:=DT.Text <> '';
       UpStr(DT.Text);
       if FindList<>nil then Dispose(FindList,Done); FindList:=nil;
       P:=WriteMsg(GetString(dlPleaseStandBy));
       if DT.O and 2 <> 0 then SearchInAllFiles
                          else SearchInStream(Stream, GetName(Name^));
       P^.Free;
       if FindList = nil then
          begin
            ErrMsg(dlNoFilesFound);
            Exit;
          end;
1:
       if FindList <> nil then ExecuteFindDialog;
     end;
  if FindList <> nil then Dispose(FindList,Done); FindList:=nil;
end;

procedure TArvidDrive.GetDirLength(PF: PFileRec);
  var SaveDir: String;
      LL: TSize;
      P:  PView;
begin
  if (PF^.Attr and $80 <> 0) or (PF^.Attr and Directory = 0) then Exit;
  SaveDir:=CurDir;
  LL:=0;
  CurDir:=MakeNormName(PF^.Owner^, GetLFN(PF^.LFN));
  SeekDirectory;
  P:=WriteMsg(GetString(dlPleaseStandBy));
  case FileType of
    avdTdr: TdrCalcTotal(@Self, CurDirPos, LL);
    avdAvt: AvtCalcTotal(@Self, CurDirPos, LL);
  end;
  P^.Free;
  PF^.Size:=LL;
  PF^.Attr:=PF^.Attr or $80;
  CurDir:=SaveDir;
  SeekDirectory;
end;

END.
