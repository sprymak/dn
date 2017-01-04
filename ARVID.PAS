{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.05/DOS
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

unit Arvid;

interface
uses LFN, LFNCol, advance, advance1, advance2, advance3, Objects, DOS, Drives,
     FilesCol, Views, DiskInfo, ObjType, Tree;

type
   TTdrHeader = record
      FileTableOfs: LongInt;
      DirTableOfs: LongInt;
      PosTableOfs: LongInt;

      FileTableLen: LongInt;
      DirTableLen: LongInt;
      PosTableLen: LongInt;

      TapeFmt: Word;
      TapeID: Word;
      TapeLen: Word;
      RecordLen: Word;
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
      Res: Word;
      Time: LongInt;
      Cluster: Word;
      Size: LongInt;
   end;

   TTdrDirCell = record
      Level: Word;
      Name: Array[1..11] of Char;
      Attr: Byte;
      StartSector: LongInt;
      Description: LongInt;
      Res: Word;
      Time: LongInt;
      Cluster: Word;
      Size: LongInt;

      Files: Word;
      LastFile: Word;
      NumFiles: Word;

   end;

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
       TapeFmt:                 Word;            {16,32}
       TapeLen:                 Word;
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
      Flags:         Word;
      Attr:          Word;
      case Char of
        0: (Name0: Array[1..16] of Char);
        1: (Name1: Array[1..12] of Char; Next1: LongInt);
        2: (Name2: Array[1..12] of Char; DescPtr2: LongInt);
        3: (NamePtr, NonUsed1, NonUsed2, DescPtr3: LongInt);
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
   TAvtTextCell = record
      NextTextCell: LongInt;
      Data: Array[1..36] of Char;
   end;

   TAvdType = (avdTdr, avdAvt);

   PArvidDrive = ^TArvidDrive;
   TArvidDrive = object(TDrive)
      Name: PString;
      Stream: PStream;
      CurFile: LongInt;
      CurDirPos: LongInt;
      CurFileNum: Word;
      CurLevel: Word;
      CurDate: LongInt;
      KillAfterUse: Boolean;
      FileType: TAvdType;
      D: TTdrHeader;
      AVT: TAvtHeader;
      TapeFmt: word;
      TapeTotalTime: word;
      TapeRecordedTime: word;
      TotFiles: LongInt;      {in directory}
      TotLen: LongInt;
      Changed: Boolean;
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
      procedure AvtCheckTree;
      function  AvtDelFile( AName: String): Boolean;
      function  AvtNewFile( AName:        String;
                            ADescription: string;
                            AIsDir:       Boolean;
                            AChildOrSize: TSize;
                            ATime:        LongInt;
                            AStartSector: LongInt;
                            AAttr:        Word ): word;
    private
      FCtemp, FCLtemp, FCRtemp: TAvtFileCell;
      procedure SeekDirectory;
      procedure Kill;
      procedure AvtFreeCell(const L: LongInt);
      function  AvtGetCell: LongInt;
      function  AvtCellRotateLeft(Loc: LongInt): LongInt;
      function  AvtCellRotateRight(Loc: LongInt): LongInt;
  end;


const
  RArvidDrive: TStreamRec = (
    ObjType: otArvidDrive;
    VmtLink: Ofs(TypeOf(TArvidDrive)^);
    Load: @TArvidDrive.Load;
    Store: @TArvidDrive.Store);

  ArvidDrives: PCollection = nil;
  ArvidWithDN: Boolean     = True;

function CopyFilesToArvid(const S: String; Files: PCollection; MoveMode: Boolean; Owner: Pointer): Boolean;

implementation
uses FlPanel, Commands, Startup, DNApp, Drivers, Messages,
     Dialogs, Memory, FileFind, DnUtil, FileCopy;

var
  ArvidDeleteAllFiles: Boolean;

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
var
  FF: TTdrFileCell;
  I: LongInt;
  J: Word;
  A: TAvtMediaCell;
  Attrb: Word;
label 1;

  procedure GetAttr;
  var F: lfile; begin lAssignFile(F, AName); lGetFAttr(F, Attrb) end;

  procedure SetAttr(A: Word);
  var F: lfile; begin lAssignFile(F, AName); lSetFAttr(F, A) end;

begin
  TObject.Init;
  if num<1 then GetParam(1)
           else GetParam(num);
  innum:=num;
  Stream:=New(PBufStream, Init(AName, stOpen, 2048));
  if Stream^.Status <> stOK then begin
    GetAttr; if Attrb and ReadOnly <> 0 then
    begin
      SetAttr(Archive);
      if (DosError <> 0 ) then begin Dispose(Stream, Done); Fail; end;
    end;
    Stream:=New(PBufStream, Init(AName, stOpen, 2048));
    if Stream^.Status <> stOK then begin Dispose(Stream, Done); Fail end;
  end;
  FileType := avdAvt;
  Stream^.Read(AVT, SizeOf(AVT));
  if Stream^.Status <> stOK then begin Dispose(Stream, Done); Fail end;
  if AVT.Signature <> 'AVTP' then begin
    FileType := avdTdr;
    Stream^.Seek(0);
    Stream^.Read(D, SizeOf(D));
    if D.PosTableLen <> 4656 then begin
1:    MessageBox(GetString(erInvalidFileFormat), nil, mfError + mfOKButton);
      Dispose(Stream, Done); Fail;
    end;
    if D.FileTableOfs <> sizeof(TTdrHeader) then Goto 1;
    if D.DirTableOfs <> (D.FileTableOfs+D.FileTableLen) then Goto 1;
    if D.DescTableOfs < (D.DirTableOfs+D.DirTableLen) then Goto 1;
    if D.PosTableOfs <> ((D.DescTableOfs+D.DescTableLen+511) div 512 * 512)
      then Goto 1;
    if D.TapeLen < D.RecordLen then Goto 1;
    if D.FileTableLen <> (D.FileTableLen div sizeof(TTdrFileCell)
      * sizeof(TTdrFileCell)) then Goto 1;
    TapeFmt:=D.TapeFmt;
    TapeTotalTime:=D.TapeLen;
    Stream^.Seek(D.PosTableOfs);
    Stream^.Read(J, SizeOf(J));
    if Stream^.Status <> stOK then Goto 1;
    TapeRecordedTime:=J*8;
  end
  else begin
    Stream^.Seek(AVT.AvtMediaCell);
    Stream^.Read(A, SizeOf(A));
    if Stream^.Status <> stOK then Goto 1;
    TapeFmt:=A.TapeFmt;
    if TapeFmt <> 0 then begin
      TapeTotalTime:=A.TapeLen * 60;
      Stream^.Seek(A.PositionTable);
      if A.PositionTableSize <> 4656 then begin
        Stream^.Read(I, SizeOf(I));
        if Stream^.Status <> stOK then Goto 1;
        TapeRecordedTime:=I*4;
      end else begin
        Stream^.Read(J, SizeOf(J));
        if Stream^.Status <> stOK then Goto 1;
        TapeRecordedTime:=J*8;
      end;
    end else begin
      TapeTotalTime:=0;
      TapeRecordedTime:=0;
    end;
  end;
  Name := NewStr(lFExpand(AName));
  DriveType := dtArvid;
  KillAfterUse := TempFile <> '';
  TempFile := '';
  CurDir := '\';
  SeekDirectory;
  if ArvidDrives = nil then New(ArvidDrives, Init($100, $100));
  ArvidDrives^.Insert(@Self);
end;

function WinToDos(S: String): String;
 var I: word;
begin
  for I:=1 to Length(S) do begin
    if (S[I]>=Char(192)) and (S[I]<=Char(239)) then
      S[I]:=Char(Byte(S[I])-64)
    else
    if (S[I]>=Char(240)) and (S[I]<=Char(255)) then
      S[I]:=Char(Byte(S[I])-16);
  end;
  WinToDos:=S;
end;

function DosToWin(S: String): String;
 var I: word;
begin
  for I:=1 to Length(S) do begin
    if (S[I]>=Char(128)) and (S[I]<=Char(175)) then
      S[I]:=Char(Byte(S[I])+64)
    else
    if (S[I]>=Char(224)) and (S[I]<=Char(239)) then
      S[I]:=Char(Byte(S[I])+16);
  end;
  DosToWin:=S;
end;

function AvtCMP(S1, S2: String): Integer;
var
  I: Integer;
begin
  AvtCMP:=0;
  if S1=S2 then Exit;
  I:=Pos('_', S1);
  while(I>0) do begin S1[I]:=Char($1F); I:=Pos('_', S1); end;
  I:=Pos('_', S2);
  while(I>0) do begin S2[I]:=Char($1F); I:=Pos('_', S2); end;
  if S1<S2 then AvtCMP:=-1 else AvtCMP:=1;
end;

function AvtCellText(Offset: LongInt; var S: TStream): String;
var
  S1: String;
  T:  TAvtTextCell;
begin
  S1:='';
  while Offset <> 0 do begin
    S.Seek(Offset);
    S.Read(T, SizeOf(T));
    if S.Status <> stOK then break;
    S1:=S1+Copy(T.Data,1,36);
    Offset:=T.NextTextCell;
  end;
  AvtCellText:=WinToDos(S1);
end;

function AvtCellName(const C: TAvtFileCell; var AStream: TStream): String;
var
  S: String;
begin
  case C.Flags and avtCellFormat of
    avtCell0: S:=WinToDos(Copy(C.Name0, 1, 16));
    avtCell1: S:=WinToDos(Copy(C.Name1, 1, 12));
    avtCell2: S:=WinToDos(Copy(C.Name2, 1, 12));
    avtCell3: S:=AvtCellText(C.NamePtr, AStream);
  end;
  while (Length(S) <> 0) and (S[Length(S)] = #0) do Dec(S[0]);
  AvtCellName:=S;
end;

function AvtCellDesc(const C: TAvtFileCell; var AStream: TStream): String;
var
  S: String;
begin
  case C.Flags and avtCellFormat of
    avtCell0: S:='';
    avtCell1: S:='';
    avtCell2: S:=AvtCellText(C.DescPtr2, AStream);
    avtCell3: S:=AvtCellText(C.DescPtr3, AStream);
  end;
  while (Length(S) <> 0) and (S[Length(S)] = #0) do Dec(S[0]);
  AvtCellDesc:=S;
end;

function TArvidDrive.AvtGetCell: LongInt;
var
  T: TAvtTextCell;
begin
  if AVT.FreeCell = 0 then begin
    AvtGetCell:=AVT.AfterLastCell;
    Inc(AVT.AfterLastCell,SizeOf(T));
  end else begin
    AvtGetCell:=AVT.FreeCell;
    Stream^.Seek(AVT.FreeCell);
    Stream^.Read(T, SizeOf(T));
    AVT.FreeCell:=T.NextTextCell;
  end;
end;

procedure TArvidDrive.AvtFreeCell(const L: LongInt);
var
  T: TAvtTextCell;
begin
  if L = 0 then Exit;
  T.NextTextCell:=AVT.FreeCell;
  Stream^.Seek(L);
  Stream^.Write(T, SizeOf(T));
  AVT.FreeCell:=L;
end;

function TArvidDrive.AvtCellRotateLeft(Loc: LongInt): LongInt;
var
  Right: LongInt;
begin
  Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
  Stream^.Seek(FCtemp.RightFileCell); Stream^.Read(FCRtemp, SizeOf(FCRtemp));
  if (FCRtemp.Flags and avtBalance) = $00000300 then begin
    Right:=AvtCellRotateRight(FCtemp.RightFileCell);
    Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
    FCtemp.RightFileCell:=Right;
    Stream^.Seek(FCtemp.RightFileCell); Stream^.Read(FCRtemp, SizeOf(FCRtemp));
  end;
  Right:=FCtemp.RightFileCell;
  FCtemp.RightFileCell:=FCRtemp.LeftFileCell;
  FCRtemp.LeftFileCell:=Loc;
  if (FCRtemp.Flags and avtBalance) = 0 then
    FCtemp.Flags:=(FCtemp.Flags and (not avtBalance)) or $00000000 else
    FCtemp.Flags:=(FCtemp.Flags and (not avtBalance)) or $00000300;
  FCRtemp.Flags:=(FCRtemp.Flags and (not avtBalance)) or $00000300;
  Stream^.Seek(Loc); Stream^.Write(FCtemp, SizeOf(FCtemp));
  Stream^.Seek(Right); Stream^.Write(FCRtemp, SizeOf(FCRtemp));
  AvtCellRotateLeft:=Right;
end;

function TArvidDrive.AvtCellRotateRight(Loc: LongInt): LongInt;
var
  Left: LongInt;
begin
  Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
  Stream^.Seek(FCtemp.LeftFileCell); Stream^.Read(FCLtemp, SizeOf(FCLtemp));
  if (FCLtemp.Flags and avtBalance) = $00000100 then begin
    Left:=AvtCellRotateLeft(FCtemp.LeftFileCell);
    Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
    FCtemp.LeftFileCell:=Left;
    Stream^.Seek(FCtemp.LeftFileCell); Stream^.Read(FCLtemp, SizeOf(FCLtemp));
  end;
  Left:=FCtemp.LeftFileCell;
  FCtemp.LeftFileCell:=FCLtemp.RightFileCell;
  FCLtemp.RightFileCell:=Loc;
  if (FCLtemp.Flags and avtBalance) = 0 then
    FCtemp.Flags:=(FCtemp.Flags and (not avtBalance)) or $00000000 else
    FCtemp.Flags:=(FCtemp.Flags and (not avtBalance)) or $00000100;
  FCLtemp.Flags:=(FCLtemp.Flags and (not avtBalance)) or $00000100;
  Stream^.Seek(Loc); Stream^.Write(FCtemp, SizeOf(FCtemp));
  Stream^.Seek(Left); Stream^.Write(FCLtemp, SizeOf(FCLtemp));
  AvtCellRotateRight:=Left;
end;

procedure TArvidDrive.AvtCheckTree;
  function AvtNodeHight(const Loc: LongInt): LongInt;
  var
    LeftHight:  LongInt;
    RightHight: LongInt;
  begin
    AvtNodeHight:=0;
    if Loc <> 0 then begin
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      LeftHight:=AvtNodeHight(FCtemp.LeftFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      RightHight:=AvtNodeHight(FCtemp.RightFileCell);
      if LeftHight >= RightHight then
        AvtNodeHight:=LeftHight + 1 else
        AvtNodeHight:=RightHight + 1;
    end;
  end;

  procedure AvtNodeCheck(const Loc: LongInt);
  var
    LeftHight:  LongInt;
    RightHight: LongInt;
    Balance:    LongInt;
  begin
    if Loc <> 0 then begin
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      if (FCtemp.Flags and avtIsDir) <> 0 then begin
        AvtNodeCheck(FCtemp.ChildOrSize);
        Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      end;
      AvtNodeCheck(FCtemp.LeftFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      AvtNodeCheck(FCtemp.RightFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));

      LeftHight:=AvtNodeHight(FCtemp.LeftFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      RightHight:=AvtNodeHight(FCtemp.RightFileCell);
      Balance:=RightHight-LeftHight;
      if (Balance < -1) or (Balance > 1) then
        MessageBox(GetString(erInvalidFileFormat), nil, mfError + mfOKButton)
      else begin
        Balance:=(Balance shl 8) and $00000300;
        Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
        if Balance <> (FCtemp.Flags and $00000300) then
          MessageBox(GetString(erInvalidFileFormat), nil, mfError + mfOKButton);
      end;
    end;
  end;

begin
  AvtNodeCheck(AVT.RootDirCell);
end;


function TArvidDrive.AvtDelFile( AName: String): Boolean;
var
  SaveCurDir: String;
  CurDir2:    String;
  Dr:         String;
  Nm:         String;
  Xt:         String;
  SN:         String;
  FC:         TAvtFileCell;
  FCL:        TAvtFileCell;
  FCR:        TAvtFileCell;
  NewCurDirPos:  LongInt;
  DelDirAnswer:  Word;

label 1;

  procedure  AvtDelCell(Loc: LongInt);
  var
    FC: TAvtFileCell;
    T:  TAvtTextCell;
    L:  LongInt;
  begin
    Stream^.Seek(Loc);  Stream^.Read(FC, SizeOf(FC));
    case FC.Flags and avtCellFormat of
      avtCell0: L:=0;
      avtCell1: L:=0;
      avtCell2: L:=FC.DescPtr2;
      avtCell3: L:=FC.DescPtr3;
    end;
    while L <> 0 do begin
      Stream^.Seek(L);
      Stream^.Read(T, SizeOf(T));
      AvtFreeCell(L);
      L:=T.NextTextCell;
    end;
    case FC.Flags and avtCellFormat of
      avtCell0: L:=0;
      avtCell1: L:=0;
      avtCell2: L:=0;
      avtCell3: L:=FC.NamePtr;
    end;
    while L <> 0 do begin
      Stream^.Seek(L);
      Stream^.Read(T, SizeOf(T));
      AvtFreeCell(L);
      L:=T.NextTextCell;
    end;
    AvtFreeCell(Loc);
  end;

  procedure AvtFreeTree(const Loc: LongInt);
  begin
    if Loc = 0 then Exit;
    Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
    if FC.Flags and avtIsDir <> 0 then begin
     AvtFreeTree(FC.ChildOrSize);
     Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
    end;
    AvtFreeTree(FC.LeftFileCell);
    Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
    AvtFreeTree(FC.RightFileCell);
    AvtDelCell(Loc);
  end;

  function AvtTreeNodeRemove(const Loc: LongInt): LongInt;
  var
    OldBalance:     LongInt;
    L0, L1, L2:     LongInt;

    function AvtRestoreLeftBalance( Loc: LongInt;
                                    const OldBalance: LongInt): LongInt;
    var
      LeftBalance:  LongInt;
      CurBalance:   LongInt;
      RightZeroBal: Boolean;
    begin
      Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
      CurBalance:=FC.Flags and avtBalance;
      if FC.LeftFileCell <> 0 then begin
        Stream^.Seek(FC.LeftFileCell); Stream^.Read(FCL, SizeOf(FCL));
        LeftBalance:=FCL.Flags and avtBalance;
      end;
      if (FC.LeftFileCell = 0) or
         ((LeftBalance<>OldBalance) and (LeftBalance = 0)) then
      begin
        FC.Flags:= FC.Flags and (not avtBalance);
        case CurBalance of
          $00000000: FC.Flags:=FC.Flags or $00000100;
          $00000300: FC.Flags:=FC.Flags or $00000000;
          $00000100:
            begin
              Stream^.Seek(FC.RightFileCell); Stream^.Read(FCR, SizeOf(FCR));
              RightZeroBal:= (FCR.Flags and avtBalance) = 0;
              Loc:=AvtCellRotateLeft(Loc);
              Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
              Stream^.Seek(FC.LeftFileCell); Stream^.Read(FCL, SizeOf(FCL));
              if RightZeroBal then begin
                FC.Flags := (FC.Flags  and (not avtBalance)) or $00000300;
                FCL.Flags:= (FCL.Flags and (not avtBalance)) or $00000100;
              end else begin
                FC.Flags := (FC.Flags  and (not avtBalance)) or $00000000;
                FCL.Flags:= (FCL.Flags and (not avtBalance)) or $00000000;
              end;
              Stream^.Seek(FC.LeftFileCell); Stream^.Write(FCL, SizeOf(FCL));
            end;
        end;
        Stream^.Seek(Loc); Stream^.Write(FC, SizeOf(FC));
      end;
      AvtRestoreLeftBalance:=Loc;
    end;

    function AvtRestoreRightBalance( Loc: LongInt;
                                     const OldBalance: LongInt): LongInt;
    var
      RightBalance: LongInt;
      CurBalance:   LongInt;
      LeftZeroBal:  Boolean;
    begin
      Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
      CurBalance:=FC.Flags and avtBalance;
      if FC.RightFileCell <> 0 then begin
        Stream^.Seek(FC.RightFileCell); Stream^.Read(FCR, SizeOf(FCR));
        RightBalance:=FCR.Flags and avtBalance;
      end;
      if (FC.RightFileCell = 0) or
         ((RightBalance<>OldBalance) and (RightBalance = 0)) then
      begin
        FC.Flags:= FC.Flags and $FFFFFCFF;
        case CurBalance of
          $00000000: FC.Flags:=FC.Flags or $00000300;
          $00000100: FC.Flags:=FC.Flags or $00000000;
          $00000300:
            begin
              Stream^.Seek(FC.LeftFileCell); Stream^.Read(FCL, SizeOf(FCL));
              LeftZeroBal:= (FCL.Flags and avtBalance) = 0;
              Loc:=AvtCellRotateRight(Loc);
              Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
              Stream^.Seek(FC.RightFileCell); Stream^.Read(FCR, SizeOf(FCR));
              if LeftZeroBal then begin
                FC.Flags := (FC.Flags  and (not avtBalance)) or $00000100;
                FCR.Flags:= (FCR.Flags and (not avtBalance)) or $00000300;
              end else begin
                FC.Flags := (FC.Flags  and (not avtBalance)) or $00000000;
                FCR.Flags:= (FCR.Flags and (not avtBalance)) or $00000000;
              end;
              Stream^.Seek(FC.RightFileCell); Stream^.Write(FCR, SizeOf(FCR));
            end;
        end;
        Stream^.Seek(Loc); Stream^.Write(FC, SizeOf(FC));
      end;
      AvtRestoreRightBalance:=Loc;
    end;

    function AvtTreeRemoveLeftmost(const Loc: LongInt): LongInt;
    var
      OldBalance:   LongInt;
      L1, L2:       LongInt;
    begin
      Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
      if FC.LeftFileCell = 0 then begin
        L0:=Loc; AvtTreeRemoveLeftmost:=FC.RightFileCell; Exit;
      end;
      L1:=FC.LeftFileCell;
      Stream^.Seek(L1); Stream^.Read(FC, SizeOf(FC));
      OldBalance:=FC.Flags and avtBalance;
      L2:=AvtTreeRemoveLeftmost(L1);
      if L1 <> L2 then begin
        Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
        FC.LeftFileCell:=L2;
        Stream^.Seek(Loc); Stream^.Write(FC, SizeOf(FC));
      end;
      AvtTreeRemoveLeftmost:=AvtRestoreLeftBalance(Loc, OldBalance);
    end;

    function AvtTreeRemoveRightmost(const Loc: LongInt): LongInt;
    var
      OldBalance3:  LongInt;
      L1, L2:       LongInt;
    begin
      Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
      if FC.RightFileCell = 0 then begin
        L0:=Loc; AvtTreeRemoveRightmost:=FC.LeftFileCell; Exit;
      end;
      L1:=FC.RightFileCell;
      Stream^.Seek(L1); Stream^.Read(FC, SizeOf(FC));
      OldBalance:=FC.Flags and avtBalance;
      L2:=AvtTreeRemoveRightmost(L1);
      if L1 <> L2 then begin
        Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
        FC.RightFileCell:=L2;
        Stream^.Seek(Loc); Stream^.Write(FC, SizeOf(FC));
      end;
      AvtTreeRemoveRightmost:=AvtRestoreRightBalance(Loc, OldBalance);
    end;

  { function AvtTreeNodeRemove(const Loc: LongInt): LongInt; }
  begin
    AvtTreeNodeRemove:=Loc;
    if Loc = 0 then Exit;
    Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
    SN:=AvtCellName(FC, Stream^);
    if AvtCMP(AName,SN) < 0 then begin
      L1:=FC.LeftFileCell;
      if L1 <> 0 then begin
        Stream^.Seek(L1); Stream^.Read(FC, SizeOf(FC));
        OldBalance:=FC.Flags and avtBalance;
        L2:=AvtTreeNodeRemove(L1);
        if L1 <> L2 then begin
          Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
          FC.LeftFileCell:=L2;
          Stream^.Seek(Loc); Stream^.Write(FC, SizeOf(FC));
        end;
        AvtTreeNodeRemove:=AvtRestoreLeftBalance(Loc, OldBalance);
      end;
    end else
    if AvtCMP(AName,SN) > 0 then begin
      L1:=FC.RightFileCell;
      if L1 <> 0 then begin
        Stream^.Seek(L1); Stream^.Read(FC, SizeOf(FC));
        OldBalance:=FC.Flags and avtBalance;
        L2:=AvtTreeNodeRemove(L1);
        if L1 <> L2 then begin
          Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
          FC.RightFileCell:=L2;
          Stream^.Seek(Loc); Stream^.Write(FC, SizeOf(FC));
        end;
        AvtTreeNodeRemove:=AvtRestoreRightBalance(Loc, OldBalance);
      end;
    end
    else begin
      if (FC.Flags and avtIsDir) <> 0 then begin
        DelDirAnswer:=cmOK;
        if (not ArvidDeleteAllFiles) then
        begin
          Dec(SkyEnabled);
          if Confirms and cfEraseSubDir = 0 then
            DelDirAnswer:=cmYes
          else begin
            DelDirAnswer:=cmCancel;
            if FC.ChildOrSize = 0 then DelDirAnswer:=cmOK else begin
              Stream^.Seek(FC.ChildOrSize); Stream^.Read(FCR, SizeOf(FCR));
              if (FCR.LeftFileCell = 0) and (FCR.RightFileCell = 0) then
                DelDirAnswer:=cmOK
              else begin
                if AvtCellName(FCR, Stream^) = '..' then DelDirAnswer:=cmOK
                else
                  DelDirAnswer:=MessageBox(^C + GetString(dlDirectory) + ' ' +
                      Cut(AName, 40) + GetString(dlEraseDirNotEmpty), nil,
                      mfConfirmation + mfNoButton + mfAllButton+ mf2YesButton +
                      mfCancelButton);
              end;
            end;
          end;
          Inc(SkyEnabled);
          ArvidDeleteAllFiles:= DelDirAnswer = cmOK;
          Abort:= DelDirAnswer = cmCancel;
        end;
        if not (DelDirAnswer in [cmYes, cmOK]) then Exit;
        AvtFreeTree(FC.ChildOrSize);
        Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
      end;
      AvtDelFile:=True;
      if FC.RightFileCell = 0 then AvtTreeNodeRemove:=FC.LeftFileCell
      else case (FC.Flags and avtBalance) of
        0, $00000100:
          begin
             L1:=FC.RightFileCell;
             Stream^.Seek(L1); Stream^.Read(FCR, SizeOf(FCR));
             OldBalance:=FCR.Flags and avtBalance;
             L2:=AvtTreeRemoveLeftmost(L1);
             Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
             FC.RightFileCell:=L2;
             Stream^.Seek(L0); Stream^.Read(FCL, SizeOf(FCL));
             FCL.LeftFileCell:=FC.LeftFileCell;
             FCL.RightFileCell:=FC.RightFileCell;
             FCL.Flags:=(FCL.Flags and (not avtBalance)) or (FC.Flags and $00000300);
             Stream^.Seek(L0); Stream^.Write(FCL, SizeOf(FCL));
             AvtTreeNodeRemove:=AvtRestoreRightBalance(L0, OldBalance);
          end;
        $00000300:
          begin
             L1:=FC.LeftFileCell;
             Stream^.Seek(L1); Stream^.Read(FCL, SizeOf(FCL));
             OldBalance:=FCL.Flags and avtBalance;
             L2:=AvtTreeRemoveRightmost(L1);
             Stream^.Seek(Loc); Stream^.Read(FC, SizeOf(FC));
             FC.LeftFileCell:=L2;
             Stream^.Seek(L0); Stream^.Read(FCR, SizeOf(FCR));
             FCR.LeftFileCell:=FC.LeftFileCell;
             FCR.RightFileCell:=FC.RightFileCell;
             FCR.Flags:=(FCR.Flags and (not avtBalance)) or (FC.Flags and $00000300);
             Stream^.Seek(L0); Stream^.Write(FCR, SizeOf(FCR));
             AvtTreeNodeRemove:=AvtRestoreLeftBalance(L0, OldBalance);
          end;
      end;
      AvtDelCell(Loc);
    end;
  end;

{ function TArvidDrive.AvtDelFile( AName: String): Boolean; }
begin
  AvtDelFile:=False;
  SaveCurDir:=CurDir;
  if (FileType <> avdAvt) or (Length(AName) = 0) then goto 1;
  CurDir2:=CurDir;
  if (CurDir2[Length(CurDir2)] <> '\') then CurDir2:=CurDir2 + '\';
  if AName[1] <> '\' then begin
    AName:=CurDir2 + AName;
  end;
  lFSplit(AName, Dr, Nm, Xt);
  if (Dr[1] = '\') and (Length(Dr)>1) then DelFC(Dr);
  if CurDir2 <> Dr then begin
    CurDir:=Dr; SeekDirectory;
    CurDir2:=CurDir;
    if (CurDir2[Length(CurDir2)] <> '\') then CurDir2:=CurDir2 + '\';
    if CurDir <> Dr then goto 1;
  end;
  AName:=Nm+Xt; if Length(AName) = 0 then goto 1;
  Stream^.Status:=stOK;
  AName:=AName;
  NewCurDirPos:=AvtTreeNodeRemove(CurDirPos);
  if NewCurDirPos <> CurDirPos then begin
    if CurDirCellPos = 0 then begin
      AVT.RootDirCell:=NewCurDirPos;
      Stream^.Seek(0); Stream^.Write(AVT, SizeOf(AVT));
    end else begin
      Stream^.Seek(CurDirCellPos); Stream^.Read(FC, SizeOf(FC));
      FC.ChildOrSize:=NewCurDirPos;
      Stream^.Seek(CurDirCellPos); Stream^.Write(FC, SizeOf(FC));
    end;
    CurDirPos:=NewCurDirPos;
  end;
  1: CurDir:=SaveCurDir; SeekDirectory;
end;

function TArvidDrive.AvtNewFile(
  AName:        String;
  ADescription: String;
  AIsDir:       Boolean;
  AChildOrSize: TSize;
  ATime:        LongInt;
  AStartSector: LongInt;
  AAttr:        Word ): word;

var
  SaveCurDir: String;
  CurDir2:    String;
  Dr:         String;
  Nm:         String;
  Xt:         String;
  SN:         String;
  FC:         TAvtFileCell;
  SS:         String;
  NewCurDirPos:  LongInt;
  NewCell:       LongInt;
  NewCellFailed:      Boolean;
  FailedCellIsDir:    Boolean;
  CreatedCellIsDir:   Boolean;
  NewCellIsDir:       Boolean;
  FirstNameProcessed: Boolean;
  Lv:                 Integer;

  function AvtPutText(S: String): LongInt;
  var
    T:    TAvtTextCell;
    L,L2: LongInt;
    I:    Integer;
  begin
    if S = '' then begin AvtPutText:=0; Exit; end;
    L:=AvtGetCell; AvtPutText:=L;
    while L <> 0 do begin
      for I:=1 to 36 do begin
        if Length(S) <> 0 then begin
          T.Data[I]:=S[1]; DelFC(S);
        end else
          T.Data[I]:=#0;
      end;
      L2:=0;
      if Length(S) <> 0 then L2:=AvtGetCell;
      T.NextTextCell:=L2;
      Stream^.Seek(L); L:=L2;
      Stream^.Write(T, SizeOf(T));
    end;
  end;

  procedure AvtPutName(S: String);
  var
    I:  Integer;
  begin
    if Length(S) < 13 then begin
      FC.Flags:=(FC.Flags and (not avtCellFormat)) or avtCell2;
      for I:=1 to 12 do begin
        if Length(S) <> 0 then begin
          FC.Name2[I]:=S[1]; DelFC(S);
        end else
          FC.Name2[I]:=#0;
      end;
    end
    else begin
      FC.Flags:=(FC.Flags and (not avtCellFormat)) or avtCell3;
      FC.NamePtr:=AvtPutText(S);
    end;
  end;

  procedure AvtPutDesc(S: String);
  begin
    if Length(S) <> 0 then
      FC.DescPtr2:=AvtPutText(DosToWin(S)) else
      FC.DescPtr2:=0;
  end;

  procedure AvtNewCell;
  begin
    NewCell:=AvtGetCell;
    FC.Flags:=0;
    if (AName[1] = '\') or AIsDir then
      AAttr:= AAttr or Directory else
      AAttr:= AAttr and not Directory;
    FC.Attr:=AAttr;
    FC.LeftFileCell:=0; FC.RightFileCell:=0;
    FC.StartSector:=0;
    FC.ChildOrSize:=Round(AChildOrSize);
    FC.Time:=ATime;
    if (AName[1] = '\') or AIsDir then begin
      FC.Flags:=(FC.Flags and (not avtIsDir)) or avtIsDir;
    end;
    if (AName = '') or (AName = '\') then
      AvtPutDesc(ADescription) else
      AvtPutDesc('');
    AvtPutName(SS);
    CreatedCellIsDir:=True;
    if (FC.Flags and avtIsDir) = 0 then begin
      FC.StartSector:=AStartSector;
      CreatedCellIsDir:=False;
    end;
    Stream^.Seek(NewCell); Stream^.Write(FC, SizeOf(FC));
    AvtNewFile:=1;
  end;


  function AvtTreeNodeInsert(APos: LongInt): LongInt;
  var
    OldBalance: LongInt;
    NewPointer: LongInt;
  begin
    if APos = 0 then begin
      AvtNewCell; AvtTreeNodeInsert:=NewCell;
      Exit;
    end;
    Stream^.Seek(APos); Stream^.Read(FCtemp, SizeOf(FCtemp));
    SN:=AvtCellName(FCtemp, Stream^);
    AvtTreeNodeInsert:=APos;
    if SS = SN then begin
      FailedCellIsDir:= (FCtemp.Flags and avtIsDir) <> 0;
      NewCellFailed:=True; Exit;
    end;
    if AvtCMP(SS,SN) < 0 then begin
      if FCtemp.LeftFileCell <> 0 then begin
        Stream^.Seek(FCtemp.LeftFileCell);
        Stream^.Read(FCLtemp, SizeOf(FCLtemp));
        OldBalance:=FCLtemp.Flags and avtBalance;
        NewPointer:=AvtTreeNodeInsert(FCtemp.LeftFileCell);
        Stream^.Seek(APos); Stream^.Read(FCtemp, SizeOf(FCtemp));
        if NewPointer <> FCtemp.LeftFileCell then begin
          FCtemp.LeftFileCell:=NewPointer;
          Stream^.Seek(APos); Stream^.Write(FCtemp, SizeOf(FCtemp));
        end;
        Stream^.Seek(FCtemp.LeftFileCell);
        Stream^.Read(FCLtemp, SizeOf(FCLtemp));
        if ((FCLtemp.Flags and avtBalance) <> OldBalance) and
           ((FCLtemp.Flags and avtBalance) <> 0) then
        begin
          if (FCtemp.Flags and avtBalance) = $00000300 then begin
            APos:=AvtCellRotateRight(APos);
            AvtTreeNodeInsert:=APos;

            Stream^.Seek(APos); Stream^.Read(FCtemp, SizeOf(FCtemp));
            FCtemp.Flags:= (FCtemp.Flags and (not avtBalance)) or $00000000;
            Stream^.Seek(APos); Stream^.Write(FCtemp, SizeOf(FCtemp));

            Stream^.Seek(FCtemp.RightFileCell);
            Stream^.Read(FCRtemp, SizeOf(FCRtemp));
            FCRtemp.Flags:= (FCRtemp.Flags and (not avtBalance)) or $00000000;
            Stream^.Seek(FCtemp.RightFileCell);
            Stream^.Write(FCRtemp, SizeOf(FCRtemp));
          end else begin
            if (FCtemp.Flags and avtBalance) = $00000100 then
              FCtemp.Flags:= (FCtemp.Flags and (not avtBalance)) or $00000000
            else
              FCtemp.Flags:= (FCtemp.Flags and (not avtBalance)) or $00000300;
            Stream^.Seek(APos); Stream^.Write(FCtemp, SizeOf(FCtemp));
          end;
        end;
      end else begin {FCtemp.LeftFileCell = 0}
        AvtNewCell;
        FCtemp.LeftFileCell:=NewCell;
        if (FCtemp.Flags and avtBalance) = $00000100 then
          FCtemp.Flags:= (FCtemp.Flags and (not avtBalance)) or $00000000
        else
          FCtemp.Flags:= (FCtemp.Flags and (not avtBalance)) or $00000300;
        Stream^.Seek(APos); Stream^.Write(FCtemp, SizeOf(FCtemp));
      end;
    end else begin {SS > SN}
      if FCtemp.RightFileCell <> 0 then begin
        Stream^.Seek(FCtemp.RightFileCell);
        Stream^.Read(FCRtemp, SizeOf(FCRtemp));
        OldBalance:=FCRtemp.Flags and avtBalance;
        NewPointer:=AvtTreeNodeInsert(FCtemp.RightFileCell);
        Stream^.Seek(APos); Stream^.Read(FCtemp, SizeOf(FCtemp));
        if NewPointer <> FCtemp.RightFileCell then begin
          FCtemp.RightFileCell:=NewPointer;
          Stream^.Seek(APos); Stream^.Write(FCtemp, SizeOf(FCtemp));
        end;
        Stream^.Seek(FCtemp.RightFileCell);
        Stream^.Read(FCRtemp, SizeOf(FCRtemp));
        if ((FCRtemp.Flags and avtBalance) <> OldBalance) and
           ((FCRtemp.Flags and avtBalance) <> 0) then
        begin
          if (FCtemp.Flags and avtBalance) = $00000100 then begin
            APos:=AvtCellRotateLeft(APos);
            AvtTreeNodeInsert:=APos;

            Stream^.Seek(APos); Stream^.Read(FCtemp, SizeOf(FCtemp));
            FCtemp.Flags:= (FCtemp.Flags and (not avtBalance)) or $00000000;
            Stream^.Seek(APos); Stream^.Write(FCtemp, SizeOf(FCtemp));

            Stream^.Seek(FCtemp.LeftFileCell);
            Stream^.Read(FCLtemp, SizeOf(FCLtemp));
            FCLtemp.Flags:= (FCLtemp.Flags and (not avtBalance)) or $00000000;
            Stream^.Seek(FCtemp.LeftFileCell);
            Stream^.Write(FCLtemp, SizeOf(FCLtemp));
          end else begin
            if (FCtemp.Flags and avtBalance) = $00000300 then
              FCtemp.Flags:= (FCtemp.Flags and (not avtBalance)) or $00000000
            else
              FCtemp.Flags:= (FCtemp.Flags and (not avtBalance)) or $00000100;
            Stream^.Seek(APos); Stream^.Write(FCtemp, SizeOf(FCtemp));
          end;
        end;
      end else begin {FCtemp.RightFileCell = 0}
        AvtNewCell;
        FCtemp.RightFileCell:=NewCell;
        if (FCtemp.Flags and avtBalance) = $00000300 then
          FCtemp.Flags:= (FCtemp.Flags and (not avtBalance)) or $00000000
        else
          FCtemp.Flags:= (FCtemp.Flags and (not avtBalance)) or $00000100;
        Stream^.Seek(APos); Stream^.Write(FCtemp, SizeOf(FCtemp));
      end;
    end;
  end;

  procedure AvtMakeNew;
  var
    NewCurDirPos: LongInt;
    DD:           TAvtFileCell;
    I:            Integer;
  begin
    NewCellFailed:=False;
    Lv:=CurLevel+1; Stream^.Status:=stOK;
    if AName[1] = '\' then DelFC(AName);
    while AName <> '' do begin
      SS:='';
      while (AName[1] <> '\') and (AName <> '') do begin
        AddStr(SS, AName[1]);
        DelFC(AName);
        {if SS[0] = #12 then Break;}
      end;
      {if SS[0] = #12 then }while (AName[1] <> '\') and (AName <> '') do
        DelFC(AName);
      SN:=WinToDos(CurDir2+SS);
      if (SS = '.') or (SS = '..') or (Pos('?',SS)>0) or (Pos('*',SS)>0)
         or (Pos(':',SS)>0)
      then begin
        MessageBox(GetString(dlFCNoCreateDir) + SN, nil, mfError + mfOKButton);
        Exit;
      end;
      I:=Pos('.',SS);
      if I>0 then begin
        if i>9 then
          Delete(SS,9,i-9) else
          Insert(Copy('        ',1,9-i),SS,I);
          SS[9]:=' ';
          if (Pos('.',SS)>0) then begin
             MessageBox(GetString(dlFCNoCreateDir) + SN, nil, mfError + mfOKButton);
             Exit;
          end;
      end else
        Delete(SS,9,Length(SS)-9);
      SS:=MakeFileName(SS);
      if SS = '' then begin
        MessageBox(GetString(dlFCNoCreateDir) + SN, nil, mfError + mfOKButton);
        Exit;
      end;
      NewCurDirPos:=AvtTreeNodeInsert(CurDirPos);
      NewCellIsDir:=(AName[1] = '\') or AIsDir;
      DelFC(AName);
      if NewCellFailed then begin
        if NewCellIsDir <> FailedCellIsDir then begin
          SN:=WinToDos(CurDir2+SS);
          if NewCellIsDir then
            MessageBox(GetString(dlFCNoCreateDir) + SN, nil, mfError + mfOKButton)
          else
            MessageBox(GetString(dlleCantCreate) + SN, nil, mfError + mfOKButton);
          Exit;
        end;
      end;
      if FirstNameProcessed = False then begin
        FirstNameProcessed:=True;
        if NewCellIsDir then
          if (CurDir[Length(CurDir)] <> '\') and (CurDir <> '') then
            CreatedDir:=GetDir+'\'+SS else
            CreatedDir:=GetDir+SS;
      end;
      if NewCurDirPos <> CurDirPos then begin
        if CurDirCellPos = 0 then begin
          AVT.RootDirCell:=NewCurDirPos;
          Stream^.Seek(0); Stream^.Write(AVT, SizeOf(AVT));
        end else begin
          Stream^.Seek(CurDirCellPos); Stream^.Read(FC, SizeOf(FC));
          FC.ChildOrSize:=NewCurDirPos;
          Stream^.Seek(CurDirCellPos); Stream^.Write(FC, SizeOf(FC));
        end;
        CurDirPos:=NewCurDirPos;
      end;
      if CreatedCellIsDir then begin
        if (CurDir[Length(CurDir)] <> '\') and
           (CurDir <> '') then AddStr(CurDir, '\');
        CurDir:=CurDir + SS;
        CurLevel:=Lv; Inc(Lv);
        CurDirCellPos:=NewCell;
        CurDirPos:=0;
      end;
    end;
    CurDate:=ATime;
  end;

{ function TArvidDrive.AvtNewFile; }
begin
  AvtNewFile:=0;
  if (FileType <> avdAvt) or (Length(AName) = 0) then Exit;
  NewCell:=0; AName:=DosToWin(AName);
  CurDir2:=CurDir;
  if (CurDir2[Length(CurDir2)] <> '\') then CurDir2:=CurDir2 + '\';
  SaveCurDir:=CurDir2;
  if AName[1] <> '\' then begin
    AName:=CurDir2 + AName;
  end;
  if (AName <> '') and (AName[1] = '\') then DelFC(AName);
  lFSplit(AName, Dr, Nm, Xt);
  Stream^.Status:=stOK;
  if CurDir2 <> Dr then begin
    CurDir:=Dr; SeekDirectory;
    CurDir2:=CurDir;
    if (CurDir2[Length(CurDir2)] <> '\') then CurDir2:=CurDir2 + '\';
  end;
  if CurDir2 <> '\' then Dr:=Copy(Dr,Length(CurDir2)+1,Length(Dr));
  AName:=Dr+Nm+Xt;

  FirstNameProcessed:=False;
  SN:=Copy(CurDir2,1,Length(SaveCurDir));
  if SN = SaveCurDir then begin
    SN:=Copy(CurDir2,Length(SaveCurDir)+1,Length(CurDir2));
    SS:='';
    while (SN <> '\') and (SN <> '') do begin
      AddStr(SS, SN[1]); DelFC(SN);
    end;
    if SS <> '' then begin
      SN:=CurDir; CurDir:=SaveCurDir;
      if (CurDir[Length(CurDir)] <> '\') and (CurDir <> '') then
            CreatedDir:=GetDir+'\'+SS else
            CreatedDir:=GetDir+SS;
      CurDir:=SN;
      FirstNameProcessed:=True;
    end;
  end;
  if AName <> '' then AvtMakeNew;
  CurDir:=SaveCurDir; SeekDirectory;
end;

procedure TArvidDrive.SeekDirectory;
var
    SavedCurDirPos: LongInt;
    SeekFailed:     Boolean;

    procedure TdrSeekDirectory;
    var
          I,J: LongInt;
          Lv: Integer;
          DD: TTdrDirCell;
          S: String;
          SS: String[12];
    begin
      Stream^.Status:=stOK;
      Stream^.Seek(D.DirTableOfs);
      Stream^.Read(DD, SizeOf(DD));
      CurDirPos:=Stream^.GetPos;
      S:=CurDir; CurLevel:=0; CurDir:=''; Lv:=1;
      if S[1] = '\' then DelFC(S);
      while S <> '' do
        begin
          SS:='';
          while (S[1] <> '\') and (S <> '') do
            begin
              AddStr(SS, S[1]);
              DelFC(S);
              if SS[0] = #12 then Break;
            end;
          if SS[0] = #12 then while (S[1] <> '\') and (S <> '') do
            DelFC(S);
          DelFC(S);
          SS:=Norm12(SS); UpStr(SS);
          Delete(SS, 9, 1);
          repeat
            Stream^.Read(DD, SizeOf(DD));
          until (UpStrg(Copy(DD.Name, 1, 11)) = SS) and (DD.Level = Lv) or (DD.Level < Lv);
          if (DD.Level < Lv) or (DD.Level = 0) then Break;
          Insert('.', SS, 9);
          if (CurDir[Length(CurDir)] <> '\') and
             (CurDir <> '') then AddStr(CurDir, '\');
          CurDir:=CurDir + MakeFileName(SS);
          CurDirPos:=Stream^.GetPos;
          SavedCurDirPos:=CurDirPos;
          CurLevel:=Lv;
          Inc(Lv);
        end;
      CurDate:=DD.Time;
      CurFile:=DD.Files;
      CurFileNum:=DD.NumFiles;
    end;

    procedure AvtSeekDirectory;
    var
          I,J: LongInt;
          Lv: Integer;
          DD: TAvtFileCell;
          S: String;
          SS: String;
          S2: String;
    begin
      S:=CurDir;
      CurDirPos:=AVT.RootDirCell; SavedCurDirPos:=CurDirPos;
      CurDir:=''; CurLevel:=0; Lv:=1;
      CurDirCellPos:=0;
      Stream^.Status:=stOK; SeekFailed:=False;
      if S[1] = '\' then DelFC(S);
      while S <> '' do
        begin
          SS:='';
          while (S[1] <> '\') and (S <> '') do
            begin
              AddStr(SS, S[1]);
              DelFC(S);
            end;
          while (S[1] <> '\') and (S <> '') do DelFC(S);
          DelFC(S);
          while True do begin
            if CurDirPos = 0 then begin SeekFailed:=True; break; end;
            Stream^.Seek(CurDirPos);
            Stream^.Read(DD, SizeOf(DD));
            if Stream^.Status <> stOK then Break;
            S2:=AvtCellName(DD, Stream^);
            if AvtCMP(SS,S2) < 0 then begin
              CurDirPos:=DD.LeftFileCell;
              if CurDirPos = 0 then SeekFailed:=True; end
            else if AvtCMP(SS,S2) > 0 then begin
              CurDirPos:=DD.RightFileCell;
              if CurDirPos = 0 then SeekFailed:=True; end
            else begin
              if (DD.Flags and avtIsDir) = 0 then begin
                SeekFailed:=True; break;
              end;
              CurDirCellPos:=CurDirPos;
              CurDirPos:=DD.ChildOrSize; SavedCurDirPos:=CurDirPos;
              break;
            end;
          end;
          if Stream^.Status <> stOK then Break;
          if SeekFailed then begin CurDirPos:=SavedCurDirPos; break; end;
          if (CurDir[Length(CurDir)] <> '\') and
             (CurDir <> '') then AddStr(CurDir, '\');
          CurDir:=CurDir + SS;
          CurLevel:=Lv;
          Inc(Lv);
        end;
      CurDate:=DD.Time;
    end;

{ procedure TArvidDrive.SeekDirectory; }
begin
  case FileType of
    avdTdr: TdrSeekDirectory;
    avdAvt: AvtSeekDirectory;
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
  F: PFileRec;
  AllFiles, ShowD: Boolean;
  Str: String;
  TAttr: Word;

  procedure TdrGetDirectory;
  var
    FF: TTdrFileCell;
    DD: TTdrDirCell;
    I, J, SeekPos: LongInt;

    procedure AddFile;
    var
      S: Str12;
    begin
      S:=FF.Name;
      UpStr(S);
      Insert('.', S, 9);
      if
      not (ArvidWithDN and Security and (FF.Attr and (Hidden+SysFile) <> 0))
      and (AllFiles or (FF.Attr and Directory <> 0) or InFilter(S, FileMask)) then
          begin
            F:=NewFileRec(MakeFileName(S), MakeFileName(S), FF.Size, FF.Time, FF.Attr, @CurDir);
            if ShowD then
            begin
              New(F^.DIZ);
              F^.DIZ^.Owner:=nil;
              F^.DIZ^.isDisposable:=On;
              F^.DIZ^.Line:=SeekPos;
              if FF.Description <> 0 then
                begin
                   J:=Stream^.GetPos;
                   Stream^.Seek(D.DescTableOfs+FF.Description-1);
                   Stream^.Read(FreeStr, 2);
                   Stream^.Read(FreeStr[1], Length(FreeStr));
                   Stream^.Seek(J);
                   F^.DIZ^.DIZ:=NewStr(FreeStr);
                end else F^.DIZ^.DIZ:=nil;
            end else F^.Diz:=nil;
            if FF.Attr and Directory = 0 then
              begin
                Inc(TotFiles);
                TotLen:=TotLen + FF.Size;
              end;
            F^.PSize:=SeekPos;
            FC^.Insert(F);
          end;
    end;

  begin
    Stream^.Seek(CurDirPos);
    repeat
      SeekPos:=Stream^.GetPos+2;
      Stream^.Read(DD, SizeOf(DD));
      if DD.Level = CurLevel+1 then
        begin
          Move(DD.Name, FF, SizeOf(FF));
          FF.Attr:=FF.Attr or Directory;
          AddFile;
        end;
    until (DD.Level = 0) or (DD.Level <= CurLevel);
    Stream^.Status:=stOK;
    Stream^.Seek(D.FileTableOfs + CurFile*SizeOf(TTdrFileCell));
    FC^.SetLimit(FC^.Count + CurFileNum);
    for I:=1 to CurFileNum do
      begin
        SeekPos:=Stream^.GetPos;
        Stream^.Read(FF, SizeOf(FF));
        AddFile;
      end;
  end;

  procedure AvtGetDirectory(ALocation: LongInt);
  var
    Cell: TAvtFileCell;
    IsDir: Boolean;
  begin
    if ALocation = 0 then Exit;
    Stream^.Status:=stOK;
    Stream^.Seek(ALocation);
    if Stream^.Status <> stOK then Exit;
    Stream^.Read(Cell, SizeOf(Cell));
    if Stream^.Status <> stOK then Exit;
    AvtGetDirectory(Cell.LeftFileCell);
     Str:=AvtCellName(Cell, Stream^);
     IsDir:=Cell.Flags and avtIsDir <> 0;
     if Cell.Flags and avtIsDir <> 0 then
       TAttr:=Cell.Attr or Directory else
       TAttr:=Cell.Attr and not Directory;
     if
     not (ArvidWithDN and Security and (TAttr and (Hidden+SysFile) <> 0))
     and (AllFiles or IsDir or InFilter(Str, FileMask)) then
     begin
       if Cell.Flags and avtIsDir <> 0
        then F:=NewFileRec(Str, Str, 0, Cell.Time, TAttr, @CurDir)
        else F:=NewFileRec(Str, Str, Cell.ChildOrSize, Cell.Time, TAttr, @CurDir);
       if ShowD then
       begin
         New(F^.DIZ);
         F^.DIZ^.Owner:=nil;
         F^.DIZ^.isDisposable:=On;
         F^.DIZ^.Line:=ALocation;
         F^.DIZ^.DIZ:=NewStr(AvtCellDesc(Cell, Stream^));
       end else F^.Diz:=nil;
       if not IsDir then begin
         Inc(TotFiles);
         TotLen:=TotLen + Cell.ChildOrSize;
       end;
       F^.PSize:=ALocation; {non standard use: location in AVT file}
       FC^.Insert(F);
     end;
    AvtGetDirectory(Cell.RightFileCell);
  end;

{ function TArvidDrive.GetDirectory; }
begin
  New(FC, Init($80, $40));
  FC^.SortMode := SortMode;
  GetDirectory := FC;
  ShowD := (Flags and psShowDescript <> 0);
  if ArvidWithDN then GetFreeSpace(FreeSpace);
  TotalInfo:=''; TotFiles:=0; TotLen:=0;
  AllFiles := FileMask = x_x;
  case FileType of
    avdTdr: TdrGetDirectory;
    avdAvt: AvtGetDirectory(CurDirPos);
  end;
  if CurDir = '' then P := Name else P := @CurDir;
  if ArvidWithDN then TotalInfo:=CalcTotalInfo(FC^.Count, TotFiles, TotLen);
  FC^.AtInsert(0, NewFileRec('..','..',0,CurDate,Directory,P));
end;

destructor TArvidDrive.Done;
begin
  ArvidDrives^.Delete(@Self);
  if ArvidDrives^.Count = 0 then begin
    Dispose(ArvidDrives, Done);
    ArvidDrives:=nil;
  end;
  if Stream <> nil then begin
    Dispose(Stream, Done);
  end;
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
 if Xt = '..' then
  begin
   if Dr <> '' then CurDir := Dr;
   if not (CurDir[1] in ['/','\']) then Insert('\', CurDir, 1);
   repeat Dec(CurDir[0]) until (CurDir = '') or (CurDir[Length(CurDir)] in ['\','/']);
   if CurDir <> '' then Dec(CurDir[0]);
  end else CurDir := ADir;
 SeekDirectory;
 While (PosChar(CurDir[Length(CurDir)], ' .\') > 0) do Dec(CurDir[0]);
end;

function TArvidDrive.GetDir;
 var Dr: String;
     Nm,M: String;
     Xt: String;
begin
 lFSplit(Name^, Dr, Nm, Xt);
 if FileType = avdTdr
  then GetDir:= 'TDR:'+Nm+'\'+CurDir
  else GetDir:= 'AVT:'+Nm+'\'+CurDir;
end;

constructor TArvidDrive.Load;
var
  A: TAvtMediaCell;
  I: LongInt;
  J: Word;

label 1;
begin
  TObject.Init;
  inherited Load(S);
  DriveType := dtArvid;
  S.Read(KillAfterUse, 1);
  Name := S.ReadStr;
  Stream:=New(PBufStream, Init(Name^, stOpen, 2048));
  if Stream^.Status <> stOK then
    begin
1:
      Dispose(Stream, Done);
      Done;
      Fail;
    end;
  FileType := avdAvt;
  Stream^.Read(AVT, SizeOf(AVT));
  if Stream^.Status <> stOK then begin Dispose(Stream, Done); Fail end;
  if AVT.Signature <> 'AVTP' then begin
    FileType := avdTdr;
    Stream^.Seek(0);
    Stream^.Read(D, SizeOf(D));
    if D.PosTableLen <> 4656 then Goto 1;
    if D.FileTableOfs <> sizeof(TTdrHeader) then Goto 1;
    if D.DirTableOfs <> (D.FileTableOfs+D.FileTableLen) then Goto 1;
    if D.DescTableOfs < (D.DirTableOfs+D.DirTableLen) then Goto 1;
    if D.PosTableOfs <> ((D.DescTableOfs+D.DescTableLen+511) div 512 * 512)
      then Goto 1;
    if D.TapeLen < D.RecordLen then Goto 1;
    if D.FileTableLen <> (D.FileTableLen div sizeof(TTdrFileCell)
      * sizeof(TTdrFileCell)) then Goto 1;
    TapeFmt:=D.TapeFmt;
    TapeTotalTime:=D.TapeLen;
    Stream^.Seek(D.PosTableOfs);
    Stream^.Read(J, SizeOf(J));
    if Stream^.Status <> stOK then Goto 1;
    TapeRecordedTime:=J*8;
  end else begin
    Stream^.Seek(AVT.AvtMediaCell);
    Stream^.Read(A, SizeOf(A));
    if Stream^.Status <> stOK then Goto 1;
    TapeFmt:=A.TapeFmt;
    if TapeFmt <> 0 then begin
      TapeTotalTime:=A.TapeLen * 60;
      Stream^.Seek(A.PositionTable);
      if A.PositionTableSize <> 4656 then begin
        Stream^.Read(I, SizeOf(I));
        if Stream^.Status <> stOK then Goto 1;
        TapeRecordedTime:=I*4;
      end else begin
        Stream^.Read(J, SizeOf(J));
        if Stream^.Status <> stOK then Goto 1;
        TapeRecordedTime:=J*8;
      end;
    end else begin
      TapeTotalTime:=0;
      TapeRecordedTime:=0;
    end;
  end;
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
  FreeObject(Stream);
  Stream:=New(PBufStream, Init(Name^, stOpen, 2048));
  Stream^.Seek(0);
  if FileType = avdAvt then Stream^.Read(AVT, SizeOf(AVT))
  else                      Stream^.Read(D, SizeOf(D));
  SeekDirectory;
end;

procedure TArvidDrive.KillUse;
begin
  if Prev <> nil then Prev^.KillUse;
  Kill;
end;

procedure TArvidDrive.UseFile;
begin
{ MessageBox('TArvidDrive.UseFile', nil, mfError + mfOKButton);}
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
     if AvtNewFile(S2+Nam, Desc, (FC.Flags and avtIsDir) <> 0, FC.ChildOrSize,
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
     S1:= {Dr+}'TDR:'+{}Dr+{}Nm+'\' else
     S1:= {Dr+}'AVT:'+{}Dr+{}Nm+'\';
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
           AvtDelFile(S1);
           if AvtNewFile(S2, Desc, (FC.Flags and avtIsDir) <> 0,
             FC.ChildOrSize, FC.Time, FC.StartSector, FC.Attr) = 0 then
           begin
             AvtNewFile(S1, Desc, (FC.Flags and avtIsDir) <> 0,
               FC.ChildOrSize, FC.Time, FC.StartSector, FC.Attr);
             MessageBox(GetString(dlFBBOver1)+S2, nil, mfError + mfOKButton);
           end;
         end else begin {copy}
           if (PF^.Attr and Directory <> 0) then begin
             FC2:=FC;
             FC.ChildOrSize:=0;
           end;
           if AvtNewFile(S2, Desc, (FC.Flags and avtIsDir) <> 0,
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
     FreeObject(Stream);
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
     S2:=CopyDir;
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
       S2:=MakeNormName(CopyDir, MkName(PF^.Name, Mask));
       if (PF^.Attr and Directory) = 0 then
         WriteLn(T.T, 'COPY TP:'+SquashesName(S1)+' '+SquashesName(S2)+' /O/R/C/H')
       else begin
         lMkDir(S2);
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

function CopyFilesToArvid(const S: String; Files: PCollection; MoveMode: Boolean; Owner: Pointer): Boolean;
var
   I,J: Integer;
   PF:  PFileRec;
   PD:  PDrive;
   PAD: PArvidDrive;
   S2:   String;
   ToStr:  String;
   OldDir: String;

   function FindDrive(P: PArvidDrive): Boolean; far;
   begin
     FindDrive:= P^.Name^=S2;
   end;

begin
   CopyFilesToArvid:=False;
   if Owner = nil then Exit;
   PD:=PFilePanel(Owner)^.Drive;
   if PD^.DriveType=dtArvid then Exit;
   I:=Pos('\AVT:', S); J:=Pos('\TDR:', S);
   if (I = 0)  and (J = 0) then Exit;
   if (I <> 0) and (J <> 0) then Exit;
   if I<>0 then begin
     S2:=Copy(S,1,I);
     Inc(I,5);
     while (I<=Length(S)) and (S[I]<>'\') do begin
       AddStr(S2, S[I]); Inc(I); end;
     S2:=S2+'.AVT'
   end else begin
     I:=J;
     S2:=Copy(S,1,I);
     Inc(I,5);
     while (I<=Length(S)) and (S[I]<>'\') do begin
       AddStr(S2, S[I]); Inc(I); end;
     S2:=S2+'.TDR'
   end;
   if ArvidDrives = nil then Exit;
   PAD:=ArvidDrives^.FirstThat(@FindDrive);
   if PAD=nil then Exit;
   CopyFilesToArvid:=True;
   ToStr:=Copy(S,I,256); if ToStr='' then ToStr:='\';
   OldDir:=PAD^.CurDir;
   PAD^.CurDir:=ToStr;
   PAD^.SeekDirectory;
   PAD^.CopyFilesInto(Files, Owner, MoveMode);
   PAD^.CurDir:=OldDir;
   PAD^.SeekDirectory;
end;

{ CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
procedure TArvidDrive.CopyFilesInto;
var
  From, S1, S2, S3, S4:   String;
  CmdFileNam: String;
  PD:     PDrive;
  I,J:    Integer;
  PF:     PFileRec;
  T:      lText;
  CmdFileCreated: Boolean;
  P:      PView;
  Dr: String;
  Nm: String;
  Xt: String;
  Desc: String;
  OldDir: String;

  procedure AvtWalkTree;
  var
    I:  Integer;
    PC: PCollection;
  begin
    PD^.lChDir(S2);
    PC:=PD^.GetDirectory(141, 0, x_x, FreeStr, FreeStr);
    for I:=0 to PC^.Count - 1 do begin
      PF:=PC^.at(i);
      S3:=MakeNormName(S1, GetLFN(PF^.LFN));
      S4:=MakeNormName(S2, GetLFN(PF^.LFN));
      Desc:='';
      if (PF^.DIZ <> nil) and (PF^.DIZ^.DIZ <> nil) then Desc:=PF^.DIZ^.DIZ^;
      if (PF^.Attr and Directory) = 0 then
        AvtNewFile( S3, Desc, False, PF^.Size, PackedDate(PF), 0, PF^.Attr)
      else begin
        Nm:=MakeFileName(PF^.Name);
        if (Nm <> '.') and (Nm <> '..') then begin
          if AvtNewFile( S3, Desc, True, 0, PackedDate(PF), 0, PF^.Attr) <> 0 then begin
            S1:=S3+'\';
            S2:=S4+'\';
            AvtWalkTree;
            Dec(S1[0]); Dec(S2[0]);
            while (S1<>'') and (S1[Length(S1)] <> '\') do begin
               Dec(S1[0]); Dec(S2[0]);
            end;
          end;
        end;
      end;
    end;
    Dispose(PC, Done);
  end;

begin
  if Own = nil then Exit;
  PD:=PFilePanel(Own)^.Drive;
  if PD^.DriveType=dtArvid then Exit;
  From:=PD^.GetRealName;
  S2:=From;
  J:=0;
  I:=Pos(':',S2);
  while I>0 do begin Inc(J); S2[I]:=' '; I:=Pos(':',S2); end;
  if (J>1) or MoveMode then begin
    MessageBox(GetString(dlArvidNeedDisk), nil, mfError + mfOKButton);
    Exit;
  end;
  S2:=From;
  I:=Pos(':',S2);
  if (I<>2) or (S2[1]<'A') or (S2[1]>'Z') then begin
    MessageBox(GetString(dlArvidNeedDisk), nil, mfError + mfOKButton);
    Exit;
  end;
  if TapeFmt <> 0 then begin
     lFSplit(Name^, Dr, Nm, Xt);
     CmdFileNam:=MakeNormName(Dr, Nm+'.WR');
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
        S2:=MakeNormName(From, GetLFN(PF^.LFN));
        if (PF^.Attr and Directory) = 0 then
          WriteLn(T.T, 'COPY '+S2+' TP:'+S1+' /O/R/C/H')
        else begin
          WriteLn(T.T, 'COPYDIR '+S2+' TP:'+S1+' /I/O/R/C/H')
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
  end else begin
     if FileType <> avdAvt then begin
       MessageBox(GetString(dlArvidVolumeIsNotTape), nil, mfInformation + mfOKButton);
       MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil, mfInformation + mfOKButton);
       Exit;
     end;
     P:=WriteMsg(GetString(dlPleaseStandBy));
     for I:=0 to AFiles^.Count - 1 do begin
        PF:=AFiles^.at(i);
        S1:=MakeNormName('\'+CurDir, GetLFN(PF^.LFN));
        S2:=MakeNormName(From, GetLFN(PF^.LFN));
        Desc:='';
        if (PF^.DIZ <> nil) and (PF^.DIZ^.DIZ <> nil) then Desc:=PF^.DIZ^.DIZ^;
        if (PF^.Attr and Directory) = 0 then
          AvtNewFile( S1, Desc, False, PF^.Size, PackedDate(PF), 0, PF^.Attr)
        else begin
          AvtNewFile( S1, Desc, True, 0, PackedDate(PF), 0, PF^.Attr);
          S1:=S1+'\';
          S2:=S2+'\';
          OldDir:=PD^.GetDir;
          AvtWalkTree;
          PD^.lChDir(OldDir);
        end;
     end;
     P^.Free;
     Stream^.Seek(0); Stream^.Write(AVT, SizeOf(AVT));
     FreeObject(Stream);
     Stream:=New(PBufStream, Init(Name^, stOpen, 2048));
  end;
  GlobalMessage(evCommand, cmPanelReread, nil);
end;

procedure TArvidDrive.EraseFiles;
var
     PF: PFileRec;
     S:  String;
     I:  Word;
     P:  PView;
     R:  Boolean;
begin
  if FileType <> avdAvt then begin
    MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil, mfInformation + mfOKButton);
    Exit;
  end;
  if AFiles^.Count = 0 then Exit;
  if AFiles^.Count = 1 then
  begin
    PF:=AFiles^.At(0);
    if (PF^.Attr and Directory) <> 0 then
        S:=GetString(dlEraseConfirmDir)+MakeFileName(PF^.Name)+' ?' else
        S:=GetString(dlEraseConfirm1) + MakeFileName(PF^.Name) + ' ?';
  end
  else  S:=GetString(dlEraseConfirms1);
  I:=MessageBox(S,nil,mfConfirmation+mfYesButton+mfNoButton{+mfFastButton});
  if (I <> cmYes) then Exit;
  if AFiles^.Count > 1 then
  begin
    S:=GetString(dlEraseConfirm2)+ItoS(AFiles^.Count)+' '+GetString(dlDIFiles)+ ' ?';
    I:=MessageBox(S,nil,mfConfirmation+mfYesButton+mfNoButton);
    if (I <> cmYes) then Exit;
  end;
  ArvidDeleteAllFiles:=False;
  P:=WriteMsg(GetString(dlPleaseStandBy));
  for I:=0 to AFiles^.Count-1 do begin
    PF:=AFiles^.At(I);
    S:=GetLFN(PF^.LFN);
    R:=AvtDelFile(S);
    if R = False then
      MessageBox(GetString(dlErasingNoFile) + S, nil, mfError + mfOKButton);
  end;
  { AvtCheckTree; }
  P^.Free;
  Stream^.Seek(0); Stream^.Write(AVT, SizeOf(AVT));
  FreeObject(Stream);
  Stream:=New(PBufStream, Init(Name^, stOpen, 2048));
  GlobalMessage(evCommand, cmPanelReread, nil);
end;

procedure TArvidDrive.MakeDir;
var
  S: string;
begin
  if LowMemory then Exit;
  if FileType <> avdAvt then begin
     MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil, mfInformation + mfOKButton);
     Exit;
  end;
  S:=''; if ExecResource(dlgMkDir, S) <> cmOK then Exit;
  DelLeft(S); DelRight(S); if S = '' then Exit;
  if(S[Length(S)] <> '\') then S:=S+'\';
  AvtNewFile(S,'',True, 0, 0, 0, directory);
  { AvtCheckTree; }
  Stream^.Seek(0); Stream^.Write(AVT, SizeOf(AVT));
  FreeObject(Stream);
  Stream:=New(PBufStream, Init(Name^, stOpen, 2048));
  GlobalMessage(evCommand, cmPanelReread, nil);
end;

function TArvidDrive.isUp;
begin
  isUp := True;
end;

procedure TArvidDrive.ChangeUp;
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
  L[1]:=Max(0, (LongInt(TapeTotalTime) - TapeRecordedTime) div 60); L[2]:=(L[1] * 60 * M) div 1024;
  FormatStr(S, GetString(dlArvid_TimeLeft), L);
end;

function TArvidDrive.GetRealName;
begin
 if FileType = avdTdr
  then GetRealName:='TDR:'+Name^
  else GetRealName:='AVT:'+Name^;
end;

procedure TArvidDrive.GetDirInfo;
  var L: Array [1..2] of LongInt;
      S,NF,Sz: LongInt;
      S1, S2: String[30];
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
  Sz:=TotLen div 1024;
  Nf:=TotFiles;
  L[1]:=LongInt(TapeTotalTime) div 60; L[2]:=(TapeTotalTime * S) div 1024;
  FormatStr(FreeStr, GetString(dlArvid_TimeUsed), L);
  B.Total:=NewStr( FreeStr );
  L[1]:=Max(0, (LongInt(TapeTotalTime) - TapeRecordedTime) div 60); L[2]:=(L[1] * 60 * S) div 1024;
  FormatStr(FreeStr, GetString(dlArvid_TimeLeft), L);
  B.Free :=NewStr( FreeStr );
  if NF = 0 then B.Files := NewStr(GetString(dlDINoFiles))
   else
    begin
     if NF = 1 then S1 := GetString(dlDIFile) else S1 := GetString(dlDIFiles);
     S2 := GetString(dlDIBytes);
     B.Files := NewStr('~'+FStr(NF)+'~ '+S1+GetString(dlDIWith)+'~'+FStr(Sz)+'~K '+S2);
    end;
end;

procedure TArvidDrive.EditDescription;
var S: String;
    I, J: LongInt;
    FF: TTdrFileCell;
    Nam: String;

label 1;

   procedure ExpandStream;
     var B: Array [1..512] of Byte;
         I,J: LongInt;
         L: Word;
   begin
     J := Stream^.GetSize;
     repeat
       I := J-512;
       if I < D.PosTableOfs then I := D.PosTableOfs;
       L := J - I;
       Stream^.Seek(I);
       Stream^.Read(B, L);
       Stream^.Seek(I+512);
       Stream^.Write(B, L);
       J := I;
     until J <= D.PosTableOfs;
     FillChar(B, 512, 0);
     Stream^.Seek(I);
     Stream^.Write(B, 512);
     Inc(D.PosTableOfs, 512);
   end;

var
   RealAttr, Attrb: Word;

procedure GetAttr;
var F: lfile; begin lAssignFile(F, Name^); lGetFAttr(F, Attrb) end;

procedure SetAttr(A: Word);
var F: lfile; begin lAssignFile(F, Name^); lSetFAttr(F, A) end;

procedure Err; begin CantWrite(Name^) end;

   procedure TdrEditDescription;
   begin
     Stream^.Read(FF, SizeOf(FF));
     if Length(S) <= Length(CnvString(PF^.DIZ^.DIZ)) then
       begin
         if S = '' then
           begin
             FF.Description:=0;
             Stream^.Seek(PF^.DIZ^.Line);
             Stream^.Write(FF, SizeOf(FF));
           end else
           begin
             Stream^.Seek(D.DescTableOfs+FF.Description-1);
             Stream^.Write(S[0],1);
             I:=0;
             Stream^.Write(I, 1);
             Stream^.Write(S[1], Length(S));
           end;
       end else
       begin
         if D.PosTableOfs - D.DescTableOfs - D.DescTableLen < 2 + Length(S)
            then ExpandStream;
         FF.Description:=D.DescTableLen;
         Stream^.Seek(PF^.DIZ^.Line);
         Stream^.Write(FF, SizeOf(FF));
         Inc(D.DescTableLen, Length(S)+2);
         Stream^.Seek(D.DescTableOfs+FF.Description-1);
         Stream^.Write(S[0],1);
         I:=0;
         Stream^.Write(I, 1);
         Stream^.Write(S[1], Length(S));
       end;
   end;

   procedure AvtEditDescription;
   var
      C: TAvtFileCell;
      T: TAvtTextCell;
      L1, L2, L3, L: LongInt;
      I: word;

      function AvtPutText(S: String): LongInt;
      var
        T:    TAvtTextCell;
        L,L2: LongInt;
        I:    Integer;
      begin
        if S = '' then begin AvtPutText:=0; Exit; end;
        L:=AvtGetCell; AvtPutText:=L;
        while L <> 0 do begin
          for I:=1 to 36 do begin
            if Length(S) <> 0 then begin
              T.Data[I]:=S[1]; DelFC(S);
            end else
              T.Data[I]:=#0;
          end;
          L2:=0;
          if Length(S) <> 0 then L2:=AvtGetCell;
          T.NextTextCell:=L2;
          Stream^.Seek(L); L:=L2;
          Stream^.Write(T, SizeOf(T));
        end;
      end;

      procedure AvtPutName(S: String);
      var
        I:  Integer;
      begin
        if Length(S) < 13 then begin
          C.Flags:=(C.Flags and (not avtCellFormat)) or avtCell2;
          for I:=1 to 12 do begin
            if Length(S) <> 0 then begin
              C.Name2[I]:=S[1]; DelFC(S);
            end else
              C.Name2[I]:=#0;
          end;
        end
        else begin
          C.Flags:=(C.Flags and (not avtCellFormat)) or avtCell3;
          C.NamePtr:=AvtPutText(S);
        end;
      end;

   begin
      FreeStr:=DosToWin(S);
      L1:=Stream^.GetPos;
      Stream^.Read(C, SizeOf(C));
      if Stream^.Status <> stOK then Exit;
      case C.Flags and avtCellFormat of
        avtCell0: L:=0;
        avtCell1: L:=0;
        avtCell2: L:=C.DescPtr2;
        avtCell3: L:=C.DescPtr3;
      end;
      while L <> 0 do begin
        Stream^.Read(T, SizeOf(T));
        if Stream^.Status <> stOK then Exit;
        AvtFreeCell(L);
        L:=T.NextTextCell;
      end;
      if Length(FreeStr) = 0 then L2:=0
      else begin
        L2:=AvtGetCell;
        L:=L2;
        while L <> 0 do begin
          Stream^.Seek(L);
          for I:=1 to 36 do begin
            if Length(FreeStr) <> 0 then begin
              T.Data[I]:=FreeStr[1]; DelFC(FreeStr);
            end else
              T.Data[I]:=#0;
          end;
          L3:=0;
          if Length(FreeStr) <> 0 then L3:=AvtGetCell;
          T.NextTextCell:=L3;
          Stream^.Seek(L); L:=L3;
          Stream^.Write(T, SizeOf(T));
        end;
      end;
      case C.Flags and avtCellFormat of
        avtCell0..avtCell1:
          begin
            Nam:=AvtCellName(C, Stream^);
            AvtPutName(Nam);
            C.DescPtr2:=L2;
          end;
        avtCell2: C.DescPtr2:=L2;
        avtCell3: C.DescPtr3:=L2;
      end;
      Stream^.Seek(L1);
      Stream^.Write(C, SizeOf(C));
   end;

{ procedure TArvidDrive.EditDescription; }
begin
  if (PF^.DIZ = nil) then Exit;
  S := CnvString(PF^.Diz^.Diz);
  if BigInputBox(GetString(dlEditDesc),GetString(dl_D_escription), S, 255, hsEditDesc) <> cmOK then Exit;
  FreeObject(Stream); RealAttr := $FFFF;
  ClrIO; Abort := Off;
  GetAttr; if Attrb and ReadOnly <> 0 then
  begin
    SetAttr(Archive);
    if Abort or (DosError <> 0 ) then begin Err; Goto 1 end;
    RealAttr := Attrb or Archive;
  end;
  Stream := New(PBufStream, Init(Name^, stOpen, 2048));
  if Abort or (Stream^.Status <> stOK) then begin Err; Goto 1 end;
  Stream^.Seek(PF^.DIZ^.Line);
  case FileType of
    avdTdr: TdrEditDescription;
    avdAvt: AvtEditDescription;
  end;
1: ClrIO; Abort := False;
  Stream^.Seek(0);
  if FileType = avdAvt then Stream^.Write(AVT, SizeOf(AVT))
  else                      Stream^.Write(D, SizeOf(D));
  FreeObject(Stream);
  if RealAttr <> $FFFF then SetAttr(RealAttr);
  Stream:=New(PBufStream, Init(Name^, stOpen, 2048));
  GlobalMessage(evCommand, cmPanelReread, nil);
end;


type
     PFindObject = ^TFindObject;
     TFindObject = Object(TObject)
        Text: PString;
        TT: (ttTape, ttDir, ttFile);
        constructor Init(const S: String);
        function GetText: String; virtual;
        destructor Done; virtual;
     end;

     PFindDir = ^TFindDir;
     TFindDir = object(TFindObject)
        Pos: LongInt;
        constructor Init(const S: String; APos: LongInt);
        function GetText: String; virtual;
     end;

     PFindFile = ^TFindFile;
     TFindFile = object(TFindObject)
        Name: String[12];
        Size: LongInt;
        Time: LongInt;
        constructor Init(const S: String; ASize, ATime: LongInt);
        function GetText: String; virtual;
     end;

     PFindBox = ^TFindBox;
     TFindBox = object(TListBox)
        function GetText(Item: Integer; MaxLen: Integer): String; virtual;
        function IsSelected(Item: Integer): Boolean; virtual;
     end;

constructor TFindObject.Init;
begin
  inherited Init;
  Text := NewStr(S);
  TT := ttTape;
end;

function TFindObject.GetText;
begin
  GetText := GetString(dlArvid_TapeDir)+CnvString(Text);
end;

destructor TFindObject.Done;
begin
  DisposeStr(Text);
end;

constructor TFindDir.Init;
begin
   inherited Init(S);
   Pos := APos;
   TT := ttDir;
end;

function TFindDir.GetText;
begin
  GetText := GetString(dlDirectory)+' '+CnvString(Text);
end;

constructor TFindFile.Init;
begin
   inherited Init('');
   Name := S;
   Size := ASize;
   Time := ATime;
   TT := ttFile;
end;

function TFindFile.GetText;
  var DT: DateTime;
      S: String[30];
begin
  UnPackTime(Time, DT);
  MakeDate(DateMode, DT.Day, DT.Month, DT.Year mod 100, DT.Hour, DT.Min, S);
  GetText := '  '+AddSpace(Name, 13)+PredSpace(FStr(Size), 13)+' '+S;
end;


function TFindBox.GetText;
  var P: PFindObject;
begin
  P := List^.At(Item);
  if P <> nil then GetText := P^.GetText
              else GetText := '';
end;

function TFindBox.IsSelected;
  var P: PFindObject;
begin
  P := List^.At(Item);
  IsSelected := (P <> nil) and (P^.TT = ttTape);
end;


const
      FindList: PCollection = nil;


procedure TArvidDrive.HandleCommand;
  var DT: record
            Mask: String[255];
            Text: String[255];
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
      Dr: PathStr;
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
      FreeObject(P);
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
    LP := D.DirTableOfs;
    repeat
      St^.Seek(LP);
      St^.Read(DD, SizeOf(DD));
      if DD.Level = 0 then
         if Lv0 then Break
                else Lv0 := On;
      SS := DD.Name; Insert('.', SS, 9);
      if DD.Level > LastLv then Dr := MakeNormName(Dr, MakeFileName(SS))
         else if DD.Level <= LastLv then
                begin
                  repeat
                    while (not (Dr[Length(Dr)] in ['\','/'])) and (Dr <> '') do Dec(Dr[0]);
                    if Dr <> '' then Dec(Dr[0]);
                    Dec(LastLv);
                  until DD.Level > LastLv;
                  if Dr = '' then Dr := '\';
                  Dr := MakeNormName(Dr, MakeFileName(SS))
                end;
      LastLv := DD.Level;
      WasDir := Off;
      if InSpaceFilter(SS, DT.Mask) then
        begin
          if SDesc and (DD.Description <> 0) then
             begin
               St^.Seek(D.DescTableOfs+DD.Description-1);
               St^.Read(FreeStr, 2);
               St^.Read(FreeStr[1], Length(FreeStr));
             end else FreeStr := '';
          if (not SDesc or (Pos(DT.Text, FreeStr) > 0)) and
                 ((DT.O and 1 =0) or
                  (FF.Time <= DateBefore) and  (FF.Time >= DateAfter) and
                  (FF.Size >= SizeGreat) and (FF.Size <=SizeLess) and
                  ((Attr = 0) or (FF.Attr and Attr <> 0))) then
              begin
                WasDir := On;
                Add(New(PFindDir, Init(Dr, LP)), Name);
              end;
        end;
      if not OOM then
      begin
        St^.Seek(D.FileTableOfs + LongInt(DD.Files)*SizeOf(TTdrFileCell));
        for I := 1 to DD.NumFiles do
          begin
            St^.Read(FF, SizeOf(FF));
            SS := FF.Name; Insert('.', SS, 9);
            if InSpaceFilter(SS, DT.Mask) then
              begin
                if SDesc and (FF.Description <> 0) then
                   begin
                     J := St^.GetPos;
                     St^.Seek(D.DescTableOfs+FF.Description-1);
                     St^.Read(FreeStr, 2);
                     St^.Read(FreeStr[1], Length(FreeStr));
                     St^.Seek(J);
                   end else FreeStr := '';
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
        if SDesc then FreeStr:=AvtCellDesc(AA0, St0^);
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
    var SR: lSearchRec;
        St: PBufStream;
        D: TTdrHeader;
  begin
    ClrIO;
    lFindFirst(MakeNormName(GetPath(Name^), '*.TDR'), Archive+Byte(Security)*Hidden+ReadOnly+SysFile, SR);
    while (DOSError = 0) and not Abort and not LowMemory do
      begin
        New(St, Init(MakeNormName(GetPath(Name^), SR.FullName), stOpenRead, 2048));
        if St^.Status = stOK then
          begin
            SearchInStream(St, SR.FullName);
          end;
        Dispose(St, Done);
        ClrIO;
        lFindNext(SR);
      end;
    lFindClose(SR);
    lFindFirst(MakeNormName(GetPath(Name^), '*.AVT'), Archive+Byte(Security)*Hidden+ReadOnly+SysFile, SR);
    while (DOSError = 0) and not Abort and not LowMemory do
      begin
        New(St, Init(MakeNormName(GetPath(Name^), SR.FullName), stOpenRead, 2048));
        if St^.Status = stOK then
          begin
            SearchInStream(St, SR.FullName);
          end;
        Dispose(St, Done);
        ClrIO;
        lFindNext(SR);
      end;
    lFindClose(SR);
  end;

  procedure ExecuteFindDialog;
    label 1;
    var D: PDialog;
        R: TRect;
        PL: PFindBox;
        P: PView;
        F: PFindObject;

      procedure DoCount(P: PFindObject); far;
      begin
        Inc(R.A.X, Byte(P^.TT = ttFile));
      end;

  begin

     D := PDialog(LoadResource(dlgArvidFindResults));
     if D = nil then Exit;

     R.Assign(58, 1, 59, 13);
     P := New(PScrollBar, Init(R));
     D^.Insert(P);

     R.Assign(2,1,58,13);
     New(PL, Init(R, 1, PScrollBar(P)));
     PL^.NewLisT(FindList);
     D^.Insert(PL);

     R.A.X := 0;
     if (FindList <> nil) then FindList^.ForEach(@DoCount);

     FreeStr := FStr(R.A.X) + GetString(dlFilesFound);
     R.Assign(1, 13, 1+Length(FreeStr), 14);
     P := New(PStaticText, Init(R, FreeStr));
     P^.Options := P^.Options or ofCenterX;
     D^.Insert(P);

     PL^.Select;
     R.A.X := Desktop^.ExecView(D);
     R.A.Y := PL^.Focused;

     PL^.List := nil;

     Dispose(D, Done);

     if R.A.X = cmNo then
       begin
         Dispose(FindList, Done);
         FindList := nil;
       end;

     if (R.A.X = cmOK) and (FindList <> nil) and (FindList^.Count > 0) then
       begin
         F := FindList^.At(R.A.Y);
         if F = nil then Exit;
         FreeStr := '';
         for R.B.X := R.A.Y downto 0 do
           begin
             F := FindList^.At(R.B.X);
             if (F^.TT = ttDir) and (FreeStr = '') then
                   FreeStr := CnvString(F^.Text);
             if (F^.TT = ttTape) then Break;
           end;
         if UpStrg(F^.Text^) <> UpStrg(GetName(Name^)) then
            begin
              Dispose(Stream, Done); Stream := nil;
              CurDir := FreeStr;
              FreeStr := MakeNormName(GetPath(Name^), F^.Text^);
              DisposeStr(Name);
              Name := NewStr(FreeStr);
              Stream := New(PBufStream, Init(FreeStr, stOpenRead, 2048));
              FreeStr := CurDir;
              CurDir := '';
              if Stream^.Status <> stOK then
                begin
1:
                  ChangeUp(FreeStr);
                  Exit;
                end;
              Stream^.Read(Self.D, SizeOf(Self.D));
              if Stream^.Status <> stOK then Goto 1;
            end;
         CurDir := FreeStr;
         if CurDir[1] in ['\','/'] then DelFC(CurDir);
         SeekDirectory;
         F := FindList^.At(R.A.Y);
         if F^.TT = ttFile then FreeStr := MakeNormName(FreeStr, PFindFile(F)^.Name);
         Message(Owner, evCommand, cmFindGotoFile, @FreeStr);
       end;

  end;

begin
   OOM := False;
   if Command = cmFindFile then
     begin
       DT.Mask := '';
       DT.Text := '';
       DT.O := 0;
       if ExecResource(dlgArvidFileFind, DT) <> cmOK then Goto 1;
       if DT.O and 1 <> 0 then
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
       if DT.Mask = '' then DT.Mask := x_x;
       SDesc := DT.Text <> '';
       FreeObject(FindList);
       P := WriteMsg(GetString(dlPleaseStandBy));
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
  if FindList <> nil then FreeObject(FindList);
end;

procedure TArvidDrive.GetDirLength(PF: PFileRec);
  var SaveDir: String;
      LL: TSize;
      P:  PView;

  procedure AvtCalcTotal(const Offset: LongInt);
  var
    AA: TAvtFileCell;
  begin
    if Offset = 0 then Exit;
    Stream^.Seek(Offset);
    Stream^.Read(AA, SizeOf(AA));
    if Stream^.Status <> stOK then Exit;
    AvtCalcTotal(AA.LeftFileCell);
     if AA.Flags and avtIsDir <> 0 then AvtCalcTotal(AA.ChildOrSize) else
     begin LL:=LL+AA.ChildOrSize; end;
    AvtCalcTotal(AA.RightFileCell);
  end;

  procedure CountDirectory(DD, Num: LongInt);
   var FF: TTdrFileCell;
       I: Integer;
  begin
    Stream^.Status := stOK;
    Stream^.Seek(D.FileTableOfs + DD*SizeOf(TTdrFileCell));
    for I := 1 to Num do
      begin
        Stream^.Read(FF, SizeOf(FF));
        LL := LL + FF.Size;
      end;
  end;

  procedure TdrCalcTotal;
  var
    DD: TTdrDirCell;
    SPos: LongInt;
  begin
    SPos:=CurDirPos;
    repeat
      Stream^.Seek(SPos);
      Stream^.Read(DD, SizeOf(DD));
      SPos:=Stream^.GetPos;
      if DD.Level > CurLevel then CountDirectory(DD.Files, DD.NumFiles);
    until (DD.Level = 0) or (DD.Level <= CurLevel);
    CountDirectory(CurFile, CurFileNum);
  end;

begin
  if (PF^.Attr and $8000 <> 0) or (PF^.Attr and Directory = 0) then Exit;
  SaveDir := CurDir;
  LL := 0;
  CurDir := MakeNormName(PF^.Owner^, MakeFileName(PF^.Name));
  SeekDirectory;
  P:=WriteMsg(GetString(dlPleaseStandBy));
  case FileType of
    avdTdr: TdrCalcTotal;
    avdAvt: AvtCalcTotal(CurDirPos);
  end;
  P^.Free;
  if LL<MaxLongInt then
  begin
    PF^.Size := Round(LL);
    PF^.Attr := PF^.Attr or $8000;
  end;
  CurDir := SaveDir;
  SeekDirectory;
end;



end.
