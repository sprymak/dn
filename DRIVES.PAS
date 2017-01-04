{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.10
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

unit Drives;

interface
uses Dos, LFN, LFNCol, Objects, Memory, Views, Drivers, DnIni, FilesCol,
     DiskInfo, FStorage, advance, advance1, advance2, advance3, collect;

const
     dsActive      = 0 ;
     dsInvalid     = 1 ;

type
  PDrive = ^TDrive;
  TDrive = object(TObject)
   Owner: Pointer;
   Prev: PDrive;
   DriveType: ( dtUndefined, dtDisk, dtFind, dtTemp, dtList, dtArc, dtLink,  dtArvid );
   CurDir: String;          {DataCompBoy}
   DIZOwner: String;        {DataCompBoy}
   NoMemory: Boolean;
   Flags: AWord;
   LFNLen: byte; EXTLen: byte;
   DirFLP, FilFLP: Aword;
   Param: byte; innum: byte;
   SizeX: LongInt;
   constructor Init(ADrive: Byte; AOwner: Pointer; num: byte);
   constructor Load(var S: TStream);
   procedure Store(var S: TStream); virtual;
   procedure KillUse; virtual;
   procedure lChDir(ADir: String); virtual;     {DataCompBoy}
   function  GetDir: String; virtual;            {DataCompBoy}
   function  GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String;
                          var FreeSpace, TotalInfo: String ): PCollection; virtual;
   procedure CopyFiles(Files: PCollection; Own: PView; MoveMode: Boolean); virtual;
   procedure CopyFilesInto(Files: PCollection; Own: PView; MoveMode: Boolean); virtual;
   procedure EraseFiles(Files: PCollection); virtual;
   procedure UseFile(P: PFileRec; Command: Word); virtual; {DataCompBoy}
   procedure GetFreeSpace(var S: String); virtual;
   function  Disposable: Boolean; virtual;
   function  GetRealName: String; virtual;
   procedure GetFull(var B; P:PFileRec; C, SC:Word); virtual; {DataCompBoy}
   procedure GetEmpty(var B; SC: Word); virtual;
   function  CalcLength: Integer; virtual;
   procedure RereadDirectory(S: String); virtual; {DataCompBoy}
   procedure MakeTop(var S: String); virtual;
   procedure GetDown(var B; C: Word; P: PFileRec); virtual; {DataCompBoy}
   procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;
   procedure GetDirInfo(var B: TDiskInfoRec); virtual;
   function  GetRealDir: String; virtual;
   procedure MakeDir; virtual;
   function  isUp: Boolean; virtual;
   procedure ChangeUp(var S: String); virtual;
   procedure ChangeRoot; virtual;
   function  GetFullFlags: Word; virtual;
   procedure EditDescription(PF: PFileRec); virtual; {DataCompBoy}
   procedure GetDirLength(PF: PFileRec); virtual; {DataCompBoy}
   procedure GetParam(n: byte); virtual; {DataCompBoy}
   destructor Done; virtual;
  end;

procedure RereadDirectory(Dir: String);
function CalcTotalInfo(A, B: Word; C: TSize): string;
function GetFilesColl(
    var TotalLen: TSize;
    var TFiles: Word;
    PanelFlags: Integer;
    CurDir: PString;
    const FileMask: string;
    Owner: Pointer;
    SortMode: Integer
    ): PFilesCollection;


const
  TempDirs: PSortedCollection = nil;
  TempFiles: PFilesCollection = nil;

implementation
uses Startup, Tree, DNApp, RStrings, FileCopy, Eraser, FlPanel, Commands,
     Dialogs, FileFind, FlPanelX, filediz, CmdLine{!!};

type
        {-DataCompBoy-}
       PDesc = ^TDesc;
       TDesc = record
         {$IFNDEF OS2}
         Name: Str12;
         {$ENDIF}
         LFN: TLFNIndex;
         DIZ: PString;
         Line: LongInt;
       end;
        {-DataCompBoy-}

       PDIZCol = ^TDIZCol;
       TDIZCol = object(TSortedCollection)
         procedure FreeItem(P: Pointer); virtual;
         function Compare(P1, P2: Pointer): Integer; virtual;
       end;

       TFName = record
        {$IFNDEF OS2}
        SFN: Str12;
        {$ENDIF}
        LFN: TLFNIndex;
       end;

        {-DataCompBoy-}
procedure TDIZCol.FreeItem;
begin
 if P <> nil then
  begin
    DisposeStr(PDesc(P)^.DIZ);
    DelLFN(PDesc(P)^.LFN);
    Dispose(PDesc(P));
  end;
end;
        {-DataCompBoy-}

function TDIZCol.Compare;
begin
  {$IFNDEF OS2}
  if PDesc(P1)^.Name = PDesc(P2)^.Name then Compare := 0
   else Compare := CmpLFN(PDesc(P1)^.LFN, PDesc(P2)^.LFN);
  {$ELSE}
  Compare:=CmpLFN(PDesc(P1)^.LFN, PDesc(P2)^.LFN)
  {$ENDIF}
end;

        {-DataCompBoy-}
procedure TDrive.GetFreeSpace;
 var FreeSpc: TSize;
begin
 FreeSpc := Disk_Free(Byte(CurDir[1])-64);
 S := '~'+FStr(FreeSpc)+GetString(dlDIFreeDisk)+Copy(CurDir, 1, 2);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDrive.GetParam;
begin
 Param:=n;
 with ColumnsDefaultsDisk do
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
constructor TDrive.Init;
begin
 TObject.Init;
 Owner := AOwner;
 if num < 1 then GetParam(1)
            else GetParam(num);
 innum:=num;
 ClrIO;
 lGetDir(ADrive, CurDir);
 DriveType := dtDisk;
 NoMemory := False;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
constructor TDrive.Load(var S: TStream);
begin
 TObject.Init;
 Prev := PDrive(S.Get);
 S.Read(CurDir[0], 1);
 S.Read(CurDir[1], Length(CurDir));
 S.Read(Flags, SizeOf(Flags));
 S.Read(LFNLen, 1);
 S.Read(EXTLen, 1);
 S.Read(DirFLP, 2);
 S.Read(FilFLP, 2);
 S.Read(Param,  1);
 DriveType := dtDisk;
 NoMemory := False;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDrive.Store(var S: TStream);
begin
 S.Put(Prev);
 S.Write(CurDir, Length(CurDir)+1);
 S.Write(Flags, SizeOf(Flags));
 S.Write(LFNLen, 1);
 S.Write(EXTLen, 1);
 S.Write(DirFLP, 2);
 S.Write(FilFLP, 2);
 S.Write(Param,  1);
end;
        {-DataCompBoy-}

destructor TDrive.Done;
begin
 if Prev <> nil then Dispose(Prev,Done);
 inherited Done;
end;

function TDrive.Disposable;
begin
 Disposable := On;
end;

        {-DataCompBoy-}
procedure TDrive.ChangeUp;
begin
 S:=GetName(CurDir);
 lChDir(MakeNormName(CurDir,'..'));
 if Abort then Exit;
 lGetDir(0, CurDir);
 if Abort then Exit;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDrive.ChangeRoot;
begin
 lChDir(CurDir[1] + ':\');
 if Abort then Exit;
 lGetDir(0, CurDir);
 if Abort then Exit;
end;
        {-DataCompBoy-}

function TDrive.CalcLength;
 var B: Word;
begin
 if Flags and psShowLongName <> 0 then B:=LFNLen+1 else B:=13;
 B := B +
      (10 + CountryInfo.TimeFmt) * Byte(Flags and psShowSize <> 0) +
      9 * Byte(Flags and psShowDate <> 0) +
      (7 - CountryInfo.TimeFmt) * Byte(Flags and psShowTime <> 0);
 if Flags and psShowDescript <> 0 then CalcLength := MaxViewWidth
                                  else CalcLength := B;
end;

procedure TDrive.MakeTop;
begin
 if Flags and psShowLongName <> 0 then S := CenterStr(GetString(dlTopLFN), LFNLen)+GetString(dlTopSplit)
                                  else S := GetString(dlTopName);
 if Flags and psShowSize     <> 0 then S := S + Copy(GetString(dlTopSize),2-CountryInfo.TimeFmt, 255);
 if Flags and psShowDate     <> 0 then S := S + GetString(dlTopDate);
 if Flags and psShowTime     <> 0 then S := S + Copy(' '+GetString(dlTopTime), 1+CountryInfo.TimeFmt, 255);
 if Flags and psShowDescript <> 0 then S := S + ' ' + GetString(dlPnlDescription) + ' ' + Strg(#32,255);
end;

procedure TDrive.GetFull;
 var S: String;
     S1: String[40];
     X: Byte;
     QW: Boolean;
  NFM: TNameFormatMode;
  OPT: Word;
label NE;
begin
  if Flags and psShowLongName <> 0 then
   begin
    S:=GetLFN(P^.LFN);
    NFM:=nfmNull;
    if EXTLen<>0 then begin
     case DriveType of
      dtLink, dtArc, dtDisk: QW := (Flags and psVariableExtDA<>0);
      dtFind, dtTemp, dtList, dtArvid: QW := (Flags and psVariableExtSTA<>0);
     end;
     if QW and (Length(GetExt(s))=1) then goto NE;
     OPT:=flnHardExtArea or flnAutoHideDot or flnUseCutChar or flnPadRight or flnHighlight or flnHandleTildes
    end else
NE:  OPT:=flnPreferName or flnUseCutChar or flnPadRight or flnHighlight or flnHandleTildes;
    if P^.Selected and (FMSetup.Show and fmsTagChar<>0) then OPT:=OPT+flnSelected;
    if P^.Attr and SysFile<>0 then NFM:=nfmSystem;
    if P^.Attr and Hidden <>0 then NFM:=nfmHidden;
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
{{{{{{{{{{{{}
  if P^.Selected and PView(Owner)^.GetState(sfFocused) then begin
   C:=C shr 8;
   SC:=SC and $00FF + C shl 8;
  end;
  Inc(X);
  TWordArray(B)[X-1]:=SC;
  if (P^.Size > 0) or (P^.Attr and Directory = 0) or (P^.Attr and $8000 <> 0)
     and (P^.Size >= 0)
    then S := FileSizeStr( P^.Size )
    else if P^.Name[1]= '.'
      then S := GetString( dlUpDir )
      else S := GetString( dlSubDir );
  MakeDate(DateMode, P^.Day, P^.Month, P^.Yr, P^.Hour, P^.Minute, S1);
  if Flags and psShowSize <> 0 then
   begin
    MoveStr(TWordArray(B)[X + 9 - Length(S) + CountryInfo.TimeFmt], S, C);
    X := X + 10 + CountryInfo.TimeFmt;
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
    MoveStr(TWordArray(B)[X], Copy(S1,10,6-CountryInfo.TimeFmt), C);
    Inc(X, 7 - CountryInfo.TimeFmt);
    TWordArray(B)[X-1]:=SC;
   end;
  if (Flags and psShowDescript <> 0) and ((DriveType = dtDisk) or
     (DriveType = dtArvid)) then
   begin
    if P^.Diz <> nil then S := Copy(CnvString(P^.DIZ^.DIZ), 1, MaxViewWidth - X - 1) else S := '';
    MoveStr(TWordArray(B)[X], S, C);
   end;
end;

procedure TDrive.GetEmpty;
 var
  X: Byte;
begin
  if Flags and psShowLongName <> 0 then X:=LFNLen+1 else X:=13;
  TWordArray(B)[X-1]:=SC;
  if Flags and psShowSize <> 0 then
   begin
    X := X + 10 + CountryInfo.TimeFmt;
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowDate <> 0 then
   begin
    Inc(X, 9);
    TWordArray(B)[X-1]:=SC;
   end;
  if Flags and psShowTime <> 0 then
   begin
    Inc(X, 7 - CountryInfo.TimeFmt);
    TWordArray(B)[X-1]:=SC;
   end;
end;

procedure TDrive.EraseFiles;
begin
 if Disposable then Eraser.EraseFiles(Files);
end;

procedure TDrive.MakeDir;
begin
 MakeDirectory;
end;

procedure TDrive.CopyFiles;
var B: Boolean;
begin
 if ReflectCopyDirection
  then RevertBar:=Message(DeskTop, evBroadCast, cmIsRightPanel, Own) <> Nil
  else RevertBar:=false;
 if Disposable then FileCopy.CopyFiles(Files, Own, MoveMode, 2*Byte(TypeOf(Self)=TypeOf(TFindDrive)));
end;

procedure TDrive.CopyFilesInto;
var B: Boolean;
begin
 if ReflectCopyDirection
  then RevertBar:=(Message(DeskTop, evBroadCast, cmIsRightPanel, Own) <> Nil)
  else RevertBar:=false;
 FileCopy.CopyFiles(Files, Own, MoveMode, 0);
end;

        {-DataCompBoy-}
procedure TDrive.lChDir;
begin
 ClrIO;
 if (Length(ADir)>3) and (ADir[Length(ADir)] in ['\','/']) then dec(ADir[0]);
 LFN.lChDir(ADir);
 if Abort then Exit;
 LFN.lGetDir(0, ADir);
 if Abort then Exit;
 CurDir := ADir;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TDrive.GetDir;
begin
 GetDir := {$IFNDEF OS2}lfGetLongFileName{$ENDIF}(CurDir);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDrive.UseFile;
 var S: String;
begin
 if P^.Owner<>nil then S := MakeNormName(P^.Owner^, GetLFN(P^.LFN));
 Message(Application, evCommand, Command, @S);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure ReadFileList(
     const CurDir: String;
     const FName: String;
     var Descriptions: PDIZCol);

var T: PTextReader;
    S: String;
    Nam: String;
    I, J: LongInt; zz: boolean;
    CurLine: LongInt;
    PD: PDesc;
begin
  ClrIO;
  T := New(PTextReader, Init(MakeNormName(CurDir, FName)));
  if (T = nil) or Abort then Exit;
  CurLine := 0;
  While not T^.EOF and (IOResult = 0) do
    begin
      Inc(CurLine);
      S := T^.GetStr;
      if (S <> '') and not (s[1] in [' ',#9]) then
       begin
         zz:=false;
         for i:=2 to length(s) do
          if (s[i] in [' ',#9]) and not zz
           then break
           else if s[i]='"' then zz:=not zz;
         if s[i] in [' ',#9] then dec(i);
         j:=i+1;
         while (s[j] in [' ',#9]) do inc(j);
         if not (S[j] in [' ',#9]) then dec(j);
         Nam:=DelSquashes(Copy(S, 1, i));
         Delete(S, 1, j);

         New(PD);
         {$IFNDEF OS2}
         PD^.Name := LowStrg(Norm12(GetName(lfGetShortFileName(Nam))));
         {$ENDIF}
         PD^.LFN := AddLFN(GetName({$IFNDEF OS2}lfGetLongFileName{$ENDIF}(Nam)));
         PD^.Name[9] := ' ';
         PD^.DIZ := NewStr(S);
         PD^.Line := CurLine;
         if not Descriptions^.Search(PD, I)
          then Descriptions^.AtInsert(I, PD)
          else Descriptions^.FreeItem(PD);
       end;
    end;
  Dispose(T,Done);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure PrepareDIZ(
        const CurDir: string;
        var DIZOwner: string;
        var Descriptions: PDIZCol);
var
  S: String;
  FName: String;
begin
  ClrIO;
  S := GetDizOwner(CurDir, '', On);
  if S <> '' then
     begin
       FName := GetName(S);
       Descriptions := New(PDIZCol, Init($10,$10));
       ReadFileList(CurDir, FName, Descriptions);
       DIZOwner := MakeNormName(CurDir, FName);
     end;
  ClrIO;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TossDescriptions(
    PDizOwner: Pointer;
    Files: PFilesCollection;
    Descriptions: PDIZCol);
var
  I, J: LongInt;
  P: PFileRec;
  FName: TFname;
  PD: PDesc;
begin
 for I := 1 to Files^.Count do
  begin
    P := Files^.At(I-1);
    FName.SFN := P^.Name;
    FName.LFN := P^.LFN;
    if P^.Attr and (Directory+SysFile) <> 0 then LowStr(FName.SFN);
    if Descriptions^.Search(@FName, J) then begin
                                        PD := PDesc(Descriptions^.At(J));
                                        New(P^.DIZ);
                                        P^.DIZ^.DIZ := PD^.DIZ;
                                        P^.DIZ^.Owner := PDIZOwner;
                                        P^.DIZ^.Line := PD^.Line;
                                        P^.DIZ^.isDisposable := On;
                                        PD^.DIZ := nil;
                                       end
                                  else P^.DIZ := nil;
  end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function GetFilesColl;
var
  Files: PFilesCollection;
  SR: lSearchRec;
  NormMask: Boolean;
  P: PFileRec;
  L: TSize;
  q: Longint;
  fm: string;
{$IFDEF CHDIRDEBUG}
  ev: TEvent;
 const NumCal: word = 0;
       States:Array[0..3] of char = '|/-\';
       PanelState: PChar = Ptr($B800, 0);
       StartChar: Char = ' ';
begin
 PanelState:=Ptr(SegB800,0);
 StartChar:=PanelState^;
{$ELSE}
begin
{$ENDIF}
  {$IFDEF CHDIRTIMER}
  if ShiftState and (kbLeftShift+kbRightShift) <> 0 then DDTimer:=Get100s;
  {$ENDIF}
  TFiles:=0;
  Files := New(PFilesCollection, Init($10, $20));
  PFilesCollection(Files)^.Owner := Owner;
  PFilesCollection(Files)^.SortMode := SortMode;

  NormMask := not PackMask(FileMask, fm, LFNDisabled(CurDir^));

  lFindFirst(MakeNormName(CurDir^,fm), AnyFile, SR); {JO}
  While (DosError = 0) and not Abort and (IOResult = 0) and MemOK do
  begin
{$IFDEF CHDIRDEBUG}
   PanelState^:=States[NumCal];
   inc(NumCal);
   if NumCal=4 then NumCal:=0;
{$ENDIF}
   if ({$IFNDEF OS2}(SR.SR.Attr and VolumeID = 0) and {$ENDIF}
       not ((SR.SR.Name[0] = #1) and (SR.SR.Name[1] = '.'))
      )          and
      ((not Security) or
       (SR.SR.Attr and Hidden=0) or
       (SR.SR.Name[1] = '.')
      )          and
      (NormMask or
       (SR.SR.Attr and Directory <> 0) or
       (InFilter(SR.FullName, FileMask))
      )          and
      ((SR.SR.Attr and Directory = 0) or
       (SR.SR.Name[1] <> '.') or
       (CurDir^[0] > #3)
      )          then
    begin
      P := NewFileRec(SR.FullName, {$IFNDEF OS2}SR.SR.Name, {$ENDIF}SR.FullSize, SR.SR.Time, SR.SR.Attr, CurDir);
      if SR.SR.Attr and Directory = 0 then
       begin
        TotalLen := TotalLen + P^.Size;
        Inc(TFiles);
       end else
       begin
         if PanelFlags and fmiDirLen <> 0 then
          begin
            if SR.SR.Name[1] = '.' then P^.Size := GetDirLen(CurDir^)
                                   else P^.Size := GetDirLen(MakeNormName(CurDir^,SR.FullName));
            if Abort then begin PanelFlags := PanelFlags and not fmiDirLen end;
            ClrIO;
          end;
        end;
       with Files^ do AtInsert(Count, P)
    end;
   DosError := 0;
   lFindNext(SR);
  end;
  lFindClose(SR);
  if (Length(CurDir^) > 3) then
   if ((Files^.Count=0) or
      (PFileRec(Files^.At(0))^.Name[1] <> '.')) then
   begin
    if PanelFlags and fmiDirLen <> 0 then
    begin
      L := GetDirLen(CurDir^); if Abort then L := 0;
      ClrIO;
    end else L := 0;
    Files^.AtInsert(0, NewFileRec('..',{$IFNDEF OS2}'..'{$ENDIF},L,0,Directory,CurDir));
   end;
  GetFilesColl := Files;
 if (SortMode <> -1) and (Files^.Count>0) then Files^.Sort;
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

        {-DataCompBoy-}
function TDrive.GetDirectory;
 var Files: PFilesCollection;
     SR: lSearchRec;
     P: PFileRec;
     I,J: Integer;
     AllFiles, ShowD: Boolean;
     FreeSpc, TotalLen: TSize;
     TFiles: Word;
     Descriptions: PDIZCol;
{DataCompBoy
  function Get32(S: String): LongInt;
   var I, J, K: LongInt;
  begin
    J := 0;
    for I := Length(S) downto 1 do
     begin
       if (S[I] >= '0') and (S[I] <= '9') then K := Byte(S[I])-48
                                          else K := Byte(S[I])-55;
       Inc(J, K * LongInt(1 shl (5*(Length(S)-I))));
     end;
     Get32 := J;
  end;
}
begin
 ClrIO;
 TFiles := 0; DOSError := 0; TotalLen := 0;
 DIZOwner := ''; FreeSpace := ''; TotalInfo := '';
 Descriptions := nil;
 Abort := Off;
 ShowD := (Flags and psShowDescript <> 0);

 if ShowD or (FMSetup.Options and fmoAlwaysCopyDesc <> 0) then PrepareDIZ(CurDir, DIZOwner, Descriptions);

 Files := GetFilesColl(TotalLen, TFiles, PanelFlags, @CurDir, FileMask, Owner, SortMode);

 NoMemory := not MemOK;

 TotalInfo := CalcTotalInfo(Files^.Count, TFiles, TotalLen);

 if PanelFlags and fmiFree <> 0 then GetFreeSpace(FreeSpace);

 if Descriptions <> nil then
 begin
   TossDescriptions(@DizOwner, Files, Descriptions);
   Dispose(Descriptions,Done)
 end;
 GetDirectory := Files;
end;
        {-DataCompBoy-}

function TDrive.isUp;
begin
 {if Length(CurDir)>3 then}isUp := False{ else isUp:=true;}
end;

procedure TDrive.RereadDirectory;
begin
 if Prev <> nil then Prev^.RereadDirectory(S);
end;

        {-DataCompBoy-}
procedure TDrive.GetDirInfo;
begin
  ReadDiskInfo(CurDir, B, Flags);
end;
        {-DataCompBoy-}

procedure TDrive.KillUse;
begin
 if Prev <> nil then Prev^.KillUse;
end;

        {-DataCompBoy-}
procedure TDrive.GetDown;
 var FR: TFileRec;
     S,S1 {$IFDEF OS2} ,S2 {$ENDIF}: String;
begin
    if P=nil then exit;
    FR := P^; FR.Name[9] := ' ';
    if (FR.Attr and Directory = 0) or (FR.Attr and $8000 <> 0) and (FR.Size >= 0)
    or (PFilePanel(Owner)^.PanelFlags and fmiDirLen <> 0)
     {or (FR.Size >= 0)}
      then S := FileSizeStr( FR.Size )
      else if FR.Name[1]= '.'
        then S := GetString( dlUpDir )
        else S := GetString( dlSubDir );
    MakeDate(DateMode, FR.Day, FR.Month, FR.Yr, FR.Hour, FR.Minute, S1);
{$IFNDEF OS2}
    MoveStr(B, MakeFileName(FR.Name), C);
{$ELSE}
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

function TDrive.GetRealName;
begin
 GetRealName := GetDir;
end;

        {-DataCompBoy-}
function TDrive.GetRealDir;
 var S: String;
     C: Char;
     D: PDialog;
var MM: record
         case byte of
          1: (L: LongInt; S: String[1]);
          2: (C: Char);
        end;
begin
 if DriveType = dtDisk then
  begin
   C := GetCurDrive;
   if C = CurDir[1] then
    begin
      ClrIO;
      NeedAbort := On;
      lGetDir(0, S); if Abort then S := CurrentDirectory;
      NeedAbort := On; Advance2.lChDir(CurDir);
      repeat
       Abort := Off; NeedAbort := On;
       lGetDir(0, CurDir);
       if Abort then
        begin
         repeat
           MM.L := 0; MM.C := GetCurDrive; MM.S := MM.C;
           D := PDialog(LoadResource(dlgDiskError));
           if D <> nil then
            begin
             D^.SetData(MM);
             Application^.ExecView(D);
             D^.GetData(MM);
             Dispose(D,Done);
            end;
           UpStr(MM.S);
           if ValidDrive(MM.S[1]) then
            begin SetCurDrive(MM.S[1]); Break; end;
         until False;
         Abort := On;
        end;
      until not Abort;
      NeedAbort := Off;
      lGetDir(0,CurDir);
      Advance2.lChDir(S);
    end else
    begin
      Advance2.lChDir(CurDir);
      if not Abort then
       repeat
        Abort := Off; NeedAbort := On;
        lGetDir(0, CurDir);
        if Abort then
         begin
          repeat
            MM.L := 0; MM.C := GetCurDrive; MM.S := MM.C;
            D := PDialog(LoadResource(dlgDiskError));
            if D <> nil then
             begin
              D^.SetData(MM);
              Application^.ExecView(D);
              D^.GetData(MM);
              Dispose(D,Done);
             end;
            UpStr(MM.S);
            if ValidDrive(MM.S[1]) then
             begin SetCurDrive(MM.S[1]); Break; end;
          until False;
          Abort := On;
         end;
       until not Abort;
      SetCurDrive(C);
    end;
   GetRealDir := CurDir;
  end else GetRealDir := GetDir;
  NeedAbort := Off;
end;
        {-DataCompBoy-}

procedure TDrive.HandleCommand;
begin
end;

function TDrive.GetFullFlags;
begin
 GetFullFlags := psShowSize + psShowDate + psShowTime;
end;

procedure TDrive.EditDescription;
begin
  if (DriveType = dtDisk) and (PF^.Name[1] <> '.') then
    SetDescription(PF, DIZOwner);
end;

        {-DataCompBoy-}
procedure TDrive.GetDirLength(PF: PFileRec);
 var S: String;
     I: TSize;
     J: LongInt;
begin
  if (PF^.Attr and $8000 <> 0) or (PF^.Attr and Directory = 0) then Exit;
  S := PF^.Owner^;
  if PF^.Name[1] <> '.' then S := MakeNormName(S, GetLFN(PF^.LFN));
  I := 1;
  PF^.Size := CountDirLen(S, On, I, Integer(J));
  if not Abort then PF^.Attr := PF^.Attr or $8000;
end;
        {-DataCompBoy-}

function CalcTotalInfo;
var
  S: string;
begin
 if A = 0 then S := GetString(dlDINoFiles)
  else begin
        S := GetString(dlTotal);
        if B = 1 then S := S+'1~ '+GetString(dlDIFile)
                 else S := S+ItoS(B)+'~ '+GetString(dlDIFiles);
        S := S + GetString(dlDIWith) + '~';
        if C = 1 then S := S + '1~ '+GetString(dlDIByte)
                 else S := S + FStr(C)+'~ '+GetString(dlDIBytes);
       end;
 CalcTotalInfo := S;
end;

procedure RereadDirectory;
 var Event: TEvent;

 procedure Action(View: PView); {$IFDEF BIT_16}far;{$ENDIF}
 begin
  Event.What := evCommand; Event.Command := cmRereadDir;
  Event.InfoPtr := @Dir;
  View^.HandleEvent(Event);
 end;

begin
 {$IFNDEF OS2}
 Dir:=lfGetLongFileName(Dir);
 {$ENDIF}
 Desktop^.ForEach(@Action);
end;

end.
