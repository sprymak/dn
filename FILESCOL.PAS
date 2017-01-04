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

unit FilesCol;

interface
 uses LFN, LFNCol, {DataCompBoy} Objects, Memory, DOS, Startup,
 DNHelp, advance, advance1, advance2, advance3, Collect, Drivers;

type
    PDIZ = ^TDIZ;
    TDIZ = record
      Owner: PString;
      DIZ: PString;
      Line: LongInt;
      isDisposable: Boolean;
    end;

    PFileRec = ^TFileRec;
    TFileRec = record
     Size: TSize;
     PSize: TSize;
     Owner: PString;
     OwnerDisposible: Boolean;
     Diz: PDIZ;
     Name: Str12;
     LFN: TLFNIndex; {DataCompBoy}
     Yr: Word;
     TType: Byte;
     Attr: Word;
     Second: Byte;
     Selected: Boolean;
     case Boolean of
       True : (FDate: LongInt);
       False: (Minute, Hour, Day, Month: Byte);
    end;

    TMakeListRec = record
     FileName  : String; {DataCompBoy}
     Header    : String;
     HeaderMode: Word;
     Action    : String;
     Footer    : String;
     FooterMode: Word;
     Options   : Word;
    end;

        {-DataCompBoy-}
    PUserParams = ^TUserParams;
    TUserParams = record
      Active, Passive: PFileRec;
      ActiveList, PassiveList: String;
    end;
        {-DataCompBoy-}

    PLineCollection = PTextCollection;
    TLineCollection = TTextCollection;

    PFilesCollection = ^TFilesCollection;
    TFilesCollection = object(TSortedCollection)
     SortMode: Byte;
     Selected: LongInt;
     Owner: Pointer;
{$IFDEF SORTDEBUG}
     PanelState: PChar;
     InSort: boolean;
     constructor Init(ALimit, ADelta: LongInt);
     procedure Sort; virtual;
{$ENDIF}
     constructor Load(var S: TStream);
     procedure Store(var S: TStream); virtual;
     procedure FreeItem(Item: Pointer); virtual;
     function Compare(Key1, Key2: Pointer): Integer; virtual;
    end;

const
      cmlPathNames     = 1;
      cmlAutoDetermine = 2;

      hfmAuto          = 0;
      hfmInsertText    = 1;
      hfmInsertFiles   = 2;

function  SelectDrive(X, Y: Integer; Default: Char; IncludeTemp: Boolean): String;
function  CopyFileRec(FR: PFileRec): PFileRec; {DataCompBoy}
function  CreateFileRec(Name: string): PFileRec;
function  NewFileRec(const LFN, Name: String; Size:TSize; Date: LongInt; Attr: Word; AOwner: PString): PFileRec; {DataCompBoy}
procedure DelFileRec(var FR: PFileRec); {DataCompBoy}
function  LoadFileRec(var s: TStream): PFileRec; {DataCompBoy}
procedure StoreFileRec(var s: TStream; fr: PFileRec); {DataCompBoy}
function  LoadFileRecOwn(var s: TStream; Dirs: PCollection): PFileRec; {DataCompBoy}
procedure StoreFileRecOwn(var s: TStream; fr: PFileRec; Dirs: PCollection); {DataCompBoy}
function  PackedDate(P: PFileRec): LongInt; {DataCompBoy}
function  GetFileType(const S: String; Attr: Byte): Integer;
procedure DosReread(Files: PFilesCollection);

 const Executables: TMaskData = (Filter: 'exe;bat;com');

implementation
uses DNApp, Menus, Views, FlPanel, Drives,
     Commands, Messages,
     {!!}CmdLine
     {$IFDEF MODEM}{$IFDEF LINK},NavyLink{$ENDIF}{$ENDIF}
     {$IFDEF NETINFO}, Novell{$ENDIF}
     {$IFNDEF NONBP}, DiskTool{$ENDIF}
     {$IFDEF VIRTUALPASCAL}, VpSysLow{$ELSE}, DrvTypes{$ENDIF}
     , DnIni

;

const
    pfrPacked   = $80;
    pfrSelect   = $40;

type
    TPackedFileRec = record
      Time: LongInt;
      Attr: Byte;
      NLen: Byte;
    end;

        {-DataCompBoy-}
function  CreateFileRec(Name: string): PFileRec;
 var fr: PFileRec;
     lsr: lSearchRec;
     D: DateTime;
     path: string;
 begin
  new(fr);
  CreateFileRec:=fr;
  fillword(fr^, sizeof(fr^) shr 1, 0);
  Name:=lfGetLongFileName(lFExpand(Name));
  lFindFirst(Name+'*', AnyFile, lsr);
  Path:=GetPath(Name);
  Name:=GetName(Name);
  while (lsr.FullName<>Name) and (DosError=0) do lFindNext(lsr);
  lFindClose(lsr);
  if DosError=0 then
   begin
    fr^.name:=Norm12(lsr.sr.name);
    fr^.LFN:=AddLFN(lsr.FullName);
    fr^.Size:=lsr.FullSize;
    fr^.PSize:=lsr.FullSize;
    fr^.Owner:=NewStr(Path);
    fr^.OwnerDisposible:=true;
    fr^.Diz:=nil;
    UnpackTime(lsr.sr.Time, D);
    fr^.Yr := D.Year;
    fr^.Month := D.Month;
    fr^.Day := D.Day;
    fr^.Hour := D.Hour;
    fr^.Minute := D.Min;
    fr^.Second := D.Sec;
    fr^.Attr:=lsr.sr.attr;
    if CharCount('.', lsr.FullName)=0 then begin
     inc(lsr.FullName[0]);
     lsr.FullName[length(lsr.FullName)]:='.';
     fr^.TType := GetFileType(lsr.FullName, fr^.Attr);
     dec(lsr.FullName[0]);
    end else fr^.TType := GetFileType(lsr.FullName, fr^.Attr);
    if fr^.Attr and Directory <> 0 then UpStr(fr^.Name) else LowStr(fr^.Name);
   end
  else
   begin
    fr^.Owner:=NewStr(Path);
    fr^.OwnerDisposible:=true;
    fr^.Name:=Norm12(GetURZ(GetName(lfGetShortFileName(Name))));
    fr^.LFN:=AddLFN(GetName(Name));
   end;
 end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function CopyFileRec;
 var P: PFileRec;
begin
 New(P);
 P^ := FR^;
 P^.LFN := UseLFN(FR^.LFN);
 P^.OwnerDisposible := False;
 if P^.DIZ <> nil then P^.DIZ^.isDisposable := Off;
 CopyFileRec := P;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure DelFileRec(var FR: PFileRec);
begin
 if FR <> nil then begin
  if (FR^.DIZ <> nil) and FR^.DIZ^.isDisposable then begin
   DisposeStr(FR^.DIZ^.DIZ);
   Dispose(FR^.DIZ);
  end;
  if FR^.OwnerDisposible then DisposeStr(FR^.Owner);
  DelLFN(FR^.LFN);
  Dispose(FR);
  FR:=Nil;
 end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function LoadFileRec(var s: TStream): PFileRec;
var P: PFileRec;
begin
 new(P); fillchar(p^, sizeof(p^), 0);
 with P^ do begin
  s.Read(Size, Sizeof(Size));
  s.Read(PSize, Sizeof(PSize));
  S.Read(Name, SizeOf(Name));
  LFN:=LoadLFN(S);
  S.Read(Yr, SizeOf(Yr));
  S.Read(TType, SizeOf(TType));
  S.Read(Attr, SizeOf(Attr));
  S.Read(Second, SizeOf(Second));
  S.Read(Selected, SizeOf(Selected));
  S.Read(FDate, SizeOf(FDate));
  S.Read(OwnerDisposible, SizeOf(OwnerDisposible));
  if OwnerDisposible then Owner:=S.ReadStr;
 end;
 LoadFileRec:=P;
end;

procedure StoreFileRec(var s: TStream; fr: PFileRec);
begin with fr^ do begin
  s.Write(Size, Sizeof(Size));
  s.Write(PSize, Sizeof(PSize));
  S.Write(Name, SizeOf(Name));
  StoreLFN(S, LFN);
  S.Write(Yr, SizeOf(Yr));
  S.Write(TType, SizeOf(TType));
  S.Write(Attr, SizeOf(Attr));
  S.Write(Second, SizeOf(Second));
  S.Write(Selected, SizeOf(Selected));
  S.Write(FDate, SizeOf(FDate));
  S.Write(OwnerDisposible, SizeOf(OwnerDisposible));
  if OwnerDisposible then S.WriteStr(Owner);
end end;
        {-DataCompBoy-}

function  LoadFileRecOwn(var s: TStream; Dirs: PCollection): PFileRec; {DataCompBoy}
var FR: PFileRec;
    w: longint;
begin
 FR := LoadFileRec(S);
 if FR <> nil then begin
  S.Read(W, SizeOf(W));
  if not FR^.OwnerDisposible then FR^.Owner := Dirs^.At(W);
 end;
 LoadFileRecOwn := FR;
end;

procedure StoreFileRecOwn(var s: TStream; fr: PFileRec; Dirs: PCollection); {DataCompBoy}
var w: longint;
begin
 StoreFileRec(S, fr);
 if not FR^.OwnerDisposible then begin
  W := Dirs^.IndexOf(FR^.Owner);
  S.Write(W, SizeOf(W));
 end;
end;

{$IFDEF SORTDEBUG}
        {-DataCompBoy-}
constructor TFilesCollection.Init(ALimit, ADelta: longint);
begin
 Inherited Init(ALimit, ADelta);
 PanelState:=Ptr(segB800, 0);
 InSort:=false;
end;
        {-DataCompBoy-}
{$ENDIF}

        {-DataCompBoy-}
constructor TFilesCollection.Load;
var
  C, I: LongInt;
begin
  TObject.Init;
  S.Read(Count, Sizeof(Count));
  S.Read(Limit, Sizeof(Limit));
  S.Read(Delta, Sizeof(Delta));
  if (Count > Limit) or (Delta < 0) then Fail;
  C:=Count; I:=Limit;
  Count:=0; Limit:=0;
  SetLimit(I);
  for I:=0 to C-1 do begin
   AtInsert(I, LoadFileRec(S));
   if (S.Status<>stOK) then begin SetLimit(0); Fail; end;
  end;
  S.Read(Selected, SizeOf(Selected));
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TFilesCollection.Store;
 var I, J, Sel: LongInt;
begin
 J := 0;
 for I := 1 to Count do
  if (I - 1 = Selected) or (PFileRec(At(I-1))^.Selected) then
   begin if I - 1 = Selected then Sel := J; Inc(J) end;
 I := Count; Count := J;
 S.Write(Count, Sizeof(Count));
 S.Write(Limit, Sizeof(Limit));
 S.Write(Delta, Sizeof(Delta));
 Count := I;
 for I := 1 to Count do
  if (I - 1 = Selected) or (PFileRec(At(I-1))^.Selected) then
   StoreFileRec(S, At(I-1));
 S.Write(Sel, SizeOf(Sel));
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TFilesCollection.FreeItem;
var P: PFileRec absolute Item; begin DelFileRec(P); end;
        {-DataCompBoy-}

{$IFDEF SORTDEBUG}
        {-DataCompBoy-}
procedure TFilesCollection.Sort;
var C: char;
begin
 PanelState:=Ptr(segB800, 0);
 if PanelState<>nil then C:=PanelState^;
 InSort:=true;
 inherited Sort;
 if PanelState<>nil then PanelState^:=C;
 InSort:=false;
end;
        {-DataCompBoy-}
{$ENDIF}

        {-DataCompBoy-}
function TFilesCollection.Compare;
 var T1: TFileRec;
     P2: PFileRec absolute Key2;
     C: Integer;
     SM, I1, I2: Integer;
     P1P, P2P: Boolean;
     PanelFlags: Word;
     OneIsFile: Boolean;
{$IFDEF SORTDEBUG}
 const NumCal: word = 0;
       States:Array[0..3] of char = '|/-\';
begin
 inc(NumCal);
 if InSort then
  if NumCal=1 then PanelState^:=States[0]
  else if NumCal=2 then PanelState^:=States[1]
  else if NumCal=3 then PanelState^:=States[2]
  else if NumCal=4 then begin PanelState^:=States[3]; NumCal:=0 end;
{$ELSE}
begin
{$ENDIF}
 if Owner<>nil then PanelFlags := PFilePanel(Owner)^.PanelFlags
               else PanelFlags := PanelDefaults.Show;
 Move(Key1^,T1,SizeOf(T1));
 with P2^ do
   begin
     P1P := T1.Name[1] = '.';
     P2P := Name[1] = '.';

     if P1P and not P2P then begin Compare := -1; Exit; end;
     if P2P and not P1P then begin Compare := +1; Exit; end;

     OneIsFile:=True;
{    if (T1.Attr and Directory <> 0) and (Attr and Directory <> 0) then
       OneIsFile:=False;}
     SM := SortMode;
     if (SM >= 7) and (SM <= 20) then begin
       if (T1.Name = Name) and ((T1.Attr or Attr) and Directory = 0) and
          (((SM-7) and 1 = 0) or (T1.Size = Size)) and
          (((SM-7) and 2 = 0) or (T1.Yr < Yr) or
           ((T1.Yr = Yr) and (T1.FDate <= FDate)) or
           ((T1.Yr = Yr) and (T1.FDate = FDate) and (T1.Second < Second))) and
          (((SM-7) and 4 = 0) or (T1.Attr = Attr)) and
          (((SM-7) and 8 = 0) or
                  CompareFiles(MakeNormName(T1.Owner^, MakeFileName(T1.Name)),
                               MakeNormName(Owner^, MakeFileName(Name))) )
           then C := 0 else if (T1.Name < Name) then C := -1
                                                 else C := 1;
       Compare := C;
       Exit;
     end;

     if (Owner <> nil) and (SM <> 5) and (SM <> 6) then
     begin
       if PanelFlags and fmiExeFirst <> 0 then
       begin
         if ((T1.Attr xor Attr) and Directory = 0) then
         begin
           if (T1.TType = ttExec) and (TType <> ttExec) then begin Compare := -1; Exit; end;
           if (T1.TType <> ttExec) and (TType = ttExec) then begin Compare := 1; Exit; end;
         end;
       end;
       if PanelFlags and fmiArchivesFirst <> 0 then
       begin
         if ((T1.Attr xor Attr) and Directory = 0) then
         begin
           if (T1.TType = ttArc) and (TType <> ttArc) then begin Compare := -1; Exit; end;
           if (T1.TType <> ttArc) and (TType = ttArc) then begin Compare := 1; Exit; end;
         end;
       end;
     end;

     case SM of
      0: if T1.Owner^ = Owner^
          then C := CmpLFN(T1.LFN,LFN)
          else if T1.Owner^ < Owner^ then C := -1 else C := 1;
      psmOrdered: begin
           I1 := T1.TType; I2 := TType;
           if I1 = 0 then I1 := 100;
           if I2 = 0 then I2 := 100;
           if I1 < I2 then C := -1 else
           if I1 > I2 then C := 1 else
              if T1.Name < Name then C := -1
                else if T1.Name > Name then C := 1
                  else begin
                    C := CmpLFN(T1.LFN,LFN);
                    If C = 0 then if T1.Owner^ < Owner^ then C := -1
                                    else if T1.Owner^ > Owner^ then C := 1;
                  end;
          end;
{NAME}1: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if T1.Name < Name then C := -1
            else if T1.Name > Name then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
{LFN} 2: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else begin
           C := CmpLFN(T1.LFN, LFN);
           If C = 0 then if T1.Owner^ < Owner^ then C := -1
                           else if T1.Owner^ > Owner^ then C := 1;
         end;
     141: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if T1.Name < Name then C := -1
            else if (T1.Name > Name) or (GetLFN(T1.LFN)<>GetLFN(LFN))
                  then C := 1
                  else C := 0;
{EXT} 3: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C:=-1
          else                               C:=1
         else
         if OneIsFile and (Copy(T1.Name, 10, 3) < Copy(Name, 10, 3)) then
           C:=-1
         else
         if OneIsFile and (Copy(Name, 10, 3) < Copy(T1.Name, 10, 3)) then
           C:=1
         else
         if T1.Name   < Name   then C:=-1 else
         if T1.Name   > Name   then C:= 1 else
         if T1.Owner^ < Owner^ then C:=-1 else
         if T1.Owner^ > Owner^ then C:= 1;
{LEXT}4: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C:=-1
          else                               C:=1
         else begin
          If OneIsFile then C := CmpLEX(T1.LFN, LFN) else C := 0;
          If C = 0 then C := CmpLFN(T1.LFN, LFN);
          If C = 0 then if T1.Owner^ < Owner^ then C:=-1 else
                        if T1.Owner^ > Owner^ then C:= 1;
         end;
{SIZE}5: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if T1.Size > Size then C := -1 else
           if T1.Size < Size then C := 1 else
           if T1.Name < Name then C := -1
            else if T1.Name > Name then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
{DATE}6: begin
         if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if (T1.Yr < Yr) then C := 1 else
           if (T1.Yr > Yr) then C := -1 else
           if (T1.FDate < FDate) then C := 1 else
           if (T1.FDate > FDate) then C := -1 else
           if (T1.Second < Second) then C := 1 else
           if (T1.Second > Second) then C := -1 else
           if T1.Name < Name then C := -1
            else if T1.Name > Name then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
         end;
      else if (T1.Attr and $3F = Attr and $3F) and (T1.Yr=Yr) and (T1.FDate = FDate) and (T1.Second = Second)
              and (T1.Name=Name) and (T1.Owner^ = Owner^) and (GetLFN(T1.LFN)=GetLFN(LFN))
               then C := 0 else C := -1;
     end;
  end;
 Compare := C;
end;
        {-DataCompBoy-}

function SelectDrive;
 var R: TRect;
     P: PMenuBox;
     Menu: PMenu;
     Items, Lnk: PMenuItem;
     C: Char;
     N, MaxRY: Integer;
     SC: TCharSet;
     Server_Num, Handle_Num, RetCode, DriveNum, MaxL: integer; {-$VIV start}
     FullS, Server, PathName, TmpS: String;       {-$VIV end}
     pSaveNeedAbort,ShowDir: Boolean;

  function CutLongString(S: String): String;
  var P, L: Integer;
      S1: String;
  begin
    CutLongString := #0+S+#0;
    if not CutDriveInfo then Exit;
    (* X-Man *)
    CutLongString:=FormatLongName(S,30,0,flnPreferName+flnHighlight+
    flnUseCutChar+flnHandleTildes,nfmNull,nil)
  end;

var IDDQD: record fl, dr: longint; end;

 procedure GetInfo(P: PFileRec); {$IFDEF BIT_16} far; {$ENDIF}
 begin
  if P<>nil then
   if P^.Attr and Directory = 0
    then inc(IDDQD.fl)
    else inc(IDDQD.dr)
 end;

begin
 Items := nil; N := 0; Lnk := nil;
 MaxL := 8; {-$VIV}

 if IncludeTemp and (DriveInfoType and ditIncludeQick <> 0) then begin
  C := ' ';
  for DriveNum := 8 downto 0 do
    if DirsToChange[DriveNum] <> nil then begin
      FreeStr:='  ~'+Itos(DriveNum+1)+':~ '+ CutH(DirsToChange[DriveNum]^,24);
      Items := NewItem(FreeStr, 'Alt-'+Itos(DriveNum+1), kbAlt1,
                       cmQuickChange1 + DriveNum, hcNoContext, Items);
      MaxL:=Max(CStrLen(FreeStr), MaxL);
      inc(N);
      C := '!';
    end;
  if C = '!' then Items := NewLine(Items);
 end;

 {$IFDEF MODEM}
 {$IFDEF LINK}
 if IncludeTemp and (Linker <> nil) then
   begin
     CL_GetLinkDrives(SC);
     if SC <> [] then
       begin
         for C := 'Z' downto 'A' do
           if C in SC then
             Items := NewItem('  ~'+C+':~ ', '', kbNoKey, 2000 + Byte(C),
                               hcNoContext, Items);
         Items := NewSubMenu('  ~+:~ LINK', hcNoContext, NewMenu(Items), nil);
         Lnk := Items;
         Inc(N)
       end;
   end;
 {$ENDIF}
 {$ENDIF}

 if IncludeTemp then begin
     If (DriveInfoType and ditIncludeTempInfo <> 0) then
      If TempFiles<>nil then begin
       IDDQD.Dr := 0; IDDQD.Fl := 0;
       TempFiles^.ForEach(@GetInfo);
       if IDDQD.Dr+IDDQD.Fl = 0 then FreeStr := GetString(dlEmpty) else
       if IDDQD.Dr = 0 then FreeStr := ItoS(IDDQD.Fl) + ' ' + #0 + GetString(dlDIFiles) + #0 else
       if IDDQD.Fl = 0 then FreeStr := ItoS(IDDQD.Dr) + ' ' + #0 + GetString(dlDirectories) + #0 else
        FormatStr(FreeStr, GetString(dlFilDir), IDDQD);
      end else FreeStr := GetString(dlEmpty)
     else FreeStr := '';

     Items := NewItem('  ~*:~ TEMP: '+FreeStr, '', kbSpace, 1200, hcTempList, Items);
     Items := NewLine(Items);
     Inc(N)
 end;

 for C := 'Z' downto 'A' do
  if ValidDrive(C) then
   begin
    FullS := '  ~'+C+':~ ';                          {-$VIV start}
    if InterfaceData.Options and ouiHideDriveInfo = 0 then
    begin
      DriveNum := Byte(C)-64;
      if DriveInfoType and ditMediaType <> 0 then
       begin
        {$IFDEF VIRTUALPASCAL}
        case SysGetDriveType(C) of
         dtFloppy    : FullS := FullS + 'Floppy    ';
         dtHDFAT     : FullS := FullS + 'HDD, FAT  ';
         dtHDHPFS    : FullS := FullS + 'HDD, HPFS ';
         dtInvalid   : FullS := FullS + 'Invalid   ';
         dtNovellNet : FullS := FullS + 'Novell    ';
         dtCDROM     : FullS := FullS + 'CD-ROM    ';
         dtLAN       : FullS := FullS + 'Lan Server';
         dtHDNTFS    : FullS := FullS + 'HDD, NTFS ';
         dtTVFS      : FullS := FullS + 'TVFS virt ';
         dtHDExt2    : FullS := FullS + 'HDD,ext2fs';
        end;
        {$ELSE}
        case getDriveType(DriveNum) of
         dtError     : FullS := FullS + sdtError     ;
         dtFixed     : FullS := FullS + sdtFixed     ;
         dtRemovable : FullS := FullS + sdtRemovable ;
         dtRemote    : FullS := FullS + sdtRemote    ;
         dtCDROM     : FullS := FullS + sdtCDROM     ;
         dtDblSpace  : FullS := FullS + sdtDblSpace  ;
         dtSUBST     : FullS := FullS + sdtSUBST     ;
         dtStacker   : FullS := FullS + sdtStacker   ;
         dtRAMDrive  : FullS := FullS + sdtRAMDrive  ;
         dtDublDisk  : FullS := FullS + sdtDublDisk  ;
         dtBernoully : FullS := FullS + sdtBernoully ;
         dtDiskreet  : FullS := FullS + sdtDiskreet  ;
         dtSuperStor : FullS := FullS + sdtSuperStor ;
        end;
        {$ENDIF}
       end;
      {$IFDEF NETINFO}
      Get_drive_connection_id(DriveNum, Server_num);
      if (Server_num > 0) and (DriveInfoType and ditNetDir <> 0) then
      begin
        Get_File_Server_Name(Server_Num, Server);
        Set_Preferred_Connection_Id(Server_Num);
        Get_Drive_Handle_Id(DriveNum, Handle_Num);
        Get_Directory_Path(Handle_Num, PathName, RetCode);
        lGetDir(DriveNum, TmpS); {DataCompBoy}
        if Length(TmpS) > 3 then
        begin
          TmpS := ' <' + Copy(PathName, Length(PathName) - Length(TmpS) + 4, 255) + '>';
          Delete(PathName, Length(PathName) - Length(TmpS) + 4, 255);
          if PathName[Length(PathName)] = '/' then Delete(PathName, Length(PathName), 1);
        end else TmpS := '';
        PathName := CutLongString(Server + '/' + PathName);
        PathName := PathName + TmpS;
        FullS := FullS + PathName;
      end else
      {$ENDIF}
      begin
          if (DriveNum<=2) or ((DriveInfoType and ditCDDir=0)
          and (DriveInfoType and ditLocalDir=0)) then ShowDir:=False
          else if (DriveInfoType and ditCDDir<>0)
          and (DriveInfoType and ditLocalDir<>0) then ShowDir:=True
          else if (DriveInfoType and ditLocalDir<>0)
          then ShowDir:=not IsDriveCDROM(Char(DriveNum+64))
          else ShowDir:=IsDriveCDROM(Char(DriveNum+64));
      if ShowDir then
      begin
        pSaveNeedAbort := NeedAbort;
        NeedAbort := True;
        lGetDir(DriveNum, PathName);{DataCompBoy}
        if IOResult = 0 then
        begin
          if Length(PathName) = 3 then PathName := '' else
          begin
            Delete(PathName, 1, 3);
            PathName := '<' + CutLongString(PathName) + '>';
          end;
          FullS := FullS + PathName;
        end;
        NeedAbort := pSaveNeedAbort;
      end
      end;
      if Length(FullS) > MaxL then MaxL := Length(FullS);
    end;                                                {-$VIV end}

    Items := NewItem(FullS, '', kbNoKey, 1000 + Byte(C),
                      hcNoContext, Items);
    Inc(N);
   end;
 if not (Default in ['A'..'Z']) and not ((Default = '+') and (Lnk <> nil))
  then C := GetCurDrive else C := Default;
 Menu := NewMenu(Items);
 DeskTop^.GetExtent(R);
 {-$VIV start}
 X := X - (MaxL div 2);
 if (X + MaxL + 4) > R.B.X then X := R.B.X - MaxL - 4;
 if (X < 0) then X := 0;
 Y := Y - (N div 2) + 1;
 if (Y + N + 2) > R.B.Y then Y := R.B.Y - N - 2;
 if (Y < 0) then Y := 0;
 R.A.X := X; R.A.Y := Y;
 R.B.X := R.A.X + MaxL + 4;
 MaxRY := R.B.Y;
 R.B.Y := R.A.Y + N + 2;
 if R.A.Y=0 then begin Inc(R.A.Y); Inc(R.B.Y) end;
 if (R.B.Y > MaxRY) then R.B.Y := MaxRY;
 {-$VIV end}
 P := New(PMenuBox, Init(R, Menu, nil)); {-$VIV}
 if (C='+') then Items := Lnk else Items := P^.FindItem(C);
 if Items <> nil then Menu^.Default := Items;
 P^.HelpCtx := hcSelectDrive + Byte( IncludeTemp = True );

 N := DeskTop^.ExecView(P);
 Dispose(P,Done);
 DisposeMenu(Menu);
 SelectDrive := '';
 if N > 1000 then SelectDrive := Char(N - 1000)+':';
 if N = 1200 then SelectDrive := cTEMP_;
 if N > 2000 then SelectDrive := '+' + Char(N - 2000);
 if (N >= cmQuickChange1) and (N <= cmQuickChange9)
             then SelectDrive := CnvString(DirsToChange[N-cmQuickChange1]);
end;

function GetFileType(const S: String; Attr: Byte): Integer;
begin
  if Attr and Directory <> 0 then GetFileType := ttDirectory else
  begin
   GetFileType := 0;
   if InExtFilter(S, Executables) then GetFileType := ttExec
    else if InExtFilter(S, Archives   ^)  then GetFileType := ttArc
    else if InExtFilter(S, CustomMask1^)  then GetFileType := ttCust1
    else if InExtFilter(S, CustomMask2^)  then GetFileType := ttCust2
    else if InExtFilter(S, CustomMask3^)  then GetFileType := ttCust3
    else if InExtFilter(S, CustomMask4^)  then GetFileType := ttCust4
    else if InExtFilter(S, CustomMask5^)  then GetFileType := ttCust5
{JO}else if InExtFilter(S, CustomMask6^)  then GetFileType := ttCust6
    else if InExtFilter(S, CustomMask7^)  then GetFileType := ttCust7
    else if InExtFilter(S, CustomMask8^)  then GetFileType := ttCust8
    else if InExtFilter(S, CustomMask9^)  then GetFileType := ttCust9
{JO}else if InExtFilter(S, CustomMask10^) then GetFileType := ttCust10
  end;
end;

        {-DataCompBoy-}
function NewFileRec;
  var PR: PFileRec;
      T:  TFileRec;
      D: DateTime;
begin
  if Name[1] <> '.'
   then T.Name:=Norm12(GetUrz(Name))
   else T.Name := '..          ';
  T.Name[9] := ' ';
  if Attr and Directory <> 0 then UpStr(T.Name) else LowStr(T.Name);
  if Attr and Hidden <> 0 then T.Name[9] := #177;
  if Attr and SysFile <> 0 then begin T.Name[9] := #178; T.Name[1] := Upcase(T.Name[1]); end;
  T.LFN := AddLFN(GetName(LFN));
  T.Size := Size; T.PSize := Size;
  if Date = 0 then
  begin
    T.Yr     := 1980;
    T.Month  := 1;
    T.Day    := 1;
    T.Hour   := 1;
    T.Minute := 1;
    T.Second := 1;
  end else
  begin
    UnpackTime(Date, D);
    T.Yr := D.Year;
    T.Month := D.Month;
    T.Day := D.Day;
    T.Hour := D.Hour;
    T.Minute := D.Min;
    T.Second := D.Sec;
  end;
  T.Attr := Attr and $7FFF;
  T.Selected := Off;
  T.DIZ := nil;
  T.TType := GetFileType(LFN, T.Attr);
  T.Owner := AOwner;
  T.OwnerDisposible := false;
  New(PR);
  PR^ := T;
  NewFileRec := PR;
end;
        {-DataCompBoy-}

function PackedDate(P: PFileRec): LongInt;
var
  DT: DateTime;
  L: LongInt;
begin
  with P^ do
  begin
    DT.Year   := Yr;
    DT.Month  := Month;
    DT.Day    := Day;
    DT.Hour   := Hour;
    DT.Min    := Minute;
    DT.Sec    := Second;
  end;
  PackTime(DT, L);
  PackedDate := L;
end;

procedure DosReread(Files: PFilesCollection);
var i: longint;
    sr: lSearchRec;
    p: PFileRec;
    D: DateTime;
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
 if Files=nil then exit;
 i:=0;
 while i<Files^.Count do begin
{$IFDEF CHDIRDEBUG}
   PanelState^:=States[NumCal];
   inc(NumCal);
   if NumCal=4 then NumCal:=0;
{$ENDIF}
  ClrIO;
  p:=Files^.At(I);
  with P^ do lFindFirst(MakeNormName(Owner^, GetLFN(LFN)),
                        $3F xor VolumeID, SR);
  lFindClose(SR);
  if (DosError <> 0) or Abort then Files^.AtFree(i)
  else begin
   P^.Size  := SR.FullSize;
   UnpackTime(SR.SR.Time, D);
   P^.Yr := D.Year;
   P^.Month := D.Month;
   P^.Day := D.Day;
   P^.Hour := D.Hour;
   P^.Minute := D.Min;
   P^.Second := D.Sec;
   P^.Attr  := SR.SR.Attr;
   inc(i);
  end;
 end;
 If Files^.Count > 0 then Files^.Sort;
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

end.
