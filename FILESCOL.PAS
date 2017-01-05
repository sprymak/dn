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

unit FilesCol;

interface
 uses LFN,
 {$IFNDEF OS2}LFNCol,{$ENDIF} {DataCompBoy}
 Objects, Memory, Startup,
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
{$IFNDEF OS2}
     Name: Str12;
     LFN: TLFNIndex; {DataCompBoy}
{$ELSE}
     Name: String;
{$ENDIF}
     Yr: Word;
     YrCreat: Word;
     YrLAcc: Word;
     TType: Byte;
     Attr: Word;
     Second: Byte;
     SecondCreat: Byte;
     SecondLAcc: Byte;
     Selected: Boolean;
     UsageCount: byte;{DataCompBoy}
     case Boolean of
       True : (FDate, FDateCreat, FDateLAcc: LongInt);
       False: (Minute, Hour, Day, Month,
               MinuteCreat, HourCreat, DayCreat, MonthCreat,
               MinuteLAcc, HourLAcc, DayLAcc, MonthLAcc: Byte);
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
{$IFDEF Win32}
     LFNActive: Boolean;
{$ENDIF}
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
{$IFNDEF OS2}
function  NewFileRec(const LFN, Name: String; Size:TSize; Date, CreationDate, LastAccDate: LongInt; Attr: Word; AOwner: PString): PFileRec; {DataCompBoy}
{$ELSE}
function  NewFileRec(const Name: String; Size:TSize; Date, CreationDate, LastAccDate: LongInt; Attr: Word; AOwner: PString): PFileRec; {DataCompBoy}
{$ENDIF}
procedure DelFileRec(var FR: PFileRec); {DataCompBoy}
function  LoadFileRec(var s: TStream): PFileRec; {DataCompBoy}
procedure StoreFileRec(var s: TStream; fr: PFileRec); {DataCompBoy}
function  LoadFileRecOwn(var s: TStream; Dirs: PCollection): PFileRec; {DataCompBoy}
procedure StoreFileRecOwn(var s: TStream; fr: PFileRec; Dirs: PCollection); {DataCompBoy}
function  PackedDate(P: PFileRec): LongInt; {DataCompBoy}
function PackedCreationDate(P: PFileRec): LongInt; {JO}
function PackedLastAccDate(P: PFileRec): LongInt;  {JO}
function  GetFileType(const S: String; Attr: Byte): Integer;
procedure DosReread(Files: PFilesCollection);

 const Executables: TMaskData = (Filter: 'exe;bat;com;cmd');

implementation
uses DNApp, Menus, Views, FlPanel, Drives,
     Commands, Messages,
     {!!}CmdLine
     {$IFDEF MODEM}{$IFDEF LINK},NavyLink{$ENDIF}{$ENDIF}
     {$IFDEF NETINFO}, Novell{$ENDIF}
     {$IFNDEF NONBP}, DiskTool{$ENDIF}
     {$IFDEF VIRTUALPASCAL}, VpSysLow, VPUtils{$ELSE},DrvTypes{$ENDIF} {$IFDEF OS2},FlOS2Tl{$ENDIF}
     , DnIni, DOS

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
  fillchar(fr^, sizeof(fr^), 0);  {AK155}
{AK155  fillword(fr^, sizeof(fr^) shr 1, 0);
     Это хороший пример того, как не надо делать.
 Экономим микросекунды, а закладываем на будущее глюки,
 если размер word станет не 2 (а, скажем, 4) или если
 размер fr^ станет нечетным (что вполне может произойти) }

{$IFDEF OS_DOS}
  Name:=lfGetLongFileName(lFExpand(Name));
{$ELSE}
  Name:=lFExpand(Name);
{$ENDIF}
  lFindFirst(Name+'*', AnyFile, lsr);
  Path:=GetPath(Name);
  Name:=GetName(Name);
  while (lsr.FullName<>Name) and (DosError=0) do lFindNext(lsr);
  lFindClose(lsr);
  if DosError=0 then
   begin
{$IFNDEF OS2}
    fr^.name:=Norm12(lsr.sr.name);
    fr^.LFN:=AddLFN(lsr.FullName);
{$ELSE}
   {fr^.name:=lsr.sr.name;}{???}
    fr^.name:=lsr.FullName;
{$ENDIF}
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
    UnpackTime(lsr.sr.CreationTime, D);         {JO}
    fr^.YrCreat := D.Year;
    fr^.MonthCreat := D.Month;
    fr^.DayCreat := D.Day;
    fr^.HourCreat := D.Hour;
    fr^.MinuteCreat := D.Min;
    fr^.SecondCreat := D.Sec;
    UnpackTime(lsr.sr.LastAccessTime, D);
    fr^.YrLAcc := D.Year;
    fr^.MonthLAcc := D.Month;
    fr^.DayLAcc := D.Day;
    fr^.HourLAcc := D.Hour;
    fr^.MinuteLAcc := D.Min;
    fr^.SecondLAcc := D.Sec;                    {/JO}
    fr^.Attr:=lsr.sr.attr;
    if CharCount('.', lsr.FullName)=0 then begin
     SetLength(lsr.FullName, Length(lsr.FullName)+1);
     lsr.FullName[length(lsr.FullName)]:='.';
     fr^.TType := GetFileType(lsr.FullName, fr^.Attr);
     SetLength(lsr.FullName, Length(lsr.FullName)-1);
    end else fr^.TType := GetFileType(lsr.FullName, fr^.Attr);
{Cat: это явно лишнее - для масок !\!.! для просмотрщиков и запуска по
      расширению должны передаваться неискажённые имена файлов
  PS  и следует ещё раз посмотреть и проверить - нужна ли вообще эта функция
     if fr^.Attr and Directory <> 0 then UpStr(fr^.Name) else LowStr(fr^.Name);}
{/Cat}
   end
  else
   begin
    fr^.Owner:=NewStr(Path);
    fr^.OwnerDisposible:=true;
{$IFNDEF OS2}
    fr^.Name:=Norm12(GetURZ(GetName(lfGetShortFileName(Name))));
    fr^.LFN:=AddLFN(GetName(Name));
{$ELSE}
    fr^.Name:=GetName(Name);
{$ENDIF}
   end;
 fr^.UsageCount:=1;
 end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function CopyFileRec;
begin
 CopyFileRec := FR;
 Inc(FR^.UsageCount);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure DelFileRec(var FR: PFileRec);
begin
 if FR <> nil then begin
 Dec(FR^.UsageCount);
 IF FR^.UsageCount=0 then begin
  if (FR^.DIZ <> nil) and FR^.DIZ^.isDisposable then begin
   DisposeStr(FR^.DIZ^.DIZ);
   Dispose(FR^.DIZ);
  end;
  if FR^.OwnerDisposible then DisposeStr(FR^.Owner);
  {$IFNDEF OS2}
  DelLFN(FR^.LFN);
  {$ENDIF}
  Dispose(FR);
  FR:=Nil;
 end end;
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
  {$IFNDEF OS2}
  LFN:=LoadLFN(S);
  {$ENDIF}
  S.Read(Yr, SizeOf(Yr));
  S.Read(YrCreat, SizeOf(YrCreat));
  S.Read(YrLAcc, SizeOf(YrLAcc));
  S.Read(TType, SizeOf(TType));
  S.Read(Attr, SizeOf(Attr));
  S.Read(Second, SizeOf(Second));
  S.Read(SecondCreat, SizeOf(SecondCreat));
  S.Read(SecondLAcc, SizeOf(SecondLAcc));
  S.Read(Selected, SizeOf(Selected));
  S.Read(FDate, SizeOf(FDate));
  S.Read(FDateCreat, SizeOf(FDateCreat));
  S.Read(FDateLAcc, SizeOf(FDateLAcc));
  S.Read(OwnerDisposible, SizeOf(OwnerDisposible));
  if OwnerDisposible then Owner:=S.ReadStr;
  UsageCount:=1;
 end;
 LoadFileRec:=P;
end;

procedure StoreFileRec(var s: TStream; fr: PFileRec);
begin with fr^ do begin
  s.Write(Size, Sizeof(Size));
  s.Write(PSize, Sizeof(PSize));
  S.Write(Name, SizeOf(Name));
  {$IFNDEF OS2}
  StoreLFN(S, LFN);
  {$ENDIF}
  S.Write(Yr, SizeOf(Yr));
  S.Write(YrCreat, SizeOf(YrCreat));
  S.Write(YrLAcc, SizeOf(YrLAcc));
  S.Write(TType, SizeOf(TType));
  S.Write(Attr, SizeOf(Attr));
  S.Write(Second, SizeOf(Second));
  S.Write(SecondCreat, SizeOf(SecondCreat));
  S.Write(SecondLAcc, SizeOf(SecondLAcc));
  S.Write(Selected, SizeOf(Selected));
  S.Write(FDate, SizeOf(FDate));
  S.Write(FDateCreat, SizeOf(FDateCreat));
  S.Write(FDateLAcc, SizeOf(FDateLAcc));
  S.Write(OwnerDisposible, SizeOf(OwnerDisposible));
  if OwnerDisposible then S.WriteStr(Owner);
end end;
        {-DataCompBoy-}

function  LoadFileRecOwn(var s: TStream; Dirs: PCollection): PFileRec; {DataCompBoy}
var FR: PFileRec;
    w: longint;
begin
 FR := LoadFileRec(S);
 if FR <> nil then
  if not FR^.OwnerDisposible then begin
   S.Read(W, SizeOf(W));
   FR^.Owner := Dirs^.At(W);
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
 {$IFNDEF OS2}
 LFNActive := True;
 {$ENDIF}
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
     ST1, ST2: String;

{$IFNDEF OS_DOS}
    function    CmpName (S1, S2: String): Integer;
    begin
  {$IFDEF Win32}
    UpLowStr(S1); UpLowStr(S2);
  {$ENDIF}
      if S1>S2 then CmpName := +1 else
      if S1<S2 then CmpName := -1 else
                    CmpName := 0;
    end;

    function    CmpEXT(S1, S2: String): Integer;
    var E1, E2: String;
        B, K: Byte;
    begin
  {$IFDEF Win32}
    S1 := UpLowStrg(S1);
  {$ENDIF}
     For B := Length(S1) downto 1 do if S1[B]='.' then break;
     if (B > 1) or (S1[1]='.') then begin
      K := Length(S1)-B+1;
      Move(S1[B], E1[1], K);
      SetLength(E1, K);
      SetLength(S1, B-1);
     end else Word(((@E1)^)) := $2E01; {E1 := '.'}
  {$IFDEF Win32}
     S2 := UpLowStrg(S2);
  {$ENDIF}
     For B := Length(S2) downto 1 do if S2[B]='.' then break;
     if (B > 1) or (S2[1]='.') then begin
      K := Length(S2)-B+1;
      Move(S2[B], E2[1], K);
      SetLength(E2, K);
      SetLength(S2, B-1);
     end else Word(((@E2)^)) := $2E01; {E2 := '.'}
     if E1>E2 then CmpEXT := +1 else
     if E1<E2 then CmpEXT := -1 else
     if S1>S2 then CmpEXT := +1 else
     if S1<S2 then CmpEXT := -1 else
                   CmpEXT := 0;
    end;

{$ELSE}

    function    CmpLFN(LFN1, LFN2: TLFNIndex): integer;
    var S1, S2: string;
    begin
     begin
      S1 := GetLFN(LFN1); S2 := GetLFN(LFN2);
      UpLowStr(S1); UpLowStr(S2); {JO}
      if S1>S2 then CmpLFN := +1 else
      if S1<S2 then CmpLFN := -1 else
                    CmpLFN := 0;
     end;
    end;

    function    CmpLEX(LFN1, LFN2: TLFNIndex): integer;
    var S1, S2: string;
        E1, E2: string;
        B, K: Byte;
    begin
     S1 := UpLowStrg(GetLFN(LFN1)); {JO}
     For B := Length(S1) downto 1 do if S1[B]='.' then break;
     if (B > 1) or (S1[1]='.') then begin
      K := Length(S1)-B+1;
      Move(S1[B], E1[1], K);
      SetLength(E1, K);
      SetLength(S1, B-1);
     end else Word(((@E1)^)) := $2E01; {E1 := '.'}
     S2 := UpLowStrg(GetLFN(LFN2)); {JO}
     For B := Length(S2) downto 1 do if S2[B]='.' then break;
     if (B > 1) or (S2[1]='.') then begin
      K := Length(S2)-B+1;
      Move(S2[B], E2[1], K);
      SetLength(E2, K);
      SetLength(S2, B-1);
     end else Word(((@E2)^)) := $2E01; {E2 := '.'}
     if E1>E2 then CmpLEX := +1 else
     if E1<E2 then CmpLEX := -1 else
     if S1>S2 then CmpLEX := +1 else
     if S1<S2 then CmpLEX := -1 else
                   CmpLEX := 0;
    end;

{$ENDIF}

    var
      C1: integer; { результат сравнения имён   // AK155}
      Name2: string; {UpStrg(P2^.Name)  // AK155}
begin
 if Owner<>nil then PanelFlags := PFilePanel(Owner)^.PanelFlags
               else PanelFlags := PanelDefaults.Show;
 Move(Key1^,T1,SizeOf(T1));
 with P2^ do
   begin
{$IFNDEF OS2}
  {$IFNDEF WIN32}
     P1P := T1.Name[1] = '.';
     P2P := Name[1] = '.';
  {$ELSE}
     P1P := T1.LFN = '..';
     P2P := LFN = '..';
  {$ENDIF}
{$ELSE}
     P1P := T1.Name = '..';
     P2P := Name = '..';
{$ENDIF}
     if P1P and not P2P then begin Compare := -1; Exit; end;
     if P2P and not P1P then begin Compare := +1; Exit; end;

     Name2 := UpLowStrg(Name); {JO}

     OneIsFile:=True;
{    if (T1.Attr and Directory <> 0) and (Attr and Directory <> 0) then
       OneIsFile:=False;}
     SM := SortMode;

     if (SM >= NumSortModes) and (SM <= NumSortModes + 31) then begin { условие используется для сравнения }
                                                                      {  каталогов по Ctrl-C }
  {$IFDEF Win32}     {здесь C1 используется для сравнения каталогов по Ctrl-C}
     if LFNActive then
       begin
         if ((SM-NumSortModes) and 16 = 0) then C1 := CmpName(T1.LFN, LFN)
          else
             if T1.LFN < LFN then
               C1 := -1
             else if T1.LFN = LFN then
               C1 := 0
             else
               C1 := 1;
       end
      else
  {$ENDIF}
  {$IFNDEF Win32} {JO: регистрочувствительное сравнение по коротким именам лишено смысла}
       begin
         if ((SM-NumSortModes) and 16 = 0) then
  {$ENDIF}
           begin
             UpLowStr(T1.Name);
             if T1.Name < Name2 then
               C1 := -1
             else if T1.Name = Name2 then
               C1 := 0
             else
               C1 := 1;
           end
  {$IFNDEF Win32}
          else
           begin
             if T1.Name < Name then
               C1 := -1
             else if T1.Name = Name then
               C1 := 0
             else
               C1 := 1;
           end;
       end
  {$ENDIF}
       ;

       if (C1=0) and ((T1.Attr or Attr) and Directory = 0) and
          (((SM-NumSortModes) and 1 = 0) or (T1.Size = Size)) and
          (((SM-NumSortModes) and 2 = 0) or (T1.Yr < Yr) or
           ((T1.Yr = Yr) and (T1.FDate <= FDate)) or
           ((T1.Yr = Yr) and (T1.FDate = FDate) and (T1.Second < Second))) and
          (((SM-NumSortModes) and 4 = 0) or (T1.Attr = Attr)) and
          (((SM-NumSortModes) and 8 = 0) or
{$IFNDEF OS2}
                  CompareFiles(MakeNormName(T1.Owner^, MakeFileName(T1.Name)),
                               MakeNormName(Owner^, MakeFileName(Name2))) )
{$ELSE}
                  CompareFiles(MakeNormName(T1.Owner^, T1.Name),
                               MakeNormName(Owner^, Name2)) )
{$ENDIF}
           then C := 0 else if (C1=-1) then C := -1
                                                 else C := 1;
       Compare := C;
       Exit;
     end; { конец куска для сравнения каталогов по Ctrl-C }
          { дальше идёт кусок для сортировки файлов в панелях }

     UpLowStr(T1.Name); {JO}

{$IFDEF Win32}       {далее C1 используется для сортировки файлов в панелях}
     if LFNActive then C1 := CmpName(T1.LFN, LFN) else
{$ENDIF}
     begin
       if T1.Name < Name2 then
         C1 := -1
       else if T1.Name = Name2 then
         C1 := 0
       else
         C1 := 1;
     end;

     if (Owner <> nil) and not (SM in [psmSize, psmTime, psmCrTime, psmLATime]) then
     if PanelFlags and fmiExeBeforeArc <> 0 then
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
       end
     else
       begin
         if PanelFlags and fmiArchivesFirst <> 0 then
         begin
           if ((T1.Attr xor Attr) and Directory = 0) then
           begin
             if (T1.TType = ttArc) and (TType <> ttArc) then begin Compare := -1; Exit; end;
             if (T1.TType <> ttArc) and (TType = ttArc) then begin Compare := 1; Exit; end;
           end;
         end;
         if PanelFlags and fmiExeFirst <> 0 then
         begin
           if ((T1.Attr xor Attr) and Directory = 0) then
           begin
             if (T1.TType = ttExec) and (TType <> ttExec) then begin Compare := -1; Exit; end;
             if (T1.TType <> ttExec) and (TType = ttExec) then begin Compare := 1; Exit; end;
           end;
       end;
     end;

     case SM of
     psmUnsorted: if T1.Owner^ = Owner^
        then if C1 = 0 then C := 0 else C := -1
          else if T1.Owner^ < Owner^ then C := -1 else C := 1;
      psmOrdered: begin
           I1 := T1.TType; I2 := TType;
           if I1 = 0 then I1 := 100;
           if I2 = 0 then I2 := 100;
           if I1 < I2 then C := -1 else
           if I1 > I2 then C := 1 else
              if C1=-1 then C := -1
                else if C1=1 then C := 1
                  else begin
{$IFNDEF OS2}
     {$IFDEF STRINGLFN}
                    C := CmpName(T1.LFN, LFN);
     {$ELSE}
                    C := CmpLFN(T1.LFN, LFN);
     {$ENDIF}
{$ELSE}
                    C := CmpName(T1.Name, Name2);
{$ENDIF}
                    If C = 0 then if T1.Owner^ < Owner^ then C := -1
                                    else if T1.Owner^ > Owner^ then C := 1;
                  end;
          end;
{$IFNDEF OS2}
{NAME}psmName:
         if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if T1.Name < Name2 then C := -1
            else if T1.Name > Name2 then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
{$ENDIF}
{LFN}psmLongName:
          if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else begin
{$IFNDEF OS2}
  {$IFDEF STRINGLFN}
           C := CmpName(T1.LFN, LFN);
  {$ELSE}
           C := CmpLFN(T1.LFN, LFN);
  {$ENDIF}
{$ELSE}
           C := CmpName(T1.Name, Name2);
{$ENDIF}
           If C = 0 then if T1.Owner^ < Owner^ then C := -1
                           else if T1.Owner^ > Owner^ then C := 1;
         end;
     141: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if T1.Name < Name2 then C := -1
            else if (T1.Name > Name2) {$IFNDEF OS2} or (GetLFN(T1.LFN)<>GetLFN(LFN)) {$ENDIF}
                  then C := 1
                  else C := 0;
{$IFNDEF OS2}
{EXT}psmExtension:
         if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C:=-1
          else                               C:=1
         else
         if OneIsFile and (Copy(T1.Name, 10, 3) < Copy(Name, 10, 3)) then
           C:=-1
         else
         if OneIsFile and (Copy(Name, 10, 3) < Copy(T1.Name, 10, 3)) then
           C:=1
         else
         if T1.Name   < Name2  then C:=-1 else
         if T1.Name   > Name2  then C:= 1 else
         if T1.Owner^ < Owner^ then C:=-1 else
         if T1.Owner^ > Owner^ then C:= 1;
{$ENDIF}
{LEXT}psmLongExt:
          if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C:=-1
          else                               C:=1
         else begin
{$IFNDEF OS2}
   {$IFDEF STRINGLFN}
          If OneIsFile then C := CmpEXT(T1.LFN, LFN) else C := 0;
          If           C = 0 then C := CmpName(T1.LFN, LFN);
   {$ELSE}
          If OneIsFile then C := CmpLEX(T1.LFN, LFN) else C := 0;
          If C = 0 then C := CmpLFN(T1.LFN, LFN);
   {$ENDIF}
{$ELSE}
          If OneIsFile then C := CmpEXT(T1.Name, Name2) else C := 0;
          If C = 0 then C := CmpName(T1.Name, Name2);
{$ENDIF}
          If C = 0 then if T1.Owner^ < Owner^ then C:=-1 else
                        if T1.Owner^ > Owner^ then C:= 1;
         end;
{SIZE}psmSize:
          if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if T1.Size > Size then C := -1 else
           if T1.Size < Size then C := 1 else
           if C1=-1 then C := -1
            else if C1=1 then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
{DATE}psmTime:
         begin
         if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if (T1.Yr < Yr) then C := 1 else
           if (T1.Yr > Yr) then C := -1 else
           if (T1.FDate < FDate) then C := 1 else
           if (T1.FDate > FDate) then C := -1 else
           if (T1.Second < Second) then C := 1 else
           if (T1.Second > Second) then C := -1 else
           if C1=-1 then C := -1
            else if C1=1 then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
         end;
{JO}
{$IFNDEF OS_DOS}
{Creat.DATE}psmCrTime:
         begin
         if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if (T1.YrCreat < YrCreat) then C := 1 else
           if (T1.YrCreat > YrCreat) then C := -1 else
           if (T1.FDateCreat < FDateCreat) then C := 1 else
           if (T1.FDateCreat > FDateCreat) then C := -1 else
           if (T1.SecondCreat < SecondCreat) then C := 1 else
           if (T1.SecondCreat > SecondCreat) then C := -1 else
           if C1=-1 then C := -1
            else if C1=1 then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
         end;
{L.Acc.DATE}psmLATime:
         begin
         if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if (T1.YrLAcc < YrLAcc) then C := 1 else
           if (T1.YrLAcc > YrLAcc) then C := -1 else
           if (T1.FDateLAcc < FDateLAcc) then C := 1 else
           if (T1.FDateLAcc > FDateLAcc) then C := -1 else
           if (T1.SecondLAcc < SecondLAcc) then C := 1 else
           if (T1.SecondLAcc > SecondLAcc) then C := -1 else
           if C1=-1 then C := -1
            else if C1=1 then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
         end;
{$ENDIF OS_DOS}

{DIZ}psmDIZ:
         if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
          begin
           if (T1.DIZ <> nil) and (T1.DIZ^.DIZ <> nil) then ST1 := UpLowStrg(CnvString(T1.DIZ^.DIZ)) else ST1 := '';
           if (P2^.DIZ <> nil) and (P2^.DIZ^.DIZ <> nil) then ST2 := UpLowStrg(CnvString(DIZ^.DIZ)) else ST2 := '';
           if (ST1 <> '') and (ST2 = '') then C := -1 else
             if (ST1 = '') and (ST2 <> '') then C := 1 else
               C := CmpName(ST1, ST2);
           if C = 0 then C := C1;
           if C = 0 then if T1.Owner^ < Owner^ then C := -1
                           else if T1.Owner^ > Owner^ then C := 1;
          end;
{/JO}
      else if (T1.Attr and $3F = Attr and $3F) and (T1.Yr=Yr) and (T1.FDate = FDate) and (T1.Second = Second)
              and (C1=0) and (T1.Owner^ = Owner^) {$IFNDEF OS2} and (GetLFN(T1.LFN)=GetLFN(LFN)) {$ENDIF}
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
{$IFDEF VIRTUALPASCAL}
     DrInvalid: Boolean;
{$ENDIF}

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

var
  DT: TDriveType;
begin
 Items := nil; N := 0; Lnk := nil;
 MaxL := 8; {-$VIV}

 if IncludeTemp and (DriveInfoType and ditIncludeQick <> 0) then begin
  C := ' ';
  for DriveNum := 8 downto 0 do
    if DirsToChange[DriveNum] <> nil then begin
      FreeStr:=' ~'+Itos(DriveNum+1)+':~ '+ CutH(DirsToChange[DriveNum]^,24);
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
             Items := NewItem(' ~'+C+':~ ', '', kbNoKey, 2000 + Byte(C),
                               hcNoContext, Items);
         Items := NewSubMenu(' ~+:~ LINK', hcNoContext, NewMenu(Items), nil);
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

     Items := NewItem(' ~*:~ TEMP: '+FreeStr, '', kbSpace, 1200, hcTempList, Items);
     Items := NewLine(Items);
     Inc(N)
 end;

 for C := 'Z' downto 'A' do
  if ValidDrive(C) then
   begin
    FullS := '~'+C+':~ ';                          {-$VIV start}
    if (InterfaceData.Options and ouiHideDriveInfo = 0) then
    begin
      DrInvalid := False;
      DriveNum := Byte(C)-64;
      if (DriveInfoType and ditMediaType <> 0) then
       if not ((DriveInfoType and ditSkipFloppy <> 0) and (DriveNum <= 2)) then
       begin
{$IFNDEF OS2}
        {$IFDEF VIRTUALPASCAL}
        DT := SysGetDriveType(C);
        case SysGetDriveType(C) of
         dtFloppy    : FullS := FullS + GetString(sdtRemovable) + '    ';
         dtHDFAT     : FullS := FullS + GetString(sdtFixed) + ' FAT  ' + GetString(sdtFixed1);
         dtHDHPFS    : FullS := FullS + GetString(sdtFixed) + ' HPFS ' + GetString(sdtFixed1);
         dtInvalid   : begin FullS := FullS + GetString(sdtError) + '    '; DrInvalid := True; end;
         dtNovellNet : FullS := FullS + ' Novell     ';
         dtCDROM     : FullS := FullS + ' CD-ROM     ';
         dtLAN       : FullS := FullS + ' Lan Server ';
         dtHDNTFS    : FullS := FullS + GetString(sdtFixed) + ' NTFS ' + GetString(sdtFixed1);
         dtTVFS      : FullS := FullS + 'TVFS       ';
         dtHDExt2    : FullS := FullS + GetString(sdtFixed) + ' ext2 ' + GetString(sdtFixed1);
         dtJFS       : FullS := FullS + GetString(sdtFixed) + ' JFS  ' + GetString(sdtFixed1);
        end;
        {$ELSE}
{X-Man >>>}
         dtError     : FullS := FullS + GetString(sdtError);
         dtFixed     : FullS := FullS + GetString(sdtFixed) + GetString(sdtFixed1) + '   ';
         dtRemovable : FullS := FullS + GetString(sdtRemovable);
         dtRemote    : FullS := FullS + GetString(sdtRemote);
         dtCDROM     : FullS := FullS + GetString(sdtCDROM);
         dtDblSpace  : FullS := FullS + GetString(sdtDblSpace);
         dtSUBST     : FullS := FullS + GetString(sdtSUBST);
         dtStacker   : FullS := FullS + GetString(sdtStacker);
         dtRAMDrive  : FullS := FullS + GetString(sdtRAMDrive);
         dtDublDisk  : FullS := FullS + GetString(sdtDublDisk);
         dtBernoully : FullS := FullS + GetString(sdtBernoully);
         dtDiskreet  : FullS := FullS + GetString(sdtDiskreet);
         dtSuperStor : FullS := FullS + GetString(sdtSuperStor);
        {$ENDIF}
{X-Man <<<}
{$ELSE}
FullS := FullS + GetDriveTypeString(C) + ' ';
{$ENDIF}
       end
       else if DriveNum <= 2 then FullS := FullS + GetString(sdtRemovable) + '    ';
      {$IFDEF VIRTUALPASCAL}
       if (DriveInfoType and ditShowVolume <> 0) and not DrInvalid and
         not ((DriveInfoType and ditSkipFloppy <> 0) and (DriveNum <= 2)) then
         FullS := FullS + GetVolumeLabel(C);
      {$ENDIF}
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
      if Length(FullS) > MaxL then
        MaxL := Length(FullS);
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
  {$IFDEF Win32}
      Name12: Str12;
  {$ENDIF}
begin
{$IFNDEF OS2}
  {$IFDEF OS_DOS}
  if Name[1] <> '.'
  {$ELSE}
  if LFN <> '..'
  {$ENDIF}
   then
  {$IFDEF OS_DOS}
      T.Name := Norm12(GetUrz(Name))
  {$ELSE}
      begin
       Name12 := Norm12(GetUrz(Name));
       if Norm12(Name) = Name12 then T.Name := Name12
         else T.Name := #22#22#22#22#22#22#22#22' '#22#22#22;
      end
  {$ENDIF}
   else T.Name := '..          ';
  T.Name[9] := ' ';
  if Attr and Directory <> 0 then UpStr(T.Name) else LowStr(T.Name);
  if Attr and Hidden <> 0 then T.Name[9] := #177;
  if Attr and SysFile <> 0 then begin T.Name[9] := #178; T.Name[1] := Upcase(T.Name[1]); end;
  {$IFDEF OS_DOS}
  T.LFN := AddLFN(GetName(LFN));
  {$ELSE}
  T.LFN := LFN;
  {$ENDIF}
{$ELSE}
  T.Name := Name;
{$ENDIF}
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
  if CreationDate = 0 then
    begin
      T.YrCreat     := 0;
      T.MonthCreat  := 0;
      T.DayCreat    := 0;
      T.HourCreat   := 0;
      T.MinuteCreat := 0;
      T.SecondCreat := 0;
    end else
    begin
      UnpackTime(CreationDate, D);
      T.YrCreat := D.Year;
      T.MonthCreat := D.Month;
      T.DayCreat := D.Day;
      T.HourCreat := D.Hour;
      T.MinuteCreat := D.Min;
      T.SecondCreat := D.Sec;
    end;
  if LastAccDate = 0 then
    begin
      T.YrLAcc     := 0;
      T.MonthLAcc  := 0;
      T.DayLAcc    := 0;
      T.HourLAcc   := 0;
      T.MinuteLAcc := 0;
      T.SecondLAcc := 0;
    end else
    begin
      UnpackTime(LastAccDate, D);
      T.YrLAcc := D.Year;
      T.MonthLAcc := D.Month;
      T.DayLAcc := D.Day;
      T.HourLAcc := D.Hour;
      T.MinuteLAcc := D.Min;
      T.SecondLAcc := D.Sec;
    end;
  T.Attr := Attr and $7FFF;
  T.Selected := Off;
  T.DIZ := nil;
{$IFNDEF OS2}
  T.TType := GetFileType(LFN, T.Attr);
{$ELSE}
  T.TType := GetFileType(Name, T.Attr);
{$ENDIF}
  T.Owner := AOwner;
  T.OwnerDisposible := false;
  T.UsageCount:=1;
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

function PackedCreationDate(P: PFileRec): LongInt;
var
  DT: DateTime;
  L: LongInt;
begin
  with P^ do
  begin
    DT.Year   := YrCreat;
    DT.Month  := MonthCreat;
    DT.Day    := DayCreat;
    DT.Hour   := HourCreat;
    DT.Min    := MinuteCreat;
    DT.Sec    := SecondCreat;
  end;
  PackTime(DT, L);
  PackedCreationDate := L;
end;

function PackedLastAccDate(P: PFileRec): LongInt;
var
  DT: DateTime;
  L: LongInt;
begin
  with P^ do
  begin
    DT.Year   := YrLAcc;
    DT.Month  := MonthLAcc;
    DT.Day    := DayLAcc;
    DT.Hour   := HourLAcc;
    DT.Min    := MinuteLAcc;
    DT.Sec    := SecondLAcc;
  end;
  PackTime(DT, L);
  PackedLastAccDate := L;
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
{$IFNDEF OS2}
  with P^ do lFindFirst(MakeNormName(Owner^, GetLFN(LFN)),
                                    Anyfile XOR VolumeID, SR);
{$ELSE}
  with P^ do lFindFirst(MakeNormName(Owner^, Name), Anyfile , SR); {JO}
{$ENDIF}
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
