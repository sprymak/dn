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

uses
  Lfn, Files,
  Objects, Memory, Startup,
  DNHelp, advance, advance1, advance2, advance3, Collect, Drivers;

type
  PDiz = ^TDIZ;
  TDIZ = record
    Owner: PString;
    DIZ: PString;
    Line: longInt;
    isDisposable: boolean;
    end;

  TShortName = String[12];
  TFlName = array[TUseLFN] of TShortName;
    {Использовано тут и в TDirRec}
  TDate4 = record
    Minute, Hour, Day, Month: byte
    end;

  PPFileRec = ^PFileRec;
  PFileRec = ^TFileRec;
  TFileRec = record
    Size: TSize;
    PSize: TSize;
    Owner: PString;
    OwnerDisposible: boolean;
    DIZ: PDiz;
    Yr: word;
    YrCreat: word;
    YrLAcc: word;
    TType: byte;
    Attr: word;
    Second: byte;
    SecondCreat: byte;
    SecondLAcc: byte;
    Selected: boolean;
    UsageCount: byte; {DataCompBoy}
    FDate, FDateCreat, FDateLAcc: longInt; {фактически - TDate4}
    FlName: TFlName;
    {см. files.pas }
    Dummy: array[1..SizeOf(ShortString)-SizeOf(TShortName)] of Char;
    {а это место, куда будет свешиваться хвост длинного имени в тех
      случаях, когда заводится локальная переменна типа TFileRec или
      при временном динамическом резервировании в стиле new(PFilerec).
      Нормально динамическое резервирование должно делаться через
      CreateFileRec или NewFileRec, где памяти резервируется ровно
      столько, сколько нужно. Из-за этого фокуса поле FlName
      обязательно должно быть в самом конце этой структуры. AK155 }
    end;
var
  pr: TFileRec;
const
  TFileRecFixedSize = SizeOf(TFileRec)
  -SizeOf(pr.Dummy)-SizeOf(pr.FlName[True])+1;

type
  TMakeListRec = record
    FileName: String; {DataCompBoy}
    Header: String;
    HeaderMode: word;
    Action: String;
    Footer: String;
    FooterMode: word;
    Options: word;
    end;

  {-DataCompBoy-}
  PUserParams = ^TUserParams;
  tUserParams = record
    active, Passive: PFileRec;
    ActiveList, PassiveList: String;
    end;
  {-DataCompBoy-}

  {Cat: выкинул, теперь используется Collect.TLineCollection}
  (*
    PLineCollection = PTextCollection;
    TLineCollection = TTextCollection;
*)

  PFilesCollection = ^TFilesCollection;
  TFilesCollection = object(TSortedCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    SortMode: byte;
    Selected: longInt;
    Owner: Pointer;
    {$IFDEF Win32}
    LFNActive: boolean;
    {$ENDIF}
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream); virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function Compare(Key1, Key2: Pointer): integer; virtual;
    end;

const
  cmlPathNames = 1;
  cmlAutoDetermine = 2;

  hfmAuto = 0;
  hfmInsertText = 1;
  hfmInsertFiles = 2;

function SelectDrive(X, Y: integer; Default: Char; IncludeTemp:
    boolean): String;
function CopyFileRec(fr: PFileRec): PFileRec; {DataCompBoy}
function CreateFileRec(Name: String): PFileRec;
{$IFNDEF OS2}
function NewFileRec(const Lfn, Name: String; Size: TSize; Date,
    CreationDate, LastAccDate: longInt; Attr: word; AOwner: PString):
  PFileRec; {DataCompBoy}
{$ELSE}
function NewFileRec(const Name: String; Size: TSize; Date,
    CreationDate, LastAccDate: longInt; Attr: word; AOwner: PString):
  PFileRec; {DataCompBoy}
{$ENDIF}
procedure DelFileRec(var fr: PFileRec); {DataCompBoy}
function LoadFileRec(var s: TStream): PFileRec; {DataCompBoy}
procedure StoreFileRec(var s: TStream; fr: PFileRec); {DataCompBoy}
function LoadFileRecOwn(var s: TStream; Dirs: PCollection): PFileRec;
  {DataCompBoy}
procedure StoreFileRecOwn(var s: TStream; fr: PFileRec; Dirs:
    PCollection); {DataCompBoy}
function PackedDate(P: PFileRec): longInt; {DataCompBoy}
function PackedCreationDate(P: PFileRec): longInt; {JO}
function PackedLastAccDate(P: PFileRec): longInt; {JO}
function GetFileType(const s: String; Attr: byte): integer;
procedure DosReread(Files: PFilesCollection);
{procedure ReplaceLongName(var fr: PFileRec; const NewName: string);}

const
  Executables: TMaskData = (Filter: 'exe;bat;com');
  {JO: дополнительные расширения для поиска в архивах}
  AddArchives: TMaskData = (Filter:
    'exe;com;@;pk[0-9];mo[0-9];tu[0-9];we[0-9];th[0-9];fr[0-9];sa[0-9];su[0-9]'
    );

implementation
uses
  DNApp, Menus, Views, FlPanel, Drives,
  Commands, Messages,
  {!!}CmdLine
  {$IFDEF MODEM} {$IFDEF LINK}, NavyLink {$ENDIF} {$ENDIF}
  {$IFDEF NETINFO}, Novell {$ENDIF}
  , VpSysLow, VPUtils
  {$IFDEF PLUGIN}, Plugin {$ENDIF}
  , FlTl, DnIni, Dos;

const
  pfrPacked = $80;
  pfrSelect = $40;

type
  TPackedFileRec = record
    Time: longInt;
    Attr: byte;
    NLen: byte;
    end;

  {-DataCompBoy-}
function CreateFileRec(Name: String): PFileRec;
  var
    fr: PFileRec;
    lSR: lSearchRec;
    l: longInt;
    D: DateTime;
    Path: String;
    FName: String;
    iLFN: TUseLFN;
  begin
    Name := lFExpand(Name);
    lFindFirst(Name, AnyFile, lSR);
    lFindClose(lSR);
    Path := GetPath(Name);
    Name := GetName(Name);
    {  l := TFileRecFixedSize+length(lsr.FullName);}
    l := SizeOf(TFileRec);
    GetMem(fr, l);
    CreateFileRec := fr;
    FillChar(fr^, l, 0);

    if DOSError = 0 then
      begin
        {$IFNDEF OS2}
        fr^.FlName[False] := lSR.SR.Name;
        {$ENDIF}
        CopyShortString(lSR.FullName, fr^.FlName[True]);
        fr^.Size := lSR.FullSize;
        fr^.PSize := lSR.FullSize;
        fr^.Owner := NewStr(Path);
        fr^.OwnerDisposible := True;
        fr^.DIZ := nil;

        UnpackTime(lSR.SR.Time, D);
        fr^.Yr := D.Year;
        with TDate4(fr^.FDate) do
          begin
            Month := D.Month;
            Day := D.Day;
            Hour := D.Hour;
            Minute := D.Min;
          end;
        fr^.Second := D.Sec;

        UnpackTime(lSR.SR.CreationTime, D); {JO}
        fr^.YrCreat := D.Year;
        with TDate4(fr^.FDateCreat) do
          begin
            Month := D.Month;
            Day := D.Day;
            Hour := D.Hour;
            Minute := D.Min;
          end;
        fr^.SecondCreat := D.Sec;

        UnpackTime(lSR.SR.LastAccessTime, D);
        fr^.YrLAcc := D.Year;
        with TDate4(fr^.FDateLAcc) do
          begin
            Month := D.Month;
            Day := D.Day;
            Hour := D.Hour;
            Minute := D.Min;
          end;
        fr^.SecondLAcc := D.Sec; {/JO}

        fr^.Attr := lSR.SR.Attr;
        if CharCount('.', lSR.FullName) = 0 then
          begin
            SetLength(lSR.FullName, Length(lSR.FullName)+1);
            lSR.FullName[Length(lSR.FullName)] := '.';
            fr^.TType := GetFileType(lSR.FullName, fr^.Attr);
            SetLength(lSR.FullName, Length(lSR.FullName)-1);
          end
        else
          fr^.TType := GetFileType(lSR.FullName, fr^.Attr);
        {Cat: это явно лишнее - для масок !\!.! для просмотрщиков и запуска по
      расширению должны передаваться неискажённые имена файлов
  PS  и следует ещё раз посмотреть и проверить - нужна ли вообще эта функция
     if fr^.Attr and Directory <> 0 then UpStr(fr^.Name) else LowStr(fr^.Name);}
        {/Cat}
      end
    else
      begin
        fr^.Owner := NewStr(Path);
        fr^.OwnerDisposible := True;
        {$IFNDEF OS2}
        fr^.FlName[False] := GetURZ(GetName(lfGetShortFileName(Name)));
        {$ENDIF}
        fr^.FlName[True] := Name;
      end;
    with fr^ do
      begin
        UsageCount := 1;
      end;
  end { CreateFileRec };
{-DataCompBoy-}

{-DataCompBoy-}
function CopyFileRec;
  begin
    CopyFileRec := fr;
    with fr^ do
      Inc(UsageCount);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure DelFileRec(var fr: PFileRec);
  begin
    if fr <> nil then
      begin
        Dec(fr^.UsageCount);
        if fr^.UsageCount = 0 then
          begin
            if (fr^.DIZ <> nil) and fr^.DIZ^.isDisposable then
              begin
                DisposeStr(fr^.DIZ^.DIZ);
                Dispose(fr^.DIZ);
              end;
            if fr^.OwnerDisposible then
              DisposeStr(fr^.Owner);
            {  FreeMem(FR, TFileRecFixedSize+length(FR^.FlName[true]));}
            FreeMem(fr, SizeOf(TFileRec));
            fr := nil;
          end
      end;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function LoadFileRec(var s: TStream): PFileRec;
  var
    P: PFileRec;
    l: byte;
    FullLen: longInt;
  begin
    s.Read(l, SizeOf(l)); {длина длинного имени}
    { FullLen := TFileRecFixedSize+l;}
    FullLen := SizeOf(TFileRec);
    GetMem(P, FullLen);
    FillChar(P^, FullLen, 0);
    with P^ do
      begin
        s.Read(Size, SizeOf(Size));
        s.Read(PSize, SizeOf(PSize));
        s.Read(Yr, SizeOf(Yr));
        s.Read(YrCreat, SizeOf(YrCreat));
        s.Read(YrLAcc, SizeOf(YrLAcc));
        s.Read(TType, SizeOf(TType));
        s.Read(Attr, SizeOf(Attr));
        s.Read(Second, SizeOf(Second));
        s.Read(SecondCreat, SizeOf(SecondCreat));
        s.Read(SecondLAcc, SizeOf(SecondLAcc));
        s.Read(Selected, SizeOf(Selected));
        s.Read(FDate, SizeOf(FDate));
        s.Read(FDateCreat, SizeOf(FDateCreat));
        s.Read(FDateLAcc, SizeOf(FDateLAcc));
        s.Read(OwnerDisposible, SizeOf(OwnerDisposible));
        if OwnerDisposible then
          Owner := s.ReadStr;
        {$IFNDEF OS2}
        s.Read(FlName[False], SizeOf(FlName[False]));
        {$ENDIF}
        s.Read(FlName[True], l+1);
        UsageCount := 1;
      end;
    LoadFileRec := P;
  end { LoadFileRec };

procedure StoreFileRec(var s: TStream; fr: PFileRec);
  var
    l: byte;
  begin
    with fr^ do
      begin
        l := Length(FlName[True]);
        s.Write(l, SizeOf(l));
        s.Write(Size, SizeOf(Size));
        s.Write(PSize, SizeOf(PSize));
        s.Write(Yr, SizeOf(Yr));
        s.Write(YrCreat, SizeOf(YrCreat));
        s.Write(YrLAcc, SizeOf(YrLAcc));
        s.Write(TType, SizeOf(TType));
        s.Write(Attr, SizeOf(Attr));
        s.Write(Second, SizeOf(Second));
        s.Write(SecondCreat, SizeOf(SecondCreat));
        s.Write(SecondLAcc, SizeOf(SecondLAcc));
        s.Write(Selected, SizeOf(Selected));
        s.Write(FDate, SizeOf(FDate));
        s.Write(FDateCreat, SizeOf(FDateCreat));
        s.Write(FDateLAcc, SizeOf(FDateLAcc));
        s.Write(OwnerDisposible, SizeOf(OwnerDisposible));
        if OwnerDisposible then
          s.WriteStr(Owner);
        {$IFNDEF OS2}
        s.Write(FlName[False], SizeOf(FlName[False]));
        {$ENDIF}
        s.Write(FlName[True], l+1);
      end
  end { StoreFileRec };
{-DataCompBoy-}

function LoadFileRecOwn(var s: TStream; Dirs: PCollection): PFileRec;
    {DataCompBoy}
  var
    fr: PFileRec;
    W: longInt;
  begin
    fr := LoadFileRec(s);
    if fr <> nil then
      if not fr^.OwnerDisposible then
        begin
          s.Read(W, SizeOf(W));
          fr^.Owner := Dirs^.At(W);
        end;
    LoadFileRecOwn := fr;
  end;

procedure StoreFileRecOwn(var s: TStream; fr: PFileRec; Dirs:
    PCollection); {DataCompBoy}
  var
    W: longInt;
  begin
    StoreFileRec(s, fr);
    if not fr^.OwnerDisposible then
      begin
        W := Dirs^.IndexOf(fr^.Owner);
        s.Write(W, SizeOf(W));
      end;
  end;

{-DataCompBoy-}
Constructor TFilesCollection.Load;
  var
    C, i: longInt;
  begin
    TObject.Init;
    s.Read(Count, SizeOf(Count));
    s.Read(Limit, SizeOf(Limit));
    s.Read(Delta, SizeOf(Delta));
    if (Count > Limit) or (Delta < 0) then
      Fail;
    C := Count;
    i := Limit;
    Count := 0;
    Limit := 0;
    SetLimit(i);
    for i := 0 to C-1 do
      begin
        AtInsert(i, LoadFileRec(s));
        if (s.Status <> stOK) then
          begin
            SetLimit(0);
            Fail;
          end;
      end;
    s.Read(Selected, SizeOf(Selected));
  end { TFilesCollection.Load };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFilesCollection.Store;
  var
    i, j, Sel: longInt;
  begin
    j := 0;
    for i := 1 to Count do
      if (i-1 = Selected) or (PFileRec(At(i-1))^.Selected) then
        begin
          if i-1 = Selected then
            Sel := j;
          Inc(j)
        end;
    i := Count;
    Count := j;
    s.Write(Count, SizeOf(Count));
    s.Write(Limit, SizeOf(Limit));
    s.Write(Delta, SizeOf(Delta));
    Count := i;
    for i := 1 to Count do
      if (i-1 = Selected) or (PFileRec(At(i-1))^.Selected) then
        StoreFileRec(s, At(i-1));
    s.Write(Sel, SizeOf(Sel));
  end { TFilesCollection.Store };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFilesCollection.FreeItem;
  var
    P: PFileRec absolute Item;
  begin
    DelFileRec(P);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TFilesCollection.Compare;
  var{T1: TFileRec;}
    P1: PFileRec absolute Key1;
    P2: PFileRec absolute Key2;
    C: integer;
    SM, I1, I2: integer;
    P1P, P2P: boolean;
    PanelFlags: word;
    {OneIsFile: Boolean;}
    ST1, ST2: String;

    {$IFNDEF OS_DOS}
  function CmpName(const s1: String; const s2: String;
    const owner1: String; const owner2: String): integer;
    begin
      if s1 > s2 then
        CmpName := +1
      else if s1 < s2 then
        CmpName := -1
        // JO: в чём смысл этого условия с #22 непонятно, но оно приводит к
        //     несходимости QuickSort и зависанию/падениям при наличии NoShortName
        //     в панели при компиляции с дефайном QSort
        {else if S1[1] = #22 then CmpName := +1}
      else if owner1 > owner2 then
        CmpName := +1
      else if owner1 < owner2 then
        CmpName := -1
      else
        CmpName := 0;
    end;

  function CmpEXT(s1, s2: String): integer;
    var
      E1, E2: PString;
      B: byte;
    const
      D1: Char = #0;
    begin
      for B := Length(s1) downto 1 do
        if s1[B] = '.' then
          break;
      if (B > 1) or (s1[1] = '.') then
        begin
          E1 := @S1[B];
          SetLength(E1^, Length(s1)-B);
          SetLength(s1, B-1);
        end
      else
        E1 := @D1;

      for B := Length(s2) downto 1 do
        if s2[B] = '.' then
          break;
      if (B > 1) or (s2[1] = '.') then
        begin
          E2 := @S2[B];
          SetLength(E2^, Length(s2)-B);
          SetLength(s2, B-1);
        end
      else
        E2 := @D1;

      if E1^ > E2^ then
        CmpEXT := +1
      else if E1^ < E2^ then
        CmpEXT := -1
      else if s1 > s2 then
        CmpEXT := +1
      else if s1 < s2 then
        CmpEXT := -1
      else
        CmpEXT := 0;
    end { CmpEXT };

    {$ELSE}

  function CmpLFN(LFN1, LFN2: TLFNIndex): integer;
    var
      s1, s2: String;
    begin
      begin
        s1 := GetLFN(LFN1);
        s2 := GetLFN(LFN2);
        UpLowStr(s1);
        UpLowStr(s2); {JO}
        if s1 > s2 then
          CmpLFN := +1
        else if s1 < s2 then
          CmpLFN := -1
        else
          CmpLFN := 0;
      end;
    end;

  function CmpLEX(LFN1, LFN2: TLFNIndex): integer;
    var
      s1, s2: String;
      E1, E2: String;
      B, k: byte;
    begin
      s1 := UpLowStrg(GetLFN(LFN1)); {JO}
      for B := Length(s1) downto 1 do
        if s1[B] = '.' then
          break;
      if (B > 1) or (s1[1] = '.') then
        begin
          k := Length(s1)-B+1;
          Move(s1[B], E1[1], k);
          SetLength(E1, k);
          SetLength(s1, B-1);
        end
      else
        word(((@E1)^)) := $2E01; {E1 := '.'}
      s2 := UpLowStrg(GetLFN(LFN2)); {JO}
      for B := Length(s2) downto 1 do
        if s2[B] = '.' then
          break;
      if (B > 1) or (s2[1] = '.') then
        begin
          k := Length(s2)-B+1;
          Move(s2[B], E2[1], k);
          SetLength(E2, k);
          SetLength(s2, B-1);
        end
      else
        word(((@E2)^)) := $2E01; {E2 := '.'}
      if E1 > E2 then
        CmpLEX := +1
      else if E1 < E2 then
        CmpLEX := -1
      else if s1 > s2 then
        CmpLEX := +1
      else if s1 < s2 then
        CmpLEX := -1
      else
        CmpLEX := 0;
    end { CmpLEX };

  {$ENDIF}

  var
    C1: integer; { результат сравнения имён   // AK155}
    Name1, Name2: String; {UpStrg(P2^.Name)  // AK155}

  begin { TFilesCollection.Compare }
    if Owner <> nil then
      PanelFlags := PFilePanel(Owner)^.PanelFlags
    else
      PanelFlags := PanelDefaults.Show;
    {Move(Key1^,T1,SizeOf(T1));}
    with P2^ do
      begin
        P1P := P1^.TType = ttUpDir;
        P2P := TType = ttUpDir;
        if P1P and not P2P then
          begin
            Compare := -1;
            exit;
          end;
        if P2P and not P1P then
          begin
            Compare := +1;
            exit;
          end;

        Name1 := P1^.FlName[uLfn];
        Name2 := FlName[uLfn];
        {$IFDEF WIN32}
        {$IFDEF RecodeWhenDraw}
        CharToOemSt(Name1);
        CharToOemSt(Name2);
        {$ENDIF}
        {$ENDIF}
        if UpperCaseSorting then
          begin
            UpStr(Name1);
            UpStr(Name2);
          end
        else
          begin
            LowStr(Name1);
            LowStr(Name2);
          end;

        SM := SortMode;

        if (SM >= NumSortModes) and (SM <= NumSortModes+31) then
            begin{ условие используется для сравнения }
            {  каталогов по Ctrl-C }

            {здесь C1 используется для сравнения каталогов по Ctrl-C}

            if ((SM-NumSortModes) and 16 = 0)
              {$IFDEF Win32}
              {JO: регистрочувствительное сравнение по коротким именам лишено смысла}
              or (not uLfn)
              {$ENDIF}
            then
              begin
                if Name1 > Name2 then
                  C1 := 1
                else if Name1 < Name2 then
                  C1 := -1
                else
                  C1 := 0;
              end
            else
              begin
                if P1^.FlName[True] < FlName[True] then
                  C1 := -1
                else if P1^.FlName[True] = FlName[True] then
                  C1 := 0
                else
                  C1 := 1;
              end;

            if (C1 = 0) and ((P1^.Attr or Attr) and Directory = 0)
                and
              (((SM-NumSortModes) and 1 = 0) or (P1^.Size = Size))
                and
              (((SM-NumSortModes) and 2 = 0) or (P1^.Yr < Yr) or
              ((P1^.Yr = Yr) and (P1^.FDate <= FDate)) or
              ((P1^.Yr = Yr) and (P1^.FDate = FDate) and (P1^.Second <
                Second))) and
              (((SM-NumSortModes) and 4 = 0) or (P1^.Attr = Attr))
                and
              (((SM-NumSortModes) and 8 = 0) or
              CompareFiles(MakeNormName(P1^.Owner^, Name1),
              MakeNormName(Owner^, Name2)))
            then
              C := 0
            else if (C1 = -1) then
              C := -1
            else
              C := 1;
            Compare := C;
            exit;
          end; { конец куска для сравнения каталогов по Ctrl-C }
        { дальше идёт кусок для сортировки файлов в панелях }

        {UpLowStr(P1^.Name);} {JO}

        {далее C1 используется для сортировки файлов в панелях}
        if not (SM in [psmLongName, psmLongExt]) then
          begin
            if Name1 < Name2 then
              C1 := -1
            else if Name1 = Name2 then
              C1 := 0
            else
              C1 := 1;
          end;

        if (Owner <> nil) and not (SM in [psmSize, psmTime
            {$IFNDEF DPMI32}, psmCrTime, psmLATime {$ENDIF}])
        then
          if PanelFlags and fmiExeBeforeArc <> 0 then
            begin
              if PanelFlags and fmiExeFirst <> 0 then
                begin
                  if ((P1^.Attr xor Attr) and Directory = 0) then
                    begin
                      if (P1^.TType = ttExec) and (TType <> ttExec)
                      then
                        begin
                          Compare := -1;
                          exit;
                        end;
                      if (P1^.TType <> ttExec) and (TType = ttExec)
                      then
                        begin
                          Compare := 1;
                          exit;
                        end;
                    end;
                end;
              if PanelFlags and fmiArchivesFirst <> 0 then
                begin
                  if ((P1^.Attr xor Attr) and Directory = 0) then
                    begin
                      if (P1^.TType = ttArc) and (TType <> ttArc)
                      then
                        begin
                          Compare := -1;
                          exit;
                        end;
                      if (P1^.TType <> ttArc) and (TType = ttArc)
                      then
                        begin
                          Compare := 1;
                          exit;
                        end;
                    end;
                end;
            end
          else
            begin
              if PanelFlags and fmiArchivesFirst <> 0 then
                begin
                  if ((P1^.Attr xor Attr) and Directory = 0) then
                    begin
                      if (P1^.TType = ttArc) and (TType <> ttArc)
                      then
                        begin
                          Compare := -1;
                          exit;
                        end;
                      if (P1^.TType <> ttArc) and (TType = ttArc)
                      then
                        begin
                          Compare := 1;
                          exit;
                        end;
                    end;
                end;
              if PanelFlags and fmiExeFirst <> 0 then
                begin
                  if ((P1^.Attr xor Attr) and Directory = 0) then
                    begin
                      if (P1^.TType = ttExec) and (TType <> ttExec)
                      then
                        begin
                          Compare := -1;
                          exit;
                        end;
                      if (P1^.TType <> ttExec) and (TType = ttExec)
                      then
                        begin
                          Compare := 1;
                          exit;
                        end;
                    end;
                end;
            end;

        case SM of
          psmUnsorted:
            if P1^.Owner^ = Owner^
            then
              if C1 = 0 then
                C := 0
              else
                C := -1
            else if P1^.Owner^ < Owner^ then
              C := -1
            else
              C := 1;
          psmOrdered:
            begin
              I1 := P1^.TType;
              I2 := TType;
              if I1 = 0 then
                I1 := 100;
              if I2 = 0 then
                I2 := 100;
              if I1 < I2 then
                C := -1
              else if I1 > I2 then
                C := 1
              else if C1 = -1 then
                C := -1
              else if C1 = 1 then
                C := 1
              else
                C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
            end;
          {LFN}psmLongName:
            if (P1^.Attr xor Attr) and Directory <> 0 then
              if P1^.Attr and Directory <> 0 then
                C := -1
              else
                C := 1
            else
              C := CmpName(Name1, Name2, P1^.Owner^, Owner^);

          141:
            if (P1^.Attr xor Attr) and Directory <> 0 then
              if P1^.Attr and Directory <> 0 then
                C := -1
              else
                C := 1
            else if Name1 < Name2 then
              C := -1
            else if (Name1 > Name2) then
              C := 1
            else
              C := 0;

          {LEXT}psmLongExt:
            if (P1^.Attr xor Attr) and Directory <> 0 then
              if P1^.Attr and Directory <> 0 then
                C := -1
              else
                C := 1
            else
              begin
                C := CmpEXT(Name1, Name2);
                if C = 0 then
                  C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
              end;
          {SIZE}psmSize:
            if (P1^.Attr xor Attr) and Directory <> 0 then
              if P1^.Attr and Directory <> 0 then
                C := -1
              else
                C := 1
            else if P1^.Size > Size then
              C := -1
            else if P1^.Size < Size then
              C := 1
            else if C1 = -1 then
              C := -1
            else if C1 = 1 then
              C := 1
            else
              C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
          {DATE}psmTime:
            begin
              if (P1^.Attr xor Attr) and Directory <> 0 then
                if P1^.Attr and Directory <> 0 then
                  C := -1
                else
                  C := 1
              else if (P1^.Yr < Yr) then
                C := 1
              else if (P1^.Yr > Yr) then
                C := -1
              else if (P1^.FDate < FDate) then
                C := 1
              else if (P1^.FDate > FDate) then
                C := -1
              else if (P1^.Second < Second) then
                C := 1
              else if (P1^.Second > Second) then
                C := -1
              else if C1 = -1 then
                C := -1
              else if C1 = 1 then
                C := 1
              else
                C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
            end;
          {JO}
          {$IFNDEF OS_DOS}
          {$IFNDEF DPMI32}
          {Creat.DATE}psmCrTime:
            begin
              if (P1^.Attr xor Attr) and Directory <> 0 then
                if P1^.Attr and Directory <> 0 then
                  C := -1
                else
                  C := 1
              else if (P1^.YrCreat < YrCreat) then
                C := 1
              else if (P1^.YrCreat > YrCreat) then
                C := -1
              else if (P1^.FDateCreat < FDateCreat) then
                C := 1
              else if (P1^.FDateCreat > FDateCreat) then
                C := -1
              else if (P1^.SecondCreat < SecondCreat) then
                C := 1
              else if (P1^.SecondCreat > SecondCreat) then
                C := -1
              else if C1 = -1 then
                C := -1
              else if C1 = 1 then
                C := 1
              else
                C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
            end;
          {L.Acc.DATE}psmLATime:
            begin
              if (P1^.Attr xor Attr) and Directory <> 0 then
                if P1^.Attr and Directory <> 0 then
                  C := -1
                else
                  C := 1
              else if (P1^.YrLAcc < YrLAcc) then
                C := 1
              else if (P1^.YrLAcc > YrLAcc) then
                C := -1
              else if (P1^.FDateLAcc < FDateLAcc) then
                C := 1
              else if (P1^.FDateLAcc > FDateLAcc) then
                C := -1
              else if (P1^.SecondLAcc < SecondLAcc) then
                C := 1
              else if (P1^.SecondLAcc > SecondLAcc) then
                C := -1
              else if C1 = -1 then
                C := -1
              else if C1 = 1 then
                C := 1
              else
                C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
            end;
          {$ENDIF}
          {$ENDIF OS_DOS}

          {DIZ}psmDIZ:
            if (P1^.Attr xor Attr) and Directory <> 0 then
              if P1^.Attr and Directory <> 0 then
                C := -1
              else
                C := 1
            else
              begin
                if (P1^.DIZ <> nil) and (P1^.DIZ^.DIZ <> nil) then
                    ST1 := UpLowStrg(CnvString(P1^.DIZ^.DIZ))
                else
                  ST1 := '';
                if (P2^.DIZ <> nil) and (P2^.DIZ^.DIZ <> nil) then
                    ST2 := UpLowStrg(CnvString(DIZ^.DIZ))
                else
                  ST2 := '';
                if (ST1 <> '') and (ST2 = '') then
                  C := -1
                else if (ST1 = '') and (ST2 <> '') then
                  C := 1
                else
                  {C := CmpName(ST1, ST2);}
                  begin
                    if ST1 > ST2 then
                      C := 1
                    else if ST1 < ST2 then
                      C := -1
                    else
                      C := 0;
                  end;
                if C = 0 then
                  C := C1;
                if C = 0 then
                  if P1^.Owner^ < Owner^ then
                    C := -1
                  else if P1^.Owner^ > Owner^ then
                    C := 1;
              end;
          {/JO}
          else
            if (P1^.Attr and $3F = Attr and $3F) and (P1^.Yr = Yr)
                and (P1^.FDate = FDate) and (P1^.Second = Second)
              and (C1 = 0) and (P1^.Owner^ = Owner^)
                //! {$IFNDEF OS2} and (GetLFN(P1^.LFN)=GetLFN(LFN)) {$ENDIF}
            then
              C := 0
            else
              C := -1;
        end {case};
      end;
    Compare := C;
  end { TFilesCollection.Compare };

{-DataCompBoy-}

function SelectDrive;
  var
    R: TRect;
    P: PMenuBox;
    Menu: PMenu;
    Items, Lnk: PMenuItem;
    C: Char;
    n, MaxRY: integer;
    Sc: TCharSet;
    Server_Num, Handle_Num, RetCode, DriveNum, MaxL: integer;
      {-$VIV start}
    FullS, Server, PathName, TmpS: String; {-$VIV end}
    pSaveNeedAbort, ShowDir: boolean;
    DrInvalid: boolean;

  function CutLongString(s: String): String;
    var
      P, l: integer;
      s1: String;
    begin
      CutLongString := #0+s+#0;
      if not CutDriveInfo then
        exit;
      (* X-Man *)
      CutLongString := FormatLongName(s, 30, 0, flnPreferName+
        flnHighlight+
      flnUseCutChar+flnHandleTildes, nfmNull, nil)
    end;

  var
    IDDQD: record
      Fl, dr: longInt;
      end;

  procedure GetInfo(P: PFileRec);
    begin
      if P <> nil then
        if P^.Attr and Directory = 0
        then
          Inc(IDDQD.Fl)
        else
          Inc(IDDQD.dr)
    end;

  var
    DT: TDriveType;
  begin { SelectDrive }
    Items := nil;
    n := 0;
    Lnk := nil;
    MaxL := 8; {-$VIV}

    {Cat}
    {$IFDEF PLUGIN}
    if IncludeTemp then
      Inc(n, CreateDriveMenus(Items, MaxL));
    {$ENDIF}
    {/Cat}

    if IncludeTemp and (InterfaceData.DrvInfType and ditIncludeQick <> 0
        )
    then
      begin
        C := ' ';
        for DriveNum := 8 downto 0 do
          if DirsToChange[DriveNum] <> nil then
            begin
              FreeStr := '~'+ItoS(DriveNum+1)+':~ '+CutH(
                DirsToChange[DriveNum]^, 24);
              Items := NewItem(FreeStr, 'Alt-'+ItoS(DriveNum+1),
                kbNoKey,
              cmQuickChange1+DriveNum, hcNoContext, Items);
              MaxL := Max(CStrLen(FreeStr), MaxL);
              Inc(n);
              C := '!';
            end;
        if C = '!' then
          Items := NewLine(Items);
      end;

    {$IFDEF MODEM}
    {$IFDEF LINK}
    if IncludeTemp and (Linker <> nil) then
      begin
        CL_GetLinkDrives(Sc);
        if Sc <> [] then
          begin
            for C := 'Z'downto'A' do
              if C in Sc then
                Items := NewItem('~'+C+':~ ', '', kbNoKey, 2000+byte(C),
                hcNoContext, Items);
            Items := NewSubMenu('~+:~ LINK', hcNoContext, NewMenu(
              Items), nil);
            Lnk := Items;
            Inc(n)
          end;
      end;
    {$ENDIF}
    {$ENDIF}

    if IncludeTemp then
      begin
        if (InterfaceData.DrvInfType and ditIncludeTempInfo <> 0)
        then
          if TempFiles <> nil then
            begin
              IDDQD.dr := 0;
              IDDQD.Fl := 0;
              TempFiles^.ForEach(@GetInfo);
              if IDDQD.dr+IDDQD.Fl = 0 then
                FreeStr := GetString(dlEmpty)
              else if IDDQD.dr = 0 then
                FreeStr := ItoS(IDDQD.Fl)+' '+#0+GetString(dlDIFiles)+#0
              else if IDDQD.Fl = 0 then
                FreeStr := ItoS(IDDQD.dr)+' '+#0+GetString(
                  dlDirectories)+#0
              else
                FormatStr(FreeStr, GetString(dlFilDir), IDDQD);
            end
          else
            FreeStr := GetString(dlEmpty)
        else
          FreeStr := '';

        Items := NewItem('~*:~ TEMP: '+FreeStr, '', kbSpace, 1200,
          hcTempList, Items);
        Items := NewLine(Items);
        Inc(n)
      end;

    for C := 'Z'downto'A' do
      if ValidDrive(C) then
        begin
          FullS := '~'+C+':~ '; {-$VIV start}
          if (InterfaceData.Options and ouiHideDriveInfo = 0) then
            begin
              DrInvalid := False;
              DriveNum := byte(C)-64;
              if (InterfaceData.DrvInfType and ditMediaType <> 0)
              then
                if not ((InterfaceData.DrvInfType and ditSkipFloppy <> 0
                    ) and (DriveNum <= 2))
                then
                  begin
                    {$IFNDEF OS2}
                    DT := SysGetDriveType(C);
                    case SysGetDriveType(C) of
                      dtFloppy:
                        FullS := FullS+GetString(sdtRemovable)+
                          '    ';
                      dtHDFAT:
                        FullS := FullS+GetString(sdtFixed)+' FAT  '+
                          GetString(sdtFixed1);
                      dtHDHPFS:
                        FullS := FullS+GetString(sdtFixed)+' HPFS '+
                          GetString(sdtFixed1);
                      dtInvalid:
                        begin
                          FullS := FullS+GetString(sdtError)+'    ';
                            DrInvalid := True;
                        end;
                      dtNovellNet:
                        FullS := FullS+' Novell     ';
                      dtCDRom:
                        FullS := FullS+' CD-ROM     ';
                      dtLAN:
                        FullS := FullS+' Lan Server ';
                      dtHDNTFS:
                        FullS := FullS+GetString(sdtFixed)+' NTFS '+
                          GetString(sdtFixed1);
                      dtTVFS:
                        FullS := FullS+'TVFS       ';
                      dtHDExt2:
                        FullS := FullS+GetString(sdtFixed)+' ext2 '+
                          GetString(sdtFixed1);
                      dtJFS:
                        FullS := FullS+GetString(sdtFixed)+' JFS  '+
                          GetString(sdtFixed1);
                    end {case};
                    {X-Man <<<}
                    {$ELSE}
                    FullS := FullS+GetDriveTypeString(C)+' ';
                    {$ENDIF}
                  end
                else if DriveNum <= 2 then
                  FullS := FullS+GetString(sdtRemovable)+'    ';
              {$IFDEF WIN32}
              if (InterfaceData.DrvInfType and ditShowVolume <> 0)
                  and not DrInvalid and
                not ((InterfaceData.DrvInfType and ditSkipFloppy <> 0)
                  and (DriveNum <= 2))
              then
                FullS := FullS+GetVolumeLabel(C);
              {$ELSE}
              if (InterfaceData.DrvInfType and ditShowVolume <> 0)
                  and not DrInvalid and
                not ((InterfaceData.DrvInfType and ditSkipFloppy <> 0)
                  and (DriveNum <= 2))
              then
                FullS := FullS+GetVolumeLabel(C);
              {$ENDIF}
              {$IFDEF NETINFO}
              Get_drive_connection_id(DriveNum, Server_Num);
              if (Server_Num > 0) and (InterfaceData.DrvInfType and
                  ditNetDir <> 0)
              then
                begin
                  Get_File_Server_Name(Server_Num, Server);
                  Set_Preferred_Connection_Id(Server_Num);
                  Get_Drive_Handle_Id(DriveNum, Handle_Num);
                  Get_Directory_Path(Handle_Num, PathName, RetCode);
                  lGetDir(DriveNum, TmpS); {DataCompBoy}
                  if Length(TmpS) > 3 then
                    begin
                      TmpS := ' <'+Copy(PathName, Length(PathName)-
                        Length(TmpS)+4, MaxStringLength)+'>';
                      Delete(PathName, Length(PathName)-Length(TmpS)+4,
                        MaxStringLength);
                      if PathName[Length(PathName)] = '/' then
                        Delete(PathName, Length(PathName), 1);
                    end
                  else
                    TmpS := '';
                  PathName := CutLongString(Server+'/'+PathName);
                  PathName := PathName+TmpS;
                  FullS := FullS+PathName;
                end
              else
                {$ENDIF}
                begin
                  if (DriveNum <= 2) or ((InterfaceData.DrvInfType
                      and ditCDdir = 0)
                    and (InterfaceData.DrvInfType and ditLocalDir = 0))
                  then
                    ShowDir := False
                  else if (InterfaceData.DrvInfType and ditCDdir <> 0)
                    and (InterfaceData.DrvInfType and ditLocalDir <> 0)
                  then
                    ShowDir := True
                  else if (InterfaceData.DrvInfType and ditLocalDir <> 0
                      )
                  then
                    ShowDir := not IsDriveCDROM(Char(DriveNum+64))
                  else
                    ShowDir := IsDriveCDROM(Char(DriveNum+64));
                  if ShowDir then
                    begin
                      pSaveNeedAbort := NeedAbort;
                      NeedAbort := True;
                      lGetDir(DriveNum, PathName); {DataCompBoy}
                      if IOResult = 0 then
                        begin
                          if Length(PathName) = 3 then
                            PathName := ''
                          else
                            begin
                              Delete(PathName, 1, 3);
                              PathName := '<'+CutLongString(PathName)+
                                '>';
                            end;
                          FullS := FullS+PathName;
                        end;
                      NeedAbort := pSaveNeedAbort;
                    end
                end;
              if Length(FullS) > MaxL then
                MaxL := Length(FullS);
            end; {-$VIV end}

          Items := NewItem(FullS, '', kbNoKey, 1000+byte(C),
          hcNoContext, Items);
          Inc(n);
        end;
    if not (Default in ['A'..'Z']) and not ((Default = '+') and (Lnk <>
        nil))
    then
      C := GetCurDrive
    else
      C := Default;
    Menu := NewMenu(Items);
    Desktop^.GetExtent(R);
    {-$VIV start}
    X := X-(MaxL div 2);
    if (X+MaxL+4) > R.B.X then
      X := R.B.X-MaxL-4;
    if (X < 0) then
      X := 0;
    Y := Y-(n div 2)+1;
    if (Y+n+2) > R.B.Y then
      Y := R.B.Y-n-2;
    if (Y < 0) then
      Y := 0;
    R.A.X := X;
    R.A.Y := Y;
    R.B.X := R.A.X+MaxL+4;
    MaxRY := R.B.Y;
    R.B.Y := R.A.Y+n+2;
    if R.A.Y = 0 then
      begin
        Inc(R.A.Y);
        Inc(R.B.Y)
      end;
    if (R.B.Y > MaxRY) then
      R.B.Y := MaxRY;
    {-$VIV end}
    P := New(PMenuBox, Init(R, Menu, nil)); {-$VIV}
    if (C = '+') then
      Items := Lnk
    else
      Items := P^.FindItem(C);
    if Items <> nil then
      Menu^.Default := Items;
    P^.HelpCtx := hcSelectDrive+byte(IncludeTemp = True);

    n := Desktop^.ExecView(P);
    Dispose(P, Done);
    DisposeMenu(Menu);
    SelectDrive := '';
    if n > 1000 then
      SelectDrive := Char(n-1000)+':';
    if n = 1200 then
      SelectDrive := cTEMP_;
    if n > 2000 then
      SelectDrive := '+'+Char(n-2000);
    {$IFDEF PLUGIN}
    if n > 65000 then
      SelectDrive := #26+Char(n-65000); {Cat}
    {$ENDIF}
    if (n >= cmQuickChange1) and (n <= cmQuickChange9)
    then
      SelectDrive := CnvString(DirsToChange[n-cmQuickChange1]);
  end { SelectDrive };

function GetFileType(const s: String; Attr: byte): integer;
  begin
    if s = '..' then
      GetFileType := ttUpDir
    else{AK155} if Attr and Directory <> 0 then
      GetFileType := ttDirectory
    else
      begin
        GetFileType := 0;
        if InExtFilter(s, Executables) then
          GetFileType := ttExec
        else if InExtFilter(s, Archives^) then
          GetFileType := ttArc
        else if InExtFilter(s, CustomMask1^) then
          GetFileType := ttCust1
        else if InExtFilter(s, CustomMask2^) then
          GetFileType := ttCust2
        else if InExtFilter(s, CustomMask3^) then
          GetFileType := ttCust3
        else if InExtFilter(s, CustomMask4^) then
          GetFileType := ttCust4
        else if InExtFilter(s, CustomMask5^) then
          GetFileType := ttCust5
          {JO}
        else if InExtFilter(s, CustomMask6^) then
          GetFileType := ttCust6
        else if InExtFilter(s, CustomMask7^) then
          GetFileType := ttCust7
        else if InExtFilter(s, CustomMask8^) then
          GetFileType := ttCust8
        else if InExtFilter(s, CustomMask9^) then
          GetFileType := ttCust9
          {JO}
        else if InExtFilter(s, CustomMask10^) then
          GetFileType := ttCust10
          (*
{JO} {работает, но имхо притормаживает}
    else if InFilter(S, Archives   ^.Filter)  then GetFileType := ttArc
    else if InFilter(S, CustomMask1^.Filter)  then GetFileType := ttCust1
    else if InFilter(S, CustomMask2^.Filter)  then GetFileType := ttCust2
    else if InFilter(S, CustomMask3^.Filter)  then GetFileType := ttCust3
    else if InFilter(S, CustomMask4^.Filter)  then GetFileType := ttCust4
    else if InFilter(S, CustomMask5^.Filter)  then GetFileType := ttCust5
    else if InFilter(S, CustomMask6^.Filter)  then GetFileType := ttCust6
    else if InFilter(S, CustomMask7^.Filter)  then GetFileType := ttCust7
    else if InFilter(S, CustomMask8^.Filter)  then GetFileType := ttCust8
    else if InFilter(S, CustomMask9^.Filter)  then GetFileType := ttCust9
    else if InFilter(S, CustomMask10^.Filter) then GetFileType := ttCust10
{/JO}
*)
      end;
  end { GetFileType };

{-DataCompBoy-}
function NewFileRec;
  var
    pr: PFileRec;
    t: TFileRec;
    D: DateTime;
    {$IFNDEF OS2}
    Name12: Str12;
    {$ENDIF}
    l: longInt;
  begin
    {$IFNDEF OS2}
    t.FlName[False] := Name;
    if Attr and Directory <> 0 then
      UpStr(t.FlName[False])
    else
      LowStr(t.FlName[False]);
    (*!  case Attr and (Hidden+SysFile) of                  {Pavel Anufrikov -> }
   Hidden  :        T.Name[9] := NameFormatChar[nfmHidden];
   SysFile :        T.Name[9] := NameFormatChar[nfmSystem];
   Hidden+SysFile : T.Name[9] := NameFormatChar[nfmHiddenSystem];
  end{case};                                         { <- Pavel Anufrikov}
!*)
    if Attr and SysFile <> 0 then
      t.FlName[False][1] := UpCase(t.FlName[False][1]);
    CopyShortString(Lfn, t.FlName[True]);
    {$ELSE}
    CopyShortString(Name, t.FlName[True]);
    {$ENDIF}
    t.Size := Size;
    t.PSize := Size;
    if Date = 0 then
      begin
        t.Yr := 1980;
        t.FDate := $1111;
        t.Second := 1;
      end
    else
      begin
        UnpackTime(Date, D);
        t.Yr := D.Year;
        with TDate4(t.FDate) do
          begin
            Month := D.Month;
            Day := D.Day;
            Hour := D.Hour;
            Minute := D.Min;
          end;
        t.Second := D.Sec;
      end;
    if CreationDate = 0 then
      begin
        t.YrCreat := 0;
        t.FDateCreat := $0000;
        t.SecondCreat := 0;
      end
    else
      begin
        UnpackTime(CreationDate, D);
        t.YrCreat := D.Year;
        with TDate4(t.FDateCreat) do
          begin
            Month := D.Month;
            Day := D.Day;
            Hour := D.Hour;
            Minute := D.Min;
          end;
        t.SecondCreat := D.Sec;
      end;
    if LastAccDate = 0 then
      begin
        t.YrLAcc := 0;
        t.FDateLAcc := $0000;
        t.SecondLAcc := 0;
      end
    else
      begin
        UnpackTime(LastAccDate, D);
        t.YrLAcc := D.Year;
        with TDate4(t.FDateLAcc) do
          begin
            Month := D.Month;
            Day := D.Day;
            Hour := D.Hour;
            Minute := D.Min;
          end;
        t.SecondLAcc := D.Sec;
      end;
    t.Attr := Attr and $7FFF;
    t.Selected := False;
    t.DIZ := nil;
    t.TType := GetFileType(t.FlName[True], t.Attr);
    t.Owner := AOwner;
    t.OwnerDisposible := False;
    t.UsageCount := 1;
    {  l := TFileRecFixedSize+length(T.FlName[true]);}
    l := SizeOf(TFileRec);
    GetMem(pr, l);
    Move(t, pr^, l);
    NewFileRec := pr;
  end { NewFileRec };
{-DataCompBoy-}

function PackedDate(P: PFileRec): longInt;
  var
    DT: DateTime;
    l: longInt;
  begin
    with P^ do
      begin
        DT.Year := Yr;
        with TDate4(FDate) do
          begin
            DT.Month := Month;
            DT.Day := Day;
            DT.Hour := Hour;
            DT.Min := Minute;
          end;
        DT.Sec := Second;
      end;
    PackTime(DT, l);
    PackedDate := l;
  end;

function PackedCreationDate(P: PFileRec): longInt;
  var
    DT: DateTime;
    l: longInt;
  begin
    with P^ do
      begin
        DT.Year := YrCreat;
        with TDate4(FDateCreat) do
          begin
            DT.Month := Month;
            DT.Day := Day;
            DT.Hour := Hour;
            DT.Min := Minute;
          end;
        DT.Sec := SecondCreat;
      end;
    PackTime(DT, l);
    PackedCreationDate := l;
  end;

function PackedLastAccDate(P: PFileRec): longInt;
  var
    DT: DateTime;
    l: longInt;
  begin
    with P^ do
      begin
        DT.Year := YrLAcc;
        with TDate4(FDateLAcc) do
          begin
            DT.Month := Month;
            DT.Day := Day;
            DT.Hour := Hour;
            DT.Min := Minute;
          end;
        DT.Sec := SecondLAcc;
      end;
    PackTime(DT, l);
    PackedLastAccDate := l;
  end;

procedure DosReread(Files: PFilesCollection);
  var
    i: longInt;
    SR: lSearchRec;
    P: PFileRec;
    D: DateTime;
    s: String;
  begin
    if Files = nil then
      exit;
    i := 0;
    while i < Files^.Count do
      begin
        ClrIO;
        P := Files^.At(i);
        {JO: проверяем, не лежит ли файл в архиве в панели поиска}
        if not PathFoundInArc(P^.Owner^) then
          begin
            with P^ do
              lFindFirst(MakeNormName(Owner^, FlName[True]), AnyFile, SR
                ); {JO}
            lFindClose(SR);
            if (DOSError <> 0) or Abort then
              Files^.AtFree(i)
            else
              begin
                P^.Size := SR.FullSize;
                UnpackTime(SR.SR.Time, D);
                P^.Yr := D.Year;
                with TDate4(P^.FDate) do
                  begin
                    Month := D.Month;
                    Day := D.Day;
                    Hour := D.Hour;
                    Minute := D.Min;
                  end;
                P^.Second := D.Sec;
                P^.Attr := SR.SR.Attr;
                {JO: чтобы менялся цвет в панели поиска и временной панели при переименовании}
                P^.TType := GetFileType(P^.FlName[True], P^.Attr);
                Inc(i);
              end;
          end
        else
          begin
            {JO: проверяем, существует ли архив, в котором лежит файл }
            s := P^.Owner^;
            if Length(s) > 1 then
              s[2] := ';';
              {JO: реально не важно, какой символ взять, лишь бы не ':'}
            s := Copy(P^.Owner^, 1, PosChar(':', s)-1);
            lFindFirst(s, AnyFile, SR);
            lFindClose(SR);
            if (DOSError <> 0) or Abort then
              Files^.AtFree(i)
            else
              Inc(i);
          end;
      end;
    if Files^.Count > 0 then
      Files^.Sort;
  end { DosReread };
end.
