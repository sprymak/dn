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
  LFN, files,
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

    TShortName = String[12];
    TFlName = array[TUseLFN] of TShortName; {Использовано тут и в TDirRec}
    TDate4 = record Minute, Hour, Day, Month: byte end;

    PPFileRec = ^PFileRec;
    PFileRec = ^TFileRec;
    TFileRec = record
     Size: TSize;
     PSize: TSize;
     Owner: PString;
     OwnerDisposible: Boolean;
     Diz: PDIZ;
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
     FDate, FDateCreat, FDateLAcc: longint; {фактически - TDate4}
     FlName: TFlName;
      {см. files.pas }
     dummy: array [1..SizeOf(ShortString)-SizeOf(TShortName)] of char;
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
    -SizeOf(pr.Dummy)-SizeOf(pr.FlName[true])+1;

type
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

{Cat: выкинул, теперь используется Collect.TLineCollection}
(*
    PLineCollection = PTextCollection;
    TLineCollection = TTextCollection;
*)

    PFilesCollection = ^TFilesCollection;
    TFilesCollection = object(TSortedCollection)
  {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
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
{procedure ReplaceLongName(var fr: PFileRec; const NewName: string);}


 const Executables: TMaskData = (Filter: 'exe;bat;com');
      {JO: дополнительные расширения для поиска в архивах}
       AddArchives: TMaskData = (Filter: 'exe;com;@;pk[0-9];mo[0-9];tu[0-9];we[0-9];th[0-9];fr[0-9];sa[0-9];su[0-9]');

implementation
uses DNApp, Menus, Views, FlPanel, Drives,
     Commands, Messages,
     {!!}CmdLine
     {$IFDEF MODEM}{$IFDEF LINK},NavyLink{$ENDIF}{$ENDIF}
     {$IFDEF NETINFO}, Novell{$ENDIF}
     {$IFNDEF NONBP}, DiskTool{$ENDIF}
     {$IFDEF VIRTUALPASCAL}, VpSysLow, VpUtils {$ELSE}, DrvTypes{$ENDIF}
     {$IFDEF PLUGIN}, Plugin {$ENDIF}
     , FlTl, DnIni, DOS;

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
     l: longint;
     D: DateTime;
     path: string;
     FName: string;
     iLFN: TUseLFN;
 begin
  Name:=lFExpand(Name);
  lFindFirst(Name, AnyFile, lsr);
  lFindClose(lsr);
  Path:=GetPath(Name);
  Name:=GetName(Name);
{  l := TFileRecFixedSize+length(lsr.FullName);}
  l := SizeOf(TFileRec);
  GetMem(fr, l);
  CreateFileRec:=fr;
  FillChar(fr^, l, 0);

  if DosError=0 then
   begin
{$IFNDEF OS2}
    fr^.FlName[false]:=lsr.sr.name;
{$ENDIF}
    CopyShortString(lsr.FullName, fr^.FlName[true]);
    fr^.Size:=lsr.FullSize;
    fr^.PSize:=lsr.FullSize;
    fr^.Owner:=NewStr(Path);
    fr^.OwnerDisposible:=true;
    fr^.Diz:=nil;

    UnpackTime(lsr.sr.Time, D);
    fr^.Yr := D.Year;
    with TDate4(fr^.FDate) do
      begin Month := D.Month; Day := D.Day; Hour := D.Hour; Minute := D.Min;
      end;
    fr^.Second := D.Sec;

    UnpackTime(lsr.sr.CreationTime, D);         {JO}
    fr^.YrCreat := D.Year;
    with TDate4(fr^.FDateCreat) do
      begin Month := D.Month; Day := D.Day; Hour := D.Hour; Minute := D.Min;
      end;
    fr^.SecondCreat := D.Sec;

    UnpackTime(lsr.sr.LastAccessTime, D);
    fr^.YrLAcc := D.Year;
    with TDate4(fr^.FDateLAcc) do
      begin Month := D.Month; Day := D.Day; Hour := D.Hour; Minute := D.Min;
      end;
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
    fr^.FlName[false]:=GetURZ(GetName(lfGetShortFileName(Name)));
{$ENDIF}
    fr^.FlName[true]:=Name;
   end;
 with fr^ do
   begin
   UsageCount:=1;
   end;
 end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function CopyFileRec;
begin
 CopyFileRec := FR;
 with FR^ do
   Inc(UsageCount);
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
{  FreeMem(FR, TFileRecFixedSize+length(FR^.FlName[true]));}
  FreeMem(FR, SizeOf(TFileRec));
  FR:=Nil;
 end end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function LoadFileRec(var s: TStream): PFileRec;
var P: PFileRec;
    l: byte;
    FullLen: longint;
begin
 S.Read(l, SizeOf(l)); {длина длинного имени}
{ FullLen := TFileRecFixedSize+l;}
 FullLen := SizeOf(TFileRec);
 GetMem(P, FullLen);
 fillchar(p^, FullLen, 0);
 with P^ do begin
  s.Read(Size, Sizeof(Size));
  s.Read(PSize, Sizeof(PSize));
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
{$IFNDEF OS2}
  S.Read(FlName[false], SizeOf(FlName[false]));
{$ENDIF}
  S.Read(FlName[true], l+1);
  UsageCount:=1;
 end;
 LoadFileRec:=P;
end;

procedure StoreFileRec(var s: TStream; fr: PFileRec);
var
    l: byte;
begin
with fr^ do begin
  l := length(FlName[true]);
  s.Write(l, Sizeof(l));
  s.Write(Size, Sizeof(Size));
  s.Write(PSize, Sizeof(PSize));
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
{$IFNDEF OS2}
  S.Write(FlName[false], SizeOf(FlName[false]));
{$ENDIF}
  S.Write(FlName[true], l+1);
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
 var {T1: TFileRec;}
     P1: PFileRec absolute Key1;
     P2: PFileRec absolute Key2;
     C: Integer;
     SM, I1, I2: Integer;
     P1P, P2P: Boolean;
     PanelFlags: Word;
    {OneIsFile: Boolean;}
     ST1, ST2: String;

{$IFNDEF OS_DOS}
   function    CmpName (const S1: string; const S2: string;
        const owner1: string; const owner2: string): Integer;
    begin
      if S1>S2 then CmpName := +1
      else if S1<S2 then CmpName := -1
// JO: в чём смысл этого условия с #22 непонятно, но оно приводит к
//     несходимости QuickSort и зависанию/падениям при наличии NoShortName
//     в панели при компиляции с дефайном QSort
     {else if S1[1] = #22 then CmpName := +1}
      else if owner1>owner2 then CmpName := +1
      else if owner1<owner2 then CmpName := -1
      else CmpName := 0;
    end;

    function    CmpEXT(S1, S2: String): Integer;
    var E1, E2: String;
        B, K: Byte;
    begin

     For B := Length(S1) downto 1 do if S1[B]='.' then break;
     if (B > 1) or (S1[1]='.') then begin
      K := Length(S1)-B+1;
      Move(S1[B], E1[1], K);
      SetLength(E1, K);
      SetLength(S1, B-1);
     end else E1 := '.';

     For B := Length(S2) downto 1 do if S2[B]='.' then break;
     if (B > 1) or (S2[1]='.') then begin
      K := Length(S2)-B+1;
      Move(S2[B], E2[1], K);
      SetLength(E2, K);
      SetLength(S2, B-1);
     end else E2 := '.';

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
      Name1, Name2: string; {UpStrg(P2^.Name)  // AK155}


begin
 if Owner<>nil then PanelFlags := PFilePanel(Owner)^.PanelFlags
               else PanelFlags := PanelDefaults.Show;
{Move(Key1^,T1,SizeOf(T1));}
 with P2^ do
   begin
     P1P := P1^.TType = ttUpDir;
     P2P := TType = ttUpDir;
     if P1P and not P2P then begin Compare := -1; Exit; end;
     if P2P and not P1P then begin Compare := +1; Exit; end;

     Name1 := P1^.FlName[uLfn];
     Name2 := FlName[uLfn];
{$IFDEF WIN32}
  {$IFDEF RecodeWhenDraw}
     CharToOemSt(Name1); CharToOemSt(Name2);
  {$ENDIF}
{$ENDIF}
    if UpperCaseSorting then
      begin
       UpStr(Name1); UpStr(Name2);
      end
     else
      begin
       LowStr(Name1); LowStr(Name2);
      end;

     SM := SortMode;

     if (SM >= NumSortModes) and (SM <= NumSortModes + 31) then begin { условие используется для сравнения }
                                                                      {  каталогов по Ctrl-C }

  {здесь C1 используется для сравнения каталогов по Ctrl-C}

     if ((SM-NumSortModes) and 16 = 0)
      {$IFDEF Win32}
    {JO: регистрочувствительное сравнение по коротким именам лишено смысла}
        or (not uLfn)
      {$ENDIF}
      then
       begin
         if Name1 > Name2 then C1 := 1 else
           if Name1 < Name2 then C1 := -1 else
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

       if (C1=0) and ((P1^.Attr or Attr) and Directory = 0) and
          (((SM-NumSortModes) and 1 = 0) or (P1^.Size = Size)) and
          (((SM-NumSortModes) and 2 = 0) or (P1^.Yr < Yr) or
           ((P1^.Yr = Yr) and (P1^.FDate <= FDate)) or
           ((P1^.Yr = Yr) and (P1^.FDate = FDate) and (P1^.Second < Second))) and
          (((SM-NumSortModes) and 4 = 0) or (P1^.Attr = Attr)) and
          (((SM-NumSortModes) and 8 = 0) or
                  CompareFiles(MakeNormName(P1^.Owner^, Name1),
                               MakeNormName(Owner^, Name2)) )
           then C := 0 else if (C1=-1) then C := -1
                                                 else C := 1;
       Compare := C;
       Exit;
     end; { конец куска для сравнения каталогов по Ctrl-C }
          { дальше идёт кусок для сортировки файлов в панелях }

    {UpLowStr(P1^.Name);} {JO}

  {далее C1 используется для сортировки файлов в панелях}
     begin
       if Name1 < Name2 then
         C1 := -1
       else if Name1 = Name2 then
         C1 := 0
       else
         C1 := 1;
     end;

     if (Owner <> nil) and not (SM in [psmSize, psmTime {$IFNDEF DPMI32}, psmCrTime, psmLATime {$ENDIF}]) then
     if PanelFlags and fmiExeBeforeArc <> 0 then
     begin
       if PanelFlags and fmiExeFirst <> 0 then
         begin
           if ((P1^.Attr xor Attr) and Directory = 0) then
           begin
             if (P1^.TType = ttExec) and (TType <> ttExec) then begin Compare := -1; Exit; end;
             if (P1^.TType <> ttExec) and (TType = ttExec) then begin Compare := 1; Exit; end;
           end;
         end;
       if PanelFlags and fmiArchivesFirst <> 0 then
         begin
           if ((P1^.Attr xor Attr) and Directory = 0) then
           begin
             if (P1^.TType = ttArc) and (TType <> ttArc) then begin Compare := -1; Exit; end;
             if (P1^.TType <> ttArc) and (TType = ttArc) then begin Compare := 1; Exit; end;
           end;
         end;
       end
     else
       begin
         if PanelFlags and fmiArchivesFirst <> 0 then
         begin
           if ((P1^.Attr xor Attr) and Directory = 0) then
           begin
             if (P1^.TType = ttArc) and (TType <> ttArc) then begin Compare := -1; Exit; end;
             if (P1^.TType <> ttArc) and (TType = ttArc) then begin Compare := 1; Exit; end;
           end;
         end;
         if PanelFlags and fmiExeFirst <> 0 then
         begin
           if ((P1^.Attr xor Attr) and Directory = 0) then
           begin
             if (P1^.TType = ttExec) and (TType <> ttExec) then begin Compare := -1; Exit; end;
             if (P1^.TType <> ttExec) and (TType = ttExec) then begin Compare := 1; Exit; end;
           end;
       end;
     end;

     case SM of
     psmUnsorted: if P1^.Owner^ = Owner^
        then if C1 = 0 then C := 0 else C := -1
          else if P1^.Owner^ < Owner^ then C := -1 else C := 1;
      psmOrdered: begin
           I1 := P1^.TType; I2 := TType;
           if I1 = 0 then I1 := 100;
           if I2 = 0 then I2 := 100;
           if I1 < I2 then C := -1 else
           if I1 > I2 then C := 1 else
           if C1=-1 then C := -1
           else if C1=1 then C := 1
           else
              C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
          end;
{LFN}psmLongName:
          if (P1^.Attr xor Attr) and Directory <> 0 then
            if P1^.Attr and Directory <> 0 then C := -1
              else C := 1
           else C := CmpName(Name1, Name2, P1^.Owner^, Owner^);

     141: if (P1^.Attr xor Attr) and Directory <> 0 then
          if P1^.Attr and Directory <> 0 then C := -1 else C := 1 else
           if Name1 < Name2 then C := -1
            else if (Name1 > Name2) then C := 1
                  else C := 0;

{LEXT}psmLongExt:
          if (P1^.Attr xor Attr) and Directory <> 0 then
            if P1^.Attr and Directory <> 0 then C:=-1
            else C:=1
          else
            begin
              C := CmpEXT(Name1, Name2);
              if C = 0 then C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
            end;
{SIZE}psmSize:
          if (P1^.Attr xor Attr) and Directory <> 0 then
          if P1^.Attr and Directory <> 0 then C := -1 else C := 1 else
           if P1^.Size > Size then C := -1 else
           if P1^.Size < Size then C := 1 else
           if C1=-1 then C := -1
            else if C1=1 then C := 1
             else C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
{DATE}psmTime:
         begin
         if (P1^.Attr xor Attr) and Directory <> 0 then
          if P1^.Attr and Directory <> 0 then C := -1 else C := 1 else
           if (P1^.Yr < Yr) then C := 1 else
           if (P1^.Yr > Yr) then C := -1 else
           if (P1^.FDate < FDate) then C := 1 else
           if (P1^.FDate > FDate) then C := -1 else
           if (P1^.Second < Second) then C := 1 else
           if (P1^.Second > Second) then C := -1 else
           if C1=-1 then C := -1
            else if C1=1 then C := 1
             else C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
         end;
{JO}
{$IFNDEF OS_DOS}
{$IFNDEF DPMI32}
{Creat.DATE}psmCrTime:
         begin
         if (P1^.Attr xor Attr) and Directory <> 0 then
          if P1^.Attr and Directory <> 0 then C := -1 else C := 1 else
           if (P1^.YrCreat < YrCreat) then C := 1 else
           if (P1^.YrCreat > YrCreat) then C := -1 else
           if (P1^.FDateCreat < FDateCreat) then C := 1 else
           if (P1^.FDateCreat > FDateCreat) then C := -1 else
           if (P1^.SecondCreat < SecondCreat) then C := 1 else
           if (P1^.SecondCreat > SecondCreat) then C := -1 else
           if C1=-1 then C := -1
            else if C1=1 then C := 1
             else C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
         end;
{L.Acc.DATE}psmLATime:
         begin
         if (P1^.Attr xor Attr) and Directory <> 0 then
          if P1^.Attr and Directory <> 0 then C := -1 else C := 1 else
           if (P1^.YrLAcc < YrLAcc) then C := 1 else
           if (P1^.YrLAcc > YrLAcc) then C := -1 else
           if (P1^.FDateLAcc < FDateLAcc) then C := 1 else
           if (P1^.FDateLAcc > FDateLAcc) then C := -1 else
           if (P1^.SecondLAcc < SecondLAcc) then C := 1 else
           if (P1^.SecondLAcc > SecondLAcc) then C := -1 else
           if C1=-1 then C := -1
            else if C1=1 then C := 1
             else C := CmpName(Name1, Name2, P1^.Owner^, Owner^);
         end;
{$ENDIF}
{$ENDIF OS_DOS}

{DIZ}psmDIZ:
         if (P1^.Attr xor Attr) and Directory <> 0 then
          if P1^.Attr and Directory <> 0 then C := -1 else C := 1 else
          begin
           if (P1^.DIZ <> nil) and (P1^.DIZ^.DIZ <> nil) then ST1 := UpLowStrg(CnvString(P1^.DIZ^.DIZ)) else ST1 := '';
           if (P2^.DIZ <> nil) and (P2^.DIZ^.DIZ <> nil) then ST2 := UpLowStrg(CnvString(DIZ^.DIZ)) else ST2 := '';
           if (ST1 <> '') and (ST2 = '') then C := -1 else
             if (ST1 = '') and (ST2 <> '') then C := 1 else
              {C := CmpName(ST1, ST2);}
               begin
                 if ST1 > ST2 then C := 1 else
                   if ST1 < ST2 then C := -1 else
                     C := 0;
               end;
           if C = 0 then C := C1;
           if C = 0 then if P1^.Owner^ < Owner^ then C := -1
                           else if P1^.Owner^ > Owner^ then C := 1;
          end;
{/JO}
      else if (P1^.Attr and $3F = Attr and $3F) and (P1^.Yr=Yr) and (P1^.FDate = FDate) and (P1^.Second = Second)
              and (C1=0) and (P1^.Owner^ = Owner^) //! {$IFNDEF OS2} and (GetLFN(P1^.LFN)=GetLFN(LFN)) {$ENDIF}
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

 {Cat}
 {$IFDEF PLUGIN}
 if IncludeTemp then
   Inc(N, CreateDriveMenus(Items, MaxL));
 {$ENDIF}
 {/Cat}

 if IncludeTemp and (InterfaceData.DrvInfType and ditIncludeQick <> 0) then begin
  C := ' ';
  for DriveNum := 8 downto 0 do
    if DirsToChange[DriveNum] <> nil then begin
      FreeStr:='~'+Itos(DriveNum+1)+':~ '+ CutH(DirsToChange[DriveNum]^,24);
      Items := NewItem(FreeStr, 'Alt-'+Itos(DriveNum+1), kbNoKey,
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
             Items := NewItem('~'+C+':~ ', '', kbNoKey, 2000 + Byte(C),
                               hcNoContext, Items);
         Items := NewSubMenu('~+:~ LINK', hcNoContext, NewMenu(Items), nil);
         Lnk := Items;
         Inc(N)
       end;
   end;
 {$ENDIF}
 {$ENDIF}

 if IncludeTemp then begin
     If (InterfaceData.DrvInfType and ditIncludeTempInfo <> 0) then
      If TempFiles<>nil then begin
       IDDQD.Dr := 0; IDDQD.Fl := 0;
       TempFiles^.ForEach(@GetInfo);
       if IDDQD.Dr+IDDQD.Fl = 0 then FreeStr := GetString(dlEmpty) else
       if IDDQD.Dr = 0 then FreeStr := ItoS(IDDQD.Fl) + ' ' + #0 + GetString(dlDIFiles) + #0 else
       if IDDQD.Fl = 0 then FreeStr := ItoS(IDDQD.Dr) + ' ' + #0 + GetString(dlDirectories) + #0 else
        FormatStr(FreeStr, GetString(dlFilDir), IDDQD);
      end else FreeStr := GetString(dlEmpty)
     else FreeStr := '';

     Items := NewItem('~*:~ TEMP: '+FreeStr, '', kbSpace, 1200, hcTempList, Items);
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
      if (InterfaceData.DrvInfType and ditMediaType <> 0) then
       if not ((InterfaceData.DrvInfType and ditSkipFloppy <> 0) and (DriveNum <= 2)) then
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
       if (InterfaceData.DrvInfType and ditShowVolume <> 0) and not DrInvalid and
         not ((InterfaceData.DrvInfType and ditSkipFloppy <> 0) and (DriveNum <= 2)) then
         FullS := FullS + {$IFDEF WIN32}CharToOemStr{$ENDIF}(GetVolumeLabel(C)); {Cat}
      {$ENDIF}
      {$IFDEF NETINFO}
      Get_drive_connection_id(DriveNum, Server_num);
      if (Server_num > 0) and (InterfaceData.DrvInfType and ditNetDir <> 0) then
      begin
        Get_File_Server_Name(Server_Num, Server);
        Set_Preferred_Connection_Id(Server_Num);
        Get_Drive_Handle_Id(DriveNum, Handle_Num);
        Get_Directory_Path(Handle_Num, PathName, RetCode);
        lGetDir(DriveNum, TmpS); {DataCompBoy}
        if Length(TmpS) > 3 then
        begin
          TmpS := ' <' + Copy(PathName, Length(PathName) - Length(TmpS) + 4, MaxStringLength) + '>';
          Delete(PathName, Length(PathName) - Length(TmpS) + 4, MaxStringLength);
          if PathName[Length(PathName)] = '/' then Delete(PathName, Length(PathName), 1);
        end else TmpS := '';
        PathName := CutLongString(Server + '/' + PathName);
        PathName := PathName + TmpS;
        FullS := FullS + PathName;
      end else
      {$ENDIF}
      begin
          if (DriveNum<=2) or ((InterfaceData.DrvInfType and ditCDDir=0)
          and (InterfaceData.DrvInfType and ditLocalDir=0)) then ShowDir:=False
          else if (InterfaceData.DrvInfType and ditCDDir<>0)
          and (InterfaceData.DrvInfType and ditLocalDir<>0) then ShowDir:=True
          else if (InterfaceData.DrvInfType and ditLocalDir<>0)
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
 {$IFDEF PLUGIN}
 if N > 65000 then SelectDrive := #26+Char(N-65000); {Cat}
 {$ENDIF}
 if (N >= cmQuickChange1) and (N <= cmQuickChange9)
             then SelectDrive := CnvString(DirsToChange[N-cmQuickChange1]);
end;

function GetFileType(const S: String; Attr: Byte): Integer;
begin
  if S = '..' then GetFileType := ttUpDir else {AK155}
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
end;

        {-DataCompBoy-}
function NewFileRec;
  var PR: PFileRec;
      T:  TFileRec;
      D: DateTime;
  {$IFNDEF OS2}
      Name12: Str12;
  {$ENDIF}
      l: longint;
begin
{$IFNDEF OS2}
  T.FlName[false] := Name;
  if Attr and Directory <> 0 then UpStr(T.FlName[false]) else LowStr(T.FlName[false]);
(*!  case Attr and (Hidden+SysFile) of                  {Pavel Anufrikov -> }
   Hidden  :        T.Name[9] := NameFormatChar[nfmHidden];
   SysFile :        T.Name[9] := NameFormatChar[nfmSystem];
   Hidden+SysFile : T.Name[9] := NameFormatChar[nfmHiddenSystem];
  end{case};                                         { <- Pavel Anufrikov}
!*)
  if Attr and SysFile <> 0 then T.FlName[false][1] := Upcase(T.FlName[false][1]);
  CopyShortString(LFN, T.FlName[true]);
{$ELSE}
  CopyShortString(Name, T.FlName[true]);
{$ENDIF}
  T.Size := Size; T.PSize := Size;
  if Date = 0 then
    begin
      T.Yr     := 1980;
      T.FDate := $1111;
      T.Second := 1;
    end else
    begin
      UnpackTime(Date, D);
      T.Yr := D.Year;
      with TDate4(T.FDate) do
        begin Month := D.Month; Day := D.Day; Hour := D.Hour; Minute := D.Min;
        end;
      T.Second := D.Sec;
    end;
  if CreationDate = 0 then
    begin
      T.YrCreat     := 0;
      T.FDateCreat  := $0000;
      T.SecondCreat := 0;
    end else
    begin
      UnpackTime(CreationDate, D);
      T.YrCreat := D.Year;
      with TDate4(T.FDateCreat) do
        begin Month := D.Month; Day := D.Day; Hour := D.Hour; Minute := D.Min;
        end;
      T.SecondCreat := D.Sec;
    end;
  if LastAccDate = 0 then
    begin
      T.YrLAcc     := 0;
      T.FDateLAcc  := $0000;
      T.SecondLAcc := 0;
    end else
    begin
      UnpackTime(LastAccDate, D);
      T.YrLAcc := D.Year;
      with TDate4(T.FDateLAcc) do
        begin Month := D.Month; Day := D.Day; Hour := D.Hour; Minute := D.Min;
        end;
      T.SecondLAcc := D.Sec;
    end;
  T.Attr := Attr and $7FFF;
  T.Selected := false;
  T.DIZ := nil;
  T.TType := GetFileType(T.FlName[true], T.Attr);
  T.Owner := AOwner;
  T.OwnerDisposible := false;
  T.UsageCount:=1;
{  l := TFileRecFixedSize+length(T.FlName[true]);}
  l := SizeOf(TFileRec);
  GetMem(PR, l);
  move(T, PR^, l);
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
    with TDate4(FDate) do
      begin DT.Month := Month; DT.Day := Day; DT.Hour := Hour; DT.Min := Minute;
      end;
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
    with TDate4(FDateCreat) do
      begin DT.Month := Month; DT.Day := Day; DT.Hour := Hour; DT.Min := Minute;
      end;
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
    with TDate4(FDateLAcc) do
      begin DT.Month := Month; DT.Day := Day; DT.Hour := Hour; DT.Min := Minute;
      end;
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
    S: String;
{$IFDEF CHDIRTIMER}
  ev: TEvent;
{$ENDIF}
{$IFDEF CHDIRDEBUG}
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
 if ChDirTimer and (ShiftState and (kbLeftShift+kbRightShift) <> 0) then
   DDTimer := Get100s;
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
 {JO: проверяем, не лежит ли файл в архиве в панели поиска}
  if not PathFoundInArc(P^.Owner^) then
    begin
      with P^ do lFindFirst(MakeNormName(Owner^, FlName[true]), Anyfile , SR); {JO}
      lFindClose(SR);
      if (DosError <> 0) or Abort then Files^.AtFree(i)
      else begin
       P^.Size  := SR.FullSize;
       UnpackTime(SR.SR.Time, D);
       P^.Yr := D.Year;
       with TDate4(P^.FDate) do
         begin Month := D.Month; Day := D.Day; Hour := D.Hour; Minute := D.Min;
         end;
       P^.Second := D.Sec;
       P^.Attr  := SR.SR.Attr;
 {JO: чтобы менялся цвет в панели поиска и временной панели при переименовании}
       P^.TType := GetFileType(P^.FlName[true], P^.Attr);
       inc(i);
      end;
    end
   else
    begin
 {JO: проверяем, существует ли архив, в котором лежит файл }
     S := P^.Owner^;
     if Length(S) > 1 then S[2] := ';'; {JO: реально не важно, какой символ взять, лишь бы не ':'}
     S := Copy(P^.Owner^, 1, PosChar(':', S) - 1);
     lFindFirst(S, Anyfile, SR);
     lFindClose(SR);
     if (DosError <> 0) or Abort then Files^.AtFree(i)
      else inc(i);
    end;
 end;
 If Files^.Count > 0 then Files^.Sort;
{$IFDEF CHDIRDEBUG}
 PanelState^:=StartChar;
{$ENDIF}
{$IFDEF CHDIRTIMER}
 if ChDirTimer and (ShiftState and (kbLeftShift+kbRightShift) <> 0) then
  begin
   DDTimer:=Get100s-DDTimer;
   ev.what:=evCommand;
   ev.command:=cmShowTimeInfo;
   ev.infoptr:=nil;
   Application^.PutEvent(ev);
  end;
{$ENDIF}
end;
end.
