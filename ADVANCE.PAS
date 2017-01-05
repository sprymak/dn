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
{AK155 = Alexey Korop, 2:461/155@fidonet}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

unit advance;

interface

uses
  Dos, {DnIni}
  Strings,
  Objects, Lfn {DataCompBoy}
  {Cat}, U_KeyMap {/Cat}
  ;

{$IFDEF DNPRG}
{$I Version.Inc}
  {DataCompBoy: DO NOT INCLUDE Version.inc IN OTHER UNITS!}
{simple add in USES Advance}
{$ENDIF}

type
  TNameFormatMode = (nfmNull, nfmSelected, nfmCut, nfmHidden,
    nfmSystem, nfmHiddenSystem);

const
  NameFormatChar: array[TNameFormatMode] of Char = (#46, #251, #16,
    #176, #177, #178);

  flnPreferName = 1;
  flnHardExtArea = 2;
  flnAutoHideDot = 4;
  flnUseCutChar = 8;
  flnHighlight = 16;
  flnUpCase = 32;
  flnLowCase = 64;
  flnCapitalCase = 128;
  flnPadRight = 256;
  flnHandleTildes = 512;
  flnSelected = 1024;

const
  TextReaderBufSize = $1000;

type
  TTextReaderBuf = array[0..TextReaderBufSize-1] of Char;

  PTextReader = ^TTextReader;
  TTextReader = object
    Eof: boolean;
    Constructor Init(const FName: String); {DataCompBoy}
    function GetStr: String;
    function FileName: String; {DataCompBoy}
    destructor Done; virtual;
    private
    Handle: lFile; {DataCompBoy}
    BufSz: integer;
    BufPos: integer;
    Buf: TTextReaderBuf;
    Skip1: boolean;
    end;

  { TColorIndexes }

  PColorIndex = ^TColorIndex;
  TColorIndex = record
    GroupIndex: byte;
    ColorSize: byte;
    ColorIndex: array[0..255] of byte;
    end;

type
  TSize = Comp;

  { TCharImage = array [0..15] of Byte;}

type

  {PZ - 2000.03.26 - begin}

  TCountryInfoEx = record
    CountryCode: word;
    DateFormat: word;
    MoneySymbol: array[0..4] of Char;
    ThousandsSeparator: array[0..1] of Char;
    DecimalSeparator: array[0..1] of Char;
    DateSeparator: array[0..1] of Char;
    TimeSeparator: array[0..1] of Char;
    MoneyFormat: byte;
    MoneyDigits: byte;
    TimeFormat: byte;
    UpCaseCall: Pointer;
    DataSeparator: array[0..1] of Char;
    Reserved: array[0..7] of byte;
    UpperTable: array[#$80..#$FF] of Char;
    end;
  {PZ - end}

  TCountryInfo = record
    DateFmt: word; {Radiobuttons}
    TimeFmt: word; {Radiobuttons}
    DateSep: String[1]; {Inputline}
    TimeSep: String[1]; {Inputline}
    ThouSep: String[1]; {Inputline}
    DecSep: String[1]; {Inputline}
    DecSign: String[1]; {Inputline}
    Currency: String[4]; {Inputline}
    CurrencyFmt: word; {Radiobuttons}
    UpperTable: array[#128..#255] of Char;
    end;

var
  RK: byte;

const

  ColorIndexes: PColorIndex = nil;

  { Keyboard state and shift masks }

  kbRightShift = $0001;
  kbLeftShift = $0002;
  kbCtrlShift = $0004;
  kbAltShift = $0008;
  kbScrollState = $0010;
  kbNumState = $0020;
  kbCapsState = $0040;
  kbInsState = $0080;

  NumFloppy: byte = 0;

  opUnk = 0; { Unknown  }
  opDOS = 1; { DOS      }
  opOS2 = 2; { OS/2     }
  opWin = 4; { Wind0ze  }
  opDV = 8; { DesqView }
  opWNT = 16; { Win NT & Win y2k }
  opDPMI32 = 32; { DPMI32 }
    {Cat: ≠•Á‚Æ ·‡•§≠•• ¨•¶§„ DOS ® Win32, ØÆÌ‚Æ¨„ Æ‚§•´Ï≠Æ Æ‚ DOS}

  Abort: boolean = False;

  opSys: byte = opUnk;

  { useful under OS2 with WinAPI emulator: if (opSys and opWin)=opWin then ...}

procedure CheckOS;
  { Check for OS - For checking API please use error codes }
{ Automatically called while program started }

procedure ClrIO;
  inline;
  begin
    InOutRes := 0;
    DOSError := 0;
    Abort := False;
  end;

function Cut(P: String; len: integer): String;
function CutH(P: String; len: integer): String;

function MemOK: boolean;
  inline;
  begin
    MemOK := True
  end; {JO}
function GetMeMemoStream: PStream; {-$VOL}
function MemAdjust(l: longInt): longInt;
procedure FillWord(var B; Count, W: word);
procedure LocateCursor(X, Y: byte);
function XDiv1024(X: longInt): longInt;
function LongRatio(X, Y: longInt): integer;
function Min(X, Y: longInt): longInt;
function Max(X, Y: longInt): longInt;
function Sgn(X: integer): integer;
procedure TinySlice;

function FormatLongName(const Name: String; Size, ExtSize: byte;
  Options: word;
FormatMode: TNameFormatMode; Delim: PChar): String; (* X-Man *)

{-DataCompBoy-}
const
  UpperString: String =
  #1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16+
  #17#18#19#20#21#22#23#24#25#26#27#28#29#30#31#32+
  '!"#$%&''()*+,-./0123456789:;<=>?@'+
  'ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`'+
  'ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~'+
  'ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôöõúùûü'+
  'ÄÅÇÉÑÖÜáàâäãåçéè∞±≤≥¥µ∂∑∏π∫ªºΩæø¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ'+
  'êëíìîïñóòôöõúùûüÚÚÙÙˆˆ¯˘˙˚¸˝˛'#255; {-$VIV}
  LowerString: String =
  (#1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16+
  #17#18#19#20#21#22#23#24#25#26#27#28#29#30#31#32+
  '!"#$%&''()*+,-./0123456789:;<=>?@'+
  'abcdefghijklmnopqrstuvwxyz[\]^_`'+
  'abcdefghijklmnopqrstuvwxyz{|}~'+
  '†°¢£§•¶ß®©™´¨≠ÆØ‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔ'+
  '†°¢£§•¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ'+
  '‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÒÛÛıı˜˜¯˘˙˚¸˝˛'#255); {-$VIV}
  {-DataCompBoy-}

  BreakChars: Set of Char = [',', ' ', '[', ']', '{', '}', '(', ')',
    ':', ';', '.', '^',
  '&', '*', '!', '#', '$', '/', '\', '"', '%', '>', '<',
  '-', '+', '=', '|', '?', #13, #10, #9, #26, #12, '@'];

  HexStr: array[1..$10] of Char = '0123456789ABCDEF';
  LoHexChar: array[0..$F] of Char = '0123456789abcdef';
  N_O_E_M_S: array[1..5] of Char = 'NOEMS';
  cTEMP_: String[5] = 'TEMP:';
  cLINK_: String[5] = 'LINK:';
  {.$IFNDEF OS2}
  x_x: String[3] = '*.*';
  {.$ELSE}
  {x_x     : string[3] = '*';}
  {.$ENDIF}

var
  DOS40: boolean;
  OS2exec: boolean;
    { use OS2exec to determine if starting of OS/2  }
  { programs is supported                         }
  { use opsys and opOS2 <> 0 to determine if OS/2 }
  { is running                                    }
  FreeStr: String;
  FreeLongStr: LongString;
  FreeByte: byte; {DataCompBoy 8)}
  ActiveDir: String; {DataCompBoy}
  DNNumber: byte;
  UpCaseArray: array[Char] of Char absolute UpperString;
  LowCaseArray: array[Char] of Char absolute LowerString;
  {Cat}
  UpCaseArray_Ascii_Ascii: TXlat absolute UpperString;
  UpCaseArray_Ascii_Koi8r: TXlat;
  UpCaseArray_Ascii_Ansi: TXlat;
  UpCaseArray_Koi8r_Ascii: TXlat;
  UpCaseArray_Ansi_Ascii: TXlat;
  UpCaseArray_Ascii_Ansi_Koi8r_Ascii: TXlat;
  UpCaseArray_Ascii_Koi8r_Ansi_Ascii: TXlat;
  {/Cat}
  Tran: array[Char] of Char;

  {Cat: ØÆ‡Ô§Æ™ Ø•‡•¨•≠≠ÎÂ ≠• ¨•≠Ô‚Ï, ®≠†Á• °„§„‚ Ø‡Æ°´•¨Î · Ø´†£®≠†¨®}

  {-DataCompBoy-}
  StartupDir: String;
  SourceDir: String;
  TempDir: String;
  TempFile: String;
  TempFileSWP: String; {JO}
  LngFile: String;
  SwpDir: String;
  {-DataCompBoy-}
const
  DirToChange: String = ''; {DataCompBoy}

const
  CL_SafeBuf = $8000;

  Linker: Pointer = nil;
  NeedLocated: longInt = 0;

const
  CountryInfo: TCountryInfo =
  (DateFmt: 1;
  TimeFmt: 1;
  DateSep: '-';
  TimeSep: ':';
  ThouSep: ',';
  DecSep: '.';
  DecSign: '2';
  Currency: '$';
  CurrencyFmt: 0;
  UpperTable: 'ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôöõúùûü'+
  'ÄÅÇÉÑÖÜáàâäãåçéè∞±≤≥¥µ∂∑∏π∫ªºΩæø'+
  '¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ'+
  'êëíìîïñóòôöõúùûüÚÚÙÙˆˆ¯˘˙˚¸˝˛'#255
  );

type
  TCrc_Table = array[0..255] of longInt;
const
  crc_table_empty: boolean = True;
  Crc_Table: ^TCrc_Table = nil;

type
  TPosArray = array[1..9] of TPoint;

implementation

uses
  xTime, Startup, advance1, advance3,
  VpSysLow, advance2,
  Commands;

function Cut;
  begin
    (* X-Man *)
    if len < 0 then
      len := 0;
    Cut := FormatLongName(P, len, 0, flnPreferName+flnUseCutChar,
      nfmNull, nil)
  end;

function CutH;
  begin
    (* X-Man *)
    if len < 0 then
      len := 0;
    CutH := FormatLongName(P, len, 0,
    flnHighlight+flnPreferName+flnUseCutChar+flnHandleTildes,
      nfmNull, nil)
  end;

{-DataCompBoy-}
Constructor TTextReader.Init;
  var
    FileSz: longInt;
    ToRead: integer;
  begin
    ClrIO;
    FileMode := $40;
    lAssignFile(Handle, FName);
    lResetFile(Handle, 1);
    if (IOResult <> 0) or Abort then
      Fail;
    FileSz := FileSize(Handle.F);
    if (IOResult <> 0) or Abort then
      Fail;
    Eof := FileSz = 0;
    if not Eof then
      begin
        ToRead := Min(FileSz, TextReaderBufSize);
        BlockRead(Handle.F, Buf, ToRead, BufSz);
        if (IOResult <> 0) or Abort or (ToRead <> BufSz) then
          begin
            ClrIO;
            Close(Handle.F);
            ClrIO;
            Fail;
          end;
      end;
    BufPos := 0;
  end { TTextReader.Init };
{-DataCompBoy-}

{-DataCompBoy-}
function TTextReader.FileName: String;
  begin
    FileName := lFileNameOf(Handle);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TTextReader.GetStr: String;
  var
    CurStr: String;
    BufBeg: integer;
    Was: boolean;

  procedure aStr(D: integer);
    var
      Grow: integer;
    begin
      Grow := Min((BufPos-BufBeg)-D-byte(Was), 255-Length(CurStr));
      if Grow > 0 then
        begin
          Move(Buf[BufBeg], CurStr[Length(CurStr)+1], Grow);
          SetLength(CurStr, Length(CurStr)+Grow);
        end;
    end;

  var
    PrevC: integer;
    C: Char;

  begin { TTextReader.GetStr: }
    CurStr := '';
    if not Eof then
      begin
        PrevC := -1;
        SetLength(CurStr, 0);
        BufBeg := BufPos;
        Was := False;
        repeat
          if BufPos = BufSz then
            begin
              aStr(0);
              ClrIO;
              BlockRead(Handle.F, Buf, TextReaderBufSize, BufSz);
              if BufSz = 0 then
                begin
                  Eof := True;
                  break;
                end;
              BufPos := 0;
              BufBeg := 0;
              if Was then
                begin
                  Skip1 := True;
                  break
                end;
            end;
          C := Buf[BufPos];
          Inc(BufPos);
          case C of
            #0, #10, #13:
              begin
                if Skip1 then
                  begin
                    BufBeg := BufPos;
                    Skip1 := False;
                    continue;
                  end;
                if Was then
                  begin
                    aStr(1);
                    Dec(BufPos, integer(PrevC = integer(C)));
                    break;
                  end
                else
                  Was := True;
                PrevC := integer(C);
              end;
            else
              begin
                if Was then
                  begin
                    aStr(1);
                    Dec(BufPos);
                    break;
                  end;
                Skip1 := False
              end;
          end {case};
        until False;
      end;
    GetStr := CurStr;
  end { TTextReader.GetStr: };
{-DataCompBoy-}

{-DataCompBoy-}
destructor TTextReader.Done;
  begin
    ClrIO;
    Close(Handle.F);
    ClrIO;
  end;
{-DataCompBoy-}

function FormatLongName(const Name: String; Size, ExtSize: byte;
    Options: word;
  FormatMode: TNameFormatMode; Delim: PChar): String;

  var
    P, n, E, R: ^String;
    PSize, NSize, ESize: integer;
    Hi1, Hi2, Hi3, DotPos, EFlag, i: byte;

  procedure FillR(A, B: byte; Value: Char);
    begin
      while A <= B do
        begin
          R^[A] := Value;
          Inc(A)
        end
    end;

  function MakeResult: String;
    var
      i: byte;
      Res: ^String;
    begin
      New(Res);
      SetLength(Res^, 0);
      if (Options and flnPadRight) = 0
      then
        while R^[Length(R^)] = #32 do
          SetLength(R^, Length(R^)-1);
      if ((Options and flnSelected) <> 0) and
        ((Options and flnHighlight) <> 0)
      then
        AddStr(Res^, '~');
      for i := 1 to Length(R^) do
        begin
          if ((i = Hi1) or (i = Hi2) or (i = Hi3)) and
            ((Options and flnHighlight) <> 0)
          then
            AddStr(Res^, '~');
          if (R^[i] = '~') and ((Options and flnHandleTildes) <> 0)
          then
            AddStr(Res^, #0);
          AddStr(Res^, R^[i]);
          {AK155: ß†Á•¨ Ì‚Æ‚ ¢‚Æ‡Æ© #0 - Ø‡®§„¨†‚Ï ≠• ·¨Æ£, ØÆÌ‚Æ¨„ „°‡†´ •£Æ.
 Ä Æ‚≠Æ·®‚•´Ï≠Æ Ø•‡¢Æ£Æ #0 §Æ£†§†´·Ô, Á‚Æ Æ≠ §Æ´¶•≠ Æ°•·Ø•Á®¢†‚Ï
 Æ‚‡®·Æ¢™„ ‚®´Ï§Î. à ¢≠•· ·ÆÆ‚¢•‚·‚¢„ÓÈ„Ó ™Æ‡‡•™Ê®Ó ¢
 drivers._vp.MoveCStr. 06.01.2001}
            {
            if (R^[i]='~') and ((Options and flnHandleTildes)<>0)
            then AddStr(Res^,#0);}

          {/AK155}
          if ((i = Hi1) or (i = Hi2) or (i = Hi3)) and
            ((Options and flnHighlight) <> 0)
          then
            AddStr(Res^, '~');
        end;
      if ((Options and flnSelected) <> 0) and
        ((Options and flnHighlight) <> 0)
      then
        AddStr(Res^, '~');
      MakeResult := Res^;
      Dispose(Res)
    end { MakeResult: };

  begin { FormatLongName }
    New(P);
    New(n);
    New(E);
    New(R);

    {-DataCompBoy-}
    if FMSetup.TagChar <> '' then
      NameFormatChar[nfmSelected] := FMSetup.TagChar[1];
    if FMSetup.RestChar <> '' then
      NameFormatChar[nfmCut] := FMSetup.RestChar[1];
    {-DataCompBoy-}

    {-DataCompBoy-}
    if Name <> '..' then
      lFSplit(Name, P^, n^, E^)
    else
      begin
        P^:= '';
        n^:= '..';
        E^:= '';
      end;
    if (Length(E^) > 0) and (E^[1] = '.') then
      begin
        for i := 1 to Length(E^)-1 do
          E^[i] := E^[i+1];
        SetLength(E^, Length(E^)-1)
      end;
    {-DataCompBoy-}
    if (Options and flnUpCase) <> 0 then
      begin
        UpStr(P^);
        UpStr(n^);
        UpStr(E^)
      end
    else if (Options and (flnLowCase or flnCapitalCase)) <> 0 then
        begin
        LowStr(P^);
        LowStr(n^);
        LowStr(E^);
        if (Options and flnCapitalCase) <> 0 then
          n^[1] := UpCase(n^[1])
      end;
    SetLength(R^, Size);
    if Size < 5+ExtSize then
      begin
        FillR(1, Size, NameFormatChar[nfmCut]);
        if Delim <> nil then
          Delim^:= NameFormatChar[nfmCut]
        else
          Hi1 := Size;
        FormatLongName := MakeResult;
        Dispose(R);
        Dispose(E);
        Dispose(n);
        Dispose(P);
        exit
      end;
    PSize := Length(P^);
    NSize := Length(n^);
    ESize := Length(E^);
    EFlag := byte(ESize > 0);
    if (Options and flnHardExtArea) <> 0 then
      begin
        if ESize > ExtSize then
          ESize := ExtSize;
        if PSize+NSize+1 > Size-ExtSize then
          begin
            PSize := Size-1-ExtSize-NSize;
            if PSize < 3 then
              PSize := 3;
            if PSize > Length(P^) then
              PSize := Length(P^)
          end;
        if PSize+NSize+1 > Size-ExtSize then
          begin
            NSize := Size-1-ExtSize-PSize;
            if NSize < 1 then
              NSize := 1;
            if NSize > Length(n^) then
              NSize := Length(n^)
          end
      end
    else
      begin
        if PSize+NSize+ESize+EFlag > Size then
          begin
            PSize := Size-EFlag-ESize-NSize;
            if PSize < 3 then
              PSize := 3;
            if PSize > Length(P^) then
              PSize := Length(P^)
          end;
        if (Options and flnPreferName) <> 0 then
          begin
            if PSize+NSize+ESize+EFlag > Size then
              begin
                ESize := Size-EFlag-PSize-NSize;
                if ESize < 0 then
                  ESize := 0;
                if ESize > Length(E^) then
                  ESize := Length(E^)
              end;
            if PSize+NSize+ESize+EFlag > Size then
              begin
                NSize := Size-EFlag-PSize-ESize;
                if NSize < 1 then
                  NSize := 1;
                if NSize > Length(n^) then
                  NSize := Length(n^)
              end
          end
        else
          begin
            if PSize+NSize+ESize+EFlag > Size then
              begin
                NSize := Size-EFlag-PSize-ESize;
                if NSize < 1 then
                  NSize := 1;
                if NSize > Length(n^) then
                  NSize := Length(n^)
              end;
            if PSize+NSize+ESize+EFlag > Size then
              begin
                ESize := Size-EFlag-PSize-NSize;
                if ESize < 0 then
                  ESize := 0;
                if ESize > Length(E^) then
                  ESize := Length(E^)
              end
          end
      end;

    FillR(1, Size+1, #32);
    Hi1 := 0;
    Hi2 := 0;
    Hi3 := 0;
    if ExtSize > 0 then
      begin
        if (Options and flnHardExtArea) <> 0 then
          DotPos := Size-ExtSize
        else
          DotPos := Size-ESize
      end
    else
      DotPos := PSize+NSize+1;
    if DotPos > Size then
      begin
        if Delim = nil then
          DotPos := Size
        else
          DotPos := Size+1
      end;
    for i := 1 to PSize do
      R^[i] := P^[i];
    for i := 1 to NSize do
      R^[PSize+i] := n^[i];
    for i := 1 to ESize do
      R^[DotPos+i] := E^[i];
    if (((Options and flnAutoHideDot) <> 0) and (DotPos = Size-
        ExtSize)
      and (ExtSize > 0) or (Length(E^) = 0))
      and (FormatMode = nfmNull)
    then
      begin
      end
    else if R^[DotPos] = #32 then
      R^[DotPos] := NameFormatChar[FormatMode];
    if PSize < Length(P^) then
      begin
        if (Options and flnUseCutChar) <> 0
        then
          R^[PSize] := NameFormatChar[nfmCut];
        Hi1 := PSize
      end;
    if ESize < Length(E^) then
      begin
        if (Options and flnUseCutChar) <> 0 then
          begin
            if Delim <> nil then
              Delim^:= NameFormatChar[nfmCut]
            else
              begin
                if DotPos+ESize <= Size then
                  begin
                    R^[DotPos+ESize] := NameFormatChar[nfmCut];
                    Hi3 := DotPos+ESize
                  end
                else
                  begin
                    if Delim <> nil then
                      Delim^:= NameFormatChar[nfmCut]
                    else
                      begin
                        R^[Size] := NameFormatChar[nfmCut];
                        Hi3 := DotPos+ESize
                      end;
                  end
              end
          end
        else
          Hi3 := DotPos+ESize
      end;
    if NSize < Length(n^) then
      begin
        if (Options and flnUseCutChar) <> 0 then
          begin
            if Delim <> nil then
              begin
                Delim^:= NameFormatChar[nfmCut];
                R^[DotPos] := n^[NSize+1]
              end
            else
              begin
                R^[DotPos] := NameFormatChar[nfmCut];
                Hi2 := DotPos
              end;
          end
        else
          Hi2 := PSize+NSize
      end;
    if ((Options and flnSelected) <> 0) then
      begin
        if DotPos <= Size then
          begin
            R^[DotPos] := NameFormatChar[nfmSelected];
            Hi2 := DotPos
          end
        else
          begin
            R^[Size] := NameFormatChar[nfmSelected];
            Hi2 := Size
          end;
      end;
    FormatLongName := MakeResult;
    Dispose(R);
    Dispose(E);
    Dispose(n);
    Dispose(P)
  end { FormatLongName };

procedure CheckOS;
  begin
    {$IFDEF OS2}opSys := opOS2; {$ENDIF}
    {$IFDEF WIN32}
    if SysPlatformId = 1 then
      opSys := opWin
    else
      opSys := opWNT;
    {$ENDIF}
    {$IFDEF DPMI32}opSys := opDPMI32; {$ENDIF}
  end;


function GetMeMemoStream: PStream; {-$VOL begin}
  const
    _1: byte = 1;
  var
    s: PStream;
    Pos: longInt;
  begin
    s := New(PMemoryStream, Init(2048, 2048));
    {Cat: † ‚•Ø•‡Ï ß†Ø®Ë•¨ ‚„§† •§®≠®Á™„, Á‚Æ°Î §‡„£®• £´„ØÎ• Ø‡ÆÊ•§„‡Î,     }
    {     ™Æ‚Æ‡Î• Á®‚†Ó‚ ®ß ØÆ‚Æ™† ‚Æ, Á‚Æ Æ≠® ‚„§† ≠• ß†Ø®·Î¢†´®, ·Á®‚†´®,  }
    {     Á‚Æ ≠†Ë ØÆ‚Æ™ ·Æ§•‡¶®‚ §´®≠≠Î• ·‚‡Æ™®                              }
    s^.Seek(0);
    s^.Write(_1, 1);
    s^.Seek(0);
    {/Cat}
    if s^.Status <> stOK then
      begin
        Dispose(s, Done);
        s := nil
      end;
    GetMeMemoStream := s;
  end { GetMeMemoStream: }; {-$VOL end}

function MemAdjust(l: longInt): longInt;
  begin
    if Linker <> nil then
      begin
        if l > CL_SafeBuf then
          l := l-CL_SafeBuf
        else
          l := 0;
      end;
    MemAdjust := l;
  end;

procedure FillWord(var B; Count, W: word); assembler;
  {&Frame-} {$USES EDI, ECX}
asm
    cld
    mov  eax, W
    mov  edi, B
    mov  ecx, Count
    rep  stosw
 end
  ;

procedure LocateCursor(X, Y: byte);
  begin
    SysTVSetCurPos {GotoXY}(X, Y)
  end;

{&Frame-}
function XDiv1024; assembler;
asm
  mov eax,X
  shr eax,10
end;

function LongRatio;
  begin
    while X > (MaxLongInt div 100) do
      begin
        X := X div 128;
        Y := Y div 128;
      end;
    LongRatio := 100*X div Y;
  end;

function Min;
  begin
    if X < Y then
      Min := X
    else
      Min := Y
  end;
function Max;
  begin
    if X < Y then
      Max := Y
    else
      Max := X
  end;
function Sgn;
  begin
    if X > 0 then
      Sgn := 1
    else if X < 0 then
      Sgn := -1
    else
      Sgn := 0
  end;

procedure TinySlice;
  {$IFDEF OS_DOS}
  var
    Released: boolean;
    {$ENDIF}
  begin
    {$IFNDEF OS_DOS}
    if (StartupData.Slice2 and osuOnceRelease <> 0) then
      SysCtrlSleep {Delay}(1) {Delay(32)}
    else
      SysCtrlSleep {Delay}(0);
    {$ELSE}
    Released := False;
    if (StartupData.Slice and osuInt15 <> 0) and
      ((StartupData.Slice2 and osuOnceRelease = 0) or not Released)
        and
      (opSys and opDV <> 0)
    then
      asm
   mov al, 1
   mov Released, al
   mov ax, $1000
   int 15h
  end;

    if (StartupData.Slice and osuInt2F <> 0) and
      ((StartupData.Slice2 and osuOnceRelease = 0) or not Released)
    then
      asm
   mov ax, 1680h
   int 2Fh
   or al, al
   je @@e
   mov al, 1
   mov Released, al
@@e:
  end
        ;
    if (StartupData.Slice and osuInt28 <> 0) and
      ((StartupData.Slice2 and osuOnceRelease = 0) or not Released)
    then
      asm int 28h end;
    {$ENDIF}
  end { TinySlice };

begin
  NumFloppy := 0;
  if ValidDrive('A') then
    Inc(NumFloppy);
  if ValidDrive('B') then
    Inc(NumFloppy);
  UpCaseArray[#0] := #0;
  LowCaseArray[#0] := #0;
  DOS40 := Lo(DosVersion) >= 4;
  CheckOS;
  { Starting of OS/2 programs is possible under:              }
  { - OS/2 2.10+, OS2COMSPEC environment variable is required }
  { - OS/2 Warp 3+, no additional requirements                }
  { - OS/2 for PPC, future versions for IA64 (I hope)         }
  {$IFDEF OS_DOS}
  OS2exec := (opSys and opOS2 <> 0) and
  ((Lo(DosVersion) > 20) or
  ((Lo(DosVersion) = 20) and
  ((Hi(DosVersion) >= 30) or
  ((Hi(DosVersion) >= 10) and (GetEnv('OS2COMSPEC') <> ''))
  )));
  {$ENDIF}
  {$IFDEF OS2}
  OS2exec := True;
  {$ENDIF}
  {$IFDEF WIN32}
  OS2exec := False;
  {$ENDIF}
  {$IFDEF DPMI32}
  OS2exec := False;
  {$ENDIF}
end.
