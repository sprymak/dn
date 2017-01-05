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

{$IFNDEF NONBP}
{$A-,B-}
{$ENDIF}

UNIT Advance;

INTERFACE

USES Dos,{DnIni}
     {$IFNDEF NONBP}BStrings{$ELSE}Strings{$ENDIF},
     Objects, LFN{DataCompBoy}
     {$IFDEF DPMI},Dpmi,DosMem{$ENDIF}
     {Cat} ,U_KeyMap {/Cat}
     ;

{$IFDEF DNPRG}
{$I Version.Inc} {DataCompBoy: DO NOT INCLUDE Version.inc IN OTHER UNITS!}
                              {simple add in USES Advance}
{$ENDIF}

type TNameFormatMode=(nfmNull,nfmSelected,nfmCut,nfmHidden,nfmSystem,nfmHiddenSystem);

const
    NameFormatChar:array [TNameFormatMode] of char=(#46,#251,#16,#176,#177,#178);

    flnPreferName   = 1;
    flnHardExtArea  = 2;
    flnAutoHideDot  = 4;
    flnUseCutChar   = 8;
    flnHighlight    = 16;
    flnUpCase       = 32;
    flnLowCase      = 64;
    flnCapitalCase  = 128;
    flnPadRight     = 256;
    flnHandleTildes = 512;
    flnSelected     = 1024;

const
  TextReaderBufSize = $1000;

type
  TTextReaderBuf = array[0..TextReaderBufSize-1] of Char;

   PTextReader = ^TTextReader;
   TTextReader = object
     Eof: Boolean;
     constructor Init(const FName: string); {DataCompBoy}
     function GetStr: string;
     function FileName: string; {DataCompBoy}
     destructor Done; virtual;
   private
     Handle: lFile; {DataCompBoy}
     BufSz: Integer;
     BufPos: Integer;
     Buf: TTextReaderBuf;
     Skip1: Boolean;
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
    CountryCode        : Word;
    DateFormat         : Word;
    MoneySymbol        : array [0..4] of Char;
    ThousandsSeparator : array [0..1] of Char;
    DecimalSeparator   : array [0..1] of Char;
    DateSeparator      : array [0..1] of Char;
    TimeSeparator      : array [0..1] of Char;
    MoneyFormat        : Byte;
    MoneyDigits        : Byte;
    TimeFormat         : Byte;
    UpCaseCall         : Pointer;
    DataSeparator      : array [0..1] of Char;
    Reserved           : array [0..7] of Byte;
    UpperTable         : array [#$80..#$FF] of Char;
  end;
{PZ - end}

    TCountryInfo = record
      DateFmt: Word;  {Radiobuttons}
      TimeFmt: Word;  {Radiobuttons}
      DateSep: string[1]; {Inputline}
      TimeSep: string[1]; {Inputline}
      ThouSep: string[1]; {Inputline}
      DecSep: string[1]; {Inputline}
      DecSign: string[1]; {Inputline}
      Currency: string[4]; {Inputline}
      CurrencyFmt: Word;  {Radiobuttons}
      UpperTable: array[#128..#255] of Char;
    end;

var RK: byte;

CONST

  ColorIndexes: PColorIndex = nil;

{ Keyboard state and shift masks }

  kbRightShift  = $0001;
  kbLeftShift   = $0002;
  kbCtrlShift   = $0004;
  kbAltShift    = $0008;
  kbScrollState = $0010;
  kbNumState    = $0020;
  kbCapsState   = $0040;
  kbInsState    = $0080;

  On  = True;
  Off = False;

  NumFloppy:      Byte    = 0;

  opUnk  = 0;  { Unknown  }
  opDOS  = 1;  { DOS      }
  opOS2  = 2;  { OS/2     }
  opWin  = 4;  { Wind0ze  }
  opDV   = 8;  { DesqView }
  opWNT  = 16; { Win NT & Win y2k }

  Abort : Boolean = false;

  opSys : byte = opUnk;

{ useful under OS2 with WinAPI emulator: if (opSys and opWin)=opWin then ...}

procedure CheckOS; { Check for OS - For checking API please use error codes }
                   { Automatically called while program started }

Procedure ClrIO;
{$IFNDEF VIRTUALPASCAL}
inline {DataCompBoy}
($33/$C0/          { XOR ax, ax       }
 $3E/$A2/>InOutRes/{ mov InOutRes, al }{ InOutRes := 0;  }
 $3E/$A2/>DosError/{ mov DosError, al }{ DosError := 0;  }
 $3E/$A2/>Abort    { mov Abort,    al }{ Abort    := Off;}
);
{$ELSE}inline;
begin
 InOutRes := 0;
 DosError := 0;
 Abort    := Off;
end;
{$ENDIF}

Function  Cut(p : string; len : integer ) : string;
Function  CutH(p : string; len : integer ) : string;

{$IFNDEF VIRTUALPASCAL}
Procedure Delay(MS: longint); {DataCompBoy}
{$ENDIF}

function  MemOK: Boolean;
{$IFDEF VIRTUALPASCAL}
inline; begin MemOK := True end; {JO}
{$ENDIF}
function  GetMeMemoStream: PStream; {-$VOL}
Function  MemAdjust(L: LongInt): LongInt;
procedure FillWord(var B;Count, W: Word);
procedure LocateCursor(X, Y: Byte);
Function  XDiv1024( X: LongInt ): LongInt;
Function  LongRatio( X, Y: LongInt ): Integer;
Function  Min(x,y: LongInt):LongInt;
Function  Max(x,y: LongInt):LongInt;
Function  Sgn(x  : integer):integer;
procedure TinySlice;
{$IFDEF DPMI}
Function  RSeg(Seg: Word): Word; Inline (
  $5B/         {pop BX}
  $B8/$02/$00/ {mov AX, 0002h}
  $CD/$31      {int 31h}
);
{$ENDIF}

function FormatLongName(const Name:string; Size,ExtSize:byte; Options:word;
FormatMode:TNameFormatMode; Delim:PChar):string; (* X-Man *)

          {-DataCompBoy-}
const
     UpperString: string =
     #1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16 +
     #17#18#19#20#21#22#23#24#25#26#27#28#29#30#31#32 +
     '!"#$%&''()*+,-./0123456789:;<=>?@' +
     'ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`' +
     'ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~' +
     'ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôöõúùûü' +
     'ÄÅÇÉÑÖÜáàâäãåçéè∞±≤≥¥µ∂∑∏π∫ªºΩæø¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ'+
     'êëíìîïñóòôöõúùûüÚÚÙÙˆˆ¯˘˙˚¸˝˛'#255; {-$VIV}
     LowerString: string =
     (#1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16 +
     #17#18#19#20#21#22#23#24#25#26#27#28#29#30#31#32 +
     '!"#$%&''()*+,-./0123456789:;<=>?@' +
     'abcdefghijklmnopqrstuvwxyz[\]^_`' +
     'abcdefghijklmnopqrstuvwxyz{|}~' +
     '†°¢£§•¶ß®©™´¨≠ÆØ‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔ' +
     '†°¢£§•¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ'+
     '‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÒÛÛıı˜˜¯˘˙˚¸˝˛'#255); {-$VIV}
       {-DataCompBoy-}

      BreakChars  : Set of Char = [',',' ','[',']','{','}','(',')',':',';','.','^',
                                   '&','*','!','#','$','/','\','"','%','>','<',
                                   '-','+','=','|','?',#13,#10,#9,#26,#12,'@'];

      HexStr     : array[1..$10] of char = '0123456789ABCDEF';
      LoHexChar  : array[0..$F] of char = '0123456789abcdef';
      N_O_E_M_S  : array[1..5] of char = 'NOEMS';
         cTEMP_  : string[5] = 'TEMP:';
         cLINK_  : string[5] = 'LINK:';
 {.$IFNDEF OS2}
         x_x     : string[3] = '*.*';
 {.$ELSE}
        {x_x     : string[3] = '*';}
 {.$ENDIF}

var
    DOS40 : Boolean;
    OS2exec : Boolean;    { use OS2exec to determine if starting of OS/2  }
                          { programs is supported                         }
                          { use opsys and opOS2 <> 0 to determine if OS/2 }
                          { is running                                    }
    FreeStr  : String;
    FreeLongStr : LongString;
    FreeByte : byte; {DataCompBoy 8)}
    ActiveDir: String; {DataCompBoy}
    DNNumber: Byte;
    UpCaseArray: array[Char] of Char Absolute UpperString;
    LowCaseArray: array[Char] of Char Absolute LowerString;
{Cat}
    UpCaseArray_Ascii_Ascii: TXlat Absolute UpperString;
    UpCaseArray_Ascii_Koi8r: TXlat;
    UpCaseArray_Ascii_Ansi: TXlat;
    UpCaseArray_Koi8r_Ascii: TXlat;
    UpCaseArray_Ansi_Ascii: TXlat;
    UpCaseArray_Ascii_Ansi_Koi8r_Ascii: TXlat;
    UpCaseArray_Ascii_Koi8r_Ansi_Ascii: TXlat;
{/Cat}
    Tran: array[Char] of Char;

       {-DataCompBoy-}
  StartupDir: String;
  SourceDir:  String;
  TempDir:    String;
  TempFile:   String;
{$IFDEF VIRTUALPASCAL}
  TempFileSWP: String; {JO}
{$ENDIF}
  LngFile:    String;
  SwpDir:     String;
       {-DataCompBoy-}
Const
  DirToChange: String = ''; {DataCompBoy}

const
    CL_SafeBuf           = $8000;

    Linker: Pointer      = nil;
    NeedLocated: LongInt = 0;


const
  CountryInfo: TCountryInfo =
    ( DateFmt:1;
      TimeFmt:1;
      DateSep:'-';
      TimeSep:':';
      ThouSep:',';
      DecSep:'.';
      DecSign:'2';
      Currency:'$';
      CurrencyFmt:0;
      UpperTable:'ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôöõúùûü' +
                 'ÄÅÇÉÑÖÜáàâäãåçéè∞±≤≥¥µ∂∑∏π∫ªºΩæø' +
                 '¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ' +
                 'êëíìîïñóòôöõúùûüÚÚÙÙˆˆ¯˘˙˚¸˝˛'#255
    );

Type TCrc_Table = array[0..255] of LongInt;
const
  crc_table_empty : boolean = on;
  Crc_Table : ^TCrc_Table = nil;

{$IFNDEF NONBP}
{PZ - 2000.03.26 - begin}

function  GetCountryInfoEx ( var CountryInfoEx : TCountryInfoEx ) : Integer;
procedure FixCountryInfoEx ( var CountryInfoEx : TCountryInfoEx );
procedure CopyCountryInfo  ( const CountryInfoEx : TCountryInfoEx; var CountryInfo : TCountryInfo );

{PZ - end}
{$ENDIF}

type TPosArray = Array [1..9] of TPoint;

IMPLEMENTATION

uses xTime,
     Startup,
     advance3, advance1,
     {$IFDEF VIRTUALPASCAL}
     advance2, crt,
     {$ENDIF}
     ExtraMem,
     Commands
     ;

function Cut;
begin
 (* X-Man *)
 if Len<0 then Len:=0;
 Cut:=FormatLongName(P,Len,0,flnPreferName+flnUseCutChar,nfmNull,nil)
end;

function CutH;
begin
 (* X-Man *)
 if Len<0 then Len:=0;
 CutH:=FormatLongName(P,Len,0,
 flnHighlight+flnPreferName+flnUseCutChar+flnHandleTildes,nfmNull,nil)
end;

        {-DataCompBoy-}
constructor TTextReader.Init;
var
  FileSz: LongInt;
  ToRead: Integer;
begin
  ClrIO; FileMode := $40; lAssignFile(Handle, FName); lResetFile(Handle, 1);
  if (IOResult <> 0) or Abort then Fail;
  FileSz := FileSize(Handle.F);
  if (IOResult <> 0) or Abort then Fail;
  Eof := FileSz = 0;
  if not Eof then
  begin
    ToRead := Min(FileSz, TextReaderBufSize);
    BlockRead(Handle.F, Buf, ToRead, BufSz);
    if (IOResult <> 0) or Abort or (ToRead <> BufSz) then
    begin
      ClrIO; Close(Handle.F); ClrIO;
      Fail;
    end;
  end;
  BufPos := 0;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TTextReader.FileName: string;
begin
  FileName := lFileNameOf(Handle);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TTextReader.GetStr: string;
var
  CurStr: string;
  BufBeg: Integer;
  Was: Boolean;

procedure aStr(D: Integer);
var
  Grow: Integer;
begin
  Grow := Min((BufPos - BufBeg) - D - Byte(Was), 255-Length(CurStr));
  if Grow > 0 then
  begin
    Move(Buf[BufBeg], CurStr[Length(CurStr)+1], Grow);
    SetLength(CurStr, Length(CurStr)+Grow);
  end;
end;

var
  PrevC: Integer;
  C: Char;

begin
 CurStr := ''; if not Eof then
 begin
  PrevC := -1;
  SetLength(CurStr, 0); BufBeg := BufPos; Was := False;
  repeat
    if BufPos = BufSz then
    begin
      aStr(0);
      ClrIO; BlockRead(Handle.F, Buf, TextReaderBufSize, BufSz);
      if BufSz = 0 then
      begin
        Eof := True;
        Break;
      end;
      BufPos := 0;
      BufBeg := 0;
      if Was then begin Skip1 := True; Break end;
    end;
    C := Buf[BufPos]; Inc(BufPos);
    case C of
      #0, #10, #13 :
        begin
          if Skip1 then
          begin
            BufBeg := BufPos;
            Skip1 := False; Continue;
          end;
          if Was then
          begin
            aStr(1);
            Dec(BufPos, Integer(PrevC=Integer(C)));
            Break;
          end else Was := True;
          PrevC := Integer(C);
        end;
      else
      begin
        if Was then
        begin
          aStr(1);
          Dec(BufPos);
          Break;
        end;
        Skip1 := False
      end;
    end;
  until False;
 end;
 GetStr := CurStr;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
destructor TTextReader.Done;
begin
  ClrIO; Close(Handle.F); ClrIO;
end;
        {-DataCompBoy-}

function FormatLongName(const Name:string; Size,ExtSize:byte; Options:word;
FormatMode:TNameFormatMode; Delim:PChar):string;

var
    P,N,E,R:^String;
    PSize,NSize,ESize:integer;
    Hi1,Hi2,Hi3,DotPos,EFlag,i:byte;

    procedure FillR(A,B:byte;Value:char);
    begin
        while A<=B do begin
            R^[A]:=Value;
            Inc(A)
        end
    end;

    function MakeResult:string;
    var i:byte; Res:^String;
    begin
        New(Res); SetLength(Res^, 0);
        if (Options and flnPadRight)=0
        then while R^[Length(R^)] = #32 do SetLength(R^, Length(R^)-1);
        if ((Options and flnSelected)<>0) and
        ((Options and flnHighlight)<>0) then AddStr(Res^,'~');
        for i:=1 to Length(R^) do begin
            if ((i=Hi1) or (i=Hi2) or (i=Hi3)) and
            ((Options and flnHighlight)<>0) then AddStr(Res^,'~');
            if (R^[i]='~') and ((Options and flnHandleTildes)<>0)
            then AddStr(Res^,#0);
            AddStr(Res^,R^[i]);
{AK155: ß†Á•¨ Ì‚Æ‚ ¢‚Æ‡Æ© #0 - Ø‡®§„¨†‚Ï ≠• ·¨Æ£, ØÆÌ‚Æ¨„ „°‡†´ •£Æ.
 Ä Æ‚≠Æ·®‚•´Ï≠Æ Ø•‡¢Æ£Æ #0 §Æ£†§†´·Ô, Á‚Æ Æ≠ §Æ´¶•≠ Æ°•·Ø•Á®¢†‚Ï
 Æ‚‡®·Æ¢™„ ‚®´Ï§Î. à ¢≠•· ·ÆÆ‚¢•‚·‚¢„ÓÈ„Ó ™Æ‡‡•™Ê®Ó ¢
 drivers._vp.MoveCStr. 06.01.2001}{
            if (R^[i]='~') and ((Options and flnHandleTildes)<>0)
            then AddStr(Res^,#0);}

{/AK155}
            if ((i=Hi1) or (i=Hi2) or (i=Hi3)) and
            ((Options and flnHighlight)<>0) then AddStr(Res^,'~');
        end;
        if ((Options and flnSelected)<>0) and
        ((Options and flnHighlight)<>0) then AddStr(Res^,'~');
        MakeResult:=Res^;
        Dispose(Res)
    end;

begin
    New(P); New(N); New(E); New(R);

        {-DataCompBoy-}
    if FMSetup.TagChar <>'' then NameFormatChar[nfmSelected]:=FMSetup.TagChar[1];
    if FMSetup.RestChar<>'' then NameFormatChar[nfmCut]:=FMSetup.RestChar[1];
        {-DataCompBoy-}

        {-DataCompBoy-}
    if Name<>'..' then lFSplit(Name,P^,N^,E^)
     else begin
           P^:=''; N^:='..'; E^:='';
          end;
    if (Length(E^)>0) and (E^[1]='.') then begin
        for i:=1 to Length(E^)-1 do E^[i]:=E^[i+1];
        SetLength(E^, Length(E^)-1)
    end;
        {-DataCompBoy-}
    if (Options and flnUpCase)<>0 then begin
        UpStr(P^);
        UpStr(N^);
        UpStr(E^)
    end else if (Options and (flnLowCase or flnCapitalCase))<>0 then begin
        LowStr(P^);
        LowStr(N^);
        LowStr(E^);
        if (Options and flnCapitalCase)<>0 then N^[1]:=UpCase(N^[1])
    end;
    SetLength(R^, Size);
    if Size<5+ExtSize then begin
        FillR(1,Size,NameFormatChar[nfmCut]);
        if Delim<>nil then Delim^:=NameFormatChar[nfmCut]
        else Hi1:=Size;
        FormatLongName:=MakeResult;
        Dispose(R); Dispose(E); Dispose(N); Dispose(P);
        Exit
    end;
    PSize:=Length(P^);
    NSize:=Length(N^);
    ESize:=Length(E^);
    EFlag:=byte(ESize>0);
    if (Options and flnHardExtArea)<>0 then begin
        if ESize>ExtSize then ESize:=ExtSize;
        if PSize+NSize+1>Size-ExtSize then begin
            PSize:=Size-1-ExtSize-NSize;
            if PSize<3 then PSize:=3;
            if PSize>Length(P^) then PSize:=Length(P^)
        end;
        if PSize+NSize+1>Size-ExtSize then begin
            NSize:=Size-1-ExtSize-PSize;
            if NSize<1 then NSize:=1;
            if NSize>Length(N^) then NSize:=Length(N^)
        end
    end else begin
        if PSize+NSize+ESize+EFlag>Size then begin
            PSize:=Size-EFlag-ESize-NSize;
            if PSize<3 then PSize:=3;
            if PSize>Length(P^) then PSize:=Length(P^)
        end;
        if (Options and flnPreferName)<>0 then begin
            if PSize+NSize+ESize+EFlag>Size then begin
                ESize:=Size-EFlag-PSize-NSize;
                if ESize<0 then ESize:=0;
                if ESize>Length(E^) then ESize:=Length(E^)
            end;
            if PSize+NSize+ESize+EFlag>Size then begin
                NSize:=Size-EFlag-PSize-ESize;
                if NSize<1 then NSize:=1;
                if NSize>Length(N^) then NSize:=Length(N^)
            end
        end else begin
            if PSize+NSize+ESize+EFlag>Size then begin
                NSize:=Size-EFlag-PSize-ESize;
                if NSize<1 then NSize:=1;
                if NSize>Length(N^) then NSize:=Length(N^)
            end;
            if PSize+NSize+ESize+EFlag>Size then begin
                ESize:=Size-EFlag-PSize-NSize;
                if ESize<0 then ESize:=0;
                if ESize>Length(E^) then ESize:=Length(E^)
            end
        end
    end;
    FillR(1,Size+1,#32);
    Hi1:=0; Hi2:=0; Hi3:=0;
    if ExtSize>0 then begin
        if (Options and flnHardExtArea)<>0 then DotPos:=Size-ExtSize
        else DotPos:=Size-ESize
    end else DotPos:=PSize+NSize+1;
    if DotPos>Size then begin
        if Delim=nil then DotPos:=Size else DotPos:=Size+1
    end;
    for i:=1 to PSize do R^[i]:=P^[i];
    for i:=1 to NSize do R^[PSize+i]:=N^[i];
    for i:=1 to ESize do R^[DotPos+i]:=E^[i];
    if (((Options and flnAutoHideDot)<>0) and (DotPos=Size-ExtSize)
    and (ExtSize>0) or (Length(E^)=0))
    and (FormatMode=nfmNull) then begin end
    else if R^[DotPos]=#32 then R^[DotPos]:=NameFormatChar[FormatMode];
    if PSize<Length(P^) then begin
        if (Options and flnUseCutChar)<>0
        then R^[PSize]:=NameFormatChar[nfmCut];
        Hi1:=PSize
    end;
    if ESize<Length(E^) then begin
        if (Options and flnUseCutChar)<>0 then begin
            if Delim<>nil then Delim^:=NameFormatChar[nfmCut]
            else begin
                if DotPos+ESize<=Size then begin
                    R^[DotPos+ESize]:=NameFormatChar[nfmCut];
                    Hi3:=DotPos+ESize
                end else begin
                    if Delim<>nil then Delim^:=NameFormatChar[nfmCut]
                    else begin
                        R^[Size]:=NameFormatChar[nfmCut];
                        Hi3:=DotPos+ESize
                    end;
                end
            end
        end else Hi3:=DotPos+ESize
    end;
    if NSize<Length(N^) then begin
        if (Options and flnUseCutChar)<>0 then begin
            if Delim<>nil then begin
                Delim^:=NameFormatChar[nfmCut];
                R^[DotPos]:=N^[NSize+1]
            end else begin
                R^[DotPos]:=NameFormatChar[nfmCut];
                Hi2:=DotPos
            end;
        end else Hi2:=PSize+NSize
    end;
    if ((Options and flnSelected)<>0) then begin
        if DotPos<=Size then begin
            R^[DotPos]:=NameFormatChar[nfmSelected];
            Hi2:=DotPos
        end else begin
            R^[Size]:=NameFormatChar[nfmSelected];
            Hi2:=Size
        end;
    end;
    FormatLongName:=MakeResult;
    Dispose(R); Dispose(E); Dispose(N); Dispose(P)
end;

procedure CheckOS;
{$IFNDEF NONBP}
assembler; {piwamoto}
{ Currently Supports DesqView, Microsoft Windows and IBM's OS/2 }
asm
  xor   AL, AL
  mov   opSys, AL
{OS2}
(*  mov   ax, 3306h                                     *)
(*  int   21h           { get true dos version }        *)
(*  cmp   bl, 14h       { is it OS2 ? }                 *)
(*  jb    @nonOS2                                       *)
{Vadim Ochkin}
  mov   AX, 4010h
  int   2Fh
  cmp   AX, 4010h
  JZ    @nonOS2
{Vadim Ochkin}
  {OS2 detected}
  or    opSys, opOS2
@nonOS2:
  mov   ax, 3306h
  int   21h           { get true dos version }
{WinNT}{since WinNT don't support GetWinVer via int2f}
  cmp   bx,3205h
  jne   @nonWNT
  {Win NT and 2000 detected }
  or    opSys, opWNT+opWin
@nonWNT:
{DOS}
  cmp   bl,3
  jb    @nonDOS
  cmp   bl,7
  ja    @nonDOS
  {DOS detected}
  or    opSys, opDos
@nonDOS:
{DV}
  mov   ax, 2B01h
  mov   cx, 4445h
  mov   dx, 5351h
  int   21h           { Desqview Installed? }
  cmp   al, 255
  je    @nonDV
  {DesqView detected}
  or    opSys, opDV
@nonDV:
{WIN 3.x and 9x}
  mov   ax, 160Ah
  int   2Fh           { Windows Install? }
  or    ax,ax
  jne   @nonWin
  {Windows detected}
  or    opSys, opWin
@nonWin:
end; { checkos }
{$ELSE}
begin
  {$IFDEF WIN32}opSys := opWin;{$ENDIF}
  {$IFDEF OS2}  opSys := opOS2;{$ENDIF}
end;
{$ENDIF}

{$IFNDEF NONBP}
{PZ - 2000.03.26 - begin}

(****************************************************************
 *
 * FUNCTION:    GetCountryInfoEx
 *
 * PURPOSE:     This function reads country info data that are
 *              set in DOS.
 *
 * INPUTS:      CountryInfoEx - A buffer for country info data.
 *
 * OUTPUTS:     CountryInfoEx - A buffer filled with new data.
 *
 * RETURNS:     0     - Succesful.
 *              Other - DOS error code.
 *
 * NOTES:       PZ - 2000.04.27 - DPMI support added.
 *
 ****************************************************************)

function GetCountryInfoEx ( var CountryInfoEx : TCountryInfoEx ) : Integer;
assembler;
{$IFDEF DPMI}
type
  TReg32 = record
  case Integer of
    0 : (l, h : Byte);
    1 : (x    : Word);
    2 : (e    : Longint);
  end;
var
  RealRegs : record
    _EDI, _ESI, _EBP, Reserved, _EBX, _EDX, _ECX, _EAX : TReg32;
    Flags, _ES, _DS, _FS, _GS, _IP, _CS, _SP, _SS      : Word;
  end;
{$ENDIF}
asm
        PUSH    DS
        LDS     SI,CountryInfoEx
        LEA     DX,[SI].TCountryInfoEx.DateFormat
        MOV     AX,$3800
        INT     $21
        JC      @@2
        MOV     DS:[SI].TCountryInfoEx.CountryCode,BX
{$IFDEF DPMI}
        MOV     AX,WORD PTR DS:[SI].TCountryInfoEx.UpCaseCall
        MOV     RealRegs._IP,AX
        MOV     AX,WORD PTR DS:[SI].TCountryInfoEx.UpCaseCall+2
        MOV     RealRegs._CS,AX
        XOR     CX,CX
        MOV     RealRegs.Flags,CX
        MOV     RealRegs._SP,CX
        MOV     RealRegs._SS,CX
        MOV     AX,SS
        MOV     ES,AX
        LEA     DI,RealRegs
{$ENDIF}
        XOR     BX,BX
@@1:
        MOV     AL,BL
        OR      AL,$80
{$IFDEF DPMI}
        MOV     RealRegs._EAX.l,AL
        MOV     AX,$0301
        INT     $31
        JC      @@2
        MOV     AL,RealRegs._EAX.l
{$ELSE}
        CALL    DS:[SI].TCountryInfoEx.UpCaseCall
{$ENDIF}
        MOV     BYTE PTR DS:[SI+BX].TCountryInfoEx.UpperTable,AL
        INC     BL
        CMP     BL,$80
        JB      @@1
        XOR     AX,AX
@@2:
        POP     DS
end;

(****************************************************************
 *
 * FUNCTION:    FixCountryInfoEx
 *
 * PURPOSE:     This function fixes some data fields in given
 *              buffer with country info data to be more
 *              compatible with TCountryInfo structure.
 *
 * INPUTS:      CountryInfoEx - A buffer with country info data.
 *
 * OUTPUTS:     CountryInfoEx - A buffer with corrected data.
 *
 * RETURNS:     None.
 *
 * NOTES:       Function should be called before CopyCountryInfo.
 *
 ****************************************************************)

procedure FixCountryInfoEx ( var CountryInfoEx : TCountryInfoEx );
begin
  with CountryInfoEx do begin
    if DateFormat  > 2 then DateFormat  := 2;
    if TimeFormat  > 1 then TimeFormat  := 1;
    if MoneyDigits > 9 then MoneyDigits := 9;
    if MoneyFormat > 4 then Moneyformat := 4;
  end;
end;

(****************************************************************
 *
 * FUNCTION:    CopyCountryInfo
 *
 * PURPOSE:     This function copies data from TCountryInfoEx
 *              structure to TCountryInfo structure.
 *
 * INPUTS:      CountryInfoEx - A source data buffer.
 *              CountryInfo   - A destination data buffer.
 *
 * OUTPUTS:     CountryInfo   - Buffer filled with data from
 *                              CountryInfoEx structure.
 *
 * RETURNS:     None.
 *
 * NOTES:       Call FixCountryInfoEx function before to be sure
 *              that the extended country info is compatible
 *              with CountryInfo data.
 *
 ****************************************************************)

procedure CopyCountryInfo ( const CountryInfoEx : TCountryInfoEx; var CountryInfo : TCountryInfo );
begin
  CountryInfo.DateFmt     := CountryInfoEx.DateFormat;
  CountryInfo.TimeFmt     := CountryInfoEx.TimeFormat;
  CountryInfo.DateSep     := StrPas(@CountryInfoEx.DateSeparator);
  CountryInfo.TimeSep     := StrPas(@CountryInfoEx.TimeSeparator);
  CountryInfo.ThouSep     := StrPas(@CountryInfoEx.ThousandsSeparator);
  CountryInfo.DecSep      := StrPas(@CountryInfoEx.DecimalSeparator);
  CountryInfo.DecSign     := Chr(CountryInfoEx.MoneyDigits+Ord('0'));
  CountryInfo.Currency    := StrPas(@CountryInfoEx.MoneySymbol);
  CountryInfo.CurrencyFmt := CountryInfoEx.MoneyFormat;
  Move ( CountryInfoEx.UpperTable, CountryInfo.UpperTable, SizeOf(CountryInfo.UpperTable) );
end;

{PZ - end}
{$ENDIF}

{$IFNDEF VIRTUALPASCAL}
function MemOK: Boolean;
begin
  {MemOK := (MemAdjust(System.MemAvail) > $10000) and}
   MemOK := (MemAdjust(System.MemAvail) > $C000) and
           (System.MaxAvail > $6000);
end;
{$ENDIF}

function GetMeMemoStream: PStream; {-$VOL begin}
{$IFDEF USELONGSTRING}
const
  _1: Byte = 1;
{$ENDIF}
var
  S: PStream;
  Pos: longint;
begin
{$IFNDEF NOEXTRA} {DataCompBoy: In DPMI EMS and XMS is no needed}
  S:=nil;
  if XMSFound then begin
    Pos:=longint(XMSFree);
    if Pos>=512 then begin
      if Pos > 8192 then Pos := 8192;
      Pos := LongInt(Pos-1) shl 10;
      if Pos > 10240 then Pos := 10240;
      S:=New(PXMSStream,Init(Pos,0));
    end;
  end;
  if (S=nil) and EMSFound then begin
    Pos:=longint(EMSFreePages);
    if Pos>=32 then begin
      if Pos > 512 then Pos := 512;
      Pos := (Pos-1) * LongInt(16384);
      if Pos > 10240 then Pos := 10240;
      S:=New(PEMSStream,Init(Pos,0));
    end;
  end;
  if S=nil then
{$ENDIF}
    S:=New(PMemoryStream,Init(2048,2048));
{Cat: † ‚•Ø•‡Ï ß†Ø®Ë•¨ ‚„§† •§®≠®Á™„, Á‚Æ°Î §‡„£®• £´„ØÎ• Ø‡ÆÊ•§„‡Î,     }
{     ™Æ‚Æ‡Î• Á®‚†Ó‚ ®ß ØÆ‚Æ™† ‚Æ, Á‚Æ Æ≠® ‚„§† ≠• ß†Ø®·Î¢†´®, ·Á®‚†´®,  }
{     Á‚Æ ≠†Ë ØÆ‚Æ™ ·Æ§•‡¶®‚ §´®≠≠Î• ·‚‡Æ™®                              }
{$IFDEF USELONGSTRING}
  S^.Seek(0);
  S^.Write(_1, 1);
  S^.Seek(0);
{$ENDIF}
{/Cat}
  if S^.Status<>stOk then
    begin
      Dispose(S, Done);
      S:=nil
    end;
  GetMeMemoStream:=S;
end; {-$VOL end}

function MemAdjust(L: LongInt): LongInt;
begin
  if Linker <> nil then
  begin
    if L > CL_SafeBuf then L := L - CL_SafeBuf else L := 0;
  end;
  MemAdjust := L;
end;

 procedure FillWord(var B;Count, W: Word); assembler;
 {$IFNDEF BIT_32}
 asm
    cld
    mov  ax, W
    les  di, B
    mov  cx, Count
    rep  stosw
 end;
 {$ELSE BIT_32}{&Frame-}{$USES EDI, ECX}
 asm
    cld
    mov  eax, W
    mov  edi, B
    mov  ecx, Count
    rep  stosw
 end;
 {$ENDIF BIT_32}

procedure LocateCursor(X, Y: Byte);
{$IFDEF VIRTUALPASCAL}
begin GotoXY(X, Y) end;
{$ELSE}
assembler;
asm
        MOV     AH, 2
        XOR     BX, BX
        MOV     DL, X
        MOV     DH, Y
        INT     10H
end;
{$ENDIF}

{$IFNDEF BIT_32}
Function XDiv1024; assembler;
asm
  mov ax,word ptr X
  mov al,ah
  mov ah,dl
  mov dl,dh
  xor dh,dh
  shr dx,1
  rcr ax,1
  shr dx,1
  rcr ax,1
end;
{$ELSE BIT_32}{&Frame-}
Function XDiv1024; assembler;
asm
  mov eax,X
  shr eax,10
end;
{$ENDIF}

Function LongRatio;
begin
  While X > ( MaxLongInt div 100 ) do begin
    X := X div 128;
    Y := Y div 128;
  end;
  LongRatio := 100 * X div Y;
end;

FUNCTION Min; begin if x<y then Min:=X else Min:=Y end;
FUNCTION Max; begin if x<y then Max:=Y else Max:=X end;
FUNCTION Sgn; begin if x>0 then Sgn:=1 else if x<0 then Sgn:=-1 else Sgn:=0 end;

procedure TinySlice;
 {$IFDEF OS_DOS}
var Released: boolean;
 {$ENDIF}
begin
 {$IFNDEF OS_DOS}
 if (StartupData.Slice2 and osuOnceRelease <> 0) then Delay(1) {Delay(32)}
   else Delay(0);
 {$ELSE}
 Released:=false;
 if (StartupData.Slice  and osuInt15 <> 0) and
   ((StartupData.Slice2 and osuOnceRelease = 0) or not Released) and
    (OpSys and opDV <>0)
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
  end;
 if (StartupData.Slice and osuInt28 <> 0) and
   ((StartupData.Slice2 and osuOnceRelease = 0) or not Released)
  then asm int 28h end;
 {$ENDIF}
end;

{$IFNDEF VIRTUALPASCAL}
{-DataCompBoy-}
Procedure Delay(MS: longint);
var R: {$IFDEF DPMI}
       DPMIRegisters;
       {$ELSE}
       Registers;
       {$ENDIF}
begin
 R.CX:=MS shr 16;
 R.DX:=MS and $FFFF;
 {$IFDEF DPMI}SimulateRealModeInt{$ELSE}Intr{$ENDIF}($15, R);
end;
{-DataCompBoy-}
{$ENDIF}

BEGIN
 {$IFNDEF VIRTUALPASCAL}
 asm
  int 11h
  test al, 1
  jz  @@1
  mov cl, 6
  shr al, cl
  inc al
  mov NumFloppy, al
@@1:
 end;
 {$ELSE}
 NumFloppy := 0;
 if ValidDrive('A') then inc(NumFloppy);
 if ValidDrive('B') then inc(NumFloppy);
 {$ENDIF}
 UpcaseArray[#0] := #0;
 LowcaseArray[#0] := #0;
 Dos40 := Lo(DosVersion) >= 4;
 CheckOS;
 { Starting of OS/2 programs is possible under:              }
 { - OS/2 2.10+, OS2COMSPEC environment variable is required }
 { - OS/2 Warp 3+, no additional requirements                }
 { - OS/2 for PPC, future versions for IA64 (I hope)         }
{$IFDEF OS_DOS}
 OS2exec := (opsys and opOS2 <> 0) and
            ((Lo(DosVersion) > 20) or
             ((Lo(DosVersion) = 20) and
              ((Hi(DosVersion) >= 30) or
               ((Hi(DosVersion) >= 10) and (GetEnv('OS2COMSPEC') <> ''))
            )));
{$ENDIF}
{$IFDEF OS2}
 OS2exec := True;
{$ENDIF}
{$IFDEF Win32}
 OS2exec := False;
{$ENDIF}
END.
