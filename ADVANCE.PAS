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
//////////////////////////////////////////////////////////////////////////
//
//      LFN®ß†Ê®Ô ¢ÎØÆ´≠•≠† Ä≠‚Æ≠Æ¨ î•§ÆpÆ¢Î¨ aka DataCompBoy'•¨.
//                  Ç Á•·‚Ï å†‚†pÎ™®≠Æ© ì´ÏÔ≠Î...
//////////////////////////////////////////////////////////////////////////}

{$A-,B-}

UNIT Advance;

INTERFACE

USES Dos,{DnIni,}Strings,
     Objects,
     LFN{DataCompBoy};

type TNameFormatMode=(nfmNull,nfmSelected,nfmCut,nfmHidden,nfmSystem);

const
    NameFormatChar:array [TNameFormatMode] of char=(#46,#251,#16,#177,#178);

    flnPreferName   = 1;        {Ö·´® ≠•‚ flnHardExt ‚Æ y ®¨•≠® Øp®Æp®‚•‚ ≠†§ p†·Ë®p•≠®•¨}
    flnHardExtArea  = 2;
    flnAutoHideDot  = 4;        {Ö·´® ‚ÆÁ™† ¢ ®¨•≠® pÆ¢≠Æ ≠† ExtSize Æ‚ ™Æ≠Ê†, ‚Æ •• ≠• ØÆ™†ß.}
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


  TSize = Comp;
  CharSet = set of Char;
  Str2 = string[2];   PStr2  = ^Str2;
  Str3 = string[3];   PStr3  = ^Str3;
  Str4 = string[4];   PStr4  = ^Str4;
  Str5 = string[5];   PStr5  = ^Str5;
  Str6 = string[6];   PStr6  = ^Str6;
  Str8 = string[8];   PStr8  = ^Str8;
  Str12 = string[12]; PStr12 = ^Str12;
  Str40 = string[40]; PStr40 = ^Str40;
  Str50 = string[50]; PStr50 = ^Str50;
{ TCharImage = array [0..15] of Byte;}
  TDCountryInfo = record
                    DateFmt: Word;
                    Currency: array[1..5] of Char;
                    ThSep: array[1..2] of Char;
                    DecSep: array[1..2] of Char;
                    DateSep: array[1..2] of Char;
                    TimeSep: array[1..2] of Char;
                    CurFmt: Byte;
                    CurDec: Word;
                    TimeFmt: Byte;
                    CaseTbl: Pointer;
                    DataSep: array[1..2] of Char;
                    Zeros: array[1..10] of Byte;
                  end;

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
{ JustRegistered: Boolean = Off;}

const
  opUnk  = 0;  { Unk     }
  opDOS  = 1;  { DOS     }
  opOS2  = 2;  { OS/2    }
  opWin  = 3;  { Wind0ze }
  opDV   = 4;  { DV      }

  ops : byte = opUnk;

procedure CheckOS; { Check for OS - For checking API please use error codes }
                   { Automatically called while program started }

Procedure ClrIO;

Function  Cut(p : string; len : Byte ) : string;
Function  CutH(p : string; len : Byte ) : string;

Procedure Delay(MS: longint); {DataCompBoy}

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
     'êëíìîïñóòôöõúùûüÚÛÙıˆ˜¯˘˙˚¸˝˛'#255; {-$VIV}
     LowerString: string =
     (#1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16 +
     #17#18#19#20#21#22#23#24#25#26#27#28#29#30#31#32 +
     '!"#$%&''()*+,-./0123456789:;<=>?@' +
     'abcdefghijklmnopqrstuvwxyz[\]^_`' +
     'abcdefghijklmnopqrstuvwxyz{|}~' +
     '†°¢£§•¶ß®©™´¨≠ÆØ‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔ' +
     '†°¢£§•¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ'+
     '‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÒÚÛÙıˆ˜¯˘˙˚¸˝˛'#255); {-$VIV}
       {-DataCompBoy-}

      BreakChars  : Set of Char = [',',' ','[',']','{','}','(',')',':',';','.','^',
                                   '&','*','!','#','$','/','\','''','"','%','>','<',
                                   '-','+','=','|','?',#13,#10,#9,#26,#12,'@'];

      QuickExecExternalStr: string='';
      UpcaseInit : Boolean = Off;
      HexStr     : string[16] = '0123456789ABCDEF';
      LoHexChar  : array[0..$F] of char='0123456789abcdef';
      N_O_E_M_S  : string[5] = 'NOEMS';
         cTEMP_  : string[5] = 'TEMP:';
         cLINK_  : string[5] = 'LINK:';
         x_x     : string[3] = '*.*';

var
    ShiftState: Byte absolute $40:$17;
    Abort : Boolean;
    DOS40 : Boolean;
    OS210 : Boolean;
    FreeStr  : string;
    FreeByte : byte; {DataCompBoy 8)}
    ActiveDir: String; {DataCompBoy}
    DNNumber: Byte;
    UpCaseArray: array[Char] of Char Absolute UpperString ;
    LowCaseArray: array[Char] of Char Absolute LowerString ;
    Tran: array[Char] of Char;

       {-DataCompBoy-}
  StartupDir: String;
  SourceDir:  String;
  TempDir:    String;
  TempFile:   String;
  LngFile:    String;
  SwpDir:     String;
       {-DataCompBoy-}

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
                 'êëíìîïñóòôöõúùûüÒÚÛÙıˆ˜¯˘˙˚¸˝˛'#255
    );

{         CountryInfo: record
                        Id: Byte;
                        Size: Word;
                        CoutryID: Word;
                        CodePage: Word;
                        DateFormat: Word;
                        Currency: array[1..5] of Char;
                        thouasands: Word;
                        decimal: Word;
                        DateSep: Word;
                        TimeSep: Word;
                        CurrencyFlags: Word;
                        CurDecimals: Word;
                        TimeForm: Byte;
                        CaseMap: Pointer;
                        DataList: Word;
                        Zeros: array [1..10] of Byte;
                        Length: Word;
                     end;
}
var  DCountryInfo: TDCountryInfo;

type TPosArray = Array ['1'..'9'] of TPoint;

IMPLEMENTATION

uses xTime, u_keymap,
     Startup {DataCompBoy}
     ,advance3, advance1
     ;

Procedure ClrIO;
begin
 InOutRes := 0;
 DosError := 0;
 Abort    := Off;
end;

function Cut;
begin
 (* X-Man *)
 Cut:=FormatLongName(P,Len,0,flnPreferName+flnUseCutChar,nfmNull,nil)
end;

function CutH;
begin
 (* X-Man *)
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
  CurLen: byte absolute CurStr;
  BufBeg: Integer;
  Was: Boolean;

procedure aStr(D: Integer);
var
  Grow: Integer;
begin
  Grow := Min((BufPos - BufBeg) - D - Byte(Was), 255-CurLen);
  if Grow > 0 then
  begin
    Move(Buf[BufBeg], CurStr[CurLen+1], Grow);
    Inc(CurLen, Grow);
  end;
end;

var
  PrevC: Integer;
  C: Char;

begin
 CurStr := ''; if not Eof then
 begin
  PrevC := -1;
  CurLen := 0; BufBeg := BufPos; Was := False;
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
        New(Res); Res^[0]:=#0;
        if (Options and flnPadRight)=0
        then while R^[Byte(R^[0])]=#32 do Dec(R^[0]);
        if ((Options and flnSelected)<>0) and
        ((Options and flnHighlight)<>0) then AddStr(Res^,'~');
        for i:=1 to Length(R^) do begin
            if ((i=Hi1) or (i=Hi2) or (i=Hi3)) and
            ((Options and flnHighlight)<>0) then AddStr(Res^,'~');
            if (R^[i]='~') and ((Options and flnHandleTildes)<>0)
            then AddStr(Res^,#0);
            AddStr(Res^,R^[i]);
            if (R^[i]='~') and ((Options and flnHandleTildes)<>0)
            then AddStr(Res^,#0);
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
        Dec(E^[0])
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
    R^[0]:=Char(Size);
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

procedure CheckOS; assembler;
{ Currently Supports DesqView, Microsoft Windows and IBM's OS/2 }
asm
  mov   ops, opDOS   { Default DOS }
  mov   ah, 30h      { AH = 30h }
  int   21h          { dos version }
  cmp   al, 14h
  jae   @IBMOS2      { Jump if >= to 20 }

  mov   ax, 2B01h
  mov   cx, 4445h
  mov   dx, 5351h
  int   21h           { Desqview Installed? }
  cmp   al, 255
  jne   @DesqView     { Jump if AL <> 255 }

  mov   ax, 160Ah
  int   2Fh           { Windows Install?}
  cmp   ax, 0h
  je    @Windows      { If = Jump to Windows }

  jmp   @Finish       { Nothing found, go to the end }

@IBMOS2:
  mov   ops, opOS2    { Set OS Value }
  jmp   @Finish

@DesqView:
  mov   ops, opDV     { Set OS Value }
  jmp   @Finish

@Windows:
  mov   ops, opWin    { Set OS Value }
  jmp   @Finish

@FINISH:
end; { checkos }


{-DataCompBoy-}
var MSD: longint;
    QWE: longint;

Procedure Delay(MS: longint);
var j: longint;
begin
 while MS>0 do begin
  dec(MS);
  for j:=MSD downto 0 do begin
   Inc(QWE, 10);
   asm
    pusha
    popa
   end;
  end;
 end;
end;
{-DataCompBoy-}

BEGIN
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
 asm
    mov ax, $3800
    lea dx, DCountryInfo
    xor bx, bx
    int 21h
 end;
 UpcaseArray[#0] := #0;
 LowcaseArray[#0] := #0;
 Dos40 := Lo(DosVersion)>=4;
 OS210 := (Lo(DosVersion)>=20) and (GetEnv('OS2COMSPEC') <> '');
 CheckOS;

{-DataCompBoy-}
 MSD:=0;
 QWE:=LongInt(Ptr(Seg0040, $6C)^);
 while LongInt(Ptr(Seg0040, $6C)^)=QWE do begin
  Inc(MSD);
  asm
   pusha
   popa
  end;
 end;
 MSD:=Round(MSD * 0.018206);
{
   1                1           18.206
 ------ = MSD =>  ----- = MSD * ------
 18.206            1000          1000
}
{-DataCompBoy-}
END.
