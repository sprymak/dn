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

{---------------------------------------------------------------------------}
{                                                                           }
{ ASCII(DOS-866), ANSI(WIN-1251), KOI8-R CODEPAGES SUPPORT                  }
{                                                                           }
{ VIV (C) 1999                                                              }
{                                                                           }
{ PZ - 2000.03.25 - Totaly rebuild to save DATA segment space.              }
{                                                                           }
{---------------------------------------------------------------------------}

unit u_keymap;
interface

type
 TKeyMap = (kmNone, kmAscii, kmAnsi, kmKoi8r); {-$VIV}

{(c) SeYKo}
 TCodePageDetector = object
   constructor Init;
   destructor  Done; virtual;
   procedure   CheckString(const S: String);
   function    DetectedCodePage: TKeyMap;
 private
   Koi, KoiA, Win, WinA, Win2: LongInt;
 end;
{(c) SeYKo}

 TXlat = Array[Char] of Char;

 procedure NullXLAT(var X: TXlat);

 function  Ascii_Ansi ( const S : string ) : string;
 function  Ansi_Ascii ( const S : string ) : string;
 procedure Ascii_Ansi_Buf ( var Buf; L: Word );
 procedure Ansi_Ascii_Buf ( var Buf; L: Word );

 function  Ascii_Koi8r ( const S : string ) : string;
 function  Koi8r_Ascii ( const S : string ) : string;
 procedure Ascii_Koi8r_Buf ( var Buf; L: Word );
 procedure Koi8r_Ascii_Buf ( var Buf; L: Word );

 function  RusEngConv ( const S : string ) : string;
 function  GetToAsciiRecodeTable ( K : TKeyMap; var Buf ) : Boolean;

 procedure XlatBuf(var B; Len: Integer; var XTable);

implementation

  procedure XlatBuf(var B; Len: Integer; var XTable);
  assembler;
  {$IFNDEF BIT_32}
  asm
    cmp Len, 0
    jle @@1
    push ds
    mov cx, Len
    lds si, B
    les bx, XTable
    xor ah, ah
    cld
 @@Loop:
    lodsb
    mov di, ax
    mov al, es:[bx+di]
    mov ds:[si-1], al
    loop @@Loop
    pop  ds
 @@1:
  end;
  {$ELSE BIT_32}{&Frame-}{$USES ESI, EBX, ECX, EDI}
  asm
    cmp  Len, 0
    jle  @@1
    mov  ECX, Len
    mov  ESI, B
    mov  EBX, XTable
    xor  EAX, EAX
    cld
 @@Loop:
    lodsb
    mov  EDI, EAX
    mov  AL, [EBX+EDI]
    mov  [ESI-1], AL
    loop @@Loop
 @@1:
  end;
  {$ENDIF}

{(c) SeYKo}
constructor TCodePageDetector.Init;
begin
  (* inherited Init; *)
  Koi:=0; KoiA:=0; Win:=0; WinA:=0; Win2:=0;
end;

destructor TCodePageDetector.Done;
begin
  (* inherited Done; *)
end;

const
  WinSetA = [#224,#229,#232,#238,#243];
  KoiSetA = [#193,#197,#201,#207,#213];

procedure TCodePageDetector.CheckString(const S: String);
var
  C: Char;
  P1, P2: PChar;
begin
  P1:=@S[1]; P2:=P1+Length(S);
  while P1<P2 do
  begin
    C:=P1^; Inc(P1);
    if C >= #$C0 then
      if C <= #$DF then
      begin
        Inc(Koi); if C in KoiSetA then Inc(KoiA);
      end
      else begin
        Inc(Win); if C in WinSetA then Inc(WinA);
        if C >= #$F0 then Inc(Win2);
      end;
  end;
end;

function TCodePageDetector.DetectedCodePage: TKeyMap;
begin
  DetectedCodePage:=kmAscii;
  if (Koi<>0) and (KoiA<>0) and (Win<>0) and
     (Win >= Koi div 500) and (Win <= Koi div 5) and
     (KoiA >= Koi div 5)
  then DetectedCodePage:=kmKoi8r else
  if (Win<>0) and (WinA<>0) and (Koi<>0) and
     (Koi >= Win div 500) and (Koi <= Win div 5) and
     (WinA >= Win div 5) and (Win2 >= Win div 5)
  then DetectedCodePage:=kmAnsi;
end;
{(c) SeYKo}

 procedure NullXLAT; var C: Char; begin for c:=#0 to #255 do X[c]:=c; end;

type
  TConvertTable = array[0..255] of Byte;

 procedure CharsToTable ( var ConvertTable : TConvertTable; const AllChars : string );
 begin
   FillChar ( ConvertTable, SizeOf(ConvertTable), #$00 );
   Move ( AllChars[1], ConvertTable[1], Length(AllChars) );
 end;

 procedure CharsToBackTable ( var ConvertTable : TConvertTable; const AllChars : string );
 var
   i : Integer;
 begin
   FillChar ( ConvertTable, SizeOf(ConvertTable), #$00 );
   for i := Low(ConvertTable)+1 to High(ConvertTable) do
     ConvertTable[Ord(AllChars[i-Low(ConvertTable)])] := i;
 end;

 procedure ConvertBuf ( var Buf; Len : Word; const ConvertTable : TConvertTable );
 {$IFDEF  NOASM}
 var
   i : Integer;
   p : array [0..0] of Byte absolute Buf;
 begin
   for i := 0 to Len-1 do
     p[i] := ConvertTable[p[i]];
 end;
 {$ELSE  !NOASM}
 assembler;
 {$IFNDEF BIT_32}
 asm
         MOV     CX,Len
         JCXZ    @@2
         MOV     DX,DS
         LES     DI,Buf
         LDS     BX,ConvertTable
 @@1:
         MOV     AL,ES:[DI]
         XLAT
         STOSB
         LOOP    @@1
         MOV     DS,DX
 @@2:
 end;
 {$ELSE BIT_32}{&Frame-}{$USES EDI, EBX}
 asm
         MOV     ECX,Len
         JCXZ    @@2
         LEA     EDI,Buf
         LEA     EBX,ConvertTable
 @@1:
         MOV     AL,[EDI]
         XLAT
         STOSB
         LOOP    @@1
 @@2:
 end;
 {$ENDIF BIT}
 {$ENDIF !NOASM}

 procedure CharsConvertBuf ( var Buf; Len : Word; const AllChars : string );
 var
   ConvertTable : TConvertTable;
 begin
   CharsToTable ( ConvertTable, AllChars );
   ConvertBuf ( Buf, Len, ConvertTable );
 end;

 procedure CharsConvertBufBack ( var Buf; Len : Word; const AllChars : string );
 var
   ConvertTable : TConvertTable;
 begin
   CharsToBackTable ( ConvertTable, AllChars );
   ConvertBuf ( Buf, Len, ConvertTable );
 end;

 {ASCII-ANSI}

 function AnsiAllChars : string;
 begin
   AnsiAllChars
     :=    #$01#$02#$03#$04#$05#$06#$07#$08#$09#$0A#$0B#$0C#$0D#$0E#$0F
     + #$10#$11#$12#$13#$14#$15#$16#$17#$18#$19#$1A#$1B#$1C#$1D#$1E#$1F
     + #$20#$21#$22#$23#$24#$25#$26#$27#$28#$29#$2A#$2B#$2C#$2D#$2E#$2F
     + #$30#$31#$32#$33#$34#$35#$36#$37#$38#$39#$3A#$3B#$3C#$3D#$3E#$3F
     + #$40#$41#$42#$43#$44#$45#$46#$47#$48#$49#$4A#$4B#$4C#$4D#$4E#$4F
     + #$50#$51#$52#$53#$54#$55#$56#$57#$58#$59#$5A#$5B#$5C#$5D#$5E#$5F
     + #$60#$61#$62#$63#$64#$65#$66#$67#$68#$69#$6A#$6B#$6C#$6D#$6E#$6F
     + #$70#$71#$72#$73#$74#$75#$76#$77#$78#$79#$7A#$7B#$7C#$7D#$7E#$7F
     + #$C0#$C1#$C2#$C3#$C4#$C5#$C6#$C7#$C8#$C9#$CA#$CB#$CC#$CD#$CE#$CF
     + #$D0#$D1#$D2#$D3#$D4#$D5#$D6#$D7#$D8#$D9#$DA#$DB#$DC#$DD#$DE#$DF
     + #$E0#$E1#$E2#$E3#$E4#$E5#$E6#$E7#$E8#$E9#$EA#$EB#$EC#$ED#$EE#$EF
     + #$80#$81#$82#$83#$84#$85#$86#$87#$88#$89#$8A#$8B#$8C#$8D#$8E#$8F
     + #$90#$91#$92#$93#$94#$95#$96#$97#$98#$99#$9A#$9B#$9C#$9D#$9E#$9F
     + #$A0#$A1#$A2#$A3#$A4#$A5#$A6#$A7#$A9#$AA#$AB#$AC#$AD#$AE#$AF#$B0
     + #$F0#$F1#$F2#$F3#$F4#$F5#$F6#$F7#$F8#$F9#$FA#$FB#$FC#$FD#$FE#$FF
     + #$A8#$B8#$B1#$B2#$B3#$B4#$B5#$B6#$B7#$B9#$BA#$BB#$BC#$BD#$BE#$BF;
 end;

 function Ascii_Ansi ( const S : string ) : string;
 var
   result : string;
 begin
   result := S;
   CharsConvertBuf ( result[1], Length(result), AnsiAllChars );
   Ascii_Ansi := result;
 end;

 function Ansi_Ascii ( const S : string ) : string;
 var
   result : string;
 begin
   result := S;
   CharsConvertBufBack ( result[1], Length(result), AnsiAllChars );
   Ansi_Ascii := result;
 end;

 procedure Ascii_Ansi_Buf ( var Buf; L : Word );
 begin
   CharsConvertBuf ( Buf, L, AnsiAllChars );
 end;

 procedure Ansi_Ascii_Buf ( var Buf; L: Word );
 begin
   CharsConvertBufBack ( Buf, L, AnsiAllChars );
 end;

 {ASCII-KOI8R}

 function Koi8rAllChars : string;
 begin
   Koi8rAllChars
     :=    #$01#$02#$03#$04#$05#$06#$07#$08#$09#$0A#$0B#$0C#$0D#$0E#$0F
     + #$10#$11#$12#$13#$14#$15#$16#$17#$18#$19#$1A#$1B#$1C#$1D#$1E#$1F
     + #$20#$21#$22#$23#$24#$25#$26#$27#$28#$29#$2A#$2B#$2C#$2D#$2E#$2F
     + #$30#$31#$32#$33#$34#$35#$36#$37#$38#$39#$3A#$3B#$3C#$3D#$3E#$3F
     + #$40#$41#$42#$43#$44#$45#$46#$47#$48#$49#$4A#$4B#$4C#$4D#$4E#$4F
     + #$50#$51#$52#$53#$54#$55#$56#$57#$58#$59#$5A#$5B#$5C#$5D#$5E#$5F
     + #$60#$61#$62#$63#$64#$65#$66#$67#$68#$69#$6A#$6B#$6C#$6D#$6E#$6F
     + #$70#$71#$72#$73#$74#$75#$76#$77#$78#$79#$7A#$7B#$7C#$7D#$7E#$7F
     + #$E1#$E2#$F7#$E7#$E4#$E5#$F6#$FA#$E9#$EA#$EB#$EC#$ED#$EE#$EF#$F0
     + #$F2#$F3#$F4#$F5#$E6#$E8#$E3#$FE#$FB#$FD#$FF#$F9#$F8#$FC#$E0#$F1
     + #$C1#$C2#$D7#$C7#$C4#$C5#$D6#$DA#$C9#$CA#$CB#$CC#$CD#$CE#$CF#$D0
     + #$90#$91#$92#$81#$87#$B2#$B4#$A7#$A6#$B5#$A1#$A8#$AE#$AD#$AC#$83
     + #$84#$89#$88#$86#$80#$8A#$AF#$B0#$AB#$A5#$BB#$B8#$B1#$A0#$BE#$B9
     + #$BA#$B6#$B7#$AA#$A9#$A2#$A4#$BD#$BC#$85#$82#$8D#$8C#$8E#$8F#$8B
     + #$D2#$D3#$D4#$D5#$C6#$C8#$C3#$DE#$DB#$DD#$DF#$D9#$D8#$DC#$C0#$D1
     + #$B3#$A3#$99#$98#$93#$9B#$9F#$97#$9C#$95#$9E#$96#$BF#$9D#$94#$9A;
 end;

 function Ascii_Koi8r ( const S : string ): string;
 var
   result : string;
 begin
   result := S;
   CharsConvertBuf ( result[1], Length(result), Koi8rAllChars );
   Ascii_Koi8r := result;
 end;

 function Koi8r_Ascii ( const S : string ) : string;
 var
   result : string;
 begin
   result := S;
   CharsConvertBufBack ( result[1], Length(result), Koi8rAllChars );
   Koi8r_Ascii := result;
 end;

 procedure Ascii_Koi8r_Buf ( var Buf; L : Word );
 begin
   CharsConvertBuf ( Buf, L, Koi8rAllChars );
 end;

 procedure Koi8r_Ascii_Buf ( var Buf; L: Word );
 begin
   CharsConvertBufBack ( Buf, L, Koi8rAllChars );
 end;

 {RUS-ENG}

 function RusEngAllChars : string;
 begin
   RusEngAllChars
     :=    #$01#$02#$03#$04#$05#$06#$07#$08#$09#$0A#$0B#$0C#$0D#$0E#$0F
     + #$10#$11#$12#$13#$14#$15#$16#$17#$18#$19#$1A#$1B#$1C#$1D#$1E#$1F
     + #$20#$21#$9D#$23#$24#$25#$26#$ED#$28#$29#$2A#$2B#$A1#$2D#$EE#$2F
     + #$30#$31#$32#$33#$34#$35#$36#$37#$38#$39#$86#$A6#$81#$3D#$9E#$3F
     + #$40#$94#$88#$91#$82#$93#$80#$8F#$90#$98#$8E#$8B#$84#$9C#$92#$99
     + #$87#$89#$8A#$9B#$85#$83#$8C#$96#$97#$8D#$9F#$E5#$5C#$EA#$5E#$5F
     + #$F1#$E4#$A8#$E1#$A2#$E3#$A0#$AF#$E0#$E8#$AE#$AB#$A4#$EC#$E2#$E9
     + #$A7#$A9#$AA#$EB#$A5#$A3#$AC#$E6#$E7#$AD#$EF#$95#$7C#$9A#$F0#$7F
     + #$46#$3C#$44#$55#$4C#$54#$3A#$50#$42#$51#$52#$4B#$56#$59#$4A#$47
     + #$48#$43#$4E#$45#$41#$7B#$57#$58#$49#$4F#$7D#$53#$4D#$22#$3E#$5A
     + #$66#$2C#$64#$75#$6C#$74#$3B#$70#$62#$71#$72#$6B#$76#$79#$6A#$67
     + #$B0#$B1#$B2#$B3#$B4#$B5#$B6#$B7#$B8#$B9#$BA#$BB#$BC#$BD#$BE#$BF
     + #$C0#$C1#$C2#$C3#$C4#$C5#$C6#$C7#$C8#$C9#$CA#$CB#$CC#$CD#$CE#$CF
     + #$D0#$D1#$D2#$D3#$D4#$D5#$D6#$D7#$D8#$D9#$DA#$DB#$DC#$DD#$DE#$DF
     + #$68#$63#$6E#$65#$61#$5B#$77#$78#$69#$6F#$5D#$73#$6D#$27#$2E#$7A
     + #$7E#$60#$F2#$F3#$F4#$F5#$F6#$F7#$F8#$F9#$FA#$FB#$FC#$FD#$FE#$FF;
 end;

 function RusEngConv ( const S : string ): string;
 var
   result : string;
 begin
   result := S;
   CharsConvertBuf ( result[1], Length(result), RusEngAllChars );
   RusEngConv := result;
 end;


 function GetToAsciiRecodeTable ( K : TKeyMap; var Buf ) : Boolean;
 begin
   GetToAsciiRecodeTable := False;
   case K of
     kmAnsi  : CharsToBackTable ( TConvertTable(Buf), AnsiAllChars );
     kmKoi8r : CharsToBackTable ( TConvertTable(Buf), Koi8rAllChars );
   else Exit;
   end;
   GetToAsciiRecodeTable := True;
 end;

 end.
