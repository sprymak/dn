{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.07/DOS
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
{$IFDEF DEBUG}
   {$A-,B-,D+,E+,F-,G-,I-,L+,N-,O-,P-,Q+,R+,S+,T-,V+,X+}
{$ELSE}
   {$A-,B-,D-,E-,F-,G-,I-,L-,N-,O-,P-,Q-,R-,S-,T-,V+,X+}
{$ENDIF}
{$IFDEF DPMI} {$G+} {$ENDIF}
unit      Keyboard;

interface

const     kbF1        = $3B00;   kbF2        = $3C00;   kbAltA      = $1E00;
          kbF3        = $3D00;   kbF4        = $3E00;   kbAltB      = $3000;
          kbF5        = $3F00;   kbF6        = $4000;   kbAltC      = $2E00;
          kbF7        = $4100;   kbF8        = $4200;   kbAltD      = $2000;
          kbF9        = $4300;   kbF10       = $4400;   kbAltE      = $1200;
          kbF11       = $8500;   kbF12       = $8600;   kbAltF      = $2100;
          kbAltF1     = $6800;   kbAltF2     = $6900;   kbAltG      = $2200;
          kbAltF3     = $6A00;   kbAltF4     = $6B00;   kbAltH      = $2300;
          kbAltF5     = $6C00;   kbAltF6     = $6D00;   kbAltI      = $1700;
          kbAltF7     = $6E00;   kbAltF8     = $6F00;   kbAltJ      = $2400;
          kbAltF9     = $7000;   kbAltF10    = $7100;   kbAltK      = $2500;
          kbAltF11    = $8B00;   kbAltF12    = $8C00;   kbAltL      = $2600;
          kbShiftF1   = $5400;   kbShiftF2   = $5500;   kbAltM      = $3200;
          kbShiftF3   = $5600;   kbShiftF4   = $5700;   kbAltN      = $3100;
          kbShiftF5   = $5800;   kbShiftF6   = $5900;   kbAltO      = $1800;
          kbShiftF7   = $5A00;   kbShiftF8   = $5B00;   kbAltP      = $1900;
          kbShiftF9   = $5C00;   kbShiftF10  = $5D00;   kbAltQ      = $1000;
          kbShiftF11  = $8700;   kbShiftF12  = $8800;   kbAltR      = $1300;
          kbCtrlF1    = $5E00;   kbCtrlF2    = $5F00;   kbAltS      = $1F00;
          kbCtrlF3    = $6000;   kbCtrlF4    = $6100;   kbAltT      = $1400;
          kbCtrlF5    = $6200;   kbCtrlF6    = $6300;   kbAltU      = $1600;
          kbCtrlF7    = $6400;   kbCtrlF8    = $6500;   kbAltV      = $2F00;
          kbCtrlF9    = $6600;   kbCtrlF10   = $6700;   kbAltW      = $1100;
          kbCtrlF11   = $8900;   kbCtrlF12   = $8A00;   kbAltX      = $2D00;
          kbAltEqual  = $8300;   kbAltMinus  = $8200;   kbAltY      = $1500;
          kbAltSpace  = $0200;   kbAltEnter  = $1C00;   kbAltZ      = $2C00;
          kbAlt1      = $7800;   kbAlt2      = $7900;   kbAlt3      = $7A00;
          kbAlt4      = $7B00;   kbAlt5      = $7C00;   kbAlt6      = $7D00;
          kbAlt7      = $7E00;   kbAlt8      = $7F00;   kbAlt9      = $8000;
          kbAlt0      = $8100;   kbEsc       = $011B;   kbBack      = $0E08;
          kbUp        = $4800;   kbDown      = $5000;   kbPgUp      = $4900;
          kbPgDn      = $5100;   kbDel       = $5300;   kbLeft      = $4B00;
          kbRight     = $4D00;   kbHome      = $4700;   kbEnd       = $4F00;
          kbEnter     = $1C0D;   kbCtrlEnter = $1C0A;   kbGrayMinus = $4A2D;
          kbGrayPlus  = $4E2B;   kbShiftDel  = $0700;   kbShiftIns  = $0500;
          kbShiftTab  = $0F00;   kbTab       = $0F09;   kbCtrlLeft  = $7300;
          kbCtrlRight = $7400;   kbCtrlUp    = $8D00;   kbCtrlPgDn  = $7600;
          kbCtrlEnd   = $7500;   kbCtrlHome  = $7700;   kbCtrlIns   = $0400;
          kbCtrlDel   = $0600;   kbCtrlBack  = $0E7F;   kbCtrlR     = $1312;
          kbCtrlDown  = $9100;   kbIns       = $5200;   kbCtrlPgUp  = $8400;

const     SubsGray    : boolean = true;

function  KeyPressed : boolean;
function  ReadKey : char;
function  ReadFulKey : word;

implementation

function  KeyPressed : boolean; assembler;
asm
        mov     ah, 11h
        int     16h
        lahf
        and     ax, 4000h
        mov     cx, 2
        rol     ax, cl
        xor     ax, 1
end;

function  ReadFulKey : word; assembler;
asm
        mov     ah, 10h
        int     16h
        cmp     byte ptr [SubsGray], 0
        jz      @@2
        cmp     al, 0E0h
        jnz     @@1
        or      ah, ah
        jz      @@1
        xor     al, al
@@1:    cmp     ax, 0E00Dh
        jnz     @@2
        mov     ax, kbEnter
@@2:
end;

function ReadKey:char;
 begin
  ReadKey:=chr(lo(ReadFulKey));
 end;

end.
