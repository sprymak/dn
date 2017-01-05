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
UNIT VideoMan;

INTERFACE

{$IFDEF VIRTUALPASCAL}
var ScreenMirror: array[0..16383] of Byte;
procedure DetectVideo;
{$ENDIF}

Procedure SetScrMode(Mode: Word);
Procedure SetBlink(Mode : boolean);
{$IFNDEF NONBP}
Procedure GetCrtMode; { used to fixup screen before application starts }
{$ENDIF}

{ ******** SCREEN MANAGER ******** }

type

  vga_pal = array [1..3,0..15] of Byte  ;

const

 { VGA palette }

  VGA_palette : vga_pal = (
  { red   } (0,0 ,0 ,7 ,42,42,42,42,19,21, 0,19,63,63,53,63),
  { green } (0,21,42,42,7 ,7 ,28,42,19,21,63,55,21,21,57,63),
  { blue  } (0,42,21,42,7 ,42,0 ,42,19,63,32,55,21,63,18,63)
                          ) ;
  { default values }

  VGA_Default : vga_pal = (
  { red   } (0,0,0,0,42,42,42,42,21,21,21,21,63,63,63,63),
  { green } (0,0,42,42,0,0,21,42,21,21,63,63,21,21,63,63),
  { blue  } (0,42,0,42,0,42,0,42,21,63,21,63,21,63,21,63)
                          ) ;
  { palette slot }
  SL     : Array[0..15] of Byte =(0,1,2,3,4,5,20,7,56,57,58,59,60,61,62,63);

(*****************************************************************
 *
 * Video Device Type
 *
 *****************************************************************)

type

  TVideoType = (
    vtUnknown,
    vtMONO,
    vtCGA,
    vtEGA,
    vtVGA,
    vtXGA,
    vtSVGA,
    vtVBE2
  );

var

  VideoType : TVideoType;

(*****************************************************************
 *
 * Screen Modes
 *
 *****************************************************************)

const

  smBW40  = $0000;
  smCO40  = $0001;      { CGA }
  smBW80  = $0002;
  smCO80  = $0003;      { CGA,EGA,VGA }
  smMONO  = $0007;      { CGA,EGA,VGA }

  sm40x12 = $80;        { VGA }
  sm40x14 = $81;        { VGA }
  sm40x25 = $82;        { CGA,EGA,VGA }
  sm40x30 = $83;        { VGA }
  sm40x34 = $84;        { VGA }
  sm40x43 = $85;        { VGA }
  sm40x50 = $86;        { VGA }
  sm40x60 = $87;        { VGA }

  sm80x12 = $90;        { VGA }
  sm80x14 = $91;        { VGA }
  sm80x25 = $92;        { VGA }
  sm80x30 = $93;        { VGA }
  sm80x34 = $94;        { VGA }
  sm80x43 = $95;        { EGA,VGA }
  sm80x50 = $96;        { VGA }
  sm80x60 = $97;        { VGA }

  sm94x12 = $A0;        { VGA }
  sm94x14 = $A1;        { VGA }
  sm94x25 = $A2;        { VGA }
  sm94x30 = $A3;        { VGA }
  sm94x34 = $A4;        { VGA }
  sm94x43 = $A5;        { VGA }
  sm94x50 = $A6;        { VGA }
  sm94x60 = $A7;        { VGA }
  smNonStandard = $00FF;

  NonStandardModes = True;

const

  smSVGALo : Word = $109;
  smSVGAHi : Word = $10C;

{ Screen manager routines }

procedure InitVideo;
procedure DoneVideo;
procedure SetVideoMode(Mode: Word);
procedure ClearScreen;

Procedure ResetVGApalette( Update : Boolean );  { reset palette to default}
Procedure GetPalette(var Buf); { fill buff with palette 64 bytes   }
Procedure SetPalette(var Buf); { set palette using buf 64 bytes    }
Function  VGASystem: Boolean;
                               {Knave begin}
{$IFNDEF VIRTUALPASCAL}
Procedure FadePalette;
Procedure GlowPalette;
Procedure BlackPalette;
{$ENDIF}

Procedure Set_palette(color,r,g,b :Byte);
Procedure Get_palette(color :Byte; Var r,g,b :Byte);
                               {Knave end}

const
  EquipmentOfs  = $0010;        { Word }
  CrtColsOfs    = $004A;        { Byte }
  CrtRowsOfs    = $0084;        { Byte }
  CrtCharOfs    = $0085;        { Byte }
  CrtInfoOfs    = $0087;        { Byte }
  CrtPSizOfs    = $004C;        { Word }

IMPLEMENTATION
{$IFNDEF VIRTUALPASCAL}
uses DnIni, Drivers, Objects, DnApp, Startup, Advance, Commands
     {$IFDEF NOASM},Dos{$ENDIF}
     {$IFDEF DPMI},Dpmi, DosMem{$ENDIF}
;

(*****************************************************************
 *
 * S C R E E N     M A N A G E R
 *
 * Rewritten by Pawel Ziemian on 2000.06.14
 *
 *****************************************************************)

{ Save registers and call video interrupt }

procedure VideoInt; near;
assembler;
asm
        PUSH    BP
        PUSH    ES
        INT     $10
        POP     ES
        POP     BP
end;

type
  PVideoInfo = ^TVideoInfo;
  TVideoInfo = record
  case Integer of
    0 : ( ID    : array [1..4] of Char );
    1 : ( XAttr : Word );
    3 : ( Bytes : array [0..512] of Byte );
  end;

{$IFDEF DPMI}

{Doesn't work under Windows 3.1. Don't use in Windows!}
function AllocDosMem(SizeInParas : Word;
                     var RealModeSeg : Word;
                     var ProtModeSel : Word) : Word; Assembler;
asm
  mov     bx,SizeInParas;
  mov     ax,0100h;
  int     31h;
  jc      @@ExitPoint;
  les     di,RealModeSeg;
  mov     es:[di],ax;
  les     di,ProtModeSel;
  mov     es:[di],dx;
  xor     ax,ax;
@@ExitPoint:
end;

{Doesn't work under Windows 3.1. Don't use in Windows!}
function FreeDosMem(ProtModeSel : Word) : Word; Assembler;
asm
  mov     ax,0101h;
  mov     dx,ProtModeSel;
  int     31h;
  jc      @@ExitPoint;
  xor     ax,ax;
@@ExitPoint:
end;

{$ENDIF}

{$IFNDEF DPMI}
procedure DetectVesaType;
var
  V : TVideoInfo;
begin
  If not DoVESATest then exit;
  V.ID :='QQQQ';
  asm
        MOV     AX,$4E00
        PUSH    SS
        POP     ES
        LEA     DI,V
        CALL    VideoInt
  end;
  if V.ID = 'VESA' then begin
    VideoType := vtXGA;
  end else begin
    asm
        MOV     AX,$4F00
        PUSH    SS
        POP     ES
        LEA     DI,V
        CALL    VideoInt
    end;
    if V.ID = 'VESA' then begin
      VideoType := vtSVGA;
      V.ID      := 'VBE2';
      asm
        MOV     AX,$4F00
        PUSH    SS
        POP     ES
        LEA     DI,V
        CALL    VideoInt
      end;
      if V.ID = 'VESA' then begin
        VideoType := vtVBE2;
      end;
    end;
  end;
end;
{$ELSE}
procedure DetectVesaType;
var
  P    : PVideoInfo;
  RSeg : Word;
  PSel : Word;
  R    : DPMIRegisters;
begin
  If not DoVESATest then exit;
  FillChar(R, SizeOf(R), 0);
  if AllocDosMem ( (SizeOf(TVideoInfo)+15) shr 4, RSeg, PSel ) <> 0 then
    Exit;
  R.DI  := 0;
  R.ES  := RSeg;
  P     := Ptr ( PSel, 0 );
  P^.ID := 'QQQQ';
  R.AX  := $4E00;
  SimulateRealModeInt ( $10, R );
  if P^.ID = 'VESA' then begin
    VideoType := vtXGA;
  end else begin
    R.AX := $4F00;
    SimulateRealModeInt ( $10, R );
    if P^.ID = 'VESA' then begin
      VideoType := vtSVGA;
      P^.ID     := 'VBE2';
      R.AX      := $4F00;
      SimulateRealModeInt ( $10, R );
      if P^.ID = 'VESA' then begin
        VideoType := vtVBE2;
      end;
    end;
  end;
  FreeDosMem ( PSel );
end;
{$ENDIF}

procedure DetectVideoType;
assembler;
asm
        CMP     VideoType,vtUnknown
        JNE     @@Done
        MOV     VideoType,vtMONO
        {$IFNDEF BIT_32}
        XOR     AX,AX
        {$ELSE}
        XOR     EAX,EAX
        {$ENDIF}
        INT     $11
        AND     AL,$30
        CMP     AL,$30
        JE      @@Done
        MOV     VideoType,vtCGA
        MOV     AH,$12
        MOV     BX,$FF10
        CALL    VideoInt
        CMP     BH,$FF
        JE      @@Done
        MOV     VideoType,vtEGA
        MOV     AX,$1C00
        MOV     CX,7
        CALL    VideoInt
        CMP     AL,$1C
        JE      @@vga
        MOV     AX,$1200                { VGA, MCGA, enable video addressing }
        MOV     BL,$32
        CALL    VideoInt
        CMP     AL,$12
        JNE     @@Done                  { leave EGA Crt type }
@@vga:
        MOV     VideoType,vtVGA
        CALL    DetectVesaType
@@Done:
end;

{$IFNDEF DPMI}
function GetVesaMode : Word;
assembler;
asm
        MOV     AX,$4E03
        CMP     VideoType,vtXGA
        JE      @@1
        MOV     AX,$4F03
        CMP     VideoType,vtSVGA
        JB      @@2
@@1:
        XOR     DH,DH
        MOV     DL,AH
        CALL    VideoInt
        CMP     AX,DX
        JE      @@3
@@2:
        MOV     BX,$FFFF
@@3:
        MOV     AX,BX
end;
{$ELSE}
function GetVesaMode : Word;
var
  R : DPMIRegisters;
begin
  GetVesaMode := $FFFF;
  R.SS        := $0000;
  R.SP        := $0000;
  if VideoType = vtXGA then begin
    R.AX := $4E04;
    R.DX := $4E;
  end else if VideoType >= vtSVGA then begin
    R.AX := $4F03;
    R.DX := $4F;
  end else begin
    Exit;
  end;
  SimulateRealModeInt ( $10, R );
  if R.AX = R.DX then begin
    GetVesaMode := R.BX;
  end;
end;
{$ENDIF}

procedure GetCrtMode;
assembler;
asm
        CALL    DetectVideoType
        CALL    GetVesaMode
        CMP     AX,$0100
        JB      @@0
        CMP     AX,$FFFF
        JNE     @@1
        MOV     AH,0Fh
        CALL    VideoInt
@@0:
        AND     AX,$007F
@@1:
        MOV     ES,Seg0040
        MOV     CL,byte ptr ES:[CrtCharOfs] { char height }
        MOV     DL,byte ptr ES:[CrtRowsOfs] { screen rows }
        MOV     DH,byte ptr ES:[CrtColsOfs] { screen cols }
        CMP     AX,$0100
        JAE     @@2                         { VESA keeps number of rows }
        INC     DL                          { OEM keeps max row number  }
@@2:
        LEA     SI,@@5[-3]
        CMP     AX,smCO80                   { BIOS mode for ext. modes  }
        JE      @@3
        LEA     SI,@@4[-3]
        CMP     AX,smCO40
        JNE     @@6
@@3:
        ADD     SI,3
        CMP     CS:[SI],AH
        JE      @@6
        CMP     CS:[SI],DX
        JNE     @@3
        MOV     AL,CS:[SI+2]
        JMP     @@6
@@4:
        DB      12, 40, sm40x12
        DB      14, 40, sm40x14
        DB      25, 40, sm40x25
        DB      30, 40, sm40x30
        DB      34, 40, sm40x34
        DB      43, 40, sm40x43
        DB      50, 40, sm40x50
        DB      60, 40, sm40x60
        DB       0
@@5:
        DB      12, 80, sm80x12
        DB      14, 80, sm80x14
        DB      25, 80, sm80x25
        DB      30, 80, sm80x30
        DB      34, 80, sm80x34
        DB      43, 80, sm80x43
        DB      50, 80, sm80x50
        DB      60, 80, sm80x60

        DB      12, 94, sm94x12
        DB      14, 94, sm94x14
        DB      25, 94, sm94x25
        DB      30, 94, sm94x30
        DB      34, 94, sm94x34
        DB      43, 94, sm94x43
        DB      50, 94, sm94x50
        DB      60, 94, sm94x60
        DB       0
@@6:
end;

{$IFNDEF DPMI}
function CheckVesaMode ( Mode : Word ) : Word;
assembler;
var
  V : TVideoInfo;
asm
        CMP     VideoType,vtXGA
        JB      @@2
        MOV     AX,$4E02
        MOV     SI,$0C
        JE      @@1
        MOV     AX,$4F01
        MOV     SI,$1B
@@1:
        XOR     BH,BH
        MOV     BL,AH
        MOV     CX,Mode
        PUSH    SS
        POP     ES
        LEA     DI,V
        CALL    VideoInt
        CMP     AX,BX
        JNE     @@2
        MOV     BX,SI
        CMP     BYTE PTR ES:[DI+BX],0
        MOV     AX,Mode
        JE      @@3
@@2:
        MOV     AX,$FFFF
@@3:
end;
{$ELSE}
function CheckVesaMode ( Mode : Word ) : Word;
var
  P    : PVideoInfo;
  RSeg : Word;
  PSel : Word;
  R    : DPMIRegisters;
begin
  CheckVesaMode := $FFFF;
  if VideoType < vtXGA then
    Exit;
  if AllocDosMem ( (SizeOf(TVideoInfo)+15) shr 4, RSeg, PSel ) <> 0 then
    Exit;
  R.DI  := 0;
  R.ES  := RSeg;
  R.SS  := 0;
  R.SP  := 0;
  P     := Ptr ( PSel, 0 );
  if VideoType = vtXGA then begin
    R.AX := $4E02;
    R.CX := Mode;
    R.DX := $0000;
    SimulateRealModeInt ( $10, R );
    if (R.AX = $004E) and (P^.Bytes[$0C] = 0) then
      CheckVesaMode := Mode;
  end else if VideoType >= vtSVGA then begin
    R.AX := $4F01;
    R.CX := Mode;
    SimulateRealModeInt ( $10, R );
    if (R.AX = $004F) and (P^.Bytes[$1B] = 0) then
      CheckVesaMode := Mode;
  end;
  FreeDosMem ( PSel );
end;
{$ENDIF}

function FixCrtMode ( Mode : Word ) : Word;
assembler;
asm
        MOV     AX,Mode
        OR      AH,AH
        JNE     @@2
        PUSH    CS
        POP     ES
        LEA     DI,@@3
        MOV     CX,27                   { Number of valid modes }
        CMP     VideoType,vtVGA
        JAE     @@1
        MOV     CX,6
        CMP     VideoType,vtEGA
        JAE     @@1
        MOV     CX,5
@@1:
        REPNE   SCASB
        JE      @@4
@@2:
        PUSH    AX
        CALL    CheckVesaMode
        CMP     AX,Mode
        JE      @@4
        MOV     AX,smCO80
        JMP     @@4
@@3:
        DB      smMONO
        DB      smCO40
        DB      smCO80
        DB      sm40x25
        DB      sm80x25
        DB      sm80x43
        DB      sm40x12
        DB      sm40x14
        DB      sm40x30
        DB      sm40x34
        DB      sm40x43
        DB      sm40x50
        DB      sm40x60
        DB      sm80x12
        DB      sm80x14
        DB      sm80x30
        DB      sm80x34
        DB      sm80x50
        DB      sm80x60
        DB      sm94x12
        DB      sm94x14
        DB      sm94x25
        DB      sm94x30
        DB      sm94x34
        DB      sm94x43
        DB      sm94x50
        DB      sm94x60
@@4:
end;

{$IFNDEF DPMI}
procedure SetVesaMode ( Mode : Word );
assembler;
var
  V : TVideoInfo;
asm
        CMP     VideoType,vtXGA
        JB      @@2
        MOV     AX,$4F02
        JA      @@1
        MOV     AX,$4E02
        XOR     CX,CX
        XOR     DX,DX
        PUSH    SS
        POP     ES
        LEA     DI,V
@@1:
        MOV     BX,Mode
        CALL    VideoInt
@@2:
end;
{$ELSE}
procedure SetVesaMode ( Mode : Word );
var
  P    : PVideoInfo;
  RSeg : Word;
  PSel : Word;
  R    : DPMIRegisters;
begin
  if VideoType = vtXGA then begin
    if AllocDosMem ( (SizeOf(TVideoInfo)+15) shr 4, RSeg, PSel ) <> 0 then
      Exit;
    R.DI  := 0;
    R.ES  := RSeg;
    R.SS  := 0;
    R.SP  := 0;
    P     := Ptr ( PSel, 0 );
    R.AX  := $4E02;
    R.BX  := Mode;
    R.CX  := $0000;
    R.DX  := $0000;
    SimulateRealModeInt ( $10, R );
    FreeDosMem ( PSel );
  end else if VideoType >= vtSVGA then begin
    R.AX  := $4F02;
    R.BX  := Mode;
    SimulateRealModeInt ( $10, R );
  end;
end;
{$ENDIF}

procedure _VGAoff; near; assembler;
asm
        mov   dx,3C4h           { EGA Sequencer         }
        mov   al,0              { reset register        }
        out   dx,al
        mov   al,1              { synchronous reset VGA }
        inc   dx
        out   dx,al

        mov   dx,3D4h           { CRT Control Register  }
        mov   al,23             { CR17 mode register    }
        out   dx,al
        inc   dx
        in    al,dx
        and   al,127            { CRTC reset and stop   }
        out   dx,al
        dec   dx

        mov   al,17             { end vertical retrace  }
        out   dx,al
        inc   dx
        in    al,dx
        and   al,127            { enable wr access 0-7  }
        out   dx,al
end;

procedure _VGAon; near; assembler;
asm
        mov   dx,3D4h           { CRT Control Register  }
        mov   al,17             { end vertical retrace  }
        out   dx,al
        inc   dx
        in    al,dx
        or    al,128            { read only from 0-7    }
        out   dx,al
        dec   dx

        mov   al,23             { CRT Control Register  }
        out   dx,al
        inc   dx
        in    al,dx
        or    al,128            { resume reset          }
        out   dx,al

        mov   dx,3C4h           { EGA Sequencer         }
        mov   al,0              { reset register        }
        out   dx,al
        mov   al,3              { sync/async reset      }
        inc   dx
        out   dx,al
end;

procedure _VGA480ScanLines; near; assembler;
asm
        MOV     DX,03CCh                { Set Sync-Polarity     }
        IN      AL,DX
        OR      AL,$C0
        MOV     DX,03C2h
        OUT     DX,AL
        MOV     AL,6                    { Vertical Total        }
        MOV     DX,03D4h
        OUT     DX,AL
        MOV     AL,11                   { CRT Overflow          }
        MOV     DX,03D5h
        OUT     DX,AL
        MOV     AL,7
        DEC     DX
        OUT     DX,AL
        MOV     AL,62                   { Maximum Scan Line     }
        INC     DX
        OUT     DX,AL
        MOV     AL,9
        DEC     DX
        OUT     DX,AL
        MOV     AL,79                   { Start Vert. Retrace   }
        INC     DX
        OUT     DX,AL
        MOV     AL,16
        DEC     DX
        OUT     DX,AL
        MOV     AL,234                  { End Vertical Retrace  }
        INC     DX
        OUT     DX,AL
        MOV     AL,17
        DEC     DX
        OUT     DX,AL
        MOV     AL,140                  { Vert. Disp Enable End }
        INC     DX
        OUT     DX,AL
        MOV     AL,18
        DEC     DX
        OUT     DX,AL
        MOV     AL,223                  { Start Vert. Blanking  }
        INC     DX
        OUT     DX,AL
        MOV     AL,21
        DEC     DX
        OUT     DX,AL
        MOV     AL,231                  { End Vertical Blanking }
        INC     DX
        OUT     DX,AL
        MOV     AL,22
        DEC     DX
        OUT     DX,AL
        MOV     AL,4
        INC     DX
        OUT     DX,AL
end;

procedure _VGA94Cols; near; assembler;
asm
        mov   dx,3CCh
        in    al,dx
        and   al,$F3
        or    al,$04
        mov   dx,3C2h
        out   dx,al

        mov   dx,3C4h
        mov   al,$01
        out   dx,al
        inc   dx
        in    al,dx
        or    al,$01
        out   dx,al

        mov   dx,3D4h
        mov   al,$00
        out   dx,al
        inc   dx
        mov   al,$6C
        out   dx,al
        dec   dx

        mov   al,$01
        out   dx,al
        inc   dx
        mov   al,$5D
        out   dx,al
        dec   dx

        mov   al,$02
        out   dx,al
        inc   dx
        mov   al,$5E
        out   dx,al
        dec   dx

        mov   al,$03
        out   dx,al
        inc   dx
        mov   al,$8F
        out   dx,al
        dec   dx

        mov   al,$04
        out   dx,al
        inc   dx
        mov   al,$62
        out   dx,al
        dec   dx

        mov   al,$05
        out   dx,al
        inc   dx
        mov   al,$8E
        out   dx,al
        dec   dx

        mov   al,$13
        out   dx,al
        inc   dx
        mov   al,$2F
        out   dx,al

        mov   dx,3DAh
        in    al,dx

        mov   dx,3C0h
        mov   al,$13
        out   dx,al
        mov   al,$00
        out   dx,al
        mov   al,$20
        out   dx,al
        mov   al,$20
        out   dx,al
end;

{ CL - Char height }

procedure _VGACharHeight; near; assembler;
asm
        mov   dx,3D4h
        mov   al,$09
        out   dx,al
        inc   dx
        in    al,dx
        and   al,$E0
        dec   cl
        or    al,cl
        out   dx,al
        dec   dx

        mov   al,$0A
        out   dx,al
        inc   dx
        dec   cl
        cmp   cl,10
        jbe   @@1
        dec   cl
      @@1:
        mov   al,cl
        out   dx,al
        dec   dx

        mov   al,$0B
        out   dx,al
        inc   dx
        inc   cl
        mov   al,cl
        out   dx,al
end;

const
  vga200ScanLines = $00;
  vga350ScanLines = $01;
  vga400ScanLines = $02;
  vga480ScanLines = $03;        { DN specific }

  vgaFont16x8     = $0C;
  vgaFont14x8     = $00;
  vgaFont8x8      = $04;

  vga40Cols       = $00;
  vga80Cols       = $20;
  vga94Cols       = $30;        { DN specific }

procedure SetVgaMode ( Flags : Byte; Size : Word ); near;
assembler;
asm
        MOV     AH,$12
        MOV     AL,Flags
        AND     AL,vga480ScanLines
        CMP     AL,vga480ScanLines
        JB      @@1
        DEC     AL              { 480 temporary set to 400 }
@@1:
        MOV     BL,$30
        CALL    VideoInt
        MOV     AH,$00
        MOV     AL,Flags
        AND     AL,vga94Cols
        SHR     AL,4
        CMP     AL,vga94Cols SHR 4
        JE      @@2                { 94 temporary set to 80 }
        INC     AL
@@2:
        CALL    VideoInt
        MOV     AH,$11
        MOV     AL,Flags
        AND     AL,vgaFont16x8
        SHR     AL,2
        ADD     AL,AH
        MOV     BL,$00
        CALL    VideoInt
        CLI
        CALL    _VGAoff
        MOV     AL,Flags
        AND     AL,vga94Cols
        CMP     AL,vga94Cols
        JNE     @@3
        CALL    _VGA94Cols
@@3:
        MOV     AL,Flags
        AND     AL,vga480ScanLines
        CMP     AL,vga480ScanLines
        JNE     @@4
        CALL    _VGA480ScanLines
@@4:
        MOV     AL,Flags
        MOV     CL,14
        AND     AL,vgaFont16x8
        JZ      @@5
        MOV     CL,16
        CMP     AL,vgaFont16x8
        JE      @@5
        MOV     CL,8
@@5:
        PUSH    CX
        CALL    _VGACharHeight
        CALL    _VGAon
        POP     CX
        STI
        MOV     ES,Seg0040              { set BIOS values       }
        MOV     ES:[CrtCharOfs],CL
        MOV     AX,Size
        MOV     ES:[CrtColsOfs],AH
        DEC     AL
        MOV     ES:[CrtRowsOfs],AL      { how many rows minus 1 }
        INC     AL
        MUL     AH
        SHL     AX,1
        MOV     ES:[CrtPSizOfs],AX
end;

const
  vm40x12 = vga200ScanLines or vgaFont16x8 or vga40Cols;
  vm80x12 = vga200ScanLines or vgaFont16x8 or vga80Cols;
  vm94x12 = vga200ScanLines or vgaFont16x8 or vga94Cols;
  vm40x14 = vga200ScanLines or vgaFont14x8 or vga40Cols;
  vm80x14 = vga200ScanLines or vgaFont14x8 or vga80Cols;
  vm94x14 = vga200ScanLines or vgaFont14x8 or vga94Cols;
  vm40x25 = vga400ScanLines or vgaFont16x8 or vga40Cols;
  vm80x25 = vga400ScanLines or vgaFont16x8 or vga80Cols;
  vm94x25 = vga400ScanLines or vgaFont16x8 or vga94Cols;
  vm40x30 = vga480ScanLines or vgaFont16x8 or vga40Cols;
  vm80x30 = vga480ScanLines or vgaFont16x8 or vga80Cols;
  vm94x30 = vga480ScanLines or vgaFont16x8 or vga94Cols;
  vm40x34 = vga480ScanLines or vgaFont14x8 or vga40Cols;
  vm80x34 = vga480ScanLines or vgaFont14x8 or vga80Cols;
  vm94x34 = vga480ScanLines or vgaFont14x8 or vga94Cols;
  vm40x43 = vga350ScanLines or vgaFont8x8  or vga40Cols;
  vm80x43 = vga350ScanLines or vgaFont8x8  or vga80Cols;
  vm94x43 = vga350ScanLines or vgaFont8x8  or vga94Cols;
  vm40x50 = vga400ScanLines or vgaFont8x8  or vga40Cols;
  vm80x50 = vga400ScanLines or vgaFont8x8  or vga80Cols;
  vm94x50 = vga400ScanLines or vgaFont8x8  or vga94Cols;
  vm40x60 = vga480ScanLines or vgaFont8x8  or vga40Cols;
  vm80x60 = vga480ScanLines or vgaFont8x8  or vga80Cols;
  vm94x60 = vga480ScanLines or vgaFont8x8  or vga94Cols;

procedure SetDNMode ( Mode : Byte );
begin
  if (VideoType < vtVGA) then begin
    case Mode of
      sm40x25: asm
        MOV     AX,$0001
        CALL    VideoInt
        MOV     AX,$1111        { reload font }
        MOV     BL,$00
        CALL    VideoInt
      end;

      sm80x25: asm
        MOV     AX,$0003
        CALL    VideoInt
        MOV     AX,$1111        { reload font }
        MOV     BL,$00
        CALL    VideoInt
      end;

      sm80x43: if VideoType = vtEGA then asm
        MOV     AX,$0003
        CALL    VideoInt
        MOV     AX,$1112        { reload font 8x8 }
        MOV     BL,$00
        CALL    VideoInt
      end;

    end;
  end else begin { VideoType >= vtVGA }
    case Mode of
      sm40x12: SetVgaMode ( vm40x12,  40*256+12 );
      sm40x14: SetVgaMode ( vm40x14,  40*256+14 );
      sm40x25: SetVgaMode ( vm40x25,  40*256+25 );
      sm40x30: SetVgaMode ( vm40x30,  40*256+30 );
      sm40x34: SetVgaMode ( vm40x34,  40*256+34 );
      sm40x43: SetVgaMode ( vm40x43,  40*256+43 );
      sm40x50: SetVgaMode ( vm40x50,  40*256+50 );
      sm40x60: SetVgaMode ( vm40x60,  40*256+60 );
      sm80x12: SetVgaMode ( vm80x12,  80*256+12 );
      sm80x14: SetVgaMode ( vm80x14,  80*256+14 );
      sm80x25: SetVgaMode ( vm80x25,  80*256+25 );
      sm80x30: SetVgaMode ( vm80x30,  80*256+30 );
      sm80x34: SetVgaMode ( vm80x34,  80*256+34 );
      sm80x43: SetVgaMode ( vm80x43,  80*256+43 );
      sm80x50: SetVgaMode ( vm80x50,  80*256+50 );
      sm80x60: SetVgaMode ( vm80x60,  80*256+60 );
      sm94x12: SetVgaMode ( vm94x12,  94*256+12 );
      sm94x14: SetVgaMode ( vm94x14,  94*256+14 );
      sm94x25: SetVgaMode ( vm94x25,  94*256+25 );
      sm94x30: SetVgaMode ( vm94x30,  94*256+30 );
      sm94x34: SetVgaMode ( vm94x34,  94*256+34 );
      sm94x43: SetVgaMode ( vm94x43,  94*256+43 );
      sm94x50: SetVgaMode ( vm94x50,  94*256+50 );
      sm94x60: SetVgaMode ( vm94x60,  94*256+60 );
    end;
  end;
end;

{ Set CRT data areas and mouse range }

procedure SetCrtData; near; assembler;
asm
        CALL    GetCrtMode
        MOV     CL,0
        CMP     DL,24
        JBE     @@1
        MOV     CL,1
@@1:
        MOV     ScreenMode,AX
        MOV     byte ptr ScreenWidth,DH
        MOV     byte ptr ScreenWidth + 1, 0
        MOV     byte ptr ScreenHeight,DL
        MOV     byte ptr ScreenHeight + 1, 0
        MOV     HiResScreen,CL
        XOR     CL,1
        MOV     BX,SegB800
        CMP     AL,smMono
        JNE     @@2
        MOV     CL,0
        MOV     BX,SegB000
@@2:    MOV     CheckSnow,CL
        XOR     AX,AX
        MOV     word ptr ScreenBuffer[0],AX
        MOV     word ptr ScreenBuffer[2],BX

        MOV     AH,3
        MOV     BH,0
        CALL    VideoInt
        {MOV     CX, $0607;}
        MOV     CursorLines,CX
        MOV     AH,1
        MOV     CX,2000H
        CALL    VideoInt
        CMP     ButtonCount,0
        JE      @@4
        MOV     AX,7
        MOV     DL,byte ptr ScreenWidth
        CALL    @@3
        MOV     AX,8
        MOV     DL,byte ptr ScreenHeight
@@3:    XOR     DH,DH
        MOV     CL,3
        SHL     DX,CL
        DEC     DX
        XOR     CX,CX
        INT     33H
@@4:
end;

procedure SetVideoMode ( Mode : Word );
assembler;
asm
        MOV     AX,Mode
        CMP     AX,$007F
        JBE     @@2
        PUSH    AX
        CALL    FixCrtMode
        CMP     AX,$007F
        JBE     @@2
        PUSH    AX
        CMP     AX,$0100
        JAE     @@1
        CALL    SetDNMode
        JMP     @@3
@@1:
        CALL    SetVesaMode
        JMP     @@3
@@2:
        CALL    VideoInt
@@3:
        CALL    SetCrtData
end;

procedure InitVideo;
begin
  asm
        CMP     ScreenSaved, 0
        JNZ     @@2
        MOV     AH, 3
        XOR     BX,BX
        INT     10H
        MOV     OldCursorShape, CX
        MOV     OldCursorPos, DX
  @@2:
        CALL    GetCrtMode
        MOV     StartupMode,AX
        PUSH    AX
        CALL    FixCrtMode
        CMP     StartupMode,AX
        JE      @@1
        MOV     StartupMode,AX
        PUSH    AX
        CALL    SetVideoMode
  @@1:
        CALL    SetCrtData
  end;
  if WordRec(CursorLines).Hi > $10 then
    CursorLines := $607;
  if not ScreenSaved then begin
     if UserScreen <> nil then
       FreeMem ( UserScreen, UserScreenSize );
     UserScreenSize  := ScreenWidth * ScreenHeight * 2;
     UserScreenWidth := ScreenWidth;
     GetMem ( UserScreen, UserScreenSize );
     Move ( ScreenBuffer^, UserScreen^, UserScreenSize );
     ScreenSaved := True;
  end;
  SetBlink ( CurrentBlink );
end;

function VGASystem : Boolean;
begin
  VGASystem := VideoType >= vtVGA;      { PZ 2000.06.14 }
end;

{ PZ - end of rewritten chunk 2000.06.14 }

procedure DoneVideo;
var
  I, J, H: Integer;
  Shift: Integer;
  procedure FillEStr( X, Y: Integer ); assembler;
  asm
    mov  cx,ScreenWidth
    sub  cx,X
    les  di,ScreenBuffer
    mov  ax,ScreenWidth
    shl  ax,1
    mul  Y
    add  ax,X
    add  ax,X
    add  di,ax
    mov  ax,0720h
    cld
    rep  stosw
  end;
begin
 If (FadeDelay > 0) and TottalExit then FadePalette; {Knave}
  if (ScreenSaved) and (UserScreen <> nil) then begin
    J := UserScreenWidth; if J > ScreenWidth then J := ScreenWidth;
    if WordRec(OldCursorPos).Hi > ScreenHeight - 1 then WordRec(OldCursorPos).Hi := ScreenHeight - 1;
    if WordRec(OldCursorPos).Lo > ScreenWidth - 1 then WordRec(OldCursorPos).Lo := ScreenWidth - 1;
{    Shift := Byte(( SkyEnabled = 0 ) and ( InterfaceData.Options and ouiHideStatus = 0 ));}
    Shift := 0; {////////////////////}
    H := UserScreenSize div (UserScreenWidth * 2) - 1 - Shift;
    For I := 0 to H do begin
      Move(PWordArray(UserScreen)^[(I+Shift)*UserScreenWidth], PWordArray(ScreenBuffer)^[I*ScreenWidth], J*2);
      If J < ScreenWidth then FillEStr( J, I );
    end;
    For I := H + 1 to ScreenHeight - 1 do FillEStr( 0, I );
    If Shift > 0 then begin
      FillEStr( 0, H + 1 );
      Dec( WordRec( OldCursorPos ).Hi );
    end;
    if OldCursorShape <> $FFFF then begin
      if WordRec(OldCursorShape).Hi > $10 then OldCursorShape := $607;
      WordRec(PWordArray(ScreenBuffer)^[Lo(OldCursorPos)+Hi(OldCursorPos)*ScreenWidth]).Hi := 7;
    end;
  end else
    If ClsAct then ClearScreen;
  asm
    MOV  AH,1
    MOV  CX,CursorLines
    CALL VideoInt
  end;
  asm
    MOV     CX, OldCursorShape
    MOV     AH, 1
    XOR     BX, BX
    INT     10H
    MOV     DX, OldCursorPos
    MOV     AH, 2
    XOR     BX, BX
    INT     10H
(*    TEST    StartupData.Unload,osuBlinking {///////////}
    JNZ     @@1*)
  end;
 If (FadeDelay > 0) then ResetVgaPalette(ON); {Knave}
end;

procedure ClearScreen; assembler;
asm
        MOV     AX,600H
        MOV     BH,07H
        XOR     CX,CX
        MOV     DL,byte ptr ScreenWidth
        DEC     DL
        MOV     DH,byte ptr ScreenHeight
        DEC     DH
        CALL    VideoInt
        MOV     AH,2
        MOV     BH,0
        XOR     DX,DX
        CALL    VideoInt
end;

 Procedure GetPalette;
 var PAL : VGA_pal absolute Buf ;
     I{,y} : byte ;
  begin
  PAL := VGA_default ;
  if not VGAsystem then Exit ;
   for I:=0 to 15 do
     Get_palette( sl[I], pal[1,i], pal[2,i], pal[3,i]);
  end;

 Procedure SetPalette;
 var PAL : VGA_pal absolute Buf ;
     I{,y} : byte ;
  begin
  if not VGAsystem then Exit ;
   for i:=0 to 15 do
     Set_palette( sl[i], pal[1,i], pal[2,i], pal[3,i]);
  end;

 Procedure ResetVGApalette ;
  begin
    if Update then VGA_palette := VGA_default ;
    SetPalette( VGA_default );
  end;

{Knave begin}
procedure Set_palette(color,r,g,b: Byte); Assembler;
 asm
 mov dx,3C8h
  mov al,color
  out dx,al
  inc dx
  mov al,r
  out dx,al
  mov al,g
  out dx,al
  mov al,b
  out dx,al
 end;

procedure Get_palette(color:byte; var r,g,b:byte); Assembler;
 asm
  mov dx,3C7h
  mov al,color
  out dx,al
  add dx,2
  in al,dx
  les di,r
  stosb
  in al,dx
  les di,g
  stosb
  in al,dx
  les di,b
  stosb
 end;

Procedure FadePalette;
var i,j,r,g,b:byte;
begin
  for i:=0 to 63 do begin
    Delay(FadeDelay);
    for j:=0 to 15 do begin
      Get_palette(SL[j],r,g,b);
      if r>0 then dec(r);
      if g>0 then dec(g);
      if b>0 then dec(b);
      Set_palette(SL[j],r,g,b);
    end;
  end;
end;

Procedure BlackPalette;
var j:byte;
begin
for j:=0 to 15 do Set_palette(SL[j],0,0,0);
end;

Procedure GlowPalette;
var i,j,r,g,b:byte;
begin
 for i:=0 to 63 do begin
    Delay(FadeDelay);
    for j:=0 to 15 do begin
      Get_palette(SL[j],r,g,b);
      if r<VGA_Palette[1,j] then inc(r);
      if g<VGA_Palette[2,j] then inc(g);
      if b<VGA_Palette[3,j] then inc(b);
      Set_palette(SL[j],r,g,b);
    end;
  end;
end;
{Knave end}

Procedure SetBlink(Mode : boolean);
{$IFDEF NOASM}
var r: registers;
begin
 r.ax:=$1003;
 r.bl:=ord(Mode);
 intr($10, r);
end;
{$ELSE}
assembler;
asm
      PUSH BP
      PUSH DS
      MOV  AX,1003H
      MOV  BL,Mode
      INT  10h
      POP  DS
      POP  BP
end;
{$ENDIF}

{$ELSE VIRTUALPASCAL}
Uses {$IFDEF WIN32}Windows,{$ENDIF} Dos, VpSysLow, Drivers, Objects, DnApp, DnIni, Startup,
     Commands, VPUtils {$IFDEF OS2}, os2base, messages{$ENDIF};

var
  StrtCurY1: Integer;
  StrtCurY2: Integer;
  StrtCurVisible: Boolean;

{ ******** SCREEN MANAGER ******** }

procedure DetectVideoType;  {JO}
{$IFDEF OS2}
 var VideoConfig: VioConfigInfo;
begin
 VideoConfig.cb := SizeOf(VideoConfig); {AK155}
 if VioGetConfig(0, VideoConfig, 0) = 0 then
 begin
     case VideoConfig.Adapter of
  0: VideoType := vtMONO;
  1: VideoType := vtCGA;
  2: VideoType := vtEGA;
  3: VideoType := vtVGA;
4..8: VideoType := vtUnknown;
  9: VideoType := vtXGA;
     else VideoType := vtSVGA; {???}
     end;
 end
   else VideoType := vtUnknown;
end;                      {/JO}
{$ELSE}
begin
  VideoType := vtUnknown;
end;
{$ENDIF}

{  vtUnknown, vtMONO, vtCGA, vtEGA, vtVGA, vtXGA, vtSVGA, vtVBE2 }

// Fixes the CRT mode if required

function FixCrtMode(Mode: Word): Word;
begin
  case Lo(Mode) of
    smMono,smCO80,smBW80:
      FixCrtMode := Mode;
    smNonStandard:
      if NonStandardModes then
        FixCrtMode := Mode
      else
        FixCrtMode := smCO80;
    else FixCrtMode := smCO80;
  end;
end;

// Updates the CRT-related variables

procedure SetCrtData;
var
  BufSize: SmallWord;
  Y1,Y2: Integer;
  Visible: Boolean;
  SrcSize: TSysPoint;
begin
  DetectVideoType;
  ScreenMode := SysTVGetScrMode(@SrcSize);
  ScreenHeight := SrcSize.Y;
  ScreenWidth := SrcSize.X;
  ShowMouse;
  HiResScreen := True;
  ScreenBuffer := SysTVGetSrcBuf;
  SysTVGetCurType(Y1, Y2, Visible);
  WordRec(CursorLines).Hi := Y1;
  WordRec(CursorLines).Lo := Y2;
  SysTVSetCurType(Y1, Y2, False);   // Hide cursor
end;

// Detects video modes

procedure DetectVideo;
begin
  ScreenMode := FixCrtMode(SysTVGetScrMode(nil));
end;

// Sets the video mode. Mode is one of the constants smCO80, smBW80, or smMono,
// optionally with smFont8x8 added to select 43- or 50-line mode on an EGA or
// VGA. SetVideoMode initializes the same variables as InitVideo (except for
// the StartupMode variable, which isn't affected).

procedure SetVideoMode(Mode: Word);
var Cols, Rows: Word;
begin
 {GetVideoModeInfo(Cols1,Rows1,Rows);} {вводить отдельную переменную для цветов впадлу}
 Cols := 80;
 Rows := 0;
 case Mode of
  sm80x25: Rows := 25;
  sm80x30: Rows := 30;
  sm80x34: Rows := 34;
  sm80x43: Rows := 43;
  sm80x50: Rows := 50;
  sm80x60: Rows := 60;
{$IFNDEF Win32}
 $140A..$FFFE: begin {минимальный размер окна 20x10, меньше просто нет смысла}
                 Rows := Lo(Mode); Cols := Hi(Mode);
                 if Rows < 10 then Rows := 10;
                 if not PMWindowed then
                   begin
                     if not ((Cols in [40,80]) and (Rows in [25,30,34,43,50,60])) then Rows := 0;
                   end;
               end;
{$ENDIF}
 end;
 if Rows <> 0 then
   begin
     VPUtils.SetVideoMode(Cols,Rows);
     ScreenHeight := Rows;
     ScreenWidth := Cols;
     ScreenMode := Mode;
   end;
end;

// Initializes Turbo Vision's video manager. Saves the current screen
// mode in StartupMode, and switches the screen to the mode indicated by
// ScreenMode. The ScreenWidth, ScreenHeight, HiResScreen, ScreenBuffer,
// and CursorLines variables are updated accordingly.InitVideo is called
// automatically by TApplication.Init.

procedure InitVideo;
{$IFDEF WIN32}var s, c: TCOORD; r: TSMALLRECT;
                  Bf: Pointer;

  procedure Rebuf(Bf: pointer); assembler; {$USES ESI, EDI, ECX}
  asm
   mov esi,Bf
   mov edi,UserScreen
   mov ecx,UserScreenSize
@@1:
   lodsw
   stosb
   loop @@1
  end;
{$ENDIF}
 begin
  SysTVGetCurType(StrtCurY1, StrtCurY2, StrtCurVisible);
  if StartupMode <> ScreenMode then
    SetVideoMode({ScreenMode}StartupMode);
  SetCrtData;
  if not ScreenSaved then begin
     if UserScreen <> nil then
       FreeMem ( UserScreen, UserScreenSize );
     UserScreenSize  := ScreenWidth * ScreenHeight * 2;
     UserScreenWidth := ScreenWidth;
     GetMem ( UserScreen, UserScreenSize );
     {$IFDEF WIN32}        {?}{DataCompBoy: how to do this in OS/2 ???}
     GetMem ( Bf, UserScreenSize shl 1 );
     s.x:=ScreenWidth;
     s.y:=ScreenHeight;
     c.x:=0;
     c.y:=0;
     r.left:=0; r.right:=screenwidth; r.top:=0; r.bottom:=screenheight;
     ReadConsoleOutput(SysFileStdOut, Bf, s, c, r);
     Rebuf(Bf);
     FreeMem(Bf, UserScreenSize shl 1);
     {$ENDIF}
     {$IFDEF OS2}
     Move ( ScreenBuffer^, UserScreen^, UserScreenSize );  {JO}
     {$ENDIF}
     ScreenSaved := True;
  end;
  SetBlink(CurrentBlink);
end;

// Terminates Turbo Vision's video manager by restoring the initial
// screen mode, clearing the screen, and restoring the cursor. Called
// automatically by TApplication.Done.

procedure DoneVideo;
begin
  if (StartupMode <> $FFFF) and (StartupMode <> ScreenMode) then
    SetVideoMode(StartupMode);
  FillChar(ScreenBuffer^, ScreenWidth * ScreenHeight * 2, 0); {JO: нужно, чтобы куски панелей не "линяли" в UserScreen}
  Move(UserScreen^, ScreenBuffer^, UserScreenSize);
  SysTVShowBuf(0, UserScreenSize);
  SysTVSetCurType(StrtCurY1, StrtCurY2, StrtCurVisible);
  FillChar(ScreenMirror, SizeOf(ScreenMirror), 0);
end;

// Clears the screen, moves cursor to the top left corner

procedure ClearScreen;
begin
  SysTVClrScr;
end;

{$IFDEF OS2} {JO} {установка VGA-палитры в окне}

 Procedure ResetVGApalette( Update : Boolean );
  begin
    if Update then VGA_palette := VGA_default ;
    SetPalette( VGA_default );
  end;

function VGASystem : Boolean;
begin
  VGASystem := VideoType >= vtVGA;      { PZ 2000.06.14 }
end;

type RGB=record
       red,green,blue:byte
     end;
     VGAPalette=array[byte] of RGB;

procedure SetGetVGAPal(var p:VGAPalette;Get:boolean);
const
ColorReg : VioColorReg = (
    cb:  sizeof(VioColorReg);   // Size of this structure
    rType:    3;                // 3 = Color registers
    FirstColorReg: 0;           // Specifies the first color registers
    NumColorRegs:  256;         // Number of color registers
    ColorRegAddr: nil           // Pointer to array with color values
);
begin
    with ColorReg do
    begin
      ColorRegAddr := @p;
      FLatToSel(ColorRegAddr);
    end;
    if Get Then VioGetState(ColorReg,0) else VioSetState(ColorReg,0)
end;

Procedure Set_palette(color,r,g,b :Byte);
 var curpal:VGAPalette;
begin
 SetGetVGAPal(curpal, True);
 with curpal[color] do
   begin
     red   := r;
     green := g;
     blue  := b;
   end;
 SetGetVGAPal(curpal, False);
end;

Procedure Get_palette(color :Byte; Var r,g,b :Byte);
 var curpal:VGAPalette;
begin
 SetGetVGAPal(curpal, True);
 with curpal[color] do
   begin
     r := red;
     g := green;
     b := blue;
   end;
end;

 Procedure GetPalette(var Buf);
 var PAL : VGA_pal absolute Buf ;
     curpal:VGAPalette;
     I: byte ;
 begin
  if not (VGAsystem and not PMWindowed) then Exit ;
  PAL := VGA_default ;
  SetGetVGAPal(curpal, True);
  for I:=0 to 15 do
    with curpal[sl[I]] do
      begin
        pal[1,I] := red;
        pal[2,I] := green;
        pal[3,I] := blue;
      end;
 end;

 Procedure SetPalette(var Buf);
 var PAL : VGA_pal absolute Buf ;
     curpal:VGAPalette;
     I: byte ;
 begin
  if not (VGAsystem and not PMWindowed) then Exit ;
  SetGetVGAPal(curpal, True);
  for i:=0 to 15 do
    with curpal[sl[I]] do
      begin
        red   := pal[1,I];
        green := pal[2,I];
        blue  := pal[3,I];
      end;
  SetGetVGAPal(curpal, False);
 end;

Procedure SetBlink(Mode : boolean);  {JO}
var
  I: VioIntensity;
begin
  with I do
    begin
      cb := sizeof(VioIntensity);
      rType := 2;
      if Mode then fs := 0 else fs := 1;
    end;
  VioSetState(I, TVVioHandle);
end;

{$ELSE}
Procedure ResetVGApalette( Update : Boolean );       begin end;
Procedure GetPalette(var Buf);                       begin end;
Procedure SetPalette(var Buf);                       begin end;
Function  VGASystem: Boolean;                        begin end;
Procedure Set_palette(color,r,g,b :Byte);            begin end;
Procedure Get_palette(color :Byte; Var r,g,b :Byte); begin end;
Procedure SetBlink(Mode : boolean);                  begin end;
{$ENDIF}

{Procedure GetCrtMode;                                begin end;}

{$ENDIF VIRTUALPASCAL}

 procedure SetScrMode(Mode: Word);
   var R, R1, A: TRect;
 begin with PApplication(Application)^ do begin
   if Mode = ScreenMode then
     Exit;
{$IFDEF OS2}
  if (Mode > $140A) and (Mode < $FFFE) and (not PMWindowed)
       and not ((Hi(Mode) in [40,80]) and (Lo(Mode) in [25,30,34,43,50,60])) then
        begin
          messagebox(GetString(dlNotValidForFullscreen), nil, mfError+mfOKButton);
          Exit;
        end;
{$ENDIF}
  GetExtent(R1);
  Clock^.GetBounds(A);
  SetScreenMode(Mode);
  GetExtent(R);
  A.A.X := Round(A.A.X*R.B.X/R1.B.X);
  if A.B.Y = R1.B.Y then A.A.Y := R.B.Y-1
     else A.A.Y := Round(A.A.Y*R.B.Y/R1.B.Y);
  if ShowSeconds then A.B.X := A.A.X + 10 else A.B.X := A.A.X + 7;
  A.B.Y := A.A.Y + 1;
  Clock^.Locate(A);
  SetBlink(CurrentBlink);
  if (StartupData.Load and osuResetPalette <> 0) and VGASystem
   then SetPalette(vga_palette);
  ReDraw;
 end;
{$IFDEF OS2}
 if not PMWindowed then
{$ENDIF}
   NonVIOScreenMode := ScreenMode
{$IFDEF OS2}
   else VIOScreenMode := ScreenMode
{$ENDIF};
 SaveDnIniSettings(nil);
 DoneIniEngine;
 end;

END.
