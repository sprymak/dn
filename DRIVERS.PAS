{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.11
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

{$S-}
unit Drivers;

{-----------------------------------------------------}
{ This module is based on Turbo Vision Drivers Unit   }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

interface

uses LFN, Objects, xTime, dnini, advance6, Commands
{$IFNDEF NONBP}, StakDump{$ENDIF}
     ;

Procedure DriversInitUnit;

var
  LSliceTimer: TEventTimer;
  LSliceCnt: LongInt;


procedure SliceAwake;

{ ******** EVENT MANAGER ******** }

const
  AltCodes1: array[$10..$34] of Char =
    'QWERTYUIOP'#0#0#0#0'ASDFGHJKL'#0#0#0#0#0'ZXCVBNM'#0#0;

{ Mouse button state masks }

  mbLeftButton  = $01;
  mbRightButton = $02;

  ShowScrollBar:Boolean=True;

  ShiftState:  Byte = 0;
  ShiftState2: Byte = 0;

type

{ Event record }

  PEvent = ^TEvent;
  TEvent = record
    What: Word;
    case Word of
      evNothing: ();
      evMouse: (
        Buttons: Byte;
        Double: Boolean;
        Where: TPoint);
      evKeyDown: (
        case Integer of
          0: (KeyCode: Word);
          1: (CharCode: Char;
              ScanCode: Byte));
      evMessage: (
        Command: Word;
        case Word of
          0: (InfoPtr: Pointer);
          1: (InfoLong: Longint);
          2: (InfoWord: Word);
          3: (InfoInt: Integer);
          4: (InfoByte: Byte);
          5: (InfoChar: Char));
  end;

  CharDef = Array [0..15] of Byte;


{ Initialized variables }
Const

  ButtonCount : Byte = 0;
  MouseEvents : Boolean = False;
  MouseReverse: Boolean = False;
  MouseButtons: Byte = 0;
  DoubleDelay : Word = 8;
  RepeatDelay : Word = 8;
  AutoRepeat  : Word = 1;
  UserScreen  : Pointer = nil;
{DataCompBoy
  GraphMouse  : Boolean = False;
  MouseMouse  : Boolean = True;
DataCompBoy}
  CharHeight  : Byte = 16;
  ScreenSaved : Boolean = False;
  NeedAbort   : Boolean = False;
  StdMouse    : Boolean = False; { Is engine really using standard mouse }
  XSens       : Byte = 11;
  YSens       : Byte = 11;
  CLSAct      : Boolean = True;
  CurrentBlink: Boolean = False;
  MouseWhere  : TPoint = (X: 0; Y:0);

var

{ Uninitialized variables }
  OldBlink: Boolean;

  MouseIntFlag: Byte;
  UserScreenSize: Word;
  UserScreenWidth: Word;
  MouseX, MouseY,
  OldMouseX, OldMouseY: Word;
  ShowMouseProc,
  HideMouseProc: Pointer;

{ Event manager routines }


procedure InitDrivers;
procedure InitEvents;
procedure DoneEvents;
procedure ShowMouse;
procedure HideMouse;
{$IFDEF VIRTUALPASCAL}
procedure UpdateMouseWhere;
{$ENDIF}
procedure GetMouseEvent(var Event: TEvent);
procedure GetKeyEvent(var Event: TEvent);
procedure SetMouseSpeed(XS, YS: Byte);


const

{ Initialized variables }

  StartupMode: Word = $FFFF;
  SkyEnabled: Integer = 0;
  TottalExit: Boolean = False; { Set it only when leaving your program }
  Exiting: Boolean = False;    { Set it when temporary leaving your program }

  MouseVisible   : Boolean = True;

var

{ Uninitialized variables }

  ScreenMode: Word;
  ScreenWidth: Word;
  ScreenHeight: Word;
  HiResScreen: Boolean;
  CheckSnow: Boolean;
  ScreenBuffer: Pointer;
  CursorLines: {$IFDEF VIRTUALPASCAL}SmallWord;{$ELSE}Word;{$ENDIF}
  OldCursorShape: Word;
  OldCursorPos: Word;

{ ******** SYSTEM ERROR HANDLER ******** }

type

{ System error handler function type }

  TSysErrorFunc = function(ErrorCode: Integer; Drive: Byte): Integer;

{ Default system error handler routine }

function SystemError(ErrorCode: Integer; Drive: Byte): Integer;

const

{ Initialized variables }

  SysErrorFunc: TSysErrorFunc = SystemError;
  SysColorAttr: Word = $4E4F;
  SysColorButtonAttr: Word = $0E0F;
  SysMonoAttr: Word = $7070;
  CtrlBreakHit: Boolean = False;
  SaveCtrlBreak: Boolean = False;
  SysErrActive: Boolean = False;

{ System error handler routines }

procedure InitSysError;
procedure DoneSysError;

{ ******** UTILITY ROUTINES ******** }

{ Keyboard support routines }

function GetAltChar(KeyCode: Word): Char;
function GetAltCode(Ch: Char): Word;
function GetCtrlChar(KeyCode: Word): Char;
function GetCtrlCode(Ch: Char): Word;
function CtrlToArrow(KeyCode: Word): Word;

{ String routines }

procedure FormatStr(var Result: String; const Format: String; var Params);
procedure PrintStr(const S: String);

{ Buffer move routines }

procedure MoveColor(var Buf; Num, Attr: Byte);
procedure MoveBuf(var Dest; var Source; Attr: Byte; Count: Word);
procedure MoveChar(var Dest; C: Char; Attr: Byte; Count: Word);
procedure MoveCStr(var Dest; const Str: String; Attrs: Word);
procedure MoveStr(var Dest; const Str: String; Attr: Byte);
function  CStrLen(const S: String): Integer;

implementation
uses
  Drivers2,
  VideoMan,
  Dos,
  {$IFDEF DPMI}
  Dpmi,
  {$ENDIF}
  advance,
  advance1,
  advance2,
  advance3,
  Views,
  Startup,
  DnApp,
  DnUtil,
  {$IFDEF VIRTUALPASCAL} VpSysLow, Crt, {$ENDIF}
  FlPanelX
  ;

const
      MCurMP    : CharDef=($00,$40,$60,$30,$18,$3C,$3E,$3E,$3E,$1F,$07,$06,$00,$00,$00,$00);
      MCurSP    : CharDef=($BF,$1F,$0F,$87,$C3,$80,$80,$80,$80,$C0,$E0,$F0,$F0,$FF,$FF,$FF);
      MCurLP    : CharDef=($00,$00,$38,$04,$36,$4E,$0E,$6E,$3E,$1F,$07,$06,$00,$00,$00,$00);
      MCurLSP   : CharDef=($FF,$C7,$83,$C1,$80,$00,$00,$00,$80,$C0,$E0,$F0,$F0,$FF,$FF,$FF);
      MCurM0    : CharDef=($00,$40,$60,$70,$78,$7C,$7E,$78,$4C,$0E,$06,$00,$00,$00,$00,$00);
      MCurS0    : CharDef=($1F,$0F,$07,$03,$01,$00,$00,$00,$01,$00,$F0,$F8,$F8,$FF,$FF,$FF);

      MCurM1    : CharDef=($00,$18,$3C,$3C,$19,$7E,$BC,$3D,$7E,$98,$08,$04,$04,$08,$00,$00);
      MCurS1    : CharDef=($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
      MCurM2    : CharDef=($00,$18,$3C,$3C,$98,$7E,$3D,$BC,$7E,$19,$10,$20,$20,$10,$00,$00);
      MCurS2    : CharDef=($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);

      StdMouseBlock  : Array [0..5] of Byte = ($D6,$D7,$D0,$D2,$B7,$B8);

      LastMouseMouse : Boolean = False;

{ ******** EVENT MANAGER ******** }

const

{ Event manager constants }

  EventQSize = 16;

var

{ Event manager variables }


  OldAttr: Byte;
  OldSymbols: Array [0..5] of Byte;
  SaveFont: Array [0..5] of CharDef;
  LastButtons: Byte;
  DownButtons: Byte;
  LastDouble: Boolean;
const
  LastWhere: TPoint = (X:0; Y:0);
  DownWhere: TPoint = (X:0; Y:0);
Var
  DownTicks: Word;
  AutoTicks: Word;
  AutoDelay: Word;
  EventCount: Word;
  EventQHead: Word;
  EventQTail: Word;
  EventQueue: array[0..EventQSize - 1] of TEvent;
  EventQLast: record end;

{const}
{  AltFlag : Boolean = False;}
var
  ErrAdd  : Longint ;
{ Old09   : Pointer;}
{ Vect09  : Pointer absolute $0000:$0024;}

{$IFDEF VIRTUALPASCAL}{$I DRIVERS._VP}
 {$ELSE}
  {$IFDEF FPC}{$I DRIVERS._FP}
   {$ELSE}
    {$I DRIVERS._BP}
  {$ENDIF}
{$ENDIF}

{ ******** SYSTEM ERROR HANDLER ******** }
{$IFNDEF VIRTUALPASCAL}
 {$IFDEF DPMI}
 {$L SYSINT.OBP}
 {$ELSE}
 {$L SYSINT.OBJ}
 {$ENDIF}
{$ENDIF VP}
const

{ System error messages }

 (*
  SCriticalError:    string[31] = 'Critical disk error on drive %c';
  SWriteProtected:   string[35] = 'Disk is write-protected in drive %c';
  SDiskNotReady:     string[29] = 'Disk is not ready in drive %c';
  SDataIntegrity:    string[32] = 'Data integrity error on drive %c';
  SSeekError:        string[22] = 'Seek error on drive %c';
  SUnknownMedia:     string[30] = 'Unknown media type in drive %c';
  SSectorNotFound:   string[28] = 'Sector not found on drive %c';
  SOutOfPaper:       string[20] = 'Printer out of paper';
  SWriteFault:       string[23] = 'Write fault on drive %c';
  SReadFault:        string[22] = 'Read fault on drive %c';
  SGeneralFailure:   string[28] = 'Hardware failure on drive %c';
  SBadImageOfFAT:    string[32] = 'Bad memory image of FAT detected';
  SDeviceError:      string[19] = 'Device access error';
  SInsertDisk:       string[27] = 'Insert diskette in drive %c';
  SSharingViolation: string[17] = 'Sharing violation';
  ButAbort:          string[11] = '[ ~A~bort ]';
  ButRetry:          string[11] = '[ ~R~etry ]';
  ButIgnore:         string[12] = '[ ~I~gnore ]';
*)

  NumButtons: array[0..16] of Byte =
  (2,2,2,2,3,2,3,2,3,2,3,3,2,2,2,2,2);
  ButtonX: array[2..3] of array [1..3] of Byte =
  ((8,23,0),(3,15,27));
  XErrTable: Array [1..3] of Byte = (3,1,0);

  (*
  ButtonOffset: array[1..3] of Word =
  (Ofs(ButAbort), Ofs(ButRetry), Ofs(ButIgnore));
  *)


{ Critical error message translation table }

  (*
  ErrorString: array[0..16] of Word = (
    Ofs(SWriteProtected),
    Ofs(SCriticalError),
    Ofs(SDiskNotReady),
    Ofs(SCriticalError),
    Ofs(SDataIntegrity),
    Ofs(SCriticalError),
    Ofs(SSeekError),
    Ofs(SUnknownMedia),
    Ofs(SSectorNotFound),
    Ofs(SOutOfPaper),
    Ofs(SWriteFault),
    Ofs(SReadFault),
    Ofs(SGeneralFailure),
    Ofs(SBadImageOfFAT),
    Ofs(SDeviceError),
    Ofs(SInsertDisk),
    Ofs(SSharingViolation)
    );
   *)

  CE_LastMsg = 16;
  CE_Idx: array[0..CE_LastMsg] of TStrIdx = (
    {00}dlCE_WriteProtected,   {13  Attempt to write on a write-protected diskette}
    {01}dlCE_CriticalError,    {14  Unknown unit}
    {02}dlCE_DiskNotReady,     {15  Drive not ready}
    {03}dlCE_CriticalError,    {16  Unknown command}
    {04}dlCE_DataIntegrity,    {17  CRC error}
    {05}dlCE_CriticalError,    {18  Bad request structure length}
    {06}dlCE_SeekError,        {19  Seek error}
    {07}dlCE_UnknownMedia,     {1A  Unknown media type}
    {08}dlCE_SectorNotFound,   {1B  Sector not found}
    {09}dlCE_OutOfPaper,       {1C  Printer out of paper}
    {10}dlCE_WriteFault,       {1D  Write fault}
    {11}dlCE_ReadFault,        {1E  Read fault}
    {12}dlCE_GeneralFailure,   {1F  General failure}
    {13}dlCE_BadImageOfFAT,    {?}
    {14}dlCE_DeviceError,      {?}
    {15}dlCE_InsertDisk,       {?}
    {16}dlCE_SharingViolation);{20  Sharing violation}


var
  CE_Buttons:  array[1..3] of PString;
  CE_Messages: array[0..CE_LastMsg] of PString;
  CE_K: array[1..3] of Byte;
  ErrWndTitle: PString;

{ System error handler routines }

{$IFNDEF VIRTUALPASCAL}
procedure InitSysError; external;
procedure DoneSysError; external;
{$ENDIF}

{$V-}
procedure SwapStatusLine(var Buffer; ShadowAttr: Byte);
{$IFNDEF BIT_32}
near; assembler;
asm     cld
        mov     cl,byte ptr ScreenWidth
        xor     ch,ch
        mov     al,byte ptr ScreenHeight
        sub     al,6
        shr     al,1
        mul     cl
        shl     ax,1
        les     di,ScreenBuffer
        add     di,ax
        mov     al,byte ptr ScreenWidth
        mov     dl,al
        mov     dh,0
        shl     dx,1
        sub     al,40
        shr     al,1
        cbw
        shl     ax,1
        add     di,ax
        push    ds
        lds     si,Buffer
        mov     bl,6
@@0:    mov     cx,40
        push    di
@@1:    mov     ax,es:[di]
        movsw
        mov     ds:[si-2],ax
        loop    @@1
        cmp     bl,6
        jne     @@2
        add     si,4
        add     di,4
        jmp     @@3
@@2:    mov     ax,es:[di]
        mov     ds:[si],ax
        mov     ah,ShadowAttr
        stosw
        add     si,2
        mov     ax,es:[di]
        mov     ds:[si],ax
        mov     ah,ShadowAttr
        stosw
        add     si,2
@@3:    pop     di
        add     di,dx
        dec     bl
        jne     @@0
        add     di,4
        mov     cx,40
@@4:    mov     ax,es:[di]
        mov     ds:[si],ax
        mov     ah,ShadowAttr
        stosw
        add     si,2
        loop    @@4
        pop     ds
end;
{$ELSE}
near; assembler;
asm     cld
        mov     ecx,ScreenWidth
        mov     eax,ScreenHeight
        sub     eax,6
        shr     eax,1
        mul     ecx
        shl     eax,1
        mov     edi,ScreenBuffer
        add     edi,eax
        mov     eax,ScreenWidth
        mov     edx,eax
        shl     edx,1
        sub     eax,40
        shr     eax,1
        cbw
        cwd
        shl     eax,1
        add     edi,eax
        mov     esi,Buffer
        mov     bl,6
@@0:    mov     ecx,40
        push    edi
@@1:    mov     ax,[edi]
        movsw
        mov     [esi-2],ax
        loop    @@1
        cmp     bl,6
        jne     @@2
        add     esi,4
        add     edi,4
        jmp     @@3
@@2:    mov     ax,[edi]
        mov     [esi],ax
        mov     ah,ShadowAttr
        stosw
        add     esi,2
        mov     ax,[edi]
        mov     [esi],ax
        mov     ah,ShadowAttr
        stosw
        add     esi,2
@@3:    pop     edi
        add     edi,edx
        dec     bl
        jne     @@0
        add     edi,4
        mov     ecx,40
@@4:    mov     ax,[di]
        mov     [esi],ax
        mov     ah,ShadowAttr
        stosw
        add     esi,2
        loop    @@4
end;
{$ENDIF}

function SystemError(ErrorCode: Integer; Drive: Byte): Integer;
{$IFNDEF NONBP}
var
  C,AC,WW,K : Word;
  P : Pointer;
  S : string[63];
  B,SW : PWordArray;
  I : Byte;
  Event: TEvent;
  PExit, ActiveButton: Byte;

function GetCoord(X, Y: LongInt): Word;
begin
 GetCoord := ((ScreenHeight-6) div 2+Y)*(ScreenWidth*2)+((ScreenWidth-40) div 2+X)*2;
end;

Procedure DrawButton(No : Byte; State : Boolean);
var X,I : Word;
    P   : PString;
begin
 I:=NumButtons[ErrorCode];
 X:=ButtonX[I,No]; X := GetCoord(X, 4);
 P := CE_Buttons[No];
 MoveCStr(PByteArray(ScreenBuffer)^[X], P^, Byte(State)*AC+Byte(not State)*C);
end;

function MPos(P: TPoint): LongInt;
  var X, I, N: LongInt;
begin
 MPos := -1; N := NumButtons[ErrorCode];
 X := P.X - ((ScreenWidth-40) div 2);
 if P.Y = (ScreenHeight-6) div 2+4 then
    for I := 1 to N do
      if (X >= ButtonX[N, I]) and (X< ButtonX[N, I]+CStrLen(CE_Buttons[I]^)) then
        begin MPos := I; Exit end;
end;

procedure ControlMousePos(P: TPoint);
  var I,J: LongInt;
begin
  J := MPos(P);
  if (J < 0) or (J = ActiveButton) then Exit;
  ActiveButton := J;
  HideMouse;
  for i:=1 to NumButtons[ErrorCode] do DrawButton(i,i=ActiveButton);
  ShowMouse;
end;

var LM: TPoint;

begin

 if NeedAbort then
  begin
   SystemError := 3;
   Abort := On;
   NeedAbort := Off;
   Exit;
  end;

if Abort then
 begin
   SystemError := 3;
   Exit ;
 end;

 asm    MOV     AH,3
        MOV     BH,0
        PUSH    BP
        PUSH    ES
        INT     $10
        POP     ES
        POP     BP
        PUSH    CX
        MOV     AH,1
        MOV     CX,2000H
        PUSH    BP
        PUSH    ES
        INT     $10
        POP     ES
        POP     BP

        MOV     AH, 59h
        xor     bx, bx
        INT     21h
        mov     WW, AX
 end;
 if (WW = 32) or (WW = 33) then ErrorCode := 16;
  if Lo(ScreenMode) = smMono then
    begin C := SysMonoAttr; AC := $0F07 end else
    begin C := SysColorAttr; AC := SysColorButtonAttr end;
  GetMem(B, 42*7*2); GetMem(SW, 42*7*2);
  MoveChar(B^, ' ', Byte(C), 42*6);
  MoveChar(B^, #205, Byte(C), 40);
  MoveChar(B^, #201, Byte(C), 1);
  MoveChar(B^[39], #187, Byte(C), 1);
  MoveChar(B^[42*5], #205, Byte(C), 40);
  MoveChar(B^[42*5], #200, Byte(C), 1);
  MoveChar(B^[42*5+39], #188, Byte(C), 1);
  for i:=1 to 4 do
      begin
       MoveChar(B^[42*i], #186, Byte(C), 1);
       MoveChar(B^[42*i+39], #186, Byte(C), 1);
      end;
  MoveStr(B^[(40-Length(ErrWndTitle^))div 2], ' '+ErrWndTitle^+' ', Byte(C));
  if (ErrorCode < 0) or (ErrorCode > CE_LastMsg) then ErrorCode := 12;
  S := CE_Messages[ErrorCode]^;
  I := Pos('%', S);
  if I > 0 then begin S[I] := Char(Drive+65); S[I+1] := ':' end;
  MoveStr(B^[42*2+1+(38-Length(S)) div 2], S, Byte(C));
  for I := 0 to 6 do
   Move(PByteArray(ScreenBuffer)^[GetCoord(0,I)], SW^[I*42], 84);
  HideMouse;
  SwapStatusLine(B^, ShadowAttr);
  for i:=1 to NumButtons[ErrorCode] do DrawButton(i,i=2);
  ActiveButton := 2;
  ShowMouse;
  repeat
   PExit := 0;
   GetMouseEvent(Event);
   case Event.What of
     evMouseMove: ControlMousePos(Event.Where);
     evMouseDown: LM := Event.Where;
     evMouseUp: if (MPos(LM) = ActiveButton) and (MPos(Event.Where) = ActiveButton)
                  then begin PExit := ActiveButton; Break; end;
       else GetKeyEvent(Event);
   end;
   if Event.What = evKeyDown then
    begin
     if Event.CharCode = #$e0 then Event.CharCode := #0;
     K := Event.KeyCode;
     case K of
      kbLeft : if ActiveButton > 1 then Dec(ActiveButton) else ActiveButton :=  NumButtons[ErrorCode];
      kbRight : if ActiveButton < NumButtons[ErrorCode] then Inc(ActiveButton) else ActiveButton := 1;
      kbSpace,
      kbEnter : PExit := ActiveButton;
      kbESC   : PExit := 1;
       else
       begin
         K := K shr 8;
         if K = CE_K[1] then PExit := 1 else
         if K = CE_K[2] then PExit := 2 else
         if K = CE_K[3] then if NumButtons[ErrorCode] > 2 then PExit := 3;
       end;
     end;
     HideMouse;
     for i:=1 to NumButtons[ErrorCode] do DrawButton(i,i=ActiveButton);
     ShowMouse;
    end;
  until (PExit  > 0);
  HideMouse;
  for I := 0 to 6 do
   Move(SW^[I*42], PByteArray(ScreenBuffer)^[GetCoord(0,I)], 84);
  ShowMouse;
  FreeMem(B, 42*7*2);
  FreeMem(SW, 42*7*2);
  asm   POP     CX
        PUSH    DX
        MOV     AH,1
        PUSH    BP
        PUSH    ES
        INT     $10
        POP     ES
        POP     BP
        POP     AX
  end;
 I := XErrTable[PExit];
 Abort := I = 3;
 SystemError := I;
end;
{$ELSE}
begin

  {?}

 Abort := True;
 SystemError := 3;
end;
{$ENDIF}

{$V+}

{ ******** UTILITY ROUTINES ******** }

{ Keyboard support routines }

const
  AltCodes2: array[$78..$83] of Char =
    '1234567890-=';

function GetAltChar(KeyCode: Word): Char;
begin
  GetAltChar := #0;
  if Lo(KeyCode) = 0 then
    case Hi(KeyCode) of
      $C2: GetAltChar := Char(SystemMenuChar);
      $10..$34: GetAltChar := AltCodes1[Hi(KeyCode)];
      $78..$83: GetAltChar := AltCodes2[Hi(KeyCode)];
    end;
end;

function GetAltCode(Ch: Char): Word;
var
  I: Word;
begin
  GetAltCode := 0;
  if Ch = #0 then Exit;
  Ch := UpCase(Ch);
  if Ch = Char(SystemMenuChar) then
  begin
    GetAltCode := $0200;
    Exit;
  end;
  for I := $10 to $34 do
    if AltCodes1[I] = Ch then
    begin
      GetAltCode := I shl 8;
      Exit;
    end;
  for I := $78 to $83 do
    if AltCodes2[I] = Ch then
    begin
      GetAltCode := I shl 8;
      Exit;
    end;
end;

function GetCtrlChar(KeyCode: Word): Char;
begin
  GetCtrlChar := #0;
  if (Lo(KeyCode) <> 0) and (Lo(KeyCode) <= Byte('Z') - Byte('A') + 1) then
    GetCtrlChar := Char(Lo(KeyCode) + Byte('A') - 1);
end;

function GetCtrlCode(Ch: Char): Word;
begin
  GetCtrlCode := GetAltCode(Ch) or (Byte(UpCase(Ch)) - Byte('A') + 1);
end;

function CtrlToArrow(KeyCode: Word): Word;
const
  NumCodes = 11;
  CtrlCodes: array[0..NumCodes-1] of Char = ^S^D^E^X^A^F^G^V^R^C^H;
  ArrowCodes: array[0..NumCodes-1] of Word =
    (kbLeft, kbRight, kbUp, kbDown, kbHome, kbEnd, kbDel, kbIns,
     kbPgUp, kbPgDn, kbBack);
var
  I: Integer;
begin
  CtrlToArrow := KeyCode; Exit;
  for I := 0 to NumCodes - 1 do
    if WordRec(KeyCode).Lo = Byte(CtrlCodes[I]) then
    begin
      CtrlToArrow := ArrowCodes[I];
      Exit;
    end;
end;

{!SEE Drivers._??!}

{ Drivers unit initialization and shutdown }

const
  DriversInit: Boolean = False;

procedure InitDrivers;
var I: Integer;
    S: string;
begin
  if DriversInit then Exit;
  DriversInit := True;

  S := GetString(dlAltTable);
  if S <> '' then Move(S[1], AltCodes1, Min(Length(S), 37));

  for I := 0 to CE_LastMsg do
    CE_Messages[I] := NewStr(GetString(CE_Idx[I]));

  for I := 1 to 3 do
  begin
    S := GetString(TStrIdx(Ord(dlCE_ButAbort)+I-1));
    CE_Buttons[I] := NewStr(S);
    CE_K[I] := GetAltCode(HotKey(S)) shr 8;
  end;

  ErrWndTitle := NewStr(GetString(dlCE_ErrWndTitle));
  InitSysError;
end;

procedure SliceAwake;
begin
  NewTimerSecs(LSliceTimer, 3);
  LSliceCnt := -2;
end;

Procedure DriversInitUnit;
{$IFNDEF VIRTUALPASCAL}
begin
  DetectMouse;
  CharHeight := Mem[Seg0040:CrtCharOfs]; { PZ 2000.06.14 }
  SaveExit   := ExitProc;
  ExitProc   := @ExitDrivers;
{$ELSE}
begin
  {?}{CodePage := SysGetCodePage;}
  SysTVInitCursor;
  InitKeyboard;
  DetectMouse;
  DetectVideo;
  SaveExit := ExitProc;
  ExitProc := @ExitDrivers;
  CtrlBreakHandler := TVCtrlBreak;
  SysCtrlSetCBreakHandler;
{$ENDIF}
end;

end.
