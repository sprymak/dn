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
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{$S-}
unit Drivers;

{-----------------------------------------------------}
{ This module is based on Turbo Vision Drivers Unit   }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

interface

uses
  Lfn, Defines, xTime, DnIni, advance6, Commands
  ;

var
  LSliceTimer: TEventTimer;
  LSliceCnt: LongInt;

procedure SliceAwake;

{ ******** EVENT MANAGER ******** }

const
  AltCodes1: array[$10..$34] of Char =
  'QWERTYUIOP'#0#0#0#0'ASDFGHJKL'#0#0#0#0#0'ZXCVBNM'#0#0;

  { Mouse button state masks }

  mbLeftButton = $01;
  mbRightButton = $02;

  ShowScrollBar: Boolean = True;

  ShiftState: Byte = 0;
  ShiftState2: Byte = 0;
  OldShiftState: Byte = 0;
  DoubleAltUnlock: Boolean = True;
  DoubleCtrlUnlock: Boolean = True;

type

  { Event record }

  PEvent = ^TEvent;
  TEvent = record
    What: word;
    case word of
      evNothing: ();
      evMouse: (
        Buttons: Byte;
        Double: Boolean;
        Where: TPoint);
      evKeyDown: (
        case integer of
          0: (KeyCode: LongInt);
          1: (CharCode: Char;
            ScanCode: Byte;
            ShiftCode: Byte));
      evMessage: (
        Command: word;
        case word of
          0: (InfoPtr: Pointer);
          1: (InfoLong: LongInt);
          2: (InfoWord: word);
          3: (InfoInt: integer);
          4: (InfoByte: Byte);
          5: (InfoChar: Char));
    end;

  CharDef = array[0..15] of Byte;

  { Initialized variables }
const

  ButtonCount: Byte = 0;
  MouseEvents: Boolean = False;
  MouseReverse: Boolean = False;
  MouseButtons: Byte = 0;
  DoubleDelay: word = 8;
  RepeatDelay: word = 8;
  AutoRepeat: word = 1;
  UserScreen: Pointer = nil;
  {DataCompBoy
  GraphMouse  : Boolean = False;
  MouseMouse  : Boolean = True;
DataCompBoy}
  CharHeight: Byte = 16;
  ScreenSaved: Boolean = False;
  NeedAbort: Boolean = False;
  StdMouse: Boolean = False; { Is engine really using standard mouse }
  XSens: Byte = 11;
  YSens: Byte = 11;
  CLSAct: Boolean = True;
  CurrentBlink: Boolean = False;
  MouseWhere: TPoint = (X: 0; Y: 0);

var

  { Uninitialized variables }
  OldBlink: Boolean;

  MouseIntFlag: Byte;
  UserScreenSize: word;
  UserScreenWidth: word;
  MouseX, MouseY,
  OldMouseX, OldMouseY: word;
  ShowMouseProc,
  HideMouseProc: Pointer;

  { Event manager routines }

procedure InitDrivers;
procedure DoneDrivers; {Cat}
procedure InitEvents;
procedure DoneEvents;
procedure ShowMouse;
procedure HideMouse;
procedure UpdateMouseWhere;
procedure GetMouseEvent(var Event: TEvent);
procedure GetKeyEvent(var Event: TEvent);
procedure SetMouseSpeed(XS, YS: Byte);

const

  { Initialized variables }

  StartupMode: word = $FFFF;
  SkyEnabled: integer = 0;
  TottalExit: Boolean = False;
  { Set it only when leaving your program }
  Exiting: Boolean = False;
  { Set it when temporary leaving your program }

  MouseVisible: Boolean = True;

  ScreenMode: word = $92; { sm80x25 }
  ScreenWidth: word = 80;
  ScreenHeight: word = 25;

var

  { Uninitialized variables }

  HiResScreen: Boolean;
  CheckSnow: Boolean;
  ScreenBuffer: Pointer;
  CursorLines: SmallWord;
  OldCursorShape: word;
  OldCursorPos: word;

  { ******** SYSTEM ERROR HANDLER ******** }

type

  { System error handler function type }

  TSysErrorFunc = function (ErrorCode: integer; Drive: Byte): integer;

  { Default system error handler routine }

function SystemError(ErrorCode: integer; Drive: Byte): integer;

const

  { Initialized variables }

  SysErrorFunc: TSysErrorFunc = SystemError;
  SysColorAttr: word = $4E4F;
  SysColorButtonAttr: word = $0E0F;
  SysMonoAttr: word = $7070;
  CtrlBreakHit: Boolean = False;
  SaveCtrlBreak: Boolean = False;
  SysErrActive: Boolean = False;

  { System error handler routines }

procedure InitSysError;
procedure DoneSysError;

{ ******** UTILITY ROUTINES ******** }

{ Keyboard support routines }

function GetAltChar(KeyCode: word): Char;
function GetAltCode(Ch: Char): word;
function GetCtrlChar(KeyCode: word): Char;
function GetCtrlCode(Ch: Char): word;
{function CtrlToArrow(KeyCode: Word): Word;}

{ String routines }

procedure FormatStr(var Result: String; const Format: String; var Params);
procedure PrintStr(const S: String);

{ Buffer move routines }

procedure MoveColor(var Buf; Num: word; Attr: Byte);
procedure MoveBuf(var Dest; var Source; Attr: Byte; Count: word);
procedure MoveChar(var Dest; C: Char; Attr: Byte; Count: word);
procedure MoveCStr(var Dest; const Str: String; Attrs: word);
procedure MoveStr(var Dest; const Str: String; Attr: Byte);
function CStrLen(const S: String): integer;

{$IFDEF DNPRG}
{$IFDEF LINEPOSIT}
var
  SourceFileName: PString;
  SourceLineNo: LongInt;
  {$ENDIF}

procedure EndFatalError;
{$ENDIF}
{function TVCtrlBreak: Boolean;} {JO}

implementation
uses
  Drivers2,
  VideoMan,
  Dos,
  advance,
  advance1,
  advance2,
  advance3,
  Views,
  Startup,
  DNApp,
  DNUtil,
  VPUtils, VpSysLow, VPSysLo2, {Crt,}Messages,
  FlPanelX
  ;

const
  MCurMP: CharDef = ($00, $40, $60, $30, $18, $3C, $3E, $3E, $3E, $1F,
     $07, $06, $00, $00, $00, $00);
  MCurSP: CharDef = ($BF, $1F, $0F, $87, $C3, $80, $80, $80, $80, $C0,
     $E0, $F0, $F0, $FF, $FF, $FF);
  MCurLP: CharDef = ($00, $00, $38, $04, $36, $4E, $0E, $6E, $3E, $1F,
     $07, $06, $00, $00, $00, $00);
  MCurLSP: CharDef = ($FF, $C7, $83, $C1, $80, $00, $00, $00, $80, $C0,
     $E0, $F0, $F0, $FF, $FF, $FF);
  MCurM0: CharDef = ($00, $40, $60, $70, $78, $7C, $7E, $78, $4C, $0E,
     $06, $00, $00, $00, $00, $00);
  MCurS0: CharDef = ($1F, $0F, $07, $03, $01, $00, $00, $00, $01, $00,
     $F0, $F8, $F8, $FF, $FF, $FF);

  MCurM1: CharDef = ($00, $18, $3C, $3C, $19, $7E, $BC, $3D, $7E, $98,
     $08, $04, $04, $08, $00, $00);
  MCurS1: CharDef = ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF);
  MCurM2: CharDef = ($00, $18, $3C, $3C, $98, $7E, $3D, $BC, $7E, $19,
     $10, $20, $20, $10, $00, $00);
  MCurS2: CharDef = ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF);

  StdMouseBlock: array[0..5] of Byte = ($D6, $D7, $D0, $D2, $B7, $B8);

  LastMouseMouse: Boolean = False;

  { ******** EVENT MANAGER ******** }

const

  { Event manager constants }

  EventQSize = 16;

var

  { Event manager variables }

  OldAttr: Byte;
  OldSymbols: array[0..5] of Byte;
  SaveFont: array[0..5] of CharDef;
  LastButtons: Byte;
  DownButtons: Byte;
  LastDouble: Boolean;
const
  LastWhere: TPoint = (X: 0; Y: 0);
  DownWhere: TPoint = (X: 0; Y: 0);
var
  DownTicks: word;
  AutoTicks: word;
  AutoDelay: word;
  EventCount: word;
  EventQHead: word;
  EventQTail: word;
  EventQueue: array[0..EventQSize-1] of TEvent;
  EventQLast: record
    end;

  {$I DRIVERS._VP}

  { ******** SYSTEM ERROR HANDLER ******** }
const

  { System error messages }

  {AK155}
  {$IFDEF OS2}
  CE_LastMsg = 22;
  NumButtons: array[0..22] of Byte =
    (2, 2, 2, 2, 3, 2, 3, 2, 3, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2);
  {$ELSE}
  CE_LastMsg = 19;
  NumButtons: array[0..19] of Byte =
    (2, 2, 2, 2, 3, 2, 3, 2, 3, 2, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2);
  {$ENDIF}

  ButtonX: array[2..3] of array[1..3] of Byte =
    ( (8, 23, 0), (3, 15, 27));
  XErrTable: array[1..3] of Byte = (3, 1, 0);

  { Critical error message translation table }

  CE_Idx: array[0..CE_LastMsg] of TStrIdx = (
    {00}dlCE_WriteProtected,
    {13  Attempt to write on a write-protected diskette}
    {01}dlCE_CriticalError, {14  Unknown unit}
    {02}dlCE_DiskNotReady, {15  Drive not ready}
    {03}dlCE_CriticalError, {16  Unknown command}
    {04}dlCE_DataIntegrity, {17  CRC error}
    {05}dlCE_CriticalError, {18  Bad request structure length}
    {06}dlCE_SeekError, {19  Seek error}
    {07}dlCE_UnknownMedia, {1A  Unknown media type}
    {08}dlCE_SectorNotFound, {1B  Sector not found}
    {09}dlCE_OutOfPaper, {1C  Printer out of paper}
    {10}dlCE_WriteFault, {1D  Write fault}
    {11}dlCE_ReadFault, {1E  Read fault}
    {12}dlCE_GeneralFailure, {1F  General failure}
    {13}dlCE_BadImageOfFAT, {?}
    {14}dlCE_DeviceError, {?}
    {15}dlCE_InsertDisk, {?}
    {16}dlCE_SharingViolation {20  Sharing violation}
    ,
    {17}dlCE_WrongDisk,
    {18}dlCE_DiskFull,
    {19}dlCE_DeviceInUse
    {AK155}
    {$IFDEF OS2}
    {20}, dlCE_Filename_Exced_Range
    {21}, dlCE_Dirty_Flag
    {22}, dlCE_Filename_Illegal {JO}
    {$ENDIF}
    {/AK155}
    );

var
  CE_Buttons: array[1..3] of PString;
  CE_Messages: array[0..CE_LastMsg] of PString;
  CE_K: array[1..3] of Byte;
  ErrWndTitle: PString;

  { System error handler routines }

  {$V-}
procedure SwapStatusLine(var Buffer; ShadowAttr: Byte);
  near;
  assembler;
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

function SystemError(ErrorCode: integer; Drive: Byte): integer;
  var
    Res: LongInt;
    S: String[63];
    I: Byte;
    AOptions: word;
    MsgCode: word;
  begin
  if NeedAbort then
    begin
    SystemError := 3;
    Abort := True;
    NeedAbort := False;
    exit;
    end;

  if Abort then
    begin
    SystemError := 3;
    exit;
    end;

  {AK155}
  S := '';
  if  (ErrorCode in [19..31]) then
    MsgCode := ErrorCode-19
  else if (ErrorCode in [5, 32, 33, 108]) then
    MsgCode := 16
  else if (ErrorCode = 73) then
    MsgCode := 0
  else if (ErrorCode in [34, 107]) then
    MsgCode := 17
  else if (ErrorCode in [39, 112]) then
    MsgCode := 18
  else if (ErrorCode = 99) then
    MsgCode := 19
  else
    {$IFDEF OS2}
   if (ErrorCode = 123) then
    MsgCode := 22
  else if (ErrorCode = 206) then
    MsgCode := 20
  else if (ErrorCode = 627) then
    MsgCode := 21
  else
    {$ENDIF}
    begin
    MsgCode := 12;
    end;
  // S := CE_Messages[MsgCode]^ + ' (' + S + ')';
  Str(ErrorCode, S);
  S := 'RC='+S+' '+CE_Messages[MsgCode]^;
  {/AK155}

  I := Pos('%', S);
  {Cat:warn если ошибка для сети, то будет выводиться "ошибка на диске :"}
  if I > 0 then
    begin
    S[I] := Char(Drive+65);
    S[I+1] := ':'
    end;
  if  (NumButtons[ErrorCode] = 3) then
    AOptions := mfYesButton+mfOKButton+mfNoButton+mfSysError
  else
    AOptions := mfYesButton+mfOKButton+mfSysError;
  Res := MessageBox(#03+S, nil, AOptions);
  case Res of
    cmYes:
      begin
      Abort := False;
      SystemError := 1;
      end;
    cmOK:
      begin
      Abort := True;
      SystemError := 3;
      end;
    cmNo:
      begin
      Abort := True;
      SystemError := 0;
      end
    else {case}
      begin
      Abort := True;
      SystemError := 3;
      end;
  end {case};
  end { SystemError };

{ ******** UTILITY ROUTINES ******** }

{ Keyboard support routines }

{Cat:warn есть подозрение, что GetAltCode и GetCtrlCode не работают}

const
  AltCodes2: array[$78..$83] of Char =
  '1234567890-=';

function GetAltChar(KeyCode: word): Char;
  begin
  GetAltChar := #0;
  if KeyCode = kbAltSpace then
    GetAltChar := Char(SystemMenuChar);
  if  (KeyCode shr 16) in [0, 8] then
    {Cat: проверяем, что это Alt-Char, а не Alt-Shift-Char или Ctrl-Alt-Char}
    if Lo(KeyCode) = 0 then
      case Hi(KeyCode) of
        $10..$34:
          GetAltChar := AltCodes1[Hi(KeyCode)];
        $78..$83:
          GetAltChar := AltCodes2[Hi(KeyCode)];
      end {case};
  end;

function GetAltCode(Ch: Char): word;
  var
    I: word;
  begin
  GetAltCode := 0;
  if Ch = #0 then
    exit;
  Ch := UpCase(Ch);
  if Ch = Char(SystemMenuChar) then
    begin
    GetAltCode := $0200;
    exit;
    end;
  for I := $10 to $34 do
    if AltCodes1[I] = Ch then
      begin
      GetAltCode := I shl 8;
      exit;
      end;
  for I := $78 to $83 do
    if AltCodes2[I] = Ch then
      begin
      GetAltCode := I shl 8;
      exit;
      end;
  end { GetAltCode };

function GetCtrlChar(KeyCode: word): Char;
  begin
  GetCtrlChar := #0;
  if  (KeyCode shr 16) in [0, 4] then
    {Cat: проверяем, что это Ctrl-Char, а не Ctrl-Shift-Char}
    if  (Lo(KeyCode) <> 0) and (Lo(KeyCode) <= Byte('Z')-Byte('A')+1)
    then
      GetCtrlChar := Char(Lo(KeyCode)+Byte('A')-1);
  end;

function GetCtrlCode(Ch: Char): word;
  begin
  GetCtrlCode := GetAltCode(Ch) or (Byte(UpCase(Ch))-Byte('A')+1);
  end;

{Cat: а вот это абсолютно точно не работает, т.к. сразу происходит Exit }
{     закомментировал также все вызовы этой функции }
(*
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
*)

{!SEE Drivers._??!}

{ Drivers unit initialization and shutdown }

const
  DriversInit: Boolean = False;

procedure InitDrivers;
  var
    I: integer;
    S: String;
  begin
  if DriversInit then
    exit;
  DriversInit := True;

  S := GetString(dlAltTable);
  if S <> '' then
    Move(S[1], AltCodes1, Min(Length(S), 37));

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
  end { InitDrivers };

{Cat}
procedure DoneDrivers;
  var
    I: integer;
  begin
  for I := 0 to CE_LastMsg do
    DisposeStr(CE_Messages[I]);
  for I := 1 to 3 do
    DisposeStr(CE_Buttons[I]);
  DisposeStr(ErrWndTitle);
  end;
{/Cat}

procedure SliceAwake;
  begin
  NewTimerSecs(LSliceTimer, 3);
  LSliceCnt := -2;
  end;

begin
{?} {CodePage := SysGetCodePage;}
SysTVInitCursor;
InitKeyboard;
DetectMouse;
DetectVideo;
SaveExit := ExitProc;
ExitProc := @ExitDrivers;
CtrlBreakHandler := TVCtrlBreak;
SysCtrlSetCBreakHandler;
end.
