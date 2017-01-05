//
// This source file is included as a unit used by VpSysW32.Pas, and
// implements default keyboard handling for Win32 Virtual Pascal programs.
//
// By defining the conditional define KEYDLL, compiling this file results
// in a DLL, VpKbdW32.Dll to be produced.  When this DLL is present, the
// keyboard handling functions of the DLL supercede those statically
// linked into the program, allowing for advanced customization of the
// keyboard handler.
// Some change for russian by Peter S. Voronov aka Chem O'Dun
// Some change by Jaroslaw Osadtchiy (JO) and Alexey Korop (AK155)
// Some change by Aleksej Kozlov (Cat)

{$Ifdef KeyDll} library {$Else} unit {$Endif}
VpKbdW32;

{$I ..\STDEFINE.INC}
{&Delphi+,S-,R-,Optimize+,J+,X+}

{$Ifndef KeyDll}
interface
{$Endif}

uses
  Windows;

const AltGreyAsAlt: Boolean = True; {JO}

{$Ifndef KeyDll}
procedure KbdInit(var _pSysKeyCount, _pSysKeyQue, _pSysShiftState,
    _pSysMouCount, _pSysMouQue);
procedure KbdUpdateEventQueues;
function GetWinShiftState2: byte;

implementation
{$Endif}

const
  Ctrl_Pressed = Right_Ctrl_Pressed or Left_Ctrl_Pressed;
  Alt_Pressed = Right_Alt_Pressed or Left_Alt_Pressed;
  AltCtrl_Pressed = Alt_Pressed or Ctrl_Pressed;

  // Type definitions duplicated from VpSysLow
type
  PSysPoint = ^TSysPoint;
  TSysPoint = packed record
    X, Y: SmallInt;
    end;

  TSysMouseEvent = packed record
    smeTime: longInt;
    smePos: TSysPoint;
    smeButtons: byte;
    end;

  TSysKeyEvent = packed record
    skeKeyCode: SmallWord;
    skeShiftState: byte;
    end;

  TSysMouQUeue = array[0..15] of TSysMouseEvent;
  PSysMouQueue = ^TSysMouQueue;
  TSysKeyQueue = array[0..15] of TSysKeyEvent;
  PSysKeyQueue = ^tSysKeyQueue;

  // Variables holding the keyboard and mouse queues
var
  SysKeyQue: TSysKeyQueue;
  SysMouQue: TSysMouQUeue;

const
  SysKeyCount: integer = 0;
  SysMouCount: integer = 0;
  SysShiftState: byte = 0;
  SysPlatform: integer = 0;

  // Initialisation - set VpSysW32 pointer variables to point to to here
procedure KbdInit(var _pSysKeyCount, _pSysKeyQue, _pSysShiftState,
    _pSysMouCount, _pSysMouQue);
  var
    OSVersionInfo: TOSVersionInfo;
  begin
    // Link VpSysW32 variables to the ones defined here
    Pointer(_pSysKeyCount) := @SysKeyCount;
    Pointer(_pSysKeyQue) := @SysKeyQue;
    Pointer(_pSysShiftState) := @SysShiftState;
    Pointer(_pSysMouCount) := @SysMouCount;
    Pointer(_pSysMouQue) := @SysMouQue;

    // Determine the operating system (Win95/98 or Windows NT)
    OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
    GetVersionEx(OSVersionInfo);
    SysPlatform := OSVersionInfo.dwPlatformId;
    SetConsoleMode(SysFileStdIn, ENABLE_MOUSE_INPUT);
      {Alexey Suhinin}
  end;

{ Translate Windows CtrlKeys field to the low byte of OS/2-compatible
  ShiftStates, with the following valid values:
  Bit Value
   0  0001 Right Shift
   1  0002 Left shift
   2  0004 Either Ctrl
   3  0008 Either Alt
   4  0010 ScrollLock
   5  0020 NumLock
   6  0040 CapsLock
   --- unused:
   7  0080 Insert
   8  0100 Left Ctrl
   9  0200 Left Alt
  10  0400 Right Ctrl
  11  0800 Right Alt (AltGr)
  12  1000 ScrollLock down
  13  2000 NumLock down
  14  4000 CapsLock down
  15  8000 SysReq down
}

const
  WinShiftState2: byte = 0; {как старший байт KbdGetStatus для OS/2}
const
  AdvShiftKeys: array[0..6] of byte =
  (Left_Ctrl_Pressed
  , Left_Alt_Pressed
  , Right_Ctrl_Pressed
  , Right_Alt_Pressed
  , SCROLLLOCK_ON
  , NUMLOCK_ON
  , CAPSLOCK_ON
  );

function GetWinShiftState2: byte;
  begin
    Result := WinShiftState2
  end;

function CtrlKeysToShiftState(CtrlKeys: integer): byte;
  var
    i: integer;
  begin
    WinShiftState2 := 0;
    for i := 0 to High(AdvShiftKeys) do
      if CtrlKeys and AdvShiftKeys[i] <> 0 then
        WinShiftState2 := WinShiftState2 or (1 shl i);
    Result := 0;
    if CtrlKeys and (Right_Alt_Pressed+Left_Alt_Pressed) <> 0 then
      Result := $0008;
    if CtrlKeys and (Right_Ctrl_Pressed+Left_Ctrl_Pressed) <> 0 then
      Inc(Result, $0004);
    if CtrlKeys and SHIFT_PRESSED <> 0 then
      Inc(Result, $0003);
    if CtrlKeys and SCROLLLOCK_ON <> 0 then
      Inc(Result, $0010);
    if CtrlKeys and NUMLOCK_ON <> 0 then
      Inc(Result, $0020);
    if CtrlKeys and CAPSLOCK_ON <> 0 then
      Inc(Result, $0040);
  end { CtrlKeysToShiftState };

{
  Revised implemenatation of TranslateKeyCode, where every
  effort has been made to ensure that the keys returned are identical
  for Win32 and OS/2, and that national language characters are supported.

  In Win32, "dead" keys (diacritics) are returned as normal characters,
  with nothing to distinguish them from normal keys.  This means that when
  pressing a dead key, a character will appear - but the dead key will
  still be in memory and be activated by the next keystroke.  A solution
  is wanted!
}

function TranslateKeyCode(KeyCode, ScanCode, CharCode: byte;
    ShiftState: integer): word;
  const
    // Table for CTRL + some special keys
    CtrlTable: array[71..83] of byte =
    (119, 141, 132, 142, 115, 143, 116, 144, 117, 145, 118, 146, 147);
    // Table for ALT + some special keys
    AltTable: array[71..83] of byte =
    (151, 152, 153, 74, 155, 76, 157, 78, 159, 160, 161, 162, 163);
  begin
    // First we check whether the system suggests a printable character
    if (CharCode <> 0) and (ShiftState and {Left_} {JO}Alt_Pressed = 0)
    then
      begin
        if (CharCode = 9) and (ShiftState and SHIFT_PRESSED <> 0)
        then
          begin
            // Special treatment for SHIFT + TAB
            Result := 15 shl 8;
            exit;
          end
        else
          // Special treatment for Ctrl-keys
          if (CharCode <> $E0) or ((CharCode = $E0) and (KeyCode =
              $48) and (ScanCode = $23) and (ShiftState and
              AltCtrl_Pressed = 0))
          then
            begin
              if ShiftState and $100 <> 0 then
                ScanCode := $E0; // Signal special character
              if (ShiftState and Right_Alt_Pressed <> 0) and (
                  CharCode = $F0)
              then
                CharCode := $00;
              // Ordinary characters
              Result := ScanCode shl 8 or CharCode;
              exit;
            end;
      end;

    // Default: No recognised key pressed: Return zero. The
    // calling function will not insert this into the input buffer.

    Result := 0;

    // No printable character. Return an extended keystroke based on the scan code
    case ScanCode of
      1:{AK155 Win9x: Esc with CapsLock contain CharCode=0 }
        if ShiftState = CAPSLOCK_ON then
          Result := $11B;

      2..13:// ALT + number keys, ALT + EQ, ALT + MINUS, ALT + =
        if ShiftState and Alt_Pressed <> 0 then
          Result := (ScanCode+118) shl 8
        else
          Result := ScanCode shl 8+CharCode;

      15:// AK155 TurboVision code for Ctrl-Tab
        if ShiftState and Ctrl_Pressed <> 0 then
          Result := $9400
        else if ShiftState and Alt_Pressed <> 0 then{JO: Alt-Tab}
          Result := $A500;

      {
  // Test code; should not be necessary
    27:
    begin
      // ∙^~
      if ShiftState and Left_Alt_Pressed <> 0 then
        Result := ScanCode shl 8
      else if ShiftState and Right_Alt_Pressed <> 0 then
        Result := Ord ('~')
      else if ShiftState and Shift_Pressed <> 0 then
        Result := Ord ('^')
      else
        Result := Ord ('∙');

      Exit;
    end;
}
      28:
        // ALT + Grey Enter
        if ShiftState and Alt_Pressed <> 0 then
          Result := $a600;

      14, 16..27, 29..52:
        // ALT + BS, ALT + characters, ALT + COLON, ALT + DOT
        if ShiftState and Alt_Pressed <> 0 then
          Result := ScanCode shl 8;

      53:
        // ALT + grey DIV
        if ShiftState and Alt_Pressed <> 0 then
          Result := $a400
        else if ShiftState and Ctrl_Pressed <> 0 then
          Result := $9500
        else
          Result := ScanCode shl 8;

      55:
        // ALT + grey MUL
        if ShiftState and Alt_Pressed <> 0 then
          Result := $3700
        else if ShiftState and Ctrl_Pressed <> 0 then
          Result := $9600
        else
          Result := ScanCode shl 8;

      57:// JO: Alt-Space
        if ShiftState and Alt_Pressed <> 0 then
          Result := $3920;

      59..68:
        // FN keys 1 to 10
        if ShiftState and Alt_Pressed <> 0 then
          Result := (ScanCode+45) shl 8
        else if ShiftState and Ctrl_Pressed <> 0 then
          Result := (ScanCode+35) shl 8
        else if ShiftState and SHIFT_PRESSED > 0 then
          Result := (ScanCode+25) shl 8
        else
          Result := ScanCode shl 8;

      71..83:
        // INS, DEL, HOME, END, PGUP, PGDN and CURSOR keys
        if (ShiftState and Ctrl_Pressed <> 0) then
          Result := CtrlTable[ScanCode] shl 8
        else if ShiftState and Alt_Pressed <> 0 then
          Result := AltTable[ScanCode] shl 8
        else
          Result := ScanCode shl 8;

      87, 88:
        // FN keys 11 and 12
        if ShiftState and Alt_Pressed <> 0 then
          Result := (ScanCode+52) shl 8
        else if ShiftState and Ctrl_Pressed <> 0 then
          Result := (ScanCode+50) shl 8
        else if ShiftState and SHIFT_PRESSED <> 0 then
          Result := (ScanCode+48) shl 8
        else
          Result := (ScanCode+46) shl 8;

      91:// Cat: LeftSuxx
        Result := $EC00;
      92:// Cat: RightSuxx
        Result := $ED00;
      93:// Cat: MenuSuxx
        Result := $EE00;
    end {case}; // Case ScanCode
  end { TranslateKeyCode };

procedure KbdUpdateEventQueues;
  const
    AltNumericKeys: array[0..9] of record
      VK: SmallWord;
      Value: byte
      end = (
    (VK: $9B00; Value: 4),
    (VK: $9D00; Value: 6),
    (VK: $9F00; Value: 1),
    (VK: $A100; Value: 3),
    (VK: $9700; Value: 7),
    (VK: $9900; Value: 9),
    (VK: $9800; Value: 8),
    (VK: $A000; Value: 2),
    (VK: $A200; Value: 0),
    (VK: $4C00; Value: 5));
  const
    AltNumeric: byte = 0;
    {Platform: Longint = -1;}
  var
    EventCount: DWord;
    InRec: TInputRecord;
    i: integer;
    FoundAlt: boolean;
  begin { KbdUpdateEventQueues }
    if SysKeyCount > High(SysKeyQue) then
      exit;
    repeat
      GetNumberOfConsoleInputEvents(SysFileStdIn, EventCount);
      if EventCount > 0 then
        PeekConsoleInput(SysFileStdIn, InRec, 1, EventCount);
      if EventCount = 0 then
        exit;
      {$IFDEF WIN95_HIGHPRIORITY}
      if SysPlatform = 1 then
          {Cat: если сидим в Win9x, значит для нормализации работы с консолью у нас увеличен приоритет}
        Sleep(1);
        {     и тогда здесь нужно отдать ConAgent-у время, чтобы не "глотались" нажатия клавиш}
      {$ENDIF}
      with InRec do
        case EventType of
          key_Event:
            if SysKeyCount <= High(SysKeyQue) then
              with SysKeyQue[SysKeyCount], KeyEvent do
                begin
                  {Cat: переделал так, чтобы при отпускании клавиш тоже обновлять ShiftState}
                  SysShiftState := CtrlKeysToShiftState(
                    dwControlKeyState); {Cat}
                  if bKeyDown then
                    begin
                      skeKeyCode := 0;
                      {AK155 Под Win9x 'дочитывание' при помощи readConsole необходимо для
"нормальных" символов, недопустимо для Shift-Tab, а для стрелок, Home,
End и т.п. на _некоторых_ компьютерах тоже недопустимо при использовании
стандартной переключалки keyb (от чего это зависит - неизвестно,
возможно, от BIOS. Условие wVirtualKeyCode >= $30, вроде, накрывает все.}
                      if not ((SysPlatform = 1)
                        and (wVirtualKeyCode >= $30) {AK155}
                        and (ASCIIChar <> #0)
                        and ((dwControlKeyState and AltCtrl_Pressed) = 0
                          )
                        and ReadConsole(SysFileStdIn, @UnicodeChar, 1,
                          EventCount, nil)
                        )
                      then
                        ReadConsoleInput(SysFileStdIn, InRec, 1,
                          EventCount);
                      {skeShiftState := CtrlKeysToShiftState(dwControlKeyState);}
                        {Cat}
                      {SysShiftState := skeShiftState;} {Cat}
                      skeShiftState := SysShiftState; {Cat}

                      if (wVirtualScanCode = $1C) and (SysShiftState
                          and 7 = 7)
                      then
                        ASCIIChar := #$A;
                        {AK155  Ctrl-Shift-Enter under NT}
                      skeKeyCode := TranslateKeyCode(wVirtualKeyCode,
                        wVirtualScanCode, Ord(ASCIIChar),
                        dwControlKeyState);
                      if skeKeyCode = 0 then
                        exit; // Do not report non-event

                      // If numeric keypad, Alt pressed: record keys
                      FoundAlt := False;
                      if (skeShiftState and $8 = $8) and (
                          dwControlKeyState and $100 = 0)
                      then
                        if SysPlatform = 1 then// Windows 95
                          begin
                            for i := 0 to 9 do
                              if skeKeyCode = AltNumericKeys[i].VK
                              then
                                begin
                                  AltNumeric := AltNumeric*10+
                                    AltNumericKeys[i].Value;
                                  FoundAlt := True;
                                end;
                          end
                        else// Windows NT
                          if wVirtualKeyCode in [VK_NUMPAD0..
                              VK_NUMPAD9]
                          then
                            begin
                              AltNumeric := AltNumeric*10+
                                wVirtualKeyCode-VK_NUMPAD0;
                              FoundAlt := True;
                            end;

                      if not FoundAlt then
                        begin
                          Inc(SysKeyCount);
                          AltNumeric := 0;
                        end;
                    end
                  else
                    begin
                      case KeyEvent.wVirtualKeyCode of
                        VK_MENU:
                          if (AltNumeric <> 0) then
                            begin
                              skeKeyCode := AltNumeric;
                              AltNumeric := 0;
                              Inc(SysKeyCount);
                            end;
                      end {case};
                      ReadConsoleInput(SysFileStdIn, InRec, 1,
                        EventCount);
                    end;
                end;
          _mouse_Event:
            begin
              if SysMouCount <= High(SysMouQue) then
                with SysMouQue[SysMouCount] do
                  begin
                    smePos.X := MouseEvent.dwMousePosition.X;
                    smePos.Y := MouseEvent.dwMousePosition.Y;
                    smeButtons := MouseEvent.dwButtonState and $0003;
                    smeTime := SysSysMsCount;
                    Inc(SysMouCount);
                  end;
              ReadConsoleInput(SysFileStdIn, InRec, 1, EventCount);
            end;
          else
            ReadConsoleInput(SysFileStdIn, InRec, 1, EventCount);
        end {case};
    until (False) or (SysKeyCount > High(SysKeyQue));
  end { KbdUpdateEventQueues };

{$Ifdef KeyDll}
exports
KbdInit Name'KbdInit',
KbdUpdateEventQueues Name'KbdUpdateEventQueues';
{$Endif}

begin
end.
