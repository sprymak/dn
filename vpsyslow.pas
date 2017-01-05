// |---------------------------------------------------------|
// |                                                         |
// |     Virtual Pascal Runtime Library.  Version 2.1.       |
// |     System interface layer for all OSes                 |
// |     ----------------------------------------------------|
// |     Copyright (C) 1995-2003 vpascal.com                 |
// |                                                         |
// |---------------------------------------------------------|

{&OrgName+,Speed+,AlignCode+,AlignRec-,CDecl-,Far16-,Frame+,Delphi+}
{$X+,W-,I-,J+,H-,Delphi+,R-,S-,Q-,B-,T-,Use32+}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

unit VpSysLow;
{$I STDEFINE.INC}

//JO: uncomment this define to use this unit with
//    Virtual Pascal 2.1 Build 243
//    or keep it commented to use with
//    Virtual Pascal 2.1 Build 274
{.$DEFINE B243}

interface

uses
  {$IFDEF OS2}Os2Def, Os2Base; {$Undef KeyDll} {$ENDIF}
{$IFDEF LINUX}Linux; {$ENDIF}
{$IFDEF WIN32}Windows; {$ENDIF}
{$IFDEF DPMI32}Dpmi32df;

var
  Video_Adapter_Found: (mda_found, cga_found, ega_found, vga_found);
  {$ENDIF}

  {$IFDEF WIN32}
var
  RecodeAnsiNames: boolean; {Cat, AK155}
  hExtCmd: THandle;
procedure InitialiseKeyboardHandler;
{$ENDIF}

type
  TQuad = Comp;
  PQuad = ^TQuad;
  TSemHandle = longInt;

  {$IFDEF OS2}
  { не позиционировать мышь в левый верхний угол}
const
  NoMouseMove: boolean = False;
  {$ENDIF}

const
  {$IFDEF OS2}
  SemInfinite = sem_indefinite_wait;
  PathSeparator = ';';
  AllFilesMask = '*';
  {$ENDIF}
  {$IFDEF WIN32}
  SemInfinite = INFINITE;
  PathSeparator = ';';
  AllFilesMask = '*';
  {$ENDIF}
  {$IFDEF DPMI32}
  SemInfinite = -1; // not used
  PathSeparator = ';';
  AllFilesMask = '*.*';
  {$ENDIF}
  {$IFDEF LINUX}
  SemInfinite = -1; // not used
  Exception_Maximum_Parameters = 4;
  PathSeparator = ':';
  AllFilesMask = '*';
  {$ENDIF}

  {$IFDEF LINUX}
  sysmem_Read = PROT_READ;
  sysmem_Write = PROT_WRITE;
  sysmem_Execute = PROT_EXEC;
  sysmem_Guard = 0; // Not supported
  sysmem_Default = PROT_READ or PROT_EXEC;
  {$ELSE}
  sysmem_Read = $01;
  sysmem_Write = $02;
  sysmem_Execute = $04;
  sysmem_Guard = $08;
  sysmem_Default = $05;
  {$ENDIF}

const
  // SysFileOpen_Create flags
  // Flags: If the file already exists; set only one of these
  create_FailIfExists = $0000;
  create_TruncateIfExists = $0001;

  // Flags: If the file does not exist; set only one of these
  open_FailIfNew = $0000; // ocFileOpen fails if no file
  open_CreateIfNew = $0001; // ocFileOpen creates file if no file
  open_TruncateIfExists = $0002;
    // ocFileOpen truncates existing file

  // Required by the System unit
function SysFileStdIn: longInt;
function SysFileStdOut: longInt;
function SysFileStdErr: longInt;
function SysFileOpen(FileName: PChar; Mode: longInt; var Handle:
  longInt): longInt;
function SysFileCreate(FileName: PChar; Mode, Attr: longInt; var
    Handle: longInt): longInt;
function SysFileOpen_Create(Open: boolean; FileName: PChar; Mode,
    Attr, Action: longInt; var Handle: longInt): longInt;
function SysFileCopy(_Old, _New: PChar; _Overwrite: boolean):
  boolean;
function SysFileSeek(Handle, Distance, Method: longInt; var Actual:
    longInt): longInt;
function SysFileRead(Handle: longInt; var Buffer; Count: longInt;
    var Actual: longInt): longInt;
function SysFileWrite(Handle: longInt; const Buffer; Count: longInt;
    var Actual: longInt): longInt;
function SysFileSetSize(Handle, NewSize: longInt): longInt;
function SysFileClose(Handle: longInt): longInt;
function SysFileFlushBuffers(Handle: longInt): longInt;
function SysFileDelete(FileName: PChar): longInt;
function SysFileMove(OldName, NewName: PChar): longInt;
function SysFileIsDevice(Handle: longInt): longInt;
function SysDirGetCurrent(Drive: longInt; Path: PChar): longInt;
function SysDirSetCurrent(Path: PChar): longInt;
function SysDirCreate(Path: PChar): longInt;
function SysDirDelete(Path: PChar): longInt;
function SysMemAvail: longInt;
function SysMemAlloc(Size, Flags: longInt; var MemPtr: Pointer):
  longInt;
function SysMemFree(MemPtr: Pointer): longInt;
function SysSysMsCount: longInt;
procedure SysSysWaitSem(var Sem: longInt);
procedure SysSysSelToFlat(var P: Pointer);
procedure SysSysFlatToSel(var P: Pointer);
function SysCtrlSelfAppType: longInt;
function SysCtrlCreateThread(Attrs: Pointer; StackSize: longInt;
  func, Param: Pointer; Flags: longInt; var Tid: longInt): longInt;
function SysCtrlKillThread(Handle: longInt): longInt;
function SysCtrlSuspendThread(Handle: longInt): longInt;
function SysCtrlResumeThread(Handle: longInt): longInt;
function SysGetThreadId: longInt;
function SysGetProcessId: longInt;
procedure SysCtrlExitThread(ExitCode: longInt);
procedure SysCtrlExitProcess(ExitCode: longInt);
function SysCtrlGetModuleName(Handle: longInt; Buffer: PChar):
  longInt;
procedure SysCtrlEnterCritSec;
procedure SysCtrlLeaveCritSec;
function SysCtrlGetTlsMapMem: Pointer;
function SysCmdln: PChar;
function SysCmdlnCount: longInt;
procedure SysCmdlnParam(Index: longInt; var Param: ShortString);
function SysGetEnvironment: PChar;
procedure SysFreeEnvironment(_Env: PChar);

// Dos, WinDos, SysUtils

type
  TOSSearchRec = packed record
    Handle: longInt;
    NameLStr: Pointer;
    Attr: byte;
    Time: longInt;
    Size: longInt;
    Name: ShortString;
    Filler: array[0..3] of Char;
    {$IFDEF WIN32}
    ExcludeAttr: longInt;
    FindData: TWin32FindData;
    {$ENDIF}
    {$IFDEF DPMI32}
    attr_must: byte;
    dos_dta:
    record
      fill: array[1..21] of byte;
      Attr: byte;
      Time: longInt;
      Size: longInt;
      Name: array[0..12] of Char;
      end;
    {$ENDIF}
    {$IFDEF LINUX}
    FindDir: array[0..255] of Char;
    FindName: ShortString;
    FindAttr: longInt;
    {$ENDIF}
    end;

  PLongInt = ^Longint;
    // Define here rather than using Use32 definition
  THandle = longInt;

function SysOsVersion: longInt;
procedure SysGetDateTime(Year, Month, Day, DayOfWeek, Hour, Minute,
  Second, MSec: PLongInt);
procedure SysSetDateTime(Year, Month, Day, Hour, Minute, Second,
    MSec: PLongInt);
function SysVerify(SetValue: boolean; Value: boolean): boolean;
function SysDiskFree(Drive: byte): longInt;
function SysDiskSize(Drive: byte): longInt;
function SysDiskFreeLong(Drive: byte): TQuad;
function SysDiskSizeLong(Drive: byte): TQuad;
function SysDiskFreeLongX(Path: PChar): TQuad; {Cat}
function SysDiskSizeLongX(Path: PChar): TQuad; {Cat}
function SysGetFileAttr(FileName: PChar; var Attr: longInt): longInt;
function SysSetFileAttr(FileName: PChar; Attr: longInt): longInt;
function SysGetFileTime(Handle: longInt; var Time: longInt): longInt;
function SysSetFileTime(Handle: longInt; Time: longInt): longInt;
function SysFindFirst(Path: PChar; Attr: longInt; var F:
    TOSSearchRec; IsPChar: boolean): longInt;
function SysFindNext(var F: TOSSearchRec; IsPChar: boolean): longInt;
function SysFindClose(var F: TOSSearchRec): longInt;
function SysFileSearch(Dest, Name, List: PChar): PChar;
function SysFileExpand(Dest, Name: PChar): PChar;
function SysFileAsOS(FileName: PChar): boolean;
function SysExecute(Path, CmdLine, Env: PChar; Async: boolean; Pid:
    PLongInt; StdIn, StdOut, StdErr: longInt): longInt;
function SysExitCode: longInt;
function SysFileExists(const FileName: PChar): boolean;

// Memory mapping functions.  The Alloc and Access functions return
// a handle or -1 (invalid).
function SysAllocSharedMemory(var _Base: Pointer; _Name: PChar;
    _Size: longInt): longInt;
function SysAccessSharedMemory(var _Base: Pointer; _Name: PChar):
  longInt;
procedure SysFreeSharedMemory(_Base: Pointer; _Handle: longInt);

// Semaphores

function SemCreateEvent(_Name: PChar; _Shared, _State: boolean):
  TSemHandle;
function SemAccessEvent(_Name: PChar): TSemHandle;
function SemPostEvent(_Handle: TSemHandle): boolean;
function SemResetEvent(_Handle: TSemHandle; var _PostCount: longInt):
  boolean;
function SemWaitEvent(_Handle: TSemHandle; _TimeOut: longInt):
  boolean;
procedure SemCloseEvent(_Handle: TSemHandle);

function SemCreateMutex(_Name: PChar; _Shared, _State: boolean):
  TSemHandle;
function SemAccessMutex(_Name: PChar): TSemHandle;
function SemRequestMutex(_Handle: TSemHandle; _TimeOut: longInt):
  boolean;
function SemReleaseMutex(_Handle: TSemHandle): boolean;
procedure SemCloseMutex(_Handle: TSemHandle);

// Memory management

function SysMemInfo(_Base: Pointer; _Size: longInt; var _Flags:
    longInt): boolean;
function SysSetMemProtection(_Base: Pointer; _Size: longInt; _Flags:
    longInt): boolean;
function PhysMemAvail: longInt; {AK155 20-08-2003}

// GUI

{procedure SysMessageBox(_Msg, _Title: PChar; _Error: Boolean);}

// VPUtils

type
  TDriveType = (dtFloppy, dtHDFAT, dtHDHPFS, dtInvalid,
  dtNovellNet, dtCDRom, dtLAN, dtHDNTFS, dtUnknown,
  dtTVFS, dtHDExt2, dtJFS);

function SysGetVolumeLabel(Drive: Char): ShortString;
function SysSetVolumeLabel(Drive: Char; _Label: ShortString):
  boolean;
function SysGetForegroundProcessId: longInt;
function SysGetBootDrive: Char;
function SysGetDriveType(Drive: Char): TDriveType;
function SysGetVideoModeInfo(var Cols, Rows, Colours: word): boolean;
function SysSetVideoMode(Cols, Rows: word): boolean;
function SysGetVisibleLines(var Top, Bottom: longInt): boolean;

// Crt

function SysKeyPressed: boolean;
function SysReadKey: Char;
function SysPeekKey(var Ch: Char): boolean;
procedure SysFlushKeyBuf;
procedure SysGetCurPos(var X, Y: SmallWord);
procedure SysWrtCharStrAtt(CharStr: Pointer; len, X, Y: SmallWord;
    var Attr: byte);
function SysReadAttributesAt(X, Y: SmallWord): byte;
function SysReadCharAt(X, Y: SmallWord): Char;
procedure SysScrollUp(X1, Y1, X2, Y2, Lines, Cell: SmallWord);
procedure SysScrollDn(X1, Y1, X2, Y2, Lines, Cell: SmallWord);
procedure SysBeepEx(Freq, Dur: longInt);
{$IFDEF DPMI32}
procedure SysSound(Freq: longInt);
procedure SysNoSound;
{$ENDIF}

// TVision and Crt

type
  PSysPoint = ^TSysPoint;
  TSysPoint = packed record
    X, Y: SmallInt;
    end;

  PSysRect = ^TSysRect;
  TSysRect = packed record
    A, B: TSysPoint;
    end;

type
  TSysMouseEvent = packed record
    smeTime: longInt;
    smePos: TSysPoint;
    smeButtons: byte;
    end;

  TSysKeyEvent = packed record
    skeKeyCode: SmallWord;
    skeShiftState: byte;
    end;

function SysTVDetectMouse: longInt;
procedure SysTVInitMouse(var X, Y: integer);
procedure SysTVDoneMouse(Close: boolean);
procedure SysTVShowMouse;
procedure SysTVHideMouse;
procedure SysTVUpdateMouseWhere(var X, Y: integer);
function SysTVGetMouseEvent(var Event: TSysMouseEvent): boolean;
procedure SysTVKbdInit;
function SysTVGetKeyEvent(var Event: TSysKeyEvent): boolean;
function SysTVPeekKeyEvent(var Event: TSysKeyEvent): boolean;
function SysTVGetShiftState: byte;
procedure SysTVSetCurPos(X, Y: integer);
procedure SysTVSetCurType(Y1, Y2: integer; Show: boolean);
procedure SysTVGetCurType(var Y1, Y2: integer; var Visible: boolean);
procedure SysTvShowBuf(Pos, Size: integer);
procedure SysTVClrScr;
function SysTvGetScrMode(_Size: PSysPoint; _Align: boolean): integer;
procedure SysTVSetScrMode(Mode: integer);
function SysTVGetSrcBuf: Pointer;
procedure SysTVInitCursor;
procedure SysTvDoneCursor;
procedure SysCtrlSleep(Delay: integer);
function SysGetValidDrives: longInt;

// Other

type
  TCtrlBreakHandler = function : boolean;
  TCharCase = (ccLower, ccUpper, ccAnsiLower, ccAnsiUpper);

const
  CtrlBreakHandler: TCtrlBreakHandler = nil;
  TVVioHandle: word = 0;

function SysGetCodePage: longInt;
procedure SysCtrlSetCBreakHandler;
function SysFileIncHandleCount(Count: longInt): longInt;
function SysGetSystemSettings: longInt;
function SysCompareStrings(s1, s2: PChar; l1, L2: longInt;
  IgnoreCase: boolean): longInt;
procedure SysChangeCase(Source, Dest: PChar; len: longInt; NewCase:
    TCharCase);
function SysLowerCase(s: PChar): PChar;
function SysUpperCase(s: PChar): PChar;

// IDE

procedure SysDisableHardErrors;
function SysKillProcess(Process: longInt): longInt;
function SysAllocSharedMem(Size: longInt; var MemPtr: Pointer):
  longInt;
function SysGiveSharedMem(MemPtr: Pointer): longInt;

function SysPipeCreate(var ReadHandle, WriteHandle: longInt; Size:
    longInt): longInt;
function SysPipePeek(Pipe: longInt; Buffer: Pointer; BufSize:
    longInt; var BytesRead: longInt; var IsClosing: boolean):
  longInt;
function SysPipeClose(Pipe: longInt): longInt;

// Required by SysUtils unit
const
  Open_Access_ReadOnly = $0000; { ---- ---- ---- -000 }
  Open_Access_WriteOnly = $0001; { ---- ---- ---- -001 }
  Open_Access_ReadWrite = $0002; { ---- ---- ---- -010 }
  Open_Share_DenyReadWrite = $0010; { ---- ---- -001 ---- }
  Open_Share_DenyWrite = $0020; { ---- ---- -010 ---- }
  Open_Share_DenyRead = $0030; { ---- ---- -011 ---- }
  open_share_DenyNone = $0040; { ---- ---- -100 ---- }

  xcpt_Signal_Ctrl_C =
  {$IFDEF OS2}xcpt_Signal; {$ENDIF}
  {$IFDEF WIN32}xcpt_Control_C_exit; {$ENDIF}
  {$IFDEF DPMI32}xcpt_Ctrl_Break; {$ENDIF}
  {$IFDEF LINUX}xcpt_Ctrl_Break; {$ENDIF}

type
  TQuadRec = record
    Lo, Hi: longInt;
    end;

  POSExceptionRecord = ^TOSExceptionRecord;
  TOSExceptionRecord = record
    fExceptionNum: longInt; { exception number }
    fHandlerFlags: longInt;
    fNestedExceptionRecord: POSExceptionRecord;
    fExceptionAddress: Pointer;
    fParameters: longInt; { Size of Exception Specific Info }
    fExceptionInfo: array[0..Exception_Maximum_Parameters-1] of
      longInt;
    end;

procedure SysGetCaseMap(TblLen: longInt; Tbl: PChar);
procedure SysGetWeightTable(TblLen: longInt; WeightTable: PChar);
function SysLoadResourceString(Id: longInt; Buffer: PChar; BufSize:
    longInt): PChar;
function SysFileExpandS(Name: ShortString): ShortString;
function SysGetSystemError(Code: longInt; Buffer: PChar; BufSize:
    longInt; var MsgLen: longInt): PChar;
function SysGetModuleName(var Address: Pointer; Buffer: PChar;
    BufSize: longInt): PChar;
function SysFileUNCExpand(Dest, Name: PChar): PChar;
procedure SysGetCurrencyFormat(CString: PChar; var CFormat,
    CNegFormat, CDecimals: byte; var CThousandSep, CDecimalSep: Char);
procedure SysGetDateFormat(var DateSeparator: Char; ShortDateFormat,
    LongDateFormat: PChar);
procedure SysGetTimeFormat(var TimeSeparator: Char; TimeAMString,
    TimePMString, ShortTimeFormat, LongTimeFormat: PChar);
procedure SysDisplayConsoleError(PopupErrors: boolean; Title, Msg:
    PChar);
procedure SysDisplayGUIError(Title, Msg: PChar);
function SysPlatformId: longInt;
function SysPlatformName: String;
function SysPlatformNameForId(_Id: integer): String;
procedure SysBeep;
procedure SysLowInit;

// Clipboard interface

{$IFDEF WIN32}
function SysClipCanPaste: boolean;
function SysClipCopy(P: PChar; Size: longInt): boolean;
function SysClipPaste(var Size: integer): Pointer;
{$ENDIF}

{$IFDEF DPMI32}
{$ENDIF}

{$IFDEF LINUX}
function SysConvertFileName(Dest, Source: PChar; DestFS, SourceFS:
    TFileSystem): PChar;
function SysIsValidFileName(FileName: PChar; FileSystem: TFileSystem):
  boolean;
{$ENDIF}

{$IFDEF OS2}
// Routines used to safely call 16-bit OS/2 functions
function Invalid16Parm(const _p: Pointer; const _Length: longInt):
  boolean;
function Fix_64k(const _Memory: Pointer; const _Length: longInt):
  Pointer;
{$ENDIF}

function SysPathSep: Char;

implementation

{&OrgName-}

uses
  {$Ifdef Win32} {$Ifndef KeyDll}
  VpKbdW32, // Statically linked default Win32 keyboard handler
  {$Endif} {$Endif}
  {$Ifdef DPMI32}
  Dpmi32, D32Res, // Dpmi support files
  {$Endif}
  {$IFDEF LINUX}
  LnxRes,
  {$ELSE}
  ExeHdr,
  {$ENDIF}
  Strings;

const
  // Max. Amount of TLS memory
  {$IFDEF B243}
  SharedMemSize = 8*1024;
  {$ELSE}
  SharedMemSize = 32*1024;
  {$ENDIF}

type
  // This type *must* be in sync with System.TSharedMem or *everything* breaks
  PSharedMem = ^TSharedMem;
  TSharedMem = record
    TlsPerThread: Pointer; // Actual TLS
    MaxThreadCount: longInt; // Max thread ID so far
    MaxThreadId: longInt; // Updated before MaxThreadCount
    TlsMemMgr: TMemoryManager; // Memory Manager used by Tls Mgr
    {$IFNDEF B243} {Fields added in build 274 of VP 2.1}
    HeapSemaphore: longInt; // For synchronizing heap access
    HeapLockCount: longInt;
    HeapOwnerTid: longInt;
    {$ENDIF}
    end;

  TDateTimeRec = record
    FTime, FDate: SmallWord;
    end;

procedure SysSysWaitSem(var Sem: longInt); {&USES None} {&FRAME-}
asm
      @@1:
        mov     eax,Sem
   lock bts     [eax].Longint,0
        jnc     @@RET
        push    31              // Wait for at least one timer slice
        Call    SysCtrlSleep    // and try to check again
        jmp     @@1
      @@RET:
end
  ;

function SysFileExpandS(Name: ShortString): ShortString;
  begin
    Name[Length(Name)+1] := #0;
    SysFileExpand(@Result[1], @Name[1]);
    SetLength(Result, StrLen(@Result[1]));
  end;

function SysDiskFree(Drive: byte): longInt;
  var
    Temp: TQuad;
  begin
    Temp := SysDiskFreeLong(Drive);
    Result := TQuadRec(Temp).Lo;
    if Temp > MaxLongInt then
      Result := MaxLongInt; // Handle overflow
  end;

function SysDiskSize(Drive: byte): longInt;
  var
    Temp: TQuad;
  begin
    Temp := SysDiskSizeLong(Drive);
    Result := TQuadRec(Temp).Lo;
    if Temp > MaxLongInt then
      Result := MaxLongInt; // Handle overflow
  end;

function SysPathSep: Char;
  begin
    {$IFDEF LINUX}
    if FileSystem = fsUnix then
      Result := '/'
    else
      Result := '\';
    {$ELSE}
    Result := '\';
    {$ENDIF}
  end;

function SysPlatformNameForId(_Id: integer): String;
  begin
    case _Id of
      -3:
        Result := 'Linux';
      -2:
        Result := 'DPMI';
      -1:
        Result := 'OS/2';
      0:
        Result := 'Win32s';
      1:
        Result := 'Win9x';
      2:
        Result := 'WinNT';
      else
        Result := 'Unknown';
    end {case};
  end;

function SysPlatformName: String;
  begin
    Result := SysPlatformNameForId(SysPlatformId);
  end;

function SysFileExists(const FileName: PChar): boolean;
  var
    s: TOSSearchRec;
  begin
    Result := SysFindFirst(FileName, $27, s, True) = 0;
    if Result then
      SysFindClose(s);
  end;

// Include platform specific implementations

{$IFDEF OS2}
{$I VpSysOs2.Pas}
{$ENDIF}

{$IFDEF WIN32}
{$I VpSysW32.Pas}
{$ENDIF}

{$IFDEF DPMI32}
{$I VpSysD32.Pas}
{$ENDIF}

{$IFDEF LINUX}
{$I VpSysLnx.Pas}
{$ENDIF}

end.
