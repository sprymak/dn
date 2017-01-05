// |---------------------------------------------------------|
// |                                                         |
// |     Virtual Pascal Runtime Library.  Version 2.1.       |
// |     System interface layer for Win32                    |
// |     ----------------------------------------------------|
// |     Copyright (C) 1995-2003 vpascal.com                 |
// |                                                         |
// |---------------------------------------------------------|
{$DEFINE RouteConsoleToStdInOut} // !!.kg
{.$DEFINE AutoKbdUpdateEventQueues} // Cat:экспериментальный
{.$DEFINE LazyCurTypeUpdate} // Cat:экспериментальный
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}
{AK155 = Alexey Korop, 2:261/155@fidonet}

{Cat
   18-10-2001 - изменил функции работы с Clipboard-ом, теперь
   вместо описателя данных cf_Text и перекодировок Ansi-Oem
   используется описатель cf_OemText

   19-10-2001 - теперь используется собственный RecodeCyrillicNames,
   который должен быть проинициализирован в модуле DnIni

   09-12-2001 - окончательно вычистил загрузку VPKBDW32.DLL;
   убрал вызовы InitialiseKeyboardHandler из всех мест, кроме
   InitialiseConsole, вызов которой происходит при инициализации
   модуля

   10-12-2001 - раньше установка позиции курсора происходила в
   отдельной нитке, теперь в эту же нитку добавил ещё и изменение
   вида курсора (SetCurType, GetCurType); идея такая: после запроса
   на изменение ждём 0.1 секунды перед тем, как на самом деле
   изменить вид курсора, а если за это время произойдёт ещё один
   запрос на изменение - старый запрос игнорируем
}
{AK155
   10-01-2002 - изменил функции работы с Clipboard-ом, теперь
   под WinNT вместо описателя cf_OemText используется cf_UnicodeText
   и выполняются соответствующие перекодировки. Без этого не удается
   получить одновременно корректную работу с русскими буквами и
   с псевдографикой, притом, чтобы взятое в буфер в DN нормально
   вставлялось и в консоль, и в GUI.
}
{Cat
   20-03-2002 - теперь для получения размера диска и размера
   свободного места на диске там, где это возможно, используется
   функция GetDiskFreeSpaceExA. Это нужно для устранения проблемы
   с неправильным показом этих параметров под Win9x (раньше если
   размер диска или размер свободного места на диске превышал 2Г,
   то он показывался равным 2Г)

   13-11-2002 - теперь нараяду с функциями
      function SysDiskFreeLong(Drive: Byte): TQuad;
      function SysDiskSizeLong(Drive: Byte): TQuad;
   существуют их аналоги, работающие не с буквой диска, а с полным путём:
      function SysDiskFreeLongX(Path: PChar): TQuad;
      function SysDiskSizeLongX(Path: PChar): TQuad;
   Это нужно для получения размеров для сетевых путей, а также в случаях,
   когда один диск подмонтирован в какой-то каталог другого
}

var
  SysBufInfo: TConsoleScreenBufferInfo;
  SysScrBuf: array[0..$13000] of Char;

type
  TSysMouQUeue = array[0..15] of TSysMouseEvent;
  PSysMouQueue = ^TSysMouQueue;
  TSysKeyQueue = array[0..15] of TSysKeyEvent;
  PSysKeyQueue = ^tSysKeyQueue;

const
  // Pointers to keyboard interface variables located either in the
  // VpKbdW32 unit, or in the VpKbdW32.Dll file, if available
  pSysKeyCount: PLongInt = nil;
  pSysMouCount: PLongInt = nil;
  pSysKeyQue: PSysKeyQueue = nil;
  pSysMouQue: PSysMouQueue = nil;
  pSysShiftState: PByte = nil;

  SysConIn: longInt = -1;
  SysConOut: longInt = -1;
  tidCursor: longInt = -1; // Thread ID of cursor thread
  semCursor: longInt = -1;
    // Event semaphore, set when cursor pos changes
  CurXPos: longInt = -1; // Internally maintained cursor position
  CurYPos: longInt = -1;
  SysPlatform: longInt = -1; // Platform ID, from SysPlatformID
  {SysSpecialKeys: Set of Byte = [0, $e0];}
  SysSpecialKeys: Set of byte = [0]; {JO}
  LastX: longInt = -1;
  LastY: longInt = -1;


type
  PStandardCell = ^TStandardCell;
  TStandardCell = packed record
    Ch: Char;
    Attr: byte;
    end;

  TKbdInit = procedure (var _pSysKeyCount, _pSysKeyQue,
    _pSysShiftState, _pSysMouCount, _pSysMouQue);
  TKbdUpdateEventQueues = procedure;

const
  (*
  KbdDllName     = 'VpKbdW32.Dll'; // Name of keyboard handler DLL
  hKbdDll        : Longint = -1;   // Handle of keyboard DLL
  name_KbdInit             = 'KbdInit';
  name_KbdUpdateEventQueue = 'KbdUpdateEventQueues';

  pKbdInit : TKbdInit = nil;
  pKbdUpdateEventQueues : TKbdUpdateEventQueues = nil;
*)
  pKbdInit: TKbdInit = VpKbdW32.KbdInit; {Cat}
  pKbdUpdateEventQueues: TKbdUpdateEventQueues = VpKbdW32.
    KbdUpdateEventQueues; {Cat}

  GetDiskFreeSpaceEx: function (Drive: PChar; var AvailableForCaller,
    Total, Free): Longbool stdcall = nil; {Cat}

  {&StdCall+}
const
  // Function pointer to GetDiskFreeSpaceEx function, available in
  // later versions of Win95 and working for 2GB+ disks
  pGetDiskFreeSpaceEx: function (RootPathName: PChar;
  var FreeBytesAvailableToCaller, TotalNumberOfBytes: TQuad;
  pTotalNumberOfFreeBytes: PQuad): Bool = nil;
  {&StdCall-}

function QueryProcAddr(Name: PChar; IsKernel: boolean): Pointer;
  const
    Names: array[boolean] of PChar = ('user32.dll', 'kernel32.dll');
    Handles: array[boolean] of THandle = (0, 0);
  var
    k: boolean;
  begin
    k := IsKernel;
    if Handles[k] = 0 then
      Handles[k] := LoadLibrary(Names[k]);
    Result := GetProcAddress(Handles[k], Name);
  end;

const
  AccessMode: array[0..2] of integer = (
  generic_Read, GENERIC_WRITE, generic_Read or GENERIC_WRITE);
  ShareMode: array[0..4] of integer = (
  0, 0, FILE_SHARE_READ, FILE_SHARE_WRITE, FILE_SHARE_READ or
    FILE_SHARE_WRITE);

function SetResult(Success: boolean): longInt;
  begin
    Result := 0;
    if not Success then
      Result := GetLastError;
  end;

function SysFileStdIn: longInt;
  begin
    Result := GetStdHandle(std_Input_Handle);
  end;

function SysFileStdOut: longInt;
  begin
    Result := GetStdHandle(std_Output_Handle);
  end;

function SysFileStdErr: longInt;
  begin
    Result := GetStdHandle(std_Error_Handle);
  end;

function SysFileOpen_Create(Open: boolean; FileName: PChar; Mode,
    Attr, Action: longInt; var Handle: longInt): longInt;
  var
    SA: TSecurityAttributes;
    APIFlags: longInt;
  begin
    if Open then
      if Action and open_CreateIfNew <> 0 then
        APIFlags := open_Always // Open or create
      else if Action and open_TruncateIfExists <> 0 then
        APIFlags := truncate_existing // Open and truncate
      else
        APIFlags := OPEN_EXISTING // Open; fail if no file
    else if Action and create_TruncateIfExists <> 0 then
      APIFlags := create_Always // Create and truncate
    else
      APIFlags := create_New; // Create; fail if exists

    SA.nLength := SizeOf(SA);
    SA.lpSecurityDescriptor := nil;
    SA.bInheritHandle := True;
    Handle := CreateFile(FileName, AccessMode[Mode and 3], ShareMode[(
      Mode and $F0) shr 4],
    @SA, APIFlags, file_attribute_Normal, 0);
    Result := SetResult(Handle <> invalid_Handle_Value);
  end { SysFileOpen_Create };

function SysFileOpen(FileName: PChar; Mode: longInt; var Handle:
    longInt): longInt;
  var
    SA: TSecurityAttributes;
  begin
    SA.nLength := SizeOf(SA);
    SA.lpSecurityDescriptor := nil;
    SA.bInheritHandle := True;
    Handle := CreateFile(FileName, AccessMode[Mode and 3], ShareMode[(
      Mode and $F0) shr 4],
    @SA, OPEN_EXISTING, file_attribute_Normal, 0);
    Result := SetResult(Handle <> invalid_Handle_Value);
  end;

function SysFileCreate(FileName: PChar; Mode, Attr: longInt; var
    Handle: longInt): longInt;
  var
    SA: TSecurityAttributes;
  begin
    SA.nLength := SizeOf(SA);
    SA.lpSecurityDescriptor := nil;
    SA.bInheritHandle := True;
    Handle := CreateFile(FileName, AccessMode[Mode and 3], ShareMode[(
      Mode and $F0) shr 4],
    @SA, create_Always, file_attribute_Normal, 0);
    Result := SetResult(Handle <> invalid_Handle_Value);
  end;

function SysFileCopy(_Old, _New: PChar; _Overwrite: boolean):
    boolean;
  begin
    Result := CopyFile(_Old, _New, not _Overwrite);
  end;

function SysFileSeek(Handle, Distance, Method: longInt; var Actual:
    longInt): longInt;
  begin
    Actual := SetFilePointer(Handle, Distance, nil, Method);
    Result := SetResult(Actual <> $FFFFFFFF);
  end;

function SysFileRead(Handle: longInt; var Buffer; Count: longInt;
    var Actual: longInt): longInt;
  begin
    Result := SetResult(ReadFile(Handle, Buffer, Count, DWord(Actual),
      nil));
  end;

function SysFileWrite(Handle: longInt; const Buffer; Count: longInt;
    var Actual: longInt): longInt;
  var
    sbi: TConsoleScreenBufferInfo;
  begin
    Result := SetResult(WriteFile(Handle, Buffer, Count, DWord(
      Actual), nil));
    if (tidCursor <> -1) and (Handle = SysConOut) then
      begin
        // Writeln without Crt unit: Update cursor position variable
        GetConsoleScreenBufferInfo(SysConOut, sbi);
        CurXPos := sbi.dwCursorPosition.X;
        CurYPos := sbi.dwCursorPosition.Y;
      end;
  end;

function SysFileSetSize(Handle, NewSize: longInt): longInt;
  var
    CurPos: longInt;
  begin
    CurPos := SetFilePointer(Handle, 0, nil, file_Current);
    Result := SetResult((CurPos <> $FFFFFFFF) and
    (SetFilePointer(Handle, NewSize, nil, file_Begin) <> $FFFFFFFF)
      and
    SetEndOfFile(Handle) or
    (SetFilePointer(Handle, CurPos, nil, file_Begin) <> $FFFFFFFF));
  end;

function SysFileClose(Handle: longInt): longInt;
  begin
    Result := SetResult(CloseHandle(Handle));
  end;

function SysFileFlushBuffers(Handle: longInt): longInt;
  begin
    Result := SetResult(FlushFileBuffers(Handle));
  end;

function SysFileDelete(FileName: PChar): longInt;
  begin
    Result := SetResult(DeleteFile(FileName));
  end;

function SysFileMove(OldName, NewName: PChar): longInt;
  begin
    Result := SetResult(MoveFile(OldName, NewName));
  end;

function SysFileIsDevice(Handle: longInt): longInt;
  var
    HandleType: longInt;
  begin
    HandleType := GetFileType(Handle);
    case HandleType of
      0, 1:
        Result := 0; // File;
      2:
        Result := 1; // Device
      3:
        Result := 2; // Pipe
    end {case};
  end;

function SysDirGetCurrent(Drive: longInt; Path: PChar): longInt;
  var
    DriveName: array[0..3] of Char;
    Buffer: array[0..259] of Char;
  begin
    // !! Compiler problem? Result is set by GetCurrentDirectory call!
    Result := 0;
    if Drive <> 0 then
      begin
        DriveName[0] := Chr(Drive+(Ord('A')-1));
        DriveName[1] := ':';
        DriveName[2] := #0;
        GetCurrentDirectory(SizeOf(Buffer), Buffer);
        SetCurrentDirectory(DriveName);
      end;
    GetCurrentDirectory(260, Path);
    if Drive <> 0 then
      SetCurrentDirectory(Buffer);
  end;

function SysDirSetCurrent(Path: PChar): longInt;
  begin
    if Path^ = #0 then
      Result := 0 // Otherwise returns rc = 161: Bad path name
    else
      Result := SetResult(SetCurrentDirectory(Path));
  end;

function SysDirCreate(Path: PChar): longInt;
  begin
    Result := SetResult(CreateDirectory(Path, nil));
  end;

function SysDirDelete(Path: PChar): longInt;
  begin
    Result := SetResult(RemoveDirectory(Path));
  end;

function SysMemAvail: longInt;
  var
    Status: TMemoryStatus;
  begin
    Status.dwLength := SizeOf(TMemoryStatus);
    GlobalMemoryStatus(Status);
    with Status do
      begin
        Result := dwAvailPhys+dwAvailPageFile;
        if Result > dwAvailVirtual then
          Result := dwAvailVirtual;
      end;
  end;

function PhysMemAvail: longInt; {AK155 20-08-2003}
  var
    Status: TMemoryStatus;
  begin
    Status.dwLength := SizeOf(TMemoryStatus);
    GlobalMemoryStatus(Status);
    Result := Status.dwAvailPhys;
  end;

function SysMemAlloc(Size, Flags: longInt; var MemPtr: Pointer):
    longInt;
  begin
    MemPtr := VirtualAlloc(nil, Size, Flags, page_ReadWrite);
    Result := SetResult(MemPtr <> nil);
  end;

function SysMemFree(MemPtr: Pointer): longInt;
  begin
    Result := SetResult(VirtualFree(MemPtr, 0, mem_Release));
  end;

function SysSysMsCount: longInt;
  begin
    Result := GetTickCount;
  end;

procedure SysSysSelToFlat(var P: Pointer);
  begin
    // Do nothing; n/a for Win32
  end;

procedure SysSysFlatToSel(var P: Pointer);
  begin
    // Do nothing; n/a for Win32
  end;

function SysCtrlSelfAppType: longInt;
  var
    F: file;
    lExeHdr: TImageDosHeader;
    lPEHdr: TImageNtHeaders;
    SaveMode: integer;
  begin
    // Set default return value: GUI
    Result := 3;

    // Attempt to read information from PE file header.  This only works
    // if the file has not been compressed or otherwise manipulated.
    SaveMode := FileMode;
    FileMode := $40; // Read-only, deny-none
    Assign(F, ParamStr(0));
    Reset(F, 1);
    if IOResult = 0 then
      begin
        BlockRead(F, lExeHdr, SizeOf(lExeHdr));

        if (IOResult = 0) and (lExeHdr.e_Magic = image_DOS_Signature)
        then
          begin
            Seek(F, lExeHdr.e_lfanew);
            BlockRead(F, lExeHdr.e_Magic, SizeOf(lExeHdr.e_Magic));
          end
        else
          lExeHdr.e_lfanew := 0;

        Seek(F, lExeHdr.e_lfanew);
        if (IOResult = 0) and (lExeHdr.e_Magic = image_NT_Signature)
        then
          begin
            BlockRead(F, lPEHdr, SizeOf(lPEHdr));
            if (IOResult = 0) and (lPEHdr.signature =
                image_NT_Signature)
            then
              if lPEHdr.OptionalHeader.Subsystem =
                  image_Subsystem_Windows_CUI
              then
                Result := 2; // Text mode
          end;
        Close(F);
        InOutRes := 0;
      end;
    FileMode := SaveMode;
  end { SysCtrlSelfAppType: };

function SysGetThreadId: longInt;
  begin
    Result := GetCurrentThreadId;
  end;

function SysCtrlKillThread(Handle: longInt): longInt;
  begin
    Result := SetResult(TerminateThread(Handle, 0));
  end;

function SysCtrlSuspendThread(Handle: longInt): longInt;
  begin
    Result := SetResult(SuspendThread(Handle) <> $FFFFFFFF);
  end;

function SysCtrlResumeThread(Handle: longInt): longInt;
  begin
    Result := SetResult(ResumeThread(Handle) <> $FFFFFFFF);
  end;

procedure SysCtrlExitThread(ExitCode: longInt);
  var
    P: Pointer;
  type
    TExitThread = procedure (ExitCode: longInt) stdcall;
  begin
    P := QueryProcAddr('ExitThread', True);
    if P <> nil then
      TExitThread(P)(ExitCode)
    else
      SysCtrlExitProcess(ExitCode);
  end;

procedure SysCtrlExitProcess(ExitCode: longInt);
  begin
    ExitProcess(ExitCode);
  end;

function SysCtrlCreateThread(Attrs: Pointer; StackSize: longInt;
    func, Param: Pointer; Flags: longInt; var Tid: longInt): longInt;
  begin
    Result := CreateThread(Attrs, StackSize, func, Param, Flags, Tid);
    CloseHandle(Result);
    Result := SetResult(Result <> 0);
  end;

function SysCtrlGetModuleName(Handle: longInt; Buffer: PChar):
    longInt;
  begin
    SetResult(GetModuleFileName(0, Buffer, 260) <> 0);
  end;

var
  SysCritSec: TRTLCriticalSection;
  InitCritSec: boolean;

procedure SysCtrlEnterCritSec;
  var
    P: Pointer;
  type
    TInitializeCriticalSectionAndSpinCount =
    procedure (CriticalSection: TRTLCriticalSection; SpinCount:
      DWord) stdcall;
  begin
    if not InitCritSec then
      begin
        P := QueryProcAddr('InitializeCriticalSectionAndSpinCount',
          True);
        if Assigned(P) then
          TInitializeCriticalSectionAndSpinCount(P)(SysCritSec, 4000)
        else
          InitializeCriticalSection(SysCritSec);
        InitCritSec := True;
      end;
    EnterCriticalSection(SysCritSec);
  end;

procedure SysCtrlLeaveCritSec;
  begin
    LeaveCriticalSection(SysCritSec);
  end;

function GetParamStr(P: PChar; var Param: ShortString): PChar;
  var
    len: longInt;
  begin
    Result := P;
    repeat
      while Result^ in [#1..' '] do
        Inc(Result);
      if PSmallWord(Result)^ = (Ord('"') shl 8+Ord('"')) then
        Inc(Result, 2)
      else
        break;
    until False;
    len := 0;
    while Result^ > ' ' do
      if Result^ = '"' then
        begin
          Inc(Result);
          while not (Result^ in [#0, '"']) do
            begin
              Inc(len);
              Param[len] := Result^;
              Inc(Result);
            end;
          if Result^ <> #0 then
            Inc(Result);
        end
      else
        begin
          Inc(len);
          Param[len] := Result^;
          Inc(Result);
        end;
    Param[0] := Chr(len);
  end { GetParamStr };

// Cache for SysCmdLn and SysCmdLnCount
const
  cSysCmdLn: PChar = nil;
  cSysCmdLnCount: longInt = -1;

function SysCmdln: PChar;
  var
    Buffer: array[0..260] of Char;
    P: PChar;
  begin
    if Assigned(cSysCmdLn) then
      Result := cSysCmdLn
    else
      begin
        P := GetCommandLine;
        GetMem(Result, StrLen(P)+2); // have 2 #0
        FillChar(Result^, StrLen(P)+2, 0); // blank Result
        StrCopy(Result, P);
        if IsConsole then
          CharToOemBuff(Result, Result, StrLen(Result));
        SysCtrlGetModuleName(0, Buffer);
        if StrLIComp(Buffer, Result, StrLen(Buffer)) = 0 then
          P := Result+StrLen(Buffer) // have position for #0
        else
          begin
            P := StrScan(Result, ' '); // guess position
            if not Assigned(P) then
              P := StrEnd(Result); // no parameters are given
          end;

        P[0] := #0; // place separator

        cSysCmdLn := Result;
      end;
  end { SysCmdln: };
function SysCmdlnCount: longInt;
  var
    P: PChar;
    s: ShortString;
  begin
    if cSysCmdLnCount >= 0 then
      Result := cSysCmdLnCount
    else
      begin
        P := SysCmdln;
        Result := -1;
        repeat
          P := GetParamStr(P, s);
          if s = '' then
            break;
          Inc(Result);
          if Result = 0 then// Skip the first #0
            Inc(P);
        until False;
        cSysCmdLnCount := Result;
      end;
  end { SysCmdlnCount: };

procedure SysCmdlnParam(Index: longInt; var Param: ShortString);
  var
    i: longInt;
    P: PChar;
    Buffer: array[0..260] of Char;
  begin
    i := Index;
    if i = 0 then
      begin
        SysCtrlGetModuleName(0, Buffer);
        P := Buffer;
        Param := '';
        while (P^ <> #0) and (i < 255) do
          begin
            Inc(i);
            Param[i] := P^;
            Inc(P);
          end;
        SetLength(Param, i);
      end
    else
      begin
        P := SysCmdln;
        P := GetParamStr(P, Param); // Skip program name
        Inc(P);
        Dec(i);
        repeat
          P := GetParamStr(P, Param);
          if (i = 0) or (Param = '') then
            break;
          Dec(i);
        until False;
      end;
  end { SysCmdlnParam };

function SysGetProcessId: longInt;
  begin
    Result := GetCurrentProcessID;
  end;

type
  TOpenFileMapping = function (Acc: DWord; Inherit: Bool; Name:
    PChar): THandle stdcall;

function SysCtrlGetTlsMapMem: Pointer;
  var
    IsNew: boolean;
    MapHandle: longInt;
    SharedMemName: record
      l0: longInt;
      l1: longInt;
      L2: longInt;
      Id: array[0..11] of Char;
      end;
    P: Pointer;

  begin
    SharedMemName.l0 := Ord('S')+Ord('H') shl 8+Ord('A') shl 16+Ord(
      'R') shl 24;
    SharedMemName.l1 := Ord('E')+Ord('D') shl 8+Ord('M') shl 16+Ord(
      'E') shl 24;
    {$IFDEF B243}
    SharedMemName.L2 := Ord('M')+Ord('4') shl 8+Ord('V') shl 16+Ord(
      'S') shl 24;
    {$ELSE}
    SharedMemName.L2 := Ord('M')+Ord('5') shl 8+Ord('V') shl 16+Ord(
      'S') shl 24;
    {$ENDIF}
    Str(GetCurrentProcessID, SharedMemName.Id);
    MapHandle := 0;
    IsNew := False;
    P := QueryProcAddr('OpenFileMappingA', True);
    if P = nil then
      begin
        GetMem(Result, SharedMemSize);
        IsNew := True;
      end
    else
      begin
        MapHandle := TOpenFileMapping(P)(file_map_Read+
          file_map_Write, False, PChar(@SharedMemName));
        if MapHandle = 0 then
          begin
            MapHandle := CreateFileMapping($FFFFFFFF, nil,
              page_ReadWrite, 0, SharedMemSize, PChar(@SharedMemName));
            IsNew := True;
          end;
        Result := MapViewOfFile(MapHandle, file_map_Read+
          file_map_Write, 0, 0, 0);
      end;
    if IsNew then
      begin
        FillChar(Result^, SharedMemSize, $FF);
        FillChar(Result^, SizeOf(TSharedMem), 0);
        // Set up pointers to functions to use when allocating memory
        System.GetMemoryManager(PSharedMem(Result)^.TlsMemMgr);
      end;
  end { SysCtrlGetTlsMapMem: };

function SysGetEnvironment: PChar;
  begin
    Result := GetEnvironmentStrings;
  end;

procedure SysFreeEnvironment(_Env: PChar);
  begin
    FreeEnvironmentStrings(_Env);
  end;

function SysOsVersion: longInt;
  begin
    Result := SmallWord(GetVersion);
  end;

{Cat: SysPlatformID хранится в переменной, а содержимое этой функции перенесено в раздел инициализации}
(*
function SysPlatformID: Longint;
var
  OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  Result := OSVersionInfo.dwPlatformId;
end;
*)
function SysPlatformId: longInt;
  begin
    SysPlatformId := SysPlatform;
  end;
{/Cat}

procedure SysGetDateTime(Year, Month, Day, DayOfWeek, Hour, Minute,
    Second, MSec: PLongInt);
  var
    DT: TSystemTime;
  begin
    GetLocalTime(DT);
    if Year <> nil then
      Year^:= DT.wYear;
    if Month <> nil then
      Month^:= DT.wMonth;
    if Day <> nil then
      Day^:= DT.wDay;
    if DayOfWeek <> nil then
      DayOfWeek^:= DT.wDayOfWeek;
    if Hour <> nil then
      Hour^:= DT.wHour;
    if Minute <> nil then
      Minute^:= DT.wMinute;
    if Second <> nil then
      Second^:= DT.wSecond;
    if MSec <> nil then
      MSec^:= DT.wMilliseconds;
  end { SysGetDateTime };

procedure SysSetDateTime(Year, Month, Day, Hour, Minute, Second,
    MSec: PLongInt);
  var
    DT: TSystemTime;
  begin
    GetLocalTime(DT);
    if Year <> nil then
      DT.wYear := Year^;
    if Month <> nil then
      DT.wMonth := Month^;
    if Day <> nil then
      DT.wDay := Day^;
    if Hour <> nil then
      DT.wHour := Hour^;
    if Minute <> nil then
      DT.wMinute := Minute^;
    if Second <> nil then
      DT.wSecond := Second^;
    if MSec <> nil then
      DT.wMilliseconds := MSec^;
    SetLocalTime(DT);
  end { SysSetDateTime };

function SysVerify(SetValue: boolean; Value: boolean): boolean;
  begin
    Result := False;
  end;

procedure LoadOSR2Functions;
  const
    LoadedOSR2: boolean = False;
  begin
    if LoadedOSR2 then
      exit;

    // Dynamically load functions only available in Win95 OSR2 and later
    if not Assigned(pGetDiskFreeSpaceEx) then
      @pGetDiskFreeSpaceEx := QueryProcAddr('GetDiskFreeSpaceExA',
        True);

    LoadedOSR2 := True;
  end;

function SysDiskFreeLong(Drive: byte): TQuad;
  var
    RootPath: array[0..3] of Char;
    RootPtr: PChar;
    SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters:
      DWord;
    TotalAvail: TQuad;
  begin
    RootPtr := nil;
    if Drive > 0 then
      begin
        RootPath[0] := Char(Drive+(Ord('A')-1));
        RootPath[1] := ':';
        RootPath[2] := '\';
        RootPath[3] := #0;
        RootPtr := RootPath;
      end;

    LoadOSR2Functions;

    if Assigned(pGetDiskFreeSpaceEx) then
      begin
        if not pGetDiskFreeSpaceEx(RootPtr, Result, TotalAvail, nil)
        then
          Result := -1
      end
    else if GetDiskFreeSpace(RootPtr, SectorsPerCluster,
        BytesPerSector, FreeClusters, TotalClusters)
    then
      Result := 1.0*SectorsPerCluster*BytesPerSector*FreeClusters
    else
      Result := -1;
  end { SysDiskFreeLong };

function SysDiskSizeLong(Drive: byte): TQuad;
  var
    RootPath: array[0..3] of Char;
    RootPtr: PChar;
    SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters:
      DWord;
    FreeBytes: TQuad;
  begin
    RootPtr := nil;
    if Drive > 0 then
      begin
        RootPath[0] := Char(Drive+(Ord('A')-1));
        RootPath[1] := ':';
        RootPath[2] := '\';
        RootPath[3] := #0;
        RootPtr := RootPath;
      end;
    LoadOSR2Functions;

    if Assigned(pGetDiskFreeSpaceEx) then
      pGetDiskFreeSpaceEx(RootPtr, FreeBytes, Result, nil)
    else if GetDiskFreeSpace(RootPtr, SectorsPerCluster,
        BytesPerSector, FreeClusters, TotalClusters)
    then
      Result := 1.0*SectorsPerCluster*BytesPerSector*TotalClusters
    else
      Result := -1;
  end { SysDiskSizeLong };

{Cat}
function SysDiskFreeLongX(Path: PChar): TQuad;
  var
    SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters:
      DWord;
    AvailableForCaller, Total, Free: TQuad;
  begin
    if Assigned(GetDiskFreeSpaceEx) then
      if GetDiskFreeSpaceEx(Path, AvailableForCaller, Total, Free)
      then
        Result := Free
      else
        Result := -1
    else if GetDiskFreeSpace(Path, SectorsPerCluster, BytesPerSector,
        FreeClusters, TotalClusters)
    then
      Result := 1.0*SectorsPerCluster*BytesPerSector*FreeClusters
    else
      Result := -1;
  end;

function SysDiskSizeLongX(Path: PChar): TQuad;
  var
    SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters:
      DWord;
    AvailableForCaller, Total, Free: TQuad;
  begin
    if Assigned(GetDiskFreeSpaceEx) then
      if GetDiskFreeSpaceEx(Path, AvailableForCaller, Total, Free)
      then
        Result := Total
      else
        Result := -1
    else if GetDiskFreeSpace(Path, SectorsPerCluster, BytesPerSector,
        FreeClusters, TotalClusters)
    then
      Result := 1.0*SectorsPerCluster*BytesPerSector*TotalClusters
    else
      Result := -1;
  end;
{/Cat}

function SysGetFileAttr(FileName: PChar; var Attr: longInt): longInt;
  begin
    Attr := GetFileAttributes(FileName);
    Result := SetResult(Attr <> -1);
    if Attr = -1 then
      Inc(Attr);
  end;

function SysSetFileAttr(FileName: PChar; Attr: longInt): longInt;
  begin
    Result := SetResult(SetFileAttributes(FileName, Attr));
  end;

function SysGetFileTime(Handle: longInt; var Time: longInt): longInt;
  var
    FileTime, LocalFileTime: TFileTime;
  begin
    Result := SetResult(GetFileTime(Handle, nil, nil, @FileTime) and
    FileTimeToLocalFileTime(FileTime, LocalFileTime) and
    FileTimeToDosDateTime(LocalFileTime, TDateTimeRec(Time).FDate,
      TDateTimeRec(Time).FTime));
  end;

function SysSetFileTime(Handle: longInt; Time: longInt): longInt;
  var
    LocalFileTime, FileTime: TFileTime;
  begin
    Result := SetResult(DosDateTimeToFileTime(TDateTimeRec(Time).
      FDate, TDateTimeRec(Time).FTime, LocalFileTime) and
    LocalFileTimeToFileTime(LocalFileTime, FileTime) and
    SetFileTime(Handle, nil, nil, @FileTime));
  end;

function DoFindFile(var F: TOSSearchRec; IsPChar: boolean): longInt;
  var
    LocalFileTime: TFileTime;
    ExclAttr: longInt;
    InclAttr: longInt;
  begin
    // Extract Include/Exclude attributes from F.ExcludeAttr field
    ExclAttr := not F.ExcludeAttr and (file_Attribute_Hidden or
      file_Attribute_System or $8 or file_Attribute_Directory or
      file_Attribute_Archive);
    InclAttr := (F.ExcludeAttr and $FF00) shr 8;
    // Make sure attributes are not both excluded and included
    ExclAttr := ExclAttr and not InclAttr;
    with F do
      begin
        // Reject entries where
        // - Attributes that are excluded are present.
        // - Attributes that must be present are not all there
        while (FindData.dwFileAttributes and ExclAttr <> 0) or
          (FindData.dwFileAttributes and InclAttr <> InclAttr)
        do
          if not FindNextFile(Handle, FindData) then
            begin
              Result := GetLastError;
              exit;
            end;
        FileTimeToLocalFileTime(FindData.ftLastWriteTime,
          LocalFileTime);
        FileTimeToDosDateTime(LocalFileTime, TDateTimeRec(Time).
          FDate, TDateTimeRec(Time).FTime);
        Size := FindData.nFileSizeLow;
        Attr := FindData.dwFileAttributes;
        // Convert filename to OEM character set
        if RecodeAnsiNames then{JO}
          CharToOEM(FindData.cFileName, FindData.cFileName);
        if IsPChar then
          StrCopy(PChar(@Name), FindData.cFileName)
        else
          Name := StrPas(FindData.cFileName);
      end;
    Result := 0;
  end { DoFindFile };

function SysFindFirst(Path: PChar; Attr: longInt; var F:
    TOSSearchRec; IsPChar: boolean): longInt;
  begin
    F.ExcludeAttr := Attr;
    F.Handle := FindFirstFile(Path, F.FindData);
    if F.Handle <> invalid_Handle_Value then
      begin
        Result := DoFindFile(F, IsPChar);
        if Result <> 0 then
          begin
            FindClose(F.Handle);
            F.Handle := invalid_Handle_Value;
          end;
      end
    else
      Result := GetLastError;
  end;

function SysFindNext(var F: TOSSearchRec; IsPChar: boolean): longInt;
  begin
    if FindNextFile(F.Handle, F.FindData) then
      Result := DoFindFile(F, IsPChar)
    else
      Result := GetLastError;
  end;

function SysFindClose(var F: TOSSearchRec): longInt;
  begin
    if F.Handle = invalid_Handle_Value then
      Result := 0
    else
      Result := SetResult(Windows.FindClose(F.Handle));
    F.Handle := invalid_Handle_Value;
  end;

// Check if file exists; if it does, update FileName parameter
// to include correct case of existing file
function SysFileAsOS(FileName: PChar): boolean;
  var
    Handle: THandle;
    FindData: TWin32FindData;
    LocalFileTime: TFileTime;
    P: PChar;
  begin
    Handle := FindFirstFile(FileName, FindData);
    if Handle <> invalid_Handle_Value then
      begin
        if FindData.cFileName[0] <> #0 then
          begin
            // Replace filename part with data returned by Windows
            P := StrRScan(FileName, '\');
            if P = nil then
              P := FileName
            else
              Inc(P); // Point to first character of file name
            StrCopy(P, FindData.cFileName);
          end;
        FindClose(Handle);
        Result := True;
      end
    else
      Result := False;
  end { SysFileAsOS };

function SysFileSearch(Dest, Name, List: PChar): PChar;
  var
    i, P, l: integer;
    Buffer: array[0..259] of Char;
  begin
    Result := Dest;
    StrCopy(Buffer, Name);
    P := 0;
    l := StrLen(List);
    while True do
      begin
        if SysFileAsOS(Buffer) then
          begin
            SysFileExpand(Dest, Buffer);
            exit;
          end;
        while (P < l) and (List[P] = ';') do
          Inc(P);
        if P >= l then
          break;
        i := P;
        while (P < l) and (List[P] <> ';') do
          Inc(P);
        StrLCopy(Buffer, List+i, P-i);
        if not (List[P-1] in [':', '\']) then
          StrLCat(Buffer, '\', 259);
        StrLCat(Buffer, Name, 259);
      end;
    Dest^:= #0;
  end { SysFileSearch };

function SysFileExpand(Dest, Name: PChar): PChar;
  var
    l: longInt;
    NameOnly: PChar;
  begin
    if StrLen(Name) = 0 then
      SysDirGetCurrent(0, Dest)
    else if GetFullPathName(Name, 260, Dest, NameOnly) = 0 then
      StrCopy(Dest, Name) // API failed; copy name to dest
    else if (StrComp(Name, '.') <> 0) and (StrComp(Name, '..') <> 0)
    then
      begin
        l := StrLen(Name);
        if (l > 0) and (Name[l-1] = '.') then
          begin
            l := StrLen(Dest);
            if (l > 0) and (Dest[l-1] <> '.') then
              begin
                Dest[l] := '.';
                Dest[l+1] := #0;
              end;
          end;
      end;
    Result := Dest;
  end { SysFileExpand };

threadvar
  ProcessInfo: TProcessInformation;
  LastAsync: boolean;

function SysExecute(Path, CmdLine, Env: PChar; Async: boolean; Pid:
    PLongInt; StdIn, StdOut, StdErr: longInt): longInt;
  var
    P: PChar;
    Flags: longInt;
    StartupInfo: TStartupInfo;
    CmdLineBuf: array[0..8191] of Char;
  begin
    P := CmdLineBuf;
    P^:= '"'; // Quotes to support spaces
    Inc(P);
    P := StrECopy(P, Path); // 'Path'#0
    P^:= '"';
    Inc(P);
    P^:= ' ';
    StrCopy(P+1, CmdLine); // 'Path CommandLine'#0
    FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
    with StartupInfo do
      begin
        cb := SizeOf(TStartupInfo);
        dwFlags := startf_UseShowWindow;
        wShowWindow := sw_ShowNormal;
        if StdIn = -1 then
          hStdInput := SysFileStdIn
        else
          hStdInput := StdIn;
        if StdOut = -1 then
          hStdOutput := SysFileStdOut
        else
          hStdOutput := StdOut;
        if StdErr = -1 then
          hStdError := SysFileStdErr
        else
          hStdError := StdErr;
        if (StdIn <> -1) or (StdOut <> -1) or (StdErr <> -1) then
          Inc(dwFlags, startf_UseStdHandles);
      end;
    Flags := Normal_Priority_Class;
    LastAsync := Async;
    Result := SetResult(CreateProcess(Path, CmdLineBuf, nil, nil,
      True, Flags, Env, nil, StartupInfo, ProcessInfo));
    if Result = 0 then
      if Async then
        begin
          if Pid <> nil then
            Pid^:= ProcessInfo.hProcess;
        end
      else
        begin
          hExtCmd := ProcessInfo.hProcess; {AK155: для Killer}
          WaitForSingleObject(hExtCmd, INFINITE);
        end;
  end { SysExecute };

function SysExitCode: longInt;
  begin
    if LastAsync then
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
  end;

procedure SysGetCaseMap(TblLen: longInt; Tbl: PChar);
  begin
    CharUpperBuff(Tbl, TblLen);
  end;

procedure SysGetWeightTable(TblLen: longInt; WeightTable: PChar);
  var
    i: longInt;

  begin
    // !!! Must use Win32 function
    for i := 0 to pred(TblLen) do
      begin
        WeightTable^:= Chr(i);
        Inc(WeightTable);
      end;
  end;

function SysGetCodePage: longInt;
  var
    P: Pointer;
  type
    TGetKBCodePage = function : longInt;
  begin
    P := QueryProcAddr('GetKBCodePage', False);
    if P = nil then
      Result := 0
    else
      Result := TGetKBCodePage(P);
  end;

function SysCompareStrings(s1, s2: PChar; l1, L2: longInt;
    IgnoreCase: boolean): longInt;
  begin
    if IgnoreCase then
      Result := CompareString(Locale_User_Default, norm_ignorecase, s1,
        l1, s2, L2)-2
    else
      Result := CompareString(Locale_User_Default, 0, s1, l1, s2, L2)-2;
  end;

procedure SysChangeCase(Source, Dest: PChar; len: longInt; NewCase:
    TCharCase);
  var
    i: longInt;
  begin
    if NewCase in [ccLower, ccUpper] then
      begin
        i := 0;
        while i < len do
          begin
            if NewCase = ccLower then
              if Source^ in ['A'..'Z'] then
                Dest^:= Chr(Ord(Source^)+32)
              else
                Dest^:= Source^
            else if Source^ in ['a'..'z'] then
              Dest^:= Chr(Ord(Source^)-32)
            else
              Dest^:= Source^;
            Inc(i);
            Inc(Source);
            Inc(Dest);
          end;
      end
    else
      begin
        // AnsiUpper and AnsiLower
        StrLCopy(Dest, Source, len);
        if NewCase = ccAnsiLower then
          CharLowerBuff(Dest, len)
        else
          CharUpperBuff(Dest, len);
      end;
  end { SysChangeCase };

function SysLowerCase(s: PChar): PChar;
  begin
    Result := CharLower(s);
  end;

function SysUpperCase(s: PChar): PChar;
  begin
    Result := CharUpper(s);
  end;

var
  PrevXcptProc: Pointer = Ptr(-1);

function SignalHandler(Report: PExceptionRecord;
  Registration: Pointer;
  Context: PContext;
  P: Pointer): longInt;
  stdcall;
  begin
    if (Report^.ExceptionCode = status_Control_C_Exit) and Assigned(
        CtrlBreakHandler) and CtrlBreakHandler
    then
      Result := 1
    else
      Result := 0;
    XcptProc := PrevXcptProc;
  end;

function CtrlHandler(CtrlType: DWord): Bool;
  stdcall;
  begin
    if Assigned(CtrlBreakHandler) and CtrlBreakHandler then
      Result := True
    else
      Result := False;
  end;

procedure SysCtrlSetCBreakHandler;
  begin
    if PrevXcptProc = Ptr(-1) then
      begin
        PrevXcptProc := XcptProc;
        XcptProc := @SignalHandler;
      end;
    SetConsoleCtrlHandler(@CtrlHandler, True);
  end;

function SysFileIncHandleCount(Count: longInt): longInt;
  begin
    // Return 0, indicating success.  In Win95/NT, the number of file handles
    // is limited by available physical memory only.
    Result := 0;
  end;

procedure DoSetCursorPosition;
  var
    CurPos: TCOORD;
  begin
    CurPos.X := CurXPos;
    CurPos.Y := CurYPos;
    SetConsoleCursorPosition(SysConOut, CurPos);
  end;

function CursorThreadFunc(P: Pointer): longInt;
  //AK155  LastX, LastY: Longint;
  begin
    LastX := -1;
    LastY := -1;
    repeat
      SemWaitEvent(semCursor, {300}INFINITE);
        {AK155
  С таймаутом 300 при вызове внешней консольной программы курсор
продолжал управляться этой ниткой, что неправильно и приводило к
установке курсора в начало строки, даже если вызванная программа
сделала вывод без перевода строки (например, задала вопрос и ждет
ответа в той же строке).
   А зачем нужно 300 - я понять не смог. }
      if (CurXPos <> LastX) or (CurYPos <> LastY) then
        begin
          DoSetCursorPosition;
          LastX := CurXPos;
          LastY := CurYPos;
        end;
      {Cat}
      {/Cat}
    until tidCursor = -2;
    tidCursor := -1;
  end;

procedure CursorThreadExitProc;
  begin
    // Force cursor thread to terminate
    tidCursor := -2;
    SemPostEvent(semCursor);
    // Update cursor position
    DoSetCursorPosition;

end;

{Cat}
{/Cat}

procedure InitialiseKeyboardHandler;
  begin
    {AK155,Cat: ни к чему загружать неизвестно какую VpKbdW32.dll}
    (*
  if hKbdDll = -1 then // First try - attempt load
    begin
      hKbdDll := LoadLibrary(KbdDllName);
      if hKbdDll <> 0 then
        begin
          @pKbdInit := GetProcAddress(hKbdDll, name_KbdInit);
          @pKbdUpdateEventQueues := GetProcAddress(hKbdDll, name_KbdUpdateEventQueue);
        end;
      // If any of the entry points were not found in the DLL, point them
      // to statically linked default handlers
      if not assigned(pKbdInit) or not assigned(pKbdUpdateEventQueues) then
        begin
          @pKbdInit := @VpKbdW32.KbdInit;
          @pKbdUpdateEventQueues := @VpKbdW32.KbdUpdateEventQueues;
        end;
      pKbdInit(pSysKeyCount, pSysKeyQue, pSysShiftState, pSysMouCount, pSysMouQue);

      // Check if we're in Russia, cp 866.  If we are, allow key $E0 to be
      // interpreted as a character
     {if SysGetCodePage = 866 then}
        SysSpecialKeys := [0]
     {else
        SysSpecialKeys := [0, $E0]};
    end;
*)
    pKbdInit(pSysKeyCount, pSysKeyQue, pSysShiftState, pSysMouCount,
      pSysMouQue);

    {/Cat}
  end;

procedure InitialiseConsole;
  begin
    SysConIn := SysFileStdIn;
    SysConOut := SysFileStdOut;
    InitialiseKeyboardHandler;
  end;

const
  CrtScanCode: byte = 0;

function SysKeyPressed: boolean;
  begin
    if CrtScanCode <> 0 then
      Result := True
    else
      begin
        {InitialiseKeyboardHandler;}
        pKbdUpdateEventQueues;
        Result := pSysKeyCount^ <> 0;
      end;
  end;

function SysPeekKey(var Ch: Char): boolean;
  begin
    pKbdUpdateEventQueues;
    if pSysKeyCount^ = 0 then{ Kirill: Change ">" -> "=" }
      Result := False
    else
      begin
        Result := True;
        if Lo(pSysKeyQue^[0].skeKeyCode) in [0 {,$E0}] then{JO}
          Ch := #0
        else
          Ch := Chr(Lo(pSysKeyQue^[0].skeKeyCode));
      end;
  end;

function SysReadKey: Char;
  var
    EventCount: DWord;
    InRec: TInputRecord;
  begin
    if CrtScanCode <> 0 then
      begin
        Result := Chr(CrtScanCode);
        CrtScanCode := 0;
      end
    else
      begin
        {InitialiseKeyboardHandler;}
        repeat
          pKbdUpdateEventQueues;
          if pSysKeyCount^ = 0 then
            WaitForSingleObject(SysConIn, INFINITE);
        until pSysKeyCount^ <> 0;
        Dec(pSysKeyCount^);
        if Lo(pSysKeyQue^[0].skeKeyCode) in [0 {,$E0}] then{JO}
          begin
            CrtScanCode := Hi(pSysKeyQue^[0].skeKeyCode);
            Result := #0;
          end
        else
          Result := Chr(Lo(pSysKeyQue^[0].skeKeyCode));
        Move(pSysKeyQue^[1], pSysKeyQue^[0], pSysKeyCount^*SizeOf(
          TSysKeyEvent));
      end;
  end { SysReadKey: };

procedure SysFlushKeyBuf;
  begin
    {InitialiseKeyboardHandler;}
    CrtScanCode := 0;
    pSysKeyCount^:= 0;
    FlushConsoleInputBuffer(SysConIn);
  end;

procedure InitialiseCursorThread;
  var
    sbi: TConsoleScreenBufferInfo;
  begin
    if tidCursor = -1 then
      begin
        // Get initial cursor position
        GetConsoleScreenBufferInfo(SysConOut, sbi);
        CurXPos := sbi.dwCursorPosition.X;
        CurYPos := sbi.dwCursorPosition.Y;

        semCursor := SemCreateEvent(nil, False, False);
        BeginThread(nil, 16384, CursorThreadFunc, nil, 0, tidCursor);
        SemPostEvent(semCursor);
        AddExitProc(CursorThreadExitProc);
      end;
  end;

procedure SysWrtCharStrAtt(CharStr: Pointer; len, X, Y: SmallWord;
    var Attr: byte);
  var
    Buffer: array[0..255] of TWin32Cell;
    BufferSize,
    BufferCoord: TCOORD;
    WriteRegion: TSMALLRECT;
    BufNext: ^TWin32Cell;
    i: longInt;
  begin
    InitialiseCursorThread;
    BufNext := @Buffer;
    for i := 0 to len-1 do
      begin
        BufNext^.Attr := Attr;
        BufNext^.Ch := Ord(PChar(CharStr)^);
        Inc(PChar(CharStr));
        Inc(BufNext);
      end;
    with BufferSize do
      begin
        X := len;
        Y := 1;
      end;
    with BufferCoord do
      begin
        X := 0;
        Y := 0;
      end;
    with WriteRegion do
      begin
        Left := X;
        Top := Y;
        Right := X+len-1;
        Bottom := Y;
      end;
    WriteConsoleOutput(SysConOut, @Buffer, BufferSize, BufferCoord,
      WriteRegion);
  end { SysWrtCharStrAtt };

function SysReadAttributesAt(X, Y: SmallWord): byte;
  var
    WasRead: longInt;
    Coor: TCOORD;
    Temp: SmallWord;
  begin
    SysTVInitCursor;
    Coor.X := X;
    Coor.Y := Y;
    ReadConsoleOutputAttribute(SysConOut, @Temp, 1, Coor, WasRead);
    Result := Temp;
  end;

function SysReadCharAt(X, Y: SmallWord): Char;
  var
    WasRead: longInt;
    Coor: TCOORD;
  begin
    SysTVInitCursor;
    Coor.X := X;
    Coor.Y := Y;
    ReadConsoleOutputCharacter(SysConOut, @Result, 1, Coor, WasRead);
    if WasRead = 0 then
      Result := #0;
  end;

procedure SysScrollUp(X1, Y1, X2, Y2, Lines, Cell: SmallWord);
  var
    ClipRect,
    ScrollRect: TSMALLRECT;
    DestCoord: TCOORD;
    fill: TWin32Cell;
    i: integer;
  begin
    {if SysPlatform = -1 then
    SysPlatform := SysPlatformID;}
    fill.Ch := Lo(Cell);
    fill.Attr := Hi(Cell);
    ScrollRect.Left := X1;
    ScrollRect.Top := Y1;
    ScrollRect.Right := X2;
    ScrollRect.Bottom := Y2;
    ClipRect := ScrollRect;
    if SysPlatform = VER_PLATFORM_WIN32_NT then
      begin
        DestCoord.X := X1; // This API works in Windows NT
        DestCoord.Y := Y1-Lines;
        ScrollConsoleScreenBuffer(SysConOut, ScrollRect, @ClipRect,
          DestCoord, PCharInfo(@Fill)^);
      end
    else
      begin
        if Lines > 1 then
          for i := 1 to 2 do// Half a screen at a time; bug in Win95
            begin
              DestCoord.X := X1;
              DestCoord.Y := Y1-Lines div 2;
              ScrollConsoleScreenBuffer(SysConOut, ScrollRect,
                @ClipRect, DestCoord, PCharInfo(@Fill)^);
            end;
        if odd(Lines) then// Also do last line, if odd number
          begin
            DestCoord.X := X1;
            DestCoord.Y := Y1-1;
            ScrollConsoleScreenBuffer(SysConOut, ScrollRect,
              @ClipRect, DestCoord, PCharInfo(@Fill)^);
          end;
      end;
  end { SysScrollUp };

procedure SysScrollDn(X1, Y1, X2, Y2, Lines, Cell: SmallWord);
  var
    ScrollRect: TSMALLRECT;
    DestCoord: TCOORD;
    fill: TWin32Cell;
  begin
    fill.Ch := Lo(Cell);
    fill.Attr := Hi(Cell);
    ScrollRect.Left := X1;
    ScrollRect.Top := Y1;
    ScrollRect.Right := X2;
    ScrollRect.Bottom := Y2-Lines;
    DestCoord.X := X1;
    DestCoord.Y := Y1+Lines;
    ScrollConsoleScreenBuffer(SysConOut, ScrollRect, nil, DestCoord,
      PCharInfo(@Fill)^);
  end;

procedure SysGetCurPos(var X, Y: SmallWord);
  begin
    if CurXPos = -1 then
      InitialiseCursorThread;
    X := CurXPos;
    Y := CurYPos;
  end;

function SysTVDetectMouse: longInt;
  begin
    Result := 2;
  end;

procedure SysTVInitMouse(var X, Y: integer);
  begin
    X := 0;
    Y := 0;
  end;

procedure SysTVDoneMouse(Close: boolean);
  begin
  end;

procedure SysTVShowMouse; // No control over mouse pointer in Win32
  begin
  end;

procedure SysTVHideMouse; // No control over mouse pointer in Win32
  begin
  end;

procedure SysTVUpdateMouseWhere(var X, Y: integer);
  begin
  end;

function SysTVGetMouseEvent(var Event: TSysMouseEvent): boolean;
  begin
    {InitialiseKeyboardHandler;}
    pKbdUpdateEventQueues;
    if pSysMouCount^ = 0 then
      Result := False
    else
      begin
        Dec(pSysMouCount^);
        Event := pSysMouQue^[0];
        Move(pSysMouQue^[1], pSysMouQue^[0], pSysMouCount^*SizeOf(
          TSysMouseEvent));
        Result := True;
      end;
  end;

procedure SysTVKbdInit;
  begin
    SetConsoleMode(SysConIn, ENABLE_MOUSE_INPUT);
  end;

function SysTVGetKeyEvent(var Event: TSysKeyEvent): boolean;
  begin
    {InitialiseKeyboardHandler;}
    pKbdUpdateEventQueues;
    if pSysKeyCount^ = 0 then
      Result := False
    else
      begin
        Dec(pSysKeyCount^);
        Event := pSysKeyQue^[0];
        Move(pSysKeyQue^[1], pSysKeyQue^[0], pSysKeyCount^*SizeOf(
          TSysKeyEvent));
        Result := True;
      end;
  end;

function SysTVPeekKeyEvent(var Event: TSysKeyEvent): boolean;
  begin
    {InitialiseKeyboardHandler;}
    pKbdUpdateEventQueues;
    if pSysKeyCount^ = 0 then
      Result := False
    else
      begin
        Event := pSysKeyQue^[0];
        Result := True;
      end;
  end;

function SysTVGetShiftState: byte;
  begin
    {InitialiseKeyboardHandler;}
    Result := pSysShiftState^;
  end;

procedure SysTVSetCurPos(X, Y: integer);
  var
    CurPos: TCOORD;
  begin
    if (CurXPos = X) and (CurYPos = Y) then
      exit; {KV}
    CurXPos := X;
    CurYPos := Y;
    if tidCursor = -1 then
      // Set cursor position without using cursor thread
      DoSetCursorPosition
    else
      // Record cursor position; tell cursor thread to update
      SemPostEvent(semCursor);
    WaitForSingleObjectEx(SysConOut, 16, True); {KV}
  end;

{Cat}
{/Cat}
procedure SysTVSetCurType(Y1, Y2: integer; Show: boolean);
  var
    Info: TConsoleCursorInfo;
  begin
    Info.bVisible := Show;
    if Abs(Y1-Y2) <= 1 then
      Info.dwSize := 15
    else
      Info.dwSize := 99;
    SetConsoleCursorInfo(SysConOut, Info);
  end;

procedure SysTVGetCurType(var Y1, Y2: integer; var Visible: boolean);
  var
    Info: TConsoleCursorInfo;
  begin
    GetConsoleCursorInfo(SysConOut, Info);
    Visible := Info.bVisible;
    if Info.dwSize <= 25 then
      begin
        Y1 := 6;
        Y2 := 7;
      end
    else
      begin
        Y1 := 1;
        Y2 := 7;
      end;
  end;

procedure WriteConsoleLine(X, Y: integer; len: integer);
  var
    P: PChar;
    Q: PWin32Cell;
    LineBuf: array[0..255] of TWin32Cell;
    R: TSMALLRECT;
    BufPos: TCOORD;
    LineSize: TCOORD;
  begin
    InitialiseCursorThread;
    { Prepared parameters }
    LineSize.X := SysBufInfo.dwSize.X;
    LineSize.Y := 1;
    BufPos.X := 0;
    BufPos.Y := 0;
    R.Left := X;
    R.Top := Y;
    R.Right := X+len-1;
    R.Bottom := Y;
    { Translate the buffer from DOS-OS/2 format to Win32 }
    P := SysScrBuf+((Y*SysBufInfo.dwSize.X)+X)*2;
    Q := @LineBuf;
    while len > 0 do
      begin
        Q^.Ch := Ord(P^);
        Inc(P);
        Q^.Attr := Ord(P^);
        Inc(P);
        Inc(Q);
        Dec(len);
      end;
    WriteConsoleOutput(SysConOut, @LineBuf, LineSize, BufPos, R);
  end { WriteConsoleLine };

function Min(X, Y: integer): integer;
  begin
    Result := Y;
    if X < Y then
      Result := X;
  end;

procedure SysTvShowBuf(Pos, Size: integer);
  var
    i, X, Y: integer;
  begin
    Pos := Pos div 2;
    X := Pos mod SysBufInfo.dwSize.X;
    Y := Pos div SysBufInfo.dwSize.X;
    while Size > 0 do
      begin
        i := Min(SysBufInfo.dwSize.X-X, Size div 2);
        WriteConsoleLine(X, Y, i);
        Dec(Size, i*2);
        X := 0;
        Inc(Y);
      end;
  end;

procedure SysTVClrScr;
  var
    i, BufSize: integer;
  begin
    BufSize := SysBufInfo.dwSize.X*SysBufInfo.dwSize.Y*2;
    i := 0;
    while i < BufSize do
      begin
        SysScrBuf[i] := ' ';
        Inc(i);
        SysScrBuf[i] := #7;
        Inc(i);
      end;
    SysTvShowBuf(0, BufSize);
    SysTVSetCurPos(0, 0);
  end;

function SysTvGetScrMode(_Size: PSysPoint; _Align: boolean): integer;
  var
    C: TCOORD;
    R: TSMALLRECT;
  begin
    // Get the size of the window and the screen buffer
    GetConsoleScreenBufferInfo(SysConOut, SysBufInfo);
    C.X := SysBufInfo.srWindow.Right+1;
    C.Y := SysBufInfo.srWindow.Bottom+1;
    // If the window size differs from the buffer size...
    if _Align and (integer(C) <> integer(SysBufInfo.dwSize)) then
      begin
        // Set the buffer to be identical to the window if _Align is True
        SetConsoleScreenBufferSize(SysConOut, C);
        R.Left := 0;
        R.Top := 0;
        R.Right := C.X-1;
        R.Bottom := C.Y-1;
        SetConsoleWindowInfo(SysConOut, True, R);
        GetConsoleScreenBufferInfo(SysConOut, SysBufInfo);
      end;
    case SysBufInfo.dwSize.Y of
      25:
        Result := $0003;
      43, 50:
        Result := $0103;
      else
        Result := $00FF;
    end {case};
    if _Size <> nil then
      with _Size^ do
        begin
          X := SysBufInfo.dwSize.X;
          Y := SysBufInfo.dwSize.Y;
          if _Size.Y > 234 then
            _Size.Y := 234;
        end;
  end { SysTvGetScrMode };

procedure SysTVSetScrMode(Mode: integer);
  var
    R: TSMALLRECT;
    Size: TCOORD;
  begin
    Size.X := 80;
    Size.Y := 25;
    if Mode and $0100 <> 0 then
      Size.Y := 50;
    SetConsoleScreenBufferSize(SysConOut, Size);
    R.Left := 0;
    R.Top := 0;
    R.Right := Size.X-1;
    R.Bottom := Size.Y-1;
    SetConsoleWindowInfo(SysConOut, True, R);
  end;

function SysTVGetSrcBuf: Pointer;
  const
    First: boolean = True;
    UpLeft: TCOORD = (X: 0; Y: 0);
    ReadFrom: TSMALLRECT = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  var
    Size: TSysPoint;
    Coord: TCOORD;
    Buffer: PWin32Cell;
    PDest: PStandardCell;
    PSrc: PWin32Cell;
    X, Y: longInt;
  begin
    Result := @SysScrBuf;
    if First then
      begin
        First := False;
        SysTvGetScrMode(@Size, False);
        Coord.X := Size.X;
        Coord.Y := Size.Y;
        ReadFrom.Right := Size.X;
        ReadFrom.Bottom := Size.Y;
        // Read existing content of screen into buffer
        GetMem(Buffer, (Size.X+1)*(Size.Y+1)*SizeOf(TWin32Cell));
        if not ReadConsoleOutput(SysConOut, Buffer, Coord, UpLeft,
            ReadFrom)
        then
          X := GetLastError;
        // Move the data to the screen buffer in standard format
        PSrc := Buffer;
        PDest := Result;
        for Y := 0 to Size.Y-1 do
          for X := 0 to Size.X-1 do
            begin
              PDest^.Ch := Chr(PSrc^.Ch);
              PDest^.Attr := byte(PSrc^.Attr);
              Inc(PSrc);
              Inc(PDest);
            end;
        FreeMem(Buffer);
      end;
  end { SysTVGetSrcBuf: };

{AK155 После вызова внешней программы реальное положение курсора
может не соответствовать LastX, LastX, которые занесены ниткой
курсора. Поэтому осторожнее будет запросить положение курсора у
системы и привести переменные в соответствие с реезультатом.
Прямой или косвенный вызов SysTVInitCursor должен быть после
любого вызова внешней программы.  }
procedure SysTVInitCursor;
  var
    B: boolean;
    sbi: TConsoleScreenBufferInfo;
  begin
    if GetConsoleScreenBufferInfo(SysConOut, sbi) then
      begin
        CurXPos := sbi.dwCursorPosition.X;
        LastX := CurXPos;
        CurYPos := sbi.dwCursorPosition.Y;
        LastY := CurYPos;
      end;
    (*
  if SysConIn = -1 then
    InitialiseConsole;
*)
  end;

procedure SysTvDoneCursor;
  begin
  end;

procedure SysCtrlSleep(Delay: integer);
  begin
    Sleep(Delay);
  end;

function SysGetValidDrives: longInt;
  begin
    Result := GetLogicalDrives;
  end;

procedure SysDisableHardErrors;
  begin
    SetErrorMode(sem_FailCriticalErrors);
  end;

function SysKillProcess(Process: longInt): longInt;
  begin
    Result := SetResult(TerminateProcess(Process, -1));
  end;

function SysAllocSharedMemory(var _Base: Pointer; _Name: PChar;
    _Size: longInt): longInt;
  begin
    Result := CreateFileMapping($FFFFFFFF, nil, page_ReadWrite, 0,
      SharedMemSize, PChar(@_Name));
    _Base := MapViewOfFile(Result, file_map_Read+file_map_Write, 0, 0, 0
      );
  end;

function SysAccessSharedMemory(var _Base: Pointer; _Name: PChar):
    longInt;
  var
    P: Pointer;
  begin
    Result := 0;
    P := QueryProcAddr('OpenFileMappingA', True);
    if P <> nil then
      begin
        Result := TOpenFileMapping(P)(file_map_Read+file_map_Write,
          False, PChar(@_Name));
        if Result <> 0 then
          _Base := MapViewOfFile(Result, file_map_Read+
            file_map_Write, 0, 0, 0);
      end;
  end;

procedure SysFreeSharedMemory(_Base: Pointer; _Handle: longInt);
  begin
    CloseHandle(_Handle);
    UnmapViewOfFile(_Base);
  end;

function SysAllocSharedMem(Size: longInt; var MemPtr: Pointer):
    longInt;
  begin
    Result := -1;
  end;

function SysGiveSharedMem(MemPtr: Pointer): longInt;
  begin
    Result := -1;
  end;

function SysPipeCreate(var ReadHandle, WriteHandle: longInt; Size:
    longInt): longInt;
  var
    SA: TSecurityAttributes;
  begin
    SA.nLength := SizeOf(SA);
    SA.lpSecurityDescriptor := nil;
    SA.bInheritHandle := True;
    Result := SetResult(CreatePipe(ReadHandle, WriteHandle, @SA,
      Size));
  end;

function SysPipePeek(Pipe: longInt; Buffer: Pointer; BufSize:
    longInt; var BytesRead: longInt; var IsClosing: boolean):
    longInt;
  var
    State: longInt;
  begin
    Result := SetResult(PeekNamedPipe(Pipe, Buffer, BufSize,
      @BytesRead, nil, nil));
    IsClosing := WaitForSingleObject(ProcessInfo.hProcess, 0) =
      wait_Object_0;
  end;

function SysPipeClose(Pipe: longInt): longInt;
  begin
    Result := SysFileClose(Pipe);
  end;

function SysLoadResourceString(Id: longInt; Buffer: PChar; BufSize:
    longInt): PChar;
  begin
    Buffer[0] := #0;
    LoadString(HInstance, Id, Buffer, BufSize);
    Result := Buffer;
  end;

function SysFileUNCExpand(Dest, Name: PChar): PChar;

  procedure GetUNCPath(FileName: PChar);
    type
      PNetResourceArray = ^TNetResourceArray;
      TNetResourceArray = array[0..MaxInt div SizeOf(TNetResource)-1] of
        TNetResource;
    var
      Done: boolean;
      i, Count, Size: integer;
      NetHandle: THandle;
      P, NetResources: PNetResource;
      RemoteNameInfo: array[0..1023] of byte;
      Drive: Char;
    begin
      if SysPlatform <> VER_PLATFORM_WIN32_WINDOWS then
        begin
          Size := SizeOf(RemoteNameInfo);
          if WNetGetUniversalName(FileName,
              UNIVERSAL_NAME_INFO_LEVEL,
            @RemoteNameInfo, Size) <> NO_ERROR
          then
            exit;
          StrCopy(FileName, PRemoteNameInfo(@RemoteNameInfo).
            lpUniversalName);
        end
      else
        begin
          { The following works around a bug in WNetGetUniversalName under Windows 95 }
          Drive := UpCase(FileName[1]);
          if (Drive < 'A') or (Drive > 'Z') or (StrLen(FileName) < 3) or
            (FileName[1] <> ':') or (FileName[2] <> '\')
          then
            exit;
          if WNetOpenEnum(RESOURCE_CONNECTED, RESOURCETYPE_DISK, 0,
              nil, NetHandle) <> NO_ERROR
          then
            exit;
          Count := -1;
          if WNetEnumResource(NetHandle, Count, nil, Size) =
              ERROR_MORE_DATA
          then
            begin
              GetMem(NetResources, Size);
              Done := False;
              P := NetResources;
              repeat
                if WNetEnumResource(NetHandle, Count, P, Size) <>
                    NO_ERROR
                then
                  break;
                i := 0;
                while i < Count do
                  begin
                    with P^ do
                      if (lpLocalName <> nil) and (UpCase(FileName[0]) =
                          UpCase(lpLocalName[0]))
                      then
                        begin
                          i := StrLen(lpRemoteName);
                          StrMove(@FileName[i], @FileName[2], MaxInt);
                          Move(lpRemoteName^, FileName^, i);
                          Done := True;
                          break;
                        end;
                    Inc(i);
                    Inc(P);
                  end;
              until Done;
              FreeMem(NetResources);
            end;
          WNetCloseEnum(NetHandle);
        end;
    end { GetUNCPath };

  begin { SysFileUNCExpand }
    {if SysPlatform = -1 then
    SysPlatform := SysPlatformID;}
    SysFileExpand(Dest, Name);
    if (UpCase(Dest[0]) in ['A'..'Z']) and (Dest[1] = ':') and (Dest[2]
        = '\')
    then
      GetUNCPath(Dest);
    Result := Dest;
  end { SysFileUNCExpand };

function SysGetSystemError(Code: longInt; Buffer: PChar; BufSize:
    longInt; var MsgLen: longInt): PChar;
  begin
    MsgLen := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, Code, 0, Buffer, BufSize,
      nil);
    Result := Buffer;
  end;

function GetLocaleStr(Locale, LocaleType: integer; Default, Dest:
    PChar): PChar;
  begin
    if GetLocaleInfo(Locale, LocaleType, Dest, 260) <= 0 then
      StrCopy(Dest, Default);
    Result := Dest;
  end;

function GetLocaleChar(Locale, LocaleType: integer; Default: Char):
    Char;
  var
    Buffer: array[0..1] of Char;
  begin
    if GetLocaleInfo(Locale, LocaleType, @Buffer, 2) > 0 then
      Result := Buffer[0]
    else
      Result := Default;
  end;

function SysStrToIntDef(s: PChar; Default: integer): integer;
  var
    E: integer;
  begin
    Val(s, Result, E);
    if E <> 0 then
      Result := Default;
  end;

procedure SysGetCurrencyFormat(CString: PChar; var CFormat,
    CNegFormat, CDecimals: byte; var CThousandSep, CDecimalSep: Char);
  var
    DefaultLCID: LCID;
    Buffer: array[0..259] of Char;
  begin
    DefaultLCID := GetThreadLocale;
    GetLocaleStr(DefaultLCID, LOCALE_SCURRENCY, '', CString);
    CFormat := SysStrToIntDef(GetLocaleStr(DefaultLCID,
      LOCALE_ICURRENCY, '0', Buffer), 0);
    CNegFormat := SysStrToIntDef(GetLocaleStr(DefaultLCID,
      LOCALE_INEGCURR, '0', Buffer), 0);
    CThousandSep := GetLocaleChar(DefaultLCID, LOCALE_STHOUSAND, ',');
    CDecimalSep := GetLocaleChar(DefaultLCID, LOCALE_SDECIMAL, '.');
    CDecimals := SysStrToIntDef(GetLocaleStr(DefaultLCID,
      LOCALE_ICURRDIGITS, '0', Buffer), 0);
  end;

procedure SysGetDateFormat(var DateSeparator: Char; ShortDateFormat,
    LongDateFormat: PChar);
  var
    DefaultLCID: LCID;
  begin
    DefaultLCID := GetThreadLocale;
    DateSeparator := GetLocaleChar(DefaultLCID, LOCALE_SDATE, '/');
    GetLocaleStr(DefaultLCID, LOCALE_SSHORTDATE, 'm/d/yy',
      ShortDateFormat);
    GetLocaleStr(DefaultLCID, LOCALE_SLONGDATE, 'mmmm d, yyyy',
      LongDateFormat);
  end;

procedure SysGetTimeFormat(var TimeSeparator: Char; TimeAMString,
    TimePMString, ShortTimeFormat, LongTimeFormat: PChar);
  var
    TimePostfix: PChar;
    DefaultLCID: LCID;
    Buffer: array[0..259] of Char;
  begin
    DefaultLCID := GetThreadLocale;
    TimeSeparator := GetLocaleChar(DefaultLCID, LOCALE_STIME, ':');
    GetLocaleStr(DefaultLCID, LOCALE_S1159, 'am', TimeAMString);
    GetLocaleStr(DefaultLCID, LOCALE_S2359, 'pm', TimePMString);
    TimePostfix := '';
    if SysStrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITLZERO, '0',
        Buffer), 0) = 0
    then
      begin
        StrCopy(ShortTimeFormat, 'h:mm');
        StrCopy(LongTimeFormat, 'h:mm:ss');
      end
    else
      begin
        StrCopy(ShortTimeFormat, 'hh:mm');
        StrCopy(LongTimeFormat, 'hh:mm:ss');
      end;
    if SysStrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIME, '0',
        Buffer), 0) = 0
    then
      TimePostfix := ' AMPM';
    StrCat(ShortTimeFormat, TimePostfix);
    StrCat(LongTimeFormat, TimePostfix);
  end { SysGetTimeFormat };

function SysGetModuleName(var Address: Pointer; Buffer: PChar;
    BufSize: longInt): PChar;
  var
    meminfo: TMemoryBasicInformation;
    ModName: array[0..MAX_PATH] of Char;
  begin
    VirtualQuery(Address, meminfo, SizeOf(meminfo));
    if (meminfo.State <> mem_Commit) or
      (GetModuleFileName(THandle(meminfo.AllocationBase), ModName,
        SizeOf(ModName)) = 0)
    then
      begin
        GetModuleFileName(HInstance, ModName, SizeOf(ModName));
        if Assigned(Address) then
          Dec(PChar(Address), $1000);
      end
    else
      Dec(PChar(Address), longInt(meminfo.AllocationBase));
    StrLCopy(Buffer, StrRScan(ModName, '\')+1, BufSize-1);
    Result := Buffer;
  end { SysGetModuleName };

procedure SysDisplayConsoleError(PopupErrors: boolean; Title, Msg:
    PChar);
  var
    Count: longInt;
  begin
    SysFileWrite(SysFileStdOut, Msg^, StrLen(Msg), Count);
  end;

procedure SysDisplayGUIError(Title, Msg: PChar);
  begin
    MessageBox(0, Msg, Title, mb_Ok or MB_IconStop or MB_TaskModal);
  end;

procedure SysBeep;
  begin
    MessageBeep(0);
  end;

procedure SysBeepEx(Freq, Dur: longInt);
  begin
    Windows.Beep(Freq, Dur);
  end;

function SysGetVolumeLabel(Drive: Char): ShortString;
  const
    Root: array[0..4] of Char = 'C:\'#0;
  var
    VolLabel: array[0..256] of Char;
    MaxLength: longInt;
    FSFlags: longInt;
  begin
    Root[0] := Drive;
    if GetVolumeInformation(Root, VolLabel, SizeOf(VolLabel),
      nil, MaxLength, FSFlags, nil, 0)
    then
      Result := StrPas(VolLabel)
    else
      Result := '';
  end;

function SysSetVolumeLabel(Drive: Char; _Label: ShortString):
    boolean;
  const
    Root: array[0..4] of Char = 'C:\'#0;
  begin
    Root[0] := Drive;
    _Label[Length(_Label)+1] := #0;
    Result := SetVolumeLabel(Root, PChar(@_Label[1]));
  end;

function SysGetForegroundProcessId: longInt;
  var
    WHandle: longInt;
    ThreadID: longInt;
  begin
    WHandle := GetForegroundWindow;
    Result := GetWindowThreadProcessId(WHandle, @ThreadID);
  end;

function SysGetBootDrive: Char;
  begin
    Result := 'C';
  end;

function SysGetDriveType(Drive: Char): TDriveType;
  const
    Root: array[0..4] of Char = 'C:\'#0;
  var
    FSName: array[0..255] of Char;
    MaxLength: longInt;
    FSFlags: longInt;
  begin
    Root[0] := Drive;
    Result := dtInvalid;
    {JO: сначала проверяем тип устройства через GetDriveType, и только после }
    {    этого, если оказалось что оно не Removable, есть смысл проверять ФС }
    case GetDriveType(Root) of
      Drive_Fixed:
        Result := dtHDFAT;
      Drive_Removable:
        Result := dtFloppy;
      Drive_CDRom:
        Result := dtCDRom;
      Drive_Remote:
        Result := dtLAN;
      0, 1:
        Result := dtInvalid;
      else
        Result := dtUnknown;
    end {case};
    if (Result <> dtFloppy) and (Result <> dtCDRom) then
      if GetVolumeInformation(Root, nil, 0, nil, MaxLength, FSFlags,
          FSName, SizeOf(FSName))
      then
        begin
          if StrLComp(FSName, 'FAT', 3) = 0 then
            Result := dtHDFAT
          else if StrComp(FSName, 'HPFS') = 0 then
            Result := dtHDHPFS
          else if StrComp(FSName, 'NTFS') = 0 then
            Result := dtHDNTFS
          else if StrLComp(FSName, 'CD', 2) = 0 then
            Result := dtCDRom
          else if StrComp(FSName, 'LAN') = 0 then
            Result := dtLAN
          else if StrComp(FSName, 'NOVELL') = 0 then
            Result := dtNovellNet;
        end;

    if Result = dtHDNTFS then
        // Fix network drive detection in Win2k/XP
      if GetDriveType(Root) = Drive_Remote then
        Result := dtLAN;
  end { SysGetDriveType };

function SysGetVideoModeInfo(var Cols, Rows, Colours: word): boolean;
  var
    Buffer: TConsoleScreenBufferInfo;
  begin
    SysTVInitCursor;
    GetConsoleScreenBufferInfo(SysConOut, Buffer);

    Cols := Buffer.dwSize.X;
    Rows := Buffer.dwSize.Y;
    Colours := 16; //Buffer.wAttributes;
  end;

function SysGetVisibleLines(var Top, Bottom: longInt): boolean;
  var
    Buffer: TConsoleScreenBufferInfo;
  begin
    SysTVInitCursor;
    GetConsoleScreenBufferInfo(SysConOut, Buffer);
    Top := Buffer.srWindow.Top+1;
    Bottom := Buffer.srWindow.Bottom+1;
    Result := True;
  end;

function SysSetVideoMode(Cols, Rows: word): boolean;
  var
    Size: TCOORD;
    R: TSMALLRECT;
    res1: boolean;
    SrcSize: TSysPoint;
  begin
    SysTvGetScrMode(@SrcSize, True);
    SysTVInitCursor;
    Size.X := Cols;
    Size.Y := Rows;
    res1 := SetConsoleScreenBufferSize(SysConOut, Size);
    {Этот вызов может быть неудачным, если текущий экран не
     помещается в новый буфер (при уменьшении размера экрана).
     В этом случае надо будет еще раз сделать установку размера
     буфера после установки размера экрана.}
    R.Left := 0;
    R.Top := 0;
    R.Right := Size.X-1;
    R.Bottom := Size.Y-1;
    Result := SetConsoleWindowInfo(SysConOut, True, R);
    if Result and not res1 then
      begin
        Result := SetConsoleScreenBufferSize(SysConOut, Size);
        if not Result then
          begin
            R.Left := 0;
            R.Top := 0;
            R.Right := SrcSize.X-1;
            R.Bottom := SrcSize.Y-1;
            SetConsoleWindowInfo(SysConOut, True, R);
          end;
      end;
  end { SysSetVideoMode };

function SemCreateEvent(_Name: PChar; _Shared, _State: boolean):
    TSemHandle;
  var
    Security: TSecurityAttributes;
  begin
    if _Shared then
      begin
        with Security do
          begin
            nLength := SizeOf(Security);
            lpSecurityDescriptor := nil;
            bInheritHandle := True;
          end;
        Result := CreateEvent(@Security, False, _State, _Name);
      end
    else
      Result := CreateEvent(nil, False, _State, _Name);
  end;

function SemAccessEvent(_Name: PChar): TSemHandle;
  begin
    Result := OpenEvent(Event_all_access, False, _Name);
    if Result = 0 then
      Result := -1;
  end;

function SemResetEvent(_Handle: TSemHandle; var _PostCount: longInt):
    boolean;
  begin
    Result := ResetEvent(_Handle);
    // Win32 does not tell us how often the event was posted:
    if Result then
      _PostCount := 1
    else
      _PostCount := 0;
  end;

function SemPostEvent(_Handle: TSemHandle): boolean;
  begin
    Result := SetEvent(_Handle);
  end;

function SemWaitEvent(_Handle: TSemHandle; _TimeOut: longInt):
    boolean;
  begin
    Result := WaitForSingleObject(_Handle, _TimeOut) = wait_Object_0;
  end;

function SemCreateMutex(_Name: PChar; _Shared, _State: boolean):
    TSemHandle;
  var
    Security: TSecurityAttributes;
  begin
    if _Shared then
      begin
        with Security do
          begin
            nLength := SizeOf(Security);
            lpSecurityDescriptor := nil;
            bInheritHandle := True;
          end;
        Result := CreateMutex(@Security, _State, _Name);
      end
    else
      // Non-shared mutex does not require security descriptor
      Result := CreateMutex(nil, _State, _Name);
  end;

function SemRequestMutex(_Handle: TSemHandle; _TimeOut: longInt):
    boolean;
  begin
    Result := WaitForSingleObject(_Handle, _TimeOut) = wait_Object_0;
    if Result = False then
      _Handle := GetLastError;
  end;

function SemAccessMutex(_Name: PChar): TSemHandle;
  begin
    Result := OpenMutex(mutex_all_access, False, _Name);
    if Result = 0 then
      Result := -1;
  end;

function SemReleaseMutex(_Handle: TSemHandle): boolean;
  begin
    Result := ReleaseMutex(_Handle);
    if Result = False then
      _Handle := GetLastError;
  end;

procedure SemCloseEvent(_Handle: TSemHandle);
  begin
    CloseHandle(_Handle);
  end;

procedure SemCloseMutex(_Handle: TSemHandle);
  begin
    CloseHandle(_Handle);
  end;

function SysMemInfo(_Base: Pointer; _Size: longInt; var _Flags:
    longInt): boolean;
  var
    Buffer: TMemoryBasicInformation;
  begin
    Result := VirtualQuery(_Base, Buffer, SizeOf(Buffer)) = SizeOf(
      Buffer);
    if Result then
      with Buffer do
        begin
          _Flags := 0;
          if Protect and (Page_ReadOnly or page_ReadWrite or
              Page_Execute_Read) <> 0
          then
            _Flags := _Flags or sysmem_Read or sysmem_Execute;
          if Protect and (Page_WriteCopy or page_ReadWrite) <> 0
          then
            _Flags := _Flags or sysmem_Write;
          if Protect and (Page_Execute or Page_Execute_Read or
              page_Execute_ReadWrite) <> 0
          then
            _Flags := _Flags or sysmem_Execute;
          if Protect and Page_Guard <> 0 then
            _Flags := _Flags or sysmem_Guard;
        end;
  end { SysMemInfo };

function SysSetMemProtection(_Base: Pointer; _Size: longInt; _Flags:
    longInt): boolean;
  var
    Flags: longInt;
    Buffer: TMemoryBasicInformation;
  begin
    VirtualQuery(_Base, Buffer, SizeOf(Buffer));
    if _Flags and sysmem_Execute <> 0 then
      if _Flags and sysmem_Read <> 0 then
        if _Flags and sysmem_Write <> 0 then
          Flags := page_Execute_ReadWrite
        else
          Flags := Page_Execute_Read
      else if _Flags and sysmem_Write <> 0 then
        Flags := page_Execute_WriteCopy
      else
        Flags := Page_Execute
    else if _Flags and sysmem_Read <> 0 then
      if _Flags and sysmem_Write <> 0 then
        Flags := page_ReadWrite
      else
        Flags := Page_ReadOnly
    else if _Flags and sysmem_Write <> 0 then
      Flags := Page_WriteCopy
    else
      Flags := page_NoAccess;
    Result := VirtualProtect(_Base, _Size, Flags, @Buffer);
  end { SysSetMemProtection };

procedure SysMessageBox(_Msg, _Title: PChar; _Error: boolean);
  var
    Flag: longInt;
  begin
    if _Error then
      Flag := mb_IconError
    else
      Flag := mb_IconInformation;
    MessageBox(0, _Msg, _Title, Flag or mb_ApplModal);
  end;

var
  ClipFormat: longInt;

function SysClipCanPaste: boolean;
  var
    IsClipboardFormatAvailable: function (Format: UInt): Bool
      stdcall;
  begin
    @IsClipboardFormatAvailable := QueryProcAddr(
      'IsClipboardFormatAvailable', False);
    if Assigned(IsClipboardFormatAvailable) then
      Result := IsClipboardFormatAvailable(ClipFormat)
    else
      Result := False;
  end;

function SysClipCopy(P: PChar; Size: longInt): boolean;
  var
    Q: PChar; {Под NT - LPWSTR}
    MSize: longInt;
    MemHandle: HGlobal;
    OpenClipBoard: function (wnd: HWND): Bool stdcall;
    EmptyClipboard: function : Bool stdcall;
    CloseClipboard: function : Bool stdcall;
    SetClipboardData: function (Format: UInt; Mem: THandle): THandle
      stdcall;
  begin
    Result := False;
    @OpenClipboard := QueryProcAddr('OpenClipboard', False);
    @EmptyClipboard := QueryProcAddr('EmptyClipboard', False);
    @CloseClipboard := QueryProcAddr('CloseClipboard', False);
    @SetClipboardData := QueryProcAddr('SetClipboardData', False);
    if Assigned(OpenClipBoard) and Assigned(EmptyClipboard) and
      Assigned(CloseClipboard) and Assigned(SetClipboardData)
    then
      begin
        // Open clipboard
        if OpenClipBoard(0) then
          begin
            EmptyClipboard;
            // Allocate a shared block of memory
            MSize := Size+1;
            if SysPlatform <> 1 {Win 9x} then
              MSize := 2*MSize; {for unicode string}
            MemHandle := GlobalAlloc(gmem_Moveable or gmem_DDEShare,
              MSize);
            Q := GlobalLock(MemHandle);
            Result := Q <> nil;
            if Result then
              begin// Copy clipboard data across
                if SysPlatform = 1 {Win 9x} then
                  Move(P^, Q^, MSize)
                else
                  begin
                      // Copy clipboard data across and translate to unicode
                    MSize := MultiByteToWideChar(CP_OEMCP, 0,
                    P, -1, LPWSTR(Q), Size+1);
                    Result := MSize <> 0;
                  end;
              end;
            GlobalUnlock(MemHandle);
            // Insert data into clipboard
            if Result then
              Result := SetClipboardData(ClipFormat, MemHandle) <> 0;
            // Do not free memory: Windows does this!
            // GlobalFree(MemHandle);

            {AK155 14/05/2002
Без нижеследующего заклинания русские буквы превращаются в вопросики
при вставке в Delphi или MS VC или в написанной на них программе.
Интересно, какая локаль им мерещится вместо LOCALE_SYSTEM_DEFAULT?
}
            if Result and (SysPlatform <> 1) then
              begin
                MemHandle := GlobalAlloc(gmem_Moveable or
                  gmem_DDEShare, 8);
                Q := GlobalLock(MemHandle);
                Result := Q <> nil;
                if Result then
                  begin
                    PDWORD(Q)^:= LOCALE_SYSTEM_DEFAULT;
                    GlobalUnlock(MemHandle);
                    Result := SetClipboardData(CF_LOCALE, MemHandle) <>
                      0;
                  end;
              end;
          end;
        CloseClipboard;
      end;
  end { SysClipCopy };

function SysClipPaste(var Size: integer): Pointer;
  var
    P: Pointer;
    ActualClipFormat: UInt;
    MemHandle: HGlobal;
    OpenClipBoard: function (wnd: HWND): Bool stdcall;
    CloseClipboard: function : Bool stdcall;
    GetClipboardData: function (Format: UInt): THandle stdcall;
  begin
    Result := nil;
    @OpenClipboard := QueryProcAddr('OpenClipboard', False);
    @CloseClipboard := QueryProcAddr('CloseClipboard', False);
    @GetClipboardData := QueryProcAddr('GetClipboardData', False);
    if Assigned(OpenClipBoard) and Assigned(CloseClipboard)
      and Assigned(GetClipboardData)
    then
      begin
        if OpenClipBoard(0) then
          begin
            {AK155 14/05/2002
  Оказалось, что некоторые символы (например, номер) криво перекодируются
из ANSI хоть в OEM, хоть в юникод, если в буфер данные клал Delphi или
MS VC или написанная на них программа. Но, к счастью они кладут в буфер
первым пакетом прямо текст ANSI, так что если, как положено, использовать
первый же текстовый формат, то получается нормально. Цикл перебора
форматов нужен для пропуска CF_LOCALE.
}
            ActualClipFormat := 0;
            while True do
              begin
                ActualClipFormat := EnumClipboardFormats(
                  ActualClipFormat);
                case ActualClipFormat of
                  CF_UNICODETEXT:
                    break;
                  CF_OEMTEXT:
                    break;
                  cf_Text:
                    break;
                end {case};
              end;
            MemHandle := GetClipboardData(ActualClipFormat);
            P := GlobalLock(MemHandle);
            if Assigned(P) then
              begin
                case ActualClipFormat of
                  CF_UNICODETEXT:
                    begin
                      Size := WideCharToMultiByte(
                      CP_OEMCP, 0, P, -1, nil, 0, nil, nil);
                      GetMem(Result, Size);
                      WideCharToMultiByte(
                      CP_OEMCP, 0, P, -1, Result, Size, nil, nil);
                    end;
                  CF_OEMTEXT:
                    begin
                      Size := StrLen(P)+1;
                      GetMem(Result, Size);
                      Move(P^, Result^, Size);
                    end;
                  cf_Text:
                    begin
                      Size := StrLen(P)+1;
                      GetMem(Result, Size);
                      CharToOEM(P, Result);
                    end;
                end {case};

              end;
            GlobalUnlock(MemHandle);
            CloseClipboard;
          end;
      end;
  end { SysClipPaste };

// Pharlap's TNT Embedded System support

function _malloc(Size: longInt): Pointer;
  cdecl;
  orgname;
  begin
    GetMem(Result, Size);
  end;

procedure _free(P: Pointer);
  cdecl;
  orgname;
  begin
    FreeMem(P);
  end;

// Retrieve various system settings, bitmapped:
// 0: Enhanced keyboard installed

function SysGetSystemSettings: longInt;
  var
    KbdFlag: longInt;
  begin
    Result := 0;
    KbdFlag := GetKeyboardType(0);
    if KbdFlag in [2, 4] then
      Result := Result or 1;
  end;

procedure SysLowInit;
  var
    OSVersionInfo: TOSVersionInfo;
  begin
    {Cat}
    OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
    GetVersionEx(OSVersionInfo);
    SysPlatform := OSVersionInfo.dwPlatformId;

    if ((OSVersionInfo.dwPlatformId = 1 {Win9x}) and (OSVersionInfo.
        dwBuildNumber >= 1000)) {Cat:warn}
      or (OSVersionInfo.dwPlatformId = 2 {WinNT})
    then
      @GetDiskFreeSpaceEx := QueryProcAddr('GetDiskFreeSpaceExA',
        True)
    else
      @GetDiskFreeSpaceEx := nil;

    {WriteLn('Platform Id = ',OSVersionInfo.dwPlatformId,',  Build Number = ',OSVersionInfo.dwBuildNumber,',  GetDiskFreeSpaceExA loaded = ',Assigned(GetDiskFreeSpaceEx));}
    {/Cat}

    InitialiseConsole; {Cat}
    SysPlatform := SysPlatformId;
      {AK155 делаем это безусловно и в одном месте}
    if SysPlatform = 1 {Win 9x} then
      ClipFormat := CF_OEMTEXT
    else
      ClipFormat := CF_UNICODETEXT;
    if IsConsole then
      SetFileApisToOEM
    else
      SetFileApisToANSI;
  end { SysLowInit };
{$IFDEF B243}
  {JO: in VP 2.1 build 274 SysLowInit called in System unit}
begin
  SysLowInit;
  {$ENDIF}

