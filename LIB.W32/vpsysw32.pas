//���������������������������������������������������������
//�                                                       �
//�      Virtual Pascal Runtime Library.  Version 2.1.    �
//�      System interface layer Win32                     �
//�      ��������������������������������������������������
//�      Copyright (C) 1995-2000 vpascal.com              �
//�                                                       �
//���������������������������������������������������������
{$DEFINE RouteConsoleToStdInOut}  // !!.kg
{.$DEFINE AutoKbdUpdateEventQueues}  // Cat:��ᯥਬ��⠫��
{.$DEFINE LazyCurTypeUpdate}  // Cat:��ᯥਬ��⠫��
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}
{AK155 = Alexey Korop, 2:261/155@fidonet}

{Cat
   18-10-2001 - ������� �㭪樨 ࠡ��� � Clipboard-��, ⥯���
   ����� ����⥫� ������ cf_Text � ��४���஢�� Ansi-Oem
   �ᯮ������ ����⥫� cf_OemText

   19-10-2001 - ⥯��� �ᯮ������ ᮡ�⢥��� RecodeCyrillicNames,
   ����� ������ ���� �ந��樠����஢�� � ���㫥 DnIni

   09-12-2001 - �����⥫쭮 ����⨫ ����㧪� VPKBDW32.DLL;
   �ࠫ �맮�� InitialiseKeyboardHandler �� ��� ����, �஬�
   InitialiseConsole, �맮� ���ன �ந�室�� �� ���樠����樨
   �����

   10-12-2001 - ࠭�� ��⠭���� ����樨 ����� �ந�室��� �
   �⤥�쭮� ��⪥, ⥯��� � ��� �� ���� ������� ��� � ���������
   ���� ����� (SetCurType, GetCurType); ���� ⠪��: ��᫥ �����
   �� ��������� ��� 0.1 ᥪ㭤� ��। ⥬, ��� �� ᠬ�� ����
   �������� ��� �����, � �᫨ �� �� �६� �ந������ ��� ����
   ����� �� ��������� - ���� ����� ������㥬
}
{AK155
   10-01-2002 - ������� �㭪樨 ࠡ��� � Clipboard-��, ⥯���
   ��� WinNT ����� ����⥫� cf_OemText �ᯮ������ cf_UnicodeText
   � �믮������� ᮮ⢥�����騥 ��४���஢��. ��� �⮣� �� 㤠����
   ������� �����६���� ���४��� ࠡ��� � ���᪨�� �㪢��� �
   � �ᥢ����䨪��, ��⮬, �⮡� ���⮥ � ���� � DN ��ଠ�쭮
   ��⠢�﫮�� � � ���᮫�, � � GUI.
}
{Cat
   20-03-2002 - ⥯��� ��� ����祭�� ࠧ��� ��᪠ � ࠧ���
   ᢮������� ���� �� ��᪥ ⠬, ��� �� ��������, �ᯮ������
   �㭪�� GetDiskFreeSpaceExA. �� �㦭� ��� ���࠭���� �஡����
   � ���ࠢ���� ������� ��� ��ࠬ��஢ ��� Win9x (࠭�� �᫨
   ࠧ��� ��᪠ ��� ࠧ��� ᢮������� ���� �� ��᪥ �ॢ�蠫 2�,
   � �� �����뢠��� ࠢ�� 2�)

   13-11-2002 - ⥯��� ����� � �㭪�ﬨ
      function SysDiskFreeLong(Drive: Byte): TQuad;
      function SysDiskSizeLong(Drive: Byte): TQuad;
   �������� �� �������, ࠡ���騥 �� � �㪢�� ��᪠, � � ����� ����:
      function SysDiskFreeLongX(Path: PChar): TQuad;
      function SysDiskSizeLongX(Path: PChar): TQuad;
   �� �㦭� ��� ����祭�� ࠧ��஢ ��� �⥢�� ��⥩, � ⠪�� � �����,
   ����� ���� ��� �������஢�� � �����-� ��⠫�� ��㣮��
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
  pSysKeyCount   : pLongint     = nil;
  pSysMouCount   : pLongint     = nil;
  pSysKeyQue     : pSysKeyQueue = nil;
  pSysMouQue     : pSysMouQueue = nil;
  pSysShiftState : pByte        = nil;

  SysConIn:    Longint = -1;
  SysConOut:   Longint = -1;
  tidCursor:   Longint = -1;  // Thread ID of cursor thread
  semCursor:   Longint = -1;  // Event semaphore, set when cursor pos changes
  CurXPos:     Longint = -1;  // Internally maintained cursor position
  CurYPos:     Longint = -1;
  LastX:       Longint = -1;
  LastY:       Longint = -1;

 {SysSpecialKeys: Set of Byte = [0, $e0];}
  SysSpecialKeys: Set of Byte = [0]; {JO}

{$IFDEF LazyCurTypeUpdate}
  CurTypeY1:   Integer = -1; {Cat}
  CurTypeY2:   Integer = -1; {Cat}
  CurTypeShow: Boolean = False; {Cat}
{$ENDIF}
{$IFDEF AutoKbdUpdateEventQueues}
  tidKbdUpdateEventQueues: Longint = -1; {Cat}
{$ENDIF}

  SysPlatform: Longint = -1; {Cat: ⥯��� �� ���祭�� �࠭���� � ��६�����, ����� ⮣�, �⮡� ����� ࠧ
                                  ��뢠�� �㭪��; ᠬ� �㭪�� ��뢠���� ⮫쪮 ���� ࠧ �� ���樠����樨}

type
  PStandardCell = ^TStandardCell;
  TStandardCell = packed record
    Ch   : Char;
    Attr : Byte;
  end;

  TKbdInit = procedure(var _pSysKeyCount, _pSysKeyQue, _pSysShiftState, _pSysMouCount, _pSysMouQue);
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
  pKbdInit : TKbdInit = VpKbdW32.KbdInit; {Cat}
  pKbdUpdateEventQueues : TKbdUpdateEventQueues = VpKbdW32.KbdUpdateEventQueues; {Cat}

  GetDiskFreeSpaceEx: function(Drive: PChar; var AvailableForCaller, Total, Free): LongBool stdcall = nil; {Cat}

function QueryProcAddr(Name: PChar; IsKernel: Boolean): Pointer;
const
  Names: array[Boolean] of PChar = ('user32.dll', 'kernel32.dll');
  Handles: array[Boolean] of THandle = (0, 0);
var
  K: Boolean;
begin
  K := IsKernel;
  if Handles[K] = 0 then
    Handles[K] := LoadLibrary(Names[K]);
  Result := GetProcAddress(Handles[K], Name);
end;

const
  AccessMode: array[0..2] of Integer = (
    generic_Read, generic_Write, generic_Read or generic_Write);
  ShareMode: array[0..4] of Integer = (
    0, 0, file_share_Read, file_share_Write, file_share_Read or file_share_Write);

function SetResult(Success: Boolean): Longint;
begin
  Result := 0;
  if not Success then
    Result := GetLastError;
end;

function SysFileStdIn: Longint;
begin
  Result := GetStdHandle(std_Input_Handle);
end;

function SysFileStdOut: Longint;
begin
  Result := GetStdHandle(std_Output_Handle);
end;

function SysFileStdErr: Longint;
begin
  Result := GetStdHandle(std_Error_Handle);
end;

threadvar
  SysAnsiFn: Array[0..260] of char;
  SysOemFn: Array[0..260] of char;

function AnsiFn(_FileName: PChar): PChar;
begin
  if RecodeAnsiNames then  {JO}
    begin // Convert filename to ANSI character set
      OemToChar(_FileName, SysAnsiFn);
      Result := SysAnsiFn;
    end
   else
      Result := _FileName;
end;

function OemFn(_FileName: PChar): PChar;
begin
  if RecodeAnsiNames then  {JO}
    begin // Convert filename to OEM character set
      CharToOem(_FileName, SysOemFn);
      Result := SysOemFn;
    end
   else
      Result := _FileName;
end;

function SysFileOpen_Create(Open: Boolean;FileName: PChar; Mode,Attr,Action: Longint; var Handle: Longint): Longint;
var
  SA: TSecurityAttributes;
  APIFlags: Longint;
begin
  if Open then
    if Action and open_CreateIfNew <> 0 then
      APIFlags := open_Always       // Open or create
    else if Action and open_TruncateIfExists <> 0 then
      APIFlags := truncate_existing // Open and truncate
    else
      APIFlags := open_Existing     // Open; fail if no file
  else
    if Action and create_TruncateIfExists <> 0 then
      APIFlags := create_Always     // Create and truncate
    else
      APIFlags := create_New;       // Create; fail if exists

  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := nil;
  SA.bInheritHandle := True;
  Handle := CreateFile(AnsiFn(FileName), AccessMode[Mode and 3], ShareMode[(Mode and $F0) shr 4],
      @SA, APIFlags, file_attribute_Normal, 0);
  Result := SetResult(Handle <> invalid_Handle_Value);
end;

function SysFileOpen(FileName: PChar; Mode: Longint; var Handle: Longint): Longint;
var
  SA: TSecurityAttributes;
begin
  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := nil;
  SA.bInheritHandle := True;
  Handle := CreateFile(AnsiFn(FileName), AccessMode[Mode and 3], ShareMode[(Mode and $F0) shr 4],
    @SA, open_Existing, file_attribute_Normal, 0);
  Result := SetResult(Handle <> invalid_Handle_Value);
end;

function SysFileCreate(FileName: PChar; Mode,Attr: Longint; var Handle: Longint): Longint;
var
  SA: TSecurityAttributes;
begin
  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := nil;
  SA.bInheritHandle := True;
  Handle := CreateFile(AnsiFn(FileName), AccessMode[Mode and 3], ShareMode[(Mode and $F0) shr 4],
    @SA, create_Always, file_attribute_Normal, 0);
  Result := SetResult(Handle <> invalid_Handle_Value);
end;

function SysFileCopy(_Old, _New: PChar; _Overwrite: Boolean): Boolean;
begin
  Result := CopyFile(_Old, _New, not _Overwrite);
end;

function SysFileSeek(Handle,Distance,Method: Longint; var Actual: Longint): Longint;
begin
  Actual := SetFilePointer(Handle, Distance, nil, Method);
  Result := SetResult(Actual <> $FFFFFFFF);
end;

function SysFileRead(Handle: Longint; var Buffer; Count: Longint; var Actual: Longint): Longint;
begin
  Result := SetResult(ReadFile(Handle, Buffer, Count, DWord(Actual), nil));
end;

function SysFileWrite(Handle: Longint; const Buffer; Count: Longint; var Actual: Longint): Longint;
var
  sbi: TConsoleScreenBufferInfo;
begin
  Result := SetResult(WriteFile(Handle, Buffer, Count, DWord(Actual), nil));
  {$IFDEF RouteConsoleToStdInOut}
  if (tidCursor <> -1) and (Handle = SysConOut) then
  {$ELSE}
  if  (tidCursor <> -1)
  and ((Handle = SysConOut)
  or  ((Handle = SysFileStdOut) and (SysFileIsDevice(SysFileStdOut)=1))
  or  ((Handle = SysFileStdErr) and (SysFileIsDevice(SysFileStdErr)=1))) then
  {$ENDIF}
    begin
      // Writeln without Crt unit: Update cursor position variable
      GetConsoleScreenBufferInfo(SysConOut, sbi);
      CurXPos := sbi.dwCursorPosition.x;
      CurYPos := sbi.dwCursorPosition.y;
    end;
end;

function SysFileSetSize(Handle,NewSize: Longint): Longint;
var
  CurPos: Longint;
begin
  CurPos := SetFilePointer(Handle, 0, nil, file_Current);
  Result := SetResult((CurPos <> $FFFFFFFF) and
    (SetFilePointer(Handle, NewSize, nil, file_Begin) <> $FFFFFFFF) and
    SetEndOfFile(Handle) or
    (SetFilePointer(Handle, CurPos, nil, file_Begin) <> $FFFFFFFF));
end;

function SysFileClose(Handle: Longint): Longint;
begin
  Result := SetResult(CloseHandle(Handle));
end;

function SysFileFlushBuffers(Handle: Longint): Longint;
begin
  Result := SetResult(FlushFileBuffers(Handle));
end;

function SysFileDelete(FileName: PChar): Longint;
begin
  Result := SetResult(DeleteFile(AnsiFn(FileName)));
end;

function SysFileMove(OldName,NewName: PChar): Longint;
begin
  Result := SetResult(MoveFile(OldName, NewName));
end;

function SysFileIsDevice(Handle: Longint): Longint;
var
  HandleType: Longint;
begin
  HandleType := GetFileType(Handle);
  case HandleType of
  0,1 : Result := 0; // File;
    2 : Result := 1; // Device
    3 : Result := 2; // Pipe
  end;
end;

function SysDirGetCurrent(Drive: Longint; Path: PChar): Longint;
var
  DriveName: array[0..3] of Char;
  Buffer: array[0..259] of Char;
begin
  // !! Compiler problem? Result is set by GetCurrentDirectory call!
  Result := 0;
  if Drive <> 0 then
  begin
    DriveName[0] := Chr(Drive + (Ord('A') - 1));
    DriveName[1] := ':';
    DriveName[2] := #0;
    GetCurrentDirectory(SizeOf(Buffer), Buffer);
    SetCurrentDirectory(DriveName);
  end;
  GetCurrentDirectory(260, Path);
  StrCopy( Path, OemFn(Path) );
  if Drive <> 0 then
    SetCurrentDirectory(Buffer);
end;

function SysDirSetCurrent(Path: PChar): Longint;
begin
  if Path^ = #0 then
    Result := 0 // Otherwise returns rc = 161: Bad path name
  else
    Result := SetResult(SetCurrentDirectory(AnsiFn(Path)));
end;

function SysDirCreate(Path: PChar): Longint;
begin
  Result := SetResult(CreateDirectory(AnsiFn(Path), nil));
end;

function SysDirDelete(Path: PChar): Longint;
begin
  Result := SetResult(RemoveDirectory(AnsiFn(Path)));
end;

function SysMemAvail: Longint;
var
  Status: TMemoryStatus;
begin
  Status.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(Status);
  with Status do
  begin
    Result := dwAvailPhys + dwAvailPageFile;
    if Result > dwAvailVirtual then
      Result := dwAvailVirtual;
  end;
end;

function PhysMemAvail: Longint;  {AK155 20-08-2003}
var
  Status: TMemoryStatus;
begin
  Status.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(Status);
  Result := Status.dwAvailPhys;
end;

function SysMemAlloc(Size,Flags: Longint; var MemPtr: Pointer): Longint;
begin
  MemPtr := VirtualAlloc(nil, Size, Flags, page_ReadWrite);
  Result := SetResult(MemPtr <> nil);
end;

function SysMemFree(MemPtr: Pointer): Longint;
begin
  Result := SetResult(VirtualFree(MemPtr, 0, mem_Release));
end;

function SysSysMsCount: Longint;
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

function SysCtrlSelfAppType: Longint;
var
  F       : File;
  lExeHdr : TImageDosHeader;
  lPEHdr  : TImageNtHeaders;
  SaveMode: Integer;
begin
  // Set default return value: GUI
  Result := 3;

  // Attempt to read information from PE file header.  This only works
  // if the file has not been compressed or otherwise manipulated.
  SaveMode := FileMode;
  FileMode := $40;          // Read-only, deny-none
  Assign(F, ParamStr(0));
  Reset(F, 1);
  if IOResult = 0 then
    begin
      BlockRead(f, lExeHdr, SizeOf(lExeHdr));

      if (IOResult = 0) and (lExeHdr.e_Magic = image_DOS_Signature) then
        begin
          Seek(F, lExeHdr.e_lfanew);
          BlockRead(F, lExeHdr.e_magic, SizeOf(lExeHdr.e_magic));
        end
      else
        lExeHdr.e_lfanew := 0;

      Seek(F, lExeHdr.e_lfanew);
      if (IOResult = 0) and (lExeHdr.e_magic = image_NT_Signature) then
        begin
          BlockRead(F, lPEHdr, SizeOf(lPEHdr));
          if (IOResult = 0) and (lPEHdr.Signature = image_NT_Signature) then
            if lPEHdr.OptionalHeader.SubSystem = image_Subsystem_Windows_CUI then
              Result := 2; // Text mode
        end;
      Close(F);
      InOutRes := 0;
    end;
  FileMode := SaveMode;
end;

function SysGetThreadId: Longint;
begin
  Result := GetCurrentThreadId;
end;

function SysCtrlKillThread(Handle: Longint): Longint;
begin
  Result := SetResult(TerminateThread(Handle, 0));
end;

function SysCtrlSuspendThread(Handle: Longint): Longint;
begin
  Result := SetResult(SuspendThread(Handle) <> $FFFFFFFF);
end;

function SysCtrlResumeThread(Handle: Longint): Longint;
begin
  Result := SetResult(ResumeThread(Handle) <> $FFFFFFFF);
end;

procedure SysCtrlExitThread(ExitCode: Longint);
var
  P: Pointer;
type
  TExitThread = procedure(ExitCode: Longint) stdcall;
begin
  P := QueryProcAddr('ExitThread', True);
  if P <> nil then
    TExitThread(P)(ExitCode)
  else
    SysCtrlExitProcess(ExitCode);
end;

procedure SysCtrlExitProcess(ExitCode: Longint);
begin
  ExitProcess(ExitCode);
end;

function SysCtrlCreateThread(Attrs: Pointer; StackSize: Longint; Func,Param: Pointer; Flags: Longint; var Tid: Longint): Longint;
begin
  Result := SetResult(CreateThread(Attrs, StackSize, Func, Param, Flags, Tid) <> 0);
end;

function SysCtrlGetModuleName(Handle: Longint; Buffer: PChar): Longint;
begin
  SetResult(GetModuleFileName(0, Buffer, 260) <> 0);
end;

var
  SysCritSec: TRTLCriticalSection;
  InitCritSec: Boolean;

procedure SysCtrlEnterCritSec;
var
  P: Pointer;
type
  TInitializeCriticalSectionAndSpinCount =
    procedure(CriticalSection: TRTLCriticalSection; SpinCount: DWord) stdcall;
begin
  if not InitCritSec then
    begin
      P := QueryProcAddr('InitializeCriticalSectionAndSpinCount', True);
      if assigned(P) then
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
  Len: Longint;
begin
  Result := P;
  repeat
    while Result^ in [#1..' '] do
      Inc(Result);
    if PSmallWord(Result)^ = (Ord('"') shl 8 + Ord('"')) then
      Inc(Result, 2)
    else
      Break;
  until False;
  Len := 0;
  while Result^ > ' ' do
    if Result^ = '"' then
      begin
        Inc(Result);
        while not (Result^ in [#0,'"']) do
        begin
          Inc(Len);
          Param[Len] := Result^;
          Inc(Result);
        end;
        if Result^ <> #0 then
          Inc(Result);
      end
    else
      begin
        Inc(Len);
        Param[Len] := Result^;
        Inc(Result);
      end;
  Param[0] := Chr(Len);
end;

function SysCmdln: PChar;
begin
  Result := GetCommandLine;
end;

function SysCmdlnCount: Longint;
var
  P: PChar;
  S: ShortString;
begin
  P := SysCmdln;
  Result := -1;
  repeat
    P := GetParamStr(P, S);
    if S = '' then
      Exit;
    Inc(Result);
  until False;
end;

procedure SysCmdlnParam(Index: Longint; var Param: ShortString);
var
  I: Longint;
  P: PChar;
  Buffer: array[0..260] of Char;
begin
  I := Index;
  if I = 0 then
    begin
      SysCtrlGetModuleName(0, Buffer);
      P := Buffer;
      Param := '';
      while (P^ <> #0) and (I < 255) do
      begin
        Inc(I);
        Param[I] := P^;
        Inc(P);
      end;
      SetLength(Param, I);
    end
  else
    begin
      // Skip ParamStr(0)
      P := SysCmdln;// + Length(ParamStr(0));
      repeat
        P := GetParamStr(P, Param);
        if (I = 0) or (Param = '') then
          Exit;
        Dec(I);
      until False;
    end;
end;

function SysGetProcessId: Longint;
begin
  Result := GetCurrentProcessID;
end;

function SysCtrlGetTlsMapMem: Pointer;
var
  IsNew: Boolean;
  MapHandle: Longint;
  SharedMemName: record
    L0: Longint;
    L1: Longint;
    L2: Longint;
    ID: array[0..11] of Char;
  end;
  P: Pointer;
type
  TOpenFileMapping = function(Acc: DWord; Inherit: Bool; Name: PChar): THandle stdcall;

begin
  SharedMemName.L0 := Ord('S') + Ord('H') shl 8 + Ord('A') shl 16 + Ord('R') shl 24;
  SharedMemName.L1 := Ord('E') + Ord('D') shl 8 + Ord('M') shl 16 + Ord('E') shl 24;
  SharedMemName.L2 := Ord('M') + Ord('4') shl 8 + Ord('V') shl 16 + Ord('S') shl 24;
  Str(GetCurrentProcessID, SharedMemName.ID);
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
      MapHandle := TOpenFileMapping(P)(file_map_Read+file_map_Write, False, PChar(@SharedMemName));
      if MapHandle = 0 then
      begin
        MapHandle := CreateFileMapping($FFFFFFFF, nil, page_ReadWrite, 0, SharedMemSize, PChar(@SharedMemName));
        IsNew := True;
      end;
      Result := MapViewOfFile(MapHandle, file_map_Read+file_map_Write, 0, 0, 0);
    end;
  if IsNew then
    begin
      FillChar(Result^, SharedMemSize, $FF);
      FillChar(Result^, SizeOf(TSharedMem), 0);
      // Set up pointers to functions to use when allocating memory
      System.GetMemoryManager( PSharedMem(Result)^.TlsMemMgr );
    end;
end;

function SysGetEnvironment: PChar;
begin
  Result := GetEnvironmentStrings;
end;

function SysOsVersion: Longint;
begin
  Result := SmallWord(GetVersion);
end;

{Cat: SysPlatformID �࠭���� � ��६�����, � ᮤ�ন��� �⮩ �㭪樨 ��७�ᥭ� � ࠧ��� ���樠����樨}
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
function SysPlatformID: Longint;
begin
  SysPlatformID := SysPlatform;
end;
{/Cat}

procedure SysGetDateTime(Year,Month,Day,DayOfWeek,Hour,Minute,Second,MSec: PLongint);
var
  DT: TSystemTime;
begin
  GetLocalTime(DT);
  if Year <> nil then Year^ := DT.wYear;
  if Month <> nil then Month^ := DT.wMonth;
  if Day <> nil then Day^ := DT.wDay;
  if DayOfWeek <> nil then DayOfWeek^ := DT.wDayOfWeek;
  if Hour <> nil then Hour^ := DT.wHour;
  if Minute <> nil then Minute^ := DT.wMinute;
  if Second <> nil then Second^ := DT.wSecond;
  if MSec <> nil then MSec^ := DT.wMilliseconds;
end;

procedure SysSetDateTime(Year,Month,Day,Hour,Minute,Second,MSec: PLongint);
var
  DT: TSystemTime;
begin
  GetLocalTime(DT);
  if Year <> nil then DT.wYear := Year^;
  if Month <> nil then DT.wMonth := Month^;
  if Day <> nil then DT.wDay := Day^;
  if Hour <> nil then DT.wHour := Hour^;
  if Minute <> nil then DT.wMinute := Minute^;
  if Second <> nil then DT.wSecond := Second^;
  if MSec <> nil then DT.wMilliseconds := MSec^;
  SetLocalTime(DT);
end;

function SysVerify(SetValue: Boolean; Value: Boolean): Boolean;
begin
  Result := False;
end;

{Cat}
function SysDiskFreeLong(Drive: Byte): TQuad;
var
  RootPath: LongInt;
begin
  if Drive > 0 then
    begin
      RootPath := Drive + $005C3A40; {'A:\'#0}
      Result := SysDiskFreeLongX(@RootPath);
    end;
end;

function SysDiskSizeLong(Drive: Byte): TQuad;
var
  RootPath: LongInt;
begin
  if Drive > 0 then
    begin
      RootPath := Drive + $005C3A40; {'A:\'#0}
      Result := SysDiskSizeLongX(@RootPath);
    end;
end;

function SysDiskFreeLongX(Path: PChar): TQuad;
var
  SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters: DWord;
  AvailableForCaller, Total, Free: TQuad;
begin
  if Assigned(GetDiskFreeSpaceEx) then
     if GetDiskFreeSpaceEx(Path, AvailableForCaller, Total, Free) then
       Result := Free
     else
       Result := -1
  else
    if GetDiskFreeSpace(Path, SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters) then
      Result := 1.0 * SectorsPerCluster * BytesPerSector * FreeClusters
    else
      Result := -1;
end;

function SysDiskSizeLongX(Path: PChar): TQuad;
var
  SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters: DWord;
  AvailableForCaller, Total, Free: TQuad;
begin
  if Assigned(GetDiskFreeSpaceEx) then
     if GetDiskFreeSpaceEx(Path, AvailableForCaller, Total, Free) then
       Result := Total
     else
       Result := -1
  else
    if GetDiskFreeSpace(Path, SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters) then
      Result := 1.0 * SectorsPerCluster * BytesPerSector * TotalClusters
    else
      Result := -1;
end;
{/Cat}

function SysGetFileAttr(FileName: PChar; var Attr: Longint): Longint;
begin
  Attr := GetFileAttributes(AnsiFn(FileName));
  Result := SetResult(Attr <> -1);
  if Attr = -1 then
    Inc(Attr);
end;

function SysSetFileAttr(FileName: PChar; Attr: Longint): Longint;
begin
  Result := SetResult(SetFileAttributes(AnsiFn(FileName), Attr));
end;

function SysGetFileTime(Handle: Longint; var Time: Longint): Longint;
var
  FileTime, LocalFileTime: TFileTime;
begin
  Result := SetResult(GetFileTime(Handle, nil, nil, @FileTime) and
    FileTimeToLocalFileTime(FileTime, LocalFileTime) and
    FileTimeToDosDateTime(LocalFileTime, TDateTimeRec(Time).FDate, TDateTimeRec(Time).FTime));
end;

function SysSetFileTime(Handle: Longint; Time: Longint): Longint;
var
  LocalFileTime, FileTime: TFileTime;
begin
  Result := SetResult(DosDateTimeToFileTime(TDateTimeRec(Time).FDate, TDateTimeRec(Time).FTime, LocalFileTime) and
    LocalFileTimeToFileTime(LocalFileTime, FileTime) and
    SetFileTime(Handle, nil, nil, @FileTime));
end;

function DoFindFile(var F: TOSSearchRec; IsPChar: Boolean): Longint;
var
  LocalFileTime: TFileTime;
  ExclAttr: Longint;
  InclAttr: Longint;
begin
  // Extract Include/Exclude attributes from F.ExcludeAttr field
  ExclAttr := not F.ExcludeAttr and (file_Attribute_Hidden or file_Attribute_System or $8 or file_Attribute_Directory or file_Attribute_Archive);
  InclAttr := (F.ExcludeAttr and $FF00) shr 8;
  // Make sure attributes are not both excluded and included
  ExclAttr := ExclAttr and not InclAttr;
  with F do
  begin
    // Reject entries where
    // - Attributes that are excluded are present.
    // - Attributes that must be present are not all there
    while (FindData.dwFileAttributes and ExclAttr <> 0) or
      (FindData.dwFileAttributes and InclAttr <> InclAttr) do
      if not FindNextFile(Handle, FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, TDateTimeRec(Time).FDate, TDateTimeRec(Time).FTime);
    Size := FindData.nFileSizeLow;
    Attr := FindData.dwFileAttributes;
    // Convert filename to OEM character set
    if RecodeAnsiNames then                         {JO}
      CharToOem(FindData.cFileName, FindData.cFileName);
    if IsPChar then
      StrCopy(PChar(@Name), FindData.cFileName)
    else
      Name := StrPas(FindData.cFileName);
  end;
  Result := 0;
end;

function SysFindFirst(Path: PChar; Attr: Longint; var F: TOSSearchRec; IsPChar: Boolean): Longint;
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

function SysFindNext(var F: TOSSearchRec; IsPChar: Boolean): Longint;
begin
  if FindNextFile(F.Handle, F.FindData) then
    Result := DoFindFile(F, IsPChar)
  else
    Result := GetLastError;
end;

function SysFindClose(var F: TOSSearchRec): Longint;
begin
  if F.Handle = invalid_Handle_Value then
    Result := 0
  else
    Result := SetResult(Windows.FindClose(F.Handle));
  F.Handle := invalid_Handle_Value;
end;

// Check if file exists; if it does, update FileName parameter
// to include correct case of existing file
function SysFileAsOS(FileName: PChar): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
  P: PChar;
begin
  Handle := FindFirstFile(AnsiFn(FileName), FindData);
  if Handle <> invalid_Handle_Value then
    begin
      if FindData.cFileName[0] <> #0 then
        begin
          // Replace filename part with data returned by Windows
          P := StrRScan(FileName, '\');
          if P = nil then
            P := FileName
          else
            inc(P); // Point to first character of file name
          strcopy(P, FindData.cFileName);
        end;
      FindClose(Handle);
      Result := True;
    end
  else
    Result := False;
end;

function SysFileSearch(Dest,Name,List: PChar): PChar;
var
  I, P, L: Integer;
  Buffer: array[0..259] of Char;
begin
  Result := Dest;
  StrCopy(Buffer, Name);
  P := 0;
  L := StrLen(List);
  while True do
  begin
    if SysFileAsOS(Buffer) then
    begin
      SysFileExpand(Dest, Buffer);
      Exit;
    end;
    while (P < L) and (List[P] = ';') do
      Inc(P);
    if P >= L then
      Break;
    I := P;
    while (P < L) and (List[P] <> ';') do
      Inc(P);
    StrLCopy(Buffer, List + I, P - I);
    if not (List[P-1] in [':', '\']) then
      StrLCat(Buffer, '\', 259);
    StrLCat(Buffer, Name, 259);
  end;
  Dest^ := #0;
end;

function SysFileExpand(Dest,Name: PChar): PChar;
var
  L: Longint;
  NameOnly: PChar;
begin
  if strlen(Name) = 0 then
    SysDirGetCurrent(0, Dest)
  else
    if GetFullPathName(AnsiFn(Name), 260, Dest, NameOnly) = 0 then
      StrCopy(Dest, Name) // API failed; copy name to dest
    else
     begin
      Dest := OemFn(Dest);
      if (StrComp(Name, '.') <> 0) and (StrComp(Name, '..') <> 0) then
        begin
          L := StrLen(Name);
          if (L > 0) and (Name[L-1] = '.') then
          begin
            L := StrLen(Dest);
            if (L > 0) and (Dest[L-1] <> '.') then
            begin
              Dest[L] := '.';
              Dest[L+1] := #0;
            end;
          end;
        end;
     end;
  Result := Dest;
end;

threadvar
  ProcessInfo: TProcessInformation;
  LastAsync: Boolean;

function SysExecute(Path,CmdLine,Env: PChar; Async: Boolean; PID: PLongint; StdIn,StdOut,StdErr: Longint): Longint;
var
  P: PChar;
  Flags: Longint;
  StartupInfo: TStartupInfo;
  CmdLineBuf: array [0..8191] of Char;
begin
  P := CmdLineBuf;
  P^ := '"';                   // Quotes to support spaces
  inc(P);
  P := StrECopy(P, Path);      // 'Path'#0
  P^ := '"';
  inc(P);
  P^ := ' ';
  StrCopy(P+1, CmdLine);                // 'Path CommandLine'#0
  if RecodeAnsiNames then OemToChar(P, P); {JO}
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
    if (StdIn <> - 1) or (StdOut <> -1) or (StdErr <> -1) then
      Inc(dwFlags, startf_UseStdHandles);
  end;
  Flags := normal_Priority_Class;
  LastAsync := Async;
  Result := SetResult(CreateProcess(Path, CmdLineBuf, nil, nil, True, Flags, Env, nil, StartupInfo, ProcessInfo));
  if Result = 0 then
    if Async then
      begin
        if PID <> nil then
          PID^ := ProcessInfo.hProcess;
      end
    else
      begin
      hExtCmd := ProcessInfo.hProcess; {AK155: ��� Killer}
      WaitForSingleObject(hExtCmd, Infinite);
      end;
end;

function SysExitCode: Longint;
begin
  if LastAsync then
    WaitForSingleObject(ProcessInfo.hProcess, Infinite);
  GetExitCodeProcess(ProcessInfo.hProcess, Result);
end;

procedure SysGetCaseMap(TblLen: Longint; Tbl: PChar );
begin
  CharUpperBuff(Tbl, TblLen);
end;

procedure SysGetWeightTable(TblLen: Longint; WeightTable: PChar);
var
  I: Longint;

begin
  // !!! Must use Win32 function
  for I := 0 to pred(TblLen) do
    begin
      WeightTable^ := Chr(i);
      inc(WeightTable);
    end;
end;

function SysGetCodePage: Longint;
var
  P: Pointer;
type
  TGetKBCodePage = function: Longint;
begin
  P := QueryProcAddr('GetKBCodePage', False);
  if P = nil then
    Result := 0
  else
    Result := TGetKBCodePage(P);
end;

function SysCompareStrings(s1, s2: PChar; l1, l2: Longint; IgnoreCase: Boolean): Longint;
begin
  if IgnoreCase then
    Result := CompareString(Locale_User_Default, norm_ignorecase,S1, l1, s2, l2)-2
  else
    Result := CompareString(Locale_User_Default, 0, S1, l1, s2, l2)-2;
end;

procedure SysChangeCase(Source, Dest: PChar; Len: Longint; NewCase: TCharCase);
var
  I: Longint;
begin
  if NewCase in [ccLower, ccUpper] then
    begin
      i := 0;
      while i < Len do
        begin
          if NewCase = ccLower then
            if Source^ in ['A'..'Z'] then
              Dest^ := chr(ord(Source^)+32)
            else
              Dest^ := Source^
          else
            if Source^ in ['a'..'z'] then
              Dest^ := chr(ord(Source^)-32)
            else
              Dest^ := Source^;
          inc(i);
          inc(Source);
          inc(Dest);
        end;
    end
  else
    begin
      // AnsiUpper and AnsiLower
      StrLCopy(Dest, Source, Len);
      if NewCase = ccAnsiLower then
        CharLowerBuff(Dest, Len)
      else
        CharUpperBuff(Dest, Len);
    end;
end;

function SysLowerCase(s: PChar): PChar;
begin
  Result := CharLower(s);
end;

function SysUpperCase(s: PChar): PChar;
begin
  Result := CharUpper(s);
end;

var
  PrevXcptProc: Pointer;

function SignalHandler(Report:       PExceptionRecord;
                       Registration: Pointer;
                       Context:      PContext;
                       P:            Pointer): Longint; stdcall;
begin
  if (Report^.ExceptionCode = status_Control_C_Exit) and Assigned(CtrlBreakHandler) and CtrlBreakHandler then
    Result := 1
  else
    Result := 0;
  XcptProc := PrevXcptProc;
end;

function CtrlHandler(CtrlType: DWord): Bool; stdcall;
begin
  if Assigned(CtrlBreakHandler) and CtrlBreakHandler then
    Result := True
  else
    Result := False;
end;

procedure SysCtrlSetCBreakHandler;
begin
  PrevXcptProc := XcptProc;
  XcptProc := @SignalHandler;
  SetConsoleCtrlHandler(@CtrlHandler, True);
end;

function SysFileIncHandleCount(Count: Longint): Longint;
begin
  // Return 0, indicating success.  In Win95/NT, the number of file handles
  // is limited by available physical memory only.
  Result := 0;
end;

procedure DoSetCursorPosition;
var
  CurPos: TCoord;
begin
  CurPos.x := CurXPos;
  CurPos.y := CurYPos;
  SetConsoleCursorPosition(SysConOut, CurPos);
end;

{$IFDEF LazyCurTypeUpdate}
procedure DoSetCurType(Y1,Y2: Integer; Show: Boolean); forward; {Cat}
{$ENDIF}

function CursorThreadFunc(P: Pointer): Longint;
//AK155  LastX, LastY: Longint;
  {$IFDEF LazyCurTypeUpdate}
var
  LastCurTypeY1, LastCurTypeY2: Integer;
  LastCurTypeShow: Boolean;
  {$ENDIF}
begin
  LastX := -1;
  LastY := -1;
  repeat
    {$IFNDEF LazyCurTypeUpdate}
    SemWaitEvent(semCursor, {300}Infinite); {AK155
  � ⠩���⮬ 300 �� �맮�� ���譥� ���᮫쭮� �ணࠬ�� �����
�த����� �ࠢ������ �⮩ ��⪮�, �� ���ࠢ��쭮 � �ਢ����� �
��⠭���� ����� � ��砫� ��ப�, ���� �᫨ �맢����� �ணࠬ��
ᤥ���� �뢮� ��� ��ॢ��� ��ப� (���ਬ��, ������ ����� � ����
�⢥� � ⮩ �� ��ப�).
   � ��祬 �㦭� 300 - � ������ �� ᬮ�. }
    {$ENDIF}
    if (CurXPos <> LastX) or (CurYPos <> LastY) then
      begin
        DoSetCursorPosition;
        LastX := CurXPos;
        LastY := CurYPos;
      end;
    {Cat}
    {$IFDEF LazyCurTypeUpdate}
    if not SemWaitEvent(semCursor, 100) then
      if (CurTypeY1 <> LastCurTypeY1) or (CurTypeY2 <> LastCurTypeY2) or (CurTypeShow <> LastCurTypeShow) then
        begin
          DoSetCurType(CurTypeY1, CurTypeY2, CurTypeShow);
          LastCurTypeY1 := CurTypeY1;
          LastCurTypeY2 := CurTypeY2;
          LastCurTypeShow := CurTypeShow;
        end;
    {$ENDIF}
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

{$IFDEF AutoKbdUpdateEventQueues}
  tidKbdUpdateEventQueues := -2; {Cat}
{$ENDIF}
end;

{Cat}
{$IFDEF AutoKbdUpdateEventQueues}
function KbdUpdateEventQueuesThreadFunc(P: Pointer): Longint;
begin
  repeat
    WaitForSingleObject(SysFileStdIn,10000);
    pKbdUpdateEventQueues;
  until tidCursor = -2;
  tidCursor := -1;
end;
{$ENDIF}
{/Cat}

procedure InitialiseKeyboardHandler;
begin
{AK155,Cat: �� � 祬� ����㦠�� �������⭮ ����� VpKbdW32.dll}
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
  pKbdInit(pSysKeyCount, pSysKeyQue, pSysShiftState, pSysMouCount, pSysMouQue);

  {$IFDEF AutoKbdUpdateEventQueues}
  BeginThread(nil, 16384, KbdUpdateEventQueuesThreadFunc, nil, 0, tidKbdUpdateEventQueues);
  {$ENDIF}
{/Cat}
end;

procedure InitialiseConsole;
begin
  {$IFDEF RouteConsoleToStdInOut}
  SysConIn  := SysFileStdIn;
  SysConOut := SysFileStdOut;
  {$ELSE}
  SysConIn  := CreateFile('CONIN$' , generic_Read or generic_Write, file_share_Read, nil, open_Existing, file_attribute_Normal, 0);
  SysConOut := CreateFile('CONOUT$', generic_Read or generic_Write, file_share_Read or file_share_Write, nil, open_Existing, file_attribute_Normal, 0);
  {$ENDIF}
  InitialiseKeyboardHandler;
end;


const
  CrtScanCode: Byte = 0;

function SysKeyPressed: Boolean;
begin
  if CrtScanCode <> 0 then
    Result := True
  else
    begin
      {InitialiseKeyboardHandler;}
      {$IFNDEF AutoKbdUpdateEventQueues}
      pKbdUpdateEventQueues;
      {$ENDIF}
      Result := pSysKeyCount^ <> 0;
    end;
end;

function SysPeekKey(Var Ch: Char): Boolean;
begin
  {$IFNDEF AutoKbdUpdateEventQueues}
  pKbdUpdateEventQueues;
  {$ENDIF}
  if pSysKeyCount^ = 0 then { Kirill: Change ">" -> "=" }
    Result := False
  else
    begin
      Result := True;
      if Lo(pSysKeyQue^[0].skeKeyCode) in [0{,$E0}] then {JO}
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
        {$IFNDEF AutoKbdUpdateEventQueues}
        pKbdUpdateEventQueues;
        {$ENDIF}
        if pSysKeyCount^ = 0 then
          WaitForSingleObject(SysConIn, infinite);
      until pSysKeyCount^ <> 0;
      Dec(pSysKeyCount^);
      if Lo(pSysKeyQue^[0].skeKeyCode) in [0{,$E0}] then {JO}
        begin
          CrtScanCode := Hi(pSysKeyQue^[0].skeKeyCode);
          Result := #0;
        end
      else
        Result := Chr(Lo(pSysKeyQue^[0].skeKeyCode));
      Move(pSysKeyQue^[1], pSysKeyQue^[0], pSysKeyCount^ * SizeOf(TSysKeyEvent));
    end;
end;

procedure SysFlushKeyBuf;
begin
  {InitialiseKeyboardHandler;}
  CrtScanCode := 0;
  pSysKeyCount^ := 0;
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
      CurXPos := sbi.dwCursorPosition.x;
      CurYPos := sbi.dwCursorPosition.y;

      semCursor := SemCreateEvent(nil, false, false);
      BeginThread(nil, 16384, CursorThreadFunc, nil, 0, tidCursor );
      SemPostEvent(semCursor);
      AddExitProc(CursorThreadExitProc);
    end;
end;

procedure SysWrtCharStrAtt(CharStr: Pointer; Len, X, Y: SmallWord; var Attr: Byte);
var
  Buffer: Array[0..255] of TWin32Cell;
  BufferSize,
  BufferCoord: TCoord;
  WriteRegion: TSmallRect;
  BufNext: ^TWin32Cell;
  I: Longint;
begin
  InitialiseCursorThread;
  BufNext := @Buffer;
  for i := 0 to Len-1 do
    begin
      BufNext^.Attr := Attr;
      BufNext^.Ch := Ord( PChar(CharStr)^ );
      inc(PChar(CharStr));
      inc(BufNext);
    end;
  with BufferSize do
    begin
      x := Len;
      y := 1;
    end;
  with BufferCoord do
    begin
      x := 0;
      y := 0;
    end;
  with WriteRegion do
    begin
      Left := x;
      Top := y;
      Right := x+Len-1;
      Bottom := y;
    end;
  WriteConsoleOutput( SysConOut, @Buffer, BufferSize, BufferCoord, WriteRegion );
end;

function SysReadAttributesAt(x,y: SmallWord): Byte;
var
  WasRead: Longint;
  Coor: TCoord;
  Temp: SmallWord;
begin
  SysTVInitCursor;
  Coor.x := x;
  Coor.y := y;
  ReadConsoleOutputAttribute(SysConOut, @Temp, 1, Coor, WasRead);
  Result := Temp;
end;

function SysReadCharAt(x,y: SmallWord): Char;
var
  WasRead: Longint;
  Coor: TCoord;
begin
  SysTVInitCursor;
  Coor.x := x;
  Coor.y := y;
  ReadConsoleOutputCharacter(SysConOut, @Result, 1, Coor, WasRead);
  if WasRead = 0 then
    Result := #0;
end;

procedure SysScrollUp(X1,Y1,X2,Y2,Lines,Cell: SmallWord);
var
  Cliprect,
  ScrollRect: TSmallRect;
  DestCoord: TCoord;
  Fill: TWin32Cell;
  i: Integer;
begin
  {if SysPlatform = -1 then
    SysPlatform := SysPlatformID;}
  Fill.ch := Lo(Cell);
  Fill.Attr := Hi( Cell );
  ScrollRect.Left := X1;
  ScrollRect.Top := Y1;
  ScrollRect.Right := X2;
  ScrollRect.Bottom := Y2;
  ClipRect := ScrollRect;
  if SysPlatform = VER_PLATFORM_WIN32_NT then
    begin
      DestCoord.x := X1;       // This API works in Windows NT
      DestCoord.y := Y1-Lines;
      ScrollConsoleScreenBuffer(SysConOut, ScrollRect, @ClipRect, DestCoord, PCharInfo(@Fill)^);
    end
  else
    begin
      if Lines > 1 then
        for i := 1 to 2 do // Half a screen at a time; bug in Win95
          begin
            DestCoord.x := X1;
            DestCoord.y := Y1-Lines div 2;
            ScrollConsoleScreenBuffer(SysConOut, ScrollRect, @ClipRect, DestCoord, PCharInfo(@Fill)^);
          end;
      if odd(Lines) then // Also do last line, if odd number
        begin
          DestCoord.x := X1;
          DestCoord.y := Y1-1;
          ScrollConsoleScreenBuffer(SysConOut, ScrollRect, @ClipRect, DestCoord, PCharInfo(@Fill)^);
        end;
    end;
end;

procedure SysScrollDn(X1,Y1,X2,Y2,Lines,Cell: SmallWord);
var
  ScrollRect: TSmallRect;
  DestCoord: TCoord;
  Fill: TWin32Cell;
begin
  Fill.ch := Lo(Cell);
  Fill.Attr := Hi(Cell);
  ScrollRect.Left := X1;
  ScrollRect.Top := Y1;
  ScrollRect.Right := X2;
  ScrollRect.Bottom := Y2-Lines;
  DestCoord.x := X1;
  DestCoord.y := Y1+Lines;
  ScrollConsoleScreenBuffer(SysConOut, ScrollRect, nil, DestCoord, PCharInfo(@Fill)^);
end;

procedure SysGetCurPos(var X,Y: SmallWord);
begin
  if CurXPos = -1 then
    InitialiseCursorThread;
  X := CurXPos;
  Y := CurYPos;
end;

function SysTVDetectMouse: Longint;
begin
  Result := 2;
end;

procedure SysTVInitMouse(var X,Y: Integer);
begin
  X := 0;
  Y := 0;
end;

procedure SysTVDoneMouse(Close: Boolean);
begin
end;

procedure SysTVShowMouse; // No control over mouse pointer in Win32
begin
end;

procedure SysTVHideMouse; // No control over mouse pointer in Win32
begin
end;

procedure SysTVUpdateMouseWhere(var X,Y: Integer);
begin
end;

function SysTVGetMouseEvent(var Event: TSysMouseEvent): Boolean;
begin
  {InitialiseKeyboardHandler;}
  {$IFNDEF AutoKbdUpdateEventQueues}
  pKbdUpdateEventQueues;
  {$ENDIF}
  if pSysMouCount^ = 0 then
    Result := False
  else
    begin
      Dec(pSysMouCount^);
      Event := pSysMouQue^[0];
      Move(pSysMouQue^[1], pSysMouQue^[0], pSysMouCount^ * SizeOf(TSysMouseEvent));
      Result := True;
    end;
end;

procedure SysTVKbdInit;
begin
  SetConsoleMode(SysConIn, ENABLE_MOUSE_INPUT);
end;

function SysTVGetKeyEvent(var Event: TSysKeyEvent): Boolean;
begin
  {InitialiseKeyboardHandler;}
  {$IFNDEF AutoKbdUpdateEventQueues}
  pKbdUpdateEventQueues;
  {$ENDIF}
  if pSysKeyCount^ = 0 then
    Result := False
  else
    begin
      Dec(pSysKeyCount^);
      Event := pSysKeyQue^[0];
      Move(pSysKeyQue^[1], pSysKeyQue^[0], pSysKeyCount^ * SizeOf(TSysKeyEvent));
      Result := True;
    end;
end;

function SysTVPeekKeyEvent(var Event: TSysKeyEvent): Boolean;
begin
  {InitialiseKeyboardHandler;}
  {$IFNDEF AutoKbdUpdateEventQueues}
  pKbdUpdateEventQueues;
  {$ENDIF}
  if pSysKeyCount^ = 0 then
    Result := False
  else
    begin
      Event := pSysKeyQue^[0];
      Result := True;
    end;
end;

function SysTVGetShiftState: Byte;
begin
  {InitialiseKeyboardHandler;}
  Result := pSysShiftState^;
end;

procedure SysTVSetCurPos(X,Y: Integer);
var
  CurPos: TCoord;
begin
  if (CurXPos = X) and (CurYPos = Y) then Exit; {KV}
  CurXPos := X;
  CurYPos := Y;
  {$IFDEF RouteConsoleToStdInOut}
  if tidCursor = -1 then
  {$ENDIF}
    // Set cursor position without using cursor thread
    DoSetCursorPosition
  {$IFDEF RouteConsoleToStdInOut}
  else
    // Record cursor position; tell cursor thread to update
    SemPostEvent(semCursor);
  {$ENDIF}
  WaitForSingleObjectEx(SysConOut,16,true); {KV}
end;

{Cat}
{$IFDEF LazyCurTypeUpdate}
procedure DoSetCurType(Y1,Y2: Integer; Show: Boolean);
var
  Info: TConsoleCursorInfo;
begin
  Info.bVisible := Show;
  if Abs(Y1 - Y2) <= 1 then
    Info.dwSize := 15
  else
    Info.dwSize := 99;
  SetConsoleCursorInfo(SysConOut, Info);
end;

procedure DoGetCurType(var Y1,Y2: Integer; var Visible: Boolean);
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

procedure SysTVSetCurType(Y1,Y2: Integer; Show: Boolean);
begin
  CurTypeY1 := Y1;
  CurTypeY2 := Y2;
  CurTypeShow := Show;
  SemPostEvent(semCursor);
end;

procedure SysTVGetCurType(var Y1,Y2: Integer; var Visible: Boolean);
begin
  Y1 := CurTypeY1;
  Y2 := CurTypeY2;
  Visible := CurTypeShow;
end;
{$ELSE}
{/Cat}
procedure SysTVSetCurType(Y1,Y2: Integer; Show: Boolean);
var
  Info: TConsoleCursorInfo;
begin
  Info.bVisible := Show;
  if Abs(Y1 - Y2) <= 1 then
    Info.dwSize := 15
  else
    Info.dwSize := 99;
  SetConsoleCursorInfo(SysConOut, Info);
end;

procedure SysTVGetCurType(var Y1,Y2: Integer; var Visible: Boolean);
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
{$ENDIF}

procedure WriteConsoleLine(X,Y: Integer; Len: Integer);
var
  P: PChar;
  Q: PWin32Cell;
  LineBuf: array[0..255] of TWin32Cell;
  R: TSmallRect;
  BufPos: TCoord;
  LineSize: TCoord;
begin
  InitialiseCursorThread;
  { Prepared parameters }
  LineSize.X := SysBufInfo.dwSize.X;
  LineSize.Y := 1;
  BufPos.X := 0;
  BufPos.Y := 0;
  R.Left := X;
  R.Top  := Y;
  R.Right := X + Len - 1;
  R.Bottom := Y;
  { Translate the buffer from DOS-OS/2 format to Win32 }
  P := SysScrBuf + ((Y * SysBufInfo.dwSize.X) + X) * 2;
  Q := @LineBuf;
  while Len > 0 do
  begin
    Q^.Ch := Ord(P^);
    Inc(P);
    Q^.Attr := Ord(P^);
    Inc(P);
    Inc(Q);
    Dec(Len);
  end;
  WriteConsoleOutput(SysConOut, @LineBuf, LineSize, BufPos, R);
end;

function Min(X,Y: Integer): Integer;
begin
  Result := Y;
  if X < Y then
    Result := X;
end;

procedure SysTVShowBuf(Pos,Size: Integer);
var
  I,X,Y: Integer;
begin
  Pos := Pos div 2;
  X := Pos mod SysBufInfo.dwSize.X;
  Y := Pos div SysBufInfo.dwSize.X;
  while Size > 0 do
  begin
    I := Min(SysBufInfo.dwSize.X - X, Size div 2);
    WriteConsoleLine(X, Y, I);
    Dec(Size, I * 2);
    X := 0;
    Inc(Y);
  end;
end;

procedure SysTVClrScr;
var
  I,BufSize: Integer;
begin
  BufSize := SysBufInfo.dwSize.X * SysBufInfo.dwSize.Y * 2;
  I := 0;
  while I < BufSize do
  begin
    SysScrBuf[I] := ' ';
    Inc(I);
    SysScrBuf[I] := #7;
    Inc(I);
  end;
  SysTVShowBuf(0, BufSize);
  SysTVSetCurPos(0, 0);
end;

function SysTVGetScrMode(Size: PSysPoint): Integer;
begin
  GetConsoleScreenBufferInfo(SysConOut, SysBufInfo);
  case SysBufInfo.dwSize.Y of
    25:    Result := $0003;
    43,50: Result := $0103;
    else   Result := $00FF;
  end;
  if Size <> nil then
    with Size^ do
    begin
      X := SysBufInfo.dwSize.X;
      Y := SysBufInfo.dwSize.Y;
      if Size.Y > 234 then
        Size.Y := 234;
    end;
end;

procedure SysTVSetScrMode(Mode: Integer);
var
  R: TSmallRect;
  Size: TCoord;
begin
  Size.X := 80;
  Size.Y := 25;
  if Mode and $0100 <> 0 then
    Size.Y := 50;
  SetConsoleScreenBufferSize(SysConOut, Size);
  R.Left   := 0;
  R.Top    := 0;
  R.Right  := Size.X - 1;
  R.Bottom := Size.Y - 1;
  SetConsoleWindowInfo(SysConOut, True, R);
end;

function SysTVGetSrcBuf: Pointer;
const
  First: Boolean = True;
  UpLeft: TCoord= (X:0; Y:0);
  ReadFrom: TSmallRect = (Left:0; Top:0; Right:0; Bottom:0);
var
  Size: TSysPoint;
  Coord: TCoord;
  Buffer: PWin32Cell;
  PDest: PStandardCell;
  PSrc: PWin32Cell;
  X,Y: Longint;
begin
  Result := @SysScrBuf;
  if First then
    begin
      First := False;
      SysTVGetScrMode(@Size);
      Coord.X := Size.X;
      Coord.Y := Size.Y;
      ReadFrom.Right := Size.X;
      ReadFrom.Bottom := Size.Y;
      // Read existing content of screen into buffer
      GetMem(Buffer, (Size.X+1)*(Size.Y+1)*SizeOf(TWin32Cell));
      if not ReadConsoleOutput(SysConOut, Buffer, Coord, UpLeft, ReadFrom) then
        X := GetLastError;
      // Move the data to the screen buffer in standard format
      PSrc := Buffer;
      PDest := Result;
      for Y := 0 to Size.Y-1 do
        for X := 0 to Size.X-1 do
          begin
            PDest^.Ch := chr(PSrc^.Ch);
            PDest^.Attr := byte(PSrc^.Attr);
            inc(PSrc);
            inc(PDest);
          end;
      FreeMem(Buffer);
    end;
end;

{AK155 ��᫥ �맮�� ���譥� �ணࠬ�� ॠ�쭮� ��������� �����
����� �� ᮮ⢥��⢮���� LastX, LastX, ����� ����ᥭ� ��⪮�
�����. ���⮬� ���஦��� �㤥� ������� ��������� ����� �
��⥬� � �ਢ��� ��६���� � ᮮ⢥��⢨� � ॥����⮬.
��אַ� ��� ��ᢥ��� �맮� SysTVInitCursor ������ ���� ��᫥
��� �맮�� ���譥� �ணࠬ��.  }
procedure SysTVInitCursor;
var
  b: boolean;
  sbi: TConsoleScreenBufferInfo;
begin
if GetConsoleScreenBufferInfo(SysConOut, sbi) then
  begin
  CurXPos := sbi.dwCursorPosition.x;
  LastX := CurXPos;
  CurYPos := sbi.dwCursorPosition.y;
  LastY := CurYPos;
  end;
(*
  if SysConIn = -1 then
    InitialiseConsole;
*)
end;

procedure SysCtrlSleep(Delay: Integer);
begin
  Sleep(Delay);
end;

function SysGetValidDrives: Longint;
begin
  Result := GetLogicalDrives;
end;

procedure SysDisableHardErrors;
begin
  SetErrorMode(sem_FailCriticalErrors);
end;

function SysKillProcess(Process: Longint): Longint;
begin
  Result := SetResult(TerminateProcess(Process, -1));
end;

function SysAllocSharedMem(Size: Longint; var MemPtr: Pointer): Longint;
begin
  Result := -1;
end;

function SysGiveSharedMem(MemPtr: Pointer): Longint;
begin
  Result := -1;
end;

function SysPipeCreate(var ReadHandle,WriteHandle: Longint; Size: Longint): Longint;
var
  SA: TSecurityAttributes;
begin
  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := nil;
  SA.bInheritHandle := True;
  Result := SetResult(CreatePipe(ReadHandle, WriteHandle, @SA, Size));
end;

function SysPipePeek(Pipe: Longint; Buffer: Pointer; BufSize: Longint; var BytesRead: Longint; var IsClosing: Boolean): Longint;
var
  State: Longint;
begin
  Result := SetResult(PeekNamedPipe(Pipe, Buffer, BufSize, @BytesRead, nil, nil));
  IsClosing := WaitForSingleObject(ProcessInfo.hProcess, 0) = wait_Object_0;
end;

function SysPipeClose(Pipe: Longint): Longint;
begin
  Result := SysFileClose(Pipe);
end;

{$IFDEF USESYSUTILS}
function SysLoadResourceString(ID: Longint; Buffer: PChar; BufSize: Longint): PChar;
begin
  Buffer[0] := #0;
  LoadString(HInstance, ID, Buffer, BufSize);
  Result := Buffer;
end;
{$ENDIF}

function SysFileUNCExpand(Dest,Name: PChar): PChar;

  procedure GetUNCPath(FileName: PChar);
  type
    PNetResourceArray = ^TNetResourceArray;
    TNetResourceArray = array[0..MaxInt div SizeOf(TNetResource) - 1] of TNetResource;
  var
    Done: Boolean;
    I,Count,Size: Integer;
    NetHandle: THandle;
    P,NetResources: PNetResource;
    RemoteNameInfo: array[0..1023] of Byte;
    Drive: char;
  begin
    if SysPlatform <> VER_PLATFORM_WIN32_WINDOWS then
      begin
        Size := SizeOf(RemoteNameInfo);
        if WNetGetUniversalName(FileName, UNIVERSAL_NAME_INFO_LEVEL,
          @RemoteNameInfo, Size) <> NO_ERROR then
          Exit;
        StrCopy(FileName, PRemoteNameInfo(@RemoteNameInfo).lpUniversalName);
      end
    else
      begin
      { The following works around a bug in WNetGetUniversalName under Windows 95 }
        Drive := UpCase(FileName[1]);
        if (Drive < 'A') or (Drive > 'Z') or (StrLen(FileName) < 3) or
          (FileName[1] <> ':') or (FileName[2] <> '\') then
          Exit;
        if WNetOpenEnum(RESOURCE_CONNECTED, RESOURCETYPE_DISK, 0, nil, NetHandle) <> NO_ERROR then
          Exit;
        Count := -1;
        if WNetEnumResource(NetHandle, Count, nil, Size) = ERROR_MORE_DATA then
        begin
          GetMem(NetResources, Size);
          Done := False;
          P := NetResources;
          repeat
            if WNetEnumResource(NetHandle, Count, P, Size) <> NO_ERROR then
              Break;
            I := 0;
            while I < Count do
            begin
              with P^ do
                if (lpLocalName <> nil) and (UpCase(FileName[0]) = UpCase(lpLocalName[0])) then
                begin
                  I := StrLen(lpRemoteName);
                  StrMove(@FileName[I], @FileName[2], MaxInt);
                  Move(lpRemoteName^, FileName^, I);
                  Done := True;
                  Break;
                end;
              Inc(I);
              Inc(P);
            end;
          until Done;
          FreeMem(NetResources);
        end;
        WNetCloseEnum(NetHandle);
      end;
  end;

begin
  {if SysPlatform = -1 then
    SysPlatform := SysPlatformID;}
  SysFileExpand(Dest, Name);
  if (UpCase(Dest[0]) in ['A'..'Z']) and (Dest[1] = ':') and (Dest[2] = '\') then
    GetUNCPath(Dest);
  Result := Dest;
end;

function SysGetSystemError(Code: Longint; Buffer: PChar; BufSize: Longint;var MsgLen: Longint): PChar;
begin
  MsgLen := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, Code, 0, Buffer, BufSize, nil);
  Result := Buffer;
end;

function GetLocaleStr(Locale,LocaleType: Integer; Default,Dest: PChar): PChar;
begin
  if GetLocaleInfo(Locale, LocaleType, Dest, 260) <= 0 then
    StrCopy(Dest, Default);
  Result := Dest;
end;

function GetLocaleChar(Locale, LocaleType: Integer; Default: Char): Char;
var
  Buffer: array[0..1] of Char;
begin
  if GetLocaleInfo(Locale, LocaleType, @Buffer, 2) > 0 then
    Result := Buffer[0] else
    Result := Default;
end;

function SysStrToIntDef(S: PChar; Default: Integer): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then
    Result := Default;
end;

{$IFDEF USESYSUTILS}
procedure SysGetCurrencyFormat(CString: PChar; var CFormat, CNegFormat, CDecimals: Byte; var CThousandSep, CDecimalSep: Char);
var
  DefaultLCID: LCID;
  Buffer: array[0..259] of Char;
begin
  DefaultLCID := GetThreadLocale;
  GetLocaleStr(DefaultLCID, LOCALE_SCURRENCY, '', CString);
  CFormat := SysStrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRENCY, '0', Buffer), 0);
  CNegFormat := SysStrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_INEGCURR, '0', Buffer), 0);
  CThousandSep := GetLocaleChar(DefaultLCID, LOCALE_STHOUSAND, ',');
  CDecimalSep := GetLocaleChar(DefaultLCID, LOCALE_SDECIMAL, '.');
  CDecimals := SysStrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRDIGITS, '0', Buffer), 0);
end;

procedure SysGetDateFormat(var DateSeparator: Char; ShortDateFormat, LongDateFormat: PChar);
var
  DefaultLCID: LCID;
begin
  DefaultLCID := GetThreadLocale;
  DateSeparator := GetLocaleChar(DefaultLCID, LOCALE_SDATE, '/');
  GetLocaleStr(DefaultLCID, LOCALE_SSHORTDATE, 'm/d/yy', ShortDateFormat);
  GetLocaleStr(DefaultLCID, LOCALE_SLONGDATE, 'mmmm d, yyyy', LongDateFormat);
end;

procedure SysGetTimeFormat(var TimeSeparator: Char; TimeAMString,TimePMString,ShortTimeFormat,LongTimeFormat: PChar);
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
  if SysStrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITLZERO, '0', Buffer), 0) = 0 then
    begin
      StrCopy(ShortTimeFormat, 'h:mm');
      StrCopy(LongTimeFormat, 'h:mm:ss');
    end
  else
    begin
      StrCopy(ShortTimeFormat, 'hh:mm');
      StrCopy(LongTimeFormat, 'hh:mm:ss');
    end;
  if SysStrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIME, '0', Buffer), 0) = 0 then
    TimePostfix := ' AMPM';
  StrCat(ShortTimeFormat, TimePostfix);
  StrCat(LongTimeFormat, TimePostfix);
end;
{$ENDIF}

function SysGetModuleName(var Address: Pointer; Buffer: PChar; BufSize: Longint): PChar;
var
  MemInfo: TMemoryBasicInformation;
  ModName: array[0..Max_Path] of Char;
begin
  VirtualQuery(Address, MemInfo, SizeOf(MemInfo));
  if (MemInfo.State <> mem_Commit) or
     (GetModuleFilename(THandle(MemInfo.AllocationBase), ModName, SizeOf(ModName)) = 0) then
    begin
      GetModuleFileName(HInstance, ModName, SizeOf(ModName));
      if Assigned(Address) then
        Dec(PChar(Address), $1000);
    end
  else
    Dec(PChar(Address), Longint(MemInfo.AllocationBase));
  StrLCopy(Buffer, StrRScan(ModName, '\') + 1, BufSize - 1);
  Result := Buffer;
end;

procedure SysDisplayConsoleError(PopupErrors: Boolean; Title, Msg: PChar);
var
  Count: Longint;
begin
  SysFileWrite(SysFileStdOut, Msg^, StrLen(Msg), Count);
end;

{$IFDEF USESYSUTILS}
procedure SysDisplayGUIError(Title, Msg: PChar);
begin
  MessageBox(0, Msg, Title, MB_OK or MB_IconStop or MB_TaskModal);
end;

procedure SysBeep;
begin
  MessageBeep(0);
end;
{$ENDIF}

procedure SysBeepEx(Freq,Dur: Longint);
begin
  Windows.Beep(Freq, Dur);
end;

function SysGetVolumeLabel(Drive: Char): ShortString;
const
  Root: Array[0..4] of char = 'C:\'#0;
var
  VolLabel: Array[0..256] of char;
  MaxLength: Longint;
  FSFlags: Longint;
begin
  Root[0] := Drive;
  if GetVolumeInformation(Root, VolLabel, Sizeof(VolLabel),
    nil, MaxLength, FSFlags, nil, 0) then
    Result := StrPas(VolLabel)
  else
    Result := '';
end;

function SysSetVolumeLabel(Drive: Char; _Label: ShortString): Boolean;
const
  Root: Array[0..4] of char = 'C:\'#0;
begin
  Root[0] := Drive;
  _Label[Length(_Label)+1] := #0;
  Result := SetVolumeLabel(Root, PChar(@_Label[1]));
end;

function SysGetForegroundProcessId: Longint;
var
  WHandle: Longint;
  ThreadID: Longint;
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
  Root: Array[0..4] of char = 'C:\'#0;
var
  FSName: Array[0..255] of char;
  MaxLength: Longint;
  FSFlags: Longint;
begin
  Root[0] := Drive;
  Result := dtInvalid;
{JO: ᭠砫� �஢��塞 ⨯ ���ன�⢠ �१ GetDriveType, � ⮫쪮 ��᫥ }
{    �⮣�, �᫨ ��������� �� ��� �� Removable, ���� ��� �஢����� �� }
  case GetDriveType(Root) of
    Drive_Fixed     : Result := dtHDFAT;
    Drive_Removable : Result := dtFloppy;
    Drive_CDRom     : Result := dtCDROM;
    Drive_Remote    : Result := dtLAN;
    0, 1            : Result := dtInvalid;
  else                Result := dtUnknown;
  end;
  if (Result <> dtFloppy) and (Result <> dtCDROM) then
    if GetVolumeInformation(Root, nil, 0, nil, MaxLength, FSFlags, FSName, sizeof(FSName)) then
      begin
        if StrLComp(FSName, 'FAT', 3) = 0 then
          Result := dtHDFAT
        else if StrComp(FSName, 'HPFS') = 0 then
          Result := dtHDHPFS
        else if StrComp(FSName, 'NTFS') = 0 then
          Result := dtHDNTFS
        else if StrLComp(FSName, 'CD', 2) = 0 then
          Result := dtCDROM
        else if StrComp(FSName, 'LAN') = 0 then
          Result := dtLan
        else if StrComp(FSName, 'NOVELL') = 0 then
          Result := dtNovellNet;
      end;
end;

function SysGetVideoModeInfo( Var Cols, Rows, Colours : Word ): Boolean;
var
  Buffer: TConsoleScreenBufferInfo;
begin
  SysTVInitCursor;
  GetConsoleScreenBufferInfo(SysConOut, Buffer);

  Cols := Buffer.dwSize.x;
  Rows := Buffer.dwSize.y;
  Colours := 16; //Buffer.wAttributes;
end;

function SysGetVisibleLines( var Top, Bottom: Longint ): Boolean;
var
  Buffer: TConsoleScreenBufferInfo;
begin
  SysTVInitCursor;
  GetConsoleScreenBufferInfo(SysConOut, Buffer);
  Top := Buffer.srwindow.top+1;
  Bottom := Buffer.srwindow.bottom+1;
  Result := True;
end;

function SysSetVideoMode( Cols, Rows: Word ): Boolean;
var
  Size: TCoord;
  R: TSmallRect;
  Res1: boolean;
  SrcSize: TSysPoint;
begin
  SysTVGetScrMode(@SrcSize);
  SysTVInitCursor;
  Size.X := Cols;
  Size.Y := Rows;
  Res1 := SetConsoleScreenBufferSize(SysConOut, Size);
    {��� �맮� ����� ���� ��㤠��, �᫨ ⥪�騩 ��࠭ ��
     ����頥��� � ���� ���� (�� 㬥��襭�� ࠧ��� ��࠭�).
     � �⮬ ��砥 ���� �㤥� �� ࠧ ᤥ���� ��⠭���� ࠧ���
     ���� ��᫥ ��⠭���� ࠧ��� ��࠭�.}
  R.Left   := 0;
  R.Top    := 0;
  R.Right  := Size.X - 1;
  R.Bottom := Size.Y - 1;
  Result := SetConsoleWindowInfo(SysConOut, True, R);
  if Result and not Res1 then
   begin
    Result := SetConsoleScreenBufferSize(SysConOut, Size);
    if not Result then
      begin
        R.Left   := 0;
        R.Top    := 0;
        R.Right  := SrcSize.X - 1;
        R.Bottom := SrcSize.Y - 1;
        SetConsoleWindowInfo(SysConOut, True, R);
      end;
   end;
end;

function SemCreateEvent(_Name: pChar; _Shared, _State: Boolean): TSemHandle;
var
  Security: TSecurityAttributes;
begin
  if _Shared then
    begin
      with Security do
        begin
          nLength := Sizeof(Security);
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
  Result := OpenEvent( Event_all_access, False, _Name);
  if Result = 0 then
    Result := -1;
end;

function SemPostEvent(_Handle: TSemhandle): Boolean;
begin
  Result := SetEvent(_Handle);
end;

function SemWaitEvent(_Handle: TSemHandle; _TimeOut: Longint): Boolean;
begin
  Result := WaitForSingleObject(_Handle, _TimeOut) = WAIT_OBJECT_0;
end;

function SemCreateMutex(_Name: PChar; _Shared, _State: Boolean): TSemHandle;
var
  Security: TSecurityAttributes;
begin
  if _Shared then
    begin
      with Security do
        begin
          nLength := Sizeof(Security);
          lpSecurityDescriptor := nil;
          bInheritHandle := True;
        end;
      Result := CreateMutex(@Security, _State, _Name);
    end
  else
    // Non-shared mutex does not require security descriptor
    Result := CreateMutex(nil, _State, _Name);
end;

function SemRequestMutex(_Handle: TSemHandle; _TimeOut: Longint): Boolean;
begin
  Result := WaitForSingleObject(_Handle, _TimeOut) = WAIT_OBJECT_0;
  if Result = False then
    _Handle := GetLastError;
end;

function SemAccessMutex(_Name: PChar): TSemHandle;
begin
  Result := OpenMutex( mutex_all_access, False, _Name);
  if Result = 0 then
    Result := -1;
end;

function SemReleaseMutex(_Handle: TSemHandle): Boolean;
begin
  Result := ReleaseMutex( _Handle );
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

function SysMemInfo(_Base: Pointer; _Size: Longint; var _Flags: Longint): Boolean;
var
  Buffer: TMemoryBasicInformation;
begin
  Result := VirtualQuery(_Base, Buffer, Sizeof(Buffer)) = Sizeof(Buffer);
  if Result then
    with Buffer do
     begin
       _Flags := 0;
       if Protect and (Page_ReadOnly or Page_ReadWrite or Page_Execute_Read) <> 0 then
         _Flags := _Flags or sysmem_Read or sysmem_Execute;
       if Protect and (Page_WriteCopy or Page_ReadWrite) <> 0 then
         _Flags := _Flags or sysmem_Write;
       if Protect and (Page_Execute or Page_Execute_Read or Page_Execute_ReadWrite) <> 0 then
         _Flags := _Flags or sysmem_Execute;
       if Protect and Page_Guard <> 0 then
         _Flags := _Flags or sysmem_Guard;
     end;
end;

function SysSetMemProtection(_Base: Pointer; _Size: Longint; _Flags: Longint): Boolean;
var
  Flags: Longint;
  Buffer: TMemoryBasicInformation;
begin
  VirtualQuery(_Base, Buffer, Sizeof(Buffer));
  if _Flags and sysmem_Execute <> 0 then
    if _Flags and sysmem_Read <> 0 then
      if _Flags and sysmem_Write <> 0 then
        Flags := page_Execute_ReadWrite
      else
        Flags := page_Execute_Read
    else
      if _Flags and sysmem_Write <> 0 then
        Flags := page_Execute_WriteCopy
      else
        Flags := page_Execute
  else
    if _Flags and sysmem_Read <> 0 then
      if _Flags and sysmem_Write <> 0 then
        Flags := page_ReadWrite
      else
        Flags := page_ReadOnly
    else
      if _Flags and sysmem_Write <> 0 then
        Flags := page_WriteCopy
      else
        Flags := page_NoAccess;
  Result := VirtualProtect(_Base, _Size, Flags, @Buffer);
end;

procedure SysMessageBox(_Msg, _Title: PChar; _Error: Boolean);
var
  Flag: Longint;
begin
  if _Error then
    Flag := mb_IconError
  else
    Flag := mb_IconInformation;
  MessageBox( 0, _Msg, _Title, Flag or mb_ApplModal);
end;

var
  ClipFormat: longint;

function SysClipCanPaste: Boolean;
var
  IsClipboardFormatAvailable: function(Format: UInt): Bool stdcall;
begin
  @IsClipboardFormatAvailable := QueryProcAddr('IsClipboardFormatAvailable', False);
  if Assigned(IsClipboardFormatAvailable) then
    Result := IsClipboardFormatAvailable(ClipFormat)
  else
    Result := False;
end;

function SysClipCopy(P: PChar; Size: Longint): Boolean;
var
  Q: pChar; {��� NT - LPWSTR}
  MSize: longint;
  MemHandle: HGlobal;
  OpenClipboard: function(Wnd: hWnd): Bool stdcall;
  EmptyClipboard: function: Bool stdcall;
  CloseClipboard: function: Bool stdcall;
  SetClipboardData: function(Format: UInt; Mem: THandle): THandle stdcall;
begin
  Result := False;
  @OpenClipboard := QueryProcAddr('OpenClipboard', False);
  @EmptyClipboard := QueryProcAddr('EmptyClipboard', False);
  @CloseClipboard := QueryProcAddr('CloseClipboard', False);
  @SetClipboardData := QueryProcAddr('SetClipboardData', False);
  if Assigned(OpenClipboard) and Assigned(EmptyClipboard) and
    Assigned(CloseClipboard) and Assigned(SetClipboardData) then
  begin
    // Open clipboard
    if OpenClipboard(0) then
    begin
      EmptyClipboard;
      // Allocate a shared block of memory
      MSize := Size+1;
      if SysPlatform <> 1{Win 9x} then
        MSize := 2*MSize; {for unicode string}
      MemHandle := GlobalAlloc(gmem_Moveable or gmem_DDEShare, MSize);
      Q := GlobalLock(MemHandle);
      Result := Q <> nil;
      if Result then
        begin // Copy clipboard data across
        if SysPlatform = 1{Win 9x} then
          move(P^, Q^, MSize)
        else
          begin // Copy clipboard data across and translate to unicode
          MSize := MultiByteToWideChar(CP_OEMCP, 0,
             P, -1,    LPWSTR(Q), Size+1);
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
��� ����᫥���饣� ���������� ���᪨� �㪢� �ॢ������ � ����ᨪ�
�� ��⠢�� � Delphi ��� MS VC ��� � ����ᠭ��� �� ��� �ணࠬ��.
����᭮, ����� ������ �� ������� ����� LOCALE_SYSTEM_DEFAULT?
}
      if Result and (SysPlatform <> 1) then
        begin
        MemHandle := GlobalAlloc(gmem_Moveable or gmem_DDEShare, 8);
        Q := GlobalLock(MemHandle);
        Result := Q <> nil;
        if Result then
          begin
          PDWORD(Q)^ := LOCALE_SYSTEM_DEFAULT;
          GlobalUnlock(MemHandle);
          Result := SetClipboardData(CF_LOCALE, MemHandle) <> 0;
          end;
        end;
    end;
    CloseClipboard;
  end;
end;

function SysClipPaste(var Size: Integer): Pointer;
var
  P: Pointer;
  ActualClipFormat: UINT;
  MemHandle: HGlobal;
  OpenClipboard: function(Wnd: hWnd): Bool stdcall;
  CloseClipboard: function: Bool stdcall;
  GetClipboardData: function(Format: UInt): THandle stdcall;
begin
  Result := nil;
  @OpenClipboard := QueryProcAddr('OpenClipboard', False);
  @CloseClipboard := QueryProcAddr('CloseClipboard', False);
  @GetClipboardData := QueryProcAddr('GetClipboardData', False);
  if Assigned(OpenClipboard) and Assigned(CloseClipboard)
    and Assigned(GetClipboardData) then
  begin
    if OpenClipboard(0) then
    begin
{AK155 14/05/2002
  ���������, �� ������� ᨬ���� (���ਬ��, �����) �ਢ� ��४���������
�� ANSI ��� � OEM, ��� � ���, �᫨ � ���� ����� ���� Delphi ���
MS VC ��� ����ᠭ��� �� ��� �ணࠬ��. ��, � ������ ��� ������ � ����
���� ����⮬ ��אַ ⥪�� ANSI, ⠪ �� �᫨, ��� ��������, �ᯮ�짮����
���� �� ⥪�⮢� �ଠ�, � ����砥��� ��ଠ�쭮. ���� ��ॡ��
�ଠ⮢ �㦥� ��� �ய�᪠ CF_LOCALE.
}
      ActualClipFormat := 0;
      while true do
      begin
        ActualClipFormat := EnumClipboardFormats(ActualClipFormat);
        case ActualClipFormat of
         CF_UNICODETEXT:
          break;
         CF_OEMTEXT:
          break;
         CF_TEXT:
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
          Size := StrLen(P) + 1;
          GetMem(Result, Size);
          Move(P^, Result^, Size);
          end;
         CF_TEXT:
          begin
          Size := StrLen(P) + 1;
          GetMem(Result, Size);
          CharToOem(P, Result);
          end;
        end {case};


      end;
      GlobalUnlock(MemHandle);
      CloseClipBoard;
    end;
  end;
end;

// Pharlap's TNT Embedded System support

function _malloc(Size: Longint): Pointer; cdecl; orgname;
begin
  GetMem(Result, Size);
end;

procedure _free(P: Pointer); cdecl; orgname;
begin
  FreeMem(P);
end;

// Retrieve various system settings, bitmapped:
// 0: Enhanced keyboard installed

function SysGetSystemSettings: Longint;
var
  KbdFlag: Longint;
begin
  Result := 0;
  KbdFlag := GetKeyboardType(0);
  if KbdFlag in [2, 4] then
    Result := Result OR 1;
end;

procedure Init;
var
  OSVersionInfo: TOSVersionInfo;
begin
  {Cat}
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  SysPlatform := OSVersionInfo.dwPlatformId;

  if ((OSVersionInfo.dwPlatformId = 1{Win9x}) and (OSVersionInfo.dwBuildNumber >= 1000)) {Cat:warn}
  or (OSVersionInfo.dwPlatformId = 2{WinNT}) then
    @GetDiskFreeSpaceEx := QueryProcAddr('GetDiskFreeSpaceExA', True)
  else
    @GetDiskFreeSpaceEx := nil;

  {WriteLn('Platform Id = ',OSVersionInfo.dwPlatformId,',  Build Number = ',OSVersionInfo.dwBuildNumber,',  GetDiskFreeSpaceExA loaded = ',Assigned(GetDiskFreeSpaceEx));}
  {/Cat}

  InitialiseConsole; {Cat}
  SysPlatform := SysPlatformID; {AK155 ������ �� ����᫮��� � � ����� ����}
  if SysPlatform = 1{Win 9x} then
    ClipFormat := cf_OemText
  else
    ClipFormat := cf_UnicodeText;
end;

begin {main}
  Init;
{end.} {in vpsyslow.pas}
