unit Proc_W32;
(******

Task List plugin
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{JO: 20-10-2002 - исправлено попадание kernel32.dll в список процессов под w9x }

{&Delphi+}
{&Use32+}

interface

uses
  Objects, Collect;

type
  PProcessItem = ^TProcessItem;
  TProcessItem = object(TObject)
    Name: ShortString;
    Pid: longInt;
    function GetString: String;
    end;

  PProcessCollection = ^TProcessCollection;
  TProcessCollection = object(TSortedCollection)
    function Compare(P1, P2: Pointer): integer; virtual;
    end;

function GetProcessList: PProcessCollection;
function ProcessKill(Pid: longInt): longInt;
function ProcessSwitch(Pid: longInt): longInt;

implementation

uses
  VpSysLow, Windows,
  DnIni, advance1, advance2;

function TProcessItem.GetString: String;

  function Hex8(A: longInt): Str8;
    const
      Hex: array[0..15] of Char =
      ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B',
        'C', 'D', 'E', 'F');
    begin
      Result := Hex[(A shr 28) and $F]+
      Hex[(A shr 24) and $F]+
      Hex[(A shr 20) and $F]+
      Hex[(A shr 16) and $F]+
      Hex[(A shr 12) and $F]+
      Hex[(A shr 08) and $F]+
      Hex[(A shr 04) and $F]+
      Hex[(A shr 00) and $F];
    end;

  begin
    if ShowExePaths then
      Result := Name
    else
      Result := GetName(Name);
    GetString := Hex8(Pid)+'  '+Result;
  end { TProcessItem.GetString: };

function TProcessCollection.Compare(P1, P2: Pointer): integer;
  begin
    Compare := PProcessItem(P1)^.Pid-PProcessItem(P2)^.Pid;
  end;

procedure FillWin95ProcessCollection(Collection: PProcessCollection);
  type
    PProcessEntry32 = ^TProcessEntry32;
    TProcessEntry32 = packed record
      dwSize: longInt;
      cntUsage: longInt;
      th32ProcessID: longInt;
      th32DefaultHeapID: longInt;
      th32ModuleID: longInt;
      cntThreads: longInt;
      th32ParentProcessID: longInt;
      pcPriClassBase: longInt;
      dwFlags: longInt;
      szExeFile: array[0..MAX_PATH-1] of Char;
      end;
  var
    hKernel: Handle;
    CreateToolhelp32Snapshot: function (dwFlags, th32ProcessID:
      longInt): Handle stdcall;
    Process32First: function (hSnapshot: Handle; lppe:
      PProcessEntry32): boolean stdcall;
    Process32Next: function (hSnapshot: Handle; lppe:
      PProcessEntry32): boolean stdcall;
    hSnapshot: Handle;
    Entry: TProcessEntry32;
    P: PProcessItem;
    i: byte;
  begin { FillWin95ProcessCollection }
    hKernel := GetModuleHandle('kernel32.dll');
    if hKernel = 0 then
      exit;
    @CreateToolhelp32Snapshot := GetProcAddress(hKernel,
      'CreateToolhelp32Snapshot');
    @Process32First := GetProcAddress(hKernel, 'Process32First');
    @Process32Next := GetProcAddress(hKernel, 'Process32Next');
    if (@CreateToolhelp32Snapshot = nil) or (@Process32First = nil) or (
        @Process32Next = nil)
    then
      exit;

    hSnapshot := CreateToolhelp32Snapshot(2 {TH32CS_SNAPPROCESS}, 0);
    if hSnapshot = invalid_Handle_Value then
      exit;

    Entry.dwSize := SizeOf(Entry);
    if not Process32First(hSnapshot, @Entry) then
      exit;

    {JO: нам совсем не нужно, чтобы kernel32.dll попадал в список процессов, }
    {    а посему repeat ... until здесь использовать не надо                }
    {repeat}
    while Process32Next(hSnapshot, @Entry) do
      begin
        P := New(PProcessItem, Init);
        if Entry.dwSize > SizeOf(Entry)-SizeOf(Entry.szExeFile) then
          with Entry do
            begin
              P^.Name := '';
              for i := 0 to 254 do
                if szExeFile[i] = #0 then
                  break
                else
                  P^.Name := P^.Name+szExeFile[i];
            end
          else
          P^.Name := '?'#0;
        P^.Pid := Entry.th32ProcessID;
        if not (FilteredList and (Pos('#'+UpStrg(GetName(P^.Name))+
            '#',
          '#'+UpStrg(UserTaskFilter)+'#') > 0))
        then
          Collection^.Insert(P)
        else
          Dispose(P, Done);
        Entry.dwSize := SizeOf(Entry);
      end;
    {until not Process32Next(hSnapshot, @Entry);}

    CloseHandle(hSnapshot);
  end { FillWin95ProcessCollection };

procedure FillWinNTProcessCollection(Collection: PProcessCollection);
  type
    PWideChar = ^WideChar;

    PThreadInfo = ^TThreadInfo;
    TThreadInfo = packed record
      ftCreationTime: TFileTime;
      dwUnknown1: longInt;
      dwStartAddress: longInt;
      dwOwningPID: longInt;
      dwThreadID: longInt;
      dwCurrentPriority: longInt;
      dwBasePriority: longInt;
      dwContextSwitches: longInt;
      dwThreadState: longInt;
      dwUnknown2: longInt;
      dwUnknown3: longInt;
      dwUnknown4: longInt;
      dwUnknown5: longInt;
      dwUnknown6: longInt;
      dwUnknown7: longInt;
      end;

    PProcessInfo = ^TProcessInfo;
    TProcessInfo = packed record
      dwOffset: longInt;
      dwThreadCount: longInt;
      dwUnkown1: array[1..6] of longInt;
      ftCreationTime: TDateTime;
      dwUnkown2: longInt;
      dwUnkown3: longInt;
      dwUnkown4: longInt;
      dwUnkown5: longInt;
      dwUnkown6: longInt;
      pszProcessName: PWideChar;
      dwBasePriority: longInt;
      dwProcessID: longInt;
      dwParentProcessID: longInt;
      dwHandleCount: longInt;
      dwUnkown7: longInt;
      dwUnkown8: longInt;
      dwVirtualBytesPeak: longInt;
      dwVirtualBytes: longInt;
      dwPageFaults: longInt;
      dwWorkingSetPeak: longInt;
      dwWorkingSet: longInt;
      dwUnkown9: longInt;
      dwPagedPool: longInt;
      dwUnkown10: longInt;
      dwNonPagedPool: longInt;
      dwPageFileBytesPeak: longInt;
      dwPageFileBytes: longInt;
      dwPrivateBytes: longInt;
      dwUnkown11: longInt;
      dwUnkown12: longInt;
      dwUnkown13: longInt;
      dwUnkown14: longInt;
      ati: array[1..1] of TThreadInfo;
      end;
  var
    hNTDll: Handle;
    NtQuerySystemInformation: function (Nmb: integer; Ptr: Pointer;
      Size1, Size2: integer): longInt stdcall;
    Buf: array[1..1024*512] of byte;
    PInfo: PProcessInfo;
    P: PProcessItem;
    i: byte;
  begin { FillWinNTProcessCollection }
    hNTDll := LoadLibrary('ntdll.dll');
    @NtQuerySystemInformation := GetProcAddress(hNTDll,
      'NtQuerySystemInformation');
    if @NtQuerySystemInformation <> nil then
      begin
        NtQuerySystemInformation(5, @Buf, SizeOf(Buf), 0);
        PInfo := @Buf;
        repeat
          P := New(PProcessItem, Init);
          WideCharToMultiByte(CP_OEMCP, 0, Pointer(PInfo^.
            pszProcessName), -1, @P^.Name[1], 255, nil, nil);
          for i := 1 to 255 do
            if P^.Name[i] = #0 then
              break;
          P^.Name[0] := Char(i);
          P^.Pid := PInfo^.dwProcessID;
          if not (FilteredList and (Pos('#'+UpStrg(GetName(P^.Name))+
              '#',
            '#'+UpStrg(UserTaskFilter)+'#') > 0))
          then
            Collection^.Insert(P)
          else
            Dispose(P, Done);
          if PInfo^.dwOffset = 0 then
            break;
          PInfo := PProcessInfo(PChar(PInfo)+PInfo^.dwOffset);
        until False;
      end;
    FreeLibrary(hNTDll);
  end { FillWinNTProcessCollection };

function GetProcessList: PProcessCollection;
  begin
    Result := New(PProcessCollection, Init(32, 16));
    if Result <> nil then
      if SysPlatformId = VER_PLATFORM_WIN32_NT then
        FillWinNTProcessCollection(Result)
      else
        FillWin95ProcessCollection(Result);
  end;

function ProcessKill(Pid: longInt): longInt;
  begin
    if TerminateProcess(OpenProcess(1, True, Pid), 1) then
      Result := 0
    else
      Result := 1;
  end;

function ProcessSwitch(Pid: longInt): longInt;
  begin
    Result := 0;
  end;

end.
