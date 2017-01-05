// |---------------------------------------------------------|
// |                                                         |
// |     Virtual Pascal Runtime Library.  Version 2.1.       |
// |     System Interface Layer for OS/2                     |
// |     ----------------------------------------------------|
// |     Copyright (C) 1995-2003 vpascal.com                 |
// |                                                         |
// |---------------------------------------------------------|

// PmWin and PmShApi function prototypes without Os2PmApi

// If CHECK_NO_PM is defined, the executable is bigger, the
// program temporarily uses more resources, but does not hang
// if PM-functions like SysClipCanPaste are called in an
// environment without Presentation Manager

// By default, we live with this overhead:
{$DEFINE CHECK_NO_PM}

type
  HIni = lHandle;
  tClipHack = (clipInit, clipFailed, clipOk);
  tPMInit = (pmUntested, pmOK, pmFailed);

const
  HIni_UserProfile = HIni(-1);
  hwnd_Desktop = HWND(1);
  mb_Ok = 0;
  mb_Information = $0030;
  mb_CUACritical = $0040;
  mb_Error = mb_CUACritical;
  mb_Moveable = $4000;
  wa_Error = 2;
  cf_Text = 1;
  cfi_Pointer = $0400;
  SIntl: PChar = 'PM_National';

  // State constants for PM and clipboard hack
  PM_Initialised: tPMInit = pmUntested;
  PM_ClipboardHack: tClipHack = clipInit;

const
  {&Cdecl+}
  PM_LoadString: function (AB: HAB; Module: hModule; Id: ULong;
    MaxLen: Long; Buffer: PChar): Long = nil;
  PM_CreateMsgQueue: function (AB: HAB; CMsg: Long): HMQ = nil;
  PM_Initialize: function (Options: ULong): HAB = nil;
  PM_MessageBox: function (Parent, Owner: HWND; text, Caption: PChar;
    IdWindow, Style: ULong): ULong = nil;
  PM_Alarm: function (Desktop: HWND; rgfType: ULong): Bool = nil;
  PM_PrfQueryProfileString: function (INI: HIni; APP, Key, Default:
    PChar; Buffer: Pointer; cchBufferMax: ULong): ULong = nil;
  PM_PrfQueryProfileInt: function (INI: HIni; APP, Key: PChar;
    Default: Long): Long = nil;
  PM_WinQueryClipbrdFmtInfo: function (AB: HAB; Fmt: ULong; var
    FmtInfo: ULong): Bool = nil;
  PM_WinOpenClipbrd: function (AB: HAB): Bool = nil;
  PM_WinCloseClipbrd: function (AB: HAB): Bool = nil;
  PM_WinSetClipbrdData: function (AB: HAB; Data, Fmt, rgfFmtInfo:
    ULong): Bool = nil;
  PM_WinQueryClipbrdData: function (AB: HAB; Fmt: ULong): ULong =
    nil;
  {&Cdecl-}
  // Module handles
  dll_PMWIN: hModule = 0;
  dll_PMSHAPI: hModule = 0;
  // Queue and Anchor block handles
  PM_MsgQueue: HMQ = 0;
  PM_Anchor: HAB = 0;

  { Initialise Win* and Prf* entry points, if Presentation Manager is available }

procedure FreePMModules;
  begin
    // Free modules
    if dll_PMWIN <> 0 then
      DosFreeModule(dll_PMWIN);
    if dll_PMSHAPI <> 0 then
      DosFreeModule(dll_PMSHAPI);
  end;

procedure InitPMModules;
  const
    {$Far16+}
    Dos16SMPresent: function (var present: SmallWord): apiret16 =
      nil;
    {$Far16-}
  var
    FailedModule: array[0..259] of Char;
    dll_DOSCALLS: hModule;
    sm_present: SmallWord;
  begin
    if PM_Initialised in [pmOK, pmFailed] then
      exit;

    // Full-screen session under PM ?
    if (SysCtrlSelfAppType = 0) then
      begin
        PM_Initialised := pmFailed;
        if DosLoadModule(FailedModule, SizeOf(FailedModule),
            'DOSCALLS', dll_DOSCALLS) = 0
        then
          begin

            sm_present := 0;

            if DosQueryProcAddr(dll_DOSCALLS, 712, nil,
                @Dos16SMPresent) = NO_ERROR
            then
              begin
                //if Dos16SMPresent(sm_present)=no_Error then
                //  if sm_present=1 then
                //    PM_Initialised:=pmOK;
                asm
                lea eax,sm_present
                push eax
                push [Dos16SMPresent]
                push 3 // B:0011: Addr
                call _Far16Pas

                cmp ax,no_Error
                jne @done

                cmp [sm_present], 1
                jne @done

                mov [PM_Initialised], pmOK
              @done:
              end
                  ;

              end;

            DosFreeModule(dll_DOSCALLS);
          end;

      end;

    // Do not hang on boot disk
    if PM_Initialised = pmFailed then
      exit;

    if DosLoadModule(FailedModule, SizeOf(FailedModule), 'PMWIN',
        dll_PMWIN) = 0
    then
      begin
        DosQueryProcAddr(dll_PMWIN, 781, nil, @PM_LoadString);
        DosQueryProcAddr(dll_PMWIN, 716, nil, @PM_CreateMsgQueue);
        DosQueryProcAddr(dll_PMWIN, 763, nil, @PM_Initialize);
        DosQueryProcAddr(dll_PMWIN, 789, nil, @PM_MessageBox);
        DosQueryProcAddr(dll_PMWIN, 701, nil, @PM_Alarm);
        DosQueryProcAddr(dll_PMWIN, 807, nil,
          @PM_WinQueryClipbrdFmtInfo);
        DosQueryProcAddr(dll_PMWIN, 793, nil, @PM_WinOpenClipbrd);
        DosQueryProcAddr(dll_PMWIN, 707, nil, @PM_WinCloseClipbrd);
        DosQueryProcAddr(dll_PMWIN, 854, nil, @PM_WinSetClipbrdData);
        DosQueryProcAddr(dll_PMWIN, 806, nil,
          @PM_WinQueryClipbrdData);
      end;
    if DosLoadModule(FailedModule, SizeOf(FailedModule), 'PMSHAPI',
        dll_PMSHAPI) = 0
    then
      begin
        DosQueryProcAddr(dll_PMSHAPI, 115, nil,
          @PM_PrfQueryProfileString);
        DosQueryProcAddr(dll_PMSHAPI, 114, nil,
          @PM_PrfQueryProfileInt);
      end;
    PM_Initialised := pmOK;
    AddExitProc(FreePMModules);
  end { InitPMModules };

function WinLoadString(AB: HAB; Module: hModule; Id: ULong; MaxLen:
    Long; Buffer: PChar): Long;
  var
    Stringtable_Resource: Pointer;
    Search: PChar;
  begin
    InitPMModules;
    if Assigned(PM_LoadString) then
      Result := PM_LoadString(AB, Module, Id, MaxLen, Buffer)
    else
      begin(* try to do the work in bootdisk mode *)
        if DosGetResource(Module, rt_String, Id div 16+1,
            Stringtable_Resource) = 0
        then
          begin
            Search := Stringtable_Resource;
            Inc(longInt(Search), SizeOf(SmallWord));
              (* skip/ignore codepage *)
            Id := Id mod 16; (* string number in this package *)
            while Id > 0 do
              begin
                Dec(Id); (* skip one string: Length byte+Length *)
                Inc(longInt(Search), 1+Ord(Search[0]));
              end;
            StrLCopy(Buffer, @Search[1], MaxLen);
              (* @Search[1] is #0 terminated *)
            DosFreeResource(Stringtable_Resource);
            Result := StrLen(Buffer);
          end
        else
          Result := 0; // Return string length 0
      end;
  end { WinLoadString };

function WinCreateMsgQueue(AB: HAB; CMsg: Long): HMQ;
  var
    TB: PTib;
    PB: PPib;
    org_Pib_ulType: ULong;
  begin
    if PM_MsgQueue <> NULLHANDLE then
      Result := PM_MsgQueue
    else
      begin
        InitPMModules;
        if Assigned(PM_CreateMsgQueue) then
          begin
            Result := PM_CreateMsgQueue(AB, CMsg);
            if (Result = NULLHANDLE) and (AB <> 0) and
              (PM_ClipboardHack = clipInit) and IsConsole
            then
              begin
                // Attempt to force OS/2 into believing we're a PM app
                // so we can create a message queue
                PM_ClipboardHack := clipFailed;
                DosGetInfoBlocks(TB, PB);

                // Save program type and override it as PM
                org_Pib_ulType := PB^.Pib_ulType;
                PB^.Pib_ulType := 3;

                // Create queue and restore the program type
                Result := PM_CreateMsgQueue(AB, CMsg);
                PB^.Pib_ulType := org_Pib_ulType;
                if Result <> NULLHANDLE then
                  PM_ClipboardHack := clipOk;
              end;
            PM_MsgQueue := Result;
          end
        else
          Result := $1051; // pmErr_Not_in_a_XSession
      end;
  end { WinCreateMsgQueue };

function WinInitialize(Options: ULong): HAB;
  begin
    Result := PM_Anchor;
    if Result = 0 then
      begin
        InitPMModules;
        if Assigned(PM_Initialize) then
          begin
            Result := PM_Initialize(Options);
            PM_Anchor := Result;
          end
        else
          Result := 0;
      end;
  end;

function WinMessageBox(Parent, Owner: HWND; text, Caption: PChar;
    IdWindow, Style: ULong): ULong;
  begin
    InitPMModules;
    if Assigned(PM_MessageBox) then
      Result := PM_MessageBox(Parent, Owner, text, Caption, IdWindow,
        Style)
    else
      Result := $FFFF; // mbid_Error
  end;

function WinQueryClipBrdFmtInfo(AB: HAB; Fmt: ULong; var FmtInfo:
    ULong): Bool;
  begin
    InitPMModules;
    if Assigned(PM_WinQueryClipbrdFmtInfo) then
      Result := PM_WinQueryClipbrdFmtInfo(AB, Fmt, FmtInfo)
    else
      Result := False;
  end;

function WinOpenClipBrd(AB: HAB): Bool;
  begin
    InitPMModules;
    if Assigned(PM_WinOpenClipbrd) then
      Result := PM_WinOpenClipbrd(AB)
    else
      Result := False;
  end;

function WinCloseClipBrd(AB: HAB): Bool;
  begin
    InitPMModules;
    if Assigned(PM_WinCloseClipbrd) then
      Result := PM_WinCloseClipbrd(AB)
    else
      Result := False;
  end;

function WinSetClipBrdData(AB: HAB; Data, Fmt, rgfFmtInfo: ULong):
    Bool;
  begin
    InitPMModules;
    if Assigned(PM_WinSetClipbrdData) then
      Result := PM_WinSetClipbrdData(AB, Data, Fmt, rgfFmtInfo)
    else
      Result := False;
  end;

function WinQueryClipBrdData(AB: HAB; Fmt: ULong): ULong;
  begin
    InitPMModules;
    if Assigned(PM_WinQueryClipbrdData) then
      Result := PM_WinQueryClipbrdData(AB, Fmt)
    else
      Result := 0;
  end;

function WinAlarm(Desktop: HWND; rgfType: ULong): Bool;
  begin
    InitPMModules;
    if Assigned(PM_Alarm) then
      Result := PM_Alarm(Desktop, rgfType)
    else
      Result := False;
  end;

function PrfQueryProfileInt(INI: HIni; APP, Key: PChar; Default:
    Long): Long;
  begin
    InitPMModules;
    if Assigned(PM_PrfQueryProfileInt) then
      Result := PM_PrfQueryProfileInt(INI, APP, Key, Default)
    else
      Result := Default;
  end;

function PrfQueryProfileString(INI: HIni; APP, Key, Default: PChar;
    Buffer: Pointer; cchBufferMax: ULong): ULong;
  begin
    InitPMModules;
    if Assigned(PM_PrfQueryProfileString) then
      Result := PM_PrfQueryProfileString(INI, APP, Key, Default,
        Buffer, cchBufferMax)
    else
      begin
        StrLCopy(Buffer, Default, cchBufferMax);
        Result := StrLen(Buffer)+1;
      end;
  end;

// Other non-Presentation Manager OS/2 functions

// Protect parameters of 16 bit functions to wrap around 64KB

function Invalid16Parm(const _p: Pointer; const _Length: longInt):
    boolean;
  begin
    Result := (longInt(_p) and $0000ffff)+_Length >= $00010000;
  end;

function Fix_64k(const _Memory: Pointer; const _Length: longInt):
    Pointer;
  begin
    // Test if memory crosses segment boundary
    if Invalid16Parm(_Memory, _Length) then
      // It does: Choose address in next segment
      Fix_64k := Ptr((Ofs(_memory) and $ffff0000)+$00010000)
    else
      // It doesn't: return original pointer
      Fix_64k := _Memory;
  end;

function SysFileStdIn: longInt;
  begin
    Result := 0;
  end;

function SysFileStdOut: longInt;
  begin
    Result := 1;
  end;

function SysFileStdErr: longInt;
  begin
    Result := 2;
  end;

function SysFileOpen_Create(Open: boolean; FileName: PChar; Mode,
    Attr, Action: longInt; var Handle: longInt): longInt;
  var
    APIFlags: longInt;
    ActionTaken: longInt;
  begin
    APIFlags := 0;
    if Open then
      if Action and open_CreateIfNew <> 0 then
        APIFlags := open_action_create_if_new or
          open_action_open_if_exists
      else if Action and open_TruncateIfExists <> 0 then
        APIFlags := open_action_fail_if_new or
          open_action_replace_if_exists
      else
        APIFlags := open_action_open_if_exists or
          open_action_fail_if_new
    else if Action and create_TruncateIfExists <> 0 then
      APIFlags := open_action_create_if_new or
        open_action_replace_if_exists
    else
      APIFlags := open_action_create_if_new or
        open_action_fail_if_exists;

    if (Mode and $70) = 0 then
      Inc(Mode, open_share_DenyNone);
    Result := DosOpen(FileName, Handle, ActionTaken, 0, 0, APIFlags,
      Mode, nil);
  end { SysFileOpen_Create };

function SysFileOpen(FileName: PChar; Mode: longInt; var Handle:
    longInt): longInt;
  var
    Action: longInt;
  begin
    if (Mode and $70) = 0 then
      Inc(Mode, open_share_DenyNone);
    Result := DosOpen(FileName, Handle, Action, 0, 0, file_Open,
      Mode, nil);
  end;

function SysFileCreate(FileName: PChar; Mode, Attr: longInt; var
    Handle: longInt): longInt;
  var
    Action: longInt;
  begin
    if (Mode and $70) = 0 then
      Inc(Mode, open_share_DenyNone);
    Result := DosOpen(FileName, Handle, Action, 0, Attr, file_Create+
      file_Truncate, Mode, nil);
  end;

function SysFileCopy(_Old, _New: PChar; _Overwrite: boolean):
    boolean;
  var
    Flag: longInt;
  begin
    if _Overwrite then
      Flag := dcpy_existing
    else
      Flag := 0;
    Result := (DosCopy(_Old, _New, Flag) = NO_ERROR);
  end;

function SysFileSeek(Handle, Distance, Method: longInt; var Actual:
    longInt): longInt;
  begin
    Result := DosSetFilePtr(Handle, Distance, Method, Actual);
  end;

function SysFileRead(Handle: longInt; var Buffer; Count: longInt;
    var Actual: longInt): longInt;
  begin
    Result := DosRead(Handle, Buffer, Count, Actual);
  end;

function SysFileWrite(Handle: longInt; const Buffer; Count: longInt;
    var Actual: longInt): longInt;
  begin
    Result := DosWrite(Handle, Buffer, Count, Actual);
  end;

function SysFileSetSize(Handle, NewSize: longInt): longInt;
  begin
    Result := DosSetFileSize(Handle, NewSize);
  end;

function SysFileClose(Handle: longInt): longInt;
  begin
    Result := 0;
    if (Handle > 2) or (Handle < 0) then
      Result := DosClose(Handle);
  end;

function SysFileFlushBuffers(Handle: longInt): longInt;
  begin
    Result := DosResetBuffer(Handle);
  end;

function SysFileDelete(FileName: PChar): longInt;
  begin
    Result := DosDelete(FileName);
  end;

function SysFileMove(OldName, NewName: PChar): longInt;
  begin
    Result := DosMove(OldName, NewName);
  end;

function SysFileIsDevice(Handle: longInt): longInt;
  var
    HandleType, Flags: longInt;
  begin
    if DosQueryHType(Handle, HandleType, Flags) <> 0 then
      Result := 0
    else
      Result := HandleType; // 0=File, 1=Character device, 2=Pipe
  end;

function SysDirGetCurrent(Drive: longInt; Path: PChar): longInt;
  var
    P: PChar;
    X: longInt;
  begin
    if Drive = 0 then
      DosQueryCurrentDisk(Drive, X);
    P := Path;
    P^:= Chr(Drive+(Ord('A')-1));
    Inc(P);
    P^:= ':';
    Inc(P);
    P^:= '\';
    Inc(P);
    X := 260-4; // 4: 'D:\', #0
    Result := DosQueryCurrentDir(Drive, P^, X);
  end;

function SysDirSetCurrent(Path: PChar): longInt;
  var
    P: PChar;
  begin
    P := Path;
    Result := 0;
    if P^ <> #0 then
      begin
        if P[1] = ':' then
          begin
            Result := DosSetDefaultDisk((Ord(P^) and $DF)-(Ord('A')-1));
            if Result <> 0 then
              exit;
            Inc(P, 2);
            if P^ = #0 then// "D:",0  ?
              exit; // yes, exit
          end;
        Result := DosSetCurrentDir(P);
      end;
  end;

function SysDirCreate(Path: PChar): longInt;
  begin
    Result := DosCreateDir(Path, nil);
  end;

function SysDirDelete(Path: PChar): longInt;
  begin
    Result := DosDeleteDir(Path);
  end;

// from vputils.pas
function Min(A, B: longInt): longInt;
  inline;
  begin
    if A < B then
      Min := A
    else
      Min := B;
  end;

function Max(A, B: longInt): longInt;
  inline;
  begin
    if A > B then
      Max := A
    else
      Max := B;
  end;

function SysMemAvail: longInt;
  var
    meminfo:
    packed record
      TotPhysMem,
      TotResMem,
      TotAvailMem,
      MaxPrMem: ULong;
      end;

  begin
    // qsv_TotAvailMem is unreliable like:
    // qsv_TotAvailMem=-6MB, qsv_MaxPrMem=316MB
    DosQuerySysInfo(qsv_TotPhysMem, qsv_MaxPrMem, meminfo, SizeOf(
      meminfo));
    with meminfo do
      begin
        if MaxPrMem < 0 then
          MaxPrMem := High(longInt);
        // this formula does not thinks about swapfile memory, but
        // assumes that ther should be 0.5 MB available for it,
        // if address space (MaxPrMem) is availabe.
        Result := Min(Max(TotPhysMem-TotResMem, 512*1024), MaxPrMem);
      end;
  end { SysMemAvail: };

function SysMemAlloc(Size, Flags: longInt; var MemPtr: Pointer):
    longInt;
  begin
    Result := DosAllocMem(MemPtr, Size, Flags);
  end;

function SysMemFree(MemPtr: Pointer): longInt;
  begin
    Result := DosFreeMem(MemPtr);
  end;

function SysSysMsCount: longInt;
  begin
    DosQuerySysInfo(qsv_Ms_Count, qsv_Ms_Count, Result, SizeOf(
      Result));
  end;

procedure SysSysSelToFlat(var P: Pointer); {&USES ebx} {&FRAME-}
asm
        mov     ebx,P
        mov     eax,[ebx]
        Call    DosSelToFlat
        mov     [ebx],eax
end
  ;

procedure SysSysFlatToSel(var P: Pointer); {&USES ebx} {&FRAME-}
asm
        mov     ebx,P
        mov     eax,[ebx]
        Call    DosFlatToSel
        mov     [ebx],eax
end
  ;

function SysCtrlSelfAppType: longInt;
  var
    TB: PTib;
    PB: PPib;
  begin
    DosGetInfoBlocks(TB, PB);
    Result := PB^.Pib_ulType;
  end;

function SysGetThreadId: longInt;
  var
    TB: PTib;
    PB: PPib;
  begin
    DosGetInfoBlocks(TB, PB);
    Result := TB^.tib_ordinal;
  end;

function SysCtrlCreateThread(Attrs: Pointer; StackSize: longInt;
    func, Param: Pointer; Flags: longInt; var Tid: longInt): longInt;
  begin
    Result := DosCreateThread(Tid, FnThread(func), longInt(Param),
      Flags, StackSize);
    if Result <> 0 then
      Tid := 0;
  end;

function SysCtrlKillThread(Handle: longInt): longInt;
  begin
    Result := DosKillThread(Handle);
  end;

function SysCtrlSuspendThread(Handle: longInt): longInt;
  begin
    Result := DosSuspendThread(Handle);
  end;

function SysCtrlResumeThread(Handle: longInt): longInt;
  begin
    Result := DosResumeThread(Handle);
  end;

procedure SysCtrlExitThread(ExitCode: longInt);
  begin
    DosExit(exit_Thread, ExitCode);
  end;

procedure SysCtrlExitProcess(ExitCode: longInt);
  begin
    DosExit(exit_Process, ExitCode);
  end;

function SysCtrlGetModuleName(Handle: longInt; Buffer: PChar):
    longInt;
  begin
    Result := DosQueryModuleName(0, 260, Buffer);
  end;

procedure SysCtrlEnterCritSec;
  begin
    DosEnterCritSec;
  end;

procedure SysCtrlLeaveCritSec;
  begin
    DosExitCritSec;
  end;

function GetParamStr(P: PChar; var Param: String): PChar;
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

function SysCmdlnCount: longInt;
  var
    P: PChar;
    s: String;
  begin
    P := SysCmdln;
    Result := -1;
    repeat
      P := GetParamStr(P, s);
      if s = '' then
        begin
          if Result < 0 then
            Result := 0;
          exit;
        end;
      Inc(Result);
      if Result = 0 then// Skip the first #0
        Inc(P);
    until False;
  end;

procedure SysCmdlnParam(Index: longInt; var Param: ShortString);
  var
    P: PChar;
    len: integer;
  begin
    P := SysCmdln;
    if Index = 0 then
      begin
        len := 0;
        Dec(P, 2);
        while P^ <> #0 do
          begin
            Dec(P);
            Inc(len);
          end;
        SetString(Param, P+1, len);
      end
    else
      begin
        P := GetParamStr(P, Param);
        Inc(P);
        Dec(Index);
        repeat
          P := GetParamStr(P, Param);
          if (Index = 0) or (Param = '') then
            exit;
          Dec(Index);
        until False;
      end;
  end { SysCmdlnParam };

function SysCmdln: PChar;
  var
    TB: PTib;
    PB: PPib;
  begin
    DosGetInfoBlocks(TB, PB);
    Result := PB^.Pib_pchCmd;
  end;

function SysGetProcessId: longInt;
  var
    TB: PTib;
    PB: PPib;
  begin
    DosGetInfoBlocks(TB, PB);
    Result := PB^.Pib_ulPid;
  end;

function SysCtrlGetTlsMapMem: Pointer;
  var
    TB: PTib;
    PB: PPib;
    SharedMemName: record
      l0: longInt;
      l1: longInt;
      L2: longInt;
      Id: array[0..11] of Char;
      end;
  begin
    DosGetInfoBlocks(TB, PB);
    SharedMemName.l0 := Ord('\')+Ord('S') shl 8+Ord('H') shl 16+Ord(
      'A') shl 24;
    SharedMemName.l1 := Ord('R')+Ord('E') shl 8+Ord('M') shl 16+Ord(
      'E') shl 24;
    {$IFDEF B243}
    SharedMemName.L2 := Ord('M')+Ord('\') shl 8+Ord('V') shl 16+Ord(
      'R') shl 24;
    {$ELSE}
    SharedMemName.L2 := Ord('M')+Ord('\') shl 8+Ord('V') shl 16+Ord(
      'S') shl 24;
    {$ENDIF}
    Str(PB^.Pib_ulPid, SharedMemName.Id);
    if DosGetNamedSharedMem(Result, PChar(@SharedMemName), pag_Read+
        pag_Write) <> 0
    then
      begin
        DosAllocSharedMem(Result, PChar(@SharedMemName),
          SharedMemSize, pag_Read+pag_Write+pag_Commit);
        FillChar(Result^, SharedMemSize, $FF);
        FillChar(Result^, SizeOf(TSharedMem), 0);
        // Set up pointers to functions to use when allocating memory
        System.GetMemoryManager(PSharedMem(Result)^.TlsMemMgr);
      end;
  end { SysCtrlGetTlsMapMem: };

function SysGetEnvironment: PChar;
  var
    TB: PTib;
    PB: PPib;
  begin
    DosGetInfoBlocks(TB, PB);
    Result := PB^.Pib_pchEnv;
  end;

procedure SysFreeEnvironment(_Env: PChar);
  begin
    // Nothing; the environment does not need freeing
  end;

function SysOsVersion: longInt;
  var
    Version: array[0..1] of longInt;
  begin
    DosQuerySysInfo(qsv_Version_Major, qsv_Version_Minor, Version,
      SizeOf(Version));
    Result := Version[0]+Version[1] shl 8;
  end;

function SysPlatformId: longInt;
  begin
    Result := -1; // -1 = OS/2
  end;

procedure SysGetDateTime(Year, Month, Day, DayOfWeek, Hour, Minute,
    Second, MSec: PLongInt);
  var
    DT: Os2Base.DateTime;
  begin
    DosGetDateTime(DT);
    if Year <> nil then
      Year^:= DT.Year;
    if Month <> nil then
      Month^:= DT.Month;
    if Day <> nil then
      Day^:= DT.Day;
    if DayOfWeek <> nil then
      DayOfWeek^:= DT.WeekDay;
    if Hour <> nil then
      Hour^:= DT.Hours;
    if Minute <> nil then
      Minute^:= DT.Minutes;
    if Second <> nil then
      Second^:= DT.Seconds;
    if MSec <> nil then
      MSec^:= DT.Hundredths*10;
  end { SysGetDateTime };

procedure SysSetDateTime(Year, Month, Day, Hour, Minute, Second,
    MSec: PLongInt);
  var
    DT: Os2Base.DateTime;
  begin
    DosGetDateTime(DT);
    if Year <> nil then
      DT.Year := Year^;
    if Month <> nil then
      DT.Month := Month^;
    if Day <> nil then
      DT.Day := Day^;
    if Hour <> nil then
      DT.Hours := Hour^;
    if Minute <> nil then
      DT.Minutes := Minute^;
    if Second <> nil then
      DT.Seconds := Second^;
    if MSec <> nil then
      DT.Hundredths := MSec^ div 10;
    DosSetDateTime(DT);
  end { SysSetDateTime };

function SysVerify(SetValue: boolean; Value: boolean): boolean;
  var
    Flag: Longbool;
  begin
    if SetValue then
      Result := DosSetVerify(Value) = 0
    else
      begin
        DosQueryVerify(Flag);
        Result := Flag;
      end;
  end;

function SysDiskFreeLong(Drive: byte): TQuad;
  var
    Info: FsAllocate;
  begin
    if DosQueryFSInfo(Drive, fsil_Alloc, Info, SizeOf(Info)) = 0
    then
      Result := 1.0*Info.cUnitAvail*Info.cSectorUnit*Info.cbSector
    else
      Result := -1;
  end;

function SysDiskSizeLong(Drive: byte): TQuad;
  var
    Info: FsAllocate;
  begin
    if DosQueryFSInfo(Drive, fsil_Alloc, Info, SizeOf(Info)) = 0
    then
      Result := 1.0*Info.cUnit*Info.cSectorUnit*Info.cbSector
    else
      Result := -1;
  end;

{Cat: пока что просто заглушки, потом надо переделать для поддержки сетевых путей}
function SysDiskFreeLongX(Path: PChar): TQuad;
  begin
    Result := SysDiskFreeLong(byte(UpCase(Path^))-byte('A')+1);
  end;

function SysDiskSizeLongX(Path: PChar): TQuad;
  begin
    Result := SysDiskSizeLong(byte(UpCase(Path^))-byte('A')+1);
  end;
{/Cat}

function SysGetFileAttr(FileName: PChar; var Attr: longInt): longInt;
  var
    Info: FileStatus3;
  begin
    Attr := 0;
    Result := DosQueryPathInfo(FileName, fil_Standard, Info, SizeOf(
      Info));
    if Result = 0 then
      Attr := Info.attrFile;
  end;

function SysSetFileAttr(FileName: PChar; Attr: longInt): longInt;
  var
    Info: FileStatus3;
  begin
    Result := DosQueryPathInfo(FileName, fil_Standard, Info, SizeOf(
      Info));
    if Result = 0 then
      begin
        Info.attrFile := Attr;
        Result := DosSetPathInfo(FileName, fil_Standard, Info,
          SizeOf(Info), dspi_WrtThru);
      end;
  end;

function SysGetFileTime(Handle: longInt; var Time: longInt): longInt;
  var
    Info: FileStatus3;
    FDateTime: TDateTimeRec absolute Time;
  begin
    Time := 0;
    Result := DosQueryFileInfo(Handle, fil_Standard, Info, SizeOf(
      Info));
    if Result = 0 then
      with FDateTime do
        begin
          FTime := Info.ftimeLastWrite;
          FDate := Info.fdateLastWrite;
        end
  end;

function SysSetFileTime(Handle: longInt; Time: longInt): longInt;
  var
    Info: FileStatus3;
    FDateTime: TDateTimeRec absolute Time;
  begin
    Result := DosQueryFileInfo(Handle, fil_Standard, Info, SizeOf(
      Info));
    if Result = 0 then
      with FDateTime do
        begin
          Info.ftimeLastWrite := FTime;
          Info.fdateLastWrite := FDate;
          Result := DosSetFileInfo(Handle, fil_Standard, Info,
            SizeOf(Info));
        end;
  end;

function SysFindFirst(Path: PChar; Attr: longInt; var F:
    TOSSearchRec; IsPChar: boolean): longInt;
  var
    Count: longInt;
    SR: FileFindBuf3;
    Path2: array[0..259] of Char;
  begin
    Attr := Attr and not $8; // No VolumeID under OS/2
    Count := 1;
    F.Handle := hdir_Create;
    Result := DosFindFirst(Path, F.Handle, Attr, SR, SizeOf(SR),
      Count, fil_Standard);

    // If a specific error occurs, and the call is to look for directories, and
    // the path is a UNC name, then retry
    if (Result = msg_Net_Dev_Type_Invalid) and
      (Hi(Attr) = $10) and
      (StrLen(Path) > Length('\\')) and
      (StrLComp(Path, '\\', Length('\\')) = 0)
    then
      begin
        DosFindClose(F.Handle);
        StrCat(StrCopy(Path2, Path), '\*.*');
        Result := DosFindFirst(Path2, F.Handle, Attr, SR, SizeOf(SR),
          Count, fil_Standard);
        if (Result = 0) and (Count <> 0) then
          Result := 0;
      end;

    if Result = 0 then
      with F, SR do
        begin
          Attr := attrFile;
          TDateTimeRec(Time).FTime := ftimeLastWrite;
          TDateTimeRec(Time).FDate := fdateLastWrite;
          Size := cbFile;
          if IsPChar then
            StrPCopy(PChar(@Name), achName)
          else
            Name := achName;
        end
      else
      F.Handle := hdir_Create;
  end { SysFindFirst };

function SysFindNext(var F: TOSSearchRec; IsPChar: boolean): longInt;
  var
    Count: longInt;
    SR: FileFindBuf3;
  begin
    Count := 1;
    Result := DosFindNext(F.Handle, SR, SizeOf(SR), Count);
    if Result = 0 then
      with F, SR do
        begin
          Attr := attrFile;
          TDateTimeRec(Time).FTime := ftimeLastWrite;
          TDateTimeRec(Time).FDate := fdateLastWrite;
          Size := cbFile;
          if IsPChar then
            StrPCopy(PChar(@Name), achName)
          else
            Name := achName;
        end;
  end;

function SysFindClose(var F: TOSSearchRec): longInt;
  begin
    if F.Handle = hdir_Create then
      Result := 0
    else
      Result := DosFindClose(F.Handle);
  end;

// Check if file exists; if it does, update FileName parameter
// to include correct case of existing file
function SysFileAsOS(FileName: PChar): boolean;
  var
    SRec: TOSSearchRec;
    P: PChar;
  begin
    Result := False;
    if SysFindFirst(FileName, $37, SRec, False) = 0 then
      begin
        if SRec.Name[1] <> #0 then
          begin
            // Replace '/' with '\'
            repeat
              P := StrRScan(FileName, '/');
              if P = nil then
                break;
              P[0] := '\';
            until False;

            // Replace filename part with data returned by OS
            P := StrRScan(FileName, '\');
            if P = nil then
              P := FileName
            else
              Inc(P); // Point to first character of file name
            StrPCopy(P, SRec.Name);
          end;
        SysFindClose(SRec);
        Result := True;
      end;
  end { SysFileAsOS };

function SysFileSearch(Dest, Name, List: PChar): PChar;
  var
    Info: FileStatus3;
  begin
    if (DosQueryPathInfo(Name, fil_Standard, Info, SizeOf(Info)) = 0)
      and ((Info.attrFile and file_Directory) = 0)
    then
      SysFileExpand(Dest, Name)
    else if DosSearchPath(dsp_ImpliedCur+dsp_IgnoreNetErr, List,
        Name, Dest, 260) <> 0
    then
      Dest[0] := #0;
    SysFileAsOS(Dest);
    Result := Dest;
  end;

function SysFileExpand(Dest, Name: PChar): PChar;
  var
    i, j, l: integer;
    C: Char;
    CurDir: array[0..259] of Char;

  procedure AdjustPath;
    begin
      if (Dest[j-2] = '\') and (Dest[j-1] = '.') then
        Dec(j, 2)
      else if (j > 3) and (Dest[j-3] = '\') and (Dest[j-2] = '.')
          and (Dest[j-1] = '.')
      then
        begin
          Dec(j, 3);
          if Dest[j-1] <> ':' then
            repeat
              Dec(j);
            until Dest[j] = '\';
        end;
    end;

  begin { SysFileExpand }// SysFileExpand
    l := StrLen(Name);
    if (l >= Length('\\')) and (Name[0] = '\') and (Name[1] = '\')
    then
      StrCopy(Dest, Name) // '\\SERVER\DIR'
    else if (l >= Length('X:')) and (Name[1] = ':') then
      begin// Path is already in form 'X:\Path' or 'X:/Path'
        if (l >= Length('X:\')) and (Name[2] in ['\', '/']) then
          StrCopy(Dest, Name)
        else if SysDirGetCurrent(Ord(UpCase(Name[0]))-(Ord('A')-1),
          CurDir) = 0
        then
          begin// Path is in form 'X:Path'
            if StrLen(CurDir) > Length('X:\') then
              StrCat(CurDir, '\');
            StrLCat(StrCopy(Dest, CurDir), @Name[2], 259);
          end
        else
          StrCopy(Dest, Name); {AK155} // SysDirGetCurrent fail
      end
    else
      begin// Path is without drive letter
        SysDirGetCurrent(0, CurDir);
          // Get default drive & directory
        if StrLen(CurDir) > Length('X:\') then
          StrCat(CurDir, '\');
        if Name[0] in ['\', '/'] then
          StrLCopy(Dest, @CurDir[0], 2) // 'X:' only
        else
          StrCopy(Dest, CurDir);
        StrLCat(Dest, Name, 259);
      end;

    j := 0;
    for i := 0 to StrLen(Dest)-1 do
      begin
        C := Dest[i];
        if C = '/' then
          begin
            C := '\';
            Dest[i] := C;
          end;
        if C = '\' then
          AdjustPath;
        Dest[j] := C;
        Inc(j);
      end;
    AdjustPath;
    if Dest[j-1] = ':' then
      begin
        Dest[j] := '\';
        Inc(j);
      end;
    Dest[j] := #0;
    Result := Dest;
  end { SysFileExpand };

threadvar
  ExecResult: ResultCodes;
  LastAsync: boolean;

function SysExecute(Path, CmdLine, Env: PChar; Async: boolean; Pid:
    PLongInt; StdIn, StdOut, StdErr: longInt): longInt;
  var
    P, Os2CmdLine: PChar;
    i, ExecFlags: longInt;
    FailedObj: array[0..259] of Char;
    CmdLineBuf: array[0..1024*8-1] of Char;
    StdHandles: array[0..2] of longInt;
    NewHandles: array[0..2] of longInt;
    OldHandles: array[0..2] of longInt;
  begin
    StdHandles[0] := StdIn;
    StdHandles[1] := StdOut;
    StdHandles[2] := StdErr;
    LastAsync := Async;
    ExecFlags := exec_Sync;
    if Async then
      ExecFlags := exec_AsyncResult;
    Os2CmdLine := CmdLineBuf;
    // Work around a bug in OS/2: Argument to DosExecPgm should not cross 64K boundary
    if ((longInt(Os2CmdLine)+1024) and $FFFF) < 1024 then
      Inc(Os2CmdLine, 1024);
    P := StrECopy(Os2CmdLine, Path); // 'Path'#0
    P := StrECopy(P+1, CmdLine); // 'Path'#0'CommandLine'#0
    P[1] := #0; // 'Path'#0'CommandLine'#0#0
    for i := 0 to 2 do
      if StdHandles[i] <> -1 then
        begin
          OldHandles[i] := $FFFFFFFF;
            // Save original StdIn to OldIn
          NewHandles[i] := i;
          DosDupHandle(NewHandles[i], OldHandles[i]);
          DosDupHandle(StdHandles[i], NewHandles[i]);
        end;
    Result := DosExecPgm(FailedObj, SizeOf(FailedObj), ExecFlags,
      Os2CmdLine,
    Env, ExecResult, Path);
    for i := 0 to 2 do
      if StdHandles[i] <> -1 then
        begin
          DosDupHandle(OldHandles[i], NewHandles[i]);
          SysFileClose(OldHandles[i]);
        end;
    if Async and (Pid <> nil) then
      Pid^:= ExecResult.codeTerminate;
  end { SysExecute };

function SysExitCode: longInt;
  var
    RetPid: longInt;
  begin
    if LastAsync then
      DosWaitChild(dcwa_Process, dcww_Wait, ExecResult, RetPid,
        ExecResult.codeTerminate);
    Result := ExecResult.codeResult;
    if ExecResult.codeTerminate <> tc_Exit then
      Result := -1;
  end;

type
  TCharCaseTable = array[0..255] of Char;
var
  UpperCaseTable: TCharCaseTable;
  LowerCaseTable: TCharCaseTable;
  AnsiUpperCaseTable: TCharCaseTable;
  AnsiLowerCaseTable: TCharCaseTable;
  WeightTable: TCharCaseTable;
const
  CaseTablesInitialized: boolean = False;

procedure InitCaseTables;
  var
    i, j: integer;
  begin
    for i := 0 to 255 do
      begin
        UpperCaseTable[i] := Chr(i);
        LowerCaseTable[i] := Chr(i);
        AnsiUpperCaseTable[i] := Chr(i);
        AnsiLowerCaseTable[i] := Chr(i);
        if i in [Ord('A')..Ord('Z')] then
          LowerCaseTable[i] := Chr(i+(Ord('a')-Ord('A')));
        if i in [Ord('a')..Ord('z')] then
          UpperCaseTable[i] := Chr(i-(Ord('a')-Ord('A')));
      end;
    SysGetCaseMap(SizeOf(AnsiUpperCaseTable), AnsiUpperCaseTable);
    for i := 255 downto 0 do
      begin
        j := Ord(AnsiUpperCaseTable[i]);
        if (j <> i) {and (AnsiLowerCaseTable[J] <> chr(J))} then
          AnsiLowerCaseTable[j] := Chr(i);
      end;
    SysGetWeightTable(SizeOf(WeightTable), WeightTable);
    CaseTablesInitialized := True;
  end { InitCaseTables };

procedure ConvertCase(s1, s2: PChar; Count: integer; var Table:
    TCharCaseTable); {&USES esi,edi} {&FRAME-}
asm
                cmp     CaseTablesInitialized,0
                jne     @@1
                Call    InitCaseTables
              @@1:
                xor     eax,eax
                mov     esi,S1
                mov     edi,S2
                mov     ecx,Count
                mov     edx,Table
                jecxz   @@3
              @@2:
                dec     ecx
                mov     al,[esi+ecx]
                mov     al,[edx+eax]
                mov     [edi+ecx],al
                jnz     @@2
              @@3:
end
  ;

procedure SysChangeCase(Source, Dest: PChar; len: longInt; NewCase:
    TCharCase);
  begin
    case NewCase of
      ccLower:
        ConvertCase(Source, Dest, len, LowerCaseTable);
      ccUpper:
        ConvertCase(Source, Dest, len, UpperCaseTable);
      ccAnsiLower:
        ConvertCase(Source, Dest, len, AnsiLowerCaseTable);
      ccAnsiUpper:
        ConvertCase(Source, Dest, len, AnsiUpperCaseTable);
    end {case};
  end;

function SysLowerCase(s: PChar): PChar;
  begin
    ConvertCase(s, s, StrLen(s), AnsiLowerCaseTable);
    Result := s;
  end;

function SysUpperCase(s: PChar): PChar;
  begin
    ConvertCase(s, s, StrLen(s), AnsiUpperCaseTable);
    Result := s;
  end;

function MemComp(P1, P2: Pointer; l1, L2: integer; T1, T2: PChar):
    integer; {&USES ebx,esi,edi,ebp} {&FRAME-}
asm
                cmp     CaseTablesInitialized,0
                jne     @@0
                Call    InitCaseTables
              @@0:
                mov     ecx,L1
                mov     eax,L2
                mov     esi,P1
                mov     edi,P2
                cmp     ecx,eax
                jbe     @@1
                mov     ecx,eax
              @@1:
                mov     ebx,T1
                mov     ebp,T2
                xor     eax,eax
                xor     edx,edx
                test    ecx,ecx
                jz      @@5
              @@2:
                mov     al,[esi]
                mov     dl,[edi]
                inc     esi
                inc     edi
                test    ebp,ebp
                mov     al,[ebx+eax]    // Table1
                mov     dl,[ebx+edx]
                jz      @@3
                mov     al,[ebp+eax]    // Table2
                mov     dl,[ebp+edx]
              @@3:
                cmp     al,dl
                jne     @@RET
                dec     ecx
                jnz     @@2
              @@5:
                mov     eax,L1
                mov     edx,L2
              @@RET:
                sub     eax,edx
end
  ;

function SysCompareStrings(s1, s2: PChar; l1, L2: longInt;
    IgnoreCase: boolean): longInt;
  begin
    if IgnoreCase then
      Result := MemComp(s1, s2, l1, L2, @WeightTable, nil)
    else
      Result := MemComp(s1, s2, l1, L2, @AnsiUpperCaseTable,
        @WeightTable);
  end;

procedure SysGetCaseMap(TblLen: longInt; Tbl: PChar);
  var
    CC: CountryCode;
  begin
    CC.Country := 0; // Use default
    CC.CodePage := 0;
    DosMapCase(TblLen, CC, Tbl);
  end;

procedure SysGetWeightTable(TblLen: longInt; WeightTable: PChar);
  var
    CC: CountryCode;
    DataLen: longInt;
  begin
    CC.Country := 0; // Use default
    CC.CodePage := 0;
    DosQueryCollate(TblLen, CC, WeightTable, DataLen);
  end;

function SysGetCodePage: longInt;
  var
    Returned: longInt;
    CC: CountryCode;
    CI: CountryInfo;
  begin
    Result := 0;
    DosQueryCp(SizeOf(Result), Result, Returned);
    if Result = 0 then
      begin
        CC.Country := 0;
        CC.CodePage := 0;
        DosQueryCtryInfo(SizeOf(CountryInfo), CC, CI, Returned);
        Result := CI.CodePage;
      end;
  end;

var
  PrevXcptProc: Pointer = Ptr(-1);

function SignalHandler(Report: PExceptionReportRecord;
  Registration: PExceptionRegistrationRecord;
  Context: PContextRecord;
  P: Pointer): longInt;
  cdecl;
  begin
    Result := xcpt_Continue_Search;
    if Report^.ExceptionNum = xcpt_Signal then
      case Report^.ExceptionInfo[0] of
        xcpt_Signal_Intr, xcpt_Signal_Break:
          if Assigned(CtrlBreakHandler) then
            if CtrlBreakHandler then
              Result := xcpt_Continue_Execution
      end {case};
    XcptProc := PrevXcptProc;
  end;

procedure SysCtrlSetCBreakHandler;
  var
    Times: longInt;
  begin
    DosSetSignalExceptionFocus(True, Times);
    if PrevXcptProc = Ptr(-1) then
      begin
        PrevXcptProc := XcptProc;
        XcptProc := @SignalHandler;
      end;
  end;

function SysFileIncHandleCount(Count: longInt): longInt;
  var
    hDelta, hMax: longInt;
  begin
    hDelta := Count;
    Result := DosSetRelMaxFH(hDelta, hMax);
  end;

const
  CrtScanCode: byte = 0;

function SysKeyPressed: boolean;
  var
    Key: ^KbdKeyInfo;
    LKey: array[1..2] of KbdKeyInfo;
  begin
    Key := Fix_64k(@LKey, SizeOf(Key^));
    KbdPeek(Key^, 0);
    Result := (CrtScanCode <> 0) or ((Key^.fbStatus and
      kbdtrf_Final_Char_In) <> 0);
  end;

procedure SysFlushKeyBuf;
  begin
    CrtScanCode := 0;
  end;

function SysPeekKey(var Ch: Char): boolean;
  var
    ChData: ^KbdKeyInfo;
    LChData: array[1..2] of KbdKeyInfo;
  begin
    ChData := Fix_64k(@LChData, SizeOf(ChData^));
    KbdPeek(ChData^, 0);
    if ChData^.fbStatus and kbdtrf_Final_Char_In <> 0 then
      begin
        Ch := ChData^.ChChar;
        Result := True;
      end
    else
      Result := False;
  end;

function SysReadKey: Char;
  var
    Key: ^KbdKeyInfo;
    LKey: array[1..2] of KbdKeyInfo;
  begin
    if CrtScanCode <> 0 then
      begin
        Result := Chr(CrtScanCode);
        CrtScanCode := 0;
      end
    else
      begin
        Key := Fix_64k(@LKey, SizeOf(Key^));
        KbdCharIn(Key^, io_Wait, 0);
        case Key^.ChChar of
          #0:
            CrtScanCode := Key^.chScan;
          #$E0:
              {   Up, Dn, Left Rt Ins Del Home End PgUp PgDn C-Home C-End C-PgUp C-PgDn C-Left C-Right C-Up C-Dn }
            if Key^.chScan in [$48, $50, $4B, $4D, $52, $53, $47,
                $4F, $49, $51, $77, $75, $84, $76, $73, $74, $8D,
                $91]
            then
              begin
                CrtScanCode := Key.chScan;
                Key^.ChChar := #0;
              end;
        end {case};
        Result := Key^.ChChar;
      end;
  end { SysReadKey: };

procedure SysGetCurPos(var X, Y: SmallWord);
  begin
    VioGetCurPos(Y, X, TVVioHandle);
  end;

procedure SysSetCurPos(X, Y: SmallWord);
  begin
    VioSetCurPos(Y, X, TVVioHandle);
  end;

procedure SysWrtCharStrAtt(CharStr: Pointer; len, X, Y: SmallWord;
    var Attr: byte);
  var
    pGood: Pointer;
    pTemp: Pointer;
  begin
    if Invalid16Parm(CharStr, len) then
      begin
        GetMem(pTemp, 2*len);
        pGood := Fix_64k(pTemp, len);
        Move(CharStr^, pGood^, len);
        VioWrtCharStrAtt(pGood, len, Y, X, Attr, TVVioHandle);
        FreeMem(pTemp);
      end
    else
      VioWrtCharStrAtt(CharStr, len, Y, X, Attr, TVVioHandle);
  end;

function SysReadAttributesAt(X, Y: SmallWord): byte;
  var
    Cell, Size: SmallWord;
  begin
    Size := SizeOf(Cell);
    VioReadCellStr(Cell, Size, Y, X, 0);
    Result := Hi(Cell); // and $7f;
  end;

function SysReadCharAt(X, Y: SmallWord): Char;
  var
    Cell, Size: SmallWord;
  begin
    Size := SizeOf(Cell);
    if VioReadCellStr(Cell, Size, Y, X, 0) = 0 then
      Result := Chr(Lo(Cell))
    else
      Result := #0;
  end;

procedure SysScrollUp(X1, Y1, X2, Y2, Lines, Cell: SmallWord);
  begin
    VioScrollUp(Y1, X1, Y2, X2, Lines, Cell, TVVioHandle);
  end;

procedure SysScrollDn(X1, Y1, X2, Y2, Lines, Cell: SmallWord);
  begin
    VioScrollDn(Y1, X1, Y2, X2, Lines, Cell, TVVioHandle);
  end;

const
  MouseHandle: SmallWord = $FFFF;
var
  ProtectArea: NoPtrRect;
  MouseEventMask: SmallWord;
  MouseMSec: longInt;
  ButtonCount: longInt;

function SysTVDetectMouse: longInt;
  var
    MouLoc: PtrLoc;
    Buttons: SmallWord;
  begin
    if MouOpen(nil, MouseHandle) = 0 then
      begin
        MouGetNumButtons(Buttons, MouseHandle);
        ButtonCount := Buttons;
        if not NoMouseMove then
          begin
            MouLoc.Row := 0;
            MouLoc.Col := 0;
            MouSetPtrPos(MouLoc, MouseHandle);
          end;
        Result := Buttons;
      end
    else
      Result := 0;
  end;

procedure SysTVInitMouse(var X, Y: integer);
  var
    MouLoc: PtrLoc;
    EventMask: SmallWord;
  begin
    if MouseHandle <> $FFFF then
      begin
        MouGetPtrPos(MouLoc, MouseHandle);
        X := MouLoc.Col;
        Y := MouLoc.Row;
        MouDrawPtr(MouseHandle);
        MouGetEventMask(MouseEventMask, MouseHandle);
        EventMask := $FFFF;
        MouSetEventMask(EventMask, MouseHandle);
          // Select all events
      end;
  end;

procedure SysTVDoneMouse(Close: boolean);
  begin
    if MouseHandle <> $FFFF then
      begin
        if Close then
          MouClose(MouseHandle)
        else
          begin
            SysTVHideMouse; // Restore events to original state
            MouSetEventMask(MouseEventMask, MouseHandle);
          end;
      end;
  end;

procedure SysTVShowMouse;
  begin
    if MouseHandle <> $FFFF then
      MouDrawPtr(MouseHandle);
  end;

procedure SysTVHideMouse;
  begin
    // Assume that ProtectArea does not wrap around segment boundary
    if MouseHandle <> $FFFF then
      MouRemovePtr(ProtectArea, MouseHandle);
  end;

procedure SysTVUpdateMouseWhere(var X, Y: integer);
  var
    MouLoc: PtrLoc;
    MSec: longInt;
  begin
    MSec := SysSysMsCount;
    if MSec-MouseMSec >= 5 then
      begin
        MouseMSec := MSec;
        MouGetPtrPos(MouLoc, MouseHandle);
        X := MouLoc.Col;
        Y := MouLoc.Row;
      end;
  end;

function SysTVGetMouseEvent(var Event: TSysMouseEvent): boolean;
  var
    MouEvent: ^MouEventInfo;
    MouQInfo: ^MouQueInfo;
    LMouEvent: array[1..2] of MouEventInfo;
    LMouQInfo: array[1..2] of MouQueInfo;
  const
    WaitFlag: SmallWord = mou_NoWait;
  begin
    MouQInfo := Fix_64k(@LMouQInfo, SizeOf(MouQInfo^));

    MouGetNumQueEl(MouQInfo^, MouseHandle);
    if MouQInfo^.cEvents = 0 then
      Result := False
    else
      begin
        MouEvent := Fix_64k(@LMouEvent, SizeOf(MouEvent^));
        MouReadEventQue(MouEvent^, WaitFlag, MouseHandle);
        with Event do
          begin
            smeTime := MouEvent^.Time;
            MouseMSec := MouEvent^.Time;
            smeButtons := 0;
            if (MouEvent^.fs and (mouse_Motion_With_Bn1_Down or
                mouse_Bn1_Down)) <> 0
            then
              Inc(smeButtons, $0001);
            if (MouEvent^.fs and (mouse_Motion_With_Bn2_Down or
                mouse_Bn2_Down)) <> 0
            then
              Inc(smeButtons, $0002);
            smePos.X := MouEvent^.Col;
            smePos.Y := MouEvent^.Row;
          end;
        Result := True;
      end;
  end { SysTVGetMouseEvent };

procedure SysTVKbdInit;
  var
    Key: ^KbdInfo;
    LKey: array[1..2] of KbdInfo;

  begin
    Key := Fix_64k(@LKey, SizeOf(Key^));
    Key^.cb := SizeOf(KbdInfo);
    KbdGetStatus(Key^, 0);
      { Disable ASCII & Enable raw (binary) mode}
    Key^.fsMask := (Key^.fsMask and (not keyboard_Ascii_Mode)) or
      keyboard_Binary_Mode;
    KbdSetStatus(Key^, 0);
  end;

function SysTVGetPeekKeyEvent(var Event: TSysKeyEvent; _Peek:
    boolean): boolean;
  var
    Key: ^KbdKeyInfo;
    LKey: array[1..2] of KbdKeyInfo;
  begin
    Key := Fix_64k(@LKey, SizeOf(Key^));
    if _Peek then
      KbdPeek(Key^, 0)
    else
      KbdCharIn(Key^, io_NoWait, 0);
    if (Key^.fbStatus and kbdtrf_Final_Char_In) = 0 then
      Result := False
    else
      with Event do// Key is ready
        begin
          skeKeyCode := Ord(Key^.ChChar)+Key^.chScan shl 8;
          skeShiftState := Lo(Key^.fsState);
          Result := True;
        end;
  end { SysTVGetPeekKeyEvent };

function SysTVGetKeyEvent(var Event: TSysKeyEvent): boolean;
  begin
    Result := SysTVGetPeekKeyEvent(Event, False);
  end;

function SysTVPeekKeyEvent(var Event: TSysKeyEvent): boolean;
  begin
    Result := SysTVGetPeekKeyEvent(Event, True);
  end;

function SysTVGetShiftState: byte;
  var
    Key: ^KbdInfo;
    LKey: array[1..2] of KbdInfo;

  begin
    Key := Fix_64k(@LKey, SizeOf(Key^));
    Key^.cb := SizeOf(KbdInfo);
    KbdGetStatus(Key^, 0);
    Result := Lo(Key^.fsState);
  end;

procedure SysTVSetCurPos(X, Y: integer);
  begin
    VioSetCurPos(Y, X, TVVioHandle);
  end;

procedure SysTVSetCurType(Y1, Y2: integer; Show: boolean);
  var
    CurData: ^VioCursorInfo;
    LCurData: array[1..2] of VioCursorInfo;
  begin
    CurData := Fix_64k(@LCurData, SizeOf(CurData^));
    with CurData^ do
      begin
        yStart := Y1;
        cend := Y2;
        cX := 1;
        if Show then
          Attr := 0
        else
          begin
            Attr := $FFFF;
            yStart := 0;
            cend := 1;
          end;
      end;
    VioSetCurType(CurData^, TVVioHandle);
  end { SysTVSetCurType };

procedure SysTVGetCurType(var Y1, Y2: integer; var Visible: boolean);
  var
    CurData: ^VioCursorInfo;
    LCurData: array[1..2] of VioCursorInfo;
  begin
    CurData := Fix_64k(@LCurData, SizeOf(CurData^));
    VioGetCurType(CurData^, TVVioHandle);
    Visible := CurData^.Attr <> $FFFF;
    Y1 := CurData^.yStart;
    Y2 := CurData^.cend;
  end;

procedure SysTvShowBuf(Pos, Size: integer);
  begin
    VioShowBuf(Pos, Size, TVVioHandle);
  end;

procedure SysTVClrScr;
  const
    Cell: SmallWord = $0720; // Space character, white on black
  begin
    VioScrollUp(0, 0, 65535, 65535, 65535, Cell, TVVioHandle);
    SysTVSetCurPos(0, 0);
  end;

procedure SetMouseArea(X, Y: integer);
  begin
    ProtectArea.Row := 0;
    ProtectArea.Col := 0;
    ProtectArea.cRow := Y-1;
    ProtectArea.cCol := X-1;
  end;

function SysTvGetScrMode(_Size: PSysPoint; _Align: boolean): integer;
  var
    VioMode: ^VioModeInfo;
    LVioMode: array[1..2] of VioModeInfo;
  begin
    VioMode := Fix_64k(@LVioMode, SizeOf(VioMode^));
    VioMode^.cb := SizeOf(VioMode^);
    if VioGetMode(VioMode^, TVVioHandle) <> 0 then
      Result := $FF // smNonStandard
    else
      begin
        with VioMode^ do
          begin
            if (fbType and vgmt_DisableBurst) = 0 then
              Result := 3 // smCO80
            else
              Result := 2; // smBW80;
            if Color = 0 then
              Result := 7; // smMono
            case Row of
              25:
                ;
              43, 50:
                Inc(Result, $0100); // smFont8x8
              else
                Result := $FF; // smNonStandard
            end {case};
            if (VioMode^.fbType and vgmt_Graphics) <> 0 then
              Result := 0;
          end;
        SetMouseArea(VioMode^.Col, VioMode^.Row);
        if _Size <> nil then
          with _Size^ do
            begin
              X := VioMode^.Col;
              Y := VioMode^.Row;
            end;
      end;
  end { SysTvGetScrMode };

procedure SysTVSetScrMode(Mode: integer);
  var
    BiosMode: byte;
    VioMode: ^VioModeInfo;
    VideoConfig: ^VioConfigInfo;
    LVioMode: array[1..2] of VioModeInfo;
    LVideoConfig: array[1..2] of VioConfigInfo;
  begin
    VioMode := Fix_64k(@LVioMode, SizeOf(VioMode^));
    VideoConfig := Fix_64k(@LVideoConfig, SizeOf(VideoConfig^));

    BiosMode := Lo(Mode);
    VideoConfig^.cb := SizeOf(VideoConfig^);
    VioGetConfig(0, VideoConfig^, TVVioHandle);

    with VioMode^ do
      begin
        // Indicate that we only filled important Entrys
        // the Video handler will find the best values itself
        cb := Ofs(HRes)-Ofs(cb);

        case Lo(Mode) of
          0, 2:
            fbType := vgmt_Other+vgmt_DisableBurst;
          7:
            fbType := 0;
          else
            fbType := vgmt_Other;
        end {case};

        if Lo(Mode) = 7 then
          Color := 0
        else
          Color := colors_16; // Color

        Row := 25;

        if Lo(Mode) < 2 then
          Col := 40
        else
          Col := 80;

        case VideoConfig^.Adapter of
          display_Monochrome..display_CGA:
            ; // only 25 Lines
          display_EGA:
            if Hi(Mode) = 1 then// font 8x8
              Row := 43; // 350/8=43
          else// VGA
            if Hi(Mode) = 1 then// font 8x8
              Row := 50; // 400/8=25
        end {case};
      end;

    SetMouseArea(VioMode^.Col, VioMode^.Row);
    VioSetMode(VioMode^, TVVioHandle);
  end { SysTVSetScrMode };

function SysTVGetSrcBuf: Pointer;
  var
    BufSize: SmallWord;
  begin
    VioGetBuf(Result, BufSize, TVVioHandle);
    SelToFlat(Result);
  end;

procedure SysTVInitCursor;
  var
    Font: ^VioFontInfo;
    LFont: array[1..2] of VioFontInfo;
  begin
    Font := Fix_64k(@LFont, SizeOf(Font^));

    FillChar(Font^, SizeOf(Font^), 0);
    Font^.cb := SizeOf(VioFontInfo);
    Font^.RType := vgfi_GetCurFont;
    // Set underline cursor to avoid cursor shape problems
    if VioGetFont(Font^, TVVioHandle) = 0 then
      SysTVSetCurType(Font^.cyCell-2, Font^.cyCell-1, True);
  end;

procedure SysTvDoneCursor;
  begin
  end;

procedure SysCtrlSleep(Delay: integer);
  begin
    DosSleep(Delay);
  end;

function SysGetValidDrives: longInt;
  var
    CurDrive: longInt;
  begin
    if DosQueryCurrentDisk(CurDrive, Result) <> 0 then
      Result := 0;
  end;

procedure SysDisableHardErrors;
  begin
    DOSError(ferr_DisableHardErr);
  end;

function SysKillProcess(Process: longInt): longInt;
  begin
    Result := DosKillProcess(dkp_ProcessTree, Process);
  end;

// Copy _Name to _Os2Name, prefixing \SHAREMEM\ if necessary
procedure MakeSharedMemName(_Os2Name, _Name: PChar);
  const
    Os2ShareMemPrefix: PChar = '\SHAREMEM\';
  begin
    _Os2Name^:= #0;
    if StrLComp(_Name, Os2ShareMemPrefix, 10) <> 0 then
      StrCopy(_Os2Name, Os2ShareMemPrefix);
    StrCat(_Os2Name, _Name);
  end;

function SysAllocSharedMemory(var _Base: Pointer; _Name: PChar;
    _Size: longInt): longInt;
  var
    Name: array[0..512] of Char;
  begin
    MakeSharedMemName(@Name, _Name);
    if DosAllocSharedMem(_Base, Name, _Size, obj_Giveable+pag_Read+
        pag_Write+pag_Commit) = 0
    then
      Result := 0 // OS/2 does not use handles; 0 is its "handle"
    else
      Result := -1; // Error
  end;

function SysAccessSharedMemory(var _Base: Pointer; _Name: PChar):
    longInt;
  var
    Name: array[0..512] of Char;
  begin
    MakeSharedMemName(@Name, _Name);
    if DosGetNamedSharedMem(_Base, _Name, obj_Giveable+pag_Read+
        pag_Write+pag_Commit) = 0
    then
      Result := 0
    else
      Result := -1; //Error
  end;

procedure SysFreeSharedMemory(_Base: Pointer; _Handle: longInt);
  begin
    DosFreeMem(_Base);
  end;

function SysAllocSharedMem(Size: longInt; var MemPtr: Pointer):
    longInt;
  begin
    Result := DosAllocSharedMem(MemPtr, nil, Size, obj_Giveable+
      pag_Read+pag_Write+pag_Commit);
  end;

function SysGiveSharedMem(MemPtr: Pointer): longInt;
  var
    PB: PPib;
    TB: PTib;
  begin
    DosGetInfoBlocks(TB, PB);
    Result := DosGiveSharedMem(MemPtr, PB^.Pib_ulPPid, pag_Read+
      pag_Write);
  end;

function SysPipeCreate(var ReadHandle, WriteHandle: longInt; Size:
    longInt): longInt;
  var
    PipeName: array[0..259] of Char;
    Number: array[0..10] of Char;
  begin
    StrCopy(PipeName, '\PIPE\');
    Str(SysSysMsCount, Number);
    StrCopy(@PipeName[6], Number);
    Result := DosCreateNPipe(PipeName, ReadHandle, np_Access_InBound,
      np_NoWait+1, 0, Size, 0);
    DosConnectNPipe(ReadHandle);
    SysFileOpen(PipeName, $41, WriteHandle);
  end;

function SysPipePeek(Pipe: longInt; Buffer: Pointer; BufSize:
    longInt; var BytesRead: longInt; var IsClosing: boolean):
    longInt;
  var
    State: longInt;
    Avail: AvailData;
  begin
    Result := DosPeekNPipe(Pipe, Buffer^, BufSize, BytesRead, Avail,
      State);
    IsClosing := State = np_State_Closing;
  end;

function SysPipeClose(Pipe: longInt): longInt;
  begin
    Result := SysFileClose(Pipe);
  end;

function SysLoadResourceString(Id: longInt; Buffer: PChar; BufSize:
    longInt): PChar;
  begin
    Buffer[0] := #0;
    WinLoadString(0, 0, Id, BufSize, Buffer);
    Result := Buffer;
  end;

function SysFileUNCExpand(Dest, Name: PChar): PChar;
  var
    P: PChar;
    len: longInt;
    pfsqb: pFSQBuffer2;
    DevName: array[0..2] of Char;
    Drive: String;
  begin
    SysFileExpand(Dest, Name);
    if (UpCase(Dest[0]) in ['A'..'Z']) and (Dest[1] = ':') and (Dest[2]
        = '\')
    then
      begin
        DevName[0] := Dest[0];
        DevName[1] := ':';
        DevName[2] := #0;
        len := 1024;
        GetMem(pfsqb, len);
        FillChar(pfsqb^, len, 0);
        DosQueryFSAttach(DevName, 1, fsail_QueryName, pfsqb, len);
        P := pfsqb^.szName; // Points to name of entry
        P := P+StrLen(P)+1;
          // Points to name of FS (LAN, NETWARE, etc)
        P := P+StrLen(P)+1; // Points to UNC name
        if (P^ = '\') and ((P+1)^ = '\') then
            // Filter out results that do not start with '\\'
          begin
            len := StrLen(P);
            StrCopy(@Dest[len], @Dest[2]);
            Move(P^, Dest^, len);
          end;
        FreeMem(pfsqb);
      end;
    Result := Dest;
  end { SysFileUNCExpand };

function SysGetSystemError(Code: longInt; Buffer: PChar; BufSize:
    longInt; var MsgLen: longInt): PChar;
  begin
    Result := Buffer;
    if DosGetMessage(nil, 0, Buffer, BufSize-1, Code, 'OSO001.MSG',
        MsgLen) <> 0
    then
      MsgLen := 0;
  end;

function SysGetProfileStr(Section, Entry, Default, Dest: PChar):
    PChar;
  begin
    Dest[0] := #0;
    PrfQueryProfileString(HIni_UserProfile, Section, Entry, Default,
      Dest, 260);
    Result := Dest;
  end;

function SysGetProfileChar(Section, Entry: PChar; Default: Char):
    Char;
  var
    CDefault, Buffer: array[0..1] of Char;
  begin
    CDefault[0] := Default;
    CDefault[1] := #0;
    PrfQueryProfileString(HIni_UserProfile, Section, Entry, CDefault,
      @Buffer, SizeOf(Buffer));
    Result := Buffer[0];
  end;

procedure SysGetCurrencyFormat(CString: PChar; var CFormat,
    CNegFormat, CDecimals: byte; var CThousandSep, CDecimalSep: Char);
  begin
    SysGetProfileStr(SIntl, 'sCurrency', '', CString);
    CFormat := PrfQueryProfileInt(HIni_UserProfile, SIntl,
      'iCurrency', 0);
    CNegFormat := 0; { N/A under PM }
    CThousandSep := SysGetProfileChar(SIntl, 'sThousand', ',');
    CDecimalSep := SysGetProfileChar(SIntl, 'sDecimal', '.');
    CDecimals := PrfQueryProfileInt(HIni_UserProfile, SIntl,
      'iDigits', 2);
  end;

procedure SysGetDateFormat(var DateSeparator: Char; ShortDateFormat,
    LongDateFormat: PChar);
  var
    Date: integer;
  const
    DateStr: array[0..2] of PChar =
    ('mm/dd/yy', 'dd/mm/yy', 'yy/mm/dd');
    LongDateStr: array[0..2] of PChar =
    ('mmmm d, yyyy', 'dd mmmm yyyy', 'yyyy mmmm d');
  begin
    DateSeparator := SysGetProfileChar(SIntl, 'sDate', '/');
    Date := PrfQueryProfileInt(HIni_UserProfile, SIntl, 'iDate', 0);
    if Date > 2 then
      Date := 0;
    StrCopy(ShortDateFormat, DateStr[Date]);
      // No exact equivalent under PM
    StrCopy(LongDateFormat, LongDateStr[Date]);
  end;

procedure SysGetTimeFormat(var TimeSeparator: Char; TimeAMString,
    TimePMString, ShortTimeFormat, LongTimeFormat: PChar);
  var
    TimePostfix: PChar;
  const
    SIntl: PChar = 'PM_National';
  begin
    TimeSeparator := SysGetProfileChar(SIntl, 'sTime', ':');
    SysGetProfileStr(SIntl, 's1159', 'am', TimeAMString);
    SysGetProfileStr(SIntl, 's2359', 'pm', TimePMString);
    if PrfQueryProfileInt(HIni_UserProfile, SIntl, 'iLzero', 0) = 0
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
    TimePostfix := '';
    if PrfQueryProfileInt(HIni_UserProfile, SIntl, 'iTime', 0) = 0
    then
      TimePostfix := ' AMPM';
    StrCat(ShortTimeFormat, TimePostfix);
    StrCat(LongTimeFormat, TimePostfix);
  end { SysGetTimeFormat };

function SysGetModuleName(var Address: Pointer; Buffer: PChar;
    BufSize: longInt): PChar;
  var
    ModuleName: array[0..259] of Char;
  begin
    DosQueryModuleName(ModuleHandle, SizeOf(ModuleName), ModuleName);
    StrLCopy(Buffer, StrRScan(ModuleName, '\')+1, BufSize-1);
    Result := Buffer;
  end;

procedure SysDisplayConsoleError(PopupErrors: boolean; Title, Msg:
    PChar);
  var
    PopupFlags: SmallWord;
    Info: ^KbdKeyInfo;
    LInfo: array[1..2] of KbdKeyInfo;
    Count: longInt;
    pTemp: PChar;
    pGood: PChar;
  type
    WordRec = packed record
      Lo, Hi: byte;
      end;
  const
    Cell: SmallWord = $4F20;
  begin
    if PopupErrors then
      begin
        PopupFlags := 1;
        VioPopup(PopupFlags, 0);
        VioScrollUp(0, 0, 127, 127, 127, Cell, 0);
        VioSetCurPos(12, 0, 0);
        Count := StrLen(Msg);
        if Invalid16Parm(Msg, Count) then
          begin
            GetMem(pTemp, 2*Count);
            pGood := Fix_64k(pTemp, Count);
            Move(Msg^, pGood^, Count);
            VioWrtTTy(pGood, Count, 0);
            FreeMem(pTemp);
          end
        else
          VioWrtTTy(@Msg, Count, 0);
        VioWrtNAttr(WordRec(Cell).Hi, 2000, 0, 0, 0);
        Info := Fix_64k(@LInfo, SizeOf(Info^));
        if KbdCharIn(Info^, io_Wait, 0) <> 0 then
          // Wait if kbd call fails.  It does so when exception is
          // caused by Ctrl-Brk or Ctrl-C.
          DosSleep(5000);
        VioEndPopUp(0);
      end
    else
      DosWrite(1, Msg^, StrLen(Msg), Count);
  end { SysDisplayConsoleError };

procedure SysDisplayGUIError(Title, Msg: PChar);
  begin
    WinCreateMsgQueue(WinInitialize(0), 0);
    WinMessageBox(hwnd_Desktop, hwnd_Desktop, Msg, Title, 0,
      mb_Error+mb_Moveable);
  end;

procedure SysBeep;
  begin
    WinAlarm(hwnd_Desktop, wa_Error);
  end;

procedure SysBeepEx(Freq, Dur: longInt);
  begin
    DosBeep(Freq, Dur);
  end;

function SysGetVolumeLabel(Drive: Char): ShortString;
  var
    rc: longInt;
    DriveNumber: word;
    Buf: record
      SerialNum: word;
      VolLabel: String[12];
      end;

  begin
    DriveNumber := Ord(UpCase(Drive))-Ord('A')+1;

    rc := DosQueryFSInfo(DriveNumber, fsil_VolSer, Buf, SizeOf(Buf));
    if rc = NO_ERROR then
      Result := Buf.VolLabel
    else
      Result := '';
  end;

function SysSetVolumeLabel(Drive: Char; _Label: ShortString):
    boolean;
  var
    DriveNumber: word;
  begin
    DriveNumber := Ord(Drive)-Ord('A')+1;
    _Label[Length(_Label)+1] := #0;
    Result := 0 = DosSetFSInfo(DriveNumber, fsil_VolSer, _Label,
      Length(_Label)+1);
  end;

function SysGetForegroundProcessId: longInt;
  var
    Res: word;
    rc: longInt;

  begin
    rc := DosQuerySysInfo(qsv_foreground_process,
      qsv_foreground_process,
    Res, SizeOf(Res));
    if rc = NO_ERROR then
      Result := Res
    else
      Result := 0;
  end;

function SysGetBootDrive: Char;
  var
    Res: word;
    rc: longInt;

  begin
    rc := DosQuerySysInfo(qsv_boot_drive, qsv_boot_drive, Res,
      SizeOf(Res));
    if rc = NO_ERROR then
      Result := Chr(Res+Ord('A')-1)
    else
      Result := #0;
  end;

function SysGetDriveType(Drive: Char): TDriveType;
  var
    BufLen: word;
    FSQb: pFSQBuffer2;
    DrvName: String[3];
    Ordinal: SmallWord;
    Name: PChar;
    rc: word;
    DiskSize: word;

  begin
    Result := dtInvalid;
    BufLen := 100;
    GetMem(FSQb, BufLen);
    DrvName := Drive+':'#0;
    Ordinal := 0;
    rc := DosQueryFSAttach(@DrvName[1], Ordinal, fsail_QueryName,
      FSQb, BufLen);
    if rc = 0 then
      with FSQb^ do
        begin
          Name := szName+cbName+1;
          if StrComp(Name, 'FAT') = 0 then
            if Drive <= 'B' then
              Result := dtFloppy
            else
              Result := dtHDFAT
          else if StrComp(Name, 'HPFS') = 0 then
            if Drive <= 'B' then
              Result := dtFloppy
            else
              Result := dtHDHPFS
          else if StrComp(Name, 'NETWARE') = 0 then
            Result := dtNovellNet
          else if StrComp(Name, 'CDFS') = 0 then
            Result := dtCDRom
          else if StrComp(Name, 'TVFS') = 0 then
            Result := dtTVFS
          else if StrComp(Name, 'ext2') = 0 then
            Result := dtHDExt2
          else if StrComp(Name, 'LAN') = 0 then
            Result := dtLAN
          else if StrComp(Name, 'JFS') = 0 then
            Result := dtJFS;
        end;

    FreeMem(FSQb, 100);
  end { SysGetDriveType };

function SysGetVideoModeInfo(var Cols, Rows, Colours: word): boolean;
  var
    VM: ^VioModeInfo;
    Lvm: array[1..2] of VioModeInfo;

  begin
    VM := Fix_64k(@Lvm, SizeOf(VM^));
    VM.cb := SizeOf(VM^);
    VioGetMode(VM^, 0);
    with VM^ do
      begin
        Rows := Row;
        Cols := Col;
        Colours := 1 shl Color;
      end;
    Result := True;
  end;

function SysGetVisibleLines(var Top, Bottom: longInt): boolean;
  var
    Cols, Rows, Colours: word;
  begin
    if SysGetVideoModeInfo(Cols, Rows, Colours) then
      begin
        Result := True;
        Top := 1;
        Bottom := Rows;
      end
    else
      Result := False;
  end;

function SysSetVideoMode(Cols, Rows: word): boolean;
  var
    VM: ^VioModeInfo;
    Lvm: array[1..2] of VioModeInfo;

  begin
    VM := Fix_64k(@Lvm, SizeOf(VM^));
    VM^.cb := 8; { Size of structure }
    VioGetMode(VM^, 0);
    VM^.fbType := 1; { Text mode }
    VM^.Row := Rows;
    VM^.Col := Cols;
    VM^.Color := 4; { 16 colors }
    Result := (VioSetMode(VM^, 0) = 0);
  end;

function SemCreateEvent(_Name: PChar; _Shared, _State: boolean):
    TSemHandle;
  var
    rc: apiret;
    Attr: ULong;
    Buf: packed array[0..255] of Char;
  begin
    if _Shared then
      Attr := dc_Sem_Shared
    else
      Attr := 0;
    if (_Name <> nil) and (_Name^ <> #0) then
      begin
        StrCat(StrCopy(@Buf, '\SEM32\'), _Name);
        rc := DosCreateEventSem(@Buf, Result, Attr, _State);
      end
    else
      rc := DosCreateEventSem(_Name, Result, Attr, _State);
    if rc <> NO_ERROR then
      Result := -1;
  end { SemCreateEvent };

function SemAccessEvent(_Name: PChar): TSemHandle;
  var
    Buf: packed array[0..255] of Char;
    rc: apiret;
  begin
    if (_Name <> nil) and (_Name^ <> #0) then
      begin
        StrCat(StrCopy(@Buf, '\SEM32\'), _Name);
        Result := 0;
        rc := DosOpenEventSem(@Buf, Result);
        if rc <> NO_ERROR then
          Result := -1;
      end
    else
      Result := -1;
  end;

function SemPostEvent(_Handle: TSemHandle): boolean;
  begin
    Result := DosPostEventSem(_Handle) = 0;
  end;

function SemResetEvent(_Handle: TSemHandle; var _PostCount: longInt):
    boolean;
  begin
    Result := DosResetEventSem(_Handle, _PostCount) = 0;
  end;

function SemWaitEvent(_Handle: TSemHandle; _TimeOut: longInt):
    boolean;
  var
    Dummy: longInt;
  begin
    Result := DosWaitEventSem(_Handle, _TimeOut) = 0;
    DosResetEventSem(_Handle, Dummy);
  end;

procedure SemCloseEvent(_Handle: TSemHandle);
  begin
    DosCloseEventSem(_Handle);
  end;

function SemCreateMutex(_Name: PChar; _Shared, _State: boolean):
    TSemHandle;
  var
    Flags: longInt;
    rc: apiret;
    Buf: packed array[0..255] of Char;
  begin
    Flags := 0;
    if _Shared then
      Flags := dc_Sem_Shared;
    if (_Name <> nil) and (_Name^ <> #0) then
      begin
        StrCat(StrCopy(@Buf, '\SEM32\'), _Name);
        rc := DosCreateMutexSem(@Buf, Result, Flags, _State);
      end
    else
      rc := DosCreateMutexSem(_Name, Result, Flags, _State);
    if rc <> NO_ERROR then
      Result := -1;
  end;

function SemAccessMutex(_Name: PChar): TSemHandle;
  var
    rc: apiret;
    Buf: packed array[0..255] of Char;
  begin
    Result := 0;
    if (_Name <> nil) and (_Name^ <> #0) then
      begin
        StrCat(StrCopy(@Buf, '\SEM32\'), _Name);
        rc := DosOpenMutexSem(@Buf, Result);
      end
    else
      rc := DosOpenMutexSem(_Name, Result);
    if rc <> NO_ERROR then
      Result := -1;
  end;

function SemRequestMutex(_Handle: TSemHandle; _TimeOut: longInt):
    boolean;
  begin
    Result := DosRequestMutexSem(_Handle, _TimeOut) = 0;
  end;

function SemReleaseMutex(_Handle: TSemHandle): boolean;
  begin
    Result := DosReleaseMutexSem(_Handle) = 0;
  end;

procedure SemCloseMutex(_Handle: TSemHandle);
  begin
    DosCloseMutexSem(_Handle);
  end;

function SysMemInfo(_Base: Pointer; _Size: longInt; var _Flags:
    longInt): boolean;
  begin
    Result := (DosQueryMem(_Base, _Size, _Flags) = NO_ERROR);
  end;

function SysSetMemProtection(_Base: Pointer; _Size: longInt; _Flags:
    longInt): boolean;
  begin
    Result := (DosSetMem(_Base, _Size, _Flags) = NO_ERROR);
  end;

{AK155 20-08-2003}
{ "секретные" старинные функции, не описанные в тулките }
{&OrgName+}
function DosMemAvail(var l: longInt): apiret16;
external 'DOSCALLS'Index 127;
function DosFlatToSel(P: Pointer): Pointer;
external 'DOSCALLS'Index 425;
{&OrgName-}

var
  l: longInt;
    {локальную переменную PhysMemAvail (например, result)
    использовать нельзя - трапается при выходе из DosMemAvail}

function PhysMemAvail: longInt;
  begin
    asm
   mov   eax,offset l
   call  DosFlatToSel
   push  eax
   call  far ptr DosMemAvail     { call via CallGate }
end
      ;
    Result := l;
  end;
{/AK155}

procedure SysMessageBox(_Msg, _Title: PChar; _Error: boolean);
  var
    Flag: longInt;
  begin
    if _Error then
      Flag := mb_Error
    else
      Flag := mb_Information;
    WinMessageBox(hwnd_Desktop, hwnd_Desktop, _Msg, _Title, 0, Flag or
      mb_Ok);
  end;

function SysClipCanPaste: boolean;
  var
    Fmt: ULong;
  begin
    WinCreateMsgQueue(WinInitialize(0), 0);
    // Console apps can only use the OS/2 clipboard if the "hack" works
    Result := (not IsConsole or (PM_ClipboardHack = clipOk)) and
    WinQueryClipBrdFmtInfo(WinInitialize(0), cf_Text, Fmt);
  end;

function SysClipCopy(P: PChar; Size: longInt): boolean;
  var
    Q: PChar;
    Anchor: HAB;
  begin
    Result := False;
    Anchor := WinInitialize(0);
    WinCreateMsgQueue(Anchor, 0);
    // Open PM clipboard
    if WinOpenClipBrd(Anchor) then
      begin
        // Allocate giveable block of memory
        DosAllocSharedMem(Pointer(Q), nil, Size+1, pag_Write+
          pag_Commit+obj_Giveable);
        if Q <> nil then
          begin
            // Copy clipboard data across
            Move(P^, Q^, Size);
            Q[Size] := #0;
            // Insert data into clipboard
            Result := WinSetClipBrdData(Anchor, ULong(Q), cf_Text,
              cfi_Pointer);
          end;
        WinCloseClipBrd(Anchor);
      end;
  end { SysClipCopy };

function SysClipPaste(var Size: integer): Pointer;
  var
    P: PChar;
    Anchor: HAB;
    Flags: longInt;
  begin
    Result := nil;
    Anchor := WinInitialize(0);
    WinCreateMsgQueue(Anchor, 0);
    if WinOpenClipBrd(Anchor) then
      begin
        P := PChar(WinQueryClipBrdData(Anchor, cf_Text));
        if Assigned(P) then
          if SysMemInfo(P, 1, Flags) and (Flags and sysmem_Read <> 0)
          then
            begin
              Size := StrLen(P)+1;
              GetMem(Result, Size);
              Move(P^, Result^, Size);
            end;
        WinCloseClipBrd(Anchor);
      end;
  end { SysClipPaste };

// Retrieve various system settings, bitmapped:
// 0: Enhanced keyboard installed

function SysGetSystemSettings: longInt;
  var
    rc: apiret16;
    HwId: ^KbdHwId;
    LHwId: array[1..2] of KbdHwId;

  begin
    Result := 0;

    // Determine if enhanced keyboard is available
    rc := KbdGetFocus(1, 0);
    if rc = NO_ERROR then
      begin
        HwId := Fix_64k(@LHwId, SizeOf(HwId^));
        HwId^.cb := SizeOf(HwId^);
        rc := KbdGetHwId(HwId^, 0);
        if rc = NO_ERROR then
          if (HwId^.IdKbd = keyboard_Enhanced_101) or
            (HwId^.IdKbd = keyboard_Enhanced_122)
          then
            Result := Result or 1;

        rc := KbdFreeFocus(0);
      end;
  end { SysGetSystemSettings: };

procedure SysLowInit;
  begin
    // Nothing
  end;

