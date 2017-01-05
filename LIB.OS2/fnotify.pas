unit fnotify {OS2};
(******

Directory Change Notifier - OS/2 version
Written by Cat 2:5030/1326.13
(for use in DN/2)

******)

{ Содержит шаманские штуки, без бубна не влезать! }

{&Delphi+}
{$I stdefine.inc}
interface

uses
  xTime;

{.$DEFINE DEBUGLOG}
var
  NotifyTmr: TEventTimer; {JO}

procedure NotifyAddWatcher(const Path: String);
  inline;
  begin
  end;
procedure NotifyDeleteWatcher(const Path: String);
  inline;
  begin
  end;

procedure NotifyInit;
procedure NotifyAsk(var s: String);
procedure NotifySuspend;
procedure NotifyResume;

implementation

uses
  Os2Def, Os2Base;

// Equates for ChangeNotifyInfo bAction field

const
  RCNF_FILE_ADDED = $0001;
const
  RCNF_FILE_DELETED = $0002;
const
  RCNF_DIR_ADDED = $0003;
const
  RCNF_DIR_DELETED = $0004;
const
  RCNF_MOVED_IN = $0005;
const
  RCNF_MOVED_OUT = $0006;
const
  RCNF_CHANGED = $0007;
const
  RCNF_OLDNAME = $0008;
const
  RCNF_NEWNAME = $0009;
const
  RCNF_DEVICE_ATTACHED = $000A;
const
  RCNF_DEVICE_DETACHED = $000B;

type
  PCNPath = ^TCNPath;
  TCNPath = packed record// CHANGENOTIFYPATH
    oNextEntryOffset: ULong;
    wFlags: ULong;
    cbName: UShort;
    szName: array[0..0] of Char;
    end;

  PCNInfo = ^TCNInfo;
  TCNInfo = packed record// CHANGENOTIFYINFO
    oNextEntryOffset: ULong;
    bAction: Char;
    cbName: UShort;
    szName: array[0..0] of Char;
    end;

var
  DOSCALLS: lHandle;

  Dos32OpenChangeNotify: function (PathBuf: PCNPath;
  LogSize: ULong;
  var hdir: lHandle;
  ulReserved: ULong): apiret cdecl;
  Dos32ResetChangeNotify: function (LogBuf: PCNInfo;
  BufferSize: ULong;
  var LogCount: ULong;
  hdir: lHandle): apiret cdecl;
  Dos32CloseChangeNotify: function (hdir: lHandle): apiret cdecl;

var
  NotifyHandle: lHandle;
  NotifyThread: lHandle;
  Initialized: boolean;
  DataReady: boolean;
  Data: array[1..2048] of byte;
  DataPtr: PCNInfo;
  SuspendCount: longInt;

const
  SpyAll: TCNPath =
  (oNextEntryOffset: 0;
  wFlags: 0;
  cbName: 0;
  szName: '');

  {$IFDEF DEBUGLOG}
procedure DebugLog(const s: String);
  var
    F: text;
  begin
    Assign(F, 'c:\fnoteos2.log');
    if s = '' then
      rewrite(F)
    else
      Append(F);
    Writeln(F, s);
    Close(F);
  end;
{$ENDIF}

function NotifyThreadFunction(P: ULong): apiret;
  cdecl;
  var
    LogCount: longInt;
  begin
    repeat
      repeat
        DosSleep(100);
      until not DataReady;

      if Dos32ResetChangeNotify(@Data, SizeOf(Data), LogCount,
          NotifyHandle) = 0
      then
        { здесь не учитывается, что буфер может переполниться, а часть сообщений потеряться }
        begin
          DataPtr := @Data;
          DataReady := True;
        end;
      Dos32CloseChangeNotify(NotifyHandle);
      Dos32OpenChangeNotify(@SpyAll, SizeOf(SpyAll), NotifyHandle, 0);
    until False;
  end { NotifyThreadFunction };

procedure NotifyAsk(var s: String);
  var
    P: byte;
  begin
    if DataReady {AK155} and (SuspendCount = 0) {/AK155} then
      with DataPtr^ do
        begin
          if cbName > 255 then
            P := 255
          else
            P := cbName;

          SetLength(s, P);
          Move(szName, s[1], P);

          if oNextEntryOffset = 0 then
            DataReady := False
          else
            DataPtr := Pointer(longInt(DataPtr)+oNextEntryOffset);

          {$IFDEF DEBUGLOG}
          DebugLog(Char(byte(bAction)+byte('0'))+' '+s);
          {$ENDIF}
          {
        while (P <> 0) and (Result[P] <> '\') do
          Dec(P);
        if P <> 0 then
          SetLength(Result, P-1);
        }
        end
      else
      s := '';
  end { NotifyAsk };

procedure NotifySuspend;
  begin
    Inc(SuspendCount);

    {$IFDEF DEBUGLOG}
    DebugLog('! suspend');
    {$ENDIF}
  end;

procedure NotifyResume;
  begin
    if SuspendCount > 0 then
      Dec(SuspendCount);

    {$IFDEF DEBUGLOG}
    DebugLog('! resume');
    {$ENDIF}
  end;

procedure NotifyDone;
  begin
    if NotifyThread <> 0 then
      DosKillThread(NotifyThread);

    if Initialized then
      Dos32CloseChangeNotify(NotifyHandle);

    if DOSCALLS <> 0 then
      DosFreeModule(DOSCALLS);
  end;

procedure NotifyInit;
  begin
    xTime.NewTimerSecs(NotifyTmr, 1); {JO}
    Initialized := (DosLoadModule(nil, 0, 'DOSCALLS', DOSCALLS) = 0)
    and (DosQueryProcAddr(DOSCALLS, 440, nil, @Dos32OpenChangeNotify) =
      0)
    and (DosQueryProcAddr(DOSCALLS, 441, nil,
      @Dos32ResetChangeNotify) = 0)
    and (DosQueryProcAddr(DOSCALLS, 442, nil,
      @Dos32CloseChangeNotify) = 0);

    if Initialized then
      begin
        for NotifyHandle := 0 to 65535 do
          Dos32CloseChangeNotify(NotifyHandle);

        Dos32OpenChangeNotify(@SpyAll, SizeOf(SpyAll), NotifyHandle, 0);
        DosCreateThread(NotifyThread, NotifyThreadFunction, 0, 0,
          8192);
      end;

    AddExitProc(NotifyDone);

    {$IFDEF DEBUGLOG}
    DebugLog('');
    {$ENDIF}
  end { NotifyInit };

end.
