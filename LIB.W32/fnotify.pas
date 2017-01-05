unit fnotify {W32};
(******

Directory Change Notifier - Win32 version
Written by Cat 2:5030/1326.13
(for use in DN/2)

******)

interface
{$I stdefine.inc}

uses
  xTime;

{.$DEFINE DEBUGLOG}
var
  NotifyTmr: TEventTimer; {JO}

procedure NotifyInit;
procedure NotifyAddWatcher(const Path: String);
procedure NotifyDeleteWatcher(const Path: String);
procedure NotifyAsk(var s: String);
procedure NotifySuspend;
procedure NotifyResume;

implementation

uses
  Windows, Objects, Collect, advance1;

type
  PNotifierRec = ^TNotifierRec;
  TNotifierRec = record
    Path: PString;
    Handle: longInt;
    end;

  PNotifierCollection = ^TNotifierCollection;
  TNotifierCollection = object(TStringCollection)
      { чтобы использовать Compare для строк }
    function KeyOf(Item: Pointer): Pointer; virtual;
    {function Compare(Key1, Key2: Pointer): Integer; virtual;}
    procedure FreeItem(Item: Pointer); virtual;
    end;

function TNotifierCollection.KeyOf(Item: Pointer): Pointer;
  begin
    KeyOf := PNotifierRec(Item)^.Path;
  end;

procedure TNotifierCollection.FreeItem(Item: Pointer);
  begin
    FindCloseChangeNotification(PNotifierRec(Item)^.Handle);
    DisposeStr(PNotifierRec(Item)^.Path);
    Dispose(PNotifierRec(Item));
  end;

{$IFDEF DEBUGLOG}
procedure DebugLog(const s: String);
  var
    F: text;
  begin
    Assign(F, 'c:\fnotew32.log');
    if s = '' then
      rewrite(F)
    else
      Append(F);
    Writeln(F, s);
    Close(F);
  end;
{$ENDIF}

var
  NotifierCollection: PNotifierCollection;
  SuspendCount: longInt;

procedure NotifyDone;
  begin
    if NotifierCollection <> nil then
      Dispose(NotifierCollection, Done);
  end;

procedure NotifyInit;
  begin
    xTime.NewTimerSecs(NotifyTmr, 1); {JO}
    if NotifierCollection <> nil then
      Dispose(NotifierCollection, Done)
    else
      AddExitProc(NotifyDone);

    NotifierCollection := New(PNotifierCollection, Init(16, 16,
      False));
    NotifierCollection^.Duplicates := False;

    {$IFDEF DEBUGLOG}
    DebugLog('');
    {$ENDIF}
  end;

procedure NotifyAddWatcher(const Path: String);
  var
    s: String;
    P: PNotifierRec;
  begin
    if (Path = '') or (NotifierCollection = nil) then
      exit;
    {$IFDEF DEBUGLOG}
    DebugLog('+ '+Path);
    {$ENDIF}

    s := Path+#0;
    New(P);
    P^.Path := NewStr(Path);
    P^.Handle := FindFirstChangeNotification(@S[1],
    True,
    file_Notify_Change_File_Name or
    file_Notify_Change_Dir_Name or
    file_Notify_Change_Attributes or
    file_Notify_Change_Size or
    file_Notify_Change_Last_Write {or}
    {file_Notify_Change_Security});
    if P^.Handle <> invalid_Handle_Value then
      NotifierCollection^.Insert(P)
    else
      begin
        {$IFDEF DEBUGLOG}
        DebugLog('! не добавился');
        {$ENDIF}
        DisposeStr(P^.Path);
        Dispose(P);
      end;
  end { NotifyAddWatcher };

procedure NotifyDeleteWatcher(const Path: String);
  var
    i: longInt;
  begin
    if (Path = '') or (NotifierCollection = nil) then
      exit;
    {$IFDEF DEBUGLOG}
    DebugLog('- '+Path);
    {$ENDIF}

    if NotifierCollection^.Search(@Path, i) then
      NotifierCollection^.AtFree(i)
      {$IFDEF DEBUGLOG}
    else
      DebugLog('! не удалился');
    {$ENDIF}
  end;

procedure NotifyAsk(var s: String);
  var
    i: longInt;
  begin
    {JO}
    if (NotifierCollection = nil)
      {AK155} or (SuspendCount <> 0) {/AK155}
    then
      begin
        s := '';
        exit;
      end;
    {/JO}
    for i := 0 to NotifierCollection^.Count-1 do
      with PNotifierRec(NotifierCollection^.At(i))^ do
        if WaitForSingleObject(Handle, 0) = wait_Object_0 then
          begin
            if FindNextChangeNotification(Handle) then
              ;
            if SuspendCount > 0 then
              s := ''
            else
              s := Path^;
            exit;
          end;
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

end.
