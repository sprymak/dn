unit FNoteW32;
(******

Directory Change Notifier - Win32 version
Written by Cat 2:5030/1326.13
(for use in DN/2)

******)

interface

 uses xTime;

{.$DEFINE DEBUGLOG}
Var
 NotifyTmr: TEventTimer;       {JO}

procedure NotifyInit;
procedure NotifyAddWatcher(const Path: String);
procedure NotifyDeleteWatcher(const Path: String);
function NotifyAsk: String;
procedure NotifySuspend;
procedure NotifyResume;

implementation

uses
  Windows, Objects, Collect, Advance1;

type
  PNotifierRec = ^TNotifierRec;
  TNotifierRec = record
    Path: PString;
    Handle: LongInt;
  end;

  PNotifierCollection = ^TNotifierCollection;
  TNotifierCollection = object(TStringCollection) { �⮡� �ᯮ�짮���� Compare ��� ��ப }
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
procedure DebugLog(const S: String);
var
  F: Text;
begin
  Assign(F, 'c:\fnotew32.log');
  if S = '' then
    Rewrite(F)
  else
    Append(F);
  Writeln(F, S);
  Close(F);
end;
{$ENDIF}

var
  NotifierCollection: PNotifierCollection;
  SuspendCount: LongInt;

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

  NotifierCollection := New(PNotifierCollection, Init(16,16, False));
  NotifierCollection^.Duplicates := False;

  {$IFDEF DEBUGLOG}
  DebugLog('');
  {$ENDIF}
end;

procedure NotifyAddWatcher(const Path: String);
var
  S: String;
  P: PNotifierRec;
begin
  if (Path = '') or (NotifierCollection = nil) then
    Exit;
  {$IFDEF DEBUGLOG}
  DebugLog('+ '+Path);
  {$ENDIF}

  S := Path+#0;
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
  if P^.Handle <> INVALID_HANDLE_VALUE then
    NotifierCollection^.Insert(P)
  else
    begin
      {$IFDEF DEBUGLOG}
      DebugLog('! �� ���������');
      {$ENDIF}
      DisposeStr(P^.Path);
      Dispose(P);
    end;
end;

procedure NotifyDeleteWatcher(const Path: String);
var
  I: LongInt;
begin
  if (Path = '') or (NotifierCollection = nil) then
    Exit;
  {$IFDEF DEBUGLOG}
  DebugLog('- '+Path);
  {$ENDIF}

  if NotifierCollection^.Search(@Path, I) then
    NotifierCollection^.AtFree(I)
  {$IFDEF DEBUGLOG}
  else
    DebugLog('! �� 㤠�����');
  {$ENDIF}
end;

function NotifyAsk: String;
var
  I: LongInt;
begin
{JO}
  if NotifierCollection = nil then
     begin
       NotifyAsk := '';
       Exit;
     end;
{/JO}
  for I := 0 to NotifierCollection^.Count-1 do
    with PNotifierRec(NotifierCollection^.At(I))^ do
      if WaitForSingleObject(Handle, 0) = wait_Object_0 then
        begin
          if FindNextChangeNotification(Handle) then
            ;
          if SuspendCount > 0 then
            NotifyAsk := ''
          else
            NotifyAsk := Path^;
          Exit;
        end;
  NotifyAsk := '';
end;

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