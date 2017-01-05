unit Plugin;
(******

Interface with plugins
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)


{$I STDEFINE.INC}

{Cat
   05/10/2001 - плагины типа EventCatcher
}

{$IFNDEF OS2}
{$IFNDEF WIN32}
{$ERROR Only OS2 or WIN32!}
{$ENDIF}
{$ENDIF}

interface

uses
  Drivers;

procedure CatchersHandleEvent(var Event: TEvent);

implementation

uses
  {$IFDEF OS2} Os2Def, Os2Base, {$ENDIF}
  {$IFDEF WIN32} Windows, {$ENDIF}
  Use16, Commands, Advance, Messages, DnApp;

type
  THandleEventProc=procedure(var Event:TEvent);
  PEventCatcherInfo=^TEventCatcherInfo;
  TEventCatcherInfo=record
    CatchedEvent: TEvent;
    PluginPath: String;
    LibHandle: HModule;
    Entry: THandleEventProc;
  end;
  PEventCatcherArray=^TEventCatcherArray;
  TEventCatcherArray=array[1..1] of TEventCatcherInfo;

var
  EventCatchers: PEventCatcherArray;
  EventCatchersCount: Word;
  Initialized: Boolean;

const
  pierr_Ok                   =0;
  pierr_LoadModule           =1;
  pierr_QueryGetIdProcAdddr  =2;
  pierr_PluginInit           =3;

procedure DonePlugins;
var
  i: Word;
begin
  for i:=1 to EventCatchersCount do
    with EventCatchers^[i] do
      if LibHandle<>0 then
        {$IFDEF OS2}
        DosFreeModule(LibHandle);
        {$ENDIF}
        {$IFDEF WIN32}
        FreeLibrary(LibHandle);
        {$ENDIF}
  FreeMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));
end;

procedure InitPlugins;
var
  f: File;
  i: Word;
  Len: Byte;
begin
  if Initialized then
    exit;
  Initialized:=True;

  Assign(f, SourceDir+'PLUGINS.CFG');
  Reset(f, 1);
  BlockRead(f, EventCatchersCount, SizeOf(EventCatchersCount));
  if (IOResult<>0) or (EventCatchersCount=0) then
    exit;

  GetMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));

  for i:=1 to EventCatchersCount do
    with EventCatchers^[i] do
      begin
        BlockRead(f, CatchedEvent, SizeOf(CatchedEvent));
        BlockRead(f, Len, SizeOf(Len));
        SetLength(PluginPath, Len);
        BlockRead(f, PluginPath[1], Len);
        PluginPath:=PluginPath+#0;
        LibHandle:=0;
        @Entry:=nil;
      end;

  Close(f);
  if IOResult=0 then
    AddExitProc(DonePlugins)
  else
    begin
      FreeMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));
      EventCatchers:=nil;
      Writeln('Error reading file PLUGINS.CFG');
    end;
end;

procedure CatchersHandleEvent(var Event: TEvent);
var
  i: Word;
begin
  if EventCatchers<>nil then
    for i:=1 to EventCatchersCount do
      with EventCatchers^[i] do
        if (Event.What=CatchedEvent.What) and (Event.KeyCode=CatchedEvent.KeyCode) then
          begin
            if Assigned(Entry) then
              Entry(Event)
            else
              begin
                {$IFDEF OS2}
                if (LibHandle<>0)
                or (DosLoadModule(nil, 0, @PluginPath[1], LibHandle)<>0)
                or (DosQueryProcAddr(LibHandle, 0, 'CatchHandleEvent', @Entry)<>0) then
                  begin
                    MessageBox(GetString(dlCantLoad)+PluginPath, nil, mfError+mfOKButton);
                    exit;
                  end;
                {$ENDIF}
                {$IFDEF WIN32}
                if LibHandle=0 then
                  begin
                    LibHandle:=LoadLibrary(@PluginPath[1]);
                    if LibHandle<HINSTANCE_ERROR then
                      begin
                        LibHandle:=0;
                        MessageBox(GetString(dlCantLoad)+PluginPath, nil, mfError+mfOKButton);
                        exit;
                      end;
                    @Entry:=GetProcAddress(LibHandle, 'CatchHandleEvent');
                    if not Assigned(Entry) then
                      begin
                        MessageBox(GetString(dlCantLoad)+PluginPath, nil, mfError+mfOKButton);
                        exit
                      end;
                  end;
                {$ENDIF}
                Entry(Event)
              end;
            exit;
          end;
end;

begin
  InitPlugins
end.
