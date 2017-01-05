unit Plugin;
(******

Interface with plugins
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)


{$I STDEFINE.INC}

{Cat
   05/10/2001 - начато добавление плагинов к ДН-у
   19/10/2001 - плагины типа EventCatcher
   21/10/2001 - плагины используют индексы в файлах ресурсов (Lng и Dlg)
   27/10/2001 - плагины используют индекс в файле помощи (Hlp)
   17/01/2002 - решил, что не следует изменять языковой файл и файл
                ресурсов (за исключением добавления/удаления строк меню),
                поэтому индексы в Lng, Dlg, Hlp файлах более не используются,
                а ресурсы плагинов теперь будут содержаться в файлах *.REZ,
                к которым Plugin Manager не будет иметь никакого отношения
                Версия плагинов EventCatcher - 2.0
   23/01/2002 - плагины типа ArchiveViewer
   28/01/2002 - плагины могут регистрировать объекты для операций с потоками
}

{$IFNDEF OS2}
{$IFNDEF WIN32}
{$ERROR Only OS2 or WIN32!}
{$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF OS2} Os2Def, Os2Base, {$ENDIF}
  {$IFDEF WIN32} Windows, {$ENDIF}
  Archiver;

(*** EVENT CATCHER ***)

type
  {&Cdecl+}
  {THandleCommandProc=procedure(Command, LngIndex, DlgIndex, HlpIndex: SmallWord);} {1.0}
  THandleCommandProc=procedure(Command, ObjType: SmallWord; const PluginName: ShortString);
  {&Cdecl-}
  PEventCatcherInfo=^TEventCatcherInfo;
  TEventCatcherInfo=record
    FirstCatchedCommand: SmallWord;
    LastCatchedCommand: SmallWord;
    {FirstLngIndex: SmallWord;}
    {LastLngIndex: SmallWord;}
    {FirstDlgIndex: SmallWord;}
    {LastDlgIndex: SmallWord;}
    {FirstHlpIndex: SmallWord;}
    {LastHlpIndex: SmallWord;}
    FirstObjType: SmallWord;
    LastObjType: SmallWord;
    PluginPath: String[8];
    LibHandle: HModule;
    Entry: THandleCommandProc;
  end;
  PEventCatcherArray=^TEventCatcherArray;
  TEventCatcherArray=array[1..1] of TEventCatcherInfo;

var
  EventCatchers: PEventCatcherArray;
  EventCatchersCount: SmallWord;

procedure CatchersHandleCommand(Command: SmallWord);
procedure CatchersRegisterObject(ObjType: SmallWord);


(*** ARCHIVE VIEWER ***)

const
  arcFirst=100;
  arcLast=249;

type
  {&Cdecl+}
  TFormatsCountProc = function: SmallWord;
  TArchiveSignProc = function(Id: SmallWord): TStr4;
  TCreateArchiveObjectProc = function(Id: SmallWord): PARJArchive;
  TDetectCreateArchiveObjectProc = function: PARJArchive;
  {&Cdecl-}
  PArchiveViewerInfo=^TArchiveViewerInfo;
  TArchiveViewerInfo=record
    FirstTag: Byte;
    PluginPath: String[8];
    LibHandle: HModule;
    FormatsCount: TFormatsCountProc;
    ArchiveSign: TArchiveSignProc;
    CreateArchiveObject: TCreateArchiveObjectProc;
    DetectCreateArchiveObject: TDetectCreateArchiveObjectProc;
  end;
  PArchiveViewerArray=^TArchiveViewerArray;
  TArchiveViewerArray=array[arcFirst-1..arcLast+1] of PArchiveViewerInfo;

var
  ArchiveViewers: TArchiveViewerArray;

function DetectCreateArchiveObject: PARJArchive;
function GetArchiveTagBySign(Sign: TStr4): Byte;
function GetArchiveByTag(ID: Byte): PARJArchive;

implementation

uses
  Commands, ObjType, Advance, Messages, DnApp;

const
  Initialized: Boolean = False;

procedure DonePlugins;
var
  I: SmallWord;
begin
  for I:=1 to EventCatchersCount do
    with EventCatchers^[I] do
      if LibHandle<>0 then
        {$IFDEF OS2}
        DosFreeModule(LibHandle);
        {$ENDIF}
        {$IFDEF WIN32}
        FreeLibrary(LibHandle);
        {$ENDIF}
  FreeMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));

  for I:=arcLast downto arcFirst do
    if ArchiveViewers[I]<>nil then
      if I=ArchiveViewers[I]^.FirstTag then
        Dispose(ArchiveViewers[I]);
end;

procedure InitPlugins;
var
  F: File;
  I, J, K: Integer;
  Len: Byte;
  ArchiveViewersCount: SmallWord;
  FullPath: String;
begin
  if Initialized then
    Exit;
  Initialized:=True;

  Assign(F, SourceDir+'PLUGINS.CFG');
  Reset(F, 1);
  BlockRead(F, EventCatchersCount, SizeOf(EventCatchersCount));
  if IOResult<>0 then
    begin
      FreeMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));
      EventCatchers:=nil;
      Writeln('Error reading file PLUGINS.CFG');
      Exit;
    end;

  GetMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));

  for I:=1 to EventCatchersCount do
    with EventCatchers^[I] do
      begin
        BlockRead(F, FirstCatchedCommand, SizeOf(FirstCatchedCommand));
        BlockRead(F, LastCatchedCommand, SizeOf(LastCatchedCommand));
        {BlockRead(F, FirstLngIndex, SizeOf(FirstLngIndex));}
        {BlockRead(F, LastLngIndex, SizeOf(LastLngIndex));}
        {BlockRead(F, FirstDlgIndex, SizeOf(FirstDlgIndex));}
        {BlockRead(F, LastDlgIndex, SizeOf(LastDlgIndex));}
        {BlockRead(F, FirstHlpIndex, SizeOf(FirstHlpIndex));}
        {BlockRead(F, LastHlpIndex, SizeOf(LastHlpIndex));}
        BlockRead(F, FirstObjType, SizeOf(FirstObjType));
        BlockRead(F, LastObjType, SizeOf(LastObjType));
        BlockRead(F, Len, SizeOf(Len));
        if Len>8 then
          Len:=8;
        SetLength(PluginPath, Len);
        BlockRead(F, PluginPath[1], Len);
        LibHandle:=0;
        @Entry:=nil;
      end;

  BlockRead(F, ArchiveViewersCount, SizeOf(ArchiveViewersCount));
  FillChar(ArchiveViewers, SizeOf(ArchiveViewers), #0);
  if IOResult<>0 then
    begin
      FreeMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));
      EventCatchers:=nil;
      Writeln('Error reading file PLUGINS.CFG');
      Exit;
    end;

  J:=arcFirst;
  for I:=1 to ArchiveViewersCount do
    begin
      New(ArchiveViewers[J]);
      with ArchiveViewers[J] do
        begin
          FirstTag:=J;
          BlockRead(F, Len, SizeOf(Len));
          if Len>8 then
            Len:=8;
          SetLength(PluginPath, Len);
          BlockRead(F, PluginPath[1], Len);
          if IOResult<>0 then
            begin
              FreeMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));
              EventCatchers:=nil;
              Dispose(ArchiveViewers[J]);
              ArchiveViewers[J]:=nil;
              Writeln('Error reading file PLUGINS.CFG');
              Exit;
            end;
          FullPath:=SourceDir+PluginPath+'.DLL'#0;
          {$IFDEF OS2}
          if (DosLoadModule(nil, 0, @FullPath[1], LibHandle)<>0)
          or (DosQueryProcAddr(LibHandle, 0, 'FormatsCount', @FormatsCount)<>0)
          or (DosQueryProcAddr(LibHandle, 0, 'ArchiveSign', @ArchiveSign)<>0)
          or (DosQueryProcAddr(LibHandle, 0, 'CreateArchiveObject', @CreateArchiveObject)<>0)
          or (DosQueryProcAddr(LibHandle, 0, 'DetectCreateArchiveObject', @DetectCreateArchiveObject)<>0) then
            begin
              Writeln('Cannot load module ', PluginPath);
              Dispose(ArchiveViewers[J]);
              ArchiveViewers[J]:=nil;
              Continue;
            end;
          {$ENDIF}
          {$IFDEF WIN32}
          LibHandle:=LoadLibrary(@FullPath[1]);
          if LibHandle<HINSTANCE_ERROR then
            begin
              LibHandle:=0;
              Writeln('Cannot load module ', PluginPath);
              Dispose(ArchiveViewers[J]);
              ArchiveViewers[J]:=nil;
              Continue;
            end;
          @FormatsCount:=GetProcAddress(LibHandle, 'FormatsCount');
          @ArchiveSign:=GetProcAddress(LibHandle, 'ArchiveSign');
          @CreateArchiveObject:=GetProcAddress(LibHandle, 'CreateArchiveObject');
          @DetectCreateArchiveObject:=GetProcAddress(LibHandle, 'DetectCreateArchiveObject');
          if not Assigned(FormatsCount) or not Assigned(ArchiveSign) or not Assigned(CreateArchiveObject) or not Assigned(DetectCreateArchiveObject) then
            begin
              Writeln('Cannot load module ', PluginPath);
              Dispose(ArchiveViewers[J]);
              ArchiveViewers[J]:=nil;
              Continue;
            end;
          {$ENDIF}
        end;
      for K:=1 to ArchiveViewers[J]^.FormatsCount do
        begin
          Inc(J);
          if J>arcLast then
            Break;
          ArchiveViewers[J]:=ArchiveViewers[J-1];
        end;
    end;

  Close(F);
  if IOResult=0 then
    AddExitProc(DonePlugins)
  else
    begin
      FreeMem(EventCatchers, EventCatchersCount*SizeOf(TEventCatcherInfo));
      EventCatchers:=nil;
      Writeln('Error reading file PLUGINS.CFG');
    end;
end;

procedure CatchersHandleCommand(Command: SmallWord);
var
  I: SmallWord;
  FullPath: String;
begin
  if EventCatchers<>nil then
    for I:=1 to EventCatchersCount do
      with EventCatchers^[I] do
        if (Command>=FirstCatchedCommand) and (Command<=LastCatchedCommand) then
          begin
            if Assigned(Entry) then
              {Entry(Command-FirstCatchedCommand, FirstLngIndex, FirstDlgIndex, FirstHlpIndex)} {1.0}
              Entry(Command-FirstCatchedCommand, FirstObjType, PluginPath)
            else
              begin
                FullPath:=SourceDir+PluginPath+'.DLL'#0;
                {$IFDEF OS2}
                if (LibHandle<>0)
                or (DosLoadModule(nil, 0, @FullPath[1], LibHandle)<>0)
                or (DosQueryProcAddr(LibHandle, 0, 'CatchCommand', @Entry)<>0) then
                  begin
                    MessageBox(GetString(dlCantLoad)+PluginPath, nil, mfError+mfOKButton);
                    Exit;
                  end;
                {$ENDIF}
                {$IFDEF WIN32}
                if LibHandle=0 then
                  begin
                    LibHandle:=LoadLibrary(@FullPath[1]);
                    if LibHandle<HINSTANCE_ERROR then
                      begin
                        LibHandle:=0;
                        MessageBox(GetString(dlCantLoad)+PluginPath, nil, mfError+mfOKButton);
                        Exit;
                      end;
                    @Entry:=GetProcAddress(LibHandle, 'CatchCommand');
                    if not Assigned(Entry) then
                      begin
                        MessageBox(GetString(dlCantLoad)+PluginPath, nil, mfError+mfOKButton);
                        Exit
                      end;
                  end;
                {$ENDIF}
                {Entry(Command-FirstCatchedCommand, FirstLngIndex, FirstDlgIndex, FirstHlpIndex);} {1.0}
                Entry(Command-FirstCatchedCommand, FirstObjType, PluginPath);
              end;
            Exit;
          end;
end;

procedure CatchersRegisterObject(ObjType: SmallWord);
var
  I: SmallWord;
  FullPath: String;
begin
  if ObjType>=otPlugins then
    if EventCatchers<>nil then
      for I:=1 to EventCatchersCount do
        with EventCatchers^[I] do
          if (ObjType>=FirstObjType) and (ObjType<=LastObjType) then
            begin
              if Assigned(Entry) then
                Entry($FFFF, FirstObjType, PluginPath)
              else
                begin
                  FullPath:=SourceDir+PluginPath+'.DLL'#0;
                  {$IFDEF OS2}
                  if (LibHandle<>0)
                  or (DosLoadModule(nil, 0, @FullPath[1], LibHandle)<>0)
                  or (DosQueryProcAddr(LibHandle, 0, 'CatchCommand', @Entry)<>0) then
                    begin
                      MessageBox(GetString(dlCantLoad)+PluginPath, nil, mfError+mfOKButton);
                      Exit;
                    end;
                  {$ENDIF}
                  {$IFDEF WIN32}
                  if LibHandle=0 then
                    begin
                      LibHandle:=LoadLibrary(@FullPath[1]);
                      if LibHandle<HINSTANCE_ERROR then
                        begin
                          LibHandle:=0;
                          MessageBox(GetString(dlCantLoad)+PluginPath, nil, mfError+mfOKButton);
                          Exit;
                        end;
                      @Entry:=GetProcAddress(LibHandle, 'CatchCommand');
                      if not Assigned(Entry) then
                        begin
                          MessageBox(GetString(dlCantLoad)+PluginPath, nil, mfError+mfOKButton);
                          Exit
                        end;
                    end;
                  {$ENDIF}
                  Entry($FFFF, FirstObjType, PluginPath);
                end;
              Exit;
            end;
end;

{&Delphi+}
function DetectCreateArchiveObject: PARJArchive;
var
  J: SmallWord;
begin
  for J:=arcFirst to arcLast do
    if ArchiveViewers[J]<>nil then
      begin
        Result:=ArchiveViewers[J]^.DetectCreateArchiveObject;
        if Result<>nil then
          Exit;
      end;
  Result:=nil;
end;

function GetArchiveTagBySign(Sign: TStr4): Byte;
var
  J: SmallWord;
begin
  for J:=arcFirst to arcLast do
    if ArchiveViewers[J]<>nil then
      with ArchiveViewers[J] do
        if ArchiveSign(J-FirstTag)=Sign then
          begin
            Result:=J;
            Exit;
          end;
  Result:=arcUNK;
end;

function GetArchiveByTag(ID: Byte): PARJArchive;
begin
  if ArchiveViewers[ID]=nil then
    Result:=nil
  else
    with ArchiveViewers[ID] do
      Result:=CreateArchiveObject(ID-FirstTag);
end;
{&Delphi-}

begin
  InitPlugins
end.
