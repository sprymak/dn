unit Plugin;
(******

Interface with plugins
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)


{$I STDEFINE.INC}

{Cat
   05-10-2001 - начато добавление плагинов к ДН-у
   19-10-2001 - плагины типа EventCatcher
   21-10-2001 - плагины используют индексы в файлах ресурсов (Lng и Dlg)
   27-10-2001 - плагины используют индекс в файле помощи (Hlp)
   17-01-2002 - решил, что не следует изменять языковой файл и файл
                ресурсов (за исключением добавления/удаления строк меню),
                поэтому индексы в Lng, Dlg, Hlp файлах более не используются,
                а ресурсы плагинов теперь будут содержаться в файлах *.REZ,
                к которым Plugin Manager не будет иметь никакого отношения
                Версия плагинов EventCatcher - 2.0
   23-01-2002 - плагины типа ArchiveViewer
   28-01-2002 - плагины могут регистрировать объекты для операций с потоками
   10-05-2002 - вся структура плагинов переделана заново, без использования
                DN2CAT.DLL
}

{$IFNDEF PLUGIN}
{$ERROR This unit is for use only in plugin version of DN/2!}
{$ENDIF}

interface

uses
  {$IFDEF OS2} Os2Def, Os2Base, {$ENDIF}
  {$IFDEF WIN32} Windows, {$ENDIF}
  Drivers, Archiver;

(*** EVENT CATCHER ***)

type
  {&Cdecl+}
  THandleCommandProc=procedure(Command, ObjType: SmallWord; const PluginName: ShortString; DNFuncs, DNMethods: Pointer; var Finalization: Pointer);
  {&Cdecl-}
  PEventCatcherInfo=^TEventCatcherInfo;
  TEventCatcherInfo=packed record
    FirstCatchedCommand: Word;
    LastCatchedCommand: Word;
    {FirstLngIndex: Word;}
    {LastLngIndex: Word;}
    {FirstDlgIndex: Word;}
    {LastDlgIndex: Word;}
    {FirstHlpIndex: Word;}
    {LastHlpIndex: Word;}
    FirstObjType: Word;
    LastObjType: Word;
    PluginPath: String[8];
    Reserved: packed array[0..2] of Byte;
    LibHandle: HModule;
    Entry: THandleCommandProc;
  end;
  PEventCatcherArray=^TEventCatcherArray;
  TEventCatcherArray=packed array[1..1] of TEventCatcherInfo;

procedure CatchersHandleCommand(Command: Word);
procedure CatchersRegisterObject(ObjType: Word);


(*** EDITOR EVENT HOOKS ***)

{ помимо обычных отлавливаются следующие специальные события: }
{   65001 - начало перерисовки                                }
{   65002 - хотим получить данные подсветки                   }
{   65003 - после загрузки файла                              }
{   65004 - перед сохранением файла                           }
{   65005 - перед "сохранением как"                           }
{ в случае 65002 поле InfoPtr содержит указатель на структуру }
{ TFillColorsData                                             }

type
  PFillColorsData=^TFillColorsData;
  TFillColorsData=record
    DrawBuffer: Pointer;
    StrNum, StartPos, EndPos: LongInt;
  end;

type
  TEditorEventHook=function(var Event: TEvent; Editor: Pointer): Boolean;

function SetEditorEventHook(EditorEventHook: TEditorEventHook): Boolean;
procedure RemoveEditorEventHook(EditorEventHook: TEditorEventHook);
function ProcessEditorEventHook(var Event: TEvent; Editor: Pointer): Boolean;


(*** ARCHIVE VIEWER ***)

const
  arcFirst=100;
  arcLast=249;

type
  {&Cdecl+}
  TFormatsCountProc=function: Word;
  TArchiveSignProc=function(Id: Word): TStr4;
  TCreateArchiveObjectProc=function(Id: Word): PARJArchive;
  TDetectCreateArchiveObjectProc=function: PARJArchive;
  {&Cdecl-}
  PArchiveViewerInfo=^TArchiveViewerInfo;
  TArchiveViewerInfo=packed record
    FirstTag: Byte;
    PluginPath: String[8];
    Reserved: SmallWord;
    LibHandle: HModule;
    FormatsCount: TFormatsCountProc;
    ArchiveSign: TArchiveSignProc;
    CreateArchiveObject: TCreateArchiveObjectProc;
    DetectCreateArchiveObject: TDetectCreateArchiveObjectProc;
  end;
  PArchiveViewerArray=^TArchiveViewerArray;
  TArchiveViewerArray=packed array[arcFirst-1..arcLast+1] of PArchiveViewerInfo;

{Cat: порядок переменных не менять, иначе будут проблемы с плагинами}

var
  EventCatchers: PEventCatcherArray;
  EventCatchersCount: Integer;
  ArchiveViewers: TArchiveViewerArray;

function DetectCreateArchiveObject: PARJArchive;
function GetArchiveTagBySign(Sign: TStr4): Byte;
function GetArchiveByTag(ID: Byte): PARJArchive;

implementation

uses
  Commands, ObjType, Messages, DNApp, Advance, Advance2,
  DNFuncs;

(*** EVENT CATCHER ***)

procedure DonePlugins;
var
  I: Integer;
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
  ArchiveViewersCount: Integer;
  FullPath: String;
const
  Initialized: Boolean = False;
  PLUGINS_CFG: array[0..31] of Char =
    #$01#$00#$00#$00#$00#$7D#$00#$00#$00#$7D#$00#$00#$FF#$FF#$00#$00#$FF#$FF#$00#$00#$07#$50#$4C#$55#$47#$4D#$41#$4E#$00#$00#$00#$00;
begin
  if Initialized then
    Exit;
  Initialized:=True;

  DNFunctions.DN2Version:=VersionWord;

  FullPath:=SourceDir+'PLUGINS.CFG';

  if not ExistFile(FullPath) then
    begin
      Assign(F, FullPath);
      Rewrite(F, 1);
      BlockWrite(F, PLUGINS_CFG, SizeOf(PLUGINS_CFG));
      Close(F);
      if IOResult=0 then
        ;
    end;

  Assign(F, FullPath);
  Reset(F, 1);
  BlockRead(F, EventCatchersCount, SizeOf(EventCatchersCount));
  if (IOResult<>0) or (EventCatchersCount<0) or (EventCatchersCount>60000) then
    begin
      Close(F);
      if IOResult=0 then
        ;
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
      Close(F);
      if IOResult=0 then
        ;
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
              Close(F);
              if IOResult=0 then
                ;
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

procedure CatchersHandleCommand(Command: Word);
var
  I: Integer;
  FullPath: String;
  Finalization: Pointer;
begin
  if EventCatchers<>nil then
    for I:=1 to EventCatchersCount do
      with EventCatchers^[I] do
        if (Command>=FirstCatchedCommand) and (Command<=LastCatchedCommand) then
          begin
            if Assigned(Entry) then
              begin
                Entry(Command-FirstCatchedCommand, FirstObjType, PluginPath, @DNFunctions, @DNMethods, Finalization);
                if Finalization<>nil then
                  AddExitProc(Finalization);
              end
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
                Entry(Command-FirstCatchedCommand, FirstObjType, PluginPath, @DNFunctions, @DNMethods, Finalization);
                if Finalization<>nil then
                  AddExitProc(Finalization);
              end;
            Exit;
          end;
end;

procedure CatchersRegisterObject(ObjType: Word);
var
  I: Integer;
  FullPath: String;
  Finalization: Pointer;
begin
  if (ObjType>=otPlugins) and (ObjType<=otPluginsEnd) then
    if EventCatchers<>nil then
      for I:=1 to EventCatchersCount do
        with EventCatchers^[I] do
          if (ObjType>=FirstObjType) and (ObjType<=LastObjType) then
            begin
              if Assigned(Entry) then
                begin
                  Entry($FFFF, FirstObjType, PluginPath, @DNFunctions, @DNMethods, Finalization);
                  if Finalization<>nil then
                    AddExitProc(Finalization);
                end
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
                  Entry($FFFF, FirstObjType, PluginPath, @DNFunctions, @DNMethods, Finalization);
                  if Finalization<>nil then
                    AddExitProc(Finalization);
                end;
              Exit;
            end;
end;



(*** EDITOR EVENT HOOKS ***)

{&Delphi+}
const
  MaxEditorEventHookCount = 32;

var
  EditorEventHookArray: array[1..MaxEditorEventHookCount] of Pointer{TEditorEventHook};
  EditorEventHookCount: Integer;

function SetEditorEventHook(EditorEventHook: TEditorEventHook): Boolean;
begin
  if EditorEventHookCount<MaxEditorEventHookCount then
    begin
      Result:=True;
      Inc(EditorEventHookCount);
      EditorEventHookArray[EditorEventHookCount]:=@EditorEventHook;
    end
  else
    Result:=False;
end;

procedure RemoveEditorEventHook(EditorEventHook: TEditorEventHook);
var
  I: Integer;
begin
  for I:=1 to EditorEventHookCount do
    if EditorEventHookArray[I]=@EditorEventHook then
      begin
        for I:=I+1 to EditorEventHookCount do
          EditorEventHookArray[I-1]:=EditorEventHookArray[I];
        Dec(EditorEventHookCount);
        Exit;
      end;
end;

function ProcessEditorEventHook(var Event: TEvent; Editor: Pointer): Boolean;
var
  I: Integer;
begin
  for I:=1 to EditorEventHookCount do
    if TEditorEventHook(EditorEventHookArray[I])(Event, Editor) then
      begin
        Result:=True;
        Exit;
      end;
  Result:=False;
end;
{&Delphi-}


(*** ARCHIVE VIEWER ***)

{&Delphi+}
function DetectCreateArchiveObject: PARJArchive;
var
  J: Integer;
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
  J: Integer;
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
