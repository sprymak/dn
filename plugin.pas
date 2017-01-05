unit Plugin;
(******

Interface with plugins
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)

{$I STDEFINE.INC}
{&Delphi-}
{&Use32+}

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
   10-09-2002 - плагины типа RuntimePatch
   03-11-2002 - плагины типа DrivePanel
}

{Cat:warn при выходе из программы хорошо было бы освобождать библиотеки }
{ модулей RuntimePatch, а пока надеемся, что система всё сделает за нас }

{$IFNDEF PLUGIN}
{$ERROR This unit is for use only in plugin version of DN/2!}
{$ENDIF}

interface

uses
  Modules, Objects, Drivers, Menus, Archiver;

(*** RUNTIME PATCH ***)

type
  {&Cdecl+}
  TRuntimePatch = function (const PluginName: ShortString; DNFuncs,
    DNMethods: Pointer; var Finalization: Pointer): boolean;
  {&Cdecl-}

  (*** EVENT CATCHER ***)

type
  {&Cdecl+}
  THandleCommandProc = procedure (Command, ObjType: SmallWord; const
    PluginName: ShortString; DNFuncs, DNMethods: Pointer; var
    Finalization: Pointer);
  {&Cdecl-}
  PEventCatcherInfo = ^TEventCatcherInfo;
  TEventCatcherInfo = packed record
    FirstCatchedCommand: word;
    LastCatchedCommand: word;
    {FirstLngIndex: Word;}
    {LastLngIndex: Word;}
    {FirstDlgIndex: Word;}
    {LastDlgIndex: Word;}
    {FirstHlpIndex: Word;}
    {LastHlpIndex: Word;}
    FirstObjType: word;
    LastObjType: word;
    PluginPath: String[8];
    Reserved: packed array[0..2] of byte;
    LibHandle: integer;
    Entry: THandleCommandProc;
    end;
  PEventCatcherArray = ^TEventCatcherArray;
  TEventCatcherArray = packed array[1..1] of TEventCatcherInfo;

procedure CatchersHandleCommand(Command: word);

(*** DRIVE PANELS ***)

const
  MaxDrivePanelsInfo = 32;

type
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = packed array[0..0] of integer;

  PPCharArray = ^TPCharArray;
  TPCharArray = packed array[0..0] of PChar;

  PMenuStringsRet = ^TMenuStringsRet;
  TMenuStringsRet = packed record
    Reserved0: integer;
    Count: byte;
    Cacheable: boolean;
    reserved1: SmallWord;
    Strings1: PPCharArray;
    Strings2: PPCharArray;
    Keys: PIntegerArray;
    Reserved2: integer;
    Reserved3: integer;
    Reserved4: integer;
    end;

  {&Cdecl+}
  TGetMenuStringsProc = function (Command, ObjType: SmallWord; const
    PluginName: ShortString; DNFuncs, DNMethods: Pointer):
    PMenuStringsRet;
  TCreateDriveObjectProc = function (Command, ObjType: SmallWord;
    const PluginName: ShortString; DNFuncs, DNMethods: Pointer;
    AOwner: Pointer; Num: integer): Pointer;
  TRegisterDriveObjectProc = function (Command, ObjType: SmallWord;
    const PluginName: ShortString; DNFuncs, DNMethods: Pointer):
    integer;
  {&Cdecl-}

  PDrivePanelsInfo = ^TDrivePanelsInfo;
  TDrivePanelsInfo = packed record
    PluginPath: PString;
    LibHandle: integer;
    CreateDriveObject: TCreateDriveObjectProc;
    RegisterDriveObject: TRegisterDriveObjectProc;
    ObjType: word;
    MenuString1: PString;
    MenuString2: PString;
    MenuKey: integer;
    end;

  PDrivePanelsInfoArray = ^TDrivePanelsInfoArray;
  TDrivePanelsInfoArray = packed array[1..MaxDrivePanelsInfo] of
    PDrivePanelsInfo;

  PDrivePanelsInfo2 = ^TDrivePanelsInfo2;
  TDrivePanelsInfo2 = packed record
    PluginPath: PString;
    LibHandle: integer;
    CreateDriveObject: TCreateDriveObjectProc;
    RegisterDriveObject: TRegisterDriveObjectProc;
    ObjType: word;
    MenuStrings: PMenuStringsRet;
    end;

  PDrivePanelsInfoArray2 = ^TDrivePanelsInfoArray2;
  TDrivePanelsInfoArray2 = packed array[1..MaxDrivePanelsInfo] of
    PDrivePanelsInfo2;

function CreateDriveMenus(var Items: PMenuItem; var MaxL: integer):
  integer;
function CreateDriveObject(i: integer; AOwner: Pointer; Num: integer):
  Pointer;

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
  PFillColorsData = ^TFillColorsData;
  TFillColorsData = record
    DrawBuffer: Pointer;
    StrNum, StartPos, EndPos: longInt;
    end;

type
  TEditorEventHook = function (var Event: TEvent; Editor: Pointer):
    boolean;

function SetEditorEventHook(EditorEventHook: TEditorEventHook):
  boolean;
procedure RemoveEditorEventHook(EditorEventHook: TEditorEventHook);
function ProcessEditorEventHook(var Event: TEvent; Editor: Pointer):
  boolean;

(*** ARCHIVE VIEWER ***)

const
  arcFirst = 100;
  arcLast = 249;

type
  {&Cdecl+}
  TFormatsCountProc = function : word;
  TArchiveSignProc = function (Id: word): TStr4;
  TCreateArchiveObjectProc = function (Id: word): PARJArchive;
  TDetectCreateArchiveObjectProc = function : PARJArchive;
  {&Cdecl-}
  PArchiveViewerInfo = ^TArchiveViewerInfo;
  TArchiveViewerInfo = packed record
    FirstTag: byte;
    PluginPath: String[8];
    Reserved: SmallWord;
    LibHandle: integer;
    FormatsCount: TFormatsCountProc;
    ArchiveSign: TArchiveSignProc;
    CreateArchiveObject: TCreateArchiveObjectProc;
    DetectCreateArchiveObject: TDetectCreateArchiveObjectProc;
    end;
  PArchiveViewerArray = ^TArchiveViewerArray;
  TArchiveViewerArray = packed array[arcFirst-1..arcLast+1] of
    PArchiveViewerInfo;

function DetectCreateArchiveObject: PARJArchive;
function GetArchiveTagBySign(Sign: TStr4): byte;
function GetArchiveByTag(Id: byte): PARJArchive;

{Cat: порядок переменных не менять, иначе будут проблемы с плагинами}

var
  EventCatchers: PEventCatcherArray;
  EventCatchersCount: integer;
  ArchiveViewers: TArchiveViewerArray;
  DrivePanelsInfo: TDrivePanelsInfoArray;
  DrivePanelsInfoCount: integer;
  DrivePanelsInfo2: TDrivePanelsInfoArray2;
  DrivePanelsInfo2Count: integer;

const
  ProcNamesArchiveViewer: array[0..3] of PChar =
  ('FormatsCount',
  'ArchiveSign',
  'CreateArchiveObject',
  'DetectCreateArchiveObject');
  ProcNamesEventCatcher: array[0..0] of PChar =
  ('CatchCommand');
  ProcNamesDrivePanels: array[0..2] of PChar =
  ('GetMenuStrings',
  'CreateDriveObject',
  'RegisterDriveObject');

procedure PluginRegisterObject(ObjType: word);

implementation

uses
  Dos, Strings, Commands,
  ObjType, Messages, DNApp, advance, advance1, advance2, Lfn,
    DNFuncs;

const
  s_Cannot_load_module = 'Cannot load module ';
  s_Error_reading_file_PLUGINS_CFG =
    'Error reading file PLUGINS.CFG';
  s_Error_reading_file_PLUGINS2_CFG =
    'Error reading file PLUGINS2.CFG';
  s_Error_writing_file_PLUGINS2_CFG =
    'Error writing file PLUGINS2.CFG';

  {$IFNDEF DNPRG}
  {$I Version.Inc}
  {$ENDIF}

function StrPas(P: PChar): String;
  begin
    if P = nil then
      StrPas := ''
    else
      StrPas := Strings.StrPas(P);
  end;

(*** RUNTIME PATCH ***)

procedure ApllyRuntimePatches;
  var
    PlugDir: String;
    SR: lSearchRec;
    LibHandle: integer;
    RuntimePatch: TRuntimePatch;
    Finalization: Pointer;

  procedure ApplyRuntimePatch(const FullPath: String);
    begin
      if LoadModule(@FullPath[1], LibHandle)
        and GetProcAddress(LibHandle, 'RuntimePatch', @RuntimePatch)
      then
        begin
          if RuntimePatch(FullPath, @DNFunctions, @DNMethods,
              Finalization)
          then
            FreeModule(LibHandle)
          else if Finalization <> nil then
            AddExitProc(Finalization);
        end
      else
        begin
          Writeln(s_Cannot_load_module, FullPath);
          FreeModule(LibHandle);
        end;
    end;

  begin { ApllyRuntimePatches }
    PlugDir := SourceDir+'PLUG_X\';

    { сначала запускаем все плагины из подкаталога PLUG_X }
    lFindFirst(PlugDir+'*.DLL', AnyFile, SR);
    while DOSError = 0 do
      begin
        if (SR.SR.Attr and (Directory or Hidden)) = 0 then
          ApplyRuntimePatch(PlugDir+SR.FullName+#0);
        lFindNext(SR);
      end;
    lFindClose(SR);

    { теперь попробуем запустить X.DLL из каталога DN-а }
    if ExistFile(SourceDir+'X.DLL') then
      ApplyRuntimePatch(SourceDir+'X.DLL'#0);
  end { ApllyRuntimePatches };

(*** EVENT CATCHER ***)

procedure DoneDrivePanels(NeedWriteConfig: boolean);
forward;

procedure DonePlugins;
  var
    i: integer;
  begin
    for i := 1 to EventCatchersCount do
      with EventCatchers^[i] do
        FreeModule(LibHandle);
    FreeMem(EventCatchers, EventCatchersCount*SizeOf(
      TEventCatcherInfo));

    for i := arcLast downto arcFirst do
      if ArchiveViewers[i] <> nil then
        if i = ArchiveViewers[i]^.FirstTag then
          Dispose(ArchiveViewers[i]);

    DoneDrivePanels(True);
  end;

procedure InitPlugins;
  label
    Plugins2;
  var
    F: file;
    i, j, k: integer;
    ArchiveViewersCount: integer;
    FullPath: String;

    {&Delphi+}
  function ReadStr: String;
    var
      len: byte;
    begin
      BlockRead(F, len, SizeOf(len));
      SetLength(Result, len);
      BlockRead(F, Result[1], len);
    end;
  {&Delphi-}

  const
    Initialized: boolean = False;
    PLUGINS_CFG: array[0..31] of Char =
    #$01#$00#$00#$00#$00#$7D#$00#$00#$00#$7D#$00#$00#$FF#$FF#$00#$00#$FF#$FF#$00#$00#$07#$50#$4C#$55#$47#$4D#$41#$4E#$00#$00#$00#$00
      ;
  begin { InitPlugins }
    if Initialized then
      exit;
    Initialized := True;

    DNFunctions.DN2Version := VersionWord;

    ApllyRuntimePatches;

    FullPath := SourceDir+'PLUGINS.CFG';

    if not ExistFile(FullPath) then
      begin
        Assign(F, FullPath);
        rewrite(F, 1);
        BlockWrite(F, PLUGINS_CFG, SizeOf(PLUGINS_CFG));
        Close(F);
        if IOResult = 0 then
          ;
      end;

    Assign(F, FullPath);
    Reset(F, 1);
    BlockRead(F, EventCatchersCount, SizeOf(EventCatchersCount));
    if (IOResult <> 0) or (EventCatchersCount < 0) or (
        EventCatchersCount > 60000)
    then
      begin
        Close(F);
        if IOResult = 0 then
          ;
        EventCatchers := nil;
        Writeln(s_Error_reading_file_PLUGINS_CFG);
        goto Plugins2;
      end;

    GetMem(EventCatchers, EventCatchersCount*SizeOf(
      TEventCatcherInfo));

    for i := 1 to EventCatchersCount do
      with EventCatchers^[i] do
        begin
          BlockRead(F, FirstCatchedCommand, SizeOf(
            FirstCatchedCommand));
          BlockRead(F, LastCatchedCommand, SizeOf(LastCatchedCommand));
          {BlockRead(F, FirstLngIndex, SizeOf(FirstLngIndex));}
          {BlockRead(F, LastLngIndex, SizeOf(LastLngIndex));}
          {BlockRead(F, FirstDlgIndex, SizeOf(FirstDlgIndex));}
          {BlockRead(F, LastDlgIndex, SizeOf(LastDlgIndex));}
          {BlockRead(F, FirstHlpIndex, SizeOf(FirstHlpIndex));}
          {BlockRead(F, LastHlpIndex, SizeOf(LastHlpIndex));}
          BlockRead(F, FirstObjType, SizeOf(FirstObjType));
          BlockRead(F, LastObjType, SizeOf(LastObjType));
          PluginPath := Copy(ReadStr, 1, 8);
          LibHandle := 0;
          @Entry := nil;
        end;

    BlockRead(F, ArchiveViewersCount, SizeOf(ArchiveViewersCount));
    FillChar(ArchiveViewers, SizeOf(ArchiveViewers), #0);
    if IOResult <> 0 then
      begin
        Close(F);
        if IOResult = 0 then
          ;
        FreeMem(EventCatchers, EventCatchersCount*SizeOf(
          TEventCatcherInfo));
        EventCatchers := nil;
        Writeln(s_Error_reading_file_PLUGINS_CFG);
        goto Plugins2;
      end;

    j := arcFirst;
    for i := 1 to ArchiveViewersCount do
      begin
        New(ArchiveViewers[j]);
        with ArchiveViewers[j] do
          begin
            FirstTag := j;
            PluginPath := Copy(ReadStr, 1, 8);
            if IOResult <> 0 then
              begin
                Close(F);
                if IOResult = 0 then
                  ;
                FreeMem(EventCatchers, EventCatchersCount*SizeOf(
                  TEventCatcherInfo));
                EventCatchers := nil;
                Dispose(ArchiveViewers[j]);
                ArchiveViewers[j] := nil;
                Writeln(s_Error_reading_file_PLUGINS_CFG);
                goto Plugins2;
              end;
            if not LoadPluginModuleAndGetProcAddress(PluginPath,
                LibHandle, ProcNamesArchiveViewer,
              [@@FormatsCount, @@ArchiveSign, @@CreateArchiveObject,
                @@DetectCreateArchiveObject])
            then
              begin
                Writeln(s_Cannot_load_module, PluginPath);
                Dispose(ArchiveViewers[j]);
                ArchiveViewers[j] := nil;
                continue;
              end;
          end;
        for k := 1 to ArchiveViewers[j]^.FormatsCount do
          begin
            Inc(j);
            if j > arcLast then
              break;
            ArchiveViewers[j] := ArchiveViewers[j-1];
          end;
      end;

    Close(F);
    if IOResult = 0 then
      AddExitProc(DonePlugins)
    else
      begin
        FreeMem(EventCatchers, EventCatchersCount*SizeOf(
          TEventCatcherInfo));
        EventCatchers := nil;
        Writeln(s_Error_reading_file_PLUGINS_CFG);
      end;

Plugins2:

    FullPath := SourceDir+'PLUGINS2.CFG';

    DrivePanelsInfoCount := 0;
    if ExistFile(FullPath) then
      begin
        Assign(F, FullPath);
        Reset(F, 1);
        while not Eof(F) do
          begin
            Inc(DrivePanelsInfoCount);
            if DrivePanelsInfoCount > MaxDrivePanelsInfo then
              break;
            New(DrivePanelsInfo[DrivePanelsInfoCount]);
            with DrivePanelsInfo[DrivePanelsInfoCount]^ do
              begin
                PluginPath := NewStr(ReadStr);
                LibHandle := 0;
                MenuString1 := NewStr(ReadStr);
                MenuString2 := NewStr(ReadStr);
                BlockRead(F, MenuKey, SizeOf(MenuKey));
                BlockRead(F, ObjType, SizeOf(ObjType));

                if IOResult <> 0 then
                  begin
                    DoneDrivePanels(False);
                    Writeln(s_Error_reading_file_PLUGINS2_CFG);
                    break;
                  end;
              end;
          end;
        Close(F);
      end;
  end { InitPlugins };

procedure CatchersHandleCommand(Command: word);
  var
    i: integer;
    Finalization: Pointer;
  begin
    if EventCatchers <> nil then
      for i := 1 to EventCatchersCount do
        with EventCatchers^[i] do
          if (Command >= FirstCatchedCommand) and (Command <=
              LastCatchedCommand)
          then
            begin
              if Assigned(Entry) then
                begin
                  Entry(Command-FirstCatchedCommand, FirstObjType,
                    PluginPath, @DNFunctions, @DNMethods,
                    Finalization);
                  if Finalization <> nil then
                    AddExitProc(Finalization);
                end
              else
                begin
                  if (LibHandle <> 0)
                    or not LoadPluginModuleAndGetProcAddress(
                      PluginPath, LibHandle, ProcNamesEventCatcher,
                    [@@Entry])
                  then
                    begin
                      MessageBox(GetString(dlCantLoad)+PluginPath,
                        nil, mfError+mfOKButton);
                      exit;
                    end;
                  Entry(Command-FirstCatchedCommand, FirstObjType,
                    PluginPath, @DNFunctions, @DNMethods,
                    Finalization);
                  if Finalization <> nil then
                    AddExitProc(Finalization);
                end;
              exit;
            end;
  end { CatchersHandleCommand };

procedure CatchersRegisterObject(ObjType: word);
  var
    i: integer;
    Finalization: Pointer;
  begin
    if (ObjType >= otPlugins) and (ObjType <= otPluginsEnd) then
      if EventCatchers <> nil then
        for i := 1 to EventCatchersCount do
          with EventCatchers^[i] do
            if (ObjType >= FirstObjType) and (ObjType <= LastObjType)
            then
              begin
                if Assigned(Entry) then
                  begin
                    Entry($FFFF, FirstObjType, PluginPath,
                      @DNFunctions, @DNMethods, Finalization);
                    if Finalization <> nil then
                      AddExitProc(Finalization);
                  end
                else
                  begin
                    if (LibHandle <> 0)
                      or not LoadPluginModuleAndGetProcAddress(
                        PluginPath, LibHandle, ProcNamesEventCatcher,
                      [@@Entry])
                    then
                      begin
                        MessageBox(GetString(dlCantLoad)+PluginPath,
                          nil, mfError+mfOKButton);
                        exit;
                      end;
                    Entry($FFFF, FirstObjType, PluginPath,
                      @DNFunctions, @DNMethods, Finalization);
                    if Finalization <> nil then
                      AddExitProc(Finalization);
                  end;
                exit;
              end;
  end { CatchersRegisterObject };

(*** DRIVE PANELS ***)

procedure DoneDrivePanels(NeedWriteConfig: boolean);
  var
    i: integer;

  procedure WriteConfig;
    var
      i, j, k: integer;
      F: file;

    procedure WriteStr(s: String);
      begin
        BlockWrite(F, s, 1+Length(s));
      end;

    begin
      //  Writeln('Drive Panel Plugins: ',DrivePanelsInfoCount,' old, ',DrivePanelsInfo2Count,' new');
      Assign(F, SourceDir+'PLUGINS2.CFG');
      rewrite(F, 1);
      for i := 1 to DrivePanelsInfoCount do
        with DrivePanelsInfo[i]^ do
          begin
            WriteStr(PluginPath^);
            WriteStr(CnvString(MenuString1));
            WriteStr(CnvString(MenuString2));
            BlockWrite(F, MenuKey, SizeOf(MenuKey));
            BlockWrite(F, ObjType, SizeOf(ObjType));
          end;
      for i := 1 to DrivePanelsInfo2Count do
        with DrivePanelsInfo2[i]^, MenuStrings^ do
          for j := 0 to Count-1 do
            begin
              WriteStr(PluginPath^);
              WriteStr(StrPas(Strings1^[j]));
              WriteStr(StrPas(Strings2^[j]));
              BlockWrite(F, Keys^[j], SizeOf(integer));
              k := ObjType+j;
              BlockWrite(F, k, SizeOf(integer));
            end;
      Close(F);
      if IOResult <> 0 then
        Writeln(s_Error_writing_file_PLUGINS2_CFG);
    end { WriteConfig };

  begin { DoneDrivePanels }
    if NeedWriteConfig and (DrivePanelsInfo2Count > 0) then
      WriteConfig;

    for i := 1 to DrivePanelsInfoCount do
      with DrivePanelsInfo[i]^ do
        begin
          DisposeStr(PluginPath);
          DisposeStr(MenuString1);
          DisposeStr(MenuString2);

          FreeModule(LibHandle);

          Dispose(DrivePanelsInfo[i]);
          DrivePanelsInfo[i] := nil;
        end;

    for i := 1 to DrivePanelsInfo2Count do
      with DrivePanelsInfo2[i]^ do
        begin
          FreeModule(LibHandle);

          Dispose(DrivePanelsInfo2[i]);
          DrivePanelsInfo2[i] := nil;
        end;

    DrivePanelsInfoCount := 0;
    DrivePanelsInfo2Count := 0;
  end { DoneDrivePanels };

{&Delphi+}
function CreateDriveMenus(var Items: PMenuItem; var MaxL: integer):
    integer;
  var
    i, j: integer;
    s1, s2: String;

  procedure LoadLibs;
    var
      SR: lSearchRec;
      s: String;

    procedure LoadLib(const APluginPath: String);
      var
        GetMenuStrings: TGetMenuStringsProc;
      begin
        Inc(DrivePanelsInfo2Count);
        if DrivePanelsInfo2Count <= MaxDrivePanelsInfo then
          begin
            New(DrivePanelsInfo2[DrivePanelsInfo2Count]);
            with DrivePanelsInfo2[DrivePanelsInfo2Count]^ do
              begin
                PluginPath := NewStr(APluginPath);
                if LoadPluginModuleAndGetProcAddress(APluginPath,
                    LibHandle, ProcNamesDrivePanels,
                  [@@GetMenuStrings, @@CreateDriveObject,
                    @@RegisterDriveObject])
                then
                  MenuStrings := GetMenuStrings(0, 0, APluginPath,
                    @DNFunctions, @DNMethods)
                else
                  begin
                    FreeModule(LibHandle);
                    MenuStrings := nil;
                  end;
              end;
          end;
      end { LoadLib };

    function Ok: boolean;
      var
        i: integer;
      begin
        if (SR.SR.Attr and (Directory or Hidden)) = 0 then
          begin
            Result := True;
            s := SR.FullName;
            Dec(s[0], 4);
            for i := 1 to DrivePanelsInfoCount do
              if DrivePanelsInfo[i]^.PluginPath^ = s then
                begin
                  Result := False;
                  exit;
                end;
            for i := 1 to DrivePanelsInfo2Count do
              if DrivePanelsInfo2[i]^.PluginPath^ = s then
                begin
                  Result := False;
                  exit;
                end;
          end
        else
          Result := False;
      end { Ok: };

    begin { LoadLibs }
      lFindFirst(SourceDir+'P_*.DLL', AnyFile, SR);
      while DOSError = 0 do
        begin
          if Ok then
            LoadLib(s);
          lFindNext(SR);
        end;
      lFindClose(SR);
    end { LoadLibs };

  begin { CreateDriveMenus }
    Result := 0;
    LoadLibs;

    for i := DrivePanelsInfoCount downto 1 do
      with DrivePanelsInfo[i]^ do
        begin
          Items := NewItem(CnvString(MenuString1), CnvString(
            MenuString2), MenuKey, 65000+i, 0, Items);
          MaxL := Max(CStrLen(MenuString1^), MaxL);
          Inc(Result);
        end;

    for i := DrivePanelsInfo2Count downto 1 do
      with DrivePanelsInfo2[i]^, MenuStrings^ do
        if MenuStrings <> nil then
          begin
            for j := Count-1 downto 0 do
              begin
                s1 := StrPas(Strings1^[j]);
                s2 := StrPas(Strings2^[j]);
                Items := NewItem(s1, s2, Keys^[j], 65000+
                  MaxDrivePanelsInfo+i, 0, Items);
                MaxL := Max(CStrLen(s1), MaxL);
                Inc(Result);
              end;
          end;
  end { CreateDriveMenus };

function CreateDriveObject(i: integer; AOwner: Pointer; Num: integer):
    Pointer;
  type
    PDrivePanelsInfoShort = ^TDrivePanelsInfoShort;
    TDrivePanelsInfoShort = record
      PluginPath: PString;
      LibHandle: integer;
      CreateDriveObject: TCreateDriveObjectProc;
      RegisterDriveObject: TRegisterDriveObjectProc;
      ObjType: word;
      end;
  var
    P: PDrivePanelsInfoShort;
    GetMenuStrings: TGetMenuStringsProc;
  begin
    Result := nil;

    if i <= MaxDrivePanelsInfo then
      P := PDrivePanelsInfoShort(DrivePanelsInfo[i])
    else
      P := PDrivePanelsInfoShort(DrivePanelsInfo2[i-
        MaxDrivePanelsInfo]);

    if P <> nil then
      with P^ do
        begin
          if (LibHandle = 0)
            and not LoadPluginModuleAndGetProcAddress(PluginPath^,
              LibHandle, ProcNamesDrivePanels,
            [@@GetMenuStrings, @@CreateDriveObject,
              @@RegisterDriveObject])
          then
            begin
              MessageBox(GetString(dlCantLoad)+PluginPath^, nil,
                mfError+mfOKButton);
              FreeModule(LibHandle);
            end
          else
            begin
              ObjType := RegisterDriveObject($FFFF, 0, PluginPath^,
                @DNFunctions, @DNMethods);
              Result := CreateDriveObject(0, 0, PluginPath^,
                @DNFunctions, @DNMethods, AOwner, Num);
            end;
        end;
  end { CreateDriveObject };

procedure DrivePanelsRegisterObject(AObjType: word);
  var
    i: integer;
    GetMenuStrings: TGetMenuStringsProc;
  begin
    for i := 1 to DrivePanelsInfoCount do
      with DrivePanelsInfo[i]^ do
        if ObjType = AObjType then
          if (LibHandle = 0)
            and not LoadPluginModuleAndGetProcAddress(PluginPath^,
              LibHandle, ProcNamesDrivePanels,
            [@@GetMenuStrings, @@CreateDriveObject,
              @@RegisterDriveObject])
          then
            begin
              MessageBox(GetString(dlCantLoad)+PluginPath^, nil,
                mfError+mfOKButton);
              FreeModule(LibHandle);
            end
          else
            RegisterDriveObject($FFFF, 0, PluginPath^, @DNFunctions,
              @DNMethods);
  end { DrivePanelsRegisterObject };
{&Delphi-}

(*** EDITOR EVENT HOOKS ***)

{&Delphi+}
const
  MaxEditorEventHookCount = 32;

var
  EditorEventHookArray: array[1..MaxEditorEventHookCount] of Pointer
    {TEditorEventHook};
  EditorEventHookCount: integer;

function SetEditorEventHook(EditorEventHook: TEditorEventHook):
    boolean;
  begin
    if EditorEventHookCount < MaxEditorEventHookCount then
      begin
        Result := True;
        Inc(EditorEventHookCount);
        EditorEventHookArray[EditorEventHookCount] :=
          @EditorEventHook;
      end
    else
      Result := False;
  end;

procedure RemoveEditorEventHook(EditorEventHook: TEditorEventHook);
  var
    i: integer;
  begin
    for i := 1 to EditorEventHookCount do
      if EditorEventHookArray[i] = @EditorEventHook then
        begin
          for i := i+1 to EditorEventHookCount do
            EditorEventHookArray[i-1] := EditorEventHookArray[i];
          Dec(EditorEventHookCount);
          exit;
        end;
  end;

function ProcessEditorEventHook(var Event: TEvent; Editor: Pointer):
    boolean;
  var
    i: integer;
  begin
    for i := 1 to EditorEventHookCount do
      if TEditorEventHook(EditorEventHookArray[i])(Event, Editor)
      then
        begin
          Result := True;
          exit;
        end;
    Result := False;
  end;
{&Delphi-}

(*** ARCHIVE VIEWER ***)

{&Delphi+}
function DetectCreateArchiveObject: PARJArchive;
  var
    j: integer;
  begin
    for j := arcFirst to arcLast do
      if ArchiveViewers[j] <> nil then
        begin
          Result := ArchiveViewers[j]^.DetectCreateArchiveObject;
          if Result <> nil then
            exit;
        end;
    Result := nil;
  end;

function GetArchiveTagBySign(Sign: TStr4): byte;
  var
    j: integer;
  begin
    for j := arcFirst to arcLast do
      if ArchiveViewers[j] <> nil then
        with ArchiveViewers[j] do
          if ArchiveSign(j-FirstTag) = Sign then
            begin
              Result := j;
              exit;
            end;
    Result := arcUNK;
  end;

function GetArchiveByTag(Id: byte): PARJArchive;
  begin
    if ArchiveViewers[Id] = nil then
      Result := nil
    else
      with ArchiveViewers[Id] do
        Result := CreateArchiveObject(Id-FirstTag);
  end;
{&Delphi-}

procedure PluginRegisterObject(ObjType: word);
  begin
    CatchersRegisterObject(ObjType);
    DrivePanelsRegisterObject(ObjType);
  end;

begin
  InitPlugins
end.
