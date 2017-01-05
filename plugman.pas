library PlugMan;
(******

Plugin Manager 2.0
Written by Cat 2:5030/1326.13
(EventCatcher plugin for DN/2)

******)

{.$DEFINE DEBUG}
{&Delphi+}
{&Use32+}

{Cat
   05-10-2001 - начато добавление плагинов к ДН-у
   19-10-2001 - плагины типа EventCatcher
   21-10-2001 - плагины используют индексы в файлах ресурсов (Lng и Dlg)
   27-10-2001 - плагины используют индекс в файле помощи (Hlp)
   04-11-2001 - при установке плагина изменяем меню
   26-12-2001 - для разных языков используем различные строки меню
   17-01-2002 - теперь можем менять не только главное меню,
                но и меню редактора и электронной таблицы
   17-01-2002 - решил, что не следует изменять языковой файл и файл
                ресурсов (за исключением добавления/удаления строк меню),
                поэтому индексы в Lng, Dlg, Hlp файлах более не используются,
                а ресурсы плагинов теперь будут содержаться в файлах *.REZ,
                к которым Plugin Manager не будет иметь никакого отношения
                Версия плагинов EventCatcher - 2.0
   23-01-2002 - плагины типа ArchiveViewer
   10-05-2002 - вся структура плагинов переделана заново, без использования
                DN2CAT.DLL
}

uses
  {$IFDEF OS2}Os2Base, {$ENDIF}
  {$IFDEF WIN32}Windows, {$ENDIF}
  Dos, VpSysLow, Commands, ObjType,
  _Defines, _DNFuncs, _Model1,
  _Objects, _Streams, _Collect, _Views, _Menus, _Dialogs, _Apps;

const
  type_EventCatcher = 1;
  type_ArchiveViewer = 2;

  {
const
  dlPlugins=Integer(dlPlugins0);
  dlPluginsEnd=32000;
  dlgPlugins=Integer(dlgPlugins0);
  dlgPluginsEnd=32000;
  hcPlugins=25000;
  hcPluginsEnd=45000;
}

var
  KilledEventCatchersCount: integer;

type
  PStringArray = ^TStringArray;
  PWordArray = ^TWordArray;
  TStringArray = array[0..0] of ShortString;
  TWordArray = array[0..0] of word;

  PCommandUsed = ^TCommandUsed;
  {PLngIndexUsed=^TLngIndexUsed;}
  {PDlgIndexUsed=^TDlgIndexUsed;}
  {PHlpIndexUsed=^THlpIndexUsed;}
  PObjTypeUsed = ^TObjTypeUsed;
  TCommandUsed = array[cmPlugins..cmPluginsEnd] of boolean;
  {TLngIndexUsed=array[dlPlugins..dlPluginsEnd] of Boolean;}
  {TDlgIndexUsed=array[dlgPlugins..dlgPluginsEnd] of Boolean;}
  {THlpIndexUsed=array[hcPlugins..hcPluginsEnd] of Boolean;}
  TObjTypeUsed = array[otPlugins..otPluginsEnd] of boolean;

  PPluginListItem = ^TPluginListItem;
  TPluginListItem = object(TObject)
    PluginType: longInt;
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
    Description: String[255];
    Installed: word;
    RegFilePresent: boolean;
    MenuStr1: PStringArray;
    MenuStr2: PStringArray;
    MenuKey: PWordArray;
    MenuHelpCtx: PWordArray;
    MenuType: PWordArray;
    MenuWantBeAfter: PWordArray;
    Constructor Init(PT: longInt; FCC, LCC,
      {FLI, LLI, FDI, LDI, FHI, LHI,}FOT, LOT: word; RFP: boolean; PP:
      String);
    destructor Done; virtual;
    end;

  PPluginList = ^TPluginList;
  TPluginList = object(TListBox)
    function GetText(Item: integer; MaxLen: integer): String;
      virtual;
    procedure FocusItem(Item: integer); virtual;
    end;

  PPluginManager = ^TPluginManager;
  TPluginManager = object(TDialog)
    CommandUsed: PCommandUsed;
    {LngIndexUsed: PLngIndexUsed;}
    {DlgIndexUsed: PDlgIndexUsed;}
    {HlpIndexUsed: PHlpIndexUsed;}
    ObjTypeUsed: PObjTypeUsed;

    ListBox: PListBox;
    InstallButton: PButton;
    function MakeCollection: PCollection;
    Constructor Init;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    end;

  TMenuChanger = procedure (Item: PPluginListItem; MenuView:
    PMenuView; CurMenuType: byte);

const
  PluginManager: PPluginManager = nil;

function OverwriteConfigFile: boolean;
  var
    Stream: PStream;
    i, W: integer;
  begin
    with DNFunctions^, SomeObjects2^, SomeObjects3^ do
      begin
        Stream := New(PBufStream, Init(SourceDir+'PLUGINS.CFG',
          Open_Access_WriteOnly or Open_Share_DenyReadWrite, 1024));

        W := EventCatchersCount-KilledEventCatchersCount;
        Stream^.Write(W, SizeOf(W));
        for i := 1 to EventCatchersCount do
          with EventCatchers^[i], Stream^ do
            if FirstCatchedCommand <> $FFFF then
              begin
                Write(FirstCatchedCommand, SizeOf(
                  FirstCatchedCommand));
                Write(LastCatchedCommand, SizeOf(LastCatchedCommand));
                {Write(FirstLngIndex, SizeOf(FirstLngIndex));}
                {Write(LastLngIndex, SizeOf(LastLngIndex));}
                {Write(FirstDlgIndex, SizeOf(FirstDlgIndex));}
                {Write(LastDlgIndex, SizeOf(LastDlgIndex));}
                {Write(FirstHlpIndex, SizeOf(FirstHlpIndex));}
                {Write(LastHlpIndex, SizeOf(LastHlpIndex));}
                Write(FirstObjType, SizeOf(FirstObjType));
                Write(LastObjType, SizeOf(LastObjType));
                Write(PluginPath, 1+Length(PluginPath));
              end;

        W := 0;
        for i := arcFirst to arcLast do
          if (ArchiveViewers[i] <> nil) and (ArchiveViewers[i] <>
              ArchiveViewers[i-1])
          then
            Inc(W);
        Stream^.Write(W, SizeOf(W));
        for i := arcFirst to arcLast do
          if (ArchiveViewers[i] <> nil) and (ArchiveViewers[i] <>
              ArchiveViewers[i-1])
          then
            with ArchiveViewers[i]^, Stream^ do
              Write(PluginPath, 1+Length(PluginPath));
      end;

    Stream^.Truncate;
    Result := (Stream^.Status = stOK);
    Dispose(Stream, Done);
  end { OverwriteConfigFile: };

{ дубликат этой функции находится в MenuEdit.pas, изменения согласовывать }
function ChangeMenuResource(MenuChanger: TMenuChanger; Item: Pointer;
    MenuView: PMenuView; dlgMenu: integer; CurMenuType: byte):
    boolean;
  var
    P: Pointer;
    i: integer;
    NextResourceOffset: longInt;
    MenuResourceOffset: longInt;
    RestResourceSize: longInt;
    MenuResourceSize: longInt;
    Index: PIndexArray;
  begin
    if (PPluginListItem(Item)^.PluginType and type_EventCatcher) = 0
    then
      begin
        Result := True;
        exit;
      end;

    { изменяем меню }
    MenuChanger(Item, MenuView, CurMenuType);

    with DNFunctions^, SomeObjects1^ do
      begin
        { копируем индекс ресурсов во временный буфер }
        GetMem(Index, Resource^.Count*SizeOf(longInt));
        Move(Resource^.Index^, Index^, Resource^.Count*SizeOf(
          longInt));

        { находим ресурс, располагающийся следом за меню }
        NextResourceOffset := MaxLongInt;
        MenuResourceOffset := Index^[dlgMenu];

        for i := 0 to Resource^.Count-1 do
          if (Index^[i] > MenuResourceOffset) and (Index^[i] <
              NextResourceOffset)
          then
            NextResourceOffset := Index^[i];
        RestResourceSize := PStream(ResourceStream)^.GetSize-
          NextResourceOffset;
        MenuResourceSize := NextResourceOffset-MenuResourceOffset;

        { переоткрываем файл ресурсов, чтобы можно было в него писать }
        repeat
          PStream(ResourceStream)^.Close;
          PStream(ResourceStream)^.DoOpen(Open_Access_ReadWrite or
            Open_Share_DenyWrite);
          if PStream(ResourceStream)^.Status = stOK then
            break;
          MessageBox(GetString(integer(dlPlugins5)), nil, mfError+
            mfOKButton);
        until False;

        if NextResourceOffset <> MaxLongInt then
          begin

            { сдвигаем на место меню ресурсы, располагающиеся за ним, а само меню сдвигаем в конец файла }
            for i := 0 to Resource^.Count-1 do
              if Index^[i] > MenuResourceOffset then
                Dec(Index^[i], MenuResourceSize);
            Inc(Index^[dlgMenu], RestResourceSize);

            { удаляем меню из файла ресурсов - осторожно!! }
            if MaxAvail < RestResourceSize then
              begin
                Result := False;
                exit;
              end;
            GetMem(P, RestResourceSize);
            PStream(ResourceStream)^.Seek(NextResourceOffset);
            PStream(ResourceStream)^.Read(P^, RestResourceSize);
            if PStream(ResourceStream)^.Status <> stOK then
              begin
                FreeMem(P {, RestResourceSize});
                Result := False;
                exit;
              end;
            PStream(ResourceStream)^.Seek(0);
            PStream(ResourceStream)^.Write(Resource^.Count, 2);
            PStream(ResourceStream)^.Write(Index^, Resource^.Count*
              SizeOf(longInt));
            PStream(ResourceStream)^.Seek(MenuResourceOffset);
            PStream(ResourceStream)^.Write(P^, RestResourceSize);
            FreeMem(P {, RestResourceSize});
          end
        else
          PStream(ResourceStream)^.Seek(MenuResourceOffset);

        { запихиваем меню обратно в Dlg-файл }
        PStream(ResourceStream)^.Put(MenuView);
        Result := (PStream(ResourceStream)^.Status = stOK);

        { если всё прошло успешно, то возвращаем индекс ресурсов из временного буфера }
        Move(Index^, Resource^.Index^, Resource^.Count*SizeOf(
          longInt));
        FreeMem(Index {, Resource^.Count*SizeOf(LongInt)});
      end;
  end { ChangeMenuResource };

procedure InstallMenu(Item: PPluginListItem; MenuView: PMenuView;
    CurMenuType: byte);
  var
    s: String;
    SubMenuPtr, MenuItemPtr: ^PMenuItem;
    Command: word;
    Count: word;

  procedure WalkMenu(var MenuItem: PMenuItem);
    var
      MenuItemPtr: ^PMenuItem;
      Command: word;
    begin
      with Item^ do
        begin
          MenuItemPtr := @MenuItem;
          while MenuItemPtr^ <> nil do
            begin
              if MenuItemPtr^^.Name <> nil then
                if MenuItemPtr^^.Command = 0 then
                  WalkMenu(MenuItemPtr^^.SubMenu^.Items)
                else
                  for Command := 0 to LastCatchedCommand-
                      FirstCatchedCommand
                  do
                    if MenuItemPtr^^.Command = MenuWantBeAfter^[
                        Command]
                    then
                      begin
                        MenuItemPtr := @MenuItemPtr^^.Next;
                        MenuItemPtr^:= DNFunctions^.NewItem(MenuStr1^[
                          Command], MenuStr2^[Command], MenuKey^[
                          Command], FirstCatchedCommand+Command,
                          MenuHelpCtx^[Command], MenuItemPtr^);
                        MenuWantBeAfter^[Command] := -
                          MenuWantBeAfter^[Command];
                          { для обработанных элементов ставим отрицательное значение }
                      end;
              MenuItemPtr := @MenuItemPtr^^.Next;
            end;
        end;
    end { WalkMenu };

  begin { InstallMenu }
    { сначала просматриваем меню и добавляем те строчки, }
    { которые хотят вставиться в какое-то определённое место }
    WalkMenu(MenuView^.Menu^.Items);

    { считаем, сколько ещё осталось добавить }
    Count := 0;
    with Item^ do
      for Command := 0 to LastCatchedCommand-FirstCatchedCommand do
        if MenuWantBeAfter^[Command] < 0 then
          MenuWantBeAfter^[Command] := -MenuWantBeAfter^[Command]
        else if MenuType^[Command] = CurMenuType then
          begin
            MenuWantBeAfter^[Command] := 0;
            Inc(Count);
          end;

    { если всё, что надо, уже добавлено, то тихо выходим }
    if Count = 0 then
      exit;

    { в противном случае... }
    with DNFunctions^, SomeObjects1^ do
      begin
        s := GetString(integer(dlPlugins4));
        SubMenuPtr := @PMenuBar(MenuView)^.Menu^.Items;

        { ищем подменю "плагины", если такого нету - создаём }
        while SubMenuPtr^ <> nil do
          if (SubMenuPtr^^.Name <> nil) and (SubMenuPtr^^.Name^ = s)
              and (SubMenuPtr^^.Command = 0)
          then
            break
          else
            SubMenuPtr := @SubMenuPtr^^.Next;
        if SubMenuPtr^ = nil then
          SubMenuPtr^:= NewSubMenu(s, hcNoContext, NewMenu(nil), nil);

        { спускаемся до конца меню и вставляем туда нужные строки }
        MenuItemPtr := @SubMenuPtr^^.SubMenu^.Items;
        while MenuItemPtr^ <> nil do
          MenuItemPtr := @MenuItemPtr^^.Next;
        with Item^ do
          for Command := 0 to LastCatchedCommand-FirstCatchedCommand do
            if (MenuStr1^[Command] <> '') and (MenuType^[Command] =
                CurMenuType)
            then
              begin
                MenuItemPtr^:= NewItem(MenuStr1^[Command], MenuStr2^[
                  Command], MenuKey^[Command], FirstCatchedCommand+
                  Command, MenuHelpCtx^[Command], nil);
                MenuItemPtr := @MenuItemPtr^^.Next;
              end;

        { если текущий элемент не определён - устанавливаем первый текущим }
        if SubMenuPtr^^.SubMenu^.Default = nil then
          SubMenuPtr^^.SubMenu^.Default := SubMenuPtr^^.SubMenu^.
            Items;
      end;
  end { InstallMenu };

procedure UnInstallMenu(Item: PPluginListItem; MenuView: PMenuView;
    CurMenuType: byte);

  { рекурсивное очищение подменю от ненужных команд }
  procedure ClearSubMenu(Menu: PMenu);
    var
      MenuItemPtr: ^PMenuItem;
      MenuItem: PMenuItem;
    begin
      MenuItemPtr := @Menu^.Items;
      with DNFunctions^ do
        while MenuItemPtr^ <> nil do
          begin
            { если это подменю - очищаем его рекурсивно }
            { если в результате очистки удалили все пункты - уничтожаем подменю }
            if (MenuItemPtr^^.Name <> nil) and (MenuItemPtr^^.
                Command = 0)
            then
              begin
                ClearSubMenu(MenuItemPtr^^.SubMenu);
                if MenuItemPtr^^.SubMenu^.Items = nil then
                  begin
                    MenuItem := MenuItemPtr^^.Next;
                    if MenuItemPtr^^.Name <> nil then
                      DisposeStr(MenuItemPtr^^.Name);
                    DisposeMenu(MenuItemPtr^^.SubMenu);
                    Dispose(MenuItemPtr^);
                    if Menu^.Default = MenuItemPtr^ then
                      Menu^.Default := MenuItem;
                    MenuItemPtr^:= MenuItem;
                  end
                else
                  MenuItemPtr := @MenuItemPtr^^.Next;
              end
              { если это обычный пункт меню и соответствует диапазону команд - удаляем }
            else if (MenuItemPtr^^.Command >= Item^.
                FirstCatchedCommand) and (MenuItemPtr^^.Command <=
                Item^.LastCatchedCommand)
            then
              begin
                MenuItem := MenuItemPtr^^.Next;
                if MenuItemPtr^^.Name <> nil then
                  DisposeStr(MenuItemPtr^^.Name);
                if MenuItemPtr^^.Param <> nil then
                  DisposeStr(MenuItemPtr^^.Param);
                Dispose(MenuItemPtr^);
                if Menu^.Default = MenuItemPtr^ then
                  Menu^.Default := MenuItem;
                MenuItemPtr^:= MenuItem;
              end
            else
              MenuItemPtr := @MenuItemPtr^^.Next;
          end;
      if Menu^.Default = nil then
        Menu^.Default := Menu^.Items;
    end { ClearSubMenu };

  begin { UnInstallMenu }
    ClearSubMenu(MenuView^.Menu);
  end { UnInstallMenu };

function Install(Item: PPluginListItem): boolean;
  var
    FullPath: String;
    P: PArchiveViewerInfo;
    i, j, k: integer;
    B: boolean;
  begin
    with DNFunctions^, SomeObjects2^, SomeObjects3^ do
      begin
        if (Item^.PluginType and type_EventCatcher) <> 0 then
          begin
            { если нету регистрационного файла - не будем устанавливать }
            { если плагинов слишком много - то же самое }
            if not Item^.RegFilePresent or (EventCatchersCount >=
                60000)
            then
              begin
                Result := False;
                exit;
              end;

            { добавляем плагин в список установленных }
            Inc(EventCatchersCount);
            ReallocMem(EventCatchers, EventCatchersCount*SizeOf(
              TEventCatcherInfo));
            with EventCatchers^[EventCatchersCount] do
              begin
                FirstCatchedCommand := Item^.FirstCatchedCommand;
                LastCatchedCommand := Item^.LastCatchedCommand;
                {FirstLngIndex:=Item^.FirstLngIndex;}
                {LastLngIndex:=Item^.LastLngIndex;}
                {FirstDlgIndex:=Item^.FirstDlgIndex;}
                {LastDlgIndex:=Item^.LastDlgIndex;}
                {FirstHlpIndex:=Item^.FirstHlpIndex;}
                {LastHlpIndex:=Item^.LastHlpIndex;}
                FirstObjType := Item^.FirstObjType;
                LastObjType := Item^.LastObjType;
                PluginPath := Item^.PluginPath;
                LibHandle := 0;
                @Entry := nil;

                Item^.Installed := EventCatchersCount;
              end;

            { переписываем конфигурационный файл }
            Result := OverwriteConfigFile;
          end;

        if (Item^.PluginType and type_ArchiveViewer) <> 0 then
          begin
            { грузим плагин }
            New(P);
            with P^ do
              begin
                PluginPath := Item^.PluginPath;
                FullPath := SourceDir+PluginPath+'.DLL'#0;
                {$IFDEF OS2}
                if (DosLoadModule(nil, 0, @FullPath[1], LibHandle) <> 0)
                  or (DosQueryProcAddr(LibHandle, 0, 'FormatsCount',
                    @FormatsCount) <> 0)
                  or (DosQueryProcAddr(LibHandle, 0, 'ArchiveSign',
                    @ArchiveSign) <> 0)
                  or (DosQueryProcAddr(LibHandle, 0,
                    'CreateArchiveObject', @CreateArchiveObject) <> 0)
                  or (DosQueryProcAddr(LibHandle, 0,
                    'DetectCreateArchiveObject',
                    @DetectCreateArchiveObject) <> 0)
                then
                  begin
                    MessageBox(GetString(integer(dlCantLoad))+
                      PluginPath, nil, mfError+mfOKButton);
                    Dispose(P);
                    Result := False;
                    exit;
                  end;
                {$ENDIF}
                {$IFDEF WIN32}
                LibHandle := LoadLibrary(@FullPath[1]);
                if LibHandle < HINSTANCE_ERROR then
                  begin
                    MessageBox(GetString(integer(dlCantLoad))+
                      PluginPath, nil, mfError+mfOKButton);
                    Dispose(P);
                    Result := False;
                    exit;
                  end;
                @FormatsCount := GetProcAddress(LibHandle,
                  'FormatsCount');
                @ArchiveSign := GetProcAddress(LibHandle,
                  'ArchiveSign');
                @CreateArchiveObject := GetProcAddress(LibHandle,
                  'CreateArchiveObject');
                @DetectCreateArchiveObject := GetProcAddress(
                  LibHandle, 'DetectCreateArchiveObject');
                if not Assigned(FormatsCount) or not Assigned(
                    ArchiveSign) or not Assigned(CreateArchiveObject) or
                    not Assigned(DetectCreateArchiveObject)
                then
                  begin
                    MessageBox(GetString(integer(dlCantLoad))+
                      PluginPath, nil, mfError+mfOKButton);
                    Dispose(P);
                    Result := False;
                    exit;
                  end;
                {$ENDIF}
              end;

            { спрашиваем, сколько форматов данный плагин поддерживает }
            { и ищем свободное место, куда бы его приткнуть }
            k := P^.FormatsCount;
            for i := arcFirst to arcLast-k do
              begin
                B := False;
                for j := i to i+k-1 do
                  begin
                    B := B or (ArchiveViewers[j] <> nil);
                    if B then
                      break;
                  end;
                if not B then
                  break;
              end;
            if B then
              begin
                {$IFDEF OS2}
                DosFreeModule(P^.LibHandle);
                {$ENDIF}
                {$IFDEF WIN32}
                FreeLibrary(P^.LibHandle);
                {$ENDIF}
                Dispose(P);
                Result := False;
                exit;
              end;
            P^.FirstTag := i;
            for j := i to i+k-1 do
              ArchiveViewers[j] := P;

            { переписываем конфигурационный файл }
            Result := OverwriteConfigFile;
            if Result then
              Item^.Installed := i
            else
              begin
                for j := i to i+k-1 do
                  ArchiveViewers[j] := nil;
                {$IFDEF OS2}
                DosFreeModule(P^.LibHandle);
                {$ENDIF}
                {$IFDEF WIN32}
                FreeLibrary(P^.LibHandle);
                {$ENDIF}
                for j := i to i+k-1 do
                  ArchiveViewers[j] := nil;
                Dispose(P);
              end;
          end;
      end;
  end { Install };

function UnInstall(Item: PPluginListItem): boolean;
  var
    i, j: integer;
  begin
    with DNFunctions^, SomeObjects2^, SomeObjects3^ do
      begin
        if ((Item^.PluginType and type_EventCatcher) <> 0) and (Item^.
            Installed >= 1) and (Item^.Installed <=
            EventCatchersCount)
        then
          begin
            { убираем возможность запустить плагин }
            with EventCatchers^[Item^.Installed] do
              if FirstCatchedCommand <> $FFFF then
                begin
                  FirstCatchedCommand := $FFFF;
                  Inc(KilledEventCatchersCount);
                  Item^.Installed := 0;
                end;

            { переписываем конфигурационный файл }
            Result := OverwriteConfigFile;
          end;

        if (Item^.PluginType and type_ArchiveViewer) <> 0 then
          begin
            if (Item^.Installed < arcFirst) or (Item^.Installed >
                arcLast)
            then
              begin
                Result := False;
                exit;
              end;

            { выгружаем плагин }
            { вычищаем все упоминания о нём }
            j := ArchiveViewers[Item^.Installed]^.FirstTag;
            for i := j+1 to j+ArchiveViewers[Item^.Installed]^.
                FormatsCount-1
            do
              ArchiveViewers[i] := nil;
            { надо бы выгрузить библиотеку, но она ещё может использоваться }
            { поэтому понадеемся, что при выходе из программы система всё подчистит }
            (*
          {$IFDEF OS2}
          DosFreeModule(ArchiveViewers[J]^.LibHandle);
          {$ENDIF}
          {$IFDEF WIN32}
          FreeLibrary(ArchiveViewers[J]^.LibHandle);
          {$ENDIF}
          *)
            Dispose(ArchiveViewers[j]);
            ArchiveViewers[j] := nil;

            { переписываем конфигурационный файл }
            Result := OverwriteConfigFile;
            if Result then
              Item^.Installed := 0;
          end;
      end;
  end { UnInstall };

Constructor TPluginListItem.Init(PT: longInt; FCC, LCC,
    {FLI, LLI, FDI, LDI, FHI, LHI,}FOT, LOT: word; RFP: boolean; PP:
    String);
  var
    CommandCount: word;
  begin
    _TObject^.Init(nil, @Self);
    PluginType := PT;
    FirstCatchedCommand := FCC;
    LastCatchedCommand := LCC;
    {FirstLngIndex:=FLI;}
    {LastLngIndex:=LLI;}
    {FirstDlgIndex:=FDI;}
    {LastDlgIndex:=LDI;}
    {FirstHlpIndex:=FHI;}
    {LastHlpIndex:=LHI;}
    FirstObjType := FOT;
    LastObjType := LOT;
    RegFilePresent := RFP;
    PluginPath := PP;
    DNFunctions^.UpStr(PluginPath);
    Description := '';
    Installed := 0;
    CommandCount := LastCatchedCommand-FirstCatchedCommand+1;
    GetMem(MenuStr1, CommandCount*SizeOf(String));
    GetMem(MenuStr2, CommandCount*SizeOf(String));
    GetMem(MenuKey, CommandCount*SizeOf(word));
    GetMem(MenuHelpCtx, CommandCount*SizeOf(word));
    GetMem(MenuType, CommandCount*SizeOf(word));
    GetMem(MenuWantBeAfter, CommandCount*SizeOf(word));
    FillChar(MenuStr1^, CommandCount*SizeOf(String), #0);
    FillChar(MenuStr2^, CommandCount*SizeOf(String), #0);
    FillChar(MenuKey^, CommandCount*SizeOf(word), #0);
    FillChar(MenuHelpCtx^, CommandCount*SizeOf(word), #0);
    FillChar(MenuType^, CommandCount*SizeOf(word), #0);
    FillChar(MenuWantBeAfter^, CommandCount*SizeOf(word), #0);
  end { TPluginListItem.Init };

destructor TPluginListItem.Done;
  var
    CommandCount: word;
  begin
    CommandCount := LastCatchedCommand-FirstCatchedCommand+1;
    FreeMem(MenuStr1 {, CommandCount*SizeOf(String)});
    FreeMem(MenuStr2 {, CommandCount*SizeOf(String)});
    FreeMem(MenuKey {, CommandCount*SizeOf(Word)});
    FreeMem(MenuHelpCtx {, CommandCount*SizeOf(Word)});
    FreeMem(MenuType {, CommandCount*SizeOf(Word)});
    FreeMem(MenuWantBeAfter {, CommandCount*SizeOf(Word)});
    _TObject^.VMT^.Done(0, @Self);
  end;

function TPluginList.GetText(Item: integer; MaxLen: integer): String;
  begin
    with PPluginListItem(List^.At(Item))^ do
      begin
        if Description <> '' then
          Result := Description
        else
          Result := PluginPath;
        if Installed <> 0 then
          Result := '* '+Result
        else
          Result := '  '+Result;
      end;
    if Length(Result) > MaxLen then
      SetLength(Result, MaxLen);
  end;

procedure TPluginList.FocusItem(Item: longInt);
  begin
    _TListBox^.VMT^.FocusItem(Item, @Self);
    if Owner <> nil then
      with PPluginManager(Owner)^.InstallButton^, DNFunctions^ do
        begin
          DisposeStr(Title);
          if PPluginListItem(List^.At(Item))^.Installed = 0 then
            Title := NewStr(GetString(integer(dlPlugins8)))
          else
            Title := NewStr(GetString(integer(dlPlugins9)));
          DrawView;
        end;
  end;

function TPluginManager.MakeCollection: PCollection;
  label
    1, 2, 3, l1, L2, L3, L4, L5;
  var
    i, j: integer;
    WW: array[1..16] of word;
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
    Name: String[8];
    Description: String[255];
    Item: PPluginListItem;
    SR: lSearchRec;
    Stream: PStream;
    MyLngId, PluginLngId: String[255];
  begin { TPluginManager.MakeCollection: }
    Result := New(PCollection, Init(16, 16));

    with DNFunctions^, SomeObjects2^, SomeObjects3^ do
      begin
        MyLngId := LngId;
        UpStr(MyLngId);

        { просматриваем установленные плагины EventCatcher }
        for i := 1 to EventCatchersCount do
          with EventCatchers^[i] do
            if (FirstCatchedCommand <> $FFFF) then
              begin
                Item := New(PPluginListItem, Init(type_EventCatcher,
                FirstCatchedCommand, LastCatchedCommand,
                {FirstLngIndex, LastLngIndex,}
                {FirstDlgIndex, LastDlgIndex,}
                {FirstHlpIndex, LastHlpIndex,}
                FirstObjType, LastObjType,
                False,
                PluginPath));
                Item^.Installed := i;
                Result^.Insert(Item);
                for j := FirstCatchedCommand to LastCatchedCommand do
                  CommandUsed^[j] := True;
                {
              if FirstLngIndex<>$FFFF then
                for J:=FirstLngIndex to LastLngIndex do
                  LngIndexUsed^[J]:=True;
              if FirstDlgIndex<>$FFFF then
                for J:=FirstDlgIndex to LastDlgIndex do
                  DlgIndexUsed^[J]:=True;
              if FirstHlpIndex<>$FFFF then
                for J:=FirstHlpIndex to LastHlpIndex do
                  HlpIndexUsed^[J]:=True;
              }
                if FirstObjType <> $FFFF then
                  for j := FirstObjType to LastObjType do
                    ObjTypeUsed^[j] := True;
              end;

        { просматриваем установленные плагины ArchiveViewer }
        for i := arcFirst to arcLast do
          if (ArchiveViewers[i] <> nil) and (ArchiveViewers[i] <>
              ArchiveViewers[i-1])
          then
            begin
              Item := New(PPluginListItem, Init(type_ArchiveViewer,
              0, 0,
              $FFFF, $FFFF,
              False,
              ArchiveViewers[i]^.PluginPath));
              Item^.Installed := i;
              Result^.Insert(Item);
            end;

        { просматриваем регистрационные файлы }
        lFindFirst(SourceDir+'*.REG', AnyFile, SR);
        while DOSError = 0 do
          begin
            Stream := New(PBufStream, Init(SourceDir+SR.FullName,
              stOpenRead, 512));
            Stream^.Read(WW, SizeOf(WW));
            Stream^.Read(Name, SizeOf(Name));
            UpStr(Name);
            Stream^.ReadStrV(Description);
            if (Stream^.Status <> stOK) or (WW[7] <> $777A {Magic}) or (
                WW[8] <> $DEF2 {Magic})
            then
              MessageBox(GetString(integer(dlCantLoad))+SR.FullName,
                nil, mfError+mfOKButton)
            else
              begin
                { проверяем, не попался ли уже этот плагин среди установленных }
                for i := 0 to Result^.Count-1 do
                  if PPluginListItem(Result^.At(i))^.PluginPath =
                      Name
                  then
                    begin
                      Item := PPluginListItem(Result^.At(i));
                      Item^.Description := Description;
                      Item^.RegFilePresent := True;
                      goto 1;
                    end;

                { ищем незанятую область команд нужной длины }
                for i := cmPlugins to cmPluginsEnd-WW[1] do
                  begin
                    for j := i to i+WW[1] do
                      if CommandUsed^[j] then
                        break;
                    if not CommandUsed^[j] then
                      begin
                        FirstCatchedCommand := i;
                        LastCatchedCommand := i+WW[1];
                        for j := i to i+WW[1] do
                          CommandUsed^[j] := True;
                        goto l1;
                      end;
                  end;
                MessageBox(GetString(integer(dlPlugins1)), nil,
                  mfError+mfOKButton);
                Dispose(Stream, Done);
                goto 2;
l1:

                { ищем незанятую область индексов Lng-файла нужной длины }
                {
              if WW[2]=0 then
                begin
                  FirstLngIndex:=$FFFF;
                  LastLngIndex:=$FFFF;
                end
              else
                begin
                  for I:=dlPlugins to dlPluginsEnd-WW[2] do
                    begin
                      for J:=I to I+WW[2]-1 do
                        if LngIndexUsed^[J] then
                          Break;
                      if not LngIndexUsed^[J] then
                        begin
                          FirstLngIndex:=I;
                          LastLngIndex:=I+WW[2]-1;
                          for J:=I to I+WW[2]-1 do
                            LngIndexUsed^[J]:=True;
                          goto L2;
                        end;
                    end;
                  MessageBox(GetString(Integer(dlPlugins1)), nil, mfError+mfOKButton);
                  Dispose(Stream, Done);
                  goto 2;
                  L2:
                end;
              }

                { ищем незанятую область индексов Dlg-файла нужной длины }
                {
              if WW[3]=0 then
                begin
                  FirstDlgIndex:=$FFFF;
                  LastDlgIndex:=$FFFF;
                end
              else
                begin
                  for I:=dlgPlugins to dlgPluginsEnd-WW[3] do
                    begin
                      for J:=I to I+WW[3]-1 do
                        if DlgIndexUsed^[J] then
                          Break;
                      if not DlgIndexUsed^[J] then
                        begin
                          FirstDlgIndex:=I;
                          LastDlgIndex:=I+WW[3]-1;
                          for J:=I to I+WW[3]-1 do
                            DlgIndexUsed^[J]:=True;
                          goto L3;
                        end;
                    end;
                  MessageBox(GetString(Integer(dlPlugins1)), nil, mfError+mfOKButton);
                  Dispose(Stream, Done);
                  goto 2;
                  L3:
                end;
              }

                { ищем незанятую область индексов Hlp-файла нужной длины }
                {
              if WW[4]=0 then
                begin
                  FirstHlpIndex:=$FFFF;
                  LastHlpIndex:=$FFFF;
                end
              else
                begin
                  for I:=hcPlugins to hcPluginsEnd-WW[4] do
                    begin
                      for J:=I to I+WW[4]-1 do
                        if HlpIndexUsed^[J] then
                          Break;
                      if not HlpIndexUsed^[J] then
                        begin
                          FirstHlpIndex:=I;
                          LastHlpIndex:=I+WW[4]-1;
                          for J:=I to I+WW[4]-1 do
                            HlpIndexUsed^[J]:=True;
                          goto L4;
                        end;
                    end;
                  MessageBox(GetString(Integer(dlPlugins1)), nil, mfError+mfOKButton);
                  Dispose(Stream, Done);
                  goto 2;
                  L4:
                end;
              }

                { ищем незанятую область регистрационных кодов объектов нужной длины }
                if WW[9] = 0 then
                  begin
                    FirstObjType := $FFFF;
                    LastObjType := $FFFF;
                  end
                else
                  begin
                    for i := otPlugins to otPluginsEnd-WW[9]-1 do
                      begin
                        for j := i to i+WW[9] do
                          if ObjTypeUsed^[j] then
                            break;
                        if not ObjTypeUsed^[j] then
                          begin
                            FirstObjType := i;
                            LastObjType := i+WW[9];
                            for j := i to i+WW[9] do
                              ObjTypeUsed^[j] := True;
                            goto L5;
                          end;
                      end;
                    MessageBox(GetString(integer(dlPlugins1)), nil,
                      mfError+mfOKButton);
                    Dispose(Stream, Done);
                    goto 2;
L5:
                  end;

                Item := New(PPluginListItem, Init(WW[6],
                FirstCatchedCommand, LastCatchedCommand,
                {FirstLngIndex, LastLngIndex,}
                {FirstDlgIndex, LastDlgIndex,}
                {FirstHlpIndex, LastHlpIndex,}
                FirstObjType, LastObjType,
                True,
                Name));
                Result^.Insert(Item);

1:

                { дочитываем из регистрационного файла информацию о меню }
                { повторяем до тех пор, пока не найдём нужный язык }
                { если языки закончились, а нужный не найден - используем последний }
                with Stream^, Item^ do
                  for j := 0 to WW[5] do
                    begin
                      ReadStrV(PluginLngId);
                      UpStr(PluginLngId);

                      for i := 0 to WW[1] do
                        begin
                          ReadStrV(MenuStr1^[i]);
                          ReadStrV(MenuStr2^[i]);
                          Read(MenuKey^[i], SizeOf(word));
                          Read(MenuHelpCtx^[i], SizeOf(word));
                          Read(MenuType^[i], SizeOf(word));
                          Read(MenuWantBeAfter^[i], SizeOf(word));
                        end;

                      if Stream^.Status <> stOK then
                        begin
                          MessageBox(GetString(integer(dlCantLoad))+SR.
                            FullName, nil, mfError+mfOKButton);
                          Dispose(Item, Done);
                          goto 3;
                        end;

                      if PluginLngId = MyLngId then
                        break;
                    end;

                Item^.Description := Description;
              end;
3:
            Dispose(Stream, Done);
            lFindNext(SR);
          end;
2:
        lFindClose(SR);
      end;
  end { TPluginManager.MakeCollection: };

Constructor TPluginManager.Init;
  var
    R: TRect;
    ScrollBar: PScrollBar;
  begin
    with DNFunctions^ do
      begin
        PluginManager := @Self;

        R.Assign(0, 0, 58, 20);
        _TDialog^.Init(R, GetString(integer(dlPlugins0)), nil, @Self);

        New(CommandUsed);
        {New(LngIndexUsed);}
        {New(DlgIndexUsed);}
        {New(HlpIndexUsed);}
        New(ObjTypeUsed);
        FillChar(CommandUsed^, SizeOf(CommandUsed^), #0);
        {FillChar(LngIndexUsed^, SizeOf(LngIndexUsed^), #0);}
        {FillChar(DlgIndexUsed^, SizeOf(DlgIndexUsed^), #0);}
        {FillChar(HlpIndexUsed^, SizeOf(HlpIndexUsed^), #0);}
        FillChar(ObjTypeUsed^, SizeOf(ObjTypeUsed^), #0);

        Number := GetWinNumber;
        Options := Options or ofCentered;
        R.Assign(5, 17, 22, 19);
        InstallButton := New(PButton, Init(R, GetString(integer(
          dlPlugins9)), cmOK, bfDefault));
        Insert(InstallButton);
        R.Assign(24, 17, 41, 19);
        Insert(New(PButton, Init(R, GetString(integer(dlCloseButton)),
          cmCancel, bfNormal)));
        R.Assign(55, 1, 56, 16);
        ScrollBar := New(PScrollBar, Init(R));
        Insert(ScrollBar);
        R.Assign(2, 1, 55, 16);
        ListBox := New(PPluginList, Init(R, 1, ScrollBar));
        ListBox^.NewLisT(MakeCollection);
        Insert(ListBox);
      end;
  end { TPluginManager.Init };

Constructor TPluginManager.Load(var s: TStream);
  var
    FocusOn: integer;
  begin
    Init;
    s.Read(Origin, SizeOf(Origin));
    s.Read(Size, SizeOf(Size));
    s.Read(FocusOn, SizeOf(FocusOn));
    if FocusOn >= ListBox^.Range then
      FocusOn := ListBox^.Range-1;
    if FocusOn >= 0 then
      ListBox^.FocusItem(FocusOn);
  end;

procedure TPluginManager.Store(var s: TStream);
  var
    FocusOn: integer;
  begin
    FocusOn := ListBox^.Focused;
    s.Write(Origin, SizeOf(Origin));
    s.Write(Size, SizeOf(Size));
    s.Write(FocusOn, SizeOf(FocusOn));
  end;

procedure TPluginManager.HandleEvent(var Event: TEvent);
  {$IFDEF DEBUG}
  const
    InstStr: array[boolean] of String[7] =
    ('Remove', 'Install');

  function ItoS(l: longInt): String;
    begin
      Str(l, Result);
    end;
  {$ENDIF}

  var
    PluginListItem: PPluginListItem;
    P: PString;
  begin
    _TDialog^.VMT^.HandleEvent(Event, @Self);
    if Event.What = evCommand then
      case Event.Command of
        cmOK:
          begin
            PluginListItem := PPluginListItem(ListBox^.List^.At(
              ListBox^.Focused));
            with PluginListItem^, DNFunctions^, SomeObjects1^ do
              begin
                {$IFDEF DEBUG}
                MessageBox(InstStr[Installed = 0]+': "'+PluginPath+
                  '"'
                +^M'   cmd = '+ItoS(FirstCatchedCommand)+'..'+ItoS(
                  LastCatchedCommand)
                +^M'   obj = '+ItoS(FirstObjType)+'..'+ItoS(
                  LastObjType)
                {+^M'   lng = '+ItoS(FirstLngIndex)+'..'+ItoS(LastLngIndex)}
                {+^M'   dlg = '+ItoS(FirstDlgIndex)+'..'+ItoS(LastDlgIndex)}
                {+^M'   hlp = '+ItoS(FirstHlpIndex)+'..'+ItoS(LastHlpIndex)}
                  , nil, mfInformation+mfOKButton);
                {$ENDIF}
                P := PString(@PluginPath);
                if Installed <> 0 then
                  if (PluginListItem^.PluginPath <> 'PLUGMAN')
                    and UnInstall(PluginListItem)
                    and ChangeMenuResource(UnInstallMenu,
                      PluginListItem, MenuBar, integer(dlgMainMenu), 0)
                    and ChangeMenuResource(UnInstallMenu,
                      PluginListItem, PMenuView(LoadResource(integer(
                      dlgEditorMenu))), integer(dlgEditorMenu), 1)
                    and ChangeMenuResource(UnInstallMenu,
                      PluginListItem, PMenuView(LoadResource(integer(
                      dlgWkzMenuBar))), integer(dlgWkzMenuBar), 2)
                  then
                    ListBox^.DrawView
                  else
                    MessageBox(GetString(integer(dlPlugins3)), @P,
                      mfError+mfOKButton)
                else if Install(PluginListItem)
                  and ChangeMenuResource(InstallMenu, PluginListItem,
                    MenuBar, integer(dlgMainMenu), 0)
                  and ChangeMenuResource(InstallMenu, PluginListItem,
                    PMenuView(LoadResource(integer(dlgEditorMenu))),
                    integer(dlgEditorMenu), 1)
                  and ChangeMenuResource(InstallMenu, PluginListItem,
                    PMenuView(LoadResource(integer(dlgWkzMenuBar))),
                    integer(dlgWkzMenuBar), 2)
                then
                  ListBox^.DrawView
                else
                  MessageBox(GetString(integer(dlPlugins2)), @P,
                    mfError+mfOKButton);
                with InstallButton^ do
                  begin
                    DisposeStr(Title);
                    if Installed = 0 then
                      Title := NewStr(GetString(integer(dlPlugins8)))
                    else
                      Title := NewStr(GetString(integer(dlPlugins9)));
                    DrawView;
                  end;
              end;
          end;
        cmCancel:
          Close;
        cmGetName:
          PString(Event.InfoPtr)^:= DNFunctions^.GetString(integer(
            dlPlugins0));
      end {case};
  end { TPluginManager.HandleEvent };

destructor TPluginManager.Done;
  begin
    Dispose(CommandUsed);
    {Dispose(LngIndexUsed);}
    {Dispose(DlgIndexUsed);}
    {Dispose(HlpIndexUsed);}
    Dispose(ObjTypeUsed);
    _TDialog^.VMT^.Done(0, @Self);
    PluginManager := nil;
  end;

{&Cdecl+}
procedure CatchCommand(Command, ObjType: SmallWord; const PluginName:
    ShortString; Functions, Methods: Pointer; var FinalizationProc:
    Pointer);
  const
    Initialized: boolean = False;
  begin
    FinalizationProc := nil;
    if not Initialized then
      begin
        InitDNFunctions(Functions, Methods);

        with DNFunctions^ do
          if APIVersion < 4 then
            begin
              MessageBox(
                'Newer version of DN/2 is required for start this plugin'^M^M
                +
              'Для запуска этого плагина требуется более новая версия DN/2'
                , nil, mfError+mfOKButton);
              exit;
            end;

        Initialized := True;

        TransportVMT(_TObject^.VMT, TypeOf(TObject), TypeOf(
          TPluginListItem), TObject_VMTSize);
        TransportVMT(_TListBox^.VMT, TypeOf(TListBox), TypeOf(
          TPluginList), TListBox_VMTSize);
        TransportVMT(_TDialog^.VMT, TypeOf(TDialog), TypeOf(
          TPluginManager), TDialog_VMTSize);

        TransportVMT(_TBufStream^.VMT, TypeOf(TBufStream), TypeOf(
          TBufStream), TBufStream_VMTSize);
        TransportVMT(_TCollection^.VMT, TypeOf(TCollection), TypeOf(
          TCollection), TCollection_VMTSize);
        TransportVMT(_TScrollBar^.VMT, TypeOf(TScrollBar), TypeOf(
          TScrollBar), TScrollBar_VMTSize);
        TransportVMT(_TButton^.VMT, TypeOf(TButton), TypeOf(TButton),
          TButton_VMTSize);

        KilledEventCatchersCount := 0;
      end;

    if Command = 0 then
      if PluginManager = nil then
        PDesktop(DNFunctions^.SomeObjects1^.Desktop)^.Insert(New(
          PPluginManager, Init))
      else
        PluginManager^.Select;
  end { CatchCommand };

exports
CatchCommand Name'CatchCommand';
{&Cdecl-}

begin
end.
