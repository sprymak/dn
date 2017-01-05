unit _Model2;
(******

DN/2 Plugin Interface - functional model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{ сюда попадают те объекты, которые DN/2 напрямую не экспортирует, но }
{ ссылки на которые всё-таки возможно получить какими-либо способами }

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Model1;

type
  PFilePanel = ^TFilePanel;
  TFilePanel = packed record
    VMT: PViewVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    OldSizeX: integer;
    isValid, MSelect, SelectFlag, Loaded, ChangeLocked: boolean;
    InfoView, DirView, DriveLine: PView;
    Delta, OldDelta, OldPos, DeltaX: longInt;
    Files: PFilesCollection;
    SortMode: byte;
    DirectoryName, OldDirectory: String;
    FileMask: String;
    SearchParam: TQuickSearchData;
    ScrollBar: PScrollBar;
    DrawDisableLvl, SelNum, LineLength: longInt;
    SelectedLen, PackedLen: TSize;
    WasActive, PosChanged, CommandEnabling,
    ViewEnabled: boolean;
    TotalInfo, FreeSpace: TSize;
    PanelFlags: word;
    LastDriveFlags: word;
    Drive: PDrive;
    ForceReading: boolean;
    DriveState: word;
    LastCurPos: TPoint;
    end;

  PCommandLine = ^TCommandLine;
  TCommandLine = packed record
    VMT: PViewVMT;
    ObjectIsInited: boolean;
    Owner: PGroup;
    Next: PView;
    Origin: TPoint;
    Size: TPoint;
    Cursor: TPoint;
    GrowMode: byte;
    DragMode: byte;
    HelpCtx: AWord;
    State: AWord;
    Options: AWord;
    EventMask: AWord;
    UpTmr: TEventTimer;
    UpdTicks: longInt;
    Dir: String;
    DeltaX, CurX: longInt;
    Overwrite: boolean;
    LineType: TLineType;
    end;

implementation

end.
