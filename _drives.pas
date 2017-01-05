unit _Drives;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}

interface

uses
  {Use32,} _Defines;

type
  PDrive = ^TDrive;
  TDrive = object(TObject)
    Owner: Pointer;
    Prev: PDrive;
    DriveType: (dtUndefined, dtDisk, dtFind, dtTemp, dtList, dtArc, dtLink, dtArvid);
    CurDir: String;
    DIZOwner: String;
    NoMemory: Boolean;
    Flags: AWord;
    LFNLen: Byte;
    EXTLen: Byte;
    DirFLP, FilFLP: AWord;
    Param, OldParam: Byte;
    InNum: Byte;
    SizeX: LongInt;
    {$IFDEF OS2}
    ShowLogNames: Boolean;
    {$ENDIF}
    constructor Init(ADrive: Byte; AOwner: Pointer; Num: Byte);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure KillUse; virtual;
    procedure lChDir(ADir: String); virtual;
    function GetDir: String; virtual;
    function GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String; var FreeSpace, TotalInfo: String): PCollection; virtual;
    procedure CopyFiles(Files: PCollection; Own: PView; MoveMode: Boolean); virtual;
    procedure CopyFilesInto(Files: PCollection; Own: PView; MoveMode: Boolean); virtual;
    procedure EraseFiles(Files: PCollection); virtual;
    procedure UseFile(P: PFileRec; Command: Word); virtual;
    procedure GetFreeSpace(var S: String); virtual;
    function Disposable: Boolean; virtual;
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    procedure GetFull(var B; P:PFileRec; C, SC:Word); virtual;
    procedure GetEmpty(var B; SC: Word); virtual;
    function CalcLengthWithoutName: Integer; virtual;
    function CalcLength: Integer; virtual;
    procedure RereadDirectory(S: String); virtual;
    procedure MakeTop(var S: String); virtual;
    procedure GetDown(var B; C: Word; P: PFileRec); virtual;
    procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    function GetRealDir: String; virtual;
    procedure MakeDir; virtual;
    function isUp: Boolean; virtual;
    procedure ChangeUp(var S: String); virtual;
    procedure ChangeRoot; virtual;
    function GetFullFlags: Word; virtual;
    procedure EditDescription(PF: PFileRec); virtual;
    procedure GetDirLength(PF: PFileRec); virtual;
    procedure GetParam(N: Byte); virtual;
    {destructor Done; virtual;}
  end;

  PXDoubleWindow = ^TXDoubleWindow;
  TXDoubleWindow = object(TWindow)
    isValid: Boolean;
    LeftView, RightView: PHideView;
    RDrive, LDrive: Byte;
    Separator: PSeparator;
    LPanel, RPanel: PFilePanel;
    Info: PHideView;
    NetInfo: PHideView;
    LTree: PHTreeView;
    QView: PFileViewer;
    OldBounds: TRect;
    OldPanelBounds: TRect;
    PanelZoomed: Boolean;
    LType, RType: AInt;
    constructor Init(Bounds: TRect; ANumber, ADrive: Integer);
    procedure InitLeftView(R: TRect);
    procedure InitRightView(R: TRect);
    procedure InitInterior;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    {function Valid(C: Word): Boolean; virtual;}
    {procedure ChangeBounds(var Bounds: TRect); virtual;}
    procedure HandleCommand(var Event: TEvent);
    {procedure SetState(AState: Word; Enable: Boolean); virtual;}
    {function  GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
  end;

implementation

uses
  _DNFuncs;

end.
