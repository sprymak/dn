unit _Drives;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Objects, _Streams, _Collect, _Views;

type
  PDrive = ^TDrive;
  TDrive = object(TObject)
    Owner: Pointer;
    Prev: PDrive;
    DriveType: TDriveType;
    CurDir: String;
    DizOwner: String;
    NoMemory: boolean;
    Flags: AWord;
    LFNLen: byte;
    EXTLen: byte;
    DirFLP, FilFLP: AWord;
    Param, OldParam: byte;
    innum: byte;
    SizeX: longInt;
    {$IFDEF OS2}
    ShowLogNames: boolean;
    {$ENDIF}
    Constructor Init(ADrive: byte; AOwner: Pointer; Num: byte);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream); virtual;
    procedure KillUse; virtual;
    procedure lChDir(ADir: String); virtual;
    function GetDir: String; virtual;
    function GetDirectory: function (SortMode, PanelFlags: integer;
      const FileMask: String;
    var TotalInfo: TSize; var FreeSpace: String; Obj: Pointer):
      PCollection; virtual;
    procedure CopyFiles(Files: PCollection; Own: PView; MoveMode:
      boolean); virtual;
    procedure CopyFilesInto(Files: PCollection; Own: PView; MoveMode:
      boolean); virtual;
    procedure EraseFiles(Files: PCollection); virtual;
    procedure UseFile(P: PFileRec; Command: word); virtual;
    procedure GetFreeSpace(var s: String); virtual;
    function Disposable: boolean; virtual;
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    procedure GetFull(var B; P: PFileRec; C, Sc: word); virtual;
    procedure GetEmpty(var B; Sc: word); virtual;
    function CalcLengthWithoutName: integer; virtual;
    function CalcLength: integer; virtual;
    procedure RereadDirectory(s: String); virtual;
    procedure MakeTop(var s: String); virtual;
    procedure GetDown(var B; C: word; P: PFileRec); virtual;
    procedure HandleCommand(Command: word; InfoPtr: Pointer);
      virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    function GetRealDir: String; virtual;
    procedure MakeDir; virtual;
    function isUp: boolean; virtual;
    procedure ChangeUp(var s: String); virtual;
    procedure ChangeRoot; virtual;
    function GetFullFlags: word; virtual;
    procedure EditDescription(PF: PFileRec); virtual;
    procedure GetDirLength(PF: PFileRec); virtual;
    procedure GetParam(n: byte); virtual;
    function OpenDirectory(const Dir: String): PDrive; virtual;
    {destructor Done; virtual;}
    end;

  PFindDrive = ^TFindDrive;
  TFindDrive = object(TDrive)
    isDisposable: boolean;
    Files: PFilesCollection;
    Dirs: PSortedCollection;
    ListFile: PString;
    UpFile: PFileRec;
    AMask, AWhat: PString;
    Constructor Init(const AName: String; ADirs: PCollection; AFiles:
      PFilesCollection; Num: byte);
    Constructor InitList(const AName: String);
    Constructor Load(var s: TStream);
    {procedure Store(var S: TStream); virtual;}
    {destructor Done; virtual;}
    procedure NewUpFile;
    {procedure lChDir(ADir: String); virtual;}
    {function GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String; var FreeSpace, TotalInfo: String): PCollection; virtual;}
    {procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure UseFile(P: PFileRec; Command: Word); virtual;}
    {function Disposable: Boolean; virtual;}
    {function GetRealName: String; virtual;}
    {function GetInternalName: String; virtual;}
    {function GetDir: string; virtual;}
    {procedure MakeDir; virtual;}
    {procedure GetFull(var B; P: PFileRec; C, SC: Word); virtual;}
    {procedure GetEmpty(var B; SC: Word); virtual;}
    {procedure GetFreeSpace(var S: String); virtual;}
    {function CalcLengthWithoutName: Integer; virtual;}
    {function CalcLength: Integer; virtual;}
    {procedure MakeTop(var S: String); virtual;}
    {function IsUp: Boolean; virtual;}
    {procedure ChangeUp(var S: String); virtual;}
    {procedure ChangeRoot; virtual;}
    {procedure RereadDirectory(S: String); virtual;}
    {function GetFullFlags: Word; virtual;}
    {procedure GetDirInfo(var B: TDiskInfoRec); virtual;}
    {procedure GetParam(N: Byte); virtual;}
    end;

  PTempDrive = ^TTempDrive;
  TTempDrive = object(TFindDrive)
    Constructor Init(Num: byte);
    Constructor Load(var s: TStream);
    {procedure Store(var S: TStream); virtual;}
    {destructor Done; virtual;}
    {procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {function GetRealName: String; virtual;}
    {function GetInternalName: String; virtual;}
    {procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure EraseFiles(AFiles: PCollection); virtual;}
    {procedure GetDirInfo(var B: TDiskInfoRec); virtual;}
    {procedure GetParam(N: Byte); virtual;}
    end;

  PArcDrive = ^TArcDrive;
  TArcDrive = object(TDrive)
    ArcName: String;
    VArcName: String;
    AType: Pointer {PARJArchive};
    Files: Pointer {PDirStorage};
    KillAfterUse: boolean;
    FakeKillAfterUse: boolean;
    ArcDate: longInt;
    ArcSize: Comp;
    ForceRescan: boolean;
    Password: String;
    Constructor Init(const AName, VAName: String; ViewMode: byte);
    Constructor InitCol(PC: Pointer {PDirStorage}; const AName,
      VAName: String);
    Constructor Load(var s: TStream);
    {procedure Store(var S: TStream); virtual;}
    {destructor Done; virtual;}
    {procedure RereadDirectory(S: String); virtual;}
    {procedure KillUse; virtual;}
    function ReadArchive: boolean;
    {procedure lChDir(ADir: String); virtual;}
    {function GetDir: String; virtual;}
    {function GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String; var FreeSpace, TotalInfo: String): PCollection; virtual;}
    function Exec(Prg, Cmd: String; Lst: AnsiString; B: boolean):
      boolean;
    {procedure UseFile(P: PFileRec; Command: Word); virtual;}
    function MakeListFile(PC: PCollection; UseUnp: boolean; var B:
      boolean): AnsiString;
    {procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure EraseFiles(AFiles: PCollection); virtual;}
    {procedure GetFull(var B; P: PFileRec; C, SC: Word); virtual;}
    {procedure GetEmpty(var B; SC: Word); virtual;}
    {function CalcLengthWithoutName: Integer; virtual;}
    {function CalcLength: Integer; virtual;}
    {function GetRealName: String; virtual;}
    {function GetInternalName: String; virtual;}
    {procedure MakeTop(var S: String); virtual;}
    {procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;}
    {procedure MakeDir; virtual;}
    {function IsUp: Boolean; virtual;}
    {procedure ChangeUp(var S: String); virtual;}
    {procedure ChangeRoot; virtual;}
    procedure ExtractFiles(AFiles: PCollection; ExtrDir: String; Own:
      PView; Options: byte);
    {procedure GetFreeSpace(var S: String); virtual;}
    {procedure GetDirInfo(var B: TDiskInfoRec); virtual;}
    {function GetFullFlags: Word; virtual;}
    {procedure GetDirLength(PF: PFileRec); virtual;}
    {procedure GetParam(N: Byte); virtual;}
    procedure StdMsg4;
    end;

  PArvidDrive = ^TArvidDrive;
  TArvidDrive = object(TDrive)
    Name: PString;
    Stream: PStream;
    CurFile: longInt;
    CurDirPos: longInt;
    PosTableOfs: longInt;
    CurFileNum: AWord;
    CurLevel: AWord;
    CurDate: longInt;
    KillAfterUse: boolean;
    filetype: TAvdType;
    D: TTdrHeader;
    AVT: TAvtHeader;
    TapeFmt: AWord;
    TapeTotalTime: AWord;
    TapeRecordedTime: AWord;
    TotFiles: longInt;
    TotLen: longInt;
    CurDirCellPos: longInt;
    Constructor Init(const AName: String; Num: byte);
    {destructor Done; virtual;}
    {procedure lChDir(ADir: String); virtual;}
    {function GetDir: String; virtual;}
    {function GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String; var FreeSpace, TotalInfo: String): PCollection; virtual;}
    {procedure RereadDirectory(S: String); virtual;}
    {procedure KillUse; virtual;}
    {procedure UseFile(P: PFileRec; Command: Word); virtual;}
    {procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
    {procedure EraseFiles(AFiles: PCollection); virtual;}
    {function  GetRealName: String; virtual;}
    {function  GetInternalName: String; virtual;}
    {procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;}
    {procedure MakeDir; virtual;}
    {function  IsUp: Boolean; virtual;}
    {procedure ChangeUp(var S: String); virtual;}
    {procedure ChangeRoot; virtual;}
    {procedure GetFreeSpace(var S: String); virtual;}
    {procedure GetDirInfo(var B: TDiskInfoRec); virtual;}
    {procedure EditDescription(PF: PFileRec); virtual;}
    {procedure GetDirLength(PF: PFileRec); virtual;}
    {procedure GetParam(n: byte); virtual;}
    procedure SeekDirectory;
    end;

  (*
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
*)

implementation

uses
  _DNFuncs;

Constructor TDrive.Init(ADrive: byte; AOwner: Pointer; Num: byte);
  begin
    _TDrive^.Init(ADrive, AOwner, Num, nil, @Self);
  end;

Constructor TDrive.Load(var s: TStream);
  begin
    _TDrive^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TDrive.Store(var s: TStream);
  assembler; {&Frame-}
asm
end;

procedure TDrive.KillUse;
  assembler; {&Frame-}
asm
end;

procedure TDrive.lChDir(ADir: String);
  assembler; {&Frame-}
asm
end;

function TDrive.GetDir: String;
  assembler; {&Frame-}
asm
end;

function TDrive.GetDirectory: function (SortMode, PanelFlags:
    integer; const FileMask: String;
  var TotalInfo: TSize; var FreeSpace: String; Obj: Pointer):
    PCollection;
  assembler; {&Frame-}
asm
end;

procedure TDrive.CopyFiles(Files: PCollection; Own: PView; MoveMode:
    boolean);
  assembler; {&Frame-}
asm
end;

procedure TDrive.CopyFilesInto(Files: PCollection; Own: PView;
    MoveMode: boolean);
  assembler; {&Frame-}
asm
end;

procedure TDrive.EraseFiles(Files: PCollection);
  assembler; {&Frame-}
asm
end;

procedure TDrive.UseFile(P: PFileRec; Command: word);
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetFreeSpace(var s: String);
  assembler; {&Frame-}
asm
end;

function TDrive.Disposable: boolean;
  assembler; {&Frame-}
asm
end;

function TDrive.GetRealName: String;
  assembler; {&Frame-}
asm
end;

function TDrive.GetInternalName: String;
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetFull(var B; P: PFileRec; C, Sc: word);
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetEmpty(var B; Sc: word);
  assembler; {&Frame-}
asm
end;

function TDrive.CalcLengthWithoutName: integer;
  assembler; {&Frame-}
asm
end;

function TDrive.CalcLength: integer;
  assembler; {&Frame-}
asm
end;

procedure TDrive.RereadDirectory(s: String);
  assembler; {&Frame-}
asm
end;

procedure TDrive.MakeTop(var s: String);
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetDown(var B; C: word; P: PFileRec);
  assembler; {&Frame-}
asm
end;

procedure TDrive.HandleCommand(Command: word; InfoPtr: Pointer);
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetDirInfo(var B: TDiskInfoRec);
  assembler; {&Frame-}
asm
end;

function TDrive.GetRealDir: String;
  assembler; {&Frame-}
asm
end;

procedure TDrive.MakeDir;
  assembler; {&Frame-}
asm
end;

function TDrive.isUp: boolean;
  assembler; {&Frame-}
asm
end;

procedure TDrive.ChangeUp(var s: String);
  assembler; {&Frame-}
asm
end;

procedure TDrive.ChangeRoot;
  assembler; {&Frame-}
asm
end;

function TDrive.GetFullFlags: word;
  assembler; {&Frame-}
asm
end;

procedure TDrive.EditDescription(PF: PFileRec);
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetDirLength(PF: PFileRec);
  assembler; {&Frame-}
asm
end;

procedure TDrive.GetParam(n: byte);
  assembler; {&Frame-}
asm
end;

function TDrive.OpenDirectory(const Dir: String): PDrive; virtual;
  assembler; {&Frame-}
asm
end;

Constructor TFindDrive.Init(const AName: String; ADirs: PCollection;
    AFiles: PFilesCollection; Num: byte);
  begin
    _TFindDrive^.Init(AName, _Model1.PCollection(ADirs), _Model1.
      PFilesCollection(AFiles), Num, nil, @Self);
  end;

Constructor TFindDrive.InitList(const AName: String);
  begin
    _TFindDrive^.InitList(AName, nil, @Self);
  end;

Constructor TFindDrive.Load(var s: TStream);
  begin
    _TFindDrive^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TFindDrive.NewUpFile;
  begin
    _TFindDrive^.NewUpFile(@Self);
  end;

Constructor TTempDrive.Init(Num: byte);
  begin
    _TTempDrive^.Init(Num, nil, @Self);
  end;

Constructor TTempDrive.Load(var s: TStream);
  begin
    _TTempDrive^.Load(_Model1.TStream(s), nil, @Self);
  end;

Constructor TArcDrive.Init(const AName, VAName: String; ViewMode:
    byte);
  begin
    _TArcDrive^.Init(AName, VAName, ViewMode, nil, @Self);
  end;

Constructor TArcDrive.InitCol(PC: Pointer {PDirStorage}; const AName,
    VAName: String);
  begin
    _TArcDrive^.InitCol(PC, AName, VAName, nil, @Self);
  end;

Constructor TArcDrive.Load(var s: TStream);
  begin
    _TArcDrive^.Load(_Model1.TStream(s), nil, @Self);
  end;

function TArcDrive.ReadArchive: boolean;
  begin
    Result := _TArcDrive^.ReadArchive(@Self);
  end;

function TArcDrive.Exec(Prg, Cmd: String; Lst: AnsiString; B:
    boolean): boolean;
  begin
    Result := _TArcDrive^.Exec(Prg, Cmd, Lst, B, @Self);
  end;

function TArcDrive.MakeListFile(PC: PCollection; UseUnp: boolean;
    var B: boolean): AnsiString;
  begin
    Result := _TArcDrive^.MakeListFile(_Model1.PCollection(PC),
      UseUnp, B, @Self);
  end;

procedure TArcDrive.ExtractFiles(AFiles: PCollection; ExtrDir:
    String; Own: PView; Options: byte);
  begin
    _TArcDrive^.ExtractFiles(_Model1.PCollection(AFiles), ExtrDir,
      _Model1.PView(Own), Options, @Self);
  end;

procedure TArcDrive.StdMsg4;
  begin
    _TArcDrive^.StdMsg4(@Self);
  end;

Constructor TArvidDrive.Init(const AName: String; Num: byte);
  begin
    _TArvidDrive^.Init(AName, Num, nil, @Self);
  end;

procedure TArvidDrive.SeekDirectory;
  begin
    _TArvidDrive^.SeekDirectory(@Self);
  end;

end.
