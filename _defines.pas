unit _Defines;
(******

DN/2 Plugin Interface - consts & type defines
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi-}
{&Use32+}

interface

uses
  {$IFDEF WIN32}Windows, {$ENDIF}
  VpSysLow;

const

  { TStream access modes }

  stCreate = $FFFF { $DC0B }; { Create new file }
  stOpenRead = Open_Access_ReadOnly or open_share_DenyNone;
    { Read access only }
  stOpenWrite = Open_Access_WriteOnly or open_share_DenyNone;
    { Write access only }
  stOpen = Open_Access_ReadWrite or open_share_DenyNone;
    { Read and write access }
  stOpenPacked = Open_Access_ReadWrite+1;
    { Read access only, packed files too }

  { File share mode constants }
  fmClean = $FF00; { Mask to clean low byte of file mode constatns }

  fmOpenMode = $FFF0;
    { Mask to apply fmReadOnly/fmWriteOnly/fmReadWrite }
  fmReadOnly = Open_Access_ReadOnly; { Open read-only file }
  fmWriteOnly = Open_Access_WriteOnly; { Open file for write only }
  fmReadWrite = Open_Access_ReadWrite;
    { Open file as for read, as for write }
  fmPacked = Open_Access_ReadWrite+1; { Open a packed file, if can }

  fmDeny = $FF0F; { Mask to apply fmDenyXXX }
  fmDenyAll = Open_Share_DenyReadWrite; { Exclusive file use }
  fmDenyWrite = Open_Share_DenyWrite; { Deny write access }
  fmDenyRead = Open_Share_DenyRead; { Deny read access }
  fmDenyNone = open_share_DenyNone; { Deny no access }
  fmDenyChild = open_share_DenyNone; { Don't give right's to child }

  { TStream error codes }

  stOK = 0; { No error }
  stError = -1; { Access error }
  stInitError = -2; { Cannot initialize stream }
  stReadError = -3; { Read beyond end of stream }
  stWriteError = -4; { Cannot expand stream }
  stGetError = -5; { Get of unregistered object type }
  stPutError = -6; { Put of unregistered object type }
  stSeekError = -7; { Stream seek error }
  stOpenError = -8; { Stream open error }

  { Event codes }

  evMouseDown = $0001;
  evMouseUp = $0002;
  evMouseMove = $0004;
  evMouseAuto = $0008;
  evKeyDown = $0010;
  evCommand = $0100;
  evBroadcast = $0200;

  { Event masks }

  evNothing = $0000;
  evMouse = $000F;
  evKeyboard = $0010;
  evMessage = $FF00;

  { Message box classes }

  mfWarning = $0000; { Display a Warning box }
  mfError = $0001; { Dispaly a Error box }
  mfInformation = $0002; { Display an Information Box }
  mfConfirmation = $0003; { Display a Confirmation Box }
  mfQuery = $0004;
  mfAbout = $0005;
  mfSysError = $0006;

  { Message box button flags }

  mfYesButton = $0100; { Put a Yes button into the dialog }
  mfOKButton = $0200; { Put an OK button into the dialog }
  mfNoButton = $0400; { Put a No button into the dialog }
  mfCancelButton = $8000; { Put a Cancel button into the dialog }
  mfNextDButton = $0800;
  mfAppendButton = $1000;
  mf2YesButton = $2000;
  mfAllButton = $4000;

  mfYesNoCancel = mfYesButton+mfNoButton+mfCancelButton;
  { Standard Yes, No, Cancel dialog }
  mfYesNoConfirm = mfYesButton+mfNoButton+mfConfirmation;
  { Standard Yes, No  confirmation }
  mfOKCancel = mfOKButton+mfCancelButton;
  { Standard OK, Cancel dialog }

  { TView State masks }

  sfVisible = $0001;
  sfCursorVis = $0002;
  sfCursorIns = $0004;
  sfShadow = $0008;
  sfActive = $0010;
  sfSelected = $0020;
  sfFocused = $0040;
  sfDragging = $0080;
  sfDisabled = $0100;
  sfModal = $0200;
  sfDefault = $0400;
  sfExposed = $0800;

  { TView Option masks }

  ofSelectable = $0001;
  ofTopSelect = $0002;
  ofFirstClick = $0004;
  ofFramed = $0008;
  ofPreProcess = $0010;
  ofPostProcess = $0020;
  ofBuffered = $0040;
  ofTileable = $0080;
  ofCenterX = $0100;
  ofCenterY = $0200;
  ofCentered = $0300;
  ofValidate = $0400;
  ofSecurity = $0800;
  ofVersion = $3000;
  ofVersion10 = $0000;
  ofVersion20 = $1000;

  { TView GrowMode masks }

  gfGrowLoX = $01;
  gfGrowLoY = $02;
  gfGrowHiX = $04;
  gfGrowHiY = $08;
  gfGrowAll = $0F;
  gfGrowRel = $10;

  { TView DragMode masks }

  dmDragMove = $01;
  dmDragGrow = $02;
  dmLimitLoX = $10;
  dmLimitLoY = $20;
  dmLimitHiX = $40;
  dmLimitHiY = $80;
  dmLimitAll = $F0;

  { TView Help context codes }

  hcNoContext = 0;
  hcDragging = 1;

  { TScrollBar part codes }

  sbLeftArrow = 0;
  sbRightArrow = 1;
  sbPageLeft = 2;
  sbPageRight = 3;
  sbUpArrow = 4;
  sbDownArrow = 5;
  sbPageUp = 6;
  sbPageDown = 7;
  sbIndicator = 8;

  { TScrollBar options for TWindow.StandardScrollBar }

  sbHorizontal = $0000;
  sbVertical = $0001;
  sbHandleKeyboard = $0002;

  { TWindow Flags masks }

  wfMove = $01;
  wfGrow = $02;
  wfClose = $04;
  wfZoom = $08;
  wfMaxi = $10;

  { TWindow number constants }

  wnNoNumber = 255;

  { TWindow palette entries }

  wpBlueWindow = 0;
  wpCyanWindow = 1;
  wpGrayWindow = 2;

  { TDialog palette entires }

  dpRedDialog = 1;
  dpCyanDialog = 2;
  dpGrayDialog = 3;

  { TButton flags }

  bfNormal = $00;
  bfDefault = $01;
  bfLeftJust = $02;
  bfBroadcast = $04;
  bfGrabFocus = $08;

  { TMultiCheckboxes flags }
  { hibyte = number of bits }
  { lobyte = bit mask }

  cfOneBit = $0101;
  cfTwoBits = $0203;
  cfFourBits = $040F;
  cfEightBits = $08FF;

const
  arcFirst = 100;
  arcLast = 249;

const
  MaxBytes = 128*1024*1024;
  MaxWords = MaxBytes div SizeOf(word);
  MaxPtrs = MaxBytes div SizeOf(Pointer);

type
  TRegExpStatus =
  (
  resOK,
  resCanceled,
  resNilArgument,
  resInvalidArgument,
  resRegExpTooBig,
  resOutOfSpace,
  resCorruptedProgram,
  resUnmatchedParenthesis,
  resJunkOnEnd,
  resStarPlusOperandCouldBeEmpty,
  resNestedStarQuotePlus,
  resInvalidEscape,
  resInvalidPredefinedExp,
  resUndefinedPredefinedExp,
  resStackOverflow,
  resInvalidSetRange,
  resUnmatchedSquareBracket,
  resInternalUrp,
  resOperatorFollowsNothing,
  resTrailingBackSlash,
  resInternalDisaster,
  resNoExpression,
  resMemoryCorruption,
  resCorruptedPointers,
  resInternalFoulup,
  resDuplicatedTaggedExp,
  resInvalidTaggedExp,
  resComplexBracesNotImplemented,
  resInvalidBraces,
  resLoopStackExceeded,
  resLoopWithoutEntry
  );

type
  Str2 = String[2];
  PStr2 = ^Str2;
  Str3 = String[3];
  PStr3 = ^Str3;
  Str4 = String[4];
  PStr4 = ^Str4;
  Str5 = String[5];
  PStr5 = ^Str5;
  Str6 = String[6];
  PStr6 = ^Str6;
  Str8 = String[8];
  PStr8 = ^Str8;
  Str12 = String[12];
  PStr12 = ^Str12;
  Str40 = String[40];
  PStr40 = ^Str40;
  Str50 = String[50];
  PStr50 = ^Str50;

  PString = ^String;
  PLongString = ^LongString;
  LongString = AnsiString;

  AsciiZ = packed array[0..255] of Char;

  PCharSet = ^TCharSet;
  TCharSet = Set of Char;
  TCommandSet = Set of byte;

  PPalette = ^TPalette;
  TPalette = String;

  TMenuStr = String[81];

type
  AInt = SmallInt;
  AWord = SmallWord;

  TSize = Comp;

  PByteArray = ^TByteArray;
  TByteArray = packed array[0..0] of byte;

  PWordArray = ^TWordArray;
  TWordArray = packed array[0..0] of AWord;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = packed array[0..0] of integer;

  PPointerArray = ^TPointerArray;
  TPointerArray = packed array[0..0] of Pointer;

  PPCharArray = ^TPCharArray;
  TPCharArray = packed array[0..0] of PChar;

  PVideoBuf = ^TVideoBuf;
  TVideoBuf = packed array[0..3999] of AWord;

  PDrawBuffer = ^TDrawBuffer;
  TDrawBuffer = packed array[0..255] of AWord;

  PByte = ^Byte;
  PLongInt = ^LongInt;
  PPointer = ^Pointer;

  TSelectMode = (NormalSelect, EnterSelect, LeaveSelect);
  TPhase = (phFocused, phPreProcess, phPostProcess);

type
  PSItem = ^TSItem;
  TSItem = packed record
    Value: PString;
    Next: PSItem;
    end;

  PStreamRec = ^TStreamRec;
  TStreamRec = packed record
    ObjType: word;
    VmtLink: Pointer;
    Load: Pointer;
    Store: Pointer;
    Next: PStreamRec;
    end;

  PPoint = ^TPoint;
  TPoint = object
    X, Y: longInt;
    function Equals(P: TPoint): boolean;
    function EqualsXY(AX, AY: longInt): boolean;
    procedure Assign(AX, AY: longInt);
    function isLE(P: TPoint): boolean; {less then or equal}
    function isGE(P: TPoint): boolean; {great thean or equal}
    end;

  TRect = object
    A, B: TPoint;
    procedure Assign(XA, YA, XB, YB: longInt);
    procedure Copy(R: TRect);
    procedure Move(ADX, ADY: longInt);
    procedure Grow(ADX, ADY: longInt);
    procedure Intersect(R: TRect);
    procedure Union(R: TRect);
    function Contains(P: TPoint): boolean;
    function Equals(R: TRect): boolean;
    function Empty: boolean;
    end;

  PEvent = ^TEvent;
  TEvent = packed record
    What: word;
    case word of
      evNothing:
      ();
      evMouse:
      (
      Buttons: byte;
      Double: boolean;
      Where: TPoint
      );
      evKeyDown:
      (
      case integer of
        0:
        (KeyCode: word);
        1:
        (
        CharCode: Char;
        ScanCode: byte;
        ShiftCode: byte;
        )
        );
        evMessage:
        (
        Command: word;
        case word of
          0:
          (InfoPtr: Pointer);
          1:
          (InfoLong: longInt);
          2:
          (InfoWord: word);
          3:
          (InfoInt: integer);
          4:
          (InfoByte: byte);
          5:
          (InfoChar: Char)
          );
  end;

TEventTimer = packed record
  StartTics: longInt;
  ExpireTics: longInt;
  end;

PMenu = ^TMenu;

PMenuItem = ^TMenuItem;
TMenuItem = packed record
  Next: PMenuItem;
  Name: PString;
  Command: word;
  Disabled: boolean;
  KeyCode: word;
  HelpCtx: word;
  case integer of
    0: (Param: PString);
    1: (SubMenu: PMenu);
  end;

TMenu = packed record
  Items: PMenuItem;
  Default: PMenuItem;
  end;

PStatusItem = ^TStatusItem;
TStatusItem = packed record
  Next: PStatusItem;
  text: PString;
  KeyCode: word;
  Command: word;
  end;

PStatusDef = ^TStatusDef;
TStatusDef = packed record
  Next: PStatusDef;
  Min, Max: word;
  Items: PStatusItem;
  end;

{&Cdecl+}
THandleCommandProc = procedure (Command, ObjType: SmallWord; const
  PluginName: ShortString; DNFuncs, DNMethods: Pointer; var
  _Finalization: Pointer);
TFormatsCountProc = function : word;
TArchiveSignProc = function (Id: word): Str4;
TCreateArchiveObjectProc = function (Id: word): Pointer;
TDetectCreateArchiveObjectProc = function : Pointer;
{&Cdecl-}

PEventCatcherInfo = ^TEventCatcherInfo;
TEventCatcherInfo = packed record
  FirstCatchedCommand: word;
  LastCatchedCommand: word;
  FirstObjType: word;
  LastObjType: word;
  PluginPath: String[8];
  Reserved: packed array[0..2] of byte;
  LibHandle: integer;
  Entry: THandleCommandProc;
  end;

PEventCatcherArray = ^TEventCatcherArray;
TEventCatcherArray = packed array[1..1] of TEventCatcherInfo;

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

TEditorEventHook = function (var Event: TEvent; Editor: Pointer):
  boolean;

PFillColorsData = ^TFillColorsData;
TFillColorsData = packed record
  DrawBuffer: Pointer;
  StrNum, StartPos, EndPos: longInt;
  end;

PIndexArray = ^TIndexArray;
TIndexArray = packed array[0..65520 div SizeOf(longInt)-1] of
  longInt;

PIdxResource = ^TIdxResource;
TIdxResource = packed record
  NotForYou1: Pointer;
  NotForYou2: boolean;
  Stream: Pointer;
  Index: PIndexArray;
  Count: AInt;
  end;

lSearchRec = packed record
  Handle: longInt;
  NameLStr: Pointer;
  Attr: byte;
  Time: longInt;
  Size: TSize;
  Name: ShortString;
  CreationTime: longInt;
  LastAccessTime: longInt;
  Filler: packed array[0..3] of Char;
  {$IFDEF OS2}
  //JO: Внимание! размер FindBuf должен быть согласован с размером аналогичной
  //    переменной в VpSysLo2.TOSSearchRecNew
  FindBuf: array[0..2*1024-1] of byte;
  FindCount: integer;
  FindPtr: Pointer;
  {$ENDIF}
  {$IFDEF WIN32}
  ShortName: ShortString;
  ExcludeAttr: longInt;
  FindData: TWin32FindData;
  {$ENDIF}
  {$IFDEF DPMI32}
  attr_must: byte;
  dos_dta: packed record
    fill: packed array[1..21] of byte;
    Attr: byte;
    Time: longInt;
    Size: longInt;
    Name: packed array[0..12] of Char;
    end;
  {$ENDIF}
  {$IFDEF LINUX}
  FindDir: packed array[0..255] of Char;
  FindName: ShortString;
  FindAttr: longInt;
  {$ENDIF}
  FullSize: TSize;
  FullName: ShortString;
  {$IFDEF OS2}
  PrevName: ShortString;
  {$ENDIF}
  end;

TCRLF = (cfNone, cfCRLF, cfCR, cfLF);

PEditOptions = ^TEditOptions;
TEditOptions = packed record
  AutoIndent: boolean;
  AutoBrackets: boolean;
  BackUnIndents: boolean;
  HiLite: boolean;
  HiliteLine: boolean;
  HiliteColumn: boolean;
  JustifyOnWrap: boolean;
  AutoWrap: boolean;
  LeftMargin: word;
  RightMargin: word;
  Paragraph: word;
  ForcedCRLF: TCRLF;
  SmartTab: boolean;
  end;

TPosArray = packed array[1..9] of TPoint;

TKeyMap = (kmNone, kmAscii, kmAnsi, kmKoi8r);

TXlat = array[Char] of Char;

PHighliteParams = ^THighliteParams;
THighliteParams = packed record
  GenFlags: word;
  HexFlags: word;
  DecFlags: word;
  OctFlagsQ: word;
  OctFlagsO: word;
  BinFlags: word;
  StrFlags: word;
  RulesBuffer: packed array[1..$800] of Char;
  end;

PDiz = ^TDIZ;
TDIZ = packed record
  Owner: PString;
  DIZ: PString;
  Line: longInt;
  isDisposable: boolean;
  end;

TUseLFN = {$IFDEF OS2}True {$ELSE}False {$ENDIF}..True;
TShortName = String[12];
TFlName = array[TUseLFN] of TShortName;
TDate4 = packed record
  Minute, Hour, Day, Month: byte;
  end;

PFileRec = ^TFileRec;
TFileRec = packed record
  Size: TSize;
  PSize: TSize;
  Owner: PString;
  OwnerDisposible: boolean;
  DIZ: PDiz;
  Yr: word;
  YrCreat: word;
  YrLAcc: word;
  TType: byte;
  Attr: word;
  Second: byte;
  SecondCreat: byte;
  SecondLAcc: byte;
  Selected: boolean;
  UsageCount: byte;
  FDate, FDateCreat, FDateLAcc: longInt;
  FlName: TFlName;
  //  Dummy: array[1..SizeOf(ShortString)-SizeOf(TShortName)] of Char;
  end;

TMakeListRec = packed record
  FileName: String;
  Header: String;
  HeaderMode: word;
  Action: String;
  Footer: String;
  FooterMode: word;
  Options: word;
  end;

PUserParams = ^TUserParams;
tUserParams = packed record
  active, Passive: PFileRec;
  ActiveList, PassiveList: String;
  end;

TQuickSearchData = packed record
  Mask: String;
  NumExt: word;
  ExtD: word;
  end;

TDiskInfoRec = packed record
  Title: PString;
  Dir: PString;
  Files: PString;
  Free: PString;
  Total: PString;
  VolumeID: PString;
  SerialNo: PString;
  FileSys: PString;
  DirInfo: Pointer {PCollection};
  Limit: TPoint;
  InfoFile: byte;
  end;

TDriveType = (dtUndefined, dtDisk, dtFind, dtTemp, dtList, dtArcFind,
  dtArc, dtLink, dtArvid);
TAvdType = (avdTdr, avdAvt);
TLineType = (ltNormal, ltOS2FullScreen, ltOS2Window, ltTimer);

TTdrHeader = packed record
  FileTableOfs: longInt;
  DirTableOfs: longInt;
  PosTableOfs: longInt;
  FileTableLen: longInt;
  DirTableLen: longInt;
  PosTableLen: longInt;
  TapeFmt: AWord;
  TapeID: AWord;
  TapeLen: AWord;
  RecordLen: AWord;
  NewRecordSector: longInt;
  DescTableOfs: longInt;
  Res01: array[1..16] of byte;
  DescTableLen: longInt;
  Res02: array[1..16] of byte;
  LastNewRecordSector: longInt;
  Res03: array[1..36] of byte;
  end;

TAvtHeader = packed record
  signature: array[1..4] of Char;
  AvtFmt: longInt;
  CheckSum: longInt;
  AfterLastCell: longInt;
  FreeCell: longInt;
  RootDirCell: longInt;
  NewSector: longInt;
  LastNewSector: longInt;
  AvtMediaCell: longInt;
  Undefined1: longInt;
  end;

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

implementation

function TPoint.Equals(P: TPoint): boolean;
  begin
    Equals := (X = P.X) and (Y = P.Y);
  end;

function TPoint.EqualsXY(AX, AY: longInt): boolean;
  begin
    EqualsXY := (X = AX) and (Y = AY);
  end;

procedure TPoint.Assign(AX, AY: longInt);
  begin
    X := AX;
    Y := AY;
  end;

function TPoint.isLE(P: TPoint): boolean;
  begin
    isLE := (Y = P.Y) and (X <= P.X) or (Y < P.Y);
  end;

function TPoint.isGE(P: TPoint): boolean;
  begin
    isGE := (Y = P.Y) and (X >= P.X) or (Y > P.Y);
  end;

procedure TRect.Assign(XA, YA, XB, YB: longInt);
  begin
    A.X := XA;
    A.Y := YA;
    B.X := XB;
    B.Y := YB;
  end;

procedure TRect.Copy(R: TRect);
  begin
    A := R.A;
    B := R.B;
  end;

procedure TRect.Move(ADX, ADY: longInt);
  begin
    Inc(A.X, ADX);
    Inc(A.Y, ADY);
    Inc(B.X, ADX);
    Inc(B.Y, ADY);
  end;

procedure TRect.Grow(ADX, ADY: longInt);
  begin
    Dec(A.X, ADX);
    Dec(A.Y, ADY);
    Inc(B.X, ADX);
    Inc(B.Y, ADY);
    if (A.X >= B.X) or (A.Y >= B.Y) then
      begin
        A.X := 0;
        A.Y := 0;
        B.X := 0;
        B.Y := 0;
      end;
  end;

procedure TRect.Intersect(R: TRect);
  begin
    if (R.A.X > A.X) then
      A.X := R.A.X;
    if (R.A.Y > A.Y) then
      A.Y := R.A.Y;
    if (R.B.X < B.X) then
      B.X := R.B.X;
    if (R.B.Y < B.Y) then
      B.Y := R.B.Y;
    if (A.X >= B.X) or (A.Y >= B.Y) then
      begin
        A.X := 0;
        A.Y := 0;
        B.X := 0;
        B.Y := 0;
      end;
  end;

procedure TRect.Union(R: TRect);
  begin
    if (R.A.X < A.X) then
      A.X := R.A.X;
    if (R.A.Y < A.Y) then
      A.Y := R.A.Y;
    if (R.B.X > B.X) then
      B.X := R.B.X;
    if (R.B.Y > B.Y) then
      B.Y := R.B.Y;
  end;

function TRect.Contains(P: TPoint): boolean;
  begin
    Contains := (P.X >= A.X) and (P.X < B.X) and
    (P.Y >= A.Y) and (P.Y < B.Y);
  end;

function TRect.Equals(R: TRect): boolean;
  begin
    Equals := (A.X = R.A.X) and (A.Y = R.A.Y) and
    (B.X = R.B.X) and (B.Y = R.B.Y);
  end;

function TRect.Empty: boolean;
  begin
    Empty := (A.X >= B.X) or (A.Y >= B.Y);
  end;

end.
