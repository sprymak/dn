unit _Defines;
(******

DN/2 Plugin Interface - consts & type defines
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

interface

uses
  {$IFDEF WIN32} Windows, {$ENDIF}
  {Use32,} VpSysLow;

const

{ TStream access modes }

  stCreate    = $FFFF { $DC0B };                              { Create new file }
  stOpenRead  = Open_Access_ReadOnly or Open_Share_DenyNone;  { Read access only }
  stOpenWrite = Open_Access_WriteOnly or Open_Share_DenyNone; { Write access only }
  stOpen      = Open_Access_ReadWrite or Open_Share_DenyNone; { Read and write access }
  stOpenPacked= Open_Access_ReadWrite+1;                      { Read access only, packed files too }

{ File share mode constants }
  fmClean     = $FF00;                    { Mask to clean low byte of file mode constatns }

  fmOpenMode  = $FFF0;                    { Mask to apply fmReadOnly/fmWriteOnly/fmReadWrite }
  fmReadOnly  = Open_Access_ReadOnly;     { Open read-only file }
  fmWriteOnly = Open_Access_WriteOnly;    { Open file for write only }
  fmReadWrite = Open_Access_ReadWrite;    { Open file as for read, as for write }
  fmPacked    = Open_Access_ReadWrite+1;  { Open a packed file, if can }

  fmDeny      = $FF0F;                    { Mask to apply fmDenyXXX }
  fmDenyAll   = Open_Share_DenyReadWrite; { Exclusive file use }
  fmDenyWrite = Open_Share_DenyWrite;     { Deny write access }
  fmDenyRead  = Open_Share_DenyRead;      { Deny read access }
  fmDenyNone  = Open_Share_DenyNone;      { Deny no access }
  fmDenyChild = Open_Share_DenyNone;      { Don't give right's to child }

{ TStream error codes }

  stOk         =  0;              { No error }
  stError      = -1;              { Access error }
  stInitError  = -2;              { Cannot initialize stream }
  stReadError  = -3;              { Read beyond end of stream }
  stWriteError = -4;              { Cannot expand stream }
  stGetError   = -5;              { Get of unregistered object type }
  stPutError   = -6;              { Put of unregistered object type }
  stSeekError  = -7;              { Stream seek error }
  stOpenError  = -8;              { Stream open error }

{ Event codes }

  evMouseDown = $0001;
  evMouseUp   = $0002;
  evMouseMove = $0004;
  evMouseAuto = $0008;
  evKeyDown   = $0010;
  evCommand   = $0100;
  evBroadcast = $0200;

{ Event masks }

  evNothing   = $0000;
  evMouse     = $000F;
  evKeyboard  = $0010;
  evMessage   = $FF00;

{ Message box classes }

  mfWarning      = $0000;       { Display a Warning box }
  mfError        = $0001;       { Dispaly a Error box }
  mfInformation  = $0002;       { Display an Information Box }
  mfConfirmation = $0003;       { Display a Confirmation Box }
  mfQuery        = $0004;
  mfAbout        = $0005;
  mfSysError     = $0006;

{ Message box button flags }

  mfYesButton    = $0100;       { Put a Yes button into the dialog }
  mfOKButton     = $0200;       { Put an OK button into the dialog }
  mfNoButton     = $0400;       { Put a No button into the dialog }
  mfCancelButton = $8000;       { Put a Cancel button into the dialog }
  mfNextDButton  = $0800;
  mfAppendButton = $1000;
  mf2YesButton   = $2000;
  mfAllButton    = $4000;

  mfYesNoCancel  = mfYesButton + mfNoButton + mfCancelButton;
                                { Standard Yes, No, Cancel dialog }
  mfYesNoConfirm = mfYesButton + mfNoButton + mfConfirmation;
                                { Standard Yes, No  confirmation }
  mfOKCancel     = mfOKButton + mfCancelButton;
                                { Standard OK, Cancel dialog }

{ TView State masks }

  sfVisible     = $0001;
  sfCursorVis   = $0002;
  sfCursorIns   = $0004;
  sfShadow      = $0008;
  sfActive      = $0010;
  sfSelected    = $0020;
  sfFocused     = $0040;
  sfDragging    = $0080;
  sfDisabled    = $0100;
  sfModal       = $0200;
  sfDefault     = $0400;
  sfExposed     = $0800;

{ TView Option masks }

  ofSelectable  = $0001;
  ofTopSelect   = $0002;
  ofFirstClick  = $0004;
  ofFramed      = $0008;
  ofPreProcess  = $0010;
  ofPostProcess = $0020;
  ofBuffered    = $0040;
  ofTileable    = $0080;
  ofCenterX     = $0100;
  ofCenterY     = $0200;
  ofCentered    = $0300;
  ofValidate    = $0400;
  ofSecurity    = $0800;
  ofVersion     = $3000;
  ofVersion10   = $0000;
  ofVersion20   = $1000;

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
  hcDragging  = 1;

{ TScrollBar part codes }

  sbLeftArrow  = 0;
  sbRightArrow = 1;
  sbPageLeft   = 2;
  sbPageRight  = 3;
  sbUpArrow    = 4;
  sbDownArrow  = 5;
  sbPageUp     = 6;
  sbPageDown   = 7;
  sbIndicator  = 8;

{ TScrollBar options for TWindow.StandardScrollBar }

  sbHorizontal     = $0000;
  sbVertical       = $0001;
  sbHandleKeyboard = $0002;

{ TWindow Flags masks }

  wfMove  = $01;
  wfGrow  = $02;
  wfClose = $04;
  wfZoom  = $08;
  wfMaxi  = $10;

{ TWindow number constants }

  wnNoNumber = 255;

{ TWindow palette entries }

  wpBlueWindow = 0;
  wpCyanWindow = 1;
  wpGrayWindow = 2;

{ TDialog palette entires }

  dpBlueDialog = 1;
  dpCyanDialog = 2;
  dpGrayDialog = 3;

{ TButton flags }

  bfNormal    = $00;
  bfDefault   = $01;
  bfLeftJust  = $02;
  bfBroadcast = $04;
  bfGrabFocus = $08;

{ TMultiCheckboxes flags }
{ hibyte = number of bits }
{ lobyte = bit mask }

  cfOneBit       = $0101;
  cfTwoBits      = $0203;
  cfFourBits     = $040F;
  cfEightBits    = $08FF;

const
  arcFirst=100;
  arcLast=249;

const
  MaxBytes = 128*1024*1024;
  MaxWords = MaxBytes DIV SizeOf(Word);
  MaxPtrs  = MaxBytes DIV SizeOf(Pointer);

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
  Str2 = string[2];   PStr2  = ^Str2;
  Str3 = string[3];   PStr3  = ^Str3;
  Str4 = string[4];   PStr4  = ^Str4;
  Str5 = string[5];   PStr5  = ^Str5;
  Str6 = string[6];   PStr6  = ^Str6;
  Str8 = string[8];   PStr8  = ^Str8;
  Str12 = string[12]; PStr12 = ^Str12;
  Str40 = string[40]; PStr40 = ^Str40;
  Str50 = string[50]; PStr50 = ^Str50;

  PString = ^String;
  PLongString = ^LongString;
  LongString = AnsiString;

  AsciiZ = packed array[0..255] of Char;

  PCharSet = ^TCharSet;
  TCharSet = set of Char;
  TCommandSet = set of Byte;

  PPalette = ^TPalette;
  TPalette = String;

  TMenuStr = string[81];

type
  AInt = SmallInt;
  AWord = SmallWord;

  TSize = Comp;

  PByteArray = ^TByteArray;
  TByteArray = packed array[0..0] of Byte;

  PWordArray = ^TWordArray;
  TWordArray = packed array[0..0] of AWord;

  PPointerArray = ^TPointerArray;
  TPointerArray = packed array [0..0] of Pointer;

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
    ObjType: Word;
    VmtLink: Pointer;
    Load:  Pointer;
    Store: Pointer;
    Next:  PStreamRec;
  end;

  PPoint = ^TPoint;
  TPoint = object
    X, Y: LongInt;
    function  Equals(P: TPoint): Boolean;
    function  EqualsXY(AX, AY: LongInt): Boolean;
    procedure Assign(AX, AY: LongInt);
    function  isLE(P: TPoint): Boolean; {less then or equal}
    function  isGE(P: TPoint): Boolean; {great thean or equal}
  end;

  TRect = object
    A, B: TPoint;
    procedure Assign(XA, YA, XB, YB: LongInt);
    procedure Copy(R: TRect);
    procedure Move(ADX, ADY: LongInt);
    procedure Grow(ADX, ADY: LongInt);
    procedure Intersect(R: TRect);
    procedure Union(R: TRect);
    function Contains(P: TPoint): Boolean;
    function Equals(R: TRect): Boolean;
    function Empty: Boolean;
  end;

  PEvent = ^TEvent;
  TEvent = packed record
    What: Word;
    case Word of
      evNothing:
        ();
      evMouse:
        (
         Buttons: Byte;
         Double: Boolean;
         Where: TPoint
        );
      evKeyDown:
        (
         case Integer of
           0:
             (KeyCode: Word);
           1:
             (
              CharCode: Char;
              ScanCode: Byte;
              ShiftCode: Byte;
             )
        );
      evMessage:
        (
         Command: Word;
         case Word of
           0:
             (InfoPtr: Pointer);
           1:
             (InfoLong: LongInt);
           2:
             (InfoWord: Word);
           3:
             (InfoInt: Integer);
           4:
             (InfoByte: Byte);
           5:
             (InfoChar: Char)
        );
  end;

  TEventTimer = packed record
    StartTics: LongInt;
    ExpireTics: LongInt;
  end;

  PMenu = ^TMenu;

  PMenuItem = ^TMenuItem;
  TMenuItem = packed record
    Next: PMenuItem;
    Name: PString;
    Command: Word;
    Disabled: Boolean;
    KeyCode: Word;
    HelpCtx: Word;
    case Integer of
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
    Text: PString;
    KeyCode: Word;
    Command: Word;
  end;

  PStatusDef = ^TStatusDef;
  TStatusDef = packed record
    Next: PStatusDef;
    Min, Max: Word;
    Items: PStatusItem;
  end;

  {&Cdecl+}
  THandleCommandProc = procedure(Command, ObjType: SmallWord; const PluginName: ShortString; DNFuncs, DNMethods: Pointer; var Finalization: Pointer);
  TFormatsCountProc = function: Word;
  TArchiveSignProc = function(Id: Word): Str4;
  TCreateArchiveObjectProc = function(Id: Word): Pointer;
  TDetectCreateArchiveObjectProc = function: Pointer;
  {&Cdecl-}

  PEventCatcherInfo = ^TEventCatcherInfo;
  TEventCatcherInfo = packed record
    FirstCatchedCommand: Word;
    LastCatchedCommand: Word;
    FirstObjType: Word;
    LastObjType: Word;
    PluginPath: String[8];
    Reserved: packed array[0..2] of Byte;
    LibHandle: Integer;
    Entry: THandleCommandProc;
  end;

  PEventCatcherArray = ^TEventCatcherArray;
  TEventCatcherArray = packed array[1..1] of TEventCatcherInfo;

  PArchiveViewerInfo = ^TArchiveViewerInfo;
  TArchiveViewerInfo = packed record
    FirstTag: Byte;
    PluginPath: String[8];
    Reserved: SmallWord;
    LibHandle: Integer;
    FormatsCount: TFormatsCountProc;
    ArchiveSign: TArchiveSignProc;
    CreateArchiveObject: TCreateArchiveObjectProc;
    DetectCreateArchiveObject: TDetectCreateArchiveObjectProc;
  end;

  PArchiveViewerArray = ^TArchiveViewerArray;
  TArchiveViewerArray = packed array[arcFirst-1..arcLast+1] of PArchiveViewerInfo;

  TEditorEventHook = function(var Event: TEvent; Editor: Pointer): Boolean;

  PFillColorsData = ^TFillColorsData;
  TFillColorsData = record
    DrawBuffer: Pointer;
    StrNum, StartPos, EndPos: LongInt;
  end;

  PIndexArray =^TIndexArray;
  TIndexArray = packed array [0..65520 div SizeOf(LongInt)-1] of LongInt;

  PIdxResource = ^TIdxResource;
  TIdxResource = packed record
    NotForYou1: Pointer;
    NotForYou2: Boolean;
    Stream: Pointer;
    Index: PIndexArray;
    Count: AInt;
  end;

  lSearchRec = packed record
    Handle: Longint;
    NameLStr: Pointer;
    Attr: Byte;
    Time: Longint;
    Size: Longint;
    Name: ShortString;
    CreationTime: Longint;
    LastAccessTime: Longint;
    Filler: packed array[0..3] of Char;
    {$IFDEF WIN32}
    ShortName: ShortString;
    ExcludeAttr: Longint;
    FindData: TWin32FindData;
    {$ENDIF}
    {$IFDEF DPMI32}
    Attr_Must: Byte;
    dos_dta: packed record
      Fill: packed array[1..21] of Byte;
      Attr: Byte;
      Time: Longint;
      Size: Longint;
      Name: packed array[0..12] of Char;
    end;
    {$ENDIF}
    {$IFDEF LINUX}
    FindDir: packed array[0..255] of Char;
    FindName: ShortString;
    FindAttr: LongInt;
    {$ENDIF}
    FullSize: TSize;
    FullName: ShortString;
    {$IFDEF OS2}
    PrevName: ShortString;
    {$ENDIF}
  end;

  TCrLf = (cfNone, cfCRLF, cfCR, cfLF);

  PEditOptions = ^TEditOptions;
  TEditOptions = packed record
    AutoIndent: Boolean;
    AutoBrackets: Boolean;
    BackUnIndents: Boolean;
    Hilite: Boolean;
    HiliteLine: Boolean;
    HiliteColumn: Boolean;
    JustifyOnWrap: Boolean;
    AutoWrap: Boolean;
    LeftMargin: Word;
    RightMargin: Word;
    Paragraph: Word;
    ForcedCrLf: TCrLf;
    SmartTab: Boolean;
  end;

  TPosArray = packed array[1..9] of TPoint;

  TKeyMap = (kmNone, kmAscii, kmAnsi, kmKoi8r);

  PHighliteParams = ^THighliteParams;
  THighliteParams = packed record
    GenFlags: Word;
    HexFlags: Word;
    DecFlags: Word;
    OctFlagsQ: Word;
    OctFlagsO: Word;
    BinFlags: Word;
    StrFlags: Word;
    RulesBuffer: packed array[1..$800] of Char;
  end;

  PDIZ = ^TDIZ;
  TDIZ = packed record
    Owner: PString;
    DIZ: PString;
    Line: LongInt;
    isDisposable: Boolean;
  end;

  PFileRec = ^TFileRec;
  TFileRec = packed record
    Size: TSize;
    PSize: TSize;
    Owner: PString;
    OwnerDisposible: Boolean;
    Diz: PDIZ;
    {$IFDEF OS2}
    Name: String;
    {$ELSE}
    Name: Str12;
    LFN: String;
    {$ENDIF}
    Yr: Word;
    YrCreat: Word;
    YrLAcc: Word;
    TType: Byte;
    Attr: Word;
    Second: Byte;
    SecondCreat: Byte;
    SecondLAcc: Byte;
    Selected: Boolean;
    UsageCount: Byte;
    case Integer of
      0: (FDate, FDateCreat, FDateLAcc: LongInt);
      1: (Minute, Hour, Day, Month, MinuteCreat, HourCreat, DayCreat, MonthCreat, MinuteLAcc, HourLAcc, DayLAcc, MonthLAcc: Byte);
  end;

  TMakeListRec = packed record
    FileName: String;
    Header: String;
    HeaderMode: Word;
    Action: String;
    Footer: String;
    FooterMode: Word;
    Options: Word;
  end;

  PUserParams = ^TUserParams;
  TUserParams = packed record
    Active, Passive: PFileRec;
    ActiveList, PassiveList: String;
  end;

implementation

function  TPoint.Equals(P: TPoint): Boolean;
begin
  Equals := (X = P.X) and (Y = P.Y);
end;

function  TPoint.EqualsXY(AX, AY: LongInt): Boolean;
begin
  EqualsXY := (X = AX) and (Y = AY);
end;

procedure TPoint.Assign(AX, AY: LongInt);
begin
  X := AX;
  Y := AY;
end;

function TPoint.isLE(P: TPoint): Boolean;
begin
  isLE := (Y = P.Y) and (X <= P.X) or (Y < P.Y);
end;

function TPoint.isGE(P: TPoint): Boolean;
begin
  isGE := (Y = P.Y) and (X >= P.X) or (Y > P.Y);
end;

procedure TRect.Assign(XA, YA, XB, YB: LongInt);
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

procedure TRect.Move(ADX, ADY: LongInt);
begin
  Inc(A.X, ADX);
  Inc(A.Y, ADY);
  Inc(B.X, ADX);
  Inc(B.Y, ADY);
end;

procedure TRect.Grow(ADX, ADY: LongInt);
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
  if (R.A.X > A.X) then A.X := R.A.X;
  if (R.A.Y > A.Y) then A.Y := R.A.Y;
  if (R.B.X < B.X) then B.X := R.B.X;
  if (R.B.Y < B.Y) then B.Y := R.B.Y;
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
  if (R.A.X < A.X) then A.X := R.A.X;
  if (R.A.Y < A.Y) then A.Y := R.A.Y;
  if (R.B.X > B.X) then B.X := R.B.X;
  if (R.B.Y > B.Y) then B.Y := R.B.Y;
end;

function TRect.Contains(P: TPoint): Boolean;
begin
  Contains := (P.X >= A.X) and (P.X < B.X) and
              (P.Y >= A.Y) and (P.Y < B.Y);
end;

function TRect.Equals (R: TRect): Boolean;
begin
  Equals := (A.X = R.A.X) and (A.Y = R.A.Y) and
            (B.X = R.B.X) and (B.Y = R.B.Y);
end;

function TRect.Empty: Boolean;
begin
  Empty := (A.X >= B.X) or (A.Y >= B.Y);
end;

end.
