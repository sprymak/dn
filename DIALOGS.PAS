{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
//  Based on Dos Navigator (C) 1991-99 RIT Research Labs
//
//  This programs is free for commercial and non-commercial use as long as
//  the following conditions are aheared to.
//
//  Copyright remains RIT Research Labs, and as such any Copyright notices
//  in the code are not to be removed. If this package is used in a
//  product, RIT Research Labs should be given attribution as the RIT Research
//  Labs of the parts of the library used. This can be in the form of a textual
//  message at program startup or in documentation (online or textual)
//  provided with the package.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  1. Redistributions of source code must retain the copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. All advertising materials mentioning features or use of this software
//     must display the following acknowledgement:
//     "Based on Dos Navigator by RIT Research Labs."
//
//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  The licence and distribution terms for any publically available
//  version or derivative of this code cannot be changed. i.e. this code
//  cannot simply be copied and put under another distribution licence
//  (including the GNU Public Licence).
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Dialogs;

{-----------------------------------------------------}
{ This module is based on Turbo Vision Dialogs Unit   }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

interface

uses
  Lfn, {DataCompBoy}DnIni, Collect, Defines, Streams, Drivers, Views,
   advance6,
  Scroller, Validate, U_KeyMap
  ;

const

  { Color palettes }

  CGrayDialog = #32#33#34#35#36#37#38#39#40#41#42#43#44#45#46#47+
  #48#49#50#51#52#53#54#55#56#57#58#59#60#61#62#63+
  #178#179;
  CRedDialog = #32#214#214#35#36#215#38#39#40#216#216#217#218#219#220#47+
  #48#49#50#51#52#53#54#55#56#57#58#59#60#61#62#63+
  #221#219;
  CCyanDialog = #96#97#98#99#100#101#102#103#104#105#106#107#108+
  #109#110#111#112#113#114#115#116#117#118#119#120+
  #121#122#123#124#125#126#127;

  CDialog = CGrayDialog;

  CStaticText = #6;
  CLabel = #7#8#9#9;
  CButton = #10#11#12#13#14#34#33#15;
  CCluster = #16#17#18#18#31;
  CInputLine = #19#19#20#21;
  CHistory = #22#23;
  CHistoryWindow = #19#19#21#24#25#19#20;
  CHistoryViewer = #6#6#7#6#6;

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

type

  { TDialog object }

  { Palette layout }
  {  1 = Frame passive }
  {  2 = Frame active }
  {  3 = Frame icon }
  {  4 = ScrollBar page area }
  {  5 = ScrollBar controls }
  {  6 = StaticText }
  {  7 = Label normal }
  {  8 = Label selected }
  {  9 = Label shortcut }
  { 10 = Button normal }
  { 11 = Button default }
  { 12 = Button selected }
  { 13 = Button disabled }
  { 14 = Button shortcut }
  { 15 = Button shadow }
  { 16 = Cluster normal }
  { 17 = Cluster selected }
  { 18 = Cluster shortcut }
  { 19 = InputLine normal text }
  { 20 = InputLine selected text }
  { 21 = InputLine arrows }
  { 22 = History arrow }
  { 23 = History sides }
  { 24 = HistoryWindow scrollbar page area }
  { 25 = HistoryWindow scrollbar controls }
  { 26 = ListViewer normal }
  { 27 = ListViewer focused }
  { 28 = ListViewer selected }
  { 29 = ListViewer divider }
  { 30 = InfoPane }
  { 31 = Cluster disabled }
  { 32 = Reserved }
  { 33 = Shortcut selected }
  { 34 = Shortcut default  }

  PDialog = ^TDialog;
  TDialog = object(TWindow)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    constructor Init(var Bounds: TRect; const ATitle: TTitleStr);
    constructor Load(var S: TStream);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Valid(Command: word): Boolean; virtual;
    end;

  { TSItem }

  PSItem = ^TSItem;
  TSItem = record
    Value: PString;
    Next: PSItem;
    end;

  { TInputLine object }

  { Palette layout }
  { 1 = Passive }
  { 2 = Active }
  { 3 = Selected }
  { 4 = Arrows }

  PInputline = ^TInputLine;
  TInputLine = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Data: PString;
    MaxLen: AInt;
    CurPos: AInt;
    FirstPos: AInt;
    SelStart: AInt;
    SelEnd: AInt;
    Validator: PValidator;
    DrawShift: Byte;
    CtrlK: Boolean;
    constructor Init(var Bounds: TRect; AMaxLen: AInt);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function DataSize: word; virtual;
    procedure Draw; virtual;
    procedure GetData(var Rec); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SelectAll(Enable: Boolean);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: word; Enable: Boolean); virtual;
    procedure SetValidator(AValid: PValidator);
    procedure Store(var S: TStream);
    function Valid(Command: word): Boolean; virtual;
    function CanScroll(Delta: integer): Boolean;
    end;

  PHexLine = ^THexLine;
  THexLine = object(TView)
    InputLine: PInputline;
    DeltaX, CurX: integer;
    Sec: Boolean;
    constructor Init(R: TRect; AInputLine: PInputline);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; virtual;
    end;

  { TButton object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Default text }
  { 3 = Selected text }
  { 4 = Disabled text }
  { 5 = Normal shortcut }
  { 6 = Default shortcut }
  { 7 = Selected shortcut }
  { 8 = Shadow }

  PButton = ^TButton;
  TButton = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Title: PString;
    Command: AWord;
    Flags: Byte;
    AmDefault: Boolean;
    constructor Init(var Bounds: TRect; const ATitle: TTitleStr;
         ACommand: word;
        AFlags: word);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    procedure DrawState(Down: Boolean);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure MakeDefault(Enable: Boolean);
    procedure Press; virtual;
    procedure SetState(AState: word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
    end;

  { TCluster }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }
  { 5 = Disabled text }

  PCluster = ^TCluster;
  TCluster = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Value: LongInt;
    Sel: AInt;
    EnableMask: LongInt;
    Strings: TStringCollection;
    constructor Init(var Bounds: TRect; AStrings: PSItem);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function ButtonState(Item: integer): Boolean;
    function DataSize: word; virtual;

    procedure DrawBox(const Icon: String; Marker: Char);
    procedure DrawMultiBox(const Icon, Marker: String);
    procedure GetData(var Rec); virtual;
    function GetHelpCtx: word; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Mark(Item: integer): Boolean; virtual;
    function MultiMark(Item: integer): Byte; virtual;
    procedure Press(Item: integer); virtual;
    procedure MovedTo(Item: integer); virtual;
    procedure SetButtonState(AMask: LongInt; Enable: Boolean);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: word; Enable: Boolean); virtual;
    procedure Store(var S: TStream);
  private
    function Column(Item: integer): integer;
    function FindSel(P: TPoint): integer;
    function Row(Item: integer): integer;
    end;

  { TRadioButtons }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PRadioButtons = ^TRadioButtons;
  TRadioButtons = object(TCluster)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    procedure Draw; virtual;
    function Mark(Item: integer): Boolean; virtual;
    procedure MovedTo(Item: integer); virtual;
    procedure Press(Item: integer); virtual;
    procedure SetData(var Rec); virtual;
    end;

  { TCheckBoxes }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PCheckBoxes = ^TCheckBoxes;
  TCheckBoxes = object(TCluster)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    procedure Draw; virtual;
    function Mark(Item: integer): Boolean; virtual;
    procedure Press(Item: integer); virtual;
    end;

  { TMultiCheckBoxes }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PMultiCheckBoxes = ^TMultiCheckBoxes;
  TMultiCheckBoxes = object(TCluster)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    SelRange: Byte;
    Flags: AWord;
    States: PString;
    constructor Init(var Bounds: TRect; AStrings: PSItem;
        ASelRange: Byte; AFlags: word; const AStates: String);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function DataSize: word; virtual;
    procedure Draw; virtual;
    procedure GetData(var Rec); virtual;
    function MultiMark(Item: integer): Byte; virtual;
    procedure Press(Item: integer); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    end;

  { TListBox }

  { Palette layout }
  { 1 = Active }
  { 2 = Inactive }
  { 3 = Focused }
  { 4 = Selected }
  { 5 = Divider }

  PListBox = ^TListBox;
  TListBox = object(TListViewer)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    List: PCollection;
    constructor Init(var Bounds: TRect; ANumCols: word;
        AScrollBar: PScrollBar);
    constructor Load(var S: TStream);
    function DataSize: word; virtual;
    procedure GetData(var Rec); virtual;
    function GetText(Item: LongInt; MaxLen: integer): String; virtual;
    procedure NewLisT(AList: PCollection); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    end;

  { TStaticText }

  { Palette layout }
  { 1 = Text }

  PStaticText = ^TStaticText;
  TStaticText = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Text: PString;
    constructor Init(var Bounds: TRect; const AText: String);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure GetText(var S: String); virtual;
    procedure Store(var S: TStream);
    end;

  { TParamText }

  { Palette layout }
  { 1 = Text }

  PParamText = ^TParamText;
  TParamText = object(TStaticText)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    ParamCount: AInt;
    ParamList: Pointer;
    constructor Init(var Bounds: TRect; const AText: String;
        AParamCount: AInt);
    constructor Load(var S: TStream);
    function DataSize: word; virtual;
    procedure GetText(var S: String); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var S: TStream);
    end;

  { TLabel }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Selected text }
  { 3 = Normal shortcut }
  { 4 = Selected shortcut }

  PLabel = ^TLabel;
  TLabel = object(TStaticText)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Link: PView;
    Light: Boolean;
    constructor Init(var Bounds: TRect; const AText: String; ALink: PView);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Store(var S: TStream);
    end;

  { THistoryViewer }

  { Palette layout }
  { 1 = Active }
  { 2 = Inactive }
  { 3 = Focused }
  { 4 = Selected }
  { 5 = Divider }

  PHistoryViewer = ^THistoryViewer;
  THistoryViewer = object(TListViewer)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    HistoryId: AWord;
    SearchPos: AWord;
    constructor Init(var Bounds: TRect;
         AHScrollBar, AVScrollBar: PScrollBar;
        AHistoryId: AWord);
    function GetPalette: PPalette; virtual;
    function GetText(Item: LongInt; MaxLen: integer): String; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function HistoryWidth: integer;
    end;

  { THistoryWindow }

  { Palette layout }
  { 1 = Frame passive }
  { 2 = Frame active }
  { 3 = Frame icon }
  { 4 = ScrollBar page area }
  { 5 = ScrollBar controls }
  { 6 = HistoryViewer normal text }
  { 7 = HistoryViewer selected text }

  PHistoryWindow = ^THistoryWindow;
  THistoryWindow = object(TWindow)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Viewer: PListViewer;
    constructor Init(var Bounds: TRect; HistoryId: AWord);
    function GetPalette: PPalette; virtual;
    function GetSelection: String; virtual;
    procedure InitViewer(HistoryId: AWord); virtual;
    end;

  { THistory }

  { Palette layout }
  { 1 = Arrow }
  { 2 = Sides }

  PHistory = ^THistory;
  THistory = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Link: PInputline;
    HistoryId: AWord;
    constructor Init(var Bounds: TRect; ALink: PInputline;
         AHistoryId: AWord);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function InitHistoryWindow(var Bounds: TRect): PHistoryWindow; virtual;
    procedure RecordHistory(const S: String); virtual;
    procedure Store(var S: TStream);
    end;

  { SItem routines }

function NewSItem(const Str: String; ANext: PSItem): PSItem;

implementation

uses
  ASCIITab, HistList, advance, advance1, advance2, advance3, Commands,
  Startup, xTime, FlPanelX, DNApp
  {, Crt}, VpSysLow
  ;

const

  { TButton messages }

  cmGrabDefault = 61;
  cmReleaseDefault = 62;
  cmPutInClipboard = 11027;
  cmGetFromClipboard = 11028;

  { Utility functions }

function IsBlank(Ch: Char): Boolean;
  begin
  IsBlank := (Ch = ' ') or (Ch = #13) or (Ch = #10);
  end;

{ TDialog }

constructor TDialog.Init(var Bounds: TRect; const ATitle: TTitleStr);
  begin
  inherited Init(Bounds, ATitle, wnNoNumber);
  Options := Options or ofVersion20;
  GrowMode := 0;
  Flags := wfMove+wfClose;
  Palette := dpGrayDialog;
  end;

constructor TDialog.Load(var S: TStream);
  begin
  inherited Load(S);
  if Options and ofVersion = ofVersion10 then
    begin
    Palette := dpGrayDialog;
    Inc(Options, ofVersion20);
    end;
  end;

function TDialog.GetPalette: PPalette;
  const
    P: array[dpRedDialog..dpGrayDialog] of String[Length(CDialog)] =
      (CRedDialog, CCyanDialog, CGrayDialog);
  begin
  GetPalette := @P[Palette];
  end;

procedure TDialog.HandleEvent(var Event: TEvent);
  begin
  TWindow.HandleEvent(Event);
  case Event.What of
    evMouseDown:
      if GetState(sfModal) and (Event.Buttons and mbRightButton <> 0)
      then
        begin
        Event.What := evBroadcast;
        Event.Command := cmDefault;
        Event.InfoPtr := nil;
        PutEvent(Event);
        ClearEvent(Event);
        end;
    evKeyDown:
      case Event.KeyCode of
        kbESC:
          begin
          Event.What := evCommand;
          Event.Command := cmCancel;
          Event.InfoPtr := nil;
          PutEvent(Event);
          ClearEvent(Event);
          end;
        kbEnter:
          begin
          Event.What := evBroadcast;
          Event.Command := cmDefault;
          Event.InfoPtr := nil;
          PutEvent(Event);
          ClearEvent(Event);
          end;
      end {case};
    evCommand:
      case Event.Command of
        cmOK, cmCancel, cmYes, cmNo, cmSkip:
          if State and sfModal <> 0 then
            begin
            EndModal(Event.Command);
            ClearEvent(Event);
            end;
      end {case};
  end {case};
  end { TDialog.HandleEvent };

function TDialog.Valid(Command: word): Boolean;
  begin
  if Command = cmCancel then
    Valid := True
  else
    Valid := TGroup.Valid(Command);
  end;

function NewSItem(const Str: String; ANext: PSItem): PSItem;
  var
    Item: PSItem;
  begin
  New(Item);
  Item^.Value := NewStr(Str);
  Item^.Next := ANext;
  NewSItem := Item;
  end;

function Max(A, B: integer): integer;
  inline;
  begin
  if A > B then
    Max := A
  else
    Max := B;
  end;

{ TInputLine }

constructor TInputLine.Init(var Bounds: TRect; AMaxLen: AInt);
  begin
  TView.Init(Bounds);
  State := State or sfCursorVis;
  if  (InterfaceData.Options and ouiBlockInsertCursor <> 0) then
    BlockCursor;
  if  (Bounds.A.X <> Bounds.B.X) and (Bounds.A.Y <> Bounds.B.Y)
  then
    Options := Options or (ofSelectable+ofFirstClick+ofVersion20)
  else
    Options := Options or (ofFirstClick+ofVersion20) and not ofSelectable;
  GetMem(Data, AMaxLen+1);
  Data^:= '';
  MaxLen := AMaxLen;
  DrawShift := 1;
  CtrlK := False;
  end;

constructor TInputLine.Load(var S: TStream);
  begin
  TView.Load(S);
  DrawShift := 1;
  {Cat:warn AnsiString}
  S.Read(MaxLen, SizeOf(AInt)*5);
  GetMem(Data, MaxLen+1);
  S.ReadStrV(Data^);
  {S.Read(Data^[0], 1); S.Read(Data^[1], Length(Data^));}
  if Options and ofVersion >= ofVersion20 then
    Validator := PValidator(S.Get);
  Options := Options or ofVersion20;
  SetState(sfCursorIns, (InterfaceData.Options and ouiBlockInsertCursor <>
       0));
  end;

destructor TInputLine.Done;
  begin
  FreeMem(Data, MaxLen+1);
  Data := nil;
  SetValidator(nil);
  TView.Done;
  end;

function TInputLine.CanScroll(Delta: integer): Boolean;
  begin
  if Data = nil then
    CanScroll := False
  else if Delta < 0 then
    CanScroll := FirstPos > 0
  else if Delta > 0 then
    CanScroll := Length(Data^)-FirstPos+2 > Size.X
  else
    CanScroll := False;
  end;

function TInputLine.DataSize: word;
  var
    DSize: word;
  begin
  DSize := 0;
  if Data <> nil then
    begin
    if Validator <> nil then
      DSize := Validator^.Transfer(Data^, nil, vtDataSize);
    if DSize <> 0 then
      DataSize := DSize
    else
      DataSize := MaxLen+1;
    end
  else
    DataSize := MaxLen+1; {Cat}
  end;

procedure TInputLine.Draw;
  var
    Color: Byte;
    L, R: integer;
    B: TDrawBuffer;
    S: String;
  begin
  if Data = nil then
    exit;
  if DrawShift = 0 then
    SelStart := SelEnd;
  if State and sfFocused = 0 then
    Color := GetColor(1)
  else
    Color := GetColor(2);
  MoveChar(B, ' ', Color, Size.X);
  S := Copy(Data^, FirstPos+1, Size.X-2*DrawShift);
  MoveStr(B[DrawShift], S, Color);
  if Options and ofSecurity <> 0 then
    MoveChar(B[DrawShift], #254, Color, Length(S));
  if DrawShift > 0 then
    begin
    if CanScroll(1) then
      MoveChar(B[Size.X-1], #16, GetColor(4), 1)
    else
      MoveChar(B[Size.X-1], #32, GetColor(4), 1);
    MoveChar(B[0], #32, GetColor(4), 1);
    end;
  if State and sfSelected <> 0 then
    begin
    if  (DrawShift > 0) and CanScroll(-1) then
      MoveChar(B[0], #17, GetColor(4), 1);
    L := SelStart-FirstPos;
    R := SelEnd-FirstPos;
    if L < 0 then
      L := 0;
    if R > Size.X-2*DrawShift then
      R := Size.X-2*DrawShift;
    if L < R then
      MoveChar(B[L+1], #0, GetColor(3), R-L);
    end;
  WriteLine(0, 0, Size.X, Size.Y, B);
  SetCursor(CurPos-FirstPos+DrawShift, 0);
  end { TInputLine.Draw };

procedure TInputLine.GetData(var Rec);
  begin
  if Data = nil then
    FillChar(Rec, DataSize, #0)
  else if (Validator = nil) or
      (Validator^.Transfer(Data^, @Rec, vtGetData) = 0)
  then
    begin
    FillChar(Rec, DataSize, #0);
    Move(Data^, Rec, Length(Data^)+1);
    end;
  end;

function TInputLine.GetPalette: PPalette;
  const
    P: String[Length(CInputLine)] = CInputLine;
  begin
  GetPalette := @P;
  end;

procedure TInputLine.HandleEvent(var Event: TEvent);
  const
    PadKeys = [$47, $4B, $4D, $4F, $73, $74];
  var
    Delta, Anchor, I: integer;
    OldScanCode: Byte;
    ExtendBlock: Boolean;
    OldData: String;
    OldCurPos, OldFirstPos,
    OldSelStart, OldSelEnd: integer;
    S: String;
    pS: PString;
    R: TRect;

  function MouseDelta: integer;
    var
      Mouse: TPoint;
    begin
    MakeLocal(Event.Where, Mouse);
    if Mouse.X < DrawShift then
      MouseDelta := -1
    else if Mouse.X >= Size.X-DrawShift then
      MouseDelta := 1
    else
      MouseDelta := 0;
    end;

  function MousePos: integer;
    var
      Pos: integer;
      Mouse: TPoint;
    begin
    MakeLocal(Event.Where, Mouse);
    if Mouse.X < DrawShift then
      Mouse.X := DrawShift;
    Pos := Mouse.X+FirstPos-DrawShift;
    if Pos < 0 then
      Pos := 0;
    if Pos > Length(Data^) then
      Pos := Length(Data^);
    MousePos := Pos;
    end;

  procedure DeleteSelect;
    begin
    if SelStart <> SelEnd then
      begin
      Delete(Data^, SelStart+1, SelEnd-SelStart);
      CurPos := SelStart;
      end;
    Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
    end;

  procedure AdjustSelectBlock;
    begin
    if CurPos < Anchor then
      begin
      SelStart := CurPos;
      SelEnd := Anchor;
      end
    else
      begin
      SelStart := Anchor;
      SelEnd := CurPos;
      end;
    end;

  procedure SaveState;
    begin
    if Validator <> nil then
      begin
      OldData := Data^;
      OldCurPos := CurPos;
      OldFirstPos := FirstPos;
      OldSelStart := SelStart;
      OldSelEnd := SelEnd;
      end;
    end;

  procedure RestoreState;
    begin
    if Validator <> nil then
      begin
      Data^:= OldData;
      CurPos := OldCurPos;
      FirstPos := OldFirstPos;
      SelStart := OldSelStart;
      SelEnd := OldSelEnd;
      end;
    Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
    end;

  function CheckValid(NoAutoFill: Boolean): Boolean;
    var
      OldLen: integer;
      NewData: String;
    begin
    if Validator <> nil then
      begin
      CheckValid := False;
      OldLen := Length(Data^);
      NewData := Data^;
      if not Validator^.IsValidInput(NewData, NoAutoFill) then
        RestoreState
      else
        begin
        if Length(NewData) > MaxLen then
          SetLength(NewData, MaxLen);
        Data^:= NewData;
        if  (CurPos >= OldLen) and (Length(Data^) > OldLen) then
          CurPos := Length(Data^);
        CheckValid := True;
        end;
      end
    else
      CheckValid := True;
    end { CheckValid };

  procedure InsChar(c: Char);
    begin
    if  (State and sfCursorIns <> 0) xor
        (InterfaceData.Options and ouiBlockInsertCursor <> 0)
    then
      Delete(Data^, CurPos+1, 1)
    else
      DeleteSelect;
    if CheckValid(True) then
      begin
      if Length(Data^) < MaxLen then
        begin
        if FirstPos > CurPos then
          FirstPos := CurPos;
        Inc(CurPos);
        Insert(c, Data^, CurPos);
        end;
      CheckValid(False);
      end;
    Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
    end;

  procedure ASCIITable;
    var
      P: PWindow;
      W: word;
      E: TEvent;
      R: TRect;
      CR: PView;
    function GetCH: Boolean;
      begin
      W := Desktop^.ExecView(P);
      P^.GetData(CharASCII);
      if W in [cmOK, cmYes] then
        begin
        InsChar(CharASCII);
        DrawView;
        end;
      GetCH := W = cmYes;
      end;
    begin
    P := New(PASCIIChart, Init(R));
    P^.MoveTo(boundsASCII.X, boundsASCII.Y);
    P^.SetData(CharASCII);
    CR := Desktop^.Current;
    {  Desktop^.Lock;}
    while GetCH do
      ;
    {  Desktop^.UnLock;}
    boundsASCII := P^.Origin;
    Dispose(P, Done);
    if CR <> nil then
      CR^.Select;
    end { ASCIITable };

  begin { TInputLine.HandleEvent }
  TView.HandleEvent(Event);
  if Data = nil then
    exit;
  if State and sfSelected <> 0 then
    begin
    case Event.What of
      evMouseDown:
        begin
        Delta := MouseDelta;
        if CanScroll(Delta) then
          begin
          repeat
            if CanScroll(Delta) then
              begin
              Inc(FirstPos, Delta);
              DrawView;
              end;
          until not MouseEvent(Event, evMouseAuto);
          end
        else if Event.Double then
          SelectAll(True)
        else
          begin
          Anchor := MousePos;
          repeat
            if Event.What = evMouseAuto then
              begin
              Delta := MouseDelta;
              if CanScroll(Delta) then
                Inc(FirstPos, Delta);
              end;
            CurPos := MousePos;
            AdjustSelectBlock;
            DrawView;
          until not MouseEvent(Event, evMouseMove+evMouseAuto);
          end;
        ClearEvent(Event);
        end;
      evKeyDown:
        begin
        SaveState;
        {Event.KeyCode := CtrlToArrow(Event.KeyCode);}
        if  (Event.ScanCode in PadKeys) and
            (ShiftState and $03 <> 0)
        then
          begin
          Event.CharCode := #0;
          if CurPos = SelEnd then
            Anchor := SelStart
          else
            Anchor := SelEnd;
          ExtendBlock := True;
          end
        else
          ExtendBlock := False;
        if CtrlK then
          begin
          case Event.KeyCode of
            kbCtrlSqBracketL:
              begin
              if SelStart < SelEnd then
                for I := SelStart+1 to SelEnd do
                  Data^[I] := UpCase(Data^[I]);
              if CurPos = SelEnd then
                Anchor := SelStart
              else
                Anchor := SelEnd;
              ExtendBlock := True
              end;
            kbCtrlSqBracketR:
              begin
              if SelStart < SelEnd then
                for I := SelStart+1 to SelEnd do
                  Data^[I] := LowCase(Data^[I]);
              if CurPos = SelEnd then
                Anchor := SelStart
              else
                Anchor := SelEnd;
              ExtendBlock := True
              end;
            kbCtrlSlash,
            kbCtrlBSlash:
              begin
              S := CapStrg(Copy(Data^, SelStart+1, SelEnd-SelStart));
              Delete(Data^, SelStart+1, SelEnd-SelStart);
              Insert(S, Data^, SelStart+1);
              if CurPos = SelEnd then
                Anchor := SelStart
              else
                Anchor := SelEnd;
              ExtendBlock := True
              end;
          end {case};
          CtrlK := False;
          end
        else
          case Event.KeyCode of
            kbCtrlK:
              CtrlK := True;
            kbAltF12:
              begin
              OldData := RusEngConv(Copy(Data^, SelStart+1,
                     SelEnd-SelStart));
              Data^:= Copy(Data^, 1, SelStart)
                  +OldData+Copy(Data^, SelEnd+1, MaxStringLength);
              if CurPos = SelEnd then
                Anchor := SelStart
              else
                Anchor := SelEnd;
              ExtendBlock := True
              end;
            kbAltCtrlSqBracketL,
            kbCtrlShiftSqBracketL,
            kbCtrlSqBracketL:
              if ShiftState and (kbLeftShift+kbRightShift) <> 0
              then
                UpStr(Data^)
              else
                begin
                I := CurPos;
                while (not (Data^[I] in BreakChars)) and (I > 1) do
                  Dec(I);
                repeat
                  Data^[I] := UpCase(Data^[I]);
                  Inc(I);
                until (Data^[I] in BreakChars) or (I > Length(Data^));
                end;
            kbAltCtrlSqBracketR,
            kbCtrlShiftSqBracketR,
            kbCtrlSqBracketR:
              if ShiftState and (kbLeftShift+kbRightShift) <> 0
              then
                LowStr(Data^)
              else
                begin
                I := CurPos;
                while (not (Data^[I] in BreakChars)) and (I > 1) do
                  Dec(I);
                repeat
                  Data^[I] := LowCase(Data^[I]);
                  Inc(I);
                until (Data^[I] in BreakChars) or (I > Length(Data^));
                end;
            kbCtrlShiftSlash,
            kbCtrlShiftBSlash,
            kbCtrlSlash,
            kbCtrlBSlash:
              if ShiftState and (kbLeftShift+kbRightShift) <> 0
              then
                CapStr(Data^)
              else
                begin
                I := CurPos;
                while (not (Data^[I] in BreakChars)) and (I > 1) do
                  Dec(I);
                if  (Data^[I] in BreakChars) then
                  Inc(I);
                Data^[I] := UpCase(Data^[I]);
                Inc(I);
                repeat
                  Data^[I] := LowCase(Data^[I]);
                  Inc(I);
                until (Data^[I] in BreakChars) or (I > Length(Data^));
                end;
            kbCtrlBack:
              if CurPos > 0 then
                begin
                if  (Data^[CurPos] in BreakChars) then
                  repeat
                    Delete(Data^, CurPos, 1);
                    Dec(CurPos)
                  until (CurPos = 0) or not (Data^[CurPos] in BreakChars)
                else
                  repeat
                    Delete(Data^, CurPos, 1);
                    Dec(CurPos)
                  until (CurPos = 0) or (Data^[CurPos] in BreakChars)
                end;
            kbCtrlIns:
              if Options and ofSecurity = 0 then
                begin
                S := Copy(Data^, SelStart+1, SelEnd-SelStart);
                if S <> '' then
                  Message(Application, evCommand, cmPutInClipboard, @S);
                ClearEvent(Event);
                exit;
                end;
            kbCtrlLeft:
              begin
              if CurPos > 0 then
                repeat
                  Dec(CurPos)
                until (CurPos = 0) or not (Data^[CurPos+1] in BreakChars);
              while (CurPos > 0) and not (Data^[CurPos] in BreakChars)
              do
                Dec(CurPos)
              end;
            kbCtrlRight:
              begin
              if CurPos < Length(Data^) then
                repeat
                  Inc(CurPos)
                until (CurPos >= Length(Data^))
                   or (Data^[CurPos+1] in BreakChars);
              if CurPos < Length(Data^) then
                repeat
                  Inc(CurPos)
                until (CurPos >= Length(Data^))
                   or not (Data^[CurPos+1] in BreakChars);
              end;
            kbShiftDel:
              begin
              S := Copy(Data^, SelStart+1, SelEnd-SelStart);
              if S <> '' then
                Message(Application, evCommand, cmPutInClipboard, @S);
              DeleteSelect;
              SelEnd := SelStart;
              DrawView;
              ClearEvent(Event);
              Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
              exit;
              end;
            kbShiftIns:
              begin
              ClearEvent(Event);
              S := '';
              DeleteSelect;
              Message(Application, evCommand, cmGetFromClipboard, @S);
              if S = '' then
                exit;
              SelStart := CurPos;
              SelEnd := CurPos+Length(S);
              OldData := Data^;
              Insert(S, OldData, CurPos+1);
              Data^:= Copy(OldData, 1, MaxLen);
              DrawView;
              Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
              exit;
              end;
            kbAlt1, kbAlt2, kbAlt3, kbAlt4, kbAlt5, kbAlt6, kbAlt7,
             kbAlt8, kbAlt9:
              begin
              ClearEvent(Event);
              S := '';
              DeleteSelect;
              pS := DirsToChange[Event.ScanCode-Hi(kbAlt1)];
              if  (pS = nil) or (Length(pS^) = 0) then
                exit
              else
                S := pS^;
              if S = '' then
                exit;
              SelStart := CurPos;
              SelEnd := CurPos+Length(S);
              OldData := Data^;
              Insert(S, OldData, CurPos+1);
              Data^:= Copy(OldData, 1, MaxLen);
              DrawView;
              exit;
              end;
            kbLeft, kbShiftLeft:
              if CurPos > 0 then
                Dec(CurPos);
            kbRight, kbShiftRight:
              if CurPos < Length(Data^) then
                Inc(CurPos);
            kbHome, kbShiftHome:
              CurPos := 0;
            kbEnd, kbShiftEnd:
              CurPos := Length(Data^);
            //JO: Для двух нижележащих клавиш (BackSpace и Del) зачем-то ещё со вpемён
            //    Ритлабовской веpсии в конце вставлено CheckValid. Зачем это надо
            //    - непонятно, т.к. это пpовеpка на пpевышение длины стpоки ввода и
            //    наличие в ней недопустимых символов. В pезультате удаления символа ни
            //    то, ни дpугое появиться не может. А вот невозможность удалить
            //    недопустимые символы, вставленные из буфеpа - возникает.
            kbBack:
              if CurPos > 0 then
                begin
                Delete(Data^, CurPos, 1);
                Dec(CurPos);
                if FirstPos > 0 then
                  Dec(FirstPos);
                {CheckValid(True);}
                end;
            kbDel:
              begin
              if SelStart = SelEnd then
                if CurPos < Length(Data^) then
                  begin
                  SelStart := CurPos;
                  SelEnd := CurPos+1;
                  end;
              DeleteSelect;
              {CheckValid(True);}
              end;
            kbIns:
              SetState(sfCursorIns, State and sfCursorIns = 0);
            else {case}
              case Event.CharCode of
                ' '..#254:
                  InsChar(Event.CharCode);
                ^P, ^B:
                  begin
                  ASCIITable;
                  ClearEvent(Event);
                  end;
                ^Y:
                  begin
                  Data^:= '';
                  CurPos := 0;
                  end;
                else {case}
                  exit;
              end

          end {case};
        if not CtrlK then
          if ExtendBlock then
            AdjustSelectBlock
          else
            begin
            SelStart := CurPos;
            SelEnd := CurPos;
            end;
        if FirstPos > CurPos then
          FirstPos := CurPos;
        I := CurPos-Size.X+2*DrawShift;
        if FirstPos < I then
          FirstPos := I;
        Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
        DrawView;
        ClearEvent(Event);
        end;
    end {case};
    end;
  end { TInputLine.HandleEvent };

procedure TInputLine.SelectAll(Enable: Boolean);
  begin
  if Data = nil then
    exit;
  CurPos := 0;
  FirstPos := 0;
  SelStart := 0;
  if Enable then
    SelEnd := Length(Data^)
  else
    SelEnd := 0;
  DrawView;
  end;

procedure TInputLine.SetData(var Rec);
  begin
  if Data = nil then
    exit;
  if  (Validator = nil) or
      (Validator^.Transfer(Data^, @Rec, vtSetData) = 0)
  then
    Move(Rec, Data^ {[0]}, DataSize);
  Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
  SelectAll(True);
  end;

procedure TInputLine.SetState(AState: word; Enable: Boolean);
  begin
  TView.SetState(AState, Enable);
  if Data = nil then
    exit;
  if  (AState = sfSelected) or ((AState = sfActive) and
        (State and sfSelected <> 0))
  then
    SelectAll(Enable);
  end;

procedure TInputLine.SetValidator(AValid: PValidator);
  begin
  if Validator <> nil then
    Validator^.Free;
  Validator := AValid;
  end;

procedure TInputLine.Store(var S: TStream);
  begin
  TView.Store(S);
  {Cat:warn AnsiString}
  S.Write(MaxLen, SizeOf(AInt)*5);
  S.WriteStr(Data);
  S.Put(Validator);
  end;

function TInputLine.Valid(Command: word): Boolean;
  begin
  Valid := inherited Valid(Command);
  if Validator <> nil then
    if Command = cmValid then
      Valid := Validator^.Status = vsOk
    else if Command <> cmCancel then
      if not Validator^.Valid(Data^) then
        begin
        Select;
        Valid := False;
        end;
  end;

{ TButton }

constructor TButton.Init(var Bounds: TRect; const ATitle: TTitleStr;
    ACommand: word; AFlags: word);
  begin
  TView.Init(Bounds);
  Options := Options or (ofSelectable+ofFirstClick+
      ofPreProcess+ofPostProcess);
  EventMask := EventMask or evBroadcast;
  if not CommandEnabled(ACommand) then
    State := State or sfDisabled;
  Flags := AFlags;
  if AFlags and bfDefault <> 0 then
    AmDefault := True
  else
    AmDefault := False;
  Title := NewStr(ATitle);
  Command := ACommand;
  end;

constructor TButton.Load(var S: TStream);
  begin
  TView.Load(S);
  Title := S.ReadStr;
  S.Read(Command, SizeOf(AWord)+SizeOf(Byte)+SizeOf(Boolean));
  if not CommandEnabled(Command) then
    State := State or sfDisabled
  else
    State := State and not sfDisabled;
  end;

destructor TButton.Done;
  begin
  DisposeStr(Title);
  TView.Done;
  end;

procedure TButton.Draw;
  begin
  DrawState(False);
  end;

procedure TButton.DrawState(Down: Boolean);
  var
    CButton, CShadow: word;
    Ch: Char;
    I, S, Y, T: integer;
    B: TDrawBuffer;

  procedure DrawTitle;
    var
      L, SCOff: integer;
    begin
    if Flags and bfLeftJust <> 0 then
      L := 1
    else
      begin
      L := (S-CStrLen(Title^)-1) div 2;
      if L < 1 then
        L := 1;
      end;
    MoveCStr(B[i+L], Title^, CButton);
    if ShowMarkers and not Down then
      begin
      if State and sfSelected <> 0 then
        SCOff := 0
      else if AmDefault then
        SCOff := 2
      else
        SCOff := 4;
      WordRec(B[0]).Lo := Byte(SpecialChars[SCOff]);
      WordRec(B[S]).Lo := Byte(SpecialChars[SCOff+1]);
      end;
    end { DrawTitle };

  begin { TButton.DrawState }
  if State and sfDisabled <> 0 then
    CButton := GetColor($0404)
  else
    begin
    CButton := GetColor($0501);
    if State and sfActive <> 0 then
      if State and sfSelected <> 0 then
        CButton := GetColor($0703)
      else if AmDefault then
        CButton := GetColor($0602);
    end;
  CShadow := GetColor(8);
  S := Size.X-1;
  T := Size.Y div 2-1;
  for Y := 0 to Size.Y-2 do
    begin
    MoveChar(B, ' ', Byte(CButton), Size.X);
    WordRec(B[0]).Hi := CShadow;
    if Down then
      begin
      WordRec(B[1]).Hi := CShadow;
      Ch := ' ';
      I := 2;
      end
    else
      begin
      WordRec(B[S]).Hi := Byte(CShadow);
      if ShowMarkers then
        Ch := ' '
      else
        begin
        if Y = 0 then
          WordRec(B[S]).Lo := Byte(#220)
        else
          WordRec(B[S]).Lo := Byte(#219);
        Ch := #223;
        end;
      I := 1;
      end;
    if  (Y = T) and (Title <> nil) then
      DrawTitle;
    if ShowMarkers
    then
      if not Down
      then
        begin
        WordRec(B[1]).Lo := Byte('[');
        WordRec(B[S-1]).Lo := Byte(']');
        end
      else
    else if (State and sfSelected <> 0) or (AmDefault) then
      if not Down then
        begin
        WordRec(B[1]).Lo := Byte('');
        WordRec(B[S-1]).Lo := Byte('');
        end
      else
        begin
        WordRec(B[2]).Lo := Byte('');
        WordRec(B[S]).Lo := Byte('');
        end;

    WriteLine(0, Y, Size.X, 1, B);
    end;
  MoveChar(B[0], ' ', Byte(CShadow), 2);
  MoveChar(B[2], Ch, Byte(CShadow), S-1);
  WriteLine(0, Size.Y-1, Size.X, 1, B);
  end { TButton.DrawState };

function TButton.GetPalette: PPalette;
  const
    P: String[Length(CButton)] = CButton;
  begin
  GetPalette := @P;
  end;

procedure TButton.HandleEvent(var Event: TEvent);
  var
    Down: Boolean;
    C: Char;
    Mouse: TPoint;
    ClickRect: TRect;
  begin
  GetExtent(ClickRect);
  Inc(ClickRect.A.X);
  Dec(ClickRect.B.X);
  Dec(ClickRect.B.Y);
  if Event.What = evMouseDown then
    begin
    MakeLocal(Event.Where, Mouse);
    if not ClickRect.Contains(Mouse) then
      ClearEvent(Event);
    end;
  if Flags and bfGrabFocus <> 0 then
    TView.HandleEvent(Event);
  case Event.What of
    evMouseDown:
      begin
      if State and sfDisabled = 0 then
        begin
        Inc(ClickRect.B.X);
        Down := False;
        repeat
          MakeLocal(Event.Where, Mouse);
          if Down <> ClickRect.Contains(Mouse) then
            begin
            Down := not Down;
            DrawState(Down);
            end;
        until not MouseEvent(Event, evMouseMove);
        if Down then
          begin
          Press;
          DrawState(False);
          end;
        end;
      ClearEvent(Event);
      end;
    evKeyDown:
      if  ( (Event.KeyCode = kbRight) or (Event.KeyCode = kbDown)) and
          (State and sfSelected <> 0)
      then
        begin
        Event.KeyCode := kbTab;
        TView.HandleEvent(Event);
        end
      else if ((Event.KeyCode = kbLeft) or (Event.KeyCode = kbUp)) and
          (State and sfSelected <> 0)
      then
        begin
        Event.KeyCode := kbShiftTab;
        TView.HandleEvent(Event);
        end
      else
        begin
        C := HotKey(Title^);
        if  (C <> #0) and (
              (GetAltCode(C) = Event.KeyCode) or (
                (Owner^.Phase = phPostProcess) and (
                  (UpCaseArray[Event.CharCode] = C) or
                  (UpCaseArray[GetAltChar(Event.KeyCode and $FFFF00)] = C)
                ))) or
            (State and sfSelected <> 0) and (Event.CharCode = ' ') or
            (State and sfSelected <> 0) and (Event.KeyCode = kbEnter)
        then
          begin
          Press;
          ClearEvent(Event);
          end;
        end;
    evBroadcast:
      case Event.Command of
        cmDefault:
          if AmDefault then
            begin
            Press;
            ClearEvent(Event);
            end;
        cmGrabDefault, cmReleaseDefault:
          if Flags and bfDefault <> 0 then
            begin
            AmDefault := Event.Command = cmReleaseDefault;
            DrawView;
            end;
        cmCommandSetChanged:
          begin
          SetState(sfDisabled, not CommandEnabled(Command));
          DrawView;
          end;
      end {case};
  end {case};
  end { TButton.HandleEvent };

procedure TButton.MakeDefault(Enable: Boolean);
  var
    C: word;
  begin
  if Flags and bfDefault = 0 then
    begin
    if Enable then
      C := cmGrabDefault
    else
      C := cmReleaseDefault;
    Message(Owner, evBroadcast, C, @Self);
    AmDefault := Enable;
    DrawView;

    end;
  end;

procedure TButton.Press;
  var
    E: TEvent;
  begin
  Message(Owner, evBroadcast, cmRecordHistory, nil);
  if Flags and bfBroadcast <> 0 then
    Message(Owner, evBroadcast, Command, @Self)
  else
    begin
    E.What := evCommand;
    E.Command := Command;
    E.InfoPtr := @Self;
    PutEvent(E);
    end;
  end;

procedure TButton.SetState(AState: word; Enable: Boolean);
  begin
  TView.SetState(AState, Enable);
  if AState and (sfSelected+sfActive) <> 0 then
    DrawView;
  if AState and sfFocused <> 0 then
    MakeDefault(Enable);
  end;

procedure TButton.Store(var S: TStream);
  begin
  TView.Store(S);
  S.WriteStr(Title);
  S.Write(Command, SizeOf(AWord)+SizeOf(Byte)+SizeOf(Boolean));
  end;

{ TCluster }

constructor TCluster.Init(var Bounds: TRect; AStrings: PSItem);
  var
    I: integer;
    P: PSItem;
  begin
  TView.Init(Bounds);
  Options := Options or (ofSelectable+ofFirstClick+ofPreProcess+
      ofPostProcess+ofVersion20);
  I := 0;
  P := AStrings;
  while P <> nil do
    begin
    Inc(I);
    P := P^.Next;
    end;
  Strings.Init(I, 0, False);
  while AStrings <> nil do
    begin
    P := AStrings;
    Strings.AtInsert(Strings.Count, AStrings^.Value);
    AStrings := AStrings^.Next;
    Dispose(P);
    end;
  Value := 0;
  Sel := 0;
  SetCursor(2, 0);
  ShowCursor;
  EnableMask := $FFFFFFFF;
  end { TCluster.Init };

constructor TCluster.Load(var S: TStream);
  begin
  TView.Load(S);
  if  (Options and ofVersion) >= ofVersion20 then
    begin
    {Cat:warn глюки при смене порядка полей}
    S.Read(Value, SizeOf(LongInt)*2+SizeOf(AInt));
    end
  else
    begin
    S.Read(Value, SizeOf(AWord));
    S.Read(Sel, SizeOf(AInt));
    EnableMask := $FFFFFFFF;
    Options := Options or ofVersion20;
    end;
  Strings.Load(S);
  SetButtonState(0, True);
  end;

destructor TCluster.Done;
  begin
  Strings.Done;
  TView.Done;
  end;

function TCluster.ButtonState(Item: integer): Boolean;
  var
    q: LongInt;
  begin
  ButtonState := False;
  if Item > 31 then
    exit;
  q := 1 shl Item;
  ButtonState := (EnableMask and q <> 0);
  end;

function TCluster.DataSize: word;
  begin
  DataSize := SizeOf(word);
  end;

procedure TCluster.DrawBox(const Icon: String; Marker: Char);
  begin
  DrawMultiBox(Icon, ' '+Marker);
  end;

procedure TCluster.DrawMultiBox(const Icon, Marker: String);
  var
    I, J, Cur, Col: integer;
    CNorm, CSel, CDis, Color: word;
    B: TDrawBuffer;
    SCOff: Byte;
  begin
  CNorm := GetColor($0301);
  CSel := GetColor($0402);
  CDis := GetColor($0505);
  for I := 0 to Size.Y do
    begin
    MoveChar(B, ' ', Byte(CNorm), Size.X);
    for J := 0 to (Strings.Count-1) div Size.Y+1 do
      begin
      Cur := J*Size.Y+I;
      if Cur < Strings.Count then
        begin
        Col := Column(Cur);
        if  (Col+CStrLen(CnvString(Strings.At(Cur)))+5 <
            SizeOf(TDrawBuffer) div SizeOf(word)) and (Col < Size.X)
        then
          begin
          if not ButtonState(Cur) then
            Color := CDis
          else if (Cur = Sel) and (State and sfSelected <> 0) then
            Color := CSel
          else
            Color := CNorm;
          MoveChar(B[Col], ' ', Byte(Color), Size.X-Col);
          MoveStr(B[Col], Icon, Byte(Color));
          WordRec(B[Col+2]).Lo := Byte(Marker[MultiMark(Cur)+1]);
          MoveCStr(B[Col+5], CnvString(Strings.At(Cur)), Color);
          if ShowMarkers and (State and sfSelected <> 0) and (Cur = Sel)
          then
            begin
            WordRec(B[Col]).Lo := Byte(SpecialChars[0]);
            WordRec(B[Column(Cur+Size.Y)-1]).Lo := Byte(SpecialChars[1]);
            end;
          end;
        end;
      end;
    WriteBuf(0, I, Size.X, 1, B);
    end;
  SetCursor(Column(Sel)+2, Row(Sel));
  end { TCluster.DrawMultiBox };

procedure TCluster.GetData(var Rec);
  begin
  word(Rec) := Value;
  DrawView;
  end;

function TCluster.GetHelpCtx: word;
  begin
  if HelpCtx = hcNoContext then
    GetHelpCtx := hcNoContext
  else
    GetHelpCtx := HelpCtx+Sel;
  end;

function TCluster.GetPalette: PPalette;
  const
    P: String[Length(CCluster)] = CCluster;
  begin
  GetPalette := @P;
  end;

procedure TCluster.HandleEvent(var Event: TEvent);
  var
    Mouse: TPoint;
    I, S: integer;
    C: Char;

  procedure MoveSel;
    begin
    if i <= Strings.Count then
      begin
      Sel := S;
      MovedTo(Sel);
      DrawView;
      end;
    end;

  begin
  TView.HandleEvent(Event);
  if  (Options and ofSelectable) = 0 then
    exit;
  if Event.What = evMouseDown then
    begin
    MakeLocal(Event.Where, Mouse);
    I := FindSel(Mouse);
    if I <> -1 then
      if ButtonState(I) then
        Sel := I;
    DrawView;
    repeat
      MakeLocal(Event.Where, Mouse);
      if FindSel(Mouse) = Sel then
        ShowCursor
      else
        HideCursor;
    until not MouseEvent(Event, evMouseMove); {Wait for mouse up}
    ShowCursor;
    MakeLocal(Event.Where, Mouse);
    if  (FindSel(Mouse) = Sel) and ButtonState(Sel) then
      begin
      Press(Sel);
      DrawView;
      end;
    ClearEvent(Event);
    end
  else if Event.What = evKeyDown then
    begin
    S := Sel;
    case {CtrlToArrow}(Event.KeyCode) of
      kbUp:
        if State and sfFocused <> 0 then
          begin
          I := 0;
          repeat
            Inc(I);
            Dec(S);
            if S < 0 then
              S := Strings.Count-1;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
          end;
      kbDown:
        if State and sfFocused <> 0 then
          begin
          I := 0;
          repeat
            Inc(I);
            Inc(S);
            if S >= Strings.Count then
              S := 0;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
          end;
      kbRight:
        if State and sfFocused <> 0 then
          begin
          I := 0;
          repeat
            Inc(I);
            Inc(S, Size.Y);
            if S >= Strings.Count then
              begin
              S := (S+1) mod Size.Y;
              if S >= Strings.Count then
                S := 0;
              end;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
          end;
      kbLeft:
        if State and sfFocused <> 0 then
          begin
          I := 0;
          repeat
            Inc(I);
            if S > 0 then
              begin
              Dec(S, Size.Y);
              if S < 0 then
                begin
                S := ((Strings.Count+Size.Y-1) div Size.Y)*Size.Y+S-1;
                if S >= Strings.Count then
                  S := Strings.Count-1;
                end;
              end
            else
              S := Strings.Count-1;
          until ButtonState(S) or (I > Strings.Count);
          MoveSel;
          ClearEvent(Event);
          end;
      else {case}
        begin
        for I := 0 to Strings.Count-1 do
          begin
          C := HotKey(CnvString(Strings.At(I)));
          if  (C <> #0) and (
                (GetAltCode(C) = Event.KeyCode) or (
                  (Owner^.Phase = phPostProcess) and (
                    (UpCaseArray[Event.CharCode] = C) or
                    (UpCaseArray[GetAltChar(Event.KeyCode and $FFFF00)] =
                     C)
                  )))
          then
            begin
            if ButtonState(I) then
              begin
              if Focus then
                begin
                Sel := I;
                MovedTo(Sel);
                Press(Sel);
                DrawView;
                end;
              ClearEvent(Event);
              end;
            exit;
            end;
          end;
        if  (Event.CharCode = ' ') and (State and sfFocused <> 0) then
          begin
          Press(Sel);
          DrawView;
          ClearEvent(Event);
          end;
        end
    end
    end;
  end { TCluster.HandleEvent };

procedure TCluster.SetButtonState(AMask: LongInt; Enable: Boolean);
  begin
  if Enable then
    EnableMask := EnableMask or AMask
  else
    EnableMask := EnableMask and (not AMask);
  if EnableMask = 0 then
    Options := Options and (not ofSelectable);
  end;

procedure TCluster.SetData(var Rec);
  begin
  Value := word(Rec);
  DrawView;
  end;

procedure TCluster.SetState(AState: word; Enable: Boolean);
  begin
  TView.SetState(AState, Enable);
  if AState = sfSelected then
    DrawView;
  end;

function TCluster.Mark(Item: integer): Boolean;
  begin
  Mark := False;
  end;

function TCluster.MultiMark(Item: integer): Byte;
  begin
  MultiMark := Byte(Mark(Item) = True);
  end;

procedure TCluster.MovedTo(Item: integer);
  begin
  end;

procedure TCluster.Press(Item: integer);
  begin
  end;

procedure TCluster.Store(var S: TStream);
  begin
  TView.Store(S);
  {Cat:warn глюки при смене порядка полей}
  S.Write(Value, SizeOf(Value)+SizeOf(Sel)+SizeOf(EnableMask));
  Strings.Store(S);
  end;

function TCluster.Column(Item: integer): integer;
  var
    I, Col, Width, L: integer;
  begin
  if Item < Size.Y then
    Column := 0
  else
    begin
    Width := 0;
    Col := -6;
    for I := 0 to Item do
      begin
      if I mod Size.Y = 0 then
        begin
        Inc(Col, Width+6);
        Width := 0;
        end;
      if I < Strings.Count then
        L := CStrLen(CnvString(Strings.At(I)));
      if L > Width then
        Width := L;
      end;
    Column := Col;
    end;
  end { TCluster.Column };

function TCluster.FindSel(P: TPoint): integer;
  var
    I, S: integer;
    R: TRect;
  begin
  GetExtent(R);
  if not R.Contains(P) then
    FindSel := -1
  else
    begin
    I := 0;
    while P.X >= Column(I+Size.Y) do
      Inc(I, Size.Y);
    S := I+P.Y;
    if S >= Strings.Count then
      FindSel := -1
    else
      FindSel := S;
    end;
  end;

function TCluster.Row(Item: integer): integer;
  begin
  Row := Item mod Size.Y;
  end;

{ TRadioButtons }

procedure TRadioButtons.Draw;
  const
    Button = ' ( ) ';
  begin
  DrawMultiBox(Button, #32#7);
  end;

function TRadioButtons.Mark(Item: integer): Boolean;
  begin
  Mark := Item = Value;
  end;

procedure TRadioButtons.Press(Item: integer);
  begin
  Value := Item;
  end;

procedure TRadioButtons.MovedTo(Item: integer);
  begin
  Value := Item;
  end;

procedure TRadioButtons.SetData(var Rec);
  begin
  TCluster.SetData(Rec);
  Sel := integer(Value);
  end;

{ TCheckBoxes }

procedure TCheckBoxes.Draw;
  const
    Button = ' [ ] ';
  begin
  DrawMultiBox(Button, ' X');
  end;

function TCheckBoxes.Mark(Item: integer): Boolean;
  begin
  Mark := Value and (1 shl Item) <> 0;
  end;

procedure TCheckBoxes.Press(Item: integer);
  begin
  Value := (Value xor (1 shl Item)) and $FFFF;
  end;

{ TMultiCheckBoxes }

constructor TMultiCheckBoxes.Init(var Bounds: TRect; AStrings: PSItem;
    ASelRange: Byte; AFlags: word; const AStates: String);
  begin
  inherited Init(Bounds, AStrings);
  SelRange := ASelRange;
  Flags := AFlags;
  States := NewStr(AStates);
  end;

constructor TMultiCheckBoxes.Load(var S: TStream);
  begin
  TCluster.Load(S);
  S.Read(SelRange, SizeOf(Byte));
  S.Read(Flags, SizeOf(AWord));
  States := S.ReadStr;
  end;

destructor TMultiCheckBoxes.Done;
  begin
  DisposeStr(States);
  TCluster.Done;
  end;

procedure TMultiCheckBoxes.Draw;
  const
    Button = ' [ ] ';
  begin
  DrawMultiBox(Button, States^);
  end;

function TMultiCheckBoxes.DataSize: word;
  begin
  DataSize := SizeOf(LongInt);
  end;

function TMultiCheckBoxes.MultiMark(Item: integer): Byte;
  begin
  MultiMark := LongInt((Value and (WordRec(Flags).Lo
          shl (Item*WordRec(Flags).Hi))) shr (Item*WordRec(Flags).Hi));
  end;

procedure TMultiCheckBoxes.GetData(var Rec);
  begin
  LongInt(Rec) := Value;
  DrawView;
  end;

procedure TMultiCheckBoxes.Press(Item: integer);
  var
    CurState: ShortInt;
  begin
  CurState := LongInt((Value and (WordRec(Flags).Lo
          shl (Item*WordRec(Flags).Hi))) shr (Item*WordRec(Flags).Hi));
  Dec(CurState);
  if  (CurState >= SelRange) or (CurState < 0) then
    CurState := SelRange-1;
  Value := LongInt((Value and not (WordRec(Flags).Lo
          shl (Item*WordRec(Flags).Hi)) or
          (CurState shl (Item*WordRec(Flags).Hi))));
  end;

procedure TMultiCheckBoxes.SetData(var Rec);
  begin
  Value := LongInt(Rec);
  DrawView;
  end;

procedure TMultiCheckBoxes.Store(var S: TStream);
  begin
  TCluster.Store(S);
  S.Write(SelRange, SizeOf(Byte));
  S.Write(Flags, SizeOf(AWord));
  S.WriteStr(States);
  end;

{ TListBox }

type
  TListBoxRec = record
    List: PCollection;
    Selection: word;
    end;

constructor TListBox.Init(var Bounds: TRect; ANumCols: word;
    AScrollBar: PScrollBar);
  var
    ARange: integer;
  begin
  TListViewer.Init(Bounds, ANumCols, nil, AScrollBar);
  List := nil;
  SetRange(0);
  end;

constructor TListBox.Load(var S: TStream);
  begin
  TListViewer.Load(S);
  List := PCollection(S.Get);
  end;

function TListBox.DataSize: word;
  begin
  DataSize := SizeOf(TListBoxRec);
  end;

procedure TListBox.GetData(var Rec);
  begin
  TListBoxRec(Rec).List := List;
  TListBoxRec(Rec).Selection := Focused;
  end;

function TListBox.GetText(Item: LongInt; MaxLen: integer): String;
  begin
  if List <> nil then
    GetText := PString(List^.At(Item))^
  else
    GetText := '';
  end;

procedure TListBox.NewLisT(AList: PCollection);
  begin
  if List <> nil then
    Dispose(List, Done);
  List := AList;
  if AList <> nil then
    SetRange(AList^.Count)
  else
    SetRange(0);
  if Range > 0 then
    FocusItem(0);
  DrawView;
  end;

procedure TListBox.SetData(var Rec);
  begin
  NewLisT(TListBoxRec(Rec).List);
  FocusItem(TListBoxRec(Rec).Selection);
  DrawView;
  end;

procedure TListBox.Store(var S: TStream);
  begin
  TListViewer.Store(S);
  S.Put(List);
  end;

{ TStaticText }

constructor TStaticText.Init(var Bounds: TRect; const AText: String);
  begin
  TView.Init(Bounds);
  Text := NewStr(AText);
  end;

constructor TStaticText.Load(var S: TStream);
  begin
  TView.Load(S);
  Text := S.ReadStr;
  end;

destructor TStaticText.Done;
  begin
  DisposeStr(Text);
  TView.Done;
  end;

procedure TStaticText.Draw;
  var
    Color: Byte;
    Center: Boolean;
    I, J, L, P, Y: integer;
    B: TDrawBuffer;
    S: String;
  begin
  Color := GetColor(1);
  GetText(S);
  L := Length(S);
  P := 1;
  Y := 0;
  Center := False;
  while Y < Size.Y do
    begin
    MoveChar(B, ' ', Color, Size.X);
    if P <= L then
      begin
      if S[P] = #3 then
        begin
        Center := True;
        Inc(P);
        end;
      I := P;
      repeat
        J := P;
        while (P <= L) and (S[P] = ' ') do
          Inc(P);
        while (P <= L) and (S[P] <> ' ') and (S[P] <> #13) do
          Inc(P);
      until (P > L) or (P >= I+Size.X) or (S[P] = #13);
      if P > I+Size.X then
        if J > I then
          P := J
        else
          P := I+Size.X;
      if Center then
        J := (Size.X-P+I) div 2
      else
        J := 0;
      MoveBuf(B[J], S[I], Color, P-I);
      while (P <= L) and (S[P] = ' ') do
        Inc(P);
      if  (P <= L) and (S[P] = #13) then
        begin
        Center := False;
        Inc(P);
        if  (P <= L) and (S[P] = #10) then
          Inc(P);
        end;
      end;
    WriteLine(0, Y, Size.X, 1, B);
    Inc(Y);
    end;
  end { TStaticText.Draw };

function TStaticText.GetPalette: PPalette;
  const
    P: String[Length(CStaticText)] = CStaticText;
  begin
  GetPalette := @P;
  end;

procedure TStaticText.GetText(var S: String);
  begin
  if Text <> nil then
    S := Text^
  else
    S := '';
  end;

procedure TStaticText.Store(var S: TStream);
  begin
  TView.Store(S);
  S.WriteStr(Text);
  end;

{ TParamText }

constructor TParamText.Init(var Bounds: TRect; const AText: String;
    AParamCount: AInt);
  begin
  TStaticText.Init(Bounds, AText);
  ParamCount := AParamCount;
  end;

constructor TParamText.Load(var S: TStream);
  begin
  TStaticText.Load(S);
  S.Read(ParamCount, SizeOf(AInt));
  end;

function TParamText.DataSize: word;
  begin
  DataSize := ParamCount*SizeOf(LongInt);
  end;

procedure TParamText.GetText(var S: String);
  begin
  if Text <> nil then
    FormatStr(S, Text^, ParamList^)
  else
    S := '';
  end;

procedure TParamText.SetData(var Rec);
  begin
  ParamList := @Rec;
  DrawView;
  end;

procedure TParamText.Store(var S: TStream);
  begin
  TStaticText.Store(S);
  S.Write(ParamCount, SizeOf(AInt));
  end;

{ TLabel }

constructor TLabel.Init(var Bounds: TRect; const AText: String;
     ALink: PView);
  begin
  TStaticText.Init(Bounds, AText);
  Link := ALink;
  Options := Options or (ofPreProcess+ofPostProcess);
  EventMask := EventMask or evBroadcast;
  end;

constructor TLabel.Load(var S: TStream);
  begin
  TStaticText.Load(S);
  GetPeerViewPtr(S, Link);
  end;

procedure TLabel.Draw;
  var
    Color: word;
    B: TDrawBuffer;
    SCOff: Byte;
  begin
  if Light then
    begin
    Color := GetColor($0402);
    SCOff := 0;
    end
  else
    begin
    Color := GetColor($0301);
    SCOff := 4;
    end;
  MoveChar(B[0], ' ', Byte(Color), Size.X);
  if Text <> nil then
    MoveCStr(B[1], Text^, Color);
  if ShowMarkers then
    WordRec(B[0]).Lo := Byte(SpecialChars[SCOff]);
  WriteLine(0, 0, Size.X, 1, B);
  end { TLabel.Draw };

function TLabel.GetPalette: PPalette;
  const
    P: String[Length(CLabel)] = CLabel;
  begin
  GetPalette := @P;
  end;

procedure TLabel.HandleEvent(var Event: TEvent);
  var
    C: Char;

  procedure FocusLink;
    begin
    if  (Link <> nil) and (Link^.Options and ofSelectable <> 0) then
      Link^.Focus;
    ClearEvent(Event);
    end;

  begin
  TStaticText.HandleEvent(Event);
  if Event.What = evMouseDown then
    FocusLink
  else if Event.What = evKeyDown then
    begin
    if Text <> nil then
      C := HotKey(Text^)
    else
      C := #0;
    if  (C <> #0) and (
          (GetAltCode(C) = Event.KeyCode) or (
            (Owner^.Phase = phPostProcess) and (
              (UpCaseArray[Event.CharCode] = C) or
              (UpCaseArray[GetAltChar(Event.KeyCode and $FFFF00)] = C)
            )))
    then
      FocusLink
    end
  else if Event.What = evBroadcast then
    if  ( (Event.Command = cmReceivedFocus) or
          (Event.Command = cmReleasedFocus)) and
        (Link <> nil)
    then
      begin
      Light := Link^.State and sfFocused <> 0;
      DrawView;
      end;
  end { TLabel.HandleEvent };

procedure TLabel.Store(var S: TStream);
  begin
  TStaticText.Store(S);
  PutPeerViewPtr(S, Link);
  end;

{ THistoryViewer }

constructor THistoryViewer.Init(var Bounds: TRect; AHScrollBar,
    AVScrollBar: PScrollBar; AHistoryId: AWord);
  begin
  TListViewer.Init(Bounds, 1, AHScrollBar, AVScrollBar);
  HistoryId := AHistoryId;
  SearchPos := 0;
  SetRange(HistoryCount(AHistoryId));
  if Range > 1 then
    FocusItem(1);
  HScrollBar^.SetRange(1, HistoryWidth-Size.X+3);
  end;

function THistoryViewer.GetPalette: PPalette;
  const
    P: String[Length(CHistoryViewer)] = CHistoryViewer;
  begin
  GetPalette := @P;
  end;
function THistoryViewer.GetText(Item: LongInt; MaxLen: integer): String;
  var
    C: Char;
  begin
  FreeStr := HistoryStr(HistoryId, Item);
  FreeStr := CnvString(HistList.CurString);
  C := ' ';
  if FreeStr[Length(FreeStr)] = '+' then
    C := #254;
  Insert(C, FreeStr, 1);
  GetText := Copy(FreeStr, 1, Length(FreeStr)-1);
  end;

procedure THistoryViewer.HandleEvent(var Event: TEvent);

  {-$ WildCat $ -}
  var
    CurStr: String;
    Value: integer;
    T: Boolean;

  function Equal(const S1, S2: String; Count: word): Boolean;
    var
      I: word;
    begin
    Equal := False;
    if  (Length(S1) < Count) or (Length(S2) < Count) then
      exit;
    for I := 1 to Count do
      if UpCase(S1[I]) <> UpCase(S2[I]) then
        exit;
    Equal := True;
    end;

  function Search(const S1: String; SearchPos: integer;
       var Index: integer): Boolean;
    var
      L, H, I, C: integer;
    begin
    Search := False;
    L := Index;
    H := HistoryCount(HistoryId)-1;
    while L <= H do
      begin
      if Equal(HistoryStr(HistoryId, L), S1, SearchPos) then
        begin
        Search := True;
        break;
        end;
      Inc(L);
      end;
    if L > H then
      begin
      L := 0;
      H := Index-1;
      while L <= H do
        begin
        if Equal(HistoryStr(HistoryId, L), S1, SearchPos) then
          begin
          Search := True;
          break;
          end;
        Inc(L);
        end;
      end;
    Index := L;
    end { Search };
  {- $ WildCat $ -}

  {AK155 27-03-2003 по мотивам ScanMarked из TTHistList.HandleEvent}
  function SearchNonDeletableItem(D, Bound: integer): Boolean;
    var
      i: integer;
    begin
    Result := False;
    i := Focused;
    while True do
      begin
      Inc(i, D);
      if i = Bound then
        exit;
      HistoryStr(HistoryId, i);
      if HistList.CurString^[Length(HistList.CurString^)] <> ' ' then
        begin
        FocusItem(i);
        DrawView;
        Result := True;
        exit;
        end;
      end;
    end;
  {/AK155 27-03-2003}

  begin { THistoryViewer.HandleEvent }
  if  ( (Event.What = evBroadcast) and (Event.Command = cmReleasedFocus))
    or (Event.What = evMouseDown)
  then
    begin
    SearchPos := 0;
    HideCursor
    end
  else if Event.What = evKeyDown then
    case Event.KeyCode of
      kbCtrlPgDn, kbCtrlPgUp, kbPgDn, kbPgUp,
      kbEnd, kbHome, kbDown, kbUp:
        begin
        SearchPos := 0;
        HideCursor
        end;
      {AK155 27-03-2003}
      kbAltUp:
        begin
        SearchNonDeletableItem(-1, -1);
        exit;
        end;
      kbAltDown:
        begin
        SearchNonDeletableItem(1, Range);
        exit;
        end;
      {/AK155 27-03-2003}
    end {case};
  if  ( (Event.What = evMouseDown) and (Event.Double)) or
      ( (Event.What = evKeyDown) and (Event.KeyCode = kbEnter))
  then
    begin
    EndModal(cmOK);
    ClearEvent(Event);
    end
  else if ((Event.What = evKeyDown) and (Event.CharCode = ' ')
      and (Focused < HistoryCount(HistoryId)) and (SearchPos = 0))
  then
    begin
    FreeStr := HistoryStr(HistoryId, Focused);
    if CurString <> nil then
      begin
      if CurString^[Length(CurString^)] = '+' then
        CurString^[Length(CurString^)] := ' '
      else
        CurString^[Length(CurString^)] := '+';
      DrawView;
      ClearEvent(Event);
      end;
    end
  else if (Event.What = evKeyDown) and (Event.KeyCode = kbDel) then
    begin
    ClearEvent(Event);
    FreeStr := HistoryStr(HistoryId, Focused);
    if  (CurString <> nil) and (CurString^[Length(CurString^)] = '+')
    then
      begin
      if HistoryErrorBeep then
        begin
        SysBeepEx {PlaySound}(500, 110);
        end;
      {NoDelMark;}
      exit;
      end;
    DeleteHistoryStr(HistoryId, Focused);
    SetRange(HistoryCount(HistoryId));
    DrawView;
    end
  else if ((Event.What = evKeyDown) and (Event.KeyCode = kbESC)) or
      ( (Event.What = evCommand) and (Event.Command = cmCancel))
  then
    begin
    EndModal(cmCancel);
    ClearEvent(Event);
    end
  else if (Event.What = evKeyDown) and (Event.CharCode <> #0)
       and (Range > 0)
    and ((Event.KeyCode <> kbCtrlEnter) or (SearchPos > 0))
  then
    begin
    Value := Focused;
    if Value < Range then
      CurStr := Copy(GetText(Value, 255), 2, MaxStringLength)
    else
      CurStr := '';
    if Event.KeyCode = kbBack then
      begin
      SetLength(CurStr, SearchPos);
      T := False
      end
    else if Event.KeyCode = kbCtrlEnter then
      begin
      Inc(Value);
      if Value >= Range then
        Value := 0;
      T := Search(CurStr, SearchPos, Value);
      end
    else
      begin
      Inc(SearchPos);
      SetLength(CurStr, SearchPos);
      CurStr[SearchPos] := Event.CharCode;
      T := Search(CurStr, SearchPos, Value);
      end;
    ClearEvent(Event);
    ShowCursor;
    if T then
      FocusItem(Value)
    else if SearchPos > 0 then
      begin
      Dec(SearchPos);
      SetLength(CurStr, SearchPos);
      end;
    SetCursor(SearchPos+2, Cursor.Y);
    end
  else
    TListViewer.HandleEvent(Event);
  end { THistoryViewer.HandleEvent };

function THistoryViewer.HistoryWidth: integer;
  var
    Width, T, Count, I: integer;
  begin
  Width := 0;
  Count := HistoryCount(HistoryId);
  for I := 0 to Count-1 do
    begin
    T := Length(HistoryStr(HistoryId, I));
    if T > Width then
      Width := T;
    end;
  HistoryWidth := Width;
  end;

{ THistoryWindow }

constructor THistoryWindow.Init(var Bounds: TRect; HistoryId: AWord);
  begin
  TWindow.Init(Bounds, '', wnNoNumber);
  Flags := wfClose;
  InitViewer(HistoryId);
  end;

function THistoryWindow.GetPalette: PPalette;
  const
    P: String[Length(CHistoryWindow)] = CHistoryWindow;
  begin
  GetPalette := @P;
  end;

function THistoryWindow.GetSelection: String;
  begin
  GetSelection := Copy(Viewer^.GetText(Viewer^.Focused, 255), 2,
       MaxStringLength);
  end;

procedure THistoryWindow.InitViewer(HistoryId: AWord);
  var
    R: TRect;
  begin
  GetExtent(R);
  R.Grow(-1, -1);
  Viewer := New(PHistoryViewer, Init(R,
        StandardScrollBar(sbHorizontal+sbHandleKeyboard),
        StandardScrollBar(sbVertical+sbHandleKeyboard),
        HistoryId));
  Insert(Viewer);
  end;

{ THistory }

constructor THistory.Init(var Bounds: TRect; ALink: PInputline;
    AHistoryId: AWord);
  begin
  TView.Init(Bounds);
  Options := Options or ofPostProcess;
  EventMask := EventMask or evBroadcast;
  Link := ALink;
  HistoryId := AHistoryId;
  end;

constructor THistory.Load(var S: TStream);
  begin
  TView.Load(S);
  GetPeerViewPtr(S, Link);
  S.Read(HistoryId, SizeOf(AWord));
  end;

procedure THistory.Draw;
  var
    B: TDrawBuffer;
  begin
  MoveCStr(B, #222'~'#25'~'#221, GetColor($0102));
  WriteLine(0, 0, Size.X, Size.Y, B);
  end;

function THistory.GetPalette: PPalette;
  const
    P: String[Length(CHistory)] = CHistory;
  begin
  GetPalette := @P;
  end;

procedure THistory.HandleEvent(var Event: TEvent);
  var
    HistoryWindow: PHistoryWindow;
    R, P: TRect;
    C: word;
    Rslt: String;
  begin
  TView.HandleEvent(Event);
  if  (Event.What = evMouseDown) or
      ( (Event.What = evKeyDown) and ( {CtrlToArrow}(Event.KeyCode) =
         kbDown) and
        (Link^.State and sfFocused <> 0))
  then
    begin
    if not Link^.Focus then
      begin
      ClearEvent(Event);
      exit;
      end;
    RecordHistory(Link^.Data^);
    Link^.GetBounds(R);
    Dec(R.A.X);
    Inc(R.B.X);
    Inc(R.B.Y, 7);
    Dec(R.A.Y, 1);
    Owner^.GetExtent(P);
    R.Intersect(P);
    Dec(R.B.Y, 1);
    HistoryWindow := InitHistoryWindow(R);
    if HistoryWindow <> nil then
      begin
      C := Owner^.ExecView(HistoryWindow);
      if C = cmOK then
        begin
        Rslt := HistoryWindow^.GetSelection;
        if Length(Rslt) > Link^.MaxLen then
          SetLength(Rslt, Link^.MaxLen);
        Link^.SetData(Rslt);
        Link^.SelectAll(True);
        Link^.DrawView;
        end;
      Dispose(HistoryWindow, Done);
      end;
    ClearEvent(Event);
    end
  else if (Event.What = evBroadcast) then
    if  ( (Event.Command = cmReleasedFocus) and (Event.InfoPtr = Link))
      or (Event.Command = cmRecordHistory)
    then
      RecordHistory(Link^.Data^);
  end { THistory.HandleEvent };

function THistory.InitHistoryWindow(var Bounds: TRect): PHistoryWindow;
  var
    R, O: TRect;
    P: PHistoryWindow;
  begin
  {===START====  New code realisation by Shumskii Lev aka WildCat}
  Owner^.GetExtent(O);
  Link^.GetBounds(R);
  if  (O.B.Y-R.B.Y) > 4 then
    begin
    Inc(R.A.X);
    Inc(R.B.Y);
    Inc(R.B.Y, 7);
    Inc(R.A.Y, 1);
    Inc(R.B.X, 2);
    R.Intersect(O);
    if  (R.B.Y >= O.B.Y) then
      Dec(R.B.Y, 1);
    P := New(PHistoryWindow, Init(R, HistoryId));
    P^.HelpCtx := Link^.HelpCtx;
    InitHistoryWindow := P
    end
    {===END=== New code realisation by Shumskii Lev aka WildCat}
  else
    {=== Old code realisation START ===}
    begin
    P := New(PHistoryWindow, Init(Bounds, HistoryId));
    P^.HelpCtx := Link^.HelpCtx;
    InitHistoryWindow := P;
    end;
  {=== Old code realisation END ===}
  end { THistory.InitHistoryWindow };

procedure THistory.RecordHistory(const S: String);
  begin
  HistoryAdd(HistoryId, S);
  end;

procedure THistory.Store(var S: TStream);
  begin
  TView.Store(S);
  PutPeerViewPtr(S, Link);
  S.Write(HistoryId, SizeOf(AWord));
  end;

constructor THexLine.Init;
  begin
  if AInputLine = nil then
    exit;
  inherited Init(R);
  Options := Options or ofSelectable or ofPostProcess;
  EventMask := $FFFF;
  InputLine := AInputLine;
  end;

constructor THexLine.Load(var S: TStream);
  begin
  inherited Load(S);
  GetPeerViewPtr(S, InputLine);
  end;

procedure THexLine.Store(var S: TStream);
  begin
  inherited Store(S);
  PutPeerViewPtr(S, InputLine);
  end;

procedure THexLine.HandleEvent;
  procedure CE;
    begin
    ClearEvent(Event);
    end;
  procedure CED;
    begin
    CE;
    DrawView;
    InputLine^.DrawView
    end;

  var
    S: String;

  begin
  inherited HandleEvent(Event);
  case Event.What of
    evBroadcast:
      if  (Event.Command = cmUpdateHexViews) and
          (Event.InfoPtr = InputLine)
      then
        CED;
    evKeyDown:
      case Event.KeyCode of
        kbBack:
          begin
          if CurX >= 0 then
            if Sec
            then
              begin
              Delete(InputLine^.Data^, CurX+1, 1);
              Sec := False;
              end
            else
              begin
              Delete(InputLine^.Data^, CurX, 1);
              Dec(CurX);
              Sec := False;
              end;
          CED
          end;
        kbDel:
          begin
          if CurX < Length(InputLine^.Data^) then
            begin
            Delete(InputLine^.Data^, CurX+1, 1);
            Sec := False;
            end;
          CED
          end;
        kbLeft:
          begin
          if Sec
          then
            Sec := False
          else if CurX > 0 then
            begin
            Dec(CurX);
            Sec := True
            end;
          CED
          end;
        kbRight:
          begin
          if Sec and (CurX < Length(InputLine^.Data^)) then
            begin
            Inc(CurX);
            Sec := False
            end
          else
            Sec := True;
          CED
          end;
        kbUp:
          Event.KeyCode := kbShiftTab;
        kbTab, kbShiftTab, kbESC, kbEnter:
          ;
        else {case}
          case UpCase(Event.CharCode) of
            '0'..'9', 'A'..'F':
              begin
              if  (CurX = InputLine^.MaxLen-1) and Sec then
                begin
                CE;
                exit
                end;
              S := InputLine^.Data^;
              if CurX+1 > Length(S) then
                S := S+#0;
              if Sec then
                S[CurX+1] := Char((Byte(S[CurX+1]) and $F0) or
                           (Pos(UpCase(Event.CharCode),
                        '0123456789ABCDEF')-1))
              else
                S[CurX+1] := Char((Byte(S[CurX+1]) and $F) or
                           (Pos(UpCase(Event.CharCode),
                        '0123456789ABCDEF')-1) shl 4);
              InputLine^.Data^:= Copy(S, 1, InputLine^.MaxLen);
              InputLine^.DrawView;
              if Sec then
                begin
                Inc(CurX);
                Sec := False
                end
              else
                Sec := True;
              CED;
              end;
            else {case}
              if GetState(sfFocused) and (Event.CharCode > #0) then
                CE;
          end {case};
      end {case};
  end {case};
  end { THexLine.HandleEvent };

procedure THexLine.Draw;
  var
    B: TDrawBuffer;
    S: String;
    C: word;
  begin
  if CurX > Length(InputLine^.Data^) then
    CurX := Length(InputLine^.Data^);
  if CurX < 0 then
    CurX := 0;
  if CurX <> InputLine^.CurPos then
    if InputLine^.GetState(sfSelected) then
      begin
      CurX := InputLine^.CurPos;
      Sec := False
      end;
  if DeltaX >= Length(InputLine^.Data^) then
    DeltaX := integer(Length(InputLine^.Data^))-1;
  if DeltaX < 0 then
    DeltaX := 0;
  if CurX < DeltaX then
    DeltaX := CurX;
  if 3*(CurX-DeltaX) > Size.X-2 then
    DeltaX := CurX-(Size.X-2) div 3;
  if CurX <> InputLine^.CurPos then
    if not InputLine^.GetState(sfSelected) then
      begin
      InputLine^.CurPos := CurX;
      InputLine^.FirstPos := DeltaX;
      InputLine^.DrawView
      end;
  C := InputLine^.GetColor(1);
  MoveChar(B, ' ', C, Size.X);
  S := Copy(InputLine^.Data^, DeltaX+1, MaxStringLength);
  S := Copy(DumpStr(S[1], 0, 16, 0), 11, Length(S)*3);
  SetCursor(1+3*(CurX-DeltaX)+Byte(Sec), 0);
  ShowCursor;
  MoveStr(B[1], Copy(S, 1, Size.X-2), C);
  {-DataCompBoy: This is a temporary code!}
  MoveChar(B[Size.X-1], #32, InputLine^.GetColor(4), 1);
  MoveChar(B[0], #32, InputLine^.GetColor(4), 1);
  {-DataCompBoy}
  WriteLine(0, 0, Size.X, Size.Y, B);
  end { THexLine.Draw };

end.
