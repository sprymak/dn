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
  Lfn, {DataCompBoy}DnIni, Collect, Objects, Drivers, Views,
    advance6,
  Scroller, Validate, U_KeyMap;

const

  { Color palettes }

  CGrayDialog = #32#33#34#35#36#37#38#39#40#41#42#43#44#45#46#47+
  #48#49#50#51#52#53#54#55#56#57#58#59#60#61#62#63+
  #178#179;
  CRedDialog =
    #32#214#214#35#36#215#38#39#40#216#216#217#218#219#220#47+
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
    Constructor Init(var Bounds: TRect; const ATitle: TTitleStr);
    Constructor Load(var s: TStream);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Valid(Command: word): boolean; virtual;
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
    DrawShift: byte;
    CtrlK: boolean;
    Constructor Init(var Bounds: TRect; AMaxLen: AInt);
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    function DataSize: word; virtual;
    procedure Draw; virtual;
    procedure GetData(var Rec); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SelectAll(Enable: boolean);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: word; Enable: boolean); virtual;
    procedure SetValidator(AValid: PValidator);
    procedure Store(var s: TStream);
    function Valid(Command: word): boolean; virtual;
    function CanScroll(Delta: integer): boolean;
    end;

  PHexLine = ^THexLine;
  THexLine = object(TView)
    InputLine: PInputline;
    DeltaX, CurX: integer;
    Sec: boolean;
    Constructor Init(R: TRect; AInputLine: PInputline);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
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
    Flags: byte;
    AmDefault: boolean;
    Constructor Init(var Bounds: TRect; const ATitle: TTitleStr;
      ACommand: word;
    AFlags: word);
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    procedure DrawState(Down: boolean);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure MakeDefault(Enable: boolean);
    procedure Press; virtual;
    procedure SetState(AState: word; Enable: boolean); virtual;
    procedure Store(var s: TStream);
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
    Value: longInt;
    Sel: AInt;
    EnableMask: longInt;
    Strings: TStringCollection;
    Constructor Init(var Bounds: TRect; AStrings: PSItem);
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    function ButtonState(Item: integer): boolean;
    function DataSize: word; virtual;

    procedure DrawBox(const Icon: String; Marker: Char);
    procedure DrawMultiBox(const Icon, Marker: String);
    procedure GetData(var Rec); virtual;
    function GetHelpCtx: word; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Mark(Item: integer): boolean; virtual;
    function MultiMark(Item: integer): byte; virtual;
    procedure Press(Item: integer); virtual;
    procedure MovedTo(Item: integer); virtual;
    procedure SetButtonState(AMask: longInt; Enable: boolean);
    procedure SetData(var Rec); virtual;
    procedure SetState(AState: word; Enable: boolean); virtual;
    procedure Store(var s: TStream);
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
    function Mark(Item: integer): boolean; virtual;
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
    function Mark(Item: integer): boolean; virtual;
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
    SelRange: byte;
    Flags: AWord;
    States: PString;
    Constructor Init(var Bounds: TRect; AStrings: PSItem;
    ASelRange: byte; AFlags: word; const AStates: String);
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    function DataSize: word; virtual;
    procedure Draw; virtual;
    procedure GetData(var Rec); virtual;
    function MultiMark(Item: integer): byte; virtual;
    procedure Press(Item: integer); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var s: TStream);
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
    Constructor Init(var Bounds: TRect; ANumCols: word;
    AScrollBar: PScrollBar);
    Constructor Load(var s: TStream);
    function DataSize: word; virtual;
    procedure GetData(var Rec); virtual;
    function GetText(Item: longInt; MaxLen: integer): String;
      virtual;
    procedure NewLisT(AList: PCollection); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var s: TStream);
    end;

  { TStaticText }

  { Palette layout }
  { 1 = Text }

  PStaticText = ^TStaticText;
  TStaticText = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    text: PString;
    Constructor Init(var Bounds: TRect; const AText: String);
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure GetText(var s: String); virtual;
    procedure Store(var s: TStream);
    end;

  { TParamText }

  { Palette layout }
  { 1 = Text }

  PParamText = ^TParamText;
  TParamText = object(TStaticText)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    ParamCount: AInt;
    ParamList: Pointer;
    Constructor Init(var Bounds: TRect; const AText: String;
    AParamCount: AInt);
    Constructor Load(var s: TStream);
    function DataSize: word; virtual;
    procedure GetText(var s: String); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var s: TStream);
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
    Light: boolean;
    Constructor Init(var Bounds: TRect; const AText: String; ALink:
      PView);
    Constructor Load(var s: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Store(var s: TStream);
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
    Constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
      PScrollBar;
    AHistoryId: AWord);
    function GetPalette: PPalette; virtual;
    function GetText(Item: longInt; MaxLen: integer): String;
      virtual;
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
    Constructor Init(var Bounds: TRect; HistoryId: AWord);
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
    Constructor Init(var Bounds: TRect; ALink: PInputline;
      AHistoryId: AWord);
    Constructor Load(var s: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function InitHistoryWindow(var Bounds: TRect): PHistoryWindow;
      virtual;
    procedure RecordHistory(const s: String); virtual;
    procedure Store(var s: TStream);
    end;

  { SItem routines }

function NewSItem(const Str: String; ANext: PSItem): PSItem;

implementation

uses
  ASCIITab, HistList, advance, advance1, advance2, advance3,
    Commands,
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

function IsBlank(Ch: Char): boolean;
  begin
    IsBlank := (Ch = ' ') or (Ch = #13) or (Ch = #10);
  end;

{ TDialog }

Constructor TDialog.Init(var Bounds: TRect; const ATitle: TTitleStr);
  begin
    inherited Init(Bounds, ATitle, wnNoNumber);
    Options := Options or ofVersion20;
    GrowMode := 0;
    Flags := wfMove+wfClose;
    Palette := dpGrayDialog;
  end;

Constructor TDialog.Load(var s: TStream);
  begin
    inherited Load(s);
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

function TDialog.Valid(Command: word): boolean;
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

Constructor TInputLine.Init(var Bounds: TRect; AMaxLen: AInt);
  begin
    TView.Init(Bounds);
    State := State or sfCursorVis;
    if (InterfaceData.Options and ouiBlockInsertCursor <> 0) then
        BlockCursor;
    if (Bounds.A.X <> Bounds.B.X) and (Bounds.A.Y <> Bounds.B.Y)
    then
      Options := Options or (ofSelectable+ofFirstClick+ofVersion20)
    else
      Options := Options or (ofFirstClick+ofVersion20) and not
        ofSelectable;
    GetMem(Data, AMaxLen+1);
    Data^:= '';
    MaxLen := AMaxLen;
    DrawShift := 1;
    CtrlK := False;
  end;

Constructor TInputLine.Load(var s: TStream);
  begin
    TView.Load(s);
    DrawShift := 1;
    {Cat:warn AnsiString}
    s.Read(MaxLen, SizeOf(AInt)*5);
    GetMem(Data, MaxLen+1);
    s.ReadStrV(Data^);
      {S.Read(Data^[0], 1); S.Read(Data^[1], Length(Data^));}
    if Options and ofVersion >= ofVersion20 then
      Validator := PValidator(s.Get);
    Options := Options or ofVersion20;
    SetState(sfCursorIns, (InterfaceData.Options and
      ouiBlockInsertCursor <> 0));
  end;

destructor TInputLine.Done;
  begin
    FreeMem(Data, MaxLen+1);
    Data := nil;
    SetValidator(nil);
    TView.Done;
  end;

function TInputLine.CanScroll(Delta: integer): boolean;
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
    Color: byte;
    l, R: integer;
    B: TDrawBuffer;
    s: String;
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
    s := Copy(Data^, FirstPos+1, Size.X-2*DrawShift);
    MoveStr(B[DrawShift], s, Color);
    if Options and ofSecurity <> 0 then
      MoveChar(B[DrawShift], #254, Color, Length(s));
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
        if (DrawShift > 0) and CanScroll(-1) then
          MoveChar(B[0], #17, GetColor(4), 1);
        l := SelStart-FirstPos;
        R := SelEnd-FirstPos;
        if l < 0 then
          l := 0;
        if R > Size.X-2*DrawShift then
          R := Size.X-2*DrawShift;
        if l < R then
          MoveChar(B[l+1], #0, GetColor(3), R-l);
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
    Delta, Anchor, i: integer;
    OldScanCode: byte;
    ExtendBlock: boolean;
    OldData: String;
    OldCurPos, OldFirstPos,
    OldSelStart, OldSelEnd: integer;
    s: String;
    PS: PString;
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

  function CheckValid(NoAutoFill: boolean): boolean;
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
              if (CurPos >= OldLen) and (Length(Data^) > OldLen)
              then
                CurPos := Length(Data^);
              CheckValid := True;
            end;
        end
      else
        CheckValid := True;
    end { CheckValid };

  procedure InsChar(C: Char);
    begin
      if (State and sfCursorIns <> 0) xor
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
              Insert(C, Data^, CurPos);
            end;
          CheckValid(False);
        end;
      Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
    end { InsChar };

  procedure ASCIITable;
    var
      P: PWindow;
      W: word;
      E: TEvent;
      R: TRect;
      CR: PView;
    function GetCH: boolean;
      begin
        W := Desktop^.ExecView(P);
        P^.GetData(charASCII);
        if W in [cmOK, cmYes] then
          begin
            InsChar(charASCII);
            DrawView;
          end;
        GetCH := W = cmYes;
      end;
    begin
      P := New(PASCIIChart, Init(R));
      P^.MoveTo(boundsASCII.X, boundsASCII.Y);
      P^.SetData(charASCII);
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
                  until not MouseEvent(Event, evMouseMove+
                    evMouseAuto);
                end;
              ClearEvent(Event);
            end;
          evKeyDown:
            begin
              SaveState;
              {Event.KeyCode := CtrlToArrow(Event.KeyCode);}
              if (Event.ScanCode in PadKeys) and
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
                          for i := SelStart+1 to SelEnd do
                            Data^[i] := UpCase(Data^[i]);
                        if CurPos = SelEnd then
                          Anchor := SelStart
                        else
                          Anchor := SelEnd;
                        ExtendBlock := True
                      end;
                    kbCtrlSqBracketR:
                      begin
                        if SelStart < SelEnd then
                          for i := SelStart+1 to SelEnd do
                            Data^[i] := LowCase(Data^[i]);
                        if CurPos = SelEnd then
                          Anchor := SelStart
                        else
                          Anchor := SelEnd;
                        ExtendBlock := True
                      end;
                    kbCtrlSlash,
                    kbCtrlBSlash:
                      begin
                        s := CapStrg(Copy(Data^, SelStart+1, SelEnd-
                          SelStart));
                        Delete(Data^, SelStart+1, SelEnd-SelStart);
                        Insert(s, Data^, SelStart+1);
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
                      Data^:= Copy(Data^, 1, SelStart)+OldData+Copy(
                        Data^, SelEnd+1, MaxStringLength);
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
                        i := CurPos;
                        while (not (Data^[i] in BreakChars)) and (i > 1)
                        do
                          Dec(i);
                        repeat
                          Data^[i] := UpCase(Data^[i]);
                          Inc(i);
                        until (Data^[i] in BreakChars) or (i >
                          Length(Data^));
                      end;
                  kbAltCtrlSqBracketR,
                  kbCtrlShiftSqBracketR,
                  kbCtrlSqBracketR:
                    if ShiftState and (kbLeftShift+kbRightShift) <> 0
                    then
                      LowStr(Data^)
                    else
                      begin
                        i := CurPos;
                        while (not (Data^[i] in BreakChars)) and (i > 1)
                        do
                          Dec(i);
                        repeat
                          Data^[i] := LowCase(Data^[i]);
                          Inc(i);
                        until (Data^[i] in BreakChars) or (i >
                          Length(Data^));
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
                        i := CurPos;
                        while (not (Data^[i] in BreakChars)) and (i > 1)
                        do
                          Dec(i);
                        if (Data^[i] in BreakChars) then
                          Inc(i);
                        Data^[i] := UpCase(Data^[i]);
                        Inc(i);
                        repeat
                          Data^[i] := LowCase(Data^[i]);
                          Inc(i);
                        until (Data^[i] in BreakChars) or (i >
                          Length(Data^));
                      end;
                  kbCtrlBack:
                    if CurPos > 0 then
                      begin
                        if (Data^[CurPos] in BreakChars) then
                          repeat
                            Delete(Data^, CurPos, 1);
                            Dec(CurPos)
                          until (CurPos = 0) or not (Data^[CurPos] in
                            BreakChars)
                        else
                          repeat
                            Delete(Data^, CurPos, 1);
                            Dec(CurPos)
                          until (CurPos = 0) or (Data^[CurPos] in
                            BreakChars)
                      end;
                  kbCtrlIns:
                    if Options and ofSecurity = 0 then
                      begin
                        s := Copy(Data^, SelStart+1, SelEnd-SelStart);
                        if s <> '' then
                          Message(Application, evCommand,
                            cmPutInClipboard, @S);
                        ClearEvent(Event);
                        exit;
                      end;
                  kbCtrlLeft:
                    begin
                      if CurPos > 0 then
                        repeat
                          Dec(CurPos)
                        until (CurPos = 0) or not (Data^[CurPos+1] in
                          BreakChars);
                      while (CurPos > 0) and not (Data^[CurPos] in
                          BreakChars)
                      do
                        Dec(CurPos)
                    end;
                  kbCtrlRight:
                    begin
                      if CurPos < Length(Data^) then
                        repeat
                          Inc(CurPos)
                        until (CurPos >= Length(Data^)) or (Data^[
                          CurPos+1] in BreakChars);
                      if CurPos < Length(Data^) then
                        repeat
                          Inc(CurPos)
                        until (CurPos >= Length(Data^)) or not (Data^[
                          CurPos+1] in BreakChars);
                    end;
                  kbShiftDel:
                    begin
                      s := Copy(Data^, SelStart+1, SelEnd-SelStart);
                      if s <> '' then
                        Message(Application, evCommand,
                          cmPutInClipboard, @S);
                      DeleteSelect;
                      SelEnd := SelStart;
                      DrawView;
                      ClearEvent(Event);
                      Message(Owner, evBroadcast, cmUpdateHexViews,
                        @Self);
                      exit;
                    end;
                  kbShiftIns:
                    begin
                      ClearEvent(Event);
                      s := '';
                      DeleteSelect;
                      Message(Application, evCommand,
                        cmGetFromClipboard, @S);
                      if s = '' then
                        exit;
                      SelStart := CurPos;
                      SelEnd := CurPos+Length(s);
                      OldData := Data^;
                      Insert(s, OldData, CurPos+1);
                      Data^:= Copy(OldData, 1, MaxLen);
                      DrawView;
                      Message(Owner, evBroadcast, cmUpdateHexViews,
                        @Self);
                      exit;
                    end;
                  kbAlt1, kbAlt2, kbAlt3, kbAlt4, kbAlt5, kbAlt6,
                    kbAlt7, kbAlt8, kbAlt9:
                    begin
                      ClearEvent(Event);
                      s := '';
                      DeleteSelect;
                      PS := DirsToChange[Event.ScanCode-Hi(kbAlt1)];
                      if (PS = nil) or (Length(PS^) = 0) then
                        exit
                      else
                        s := PS^;
                      if s = '' then
                        exit;
                      SelStart := CurPos;
                      SelEnd := CurPos+Length(s);
                      OldData := Data^;
                      Insert(s, OldData, CurPos+1);
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
                  else
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
                      else
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
              i := CurPos-Size.X+2*DrawShift;
              if FirstPos < i then
                FirstPos := i;
              Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
              DrawView;
              ClearEvent(Event);
            end;
        end {case};
      end;
  end { TInputLine.HandleEvent };

procedure TInputLine.SelectAll(Enable: boolean);
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
    if (Validator = nil) or
      (Validator^.Transfer(Data^, @Rec, vtSetData) = 0)
    then
      Move(Rec, Data^ {[0]}, DataSize);
    Message(Owner, evBroadcast, cmUpdateHexViews, @Self);
    SelectAll(True);
  end;

procedure TInputLine.SetState(AState: word; Enable: boolean);
  begin
    TView.SetState(AState, Enable);
    if Data = nil then
      exit;
    if (AState = sfSelected) or ((AState = sfActive) and
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

procedure TInputLine.Store(var s: TStream);
  begin
    TView.Store(s);
    {Cat:warn AnsiString}
    s.Write(MaxLen, SizeOf(AInt)*5);
    s.WriteStr(Data);
    s.Put(Validator);
  end;

function TInputLine.Valid(Command: word): boolean;
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

Constructor TButton.Init(var Bounds: TRect; const ATitle: TTitleStr;
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

Constructor TButton.Load(var s: TStream);
  begin
    TView.Load(s);
    Title := s.ReadStr;
    s.Read(Command, SizeOf(AWord)+SizeOf(byte)+SizeOf(boolean));
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

procedure TButton.DrawState(Down: boolean);
  var
    CButton, CShadow: word;
    Ch: Char;
    i, s, Y, t: integer;
    B: TDrawBuffer;

  procedure DrawTitle;
    var
      l, SCOff: integer;
    begin
      if Flags and bfLeftJust <> 0 then
        l := 1
      else
        begin
          l := (s-CStrLen(Title^)-1) div 2;
          if l < 1 then
            l := 1;
        end;
      MoveCStr(B[i+l], Title^, CButton);
      if ShowMarkers and not Down then
        begin
          if State and sfSelected <> 0 then
            SCOff := 0
          else if AmDefault then
            SCOff := 2
          else
            SCOff := 4;
          WordRec(B[0]).Lo := byte(SpecialChars[SCOff]);
          WordRec(B[s]).Lo := byte(SpecialChars[SCOff+1]);
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
    s := Size.X-1;
    t := Size.Y div 2-1;
    for Y := 0 to Size.Y-2 do
      begin
        MoveChar(B, ' ', byte(CButton), Size.X);
        WordRec(B[0]).Hi := CShadow;
        if Down then
          begin
            WordRec(B[1]).Hi := CShadow;
            Ch := ' ';
            i := 2;
          end
        else
          begin
            WordRec(B[s]).Hi := byte(CShadow);
            if ShowMarkers then
              Ch := ' '
            else
              begin
                if Y = 0 then
                  WordRec(B[s]).Lo := byte(#220)
                else
                  WordRec(B[s]).Lo := byte(#219);
                Ch := #223;
              end;
            i := 1;
          end;
        if (Y = t) and (Title <> nil) then
          DrawTitle;
        if ShowMarkers
        then
          if not Down
          then
            begin
              WordRec(B[1]).Lo := byte('[');
              WordRec(B[s-1]).Lo := byte(']');
            end
          else
        else if (State and sfSelected <> 0) or (AmDefault) then
          if not Down then
            begin
              WordRec(B[1]).Lo := byte('');
              WordRec(B[s-1]).Lo := byte('');
            end
          else
            begin
              WordRec(B[2]).Lo := byte('');
              WordRec(B[s]).Lo := byte('');
            end;

        WriteLine(0, Y, Size.X, 1, B);
      end;
    MoveChar(B[0], ' ', byte(CShadow), 2);
    MoveChar(B[2], Ch, byte(CShadow), s-1);
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
    Down: boolean;
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
        if ((Event.KeyCode = kbRight) or (Event.KeyCode = kbDown))
            and
          (State and sfSelected <> 0)
        then
          begin
            Event.KeyCode := kbTab;
            TView.HandleEvent(Event);
          end
        else if ((Event.KeyCode = kbLeft) or (Event.KeyCode = kbUp))
            and
          (State and sfSelected <> 0)
        then
          begin
            Event.KeyCode := kbShiftTab;
            TView.HandleEvent(Event);
          end
        else
          begin
            C := HotKey(Title^);
            if (C <> #0) and (
              (GetAltCode(C) = Event.KeyCode) or (
              (Owner^.Phase = phPostProcess) and (
              (UpCaseArray[Event.CharCode] = C) or
              (UpCaseArray[GetAltChar(Event.KeyCode and $FFFF00)] = C)
              ))) or
              (State and sfSelected <> 0) and (Event.CharCode = ' ') or
              (State and sfSelected <> 0) and (Event.KeyCode =
                kbEnter)
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

procedure TButton.MakeDefault(Enable: boolean);
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

procedure TButton.SetState(AState: word; Enable: boolean);
  begin
    TView.SetState(AState, Enable);
    if AState and (sfSelected+sfActive) <> 0 then
      DrawView;
    if AState and sfFocused <> 0 then
      MakeDefault(Enable);
  end;

procedure TButton.Store(var s: TStream);
  begin
    TView.Store(s);
    s.WriteStr(Title);
    s.Write(Command, SizeOf(AWord)+SizeOf(byte)+SizeOf(boolean));
  end;

{ TCluster }

Constructor TCluster.Init(var Bounds: TRect; AStrings: PSItem);
  var
    i: integer;
    P: PSItem;
  begin
    TView.Init(Bounds);
    Options := Options or (ofSelectable+ofFirstClick+ofPreProcess+
    ofPostProcess+ofVersion20);
    i := 0;
    P := AStrings;
    while P <> nil do
      begin
        Inc(i);
        P := P^.Next;
      end;
    Strings.Init(i, 0, False);
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

Constructor TCluster.Load(var s: TStream);
  begin
    TView.Load(s);
    if (Options and ofVersion) >= ofVersion20 then
      begin
        {Cat:warn глюки при смене порядка полей}
        s.Read(Value, SizeOf(longInt)*2+SizeOf(AInt));
      end
    else
      begin
        s.Read(Value, SizeOf(AWord));
        s.Read(Sel, SizeOf(AInt));
        EnableMask := $FFFFFFFF;
        Options := Options or ofVersion20;
      end;
    Strings.Load(s);
    SetButtonState(0, True);
  end;

destructor TCluster.Done;
  begin
    Strings.Done;
    TView.Done;
  end;

function TCluster.ButtonState(Item: integer): boolean;
  var
    Q: longInt;
  begin
    ButtonState := False;
    if Item > 31 then
      exit;
    Q := 1 shl Item;
    ButtonState := (EnableMask and Q <> 0);
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
    i, j, Cur, Col: integer;
    CNorm, CSel, CDis, Color: word;
    B: TDrawBuffer;
    SCOff: byte;
  begin
    CNorm := GetColor($0301);
    CSel := GetColor($0402);
    CDis := GetColor($0505);
    for i := 0 to Size.Y do
      begin
        MoveChar(B, ' ', byte(CNorm), Size.X);
        for j := 0 to(Strings.Count-1) div Size.Y+1 do
          begin
            Cur := j*Size.Y+i;
            if Cur < Strings.Count then
              begin
                Col := Column(Cur);
                if (Col+CStrLen(CnvString(Strings.At(Cur)))+5 <
                  SizeOf(TDrawBuffer) div SizeOf(word)) and (Col <
                    Size.X)
                then
                  begin
                    if not ButtonState(Cur) then
                      Color := CDis
                    else if (Cur = Sel) and (State and sfSelected <> 0)
                    then
                      Color := CSel
                    else
                      Color := CNorm;
                    MoveChar(B[Col], ' ', byte(Color), Size.X-Col);
                    MoveStr(B[Col], Icon, byte(Color));
                    WordRec(B[Col+2]).Lo := byte(Marker[MultiMark(
                      Cur)+1]);
                    MoveCStr(B[Col+5], CnvString(Strings.At(Cur)),
                      Color);
                    if ShowMarkers and (State and sfSelected <> 0)
                        and (Cur = Sel)
                    then
                      begin
                        WordRec(B[Col]).Lo := byte(SpecialChars[0]);
                        WordRec(B[Column(Cur+Size.Y)-1]).Lo := byte(
                          SpecialChars[1]);
                      end;
                  end;
              end;
          end;
        WriteBuf(0, i, Size.X, 1, B);
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
    i, s: integer;
    C: Char;

  procedure MoveSel;
    begin
      if i <= Strings.Count then
        begin
          Sel := s;
          MovedTo(Sel);
          DrawView;
        end;
    end;

  begin
    TView.HandleEvent(Event);
    if (Options and ofSelectable) = 0 then
      exit;
    if Event.What = evMouseDown then
      begin
        MakeLocal(Event.Where, Mouse);
        i := FindSel(Mouse);
        if i <> -1 then
          if ButtonState(i) then
            Sel := i;
        DrawView;
        repeat
          MakeLocal(Event.Where, Mouse);
          if FindSel(Mouse) = Sel then
            ShowCursor
          else
            HideCursor;
        until not MouseEvent(Event, evMouseMove);
          {Wait for mouse up}
        ShowCursor;
        MakeLocal(Event.Where, Mouse);
        if (FindSel(Mouse) = Sel) and ButtonState(Sel) then
          begin
            Press(Sel);
            DrawView;
          end;
        ClearEvent(Event);
      end
    else if Event.What = evKeyDown then
      begin
        s := Sel;
        case {CtrlToArrow}(Event.KeyCode) of
          kbUp:
            if State and sfFocused <> 0 then
              begin
                i := 0;
                repeat
                  Inc(i);
                  Dec(s);
                  if s < 0 then
                    s := Strings.Count-1;
                until ButtonState(s) or (i > Strings.Count);
                MoveSel;
                ClearEvent(Event);
              end;
          kbDown:
            if State and sfFocused <> 0 then
              begin
                i := 0;
                repeat
                  Inc(i);
                  Inc(s);
                  if s >= Strings.Count then
                    s := 0;
                until ButtonState(s) or (i > Strings.Count);
                MoveSel;
                ClearEvent(Event);
              end;
          kbRight:
            if State and sfFocused <> 0 then
              begin
                i := 0;
                repeat
                  Inc(i);
                  Inc(s, Size.Y);
                  if s >= Strings.Count then
                    begin
                      s := (s+1) mod Size.Y;
                      if s >= Strings.Count then
                        s := 0;
                    end;
                until ButtonState(s) or (i > Strings.Count);
                MoveSel;
                ClearEvent(Event);
              end;
          kbLeft:
            if State and sfFocused <> 0 then
              begin
                i := 0;
                repeat
                  Inc(i);
                  if s > 0 then
                    begin
                      Dec(s, Size.Y);
                      if s < 0 then
                        begin
                          s := ((Strings.Count+Size.Y-1) div Size.Y)*
                            Size.Y+s-1;
                          if s >= Strings.Count then
                            s := Strings.Count-1;
                        end;
                    end
                  else
                    s := Strings.Count-1;
                until ButtonState(s) or (i > Strings.Count);
                MoveSel;
                ClearEvent(Event);
              end;
          else
            begin
              for i := 0 to Strings.Count-1 do
                begin
                  C := HotKey(CnvString(Strings.At(i)));
                  if (C <> #0) and (
                    (GetAltCode(C) = Event.KeyCode) or (
                    (Owner^.Phase = phPostProcess) and (
                    (UpCaseArray[Event.CharCode] = C) or
                    (UpCaseArray[GetAltChar(Event.KeyCode and
                      $FFFF00)] = C)
                    )))
                  then
                    begin
                      if ButtonState(i) then
                        begin
                          if Focus then
                            begin
                              Sel := i;
                              MovedTo(Sel);
                              Press(Sel);
                              DrawView;
                            end;
                          ClearEvent(Event);
                        end;
                      exit;
                    end;
                end;
              if (Event.CharCode = ' ') and (State and sfFocused <> 0)
              then
                begin
                  Press(Sel);
                  DrawView;
                  ClearEvent(Event);
                end;
            end
        end
      end;
  end { TCluster.HandleEvent };

procedure TCluster.SetButtonState(AMask: longInt; Enable: boolean);
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

procedure TCluster.SetState(AState: word; Enable: boolean);
  begin
    TView.SetState(AState, Enable);
    if AState = sfSelected then
      DrawView;
  end;

function TCluster.Mark(Item: integer): boolean;
  begin
    Mark := False;
  end;

function TCluster.MultiMark(Item: integer): byte;
  begin
    MultiMark := byte(Mark(Item) = True);
  end;

procedure TCluster.MovedTo(Item: integer);
  begin
  end;

procedure TCluster.Press(Item: integer);
  begin
  end;

procedure TCluster.Store(var s: TStream);
  begin
    TView.Store(s);
    {Cat:warn глюки при смене порядка полей}
    s.Write(Value, SizeOf(Value)+SizeOf(Sel)+SizeOf(EnableMask));
    Strings.Store(s);
  end;

function TCluster.Column(Item: integer): integer;
  var
    i, Col, Width, l: integer;
  begin
    if Item < Size.Y then
      Column := 0
    else
      begin
        Width := 0;
        Col := -6;
        for i := 0 to Item do
          begin
            if i mod Size.Y = 0 then
              begin
                Inc(Col, Width+6);
                Width := 0;
              end;
            if i < Strings.Count then
              l := CStrLen(CnvString(Strings.At(i)));
            if l > Width then
              Width := l;
          end;
        Column := Col;
      end;
  end { TCluster.Column };

function TCluster.FindSel(P: TPoint): integer;
  var
    i, s: integer;
    R: TRect;
  begin
    GetExtent(R);
    if not R.Contains(P) then
      FindSel := -1
    else
      begin
        i := 0;
        while P.X >= Column(i+Size.Y) do
          Inc(i, Size.Y);
        s := i+P.Y;
        if s >= Strings.Count then
          FindSel := -1
        else
          FindSel := s;
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

function TRadioButtons.Mark(Item: integer): boolean;
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

function TCheckBoxes.Mark(Item: integer): boolean;
  begin
    Mark := Value and (1 shl Item) <> 0;
  end;

procedure TCheckBoxes.Press(Item: integer);
  begin
    Value := (Value xor (1 shl Item)) and $FFFF;
  end;

{ TMultiCheckBoxes }

Constructor TMultiCheckBoxes.Init(var Bounds: TRect; AStrings:
    PSItem;
  ASelRange: byte; AFlags: word; const AStates: String);
  begin
    inherited Init(Bounds, AStrings);
    SelRange := ASelRange;
    Flags := AFlags;
    States := NewStr(AStates);
  end;

Constructor TMultiCheckBoxes.Load(var s: TStream);
  begin
    TCluster.Load(s);
    s.Read(SelRange, SizeOf(byte));
    s.Read(Flags, SizeOf(AWord));
    States := s.ReadStr;
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
    DataSize := SizeOf(longInt);
  end;

function TMultiCheckBoxes.MultiMark(Item: integer): byte;
  begin
    MultiMark := longInt((Value and (WordRec(Flags).Lo
    shl (Item*WordRec(Flags).Hi))) shr (Item*WordRec(Flags).Hi));
  end;

procedure TMultiCheckBoxes.GetData(var Rec);
  begin
    longInt(Rec) := Value;
    DrawView;
  end;

procedure TMultiCheckBoxes.Press(Item: integer);
  var
    CurState: shortint;
  begin
    CurState := longInt((Value and (WordRec(Flags).Lo
    shl (Item*WordRec(Flags).Hi))) shr (Item*WordRec(Flags).Hi));
    Dec(CurState);
    if (CurState >= SelRange) or (CurState < 0) then
      CurState := SelRange-1;
    Value := longInt((Value and not (WordRec(Flags).Lo
    shl (Item*WordRec(Flags).Hi)) or
    (CurState shl (Item*WordRec(Flags).Hi))));
  end;

procedure TMultiCheckBoxes.SetData(var Rec);
  begin
    Value := longInt(Rec);
    DrawView;
  end;

procedure TMultiCheckBoxes.Store(var s: TStream);
  begin
    TCluster.Store(s);
    s.Write(SelRange, SizeOf(byte));
    s.Write(Flags, SizeOf(AWord));
    s.WriteStr(States);
  end;

{ TListBox }

type
  TListBoxRec = record
    List: PCollection;
    Selection: word;
    end;

Constructor TListBox.Init(var Bounds: TRect; ANumCols: word;
  AScrollBar: PScrollBar);
  var
    ARange: integer;
  begin
    TListViewer.Init(Bounds, ANumCols, nil, AScrollBar);
    List := nil;
    SetRange(0);
  end;

Constructor TListBox.Load(var s: TStream);
  begin
    TListViewer.Load(s);
    List := PCollection(s.Get);
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

function TListBox.GetText(Item: longInt; MaxLen: integer): String;
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

procedure TListBox.Store(var s: TStream);
  begin
    TListViewer.Store(s);
    s.Put(List);
  end;

{ TStaticText }

Constructor TStaticText.Init(var Bounds: TRect; const AText: String);
  begin
    TView.Init(Bounds);
    text := NewStr(AText);
  end;

Constructor TStaticText.Load(var s: TStream);
  begin
    TView.Load(s);
    text := s.ReadStr;
  end;

destructor TStaticText.Done;
  begin
    DisposeStr(text);
    TView.Done;
  end;

procedure TStaticText.Draw;
  var
    Color: byte;
    Center: boolean;
    i, j, l, P, Y: integer;
    B: TDrawBuffer;
    s: String;
  begin
    Color := GetColor(1);
    GetText(s);
    l := Length(s);
    P := 1;
    Y := 0;
    Center := False;
    while Y < Size.Y do
      begin
        MoveChar(B, ' ', Color, Size.X);
        if P <= l then
          begin
            if s[P] = #3 then
              begin
                Center := True;
                Inc(P);
              end;
            i := P;
            repeat
              j := P;
              while (P <= l) and (s[P] = ' ') do
                Inc(P);
              while (P <= l) and (s[P] <> ' ') and (s[P] <> #13) do
                  Inc(P);
            until (P > l) or (P >= i+Size.X) or (s[P] = #13);
            if P > i+Size.X then
              if j > i then
                P := j
              else
                P := i+Size.X;
            if Center then
              j := (Size.X-P+i) div 2
            else
              j := 0;
            MoveBuf(B[j], s[i], Color, P-i);
            while (P <= l) and (s[P] = ' ') do
              Inc(P);
            if (P <= l) and (s[P] = #13) then
              begin
                Center := False;
                Inc(P);
                if (P <= l) and (s[P] = #10) then
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

procedure TStaticText.GetText(var s: String);
  begin
    if text <> nil then
      s := text^
    else
      s := '';
  end;

procedure TStaticText.Store(var s: TStream);
  begin
    TView.Store(s);
    s.WriteStr(text);
  end;

{ TParamText }

Constructor TParamText.Init(var Bounds: TRect; const AText: String;
  AParamCount: AInt);
  begin
    TStaticText.Init(Bounds, AText);
    ParamCount := AParamCount;
  end;

Constructor TParamText.Load(var s: TStream);
  begin
    TStaticText.Load(s);
    s.Read(ParamCount, SizeOf(AInt));
  end;

function TParamText.DataSize: word;
  begin
    DataSize := ParamCount*SizeOf(longInt);
  end;

procedure TParamText.GetText(var s: String);
  begin
    if text <> nil then
      FormatStr(s, text^, ParamList^)
    else
      s := '';
  end;

procedure TParamText.SetData(var Rec);
  begin
    ParamList := @Rec;
    DrawView;
  end;

procedure TParamText.Store(var s: TStream);
  begin
    TStaticText.Store(s);
    s.Write(ParamCount, SizeOf(AInt));
  end;

{ TLabel }

Constructor TLabel.Init(var Bounds: TRect; const AText: String;
    ALink: PView);
  begin
    TStaticText.Init(Bounds, AText);
    Link := ALink;
    Options := Options or (ofPreProcess+ofPostProcess);
    EventMask := EventMask or evBroadcast;
  end;

Constructor TLabel.Load(var s: TStream);
  begin
    TStaticText.Load(s);
    GetPeerViewPtr(s, Link);
  end;

procedure TLabel.Draw;
  var
    Color: word;
    B: TDrawBuffer;
    SCOff: byte;
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
    MoveChar(B[0], ' ', byte(Color), Size.X);
    if text <> nil then
      MoveCStr(B[1], text^, Color);
    if ShowMarkers then
      WordRec(B[0]).Lo := byte(SpecialChars[SCOff]);
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
      if (Link <> nil) and (Link^.Options and ofSelectable <> 0)
      then
        Link^.Focus;
      ClearEvent(Event);
    end;

  begin
    TStaticText.HandleEvent(Event);
    if Event.What = evMouseDown then
      FocusLink
    else if Event.What = evKeyDown then
      begin
        if text <> nil then
          C := HotKey(text^)
        else
          C := #0;
        if (C <> #0) and (
          (GetAltCode(C) = Event.KeyCode) or (
          (Owner^.Phase = phPostProcess) and (
          (UpCaseArray[Event.CharCode] = C) or
          (UpCaseArray[GetAltChar(Event.KeyCode and $FFFF00)] = C)
          )))
        then
          FocusLink
      end
    else if Event.What = evBroadcast then
      if ((Event.Command = cmReceivedFocus) or
        (Event.Command = cmReleasedFocus)) and
        (Link <> nil)
      then
        begin
          Light := Link^.State and sfFocused <> 0;
          DrawView;
        end;
  end { TLabel.HandleEvent };

procedure TLabel.Store(var s: TStream);
  begin
    TStaticText.Store(s);
    PutPeerViewPtr(s, Link);
  end;

{ THistoryViewer }

Constructor THistoryViewer.Init(var Bounds: TRect; AHScrollBar,
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
function THistoryViewer.GetText(Item: longInt; MaxLen: integer):
    String;
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
    t: boolean;

  function Equal(const s1, s2: String; Count: word): boolean;
    var
      i: word;
    begin
      Equal := False;
      if (Length(s1) < Count) or (Length(s2) < Count) then
        exit;
      for i := 1 to Count do
        if UpCase(s1[i]) <> UpCase(s2[i]) then
          exit;
      Equal := True;
    end;

  function Search(const s1: String; SearchPos: integer; var Index:
      integer): boolean;
    var
      l, H, i, C: integer;
    begin
      Search := False;
      l := Index;
      H := HistoryCount(HistoryId)-1;
      while l <= H do
        begin
          if Equal(HistoryStr(HistoryId, l), s1, SearchPos) then
            begin
              Search := True;
              break;
            end;
          Inc(l);
        end;
      if l > H then
        begin
          l := 0;
          H := Index-1;
          while l <= H do
            begin
              if Equal(HistoryStr(HistoryId, l), s1, SearchPos) then
                  begin
                  Search := True;
                  break;
                end;
              Inc(l);
            end;
        end;
      Index := l;
    end { Search };
  {- $ WildCat $ -}

  {AK155 27-03-2003 по мотивам ScanMarked из TTHistList.HandleEvent}
  function SearchNonDeletableItem(D, Bound: integer): boolean;
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
          if HistList.CurString^[Length(HistList.CurString^)] <> ' '
          then
            begin
              FocusItem(i);
              DrawView;
              Result := True;
              exit;
            end;
        end;
    end { SearchNonDeletableItem };
  {/AK155 27-03-2003}

  begin { THistoryViewer.HandleEvent }
    if ((Event.What = evBroadcast) and (Event.Command =
        cmReleasedFocus))
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
    if ((Event.What = evMouseDown) and (Event.Double)) or
      ((Event.What = evKeyDown) and (Event.KeyCode = kbEnter))
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
    else if (Event.What = evKeyDown) and (Event.KeyCode = kbDel)
    then
      begin
        ClearEvent(Event);
        FreeStr := HistoryStr(HistoryId, Focused);
        if (CurString <> nil) and (CurString^[Length(CurString^)] =
            '+')
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
      ((Event.What = evCommand) and (Event.Command = cmCancel))
    then
      begin
        EndModal(cmCancel);
        ClearEvent(Event);
      end
    else if (Event.What = evKeyDown) and (Event.CharCode <> #0) and (
        Range > 0)
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
            t := False
          end
        else if Event.KeyCode = kbCtrlEnter then
          begin
            Inc(Value);
            if Value >= Range then
              Value := 0;
            t := Search(CurStr, SearchPos, Value);
          end
        else
          begin
            Inc(SearchPos);
            SetLength(CurStr, SearchPos);
            CurStr[SearchPos] := Event.CharCode;
            t := Search(CurStr, SearchPos, Value);
          end;
        ClearEvent(Event);
        ShowCursor;
        if t then
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
    Width, t, Count, i: integer;
  begin
    Width := 0;
    Count := HistoryCount(HistoryId);
    for i := 0 to Count-1 do
      begin
        t := Length(HistoryStr(HistoryId, i));
        if t > Width then
          Width := t;
      end;
    HistoryWidth := Width;
  end;

{ THistoryWindow }

Constructor THistoryWindow.Init(var Bounds: TRect; HistoryId: AWord);
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

Constructor THistory.Init(var Bounds: TRect; ALink: PInputline;
  AHistoryId: AWord);
  begin
    TView.Init(Bounds);
    Options := Options or ofPostProcess;
    EventMask := EventMask or evBroadcast;
    Link := ALink;
    HistoryId := AHistoryId;
  end;

Constructor THistory.Load(var s: TStream);
  begin
    TView.Load(s);
    GetPeerViewPtr(s, Link);
    s.Read(HistoryId, SizeOf(AWord));
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
    if (Event.What = evMouseDown) or
      ((Event.What = evKeyDown) and ( {CtrlToArrow}(Event.KeyCode) =
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
      if ((Event.Command = cmReleasedFocus) and (Event.InfoPtr =
          Link))
        or (Event.Command = cmRecordHistory)
      then
        RecordHistory(Link^.Data^);
  end { THistory.HandleEvent };

function THistory.InitHistoryWindow(var Bounds: TRect):
    PHistoryWindow;
  var
    R, o: TRect;
    P: PHistoryWindow;
  begin
    {===START====  New code realisation by Shumskii Lev aka WildCat}
    Owner^.GetExtent(o);
    Link^.GetBounds(R);
    if (o.B.Y-R.B.Y) > 4 then
      begin
        Inc(R.A.X);
        Inc(R.B.Y);
        Inc(R.B.Y, 7);
        Inc(R.A.Y, 1);
        Inc(R.B.X, 2);
        R.Intersect(o);
        if (R.B.Y >= o.B.Y) then
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

procedure THistory.RecordHistory(const s: String);
  begin
    HistoryAdd(HistoryId, s);
  end;

procedure THistory.Store(var s: TStream);
  begin
    TView.Store(s);
    PutPeerViewPtr(s, Link);
    s.Write(HistoryId, SizeOf(AWord));
  end;

Constructor THexLine.Init;
  begin
    if AInputLine = nil then
      exit;
    inherited Init(R);
    Options := Options or ofSelectable or ofPostProcess;
    EventMask := $FFFF;
    InputLine := AInputLine;
  end;

Constructor THexLine.Load(var s: TStream);
  begin
    inherited Load(s);
    GetPeerViewPtr(s, InputLine);
  end;

procedure THexLine.Store(var s: TStream);
  begin
    inherited Store(s);
    PutPeerViewPtr(s, InputLine);
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
    s: String;

  begin
    inherited HandleEvent(Event);
    case Event.What of
      evBroadcast:
        if (Event.Command = cmUpdateHexViews) and
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
          else
            case UpCase(Event.CharCode) of
              '0'..'9', 'A'..'F':
                begin
                  if (CurX = InputLine^.MaxLen-1) and Sec then
                    begin
                      CE;
                      exit
                    end;
                  s := InputLine^.Data^;
                  if CurX+1 > Length(s) then
                    s := s+#0;
                  if Sec then
                    s[CurX+1] := Char((byte(s[CurX+1]) and $F0) or (
                      Pos(UpCase(Event.CharCode),
                    '0123456789ABCDEF')-1))
                  else
                    s[CurX+1] := Char((byte(s[CurX+1]) and $F) or (
                      Pos(UpCase(Event.CharCode),
                    '0123456789ABCDEF')-1) shl 4);
                  InputLine^.Data^:= Copy(s, 1, InputLine^.MaxLen);
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
              else
                if GetState(sfFocused) and (Event.CharCode > #0)
                then
                  CE;
            end {case};
        end {case};
    end {case};
  end { THexLine.HandleEvent };

procedure THexLine.Draw;
  var
    B: TDrawBuffer;
    s: String;
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
    s := Copy(InputLine^.Data^, DeltaX+1, MaxStringLength);
    s := Copy(DumpStr(s[1], 0, 16, 0), 11, Length(s)*3);
    SetCursor(1+3*(CurX-DeltaX)+byte(Sec), 0);
    ShowCursor;
    MoveStr(B[1], Copy(s, 1, Size.X-2), C);
    {-DataCompBoy: This is a temporary code!}
    MoveChar(B[Size.X-1], #32, InputLine^.GetColor(4), 1);
    MoveChar(B[0], #32, InputLine^.GetColor(4), 1);
    {-DataCompBoy}
    WriteLine(0, 0, Size.X, Size.Y, B);
  end { THexLine.Draw };

end.
