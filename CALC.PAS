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
{AK155 = Alexey Korop, 2:461/155@fidonet}
{KV = Kirill Vodonosov}
{20.08.2002 AK Почти весь текст в той или иной степени переработан.
  См. также CellsCol. Основное нововведение - TSort и новый Recalc }
unit Calc;
{&Delphi+}
interface

uses
  advance, Lfn, Objects, Views, CellsCol, Drivers, Dialogs, Messages,
  HistList, UniWin, Commands, DNHelp, RStrings, Calculat;

const
  CellClipboard: PCellCollection = nil;
  ClipRect: record
    A, B: TPoint;
    end = (A: (X: 0; Y: 0); B: (X: 0; Y: 0));
  FalseStr: String[10] = 'False';
  TrueStr: String[10] = 'True';

type
  PCalcView = ^TCalcView;
  PCalcInput = ^TCalcInput;
  PInfoView = ^TInfoView;

  PCalcWindow = ^TCalcWindow;
  TCalcWindow = object(TUniWindow)
    CalcView: PCalcView;
    Constructor Init(Bounds: TRect; AName: String); {DataCompBoy}
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    end;

  TCalcView = object(TView)

    FocusEvent: TEvent;
    CalcInput: PCalcInput;
    CellInfo: PInfoView;

    HScroll, VScroll: PScrollBar;
    Delta, Cur, Mark: TPoint;
    Cells: PCellCollection;
    NumC: byte;
    Marking, BlockDraw, Modified: boolean;
    ShowSeparators: boolean;
    ColWidth: array[byte] of byte;
    CurrentCalc, SearchPos, ErrorCell: TPoint;
    SName: PString; {DataCompBoy}
    Constructor Init(Bounds: TRect;
    AInfo: PCalcInput; ACellInfo: PInfoView;
    AHScroll, AVScroll: PScrollBar);
    destructor Done; virtual;

    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);

    procedure Draw; virtual;
    function Valid(Command: word): boolean; virtual;
    procedure SetState(AState: word; Enable: boolean); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetCellValue(const s: String): boolean;
    {S - имя ячейки; результат - в calculat.Res}
    function GetFuncValue(s: String): boolean;
    {S - функция SUM или MUL, результат - в calculat.Res}
    procedure CalcError(Index: TStrIdx);
    procedure LoadSheet(FName: String); {DataCompBoy}
    procedure SaveSheet;
    procedure SaveSheetAs;
    function AskSave: boolean;
    procedure Copy;
    procedure Paste;
    procedure Clear;
    function CalcEval(const s: String; var Value: CReal): boolean;
    procedure ReCalc(Full: boolean);
    procedure InsertLine;
    procedure InsertCol;
    procedure DeleteLine;
    procedure DeleteCol;
    procedure GotoCell(Cell: String);
    procedure SearchCell;
    procedure RewriteFormula(var P: PCellrec; LX, LY, DX, DY:
      integer);
    end;

  TCalcInput = object(TInputLine)
    CalcView: PCalcView;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  TInfoView = object(TView)
    InfoStr: String;
    InfoAttr: byte;
    procedure SetInfo(s: String; Attr: byte);
    procedure Draw; virtual;
    end;

function GetFileName(var FileName: String; Mask, Title, ALabel:
    String; Buttons: word): word;

implementation

uses
  Collect, Strings, ErrMess,
  Memory, Dos, DNApp, DNStdDlg, advance1, advance2,
  MicroEd2, Histries;

type
  THeaderDBF = record{KV}
    DBFIdent: Char; { $03 - Нет MEMO; $83 - Есть MEMO }
    { FoxBase+, FoxPro, dBaseIII+, dBaseIV, no memo - 0x03 }
    { FoxBase+, dBaseIII+ with memo - 0x83 }
    { FoxPro with memo - 0xF5 }
    { dBaseIV with memo - 0x8B }
    { dBaseIV with SQL Table - 0x8E }

    Yar: byte; { две последние цифры года }
    Month: byte; { month number }
    Day: byte; { day number }
    LastRecord: longInt; { номер последней записи }
    DataOffset: AWord;
      { смещение первой записи относительно начала файла}
    RecSize: AWord; { размер записи с учетом символа удаления }
    Reserv1: AWord;
    WaitTrans: byte; { dB IV }
    Reserv2: array[0..12] of byte;
    Multiindex: byte;
    Reserv3: array[0..2] of byte;
    end;
  TDBFLength = record{ длина поля DBF }{KV}
    case integer of
      0: (FieldLength: AWord);
      1: (NumericLength: byte; Decimals: byte);
  end;
PDBFField = ^TDBFField; {KV}
TDBFField = record{ dB field descriptor Length = 32 bytes }
  FieldName: array[0..10] of Char; { имя поля }
  FieldType: Char; { тип: C=$43, D=$44, L=$4C, $M=$4D, N=$4E }
  Reserved: array[0..3] of Char; { Расположение поля внутри записи }
  FLength: TDBFLength; { длина поля }
  Reserved2: array[0..12] of byte;
  Tag: byte; { for multiindex, only dB IV }
  end;

PDbfFieldCollection = ^TDbfFieldCollection;
TDbfFieldCollection = object(TCollection) {KV}
  procedure FreeItem(Item: Pointer); virtual;
  end;

TDbaseWriter = object(TBufStream) {KV}
  Header: THeaderDBF;
  Fields: PDbfFieldCollection;
  CurRecord: longInt;
  EofFlag: boolean;
  Constructor Init(FileName: FNameStr; Mode: word; Size: SW_Word);
  destructor Done; virtual;
  procedure AddField(const NameField: String; TypeField: Char;
  LenField: integer; DecField: integer);
  procedure AddRecord;
  procedure CreateFile;
  procedure DeleteRecord;
  function FCount: integer;
  function FieldAsString(FieldIndex: integer): String;
  function FieldAsInteger(FieldIndex: integer): integer;
  function FieldAsFloat(FieldIndex: integer): Double;
  procedure FieldPutString(FieldIndex: integer; FieldValue: String);
  procedure FieldPutInteger(FieldIndex: integer; FieldValue: integer);
  procedure FieldPutFloat(FieldIndex: integer; FieldValue: Double);
  function FieldName(FieldIndex: integer): String;
    // Warning! FieldIndex from 1
  function FieldDec(FieldIndex: integer): byte; // FieldIndex >= 1
  function FieldLen(FieldIndex: integer): AWord;
    // Warning! FieldIndex from 1
  function FieldPos(NameField: String): integer; // FieldIndex >= 1
  function FieldType(FieldIndex: integer): Char;
  function FieldOffsetInBuffer(FieldIndex: integer): integer;
    // FieldIndex >= 1
  function FieldOffsetInFile(FieldIndex: integer): longInt;
    // FieldIndex >= 1
  procedure First;
  procedure DBGoTo(NewRecord: integer);
  procedure Next;
  procedure ReadFile;
  procedure RecallRecord;
  procedure WriteEndOfFile;
  end;

TExcelWriter = object(TBufStream) {KV}
  procedure WriteBOF;
  procedure WriteEOF;
  procedure WriteBLANK(const Col, Row: AWord; const XF: AWord);
  procedure WriteLABEL(const Data: String; const Col, Row: AWord;
    const XF: AWord);
  procedure WriteNUMBER(const Data: Double; const Col, Row: AWord;
    const XF: AWord);
  procedure WriteBOOL(const Data: boolean; const Col, Row: AWord;
    const XF: AWord);
  procedure WriteERROR(const Data: byte; const Col, Row: AWord;
    const XF: AWord);
  end;

const
  SearchData: record
    s: String[240];
    CellOptions, TxtOptions: word;
    end =
  (s: ''; CellOptions: 0; TxtOptions: 0);
  ReplaceData: record
    s, s1: String[240];
    CellOptions, TxtOptions: word;
    end =
  (s: ''; s1: ''; CellOptions: 0; TxtOptions: 0);
  WasReplace: boolean = False;
  ContSearch: boolean = False;
  SearchCanceled: boolean = False;
  DefaultColWidth = 11;
  UntitledName = 'Untitled.WKZ';

  {-DataCompBoy-}
function GetFileName(var FileName: String; Mask, Title, ALabel:
    String; Buttons: word): word;
  var
    s: String;
    D: PFileDialog;
    B: boolean;
    C: word;
  begin
    s := '';
    B := False;
    if Mask = '' then
      begin
        Mask := x_x;
        B := False
      end;
    D := PFileDialog(Application^.ValidView(New(PFileDialog,
    Init(Mask, Title, ALabel, Buttons, 211))));
    if D = nil then
      exit;
    if B then
      D^.SetData(s);
    C := Desktop^.ExecView(D);
    if C <> cmCancel then
      D^.GetFileName(s);
    GetFileName := C;
    Dispose(D, Done);
    {$IFDEF OS_DOS}
    FileName := lfGetLongFileName(s);
    {$ELSE}
    FileName := s;
    {$ENDIF}
  end { GetFileName };
{-DataCompBoy-}

{-------------------------       TInfoView       -------------------------}

procedure TInfoView.SetInfo;
  begin
    InfoStr := Copy(s, 1, Size.X);
    InfoAttr := Attr;
    DrawView;
  end;

procedure TInfoView.Draw;
  var
    B: TDrawBuffer;
  begin
    MoveChar(B, ' ', InfoAttr, Size.X);
    MoveStr(B[Size.X-Length(InfoStr)], InfoStr, InfoAttr);
    WriteLine(0, 0, Size.X, 1, B);
  end;

{-------------------------      TCalcWindow      -------------------------}

{-DataCompBoy-}
Constructor TCalcWindow.Init;
  var
    R: TRect;
    P: PView;
    P1: PInfoView;
  begin
    if (AName = '') or (GetFileAttr(AName+#0) and Directory <> 0)
    then
      AName := UntitledName
    else
      AName := lFExpand(AName);
    TWindow.Init(Bounds, Cut(AName, Bounds.B.X-Bounds.A.X-12), 0);
    Options := Options or ofTileable;
    Flags := Flags or wfMaxi;
    if LowMemory then
      exit;

    R.A.X := 1;
    R.A.Y := 5;
    R.B.X := 7;
    R.B.Y := Size.Y-1;
    P := New(PStaticText, Init(R, ''));
    P^.Options := P^.Options or ofFramed;
    P^.GrowMode := gfGrowHiY;
    Insert(P);

    R.A.X := 8;
    R.A.Y := 2;
    R.B.X := Size.X-1;
    R.B.Y := 4;
    P := New(PStaticText, Init(R, ''));
    P^.Options := P^.Options or ofFramed;
    P^.GrowMode := gfGrowHiX;
    Insert(P);

    GetExtent(R);
    R.Grow(-1, -1);
    R.B.Y := R.A.Y+1;
    P := PView(LoadResource(dlgWkzMenuBar));
    P^.Locate(R);
    P^.GrowMode := gfGrowHiX;
    Insert(P);

    GetExtent(R);
    R.Grow(-1, -1);
    Inc(R.A.Y);
    R.B.Y := R.A.Y+1;
    Inc(R.A.X, 7);
    P := New(PCalcInput, Init(R, 240));
    P^.GrowMode := gfGrowHiX;
    P^.Options := P^.Options or ofSelectable;
    Insert(P);

    R.Assign(1, 2, 7, 4);
    P1 := New(PInfoView, Init(R));
    PInfoView(P1)^.InfoStr := '';
    PInfoView(P1)^.InfoAttr := GetColor(9);
    Insert(P1);

    GetExtent(R);
    R.Grow(-1, -1);
    Inc(R.A.Y, 2);
    CalcView := New(PCalcView, Init(R, PCalcInput(P), P1,
    MakeScrollBar(sbHorizontal+sbHandleKeyboard),
    MakeScrollBar(sbVertical+sbHandleKeyboard)));
    Insert(CalcView);
    CalcView^.LoadSheet(AName);
  end { TCalcWindow.Init };
{-DataCompBoy-}

Constructor TCalcWindow.Load;
  var
    R: TRect;
    P: PView;
    P1: PInfoView;
  begin
    inherited Load(s);
    GetSubViewPtr(s, CalcView);
    if CalcView = nil then{Cat}
      Fail;
  end;

procedure TCalcWindow.Store;
  begin
    inherited Store(s);
    PutSubViewPtr(s, CalcView);
  end;

procedure TCalcWindow.HandleEvent;
  begin
    case Event.What of
      evCommand:
        case Event.Command of
          cmClose, cmQuit:
            if not CalcView^.AskSave then
              ClearEvent(Event);
        end {case};
      evBroadcast:
        case Event.Command of
          cmFindView:
            if PString(Event.InfoPtr)^ = CnvString(CalcView^.SName)
            then
              begin
                Self.Select;
                ClearEvent(Event);
              end;
        end {case};
    end {case};
    inherited HandleEvent(Event);
  end { TCalcWindow.HandleEvent };

destructor TCalcWindow.Done;
  begin
    StoreViewInfo(@Self);
    inherited Done;
  end;

{-----------------------------    TCalcInput     ---------------------------}

Constructor TCalcInput.Load;
  begin
    inherited Load(s);
    GetPeerViewPtr(s, CalcView);
  end;

procedure TCalcInput.Store;
  begin
    inherited Store(s);
    PutPeerViewPtr(s, CalcView);
  end;

function TCalcInput.GetPalette;
  const
    s: String[4] = #13#13#14#13;
  begin
    GetPalette := @S;
  end;

procedure MakeDefaultOptions(const s: String;
  var o: AWord; var D: byte; var R: CReal);
  var
    i: integer;
  begin
    Val(s, R, i);
    D := 2; { 2 десятичных знака по умолчанию }
    if i = 0 then{ число: вправо, как есть }
      o := coValue or coRight
    else if (s[1] = '=') and (Length(s) > 1) then
      { формула: вправо, десятичное, 2 знака }
      o := coFormula or coDec or coRight
    else{ текст: влево, как есть }
      begin
        o := 0;
        R := 0;
      end;
  end;

procedure TCalcInput.HandleEvent;

  function EndEdit: boolean;
    label 2;
    var
      NewS: ShortString;
      o: AWord;
      D: byte;
      R: CReal;
      i: integer;
      P: PCellrec;
      TypeChanged: boolean;
    begin
      GetData(NewS);
      EndEdit := True;
      with CalcView^ do
        begin
          Modified := True;
          CurrentCalc.X := Delta.X+Cur.X;
          CurrentCalc.Y := Delta.Y+Cur.Y;
          DelRight(NewS);
          P := Cells^.Get(Delta.X+Cur.X, Delta.Y+Cur.Y);
          if (P <> nil) and (NewS = P^.s) then
            exit;
          if NewS = '' then
            begin
              if (P = nil) then
                exit;
              if MessageBox(GetString(dlWkzWarningClearCell),
                nil, mfOKCancel) <> cmOK
              then
                begin
                  EndEdit := False;
                  exit;
                end;
              Cells^.DelItem(Delta.X+Cur.X, Delta.Y+Cur.Y);
              goto 2;
            end;

          MakeDefaultOptions(NewS, o, D, R);
          TypeChanged := True; // на случай P=nil
          if P <> nil then
            with P^ do
              begin
                if (Options and coTypeMask) = coTypeMask then
                  Options := (Options and not coTypeMask) or
                    coFormula;
                TypeChanged := (Options xor o) and coTypeMask <> 0;
                if TypeChanged then
                  begin{Изменен тип ячейки}
                    if MessageBox(GetString(
                        dlWkzWarningCellTypeChange),
                      nil, mfOKCancel) <> cmOK
                    then
                      begin
                        EndEdit := False;
                        exit;
                      end;
                  end;
              end;
          with Cells^.ReplaceItem(Delta.X+Cur.X, Delta.Y+Cur.Y, NewS)^
          do
            begin
              if TypeChanged then
                begin
                  Options := o;
                  Decimals := D;
                end;
              Value := R;
            end;
2:
          ReCalc(False);
        end {with CalcView^};
    end { EndEdit: };

  procedure Transfer;
    begin
      CalcView^.FocusEvent := Event;
      if EndEdit then
        PWindow(Owner)^.SelectNext(False);
      ClearEvent(Event)
    end;

  begin { TCalcInput.HandleEvent }
    case Event.What of
      evKeyDown:
        case Event.KeyCode of
          kbUp, kbDown, kbPgUp, kbPgDn, kbCtrlPgUp,
          kbCtrlPgDn, kbEnter:
            Transfer;
          kbLeft:
            if CurPos = 0 then
              Transfer;
          kbRight:
            if CurPos > Length(CnvString(Data))-1 then
              Transfer;
          kbTab:
            if not EndEdit then
              ClearEvent(Event);
        end {case};
    end {case};
    TInputLine.HandleEvent(Event);
  end { TCalcInput.HandleEvent };

{-----------------------------    TCalcView     ---------------------------}

Constructor TCalcView.Init;
  var
    i, j: integer;
  begin
    TView.Init(Bounds);
    HelpCtx := hcSpreadSheet;
    HScroll := AHScroll;
    VScroll := AVScroll;
    Options := Options or ofSelectable;
    GrowMode := gfGrowHiX+gfGrowHiY;
    EventMask := $FFFF;
    CalcInput := AInfo;
    CellInfo := ACellInfo;
    PCalcInput(CalcInput)^.CalcView := @Self;
    FocusEvent.What := evNothing;
    FillChar(ColWidth, SizeOf(ColWidth), DefaultColWidth);
    Marking := False;
    BlockDraw := False;
    Modified := False;
    Cells := nil;
    Delta.X := 0;
    Delta.Y := 0;
    Cur := Delta;
    Mark := Cur;
    HScroll^.SetParams(0, 0, 255, 1, 1);
    VScroll^.SetParams(0, 0, MaxCellY-1, Size.Y-3, 1);
    SName := nil;
  end { TCalcView.Init };

Constructor TCalcView.Load;
  var
    R: TRect;
  begin
    inherited Load(s);
    GetPeerViewPtr(s, CalcInput);
    GetPeerViewPtr(s, CellInfo);
    GetPeerViewPtr(s, HScroll);
    GetPeerViewPtr(s, VScroll);
    s.Read(Delta, SizeOf(Delta)*3);
    s.Read(NumC, 6); {###}
    SName := s.ReadStr;
    LoadSheet(SName^);
    FocusEvent.What := evNothing;
    ReCalc(True);
  end;

procedure TCalcView.Store;
  begin
    inherited Store(s);
    PutPeerViewPtr(s, CalcInput);
    PutPeerViewPtr(s, CellInfo);
    PutPeerViewPtr(s, HScroll);
    PutPeerViewPtr(s, VScroll);
    s.Write(Delta, SizeOf(Delta)*3);
    s.Write(NumC, 6); {###}
    s.WriteStr(SName);
  end;

function TCalcView.Valid;
  begin
    Valid := Cells <> nil;
  end;

procedure TCalcView.SetState;
  var
    Bounds: TRect;
  begin
    TView.SetState(AState, Enable);
    if Enable and (AState and sfFocused <> 0) then
      DrawView;
    if (AState and sfActive <> 0) then
      if Enable then
        begin
          HScroll^.GrowTo(HScroll^.Size.X, 1);
          VScroll^.GrowTo(1, VScroll^.Size.Y);
          EnableCommands([cmUndo, cmCut, cmCopy, cmPaste, cmClear]);
        end
      else
        begin
          HScroll^.GrowTo(HScroll^.Size.X, 0);
          VScroll^.GrowTo(0, VScroll^.Size.Y);
        end;
    if Owner <> nil then
      begin
        GetBounds(Bounds);
        DisposeStr(PWindow(Owner)^.Title);
        if SName <> nil then
          PWindow(Owner)^.Title := NewStr(Cut(SName^, Bounds.B.X-
            Bounds.A.X-12));
      end;
  end { TCalcView.SetState };

function TCalcView.GetPalette;
  const
    s: String[4] = #8#13#14;
  begin
    GetPalette := @S;
  end;

type
  TQArray = array[0..80, 0..60] of integer;
  PQArray = ^TQArray;

const
  Q: PQArray = nil;

function MakeCellText(P: PCellrec): String; {KV}
  var
    s1: String absolute Result;
    i, A1, A2: integer;
  begin
    s1 := '';
    case (P^.Options shr 4) and 7 of
      0:
        s1 := P^.s;
      1:
        Str(P^.Value: 0: P^.Decimals, s1);
      2:
        begin
          Str(P^.Value: 0: 20, s1);
          i := Length(s1);
          while (i > 0) and (s1[i] = '0') do
            Dec(i);
          SetLength(s1, i);
          if s1[Length(s1)] = '.' then
            SetLength(s1, Length(s1)-1);
          s1 := MakeComma(s1);
        end;
      3:
        begin
          Str(P^.Value, s1);
          A1 := Pos('E', s1);
          if A1 > 0 then
            begin
              A2 := A1-1;
              while (s1[A2] = '0') and (s1[A2-1] <> '.') do
                Dec(A2);
              Move(s1[A1], s1[A2+1], Length(s1)-A1+1);
              SetLength(s1, Length(s1)-(A1-A2-1));
            end;
        end;
      4:
        if P^.Value = 0 then
          s1 := FalseStr
        else
          s1 := TrueStr;
      5:
        MakeCurrency(P^.Value, s1);
      6:
        begin
          Str((P^.Value*100): 0: 2, s1);
          s1 := MakeComma(s1)+'%';
        end;
      7:
        s1 := '';
    end {case};
  end { MakeCellText };

procedure FormatCellText(const s1: String; k: integer;
  ShowSeparators: boolean;
  Options: AWord; var s: String);
  var
    t: integer;
  begin
    if Length(s1) >= k then
      begin
        if ShowSeparators then
          begin
            Move(s1[1], s[1], k-1);
            s[k] := #16;
          end
        else
          Move(s1[1], s[1], Length(s1))
      end
    else
      begin
        t := 1-Ord(ShowSeparators);
        if Options and $C = 0 then
          Move(s1[1], s[1], Length(s1))
        else if (Options and $C = coRight) then
          Move(s1[1], s[k-Length(s1)+t], Length(s1))
        else
          Move(s1[1], s[1+(k-Length(s1)) div 2], Length(s1));
      end;
  end { FormatCellText };

procedure TCalcView.Draw;

  var
    B, B1: TDrawBuffer;
    BC: array[0..High(B)] of record
      C: Char;
      A: byte;
      end absolute B;
    i, j, k, l, t: integer;
    C1, C2, C3: byte;
    s, s1: String;
    P: PCellrec;
    X1, X2, Y1, Y2: integer;
    CurPos: TPoint;
    StX, EnX, A1, A2: byte;
    Wrk: integer;
    EmptyLine: String;
  begin
    if BlockDraw then
      exit;
    C1 := GetColor(1);
    C2 := GetColor(2);
    C3 := GetColor(3);
    if Q = nil then
      New(Q);
    CurPos.X := Delta.X+Cur.X;
    CurPos.Y := Delta.Y+Cur.Y;
    if CurPos.X > 255 then
      Cur.X := 255-Delta.X;
    if CurPos.Y >= MaxCellY then
      Cur.Y := MaxCellY-1-Delta.Y;
    if Cur.Y < 0 then
      begin
        j := Cur.Y;
        Cur.Y := 0;
        VScroll^.SetValue(Delta.Y+j);
        exit
      end;
    if Cur.X < 0 then
      begin
        j := Cur.X;
        Cur.X := 0;
        HScroll^.SetValue(Delta.X+j);
        exit
      end;
    CurPos.X := Delta.X+Cur.X;
    CurPos.Y := Delta.Y+Cur.Y;
    X1 := CurPos.X;
    X2 := Mark.X;
    Y1 := CurPos.Y;
    Y2 := Mark.Y;
    if X1 > X2 then
      begin
        Wrk := X1;
        X1 := X2;
        X2 := Wrk;
      end;
    if Y1 > Y2 then
      begin
        Wrk := Y1;
        Y1 := Y2;
        Y2 := Wrk;
      end;
    if Y2 > 4095 then
      Y2 := 4095;
    SetLength(EmptyLine, Size.X);
    FillChar(EmptyLine[1], Size.X, ' ');
    MoveChar(B, ' ', C1, Size.X);
    BC[6].C := '│';
    MoveChar(B1, '─', C1, Size.X);
    WordRec(B1[6]).Lo := byte('┼');
    NumC := 0;
    i := 7;
    j := Delta.X;
    StX := i;
    EnX := Size.X;

    {Заголовки колонок и верхняя черта}
    while (i < Size.X) and (j <= HScroll^.Max) do
      begin
        k := ColWidth[j];
        l := ColWidth[j+1];
        SetLength(s, k);
        FillChar(s[1], k, ' ');
        s[k] := '│';
        s1 := GetColName(j);
        Move(s1[1], s[k div 2], Length(s1));
        WordRec(B1[i+k-1]).Lo := byte('┼');
        MoveStr(B[i], s, C1);
        if j < X1 then
          Inc(StX, k);
        if (j > X2) and (EnX > i) then
          EnX := i;
        if (j = HScroll^.Max) and (j <= X2) then
          EnX := i+k;
        Inc(NumC);
        Inc(j);
        Inc(i, k);
      end;

    if NumC = 0 then
      NumC := 1;
    if Cur.X >= NumC then
      begin
        j := Cur.X-NumC+1;
        Cur.X := NumC-1;
        HScroll^.SetValue(Delta.X+j);
        Delta.X := HScroll^.Value;
        exit;
      end;
    PInfoView(CellInfo)^.SetInfo(GetCellName(CurPos.X, CurPos.Y),
      Owner^.GetColor(9));
    FillChar(Q^, SizeOf(Q^), 255);
    for i := 1 to Cells^.Count do
      begin
        P := Cells^.At(i-1);
        if (P^.Row >= Delta.Y) and (P^.Row < Delta.Y+Size.Y-2) and
          (P^.Col >= Delta.X) and (P^.Col < Delta.X+NumC+3)
        then
          Q^[P^.Col-Delta.X, P^.Row-Delta.Y] := i-1;
      end;
    WriteBuf(6, 0, Size.X, 1, B[6]);
    WriteBuf(0, 1, Size.X, 1, B1);

    for i := 0 to Size.Y-2 do
      begin
        MoveChar(B, ' ', C2, Size.X);
        if Delta.Y+i <= VScroll^.Max then
          begin
            Str((Delta.Y+i+1): 5, s);
            MoveStr(B, s, C1);
            l := 7;
            for j := 0 to Min(NumC-1, 255-Delta.X) do
              begin
                k := ColWidth[Delta.X+j];
                if ShowSeparators then
                  begin{ ограничиваем длину по колонке }
                    SetLength(s, k);
                    FillChar(s[1], k, ' ');
                    s[k] := '│';
                  end
                else
                  begin{ длина до конца экрана }
                    t := Size.X-l+1;
                    SetLength(s, t);
                    FillChar(s[1], t, ' ');
                  end;
                P := nil;
                if Q^[j, i] >= 0 then
                  begin
                    P := Cells^.At(Q^[j, i]);
                    s1 := MakeCellText(P);
                    FormatCellText(s1, k, ShowSeparators, P^.Options, s)
                      ;
                  end;
                if ShowSeparators or (P <> nil) then
                  MoveStr(B[l], s, C2);
                if (Cur.X = j) and (Cur.Y = i) then
                  begin
                    if P <> nil then
                      begin
                        s := P^.s;
                        case P^.Options and 3 of
                          coText:
                            WriteStr(0, 0, '   T  ', 2);
                          coValue:
                            WriteStr(0, 0, '   V  ', 2);
                          else
                            WriteStr(0, 0, '   F  ', 2);
                        end {case};
                      end
                    else
                      begin
                        WriteStr(0, 0, GetString(dlWKZ_Empty), 2);
                        s := '';
                      end;
                    PCalcInput(CalcInput)^.SetData(s);
                    CalcInput^.Draw;
                  end;
                l := l+k;
              end;
          end;
        BC[6].C := '│';
        BC[6].A := C1;
        if (Delta.Y+i >= Y1) and (Delta.Y+i <= Y2) and (StX < EnX)
        then
          MoveColor(B[StX], EnX-StX-Ord(ShowSeparators), C3);
        WriteBuf(0, i+2, Size.X, 1, B);
      end;
  end { TCalcView.Draw };

{-DataCompBoy-}
procedure TCalcView.HandleEvent;
  type
    TColIndexArray = array[0..255] of integer; {KV}
    TColTypeAndLengthRec = record{KV}
      t: Char;
      l: AWord;
      D: byte;
      end;
    PColTypeAndLengthArray = ^TColTypeAndLengthArray;
    TColTypeAndLengthArray = array[0..255] of TColTypeAndLengthRec;
      {KV}

  var
    Inf: PCalcInput;
    s: String;
    ST: PStream;
    MaxX, MaxY, k, l: integer;
    P: PCellrec;
    FName: String;
    X1, X2, Y1, Y2: integer;
    R: record
      Display, Justify: word;
      Dec: String[2];
      Protect: word;
      end;
    i: integer;
    Wrk: integer;

  procedure kvGetMaxColRow(var MaxCol, MaxRow: integer); {KV}
    var
      i: integer;
      P: PCellrec;
    begin
      MaxCol := 0;
      MaxRow := 0;
      for i := 0 to Cells^.Count-1 do
        begin
          P := Cells^.At(i);
          if MaxCol < P^.Col then
            MaxCol := P^.Col;
          if MaxRow < P^.Row then
            MaxRow := P^.Row;
        end;
    end;

  procedure kvFillColIndexArray(CurRow: integer; var ColIndexArray:
      TColIndexArray); {KV}
    var
      i: integer;
      P: PCellrec;
    begin
      FillChar(ColIndexArray, SizeOf(ColIndexArray), 255);
      for i := 0 to Cells^.Count-1 do
        begin
          P := Cells^.At(i);
          if P^.Row = CurRow then
            ColIndexArray[P^.Col] := i;
        end;
    end;

  procedure ImportFromCsv; {KV}
    var
      s: String;
      FileName: String;
      CellValue: String;
      ColSeparator: Char;
      F: lText;
      CurCol, CurRow: integer;
      StartCh, EndCh: integer;
      P: PCellrec;
      R: CReal;
      i: integer;
    begin
      if Cells^.Count > 0 then
        begin
          if MessageBox(GetString(dlWkzWarningClear), nil,
            mfYesNoCancel or mfConfirmation) <> cmYes
          then
            exit;
        end;
      if GetFileName(FileName, '*.CSV', GetString(dlOpenFile),
          GetString(dlOpenFileName),
        fdOKButton+fdHelpButton) = cmCancel
      then
        exit;
      {KV: разделитель колонок можно определить автоматически, однако при этом
         возникает несколько проблем: если в файле только одна строка нельзя
         определить какие символы встречаются во второй строке столько-же раз
         сколько и в первой. Выполнять проверку только на ',',';' неправильно}
      s := ';';
      if InputBox(GetString(dlWkzQuerySeparatorTitle), GetString(
          dlWkzQuerySeparatorLabel),
        s, 1, hcSpreadSheet) <> cmOK
      then
        exit;
      if s = '' then
        ColSeparator := ';'
      else
        ColSeparator := s[1];

      lAssignText(F, FileName);
      lSetTAttr(F, Archive);
      lResetTextReadOnly(F);
      i := IOResult;
      if i <> 0 then
        begin
          MessFileNotOpen(FileName, i);
          exit;
        end;
      Cells^.FreeAll;
      CurRow := 0;
      while not Eof(F.t) do
        begin
          readln(F.t, s);
          if s <> '' then
            begin{KV: Просто на всякий случай}
              CurCol := 0;
              StartCh := 1;
              while StartCh <= Length(s) do
                begin
                  EndCh := StartCh;
                  while (EndCh <= Length(s)) and (s[EndCh] <>
                      ColSeparator)
                  do
                    Inc(EndCh);
                  if EndCh > StartCh then
                    begin
                      CellValue := System.Copy(s, StartCh, EndCh-
                        StartCh);
                      P := Cells^.NewItem(CurCol, CurRow, CellValue);
                      MakeDefaultOptions(CellValue, P^.Options, P^.
                        Decimals, P^.Value);
                    end;
                  StartCh := EndCh+1;
                  Inc(CurCol);
                end;
            end;
          Inc(CurRow);
        end;
      Close(F.t);
      ReCalc(True);
      Modified := True;
      if Owner <> nil then
        Owner^.Redraw;
    end { ImportFromCsv };

  procedure ImportFromDbf; {KV}
    var
      DBF: TDbaseWriter;
      FileName: FNameStr;
      CellValue: String;
      P: PCellrec;
      R: CReal;
      i: integer;
    begin
      if Cells^.Count > 0 then
        begin
          if MessageBox(GetString(dlWkzWarningClear), nil,
            mfYesNoCancel or mfConfirmation) <> cmYes
          then
            exit;
        end;
      if GetFileName(FileName, '*.DBF', GetString(dlOpenFile),
          GetString(dlOpenFileName),
        fdOKButton+fdHelpButton) = cmCancel
      then
        exit;
      DBF.Init(FileName, stOpenRead, 16384);
      if DBF.Status <> stOK then
        begin
          MessFileNotOpen(FileName, DBF.ErrorInfo);
          DBF.Done;
          exit;
        end;

      Cells^.FreeAll;
      DBF.First;
      while not DBF.EofFlag do
        begin
          for i := 1 to DBF.FCount do
            begin
              CellValue := DBF.FieldAsString(i);
              DelRight(CellValue);
              if CellValue <> '' then
                with Cells^.NewItem(i-1, DBF.CurRecord-1, CellValue) do
                  case DBF.FieldType(i) of
                    'N':{число}
                      begin
                        DelLeft(s);
                        Options := coValue or coRight;
                        Decimals := DBF.FieldDec(i);
                      end;
                    'L':{логическое}
                      begin
                        if UpCase(CellValue[1]) = 'T' then
                          s := '-1'
                        else
                          s := '0';
                        Options := coValue or coBool or coRight;
                      end;
                    else{текст, дата, мемо...}
                      if (CellValue[1] = '=') and (Length(CellValue) > 1
                          )
                      then
                        begin
                          Options := coFormula or coDec or coRight;
                          Decimals := 2;
                        end;
                  end {case};
            end;
          DBF.Next;
        end;
      DBF.Done;
      ReCalc(True);
      Modified := True;
      Owner^.Redraw;
    end { ImportFromDbf };

  procedure ExportToCsv; {KV}
    var
      s: String;
      PS: PString;
      ColSeparator: Char;
      UseFormatFlag: boolean;
      CalcFormulaFlag: boolean;
      F: lText;
      MaxCol, MaxRow, CurCol, CurRow: integer;
      P: PCellrec;
      ColIndexArray: TColIndexArray;
      R: record
        Separator: String[1];
        ValueFormat: word;
        FormulaFormat: word;
        end;
    begin
      if GetFileName(s, '*.CSV', GetString(dlExportTitle), GetString(
          dlExportLabel),
        fdOKButton+fdHelpButton) = cmCancel
      then
        exit;
      if ExistFile(s) then
        begin
          PS := @S;
          if MessageBox(GetString(dlED_OverQuery), @PS,
            mfYesNoCancel+mfWarning) <> cmYes
          then
            exit;
        end;

      FillChar(R, SizeOf(R), 0);
      R.Separator := ';';
      if ExecResource(dlgSetExportCsvFormat, R) = cmOK then
        begin
          if Length(R.Separator) > 0 then
            ColSeparator := R.Separator[1]
          else
            ColSeparator := ';';
          UseFormatFlag := (R.ValueFormat = 1);
          CalcFormulaFlag := (R.FormulaFormat = 1);
          lAssignText(F, s);
          lSetTAttr(F, Archive);
          lRewriteText(F);
          if IOResult <> 0 then
            begin
              PS := @S;
              Msg(erCantCreateFile, @PS, mfError+mfOKButton);
              exit;
            end;
          kvGetMaxColRow(MaxCol, MaxRow);
          for CurRow := 0 to MaxRow do
            begin
              kvFillColIndexArray(CurRow, ColIndexArray); {KV}
              for CurCol := 0 to MaxCol do
                begin
                  if ColIndexArray[CurCol] >= 0 then
                    begin
                      P := Cells^.At(ColIndexArray[CurCol]);
                      if ((P^.Options and 3) = coText) or
                        (((P^.Options and coFormula) <> 0) and (not
                          CalcFormulaFlag))
                      then
                        Write(F.t, P^.s)
                      else if ((P^.Options and 3) = coValue) or
                        (((P^.Options and coFormula) <> 0) and
                          CalcFormulaFlag)
                      then
                        begin
                          if UseFormatFlag then
                            s := MakeCellText(P)
                          else
                            begin
                              // Оставляем 5 знаков после запятой, на случай если
                              // неправильно указан формат
                              if P^.Decimals <= 5 then
                                Str(P^.Value: 0: 5, s)
                              else
                                Str(P^.Value: 0: P^.Decimals, s);
                              while s[Length(s)] = '0' do
                                SetLength(s, Length(s)-1);
                              if s[Length(s)] = '.' then
                                SetLength(s, Length(s)-1);
                            end;
                          Write(F.t, s);
                        end
                      else
                        Write(F.t, P^.s); {This code not use}
                    end;
                  if CurCol < MaxCol then
                    Write(F.t, ColSeparator);
                end;
              Writeln(F.t);
            end;
          Close(F.t);
        end;
    end { ExportToCsv };

  procedure kvCalcFieldTypeAndLength(var TypeLenArr:
      TColTypeAndLengthArray);
    var
      i: integer;
      P: PCellrec;
      s1: String;
    begin
      FillChar(TypeLenArr, SizeOf(TypeLenArr), 0);
      for i := 0 to Cells^.Count-1 do
        begin
          P := Cells^.At(i);
          with TypeLenArr[P^.Col] do
            begin
              if (P^.Options and coValue) <> coValue then
                // Если в колонке текст или ошибка в формуле,
                // то эта колонка сохраняется как текстовая
                t := 'C'
              else if (t = #0) and ((P^.Options and $F0) = coBool)
              then
                // Если тип колонки еще не оределен и первое найденное значение
                // имеет формат Boolean, то устанавливаем тип Boolean
                t := 'L'
              else if t = #0 then
                t := 'N'; // По умолчанию - число
            end;
        end;
      // Делаем второй проход когда типы полей уже известны
      // для определения размерности
      for i := 0 to Cells^.Count-1 do
        begin
          P := Cells^.At(i);
          with TypeLenArr[P^.Col] do
            begin
              case t of
                'C':
                  begin
                    if l < Length(P^.s) then
                      l := Length(P^.s);
                  end;
                'N':
                  begin
                    Str(P^.Value: 0: P^.Decimals, s1);
                    if D < P^.Decimals then
                      begin
                        if l > 0 then
                          Inc(l, P^.Decimals-D);
                        D := P^.Decimals;
                      end;
                    if l < Length(s1) then
                      l := Length(s1);
                  end;
                'L':
                  begin
                    l := 1;
                    D := 0;
                  end;
              end {case}; { case }
            end; { with }
        end; { for }
    end { kvCalcFieldTypeAndLength };

  procedure ExportToDbf; {KV}
    var
      FileName: FNameStr;
      PS: PString;
      TypeLenArr: TColTypeAndLengthArray;
      i, MaxCol, MaxRow, CurCol, CurRow: integer;
      DbfHeader: THeaderDBF;
      s: TDbaseWriter;
      Buf: PByteArray;
      TmpS: String;
      FieldName: String;
      ColIndexArray: TColIndexArray;
      P: PCellrec;
    begin
      if GetFileName(FileName, '*.DBF', GetString(dlExportTitle),
          GetString(dlExportLabel),
        fdOKButton+fdHelpButton) = cmCancel
      then
        exit;

      if ExistFile(FileName) then
        begin
          PS := @FileName;
          if MessageBox(GetString(dlED_OverQuery), @PS,
            mfYesNoCancel+mfWarning) <> cmYes
          then
            exit;
        end;

      s.Init(FileName, stCreate, 16384);
      if s.Status <> stOK then
        begin
          PS := @FileName;
          Msg(erCantCreateFile, @PS, mfError+mfOKButton);
          s.Done;
          exit;
        end;

      kvGetMaxColRow(MaxCol, MaxRow);
      kvCalcFieldTypeAndLength(TypeLenArr);

      for i := 0 to MaxCol do
        begin
          with TypeLenArr[i] do
            begin
              FieldName := GetColName(i);
              if t = #0 then
                begin
                  t := 'N';
                  l := 5;
                  D := 0;
                end;
              if l = 0 then
                l := 1;
              if (t = 'L') and (l > 1) then
                l := 1;
              if (t <> 'N') and (D <> 0) then
                D := 0;
              s.AddField(FieldName, t, l, D);
            end;
        end;

      s.CreateFile;

      for CurRow := 0 to MaxRow do
        begin
          kvFillColIndexArray(CurRow, ColIndexArray);
          i := 1; // Номер текущего поля внутри записи
          s.AddRecord; // Добавляем новую пустую запись
          for CurCol := 0 to MaxCol do
            begin
              if ColIndexArray[CurCol] >= 0 then
                begin
                  P := Cells^.At(ColIndexArray[CurCol]);
                  if TypeLenArr[CurCol].t = 'L' then
                    begin
                      if P^.Value = 0 then
                        TmpS := 'F'
                      else
                        TmpS := 'T';
                      s.FieldPutString(i, TmpS);
                    end
                  else if TypeLenArr[CurCol].t = 'N' then
                    s.FieldPutFloat(i, P^.Value)
                  else
                    s.FieldPutString(i, P^.s);
                end;
              Inc(i);
            end;
        end;
      s.WriteEndOfFile;
      s.Done;
    end { ExportToDbf };

  procedure ExportToXls; {KV}
    var
      FileName: FNameStr;
      PS: PString;
      i, MaxCol, MaxRow, CurCol, CurRow: integer;
      s: TExcelWriter;
      ColIndexArray: TColIndexArray;
      P: PCellrec;
    begin
      if GetFileName(FileName, '*.XLS', GetString(dlExportTitle),
          GetString(dlExportLabel),
        fdOKButton+fdHelpButton) = cmCancel
      then
        exit;
      if ExistFile(FileName) then
        begin
          PS := @FileName;
          if MessageBox(GetString(dlED_OverQuery), @PS,
            mfYesNoCancel+mfWarning) <> cmYes
          then
            exit;
        end;

      s.Init(FileName, stCreate, 16384);
      if s.Status <> stOK then
        begin
          PS := @FileName;
          Msg(erCantCreateFile, @PS, mfError+mfOKButton);
          s.Done;
          exit;
        end;

      kvGetMaxColRow(MaxCol, MaxRow);
      s.WriteBOF;
      for CurRow := 0 to MaxRow do
        begin
          kvFillColIndexArray(CurRow, ColIndexArray);
          for CurCol := 0 to MaxCol do
            begin
              if ColIndexArray[CurCol] < 0 then
                s.WriteBLANK(CurCol, CurRow, 0)
              else
                begin
                  P := Cells^.At(ColIndexArray[CurCol]);
                  if (P^.Options and coFormula) <> 0 then
                    s.WriteLABEL(P^.s, CurCol, CurRow, 0)
                  else if (P.Options and coValue) = coValue then
                      begin
                      if (P^.Options and $F0) = coBool then
                        s.WriteBOOL(P^.Value <> 0, CurCol, CurRow, 0)
                      else
                        s.WriteNUMBER(P^.Value, CurCol, CurRow, 0);
                    end
                  else
                    s.WriteLABEL(P^.s, CurCol, CurRow, 0);
                end;
            end;
        end;
      s.WriteEOF;
      s.Done;
    end { ExportToXls };

  procedure ExportToFile;
    var
      s, s1: String;
      C: array[0..255] of integer;
      P: PCellrec;
      MaxLv, Maxr, i, j, k, l, l1: integer;
      F: lText;
    begin
      if GetFileName(s, x_x, GetString(dlExportTitle), GetString(
          dlExportLabel),
        fdOKButton+fdHelpButton) = cmCancel
      then
        exit;
      lAssignText(F, s);
      lSetTAttr(F, Archive);
      lRewriteText(F);
      MaxLv := 0;
      for i := 1 to Cells^.Count do
        begin
          P := Cells^.At(i-1);
          if MaxLv < P^.Row then
            MaxLv := P^.Row;
        end;
      for j := 0 to MaxLv do
        begin
          FillChar(C, SizeOf(C), 255);
          Maxr := 0;
          for i := 1 to Cells^.Count do
            begin
              P := Cells^.At(i-1);
              if j = P^.Row then
                C[P^.Col] := i-1;
              if Maxr < P^.Col then
                Maxr := P^.Col;
            end;
          for i := 0 to Maxr do
            begin
              k := ColWidth[i];
              if ShowSeparators then
                s := Strg(' ', k-1)+'│'
              else
                s := Strg(' ', 255);
              if C[i] >= 0 then
                begin
                  P := Cells^.At(C[i]);
                  s1 := MakeCellText(P);
                  l1 := Length(s1);
                  FormatCellText(s1, k, ShowSeparators, P^.Options, s);
                end
              else
                l1 := Length(s);
              if not ShowSeparators then
                while (i < Maxr) and (C[i+1] < 0) do
                  begin
                    Inc(i);
                    Inc(k, ColWidth[i]);
                  end;
              if k <= l1 then
                Write(F.t, System.Copy(s, 1, k))
              else
                begin
                  if k > 255 then
                    l := 255
                  else
                    l := k;
                  Write(F.t, System.Copy(s, 1, l));
                  repeat
                    Dec(k, l);
                    if k > 255 then
                      l := 255
                    else
                      l := k;
                    if l <= 0 then
                      break;
                    Write(F.t, Strg(' ', l));
                  until k <= 0;
                end;
            end;
          Writeln(F.t);
        end;
      Close(F.t);
    end { ExportToFile };

  procedure SetMark;
    begin
      if Marking then
        exit;
      Mark.X := Delta.X+Cur.X;
      Mark.Y := Delta.Y+Cur.Y;
    end;

  procedure CheckMark;
    begin
      Marking := ShiftState and $3 <> 0
    end;

  procedure EndMarking;
    begin
      Marking := False;
      SetMark;
      DrawView;
      ClearEvent(Event)
    end;

  procedure ChangeFormat;
    const
      j: integer = 0;
      NewOptions: AWord = 0; {см. комментарий к ForRectangle}
    procedure SetFormat(Item: PCellrec);
      begin
        with Item^ do
          begin
            Options := (Options and 3) or NewOptions;
            Decimals := j;
          end;
      end;

    begin{ChangeFormat}
      ClearEvent(Event);
      P := Cells^.Get(Delta.X+Cur.X, Delta.Y+Cur.Y);
      if P = nil then
        begin
          FillChar(R, SizeOf(R), 0);
          R.Dec := '2'
        end
      else
        begin
          R.Display := (P^.Options shr 4) and 7;
          R.Justify := (P^.Options shr 2) and 3;
          Str(P^.Decimals, R.Dec);
          R.Protect := (P^.Options shr 7) and 1;
        end;
      if ExecResource(dlgSetCellFormat, R) = cmOK then
        begin
          X1 := Delta.X+Cur.X;
          X2 := Mark.X;
          Y1 := Delta.Y+Cur.Y;
          Y2 := Mark.Y;
          if X1 > X2 then
            begin
              Wrk := X1;
              X1 := X2;
              X2 := Wrk;
            end;
          if Y1 > Y2 then
            begin
              Wrk := Y1;
              Y1 := Y2;
              Y2 := Wrk;
            end;
          Val(R.Dec, j, i);
          NewOptions := ((R.Justify and 3) shl 2)
          or ((R.Display and 7) shl 4)
          or ((R.Protect and 1) shl 7);
          Cells^.ForRectangle(X1, Y1, X2, Y2, @SetFormat);
        end;
      Modified := True;
      DrawView;
    end { ChangeFormat };

  procedure CE;
    begin
      ClearEvent(Event)
    end;

  procedure ExpandCol(CurCol: integer);
    begin
      if ColWidth[CurCol] < ScreenWidth then
        begin
          Inc(ColWidth[CurCol]);
          DrawView
        end;
      Modified := True;
    end;

  procedure ShrinkCol(CurCol: integer);
    begin
      if ColWidth[CurCol] > 1 then
        begin
          Dec(ColWidth[CurCol]);
          DrawView
        end;
      Modified := True;
    end;

  var
    SaveCellWidth: byte;
    CurCol: AInt;

  begin{TCalcView.HandleEvent}
    Inf := PCalcInput(CalcInput);
    if GetState(sfFocused) and (FocusEvent.What <> evNothing) then
      begin
        PutEvent(Event);
        Event := FocusEvent;
        FocusEvent.What := evNothing;
      end;
    TView.HandleEvent(Event);
    CurCol := Delta.X+Cur.X;
    case Event.What of
      evCommand:
        case Event.Command of
          cmGetName:
            PString(Event.InfoPtr)^:= 'Speadsheet - '+SName^;
          cmImportToFile:
            begin
              ExportToFile;
              ClearEvent(Event)
            end;
          cmImportFromCsv:
            begin
              ImportFromCsv;
              ClearEvent(Event);
            end;
          cmImportFromDbf:
            begin
              ImportFromDbf;
              ClearEvent(Event);
            end;
          cmExportToCsv:
            begin
              ExportToCsv;
              ClearEvent(Event);
            end;
          cmExportToDbf:
            begin
              ExportToDbf;
              ClearEvent(Event);
            end;
          cmExportToXls:
            begin
              ExportToXls;
              ClearEvent(Event);
            end;
          cmSaveSheetAs:
            begin
              SaveSheetAs;
              CE;
              Owner^.Redraw;
            end;
          cmSave:
            begin
              SaveSheet;
              CE;
              Owner^.Redraw;
            end;
          cmOpen:
            begin
              CE;
              k := GetFileName(FName, '*.WKZ', GetString(dlOpenFile),
              GetString(dlOpenFileName), fdOpenButton
                { + fdReplaceButton, hsLoadSheet, 0});
              if k = cmCancel then
                exit;
              LoadSheet(FName);
            end;
          cmChangeWidth:
            begin
              SaveCellWidth := ColWidth[CurCol];
              while True do
                begin
                  CE;
                  GetKeyEvent(Event);
                  if Event.What = evKeyDown then
                    case Event.KeyCode of
                      kbLeft:
                        ShrinkCol(CurCol);
                      kbRight:
                        ExpandCol(CurCol);
                      kbEnter:
                        break;
                      kbESC:
                        begin
                          ColWidth[CurCol] := SaveCellWidth;
                          DrawView;
                          break;
                        end;
                    end {case};
                  Application^.Idle;
                end;
              CE;
            end;
          cmChangeFormat:
            begin
              ChangeFormat;
              CE;
            end;
          cmPaste:
            begin
              Paste;
              EndMarking
            end;
          cmCopy:
            begin
              Copy;
              EndMarking
            end;
          cmCut:
            begin
              Copy;
              Clear;
              EndMarking
            end;
          cmClear:
            begin
              Clear;
              EndMarking
            end;
          cmInsertLine:
            begin
              InsertLine;
              EndMarking
            end;
          cmInsertColumn:
            begin
              InsertCol;
              EndMarking
            end;
          cmDeleteLine:
            begin
              DeleteLine;
              EndMarking
            end;
          cmDeleteColumn:
            begin
              DeleteCol;
              EndMarking
            end;
          cmToggleShowMode:
            begin
              ShowSeparators := not ShowSeparators;
              DrawView;
              CE;
            end;
          cmRecalc:
            begin
              ReCalc(True);
              DrawView;
              CE
            end;
          cmGotoCell:
            begin
              if HistoryCount(hsGotoCell) = 0 then
                s := 'A1'
              else
                s := HistoryStr(hsGotoCell, 0);
              if ExecResource(dlgGotoCellNumber, s) = cmOK
              then
                GotoCell(s);
              ClearEvent(Event);
              exit
            end;
          cmFindCell:
            begin
              if ExecResource(dlgFindCell, SearchData) = cmOK then
                begin
                  SearchPos.X := -1;
                  SearchPos.Y := 0;
                  WasReplace := False;
                  SearchCell;
                end;
              CE;
            end;
          cmSearchAgain:
            begin
              SearchCanceled := False;
              SearchPos.X := CurCol;
              SearchPos.Y := Delta.Y+Cur.Y;
              repeat
                SearchCell
              until not WasReplace or not ContSearch or
                SearchCanceled;
              CE;
            end;
          cmReplaceCell:
            begin
              l := ExecResource(dlgReplaceCell, ReplaceData);
              if (l = cmYes) or (l = cmOK) then
                begin
                  SearchData.s := ReplaceData.s;
                  SearchData.TxtOptions := ReplaceData.TxtOptions;
                  SearchData.CellOptions := ReplaceData.CellOptions;
                  SearchCanceled := False;
                  ContSearch := l = cmYes;
                  SearchPos.X := -1;
                  SearchPos.Y := 0;
                  WasReplace := True;
                  repeat
                    SearchCell
                  until not WasReplace or not ContSearch or
                    SearchCanceled;
                end;
              CE;
            end;
          cmMainMenu:
            begin
              Message(DNApp.MenuBar, evCommand, cmMenu, nil);
              CE;
            end;
        end {case};
      evKeyDown:
        case Event.KeyCode of
          kbDoubleAlt, kbDoubleCtrl:
            CE;
          kbDel:
            begin
              Clear;
              EndMarking
            end;
          kbShiftLeft,
          kbLeft:
            if Cur.X > 0 then
              begin
                CheckMark;
                Dec(Cur.X);
                SetMark;
                DrawView;
                CE;
                exit
              end
            else
              exit;
          kbShiftRight,
          kbRight:
            if (Cur.X < NumC) then
              begin
                CheckMark;
                if (CurCol = HScroll^.Max) then
                  begin
                    if Cur.X > 0 then
                      Dec(Cur.X);
                    SetMark;
                    exit
                  end;
                Inc(Cur.X);
                SetMark;
                DrawView;
                ClearEvent(Event);
                exit
              end
            else if (CurCol = HScroll^.Max) then
              begin
                CheckMark;
                if Cur.X > 0 then
                  Dec(Cur.X);
                SetMark;
                exit
              end
            else
              exit;
          kbShiftUp, kbUp:
            if Cur.Y > 0 then
              begin
                CheckMark;
                Dec(Cur.Y);
                SetMark;
                DrawView;
                ClearEvent(Event);
                exit
              end
            else
              exit;
          kbShiftHome, kbHome:
            begin
              CheckMark;
              Cur.Y := 0;
              Cur.X := 0;
              SetMark;
              HScroll^.SetValue(0);
              VScroll^.SetValue(0);
              DrawView;
              ClearEvent(Event);
              exit
            end;
          kbShiftEnd, kbEnd:
            begin
              CheckMark;
              MaxX := 0;
              MaxY := 0;
              Cur.Y := 0;
              Cur.X := 0;
              for l := 1 to Cells^.Count do
                begin
                  P := Cells^.At(l-1);
                  if P^.Col > MaxX then
                    MaxX := P^.Col;
                end;
              if Cells^.Count <> 0 then
                MaxY := P^.Row;
              if (l+ColWidth[CurCol] < Size.X) and (MaxX > 0) then
                begin
                  l := 7;
                  while (l <= Size.X) and (MaxX >= 0) do
                    begin
                      k := ColWidth[CurCol];
                      if MaxX >= 0 then
                        begin
                          Inc(Cur.X);
                          Dec(MaxX)
                        end;
                      Inc(l, k);
                    end;
                  Dec(Cur.X);
                  Inc(MaxX);
                end;
              VScroll^.SetValue(MaxY-Size.Y+3);
              Delta.Y := VScroll^.Value;
              Cur.Y := MaxY-Delta.Y;
              HScroll^.SetValue(MaxX);
              Delta.X := HScroll^.Value;
              SetMark;
              DrawView;
              ClearEvent(Event);
              exit
            end;
          kbShiftDown, kbDown:
            if (Cur.Y < Size.Y-3) then
              begin
                CheckMark;
                if (Delta.Y+Cur.Y >= VScroll^.Max) then
                  begin
                    if Cur.Y > 0 then
                      Dec(Cur.Y);
                    SetMark;
                    exit
                  end;
                Inc(Cur.Y);
                SetMark;
                DrawView;
                ClearEvent(Event);
                exit
              end
            else if (Delta.Y+Cur.Y >= VScroll^.Max) then
              begin
                if Cur.Y > 0 then
                  Dec(Cur.Y);
                SetMark;
                exit
              end
            else
              exit;
          kbEnter:
            begin
              Marking := False;
              SetMark; //DrawView;
              PWindow(Owner)^.SelectNext(False);
              ClearEvent(Event);
            end;
          kbTab:
            begin
              Marking := False;
              SetMark;
              DrawView;
            end;
          kbAltLeft:
            ShrinkCol(CurCol);
          kbAltRight:
            ExpandCol(CurCol);
          else
            if Event.CharCode > #31 then
              begin
                Marking := False;
                SetMark;
                DrawView;
                PWindow(Owner)^.SelectNext(True);
                Event.InfoPtr := CalcInput;
                CalcInput^.PutEvent(Event);
                ClearEvent(Event)
              end;
        end {case};
      evBroadcast:
        case Event.Command of
          cmScrollBarChanged:
            if Event.InfoPtr = HScroll then
              begin
                CheckMark;
                Delta.X := HScroll^.Value;
                SetMark;
                DrawView;
                exit
              end
            else if Event.InfoPtr = VScroll then
              begin
                CheckMark;
                Delta.Y := VScroll^.Value;
                SetMark;
                DrawView;
                exit
              end
        end {case};
    end {case};
    Marking := False;
  end { TCalcView.HandleEvent };
{-DataCompBoy-}

function TCalcView.GetCellValue(const s: String): boolean;
  var
    i: integer;
    SR: TCellSearcRec;
  begin
    Result := False;
    // DelSpaces(S); UpStr(S);
    if GetCellCoord(s, SR.Col, SR.Row) then
      begin
        if not Cells^.Search(@SR, i) then
          begin
            Res := 0;
            Result := True;
          end
        else
          with PCellrec(Cells^.At(i))^ do
            if Options and 3 <> 0 then
              begin
                Res := Value;
                Result := True;
              end;
      end;
  end { TCalcView.GetCellValue };

{  Вычисление функций SUM и MUL. В скобках через запятую могут быть
диапазоны, ячейки и числа. Пробелы игнорируются. Наличие закрывающей
скобки должно быть проверено перед вызовом.
   Пример: =SUM(A 1:B7, C1 8, 1).
   Вычисленное число кладется в Calculat.Res}
function TCalcView.GetFuncValue(s: String): boolean;
  procedure DoSum(PP: PCellrec);
    begin
      with PP^ do
        if (Options and 3 <> 0) then
          Res := Res+PP^.Value
    end;

  procedure DoMul(PP: PCellrec);
    begin
      with PP^ do
        if (Options and 3 <> 0) then
          Res := Res*PP^.Value
    end;

  var
    i: integer;
    AFromX, AToX: byte;
    AFromY, AToY: AInt;
    Op: Pointer;
    SR: TCellSearcRec;
    t0, t: integer;
    s1: String;
    R: CReal;
  begin{TCalcView.GetFuncValue}
    Result := False;
    DelSpaces(s);
    UpStr(s);
    if (System.Copy(s, 1, 4) = 'SUM(') then
      begin
        Op := @DoSum;
        Res := 0;
      end
    else if (System.Copy(s, 1, 4) = 'MUL(') then
      begin
        Op := @DoMul;
        Res := 1;
      end
    else
      exit;
    t := 5;
    while t <= Length(s) do
      begin
        t0 := t;
        while not (s[t] in [',', ')']) do
          Inc(t);
        if t = t0 then
          exit;
        s1 := System.Copy(s, t0, t-t0);
        i := Pos(':', s1);
        if i <> 0 then
          begin
            if not GetCellCoord(System.Copy(s1, 1, i-1), AFromX,
                AFromY) or
              not GetCellCoord(System.Copy(s1, i+1, MaxStringLength),
                AToX, AToY)
            then
              exit;
            Cells^.ForRectangle(AFromX, AFromY, AToX, AToY, Op);
          end
        else
          begin
            if not GetCellCoord(s1, AFromX, AFromY) then
              exit;
            Cells^.ForRectangle(AFromX, AFromY, AFromX, AFromY, Op);
          end;
        Inc(t); { пропустить запятую или скобку }
      end;
    Result := True;
  end { TCalcView.GetFuncValue };

procedure TCalcView.CalcError;
  var
    l: integer;
    s: String;
    Err: String;
    C: word;
  begin
    s := GetString(Index)+' '+GetCellName(CurrentCalc.X, CurrentCalc.Y);
    if EvalueError then
      begin
        Err := GetErrOp(l);
        if (Err = '') and (CalcSym <> #3) then
          Err := ' '+CalcSym;
        s := s+^M^C+GetString(CalcErrMess)+Err;
        EvalueError := False;
      end;
    C := MessageBox(s, @Self, {mfYesNoCancel}mfYesButton+mfError);
  end;

destructor TCalcView.Done;
  begin
    if Cells <> nil then
      Dispose(Cells, Done);
    Cells := nil;
    if Q <> nil then
      Dispose(Q);
    Q := nil;
    DisposeStr(SName);
    TView.Done;
  end;

function TCalcView.AskSave;
  var
    A: word;
  begin
    AskSave := True;
    if not Modified then
      exit;
    A := MessageBox(GetString(dlWorkSheet)+' '+Cut(SName^, 40)+
      GetString(dlNotSaved),
    nil, mfWarning+mfYesNoCancel);
    if A <> cmCancel then
      Modified := False;
    if A = cmYes then
      SaveSheet
    else
      AskSave := A <> cmCancel;
  end;

{-DataCompBoy-}
procedure TCalcView.LoadSheet;
  var
    s: PStream;
    Bounds: TRect;
  begin
    GetBounds(Bounds);
    if Modified and not AskSave then
      exit;
    if Cells <> nil then
      Dispose(Cells, Done);
    Cells := nil;
    if (FName = '') or (FName = UntitledName) then
      begin
        DisposeStr(SName);
        SName := NewStr(UntitledName);
        if Owner <> nil then
          begin
            DisposeStr(PWindow(Owner)^.Title);
            PWindow(Owner)^.Title := NewStr(Cut(SName^, Bounds.B.X-
              Bounds.A.X-12));
            Owner^.Redraw;
          end;
        New(Cells, Init(10, 10));
        exit
      end;
    {$IFDEF OS_DOS}
    FName := lfGetLongFileName(lFExpand(FName));
    {$ELSE}
    FName := lFExpand(FName);
    {$ENDIF}
    s := New(PBufStream, Init(FName, stOpenRead, 2048));
    DisposeStr(SName);
    SName := NewStr(FName);
    if Owner <> nil then
      begin
        DisposeStr(PWindow(Owner)^.Title);
        PWindow(Owner)^.Title := NewStr(Cut(SName^, Bounds.B.X-
          Bounds.A.X-12));
      end;
    if s^.Status <> stOK then
      begin
        Dispose(s, Done);
        New(Cells, Init(10, 10));
        FillChar(ColWidth, SizeOf(ColWidth), DefaultColWidth);
        //   Owner^.Redraw;
        exit
      end;
    s^.Read(ColWidth, SizeOf(ColWidth));
    { Cells := PCellCollection(S^.Get);}
    New(Cells, ShortLoad(s^));
    Dispose(s, Done);
    if Cells = nil then
      begin
        ErrMsg(erInvalidFileFormat);
        DisposeStr(SName);
        SName := NewStr(UntitledName);
        if Owner <> nil then
          begin
            DisposeStr(PWindow(Owner)^.Title);
            PWindow(Owner)^.Title := NewStr(Cut(SName^, Bounds.B.X-
              Bounds.A.X-12));
          end;
        New(Cells, Init(10, 10));
        FillChar(ColWidth, SizeOf(ColWidth), 1);
      end;
    Modified := False;
    ReCalc(True);
  end { TCalcView.LoadSheet };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TCalcView.SaveSheetAs;
  var
    s: PStream;
    FName: String;
    W: word;
    PP: Pointer;
    Bounds: TRect;
  begin
    GetBounds(Bounds);
    PP := @FName;
    if GetFileName(FName, '*.WKZ', GetString(dlSaveFileAs),
      GetString(dlSaveFileAs), fdOKButton {, hsSaveSheetAs, 0}) =
        cmCancel
    then
      exit;
    {$IFDEF OS_DOS}
    FName := lfGetLongFileName(lFExpand(FName));
    {$ELSE}
    FName := lFExpand(FName);
    {$ENDIF}
    s := New(PDosStream, Init(FName, stOpen));
    W := s^.Status;
    Dispose(s, Done);
    if W = 0 then
      begin
        if Msg(dlFileExist, @PP, mfYesButton+mfNoButton+mfWarning) <>
            cmYes
        then
          exit;
      end;
    DisposeStr(SName);
    SName := NewStr(FName);
    if Owner <> nil then
      begin
        DisposeStr(PWindow(Owner)^.Title);
        PWindow(Owner)^.Title := NewStr(Cut(SName^, Bounds.B.X-
          Bounds.A.X-12));
        Owner^.Redraw;
      end;
    {PInfoView(CellInfo)^.SetInfo(' WORK ', Owner^.GetColor(12));}
    s := New(PBufStream, Init(FName, stCreate, 2048));
    s^.Write(ColWidth, SizeOf(ColWidth));
    { S^.Put(Cells);}
    Cells^.ShortStore(s^);
    if s^.Status <> stOK then
      begin
        Msg(dlCanNotWrite, @PP, mfError+mfOKButton);
      end;
    Modified := False;
    Dispose(s, Done);
    FileChanged(FName);
  end { TCalcView.SaveSheetAs };
{-DataCompBoy-}

procedure TCalcView.SaveSheet;
  var
    s: PStream;
  begin
    if SName^ = UntitledName then
      begin
        SaveSheetAs;
        exit
      end;
    {PInfoView(CellInfo)^.SetInfo(' WORK ', Owner^.GetColor(12));}
    s := New(PBufStream, Init(SName^, stCreate, 2048));
    s^.Write(ColWidth, SizeOf(ColWidth));
    {S^.Put(Cells);}
    Cells^.ShortStore(s^);
    if s^.Status <> stOK then
      Msg(dlCanNotWrite, SName, mfError+mfOKButton)
    else
      Modified := False;
    Dispose(s, Done);
    FileChanged(SName^);
  end { TCalcView.SaveSheet };

procedure TCalcView.Copy;
  var
    X1, Y1, X2, Y2: integer;
    Wrk: integer;

  procedure ToClip(P: PCellrec);
    begin
      with CellClipboard^.ReplaceItem(P^.Col, P^.Row, P^.s)^ do
        begin
          Options := P^.Options;
          Value := P^.Value;
          Decimals := P^.Decimals;
        end;
    end;

  begin
    X1 := Delta.X+Cur.X;
    X2 := Mark.X;
    Y1 := Delta.Y+Cur.Y;
    Y2 := Mark.Y;
    if X1 > X2 then
      begin
        Wrk := X1;
        X1 := X2;
        X2 := Wrk;
      end;
    if Y1 > Y2 then
      begin
        Wrk := Y1;
        Y1 := Y2;
        Y2 := Wrk;
      end;
    if CellClipboard <> nil then
      Dispose(CellClipboard, Done);
    CellClipboard := nil;
    New(CellClipboard, Init(10, 10));
    Cells^.ForRectangle(X1, Y1, X2, Y2, @ToClip);
    ClipRect.A.X := X1;
    ClipRect.A.Y := Y1;
    ClipRect.B.X := X2;
    ClipRect.B.Y := Y2;
  end { TCalcView.Copy };

{В формуле S ссылки на ячейки (>=LX,>=LY) сдвигать на (DX,DY) }
function ReformFormula(const s: String; LX, LY, DX, DY: integer):
    String;
  const
    Signs = [';', '[', ']', '{', '}', #39, ':', '"', '.', '<',
    '>', ',', '/', '?', '\', '-', '=', '|', '_', '(',
    ')', '*', '&', '^', '%', '$', '!', '~', '+', ' '];
  var
    s1, s2: String;
    X, X1: byte;
    i, l: integer;
    Y, Y1: AInt;
  label EndRowName;
  begin
    Result := '=';
    i := 2;
    while (i <= Length(s)) do
      begin
        s1 := '';
        s2 := '';
        while (i <= Length(s)) and (s[i] in Signs) do
          begin
            Result := Result+s[i];
            Inc(i)
          end;
        while (i <= Length(s)) and not (s[i] in Signs) do
          begin
            s1 := s1+s[i];
            Inc(i)
          end;
        if GetCellCoord(s1, X, Y) then
          if (X >= LX) and (Y >= LY) then
            begin
              if s1[1] = '@' then
                s2 := '@'+GetColName(X)
              else if (X+DX < 0) or (X+DX > 255) then
                s2 := '?'
              else
                s2 := GetColName(X+DX);
              { сейчас S2 - имя колонки}

              for l := 2 to Length(s1)-1 do
                if s1[l] = '@' then
                  begin
                    s1 := '@'+GetRowName(Y);
                    goto EndRowName;
                  end;
              if (Y+DY < 0) or (Y+DY > MaxCellY) then
                s1 := '?'
              else
                s1 := GetRowName(Y+DY);
            end;
EndRowName: { сейчас S1 - имя строки }

        Result := Result+s2+s1;
      end;
  end { ReformFormula };

procedure TCalcView.Paste;
  var
    i, j: integer;

  procedure RewriteFormulaClip(P: PCellrec; DX, DY: integer);
    var
      s: String;
    begin
      if (P^.Options and coFormula) <> 0 then
        s := ReformFormula(P^.s, 0, 0, DX, DY)
      else
        s := P^.s;
      with Cells^.ReplaceItem(P^.Col+DX, P^.Row+DY, s)^ do
        begin
          Options := P^.Options;
          Value := P^.Value;
          Decimals := P^.Decimals;
        end;
    end;

  begin
    if CellClipboard = nil then
      exit;
    Mark.X := Delta.X+Cur.X+ClipRect.B.X-ClipRect.A.X;
    Mark.Y := Delta.Y+Cur.Y+ClipRect.B.Y-ClipRect.A.Y;
    Clear;
    for i := 1 to CellClipboard^.Count do
      RewriteFormulaClip(CellClipboard^.At(i-1),
      -ClipRect.A.X+Delta.X+Cur.X,
      -ClipRect.A.Y+Delta.Y+Cur.Y);
    ReCalc(False);
    Modified := True;
  end { TCalcView.Paste };

procedure TCalcView.Clear;
  var
    X1, X2, Y1, Y2, k, l: integer;
    Wrk: integer;
  begin
    X1 := Delta.X+Cur.X;
    X2 := Mark.X;
    Y1 := Delta.Y+Cur.Y;
    Y2 := Mark.Y;
    if X1 > X2 then
      begin
        Wrk := X1;
        X1 := X2;
        X2 := Wrk;
      end;
    if Y1 > Y2 then
      begin
        Wrk := Y1;
        Y1 := Y2;
        Y2 := Wrk;
      end;
    for k := X1 to X2 do
      for l := Y1 to Y2 do
        Cells^.DelItem(k, l);
    ReCalc(False);
    Modified := True;
  end { TCalcView.Clear };

procedure TCalcView.RewriteFormula(var P: PCellrec; LX, LY, DX, DY:
    integer);
  var
    s: String;
    o, D: integer;
    V: CReal;
  begin
    if (P^.Options and coFormula) = 0 then
      exit;
    s := ReformFormula(P^.s, LX, LY, DX, DY);
    o := P^.Options;
    D := P^.Decimals;
    V := P^.Value;
    P := Cells^.ReplaceItem(P^.Col, P^.Row, s);
    with P^ do
      begin
        Options := o;
        Decimals := D;
        Value := V
      end;
  end { TCalcView.RewriteFormula };

procedure TCalcView.InsertLine;
  var
    i, l: integer;
    P: PCellrec;
  begin
    l := Delta.Y+Cur.Y;
    for i := 1 to Cells^.Count do
      begin
        P := Cells^.At(i-1);
        if P^.Row >= VScroll^.Max then
          Cells^.AtFree(i-1)
        else
          begin
            if P^.Row >= l then
              Inc(P^.Row);
            RewriteFormula(P, 0, l, 0, 1);
          end;
      end;
    ReCalc(False);
    Modified := True;
  end { TCalcView.InsertLine };

procedure TCalcView.DeleteLine;
  var
    i, l: integer;
    P: PCellrec;
  begin
    l := Delta.Y+Cur.Y+1;
    for i := 1 to Cells^.Count do
      begin
        P := Cells^.At(i-1);
        RewriteFormula(P, 0, l, 0, -1);
        if P^.Row = l-1 then
          begin
            Cells^.FreeItem(P);
            Cells^.AtPut(i-1, nil)
          end
        else if P^.Row >= l then
          Dec(P^.Row);
      end;
    Cells^.Pack;
    ReCalc(False);
    Modified := True;
  end { TCalcView.DeleteLine };

procedure TCalcView.InsertCol;
  var
    i, j, l: integer;
    P: PCellrec;
  begin
    l := Delta.X+Cur.X;
    for i := 1 to Cells^.Count do
      begin
        P := Cells^.At(i-1);
        if P^.Col = HScroll^.Max then
          Cells^.AtFree(i-1)
        else
          begin
            if P^.Col >= l then
              Inc(P^.Col);
            RewriteFormula(P, l, 0, 1, 0);
          end;
      end;
    Move(ColWidth[l], ColWidth[l+1],
    SizeOf(ColWidth[0])*(High(ColWidth)-l));
    ColWidth[l] := DefaultColWidth;
    ReCalc(False);
    Modified := True;
  end { TCalcView.InsertCol };

procedure TCalcView.DeleteCol;
  var
    i, j, l: integer;
    P: PCellrec;
  begin
    l := Delta.X+Cur.X+1;
    for i := 1 to Cells^.Count do
      begin
        P := Cells^.At(i-1);
        if P^.Col = l-1 then
          begin
            Cells^.FreeItem(P);
            Cells^.AtPut(i-1, nil)
          end
        else
          begin
            if P^.Col >= l then
              Dec(P^.Col);
            RewriteFormula(P, l, 0, -1, 0);
          end;
      end;
    Move(ColWidth[l], ColWidth[l-1],
    SizeOf(ColWidth[0])*(High(ColWidth)-l+1));
    ColWidth[High(ColWidth)] := DefaultColWidth;
    Cells^.Pack;
    ReCalc(False);
    Modified := True;
  end { TCalcView.DeleteCol };

{ вычисление формулы; s[1] = '=' }
function TCalcView.CalcEval(const s: String; var Value: CReal):
    boolean;
  var
    R: CReal;
  begin
    R := Evalue(System.Copy(s, 2, MaxStringLength), @self);
    if EvalueError then
      begin
        CalcError(erInvalidFormula);
        Value := 0;
        CalcEval := False;
      end
    else
      begin
        Value := R;
        CalcEval := True;
      end;
  end;

procedure TCalcView.ReCalc(Full: boolean);
  var
    StartRecalc: integer;
    ierr: integer;
  begin
    if not Cells^.TSort(StartRecalc) then
      begin
        with PCellrec(Cells^.At(StartRecalc-1)) do
          begin
            CurrentCalc.X := Col;
            CurrentCalc.Y := Row;
          end;
        CalcError(erRecurseTooDeep);
        exit;
      end;
    while StartRecalc <> 0 do
      begin
        with PCellrec(Cells^.At(StartRecalc-1))^ do
          begin
            CurrentCalc.X := Col;
            CurrentCalc.Y := Row;
            if (Options and coFormula) <> 0 then
              begin
                if not CalcEval(s, Value) then
                  exit;
              end
            else if Full and ((Options and coValue) <> 0) then
              if s[1] = '=' then
                begin
                    {После некоторых старых версий DN бывает флаг coValue
          без флага coFormula для формул, включающих только константные
          операнды. То, что TSort не воспринимал их, как формулы, не
          страшно, так как от других ячеек они не зависят. }
                  Value := Evalue(System.Copy(s, 2, 255), @self);
                  Options := Options or coValue;
                end
              else
                begin
                  Val(s, Value, ierr);
                  if ierr <> 0 then
                    exit;
                end;
            StartRecalc := NextC;
          end;
      end;
  end { TCalcView.ReCalc };

procedure TCalcView.GotoCell;
  var
    X: byte;
    Y: AInt;
    l: integer;
  begin
    if not GetCellCoord(Cell, X, Y) then
      begin
        ErrMsg(erGotoInvalidNumber);
        exit;
      end;
    Marking := False;
    BlockDraw := True;
    Delta.X := X;
    Delta.Y := Y-(Size.Y-2) div 2;
    VScroll^.SetValue(Delta.Y);
    Delta.Y := VScroll^.Value;
    Cur.X := 0;
    Cur.Y := Y-Delta.Y;
    l := 0;
    repeat
      Inc(l, ColWidth[Cur.X]);
      if Delta.X > 0 then
        begin
          Inc(Cur.X);
          Dec(Delta.X)
        end;
    until (Delta.X = 0) or (l > Size.X);
    if l > Size.X div 2 then
      begin
        Dec(Cur.X);
        Inc(Delta.X)
      end;
    HScroll^.SetValue(Delta.X);
    BlockDraw := False;
    Mark.X := Cur.X+Delta.X;
    Mark.Y := Cur.Y+Delta.Y;
    DrawView;
  end { TCalcView.GotoCell };

procedure TCalcView.SearchCell;
  const
    Signs = [#0..#128]-['A'..'Z', 'a'..'z', '0'..'9', '_'];
  var
    P: PCellrec;
    i, j: integer;
    V: CReal;
    k, l: byte;
    s, s1: String;

  function Found: boolean;
    begin
      Found := True;
      SearchPos.Y := P^.Row;
      SearchPos.X := P^.Col;
      GotoCell(GetCellName(P^.Col, P^.Row));
      if WasReplace then
        begin
          if (SearchData.TxtOptions and 4 <> 0) then
            if (SearchData.CellOptions = 0) then
              begin
                s := P^.s;
                Delete(s, k+l, Length(s1));
                Insert(ReplaceData.s1, s, k+l);
                Cells^.ReplaceItem(P^.Col, P^.Row, s);
                DrawView;
                exit
              end
            else
              begin
                if (P^.Options and coFormula) <> 0 then
                  begin
                    Found := False;
                    exit
                  end;
                Val(ReplaceData.s1, V, j);
                Cells^.ReplaceItem(P^.Col, P^.Row, ReplaceData.s1)^.
                  Value := V;
                DrawView;
                exit
              end;
        end;
    end { Found: };

  procedure SearchSign;
    begin
      while (k <= Length(s)) and not (s[k] in Signs) do
        Inc(k);
      Inc(l, k);
      s := System.Copy(s, k, MaxStringLength);
    end;

  begin { TCalcView.SearchCell }
    if SearchData.s = '' then
      exit;
    Val(SearchData.s, V, i);
    if SearchData.TxtOptions and 1 = 0 then
      s1 := UpStrg(SearchData.s)
    else
      s1 := SearchData.s;
    for i := 1 to Cells^.Count do
      begin
        P := Cells^.At(i-1);
        if (P^.Row >= SearchPos.Y) and (P^.Col > SearchPos.X) then
          if (SearchData.CellOptions = 0) then
            begin
              if SearchData.TxtOptions and 1 = 0 then
                s := UpStrg(P^.s)
              else
                s := P^.s;
              k := Pos(s1, s);
              l := 0;
              if SearchData.TxtOptions and 2 = 0 then
                begin
                  if k > 0 then
                    begin
                      if Found then
                        exit
                    end
                end
              else
                repeat
                  k := Pos(s1, s);
                  l := 0;
                  if k = 0 then
                  else if (k = 1) or (Length(s) = Length(s1)) then
                    begin
                      if (Length(s) = Length(s1)) or (Length(s) <>
                          Length(s1))
                        and (s[Length(s1)+1] in Signs)
                      then
                        begin
                          if Found then
                            exit
                        end
                      else
                        SearchSign;
                    end
                  else if (k = Length(s)-Length(s1)+1) then
                    begin
                      if (s[k-1] in Signs) then
                        begin
                          if Found then
                            exit
                        end
                      else
                        SearchSign;
                    end
                  else if (s[k-1] in Signs) and (s[Length(s1)+k] in
                      Signs)
                  then
                    begin
                      if Found then
                        exit
                    end
                  else
                    SearchSign;
                until (k = 0) or (Length(s) < Length(s1));
            end
          else if (P^.Options and 3 <> 0) and (V = P^.Value) then
            begin
              if Found then
                exit
            end;
      end;
    if not ContSearch and not WasReplace then
      ErrMsg(erTextNotFound);
    SearchCanceled := True;
  end { TCalcView.SearchCell };

{------------------------------------------------------------------------------}
{ Write dBase Dbf }

procedure TDbfFieldCollection.FreeItem(Item: Pointer); {KV}
  begin
    if Item <> nil then
      Dispose(PDBFField(Item));
  end;

Constructor TDbaseWriter.Init(FileName: FNameStr; Mode: word; Size:
    SW_Word); {KV}
  begin
    inherited Init(FileName, Mode, Size);
    CurRecord := 0;
    EofFlag := False;
    New(Fields, Init(10, 10));
    FillChar(Header, SizeOf(Header), 0);
    Header.DBFIdent := #3;
    if (Mode = stOpen) or (Mode = stOpenRead) then
      ReadFile;
  end;

destructor TDbaseWriter.Done; {KV}
  begin
    if Fields <> nil then
      Dispose(Fields, Done);
    Fields := nil;
    inherited Done;
  end;

procedure TDbaseWriter.AddField(const NameField: String; TypeField:
    Char;
  LenField: integer; DecField: integer); {KV}
  var
    P: PDBFField;
  begin
    New(P);
    FillChar(P^, SizeOf(TDBFField), 0);
    StrPCopy(P^.FieldName, System.Copy(NameField, 1, 10));
    P^.FieldType := TypeField;
    if TypeField = 'C' then
      P^.FLength.FieldLength := LenField
    else
      begin
        P^.FLength.NumericLength := LenField;
        P^.FLength.Decimals := DecField;
      end;
    Fields^.Insert(P);
  end;

procedure TDbaseWriter.AddRecord; {KV}
  var
    Buf: PChar;
  begin
    if Status <> stOK then
      exit;
    with Header do
      begin
        inherited Seek(DataOffset+RecSize*LastRecord);
        GetMem(Buf, RecSize);
        FillChar(Buf^, RecSize, ' ');
        inherited Write(Buf^, RecSize);
        FreeMem(Buf, RecSize);
        if Status = stOK then
          begin
            Inc(LastRecord);
            inherited Seek(0);
            inherited Write(Header, SizeOf(Header));
          end;
        CurRecord := LastRecord;
        EofFlag := False;
      end;
  end { TDbaseWriter.AddRecord };

procedure TDbaseWriter.CreateFile; {KV}
  var
    i: integer;
  begin
    Header.RecSize := 1;
    for i := 0 to Fields^.Count-1 do
      begin
        with PDBFField(Fields^.At(i))^ do
          begin
            if FieldType = 'C' then
              Inc(Header.RecSize, FLength.FieldLength)
            else
              Inc(Header.RecSize, FLength.NumericLength);
          end;
      end;
    Header.DataOffset := SizeOf(THeaderDBF)+(Fields^.Count)*SizeOf(
      TDBFField)+1;
    inherited Seek(0);
    inherited Write(Header, SizeOf(Header));
    for i := 0 to Fields^.Count-1 do
      begin
        inherited Write(PDBFField(Fields^.At(i))^, SizeOf(TDBFField));
      end;
    i := $1A0D;
    inherited Write(i, 2);
  end { TDbaseWriter.CreateFile };

procedure TDbaseWriter.DeleteRecord; {KV}
  var
    Ch: Char;
  begin
    if (CurRecord < 1) or (CurRecord > Header.LastRecord) then
      exit;
    inherited Seek(FieldOffsetInFile(1)-1);
    Ch := #$2A;
    inherited Write(Ch, 1);
  end;

function TDbaseWriter.FCount: integer;
  begin
    FCount := Fields^.Count;
  end;

function TDbaseWriter.FieldAsString(FieldIndex: integer): String;
    {KV}
  var
    Buf: array[0..2047] of Char;
    i: integer;
  begin
    inherited Seek(FieldOffsetInFile(FieldIndex));
    FillChar(Buf, SizeOf(Buf), ' ');
    inherited Read(Buf, FieldLen(FieldIndex));
    i := FieldLen(FieldIndex)-1;
    while (i >= 0) and (Buf[i] = ' ') do
      Dec(i);
    Buf[i+1] := #0;
    FieldAsString := StrPas(Buf);
  end;

function TDbaseWriter.FieldAsInteger(FieldIndex: integer): integer;
    {KV}
  var
    s: String;
    Value: integer;
    i: integer;
  begin
    s := FieldAsString(FieldIndex);
    if s = '' then
      Value := 0
    else
      begin
        Val(s, Value, i);
        if i <> 0 then
          Value := 0;
      end;
    FieldAsInteger := Value;
  end;

function TDbaseWriter.FieldAsFloat(FieldIndex: integer): Double;
    {KV}
  var
    s: String;
    Value: Double;
    i: integer;
  begin
    s := FieldAsString(FieldIndex);
    if s = '' then
      Value := 0.0
    else
      begin
        Val(s, Value, i);
        if i <> 0 then
          Value := 0.0;
      end;
    FieldAsFloat := Value;
  end;

procedure TDbaseWriter.FieldPutString(FieldIndex: integer;
    FieldValue: String); {KV}
  var
    P: PDBFField;
    Buf: array[0..2047] of Char;
    l: AWord;
    StrLen: integer;
  begin
    if (FieldIndex < 1) or (FieldIndex > Fields^.Count) then
      exit;
    P := PDBFField(Fields^.At(FieldIndex-1));
    FillChar(Buf, SizeOf(Buf), ' ');
    l := FieldLen(FieldIndex);
    if l > 2047 then
      l := 2047;
    StrLen := Length(FieldValue);
    if StrLen > l then
      begin
        StrLen := l;
        FieldValue := System.Copy(FieldValue, 1, StrLen);
      end;
    StrPCopy(Buf, FieldValue); // Можно было использовать Move,
    // но при работе с длинными строками
    // так безопаснее
    Buf[StrLen] := ' ';
      // Для удаления #0, который вставляет StrPCopy
    inherited Seek(FieldOffsetInFile(FieldIndex));
    inherited Write(Buf, l);
  end { TDbaseWriter.FieldPutString };

procedure TDbaseWriter.FieldPutInteger(FieldIndex: integer;
    FieldValue: integer); {KV}
  var
    s: String;
    l: AWord;
    D: byte;
    R: AWord;
  begin
    if (FieldIndex < 1) or (FieldIndex > Fields^.Count) then
      exit;
    l := FieldLen(FieldIndex);
    D := FieldDec(FieldIndex);
    if D > 0 then
      R := l-D-1
    else
      R := l;
    Str(FieldValue: l, s);
    if D > 0 then
      begin
        s := s+'.';
        Inc(R);
        while R < l do
          begin
            s := s+'0';
            Inc(R);
          end;
      end;
    FieldPutString(FieldIndex, s);
  end { TDbaseWriter.FieldPutInteger };

procedure TDbaseWriter.FieldPutFloat(FieldIndex: integer; FieldValue:
    Double); {KV}
  var
    s: String;
  begin
    if (FieldIndex < 1) or (FieldIndex > Fields^.Count) then
      exit;
    Str(FieldValue: FieldLen(FieldIndex): FieldDec(FieldIndex), s);
    FieldPutString(FieldIndex, s);
  end;

function TDbaseWriter.FieldName(FieldIndex: integer): String; {KV}
  begin
    FieldName := StrPas(PDBFField(Fields^.At(FieldIndex-1))^.
      FieldName);
  end;

function TDbaseWriter.FieldLen(FieldIndex: integer): AWord; {KV}
  begin
    with PDBFField(Fields^.At(FieldIndex-1))^ do
      begin
        if FieldType = 'C' then
          FieldLen := FLength.FieldLength
        else
          FieldLen := FLength.NumericLength;
      end;
  end;

function TDbaseWriter.FieldDec(FieldIndex: integer): byte; {KV}
  begin
    with PDBFField(Fields^.At(FieldIndex-1))^ do
      begin
        if FieldType = 'C' then
          FieldDec := 0
        else
          FieldDec := FLength.Decimals;
      end;
  end;

function TDbaseWriter.FieldPos(NameField: String): integer; {KV}
  var
    FieldIndex, i: integer;
  begin
    UpStr(NameField);
    FieldIndex := 0;
    for i := 0 to Fields^.Count-1 do
      begin
        if UpStrg(StrPas(PDBFField(Fields^.At(i))^.FieldName)) =
            NameField
        then
          begin
            FieldIndex := i+1;
            break;
          end;
      end;
    FieldPos := FieldIndex;
  end;

function TDbaseWriter.FieldType(FieldIndex: integer): Char; {KV}
  begin
    FieldType := PDBFField(Fields^.At(FieldIndex-1))^.FieldType;
  end;

function TDbaseWriter.FieldOffsetInBuffer(FieldIndex: integer):
    integer; {KV}
  var
    Offset: integer;
    i: integer;
  begin
    Offset := 1;
    for i := 0 to FieldIndex-2 do
      begin
        with PDBFField(Fields^.At(i))^ do
          begin
            if FieldType = 'C' then
              Inc(Offset, FLength.FieldLength)
            else
              Inc(Offset, FLength.NumericLength);
          end;
      end;
    FieldOffsetInBuffer := Offset;
  end;

function TDbaseWriter.FieldOffsetInFile(FieldIndex: integer):
    longInt; {KV}
  begin
    FieldOffsetInFile := Header.DataOffset+(CurRecord-1)*Header.
      RecSize+
    FieldOffsetInBuffer(FieldIndex);
  end;

procedure TDbaseWriter.First; {KV}
  begin
    CurRecord := 1;
    EofFlag := (Header.LastRecord = 0);
  end;

procedure TDbaseWriter.DBGoTo(NewRecord: integer); {KV}
  begin
    CurRecord := NewRecord;
    EofFlag := (Header.LastRecord = 0) or (Header.LastRecord <
      CurRecord);
  end;

procedure TDbaseWriter.Next; {KV}
  begin
    if CurRecord < Header.LastRecord then
      begin
        Inc(CurRecord);
        EofFlag := False;
      end
    else
      EofFlag := True;
  end;

procedure TDbaseWriter.ReadFile; {KV}
  var
    R: TDBFField;
    P: PDBFField;
  begin
    inherited Seek(0);
    FillChar(Header, SizeOf(Header), 0);
    Fields^.FreeAll;
    inherited Read(Header, SizeOf(Header));
    if Status <> stOK then
      exit;
    inherited Seek(SizeOf(Header));
    repeat
      inherited Read(R, 1);
      if R.FieldName[0] <> #$0D then
        begin
          inherited Read(R.FieldName[1], SizeOf(R)-1);
          New(P);
          Move(R, P^, SizeOf(R));
          Fields^.Insert(P);
        end;
    until (R.FieldName[0] = #$0D) or (Status <> stOK);
    EofFlag := (Header.LastRecord = 0);
    CurRecord := 1;
  end { TDbaseWriter.ReadFile };

procedure TDbaseWriter.RecallRecord; {KV}
  var
    Ch: Char;
  begin
    if (CurRecord < 1) or (CurRecord > Header.LastRecord) then
      exit;
    inherited Seek(FieldOffsetInFile(1)-1);
    Ch := #$20;
    inherited Write(Ch, 1);
  end;

procedure TDbaseWriter.WriteEndOfFile; {KV}
  var
    Ch: Char;
  begin
    Ch := #$1A;
    inherited Seek(Header.DataOffset+Header.LastRecord*Header.
      RecSize);
    inherited Write(Ch, 1);
  end;

{------------------------------------------------------------------------------}
{ Write Excel Xls }

type
  TXlsBOF = packed record
    recNo: AWord; // Record number
    RecSize: AWord; // Record size
    vers: AWord; // Version number (0500 for BIFF5 and BIFF7)
    DT: AWord; // Substream type: 0005h = Workbook globals
    //                 0006h = Visual Basic module
    //                 0010h = Worksheet or dialog sheet
    //                 0020h = Chart
    //                 0040h = Microsoft Excel 4.0 macro sheet
    //                 0100h = Workspace file
    rupBuild: AWord; // Build identifier (internal use only)
    rupYear: AWord; // Build year (internal use only)
    end;

  TXlsEOF = packed record
    recNo: AWord; // Record number
    res1: AWord;
    end;

  // LABEL: Cell Value, String Constant (204h)
  // A LABEL record describes a cell that contains a string constant.
  // The rw field contains the 0-based row number.
  // The col field contains the 0-based column number.
  // The string length is contained in the cch field and must be in the range of
  // 0000h..00FFh (0..255). The string itself is contained in the rgch field.
  TXlsLABEL = packed record
    recNo: AWord; // Record number
    RecSize: AWord; // Record size
    rw: AWord; // Cell Row
    Col: AWord; // Cell Col
    ixfe: AWord; // Index to the XF record
    cch: AWord; // Length of the string
    (* rgch: var *) // The string
    end;

  // BLANK: Cell Value, Blank Cell (201h)
  // A BLANK record describes an empty cell.
  // The rw field contains the 0-based row number.
  // The col field contains the 0-based column number.
  TXlsBLANK = packed record
    recNo: AWord; // Record number
    RecSize: AWord; // Record size
    rw: AWord; // Cell Row
    Col: AWord; // Cell Col
    ixfe: AWord; // Index to the XF record
    end;

  // NUMBER: Cell Value, Floating-Point Number (203h)
  // A NUMBER record describes a cell containing a constant floating-point number.
  // The rw field contains the 0-based row number.
  // The col field contains the 0-based column number.
  // The number is contained in the num field in 8-byte IEEE floating-point format.
  TXlsNUMBER = packed record
    recNo: AWord; // Record number
    RecSize: AWord; // Record size
    rw: AWord; // Cell Row
    Col: AWord; // Cell Col
    ixfe: AWord; // Index to the XF record
    Num: Double; // Floating-point number value
    end;

  // BOOLERR: Cell Value, Boolean or Error (205h)
  // A BOOLERR record describes a cell that contains a constant Boolean or error value.
  // The rw field contains the 0-based row number.
  // The col field contains the 0-based column number.
  TXlsBOOLERR = packed record
    recNo: AWord; // Record number
    RecSize: AWord; // Record size
    rw: AWord; // Cell Row
    Col: AWord; // Cell Col
    ixfe: AWord; // Index to the XF record
    bBoolErr: byte; // Boolean value or error value
    fError: byte; // Boolean/error flag
    end;

const
  CXlsBOF = $809;
  CXlsEOF = $0A;
  CXlsLABEL = $204;
  CXlsBLANK = $201;
  CXlsNUMBER = $203;
  CXlsBOOLERR = $205;

procedure TExcelWriter.WriteBLANK(const Col, Row, XF: AWord); {KV}
  var
    CDefXlsBLANK: TXlsBLANK;
  begin
    CDefXlsBLANK.recNo := CXlsBLANK;
    CDefXlsBLANK.RecSize := SizeOf(TXlsBLANK)-2*SizeOf(AWord);
    CDefXlsBLANK.rw := Row;
    CDefXlsBLANK.Col := Col;
    CDefXlsBLANK.ixfe := XF;
    inherited Write(CDefXlsBLANK, SizeOf(CDefXlsBLANK));
  end;

procedure TExcelWriter.WriteBOF; {KV}
  const
    CDefXlsBOF: TXlsBOF =
    (recNo: CXlsBOF;
    RecSize: SizeOf(TXlsBOF)-2*SizeOf(AWord);
    vers: 0;
    DT: $10;
    rupBuild: 0;
    rupYear: 0);
  begin
    inherited Write(CDefXlsBOF, SizeOf(CDefXlsBOF));
  end;

procedure TExcelWriter.WriteBOOL(const Data: boolean; const Col, Row, XF
    : AWord); {KV}
  const
    Values: array[boolean] of byte = (0, 1);
  var
    CDefXlsBOOLERR: TXlsBOOLERR;
  begin
    CDefXlsBOOLERR.recNo := CXlsBOOLERR;
    CDefXlsBOOLERR.RecSize := SizeOf(TXlsBOOLERR)-2*SizeOf(AWord);
    CDefXlsBOOLERR.rw := Row;
    CDefXlsBOOLERR.Col := Col;
    CDefXlsBOOLERR.ixfe := XF;
    CDefXlsBOOLERR.bBoolErr := Values[Data];
    CDefXlsBOOLERR.fError := 0;
    inherited Write(CDefXlsBOOLERR, SizeOf(CDefXlsBOOLERR));
  end;

procedure TExcelWriter.WriteEOF;
  const
    CDefXlsEOF: TXlsEOF =
    (recNo: CXlsEOF;
    res1: 0);
  begin
    inherited Write(CDefXlsEOF, SizeOf(CDefXlsEOF));
  end;

procedure TExcelWriter.WriteERROR(const Data: byte; const Col, Row, XF:
    AWord); {KV}
  var
    CDefXlsBOOLERR: TXlsBOOLERR;
  begin
    CDefXlsBOOLERR.recNo := CXlsBOOLERR;
    CDefXlsBOOLERR.RecSize := SizeOf(TXlsBOOLERR)-2*SizeOf(AWord);
    CDefXlsBOOLERR.rw := Row;
    CDefXlsBOOLERR.Col := Col;
    CDefXlsBOOLERR.ixfe := XF;
    CDefXlsBOOLERR.bBoolErr := Data;
    CDefXlsBOOLERR.fError := 1;
    inherited Write(CDefXlsBOOLERR, SizeOf(CDefXlsBOOLERR));
  end;

procedure TExcelWriter.WriteLABEL(const Data: String; const Col, Row, XF
    : AWord); {KV}
  var
    s: String;
    CDefXlsLABEL: TXlsLABEL;
  begin
    CDefXlsLABEL.recNo := CXlsLABEL;
    s := Data;
    CDefXlsLABEL.cch := Length(s);
    if CDefXlsLABEL.cch > 255 then
      begin
        CDefXlsLABEL.cch := 255;
        SetLength(s, CDefXlsLABEL.cch);
      end;
    CDefXlsLABEL.rw := Row;
    CDefXlsLABEL.Col := Col;
    CDefXlsLABEL.ixfe := XF;
    CDefXlsLABEL.RecSize := (SizeOf(TXlsLABEL)-2*SizeOf(AWord))+
      CDefXlsLABEL.cch;
    inherited Write(CDefXlsLABEL, SizeOf(CDefXlsLABEL));
    inherited Write(s[1], CDefXlsLABEL.cch);
  end { TExcelWriter.WriteLABEL };

procedure TExcelWriter.WriteNUMBER(const Data: Double; const Col,
    Row, XF: AWord); {KV}
  var
    CDefXlsNUMBER: TXlsNUMBER;
  begin
    CDefXlsNUMBER.recNo := CXlsNUMBER;
    CDefXlsNUMBER.RecSize := SizeOf(TXlsNUMBER)-2*SizeOf(AWord);
    CDefXlsNUMBER.rw := Row;
    CDefXlsNUMBER.Col := Col;
    CDefXlsNUMBER.ixfe := XF;
    CDefXlsNUMBER.Num := Data;
    inherited Write(CDefXlsNUMBER, SizeOf(CDefXlsNUMBER));
  end;

end.
