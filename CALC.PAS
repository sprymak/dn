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
  Advance, LFN, Objects, Views, CellsCol, Drivers, Dialogs, Messages,
  HistList, UniWin, Commands, DNHelp, RStrings, Calculat;

const
     CellClipboard : PCellCollection = nil;
     ClipRect      : record A, B: TPoint; end = (A:(X:0; Y:0); B:(X:0; Y:0));
     FalseStr      : String[10] = 'False';
     TrueStr       : String[10] = 'True';

type
    PCalcView = ^TCalcView;
    PCalcInput = ^TCalcInput;
    PInfoView = ^TInfoView;

    PCalcWindow = ^TCalcWindow;
    TCalcWindow = object(TUniWindow)
     CalcView: PCalcView;
     constructor Init(Bounds: TRect; AName: String); {DataCompBoy}
     constructor Load(var S: TStream);
     procedure Store(var S: TStream);
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
     NumC: Byte;
     Marking, BlockDraw, Modified: Boolean;
     ShowSeparators: boolean;
     ColWidth: Array[Byte] of Byte;
     CurrentCalc, SearchPos, ErrorCell: TPoint;
     SName: PString; {DataCompBoy}
     constructor Init(Bounds: TRect;
       AInfo: PCalcInput; ACellInfo: PInfoView;
       AHScroll, AVScroll: PScrollBar);
     destructor Done; virtual;

     constructor Load(var S: TStream);
     procedure Store(var S: TStream);

     procedure Draw; virtual;
     function  Valid(Command: Word): Boolean; virtual;
     procedure SetState(AState: Word; Enable: Boolean); virtual;
     function  GetPalette: PPalette; virtual;
     procedure HandleEvent(var Event: TEvent); virtual;
     Function  GetCellValue(const S : String) : boolean;
         {S - имя ячейки; результат - в calculat.Res}
     Function  GetFuncValue(S : String) : boolean;
         {S - функция SUM или MUL, результат - в calculat.Res}
     procedure CalcError( Index: TStrIdx );
     procedure LoadSheet(FName: String); {DataCompBoy}
     procedure SaveSheet;
     procedure SaveSheetAs;
     function  AskSave: Boolean;
     procedure Copy;
     procedure Paste;
     procedure Clear;
     function  CalcEval(const s: string; var Value: CReal): boolean;
     procedure ReCalc(Full: boolean);
     procedure InsertLine;
     procedure InsertCol;
     procedure DeleteLine;
     procedure DeleteCol;
     procedure GotoCell(Cell: String);
     procedure SearchCell;
     procedure RewriteFormula(var P: PCellrec; LX,LY, DX,DY: integer);
    end;

    TCalcInput = object(TInputLine)
     CalcView: PCalcView;
     constructor Load(var S: TStream);
     procedure Store(var S: TStream);
     function GetPalette: PPalette; virtual;
     procedure HandleEvent(var Event: TEvent); virtual;
    end;

    TInfoView = object(TView)
      InfoStr: String;
      InfoAttr: Byte;
      procedure SetInfo(S: String; Attr: Byte);
      procedure Draw; virtual;
    end;

function GetFileName(var FileName: String; Mask, Title, ALabel: String; Buttons: Word): Word;

implementation

uses Collect, Strings, ErrMess,
     Memory, Dos, DNApp, DNStdDlg, advance1, advance2,
     MicroEd2, Histries;

type
  THeaderDBF = record     {KV}
    DBFIdent : char;      { $03 - Нет MEMO; $83 - Есть MEMO }
                          { FoxBase+, FoxPro, dBaseIII+, dBaseIV, no memo - 0x03 }
                          { FoxBase+, dBaseIII+ with memo - 0x83 }
                          { FoxPro with memo - 0xF5 }
                          { dBaseIV with memo - 0x8B }
                          { dBaseIV with SQL Table - 0x8E }

    Yar : byte;           { две последние цифры года }
    Month : byte;         { month number }
    Day : byte;           { day number }
    LastRecord : longint; { номер последней записи }
    DataOffset : AWord;   { смещение первой записи относительно начала файла}
    RecSize : AWord;      { размер записи с учетом символа удаления }
    Reserv1     : AWord;
    WaitTrans  : Byte;       { dB IV }
    Reserv2     : Array[0..12] of Byte;
    Multiindex  : Byte;
    Reserv3     : Array[0..2] of Byte;
  end;
  TDBFLength = record { длина поля DBF }     {KV}
    case integer of
      0: (FieldLength   : AWord);
      1: (NumericLength : byte; Decimals : byte);
  end;
  PDBFField = ^TDBFField;                    {KV}
  TDBFField = record        { dB field descriptor Length = 32 bytes }
    FieldName : Array[0..10] of char; { имя поля }
    FieldType : char; { тип: C=$43, D=$44, L=$4C, $M=$4D, N=$4E }
    Reserved  : Array[0..3] of char; { Расположение поля внутри записи }
    FLength   : TDBFLength; { длина поля }
    Reserved2 : Array[0..12] of Byte;
    Tag       : Byte; { for multiindex, only dB IV }
  end;

  PDbfFieldCollection = ^TDbfFieldCollection;
  TDbfFieldCollection = object(TCollection) {KV}
    procedure FreeItem(Item : pointer); virtual;
  end;

  TDbaseWriter = object(TBufStream) {KV}
    Header : THeaderDbf;
    Fields : PDbfFieldCollection;
    CurRecord : longint;
    EofFlag : boolean;
    constructor Init(FileName: FNameStr; Mode: Word; Size: Sw_Word);
    destructor Done; virtual;
    procedure AddField(const NameField : string; TypeField : char;
                       LenField : integer; DecField : integer);
    procedure AddRecord;
    procedure CreateFile;
    procedure DeleteRecord;
    function FCount : integer;
    function FieldAsString(FieldIndex : integer) : string;
    function FieldAsInteger(FieldIndex : integer) : integer;
    function FieldAsFloat(FieldIndex : integer) : double;
    procedure FieldPutString(FieldIndex : integer; FieldValue : string);
    procedure FieldPutInteger(FieldIndex : integer; FieldValue : integer);
    procedure FieldPutFloat(FieldIndex : integer; FieldValue : double);
    function FieldName(FieldIndex : integer) : string; // Warning! FieldIndex from 1
    function FieldDec(FieldIndex : integer) : byte; // FieldIndex >= 1
    function FieldLen(FieldIndex : integer) : AWord; // Warning! FieldIndex from 1
    function FieldPos(NameField : string) : integer; // FieldIndex >= 1
    function FieldType(FieldIndex : integer) : char;
    function FieldOffsetInBuffer(FieldIndex : integer) : integer; // FieldIndex >= 1
    function FieldOffsetInFile(FieldIndex : integer) : longint; // FieldIndex >= 1
    procedure First;
    procedure DBGoTo(NewRecord : integer);
    procedure Next;
    procedure ReadFile;
    procedure RecallRecord;
    procedure WriteEndOfFile;
  end;

  TExcelWriter = object(TBufStream) {KV}
    procedure WriteBOF;
    procedure WriteEOF;
    procedure WriteBLANK(const Col, Row: AWord; const XF: AWord);
    procedure WriteLABEL(const Data: string; const Col, Row: AWord; const XF: AWord);
    procedure WriteNUMBER(const Data: Double; const Col, Row: AWord; const XF: AWord);
    procedure WriteBOOL(const Data: Boolean; const Col, Row: AWord; const XF: AWord);
    procedure WriteERROR(const Data: Byte; const Col, Row: AWord; const XF: AWord);
  end;


const
  SearchData: record S: String[240]; CellOptions, TxtOptions: Word; end =
                  (S:''; CellOptions:0; TxtOptions:0);
  ReplaceData: record S, S1: String[240]; CellOptions, TxtOptions: Word; end =
                  (S:''; S1:''; CellOptions:0; TxtOptions:0);
  WasReplace: Boolean = False;
  ContSearch: Boolean = False;
  SearchCanceled: Boolean = False;
  DefaultColWidth = 11;
  UntitledName = 'Untitled.WKZ';

        {-DataCompBoy-}
function GetFileName(var FileName: String; Mask, Title, ALabel: String; Buttons: Word): Word;
 var S: String;
     D: PFileDialog;
     B: Boolean;
     C: Word;
begin
 S := ''; B := False; if Mask = '' then begin Mask := x_x; B := False end;
 D := PFileDialog(Application^.ValidView(New(PFileDialog,
        Init(Mask, Title, ALabel, Buttons, 211))));
  if D = nil then Exit;
  if B then D^.SetData(S);
  C := Desktop^.ExecView(D);
  if C <> cmCancel then D^.GetFileName(S);
  GetFileName := C;
  Dispose(D,Done);
{$IFDEF OS_DOS}
  FileName := lfGetLongFileName(S);
{$ELSE}
  FileName := S;
{$ENDIF}
end;
        {-DataCompBoy-}


{-------------------------       TInfoView       -------------------------}

procedure TInfoView.SetInfo;
begin
 InfoStr := Copy(S, 1, Size.X);
 InfoAttr := Attr;
 DrawView;
end;

procedure TInfoView.Draw;
 var B: TDrawBuffer;
begin
 MoveChar(B, ' ', InfoAttr, Size.X);
 MoveStr(B[Size.X - Length(InfoStr)], InfoStr, InfoAttr);
 WriteLine(0, 0, Size.X, 1, B);
end;

{-------------------------      TCalcWindow      -------------------------}

        {-DataCompBoy-}
constructor TCalcWindow.Init;
 var R: TRect;
     P: PView;
     P1: PInfoView;
begin
 if (AName = '') or (GetFileAttr(AName+#0) and Directory<>0) then
   AName := UntitledName
 else AName := lFExpand(AName);
 TWindow.Init(Bounds, Cut(AName,Bounds.B.X-Bounds.A.X-12), 0);
 Options := Options or ofTileable;
 Flags := Flags or wfMaxi;
 if LowMemory then Exit;

 R.A.X := 1; R.A.Y := 5; R.B.X := 7; R.B.Y := Size.Y-1;
 P := New(PStaticText, Init(R, '')); P^.Options := P^.Options or ofFramed;
 P^.GrowMode := gfGrowHiY;
 Insert(P);

 R.A.X := 8; R.A.Y := 2; R.B.X := Size.X-1; R.B.Y := 4;
 P := New(PStaticText, Init(R, '')); P^.Options := P^.Options or ofFramed;
 P^.GrowMode := gfGrowHiX;
 Insert(P);

 GetExtent(R); R.Grow(-1,-1); R.B.Y := R.A.Y + 1;
 P := PView(LoadResource(dlgWkzMenuBar));
 P^.Locate(R);
 P^.GrowMode := gfGrowHiX;
 Insert(P);

 GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y);
 R.B.Y := R.A.Y + 1; Inc(R.A.X, 7);
 P := New(PCalcInput, Init(R, 240));
 P^.GrowMode := gfGrowHiX;
 P^.Options := P^.Options or ofSelectable;
 Insert(P);

 R.Assign(1, 2, 7, 4);
 P1 := New(PInfoView, Init(R));
 PInfoView(P1)^.InfoStr := '';
 PInfoView(P1)^.InfoAttr := GetColor(9);
 Insert(P1);

 GetExtent(R); R.Grow(-1,-1);
 Inc(R.A.Y, 2);
 CalcView := New(PCalcView, Init(R, PCalcInput(P), P1,
                  MakeScrollBar(sbHorizontal + sbHandleKeyboard),
                  MakeScrollBar(sbVertical + sbHandleKeyboard)));
 Insert(CalcView);
 CalcView^.LoadSheet(AName);
end;
        {-DataCompBoy-}

constructor TCalcWindow.Load;
 var R: TRect;
     P: PView;
     P1: PInfoView;
begin
  inherited Load(S);
  GetSubViewPtr(S, CalcView);
  if CalcView = nil then {Cat}
    Fail;
end;

procedure TCalcWindow.Store;
begin
  inherited Store(S);
  PutSubViewPtr(S, CalcView);
end;

procedure TCalcWindow.HandleEvent;
begin
 case Event.What of
  evCommand:
   case Event.Command of
    cmClose, cmQuit:
     if not CalcView^.AskSave then ClearEvent(Event);
   end;
  evBroadcast:
    case Event.Command of
     cmFindView:
      if PString(Event.InfoPtr)^=CnvString(CalcView^.SName) then
        begin Self.Select; ClearEvent(Event); end;
    end;
 end;
 inherited HandleEvent(Event);
end;

Destructor TCalcWindow.Done;
begin
 StoreViewInfo(@Self);
 Inherited Done;
end;

{-----------------------------    TCalcInput     ---------------------------}

constructor TCalcInput.Load;
begin
  inherited Load(S);
  GetPeerViewPtr(S, CalcView);
end;

procedure TCalcInput.Store;
begin
  inherited Store(S);
  PutPeerViewPtr(S, CalcView);
end;

function TCalcInput.GetPalette;
 const S: String[4] = #13#13#14#13;
begin
 GetPalette := @S;
end;

procedure MakeDefaultOptions(const S: string;
  var O: AWord; var D: byte; var R: CReal);
  var
    I: Integer;
  begin
  Val(S, R, I); D := 2; { 2 десятичных знака по умолчанию }
  if I = 0 then { число: вправо, как есть }
    O := coValue or coRight
  else if (S[1] = '=') and (Length(S) > 1) then
     { формула: вправо, десятичное, 2 знака }
    O := coFormula or coDec or coRight
  else { текст: влево, как есть }
    begin O := 0; R := 0; end;
  end;

procedure TCalcInput.HandleEvent;

 function EndEdit: Boolean;
  label 2;
  var NewS: ShortString;
      O: AWord; D: byte;
      R: CReal;
      I: Integer;
      P: PCellRec;
      TypeChanged: boolean;
 begin
  GetData(NewS);
  EndEdit := true;
  with CalcView^ do
   begin
   Modified := true;
   CurrentCalc.X := Delta.X + Cur.X; CurrentCalc.Y := Delta.Y + Cur.Y;
   DelRight(NewS);
   P := Cells^.Get(Delta.X + Cur.X, Delta.Y + Cur.Y);
   if (P <> nil) and (NewS = P^.S) then exit;
   if NewS = '' then
     begin
     if (P = nil) then exit;
     if MessageBox(GetString(dlWkzWarningClearCell),
         nil, mfOKCancel) <> cmOK
     then begin EndEdit := false; exit; end;
     Cells^.DelItem(Delta.X + Cur.X, Delta.Y + Cur.Y);
     goto 2;
     end;

   MakeDefaultOptions(NewS, O, D, R);
   TypeChanged := true; // на случай P=nil
   if P <> nil then with P^ do
     begin
     if (Options and coTypeMask) = coTypeMask then
       Options := (Options and not coTypeMask) or coFormula;
     TypeChanged := (Options xor O) and coTypeMask <> 0;
     if TypeChanged then
       begin {Изменен тип ячейки}
       if MessageBox(GetString(dlWkzWarningCellTypeChange),
           nil, mfOKCancel) <> cmOK
       then begin EndEdit := false; exit; end;
       end;
     end;
   with Cells^.ReplaceItem(Delta.X + Cur.X, Delta.Y + Cur.Y, NewS)^ do
     begin
     if TypeChanged then
       begin Options := O; Decimals := D; end;
     Value := R;
     end;
2:
   Recalc(false);
   end {with CalcView^};
 end;

 procedure Transfer;
 begin
  CalcView^.FocusEvent := Event;
  if EndEdit then PWindow(Owner)^.SelectNext(False);
  ClearEvent(Event)
 end;

begin
 case Event.What of
  evKeyDown:
   case Event.KeyCode of
    kbUp, kbDown, kbPgUp, kbPgDn, kbCtrlPgUp,
    kbCtrlPgDn, kbEnter: Transfer;
    kbLeft: if CurPos = 0 then Transfer;
    kbRight: if CurPos > Length(CnvString(Data)) - 1 then Transfer;
    kbTab:
      if not EndEdit then ClearEvent(Event);
   end;
 end;
 TInputLine.HandleEvent(Event);
end;

{-----------------------------    TCalcView     ---------------------------}

constructor TCalcView.Init;
 var I, J: Integer;
begin
 TView.Init(Bounds);
 HelpCtx := hcSpreadSheet;
 HScroll := AHScroll; VScroll := AVScroll;
 Options := Options or ofSelectable;
 GrowMode := gfGrowHiX + gfGrowHiY;
 EventMask := $FFFF; CalcInput := AInfo; CellInfo := ACellInfo;
 PCalcInput(CalcInput)^.CalcView := @Self;
 FocusEvent.What := evNothing;
 FillChar(ColWidth, SizeOf(ColWidth), DefaultColWidth);
 Marking := False; BlockDraw := False; Modified := false;
 Cells := nil;
 Delta.X := 0; Delta.Y := 0;
 Cur := Delta; Mark  := Cur;
 HScroll^.SetParams(0, 0, 255, 1, 1);
 VScroll^.SetParams(0, 0, MaxCellY - 1, Size.Y - 3, 1);
 SName:=nil;
end;

constructor TCalcView.Load;
var
  R: TRect;
begin
  inherited Load(S);
  GetPeerViewPtr(S, CalcInput);
  GetPeerViewPtr(S, CellInfo);
  GetPeerViewPtr(S, HScroll);
  GetPeerViewPtr(S, VScroll);
  S.Read(Delta, SizeOf(Delta)*3);
  S.Read(NumC, 6); {###}
  SName:=S.ReadStr;
  LoadSheet(SName^);
  FocusEvent.What := evNothing;
  Recalc(true);
end;

procedure TCalcView.Store;
begin
  inherited Store(S);
  PutPeerViewPtr(S, CalcInput);
  PutPeerViewPtr(S, CellInfo);
  PutPeerViewPtr(S, HScroll);
  PutPeerViewPtr(S, VScroll);
  S.Write(Delta, SizeOf(Delta)*3);
  S.Write(NumC, 6); {###}
  S.WriteStr(SName);
end;


function TCalcView.Valid;
begin
 Valid := Cells <> nil;
end;

procedure TCalcView.SetState;
 var Bounds: TRect;
begin
 TView.SetState(AState, Enable);
 if Enable and (AState and sfFocused <> 0) then DrawView;
 if (AState and sfActive <> 0) then
 if Enable then
  begin
   HScroll^.GrowTo(HScroll^.Size.X, 1);
   VScroll^.GrowTo(1, VScroll^.Size.Y);
   EnableCommands([cmUndo, cmCut, cmCopy, cmPaste, cmClear]);
  end else
  begin
   HScroll^.GrowTo(HScroll^.Size.X, 0);
   VScroll^.GrowTo(0, VScroll^.Size.Y);
  end;
 if Owner<>nil then begin
  GetBounds(Bounds);
  DisposeStr(PWindow(Owner)^.Title);
  If SName<>nil then
   PWindow(Owner)^.Title:=NewStr(Cut(SName^,Bounds.B.X-Bounds.A.X-12));
 end;
end;

function TCalcView.GetPalette;
 const S: String[4] = #8#13#14;
begin
 GetPalette := @S;
end;

type
     TQArray = Array[0..80, 0..60] of Integer;
     PQArray = ^TQArray;

const Q: PQArray = nil;


function MakeCellText(P : PCellRec) : string; {KV}
var
  S1: string absolute Result;
  i, A1, A2 : integer;
begin
  S1:='';
  case (P^.Options shr 4) and 7 of
    0: S1 := P^.S;
    1: Str(P^.Value:0:P^.Decimals, S1);
    2: begin
         Str(P^.Value:0:20, S1);
         i:=Length(S1);
         while (i > 0) and (S1[i] = '0') do dec(i);
         SetLength(S1,i);
         if S1[Length(S1)] = '.' then SetLength(S1, Length(S1)-1);
         S1 := MakeComma(S1);
       end;
    3: begin
         Str(P^.Value, S1);
         A1 := Pos('E', S1);
         if A1 > 0 then begin
           A2 := A1 - 1;
           while (S1[A2] = '0') and (S1[A2 - 1] <> '.') do Dec(A2);
           Move(S1[A1], S1[A2 + 1], Length(S1) - A1 + 1);
           SetLength(S1, Length(S1) - (A1 - A2 - 1));
         end;
       end;
    4: if P^.Value = 0 then S1 := FalseStr else S1 := TrueStr;
    5: MakeCurrency(P^.Value, S1);
    6: begin
         Str((P^.Value*100):0:2, S1);
         S1 := MakeComma(S1)+'%';
       end;
    7: S1 := '';
  end;
end;

procedure FormatCellText(const S1: string; K: integer;
              ShowSeparators: boolean;
              Options: AWord; var S: string);
  var
    T: integer;
  begin
  if Length(S1) >= K then
    begin
    if ShowSeparators then
      begin Move(S1[1], S[1], K-1);  S[K] := #16; end
    else
      Move(S1[1], S[1], length(S1))
    end
  else
    begin
    T := 1-ord(ShowSeparators);
    if Options and $C = 0 then
      Move(S1[1], S[1], Length(S1))
    else if (Options and $C = coRight) then
      Move(S1[1], S[K - Length(S1) + T], Length(S1))
    else
      Move(S1[1], S[1 + (K - Length(S1)) div 2], Length(S1));
    end;
  end;

procedure TCalcView.Draw;

 var
     B, B1: TDrawBuffer;
     BC: Array [0..High(B)] of record C: Char; A: Byte; end Absolute B;
     I, J, K, L, T: Integer;
     C1, C2, C3: Byte;
     S, S1: String;
     P: PCellRec;
     X1, X2, Y1, Y2: Integer;
     CurPos: TPoint;
     StX, EnX, A1, A2 : Byte;
     Wrk: integer;
     EmptyLine: string;
begin
 if BlockDraw then Exit;
 C1 := GetColor(1); C2 := GetColor(2); C3 := GetColor(3);
 if Q = nil then New(Q);
 CurPos.X := Delta.X + Cur.X; CurPos.Y := Delta.Y + Cur.Y;
 if CurPos.X > 255 then Cur.X := 255 - Delta.X;
 if CurPos.Y >= MaxCellY then Cur.Y := MaxCellY - 1 - Delta.Y;
 if Cur.Y < 0 then begin J := Cur.Y; Cur.Y := 0; VScroll^.SetValue(Delta.Y + J); Exit end;
 if Cur.X < 0 then begin J := Cur.X; Cur.X := 0; HScroll^.SetValue(Delta.X + J); Exit end;
 CurPos.X := Delta.X + Cur.X; CurPos.Y := Delta.Y + Cur.Y;
 X1 := CurPos.X; X2 := Mark.X;
 Y1 := CurPos.Y; Y2 := Mark.Y;
 if X1 > X2 then
   begin Wrk := X1; X1 := X2; X2 := Wrk; end;
 if Y1 > Y2 then
   begin Wrk := Y1; Y1 := Y2; Y2 := Wrk; end;
 if Y2 > 4095 then Y2 := 4095;
 SetLength(EmptyLine, Size.X); FillChar(EmptyLine[1], Size.X, ' ');
 MoveChar(B, ' ', C1, Size.X);
 BC[6].C := '│';
 MoveChar(B1, '─', C1, Size.X);
 WordRec(B1[6]).Lo := Byte('┼');
 NumC := 0; I := 7; J := Delta.X; StX := I; EnX := Size.X;

{Заголовки колонок и верхняя черта}
 While (I < Size.X) and (J <= HScroll^.Max) do
  begin
   K := ColWidth[J]; L := ColWidth[J + 1];
   SetLength(S, K); FillChar(S[1], K, ' '); S[K] := '│';
   S1 := GetColName(J);
   move(S1[1], S[K div 2], length(S1));
   WordRec(B1[I + K - 1]).Lo := Byte('┼');
   MoveStr(B[I], S, C1);
   if J < X1 then Inc(StX, K);
   if (J > X2) and (EnX > I) then EnX := I;
   if (J = HScroll^.Max) and (J <= X2) then EnX := I + K;
   Inc(NumC);
   Inc(J);
   Inc(I, K);
  end;

 if NumC = 0 then NumC := 1;
 if Cur.X >= NumC then
  begin
   J := Cur.X - NumC + 1;
   Cur.X := NumC - 1;
   HScroll^.SetValue(Delta.X + J);
   Delta.X := HScroll^.Value;
   Exit;
  end;
 PInfoView(CellInfo)^.SetInfo(GetCellName(CurPos.X, CurPos.Y), Owner^.GetColor(9));
 FillChar(Q^, SizeOf(Q^), 255);
 for I := 1 to Cells^.Count do
  begin
   P := Cells^.At(I - 1);
   if (P^.Row >= Delta.Y) and (P^.Row < Delta.Y + Size.Y - 2) and
      (P^.Col >= Delta.X) and (P^.Col < Delta.X + NumC + 3) then
       Q^[P^.Col - Delta.X, P^.Row - Delta.Y] := I - 1;
  end;
 WriteBuf(6, 0, Size.X, 1, B[6]);
 WriteBuf(0, 1, Size.X, 1, B1);

 for I := 0 to Size.Y - 2 do
  begin
   MoveChar(B, ' ', C2, Size.X);
   if Delta.Y + I <= VScroll^.Max then
    begin
     Str((Delta.Y + I + 1):5, S);
     MoveStr(B, S, C1); L := 7;
     for J := 0 to min(NumC-1, 255-Delta.X) do
      begin
       K := ColWidth[Delta.X + J];
       if ShowSeparators then
         begin { ограничиваем длину по колонке }
         SetLength(S, K); FillChar(S[1], K, ' '); S[K] := '│';
         end
       else
         begin { длина до конца экрана }
         T := Size.X - L + 1; SetLength(S, T); FillChar(S[1], T, ' ');
         end;
       P := nil;
       if Q^[J, I] >= 0 then
        begin
         P := Cells^.At(Q^[J, I]);
         S1 := MakeCellText(P);
         FormatCellText(S1, K, ShowSeparators, P^.Options, S);
        end;
       if ShowSeparators or (P <> nil) then
         MoveStr(B[L], S, C2);
       if (Cur.X = J) and (Cur.Y = I) then
        begin
         if P <> nil then
          begin
           S := P^.S;
           case P^.Options and 3 of
            coText: WriteStr(0, 0, '   T  ', 2);
            coValue: WriteStr(0, 0, '   V  ', 2);
            else WriteStr(0, 0, '   F  ', 2);
           end;
          end else
           begin
            WriteStr(0, 0, GetString(dlWKZ_Empty), 2);
            S := '';
           end;
         PCalcInput(CalcInput)^.SetData(S);
         CalcInput^.Draw;
        end;
       L := L + K;
      end;
    end;
   BC[6].C := '│'; BC[6].A := C1;
   if (Delta.Y + I >= Y1) and (Delta.Y + I <= Y2) and (StX < EnX)
      then MoveColor(B[StX], EnX - StX - ord(ShowSeparators), C3);
   WriteBuf(0, I + 2, Size.X, 1, B);
  end;
end;

        {-DataCompBoy-}
procedure TCalcView.HandleEvent;
type
  TColIndexArray = array[0..255] of integer; {KV}
  TColTypeAndLengthRec = record              {KV}
    T : char;
    L : AWord;
    D : byte;
  end;
  PColTypeAndLengthArray = ^TColTypeAndLengthArray;
  TColTypeAndLengthArray = array[0..255] of TColTypeAndLengthRec; {KV}

 var Inf: PCalcInput;
     S: String;
     St: PStream;
     MaxX, MaxY, K, L: Integer;
     P: PCellRec;
     FName: String;
     X1, X2, Y1, Y2: Integer;
     R: record Display, Justify: Word; Dec: String[2]; Protect: Word; end;
     I: Integer;
     Wrk: integer;

  procedure kvGetMaxColRow(var MaxCol, MaxRow : integer); {KV}
  var
    i : integer; P : PCellRec;
  begin
    MaxCol:=0; MaxRow:=0;
    for i:=0 to Cells^.Count-1 do begin
      P:=Cells^.At(i);
      if MaxCol < P^.Col then MaxCol:=P^.Col;
      if MaxRow < P^.Row then MaxRow:=P^.Row;
    end;
  end;

  procedure kvFillColIndexArray(CurRow : integer; var ColIndexArray : TColIndexArray); {KV}
  var
    i : integer; P : PCellRec;
  begin
    FillChar(ColIndexArray,SizeOf(ColIndexArray),255);
    for i:=0 to Cells^.Count-1 do begin
      P:=Cells^.At(i);
      if P^.Row = CurRow then ColIndexArray[P^.Col]:=i;
    end;
  end;

  procedure ImportFromCsv; {KV}
  var
    S : string;
    FileName : string;
    CellValue : string;
    ColSeparator : char;
    F : lText;
    CurCol, CurRow : integer;
    StartCh, EndCh : integer;
    P : PCellRec;
    R : CReal;
    i : integer;
  begin
    if Cells^.Count > 0 then begin
      if MessageBox(GetString(dlWkzWarningClear),nil,
           mfYesNoCancel or mfConfirmation) <> cmYes then Exit;
    end;
    if GetFileName(FileName, '*.CSV', GetString(dlOpenFile), GetString(dlOpenFileName),
                   fdOKButton+fdHelpButton) = cmCancel then Exit;
    {KV: разделитель колонок можно определить автоматически, однако при этом
         возникает несколько проблем: если в файле только одна строка нельзя
         определить какие символы встречаются во второй строке столько-же раз
         сколько и в первой. Выполнять проверку только на ',',';' неправильно}
    S:=';';
    if InputBox(GetString(dlWkzQuerySeparatorTitle),GetString(dlWkzQuerySeparatorLabel),
         S,1,hcSpreadSheet) <> cmOk then Exit;
    if S = '' then ColSeparator:=';'
    else ColSeparator:=S[1];

    lAssignText(F,FileName);
    lSetTAttr(F,Archive);
    lResetTextReadOnly(F);
    i:=IOResult;
    if i <> 0 then begin
      MessFileNotOpen(FileName,i);
      Exit;
    end;
    Cells^.FreeAll;
    CurRow:=0;
    while not Eof(F.T) do
      begin
      Readln(F.T,S);
      if S <> '' then
        begin {KV: Просто на всякий случай}
        CurCol:=0;
        StartCh:=1;
        while StartCh <= Length(S) do
          begin
          EndCh:=StartCh;
          while (EndCh <= Length(S)) and (S[EndCh] <> ColSeparator) do inc(EndCh);
          if EndCh > StartCh then
            begin
            CellValue:=System.Copy(S,StartCh,EndCh-StartCh);
            P:=Cells^.NewItem(CurCol,CurRow,CellValue);
            MakeDefaultOptions(CellValue, P^.Options, P^.Decimals, P^.Value);
            end;
          StartCh:=EndCh+1;
          inc(CurCol);
        end;
      end;
      inc(CurRow);
    end;
    Close(F.T);
    ReCalc(true); Modified := true;
    if Owner <> nil then Owner^.Redraw;
  end;

  procedure ImportFromDbf; {KV}
  var
    DBF : TDbaseWriter;
    FileName : FNameStr;
    CellValue : string;
    P : PCellRec;
    R : CReal;
    i : integer;
  begin
    if Cells^.Count > 0 then begin
      if MessageBox(GetString(dlWkzWarningClear),nil,
           mfYesNoCancel or mfConfirmation) <> cmYes then Exit;
    end;
    if GetFileName(FileName, '*.DBF', GetString(dlOpenFile), GetString(dlOpenFileName),
                   fdOKButton+fdHelpButton) = cmCancel then Exit;
    DBF.Init(FileName,stOpenRead,16384);
    if DBF.Status <> stOk then begin
      MessFileNotOpen(FileName,DBF.ErrorInfo);
      DBF.Done;
      Exit;
    end;

    Cells^.FreeAll;
    DBF.First;
    while not DBF.EofFlag do begin
      for i:=1 to DBF.FCount do begin
        CellValue:=DBF.FieldAsString(i);
        DelRight(CellValue);
        if CellValue <> '' then
          with Cells^.NewItem(i-1,DBF.CurRecord-1,CellValue) do
            case DBF.FieldType(i) of
             'N': {число}
               begin
               DelLeft(S); Options := coValue or coRight;
               Decimals:=DBF.FieldDec(i);
               end;
             'L': {логическое}
               begin
               if UpCase(CellValue[1]) = 'T' then S:='-1' else S:='0';
               Options:= coValue or coBool or coRight;
               end;
             else {текст, дата, мемо...}
               if (CellValue[1] = '=') and (Length(CellValue) > 1) then
                 begin
                 Options := coFormula or coDec or coRight;
                 Decimals := 2;
                 end;
           end{case};
      end;
      DBF.Next;
    end;
    DBF.Done;
    ReCalc(true); Modified := true;
    Owner^.Redraw;
  end;

  procedure ExportToCsv; {KV}
  var
    S : string;
    PS : PString;
    ColSeparator : char;
    UseFormatFlag : boolean;
    CalcFormulaFlag : boolean;
    F : lText;
    MaxCol, MaxRow, CurCol, CurRow : integer;
    P : PCellRec;
    ColIndexArray : TColIndexArray;
    R : record
      Separator : string[1];
      ValueFormat : Word;
      FormulaFormat : Word;
    end;
  begin
    if GetFileName(S, '*.CSV', GetString(dlExportTitle), GetString(dlExportLabel),
                   fdOKButton+fdHelpButton) = cmCancel then Exit;
    if ExistFile(S) then begin
      PS:=@S;
      if MessageBox(GetString(dlED_OverQuery),@PS,
           mfYesNoCancel+mfWarning) <> cmYes then Exit;
    end;

    FillChar(R,SizeOf(R),0);
    R.Separator:=';';
    if ExecResource(dlgSetExportCsvFormat, R) = cmOK then begin
      if Length(R.Separator) > 0 then ColSeparator:=R.Separator[1]
      else ColSeparator:=';';
      UseFormatFlag:=(R.ValueFormat = 1);
      CalcFormulaFlag:=(R.FormulaFormat = 1);
      lAssignText(F,S);
      lSetTAttr(F,Archive);
      lRewriteText(F);
      if IOResult <> 0 then begin
        PS:=@S; Msg(erCantCreateFile, @PS, mfError+mfOKButton); Exit;
      end;
      kvGetMaxColRow(MaxCol,MaxRow);
      for CurRow:=0 to MaxRow do begin
        kvFillColIndexArray(CurRow, ColIndexArray); {KV}
        for CurCol:=0 to MaxCol do begin
          if ColIndexArray[CurCol] >= 0 then begin
            P:=Cells^.At(ColIndexArray[CurCol]);
            if ((P^.Options and 3) = coText) or
               (((P^.Options and coFormula) <> 0) and (not CalcFormulaFlag)) then
              Write(F.T,P^.S)
            else
            if ((P^.Options and 3) = coValue) or
               (((P^.Options and coFormula) <> 0) and CalcFormulaFlag) then begin
              if UseFormatFlag then S:=MakeCellText(P)
              else begin
                // Оставляем 5 знаков после запятой, на случай если
                // неправильно указан формат
                if P^.Decimals <= 5 then Str(P^.Value:0:5, S)
                else Str(P^.Value:0:P^.Decimals, S);
                while S[Length(S)] = '0' do SetLength(S, Length(S)-1);
                if S[Length(S)] = '.' then SetLength(S, Length(S)-1);
              end;
              Write(F.T,S);
            end
            else Write(F.T,P^.S); {This code not use}
          end;
          if CurCol < MaxCol then Write(F.T,ColSeparator);
        end;
        Writeln(F.T);
      end;
      Close(F.T);
    end;
  end;

  procedure kvCalcFieldTypeAndLength(var TypeLenArr : TColTypeAndLengthArray);
  var
    i : integer;
    P : PCellRec;
    S1 : string;
  begin
    FillChar(TypeLenArr,SizeOf(TypeLenArr),0);
    for i:=0 to Cells^.Count-1 do begin
      P:=Cells^.At(i);
      with TypeLenArr[P^.Col] do begin
        if (P^.Options and coValue) <> coValue then
          // Если в колонке текст или ошибка в формуле,
          // то эта колонка сохраняется как текстовая
          T:='C'
        else
        if (T = #0) and ((P^.Options and $F0) = coBool) then
          // Если тип колонки еще не оределен и первое найденное значение
          // имеет формат Boolean, то устанавливаем тип Boolean
          T:='L'
        else
        if T = #0 then T:='N'; // По умолчанию - число
      end;
    end;
    // Делаем второй проход когда типы полей уже известны
    // для определения размерности
    for i:=0 to Cells^.Count-1 do begin
      P:=Cells^.At(i);
      with TypeLenArr[P^.Col] do begin
        case T of
          'C': begin
                 if L < Length(P^.S) then L:=Length(P^.S);
               end;
          'N': begin
                 Str(P^.Value:0:P^.Decimals, S1);
                 if D < P^.Decimals then begin
                   if L > 0 then inc(L,P^.Decimals-D);
                   D:=P^.Decimals;
                 end;
                 if L < Length(S1) then L:=Length(S1);
               end;
          'L': begin L:=1; D:=0; end;
        end; { case }
      end; { with }
    end; { for }
  end;

  procedure ExportToDbf; {KV}
  var
    FileName : FNameStr;
    PS : PString;
    TypeLenArr : TColTypeAndLengthArray;
    i, MaxCol, MaxRow, CurCol, CurRow : integer;
    DbfHeader : THeaderDbf;
    S : TDbaseWriter;
    Buf : PByteArray;
    TmpS : string;
    FieldName : string;
    ColIndexArray : TColIndexArray;
    P : PCellRec;
  begin
    if GetFileName(FileName, '*.DBF', GetString(dlExportTitle), GetString(dlExportLabel),
         fdOKButton+fdHelpButton) = cmCancel then Exit;

    if ExistFile(FileName) then begin
      PS:=@FileName;
      if MessageBox(GetString(dlED_OverQuery),@PS,
           mfYesNoCancel+mfWarning) <> cmYes then Exit;
    end;

    S.Init(FileName, stCreate, 16384);
    if S.Status <> stOK then begin
      PS:=@FileName;
      Msg(erCantCreateFile, @PS, mfError+mfOKButton); S.Done; Exit;
    end;

    kvGetMaxColRow(MaxCol,MaxRow);
    kvCalcFieldTypeAndLength(TypeLenArr);

    for i:=0 to MaxCol do begin
      with TypeLenArr[i] do begin
        FieldName:=GetColName(i);
        if T = #0 then begin T:='N'; L:=5; D:=0; end;
        if L = 0 then L:=1;
        if (T = 'L') and (L > 1) then L:=1;
        if (T <> 'N') and (D <> 0) then D:=0;
        S.AddField(FieldName,T,L,D);
      end;
    end;

    S.CreateFile;

    for CurRow:=0 to MaxRow do begin
      kvFillColIndexArray(CurRow, ColIndexArray);
      i:=1; // Номер текущего поля внутри записи
      S.AddRecord; // Добавляем новую пустую запись
      for CurCol:=0 to MaxCol do begin
        if ColIndexArray[CurCol] >= 0 then begin
          P:=Cells^.At(ColIndexArray[CurCol]);
          if TypeLenArr[CurCol].T = 'L' then begin
            if P^.Value = 0 then TmpS:='F'
            else TmpS:='T';
            S.FieldPutString(i,TmpS);
          end
          else
          if TypeLenArr[CurCol].T = 'N' then
            S.FieldPutFloat(i,P^.Value)
          else
            S.FieldPutString(i,P^.S);
        end;
        inc(i);
      end;
    end;
    S.WriteEndOfFile;
    S.Done;
  end;

  procedure ExportToXls; {KV}
  var
    FileName : FNameStr;
    PS : PString;
    i, MaxCol, MaxRow, CurCol, CurRow : integer;
    S : TExcelWriter;
    ColIndexArray : TColIndexArray;
    P : PCellRec;
  begin
    if GetFileName(FileName, '*.XLS', GetString(dlExportTitle), GetString(dlExportLabel),
                   fdOKButton+fdHelpButton) = cmCancel then Exit;
    if ExistFile(FileName) then begin
      PS:=@FileName;
      if MessageBox(GetString(dlED_OverQuery),@PS,
           mfYesNoCancel+mfWarning) <> cmYes then Exit;
    end;

    S.Init(FileName, stCreate, 16384);
    if S.Status <> stOK then begin
      PS:=@FileName;
      Msg(erCantCreateFile, @PS, mfError+mfOKButton); S.Done; Exit;
    end;

    kvGetMaxColRow(MaxCol,MaxRow);
    S.WriteBof;
    for CurRow:=0 to MaxRow do begin
      kvFillColIndexArray(CurRow, ColIndexArray);
      for CurCol:=0 to MaxCol do begin
        if ColIndexArray[CurCol] < 0 then
          S.WriteBlank(CurCol,CurRow,0)
        else begin
          P:=Cells^.At(ColIndexArray[CurCol]);
          if (P^.Options and coFormula) <> 0 then
            S.WriteLabel(P^.S,CurCol,CurRow,0)
          else
          if (P.Options and coValue) = coValue then begin
            if (P^.Options and $F0) = coBool then
              S.WriteBool(P^.Value <> 0,CurCol,CurRow,0)
            else S.WriteNumber(P^.Value,CurCol,CurRow,0);
          end
          else S.WriteLabel(P^.S,CurCol,CurRow,0);
        end;
      end;
    end;
    S.WriteEof;
    S.Done;
  end;

 procedure ExportToFile;
  var S, S1: String;
      C: Array[0..255] of Integer;
      P: PCellRec;
      MaxLv, Maxr, I, J, K, L, L1: Integer;
      F: lText;
 begin
   if GetFileName(S, x_x, GetString(dlExportTitle), GetString(dlExportLabel),
                     fdOKButton+fdHelpButton) = cmCancel then Exit;
   lAssignText(F, S); lSetTAttr(F, Archive);
   lRewriteText(F);
   MaxLv := 0;
   for I := 1 to Cells^.Count do
    begin
     P := Cells^.At(I-1);
     if MaxLv < P^.Row then MaxLv := P^.Row;
    end;
   for J := 0 to MaxLv do
     begin
     FillChar(C, Sizeof(C), 255); MaxR := 0;
     for I := 1 to Cells^.Count  do
       begin
       P := Cells^.At(I-1);
       if J = P^.Row then C[P^.Col] := I-1;
       if MaxR < P^.Col then MaxR := P^.Col;
       end;
     for I := 0 to MaxR do
       begin
       K := ColWidth[I];
       if ShowSeparators then
         S := Strg(' ', K-1)+'│'
       else S := Strg(' ', 255);
       if C[I] >= 0 then
         begin
         P := Cells^.At(C[I]);
         S1 := MakeCellText(P); L1 := length(S1);
         FormatCellText(S1, K, ShowSeparators, P^.Options, S);
         end
       else
         L1 := length(S);
       if not ShowSeparators then
         while (I < MaxR) and (C[I+1] < 0) do
           begin Inc(I); Inc(K, ColWidth[I]); end;
       if K <= L1 then
         Write(F.T, System.Copy(S, 1, K))
       else
         begin
         if K > 255 then L := 255 else L := K;
         Write(F.T, System.Copy(S,1,L));
         repeat
           Dec(K, L); if K > 255 then L := 255 else L := K;
           if L <= 0 then Break;
           Write(F.T, Strg(' ', L));
         until K <= 0;
         end;
       end;
     WriteLn(F.T);
     end;
   Close(F.T);
 end;

 procedure SetMark;
 begin
  if Marking then Exit;
  Mark.X := Delta.X + Cur.X;
  Mark.Y := Delta.Y + Cur.Y;
 end;

 procedure CheckMark;
 begin
  Marking := ShiftState and $3 <> 0
 end;

 procedure EndMarking;
 begin Marking := False; SetMark; DrawView; ClearEvent(Event) end;

 procedure ChangeFormat;
   const J: Integer = 0;
      NewOptions: AWord = 0; {см. комментарий к ForRectangle}
   procedure SetFormat(Item: PCellRec);
   begin
   with Item^ do
     begin
     Options := (Options and 3) or NewOptions;
     Decimals := J;
     end;
   end;

 begin {ChangeFormat}
  ClearEvent(Event);
  P := Cells^.Get(Delta.X + Cur.X, Delta.Y + Cur.Y);
  if P = nil then begin FillChar(R, sizeof(R), 0); R.Dec := '2' end
  else
   begin
    R.Display := (P^.Options shr 4) and 7;
    R.Justify := (P^.Options shr 2) and 3;
    Str(P^.Decimals, R.Dec);
    R.Protect := (P^.Options shr 7) and 1;
   end;
  if ExecResource(dlgSetCellFormat, R) = cmOK then
  begin
   X1 := Delta.X + Cur.X; X2 := Mark.X;
   Y1 := Delta.Y + Cur.Y; Y2 := Mark.Y;
   if X1 > X2 then
     begin Wrk := X1; X1 := X2; X2 := Wrk; end;
   if Y1 > Y2 then
     begin Wrk := Y1; Y1 := Y2; Y2 := Wrk; end;
   Val(R.Dec, J, I);
   NewOptions := ((R.Justify and 3) shl 2)
              or ((R.Display and 7) shl 4)
              or ((R.Protect and 1) shl 7);
   Cells^.ForRectangle(X1,Y1, X2,Y2, @SetFormat);
  end;
  Modified := true;
  DrawView;
 end;

 procedure CE; begin ClearEvent(Event) end;

 procedure ExpandCol(CurCol: integer);
   begin
   if ColWidth[CurCol] < ScreenWidth then
     begin Inc(ColWidth[CurCol]); DrawView end;
   Modified := true;
   end;

 procedure ShrinkCol(CurCol: integer);
   begin
   if ColWidth[CurCol] > 1 then
     begin Dec(ColWidth[CurCol]); DrawView end;
   Modified := true;
   end;

 var
   SaveCellWidth: Byte;
   CurCol: AInt;

begin {TCalcView.HandleEvent}
 Inf := PCalcInput(CalcInput);
 if GetState(sfFocused) and (FocusEvent.What <> evNothing) then
   begin
    PutEvent(Event); Event := FocusEvent; FocusEvent.What := evNothing;
   end;
 TView.HandleEvent(Event);
 CurCol := Delta.X + Cur.X;
 case Event.What of
  evCommand:
    case Event.Command of
     cmGetName: PString(Event.InfoPtr)^ := 'Speadsheet - '+SName^;
     cmImportToFile: begin ExportToFile; ClearEvent(Event) end;
     cmImportFromCsv: begin ImportFromCsv; ClearEvent(Event); end;
     cmImportFromDbf: begin ImportFromDbf; ClearEvent(Event); end;
     cmExportToCsv: begin ExportToCsv; ClearEvent(Event); end;
     cmExportToDbf: begin ExportToDbf; ClearEvent(Event); end;
     cmExportToXls: begin ExportToXls; ClearEvent(Event); end;
     cmSaveSheetAs: begin
              SaveSheetAs; CE;
              Owner^.ReDraw;
             end;
     cmSave: begin
              SaveSheet; CE;
              Owner^.ReDraw;
             end;
     cmOpen: begin
               CE;
               K := GetFileName(FName, '*.WKZ', GetString(dlOpenFile),
                    GetString(dlOpenFileName), fdOpenButton{ + fdReplaceButton, hsLoadSheet, 0});
               if K = cmCancel then Exit;
               LoadSheet(FName);
             end;
     cmChangeWidth:
       begin
       SaveCellWidth := ColWidth[CurCol];
       while true do
         begin
         CE; GetKeyEvent(Event);
         if Event.What = evKeyDown then
           case Event.KeyCode of
            kbLeft:
             ShrinkCol(CurCol);
            kbRight:
             ExpandCol(CurCol);
            kbEnter:
             Break;
            kbESC:
             begin
             ColWidth[CurCol] := SaveCellWidth; DrawView;
             break;
             end;
           end {case};
         Application^.Idle;
         end;
       CE;
       end;
     cmChangeFormat: begin ChangeFormat; CE; end;
     cmPaste: begin Paste; EndMarking end;
     cmCopy: begin Copy; EndMarking end;
     cmCut: begin Copy; Clear; EndMarking end;
     cmClear: begin Clear; EndMarking end;
     cmInsertLine:
       begin InsertLine; EndMarking end;
     cmInsertColumn:
       begin InsertCol; EndMarking end;
     cmDeleteLine:
       begin DeleteLine; EndMarking end;
     cmDeleteColumn:
       begin DeleteCol; EndMarking end;
     cmToggleShowMode:
       begin ShowSeparators := not ShowSeparators; DrawView; CE;  end;
     cmRecalc: begin Recalc(true); DrawView; CE end;
     cmGotoCell: begin
                  if HistoryCount(hsGotoCell) = 0 then S := 'A1'
                  else S := HistoryStr(hsGotoCell, 0);
                  if ExecResource(dlgGotoCellNumber, S) = cmOK
                  then GotoCell(S);
                  ClearEvent(Event); Exit
                 end;
     cmFindCell:
       begin
        if ExecResource(dlgFindCell, SearchData) = cmOK then
         begin
          SearchPos.X := -1; SearchPos.Y := 0;
          WasReplace := False;
          SearchCell;
         end;
        CE;
       end;
     cmSearchAgain:
       begin
        SearchCanceled := False;
        SearchPos.X := CurCol;
        SearchPos.Y := Delta.Y + Cur.Y;
        repeat SearchCell
        until not WasReplace or not ContSearch or SearchCanceled;
        CE;
       end;
     cmReplaceCell:
       begin
        L := ExecResource(dlgReplaceCell, ReplaceData);
        if (L = cmYes) or (L = cmOK) then
         begin
          SearchData.S := ReplaceData.S;
          SearchData.TxtOptions := ReplaceData.TxtOptions;
          SearchData.CellOptions := ReplaceData.CellOptions;
          SearchCanceled := False; ContSearch := L = cmYes;
          SearchPos.X := -1; SearchPos.Y := 0;
          WasReplace := True;
          repeat SearchCell
           until not WasReplace or not ContSearch or SearchCanceled;
         end;
        CE;
       end;
     cmMainMenu: begin
                  Message(DNApp.MenuBar, evCommand, cmMenu, nil); CE;
                 end;
    end;
  evKeyDown:
    case Event.KeyCode of
     kbDoubleAlt, kbDoubleCtrl: CE;
     kbDel: begin Clear; EndMarking end;
     kbShiftLeft,
     kbLeft: if Cur.X > 0 then
             begin
              CheckMark;
              Dec(Cur.X); SetMark;
              DrawView;
              CE; Exit
             end else Exit;
     kbShiftRight,
     kbRight:
       if (Cur.X < NumC)  then
         begin
          CheckMark;
          if (CurCol = HScroll^.Max) then
           begin if Cur.X > 0 then Dec(Cur.X); SetMark; Exit end;
          Inc(Cur.X); SetMark; DrawView;
          ClearEvent(Event); Exit
         end
       else if (CurCol = HScroll^.Max) then
         begin CheckMark; if Cur.X > 0 then Dec(Cur.X); SetMark; Exit end
       else Exit;
     kbShiftUp, kbUp:
       if Cur.Y > 0 then
         begin
          CheckMark;
          Dec(Cur.Y); SetMark; DrawView;
          ClearEvent(Event); Exit
         end
       else Exit;
     kbShiftHome, kbHome:
       begin
        CheckMark;
        Cur.Y := 0; Cur.X := 0; SetMark;
        HScroll^.SetValue(0);
        VScroll^.SetValue(0);
        DrawView; ClearEvent(Event); Exit
       end;
     kbShiftEnd, kbEnd:
       begin
        CheckMark;
        MaxX := 0; MaxY := 0; Cur.Y := 0; Cur.X := 0;
        for L := 1 to Cells^.Count do
         begin
          P := Cells^.At(L - 1);
          if P^.Col > MaxX then MaxX := P^.Col;
         end;
        if Cells^.Count <> 0 then MaxY := P^.Row;
        if (L + ColWidth[CurCol] < Size.X) and (MaxX > 0) then
        begin
         L := 7;
         while (L <= Size.X) and (MaxX >= 0) do
          begin
           K := ColWidth[CurCol];
           if MaxX >= 0 then begin Inc(Cur.X); Dec(MaxX) end;
           Inc(L, K);
          end;
         Dec(Cur.X); Inc(MaxX);
        end;
        VScroll^.SetValue(MaxY - Size.Y + 3); Delta.Y := VScroll^.Value;
        Cur.Y := MaxY - Delta.Y;
        HScroll^.SetValue(MaxX); Delta.X := HScroll^.Value;
        SetMark;
        DrawView; ClearEvent(Event); Exit
       end;
     kbShiftDown, kbDown:
       if (Cur.Y < Size.Y - 3)  then
         begin
          CheckMark;
          if (Delta.Y + Cur.Y >= VScroll^.Max) then
           begin if Cur.Y > 0 then Dec(Cur.Y); SetMark; Exit end;
          Inc(Cur.Y); SetMark; DrawView;
          ClearEvent(Event); Exit
         end
       else if (Delta.Y + Cur.Y >= VScroll^.Max) then
         begin if Cur.Y > 0 then Dec(Cur.Y); SetMark; Exit end
       else Exit;
      kbEnter: begin
                Marking := False; SetMark; //DrawView;
                PWindow(Owner)^.SelectNext(False);
                ClearEvent(Event);
               end;
      kbTab: begin
              Marking := False; SetMark; DrawView;
             end;
      kbAltLeft:
       ShrinkCol(CurCol);
      kbAltRight:
       ExpandCol(CurCol);
      else if Event.CharCode > #31 then
           begin
            Marking := False; SetMark; DrawView;
            PWindow(Owner)^.SelectNext(True);
            Event.InfoPtr := CalcInput;
            CalcInput^.PutEvent(Event);
            ClearEvent(Event)
           end;
    end;
  evBroadcast:
    case Event.Command of
     cmScrollBarChanged:
      if Event.InfoPtr = HScroll then
        begin CheckMark; Delta.X := HScroll^.Value; SetMark;
        DrawView; Exit
        end
      else if Event.InfoPtr = VScroll then
        begin CheckMark; Delta.Y := VScroll^.Value; SetMark;
        DrawView; Exit
        end
    end;
 end;
 Marking := False;
end;
        {-DataCompBoy-}

function TCalcView.GetCellValue(const S : String) : boolean;
 var
   I: Integer;
   SR: TCellSearcRec;
 begin
 Result:=false;
// DelSpaces(S); UpStr(S);
 if GetCellCoord(S, SR.Col, SR.Row) then
   begin
   if not Cells^.Search(@SR, i) then
     begin Res := 0; Result := True; end
   else with PCellRec(Cells^.At(i))^ do
     if Options and 3 <> 0 then
       begin Res := Value; Result := True; end;
   end;
 end;

{  Вычисление функций SUM и MUL. В скобках через запятую могут быть
диапазоны, ячейки и числа. Пробелы игнорируются. Наличие закрывающей
скобки должно быть проверено перед вызовом.
   Пример: =SUM(A 1:B7, C1 8, 1).
   Вычисленное число кладется в Calculat.Res}
function TCalcView.GetFuncValue(S : String) : boolean;
  procedure DoSum(PP: PCellRec);
    begin
    with PP^ do if (Options and 3 <> 0) then
      Res := Res + PP^.Value
    end;

  procedure DoMul(PP: PCellRec);
    begin
    with PP^ do if (Options and 3 <> 0) then
      Res := Res * PP^.Value
    end;

 var
     I: Integer;
     AFromX, AToX: Byte;
     AFromY, AToY: AInt;
     Op: pointer;
     SR: TCellSearcRec;
     t0, t: integer;
     S1: string;
     R: CReal;
 begin {TCalcView.GetFuncValue}
 Result:=false;
 DelSpaces(S); UpStr(S);
 if (system.Copy(S, 1, 4) = 'SUM(') then
   begin Op := @DoSum; Res := 0; end
 else if (system.Copy(S, 1, 4) = 'MUL(') then
   begin Op := @DoMul; Res := 1; end
 else exit;
 t := 5;
 while t <= length(S) do
   begin
   t0 := t;
   while not (S[t] in [',', ')']) do inc(t);
   if t=t0 then exit;
   S1 := system.Copy(S, t0, t-t0);
   I := Pos(':', S1);
   if I <> 0 then
     begin
     if not GetCellCoord(System.Copy(S1, 1, I-1), AFromX, AFromY) or
        not GetCellCoord(System.Copy(S1, I+1,MaxStringLength), AToX, AToY)
     then exit;
       Cells^.ForRectangle(AFromX, AFromY, AToX, AToY, Op);
     end
   else
     begin
     if not GetCellCoord(S1, AFromX, AFromY) then exit;
     Cells^.ForRectangle(AFromX, AFromY, AFromX, AFromY, Op);
     end;
   inc(t); { пропустить запятую или скобку }
   end;
 Result:=True;
 end;

procedure TCalcView.CalcError;
var
  L: integer;
  S: String;
  err: string;
  c: word;
begin
 S := GetString(Index) + ' ' + GetCellName(CurrentCalc.X,CurrentCalc.Y);
 if EvalueError then
   begin
   Err := GetErrOp(L);
   if (Err = '') and (CalcSym <> #3) then Err := ' ' + CalcSym;
   S := S + ^M^C + GetString(CalcErrMess) + Err;
   EvalueError := false;
   end;
 c := MessageBox(S, @Self, {mfYesNoCancel}mfYesButton + mfError);
end;

destructor TCalcView.Done;
begin
 If Cells<>nil then Dispose(Cells,Done); Cells:=nil;
 if Q <> nil then Dispose(Q);
 Q := nil;
 DisposeStr(SName);
 TView.Done;
end;

function TCalcView.AskSave;
 var A: Word;
begin
 AskSave := true; if not Modified then Exit;
 A := MessageBox(GetString(dlWorkSheet)+' '+Cut(SName^,40)+GetString(dlNotSaved),
                 nil, mfWarning+mfYesNoCancel);
 if A <> cmCancel then Modified := false;
 if A = cmYes then SaveSheet else AskSave := A <> cmCancel;
end;

        {-DataCompBoy-}
procedure TCalcView.LoadSheet;
 var S: PStream;
     Bounds: TRect;
begin
 GetBounds(Bounds);
 if Modified and not AskSave then Exit;
 if Cells <> nil then Dispose(Cells,Done); Cells:=nil;
 if (FName = '') or (FName = UntitledName) then begin
   DisposeStr(SName); SName := NewStr(UntitledName);
   If Owner<>nil then begin
    DisposeStr(PWindow(Owner)^.Title);
    PWindow(Owner)^.Title := NewStr(Cut(SName^,Bounds.B.X-Bounds.A.X-12));
    Owner^.Redraw;
   end;
   New(Cells, Init(10,10)); Exit
  end;
{$IFDEF OS_DOS}
 FName := lfGetLongFileName(lFExpand(FName));
{$ELSE}
 FName := lFExpand(FName);
{$ENDIF}
 S := New(PBufStream, Init(FName, stOpenRead, 2048));
 DisposeStr(SName); SName := NewStr(FName);
 if Owner<>nil then begin
  DisposeStr(PWindow(Owner)^.Title);
  PWindow(Owner)^.Title := NewStr(Cut(SName^,Bounds.B.X-Bounds.A.X-12));
 end;
 if S^.Status <> stOK then
  begin
   Dispose(S,Done);
   New(Cells, Init(10,10));
   FillChar(ColWidth, SizeOf(ColWidth), DefaultColWidth);
//   Owner^.Redraw;
   Exit
  end;
 S^.Read(ColWidth, Sizeof(ColWidth));
{ Cells := PCellCollection(S^.Get);}
 New(Cells, ShortLoad(S^));
 Dispose(S,Done);
 if Cells = nil then
  begin
   ErrMsg(erInvalidFileFormat);
   DisposeStr(SName);
   SName := NewStr(UntitledName);
   if Owner<>nil then begin
    DisposeStr(PWindow(Owner)^.Title);
    PWindow(Owner)^.Title := NewStr(Cut(SName^,Bounds.B.X-Bounds.A.X-12));
   end;
   New(Cells, Init(10,10));
   FillChar(ColWidth, Sizeof(ColWidth), 1);
  end;
 Modified := false;
 recalc(true);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TCalcView.SaveSheetAs;
 var S: PStream;
     FName: String;
     W: Word;
     PP: Pointer;
     Bounds: TRect;
begin
 GetBounds(Bounds);
 PP := @FName;
 if GetFileName(FName, '*.WKZ', GetString(dlSaveFileAs),
     GetString(dlSaveFileAs), fdOKButton{, hsSaveSheetAs, 0}) = cmCancel then Exit;
{$IFDEF OS_DOS}
 FName := lfGetLongFileName(lFExpand(FName));
{$ELSE}
 FName := lFExpand(FName);
{$ENDIF}
 S := New(PDosStream, Init(FName, stOpen));
 W := S^.Status;
 Dispose(S,Done);
 if W = 0 then
  begin
   if Msg(dlFileExist, @PP, mfYesButton + mfNoButton + mfWarning) <> cmYes then Exit;
  end;
 DisposeStr(SName); SName := NewStr(FName);
 if Owner<>nil then begin
  DisposeStr(PWindow(Owner)^.Title);
  PWindow(Owner)^.Title := NewStr(Cut(SName^,Bounds.B.X-Bounds.A.X-12));
  Owner^.Redraw;
 end;
 {PInfoView(CellInfo)^.SetInfo(' WORK ', Owner^.GetColor(12));}
 S := New(PBufStream, Init(FName, stCreate, 2048));
 S^.Write(ColWidth, Sizeof(ColWidth));
{ S^.Put(Cells);}
 Cells^.ShortStore(S^);
 if S^.Status <> stOK then
  begin
   Msg(dlCanNotWrite, @PP, mfError + mfOKButton);
  end;
 Modified := false;
 Dispose(S,Done);
 FileChanged(FName);
end;
        {-DataCompBoy-}

procedure TCalcView.SaveSheet;
 var S: PStream;
begin
 if SName^ = UntitledName then begin SaveSheetAs; Exit end;
 {PInfoView(CellInfo)^.SetInfo(' WORK ', Owner^.GetColor(12));}
 S := New(PBufStream, Init(SName^, stCreate, 2048));
 S^.Write(ColWidth, Sizeof(ColWidth));
 {S^.Put(Cells);}
 Cells^.ShortStore(S^);
 if S^.Status <> stOK then
   Msg(dlCanNotWrite, SName, mfError + mfOKButton)
 else
   Modified := false;
 Dispose(S,Done);
 FileChanged(SName^);
end;

procedure TCalcView.Copy;
 var
     X1, Y1, X2, Y2: Integer;
     Wrk: integer;

  procedure ToClip(P: PCellRec);
    begin
    With CellClipboard^.ReplaceItem(P^.Col, P^.Row, P^.S)^ do
      begin
      Options := P^.Options; Value := P^.Value; Decimals := P^.Decimals;
      end;
    end;

begin
 X1 := Delta.X + Cur.X; X2 := Mark.X;
 Y1 := Delta.Y + Cur.Y; Y2 := Mark.Y;
 if X1 > X2 then
   begin Wrk := X1; X1 := X2; X2 := Wrk; end;
 if Y1 > Y2 then
   begin Wrk := Y1; Y1 := Y2; Y2 := Wrk; end;
 if CellClipboard <> nil then Dispose(CellClipboard,Done); CellClipboard:=nil;
 New(CellClipboard, Init(10, 10));
 Cells^.ForRectangle(X1,Y1, X2,Y2, @ToClip);
 ClipRect.A.X := X1; ClipRect.A.Y := Y1;
 ClipRect.B.X := X2; ClipRect.B.Y := Y2;
end;

{В формуле S ссылки на ячейки (>=LX,>=LY) сдвигать на (DX,DY) }
function ReformFormula(const S: string; LX,LY, DX,DY: integer): String;
  const Signs = [';','[',']','{','}',#39,':','"','.','<',
                 '>',',','/','?','\','-','=','|','_','(',
                 ')','*','&','^','%','$','!','~','+', ' '];
  var S1, S2: String;
      X, X1: Byte;
      I, l: Integer;
      Y, Y1: AInt;
  label EndRowName;
  begin
  result := '='; I := 2;
  While (I <= Length(S)) do
    begin
    S1 := ''; S2 := '';
    While (I <= Length(S)) and  (S[I] in Signs) do
      begin result := result + S[I]; Inc(I) end;
    While (I <= Length(S)) and not (S[I] in Signs) do
      begin S1 := S1 + S[I]; Inc(I) end;
    if GetCellCoord(S1,X,Y) then
      if (X >= LX) and (Y >= LY) then
        begin
        if S1[1] = '@' then
          S2 := '@' + GetColName(X)
        else if (X+DX < 0) or (X+DX > 255) then
          S2 := '?'
        else
          S2 := GetColName(X+DX);
        { сейчас S2 - имя колонки}

        for l := 2 to length(S1)-1 do
          if S1[l] = '@' then
            begin
            S1 := '@' + GetRowName(Y); goto EndRowName;
            end;
        if (Y+DY < 0) or (Y+DY > MaxCellY)  then
          S1 := '?'
        else
          S1 := GetRowName(Y+DY);
        end;
EndRowName: { сейчас S1 - имя строки }

    result := result + S2 + S1;
    end;
  end;

procedure TCalcView.Paste;
 var I, J: Integer;

 procedure RewriteFormulaClip(P: PCellRec; DX, DY: integer);
  var S: String;
 begin
  if (P^.Options and coFormula) <> 0 then
    S := ReformFormula(P^.S, 0,0, DX,DY)
  else S := P^.S;
  With Cells^.ReplaceItem(P^.Col+DX, P^.Row+DY, S)^ do
    begin
    Options := P^.Options; Value := P^.Value; Decimals := P^.Decimals;
    end;
 end;

begin
 if CellClipboard = nil then Exit;
 Mark.X := Delta.X + Cur.X + ClipRect.B.X - ClipRect.A.X;
 Mark.Y := Delta.Y + Cur.Y + ClipRect.B.Y - ClipRect.A.Y;
 Clear;
 for I := 1 to CellClipboard^.Count do
   RewriteFormulaClip(CellClipboard^.At(I - 1),
     -ClipRect.A.X + Delta.X + Cur.X,
     -ClipRect.A.Y + Delta.Y + Cur.Y);
 Recalc(false);
 Modified := true;
end;

procedure TCalcView.Clear;
 var  X1, X2, Y1, Y2, K, L: Integer;
      Wrk: integer;
begin
 X1 := Delta.X + Cur.X; X2 := Mark.X;
 Y1 := Delta.Y + Cur.Y; Y2 := Mark.Y;
 if X1 > X2 then
   begin Wrk := X1; X1 := X2; X2 := Wrk; end;
 if Y1 > Y2 then
   begin Wrk := Y1; Y1 := Y2; Y2 := Wrk; end;
 for K := X1 to X2 do
   for L := Y1 to Y2 do
     Cells^.DelItem(K, L);
 Recalc(false);
 Modified := true;
end;

procedure TCalcView.RewriteFormula(var P: PCellRec; LX,LY, DX,DY: integer);
 var S : String;
     O, D: Integer;
     V: CReal;
begin
 if (P^.Options and coFormula) = 0 then
   exit;
 S := ReformFormula(P^.S, LX,LY, DX,DY);
 O := P^.Options; D := P^.Decimals; V := P^.Value;
 P := Cells^.ReplaceItem(P^.Col, P^.Row, S);
 with P^ do
  begin Options := O; Decimals := D; Value := V end;
end;

procedure TCalcView.InsertLine;
 var I, L: Integer;
     P: PCellRec;
begin
 L := Delta.Y + Cur.Y;
 for I := 1 to Cells^.Count do
  begin
   P := Cells^.At(I - 1);
   if P^.Row >= VScroll^.Max then
     Cells^.AtFree(I - 1)
   else
     begin
     if P^.Row >= L then Inc(P^.Row);
     RewriteFormula(P, 0,L,  0,1);
     end;
  end;
 ReCalc(false);
 Modified := true;
end;

procedure TCalcView.DeleteLine;
 var I, L: Integer;
     P: PCellRec;
begin
 L := Delta.Y + Cur.Y+1;
 for I := 1 to Cells^.Count do
  begin
   P := Cells^.At(I - 1);
   RewriteFormula(P, 0,L,  0,-1);
   if P^.Row = L-1 then
     begin Cells^.FreeItem(P); Cells^.AtPut(I - 1, nil) end
   else if P^.Row >= L then
     Dec(P^.Row);
  end;
 Cells^.Pack;
 ReCalc(false);
 Modified := true;
end;

procedure TCalcView.InsertCol;
 var I, J, L: Integer;
     P: PCellRec;
begin
 L := Delta.X + Cur.X;
 for I := 1 to Cells^.Count do
  begin
   P := Cells^.At(I - 1);
   if P^.Col = HScroll^.Max then
     Cells^.AtFree(I - 1)
   else
     begin
     if P^.Col >= L then Inc(P^.Col);
     RewriteFormula(P, L,0,  1,0);
     end;
  end;
 move(ColWidth[L], ColWidth[L+1],
   SizeOf(ColWidth[0])*(High(ColWidth)-L));
 ColWidth[L] := DefaultColWidth;
 ReCalc(false);
 Modified := true;
end;

procedure TCalcView.DeleteCol;
 var I, J, L: Integer;
     P: PCellRec;
begin
 L := Delta.X + Cur.X+1;
 for I := 1 to Cells^.Count do
  begin
   P := Cells^.At(I - 1);
   if P^.Col = L-1 then
     begin Cells^.FreeItem(P); Cells^.AtPut(I - 1, nil) end
   else
     begin
     if P^.Col >= L then Dec(P^.Col);
     RewriteFormula(P, L,0,  -1,0);
     end;
  end;
 move(ColWidth[L], ColWidth[L-1],
   SizeOf(ColWidth[0])*(High(ColWidth)-L+1));
 ColWidth[High(ColWidth)] := DefaultColWidth;
 Cells^.Pack;
 ReCalc(false);
 Modified := true;
end;

{ вычисление формулы; s[1] = '=' }
function TCalcView.CalcEval(const s: string; var Value: CReal): boolean;
  var
    R: CReal;
  begin
  R:=Evalue(System.Copy(S, 2, MaxStringLength), @self);
  if EvalueError then
    begin
    CalcError(erInvalidFormula); Value := 0;
    CalcEval := false;
    end
  else
    begin Value := R; CalcEval := true; end;
  end;

procedure TCalcView.ReCalc(Full: boolean);
  var
    StartRecalc: integer;
    ierr: integer;
  begin
  if not Cells^.TSort(StartRecalc) then
    begin
    with PCellRec(Cells^.At(StartRecalc-1)) do
      begin CurrentCalc.X := Col; CurrentCalc.Y := Row; end;
    CalcError(erRecurseTooDeep); exit;
    end;
  while StartRecalc <> 0 do
    begin
    with PCellRec(Cells^.At(StartRecalc-1))^ do
      begin
      CurrentCalc.X := Col; CurrentCalc.Y := Row;
      if (Options and coFormula) <> 0 then
        begin
        if not CalcEval(S, Value) then
          exit;
        end
      else if Full and ((Options and coValue) <> 0) then
        if S[1] = '=' then
          begin {После некоторых старых версий DN бывает флаг coValue
          без флага coFormula для формул, включающих только константные
          операнды. То, что TSort не воспринимал их, как формулы, не
          страшно, так как от других ячеек они не зависят. }
          Value := Evalue(system.Copy(S, 2, 255), @self);
          Options := Options or coValue;
          end
        else
          begin Val(S, Value, ierr); if ierr <> 0 then exit; end;
      StartRecalc := NextC;
      end;
    end;
  end;

procedure TCalcView.GotoCell;
 var X: Byte; Y: AInt;
     L: Integer;
begin
 if not GetCellCoord(Cell, X, Y) then
  begin
   ErrMsg(erGotoInvalidNumber);
   Exit;
  end;
 Marking := False;
 BlockDraw := True;
 Delta.X := X;
 Delta.Y := Y - (Size.Y - 2) div 2;
 VScroll^.SetValue(Delta.Y);
 Delta.Y := VScroll^.Value;
 Cur.X := 0; Cur.Y := Y - Delta.Y;
 L := 0;
 repeat
  Inc(L, ColWidth[Cur.X]);
  if Delta.X > 0 then begin Inc(Cur.X); Dec(Delta.X) end;
 until (Delta.X = 0) or (L > Size.X);
 if L > Size.X div 2 then begin Dec(Cur.X); Inc(Delta.X) end;
 HScroll^.SetValue(Delta.X);
 BlockDraw := False;
 Mark.X := Cur.X + Delta.X;
 Mark.Y := Cur.Y + Delta.Y;
 DrawView;
end;

procedure TCalcView.SearchCell;
 const Signs = [#0..#128] - ['A'..'Z','a'..'z','0'..'9','_'];
 var P: PCellRec;
     I, J: Integer;
     V: CReal;
     K, L: Byte;
     S,S1: String;

 function Found: Boolean;
 begin
  Found := True;
  SearchPos.Y := P^.Row; SearchPos.X := P^.Col;
  GotoCell(GetCellName(P^.Col, P^.Row));
  if WasReplace then
    begin
    if (SearchData.TxtOptions and 4 <> 0) then
      if (SearchData.CellOptions = 0) then
        begin
        S := P^.S; Delete(S, K+L, Length(S1));
        Insert(ReplaceData.S1, S, K+L);
        Cells^.ReplaceItem(P^.Col, P^.Row, S); DrawView; Exit
        end
      else
        begin
        if (P^.Options and coFormula) <> 0 then
             begin Found := False; Exit end;
        Val(ReplaceData.S1, V, J);
        Cells^.ReplaceItem(P^.Col, P^.Row, ReplaceData.S1)^.Value := V;
        DrawView; Exit
        end;
   end;
 end;

 procedure SearchSign;
 begin
  While (K <= Length(S)) and not (S[K] in Signs) do Inc(K);
  Inc(L, K);
  S := System.Copy(S, K, MaxStringLength);
 end;

begin
 if SearchData.S = '' then Exit;
 Val(SearchData.S, V, I);
 if SearchData.TxtOptions and 1 = 0 then S1 := UpStrg(SearchData.S)
                                    else S1 := SearchData.S;
 For I := 1 to Cells^.Count do
   begin
   P := Cells^.At(I - 1);
   if (P^.Row >= SearchPos.Y) and (P^.Col > SearchPos.X) then
     if (SearchData.CellOptions = 0) then
       begin
       if SearchData.TxtOptions and 1 = 0 then S := UpStrg(P^.S)
       else S := P^.S;
       K := Pos(S1, S); L := 0;
       if SearchData.TxtOptions and 2 = 0 then
         begin
         if K > 0 then
           begin if Found then Exit end
         end
       else
         repeat
           K := Pos(S1, S); L := 0;
           if K = 0 then
           else if (K = 1) or (Length(S) = Length(S1)) then
             begin
             if (Length(S) = Length(S1)) or (Length(S) <> Length(S1))
               and (S[Length(S1) + 1] in Signs)
             then begin if Found then Exit end else SearchSign;
             end
           else if (K = Length(S) - Length(S1) + 1) then
             begin
             if (S[K - 1] in Signs) then
               begin if Found then Exit end
             else SearchSign;
             end
           else if (S[K - 1] in Signs) and (S[Length(S1) + K] in Signs)
           then
             begin if Found then Exit
             end
           else SearchSign;
         until (K = 0) or (Length(S) < Length(S1));
     end else
      if (P^.Options and 3 <> 0) and (V = P^.Value) then
       begin if Found then Exit end;
   end;
 if not ContSearch and not WasReplace then
    ErrMsg(erTextNotFound);
 SearchCanceled := True;
end;


{------------------------------------------------------------------------------}
{ Write dBase Dbf }

procedure TDbfFieldCollection.FreeItem(Item : pointer); {KV}
begin
  if Item <> nil then Dispose(PDBFField(Item));
end;

constructor TDbaseWriter.Init(FileName: FNameStr; Mode: Word; Size: Sw_Word); {KV}
begin
  inherited Init(FileName,Mode,Size);
  CurRecord:=0;
  EofFlag:=false;
  New(Fields,Init(10,10));
  FillChar(Header,SizeOf(Header),0);
  Header.DBFIdent:=#3;
  if (Mode = stOpen) or (Mode = stOpenRead) then ReadFile;
end;

destructor TDbaseWriter.Done; {KV}
begin
  if Fields <> nil then Dispose(Fields,Done);
  Fields:=nil;
  inherited Done;
end;

procedure TDbaseWriter.AddField(const NameField : string; TypeField : char;
                                LenField : integer; DecField : integer); {KV}
var
  P : PDBFField;
begin
  New(P);
  FillChar(P^,SizeOf(TDBFField),0);
  StrPCopy(P^.FieldName,System.Copy(NameField,1,10));
  P^.FieldType:=TypeField;
  if TypeField = 'C' then
    P^.FLength.FieldLength:=LenField
  else begin
    P^.FLength.NumericLength:=LenField;
    P^.FLength.Decimals:=DecField;
  end;
  Fields^.Insert(P);
end;

procedure TDbaseWriter.AddRecord; {KV}
var
  Buf : PChar;
begin
  if Status <> stOk then Exit;
  with Header do begin
    inherited Seek(DataOffset+RecSize*LastRecord);
    GetMem(Buf,RecSize);
    FillChar(Buf^,RecSize,' ');
    inherited Write(Buf^,RecSize);
    FreeMem(Buf,RecSize);
    if Status = stOk then begin
      inc(LastRecord);
      inherited Seek(0);
      inherited Write(Header,SizeOf(Header));
    end;
    CurRecord:=LastRecord;
    EofFlag:=false;
  end;
end;

procedure TDbaseWriter.CreateFile; {KV}
var
  i : integer;
begin
  Header.RecSize:=1;
  for i:=0 to Fields^.Count-1 do begin
    with PDBFField(Fields^.At(i))^ do begin
      if FieldType = 'C' then inc(Header.RecSize,FLength.FieldLength)
      else inc(Header.RecSize,FLength.NumericLength);
    end;
  end;
  Header.DataOffset:=SizeOf(THeaderDbf)+(Fields^.Count)*SizeOf(TDbfField)+1;
  inherited Seek(0);
  inherited Write(Header,SizeOf(Header));
  for i:=0 to Fields^.Count-1 do begin
    inherited Write(PDBFField(Fields^.At(i))^,SizeOf(TDBFField));
  end;
  i:=$1A0D;
  inherited Write(i,2);
end;

procedure TDbaseWriter.DeleteRecord; {KV}
var
  Ch : char;
begin
  if (CurRecord < 1) or (CurRecord > Header.LastRecord) then Exit;
  inherited Seek(FieldOffsetInFile(1)-1);
  Ch:=#$2A;
  inherited Write(Ch,1);
end;

function TDbaseWriter.FCount : integer;
begin
  FCount:=Fields^.Count;
end;

function TDbaseWriter.FieldAsString(FieldIndex : integer) : string; {KV}
var
  Buf : array[0..2047] of char;
  i : integer;
begin
  inherited Seek(FieldOffsetInFile(FieldIndex));
  FillChar(Buf,SizeOf(Buf),' ');
  inherited Read(Buf,FieldLen(FieldIndex));
  i:=FieldLen(FieldIndex)-1;
  while (i >= 0) and (Buf[i] = ' ') do dec(i);
  Buf[i+1]:=#0;
  FieldAsString:=StrPas(Buf);
end;

function TDbaseWriter.FieldAsInteger(FieldIndex : integer) : integer; {KV}
var
  S : string;
  Value : integer;
  i : integer;
begin
  S:=FieldAsString(FieldIndex);
  if S = '' then Value:=0
  else begin
    Val(S,Value,i);
    if i <> 0 then Value:=0;
  end;
  FieldAsInteger:=Value;
end;

function TDbaseWriter.FieldAsFloat(FieldIndex : integer) : double; {KV}
var
  S : string;
  Value : double;
  i : integer;
begin
  S:=FieldAsString(FieldIndex);
  if S = '' then Value:=0.0
  else begin
    Val(S,Value,i);
    if i <> 0 then Value:=0.0;
  end;
  FieldAsFloat:=Value;
end;

procedure TDbaseWriter.FieldPutString(FieldIndex : integer; FieldValue : string); {KV}
var
  P : PDBFField;
  Buf : array[0..2047] of char;
  L : AWord;
  StrLen : integer;
begin
  if (FieldIndex < 1) or (FieldIndex > Fields^.Count) then Exit;
  P:=PDBFField(Fields^.At(FieldIndex-1));
  FillChar(Buf,SizeOf(Buf),' ');
  L:=FieldLen(FieldIndex);
  if L > 2047 then L:=2047;
  StrLen:=Length(FieldValue);
  if StrLen > L then begin
    StrLen:=L;
    FieldValue:=System.Copy(FieldValue,1,StrLen);
  end;
  StrPCopy(Buf,FieldValue); // Можно было использовать Move,
                               // но при работе с длинными строками
                               // так безопаснее
  Buf[StrLen]:=' '; // Для удаления #0, который вставляет StrPCopy
  inherited Seek(FieldOffsetInFile(FieldIndex));
  inherited Write(Buf,L);
end;

procedure TDbaseWriter.FieldPutInteger(FieldIndex : integer; FieldValue : integer); {KV}
var
  S : string;
  L : AWord;
  D : byte;
  R : AWord;
begin
  if (FieldIndex < 1) or (FieldIndex > Fields^.Count) then Exit;
  L:=FieldLen(FieldIndex);
  D:=FieldDec(FieldIndex);
  if D > 0 then R:=L-D-1 else R:=L;
  Str(FieldValue:L,S);
  if D > 0 then begin
    S:=S+'.'; inc(R);
    while R < L do begin S:=S+'0'; inc(R); end;
  end;
  FieldPutString(FieldIndex,S);
end;

procedure TDbaseWriter.FieldPutFloat(FieldIndex : integer; FieldValue : double); {KV}
var
  S : string;
begin
  if (FieldIndex < 1) or (FieldIndex > Fields^.Count) then Exit;
  Str(FieldValue:FieldLen(FieldIndex):FieldDec(FieldIndex),S);
  FieldPutString(FieldIndex,S);
end;

function TDbaseWriter.FieldName(FieldIndex : integer) : string; {KV}
begin
  FieldName:=StrPas(PDBFField(Fields^.At(FieldIndex-1))^.FieldName);
end;

function TDbaseWriter.FieldLen(FieldIndex : integer) : AWord; {KV}
begin
  with PDBFField(Fields^.At(FieldIndex-1))^ do begin
    if FieldType = 'C' then FieldLen:=FLength.FieldLength
    else FieldLen:=FLength.NumericLength;
  end;
end;

function TDbaseWriter.FieldDec(FieldIndex : integer) : byte; {KV}
begin
  with PDBFField(Fields^.At(FieldIndex-1))^ do begin
    if FieldType = 'C' then FieldDec:=0
    else FieldDec:=FLength.Decimals;
  end;
end;

function TDbaseWriter.FieldPos(NameField : string) : integer; {KV}
var
  FieldIndex, i : integer;
begin
  UpStr(NameField);
  FieldIndex:=0;
  for i:=0 to Fields^.Count-1 do begin
    if UpStrg(StrPas(PDBFField(Fields^.At(i))^.FieldName)) = NameField then begin
      FieldIndex:=i+1;
      break;
    end;
  end;
  FieldPos:=FieldIndex;
end;

function TDbaseWriter.FieldType(FieldIndex : integer) : char; {KV}
begin
  FieldType:=PDBFField(Fields^.At(FieldIndex-1))^.FieldType;
end;

function TDbaseWriter.FieldOffsetInBuffer(FieldIndex : integer) : integer; {KV}
var
  Offset : integer;
  i : integer;
begin
  Offset:=1;
  for i:=0 to FieldIndex-2 do begin
    with PDBFField(Fields^.At(i))^ do begin
      if FieldType = 'C' then inc(Offset,FLength.FieldLength)
      else inc(Offset,FLength.NumericLength);
    end;
  end;
  FieldOffsetInBuffer:=offset;
end;

function TDbaseWriter.FieldOffsetInFile(FieldIndex : integer) : longint; {KV}
begin
  FieldOffsetInFile:=Header.DataOffset+(CurRecord-1)*Header.RecSize+
    FieldOffsetInBuffer(FieldIndex);
end;

procedure TDbaseWriter.First; {KV}
begin
  CurRecord:=1;
  EofFlag:=(Header.LastRecord = 0);
end;

procedure TDbaseWriter.DBGoTo(NewRecord : integer); {KV}
begin
  CurRecord:=NewRecord;
  EofFlag:=(Header.LastRecord = 0) or (Header.LastRecord < CurRecord);
end;

procedure TDbaseWriter.Next; {KV}
begin
  if CurRecord < Header.LastRecord then begin
    inc(CurRecord);
    EofFlag:=false;
  end
  else EofFlag:=true;
end;

procedure TDbaseWriter.ReadFile; {KV}
var
  R : TDBFField;
  P : PDBFField;
begin
  inherited Seek(0);
  FillChar(Header,SizeOf(Header),0);
  Fields^.FreeAll;
  inherited Read(Header,SizeOf(Header));
  if Status <> stOk then Exit;
  inherited Seek(SizeOf(Header));
  repeat
    inherited Read(R,1);
    if R.FieldName[0] <> #$0D then begin
      inherited Read(R.FieldName[1],SizeOf(R)-1);
      New(P);
      Move(R,P^,SizeOf(R));
      Fields^.Insert(P);
    end;
  until (R.FieldName[0] = #$0D) or (Status <> stOk);
  EofFlag:=(Header.LastRecord = 0);
  CurRecord:=1;
end;

procedure TDbaseWriter.RecallRecord; {KV}
var
  Ch : char;
begin
  if (CurRecord < 1) or (CurRecord > Header.LastRecord) then Exit;
  inherited Seek(FieldOffsetInFile(1)-1);
  Ch:=#$20;
  inherited Write(Ch,1);
end;

procedure TDbaseWriter.WriteEndOfFile; {KV}
var
  Ch : char;
begin
  Ch:=#$1A;
  inherited Seek(Header.DataOffset+Header.LastRecord*Header.RecSize);
  inherited Write(Ch,1);
end;

{------------------------------------------------------------------------------}
{ Write Excel Xls }

type
  TXlsBOF = packed record
    recNo: AWord;       // Record number
    recSize: AWord;     // Record size
    vers: AWord;        // Version number (0500 for BIFF5 and BIFF7)
    dt: AWord;          // Substream type: 0005h = Workbook globals
                        //                 0006h = Visual Basic module
                        //                 0010h = Worksheet or dialog sheet
                        //                 0020h = Chart
                        //                 0040h = Microsoft Excel 4.0 macro sheet
                        //                 0100h = Workspace file
    rupBuild: AWord;    // Build identifier (internal use only)
    rupYear: AWord;     // Build year (internal use only)
  end;

  TXlsEOF = packed record
    recNo: AWord;       // Record number
    res1: AWord;
  end;

// LABEL: Cell Value, String Constant (204h)
// A LABEL record describes a cell that contains a string constant.
// The rw field contains the 0-based row number.
// The col field contains the 0-based column number.
// The string length is contained in the cch field and must be in the range of
// 0000h..00FFh (0..255). The string itself is contained in the rgch field.
  TXlsLABEL = packed record
    recNo: AWord;       // Record number
    recSize: AWord;     // Record size
    rw: AWord;          // Cell Row
    col: AWord;         // Cell Col
    ixfe: AWord;        // Index to the XF record
    cch: AWord;         // Length of the string
   (* rgch: var *)      // The string
  end;

// BLANK: Cell Value, Blank Cell (201h)
// A BLANK record describes an empty cell.
// The rw field contains the 0-based row number.
// The col field contains the 0-based column number.
  TXlsBLANK = packed record
    recNo: AWord;       // Record number
    recSize: AWord;     // Record size
    rw: AWord;          // Cell Row
    col: AWord;         // Cell Col
    ixfe: AWord;        // Index to the XF record
  end;

// NUMBER: Cell Value, Floating-Point Number (203h)
// A NUMBER record describes a cell containing a constant floating-point number.
// The rw field contains the 0-based row number.
// The col field contains the 0-based column number.
// The number is contained in the num field in 8-byte IEEE floating-point format.
  TXlsNUMBER = packed record
    recNo: AWord;       // Record number
    recSize: AWord;     // Record size
    rw: AWord;          // Cell Row
    col: AWord;         // Cell Col
    ixfe: AWord;        // Index to the XF record
    num: Double;        // Floating-point number value
  end;

// BOOLERR: Cell Value, Boolean or Error (205h)
// A BOOLERR record describes a cell that contains a constant Boolean or error value.
// The rw field contains the 0-based row number.
// The col field contains the 0-based column number.
  TXlsBOOLERR = packed record
    recNo: AWord;       // Record number
    recSize: AWord;     // Record size
    rw: AWord;          // Cell Row
    col: AWord;         // Cell Col
    ixfe: AWord;        // Index to the XF record
    bBoolErr: Byte;     // Boolean value or error value
    fError: Byte;       // Boolean/error flag
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
  CDefXlsBLANK.recSize := SizeOf(TXlsBLANK) - 2 * SizeOf(AWord);
  CDefXlsBLANK.rw := Row;
  CDefXlsBLANK.col := Col;
  CDefXlsBLANK.ixfe := XF;
  inherited Write(CDefXlsBLANK, SizeOf(CDefXlsBLANK));
end;

procedure TExcelWriter.WriteBOF; {KV}
const
  CDefXlsBOF: TXlsBOF =
    (recNo: CXlsBOF;
     recSize: SizeOf(TXlsBOF) - 2 * SizeOf(AWord);
     vers: 0;
     dt: $10;
     rupBuild: 0;
     rupYear: 0);
begin
  inherited Write(CDefXlsBOF, SizeOf(CDefXlsBOF));
end;

procedure TExcelWriter.WriteBOOL(const Data: Boolean; const Col, Row, XF: AWord); {KV}
const
  Values: array [Boolean] of Byte = (0, 1);
var
  CDefXlsBOOLERR: TXlsBOOLERR;
begin
  CDefXlsBOOLERR.recNo := CXlsBOOLERR;
  CDefXlsBOOLERR.recSize := SizeOf(TXlsBOOLERR) - 2 * SizeOf(AWord);
  CDefXlsBOOLERR.rw  := Row;
  CDefXlsBOOLERR.col := Col;
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

procedure TExcelWriter.WriteERROR(const Data: Byte; const Col, Row, XF: AWord); {KV}
var
  CDefXlsBOOLERR: TXlsBOOLERR;
begin
  CDefXlsBOOLERR.recNo := CXlsBOOLERR;
  CDefXlsBOOLERR.recSize := SizeOf(TXlsBOOLERR) - 2 * SizeOf(AWord);
  CDefXlsBOOLERR.rw := Row;
  CDefXlsBOOLERR.col := Col;
  CDefXlsBOOLERR.ixfe := XF;
  CDefXlsBOOLERR.bBoolErr := Data;
  CDefXlsBOOLERR.fError := 1;
  inherited Write(CDefXlsBOOLERR, SizeOf(CDefXlsBOOLERR));
end;

procedure TExcelWriter.WriteLABEL(const Data: string; const Col, Row, XF: AWord); {KV}
var
  S: string;
  CDefXlsLABEL: TXlsLABEL;
begin
  CDefXlsLABEL.recNo := CXlsLABEL;
  S := Data;
  CDefXlsLABEL.cch := Length(S);
  if CDefXlsLABEL.cch > 255 then
  begin
    CDefXlsLABEL.cch := 255;
    SetLength(S, CDefXlsLABEL.cch);
  end;
  CDefXlsLABEL.rw := Row;
  CDefXlsLABEL.col := Col;
  CDefXlsLABEL.ixfe := XF;
  CDefXlsLABEL.recSize := (SizeOf(TXlsLABEL) - 2 * SizeOf(AWord)) + CDefXlsLABEL.cch;
  inherited Write(CDefXlsLABEL, SizeOf(CDefXlsLABEL));
  inherited Write(S[1], CDefXlsLABEL.cch);
end;

procedure TExcelWriter.WriteNUMBER(const Data: Double; const Col, Row, XF: AWord); {KV}
var
  CDefXlsNUMBER: TXlsNUMBER;
begin
  CDefXlsNUMBER.recNo := CXlsNUMBER;
  CDefXlsNUMBER.recSize := SizeOf(TXlsNUMBER) - 2 * SizeOf(AWord);
  CDefXlsNUMBER.rw := Row;
  CDefXlsNUMBER.col := Col;
  CDefXlsNUMBER.ixfe := XF;
  CDefXlsNUMBER.num := Data;
  inherited Write(CDefXlsNUMBER, SizeOf(CDefXlsNUMBER));
end;

end.
