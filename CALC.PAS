{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  1.6.RC1
//  dn200-viewer-history.patch
//  dn200-dbf_wkz_hscroll_bar.patch
//  dn2215-windows_manager_changes_2.patch
//
//  2.3.0
//  dn247-text_and_file_find_improve.patch
//  dn2510_wkz_calculate_fix.patch
//  dn2510_wkz_search_fix.patch
//  dn2421_wkz_go_to_cell_fix.patch
//  dn2510_wkz_open_sheet_fix.patch
//  dn269-calculator_bugs_fix.patch
//  dn2624_wkz_open_not_wkz_file_fix.patch
//  dn2624-wkz_import_and_export.patch
//  dn2628_wkz_import_fix.patch
//
//  2.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
unit Calc;

interface

uses Advance, Objects, Views, CellsCol, Drivers, Dialogs, Messages,
     HistList, UniWin, Commands, DNHelp, RStrings, Calculat;

const
     CellClipboard : PCellCollection = nil;
     ClipRect      : record A, B: TPoint; end = (A:(X:0; Y:0); B:(X:0; Y:0));
     FalseStr      : String[10] = 'False';
     TrueStr       : String[10] = 'True';

type
    PCalcView = ^TCalcView;
    PCalcWindow = ^TCalcWindow;
    TCalcWindow = object(TUniWindow)
     CalcView: PCalcView;
     RealName: String;
     constructor Init(Bounds: TRect; AName: String); {DataCompBoy}
{--- start -------- Eugeny Zvyagintzev ---------}
     Procedure ChangeBounds(Var R: TRect); Virtual;
{--- finish -------- Eugeny Zvyagintzev ---------}
     constructor Load(var S: TStream);
     procedure Store(var S: TStream);
     procedure HandleEvent(var Event: TEvent); virtual;
     destructor Done; virtual;
    end;

    TCalcView = object(TView)

     FocusEvent: TEvent;
     Info, CellInfo: PView;

     HScroll, VScroll: PScrollBar;
     Delta, Cur, Mark: TPoint;
     Cells: PCellCollection;
     NumC: Byte;
     Error, ReError, Marking, BlockDraw, Modified: Boolean;
     CellWidth: Array[Byte] of Byte;
     CurrentCalc, SearchPos, ErrorCell: TPoint;
     SName: PString; {DataCompBoy}
     constructor Init(Bounds: TRect; AInfo, ACellInfo: PView;
                      AHScroll, AVScroll: PScrollBar);
     destructor Done; virtual;

     constructor Load(var S: TStream);
     procedure Store(var S: TStream);

     procedure Draw; virtual;
     function  Valid(Command: Word): Boolean; virtual;
     procedure SetState(AState: Word; Enable: Boolean); virtual;
     function  GetPalette: PPalette; virtual;
     procedure HandleEvent(var Event: TEvent); virtual;
     Function  GetCellValue( S : String; var Value: CReal) : boolean;
     procedure CalcError( Index: TStrIdx ); virtual;
     procedure ModifyCell(X, Y: Integer);
     procedure LoadSheet(FName: String); {DataCompBoy}
     procedure SaveSheet;
     procedure SaveSheetAs;
     function  AskSave: Boolean;
     procedure Copy; virtual;
     procedure Paste; virtual;
     procedure Clear; virtual;
     procedure ReCalc;
     procedure InsertLineCol(Command: Word); virtual;
     procedure DeleteLineCol(Command: Word); virtual;
     procedure GotoCell(Cell: String);
     procedure SearchCell;
    end;

    PCalcInfo = ^TCalcInfo;
    TCalcInfo = object(TInputLine)
     CalcView: PCalcView;
     constructor Load(var S: TStream);
     procedure Store(var S: TStream);
     function GetPalette: PPalette; virtual;
     procedure HandleEvent(var Event: TEvent); virtual;
    end;

    PInfoView = ^TInfoView;
    TInfoView = object(TView)
      InfoStr: String;
      InfoAttr: Byte;
      {constructor Load(var S: TStream);}
      {procedure Store(var S: TStream); }
      procedure SetInfo(S: String; Attr: Byte);
      procedure Draw; virtual;
    end;

function GetFileName(var FileName: String; Mask, Title, ALabel: String; Buttons: Word): Word;

implementation

uses Memory, DNApp, DNStdDlg, advance1, advance2, Dos, LFN,
     MicroEd2, Histries, xTime, Gauge, Collect,
     {$IFNDEF NONBP}BStrings{$ELSE}Strings{$ENDIF}
     ;

type
  THeaderDBF = record     {KV}
    DBFIdent   : char;    { $03 - Нет MEMO; $83 - Есть MEMO }
                          { FoxBase+, FoxPro, dBaseIII+, dBaseIV, no memo - 0x03 }
                          { FoxBase+, dBaseIII+ with memo - 0x83 }
                          { FoxPro with memo - 0xF5 }
                          { dBaseIV with memo - 0x8B }
                          { dBaseIV with SQL Table - 0x8E }

    Year       : byte;    { две последние цифры года }
    Month      : byte;    { month number }
    Day        : byte;    { day number }
    LastRecord : longint; { номер последней записи }
    DataOffset : AWord;   { смещение первой записи относительно начала файла}
    RecSize    : AWord;   { размер записи с учетом символа удаления }
    Reserv1    : AWord;
    WaitTrans  : Byte;    { dB IV }
    Reserv2    : Array[0..12] of Byte;
    Multiindex : Byte;
    Reserv3    : Array[0..2] of Byte;
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
    function FieldName(FieldIndex : integer) : string; { Warning! FieldIndex from 1}
    function FieldLen(FieldIndex : integer) : AWord; { Warning! FieldIndex from 1}
    function FieldDec(FieldIndex : integer) : byte; { FieldIndex >= 1}
    function FieldPos(NameField : string) : integer; { FieldIndex >= 1}
    function FieldType(FieldIndex : integer) : char;
    function FieldOffsetInBuffer(FieldIndex : integer) : integer; { FieldIndex >= 1}
    function FieldOffsetInFile(FieldIndex : integer) : longint; { FieldIndex >= 1}
    procedure First;
    procedure DBGoTo(NewRecord : integer);
    procedure Next;
    procedure ReadFile;
    procedure RecallRecord;
    procedure WriteEndOfFile;
  end;

const  SearchData: record S: String[240]; CellOptions, TxtOptions: Word; end =
                    (S:''; CellOptions:0; TxtOptions:0);
       ReplaceData: record S, S1: String[240]; CellOptions, TxtOptions: Word; end =
                    (S:''; S1:''; CellOptions:0; TxtOptions:0);
       WasReplace: Boolean = False;
       ContSearch: Boolean = False;
       SearchCanceled: Boolean = False;
       CurCalcView: PCalcView = nil;

function AGetCellValue(VarName: string; var Value: CReal): boolean;
begin
 if CurCalcView<>nil
 then AGetCellValue:=CurCalcView^.GetCellValue(VarName, Value)
 else AGetCellValue:=false;
end;

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
  FileName := {$IFNDEF OS2}lfGetLongFileName{$ENDIF}(S);
end;
        {-DataCompBoy-}

function ContainCell(Cell, Formula: String): Boolean;
 const Signs = ['#','+','-','=','*','&','^','(',')','<','>', ';', ' ',
                '%','[',']','{','}','!','|','/','\',',','?'];
 var I, J: Integer;
     FR, T: String;
     FRX, CX, TX: byte;
     FRY, CY, TY: integer;
begin
 ContainCell := False;
 UpStr(Formula);
 repeat
  I := Pos('SUM', Formula);
  if I=0 then I := Pos('MUL', Formula);
  if I>0 then begin
    for J:=I+2 to Length(Formula) do if Formula[J]='(' then break;
    FR:='';
    for J:=J+1 to Length(Formula) do
      if Formula[J]=':' then break else
      if Formula[J]<>' ' then FR:=FR+Formula[J];
    T:='';
    for J:=J+1 to Length(Formula) do
      if Formula[J]=')' then break else
      if Formula[J]<>' ' then T:=T+Formula[J];
    GetCellCoord(Cell, CX, CY);
    GetCellCoord(FR, FRX, FRY);
    GetCellCoord(T, TX, TY);
    if (CX>=FRX) and (CX<=TX) and (CY>=FRY) and (CY<=TY)
      then begin ContainCell := true; exit end;
    Delete(Formula, I, J-I);
  end;
 until I=0;
 Repeat
  I := Pos(Cell, Formula);
  if (I > 1) and (Formula[I-1] in Signs) and
     ((I+Length(Cell)>Length(Formula)) or (Formula[I+Length(Cell)] in Signs))
   then begin ContainCell := true; exit end;
  Dec(Formula[0], I);
  Move(Formula[I], Formula[1], Byte(Formula[0]));
 Until I = 0;
end;

{--- start -------- Eugeny Zvyagintzev ---- 26-06-2002 -----}
procedure SetLength(Var S: String;i: Integer); {JOHN_SW}
begin
 Delete(S,i+1,Length(S));
end;
{--- finish -------- Eugeny Zvyagintzev ---- 26-06-2002 -----}


{-------------------------       TInfoView       -------------------------}
{
constructor TInfoView.Load;
begin
  inherited Load(S);
end;

procedure TInfoView.Store;
begin
  inherited Store(S);
end;
}
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
     P, P1: PView;
     I  : Integer;
     s  : String;
begin
{--- start -------- Eugeny Zvyagintzev ---------}
 I:=PosChar('|', AName);
 If I > 0 Then
  begin
   S:=Copy(AName, 1, I-1);
   AName:=Copy(AName, I+1, 255);
  end;
{--- finish -------- Eugeny Zvyagintzev ---------}
 if AName = '' then AName := 'Untitled.WKZ'
               else if GetFileAttr(AName+#0) and Directory<>0
                     then AName := 'Untitled.WKZ'
                     else AName := {$IFNDEF OS2}lfGetLongFileName{$ENDIF}(lFExpand(AName));
 If I <= 0 Then
  S:=AName;
 TWindow.Init(Bounds, Cut(S,Bounds.B.X-Bounds.A.X-12), 0);
 RealName:=S;
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
 P := New(PCalcInfo, Init(R, 240));
 P^.GrowMode := gfGrowHiX;
 P^.Options := P^.Options or ofSelectable;
 Insert(P);

 R.Assign(1, 2, 7, 4);
 P1 := New(PInfoView, Init(R));
 PInfoView(P1)^.InfoStr := '';
 PInfoView(P1)^.InfoAttr := GetColor(9);
 P1^.Options := P1^.Options {or ofFramed};
 Insert(P1);

 GetExtent(R); R.Grow(-1,-1);
 Inc(R.A.Y, 2);
 CalcView := New(PCalcView, Init(R, P, P1,
                  MakeScrollBar(sbHorizontal + sbHandleKeyboard),
                  MakeScrollBar(sbVertical + sbHandleKeyboard)));
 CalcView^.Options := CalcView^.Options;
 Insert(CalcView);
 CalcView^.LoadSheet(AName);
end;
        {-DataCompBoy-}

constructor TCalcWindow.Load;
begin
  inherited Load(S);
  GetSubViewPtr(S, CalcView);
end;

{--- start -------- Eugeny Zvyagintzev ---------}
{Now horizontal scrollbar can't hide bottom-right resizing corner}
Procedure TCalcWindow.ChangeBounds;
Var
   R1: TRect;
Begin
 Inherited ChangeBounds(R);
 CalcView^.HScroll^.GetBounds(R1);
 R1.B.X:=Size.X-2;
 CalcView^.HScroll^.SetBounds(R1);
End;
{--- finish -------- Eugeny Zvyagintzev ---------}

procedure TCalcWindow.Store;
begin
  inherited Store(S);
  PutSubViewPtr(S, CalcView);
end;

procedure TCalcWindow.HandleEvent;
begin
 case Event.What of
  evCommand: case Event.Command of
               cmClose, cmQuit: if not CalcView^.AskSave then ClearEvent(Event);
             end;
  evBroadcast:
             case Event.Command of
               cmFindView:
                  if PString(Event.InfoPtr)^=CnvString(CalcView^.SName) then begin
                    Self.Select;
                    ClearEvent(Event);
                  end;
             end;
 end;
 inherited HandleEvent(Event);
end;

Destructor TCalcWindow.Done;
begin
 StoreViewInfo(@Self);
 Inherited Done;
end;

{-----------------------------    TCalcInfo     ---------------------------}

constructor TCalcInfo.Load;
begin
  inherited Load(S);
  GetPeerViewPtr(S, CalcView);
end;

procedure TCalcInfo.Store;
begin
  inherited Store(S);
  PutPeerViewPtr(S, CalcView);
end;

function TCalcInfo.GetPalette;
 const S: String[4] = #13#13#14#13;
begin
 GetPalette := @S;
end;

procedure TCalcInfo.HandleEvent;

 function EndEdit: Boolean;
  label 1,2;
  var S, S1: String;
      R: CReal;
      I: Integer;
 begin
  GetData(S); EndEdit := True;
  with CalcView^ do begin
   Modified := On;
   CurrentCalc.X := Delta.X + Cur.X; CurrentCalc.Y := Delta.Y + Cur.Y;
   While S[Byte(S[0])] = ' ' do Dec(S[0]);
   S1 := DelSpaces(S);
   if (S1[1] = '=') and (S1[0] > #1) then
   begin
    DelFC(S1);
    Error:=false;
    CurCalcView:=CalcView;
    R:=Evalue(S1, AGetCellValue);
    S1:='='+S1;
    if EvalueError then
     begin
      S := ''; CalcView^.CalcError(erInvalidFormula);
      EndEdit := False;
      Goto 1;
     end;
{--- start -------- Eugeny Zvyagintzev ---- 22-05-2002 -----}
{To prevent overflow}
{R: CReal = Extended}
{Value: Real}
{R may be large then Value}
    if R > 1.7e38 then
     begin
      R:=1.7e38;
      MessageBox(GetString(dlOverflow)+': '+GetCellName(CurrentCalc.X,CurrentCalc.Y)+' '+S1, @Self, mfOKButton + mfError);
     end;
    if R < 2.9e-39 then
     begin
      R:=2.9e-39;
      MessageBox(GetString(dlOverflow)+': '+GetCellName(CurrentCalc.X,CurrentCalc.Y)+' '+S1, @Self, mfOKButton + mfError);
     end;
{--- finish -------- Eugeny Zvyagintzev ---- 22-05-2002 -----}
    with Cells^.ReplaceItem(Delta.X + Cur.X, Delta.Y + Cur.Y, S1)^ do
     begin
      Value := R;
      if (Options and 3) = 0 then
       begin
        Options := coValue or coDec or coFormula;
        Decimals := 2;
       end else Options := (Options and $FFFC) or coValue or coFormula;
      if ((Options shr 4) and 7) = 0 then
       begin
        Options := Options or coDec or coFormula;
        Decimals := 2
       end;
     end;
    ModifyCell(Delta.X + Cur.X, Delta.Y + Cur.Y);
    EndEdit := not ReError;
    Exit;
   end else if S <> '' then
             begin
              Val(S, R, I);
              if I = 0 then
               begin
                with Cells^.ReplaceItem(Delta.X + Cur.X, Delta.Y + Cur.Y, S)^ do
                 begin
                  Value := R;
                  if Options and 3 = 0 then
                   begin
                    Options := coValue or coLeft;
                    Decimals := 2;
                   end else Options := (Options and $FFFC) or coValue;
                 end;
                ModifyCell(Delta.X + Cur.X, Delta.Y + Cur.Y);
                EndEdit := not ReError;
                Exit;
               end;
             end;
1:
    if S = '' then Cells^.DelItem(Delta.X + Cur.X, Delta.Y + Cur.Y)
              else with Cells^.ReplaceItem(Delta.X + Cur.X, Delta.Y + Cur.Y, S)^ do
                    begin
                     Options := Options and $FFFC;
                     Decimals := 0;
                    end;
   end;
 end;

 procedure Transfer;
 begin
  CalcView^.FocusEvent := Event;
  if EndEdit then PWindow(Owner)^.SelectNext(False);
  ClearEvent(Event)
 end;

begin
 case Event.What of
  evKeyDown : case Event.KeyCode of
               kbUp, kbDown, kbPgUp, kbPgDn, kbCtrlPgUp,
               kbCtrlPgDn, kbEnter: Transfer;
               kbLeft: if CurPos = 0 then Transfer;
               kbRight: if CurPos > Length(CnvString(Data)) - 1 then Transfer;
               kbTab: if not EndEdit then ClearEvent(Event);
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
 EventMask := $FFFF; Info := AInfo; CellInfo := ACellInfo;
 PCalcInfo(Info)^.CalcView := @Self;
 FocusEvent.What := evNothing; FillChar(CellWidth, SizeOf(CellWidth), 11);
 Marking := False; BlockDraw := False; Modified := Off;
 Cells := nil;
 Delta.X := 0; Delta.Y := 0;
 Cur := Delta; Mark  := Cur;
 HScroll^.SetParams(0, 0, 255, 1, 1);
 VScroll^.SetParams(0, 0, MaxCellY - 1, Size.Y - 3, 1);
 SName:=nil;
end;

constructor TCalcView.Load;
begin
  inherited Load(S);
  GetPeerViewPtr(S, Info);
  GetPeerViewPtr(S, CellInfo);
  GetPeerViewPtr(S, HScroll);
  GetPeerViewPtr(S, VScroll);
  S.Read(Delta, SizeOf(Delta)*3);
  S.Read(NumC, 6); {###}
  S.Read(CellWidth, SizeOf(CellWidth));
  S.Read(CurrentCalc, SizeOf(CurrentCalc)*3);
  SName:=S.ReadStr;
  {Cells := PCellCollection(S.Get);}
  New(Cells, ShortLoad(S));
  FocusEvent.What := evNothing;
end;

procedure TCalcView.Store;
begin
  inherited Store(S);
  PutPeerViewPtr(S, Info);
  PutPeerViewPtr(S, CellInfo);
  PutPeerViewPtr(S, HScroll);
  PutPeerViewPtr(S, VScroll);
  S.Write(Delta, SizeOf(Delta)*3);
  S.Write(NumC, 6); {###}
  S.Write(CellWidth, SizeOf(CellWidth));
  S.Write(CurrentCalc, SizeOf(CurrentCalc)*3);
  S.WriteStr(SName);
{  S.Put(Cells);}
  Cells^.ShortStore(S);
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

procedure TCalcView.Draw;
 const Dig : String [26] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

 var
     B, B1: TDrawBuffer;
     BC: Array [0..127] of record C: Char; A: Byte; end Absolute B;
     I, J, K, L: Integer;
     C1, C2, C3: Byte;
     S, S1: String;
     P: PCellRec;
     X1, X2, Y1, Y2: Integer;
     CurPos: TPoint;
     StX, EnX, A1, A2 : Byte;

 label 1;

begin
 if BlockDraw then Exit;
 C1 := GetColor(1); C2 := GetColor(2); C3 := GetColor(3);
 if Q = nil then New(Q);
1:
 CurPos.X := Delta.X + Cur.X; CurPos.Y := Delta.Y + Cur.Y;
 if CurPos.X > 255 then Cur.X := 255 - Delta.X;
 if CurPos.Y >= MaxCellY then Cur.Y := MaxCellY - 1 - Delta.Y;
 if Cur.Y < 0 then begin J := Cur.Y; Cur.Y := 0; VScroll^.SetValue(Delta.Y + J); Exit end;
 if Cur.X < 0 then begin J := Cur.X; Cur.X := 0; HScroll^.SetValue(Delta.X + J); Exit end;
 CurPos.X := Delta.X + Cur.X; CurPos.Y := Delta.Y + Cur.Y;
 X1 := CurPos.X; X2 := Mark.X;
 Y1 := CurPos.Y; Y2 := Mark.Y;
 if X1 > X2 then
     asm mov AX, X1; mov BX, X2; mov X1, BX; mov X2, AX end;
 if Y1 > Y2 then
     asm mov AX, Y1; mov BX, Y2; mov Y1, BX; mov Y2, AX end;
 if Y2 > 4095 then Y2 := 4095;
 MoveChar(B, ' ', C1, Size.X);
 BC[6].C := #179;
 MoveChar(B1, #196, C1, Size.X);
 WordRec(B1[6]).Lo := Byte(#197);
 NumC := 0; I := 7; J := Delta.X; StX := I; EnX := Size.X;
 While (I < Size.X) and (J <= HScroll^.Max) do
  begin
   K := CellWidth[J]; L := CellWidth[J + 1];
   FillChar(S, K + 1, 32);
   S[K] := #179;
   if J < 26 then S[1 + K div 2] := Dig[J + 1]
      else begin S[K div 2] := Dig[J div 26];
                 S[K div 2 + 1] := Dig[J mod 26 + 1] end;
   S[0] := Char(K);
   WordRec(B1[I + K - 1]).Lo := Byte(#193);
   MoveStr(B[I], S, C1);
   if J < X1 then Inc(StX, K);
   if (J > X2) and (EnX > I) then EnX := I;
   if (J = HScroll^.Max) and (J <= X2) then EnX := I + K;
   Inc(J);
   if I + K <= Size.X then Inc(NumC);
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
     for J := 0 to NumC + 2 do
      begin
       K := CellWidth[Delta.X + J];
       if Q^[J, I] >= 0 then
        begin
         P := Cells^.At(Q^[J, I]);
         if Size.X > L then
          begin
           FillChar(S, Size.X - L + 1, 32);
           S[0] := Char(Size.X - L);
          end;
         case (P^.Options shr 4) and 7 of
          0: S1 := P^.S;
          1: Str(P^.Value:0:P^.Decimals, S1);
          2: begin
              Str(P^.Value:0:20, S1);
              While S1[Length(S1)] = '0' do Dec(S1[0]);
              if S1[Length(S1)] = '.' then Dec(S1[0]);
              S1 := MakeComma(S1);
             end;
          3: begin
              Str(P^.Value, S1);
              A1 := Pos('E', S1);
              if A1 > 0 then
              begin
               A2 := A1 - 1;
               While (S1[A2] = '0') and
                     (S1[A2 - 1] <> '.') do Dec(A2);
               Move(S1[A1], S1[A2 + 1], Length(S1) - A1 + 1);
               Dec(S1[0], A1 - A2 - 1);
              end;
             end;
           4:if P^.Value = 0 then S1 := FalseStr
                             else S1 := TrueStr;
           5:begin
              MakeCurrency(P^.Value, S1);
              {Str(P^.Value:0:2, S1);
              S1 := '$'+MakeComma(S1);}
             end;
           6:begin
              Str((P^.Value*100):0:2, S1);
              S1 := MakeComma(S1)+'%';
             end;
           7:S1 := '';
         end;
         if (P^.Options and $C = 0) or (Length(S1) > K) then Move(S1[1], S[1], Length(S1))
          else if (P^.Options and $C = coLeft) then Move(S1[1],S[1 + K - Length(S1)], Length(S1))
               else Move(S1[1],S[1 + (K - Length(S1)) div 2], Length(S1));
         if L < 0 then L := 0;
         MoveStr(B[L], S, C2);
        end;
       if (Cur.X = J) and (Cur.Y = I) then
        begin
         MoveColor(B[L], K, C3);
         if Q^[J, I] >= 0 then
          begin
           P := Cells^.At(Q^[J, I]);
           S := P^.S;
           case P^.Options and 3 of
            coText: WriteStr(0, 0, '   T  ', 2);
            coValue: WriteStr(0, 0, '  V   ', 2);
            coFormula: WriteStr(0, 0, '   F  ', 2);
            coValue or coFormula: WriteStr(0, 0, '  VF  ', 2);
           end;
          end else
           begin
            WriteStr(0, 0, GetString(dlWKZ_Empty), 2);
            S := '';
           end;
         PCalcInfo(Info)^.SetData(S);
         Info^.Draw;
        end;
       L := L + K;
      end;
    end;
   BC[6].C := #179; BC[6].A := C1;
   if (Delta.Y + I >= Y1) and (Delta.Y + I <= Y2) and (StX < EnX)
      then MoveColor(B[StX], EnX - StX, C3);
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

 var Inf: PCalcInfo;
     S: String;
     St: PStream;
     MaxX, MaxY, K, L: Integer;
     P: PCellRec;
     FName: String;
     X1, X2, Y1, Y2: Integer;

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
          { Если в колонке текст или ошибка в формуле,
            то эта колонка сохраняется как текстовая}
          T:='C'
        else
        if (T = #0) and ((P^.Options and $F0) = coBool) then
          { Если тип колонки еще не оределен и первое найденное значение
            имеет формат Boolean, то устанавливаем тип Boolean}
          T:='L'
        else
        if T = #0 then T:='N'; { По умолчанию - число}
      end;
    end;
    { Делаем второй проход когда типы полей уже известны
     для определения размерности}
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

  function kvFormatValue(P : PCellRec) : string; {KV}
  var
    S1 : string;
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
           if S1[Length(S1)] = '.' then SetLength(S1, Length(S1));
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
    kvFormatValue:=S1;
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
      MessageBox(GetString(dlNoFileFound)+FileName, Nil, mfError + mfOKButton);
      Exit;
    end;
    Cells^.FreeAll;
    CurRow:=0;
    while not Eof(F.T) do begin
      Readln(F.T,S);
      if S <> '' then begin {KV: Просто на всякий случай}
        CurCol:=0;
        StartCh:=1;
        while StartCh <= Length(S) do begin
          EndCh:=StartCh;
          while (EndCh <= Length(S)) and (S[EndCh] <> ColSeparator) do inc(EndCh);
          if EndCh > StartCh then begin
            CellValue:=System.Copy(S,StartCh,EndCh-StartCh);
            P:=Cells^.NewItem(CurCol,CurRow,CellValue);
            if (Length(CellValue) > 2) and (CellValue[1] = '=') then
              P^.Options:=P^.Options or coValue or coDec or coFormula
            else begin
              Val(CellValue, R, I);
              if I = 0 then begin
                P^.Options:=P^.Options or coValue or coDec;
                if Pos('.',CellValue) > 0 then
                  P^.Decimals:=Length(CellValue)-Pos('.',CellValue);
                P^.Value:=R;
              end
              else P^.Options:=P^.Options or coText;
            end;
          end;
          StartCh:=EndCh+1;
          inc(CurCol);
        end;
      end;
      inc(CurRow);
    end;
    Close(F.T);
    ReCalc;
    Owner^.Redraw;
    Modified:=On;{JOHN_SW}
  end;

  procedure ImportFromDbf; {KV}
  var
    S : TDbaseWriter;
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
    S.Init(FileName,stOpenRead,16384);
    if S.Status <> stOk then begin
      MessageBox(GetString(dlNoFileFound)+FileName, Nil, mfError + mfOKButton);
      S.Done;
      Exit;
    end;

    Cells^.FreeAll;
    S.First;
    while not S.EofFlag do begin
      for i:=1 to S.FCount do begin
        CellValue:=S.FieldAsString(i);
        DelRight(CellValue);
        if CellValue <> '' then begin
          P:=Cells^.NewItem(i-1,S.CurRecord-1,CellValue);
          case S.FieldType(i) of
            'N' : begin
                    DelLeft(P^.S);
                    P^.Options:=P^.Options or coValue or coDec;
                    P^.Decimals:=S.FieldDec(i);
                    P^.Value:=S.FieldAsFloat(i);
                  end;
            'L' : begin
                    if System.Copy(UpStrg(CellValue),1,1) = 'T' then begin
                      P^.S:='1'; P^.Value:=1;
                    end
                    else begin
                      P^.S:='0'; P^.Value:=0;
                    end;
                  end;
          else
            if (Length(CellValue) > 2) and (CellValue[1] = '=') then
              P^.Options:=P^.Options or coValue or coDec or coFormula
            else P^.Options:=P^.Options or coText;
          end;
        end;
      end;
      S.Next;
    end;
    S.Done;
    ReCalc;
    Owner^.Redraw;
    Modified:=On;{JOHN_SW}
  end;

  procedure ExportToCsv; {KV}
  var
    FileName : string;
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
    if GetFileName(FileName, '*.CSV', GetString(dlExportTitle), GetString(dlExportLabel),
                   fdOKButton+fdHelpButton) = cmCancel then Exit;
    if ExistFile(FileName) then begin
      PS:=@FileName;
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
      lAssignText(F,FileName);
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
               (((P^.Options and 3) = coFormula) and (not CalcFormulaFlag)) then
              Write(F.T,P^.S)
            else
            if ((P^.Options and 3) = coValue) or
               (((P^.Options and 3) = (coFormula or coValue)) and CalcFormulaFlag) then begin
              if UseFormatFlag then S:=kvFormatValue(P)
              else begin
                { Оставляем 5 знаков после запятой, на случай если
                  неправильно указан формат}
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
      FileChanged(S); {JOHN_SW}
    end;
  end;

  procedure ExportToDbf; {KV}
  const Dig : String [26] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
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
        if i < 26 then FieldName:=Dig[i+1]
        else begin
          FieldName:=Dig[i div 26]+Dig[i mod 26 + 1];
        end;
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
      i:=1; { Номер текущего поля внутри записи}
      S.AddRecord; { Добавляем новую пустую запись}
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
    FileChanged(FileName); {JOHN_SW}
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
  var R: record Display, Justify: Word; Dec: String[2]; Protect: Word; end;
      I, J, X1, Y1, X2, Y2: Integer;
 begin
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
     asm mov AX, X1; mov BX, X2; mov X1, BX; mov X2, AX end;
   if Y1 > Y2 then
     asm mov AX, Y1; mov BX, Y2; mov Y1, BX; mov Y2, AX end;
   Val(R.Dec, J, I);
   for I := 1 to Cells^.Count do
    begin
     P := Cells^.At(I-1);
     if (P^.Col >= X1) and (P^.Col <= X2) and (P^.Row >= Y1) and (P^.Row <= Y2)
      then begin P^.Options := (P^.Options and 3)
                        or ((R.Justify and 3) shl 2)
                        or ((R.Display and 7) shl 4)
                        or ((R.Protect and 1) shl 7);
                 P^.Decimals := J;
           end;
    end;
  end;
  DrawView
 end;

 procedure CE; begin ClearEvent(Event) end;

begin
 Inf := PCalcInfo(Info);
 if GetState(sfFocused) and (FocusEvent.What <> evNothing) then
   begin
    PutEvent(Event); Event := FocusEvent; FocusEvent.What := evNothing;
   end;
 TView.HandleEvent(Event);
 case Event.What of
  evCommand: case Event.Command of
              cmGetName: PString(Event.InfoPtr)^ := GetString(dlWKZName) + ' - '
                         + Cut(SName^, 49 - Length(GetString(dlWKZName)));
              cmImportFromCsv: begin ImportFromCsv; ClearEvent(Event); end;
              cmImportFromDbf: begin ImportFromDbf; ClearEvent(Event); end;
              cmExportToCsv: begin ExportToCsv; ClearEvent(Event); end;
              cmExportToDbf: begin ExportToDbf; ClearEvent(Event); end;
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
                             GetString(dlOpenFileName), fdOpenButton{ + fdReplaceButton{, hsLoadSheet, 0});
                        if K = cmCancel then Exit;
                        LoadSheet(FName);
                      end;
              cmChangeWidth:
                      begin
                       repeat
                        GetKeyEvent(Event);
                        if Event.CharCode = #$e0 then Event.CharCode := #0;
                        case Event.KeyCode of
                         kbLeft: if CellWidth[Delta.X + Cur.X]> 1 then
                                   begin Dec(CellWidth[Delta.X + Cur.X]);
                                         DrawView end;
                        kbRight: if CellWidth[Delta.X + Cur.X]< ScreenWidth then
                                   begin Inc(CellWidth[Delta.X + Cur.X]);
                                         DrawView end;
                        end;
                       until (Event.KeyCode = kbESC) or (Event.KeyCode = kbEnter);
                       CE;
                      end;
              cmChangeFormat: begin ChangeFormat; CE; end;
              cmPaste: begin Paste; EndMarking end;
              cmCopy: begin Copy; EndMarking end;
              cmCut: begin Copy; Clear; EndMarking end;
              cmClear: begin Clear; EndMarking end;
              cmInsertLine, cmInsertColumn:
                begin InsertLineCol(Event.Command); EndMarking end;
              cmDeleteLine, cmDeleteColumn:
                begin DeleteLineCol(Event.Command); EndMarking end;
              cmRecalc: begin Recalc; DrawView; CE end;
              cmGotoCell: begin
                           if HistoryCount(hsGotoCell) = 0 then S := 'A1'
                                 else S := HistoryStr(hsGotoCell, 0);
                           if ExecResource(dlgGotoCellNumber, S) = cmOK
                             then GotoCell(S);
                           ClearEvent(Event); Exit
                          end;
              cmFindCell: begin
                           if ExecResource(dlgFindCell, SearchData) = cmOK then
                            begin
                             SearchPos.X := -1; SearchPos.Y := 0;
                             WasReplace := False;
                             SearchCell;
                            end;
                           CE;
                          end;
              cmSearchAgain: begin
                              SearchCanceled := False;
                              SearchPos.X := Delta.X + Cur.X;
                              SearchPos.Y := Delta.Y + Cur.Y;
                              repeat SearchCell
                              until not WasReplace or not ContSearch or SearchCanceled;
                              CE;
                             end;
              cmReplaceCell: begin
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
  evKeyDown: case Event.KeyCode of
              kbDoubleAlt: CE;
              kbDel: begin Clear; EndMarking end;
              kbLeft: if Cur.X > 0 then
                      begin
                       CheckMark;
                       Dec(Cur.X); SetMark;
                       DrawView;
                       CE; Exit
                      end else Exit;
              kbRight: if (Cur.X < NumC)  then
                      begin
                       CheckMark;
                       if (Delta.X + Cur.X = HScroll^.Max) then
                        begin if Cur.X > 0 then Dec(Cur.X); SetMark; Exit end;
                       Inc(Cur.X); SetMark; DrawView;
                       ClearEvent(Event); Exit
                      end else
                       if (Delta.X + Cur.X = HScroll^.Max) then
                        begin CheckMark; if Cur.X > 0 then Dec(Cur.X); SetMark; Exit end
                         else Exit;
                kbUp: if Cur.Y > 0 then
                      begin
                       CheckMark;
                       Dec(Cur.Y); SetMark; DrawView;
                       ClearEvent(Event); Exit
                      end else Exit;
              kbHome: begin
                       CheckMark;
                       Cur.Y := 0; Cur.X := 0; SetMark;
                       HScroll^.SetValue(0);
                       VScroll^.SetValue(0);
                       DrawView; ClearEvent(Event); Exit
                      end;
              kbEnd: begin
                      CheckMark;
                      MaxX := 0; MaxY := 0; Cur.Y := 0; Cur.X := 0;
                      for L := 1 to Cells^.Count do
                       begin
                        P := Cells^.At(L - 1);
                        if P^.Col > MaxX then MaxX := P^.Col;
                        if P^.Row > MaxY then MaxY := P^.Row;
                       end;
                      if (L + CellWidth[Delta.X + Cur.X] < Size.X) and (MaxX > 0) then
                      begin
                       L := 7;
                       while (L <= Size.X) and (MaxX >= 0) do
                        begin
                         K := CellWidth[Delta.X + Cur.X];
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
              kbDown: if (Cur.Y < Size.Y - 3)  then
                      begin
                       CheckMark;
                       if (Delta.Y + Cur.Y >= VScroll^.Max) then
                        begin if Cur.Y > 0 then Dec(Cur.Y); SetMark; Exit end;
                       Inc(Cur.Y); SetMark; DrawView;
                       ClearEvent(Event); Exit
                      end else
                       if (Delta.Y + Cur.Y >= VScroll^.Max) then
                        begin if Cur.Y > 0 then Dec(Cur.Y); SetMark; Exit end
                          else Exit;
               kbEnter: begin
                         Marking := False; SetMark; DrawView;
                         PWindow(Owner)^.SelectNext(False);
                         ClearEvent(Event);
                        end;
               kbTab: begin
                       Marking := False; SetMark; DrawView;
                      end;
               else if Event.CharCode > #31 then
                    begin
                     Marking := False; SetMark; DrawView;
                     PWindow(Owner)^.SelectNext(True);
                     Event.InfoPtr := Info;
                     Info^.PutEvent(Event);
                     ClearEvent(Event)
                    end;
             end;
  evBroadcast: case Event.Command of
                cmScrollBarChanged: if Event.InfoPtr = HScroll then
                                         begin CheckMark; Delta.X := HScroll^.Value; SetMark;
                                               DrawView; Exit end
                               else if Event.InfoPtr = VScroll then
                                         begin CheckMark; Delta.Y := VScroll^.Value; SetMark;
                                               DrawView; Exit end
               end;
 end;
 Marking := False;
end;
        {-DataCompBoy-}

function TCalcView.GetCellValue( S : String; var Value: CReal) : boolean;
 var X: Byte;
     Y, I: Integer;
     P: PCellRec;
     AFromX, AToX: Byte;
     AFromY, AToY: Integer;

 function IsNeeded(PP: PCellRec): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
 begin
  IsNeeded := (PP^.Row = Y) and (PP^.Col = X)
 end;

 procedure DoSum(PP: PCellRec); {$IFDEF BIT_16}far;{$ENDIF}
 begin
  if (PP^.Row>=AFromY) and (PP^.Row<=AToY) and
     (PP^.Col>=AFromX) and (PP^.Col<=AToX) then
   if (PP <> nil) and (PP^.Options and 3 <> 0)
    then Value := Value + PP^.Value
 end;

 procedure DoMul(PP: PCellRec); {$IFDEF BIT_16}far;{$ENDIF}
 begin
  if (PP^.Row>=AFromY) and (PP^.Row<=AToY) and
     (PP^.Col>=AFromX) and (PP^.Col<=AToX) then
   if (PP <> nil) and (PP^.Options and 3 <> 0)
    then Value := Value * PP^.Value
 end;

begin
 DelSpaces(S); UpStr(S);
 if (S[1]='S') and (S[2]='U') and (S[3]='M') and (S[4]='(') and
    (S[length(s)]=')') and
    ((CharCount(':', S)=1) or (CharCount(',', S)=1)) then begin
  Dec(S[0], 5);
  Move(S[5], S[1], Length(S));
  I := Pos(':', S); If I=0 then I:=Pos(',', S);
  if GetCellCoord(System.Copy(S, 1, I-1), AFromX, AFromY) and
     GetCellCoord(System.Copy(S, I+1,255), AToX, AToY) then begin
   Value:=0;
   Cells^.ForEach(@DoSum);
   GetCellValue:=True;
  end else GetCellValue:=false
 end else
 if (S[1]='M') and (S[2]='U') and (S[3]='L') and (S[4]='(') and
    (S[length(s)]=')') and
    ((CharCount(':', S)=1) or (CharCount(',', S)=1)) then begin
  Dec(S[0], 5);
  Move(S[5], S[1], Length(S));
  I := Pos(':', S); If I=0 then I:=Pos(',', S);
  if GetCellCoord(System.Copy(S, 1, I-1), AFromX, AFromY) and
     GetCellCoord(System.Copy(S, I+1,255), AToX, AToY) then begin
   Value:=1;
   Cells^.ForEach(@DoMul);
   GetCellValue:=True;
  end else GetCellValue:=false
 end else
  if not GetCellCoord(S, X, Y) then GetCellValue:=false else begin
   GetCellValue := True;
   P := Cells^.FirstThat(@IsNeeded);
   if (P <> nil) and (P^.Options and 3 <> 0)
    then Value := P^.Value
    else Value := 0;
  end;
end;

procedure TCalcView.CalcError;
begin
 MessageBox(GetString( Index ) + ' ' + GetCellName(CurrentCalc.X,CurrentCalc.Y), @Self, mfOKButton + mfError);
 Error := True;
end;

procedure TCalcView.ModifyCell(X, Y: Integer);

const MaxRec = 300;

var Err: Boolean;
    P: PCellRec;
    Recurse: Array [1..MaxRec] of record X: Byte; Y: Integer;end;
    NumRec: Integer;

 procedure Modify(AX, AY: Integer);
  var I: Integer;
      S: String[6];
      R: CReal;
 begin
  if Err or Error then Exit;
  for I := 1 to NumRec do
   if (Recurse[I].X = AX) and (Recurse[I].Y = AY) then
   begin CalcError(erRecurseTooDeep); Err := True; Exit end;
  Inc(NumRec);
  if NumRec > MaxRec then
   begin CalcError(erDeepDependence); Err := True; Exit end;
  Recurse[NumRec].X := AX;
  Recurse[NumRec].Y := AY;
  S := GetCellName(AX, AY);
  for I := 1 to Cells^.Count do
   begin
    if Error or Err then Exit;
    P := Cells^.At(I-1);
    if P^.Options and coFormula <> 0 then
     if ContainCell(S, P^.S) then
      begin
       CurrentCalc.X := P^.Col; CurrentCalc.Y := P^.Row;
       CurCalcView:=@Self;
       R:=Evalue(System.Copy(P^.S, 2, 255), AGetCellValue);
       if EvalueError then
        begin
         R := 0; CalcError(erInvalidFormula);
        end;
{--- start -------- Eugeny Zvyagintzev ---- 27-06-2002 -----}
{To prevent overflow}
       if R > 1.7e38 then
        begin
         R:=1.7e38;
         MessageBox(GetString(dlOverflow)+': '+GetCellName(CurrentCalc.X,CurrentCalc.Y)+' '+
                    System.Copy(P^.S, 2, 255), @Self, mfOKButton + mfError);
        end;
       if R < 2.9e-39 then
        begin
         R:=2.9e-39;
         MessageBox(GetString(dlOverflow)+': '+GetCellName(CurrentCalc.X,CurrentCalc.Y)+' '+
                    System.Copy(P^.S, 2, 255), @Self, mfOKButton + mfError);
        end;
{--- finish -------- Eugeny Zvyagintzev ---- 27-06-2002 -----}
       P^.Value := R;
       Modify(P^.Col, P^.Row);
      end;
   end;
  Dec(NumRec);
 end;

begin
 {PInfoView(CellInfo)^.SetInfo(' WORK ', Owner^.GetColor(12));}
 Modified := On;
 Err := False; ReError := False; NumRec := 0;
 Modify(X, Y); ReError := Err;
 {DrawView;}
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
 AskSave := On; if not Modified then Exit;
 A := MessageBox(GetString(dlWorkSheet)+Cut(SName^,40)+GetString(dlNotSaved),
                 nil, mfWarning+mfYesNoCancel);
 if A <> cmCancel then Modified := Off;
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
 if (FName = '') or (FName = 'Untitled.WKZ') then begin
   DisposeStr(SName); SName := NewStr('Untitled.WKZ');
   If Owner<>nil then begin
    DisposeStr(PWindow(Owner)^.Title);
    PWindow(Owner)^.Title := NewStr(Cut(SName^,Bounds.B.X-Bounds.A.X-12));
    Owner^.Redraw;
   end;
   New(Cells, Init(10,10)); Exit
  end;
 FName := {$IFNDEF OS2}lfGetLongFileName{$ENDIF}(lFExpand(FName));
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
   Owner^.Redraw;
   Exit
  end;
 S^.Read(CellWidth, Sizeof(CellWidth));
{ Cells := PCellCollection(S^.Get);}
 New(Cells, ShortLoad(S^));
 Dispose(S,Done);
 if Cells = nil then
  begin
   DisposeStr(SName);
   SName := NewStr('Untitled.WKZ');
   if Owner<>nil then begin
    DisposeStr(PWindow(Owner)^.Title);
    PWindow(Owner)^.Title := NewStr(Cut(SName^,Bounds.B.X-Bounds.A.X-12));
   end;
   New(Cells, Init(10,10));
   FillChar(CellWidth, Sizeof(CellWidth), 11);
   ErrMsg(erInvalidFileFormat);
  end;
 Modified := Off;
 Owner^.ReDraw;
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
 FName := {$IFNDEF OS2}lfGetLongFileName{$ENDIF}(lFExpand(FName));
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
 S^.Write(CellWidth, Sizeof(CellWidth));
{ S^.Put(Cells);}
 Cells^.ShortStore(S^);
 if S^.Status <> stOK then
  begin
   Msg(dlCanNotWrite, @PP, mfError + mfOKButton);
   Error := True;
  end;
 Modified := Off;
 Dispose(S,Done);
 FileChanged(FName);
end;
        {-DataCompBoy-}

procedure TCalcView.SaveSheet;
 var S: PStream;
begin
 if SName^ = 'Untitled.WKZ' then begin SaveSheetAs; Exit end;
 {PInfoView(CellInfo)^.SetInfo(' WORK ', Owner^.GetColor(12));}
 S := New(PBufStream, Init(SName^, stCreate, 2048));
 S^.Write(CellWidth, Sizeof(CellWidth));
 {S^.Put(Cells);}
 Cells^.ShortStore(S^);
 if S^.Status <> stOK then
  begin
   Msg(dlCanNotWrite, SName, mfError + mfOKButton);
   Error := True;
  end;
 Modified := Off;
 Dispose(S,Done);
 FileChanged(SName^);
end;

procedure TCalcView.Copy;
 var I: Integer;
     X1, Y1, X2, Y2: Integer;
     P: PCellRec;
begin
 X1 := Delta.X + Cur.X; X2 := Mark.X;
 Y1 := Delta.Y + Cur.Y; Y2 := Mark.Y;
 if X1 > X2 then
   asm mov AX, X1; mov BX, X2; mov X1, BX; mov X2, AX end;
 if Y1 > Y2 then
   asm mov AX, Y1; mov BX, Y2; mov Y1, BX; mov Y2, AX end;
 if CellClipboard <> nil then Dispose(CellClipboard,Done); CellClipboard:=nil;
 New(CellClipboard, Init(10, 10));
 for I := 1 to Cells^.Count do
  begin
   P := Cells^.At(I - 1);
   if (P^.Col >= X1) and (P^.Col <= X2) and (P^.Row >= Y1) and (P^.Row <= Y2) then
       With CellClipboard^.ReplaceItem(P^.Col, P^.Row, P^.S)^ do begin
         Options := P^.Options; Value := P^.Value; Decimals := P^.Decimals
        end;
  end;
 ClipRect.A.X := X1; ClipRect.A.Y := Y1;
 ClipRect.B.X := X2; ClipRect.B.Y := Y2;
end;

function ReformAddr(S1: String; X, Y, RX, RY: Word): String;
 var X1: Byte;
begin
   X1 := 0;
   if S1[1] <> '@' then X := RX else X1 := 1;
   if PosChar('@', System.Copy(S1, Byte(S1[1] = '@')+1, 255)) = 0
    then Y := RY else X1 := X1 or 2;
   S1 := GetCellName(X, Y);
   if X1 and 1 <> 0 then S1 := '@' + S1;
   if X1 and 2 <> 0 then
    begin
     X1 := 1;
     While (X1 < Length(S1)) and ((S1[X1] >= '@') and (S1[X1] <= 'Z')) do Inc(X1);
     Insert('@', S1, X1);
    end;
   ReformAddr := S1
end;

procedure TCalcView.Paste;
 var I, J: Integer;
     P: PCellRec;

 procedure RewriteFormula;
  const Signs = [';','[',']','{','}',#39,':','"','.','<',
                 '>',',','/','?','\','-','=','|','_','(',
                 ')','*','&','^','%','$','!','~','+', ' '];
  var S, S1: String;
      X, X1: Byte;
      I, Y, Y1: Integer;
 begin
  if P^.Options and 3 = coFormula then
   begin
    S := '='; I := 2;
    While (I <= Length(P^.S)) do
     begin
      S1 := '';
      While (P^.S[I] in Signs) and (I <= Length(P^.S)) do
            begin S := S + P^.S[I]; Inc(I) end;
      While not (P^.S[I] in Signs) and (I <= Length(P^.S)) do
            begin S1 := S1 + P^.S[I]; Inc(I) end;
      if GetCellCoord(S1,X,Y) then
         S1 := ReformAddr(S1, X, Y, X - ClipRect.A.X + Delta.X + Cur.X,
                                    Y - ClipRect.A.Y + Delta.Y + Cur.Y);
      S := S + S1;
     end;
    end else S := P^.S;

   With Cells^.ReplaceItem(P^.Col - ClipRect.A.X + Delta.X + Cur.X,
                           P^.Row - ClipRect.A.Y + Delta.Y + Cur.Y, S)^ do
    begin Options := P^.Options; Value := P^.Value; Decimals := P^.Decimals end;

 end;

begin
 if CellClipboard = nil then Exit;
 Mark.X := Delta.X + Cur.X + ClipRect.B.X - ClipRect.A.X;
 Mark.Y := Delta.Y + Cur.Y + ClipRect.B.Y - ClipRect.A.Y;
 Clear;
 for I := 1 to CellClipboard^.Count do
  begin
   P := CellClipboard^.At(I - 1);
   RewriteFormula;
  end;
  for I := Delta.X + Cur.X to Mark.X do
   for J := Delta.Y + Cur.Y to Mark.Y do
     ModifyCell(I, J);
end;

procedure TCalcView.Clear;
 var X1, X2, Y1, Y2, K, L: Integer;
begin
 X1 := Delta.X + Cur.X; X2 := Mark.X;
 Y1 := Delta.Y + Cur.Y; Y2 := Mark.Y;
 if X1 > X2 then
  asm mov AX, X1; mov BX, X2; mov X1, BX; mov X2, AX end;
 if Y1 > Y2 then
  asm mov AX, Y1; mov BX, Y2; mov Y1, BX; mov Y2, AX end;
 for K := X1 to X2 do
  for L := Y1 to Y2 do
   begin
    Cells^.DelItem(K, L);
    ModifyCell(K, L);
   end;
end;

procedure TCalcView.InsertLineCol;
 var I, J, L: Integer;
     P: PCellRec;
     O, D: Integer;
     V: CReal;

procedure RewriteFormula;
 const Signs = [';','[',']','{','}',#39,':','"','.','<', ' ',
                '>',',','/','?','\','-','=','|','_','(',
                ')','*','&','^','%','$',' ','!','~','+'];
 var S, S1 : String;
     X: Byte;
     I, Y: Integer;
begin
 S := '='; I := 2;
 While (I <= Length(P^.S)) do
  begin
   S1 := '';
   While (P^.S[I] in Signs) and (I <= Length(P^.S)) do
         begin S := S + P^.S[I]; Inc(I) end;
   While not (P^.S[I] in Signs) and (I <= Length(P^.S)) do
         begin S1 := S1 + P^.S[I]; Inc(I) end;
   if GetCellCoord(S1,X,Y) then
     case Command of
      cmInsertLine: if (Y >= L) then S1 := ReformAddr(S1, X, Y, X, Y + 1);
      cmInsertColumn: if (X >= L) then S1 := ReformAddr(S1, X, Y, X + 1, Y);
     end;
   S := S + S1;
  end;
  O := P^.Options; D := P^.Decimals; V := P^.Value;
  P := Cells^.ReplaceItem(P^.Col, P^.Row, S);
  with P^ do
   begin Options := O; Decimals := D; Value := V end;
end;

begin
 if Command = cmInsertLine then L := Delta.Y + Cur.Y
                           else L := Delta.X + Cur.X;
 for I := 1 to Cells^.Count do
  begin
   P := Cells^.At(I - 1);
   case Command of
    cmInsertLine : begin
                    if P^.Row >= L then Inc(P^.Row);
                    if P^.Row >= VScroll^.Max then Cells^.AtFree(I - 1)
                   end;
  cmInsertColumn : begin
                    if P^.Col >= L then Inc(P^.Col);
                    if P^.Col >= HScroll^.Max then Cells^.AtFree(I - 1)
                   end;
   end;
   if P^.Options and 3 = coFormula then RewriteFormula;
  end;
  ReCalc;
end;

procedure TCalcView.DeleteLineCol;
 var I, J, L: Integer;
     P: PCellRec;
     O, D: Integer;
     V: CReal;

 procedure RewriteFormula;
  const Signs = [';','[',']','{','}',#39,':','"','.','<', ' ',
                 '>',',','/','?','\','-','=','|','_','(',
                 ')','*','&','^','%','$',' ','!','~','+'];
  var S, S1 : String;
      X: Byte;
      I, Y: Integer;
 begin
  S := '='; I := 2;
  While (I <= Length(P^.S)) do
   begin
    S1 := '';
    While (P^.S[I] in Signs) and (I <= Length(P^.S)) do
          begin S := S + P^.S[I]; Inc(I) end;
    While not (P^.S[I] in Signs) and (I <= Length(P^.S)) do
          begin S1 := S1 + P^.S[I]; Inc(I) end;
    if GetCellCoord(S1,X,Y) then
      case Command of
       cmDeleteLine: if (Y > L) then S1 := ReformAddr(S1, X, Y, X, Y - 1);
       cmDeleteColumn: if (X > L) then S1 := ReformAddr(S1, X, Y, X - 1, Y);
      end;
    S := S + S1;
   end;
  O := P^.Options; D := P^.Decimals; V := P^.Value;
  P := Cells^.ReplaceItem(P^.Col, P^.Row, S);
  with P^ do
   begin Options := O; Decimals := D; Value := V end;
 end;

begin
 if Command = cmDeleteLine then L := Delta.Y + Cur.Y
                           else L := Delta.X + Cur.X;
 for I := 1 to Cells^.Count do
  begin
   P := Cells^.At(I - 1);
   if P^.Options and 3 = coFormula then RewriteFormula;
   case Command of
    cmDeleteLine:
     if P^.Row = L then begin Cells^.FreeItem(P); Cells^.AtPut(I - 1, nil) end else
      if P^.Row > L then Dec(P^.Row);
    cmDeleteColumn:
     if P^.Col = L then begin Cells^.FreeItem(P); Cells^.AtPut(I - 1, nil) end else
      if P^.Col > L then Dec(P^.Col);
   end;
  end;
 Cells^.Pack;
 ReCalc;
end;

procedure TCalcView.ReCalc;
 var I: Integer;
     P: PCellRec;
     R: CReal;
begin
 for I := 1 to Cells^.Count do
  begin
   P := Cells^.At(I-1);
   if P^.Options and coFormula <> 0 then
     begin
      CurrentCalc.X := P^.Col; CurrentCalc.Y := P^.Row;
      Error:=false;
      CurCalcView:=@Self;
      R:=Evalue(System.Copy(P^.S, 2, 255), AGetCellValue);
      if EvalueError then
       begin
        R := 0; CalcError(erInvalidFormula);
       end;
{--- start -------- Eugeny Zvyagintzev ---- 22-05-2002 -----}
{To prevent overflow}
      if R > 1.7e38 then
       begin
        R:=1.7e38;
        MessageBox(GetString(dlOverflow)+': '+GetCellName(CurrentCalc.X,CurrentCalc.Y)+' '+
                   System.Copy(P^.S, 2, 255), @Self, mfOKButton + mfError);
       end;
      if R < 2.9e-39 then
       begin
        R:=2.9e-39;
        MessageBox(GetString(dlOverflow)+': '+GetCellName(CurrentCalc.X,CurrentCalc.Y)+' '+
                   System.Copy(P^.S, 2, 255), @Self, mfOKButton + mfError);
       end;
{--- finish -------- Eugeny Zvyagintzev ---- 22-05-2002 -----}
      P^.Value := R;
      ModifyCell(P^.Col, P^.Row);
     end;
  end;
end;

procedure TCalcView.GotoCell;
 var X: Byte;
     Y,L: Integer;
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
  Inc(L, CellWidth[Cur.X]);
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
{--- start -------- Eugeny Zvyagintzev ---- 12-04-2002 -----}
{Now DN will display Search Progress Window}
     Tmr: TEventTimer;
     Inf: PWhileView;
     CancelSearch: Boolean;
     R: Trect;
{--- finish -------- Eugeny Zvyagintzev ---- 12-04-2002 -----}

 function Found: Boolean;
 begin
  Found := True;
  SearchPos.Y := P^.Row; SearchPos.X := P^.Col;
  GotoCell(GetCellName(P^.Col, P^.Row));
  if WasReplace then
    begin
     if (SearchData.TxtOptions and 4 <> 0) then
      if (SearchData.CellOptions = 0) then
       begin S := P^.S; Delete(S, K+L, Length(S1));
             Insert(ReplaceData.S1, S, K+L);
             Cells^.ReplaceItem(P^.Col, P^.Row, S); DrawView; Exit
       end else
       begin if P^.Options and 3 = coFormula then
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
  S := System.Copy(S, K, 255);
 end;

begin
 if SearchData.S = '' then Exit;
 Val(SearchData.S, V, I);
 if SearchData.TxtOptions and 1 = 0 then S1 := UpStrg(SearchData.S)
                                    else S1 := SearchData.S;

{--- start -------- Eugeny Zvyagintzev ---- 12-04-2002 -----}
 R.Assign(1,1,30,9);
 New(Inf, Init(R));
 Inf^.Top := GetString(dlSearching) + '"' + Cut(S1, 40) + '"';
 Desktop^.Insert(Inf);
 CancelSearch:=False;
 NewTimer(Tmr, 0);
{--- finish -------- Eugeny Zvyagintzev ---- 12-04-2002 -----}

 For I := 1 to Cells^.Count do
  begin
   P := Cells^.At(I - 1);
   if ((P^.Row = SearchPos.Y) and (P^.Col > SearchPos.X)) or
      (P^.Row > SearchPos.Y)  then
    if (SearchData.CellOptions = 0) then
     begin
      if SearchData.TxtOptions and 1 = 0 then S := UpStrg(P^.S)
                                         else S := P^.S;
      K := Pos(S1, S); L := 0;
      if SearchData.TxtOptions and 2 = 0 then
       begin if K > 0 then
        if Found then
         begin
          If Inf <> Nil Then Dispose(Inf,Done);
          Inf:=Nil;
          Exit
         end
       end
       else repeat
              K := Pos(S1, S); L := 0;
              if K = 0 then else
              if (K = 1) or (S[0] = S1[0]) then
               begin
                 if (S[0] = S1[0]) or (S[0] <> S1[0]) and (S[Length(S1) + 1] in Signs)
                  then
                  if Found then
                   begin
                    If Inf <> Nil Then Dispose(Inf,Done);
                    Inf:=Nil;
                    Exit
                   end
                  else SearchSign;
               end else
              if (K = Length(S) - Length(S1) + 1) then
               begin if (S[K - 1] in Signs) then
                if Found then
                 begin
                  If Inf <> Nil Then Dispose(Inf,Done);
                  Inf:=Nil;
                  Exit
                 end
                else SearchSign;
               end else
              if (S[K - 1] in Signs) and (S[Length(S1) + K] in Signs)
               then
                if Found then
                 begin
                  If Inf <> Nil Then Dispose(Inf,Done);
                  Inf:=Nil;
                  Exit
                 end
               else SearchSign;
            until (K = 0) or (S[0] < S1[0]);
     end else
      if (P^.Options and 3 <> 0) and (V = P^.Value) then
       if Found then
        begin
         If Inf <> Nil Then Dispose(Inf,Done);
         Inf:=Nil;
         Exit
        end;
{--- start -------- Eugeny Zvyagintzev ---- 13-04-2002 -----}
     If TimerExpired(Tmr) Then
      Begin
       Inf^.Write(1, StrGrd(Cells^.Count, I, 30, False));
       Inf^.Write(2, ZtoS(Percent(Cells^.Count,I))+'%');
       DispatchEvents(Inf,CancelSearch);
       If CancelSearch Then
        Begin
         If Inf <> Nil Then
          Dispose(Inf,Done);
         Inf:=Nil;
         SearchCanceled := True;
         Exit;
        End;
       NewTimer(Tmr, 1);
      End;
{--- finish -------- Eugeny Zvyagintzev ---- 13-04-2002 -----}
  end;
 If Inf <> Nil Then Dispose(Inf,Done);
 Inf:=Nil;
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
  StrPCopy(Buf,FieldValue); { Можно было использовать Move,
                            { но при работе с длинными строками
                            { так безопаснее}
  Buf[StrLen]:=' '; { Для удаления #0, который вставляет StrPCopy}
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
  FieldOffsetInBuffer:=Offset;
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

end.
