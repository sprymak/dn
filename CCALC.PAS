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
//  dn16rc1-hex_full_range.patch
//  dn16rc1-vp_noasm_compatible.patch
//
//  2.0.0
//  dn269-calculator_bugs_fix.patch
//  dn2628-calculator_bugs_fix.patch
//  dn2628-calculator_error_messages_improve.patch
//
//  2.7.0
//  dn2825-calculator_improve.patch
//  dn21202-calculator(f)-bugs_fix.patch
//  dn21225-calculator(if)-multilanguage.patch
//  dn328-InputLine(fi)-new_shortkey_added_and_some_bugs_fixed.patch
//  dn368-calculator_clipboard_fix.patch
//
//  3.7.0
//  dn370-calculator(i)_optimization.patch
//  dn31005-bp_to_vp_on_off_true_false.patch
//  dn31220-Kernel(i)-DN_changes_language_immediately.patch
//
//  4.9.0
//  dn50202-calc(f)-save_desktop_fix.patch
//  dn50208-cleanup.patch
//
//  5.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
{(c) Sergey Biryukov aka Flash, 2002}
UNIT CCalc;

interface

uses Advance, Advance1, Calculat, DNApp, Dialogs, Views, Objects, Drivers,
     Commands, HistList, DNHelp;

type
     PCalcLine = ^TCalcLine;
     TCalcLine = object(TInputLine)
        Radio: PRadioButtons; { Flash 02-02-2003 }
        procedure HandleEvent(var Event: TEvent); virtual;
        procedure SetData(var B); virtual;
        procedure SetValues(SetSelf: Boolean);
        procedure Awaken; virtual;
        destructor Done; virtual;
     end;

     PIndicator = ^TIndicator;
     TIndicator = object(TView)
       CalcError: Boolean;
       Value: CReal;
       constructor Init(var R: TRect);
       procedure HandleEvent(var Event: TEvent); virtual;
       procedure Draw; virtual;
       function GetPalette: PPalette; virtual;
     end;

     PCCalcWindow = ^TCCalcWindow;
     TCCalcWindow = object(TDialog)
       CalcLine: PCalcLine;
       Label1, Label2: PLabel;
       Radio: PRadioButtons;
       btnCalc, btnCopy, btnClose, btnHelp: PButton;
       Histry: PHistory;
       Indicator: PIndicator;
       constructor Init(Modal: Boolean);
       constructor Load(var S: TStream);
       destructor Done; virtual;
       procedure InsertControls;
       procedure DeleteControls;
       procedure CenterButtons;
       procedure Store(var S: TStream);
       procedure ChangeLanguage; virtual;  {John_SW  23-12-2003}
     end;

function InsertCalc(s: PString): String;
function CTrunc(Value:Double): LongInt; { Max Morozov }
procedure GetNValue(L: LongInt; A: Integer; var S: String);

const Calc: PView = nil;
      ModalCalc: PCCalcWindow = Nil;

implementation

uses Messages;

const Limit = 4294967295.0;

{ Max Morozov: begin } { trunc without overflow exception }
function CTrunc(Value:Double): LongInt; assembler;
type
  TWordStruct = record
     Byte0: Byte;
     Byte1: Byte;
  end;
  TQWordStruct = record
     Word0: AWord; { Kirill }{ for compatible Virtual Pascal Word -> AWord }
     Word1: AWord;
     Word2: AWord;
     Word3: AWord;
  end;
var
 temp1:TWordStruct;
 temp2:TQWordStruct;
asm
        fld     Value
        fstcw   temp1                   { save the control word }
        fwait
        mov     al, temp1.Byte1
        or      temp1.Byte1, 0Ch
        fldcw   temp1
        fistp   qword ptr temp2         { convert to 64-bit integer }
        mov     temp1.Byte1, al
        fldcw   temp1                   { restore the control word }
        mov     ax, temp2.Word0         { return LS 32 bits }
        mov     dx, temp2.Word1
end;
{ Max Morozov: end }

constructor TIndicator.Init;
begin
  inherited Init(R);
  EventMask := evBroadcast;
  CalcError := False;
  Value := 0;
end;

function TIndicator.GetPalette;
  const S: String[1] = CCluster;
begin
  GetPalette := @S;
end;

  procedure GetNValue(L: LongInt; A: Integer; var S: String);
  begin
    S := '';
    if L = 0 then S := '0' else
    while (L <> 0) do
     begin
       S := HexStr[(L and ((1 shl A) - 1)) + 1] + S;
       L := L shr A;
     end;
  end;

{ Flash 26-08-2003 >>> }
procedure GetForm(N: byte; V: CReal; var S: string);
begin
case N of
 0: begin
    if Abs(V)>=1E+14 then Str(V, S) else Str(V:0:15, S);
    if Pos('E',S)=0 then While S[Length(S)] = '0' do Dec(S[0]);
    if S[Length(S)] = '.' then Dec(S[0]);
    end;
 1: if Abs(V) < Limit then GetNValue(CTrunc(V), 4, S) else Exit;
 2: if Abs(V) < Limit then GetNValue(CTrunc(V), 1, S) else Exit;
 3: if Abs(V) < Limit then GetNValue(CTrunc(V), 3, S) else Exit;
 4: Str(V, S);
end;
DelSpace(S);
end;
{ Flash 26-08-2003 <<< }

procedure TIndicator.HandleEvent;
 var D: record
         N: Word;
         S: String[250];
        end;

  procedure CE; begin DrawView; ClearEvent(Event); end;

begin
  inherited HandleEvent(Event);
  if (Event.What = evBroadcast) then
    case Event.Command of
       cmSetValue: begin
                     CalcError := False;
                     Value := PCReal(Event.InfoPtr)^;
                     CE
                   end;
       cmSetError: begin
                     CalcError := True;
                     CE
                   end;
       cmCancel: begin
                   Event.What := evCommand;
                   Event.Command := cmClose;
                   Event.InfoPtr := nil;
                   PutEvent(Event);
                   CE
                 end;
       cmCopyClip: begin
                  ClearEvent(Event);
                  Owner^.GetData(D);
                  GetForm(D.N, Value, D.S);
                  Message(Application, evCommand, cmPutInClipboard, @D.S);
               end;
    end;
end;

procedure TIndicator.Draw;
  var
      LL: LongInt;
      S: String[40];
      C: Byte;
      B: TDrawBuffer;

  procedure Wrt(N: Integer);
  begin
     MoveChar(B, ' ', C, Size.X);
     MoveStr(B[Size.X - Length(S) - 1], S, C);
     WriteLine(0, N, Size.X, 1, B);
  end;

  procedure WrtN(N, A: Integer);
  begin
    GetNValue(LL, A, S);
    Wrt(N);
  end;

begin
   C := GetColor(1);
   if CalcError then
     begin
        MoveChar(B, ' ', C, Size.X);
        WriteLine(0,0,Size.X,2,B);
        WriteLine(0,3,Size.X,2,B);

        { Flash >>> }
        case ErrCode of
        Stack_Overflow          : S:=GetString(clStack_Overflow);
        Too_Large_Number        : S:=GetString(clToo_Large_Number);
        Argument_Missing        : S:=GetString(clArgument_Missing)+ErrString;
        Too_Many_Arguments      : S:=GetString(clToo_Many_Arguments)+ErrString;
        Syntax_Error            : S:=GetString(clSyntax_Error);
        Unknown_Function        : S:=GetString(clUnknown_Function);
        Left_Bracket_Missing    : S:=GetString(clLeft_Bracket_Missing);
        Right_Bracket_Missing   : S:=GetString(clRight_Bracket_Missing);
        Operation_Expected      : S:=GetString(clOperation_Expected);
        Unknown_Symbol          : S:=GetString(clUnknown_Symbol);
        Two_Operations          : S:=GetString(clTwo_Operations);
        Incorrect_Usage         : S:=GetString(clIncorrect_Usage)+ErrString;
        Division_By_Zero        : S:=GetString(clDivision_By_Zero);
        LOG_Wrong_Base          : S:=GetString(clLOG_Wrong_Base);
        Power_Wrong_Index       : S:=GetString(clPower_Wrong_Index);
        Root_Wrong_Index        : S:=GetString(clRoot_Wrong_Index);
        Wrong_Argument          : S:=GetString(clWrong_Argument)+ErrString;
        Power_Wrong_Argument    : S:=GetString(clPower_Wrong_Argument);
        Incorrect_Form          : S:=GetString(clIncorrect_Form)+ErrString;
        else S:=GetString(dlMsgError)
        end;
        { Flash <<< }

        MoveStr(B[(Size.X - Byte(S[0])) div 2], S, C);
        WriteLine(0,2,Size.X,1,B);
     end else
     begin
       GetForm(0, Value, S);
       Wrt(0);
       if Abs(Value) > Limit then
         begin S := GetString(dlOverflow); Wrt(1); Wrt(2); Wrt(3); end
       else
         begin LL := CTrunc(Value); WrtN(1, 4); WrtN(2, 1); WrtN(3, 3); end;
       Str(Value, S);
       Wrt(4);
     end;
end;


procedure TCalcLine.HandleEvent;
 var WasKey: Boolean;
begin
   WasKey := Event.What = evKeyDown;
   if (Event.What = evKeyDown) and (Event.KeyCode = kbUp) then
     ClearEvent(Event);
   inherited HandleEvent(Event);
   if WasKey then
    begin
     SetValues(False);
     if Event.KeyCode = kbESC then
      begin
        GetData(FreeStr);
        HistoryAdd(hsCalcLine, FreeStr);
        Event.What := evCommand;
        Event.Command := cmClose;
        Event.InfoPtr := nil;
        PutEvent(Event);
        ClearEvent(Event);
      end;
    end else
    if (Event.What = evCommand) then
      case Event.Command of
        cmCalcValue: Begin
                      SetValues(True);
                      SelectAll(True);
                      GetData(FreeStr);
                      CurPos:=Length(FreeStr);
                      if ErrPos>0 then
                       begin
                       GetData(FreeStr);
                       Inc(ErrPos,CharCount(' ',Copy(FreeStr,1,ErrPos-1)));
                       CurPos:=ErrPos-1; SelStart:=0; SelEnd:=0
                       end;
                      DrawView;
                      If Owner^.GetState(sfModal) Then EndModal(cmOk); {John_SW 09-02-2003}
                     End;
        cmGetName: PString(Event.InfoPtr)^ := GetString(dlCalculator);
      end;
end;

procedure TCalcLine.SetData;
begin
  inherited SetData(B);
  SetValues(False);
end;

procedure TCalcLine.Awaken;
begin
  inherited Awaken;
  SetValues(False);
  If ModalCalc = Nil Then Calc := Owner Else PView(ModalCalc) := Owner;
end;

destructor TCalcLine.Done;
begin
  If ModalCalc = Nil Then Calc := nil;
  inherited Done;
end;

procedure TCalcLine.SetValues;
 var
     N: Word;
     R: CReal;
     S: String;
begin
  GetData(S);
  if S = '' then
   begin R := 0; Message(Owner, evBroadcast, cmSetValue, @R); Exit end;
  R := Evalue(S, nil);
  if EvalueError then Message(Owner, evBroadcast, cmSetError, nil)
  else begin
   Message(Owner, evBroadcast, cmSetValue, @R);
   if SetSelf then begin
    GetData(S);
    { Flash 02-02-2003 >>> }
    Radio^.GetData(N);
    GetForm(N, R, S);
    case N of
     1: S:='$'+S;
     2: S:=S+'b';
     3: S:=S+'o';
    end;
    HistoryAdd(hsCalcLine, S);
    { Flash 02-02-2003 <<< }
    SetData(S);
    DrawView;
   end;
  end;
end;

procedure TCCalcWindow.InsertControls;
 var
     N: Word;
     R: TRect;
     T: String;
begin
  { Input line }
  R.Assign(2,3,46,4);
  CalcLine:= New(PCalcLine, Init(R, 250));
  CalcLine^.Options := CalcLine^.Options or ofFramed or ofPostProcess;
  Insert(CalcLine);

  { Radio buttons }
  R.Assign(2,6,11,11);
  Radio := New(PRadioButtons, Init(R,
  NewSItem('~D~EC',
  NewSItem('~H~EX',
  NewSItem('~B~IN',
  NewSItem('~O~CT',
  NewSItem('~E~XP',Nil)))))));
  Insert(Radio); CalcLine^.Radio := Radio;

  { Buttons }
  T:=GetString(dlEvaluateButton);
  R.Assign(1,12,Length(T)+3,14);
  btnCalc :=New(PButton, Init(R, T, cmCalcValue, bfDefault));
  N:=1+btnCalc^.Size.X;
  T:=GetString(dlCalcCopyButton);
  R.Assign(N,12,N+Length(T)+2,14);
  btnCopy :=New(PButton, Init(R, T, cmCopyClip, bfBroadcast));
  N:=N+btnCopy^.Size.X;
  T:=GetString(dlCloseButton);
  R.Assign(N,12,N+Length(T)+2,14);
  btnClose:=New(PButton, Init(R, T, cmCancel, bfBroadcast));
  N:=N+btnClose^.Size.X;
  T:=GetString(dlHelpButton);
  R.Assign(N,12,N+Length(T)+2,14);
  btnHelp :=New(PButton, Init(R, T, cmHelp, bfNormal));
  CenterButtons;
  Insert(btnCalc);
  Insert(btnCopy);
  Insert(btnClose);
  Insert(btnHelp);

  { Label "Expression" }
  T:=GetString(dlCalcEx_p_ression);
  R.Assign(2,2,Length(T)+2,3);
  Label1 := New(PLabel, Init(R, T, CalcLine));
  Insert(Label1);

  { Label "Copy as" }
  T:=GetString(dlCalcCopy_a_s);
  R.Assign(2,5,Length(T)+2,6);
  Label2 := New(PLabel, Init(R, T, Radio));
  Insert(Label2);

  { History }
  R.Assign(47,3,50,4);
  Histry := New(PHistory, Init(R, PInputline(CalcLine), hsCalcLine));
  Histry^.Options := Histry^.Options or ofFramed;
  Insert(Histry);

  { Evaluation indicator }
  R.Assign(12,6,50,11);
  Indicator:= New(PIndicator, Init(R));
  Indicator^.Options := Indicator^.Options or ofFramed;
  Insert(Indicator);

  CalcLine^.MakeFirst;
  Title:=NewStr(GetString(dlCalculator));
end;

procedure TCCalcWindow.DeleteControls;
Begin
 Delete(CalcLine);
 Delete(Label1);
 Delete(Label2);
 Delete(Radio);
 Delete(btnCalc);
 Delete(btnCopy);
 Delete(btnClose);
 Delete(btnHelp);
 Delete(Histry);
 Delete(Indicator);
 DisposeStr(Title);
End;

procedure TCCalcWindow.CenterButtons;
 var N: Word;
begin
  N:=btnCalc^.Size.X+btnCopy^.Size.X+btnClose^.Size.X+btnHelp^.Size.X;
  N:=(Size.X-N) div 2;   btnCalc^.MoveTo(N, 12);
  N:=N+btnCalc^.Size.X;  btnCopy^.MoveTo(N, 12);
  N:=N+btnCopy^.Size.X;  btnClose^.MoveTo(N, 12);
  N:=N+btnClose^.Size.X; btnHelp^.MoveTo(N, 12);
end;

constructor TCCalcWindow.Init;
 var R: TRect;
begin
  { Main window }
  R.Assign(0,0,52,15);
  inherited Init(R, GetString(dlCalculator));
  HelpCtx := hcCalculator;
  if Modal and (Calc <> nil) then
   begin
   Origin.X := (Desktop^.Size.X - Size.X) div 2 + 1;
   Origin.Y := (Desktop^.Size.Y - Size.Y) div 2 + 1;
   end
  else Options := Options or ofCentered;
  if not Modal then Number := GetNum;
  InsertControls;
end;

destructor TCCalcWindow.Done;
begin
  If ModalCalc = Nil Then Calc := nil;
  inherited Done;
end;

constructor TCCalcWindow.Load(var S: TStream);
 var
     N: Word;
     C: Byte;
     R: TRect;
     T: String;
begin
  inherited Load(S);
  T:=CnvString(S.ReadStr);
  S.Read(N, SizeOf(N));
  CalcLine^.SetData(T);
  Radio^.SetData(N);
  Calc:=@Self;
end;

procedure TCCalcWindow.Store(var S: TStream);
Var
    N: Word;
    T: String;
begin
  CalcLine^.GetData(T);
  Radio^.GetData(N);
  inherited Store(S);
  S.WriteStr(@T);
  S.Write(N, SizeOf(N));
end;

{--- start -------- Eugeny Zvyagintzev ---- 23-12-2003 ----}
procedure TCCalcWindow.ChangeLanguage;
begin
 DeleteControls;
 InsertControls;
end;
{--- finish -------- Eugeny Zvyagintzev ---- 23-12-2003 ----}

{--- start -------- Eugeny Zvyagintzev ---- 09-02-2003 ----}
{Now calculator can be called from InputLine as Modal window}
Function InsertCalc(S: PString): String;
begin
  If (S <> Nil) Then
   Begin
    If (ModalCalc = Nil) Then
     Begin
      Evalue(S^, nil);
      ModalCalc:=New(PCCalcWindow, Init(True));
      If Not EvalueError Then ModalCalc^.CalcLine^.SetData(S^);
      If Desktop^.ExecView(ModalCalc) = cmOk Then
       Begin
        ModalCalc^.CalcLine^.GetData(FreeStr);
        InsertCalc:=FreeStr;
       End
      Else
       InsertCalc:='';
      Dispose(ModalCalc,Done);
      ModalCalc:=Nil;
     End
   End
  Else
   If Calc = Nil Then
    Begin
     Calc := New(PCCalcWindow, Init(False));
     Desktop^.Insert(PWindow(Calc))
    End
   Else
    Calc^.Select;
end;
{--- finish -------- Eugeny Zvyagintzev ---- 09-02-2003 ----}

END.
