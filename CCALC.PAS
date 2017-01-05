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

unit CCalc;

interface

uses
  Advance, Advance1, Advance2, Advance3, Calculat, RStrings, DNApp,
  Dialogs, Views, Defines, Drivers, Commands, HistList, DNHelp
  ;

type
  PCalcLine = ^TCalcLine;
  TCalcLine = object(TInputLine)
    ResultSelected: Boolean;
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
    procedure WrtT(X: CReal; var S: String);
    function GetPalette: PPalette; virtual;
    end;

procedure InsertCalc;

const
  Calc: PView = nil;

implementation

constructor TIndicator.Init;
  begin
  inherited Init(R);
  EventMask := evBroadcast;
  CalcError := False;
  Value := 0;
  end;

function TIndicator.GetPalette;
  const
    S: String[1] = CCluster;
  begin
  GetPalette := @S;
  end;

procedure GetNValue(L: LongInt; A: Integer; var S: String);
  begin
  S := '';
  if L = 0 then
    S := '0'
  else
    while (L <> 0) do
      begin
      S := HexStr[(L and ((1 shl A)-1))+1]+S;
      L := L shr A;
      end;
  end;

procedure TIndicator.HandleEvent;
  var
    D: record
      n: Word;
      S: String[250];
      end;

  procedure CE;
    begin
    DrawView;
    ClearEvent(Event);
    end;

  begin
  inherited HandleEvent(Event);
  if  (Event.What = evBroadcast) then
    case Event.Command of
      cmSetValue:
        begin
        CalcError := False;
        Value := PCReal(Event.InfoPtr)^;
        CE
        end;
      cmSetError:
        begin
        CalcError := True;
        CE
        end;
      cmCancel:
        begin
        Event.What := evCommand;
        Event.Command := cmClose;
        Event.InfoPtr := nil;
        PutEvent(Event);
        CE
        end;
      cmCopyClip:
        begin
        ClearEvent(Event);
        Owner^.GetData(D);
        case D.n of
          0:
            begin
            Str(Value: 0: 20, D.S);
            while D.S[Length(D.S)] = '0' do
              SetLength(D.S, Length(D.S)-1);
            if D.S[Length(D.S)] = '.' then
              SetLength(D.S, Length(D.S)-1);
            end;
          1:
            if Abs(Value) < $7FFFFFFF then
              GetNValue(Trunc(Value), 4, D.S)
            else
              Exit;
          2:
            if Abs(Value) < $7FFFFFFF then
              GetNValue(Trunc(Value), 1, D.S)
            else
              Exit;
          3:
            if Abs(Value) < $7FFFFFFF then
              GetNValue(Trunc(Value), 3, D.S)
            else
              Exit;
          4:
            Str(Value, D.S);
          5:
            WrtT(Value, D.S);
        end {case};
        Message(Application, evCommand, cmPutInClipboard, @D.S);
        end;
    end {case};
  end { TIndicator.HandleEvent };

procedure TIndicator.WrtT(X: CReal; var S: String);
  var
    Hour, Min: LongInt;
    Sec: CReal;
    Negative: Boolean;
    sHour, sMin: String[20];
  begin
  if X < 0 then
    begin
    Negative := True;
    X := -X;
    end
  else
    Negative := False;
  Hour := Trunc(X/3600);
  Min := Trunc((X-Hour*3600)/60);
  Sec := X-Hour*3600-Min*60;
  Str(Hour, sHour);
  if Negative then
    sHour := '-'+sHour;
  Str(Min, sMin);
  if Length(sMin) < 2 then
    sMin := '0'+sMin;
  Str(Sec: 0: 8, S);
  if PosChar('E', S) = 0 then
    while S[Length(S)] = '0' do
      SetLength(S, Length(S)-1);
  if S[Length(S)] = '.' then
    SetLength(S, Length(S)-1);
  if  (Length(S) < 2) or (PosChar('.', S) = 2) then
    S := '0'+S;
  S := sHour+':'+sMin+':'+S;
  end { TIndicator.WrtT };

procedure TIndicator.Draw;
  var
    B: TDrawBuffer;
    S: String[40];
    C: Byte;
    L, LL: LongInt;

  procedure Wrt(N: Integer);
    begin
    MoveChar(B, ' ', C, Size.X);
    MoveStr(B[Size.X-Length(S)-1], S, C);
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
    WriteLine(0, 0, Size.X, 2, B);
    WriteLine(0, 3, Size.X, 3, B);
    S := GetString(CalcErrMess)+GetErrOp(L);
    MoveStr(B[(Size.X-Length(S)) div 2], S, C);
    WriteLine(0, 2, Size.X, 1, B);
    end
  else
    begin
    Str(Value: 0: 20, S);
    if PosChar('E', S) = 0 then
      while S[Length(S)] = '0' do
        SetLength(S, Length(S)-1);
    if S[Length(S)] = '.' then
      SetLength(S, Length(S)-1);
    Wrt(0);
    if Abs(Value) > $7FFFFFFF then
      begin
      S := GetString(dlOverflow);
      Wrt(1);
      Wrt(2);
      Wrt(3);
      Wrt(5);
      end
    else
      begin
      LL := Trunc(Value);
      WrtN(1, 4);
      WrtN(2, 1);
      WrtN(3, 3);
      WrtT(Value, S);
      Wrt(5);
      end;
    Str(Value, S);
    Wrt(4);
    end;
  end { TIndicator.Draw };

procedure TCalcLine.HandleEvent;
  var
    WasKey: Boolean;
  begin
  WasKey := Event.What = evKeyDown;
  if WasKey and ResultSelected then
    if not (Event.CharCode in ['0'..'9']) then
      SelEnd := 0;
  if  (Event.What = evKeyDown) and (Event.KeyCode = kbUp) then
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
    end
  else if (Event.What = evCommand) then
    case Event.Command of
      cmCalcValue:
        SetValues(True);
      cmGetName:
        PString(Event.InfoPtr)^:= GetString(dlCalculator);
    end {case};
  end { TCalcLine.HandleEvent };

procedure TCalcLine.SetData;
  begin
  inherited SetData(B);
  SetValues(False);
  end;

procedure TCalcLine.Awaken;
  begin
  inherited Awaken;
  SetValues(False);
  Calc := Owner;
  end;

destructor TCalcLine.Done;
  begin
  Calc := nil;
  inherited Done;
  end;

procedure TCalcLine.SetValues;
  var
    R: CReal;
    S: String;
  begin
  ResultSelected := False;
  GetData(S);
  //  DelSpace(S);
  if S = '' then
    begin
    R := 0;
    Message(Owner, evBroadcast, cmSetValue, @R);
    Exit
    end;
  R := Evalue(S, nil);
  if EvalueError then
    begin
    if not SetSelf then
      CalcErrMess := dlMsgError
    else
      begin
      CurPos := CalcErrPos;
      DrawView;
      end;
    Message(Owner, evBroadcast, cmSetError, nil);
    EvalueError := False;
    end
  else
    begin
    Message(Owner, evBroadcast, cmSetValue, @R);
    if SetSelf then
      begin
      GetData(S);
      HistoryAdd(hsCalcLine, S);
      Str(R: 0: 20, S);
      while S[Length(S)] = '0' do
        SetLength(S, Length(S)-1);
      if S[Length(S)] = '.' then
        SetLength(S, Length(S)-1);
      SetData(S);
      SelStart := 0;
      SelEnd := Length(Data^);
      CurPos := SelEnd;
      DrawView;
      ResultSelected := True;
      end;
    end;
  end { TCalcLine.SetValues };

procedure InsertCalc;

  function MakeDialog: PDialog;
    var
      Dlg: PDialog;
      R: TRect;
      Control, Labl, Histry: PView;
    begin
    Dlg := PDialog(LoadResource(dlgCalculator));
    R.Move(10, 5);
    Dlg^.Number := GetNum;

    R.Assign(2, 3, Dlg^.Size.X-6, 4);
    Control := New(PCalcLine, Init(R, 250));
    Control^.Options := Control^.Options or ofFramed or ofPostProcess;
    Dlg^.Insert(Control);

    R.Assign(Dlg^.Size.X-5, 3, Dlg^.Size.X-2, 4);
    Histry := New(PHistory, Init(R, PInputline(Control), hsCalcLine));
    Histry^.Options := Histry^.Options or ofFramed;
    Dlg^.Insert(Histry);

    R.Assign(2, 2, 13, 3);
    Labl := New(PLabel, Init(R, GetString(dlCalcEx_p_ression), Control));
    Dlg^.Insert(Labl);

    R.Assign(12, 6, Dlg^.Size.X-2, 12);
    Control := New(PIndicator, Init(R));
    Control^.Options := Control^.Options or ofFramed;
    Dlg^.Insert(Control);

    MakeDialog := Dlg;
    end { MakeDialog: };

  begin { InsertCalc }
  if Calc = nil then
    begin
    Calc := MakeDialog;
    Application^.InsertWindow(PWindow(Calc));
    end
  else
    Calc^.Select;
  end { InsertCalc };

end.
