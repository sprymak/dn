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
  advance, advance1, advance2, advance3, Calculat, RStrings, DNApp,
  Dialogs, Views, Objects, Drivers, Commands, HistList, DNHelp;

type
  PCalcLine = ^TCalcLine;
  TCalcLine = object(TInputLine)
    ResultSelected: boolean;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetData(var B); virtual;
    procedure SetValues(SetSelf: boolean);
    procedure Awaken; virtual;
    destructor Done; virtual;
    end;

  PIndicator = ^TIndicator;
  TIndicator = object(TView)
    CalcError: boolean;
    Value: CReal;
    Constructor Init(var R: TRect);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; virtual;
    procedure WrtT(X: CReal; var s: String);
    function GetPalette: PPalette; virtual;
    end;

procedure InsertCalc;

const
  Calc: PView = nil;

implementation

Constructor TIndicator.Init;
  begin
    inherited Init(R);
    EventMask := evBroadcast;
    CalcError := False;
    Value := 0;
  end;

function TIndicator.GetPalette;
  const
    s: String[1] = CCluster;
  begin
    GetPalette := @S;
  end;

procedure GetNValue(l: longInt; A: integer; var s: String);
  begin
    s := '';
    if l = 0 then
      s := '0'
    else
      while (l <> 0) do
        begin
          s := HexStr[(l and ((1 shl A)-1))+1]+s;
          l := l shr A;
        end;
  end;

procedure TIndicator.HandleEvent;
  var
    D: record
      n: word;
      s: String[250];
      end;

  procedure CE;
    begin
      DrawView;
      ClearEvent(Event);
    end;

  begin
    inherited HandleEvent(Event);
    if (Event.What = evBroadcast) then
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
                  Str(Value: 0: 20, D.s);
                  while D.s[Length(D.s)] = '0' do
                    SetLength(D.s, Length(D.s)-1);
                  if D.s[Length(D.s)] = '.' then
                    SetLength(D.s, Length(D.s)-1);
                end;
              1:
                if Abs(Value) < $7FFFFFFF then
                  GetNValue(Trunc(Value), 4, D.s)
                else
                  exit;
              2:
                if Abs(Value) < $7FFFFFFF then
                  GetNValue(Trunc(Value), 1, D.s)
                else
                  exit;
              3:
                if Abs(Value) < $7FFFFFFF then
                  GetNValue(Trunc(Value), 3, D.s)
                else
                  exit;
              4:
                Str(Value, D.s);
              5:
                WrtT(Value, D.s);
            end {case};
            Message(Application, evCommand, cmPutInClipboard, @D.s);
          end;
      end {case};
  end { TIndicator.HandleEvent };

procedure TIndicator.WrtT(X: CReal; var s: String);
  var
    Hour, Min: longInt;
    Sec: CReal;
    Negative: boolean;
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
    Str(Sec: 0: 8, s);
    if PosChar('E', s) = 0 then
      while s[Length(s)] = '0' do
        SetLength(s, Length(s)-1);
    if s[Length(s)] = '.' then
      SetLength(s, Length(s)-1);
    if (Length(s) < 2) or (PosChar('.', s) = 2) then
      s := '0'+s;
    s := sHour+':'+sMin+':'+s;
  end { TIndicator.WrtT };

procedure TIndicator.Draw;
  var
    B: TDrawBuffer;
    s: String[40];
    C: byte;
    l, LL: longInt;

  procedure Wrt(n: integer);
    begin
      MoveChar(B, ' ', C, Size.X);
      MoveStr(B[Size.X-Length(s)-1], s, C);
      WriteLine(0, n, Size.X, 1, B);
    end;

  procedure WrtN(n, A: integer);
    begin
      GetNValue(LL, A, s);
      Wrt(n);
    end;

  begin
    C := GetColor(1);
    if CalcError then
      begin
        MoveChar(B, ' ', C, Size.X);
        WriteLine(0, 0, Size.X, 2, B);
        WriteLine(0, 3, Size.X, 3, B);
        s := GetString(CalcErrMess)+GetErrOp(l);
        MoveStr(B[(Size.X-Length(s)) div 2], s, C);
        WriteLine(0, 2, Size.X, 1, B);
      end
    else
      begin
        Str(Value: 0: 20, s);
        if PosChar('E', s) = 0 then
          while s[Length(s)] = '0' do
            SetLength(s, Length(s)-1);
        if s[Length(s)] = '.' then
          SetLength(s, Length(s)-1);
        Wrt(0);
        if Abs(Value) > $7FFFFFFF then
          begin
            s := GetString(dlOverflow);
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
            WrtT(Value, s);
            Wrt(5);
          end;
        Str(Value, s);
        Wrt(4);
      end;
  end { TIndicator.Draw };

procedure TCalcLine.HandleEvent;
  var
    WasKey: boolean;
  begin
    WasKey := Event.What = evKeyDown;
    if WasKey and ResultSelected then
      if not (Event.CharCode in ['0'..'9']) then
        SelEnd := 0;
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
    s: String;
  begin
    ResultSelected := False;
    GetData(s);
    //  DelSpace(S);
    if s = '' then
      begin
        R := 0;
        Message(Owner, evBroadcast, cmSetValue, @R);
        exit
      end;
    R := Evalue(s, nil);
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
            GetData(s);
            HistoryAdd(hsCalcLine, s);
            Str(R: 0: 20, s);
            while s[Length(s)] = '0' do
              SetLength(s, Length(s)-1);
            if s[Length(s)] = '.' then
              SetLength(s, Length(s)-1);
            SetData(s);
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
      Control^.Options := Control^.Options or ofFramed or
        ofPostProcess;
      Dlg^.Insert(Control);

      R.Assign(Dlg^.Size.X-5, 3, Dlg^.Size.X-2, 4);
      Histry := New(PHistory, Init(R, PInputline(Control),
        hsCalcLine));
      Histry^.Options := Histry^.Options or ofFramed;
      Dlg^.Insert(Histry);

      R.Assign(2, 2, 13, 3);
      Labl := New(PLabel, Init(R, GetString(dlCalcEx_p_ression),
        Control));
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
