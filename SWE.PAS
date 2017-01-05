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

unit SWE;

interface

uses
  Drivers, Objects, Views, Dialogs;

type
  TDate = record
    Year: word;
    Month, Day: byte;
    end;

const
  maxByte {: Byte} = $FF;
  minByte {: Byte} = $00;
  maxWord {: Word} = $FFFF;
  minWord {: Word} = $0000;
  minInteger {: Integer} = -1*MaxInt;
  minLongInt {: LongInt} = -1*MaxLongInt;
  maxReal = 1.7e38;
  minReal = -1*maxReal;
  maxDate: TDate = (Year: 9999;
  Month: 12;
  Day: 31
  );
  minDate: TDate = (Year: 1000;
  Month: 1;
  Day: 1
  );

procedure AssignDate(Year: word; Month, Day: byte; var D: TDate);
procedure CopyDate(s: TDate; var D: TDate);
function MoreThat(D1, d2: TDate): boolean;
function LessThat(D1, d2: TDate): boolean;
procedure SetInLimits(var D: TDate; Min, Max: TDate);
function DateInStr(D: TDate): String;

type
  PInpLine = ^TInpLine;
  TInpLine = object(TInputLine)
    function GetPalette: PPalette; virtual;
    end;

  PInputByte = ^TInputByte;
  TInputByte = object(TInpLine)
    MinValue, MaxValue, Value: byte;
    EndView: word;
    Constructor Init(var Bounds: TRect; AMinValue, AMaxValue: byte);
    function Execute: word; virtual;
    function DataSize: word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PInputWord = ^TInputWord;
  TInputWord = object(TInpLine)
    MinValue, MaxValue, Value: word;
    EndView: word;
    Constructor Init(var Bounds: TRect; AMinValue, AMaxValue: word);
    function Execute: word; virtual;
    function DataSize: word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PInputInteger = ^TInputInteger;
  TInputInteger = object(TInpLine)
    MinValue, MaxValue, Value: integer;
    EndView: word;
    Constructor Init(var Bounds: TRect; AMinValue, AMaxValue:
      integer);
    function Execute: word; virtual;
    function DataSize: word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PInputLongInt = ^TInputLongInt;
  TInputLongInt = object(TInpLine)
    MinValue, MaxValue, Value: longInt;
    EndView: word;
    Constructor Init(var Bounds: TRect; AMinValue, AMaxValue:
      longInt);
    function Execute: word; virtual;
    function DataSize: word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PInputDate = ^TInputDate;
  TInputDate = object(TInpLine)
    MinValue, MaxValue, Value: TDate;
    EndView: word;
    Constructor Init(var Bounds: TRect; AMinValue, AMaxValue: TDate);
    function Execute: word; virtual;
    function DataSize: word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PInputReal = ^TInputReal;
  TInputReal = object(TInpLine)
    MinValue, MaxValue, Value: real;
    Decimals: byte;
    EndView: word;
    Constructor Init(var Bounds: TRect; AMinValue, AMaxValue: real; D:
      byte);
    function Execute: word; virtual;
    function DataSize: word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PInputString = ^TInputString;
  TInputString = object(TInpLine)
    EndView: word;
    function Execute: word; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PInputFName = ^TInputFName;
  TInputFName = object(TInputString)
    LC: Char;
    rc: Char;
    C1, C2, C3: word;
    CR, CL: word;
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PInputSFName = ^TInputSFName;
  TInputSFName = object(TInputString)
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PColorPoint = ^TColorPoint;
  TColorPoint = object(TView)
    Color: byte;
    Constructor Init(var ABounds: TRect; AColor: byte);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream); virtual;
    procedure Draw; virtual;
    end;

implementation

uses
  advance, advance1, advance2, advance3, Commands, DNApp;

function TInpLine.GetPalette;
  const
    s: array[0..1] of String[Length(CInputLine)] =
    (#9#9#10#8, #16#16#16#13);
  begin
    GetPalette := @S[DrawShift];
  end;

procedure AssignDate(Year: word; Month, Day: byte; var D: TDate);
  begin
    D.Year := Year;
    D.Month := Month;
    D.Day := Day;
  end;

procedure CopyDate(s: TDate; var D: TDate);
  begin
    D.Year := s.Year;
    D.Month := s.Month;
    D.Day := s.Day;
  end;

function MoreThat(D1, d2: TDate): boolean;
  begin
    MoreThat := False;
    if (D1.Year > d2.Year) or
      ((D1.Year = d2.Year) and ((D1.Month > d2.Month) or
      ((D1.Month = d2.Month) and (D1.Day > d2.Day))))
    then
      MoreThat := True;
  end;

function LessThat(D1, d2: TDate): boolean;
  begin
    LessThat := False;
    if (D1.Year < d2.Year) or
      ((D1.Year = d2.Year) and ((D1.Month < d2.Month) or
      ((D1.Month = d2.Month) and (D1.Day < d2.Day))))
    then
      LessThat := True;
  end;

procedure SetInLimits(var D: TDate; Min, Max: TDate);
  begin
    if MoreThat(D, Max) or LessThat(D, Min) then
      CopyDate(Min, D);
  end;

function DateInStr(D: TDate): String;
  var
    A, B, C: integer;
    s, s1, s2: String[10];
  begin
    Str(D.Day: 2, s);
    Str(D.Month: 2, s1);
    Str(D.Year: 4, s2);
    s := s+'.'+s1+'.'+s2;
    for A := 1 to 10 do
      if s[A] = ' ' then
        s[A] := '0';
    DateInStr := s;
  end;

{---------------------------------------------------------------------------}
{------------------------------  TInputByte  -------------------------------}
{---------------------------------------------------------------------------}

Constructor TInputByte.Init;
  begin
    Value := 0;
    MaxLen := Trunc(ln(AMaxValue)/ln(10))+1;
    TInputLine.Init(Bounds, MaxLen);
    MaxValue := AMaxValue;
    MinValue := AMinValue;
    EndView := 0;
  end;

function TInputByte.Execute: word;
  var
    Event: TEvent;
  begin
    repeat
      Owner^.GetEvent(Event);
      if Event.What <> evNothing then
        HandleEvent(Event);
    until EndView <> 0;

    ClearEvent(Event);

    case EndView of
      kbEnter:
        Execute := cmOK;
      kbESC:
        Execute := cmCancel;
      else
        Execute := EndView;
    end {case};
  end { TInputByte.Execute: };

function TInputByte.DataSize;
  begin
    DataSize := SizeOf(Value);
  end;

procedure TInputByte.GetData;
  begin
    if (Value > MaxValue) or (Value < MinValue) then
      Value := MinValue;
    Move(Value, Rec, DataSize);
  end;

procedure TInputByte.SetData;
  begin
    Move(Rec, Value, DataSize);
    if (Value > MaxValue) or (Value < MinValue) then
      Value := MinValue;
    Str(Value, PString(Data)^);
    DrawView;
  end;

procedure TInputByte.HandleEvent;
  var
    C: integer;
  begin
    case Event.What of
      evKeyDown:
        begin
          if
            (Event.KeyCode = kbEnter) or
            (Event.KeyCode = kbESC) or

            (Event.KeyCode = kbUp) or (Event.KeyCode = kbDown) or
            ((Event.KeyCode = kbLeft) and (CurPos = 0)) or
            ((Event.KeyCode = kbRight) and (CurPos = Length(PString(
              Data)^)))

          then
            EndView := Event.KeyCode;

          if (Event.CharCode in [#8, ^Y, #0, '0', '1'..'9'])
          then
            begin
              TInputLine.HandleEvent(Event);
            end;
        end;
      evMouseDown:
        TInputLine.HandleEvent(Event);
    end {case};
    Val(PString(Data)^, Value, C);
    if C <> 0 then
      Value := MaxValue;
  end { TInputByte.HandleEvent };

{---------------------------------------------------------------------------}
{-----------------------------  TInputWord  --------------------------------}
{---------------------------------------------------------------------------}

Constructor TInputWord.Init;
  begin
    Value := 0;
    MaxLen := Trunc(ln(AMaxValue)/ln(10))+1;
    TInputLine.Init(Bounds, MaxLen);
    MaxValue := AMaxValue;
    MinValue := AMinValue;
    EndView := 0;
  end;

function TInputWord.Execute: word;
  var
    Event: TEvent;
  begin
    repeat
      Owner^.GetEvent(Event);
      HandleEvent(Event);
    until EndView <> 0;

    ClearEvent(Event);

    case EndView of
      kbEnter:
        Execute := cmOK;
      kbESC:
        Execute := cmCancel;
      else
        Execute := EndView;
    end {case};
  end;

function TInputWord.DataSize;
  begin
    DataSize := SizeOf(Value);
  end;

procedure TInputWord.GetData;
  begin
    if (Value > MaxValue) or (Value < MinValue) then
      Value := MinValue;
    Move(Value, Rec, DataSize);
  end;

procedure TInputWord.SetData;
  begin
    Move(Rec, Value, DataSize);
    if (Value > MaxValue) or (Value < MinValue) then
      Value := MinValue;
    Str(Value, PString(Data)^);
    DrawView;
  end;

procedure TInputWord.HandleEvent;
  var
    C: integer;
  begin
    SelStart := SelEnd;
    case Event.What of
      evKeyDown:
        begin
          if
            (Event.KeyCode = kbEnter) or
            (Event.KeyCode = kbESC) or

            (Event.KeyCode = kbUp) or (Event.KeyCode = kbDown) or
            ((Event.KeyCode = kbLeft) and (CurPos = 0)) or
            ((Event.KeyCode = kbRight) and (CurPos = Length(PString(
              Data)^)))

          then
            EndView := Event.KeyCode;

          if (Event.CharCode in [#8, ^Y, #0, '0', '1'..'9'])
          then
            begin
              if (Length(Data^) = MaxLen) and (CurPos < MaxLen) then
                  SetLength(Data^, Length(Data^)-1);
              TInputLine.HandleEvent(Event);
            end;
        end;
      evMouseDown:
        TInputLine.HandleEvent(Event);
    end {case};
    Val(PString(Data)^, Value, C);
    if C <> 0 then
      Value := MaxValue;
  end { TInputWord.HandleEvent };

{---------------------------------------------------------------------------}
{----------------------------  TInputInteger  ------------------------------}
{---------------------------------------------------------------------------}

Constructor TInputInteger.Init;
  begin
    Value := 0;
    MaxLen := Trunc(ln(Abs(AMaxValue))/ln(10))+1+byte(MinValue < 0)
      {-};
    TInputLine.Init(Bounds, MaxLen);
    MaxValue := AMaxValue;
    MinValue := AMinValue;
    EndView := 0;
  end;

function TInputInteger.Execute: word;
  var
    Event: TEvent;
  begin
    repeat
      Owner^.GetEvent(Event);
      HandleEvent(Event);
    until EndView <> 0;

    ClearEvent(Event);

    case EndView of
      kbEnter:
        Execute := cmOK;
      kbESC:
        Execute := cmCancel;
      else
        Execute := EndView;
    end {case};
  end;

function TInputInteger.DataSize;
  begin
    DataSize := SizeOf(Value);
  end;

procedure TInputInteger.GetData;
  begin
    if (Value > MaxValue) or (Value < MinValue) then
      Value := MinValue;
    Move(Value, Rec, DataSize);
  end;

procedure TInputInteger.SetData;
  begin
    Move(Rec, Value, DataSize);
    if (Value > MaxValue) or (Value < MinValue) then
      Value := MinValue;
    Str(Value, PString(Data)^);
    DrawView;
  end;

procedure TInputInteger.HandleEvent;
  var
    C: integer;
  begin
    case Event.What of
      evKeyDown:
        begin
          if
            (Event.KeyCode = kbEnter) or
            (Event.KeyCode = kbESC) or

            (Event.KeyCode = kbUp) or (Event.KeyCode = kbDown) or
            ((Event.KeyCode = kbLeft) and (CurPos = 0)) or
            ((Event.KeyCode = kbRight) and (CurPos = Length(PString(
              Data)^)))

          then
            EndView := Event.KeyCode;
          if (Event.CharCode = '-') and (MinValue >= 0) then
            Event.CharCode := 'A';
          if ((Event.CharCode in [#8, ^Y, #0, '0'..'9']) or
            ((CurPos = 0) and (Event.CharCode = '-'))) and
            not ((CurPos = 0) and (Data^[1] = '-') and
            (Event.CharCode in ['0'..'9', '-']))
          then
            TInputLine.HandleEvent(Event);
        end;
      evMouseDown:
        TInputLine.HandleEvent(Event);
    end {case};
    Val(PString(Data)^, Value, C);
    if C <> 0 then
      Value := MinValue;
  end { TInputInteger.HandleEvent };

{---------------------------------------------------------------------------}
{----------------------------  TInputLongInt  ------------------------------}
{---------------------------------------------------------------------------}

Constructor TInputLongInt.Init;
  begin
    Value := 0;
    MaxLen := Trunc(ln(AMaxValue)/ln(10))+1;
    TInputLine.Init(Bounds, MaxLen);
    MaxValue := AMaxValue;
    MinValue := AMinValue;
    EndView := 0;
  end;

function TInputLongInt.Execute: word;
  var
    Event: TEvent;
  begin
    repeat
      Owner^.GetEvent(Event);
      HandleEvent(Event);
    until EndView <> 0;

    ClearEvent(Event);

    case EndView of
      kbEnter:
        Execute := cmOK;
      kbESC:
        Execute := cmCancel;
      else
        Execute := EndView;
    end {case};
  end;

function TInputLongInt.DataSize;
  begin
    DataSize := SizeOf(Value);
  end;

procedure TInputLongInt.GetData;
  begin
    if (Value > MaxValue) or (Value < MinValue) then
      Value := MinValue;
    Move(Value, Rec, DataSize);
  end;

procedure TInputLongInt.SetData;
  begin
    Move(Rec, Value, DataSize);
    if (Value > MaxValue) or (Value < MinValue) then
      Value := MinValue;
    Str(Value, PString(Data)^);
    DrawView;
  end;

procedure TInputLongInt.HandleEvent;
  var
    C: integer;
  begin
    SelStart := SelEnd;
    case Event.What of
      evKeyDown:
        begin
          if
            (Event.KeyCode = kbEnter) or
            (Event.KeyCode = kbESC) or

            (Event.KeyCode = kbUp) or (Event.KeyCode = kbDown) or
            ((Event.KeyCode = kbLeft) and (CurPos = 0)) or
            ((Event.KeyCode = kbRight) and (CurPos = Length(PString(
              Data)^)))

          then
            EndView := Event.KeyCode;

          if (Event.CharCode in [#8, ^Y, #0, '0', '1'..'9'])
          then
            begin
              if (Length(Data^) = MaxLen) and (CurPos < MaxLen) then
                  SetLength(Data^, Length(Data^)-1);
              TInputLine.HandleEvent(Event);
            end;
        end;
      evMouseDown:
        TInputLine.HandleEvent(Event);
    end {case};
    Val(PString(Data)^, Value, C);
    if C <> 0 then
      Value := MaxValue;
  end { TInputLongInt.HandleEvent };

{---------------------------------------------------------------------------}
{-------------------------------  TInputDate  ------------------------------}
{---------------------------------------------------------------------------}

Constructor TInputDate.Init;
  begin
    AssignDate(1991, 1, 1, Value);
    TInputLine.Init(Bounds, 10);
    MaxValue := AMaxValue;
    MinValue := AMinValue;
    EndView := 0;
    BlockCursor;
  end;

function TInputDate.Execute: word;
  var
    Event: TEvent;
  begin
    repeat
      Owner^.GetEvent(Event);
      HandleEvent(Event);
    until EndView <> 0;

    ClearEvent(Event);

    case EndView of
      kbEnter:
        Execute := cmOK;
      kbESC:
        Execute := cmCancel;
      else
        Execute := EndView;
    end {case};
  end;

function TInputDate.DataSize;
  begin
    DataSize := SizeOf(Value);
  end;

function SStr(Value, C: word): String;
  var
    s: String;
    i: integer;
  begin
    Str(Value: C, s);
    for i := 1 to C do
      if s[i] = ' ' then
        s[i] := '0';
    SStr := s;
  end;

procedure TInputDate.GetData;
  begin
    SetInLimits(Value, MinValue, MaxValue);
    Move(Value, Rec, DataSize);
  end;

procedure TInputDate.SetData;
  begin
    Move(Rec, Value, DataSize);
    SetInLimits(Value, MinValue, MaxValue);
    DisposeStr(Data);
    Data := NewStr(SStr(Value.Day, 2)+'/'+SStr(Value.Month, 2)+'/'+
      SStr(Value.Year, 4));
    DrawView;
  end;

procedure TInputDate.HandleEvent;
  var
    C: integer;
    s: String;
  begin
    case Event.What of
      evKeyDown:
        begin
          if
            (Event.KeyCode = kbEnter) or
            (Event.KeyCode = kbESC) or

            (Event.KeyCode = kbUp) or (Event.KeyCode = kbDown) or
            ((Event.KeyCode = kbLeft) and (CurPos = 0)) or
            ((Event.KeyCode = kbRight) and (CurPos = Length(PString(
              Data)^)-1))

          then
            EndView := Event.KeyCode;

          if (Event.KeyCode = kbIns) or (Event.KeyCode = kbDel)
          then
            ClearEvent(Event);
          if (Event.CharCode in [#0, '0', '1'..'9'])
          then
            begin
              if (Event.KeyCode = kbLeft) and (CurPos in [3, 6])
              then
                Dec(CurPos);
              if (Event.KeyCode = kbRight) and (CurPos in [1, 4])
              then
                Inc(CurPos);
              TInputLine.HandleEvent(Event);
            end;
        end;
      evMouseDown:
        TInputLine.HandleEvent(Event);
    end {case};
    if CurPos in [2, 5] then
      Inc(CurPos);
    if CurPos = 10 then
      Dec(CurPos);

    s := Copy(PString(Data)^, 1, 2);
    Val(s, Value.Day, C);
    if not (Value.Day in [1..31]) then
      Value.Day := 1;

    s := Copy(PString(Data)^, 4, 2);
    Val(s, Value.Month, C);
    if not (Value.Month in [1..12]) then
      Value.Month := 1;

    s := Copy(PString(Data)^, 7, 4);
    Val(s, Value.Year, C);

    DisposeStr(Data);
    Data := NewStr(SStr(Value.Day, 2)+'/'+SStr(Value.Month, 2)+'/'+
      SStr(Value.Year, 4));
    DrawView;
  end { TInputDate.HandleEvent };

{---------------------------------------------------------------------------}
{-------------------------------  TInputReal  ------------------------------}
{---------------------------------------------------------------------------}

Constructor TInputReal.Init;
  begin
    Value := 0;
    MaxLen := Trunc(ln(AMaxValue)/ln(10))+1+byte(AMinValue < 0) {-}+1
      {.}+D {Decimals};
    TInputLine.Init(Bounds, MaxLen);
    MaxValue := AMaxValue;
    MinValue := AMinValue;
    Decimals := D;
    EndView := 0;
  end;

function TInputReal.Execute: word;
  var
    Event: TEvent;
  begin
    repeat
      Owner^.GetEvent(Event);
      HandleEvent(Event);
    until EndView <> 0;

    ClearEvent(Event);

    case EndView of
      kbEnter:
        Execute := cmOK;
      kbESC:
        Execute := cmCancel;
      else
        Execute := EndView;
    end {case};
  end;

function TInputReal.DataSize;
  begin
    DataSize := SizeOf(Value);
  end;

procedure TInputReal.GetData;
  begin
    if (Value > MaxValue) or (Value < MinValue) then
      Value := MinValue;
    Move(Value, Rec, DataSize);
  end;

procedure TInputReal.SetData;
  begin
    Move(Rec, Value, DataSize);
    Str(Value: 0: Decimals, PString(Data)^);
  end;

procedure TInputReal.HandleEvent;
  var
    C: integer;
  begin
    case Event.What of
      evKeyDown:
        begin
          if
            (Event.KeyCode = kbEnter) or
            (Event.KeyCode = kbESC) or

            (Event.KeyCode = kbUp) or (Event.KeyCode = kbDown) or
            ((Event.KeyCode = kbLeft) and (CurPos = 0)) or
            ((Event.KeyCode = kbRight) and (CurPos = Length(PString(
              Data)^)))

          then
            EndView := Event.KeyCode;

          if (Event.CharCode in [#8, ^Y, #0, '0'..'9']) or
            ((CurPos = 0) and (Event.CharCode = '-') and
            (PString(Data)^[1] <> '-') and (MinValue < 0)) or
            ((Event.CharCode = '.') and (Pos('.', PString(Data)^) = 0))
          then
            begin
              TInputLine.HandleEvent(Event);
            end;
        end;
      evMouseDown:
        TInputLine.HandleEvent(Event);
    end {case};
    Val(PString(Data)^, Value, C);
    if C <> 0 then
      Value := MaxValue;
  end { TInputReal.HandleEvent };

{---------------------------------------------------------------------------}
{-------------------------------  TInputString  ----------------------------}
{---------------------------------------------------------------------------}

function TInputString.Execute: word;
  var
    Event: TEvent;
  begin
    EndView := 0;
    repeat
      Owner^.GetEvent(Event);
      HandleEvent(Event);
    until EndView <> 0;

    ClearEvent(Event);

    case EndView of
      kbEnter:
        Execute := cmOK;
      kbESC:
        Execute := cmCancel;
      else
        Execute := EndView;
    end {case};
  end { TInputString.Execute: };

procedure TInputString.HandleEvent;
  var
    C: integer;
  begin
    { SelStart := SelEnd;}
    case Event.What of
      evKeyDown:
        begin
          if
            (Event.KeyCode = kbEnter) or
            (Event.KeyCode = kbESC) or

            (Event.KeyCode = kbUp) or (Event.KeyCode = kbDown) or
            ((Event.KeyCode = kbLeft) and (CurPos = 0)) or
            ((Event.KeyCode = kbRight) and (CurPos = Length(PString(
              Data)^)))

          then
            EndView := Event.KeyCode
              { else
          if Event.CharCode >= ' ' then
           if (Length(Data^) = MaxLen) and (CurPos < MaxLen) then SetLength(Data^, Length(Data^)-1);}
        end;
    end {case};
    TInputLine.HandleEvent(Event);
  end { TInputString.HandleEvent };

procedure TInputFName.HandleEvent;
  var
    WasKey: boolean;
  begin
    WasKey := False;
    if (Event.What = evKeyDown) then
      WasKey := True;
    case Event.KeyCode of
      kbLeft:
        if CurPos = 0 then
          ClearEvent(Event);
      kbRight:
        if CurPos = Length(Data^) then
          ClearEvent(Event);
    end {case};
    if (Event.What <> evNothing) then
      begin
        inherited HandleEvent(Event);
        if WasKey then
          if CurPos-FirstPos+DrawShift > Size.X-2 then
            begin
              FirstPos := CurPos+DrawShift-(Size.X-2);
              DrawView;
            end;
      end;
  end { TInputFName.HandleEvent };

procedure TInputFName.Draw;
  var
    Color: byte;
    l, R: integer;
    B: TDrawBuffer;
    s: String;
  begin
    { if DrawShift = 0 then SelStart := SelEnd;}
    if State and sfFocused = 0 then
      Color := C1
    else
      Color := C2;
    MoveChar(B, ' ', Color, Size.X);
    s := Copy(Data^, FirstPos+1, Size.X-2*DrawShift);
    MoveStr(B[DrawShift], s, Color);
    if Options and ofSecurity <> 0 then
      MoveChar(B[DrawShift], #254, Color, Length(s));
    if DrawShift > 0 then
      begin
        if CanScroll(1) then
          MoveChar(B[Size.X-1], #16, CR, 1)
        else
          MoveChar(B[Size.X-1], rc, CR, 1);
        MoveChar(B[0], {#32}LC, CL, 1);
      end;
    if State and sfSelected <> 0 then
      begin
        if (DrawShift > 0) and CanScroll(-1) then
          MoveChar(B[0], #17, CL, 1)
        else
          MoveChar(B[0], LC, CL, 1);
        l := SelStart-FirstPos;
        R := SelEnd-FirstPos;
        if l < 0 then
          l := 0;
        if R > Size.X-2*DrawShift then
          R := Size.X-2*DrawShift;
        if l < R then
          MoveChar(B[l+1], #0, C3, R-l);
      end;
    {$IFDEF OS2}
    if PMWindowed and (WordRec(B[CurPos-FirstPos+DrawShift]).Hi and
        $F0 = $80)
    then{JO: чтобы решить пробему курсора на сером фоне в окне}
      begin
        WordRec(B[CurPos-FirstPos+DrawShift]).Hi := WordRec(B[CurPos-
          FirstPos+DrawShift]).Hi and $0F;
        if WordRec(B[CurPos-FirstPos+DrawShift]).Hi and $FF = 0 then
            WordRec(B[CurPos-FirstPos+DrawShift]).Hi := $07;
      end;
    {$ENDIF}
    WriteLine(0, 0, Size.X, Size.Y, B);
    SetCursor(CurPos-FirstPos+DrawShift, 0);
  end { TInputFName.Draw };

procedure TInputSFName.HandleEvent;
  var
    Dir, i: integer;
  begin
    Dir := 1;
    if (Event.What = evKeyDown) then
      begin
        if Length(Data^) > 12 then
          begin{JO}
            if CurPos > 12 then
              CurPos := 12;
            Data^[0] := #12;
            DrawView;
          end; {/JO}
        if ((Event.KeyCode = kbLeft) or (Event.CharCode = #8)) then
            Dir := -1;
        if (Event.CharCode = #8) and (CurPos = 9) then
          Event.KeyCode := kbLeft;
        if (Event.CharCode = '.') then
          begin
            CurPos := 9;
            ClearEvent(Event);
            DrawView;
          end;
        {if (Event.KeyCode = kbIns) or (Event.CharCode = ^V) then ClearEvent(Event);
      }
      end;
    inherited HandleEvent(Event);
    if Data <> nil then
      i := PosChar('.', Data^)
    else
      i := 0;
    if (i <> 9) and (i > 0) then
      begin
        Data^:= AddSpace(Copy(Data^, 1, Min(8, i-1)), 8)+Copy(Data^, i,
          4);
        DrawView;
      end;
    if CurPos = 8 then
      begin
        Inc(CurPos, Dir);
        DrawView
      end;
  end { TInputSFName.HandleEvent };

{     TColorPoint = object(TView)                                              }
{       Color: Word;                                                           }
{       constructor Init (var ABounds: TRect; AColor: Word);                   }
{       constructor Load (var S: TStream);                                     }
{       procedure   Store(var S: TStream);                                     }
{       procedure   Draw; virtual;                                             }
{     end;                                                                     }

Constructor TColorPoint.Init(var ABounds: TRect; AColor: byte);
  begin
    ABounds.B.X := ABounds.A.X+1;
    ABounds.B.Y := ABounds.A.Y+1;
    inherited Init(ABounds);
    Color := AColor;
  end;

Constructor TColorPoint.Load(var s: TStream);
  begin
    inherited Load(s);
    s.Read(Color, SizeOf(Color));
  end;

procedure TColorPoint.Store(var s: TStream);
  begin
    inherited Store(s);
    s.Write(Color, SizeOf(Color));
  end;

procedure TColorPoint.Draw;
  var
    B: word;
  begin
    B := Application^.GetColor(Color) shl 8+$00FE {*};
    WriteLine(0, 0, 1, 1, B);
  end;

end.
