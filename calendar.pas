{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.09
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

unit Calendar;

{$F+,O+,X+,S-}

interface

uses Commands, Drivers, Objects, dnApp, Views, Dos, Dialogs, Advance1,
     DNHelp, DnIni;

const
   DaysInMonth: array[1..12] of Byte =
     (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

type
 PCalendarView = ^TCalendarView;
 TCalendarView = object(TView)
   Year, Month, Days: AWord;
   CurYear, CurMonth, CurDay : Word;
   constructor Init(Bounds: TRect);
   constructor Load(var S: TStream);
   procedure HandleEvent(var Event: TEvent); virtual;
   procedure Draw; virtual;
   procedure Store(var S: TStream);
 end;

 PCalendarWindow = ^TCalendarWindow;
 TCalendarWindow = object(TDialog)
   constructor Init;
   procedure   HandleEvent(var Event: TEvent); virtual;
   procedure   Awaken; virtual;
   function    GetTitle(MaxSize: Integer): TTitleStr; virtual;
   destructor  Done; virtual;
 end;

procedure InsertCalendar;

const
  Calend: PCalendarWindow = Nil;

implementation

{ TCalendarWindow }
constructor TCalendarWindow.Init;
var
  R:TRect;
begin
  R.Assign(1, 1, 30, 11); {JO}

  inherited Init(R, GetString(dlcTitle));
  Number := GetNum;

  Flags := Flags and not (wfZoom + wfGrow);    { Not resizeable }

   MoveTo(25, 7);

  GrowMode  := 0;

  GetExtent(R);
  R.Grow(-1, -1);
  Insert(New(PCalendarView, Init(R)));

  Calend := @Self;

  HelpCtx := hcCalendar;
end;

function TCalendarWindow.GetTitle(MaxSize: Integer): TTitleStr;
begin
 GetTitle:=GetString(dlcTitle);
end;

procedure TCalendarWindow.HandleEvent(var Event: TEvent);
 begin
  if Event.What = evCommand then
   repeat
    case Event.Command of
     cmGetName: PString(Event.InfoPtr)^ := GetString(dlcTitle);
    else
     Break;
    end;

    ClearEvent(Event);
   until True;
  if Event.What = evKeyDown then
   case Event.KeyCode of
    kbESC, kbEnter: begin
                     Event.What:=evCommand;
                     Event.Command:=cmClose;
                    end;
   end;
  inherited HandleEvent(Event);
 end;

procedure TCalendarWindow.Awaken;
 begin
  inherited Awaken;

  Calend := @Self;
 end;

destructor TCalendarWindow.Done;
 begin
  Calend := Nil;

  inherited Done;
 end;

{ TCalendarView }
constructor TCalendarView.Init(Bounds: TRect);
var
  H: Word;
begin
  inherited Init(Bounds);
  Options := Options or ofSelectable;
  EventMask := EventMask or evMouseAuto;
  GetDate(CurYear, CurMonth, CurDay, H);
  Year := CurYear;
  Month := CurMonth;
  DrawView;
end;

constructor TCalendarView.Load(var S: TStream);
var
  H: AWord;
begin
  inherited Load(S);
  GetDate(CurYear, CurMonth, CurDay, H);
  S.Read(Year, SizeOf(Year));
  S.Read(Month, SizeOf(Month));
end;

function DayOfWeek(Day, Month, Year: Integer) : Integer;
var
  century, yr, dw: Integer;
begin
  if Month < 3 then
  begin
    Inc(Month, 10);
    Dec(Year);
  end
  else
     Dec(Month, 2);
  century := Year div 100;
  yr := year mod 100;
  dw := (((26 * month - 2) div 10) + day + yr + (yr div 4) +
    (century div 4) - (2 * century)) mod 7;
  if dw < 0 then DayOfWeek := dw + 7
  else DayOfWeek := dw;
end;

procedure TCalendarView.Draw;
const
  Width = 27; {JO}
var
  i, j, DayOf, CurDays: Integer;
  S, D: String;
  B: TDrawBuffer;
  Color, BoldColor, SpecialColor: Byte;

function Num2Str(I: Integer): String;
var
  S:String;
begin
  Str(i:2, S);
  Num2Str := S;
end;

begin
  Color :=  GetColor(6);
  BoldColor :=  GetColor(3);
  DayOf := DayOfWeek(1, Month, Year);
  Days := DaysInMonth[Month] + Byte((Year mod 4 = 0) and (Month = 2));
  Str(Year:4, S);
  MoveChar(B, ' ', Color, Width);

  case Month of
   1: D := GetString(dlcJanuary);
   2: D := GetString(dlcFebruary);
   3: D := GetString(dlcMarch);
   4: D := GetString(dlcApril);
   5: D := GetString(dlcMay);
   6: D := GetString(dlcJune);
   7: D := GetString(dlcJuly);
   8: D := GetString(dlcAugust);
   9: D := GetString(dlcSeptember);
   10: D := GetString(dlcOctober);
   11: D := GetString(dlcNovember);
   12: D := GetString(dlcDecember);
  end;

  MoveCStr(B, '                   '#30'  '#31'  '#04, BoldColor);
  MoveCStr(B, AddSpace(D, 14) + S, Color);

  WriteLine(0, 0, Width, 1, B);
  MoveChar(B, ' ', Color, Width);

 {JO}

 if (Length(DaysOfWeek)<>14) and (Length(DaysOfWeek)<>21)
   then
     begin
       for j := 0 to 5 do MoveStr(B[J * 4], (Copy(GetString(stDaysWeek),J*2+3,2) + '  '), Color);
       MoveStr(B[24], Copy(GetString(stDaysWeek),1,2), Color);
     end
   else
     begin
       for j := 0 to 5 do MoveStr(B[J * 4], Copy(DaysOfWeek, (J+1)*(Length(DaysOfWeek) div 7) + 1,
                                                                                (Length(DaysOfWeek) div 7)) + '  ', Color);
       MoveStr(B[24], Copy(DaysOfWeek,1, (Length(DaysOfWeek) div 7)), Color);
     end;

 {JO}

  {MoveStr(B, GetString(dlcLine), Color);} {JO}
  WriteLine(0, 1, Width, 1, B);
  CurDays := 1 - DayOf + 1;

  for i := 1 to 6 do
  begin
    MoveChar(B, ' ', Color, Width);
    for j := 0 to 6 do
    begin
      if (CurDays < 1) or (CurDays > Days) then
        MoveStr(B[J * 4], '   ', Color) {JO}
      else
        if (Year = CurYear) and (Month = CurMonth) and
          (CurDays = CurDay) then
          MoveStr(B[J * 4], Num2Str(CurDays), BoldColor) {JO}
        else
          MoveStr(B[J * 4], Num2Str(CurDays), Color); {JO}

      Inc(CurDays);
    end;
     WriteLine(0, i + 1, Width, 1, B);
  end;
end;

procedure TCalendarView.HandleEvent(var Event: TEvent);
var
  Point:TPoint;
  SelectDay: Word;
begin
  inherited HandleEvent(Event);
  if (State and sfSelected <> 0) then
  begin
    if Event.What and (evMouseDown + evMouseAuto) <> 0 then             {JO}
    begin
      MakeLocal(Event.Where, Point);
      if ((Point.X >= 18) and (Point.X <= 20) and (Point.Y = 0)) then
        if (Event.Buttons and mbLeftButton <> 0) then
           begin
             Inc(Month);
             if Month > 12 then
             begin
               Inc(Year);
               Month := 1;
             end;
             DrawView;
           end
        else if (Event.Buttons and mbRightButton <> 0) then
           begin
             Inc(Year);
             DrawView;
           end;
      if ((Point.X >= 21) and (Point.X <= 23) and (Point.Y = 0)) then
        if (Event.Buttons and mbLeftButton <> 0) then
           begin
             Dec(Month);
             if Month < 1 then
             begin
               Dec(Year);
               Month := 12;
             end;
             DrawView;
           end
        else if (Event.Buttons and mbRightButton <> 0) then
           begin
             Dec(Year);
             DrawView;
           end;
      if ((Point.X >= 24) and (Point.X <= 26) and (Point.Y = 0)) then
        begin
          GetDate(CurYear, CurMonth, CurDay, Event.InfoWord);
          Year := CurYear;
          Month := CurMonth;
          DrawView;
        end;
    end                                                                 {JO}
    else if Event.What = evKeyDown then
    begin
      if (Lo(Event.KeyCode) = byte('+')) or
         (Event.KeyCode = kbUp) then
      begin
        Inc(Month);
        if Month > 12 then
        begin
          Inc(Year);
          Month := 1;
        end;
       ClearEvent(Event);
      end;
      if (Lo(Event.KeyCode) = Byte('-')) or
         (Event.KeyCode = kbDown) then
      begin
        Dec(Month);
        if Month < 1 then
        begin
          Dec(Year);
          Month := 12;
        end;
       ClearEvent(Event);
      end;
       case Event.KeyCode of        {JO}
        kbCtrlLeft:
          begin
            Dec(Year);
            ClearEvent(Event);
          end;
        kbCtrlRight:
          begin
            Inc(Year);
            ClearEvent(Event);
          end;
        kbSpace:
          begin
            GetDate(CurYear, CurMonth, CurDay, Event.InfoWord);
            Year := CurYear;
            Month := CurMonth;
            ClearEvent(Event);
          end;
       end;                         {JO}
      DrawView;
    end;
  end;
end;

procedure TCalendarView.Store(var S: TStream);
begin
  inherited Store(S);
  S.Write(Year, SizeOf(Year));
  S.Write(Month, SizeOf(Month));
end;

procedure InsertCalendar;
 begin
  if Calend = Nil then
   Application^.InsertWindow(New(PCalendarWindow, Init))
  else
   Calend^.Select;
 end;

end.
