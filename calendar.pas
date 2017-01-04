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
//  dn15112-enhanced_calendar.patch
//  dn16rc1-Calendar_improvement_diff129byMV.patch
//  dn16rc1-piwcal22.patch
//  dn16rc1-piwcal23.patch
//
//  2.0.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Calendar;

{$F+,O+,X+,S-}

interface

uses
  Drivers, Objects, Views, Collect;

type

 PCalendarView = ^TCalendarView;
 TCalendarView = object(TView)
   Year             : AWord;
   Month            : Byte;
   Days             : Byte;
   CurYear          : AWord;
   CurMonth         : Byte;
   CurDay           : Byte;
   FDay             : Byte;
   RomanEaster      : Integer;
   OrthodoxEaster   : Integer;
   TopIndex         : Integer;
   DayInfo          : PLineCollection;
   WDayCategories   : array [0..6] of Byte;
   DayCategories    : array [1..31] of Byte;
   constructor Init(Bounds: TRect);
   constructor Load(var S: TStream);
   destructor  Done; virtual;
   procedure HandleEvent(var Event: TEvent); virtual;
   procedure Draw; virtual;
   procedure Store(var S: TStream);
   procedure Update; virtual;
   procedure NextYear;
   procedure PrevYear;
   procedure NextMonth;
   procedure PrevMonth;
   procedure NextWeek;
   procedure PrevWeek;
   procedure NextDay;
   procedure PrevDay;
   procedure CurDate;
 private
   procedure SetDay ( ADay : Byte );
   procedure _NextDay;
   procedure _PrevDay;
   procedure FixNext;
   procedure FixPrev;
   procedure MonthChanged;
   procedure YearChanged;
   procedure DayChanged;
   procedure ParseCalendarIni;
 end;

 PCalendarWindow = ^TCalendarWindow;
 TCalendarWindow = object(TWindow)
   constructor Init;
   procedure   HandleEvent(var Event: TEvent); virtual;
   procedure   Awaken; virtual;
   function    GetTitle(MaxSize: Integer): TTitleStr; virtual;
   destructor  Done; virtual;
   function    GetPalette : PPalette; virtual;
 end;

procedure InsertCalendar;

implementation

uses
  Commands, dnApp, Dos, Dialogs, Advance1, DNHelp, DnIni, Advance, xTime,
  Advance7;

var
  SundayFirst : Boolean;

const

  MinYear              = 1;
  MaxYear              = High(AWord);

  CalendarViewWidth    = 28;
  CalendarViewHeight   = 11;

  GregorianThreshold   = 1582;    { 5..14 October skipped }

  FirstCategory        = 1;
  LastCategory         = 4;

{ TCalendarWindow object }

const

  CCalendarWindow = #32#33#34#35#36#205
                  + #206#207#208#209
                  + #210#211;

  { Palette layout }
  {  1 = Frame passive }
  {  2 = Frame active }
  {  3 = Frame icon }
  {  4 = ScrollBar page area }
  {  5 = ScrollBar controls }

  {  6 = StaticText/Category 0 }

  {  7 = Category 1 }
  {  8 = Category 2 }
  {  9 = Category 3 }
  { 10 = Category 4 }

  { 11 = Header     }
  { 12 = Info panel }

const
  Calend : PCalendarWindow = nil;

(*****************************************************************
 *
 * Calendar Tools
 *
 *****************************************************************)

(*****************************************************************
 *
 * FUNCTION:    GetEasterDate
 *
 * PURPOSE:     This function returns Easter Sunday day and month
 *              for a specified year and method.
 *
 * INPUTS:      Year   - Any year between 326 and 4099.
 *              Method - 1 = the original calculation based on the
 *                           Julian calendar
 *                       2 = the original calculation, with the
 *                           Julian date converted to the
 *                           equivalent Gregorian calendar
 *                       3 = the revised calculation based on the
 *                           Gregorian calendar
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     0          - Error; invalid arguments
 *              Hi(result) - Month
 *              Lo(result) - Day
 *
 * ORIGINAL NOTES:
 *
 *              This algorithm is an arithmetic interpretation
 *              of the 3 step Easter Dating Method developed
 *              by Ron Mallen 1985, as a vast improvement on
 *              the method described in the Common Prayer Book
 *
 *              Published Australian Almanac 1988
 *              Refer to this publication, or the Canberra Library
 *              for a clear understanding of the method used
 *
 *              Because this algorithm is a direct translation of
 *              the official tables, it can be easily proved to be
 *              100% correct
 *
 *              It's free! Please do not modify code or comments!
 *
 * HISTORY:
 *
 *****************************************************************)

function GetEasterDate ( Year : Word; Method : Word ) : Word;
var
  FirstDig : Integer;   { intermediate results }
  Remain19 : Integer;
  temp     : Integer;
  tA       : Integer;   { table A to E results }
  tB       : Integer;
  tC       : Integer;
  tD       : Integer;
  tE       : Integer;
  d        : Integer;   { Easter Sunday day    }
begin
  GetEasterDate := 0;                           { default invalid result    }
  if (Year < 326) or (Year > 4099) then         { Easter dates are valid    }
    Exit;                                       { for years 326..4099       }
  if (Year < 1583) and (Method <> 1) then       { Wester or Orthodox Easter }
    Exit;                                       { is valid since 1583       }
  FirstDig       := Year div 100;               { first two digits of year  }
  Remain19       := Year mod 19;                { remainder of year / 19    }
  if (Method = 1) or (Method = 2) then begin
    { calculate PFM date }
    tA := ((225 - 11 * Remain19) mod 30) + 21;
    { find the next Sunday }
    tB   := (tA - 19) mod 7;
    tC   := (40 - FirstDig) mod 7;
    temp := Year mod 100;
    tD   := (temp + temp div 4) mod 7;
    tE   := ((20 - tB - tC - tD) mod 7) + 1;
    d    := tA + tE;
    if Method = 2 then begin
      { convert Julian to Gregorian date }
      { 10 days were skipped in the Gregorian calendar from 5-14 Oct 1582 }
      temp := 10;
      { only 1 in every 4 century years are leap years in the Gregorian   }
      { calendar (every century is a leap year in the Julian calendar)    }
      if Year > 1600 then
        temp := temp + FirstDig - 16 - ((FirstDig - 16) div 4);
      d := d + temp;
    end;
  end else if Method = 3 then begin
    { calculate PFM date }
    temp := (FirstDig - 15) div 2 + 202 - 11 * Remain19;
    if (FirstDig > 26) then
      temp := temp - 1;
    if (FirstDig > 38) then
      temp := temp - 1;
    if FirstDig in [21,24,25,33,36,37] then
      temp := temp - 1;
    temp := temp mod 30;
    tA   := temp + 21;
    if temp = 29 then
      tA := tA - 1;
    if (temp = 28) and (Remain19 > 10) then
      tA := tA - 1;
    { find the next Sunday }
    tB := (tA - 19) mod 7;
    tC := (40 - FirstDig) mod 4;
    if tC = 3 then
      tC := tC + 1;
    if tC > 1 then
      tC := tC + 1;
    temp := Year mod 100;
    tD   := (temp + temp div 4) mod 7;
    tE   := ((20 - tB - tC - tD) mod 7) + 1;
    d    := tA + tE;
  end else begin
    Exit;
  end;
  if d > 61 then begin
    { when the original calculation is converted to the Gregorian }
    { calendar, Easter Sunday can occur in May                    }
    GetEasterDate := $0500 + (d - 61);
  end else if d > 31 then begin
    GetEasterDate := $0400 + (d - 31);
  end else begin
    GetEasterDate := $0300 + d;
  end;
end;

(*****************************************************************
 *
 * FUNCTION:    IsLeapYear
 *
 * PURPOSE:     This function checks if given year is a leap year.
 *
 * INPUTS:      Year - A year to check.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - The given year is a leap year.
 *              False - The given year is not a leap year.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function IsLeapYear ( Year : Word ) : Boolean;
begin
  if Year > GregorianThreshold then
    IsLeapYear := ((Year mod 4) = 0) and (((Year mod 100) <> 0) or ((Year mod 400) = 0))
  else
    IsLeapYear := (Year mod 4) = 0;
end;

(*****************************************************************
 *
 * FUNCTION:    DaysInMonth
 *
 * PURPOSE:     This function returns number of days in given
 *              month. The year is important to calculating
 *              February days.
 *
 * INPUTS:      Month - A month number (1..12).
 *              Year  - A year.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     Number of days for selected month.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function DaysInMonth ( Month, Year : Word ) : Word;
begin
  case Month of
    1, 3, 5, 7, 8, 10, 12 :
      DaysInMonth := 31;
    4, 6, 9, 11 :
      DaysInMonth := 30;
    2 :
      DaysInMonth := 28+Ord(IsLeapYear(Year));
  else
    DaysInMonth := 0;
  end;
end;

(*****************************************************************
 *
 * FUNCTION:    DayOfYear
 *
 * PURPOSE:     This function return the day number in a year.
 *
 * INPUTS:      Day   - A day in month.
 *              Month - A month in a year (1..12).
 *              Year  - A year.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     The day number in a year:
 *                1   - 1 January
 *                ...
 *                365 - 31 December (ordinary year)
 *                366 - 31 December (leap year)
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function DayOfYear ( Day, Month, Year : Word ) : Word;
begin
  if (Year = GregorianThreshold) and (((Month = 10) and (Day > 14)) or (Month > 10)) then
    Dec ( Day, 10 );
  while Month > 1 do begin
    Dec ( Month );
    Inc ( Day, DaysInMonth ( Month, Year ) );
  end;
  DayOfYear := Day;
end;

(*****************************************************************
 *
 * FUNCTION:    JulianDay
 *
 * PURPOSE:     This function calculates day number since 1.1.1.
 *
 * INPUTS:      Day   - A day in month.
 *              Month - A month in a year (1..12).
 *              Year  - A year.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     1   = 1.1.1
 *              365 = 31.12.1
 *              366 = 1.1.2
 *              ...
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function JulianDay ( Day, Month, Year : Word ) : Longint;
var
  result : Longint;
begin
  Dec ( Year );
  result := Longint(365) * Year + Year div 4;
  if Year >= GregorianThreshold then
    Dec ( result, Year div 100 - Year div 400 - 2 );
  Inc ( result, DayOfYear ( Day, Month, Year+1 ) );
  JulianDay := result;
end;

(*****************************************************************
 *
 * FUNCTION:    DayOfWeek
 *
 * PURPOSE:     This function returns weekday for given date.
 *
 * INPUTS:      Day   - A day in month.
 *              Month - A month in a year (1..12).
 *              Year  - A year.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     0 - Sunday
 *              1 - Monday
 *              2 - Tuesday
 *              3 - Wednesday
 *              4 - Thursday
 *              5 - Friday
 *              6 - Saturday
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function DayOfWeek ( Day, Month, Year : Word ) : Word;
begin
  DayOfWeek := (JulianDay ( Day, Month, Year ) + 5) mod 7;
end;

(*****************************************************************
 *
 * FUNCTION:    WeekNumber
 *
 * PURPOSE:     This function returns week number in a year for
 *              a given day.
 *
 * INPUTS:      Day         - A day in month.
 *              Month       - A month in a year (1..12).
 *              Year        - A year.
 *              SundayFirst - Is a Sunday first day of week?
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     Week number from 1 to 53.
 *
 * NOTES:       1) What Is the Week Number?
 *                 -----------------------------
 *                 International standard IS-8601 (mentioned in
 *                 section 5.5) assigns a number to each week of
 *                 the year. A week that lies partly in one year
 *                 and partly in another is assigned a number in
 *                 the year in which most of its days lie. This
 *                 means that
 *
 *                   Week 1 of any year is the week that contains
 *                   4 January,
 *
 *                 or equivalently
 *
 *                   Week 1 of any year is the week that contains
 *                   the first Thursday in January.
 *
 *                 Most years have 52 weeks, but years that start
 *                 on a Thursday and leap years that start on a
 *                 Wednesday have 53 weeks.
 *
 *              2) According to note 1) Sunday is a last day in
 *                 a week. But there is SundayFirst argument that
 *                 calculates weeks with Sunday as a first day of
 *                 week. This is not compatible with ISO standard
 *                 but is compatible with "week-rows" in DN's
 *                 calendar :).
 *
 * HISTORY:
 *
 *****************************************************************)

function WeekNumber ( Day, Month, Year : Word; SundayFirst : Boolean ) : Word;
var
  w : Word;
  d : Word;
begin
  d := DayOfYear ( Day, Month, Year ); { current day in a year }
  w := DayOfWeek ( Day, Month, Year );
  if (w = 0) and not SundayFirst then
    w := 7;
  { look for thursday and its year in the week }
  if w < 4 then begin          { forward  }
    Inc ( d, 4-w );
    w := DayOfYear ( 31, 12, Year );
    if d > w  then begin
      Inc ( Year );
      Dec ( d, w );
    end;
  end else if w > 4 then begin { backward }
    Dec ( w, 4 );
    if d <= w then begin
      Dec ( Year );
      Inc ( d, DayOfYear ( 31, 12, Year ) );
    end;
    Dec ( d, w );
  end;
  { look for the first thurstay in the year }
  w := (11 - DayOfWeek ( 1, 1, Year )) mod 7;
  { calculate week number }
  WeekNumber := (d-w) div 7 + 1;
end;

(*****************************************************************
 *
 * Calendar.Ini Tools
 *
 *****************************************************************)

function OpenCalendarIni : PTextReader;
begin
  OpenCalendarIni := New ( PTextReader, Init ( SourceDir + 'CALENDAR.INI' ) );
end;

(*****************************************************************
 *
 * TCalendarWindow
 *
 *****************************************************************)

constructor TCalendarWindow.Init;
var
  R : TRect;
  p : PCalendarView;
begin
  if CalSundayFirst < 2
    then SundayFirst := (CalSundayFirst = 1)
    else SundayFirst := not (UpStrg(ActiveLanguage) = 'RUSSIAN');
  R.Assign ( 1, 1, CalendarViewWidth+3, CalendarViewHeight+3 );
  inherited Init ( R, GetString(dlcTitle), 0 );
  Options  := Options or ofVersion20;
  GrowMode := 0;
  Flags    := Flags and not(wfZoom + wfGrow) or (wfMove + wfClose); { Not resizeable }
  MoveTo ( 25, 7 );
  GetExtent ( R );
  R.Grow ( -1, -1 );
  Insert ( New ( PCalendarView, Init ( R ) ) );
  Calend  := @Self;
  HelpCtx := hcCalendar;
end;

function TCalendarWindow.GetTitle ( MaxSize : Integer ) : TTitleStr;
begin
  GetTitle := GetString ( dlcTitle );
end;

procedure TCalendarWindow.HandleEvent(var Event: TEvent);
begin
  case Event.What of

    evCommand : repeat
      case Event.Command of
        cmGetName : PString(Event.InfoPtr)^ := GetString ( dlcTitle );
      else
        Break;  { skip ClearEvent without using goto statement }
      end;
      ClearEvent ( Event );
    until True;

    evKeyDown: begin
      case Event.KeyCode of
        kbESC, kbEnter : begin
          Event.What    := evCommand;
          Event.Command := cmClose;
        end;
      end;
    end;

  end;
  inherited HandleEvent ( Event );
end;

procedure TCalendarWindow.Awaken;
begin
  inherited Awaken;
  Calend := @Self;
end;

destructor TCalendarWindow.Done;
begin
  Calend := nil;
  inherited Done;
end;

function TCalendarWindow.GetPalette : PPalette;
const
  P : string[Length(CCalendarWindow)] = CCalendarWindow;
begin
  GetPalette := @P;
end;

(*****************************************************************
 *
 * TCalendarView
 *
 *****************************************************************)

constructor TCalendarView.Init ( Bounds : TRect );
begin
  inherited Init ( Bounds );
  Options   := Options or ofSelectable;
  EventMask := EventMask or evMouseAuto or evBroadcast;
  DayInfo   := New ( PLineCollection, Init ( 3, 3 ) );
  TopIndex  := 0;
  UpdTicks  := 20;
  RegisterToBackground(@Self);
  YearChanged;
  MonthChanged;
  CurDate;
  DrawView;
end;

constructor TCalendarView.Load ( var S : TStream );
var
  H, y, m, d : Word;
begin
  inherited Load ( S );
  DayInfo   := New ( PLineCollection, Init ( 3, 3 ) );
  GetDate ( y, m, d, H );
  CurYear  := y;
  CurMonth := Lo(m);
  CurDay   := Lo(d);
  TopIndex := 0;
  UpdTicks := 20;
  RegisterToBackground(@Self);
  S.Read ( Year, SizeOf ( Year ) );
  S.Read ( Month, SizeOf ( Month ) );
  S.Read ( FDay, SizeOf ( FDay ) );
  Days  := DaysInMonth ( Month, Year );
  YearChanged;
  MonthChanged;
  DayChanged;
  DrawView;
end;

destructor TCalendarView.Done;
begin
  PObject(DayInfo)^.Free;
  inherited Done;
end;

procedure TCalendarView.Draw;
const
  uPalette = #3#6#7#8#9#10#11#12;
var
  Colors  : array [1..8] of Byte;
  B       : array [0..CalendarViewWidth] of AWord;
  S1      : string[CalendarViewWidth];
  S2      : string[3];
  w       : Integer;
  i, j, d : Integer;
  c       : Byte;
  wc, dc  : Byte;
  DayOf   : Byte;
  Width   : Integer;
begin
  {0123456789012345678901234567}
  { July          2000      }
  { Mo  Tu  We  Th  Fr  Sa  Su }

  Width := Size.X;

  S1 := uPalette;
  for i := Low(Colors) to High(Colors) do
    Colors[i] := GetColor ( Ord(S1[i]) );

  { draw header }
  MoveChar ( B[0], ' ', Colors[7], Width );
  S1 := GetString(TStrIdx(Ord(dlcJanuary)+Month-1));
  MoveStr ( B[1], S1, Colors[7] );
  Str ( Year:4, S1 );
  MoveStr ( B[15], S1, Colors[7] );
  MoveStr ( B[19], ' '#17'  '#16'  '#04' ', Colors[1] );
  WriteLine ( 0, 0, Width, 1, B );

  { draw weekdays }
  MoveChar ( B[0], ' ', Colors[2], Width );
  S1 := DaysOfWeek;
  if (Length(S1) <> 14) and (Length(S1) <> 21) then begin
    S1 := GetString ( stDaysWeek );
  end;
  if Length(S1) = 14 then begin
    for i := 6 downto 0 do
      Insert ( ' ', S1, i*2+1 );
  end;
  if SundayFirst then
    w := 0
  else
    w := 1;
  for i := 0 to 6 do begin
    j  := (i + w) mod 7;
    c  := Colors[2];
    wc := WDayCategories[j];
    if wc in [FirstCategory..LastCategory] then
      c := Colors[wc-FirstCategory+3];
    MoveStr ( B[i*4], Copy ( S1, j*3+1, 3 )+' ', c );
  end;
  WriteLine ( 0, 1, Width, 1, B );

  { draw days }
  DayOf := DayOfWeek ( 1, Month, Year );
  w     := 2 - DayOf;
  if SundayFirst then
    Dec ( w, 1 );
  if w > 1 then
    Dec ( w, 7 );
  for i := 1 to 6 do begin
    MoveChar ( B[0], ' ', Colors[2], Width );
    for j := 0 to 6 do begin
      c := Colors[2];
      if (w < 1) or (w > Days) then begin
        S1 := '    ';
      end else begin
        Str ( w:2, S1 );
        if w = FDay then
          S1 := #16+S1+#17
        else
          S1 := ' '+S1+' ';
        wc := WDayCategories[DayOf];
        dc := DayCategories[w];
        if (CalOptionFlags and $0001) = 0 then begin
          if (wc > 0) and (dc > 0) then begin
            if wc < dc then
              dc := 0
            else
              wc := 0;
          end;
        end;
        if wc in [FirstCategory..LastCategory] then
          c := Colors[wc-FirstCategory+3];
        if dc in [FirstCategory..LastCategory] then
          c := Colors[dc-FirstCategory+3];
        DayOf := (DayOf+1) mod 7;
      end;
      MoveStr ( B[j*4], S1, c );
      Inc ( w );
      if (Year = GregorianThreshold) and (Month = 10) and (w in [5..14]) then
        w := 15;
    end;
    { week/day indicator }
    if (i = 6) and ((CalOptionFlags and $0008) <> 0) then begin
      w := WeekNumber ( FDay, Month, Year, SundayFirst and ((CalOptionFlags and $0010) <> 0) );
      d := DayOfYear ( FDay, Month, Year );
      Str ( w:2, S1 );
      Str ( d:3, S2 );
      S1 := '['+S1+'/'+S2+']';
      MoveStr ( B[20], S1, Colors[2] );
    end;
    WriteLine ( 0, i + 1, Width, 1, B );
  end;

  { draw info }
  for i := 8 to CalendarViewHeight-1 do begin
    MoveChar ( B[0], ' ', Colors[8], Width );
    if i-8 < DayInfo^.Count then begin
      j := (i-8+TopIndex) mod DayInfo^.Count;
      MoveStr ( B[0], Copy ( PString(DayInfo^.At ( j ))^, 1, Width ), Colors[8] );
    end;
    WriteLine ( 0, i, Width, 1, B );
  end;

end;

procedure TCalendarView.HandleEvent ( var Event : TEvent );
var
  Point     : TPoint;
  SelectDay : Integer;
begin
  inherited HandleEvent ( Event );
  if (State and sfSelected <> 0) then begin
    if Event.What and (evMouseDown + evMouseAuto) <> 0 then begin
      MakeLocal ( Event.Where, Point );
      if Point.Y = 0 then repeat
        case Point.X of

          18,19,20 : begin
            if (Event.Buttons and mbLeftButton) <> 0 then
              PrevMonth
            else if (Event.Buttons and mbRightButton) <> 0 then
              PrevYear
            else
              Break;
          end;

          21,22,23 : begin
            if (Event.Buttons and mbLeftButton) <> 0 then
              NextMonth
            else if (Event.Buttons and mbRightButton) <> 0 then
              NextYear
            else
              Break;
          end;

          24,25,26 : begin
            CurDate;
          end;

          else
            Break;
        end;
        DrawView;
      until True else if (Point.Y >= 2) and (Point.Y <= 7) then begin
        SelectDay := 2-DayOfWeek ( 1, Month, Year );
        if SundayFirst then
          Dec ( SelectDay, 1 );
        if SelectDay > 1 then
          Dec ( SelectDay, 7 );
        Inc ( SelectDay, (Point.Y-2)*7 + (Point.X div 4) );
        if (Year = GregorianThreshold) and (Month = 10) and (SelectDay > 4) then
          Inc ( SelectDay, 10 );
        if (SelectDay >= 1) and (SelectDay <= DaysInMonth ( Month, Year )) then begin
          SetDay ( SelectDay );
          DrawView;
        end;
      end;
    end else if Event.What = evBroadcast then begin
      if Event.Command = cmDnIniChanged then begin
        DayChanged;
        DrawView;
      end;
    end else if Event.What = evKeyDown then begin
      if Event.CharCode = '+' then
        Event.KeyCode := kbRight
      else if Event.CharCode = '-' then
        Event.KeyCode := kbLeft;
      repeat
        case Event.KeyCode of
          kbRight                 : NextDay;
          kbLeft                  : PrevDay;
          kbUp                    : PrevWeek;
          kbDown                  : NextWeek;
          kbCtrlLeft,  kbPgUp     : PrevMonth;
          kbCtrlRight, kbPgDn     : NextMonth;
          kbAltLeft,   kbCtrlPgUp : PrevYear;
          kbAltRight,  kbCtrlPgDn : NextYear;
          kbSpace,     kbHome     : CurDate;
          else Break;
        end;
        ClearEvent ( Event );
        DrawView;
      until True;
    end;
  end;
end;

procedure TCalendarView.Store(var S: TStream);
begin
  inherited Store ( S );
  S.Write ( Year, SizeOf ( Year ) );
  S.Write ( Month, SizeOf ( Month ) );
  S.Write ( FDay, SizeOf ( FDay ) );
end;

procedure TCalendarView.Update;
begin
  if TimerExpired ( UpTmr ) and (DayInfo^.Count > 3) then begin
    Inc ( TopIndex );
    DrawView;
  end;
end;

procedure TCalendarView.NextYear;
begin
  if Year < MaxYear then begin
    Inc ( Year );
    if Month = 2 then begin
      Days := DaysInMonth ( Month, Year );
      if FDay > Days then
        FDay := Days;
    end;
    YearChanged;
  end else begin
    Month := 12;
    FDay  := 31;
  end;
  FixNext;
  MonthChanged;
  DayChanged;
end;

procedure TCalendarView.PrevYear;
begin
  if Year > MinYear then begin
    Dec ( Year );
    if Month = 2 then begin
      Days := DaysInMonth ( Month, Year );
      if FDay > Days then
        FDay := Days;
    end;
    YearChanged;
  end else begin
    Month := 1;
    FDay  := 1;
  end;
  FixPrev;
  MonthChanged;
  DayChanged;
end;

procedure TCalendarView.NextMonth;
begin
  if Month < 12 then begin
    Inc ( Month );
  end else if Year < MaxYear then begin
    Month := 1;
    Inc ( Year );
    YearChanged;
  end else begin
    FDay := 31;
  end;
  Days := DaysInMonth ( Month, Year );
  if FDay > Days then
    FDay := Days;
  FixNext;
  MonthChanged;
  DayChanged;
end;

procedure TCalendarView.PrevMonth;
begin
  if Month > 1 then begin
    Dec ( Month );
  end else if Year > MinYear then begin
    Month := 12;
    Dec ( Year );
    YearChanged;
  end else begin
    FDay := 1;
  end;
  Days := DaysInMonth ( Month, Year );
  if FDay > Days then
    FDay := Days;
  FixPrev;
  MonthChanged;
  DayChanged;
end;

procedure TCalendarView.NextWeek;
var
  i : Integer;
begin
  for i := 1 to 7 do
    _NextDay;
  DayChanged;
end;

procedure TCalendarView.PrevWeek;
var
  i : Integer;
begin
  for i := 1 to 7 do
    _PrevDay;
  DayChanged;
end;

procedure TCalendarView.NextDay;
begin
  _NextDay;
  DayChanged;
end;

procedure TCalendarView.PrevDay;
begin
  _PrevDay;
  DayChanged;
end;

procedure TCalendarView.CurDate;
var
  h  : Word;
  y  : Word;
  m  : Word;
  d  : Word;
  b1 : Boolean;
  b2 : Boolean;
begin
  GetDate ( y, m, d, h );
  CurYear  := y;
  CurMonth := m;
  CurDay   := d;
  b1       := CurYear <> Year;
  b2       := (CurMonth <> Month) or b1;
  Year     := CurYear;
  Month    := CurMonth;
  Days     := DaysInMonth ( Month, Year );
  FDay     := CurDay;
  if b1 then
    YearChanged;
  if b2 then
    MonthChanged;
  DayChanged;
end;

procedure TCalendarView.SetDay ( ADay : Byte );
begin
  if FDay = ADay then
    Exit;
  FDay := ADay;
  DayChanged;
end;

procedure TCalendarView._NextDay;
begin
  if FDay < Days then begin
    Inc ( FDay );
  end else if Month < 12 then begin
    FDay := 1;
    Inc ( Month );
    Days := DaysInMonth ( Month, Year );
    MonthChanged;
  end else if Year < MaxYear then begin
    FDay  := 1;
    Month := 1;
    Inc ( Year );
    Days := DaysInMonth ( Month, Year );
    YearChanged;
    MonthChanged;
  end;
  FixNext;
end;

procedure TCalendarView._PrevDay;
begin
  if FDay > 1 then begin
    Dec ( FDay );
  end else if Month > 1 then begin
    Dec ( Month );
    Days := DaysInMonth ( Month, Year );
    FDay := Days;
    MonthChanged;
  end else if Year > MinYear then begin
    Dec ( Year );
    Month := 12;
    Days  := DaysInMonth ( Month, Year );
    FDay  := Days;
    YearChanged;
    MonthChanged;
  end;
  FixPrev;
end;

procedure TCalendarView.FixNext;
begin
  if (Year = GregorianThreshold) and (Month = 10) and (FDay in [5..14]) then
    FDay := 15;
end;

procedure TCalendarView.FixPrev;
begin
  if (Year = GregorianThreshold) and (Month = 10) and (FDay in [5..14]) then
    FDay := 4;
end;

procedure TCalendarView.DayChanged;
begin
  TopIndex := 0;
  ParseCalendarIni;
end;

procedure TCalendarView.MonthChanged;
begin
  {might be useful in the future}
end;

procedure TCalendarView.YearChanged;
var
  Easter : Word;
begin
  if Year <= GregorianThreshold then begin
    Easter         := GetEasterDate ( Year, 1 );
    RomanEaster    := DayOfYear ( Lo(Easter), Hi(Easter), Year );
    OrthodoxEaster := RomanEaster;
  end else begin
    Easter         := GetEasterDate ( Year, 3 );
    RomanEaster    := DayOfYear ( Lo(Easter), Hi(Easter), Year );
    Easter         := GetEasterDate ( Year, 2 );
    OrthodoxEaster := DayOfYear ( Lo(Easter), Hi(Easter), Year );
  end;
end;

procedure TCalendarView.ParseCalendarIni;
const
  LToken = 20;
var
  CaseStr  : string;
  key      : string;
  token    : string[LToken];
  first    : Integer;
  last     : Integer;
  curr     : Integer;
  category : Byte;

  procedure CheckCategory;
  var
    c : Byte;
    i : Integer;
    j : Integer;
  begin
    c := 0;
    i := Pos ( ',', CaseStr );
    if i = 0 then
      i := Length ( CaseStr ) + 1;
    if i > 1 then begin
      Val ( Copy ( CaseStr, 1, i-1 ), c, j );
      if (j <> 0) or (c < FirstCategory) or (c > LastCategory) then
        c := 0;
    end;
    if c <> 0 then begin
      Delete ( CaseStr, 1, i );
      DelLeft ( CaseStr );
    end;
    category := c;
  end;

  procedure InsertCategory ( var ADest : Byte );
  begin
    if (category <> 0) and ((ADest = 0) or (category < ADest)) then
      ADest := category;
  end;

  procedure InsertInfo;
  var
    i : Integer;
  begin
    FreeStr := CaseStr;
    while FreeStr <> '' do begin
      i := Pos ( ',', FreeStr );
      if i = 0 then
        i := Length ( FreeStr ) + 1;
      DayInfo^.Insert ( NewStr ( Copy ( FreeStr, 1, i-1 ) ) );
      Delete ( FreeStr, 1, i );
    end;
  end;

  procedure StrToken;
  var
    i : Integer;
  begin
    token := '';
    i     := Pos ( ' ', FreeStr );
    if i = 0 then
      i := Length ( FreeStr ) + 1;
    if i > High(token) then
      Exit;
    token := Copy ( FreeStr, 1, i-1 );
    Delete  ( FreeStr, 1, Length(token) );
    DelLeft ( FreeStr );
  end;

  function CheckLanguage : Boolean;
  begin
    CheckLanguage := False;
    if token <> '' then begin { check language }
      if FreeStr <> '' then
        Exit;                 { syntax error }
      if token <> UpStrg  ( LngId ) then
        Exit;
    end;
    CheckLanguage := True;
  end;

  function CheckOffset ( var Offset : Integer ) : Boolean;
  var
    i : Integer;
    o : Integer;
  begin
    CheckOffset := False;
    Val ( token, o, i );
    if (i <> 0) or (o = 0) then
      Exit;
    Offset      := o;
    CheckOffset := True;
  end;

  function CheckByte ( var B : Byte; Max : Byte ) : Boolean;
  var
    i : Integer;
    x : Byte;
  begin
    CheckByte := False;
    if token <> '*' then begin
      Val ( token, x, i );
      if (i <> 0) or (x < 1) or (x > Max) then
        Exit;
      B := x;
    end;
    CheckByte := True;
  end;

  function CheckYear ( var Min, Max : Word ) : Boolean;
  var
    y : Word;
    i : Integer;
    s : string[LToken];
  begin
    CheckYear := True;
    Min       := 1;
    Max       := MaxYear;
    if token = '' then begin
      CheckYear := False;
      Exit;
    end;
    if token = '*' then
      Exit;
    Val ( token, y, i );
    if (i = 0) and (1 <= y) and (y <= MaxYear) then begin
      Min := y;
      Max := y;
      Exit;
    end;
    s := token;
    if s[1] = '*' then begin
      Delete ( s, 1, 1 );
      DelLeft ( s );
      if s[1] = '-' then begin
        Delete ( s, 1, 1 );
        DelLeft ( s );
        if s = '*' then
          Exit;
        Val ( s, y, i );
        if (i = 0) and (1 <= y) and (y <= MaxYear) then begin
          Max := y;
          Exit;
        end;
      end;
    end else if s[Length(s)] = '*' then begin
      Delete ( s, Length(s), 1 );
      DelRight ( s );
      if s[Length(s)] = '-' then begin
        Delete ( s, Length(s), 1 );
        DelRight ( s );
        if s = '*' then
          Exit;
        Val ( s, y, i );
        if (i = 0) and (1 <= y) and (y <= MaxYear) then begin
          Min := y;
          Exit;
        end;
      end;
    end;
    CheckYear := False;
  end;

  function FindWord ( const Word : string; const Words : string; Count : Integer ) : Integer;
  var
    i : Integer;
    l : Integer;
  begin
    FindWord := -1;
    if (Length(Words) mod Count) <> 0 then
      Exit;
    l := Length(Words) div Count;
    if Length(Word) <> l then
      Exit;
    for i := 0 to Count-1 do begin
      if Word = Copy ( Words, i*l+1, l ) then begin
        FindWord := i;
        Exit;
      end;
    end;
  end;

  function CheckWeekday ( var W : Byte ) : Boolean;
  const
    CWeekdays2 = 'SUMOTUWETHFRSA';
    CWeekdays3 = 'SUNMONTUEWEDTHUFRISAT';
  var
    i    : Integer;
    days : string;
  begin
    CheckWeekday := False;
    i := -1;
    if (CalOptionFlags and $0002) = 0 then begin
      days := DaysOfWeek;
      UpStr ( days );
      i := FindWord ( token, days, 7 );
    end;
    if (i = -1) and ((CalOptionFlags and $0004) = 0) then begin
      days := GetString ( stDaysWeek );
      UpStr ( days );
      i := FindWord ( token, days, 7 );
    end;
    if i = -1 then
      i := FindWord ( token, CWeekdays2, 7 );
    if i = -1 then
      i := FindWord ( token, CWeekdays3, 7 );
    if i >= 0 then begin
      W            := i;
      CheckWeekday := True;
    end;
  end;

  function CheckMonth ( var M : Byte ) : Boolean;
  const
    CMonths = 'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC';
  var
    i : Integer;
  begin
    i := FindWord ( token, CMonths, 12 );
    if i >= 0 then begin
      M          := i + 1;
      CheckMonth := True;
      Exit;
    end;
    CheckMonth := CheckByte ( m, 12 );
  end;

  function ParseRelative ( DayPivot : Integer ) : Boolean;
  var
    o : Integer;
    j : Integer;
  begin
    ParseRelative := False;
    StrToken; { offset/language }
    o := 0;
    if CheckOffset ( o ) then
      StrToken; { language }
    if not CheckLanguage then
      Exit;
    Inc ( o, DayPivot );
    if (o >= first) and (o <= last) then
      InsertCategory ( DayCategories[o-first+1] );
    if o = curr then begin
      InsertInfo;
    end;
    ParseRelative := True;
  end;

  function ParseFixed : Boolean;
  var
    d  : Byte;
    m  : Byte;
    i  : Integer;
    y0 : Word;
    y1 : Word;
  begin
    ParseFixed := False;
    FreeStr    := key;
    StrToken;
    d := 0;
    if not CheckByte ( d, 31 ) then
      Exit;
    StrToken;
    m := 0;
    if CheckMonth ( m ) then
      StrToken;
    if CheckYear ( y0, y1 ) then
      StrToken;
    if not CheckLanguage then
      Exit;
    if m = 0 then
      m := Month;
    if (m <> Month) or (y0 > Year) or (y1 < Year) then
      Exit;
    if d = 0 then begin
      for i := 1 to Days do
        InsertCategory ( DayCategories[i] );
    end else begin
      InsertCategory ( DayCategories[d] );
    end;
    if (d = 0) or (d = FDay) then begin
      InsertInfo;
    end;
    ParseFixed := True;
  end;

  function ParseRoman : Boolean;
  begin
    ParseRoman := False;
    FreeStr    := key;
    StrToken;
    if token <> 'ROMAN' then
      Exit;
    ParseRoman := ParseRelative ( RomanEaster );
  end;

  function ParseOrthodox : Boolean;
  begin
    ParseOrthodox := False;
    FreeStr       := key;
    StrToken;
    if token <> 'ORTHODOX' then
      Exit;
    ParseOrthodox := ParseRelative ( OrthodoxEaster );
  end;

  function ParseWeekday : Boolean;
  var
    o  : Integer;
    o2 : Integer;
    c  : Byte;
    d  : Byte;
    w  : Byte;
    w1 : Byte;
    m  : Byte;
    y0 : Word;
    y1 : Word;
  begin
    ParseWeekday := False;
    FreeStr      := key;
    StrToken;
    o := 0;
    if CheckOffset ( o ) then
      StrToken;
    if not CheckWeekday ( w ) then
      Exit;
    StrToken;
    if o <> 0 then begin
      d := 0;
      if CheckByte ( d, 31 ) then
        StrToken;
    end;
    m := 0;
    if CheckMonth ( m ) then
      StrToken;
    if CheckYear ( y0, y1 ) then
      StrToken;
    if o <> 0 then begin
      o2 := 0;
      if CheckOffset ( o2 ) then
        StrToken; { language }
    end;
    if not CheckLanguage then
      Exit;
    if o = 0 then begin
      if m = 0 then
        m := Month;
      if (m <> Month) or (y0 > Year) or (y1 < Year) then
        Exit;
      InsertCategory ( WDayCategories[w] );
      if w = DayOfWeek ( FDay, Month, Year ) then
        InsertInfo;
    end else begin
      if d = 0 then begin
        if o < 0 then
          d := Days
        else
          d := 1;
      end;
      if m = 0 then
        m := Month;
      if (y0 > Year) or (y1 < Year) then
        Exit;
      if d > DaysInMonth ( m, Year ) then
        Exit;
      y0 := DayOfYear ( d, m, Year );
      w1 := DayOfWeek ( d, m, Year );
      if o > 0 then begin
        Dec ( o );
        if w < w1 then
          Inc ( w, 7 );
        Dec ( w, w1 );
        Inc ( y0, w+7*o );
      end else begin
        o := -o-1;
        if w > w1 then
          Inc ( w1, 7 );
        Dec ( w1, w );
        Dec ( y0, 7*o+w1 );
      end;
      Inc ( o2, y0 );
      if (o2 >= first) and (o2 <= last) then
        InsertCategory ( DayCategories[o2-first+1] );
      if o2 = curr then
        InsertInfo;
    end;
    ParseWeekday := True;
  end;

var
  parse : Boolean;
  i     : Integer;
  f     : PTextReader;
begin
  { kill any read categories }
  FillChar ( WDayCategories, SizeOf(WDayCategories), 0 );
  FillChar ( DayCategories, SizeOf(DayCategories), 0 );
  DayInfo^.FreeAll;
  { open calendar ini }
  f := New ( PTextReader, Init ( DnIniFileName ) );
  if f <> nil then begin
    { looking for section }
    parse := False;
    { current settings }
    first := DayOfYear ( 1, Month, Year );
    last  := DayOfYear ( Days, Month, Year );
    curr  := DayOfYear ( FDay, Month, Year );
    while not f^.EOF do begin
      FreeStr := F^.GetStr;
      DelRight ( FreeStr );
      DelLeft ( FreeStr );
      CaseStr := FreeStr;
      UpStr ( FreeStr );
      if (FreeStr = '') or (FreeStr[1] = ';') then
        Continue;
      if (FreeStr[1] = '[') and (FreeStr[Length(FreeStr)] = ']') then begin
        { new section = terminate parsing }
        if parse then
          Break;
        parse := Copy ( FreeStr, 2, Length(FreeStr)-2 ) = 'CALENDAR';
        Continue;
      end;
      if parse then begin
        i := Pos ( '=', FreeStr );
        if i > 0 then begin
          { split key and value }
          Delete   ( FreeStr, i, Length(FreeStr) );
          DelRight ( FreeStr );
          Delete   ( CaseStr, 1, i );
          DelLeft  ( CaseStr );
          CheckCategory;
          key := FreeStr;
          if not ParseFixed    then
          if not ParseRoman    then
          if not ParseOrthodox then
          if not ParseWeekday  then
          ;
        end;
      end;
    end;
    Dispose ( f, Done );
  end;
  if DayInfo^.Count = 0 then begin
    CaseStr := CalDefaultInfo;
    InsertInfo;
  end;
end;

procedure InsertCalendar;
begin
  if Calend = nil then
    Application^.InsertWindow ( New ( PCalendarWindow, Init ) )
  else
    Calend^.Select;
end;

end.
