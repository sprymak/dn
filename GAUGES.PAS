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

unit Gauges;

{ Useful gauges: clock and heap available viewer }
interface

uses
  Dos, Objects, Views, Drivers, FileCopy, Eraser, Collect,
  Commands, xTime
  {$IFDEF Calendar}, Calendar {$ENDIF}
  ;

type
  {$IFDEF TrashCan}
  { Trash can object }
  PTrashCan = ^TTrashCan;
  TTrashCan = object(TView)
    ImVisible: boolean;
    Constructor Init(var R: TRect);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    procedure SetState(AState: word; Enable: boolean); virtual;
    end;
  {$ENDIF}

  PKeyMacros = ^TKeyMacros;
  TKeyMacros = object(TObject)
    Limit: AInt;
    Count: AInt;
    Keys: PWordArray;
    Constructor Init;
    Constructor Load(var s: TStream);
    procedure PutKey(KeyCode: word);
    procedure Store(var s: TStream);
    procedure Play;
    destructor Done; virtual;
    end;

const
  LastHour: word = $FF;
  LastMin: word = 0;

const
  KeyMacroses: PCollection = nil;
  MacroRecord: boolean = False;

type
  PHeapView = ^THeapView;
  THeapView = object(TView)
    OldMem: longInt;
    Constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    procedure UpDate; virtual;
    end;

  PClockView = ^TClockView;
  TClockView = object(TView)
    Refresh: byte;
    LastTime: DateTime;
    LastSemi: boolean;
    TimeStr: String[16]; {-SSK (old version: string[12])}
    OldXCoord: AInt; {-SSK}
    OldShowSeconds: boolean;
    Utimer: TEventTimer;
    Constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function FormatTimeStr(H, M, SS: word): String; virtual;
    procedure UpDate; virtual;
    end;

  {$IFDEF Printer}
procedure PrintFiles(Files: PCollection; Own: PView);
{$ENDIF}

implementation
uses
  Memory, Tree, Messages, DNApp, advance, advance1, advance2,
    advance3,
  {$IFDEF OS2}Dn2PmApi, {$ENDIF} {AK155}
  {SysUtils,}
  FilesCol, RStrings, Startup, DnIni; {-$VIV}

Constructor TKeyMacros.Init;
  begin
    inherited Init;
    Limit := 10;
    Count := 0;
    Keys := MemAlloc(Limit*SizeOf(word));
    if Keys = nil then
      Fail;
  end;

destructor TKeyMacros.Done;
  begin
    if Keys <> nil then
      FreeMem(Keys, Limit*SizeOf(word));
    inherited Done;
  end;

Constructor TKeyMacros.Load;
  begin
    s.Read(Limit, SizeOf(AWord)*2);
    Keys := MemAlloc(SizeOf(AWord)*Limit);
    if Keys = nil then
      Fail;
    s.Read(Keys^, SizeOf(AWord)*Count);
  end;

procedure TKeyMacros.Store;
  begin
    s.Write(Limit, SizeOf(Limit)*2);
    s.Write(Keys^, SizeOf(AWord)*Count);
  end;

procedure TKeyMacros.PutKey;
  var
    P: Pointer;
  begin
    if Count >= Limit then
      begin
        Inc(Limit, 10);
        P := MemAlloc(Limit*SizeOf(word));
        if P = nil then
          exit;
        Move(Keys^, P^, Count*SizeOf(word));
        Keys := P;
      end;
    Keys^[Count] := KeyCode;
    Inc(Count);
  end;

procedure TKeyMacros.Play;
  var
    i: integer;
  begin
    for i := 0 to Count-1 do
      Message(Application, evKeyDown, Keys^[i], nil);
  end;

{------ Heap Window object ----------}

Constructor THeapView.Init(var Bounds: TRect);
  begin
    TView.Init(Bounds);
    OldMem := 0;
  end;

procedure THeapView.Draw;
  var
    s: String;
    B: TDrawBuffer;
    C: byte;
  begin
    OldMem := MemAvail;
    Str(OldMem, s);
    C := GetColor(2);
    MoveChar(B, ' ', C, Size.X);
    MoveStr(B, s, C);
    WriteLine(0, 0, Size.X, 1, B);
  end;

procedure THeapView.UpDate;
  begin
    if (OldMem <> MemAvail) then
      DrawView;
  end;

{-------- ClockView Object --------}

function LeadingZero(W: word): String;
  var
    s: String[20];
  begin
    Str(W: 0, s);
    LeadingZero := Copy('00', 1, 2-Length(s))+s;
  end;

Constructor TClockView.Init(var Bounds: TRect);
  var
    s, hund: word;
  begin
    TView.Init(Bounds);
    FillChar(LastTime, SizeOf(LastTime), #$FF);
    TimeStr := '';
    Refresh := 1;
    OldXCoord := -1;
    OldShowSeconds := ShowSeconds;
    EventMask := evMouse or evMessage;
    Options := Options or ofTopSelect;
    GrowMode := gfGrowHiX;
    GetTime(LastHour, LastMin, s, hund);
    LastSemi := hund < 50;
    RegisterToBackground(@Self);
    if ShowSeconds then
      UpdTicks := 18
    else
      UpdTicks := 9;
    NewTimerSecs(Utimer, 1200+Random(1200));
  end { TClockView.Init };

procedure TClockView.Draw;
  var
    B: TDrawBuffer;
    C: byte;
  begin
    Size.Y := 1;
    C := GetColor(1);
    MoveChar(B, ' ', C, Size.X);
    if MacroRecord then
      MoveStr(B, '>MACRO<', C)
    else
      begin
        if (RightAlignClock = True) and
          (Origin.X+Size.X <> ScreenWidth)
        then
          MoveTo(ScreenWidth-Size.X, Origin.Y);
        if Length(TimeStr) <> Size.X then
          begin
            GrowTo(Length(TimeStr), 1);
            exit
          end;
        MoveStr(B, TimeStr, C);
      end;
    WriteLine(0, 0, Size.X, 1, B);
  end { TClockView.Draw };

procedure TClockView.HandleEvent;
  var
    P: TPoint;
    R: TRect;
  begin
    P := Size;
    Application^.GetBounds(R);
    if Event.What = evMouseDown then
      begin
        {$IFDEF Calendar}
        if Event.Double then
          begin
            InsertCalendar;
            exit
          end;
        {$ENDIF}
        OldXCoord := -1; {-SSK}
        TView.DragView(Event, dmDragMove, R, P, P);
      end;
  end;

procedure TClockView.UpDate;
  var
    H, M, s, hund: word;
    D, Mn, Y: word;
    SS: String[40];
    Event: TEvent;
    P: PView;
    DayWeek: byte;
    R: TRect;
    StdClockWidth: byte;
    Semi: boolean;
  begin
    if ShowSeconds then
      StdClockWidth := 10
    else
      StdClockWidth := 7;
    GetTime(H, M, s, hund);
    Semi := hund < 50;
    if (Abs(s-LastTime.Sec) >= Refresh) or (Semi <> LastSemi) then
      begin
        GetDate(Y, Mn, D, hund);
        if (ShiftState and 7 <> 0)
          {$IFDEF OS2}
          {AK155 В неактивном оконном состоянии не надо реагировать на
          Shift, Alt, Ctrl}
          and not DN_IsBGWindow
          {$ENDIF}
        then
          begin
            if ShiftState and 3 <> 0 then
              TimeStr := ' '+FStr(MemAvail)+' '
            else
              begin
                MakeDateFull(0, D, Mn, Y, 0, 0, SS, ShowCentury);
                if ShowCentury then
                  TimeStr := Copy(SS, 1, 10)+' '
                else
                  TimeStr := Copy(SS, 1, 8)+' ';
                if ShowDayOfWeek then
                  begin
                    DayWeek := hund; {DayOfWeek(Date)-1;}
                      {Cat: странные какие-то проблемы...}
                    if (Length(DaysOfWeek) <> 14) and (Length(
                        DaysOfWeek) <> 21)
                    then
                      TimeStr := ' '+Copy(GetString(stDaysWeek), 1+
                        DayWeek*2, 2)
                      +' '+TimeStr
                    else
                      TimeStr := ' '+Copy(DaysOfWeek, 1+DayWeek*
                      (Length(DaysOfWeek) div 7), (Length(DaysOfWeek)
                        div 7))
                      +' '+TimeStr
                  end
                else
                  TimeStr := ' '+TimeStr;
              end;
            if not RightAlignClock then
              begin
                if OldXCoord = -1 then
                  OldXCoord := Origin.X;
                if (Origin.X+(Size.X shr 1)) > (ScreenWidth shr 1)
                then
                  R.Assign(OldXCoord+StdClockWidth-Length(TimeStr),
                    Origin.Y,
                  OldXCoord+StdClockWidth, Origin.Y+Size.Y)
                else
                  R.Assign(OldXCoord, Origin.Y,
                  OldXCoord+Length(TimeStr), Origin.Y+Size.Y);
                TView.Locate(R);
              end;
            UpdTicks := 6;
          end
        else
          begin
            if not RightAlignClock then
              if not (OldXCoord = -1) then
                begin{-SSK}
                  R.Assign(OldXCoord, Origin.Y, OldXCoord+Length(
                    TimeStr), Origin.Y+Size.Y);
                  OldXCoord := -1;
                  TView.Locate(R);
                end; {-SSK}
            if ShowSeconds then
              UpdTicks := 18
            else
              UpdTicks := 9;
            with LastTime do
              begin
                Inc(LastMin);
                Hour := H;
                Min := M;
                Sec := s;
                Day := D;
                Month := Mn;
                Year := Y;
              end;
            LastSemi := Semi;
            TimeStr := FormatTimeStr(H, M, s);
            if not ShowSeconds then
              TimeStr := Copy(TimeStr, 1, 6)+' '; {-$VIV} {-SSK}
            if BlinkSeparator and not (Semi or ShowSeconds) then
                TimeStr[4] := ' ';
            if ShowSeconds <> OldShowSeconds then
              begin
                if Origin.X > (ScreenWidth shr 1) then
                  begin
                    R.B.X := Origin.X+Size.X;
                    R.A.X := R.B.X-StdClockWidth
                  end
                else
                  begin
                    R.A.X := Origin.X;
                    R.B.X := R.A.X+StdClockWidth
                  end;
                R.A.Y := Origin.Y;
                R.B.Y := R.A.Y+1;
                TView.Locate(R);
                OldShowSeconds := ShowSeconds
              end;
          end;
        DrawView;
      end
    else
      UpdTicks := 1
  end { TClockView.UpDate };

function TClockView.FormatTimeStr;
  var
    n: String[3];
    s: String[20];
  begin
    if (CountryInfo.TimeFmt = 0) and (H > 12) then
      begin
        s := ' '+LeadingZero(H-12)+CountryInfo.TimeSep+LeadingZero(M)+
          CountryInfo.TimeSep+LeadingZero(SS);
        n := 'pm ';
      end
    else
      begin
        s := ' '+LeadingZero(H)+CountryInfo.TimeSep+LeadingZero(M)+
          CountryInfo.TimeSep+LeadingZero(SS);
        if CountryInfo.TimeFmt = 0
        then
          if (H < 12) then
            n := 'am '
          else
            n := 'pm '
        else
          n := ' ';
      end;
    FormatTimeStr := s+n;
  end { TClockView.FormatTimeStr };

{$IFDEF TrashCan}
const
  CTrashCan: String[Length(CGrayWindow)] = CGrayWindow;

  { TTrashCan }

Constructor TTrashCan.Init;
  begin
    inherited Init(R);
    GrowMode := gfGrowAll;
    Options := Options or ofTopSelect;
    EventMask := EventMask or evBroadcast;
    Hide;
  end;

function TTrashCan.GetPalette: PPalette;
  begin
    GetPalette := @CTrashCan;
  end;

procedure TTrashCan.Draw;
  var
    B: TDrawBuffer;
    C: word;
  begin
    if State and sfDragging <> 0 then
      C := 3
    else if State and sfSelected = 0 then
      C := 1
    else
      C := 2;
    C := GetColor(C);
    MoveStr(B, #209#209#216#209#209, C);
    WriteLine(0, 0, Size.X, 1, B);
    MoveStr(B, GetString(dlTrashCaption), C);
    WriteLine(0, 1, Size.X, 1, B);
    MoveStr(B, #192#193#193#193#217, C);
    WriteLine(0, 2, Size.X, 1, B);
  end;

procedure TTrashCan.HandleEvent(var Event: TEvent);
  var
    Where: TPoint;
    SaveConfirm: boolean;
    extent: TRect;
    Msg: String;
    Cfms: word;

  begin
    inherited HandleEvent(Event);

    if (Event.What = evBroadcast) and (Event.Command = cmDropped)
    then
      begin
        Cfms := Confirms;
        if Confirms and cfMouseConfirm = 0 then
          Confirms := 0;
        Message(PCopyRec(Event.InfoPtr)^.Owner, evCommand,
          cmEraseGroup, PCopyRec(Event.InfoPtr)^.FC);
        Confirms := Cfms;
        {EraseFiles(PCopyRec(Event.InfoPtr)^.FC);}
        ClearEvent(Event);
      end;
    if Event.What = evMouseDown then
      if Event.Double then
        begin
          Event.What := evCommand;
          Event.Command := cmReanimator;
          Event.InfoPtr := nil;
          PutEvent(Event);
          ClearEvent(Event);
        end
      else
        begin
          Owner^.GetExtent(extent);
          DragView(Event, dmDragMove, extent, Size, Size);
        end;
  end { TTrashCan.HandleEvent };

procedure TTrashCan.SetState(AState: word; Enable: boolean);
  begin
    inherited SetState(AState, Enable);
    if AState and sfSelected <> 0 then
      EnableCommands([cmNext, cmPrev]);
    if (AState and (sfSelected+sfFocused+sfDragging) <> 0) then
      DrawView;
  end;
{$ENDIF}

{-DataCompBoy-}
{$IFDEF Printer}
procedure PrintFiles;
  var
    PF: PFileRec;
    i, j: integer;
    s: String;
  begin
    if Files = nil then
      exit;
    j := 0;
    for i := 0 to Files^.Count-1 do
      begin
        PF := Files^.At(i);
        if PF^.Attr and Directory = 0 then
          Inc(j);
      end;
    if j = 0 then
      exit;
    if Files^.Count = 1 then
      begin
        {$IFNDEF OS2}
        s := GetString(dlDIFile)+' '+Cut(GetLFN(PFileRec(Files^.At(0))^.
          Lfn), 40)
        {$ELSE}
        s := GetString(dlDIFile)+' '+Cut(PFileRec(Files^.At(0))^.
          Name, 40)
        {$ENDIF}
      end
    else
      s := ItoS(Files^.Count)+' '+GetString(dlDIFiles);
    if MessageBox(GetString(dlPM_Print)+s+'?', nil, mfYesNoConfirm) <>
        cmYes
    then
      exit;
    for i := 0 to Files^.Count-1 do
      begin
        PF := Files^.At(i);
        if PF^.Attr and Directory = 0 then
          begin
            {$IFNDEF OS2}
            s := MakeNormName(PF^.Owner^, GetLFN(PF^.Lfn));
            {$ELSE}
            s := MakeNormName(PF^.Owner^, PF^.Name);
            {$ENDIF}
            Message(Own, evCommand, cmCopyUnselect, PF);
            Message(Application, evCommand, cmFilePrint, @S);
          end;
      end;
  end { PrintFiles };
{$ENDIF}
{-DataCompBoy-}

end.
