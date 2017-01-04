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
//  dn370-clock(i)-disable_seconds_in_winnt.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Gauges;

{ Useful gauges: clock and heap available viewer }
interface

uses Dos, Objects, Views, Drivers, FileCopy, Eraser, Collect,
     Commands, xTime, LFNCol
{$IFDEF Calendar} ,calendar {$ENDIF}
     ;


type
{$IFDEF TrashCan}
  { Trash can object }
  PTrashCan = ^TTrashCan;
  TTrashCan = object(TView)
    ImVisible: boolean;
    constructor Init(var R: TRect);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
  end;

 Const
  TrashCan: PTrashCan = nil;
 Type
{$ENDIF}

  PKeyMacros = ^TKeyMacros;
  TKeyMacros = object(TObject)
   Limit: AInt;
   Count: AInt;
   Keys: PWordArray;
   constructor Init;
   constructor Load(var S: TStream);
   procedure PutKey(KeyCode: Word);
   procedure Store(var S: TStream);
   procedure Play;
   destructor Done; virtual;
  end;


const LastHour: Word = $FF;
      LastMin: Word = 0;

const
     KeyMacroses: PCollection = nil;
     MacroRecord: Boolean = False;

type
  PHeapView = ^THeapView;
  THeapView = object(TView)
    OldMem : LongInt;
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    procedure Update; virtual;
  end;

  PClockView = ^TClockView;
  TClockView = object(TView)
    Refresh: Byte;
    LastTime: DateTime;
    LastSemi: boolean;
    TimeStr: string[16];   {-SSK (old version: string[12])}
    OldXCoord: aint;       {-SSK}
    OldShowSeconds: boolean;
    Utimer: TEventTimer;
    constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function FormatTimeStr(H, M, SS: Word): String; virtual;
    procedure Update; virtual;
  end;

{$IFDEF PRINTMANAGER}
procedure PrintFiles(Files: PCollection; Own: PView);
{$ENDIF}

implementation
uses Memory, Tree, Messages, DNApp, advance, advance1, advance2, advance3,
     {$IFDEF VIRTUALPASCAL}SysUtils, {$ENDIF}
     FilesCol, RStrings, Startup, DnIni; {-$VIV}

constructor TKeyMacros.Init;
begin
 inherited Init;
 Limit := 10;
 Count := 0;
 Keys := MemAlloc(Limit * sizeOf(Word));
 if Keys = nil then Fail;
end;

destructor TKeyMacros.Done;
begin
 if Keys <> nil then FreeMem(Keys, Limit * sizeOf(Word));
 inherited Done;
end;

constructor TKeyMacros.Load;
begin
 S.Read(Limit, SizeOf(AWord) * 2);
 Keys := MemAlloc(SizeOf(AWord) * Limit);
 if Keys = nil then Fail;
 S.Read(Keys^, SizeOf(AWord) * Count);
end;

procedure TKeyMacros.Store;
begin
 S.Write(Limit, SizeOf(Limit) * 2);
 S.Write(Keys^, SizeOf(AWord) * Count);
end;

procedure TKeyMacros.PutKey;
 var P: Pointer;
begin
 if Count >= Limit then
    begin
       Inc(Limit, 10);
       P := MemAlloc(Limit * SizeOf(Word));
       if P = nil then Exit;
       Move(Keys^, P^, Count * SizeOf(Word));
       Keys := P;
    end;
 Keys^[Count] := KeyCode;
 Inc(Count);
end;

procedure TKeyMacros.Play;
 var I: Integer;
begin
 for I := 0 to Count - 1 do
  Message(Application, evKeyDown, Keys^[I], nil);
end;

{------ Heap Window object ----------}

constructor THeapView.Init(var Bounds: TRect);
begin
  TView.Init(Bounds);
  OldMem := 0;
end;

procedure THeapView.Draw;
var
  S: String;
  B: TDrawBuffer;
  C: Byte;
begin
  OldMem := MemAvail;
  Str(OldMem, S);
  C := GetColor(2);
  MoveChar(B, ' ', C, Size.X);
  MoveStr(B, S, C);
  WriteLine(0, 0, Size.X, 1, B);
end;


procedure THeapView.Update;
begin
  if (OldMem <> MemAvail) then DrawView;
end;

{-------- ClockView Object --------}

function LeadingZero(w: Word): String;
var s: String[20];
begin
  Str(w:0, s);
  LeadingZero := Copy('00', 1, 2 - Length(s)) + s;
end;

constructor TClockView.Init(var Bounds: TRect);
  var s, hund: word;
begin
  TView.Init(Bounds);
  FillChar(LastTime, SizeOf(LastTime), #$FF);
  TimeStr := '';
  Refresh := 1;
  OldXCoord:=-1;
  OldShowSeconds:=ShowSeconds;
  EventMask:= evMouse or evMessage ;
  Options:=Options or ofTopSelect;
  GrowMode := gfGrowHiX;
  GetTime(LastHour, LastMin, s, hund);
  LastSemi:=hund<50;
  RegisterToBackground(@Self);
  if ShowSeconds then UpdTicks := 18 else UpdTicks := 9;
  NewTimerSecs(UTimer, 1200+Random(1200));
end;

procedure TClockView.Draw;
var
  B: TDrawBuffer;
  C: Byte;
begin
  Size.Y := 1;
  C := GetColor(1);
  MoveChar(B, ' ', C, Size.X);
  if MacroRecord then MoveStr(B, '>MACRO<', C)
                 else
                   begin
                     if (RightAlignClock = True) and
                        (Origin.X + Size.X <> ScreenWidth) then
                       MoveTo(ScreenWidth - Size.X, Origin.Y);
                     if Length(TimeStr) <> Size.X then
                       begin GrowTo(Length(TimeStr), 1); Exit end;
                     MoveStr(B, TimeStr, C);
                   end;
  WriteLine(0, 0, Size.X, 1, B);
end;

procedure TClockView.HandleEvent;
var P : TPoint;
    R : TRect;
begin
 P:=Size;Application^.GetBounds(R);
 If Event.What=evMouseDown then begin
{$IFDEF Calendar}
   If Event.Double then begin InsertCalendar; exit end;
{$ENDIF}
   OldXCoord:=-1;  {-SSK}
   TView.DragView(Event,dmDragMove,R,P,P);
 end;
end;

procedure TClockView.Update;
var
  h,m,s,hund: word;
  d,mn,y : word ;
  SS: String[40];
  Event: TEvent;
  P : Pview ;
  DayWeek: byte;
  R: TRect;
  StdClockWidth: byte;
  Semi: boolean;
begin
  if ShowSeconds then StdClockWidth:=10 else StdClockWidth:=7;
  GetTime(h,m,s,hund);
  Semi:=hund<50;
  {if (Abs(s - LastTime.sec) >= Refresh) or (Semi<>LastSemi) then} {-JITR-}
  if (Abs(LongInt(s) - Longint(LastTime.sec)) >= Refresh) or       {-JITR-}
      (Semi<>LastSemi) then
  {There were some overflows for the expression s - LastTime.sec when
  LastTime.sec was greater than s; this was fixed with a typecast to the
  signed type - Longint. -JITR-}
  begin
    GetDate(y,mn,d,hund);
    if ShiftState and 7 <> 0 then
      begin
        if ShiftState and 3 <> 0 then TimeStr := ' '+FStr(MemAvail)+' '
          else begin
              MakeDateFull(0, d, mn, y, 0, 0, SS, ShowCentury);
              if ShowCentury then TimeStr:=Copy(SS,1,10)+' '
              else TimeStr:=Copy(SS,1,8)+' ';
              if ShowDayOfWeek then begin
                  {$IFNDEF VIRTUALPASCAL}
                  asm {-SSK}
                      mov ah, 02Ah
                      int 21h
                      mov DayWeek, al
                  end;
                  {$ELSE}
                  DayWeek := DayOfWeek(Date)-1;
                  {$ENDIF}
                  if (Length(DaysOfWeek)<>14) and (Length(DaysOfWeek)<>21)
                  then TimeStr:=' '+Copy(GetString(stDaysWeek),1+DayWeek*2,2)
                  +' '+TimeStr else TimeStr:=' '+Copy(DaysOfWeek,1+DayWeek*
                  (Length(DaysOfWeek) div 7),(Length(DaysOfWeek) div 7))
                  +' '+TimeStr
               end else TimeStr:=' '+TimeStr;
           end;
           if not RightAlignClock then begin
            if OldXCoord=-1 then OldXCoord:=Origin.X;
            if (Origin.X+(Size.X shr 1))>(ScreenWidth shr 1) then
             R.Assign(OldXCoord+StdClockWidth-Length(TimeStr), Origin.Y,
                      OldXCoord+StdClockWidth,                 Origin.Y+Size.Y)
            else R.Assign(OldXCoord,                 Origin.Y,
                          OldXCoord+Length(TimeStr), Origin.Y+Size.Y);
            TView.Locate(R);
           end;
           UpdTicks := 6;
      end else
      begin
        if not RightAlignClock then
         if not (OldXCoord=-1) then begin {-SSK}
          R.Assign(OldXCoord,Origin.Y, OldXCoord+Length(TimeStr),Origin.Y+Size.Y);
          OldXCoord:=-1;
          TView.Locate(R);
        end; {-SSK}
        if ShowSeconds then UpdTicks := 18 else UpdTicks := 9;
        with LastTime do
        begin
          inc(LastMin);
          Hour := h;
          Min := m;
          Sec := s;
          Day := d ;
          Month := mn ;
          Year := y ;
        end;
        LastSemi:=Semi;
        TimeStr := FormatTimeStr(h, m, s);
        if not ShowSeconds then TimeStr := Copy(TimeStr, 1, 6) + ' '; {-$VIV} {-SSK}
        if BlinkSeparator and not (Semi or ShowSeconds) then TimeStr[4]:=' ';
        if ShowSeconds<>OldShowSeconds then begin
            if Origin.X>(ScreenWidth shr 1) then begin
                R.B.X:=Origin.X+Size.X;
                R.A.X:=R.B.X-StdClockWidth
            end else begin
                R.A.X:=Origin.X;
                R.B.X:=R.A.X+StdClockWidth
            end;
            R.A.Y:=Origin.Y; R.B.Y:=R.A.Y+1;
            TView.Locate(R);
            OldShowSeconds:=ShowSeconds
        end;
      end;
    DrawView;
  end else UpdTicks:=1
end;

function TClockView.FormatTimeStr;
  var N: String[3];
      S: String[20];
begin
  if (CountryInfo.TimeFmt = 0) and (H > 12) then
   begin
      S := ' ' + LeadingZero(h-12) + CountryInfo.TimeSep + LeadingZero(m) + CountryInfo.TimeSep + LeadingZero(ss);
      N := 'pm ';
    end
  else
    begin
      S := ' ' + LeadingZero(h) + CountryInfo.TimeSep + LeadingZero(m) + CountryInfo.TimeSep + LeadingZero(ss);
      if CountryInfo.TimeFmt = 0
         then if (H < 12) then N := 'am ' else N := 'pm '
         else N := ' ';
    end;
  FormatTimeStr := S + N;
end;

{$IFDEF TrashCan}
const
  CTrashCan : string[Length(CGrayWindow)] = CGrayWindow;

{ TTrashCan }

constructor TTrashCan.Init;
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
  C: Word;
begin
  if State and sfDragging <> 0 then C := 3
  else if State and sfSelected = 0 then C := 1
  else C := 2;
  C := GetColor(C);
  if PoorFrames then
    MoveStr(B, #205#205#205#205#205, C)
  else
    MoveStr(B, #209#209#216#209#209, C);
  WriteLine(0, 0, Size.X, 1, B);
  MoveStr(B, GetString(dlTrashCaption), C);
  WriteLine(0, 1, Size.X, 1, B);
  MoveStr(B, #192#193#193#193#217, C);
  WriteLine(0, 2, Size.X, 1, B);
end;

procedure TTrashCan.HandleEvent(var Event:TEvent);
var
  Where: TPoint;
  SaveConfirm: Boolean;
  Extent: TRect;
  Msg: String;
  Cfms: Word;


begin
  inherited HandleEvent(Event);

  if (Event.What = evBroadcast) and (Event.Command = cmDropped) then
  begin
   Cfms := Confirms;
   if Confirms and cfMouseConfirm = 0 then Confirms := 0;
   Message(PCopyRec(Event.InfoPtr)^.Owner, evCommand, cmEraseGroup, PCopyRec(Event.InfoPtr)^.FC);
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
    end else
    begin
      Owner^.GetExtent(Extent);
      DragView(Event, dmDragMove, Extent, Size, Size);
    end;
end;

procedure TTrashCan.SetState(AState: Word; Enable: Boolean);
begin
  inherited SetState(AState, Enable);
  if AState and sfSelected <> 0 then
    EnableCommands([cmNext, cmPrev]);
  if (AState and (sfSelected + sfFocused + sfDragging) <> 0) then
    DrawView;
end;
{$ENDIF}

        {-DataCompBoy-}
{$IFDEF PRINTMANAGER}
procedure PrintFiles;
  var PF: PFileRec;
      I,J: Integer;
      S: String;
begin
  if Files = nil then Exit;
  J := 0;
  for I := 0 to Files^.Count-1 do
    begin
      PF := Files^.At(I);
      if PF^.Attr and Directory = 0 then Inc(J);
    end;
  if J = 0 then Exit;
  if Files^.Count = 1 then
    begin
      S := GetString(dlDIFile) + ' ' + Cut(GetLFN(PFileRec(Files^.At(0))^.LFN), 40)
    end else S := ItoS(Files^.Count)+' '+GetString(dlDIFiles);
  if MessageBox(GetString(dlPM_Print)+S+'?', nil, mfYesNoConfirm) <> cmYes then Exit;
  for I := 0 to Files^.Count - 1 do
    begin
       PF := Files^.At(I);
       if PF^.Attr and Directory = 0 then
         begin
           S := MakeNormName(PF^.Owner^, GetLFN(PF^.LFN));
           Message(Own, evCommand, cmCopyUnselect, PF);
           Message(Application, evCommand, cmFilePrint, @S);
         end;
    end;
end;
{$ENDIF}
        {-DataCompBoy-}

end.
