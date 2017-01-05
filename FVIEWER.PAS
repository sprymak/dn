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

unit FViewer;

interface

uses
  Objects, Views, Dos, Drivers, HideView, Commands, DNHelp,
  {$IFDEF VIRTUALPASCAL} {SysUtils,} {$ENDIF} {AK155}
  advance,
  advance1, advance2, advance3, highlite, U_KeyMap, microed, xTime,
  collect, macro, Advance6;

const
  MaxWrapW = 1023; { максимальная ширина для заворачивания
    строк в режиме unwrap }

type

  { TViewScroll }

  PViewScroll = ^TViewScroll;
  TViewScroll = object(TView)
   MaxV, Value: LongInt;
   function GetPalette: PPalette; virtual;
   procedure HandleEvent(var Event: TEvent); virtual;
   function GetPartCode: LongInt;
   procedure Draw; virtual;
   function GetSize: Integer;
   procedure DrawPos(Pos: Integer);
  end;

  { TFileViewer }

  PFileViewer = ^TFileViewer;
  TFileViewer = object(THideView)
    Filtr: Boolean;
    NoEdit: Boolean;
    FileName: String; {DataCompBoy}
    VFileName: String; {DataCompBoy}
    Buf: PByteArray;
    Fl: PStream;
    UpdateViewTmr: TEventTimer;
    XDelta, ViewMode, HexPos: AInt;
    SearchActive: Boolean;
    SearchResultVisible: Boolean; {AK155}
    SearchX: LongInt;
    SB: PView;
    Wrap: Byte; {DataCompBoy}
    Lines: Array[0..200] of record Pos: LongInt; Len: word; end;
    FilePos, FileSize, NumLines: LongInt;
    ExposedPos, ExposedLine: longint; {AK155}
    Cur: TPoint;
    Info: PView;
    BufPos: longint;
    BufSize, MaxLines: LongInt;
    BufLines: AInt;
    KillAfterUse, IsValid, QuickView, Loaded, HexEdit, BufModified: Boolean;
{$IFDEF VIRTUALPASCAL}
    FakeKillAfterUse: Boolean; {временная пустышка}
{$ENDIF}
    Filter: Byte;
    XLAT: TXLAT;
    UseXLAT: Boolean;
    XLatFile: PString;
    KeyMap: TKeyMap;
    MarkPos: TPosArray;
    CtrlK: Boolean;
    CtrlQ: Boolean;
    HiLite : Boolean;           {PZ 2000.06.09}
    ScrollEOF : Boolean;  {AK155}
    HiLitePar: THighliteParams;
    constructor Init(var Bounds: TRect; AStream: PStream;
                     const AFileName, AVFileName: String;
                     ASB: PView; Quick, Hex: Boolean); {DataCompBoy}
    constructor Load(var S: TStream);
    procedure   Store(var S: TStream);
    destructor  Done; virtual;
    procedure   ShowView; virtual;
    procedure   HideView; virtual;
    procedure   Draw; virtual;
    procedure   SetXlatFile(const aFName: String; DoRedraw: Boolean);
    function    ReadFile(const FName, VFName: String; NewStream: Boolean): Boolean; {DataCompBoy}
    procedure   SetState(AState: Word; Enable: Boolean); virtual;
    procedure   HandleEvent(var Event: TEvent); virtual;
    function    WriteModify: Boolean;
    procedure   CountDown(ANumber: Integer); virtual;
    procedure   CountUp(ANumber: Integer); virtual;
    procedure   Seek(APos: LongInt);
    procedure   MakeLines; virtual;
    procedure   SaveToFile(FN: String);
    function    Valid(Command: Word): Boolean; virtual;
    procedure   ChangeBounds(var Bounds: TRect); virtual;
    function    GetPalette: PPalette; virtual;
    procedure   DoHighlite ( var B; const S : String; const Attr : string );
    procedure   Update; virtual; {AK155}
    procedure   SeekEOF;
    procedure   SeekBOF;
  private
    procedure   AdjustBuf;
  end;

  PHFileViewer = ^THFileViewer;
  THFileViewer = object(TFileViewer)
   procedure ChangeBounds(var Bounds: TRect); virtual;
   function GetPalette: PPalette; virtual;
  end;

  PNFileViewer = ^TNFileViewer;
  TNFileViewer = object(TFileViewer)
   function GetPalette: PPalette; virtual;
  end;

  PViewInfo = ^TViewInfo;
  TViewInfo = object(TView)
    Viewer: PFileViewer;
    constructor Init(var R: TRect; AViewer: PFileViewer);
    constructor Load(var S: TStream);
    procedure   Store(var S: TStream);
    procedure   Draw; virtual;
    procedure   HandleEvent(var Event: TEvent); virtual;
  end;

  PFileWindow = ^TFileWindow;
  TFileWindow = object({TStd}TWindow)
    constructor Init(const FileName, VFileName: String; Hex: Boolean); {DataCompBoy}
    function    GetPalette: PPalette; virtual;
    function    ReactOnCmd: Boolean; virtual;
    procedure   ChangeBounds(var Bounds: TRect); virtual;
  end;

const
  CHViewer = #13#14#117#118#199#200#201#202#203#204;
  CViewer  = #6#7#8#9#10#11#12#13;
  CViewWindow = #112#113#114#115#116#117#118#199#200#201#202#203#204;

type
  TViewSearch = record What: String[250]; Opts: Word; Dir: Word end;

const
  cmChangeValue = 9990;
  SearchString: TViewSearch = (What:''; Opts: 0; Dir: 0);

var
  LastViewerBounds: TRect;

const
  LastViewerDeskSize: TPoint = (X:0;Y:0);
  LastEditDeskSize: TPoint = (X:0;Y:0);

function SearchFileStr(F: PStream; var Xlat: TXLAT;
                       const What: String; Pos: LongInt;
                       CaseSensitive, Display, WholeWords, Back,
                       AllCP, IsRegExp: Boolean): LongInt;

implementation

uses LFN, {DataCompBoy}
     Memory, Messages, DNApp, Startup, Dialogs, RStrings,
     {$IFDEF DisAsm} Decoder, {$ENDIF} {piwamoto}
     {$IFDEF RegExp} RegExp, {$ENDIF} {Cat}
     Gauge, DNStdDlg, Histries, Drives, DnUtil, DNIni;

function MaxAvail: LongInt;
begin
  MaxAvail := MemAdjust(System.MaxAvail);
end;

const
  ViewerBufSize = $8000;

function SearchFileStr;
 label 1, LExit;
 var Buf: PByteArray;
     Count: LongInt;
     BufLen, BufPos, BPos, T, StPos, StDelta: LongInt;
     I, L, J, OldPos, NextPos: LongInt;
     CancelSearch: Boolean;
     Info: PWhileView;
     R: Trect;
     Tmr: TEventTimer;
     Inserted: Boolean;
     W: String;
     {$IFDEF REGEXP}
     RegExp: PRegExp;
     {$ENDIF}
begin
 if IsRegExp then
   begin
     MessageBox('RegExp Search is not implemented yet', nil, mfError+mfOkButton);
     Exit;
   end;
 Info := nil;
 SearchFileStr := -1; Count := 0; ClrIO;
 OldPos := F^.GetPos; L := F^.GetSize; if L = 0 then goto LExit;
 F^.Seek (Pos);
 {$IFNDEF VIRTUALPASCAL}
 BufLen := $8000;
  {AK155: было $7000; более круглая длина уменьшает ненужные пересылки }
 {$ELSE}
 BufLen := $80000;
  {Cat: тут лучше с памятью не мелочиться, иначе сильно проигрываем в скорости}
 {$ENDIF}
 if BufLen > L then  BufLen := L;
 if BufLen > MaxAvail then BufLen := MaxAvail;
 if BufLen < Length(What) then begin F^.Seek(OldPos); goto LExit; end;
 T := 0;
 StPos := 0;
 StDelta := 0;
 if Display then
  begin
   R.Assign(1,1,30,9);
   New(Info, Init(R));
   Info^.Top := GetString(dlSearching) + '"' + Cut(SearchString.What, 40) + '"';
   NewTimer(Tmr, 2); Inserted := False;
  end;
 GetMem(Buf, BufLen); I := Pos; BPos := 0; CancelSearch := Off;
 repeat
   if Back then
     begin
       NextPos := I - BufLen + BPos;
       if NextPos < 0 then begin NextPos := 0; BPos := 0; end;
       J := I - NextPos;
       if J <= 0 then Break;
     end
   else
     begin
       NextPos := F^.GetPos;
       J := L - I + BPos; if J > BufLen then J := BufLen; ClrIO;
     end;
   if Display then
    begin
     Info^.Write(1, StrGrd(L, I, 30, False));
     Info^.Write(2, ZtoS(Percent(L, I))+'%');
     if (not Inserted) and TimerExpired(Tmr) then
     begin
       Desktop^.Insert(Info);
       Inserted := True;
     end;
     if Inserted then DispatchEvents(Info, CancelSearch);
    end;
   F^.Seek(NextPos);
   F^.Read(Buf^[BPos], J-BPos); T := J-BPos;
 1:
   if J <= 0 then BufPos := 0 else
   begin {-$VIV 14.05.99}
     if AllCP then
     begin
       if Back then BufPos := BackSearchForAllCP(What, Buf^[StPos], J-StPos, CaseSensitive)
               else BufPos := SearchForAllCP(What, Buf^[StPos], J-StPos, CaseSensitive);
     end else
     begin
       W:=What;
       XlatBuf(W[1], Length(W), XLat);
       if Back then BufPos := BackSearchFor(W, Buf^[StPos], J-StPos, CaseSensitive)
               else BufPos := SearchFor(W, Buf^[StPos], J-StPos, CaseSensitive);
     end;
   end; {-$VIV ::}
   if BufPos > 0 then
    begin
     if WholeWords and not (((BufPos = 1) or (Char(Buf^[BufPos-2+StPos]) in BreakChars)) and
         (Char(Buf^[BufPos+Length(What)-1+StPos]) in BreakChars)) then
           begin
             if Back then Dec(J, StPos-BufPos)
                     else Inc(StPos, BufPos);
             Goto 1;
           end;
     FreeMem(Buf, BufLen); F^.Seek(OldPos);
     SearchFileStr := NextPos - LongInt(BPos) + LongInt(BufPos) - 1 + StPos;
     goto LExit;
    end;
   if Back then
     begin
       Dec(I,T);
       if I < -T then Break;
       if I < 0 then I := 0;
     end else Inc(I,T);
   StPos := 1;
   BPos := Length(What)+1;
   if I < L then Move(Buf^[J-BPos], Buf^[0], BPos);
 until (Back and (I < 0)) or
       (not Back and ((I >= L) or (T <= 0) or (F^.GetPos >= F^.GetSize))) or
       CancelSearch;
 if CancelSearch then SearchFileStr := -2;
 FreeMem(Buf, BufLen); F^.Seek(OldPos);
LExit: if Info<>nil then Dispose(Info,Done); Info:=nil;
end;

constructor TViewInfo.Init(var R: TRect; AViewer: PFileViewer);
begin
 inherited Init(R);
 Viewer := AViewer;
 GrowMode := gfGrowHiY + gfGrowLoY + gfGrowHiX;
 EventMask := evMouse;
end;

constructor TViewInfo.Load(var S: TStream);
begin
 inherited Load(S);
 GetPeerViewPtr(S, Viewer);
end;

procedure TViewInfo.Store(var S: TStream);
begin
 inherited Store(S);
 PutPeerViewPtr(S, Viewer);
end;

procedure TViewInfo.Draw;
 var B: TDrawBuffer;
     S: String;
     I,J: Integer;
     Ch, Ch2: Char;
     Color: Word;
begin
 if (Viewer = nil) or
    (Owner  = nil) or
    (PWindow(Owner)^.Frame = nil)
    then begin inherited Draw; Exit; end;
 With Viewer^, Self do
  begin
    Color:=PWindow(Owner)^.Frame^.GetColor(3); Ch2:=#205;
    if not Owner^.GetState(sfActive) then begin
      Color:=PWindow(Owner)^.Frame^.GetColor(1);
      MoveChar(B, #196, Color, Size.X);
      WriteLine(0, 0, Size.X, 1, B);
      Exit;
    end else
    if Owner^.GetState(sfDragging) then begin
      Color:=PWindow(Owner)^.Frame^.GetColor(5);
      Ch2:=#196;
    end;
    if ViewMode <> vmHex then
      case Wrap of
       wmNone:  S:=Ch2+'<=>';
       wmWidth: S:=Ch2+'>=<';
       wmWords: S:=Ch2+'>W<';
      end
    else begin
      S:=Hex8(Cur.X div (1+Byte(HexEdit))+Cur.Y*HexPos+BufPos+FilePos)+' ';
      J:=(FilePos+BufPos) mod max(1,HexPos);
      for I:=0 to HexPos - 1 do S:=S+Hex2(I+J)+#196;
      SetLength(S, Length(S)-1);
      Insert('[', S, 1);
      Insert(']', S, Length(S)+1);
    end;
    if ViewMode <> vmHex then
    begin
      S:=S + Ch2+Ch2+ '<';
      for I := 1 to 9 do
        if not MarkPos[I].EqualsXY(-1,-1) then
          S:=S + Char(I+48) else
          S:=S + #250;
      S:=S + '>';
    end;
    Insert(Ch2+Ch2, S, Length(S)+1);
    if UseXlat then begin
      Insert(AddSpace(Cut(XLatFile^,8),8), S, Length(S)+1);
    end else
      S := S + KeyMapDescriptor[KeyMap] + '..   ';
    case Filter of
      0:   Insert('{00-255}'+Ch2, S, Length(S)+1);
      1:   Insert('{32-127}'+Ch2, S, Length(S)+1);
      else Insert('{32-255}'+Ch2, S, Length(S)+1);
    end;
    if ViewMode <> vmHex then
    begin
      S:=S+Ch2;
      if (Lines[Size.Y].Pos < 0) and (FilePos+BufSize >= FileSize)
        or (FileSize = 0) then
        S:=S + '100' else
        S:=S + Long2Str(Round(Percent(FileSize, FilePos+ExposedPos)),3);
      S:=S + '% of ' + FStr(FileSize) + ' Bytes'+Ch2;
    end;
  end;
 MoveChar(B, Ch2, Color, Size.X);
 MoveStr(B, S, Color);
 Self.WriteLine(0, 0, Size.X, Size.Y, B);
end;

procedure TViewInfo.HandleEvent;
  var P: TPoint;
      BookMark: Byte;
      Ev: TEvent;
begin
  if Viewer=nil then Exit;
  if (Event.What = evMouseDown) then with Viewer^ do
  begin
    Self.MakeLocal(Event.Where, P);
    if (ViewMode <> vmHex) then
    begin
      if P.X <  1 then {} else
      if P.X <  4 then Message(Viewer, evCommand, cmUnwrap, nil) else
      if P.X <  6 then {} else
      if P.X =  6 then SeekBOF else
      if P.X < 16 then begin
        BookMark:=P.X-7;
        if (Event.Buttons and mbRightButton <> 0) or
          (MarkPos[BookMark+1].Y<>-1) then
        with Ev do
        begin
          What:=evCommand;
          if (Event.Buttons and mbRightButton <> 0) then
            Command:=cmPlaceMarker1+BookMark else
            Command:=cmGotoMarker1+BookMark;
          PutEvent(Ev);
        end;
      end else
      if P.X = 16 then SeekEOF else
      if P.X < 19 then {} else
      if P.X < 28 then Message(Viewer, evCommand, cmSwitchKeyMapping, nil)
      else
      if P.X < 36 then Message(Viewer, evCommand, cmAddFilter, nil);
    end else
    begin
      if P.X < 3*HexPos+9+3 then {} else
      if P.X < 3*HexPos+9+12 then
        Message(Viewer, evCommand, cmSwitchKeyMapping, nil)
      else
      if P.X < 3*HexPos+9+20 then
        Message(Viewer, evCommand, cmAddFilter, nil);
    end;
    ClearEvent(Event);
  end;
end;


function TFileViewer.WriteModify;
 var B: Boolean;
     I: LongInt;
     S: TDOSStream;
     A: Word;
begin
 B := BufModified; WriteModify := Off;
 if (Buf = nil) or (Fl = nil) or (Fl^.Status <> stOK) or not B then Exit;
 I := MessageBox(GetString(dlViewQuery), nil, mfWarning+mfYesNoCancel);
 WriteModify := I = cmCancel;
 if I <> cmCancel then BufModified := Off;
 if I = cmNo then begin Seek(FilePos); DrawView; end;
 if I <> cmYes then Exit;
 if VFileName='' then begin
  FreeStr := GetFileNameDialog(x_x, GetString(dlSaveFileAs),
                               GetString(dlSaveFileAsName),
                               fdOKButton + fdHelpButton, hsEditSave);
  if FreeStr<>'' then begin
   SaveToFile(FreeStr);
   FileName:=FreeStr;
   VFileName:=FileName;
   WriteModify:=true;
   Owner^.Redraw;
  end else exit;
 end;
 if (TypeOf(Fl^)=TypeOf(TDOSStream)) or (TypeOf(Fl^)=TypeOf(TBufStream)) then
  begin
    Dispose(Fl,Done);
    ClrIO; NeedAbort:=True; A:=0;
    Fl := New(PDosStream, Init(FileName, stOpen));
    if not Abort and (Fl^.Status <> stOk) then begin
     Dispose(Fl,Done);
     A:=GetFileAttr(FileName);
     SetFileAttr(FileName, A and not ReadOnly);
     Fl := New(PDosStream, Init(FileName, stOpen));
    end;
    if (Fl^.Status <> stOk) or (Abort) then begin
     Dispose(Fl,Done); Fl := New(PDosStream, Init(FileName, stOpenRead));
     MessageBox(GetString(dlFBBNoWrite)+FileName, nil, mfError + mfOKButton);
    end else begin
     Fl^.Seek(FilePos); Fl^.Write(Buf^, BufSize);
    end;
    if A<>0 then begin
     Dispose(Fl,Done);
     SetFileAttr(FileName, A);
     Fl := New(PDosStream, Init(FileName, stOpenRead));
    end;
  end else begin
   Fl^.Seek(FilePos); Fl^.Write(Buf^, BufSize);
  end;
  NeedAbort:=false;
  RereadDirectory(GetPath(FileName));
end;

const SCViewer: String[Length(CViewer)] = CViewer;
      SCHViewer: String[Length(CHViewer)] = CHViewer;
      SCNViewer: String[Length(CInputLine)] = CInputLine;
      SCViewWindow: String[Length(CViewWindow)] = CViewWindow;


function TFileViewer.GetPalette;
begin
 GetPalette := @SCViewer;
end;

function TNFileViewer.GetPalette;
begin
 GetPalette := @SCNViewer;
end;

function THFileViewer.GetPalette;
begin
 GetPalette := @SCHViewer;
end;

function TFileWindow.GetPalette;
begin
 GetPalette := @SCViewWindow;
end;

{TViewScroll}

function TViewScroll.GetSize: Integer;
var
  S: Integer;
begin
  if Size.X = 1 then S := Size.Y else S := Size.X;

  if S < 3 then GetSize := 3 else GetSize := S;
end;

procedure TViewScroll.DrawPos(Pos: Integer);
var
  S: Integer;
  B: TDrawBuffer;
Const  Chars :  TScrollChars = (#30, #31, #177, #254, #178);
begin
  S := GetSize - 1;
  MoveChar(B[0], Chars[0], GetColor(2), 1);
 { if Max = Min then
    MoveChar(B[1], Chars[4], GetColor(1), S - 1)
  else    }
  begin
    MoveChar(B[1], Chars[2], GetColor(1), S - 1);
    MoveChar(B[Pos], Chars[3], GetColor(3), 1);
  end;
  MoveChar(B[S], Chars[1], GetColor(2), 1);
  WriteBuf(0, 0, Size.X, Size.Y, B);
end;


procedure TViewScroll.HandleEvent;
 var B : Boolean;
     P: TPoint;
     RD,SP: LongInt;

     Extent: TRect;
     Tracking : Boolean;

begin
 TView.HandleEvent(Event);
 case Event.What of
  evCommand: case Event.Command of
              cmChangeValue: begin Value := Event.InfoLong;
                                   DrawView; ClearEvent(Event)
                             end;
{              cmSetMargins: Message(Owner, evCommand, cmGotoLineNumber, Pointer(Value));}
             end;
  evMouseDown: if MaxV>0 then begin

               MakeLocal(Event.Where, P);
               GetExtent(Extent);
               {if LongDiv(LongMul(Value,Size.Y-2),MaxV)+1 = P.Y then}
               if (Value * (Size.Y-3)) div MaxV + 1 = P.Y then
                begin
                RD := P.Y;
                repeat
                  MakeLocal(Event.Where, P);
                  Tracking := Extent.Contains(P);
                  if Tracking then
                  begin
                  {  if Size.X = 1 then} SP := P.Y {else I := P.X};
                    if SP <= 0 then SP := 1;
                    if SP >= Size.Y - 1 then SP := Size.Y - 2;
                  end;{ else I := GetPos;}
                  if SP <> RD then
                  begin
                    DrawPos(SP);
                    RD := SP;
                  end;
                until not MouseEvent(Event, evMouseMove+evMouseAuto);
                if SP = 1 then
                  Value := 0
                else
                  Value := MaxV * (SP-1) div (Size.Y-3) + 1;
{AK155: скроллер уже нарисован и еще дважды будет нарисован }
{                DrawView; }
                MessageL(Owner, evCommand, cmScrollBarChanged, Value);
               Exit;
               end;


                MakeLocal(Event.Where,P); RD := RepeatDelay; RepeatDelay := 0;
                SP := GetPartCode;
                if P.Y = 0 then
                 begin
                  repeat
                   MakeLocal(Event.Where,P);
                   if (P.X>=-1) and (P.X<=1) and (P.Y>=-1) and (P.Y<=1) then
                    Message(Owner, evKeyDown, kbUp, nil);
                  until not MouseEvent(Event, evMouseMove+evMouseAuto);
                 end else
                if P.Y = Size.Y-1 then
                 begin
                  repeat
                   MakeLocal(Event.Where,P);
                   if (P.X>=-1) and (P.X<=1) and (P.Y>=Size.Y-2) and (P.Y<=Size.Y) then
                    Message(Owner, evKeyDown, kbDown, nil);
                  until not MouseEvent(Event, evMouseMove+evMouseAuto);
                 end else
                if P.Y < SP then
                 begin
                  RepeatDelay := RD;
                  repeat
                    GetPartCode;
                    MakeLocal(Event.Where,P);
                    if (P.X>=-1) and (P.X<=1) and (P.Y<SP) then
                      Message(Owner, evKeyDown, kbPgUp, nil);
                  until not MouseEvent(Event, evMouseMove+evMouseAuto);
                 end else
                if P.Y > SP then
                 begin
                  RepeatDelay := RD;
                  repeat
                   GetPartCode;
                   MakeLocal(Event.Where, P);
                   if (P.X>=-1) and (P.X<=1) and (P.Y>SP) then
                    Message(Owner, evKeyDown, kbPgDn, nil);
                  until not MouseEvent(Event, evMouseMove+evMouseAuto);
                 end;
                RepeatDelay := RD;
               end;
 end;
end;

function TViewScroll.GetPartCode: LongInt;
begin
if Value >= MaxV then
  GetPartCode := Size.Y - 2
else
  GetPartCode := 1+((Size.Y-3)*Value) div MaxV;
end;

function TViewScroll.GetPalette;
 const S: String[Length(CScrollBar)] = CScrollBar;
begin
 GetPalette := @S;
end;

procedure TViewScroll.Draw;
 var B1: TDrawBuffer;
     B: Array[0..128] of record C: Char; B: Byte; end Absolute B1;
     C1, C2: Byte;
     i: integer;
begin
 C1 := GetColor(1); C2 := GetColor(2);
 MoveChar(B, #177, C1, Size.Y);
 i := GetPartCode;
 B1[i] := 254+LongInt(C2*256);
 B1[0] := 30+LongInt(C2*256);
 B1[Size.Y-1] := 31+LongInt(C2*256);
 WriteBuf(0,0,1,Size.Y,B);
end;


{ THFileViewer }

procedure THFileViewer.ChangeBounds;
 var R: TRect;
begin
 SetBounds(Bounds);
 if (Wrap>wmNone) or (ViewMode = vmDump)  then MakeLines;
 if SB <> nil then
  begin R :=Bounds; R.A.X := R.B.X; Inc(R.B.X); SB^.SetBounds(R) end
end;

{ TFileViewer }

procedure TFileViewer.ChangeBounds;
 var R: TRect;
begin
 Inherited ChangeBounds(Bounds);
 if (Wrap>wmNone) or (ViewMode = vmDump) then MakeLines;
 DrawView;
{ if Info <> nil then
  begin R := Bounds; R.A.Y := R.B.Y - 1; Dec(R.B.X, 2); Info^.SetBounds(R) end}
end;

{AK155
   Обеспечить буфер подходящего размера. Этот размер не
превосходит размера файла. Прежний буфер, если он был
не такого размера, освобождается }
procedure TFileViewer.AdjustBuf;
var
  NewBufSize: longint;
begin
  NewBufSize := ViewerBufSize;
  If NewBufSize > FileSize then NewBufSize := FileSize;
  if (Buf <> nil) and (NewBufSize = BufSize) then
    exit;
  if Buf <> nil then
    FreeMem(Buf, BufSize);
  If MaxAvail < NewBufSize then NewBufSize := MaxAvail;
  BufSize := NewBufSize;
  GetMem(Buf, BufSize);
end;

{AK155
   Автодочитывание файла, если на экране виден его конец }
procedure TFileViewer.Update;
  begin
  inherited Update;
  if ((EditorDefaults.ViOpt and vbfAutoscroll) <> 0) and
     not SearchResultVisible and
     (ViewMode = vmText) and
     (ExposedPos+FilePos = FileSize) and
     TimerExpired(UpdateViewTmr)
  then
    begin
    SeekEOF;
    NewTimer(UpdateViewTmr, 9); { обновлять 2 раза в секунду }
    end;
  end;

constructor TFileViewer.Init;
var
  C: Char;
begin
  inherited Init(Bounds); ViewMode := vmText;
  XlatFile := nil;
  NullXLAT(XLat); UseXLat:=Off; KeyMap:=kmNone;
  FillChar(MarkPos, SizeOf(MarkPos), $FF);
  HelpCtx := hcView;
  GrowMode := gfGrowHiX + gfGrowHiY;
  Options := Options or ofSelectable or (LongInt(Quick)*ofTopSelect);
  EventMask := $FFFF; SB := ASB; QuickView := Quick;
  Buf := nil; IsValid := True; Loaded := Off;
  KillAfterUse := False;
  Wrap:=0;
  if EditorDefaults.ViOpt and vbfWrap <> 0 then Wrap:=1;
  if EditorDefaults.ViOpt and vbfWordWrap <> 0 then Wrap:=2;
  ViewMode := {EditorDefaults.ViOpt and}Byte(Hex);
  Fl := AStream;
  if (AFileName = '') and (Fl = nil) then Exit;
  if Fl = nil
   then if Not ReadFile(AFileName, AVFileName, On) then IsValid:=off else
   else if (Fl <> nil) and (Fl^.Status = stOK)
         then ReadFile(' ', ' ', Off)
         else IsValid := Off;
  TempFile:='';
{AK155 Это для автодочитывания файла с изменяющейся длиной,
    см. Update }
  NewTimer(UpdateViewTmr, 9);
  RegisterToBackground(@Self);
{/AK155}
end;

destructor TFileViewer.Done;
 var Dir, Name, Ext: String; {JO}
begin
  EnableCommands([cmUnwrap]);
  if Buf <> nil then FreeMem(Buf, BufSize); Buf:=nil;
  if Fl <> nil then Dispose(Fl,Done); Fl:=nil;
  If not (TottalExit or Exiting) and KillAfterUse Then Begin
   lFSplit(FileName, Dir, Name, Ext);         {JO: проверочка файла на нахождение}
   if Dir = Tempdir then EraseFile(FileName); {во временном каталоге не помешает}
   KillAfterUse:=False;
   RereadDirectory(GetPath(FileName));
  End;
  Info:=nil;
  DeRegister(@Self); {AK155}
  inherited Done;
end;

constructor TFileViewer.Load;
 var I: LongInt;
begin
 inherited Load(S);
 NoEdit := Off;
 BufModified := Off;
 GetPeerViewPtr(S, SB);
 GetPeerViewPtr(S, Info);
 S.ReadStrV(FileName); {S.Read(FileName[0], 1); S.Read(FileName[1], Length(FileName));}
 S.ReadStrV(VFileName); {S.Read(VFileName[0], 1); S.Read(VFileName[1], Length(VFileName));}
 S.Read(I, Sizeof(LongInt));
 S.Read(QuickView, 1);
 S.Read(Wrap, 1);
{$IFDEF VIRTUALPASCAL} {???}
 S.Read(FakeKillAfterUse, 1);
 KillAfterUse := False;
{$ELSE}
 S.Read(KillAfterUse, 1);
{$ENDIF}
 S.Read(Filter, 1);
 S.Read(ViewMode, 2);
 XlatFile:=S.ReadStr;
 S.Read(XLAT, SizeOf(XLAT));
 S.Read(UseXLAT, SizeOf(UseXLAT));
 S.Read(KeyMap, SizeOf(KeyMap));
 S.Read(MarkPos, Sizeof(MarkPos));
 Fl := nil; Buf := nil; IsValid := True; Loaded := On;
 if (FileName = '') and (VFileName = '') then IsValid:=off
  else IsValid:=ReadFile(FileName, VFileName, On);
 Seek(I)
end;

procedure TFileViewer.Store;
 var I: LongInt;
begin
 inherited Store(S);
 PutPeerViewPtr(S, SB);
 PutPeerViewPtr(S, Info);
 S.WriteStr(@FileName); {S.Write(FileName[0], 1 + Length(FileName));}
 S.WriteStr(@VFileName); {S.Write(VFileName[0], 1 + Length(VFileName));}
 I := FilePos + LongInt(BufPos);
 S.Write(I, Sizeof(LongInt));
 S.Write(QuickView, 1);
 S.Write(Wrap, 1);
{$IFDEF VIRTUALPASCAL}
 S.Write(FakeKillAfterUse, 1);
{$ELSE}
 S.Write(KillAfterUse, 1);
{$ENDIF}
 S.Write(Filter, 1);
 S.Write(ViewMode, 2);
 S.WriteStr(XlatFile);
 S.Write(XLAT, SizeOf(XLAT));
 S.Write(UseXLAT, SizeOf(UseXLAT));
 S.Write(KeyMap, SizeOf(KeyMap));
 S.Write(MarkPos, Sizeof(MarkPos));
end;

procedure XDumpStr(var S: String; var B; Addr: LongInt; Count: Integer; Filter: Byte);
begin
  S := Hex8(Addr)+' ';
  {$IFNDEF BIT_32}
  asm
    les  si, B
    mov  cx, Count
    push ds
    lds  di, S
    mov  bx, 10
    mov  ah, Filter
    jcxz @@3
@@1:
    mov  al, es:[si]
    or   ah, ah
    jz   @@2
    cmp  al, 32
    jnc  @@4
@@5:
    mov  al, 250
    jmp  @@2
@@4:
    cmp  ah, 1
    jnz  @@2
    cmp  al, 128
    jnc  @@5
@@2:
    mov  ds:[di+bx], al
    inc  byte ptr ds:[di]
    inc  si
    inc  bx
    loop @@1
@@3:
    pop  ds
  end;
  {$ELSE BIT_32}
  asm
    push ESI
    push EDI
    push EBX
    push ECX
    mov  ESI, B
    mov  ECX, Count
    mov  EDI, S
    mov  EBX, 10
    mov  AH, Filter
    jcxz @@3
@@1:
    mov  AL, [ESI]
    or   AH, AH
    jz   @@2
    cmp  AL, 32
    jnc  @@4
@@5:
    mov  AL, 250
    jmp  @@2
@@4:
    cmp  AH, 1
    jnz  @@2
    cmp  AL, 128
    jnc  @@5
@@2:
    mov  [EDI+EBX], AL
    inc  byte ptr [EDI]
    inc  ESI
    inc  EBX
    loop @@1
@@3:
    pop  ECX
    pop  EBX
    pop  EDI
    pop  ESI
  end;
  {$ENDIF}
end;

{ размер поля табуляции }
function GetTabSize: word;
  var
    TS: word;
  begin
  GetTabSize := 1;
  if (EditorDefaults.EdOpt{$IFNDEF OS_DOS} shl 2 {$ENDIF}and ebfTRp) <> 0
  then
  begin
    TS:=StoI(EditorDefaults.TabSize);
    if TS=0 then GetTabSize := 8
    else if TS>100 then GetTabSize := 100
    else GetTabSize := TS;
  end
  end;


procedure TFileViewer.Draw;
var
  B: TDrawBuffer;
  C: Byte;
  I, K, M, Src: Integer;
  J: longint;
  L: LongInt;
  S: String;
  TS: word;
  C1: Byte;
  C2: char;
  HP : string[6];
  CC: Array[1..8] of Byte;
  SaveXDelta: word;
{$IFDEF DisAsm}
  D: XCHGData; {GRM!}
{$ENDIF}
  SearchLineNum: integer;
    {номер строки на экране с найденным тестом или -1 }
  SearchTextStart: word;
    {X-координата в окне начала этого текста }
  ExpandTabs: boolean;
    { Табуляции заменять на пробелы }
var
  W, W2, W3: Integer;
  WDH: Integer;
begin
  ExposedPos := Lines[0].Pos;
  for I := 1 to 8 do CC[I] := GetColor(I);
  TS := GetTabSize;
  ExpandTabs := (EditorDefaults.EdOpt{$IFNDEF OS_DOS} shl 2 {$ENDIF}and ebfTRp) <> 0;
  if Buf = nil then
  begin
    ReadFile(FileName, VFileName, On);
    BufPos:=0;
    MakeLines;
  end else
  if (BufPos < 0) or (BufPos > BufSize + 20000) or
     (BufPos > 65000) then
  begin
    BufPos:=0;
    MakeLines;
  end;
  C:=CC[1];
{AK155  if Info <> nil then Info^.DrawView;}
  if {(FileName = '') or }(Buf = nil) then begin
    HideCursor; inherited Draw; Exit;
  end;
  SearchResultVisible := SearchActive;
  Case ViewMode of
   vmHex: begin
           DisableCommands([cmUnwrap]);
           HexPos:=(Size.X - 12) div 4; if HexPos <= 0 then HexPos:=1;
           XDelta:=0;
           Src:=0;
           if not ScrollEOF then {AK155}
           begin
             while (LongInt(Cur.Y * HexPos) + BufPos + FilePos >= FileSize) do
               Dec(Cur.Y);
             while (LongInt(Cur.Y * HexPos) + BufPos + FilePos + Cur.X
               div (Byte(HexEdit)+1) >= FileSize) do Dec(Cur.X);
           end;
           if Cur.Y < 0 then begin
            W:=-Cur.Y;
            Cur.Y:=0;
            CountUp(W);
           end
           else if Cur.Y >= Size.Y then begin
            W:=Cur.Y - Size.Y + 1;
            Dec(Cur.Y, W);
            CountDown(W);
           end;
           W:=BufPos;
           L:=FilePos + LongInt(BufPos);
           if HexEdit
            then SetCursor(10 + (Cur.X div 2) * 3 + Cur.X and 1, Cur.Y)
            else SetCursor(12 + HexPos * 3 + Cur.X, Cur.Y);
           ShowCursor;
           for I:=0 to Size.Y - 1 do begin
           {MoveChar(B[XDelta], ' ', C, Size.X);} {???}
            if Size.X+XDelta>255 then begin
             WDH:=255-XDelta;
             if WDH<0 then WDH:=0;
            end else WDH:=Size.X;
            MoveChar(B[XDelta], ' ', C, WDH);
            if L + LongInt(HexPos) > FileSize
             then J:=FileSize - L
             else J:=HexPos;
            if J > 0 then begin
             S:=DumpStr(Buf^[W], L, J, Filter);
             W2:=Pos(#179,S);
             XLatBuf(S[W2+1], Length(S)-W2, XLat);
             {-DataCompBoy & Axel: apply filter-}
             case Filter of
              0:   ;
              1:   for w3:=W2+1 to Length(S) do
                    if not (Byte(S[w3]) in [32..127]) then S[w3]:=#250;
              else for w3:=W2+1 to Length(S) do
                    if not (Byte(S[w3]) in [32..255]) then S[w3]:=#250;
             end;
             {-DataCompBoy & Axel: done-}
             if J = HexPos then Drivers.MoveStr(B, S, C)
             else begin
              Drivers.MoveStr(B, Copy(S, 1, J*3+10), C);
              Drivers.MoveStr(B[10+HexPos*3], Copy(S, J*3+11, MaxStringLength), C);
             end;
             if SearchActive then begin
              if (L <= SearchX) and (L+J > SearchX) then begin
               Cur.Y:=I; Cur.X:=(SearchX-L)*2;
               if HexEdit
                then SetCursor(10 + (Cur.X div 2) * 3 + Cur.X and 1, Cur.Y)
                else SetCursor(12 + HexPos * 3 + Cur.X, Cur.Y);
               ShowCursor;
               Src:=Min(L+J-SearchX, Length(SearchString.What));
               MoveColor(B[10+(SearchX-L)*3], Src*3-1, CC[2]);
               MoveColor(B[12+HexPos*3+SearchX-L], Src, CC[2]);
               Src:=Length(SearchString.What) - Src;
              end else
               if Src > 0 then begin
                K:=Min(Src, J);
                MoveColor(B[10], K*3-1, CC[2]);
                MoveColor(B[12+HexPos*3], K, CC[2]);
                Dec(Src, J);
               end;
             end else {+piwamoto}
              if I = Cur.Y then
              begin
               K:=Cur.X div (1 + Byte(HexEdit));
               MoveColor(B[10 + K * 3], 2, CC[2]);
               MoveColor(B[12 + HexPos * 3 + K], 1, CC[2]);
              end;     {-piwamoto}
              Inc(W, J);
              Inc(L, LongInt(J));
            end;
           {WriteLine(0, I, Size.X, 1, B[XDelta]);} {???}
            WriteLine(0, I, WDH, 1, B[XDelta]);
           end;
          end;
  vmDump: begin
           DisableCommands([cmUnwrap]);
           HideCursor;
           HexPos:=((Size.X - 9) div 16)*16; if HexPos < 16 then HexPos:=16;
           W:=BufPos; L:=FilePos + LongInt(BufPos); Src:=0;
           for I:=0 to Size.Y - 1 do begin
           {MoveChar(B[XDelta], ' ', C, Size.X);}
            if Size.X+XDelta>255 then begin
             WDH:=255-XDelta;
             if WDH<0 then WDH:=0;
            end else WDH:=Size.X;
            MoveChar(B[XDelta], ' ', C, WDH);
            if L + LongInt(HexPos) > FileSize
             then J:=FileSize - L
             else J:=HexPos;
            if J > 0 then begin
             XDumpStr(S, Buf^[W], L, J, Filter);
             XLatBuf(S[10], Length(S)-9, XLat);
             Drivers.MoveStr(B, S, C);
             if SearchActive then begin
              if (L <= SearchX) and (L+J > SearchX) then begin
               Src:=Min(L+J-SearchX, Length(SearchString.What));
               MoveColor(B[9+SearchX-L], Src, CC[2]);
               Src:=Length(SearchString.What) - Src;
              end else
               if Src > 0 then begin
                MoveColor(B[9], Min(Src, J), CC[2]);
                Dec(Src, J);
               end;
             end;
             Inc(W, J);
             Inc(L, LongInt(J));
            end;
            WriteLine(0, I, WDH, 1, B[XDelta]);
           end;
{$IFNDEF DisAsm}
          end
{$ELSE}
          end;
   {GRM!}
 vmAsm: begin
         DisableCommands([cmUnwrap]);
         XDelta := 0;
         L := FilePos + LongInt(BufPos);
         D.MemBuff:=Buf;
         D.Offset:=BufPos;
         XDelta := 0;

         SetCursor(Cur.X, Cur.Y);
         ShowCursor;
         for I := 0 to Size.Y - 1 do begin
          if Size.X+XDelta>255 then begin
           WDH:=255-XDelta;
           if WDH<0 then WDH:=0;
          end else WDH:=Size.X;
          MoveChar(B[XDelta], ' ', C, WDH);
          if L < FileSize - 1 then begin
           if Cur.Y=I then HexPos := L - FilePos - BufPos + ((Cur.X-10) div 2);
           D.InstrMaxLen := FileSize - L;
           {Draw - doesn't matter}
           ScanCode(D);
           S:=UpStrg(hex8(L))+': '+D.CodeStr+Strg(' ',29-length(D.CodeStr))+
                                   D.Command+Strg(' ',10-length(D.Command))+
                                   D.Operands;
           Drivers.MoveStr(B, S, C);
           Inc(L, D.InstrLen);
          end;
          WriteLine(0, I, WDH, 1, B[XDelta]);
         end;
        end
  {GRM!}
{$ENDIF}{DisAsm}
  else begin {vmText}
        ExposedLine := 0;
        EnableCommands([cmUnwrap]);
{ak155        HideCursor;}
{ возможный сдвиг XDelta, чтобы стал виден найденный текст,
и определение его X-позиции (с учетом табуляций) для будущей раскраски }
        SearchLineNum := -1;
        if SearchActive then
        begin
          for I:=0 to Size.Y - 1 do
          begin
            J:=SearchX - FilePos - Lines[I].Pos;
            if (J >= 0) and (J < Lines[I].Len) then
            begin
              if not ExpandTabs then
                W := J
              else
              begin
                W:=0;
                for M := Lines[I].Pos to Lines[I].Pos+J-1 do
                begin
                  if Buf^[M] = 9 then
                   inc(W, TS - (W mod TS))
                  else
                   inc(W);
                end;
              end;
              if (XDelta + Size.X  < W + Length(SearchString.What))
                or (XDelta > W )
              then
                XDelta:=W + (Length(SearchString.What) - Size.X) div 2;
              if XDelta < 0 then XDelta:=0;
              SearchLineNum := I;
              SearchTextStart := W-XDelta;
              break;
            end;
          end;
        end;

        for I:=0 to Size.Y - 1 do
        begin
         MoveChar(B[0], ' ', C, Size.X);
         s := '';
         if (Lines[I].Pos >=0) then
         begin
           ExposedLine := I;

           {-DataCompBoy: Expand tabs...-} {AK155: и применить фильтр }
           W:=0;
           for M := Lines[I].Pos to Lines[I].Pos + Lines[I].Len - 1 do
           begin
             C2 := char(Buf^[M]);
             If (C2=#9) and ExpandTabs then
               repeat
                 Inc(W);
                 if W > XDelta then
                   S[W-XDelta] := ' ';
               until W mod TS = 0
             else
               begin
                 Inc(W);
                 if W > XDelta then
                   S[W-XDelta] := C2;
               end;
             if W-XDelta > Size.X then
               break;
           end;
           if W > XDelta then
           begin
             SetLength(S, W-XDelta);
             XLatBuf(S[1], Length(S), XLat);
{Cat: фильтр перенесён сюда - он должен быть использован
      уже после применения таблицы перекодировки}
             case Filter of
               1:
                 for W := 1 to Length(S) do
                   if not (Byte(S[W]) in [32..127]) then S[W]:=#250;
               2:
                 for W := 1 to Length(S) do
                   if not (Byte(S[W]) in [32..255]) then S[W]:=#250;
             end {case};
{/Cat}
             MoveStr(B, S, C(*, Filter, TS*));
             if Hilite and not QuickView  then
             begin
               Highlites ( Length(S), @S[1], HiLitePar );
               C1                  := C and $F0;                 { Background }
               SetLength(HP, 6);                                 { Attr. size }
               HP[Ord(hhComment)]  := Chr(CC[3]);                { Comments   }
               HP[Ord(hhNumber)]   := Chr(C1 or (CC[6] and 15)); { Numbers    }
               HP[Ord(hhString)]   := Chr(C1 or (CC[5] and 15)); { Strings    }
               HP[Ord(hhSymbol)]   := Chr(C1 or (CC[4] and 15)); { Symbols    }
               HP[Ord(hhKeyword1)] := Chr(C1 or (CC[7] and 15)); { Keywords 1 }
               HP[Ord(hhKeyword2)] := Chr(C1 or (CC[8] and 15)); { Keywords 2 }
               DoHighlite ( B, S, HP );
             end;
           end;

         end;
         if I = SearchLineNum then { раскраска найденного текста }
           MoveColor(B[SearchTextStart], Length(SearchString.What), CC[2]);
         WriteLine(0, I, Size.X, 1, B[0]);
        end;
       end;
  if ExposedLine > MaxLines then
    ExposedPos := Lines[ExposedLine].Pos + Lines[ExposedLine].Len
  else if Lines[ExposedLine+1].Pos < 0 then
    ExposedPos := BufSize
  else
    ExposedPos := Lines[ExposedLine+1].Pos
  end;
  if Info <> nil then Info^.DrawView;
end;

function DetectKeyMap(const FName: String): TKeyMap;
begin
 DetectKeyMap := kmAscii;
end;


function  TFileViewer.ReadFile;
 var I, J: Integer;
     P: Pointer;
     Macros: PCollection;
begin
  ReadFile:=true;
  WriteModify;
  if Buf <> nil then FreeMem(Buf, BufSize); Buf := nil;
  BufModified := Off;
  IsValid := True;
  FileName := FName;
  VFileName := VFName;
  if KeyMap=kmNone then begin
    KeyMap:=DetectKeyMap(FName);
    if not UseXLat then SetXlatFile('', Off);
  end;
  if NewStream then
   begin
    if Fl <> nil then Dispose(Fl,Done); Fl:=nil;
    if FName = '' then Exit;
    Fl := New(PDosStream, Init(FName, stOpenRead));
   end;
  if (not ExistFile(FName) or isDir(FName)) and
     (FName <> '') and (FName <> ' ') then begin ReadFile:=false; Exit; end;
  Macros := nil;
  {PZ 2000.06.09 }
  if (EditorDefaults.ViOpt and vbfHlt) <> 0 then
    HiLite := Macro.InitHighLight ( VFName, HiLitePar, Macros, nil );
  {PZ end}
  ScrollEOF := (EditorDefaults.ViOpt and vbfScrollAfterEOF) <> 0; {AK155}
  if Fl^.Status <> stOK then begin IsValid := False; ReadFile:=false; Exit end;
  FileSize := FL^.GetSize; FilePos := 0; I := 1; NumLines := 1;
  if FileSize < 0 then FileSize := 0;
  AdjustBuf;
  if SB <> nil then PViewScroll(SB)^.MaxV := FileSize;
  Seek(0); XDelta := 0;
  If (UpStrg(VFileName) <> UpStrg(FileName)) then begin
   KillAfterUse:=true;
   Filtr:=true;
  end else Filtr:=off;
end;

procedure TFileViewer.SetState;
begin
  inherited SetState(AState, Enable);
  if (AState and (sfActive + sfSelected) <> 0) then begin
    if GetState(sfSelected) and (Owner^.GetState(sfActive)) then
    begin
      if SB <> nil then begin
        SB^.Show; SB^.EventMask:=$FFFF
      end;
      DrawView
    end else
    if SB <> nil then begin
      SB^.Hide; SB^.EventMask:=0;
    end;
  end;
  if (Info<>nil) then
    if (AState and sfDragging <> 0)
      or (AState and (sfSelected+sfActive) <> 0)
    then Info^.Draw;
end;

procedure TFileViewer.HideView;
 var E: TEvent;
begin
 if SB <> nil then begin SB^.Hide; SB^.EventMask := 0; end;
 Message(Owner, evCommand, cmDisableView, nil);
 inherited HideView;
end;

procedure TFileViewer.ShowView;
 var E: TEvent;
begin
 if SB <> nil then begin SB^.Show; SB^.EventMask := 0; end;
 Message(Owner, evCommand, cmEnableView, nil);
 inherited ShowView;
end;

procedure TFileViewer.Seek(APos: LongInt);
begin
 if Buf = nil then Exit;
 FileSize := Fl^.GetSize;
 WriteModify;
 if APos < 0 then APos := 0;
 FilePos := APos;
 if FilePos < 0 then FilePos := 0;
 if FilePos = 0 then
  begin
   AdjustBuf;
   if Buf = nil then Exit;
  end;
 if APos + BufSize >= FileSize then FilePos := FileSize - BufSize;
 Fl^.Seek(FilePos);
 if (FileSize-FilePos < BufSize) then
  begin
   FreeMem(Buf, BufSize); BufSize := FileSize-FilePos;
   if BufSize < 0 then BufSize := 0;
   Buf := MemAlloc(BufSize); if Buf = nil then Exit;
  end;
 BufPos := APos - FilePos; if BufPos < 0 then BufPos := 0;
 if BufPos >= BufSize then BufPos := BufSize - 1;
 Fl^.Read(Buf^, BufSize);
 MakeLines;
 MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
end;

procedure TFileViewer.MakeLines;
 var
   K: LongInt;
   LineLen: LongInt;
   LineStart: LongInt;
   TabsWith: LongInt; { Tab characters additional with }
   MaxX: longInt;
   LineSeparatorLen: longint;
   TabSize: Integer;
   CurChar, NextChar: byte;
   WrappedLine: boolean;
{$IFDEF DisAsm}
     X: XchgData; {GRM!}
{$ENDIF}
 label
   LineBegin;
begin
 if Buf = nil then Exit;
 LineStart := BufPos; LineLen := BufSize - LineStart;
 FillChar(Lines, SizeOf(Lines), $FF); {AK155}
 TabSize := GetTabSize;
 MaxLines := 200;
 case ViewMode of
  vmHex, vmDump:
    begin
      MaxX := 255;
      LineLen := BufPos;
      for K := 0 to MaxLines do
      begin Lines[K].Pos := LineLen; Lines[K].Len := HexPos; Inc(LineLen, HexPos); end;
{$IFNDEF DisAsm}
      end
{$ELSE}
    end;
  {GRM!}
  vmAsm:
    begin
      MaxX := 255;
      X.MemBuff:=Buf;
      X.Offset:=Bufpos;
      for K:=0 to MaxLines do
      begin
        Lines[K].Pos := X.Offset;
        X.InstrMaxLen := FileSize - X.Offset - FilePos - 1;
        ScanCode(X);
        Lines[K].Len := X.InstrLen;
        if X.InstrLen = 0 then Lines[K].Pos:=-1;
      end;
    end
  {GRM!}
{$ENDIF}
  else {vmText}
  begin
    if Wrap>wmNone then MaxX := Size.X else MaxX := MaxWrapW;
    WrappedLine := false;
    for K := 0 to MaxLines do
    begin {-DataCompBoy-} {AK155}
LineBegin:
      if LineStart>=BufSize then
      begin
        Lines[K].Pos := -1;
        continue;
      end;
      Lines[K].Pos := LineStart;
      {Search for e.nd of line}
      LineLen :=0;
      TabsWith := 0;
      LineSeparatorLen := 0;
      while (LineLen+TabsWith<MaxX) and (LineStart+LineLen<BufSize) do
      begin
        CurChar := Buf^[LineStart+LineLen];
        NextChar := 0;
        if (LineStart+LineLen+1 < BufSize) then
          NextChar := Buf^[LineStart+LineLen+1];
        if (CurChar in [$0D, $0A]) then
        begin
          LineSeparatorLen := 1;
          if (NextChar in [$0D,$0A]) and (CurChar <> NextChar) then
             LineSeparatorLen := 2;
          if WrappedLine and (LineLen=0) then
          begin {ignore line separator exact after wrapped line}
            inc(LineStart, LineSeparatorLen);
            WrappedLine := false;
            goto LineBegin;
          end;
          WrappedLine := false;
          break;
        end;
        if CurChar=9 then
          inc(TabsWith, TabSize-1 - ((LineLen+TabsWith) mod TabSize));
        inc(LineLen);
        WrappedLine := true;
      end;
      Lines[K].Len:=LineLen;

       {Decrease length of word-wrapped line}
      if (Wrap=wmWords) and (LineLen+TabsWith>=MaxX) then
      begin
        While (LineLen > 1) and
          not (Char(CurChar) in BreakChars) and
          not (Char(NextChar) in BreakChars)
        do
        begin
          dec(LineLen);
          NextChar := CurChar;
          CurChar := Buf^[LineStart+LineLen-1];
        end;
        If LineLen > 1 then
          begin
          Lines[K].Len:=LineLen;
          WrappedLine := true;
          end;
      end;

      Inc(LineStart, Lines[K].Len+LineSeparatorLen);
    end; {-DataCompBoy-}
  end;
 end;
end;

 procedure TFileViewer.SaveToFile(FN: String);
   var S: TDosStream;
       P: PView;
       W, L: Word;
       PS: Pointer;
       Xl: Boolean;
       Sz: LongInt;
   label 2;

 begin
   Xl := (MaxAvail > 4096) and (UseXlat or (KeyMap<>kmAscii)) and
         (MessageBox(GetString(dlViewSaveXlat), nil,
          mfYesButton+mfNoButton+mfConfirmation) = cmYes);
   S.Init(FN, stOpen);
   W := 0; {AK155: чтобы всегда была определена }
   if S.Status = stOK then
     begin
        PS := @FN;
        W := MessageBox(GetString(dlED_OverQuery)
                        , @PS, mfYesButton+mfCancelButton+mfAppendButton+mfWarning);
        if W = cmYes then
          begin {существующий файл}
{AK155 Это переоткрытие приводит под OS/2 к тяжелой
ошибке при попытке сохранить перекодированный текст в том
же самом файле. А нужно оно только для того, чтобы при перезаписи
длинного файла коротким получить новую (меньшую) длину. Но это
гораздо прямее достигается при помощи Truncate.
            S.Done;
            S.Init(FN, stCreate);
/AK155}
          end
        else if W = cmOK then
          S.Seek(S.GetSize)
        else
          begin S.Done; Exit end;
     end
    else
     begin {новый файл}
       S.Done;                {JO}
       S.Init(FN, stCreate);  {JO}
     end;

   if S.Status <> stOK then
     begin
       S.Done;
       MessageBox(GetString(dlFBBNoWrite)+Cut(FN,40), nil, mfError+mfOKButton);
       Exit
     end;
   P := _WriteMsg(^M^M^C+GetString(dlWritingFile));
   Fl^.Seek(0);
   if Xl then
     begin
       PS := MemAlloc(4096);
       if PS = nil then Goto 2;
       Sz := Fl^.GetSize;
       while Fl^.GetPos < Sz do
         begin
           if Sz - Fl^.GetPos < 4096 then L := Sz - Fl^.GetPos
                                     else L := 4096;
           Fl^.Read(PS^, L);
           XlatBuf(PS^, L, Xlat);
           S.Write(PS^, L);
         end;
       FreeMem(PS, 4096);
     end
       else
2:   S.CopyFrom(Fl^, Fl^.GetSize);
   P^.Free;
   S.Truncate; {AK155 на случай записи короткого файла поверх длинного}
   S.Done;
   FN := GetPath(FN); if (Length(FN) > 3) and (FN[Length(FN)] = '\') then SetLength(FN, Length(FN)-1);
   GlobalMessage(evCommand, cmRereadDir, @FN);
 end;


procedure TFileViewer.SeekBOF;
  begin
  SearchX := 0;
  Seek(0);
  if ViewMode = vmAsm
   then Cur.Assign(10, 0)
   else Cur.Assign( 0, 0);
  XDelta := 0;
  DrawView
  end;

procedure TFileViewer.SeekEOF;
  var i, j: Integer;
      OldFileSize: longint;
      OldStart, OldLen1: longint;
      Temp: LongInt;
begin
 if Lines[Cur.Y].Pos < 0 then Exit;
 if ViewMode = vmText then
   begin
   OldStart := FilePos+Lines[0].Pos;
   OldLen1 := Lines[0].Len;
   end
 else
   begin
   OldStart := FilePos+Lines[Cur.Y].Pos;
   OldLen1 := HexPos;
   end;
 I := (FilePos+BufPos) mod 16;
 Owner^.Lock;
{AK155}
 if TypeOf(Fl^)=TypeOf(TDOSStream) then
   begin
   with PDOSStream(Fl)^ do
     begin
     if (Status <> stOk) then
       exit;

     OldFileSize := FileSize;
     SysFileSeek(Handle, 0, 2, StreamSize);
     FileSize := StreamSize;
     if FileSize < OldFileSize then
       begin { Файл сжался }
       if Position > FileSize then
         Position := StreamSize;
       end;
     SysFileSeek(Handle, Position, 0, Temp);
     end;

   if (OldFileSize < FileSize) and (BufSize < FileSize) then
     begin
     AdjustBuf;
     if Buf = nil then
       exit;
     end;
   end;

 Seek(FileSize - BufSize);
 if (ViewMode = vmHex) or (ViewMode = vmDump) then
 begin
   BufPos := max(OldStart-FilePos, BufSize - Size.Y*HexPos);
   MakeLines;
 end
 else
 begin
   BufPos := BufSize;
   {AK155: Зачем качать на 3 строки туда-сюда, непонятно. Для глюков?
   CountUp(Size.Y+3);
   CountDown(3);}
   CountUp(Size.Y); {AK155}
   MakeLines;

   i := 0;
   while (i < MaxLines) and (FilePos+Lines[i].Pos < OldStart) do
     inc(i);
   if i <> 0 then
   begin
     if FilePos+Lines[i].Pos > OldStart then
     begin
       Lines[0].Pos := OldStart-FilePos; Lines[0].Len := OldLen1;
       j := 1;
     end
     else
       j := 0;
     Move(Lines[i], Lines[j], sizeof(Lines[0])*(MaxLines-i-j));
     Dec(MaxLines, i-j);
   end;
   BufPos := Lines[0].Pos;
 end;

 if (ViewMode = vmHex) and (FileSize <> 0) then
 begin
   i := FileSize - (FilePos+BufPos)-1;
   Cur.Y := i div HexPos;
   Cur.X := i mod HexPos * 2;
 end;
 DrawView;
 Owner^.UnLock;
end;


procedure TFileViewer.HandleEvent;

 var P: TPoint;
     LR: Integer;
     I: Integer;
     LLR,LFR: LongInt;
     F: lFile;
     Ch: Char;

 procedure CE; begin ClearEvent(Event) end;

 procedure CM_END;
  var I, MaxW: Integer;
    w,l: integer;
    TabSize: Integer;

 begin
  if Buf = nil then Exit;
  TabSize := GetTabSize;
  MaxW := 0;
  CE; for I := 0 to SIze.Y - 1 do
    begin
    w := 0;
    for l := Lines[i].Pos to Lines[i].Pos+Lines[I].Len-1 do
      begin
      inc(w);
      if Buf^[l] = 9 then
        inc(w, TabSize-1 - l mod TabSize);
      end;
    if MaxW<w then
      MaxW := w;
    end;
  Dec(MaxW, Size.X);

  if MaxW < 0 then XDelta := 0 else XDelta := MaxW;
  DrawView;
 end;

 procedure ContinueSearch(Reverse: Boolean);
  var I{, J, JJ}: Integer;
      BXL: TXLAT;
 begin
  if (SearchString.What = '') then Exit;
  if SearchX < 0 then SearchX := 0;
  if (SearchX < FilePos + Lines[0].Pos){ or
     (SearchX > FilePos + Lines[1].Pos+1)}{AK155}
     then SearchX := FilePos+BufPos;
  NullXLAT(BXL);
  for i:=255 downto 1 do
    BXL[XLat[Char(i)]]:=Char(i);
  SearchX := SearchFileStr(Fl, BXL,
                           SearchString.What, SearchX,
                           SearchString.Opts and 1 <> 0, On,
                           SearchString.Opts and 2 <> 0, (SearchString.Dir = 1) xor Reverse,
                           SearchString.Opts and 4 <> 0,
                           SearchString.Opts and 8 <> 0);
  if SearchX >= 0 then
   begin
    {if HexPos > 0 then JJ := (FilePos+BufPos) mod HexPos;}
    Seek(SearchX);
    CountUp(3);
    SearchActive := On;
    BufPos := Lines[0].Pos; MakeLines;
    DrawView;
    SearchActive := Off; Inc(SearchX);
   end else if SearchX <> -2 then
    MessageBox(GetString(dlDBViewSearchNot), nil, mfError + mfOKButton);
 end;

 procedure StartSearch;
  var D: PDialog;
      SR: TViewSearch;
      R: TRect;
      P: PView;
      PP: PInputLine;
      I: Integer;

 begin
  if LowMemory then Exit;

  D := PDialog( LoadResource( dlgViewerFind ));
  D^.SetData(SearchString);
  I := Desktop^.ExecView(D);
  D^.GetData(SR);
  Dispose(D,Done);
  if I = cmCancel then Exit;
  SearchString := SR;
{AK155}
  SearchX := FilePos + Lines[0].Pos;
  if SR.Dir = 1 then
  begin
    i := Size.Y-1;
    while (i<>0) and (Lines[i].Pos < 0) do
      dec(i);
    SearchX := FilePos + Lines[i].Pos + Lines[i].Len;
  end;
{/AK155}
  ContinueSearch(Off);
 end;

 procedure LoadXlatTable;  {JO}
   var FN: String;
       More: Boolean;
       None: Boolean;
   label SkipMenu;
 begin
   CE;
   More := True;
   None := UseXlat;
   if SkipXLatMenu then goto SkipMenu;
   FN := CnvString(XlatFile);
   if FN <> '' then FN := FN + '.xlt';
   FN := GetFileNameMenu(SourceDir+'XLT\','*.XLT', FN, On, More, None);
SkipMenu:
   if None then begin
    KeyMap:=kmAscii;
    SetXlatFile('', On);
    UseXlat := Off;
    {DrawView;}
    Exit;
   end;
   if More then FN := GetFileNameDialog(SourceDir+'XLT\*.XLT',
                                        GetString(dlSelectXLT),
                                        GetString(dlOpenFileName),
                                        fdOKButton + fdHelpButton,
                                        hsOpenXLT);
   if FN <> '' then SetXlatFile(FN, On);
 end; {JO}

 procedure NextXLat;
 begin
   KeyMap := RollKeyMap[KeyMap];
   SetXlatFile('', On);
 end;

 label 1,2,DoSave, KBCheck, NotKb;

var
  ScrollerPos: longint;
begin
 inherited HandleEvent(Event);
 case ViewMode of
  vmText,
  vmHex: begin
          HexPos := (Size.X - 12) div 4;
          if HexPos <= 0 then HexPos := 1;
         end;
  vmDump:begin
          HexPos := ((Size.X - 9) div 16)*16;
          if HexPos < 16 then HexPos := 16;
         end;
 end;
 if Loaded then begin
   Loaded := Off;
   if SB<>nil then begin
    PViewScroll(SB)^.MaxV := FileSize;
    PViewScroll(SB)^.Value:= FilePos+BufPos;
   end;
   PViewScroll(SB)^.DrawView;
  end;
 case Event.What of
  evBroadcast: case Event.Command of
                 cmFindView:
                   if (PString(Event.InfoPtr)^=FileName) or
                      (PString(Event.InfoPtr)^=VFileName) then
                     if Owner<>nil then begin
                       Owner^.Select;
                       ClearEvent(Event);
                     end;
                 cmReanimator:
DoSave:
                               if (Fl <> nil) and (Fl^.Status = stOK) then
                                 begin
                                   CE;
                                   FreeStr := GetFileNameDialog(x_x, GetString(dlSaveFileAs),
                                                                GetString(dlSaveFileAsName),
                                                                fdOKButton + fdHelpButton, hsEditSave);
                                   if FreeStr <> '' then SaveToFile(FreeStr);
                                 end;
                 cmReleaseFile: begin
                                  if UpStrg(FileName) = UpStrg(PString(Event.InfoPtr)^) then
                                      begin
                                        ReadFile('', '', On);
                                        {if not QuickView then Message(Owner, evCommand, cmClose, nil);}
                                      end;
                                end;
               end;
  evCommand: case Event.Command of
              cmFileEdit: if DelSpaces(FileName)<>'' then
                begin
                  if Filtr and
                     (MessageBox(GetString(dlViewFilter), nil,
                         mfConfirmation+mfYesButton+mfNoButton) <> cmYes)
                   then begin CE; exit end;
                  WriteModify;
                 {if Desktop<>nil then Desktop^.Lock;}
                  if Application<>nil then begin
                    if FileName<>VFileName then TempFile:='*^&'+FileName;
                    PDNApplication(Application)^.EditFile(True, FileName);
                    MessageL(Application, evCommand, cmGotoLineNumber2,
                       GetLineNumberForOffset(FileName,FilePos+BufPos)
                      );
                  end;
                 {if Desktop<>nil then Desktop^.UnLock;}
                  CE;
                  if (Owner<>nil) and (TypeOf(Owner^)=TypeOf(TFileWindow))
                  then Message(Owner, evCommand, cmClose, nil);
                end;
              cmPlaceMarker1..cmPlaceMarker9:
                begin
                  I:=Event.Command-cmPlaceMarker1+1;
                  if (I>=1) and (I<=9) then begin
                    if (MarkPos[I].Y=-1) or
                      (MarkPos[I].Y<>FilePos+BufPos) or
                      (MarkPos[I].X<>Byte(HexEdit)*256*256
                        +Cur.Y*256+Cur.X)
                    then
                      MarkPos[I].Assign(Byte(HexEdit)*256*256
                        +Cur.Y*256+Cur.X,FilePos+BufPos) else
                      MarkPos[I].Assign(-1,-1);
                    DrawView;
                  end;
                  CE;
                end;
              cmGotoMarker1..cmGotoMarker9:
                begin
                  I:=Event.Command-cmGotoMarker1+1;
                  if (I>=1) and (I<=9) and (MarkPos[I].Y<>-1) then
                  begin
                    Cur.X:=MarkPos[I].X mod 256;
                    Cur.Y:=(MarkPos[I].X div 256) mod 256;
                    HexEdit:=(MarkPos[I].X div (256*256)) = 0;
                    Seek(MarkPos[I].Y); MakeLines;
                    DrawView;
                  end;
                  CE;
                end;
              cmGotoCell: begin
                           CE;
                           FreeStr := '';
                           if ViewMode = vmText then begin
                            if ExecResource(dlgGotoLine, FreeStr) <> cmOk then Exit;
                            Val(FreeStr, LLR, I);
                            if I <> 0 then exit;
                            LLR := GetOffsetForLineNumber(FileName, LLR);
                            if LLR < 0 then exit;
                            Seek(LLR);
                            MakeLines;
                            DrawView;
                           end else begin
                            if ExecResource(dlgGotoAddress, FreeStr) <> cmOK then Exit;
                            Val('$'+FreeStr, LLR, I);
                            if I <> 0 then exit;
                            LFR := LLR;
                            Cur.Assign(0,0);
                            while ((LFR mod HexPos) <> ((BufPos+FilePos) mod HexPos)) and (LFR > 0) do
                             begin Dec(LFR); Inc(Cur.X,2); end;
                            Seek(LFR); MakeLines;
                            DrawView;
                           end;
                          end;
              cmLoadXlatTable: LoadXlatTable;
              cmAddFilter: begin Filter := (Filter+1) mod 3; DrawView; CE end;
              cmSave: begin WriteModify; CE end;
              cmSaveAll: Goto DoSave;
              cmGetName: if VFileName <> ''
                          then PString(Event.InfoPtr)^ := GetString(dlViewFile)+' - '+{$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(VFileName)
                          else PString(Event.InfoPtr)^ := GetString(dlViewFile);
              cmLoadViewFile: if QuickView and
                                 (UpStrg(lFExpand(PString(Event.InfoPtr)^))
                                   <>
                                  UpStrg(lFExpand(FileName))
                                 ) then
                               begin ReadFile(PString(Event.InfoPtr)^, PString(Event.InfoPtr)^, On);
                                     DrawView; CE end;
              cmClose, cmQuit, cmKillUsed:
                if WriteModify then CE else begin
                 if FL<>nil then Dispose(Fl,Done);
                 Fl:=nil;
                end;
              cmSearchFor: begin StartSearch; CE end;
              cmReverseSearch,
              cmContinueSearch: begin ContinueSearch(Event.Command = cmReverseSearch); CE end;
              cmHexMode:  begin
                           case ViewMode of
                             vmText: begin ViewMode := vmHex; HexEdit := On; end;
                             vmHex : begin ViewMode := vmDump; end;
{$IFDEF DisAsm}
                             vmDump: if ((EditorDefaults.ViOpt and vbfDisAsm) <> 0) then
                                 begin ViewMode := vmAsm; HexEdit := On; MakeLines; Cur.X:=10; end {GRM!}
                                   else begin ViewMode := vmText; MakeLines end;
{$ENDIF}
                              else begin ViewMode := vmText; MakeLines end;
                           end;
                           if ViewMode <> vmDump then
                             begin
                               HexPos := (Size.X - 12) div 4;
                               if HexPos <= 0 then HexPos := 1;
                             end else
                             begin
                               HexPos := ((Size.X - 9) div 16)*16;
                               if HexPos < 16 then HexPos := 16;
                               MakeLines
                             end;
                           DrawView; CE
                          end;
              cmUnWrap: begin
                         Inc(Wrap);
                         if Wrap>wmWords then Wrap:=wmNone;
                         MakeLines;
                         if Lines[Size.Y-1].Pos < 0 then
                           SeekEOF;
                         DrawView;
                         CE;
                        end;
              cmScrollBarChanged:
                begin
                Desktop^.Lock;
                Seek(Event.InfoLong);
                if (ViewMode = vmText) and (Event.InfoLong <> 0) then
                begin { стать на начало строки, но так,
                 чтобы не сдвинуть вверх ползунок курсора }
                  CountDown(1);
                  ScrollerPos := PViewScroll(SB)^.GetPartCode;
                  CountUp(1);
                  if ScrollerPos <> PViewScroll(SB)^.GetPartCode then
                    CountDown(1);
                end;
                if Lines[Size.Y-1].Pos < 0 then
                  SeekEOF
                else
                  DrawView;
                Desktop^.UnLock;
                end;
              cmSwitchKeyMapping: NextXLat;
             end;

  evKeyDown: case Event.KeyCode of
               kbCtrlHome: if ViewMode = vmHex then begin
                            Cur.Y := 0;
                            DrawView; CE;
                           end else Goto 1;
               kbCtrlEnd: if ViewMode = vmHex then begin
                           Cur.Y := Size.Y - 1;
                           DrawView; CE;
                          end else Goto 2;
               kbESC{, kbEnter}: if not QuickView then begin CE; Message(Application, evCommand, cmClose, nil) end;
               kbCtrlLeft:
                  if ViewMode = vmText then
                     begin
                       CE;
                       if XDelta>19 then Dec(XDelta,20) else XDelta:=0;
                       DrawView
                     end
                  else (*if ViewMode <> vmAsm then {GRM!}*)
                     begin
                       if BufPos > 0 then
                         Dec(BufPos)
                       else if FilePos > 100 then
                       begin
                         Owner^.Lock;
                         Seek(FilePos - 100);
                         BufPos := 99;
                         Owner^.UnLock;
                       end;
                       DrawView; CE
                     end;

               kbCtrlRight:
                   if ViewMode = vmText then
                   begin CE;
                     if XDelta<MaxWrapW*8-Size.X-19 then Inc(XDelta,20);
                     DrawView
                   end
                   else (*if ViewMode <> vmAsm then {GRM!}*)
                   begin
                     if BufPos < BufSize - 1024 then
                       Inc(BufPos)
                     else
                     begin
                       Owner^.Lock;
                       Seek(FilePos + BufPos + 1);
                       Owner^.UnLock;
                     end;
                     DrawView; CE
                   end;
               kbTab: if ViewMode = vmHex then begin
                       if HexEdit then Cur.X := Cur.X div 2
                                  else Cur.X := Cur.X * 2;
                       HexEdit := not HexEdit;
                       DrawView; CE
                      end;
               kbCtrlPgUp: begin
1:
                              CE; SeekBOF;
                           end;
               kbCtrlPgDn: begin
2:                           Lines[Size.Y].Pos := 0; {piwamoto}
                             SeekEOF;
                             SearchX :=  FileSize;
                             CE
                           end;
               kbHome: begin
                         CE; if ViewMode = vmHex then Cur.X := 0
                                                 else XDelta := 0;
                         DrawView
                       end;
               kbEnd: if ViewMode <> vmHex then CM_END
                        else begin
                               Cur.X := HexPos*(Byte(HexEdit)+1)-1;
                               DrawView
                             end;
               kbDown: case ViewMode of {GRM!}
                        vmText, vmDump:
                                begin
                                 CE;
                                 CountDown(1); DrawView;
                                end;
                         vmHex: begin
                                 Inc(Cur.Y);
                                 DrawView;
                                 CE;
                                end;
{$IFDEF DisAsm}
                        vmAsm:
                          if Cur.Y = Size.Y-1 then
                           begin
                            CountDown(1); DrawView; CE;
                           end
                           else if Lines[Cur.Y+1].pos<>-1 then
                          begin
                             Inc(Cur.Y);
                             Cur.X:=10;
                             DrawView; CE;
                          end;

{$ENDIF}
                       end;{GRM!}
               kbPgDn: begin CE; CountDown(Size.Y-1); DrawView; end;
               kbSpace: if ViewMode in [vmText, vmDump]
                         then begin CE; CountDown(Size.Y-1); DrawView;end
                         else Goto KBCheck;
               kbUp:   case ViewMode of {GRM!}
                        vmText,
                        vmDump: begin
                                 CE;
                                 CountUp(1); DrawView;
                                end;
                         vmHex:  begin
                                 Dec(Cur.Y);
                                 DrawView;
                                 CE;
                                end;
{$IFDEF DisAsm}
                        vmAsm:
                          if Cur.Y = 0 then
                           begin
                            CountUp(1); DrawView; CE;
                           end
                          else
                           begin
                            Cur.X:=10; Dec(Cur.Y);
                            DrawView; CE;
                           end;
{$ENDIF}
                        end; {GRM!}
               kbPgUp: begin
                          CE;
                          CountUp(Size.Y-1); DrawView;
                          if BufPos=0 then
                           begin PViewScroll(SB)^.Value :=0; SB^.DrawView;end;
                       end;
               kbLeft: if ViewMode = vmText then begin CE; if XDelta>0 then Dec(XDelta); DrawView end
                        else
{$IFDEF DisAsm}
                          {GRM!}
                          if ViewMode=vmAsm then
                            begin
                              CE;
                              if Cur.X > 10 then Dec(Cur.X) else
                                begin
                                  Message(@Self, evKeyDown, kbUp, nil);
                                  if (BufPos+FilePos<>0) then Cur.X:=Lines[Cur.Y].Len*2+9;
                                end;
                              DrawView;
                            end
                          else
                          {GRM!}
{$ENDIF}
                             begin
                              if Cur.X > 0 then Dec(Cur.X)
                                 else begin Cur.X := HexPos*(Byte(HexEdit)+1) - 1; Dec(Cur.Y) end;
                         {     if ViewMode = vmHex then
                                 Message(Owner, evCommand, cmSetMargins, nil);}
                              DrawView; CE
                             end;
               kbRight: if ViewMode = vmText then begin CE; if XDelta<MaxWrapW*8-Size.X then Inc(XDelta); DrawView end
                        else
{$IFDEF DisAsm}
                          {GRM!}
                          if ViewMode=vmAsm then
                            begin
                              if Cur.X >= Lines[Cur.Y].Len*2+9 then Message(@Self, evKeyDown, kbDown, nil)
                                else inc(Cur.X);
                              DrawView;
                              CE
                            end
                          else
                          {GRM!}
{$ENDIF}
                             begin
                              if Cur.X < HexPos*(Byte(HexEdit)+1)-1 then Inc(Cur.X)
                                         else begin Cur.X := 0; Inc(Cur.Y) end;
                       {       if ViewMode = vmHex then SearchX := HexPos;}
                              DrawView; CE
                             end;
  {JO}         kbEnter:
                  case UseEnterInViewer of
                      1: if not QuickView then begin CE; Message(Application, evCommand, cmClose, nil) end;
                      2: Message(Owner, evCommand, cmHexMode, nil);
                        end
               else
KBCheck:       if Event.What<>evNothing then begin
                if (Event.CharCode = #11) and not CtrlK then begin
                  CtrlK:=True; CE;
                end else
                if CtrlK then begin
                 Ch:=Event.CharCode;
                 if (Ch >= '1') and (Ch <= '9') then begin
                  Event.What:=evCommand;
                  Event.Command:=cmPlaceMarker1+Ord(Ch)-Ord('1');
                  PutEvent(Event);
                 end;
                 CtrlK:=False; CE;
                end else begin
                {$IFDEF OS2}                    {JO: поскольку хоткеи Ctr-цифра в OS/2 не работают}
                 if ShiftState and 7 <> 0 then  {    по непонятной причине, введены временные хоткеи}
                 case Event.KeyCode of          {    Ctrl-Alt-Shift-цифра для перехода к закладкам}
                  kbAlt1: Event.Command := cmGotoMarker1;
                  kbAlt2: Event.Command := cmGotoMarker2;
                  kbAlt3: Event.Command := cmGotoMarker3;
                  kbAlt4: Event.Command := cmGotoMarker4;
                  kbAlt5: Event.Command := cmGotoMarker5;
                  kbAlt6: Event.Command := cmGotoMarker6;
                  kbAlt7: Event.Command := cmGotoMarker7;
                  kbAlt8: Event.Command := cmGotoMarker8;
                  kbAlt9: Event.Command := cmGotoMarker9;
                  else goto NotKb;
                 end
                 else
                {$ENDIF}
                 case Event.KeyCode of
                  kbAlt1: Event.Command:=cmPlaceMarker1;
                  kbAlt2: Event.Command:=cmPlaceMarker2;
                  kbAlt3: Event.Command:=cmPlaceMarker3;
                  kbAlt4: Event.Command:=cmPlaceMarker4;
                  kbAlt5: Event.Command:=cmPlaceMarker5;
                  kbAlt6: Event.Command:=cmPlaceMarker6;
                  kbAlt7: Event.Command:=cmPlaceMarker7;
                  kbAlt8: Event.Command:=cmPlaceMarker8;
                  kbAlt9: Event.Command:=cmPlaceMarker9;
                {$IFNDEF OS2}
                  kbCtrl1:Event.Command:=cmGotoMarker1;
                  kbCtrl2:Event.Command:=cmGotoMarker2;
                  kbCtrl3:Event.Command:=cmGotoMarker3;
                  kbCtrl4:Event.Command:=cmGotoMarker4;
                  kbCtrl5:Event.Command:=cmGotoMarker5;
                  kbCtrl6:Event.Command:=cmGotoMarker6;
                  kbCtrl7:Event.Command:=cmGotoMarker7;
                  kbCtrl8:Event.Command:=cmGotoMarker8;
                  kbCtrl9:Event.Command:=cmGotoMarker9;
                 {$ENDIF}
                  else goto NotKb;
                 end;
                 Event.What:=evCommand;
                 PutEvent(Event);
                 CE;
                 Exit;
                end;
NotKb:          if (Event.CharCode = #17) and not CtrlQ then begin
                 CtrlQ:=True; CE;
                end else
                if CtrlQ then begin
                 Ch:=Event.CharCode;
                 if (Ch >= '1') and (Ch <= '9') then begin
                  Event.What:=evCommand;
                  Event.Command:=cmGotoMarker1+Ord(Ch)-Ord('1');
                  PutEvent(Event);
                 end;
                 CtrlQ:=False; CE;
                end else
                if (Event.CharCode>=#32) and not NoEdit then begin
                 {GRM!}
                 if ((ViewMode = vmHex) and (Buf <> nil) and
                     (Cur.X div (Byte(HexEdit)+1)+Cur.Y*HexPos+BufPos < BufSize))
{$IFDEF DisAsm}
                     or (ViewMode = vmAsm)
{$ENDIF}
                     then begin
                 {GRM!}
                  if Filtr then begin
                   case MessageBox(GetString(dlViewFilter), nil,
                        mfConfirmation+mfYesButton+mfNoButton) of
                    cmYes: begin
                            Filtr:=false;
                            VFileName:='';
                            DisposeStr(PWindow(Owner)^.Title);
                            PWindow(Owner)^.Title:=NewStr(GetString(dlViewFile));
                            PWindow(Owner)^.ReDraw;
                           end;
                    else   begin CE; exit end;
                   end;
                  end;
                  if HexEdit then begin
                   Event.CharCode:=UpCase(Event.CharCode);
                   if Event.CharCode in ['A'..'F', '0'..'9'] then begin
                    BufModified:=On;
                    I:=PosChar(Event.CharCode, '0123456789ABCDEF')-1;
                    P.X:=Cur.X div 2+Cur.Y*HexPos+BufPos;
                    if Odd(Cur.X) then Buf^[P.X]:=(Buf^[P.X] and $F0) or I
                                  else Buf^[P.X]:=(Buf^[P.X] and $F) or (I shl 4);
                    Message(@Self, evKeyDown, kbRight, nil);
                    CE
                   end;
                  end else begin
                   BufModified:=On; I:=0;
                   while I < 256 do begin
                    if XLat[Char(I)] = Char(Event.CharCode) then begin
                     Event.CharCode:=Char(I);
                     break;
                    end;
                    Inc(I);
                   end;
                   Char(Buf^[Cur.X+Cur.Y*HexPos+BufPos]):=Event.CharCode;
                   Message(@Self, evKeyDown, kbRight, nil);
                   CE
                  end;
                 end;
                end;
               end;
             end;
  evMouseDown: begin
                LR := RepeatDelay;
                LLR := AutoRepeat;
                repeat
                 MakeLocal(Event.Where, P);
                 I := Abs((Size.Y shr 1)-P.Y);
                 if I = 0 then RepeatDelay := 0 else
                  if MouseInView(Event.Where) then begin
                   RepeatDelay := 6 - Round((I / (Size.Y shr 1)) * 6);
                   AutoRepeat := RepeatDelay;
                   if P.X < Size.X div 4 then Message(@Self, evKeyDown, kbLeft, nil) else
                    if P.X >= (Size.X*3) div 4 then Message(@Self, evKeyDown, kbRight, nil) else
                     if P.Y < Size.Y div 2 then Message(@Self, evKeyDown, kbUp, nil) else
                      Message(@Self, evKeyDown, kbDown, nil)
                  end else RepeatDelay := 0;
                until not MouseEvent(Event, evMouseMove+evMouseAuto);
                RepeatDelay := LR;
                AutoRepeat := LLR;
                CE
               end;
 end;
end;

        {-DataCompBoy-}
procedure TFileViewer.SetXlatFile;
  var Dr: String;
      Nm: String;
      Xt: String;
      S: TDosStream;
      Table: Array [1..256] of Record A, B: Char end;
      C, CC: Char;
      I,J: Integer;
{      PC: PChar;}
begin
  UseXLat:=Off;
  if aFName <> '' then
  begin
    lFSplit(aFName, Dr, Nm, Xt);
    if Dr = '' then begin Dr:=SourceDir+'XLT\'; Xt:='.xlt'; end;
    S.Init(Dr+Nm+Xt, stOpenRead);
    if (S.Status = stOK) and (S.GetSize >= 2) then
    begin
      J:=Min(256, S.GetSize shr 1);
      S.Read(Table, J * 2);
      NullXLAT(XLAT);
      for I:=1 to J do XLat[Table[I].A]:=Table[I].B;
      UseXlat:=S.Status = stOK;
      if XlatFile<>nil then DisposeStr(XlatFile); XlatFile:=nil;
      if UseXlat then XlatFile:=NewStr(Nm);
    end;
    S.Done;
  end;
  if aFName='' then begin
    if not GetToAsciiRecodeTable ( KeyMap, XLat ) then
      NullXLAT(XLat);
  end;
  if DoRedraw then DrawView;
end;
        {-DataCompBoy-}

procedure TFileViewer.DoHighlite
          ( var B; const S : String; const Attr : string );
var
  i : Integer;
  j : Integer;
  k : Integer;
  l : Integer;
  c : Char;
begin
  i := 1;
  j := 0;
  l := Length(S);
  while (i <= l) do begin
    c := S[i];
    k := i + 1;
    while (k <= l) and (S[k] = c) do
      Inc ( k );
    if (c <> #0) and (Ord(c) <= Length(Attr)) then
      MoveColor ( TWordArray(B)[j], k-i, Ord(Attr[Ord(c)]) );
    Inc ( j, k-i );
    i := k;
  end;
end;

{AK155 05.2001 Процедура CountDown почти полностью переписана;
части, соотвествующие vmAsm, собраны до кучи, но не проверялись}

procedure TFileViewer.CountDown;
{$IFDEF DisAsm}
var
  D: XchgData; {GRM!}
{$ENDIF}
begin
 if Buf = nil then Exit;
 case ViewMode of

  vmHex, vmDump:
    begin
      if not ScrollEOF and
         (FilePos + BufPos + HexPos*(Size.Y) >= FileSize)
      then
        exit;
      if FilePos + BufPos + HexPos*ANumber >= FileSize then
        Exit;
      while (BufPos + HexPos < BufSize) and (ANumber > 0) do
        begin Inc(BufPos, HexPos); Dec(ANumber) end;
      if BufPos + HexPos*Size.Y > BufSize then
        Seek(FilePos+LongInt(BufPos));
      inc(BufPos, ANumber*HexPos);
    end {vmHex, vmDump};

{$IFDEF DisAsm} {GRM!}

  vmAsm:
    begin
      if (Lines[ANumber].Pos < 0) or (Lines[Size.Y].Pos < 0) then
        Exit;
      D.MemBuff:=Buf;
      while (ANumber > 0) do begin
       D.InstrMaxLen := FileSize - FilePos - BufPos - 1;
       D.Offset:=BufPos;
       ScanCode(D);
       if BufPos+D.InstrLen+15*Size.Y > BufSize then
        Seek(FilePos+LongInt(BufPos));
       if FilePos+BufPos+D.InstrLen >= FileSize then break;
       Inc(BufPos, D.InstrLen);
       Dec(ANumber);
      end;
      MakeLines;
      while (Cur.Y<>0) and (Lines[Cur.Y].Pos < 0) do Dec(Cur.Y);
      Cur.X:=10;
    end {vmAsm};
{$ENDIF} {GRM!}

  else {vmText}
    begin
      if not ScrollEOF and (ExposedPos+FilePos = FileSize) then
        exit;
      if ((MaxLines < Size.Y+Anumber-1) or (Lines[Size.Y+Anumber-1].Pos = - 1))
         and (FilePos+BufSize < FileSize)
      then
         Seek(FilePos+Lines[0].Pos);
      if Lines[ANumber].Pos < 0 then
        exit;
      BufPos := Lines[ANumber].Pos;
      if ANumber <> 0 then
      begin
        if (ANumber+Size.Y < MaxLines)  then
        begin
          Move(Lines[ANumber], Lines[0], sizeof(Lines[0])*(MaxLines-ANumber));
          Dec(MaxLines, ANumber);
        end
        else MakeLines;
      end;
    end {vmText};
 end {case};

 DrawView;
 MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
end;


procedure TFileViewer.CountUp;
  var I, LineStart, LineEnd: Longint;
     NextLineStart: longint;
     NewFilePos, ReadLen: longint;
{$IFDEF DisAsm}
     D: XchgData; {GRM!}
{$ENDIF}
     N: longint;
     MaxX: longint;

 label 1;

{AK155}
 procedure ScrollUp;
{ Поиск в буфере начала предыдущей строчки для показа
  (режим TextView, с учетом Wrap и Size.X).
 Вход: NextLineStart - текущее начало строки в буфере
   (_после_ маркера типа CR LF, если он есть);
 Выход: LineStart - начало предыдущей строки;
        LineEnd - ее конец (на маркере, если он есть;
                  обычно LineEnd=NextLineStart-1).
 Если при поиске начала упираемся в начало буфера, то читать предыдущий
 файл не пытаемся, а просто возвращаем LineStart=0. }

  var I, M, W: Longint;
      TS: Integer;
      c1, c2: byte;
begin
  TS := GetTabSize;
  LineEnd := NextLineStart;
  LineStart := LineEnd;
  if LineEnd <= 0 then Exit;
  { Пропуск маркера конца }
  if (LineEnd > 0) then
  begin
    c1 := Buf^[LineEnd-1];
    if c1 in [$0D, $0A] then
    begin
      dec(LineEnd);
      if LineEnd > 0 then
      begin
        c2 := Buf^[LineEnd-1];
        if (c2 in [$0D, $0A]) and (c1 <> c2) then
          dec(LineEnd);
      end;
    end;
  end;

  { Поиска начала строки в буфере. Ограничиться шириной экрана
  прямо сейчас нельзя, так как правильный учет табуляций возможен
  только при просмотре от самого начала строки.}
  LineStart := LineEnd;
  while (LineStart > 0) and not (Buf^[LineStart-1] in [$0D, $0A])
        and (LineEnd-LineStart < MaxWrapW)
         { > MaxWrapW бывает при просмотре файла без CR и LF. В этом
         случае границы строк чисто условные, так что нет никакого
         смысла сканировать на неизвестную глубину назад, а потом
         снова сканировать вперед, нарезая на куски максимальной
         длины }
  do
    dec(LineStart);

  { Сейчас LineStart - настоящее начало строчки в буфере. Но с учетом wrap
   она может разделиться на несколько строчек экрана. Ниже это проверяется
   и в качестве LineStart возвращается начало последней из них. }

  M := LineStart; { M - кандидат на конец строки }
  W := 0;
  While M < LineEnd-1 do
  begin
    repeat Inc(W)
    until (Buf^[M] <> 9) or (W mod TS = 0);
    if W <= MaxX then
      Inc(M)
    else
    begin { нужен разрез строки }
      if Wrap=wmWords then
      begin { вернуться до границы слова }
        I := M;
        While I > LineStart do
        begin
          if Char(Buf^[I-1]) in BreakChars then
          begin { граница слова найдена }
            M := I; break;
          end;
          dec(I);
        end;
      end;
      LineStart := M; W := 0; { выполнили разрез }
    end;
  end;
end;
{/AK155}

begin
  if Buf = nil then Exit;
  if BufPos > 65000 then
    begin BufPos := 0; MakeLines; end; {AK155: IMHO так не бывает}
  NextLineStart := BufPos;
 {$IFDEF DisAsm}
  D.MemBuff:=Buf; {GRM!}
  D.Offset:=BufPos; {GRM!}
 {$ENDIF}
  if Wrap>wmNone then MaxX := Size.X else MaxX := MaxWrapW;
  for N := 1 to ANumber do
  begin
    if (NextLineStart <= 0) and (FilePos <= 0) then
      break; { уже стоим в начале, двигаться некуда }
1:
    {GRM!}
    case ViewMode of
     vmText:
       ScrollUp;
     vmHex, vmDump:
       begin
       LineStart := max(0, NextLineStart - HexPos);
       LineEnd := NextLineStart;
       end;
  {$IFDEF DisAsm}
     vmAsm: begin
             ScanUp(D);
             Dec(LineStart, D.InstrLen);
            end;
  {$ENDIF}
    end;
    {GRM!}
    if Wrap>wmNone then MaxX := Size.X else MaxX := MaxWrapW;
    if (ViewMode = vmText) and (LineEnd = Lines[0].Pos) and
      (LineEnd <> 0) {AK155 такое бывает в самом начале буфера}
      and (Lines[0].Len + Lines[0].Pos-LineStart <= MaxX)
       {AK155 > MaxWrapW бывает при просмотре файла без CR и LF }
    then
    begin
  {AK155 При переходе от Hex/Dump к Text начало первой строчки
  экрана могло оказаться не началом строки в буфере. В момент смены
  режима на настоящее начало строки скакать было нехорошо,
  чтобы переключение режима не сбивало позицию, а сейчас надо
  таки перейти к настоящему началу строки. Реально такое
  может быть только при I=1 }
      inc(Lines[0].Len, Lines[0].Pos-LineStart);
      Lines[0].Pos := LineStart;
      BufPos := LineStart;
    end
    else {if ViewMode = vmText then } {/AK155}
    begin
      if LineStart > 0 then
        BufPos := LineStart { нормальное начало строки }
      else if FilePos = 0 then
      begin { начало файла }
        if BufPos = 0 then
          exit; { и так уже были в начале }
        BufPos := 0; { нормальное начало строки }
      end
      else
      begin { начало буфера не в начале файла: сдвинуться по файлу
              на полбуфера и повторить поиск начала строки }
        NewFilePos := max(0, FilePos - (BufSize div 2));
        ReadLen := FilePos-NewFilePos;
        move(buf^[0], buf^[ReadLen], BufSize-ReadLen);
        FL^.Seek(NewFilePos);
        Fl^.Read(Buf^, ReadLen);
        FilePos := NewFilePos;
        BufPos := ReadLen;
        for i := 0 to MaxLines do
        begin
          if Lines[i].Pos < 0 then
            break;
          inc(Lines[i].Pos, ReadLen);
          if Lines[i].Pos+Lines[i].Len > BufSize then
            begin
            MaxLines := i-1;
            if i=0 then
              break;
            break;
            end;
        end;
        inc(NextLineStart, ReadLen);
        Goto 1;
      end;

      System.Move(Lines[0], Lines[1], (MaxLines - 1) * sizeof(Lines[0]));
      if MaxLines < 200 then Inc(MaxLines);
      Lines[0].Pos := LineStart;
      Lines[0].Len := LineEnd-LineStart;
    end;

  NextLineStart := LineStart;
  end;
 {$IFDEF DisAsm}
  if ViewMode=vmAsm then Cur.X:=10; {GRM!}
 {$ENDIF}
  DrawView;
  MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
end;

function TFileViewer.Valid;
begin
  Valid := IsValid or QuickView;
  if (Command = cmClose) or (Command = cmQuit) then
   if (Owner <> nil) and (not QuickView) then
    StoreViewInfo(Owner);
end;

{ TFileWindow }
constructor TFileWindow.Init;
var
  R: TRect;
  P: PView;
  PV: PFileViewer;
begin
  if LastViewerBounds.Empty or (InterfaceData.Options and ouiStoreViewerPosition = 0)
    then begin Desktop^.GetExtent(LastViewerBounds); LastViewerDeskSize := Desktop^.Size; end;
  R := LastViewerBounds; AdjustToDesktopSize(R, LastViewerDeskSize);
  TWindow.Init(R, {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(VFileName), 0);
  Flags := Flags or wfMaxi;
  Options := Options or ofTileAble;
  GetExtent(R);
  R.Grow(-1, -1);
  R.A.X := R.B.X; Inc(R.B.X);
  P := New(PViewScroll, Init(R)); P^.GrowMode := gfGrowHiX + gfGrowLoX + gfGrowHiY;
  Insert(P);
  GetExtent(R);
  R.Grow(-1, -1);
  PV := New(PFileViewer, Init(R, nil, Filename, VFileName, P, False, Hex));
  Insert(PV);
  GetExtent(R);
  Inc(R.A.X); R.A.Y := R.B.Y - 1; Dec(R.B.X, 2);
  P := New(PViewInfo, Init(R, PV));
  Insert(P);
  PV^.Info := P;
end;

procedure TFileWindow.ChangeBounds;
begin
  inherited ChangeBounds(Bounds);
  if not GetState(sfModal) then
    begin
       GetBounds(LastViewerBounds);
       LastViewerDeskSize := Desktop^.Size;
    end;
end;

function TFileWindow.ReactOnCmd;
begin
  ReactOnCmd := On
end;


end.
