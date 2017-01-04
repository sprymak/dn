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
//  dn16rc1-XLat_bugfix.patch
//  dn16rc1-viewer_gotolabel.patch
//  dn16rc1-viewer_backward_search.patch
//  dn16rc1-deleting_temporary_files_safety_fix.patch
//  dn16rc1-window-manager-changes.patch
//  dn16rc1-Bugfix_for_dn16rc1-viewer_backward_search_patch-diff151byMV.patch
//
//  2.0.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit FViewer;

interface

uses Objects, Views, Dos, Drivers, HideView, Commands, DNHelp, advance,
     advance1, advance2, advance3, highlite, U_KeyMap, microed,
     collect, macro, Advance6;

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
    XDelta, ViewMode, HexPos: AInt;
    SearchActive: Boolean;
    SearchX: LongInt;
    SB: PView;
    Wrap: Byte; {DataCompBoy}
    Lines: Array[0..200] of record Pos: LongInt; Len: Byte; end;
    FilePos, FileSize, NumLines: LongInt;
    Cur: TPoint;
    Info: PView;
    BufPos: AWord;
    BufSize, MaxLines: LongInt;
    BufLines: AInt;
    KillAfterUse, IsValid, QuickView, Loaded, HexEdit, BufModified: Boolean;
    Filter: Byte;
    XLAT: TXLAT;
    UseXLAT: Boolean;
    XLatFile: PString;
    KeyMap: TKeyMap;
    MarkPos: TPosArray;
    CtrlK: Boolean;
    CtrlQ: Boolean;
    HiLite : Boolean;           {PZ 2000.06.09}
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
  private
    procedure   AdjustBufSize;
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

  {Dir = bit 0: [basic search] 0 - forward, 1 - backward
         bit 1: [last search]  0 - forward, 1 - backward }

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
                       AllCP: Boolean): LongInt;

implementation

uses LFN, {DataCompBoy}
     Memory, Messages, DNApp, Startup, Dialogs, RStrings,
     {$IFDEF DisAsm} Decoder, {$ENDIF} {piwamoto}
     Gauge, DNStdDlg, Histries, xTime, Drives, DnUtil, DNIni;

function MaxAvail: LongInt;
begin
  MaxAvail := MemAdjust(System.MaxAvail);
end;

const
  ViewerBufSize = $8000{8192};

function SearchFileStr;
 label 1, LExit;
 var Buf: PByteArray;
     Count: LongInt;
     BufLen, BufPos, BPos, T, StPos, StDelta: Word;
     I, L, J, OldPos, NextPos: LongInt;
     CancelSearch: Boolean;
     Info: PWhileView;
     R: Trect;
     Tmr: TEventTimer;
     Inserted: Boolean;
     W: String;
begin
 Info := nil;
 SearchFileStr := -1; Count := 0; ClrIO;
 OldPos := F^.GetPos; L := F^.GetSize; if L = 0 then goto LExit;
 F^.Seek (Pos);
 BufLen := $7000;
 if LongInt(BufLen) > L then  BufLen := L;
 if BufLen > MaxAvail then BufLen := MaxAvail;
 if BufLen < Length(What) then begin F^.Seek(OldPos); goto LExit; end;
 StPos := 0;
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
       J := I - NextPos + BPos;
       if NextPos < 0 then
         begin
           NextPos := 0;
           J := I + BPos;
           BPos := 0; StPos := 0;
         end;
       if J <= 0 then Break;
     end else
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
   if Back then F^.Read(Buf^[0], J-BPos)
           else F^.Read(Buf^[BPos], J-BPos);
   T := J-BPos;
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
             if Back then J := BufPos + Length(What) - 2
                     else Inc(StPos, BufPos);
             Goto 1;
           end;
     FreeMem(Buf, BufLen); F^.Seek(OldPos);
     if Back then SearchFileStr := NextPos + LongInt(BufPos) - 1 + StPos
             else SearchFileStr := NextPos - LongInt(BPos) + LongInt(BufPos) - 1 + StPos;
     goto LExit;
    end;
   StPos := 1;
   BPos := Length(What)+1;
   if Back then
     begin
       Dec(I,T);
       if I > 0 then Move(Buf^[0], Buf^[J-BPos], BPos);
     end else
     begin
       Inc(I,T);
       if I < L then Move(Buf^[J-BPos], Buf^[0], BPos);
     end;
 until  Back and ((I <= 0) or CancelSearch) or
        not Back and ((I >= L) or (T <= 0) or CancelSearch or (F^.GetPos >= F^.GetSize));
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
      Dec(S[0]);
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
    begin
      case KeyMap of
        kmAscii: S:=S+'DOS..   ';
        kmAnsi:  S:=S+'WIN..   ';
        kmKoi8r: S:=S+'KOI..   ';
        else
                 S:=S+'XXX..   ';
      end;
    end;
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
        S:=S + Long2Str(Round(Percent(FileSize, FilePos+BufPos)),3);
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
      if P.X <  7 then {} else
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
     Fl:=New(PDosStream, Init(FileName, stOpen));
    end;
    if (Fl^.Status <> stOk) or (Abort) then begin
     Dispose(Fl,Done); Fl:=New(PDosStream, Init(FileName, stOpenRead));
     MessageBox(GetString(dlFBBNoWrite)+FileName, nil, mfError + mfOKButton);
    end else begin
     Fl^.Seek(FilePos); Fl^.Write(Buf^, BufSize);
    end;
    if A<>0 then begin
     Dispose(Fl,Done);
     SetFileAttr(FileName, A);
     Fl:=New(PDosStream, Init(FileName, stOpenRead));
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
               if Longint( Longint(Longint(Value) * Longint(Size.Y-2)) div MaxV ) + 1 = P.Y then
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
                  if SP = Size.Y - 2  then
                     begin
                        Value := MaxV - (Size.Y-10)*10;
                        if Value > MaxV then Value := MaxV;
                     end
                  else if SP=1 then Value := 0
                 { else Value := LongMul( LongDiv(MaxV,Size.Y-2),SP );}
                  else Value := Longint( Longint( MaxV div Longint(Size.Y-2)) * SP );
                if Size.Y*4 > MaxV then Value:=0;
                DrawView;
                MessageL(Owner, evCommand, cmScrollBarChanged, Value);
               Exit;
               end;


                MakeLocal(Event.Where,P); RD := RepeatDelay; RepeatDelay := 0;
                if Value >= MaxV then SP := Size.Y - 2
                                 else SP := GetPartCode;
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
                   if Value >= MaxV then SP := Size.Y - 2
                                    else SP := GetPartCode;
                   MakeLocal(Event.Where,P);
                   if (P.X>=-1) and (P.X<=1) and (P.Y<SP) then
                    Message(Owner, evKeyDown, kbPgUp, nil);
                  until not MouseEvent(Event, evMouseMove+evMouseAuto);
                 end else
                if P.Y > SP then
                 begin
                  RepeatDelay := RD;
                  repeat
                   if Value >= MaxV then SP := Size.Y - 2
                                    else SP := GetPartCode;
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
GetPartCode := 1+((Size.Y-2)*Value) div MaxV;
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
begin
 C1 := GetColor(1); C2 := GetColor(2);
 MoveChar(B, #177, C1, Size.Y);
 if Value >= MaxV then B1[Size.Y-2] := 254+LongInt(C2*256)
                  else B1[GetPartCode] := 254+LongInt(C2*256);
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

procedure TFileViewer.AdjustBufSize;
begin
  BufSize := ViewerBufSize;
  If BufSize > FileSize then BufSize := FileSize;
  If MaxAvail < BufSize then BufSize := MaxAvail;
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
  KillAfterUse := TempFile <> '';
  Wrap:=0;
  if ViewerDefaults.Options and vbfWrp <> 0 then Wrap:=1;
  if ViewerDefaults.Options and vbfWWr <> 0 then Wrap:=2;
  ViewMode := Byte(Hex);
  Fl := AStream;
  if (AFileName = '') and (Fl = nil) then Exit;
  if Fl = nil
   then if Not ReadFile(AFileName, AVFileName, On) then IsValid:=off else
   else if (Fl <> nil) and (Fl^.Status = stOK)
         then ReadFile(' ', ' ', Off)
         else IsValid := Off;
  TempFile:='';
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
 S.Read(FileName[0], 1); S.Read(FileName[1], Length(FileName));
 S.Read(VFileName[0], 1); S.Read(VFileName[1], Length(VFileName));
 S.Read(I, Sizeof(LongInt));
 S.Read(QuickView, 1);
 S.Read(Wrap, 1);
 S.Read(KillAfterUse, 1);
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
 S.Write(FileName[0], 1 + Length(FileName));
 S.Write(VFileName[0], 1 + Length(VFileName));
 I := FilePos + LongInt(BufPos);
 S.Write(I, Sizeof(LongInt));
 S.Write(QuickView, 1);
 S.Write(Wrap, 1);
 S.Write(KillAfterUse, 1);
 S.Write(Filter, 1);
 S.Write(ViewMode, 2);
 S.WriteStr(XlatFile);
 S.Write(XLAT, SizeOf(XLAT));
 S.Write(UseXLAT, SizeOf(UseXLAT));
 S.Write(KeyMap, SizeOf(KeyMap));
 S.Write(MarkPos, Sizeof(MarkPos));
end;

procedure XDumpStr(var S: String; var B; Addr: LongInt; Count: Integer);
begin
  S := Hex8(Addr)+' ';
  {$IFNDEF BIT_32}
  asm
    les  si, B
    mov  cx, Count
    push ds
    lds  di, S
    mov  bx, 10
    jcxz @@3
@@1:
    mov  al, es:[si]
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
    jcxz @@3
@@1:
    mov  AL, [ESI]
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


procedure TFileViewer.Draw;
var
  B: TDrawBuffer;
  C: Byte;
  I, J, K, M, Src: Integer;
  L: LongInt;
  S: String;
  TS: word;
  C1: Byte;
  HP : string[6];
  CC: Array[1..8] of Byte;
{$IFDEF DisAsm}
  D: XCHGData; {GRM!}
{$ENDIF}

var
  W, W2, W3: Integer;
  WDH: Integer;

begin
  for I := 1 to 8 do CC[I] := GetColor(I);
  if (ViewerDefaults.Options and vbfTRp)=0 then TS:=0
  else begin
    TS:=StoI(ViewerDefaults.TabSize);
    if TS=0 then TS:=8;
  end;
  if TS>100 then TS:=100;
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
  if Info <> nil then Info^.DrawView;
  if {(FileName = '') or }(Buf = nil) then begin
    HideCursor; inherited Draw; Exit;
  end;
  Case ViewMode of
   vmHex: begin
           DisableCommands([cmUnwrap]);
           HexPos:=(Size.X - 12) div 4; if HexPos <= 0 then HexPos:=1;
           XDelta:=0;
           while (LongInt(Cur.Y * HexPos) + BufPos + FilePos >= FileSize) do
             Dec(Cur.Y);
           while (LongInt(Cur.Y * HexPos) + BufPos + FilePos + Cur.X
             div (Byte(HexEdit)+1) >= FileSize) do Dec(Cur.X);
           if Cur.Y < 0 then begin
            W:=-Cur.Y;
            Cur.Y:=0;
            CountUp(W);
            Exit;
           end;
           if Cur.Y >= Size.Y then begin
            W:=Cur.Y - Size.Y + 1;
            Dec(Cur.Y, W);
            CountDown(W);
            Exit;
           end;
           W:=BufPos;
           L:=FilePos + LongInt(BufPos);
           if HexEdit
            then SetCursor(10 + (Cur.X div 2) * 3 + Cur.X and 1, Cur.Y)
            else SetCursor(12 + HexPos * 3 + Cur.X, Cur.Y);
           ShowCursor;
           for I:=0 to Size.Y - 1 do begin
            MoveChar(B, ' ', C, Size.X);
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
              Drivers.MoveStr(B[10+HexPos*3], Copy(S, J*3+11, 255), C);
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
            WriteLine(0, I, Size.X, 1, B);
           end;
          end;
  vmDump: begin
           DisableCommands([cmUnwrap]);
           HideCursor;
           HexPos:=((Size.X - 9) div 16)*16; if HexPos < 16 then HexPos:=16;
           W:=BufPos; L:=FilePos + LongInt(BufPos); Src:=0;
           for I:=0 to Size.Y - 1 do begin
            MoveChar(B, ' ', C, Size.X);
            if L + LongInt(HexPos) > FileSize
             then J:=FileSize - L
             else J:=HexPos;
            if J > 0 then begin
             XDumpStr(S, Buf^[W], L, J);
             XLatBuf(S[10], Length(S)-9, XLat);
             {-DataCompBoy: apply filter-}
             case Filter of
              0:   ;
              1:   For w2:=10 to Length(S) do
                    if not (Byte(S[w2]) in [32..127]) then S[w2]:=#250;
              else For w2:=10 to Length(S) do
                    if not (Byte(S[w2]) in [32..255]) then S[w2]:=#250;
             end;
             {-DataCompBoy: done-}
             MoveStr(B, S, C);
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
            WriteLine(0, I, Size.X, 1, B);
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
          MoveChar(B, ' ', C, Size.X);
          if L < FileSize - 1 then begin
           if Cur.Y=I then HexPos := L - FilePos - BufPos + ((Cur.X-10) div 2);
           D.InstrMaxLen := FileSize - L;
           {Draw - doesn't matter}
           ScanCode(D);
           S:=UpStrg(hex8(L))+': '+D.CodeStr+Strg(' ',29-length(D.CodeStr))+
                                   D.Command+Strg(' ',10-length(D.Command))+
                                   D.Operands;
           MoveStr(B, S, C);
           Inc(L, D.InstrLen);
          end;
          WriteLine(0, I, Size.X, 1, B);
         end;
        end
  {GRM!}
{$ENDIF}{DisAsm}
  else begin
        EnableCommands([cmUnwrap]);
        for I:=0 to Size.Y - 1 do begin
         HideCursor;
         J:=SearchX - FilePos - Lines[I].Pos;
         if SearchActive and (Lines[I].Pos <= SearchX - FilePos) and
            (Lines[I+1].Pos >= SearchX - FilePos) then begin
          K:=0; M:=Lines[I].Pos;
          While M < J + Lines[I].Pos do begin
           repeat Inc(K)
           until (Buf^[M] <> 9) or (K mod TS = 0);
           Inc(M);
          end;
          if (XDelta + Size.X < K) or (XDelta > K )
           then XDelta:=K - Size.X div 2;
          if XDelta < 0 then XDelta:=0;
         end;
         MoveChar(B, ' ', C, Size.X);
         if (Lines[I].Pos >=0) then begin
          Move(Buf^[Lines[I].Pos], S[1], Lines[I].Len);
          S[0]:=Char(Lines[I].Len);

          if TS>0 then begin {-$VOL}
           {-DataCompBoy: Expand tabs...-}
           W:=1;
           While W<=Length(S) do
            If S[W]=#9 then begin
             Delete(S, W, 1);
             Insert(#32, S, W);
             Inc(W);
             While W mod TS <> 1 do begin
              Insert(#32, S, W);
              Inc(W);
             end;
            end else Inc(W);
           {-DataCompBoy: done...-}
          End;

          if (S<>'') and (S[Length(S)]=#13) then Dec(S[0]);
          XLatBuf(S[1], Length(S), XLat);
          {-DataCompBoy: apply filter-}
          case Filter of
           0:   ;
           1:   For w:=1 to Length(S) do
                 if not (Byte(S[w]) in [32..127]) then S[w]:=#250;
           else For w:=1 to Length(S) do
                 if not (Byte(S[w]) in [32..255]) then S[w]:=#250;
          end;
          {-DataCompBoy: done-}
          MoveStr(B, Copy(S, XDelta+1, Size.X), C);
          if Hilite and not QuickView then begin
           Highlites ( Length(S), @S[1], HiLitePar );
           C1                  := C and $F0;                 { Background }
           HP[0]               := #6;                        { Attr. size }
           HP[Ord(hhComment)]  := Chr(CC[3]);                { Comments   }
           HP[Ord(hhNumber)]   := Chr(C1 or (CC[6] and 15)); { Numbers    }
           HP[Ord(hhString)]   := Chr(C1 or (CC[5] and 15)); { Strings    }
           HP[Ord(hhSymbol)]   := Chr(C1 or (CC[4] and 15)); { Symbols    }
           HP[Ord(hhKeyword1)] := Chr(C1 or (CC[7] and 15)); { Keywords 1 }
           HP[Ord(hhKeyword2)] := Chr(C1 or (CC[8] and 15)); { Keywords 2 }
           DoHighlite ( B, S, HP );
          end;
         end;
         if SearchActive and (Lines[I].Pos <= SearchX - FilePos) and
            (Lines[I+1].Pos >= SearchX - FilePos) then begin
          K:=0; M:=Lines[I].Pos;
          While M < J + Lines[I].Pos do begin
           repeat Inc(K)
           until (Buf^[M] <> 9) or (K mod TS = 0);
           Inc(M);
          end;
          WDH:=K-XDelta;
          if WDH<0 then begin
           K:=Length(SearchString.What)+WDH;
           WDH:=0
          end else K:=Length(SearchString.What);
          MoveColor(B[WDH], K, CC[2]);
         end;
         WriteLine(0, I, Size.X, 1, B);
        end;
       end;
  end;
end;

function DetectKeyMap(const FName: String): TKeyMap;
begin
 DetectKeyMap:=kmAscii;
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
  if (ViewerDefaults.Options and vbfHlt) <> 0 then
    HiLite := Macro.InitHighLight ( VFName, HiLitePar, Macros, nil );
  {PZ end}
  if Fl^.Status <> stOK then begin IsValid := False; ReadFile:=false; Exit end;
  FileSize := FL^.GetSize; FilePos := 0; I := 1; NumLines := 1;
  if FileSize < 0 then FileSize := 0;
  AdjustBufSize;
  Buf := MemAlloc(BufSize);
  if SB <> nil then PViewScroll(SB)^.MaxV := FileSize;
  Seek(0); XDelta := 0;
  If VFileName<>FileName then begin
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

procedure TFileViewer.Seek;
begin
 if Buf = nil then Exit;
 FileSize := Fl^.GetSize;
 WriteModify;
 if APos < 0 then APos := 0;
 FilePos := APos;
 if FilePos < 0 then FilePos := 0;
 if FilePos = 0 then
  begin
   if Buf <> nil then FreeMem(Buf, BufSize);
   AdjustBufSize;
   GetMem(Buf, BufSize);
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
 if BufPos > BufSize then BufPos := BufSize - 1;
 Fl^.Read(Buf^, BufSize);
 MakeLines;
 MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
end;

procedure TFileViewer.MakeLines;
 var I, J, K, D: LongInt;
     MaxX: longInt;
     TSt: Integer;
{$IFDEF DisAsm}
     X: XchgData; {GRM!}
{$ENDIF}
begin
 if Buf = nil then Exit;
 if Wrap>wmNone then MaxX := Size.X else MaxX := 255;
 J := BufPos; I := BufSize - J;
 FillChar(Lines, SizeOf(Lines), 0);
 TSt := StoI(ViewerDefaults.TabSize);
 if TSt = 0 then TSt := 8;
 MaxLines := 200;
 case ViewMode of
  vmHex, vmDump: begin
      I := BufPos;
      for K := 0 to MaxLines do
       begin Lines[K].Pos := I; Lines[K].Len := HexPos; Inc(I, HexPos); end;
{$IFNDEF DisAsm}
     end
{$ELSE}
     end;
  {GRM!}
  vmAsm: begin
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
  else for K := 0 to MaxLines do begin {-DataCompBoy-}
        if J>=BufSize then begin
         Lines[K].Pos := -1;
         continue;
        end;
        Lines[K].Pos := J;
        {Search for e.nd of line}
        I:=0;
        D:=0;
        while (I<=MaxX-D) and (J+I<BufSize) do begin
         if (Buf^[J+I] in [$0D, $0A]) then begin
          if (J+I<BufSize-1) and (Buf^[J+I+1] in [$0D,$0A]) and
             (Buf^[J+I+1] <> Buf^[J+I]) then Inc(J);
          Inc(J);
          break;
         end else
         if (I>=MaxX-D) then break else
         if Buf^[J+I]=9 then inc(D, TSt);
         inc(I);
        end;
         {Search for e.nd of word-wrapped line}
        if (Wrap=wmWords) and (I>=MaxX-D) then begin
         D := I;
         While (I > 1) and not (Char(Buf^[Lines[K].Pos+I-1]) in BreakChars)
          do dec(I);
         If I <= 1
          then I := D
          else J := Lines[K].Pos;
        end;

        Lines[K].Len:=I;
        Inc(J, I);
       end; {-DataCompBoy-}
 end;
end;

 procedure TFileViewer.SaveToFile(FN: String);
   var S: TDosStream;
       P: PView;
       W, L: Word;
       PS: Pointer;
       Xl: Boolean;
       Sz: LongInt;

   label 1, 2;

 begin
   Xl := (MaxAvail > 4096) and (UseXlat or (KeyMap<>kmAscii)) and
         (MessageBox(GetString(dlViewSaveXlat), nil,
          mfYesButton+mfNoButton+mfConfirmation) = cmYes);
   S.Init(FN, stOpen);
   if S.Status = stOK then
     begin
        PS := @FN;
        W := MessageBox(GetString(dlED_OverQuery)
                        , @PS, mfYesButton+mfCancelButton+mfAppendButton+mfWarning);
        if W = cmYes then
          begin
1:
            S.Done;
            S.Init(FN, stCreate);
          end else
           if W = cmOK then S.Seek(S.GetSize) else
             begin S.Done; Exit end;
     end else Goto 1;

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
     end else 2: S.CopyFrom(Fl^, Fl^.GetSize);
   P^.Free;
   S.Done;
   FN := GetPath(FN); if (FN[0] > #3) and (FN[Length(FN)] = '\') then Dec(FN[0]);
   GlobalMessage(evCommand, cmRereadDir, @FN);
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
  var I, J: Integer;
 begin
  if Buf = nil then Exit;
  J := 0;
  CE; for I := 0 to SIze.Y - 1 do if J<Lines[I].Len then J := Lines[I].Len;
  Dec(J, Size.X);

  if J < 0 then XDelta := 0 else XDelta := J;
  DrawView;
 end;

 procedure SeekEOF;
   var I: Integer;
 begin
  if Lines[Cur.Y].Pos < 0 then Exit;
  I := (FilePos+BufPos) mod 16;
  Owner^.Lock;
  Seek(FileSize - BufSize); BufPos := BufSize;
  if ViewMode = vmHex then
      While (FilePos+BufPos) mod 16 <> I do Inc(BufPos);
  CountUp(Size.Y+3);
  CountDown(3);
  MakeLines;
  case ViewMode of
   vmHex: begin
           Cur.X := HexPos*2-2;
           Cur.Y := 0;
           while Lines[Cur.Y+1].Pos >= 0 do Inc(Cur.Y);
          end;
{$IFDEF DisAsm}
   vmAsm: begin{GRM!}
           Cur.Y := 0;
           Cur.X := 10;
           while Lines[Cur.Y+1].Pos >= 0 do Inc(Cur.Y);
          end; {GRM!}
{$ENDIF}
  end;
  DrawView;
  if Cur.X and 1 <> 0 then
   begin
     Dec(Cur.X);
     DrawView;
   end;
  Owner^.UnLock;
 end;

 procedure ContinueSearch(Reverse: Boolean);
  var I, J, JJ: Integer;
      BXL: TXLAT;
 begin
  if (SearchString.What = '') then Exit;
  if SearchX < 0 then SearchX := 0;
  if (SearchX < FilePos + Lines[0].Pos) or
     (SearchX > FilePos + Lines[1].Pos+1)
     then SearchX := FilePos+BufPos;
  NullXLAT(BXL);
  for i:=255 downto 1 do
   if XLat[Char(i)]<>Char(i) then BXL[XLat[Char(i)]]:=Char(i);

  if (((SearchString.Dir and 2) = 2) <> (((SearchString.Dir and 1) = 1) xor Reverse)) then
  begin
    if (SearchString.Dir and 2 = 0) then
      Inc(SearchX, LongInt(Byte(SearchString.What[0])) - 2)
    else
      Dec(SearchX, LongInt(Byte(SearchString.What[0])) - 2);
    SearchString.Dir := SearchString.Dir xor 2;
  end;

  SearchX := SearchFileStr(Fl, BXL,
                           SearchString.What, SearchX,
                           SearchString.Opts and 1 <> 0, On,
                           SearchString.Opts and 2 <> 0, ((SearchString.Dir and 1) = 1) xor Reverse,
                           SearchString.Opts and 4 <> 0);

  if SearchX >= 0 then
   begin
    if HexPos > 0 then JJ := (FilePos+BufPos) mod HexPos;
    Seek(SearchX-256);
    J := SearchX - FilePos;
    if ViewMode = vmText then
      begin
        MakeLines;
        for I := 0 to MaxLines do
         if (Lines[I].Pos <= J) and ((Lines[I+1].Pos > J) or (Lines[I+1].Pos < 0)) then
          begin BufPos := Lines[I].Pos; MakeLines; Break end;
      end else
      begin
        BufPos := SearchX - FilePos;
{$IFDEF DisAsm}
        if ViewMode<>vmAsm then {GRM!}
{$ENDIF}
         while (BufPos <> 0) and
          (JJ <> (FilePos + BufPos) mod HexPos) do Dec(BufPos);
      end;
    MakeLines;
    SearchActive := On;
    DrawView;
    SearchActive := Off;
    if (((SearchString.Dir and 1) = 1) xor Reverse) then
      Inc(SearchX, LongInt(Byte(SearchString.What[0]) - 1))
    else
      Inc(SearchX);
   end else if SearchX <> -2 then MessageBox(GetString(dlDBViewSearchNot), nil, mfError + mfOKButton);
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
  SearchString.Dir := SearchString.Dir and 1;
  D^.SetData(SearchString);
  I := Desktop^.ExecView(D);
  D^.GetData(SR);
  Dispose(D,Done);
  if I = cmCancel then Exit;
  SearchString := SR;
  SearchX := 0;
  Inc(SearchString.Dir, SearchString.Dir shl 1);
  ContinueSearch(Off);
 end;

 procedure LoadXlatTable;  {JO}
   var FN: String;
       More: Boolean;
       None: Boolean;
 begin
   CE;
   FN := CnvString(XlatFile); if FN <> '' then FN := FN + '.xlt';
   DisposeStr(XlatFile); XlatFile:=nil;
   More := True;
   None := UseXlat;
   FN := GetFileNameMenu(SourceDir+'XLT\','*.XLT', FN, On, More, None);
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
  if KeyMap=kmAscii then KeyMap:=kmAnsi  else
   if KeyMap=kmAnsi  then KeyMap:=kmKoi8r else
    KeyMap:=kmAscii;
  SetXlatFile('', On);
 end;

 label 1,2,DoSave, KBCheck, NotKb;

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
                    PDNAppl(Application)^.EditFile(True, FileName);
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
                          then PString(Event.InfoPtr)^ := GetString(dlViewFile)
                          + ' - ' + Cut(VFileName, 49 - Length(GetString(dlViewFile)))
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
                             vmDump: begin ViewMode := vmAsm; HexEdit := On; MakeLines; Cur.X:=10; end; {GRM!}
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
                         DrawView; CE;
                        end;
              cmScrollBarChanged: begin
                                   Seek(Event.InfoLong);
                                   if ViewMode = vmText then
                                       begin
                                        CountDown(1);
                                        CountUp(1);
                                       end;
                                   MakeLines;
                                   DrawView;
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
               kbESC: if not QuickView then begin CE; Message(Application, evCommand, cmClose, nil) end;
               kbCtrlLeft: if ViewMode = vmText then
                     begin CE; if XDelta>19 then Dec(XDelta,20) else XDelta:=0; DrawView end
                          else (*if ViewMode <> vmAsm then {GRM!}*)
                             begin
                               if BufPos > 0 then Dec(BufPos) else
                                 if FilePos > 100 then
                                   begin
                                     Owner^.Lock;
                                     Seek(FilePos - 100);
                                     BufPos := 99;
                                     Owner^.UnLock;
                                   end;
                               DrawView; CE
                             end;

               kbCtrlRight: if ViewMode = vmText then
                          begin CE; if XDelta<255*8-Size.X-19 then Inc(XDelta,20); DrawView end
                            else (*if ViewMode <> vmAsm then {GRM!}*)
                             begin
                               if BufPos < BufSize - 1024 then Inc(BufPos) else
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
1:                            SearchX := 0;
                              CE; Seek(0);
                              if ViewMode = vmAsm
                               then Cur.Assign(10, 0)
                               else Cur.Assign( 0, 0);
                              XDelta := 0;
                              DrawView
                           end;
               kbCtrlPgDn: begin
2:                           SearchX :=  FileSize;
                             Lines[Size.Y].Pos := 0; {piwamoto}
                             SeekEOF;
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
                        vmText,
                        vmDump: begin
                                 CE;
                                 CountDown(1)
                                end;
                        vmHex:  begin
                                 Inc(Cur.Y);
                                 DrawView;
                                 CE;
                                end;
{$IFDEF DisAsm}
                        vmAsm:  if Cur.Y = Size.Y-1 then begin
                                 CE;
                                 CountDown(1)
                                end else
                                 if Lines[Cur.Y+1].pos<>-1 then begin
                                  Inc(Cur.Y);
                                  Cur.X:=10;
                                  DrawView;
                                  CE;
                                 end;
{$ENDIF}
                       end;{GRM!}
               kbPgDn: begin CE; CountDown(Size.Y-1) end;
               kbSpace: if ViewMode in [vmText, vmDump]
                         then begin CE; CountDown(Size.Y-1) end
                         else Goto KBCheck;
               kbUp:   case ViewMode of {GRM!}
                        vmText,
                        vmDump: begin
                                 CE;
                                 CountUp(1)
                                end;
                        vmHex:  begin
                                 Dec(Cur.Y);
                                 DrawView;
                                 CE;
                                end;
{$IFDEF DisAsm}
                        vmAsm:  if Cur.Y = 0 then begin
                                 CE;
                                 CountUp(1)
                                end else begin
                                 Cur.X:=10;
                                 Dec(Cur.Y);
                                 DrawView;
                                 CE;
                                end;
{$ENDIF}
                        end; {GRM!}
               kbPgUp: begin
                          CE;
                          CountUp(Size.Y-1);
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
               kbRight: if ViewMode = vmText then begin CE; if XDelta<255*8-Size.X then Inc(XDelta); DrawView end
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
  {JO}         kbEnter: case UseEnterInViewer of
                      1: if not QuickView then begin CE; Message(Application, evCommand, cmClose, nil) end;
                      2: Message(Owner, evCommand, cmHexMode, nil);
                        end;
  (* X-Man *)  kbF3: case UseF3InViewer of
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
                  kbCtrl1:Event.Command:=cmGotoMarker1;
                  kbCtrl2:Event.Command:=cmGotoMarker2;
                  kbCtrl3:Event.Command:=cmGotoMarker3;
                  kbCtrl4:Event.Command:=cmGotoMarker4;
                  kbCtrl5:Event.Command:=cmGotoMarker5;
                  kbCtrl6:Event.Command:=cmGotoMarker6;
                  kbCtrl7:Event.Command:=cmGotoMarker7;
                  kbCtrl8:Event.Command:=cmGotoMarker8;
                  kbCtrl9:Event.Command:=cmGotoMarker9;
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
  i := XDelta+1;
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

procedure TFileViewer.CountDown;
{$IFDEF DisAsm}
var D: XchgData; {GRM!}
{$ENDIF}
begin
 if Buf = nil then Exit;
 {GRM!}
 if ((Lines[Size.Y-1].Pos < 0) and (ViewMode = vmText)) or
    ((FilePos + LongInt(BufPos) + LongInt(HexPos)*LongInt(Size.Y) > FileSize) and
     (ViewMode in [vmHex,vmDump]))
{$IFDEF DisAsm}
    or ((Lines[Size.Y].Pos < 0) and (ViewMode = vmAsm))
{$ENDIF}
    then Exit;
 {GRM!}
 if (Lines[Size.Y-1].Pos < 0) and (ViewMode = vmText) or
    (FilePos + LongInt(BufPos) + LongInt(HexPos)*LongInt(Size.Y) > FileSize) and
    (ViewMode in [vmHex, vmDump]) then Exit;
 case ViewMode of
  vmHex,
  vmDump: begin
           while (BufPos + HexPos < BufSize) and (ANumber > 0) do
            begin Inc(BufPos, HexPos); Dec(ANumber) end;
           if BufPos + HexPos*Size.Y > BufSize then
             Seek(FilePos+LongInt(BufPos));
           while (ANumber > 0) do begin Inc(BufPos, HexPos); Dec(ANumber) end;
           DrawView;
           MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
           Exit;
          end;
{$IFDEF DisAsm}
  vmAsm: begin {GRM!}
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
          DrawView;
          Message(SB, evCommand, cmChangeValue, Pointer(FilePos+BufPos));
          Exit;
         end; {GRM!}
{$ENDIF DisAsm}
 end;
 repeat
  BufPos := Lines[ANumber].Pos;
  Dec(ANumber);
 until (ANumber = 0) or (BufPos >= 0);
 if ANumber+Size.Y+1 < MaxLines then
  begin
   Move(Lines[ANumber+1], Lines[0], sizeof(Lines[0])*(MaxLines-ANumber-1));
   Dec(MaxLines, ANumber+1);
  end else MakeLines;
 if (Lines[Size.Y-1].Pos = - 1) and (FilePos+BufSize < FileSize) then
    Seek(FilePos+Lines[0].Pos);
{$IFDEF DisAsm}
 if ViewMode=vmAsm then Cur.X:=10; {GRM!}
{$ENDIF}
 DrawView;
 MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
end;

procedure TFileViewer.CountUp;
 var I, J: LongInt;
     FP: LongInt;
     OBP: Word;
     a : Word;
{$IFDEF DisAsm}
     D: XchgData; {GRM!}
{$ENDIF}

 label 1;

 procedure ScrollUp;
  var I, D, K, E: Longint;
      P: Pointer;
      MaxX: Integer;
      TSt: Integer;
 begin
  TSt := StoI(ViewerDefaults.TabSize);
  if TSt = 0 then TSt := 8;
  if J <= 0 then Exit;
  I := J - 1;
  if Buf^[I] in [$0D, $0A] then begin
   Dec(I);
   if (I > 0) and (Buf^[I] in [$0D, $0A]) and (Buf^[I]<>Buf^[I+1]) then Dec(I);
   if I <= 0 then begin J := 0; exit end;
  end;
  E := I;
  while I >= 0 do if Buf^[I] in [$0D, $0A] then break else dec(I);
  begin
    J := I + 1;
    if Wrap>wmNone then MaxX := Size.X else MaxX := 255;
    while true do begin
        K := J;
        {Search for e.nd of line}
        I:=0;
        D:=0;
        while (I<MaxX-D) and (J+I<BufSize) do begin
         if Buf^[J+I]=9 then inc(D, TSt)
          else
           if (Buf^[J+I] in [$0D, $0A]) then begin
            if (J+I<BufSize-1) and (Buf^[J+I+1] in [$0D,$0A]) and
               (Buf^[J+I+1] <> Buf^[J+I]) then Inc(J);
            Inc(J);
            break;
           end;
         inc(I);
        end;
         {Search for e.nd of word-wrapped line}
        if (Wrap=wmWords) and (I>=MaxX-D) then begin
         D := I;
         While (I > 1) and not (Char(Buf^[K+I-1]) in BreakChars)
          do dec(I);
         If I <= 1 then I := D
          else J := K;
        end;
        Inc(J, I);
        If J > E then break;
    end;
    J := K;
   end;
 end;

begin
 if Buf = nil then Exit;
 if BufPos > 65000 then begin BufPos := 0; MakeLines; end;
1:
 J := BufPos; if (J < 0) and (FilePos <= 0) then Exit;
{$IFDEF DisAsm}
 D.MemBuff:=Buf; {GRM!}
 D.Offset:=BufPos; {GRM!}
{$ENDIF}
 for I := 1 to ANumber do begin
  {GRM!}
  case ViewMode of
   vmText: ScrollUp;
   vmHex,
   vmDump: Dec(J, HexPos);
{$IFDEF DisAsm}
   vmAsm: begin
           ScanUp(D);
           Dec(J, D.InstrLen);
          end;
{$ENDIF}
  end;
  {GRM!}
  if J <= 0 then if FilePos = 0 then
                  begin
                   if BufPos > 0 then begin BufPos := 0; MakeLines end;
                   BufPos := 0; DrawView; Exit
                  end
                 else
                  begin
                   FP := FilePos;
                   I := BufSize div 2;
                   if FilePos+BufPos < I then begin I := FP; FP := 0; end
                                         else FP := FP - I;
                   BufPos := BufPos + I;
                   OBP := BufPos;
                   FL^.Seek(FP);
                   Fl^.Read(Buf^, BufSize);
                   BufPos := OBP;
                   FilePos := FP;
                   MakeLines;
                   Goto 1;
                  end
           else BufPos := J;
  System.Move(Lines[0], Lines[1], (MaxLines - 1) * sizeof(Lines[0]));
  Lines[0].Pos := J;
  a := 0;
  while (j+a<Lines[1].Pos) and not (Buf^[J+A] in [10]) do Inc(a);
  {if a > $FF then asm nop end;}
  Lines[0].Len := a;
  if MaxLines < 200 then Inc(MaxLines);
 end;
 if Wrap>wmNone then MakeLines;
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
  TWindow.Init(R, VFileName, 0);
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
