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
  sysutils,
  Objects, Views, Dos, Drivers, HideView, Commands, DNHelp,
  {SysUtils,} {AK155}
  advance,
  advance1, advance2, advance3, highlite, U_KeyMap, Microed, xTime,
  Collect, Macro, advance6;

const
  MaxILines = 200; { максимальный индекс масива Lines }
  MaxWrapW = 1023;
    { максимальная ширина для заворачивания
    строк в режиме unwrap }

type

  { TViewScroll }

  PViewScroll = ^TViewScroll;
  TViewScroll = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    MaxV, Value: longInt;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetPartCode: longInt;
    procedure Draw; virtual;
    function GetSize: integer;
    procedure DrawPos(Pos: integer);
    end;

  { TFileViewer }

  PFileViewer = ^TFileViewer;
  TFileViewer = object(THideView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Filtr: boolean;
    NoEdit: boolean;
    FileName: String; {DataCompBoy}
    VFileName: String; {DataCompBoy}
    Buf: PByteArray;
    Fl: PStream;
    UpdateViewTmr: TEventTimer;
    XDelta, ViewMode, HexPos: AInt;
    SearchActive: boolean;
    SearchResultVisible: boolean; {AK155}
    SearchX: longInt;
    SB: PView;
    Wrap: byte; {DataCompBoy}
    Lines: array[0..MaxILines] of record
      Pos: longInt;
      len: word;
      end;
    FilePos, FileSize, NumLines: longInt;
    ExposedPos, ExposedLine: longInt; {AK155}
    Cur: TPoint;
    Info: PView;
    BufPos: longInt;
    BufSize, MaxLines: longInt;
    BufLines: AInt;
    KillAfterUse, isValid, QuickView, Loaded, HexEdit, BufModified:
      boolean;
    FakeKillAfterUse: boolean; {временная пустышка}
    Filter: byte;
    XLAT: TXlat;
    UseXLat: boolean;
    XLatFile: PString;
    KeyMap: TKeyMap;
    MarkPos: TPosArray;
    CtrlK: boolean;
    CtrlQ: boolean;
    HiLite: boolean; {PZ 2000.06.09}
    ScrollEOF: boolean; {AK155}
    HiLitePar: THighliteParams;
    Constructor Init(var Bounds: TRect; AStream: PStream;
    const AFileName, AVFileName: String;
    ASB: PView; Quick, Hex: boolean); {DataCompBoy}
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    destructor Done; virtual;
    procedure ShowView; virtual;
    procedure HideView; virtual;
    procedure Draw; virtual;
    procedure SetXlatFile(const aFName: String; DoRedraw: boolean);
    function ReadFile(const FName, VFName: String; NewStream:
      boolean): boolean; {DataCompBoy}
    procedure SetState(AState: word; Enable: boolean); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function WriteModify: boolean;
    procedure CountDown(ANumber: integer); virtual;
    procedure CountUp(ANumber: integer); virtual;
    procedure Seek(APos: longInt);
    procedure MakeLines; virtual;
    procedure SaveToFile(FN: String);
    function Valid(Command: word): boolean; virtual;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    function GetPalette: PPalette; virtual;
    procedure DoHighlite(var B; const s: String; const Attr: String);
    procedure UpDate; virtual; {AK155}
    procedure SeekEof;
    procedure SeekBof;
    function BreakOnStreamReadError: boolean;
    private
    procedure AdjustBuf;
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
    Constructor Init(var R: TRect; AViewer: PFileViewer);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PFileWindow = ^TFileWindow;
  TFileWindow = object( {TStd}TWindow)
    Constructor Init(const FileName, VFileName: String; Hex: boolean);
      {DataCompBoy}
    function GetPalette: PPalette; virtual;
    function ReactOnCmd: boolean; virtual;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    end;

const
  CHViewer = #13#14#117#118#199#200#201#202#203#204;
  CViewer = #6#7#8#9#10#11#12#13;
  CViewWindow = #112#113#114#115#116#117#118#199#200#201#202#203#204;

type
  TViewSearch = record
    What: String[250];
    Opts: word;
    Dir: word
    end;

const
  cmChangeValue = 9990;
  SearchString: TViewSearch = (What: ''; Opts: 0; Dir: 0);

  {Cat: порядок переменных не менять, сохраняются подряд в DSK-файл}

var
  LastViewerBounds: TRect;
  LastViewerDeskSize: TPoint {= (X:0;Y:0)};
  LastEditDeskSize: TPoint {= (X:0;Y:0)};

function SearchFileStr(F: PStream; var XLAT: TXlat;
  const What: String; Pos: longInt;
  CaseSensitive, Display, WholeWords, Back,
  AllCP, IsRegExp: boolean): longInt;

type
  eFileError = class(Exception)
    public
    rc: longInt;
    end;

implementation

uses
  Lfn, {DataCompBoy}
  Memory, Messages, DNApp, Startup, Dialogs, RStrings,
  {$IFDEF DisAsm}Decoder, {$ENDIF} {piwamoto}
  {$IFDEF RegExp}RegExp, {$ENDIF} {Cat}
  ErrMess, {AK155}
  DiskInfo,
  Gauge, DNStdDlg, Histries, Drives, DNUtil, DnIni;

function MaxAvail: longInt;
  begin
    MaxAvail := MemAdjust(System.MaxAvail);
  end;

const
  ViewerBufSize = (1+MaxILines)*(MaxWrapW+2);

function SearchFileStr;
  label 1, LExit;
  var
    Buf: PByteArray;
    Count: longInt;
    BufLen, BufPos, BPos, t, StPos, StDelta: longInt;
    i, l, j, OldPos, NextPos: longInt;
    CancelSearch: boolean;
    Info: PWhileView;
    R: TRect;
    tmr: TEventTimer;
    Inserted: boolean;
    W: String;
    {$IFDEF REGEXP}
    RegExp: PRegExp;
    {$ENDIF}
  begin
    if IsRegExp then
      begin
        MessageBox('RegExp Search is not implemented yet', nil,
          mfError+mfOKButton);
        exit;
      end;
    Info := nil;
    SearchFileStr := -1;
    Count := 0;
    ClrIO;
    OldPos := F^.GetPos;
    l := F^.GetSize;
    if l = 0 then
      goto LExit;
    F^.Seek(Pos);
    BufLen := $80000;
    {Cat: тут лучше с памятью не мелочиться, иначе сильно проигрываем в скорости}
    if BufLen > l then
      BufLen := l;
    if BufLen > MaxAvail then
      BufLen := MaxAvail;
    if BufLen < Length(What) then
      begin
        F^.Seek(OldPos);
        goto LExit;
      end;
    t := 0;
    StPos := 0;
    StDelta := 0;
    if Display then
      begin
        R.Assign(1, 1, 30, 9);
        New(Info, Init(R));
        Info^.Top := GetString(dlSearching)+' "'+Cut(SearchString.
          What, 40)+'"';
        NewTimer(tmr, 2);
        Inserted := False;
      end;
    GetMem(Buf, BufLen);
    i := Pos;
    BPos := 0;
    CancelSearch := False;
    repeat
      if Back then
        begin
          NextPos := i-BufLen+BPos;
          if NextPos < 0 then
            begin
              NextPos := 0;
              BPos := 0;
            end;
          j := i-NextPos;
          if j <= 0 then
            break;
        end
      else
        begin
          NextPos := F^.GetPos;
          j := l-i+BPos;
          if j > BufLen then
            j := BufLen;
          ClrIO;
        end;
      if Display then
        begin
          Info^.Write(1, StrGrd(l, i, 30, False));
          Info^.Write(2, ZtoS(Percent(l, i))+'%');
          if (not Inserted) and TimerExpired(tmr) then
            begin
              Desktop^.Insert(Info);
              Inserted := True;
            end;
          if Inserted then
            DispatchEvents(Info, CancelSearch);
        end;
      F^.Seek(NextPos);
      F^.Read(Buf^[BPos], j-BPos);
      t := j-BPos;
1:
      if j <= 0 then
        BufPos := 0
      else
        begin{-$VIV 14.05.99}
          if AllCP then
            begin
              if Back then
                BufPos := BackSearchForAllCP(What, Buf^[StPos], j-
                  StPos, CaseSensitive)
              else
                BufPos := SearchForAllCP(What, Buf^[StPos], j-StPos,
                  CaseSensitive);
            end
          else
            begin
              W := What;
              XLatBuf(W[1], Length(W), XLAT);
              if Back then
                BufPos := BackSearchFor(W, Buf^[StPos], j-StPos,
                  CaseSensitive)
              else
                BufPos := SearchFor(W, Buf^[StPos], j-StPos,
                  CaseSensitive);
            end;
        end; {-$VIV ::}
      if BufPos > 0 then
        begin
          if WholeWords and not (((BufPos = 1) or (Char(Buf^[BufPos-2+
              StPos]) in BreakChars)) and
            (Char(Buf^[BufPos+Length(What)-1+StPos]) in BreakChars))
          then
            begin
              if Back then
                j := BufPos+Length(What)-2
              else
                Inc(StPos, BufPos);
              goto 1;
            end;
          FreeMem(Buf, BufLen);
          F^.Seek(OldPos);
          SearchFileStr := NextPos-longInt(BPos)+longInt(BufPos)-1+
            StPos;
          goto LExit;
        end;
      if Back then
        begin
          Dec(i, t);
          if i < -t then
            break;
          if i < 0 then
            i := 0;
        end
      else
        Inc(i, t);
      StPos := 1;
      BPos := Length(What)+1;
      if i < l then
        Move(Buf^[j-BPos], Buf^[0], BPos);
    until (Back and (i < 0)) or
    (not Back and ((i >= l) or (t <= 0) or (F^.GetPos >= F^.GetSize)))
      or
    CancelSearch;
    if CancelSearch then
      SearchFileStr := -2;
    FreeMem(Buf, BufLen);
    F^.Seek(OldPos);
LExit:
    if Info <> nil then
      Dispose(Info, Done);
    Info := nil;
  end { SearchFileStr };

Constructor TViewInfo.Init(var R: TRect; AViewer: PFileViewer);
  begin
    inherited Init(R);
    Viewer := AViewer;
    GrowMode := gfGrowHiY+gfGrowLoY+gfGrowHiX;
    EventMask := evMouse;
  end;

Constructor TViewInfo.Load(var s: TStream);
  begin
    inherited Load(s);
    GetPeerViewPtr(s, Viewer);
  end;

procedure TViewInfo.Store(var s: TStream);
  begin
    inherited Store(s);
    PutPeerViewPtr(s, Viewer);
  end;

procedure TViewInfo.Draw;
  var
    B: TDrawBuffer;
    s: String;
    i, j: integer;
    Ch, Ch2: Char;
    Color: word;
  begin
    if (Viewer = nil) or
      (Owner = nil) or
      (PWindow(Owner)^.Frame = nil)
    then
      begin
        inherited Draw;
        exit;
      end;
    with Viewer^, Self do
      begin
        Color := PWindow(Owner)^.Frame^.GetColor(3);
        Ch2 := #205;
        if not Owner^.GetState(sfActive) then
          begin
            Color := PWindow(Owner)^.Frame^.GetColor(1);
            MoveChar(B, #196, Color, Size.X);
            WriteLine(0, 0, Size.X, 1, B);
            exit;
          end
        else if Owner^.GetState(sfDragging) then
          begin
            Color := PWindow(Owner)^.Frame^.GetColor(5);
            Ch2 := #196;
          end;
        if ViewMode = vmHex then
          begin
            s := Hex8(Cur.X div (1+byte(HexEdit))+
            Cur.Y*HexPos+BufPos+FilePos)+' ';
            j := (FilePos+BufPos) mod Max(1, HexPos);
            for i := 0 to HexPos-1 do
              s := s+Hex2(i+j)+#196;
            SetLength(s, Length(s)-1);
            Insert('[', s, 1);
            Insert(']', s, Length(s)+1);
          end
        else if ViewMode = vmAsm then
          begin
            j := 0;
            for i := 0 to(Cur.Y-1) do
              Inc(j, Lines[i].len);
            s := '['+Hex8(((Cur.X-10) div 2)+j+BufPos+FilePos)+']';
          end
        else
          case Wrap of
            wmNone:
              s := Ch2+'<=>';
            wmWidth:
              s := Ch2+'>=<';
            wmWords:
              s := Ch2+'>W<';
          end {case};
        if ViewMode <> vmHex then
          begin
            s := s+Ch2+Ch2+'<';
            for i := 1 to 9 do
              if not MarkPos[i].EqualsXY(-1, -1) then
                s := s+Char(i+48)
              else
                s := s+#250;
            s := s+'>';
          end;
        Insert(Ch2+Ch2, s, Length(s)+1);
        if ViewMode <> vmAsm then
          begin
            if UseXLat then
              begin
                Insert(AddSpace(Cut(XLatFile^, 8), 8), s, Length(s)+1);
              end
            else
              s := s+KeyMapDescriptor[KeyMap]+'..   ';
            case Filter of
              0:
                Insert('{00-255}'+Ch2, s, Length(s)+1);
              1:
                Insert('{32-127}'+Ch2, s, Length(s)+1);
              else
                Insert('{32-255}'+Ch2, s, Length(s)+1);
            end {case};
          end;
        if ViewMode <> vmHex then
          begin
            s := s+Ch2;
            if (Lines[Size.Y].Pos < 0) and (FilePos+BufSize >=
                FileSize)
              or (FileSize = 0)
            then
              s := s+'100'
            else
              s := s+Long2Str(Round(Percent(FileSize, FilePos+
                ExposedPos)), 3);
            s := s+'% of '+FStr(FileSize)+' Bytes'+Ch2;
          end;
      end;
    MoveChar(B, Ch2, Color, Size.X);
    MoveStr(B, s, Color);
    Self.WriteLine(0, 0, Size.X, Size.Y, B);
  end { TViewInfo.Draw };

procedure TViewInfo.HandleEvent;
  var
    P: TPoint;
    BookMark: byte;
    EV: TEvent;
  begin
    if Viewer = nil then
      exit;
    if (Event.What = evMouseDown) then
      with Viewer^ do
        begin
          Self.MakeLocal(Event.Where, P);
          if (ViewMode = vmHex) then
            begin
              if P.X < 3*HexPos+9+3 then{}
              else if P.X < 3*HexPos+9+12 then
                Message(Viewer, evCommand, cmSwitchKeyMapping, nil)
              else if P.X < 3*HexPos+9+20 then
                Message(Viewer, evCommand, cmAddFilter, nil);
            end
          else if (ViewMode = vmAsm) then
            begin
              if P.X < 12 then{}
              else if P.X = 12 then
                SeekBof
              else if P.X < 22 then
                begin
                  BookMark := P.X-13;
                  if (Event.Buttons and mbRightButton <> 0) or
                    (MarkPos[BookMark+1].Y <> -1)
                  then
                    with EV do
                      begin
                        What := evCommand;
                        if (Event.Buttons and mbRightButton <> 0)
                        then
                          Command := cmPlaceMarker1+BookMark
                        else
                          Command := cmGoToMarker1+BookMark;
                        PutEvent(EV);
                      end;
                end
              else if P.X = 22 then
                SeekEof;
            end
          else
            begin
              if P.X < 1 then{}
              else if P.X < 4 then
                Message(Viewer, evCommand, cmUnWrap, nil)
              else if P.X < 6 then{}
              else if P.X = 6 then
                SeekBof
              else if P.X < 16 then
                begin
                  BookMark := P.X-7;
                  if (Event.Buttons and mbRightButton <> 0) or
                    (MarkPos[BookMark+1].Y <> -1)
                  then
                    with EV do
                      begin
                        What := evCommand;
                        if (Event.Buttons and mbRightButton <> 0)
                        then
                          Command := cmPlaceMarker1+BookMark
                        else
                          Command := cmGoToMarker1+BookMark;
                        PutEvent(EV);
                      end;
                end
              else if P.X = 16 then
                SeekEof
              else if P.X < 19 then{}
              else if P.X < 28 then
                Message(Viewer, evCommand, cmSwitchKeyMapping, nil)
              else if P.X < 36 then
                Message(Viewer, evCommand, cmAddFilter, nil);
            end;
          ClearEvent(Event);
        end;
  end { TViewInfo.HandleEvent };

function TFileViewer.WriteModify;
  var
    B: boolean;
    i: longInt;
    s: TDOSStream;
    A: word;
  begin
    B := BufModified;
    WriteModify := False;
    if (Buf = nil) or (Fl = nil) or (Fl^.Status <> stOK) or not B
    then
      exit;
    i := MessageBox(GetString(dlViewQuery), nil, mfWarning+
      mfYesNoCancel);
    WriteModify := i = cmCancel;
    if i <> cmCancel then
      BufModified := False;
    if i = cmNo then
      begin
        Seek(FilePos);
        DrawView;
      end;
    if i <> cmYes then
      exit;
    if VFileName = '' then
      begin
        FreeStr := GetFileNameDialog(x_x, GetString(dlSaveFileAs),
        GetString(dlSaveFileAsName),
        fdOKButton+fdHelpButton, hsEditSave);
        if FreeStr <> '' then
          begin
            SaveToFile(FreeStr);
            FileName := FreeStr;
            VFileName := FileName;
            WriteModify := True;
            Owner^.Redraw;
          end
        else
          exit;
      end;
    if (TypeOf(Fl^) = TypeOf(TDOSStream)) or (TypeOf(Fl^) = TypeOf(
        TBufStream))
    then
      begin
        Dispose(Fl, Done);
        ClrIO;
        NeedAbort := True;
        A := 0;
        Fl := New(PDosStream, Init(FileName, stOpen));
        if not Abort and (Fl^.Status <> stOK) then
          begin
            Dispose(Fl, Done);
            A := GetFileAttr(FileName);
            SetFileAttr(FileName, A and not ReadOnly);
            Fl := New(PDosStream, Init(FileName, stOpen));
          end;
        if (Fl^.Status <> stOK) or (Abort) then
          begin
            Dispose(Fl, Done);
            Fl := New(PDosStream, Init(FileName, stOpenRead));
            MessageBox(GetString(dlFBBNoWrite)+FileName, nil,
              mfError+mfOKButton);
          end
        else
          begin
            Fl^.Seek(FilePos);
            Fl^.Write(Buf^, BufSize);
          end;
        if A <> 0 then
          begin
            Dispose(Fl, Done);
            SetFileAttr(FileName, A);
            Fl := New(PDosStream, Init(FileName, stOpenRead));
          end;
      end
    else
      begin
        Fl^.Seek(FilePos);
        Fl^.Write(Buf^, BufSize);
      end;
    NeedAbort := False;
    if not DnIni.AutoRefreshPanels then
      RereadDirectory(GetPath(FileName));
  end { TFileViewer.WriteModify };

const
  SCViewer: String[Length(CViewer)] = CViewer;
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

function TViewScroll.GetSize: integer;
  var
    s: integer;
  begin
    if Size.X = 1 then
      s := Size.Y
    else
      s := Size.X;

    if s < 3 then
      GetSize := 3
    else
      GetSize := s;
  end;

procedure TViewScroll.DrawPos(Pos: integer);
  var
    s: integer;
    B: TDrawBuffer;
  const
    Chars: TScrollChars = (#30, #31, #177, #254, #178);
  begin
    s := GetSize-1;
    MoveChar(B[0], Chars[0], GetColor(2), 1);
    { if Max = Min then
    MoveChar(B[1], Chars[4], GetColor(1), S - 1)
  else    }
    begin
      MoveChar(B[1], Chars[2], GetColor(1), s-1);
      MoveChar(B[Pos], Chars[3], GetColor(3), 1);
    end;
    MoveChar(B[s], Chars[1], GetColor(2), 1);
    WriteBuf(0, 0, Size.X, Size.Y, B);
  end;

procedure TViewScroll.HandleEvent;
  var
    B: boolean;
    P: TPoint;
    RD, Sp: longInt;

    extent: TRect;
    Tracking: boolean;

    {JO}
  function GetPrecValue: longInt;
    var
      Max1: Double; {JO}
    begin
      Max1 := MaxV;
      GetPrecValue := Trunc(Max1*(Sp-1)/(Size.Y-3))+1;
    end;
  {/JO}

  begin
    TView.HandleEvent(Event);
    case Event.What of
      evCommand:
        case Event.Command of
          cmChangeValue:
            begin
              Value := Event.InfoLong;
              DrawView;
              ClearEvent(Event)
            end;
          {              cmSetMargins: Message(Owner, evCommand, cmGotoLineNumber, Pointer(Value));}
        end {case};
      evMouseDown:
        if MaxV > 0 then
          begin

            MakeLocal(Event.Where, P);
            GetExtent(extent);
            {if (Value * (Size.Y-3)) div MaxV + 1 = P.Y then}
            if GetPartCode = P.Y then{JO}
              begin
                RD := P.Y;
                repeat
                  MakeLocal(Event.Where, P);
                  Tracking := extent.Contains(P);
                  if Tracking then
                    begin
                      {  if Size.X = 1 then}Sp := P.Y
                        {else I := P.X};
                      if Sp <= 0 then
                        Sp := 1;
                      if Sp >= Size.Y-1 then
                        Sp := Size.Y-2;
                    end; { else I := GetPos;}
                  if Sp <> RD then
                    begin
                      DrawPos(Sp);
                      RD := Sp;
                    end;
                until not MouseEvent(Event, evMouseMove+evMouseAuto);
                if Sp = 1 then
                  Value := 0
                else
                  {Value := MaxV * (SP-1) div (Size.Y-3) + 1;}
                  {JO: точности вычисления с Longint для очень больших файлов недостаточно}
                  Value := GetPrecValue; {JO}
                {AK155: скроллер уже нарисован и еще дважды будет нарисован }
                {                DrawView; }
                MessageL(Owner, evCommand, cmScrollBarChanged, Value);
                exit;
              end;

            MakeLocal(Event.Where, P);
            RD := RepeatDelay;
            RepeatDelay := 0;
            Sp := GetPartCode;
            if P.Y = 0 then
              begin
                repeat
                  MakeLocal(Event.Where, P);
                  if (P.X >= -1) and (P.X <= 1) and (P.Y >= -1) and (P.Y
                      <= 1)
                  then
                    Message(Owner, evKeyDown, kbUp, nil);
                until not MouseEvent(Event, evMouseMove+evMouseAuto);
              end
            else if P.Y = Size.Y-1 then
              begin
                repeat
                  MakeLocal(Event.Where, P);
                  if (P.X >= -1) and (P.X <= 1) and (P.Y >= Size.Y-2)
                      and (P.Y <= Size.Y)
                  then
                    Message(Owner, evKeyDown, kbDown, nil);
                until not MouseEvent(Event, evMouseMove+evMouseAuto);
              end
            else if P.Y < Sp then
              begin
                RepeatDelay := RD;
                repeat
                  GetPartCode;
                  MakeLocal(Event.Where, P);
                  if (P.X >= -1) and (P.X <= 1) and (P.Y < Sp) then
                    Message(Owner, evKeyDown, kbPgUp, nil);
                until not MouseEvent(Event, evMouseMove+evMouseAuto);
              end
            else if P.Y > Sp then
              begin
                RepeatDelay := RD;
                repeat
                  GetPartCode;
                  MakeLocal(Event.Where, P);
                  if (P.X >= -1) and (P.X <= 1) and (P.Y > Sp) then
                    Message(Owner, evKeyDown, kbPgDn, nil);
                until not MouseEvent(Event, evMouseMove+evMouseAuto);
              end;
            RepeatDelay := RD;
          end;
    end {case};
  end { TViewScroll.HandleEvent };

function TViewScroll.GetPartCode: longInt;
  var
    Val1, Max1: Double; {JO}
  begin
    if Value >= MaxV then
      GetPartCode := Size.Y-2
    else
      {JO}
      begin
        Val1 := Value;
        Max1 := MaxV;
        {GetPartCode := 1+((Size.Y-3)*Value) div MaxV}
        {JO: точности вычисления с Longint для очень больших файлов недостаточно}
        GetPartCode := 1+Trunc(((Size.Y-3)*Val1)/Max1);
      end;
    {/JO}
  end;

function TViewScroll.GetPalette;
  const
    s: String[Length(CScrollBar)] = CScrollBar;
  begin
    GetPalette := @S;
  end;

procedure TViewScroll.Draw;
  var
    B1: TDrawBuffer;
    B: array[0..128] of record
      C: Char;
      B: byte;
      end absolute B1;
    C1, C2: byte;
    i: integer;
  begin
    C1 := GetColor(1);
    C2 := GetColor(2);
    MoveChar(B, #177, C1, Size.Y);
    i := GetPartCode;
    B1[i] := 254+longInt(C2*256);
    B1[0] := 30+longInt(C2*256);
    B1[Size.Y-1] := 31+longInt(C2*256);
    WriteBuf(0, 0, 1, Size.Y, B);
  end;

{ THFileViewer }

procedure THFileViewer.ChangeBounds;
  var
    R: TRect;
  begin
    SetBounds(Bounds);
    if (Wrap > wmNone) or (ViewMode = vmDump) then
      MakeLines;
    if SB <> nil then
      begin
        R := Bounds;
        R.A.X := R.B.X;
        Inc(R.B.X);
        SB^.SetBounds(R)
      end
  end;

{ TFileViewer }

procedure TFileViewer.ChangeBounds;
  var
    R: TRect;
  begin
    inherited ChangeBounds(Bounds);
    if (Wrap > wmNone) or (ViewMode = vmDump) then
      MakeLines;
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
    NewBufSize: longInt;
  begin
    NewBufSize := ViewerBufSize;
    if NewBufSize > FileSize then
      NewBufSize := FileSize;
    if (Buf <> nil) and (NewBufSize = BufSize) then
      exit;
    if Buf <> nil then
      FreeMem(Buf, BufSize);
    if MaxAvail < NewBufSize then
      NewBufSize := MaxAvail;
    BufSize := NewBufSize;
    GetMem(Buf, BufSize);
  end;

{AK155
   Автодочитывание файла, если на экране виден его конец }
procedure TFileViewer.UpDate;
  begin
    inherited UpDate;
    if ((EditorDefaults.ViOpt and vbfAutoscroll) <> 0) and
      not BufModified and
      not SearchResultVisible and
      (ViewMode = vmText) and
      (ExposedPos+FilePos = FileSize) and
      TimerExpired(UpdateViewTmr)
    then
      begin
        SeekEof;
        NewTimer(UpdateViewTmr, 9); { обновлять 2 раза в секунду }
      end;
  end;

Constructor TFileViewer.Init;
  var
    C: Char;
  begin
    inherited Init(Bounds);
    ViewMode := vmText;
    XLatFile := nil;
    NullXLAT(XLAT);
    UseXLat := False;
    KeyMap := kmNone;
    FillChar(MarkPos, SizeOf(MarkPos), $FF);
    HelpCtx := hcView;
    GrowMode := gfGrowHiX+gfGrowHiY;
    Options := Options or ofSelectable or (longInt(Quick)*
      ofTopSelect);
    EventMask := $FFFF;
    SB := ASB;
    QuickView := Quick;
    Buf := nil;
    isValid := True;
    Loaded := False;
    KillAfterUse := False;
    Wrap := 0;
    if EditorDefaults.ViOpt and vbfWrap <> 0 then
      Wrap := 1;
    if EditorDefaults.ViOpt and vbfWordWrap <> 0 then
      Wrap := 2;
    ViewMode := {EditorDefaults.ViOpt and}byte(Hex);
    Fl := AStream;
    if (AFileName = '') and (Fl = nil) then
      exit;
    TempFile := '';
    try
      if Fl = nil then
        ReadFile(AFileName, AVFileName, True)
      else if Fl^.Status = stOK then
        ReadFile(' ', ' ', False);
    except
      on E: eFileError do
        exit;
    end;
    {AK155 Это для автодочитывания файла с изменяющейся длиной,
    см. Update }
    NewTimer(UpdateViewTmr, 9);
    RegisterToBackground(@Self);
    {/AK155}
  end { TFileViewer.Init };

destructor TFileViewer.Done;
  begin
    EnableCommands([cmUnWrap]);
    if Buf <> nil then
      FreeMem(Buf, BufSize);
    Buf := nil;
    if Fl <> nil then
      Dispose(Fl, Done);
    Fl := nil;
    if not (TottalExit or Exiting) and KillAfterUse then
      begin
        EraseTempFile(FileName);
        KillAfterUse := False;
        if not DnIni.AutoRefreshPanels then
          RereadDirectory(GetPath(FileName));
      end;
    Info := nil;
    inherited Done;
  end;

Constructor TFileViewer.Load;
  var
    i: longInt;
  begin
    inherited Load(s);
    NoEdit := False;
    BufModified := False;
    GetPeerViewPtr(s, SB);
    GetPeerViewPtr(s, Info);
    s.ReadStrV(FileName);
      {S.Read(FileName[0], 1); S.Read(FileName[1], Length(FileName));}
    s.ReadStrV(VFileName);
      {S.Read(VFileName[0], 1); S.Read(VFileName[1], Length(VFileName));}
    s.Read(i, SizeOf(longInt));
    s.Read(QuickView, 1);
    s.Read(Wrap, 1);
    {???}
    s.Read(FakeKillAfterUse, 1);
    KillAfterUse := False;
    s.Read(Filter, 1);
    s.Read(ViewMode, 2);
    XLatFile := s.ReadStr;
    s.Read(XLAT, SizeOf(XLAT));
    s.Read(UseXLat, SizeOf(UseXLat));
    s.Read(KeyMap, SizeOf(KeyMap));
    s.Read(MarkPos, SizeOf(MarkPos));
    Fl := nil;
    Buf := nil;
    isValid := True;
    Loaded := True;
    if (FileName = '') and (VFileName = '') then
      isValid := False
    else
      begin
        try
          ReadFile(FileName, VFileName, True);
          Seek(i);
          isValid := True;
        except
          on E: eFileError do
            ;
            {AK155 так бывает, например, при перезапуске DN
       с запомненным просмотром на отсутствующей дискете }
        end;
      end;
  end { TFileViewer.Load };

procedure TFileViewer.Store;
  var
    i: longInt;
  begin
    inherited Store(s);
    PutPeerViewPtr(s, SB);
    PutPeerViewPtr(s, Info);
    s.WriteStr(@FileName);
      {S.Write(FileName[0], 1 + Length(FileName));}
    s.WriteStr(@VFileName);
      {S.Write(VFileName[0], 1 + Length(VFileName));}
    i := FilePos+longInt(BufPos);
    s.Write(i, SizeOf(longInt));
    s.Write(QuickView, 1);
    s.Write(Wrap, 1);
    s.Write(FakeKillAfterUse, 1);
    s.Write(Filter, 1);
    s.Write(ViewMode, 2);
    s.WriteStr(XLatFile);
    s.Write(XLAT, SizeOf(XLAT));
    s.Write(UseXLat, SizeOf(UseXLat));
    s.Write(KeyMap, SizeOf(KeyMap));
    s.Write(MarkPos, SizeOf(MarkPos));
  end { TFileViewer.Store };

procedure XDumpStr(var s: String; var B; Addr: longInt; Count:
    integer; Filter: byte);
  begin
    s := Hex8(Addr)+' ';
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
  end
      ;
  end;

{ размер поля табуляции }
function GetTabSize: word;
  var
    TS: word;
  begin
    GetTabSize := 1;
    if (EditorDefaults.EdOpt {$IFNDEF OS_DOS} shl 2 {$ENDIF} and
        ebfTRp) <> 0
    then
      begin
        TS := StoI(EditorDefaults.TabSize);
        if TS = 0 then
          GetTabSize := 8
        else if TS > 100 then
          GetTabSize := 100
        else
          GetTabSize := TS;
      end
  end;

procedure TFileViewer.Draw;
  var
    B: TDrawBuffer;
    C: byte;
    i, k, M, Src: integer;
    j: longInt;
    l: longInt;
    s, Sz: String;
    IZ: byte;
    TS: word;
    C1: byte;
    C2: Char;
    HP: String[6];
    CC: array[1..8] of byte;
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
    W, W2, W3: integer;
    WDH: integer;
  begin { TFileViewer.Draw }
    ExposedPos := Lines[0].Pos;
    for i := 1 to 8 do
      CC[i] := GetColor(i);
    TS := GetTabSize;
    ExpandTabs := (EditorDefaults.EdOpt {$IFNDEF OS_DOS} shl 2
      {$ENDIF} and ebfTRp) <> 0;
    if (Buf = nil) and not QuickView and
      not (VFileName = ' ')
        {AK155 11-01-2004
      бывает в просмотре dbf при просмотре memo, когда memo-файл то ли
      пустой, то ли битый. VFileName = ' ' - это внешний поток и при
      нормальной работе , вроде, не должно быть Buf = nil}
    then
      begin
        ReadFile(FileName, VFileName, True);
        BufPos := 0;
        MakeLines;
      end
      (* {AK155 11-01-2003 эти бредовые проверки, вероятно, когда-то
        служили для компенсации  последствий каких-то багов.
        Но после того, как BufSize стал не $8000, а поболее, они
        стали в прежнем виде вредны, а обновлять их не хочется,
        так как самих багов, наверно, уже давно нет}
  else
  if (BufPos < 0) or (BufPos > BufSize + 20000) or
     (BufPos > 65000) then
  begin
    BufPos:=0;
    MakeLines;
  end *)
        ;
    C := CC[1];
    {AK155  if Info <> nil then Info^.DrawView;}
    if {(FileName = '') or }(Buf = nil) then
      begin
        HideCursor;
        inherited Draw;
        exit;
      end;
    SearchResultVisible := SearchActive;
    case ViewMode of
      vmHex:
        begin
          DisableCommands([cmUnWrap]);
          HexPos := (Size.X-12) div 4;
          if HexPos <= 0 then
            HexPos := 1;
          XDelta := 0;
          Src := 0;
          if not ScrollEOF then{AK155}
            begin
              while (longInt(Cur.Y*HexPos)+BufPos+FilePos >=
                  FileSize)
              do
                Dec(Cur.Y);
              while (longInt(Cur.Y*HexPos)+BufPos+FilePos+Cur.X
                div (byte(HexEdit)+1) >= FileSize)
              do
                Dec(Cur.X);
            end;
          if Cur.Y < 0 then
            begin
              W := -Cur.Y;
              Cur.Y := 0;
              CountUp(W);
            end
          else if Cur.Y >= Size.Y then
            begin
              W := Cur.Y-Size.Y+1;
              Dec(Cur.Y, W);
              CountDown(W);
            end;
          W := BufPos;
          l := FilePos+longInt(BufPos);
          if HexEdit
          then
            SetCursor(10+(Cur.X div 2)*3+Cur.X and 1, Cur.Y)
          else
            SetCursor(12+HexPos*3+Cur.X, Cur.Y);
          ShowCursor;
          for i := 0 to Size.Y-1 do
            begin
              {MoveChar(B[XDelta], ' ', C, Size.X);} {???}
              if Size.X+XDelta > 255 then
                begin
                  WDH := 255-XDelta;
                  if WDH < 0 then
                    WDH := 0;
                end
              else
                WDH := Size.X;
              MoveChar(B[XDelta], ' ', C, WDH);
              if l+longInt(HexPos) > FileSize
              then
                j := FileSize-l
              else
                j := HexPos;
              if j > 0 then
                begin
                  s := DumpStr(Buf^[W], l, j, Filter);
                  W2 := Pos(#179, s);
                  SetLength(Sz, Length(s)-W2);
                  for IZ := 1 to Length(s)-W2 do
                    if s[W2+IZ] = #0 then
                      begin
                        Sz[IZ] := #1;
                        s[W2+IZ] := '.';
                      end
                    else
                      Sz[IZ] := #0;
                  XLatBuf(s[W2+1], Length(s)-W2, XLAT);
                  {-DataCompBoy & Axel: apply filter-}
                  case Filter of
                    0:
                      ;
                    1:
                      for W3 := W2+1 to Length(s) do
                        if not (byte(s[W3]) in [32..127]) then
                          s[W3] := #250;
                    else
                      for W3 := W2+1 to Length(s) do
                        if not (byte(s[W3]) in [32..255]) then
                          s[W3] := #250;
                  end {case};
                  {-DataCompBoy & Axel: done-}
                  if j = HexPos then
                    Drivers.MoveStr(B, s, C)
                  else
                    begin
                      Drivers.MoveStr(B, Copy(s, 1, j*3+10), C);
                      Drivers.MoveStr(B[10+HexPos*3], Copy(s, j*3+11,
                        MaxStringLength), C);
                    end;
                  for IZ := 1 to Length(Sz) do
                    if Sz[IZ] = #1 then
                      MoveColor(B[W2+IZ-1], 1, (CC[1] and $F0)+(CC[3]
                        and $0F));
                  if SearchActive then
                    begin
                      if (l <= SearchX) and (l+j > SearchX) then
                          begin
                          Cur.Y := i;
                          Cur.X := (SearchX-l)*2;
                          if HexEdit
                          then
                            SetCursor(10+(Cur.X div 2)*3+Cur.X and 1,
                              Cur.Y)
                          else
                            SetCursor(12+HexPos*3+Cur.X, Cur.Y);
                          ShowCursor;
                          Src := Min(l+j-SearchX, Length(
                            SearchString.What));
                          MoveColor(B[10+(SearchX-l)*3], Src*3-1, CC[2])
                            ;
                          MoveColor(B[12+HexPos*3+SearchX-l], Src, CC[2]
                            );
                          Src := Length(SearchString.What)-Src;
                        end
                      else if Src > 0 then
                        begin
                          k := Min(Src, j);
                          MoveColor(B[10], k*3-1, CC[2]);
                          MoveColor(B[12+HexPos*3], k, CC[2]);
                          Dec(Src, j);
                        end;
                    end
                  else{+piwamoto} if i = Cur.Y then
                    begin
                      k := Cur.X div (1+byte(HexEdit));
                      MoveColor(B[10+k*3], 2, CC[2]);
                      MoveColor(B[12+HexPos*3+k], 1, CC[2]);
                    end; {-piwamoto}
                  Inc(W, j);
                  Inc(l, longInt(j));
                end;
              {WriteLine(0, I, Size.X, 1, B[XDelta]);} {???}
              WriteLine(0, i, WDH, 1, B[XDelta]);
            end;
        end;
      vmDump:
        begin
          DisableCommands([cmUnWrap]);
          HideCursor;
          HexPos := ((Size.X-9) div 16)*16;
          if HexPos < 16 then
            HexPos := 16;
          W := BufPos;
          l := FilePos+longInt(BufPos);
          Src := 0;
          for i := 0 to Size.Y-1 do
            begin
              {MoveChar(B[XDelta], ' ', C, Size.X);}
              if Size.X+XDelta > 255 then
                begin
                  WDH := 255-XDelta;
                  if WDH < 0 then
                    WDH := 0;
                end
              else
                WDH := Size.X;
              MoveChar(B[XDelta], ' ', C, WDH);
              if l+longInt(HexPos) > FileSize
              then
                j := FileSize-l
              else
                j := HexPos;
              if j > 0 then
                begin
                  XDumpStr(s, Buf^[W], l, j, Filter);
                  XLatBuf(s[10], Length(s)-9, XLAT);
                  Drivers.MoveStr(B, s, C);
                  if SearchActive then
                    begin
                      if (l <= SearchX) and (l+j > SearchX) then
                          begin
                          Src := Min(l+j-SearchX, Length(
                            SearchString.What));
                          MoveColor(B[9+SearchX-l], Src, CC[2]);
                          Src := Length(SearchString.What)-Src;
                        end
                      else if Src > 0 then
                        begin
                          MoveColor(B[9], Min(Src, j), CC[2]);
                          Dec(Src, j);
                        end;
                    end;
                  Inc(W, j);
                  Inc(l, longInt(j));
                end;
              WriteLine(0, i, WDH, 1, B[XDelta]);
            end;
          {$IFNDEF DisAsm}
        end
          {$ELSE}
        end;
      {GRM!}
      vmAsm: begin
        DisableCommands([cmUnWrap]);
        XDelta := 0;
        l := FilePos+longInt(BufPos);
        D.MemBuff := Buf;
        D.Offset := BufPos;
        XDelta := 0;

        SetCursor(Cur.X, Cur.Y);
        ShowCursor;
        for i := 0 to Size.Y-1 do
          begin
            if Size.X+XDelta > 255 then
              begin
                WDH := 255-XDelta;
                if WDH < 0 then
                  WDH := 0;
              end
            else
              WDH := Size.X;
            MoveChar(B[XDelta], ' ', C, WDH);
            if l < FileSize-1 then
              begin
                if Cur.Y = i then
                  HexPos := l-FilePos-BufPos+((Cur.X-10) div 2);
                D.InstrMaxLen := FileSize-l;
                {Draw - doesn't matter}
                ScanCode(D);
                s := UpStrg(Hex8(l))+': '+D.CodeStr+Strg(' ', 29-
                  Length(D.CodeStr))+
                D.Command+Strg(' ', 10-Length(D.Command))+
                D.Operands;
                Drivers.MoveStr(B, s, C);
                Inc(l, D.InstrLen);
              end;
            WriteLine(0, i, WDH, 1, B[XDelta]);
          end;
      end
      {GRM!}
      {$ENDIF} {DisAsm}
      else
        begin{vmText}
          ExposedLine := 0;
          EnableCommands([cmUnWrap]);
          {ak155        HideCursor;}
          { возможный сдвиг XDelta, чтобы стал виден найденный текст,
и определение его X-позиции (с учетом табуляций) для будущей раскраски }
          SearchLineNum := -1;
          if SearchActive then
            begin
              for i := 0 to Size.Y-1 do
                begin
                  j := SearchX-FilePos-Lines[i].Pos;
                  if (j >= 0) and (j < Lines[i].len) then
                    begin
                      if not ExpandTabs then
                        W := j
                      else
                        begin
                          W := 0;
                          for M := Lines[i].Pos to Lines[i].Pos+j-1 do
                            begin
                              if Buf^[M] = 9 then
                                Inc(W, TS-(W mod TS))
                              else
                                Inc(W);
                            end;
                        end;
                      if (XDelta+Size.X < W+Length(SearchString.What))
                        or (XDelta > W)
                      then
                        XDelta := W+(Length(SearchString.What)-Size.X)
                          div 2;
                      if XDelta < 0 then
                        XDelta := 0;
                      SearchLineNum := i;
                      SearchTextStart := W-XDelta;
                      break;
                    end;
                end;
            end;

          for i := 0 to Size.Y-1 do
            begin
              MoveChar(B[0], ' ', C, Size.X);
              s := '';
              if (Lines[i].Pos >= 0) then
                begin
                  ExposedLine := i;

                  {-DataCompBoy: Expand tabs...-}
                    {AK155: и применить фильтр }
                  W := 0;
                  for M := Lines[i].Pos to Lines[i].Pos+Lines[i].len-1
                  do
                    begin
                      C2 := Char(Buf^[M]);
                      if (C2 = #9) and ExpandTabs then
                        repeat
                          Inc(W);
                          if W > XDelta then
                            s[W-XDelta] := ' ';
                        until W mod TS = 0
                      else
                        begin
                          Inc(W);
                          if W > XDelta then
                            s[W-XDelta] := C2;
                        end;
                      if W-XDelta > Size.X then
                        break;
                    end;
                  if W > XDelta then
                    begin
                      SetLength(s, W-XDelta);
                      XLatBuf(s[1], Length(s), XLAT);
                      {Cat: фильтр перенесён сюда - он должен быть использован
      уже после применения таблицы перекодировки}
                      case Filter of
                        1:
                          for W := 1 to Length(s) do
                            if not (byte(s[W]) in [32..127]) then
                              s[W] := #250;
                        2:
                          for W := 1 to Length(s) do
                            if not (byte(s[W]) in [32..255]) then
                              s[W] := #250;
                      end {case};
                      {/Cat}
                      MoveStr(B, s, C (*, Filter, TS*));
                      if HiLite and not QuickView then
                        begin
                          Highlites(Length(s), @S[1], HiLitePar);
                          C1 := C and $F0; { Background }
                          SetLength(HP, 6); { Attr. size }
                          HP[Ord(hhComment)] := Chr(CC[3]);
                            { Comments   }
                          HP[Ord(hhNumber)] := Chr(C1 or (CC[6] and 15))
                            ; { Numbers    }
                          HP[Ord(hhString)] := Chr(C1 or (CC[5] and 15))
                            ; { Strings    }
                          HP[Ord(hhSymbol)] := Chr(C1 or (CC[4] and 15))
                            ; { Symbols    }
                          HP[Ord(hhKeyword1)] := Chr(C1 or (CC[7]
                            and 15)); { Keywords 1 }
                          HP[Ord(hhKeyword2)] := Chr(C1 or (CC[8]
                            and 15)); { Keywords 2 }
                          DoHighlite(B, s, HP);
                        end;
                    end;

                end;
              if i = SearchLineNum then
                  { раскраска найденного текста }
                MoveColor(B[SearchTextStart], Length(SearchString.
                  What), CC[2]);
              WriteLine(0, i, Size.X, 1, B[0]);
            end;
        end;
      if ExposedLine > MaxLines then
        ExposedPos := Lines[ExposedLine].Pos+Lines[ExposedLine].len
      else if Lines[ExposedLine+1].Pos < 0 then
        ExposedPos := BufSize
      else
        ExposedPos := Lines[ExposedLine+1].Pos
    end {case};
    if Info <> nil then
      Info^.DrawView;
  end { TFileViewer.Draw };

function DetectKeyMap(const FName: String): TKeyMap;
  begin
    DetectKeyMap := kmAscii;
  end;

function TFileViewer.ReadFile;
  var
    i, j: integer;
    P: Pointer;
    Macros: PCollection;
    ClusterLen: TSize;
    NumFiles, NumDirs: integer;
    DirLen: TSize;
    DirString: String;
    ReadDir: boolean;
    Nm: String; {John_SW 14-09-2002}
    XT: String; {John_SW 14-09-2002}
  begin
    ReadFile := True;
    WriteModify;
    if Buf <> nil then
      FreeMem(Buf, BufSize);
    Buf := nil;
    BufModified := False;
    isValid := True;
    FileName := FName;
    VFileName := VFName;
    if KeyMap = kmNone then
      begin
        KeyMap := DetectKeyMap(FName);
        if not UseXLat then
          SetXlatFile('', False);
      end;
    if NewStream then
      begin
        if Fl <> nil then
          Dispose(Fl, Done);
        Fl := nil;
        if FName = '' then
          exit;
        Fl := New(PDosStream, Init(FName, stOpenRead));
      end;

    { Если FName - имя каталога, то формируем буфер (без Stream) с
текстовыми данными о содержимом каталога. Это используется в QuickView,
если куросор стоит на каталоге AK155}
    ReadDir := True;
    i := Length(FName);
    if Copy(FName, i-2, 3) = '\..' then
      begin
        SetLength(FileName, i-3);
        DirString := FileName;
      end
    else if (i = 2) and (FileName[2] = ':') then
      DirString := FileName
        {так бывает, например, при перезапуске
     DN с запомненным состоянием с Branch в корне диска }
    else if (FileName = ' ') or not IsDir(FileName) then
      {Cat: проверка на пробел нужна, т.к. IsDir вернёт для него True, что нас
        не устраивает, поскольку пробел означает использование просмотрщика
        не для конкретного файла, а для заданного конструктору потока}
      ReadDir := False;
    if ReadDir then
      begin
        DirLen := CountDirLen(FileName, True, ClusterLen, NumFiles,
          NumDirs);
        DirString := FileName+#$0D#$0A#$0D#$0A+
        GetString(dlDirectories)+': '+FStr(NumDirs)+#$0D#$0A#$0D#$0A+
        GetString(dlDIFiles)+': '+FStr(NumFiles-NumDirs)+
          #$0D#$0A#$0D#$0A+
        GetString(dlDIBytes)+': '+FStr(DirLen);
        BufSize := Length(DirString);
        BufPos := 0;
        FilePos := 0;
        FileSize := BufSize;
        GetMem(Buf, 100);
        Move(DirString[1], Buf^, Length(DirString));
        MakeLines;
        Fl^.Status := stOK; { сбрасываем stInitError }
        exit;
      end;
    BreakOnStreamReadError;
    {см. flpanelx, TFilePanelRoot.SendLocated;}

    (*  if (not ExistFile(FName) or isDir(FName)) and
     (FName <> '') and (FName <> ' ') then begin ReadFile:=false; Exit; end;
*)
    Macros := nil;
    lFSplit(VFName, FreeStr, Nm, XT); {John_SW 14-09-2002}
    {PZ 2000.06.09 }
    if (EditorDefaults.ViOpt and vbfHlt) <> 0 then
      HiLite := Macro.InitHighLight(Nm+XT, HiLitePar, Macros, nil);
    {PZ end}
    ScrollEOF := (EditorDefaults.ViOpt and vbfScrollAfterEOF) <> 0;
      {AK155}
    FileSize := Fl^.GetSize;
    FilePos := 0;
    i := 1;
    NumLines := 1;
    if FileSize < 0 then
      FileSize := 0;
    AdjustBuf;
    if SB <> nil then
      PViewScroll(SB)^.MaxV := FileSize;
    Seek(0);
    XDelta := 0;
    if (UpStrg(VFileName) <> UpStrg(FileName)) then
      begin
        KillAfterUse := True;
        Filtr := True;
      end
    else
      Filtr := False;
    BreakOnStreamReadError;
  end { TFileViewer.ReadFile };

procedure TFileViewer.SetState;
  begin
    inherited SetState(AState, Enable);
    if (AState and (sfActive+sfSelected) <> 0) then
      begin
        if GetState(sfSelected) and (Owner^.GetState(sfActive)) then
          begin
            if SB <> nil then
              begin
                SB^.Show;
                SB^.EventMask := $FFFF
              end;
            DrawView
          end
        else if SB <> nil then
          begin
            SB^.Hide;
            SB^.EventMask := 0;
          end;
      end;
    if (Info <> nil) then
      if (AState and sfDragging <> 0)
        or (AState and (sfSelected+sfActive) <> 0)
      then
        Info^.Draw;
  end { TFileViewer.SetState };

procedure TFileViewer.HideView;
  var
    E: TEvent;
  begin
    if SB <> nil then
      begin
        SB^.Hide;
        SB^.EventMask := 0;
      end;
    Message(Owner, evCommand, cmDisableView, nil);
    inherited HideView;
  end;

procedure TFileViewer.ShowView;
  var
    E: TEvent;
  begin
    if SB <> nil then
      begin
        SB^.Show;
        SB^.EventMask := 0;
      end;
    Message(Owner, evCommand, cmEnableView, nil);
    inherited ShowView;
  end;

procedure TFileViewer.Seek(APos: longInt);
  begin
    if Buf = nil then
      exit;
    FileSize := Fl^.GetSize;
    WriteModify;
    if APos < 0 then
      APos := 0;
    FilePos := APos;
    if FilePos < 0 then
      FilePos := 0;
    if FilePos = 0 then
      begin
        AdjustBuf;
        if Buf = nil then
          exit;
      end;
    if APos+BufSize >= FileSize then
      FilePos := FileSize-BufSize;
    Fl^.Seek(FilePos);
    if (FileSize-FilePos < BufSize) then
      begin
        FreeMem(Buf, BufSize);
        BufSize := FileSize-FilePos;
        if BufSize < 0 then
          BufSize := 0;
        Buf := MemAlloc(BufSize);
        if Buf = nil then
          exit;
      end;
    BufPos := APos-FilePos;
    if BufPos < 0 then
      BufPos := 0;
    if BufPos >= BufSize then
      BufPos := BufSize-1;
    Fl^.Read(Buf^, BufSize);
    BreakOnStreamReadError;
    MakeLines;
    MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
  end { TFileViewer.Seek };

procedure TFileViewer.MakeLines;
  var
    k: longInt;
    LineLen: longInt;
    LineStart: longInt;
    TabsWith: longInt; { Tab characters additional with }
    MaxX: longInt;
    LineSeparatorLen: longInt;
    TabSize: integer;
    CurChar, NextChar: byte;
    WrappedLine: boolean;
    {$IFDEF DisAsm}
    X: XCHGData; {GRM!}
    {$ENDIF}
  label
    LineBegin;
  begin
    if Buf = nil then
      exit;
    LineStart := BufPos;
    LineLen := BufSize-LineStart;
    FillChar(Lines, SizeOf(Lines), $FF); {AK155}
    TabSize := GetTabSize;
    MaxLines := MaxILines;
    case ViewMode of
      vmHex, vmDump:
      begin
        MaxX := 255;
        LineLen := BufPos;
        for k := 0 to MaxLines do
          begin
            Lines[k].Pos := LineLen;
            Lines[k].len := HexPos;
            Inc(LineLen, HexPos);
          end;
        {$IFNDEF DisAsm}
      end
        {$ELSE}
      end;
      {GRM!}
      vmAsm:
      begin
        MaxX := 255;
        X.MemBuff := Buf;
        X.Offset := BufPos;
        for k := 0 to MaxLines do
          begin
            Lines[k].Pos := X.Offset;
            X.InstrMaxLen := FileSize-X.Offset-FilePos-1;
            ScanCode(X);
            Lines[k].len := X.InstrLen;
            if X.InstrLen = 0 then
              Lines[k].Pos := -1;
          end;
      end
      {GRM!}
      {$ENDIF}
      else{vmText}
        begin
          if Wrap > wmNone then
            MaxX := Size.X
          else
            MaxX := MaxWrapW;
          WrappedLine := False;
          for k := 0 to MaxLines do
            begin{-DataCompBoy-}{AK155}
LineBegin:
              if LineStart >= BufSize then
                begin
                  Lines[k].Pos := -1;
                  continue;
                end;
              Lines[k].Pos := LineStart;
              {Search for e.nd of line}
              LineLen := 0;
              TabsWith := 0;
              LineSeparatorLen := 0;
              while (LineLen+TabsWith < MaxX) and (LineStart+LineLen <
                  BufSize)
              do
                begin
                  CurChar := Buf^[LineStart+LineLen];
                  NextChar := 0;
                  if (LineStart+LineLen+1 < BufSize) then
                    NextChar := Buf^[LineStart+LineLen+1];
                  if (CurChar in [$0D, $0A]) then
                    begin
                      LineSeparatorLen := 1;
                      if (NextChar in [$0D, $0A]) and (CurChar <>
                          NextChar)
                      then
                        LineSeparatorLen := 2;
                      if WrappedLine and (LineLen = 0) then
                        begin
                            {ignore line separator exact after wrapped line}
                          Inc(LineStart, LineSeparatorLen);
                          WrappedLine := False;
                          goto LineBegin;
                        end;
                      WrappedLine := False;
                      break;
                    end;
                  if CurChar = 9 then
                    Inc(TabsWith, TabSize-1-((LineLen+TabsWith) mod
                      TabSize));
                  Inc(LineLen);
                  WrappedLine := True;
                end;
              Lines[k].len := LineLen;

              {Decrease length of word-wrapped line}
              if (Wrap = wmWords) and (LineLen+TabsWith >= MaxX)
              then
                begin
                  while (LineLen > 1) and
                    not (Char(CurChar) in BreakChars) and
                    not (Char(NextChar) in BreakChars)
                  do
                    begin
                      Dec(LineLen);
                      NextChar := CurChar;
                      CurChar := Buf^[LineStart+LineLen-1];
                    end;
                  if LineLen > 1 then
                    begin
                      Lines[k].len := LineLen;
                      WrappedLine := True;
                    end;
                end;

              Inc(LineStart, Lines[k].len+LineSeparatorLen);
            end; {-DataCompBoy-}
        end;
    end {case};
  end { TFileViewer.MakeLines };

procedure TFileViewer.SaveToFile(FN: String);
  var
    s: TDOSStream;
    P: PView;
    W, l: word;
    PS: Pointer;
    XL: boolean;
    Sz: longInt;
  label 2;

  begin
    XL := (MaxAvail > 4096) and (UseXLat or (KeyMap <> kmAscii)) and
    (MessageBox(GetString(dlViewSaveXlat), nil,
    mfYesButton+mfNoButton+mfConfirmation) = cmYes);
    s.Init(FN, stOpen);
    W := 0; {AK155: чтобы всегда была определена }
    if s.Status = stOK then
      begin
        PS := @FN;
        W := MessageBox(GetString(dlED_OverQuery)
        , @PS, mfYesButton+mfCancelButton+mfAppendButton+mfWarning);
        if W = cmYes then
          begin{существующий файл}
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
          s.Seek(s.GetSize)
        else
          begin
            s.Done;
            exit
          end;
      end
    else
      begin{новый файл}
        s.Done; {JO}
        s.Init(FN, stCreate); {JO}
      end;

    if s.Status <> stOK then
      begin
        s.Done;
        MessageBox(GetString(dlFBBNoWrite)+Cut(FN, 40), nil, mfError+
          mfOKButton);
        exit
      end;
    P := _WriteMsg(^M^M^C+GetString(dlWritingFile));
    Fl^.Seek(0);
    BreakOnStreamReadError;
    if XL then
      begin
        PS := MemAlloc(4096);
        if PS = nil then
          goto 2;
        Sz := Fl^.GetSize;
        while Fl^.GetPos < Sz do
          begin
            if Sz-Fl^.GetPos < 4096 then
              l := Sz-Fl^.GetPos
            else
              l := 4096;
            Fl^.Read(PS^, l);
            BreakOnStreamReadError;
            XLatBuf(PS^, l, XLAT);
            s.Write(PS^, l);
          end;
        FreeMem(PS, 4096);
      end
    else
2:
      s.CopyFrom(Fl^, Fl^.GetSize);
    P^.Free;
    s.Truncate;
      {AK155 на случай записи короткого файла поверх длинного}
    s.Done;
    FN := GetPath(FN);
    if (Length(FN) > 3) and (FN[Length(FN)] = '\') then
      SetLength(FN, Length(FN)-1);
    if not DnIni.AutoRefreshPanels then
      GlobalMessage(evCommand, cmRereadDir, @FN);
  end { TFileViewer.SaveToFile };

procedure TFileViewer.SeekBof;
  begin
    SearchX := 0;
    Seek(0);
    if ViewMode = vmAsm
    then
      Cur.Assign(10, 0)
    else
      Cur.Assign(0, 0);
    XDelta := 0;
    DrawView
  end;

{AK155}
{ Выдача сообщения, закрытие окна и возбуждение исключения }
function TFileViewer.BreakOnStreamReadError: boolean;
  var
    E: eFileError;
  begin
    BreakOnStreamReadError := False;
    if Fl^.Status <> stOK then
      begin
        if Fl^.Status = stInitError then
          MessFileNotOpen(FileName, Fl^.ErrorInfo)
        else
          MessFileNotRead(FileName, Fl^.ErrorInfo);
        BreakOnStreamReadError := True;
        E := eFileError.Create('TFileViewer stream read error');
        E.rc := Fl^.ErrorInfo;
        { на самом деле это собщение и RC никому не нужны, но это может
      пригодиться, если не все вызовы BreakOnStreamReadError обложены
      try - except и исключение окажется фатальным}

        { При QuickView просмотрщик вставлен не в самостоятельное окно,
а прямо в менеджер файлов, так что при QuickView закрывать владельца
не просто не нужно, а недопустимо }
        if (Owner <> nil) and not QuickView then
          Owner^.Free;
        isValid := False;
        raise E At ReturnAddr;
      end;
  end { TFileViewer.BreakOnStreamReadError: };
{/AK155}

procedure TFileViewer.SeekEof;
  var
    i, j: integer;
    OldFileSize: longInt;
    OldStart, OldLen1: longInt;
    Temp: longInt;
  label
    EndProc;
  begin
    with Lines[Cur.Y] do
      begin
        if Pos < 0 then
          exit;
        if Pos+len >= Fl^.StreamSize then
          exit;
      end;
    if ViewMode = vmText then
      begin
        OldStart := FilePos+Lines[0].Pos;
        OldLen1 := Lines[0].len;
      end
    else
      begin
        OldStart := FilePos+Lines[Cur.Y].Pos;
        OldLen1 := HexPos;
      end;
    i := (FilePos+BufPos) mod 16;
    Owner^.Lock;
    {AK155}
    if TypeOf(Fl^) = TypeOf(TDOSStream) then
      begin
        with PDosStream(Fl)^ do
          begin
            if (Status <> stOK) then
              goto EndProc;

            OldFileSize := FileSize;
            SysFileSeek(Handle, 0, 2, StreamSize);
            FileSize := StreamSize;
            if FileSize < OldFileSize then
              begin{ Файл сжался }
                if Position > FileSize then
                  Position := StreamSize;
              end;
            SysFileSeek(Handle, Position, 0, Temp);
          end;

        if (OldFileSize < FileSize) and (BufSize < FileSize) then
          begin
            AdjustBuf;
            if Buf = nil then
              goto EndProc;
          end;
      end;

    Seek(FileSize-BufSize);
    if (ViewMode = vmHex) or (ViewMode = vmDump) then
      begin
        BufPos := Max(OldStart-FilePos, BufSize-Size.Y*HexPos);
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
          Inc(i);
        if i <> 0 then
          begin
            if FilePos+Lines[i].Pos > OldStart then
              begin
                Lines[0].Pos := OldStart-FilePos;
                Lines[0].len := OldLen1;
                j := 1;
              end
            else
              j := 0;
            Move(Lines[i], Lines[j], SizeOf(Lines[0])*(MaxLines-i-j));
            Dec(MaxLines, i-j);
          end;
        BufPos := Lines[0].Pos;
      end;

    if (ViewMode = vmHex) and (FileSize <> 0) then
      begin
        i := FileSize-(FilePos+BufPos)-1;
        Cur.Y := i div HexPos;
        Cur.X := i mod HexPos*2;
      end;
    DrawView;

EndProc:
    Owner^.UnLock;
  end { TFileViewer.SeekEof };

procedure TFileViewer.HandleEvent;

  var
    P: TPoint;
    LR: integer;
    i: integer;
    LLR, LFR: longInt;
    F: lFile;
    Ch: Char;
    Lin: byte;

  procedure CE;
    begin
      ClearEvent(Event)
    end;

  procedure CM_END;
    var
      i, MaxW: integer;
      W, l: integer;
      TabSize: integer;

    begin
      if Buf = nil then
        exit;
      TabSize := GetTabSize;
      MaxW := 0;
      CE;
      for i := 0 to Size.Y-1 do
        begin
          W := 0;
          for l := Lines[i].Pos to Lines[i].Pos+Lines[i].len-1 do
            begin
              Inc(W);
              if Buf^[l] = 9 then
                Inc(W, TabSize-1-l mod TabSize);
            end;
          if MaxW < W then
            MaxW := W;
        end;
      Dec(MaxW, Size.X);

      if MaxW < 0 then
        XDelta := 0
      else
        XDelta := MaxW;
      DrawView;
    end { CM_END };

  procedure ContinueSearch(Reverse: boolean);
    var
      i {, J, JJ}: integer;
      BXL: TXlat;
    begin
      if (SearchString.What = '') then
        exit;
      if SearchX < 0 then
        SearchX := 0;
      if (SearchX < FilePos+Lines[0].Pos)
          { or
     (SearchX > FilePos + Lines[1].Pos+1)} {AK155}
      then
        SearchX := FilePos+BufPos;
      NullXLAT(BXL);
      for i := 255 downto 1 do
        BXL[XLAT[Char(i)]] := Char(i);
      SearchX := SearchFileStr(Fl, BXL,
      SearchString.What, SearchX,
      SearchString.Opts and 1 <> 0, True,
      SearchString.Opts and 2 <> 0, (SearchString.Dir = 1) xor
        Reverse,
      SearchString.Opts and 4 <> 0,
      SearchString.Opts and 8 <> 0);
      if SearchX >= 0 then
        begin
          {if HexPos > 0 then JJ := (FilePos+BufPos) mod HexPos;}
          Seek(SearchX);
          CountUp(3);
          SearchActive := True;
          BufPos := Lines[0].Pos;
          MakeLines;
          DrawView;
          SearchActive := False;
          Inc(SearchX);
        end
      else if SearchX <> -2 then
        MessageBox(GetString(dlDBViewSearchNot), nil, mfInformation+
          mfOKButton);
    end { ContinueSearch };

  procedure StartSearch;
    var
      D: PDialog;
      SR: TViewSearch;
      R: TRect;
      P: PView;
      PP: PInputline;
      i: integer;

    begin
      if LowMemory then
        exit;

      D := PDialog(LoadResource(dlgViewerFind));
      D^.SetData(SearchString);
      i := Desktop^.ExecView(D);
      D^.GetData(SR);
      Dispose(D, Done);
      if i = cmCancel then
        exit;
      SearchString := SR;
      {AK155}
      SearchX := FilePos+Lines[0].Pos;
      if SR.Dir = 1 then
        begin
          i := Size.Y-1;
          while (i <> 0) and (Lines[i].Pos < 0) do
            Dec(i);
          SearchX := FilePos+Lines[i].Pos+Lines[i].len;
        end;
      {/AK155}
      ContinueSearch(False);
    end { StartSearch };

  procedure LoadXlatTable; {JO}
    var
      FN: String;
      More: boolean;
      None: boolean;
    label SkipMenu;
    begin
      CE;
      More := True;
      None := UseXLat;
      if SkipXLatMenu then
        goto SkipMenu;
      FN := CnvString(XLatFile);
      if FN <> '' then
        FN := FN+'.xlt';
      FN := GetFileNameMenu(SourceDir+'XLT\', '*.XLT', FN, True,
        More, None);
SkipMenu:
      if None then
        begin
          KeyMap := kmAscii;
          SetXlatFile('', True);
          UseXLat := False;
          {DrawView;}
          exit;
        end;
      if More then
        FN := GetFileNameDialog(SourceDir+'XLT\*.XLT',
        GetString(dlSelectXLT),
        GetString(dlOpenFileName),
        fdOKButton+fdHelpButton,
        hsOpenXLT);
      if FN <> '' then
        SetXlatFile(FN, True);
    end { LoadXlatTable }; {JO}

  procedure NextXLat;
    begin
      KeyMap := RollKeyMap[KeyMap];
      SetXlatFile('', True);
    end;

  label 1, 2, DoSave, KBCheck, NotKb;

  var
    ScrollerPos:
    longInt;
  begin { TFileViewer.HandleEvent }

    try

      inherited HandleEvent(Event);
      case ViewMode of
        vmText,
        vmHex:
          begin
            HexPos := (Size.X-12) div 4;
            if HexPos <= 0 then
              HexPos := 1;
          end;
        vmDump:
          begin
            HexPos := ((Size.X-9) div 16)*16;
            if HexPos < 16 then
              HexPos := 16;
          end;
      end {case};
      if Loaded then
        begin
          Loaded := False;
          if SB <> nil then
            begin
              PViewScroll(SB)^.MaxV := FileSize;
              PViewScroll(SB)^.Value := FilePos+BufPos;
            end;
          PViewScroll(SB)^.DrawView;
        end;
      case Event.What of
        evBroadcast:
          case Event.Command of
            cmFindView:
              if (PString(Event.InfoPtr)^ = FileName) or
                (PString(Event.InfoPtr)^ = VFileName)
              then
                if Owner <> nil then
                  begin
                    Owner^.Select;
                    ClearEvent(Event);
                  end;
            cmReanimator:
DoSave:
                if (Fl <> nil) and (Fl^.Status = stOK) then
                  begin
                    CE;
                    FreeStr := GetFileNameDialog(x_x, GetString(
                      dlSaveFileAs),
                    GetString(dlSaveFileAsName),
                    fdOKButton+fdHelpButton, hsEditSave);
                    if FreeStr <> '' then
                      SaveToFile(FreeStr);
                  end;
            cmReleaseFile:
              begin
                if UpStrg(FileName) = UpStrg(PString(Event.InfoPtr)^)
                then
                  begin
                    ReadFile('', '', True);
                    {if not QuickView then Message(Owner, evCommand, cmClose, nil);}
                  end;
              end;
          end {case};
        evCommand:
          case Event.Command of
            cmFileEdit:
              if DelSpaces(FileName) <> '' then
                begin
                  if Filtr and
                    (MessageBox(GetString(dlViewFilter), nil,
                    mfConfirmation+mfYesButton+mfNoButton) <> cmYes)
                  then
                    begin
                      CE;
                      exit
                    end;
                  WriteModify;
                  {if Desktop<>nil then Desktop^.Lock;}
                  if Application <> nil then
                    begin
                      if FileName <> VFileName then
                        TempFile := '*^&'+FileName;
                      PDNApplication(Application)^.EditFile(True,
                        FileName);
                      MessageL(Application, evCommand,
                        cmGotoLineNumber2,
                      GetLineNumberForOffset(FileName, FilePos+
                        BufPos)
                      );
                    end;
                  {if Desktop<>nil then Desktop^.UnLock;}
                  CE;
                  if (Owner <> nil) and (TypeOf(Owner^) = TypeOf(
                      TFileWindow))
                  then
                    Message(Owner, evCommand, cmClose, nil);
                end;
            cmPlaceMarker1..cmPlaceMarker9:
              begin
                i := Event.Command-cmPlaceMarker1+1;
                if (i >= 1) and (i <= 9) then
                  begin
                    if (MarkPos[i].Y = -1) or
                      (MarkPos[i].Y <> FilePos+BufPos) or
                      (MarkPos[i].X <> byte(HexEdit)*256*256
                      +Cur.Y*256+Cur.X)
                    then
                      MarkPos[i].Assign(byte(HexEdit)*256*256
                      +Cur.Y*256+Cur.X, FilePos+BufPos)
                    else
                      MarkPos[i].Assign(-1, -1);
                    DrawView;
                  end;
                CE;
              end;
            cmGoToMarker1..cmGoToMarker9:
              begin
                i := Event.Command-cmGoToMarker1+1;
                if (i >= 1) and (i <= 9) and (MarkPos[i].Y <> -1)
                then
                  begin
                    Cur.X := MarkPos[i].X mod 256;
                    Cur.Y := (MarkPos[i].X div 256) mod 256;
                    HexEdit := (MarkPos[i].X div (256*256)) = 0;
                    Seek(MarkPos[i].Y);
                    MakeLines;
                    DrawView;
                  end;
                CE;
              end;
            cmGotoCell:
              begin
                CE;
                FreeStr := '';
                if ViewMode = vmText then
                  begin
                    if ExecResource(dlgGotoLine, FreeStr) <> cmOK
                    then
                      exit;
                    Val(FreeStr, LLR, i);
                    if i <> 0 then
                      exit;
                    LLR := GetOffsetForLineNumber(FileName, LLR);
                    if LLR < 0 then
                      exit;
                    Seek(LLR);
                    MakeLines;
                    DrawView;
                  end
                else
                  begin
                    if ExecResource(dlgGotoAddress, FreeStr) <> cmOK
                    then
                      exit;
                    Val('$'+FreeStr, LLR, i);
                    if i <> 0 then
                      exit;
                    LFR := LLR;
                    Cur.Assign(0, 0);
                    {JO} {$IFDEF DisAsm}
                    if ViewMode <> vmAsm then
                      begin
                        {$ENDIF}
                        while ((LFR mod HexPos) <>
                          ((BufPos+FilePos) mod HexPos)) and (LFR > 0)
                        do
                          begin
                            Dec(LFR);
                            Inc(Cur.X, 2);
                          end;
                        {$IFDEF DisAsm}
                      end
                    else
                      Cur.X := 10;
                    {/JO} {$ENDIF}
                    Seek(LFR);
                    MakeLines;
                    DrawView;
                  end;
              end;
            cmLoadXlatTable:
              LoadXlatTable;
            cmAddFilter:
              begin
                Filter := (Filter+1) mod 3;
                DrawView;
                CE
              end;
            cmSave:
              begin
                WriteModify;
                CE
              end;
            cmSaveAll:
              goto DoSave;
            cmGetName:
              if VFileName <> ''
              then
                PString(Event.InfoPtr)^:= GetString(dlViewFile)+
                  ' - '+ {$IFDEF RecodeWhenDraw}CharToOemStr
                  {$ENDIF}(VFileName)
              else
                PString(Event.InfoPtr)^:= GetString(dlViewFile);
            cmLoadViewFile:
              if QuickView and
                (UpStrg(lFExpand(PString(Event.InfoPtr)^))
                <>
                UpStrg(lFExpand(FileName))
                )
              then
                begin
                  ReadFile(PString(Event.InfoPtr)^,
                  PString(Event.InfoPtr)^, True);
                  DrawView;
                  CE;
                end;
            cmClose, cmQuit, cmKillUsed:
              if WriteModify then
                CE;
            cmSearchFor:
              begin
                StartSearch;
                CE
              end;
            cmReverseSearch,
            cmContinueSearch:
              begin
                ContinueSearch(Event.Command = cmReverseSearch);
                CE
              end;
            cmHexMode:
              begin
                case ViewMode of
                  vmText: begin
                    ViewMode := vmHex;
                    HexEdit := True;
                  end;
                  vmHex: begin
                    ViewMode := vmDump;
                  end;
                  {$IFDEF DisAsm}
                  vmDump: if ((EditorDefaults.ViOpt and vbfDisAsm) <> 0)
                  then
                    begin
                      ViewMode := vmAsm;
                      HexEdit := True;
                      MakeLines;
                      Cur.X := 10;
                    end {GRM!}
                  else
                    begin
                      ViewMode := vmText;
                      MakeLines
                    end;
                  {$ENDIF}
                  else
                    begin
                      ViewMode := vmText;
                      HideCursor;
                      MakeLines
                    end;
                end {case};
                if ViewMode <> vmDump then
                  begin
                    HexPos := (Size.X-12) div 4;
                    if HexPos <= 0 then
                      HexPos := 1;
                  end
                else
                  begin
                    HexPos := ((Size.X-9) div 16)*16;
                    if HexPos < 16 then
                      HexPos := 16;
                    MakeLines
                  end;
                DrawView;
                CE
              end;
            cmUnWrap:
              begin
                Inc(Wrap);
                if Wrap > wmWords then
                  Wrap := wmNone;
                MakeLines;
                if Lines[Size.Y-1].Pos < 0 then
                  SeekEof;
                DrawView;
                CE;
              end;
            cmScrollBarChanged:
              begin
                Desktop^.Lock;
                Seek(Event.InfoLong);
                if (ViewMode = vmText) and (Event.InfoLong <> 0)
                then
                  begin
                      { стать на начало строки, но так,
                 чтобы не сдвинуть вверх ползунок курсора }
                    CountDown(1);
                    ScrollerPos := PViewScroll(SB)^.GetPartCode;
                    CountUp(1);
                    if ScrollerPos <> PViewScroll(SB)^.GetPartCode
                    then
                      CountDown(1);
                  end;
                if Lines[Size.Y-1].Pos < 0 then
                  SeekEof
                else
                  DrawView;
                Desktop^.UnLock;
              end;
            cmSwitchKeyMapping:
              NextXLat;
          end {case};

        evKeyDown:
          case Event.KeyCode of
            kbCtrlHome:
              if ViewMode = vmHex then
                begin
                  Cur.Y := 0;
                  DrawView;
                  CE;
                end
              else
                goto 1;
            kbCtrlEnd:
              if ViewMode = vmHex then
                begin
                  Cur.Y := Size.Y-1;
                  DrawView;
                  CE;
                end
              else
                goto 2;
            kbESC {, kbEnter}:
              if not QuickView then
                begin
                  CE;
                  Message(Application, evCommand, cmClose, nil)
                end;
            kbCtrlLeft:
              if ViewMode = vmText then
                begin
                  CE;
                  if XDelta > 19 then
                    Dec(XDelta, 20)
                  else
                    XDelta := 0;
                  DrawView
                end
              else(*if ViewMode <> vmAsm then {GRM!}*)
                begin
                  if BufPos > 0 then
                    Dec(BufPos)
                  else if FilePos > 100 then
                    begin
                      Owner^.Lock;
                      Seek(FilePos-100);
                      BufPos := 99;
                      Owner^.UnLock;
                    end;
                  DrawView;
                  CE
                end;

            kbCtrlRight:
              if ViewMode = vmText then
                begin
                  CE;
                  if XDelta < MaxWrapW*8-Size.X-19 then
                    Inc(XDelta, 20);
                  DrawView
                end
              else(*if ViewMode <> vmAsm then {GRM!}*)
                begin
                  if BufPos < BufSize-1024 then
                    Inc(BufPos)
                  else
                    begin
                      Owner^.Lock;
                      Seek(FilePos+BufPos+1);
                      Owner^.UnLock;
                    end;
                  DrawView;
                  CE
                end;
            kbTab:
              if ViewMode = vmHex then
                begin
                  if HexEdit then
                    Cur.X := Cur.X div 2
                  else
                    Cur.X := Cur.X*2;
                  HexEdit := not HexEdit;
                  DrawView;
                  CE
                end;
            kbCtrlPgUp:
              begin
1:
                CE;
                SeekBof;
              end;
            kbCtrlPgDn:
              begin
2: Lines[Size.Y].Pos := 0; {piwamoto}
                SeekEof;
                SearchX := FileSize;
                CE
              end;
            kbHome:
              begin
                CE;
                if ViewMode = vmHex then
                  Cur.X := 0
                else
                  XDelta := 0;
                DrawView
              end;
            kbEnd:
              if ViewMode <> vmHex then
                CM_END
              else
                begin
                  Cur.X := HexPos*(byte(HexEdit)+1)-1;
                  DrawView
                end;
            kbDown:
              case ViewMode of{GRM!}
                vmText, vmDump:
                  begin
                    CE;
                    CountDown(1);
                    DrawView;
                  end;
                vmHex:
                  begin
                    Inc(Cur.Y);
                    DrawView;
                    CE;
                  end;
                {$IFDEF DisAsm}
                vmAsm:
                  if Cur.Y = Size.Y-1 then
                    begin
                      CountDown(1);
                      DrawView;
                      CE;
                    end
                  else if Lines[Cur.Y+1].Pos <> -1 then
                    begin
                      Inc(Cur.Y);
                      Cur.X := 10;
                      DrawView;
                      CE;
                    end;

                {$ENDIF}
              end {case}; {GRM!}
            kbPgDn:
              begin
                CE;
                CountDown(Size.Y-1);
                DrawView;
              end;
            kbSpace:
              if ViewMode in [vmText, vmDump]
              then
                begin
                  CE;
                  CountDown(Size.Y-1);
                  DrawView;
                end
              else
                goto KBCheck;
            kbUp:
              case ViewMode of{GRM!}
                vmText,
                vmDump:
                  begin
                    CE;
                    CountUp(1);
                    DrawView;
                  end;
                vmHex:
                  begin
                    Dec(Cur.Y);
                    DrawView;
                    CE;
                  end;
                {$IFDEF DisAsm}
                vmAsm:
                  if Cur.Y = 0 then
                    begin
                      CountUp(1);
                      DrawView;
                      CE;
                    end
                  else
                    begin
                      Cur.X := 10;
                      Dec(Cur.Y);
                      DrawView;
                      CE;
                    end;
                {$ENDIF}
              end {case}; {GRM!}
            kbPgUp:
              begin
                CE;
                CountUp(Size.Y-1);
                DrawView;
                if BufPos = 0 then
                  begin
                    PViewScroll(SB)^.Value := 0;
                    SB^.DrawView;
                  end;
              end;
            kbLeft:
              if ViewMode = vmText then
                begin
                  CE;
                  if XDelta > 0 then
                    Dec(XDelta);
                  DrawView
                end
              else
                {$IFDEF DisAsm}
                {GRM!} if ViewMode = vmAsm then
                begin
                  CE;
                  if Cur.X > 10 then
                    Dec(Cur.X)
                  else
                    begin
                      Message(@Self, evKeyDown, kbUp, nil);
                      if (BufPos+FilePos <> 0) then
                        Cur.X := Lines[Cur.Y].len*2+9;
                    end;
                  DrawView;
                end
              else
                {GRM!}
                {$ENDIF}
                begin
                  if Cur.X > 0 then
                    Dec(Cur.X)
                  else
                    begin
                      Cur.X := HexPos*(byte(HexEdit)+1)-1;
                      Dec(Cur.Y)
                    end;
                  {     if ViewMode = vmHex then
                                 Message(Owner, evCommand, cmSetMargins, nil);}
                  DrawView;
                  CE
                end;
            kbRight:
              if ViewMode = vmText then
                begin
                  CE;
                  if XDelta < MaxWrapW*8-Size.X then
                    Inc(XDelta);
                  DrawView
                end
              else
                {$IFDEF DisAsm}
                {GRM!} if ViewMode = vmAsm then
                begin
                  if Cur.X >= Lines[Cur.Y].len*2+9 then
                    Message(@Self, evKeyDown, kbDown, nil)
                  else
                    Inc(Cur.X);
                  DrawView;
                  CE
                end
              else
                {GRM!}
                {$ENDIF}
                begin
                  if Cur.X < HexPos*(byte(HexEdit)+1)-1 then
                    Inc(Cur.X)
                  else
                    begin
                      Cur.X := 0;
                      Inc(Cur.Y)
                    end;
                  {       if ViewMode = vmHex then SearchX := HexPos;}
                  DrawView;
                  CE
                end;
            {JO}kbEnter:
              case UseEnterInViewer of
1:
                  if not QuickView then
                    begin
                      CE;
                      Message(Application, evCommand, cmClose, nil)
                    end;
2:
                  Message(Owner, evCommand, cmHexMode, nil);
              end
              else
KBCheck:
                if Event.What <> evNothing then
                  begin
                    if (Event.CharCode = #11) and not CtrlK then
                        begin
                        CtrlK := True;
                        CE;
                      end
                    else if CtrlK then
                      begin
                        Ch := Event.CharCode;
                        if (Ch >= '1') and (Ch <= '9') then
                          begin
                            Event.What := evCommand;
                            Event.Command := cmPlaceMarker1+Ord(Ch)-
                              Ord('1');
                            PutEvent(Event);
                          end;
                        CtrlK := False;
                        CE;
                      end
                    else
                      begin
                        (*{$IFDEF OS2}*)
                          {JO: поскольку хоткеи Ctr-цифра в OS/2 не работают}
                        (*if ShiftState and 7 <> 0 then*)
                          {    по непонятной причине, введены временные хоткеи}
                        case Event.KeyCode of
                            {    Ctrl-Alt-Shift-цифра для перехода к закладкам}
                          {$IFDEF OS2}
                          kbCtrlAltShift1:
                            Event.Command := cmGoToMarker1;
                          kbCtrlAltShift2:
                            Event.Command := cmGoToMarker2;
                          kbCtrlAltShift3:
                            Event.Command := cmGoToMarker3;
                          kbCtrlAltShift4:
                            Event.Command := cmGoToMarker4;
                          kbCtrlAltShift5:
                            Event.Command := cmGoToMarker5;
                          kbCtrlAltShift6:
                            Event.Command := cmGoToMarker6;
                          kbCtrlAltShift7:
                            Event.Command := cmGoToMarker7;
                          kbCtrlAltShift8:
                            Event.Command := cmGoToMarker8;
                          kbCtrlAltShift9:
                            Event.Command := cmGoToMarker9;
                          {else goto NotKb;}
                          {end}
                          {else}
                          {$ENDIF}
                          {case Event.KeyCode of}
                          kbAlt1:
                            Event.Command := cmPlaceMarker1;
                          kbAlt2:
                            Event.Command := cmPlaceMarker2;
                          kbAlt3:
                            Event.Command := cmPlaceMarker3;
                          kbAlt4:
                            Event.Command := cmPlaceMarker4;
                          kbAlt5:
                            Event.Command := cmPlaceMarker5;
                          kbAlt6:
                            Event.Command := cmPlaceMarker6;
                          kbAlt7:
                            Event.Command := cmPlaceMarker7;
                          kbAlt8:
                            Event.Command := cmPlaceMarker8;
                          kbAlt9:
                            Event.Command := cmPlaceMarker9;
                          {$IFNDEF OS2}
                          kbCtrl1:
                            Event.Command := cmGoToMarker1;
                          kbCtrl2:
                            Event.Command := cmGoToMarker2;
                          kbCtrl3:
                            Event.Command := cmGoToMarker3;
                          kbCtrl4:
                            Event.Command := cmGoToMarker4;
                          kbCtrl5:
                            Event.Command := cmGoToMarker5;
                          kbCtrl6:
                            Event.Command := cmGoToMarker6;
                          kbCtrl7:
                            Event.Command := cmGoToMarker7;
                          kbCtrl8:
                            Event.Command := cmGoToMarker8;
                          kbCtrl9:
                            Event.Command := cmGoToMarker9;
                          {$ENDIF}
                          else
                            goto NotKb;
                        end {case};
                        Event.What := evCommand;
                        PutEvent(Event);
                        CE;
                        exit;
                      end;
NotKb: if (Event.CharCode = #17) and not CtrlQ then
                      begin
                        CtrlQ := True;
                        CE;
                      end
                    else if CtrlQ then
                      begin
                        Ch := Event.CharCode;
                        if (Ch >= '1') and (Ch <= '9') then
                          begin
                            Event.What := evCommand;
                            Event.Command := cmGoToMarker1+Ord(Ch)-
                              Ord('1');
                            PutEvent(Event);
                          end;
                        CtrlQ := False;
                        CE;
                      end
                    else if (Event.CharCode >= #32) and not NoEdit
                    then
                      begin
                        {GRM!}
                        if ((ViewMode = vmHex) and (Buf <> nil) and
                          (Cur.X div (byte(HexEdit)+1)+Cur.Y*HexPos+
                            BufPos < BufSize))
                          {$IFDEF DisAsm}
                          or (ViewMode = vmAsm)
                          {$ENDIF}
                        then
                          begin
                            {GRM!}
                            if Filtr then
                              begin
                                case MessageBox(GetString(
                                  dlViewFilter), nil,
                                mfConfirmation+mfYesButton+
                                  mfNoButton) of
                                  cmYes:
                                    begin
                                      Filtr := False;
                                      VFileName := '';
                                      DisposeStr(PWindow(Owner)^.
                                        Title);
                                      PWindow(Owner)^.Title :=
                                        NewStr(GetString(dlViewFile));
                                      PWindow(Owner)^.Redraw;
                                    end;
                                  else
                                    begin
                                      CE;
                                      exit
                                    end;
                                end {case};
                              end;
                            if HexEdit then
                              begin
                                Event.CharCode := UpCase(Event.
                                  CharCode);
                                if Event.CharCode in ['A'..'F', '0'..
                                    '9']
                                then
                                  begin
                                    BufModified := True;
                                    i := PosChar(Event.CharCode,
                                      '0123456789ABCDEF')-1;
                                    {$IFDEF DisAsm}
                                    if (ViewMode <> vmAsm) then{JO}
                                      {$ENDIF}
                                      P.X := Cur.X div 2+Cur.Y*
                                        HexPos+BufPos
                                      {JO} {$IFDEF DisAsm}
                                    else
                                      begin
                                        MakeLines;
                                        P.X := 0;
                                        for Lin := 0 to(Cur.Y-1) do
                                          P.X := P.X+Lines[Lin].len;
                                        P.X := P.X+((Cur.X-10) div 2)+
                                          BufPos;
                                        if P.X < 0 then
                                          P.X := 0;
                                      end
                                      {/JO} {$ENDIF}
                                      ;
                                    if odd(Cur.X) then
                                      Buf^[P.X] := (Buf^[P.X] and
                                        $F0) or i
                                    else
                                      Buf^[P.X] := (Buf^[P.X] and $F) or
                                        (i shl 4);
                                    if ViewMode = vmAsm then
                                        MakeLines; {JO}
                                    Message(@Self, evKeyDown,
                                      kbRight, nil);
                                    CE
                                  end;
                              end
                            else
                              begin
                                BufModified := True;
                                i := 0;
                                while i < 256 do
                                  begin
                                    if XLAT[Char(i)] = Char(Event.
                                        CharCode)
                                    then
                                      begin
                                        Event.CharCode := Char(i);
                                        break;
                                      end;
                                    Inc(i);
                                  end;
                                Char(Buf^[Cur.X+Cur.Y*HexPos+BufPos]) :=
                                  Event.CharCode;
                                Message(@Self, evKeyDown, kbRight,
                                  nil);
                                CE
                              end;
                          end;
                      end;
                  end;
          end {case};
        evMouseDown:
          begin
            LR := RepeatDelay;
            LLR := AutoRepeat;
            repeat
              MakeLocal(Event.Where, P);
              i := Abs((Size.Y shr 1)-P.Y);
              if i = 0 then
                RepeatDelay := 0
              else if MouseInView(Event.Where) then
                begin
                  RepeatDelay := 6-Round((i/(Size.Y shr 1))*6);
                  AutoRepeat := RepeatDelay;
                  if P.X < Size.X div 4 then
                    Message(@Self, evKeyDown, kbLeft, nil)
                  else if P.X >= (Size.X*3) div 4 then
                    Message(@Self, evKeyDown, kbRight, nil)
                  else if P.Y < Size.Y div 2 then
                    Message(@Self, evKeyDown, kbUp, nil)
                  else
                    Message(@Self, evKeyDown, kbDown, nil)
                end
              else
                RepeatDelay := 0;
            until not MouseEvent(Event, evMouseMove+evMouseAuto);
            RepeatDelay := LR;
            AutoRepeat := LLR;
            CE
          end;
      end {case};
    except
      on E:
      eFileError
    do
        begin
          Ch := ' ';
        end;
    end;
  end { TFileViewer.HandleEvent };

{-DataCompBoy-}
procedure TFileViewer.SetXlatFile;
  var
    dr: String;
    Nm: String;
    XT: String;
    s: TDOSStream;
    Table: array[1..256] of record
      A, B: Char
      end;
    C, CC: Char;
    i, j: integer;
    {      PC: PChar;}
  begin
    UseXLat := False;
    if aFName <> '' then
      begin
        lFSplit(aFName, dr, Nm, XT);
        if dr = '' then
          begin
            dr := SourceDir+'XLT\';
            XT := '.xlt';
          end;
        s.Init(dr+Nm+XT, stOpenRead);
        if (s.Status = stOK) and (s.GetSize >= 2) then
          begin
            j := Min(256, s.GetSize shr 1);
            s.Read(Table, j*2);
            NullXLAT(XLAT);
            for i := 1 to j do
              XLAT[Table[i].A] := Table[i].B;
            UseXLat := s.Status = stOK;
            if XLatFile <> nil then
              DisposeStr(XLatFile);
            XLatFile := nil;
            if UseXLat then
              XLatFile := NewStr(Nm);
          end;
        s.Done;
      end;
    if aFName = '' then
      begin
        if not GetToAsciiRecodeTable(KeyMap, XLAT) then
          NullXLAT(XLAT);
      end;
    if DoRedraw then
      DrawView;
  end { TFileViewer.SetXlatFile };
{-DataCompBoy-}

procedure TFileViewer.DoHighlite
  (var B; const s: String; const Attr: String);
  var
    i: integer;
    j: integer;
    k: integer;
    l: integer;
    C: Char;
  begin
    i := 1;
    j := 0;
    l := Length(s);
    while (i <= l) do
      begin
        C := s[i];
        k := i+1;
        while (k <= l) and (s[k] = C) do
          Inc(k);
        if (C <> #0) and (Ord(C) <= Length(Attr)) then
          MoveColor(TWordArray(B)[j], k-i, Ord(Attr[Ord(C)]));
        Inc(j, k-i);
        i := k;
      end;
  end { TFileViewer.DoHighlite };

{AK155 05.2001 Процедура CountDown почти полностью переписана;
части, соотвествующие vmAsm, собраны до кучи, но не проверялись}

procedure TFileViewer.CountDown;
  {$IFDEF DisAsm}
  var
    D: XCHGData; {GRM!}
    {$ENDIF}
  begin
    if Buf = nil then
      exit;
    case ViewMode of

      vmHex, vmDump:
        begin
          if not ScrollEOF and
            (FilePos+BufPos+HexPos*(Size.Y) >= FileSize)
          then
            exit;
          if FilePos+BufPos+HexPos*ANumber >= FileSize then
            exit;
          while (BufPos+HexPos < BufSize) and (ANumber > 0) do
            begin
              Inc(BufPos, HexPos);
              Dec(ANumber)
            end;
          if BufPos+HexPos*Size.Y > BufSize then
            Seek(FilePos+longInt(BufPos));
          Inc(BufPos, ANumber*HexPos);
        end {vmHex, vmDump};

      {$IFDEF DisAsm} {GRM!}

      vmAsm:
        begin
          if (Lines[ANumber].Pos < 0) or (Lines[Size.Y].Pos < 0)
          then
            exit;
          D.MemBuff := Buf;
          while (ANumber > 0) do
            begin
              D.InstrMaxLen := FileSize-FilePos-BufPos-1;
              D.Offset := BufPos;
              ScanCode(D);
              if BufPos+D.InstrLen+15*Size.Y > BufSize then
                Seek(FilePos+longInt(BufPos));
              if FilePos+BufPos+D.InstrLen >= FileSize then
                break;
              Inc(BufPos, D.InstrLen);
              Dec(ANumber);
            end;
          MakeLines;
          while (Cur.Y <> 0) and (Lines[Cur.Y].Pos < 0) do
            Dec(Cur.Y);
          Cur.X := 10;
        end {vmAsm};
      {$ENDIF} {GRM!}

        else{vmText}
        begin
          if not ScrollEOF and (ExposedPos+FilePos = FileSize) then
            exit;
          if ((MaxLines < Size.Y+ANumber-1) or (Lines[Size.Y+ANumber-1].
              Pos = -1))
            and (FilePos+BufSize < FileSize)
          then
            Seek(FilePos+Lines[0].Pos);
          if Lines[ANumber].Pos < 0 then
            exit;
          BufPos := Lines[ANumber].Pos;
          if ANumber <> 0 then
            begin
              if (ANumber+Size.Y < MaxLines) then
                begin
                  Move(Lines[ANumber], Lines[0], SizeOf(Lines[0])*(
                    MaxLines-ANumber));
                  Dec(MaxLines, ANumber);
                end
              else
                MakeLines;
            end;
        end {vmText};
    end {case};

    DrawView;
    MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
  end { TFileViewer.CountDown };

procedure TFileViewer.CountUp;
  var
    i, LineStart, LineEnd: longInt;
    NextLineStart: longInt;
    NewFilePos, ReadLen: longInt;
    {$IFDEF DisAsm}
    D: XCHGData; {GRM!}
    {$ENDIF}
    n: longInt;
    MaxX: longInt;

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

    var
      i, M, W: longInt;
      TS: integer;
      C1, C2: byte;
    begin
      TS := GetTabSize;
      LineEnd := NextLineStart;
      LineStart := LineEnd;
      if LineEnd <= 0 then
        exit;
      { Пропуск маркера конца }
      if (LineEnd > 0) then
        begin
          C1 := Buf^[LineEnd-1];
          if C1 in [$0D, $0A] then
            begin
              Dec(LineEnd);
              if LineEnd > 0 then
                begin
                  C2 := Buf^[LineEnd-1];
                  if (C2 in [$0D, $0A]) and (C1 <> C2) then
                    Dec(LineEnd);
                end;
            end;
        end;

      { Поиска начала строки в буфере. Ограничиться шириной экрана
  прямо сейчас нельзя, так как правильный учет табуляций возможен
  только при просмотре от самого начала строки.}
      LineStart := LineEnd;
      while (LineStart > 0) and not (Buf^[LineStart-1] in [$0D, $0A]) do
        begin
          if (LineEnd-LineStart >= MaxWrapW) then
            begin
              if Wrap = wmNone then
                exit;
              { Так бывает при просмотре файла без CR и LF. В этом
         случае границы строк чисто условные, так что нет никакого
         смысла сканировать на неизвестную глубину назад, а потом
         снова сканировать вперед, нарезая на куски максимальной
         длины. Так что теперь просто ищем от текущей точки назад
         начало экранной строки.}
              LineStart := LineEnd;
              W := MaxX;
              while W > 0 do
                begin
                  Dec(LineStart);
                  Dec(W);
                  if Buf^[LineStart-1] = 9 then
                    Dec(W, W mod TS);
                end;
              if Wrap = wmWords then
                begin
                  if (Char(Buf^[LineStart])) in BreakChars then
                    exit;
                  if (Char(Buf^[LineStart-1])) in BreakChars then
                      exit;
                  { граница экранной строки лежит внутри слова }
                  i := LineStart;
                  Inc(LineStart, 2);
                  while LineStart < LineEnd do
                    begin
                      if (Char(Buf^[LineStart])) in BreakChars then
                        exit;
                      Inc(LineStart);
                    end;
                  LineStart := i; { ну очень длинное слово }
                end;
              exit;
            end;
          Dec(LineStart);
        end;

      { Сейчас LineStart - настоящее начало строчки в буфере. Но с учетом wrap
   она может разделиться на несколько строчек экрана. Ниже это проверяется
   и в качестве LineStart возвращается начало последней из них. }

      M := LineStart; { M - кандидат на конец строки }
      W := 0;
      while M < LineEnd-1 do
        begin
          repeat
            Inc(W)
          until (Buf^[M] <> 9) or (W mod TS = 0);
          if W <= MaxX then
            Inc(M)
          else
            begin{ нужен разрез строки }
              if Wrap = wmWords then
                begin{ вернуться до границы слова }
                  i := M;
                  while i > LineStart do
                    begin
                      if Char(Buf^[i-1]) in BreakChars then
                        begin{ граница слова найдена }
                          M := i;
                          break;
                        end;
                      Dec(i);
                    end;
                end;
              LineStart := M;
              W := 0; { выполнили разрез }
            end;
        end;
    end { ScrollUp };
  {/AK155}

  begin { TFileViewer.CountUp }
    if Buf = nil then
      exit;
    (* {AK155 11-01-2003 эти бредовые проверки, вероятно, когда-то
        служили для компенсации  последствий каких-то багов.
        Но после того, как BufSize стал не $8000, а поболее, они
        стали в прежнем виде вредны, а обновлять их не хочется,
        так как самих багов, наверно, уже давно нет}
  if BufPos > 65000 then
    begin BufPos := 0; MakeLines; end; {AK155: IMHO так не бывает}
*)
    NextLineStart := BufPos;
    {$IFDEF DisAsm}
    D.MemBuff := Buf; {GRM!}
    D.Offset := BufPos; {GRM!}
    {$ENDIF}
    if Wrap > wmNone then
      MaxX := Size.X
    else
      MaxX := MaxWrapW;
    for n := 1 to ANumber do
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
            LineStart := Max(0, NextLineStart-HexPos);
            LineEnd := NextLineStart;
          end;
          {$IFDEF DisAsm}
          vmAsm: begin
            ScanUp(D);
            Dec(LineStart, D.InstrLen);
          end;
          {$ENDIF}
        end {case};
        {GRM!}
        if (ViewMode = vmText) and (LineEnd = Lines[0].Pos) and
          (LineEnd <> 0) {AK155 такое бывает в самом начале буфера}
          and (Lines[0].len+Lines[0].Pos-LineStart <= MaxX)
          {AK155 > MaxWrapW бывает при просмотре файла без CR и LF }
        then
          begin
            {AK155 При переходе от Hex/Dump к Text начало первой строчки
  экрана могло оказаться не началом строки в буфере. В момент смены
  режима на настоящее начало строки скакать было нехорошо,
  чтобы переключение режима не сбивало позицию, а сейчас надо
  таки перейти к настоящему началу строки. Реально такое
  может быть только при I=1 }
            Inc(Lines[0].len, Lines[0].Pos-LineStart);
            Lines[0].Pos := LineStart;
            BufPos := LineStart;
          end
        else{if ViewMode = vmText then }{/AK155}
          begin
            if LineStart > 0 then
              BufPos := LineStart { нормальное начало строки }
            else if FilePos = 0 then
              begin{ начало файла }
                if BufPos = 0 then
                  exit; { и так уже были в начале }
                BufPos := 0; { нормальное начало строки }
              end
            else
              begin
                  { начало буфера не в начале файла: сдвинуться по файлу
              на полбуфера и повторить поиск начала строки }
                NewFilePos := Max(0, FilePos-(BufSize div 2));
                ReadLen := FilePos-NewFilePos;
                Move(Buf^[0], Buf^[ReadLen], BufSize-ReadLen);
                Fl^.Seek(NewFilePos);
                Fl^.Read(Buf^, ReadLen);
                BreakOnStreamReadError;
                FilePos := NewFilePos;
                BufPos := ReadLen;
                for i := 0 to MaxLines do
                  begin
                    if Lines[i].Pos < 0 then
                      break;
                    Inc(Lines[i].Pos, ReadLen);
                    if Lines[i].Pos+Lines[i].len > BufSize then
                      begin
                        MaxLines := i-1;
                        if i = 0 then
                          break;
                        break;
                      end;
                  end;
                Inc(NextLineStart, ReadLen);
                goto 1;
              end;

            System.Move(Lines[0], Lines[1], (MaxLines-1)*SizeOf(
              Lines[0]));
            if MaxLines < MaxILines then
              Inc(MaxLines);
            Lines[0].Pos := LineStart;
            Lines[0].len := LineEnd-LineStart;
          end;

        NextLineStart := LineStart;
      end;
    {$IFDEF DisAsm}
    if ViewMode = vmAsm then
      Cur.X := 10; {GRM!}
    {$ENDIF}
    DrawView;
    MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
  end { TFileViewer.CountUp };

function TFileViewer.Valid;
  begin
    Valid := isValid or QuickView;
    if (Command = cmClose) or (Command = cmQuit) then
      if (Owner <> nil) and (not QuickView) then
        StoreViewInfo(Owner);
  end;

{ TFileWindow }
Constructor TFileWindow.Init;
  var
    R: TRect;
    P: PView;
    PV: PFileViewer;
  begin
    if LastViewerBounds.Empty or (InterfaceData.Options and
        ouiStoreViewerPosition = 0)
    then
      begin
        Desktop^.GetExtent(LastViewerBounds);
        LastViewerDeskSize := Desktop^.Size;
      end;
    R := LastViewerBounds;
    AdjustToDesktopSize(R, LastViewerDeskSize);
    TWindow.Init(R, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(
      VFileName), 0);
    Flags := Flags or wfMaxi;
    Options := Options or ofTileable;
    GetExtent(R);
    R.Grow(-1, -1);
    R.A.X := R.B.X;
    Inc(R.B.X);
    P := New(PViewScroll, Init(R));
    P^.GrowMode := gfGrowHiX+gfGrowLoX+gfGrowHiY;
    Insert(P);
    GetExtent(R);
    R.Grow(-1, -1);
    PV := New(PFileViewer, Init(R, nil, FileName, VFileName, P,
      False, Hex));
    Insert(PV);
      {Вставить надо даже при ошибке для последующего контроля }
    if not PV^.isValid then{AK155}
      exit;
    GetExtent(R);
    Inc(R.A.X);
    R.A.Y := R.B.Y-1;
    Dec(R.B.X, 2);
    P := New(PViewInfo, Init(R, PV));
    Insert(P);
    PV^.Info := P;
  end { TFileWindow.Init };

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
    ReactOnCmd := True
  end;

end.
