{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.12
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

unit DNApp;

interface

uses Objects, Drivers, Memory, HistList, Views, Menus, Dialogs, LFN,
     RStrings, Commands, xTime, Messages, Collect, Advance7, Startup
{$IFNDEF NONBP},StakDump{$ENDIF}
     ;

const

  EventsLen: Byte = 0;
  MaxEvents = 15;
var
  EventQueue: Array [1..MaxEvents] of TEvent;

const
  IdleWas : Boolean = false; { Used by GetEvent }

{ TApplication palette entries }

  apColor      = 0;
  apBlackWhite = 1;
  apMonochrome = 2;

{ TApplication palettes }

  { Turbo Vision 1.0 Color Palettes }

  CColor =                       { 8 - 12 - Terminal }
{ 01}#$71#$70#$78#$74#$20#$28#$24#$07#$0F#$0B#$F0#$F0#$78#$7F#$1F#$31 +
{ 17}#$3F#$3A#$13#$13#$3E#$21#$3F#$70#$7F#$7A#$13#$13#$70#$7F#$7E#$70 +
{ 33}#$7F#$7A#$13#$13#$70#$70#$7F#$7E#$8F#$8B#$9F#$78#$8E#$70#$78#$7F +
{ 49}#$7E#$0F#$9F#$0E#$9F#$79#$31#$31#$30#$0F#$3F#$31#$8F#$30#$3E#$30 +
{ 65}#$38#$3E#$0F#$07#$0E#$87#$8F#$8F#$8B#$8F#$F0#$87#$7F#$1C#$00#$87 +
{ 81}#$8F#$8B#$8F#$F0#$87#$8F#$8E#$30#$3E#$3F#$8F#$87#$70#$87#$8F#$3F +
{ 97}#$8F#$3F#$8E#$8E#$70#$8B#$87#$30#$3F#$0F#$3E#$0E#$31#$31#$00#$87 +
{113}#$8F#$8B#$8F#$F0#$87#$70#$8B#$8E#$8F#$87#$8F#$87#$8F#$70#$7F#$7A +
{129}#$13#$13#$70#$30#$79#$39#$7E#$7E#$7F#$70#$37#$3F#$3A#$13#$13#$30 +
{145}#$3E#$1E#$30#$38#$3E#$0F#$07#$0E#$87#$8F#$8F#$8B#$8F#$F0#$87#$70 +
{161}#$8E#$30#$3F#$83#$8E#$38#$3F#$3A#$31#$3F#$70#$8F#$8B#$8A#$83#$82 +
{177}#$8D#$9E#$8E#$8C#$89#$07#$7F#$03#$0F#$8F#$8F#$8E#$8F#$8D#$8B#$81 + {JO}
{193}#$86#$84#$85#$80#$8E#$8A#$83#$8F#$8D#$8B#$8E#$8A#$81#$3F#$3F#$3F + {JO}
{209}#$3F#$3F#$3F#$3F#$3F#$3F#$3F#$3F#$3F#$3F#$3F#$3F#$3F#$3F#$3F#$08 +
{225}#$CF#$CE#$0F#$0E;

  CBlackWhite =
        #$70#$70#$78#$7F#$07#$07#$0F#$07#$0F#$07#$70#$70#$07#$70#$0F +
    #$07#$0F#$07#$70#$70#$07#$70#$0F#$70#$7F#$7F#$70#$07#$70#$07#$0F +
    #$70#$7F#$7F#$70#$07#$70#$70#$7F#$7F#$07#$0F#$0F#$78#$0F#$78#$07 +
    #$0F#$0F#$0F#$70#$0F#$07#$70#$70#$70#$07#$70#$0F#$07#$07#$78#$00 +
    #$70#$78#$7F#$07#$07#$0F#$07#$0F#$0F#$0F#$70#$70#$07#$70#$07#$00 +  { 64 - 79 Editor}
    #$07#$0F#$0F#$70#$70#$07#$07#$0F#$70#$7F#$7F#$0F#$70#$70#$70#$7F +
    #$0F#$70#$07#$7F#$7F#$07#$70#$7F#$30#$3F#$1F#$30#$17#$3E#$3E#$00 +
    #$17#$1F#$1A#$31#$31#$17#$71 +
    #$1B#$1E#$1E#$1F#$17#$1F#$17 + {119 - 125 - File Info}
    #$70#$7F#$7A#$13#$13#$70#$30#$79#$39#$7E#$7E#$7F#$70 + {126 - 138 - File Find}
    #$07#$0F#$07#$70#$70#$07#$0F#$70 +
    #$70#$78#$7F#$07#$07#$0F#$07#$0F#$0F#$0F#$70#$70#$07#$70#$0F#$70 +
    #$0F#$07#$1F#$31#$31#$1B#$1F#$1E#$30#$3E#$3F#$1F#$17#$71#$30#$3F +
    #$3F#$1F#$1A#$31#$31#$1B#$1F#$1E#$30#$3E#$3F#$1F#$17#$71#$30#$3F +
    #$3F#$1F#$1A#$31#$31#$1B#$1F#$1E#$30#$3E#$3F#$1F#$17#$71#$30#$3F +
    #$7F#$1F#$1A#$31#$31#$1B#$1F#$1E#$30#$3E#$3F#$1F#$17#$71#$30#$3F;

  CMonochrome =
        #$70#$07#$07#$0F#$70#$70#$70#$07#$0F#$07#$70#$70#$07#$70#$00 +
    #$07#$0F#$07#$70#$70#$07#$70#$00#$70#$70#$70#$07#$07#$70#$07#$00 +
    #$70#$70#$70#$07#$07#$70#$70#$70#$0F#$07#$07#$0F#$70#$0F#$70#$07 +
    #$0F#$0F#$07#$70#$07#$07#$70#$07#$07#$07#$70#$0F#$07#$07#$70#$00 +
    #$70#$78#$7F#$07#$07#$0F#$07#$0F#$0F#$0F#$70#$70#$07#$70#$1C#$00 +  { 64 - 79 Editor}
    #$07#$0F#$0F#$70#$70#$07#$07#$0F#$70#$7F#$7F#$0F#$70#$70#$70#$7F +
    #$0F#$70#$07#$7F#$7F#$07#$70#$7F#$30#$3F#$1F#$30#$17#$3E#$3E#$00 +
    #$17#$1F#$1A#$31#$31#$17#$71 +
    #$1B#$1E#$1E#$1F#$17#$1F#$17 + {119 - 125 - File Info}
    #$70#$7F#$7A#$13#$13#$70#$30#$79#$39#$7E#$7E#$7F#$70 +
    #$07#$0F#$07#$70#$70#$07#$0F#$70 +
    #$70#$78#$7F#$07#$07#$0F#$07#$0F#$0F#$0F#$70#$70#$07#$70#$0F#$70 +
    #$0F#$07#$1F#$31#$31#$1B#$1F#$1E#$30#$3E#$3F#$1F#$17#$71#$30#$3F +
    #$3F#$1F#$1A#$31#$31#$1B#$1F#$1E#$30#$3E#$3F#$1F#$17#$71#$30#$3F +
    #$3F#$1F#$1A#$31#$31#$1B#$1F#$1E#$30#$3E#$3F#$1F#$17#$71#$30#$3F +
    #$7F#$1F#$1A#$31#$31#$1B#$1F#$1E#$30#$3E#$3F#$1F#$17#$71#$30#$3F;

  { Turbo Vision 2.0 Color Palettes }

  CAppColor =
        #$71#$70#$78#$74#$20#$28#$24#$17#$1F#$1A#$31#$31#$1E#$71#$1F +
    #$37#$3F#$3A#$13#$13#$3E#$21#$3F#$70#$7F#$7A#$13#$13#$70#$7F#$7E +
    #$70#$7F#$7A#$13#$13#$70#$70#$7F#$7E#$20#$2B#$2F#$78#$2E#$70#$30 +
    #$3F#$3E#$1F#$2F#$1A#$20#$72#$31#$31#$30#$2F#$3E#$31#$13#$38#$00 +
    #$17#$1F#$1A#$71#$71#$1E#$17#$1F#$1E#$20#$2B#$2F#$78#$2E#$10#$30 +
    #$3F#$3E#$70#$2F#$7A#$20#$12#$31#$31#$30#$2F#$3E#$31#$13#$38#$00 +
    #$37#$3F#$3A#$13#$13#$3E#$30#$3F#$3E#$20#$2B#$2F#$78#$2E#$30#$70 +
    #$7F#$7E#$1F#$2F#$1A#$20#$32#$31#$71#$70#$2F#$7E#$71#$13#$38#$00;

  CAppBlackWhite =
        #$70#$70#$78#$7F#$07#$07#$0F#$07#$0F#$07#$70#$70#$07#$70#$0F +
    #$07#$0F#$07#$70#$70#$07#$70#$0F#$70#$7F#$7F#$70#$07#$70#$07#$0F +
    #$70#$7F#$7F#$70#$07#$70#$70#$7F#$7F#$07#$0F#$0F#$78#$0F#$78#$07 +
    #$0F#$0F#$0F#$70#$0F#$07#$70#$70#$70#$07#$70#$0F#$07#$07#$78#$00 +
    #$07#$0F#$0F#$07#$70#$07#$07#$0F#$0F#$70#$78#$7F#$08#$7F#$08#$70 +
    #$7F#$7F#$7F#$0F#$70#$70#$07#$70#$70#$70#$07#$7F#$70#$07#$78#$00 +
    #$70#$7F#$7F#$70#$07#$70#$70#$7F#$7F#$07#$0F#$0F#$78#$0F#$78#$07 +
    #$0F#$0F#$0F#$70#$0F#$07#$70#$70#$70#$07#$70#$0F#$07#$07#$78#$00;

  CAppMonochrome =
        #$70#$07#$07#$0F#$70#$70#$70#$07#$0F#$07#$70#$70#$07#$70#$00 +
    #$07#$0F#$07#$70#$70#$07#$70#$00#$70#$70#$70#$07#$07#$70#$07#$00 +
    #$70#$70#$70#$07#$07#$70#$70#$70#$0F#$07#$07#$0F#$70#$0F#$70#$07 +
    #$0F#$0F#$07#$70#$07#$07#$70#$07#$07#$07#$70#$0F#$07#$07#$70#$00 +
    #$70#$70#$70#$07#$07#$70#$70#$70#$0F#$07#$07#$0F#$70#$0F#$70#$07 +
    #$0F#$0F#$07#$70#$07#$07#$70#$07#$07#$07#$70#$0F#$07#$07#$70#$00 +
    #$70#$70#$70#$07#$07#$70#$70#$70#$0F#$07#$07#$0F#$70#$0F#$70#$07 +
    #$0F#$0F#$07#$70#$07#$07#$70#$07#$07#$07#$70#$0F#$07#$07#$70#$00;

{ TBackground palette }

  CBackground = #1;

const
  SystemColors: array[apColor..apMonochrome] of String =
    (CColor, CBlackWhite, CMonochrome);

type
  PCacheCollection = ^TCacheCollection;
  TCacheCollection = object(TCollection)
    procedure FreeItem(Item: Pointer); virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
  end;


{ TBackground object }

  PBackground = ^TBackground;
  TBackground = object(TView)
    Pattern: Char;
    constructor Init(var Bounds: TRect; APattern: Char);
    constructor Load(var S: TStream);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure Store(var S: TStream);
  end;

{ TDesktop object }

  PDesktop = ^TDesktop;
  TDesktop = object(TGroup)
    Background: PBackground;
    TileColumnsFirst: Boolean;
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    procedure Cascade(var R: TRect);
    procedure Clear;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitBackground; virtual;
    procedure Store(var S: TStream);
    procedure Tile(var R: TRect);
    procedure TileError; virtual;
  end;

{ TProgram object }

  { Palette layout }
  {     1 = TBackground }
  {  2- 7 = TMenuView and TStatusLine }
  {  8-15 = TWindow(Blue) }
  { 16-23 = TWindow(Cyan) }
  { 24-31 = TWindow(Gray) }
  { 32-63 = TDialog }

  PProgram = ^TProgram;
  TProgram = object(TGroup)
    IdleSecs: TEventTimer;
    FullSpeed: boolean;
    {$IFDEF WIN32}
    {CS: CRITICAL_SECTION;}
    {$ENDIF}
    constructor Init;
    destructor Done; virtual;
    function CanMoveFocus: Boolean;
    function ExecuteDialog(P: PDialog; Data: Pointer): Word;
{$IFDEF SS}
    procedure InsertIdler;
    procedure InsertIdlerN(N: integer);
    procedure InsertAvIdlerN(Data: TSaversData; N: integer);
{$ENDIF}
    procedure GetEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Idle; virtual;
    procedure InitDesktop; virtual;
    procedure InitMenuBar; virtual;
    procedure InitScreen; virtual;
    procedure InitStatusLine; virtual;
    procedure InitCommandLine; virtual;
    function InsertWindow(P: PWindow): PWindow;
    procedure ActivateView(P: PView);
    procedure OutOfMemory; virtual;
    procedure PutEvent(var Event: TEvent); virtual;
    procedure Run; virtual;
    procedure SetScreenMode(Mode: Word);
    function ValidView(P: PView): PView;
    procedure Redraw; virtual;
    Procedure BFSpeed; {DataCompBoy}
    Procedure EFSpeed; {DataCompBoy}
   end;

{ TApplication object }

  PApplication = ^TApplication;
  TApplication = object(TProgram)
    Clock: PView;
    constructor Init;
    destructor Done; virtual;
    procedure Cascade;
    procedure ShowUserScreen;
    procedure WhenShow; virtual;
    procedure GetTileRect(var R: TRect); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Tile;
  end;

{ Interface and resource procedures }

procedure UpdateWriteView(P: Pointer);
procedure InitStringCache;
procedure OpenResource;
function  ExecResource( Key: TDlgIdx; var Data): Word;
function  LoadResource( Key: TDlgIdx ): PObject;
function  GlobalMessage(What, Command: Word; InfoPtr: Pointer):pointer;
function  GlobalMessageL(What, Command: Word; InfoLng: LongInt):pointer;
procedure GlobalEvent(What, Command: Word; InfoPtr: Pointer);
function  ViewPresent(Command: Word; InfoPtr: Pointer): PView;
function  WriteMsg(Text: String): PView;
function _WriteMsg(const Text: String): PView;
procedure ForceWriteShow(P: Pointer);
function  GetString( Index: TStrIdx ): String;
procedure ToggleCommandLine(OnOff: Boolean);
procedure AdjustToDesktopSize(var R: TRect; OldDeskSize: TPoint);

const

{ Public variables }

  StringCache: PCollection = nil;
  Application: PProgram = nil;
  Desktop: PDesktop = nil;
  StatusLine: PStatusLine = nil;
  MenuBar: PMenuView = nil;
  ResourceStream: PStream = nil;
  LngStream: PStream = nil;
  LStringList : PStringList = nil;
  Resource:  PIdxResource = nil;
  AppPalette: Integer = apColor;
  CommandLine: PView = nil;
{  HintString: String[80] = '';}

const SkyVisible: Boolean = False;

type
   PWriteWin = ^TWriteWin;
   TWriteWin = object(TWindow)
     Tmr: TEventTimer;
     IState: Byte;
   end;

implementation

uses Dos, DNHelp, advance, advance1, advance2, VideoMan,
     ExtraMem, UserMenu, MicroEd, FViewer, DblWnd, CmdLine
{$IFDEF SS},Idlers{$ENDIF}
{$IFDEF VIRTUALPASCAL}, VpSysLow {$ENDIF}
     ;

var

{ Private variables }

  LastComTime: TEventTimer;

procedure ToggleCommandLine(OnOff: Boolean);
begin
  if (CommandLine <> nil) and (CommandLine^.GetState(sfVisible) xor OnOff) then
    Message(Application, evCommand, cmHideCmdLine, nil);
end;

procedure AdjustToDesktopSize(var R: TRect; OldDeskSize: TPoint);
  var KX, KY: Real;
begin
  if OldDeskSize.X = 0 then OldDeskSize.X := Desktop^.Size.X;
  if OldDeskSize.Y = 0 then OldDeskSize.Y := Desktop^.Size.Y;
  if Desktop^.Size.Equals(OldDeskSize) then Exit;
  KX := Desktop^.Size.X/OldDeskSize.X;
  R.A.X := Trunc(R.A.X*KX); R.B.X := Trunc(R.B.X*KX);
  KY := Desktop^.Size.Y/OldDeskSize.Y;
  R.A.Y := Trunc(R.A.Y*KY); R.B.Y := Trunc(R.B.Y*KY);
  if R.Empty or (R.A.X >= Desktop^.Size.X) or (R.B.X < 0)
             or (R.A.Y >= Desktop^.Size.Y) or (R.B.Y < 0)
             or (R.B.X - R.A.X < MinWinSize.X)
             or (R.B.Y - R.A.Y < MinWinSize.Y) then Desktop^.GetExtent(R);
end;

{ TBackground }

constructor TBackground.Init(var Bounds: TRect; APattern: Char);
begin
  TView.Init(Bounds);
  GrowMode := gfGrowHiX + gfGrowHiY;
  Pattern := APattern;
end;

constructor TBackground.Load(var S: TStream);
begin
  TView.Load(S);
  S.Read(Pattern, SizeOf(Pattern));
end;

procedure TBackground.Draw;
var
  B, B1: TDrawBuffer;
  I, H, Src: Integer;
begin
  MoveChar(B, ' ', $07, Size.X);

  if UserScreen = nil
    then WriteLine(0, 0, Size.X, Size.Y, B)
    else begin
      H := (UserScreenSize div (UserScreenWidth*2));
      Src := 0;
      If (InterfaceData.Options and ouiHideMenu = 0) and (Size.Y <> Application^.Size.Y) then
      begin
        Inc( Src, UserScreenWidth );
        Dec( H );
      end;
      For I := 0 to H-1 do begin
        B1 := B;
        Move( PWordArray( UserScreen )^[ Src ], B1, UserScreenWidth * 2 );
        WriteLine( 0, I, Size.X, 1, B1 );
        Inc( Src, UserScreenWidth );
      end;
      WriteLine(0, H, Size.X, Size.Y - H + 1, B);
    end;
end;

function TBackground.GetPalette: PPalette;
const
  P: string[Length(CBackground)] = CBackground;
begin
  GetPalette := @P;
end;

procedure TBackground.Store(var S: TStream);
begin
  TView.Store(S);
  S.Write(Pattern, SizeOf(Pattern));
end;

{ TDesktop object }

constructor TDesktop.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
  GrowMode := gfGrowHiX + gfGrowHiY;
  HelpCtx := hcDesktop;
  InitBackground;
  if Background <> nil then Insert(Background);
end;

constructor TDesktop.Load(var S: TStream);
begin
  inherited Load(S);
  GetSubViewPtr(S, Background);
  S.Read(TileColumnsFirst, SizeOf(TileColumnsFirst));
end;

function Tileable(P: PView): Boolean;
begin
  Tileable := (P^.Options and ofTileable <> 0) and
    (P^.State and sfVisible <> 0);
end;

procedure TDesktop.Clear;
  procedure CloseView(P: PView); {$IFDEF BIT_16}far;{$ENDIF}
  begin Message(P, evCommand, cmClose, nil) end;
begin
  if Desktop^.Valid(cmClose) then Desktop^.ForEach(@CloseView);
end;

procedure TDesktop.Cascade(var R: TRect);
var
  CascadeNum: Integer;
  LastView: PView;
  Min, Max: TPoint;

procedure DoCount(P: PView); {$IFDEF BIT_16}far;{$ENDIF}
begin
  if Tileable(P) then
  begin
    Inc(CascadeNum);
    LastView := P;
  end;
end;

procedure DoCascade(P: PView); {$IFDEF BIT_16}far;{$ENDIF}
var
  NR: TRect;
begin
  if Tileable(P) and (CascadeNum >= 0) then
  begin
    NR.Copy(R);
    Inc(NR.A.X, CascadeNum); Inc(NR.A.Y, CascadeNum);
    P^.Locate(NR);
    Dec(CascadeNum);
  end;
end;

begin
  CascadeNum := 0;
  ForEach(@DoCount);
  if CascadeNum > 0 then
  begin
    LastView^.SizeLimits(Min, Max);
    if (Min.X > R.B.X - R.A.X - CascadeNum) or
       (Min.Y > R.B.Y - R.A.Y - CascadeNum) then TileError
    else
    begin
      Dec(CascadeNum);
      Lock;
      ForEach(@DoCascade);
      Unlock;
    end;
  end;
end;

procedure TDesktop.HandleEvent(var Event: TEvent);
 var PCur, PNext: PView;
    E: TEvent;

  procedure _Next; begin SelectNext(False); end;
  procedure _Prev; begin if Valid(cmReleasedFocus) and (Current<>nil) then Current^.PutInFrontOf(Background); end;

  procedure _NextSame;
   var Old: PView;
   begin
    Old:=Desktop^.Current;
    Lock;
    Repeat _Next
    until (Old=Desktop^.Current) or (TypeOf(Desktop^.Current^)=TypeOf(Old^));
    UnLock;
   end;

  procedure _PrevSame;
   var Old: PView;
   begin
    Old:=Desktop^.Current;
    Lock;
    Repeat _Prev;
    until (Old=Desktop^.Current) or (TypeOf(Desktop^.Current^)=TypeOf(Old^));
    UnLock;
  end;

  procedure _NextOther;
   var Old: PView;
   begin
    Old:=Desktop^.Current;
    Lock;
    Repeat _Next
    until (Old=Desktop^.Current) or (TypeOf(Desktop^.Current^)<>TypeOf(Old^));
    UnLock;
   end;

  procedure _PrevOther;
   var Old: PView;
   begin
    Old:=Desktop^.Current;
    Lock;
    Repeat _Prev;
    until (Old=Desktop^.Current) or (TypeOf(Desktop^.Current^)<>TypeOf(Old^));
    UnLock;
  end;

begin
  TGroup.HandleEvent(Event);
  if Event.What = evCommand then
  begin
    case Event.Command of
     cmNext: _Next;
     cmPrev: _Prev;
     cmNext2:  _NextSame;
     cmPrev2:  _PrevSame;
     cmNext3:  _NextOther;
     cmPrev3:  _PrevOther;
     else      Exit;
    end;
    ClearEvent(Event);
  end;
end;

procedure TDesktop.InitBackground;
var
  R: TRect;
begin
  GetExtent(R);
  New(Background, Init(R, #176));
end;

function ISqr(X: Integer): Integer; assembler;
{$IFNDEF BIT_32}
asm
        MOV     CX,X
        XOR     BX,BX
@@1:    INC     BX
        MOV     AX,BX
        IMUL    AX
        CMP     AX,CX
        JLE     @@1
        MOV     AX,BX
        DEC     AX
end;
{$ELSE BIT_32}{&Frame-}{$USES EBX, ECX}
asm
        MOV     ECX,X
        XOR     EBX,EBX
@@1:    INC     EBX
        MOV     EAX,EBX
        IMUL    EAX
        CMP     EAX,ECX
        JLE     @@1
        MOV     EAX,EBX
        DEC     EAX
end;
{$ENDIF}

procedure MostEqualDivisors(N: Integer; var X, Y: Integer; FavorY: Boolean);
var
  I: Integer;
begin
  I := ISqr(N);
  if ((N mod I) <> 0) then
    if (N mod (I+1)) = 0 then Inc(I);
  if I < (N div I) then I := N div I;
  if FavorY then
  begin
    X := N div I;
    Y := I;
  end
  else
  begin
    Y := N div I;
    X := I;
  end;
end;

procedure TDesktop.Store(var S: TStream);
begin
  inherited Store(S);
  PutSubViewPtr(S, Background);
  S.Write(TileColumnsFirst, SizeOf(TileColumnsFirst));
end;

procedure TDesktop.Tile(var R: TRect);
var
  NumCols, NumRows, NumTileable, LeftOver, TileNum: Integer;

procedure DoCountTileable(P: PView); {$IFDEF BIT_16}far;{$ENDIF}
begin
  if Tileable(P) then Inc(NumTileable);
end;

function DividerLoc(Lo, Hi, Num, Pos: Integer): Integer;
begin
  DividerLoc := LongDiv(LongMul(Hi - Lo, Pos), Num) + Lo;
end;

procedure CalcTileRect(Pos: Integer; var NR: TRect);
var
  X,Y,D: Integer;
begin
  D := (NumCols - LeftOver) * NumRows;
  if Pos < D then
  begin
    X := Pos div NumRows;
    Y := Pos mod NumRows;
  end else
  begin
    X := (Pos - D) div (NumRows + 1) + (NumCols - LeftOver);
    Y := (Pos - D) mod (NumRows + 1);
  end;
  NR.A.X := DividerLoc(R.A.X, R.B.X, NumCols, X);
  NR.B.X := DividerLoc(R.A.X, R.B.X, NumCols, X+1);
  if Pos >= D then
  begin
    NR.A.Y := DividerLoc(R.A.Y, R.B.Y, NumRows+1, Y);
    NR.B.Y := DividerLoc(R.A.Y, R.B.Y, NumRows+1, Y+1);
  end else
  begin
    NR.A.Y := DividerLoc(R.A.Y, R.B.Y, NumRows, Y);
    NR.B.Y := DividerLoc(R.A.Y, R.B.Y, NumRows, Y+1);
  end;
end;

procedure DoTile(P: PView); {$IFDEF BIT_16}far;{$ENDIF}
var
  R: TRect;
begin
  if Tileable(P) then
  begin
    CalcTileRect(TileNum, R);
    P^.Locate(R);
    Dec(TileNum);
  end;
end;

begin
  NumTileable := 0;
  ForEach(@DoCountTileable);
  if NumTileable > 0 then
  begin
    MostEqualDivisors(NumTileable, NumCols, NumRows, not TileColumnsFirst);
    if ((R.B.X - R.A.X) div NumCols = 0) or
       ((R.B.Y - R.A.Y) div NumRows = 0) then TileError
    else
    begin
      LeftOver := NumTileable mod NumCols;
      TileNum := NumTileable-1;
      Lock;
      ForEach(@DoTile);
      Unlock;
    end;
  end;
end;

procedure TDesktop.TileError;
begin
end;

{ TProgram }

constructor TProgram.Init;
var
  R: TRect;
begin
  Application := @Self;
  InitScreen;
  R.Assign(0, 0, ScreenWidth, ScreenHeight);
  TGroup.Init(R);
  State := sfVisible + sfSelected + sfFocused + sfModal + sfExposed;
  Options := 0;
  Buffer := ScreenBuffer;
  InitStatusLine;
  InitMenuBar;
  InitDesktop;
  if StatusLine <> nil then Insert( StatusLine );
  if MenuBar <> nil then Insert(MenuBar);
  if Desktop <> nil then Insert(Desktop);
  InitCommandLine;
  if StatusLine <> nil then StatusLine^.GrowTo(StatusLine^.Size.X, 1);
  if MenuBar <> nil then MenuBar^.GrowTo(MenuBar^.Size.X, 1);
  NewTimer(LastComTime, 0);
  NewTimer(IdleSecs, 0);
end;

destructor TProgram.Done;
begin
  if MenuBar    <> nil then Dispose(MenuBar,Done);    MenuBar:=nil;
  if StatusLine <> nil then Dispose(StatusLine,Done); StatusLine:=nil;
  if Desktop    <> nil then Dispose(Desktop,Done);    DeskTop:=nil;
  Application := nil;
  inherited Done;
end;

function TProgram.CanMoveFocus: Boolean;
begin
  CanMoveFocus := Desktop^.Valid(cmReleasedFocus);
end;

function TProgram.ExecuteDialog(P: PDialog; Data: Pointer): Word;
var
  C: Word;
begin
  ExecuteDialog := cmCancel;
  if ValidView(P) <> nil then
  begin
    if Data <> nil then P^.SetData(Data^);
    C := {Desktop^.}ExecView(P);
    if (C <> cmCancel) and (Data <> nil) then P^.GetData(Data^);
    Dispose(P,Done);
    ExecuteDialog := C;
  end;
end;

procedure ExecExit;
   var S: String; {DataCompBoy}
begin
  {if SSaver <> nil then SSaver^.Free;}
  S := GetEnv('DNIDLE');
  if S = '' then begin TottalExit:=true; Application^.Done; Halt(0); end
            else Message(Application, evCommand, cmExecString, @S);
end;

{$IFDEF SS}
procedure TProgram.InsertIdler;
begin
  if (SkyEnabled > 0) or SaversData.Mouse and
  (MouseWhere.Y= Size.Y-1) and (MouseWhere.X = Size.X-1)
   or (SaversData.Selected.List = nil) or
      (SaversData.Selected.List^.Count = 0) or
      (SSaver <> nil) then Exit;
  InsertIdlerN(Random(SaversData.Selected.List^.Count));
end;

procedure TProgram.InsertIdlerN;
var I: Integer;
    S: String;
    R: TRect;
    Event: TEvent;
    MX, MY: Word;
begin
  if (SkyEnabled > 0) or SaversData.Mouse and
  (MouseWhere.Y= Size.Y-1) and (MouseWhere.X = Size.X-1)
   or (SaversData.Selected.List = nil) or
      (SaversData.Selected.List^.Count = 0) or
      (SSaver <> nil) then Exit;

  S := PString(SaversData.Selected.List^.At(N))^;
  if S[1] = #249 then
    begin
      Application^.GetExtent(R);
      case S[3] of
         'F': SSaver := New(PProjector, Init);
         'B': SSaver := New(PSSaver, Init(R));
         'C': SSaver := New(PClockSaver, Init);
         else SSaver := New(PStarSkySaver, Init);
      end;
      if (SSaver <> nil) then
       begin
         if ExecView(SSaver) = cmCancel then ExecExit;
         Dispose(SSaver,Done);
         SSaver:=nil;
       end;
    end else begin
               CallExternalSaver(S);
             end;
end;

procedure TProgram.InsertAvIdlerN;
var I: Integer;
    S: String;
    R: TRect;
    Event: TEvent;
    MX, MY: Word;
begin
  if (SkyEnabled > 0) or SaversData.Mouse and
  (MouseWhere.Y= Size.Y-1) and (MouseWhere.X = Size.X-1)
   or (Data.Available.List = nil) or
      (Data.Available.List^.Count = 0) or
      (SSaver <> nil) then Exit;

  S := PString(Data.Available.List^.At(N))^;
  if S[1] = #249 then
    begin
      Application^.GetExtent(R);
      case S[3] of
         'F': SSaver := New(PProjector, Init);
         'B': SSaver := New(PSSaver, Init(R));
         'C': SSaver := New(PClockSaver, Init);
         else SSaver := New(PStarSkySaver, Init);
      end;
      if (SSaver <> nil) then
       begin
         if ExecView(SSaver) = cmCancel then ExecExit;
         Dispose(SSaver,Done);
         SSaver:=nil;
       end;
    end else CallExternalSaver(S);
end;
{$ENDIF}

procedure TProgram.GetEvent(var Event: TEvent);

function ContainsMouse(P: PView): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
begin
  ContainsMouse := (P^.State and sfVisible <> 0) and
    P^.MouseInView(Event.Where);
end;

var
    P: Procedure;
    R: TRect;
    A1, A2: {$IFNDEF VIRTUALPASCAL}Word{$ELSE}SmallWord{$ENDIF};
label 11;
begin
11:
  {$IFDEF VIRTUALPASCAL}
  ShiftState:=SysTVGetShiftState;
  {$ENDIF}
  if EventsLen > 0 then
  begin
    Event := EventQueue[1];
    Dec(EventsLen);
    Move(EventQueue[2], EventQueue[1], SizeOf(Event)*EventsLen);
    NewTimer(IdleSecs, 0);
    SliceAwake;
  end else
  begin
    EventsLen := 0;
    GetMouseEvent(Event);
    if Event.What = evNothing then
    begin
      GetKeyEvent(Event);
      if Event.What = evNothing then
       begin
         Idle;
         if EventsLen > 0 then goto 11;
         if (ElapsedTimeInSecs(IdleSecs) > 3600) and
            (StartupData.Unload and osuInactivityExit <> 0) then
           begin
             ExecExit;
           end;
       end else
      begin
        NewTimer(IdleSecs, 0);
        SliceAwake;
      end;
    end else
    begin
      NewTimer(IdleSecs, 0);
      SliceAwake;
    end;
  end;

  if (Event.What <> evNothing) or (SkyEnabled <> 0) then NewTimer(LastComTime, 0) else
{$IFDEF SS}
  if (SSaver = nil) and (SkyEnabled=0) and (SkyDelay <> 255) and
     (ElapsedTimeInSecs(LastComTime) >= SkyDelay*60) then
           begin
            InsertIdler;
            NewTimer(LastComTime, 0)
           end;
{$ENDIF}
  if (StatusLine <> nil)
{$IFDEF SS}
     and (SSaver = nil)
{$ENDIF}
   then
    if (Event.What and evMouseDown <> 0) and (Event.Buttons and mbLeftButton <> 0) and
       (FirstThat(@ContainsMouse) = PView(StatusLine)) then
        StatusLine^.HandleEvent(Event);

  if (IdleWas) and
     (SkyEnabled = 0) and
{$IFDEF SS}
     (SSaver = nil) and
{$ENDIF}
     (CommandLine <> nil) and
     (CommandLine^.Size.Y <> 0) and
     (CommandLine^.GetState(sfVisible)) then
  begin
    IdleWas := False;
    {$IFDEF VIRTUALPASCAL}
    SysGetCurPos(A1, A2);
    {$ELSE}
    asm
     mov ah,3
     xor bx,bx
     int 10h
     mov A1, dx
     mov A2, cx
    end;
    {$ENDIF}
    if ((Hi(A2) = $20) or (Hi(A1) = Size.Y - 1 - Byte( InterfaceData.Options and ouiHideStatus = 0 )))
       then CommandLine^.Update;
  end;
end;

function TProgram.GetPalette: PPalette;
begin
  GetPalette := @SystemColors[AppPalette];
end;

procedure TProgram.Redraw;
begin
 if (Size.X <> ScreenWidth) or (Size.Y <> ScreenHeight) then
  GrowTo(ScreenWidth, ScreenHeight) else inherited Redraw;
end;

procedure TProgram.HandleEvent(var Event: TEvent);
var
  I: Word;
  C: Char;
begin
  if (Event.What = evKeyDown) and
     (Event.KeyCode>=kbCtrlShift1) and
     (Event.KeyCode<=kbCtrlShift9)
   then if MessageL(Desktop, evBroadCast,
                    cmSelectWindowNum,
                    (Event.KeyCode - kbCtrlShift1) shr 8 + 1) <> nil
         then ClearEvent(Event);
  TGroup.HandleEvent(Event);
  if Event.What = evCommand then
    if Event.Command = cmQuit then
    begin
      EndModal(cmQuit);
      ClearEvent(Event);
    end;
end;

procedure TProgram.Idle;
 var P: PView;
     E: TEvent;
     A1, A2: Word;
begin
  if StatusLine <> nil then StatusLine^.Update;
  if CommandSetChanged then
  begin
    Message(@Self, evBroadcast, cmCommandSetChanged, nil);
    CommandSetChanged := False;
  end;
end;

procedure TProgram.InitDesktop;
var
  R: TRect;
begin
  GetExtent(R);
  if SystemData.Options and ouiHideMenu = 0 then Inc(R.A.Y);
  if SystemData.Options and ouiHideStatus = 0 then Dec(R.B.Y);
  New(Desktop, Init(R));
end;

procedure TProgram.InitMenuBar;
var
  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenuBar, Init(R, nil));
end;

procedure TProgram.InitScreen;
begin
  if ScreenMode <> smMono then
  begin
    if ScreenWidth div ScreenHeight < 2 then
      ShadowSize.X := 1 else
      ShadowSize.X := 2;
    ShadowSize.Y := 1;
    ShowMarkers := False;
    if ScreenMode = smBW80 then
      AppPalette := apBlackWhite else
      AppPalette := apColor;
  end else
  begin
    ShadowSize.X := 0;
    ShadowSize.Y := 0;
    ShowMarkers := True;
    AppPalette := apMonochrome;
  end;
end;

procedure TProgram.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  New(StatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit,
      nil), nil)));
end;

procedure TProgram.InitCommandLine;
begin
end;

function TProgram.InsertWindow(P: PWindow): PWindow;
 var PP: TPoint;
     EV: TEvent;
     PV: PView;
begin
  InsertWindow := nil;
  if ValidView(P) <> nil then
    if CanMoveFocus then
    begin
      Desktop^.Insert(P);
      InsertWindow := P;
      {P^.Select;}
      ActivateView(P);
    end
    else
      Dispose(P,Done);
  Idle;
end;

procedure TProgram.ActivateView;
 var EV: TEvent;
     PP: TPoint;
begin
  If P=nil then exit;
  EV.What := evMouseDown;
  PP.X := P^.Origin.X;
  PP.Y := P^.Origin.Y + P^.Size.Y - 2;
  Desktop^.MakeGlobal(PP, PP); EV.Where := PP;
  EV.Buttons := mbRightButton; {PutEvent(EV);}
end;

procedure TProgram.OutOfMemory;
begin
end;

procedure TProgram.PutEvent(var Event: TEvent);
begin
  if (Event.What = evNothing) then Exit;
  if (EventsLen = MaxEvents) then
    Move(EventQueue[2],EventQueue[1],(MaxEvents-1)*SizeOf(Event))
   else Inc(EventsLen);
  EventQueue[EventsLen] := Event;
end;

procedure TProgram.Run;
begin
  Execute;
end;

procedure TProgram.SetScreenMode;
var
  R: TRect;
begin
   HideMouse;
   DoneEvents;
   DoneMemory;
   SetVideoMode(Mode);
   InitMemory;
   InitEvents;
   InitScreen;
   Buffer:=ScreenBuffer;
   R.Assign(0, 0, ScreenWidth, ScreenHeight);
   ChangeBounds(R);
   ShowMouse;
   SetBlink(CurrentBlink);
end;

function TProgram.ValidView(P: PView): PView;
begin
  ValidView := nil;
  if P <> nil then
  begin
    if LowMemory then
    begin
      Dispose(P,Done);
      OutOfMemory;
      Exit;
    end;
    if not P^.Valid(cmValid) then
    begin
      Dispose(P,Done);
      Exit;
    end;
    ValidView := P;
  end;
end;

Procedure TProgram.BFSpeed; {DataCompBoy}
begin
 {$IFDEF WIN32}
 {EnterCriticalSection(CS);}
 {$ELSE}
 if OpSys and (opWNT+opWIN) <> 0 then asm
  mov AX, 1681h
  int 21h
 end;
 {$ENDIF}
 FullSpeed:=True;
end;

Procedure TProgram.EFSpeed; {DataCompBoy}
begin
 {$IFDEF WIN32}
 {LeaveCriticalSection(CS);}
 {$ELSE}
 if OpSys and (opWNT+opWIN) <> 0 then asm
  mov AX, 1682h
  int 21h
 end;
 {$ENDIF}
 FullSpeed:=False;
end;

{ TApplication }

constructor TApplication.Init;
begin
  InitMemory;
  InitVideo;
  InitEvents;
  InitHistory;
  TProgram.Init;
end;

destructor TApplication.Done;
begin
  if LStringList <> nil then Dispose(LStringList,Done); LStringList:=nil;
  if LngStream <> nil then Dispose(LngStream,Done); LngStream:=nil;
  if Resource <> nil then Dispose(Resource,Done); Resource:=nil;
  TProgram.Done;
  DoneHistory;
  DoneSysError;
  DoneEvents;
  DoneVideo;
  DoneMemory;
end;

procedure TApplication.ShowUserScreen;
 var B, BB: Word;
     PV: PView;
     R: TRect;
begin
 GetExtent(R);
 PV := New(PBackground, Init(R, ' '));
 if CommandLine <> nil then CommandLine^.SetState(sfDisabled, On);
 {$IFDEF OS_DOS}
 asm
    mov cx, $2020
    xor bx, bx
    mov ah, 1
    push bp
    int 10h
    pop  bp
 end;
 {$ENDIF}
 Insert(PV);
 if (StartupData.Load and osuResetPalette <> 0) and VGASystem
  then SetPalette(vga_default);
 WhenShow;
 if CommandLine <> nil then CommandLine^.SetState(sfDisabled, Off);
 if (StartupData.Load and osuResetPalette <> 0) and VGASystem
  then SetPalette(vga_palette);
 PV^.Free;

 (*
 B := ScreenMode; BB := CursorLines;
 HideMouse;
 Inc(SkyEnabled); SkyVisible := On;
 DoneVideo;
{ if B <> StartupMode then SetVideoMode(StartupMode);}
 WhenShow;
 InitVideo; SetBlink(Off); Dec(SkyEnabled); SkyVisible := Off;
{ if B <> StartupMode then SetScreenMode(B);}
 InitVideo; CursorLines := BB;
 Redraw; ShowCursor; ShowMouse;
 *)
end;

procedure TApplication.WhenShow;
 var Event: TEvent;
begin
 repeat
   GetEvent(Event);
 until (Event.What and (evKeyDown + evMouseDown) <> 0);
 ClearEvent(Event);
end;

procedure TApplication.Cascade;
var
  R: TRect;
begin
  GetTileRect(R);
  if Desktop <> nil then Desktop^.Cascade(R);
end;

procedure TApplication.GetTileRect(var R: TRect);
begin
  Desktop^.GetExtent(R);
end;

procedure TApplication.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      begin
        case Event.Command of
          cmTile: Tile;
          cmCascade: Cascade;
        else
          Exit;
        end;
        ClearEvent(Event);
      end;
    evKeyDown: if Event.KeyCode=kbCtrlShiftTilde then begin
                MessageBox('Status dumped to DN.ERR',nil,0);
                ClearEvent(Event);
               end;
  end;
end;

procedure TApplication.Tile;
var
  R: TRect;
begin
  GetTileRect(R);
  if Desktop <> nil then Desktop^.Tile(R);
end;

procedure ResourceFail(const S: string);
begin
  WriteLn('FATAL: Could not open resource file ('+S+')');
  Halt(0);
end;


procedure OpenResource;
 var S: String;
begin
 if Resource <> nil then Exit;
 S := GetEnv('DNDLG'); if S = '' then S := SourceDir;
 if S[Byte(S[0])] <> '\' then S := S + '\';
 ResourceStream := New(PBufStream,
                       Init(S + LngId + '.DLG', stOpenPacked, 1024)
                      );
 if ResourceStream^.Status <> stOK then begin
  Dispose(ResourceStream,Done);
  ResourceStream := New(PBufStream,
                        Init(StartupDir + LngId + '.DLG', stOpenPacked, 1024)
                       );
 end;
 if ResourceStream^.Status <> stOK then ResourceFail(LngId + '.DLG');
 New(Resource, Init(ResourceStream));
end;

function ExecResource;
 var D: PDialog;
     W: Word;
begin
 ExecResource := cmCancel; OpenResource;
 if Resource = nil then Exit;
 D := PDialog(Resource^.Get( Key ));
 D := PDialog(Application^.ValidView(D));
 if D = nil then Exit;
 D^.SetData(Data);
 W := Desktop^.ExecView(D);
 if W <> cmCancel then D^.GetData(Data);
 ExecResource := W;
 Dispose(D,Done);
end;

function LoadResource;
begin
 LoadResource := nil; OpenResource;
 if Resource = nil then Exit;
 LoadResource := Resource^.Get(Key);
end;

function GlobalMessage;

 var Event: TEvent;
     Ptr : Pointer ;

 procedure Action(View: PView);{$IFDEF BIT_16}far;{$ENDIF}
 begin
  Event.What := What;
  Event.Command := Command;
  Event.InfoPtr := InfoPtr;
  View^.HandleEvent(Event);
  if Event.What = evNothing then Ptr := Event.InfoPtr ;

 end;

begin
 Ptr := Nil ;
 if Command = cmPanelReread then Command := cmTotalReread;
 Application^.ForEach(@Action);
 Desktop^.ForEach(@Action);
 GlobalMessage := Ptr ;
end;

function GlobalMessageL;

 var Event: TEvent;
     Ptr : Pointer ;

 procedure Action(View: PView);{$IFDEF BIT_16}far;{$ENDIF}
 begin
  Event.What := What;
  Event.Command := Command;
  Event.InfoLong := InfoLng;
  View^.HandleEvent(Event);
  if Event.What = evNothing then Ptr := Event.InfoPtr ;

 end;

begin
 Ptr := Nil ;
 if Command = cmPanelReread then Command := cmTotalReread;
 Application^.ForEach(@Action);
 Desktop^.ForEach(@Action);
 GlobalMessageL := Ptr ;
end;

procedure GlobalEvent;

 var Event: TEvent;

 procedure Action(View: PView);{$IFDEF BIT_16}far;{$ENDIF}
 begin
  if (Event.What <> evNothing) and (View <> nil) then
   View^.HandleEvent(Event);
 end;

begin
 Event.What := What;
 if Command = cmPanelReread then Event.Command := cmTotalReread
                            else Event.Command := Command;
 Event.InfoPtr := InfoPtr;
 Application^.ForEach(@Action);
 Desktop^.ForEach(@Action);
end;

var ActInt: Boolean;
    V: PView;
    Event: TEvent;

function ViewPresent;

 procedure FAction(View: PView);{$IFDEF BIT_16}far;{$ENDIF}
 begin
  if ActInt then Exit;
  View^.HandleEvent(Event);
  if Event.What = evNothing then V := Event.InfoPtr;
 end;

begin
 V := nil; ActInt := False;
 Event.What := evCommand;
 Event.Command := Command;
 Event.InfoPtr := InfoPtr;
 Application^.ForEach(@FAction);
 Desktop^.ForEach(@FAction);
 ViewPresent := V;
end;

procedure UpdateWriteView(P: Pointer);
begin
  if P = nil then Exit;
  with PWriteWin(P)^ do
  begin
    case IState of
      0: if TimerExpired(Tmr) then Desktop^.Insert(P) else Exit;
      1: if TimerExpired(Tmr) then Application^.Insert(P) else Exit;
      2: Exit;
    end;
    IState := 2;
  end;
end;


function WriteMsg;
 var W: PWriteWin;
     R: TRect;
     P: PStaticText;
begin
 R.Assign(1,1,30,8);
 New(W,Init(R, '', wnNoNumber));
 W^.Options := W^.Options or ofCentered;
 W^.Palette := wpGrayWindow;
 W^.Flags := 0;
 W^.Options := W^.Options and not (ofSelectable or ofTopSelect);

 W^.GetExtent(R);
 R.Grow(-1,-1);
 New(P,Init(R, Text));
 W^.Insert(P);

 W^.SetState(sfShadow, True);
 W^.IState := Byte(Text[1] = ' ');
 NewTimer(W^.Tmr, 4);
 Desktop^.Insert(W);
 WriteMsg := W;
end;

procedure ForceWriteShow;
begin
  if P = nil then Exit;
  NewTimer(PWriteWin(P)^.Tmr, 0);
  UpdateWriteView(P);
end;

function _WriteMsg;
var
  W: PWriteWin;
begin
  W := PWriteWin(WriteMsg(Text));
  ForceWriteShow(W);
  _WriteMsg := W;
end;

procedure InitLngStream;
var PS,XS: PStream;
    S: String;

begin
   S := GetEnv('DNDLG'); if S = '' then S := SourceDir;
   if S[Byte(S[0])] <> '\' then S := S + '\';

   PS := New(PBufStream,
             Init(S + LngId + '.LNG', stOpenPacked, 4096)
            );
   if PS^.Status <> stOK then
     begin
       Dispose(PS,Done);
       PS := New(PBufStream, Init(StartupDir + LngId + '.LNG', stOpenRead, 4096));
     end;
   if PS^.Status <> stOK then ResourceFail(LngId + '.LNG');

   XS := nil;
   {$IFNDEF NOEXTRA} {DataCompBoy: In DPMI EMS and XMS no needed}
   if XMSFound then
     begin
       XS := New(PXMSStream, Init(PS^.GetSize, PS^.GetSize));
       if XS^.Status <> stOK then
         begin
           Dispose(XS,Done);
           XS := nil;
         end;
     end;

   if (XS = nil) and EMSFound then
     begin
       XS := New(PEMSStream, Init(PS^.GetSize, PS^.GetSize));
       if XS^.Status <> stOK then
         begin
           Dispose(XS,Done);
           XS := nil;
         end;
     end;
   {$ELSE}
   If PS^.GetSize<MemAvail/100 then {If Language file <1% of free memory,}
                                    {then load it in memory              }
    begin
     XS := New(PMemoryStream, Init(PS^.GetSize, PS^.GetSize));
     if XS^.Status <> stOK then begin
      Dispose(XS,Done);
      XS := nil;
     end;
    end;
   {$ENDIF}

   if XS <> nil then
     begin
       XS^.CopyFrom(PS^, PS^.GetSize);
       if XS^.Status = stOK then
       begin
         Dispose(PS,Done);
         PS := XS;
       end else Dispose(XS,Done);

     end;

   LngStream := PS;

   PS^.Seek(0);
   LStringList := PStringList(PS^.Get);
   if (PS^.Status <> stOK) or (LStringList = nil) then ResourceFail('reading '+LngId + '.LNG');

end;


const
  CacheSize = 64;

type
  PCacheRec = ^TCacheRec;
  TCacheRec = record
    Id: TStrIdx;
    Str: string;
  end;

function NewCacheRec(const S: string; Id: TStrIdx): PCacheRec;
var
  SL: Byte absolute S;
  P: PCacheRec;
begin
  GetMem(P, SL + 1 + SizeOf(TStrIdx));
  P^.Id := Id; Move(S, P^.Str, SL+1);
  NewCacheRec := P;
end;

procedure DisposeCacheRec(P: PCacheRec);
begin
  FreeMem(P, Length(P^.Str)+1+SizeOf(TStrIdx));
end;

procedure TCacheCollection.FreeItem(Item: Pointer);
begin
  DisposeCacheRec(Item);
end;

procedure TCacheCollection.PutItem(var S: TStream; Item: Pointer);
var R: PCacheRec absolute Item;
begin
  S.Write(R^, Length(R^.Str) + 1 + SizeOf(TStrIdx));
end;

function TCacheCollection.GetItem(var S: TStream): Pointer;
var
  R: TCacheRec;
  P: PCacheRec;
  CRL: Byte;
begin
  S.Read(R, 1 + SizeOf(TStrIdx)); CRL := Byte(R.Str[0]);
  S.Read(R.Str[1], CRL);
  GetMem(P, CRL + 1 + SizeOf(TStrIdx));
  Move(R, P^, CRL + 1 + SizeOf(TStrIdx));
  GetItem := P;
end;


function CacheIndexOf(Id: TStrIdx): Integer;
var
  A: Integer;
begin
  CacheIndexOf := -1;
  StringCache^.Pack;
  for A := 0 to StringCache^.Count-1 do
   if PCacheRec(StringCache^.At(A))^.Id = Id then
  begin
    CacheIndexOf := A; Exit;
  end;
end;

procedure InitStringCache;
begin
  If StringCache<>nil then Dispose(StringCache,Done);
  StringCache := New(PCacheCollection, Init(CacheSize, 0));
end;

function GetString;
var
  S: string;
  I: Integer;
  P: PCacheRec;
begin
  if StringCache = nil then InitStringCache;

  I := CacheIndexOf(Index);
  if I = -1 then
  begin
    if LStringList = nil then InitLngStream;
    S := LStringList^.Get( Word( Index ));
    P := NewCacheRec(S, Index);
    if StringCache^.Count = CacheSize then StringCache^.AtFree(CacheSize-1);
    StringCache^.AtInsert(0, P);
  end else
  begin
    P := StringCache^.At(I);
    S := P^.Str;
    if I <> 0 then
    begin
      StringCache^.AtDelete(I); StringCache^.Pack;
      StringCache^.AtInsert(0, P);
    end;
  end;
  GetString := S;
end;

end.
