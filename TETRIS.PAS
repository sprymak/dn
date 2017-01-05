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
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{Cat
   05/10/2001 - переписал этот модуль так, чтобы при задании директивы
   $DEFINE PLUGIN он компилировался в библиотеку, представляющую собой
   плагин типа EventCatcher
   28/01/2002 - в плагинном случае добавил регистрацию объектов
}

{$IFDEF PLUGIN1}
  {JO: временно заменил PLUGIN на несуществующий дефайн PLUGIN1 }
library Tetris;

uses
  Objects, Drivers, Views, DNApp, Dialogs, Memory, Menus, Messages,
  Commands, advance, advance1, advance2, advance3, RStrings,
  xTime, Startup, DNHelp,
  VPUtils;


{$ELSE}
unit Tetris;

interface

uses
  Objects, Drivers, Views, DNApp, Dialogs, Memory, Menus, Messages,
  Commands, advance, advance1, advance2, advance3, RStrings;
{$ENDIF}

const
  Shi = 12;
  Vis = 19;
  F = False;
  t = True;

type
  PGameWindow = ^TGameWindow;
  TGameWindow = object(TDialog)
    Constructor Init;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PGameView = ^TGameView;
  TGameView = object(TView)
    Info: PView;
    {Hi  : PView;}

    Glass: array[-2..Vis+2, -1..Shi+2] of byte;

    Delay: word;
    CurFig: integer;
    Fig, OldF: array[1..5, 1..2] of byte;

    Pos: TPoint;
    LastT: longInt;
    ChPos: boolean;
    HideFig, Stop: boolean;
    Score, Lines: longInt;
    NextFig: byte;
    OldX, OldY, X, Y: shortint;

    TMaxFig: byte;

    Level, StartLevel: integer;
    Preview: boolean;
    Pentix: boolean;
    HiScores: array[1..20] of record
      Name: String[30];
      StLv, EndLv: byte;
      Score: longInt;
      end;
    Constructor Init(R: TRect);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    procedure NewGame;
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Rotate;
    procedure ReadFig;
    procedure UpDate; virtual;
    function MoveFig(DeltaX, DeltaY: integer): boolean;
    function ValidMove(DeltaX, DeltaY: integer): boolean;
    function MoveDown: boolean;
    procedure MakeTime;
    procedure ShowScores(HighLight: integer);
    procedure SetState(AState: word; Enable: boolean); virtual;
    destructor Done; virtual;
    end;

  PGameInfo = ^TGameInfo;
  TGameInfo = object(TView)
    Hc, Gm: PGameView;
    function GetPalette: PPalette; virtual;
    procedure Draw; virtual;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    end;

const
  Game: PGameView = nil;

  {$IFNDEF PLUGIN1}
implementation

uses
  xTime, Startup, DNHelp, VPUtils;
{$ENDIF}

const
  MaxFig = 28;
  Figures: array[1..5*MaxFig, 1..2] of byte =
  ((1, 3), (2, 3), (3, 3), (3, 2), (0, 0),
  (1, 2), (2, 2), (3, 2), (3, 3), (0, 0),
  (1, 2), (2, 2), (3, 2), (4, 2), (0, 0),
  (2, 2), (2, 3), (3, 2), (3, 3), (0, 0),
  (1, 2), (2, 2), (2, 3), (3, 2), (0, 0),
  (1, 2), (2, 2), (2, 3), (3, 3), (0, 0),
  (1, 3), (2, 3), (2, 2), (3, 2), (0, 0),
  (1, 1), (0, 0), (0, 0), (0, 0), (0, 0),
  (1, 1), (2, 1), (0, 0), (0, 0), (0, 0),
  (1, 2), (2, 2), (3, 2), (0, 0), (0, 0),
  (1, 1), (2, 1), (2, 2), (0, 0), (0, 0),
  (1, 3), (2, 3), (3, 3), (4, 3), (5, 3),
  (1, 2), (2, 1), (2, 2), (2, 3), (3, 2),
  (2, 2), (2, 3), (3, 2), (3, 3), (3, 4),
  (2, 2), (2, 3), (3, 1), (3, 2), (3, 3),
  (1, 1), (1, 3), (2, 1), (2, 2), (2, 3),
  (1, 1), (2, 1), (2, 2), (3, 2), (3, 3),
  (1, 2), (2, 2), (3, 2), (3, 3), (4, 3),
  (1, 3), (2, 3), (3, 2), (3, 3), (4, 2),
  (1, 1), (1, 2), (2, 2), (3, 2), (3, 3),
  (1, 2), (1, 3), (2, 2), (3, 1), (3, 2),
  (1, 1), (2, 1), (2, 2), (2, 3), (3, 2),
  (1, 3), (2, 1), (2, 2), (2, 3), (3, 2),
  (1, 1), (1, 2), (1, 3), (2, 2), (3, 2),
  (1, 2), (2, 2), (2, 3), (3, 2), (4, 2),
  (1, 3), (2, 2), (2, 3), (3, 3), (4, 3),
  (1, 1), (2, 1), (3, 1), (3, 2), (3, 3),
  (1, 3), (1, 2), (2, 3), (3, 3), (4, 3) {AK155}
  );
  ColPo: array[0..MaxFig-1] of byte =
  (4, 4, 4, 4, 4, 4, 4, 1, 2, 3, 3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5);
  {0         5         10         15         20}
  CRot: array[0..MaxFig-1] of byte =
  (4, 4, 4, 4, 3, 4, 4, 1, 2, 3, 2, 5, 3, 4, 4, 3, 3, 4, 4, 3, 3, 3, 3,
    3, 4, 4, 3, 4);

function LevelDelay(Level: byte): word;
  var
    Delay: word;
  begin
    case Level of
      1:
        Delay := 80;
      2:
        Delay := 60;
      3:
        Delay := 50;
      4:
        Delay := 30;
      5:
        Delay := 25;
      6:
        Delay := 20;
      7:
        Delay := 15;
      8:
        Delay := 10;
      9:
        Delay := 5;
      10:
        Delay := 4;
    end {case};
    LevelDelay := Delay*200
  end { LevelDelay };

Constructor TGameWindow.Init;
  var
    R: TRect;
    Gm: PGameView;
    Hi, Info: PGameInfo;
  begin
    R.Assign(1, 1, 30+Shi*2, 4+Vis);
    inherited Init(R, GetString(dlGameTitle));
    Number := GetNum;
    HelpCtx := hcTetris+byte(TetrisRec.s = 1);
    Options := Options or ofCentered;
    R.Assign(2, 2, 2+Shi*2, 2+Vis);
    Gm := New(PGameView, Init(R));
    Insert(Gm);
    R.Assign(4+Shi*2, 1, 27+Shi*2, 16);
    Info := New(PGameInfo, Init(R));
    Insert(Info);
    Info^.Gm := Gm;
    Gm^.Info := Info;
    R.Assign(4+Shi*2, 17, 15+Shi*2, 19);
    Insert(New(PButton, Init(R, GetString(dlNewButton), cmNewGame, 0)));
    R.Assign(15+Shi*2, 17, 26+Shi*2, 19);
    Insert(New(PButton, Init(R, GetString(dlSetupButton), cmSetup, 0)));
    R.Assign(4+Shi*2, 19, 15+Shi*2, 21);
    Insert(New(PButton, Init(R, GetString(dlTop10Button), cmShowHi, 0)))
      ;
    R.Assign(15+Shi*2, 19, 26+Shi*2, 21);
    Insert(New(PButton, Init(R, GetString(dlPauseButton), cmStop, 0)));
    SelectNext(False);
  end { TGameWindow.Init };

Constructor TGameInfo.Load(var s: TStream);
  begin
    inherited Load(s);
    GetPeerViewPtr(s, Gm);
  end;

procedure TGameInfo.Store(var s: TStream);
  begin
    inherited Store(s);
    PutPeerViewPtr(s, Gm);
  end;

function TGameInfo.GetPalette;
  const
    s: String[2] = #7#8;
  begin
    GetPalette := @S;
  end;

procedure TGameInfo.Draw;
  var
    B: TDrawBuffer;
    s: String;
    C, i, j, k: word;
  begin
    C := GetColor($0201);
    MoveChar(B, #196, C, Size.X);
    s := GetString(dlGameInfo);
    MoveCStr(B[(Size.X-CStrLen(s)) div 2], s, C);
    MoveChar(B, #218, C, 1);
    MoveChar(B[Size.X-1], #191, C, 1);
    WriteLine(0, 0, Size.X, 1, B);

    MoveChar(B, ' ', C, Size.X);
    MoveCStr(B, GetString(dlGameScore2)+ItoS(Gm^.Score)+'~', C);
    MoveChar(B[Size.X-1], #179, C, 1);
    WriteLine(0, 1, Size.X, 1, B);
    MoveChar(B, ' ', C, Size.X);
    MoveCStr(B, GetString(dlGameLines)+ItoS(Gm^.Lines)+'~', C);
    MoveChar(B[Size.X-1], #179, C, 1);
    WriteLine(0, 2, Size.X, 1, B);
    MoveChar(B, ' ', C, Size.X);
    MoveCStr(B, GetString(dlGameLevel)+ItoS(Gm^.Level)+'~', C);
    MoveChar(B[Size.X-1], #179, C, 1);
    WriteLine(0, 3, Size.X, 1, B);
    { MoveChar(B, #196, C, Size.X);
 MoveChar(B, #192, C, 1);
 MoveChar(B[Size.X-1], #217, C, 1);
 WriteLine(0,4,Size.X,1,B);}

    MoveChar(B, #196, C, Size.X);
    s := GetString(dlTetrisNext);
    MoveCStr(B[(Size.X-CStrLen(s)) div 2], s, C);
    MoveChar(B, #195, C, 1);
    MoveChar(B[Size.X-1], #180, C, 1);
    WriteLine(0, 4, Size.X, 1, B);
    k := ((15-Gm^.NextFig mod 7) shl 8)+219;
    for i := 5 to 10 do
      begin
        MoveChar(B, ' ', 07, Size.X);
        if not Gm^.Stop and Gm^.Preview then
          begin
            for j := 1 to ColPo[Gm^.NextFig] do
              if Figures[Gm^.NextFig*5+j, 1] = i-4 then
                begin
                  B[Figures[Gm^.NextFig*5+j, 2]*2+3] := k;
                  B[Figures[Gm^.NextFig*5+j, 2]*2+4] := k;
                end;
          end;
        MoveChar(B, #179, C, 1);
        MoveChar(B[Size.X-1], #179, C, 1);
        WriteLine(0, i, Size.X, 1, B);
      end;
    MoveChar(B, #196, C, Size.X);
    MoveChar(B, #192, C, 1);
    MoveChar(B[Size.X-1], #217, C, 1);
    WriteLine(0, 10, Size.X, 1, B);

    C := Owner^.GetColor($1112);
    MoveChar(B, #196, C, Size.X);
    s := GetString(dlTetrisBest);
    MoveCStr(B[(Size.X-CStrLen(s)) div 2], s, C);
    MoveChar(B, #218, C, 1);
    MoveChar(B[Size.X-1], #191, C, 1);
    WriteLine(0, 11, Size.X, 1, B);
    MoveChar(B, ' ', C, Size.X);
    MoveCStr(B, GetString(dlTetName)+Gm^.HiScores[1+10*byte(Gm^.
      Pentix)].Name, C);
    MoveChar(B[Size.X-1], #179, C, 1);
    WriteLine(0, 12, Size.X, 1, B);
    MoveChar(B, ' ', C, Size.X);
    MoveCStr(B, +GetString(dlGameScore)+ItoS(Gm^.HiScores[1+10*byte(Gm^.
      Pentix)].Score), C);
    MoveChar(B[Size.X-1], #179, C, 1);
    WriteLine(0, 13, Size.X, 1, B);
    MoveChar(B, #196, C, Size.X);
    MoveChar(B, #192, C, 1);
    MoveChar(B[Size.X-1], #217, C, 1);
    WriteLine(0, 14, Size.X, 1, B);
  end { TGameInfo.Draw };

Constructor TGameView.Init;
  var
    i, j: integer;
    s: PDosStream;
    C: word;
    B: array[0..1024] of byte;
  begin
    Randomize;
    inherited Init(R);
    Options := Options or ofFramed or ofSelectable or ofPreProcess;
    EventMask := $FFFF;
    Game := @Self;
    Delay := 30;
    with TetrisRec do
      begin
        StartLevel := l+1;
        Level := StartLevel;
        Pentix := (s = 1);
        Preview := (P and 1 <> 0);
      end;
    for i := 1 to 20 do
      begin
        HiScores[i].Name := GetString(dlAnonymous);
        HiScores[i].Score := 0;
        HiScores[i].StLv := 1;
        HiScores[i].EndLv := 1;
      end;
    s := New(PDosStream, Init(SourceDir+'TETRIS.CFG', stOpenRead));
    if s^.Status = stOK then
      s^.Read(B, SizeOf(HiScores));
    Dispose(s, Done);
    i := SizeOf(HiScores);
    asm
   lea ebx, B
   mov ecx, I
 @@1:
   mov dl, cl
   xor dl, $AA
   xor [ebx], dl
   inc ebx
   loop @@1
 end
      ;
    Move(B, HiScores, SizeOf(HiScores));
    for i := 1 to 20 do
      if not (HiScores[i].StLv in [1..10]) then
        begin
          for i := 1 to 20 do
            begin
              HiScores[i].Name := GetString(dlAnonymous);
              HiScores[i].Score := 0;
              HiScores[i].StLv := 1;
              HiScores[i].EndLv := 1;
            end;
          s := New(PDosStream, Init(SourceDir+'TETRIS.CFG', stCreate));
          s^.Write(HiScores, SizeOf(HiScores));
          Dispose(s, Done);
          break;
        end;
    NewGame;
    RegisterToBackground(@Self);
  end { TGameView.Init };

(*
FUNCTION CLen(s:string):byte;
var i,l: byte;
begin
  l:=0;
  for i:=1 to length(s) do if s[i]<>'~' then inc(l);
  CLen:=l
end;
*)

procedure TGameView.ShowScores;
  var
    D: PDialog;
    R: TRect;
    P: PView;
    s: String;
    i: byte;
    PP: boolean;
  begin
    D := PDialog(LoadResource(TDlgIdx(byte(dlgTetrisTop10)+byte(
      Pentix))));
    R.Assign(2, 4, D^.Size.X-2, 14);
    P := New(PView, Init(R));
    P^.Options := P^.Options or ofFramed;
    D^.Insert(P);
    for i := 1 to 10 do
      with HiScores[i+10*byte(Pentix)] do
        begin
          if Score > 0 then
            begin
              s := Name;
              if i = HighLight then
                s := '~'+s+'~';
              s := s+Strg('.', 30-CStrLen(s));
              s := s+' '+SStr(StLv, 5, '.')+' '+
              SStr(EndLv, 5, '.')+' '+
              SStr(Score, 10, '.');
            end
          else
            s := '';
          R.Assign(2, 3+i, D^.Size.X-2, 4+i);
          D^.Insert(New(PLabel, Init(R, s, nil)));
        end;
    PP := Stop;
    Stop := True;
    Desktop^.ExecView(D);
    Stop := PP;
    Dispose(D, Done);
  end { TGameView.ShowScores };

Constructor TGameView.Load(var s: TStream);
  begin
    inherited Load(s);
    GetPeerViewPtr(s, Info);
    s.Read(Glass, SizeOf(Self)-SizeOf(TView)-SizeOf(PView));
    Game := @Self;
    RegisterToBackground(@Self);
  end;

procedure TGameView.Store(var s: TStream);
  begin
    inherited Store(s);
    PutPeerViewPtr(s, Info);
    s.Write(Glass, SizeOf(Self)-SizeOf(TView)-SizeOf(PView));
  end;

destructor TGameView.Done;
  begin
    Game := nil;
    inherited Done;
  end;

procedure TGameView.NewGame;
  var
    i, j: integer;
  begin
    TMaxFig := 7;

    if Pentix then
      TMaxFig := MaxFig;

    for i := 0 to Vis+2 do
      for j := -1 to Shi+2 do
        Glass[i, j] := byte((j < 1) or (j > Shi) or (i > Vis-1) or (i <
          0));

    CurFig := Random(TMaxFig);
    ReadFig;
    X := Shi shr 1-2;
    Y := 0;
    NextFig := Random(TMaxFig);
    Score := 0;
    Lines := 0;
    Delay := LevelDelay(StartLevel);
    Level := StartLevel;

    if Info <> nil then
      Info^.DrawView;

end { TGameView.NewGame };

function TGameView.ValidMove;
  var
    i: integer;
  begin
    ValidMove := False;
    for i := 1 to ColPo[CurFig] do
      if Glass[Fig[i, 1]+Y+DeltaY, Fig[i, 2]+X+DeltaX+1] <> 0 then
          exit;
    ValidMove := True;
  end;

procedure TGameView.SetState;
  begin
    inherited SetState(AState, Enable);
    if not Enable and (AState and (sfActive+sfFocused) <> 0) and
      (Owner^.State and sfActive <> 0) and not Stop and
      ValidMove(0, 0)
    then
      Stop := True;
  end;

function TGameView.MoveFig;
  label 1;
  var
    i, j: integer;
  begin
    MoveFig := False;
    if not ValidMove(DeltaX, DeltaY) then
      exit;
    MoveFig := True;
    ChPos := True;
    HideFig := True;
    Draw;
    Inc(X, DeltaX);
    Inc(Y, DeltaY);
    ChPos := True;
    Draw;
1:
  end;

function TGameView.MoveDown;
  var
    i, j, k, l, W, Bw: integer;
    B, BB, BBW: boolean;

  procedure CheckHiScores;
    var
      i, j: integer;
      s: PDosStream;
      Nm: String;
      B: array[0..1024] of byte;
    label 1;
    begin
      for i := 1 to 10 do
        if HiScores[i+10*byte(Pentix)].Score < Score then
          goto 1;
      exit;
1:
      for j := 10 downto i+1 do
        HiScores[j+10*byte(Pentix)] := HiScores[j-1+10*byte(Pentix)];
      Nm := GetString(dlAnonymous);
      ExecResource(dlgTetrisWinner, Nm);

      HiScores[i+10*byte(Pentix)].Name := Nm;
      HiScores[i+10*byte(Pentix)].StLv := StartLevel;
      HiScores[i+10*byte(Pentix)].EndLv := Level;
      HiScores[i+10*byte(Pentix)].Score := Score;
      s := New(PDosStream, Init(SourceDir+'TETRIS.CFG', stCreate));
      Move(HiScores, B, SizeOf(HiScores));
      j := SizeOf(HiScores);
      asm
    lea ebx, B
    mov ecx, J
  @@1:
    mov dl, cl
    xor dl, $AA
    xor [ebx], dl
    inc ebx
    loop @@1
  end
        ;
      s^.Write(B, SizeOf(HiScores));
      Dispose(s, Done);
      ShowScores(i);
    end { CheckHiScores };

  begin { TGameView.MoveDown }
    if Stop then
      exit;
    if MoveFig(0, 1) then
      MoveDown := True
    else
      begin
        for i := 1 to ColPo[CurFig] do
          Glass[Fig[i, 1]+Y, Fig[i, 2]+X+1] := 15-CurFig mod 7;
        Inc(Score, ((Size.Y-Y)*2+Level*6)*(10-3*byte(Preview)) div 10);
          BB := False;
        W := 0;
        Bw := 0;
        for i := 0 to Vis-1 do
          begin
            B := True;
            BBW := True;
            for j := 1 to Shi do
              begin
                B := B and (Glass[i, j] <> 0);
                BBW := BBW and (Glass[i, j] = 0);
              end;
            Inc(Bw, byte(BBW));
            if B then
              begin
                for k := i downto 1 do
                  for j := 1 to Shi do
                    begin
                      Glass[k, j] := Glass[k-1, j];
                      Glass[k-1, j] := 0;
                    end;
                Inc(Score, (W+Size.Y-i)*30);
                Inc(Lines);
                Inc(W);
                BB := True;
              end;
          end;
        if Bw = Vis then
          Inc(Score, (Level*300*(10-3*byte(Preview))) div 10);
        CurFig := NextFig;
        ReadFig;
        NextFig := Random(TMaxFig);
        X := Shi shr 1-2;
        Y := -Fig[1, 1];
        if Lines > 90-(9-StartLevel)*10 then
          begin
            Level := 1+(Lines-(90-(9-StartLevel)*10)) div 20+
              StartLevel;
            if Level > 10 then
              Level := 10;
            if Level < StartLevel then
              Level := StartLevel;
            Delay := LevelDelay(Level);
          end;
        Info^.DrawView;
        Stop := not ValidMove(0, 0);
        if BB then
          xTime.DelayTics(2)
        else
          xTime.DelayTics(1);
        if Stop then
          begin
            for i := 1 to ColPo[CurFig] do
              Glass[Fig[i, 1]+Y, Fig[i, 2]+X+1] := 15-CurFig mod 7;
            Draw;
            MessageBox(GetString(dlGameOver), @Score, mfInformation+
              mfOKButton);
            CheckHiScores;
            Info^.Draw;
          end;
        DrawView;
        MoveDown := False;
        MakeTime;
      end

end { TGameView.MoveDown };

procedure TGameView.Draw;
  var
    B: TDrawBuffer;
    i, j, k: integer;
  begin
    if not ChPos then
      for i := 0 to Vis do
        begin
          MoveChar(B, ' ', $0, Shi*2);
          for j := 1 to Shi do
            begin
              k := (j-1)*2;
              B[k] := (Glass[i, j] shl 8)+219;
              B[k+1] := B[k];
            end;
          WriteLine(0, i, Shi*2, 1, B);
        end;

    if not Stop then
      begin
        k := (((15-CurFig mod 7) shl 8)+219)*byte(not HideFig);
        B[0] := k;
        B[1] := k;
        for i := 1 to ColPo[CurFig] do
          WriteBuf((X+Fig[i, 2])*2, Y+Fig[i, 1], 2, 1, B);

      end;
    ChPos := False;
    HideFig := False;
  end { TGameView.Draw };

procedure TGameView.MakeTime;
  var
    A, B, C: byte;
  begin
    if Stop then
      exit;
    {$IFDEF OS_DOS}
    asm
  mov ah, 2Ch
  int 21h
  mov A, DH
  mov B, DL
  mov C, CL
 end;
    LastT := longInt(A+longInt(C)*60)*100+B;
    {$ELSE}
    LastT := GetTimemSec*10;
    {$ENDIF}
  end;

procedure TGameView.UpDate;
  var
    A, B, C: byte;
  begin
    if Stop then
      exit;
    {$IFDEF OS_DOS}
    asm
  mov ah, 2Ch
  int 21h
  mov A, DH
  mov B, DL
  mov C, CL
 end;
    if longInt(longInt(C)*60+word(A))*100+B > LastT+Delay then
      {$ELSE}
      if GetTimemSec*10 > LastT+Delay then
        {$ENDIF}
        begin
          MoveDown;
          MakeTime
        end;
  end;

procedure TGameView.HandleEvent;
  procedure CE;
    begin
      ClearEvent(Event)
    end;

  procedure Setup;
    begin
      Stop := True;
      with TetrisRec do
        begin
          l := Level-1;
          s := byte(Pentix);
          P := byte(Preview);
        end;
      if ExecResource(dlgGameSetup, TetrisRec) <> cmOK then
        exit;
      with TetrisRec do
        begin
          StartLevel := l+1;
          Pentix := (s = 1);
          Preview := (P and 1 <> 0);
        end;
      HelpCtx := hcTetris+byte(Pentix);
      NewGame;
      Owner^.Redraw;
      Message(Application, evCommand, cmUpdateConfig, nil);
    end { Setup };

  begin { TGameView.HandleEvent }
    inherited HandleEvent(Event);
    case Event.What of
      evCommand:
        case Event.Command of
          cmGetName:
            PString(Event.InfoPtr)^:= GetString(dlGameTitle);
          cmStop:
            begin
              if ValidMove(0, 0) then
                Stop := not Stop;
              CE
            end;
          cmNewGame:
            begin
              NewGame;
              Stop := False;
              DrawView;
              CE
            end;
          cmSetup:
            begin
              Setup;
              CE
            end;
          cmShowHi:
            begin
              ShowScores(0);
              CE
            end;
          cmTetrisIncLevel:
            begin
              if Level < 10 then
                Inc(Level);
              Delay := LevelDelay(Level);
              Owner^.Redraw;
              CE
            end;
          cmTetrisPreview:
            begin
              Preview := not Preview;
              Owner^.Redraw;
              CE
            end;
        end {case};
      evKeyDown:
        case Event.KeyCode of
          kbESC:
            begin
              Message(Owner, evCommand, cmClose, nil);
              CE
            end;
          else
            if not Stop then
              case Event.KeyCode and $FF00 of
                kbLeft, kbHome:
                  begin
                    MoveFig(-1, 0);
                    CE
                  end;
                kbRight, kbPgUp:
                  begin
                    MoveFig(1, 0);
                    CE
                  end;
                kbDown, $3900
                :
                  begin
                    if not Stop then
                      while ValidMove(0, 1) do
                        MoveDown;
                    MakeTime;
                    CE;
                  end;
                kbUp:
                  begin
                    OldF := Fig;
                    Rotate;
                    if ValidMove(0, 0) then
                      begin
                        Fig := OldF;
                        ChPos := True;
                        HideFig := True;
                        Draw;
                        Rotate;
                        MoveFig(0, 0)
                      end
                    else
                      Fig := OldF;
                    CE;
                  end;
                else
                  if (Event.CharCode > #0) and (CommandLine <> nil)
                  then
                    CommandLine^.HandleEvent(Event);
              end
            else if (Event.CharCode > #0) and (CommandLine <> nil)
            then
              CommandLine^.HandleEvent(Event);
        end {case};
    end {case};
  end { TGameView.HandleEvent };

procedure TGameView.Rotate;
  var
    i: integer;
  begin
    for i := 1 to ColPo[CurFig] do
      begin
        Fig[i, 2] := OldF[i, 1];
        Fig[i, 1] := CRot[CurFig]+1-OldF[i, 2];
      end;
  end;

procedure TGameView.ReadFig;
  var
    i: integer;
  begin
    for i := 1 to ColPo[CurFig] do
      begin
        Fig[i, 1] := Figures[CurFig*5+i, 1];
        Fig[i, 2] := Figures[CurFig*5+i, 2];
      end;
  end;

procedure TGameWindow.HandleEvent;
  begin
    if (Event.What = evKeyDown) and (Event.CharCode = ' ') then
      Event.KeyCode := kbDown;
    inherited HandleEvent(Event);
  end;

{$IFDEF PLUGIN1}
const
  Reg1: TStreamRec =
  (ObjType: $FFFF;
  VmtLink: System.TypeOf(TGameWindow);
  Load: @TGameWindow.Load;
  Store: @TGameWindow.Store);
  Reg2: TStreamRec =
  (ObjType: $FFFF;
  VmtLink: System.TypeOf(TGameView);
  Load: @TGameView.Load;
  Store: @TGameView.Store);
  Reg3: TStreamRec =
  (ObjType: $FFFF;
  VmtLink: System.TypeOf(TGameInfo);
  Load: @TGameInfo.Load;
  Store: @TGameInfo.Store);
  RegisterNeed: boolean = True;

  {&Cdecl+}
  {procedure CatchCommand(Command, LngIndex, DlgIndex: SmallWord);}
    {1.0}
procedure CatchCommand(Command, ObjType: SmallWord; const PluginName:
    ShortString);
  begin
    if RegisterNeed then
      begin
        Reg1.ObjType := ObjType;
        Reg2.ObjType := ObjType+1;
        Reg3.ObjType := ObjType+2;
        RegisterType(Reg1);
        RegisterType(Reg2);
        RegisterType(Reg3);
        RegisterNeed := False;
      end;

    if Command = 0 then
      if Game = nil then
        Application^.InsertWindow(New(PGameWindow, Init))
      else
        Game^.Owner^.Select;
  end;
{&Cdecl-}

exports
CatchCommand Name'CatchCommand';
{$ENDIF}

begin
end.
