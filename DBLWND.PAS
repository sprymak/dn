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

unit DblWnd;

interface

uses
  Lfn, Views, Defines, Streams, Drivers, FlPanel, Tree, FViewer
  ;

type
  {
    PCloseBox = ^TCloseBox;
    TCloseBox = object(TView)
      constructor Init(var R: TRect; ACommand: Word);
      procedure Draw; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
    end;
}

  PDoubleWindow = ^TDoubleWindow;
  TDoubleWindow = object( {TStd}TWindow)
    isValid: Boolean;
    LeftView, RightView: PView;
    RDrive, LDrive: Byte;
    Separator: PSeparator;
    LPanel, RPanel: PFilePanel;
    Info: PView;
    {$IFDEF NETINFO}
    NetInfo: PHideView; {-$VIV}
    {$ENDIF}
    LTree: PHTreeView;
    QView: PFileViewer;
    NonFilePanelType: Byte; {тип нефайловой панели; 0 если таковой нет}
    pNonFilePanel: ^PView; {адрес указателя на нее, например, @QView}
    OldBounds: TRect;
    OldPanelBounds: TRect;
    PanelZoomed: Boolean;
    LType, RType: AInt;
    constructor Init(Bounds: TRect; ANumber, ADrive: integer);
    procedure InitLeftView(R: TRect);
    procedure InitRightView(R: TRect);
    procedure InitInterior;
    constructor Load(var S: TStream);
    procedure Store(var S: TStream);
    function Valid(C: word): Boolean; virtual;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    procedure HandleCommand(var Event: TEvent);
    end;

const
  CDoubleWindow = #80#81#82+ { 1-3    Frame (P,A,I)            }
  #83#84+ { 4,5    Scroll Bar (Page, Arrow) }
  #85#86#87#88#89+ { 6-10   Panel (NT,Sp,ST,NC,SC)   }
  #90#91+ { 11,12  Panel Top (A,P)          }
  #92#93+ { 13,14  Viewer (NT,ST)           }
  #94#95#96#97+
  #98#99#100+ { 15-21  Tree (T,NI,SI,Df,DS,SI,DI}
  #101+ { 22     Tree info                }
  #102#103+ { 23,24  Disk info (NT, HT)       }
  #119#120#121#122+
  #123#124#125+ { 25-31  File Info                }
  #165+ { 32-38  File Panel               }
  #172#173#174#175#176#177#180#181+
  #186#187#188+#192#193#194#195#196 { Drive Line };
  {JO - 5 additional highlight groups}

const
  dtInfo = 1;
  dtPanel = 2;
  dtTree = 3;
  dtView = 4;
  {$IFDEF NETINFO}
  dtNetInfo = 5; {-$VIV}
  {$ENDIF}

implementation
uses
  DiskInfo, Commands, FileCopy, FilesCol, advance, advance1, advance2,
  advance3, Startup, DNApp, RStrings, {DataCompBoy}Messages
  {$IFDEF NETINFO}, NetInfo {-$VIV} {$ENDIF}
  , Dos
  ;

constructor TDoubleWindow.Init;
  var
    P: PFilePanel;
    P2: PTopView;
    R: TRect;
    PV: PView;
  begin
  inherited Init(Bounds, '', ANumber);
  isValid := True;
  Options := Options or ofTileable;
  EventMask := $FFFF;
  Abort := False;
  if  (ADrive <= 0) or not ValidDrive(Char(ADrive+64)) then
    ADrive := 0;
  LDrive := ADrive;
  RDrive := ADrive;
  GetExtent(R);
  R.Grow(-1, 0);
  R.A.X := R.B.X div 2;
  R.B.X := R.A.X+2;
  PanelZoomed := False;
  Separator := New(PSeparator, Init(R, Size.X));
  Insert(Separator);
  LeftView := nil;
  RightView := nil;
  InitInterior;
  if Abort then
    isValid := False;
  case PanelDefaults.LeftPanel of
    fdoInfoDrive:
      Message(@Self, evCommand, cmDiskInfo, nil);
    fdoTreeFrive:
      Message(@Self, evCommand, cmDirTree, nil);
    fdoRightOnly:
      Message(@Self, evCommand, cmHideLeft, nil);
  end {case};
  end { TDoubleWindow.Init };

procedure TDoubleWindow.InitLeftView;
  var
    PV: PSpecScroll;
    P: PFilePanel;
    P1: PInfoView;
    P2: PTopView;
    PD: PDriveLine;
    B: TRect;
  begin
  if Abort then
    exit;
  B := R;
  B.A.X := B.B.X;
  Inc(B.B.X);
  Inc(B.A.Y);
  Dec(B.B.Y, 3);
  New(PV, Init(B));
  New(P, Init(R, LDrive, PV));
  PV^.Panel := P;
  if Abort then
    begin
    Dispose(P, Done);
    Dispose(PV, Done);
    exit
    end;

  New(P1, Init(R));
  P1^.Panel := P;
  P^.InfoView := P1;

  New(P2, Init(R));
  P2^.Panel := P;
  P^.DirView := P2;
  P2^.EventMask := $FFFF;

  New(PD, Init(R, P));
  P^.DriveLine := PD;

  P^.ChangeBounds(R);

  Insert(PV);
  Insert(P2);
  Insert(P1);
  Insert(P);
  Insert(PD);
  LeftView := P;
  end { TDoubleWindow.InitLeftView };

procedure TDoubleWindow.InitRightView;
  var
    P: PFilePanel;
    P1: PInfoView;
    P2: PTopView;
    PS: PSpecScroll;
    PD: PDriveLine;
    RR: TRect;
  begin
  if Abort then
    exit;
  RR.B.X := RR.A.X+1;
  New(PS, Init(RR));

  New(P, Init(R, RDrive, PS));
  PS^.Panel := P;
  if Abort then
    begin
    Dispose(P, Done);
    Dispose(PS, Done);
    exit
    end;

  New(P1, Init(R));
  P1^.Panel := P;
  P^.InfoView := P1;

  New(P2, Init(R));
  P2^.Panel := P;
  P^.DirView := P2;
  P2^.EventMask := $FFFF;

  New(PD, Init(R, P));
  P^.DriveLine := PD;

  P^.ChangeBounds(R);
  Insert(PS);
  Insert(P2);
  Insert(P1);
  Insert(PD);
  Insert(P);
  RightView := P;
  end { TDoubleWindow.InitRightView };

function TDoubleWindow.Valid;
  begin
  Valid := inherited Valid(C) and isValid;
  end;

procedure TDoubleWindow.ChangeBounds(var Bounds: TRect);
  var
    D: TPoint;
    R: TRect;
    P: PSeparator;
    LVisible, RVisible, SVisible: Boolean;
  label 1;
  begin
  D.X := Bounds.B.X-Bounds.A.X-Size.X;
  D.Y := Bounds.B.Y-Bounds.A.Y-Size.Y;
  P := PSeparator(Separator);
  GetExtent(R);
  R.Grow(-1, 0);
  R.A.X := (R.B.X*P^.OldX) div P^.OldW;
  if D.EqualsXY(0, 0) and (R.A.X = P^.Origin.X) then
    begin
    SetBounds(Bounds);
    DrawView;
    end
  else
    begin
    FreeBuffer;
    SetBounds(Bounds);
    GetExtent(Clip);
    GetBuffer;
    Lock;
    GetExtent(R);
    Frame^.ChangeBounds(R);
    P := PSeparator(Separator);
    LVisible := LeftView^.GetState(sfVisible);
    RVisible := RightView^.GetState(sfVisible);
    SVisible := P^.GetState(sfVisible);
    if not LVisible or not RVisible then
      begin
      if SVisible then
        P^.Hide;
      GetExtent(R);
      R.Grow(-1, -1);
      if LVisible then
        LeftView^.ChangeBounds(R)
      else
        RightView^.ChangeBounds(R);
      goto 1;
      end;
    if not SVisible then
      P^.Show;
    GetExtent(R);
    R.Grow(-1, 0);
    R.A.X := (R.B.X*P^.OldX) div P^.OldW;
    R.B.X := R.A.X+2;
    Separator^.ChangeBounds(R);
    GetExtent(R);
    R.Grow(-1, -1);
    R.A.X := (R.B.X*P^.OldX) div P^.OldW+2;
    RightView^.ChangeBounds(R);
    GetExtent(R);
    R.Grow(-1, -1);
    R.B.X := (R.B.X*P^.OldX) div P^.OldW;
    LeftView^.ChangeBounds(R);
1:
    Redraw;
    UnLock;
    end;
  end { TDoubleWindow.ChangeBounds };

constructor TDoubleWindow.Load;
  var
    B: Boolean;
  begin
  inherited Load(S);
  S.Read(OldBounds, SizeOf(OldBounds)*2+SizeOf(LType)+SizeOf(RType));
  GetSubViewPtr(S, Separator);
  GetSubViewPtr(S, LeftView);
  GetSubViewPtr(S, RightView);
  GetSubViewPtr(S, LPanel);
  GetSubViewPtr(S, RPanel);
  GetSubViewPtr(S, Info);
  {$IFDEF NETINFO}
  GetSubViewPtr(S, NetInfo); {-$VIV}
  {$ENDIF}
  {Cat}
  if  (Separator = nil) or (LeftView = nil) or (RightView = nil)
    or (LPanel = nil) or (RPanel = nil)
  then
    Fail;
  {/Cat}
  if TypeOf(LeftView^) = TypeOf(TFilePanel) then
    PSpecScroll(PFilePanel(LeftView)^.ScrollBar)^.Panel :=
      PFilePanel(LeftView);
  if TypeOf(RightView^) = TypeOf(TFilePanel) then
    PSpecScroll(PFilePanel(RightView)^.ScrollBar)^.Panel :=
      PFilePanel(RightView);
  S.Read(B, 1);
  LTree := nil;
  if B then
    begin
    GetSubViewPtr(S, LTree);
    if LTree = nil then
      {Cat}
      Fail;
    end;
  S.Read(B, 1);
  QView := nil;
  if B then
    begin
    GetSubViewPtr(S, QView);
    if QView = nil then
      {Cat}
      Fail;
    end;
  if  (LTree <> nil) and (LTree^.Size.X > 0) then
    LTree^.ReadAfterLoad;
  if  (RType = dtInfo) or (LType = dtInfo) then
    begin
    if Info = nil then
      {Cat}
      Fail;
    PDiskInfo(Info)^.ReadData(False);
    end;
  {$IFDEF NETINFO}
  if  (RType = dtNetInfo) or (LType = dtNetInfo) then
    begin
    if NetInfo = nil then
      {Cat}
      Fail;
    PNetInfo(NetInfo)^.ReadData(False); {-$VIV}
    end;
  {$ENDIF}
  if  (RType = dtView) then
    LPanel^.QuickViewEnabled := True
  else if (LType = dtView) then
    RPanel^.QuickViewEnabled := True;
  isValid := True;
  end { TDoubleWindow.Load };

procedure TDoubleWindow.Store;
  var
    B: Boolean;
  begin
  inherited Store(S);
  S.Write(OldBounds, SizeOf(OldBounds)*2+SizeOf(LType)+SizeOf(RType));
  PutSubViewPtr(S, Separator);
  PutSubViewPtr(S, LeftView);
  PutSubViewPtr(S, RightView);
  PutSubViewPtr(S, LPanel);
  PutSubViewPtr(S, RPanel);
  PutSubViewPtr(S, Info);
  {$IFDEF NETINFO}
  PutSubViewPtr(S, NetInfo); {-$VIV}
  {$ENDIF}
  if LTree <> nil then
    B := True
  else
    B := False;
  S.Write(B, 1);
  if B then
    PutSubViewPtr(S, LTree);
  if QView <> nil then
    B := True
  else
    B := False;
  S.Write(B, 1);
  if B then
    PutSubViewPtr(S, QView);
  end { TDoubleWindow.Store };

procedure TDoubleWindow.InitInterior;
  var
    R: TRect;
  begin
  GetExtent(R);
  R.Grow(-1, -1);
  R.A.X := R.B.X div 2+2;
  InitRightView(R);
  if Abort then
    exit;

  GetExtent(R);
  R.Grow(-1, -1);
  R.B.X := R.B.X div 2;
  InitLeftView(R);

  if Abort then
    exit;
  RightView^.Select;
  RPanel := PFilePanel(RightView);
  LType := dtPanel;
  RType := dtPanel;
  LType := dtPanel;
  LPanel := PFilePanel(LeftView);
  GetExtent(R);
  R.Grow(-1, -1);
  R.B.X := Separator^.Origin.X;
  Info := nil;
  LTree := nil;
  QView := nil;
  {$IFDEF NETINFO}
  NetInfo := nil; {-$VIV}
  {$ENDIF}
  end { TDoubleWindow.InitInterior };

procedure TDoubleWindow.HandleCommand;
  var
    R, R1, R2: TRect;
    I, K: integer;
    LVisible, RVisible, LSelected: Boolean;
    Sp: PSeparator;
    EV: TEvent;
    P: PView;
    S: String;

  procedure SwitchLeft;
    begin
    if not RVisible or PanelZoomed then
      exit;
    if LVisible then
      begin
      GetBounds(R);
      OldBounds := R;
      { Flash >>> }
      R.A.X := RightView^.Origin.X-R.A.X-1;
      if R.B.X-R.A.X <= 16 then
        Dec(R.A.X, 16-(R.B.X-R.A.X));
      { Flash <<< }
      Lock;
      LeftView^.Hide;
      RightView^.Select;
      Locate(R);
      Redraw;
      UnLock;
      end
    else
      begin
      GetBounds(R);
      R.A.X := OldBounds.A.X;
      R.B.X := OldBounds.B.X; { Flash }
      Lock;
      LeftView^.Show;
      Redraw;
      Locate(R);
      UnLock;
      Owner^.Redraw;
      end
    end { SwitchLeft };

  procedure SwitchRight;
    begin
    if not LVisible or PanelZoomed then
      exit;
    if RVisible then
      begin
      GetBounds(R);
      OldBounds := R;
      R.B.X := Separator^.Origin.X+R.A.X+1;
      Lock;
      RightView^.Hide;
      LeftView^.Select;
      Locate(R);
      Redraw;
      UnLock;
      end
    else
      begin
      GetBounds(R);
      R.A.X := OldBounds.A.X;
      R.B.X := OldBounds.B.X; { Flash }
      Lock;
      RightView^.Show;
      Redraw;
      Locate(R);
      UnLock;
      Owner^.Redraw;
      end
    end { SwitchRight };

  procedure GetDirName;
    begin
    if LVisible and (LType = dtPanel) then
      PString(Event.InfoPtr)^:= PFilePanel(LPanel)^.DirectoryName
    else if RVisible and (RType = dtPanel) then
      PString(Event.InfoPtr)^:= PFilePanel(RPanel)^.DirectoryName
    end;

  procedure ChangeDrvLeft;
    var
      S: String;
      P: TPoint;
    begin
    if LType <> dtPanel then
      begin
      LeftView^.GetBounds(R);
      R.A.Y := 1;
      R.B.Y := Size.Y;
      LPanel^.Locate(R);
      end;
    GetBounds(R);
    if not LVisible or (LType <> dtPanel) then
      begin
      if LVisible then
        P.X := Origin.X+(1+LeftView^.Size.X) div 2
      else if Separator^.OldW > 0 then
        P.X :=
              (OldBounds.A.X+((OldBounds.B.X-OldBounds.A.X)*Separator^.OldX
              ) div Separator^.OldW) div 2
      else
        P.X := 1;
      P.Y := Origin.Y+3;
      S := SelectDrive(P.X, P.Y, LPanel^.DirectoryName[1], True);
      if S = '' then
        exit;
      ClrIO;
      Message(LPanel, evCommand, cmChangeDrv, @S);
      if LType <> dtPanel then
        begin
        Lock;
        if not LVisible then
          SwitchLeft;
        LeftView^.GetBounds(R1);
        pNonFilePanel^.Free;
        pNonFilePanel^:= nil;
        NonFilePanelType := 0;
        R1.A.Y := 1;
        R1.B.Y := Size.Y-1;
        LPanel^.Locate(R1);
        LeftView := LPanel;
        LType := dtPanel;
        LPanel^.Show;
        UnLock;
        end
      else if not LVisible then
        SwitchLeft;
      LeftView^.GetBounds(R1);
      end
    else
      begin
      Message(LPanel, evCommand, cmChangeDrive, nil);
      end;
    if  (ShiftState and 3 <> 0) and (not LSelected) then
      LeftView^.Select;
    end { ChangeDrvLeft };

  procedure ChangeDrvRight;
    var
      S: String;
      P: TPoint;
    begin
    if RType <> dtPanel then
      begin
      RightView^.GetBounds(R);
      R.A.Y := 1;
      R.B.Y := Size.Y;
      RPanel^.Locate(R);
      end;
    GetBounds(R);
    if not RVisible or (RType <> dtPanel) then
      begin
      if RVisible then
        P.X := Origin.X+RightView^.Origin.X+(1+RightView^.Size.X) div 2
      else if Separator^.OldW > 0 then
        P.X := (OldBounds.B.X+OldBounds.A.X+((OldBounds.B.X-OldBounds.A.X)*
              Separator^.OldX) div Separator^.OldW) div 2
      else
        P.X := OldBounds.B.X+OldBounds.A.X;
      P.Y := Origin.Y+3;
      S := SelectDrive(P.X, P.Y, RPanel^.DirectoryName[1], True);
      if S = '' then
        exit;
      ClrIO;
      Message(RPanel, evCommand, cmChangeDrv, @S);
      if RType <> dtPanel then
        begin
        Lock;
        if not RVisible then
          SwitchRight;
        RightView^.GetBounds(R1);
        pNonFilePanel^.Free;
        pNonFilePanel^:= nil;
        NonFilePanelType := 0;
        R1.A.Y := 1;
        R1.B.Y := Size.Y-1;
        RPanel^.Locate(R1);
        RightView := RPanel;
        RType := dtPanel;
        RPanel^.Show;
        UnLock;
        end
      else if not RVisible then
        SwitchRight;
      RightView^.GetBounds(R1);
      end
    else
      Message(RPanel, evCommand, cmChangeDrive, nil);
    if  (ShiftState and 3 <> 0) and LSelected then
      RightView^.Select;
    end { ChangeDrvRight };

  procedure ViewInsert;
    var
      R2: TRect;
      S: PViewScroll;
      P: PView;
    begin
    R2.Assign(1, 1, 2, 5);
    S := New(PViewScroll, Init(R2));
    S^.Options := S^.Options or ofPostProcess;
    QView := New(PHFileViewer, Init(R1, nil, '', '', S, True,
             (EditorDefaults.ViOpt and 1) <> 0));
    if not QView^.Valid(0) then
      begin
      Dispose(S, Done);
      Dispose(QView, Done);
      QView := nil;
      exit
      end
    else
      begin
      Lock;
      Insert(QView);
      Insert(S);
      QView^.Hide;
      UnLock;
      end;
    end { ViewInsert };

  procedure InsertTree;
    var
      R2: TRect;
      S: PMyScrollBar;
      P: PView;
    begin
    R2 := R1;
    R2.A.X := R2.B.X;
    Inc(R2.B.X);
    S := New(PMyScrollBar, Init(R2));
    S^.Options := S^.Options or ofPostProcess;
    { Область делим по вертикали между подвалом (2 строки) и деревом}
    Dec(R1.B.Y, 2);
    LTree := New(PHTreeView, Init(R1, 0, False, S));
    if not LTree^.Valid(0) then
      begin
      Dispose(S, Done);
      Dispose(LTree, Done);
      LTree := nil;
      exit
      end
    else
      begin
      R1.A.Y := R1.B.Y;
      Inc(R1.B.Y, 2);
      P := New(PTreeInfoView, Init(R1, LTree));
      LTree^.Info := P;
      Insert(P);
      Insert(LTree);
      Insert(S);
      end;
    end { InsertTree };

  procedure SwitchView(dtType: Byte);
    var
      V: PView;
      pViewToFree: ^PView;
    label Ex;
    begin
    Lock;
    GetBounds(R);
    if LSelected then
      RightView^.GetBounds(R1)
    else
      LeftView^.GetBounds(R1);

    { Если нефайловая панель была - ее надо в любом случае уничтожить.
Если она была, и притом именно типа dtType, то надо восстановить
файловую панель; в осальных случаях создать панель dtType}
    pViewToFree := nil;
    if NonFilePanelType <> 0 then
      pViewToFree := Pointer(pNonFilePanel);

    case dtType of
      dtTree:
        begin
        if LTree = nil then
          begin
          {if StartupData.Slice2 and osuReleaseTree = 0 then Application^.BFSpeed;}
          InsertTree;
          {if StartupData.Slice2 and osuReleaseTree = 0 then Application^.EFSpeed;}
          end;
        if LTree = nil then
          goto Ex;
        V := LTree;
        pNonFilePanel := @LTree;
        end;
      dtView:
        begin
        if QView = nil then
          ViewInsert;
        if QView = nil then
          goto Ex;
        V := QView;
        pNonFilePanel := @QView;
        end;
      dtInfo:
        begin
        if Info = nil then
          begin
          Info := New(PDiskInfo, Init(R));
          Info^.Hide;
          Insert(Info);
          PDiskInfo(Info)^.ReadData(False);
          // Это надо делать после Insert
          end;
        V := Info;
        pNonFilePanel := @Info;
        end;
      {$IFDEF NETINFO}
      dtNetInfo:
        begin
        if NetInfo = nil then
          {-$VIV start}
          begin
          NetInfo := New(PNetInfo, Init(R));
          NetInfo^.HideView;
          Insert(NetInfo);
          end;
        V := NetInfo;
        pNonFilePanel := @NetInfo;
        end; {-$VIV end}
      {$ENDIF}
    end {case};
    if NonFilePanelType <> dtType then
      NonFilePanelType := dtType
    else
      NonFilePanelType := 0;

    V^.GetBounds(R1);
    if  (LType = dtType) and LVisible then
      begin
      LeftView := LPanel;
      Lock;
      R1.A.Y := 1;
      R1.B.Y := Size.Y-1;
      LPanel^.Locate(R1);
      LPanel^.Show;
      LType := dtPanel;
      RPanel.QuickViewEnabled := False;
      if LSelected then
        LPanel^.Select;
      UnLock
      end
    else if (RType = dtType) and RVisible then
      begin
      RightView := RPanel;
      Lock;
      R1.A.Y := 1;
      R1.B.Y := Size.Y-1;
      RPanel^.Locate(R1);
      RPanel^.Show;
      RType := dtPanel;
      LPanel.QuickViewEnabled := False;
      if not LSelected then
        RPanel^.Select;
      UnLock
      end
    else if (RType = dtType) and not RVisible then
      SwitchRight
    else if (LType = dtType) and not LVisible then
      SwitchLeft
    else if LSelected then
      begin
      RightView^.GetBounds(R1);
      RightView^.Hide;
      RightView := V;
      R1.A.Y := 1;
      R1.B.Y := Size.Y-1;
      V^.Locate(R1);
      V^.Show;
      RType := dtType;
      if not RVisible then
        SwitchRight;
      {begin GetBounds(R); R.A.X := OldBounds.A.X; R.B.X := OldBounds.B.X;end;}
      LeftView^.Select;
      if dtType = dtView then
        LPanel.QuickViewEnabled := True;
      Redraw;
      end
    else
      begin
      LeftView^.GetBounds(R1);
      LeftView^.Hide;
      LeftView := V;
      R1.A.Y := 1;
      R1.B.Y := Size.Y-1;
      V^.Locate(R1);
      V^.Show;
      LType := dtType;
      if not LVisible then
        SwitchLeft;
      { begin GetBounds(R); R.A.X := OldBounds.A.X; R.B.X := OldBounds.B.X end;}
      RightView^.Select;
      if dtType = dtView then
        RPanel.QuickViewEnabled := True;
      Redraw;
      end;
    if pViewToFree <> nil then
      begin
      pViewToFree^.Free;
      pViewToFree^:= nil;
      end;
Ex:
    UnLock;
    end { SwitchView };

  procedure CE;
    begin
    ClearEvent(Event)
    end;

  function isPanel: Boolean;
    begin
    isPanel := (Current = LPanel) or (Current = RPanel);
    end;

  var
    AP, PP: PFilePanel;
    D: word;
  begin { TDoubleWindow.HandleCommand }
  if RightView <> nil
  then
    RVisible := RightView^.GetState(sfVisible)
  else
    RVisible := False;
  if LeftView <> nil then
    begin
    LVisible := LeftView^.GetState(sfVisible);
    LSelected := LeftView^.GetState(sfSelected);
    end
  else
    begin
    LVisible := False;
    LSelected := False;
    end;
  Sp := Separator;
  case Event.What of
    evKeyDown:
      case Event.KeyCode of
        kbCtrlLeft, kbCtrlRight,
        kbCtrlShiftLeft, kbCtrlShiftRight:
          if  (FMSetup.Options and fmoCtrlDifference <> 0) then
            if  (LVisible and RVisible) then
              if FMSetup.Options and fmoAlternCtrlDiff <> 0 then
                begin
                if LSelected then
                  if  (ShiftState2 and 1 <> 0)
                  then
                    RightView^.HandleEvent(Event)
                  else
                    LeftView^.HandleEvent(Event)
                else if (ShiftState2 and 1 <> 0)
                then
                  LeftView^.HandleEvent(Event)
                else
                  RightView^.HandleEvent(Event);
                exit;
                end
              else
                begin
                if LVisible and (ShiftState2 and 1 <> 0)
                then
                  LeftView^.HandleEvent(Event)
                else
                  RightView^.HandleEvent(Event);
                exit;
                end;

        kbAlt1, kbAlt2, kbAlt3, kbAlt4, kbAlt5, kbAlt0,
        kbAlt6, kbAlt7, kbAlt8, kbAlt9:
          if FMSetup.Options and fmoAltDifference <> 0 then
            begin
            if LVisible and (ShiftState2 and 2 <> 0) then
              LeftView^.HandleEvent(Event)
            else
              RightView^.HandleEvent(Event);
            exit;
            end;
        {
              kbCtrl1, kbCtrl2, kbCtrl3, kbCtrl4, kbCtrl5, kbCtrl6, kbCtrl7,
              kbCtrl8, kbCtrl9, kbCtrl0:
                     if FMSetup.Options and fmoCtrlDifference <> 0 then
                           begin
                             if LVisible and (ShiftState2 and 1 <> 0) then
                                LeftView^.HandleEvent(Event) else
                                RightView^.HandleEvent(Event);
                             Exit;
                           end;
}
        kbAltLeft,
        kbAltShiftLeft:
          if RVisible and LVisible then
            begin
            D := 1;
            if ShiftState and (kbLeftShift+kbRightShift) <> 0 then
              begin
              if LSelected then
                if LType = dtPanel
                then
                  AP := LPanel
                else
                  AP := RPanel
              else if RType = dtPanel
              then
                AP := RPanel
              else
                AP := LPanel
                  ;
              D := AP^.LineLength; { Flash }
              end;
            if Sp^.OldX > D then
              Dec(Sp^.OldX, D); { Flash }
            Sp^.OldW := Size.X;
            R.A := Origin;
            R.B.X := Origin.X+Size.X;
            R.B.Y := Origin.Y+Size.Y;
            ChangeBounds(R);
            CE;
            end;

        kbAltRight,
        kbAltShiftRight:
          if RVisible and LVisible then
            begin
            D := 1;
            if ShiftState and (kbLeftShift+kbRightShift) <> 0 then
              begin
              if LSelected then
                if LType = dtPanel
                then
                  AP := LPanel
                else
                  AP := RPanel
              else if RType = dtPanel
              then
                AP := RPanel
              else
                AP := LPanel
                  ;
              D := AP^.LineLength; { Flash }
              end;
            Sp^.OldW := Size.X;
            if Sp^.OldX < Sp^.OldW-D then
              Inc(Sp^.OldX, D);
            R.A := Origin;
            R.B.X := Origin.X+Size.X;
            R.B.Y := Origin.Y+Size.Y;
            ChangeBounds(R);
            CE;
            end;
        kbAltCtrlSqBracketL,
        kbCtrlSqBracketL:
          begin
          S := '';
          Message(LeftView, evCommand, cmGetName, @S);
          if not (S[Length(S)] in ['\', '/']) then
            AddStr(S, '\');
          S := SquashesName(S);
          Message(CommandLine, evCommand, cmInsertName, @S);
          CE;
          end;
        kbAltCtrlSqBracketR,
        kbCtrlSqBracketR:
          begin
          S := '';
          Message(RightView, evCommand, cmGetName, @S);
          if not (S[Length(S)] in ['\', '/']) then
            AddStr(S, '\');
          S := SquashesName(S);
          Message(CommandLine, evCommand, cmInsertName, @S);
          CE;
          end;
      end {case};
    evBroadcast:
      case Event.Command of
        cmLookForPanels:
          ClearEvent(Event);
        cmGetUserParams, cmGetUserParamsWL:
          begin
          if LSelected then
            begin
            PP := RPanel;
            AP := LPanel
            end
          else
            begin
            PP := LPanel;
            AP := RPanel
            end;
          with PUserParams(Event.InfoPtr)^ do
            begin
            AP^.GetUserParams(Active, ActiveList, Event.Command =
               cmGetUserParamsWL);
            PP^.GetUserParams(Passive, PassiveList, Event.Command =
               cmGetUserParamsWL);
            end;
          CE;
          end;
        cmChangeDirectory:
          begin
          if LSelected
          then
            LPanel^.ChDir(PString(Event.InfoPtr)^)
          else
            RPanel^.ChDir(PString(Event.InfoPtr)^);
          CE;
          end;
        cmChangeDrv:
          begin
          Event.What := evCommand;
          if LSelected then
            LPanel^.HandleEvent(Event)
          else
            RPanel^.HandleEvent(Event);
          ClearEvent(Event);
          end;
        cmIsRightPanel:
          if Event.InfoPtr = RPanel then
            ClearEvent(Event);
      end {case};
    evCommand:
      case Event.Command of

        cmSwitchOther:
          if not PanelZoomed then
            begin
            CE;
            if LSelected then
              SwitchRight
            else
              SwitchLeft
            end;

        cmGetDirInfo:
          begin
          if LSelected then
            if  (LType = dtPanel) then
              LeftView^.HandleEvent(Event)
            else
              RightView^.HandleEvent(Event)
          else if (RType = dtPanel) then
            RightView^.HandleEvent(Event)
          else
            LeftView^.HandleEvent(Event);

          ClearEvent(Event);
          end;
        cmZoom:
          begin
          { Flash 05-02-2004 >>> }
          if (LSelected and (LType = dtView)) or
             (not LSelected and (RType = dtView))
          then
            begin
            GetBounds(OldBounds);
            GetBounds(OldPanelBounds);
            if (Size.X < Desktop.Size.X) or PanelZoomed then
              Message(@Self, evCommand, cmMaxi, nil);
            end;
          { Flash 05-02-2004 <<< }
          end;
        {Knave 11.09.99 --------------- begin 1}
        {DataCompBoy}cmMaxi:
          if RVisible and LVisible then
            begin
            CE;
            Lock;
            if LSelected
            then
              SwitchRight
            else
              SwitchLeft;
            GetBounds(OldPanelBounds);
            ChangeBounds(OldBounds);
            PanelZoomed := True;
            UnLock;
            end
          else if PanelZoomed then
            begin
            CE;
            Lock;
            ChangeBounds(OldPanelBounds);
            PanelZoomed := False;
            if LSelected
            then
              SwitchRight
            else
              SwitchLeft;
            UnLock;
            end;
        {Knave 11.09.99 --------------- end 1}
        cmGetName:
          begin
          PString(Event.InfoPtr)^:= GetString(dlFileManager);
          PString(Event.InfoPtr)^:= PString(Event.InfoPtr)^+Cut(
              {$IFDEF RecodeWhenDraw}CharToOemStr
                 {$ENDIF}(LPanel^.DirectoryName),
                (55-Length(PString(Event.InfoPtr)^)) div 2)+','
            +Cut( {$IFDEF RecodeWhenDraw}CharToOemStr
                 {$ENDIF}(RPanel^.DirectoryName),
                (55-Length(PString(Event.InfoPtr)^)) div 2);
          CE
          end;
        cmPostHideRight:
          begin
          if not RVisible then
            SwitchRight;
          RVisible := True;
          if LVisible then
            SwitchLeft;
          end;
        cmPostHideLeft:
          begin
          if not LVisible then
            SwitchLeft;
          LVisible := True;
          if RVisible then
            SwitchRight;
          end;
        cmChangeInactive:
          begin
          Event.Command := cmFindGotoFile;
          if LSelected then
            begin
            RPanel^.HandleEvent(Event);
            if RType <> dtPanel then
              SwitchView(RType);
            end
          else
            begin
            LPanel^.HandleEvent(Event);
            if LType <> dtPanel then
              SwitchView(LType);
            end;
          CE;
          end;
        cmGetDirName:
          GetDirName;
        cmPanelCompare:
          begin
          if LSelected then
            RPanel^.HandleEvent(Event)
          else
            LPanel^.HandleEvent(Event);
          end;
        cmDiskInfo:
          begin
          CE;
          if isPanel and not PanelZoomed then
            {AK155}
            begin
            SwitchView(dtInfo);
            if LSelected then
              Message(LeftView, evCommand, cmLViewFile, nil)
            else
              Message(RightView, evCommand, cmLViewFile, nil);
            end;
          end;
        {$IFDEF NETINFO}
        cmNetInfo:
          if not PanelZoomed then
            begin {-$VIV start}
            SwitchView(dtNetInfo);
            CE;
            if LSelected then
              Message(LeftView, evCommand, cmLViewFile, nil)
            else
              Message(RightView, evCommand, cmLViewFile, nil);
            end; {-$VIV end}
        {$ENDIF}
        cmLoadViewFile:
          if not PanelZoomed then
            begin
            if  (QView <> nil) and ((LType = dtView) or (RType = dtView))
            then
              QView^.HandleEvent(Event)
            else if (Info <> nil) and Info^.GetState(sfVisible) then
              Info^.HandleEvent(Event);
            CE;
            end;
        cmGLoadViewFile:
          CE;
        cmPushFullName,
        cmPushFirstName,
        cmPushInternalName:
          begin
          LeftView^.HandleEvent(Event);
          RightView^.HandleEvent(Event);
          CE;
          exit;
          end;
        cmFindTree,
        cmRereadTree:
          if LTree <> nil then
            LTree^.HandleEvent(Event);
        cmHideLeft:
          begin
          if LVisible and not RVisible then
            Message(Application, evCommand, cmShowOutput, nil)
          else
            SwitchLeft;
          CE
          end;
        cmHideRight:
          begin
          if RVisible and not LVisible then
            Message(Application, evCommand, cmShowOutput, nil)
          else
            SwitchRight;
          CE
          end;
        cmChangeLeft:
          begin
          ChangeDrvLeft;
          CE
          end;
        cmChangeRight:
          begin
          ChangeDrvRight;
          CE
          end;
        cmDirTree:
          begin
          if isPanel then
            {AK155}
            SwitchView(dtTree);
          CE
          end;
        cmQuickView:
          if isPanel and not PanelZoomed then
            {AK155}
            begin
            SwitchView(dtView);
            if LSelected then
              Message(LeftView, evCommand, cmLViewFile, nil)
            else
              Message(RightView, evCommand, cmLViewFile, nil);
            CE
            end;
        cmSwapPanels:
          if not PanelZoomed then
            begin
            Lock;
            if not RVisible then
              Message(@Self, evCommand, cmHideRight, nil)
            else if not LVisible then
              Message(@Self, evCommand, cmHideLeft, nil);
            LeftView^.GetBounds(R1);
            R1.B.Y := Size.Y-1;
            RightView^.GetBounds(R2);
            R2.B.Y := Size.Y-1;
            Lock;
            I := LType;
            LType := RType;
            RType := I;
            if  (LType = dtPanel) or (RType = dtPanel) then
              begin
              P := LPanel;
              LPanel := RPanel;
              RPanel := PFilePanel(P);
              end;
            LeftView^.ChangeBounds(R2);
            RightView^.ChangeBounds(R1);
            P := LeftView;
            LeftView := RightView;
            RightView := PView(P);
            Redraw;
            UnLock;
            if not LVisible then
              Message(@Self, evCommand, cmHideRight, nil)
            else if not RVisible then
              Message(@Self, evCommand, cmHideLeft, nil);
            UnLock;
            CE
            end;
      end {case};
  end {case};
  inherited HandleEvent(Event);
  end { TDoubleWindow.HandleCommand };

end.
