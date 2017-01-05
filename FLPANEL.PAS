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
{&Delphi+}
unit FlPanel;

interface

uses
  Files,
  {Cat}VpSysLow, {/Cat}
  Objects, Views, Drivers, Dos, Eraser, FilesCol, HideView, Drives,
  FlPanelX, DNHelp, TitleSet, Collect;

type
  PSeparator = ^TSeparator;
  TSeparator = object(THideView)
    OldX, OldW: AInt;
    Constructor Init(R: TRect; AH: integer);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; virtual;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    end;

  PFilePanel = ^TFilePanel;
  TFilePanel = object(TFilePanelRoot)
    procedure Draw; virtual;
    procedure SetState(AState: word; Enable: boolean); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    procedure GetFull(var B; P: PFileRec; C, CW: AWord); virtual;
      {DataCompBoy}
    procedure DrawTop(var B); virtual;
    end;

  TPanelBottomDnD = record
    TotalY, SelectedY, CurrentY1, CurrentY2: byte;
    end;
  PInfoView = ^TInfoView;
  TInfoView = object(THideView)
    Panel: PFilePanel;
    DnD: TPanelBottomDnD;
    Constructor Init(R: TRect);
    procedure Draw; virtual;
    {AK155: Этот метод нагружен двумя важными побочными эффектами:
        он в соответствии с настройками устанавливает свой Size.Y и
        заполняет DnD координатами строк, из которых возможен D&D.
        Первое используется в TFilePanelRoot.ChangeBounds,
        второе в HandleEvent}
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PTopView = ^TTopView;
  TTopView = object(THideView)
    Panel: PFilePanel;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; virtual;
    Constructor Load(var s: TStream);
    function GetPalette: PPalette; virtual;
    procedure Store(var s: TStream);
    end;

  PSpecScroll = ^TSpecScroll;
  TSpecScroll = object(TMyScrollBar)
    ForceScroll: boolean; {AK155}
    Panel: PFilePanel; {AK155}
    function ScrollStep(Part: longInt): longInt; virtual; {AK155}
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PDriveLine = ^TDriveLine;
  TDriveLine = object(THideView)
    Panel: PFilePanel;
    DriveLine: String[29];
    ViewLine: String[60];
    CharDelta: AInt;
    LogDrvMap: longInt; {Cat}
    Constructor Init(var R: TRect; APanel: PFilePanel);
    procedure MakeDriveLine;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; virtual;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    procedure UpDate; virtual; {Cat}
    end;

const
  CPanel = #6#7#8#9#10#32#33#34#35#36#37#38#39#40#44#45#46#47#48;
    {JO}
  CTopView = #11#12;
  CInfoView = #25#26#27#28#29#30#31;
  CDriveLine = #41#42#43;

var
  ActivePanel: Pointer absolute FlPanelX.ActivePanel;
  CtrlWas: boolean absolute FlPanelX.CtrlWas;
  DirsToChange: array[0..9] of PString absolute FlPanelX.
    DirsToChange;

var
  CurrentDirectory: String absolute FlPanelX.CurrentDirectory;

implementation

uses
  Lfn, DNUtil, DNApp, advance, advance1, advance2, advance3, Startup,
  Memory, FileCopy, Messages, Menus, DiskInfo, Dialogs, Commands,
  HistList, Tree, FBB, RStrings, ArcView, CmdLine, Histries,
    Archiver,
  Gauges, Gauge, FileFind, FLTools, DnIni, XDblWnd
  {$IFDEF UUENCODE}
  , UUCode
  {$ELSE}
  {$IFDEF  UUDECODE}
  , UUCode
  {$ENDIF}
  {$ENDIF} {, Crt}
  , xTime
  ;

const
  chTempDrive = '*';

  {  ------- TSpecScroll Скроллбар файловой панели }
  { AК155 23-06-2002
 Это опредление шага отличается от стандартного тем, что мышиный
шаг (флажок ForceScroll) на 1 вверх или вниз приводит не к
мучительному перебору всех файлов в панели, а к скачку на шаг
выше первого файла или на шаг ниже последнего файла панели. }
function TSpecScroll.ScrollStep(Part: longInt): longInt;
  var
    step: longInt;
  begin
    if Part and 2 = 0 then
      begin
        if not ForceScroll then
          begin
            step := ArStep;
            if Part and 1 = 0 then
              step := -ArStep;
          end
        else
          begin
            if (Part and 1 = 0) then
              step := (Panel^.Delta-Value-ArStep)
            else
              step := (Panel^.Delta+PgStep-Value)
          end;
      end
    else
      begin
        step := PgStep;
        if Part and 1 = 0 then
          step := -PgStep;
      end;
    ForceScroll := False;
    ScrollStep := step;
  end { TSpecScroll.ScrollStep };

procedure TSpecScroll.HandleEvent;
  begin
    if (Event.What = evKeyDown) and
      ((Event.CharCode in [^E, ^S, ^D, ^X]) or
      (Event.CharCode = #0) and ((ShiftState and 3 <> 0) xor
      (FMSetup.Options and fmoUseArrows = 0) and
      ((Event.KeyCode = kbRight) or (Event.KeyCode = kbLeft) or
      (Event.KeyCode = kbHome) or (Event.KeyCode = kbEnd)
      )))
    then
      exit;
    if (Event.What = evMouseDown) then
      begin
        QuickSearch := False; {AK155}
        ForceScroll := True; {AK155}
      end;

    inherited HandleEvent(Event);
  end;

Constructor TDriveLine.Init;
  begin
    inherited Init(R);
    Panel := APanel;
    EventMask := evMouse or evBroadcast;
    MakeDriveLine;
    CharDelta := 1;
    {Cat}
    if AutoRefreshDriveLine then
      begin
        LogDrvMap := SysGetValidDrives;
        UpdTicks := 55;
        RegisterToBackground(@Self);
      end;
    {/Cat}
  end;

Constructor TDriveLine.Load(var s: TStream);
  begin
    inherited Load(s);
    MakeDriveLine;
    CharDelta := 1;
    GetPeerViewPtr(s, Panel);
    {Cat}
    if AutoRefreshDriveLine then
      begin
        LogDrvMap := SysGetValidDrives;
        UpdTicks := 55;
        RegisterToBackground(@Self);
      end;
    {/Cat}
  end;

procedure TDriveLine.MakeDriveLine;
  var
    C: Char;
    i: byte;
  begin
    DriveLine := '';
    for C := 'A'to'Z' do
      if ValidDrive(C) then
        DriveLine := DriveLine+C;
    DriveLine := DriveLine+chTempDrive;
  end;

function TDriveLine.GetPalette;
  const
    s: String[Length(CDriveLine)] = CDriveLine;
  begin
    GetPalette := @S;
  end;

procedure TDriveLine.Draw;
  var
    B: TDrawBuffer;
    M: byte absolute DriveLine;
    i: integer;
    SDir: String;
  begin
    i := M*2+3;
    if (Panel^.Size.X >= i) then
      begin
        if (Size.X <> i) then
          begin
            GrowTo(i, 1);
            exit;
          end;
        ViewLine := '';
        for i := 1 to Length(DriveLine) do
          ViewLine := ViewLine+' '+DriveLine[i];
        ViewLine := '['+ViewLine+' ]';
      end
    else
      begin
        i := 2+M;
        if Panel^.Size.X-2 >= i then
          if (Size.X <> i)
          then
            begin
              GrowTo(i, 1);
              exit;
            end
          else
            ViewLine := '['+DriveLine+']'
        else if Panel^.Size.X <> Size.X+2
        then
          begin
            GrowTo(Panel^.Size.X-2, 1);
            CharDelta := 1;
            exit;
          end
        else
          begin
            ViewLine := Copy(DriveLine, CharDelta, Size.X-2);
            if Length(ViewLine) < Size.X-2 then
              ViewLine := ViewLine+Copy(DriveLine, 1, Size.X-2-
                Length(ViewLine));
            ViewLine := '{'+ViewLine+'}';
          end;
      end;
    if (Panel^.DirectoryName[2] = ':') or (Panel^.Drive^.DriveType =
        dtTemp)
    then
      begin
        if (Panel^.Drive^.DriveType = dtTemp) and (Panel^.
            DirectoryName = cTEMP_)
        then
          i := PosChar(chTempDrive, ViewLine)
        else
          i := PosChar(UpStrg(Panel^.DirectoryName)[1], ViewLine);
        if i > 0
        then
          MoveCStr(B, Copy(ViewLine, 1, i-1)+'~'+
          ViewLine[i]+'~'+Copy(ViewLine, i+1, MaxStringLength),
          GetColor($0301))
        else
          MoveCStr(B, ViewLine, GetColor($0301));
      end
      {JO}
    else
      begin
        lGetDir(0, SDir);
        i := PosChar(SDir[1], ViewLine);
        if i > 0 then
          MoveCStr(B, Copy(ViewLine, 1, i-1)+'~'+
          ViewLine[i]+'~'+Copy(ViewLine, i+1, MaxStringLength),
          GetColor($0301))
          {/JO}
        else
          MoveCStr(B, ViewLine, GetColor($0301));
      end;
    WordRec(B[0]).Hi := GetColor(2);
    WordRec(B[Size.X-1]).Hi := GetColor(2);
    WriteLine(0, 0, Size.X, 1, B);
  end { TDriveLine.Draw };

procedure TDriveLine.HandleEvent;
  var
    P: TPoint;
  begin
    inherited HandleEvent(Event);
    case Event.What of
      evBroadcast:
        case Event.Command of
          cmDropped:
            if MouseInView(PCopyRec(Event.InfoPtr)^.Where) then
              begin
                Event.What := evNothing;
                ClrIO;
                MakeLocal(PCopyRec(Event.InfoPtr)^.Where, P);
                if ViewLine[P.X+1] = ' ' then
                  Dec(P.X);
                case ViewLine[P.X+1] of
                  chTempDrive:
                    CopyDirName := cTEMP_;
                  'A'..'Z':
                    CopyDirName := ViewLine[P.X+1]+':';
                  else
                    exit;
                end {case};
                SkipCopyDialog := Confirms and cfMouseConfirm = 0;
                if ReflectCopyDirection
                then
                  RevertBar := (Message(Desktop, evBroadcast,
                    cmIsRightPanel, Panel) <> nil)
                else
                  RevertBar := False;
                Message(PCopyRec(Event.InfoPtr)^.Owner,
                evBroadcast, cmCopyCollection,
                PCopyRec(Event.InfoPtr)^.FC);
                SkipCopyDialog := False;
              end;
        end {case};
      evMouseDown:
        begin
          ClrIO;
          MakeLocal(Event.Where, P);
          if ViewLine[P.X+1] = ' ' then
            Dec(P.X);
          if Event.Double then
            begin
              case ViewLine[P.X+1] of
                chTempDrive:
                  begin
                    FreeStr := cTEMP_;
                    Message(Panel, evCommand, cmChangeDirectory,
                      @FreeStr);
                  end;
                'A'..'Z':
                  begin
                    FreeStr := ViewLine[P.X+1]+':\';
                    Message(Panel, evCommand, cmChangeDirectory,
                      @FreeStr);
                  end;
                '[':
                  Message(Owner, evCommand, cmHideLeft, nil);
                ']':
                  Message(Owner, evCommand, cmHideRight, nil);
                '}':
                  begin
                    Inc(CharDelta);
                    if CharDelta > Length(DriveLine)
                    then
                      CharDelta := 1;
                    DrawView;
                  end;
                '{':
                  begin
                    Dec(CharDelta);
                    if CharDelta < 1
                    then
                      CharDelta := Length(DriveLine);
                    DrawView;
                  end;
              end {case};
            end
          else if ViewLine[P.X+1] = Panel^.DirectoryName[1]
          then
            Message(Panel, evCommand, cmRereadDir, @Panel^.
              DirectoryName)
          else
            case ViewLine[P.X+1] of
              chTempDrive:
                begin
                  FreeStr := cTEMP_;
                  Message(Panel, evCommand, cmChangeDrv, @FreeStr);
                end;
              'A'..'Z':
                begin
                  FreeStr := ViewLine[P.X+1]+':';
                  Message(Panel, evCommand, cmChangeDrv, @FreeStr);
                end;
              '[':
                Message(Owner, evCommand, cmHideLeft, nil);
              ']':
                Message(Owner, evCommand, cmHideRight, nil);
              '}':
                begin
                  Inc(CharDelta);
                  if CharDelta > Length(DriveLine)
                  then
                    CharDelta := 1;
                  DrawView;
                end;
              '{':
                begin
                  Dec(CharDelta);
                  if CharDelta < 1
                  then
                    CharDelta := Length(DriveLine);
                  DrawView;
                end;
            end {case};
          {Cat: следующий кусок закомментировал, т.к. при его наличии в случае
      возникновения ошибки (например, дискета не вставлена) после
      нажатия в выскочившем окне кнопки "отмена" прекращается реакция
      на клавиатуру до тех пор, пока не будет нажата кнопка мыши;
      без этого куска для промотки строки дисков придётся нажимать
      мышкой на фигурные скобки столько раз, на сколько позиций нужно
      промотать (раньше можно было один раз нажать и удерживать мышку) }
          (*
                   while MouseEvent(Event, evMouseMove + evMouseAuto) do begin
                    MakeLocal(Event.Where, P);
                    if ViewLine[P.X+1]=' ' then dec(P.X);
                    case ViewLine[P.X+1] of
                     '}': begin
                           Inc(CharDelta);
                           if CharDelta > Length(DriveLine)
                            then CharDelta := 1;
                           DrawView;
                          end;
                     '{': begin
                           Dec(CharDelta);
                           if CharDelta < 1
                            then CharDelta := Length(DriveLine);
                           DrawView;
                          end;
                    end
                   end;
*)
          ClearEvent(Event);
        end;
    end {case};
  end { TDriveLine.HandleEvent };

procedure TDriveLine.Store(var s: TStream);
  begin
    inherited Store(s);
    PutPeerViewPtr(s, Panel);
  end;

{Cat}
procedure TDriveLine.UpDate;
  var
    newLogDrvMap: longInt;
  begin
    newLogDrvMap := SysGetValidDrives;
    if newLogDrvMap <> LogDrvMap then
      begin
        LogDrvMap := newLogDrvMap;
        MakeDriveLine;
        DrawView;
      end;
  end;
{/Cat}

{                                 TFilePanel                                 }
{----------------------------------------------------------------------------}
function TFilePanel.GetPalette;
  const
    s: String[Length(CPanel)] = CPanel;
  begin
    GetPalette := @S;
  end;

var
  Idx, CurPos, i, j: longInt;
  C, C1, C2, C3, C4, C5, C6, C7: byte;
  B: array[0..300] of AWord;
  B1: array[0..200] of record
    C: Char;
    A: byte;
    end absolute B;

procedure TFilePanel.GetFull;
  begin
    Drive^.GetFull(B, P, C, CW); {DataCompBoy}
  end;

procedure TFilePanel.DrawTop;
  var
    s: String;
    i, j: integer;
    C: word;
  begin
    Drive^.MakeTop(s);
    i := 0;
    C := GetColor($0206);
    j := CStrLen(s);
    while i < Size.X do
      begin
        MoveCStr(TWordArray(B)[i], s, C);
        Inc(i, j);
      end;
  end;

{-DataCompBoy-}
procedure TFilePanel.SetState;

  procedure MakeChange;
    var
      Event: TEvent;
      {A: Boolean;}
    begin
      CurrentDirectory := DirectoryName;
      ClrIO;
      NeedAbort := True;
      lChDir(CurrentDirectory);
      {A := Abort;}
      ActivePanel := @Self;

      DriveState := dsActive;

      if IOResult <> 0 then
        begin
          DriveState := DriveState and dsInvalid;
          exit;
        end;
      DriveState := dsActive;

      NeedAbort := DriveState and dsInvalid <> 0;

      Message(CommandLine, evCommand, cmRereadInfo, nil);
      {Cat:warn где-то в промежутке от b4.09 к b4.13 произошли какие-то изменения,
      скорее всего, связанные с командной строкой, в результате которых стал
      вылезать этот кусок. Вместе с тем, осуществляемые здесь действия на мой
      взгляд достаточно безумны, так что закомментировал их}
      (*
     if (UpStrg(ActiveDir) <> UpStrg(DirectoryName)) or A then
      begin
       Drive^.lChDir(ActiveDir);
       ReadDirectory;
       CurrentDirectory := DirectoryName;
       Event.What := evKeyDown;
       Event.KeyCode := kbTab;
       Event.InfoPtr := nil;
       PutEvent(Event);
      end;
*)
      Message(Owner, evCommand, cmChangeTree, @DirectoryName);
    end { MakeChange };

  var
    DoDraw: boolean;

  begin { TFilePanel.SetState }
    inherited SetState(AState, Enable);
    {AK155  3-02-2004 Подвал надо прятать синхронно с панелью.
       Раньше этим занимался HideView }
    if InfoView <> nil then
      InfoView^.SetState(AState, Enable);
    {/AK155  3-02-2004}
    DoDraw := False;
    if QuickSearch and (AState and (sfFocused+sfActive+sfVisible+
        sfSelected) <> 0) and not Enable
    then
      begin
        DoDraw := True;
        QuickSearch := False
      end;
    if (AState and sfActive <> 0) then
      if not Enable then
        begin
          DisableCommands(PanelCommands);
          if not GetState(sfActive+sfSelected) and
            (ScrollBar <> nil) and ScrollBar^.GetState(sfVisible)
          then
            ScrollBar^.Hide;
          DoDraw := True;
        end
      else
        EnableCommands(PanelCommands);
    ;
    if (AState and sfFocused and State <> 0) then
      if Enable then
        begin
          DoDraw := True;
          if ScrollBar <> nil then
            begin
              ScrollBar^.Show;
              ScrollBar^.Options := ScrollBar^.Options or
                ofPostProcess;
            end;
          if InfoView <> nil then
            InfoView^.DrawView;
          if DirView <> nil then
            DirView^.DrawView;
          if (Drive^.DriveType = dtDisk) then
            begin
              ActivePanel := @Self;
              AddToDirectoryHistory(DirectoryName, integer(dtDisk));
              if UpStrg(CurrentDirectory) <> UpStrg(DirectoryName)
              then
                MakeChange;
            end
          else if Drive^.DriveType = dtArc
          then
            Drive^.lChDir(#0);
          EnableCommands(PanelCommands);
        end;
    if GetState(sfFocused) then
      begin
        if AState and sfFocused <> 0 then
          DrawView;
        EnableCommands(PanelCommands);
        if (ScrollBar <> nil) and not ScrollBar^.GetState(sfVisible)
        then
          begin
            ScrollBar^.Show;
            ScrollBar^.Options := ScrollBar^.Options or
              ofPostProcess;
          end;
      end;
    if AState and (sfSelected+sfActive) <> 0 then
      if GetState(sfSelected+sfActive) then
        begin
          if (Drive^.DriveType = dtDisk)
          then
            MakeChange
          else if Drive^.DriveType = dtArc
          then
            Drive^.lChDir(#0);
          DoDraw := True;
          EnableCommands(PanelCommands)
        end
      else
        begin
          if ScrollBar <> nil then
            begin
              ScrollBar^.Hide;
              ScrollBar^.Options := ScrollBar^.Options and (not
                ofPostProcess);
            end;
          DoDraw := True;
          if InfoView <> nil then
            InfoView^.DrawView;
          if DirView <> nil then
            DirView^.DrawView;
        end;
    if DoDraw then
      begin
        PosChanged := False;
        DrawView;
      end;
    if GetState(sfFocused) then
      begin
        SetTitle(DirectoryName);
        FreeByte := Drive^.Param;
      end;
  end { TFilePanel.SetState };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFilePanel.Draw;
  label 1;
  var
    P: PFileRec;
    CW: AWord;
    Cs: AWord;
    PgS: AWord;
    s, s1: String;
    HLC: array[0..ttUpDir] of byte; {JO, AK155}
    PB: ^Byte;
    TagC: Char;

  procedure DrawAtIdx;
    var
      P: PFileRec;
      CC: byte;
      JJ: longInt;
    begin
      JJ := j*LineLength;
      if (Files <> nil) and (Idx < Files^.Count) then
        begin
          P := Files^.At(Idx);
          CC := HLC[P^.TType];
          if (Idx = CurPos) then
            begin
              LastCurPos.X := JJ;
              LastCurPos.Y := i+1
            end;
          if Idx = CurPos then
            if P^.Selected then
              begin
                C := C5;
                CC := C5
              end
            else
              C := C4
          else if P^.Selected then
            begin
              C := CC;
              CC := C3
            end
          else
            C := CC;
          if QuickSearch then
            if (Idx = CurPos) then
              begin
                if (Drive^.LFNLen < 250)
                  {$IFNDEF OS2}
                  or (Drive^.Flags and psShowLongName = 0)
                  {$ENDIF}
                then
                  SetCursor(JJ+GetCursorPos(SearchParam, P^.FlName[
                    uLfn],
                  Drive^.LFNLen, Drive^.EXTLen)-1, i)
                else
                  SetCursor(JJ+GetCursorPos(SearchParam, P^.FlName[
                    uLfn],
                  Drive^.LFNLen, Drive^.EXTLen)+Drive^.
                    CalcLengthWithoutName-1, i)
                  ;
                ShowCursor;
                NormalCursor
              end;
          MoveChar(B[JJ], ' ', C, LineLength);
          if (Idx = CurPos) and GetState(sfFocused) then
            GetFull(B[JJ], P, C shl 8+C, Cs and $00FF+C shl 8)
          else
            GetFull(B[JJ], P, CC shl 8+C, Cs);
        end
      else
        begin
          MoveChar(B[JJ], ' ', C1, LineLength);
          Drive^.GetEmpty(B[JJ], Cs);
        end;
    end { DrawAtIdx };

  begin { TFilePanel.Draw }
    if DrawDisableLvl > 0 then
      exit;
    if DrawDisableLvl < 0 then
      DrawDisableLvl := 0;
    if Loaded then
      RereadDir;
    LineLength := Drive^.CalcLength;

    if (DriveState and dsInvalid > 0) then
      begin
        C1 := GetColor(1); { Normal }
        for i := 0 to pred(Owner^.Size.Y) do
          begin
            MoveChar(B, ' ', C1, Size.X);
            WriteLine(0, i, Owner^.Size.X, 1, B[0]);
          end;
        exit;
      end;

    if Size.X > LineLength-1 then
      DeltaX := 0;
    if UpStrg(OldDirectory) <> UpStrg(DirectoryName) then
      begin
        if (UpStrg(OldDirectory[1]) <> UpStrg(DirectoryName[1])) and (
            OldDirectory <> '')
        then
          ScrollBar^.SetValue(0);
        PosChanged := False;
        DecDrawDisabled;
        OldDirectory := DirectoryName;
      end;
    C1 := GetColor(1); { Normal }
    C2 := GetColor(2); { Separator }
    C3 := GetColor(3); { Selected }
    C4 := C1;
    C5 := C3;
    HLC[0] := C1;
    HLC[ttUpDir] := C1;
    if FMSetup.TagChar <> '' then
      TagC := FMSetup.TagChar[1]
    else
      TagC := #251;
    if PanelFlags and fmiHiliteFiles <> 0 then
      for i := 1 to ttCust10 do
        HLC[i] := GetColor(6+i) {JO}
      else
      for i := 1 to ttCust10 do
        HLC[i] := C1; {JO}
    Cs := 179+C2 shl 8;
    CW := 32+C2 shl 8;
    PgS := (Size.Y-byte(FMSetup.Show and fmsColumnTitles <> 0))*((
      Size.X+1) div LineLength);
    if PgS = 0 then
      PgS := (Size.Y-byte(FMSetup.Show and fmsColumnTitles <> 0));
    ScrollBar^.PgStep := PgS;
    if Delta < 0 then
      Delta := 0;
    if {GetState(sfSelected) and}(Files^.Count > 0)
    then
      begin
        CurPos := ScrollBar^.Value;
        if CurPos < Delta then
          Delta := CurPos;
        if CurPos >= Delta+PgS then
          Delta := CurPos-PgS+1;
      end
    else
      CurPos := -1;
    if GetState(sfFocused) then
      begin
        C4 := GetColor(4); { Normal cursor }
        C5 := GetColor(5); { Selected cursor }
      end
    else if CurPos >= 0 then
      begin
        P := Files^.At(CurPos);
        if PanelFlags and fmiHiliteFiles <> 0 then
          C4 := HLC[P^.TType];
      end;
    if not QuickSearch then
      HideCursor;
    if PosChanged and (OldDelta = Delta) then
      begin
        if CurPos <> OldPos then
          begin
            for i := byte(FMSetup.Show and fmsColumnTitles <> 0) to
                Size.Y-1
            do
              for j := 0 to Size.X div LineLength do
                begin
                  Idx := i-byte(FMSetup.Show and fmsColumnTitles <> 0)+
                  j*(Size.Y-byte(FMSetup.Show and fmsColumnTitles <> 0))
                    +Delta;
                  if (Idx = CurPos) or (Idx = OldPos) then
                    begin
                      MoveChar(B, ' ', C1, Size.X);
                      DrawAtIdx;
                      Idx := (j+1)*LineLength;
                      if Idx < Size.X then
                        B[Idx-1] := Cs
                      else
                        B[Idx-1] := CW;
                      Idx := j*LineLength;
                      WriteLine(Idx, i, LineLength-1, 1, B[Idx+
                        DeltaX]);
                    end;
                end;
          end
        else
          goto 1;
        PosChanged := False;
        OldDelta := Delta;
        OldPos := CurPos;
        exit;
      end;
1:
    OldDelta := Delta;
    OldPos := CurPos;
    PosChanged := False;
    if (FMSetup.Show and fmsColumnTitles <> 0) then
      begin
        DrawTop(B);
        if WordRec(B[Size.X-1]).Lo = 179 then
          WordRec(B[Size.X-1]).Lo := 32;
        WriteLine(0, 0, Size.X, 1, B[DeltaX]);
      end;
    for i := byte(FMSetup.Show and fmsColumnTitles <> 0) to Size.Y-1 do
      begin
        MoveChar(B, ' ', C1, Size.X);
        for j := 0 to Size.X div LineLength do
          begin
            Idx := i-byte(FMSetup.Show and fmsColumnTitles <> 0)+
            j*(Size.Y-byte(FMSetup.Show and fmsColumnTitles <> 0))+
              Delta;
            DrawAtIdx;
            Idx := (j+1)*LineLength;
            if Idx < Size.X then
              B[Idx-1] := Cs
            else
              B[Idx-1] := CW;
          end;
        WriteLine(0, i, Size.X, 1, B[DeltaX]);
      end;
    if GetState(sfFocused) then
      SetTitle(DirectoryName);
  end { TFilePanel.Draw };
{-DataCompBoy-}

{                                 TInfoView                                  }
{----------------------------------------------------------------------------}
Constructor TInfoView.Init;
  begin
    inherited Init(R);
    EventMask := evMouse;
  end;

Constructor TInfoView.Load;
  begin
    inherited Load(s);
    GetPeerViewPtr(s, Panel);
  end;

procedure TInfoView.Store;
  begin
    inherited Store(s);
    PutPeerViewPtr(s, Panel);
  end;

procedure TInfoView.HandleEvent;
  var
    P: TPoint;
    Y: integer;
    Mover: PView;
    s: String;
    FC: PFilesCollection;
    C: TCopyRec;

  procedure CE;
    begin
      ClearEvent(Event)
    end;

  procedure DragCurrent;
    begin
      with Panel^ do
        begin
          s := Cut(PFileRec(Files^.At(ScrollBar^.Value))^.FlName[
            uLfn], 20); {DataCompBoy}
          if s = '..' then
            exit;
        end;
      New(FC, Init(1, 100));
      FC^.Insert(CopyFileRec(Panel^.Files^.At(Panel^.ScrollBar^.
        Value)));
      if P.X < Length(s) then
        Dec(P.X);
      MakeGlobal(P, P);
      DragMover(@P, s, FC, @C);
      CE;
    end;

  procedure DragSelected;
    var
      i: longInt;
      PF: PFileRec; {DataCompBoy}
    begin
      if Panel^.SelNum = 0 then
        exit;
      New(FC, Init(Panel^.SelNum, 100));
      for i := 1 to Panel^.Files^.Count do
        begin
          PF := Panel^.Files^.At(i-1); {DataCompBoy}
          if PF^.Selected then
            FC^.Insert(CopyFileRec(PF)); {DataCompBoy}
        end;
      MakeGlobal(P, P);
      DragMover(@P, ItoS(Panel^.SelNum)+GetString(dlSelectedFiles), FC,
        @C);
      CE;
    end;

  procedure DragTotals;
    var
      n, i, Start: longInt;
    begin
      Start := FirstNameNum(Panel)-1;
      with Panel^.Files^ do
        begin
          n := Count-Start;
          New(FC, Init(n, 100));
          for i := Start to n+Start-1 do
            FC^.AtInsert(FC^.Count, CopyFileRec(PFileRec(At(i))));
        end;
      MakeGlobal(P, P);
      DragMover(@P, ItoS(n)+' '+GetString(dlDIFiles), FC, @C);
      CE;
    end;

  begin{TInfoView.HandleEvent}
    inherited HandleEvent(Event);
    if Event.What and (evMouseDown+evMouseAuto) <> 0 then
      begin
        if Panel^.Files^.Count = 0 then
          exit;
        C.Owner := Panel;
        MakeLocal(Event.Where, P);
        Y := 0;
        if (FMSetup.Show and fmsDivider) <> 0 then
          begin
            if Y = P.Y then
              begin{ мышь в разделителе }
                if Panel^.Files^.Count = 0 then
                  exit;
                if (P.X >= Panel^.SelectedInfoInDividerMin) and
                  (P.X <= Panel^.SelectedInfoInDividerMax)
                then
                  begin
                    DragSelected;
                    exit;
                  end; {AK155}
                if (P.X >= Panel^.TotalInfoInDividerMin) and
                  (P.X <= Panel^.TotalInfoInDividerMax)
                then
                  begin
                    DragTotals;
                    exit;
                  end; {AK155}
                if Panel^.GetState(sfActive) and not Panel^.GetState(
                    sfSelected)
                then
                  Panel^.Select; {AK155}
                if Panel^.GetState(sfActive+sfSelected) then
                  begin
                    with Panel^ do
                      begin
                        MSelect := Event.Buttons and mbRightButton <> 0;
                        if MSelect then
                          begin
                            SelectFlag := not PFileRec(Files^.At(
                              ScrollBar^.Value))^.Selected;
                            Message(Panel, evKeyDown, kbIns, nil);
                          end;
                      end;
                    RepeatDelay := 0;
                    repeat
                      with Panel^ do
                        PSpecScroll(ScrollBar)^.ForceScroll := not
                          MSelect; {AK155}
                      Message(Self.Owner, evKeyDown, kbDown, nil);
                    until not MouseEvent(Event, evMouseMove+
                      evMouseAuto);
                    RepeatDelay := 2;
                    Panel^.MSelect := False;
                  end;
                exit
              end;
            Inc(Y);
          end;

        { D&D из подвала панели }
        if (P.Y = DnD.CurrentY1) or (P.Y = DnD.CurrentY2) then
          DragCurrent
        else if P.Y = DnD.SelectedY then
          DragSelected
        else if P.Y = DnD.TotalY then
          DragTotals;
        CE;
      end;
  end { TInfoView.HandleEvent };

{ построение краткой строки информации о каталоге (без слов)}
function BriefTotalString(Panel: PFilePanelRoot): String;
  var
    s: String absolute Result;
    FilesCount: longInt;
  begin
    FilesCount := Panel^.Files^.Count-(FirstNameNum(Panel)-1);
    s := FStr(Panel^.TotalInfo)+'('+ItoS(FilesCount)+')';
  end;

{ построение длинной строки информации о каталоге (со словами) }
function TotalString(Panel: PFilePanelRoot): String;
  var
    s: String absolute Result;
    FilesCount: longInt;
  begin
    FilesCount := Panel^.Files^.Count-(FirstNameNum(Panel)-1);
    if FilesCount = 0 then
      s := GetString(dlDINoFiles)
    else
      begin
        s := GetString(dlTotal);
        if FilesCount = 1 then
          s := s+'1~ '+GetString(dlDIFile)
        else
          s := s+ItoS(FilesCount)+'~ '+GetString(dlDIFiles);
        s := s+GetString(dlDIWith)+'~';
        if C = 1 then
          s := s+'1~ '+GetString(dlDIByte)
        else
          s := s+FStr(Panel^.TotalInfo)+'~ '+GetString(dlDIBytes);
      end;
  end { TotalString };

{-DataCompBoy-}
procedure TInfoView.Draw;
  var
    B: TDrawBuffer;
    B1: array[0..127] of record
      C: Char;
      A: byte;
      end absolute B;
    i, j: longInt;
    C1, C2, C3: word;
    PF, PPF: PFileRec;
    s, s1, s2, S3: String;
    Y: longInt;
    l: longInt;
    ShowSelected, ShowFullName: boolean;
    ShowFree, DescriptionInBottom: boolean;

    { построение краткой строки информации об отмеченных файлах (без слов)}
  function BriefSelString: String;
    begin
      with Panel^ do
        begin
          Result := '~'+FStr(SelectedLen);
          if (Drive^.DriveType = dtArc) and (SelectedLen <>
              PackedLen)
          then
            Result := Result+'/'+FStr(PackedLen);
          Result := Result+'('+ItoS(SelNum)+')~';
        end;
    end;

  { построение длинной строки информации об отмеченных файлах (со словами) }
  function SelString: String;
    begin
      with Panel^ do
        begin
          Result := '~'+FStr(SelectedLen);
          if (Drive^.DriveType = dtArc) and (SelectedLen <>
              PackedLen)
          then
            Result := Result+'('+FStr(PackedLen)+')';

          Result := Result+GetString(dlBytesIn)+
          +ItoS(SelNum)+'~'+GetString(dlSelectedFiles);
        end;
    end;

  { построение строки текущей маски быстрого поиска }
  function QuickSearchString: String;
    var
      s: String;
    begin
      s := Panel^.SearchParam.Mask;
      if QuickSearchType = 1 then
        SetLength(s, Length(s)-1);
      Result := GetString(dlFileSearch);
      if Length(s)+Length(Result) > Size.X then
        Result := Result+#17+'~'+
        Copy(s, Length(s)-Size.X-Length(s1)-1, 255)+'~'
      else
        Result := Result+'~'+s+'~';
    end;

  const
    NoDnD: TPanelBottomDnD =
    (TotalY: 255; SelectedY: 255; CurrentY1: 255; CurrentY2: 255);
  begin { TInfoView.Draw }
    if Panel^.DrawDisableLvl > 0 then
      exit;
    if Size.X <= 0 then
      exit;
      {без этого бывает плохо, если разделитель
   панелей сдвинуть до упора влево, а потом двигать назад }
    DnD := NoDnD;
    ShowSelected := (Panel^.PanelFlags and fmiSelected) <> 0;
    ShowFullName := (Panel^.PanelFlags and fmiFullNameInBottom) <> 0;
    C2 := Panel^.GetColor(2);
    i := Size.X;
    C1 := Panel^.LineLength;
    if i < C1 then
      i := C1;
    Dec(C1);
    Y := 0;

    { линия разделителя }
    if FMSetup.Show and fmsDivider <> 0 then
      begin
        MoveChar(B, #196, C2, Size.X+Panel^.DeltaX);
        i := 0;
        C2 := (C2 shl 8) or 193;
        while i < Size.X+Panel^.DeltaX do
          begin
            Panel^.Drive^.GetEmpty(B[i], C2);
            Inc(i, Panel^.LineLength);
            if i = Size.X+Panel^.DeltaX then
              B[i-1] := (C2 and $FF00)+196;
          end;

        {кое-какую информацию выводим поверх линии разделителя }
        s := '' {выделенные};
        s2 := '' {о каталоге};
        i := 0;
        Panel^.SelectedInfoInDividerMax := 0;
        Panel^.TotalInfoInDividerMax := 0;
        if not ShowSelected and (Panel^.SelNum > 0) then
          begin{ информация о выделенных файлах }
            C3 := GetColor($0302);
            {MoveChar(B, '─', C3, Size.X);}
            if BriefSelectedInfo then
              begin
                s := BriefSelString;
                i := 1;
              end
            else
              begin
                s := SelString;
                i := (Size.X-CStrLen(s)) div 2;
                if i < 0 then
                  i := 0;
              end;
            Panel^.SelectedInfoInDividerMin := i;
            Panel^.SelectedInfoInDividerMax := i+CStrLen(s)-1;
          end
        else if not ShowSelected and not ShowFullName and
            ShowFileMask and {Panel^.}QuickSearch and (Panel = ActivePanel)
            { Flash 25-01-2004:
              Если не сравнивать текущую панель с активной, то
              при включённом автообновлении строка с маской быстрого поиска
              может появиться на обеих панелях, а это нонсенс. }
        then
          begin{ маска быстрого поиска поверх }
            C3 := GetColor($0302);
            s := QuickSearchString;
            i := 0;
          end;
        if s <> '' then
          MoveCStr(B[i], s, C3);
        if BriefTotalInfo and (Panel^.PanelFlags and fmiTotals = 0)
        then
          begin{ информация о каталоге }
            s2 := BriefTotalString(Panel);
            if Panel^.SelectedInfoInDividerMax+2+Length(s2) < Size.X
            then
              begin
                MoveStr(B[Size.X-Length(s2)], s2, Panel^.GetColor(2));
                Panel^.TotalInfoInDividerMin := Size.X-Length(s2);
                Panel^.TotalInfoInDividerMax := Size.X-1;
              end;
          end;
        WriteLine(0, Y, Size.X, 1, B[Panel^.DeltaX]);
        Inc(Y);
      end;

    { описатель текущего файла }
    PF := Panel^.Files^.At(Panel^.ScrollBar^.Value);

    { информация о текущем файле }
    C1 := GetColor(1);
    if Panel^.PanelFlags and fmiCurrent <> 0 then
      begin
        DnD.CurrentY1 := Y;
        MoveChar(B, ' ', C1, Size.X);
        if PF <> nil then
          Panel^.Drive^.GetDown(B, C1, PF);
        WriteLine(0, Y, Size.X, 1, B);
        Inc(Y);
        case Panel^.Drive^.DriveType of
          dtFind, dtTemp, dtList, dtArcFind:
            if PathInfoInBottom then
              begin
                MoveChar(B, ' ', C1, Size.X);
                if (PF <> nil) and (PF^.Owner <> nil) then
                  MoveStr(B, {$IFDEF RecodeWhenDraw}CharToOemStr
                    {$ENDIF}(PF^.Owner^), C1);
                WriteLine(0, Y, Size.X, 1, B);
                Inc(Y);
              end;
          dtArc:
            if PackingInfoInBottom then
              begin
                MoveChar(B, ' ', C1, Size.X);
                C3 := GetColor($0302);
                if PF <> nil then
                  begin
                    s := ZtoS(Percent(PF^.Size, PF^.PSize))+'%';
                    MoveCStr(B, GetString(dlArcPSize)+' '+AddSpace(
                      FStr(PF^.PSize), 14)+
                    GetString(dlArcRatio)+s, C3);
                  end;
                WriteLine(0, Y, Size.X, 1, B);
                Inc(Y);
              end { case};
        end {case};
      end;

    { Выделенные файлы, маска панели, маска быстрого поиска, длинное имя }
    if ShowSelected or ShowFullName then
      begin
        s := '';
        i := -1; { по умолчанию - центрировать }
        C3 := GetColor($0302);
        MoveChar(B, ' ', C3, Size.X);
        if ShowSelected and (Panel^.SelNum <> 0) then
          begin
            DnD.SelectedY := Y;
            s := SelString;
          end
        else if ShowFileMask and (x_x <> Panel^.FileMask) and ('*' <>
            Panel^.FileMask)
        then{-$VIV 19.05.99--}
          begin
            s := fReplace(' ', '', Panel^.FileMask);
            if Copy(s, 1, 1) = ';' then
              Delete(s, 1, 1);
            s := GetString(dlFileMask)+s;
          end
        else if ShowFileMask and (Panel = ActivePanel) and (
            {Panel^.}QuickSearch)
        then
          begin
            s := QuickSearchString;
            i := 0; { прижать влево }
          end
        else if ShowFullName and (PF <> nil) then
          begin{ длинное имя прижимаем влево и обрезаем слева }
            DnD.CurrentY2 := Y;
            if PF^.TType <> ttUpDir then
              begin
                s := PF^.FlName[True];
                i := 0;
                if Length(s) > Size.X then
                  begin
                    System.Delete(s, 2, Length(s)-Size.X);
                    s[1] := #17;
                  end;
                s := FormatLongName(s, Size.X, 0, flnHandleTildes,
                  nfmNull, nil);
              end;
          end
        else if ShowSelected then
          s := GetString(dlNoFilesSelected);
        if i < 0 then
          i := (Size.X-CStrLen(s)) div 2;
        if i < 0 then
          i := 0;
        MoveCStr(B[i], s, C3);
        WriteLine(0, Y, Size.X, 1, B);
        Inc(Y);
      end;

    if Panel^.PanelFlags and fmiTotals <> 0 then
      begin
        DnD.TotalY := Y;
        C3 := GetColor($0504);
        MoveChar(B, ' ', C3, Size.X);
        s2 := TotalString(Panel);
        i := (Size.X-CStrLen(s2)) div 2;
        if i < 0 then
          i := 0;
        MoveCStr(B[i], s2, C3);
        WriteLine(0, Y, Size.X, 1, B);
        Inc(Y);
      end;

    { Свободное место, описание текущего файла }
    ShowFree := Panel^.PanelFlags and fmiFree <> 0;
    DescriptionInBottom := Panel^.PanelFlags and
      fmiDescriptionInBottom <> 0;
    if (DescriptionInBottom or ShowFree) and
      (Panel^.Drive^.DriveType = dtDisk)
    then
      begin
        {S1: свободное место (цифры); S2: описание; S3: текст свободного места;
   I: позиция начала выдачи S1 с учетом центрирования, когда нет описания;
   L - ширина поля S1 вместе с двумя пробелами }
        l := 0;
        s1 := '';
        s2 := '';
        S3 := '';
        if ShowFree then
          begin
            s1 := Panel^.FreeSpace;
            l := CStrLen(s1)+2;
            {Cat: 1) если возвращена пустая строка, то не должны ничего пририсовывать
            2) если это не диск, а сетевой путь, то вместо буквы диска выводим имя ресурса}
            if s1 = '' then
              S3 := ''
            else
              begin
                with Panel^.Drive^ do
                  if CurDir[2] = ':' then
                    S3 := GetString(dlDIFreeDisk)+CurDir[1]+':'
                  else if (CurDir[1] = '\') and (CurDir[2] = '\')
                  then
                    begin
                      for i := 3 to Length(CurDir) do
                        if CurDir[i] = '\' then
                          begin
                            for i := i+1 to Length(CurDir) do
                              if CurDir[i] = '\' then
                                begin
                                  Dec(i);
                                  break;
                                end;
                            break;
                          end;
                      S3 := GetString(dlDIFreeDisk)+Copy(CurDir, 1, i);
                    end
                  else
                    S3 := '';
                System.Delete(S3, 1, 2); { убрать ненужные '~ '}
                  {Cat:warn надо бы это с ресурсами увязать по-человечески}
              end;
            {/Cat}
            i := (Size.X-(l-2+CStrLen(S3))) div 2-1;
            if i < 0 then
              i := 0;
            { центрируем обычный показ свободного места }
          end;
        if DescriptionInBottom then
          begin
            if (PF <> nil) and (PF^.DIZ <> nil) and (PF^.DIZ^.DIZ <>
                nil)
            then
              begin
                s2 := PF^.DIZ^.DIZ^;
                if Panel^.Drive^.Flags and psShowDescript <> 0 then
                  begin
                    { выводим в подвале то, что не поместилось в панели }
                    {$IFNDEF OS2} if Panel^.Drive^.Flags and
                        psShowLongName <> 0
                    then{$ENDIF}
                      j := Panel^.Drive^.LFNLen
                      {$IFNDEF OS2}
                    else
                      j := 12 {$ENDIF};
                    j := Size.X-j+Panel^.DeltaX-1;
                    System.Delete(s2, 1, j);
                  end;
                if Length(s2)+l > Size.X then
                  begin
                    SetLength(s2, Size.X-l);
                    s2[Size.X-l] := FMSetup.RestChar[1];
                  end;
                if s2 <> '' then
                  begin
                    i := 0;
                      { свободное место с описанием не центрируем }
                    for j := 1 to Length(s2) do
                      if s2[j] = #20 then
                        s2[j] := ' ';
                    { #20 - это разделитель строк многострочного описания }
                  end;
              end;
          end;

        C3 := GetColor($0706);
        MoveChar(B, ' ', C3, Size.X);
        MoveCStr(B[i], s1, C3);
        if s2 <> '' then
          begin
            C3 := GetColor($301);
            MoveStr(B[l], s2, C3);
          end
        else
          MoveCStr(B[i+l], S3, C3);
        WriteLine(0, Y, Size.X, 1, B);
        Inc(Y);
      end;
    Size.Y := Y;
  end { TInfoView.Draw };
{-DataCompBoy-}

function TInfoView.GetPalette;
  const
    s: String[Length(CInfoView)] = CInfoView;
  begin
    GetPalette := @S;
  end;

{ ---------------------------- TTopView ------------------------------ }

Constructor TTopView.Load;
  begin
    inherited Load(s);
    GetPeerViewPtr(s, Panel);
  end;

procedure TTopView.Store;
  begin
    inherited Store(s);
    PutPeerViewPtr(s, Panel);
  end;

function TTopView.GetPalette;
  const
    s: String[Length(CTopView)] = CTopView;
  begin
    GetPalette := @S;
  end;

procedure TTopView.Draw;
  var
    C: word;
    B: TDrawBuffer;
    i: longInt;
    s: String;
    R: TRect;
  begin
    C := GetColor(1);
    if not Panel^.GetState(sfSelected) then
      C := GetColor(2);
    MoveChar(B, ' ', C, Size.X);
    {S := Panel^.DirectoryName;
 if Length(S) > Panel^.Size.X - 10  then
  begin
   Delete(S, PosChar(':', S) + 2, Length(S) - Panel^.Size.X + 13);
   Insert('...', S, PosChar(':', S) + 2);
  end;}
    (*X-Man*)
    s := Cut( {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Panel^.
      DirectoryName), Panel^.Size.X-10);
    if Length(s)+2 <> Size.X then
      if Length(s) < Panel^.Size.X-2 then
        begin
          R.A := Panel^.Origin;
          R.B.Y := R.A.Y;
          Dec(R.A.Y);
          Inc(R.A.X, (Panel^.Size.X-Length(s)-2) div 2);
          R.B.X := R.A.X+Length(s)+2;
          Locate(R);
          exit;
        end
      else
        GrowTo(0, 1);
    if Length(s) > Size.X then
      i := 0
    else
      i := (Size.X-Length(s)) div 2;
    MoveStr(B[i], s, C);
    WriteLine(0, 0, Size.X, Size.Y, B);
  end { TTopView.Draw };

procedure TTopView.HandleEvent;
  var
    s: String;
    i: longInt;
    P: TPoint;
  begin
    inherited HandleEvent(Event);
    case Event.What of
      evMouseDown:
        begin
          MakeLocal(Event.Where, P);
          s := Cut( {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(
            Panel^.DirectoryName), Panel^.Size.X-10);
          if Length(s) > Size.X then
            i := 0
          else
            i := PosChar(':', s);
          repeat
          until not MouseEvent(Event, evMouseAuto+evMouseMove);
          ClearEvent(Event);
          if (P.X <= i)
          then
            Message(Panel, evCommand, cmChangeDrive, nil)
          else
            Message(Panel, evCommand, cmChangeDir, nil);
        end;
    end {case};
  end { TTopView.HandleEvent };

{ --------------------------- TSeparator ----------------------------- }

Constructor TSeparator.Load;
  begin
    inherited Load(s);
    s.Read(OldX, 4);
  end;

procedure TSeparator.Store;
  begin
    inherited Store(s);
    s.Write(OldX, 4);
  end;

Constructor TSeparator.Init;
  begin
    inherited Init(R);
    OldX := Origin.X+1;
    OldW := AH;
    EventMask := $FFFF;
  end;

procedure TSeparator.HandleEvent;
  var
    P: TPoint;
    R: TRect;
    RD: integer;
    B: byte;
  begin
    inherited HandleEvent(Event);
    case Event.What of
      evMouseDown:
        begin
          MakeLocal(Event.Where, P);
          B := P.X;
          RD := RepeatDelay;
          RepeatDelay := 0;
          repeat
            Owner^.MakeLocal(Event.Where, P);
            if (P.X >= 1) and (P.X < Owner^.Size.X-2) then
              begin
                OldX := P.X+1-B;
                OldW := Owner^.Size.X;
                R.A := Owner^.Origin;
                R.B.X := Owner^.Origin.X+Owner^.Size.X;
                R.B.Y := Owner^.Origin.Y+Owner^.Size.Y;
                Owner^.ChangeBounds(R);
              end;
          until not MouseEvent(Event, evMouseAuto+evMouseMove);
          RepeatDelay := RD;
          ClearEvent(Event);
        end;
    end {case};
  end { TSeparator.HandleEvent };

procedure TSeparator.Draw;
  var
    B: array[0..128] of record
      C: Char;
      B: byte;
      end;
    C: word;
    Ch: Char;
  begin
    RK := Owner^.GetColor(2);
    if Owner^.GetState(sfActive) then
      C := RK
    else
      C := Owner^.GetColor(1);
    if Owner^.GetState(sfDragging) then
      C := Owner^.GetColor(3);
    B[0].B := C;
    B[Size.Y-1].B := C;
    if Owner^.GetState(sfActive) and not Owner^.GetState(sfDragging)
    then
      begin
        B[0].C := #187;
        Ch := #186;
        B[Size.Y-1].C := #188;
      end
    else
      begin
        B[0].C := #191;
        Ch := #179;
        B[Size.Y-1].C := #217;
      end;
    MoveChar(B[1], Ch, C, Size.Y-2);
    WriteBuf(0, 0, 1, Size.Y, B);
    if Owner^.GetState(sfActive) and not Owner^.GetState(sfDragging)
    then
      begin
        B[0].C := #201;
        B[Size.Y-1].C := #200;
      end
    else
      begin
        B[0].C := #218;
        B[Size.Y-1].C := #192;
      end;
    WriteBuf(1, 0, 1, Size.Y, B);
  end { TSeparator.Draw };

{ FilePanel HandleEvent}

{-DataCompBoy-}
procedure TFilePanel.HandleEvent;
  var
    PF: PFileRec;
    CurPos: longInt;
    PPC: PCollection;
    MPos: TPoint;
    LastRDelay: word;
    i, j: longInt;
    PDr: PDrive;
    KeyCode: word; {Cat}

  procedure CE;
    begin
      ClearEvent(Event)
    end;
  procedure CED;
    begin
      ClearEvent(Event);
      DrawView
    end;

  procedure CM_CopyUnselect;
    label 1;
    var
      PF: PFileRec;
      i, OSM: longInt;
    begin
      if (Files^.Count = 0) or (Event.InfoPtr = nil) then
        exit;
      OSM := PFilesCollection(Files)^.SortMode;
      PFilesCollection(Files)^.SortMode := 100;
      for i := 0 to Files^.Count-1 do
        if Files^.Compare(Files^.At(i), Event.InfoPtr) = 0 then
          goto 1; {-$VOL}
      PFilesCollection(Files)^.SortMode := OSM;
      exit;
1:
      PFilesCollection(Files)^.SortMode := 100;
      PF := {Event.InfoPtr}Files^.At(i);
      if PF^.Attr and Directory <> 0 then
        PF^.Size := -1;
      with PF^ do
        if Selected then
          begin
            Selected := False;
            SelectedLen := SelectedLen-Size;
            PackedLen := PackedLen-PSize;
            Dec(SelNum)
          end;
      if TimerExpired(_Tmr1) then
        begin
          DrawView;
          if InfoView <> nil then
            InfoView^.DrawView;
          NewTimer(_Tmr1, 10);
        end;
    end { CM_CopyUnselect };

  function MaskSearch(B: byte): boolean;
    var
      i: longInt;
    begin
      MaskSearch := True;
      i := CurPos+B;
      if i >= Files^.Count then
        i := 0;
      repeat
        if InQSMask(PFileRec(Files^.At(i))^.FlName[uLfn],
            SearchParam.Mask)
        then
          begin
            ScrollBar^.SetValue(i);
            exit;
          end;
        Inc(i);
        if i >= Files^.Count then
          i := 0;
      until i = CurPos
      ;
      MaskSearch := False;
    end { MaskSearch };

  procedure DoMakeDirs(P: PFileRec);
    begin
      if (PF^.Attr and Directory <> 0) and (PF^.Size < 0) then
        begin
          PF^.Size := 0;
          PF^.Attr := PF^.Attr and $3F
        end;
    end;

  label lbMakeUp, GotoExt, GotoKb, lbMakeDown;

  begin { TFilePanel.HandleEvent }
    {$IFNDEF OS2}uLfn := Drive^.Flags and psShowLongName <> 0;
      {$ENDIF}
    inherited HandleEvent(Event);
    if Event.What = evNothing then
      exit;
    CurPos := ScrollBar^.Value;
    if Files <> nil
    then
      if Files^.Count > CurPos
      then
        PF := Files^.At(CurPos)
      else
        PF := nil
    else
      PF := nil;
    i := ShiftState;
    KeyCode := Event.KeyCode and $FFFF; {Cat}
    if (Event.What = evKeyDown) and (i and 3 <> 0) and
      ((KeyCode = kbCtrlRight and $FFFF) or (KeyCode = kbCtrlLeft
        and $FFFF) or
      ((KeyCode = kbBack and $FFFF) and not QuickSearch))
    then
      begin
        CommandHandle(Event);
        exit;
      end;
    if (Event.What = evKeyDown) and (CommandLine <> nil) then
      if ((i and (kbRightShift+kbLeftShift) <> 0) and
        ((KeyCode = kbDown and $FFFF) or
        (KeyCode = kbUp and $FFFF) or
        ((KeyCode = kbCtrlIns and $FFFF) and (CmdLine.Str <> '')) or
        (KeyCode = kbGrayAst and $FFFF)
        )
        ) or
        (((Event.ScanCode < Hi(kbAlt1)) or
        (Event.ScanCode > Hi(kbAlt9))
        ) and
        (Event.CharCode = #0) and
        (CmdLine.Str <> '') and
        (((i and 3 <> 0) xor (FMSetup.Options and fmoUseArrows = 0))
          and
        ((KeyCode = kbRight and $FFFF) or (KeyCode = kbLeft and
          $FFFF) or
        (KeyCode = kbHome and $FFFF) or (KeyCode = kbEnd and $FFFF)
        )
        )
        )
      then
        CommandLine^.HandleEvent(Event);
    if Event.What = evNothing then
      exit;
    i := ShiftState2;
    case Event.What of
      evCommand:
        case Event.Command of
          cmDoSendLocated:
            SendLocated;
          cmGetName:
            begin
              if Drive^.DriveType = dtDisk then
                begin
                  {$IFNDEF OS2}
                  if Drive^.Flags and psShowLongName = 0 then
                    PString(Event.InfoPtr)^:= lfGetShortFileName(
                      DirectoryName)
                  else
                    {$ENDIF}
                    PString(Event.InfoPtr)^:= DirectoryName;
                end
              else if ScrollBar^.Value < Files^.Count then
                PString(Event.InfoPtr)^:= PFileRec(Files^.At(
                  ScrollBar^.Value))^.Owner^;
              ClearEvent(Event);
            end;
          cmKillUsed:
            Drive^.KillUse;
          cmClose:
            CommandEnabling := False;
          cmCopyUnselect:
            begin
              CM_CopyUnselect;
              CE
            end;
          cmRecountDirs:
            begin
              if Files <> nil then
                Files^.ForEach(@DoMakeDirs);
              CED;
            end;
          0..3, 5..100:
            ;
          else
            CommandHandle(Event);
        end {case};
      evKeyDown:
        if (((Event.KeyCode = kbDoubleAlt)) and (FMSetup.Quick =
            pqsAlt)) or
          ((Event.KeyCode = kbDoubleCtrl) and (FMSetup.Quick =
            pqsCtrl)) or
          ((Event.CharCode >= #32) and (Event.CharCode <= #254) and
          (Event.CharCode <> '\') and (FMSetup.Quick = pqsCaps) and
          (i and $40 <> 0)) or
          ((Event.CharCode >= #32) and (Event.CharCode <= #254) and
          (ShiftState and 3 <> 0) and (ShiftState and 4 = 0) and
          (not CommandLine^.GetState(sfVisible)) and
          (InterfaceData.Options and ouiHideCmdline <> 0))
        then
          begin
            CtrlWas := False;
            InitQuickSearch(SearchParam);
            QuickSearch := not QuickSearch;
            if not QuickSearch then
              InfoView^.DrawView; {Cat}
            if (Event.CharCode >= #32) and (Event.CharCode <= #254)
            then
              begin
                if not ((ShiftState and 3 <> 0) and (not CommandLine^.
                    GetState(sfVisible)))
                then
                  begin
                    ShiftState2 := i and $BF;
                    ShiftState := ShiftState and kbCapsState;
                  end
                else
                  ShiftState := ShiftState and $FC;
                DoQuickSearch(SearchParam, Event.KeyCode);
                if QuickSearch then
                  begin
                    if (FMSetup.Quick = pqsCaps) and (i and
                        kbCapsState <> 0)
                    then
                      begin
                        (*
                        {$IFDEF VIRTUALPASCAL}
                        SysTVSetShiftState(ShiftState and not kbCapsState);
                        ShiftState := SysTVGetShiftState;
                        {$ENDIF}
                        *)
                      end;
                    if not MaskSearch(0) then
                      DoQuickSearch(SearchParam, kbBack);
                  end;
              end;
            DrawView;
            CE;
          end
        else if QuickSearch and ((Event.CharCode > #31) or
          (Event.CharCode = #27) or
          (Event.CharCode = #10) or
          (Event.KeyCode = kbBack) or
          (Event.KeyCode = kbBackUp) or
          (Event.KeyCode = kbIns))
        then
          begin
            CtrlWas := False;
            if (Event.KeyCode = kbGrayPlus) or (Event.KeyCode =
                kbGrayMinus) or
              (Event.KeyCode = kbGrayAst) or (Event.KeyCode = kbIns) or
              (Event.KeyCode = kbBackUp)
            then
              goto GotoKb;
            case Event.CharCode of
              #27:
                begin
                  QuickSearch := False;
                  InitQuickSearch(SearchParam);
                  InfoView^.DrawView;
                  CED;
                  exit
                end;
              #10:
                MaskSearch(1);
              '\':
                begin
                  Event.What := evCommand;
                  Event.Command := cmChangeDir;
                  Event.InfoPtr := nil;
                  PutEvent(Event);
                  Event.What := evKeyDown;
                  for i := 1 to Length(SearchParam.Mask) do
                    if SearchParam.Mask[i] <> '*' then
                      begin
                        Event.CharCode := SearchParam.Mask[i];
                        PutEvent(Event);
                      end;
                  CE;
                  QuickSearch := False;
                  InfoView^.DrawView; {Cat}
                  exit;
                end;
              else
                begin
                  DoQuickSearch(SearchParam, Event.KeyCode);
                  if not MaskSearch(0) then
                    DoQuickSearch(SearchParam, kbBack);
                end;
            end {case};
            InfoView^.DrawView;
            CED;
          end
        else
          begin
            CtrlWas := False;
            if QuickSearch then
              begin
                QuickSearch := False;
                InfoView^.DrawView;
              end;
GotoKb:
            case Event.KeyCode of
              kbDel:
                if ((CmdLine.Str = '') and (FMSetup.Options and
                    fmoDelErase <> 0))
                then
                  Message(@Self, evCommand, cmPanelErase, nil);
              kbShiftDel:
                if ((CmdLine.Str = '') and (FMSetup.Options and
                    fmoDelErase <> 0))
                then
                  Message(@Self, evCommand, cmSingleDel, nil);
              { Flash >>> }
              kbCtrlHome:
                begin
                  CE;
                  DeltaX := 0;
                  OldDelta := -1;
                  Owner^.Redraw
                end;
              kbCtrlEnd:
                begin
                  CE;
                  DeltaX := LineLength-Size.X-1;
                  OldDelta := -1;
                  Owner^.Redraw
                end;
              kbHome:
                begin
                  CE;
                  OldDelta := -1;
                  ScrollBar^.SetValue(0);
                  Owner^.Redraw
                end;
              { Flash <<< }
              kbEnd:
                begin
                  CE;
                  OldDelta := -1;
                  ScrollBar^.SetValue(Files^.Count-1)
                end;
              kbUp, kbDown, kbCtrlUp, kbCtrlDown, kbCtrlShiftUp,
                kbCtrlShiftDown, kbUpUp, kbDownUp
              :
                begin
                  CtrlWas := ShiftState and kbCtrlShift <> 0;
                  if Event.KeyCode = kbCtrlUp then
                    Event.KeyCode := kbUp;
                  if Event.KeyCode = kbCtrlDown then
                    Event.KeyCode := kbDown;
                  if (Event.KeyCode = kbDownUp)
                    or (Event.KeyCode = kbUpUp)
                  then
                    begin
                      CE;
                      exit
                    end;
                end;
              kbIns, kbSpace:
                if CurPos < Files^.Count then
                  begin
                    QuickSearch := False;
                    if (Event.CharCode = ' ') and ((CmdLine.Str <> '')
                        or
                      (FMSetup.Options and fmoSpaceToggle = 0))
                    then
                      exit;
                    CE;
                    if Files^.Count = 0 then
                      exit;
                    PF := Files^.At(CurPos);
                    if PF^.TType <> ttUpDir then
                      begin
                        PF^.Selected := not PF^.Selected;
                        SelectedLen := SelectedLen-(1-2*integer(PF^.
                          Selected))*PF^.Size;
                        PackedLen := PackedLen-(1-2*integer(PF^.
                          Selected))*PF^.PSize;
                        Dec(SelNum, 1-2*integer(PF^.Selected));
                      end;
                    ScrollBar^.SetValue(CurPos+1);
                    if CurPos = ScrollBar^.Value then
                      DrawView;
                    if InfoView <> nil then
                      InfoView^.DrawView;
                  end;
              kbAltQuote:
                begin
                  SystemData.Options := SystemData.Options xor
                    ossShowHidden;
                  ConfigModified := True;
                  Message(Application, evCommand, cmUpdateConfig,
                    nil)
                end
                else
                CommandHandle(Event);
            end {case};
          end;
      evBroadcast:
        case Event.Command of
          cmGetCurrentPosFiles,
          cmFindForced,
          cmInsertDrive,
          cmUnArchive,
          cmCopyCollection,
          cmDropped:
            CommandHandle(Event);

          cmScrollBarChanged:
            if ScrollBar = Event.InfoPtr then
              begin
                if MSelect then
                  begin
                    CE;
                    if Files <> nil then
                      begin
                        if Files^.Count = 0 then
                          exit;
                        PF := Files^.At(ScrollBar^.Value);
                        if (PF^.TType <> ttUpDir)
                          and (PF^.Selected xor SelectFlag)
                        then
                          begin
                            PF^.Selected := SelectFlag;
                            if SelectFlag then
                              begin
                                Inc(SelNum);
                                SelectedLen := SelectedLen+PF^.Size;
                                PackedLen := PackedLen+PF^.PSize
                              end
                            else
                              begin
                                Dec(SelNum);
                                SelectedLen := SelectedLen-PF^.Size;
                                PackedLen := PackedLen-PF^.PSize
                              end;
                          end;
                      end;
                  end;
                PosChanged := True;
                if InfoView <> nil then
                  InfoView^.DrawView;
                CED;
                PosChanged := False;
                if (RepeatDelay <> 0) and (ViewEnabled or
                  (Drive^.Flags and psShowLongDesc <> 0) and
                  (Drive^.DriveType < dtArc))
                then
                  NeedLocated := GetSTime;
              end;
        end {case};
      evMouseDown:
        CommandHandle(Event);
    end {case};
  end { TFilePanel.HandleEvent };
{-DataCompBoy-}

end.
