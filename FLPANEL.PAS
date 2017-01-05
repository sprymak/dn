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
  {Cat} {$IFDEF VIRTUALPASCAL} VPSysLow, {$ENDIF} {/Cat}
  Objects, Views, Drivers, Dos, Eraser, FilesCol, HideView, Drives,
  FLPanelX, DNHelp, {$IFNDEF OS2}LFNCol,{$ENDIF} TitleSet, collect;

type
    PSeparator=^TSeparator;
    TSeparator = object(THideView)
     OldX, OldW: AInt;
     constructor Init(R: TRect; AH: Integer);
     procedure HandleEvent(var Event: TEvent); virtual;
     procedure Draw; virtual;
     constructor Load(var S: TStream);
     procedure Store(var S: TStream);
    end;

    PFilePanel = ^TFilePanel;
    TFilePanel = object(TFilePanelRoot)
     procedure Draw; virtual;
     procedure SetState(AState: Word; Enable: Boolean); virtual;
     procedure HandleEvent(var Event: TEvent); virtual;
     function  GetPalette: PPalette; virtual;
     procedure GetFull(var B; P: PFileRec; C, CW: AWord); virtual; {DataCompBoy}
     procedure DrawTop(var B); virtual;
    end;

    PInfoView = ^TInfoView;
    TInfoView = object(THideView)
     Panel: PFilePanel;
     constructor Init(R: TRect);
     procedure Draw; virtual;
     constructor Load(var S: TStream);
     procedure Store(var S: TStream);
     function GetPalette: PPalette; virtual;
     procedure HandleEvent(var Event: TEvent); virtual;
    end;

    PTopView = ^TTopView;
    TTopView = object(THideView)
     Panel: PFilePanel;
     procedure HandleEvent(var Event: TEvent); virtual;
     procedure Draw; virtual;
     constructor Load(var S: TStream);
     function GetPalette: PPalette; virtual;
     procedure Store(var S: TStream);
    end;

    PSpecScroll = ^TSpecScroll;
    TSpecScroll = object(TMyScrollBar)
     procedure HandleEvent(var Event: TEvent); virtual;
    end;

    PDriveLine = ^TDriveLine;
    TDriveLine = object(THideView)
      Panel: PFilePanel;
      DriveLine: String[29];
      ViewLine:  String[60];
      CharDelta: AInt;
{$IFDEF VIRTUALPASCAL}
      LogDrvMap: Longint; {Cat}
{$ENDIF}
      constructor Init(var R: TRect; APanel: PFilePanel);
      procedure MakeDriveLine;
      function GetPalette: PPalette; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
      procedure Draw; virtual;
      constructor Load(var S: TStream);
      procedure Store(var S: TStream);
      procedure Update; virtual; {Cat}
    end;

const
       CPanel = #6#7#8#9#10#32#33#34#35#36#37#38#39#40#44#45#46#47#48; {JO}
       CTopView = #11#12;
       CInfoView  = #25#26#27#28#29#30#31;
       CDriveLine = #41#42#43;

VAR
  ActivePanel: Pointer absolute FlPanelX.ActivePanel;
  CtrlWas: Boolean absolute FlPanelX.CtrlWas;
  DirsToChange: Array [0..9] of PString absolute FlPanelX.DirsToChange;

VAR
  CurrentDirectory: String absolute FLPanelX.CurrentDirectory;

implementation

uses LFN, DnUtil, DNApp, advance, advance1, advance2, advance3, Startup,
     Memory, FileCopy, Messages, Menus, DiskInfo, Dialogs, Commands,
     HistList, Tree, FBB, RStrings, ArcView, CmdLine, Histries, Archiver,
     Gauges, Gauge, FileFind, FlTools, DnIni, XDblWnd
{$IFDEF UUENCODE}
  ,UUCode
{$ELSE}
{$IFDEF  UUDECODE}
  ,UUCode
{$ENDIF}
{$ENDIF}{, Crt}
     ;

const
    chTempDrive = '*';

procedure TSpecScroll.HandleEvent;
begin
 if (Event.What = evKeyDown) and
    ((Event.CharCode in [^E,^S,^D,^X]) or
     (Event.CharCode = #0) and ((ShiftState and 3 <> 0) xor
     (FMSetup.Options and fmoUseArrows = 0) and
     ((Event.KeyCode = kbRight) or (Event.KeyCode = kbLeft) or
      (Event.KeyCode = kbHome) or (Event.KeyCode = kbEnd)
     )))
     then Exit;
 if (Event.What = evMouseDown) then
    QuickSearch := false; {AK155}

 inherited HandleEvent(Event);
end;

constructor TDriveLine.Init;
begin
  inherited Init(R);
  Panel := APanel;
  EventMask := evMouse or evBroadcast;
  MakeDriveLine;
  CharDelta := 1;
{Cat}
  if AutoRefreshDriveLine then
    begin
{$IFDEF VIRTUALPASCAL}
      LogDrvMap := SysGetValidDrives;
{$ENDIF}
      UpdTicks := 55;
      RegisterToBackGround(@Self);
    end;
{/Cat}
end;

constructor TDriveLine.Load(var S: TStream);
begin
  inherited Load(S);
  MakeDriveLine;
  CharDelta := 1;
  GetPeerViewPtr(S, Panel);
{Cat}
  if AutoRefreshDriveLine then
    begin
{$IFDEF VIRTUALPASCAL}
      LogDrvMap := SysGetValidDrives;
{$ENDIF}
      UpdTicks := 55;
      RegisterToBackGround(@Self);
    end;
{/Cat}
end;

procedure TDriveLine.MakeDriveLine;
 var C: Char;
     I: Byte;
begin
  DriveLine := '';
  for C := 'A' to 'Z' do
    if ValidDrive(C) then DriveLine :=  DriveLine+ C;
  DriveLine := DriveLine + chTempDrive;
end;

function TDriveLine.GetPalette;
 const S: String[Length(CDriveLine)] = CDriveLine;
begin
 GetPalette := @S;
end;

procedure TDriveLine.Draw;
  var B: TDrawBuffer;
      M: Byte absolute DriveLine;
      I: Integer;
begin
  I := M*2+3;
  if (Panel^.Size.X >= I) then
   begin
     if (Size.X <> I) then begin GrowTo(I, 1); Exit; end;
     ViewLine := '';
     for I := 1 to Length(DriveLine) do
        ViewLine := ViewLine + ' ' + DriveLine[I];
     ViewLine := '[' + ViewLine + ' ]';
   end else
    begin
      I := 2+M;
      if Panel^.Size.X-2 >= I then
       if (Size.X <> I)
        then begin GrowTo(I, 1); Exit; end
        else ViewLine := '['+DriveLine + ']'
      else
       if Panel^.Size.X <> Size.X+2
        then begin GrowTo(Panel^.Size.X-2, 1); CharDelta:=1; Exit; end
        else begin
              ViewLine := Copy(DriveLine, CharDelta, Size.X-2);
              if Length(ViewLine) < Size.X-2 then
               ViewLine := ViewLine + Copy(DriveLine, 1, Size.X-2-Length(ViewLine));
              ViewLine := '{' + ViewLine + '}';
             end;
    end;
  if (Panel^.DirectoryName[2] = ':') or (Panel^.Drive^.DriveType = dtTemp) then
    begin
     if (Panel^.Drive^.DriveType = dtTemp) and (Panel^.DirectoryName = cTEMP_)
      then I := PosChar(chTempDrive, ViewLine)
      else I := PosChar(UpStrg(Panel^.DirectoryName)[1], ViewLine);
     if I > 0
      then MoveCStr(B, Copy(ViewLine, 1, I-1)+'~'+
                       ViewLine[I]+'~'+Copy(ViewLine, I+1, MaxStringLength),
                    GetColor($0301))
{Cat: по-моему, это бред}
(*
      else begin
       if (Panel^.Drive^.DriveType = dtTemp) and (Panel^.DirectoryName = cTEMP_)
        then I := PosChar(chTempDrive, DriveLine)
        else I := PosChar(UpStrg(Panel^.DirectoryName)[1], DriveLine);
       if I = 0 then begin
        MakeDriveLine;
       {Draw;} Exit;
       end else MoveCStr(B, ViewLine, GetColor($0301))
      end;
    end else MoveCStr(B, ViewLine, GetColor($0301));
*)
     else MoveCStr(B, ViewLine, GetColor($0301));
    end else MoveCStr(B, ViewLine, GetColor($0301));
{/Cat}
  WordRec(B[0]).Hi := GetColor(2);
  WordRec(B[Size.X-1]).Hi := GetColor(2);
  WriteLine(0,0,Size.X,1,B);
end;

procedure TDriveLine.HandleEvent;
  var P: TPoint;
begin
  inherited HandleEvent(Event);
  case Event.What of
    evBroadcast: case Event.Command of
                   cmDropped: if MouseInView(PCopyRec(Event.InfoPtr)^.Where) then
                                begin
                                  Event.What := evNothing;
                                  ClrIO;
                                  MakeLocal(PCopyRec(Event.InfoPtr)^.Where, P);
                                  if ViewLine[P.X+1]=' ' then dec(P.X);
                                  case ViewLine[P.X+1] of
                                    chTempDrive: CopyDirName := cTEMP_;
                                    'A'..'Z': CopyDirName := ViewLine[P.X+1]+':';
                                    else Exit;
                                  end;
                                  SkipCopyDialog := Confirms and cfMouseConfirm = 0;
                                  if ReflectCopyDirection
                                   then RevertBar:=(Message(DeskTop, evBroadCast, cmIsRightPanel, Panel) <> Nil)
                                   else RevertBar:=false;
                                  Message(PCopyRec(Event.InfoPtr)^.Owner,
                                          evBroadcast, cmCopyCollection,
                                          PCopyRec(Event.InfoPtr)^.FC);
                                  SkipCopyDialog := Off;
                                end;
                 end;
    evMouseDown: begin
                   ClrIO;
                   MakeLocal(Event.Where, P);
                   if ViewLine[P.X+1]=' ' then dec(P.X);
                   if Event.Double then begin
                    case ViewLine[P.X+1] of
                      chTempDrive: begin
                                     FreeStr := cTEMP_;
                                     Message(Panel, evCommand, cmChangeDirectory, @FreeStr);
                                   end;
                      'A'..'Z': begin
                                 FreeStr := ViewLine[P.X+1]+':\';
                                  Message(Panel, evCommand, cmChangeDirectory, @FreeStr);
                                end;
                      '[': Message(Owner, evCommand, cmHideLeft, nil);
                      ']': Message(Owner, evCommand, cmHideRight, nil);
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
                    end;
                   end else
                   if ViewLine[P.X+1]=Panel^.DirectoryName[1]
                    then Message(Panel, evCommand, cmRereadDir, @Panel^.DirectoryName)
                    else
                     case ViewLine[P.X+1] of
                       chTempDrive: begin
                                     FreeStr := cTEMP_;
                                     Message(Panel, evCommand, cmChangeDrv, @FreeStr);
                                    end;
                       'A'..'Z': begin
                                   FreeStr := ViewLine[P.X+1]+':';
                                   Message(Panel, evCommand, cmChangeDrv, @FreeStr);
                                 end;
                       '[': Message(Owner, evCommand, cmHideLeft, nil);
                       ']': Message(Owner, evCommand, cmHideRight, nil);
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
                     end;
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
  end;
end;

procedure TDriveLine.Store(var S: TStream);
begin
  inherited Store(S);
  PutPeerViewPtr(S, Panel);
end;

{Cat}
procedure TDriveLine.Update;
{$IFDEF VIRTUALPASCAL}
var
  newLogDrvMap: Longint;
begin
  newLogDrvMap := SysGetValidDrives;
  if newLogDrvMap <> LogDrvMap then
    begin
      LogDrvMap := newLogDrvMap;
      MakeDriveLine;
      DrawView;
    end;
end;
{$ELSE}
var
  oldDriveLine: String;
begin
  oldDriveLine := DriveLine;
  MakeDriveLine;
  if oldDriveLine <> DriveLine then
    DrawView;
end;
{$ENDIF}
{/Cat}

{                                 TFilePanel                                 }
{----------------------------------------------------------------------------}
function TFilePanel.GetPalette;
 const S: String[Length(CPanel)] = CPanel;
begin
 GetPalette := @S;
end;

var Idx, CurPos, I, J: LongInt;
    C, C1, C2, C3, C4, C5, C6, C7: Byte;
    B: Array[0..300] of AWord;
    B1: Array[0..200] of record C: Char; A: Byte; end absolute B;

procedure TFilePanel.GetFull;
begin
  Drive^.GetFull(B, P, C, CW); {DataCompBoy}
end;

procedure TFilePanel.DrawTop;
 var S: String;
     I,J: Integer;
     C: Word;
begin
 Drive^.MakeTop(S);
 I := 0; C := GetColor($0206);
 J := CStrLen(S);
 While I < Size.X do
  begin
   MoveCStr(TWordArray(B)[I], S, C);
   Inc(I, J);
  end;
end;

        {-DataCompBoy-}
procedure TFilePanel.SetState;

  procedure MakeChange;
   var Event: TEvent;
       {A: Boolean;}
  begin
     CurrentDirectory := DirectoryName;
     ClrIO;
     NeedAbort := True;
     lChDir(CurrentDirectory);
     {A := Abort;}
     ActivePanel := @Self;

     DriveState := dsActive ;

     if IOresult<>0 then begin
         DriveState := DriveState and dsInvalid ;
         exit;
     end;
     DriveState := dsActive ;

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
  end;

var
  DoDraw: Boolean;

begin
  inherited SetState(AState, Enable);
  DoDraw := OFF;
  If QuickSearch and ( AState and ( sfFocused + sfActive + sfVisible + sfSelected ) <> 0 ) and not Enable then begin
    DoDraw := True;
    QuickSearch := OFF
  end;
  if (AState and sfActive <> 0) then
    if not Enable then begin
      DisableCommands(PanelCommands);
      if not GetState(sfActive + sfSelected) and
         (ScrollBar <> nil) and ScrollBar^.GetState(sfVisible) then
           ScrollBar^.Hide;
      DoDraw := True;
    end else EnableCommands(PanelCommands); ;
  if (AState and sfFocused and State <> 0) then
    if Enable then begin
      DoDraw := True;
      if ScrollBar <> nil then begin
        ScrollBar^.Show;
        ScrollBar^.Options := ScrollBar^.Options or ofPostProcess;
      end;
      if InfoView <> nil then InfoView^.DrawView;
      if DirView <> nil then DirView^.DrawView;
      if (Drive^.DriveType = dtDisk) then begin
        ActivePanel := @Self;
        AddToDirectoryHistory(DirectoryName,integer(dtDisk));
        if UpStrg(CurrentDirectory) <> UpStrg(DirectoryName)
          then MakeChange;
      end else
      if Drive^.DriveType = dtArc
        then Drive^.lChDir(#0);
      EnableCommands(PanelCommands);
    end;
  if GetState(sfFocused) then begin
    if AState and sfFocused <> 0 then DrawView;
    EnableCommands(PanelCommands);
    if (ScrollBar <> nil) and not ScrollBar^.GetState(sfVisible) then begin
      ScrollBar^.Show;
      ScrollBar^.Options := ScrollBar^.Options or ofPostProcess;
    end;
  end;
  if AState and (sfSelected+sfActive) <> 0 then
    if GetState(sfSelected+sfActive) then begin
      if (Drive^.DriveType = dtDisk)
        then MakeChange
        else
      if Drive^.DriveType = dtArc
        then Drive^.lChDir(#0);
      DoDraw := True;
      EnableCommands(PanelCommands)
    end else begin
      if ScrollBar <> nil then begin
        ScrollBar^.Hide;
        ScrollBar^.Options := ScrollBar^.Options and (not ofPostProcess);
      end;
      DoDraw := True;
      if InfoView <> nil then InfoView^.DrawView;
      if DirView <> nil then DirView^.DrawView;
    end;
  If DoDraw then begin
    PosChanged := OFF;
    DrawView;
  end;
  if GetState(sfFocused) then
   begin
    SetTitle(DirectoryName);
    FreeByte:=Drive^.Param;
   end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TFilePanel.Draw;
 label 1;
 var P: PFileRec;
     CW: AWord;
     CS: AWord;
     PgS: AWord;
     S, S1: String;
     HLC: Array[0..ttCust10] of Byte; {JO}
     PB: ^Byte;
     TagC: Char;

 procedure DrawAtIdx;
  var P: PFileRec;
      CC: Byte;
      JJ: LongInt;
 begin
   JJ := J*LineLength;
   if (Files <> nil) and (Idx < Files^.Count) then
    begin
      P := Files^.At(Idx);
      CC := HLC[P^.TType];
      if (Idx = CurPos) then begin LastCurPos.X := JJ; LastCurPos.Y := I+1 end;
      if Idx=CurPos then
       If p^.Selected then begin C:=C5; CC:=C5 end else C:=C4
      else if P^.Selected then begin C:=CC; CC:=C3 end else C:=CC;
      if QuickSearch then if (Idx = CurPos) then
        begin
         {$IFNDEF OS2}
         if Drive^.Flags and psShowLongName <> 0 then
           begin
             if Drive^.LFNLen < 250 then
                 SetCursor(JJ+GetCursorPos(SearchParam, GetLFN(P^.LFN),
                                          Drive^.LFNLen, Drive^.EXTLen) - 1, I)
               else
                 SetCursor(JJ+GetCursorPos(SearchParam, GetLFN(P^.LFN),
                                          Drive^.LFNLen, Drive^.EXTLen) + Drive^.CalcLengthWithoutName - 1, I)

           end
         else SetCursor(JJ+GetCursorPos(SearchParam, MakeFileName(P^.Name),
                                         Drive^.LFNLen, Drive^.EXTLen) - 1, I)
         {$ELSE}

         if Drive^.LFNLen < 250 then
             SetCursor(JJ+GetCursorPos(SearchParam, P^.Name,
                                    Drive^.LFNLen, Drive^.EXTLen) - 1, I)
           else
             SetCursor(JJ+GetCursorPos(SearchParam, P^.Name,
                                    Drive^.LFNLen, Drive^.EXTLen) + Drive^.CalcLengthWithoutName - 1, I)
         {$ENDIF}
              ;
         ShowCursor;
         NormalCursor
        end;
      MoveChar(B[JJ], ' ', C, LineLength);
      if Idx=CurPos then GetFull(B[JJ], P,  C shl 8+C, CS and $00FF + C shl 8)
                    else GetFull(B[JJ], P, CC shl 8+C, CS);
    end else
     begin
      MoveChar(B[JJ], ' ', C1, LineLength);
      Drive^.GetEmpty(B[JJ], CS);
     end;
 end;

begin
 if DrawDisableLvl > 0 then Exit;
 if DrawDisableLvl < 0 then DrawDisableLvl := 0;
 if Loaded then RereadDir;
 LineLength := Drive^.CalcLength;

 if (DriveState and dsInvalid>0) then
    begin
      C1 := GetColor(1);  { Normal }
     for I := 0 to Pred(Owner^.Size.Y) do
      begin
         MoveChar(B, ' ', C1, Size.X);
         WriteLine(0, I, Owner^.Size.X, 1, B[0]);
      end;
     Exit ;
   end;

 if Size.X > LineLength - 1 then DeltaX := 0;
 if UpStrg(OldDirectory) <> UpStrg(DirectoryName) then
  begin
   if (UpStrg(OldDirectory[1]) <> UpStrg(DirectoryName[1])) and (OldDirectory <> '')
      then ScrollBar^.SetValue(0);
   PosChanged := Off;
   DecDrawDisabled; OldDirectory := DirectoryName;
  end;
 C1 := GetColor(1);  { Normal }
 C2 := GetColor(2);  { Separator }
 C3 := GetColor(3);  { Selected }
 C4 := C1;
 C5 := C3;
 HLC[0] := C1;
 if FMSetup.TagChar <> '' then TagC := FMSetup.TagChar[1]
                          else TagC := #251;
 if PanelFlags and fmiHiliteFiles <> 0 then
      for I := 1 to ttCust10 do HLC[I] := GetColor(6+I) {JO}
    else for I := 1 to ttCust10 do HLC[I] := C1;        {JO}
 CS := 179+C2 shl 8;
 CW :=  32+C2 shl 8;
 PgS := (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0))*((Size.X+1) div LineLength);
 if PgS = 0 then PgS := (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0));
 ScrollBar^.PgStep := PgS;
 if Delta < 0 then Delta := 0;
 if {GetState(sfSelected) and} (Files^.Count > 0)
  then
   begin
    CurPos := ScrollBar^.Value;
    if CurPos < Delta then Delta := CurPos;
    if CurPos >= Delta + PgS then Delta := CurPos - PgS + 1;
   end else CurPos := -1;
 if GetState(sfFocused) then
  begin
    C4 := GetColor(4);  { Normal cursor }
    C5 := GetColor(5);  { Selected cursor }
  end else if CurPos >= 0 then
   begin
     P := Files^.At(CurPos);
     if PanelFlags and fmiHiliteFiles <> 0 then C4 := HLC[P^.TType];
   end;
 if not QuickSearch then HideCursor;
 if PosChanged and (OldDelta = Delta) then
  begin
   if CurPos <> OldPos then
    begin
     for I := Byte(FMSetup.Show and fmsColumnTitles <> 0) to Size.Y - 1 do
      for J := 0 to Size.X div LineLength do
       begin
        Idx := I - Byte(FMSetup.Show and fmsColumnTitles <> 0) +
               J * (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0)) + Delta;
        if (Idx = CurPos) or (Idx = OldPos) then
         begin
          MoveChar(B, ' ', C1, Size.X);
          DrawAtIdx;
          Idx := (J+1)*LineLength;
          if Idx < Size.X then B[Idx - 1] := CS else B[Idx - 1] := CW;
          Idx := J*LineLength;
          WriteLine(Idx, I, LineLength-1, 1, B[Idx+DeltaX]);
         end;
       end;
    end else Goto 1;
   PosChanged := Off;
   OldDelta := Delta; OldPos := CurPos;
   Exit;
  end;
1:
 OldDelta := Delta; OldPos := CurPos;
 PosChanged := Off;
 if (FMSetup.Show and fmsColumnTitles <> 0) then
  begin
    DrawTop(B);
    if WordRec(B[Size.X - 1]).Lo = 179 then WordRec(B[Size.X - 1]).Lo := 32;
    WriteLine(0, 0, Size.X, 1, B[DeltaX]);
  end;
 for I := Byte(FMSetup.Show and fmsColumnTitles <> 0) to Size.Y - 1 do
  begin
   MoveChar(B, ' ', C1, Size.X);
   for J := 0 to Size.X div LineLength do
    begin
     Idx := I - Byte(FMSetup.Show and fmsColumnTitles <> 0) +
            J*(Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0)) + Delta;
     DrawAtIdx;
     Idx := (J+1)*LineLength;
     if Idx < Size.X then B[Idx - 1] := CS else B[Idx - 1] := CW;
    end;
   WriteLine(0, I, Size.X, 1, B[DeltaX]);
  end;
  if GetState(sfFocused) then SetTitle(DirectoryName);
end;
        {-DataCompBoy-}

{                                 TInfoView                                  }
{----------------------------------------------------------------------------}
constructor TInfoView.Init;
begin
 inherited Init(R);
 EventMask := evMouse;
end;

constructor TInfoView.Load;
begin
 inherited Load(S);
 GetPeerViewPtr(S, Panel);
end;

procedure TInfoView.Store;
begin
 inherited Store(S);
 PutPeerViewPtr(S, Panel);
end;

procedure TInfoView.HandleEvent;
 var P: TPoint;
     Y: Integer;
     Mover: PView;
     S: String;
     FC: PFilesCollection;
     C: TCopyRec;

 procedure CE; begin ClearEvent(Event) end;

 procedure DragCurrent;
 begin
  with Panel^ do
   begin
   {$IFNDEF OS2}
    S:=Cut(GetLFN(PFileRec(Files^.At(ScrollBar^.Value))^.LFN), 20); {DataCompBoy}
    if S[1] = '.' then Exit;
   {$ELSE}
    S:=Cut(PFileRec(Files^.At(ScrollBar^.Value))^.Name, 20); {DataCompBoy}
    if S = '..' then Exit;
   {$ENDIF}
   end;
  New(FC, Init(1, 100));
  FC^.Insert(CopyFileRec(Panel^.Files^.At(Panel^.ScrollBar^.Value)));
  if P.X < Length(S) then Dec(P.X);
  MakeGlobal(P, P);
  DragMover(@P, S, FC, @C);
  CE;
 end;

 procedure DragSelected;
  var I: LongInt;
      PF: PFileRec; {DataCompBoy}
 begin
  if Panel^.SelNum = 0 then Exit;
  New(FC, Init(Panel^.SelNum, 100));
  for I := 1 to Panel^.Files^.Count do
   begin
    PF := Panel^.Files^.At(I-1); {DataCompBoy}
    if PF^.Selected then FC^.Insert(CopyFileRec(PF)); {DataCompBoy}
   end;
  MakeGlobal(P, P);
  DragMover(@P, ItoS(Panel^.SelNum)+GetString(dlSelectedFiles), FC, @C);
  CE;
 end;

 procedure DragTotals;
  var I: LongInt;
      PF: PFileRec; {DataCompBoy}
 begin
  New(FC, Init(Panel^.SelNum, 100));
  for I := 1 to Panel^.Files^.Count do
   begin
    PF := Panel^.Files^.At(I-1); {DataCompBoy}
    if {$IFNDEF OS2}
         {$IFDEF OS_DOS}
         PF^.Name[1] <> '.'
         {$ELSE}
         PF^.LFN <> '..'
         {$ENDIF}
       {$ELSE}
         PF^.Name <> '..'
       {$ENDIF}
           then FC^.Insert(CopyFileRec(PF)); {DataCompBoy}
   end;
  MakeGlobal(P, P);
  DragMover(@P, ItoS(Panel^.Files^.Count)+' '+GetString(dlDIFiles), FC, @C);
  CE;
 end;

begin
 inherited HandleEvent(Event);
 if Event.What and (evMouseDown + evMouseAuto) <> 0 then
  begin
   if Panel^.Files^.Count = 0 then Exit;
   C.Owner := Panel;
   MakeLocal(Event.Where, P); Y := 0;
   if FMSetup.Show and fmsDivider <> 0 then
    begin if Y = P.Y then
            begin
             if Panel^.GetState(sfActive+sfSelected) then
              begin
               with Panel^ do
                begin
                 if Files^.Count = 0 then Exit;
                 MSelect := Event.Buttons and mbRightButton <> 0;
                 if MSelect then
                  begin
                    SelectFlag := not PFileRec(Files^.At(ScrollBar^.Value))^.Selected;
                    Message(Panel, evKeyDown, kbIns, nil);
                  end;
                end;
               RepeatDelay := 0;
               repeat
                Message(Owner, evKeyDown, kbDown, nil);
               until not MouseEvent(Event, evMouseMove + evMouseAuto);
               RepeatDelay := 2; Panel^.MSelect := Off;
              end;
             Exit
            end;
          Inc(Y);
    end;
   if Panel^.PanelFlags and fmiCurrent <> 0 then
    begin if Y = P.Y then begin DragCurrent; Exit end; Inc(Y);end;
   if Panel^.PanelFlags and fmiSelected <> 0 then
    begin if Y = P.Y then begin DragSelected; Exit end; Inc(Y);end;
   if Panel^.PanelFlags and fmiTotals <> 0 then
    begin if Y = P.Y then begin DragTotals; Exit end; Inc(Y);end;
   CE;
  end;
end;

        {-DataCompBoy-}
procedure TInfoView.Draw;
 var B: TDrawBuffer;
     B1: Array[0..127] of record C: Char; A: Byte; end absolute B;
     I, J: LongInt;
     C1, C2, C3: Word;
     PF, PPF: PFileRec;
     S, S1, S2, S3: String;
     Y: LongInt;
     CurFile: PFileRec;
     L: longint;
     ShowSelected, ShowFullName: boolean;
     ShowFree, DescriptionInBottom: boolean;


 var FR: TFileRec;
     P: PFileRec;

 { построение строки информации об отмеченных файлах }
 function SelString: string;
  begin
   with Panel^ do
    begin
     Result := '~'+FStr(SelectedLen);
     if (Drive^.DriveType = dtArc) and (SelectedLen <> PackedLen) then
       Result := Result + '('+FStr(PackedLen)+')';

     Result := Result + GetString(dlBytesIn)+
         +ItoS(SelNum)+'~'+ GetString(dlSelectedfiles);
    end;
  end;

 { построение строки текущей маски быстрого поиска }
 function QuickSearchString: string;
  var
    S: string;
  begin
   S := Panel^.SearchParam.Mask;
   if QuickSearchType=1 then
     SetLength(S, Length(S)-1);
   Result := GetString(dlFileSearch);
   if Length(S)+Length(Result)>Size.X then
     Result := Result + #17 + '~' +
         Copy(S, Length(S) - Size.X - Length(S1) - 1, 255) + '~'
   else
     Result := Result + '~' + S + '~';
  end;


begin
 if Panel^.DrawDisableLvl > 0 then Exit;
 ShowSelected := (Panel^.PanelFlags and fmiSelected)  <> 0;
 ShowFullName := (Panel^.PanelFlags and fmiFullNameInBottom)  <> 0;
 C2 := Panel^.GetColor(2); I := Size.X;
 C1 := Panel^.LineLength;
 if I < C1 then I := C1;
 Dec(C1);
 Y := 0;

{ линия разделителя }
 if FMSetup.Show and fmsDivider <> 0 then
  begin
   MoveChar(B, #196, C2, Size.X+Panel^.DeltaX);
   I := 0; C2 := (C2 shl 8) or 193;
   While I < Size.X+Panel^.DeltaX do
    begin
     Panel^.Drive^.GetEmpty(B[I], C2);
     Inc(I, Panel^.LineLength);
     If I = Size.X+Panel^.DeltaX then B[I-1]:=(C2 and $FF00)+196;
    end;
   WriteLine(0,Y,Size.X,1,B[Panel^.DeltaX]);

   S := '';
   {if not ShowSelected and not ShowFullName then}
    begin { строки информации о выделенных файлах нет, поэтому
        кое-какую информацию выводим поверх линии разделителя }
     if not ShowSelected and (Panel^.SelNum > 0) then
      begin { информация о выделенных файлах }
       C3 := GetColor($0302);
       {MoveChar(B, '─', C3, Size.X);}
       S := SelString;
       I := (Size.X - CStrLen(S)) div 2; if I < 0 then I := 0;
      end
     else if not ShowSelected and not ShowFullName and ShowFileMask and {Panel^.}QuickSearch then
      begin { маска быстрого поиска поверх }
       C3 := GetColor($0302);
       S := QuickSearchString;
       I := 0;
      end;
    end;
   if S <> '' then
     begin
     MoveCStr(B[I], S, C3);
     WriteLine(0,Y,Size.X,1,B);
     end;
   Inc(Y);
  end;

{ информация о текущем файле }
 C1 := GetColor(1);
 if Panel^.PanelFlags and fmiCurrent <> 0 then
  begin
   MoveChar(B, ' ', C1, Size.X);
   if Panel^.ScrollBar^.Value < Panel^.Files^.Count then
    begin
      PF := Panel^.Files^.At(Panel^.ScrollBar^.Value);
      Panel^.Drive^.GetDown(B, C1, PF);
    end;
   WriteLine(0,Y,Size.X,1,B);
   Inc(Y);
   if (Panel^.Drive^.DriveType = dtFind) or (Panel^.Drive^.DriveType = dtTemp) then
    begin
     MoveChar(B, ' ', C1, Size.X);
     if (Panel^.ScrollBar^.Value < Panel^.Files^.Count) and
        (PF^.Owner <> nil) then MoveStr(B, {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(PF^.Owner^), C1);
     WriteLine(0,Y,Size.X,1,B);
     Inc(Y);
    end else
    if (Panel^.Drive^.DriveType = dtArc) then
     begin
      MoveChar(B, ' ', C1, Size.X);
      C3 := GetColor($0302);
      if Panel^.ScrollBar^.Value < Panel^.Files^.Count then begin
       if PF^.Size = 0 then S := '100%' else S := ZtoS(Percent(PF^.Size, PF^.PSize))+'%';
       MoveCStr(B, GetString(dlArcPSize) + ' ' + AddSpace(FStr(PF^.PSize),14) +
                  GetString(dlArcRatio) + S, C3);
      end;
      WriteLine(0,Y,Size.X,1,B);
      Inc(Y);
     end;
  end;

{ Выделенные файлы, маска панели, маска быстрого поиска, длинное имя }
 if ShowSelected or ShowFullName then
  begin
    S := '';
    I := -1;
    C3 := GetColor($0302);
    MoveChar(B, ' ', C3, Size.X);
    if ShowSelected and (Panel^.SelNum <> 0) then
      S := SelString
    else if ShowFileMask and (x_x<>Panel^.FileMask) and ('*'<>Panel^.FileMask) then {-$VIV 19.05.99--}
     begin
      S := fReplace('*.', '', Panel^.FileMask);
      S := fReplace(' ', '', S);
      if Copy(S, 1, 1) = ';' then Delete(S, 1, 1);
      S := GetString(dlFileMask) + S;
     end
    else if ShowFileMask and (Panel=ActivePanel) and ({Panel^.}QuickSearch) then
     begin
      S := QuickSearchString;
      I := 0;
     end
    else if ShowFullName and (Panel^.ScrollBar^.Value < Panel^.Files^.Count)then
     begin
      P:=PFileRec(Panel^.Files^.At(Panel^.ScrollBar^.Value));
      if P<>nil then begin
{$IFNDEF OS2}
        S2 := MakeFileName(P^.Name);
{$ELSE}
        S2 := P^.Name;
{$ENDIF}
        if (S2 <> '..') and (S2 <> '.') then
         begin
{$IFDEF OS2}
          S3 := S2;
          if length(S2) > 14
{$ELSE}
          S3 := GetLFN(P^.LFN); {DataCompBoy}
          if (UpStrg(S2) <> UpStrg(S3)) or (length(S2) > 14)
{$ENDIF}
          then S := FormatLongName(GetName(S3),Size.X,0,
          flnPreferName+flnHighlight+flnHandleTildes+flnUseCutChar,
          nfmNull,nil);
         end;
       end
     end
    else if ShowSelected then
      S := GetString(dlNoFilesSelected);
    if I < 0 then I := (Size.X - CStrLen(S)) div 2;
    if I < 0 then I := 0;
    MoveCStr(B[I], S, C3);
    WriteLine(0,Y,Size.X,1,B);
    Inc(Y);
  end;

 if Panel^.PanelFlags and fmiTotals <> 0 then
  begin
    C3 := GetColor($0504);
    MoveChar(B, ' ', C3, Size.X);
    I := (Size.X - CStrLen(Panel^.TotalInfo)) div 2; if I < 0 then I := 0;
    MoveCStr(B[I], Panel^.TotalInfo, C3);
    WriteLine(0,Y,Size.X,1,B);
    Inc(Y);
  end;

{ Свободное место, описание текущего файла }
 ShowFree := Panel^.PanelFlags and fmiFree <> 0;
 DescriptionInBottom := Panel^.PanelFlags and fmiDescriptionInBottom <> 0;
 if DescriptionInBottom or ShowFree then
  begin
  {S1: свободное место (цифры); S2: описание; S3: текст свободного места;
   I: позиция начала выдачи S1 с учетом центрирования, когда нет описания;
   L - ширина поля S1 вместе с двумя пробелами }
    L := 0; S1 := ''; S2 := ''; S3 := '';
    if ShowFree then
      begin
      S1 := Panel^.FreeSpace;
      L := CStrLen(S1)+2;
      S3 := GetString(dlDIFreeDisk) +Copy(Panel^.Drive^.CurDir, 1, 2);
      System.delete(S3, 1, 2); { убрать ненужные '~ '}
      I := (Size.X - (L-2+CStrLen(S3))) div 2 - 1;
      if I < 0 then I := 0;
        { центрируем обычный показ свободного места }
      end;
    if DescriptionInBottom then
    begin
      CurFile := PFileRec(Panel^.Files^.At(Panel^.ScrollBar^.Value));
      if (CurFile <> nil) and
         (CurFile^.DIZ <> nil) and
         (CurFile^.DIZ^.DIZ <> nil)
      then
      begin
        S2 := CurFile^.DIZ^.DIZ^;
        if Panel^.Drive^.Flags and psShowDescript <> 0 then
          begin
          { выводим в подвале то, что не поместилось в панели }
  {$IFNDEF OS2} if Panel^.Drive^.Flags and psShowLongName <> 0 then {$ENDIF}
                  J:=Panel^.Drive^.LFNLen
              {$IFNDEF OS2} else J:=12{$ENDIF};
          J := Size.X - J + Panel^.DeltaX - 1;
          system.delete(S2, 1,  J);
          end;
        if length(S2) + L > Size.X then
          begin
          SetLength(S2, Size.X - L);
          S2[Size.X - L] := FMSetup.RestChar[1];
          end;
        if S2 <> '' then
          begin
          I := 0; { свободное место с описанием не центрируем }
          for J := 1 to length(S2) do if S2[J]=#20 then S2[J] := ' ';
            { #20 - это разделитель строк многострочного описания }
          end;
      end;
    end;

    C3 := GetColor($0706);
    MoveChar(B, ' ', C3, Size.X);
    MoveCStr(B[I], S1, C3);
    if S2 <> '' then
      begin
      C3 := GetColor($301);
      MoveStr(B[L], S2, C3);
      end
    else
      MoveCStr(B[I+L], S3, C3);
    WriteLine(0,Y,Size.X,1,B);
    Inc(Y);
  end;
end;
        {-DataCompBoy-}

function TInfoView.GetPalette;
 const S: String[Length(CInfoView)] = CInfoView;
begin
 GetPalette := @S;
end;

{ ---------------------------- TTopView ------------------------------ }

constructor TTopView.Load;
begin
 inherited Load(S);
 GetPeerViewPtr(S, Panel);
end;

procedure TTopView.Store;
begin
 inherited Store(S);
 PutPeerViewPtr(S, Panel);
end;

function TTopView.GetPalette;
 const S: String[Length(CTopView)] = CTopView;
begin
 GetPalette := @S;
end;

procedure TTopView.Draw;
 var C: Word;
     B: TDrawBuffer;
     I: LongInt;
     S: String;
     R: TRect;
begin
 C := GetColor(1); if not Panel^.GetState(sfSelected) then C := GetColor(2);
 MoveChar(B, ' ', C, Size.X);
 {S := Panel^.DirectoryName;
 if Length(S) > Panel^.Size.X - 10  then
  begin
   Delete(S, PosChar(':', S) + 2, Length(S) - Panel^.Size.X + 13);
   Insert('...', S, PosChar(':', S) + 2);
  end;}
  (*X-Man*)
   S := Cut({$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(Panel^.DirectoryName), Panel^.Size.X-10);
 if Length(S) + 2 <> Size.X then
  if Length(S) < Panel^.Size.X - 2 then
  begin
   R.A := Panel^.Origin; R.B.Y := R.A.Y; Dec(R.A.Y);
   Inc(R.A.X, (Panel^.Size.X - Length(S) - 2) div 2);
   R.B.X := R.A.X + Length(S) + 2;
   Locate(R); Exit;
  end else GrowTo(0, 1);
 if Length(S) > Size.X then I := 0 else I := (Size.X - Length(S)) div 2;
 MoveStr(B[I], S, C);
 WriteLine(0, 0, Size.X, Size.Y, B);
end;

procedure TTopView.HandleEvent;
 var S: String;
     I: LongInt;
     P: TPoint;
begin
 inherited HandleEvent(Event);
 case Event.What of
  evMouseDown: begin
                MakeLocal(Event.Where, P);
                S := Cut({$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(Panel^.DirectoryName), Panel^.Size.X-10);
                if Length(S) > Size.X then I := 0 else I := PosChar(':', S);
                repeat until not MouseEvent(Event, evMouseAuto + evMouseMove);
                ClearEvent(Event);
                if (P.X <= I)
                 then Message(Panel, evCommand, cmChangeDrive, nil)
                 else Message(Panel, evCommand, cmChangeDir, nil);
               end;
 end;
end;

{ --------------------------- TSeparator ----------------------------- }

constructor TSeparator.Load;
begin
 inherited Load(S);
 S.Read(OldX, 4);
end;

procedure TSeparator.Store;
begin
 inherited Store(S);
 S.Write(OldX, 4);
end;

constructor TSeparator.Init;
begin
 inherited Init(R);
 OldX := Origin.X+1;
 OldW := AH;
 EventMask := $FFFF;
end;

procedure TSeparator.HandleEvent;
 var P: TPoint;
     R: TRect;
     RD: Integer;
     B: Byte;
begin
 inherited HandleEvent(Event);
 case Event.What of
  evMouseDown: begin
                MakeLocal(Event.Where, P); B := P.X;
                RD := RepeatDelay; RepeatDelay := 0;
                repeat
                 Owner^.MakeLocal(Event.Where, P);
                 if (P.X >= 1) and (P.X < Owner^.Size.X-2) then
                  begin
                   OldX := P.X + 1 - B; OldW := Owner^.Size.X;
                   R.A := Owner^.Origin;
                   R.B.X := Owner^.Origin.X + Owner^.Size.X;
                   R.B.Y := Owner^.Origin.Y + Owner^.Size.Y;
                   Owner^.ChangeBounds(R);
                  end;
                until not MouseEvent(Event, evMouseAuto + evMouseMove);
                RepeatDelay := RD;
                ClearEvent(Event);
               end;
 end;
end;

procedure TSeparator.Draw;
 var B: Array[0..128] of record C: Char; B: Byte; end;
     C: Word;
     Ch: Char;
begin
 RK:=Owner^.GetColor(2);
 if Owner^.GetState(sfActive) then C := RK
                              else C := Owner^.GetColor(1);
 if Owner^.GetState(sfDragging) then C := Owner^.GetColor(3);
 B[0].B := C; B[Size.Y-1].B := C;
 if Owner^.GetState(sfActive) and not Owner^.GetState(sfDragging) then
  begin B[0].C := #187; Ch := #186; B[Size.Y-1].C := #188; end
   else begin B[0].C := #191; Ch := #179; B[Size.Y-1].C := #217; end;
 MoveChar(B[1], Ch, C, Size.Y-2);
 WriteBuf(0,0,1,Size.Y,B);
 if Owner^.GetState(sfActive) and not Owner^.GetState(sfDragging) then
  begin B[0].C := #201; B[Size.Y-1].C := #200; end
   else begin B[0].C := #218; B[Size.Y-1].C := #192; end;
 WriteBuf(1,0,1,Size.Y,B);
end;

{ FilePanel HandleEvent}

        {-DataCompBoy-}
PROCEDURE TFilePanel.HandleEvent;
var
  PF: PFileRec;
  CurPos: LongInt;
  PPC: PCollection;
  MPos: TPoint;
  LastRDelay: Word;
  I, J: LongInt;
  PDr: PDrive;

 procedure CE;begin ClearEvent(Event) end;
 procedure CED;begin ClearEvent(Event); DrawView end;

 procedure CM_CopyUnselect;
  label 1;
  var PF: PFileRec;
      I, OSM: LongInt;
 begin
  if (Files^.Count = 0) or (Event.InfoPtr = nil) then Exit;
  OSM := PFilesCollection(Files)^.SortMode;
  PFilesCollection(Files)^.SortMode := 100;
  for I := 0 to Files^.Count - 1 do
   if Files^.Compare(Files^.At(I), Event.InfoPtr) = 0 then Goto 1; {-$VOL}
  PFilesCollection(Files)^.SortMode := OSM;
  Exit;
 1:
  PFilesCollection(Files)^.SortMode := 100;
  PF := {Event.InfoPtr}Files^.At(I);
  if PF^.Attr and Directory <> 0 then PF^.Size := -1;
  with PF^ do
   if Selected then
   begin
    Selected := Off;
    SelectedLen := SelectedLen - Size;
    PackedLen := PackedLen - PSize;
    Dec(SelNum)
  end;
  DrawView;
  if InfoView <> nil then InfoView^.DrawView;
 end;

 function MaskSearch(B: Byte): boolean;
 var
   I: LongInt;
 begin
   MaskSearch := true;
   I := CurPos + B; If I >= Files^.Count then I := 0;
{$IFNDEF OS2}
   if Drive^.Flags and psShowLongName <> 0 then
{$ENDIF}
    repeat
{$IFNDEF OS2}
     If InQSMask(GetLFN(PFileRec(Files^.At(I))^.LFN), SearchParam.Mask) then
{$ELSE}
     If InQSMask(PFileRec(Files^.At(I))^.Name, SearchParam.Mask) then
{$ENDIF}
     begin
      ScrollBar^.SetValue( I );
      Exit;
     end;
     Inc( I ); If I >= Files^.Count then I := 0;
    until I = CurPos
{$IFNDEF OS2}
   else
    repeat
  {$IFDEF OS_DOS}
     If InQSMask(MakeFileName(PFileRec(Files^.At(I))^.Name), SearchParam.Mask) then begin
  {$ELSE}
     If InQSMask(lfGetShortFileName(GetLFN(PFileRec(Files^.At(I))^.LFN)), SearchParam.Mask) then begin
  {$ENDIF}
      ScrollBar^.SetValue( I );
      Exit;
     end;
     Inc( I ); If I >= Files^.Count then I := 0;
    until I = CurPos
{$ENDIF}
 ;
   MaskSearch := false;
 end;

 procedure DoMakeDirs(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
 begin
   if (PF^.Attr and Directory <> 0) and (PF^.Size < 0) then
      begin PF^.Size := 0; PF^.Attr := PF^.Attr and $3F end;
 end;

 label lbMakeUp, GotoExt, GotoKb, lbMakeDown;

begin
 inherited HandleEvent(Event);
 if Event.What = evNothing then Exit;
 CurPos := ScrollBar^.Value;
 if Files<>nil
  then if Files^.Count > CurPos
       then PF := Files^.At(CurPos)
       else PF := nil
  else PF := nil;
 I := ShiftState;
 if (Event.What = evKeyDown) and (I and 3 <> 0) and
    ((Event.KeyCode = kbCtrlRight) or (Event.KeyCode = kbCtrlLeft) or
     ((Event.KeyCode = kbBack) and not QuickSearch))
      then begin CommandHandle(Event); Exit; end;
 if (Event.What = evKeyDown) and (CommandLine <> nil) then
  if ((I and (kbRightShift+kbLeftShift) <> 0) and
      ((Event.KeyCode = kbDown) or
       (Event.KeyCode = kbUp) or
       ((Event.KeyCode = kbCtrlIns) and (CmdLine.Str<>'')) or
       (Event.KeyCode = kbGrayAst)
      )
     ) or
     (((Event.ScanCode < (kbAlt1 shr 8)) or
       (Event.ScanCode > (kbAlt9 shr 8))
      ) and
      (Event.CharCode = #0) and
      (CmdLine.Str <> '') and
      (((I and 3 <> 0) xor (FMSetup.Options and fmoUseArrows = 0)) and
       ((Event.KeyCode = kbRight) or (Event.KeyCode = kbLeft) or
        (Event.KeyCode = kbHome) or (Event.KeyCode = kbEnd)
       )
      )
     )
     then CommandLine^.HandleEvent(Event);
 if Event.What = evNothing then Exit;
 I := ShiftState2;
 case Event.What of
  evCommand: case Event.Command of
              cmDoSendLocated: SendLocated;
              cmGetName: begin
                  if Drive^.DriveType = dtDisk
                    then PString(Event.InfoPtr)^ := DirectoryName
                      else if ScrollBar^.Value < Files^.Count then
                              PString(Event.InfoPtr)^ := PFileRec(Files^.At(ScrollBar^.Value))^.Owner^;
                  ClearEvent( Event );
                end;
              cmKillUsed: Drive^.KillUse;
              cmClose: CommandEnabling := Off;
              cmCopyUnselect: begin CM_CopyUnselect; CE end;
              cmRecountDirs: begin
                               if Files <> nil then files^.ForEach(@DoMakeDirs); CED;
                             end;
              0..3,5..100:;
                else CommandHandle(Event);
             end;
  evKeyDown: if (((Event.KeyCode = kbDoubleAlt)) and (FMSetup.Quick = pqsAlt)) or
                ((Event.KeyCode = kbDoubleCtrl) and (FMSetup.Quick = pqsCtrl)) or
                ((Event.CharCode >= #32) and (Event.CharCode <= #254) and
                 (Event.CharCode <> '\') and (FMSetup.Quick = pqsCaps) and
                 (I and $40 <> 0)) or
                ((Event.CharCode >= #32) and (Event.CharCode <= #254) and
                 (ShiftState and 3 <> 0) and (ShiftState and 4 = 0) and
                 (not CommandLine^.GetState(sfVisible)) and
                 (InterfaceData.Options and ouiHideCmdLine <> 0)) then
                begin
                CtrlWas := Off;
                InitQuickSearch(SearchParam);
                QuickSearch := not QuickSearch;
                if not QuickSearch then
                  InfoView^.DrawView; {Cat}
                if (Event.CharCode >= #32) and (Event.CharCode <= #254) then
                 begin
                   if not ((ShiftState and 3 <> 0) and ( not CommandLine^.GetState(sfVisible))) then
                    begin
                      ShiftState2 := I and $BF;
                      ShiftState := ShiftState and kbCapsState;
                    end else ShiftState := ShiftState and $FC;
                   DoQuickSearch(SearchParam, Event.KeyCode);
                   if QuickSearch then
                    begin
                     if (FMSetup.Quick = pqsCaps) and (I and kbCapsState <> 0) then begin
                        {$IFNDEF NONBP}
                        Mem[Seg0040:$17] := ShiftState and not kbCapsState;
                        ShiftState := Mem[Seg0040:$17];
                        {$ENDIF}
                        (*
                        {$IFDEF VIRTUALPASCAL}
                        SysTVSetShiftState(ShiftState and not kbCapsState);
                        ShiftState := SysTVGetShiftState;
                        {$ENDIF}
                        *)
                     end;
                     if not MaskSearch(0) then DoQuickSearch(SearchParam, kbBack);
                    end;
                 end;
                DrawView; CE;
               end
             else if QuickSearch and ((Event.CharCode > #31) or
                                      (Event.CharCode = #27) or
                                      (Event.CharCode = #10) or
                                      (Event.KeyCode = kbBack) or
                                      (Event.KeyCode = kbBackUp) or
                                      (Event.KeyCode = kbIns)) then
                   begin
                    CtrlWas := Off;
                    if (Event.KeyCode = kbGrayPlus) or (Event.KeyCode = kbGrayMinus) or
                       (Event.KeyCode = kbGrayAst) or (Event.KeyCode = kbIns) or
                       (Event.KeyCode = kbBackUp) then Goto GotoKb;
                    case Event.CharCode of
                     #27: begin
                           QuickSearch := Off;
                           InitQuickSearch(SearchParam);
                           InfoView^.DrawView;
                           CED;
                           Exit
                          end;
                     #10: MaskSearch(1);
                     '\': begin
                           Event.What := evCommand;
                           Event.Command := cmChangeDir;
                           Event.InfoPtr := nil;
                           PutEvent(Event);
                           Event.What := evKeyDown;
                           for I := 1 to Length(SearchParam.Mask) do
                            if SearchParam.Mask[I]<>'*' then
                            begin
                              Event.CharCode := SearchParam.Mask[I];
                              PutEvent(Event);
                            end;
                           CE;
                           QuickSearch := Off;
                           InfoView^.DrawView; {Cat}
                           Exit;
                          end;
                     else
                      begin
                       DoQuickSearch(SearchParam, Event.KeyCode);
                       if not MaskSearch(0) then DoQuickSearch(SearchParam, kbBack);
                      end;
                     end;
                    InfoView^.DrawView;
                    CED;
                   end else
                   begin
                    CtrlWas := Off;
                    if QuickSearch then
                     begin
                      QuickSearch := Off;
                      InfoView^.DrawView;
                     end;
GotoKB:
                    case Event.KeyCode of
                     kbDel: if ((CmdLine.Str = '') and (FMSetup.Options and fmoDelErase <> 0)) then
                               Message(@Self, evCommand, cmPanelErase, nil);
                     kbShiftDel: if ((CmdLine.Str = '') and (FMSetup.Options and fmoDelErase <> 0)) then
                               Message(@Self, evCommand, cmSingleDel, nil);
                     kbHome: begin CE; DeltaX := 0; OldDelta := -1; ScrollBar^.SetValue(0); DrawView end;
                     kbEnd: begin CE; OldDelta := -1; ScrollBar^.SetValue(Files^.Count-1) end;
                     kbUp, kbDown, kbCtrlUp, kbCtrlDown, kbUpUp, kbDownUp
                          : begin
                               CtrlWas := ShiftState and kbCtrlShift <> 0;
                               if Event.KeyCode = kbCtrlUp   then Event.KeyCode := kbUp;
                               if Event.KeyCode = kbCtrlDown then Event.KeyCode := kbDown;
                               if (Event.KeyCode = kbDownUp)
                               or (Event.KeyCode = kbUpUp)  then begin CE; exit end;
                            end;
                     kbIns,kbSpace: if CurPos < Files^.Count then
                            begin
                             QuickSearch := Off;
                             if (Event.CharCode = ' ') and ((CmdLine.Str <> '') or
                                (FMSetup.Options and fmoSpaceToggle = 0)) then Exit;
                             CE; if Files^.Count = 0 then Exit;
                             PF := Files^.At(CurPos);
                             {$IFNDEF OS2}
                               {$IFDEF OS_DOS}
                             if PF^.Name[1] <> '.' then
                               {$ELSE}
                             if PF^.LFN <> '..' then
                               {$ENDIF}
                             {$ELSE}
                             if PF^.Name <> '..' then
                             {$ENDIF}
                             begin
                              PF^.Selected := not PF^.Selected;
                              SelectedLen := SelectedLen - (1-2*Integer(PF^.Selected))*PF^.Size;
                              PackedLen := PackedLen - (1-2*Integer(PF^.Selected))*PF^.PSize;
                              Dec(SelNum, 1-2*Integer(PF^.Selected));
                             end;
                             ScrollBar^.SetValue(CurPos + 1);
                             if CurPos = ScrollBar^.Value then DrawView;
                             if InfoView <> nil then InfoView^.DrawView;
                            end;
                     kbAltQuote: begin
                                SystemData.Options:=SystemData.Options xor ossShowHidden;
                                ConfigModified:=True;
                                Message(Application, evCommand, cmUpdateConfig, nil)
                            end
                       else CommandHandle(Event);
                    end;
             end;
  evBroadcast: case Event.Command of
                 cmGetCurrentPosFiles,
                 cmFindForced,
                 cmInsertDrive,
                 cmUnarchive,
                 cmCopyCollection,
                 cmDropped: CommandHandle(Event);

                 cmScrollBarChanged: if ScrollBar = Event.InfoPtr then begin
                                      if MSelect then
                                        begin
                                         CE;
                                         if Files<>nil then begin
                                          if Files^.Count = 0 then Exit;
                                          PF := Files^.At(ScrollBar^.Value);
                                          if {$IFNDEF OS2}
                                               {$IFDEF OS_DOS}
                                              (PF^.Name[1] <> '.')
                                               {$ELSE}
                                              (PF^.LFN <> '..')
                                               {$ENDIF}
                                             {$ELSE}
                                              (PF^.Name <> '..')
                                             {$ENDIF}
                                                     and (PF^.Selected xor SelectFlag) then
                                           begin
                                            PF^.Selected := SelectFlag;
                                            if SelectFlag then begin Inc(SelNum); SelectedLen := SelectedLen + PF^.Size;
                                                                     PackedLen := PackedLen + PF^.PSize end
                                                          else begin Dec(SelNum); SelectedLen := SelectedLen - PF^.Size;
                                                                     PackedLen := PackedLen - PF^.PSize end;
                                           end;
                                         end;
                                        end;
                                      PosChanged := True;
                                      if InfoView <> nil then InfoView^.DrawView;
                                      CED; PosChanged := Off;
                                      if (RepeatDelay <> 0) and (ViewEnabled or
                                         (Drive^.Flags and psShowLongDesc <> 0) and
                                         (Drive^.DriveType < dtArc)) then
                                        NeedLocated := GetSTime;
                                     end;
               end;
  evMouseDown: CommandHandle(Event);
 end;
end;
        {-DataCompBoy-}

end.
