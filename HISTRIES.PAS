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

unit Histries;

interface

uses
  Lfn, Collect, Drivers, Objects, Views, EdWin, Drives, advance,
    advance1,
  advance2, advance3, U_KeyMap, Microed
  {, Crt}
  {$IFDEF DBView}, DBView {$ENDIF} {$IFDEF SpreadSheet}, Calc
    {$ENDIF}
  ;

type
  PViewRecord = ^TViewRecord;
  TViewRecord = record
    FName: PString;
    fOrigin: TPoint;
    fSize: TPoint;
    fDeskSize: TPoint;
    fViewMode: AInt;
    case byte of
      0: (fPos: longInt;
      fBufPos: AWord;
      fXlatFile: String[8];
      fFilter: byte;
      fHexEdit: boolean;
      fWrap: byte; {DataCompBoy}
      fXDelta: AInt;
      fHexPos: AInt;
      fCur: TPoint;
      fUseXLat: boolean;
      fKeyMap: TKeyMap;
      fMarks: TPosArray);
      {$IFDEF DBView}
      1: (fdDelta: TDBPoint;
      fdPos: TDBPoint;
      );
      {$ENDIF}
      {$IFDEF SpreadSheet}
      2: (fsDelta: TPoint;
      fsCur: TPoint;
      fsMark: TPoint;
      fsCurrentCalc: TPoint;
      fsSearchPos: TPoint;
      fsErrorCell: TPoint);
      {$ENDIF}
  end;

PEditRecord = ^TEditRecord;
TEditRecord = record
  FName: PString;
  fOrigin: TPoint;
  fSize: TPoint;
  fDeskSize: TPoint;
  fPos: TPoint;
  fDelta: TPoint;
  fMarks: TPosArray;
  fBlockStart: TPoint;
  fBlockEnd: TPoint;
  fBlockVisible: boolean;
  fVerticalBlock: boolean;
  fHighlight: boolean;
  fHiliteColumn: boolean;
  fHiliteLine: boolean;
  fAutoIndent: boolean;
  fAutoJustify: boolean;
  fAutoBrackets: boolean;
  fInsMode: boolean;
  fLeftSide,
  fRightSide,
  fInSide: AInt;
  fKeyMap: TKeyMap; {-$VIV}
  { Flash >>> }
  fBackIndent: boolean;
  fAutoWrap: boolean;
  fOptimalFill: boolean;
  fTabReplace: boolean;
  fSmartTab: boolean;
  { Flash <<< }
  end;

PEditHistoryCol = ^TEditHistoryCol;
TEditHistoryCol = object(TCollection)
  function IndexOf(P: Pointer): CondInt; virtual;
  procedure PutItem(var s: TStream; P: Pointer); virtual;
  function GetItem(var s: TStream): Pointer; virtual;
  procedure FreeItem(P: Pointer); virtual;
  end;

PViewHistoryCol = ^TViewHistoryCol;
TViewHistoryCol = object(TEditHistoryCol)
  procedure PutItem(var s: TStream; P: Pointer); virtual;
  function GetItem(var s: TStream): Pointer; virtual;
  procedure FreeItem(P: Pointer); virtual;
  end;

procedure AddToDirectoryHistory(s: String; DriveType: integer);
procedure SaveCommands(var s: TStream);
procedure LoadCommands(var s: TStream);
procedure CmdHistory;
{DataCompBoy
procedure InitCommands;
}
function DirHistoryMenu: String;
procedure EditHistoryMenu;
procedure ViewHistoryMenu;
procedure StoreEditInfo(P: Pointer);
procedure StoreViewInfo(P: Pointer);
procedure StoreExtViewer(const FileName: String);
procedure AddCommand(const LastCommand: String);
function GetCommand(Idx: integer): String;
procedure LoadHistories;
procedure SaveHistories;
procedure ClearHistories;
procedure DoneHistories; {Cat}

const
  MaxDirHistorySize = 40;
  MaxEditHistorySize = 20;

  CmdStrings: PCollection = nil;
  DirHistory: PCollection = nil;
  EditHistory: PCollection = nil;
  ViewHistory: PCollection = nil;

implementation
uses
  Dos, Commands, DNApp, Dialogs, HistList,
  Startup, RStrings, xTime, Messages, DNUtil, DnIni,
  VpSysLow,
  {$IFDEF SS}
  Idlers,
  {$ENDIF}
  FViewer, CmdLine;

procedure FreeLastUnmarked(C: PCollection);

  var
    i: integer;

  function IsThat(P: PEditRecord): boolean;
    begin
      Dec(i);
      IsThat := (P^.FName <> nil) and (P^.FName^[1] = ' ');
    end;

  begin
    if (C = nil) or (C^.Count < 1) then
      exit;
    i := C^.Count;
    if C^.LastThat(@IsThat) <> nil then
      C^.AtFree(i);
  end;

function TEditHistoryCol.IndexOf;
  var
    s, s1: String;
    i: integer;
  begin
    IndexOf := -1;
    s := CnvString(PEditRecord(P)^.FName);
    s[1] := ' ';
    UpStr(s); {AK155}
    for i := 0 to Count-1 do
      begin
        s1 := CnvString(PEditRecord(At(i))^.FName);
        s1[1] := ' ';
        UpStr(s1); {AK155}
        if s1 = s then{-$VOL}
          begin
            IndexOf := i;
            exit
          end;
      end;
  end { TEditHistoryCol.IndexOf };

procedure TEditHistoryCol.PutItem;
  begin
    s.WriteStr(PEditRecord(P)^.FName);
    s.Write(PEditRecord(P)^.fOrigin, SizeOf(TEditRecord)-SizeOf(
      PString));
  end;

function TEditHistoryCol.GetItem;
  var
    R: PEditRecord;
  begin
    New(R);
    GetItem := R;
    R^.FName := s.ReadStr;
    s.Read(R^.fOrigin, SizeOf(TEditRecord)-SizeOf(PString));
  end;

procedure TEditHistoryCol.FreeItem;
  begin
    if P <> nil then
      begin
        DisposeStr(PEditRecord(P)^.FName);
        Dispose(PEditRecord(P));
      end;
  end;

procedure TViewHistoryCol.PutItem;
  begin
    s.WriteStr(PViewRecord(P)^.FName);
    s.Write(PViewRecord(P)^.fOrigin, SizeOf(TViewRecord)-SizeOf(
      PString));
  end;

function TViewHistoryCol.GetItem;
  var
    R: PViewRecord;
  begin
    New(R);
    GetItem := R;
    R^.FName := s.ReadStr;
    s.Read(R^.fOrigin, SizeOf(TViewRecord)-SizeOf(PString));
  end;

procedure TViewHistoryCol.FreeItem;
  begin
    if P <> nil then
      begin
        DisposeStr(PViewRecord(P)^.FName);
        Dispose(PViewRecord(P));
      end;
  end;

procedure StoreViewInfo(P: Pointer);
  var
    Viewer: PFileWindow absolute P;
    {$IFDEF DBView}
    DBView: PDBWindow absolute P;
    {$ENDIF}
    {$IFDEF SpreadSheet}
    SSView: PCalcWindow absolute P;
    {$ENDIF}
    R: PViewRecord;
    i: integer;
  label Q;
  begin
    if (InterfaceData.Options and ouiTrackViewers = 0) or (P = nil)
    then
      exit;
    if ViewHistory = nil then
      ViewHistory := New(PViewHistoryCol, Init(30, 30));
    New(R);

    if TypeOf((PObject(P)^)) = TypeOf(TFileWindow) then
      with PFileViewer(Viewer^.Current)^, R^ do
        begin
          if VFileName = '' then
            goto Q;
          {$IFDEF OS_DOS}
          FName := NewStr(' '+lfGetLongFileName(VFileName));
            {DataCompBoy}
          {$ELSE}
          FName := NewStr(' '+VFileName);
          {$ENDIF}
          fOrigin := Viewer^.Origin;
          fSize := Viewer^.Size;
          fDeskSize := Desktop^.Size;
          if Filtr then
            fViewMode := ViewMode
          else
            fViewMode := ViewMode or vmInternal;

          {AK155 25-03-2003 После того, как BufPos стало longint, в
fBufPos: AWord оно может не помещаться. Но, с другой стороны,
непонятно, зачем вообще разделять запоминание FilePos и BufPos.
Работают же закладки без такого разделения.  }
          (*
     fPos      := FilePos;
     fBufPos   := BufPos;
     end;
*)
          fPos := FilePos+BufPos;
          fBufPos := 0;
          {/AK155 25-03-2003}

          fXlatFile := CnvString(XLatFile);
          fFilter := Filter;
          fHexEdit := HexEdit;
          fWrap := Wrap;
          fXDelta := XDelta;
          fHexPos := HexPos;
          fCur := Cur;
          fUseXLat := UseXLat;
          fKeyMap := KeyMap;
          fMarks := MarkPos;
        end
        {$IFDEF DBView} {-DataCompBoy-}
      else if TypeOf((PObject(P)^)) = TypeOf(TDBWindow) then
      with DBView^, R^ do
        begin
          if DBView^.RealName = '' then
            goto Q;
          FName := NewStr(' '+DBView^.RealName);
          fOrigin := Origin;
          fSize := Size;
          fDeskSize := Desktop^.Size;
          fViewMode := vmDB;
          with P^ do
            begin
              fdDelta := Delta;
              fdPos := Pos;
              fKeyMap := KeyMap; {John_SW 14-03-2003}
              fUseXLat := UseXLat;
              fXlatFile := CnvString(XLatFile);
            end;
        end
        {$ENDIF}
        {$IFDEF SpreadSheet}
      else if TypeOf((PObject(P)^)) = TypeOf(TCalcWindow) then
      with SSView^, R^ do
        begin
          {$IFDEF OS_DOS}
          FName := NewStr(' '+lfGetLongFileName(CnvString(CalcView^.
            SName)));
          {$ELSE}
          FName := NewStr(' '+CnvString(CalcView^.SName));
          {$ENDIF}
          fOrigin := Origin;
          fSize := Size;
          fDeskSize := Desktop^.Size;
          with CalcView^ do
            begin
              if ShowSeparators then
                fViewMode := vmSpreadSL {AK155}
              else
                fViewMode := vmSpread;
              fsDelta := Delta;
              fsCur := Cur;
              fsMark := Mark;
              fsCurrentCalc := CurrentCalc;
              fsSearchPos := SearchPos;
              fsErrorCell := ErrorCell;
            end;
        end
        {$ENDIF}
      else
Q:
      begin
        Dispose(R);
        R := nil
      end;
    {-DataCompBoy-}
    if R <> nil then
      begin
        i := ViewHistory^.IndexOf(R);
        if i >= 0 then
          begin
            R^.FName^[1] := PViewRecord(ViewHistory^.At(i))^.FName^[1];
            ViewHistory^.AtFree(i);
          end;
        ViewHistory^.AtInsert(0, R);
      end;
    if ViewHistory^.Count > MaxEditHistorySize then
      FreeLastUnmarked(ViewHistory);
    SaveHistories; {AK155}
  end { StoreViewInfo };

procedure StoreExtViewer(const FileName: String);
  var
    R: PViewRecord;
    i: integer;
  begin
    if (InterfaceData.Options and ouiTrackViewers = 0) or (FileName = ''
        )
    then
      exit;
    if ViewHistory = nil then
      ViewHistory := New(PViewHistoryCol, Init(30, 30));
    New(R);

    with R^ do
      begin
        {$IFDEF OS_DOS}
        FName := NewStr(' '+lfGetLongFileName(FileName));
          {DataCompBoy}
        {$ELSE}
        FName := NewStr(' '+FileName);
        {$ENDIF}
        fViewMode := vmExternal;
      end;
    i := ViewHistory^.IndexOf(R);
    if i >= 0 then
      begin
        R^.FName^[1] := PViewRecord(ViewHistory^.At(i))^.FName^[1];
        ViewHistory^.AtFree(i);
      end;
    ViewHistory^.AtInsert(0, R);
    if ViewHistory^.Count > MaxEditHistorySize then
      FreeLastUnmarked(ViewHistory);
  end { StoreExtViewer };

procedure StoreEditInfo(P: Pointer);
  var
    E: PEditWindow absolute P;
    i: integer;
    R: PEditRecord;
    PP: PEditRecord;

  begin
    if (InterfaceData.Options and ouiTrackEditors = 0) or
      (PFileEditor(E^.Intern)^.EditName = '')
    then
      exit;
    if EditHistory = nil then
      EditHistory := New(PEditHistoryCol, Init(30, 30));
    New(R);
    with PFileEditor(E^.Intern)^, R^ do
      begin
        {$IFDEF OS_DOS}
        FName := NewStr(' '+lfGetLongFileName(EditName));
          {DataCompBoy}
        {$ELSE}
        FName := NewStr(' '+EditName);
        {$ENDIF}
        fOrigin := Owner^.Origin;
        fSize := Owner^.Size;
        fDeskSize := Desktop^.Size;
        fMarks := MarkPos;
        fBlockStart := Mark.A;
        fBlockEnd := Mark.B;
        fPos := Pos;
        fDelta := Delta;
        fBlockVisible := BlockVisible;
        fVerticalBlock := VertBlock;
        fHighlight := EdOpt.HiLite;
        fHiliteColumn := EdOpt.HiliteColumn;
        fHiliteLine := EdOpt.HiliteLine;
        fAutoIndent := EdOpt.AutoIndent;
        fAutoJustify := EdOpt.AutoJustify;
        fAutoBrackets := EdOpt.AutoBrackets;
        fLeftSide := EdOpt.LeftSide;
        fRightSide := EdOpt.RightSide;
        fInSide := EdOpt.InSide;
        fInsMode := InsertMode;
        fKeyMap := KeyMap; {-$VIV}
        { Flash >>> }
        fBackIndent := EdOpt.BackIndent;
        fAutoWrap := EdOpt.AutoWrap;
        fOptimalFill := OptimalFill;
        fTabReplace := TabReplace;
        fSmartTab := EdOpt.SmartTab;
        { Flash <<< }
      end;
    i := EditHistory^.IndexOf(R);
    if i >= 0 then
      begin
        R^.FName^[1] := PViewRecord(EditHistory^.At(i))^.FName^[1];
        EditHistory^.AtFree(i);
      end;
    EditHistory^.AtInsert(0, R);
    if EditHistory^.Count > MaxEditHistorySize then
      FreeLastUnmarked(EditHistory);
    SaveHistories; {AK155}
  end { StoreEditInfo };

procedure AddCommand(const LastCommand: String);
  var
    i: integer;
    P: PString;
  label 1;
  begin
    if LastCommand <> '' then
      begin
        if CmdStrings = nil then
          CmdStrings := New(PLineCollection, Init(40, 40, False));
        for i := 0 to CmdStrings^.Count-1 do
          begin
            P := CmdStrings^.At(i);
            if Copy(CnvString(P), 2, MaxStringLength) = LastCommand
            then
              begin
                CmdStrings^.AtDelete(i);
                CmdStrings^.Insert(P);
                goto 1;
              end;
          end;
        CmdStrings^.Insert(NewStr(' '+LastCommand));
1:
        i := 0;
        while (CmdStrings^.Count > 50) and
          (i < CmdStrings^.Count)
        do
          begin
            FreeStr := CnvString(CmdStrings^.At(i));
            if FreeStr[1] <> '+' then
              begin
                {if LastTHistPos > I then Dec(LastTHistPos);}
                CmdStrings^.AtFree(i);
              end
            else
              Inc(i);
          end;
        SaveHistories; {AK155}
      end;
  end { AddCommand };
{DataCompBoy
procedure InitCommands;
begin
 if CmdStrings <> nil then Dispose(CmdStrings,Done);
 CmdStrings := New(PLineCollection, Init(40, 10));
 StrModified := False;
 CurString := 0;
end;
}
procedure SaveCommands(var s: TStream);
  var
    i, j: integer;
    s1, s2: String;
    M: PCollection;
  begin
    {AK155 зачем это Message - непонятно. Выполнение пустой команды
делает ровно ничего (см. cmdline.pas, поиск по cmExecCommandLine)
Message(CommandLine, evCommand, cmExecCommandLine, nil);
/AK155}
    if (CmdStrings <> nil) and (CmdStrings^.Count >= 50) then
      begin
        M := New(PLineCollection, Init(50, 10, False));
        for i := 1 to 40 do
          begin
            if CmdStrings^.Count <= 0 then
              break;
            M^.AtInsert(0, CmdStrings^.At(CmdStrings^.Count-1));
            CmdStrings^.AtDelete(CmdStrings^.Count-1);
          end;
        Dispose(CmdStrings, Done);
        CmdStrings := M;
      end;
    s.Put(CmdStrings);
    if InterfaceData.Options and ouiTrackDirs <> 0 then
      s.Put(DirHistory)
    else
      s.Put(nil);
    if InterfaceData.Options and ouiTrackEditors <> 0 then
      s.Put(EditHistory)
    else
      s.Put(nil);
    if InterfaceData.Options and ouiTrackViewers <> 0 then
      s.Put(ViewHistory)
    else
      s.Put(nil);
  end { SaveCommands };

procedure LoadCommands(var s: TStream);
  var
    i: integer;
  begin
    CmdStrings := PCollection(s.Get);
    DirHistory := PCollection(s.Get);
    EditHistory := PCollection(s.Get);
    ViewHistory := PCollection(s.Get);

    if CmdStrings <> nil then
      CurString := CmdStrings^.Count
    else
      CurString := 0;
    {AK155 Перерисовка комстроки не нужна, а очистка даже мешает}
    (*
 StrModified := False;
 if CommandLine <> nil then CommandLine^.DrawView;
 Str := '';
*)
    {/AK155}
  end;

function GetCommand(Idx: integer): String;
  begin
    GetCommand := '';
    if (CmdStrings = nil) or (CmdStrings^.Count <= Idx) or (
        CmdStrings^.At(Idx) = nil)
    then
      exit;
    GetCommand := Copy(PString(CmdStrings^.At(Idx))^, 2,
      MaxStringLength);
  end;

type
  PTHistList = ^TTHistList;
  TTHistList = object(TListBox)
    EVHistory, CommandHistory: boolean;
    Dlg: TDlgIdx; {AK155}
    function ItemStr(i: longInt): PString;
    function IsSelected(i: longInt): boolean; virtual;
    function GetText(Item: longInt; MaxLen: integer): String;
      virtual;
    procedure SelectItem(Item: longInt); virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    end;

function TTHistList.IsSelected;
  var
    P: PString;
  begin
    P := ItemStr(i);
    IsSelected := (P <> nil) and (P^[1] = '+');
  end;

function TTHistList.ItemStr;
  begin
    if EVHistory then
      ItemStr := PEditRecord(List^.At(i))^.FName
    else
      ItemStr := List^.At(i);
  end;

function TTHistList.GetText;
  begin
    FreeStr := Cut(CnvString(ItemStr(Item)), Size.X);
    if FreeStr[1] = '+' then
      FreeStr[1] := #254
    else
      FreeStr[1] := ' ';
    GetText := FreeStr;
  end;

procedure TTHistList.SelectItem;
  var
    P: PString;
  begin
    P := ItemStr(Focused);
    if P <> nil then
      if P^[1] <> '+' then
        P^[1] := '+'
      else
        P^[1] := ' ';
    DrawView;
  end;

procedure TTHistList.HandleEvent;
  label 1;
  var
    P: Pointer;

  procedure ScanMarked(D: integer);
    var
      P: PString;
      i: integer;
    begin
      ClearEvent(Event);
      i := Focused;
      repeat
        Inc(i, D);
        if (i < 0) or (i >= List^.Count) or (ItemStr(i)^[1] <> ' ')
        then
          break;
      until False;
      if (i >= 0) and (i < List^.Count) then
        begin
          FocusItem(i);
          DrawView
        end;
    end;

  procedure SwapItems(i: integer);
    var
      P: Pointer;
    begin
      with List^ do
        begin
          P := Items^[i];
          Items^[i] := Items^[i+1];
          Items^[i+1] := P;
        end;
      DrawView;
    end;

  begin { TTHistList.HandleEvent }
    if (Event.What = evKeyDown) and (List <> nil) then
      case Event.KeyCode of
        kbCtrlEnter:{AK155: делаем как у всех: CtrlEnter = Drop}
          begin
            EndModal(cmYes);
            ClearEvent(Event);
            exit;
          end;
        kbDel:
          goto 1;
        kbShiftDown:
          if {(ShiftState and 3 <> 0) and}(Focused < List^.Count-1)
          then
            SwapItems(Focused);
        kbShiftUp:
          if {(ShiftState and 3 <> 0) and}(Focused > 0) then
            SwapItems(Focused-1);
        kbRight, kbAltDown:
          ScanMarked(1);
        kbLeft, kbAltUp:
          ScanMarked(-1);
      end
    else if (Event.What = evBroadcast) then
      case Event.Command of
        cmOK:
          begin
            ClearEvent(Event);
            if Focused >= List^.Count then
              exit;
            FreeStr := fDelLeft(fDelRight(Copy(CnvString(List^.At(
              Focused)), 2, 255))); {-$VIV} {DataCompBoy}
            if InputBox(GetString(dlEditHistory), GetString(
                dlFindCellString), FreeStr, 255, hsEditHistory) <>
                cmOK
            then
              exit;
            Insert(Copy(CnvString(List^.At(Focused)), 1, 1), FreeStr, 1)
              ;
            List^.AtReplace(Focused, NewStr(FreeStr));
            DrawView;
          end;
        cmYes:{ удаление }
1:
            begin
              ClearEvent(Event);
              if (Dlg = dlgDirectoryHistory) and (Focused = 0) then
                begin
                  MessageBox(GetString(dlHistDelCurDir), nil,
                    mfOKButton); {John_SW}
                  exit;
                    {AK155: 0 - это текущий каталог,
                  его удалять бесполезно: он опять вставится, но при этом
                  собьет нумерацию элементов }
                end;
              if Focused >= List^.Count then
                exit;
              if Copy(CnvString(ItemStr(Focused)), 1, 1) = '+' then
                begin
                  if HistoryErrorBeep then
                    begin
                      SysBeepEx {PlaySound}(500, 110);
                    end;
                  exit;
                end;
              if CommandHistory and (Focused <= CurString) and (
                  CurString > 0)
              then
                Dec(CurString);
              List^.AtFree(Focused);
              SetRange(List^.Count);
              DrawView;
            end;
      end {case};
    inherited HandleEvent(Event);
  end { TTHistList.HandleEvent };

destructor TTHistList.Done;
  begin
    List := nil;
    inherited Done;
  end;

procedure AddToDirectoryHistory(s: String; DriveType: integer);
  var
    i: integer;
    P: PString;

  function IsThat(P: PString): boolean;
    begin
      Inc(i);
      IsThat := s = Copy(P^, 2, MaxStringLength);
    end;

  function IsThis(P: PString): boolean;
    begin
      IsThis := P^[1] = ' ';
    end;

  begin
    if InterfaceData.Options and ouiTrackDirs = 0 then
      exit;
    if (s = '') or ((s[2] <> ':') and ((s[1] <> '\') or (s[2] <> '\')))
    then
      exit;
    {Cat: добавил проверку на сетевые пути}
    {$IFDEF OS_DOS}
    s := lfGetLongFileName(s);
    {$ENDIF}
    if DirHistory = nil then
      DirHistory := New(PLineCollection, Init(40, 40, False));
    if (DriveType <> integer(dtList)) and
      (DriveType <> integer(dtFind)) and
      (DriveType <> integer(dtArcFind)) and
      (DriveType <> integer(dtArvid)) and
      (DriveType <> integer(dtArc))
    then
      begin
        if s[Length(s)] <> '\' then
          AddStr(s, '\')
      end
    else
      begin
        if s[Length(s)] in ['\', '/'] then
          SetLength(s, Length(s)-1);
        if (DriveType = integer(dtArvid)) or
          (DriveType = integer(dtArc))
        then
          AddStr(s, '\');
      end;
    i := -1;
    P := DirHistory^.FirstThat(@IsThat);
    if P <> nil then
      DirHistory^.AtDelete(i)
    else
      P := NewStr(' '+s);
    if P <> nil then
      DirHistory^.AtInsert(0, P);
    if DirHistory^.Count > MaxDirHistorySize then
      begin
        P := DirHistory^.LastThat(@IsThis);
        if P <> nil then
          DirHistory^.Free(P);
      end;
    SaveHistories; {AK155}
  end { AddToDirectoryHistory };

function GetDialog(Dlg: TDlgIdx; var List: Pointer): PDialog;
  var
    D: PDialog;
    l: PTHistList; {AK155}
    P: PView;
    R: TRect;
  begin
    D := PDialog(LoadResource(Dlg));

    R.Assign(D^.Size.X-3, 2, D^.Size.X-2, 13);
    P := New(PScrollBar, Init(R));
    D^.Insert(P);

    R.Assign(2, 2, D^.Size.X-3, 13);
    l := New(PTHistList, Init(R, 1, PScrollBar(P)));
    l^.Dlg := Dlg; {AK155: см. TTHistList.HandleEvent, cmYes }
    D^.Insert(l);
    List := l;

    GetDialog := D;
  end { GetDialog };

procedure EditHistoryMenu;
  var
    D: PDialog;
    P: PTHistList;
    i: integer;
  begin
    if InterfaceData.Options and ouiTrackEditors = 0 then
      begin
        Msg(dlSetEditHistory, nil, mfError+mfOKButton);
        exit;
      end;
    ClearHistories;
    LoadHistories; {AK155}
    if EditHistory = nil then
      EditHistory := New(PEditHistoryCol, Init(30, 30));
    {  if EditHistory^.Count = 0 then Exit;}
    D := GetDialog(dlgEditHistory, Pointer(P));
    P^.NewLisT(EditHistory);
    P^.EVHistory := True;
    if Desktop^.ExecView(D) = cmOK then
      i := P^.Focused
    else
      i := -1;
    Dispose(D, Done);
    if EditHistory^.Count = 0 then
      exit; {Proverka, esli udalyali, zarazy:}
    if (i >= 0) then
      begin
        if (PViewRecord(EditHistory^.At(i))^.FName = nil) then
          exit;
        {A eto tak, na vsyakiy sluchay proverka, esli eto ne DPMI :}
        PDNApplication(Application)^.EditFile(
        SystemData.Options and ossEditor <> 0,
          {AK155 28.09.2002:
             это чтобы через историю всегда вызывался внутренний
             редактор, поскольку вызов внешнего в историю не попадает}
        Copy(PViewRecord(EditHistory^.At(i))^.FName^, 2,
          MaxStringLength));
      end;
  end { EditHistoryMenu };

procedure ViewHistoryMenu;
  var
    D: PDialog;
    P: PTHistList;
    i: integer;
  begin
    if InterfaceData.Options and ouiTrackViewers = 0 then
      begin
        Msg(dlSetViewHistory, nil, mfError+mfOKButton);
        exit;
      end;
    ClearHistories;
    LoadHistories; {AK155}
    if ViewHistory = nil then
      ViewHistory := New(PViewHistoryCol, Init(30, 30));
    {  if ViewHistory^.Count = 0 then Exit;}
    D := GetDialog(dlgViewHistory, Pointer(P));
    P^.NewLisT(ViewHistory);
    P^.EVHistory := True;
    if Desktop^.ExecView(D) = cmOK then
      i := P^.Focused
    else
      i := -1;
    Dispose(D, Done);
    if ViewHistory^.Count = 0 then
      exit; {Proverim, esli udalyali, pa**y}
    if i >= 0 then
      begin
        if (PViewRecord(ViewHistory^.At(i))^.FName = nil) then
          exit; {Ku :}
        PDNApplication(Application)^.ViewFile(False, True, {AK155}
        Copy(PViewRecord(ViewHistory^.At(i))^.FName^, 2,
          MaxStringLength));
      end;
  end { ViewHistoryMenu };

function DirHistoryMenu: String;
  var
    PC: PLineCollection;
    D: PDialog;
    P: PView;
    R: TRect;
    i: integer;
    DT: record
      PC: PCollection;
      i: integer;
      end;
  begin
    ClearHistories;
    LoadHistories; {AK155}
    DirHistoryMenu := '';

    if InterfaceData.Options and ouiTrackDirs = 0 then
      begin
        Msg(dlSetDirHistory, nil, mfError+mfOKButton);
        exit;
      end;

    if DirHistory = nil then
      DirHistory := New(PLineCollection, Init(40, 40, False));
    {  if DirHistory^.Count = 0 then Exit;}

    D := GetDialog(dlgDirectoryHistory, Pointer(P));

    PListBox(P)^.NewLisT(DirHistory);
    if DirHistory^.Count > 1 then
      PListBox(P)^.Focused := 1;

    i := Desktop^.ExecView(D);

    DT.i := PListBox(P)^.Focused;
    Dispose(D, Done);
    if i = cmOK then
      DirHistoryMenu := Copy(CnvString(DirHistory^.At(DT.i)), 2,
        MaxStringLength);
  end { DirHistoryMenu: };

procedure CmdHistory;
  var
    PC: PLineCollection;
    D: PDialog;
    P: PView;
    R: TRect;
    i: integer;
    DT: record
      PC: PCollection;
      i: integer;
      end;
  begin
    ClearHistories;
    LoadHistories; {AK155}
    if CmdStrings = nil then
      CmdStrings := New(PLineCollection, Init(40, 40, False));

    D := GetDialog(dlgCommandsHistory, Pointer(P));

    PListBox(P)^.NewLisT(CmdStrings);
    PListBox(P)^.FocusItem(CmdStrings^.Count-1);
    PTHistList(P)^.CommandHistory := True;
    if CmdStrings^.Count > 0 then
      PListBox(P)^.FocusItem(CmdStrings^.Count-1);

    i := Desktop^.ExecView(D);

    DT.i := PListBox(P)^.Focused;
    Dispose(D, Done);

    if i = cmCancel then
      exit;
    Message(CommandLine, evKeyDown, kbDown, nil);

    CurString := DT.i;
    Str := GetCommand(DT.i);
    CommandLine^.DrawView;
    Message(CommandLine, evKeyDown, kbEnd, nil);
    if i <> cmYes then
      Message(CommandLine, evKeyDown, kbEnter, nil);
  end { CmdHistory };

const
  HistoryFileSign = 'DN OSP History file'#13#10#26#1#51#05;

  {-DataCompBoy-}
procedure LoadHistories;
  var
    s: TBufStream;
    A: AWord;
  begin
    s.Init(SourceDir+'DN'+GetEnv('DNDSK')+'.HIS', stOpenRead, 2048);
    if s.Status = stOK then
      begin
        s.Read(FreeStr[1], Length(HistoryFileSign));
        SetLength(FreeStr, Length(HistoryFileSign));
        if FreeStr = HistoryFileSign then
          begin
            s.Read(A, SizeOf(A));
            if HistorySize < A+256 then
              begin
                DoneHistory;
                HistorySize := A+256;
                InitHistory;
              end;
            HistoryUsed := A+word(HistoryBlock);
            s.Read(HistoryBlock^, A);
            LoadCommands(s);
          end
        else
          MessageBox('Can''t load histories!', nil, mfOKButton);
      end;
    s.Done;
  end { LoadHistories };
{-DataCompBoy-}

{-DataCompBoy-}
procedure SaveHistories;
  var
    s: TBufStream;
    A: AWord;
  begin
    s.Init(SourceDir+'DN'+GetEnv('DNDSK')+'.HIS', stCreate, 2048);
    if s.Status = stOK then
      begin
        FreeStr := HistoryFileSign;
        s.Write(FreeStr[1], Length(HistoryFileSign));
        A := HistoryUsed-word(HistoryBlock);
        s.Write(A, SizeOf(A));
        s.Write(HistoryBlock^, A);
        SaveCommands(s);
      end;
    s.Done;
  end;
{-DataCompBoy-}

procedure ClearHistories;
  var
    i, j, k: integer;
    B: PChar;

  procedure ClearStrCollection(C: PCollection);
    begin
      if C = nil then
        exit;
      C^.Pack;
      i := 0;
      while i < C^.Count do
        begin
          FreeStr := CnvString(C^.At(i));
          if FreeStr[1] = ' ' then
            C^.AtFree(i)
          else
            Inc(i);
        end;
    end;

  procedure ClearCollection(C: PCollection);
    begin
      if C = nil then
        exit;
      C^.Pack;
      i := 0;
      while i < C^.Count do
        begin
          FreeStr := CnvString(PEditRecord(C^.At(i))^.FName);
          if FreeStr[1] = ' ' then
            C^.AtFree(i)
          else
            Inc(i);
        end;
    end;

  var
    A: AWord;
  begin { ClearHistories }
    if HistoryBlock = nil then
      exit;
    B := PChar(HistoryBlock);
    i := 1;
    A := HistoryUsed-word(HistoryBlock);
    while i < A do
      begin
        if B[i+1] = #0 then
          break;
        Move(B[i+2], FreeStr, byte(B[i+2])+1);
        j := i+byte(B[i+2])+3;
        if FreeStr[Length(FreeStr)] <> ' ' then
          i := j
        else
          begin
            Move(B[j], B[i], A-j+1);
            Dec(A, j-i);
          end;
      end;
    if HistoryUsed < 4 then
      HistoryUsed := 0;
    i := 0;
    ClearStrCollection(CmdStrings);
    ClearStrCollection(DirHistory);
    ClearCollection(EditHistory);
    ClearCollection(ViewHistory);
  end { ClearHistories };

{Cat}
procedure DoneHistories;
  begin
    if CmdStrings <> nil then
      Dispose(CmdStrings, Done);
    if DirHistory <> nil then
      Dispose(DirHistory, Done);
    if EditHistory <> nil then
      Dispose(EditHistory, Done);
    if ViewHistory <> nil then
      Dispose(ViewHistory, Done);
  end;
{/Cat}

end.
