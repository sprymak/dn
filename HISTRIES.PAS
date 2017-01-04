{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.6.RC1
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

unit Histries;

interface
uses LFN, Collect, Drivers, Objects, Views, EdWin, Drives, advance, advance1,
     advance2, advance3, U_KeyMap, microed
     {$IFDEF VIRTUALPASCAL}, Crt{$ENDIF}
{$IFDEF DBView},DBView{$ENDIF} {$IFDEF SpreadSheet},Calc{$ENDIF}
     ;

type
     PViewRecord = ^TViewRecord;
     TViewRecord = record
       fName     : PString;
       fOrigin   : TPoint;
       fSize     : TPoint;
       fDeskSize : TPoint;
       fViewMode : AInt;
      case byte of
       0:(fPos          : LongInt;
          fBufPos       : AWord;
          fXlatFile     : String[8];
          fFilter       : Byte;
          fHexEdit      : Boolean;
          fWrap         : Byte; {DataCompBoy}
          fXDelta       : AInt;
          fHexPos       : AInt;
          fCur          : TPoint;
          fUseXlat      : Boolean;
          fKeyMap       : TKeyMap;
          fMarks        : TPosArray);
{$IFDEF DBView}
       1:(fdDelta       : TDBPoint;
          fdPos         : TDBPoint);
{$ENDIF}
{$IFDEF SpreadSheet}
       2:(fsDelta       : TPoint;
          fsCur         : TPoint;
          fsMark        : TPoint;
          fsCurrentCalc : TPoint;
          fsSearchPos   : TPoint;
          fsErrorCell   : TPoint);
{$ENDIF}
    end;

     PEditRecord = ^TEditRecord;
     TEditRecord = record
       fName: PString;
       fOrigin: TPoint;
       fSize: TPoint;
       fDeskSize: TPoint;
       fPos: TPoint;
       fDelta: TPoint;
       fMarks: TPosArray;
       fBlockStart: TPoint;
       fBlockEnd: TPoint;
       fBlockVisible: Boolean;
       fVerticalBlock: Boolean;
       fHighlight: Boolean;
       fHiliteColumn: Boolean;
       fHiliteLine: Boolean;
       fAutoIndent: Boolean;
       fAutoJustify: Boolean;
       fAutoBrackets: Boolean;
       fInsMode: Boolean;
       fLeftSide,
       fRightSide,
       fInSide: AInt;
       fKeyMap: TKeyMap; {-$VIV}
     end;

     PEditHistoryCol = ^TEditHistoryCol;
     TEditHistoryCol = object(TCollection)
        function  IndexOf(P: Pointer): LongInt; virtual;
        procedure PutItem(var S: TStream; P: Pointer); virtual;
        function  GetItem(var S: TStream): Pointer; virtual;
        procedure FreeItem(P: Pointer); virtual;
     end;

     PViewHistoryCol = ^TViewHistoryCol;
     TViewHistoryCol = object(TEditHistoryCol)
        procedure PutItem(var S: TStream; P: Pointer); virtual;
        function  GetItem(var S: TStream): Pointer; virtual;
        procedure FreeItem(P: Pointer); virtual;
     end;

procedure AddToDirectoryHistory(S: String; DriveType:integer);
procedure SaveCommands(var S: TStream);
procedure LoadCommands(var S: TStream);
procedure CmdHistory;
{DataCompBoy
procedure InitCommands;
}
function  DirHistoryMenu: String;
procedure EditHistoryMenu;
procedure ViewHistoryMenu;
procedure StoreEditInfo(P: Pointer);
procedure StoreViewInfo(P: Pointer);
procedure StoreExtViewer(const FileName: String);
procedure AddCommand(const LastCommand: String);
function  GetCommand(Idx: Integer): String;
procedure LoadHistories;
procedure SaveHistories;
procedure ClearHistories;


const
      MaxDirHistorySize = 40;
      MaxEditHistorySize = 20;

      CmdStrings:  PCollection = nil;
      DirHistory:  PCollection = nil;
      EditHistory: PCollection = nil;
      ViewHistory: PCollection = nil;

implementation
 uses Dos, Commands, DNApp, DNStdDlg, Dialogs, HistList,
      Startup, RStrings, xTime, Messages, DNUtil, DnIni,
{$IFDEF SS}
      Idlers,
{$ENDIF}
      FViewer, CmdLine;

procedure FreeLastUnmarked(C: PCollection);

 var I: Integer;

  function IsThat(P: PEditRecord): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
  begin
    Dec(I);
    IsThat := (P^.fName^[1] = ' ');
  end;

begin
  if (C = nil) or (C^.Count < 1) then Exit;
  I := C^.Count;
  if C^.LastThat(@IsThat) <> nil then C^.AtFree(I);
end;


function  TEditHistoryCol.IndexOf;
  var S: String;
      I: Integer;
begin
  IndexOf := -1;
  S := {$IFNDEF OS2}lfGetLongFileName{$ENDIF}(Copy(CnvString(PEditRecord(P)^.fName), 2, 255)); {DataCompBoy}
  for I := 0 to Count-1 do
    begin
      if Copy(CnvString(PEditRecord(At(I))^.fName), 2, 255) = S then {-$VOL}
        begin IndexOf := I; Exit end;
    end;
end;

procedure TEditHistoryCol.PutItem;
begin
  S.WriteStr(PEditRecord(P)^.fName);
  S.Write(PEditRecord(P)^.fOrigin, SizeOf(TEditRecord)-SizeOf(PString));
end;

function  TEditHistoryCol.GetItem;
  var R: PEditRecord;
begin
  New(R); GetItem := R;
  R^.fName := S.ReadStr;
  S.Read(R^.fOrigin, SizeOf(TEditRecord)-SizeOf(PString));
end;

procedure TEditHistoryCol.FreeItem;
begin
  if P <> nil then
    begin
      DisposeStr(PEditRecord(P)^.fName);
      Dispose(PEditRecord(P));
    end;
end;

procedure TViewHistoryCol.PutItem;
begin
  S.WriteStr(PViewRecord(P)^.fName);
  S.Write(PViewRecord(P)^.fOrigin, SizeOf(TViewRecord)-SizeOf(PString));
end;

function  TViewHistoryCol.GetItem;
  var R: PViewRecord;
begin
  New(R); GetItem := R;
  R^.fName := S.ReadStr;
  S.Read(R^.fOrigin, SizeOf(TViewRecord)-SizeOf(PString));
end;

procedure TViewHistoryCol.FreeItem;
begin
  if P <> nil then
    begin
      DisposeStr(PViewRecord(P)^.fName);
      Dispose(PViewRecord(P));
    end;
end;

procedure StoreViewInfo(P: Pointer);
  var Viewer: PFileWindow absolute P;
      {$IFDEF DBView}
      DBView: PDBWindow   absolute P;
      {$ENDIF}
      {$IFDEF SpreadSheet}
      SSView: PCalcWindow absolute P;
      {$ENDIF}
      R: PViewRecord;
      I: Integer;
label Q;
begin
  if (InterfaceData.Options and ouiTrackViewers = 0) or (P=nil) then Exit;
  if ViewHistory = nil then ViewHistory := New(PViewHistoryCol, Init(30, 30));
  New(R);

  if TypeOf((PObject(P)^))=TypeOf(TFileWindow) then
   with PFileViewer(Viewer^.Current)^, R^ do begin
     if VFileName = '' then goto Q;
     fName     := NewStr(' '+{$IFNDEF OS2}lfGetLongFileName{$ENDIF}(VFileName)); {DataCompBoy}
     fOrigin   := Viewer^.Origin;
     fSize     := Viewer^.Size;
     fDeskSize := Desktop^.Size;
     if Filtr then fViewMode := ViewMode
              else fViewMode := ViewMode or vmInternal;
     fPos      := FilePos;
     fBufPos   := BufPos;
     fXlatFile := CnvString(XlatFile);
     fFilter   := Filter;
     fHexEdit  := HexEdit;
     fWrap     := Wrap;
     fXDelta   := XDelta;
     fHexPos   := HexPos;
     fCur      := Cur;
     fUseXLat  := UseXLat;
     fKeyMap   := KeyMap;
     fMarks    := MarkPos;
   end
{$IFDEF DBView}{-DataCompBoy-}
  else if TypeOf((PObject(P)^))=TypeOf(TDBWindow) then
   with DBView^, R^ do begin
     fName     := NewStr(' '+{$IFNDEF OS2}lfGetLongFileName{$ENDIF}(CnvString(P^.FileName)));
     fOrigin   := Origin;
     fSize     := Size;
     fDeskSize := Desktop^.Size;
     fViewMode := vmDB;
    with P^ do begin
     fdDelta   := Delta;
     fdPos     := Pos;
    end;
   end
{$ENDIF}
{$IFDEF SpreadSheet}
  else if TypeOf((PObject(P)^))=TypeOf(TCalcWindow) then
   with SSView^, R^ do begin
     fName         := NewStr(' '+{$IFNDEF OS2}lfGetLongFileName{$ENDIF}(CnvString(CalcView^.SName)));
     fOrigin       := Origin;
     fSize         := Size;
     fDeskSize     := Desktop^.Size;
     fViewMode     := vmSpread;
    with CalcView^ do begin
     fsDelta       := Delta;
     fsCur         := Cur;
     fsMark        := Mark;
     fsCurrentCalc := CurrentCalc;
     fsSearchPos   := SearchPos;
     fsErrorCell   := ErrorCell;
    end;
   end
{$ENDIF}
  else
Q:begin Dispose(R); R:=nil end;
              {-DataCompBoy-}
  if R<> nil then begin
   I := ViewHistory^.IndexOf(R);
   if I >= 0 then begin
                    R^.fName^[1] := PViewRecord(ViewHistory^.At(I))^.fName^[1];
                    ViewHistory^.AtFree(I);
                  end;
   ViewHistory^.AtInsert(0, R);
  end;
  if ViewHistory^.Count > MaxEditHistorySize then FreeLastUnmarked(ViewHistory);
end;

procedure StoreExtViewer(const FileName: String);
  var R: PViewRecord;
      I: Integer;
begin
  if (InterfaceData.Options and ouiTrackViewers = 0) or (FileName='') then Exit;
  if ViewHistory = nil then ViewHistory := New(PViewHistoryCol, Init(30, 30));
  New(R);

  with R^ do begin
   fName     := NewStr(' '+{$IFNDEF OS2}lfGetLongFileName{$ENDIF}(FileName)); {DataCompBoy}
   fViewMode := vmExternal;
  end;
  I := ViewHistory^.IndexOf(R);
  if I >= 0 then begin
   R^.fName^[1] := PViewRecord(ViewHistory^.At(I))^.fName^[1];
   ViewHistory^.AtFree(I);
  end;
  ViewHistory^.AtInsert(0, R);
  if ViewHistory^.Count > MaxEditHistorySize then FreeLastUnmarked(ViewHistory);
end;

procedure StoreEditInfo(P: Pointer);
  var E: PEditWindow absolute P;
      I: Integer;
      R: PEditRecord;
      PP: PEditRecord;

begin
  if (InterfaceData.Options and ouiTrackEditors = 0) or
     (PFileEditor(E^.Intern)^.EditName = '') then Exit;
  if EditHistory = nil then EditHistory := New(PEditHistoryCol, Init(30, 30));
  New(R);
  with PFileEditor(E^.Intern)^, R^ do
    begin
       fName          := NewStr(' '+{$IFNDEF OS2}lfGetLongFileName{$ENDIF}(EditName)); {DataCompBoy}
       fOrigin        := Owner^.Origin;
       fSize          := Owner^.Size;
       fDeskSize      := Desktop^.Size;
       fMarks         := MarkPos;
       fBlockStart    := Mark.A;
       fBlockEnd      := Mark.B;
       fPos           := Pos;
       fDelta         := Delta;
       fBlockVisible  := BlockVisible;
       fVerticalBlock := VertBlock;
       fHighlight     := EdOpt.Hilite;
       fHiliteColumn  := EdOpt.HiliteColumn;
       fHiliteLine    := EdOpt.HiliteLine;
       fAutoIndent    := EdOpt.AutoIndent;
       fAutoJustify   := EdOpt.AutoJustify;
       fAutoBrackets  := EdOpt.AutoBrackets;
       fLeftSide      := EdOpt.LeftSide;
       fRightSide     := EdOpt.RightSide;
       fInSide        := EdOpt.InSide;
       fInsMode       := InsertMode;
       fKeyMap        := KeyMap; {-$VIV}
    end;
  I := EditHistory^.IndexOf(R);
  if I >= 0 then begin
                   R^.fName^[1] := PViewRecord(EditHistory^.At(I))^.fName^[1];
                   EditHistory^.AtFree(I);
                 end;
  EditHistory^.AtInsert(0, R);
  if EditHistory^.Count > MaxEditHistorySize then FreeLastUnmarked(EditHistory);
end;

procedure AddCommand(const LastCommand: String);
  var I: Integer;
      P: PString;
  label 1;
begin
  if LastCommand <> '' then
    begin
      if CmdStrings = nil then
        CmdStrings := New(PLineCollection, Init(40,40));
      for I := 0 to CmdStrings^.Count - 1 do
        begin
          P := CmdStrings^.At(I);
          if Copy(CnvString(P),2,255) = LastCommand then
            begin
              CmdStrings^.AtDelete(I);
              CmdStrings^.Insert(P);
              Goto 1;
            end;
        end;
      CmdStrings^.Insert(NewStr(' '+LastCommand));
1:
      I := 0;
      while (CmdStrings^.Count > 50) and
            (I < CmdStrings^.Count) do
         begin
            FreeStr := CnvString(CmdStrings^.At(I));
            if FreeStr[1] <> '+' then
              begin
                {if LastTHistPos > I then Dec(LastTHistPos);}
                CmdStrings^.AtFree(I);
              end
              else Inc(I);
         end;
    end;
end;
{DataCompBoy
procedure InitCommands;
begin
 if CmdStrings <> nil then Dispose(CmdStrings,Done);
 CmdStrings := New(PLineCollection, Init(40, 10));
 StrModified := False;
 CurString := 0;
end;
}
procedure SaveCommands(var S: TStream);
 var I, J: Integer;
     S1,S2: String;
     M: PCollection;
begin
 Message(CommandLine, evCommand, cmExecCommandLine, nil);
 if (CmdStrings <> nil) and (CmdStrings^.Count >= 50) then
   begin
     M := New(PLineCollection, Init(50, 10));
     for I := 1 to 40 do
      begin
        if CmdStrings^.Count <= 0 then Break;
        M^.AtInsert(0, CmdStrings^.At(CmdStrings^.Count-1));
        CmdStrings^.AtDelete(CmdStrings^.Count-1);
      end;
     Dispose(CmdStrings,Done);
     CmdStrings := M;
   end;
 S.Put(CmdStrings);
 if InterfaceData.Options and ouiTrackDirs <> 0 then S.Put(DirHistory) else S.Put(nil);
 if InterfaceData.Options and ouiTrackEditors <> 0 then S.Put(EditHistory) else S.Put(nil);
 if InterfaceData.Options and ouiTrackViewers <> 0 then S.Put(ViewHistory) else S.Put(nil);
end;

procedure LoadCommands(var S: TStream);
 var I: Integer;
begin
 CmdStrings := PCollection(S.Get);
 DirHistory := PCollection(S.Get);
 EditHistory := PCollection(S.Get);
 ViewHistory := PCollection(S.Get);

 if CmdStrings<>nil then CurString := CmdStrings^.Count else CurString:=0;
 StrModified := False;
 if CommandLine <> nil then CommandLine^.DrawView;
 Str := '';
end;

function GetCommand(Idx: Integer): String;
begin
  GetCommand := '';
  if (CmdStrings = nil) or (CmdStrings^.Count <= Idx) or (CmdStrings^.At(Idx) = nil) then Exit;
  GetCommand := Copy(PString(CmdStrings^.At(Idx))^,2,255);
end;


type
     PTHistList = ^TTHistList;
     TTHistList = object(TListBox)
       EVHistory, CommandHistory: Boolean;
       function ItemStr(I: LongInt): PString;
       function IsSelected(I: LongInt): Boolean; virtual;
       function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
       procedure SelectItem(Item: LongInt); virtual;
       procedure HandleEvent(var Event: TEvent); virtual;
       destructor Done; virtual;
     end;

function TTHistList.IsSelected;
  var P: PString;
begin
  P := List^.At(I);
  IsSelected := (P <> nil) and (P^[1] = '+');
end;

function TTHistList.ItemStr;
begin
  if EVHistory then ItemStr := PEditRecord(List^.At(I))^.fName
               else ItemStr := List^.At(I);
end;

function TTHistList.GetText;
begin
  FreeStr := Cut(CnvString(ItemStr(Item)), Size.X);
  if FreeStr[1] = '+' then FreeStr[1] := #254
                      else FreeStr[1] := ' ';
  GetText := FreeStr;
end;

procedure TTHistList.SelectItem;
  var P: PString;
begin
  P := ItemStr(Focused);
  if P <> nil then
    if P^[1] <> '+' then P^[1] := '+'
                    else P^[1] := ' ';
  DrawView;
end;

procedure TTHistList.HandleEvent;
  label 1;
  var P: Pointer;

  procedure ScanMarked(D: Integer);
    var P: PString;
        I: Integer;
  begin
    ClearEvent(Event);
    I := Focused;
    repeat
      Inc(I, D);
      if (I < 0) or (I >= List^.Count) or (ItemStr(I)^[1] <> ' ') then Break;
    until False;
    if (I >= 0) and (I < List^.Count) then begin FocusItem(I); DrawView end;
  end;

begin
  if (Event.What = evKeyDown) and (List <> nil) then
    case Event.KeyCode of
      kbDel: Goto 1;
      kbDown: if (ShiftState and 3 <> 0) and (Focused < List^.Count - 1) then
             begin
               P := List^.At(Focused);
               List^.AtDelete(Focused);
               List^.AtInsert(Focused+1, P);
               DrawView;
             end;
      kbUp: if (ShiftState and 3 <> 0) and (Focused > 0) then
             begin
               P := List^.At(Focused-1);
               List^.AtDelete(Focused-1);
               List^.AtInsert(Focused, P);
               DrawView;
             end;
      kbRight: ScanMarked(1);
      kbLeft: ScanMarked(-1);
    end else
  if (Event.What = evBroadcast) then
   case Event.Command of
    cmOK: begin
             ClearEvent(Event);
             if Focused >= List^.Count then Exit;
             FreeStr := fDelLeft(fDelRight(Copy(CnvString(List^.At(Focused)), 2, 255))); {-$VIV} {DataCompBoy}
             if InputBox(GetString(dlEditHistory), GetString(dlFindCellString), FreeStr, 255, hsEditHistory) <> cmOK then Exit;
             Insert(Copy(CnvString(List^.At(Focused)), 1, 1), FreeStr, 1);
             List^.AtReplace(Focused, NewStr(FreeStr));
             DrawView;
          end;
    cmYes:
1:         begin
             ClearEvent(Event);
             if Focused >= List^.Count then Exit;
             if Copy(CnvString(ItemStr(Focused)),1,1) = '+' then
               begin
                If HistoryErrorBeep then begin
                 {$IFDEF VIRTUALPASCAL}
                 PlaySound(500, 110);
                 {$ELSE}
                 Sound(500);DelayTics(2);NoSound;
                 {$ENDIF}
                end;
                Exit;
               end;
             if CommandHistory and (Focused <= CurString) and (CurString > 0) then Dec(CurString);
             List^.AtFree(Focused);
             SetRange(List^.Count);
             DrawView;
           end;
   end;
  inherited HandleEvent(Event);
end;

destructor TTHistList.Done;
begin
  List := nil;
  inherited Done;
end;

procedure AddToDirectoryHistory(S: String; DriveType:integer);
  var I: Integer;
      P: PString;

  function IsThat(P: PString): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
  begin
    Inc(I);
    IsThat := S = Copy(P^, 2, 255);
  end;

  function IsThis(P: PString): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
  begin
    IsThis := P^[1] = ' ';
  end;

begin
  if InterfaceData.Options and ouiTrackDirs = 0 then Exit;
  if (S = '') or (S[2] <> ':') then Exit;
  {$IFNDEF OS2}
  S:=lfGetLongFileName(S);
  {$ENDIF}
  if DirHistory = nil then DirHistory := New(PLineCollection, Init(40, 40));
  if (DriveType <> integer(dtList)) and
     (DriveType <> integer(dtFind)) and
     (DriveType <> integer(dtArvid)) and
     (DriveType <> integer(dtArc))
  then begin
      if S[Length(S)] <> '\' then AddStr(S, '\')
  end else begin
   if S[Length(S)] in ['\','/'] then Dec(S[0]);
   if (DriveType = integer(dtArvid)) or
      (DriveType = integer(dtArc)) then AddStr(S,'\');
  end;
  I := -1; P := DirHistory^.FirstThat(@IsThat);
  if P <> nil then DirHistory^.AtDelete(I)
              else P := NewStr(' '+S);
  if P <> nil then DirHistory^.AtInsert(0, P);
  if DirHistory^.Count > MaxDirHistorySize then
    begin
      P := DirHistory^.LastThat(@IsThis);
      if P <> nil then DirHistory^.Free(P);
    end;
end;

function GetDialog(Dlg: TDlgIdx; var List: Pointer): PDialog;
var
  D: PDialog;
  P: PView;
  R: TRect;
begin
  D := PDialog( LoadResource( Dlg ));

  R.Assign(D^.Size.X-3, 2, D^.Size.X-2, 13);
  P := New(PScrollBar, Init(R));
  D^.Insert(P);

  R.Assign(2, 2, D^.Size.X-3, 13);
  P := New(PTHistList, Init(R, 1, PScrollBar(P)));
  D^.Insert(P); List := P;

  GetDialog := D;
end;

procedure EditHistoryMenu;
  var D: PDialog;
      P: PTHistList;
      I: Integer;
begin
  if InterfaceData.Options and ouiTrackEditors = 0 then begin
    Msg(dlSetEditHistory, nil, mfError+mfOKButton);
    exit;
  end;
  if EditHistory = nil then EditHistory := New(PEditHistoryCol, Init(30, 30));
{  if EditHistory^.Count = 0 then Exit;}
  D := GetDialog(dlgEditHistory, Pointer(P));
  P^.NewList(EditHistory);
  P^.EVHistory := On;
  if Desktop^.ExecView(D) = cmOK then I := P^.Focused else I := -1;
  Dispose(D,Done);
  if EditHistory^.Count = 0 then Exit; {Proverka, esli udalyali, zarazy:}
  if (I >= 0) then
    begin
     If (PViewRecord(EditHistory^.At(I))^.fName = nil) then Exit;
     {A eto tak, na vsyakiy sluchay proverka, esli eto ne DPMI :}
     PDNAppl(Application)^.EditFile(Off,
       Copy(PViewRecord(EditHistory^.At(I))^.fName^, 2, 255));
    end;
end;

procedure ViewHistoryMenu;
  var D: PDialog;
      P: PTHistList;
      I: Integer;
begin
  if InterfaceData.Options and ouiTrackViewers = 0 then
    begin
      Msg(dlSetViewHistory, nil, mfError+mfOKButton);
      Exit;
    end;
  if ViewHistory = nil then ViewHistory := New(PViewHistoryCol, Init(30, 30));
{  if ViewHistory^.Count = 0 then Exit;}
  D := GetDialog(dlgViewHistory, Pointer(P));
  P^.NewList(ViewHistory);
  P^.EVHistory := On;
  if Desktop^.ExecView(D) = cmOK then I := P^.Focused else I := -1;
  Dispose(D,Done);
  if ViewHistory^.Count = 0 then Exit; {Proverim, esli udalyali, pa**y}
  if I >= 0 then
   begin
    If (PViewRecord(ViewHistory^.At(I))^.fName = nil) then Exit; {Ku :}
    PDNAppl(Application)^.ViewFile(1, (* X-Man *)
     Copy(PViewRecord(ViewHistory^.At(I))^.fName^,2,255));
   end;
end;

function DirHistoryMenu: String;
var
  PC: PLineCollection;
  D: PDialog;
  P: PView;
  R: TRect;
  I: Integer;
  DT: Record PC: PCollection; I: Integer; end;
begin
  DirHistoryMenu := '';

  if InterfaceData.Options and ouiTrackDirs = 0 then
    begin
      Msg(dlSetDirHistory, nil, mfError+mfOKButton);
      Exit;
    end;

  if DirHistory = nil then DirHistory := New(PLineCollection, Init(40, 40));
{  if DirHistory^.Count = 0 then Exit;}

  D := GetDialog(dlgDirectoryHistory, Pointer(P));

  PListBox(P)^.NewList(DirHistory);
  if DirHistory^.Count>1 then PListBox(P)^.Focused := 1;

  I := Desktop^.ExecView(D);

  DT.I := PListBox(P)^.Focused;
  Dispose(D,Done);
  if I = cmOK then DirHistoryMenu := Copy(CnvString(DirHistory^.At(DT.I)),2,255);
end;


procedure CmdHistory;
var
  PC: PLineCollection;
  D: PDialog;
  P: PView;
  R: TRect;
  I: Integer;
  DT: Record PC: PCollection; I: Integer; end;
begin
  if CmdStrings = nil then
    CmdStrings := New(PLineCollection, Init(40,40));

  D := GetDialog(dlgCommandsHistory, Pointer(P));

  PListBox(P)^.NewList(CmdStrings);
  PListBox(P)^.FocusItem(CmdStrings^.Count-1);
  PTHistList(P)^.CommandHistory := On;
  if CmdStrings^.Count > 0 then PListBox(P)^.FocusItem(CmdStrings^.Count - 1);

  I := Desktop^.ExecView(D);

  DT.I := PListBox(P)^.Focused;
  Dispose(D,Done);

  if I = cmCancel then Exit;
  Message(CommandLine, evKeyDown, kbDown, nil);

  CurString := DT.I;
  Str := GetCommand(DT.I);
  CommandLine^.DrawView;
  Message(CommandLine, evKeyDown, kbEnd, nil);
  if I <> cmYes then Message(CommandLine, evKeyDown, kbEnter, nil);
end;

const
      HistoryFileSign = 'DN OSP History file'#13#10#26#1#51#05;

        {-DataCompBoy-}
procedure LoadHistories;
  var S: TBufStream;
      A: AWord;
begin
    S.Init(SourceDir+'DN'+GetEnv('DNDSK')+'.HIS', stOpenRead, 2048);
    if S.Status = stOK then
      begin
        S.Read(FreeStr[1], Length(HistoryFileSign));
        FreeStr[0] := Char(Length(HistoryFileSign));
        if FreeStr = HistoryFileSign then
          begin
             S.Read(A, sizeof(A));
             if HistorySize < A + 256 then
               begin
                  DoneHistory;
                  HistorySize := A + 256;
                  InitHistory;
               end;
             {$IFDEF BIT_32}
             HistoryUsed := A + Word(HistoryBlock);
             {$ELSE}
             HistoryUsed := A;
             {$ENDIF}
             S.Read(HistoryBlock^, A);
             LoadCommands(S);
          end
        else MessageBox('Can''t load histories!', nil, mfOkButton);
      end;
    S.Done;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure SaveHistories;
  var S: TBufStream;
      A: AWord;
begin
  S.Init(SourceDir+'DN'+GetEnv('DNDSK')+'.HIS', stCreate, 2048);
  if S.Status = stOK then
    begin
      FreeStr := HistoryFileSign;
      S.Write(FreeStr[1], Length(HistoryFileSign));
      {$IFDEF BIT_32}
      A := HistoryUsed-Word(HistoryBlock);
      {$ELSE}
      A := HistoryUsed;
      {$ENDIF}
      S.Write(A, sizeof(A));
      S.Write(HistoryBlock^, A);
      SaveCommands(S);
    end;
  S.Done;
end;
        {-DataCompBoy-}

procedure ClearHistories;
  var I, J, K: Integer;
      B: PChar;

  procedure ClearStrCollection(C: PCollection);
  begin
    if C = nil then Exit;
    C^.Pack;
    I := 0;
    while I < C^.Count do
      begin
        FreeStr := CnvString(C^.At(I));
        if FreeStr[1] = ' ' then C^.AtFree(I)
                            else Inc(I);
      end;
  end;

  procedure ClearCollection(C: PCollection);
  begin
    if C = nil then Exit;
    C^.Pack;
    I := 0;
    while I < C^.Count do
      begin
        FreeStr := CnvString(PEditRecord(C^.At(I))^.fName);
        if FreeStr[1] = ' ' then C^.AtFree(I)
                            else Inc(I);
      end;
  end;

begin
  B := PChar(HistoryBlock);
  I := 1;
  while I < HistoryUsed do
    begin
      if B[I+1] = #0 then Break;
      Move(B[I+2], FreeStr, Byte(B[I+2])+1);
      J := I + Byte(B[I+2])+3;
      if FreeStr[Length(FreeStr)] <> ' ' then I := J
       else
         begin
           Move(B[J], B[I], HistoryUsed - J + 1);
           Dec(HistoryUsed, J - I);
         end;
    end;
  if HistoryUsed < 4 then HistoryUsed := 0;
  I := 0;
  ClearStrCollection(CmdStrings);
  ClearStrCollection(DirHistory);
  ClearCollection(EditHistory);
  ClearCollection(ViewHistory);
end;

end.
