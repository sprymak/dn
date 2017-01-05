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

unit Editor;

interface

uses
  Lfn, Views, Objects, Drivers, Microed, U_KeyMap, Collect, ed2;

type
  PXFileEditor = ^TXFileEditor;
  TXFileEditor = object(TFileEditor)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    function HandleCommand(var Event: TEvent): boolean; virtual;
    end;

implementation

uses
  advance, advance1, advance2, advance3, Dos, Dialogs, DNApp,
    RStrings,
  {SBlocks,}Memory, Gauge, Startup, WinClp, Messages, Commands,
    Macro,
  EdWin, xTime, DnIni, DNUtil, advance6, Calculat, FViewer {AK155}
  {$IFDEF REGEXP}, RegExp {$ENDIF}
  ;

type
  PSortCollection = ^TSortCollection;
  TSortCollection = object(TSortedCollection)
    function Compare(Key1, Key2: Pointer): integer; virtual;
    end;

var
  BoundStart, BoundEnd: longInt;

function TSortCollection.Compare;
  var
    P1: PLongString absolute Key1;
    P2: PLongString absolute Key2;
  begin
    if P1 = nil then
      Compare := -1
    else if P2 = nil then
      Compare := 1
    else if Copy(P1^, 3, MaxLongStringLength) > Copy(P2^, 3,
        MaxLongStringLength)
    then
      Compare := 1
    else
      Compare := -1
  end;

{Added by PaSkol -> }
{$F+}
function Less(const l, R: LongString): boolean;
  begin
    Less := (l < R);
  end;

function Bigger(const l, R: LongString): boolean;
  begin
    Bigger := (l > R);
  end;
{$F-}
{ <- Added by PaSkol}

function TXFileEditor.HandleCommand;

  var
    LastX, LastY: longInt;
    t: TPoint;

  procedure ChangeLine;
    label ex;
    begin
      if not WorkModified then
        goto ex;
      while (WorkString <> '') and (WorkString[Length(WorkString)] =
          ' ')
      do
        SetLength(WorkString, Length(WorkString)-1);
      if WorkModified then
        ModifyLine(LastLine, WorkString, True);
      Modified := Modified or WorkModified;
      WorkModified := False;
ex:
      WorkString := GetLine(Delta.Y);
    end;

  procedure StartSearch(Replace: boolean);
    var
      Y: longInt;
      s: LongString;

    procedure Mix2And3(var M: word);
      const
        SwapBits = $14; { swap bits: (...X.Y..) -> (...Y.X..) }
        {Cat:  efoCaseSens       =  1;}
        {      efoWholeWords     =  2;}
        { ┌─>  efoReplacePrompt  =  4;}
        { │    efoRegExp         =  8;}
        { └─>  efoAllCP          = 16;}
      begin
        case (M and SwapBits) of
          0, SwapBits:
            else
            M := M xor SwapBits;
        end {case};
      end;

    begin { StartSearch }
      s := '';
      if (LastX < Length(WorkString)) and not (WorkString[LastX+1] in
          BreakChars)
      then
        begin
          while (LastX > 0) and
            not (WorkString[LastX] in BreakChars)
          do
            Dec(LastX);
          while not (WorkString[LastX+1] in BreakChars)
            and (LastX < Length(WorkString))
          do
            begin
              s := s+WorkString[LastX+1];
              Inc(LastX);
            end;
        end;
      SearchData.Line := s;
      if AutoScopeDetect then{-$VIV 19.05.99--}
        SearchData.Scope := word(Marking and ((Mark.A.X <> Mark.B.X) or
          (Mark.A.Y <> Mark.B.Y)));
      if not Replace then
        SearchData.What := #0
      else
        SetLength(SearchData.What, 0);
      if Replace then
        Y := ExecResource(dlgEditorReplace, SearchData)
      else
        begin
          Mix2And3(SearchData.Options);
          Y := ExecResource(dlgEditorFind, SearchData);
          Mix2And3(SearchData.Options);
        end;

      if Y = cmCancel then
        exit;
      ReplaceAll := Replace and (Y = cmYes);
      {$IFDEF REGEXP}
      if SearchData.Options and {efoRegExp}16 = 0 then
          {обычный поиск}
        {$ENDIF}
        if SearchData.Origin = 0 then
          if SearchData.Dir = 0 then
            Search(0, 0)
          else
            Search(MaxLongStringLength, FileLines^.Count-1)
        else
          Search(Delta.X, Delta.Y)
          {$IFDEF REGEXP}
      else{поиск регэкспа}
        begin
          if not RegExp^.Compile(@SearchData.Line[1], Length(
              SearchData.Line))
          then
            exit;
          Search(0, 0);
        end;
      {$ENDIF}
    end { StartSearch };

  procedure MakeUndo;
    var
      P: PUndoRec;
      s, s1: LongString;
      V, M: boolean;
      i: longInt;
      PS: PLongString;
      RPT: boolean;
      l, j: longInt;
    begin
      repeat
        RPT := True;
        ChangeLine;
        Mark.B := Mark.A;
        Marking := False;
        BlockVisible := False;
        UnMark := True;
        if (UndoInfo = nil) or (UndoInfo^.Count = 0) then
          exit;
        P := UndoInfo^.At(UndoInfo^.Count-1);
        if (P^.Where.X <> Delta.X) or (P^.Where.Y <> Delta.Y) then
          begin
            ScrollTo(P^.Where.X, P^.Where.Y);
            if P^.What <> udReplaceAll then
              exit;
          end;
        KeyMap := P^.KeyMap;
        Convert4Do(P, dkRedo); {-$VOL}
        case P^.What of
          udDelBlock:
            begin
              V := VertBlock;
              VertBlock := P^.Vertical;
              M := InsertMode;
              InsertMode := P^.InsM; {-$VOL}
              InsertBlock(P^.Lines, False);
              VertBlock := V;
              InsertMode := M; {-$VOL}
              SetLimits;
            end;
          udDelChar:
            begin
              s := GetLine(P^.Where.Y);
              if Length(s) < P^.Where.X+1 then
                s := s+LongStrg(' ', P^.Where.X-Length(s)+1);
              Insert(P^.Str^, s, P^.Where.X+1);
              ModifyLine(P^.Where.Y, s, False);
            end;
          udInsChar:
            begin
              s := GetLine(P^.Where.Y);
              Delete(s, P^.Where.X-P^.Count+1, P^.Count);
              ModifyLine(P^.Where.Y, s, False);
              ScrollTo(P^.Where.X-P^.Count, P^.Where.Y);
            end;
          udInsLine:
            begin
              ModifyLine(P^.Where.Y, CnvLongString(P^.Str), False);
              FileLines^.AtFree(P^.Where.Y+1);
              for l := 1 to 9 do
                if MarkPos[l].Y >= P^.Where.Y then
                  Dec(MarkPos[l].Y);
              SetLimits;
            end;
          udSubDel:
            ModifyLine(P^.Where.Y, CnvLongString(P^.Str), False);
          udSubDelLine:
            begin
              s := GetLine(P^.Where.Y);
              if P^.Str <> nil then
                SetLength(s, Length(s)-Length(P^.Str^));
              ModifyLine(P^.Where.Y, s, False);
              if P^.Str = nil then
                s := ''
              else
                s := P^.Str^;
              KeyMapAtInsert(P^.Where.Y+1, NewLongStr(s)); {-$VIV}
              for l := 1 to 9 do
                if MarkPos[l].Y > P^.Where.Y then
                  Inc(MarkPos[l].Y);
              SetLimits;
            end;
          udInsVertBlock:
            begin
              for i := 1 to P^.Count do
                begin
                  s := GetLine(i+Delta.Y-1);
                  Delete(s, Delta.X+1, P^.Width);
                  ModifyLine(i+Delta.Y-1, s, False);
                end;
            end;
          udInsBlock, udFormatBlock:
            begin
              s := P^.Str^;
              j := byte(s[2])*256+byte(s[1]);
              for i := 1 to j do
                FileLines^.AtFree(Delta.Y);
              for l := 1 to 9 do
                if MarkPos[l].Y >= Delta.Y then
                  Dec(MarkPos[l].Y, j);
              Delete(s, 1, 2);
              KeyMapAtInsert(Delta.Y, NewLongStr(s)); {-$VIV}
              for l := 1 to 9 do
                if MarkPos[l].Y >= Delta.Y then
                  Inc(MarkPos[l].Y);
              SetLimits;
              RPT := P^.What <> udFormatBlock;
            end;
          udReplaceChar:
            begin
              s := GetLine(P^.Where.Y);
              Delete(s, P^.Where.X+1-Length(P^.Str^), Length(P^.Str^));
              Insert(P^.Str^, s, P^.Where.X+1-Length(P^.Str^));
              ModifyLine(P^.Where.Y, s, False);
            end;
          udBackDel:
            begin
              s := LongAddSpace(GetLine(P^.Where.Y), LastX+1);
              Insert(P^.Str^, s, P^.Where.X+1);
              ModifyLine(P^.Where.Y, s, False);
              ScrollTo(P^.Where.X+Length(P^.Str^), P^.Where.Y);
            end;
          udIndentBlock:
            begin
              for i := P^.Block.A.Y to P^.Block.B.Y-byte(P^.Block.B.X =
                  0)
              do
                begin
                  s := GetLine(i);
                  Delete(s, 1, P^.Count);
                  ModifyLine(i, s, False);
                end;
            end;
          udUnindentBlock:
            begin
              for i := P^.Block.A.Y to P^.Block.B.Y-byte(P^.Block.B.X =
                  0)
              do
                begin
                  s := LongStrg(' ', P^.Count)+GetLine(i);
                  ModifyLine(i, s, False);
                end;
            end;
          udReplace, udReplaceAll:
            begin
              s := GetLine(Delta.Y);
              Delete(s, Delta.X+1, byte(P^.Str^[1]));
              Insert(Copy(P^.Str^, 2, MaxLongStringLength), s, Delta.X+1
                );
              ModifyLine(Delta.Y, s, False);
              with UndoInfo^ do
                RPT := (P^.What <> udReplaceAll) or (Count <= 1) or (
                  PUndoRec(At(Count-2))^.What <> udReplaceAll);
            end;
          udDupeLine:
            begin
              FileLines^.AtFree(P^.Where.Y+1);
              for l := 1 to 9 do
                if MarkPos[l].Y >= P^.Where.Y then
                  Dec(MarkPos[l].Y);
              SetLimits;
            end;
          udStrModified:
            begin
              if P^.Str = nil then
                s := ''
              else
                s := P^.Str^;
              ModifyLine(Delta.Y, s, False);
            end;
          udReplaceBlock, udClearBlock:
            begin
              for i := Delta.Y to P^.Lines^.Count+Delta.Y-1 do
                begin
                  s := GetLine(i);
                  PS := P^.Lines^.At(i-Delta.Y);
                  if PS <> nil then
                    s1 := PS^
                  else
                    s1 := '';
                  Delete(s, Delta.X+1, Length(s1));
                  if Length(s) < Delta.X+1 then
                    s := s+LongStrg(' ', Delta.X+1-Length(s));
                  Insert(s1, s, Delta.X+1);
                  ModifyLine(i, s, False);
                end;
            end;
          udDelLine:
            begin
              j := 0;
              while P^.Lines^.Count > 0 do
                begin
                  KeyMapAtInsert(P^.Where.Y, P^.Lines^.At(P^.Lines^.
                    Count-1)); {-$VIV}
                  P^.Lines^.AtDelete(P^.Lines^.Count-1);
                  Inc(j);
                end;
              for l := 1 to 9 do
                if MarkPos[l].Y >= P^.Where.Y then
                  Dec(MarkPos[l].Y, j);
              SetLimits;
            end;
        end {case};
        WorkModified := False;
        ChangeLine;
        UndoInfo^.AtFree(UndoInfo^.Count-1);
        Dec(UndoTimes);
        Modified := True;
        if not JustSaved and (UndoTimes = LastSaveUndoTimes) then
          begin
            Modified := False;
            if PEditWindow(Owner)^.AInfo <> nil then
              PEditWindow(Owner)^.AInfo^.DrawView;
            if PEditWindow(Owner)^.ABookLine <> nil then
              PEditWindow(Owner)^.ABookLine^.DrawView; {-$VIV}
          end;
        JustSaved := False;
      until RPT;
    end { MakeUndo };

  procedure MakeRedo; {-$VOL}
    var
      P: PUndoRec;
      s, s1: LongString;
      i: longInt;
      V, M: boolean;
      RPT: boolean;
      l, j: longInt;
    begin
      repeat
        RPT := True;
        ChangeLine;
        Mark.B := Mark.A;
        Marking := False;
        BlockVisible := False;
        UnMark := True;
        if (RedoInfo = nil) or (RedoInfo^.Count = 0) then
          exit;
        P := RedoInfo^.At(RedoInfo^.Count-1);
        if (P^.Where.X <> Delta.X) or (P^.Where.Y <> Delta.Y) then
          begin
            ScrollTo(P^.Where.X, P^.Where.Y); {Exit;}
          end;
        KeyMap := P^.KeyMap;

        Convert4Do(P, dkUndo);
        case P^.What of
          udDelChar:
            begin
              s := GetLine(P^.Where.Y);
              Delete(s, P^.Where.X-P^.Count+1, P^.Count);
              ModifyLine(P^.Where.Y, s, False);
              ScrollTo(P^.Where.X-P^.Count, P^.Where.Y);
            end;
          udInsChar:
            begin
              s := GetLine(P^.Where.Y);
              if Length(s) < P^.Where.X+1 then
                s := s+LongStrg(' ', P^.Where.X-Length(s)+1);
              if P^.Str <> nil then
                Insert(P^.Str^, s, P^.Where.X+1);
              ModifyLine(P^.Where.Y, s, False);
            end;
          udDelLine:
            begin
              for i := 1 to P^.Count do
                FileLines^.AtFree(P^.Where.Y);
              j := P^.Count;
              for l := 1 to 9 do
                if MarkPos[l].Y >= P^.Where.Y then
                  Dec(MarkPos[l].Y, j);
              SetLimits;
            end;
          udInsLine:
            begin
              if P^.Lines^.Count > 0 then
                begin
                  ModifyLine(P^.Where.Y, CnvLongString(P^.Lines^.At(0)),
                    False);
                  P^.Lines^.AtDelete(0);
                end;
              j := 0;
              while P^.Lines^.Count > 0 do
                begin
                  KeyMapAtInsert(P^.Where.Y+P^.Lines^.Count, P^.
                    Lines^.At(P^.Lines^.Count-1));
                  P^.Lines^.AtDelete(P^.Lines^.Count-1);
                  Inc(j);
                end;
              for l := 1 to 9 do
                if MarkPos[l].Y >= P^.Where.Y then
                  Dec(MarkPos[l].Y, j);
              SetLimits;
            end;
          udDelBlock:
            begin
              V := VertBlock;
              VertBlock := P^.Vertical;
              M := InsertMode;
              InsertMode := P^.InsM;
              if VertBlock then
                if InsertMode then
                  begin
                    for i := 0 to P^.Lines^.Count-1 do
                      begin
                        s := CnvLongString(P^.Lines^.At(i));
                        s1 := GetLine(P^.Where.Y+i);
                        Delete(s1, P^.Where.X+1, Length(s));
                        ModifyLine(P^.Where.Y+i, s1, False);
                      end;
                  end
                else
                  InsertBlock(P^.Lines, False)
              else
                begin
                  j := P^.Lines^.Count-1;
                  for i := 0 to j do
                    FileLines^.AtFree(P^.Where.Y);
                  Inc(j);
                  for l := 1 to 9 do
                    if MarkPos[l].Y >= P^.Where.Y then
                      Dec(MarkPos[l].Y, j);
                  if j > 0 then
                    s := CnvLongString(P^.Lines^.At(0))
                  else
                    s := '';
                  KeyMapAtInsert(P^.Where.Y, NewLongStr(s));
                  for l := 1 to 9 do
                    if MarkPos[l].Y >= P^.Where.Y then
                      Inc(MarkPos[l].Y);
                end;
              VertBlock := V;
              InsertMode := M;
              SetLimits;
            end;
          udInsBlock, udFormatBlock:
            begin
              FileLines^.AtFree(Delta.Y);
              j := -1;
              while P^.Lines^.Count > 0 do
                begin
                  KeyMapAtInsert(Delta.Y, P^.Lines^.At(P^.Lines^.
                    Count-1));
                  P^.Lines^.AtDelete(P^.Lines^.Count-1);
                  Inc(j);
                end;
              for l := 1 to 9 do
                if MarkPos[l].Y >= P^.Where.Y then
                  Inc(MarkPos[l].Y, j);
              SetLimits;
              RPT := P^.What <> udFormatBlock;
            end;
          udBackDel:
            begin
              s := GetLine(P^.Where.Y);
              s := Copy(s, 1, P^.Where.X)+Copy(s, P^.Where.X+P^.
                Count+1, MaxLongStringLength);
              ModifyLine(P^.Where.Y, s, False);
            end;
          udSubDel:
            begin
              ModifyLine(P^.Where.Y, CnvLongString(P^.Str), False);
            end;
          udSubDelLine:
            begin
              ModifyLine(P^.Where.Y, CnvLongString(P^.Str), False);
              FileLines^.AtFree(P^.Where.Y+1);
              SetLimits;
            end;
          udIndentBlock:
            begin
              for i := P^.Block.A.Y to P^.Block.B.Y-byte(P^.Block.B.X =
                  0)
              do
                begin
                  s := LongStrg(' ', P^.Count)+GetLine(i);
                  ModifyLine(i, s, False);
                end;
            end;
          udUnindentBlock:
            begin
              for i := P^.Block.A.Y to P^.Block.B.Y-byte(P^.Block.B.X =
                  0)
              do
                begin
                  s := GetLine(i);
                  Delete(s, 1, P^.Count);
                  ModifyLine(i, s, False);
                end;
            end;
          udInsVertBlock:
            begin
              V := VertBlock;
              VertBlock := P^.Vertical;
              M := InsertMode;
              InsertMode := P^.InsM;
              InsertBlock(P^.Lines, False);
              VertBlock := V;
              InsertMode := M;
              SetLimits;
            end;
          udReplace, udReplaceAll:
            begin
              s := GetLine(Delta.Y);
              Delete(s, Delta.X+1, byte(P^.Str^[1]));
              Insert(Copy(P^.Str^, 2, MaxLongStringLength), s, Delta.X+1
                );
              ModifyLine(Delta.Y, s, False);
              with RedoInfo^ do
                RPT := (P^.What <> udReplaceAll) or (Count <= 1) or (
                  PUndoRec(At(Count-2))^.What <> udReplaceAll);
            end;
          udReplaceChar:
            begin
              s := GetLine(P^.Where.Y);
              Delete(s, P^.Where.X+1, Length(P^.Str^));
              Insert(P^.Str^, s, P^.Where.X+1);
              ModifyLine(P^.Where.Y, s, False);
            end;
          udReplaceBlock, udClearBlock:
            begin
              for i := Delta.Y to P^.Lines^.Count+Delta.Y-1 do
                begin
                  s := GetLine(i);
                  s1 := CnvLongString(P^.Lines^.At(i-Delta.Y));
                  Delete(s, Delta.X+1, Length(s1));
                  if Length(s) < Delta.X+1 then
                    s := s+LongStrg(' ', Delta.X+1-Length(s));
                  Insert(s1, s, Delta.X+1);
                  ModifyLine(i, s, False);
                end;
            end;
          udStrModified:
            begin
              ModifyLine(P^.Where.Y, CnvLongString(P^.Str), False);
            end;
          udDupeLine:
            begin
              s := GetLine(P^.Where.Y);
              KeyMapAtInsert(P^.Where.Y+1, NewLongStr(s));
              for l := 1 to 9 do
                if MarkPos[l].Y >= P^.Where.Y then
                  Inc(MarkPos[l].Y);
              SetLimits;
            end;
        end {case};

        WorkModified := False;
        ChangeLine;
        RedoInfo^.AtFree(RedoInfo^.Count-1);
        Inc(UndoTimes);
        Modified := True;
        if not JustSaved and (UndoTimes = LastSaveUndoTimes) then
          begin
            Modified := False;
            if PEditWindow(Owner)^.AInfo <> nil then
              PEditWindow(Owner)^.AInfo^.DrawView;
            if PEditWindow(Owner)^.ABookLine <> nil then
              PEditWindow(Owner)^.ABookLine^.DrawView; {-$VIV}
          end;
        JustSaved := False;
      until RPT;
    end { MakeRedo }; {-$VOL}

  procedure UnIndentBlock(Undo: boolean);
    var
      i: longInt;
      s: LongString;
    begin
      Modified := True;
      if not (BlockVisible and ValidBlock) then
        exit;
      ChangeLine;
      if not UnlimitUnindent then
        for i := Mark.A.Y to Mark.B.Y-integer(Mark.B.X = 0) do
            {-$VIV 13.05.99}
          begin
            s := GetLine(i);
            if (s <> '') and (s[1] <> ' ') then
              exit;
          end;
      t.X := 0;
      t.Y := Mark.A.Y;
      if Undo then
        StoreUndoInfo(udUnindentBlock, t, Mark);
      for i := Mark.A.Y to Mark.B.Y-integer(Mark.B.X = 0) do
          {-$VIV 13.05.99}
        begin
          s := GetLine(i);
          if (s = '') or (s[1] <> ' ') then
            continue;
          Delete(s, 1, 1); {DelFC(S);}
          ModifyLine(i, s, True);
        end;
      EnableMarking := False;
      Marking := False;
      Sel := Mark;
      ScrollTo(0, Mark.A.Y);
      Mark := Sel;
      DrawView;
      ChangeLine;
      EnableMarking := True;
    end { UnIndentBlock };

  procedure IndentBlock(Undo: boolean);
    var
      i: longInt;
      s: LongString;
      M: TRect;
    begin
      Modified := True;
      if not (BlockVisible and ValidBlock) then
        exit;
      ChangeLine;
      t.X := 0;
      t.Y := Mark.A.Y;
      if Undo then
        StoreUndoInfo(udIndentBlock, t, Mark);
      for i := Mark.A.Y to Mark.B.Y-integer(Mark.B.X = 0) do
        begin
          s := ' '+GetLine(i);
          ModifyLine(i, s, True)
        end;
      EnableMarking := False;
      Marking := False;
      M := Mark;
      ScrollTo(0, Mark.A.Y);
      Mark := M;
      DrawView;
      ChangeLine;
      EnableMarking := True;
    end { IndentBlock };

  procedure NextBreak(s: LongString; var i: longInt);
    begin
      while (i <= Length(s)) and not (s[i] = ' ') do
        Inc(i);
      while (i <= Length(s)) and (s[i] = ' ') do
        Inc(i);
    end;

  function FirstToken(var s: LongString): LongString;
    var
      SS: LongString;
      i: longInt;
    begin
      FirstToken := '';
      i := 1;
      while (s <> '') and (s[Length(s)] = ' ') do
        SetLength(s, Length(s)-1);
      if s = '' then
        exit;
      while (s[1] = ' ') do
        Delete(s, 1, 1); {DelFC(S);}
      NextBreak(s, i);
      SS := Copy(s, 1, i-1);
      Delete(s, 1, i-1);
      FirstToken := SS;
    end;

  procedure FormatBlock(Command: word);
    var
      s, Left, W: LongString;
      P: PLineCollection;
      PS: PLongString;
      LS, i, j, LG: longInt;
      Abort, LastLine: boolean;
      TP: TPoint;
      Q, l: longInt;

    label 1;

    procedure WriteLeft;
      var
        i: longInt;
      begin
        while (Left <> '') and (Left[Length(Left)] = ' ') do
          SetLength(Left, Length(Left)-1);
        LongDelLeft(Left);
        case Command of
          cmFRight, cmLRight:
            begin
              j := EdOpt.RightSide-Length(Left);
              if j > 0 then
                Left := LongStrg(' ', j)+Left
              else
                Left := LongStrg(' ', EdOpt.LeftSide)+Left
            end;
          cmFLeft, cmLLeft:
            Left := LongStrg(' ', EdOpt.LeftSide)+Left;
          cmFCenter, cmLCenter:
            begin
              j := (LS-Length(Left)) div 2;
              if j < 0 then
                j := 0;
              Left := LongStrg(' ', EdOpt.LeftSide+j)+Left
            end;
          cmFJustify, cmLJustify:
            begin
              i := 1;
              j := 0;
              while (Length(Left) < LS) and (j <> i) and not
                  LastLine
              do
                begin
                  Inc(i);
                  j := i;
                  NextBreak(Left, i);
                  if i < Length(Left) then
                    Insert(' ', Left, i)
                  else if j = 2 then
                    break
                  else
                    i := 1;
                end;
              j := EdOpt.LeftSide;
              if P^.Count = 0 then
                j := EdOpt.InSide;
              Left := LongStrg(' ', j)+Left;
            end;
        end {case};
        PS := NewLongStr(Left);
        if PS = nil then
          begin
            Abort := True;
            exit
          end;
        P^.Insert(PS);
        if LowMemory then
          begin
            Abort := True;
            exit
          end;
        Left := W;
        LS := EdOpt.RightSide-EdOpt.LeftSide;
      end { WriteLeft };

    procedure CompressLine(n: integer);
      var
        MS: LongString;
      begin
        s := GetLine(n);
        LongDelDoubles('  ', s);
        if (s <> '') and (s[1] = ' ') then
          Delete(s, 1, 1); {DelFC(S);}
        if n-1 < FileLines^.Count then
          MS := GetLine(n+1)
        else
          MS := '';
        LongDelDoubles('  ', MS);
        if (MS <> '') and (MS[1] = ' ') then
          Delete(MS, 1, 1); {DelFC(MS);}
        if (n < LG) and (Length(s) < EdOpt.RightSide) and (Length(MS)+
            Length(s) < MaxLongStringLength)
        then
          begin
            s := s+' '+MS;
            Inc(i);
          end;
      end { CompressLine };

    begin { FormatBlock }
      Abort := False;
      LastLine := False;
      Modified := True;
      Q := 0;
      if not (ValidBlock and BlockVisible) or VertBlock then
        exit;
      if LowMemory then
        exit;
      ChangeLine;
      {if UndoInfo <> nil then UndoInfo^.FreeAll; Inc(UndoTimes);}
      New(P, Init((Mark.B.Y-Mark.A.Y+1)*2, 10, True));
      for i := Mark.A.Y to Mark.B.Y-integer(Mark.B.X = 0) do
        P^.Insert(NewLongStr(GetLine(i)));
      TP.Y := Mark.A.Y;
      TP.X := 0;
      StoreUndoInfo(udDelBlock, TP, P);
      New(P, Init((Mark.B.Y-Mark.A.Y+1)*2, 10, True));
      if P = nil then
        exit;
      Left := '';
      LS := EdOpt.RightSide-EdOpt.LeftSide;
      W := '';
      if (Command = cmFJustify) or (Command = cmLJustify) then
        LS := EdOpt.RightSide-EdOpt.InSide;
      LG := Mark.B.Y-integer(Mark.B.X = 0);
      i := Mark.A.Y;
      CompressLine(i);
      repeat
        W := FirstToken(s);
        if W = '' then
          begin
            Left := Left+' ';
            if i < LG then
              CompressLine(i+1);
            Inc(i);
            goto 1
          end;
        j := integer((Left <> '') and (Left[Length(Left)] = ' '));
        LongDelRight(W);
        LongDelDoubles('  ', W);
        if W[1] = ' ' then
          Delete(W, 1, 1); {DelFC(W);}
        if Length(Left)+Length(W)+j < LS then
          if j = 0 then
            Left := Left+' '+W
          else
            Left := Left+W
        else
          WriteLeft;
        if (Left <> '') and (Left[1] = ' ') then
          Delete(Left, 1, 1); {DelFC(Left);}
1:
        if Abort then
          begin
            Dispose(P, Done);
            exit
          end;
      until i > LG+1;
      LastLine := True;
      WriteLeft;
      if Abort then
        begin
          Dispose(P, Done);
          exit
        end;
      Marking := False;
      EnableMarking := False;
      Delta.Y := Mark.A.Y;
      Delta.X := 0;
      for i := Mark.A.Y to LG do
        FileLines^.AtFree(Delta.Y);
      Q := LG-Mark.A.Y;
      Mark.A.X := 0;
      Mark.B.Y := Mark.A.Y+P^.Count;
      Mark.B.X := 0;
      s := Char(Lo(P^.Count))+Char(Hi(P^.Count));
      StoreUndoInfo(udFormatBlock, TP, s);
      while P^.Count > 0 do{-$VIV}
        begin
          Dec(Q);
          KeyMapAtInsert(Delta.Y, P^.At(P^.Count-1));
          P^.AtDelete(P^.Count-1);
        end;
      if Q > 0 then
        for l := 1 to 9 do
          if MarkPos[l].Y >= Mark.A.Y then
            Dec(MarkPos[l].Y, Q);
      SetLimits;
      DrawView;
      Dispose(P, Done);
      ChangeLine;
      EnableMarking := True;
    end { FormatBlock };

  {-DataCompBoy-}
  procedure Print(Block: boolean);
    var
      s: TDOSStream;
      l: PLineCollection {PCollector}; {-SBlocks}
      {PC: PCollection;}
      M: String;
      P: PWhileView;
      i: longInt;
      R: TRect;
      E: TEvent;
      PS: PLongString;
      Cancel: boolean;
      LL: array[1..3] of longInt;
      {SR: lSearchRec;}
      FName: String;
    label 1;
    begin
      ChangeLine;
      if Block then
        begin
          {PC := GetSelection;}
          {if PC = nil then Exit;}
          {L := New(PStdCollector, Init(10));}
          {Dispose(PStdCollector(L)^.Collection, Done);}
          {PStdCollector(L)^.Collection := PC;}
          {PStdCollector(L)^.Count := PC^.Count;}
          l := PLineCollection(GetSelection); {-SBlocks}
        end
      else
        l := FileLines;
      LL[1] := l^.Count;
      FormatStr(M, GetString(dlED_PrintQuery), LL);
      if MessageBox(M, nil, mfYesNoConfirm) <> cmYes
      then
        begin
          if Block then
            Dispose(l, Done);
          exit;
        end;
      R.Assign(1, 1, 30, 10);
      New(P, Init(R));
      P^.Top := GetString(dlED_Print);
      Desktop^.Insert(P);
      ClrIO;
      Cancel := False;
      {  for I := 1 to 1000 do
     begin
       FName := SwpDir+'$DN'+SStr(I, 4, '0')+'$.PRN';
       ClrIO;
       lFindFirst(FName, Archive, SR);
       if DOSError <> 0 then Break;
       lFindClose(SR);
     end;
   lFindClose(SR);}
      FName := 'PRN'; {временно}
      s.Init(FName, stCreate);
      if s.Status <> stOK then
        begin
          s.Done;
          {Cat:warn}
          PS := @FName;
          Msg(erCantCreateFile, @PS, mfError+mfOKButton);
          goto 1;
        end;
      P^.Write(1, GetString(dlED_Printed));
      for i := 0 to l^.Count-1 do
        begin
          PS := l^.At(i);
          P^.Write(2, Copy(LongStrg(#219, ((i+1)*25) div l^.Count)+
            LongStrg(#177, 25), 1, 25));
          LL[1] := i+1;
          LL[2] := l^.Count+1;
          FormatStr(M, GetString(dlED_PrintLine), LL);
          P^.Write(3, M);
          if PS = nil then
            M := ''
          else
            M := PS^;
          M := M+#13#10;
          s.Write(M[1], Length(M));
          {DispatchEvents(P, Cancel);
     if Cancel or Abort then Goto 1;}
        end;
      if not Block then
        begin
          M := #12;
          s.Write(M[1], 1);
        end;
      s.Done;
      {Message(Application, evCommand, cmFilePrint, @FName);}
1:
      P^.Free;
      if Block then
        Dispose(l, Done);
    end { Print };
  {-DataCompBoy-}

  procedure SetFormat;
    var
      Data: record
        s1, s2, S3: String[6];
        end;
      i: longInt;
      j: integer;
    begin
      Data.s1 := ItoS(EdOpt.LeftSide);
      Data.s2 := ItoS(EdOpt.RightSide);
      Data.S3 := ItoS(EdOpt.InSide);

      if ExecResource(dlgEditorFormat, Data) <> cmCancel then
        begin
          Val(Data.s1, i, j);
          if j = 0 then
            EdOpt.LeftSide := i;
          Val(Data.s2, i, j);
          if j = 0 then
            EdOpt.RightSide := i;
          Val(Data.S3, i, j);
          if j = 0 then
            EdOpt.InSide := i;

          if (EdOpt.LeftSide > EdOpt.RightSide) or (EdOpt.LeftSide < 0)
          then
            EdOpt.LeftSide := 0;
          if EdOpt.RightSide < 2 then
            EdOpt.RightSide := 2;
          if (EdOpt.InSide >= EdOpt.RightSide) or (EdOpt.InSide < 0)
          then
            EdOpt.InSide := EdOpt.LeftSide;
        end;
    end { SetFormat };

  procedure GotoLine;
    const
      s: String = '';
    var
      i: longInt;
      j: integer;
    begin
      if ExecResource(dlgGotoLine, s) <> cmOK then
        exit;
      Val(s, i, j);
      if (j = 0) and (i > 0) then
        ScrollTo(Delta.X, i-1);
    end;

  procedure SortBlock(Reverse: boolean);
    var{PC: PSortCollection;}
      {I, J, K: Integer;
        B: Boolean;}
      s: LongString;
      Info: PView;
      tmr: TEventTimer;
      Cmpr: function (const l, R: LongString): boolean;

      {AK155 QuickSort имеет существенный недостаток: при равных
ключах он может переставлять строки. Поэтому я заменил его на
метод Шелла, который этого недостака не имеет. По быстродействию
разницы особой не будет: при числе элементов до 20000 Шелл если
и проигрывает, то не сильно.}
      (*
   procedure QuickSort(L, R: LongInt);
   var
     I, J: LongInt;
     P1, P2: Pointer; {-SBlocks}

     function Ln(AI: LongInt): LongString;
     begin
       if TimerExpired(Tmr) then
       begin
         UpdateWriteView(Info);
         Application^.Idle;
         NewTimer(Tmr, 3);
       end;
       Ln := Copy(GetLine(AI), BoundStart, BoundEnd);
     end;

   begin
     repeat
       I := L;
       J := R;
       S := Ln((L + R) shr 1);
       repeat
         while Cmpr(Ln(I),S) do Inc(I);
         while Cmpr(S,Ln(J)) do Dec(J);
         if I <= J then
         begin
          {if I <> J then FileLines^.SwapItems(I, J);}
           if I <> J then               {-SBlocks}
             begin                      {-SBlocks}
               P1 := FileLines^.At(I);  {-SBlocks}
               P2 := FileLines^.At(J);  {-SBlocks}
               FileLines^.AtPut(I, P2); {-SBlocks}
               FileLines^.AtPut(J, P1); {-SBlocks}
             end;                       {-SBlocks}
           Inc(I);
           Dec(J);
         end;
       until I > J;
       if L < J then QuickSort(L, J);
       L := I;
     until I >= R;
   end;
*)

    procedure ShellSort(l, R: longInt);
      var
        i, j, step, n: longInt;
        P1, P2: PLongString;
        X: PLongString;
      label
        1;

      function ln(AI: longInt): LongString;
        begin
          if TimerExpired(tmr) then
            begin
              UpdateWriteView(Info);
              Application^.Idle;
              NewTimer(tmr, 3);
            end;
          ln := Copy(GetLine(AI), BoundStart, BoundEnd);
        end;
      begin
        n := R-l+1;
        j := 13;
        while j < n do
          j := j*3+1;
        step := j div 3;
        while step <> 1 do
          begin
            step := step div 3;
            for i := l+step to R do
              begin
                s := ln(i);
                if Cmpr(s, ln(i-step)) then
                  begin
                    X := FileLines^.At(i);
                    j := i-step;
                    repeat
                      P1 := FileLines^.At(j); {-SBlocks}
                      FileLines^.AtPut(j+step, P1);
                      if j <= l+step-1 then
                        goto 1;
                      j := j-step;
                    until Cmpr(ln(j), s);
                    j := j+step;
1:
                    FileLines^.AtPut(j, X);
                  end;
              end;
          end;
      end { ShellSort };
    {/AK155}

    begin { SortBlock }
      if not VertBlock then
        begin
          ErrMsg(dlED_VertNeed);
          exit;
        end;
      if Reverse then
        Cmpr := Bigger
      else
        Cmpr := Less;
      Info := WriteMsg(GetString(dlPleaseStandBy));
      NewTimer(tmr, 2);
      ChangeLine;
      BoundStart := Mark.A.X+1;
      BoundEnd := Mark.B.X-Mark.A.X;
      {AK155}ShellSort(Mark.A.Y, Mark.B.Y);
      {    QuickSort(Mark.A.Y, Mark.B.Y);}
      {/AK155}
      if UndoInfo <> nil then
        UndoInfo^.FreeAll;
      Inc(UndoTimes);
      Modified := True;
      Dispose(Info, Done);
    end { SortBlock };

  procedure CalcBlock;
    var
      i: longInt;
      j: integer;
      R1, R2: Extended;
      s: LongString;
      R: real;
    begin
      if not VertBlock then
        begin
          ChangeLine;
          s := DelSpaces(Copy(GetLine(Mark.A.Y), Mark.A.X+1, Mark.B.X-
            Mark.A.X));
          if s <> '' then
            begin
              EvalueError := False; {??? в .11 этой строки нет}
              R := Evalue(s, nil);
              if not EvalueError then
                begin
                  Str(R: 0: 20, s);
                  while s[Length(s)] = '0' do
                    SetLength(s, Length(s)-1);
                  if s[Length(s)] = '.' then
                    SetLength(s, Length(s)-1);
                  Message(Application, evCommand,
                    cmPutInClipboardLong, @S);
                end;
            end;
        end
      else
        begin
          ChangeLine;
          BoundStart := Mark.A.X;
          BoundEnd := Mark.B.X-Mark.A.X;
          R1 := 0;
          for i := Mark.A.Y to Mark.B.Y do
            begin
              R2 := 0;
              Val(DelSpaces(Copy(GetLine(i), BoundStart+1, BoundEnd)),
                R2, j);
              R1 := R1+R2;
            end;
          Str(R1: 0: 20, s);
          while s[Length(s)] = '0' do
            SetLength(s, Length(s)-1);
          if s[Length(s)] = '.' then
            SetLength(s, Length(s)-1);
          Message(Application, evCommand, cmPutInClipboardLong, @S);
        end;
    end { CalcBlock };

  procedure WorkModify;
    begin
      if WorkModified then
        exit;
      LastLine := Delta.Y;
      LastShape := GetLine(LastLine);
      WorkModified := True;
      Modified := True;
      WasDelete := False;
    end;

  procedure InsertDateTime(Time: boolean);
    var
      s: String[30];
      SS: integer;
    begin
      s := GetDateTime(Time);
      ChangeLine;
      WorkModify;
      WorkString := LongAddSpace(WorkString, LastX+1);
      System.Insert(s, WorkString, LastX+1);
      for SS := 1 to Length(s) do
        begin
          StoreUndoInfo(udInsChar, Delta, WorkString);
          if Delta.X < MaxLongStringLength then
            Inc(Delta.X);
        end;
      ScrollTo(Delta.X, Delta.Y);
      ChangeLine;
      DrawView;
    end;

  procedure PlayMacro;
    var
      P: PEditMacros;

    function DoFind(P: PEditMacros): boolean;
      begin
        if UpStrg(P^.Name^) = UpCase(Event.InfoChar) then
          begin
            P^.Play(@Self);
            DoFind := True;
          end
        else
          DoFind := False;
      end;

    begin
      if Event.InfoChar < #32 then
        Inc(Event.InfoChar, 64);
      Macros^.FirstThat(@DoFind);
    end;

  procedure SelectMacro;
    begin
    end;

  procedure SearchFwd;
    var
      i: longInt;
      C: String[2];
    begin
      i := 1;
      Inc(LastX);
      case WorkString[LastX] of
        '[':
          C := '[]';
        '{':
          C := '{}';
        '(':
          C := '()';
        '<':
          C := '<>'; {-$VIV}
      end {case};
      FreeLongStr := WorkString;
      while (LastY < FileLines^.Count) do
        begin
          while LastX < Length(FreeLongStr) do
            begin
              if FreeLongStr[LastX+1] = C[1] then
                Inc(i)
              else if FreeLongStr[LastX+1] = C[2] then
                Dec(i);
              if i = 0 then
                begin
                  ScrollTo(LastX, LastY);
                  HandleCommand := True;
                  exit;
                end;
              Inc(LastX);
            end;
          Inc(LastY);
          LastX := 0;
          FreeLongStr := GetLine(LastY);
        end;
    end { SearchFwd };

  procedure SearchBwd;
    var
      i: longInt;
      C: String[2];
    begin
      i := 1;
      case WorkString[LastX+1] of
        ']':
          C := '[]';
        '}':
          C := '{}';
        ')':
          C := '()';
        '>':
          C := '<>'; {-$VIV}
      end {case};
      Dec(LastX);
      FreeLongStr := WorkString;
      while (LastY >= 0) do
        begin
          while LastX >= 0 do
            begin
              if FreeLongStr[LastX+1] = C[2] then
                Inc(i)
              else if FreeLongStr[LastX+1] = C[1] then
                Dec(i);
              if i = 0 then
                begin
                  ScrollTo(LastX, LastY);
                  HandleCommand := True;
                  exit;
                end;
              Dec(LastX);
            end;
          Dec(LastY);
          FreeLongStr := GetLine(LastY);
          LastX := Length(FreeLongStr)-1;
        end;
    end { SearchBwd };

  procedure ChangeVertBlockCase(C: word); forward;

  procedure ChangeBlockCase(C: word; Show: boolean);
    var
      i, j, k, l, SX, ex, SY, EY: longInt;
      s, s1: LongString;
      A: String[4];
      P: PCollection;
    begin
      if (Mark.A.Y > Mark.B.Y) or (Mark.A.Y = Mark.B.Y) and (Mark.A.X >=
          Mark.B.X)
      then
        exit;
      ChangeLine;
      s := GetLine(Mark.A.Y);
      while (Mark.A.X > 0) and not (s[Mark.A.X+1] in BreakChars) do
          Dec(Mark.A.X);
      SY := Mark.A.Y;
      SX := Mark.A.X+1;
      s1 := Copy(s, 1, SX-1);
      s := GetLine(Mark.B.Y);
      if Mark.B.X = 0 then
        begin
          EY := Mark.B.Y-1;
          ex := MaxLongStringLength;
        end
      else
        begin
          EY := Mark.B.Y;
          while (Mark.B.X < Length(s)) and not (s[Mark.B.X+1] in
              BreakChars)
          do
            Inc(Mark.B.X);
          ex := Mark.B.X;
        end;
      P := GetSelection;
      WorkModify;
      ChangeLine;
      StoreUndoInfo(udDelBlock, Mark.A, P);
      for i := SY to EY do
        begin
          s := GetLine(i);
          if i = SY then
            j := SX
          else
            j := 1;
          if i = EY then
            l := ex
          else
            l := Length(s);
          case C of
            cmUpcaseBlock:
              s := Copy(s, 1, j-1)+UpLongStrg(Copy(s, j, l-j+1))+
                Copy(s, l+1, MaxLongStringLength);
            cmLowcaseBlock:
              s := Copy(s, 1, j-1)+LowLongStrg(Copy(s, j, l-j+1))+
                Copy(s, l+1, MaxLongStringLength);
            cmCapitalizeBlock:
              s := Copy(s, 1, j-1)+CapLongStrg(Copy(s, j, l-j+1))+
                Copy(s, l+1, MaxLongStringLength);
            cmRusEngConv:
              s := Copy(s, 1, j-1)+RusEngConv(Copy(s, j, l-j+1))+
                Copy(s, l+1, MaxLongStringLength);
          end {case};
          ModifyLine(i, s, False);
          if i = EY then
            s := Copy(s, l+1, MaxLongStringLength);
        end;
      Insert(Char(Lo(P^.Count))+Char(Hi(P^.Count))+s1, s, 1);
      StoreUndoInfo(udFormatBlock, Mark.A, s);
      ChangeLine;
      if Show then
        DrawView;
    end { ChangeBlockCase };

  procedure ChangeVertBlockCase;
    var
      OldMark: TRect;
      i: longInt;
    begin
      OldMark := Mark;
      for i := OldMark.A.Y to OldMark.B.Y do
        begin
          Mark.Assign(OldMark.A.X, i, OldMark.B.X, i);
          ChangeBlockCase(C, False);
        end;
      Mark := OldMark;
      DrawView;
    end;

  procedure CopyWinBlock;
    var
      R: TRect;
    begin
      ChangeLine;
      if (ClipBoard <> nil) then
        Dispose(ClipBoard, Done);
      ClipBoard := GetSelection;
      SetWinClip(PLineCollection(ClipBoard));
    end;

  {-$VIV 20.05.99--}

  function WordsIsEqual(pWord1, pManyWords: LongString): boolean;
    begin
      WordsIsEqual := System.Pos(';'+UpLongStrg(pWord1)+';',
      ';'+UpLongStrg(pManyWords)+';') <> 0;
    end;

  function CurrentWordIs(pWord: LongString): boolean;
    var
      s: LongString;
      Max, i: integer;
    begin
      CurrentWordIs := False;
      s := WorkString;
      Max := Delta.X+1;
      if Max > Length(s) then
        Max := Length(s)
      else if Max = 0 then
        Max := 1;
      if (s[Max] in BreakChars) then
        exit;
      for i := Max downto 1 do
        if (s[i] in BreakChars) then
          begin
            s := Copy(s, i+1, MaxLongStringLength);
            break;
          end;
      for i := 1 to Length(s) do
        if (s[i] in BreakChars) then
          begin
            s := Copy(s, 1, i-1);
            break;
          end;
      CurrentWordIs := WordsIsEqual(s, pWord);
    end { CurrentWordIs };

  { FromPos - search start position (last search stop position)  }
  { WrdPos - first character potition in th word found           }
  { pWord - the word found                                       }
  { result - found a word ?                                      }

  function GetPrevWordViv(var FromPos, WrdPos: TPoint; var pWord:
      LongString): boolean;
    var
      s, Buf: LongString;
      WasBreak, DoStore, MakeExit: boolean;
    begin
      GetPrevWordViv := False;
      if (FromPos.Y > FileLines^.Count-1) or (FromPos.Y < 0) then
          exit;
      Buf := '';
      s := GetLine(FromPos.Y);
      MakeExit := False;
      WasBreak := False;
      DoStore := (FromPos.X >= 0) and (FromPos.X < Length(s)) and (s[
        FromPos.X+1] in BreakChars);
      repeat
        if (FromPos.X >= 0) then
          begin
            if (s[FromPos.X+1] in BreakChars) then{BreakChars}
              begin
                if Buf <> '' then
                  begin
                    pWord := Buf;
                    GetPrevWordViv := True;
                    MakeExit := True;
                    if FromPos.X < Length(s)-1 then
                      Inc(FromPos.X);
                  end;
                WasBreak := True;
              end
            else
              begin{not BreakChars}
                if WasBreak then
                  DoStore := True;
                if DoStore then
                  begin
                    Buf := s[FromPos.X+1]+Buf;
                    WrdPos := FromPos;
                  end;
                WasBreak := False;
              end;
            Dec(FromPos.X);
          end;
        if (FromPos.X < 0) then
          begin
            if Buf <> '' then
              begin
                pWord := Buf;
                GetPrevWordViv := True;
                MakeExit := True;
              end
            else
              begin
                Dec(FromPos.Y);
                FromPos.X := 0;
                WasBreak := True;
                if FromPos.Y < 0 then
                  MakeExit := True
                else if not MakeExit then
                  begin
                    s := GetLine(FromPos.Y);
                    FromPos.X := Length(s)-1;
                  end;
              end;
          end;
      until MakeExit;
    end { GetPrevWordViv };

  function GetNextWordViv(var FromPos, WrdPos: TPoint; var pWord:
      LongString): boolean;
    var
      s, Buf: LongString;
      WasBreak, DoStore, MakeExit: boolean;
    begin
      GetNextWordViv := False;
      if (FromPos.Y > FileLines^.Count-1) or (FromPos.Y < 0) then
          exit;
      Buf := '';
      s := GetLine(FromPos.Y);
      MakeExit := False;
      WasBreak := False;
      DoStore := (FromPos.X >= 0) and (FromPos.X < Length(s)) and (s[
        FromPos.X+1] in BreakChars);
      repeat
        if (FromPos.X < Length(s)) then
          begin
            if (s[FromPos.X+1] in BreakChars) then{BreakChars}
              begin
                if Buf <> '' then
                  begin
                    pWord := Buf;
                    GetNextWordViv := True;
                    MakeExit := True;
                    if FromPos.X > 0 then
                      Dec(FromPos.X);
                  end;
                WasBreak := True;
              end
            else
              begin{not BreakChars}
                if WasBreak then
                  begin
                    DoStore := True;
                    WrdPos := FromPos;
                  end;
                if DoStore then
                  Buf := Buf+s[FromPos.X+1];
                WasBreak := False;
              end;
            Inc(FromPos.X);
          end;
        if (FromPos.X >= Length(s)) then
          begin
            if Buf <> '' then
              begin
                pWord := Buf;
                GetNextWordViv := True;
                MakeExit := True;
              end
            else
              begin
                Inc(FromPos.Y);
                FromPos.X := 0;
                WasBreak := True;
                if FromPos.Y > FileLines^.Count-1 then
                  MakeExit := True
                else if not MakeExit then
                  s := GetLine(FromPos.Y);
              end;
          end;
      until MakeExit;
    end { GetNextWordViv };

  procedure SearchWordViv(Frw: boolean; pIncWord, pWord: LongString);
    var
      Pnt, Wrd: TPoint;
      Res: boolean;
      sWord: LongString;
      i, PrevLine: longInt;
      DeepLevel: longInt;
      IsDeepRange: boolean;
    begin
      Pnt := Delta;
      i := 1;
      DeepLevel := FastSearchDeep;
      IsDeepRange := (DeepLevel <> 0);
      PrevLine := Pnt.Y;
      repeat
        if Frw then
          Res := GetNextWordViv(Pnt, Wrd, sWord)
        else
          Res := GetPrevWordViv(Pnt, Wrd, sWord);
        if IsDeepRange and (PrevLine <> Pnt.Y) then
          begin
            Dec(DeepLevel);
            PrevLine := Pnt.Y;
          end;
        if Res and (pIncWord <> '') and WordsIsEqual(sWord, pIncWord)
        then
          Inc(i)
        else if Res and (pIncWord <> '') and WordsIsEqual(sWord,
            pWord)
        then
          Dec(i)
        else if Res and (pIncWord = '') and WordsIsEqual(sWord,
            pWord)
        then
          i := 0;
        if i = 0 then
          begin
            ScrollTo(Wrd.X, Wrd.Y);
            HandleCommand := True;
            break;
          end;
        if IsDeepRange and (DeepLevel <= 0) then
          begin
            if MessageBox(GetString(dlVeryDeepSearch), nil,
                mfYesNoConfirm) = cmYes
            then
              IsDeepRange := False
            else
              break;
          end;
      until (not Res);
    end { SearchWordViv };

  function CheckPairsViv(pPair1, pPair2: LongString): boolean;
    const
      ForwardDir = True;
      BackwardDir = False;
    begin
      CheckPairsViv := True;
      if CurrentWordIs(pPair1) then
        SearchWordViv(ForwardDir, pPair1, pPair2)
      else if CurrentWordIs(pPair2) then
        SearchWordViv(BackwardDir, pPair2, pPair1)
      else
        CheckPairsViv := False;
    end;

  {-$VIV--}

  begin { TXFileEditor.HandleCommand }
    LastX := Delta.X;
    LastY := Delta.Y;
    HandleCommand := False;
    case Event.Command of
      cmWindowsCopy:
        CopyWinBlock;
      cmSyncClipIn:
        begin
          SyncClipIn;
          if ClipBoardStream <> nil then
            ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-1, 0));
          CopyLines2Stream(ClipBoard, ClipBoardStream);
        end;
      cmSyncClipOut:
        SyncClipOut {(true)};
      cmRusEngConv, {-$VIV}
      cmUpcaseBlock,
      cmLowcaseBlock,
      cmCapitalizeBlock:
        if ValidBlock and BlockVisible then
          if VertBlock then
            ChangeVertBlockCase(Event.Command)
          else
            ChangeBlockCase(Event.Command, True);
      cmPlayMacro:
        PlayMacro;
      cmSelectMacro:
        SelectMacro;
      cmSetMargins:
        SetFormat;
      cmGotoLineNumber:
        GotoLine;
      cmGotoLineNumber2:
        if longInt(Event.InfoPtr) > 0 then
          begin
            ScrollTo(Delta.X, longInt(Event.InfoPtr)+20);
            ScrollTo(Delta.X, longInt(Event.InfoPtr)-1);
          end;
      cmRevSortBlock:
        begin
          if ValidBlock then
            SortBlock(True);
          HandleCommand := True;
        end;
      cmSortBlock:
        begin
          if ValidBlock then
            SortBlock(False);
          HandleCommand := True;
        end;
      cmCalcBlock:
        begin
          if ValidBlock then
            CalcBlock;
          HandleCommand := True;
        end;
      cmInsertDate, cmInsertTime:
        InsertDateTime(Event.Command = cmInsertTime);
      cmStartSearch:
        begin
          ChangeLine;
          StartSearch(False);
          ChangeLine;
        end;
      cmReplace:
        begin
          ChangeLine;
          StartSearch(True);
          ChangeLine;
        end;
      cmUndo:
        begin
          MakeUndo;
          HandleCommand := True
        end;
      cmRedo:
        begin
          MakeRedo;
          HandleCommand := True
        end; {-$VOL}
      cmIndentBlock:
        begin
          UnMark := True;
          IndentBlock(True)
        end;
      cmUnIndentBlock:
        begin
          UnMark := True;
          UnIndentBlock(True)
        end;
      cmFRight, cmFLeft, cmFJustify, cmFCenter:
        begin
          FormatBlock(Event.Command)
        end;
      cmLRight, cmLLeft, cmLJustify, cmLCenter:
        begin
          ChangeLine;
          Mark.A := Delta;
          Mark.B := Delta;
          Mark.A.X := 0;
          Mark.B.X := Length(GetLine(Delta.Y));
          Sel := Mark;
          BlockVisible := True;
          FormatBlock(Event.Command)
        end;
      cmViewFile:
        if Valid(cmClose) and (EditName <> '') then
          begin
            if Desktop <> nil then
              Desktop^.Lock;
            if Application <> nil then
              begin
                {AK155}Application^.InsertWindow(New(PFileWindow,
                Init(EditName, EditName, False)));
                Message(Application, evCommand, cmViewText,
                @EditName);
                MessageL(Application, evCommand, cmScrollBarChanged,
                GetOffsetForLineNumber(EditName, Pos.Y+1));
              end;
            ClearEvent(Event);
            Message(Owner, evCommand, cmClose, nil);
            if Desktop <> nil then
              Desktop^.UnLock;
            exit;
          end;
      cmPrintBlock:
        Print(True);
      cmPrintFileEd:
        Print(False);
      cmBracketPair:
        begin
          ChangeLine;
          if (LastX < Length(WorkString)) then
            if (WorkString[LastX+1] in ['[', '{', '(', '<']) then
                SearchFwd
            else if (WorkString[LastX+1] in [')', '}', ']', '>'])
            then
              SearchBwd
              {-$VIV}
            else if CheckPairsViv('begi'+'n;ca'+'se;a'+'sm', 'e'+
                'nd')
            then{no action}
            else if CheckPairsViv('repeat', 'until') then{no action}
            else if CheckPairsViv('if', 'then') then{no action}
              ;
          {-$VIV::}
        end;
      cmSwitchKeyMapping:{-$VIV}
        begin
          if WorkModified then
            begin
              ModifyLine(LastLine, WorkString, True);
              Modified := Modified or WorkModified;
              WorkModified := False;
            end;
          case KeyMap of
            kmAscii:
              KeyMap := kmAnsi;
            kmAnsi:
              if Koi8rKeyMap then
                KeyMap := kmKoi8r
              else
                KeyMap := kmAscii;
            kmKoi8r:
              KeyMap := kmAscii;
            else
              KeyMap := kmAscii;
          end {case};
          WorkString := GetLine(Delta.Y);
          DrawView;
        end;
    end {case};
  end { TXFileEditor.HandleCommand };

end.
