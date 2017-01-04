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
//  dn16rc1-Undo_after_save_diff132byMV.patch
//  dn16rc1-Editor_search_fix.patch
//
//  2.0.0
//  dn200-Replace_QuickSort_with_ShellSort.patch
//  dn200-editor_calculate_selected_block.patch
//
//  2.3.0
//  dn281-enable_QuickSort_for_RealMode.patch
//  dn2811_editor_calculate_sum_fix.patch
//  dn2811_editor_selected_block_operation_fix.patch
//  dn2825-new_logic_for_viewers.patch
//  dn328-Editor(f)-insert_date_time_overwrite_selected_block.patch
//
//  3.7.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//  dn31029-Kernel(fi)-text_search_operation_rewritten.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Editor;

interface
uses LFN, Views, Objects, Drivers, MicroEd, U_KeyMap, collect, ed2;

type
     PXFileEditor = ^TXFileEditor;
     TXFileEditor = object(TFileEditor)
       function HandleCommand(var Event: TEvent): Boolean; virtual;
     end;

implementation
uses advance, advance1, advance2, advance3, DOS, Dialogs, DNApp, RStrings,
     SBlocks, Memory, Gauge, Startup, WinClp, Messages, Commands, Macro,
     EdWin, xTime, DnIni, DNUtil, Advance6, calculat;

type
     PSortCollection = ^TSortCollection;
     TSortCollection = object(TSortedCollection)
       function Compare(Key1, Key2: Pointer): Integer; virtual;
     end;

var BoundStart, BoundEnd: Word;

function TSortCollection.Compare;
  var P1: PString absolute Key1;
      P2: PString absolute Key2;
begin
   if P1 = nil then Compare := -1 else
     if P2 = nil then Compare := 1 else
       if Copy(P1^, 3, 255) > Copy(P2^, 3, 255) then Compare := 1
          else Compare := -1
end;


{Added by PaSkol -> }
{$F+}
function Less(const L,R:String):Boolean;
 begin
  Less:=(L<R);
 end;

function Bigger(const L,R:String):Boolean;
 begin
  Bigger:=(L>R);
 end;
{$F-}
{ <- Added by PaSkol}

function TXFileEditor.HandleCommand;

var  LastX, LastY: LongInt;
     WL: Byte absolute WorkString;
     T: TPoint;

  procedure ChangeLine;
   label Ex;
  begin
    if not WorkModified then goto Ex;
    While WorkString[WL] = ' ' do Dec(WL);
    if WorkModified then ModifyLine(LastLine, WorkString, True);
    Modified := Modified or WorkModified;
    WorkModified := False;
  Ex:
    WorkString := GetLine(Delta.Y);
  end;

  procedure StartSearch(Replace: Boolean);
   var Y: LongInt;
       S: String;
   procedure Mix2And3(var M: AWord);
   begin
     Case (M and $C) of
       0,$C:
       else M := M xor $C;
     end;
   end;
  begin
   S := '';
   if not (WorkString[LastX + 1] in BreakChars) then
       begin
         While (LastX > 0) and
               not (WorkString[LastX] in BreakChars) do Dec(LastX);
         While not (WorkString[LastX + 1] in BreakChars)
                     and (LastX < WL) do
                      begin
                       S := S + WorkString[LastX + 1];
                       Inc(LastX);
                      end;
       end;
   SearchData.Line := S;
   if AutoScopeDetect then {-$VIV 19.05.99--}
     SearchData.Scope := Word(Marking and ((Mark.A.X <> Mark.B.X) or (Mark.A.Y <> Mark.B.Y)));
   if not Replace then SearchData.What := #0
                  else SearchData.What[0] := #0;
   if Replace then
     Y := ExecResource(dlgEditorReplace, SearchData)
   else begin
    Mix2And3(SearchData.Options);
    Y := ExecResource(dlgEditorFind, SearchData);
    Mix2And3(SearchData.Options);
   end;

   if Y = cmCancel then Exit;
   ReplaceAll := Replace and (Y = cmYes);
   if SearchData.Origin = 0 then
      if SearchData.Dir = 0 then
        Search(0, 0)
      else Search (255, FileLines^.Count - 1)
   else Search(Delta.X, Delta.Y);
  end;

  procedure MakeUndo;
   var P: PUndoRec;
       S, S1: String;
       V, M: Boolean;
       I: LongInt;
       PS: PString;
       RPT: Boolean;
       L, J: Longint;
  begin
   repeat
     RPT := True;
     ChangeLine;
     Mark.B := Mark.A; Marking := False; BlockVisible := False; UnMark := True;
     if (UndoInfo = nil) or (UndoInfo^.Count = 0) then Exit;
     P := UndoInfo^.At(UndoInfo^.Count - 1);
     if (P^.Where.X <> Delta.X) or (P^.Where.Y <> Delta.Y) then
      begin ScrollTo(P^.Where.X, P^.Where.Y);
     if P^.What<>udReplaceAll then Exit; end;
     KeyMap := P^.KeyMap;
     Convert4Do(P,dkRedo); {-$VOL}
     case P^.What of
      udDelBlock: begin
                   V := VertBlock; VertBlock := P^.Vertical;
                   M := InsertMode; InsertMode := P^.InsM; {-$VOL}
                   InsertBlock(P^.Lines, False);
                   VertBlock := V;
                   InsertMode := M; {-$VOL}
                   SetLimits;
                  end;
      udDelChar: begin
                  S := GetLine(P^.Where.Y);
                  if Byte(S[0]) < P^.Where.X + 1 then S := S + Strg(' ', P^.Where.X - Byte(S[0]) + 1);
                  Insert(P^.Str^, S, P^.Where.X + 1);
                  ModifyLine(P^.Where.Y, S, False);
                 end;
      udInsChar: begin
                  S := GetLine(P^.Where.Y);
                  Delete(S, P^.Where.X - P^.Count + 1, P^.Count);
                  ModifyLine(P^.Where.Y, S, False);
                  ScrollTo(P^.Where.X - P^.Count, P^.Where.Y);
                 end;
      udInsLine: begin
                  ModifyLine(P^.Where.Y, CnvString(P^.Str), False);
                  FileLines^.AtFree(P^.Where.Y + 1);
                  for L:=1 to 9 do if MarkPos[L].Y >= P^.Where.Y then Dec(MarkPos[L].Y);
                  SetLimits;
                 end;
      udSubDel: ModifyLine(P^.Where.Y, CnvString(P^.Str), False);
      udSubDelLine: begin
                      S := GetLine(P^.Where.Y);
                      if P^.Str <> nil then Dec(S[0], Byte(P^.Str^[0]));
                      ModifyLine(P^.Where.Y, S, False);
                      if P^.Str = nil then S := '' else S := P^.Str^;
                      KeyMapAtInsert(P^.Where.Y+1, NewStr(S)); {-$VIV}
                      for L:=1 to 9 do if MarkPos[L].Y > P^.Where.Y then Inc(MarkPos[L].Y);
                      SetLimits;
                    end;
      udInsVertBlock: begin
                       for I:=1 to P^.Count do
                       begin
                        S := GetLine(I+Delta.Y-1);
                        Delete(S, Delta.X+1, P^.Width);
                        ModifyLine(I+Delta.Y-1, S, False);
                       end;
                      end;
      udInsBlock, udFormatBlock: begin
                   S := P^.Str^;
                   J := Byte(S[2])*256 + Byte(S[1]);
                   for I := 1 to J do FileLines^.AtFree(Delta.Y);
                   for L := 1 to 9 do if MarkPos[L].Y >= Delta.Y then Dec(MarkPos[L].Y, J);
                   Delete(S, 1, 2);
                   KeyMapAtInsert(Delta.Y, NewStr(S)); {-$VIV}
                   for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Inc(MarkPos[L].Y);
                   SetLimits;
                   RPT := P^.What <> udFormatBlock;
                  end;
      udReplaceChar: begin
                      S := GetLine(P^.Where.Y);
                      Delete(S, P^.Where.X + 1 - Byte(P^.Str^[0]), Byte(P^.Str^[0]));
                      Insert(P^.Str^, S, P^.Where.X + 1 - Byte(P^.Str^[0]));
                      ModifyLine(P^.Where.Y, S, False);
                     end;
      udBackDel: begin
                      S := AddSpace(GetLine(P^.Where.Y), LastX+1);
                      Insert(P^.Str^, S, P^.Where.X + 1);
                      ModifyLine(P^.Where.Y, S, False);
                      ScrollTo(P^.Where.X + Byte(P^.Str^[0]), P^.Where.Y);
                 end;
      udIndentBlock: begin
                      for I := P^.Block.A.Y to P^.Block.B.Y-Byte(P^.Block.B.X = 0) do
                       begin
                        S := GetLine(I); Delete(S, 1, P^.Count);
                        ModifyLine(I, S, False);
                       end;
                     end;
      udUnindentBlock: begin
                      for I := P^.Block.A.Y to P^.Block.B.Y-Byte(P^.Block.B.X = 0) do
                       begin
                        S := Strg(' ', P^.Count)+GetLine(I);
                        ModifyLine(I, S, False);
                       end;
                     end;
      udReplace, udReplaceAll: begin
                  S := GetLine(Delta.Y);
                  Delete(S, Delta.X+1, Byte(P^.Str^[1]));
                  Insert(Copy(P^.Str^, 2, 255), S, Delta.X+1);
                  ModifyLine(Delta.Y, S, False);
                  with UndoInfo^ do
                    RPT:=(P^.What<>udReplaceAll)or(Count<=1)or(PUndoRec(At(Count-2))^.What<>udReplaceAll);
                 end;
      udDupeLine: begin
                    FileLines^.AtFree(P^.Where.Y + 1);
                    for L:=1 to 9 do if MarkPos[L].Y >= P^.Where.Y then Dec(MarkPos[L].Y);
                    SetLimits;
                  end;
      udStrModified: begin
                       if P^.Str = nil then S := '' else S := P^.Str^;
                       ModifyLine(Delta.Y, S, False);
                     end;
      udReplaceBlock, udClearBlock:
                 begin
                  for I := Delta.Y to P^.Lines^.Count + Delta.Y - 1 do
                   begin
                    S := GetLine(I); PS := P^.Lines^.At(I-Delta.Y);
                    if PS <> nil then S1 := PS^ else S1 := '';
                    Delete(S, Delta.X+1, Length(S1));
                    if Length(S) < Delta.X + 1 then
                       S := S + Strg(' ', Delta.X + 1 - Length(S));
                    Insert(S1, S, Delta.X+1);
                    ModifyLine(I, S, False);
                   end;
                 end;
      udDelLine: begin
                  J:=0;
                  While P^.Lines^.Count > 0 do
                   begin
                    KeyMapAtInsert(P^.Where.Y, P^.Lines^.At(P^.Lines^.Count - 1)); {-$VIV}
                    P^.Lines^.AtDelete(P^.Lines^.Count-1);
                    Inc(J);
                   end;
                  for L:=1 to 9 do if MarkPos[L].Y >= P^.Where.Y then Dec(MarkPos[L].Y, J);
                  SetLimits;
                 end;
     end;
     WorkModified := False;
     ChangeLine;
     UndoInfo^.AtFree(UndoInfo^.Count - 1);
     Dec(UndoTimes);
     Modified := True;
     if not JustSaved and (UndoTimes = LastSaveUndoTimes) then
       begin
          Modified := False;
          if PEditWindow(Owner)^.AInfo <> nil then PEditWindow(Owner)^.AInfo^.DrawView;
          if PEditWindow(Owner)^.ABookLine <> nil then PEditWindow(Owner)^.ABookLine^.DrawView; {-$VIV}
       end;
     JustSaved := False;
   until RPT;
  end;

  procedure MakeRedo; {-$VOL}
   var P: PUndoRec;
       S, S1: String;
       I: LongInt;
       V, M: Boolean;
       RPT: Boolean;
       L, J: Longint;
  begin
   repeat
     RPT := True;
     ChangeLine;
     Mark.B := Mark.A; Marking := False; BlockVisible := False; UnMark := True;
     if (RedoInfo = nil) or (RedoInfo^.Count = 0) then Exit;
     P := RedoInfo^.At(RedoInfo^.Count - 1);
     if (P^.Where.X <> Delta.X) or (P^.Where.Y <> Delta.Y) then
      begin ScrollTo(P^.Where.X, P^.Where.Y); {Exit;} end;
     KeyMap := P^.KeyMap;

     Convert4Do(P,dkUndo);
     case P^.What of
       udDelChar:
          begin
            S := GetLine(P^.Where.Y);
            Delete(S, P^.Where.X - P^.Count + 1, P^.Count);
            ModifyLine(P^.Where.Y, S, False);
            ScrollTo(P^.Where.X - P^.Count, P^.Where.Y);
          end;
       udInsChar:
          begin
            S := GetLine(P^.Where.Y);
            if Byte(S[0]) < P^.Where.X + 1 then S := S + Strg(' ', P^.Where.X - Byte(S[0]) + 1);
            Insert(P^.Str^, S, P^.Where.X + 1);
            ModifyLine(P^.Where.Y, S, False);
          end;
       udDelLine:
          begin
            for I:=1 to P^.Count do FileLines^.AtFree(P^.Where.Y);
            J := P^.Count;
            for L:=1 to 9 do if MarkPos[L].Y >= P^.Where.Y then Dec(MarkPos[L].Y, J);
            SetLimits;
          end;
       udInsLine:
          begin
            if P^.Lines^.Count>0 then
              begin
                ModifyLine(P^.Where.Y,CnvString(P^.Lines^.At(0)),False);
                P^.Lines^.AtDelete(0);
              end;
            J:=0;
            while P^.Lines^.Count>0 do
              begin
                KeyMapAtInsert(P^.Where.Y+P^.Lines^.Count,P^.Lines^.At(P^.Lines^.Count-1));
                P^.Lines^.AtDelete(P^.Lines^.Count-1);
                Inc(J);
              end;
            for L:=1 to 9 do if MarkPos[L].Y >= P^.Where.Y then Dec(MarkPos[L].Y, J);
            SetLimits;
          end;
       udDelBlock:
          begin
            V:=VertBlock; VertBlock:=P^.Vertical;
            M:=InsertMode; InsertMode:=P^.InsM;
            if VertBlock then
              if InsertMode then
                begin
                  for I:=0 to P^.Lines^.Count-1 do
                    begin
                      S:=CnvString(P^.Lines^.At(I));
                      S1:=GetLine(P^.Where.Y+I);
                      Delete(S1,P^.Where.X+1,byte(S[0]));
                      ModifyLine(P^.Where.Y+I,S1,False);
                    end;
                end
              else
                InsertBlock(P^.Lines, False)
            else
              begin
                J:=P^.Lines^.Count-1;
                for I:=0 to J do
                  FileLines^.AtFree(P^.Where.Y);
                Inc(J);
                for L:=1 to 9 do if MarkPos[L].Y >= P^.Where.Y then Dec(MarkPos[L].Y, J);
                if J>0 then
                  S:=CnvString(P^.Lines^.At(0))
                else S:='';
                KeyMapAtInsert(P^.Where.Y,NewStr(S));
                for L:=1 to 9 do if MarkPos[L].Y >= P^.Where.Y then Inc(MarkPos[L].Y);
              end;
            VertBlock:=V; InsertMode:=M;
            SetLimits;
          end;
       udInsBlock, udFormatBlock:
          begin
            FileLines^.AtFree(Delta.Y);
            J:=-1;
            while P^.Lines^.Count>0 do
              begin
                KeyMapAtInsert(Delta.Y,P^.Lines^.At(P^.Lines^.Count-1));
                P^.Lines^.AtDelete(P^.Lines^.Count-1);
                Inc(J);
              end;
            for L:=1 to 9 do if MarkPos[L].Y >= P^.Where.Y then Inc(MarkPos[L].Y, J);
            SetLimits;
            RPT := P^.What <> udFormatBlock;
          end;
       udBackDel:
          begin
            S:=GetLine(P^.Where.Y);
            S:=copy(S,1,P^.Where.X)+copy(S,P^.Where.X+P^.Count+1,255);
            ModifyLine(P^.Where.Y, S, False);
          end;
       udSubDel:
          begin
            ModifyLine(P^.Where.Y, CnvString(P^.Str), False);
          end;
       udSubDelLine:
          begin
            ModifyLine(P^.Where.Y,CnvString(P^.Str),False);
            FileLines^.AtFree(P^.Where.Y+1);
            SetLimits;
          end;
       udIndentBlock:
          begin
            for I := P^.Block.A.Y to P^.Block.B.Y-Byte(P^.Block.B.X = 0) do
              begin
                S := Strg(' ', P^.Count)+GetLine(I);
                ModifyLine(I, S, False);
              end;
          end;
       udUnindentBlock:
          begin
            for I := P^.Block.A.Y to P^.Block.B.Y-Byte(P^.Block.B.X = 0) do
              begin
                S := GetLine(I); Delete(S, 1, P^.Count);
                ModifyLine(I, S, False);
              end;
          end;
       udInsVertBlock:
          begin
            V:=VertBlock; VertBlock:=P^.Vertical;
            M:=InsertMode; InsertMode:=P^.InsM;
            InsertBlock(P^.Lines, False);
            VertBlock:=V; InsertMode:=M;
            SetLimits;
          end;
       udReplace,udReplaceAll:
          begin
            S := GetLine(Delta.Y);
            Delete(S, Delta.X+1, Byte(P^.Str^[1]));
            Insert(Copy(P^.Str^, 2, 255), S, Delta.X+1);
            ModifyLine(Delta.Y, S, False);
            with RedoInfo^ do
              RPT:=(P^.What<>udReplaceAll)or(Count<=1)or(PUndoRec(At(Count-2))^.What<>udReplaceAll);
          end;
       udReplaceChar:
          begin
            S := GetLine(P^.Where.Y);
            Delete(S, P^.Where.X + 1, Byte(P^.Str^[0]));
            Insert(P^.Str^, S, P^.Where.X + 1);
            ModifyLine(P^.Where.Y, S, False);
          end;
       udReplaceBlock,udClearBlock:
          begin
            for I := Delta.Y to P^.Lines^.Count + Delta.Y - 1 do
             begin
              S := GetLine(I);
              S1 := CnvString(P^.Lines^.At(I-Delta.Y));
              Delete(S, Delta.X+1, Length(S1));
              if Length(S) < Delta.X + 1 then
                 S := S + Strg(' ', Delta.X + 1 - Length(S));
              Insert(S1, S, Delta.X+1);
              ModifyLine(I, S, False);
             end;
          end;
       udStrModified:
          begin
            ModifyLine(P^.Where.Y, CnvString(P^.Str), False);
          end;
       udDupeLine:
          begin
            S:=GetLine(P^.Where.Y);
            KeyMapAtInsert(P^.Where.Y+1,NewStr(S));
            for L:=1 to 9 do if MarkPos[L].Y >= P^.Where.Y then Inc(MarkPos[L].Y);
            SetLimits;
          end;
     end;

     WorkModified := False;
     ChangeLine;
     RedoInfo^.AtFree(RedoInfo^.Count - 1);
     Inc(UndoTimes);
     Modified := True;
     if not JustSaved and (UndoTimes = LastSaveUndoTimes) then
       begin
          Modified := False;
          if PEditWindow(Owner)^.AInfo <> nil then PEditWindow(Owner)^.AInfo^.DrawView;
          if PEditWindow(Owner)^.ABookLine <> nil then PEditWindow(Owner)^.ABookLine^.DrawView; {-$VIV}
       end;
     JustSaved := False;
   until RPT;
  end; {-$VOL}

  procedure UnIndentBlock(Undo: Boolean);
   var I: LongInt;
       S: String;
  begin
   Modified := True;
   if not (BlockVisible and ValidBlock) then Exit;
   ChangeLine;
   for I := Mark.A.Y to Mark.B.Y-Integer(Mark.B.X = 0) do {-$VIV 13.05.99}
   begin
     S[1] := ' ';
     S := GetLine(I);
     if (S[1] <> ' ') and (not UnlimitUnindent) then Exit;
   end;
   T.X := 0; T.Y := Mark.A.Y;
   if Undo then StoreUndoInfo(udUnindentBlock, T, Mark);
   for I := Mark.A.Y to Mark.B.Y-Integer(Mark.B.X = 0) do {-$VIV 13.05.99}
   begin
     S := GetLine(I);
     if (Length(S) = 0) or (S[1] <> ' ') then Continue;
     DelFC(S);
     ModifyLine(I, S, True);
   end;
   EnableMarking := False; Marking := False; Sel := Mark;
   ScrollTo(0, Mark.A.Y); Mark := Sel; DrawView; ChangeLine;
   EnableMarking := True;
  end;

  procedure IndentBlock(Undo: Boolean);
   var I: LongInt;
       S: String;
       M: TRect;
  begin
   Modified := True;
   if not (BlockVisible and ValidBlock) then Exit;
   ChangeLine;
   T.X := 0; T.Y := Mark.A.Y;
   if Undo then StoreUndoInfo(udIndentBlock, T, Mark);
   for I := Mark.A.Y to Mark.B.Y-Integer(Mark.B.X = 0) do
    begin S := ' ' + GetLine(I); ModifyLine(I, S, True) end;
   EnableMarking := False; Marking := False; M := Mark;
   ScrollTo(0, Mark.A.Y); Mark := M; DrawView; ChangeLine;
   EnableMarking := True;
  end;

  procedure NextBreak(S: String; var I: LongInt);
   var L: Byte absolute S;
  begin
   While (I<=L) and not (S[I] = ' ') do Inc(I);
   While (I<=L) and (S[I] = ' ') do Inc(I);
  end;

  function FirstToken(var S: String): String;
   var SS: String;
       I: LongInt;
       L: Byte absolute S;
  begin
   FirstToken := '';
   I := 1; While (S[L] = ' ') do Dec(L); if S = '' then Exit;
   While (S[1]=' ') do DelFC(S);
   NextBreak(S, I);
   SS := Copy(S, 1, I-1); Delete(S, 1, I-1);
   FirstToken := SS;
  end;

  procedure FormatBlock(Command: Word);
   var S, Left, W: String;
       P: PLineCollection;
       PS: PString;
       LS, I, J, LG: LongInt;
       Abort, LastLine: Boolean;
       TP: TPoint;
       Q, L: Longint;

   label 1;

   procedure WriteLeft;
    var I: LongInt;
        L: Byte absolute Left;
   begin
    while Left[Byte(Left[0])]=' ' do Dec(Left[0]);
    DelLeft(Left);
    case Command of
     cmFRight, cmLRight:
       begin J := EdOpt.RightSide - Byte(Left[0]);
             if J > 0 then Left := Strg(' ', J) + Left
                      else Left := Strg(' ', EdOpt.LeftSide)+Left end;
     cmFLeft, cmLLeft: Left := Strg(' ', EdOpt.LeftSide) + Left;
     cmFCenter, cmLCenter: begin J := (LS - Byte(Left[0])) div 2; if J < 0 then J := 0;
                      Left := Strg(' ', EdOpt.LeftSide + J)+Left end;
     cmFJustify, cmLJustify: begin
                  I := 1; J := 0;
                  While (L < LS) and (J <> I) and not LastLine do
                   begin Inc(I); J := I; NextBreak(Left, I);
                         if I < L then Insert(' ', Left, I) else
                          if J = 2 then Break else I := 1;
                   end;
                  J := EdOpt.LeftSide; if P^.Count = 0 then J := EdOpt.InSide;
                  Left := Strg(' ', J) + Left;
                 end;
    end;
    PS := NewStr(Left); if PS = nil then begin Abort := True; Exit end;
    P^.Insert(PS); if LowMemory then begin Abort := True; Exit end;
    Left := W; LS := EdOpt.RightSide - EdOpt.LeftSide;
   end;

   procedure CompressLine(N: Integer);
    var MS: String;
   begin
    S := GetLine(N); DelDoubles('  ', S);
    if S[1] = ' ' then DelFC(S);
    if N - 1 < FileLines^.Count then MS := GetLine(N+1) else MS := '';
    DelDoubles('  ', MS); if MS[1] = ' ' then DelFC(MS);
    if (N < LG) and (Byte(S[0]) < EdOpt.RightSide) and (Byte(MS[0]) + Byte(S[0]) < 254) then
     begin S := S + ' ' + MS; Inc(I); end;
   end;

  begin
   Abort := False; LastLine := False; Modified := True; Q:=0;
   if not (ValidBlock and BlockVisible) or VertBlock then Exit;
   if LowMemory then Exit; ChangeLine;
   {if UndoInfo <> nil then UndoInfo^.FreeAll; Inc(UndoTimes);}
   New(P, Init((Mark.B.Y-Mark.A.Y+1)*2,10));
   for I := Mark.A.Y to Mark.B.Y - Integer(Mark.B.X = 0) do
       P^.Insert(NewStr(GetLine(I)));
   TP.Y := Mark.A.Y; TP.X := 0;
   StoreUndoInfo(udDelBlock, TP, P);
   New(P, Init((Mark.B.Y-Mark.A.Y+1)*2,10));
   if P = nil then Exit; Left := ''; LS := EdOpt.RightSide - EdOpt.LeftSide; W := '';
   if (Command = cmFJustify) or (Command = cmLJustify) then LS := EdOpt.RightSide - EdOpt.InSide;
   LG := Mark.B.Y - Integer(Mark.B.X = 0);
   I := Mark.A.Y; CompressLine(I);
   repeat
    W := FirstToken(S);
    if W = '' then
     begin Left := Left+' '; if I < LG then CompressLine(I+1); Inc(I); GoTo 1 end;
    J := Integer(Left[Length(Left)] = ' ');
    DelRight(W);
    DelDoubles('  ', W);
    if W[1] = ' ' then DelFC(W);
    if Length(Left)+ Length(W) + J < LS then
      if J = 0 then Left := Left+' '+W else Left := Left+W
      else WriteLeft;
    if Left[1] = ' ' then DelFC(Left);
  1:
   if Abort then begin Dispose(P,Done); Exit end;
   until I > LG+1;
   LastLine := True; WriteLeft;
   if Abort then begin Dispose(P,Done); Exit end;
   Marking := False; EnableMarking := False;
   Delta.Y := Mark.A.Y; Delta.X := 0;
   for I := Mark.A.Y to LG do FileLines^.AtFree(Delta.Y);
   Q := LG - Mark.A.Y;
   Mark.A.X := 0; Mark.B.Y := Mark.A.Y + P^.Count; Mark.B.X := 0;
   S := Char(Lo(P^.Count)) + Char(Hi(P^.Count));
   StoreUndoInfo(udFormatBlock, TP, S);
   while P^.Count > 0 do {-$VIV}
    begin Dec(Q); KeyMapAtInsert(Delta.Y, P^.At(P^.Count-1)); P^.AtDelete(P^.Count-1); end;
   If Q > 0 then
    for L:=1 to 9 do if MarkPos[L].Y >= Mark.A.Y then Dec(MarkPos[L].Y, Q);
   SetLimits; DrawView; Dispose(P,Done); ChangeLine;
   EnableMarking := True;
  end;

        {-DataCompBoy-}
  {$IFDEF PrintManager}
  procedure Print(Block: Boolean);
   var S: TDOSStream;
       L: PCollector;
       PC: PCollection;
       M: String;
       P: PWhileView;
       I: LongInt;
       R: TRect;
       E: TEvent;
       PS: PString;
       Cancel: Boolean;
       LL: Array [1..3] of LongInt;
       SR: lSearchRec;
       FName: String;
   label 1;
  begin
   ChangeLine;
   if Block then
     begin
       PC := GetSelection;
       if PC = nil then Exit;
       L := New(PStdCollector, Init(10));
       Dispose(PStdCollector(L)^.Collection,Done);
       PStdCollector(L)^.Collection := PC;
       PStdCollector(L)^.Count := PC^.Count;
     end else L := FileLines;
   LL[1] := L^.Count;
   FormatStr(M, GetString(dlED_PrintQuery), LL);
   if MessageBox(M, nil, mfYesNoConfirm) <> cmYes
    then begin if Block then Dispose(L,Done); Exit; end;
   R.Assign(1,1,30,10);
   New(P,Init(R));
   P^.Top := GetString(dlED_Print);
   Desktop^.Insert(P);
   ClrIO; Cancel := False;
   for I := 1 to 1000 do
     begin
       FName := SwpDir+'$DN'+SStr(I, 4, '0')+'$.PRN';
       ClrIO;
       lFindFirst(FName, Archive, SR);
       if DOSError <> 0 then Break;
       lFindClose(SR);
     end;
   lFindClose(SR);
   S.Init(FName, stCreate);
   if S.Status <> stOK then
     begin
       S.Done;
       PS := @FName;
       Msg(erCantCreateFile, @PS, mfError+mfOKButton);
       Goto 1;
     end;
   P^.Write(1, GetString(dlED_Printed));
   for I := 0 to L^.Count - 1 do
    begin
     PS := L^.At(I);
     P^.Write(2, Copy(Strg(#219, ((I+1)*25) div L^.Count)+Strg(#177, 25), 1, 25));
     LL[1] := I+1; LL[2] := L^.Count+1;
     FormatStr(M, GetString(dlED_PrintLine), LL);
     P^.Write(3, M);
     if PS = nil then M := '' else M := PS^;
     M := M + #13#10;
     S.Write(M[1],Length(M));
     DispatchEvents(P, Cancel);
     if Cancel or Abort then Goto 1;
    end;
   if not Block then begin
    M := #12;
    S.Write(M[1], 1);
   end;
   S.Done;
   Message(Application, evCommand, cmFilePrint, @FName);
  1:
   P^.Free;
   if Block then Dispose(L,Done);
  end;
  {$ENDIF}
       {-DataCompBoy-}

  procedure SetFormat;
   var Data: Record S1, S2, S3: String[6]; end;
       I: LongInt;
       J: Integer;
  begin
   Data.S1 := ItoS(EdOpt.LeftSide);
   Data.S2 := ItoS(EdOpt.RightSide);
   Data.S3 := ItoS(EdOpt.InSide);

   if ExecResource(dlgEditorFormat, Data) <> cmCancel then
    begin
     Val(Data.S1, I, J); if J = 0 then EdOpt.LeftSide := I;
     Val(Data.S2, I, J); if J = 0 then EdOpt.RightSide := I;
     Val(Data.S3, I, J); if J = 0 then EdOpt.InSide := I;

     if (EdOpt.LeftSide > EdOpt.RightSide) or (EdOpt.LeftSide < 0) then EdOpt.LeftSide := 0;
     if EdOpt.RightSide < 2 then EdOpt.RightSide := 2;
     if (EdOpt.InSide >= EdOpt.RightSide) or (EdOpt.InSide < 0) then EdOpt.InSide := EdOpt.LeftSide;
    end;
  end;

  procedure GotoLine;
   const S: String = '';
   var I: LongInt;
       J: Integer;
  begin
   if ExecResource(dlgGotoLine, S) <> cmOK then Exit;
   Val(S, I, J);
   if (J = 0) and (I > 0) then ScrollTo(Delta.X, I-1);
  end;

  procedure SortBlock(Reverse:Boolean);
    var {PC: PSortCollection;}
        {I, J, K: Integer;
        B: Boolean;}
        S: string;
        Info: PView;
        Tmr: TEventTimer;
        UC: Boolean;
        Cmpr:function(const L,R:String):Boolean;

{AK155 QuickSort имеет существенный недостаток: при равных
ключах он может переставлять строки. Поэтому я заменил его на
метод Шелла, который этого недостака не имеет. По быстродействию
разницы особой не будет: при числе элементов до 20000 Шелл если
и проигрывает, то не сильно.}

{ Flash: I fixed the swapping of lines with equal keys in QuickSort
         and turned in on for RealMode version because ShellSort
         causes a crash in this case. }

{$IFNDEF DPMI}
   procedure QuickSort(L, R: LongInt);
   var
     I, J: LongInt;

     function Ln(AI: LongInt): string;
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
           if (I <> J) and (Cmpr(Ln(I),Ln(J)) or Cmpr(Ln(J),Ln(I))) then { Flash }
            FileLines^.SwapItems(I, J);
           Inc(I);
           Dec(J);
         end;
       until I > J;
       if L < J then QuickSort(L, J);
       L := I;
     until I >= R;
   end;
{$ELSE}
   procedure ShellSort(L, R: LongInt);
   var
     i, j, step, n: longint;
     P1, x: PString;
   label
     1;

     function Ln(AI: LongInt): String;
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
   n := R-L+1;
   j := 13; while j < n do j := j*3 + 1;
   step := j div 3;
   while step <> 1 do
     begin
     step := step div 3;
     for i := L+step to R do
       begin
       s := ln(i);
       if Cmpr(s, ln(i-step)) then
         begin
         x := FileLines^.At(i);
         j := i-step;
         repeat
           P1 := FileLines^.At(j);  {-SBlocks}
           FileLines^.AtPut(j+step, P1);
           if j <= L+step-1 then goto 1;
           j := j-step;
         until Cmpr(ln(j), s);
         j := j+step;
   1:
         FileLines^.AtPut(j, x);
         end;
       end;
     end;
   end;
{$ENDIF}
{/AK155}

  begin
    if not VertBlock then
     begin
       ErrMsg(dlED_VertNeed);
       Exit;
     end;
    if Reverse then Cmpr:=Bigger else Cmpr:=Less;
    {$IFNDEF DPMI}
    UC := PBlocker(FileLines)^.UseCache;
    PBlocker(FileLines)^.UseCache := False;
    {$ENDIF}
    Info := WriteMsg(GetString(dlPleaseStandBy));
    NewTimer(Tmr, 2);
    ChangeLine;
    BoundStart := Mark.A.X+1; BoundEnd := Mark.B.X - Mark.A.X;
{AK155} { Flash }
    {$IFNDEF DPMI}
    QuickSort(Mark.A.Y, Mark.B.Y);
    {$ELSE}
    ShellSort(Mark.A.Y, Mark.B.Y);
    {$ENDIF}
{/AK155} { Flash }
    if UndoInfo <> nil then UndoInfo^.FreeAll; Inc(UndoTimes); Modified := True;
    Dispose(Info,Done);
    {$IFNDEF DPMI}
    PBlocker(FileLines)^.UseCache := UC;
    {$ENDIF}
  end;

  procedure CalcBlock;
    var
        I: LongInt;
        J: Integer;
        R1, R2: Extended;
        S: String;
        R: Extended;
  begin
   if not VertBlock then begin
    ChangeLine;
    S:=DelSpaces(Copy(GetLine(Mark.A.Y), Mark.A.X+1, Mark.B.X - Mark.A.X));
    if S <> '' then begin
     R:=Evalue(S, nil);
     if not EvalueError then begin
      Str(R:0:20, S);
      While S[Length(S)] = '0' do Dec(S[0]);
      if S[Length(S)] = '.' then Dec(S[0]);
      Message(Application, evCommand, cmPutInClipboard, @S);
     end;
    end;
   end else begin
    ChangeLine;
    BoundStart := Mark.A.X; BoundEnd := Mark.B.X - Mark.A.X;
    R1 := 0;
    for I := Mark.A.Y to Mark.B.Y do
      begin
        R2:=EValue(DelSpaces(Copy(GetLine(I), BoundStart+1, BoundEnd)), Nil);
        If Not EValueError Then R1 := R1 + R2; { Flash }
      end;
    Str(R1:0:20, S);
    While S[Length(S)] = '0' do Dec(S[0]);
    if S[Length(S)] = '.' then Dec(S[0]);
    Message(Application, evCommand, cmPutInClipboard, @S);
   end;
  end;

  procedure WorkModify;
  begin
   if WorkModified then Exit;
   LastLine := Delta.Y;
   LastShape := GetLine(LastLine);
   WorkModified := True; Modified := True; WasDelete := False;
  end;


  procedure PlayMacro;
    var P: PEditMacros;

    function DoFind(P: PEditMacros): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
    begin
      if UpStrg(P^.Name^) = UpCase(Event.InfoChar) then
        begin
          P^.Play(@Self);
          DoFind := True;
        end else DoFind := False;
    end;

  begin
    if Event.InfoChar < #32 then Inc(Event.InfoChar, 64);
    Macros^.FirstThat(@DoFind);
  end;

  procedure SelectMacro;
  begin
  end;

  procedure SearchFwd;
    var I: LongInt;
        C: String[2];
  begin
    I := 1; Inc(LastX);
    case WorkString[LastX] of
      '[': C := '[]';
      '{': C := '{}';
      '(': C := '()';
      '<': C := '<>'; {-$VIV}
    end;
    FreeStr := WorkString;
    while (LastY < FileLines^.Count) do
     begin
        while LastX < Length(FreeStr) do
         begin
           if FreeStr[LastX+1] = C[1] then Inc(I) else
             if FreeStr[LastX+1] = C[2] then Dec(I);
           if I = 0 then
             begin
               ScrollTo(LastX, LastY);
               HandleCommand := True;
               Exit;
             end;
           Inc(LastX);
         end;
       Inc(LastY); LastX := 0; FreeStr := GetLine(LastY);
     end;
  end;

  procedure SearchBwd;
    var I: LongInt;
        C: String[2];
  begin
    I := 1;
    case WorkString[LastX+1] of
      ']': C := '[]';
      '}': C := '{}';
      ')': C := '()';
      '>': C := '<>'; {-$VIV}
    end;
    Dec(LastX);
    FreeStr := WorkString;
    while (LastY >= 0) do
     begin
        while LastX >= 0 do
         begin
           if FreeStr[LastX+1] = C[2] then Inc(I) else
             if FreeStr[LastX+1] = C[1] then Dec(I);
           if I = 0 then
             begin
               ScrollTo(LastX, LastY);
               HandleCommand := True;
               Exit;
             end;
           Dec(LastX);
         end;
       Dec(LastY); FreeStr := GetLine(LastY); LastX := Length(FreeStr)-1;
     end;
  end;

  procedure ChangeVertBlockCase(C: Word); forward;

  procedure ChangeBlockCase(C: Word; Show: Boolean);
    var I,J,K,L,SX,EX,SY,EY: LongInt;
        S,S1: String;
        A: String[4];
        P: PCollection;
  begin
    if (Mark.A.Y > Mark.B.Y) or (Mark.A.Y = Mark.B.Y) and (Mark.A.X >= Mark.B.X) then Exit;
    ChangeLine;
    S := GetLine(Mark.A.Y);
{Commented by John_SW 20-08-2002}
    {while (Mark.A.X > 0) and not (S[Mark.A.X+1] in BreakChars) do Dec(Mark.A.X);}
    SY := Mark.A.Y; SX := Mark.A.X+1;
    S1 := Copy(S, 1, SX-1);
    S := GetLine(Mark.B.Y);
    if Mark.B.X = 0 then begin EY := Mark.B.Y-1; EX := 255; end else
     begin
       EY := Mark.B.Y;
{Commented by John_SW 20-08-2002}
       {while (Mark.B.X < Length(S)) and not (S[Mark.B.X+1] in BreakChars) do Inc(Mark.B.X);}
       EX := Mark.B.X;
     end;
    P := GetSelection;
    WorkModify;
    ChangeLine;
    StoreUndoInfo(udDelBlock, Mark.A, P);
    for I := SY to EY do
      begin
        S := GetLine(I);
        if I = SY then J := SX else J := 1;
        if I = EY then L := EX else L := Length(S);
        case C of
          cmUpcaseBlock: S := Copy(S, 1, J-1)+UpStrg(Copy(S, J, L-J+1))+Copy(S,L+1,255);
          cmLowcaseBlock: S := Copy(S, 1, J-1)+LowStrg(Copy(S, J, L-J+1))+Copy(S,L+1,255);
          cmCapitalizeBlock: S := Copy(S, 1, J-1)+CapStrg(Copy(S, J, L-J+1))+Copy(S,L+1,255);
          cmRusEngConv: S := Copy(S, 1, J-1)+RusEngConv(Copy(S, J, L-J+1))+Copy(S,L+1,255);
        end;
        ModifyLine(I, S, False);
        if I = EY then S := Copy(S, L+1, 255);
      end;
    Insert(Char(Lo(P^.Count))+Char(Hi(P^.Count))+S1, S, 1);
    StoreUndoInfo(udFormatBlock, Mark.A, S);
    ChangeLine;
    if Show then DrawView;
  end;

  procedure ChangeVertBlockCase;
    var OldMark: TRect;
        I: LongInt;
  begin
    OldMark := Mark;
    for I := OldMark.A.Y to OldMark.B.Y do
      begin
        Mark.Assign(OldMark.A.X, I, OldMark.B.X, I);
        ChangeBlockCase(C, False);
      end;
    Mark := OldMark;
    DrawView;
  end;

  procedure CopyWinBlock;
   var R: TRect;
  begin
   ChangeLine;
   if (Clipboard <> nil) then Dispose(Clipboard,Done);
   Clipboard := GetSelection;
   SetWinClip( PLineCollection(Clipboard));
  end;

  {-$VIV 20.05.99--}

  function WordsIsEqual(pWord1, pManyWords: String): Boolean;
  begin
    WordsIsEqual := System.Pos(';' + UpStrg(pWord1) + ';',
                               ';' + UpStrg(pManyWords) + ';') <> 0;
  end;

  function CurrentWordIs(pWord: String): Boolean;
  var S: String;
      Max, I: Integer;
  begin
    CurrentWordIs := False;
    S := WorkString;
    Max := Delta.X+1;
    if Max > Length(S) then Max := Length(S) else if Max = 0 then Max := 1;
    if (S[Max] in BreakChars) then Exit;
    for I := Max downto 1 do
      if (S[I] in BreakChars) then begin S := Copy(S, I + 1, 255); Break; end;
    for I := 1 to Length(S) do
      if (S[I] in BreakChars) then begin S := Copy(S, 1, I - 1); Break; end;
    CurrentWordIs := WordsIsEqual(S, pWord);
  end;

  { FromPos - search start position (last search stop position)  }
  { WrdPos - first character potition in th word found           }
  { pWord - the word found                                       }
  { result - found a word ?                                      }

  function GetPrevWordViv(var FromPos, WrdPos: TPoint; var pWord: String): Boolean;
  var S, Buf: String;
      WasBreak, DoStore, MakeExit: Boolean;
  begin
    GetPrevWordViv := False;
    if (FromPos.Y > FileLines^.Count - 1) or (FromPos.Y < 0) then Exit;
    Buf := '';
    S := GetLine(FromPos.Y);
    MakeExit := False;
    WasBreak := False;
    DoStore := (FromPos.X >= 0) and (FromPos.X < Length(S)) and (S[FromPos.X+1] in BreakChars);
    repeat
      if (FromPos.X >= 0) then
      begin
        if (S[FromPos.X+1] in BreakChars) then {BreakChars}
        begin
          if Buf <> '' then
          begin
            pWord := Buf;
            GetPrevWordViv := True;
            MakeExit := True;
            if FromPos.X < Length(S) - 1 then Inc(FromPos.X);
          end;
          WasBreak := True;
        end else begin {not BreakChars}
          if WasBreak then DoStore := True;
          if DoStore then
            begin Buf := S[FromPos.X+1] + Buf; WrdPos := FromPos; end;
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
        end else begin
          Dec(FromPos.Y);
          FromPos.X := 0;
          WasBreak := True;
          if FromPos.Y < 0 then MakeExit := True
           else if not MakeExit then
            begin S := GetLine(FromPos.Y); FromPos.X := Length(S) - 1; end;
        end;
      end;
    until MakeExit;
  end;

  function GetNextWordViv(var FromPos, WrdPos: TPoint; var pWord: String): Boolean;
  var S, Buf: String;
      WasBreak, DoStore, MakeExit: Boolean;
  begin
    GetNextWordViv := False;
    if (FromPos.Y > FileLines^.Count - 1) or (FromPos.Y < 0) then Exit;
    Buf := '';
    S := GetLine(FromPos.Y);
    MakeExit := False;
    WasBreak := False;
    DoStore := (FromPos.X >= 0) and (FromPos.X < Length(S)) and (S[FromPos.X+1] in BreakChars);
    repeat
      if (FromPos.X < Length(S)) then
      begin
        if (S[FromPos.X+1] in BreakChars) then {BreakChars}
        begin
          if Buf <> '' then
          begin
            pWord := Buf;
            GetNextWordViv := True;
            MakeExit := True;
            if FromPos.X > 0 then Dec(FromPos.X);
          end;
          WasBreak := True;
        end else begin {not BreakChars}
          if WasBreak then
          begin
            DoStore := True;
            WrdPos := FromPos;
          end;
          if DoStore then Buf := Buf + S[FromPos.X+1];
          WasBreak := False;
        end;
        Inc(FromPos.X);
      end;
      if (FromPos.X >= Length(S)) then
      begin
        if Buf <> '' then
        begin
          pWord := Buf;
          GetNextWordViv := True;
          MakeExit := True;
        end else begin
          Inc(FromPos.Y);
          FromPos.X := 0;
          WasBreak := True;
          if FromPos.Y > FileLines^.Count - 1 then MakeExit := True
           else if not MakeExit then S := GetLine(FromPos.Y);
        end;
      end;
    until MakeExit;
  end;

  procedure SearchWordViv(Frw: Boolean; pIncWord, pWord: String);
  var Pnt, Wrd: TPoint;
      Res: Boolean;
      sWord: String;
      I, PrevLine: LongInt;
      DeepLevel: Longint;
      IsDeepRange: Boolean;
  begin
    Pnt := Delta;
    I := 1;
    DeepLevel := FastSearchDeep;
    IsDeepRange := (DeepLevel <> 0);
    PrevLine := Pnt.Y;
    repeat
      if Frw then Res := GetNextWordViv(Pnt, Wrd, sWord)
       else Res := GetPrevWordViv(Pnt, Wrd, sWord);
      if IsDeepRange and (PrevLine <> Pnt.Y) then
        begin Dec(DeepLevel); PrevLine := Pnt.Y; end;
      if Res and (pIncWord <> '') and WordsIsEqual(sWord, pIncWord) then Inc(I)
       else if Res and (pIncWord <> '') and WordsIsEqual(sWord, pWord) then Dec(I)
        else if Res and (pIncWord = '') and WordsIsEqual(sWord, pWord) then I := 0;
      if I = 0 then
      begin
        ScrollTo(Wrd.X, Wrd.Y);
        HandleCommand := True;
        Break;
      end;
      if IsDeepRange and (DeepLevel <= 0) then
      begin
        if MessageBox(GetString(dlVeryDeepSearch),nil,mfYesNoConfirm) = cmYes then
          IsDeepRange := False else Break;
      end;
    until (not Res);
  end;

  function CheckPairsViv(pPair1, pPair2: String): Boolean;
  const
    ForwardDir = True;
    BackwardDir = False;
  begin
    CheckPairsViv := True;
    if CurrentWordIs(pPair1) then SearchWordViv(ForwardDir, pPair1, pPair2)
     else if CurrentWordIs(pPair2) then SearchWordViv(BackwardDir, pPair2, pPair1)
      else CheckPairsViv := False;
  end;

  {-$VIV--}

begin
  LastX := Delta.X; LastY := Delta.Y; HandleCommand := False;
  case Event.Command of
     cmWindowsCopy: CopyWinBlock;
     cmSyncClipIn: begin
                     SyncClipIn;
                     if ClipBoardStream<>nil then
                      ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-1, 0));
                     CopyLines2Stream(Clipboard, ClipBoardStream);
                   end;
     cmSyncClipOut: SyncClipOut(True);
     cmRusEngConv, {-$VIV}
     cmUpcaseBlock,
     cmLowcaseBlock,
     cmCapitalizeBlock: if ValidBlock and BlockVisible then
                            if VertBlock then ChangeVertBlockCase(Event.Command)
                                         else ChangeBlockCase(Event.Command, True);
     cmPlayMacro: PlayMacro;
     cmSelectMacro: SelectMacro;
     cmSetMargins: SetFormat;
     cmGotoLineNumber: GotoLine;
     cmGotoLineNumber2: if LongInt(Event.InfoPtr)>0 then begin
                         ScrollTo(Delta.X, LongInt(Event.InfoPtr)+20);
                         ScrollTo(Delta.X, LongInt(Event.InfoPtr)-1);
                        end;
     cmRevSortBlock: begin if ValidBlock then SortBlock(True); HandleCommand := True; end;
     cmSortBlock: begin if ValidBlock then SortBlock(False); HandleCommand := True; end;
     cmCalcBlock: begin if ValidBlock then CalcBlock; HandleCommand := True; end;
     cmStartSearch: begin ChangeLine; StartSearch(False); ChangeLine end;
     cmReplace: begin ChangeLine; StartSearch(True); ChangeLine end;
     cmUndo: begin MakeUndo; HandleCommand := True end;
     cmRedo: begin MakeRedo; HandleCommand := True end; {-$VOL}
     cmIndentBlock: begin UnMark := True; IndentBlock(True) end;
     cmUnIndentBlock: begin UnMark := True; UnIndentBlock(True) end;
     cmFRight, cmFLeft, cmFJustify, cmFCenter:
              begin FormatBlock(Event.Command) end;
     cmLRight, cmLLeft, cmLJustify, cmLCenter:
              begin ChangeLine;
                    Mark.A := Delta; Mark.B := Delta;
                    Mark.A.X := 0; Mark.B.X := Length(GetLine(Delta.Y));
                    Sel := Mark; BlockVisible := True;
                    FormatBlock(Event.Command) end;
     cmViewFile: if Valid(cmClose) and (EditName<>'') then begin
                  {if Desktop<>nil then Desktop^.Lock;}
                  if Application<>nil then begin
                    PDNAppl(Application)^.ViewFile(False, False, False, EditName); (* X-Man *) { Flash }
                    MessageL(Application, evCommand, cmScrollBarChanged,
                      GetOffsetForLineNumber(EditName, Pos.Y+1));
                  end;
                  ClearEvent(Event);
                  Message(Owner, evCommand, cmClose, nil);
                  {if Desktop<>nil then Desktop^.UnLock;}
                  Exit;
                 end;
{$IFDEF PRINTMANAGER}
     cmPrintBlock: Print(True);
     cmPrintFile: Print(False);
{$ENDIF}
     cmBracketPair: Begin
                      ChangeLine;
                      if (LastX < WL) then
                        if (WorkString[LastX+1] in ['[', '{', '(', '<']) then SearchFwd
                          else if (WorkString[LastX+1] in [')', '}', ']', '>']) then SearchBwd
                          {-$VIV}
                          else if CheckPairsViv('begi'+'n;ca'+'se;a'+'sm', 'e'+'nd') then {no action}
                          else if CheckPairsViv('repeat', 'until') then {no action}
                          else if CheckPairsViv('if', 'then') then {no action}
                          ;
                          {-$VIV::}
                    end;
     cmSwitchKeyMapping: {-$VIV}
       begin
         if WorkModified then
         begin
           ModifyLine(LastLine, WorkString, True);
           Modified := Modified or WorkModified;
           WorkModified := False;
         end;
         case KeyMap of
           kmAscii: KeyMap := kmAnsi;
           kmAnsi: if Koi8rKeyMap then KeyMap := kmKoi8r
                    else KeyMap := kmAscii;
           kmKoi8r: KeyMap := kmAscii;
           else     KeyMap := kmAscii;
         end;
         WorkString := GetLine(Delta.Y);
         DrawView;
       end;
  end;
end;


end.
