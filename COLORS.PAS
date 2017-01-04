{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.10
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

unit Colors;
interface
uses Dialogs, Collect;

 {$IFDEF CHCOL}procedure ChangeColors;{$ENDIF}
 procedure WindowManager;
 procedure SetHighlightGroups;

type

     PWindowList = ^TWindowList;
     TWindowList = object(TListBox)
      function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
     end;

     PWindowCol = ^TWindowCol;
     TWindowCol = object(TCollection)
      procedure FreeItem(Item: Pointer); virtual;
     end;


implementation
uses Messages, RStrings, Drives, Startup, DnIni, VideoMan, Objects, DNApp,
     Drivers, Views, Memory, Commands, DNStdDlg, advance, DNHelp, Advance2;

procedure SetHighlightGroups;
 var D: Record S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11: String[250]; end;
begin
 With D do
  begin
    with CustomMask1^  do begin S1  := Copy(Filter, 2, Length(Filter)-2) end;
    with CustomMask2^  do begin S2  := Copy(Filter, 2, Length(Filter)-2) end;
    with CustomMask3^  do begin S3  := Copy(Filter, 2, Length(Filter)-2) end;
    with CustomMask4^  do begin S4  := Copy(Filter, 2, Length(Filter)-2) end;
    with CustomMask5^  do begin S5  := Copy(Filter, 2, Length(Filter)-2) end;
    with CustomMask6^  do begin S6  := Copy(Filter, 2, Length(Filter)-2) end;
    with CustomMask7^  do begin S7  := Copy(Filter, 2, Length(Filter)-2) end;
    with CustomMask8^  do begin S8  := Copy(Filter, 2, Length(Filter)-2) end;
    with CustomMask9^  do begin S9  := Copy(Filter, 2, Length(Filter)-2) end;
    with CustomMask10^ do begin S10 := Copy(Filter, 2, Length(Filter)-2) end;
    with Archives^     do begin S11 := Copy(Filter, 2, Length(Filter)-2) end;
    if ExecResource(dlgHighlightGroups, D) <> cmOK then Exit;
    While S1[Length(S1)]   in [' ', ';'] do Dec(S1[0]);  CustomMask1^.Filter := S1;  MakeTMaskData(CustomMask1 ^);
    While S2[Length(S2)]   in [' ', ';'] do Dec(S2[0]);  CustomMask2^.Filter := S2;  MakeTMaskData(CustomMask2 ^);
    While S3[Length(S3)]   in [' ', ';'] do Dec(S3[0]);  CustomMask3^.Filter := S3;  MakeTMaskData(CustomMask3 ^);
    While S4[Length(S4)]   in [' ', ';'] do Dec(S4[0]);  CustomMask4^.Filter := S4;  MakeTMaskData(CustomMask4 ^);
    While S5[Length(S5)]   in [' ', ';'] do Dec(S5[0]);  CustomMask5^.Filter := S5;  MakeTMaskData(CustomMask5 ^);
{JO}While S6[Length(S6)]   in [' ', ';'] do Dec(S6[0]);  CustomMask6^.Filter := S6;  MakeTMaskData(CustomMask6 ^);
    While S7[Length(S7)]   in [' ', ';'] do Dec(S7[0]);  CustomMask7^.Filter := S7;  MakeTMaskData(CustomMask7 ^);
    While S8[Length(S8)]   in [' ', ';'] do Dec(S8[0]);  CustomMask8^.Filter := S8;  MakeTMaskData(CustomMask8 ^);
    While S9[Length(S9)]   in [' ', ';'] do Dec(S9[0]);  CustomMask9^.Filter := S9;  MakeTMaskData(CustomMask9 ^);
{JO}While S10[Length(S10)] in [' ', ';'] do Dec(S10[0]); CustomMask10^.Filter:= S10; MakeTMaskData(CustomMask10^);
    While S11[Length(S11)] in [' ', ';'] do Dec(S11[0]); Archives^.Filter    := S11; MakeTMaskData(Archives    ^);
  end;
 Message(Application, evCommand, cmUpdateConfig, nil);
 GlobalMessage(evCommand, cmPanelReread, nil);
end;

{$IFDEF CHCOL}
procedure ChangeColors;
begin
  if ExecResource(dlgColors, Application^.GetPalette^) <> cmCancel then
  begin
    DoneMemory;                 { Dispose all group buffers }
    Application^.ReDraw;        { Redraw application with new palette }
  end;
  if VGASystem then GetPalette(VGA_Palette);
end;
{$ENDIF}

procedure TWindowCol.FreeItem;
begin
end;

function TWindowList.GetText;
 var S: String;
     P: PView;
begin
 P := List^.At(Item);
 S := GetString(dlUnknownWindowType);
 Message(P, evCommand, cmGetName, @S);
 if PWindow(P)^.Number in [1..9]
  then GetText := Char($30+PWindow(P)^.Number) + ' ' + S
  else GetText := '  ' + S;
end;

procedure WindowManager;
 label 1;
 var D: PDialog;
     R: TRect;
     PC: PWindowCol;
     PV: PView;
     S: String;
     DT: record P: PCollection; N: Word; end;
     I, Num: Integer;

  procedure InsView(P: PView); {$IFDEF BIT_16}far;{$ENDIF}
  begin
    if P = nil then Exit;
    S := '';
    if (P^.GetState(sfVisible)) then begin
      Message(P, evCommand, cmGetName, @S);
      if (S <> '') then PC^.Insert(P);
    end;
  end;

begin
  New(PC, Init(10,10));
  Desktop^.ForEach(@InsView);
  if PC^.Count = 0 then begin Dispose(PC,Done); Exit end;

  D := PDialog( LoadResource( dlgWindowManager ));

  R.Assign(D^.Size.X-13,3,D^.Size.X-12,D^.Size.Y-2);
  PV := New(PScrollBar, Init(R));
  PV^.Options := PV^.Options or ofPostProcess or ofSecurity;
  D^.Insert(PV);

  R.Assign(2,3,D^.Size.X-13,D^.Size.Y-2);
  PV := New(PWindowList, Init(R,1,PScrollBar(PV)));
  PV^.Options := PV^.Options or ofPostProcess or ofSecurity;
  PListBox(PV)^.NewList(PC);
  if PC^.Count > 1 then PListBox(PV)^.Focused:=1
  else                  PListBox(PV)^.Focused:=0;
  Num := 0; {-$VIV 28.05.99--}
  if (WinManagerPosToEdit) and (PC^.Count > 0) and
     (Pos(GetString(dlEditTitle)+' -', PWindowList(PV)^.GetText(0,255)) > 0)
  then
    for I := 1 to PC^.Count - 1 do
      if (Pos(GetString(dlEditTitle)+' -', PWindowList(PV)^.GetText(I,255)) > 0) then
      begin
        Num := I;
        Break;
      end;
  if (WinManagerSelectNext) and (Num = 0) and (PC^.Count > 1) then Num := 1;
  PListBox(PV)^.Focused := Num; {-$VIV--}
  D^.Insert(PV);

  R.Assign(2,2,45,3);
  PV := New(PLabel, Init(R, GetString(dlWindowsLabel), PV));
  D^.Insert(PV);

1:
  R.A.X := Desktop^.ExecView(D);
  D^.GetData(DT);
  if PC^.Count > 0 then begin
    if R.A.X = cmNo then begin
      if PView(DT.P^.At(DT.N))^.Valid( cmClose ) then begin
        PView(DT.P^.At(DT.N))^.Free;
        New(PC, Init(10,10));
        Desktop^.ForEach(@InsView);
        if PC^.Count > 0 then begin
          DT.P^.AtDelete(DT.N);
          DT.P := NIL;
          D^.SetData( DT );
          If ( DT.N > 0 ) and ( DT.N >= PC^.Count ) then Dec( DT.N );
          DT.P := PC;
          D^.SetData( DT );
          goto 1
        end
      end
    end;
    if R.A.X = cmOK then PView(DT.P^.At(DT.N))^.Select;
  end;
  Dispose(D,Done);
  Dispose(PC,Done);
end;

end.
