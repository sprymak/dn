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
UNIT FlTools;

INTERFACE

procedure CM_AdvancedFilter(AFP: Pointer);
function  GetSelection(AFP: Pointer; Single: Boolean): Pointer;
function  SelectFiles(AFP: Pointer; Select,XORs: Boolean): Boolean;
procedure CM_CopyFiles(AFP: Pointer; MoveMode, Single: Boolean);
procedure InvertSelection(AFP: Pointer; Dr: Boolean);
procedure CM_CompareDirs(AFP, IP: Pointer);
procedure CM_EraseFiles(AFP: Pointer; Single: Boolean);
procedure CM_MakeList(AFP: Pointer);
procedure CM_SetAttributes(AFP: Pointer; Single: Boolean; CurPos: Integer);
procedure CM_SetShowParms(AFP: Pointer);
procedure CM_CopyTemp(AFP: Pointer);
procedure CM_ArchiveFiles(AFP: Pointer);
{$IFDEF Printer}
procedure CM_Print(AFP: Pointer);
{$ENDIF}
procedure CM_ToggleDescriptions(AFP: Pointer);
procedure CM_ToggleLongNames(AFP: Pointer);
procedure CM_ToggleShowMode(AFP: Pointer);
procedure CM_DragDropper(AFP: Pointer; CurPos: Integer; Ev: Pointer);
procedure CM_Dropped(AFP, EI: Pointer);
procedure DragMover(AP: Pointer; Text: String; AFC, AC: Pointer);
procedure CM_RenameSingleL(AFP, PEV: Pointer);
{$IFNDEF OS2}
procedure CM_RenameSingleS(AFP, PEV: Pointer);
{$ENDIF}
procedure CM_RenameSingleDialog(AFP, PEV: Pointer);
procedure CM_SortBy(AFP: Pointer);
function  CM_ChangeDirectory(AFP: Pointer): string;
procedure CM_MakeDir(AFP: Pointer);
procedure CM_LongCopy(AFP: Pointer);
{AK155}
procedure CM_Branch(AFP: Pointer);
{/AK155}
procedure CM_ChangeCase(AFP: Pointer);
{$IFNDEF REALMODE}
procedure CM_SelectColumn(AFP: Pointer); {JO}
{$ENDIF}
{$IFDEF EAOP}
procedure CM_SetEALongname(AFP: Pointer; CurPos: Integer); {JO}
{$ENDIF}


IMPLEMENTATION
uses LFN, {$IFNDEF OS2}LFNCol,{$ENDIF} {DataCompBoy} collect, xtime, dnini,
     advance, advance1, advance2, advance3, Dos, Objects, Dialogs, FilesCol,
     Views, DnApp, Commands, Drivers, FlPanel, FlPanelX, Drives, FileCopy,
     Gauge, Gauges, Archiver, Startup, SWE, Validate, Messages, Menus, DnHelp,
     FileFind, Tree, FBB, DnUtil, filediz, filelst {$IFDEF OS2}, FlOS2Tl {$ENDIF}
     , ErrMess
     ;

type
    PSelectList = ^TSelectList;
    TSelectList = object(TListBox)
       function IsSelected(I: LongInt): Boolean; virtual;
       procedure HandleEvent(var Event: TEvent); virtual;
    end;

procedure DrawViews(P: PFilePanelRoot);
begin
  P^.DrawView;
  if P^.InfoView <> nil then P^.InfoView^.DrawView;
end;

function TSelectList.IsSelected(I: LongInt): Boolean;
begin
  IsSelected := (List <> nil) and (Copy(CnvString(List^.At(I)),1,1)= #251); {-$VIV}
end;

procedure TSelectList.HandleEvent(var Event: TEvent);
  var P: PString;

procedure CE; begin ClearEvent(Event); DrawView; end;

procedure Invert;
begin
  if (List <> nil) and (Focused < List^.Count) then P := List^.At(Focused)
                              else P := nil;
  if P <> nil then
     if P^[1] = ' ' then P^[1] := #251 {-$VIV #254}
                    else P^[1] := ' ';
  CE
end;

procedure DoSelect(P: PString); {$IFDEF BIT_16}far;{$ENDIF}
begin
 if P <> nil then
   case Event.CharCode of
    '*': if P^[1] = ' ' then P^[1] := #251 {-$VIV #254}
                        else P^[1] := ' ';
    '+': P^[1] := #251; {-$VIV #254}
    '-': P^[1] := ' ';
   end;
end;

begin
  case Event.What of
    evMouseDown: if Event.Buttons and mbRightButton <> 0 then begin
                   inherited HandleEvent(Event);
                   Invert;
                 end;
    evKeyDown: case Event.CharCode of
                ' ': begin Event.KeyCode := kbDown; PutEvent(Event); Invert; end;
                '+','*','-': if List <> nil then begin List^.ForEach(@DoSelect); CE end;
                  else if Event.KeyCode = kbIns then
                         begin Event.KeyCode := kbDown; PutEvent(Event); Invert; end;
               end;
  end;
  if Event.What <> evNothing then inherited HandleEvent(Event);
end;

const
  AdvFWPos : TPoint = (X:-1;Y:-1);

procedure CM_AdvancedFilter;
var
   P: PFilePanelRoot absolute AFP;
   i: Byte;
   C: PStringCollection;
   ClrPlus, ClrMinus : Boolean;
   S, FileMask: string;
   FItem: Integer;


function GetMaskSelection: String;
  var PC: PStringCollection;
      I: LongInt;
      D: PDialog;
      PL: PListBox;
      IL: PInputLine;

  function MakeDialog : PDialog;
    var
      Dlg : PDialog;
      R : TRect;
      Control, Labl, Histry : PView;
   begin
      Dlg := PDialog( LoadResource( dlgAdvancedFilter ));
      with Dlg^ do begin
        R.Assign( Size.X-4,2,Size.X-3,Size.Y-4);
        Control := New(PScrollbar, Init(R));
        Insert(Control);

        R.Assign(3,2,Size.X-4,Size.Y-4);
        Control := New(PSelectList, Init(R, 3, PScrollbar(Control)));
        PL := PListBox(Control);
        PListBox(PL)^.NewList(PC);
        Insert(Control);
      end;
      MakeDialog := Dlg;
    end;

    procedure DoMake(P: PString); {$IFDEF BIT_16}far;{$ENDIF}
    begin
      if (P <> nil) and (P^[1] <> ' ') then
        begin
          AddStr(FreeStr, ';');
          if I = cmNo then AddStr(FreeStr, '-');
          FreeStr := FreeStr + Copy(P^, 2, Length(P^)-1);
        end;
    end;

    procedure GetMasks;

{AK155: эта процедура должна формировать маску по расширению
для текущего файла и добавлять ее в коллекцию. Но при этом не
учитывалась возможность длинного имени (в Name). }

       procedure DoInsert(P1: PFileRec); {$IFDEF BIT_16}far;{$ENDIF} {DataCompBoy}
         var
           l, i: integer;
       begin
         if (P1 <> nil) and (P1^.Attr and Directory = 0) then
           begin
{$IFDEF OS2}
           l :=length(P1^.Name);
           i := PosLastDot(P1^.Name);
           FreeStr := Copy(P1^.Name, i+1, l-i);
{$ELSE}
           if (P^.Drive^.Flags and psShowLongName) <> 0 then
             begin
               l :=length(P1^.LFN);
               i := PosLastDot(P1^.LFN);
               FreeStr := Copy(P1^.LFN, i+1, l-i);
             end
            else FreeStr := Copy(P1^.Name, 10, 3);
{$ENDIF}
             DelRight(FreeStr); UpStr(FreeStr); Insert(' *.', FreeStr, 1);
{AK155: только лишние тормоза, поскольку Duplicate=false
     if PC^.IndexOf(@FreeStr) < 0 then
/AK155}
           PC^.Insert(NewStr(FreeStr));
           end;
       end;

    var
      d: PDrive;
      fs, ti: string;
      fc: PFilesCollection;
    begin
      d := P^.Drive;
      if TypeOf(d^) = TypeOf(TFindDrive) then PFindDrive(d)^.AllowPack := False;
      fc := PFilesCollection(
        d^.GetDirectory(P^.SortMode, P^.PanelFlags, x_x, fs, ti));
      if TypeOf(d^) = TypeOf(TFindDrive) then PFindDrive(d)^.AllowPack := True;
      if (fc <> nil) and (not d^.NoMemory) then fc^.ForEach(@DoInsert);
      Dispose(fc,Done);
    end;

begin
  GetMaskSelection := '';
  PC := New(PStringCollection, Init(20,20,False));
  GetMasks;
  if (PC^.Count>0) and (not P^.Drive^.NoMemory) then
  begin
    PC^.AtInsert(0, NewStr(' *.*'));
    D := MakeDialog; D^.Options := D^.Options or ofCentered;
    PL^.FocusItem(FItem);

    if AdvFWPos.X <> -1 then
    begin
      D^.MoveTo(AdvFWPos.X, AdvFWPos.Y);
      D^.Options := D^.Options and not ofCentered;
    end;
    I := Desktop^.ExecView(D);
    AdvFWPos.X := D^.Origin.X;  AdvFWPos.Y := D^.Origin.Y;


    if I <> cmCancel then
      begin
        FItem := PL^.Focused;
        FreeStr := '';
        PC^.ForEach(@DoMake);
        if FreeStr = '' then
           begin
             FreeStr := CnvString(PC^.At(FItem));
             if I = cmNo then Insert('-', FreeStr, 1);
           end else Delete(FreeStr, 1, 1); {DelFC(FreeStr);}
        if (I = cmNo) and (FreeStr <> '-*.*') and
           (FreeStr <> '-*')
         then Insert('*.*;', FreeStr, 1);
        DelLeft(FreeStr);
        GetMaskSelection := FreeStr;
      end;
    Dispose(D,Done);
  end;
  Dispose(PC,Done);
end;



 procedure Ins(const AAS: string);
 var
   Mns: Boolean;
 begin
   if (AAS = '') then Exit;
   Mns := (Copy(AAS,1,2) = '- ');
   if (ClrMinus and Mns) or
      (ClrPlus and not Mns) or
      (C^.IndexOf(@AAS)<>-1) then Exit;
    C^.Insert(NewStr(AAS));
 end;

 procedure MakeMask(P: PString); {$IFDEF BIT_16}far;{$ENDIF}
 begin
   if FileMask <> '' then FileMask := FileMask + ';' ;
   FileMask := FileMask + P^;
 end;

 begin   {------------------- CM_AdvancedFilter -----------------------}
  FileMask := P^.FileMask;
  FItem := 0;
  repeat
    S := GetMaskSelection;
    if (P^.Drive^.NoMemory) or (S = '') then Break;
    if (S = '- *.*') or (S = x_x) then FileMask := S else
    begin
      if FileMask = x_x then begin FileMask := ''; FreeStr := S+';' end
        else FreeStr := ';' + FileMask + ';' + S + ';';
      FileMask := S; S := FreeStr;
      ClrMinus := Pos(';- *.*;',S) > 0;
      if ClrMinus then Replace('- *.*','',S);
      ClrPlus  := Pos(';*.*;',S)  > 0;
      if ClrPlus then Replace(x_x,'',S);
      if not (ClrPlus and ClrMinus) then
      begin
        if Copy(FileMask, 1, 2) = '- ' then
           Replace(';'+Copy(FileMask, 3, 5)+';',';',S) else
           Replace(';- '+FileMask+';',';',S);
        C := New(PStringCollection, Init(4,4,False));
        repeat
          i := PosChar(';',S);
          if i = 0 then
          begin
            Ins(S);
            Break;
          end;
          Ins(Copy(S, 1, i-1));
          Delete(S, 1, i);
        until False;
        FileMask := '';
        C^.ForEach(@MakeMask);
        Dispose(C,Done);
        if FileMask <> '' then FileMask := ';'+FileMask;
        if ClrPlus then FileMask := x_x + FileMask;
        if ClrMinus then FileMask := '- *.*'+FileMask;
      end;
    end;
    P^.FileMask := FileMask;
    P^.OldDelta := -1;
    P^.RereadDir;
    P^.Delta := -1;
    P^.DrawView;
  until False;
  P^.ChkNoMem;
 {messagebox(FileMask, nil, mfOKButton);}
end;        {------------------- CM_AdvancedFilter -----------------------}

{AK155 Получение первого номера в коллекции, соответствующего
не фиктивному имени '..', а настоящему имени. Возвращает либо
1 (если это корень диска и элемента '..' нет, либо 2, если он
есть.}
function FirstNameNum(P: PFilePanelRoot): longint;
  begin
  FirstNameNum := 1;
  with P^.Files^ do
{$IFDEF OS2}
    if (Count > 0) and (PFileRec(At(0))^.Name = '..') then
{$ELSE}
  {$IFDEF Win32}
    if (Count > 0) and (PFileRec(At(0))^.LFN = '..') then
  {$ELSE}
    if (Count > 0) and (PFileRec(At(0))^.Name[0] = '.') then
  {$ENDIF}
{$ENDIF}
      FirstNameNum := 2;
  end;
{/AK155}

function GetSelection;
 var FC: PFilesCollection;
     P: PFilePanelRoot absolute AFP;
     I: LongInt;
     all: boolean;
label Sel;
begin
  GetSelection := nil;
  if P^.Files^.Count = 0 then Exit;
  All:=false;
  if (P^.SelNum = 0) or Single then
  begin
{$IFNDEF OS2}
 {$IFNDEF Win32}
   if PFileRec(P^.Files^.At(P^.ScrollBar^.Value))^.Name[1] = '.'
 {$ELSE}
   if PFileRec(P^.Files^.At(P^.ScrollBar^.Value))^.LFN = '..'
 {$ENDIF}
{$ELSE}
   if PFileRec(P^.Files^.At(P^.ScrollBar^.Value))^.Name = '..'
{$ENDIF} then
     begin
      All:=true;
      Goto Sel;
     end;
   New(FC, Init(1,1));
   FC^.Insert(P^.Files^.At(P^.ScrollBar^.Value));
  end else
sel:  begin
       if All then New(FC, Init(P^.Files^.Count,1))
              else New(FC, Init(P^.SelNum, 1));
       FC^.SortMode := 200;
       for I := FirstNameNum(P) to P^.Files^.Count do
        if All or PFileRec(P^.Files^.At(I-1))^.Selected then
          FC^.Insert(P^.Files^.At(I-1));
      end;
 GetSelection := FC;
end;

        {-DataCompBoy-}
function SelectFiles;
 var S, SN: String;
     I: LongInt;
     P: PFilePanelRoot absolute AFP;
     PF: PFileRec; {DataCompBoy}
     J, K: Byte; {JO}
    {SD: String;} {JO}

begin
 SelectFiles := False;
 if P^.Files^.Count = 0 then Exit;
 if not SelectDialog(Select, S, XORs) then Exit;
 UpStr(S);
 SelectFiles := True;
 P^.SelNum := 0;
 P^.SelectedLen := 0; P^.PackedLen := 0;
 for I := FirstNameNum(P) to P^.Files^.Count do
  begin
   PF := P^.Files^.At(I-1);
{$IFNDEF OS2}
   SN := GetLFN(PF^.LFN);
{$ELSE}
   SN := PF^.Name;
{$ENDIF}
   if ((PF^.Attr and Directory = 0) or not Select) and
      (((InFilter(UpStrg(SN), S) xor XORs)
       {$IFNDEF OS2} and ((P^.Drive^.Flags and psShowLongName) <> 0) {$ENDIF})
{$IFNDEF OS2}
      or ((InFilter(MakeFileName(PF^.Name), S) xor XORs) and ((P^.Drive^.Flags and psShowLongName) = 0))
{$ENDIF}
      )
      then PF^.Selected := Select;

(* {работает, но тормозит}
   if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) then
     begin
      SD := GetPossibleDizOwner(1);
      if SD <> '' then
      {$IFNDEF OS2}
      if (LowStrg(MakeFileName(PF^.Name))
      {$ELSE}
      if (LowStrg(PF^.Name)
      {$ENDIF}
        = LowStrg(GetName(GetDizOwner(PF^.Owner^, SD, Off)))) then PF^.Selected := Off;
     end;
*)
   if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) and (FMSetup.DIZ <> '')
    and (P^.Drive^.DriveType < dtArc) then
     begin
      K := 1;
      for J := 1 to Length(FMSetup.DIZ) do
      begin
        if (FMSetup.DIZ[J] = ';') then
          begin
         {$IFNDEF OS2}
           {$IFDEF OS_DOS}
           if (LowStrg(MakeFileName(PF^.Name)) = LowStrg(Copy(FMSetup.DIZ,K,J-K))) then PF^.Selected := Off;
           {$ELSE}
           if (LowStrg(GetLFN(PF^.LFN)) = LowStrg(Copy(FMSetup.DIZ,K,J-K))) then PF^.Selected := Off;
           {$ENDIF}
         {$ELSE}
           if (LowStrg(PF^.Name) = LowStrg(Copy(FMSetup.DIZ,K,J-K))) then PF^.Selected := Off;
         {$ENDIF}
           K:=J+1;
          end;
        if J = Length(FMSetup.DIZ) then
         {$IFNDEF OS2}
           {$IFDEF OS_DOS}
           if (LowStrg(MakeFileName(PF^.Name)) = LowStrg(Copy(FMSetup.DIZ,K,J-K+1))) then PF^.Selected := Off;
           {$ELSE}
           if (LowStrg(GetLFN(PF^.LFN)) = LowStrg(Copy(FMSetup.DIZ,K,J-K+1))) then PF^.Selected := Off;
           {$ENDIF}
         {$ELSE}
           if (LowStrg(PF^.Name) = LowStrg(Copy(FMSetup.DIZ,K,J-K+1))) then PF^.Selected := Off;
         {$ENDIF}
      end;
     end;
{JO}
   if PF^.Selected then
   begin
     P^.PackedLen := P^.PackedLen + PF^.PSize;
     P^.SelectedLen := P^.SelectedLen + PF^.Size;
     Inc(P^.SelNum);
   end;
  end;
  DrawViews(P);
end;
        {-DataCompBoy-}

procedure CM_CopyFiles;
 var FC: PFilesCollection;
     P: PFilePanelRoot absolute AFP;
begin
 CurrentDirectory := P^.Drive^.GetRealName;
 if P^.Files^.Count = 0 then Exit;
 FC := GetSelection(P, Single);
 if FC = nil then Exit;
 P^.Drive^.CopyFiles(FC, P, MoveMode);
 FC^.DeleteAll;
 Dispose(FC,Done);
end;

        {-DataCompBoy-}
procedure InvertSelection;
 var I: LongInt;
     PF: PFileRec;
     P: PFilePanelRoot absolute AFP;
     J, K: Byte;
    {SD: String;} {JO}
begin
 P^.SelNum := 0; P^.SelectedLen := 0; P^.PackedLen := 0;
 for I := FirstNameNum(P) to P^.Files^.Count do
  begin
   PF := PFileRec(P^.Files^.At(I-1));
   if Dr or (PF^.Attr and Directory = 0) then PF^.Selected := not PF^.Selected;

{JO} {работает, но тормозит}
(*
   if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) then
     begin
      SD := GetPossibleDizOwner(1);
      if SD <> '' then
      {$IFNDEF OS2}
      if (LowStrg(MakeFileName(PF^.Name))
      {$ELSE}
      if (LowStrg(PF^.Name)
      {$ENDIF}
        = LowStrg(GetName(GetDizOwner(PF^.Owner^, SD, Off)))) then PF^.Selected := Off;
     end;
*)
   if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) and (FMSetup.DIZ <> '')
    and (P^.Drive^.DriveType < dtArc) then
     begin
      K := 1;
      for J := 1 to Length(FMSetup.DIZ) do
      begin
        if (FMSetup.DIZ[J] = ';') then
          begin
         {$IFNDEF OS2}
           {$IFDEF OS_DOS}
           if (LowStrg(MakeFileName(PF^.Name)) = LowStrg(Copy(FMSetup.DIZ,K,J-K))) then PF^.Selected := Off;
           {$ELSE}
           if (LowStrg(GetLFN(PF^.LFN)) = LowStrg(Copy(FMSetup.DIZ,K,J-K))) then PF^.Selected := Off;
           {$ENDIF}
         {$ELSE}
           if (LowStrg(PF^.Name) = LowStrg(Copy(FMSetup.DIZ,K,J-K))) then PF^.Selected := Off;
         {$ENDIF}
           K:=J+1;
          end;
        if J = Length(FMSetup.DIZ) then
         {$IFNDEF OS2}
           {$IFDEF OS_DOS}
           if (LowStrg(MakeFileName(PF^.Name)) = LowStrg(Copy(FMSetup.DIZ,K,J-K+1))) then PF^.Selected := Off;
           {$ELSE}
           if (LowStrg(GetLFN(PF^.LFN)) = LowStrg(Copy(FMSetup.DIZ,K,J-K+1))) then PF^.Selected := Off;
           {$ENDIF}
         {$ELSE}
           if (LowStrg(PF^.Name) = LowStrg(Copy(FMSetup.DIZ,K,J-K+1))) then PF^.Selected := Off;
         {$ENDIF}
      end;
     end;
{JO}

   if PF^.Selected then
   begin
     P^.SelectedLen := P^.SelectedLen + PF^.Size;
     P^.PackedLen := P^.PackedLen + PF^.PSize;
     Inc(P^.SelNum);
   end;
  end;
 DrawViews(P);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure CM_CompareDirs(AFP, IP: Pointer);
 var DD, InThat: PFilesCollection;
     OSM1, OSM2: Word;
     I, J: LongInt;
     PF: PFileRec;
     DT: record O: Word; FMask: string; S: Word; end; {-$VOL added, DataCompBoy changed}
     Info: PView;
     P: PFilePanelRoot absolute AFP;
{$IFDEF Win32}
     D: PDialog; {JO}
     P1: PView;  {JO}
{$ENDIF}
 const
   CompareName = 1;
   CompareDat = 2;
   CompareLength = 4;
   CompareContens = 8;
{$IFDEF Win32}
 function IsCheckboxes( P: PView ): boolean; {$IFDEF BIT_16}far;{$ENDIF}
   begin
     IsCheckboxes := TypeOf(P^) = TypeOf(TCheckBoxes);
   end;
{$ENDIF}
 procedure Cmp1(PF: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
   function Cmp(AP: PFileRec): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
   begin
    UpdateWriteView(Info);
    Cmp := (AP^.Attr and Directory = 0) and (P^.Files^.Compare(PF, AP) = 0);
   end;
 begin
  UpdateWriteView(Info);
  if DT.S = 0 then PF^.Selected := Off;
  if (PF^.Attr and Directory = 0) and
{$IFNDEF OS2}
     (InFilter(GetLFN(PF^.LFN),DT.FMask)) and {-$VOL}
{$ELSE}
     (InFilter(PF^.Name ,DT.FMask)) and {-$VOL}
{$ENDIF}
     (InThat^.FirstThat(@Cmp) = nil) then PF^.Selected := DT.S = 0;
 end;

 label 1,2;

begin
 DT.FMask:=x_x; DT.O := 3; DT.S := 0;
{$IFDEF Win32}
 D := PDialog(LoadResource(dlgCompareDirs));
   if D = nil then Exit;
 if (not PFilesCollection(IP)^.LFNActive) or ((P^.Drive^.Flags and psShowLongName) = 0) then
    begin  {JO: делаем недоступным чекбокс регистрочувствительности}
      P1 := D^.FirstThat(@IsCheckboxes);
      PCheckBoxes(P1)^.SetButtonState(16, False);
    end;
 D^.SetData(DT);
 if Desktop^.ExecView(D) = cmCancel then begin Dispose(D,Done); Exit end;
 D^.GetData(DT);
{$ELSE}
if ExecResource( dlgCompareDirs, DT) <> cmOK then Exit;
{$ENDIF}
 if DelSpaces(DT.FMask)='' then DT.FMask := x_x;

 DD := IP;
 OSM1 := PFilesCollection(P^.Files)^.SortMode;
 OSM2 := DD^.SortMode;
 PFilesCollection(P^.Files)^.SortMode := NumSortModes+DT.O;
 DD^.SortMode := NumSortModes+DT.O;

 Info := NIL;
 If DT.O and 8 <> 0 then
 begin
   Info := WriteMsg( GetString( dlComparing ));
 end;

 InThat := DD;
 P^.Files^.ForEach(@Cmp1);
 InThat := P^.Files;
 DD^.ForEach(@Cmp1);

 PFilesCollection(P^.Files)^.SortMode := OSM1;
 DD^.SortMode := OSM2;
 Info^.Free;
end;
        {-DataCompBoy-}

procedure CM_EraseFiles;
 var FC: PFilesCollection;
     P: PFilePanelRoot absolute AFP;
begin
 FC := GetSelection(P, Single);
 if (FC = nil) or (P^.Drive = nil) then Exit;
 P^.Drive^.EraseFiles(FC);
 FC^.DeleteAll;
 Dispose(FC,Done);
 P^.SetDirName;
 P^.SendLocated;
end;


procedure CM_MakeList;
 var FC: PCollection;
     P: PFilePanelRoot absolute AFP;
begin
 if (P^.Files^.Count = 0) then Exit;
 if (P^.SelNum = 0) then
  while SelectFiles(P, On,Off) do;
 if (P^.SelNum = 0) then Exit;
 if (ActivePanel = P) and (P^.Drive^.DriveType = dtDisk)
   then CurrentDirectory := P^.DirectoryName;
 FC := GetSelection(P, Off); if FC = nil then Exit;
 MakeListFile(P, FC);
 FC^.DeleteAll;
 Dispose(FC,Done);
 P^.SetDirName;
end;

        {-DataCompBoy-}
procedure CM_SetAttributes;
 var D: record
{$IFDEF USEANSISTRING}
          T: String;
          D: String;
{$ELSE}
          T: String[8];
          D: String[10];
{$ENDIF}
          S, C: Word;
        end;
     I,UU,DD: LongInt;
     K: Word;
     S: String;
     F: lFile;
     R: TRect;
     PInfo: PWhileView;
     DTT, DTT1: LongInt;
     DT, DT1: DateTime;
     DateSet,TimeSet : boolean ;
     Res: Word;
     Dlg: PDialog;
     P: PFilePanelRoot absolute AFP;
     PF: PFileRec;
     ok: Boolean;

  Function CutNumber( var S : String ): word ;
  var S1  : string[10];
      i,j : Integer;
     begin
        CutNumber := 0;
        if not ok then Exit;
        if S = '' then
        begin
          ok := False;
          Exit;
        end;
        S1 := '';
        while (not (S[1] in ['0'..'9'])) and (S<>'') do Delete(S, 1, 1); {DelFC(S);}
        while (S[1] in ['0'..'9']) and (S<>'') do
        begin
          S1 := S1+S[1]; {AddStr(S1,S[1]);}
          Delete(S, 1, 1); {DelFC(S);}
        end;
        Val(s1,i,j);
        ok := j=0;
        CutNumber := I;
      end ;
  procedure DoHideInputLines( AP: PView ); {$IFDEF BIT_16}far;{$ENDIF}
  begin
    If ( TypeOf( AP^ ) = TypeOf( TInputLine ))
    or ( TypeOf( AP^ ) = TypeOf( TLabel )) then AP^.State := ( AP^.State or sfDisabled ) and not sfVisible;
  end;
begin
 if (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArc) then Exit;
 FillChar(D, Sizeof(D), 0);
 if (P^.SelNum > 0) and not Single then
  begin if ExecResource( dlgFilesAttr, D) <> cmOK then Exit end
  else
  begin
   Single := On;
   PF := P^.Files^.At(CurPos);
{$IFNDEF OS2}
  {$IFDEF OS_DOS}
   If PF^.Name[1] = '.' then Exit;
   S := MakeNormName(PF^.Owner^, MakeFileName(PF^.Name));
  {$ELSE}
   If PF^.LFN = '..' then Exit;
   S := MakeNormName(PF^.Owner^, GetLFN(PF^.LFN));
  {$ENDIF}
{$ELSE}
   If PF^.Name = '..' then Exit;
   S := MakeNormName(PF^.Owner^, PF^.Name);
{$ENDIF}
   AddStr(S, #0); SetLength(S, Length(S)-1);
   Dlg := PDialog( LoadResource( dlgFileAttr ));
   D.C := GetFileAttr(S);
{$IFNDEF OS2}
   if D.C and Directory = 0 then
    begin
     lAssignFile(F, S);
     FileMode := $40;
     ClrIO; lResetFile(F,1);
     if IOResult <> 0 then Exit;
     GetFTime(F.F, DTT); Close(F.F);
{$ELSE}
    DTT := GetFileAge (S, ageLastWrite);
{$ENDIF}
     UnpackTime(DTT, DT);
     D.T := SStr(DT.Hour, 2, '0')+':'+SStr(DT.Min, 2, '0')+':'+SStr(DT.Sec, 2, '0');
     D.D := ItoS(DT.Day)+'-'+SStr(DT.Month, 2, '0')+'-'+ItoS(DT.Year);
{$IFNDEF OS2}
    end else Dlg^.ForEach(@DoHideInputLines)
{$ENDIF}
   ;
   D.S:=D.C;
   Dlg^.SetData( D );
   if Desktop^.ExecView( Dlg ) <> cmOK then begin
     Dispose( Dlg, Done );
     Exit;
   end;
   Dlg^.GetData( D );
   Dispose( Dlg, Done );
   D.C :={39}$FF;
  end;

 DTT1 := $FFFFFF;

 DateSet := D.D <>'';
 TimeSet := D.T <>'';

 with DT1 do
  begin
   ok := TimeSet;
   Hour := CutNumber(D.T);
   Min  := CutNumber(D.T);
   if ok then
     begin
       Sec := CutNumber(D.T);
       ok := True;
     end;
   TimeSet := ok and not ((Hour > 24) or (Min>59) or (sec>59));


   ok := DateSet;
   Day  := CutNumber(D.D);
   Month:= CutNumber(D.D);
   Year := CutNumber(D.D);
   DateSet := ok and not ((Day>31 ) or ( Day<1 ) or (Month>12) or (Month<1));

   if DateSet then
     if Year < 80 then Inc(Year,2000)
        else if Year < 100 then Inc(Year,1900) ;

  end;

 R.Assign(1,1,26,8);
 New(PInfo, Init(R));
 PInfo^.Top := GetString(dlSetAttr);
 PInfo^.Bottom := '';
 PInfo^.SetState(sfShadow, On);
 Desktop^.Insert(PInfo);

 if Single then begin UU := CurPos+1; DD := UU end
           else begin UU := 1; DD := P^.Files^.Count end;
 for I := UU to DD do
  begin
   PF := P^.Files^.At(I-1);
   if PF^.Selected or Single then
    begin
     {$IFNDEF OS2}
     S := Cut(GetLFN(PF^.LFN), 40);
     {$ELSE}
     S := Cut(PF^.Name, 40);
     {$ENDIF}
     PInfo^.Write(1, S);
     {$IFNDEF OS2}
       {$IFDEF OS_DOS}
     S := MakeNormName(PF^.Owner^, MakeFileName(PF^.Name));
       {$ELSE}
     S := MakeNormName(PF^.Owner^, GetLFN(PF^.LFN));
       {$ENDIF}
     {$ELSE}
     S := MakeNormName(PF^.Owner^, PF^.Name);
     {$ENDIF}
     {$IFNDEF OS2}
     lAssignFile(F, S);
     {$ENDIF}
     AddStr(S, #0); SetLength(S, Length(S)-1);
     K := (GetFileAttr(S) and (not D.C)) or D.S;
     if {$IFNDEF OS2} (K and Directory = 0) and {$ENDIF} TimeSet or DateSet then
      begin
       {$IFNDEF OS2}
       FileMode := $42 ;
       lSetFAttr(F,Archive);
       lResetFile(F,1);
       GetFTime(F.F, DTT);
       {$ELSE}
       DTT := GetFileAge (S, ageLastWrite);
       {$ENDIF}
       UnPackTime(DTT, DT);
       if TimeSet then begin DT.Hour := DT1.Hour; DT.Sec := DT1.Sec; DT.Min := DT1.Min; end;
       if DateSet then begin DT.Year := DT1.Year; DT.Day := DT1.Day; DT.Month := DT1.Month; end;
       PackTime(DT, DTT1);
       DT.Sec := 0;
       UnPackTime(DTT1, DT);
       {$IFNDEF OS2}
       SetFTime(F.F, DTT1);
       Close(F.F);
       {$ELSE}
       SetFileAge(S, DTT1, ageLastWrite);
       {$ENDIF}
      end;
     SetFileAttr(S, K and not Directory and not VolumeID);
     if not Single then PF^.Selected := Off;
    end;
  end;
 Desktop^.Delete(PInfo);
 Dispose(PInfo,Done);
 if (ActivePanel = P) and (P^.Drive^.DriveType = dtDisk)
   then CurrentDirectory := P^.DirectoryName;
 RereadDirectory(P^.DirectoryName);
 if P^.Drive^.DriveType = dtDisk then GlobalMessage(evCommand, cmRereadInfo, nil);
 GlobalMessage(evCommand, cmRereadTree, @P^.DirectoryName);
 P^.SetDirName;
end;
        {-DataCompBoy-}

procedure CM_SetShowParms;
 var I: Record Prm: Word; LFNLen: String[2]; EXTLen: String[2] end;
     J,K,Old: Word;
     Idx: TDlgIdx;
     P: PFilePanelRoot absolute AFP;
begin
 Old := P^.Drive^.Flags;
 case P^.Drive^.DriveType of
   dtFind, dtTemp: Idx := dlgFindParms;
   dtArc: Idx := dlgArcParms;
   else Idx := dlgDiskParms;
 end;
 I.Prm := P^.Drive^.Flags;
 I.LFNLen := ItoS(P^.Drive^.LFNLen);
 I.EXTLen := ItoS(P^.Drive^.EXTLen);
 J := ExecResource( Idx, I );
 case J of
  cmOK: begin
         P^.Drive^.Flags := I.Prm;
         J:=StoI(I.LFNLen);
         K:=StoI(I.EXTLen);
         if J>=5 then
          begin
           if (J-K>=5) then
            begin
             P^.Drive^.LFNLen:=J;
             P^.Drive^.EXTLen:=K;
            end
           else
            begin
             P^.Drive^.LFNLen:=J;
             P^.Drive^.EXTLen:=J-5;
            end;
          end;
        end;
  cmYes: P^.Drive^.GetParam(1);
  cmNo:  P^.Drive^.GetParam(2);
  else Exit;
 end;
 P^.DeltaX := 0;
 P^.Drive^.Flags := P^.Drive^.Flags;
 if ((P^.Drive^.DriveType = dtDisk) or (P^.Drive^.DriveType = dtArvid))
     and ((P^.Drive^.Flags xor Old) and (psShowDescript+psShowLongDesc) <> 0) then P^.RereadDir;
 DrawViews(P);
 P^.SendLocated;
 DrawViews(P);
end;

procedure CM_CopyTemp;
 var FC: PFilesCollection;
     C: TCopyRec;
     P: PFilePanelRoot absolute AFP;
begin
 FC := GetSelection(P, Off);
 C.Owner := P;
 C.FC := FC;
 if FC = nil then Exit;
 GlobalEvent(evBroadcast, cmCopyToTemp, @C);
 FC^.DeleteAll;
 Dispose(FC,Done);
end;

        {-DataCompBoy-}
procedure CM_ArchiveFiles;
  var PC: PCollection;
      S: String;
      P: PFilePanelRoot absolute AFP;
    Dir: DirStr;
    Name: NameStr;
    Ext: ExtStr;
begin
  if P^.Drive^.DriveType >= dtArc then Exit;
  PC := GetSelection(P, Off);
  if PC = nil then Exit;
  if PC^.Count = 1 then
    S := {$IFNDEF OS2}MakeFileName{$ENDIF}(PFileRec(PC^.At(0))^.Name)
  else if P^.Drive^.DriveType = dtDisk then
    S := GetName(P^.DirectoryName)
  else
    S := '';
{Cat}
(*
  FSplit(S, Dir, S, Ext); {AK155: не уверен, что это нужно }
  MakeArchive(S, PC, Off, P);
*)
  FSplit(S, Dir, Name, Ext); {AK155: не уверен, что это нужно }
  MakeArchive(Name, PC, Off, P);
{/Cat}
  PC^.DeleteAll;
end;
        {-DataCompBoy-}

{$IFDEF Printer}
procedure CM_Print;
  var N: Integer;
      P: PFilePanelRoot absolute AFP;
begin
  if (P^.Drive^.DriveType  in [ dtDisk, dtFind, dtTemp ] ) then
     PrintFiles(GetSelection(P, False), P);
end;
{$ENDIF}

procedure CM_ToggleDescriptions;
var P: PFilePanelRoot absolute AFP;
begin
  case P^.Drive^.DriveType of
    dtDisk,
    dtArvid: begin
               P^.Drive^.Flags := P^.Drive^.Flags xor psShowDescript;
               P^.Drive^.Flags := P^.Drive^.Flags;
               P^.DeltaX := 0;
               P^.RereadDir;
               DrawViews(P);
             end;
  end;
end;

procedure CM_ToggleLongNames;
var P: PFilePanelRoot absolute AFP;
begin
{$IFNDEF OS2}
  P^.Drive^.Flags := P^.Drive^.Flags xor psShowLongName;
  P^.Drive^.Flags := P^.Drive^.Flags;
{$ELSE}
  P^.Drive^.ShowLogNames := not P^.Drive^.ShowLogNames;
{$ENDIF}
  P^.DeltaX := 0;
  P^.RereadDir;
  DrawViews(P);
end;

procedure CM_ToggleShowMode;
var P: PFilePanelRoot absolute AFP;
begin
  P^.DeltaX := 0;
  if P^.Drive^.Param <> 2 then
      begin
        P^.Drive^.OldParam := P^.Drive^.Param;
        P^.Drive^.GetParam(2)
      end
    else
      if P^.Drive^.OldParam <> 2 then P^.Drive^.GetParam(P^.Drive^.OldParam) else
        P^.Drive^.GetParam(1);
  P^.RereadDir;
  DrawViews(P);
 {Message(Desktop, evCommand, cmMaxi, nil);}
end;

type
    PDragger = ^TDragger;
    TDragger = object(TView)
     Text: PString;
     constructor Init(R: TRect; AText: String);
     procedure Draw; virtual;
     destructor Done; virtual;
    end;

procedure DragMover;
 var R: TRect;
     Mover: PView;
     Event: TEvent;
     P: PPoint absolute AP;
     FC: PFilesCollection absolute AFC;
     C: PCopyRec absolute AC;

function ContainsMouse(P: PView): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
begin
  ContainsMouse := (P^.State and sfVisible <> 0) and
    P^.MouseInView(R.A);
end;

begin
{Application^.BFSpeed;}
 Desktop^.MakeLocal(P^, R.A);
 Mover := New(PDragger, Init(R, Text));
 Desktop^.Insert(Mover);
 Desktop^.GetExtent(R);
 Event.Where := P^; Event.What := evMouseDown; Event.Buttons := mbLeftButton;
 Event.Double := False;
 Mover^.DragView(Event, dmDragMove, R, Mover^.Size, Mover^.Size);
 R.A := Mover^.Origin;
 Mover^.Free;
 C^.FC := FC; Desktop^.MakeGlobal(R.A, R.A); C^.Where := R.A;
 Message(Desktop^.FirstThat(@ContainsMouse), evBroadcast, cmDropped, C);
{Application^.EFSpeed;}
 Dispose(FC,Done);
end;

constructor TDragger.Init;
begin
 AText := ' '+AText+' ';
 R.B.X := R.A.X + Length(AText);
 R.B.Y := R.A.Y + 1;
 inherited Init(R);
 Options := Options or ofTopSelect;
 Text := NewStr(AText);
 SetState(sfShadow, True);
end;

procedure TDragger.Draw;
 var B: TDrawBuffer;
     C: Word;
begin
 C := $3B30;
 MoveStr(B, Text^, C);
 WriteLine(0,0,Size.X, Size.Y, B);
end;

destructor TDragger.Done;
begin
 DisposeStr(Text);
 inherited Done;
end;

procedure CM_DragDropper;
 var C: TCopyRec;
     FC: PFilesCollection;
     I: LongInt;
     S: String;
     P: PFilePanelRoot absolute AFP;

begin
 C.Owner := P;
 if (P^.Files^.Count = 0) or (P^.Files^.Count <= CurPos) then Exit;
{if (StartupData.Slice2 and osuReleaseDrag = 0) then Application^.BFSpeed;}
 if (PFileRec(P^.Files^.At(CurPos))^.Selected) or
    (typeof(P^) <> typeof(TFilePanel))
  then FC := GetSelection(P, Off)
  else
{$IFNDEF OS2}
    if PFileRec(P^.Files^.At(CurPos))^.Name[1] <> '.'
{$ELSE}
    if PFileRec(P^.Files^.At(CurPos))^.Name <> '..'
{$ENDIF}
    then begin
        New(FC, Init(10,10));
        FC^.Insert(P^.Files^.At(CurPos));
       end else Exit;
 for I := 1 to FC^.Count do FC^.AtPut(I-1, CopyFileRec(FC^.At(I-1)));
 if FC^.Count = 1 then
  {$IFNDEF OS2}
   if P^.Drive^.Flags and psShowLongName >0  then
       S := Cut(GetLFN(PFileRec(FC^.At(0))^.LFN),20)
   else S := MakeFileName(PFileRec(FC^.At(0))^.Name)
  {$ELSE}
   S := Cut(PFileRec(FC^.At(0))^.Name,20)
  {$ENDIF}
  else S := ItoS(FC^.Count)+GetString(dlSelectedFiles);
 DragMover(@TEvent(Ev^).Where, S, FC, @C);
{if (StartupData.Slice2 and osuReleaseDrag = 0) then Application^.EFSpeed;}
end;

procedure CM_Dropped;
var
  P: PFilePanelRoot absolute AFP;
  MPos: TPoint;
  I,J,K: LongInt;
  Ev: TEvent;
  S: string;
begin
  if ReflectCopyDirection
   then RevertBar:=(Message(Desktop, evBroadCast, cmIsRightPanel, P) <> Nil)
   else RevertBar:=false;
  MPos := PCopyRec(EI)^.Where;
  P^.MakeLocal(MPos, MPos);
  I := P^.Delta + (MPos.X div P^.LineLength)* (P^.Size.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0))
       + MPos.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0);
  if (PCopyRec(EI)^.Owner = AFP) and
     ((MPos.Y < 0) or (I < 0) or (I >= P^.Files^.Count) or
      ((MPos.Y=0) and (FMSetup.Show and fmsColumnTitles>0)) or
      (PFileRec(P^.Files^.At(I))^.Attr and Directory = 0)) then Exit;
  CopyDirName := P^.DirectoryName;
  if PCopyRec(EI)^.Owner <> nil then
  begin
   if (P^.Drive^.DriveType = dtArc) then
       begin
          CopyDirName := P^.Drive^.GetRealName;
          SkipCopyDialog := Confirms and cfMouseConfirm = 0;
          Message(PCopyRec(EI)^.Owner, evBroadcast, cmCopyCollection,
                  PCopyRec(EI)^.FC);
          SkipCopyDialog := Off;
          Exit;
       end;
   Ev.What := evBroadcast;
   Ev.Command := cmUnarchive;
   Ev.InfoPtr := EI;
   PCopyRec(EI)^.Owner^.HandleEvent(Ev);
   if Ev.What = evNothing then Exit;
  end;
  if (I < P^.Files^.Count) and (I >= 0) then
    begin
     S := P^.DirectoryName;
     if ((MPos.Y > 0) or ((FMSetup.Show and fmsColumnTitles=0) and (MPos.Y=0)))
        and (PFileRec(P^.Files^.At(I))^.Attr and Directory <> 0)
      then
    {$IFNDEF OS2}
      {$IFDEF OS_DOS}
      S := MakeNormName(S, MakeFileName(PFileRec(P^.Files^.At(I))^.Name));
      {$ELSE}
      S := MakeNormName(S, GetLFN(PFileRec(P^.Files^.At(I))^.LFN));
      {$ENDIF}
    {$ELSE}
      S := MakeNormName(S, PFileRec(P^.Files^.At(I))^.Name);
    {$ENDIF}
     CopyDirName := S;
    end;
   SkipCopyDialog := Confirms and cfMouseConfirm = 0;
   if SkipCopyDialog then
     begin
       S := CopyDirName; if (Length(S) > 3) and (S[Length(S)] = '\') then SetLength(S, Length(S)-1);
       UpStr(S);
       for J := 0 to PCopyRec(EI)^.FC^.Count-1 do
         with PFileRec(PCopyRec(EI)^.FC^.At(J))^ do
           if (Attr and Directory <> 0) and (UpStrg(MakeNormName(Owner^, Name)) = S) then Exit;
     end;
   P^.Drive^.CopyFilesInto(PCopyRec(EI)^.FC, PCopyRec(EI)^.Owner,
                           ShiftState and 7 <> 0);
   SkipCopyDialog := Off;
end;

        {-DataCompBoy-}
procedure CM_RenameSingleL;
  var PIF: PInputFName;
      R: TRect;
      S, S2: String;
      DosE: Word;
      SSS: String;
      PF: PFileRec;
{$IFNDEF OS2}
      Nm: Str12;
      Sn: Str12;
{$ELSE}
      Nm: String;
      Sn: String;
{$ENDIF}
      P: PFilePanelRoot absolute AFP;
      Event: PEvent absolute PEV;
      ScrollBarValue: Integer;

begin
  if (P^.Files = nil) then Exit;
  ScrollBarValue := P^.ScrollBar^.Value;
  if ScrollBarValue >= P^.Files^.Count then Exit;
  PF := P^.Files^.At(ScrollBarValue);
{$IFNDEF OS2}
  {$IFDEF OS_DOS}
  if (PF^.Name[1] = '.') or
  {$ELSE}
  if (PF^.LFN = '..') or
  {$ENDIF}
{$ELSE}
  if (PF^.Name = '..') or
{$ENDIF}
     not (P^.Drive^.DriveType in [dtDisk, dtTemp, dtFind, dtList]) then Exit;
 {$IFNDEF OS2}
  S:= GetLFN(PF^.LFN);
  Nm := MakeFileName(PF^.Name);
 {$ELSE}
  S:= PF^.Name;
  Nm := S;
 {$ENDIF}
  S2 := S;
  if P^.Drive^.LFNLen < 250 then
      R.Assign(0,0,P^.Drive^.LFNLen+1,1)
    else
      R.Assign(P^.Drive^.CalcLengthWithoutName,0,255,1);
  R.Move(P^.LastCurPos.X, P^.LastCurPos.Y);
  R.Move(P^.Origin.X, 0); dec(R.A.X);
  if R.B.X > P^.Size.X+P^.Origin.X+1 then R.B.X := P^.Size.X+P^.Origin.X+1;
  if R.B.X-P^.Origin.X-P^.Size.X=0 then Inc(R.B.X);
  New(PIF, Init(R, 255)); {PIF^.State := PIF^.State or sfCursorIns;}
  if P^.Origin.X-R.A.X=1
   then begin
    PIF^.LC:=#186;
    PIF^.CL:=RK;
   end
  else begin
   PIF^.LC:=#179;
   PIF^.CL:=P^.GetColor(2);
  end;
  if R.B.X-P^.Origin.X-P^.Size.X=1
   then begin
    PIF^.RC:=#186;
    PIF^.CR:=RK;
   end
  else begin
   PIF^.RC:=#179;
   PIF^.CR:=P^.GetColor(2);
  end;
  if (PF^.TType=0) or (P^.PanelFlags and fmiHiliteFiles = 0)
   then PIF^.C1:=P^.GetColor(1)
   else PIF^.C1:=P^.GetColor(6+PF^.TType);
  PIF^.C2:=PIF^.C1;
  PIF^.C3:=P^.GetColor(4);
  P^.Owner^.Insert(PIF);
  {$IFDEF RecodeWhenDraw}
  S := CharToOemStr(S);
  {$ENDIF}
  PIF^.SetData(S);
  PIF^.SetValidator(New(PFilterValidator, Init([#32..#255]-IllegalCharSet-['\','/','*','?','"'])));
  PIF^.SelectAll(false);
  R.A.X := P^.Owner^.ExecView(PIF); PIF^.GetData(S);
  if S[Length(S)]='.' then SetLength(S, Length(S)-1);
  {$IFDEF RecodeWhenDraw}
  if S = CharToOemStr(S2) then R.A.X := cmCancel;
  S := OemToCharStr(S);
  {$ELSE}
  if S = S2 then R.A.X := cmCancel;
  {$ENDIF}
  if R.A.X <> cmCancel then
    begin
      lGetDir(0, SSS);
      lChDir(PF^.Owner^);
      lChangeFileName(S2, S);
      DOSE:=IOResult;
      if DOSE <> 0 then
          MessFileNotRename(S2, S, DOSE)

        else begin
               {$IFNDEF OS2}
               PF^.Name := Norm12(GetName(lfGetShortFileName(S)));
               DelLFN(PF^.LFN);
               PF^.LFN := AddLFN(S);
               {$ELSE}
               PF^.Name := {GetName(}S{)};
               {$ENDIF}
               if PF^.Attr and Directory <> 0 then UpStr(PF^.Name)
                   else begin
                          LowStr(PF^.Name);
                          if PF^.Attr and SysFile <> 0 then PF^.Name[1] := UpCase(PF^.Name[1]);
                        end;
               {$IFNDEF OS2}
               PF^.Name[9] := ' ';
               Sn := MakeFileName(PF^.Name);
               {$ELSE}
               Sn := PF^.Name;
               {$ENDIF}
               S2 := GetPossibleDizOwner(1);
               if S2 <> '' then begin
                S2 := GetDizOwner(PF^.Owner^, S2, Off);
                ReplaceDIZ(S2, Nm, @Sn, nil);
               end;
             end;
      lChDir(SSS);
      P^.Owner^.Delete(PIF);
      Dispose(PIF,Done);
      GlobalMessage(evCommand, cmRereadDir, PF^.Owner);
      if R.A.X <> cmOK then
         begin
           Event^.What := evKeyDown; Event^.KeyCode := R.A.X;
           Event^.InfoPtr := nil; P^.PutEvent(Event^);
         end;
    end else PIF^.Free;
end;
        {-DataCompBoy-}

{$IFNDEF OS2}
procedure CM_RenameSingleS;
  var PIF: PInputSFName;
      R: TRect;
      S, S2: String[13];
      N1,N2: Array [0..12] of Char;
      DosE: Word;
      SSS: String;
      PF: PFileRec;
      Nm: String[20];
      P: PFilePanelRoot absolute AFP;
      Event: PEvent absolute PEV;
      ScrollBarValue: Integer;
      {$IFDEF BIT_32}FFFF: File;{$ENDIF}

  procedure UpdateDesc;
    var F, F1: lText;
        I: Integer;
        S: String;
        N: String[20];
  begin
    N := MakeFileName(PF^.Name); UpStr(N);
    lAssignText(F, PF^.DIZ^.Owner^); ClrIO;
    lResetText(F);
    if IOResult <> 0 then Exit;
    lAssignText(F1, GetPath(PF^.DIZ^.Owner^)+'$@$DN$@$.$$$');
    lRewriteText(F1);
    if IOResult = 0 then
      begin
        while not EOF(F.T) do
          begin
            ReadLn(F.T, S);
            if (S <> '') and (S[1] <> ' ') then
              begin
                I := Pos(' ', S); if I = 0 then I := Length(S)+1;
                if UpStrg(Copy(S, 1, I-1)) = Nm then
                  begin
                    Delete(S, 1, I-1);
                    Insert(N, S, 1);
                  end;
              end;
            WriteLn(F1.T, S);
          end;
        Close(F1.T); ClrIO; Close(F.T);
        lEraseText(F); lRenameText(F1, PF^.DIZ^.Owner^);
      end;
    Close(F.T); ClrIO;
  end;

begin
  if (P^.Files = nil) then Exit;
  ScrollBarValue := P^.ScrollBar^.Value;
  if ScrollBarValue >= P^.Files^.Count then Exit;
  PF := P^.Files^.At(ScrollBarValue);
  if (PF^.Name = '..') or
     not (P^.Drive^.DriveType in [dtDisk, dtTemp, dtFind, dtList]) then Exit;
  Nm := MakeFileName(PF^.Name); UpStr(Nm);
  R.Assign(0,0,12,1);
  R.Move(P^.LastCurPos.X, P^.LastCurPos.Y);
  R.Move(P^.Origin.X, 0);
  S := MakeFileName(PF^.Name); S2 := S;
  New(PIF, Init(R, 13)); PIF^.DrawShift := 0; P^.Owner^.Insert(PIF);
  S := PF^.Name; S[9] := '.';
  {$IFDEF RecodeWhenDraw}
  S := CharToOemStr(S);
  {$ENDIF}
  PIF^.SetData(S);{PIF^.State := PIF^.State xor sfCursorIns;}
  PIF^.SetValidator(New(PFilterValidator, Init([#32..#255]-
                                              ['+','|','>','<',']','[','*','?'])));
  R.A.X := P^.Owner^.ExecView(PIF); PIF^.GetData(S);
  {$IFDEF RecodeWhenDraw}
  if UpStrg(MakeFileName(S)) = UpStrg(CharToOemStr(S2)) then R.A.X := cmCancel;
  S := OemToCharStr(S);
  {$ELSE}
  if UpStrg(MakeFileName(S)) = UpStrg(S2) then R.A.X := cmCancel;
  {$ENDIF}
  S := MakeFileName(S);
  if R.A.X <> cmCancel then
    begin
      GetDir(0, SSS);
      ChDir(PF^.Owner^);
      {$IFNDEF BIT_32}
      FillChar(N1, SizeOf(N1), 0);
      FillChar(N2, SizeOf(N2), 0);
      Move(S[1], N2, Length(S));
      Move(S2[1], N1, Length(S2));
      asm
       push ds
       push bp
       push ss
       mov  ax, ss
       mov  es, ax
       mov  ds, ax
       lea  di,n2
       lea  dx,n1
       mov  ah,$56
       int  $21
       jc   @@1
       xor  ax, ax
      @@1:
       mov  DOSE, ax
       pop  ss
       pop  bp
       pop  ds
      end;
      {$ELSE}
      Assign(FFFF, S2);
      Rename(FFFF, S);
      DOSE := DosError;
      {$ENDIF}
      if DOSE <> 0 then
          MessFileNotRename(S2, S, DOSE)

        else begin
               PF^.Name := Norm12(S);
               if PF^.Attr and Directory <> 0 then UpStr(PF^.Name)
                   else begin
                          LowStr(PF^.Name);
                          if PF^.Attr and SysFile <> 0 then PF^.Name[1] := UpCase(PF^.Name[1]);
                        end;
               PF^.Name[9] := ' ';
               if PF^.DIZ <> nil then UpdateDesc;
             end;
      ChDir(SSS);
      P^.Owner^.Delete(PIF);
      Dispose(PIF,Done);
      GlobalMessage(evCommand, cmRereadDir, PF^.Owner);
      if R.A.X <> cmOK then
         begin
           Event^.What := evKeyDown; Event^.KeyCode := R.A.X;
           Event^.InfoPtr := nil; P^.PutEvent(Event^);
         end;
    end else PIF^.Free;
end;
{$ENDIF OS2}

        {-DataCompBoy-}
procedure CM_RenameSingleDialog;
  var R: TRect;
      S, S2: String;
      DosE: Word;
      SSS: String;
      PF: PFileRec;
{$IFNDEF OS2}
      Nm: Str12;
      Sn: Str12;
{$ELSE}
      Nm: String;
      Sn: String;
{$ENDIF}
      P: PFilePanelRoot absolute AFP;
      Event: PEvent absolute PEV;
      ScrollBarValue: Integer;
      U: lFile;

begin
  if (P^.Files = nil) then Exit;
  ScrollBarValue := P^.ScrollBar^.Value;
  if ScrollBarValue >= P^.Files^.Count then Exit;
  PF := P^.Files^.At(ScrollBarValue);
{$IFNDEF OS2}
  {$IFDEF OS_DOS}
  if (PF^.Name[1] = '.') or
  {$ELSE}
  if (PF^.LFN = '..') or
  {$ENDIF}
{$ELSE}
  if (PF^.Name = '..') or
{$ENDIF}
     not (P^.Drive^.DriveType in [dtDisk, dtTemp, dtFind, dtList]) then Exit;
{$IFNDEF OS2}
  S:= GetLFN(PF^.LFN);
  Nm := MakeFileName(PF^.Name);
{$ELSE}
  S:= PF^.Name;
  Nm := S;
{$ENDIF}
  S2 := S;
  {$IFDEF RecodeWhenDraw}
  S := CharToOemStr(S);
  {$ENDIF}
  R.A.X := ExecResource(dlgRenFl, S);
  {$IFDEF RecodeWhenDraw}
  S := OemToCharStr(S);
  {$ENDIF}
  If R.A.X = cmCancel then Exit;
  If R.A.X = cmYes then UpStr(S);
  If R.A.X = cmNo then LowStr(S);
  if S[Length(S)]='.' then SetLength(S, Length(S)-1);

  if S = S2 then R.A.X := cmCancel;
  if R.A.X <> cmCancel then
    begin
      lGetDir(0, SSS);
      lChDir(PF^.Owner^);
      lChangeFileName(S2, S);
      DOSE:=IOResult;
      if DOSE <> 0 then
          MessFileNotRename(S2, S, DOSE)

        else begin
               {$IFNDEF OS2}
               PF^.Name := Norm12(GetName(lfGetShortFileName(S)));
               DelLFN(PF^.LFN);
               PF^.LFN := AddLFN(S);
               {$ELSE}
               PF^.Name := GetName(S);
               {$ENDIF}
               if PF^.Attr and Directory <> 0 then UpStr(PF^.Name)
                   else begin
                          LowStr(PF^.Name);
                          if PF^.Attr and SysFile <> 0 then PF^.Name[1] := UpCase(PF^.Name[1]);
                        end;
             {$IFNDEF OS2}
               PF^.Name[9] := ' ';
               Sn := MakeFileName(PF^.Name);
             {$ELSE}
               Sn := PF^.Name;
             {$ENDIF}
               S2 := GetPossibleDizOwner(1);
               if S2 <> '' then begin
                S2 := GetDizOwner(PF^.Owner^, S2, Off);
                ReplaceDIZ(S2, Nm, @Sn, nil);
               end;
             end;
      lChDir(SSS);
      GlobalMessage(evCommand, cmRereadDir, PF^.Owner);
      if R.A.X <> cmOK then
         begin
           Event^.What := evKeyDown; Event^.KeyCode := R.A.X;
           Event^.InfoPtr := nil; P^.PutEvent(Event^);
         end;
    end;
end;
        {-DataCompBoy-}

procedure CM_SortBy;
 var Menu: PMenu;
     PM: PMenuItem;
     N, W: Word;
     R: TRect;
     PV: PView;
     P: PFilePanelRoot absolute AFP;
     Cur: PFileRec;
     ScrollBarValue: Longint;
begin
  Menu := NewMenu(
    NewItem(GetString(dlSortName),    '',kbNoKey,cmSortName    ,0,
{$IFNDEF OS2}
    NewItem(GetString(dlSortLongName),'',kbNoKey,cmSortLongName,0,
{$ENDIF}
    NewItem(GetString(dlSortExt),     '',kbNoKey,cmSortExt     ,0,
{$IFNDEF OS2}
    NewItem(GetString(dlSortLongExt), '',kbNoKey,cmSortLongExt ,0,
{$ENDIF}
    NewItem(GetString(dlSortSize),    '',kbNoKey,cmSortSize    ,0,
    NewItem(GetString(dlSortDate),    '',kbNoKey,cmSortDate    ,0,
    NewItem(GetString(dlSortCrDate),  '',kbNoKey,cmSortCrDate  ,0,
    NewItem(GetString(dlSortLADate),  '',kbNoKey,cmSortLADate  ,0,
    NewItem(GetString(dlSortDIZ),     '',kbNoKey,cmSortDIZ     ,0,
    NewItem(GetString(dlSortType),    '',kbNoKey,cmSortType    ,0,
    NewItem(GetString(dlSortUnsorted),'',kbNoKey,cmSortUnsorted,0,
  nil)))))))))){$IFNDEF OS2})){$ENDIF};
  N := NumSortModes; W := 15;
  DeskTop^.GetExtent(R);
  R.A := P^.Origin; P^.Owner^.MakeGlobal(R.A, R.A); Desktop^.MakeLocal(R.A, R.A);
  if R.A.X < 0 then R.A.X := 0 else if R.A.X + W > R.B.X then R.A.X := R.B.X - W;
  if R.A.Y < 0 then R.A.Y := 0 else if R.A.Y + N + 2 > R.B.Y then R.A.Y := R.B.Y - N - 2;
  R.B.X := R.A.X + 100; R.B.Y := R.A.Y + 100; {R.B := R.A;}
  PV := New(PMenuBox, Init(R, Menu, nil));
  PV^.HelpCtx := hcSortBy;
  Menu^.Default := LookUpMenu(PMenuBox(PV)^.Menu, P^.SortMode+cmSortUnsorted, dfByCommand);
  N := DeskTop^.ExecView(PV);
  Dispose(PV,Done);
  DisposeMenu(Menu);
  if (N >= cmSortUnsorted) and (P^.SortMode+cmSortUnsorted <> N) then begin
    P^.SortMode := N-cmSortUnsorted;
    if P^.SortMode = psmUnsorted then P^.RereadDir;
    P^.Files^.SortMode := N-cmSortUnsorted;
    ScrollBarValue := P^.ScrollBar^.Value;
    if ScrollBarValue >= P^.Files^.Count then Exit;
    Cur := P^.Files^.At(ScrollBarValue);
    if P^.Files^.SortMode <> psmUnsorted then P^.Files^.Sort;
    For ScrollBarValue:=0 to P^.Files^.Count-1 do
     if P^.Files^.At(ScrollBarValue) = Cur then begin
      P^.ScrollBar^.SetValue(ScrollBarValue);
      Break;
     end;
    P^.DrawView;
    P^.SetDirName;
  end
end;

        {-DataCompBoy-}
function CM_ChangeDirectory;
var P: PFilePanelRoot absolute AFP;
    S: String;
begin
 CM_ChangeDirectory := '';
 if P^.Drive^.DriveType <> dtDisk then
   begin
     ClrIO; {!!!}
     lGetDir(0, S);
     if Abort then Exit;
   end else S := P^.DirectoryName;
 CM_ChangeDirectory :=
    ChangeDir(GetString(dlChangeDir), Byte(S[1]) - 64);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure CM_MakeDir;
 var Dr: String;
     Nm: String;
     Xt: String;
     PF: PFileRec;
     DN: String;
     I, J: LongInt;
     P: PFilePanelRoot absolute AFP;
begin
 P^.Drive^.MakeDir; if CreatedDir = '' then Exit;
 lFSplit(CreatedDir, Dr, Nm, Xt);
 if (Length(Dr) > 3) and (Dr[Length(Dr)] = '\') then SetLength(Dr, Length(Dr)-1);
 if Dr <> P^.DirectoryName then Exit; J := -1;
 DN:=UpStrg(GetName(CreatedDir));
 for I := 1 to P^.Files^.Count do
  begin PF := P^.Files^.At(I-1);
{$IFNDEF OS2}
        if UpStrg(GetLFN(PF^.LFN)) = GetName(DN) then
{$ELSE}
        if UpStrg(PF^.Name) = GetName(DN) then
{$ENDIF}
           begin J := I - 1; Break end;
  end;
 if J >= 0 then P^.ScrollBar^.SetValue(J);
 P^.ChDirName;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure CM_LongCopy;
 var S: String;
     P: PFilePanelRoot absolute AFP;
     PF: PFileRec;
begin
 if (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArc) then Exit;
 PF := P^.Files^.At(P^.ScrollBar^.Value);
 if PF^.Attr and Directory <> 0 then Exit;
{$IFNDEF OS2}
 S := MakeNormName(PF^.Owner^, GetLFN(PF^.LFN));
{$ELSE}
 S := MakeNormName(PF^.Owner^, PF^.Name);
{$ENDIF}
 LongCopy(S);
 if (ActivePanel = P) and (P^.Drive^.DriveType = dtDisk)
   then CurrentDirectory := P^.DirectoryName;
 RereadDirectory(P^.DirectoryName);
 if P^.Drive^.DriveType = dtDisk then
  begin
   GlobalMessage(evCommand, cmRereadInfo, nil);
   GlobalMessage(evCommand, cmRereadTree, @(P^.DirectoryName));
  end;
end;
        {DataCompBoy}

{AK155 перенесена в flpanelx.pas и развернута в обработчик cmDirBranch}
{}
procedure CM_Branch;
var PP: Pointer;
    P: PFilePanelRoot absolute AFP;
begin
  if P^.Drive^.DriveType = dtDisk then
    begin
      PP := OpenDirectory(P^.DirectoryName);
      if PP <> nil then Message(P, evCommand, cmInsertDrive, PP);
    end;
end;
{/AK155}

Procedure CaseAsIs  (var S: string; b, e: byte);
Begin
End;

Procedure CaseLow   (var S: string; b, e: byte);
Begin
 For b:=b To e Do s[b]:=LowCase(s[b]);
End;

Procedure CaseCap   (var S: string; b, e: byte);
Begin
 while (s[b] in BreakChars) and (b<=e) do inc(b);
 S[b]:=UpCase(S[b]); inc(b);
 while b<=e do begin
  S[b]:=LowCase(S[b]);
  inc(b);
 end;
End;

Procedure CaseCapAll(var S: string; b, e: byte);
Begin
 repeat
  While (b<=e) and (S[b] in BreakChars) do Inc(b);
  If b>e then break;
  S[b]:=UpCase(S[b]);
  While (b<e) and (not (S[b] in BreakChars)) do begin
   Inc(b); S[b]:=LowCase(S[b]);
  end;
 until b>=e;
End;

procedure CaseUp    (var S: string; b, e: byte);
begin
 for b:=b to e do s[b]:=UpCase(s[b]);
end;

procedure CM_ChangeCase(AFP: Pointer);
 Type ChPr = procedure (var S: string; b, e: byte);

 var FC: PFilesCollection;
     L: TNamesCaseOptions;
     P: PFilePanelRoot absolute AFP;
     Info: PWhileView;
     TT: TEventTimer;
     R: TRect;
     NameChange, ExtChange: ChPr;

 function DoChangeCase(PF: PFileRec):Boolean; {$IFDEF BIT_16}far;{$ENDIF}
 var S: String;
     SSS: String;
{$IFNDEF OS2}
     NM, Sn: Str12;
{$ELSE}
     NM, Sn: String;
{$ENDIF}
     i: integer;
     U: lFile;
 begin
  if TimerExpired(TT) then begin
   DispatchEvents(Info, Abort);
   NewTimerSecs(TT, 2);
  end;
  DoChangeCase := Abort;
  if Abort then exit;
  {$IFNDEF OS2}
  S := GetLFN(PF^.LFN);
  NM := MakeFileName(PF^.Name);
  {$ELSE}
  S := PF^.Name;
  NM := S;
  {$ENDIF}
  for I:=length(s) downto 1 do if s[i]='.' then break;
  {$IFDEF RecodeWhenDraw}
  if OemToCharStr(CharToOemStr(S)) = S then
  begin
  S := CharToOemStr(S);
  {$ENDIF}
  if s[i]='.' then begin
   NameChange(s, 1, i);
   ExtChange(s, i, length(s));
  end else NameChange(s, 1, length(s));
  {$IFDEF RecodeWhenDraw}
  S := OemToCharStr(S);
  end;
  {$ENDIF}
  lGetDir(0, SSS);
  lChDir(PF^.Owner^);
  lChangeFileName(S, S);
  I := IOResult;
  lChDir(SSS);
  if I <> 0 then begin
   Info^.Hide;
{$IFNDEF OS2}
   MessFileNotRename(GetLFN(PF^.LFN), S, I);
   MessageBox(GetString(dlFCNoRename1)+GetString(dlDIFile)+^M^C+Cut(GetLFN(PF^.LFN),20)
             +GetString(dlFCNoRename2)+Cut(S,20), nil, mfError+mfOKButton);
{$ELSE}
   MessFileNotRename(PF^.Name, S, I);
   MessageBox(GetString(dlFCNoRename1)+GetString(dlDIFile)+^M^C+Cut(PF^.Name,20)
             +GetString(dlFCNoRename2)+Cut(S,20), nil, mfError+mfOKButton);
{$ENDIF}
   DoChangeCase := true;
  end else begin
{$IFNDEF OS2}
   PF^.Name := Norm12(GetName(lfGetShortFileName(S)));
   DelLFN(PF^.LFN);
   PF^.LFN := AddLFN(S);
   if PF^.Attr and Directory <> 0
    then UpStr(PF^.Name)
    else begin
          LowStr(PF^.Name);
          if PF^.Attr and SysFile <> 0
           then PF^.Name[1] := UpCase(PF^.Name[1]);
         end;
   PF^.Name[9] := ' ';
   Sn := MakeFileName(PF^.Name);
{$ELSE}
   PF^.Name := GetName(S);
   Sn := PF^.Name;
{$ENDIF}
   S := GetPossibleDizOwner(1);
   if S <> '' then begin
    S := GetDizOwner(PF^.Owner^, S, Off);
    ReplaceDIZ(S, Nm, @Sn, nil);
   end;
   Message(P, evCommand, cmCopyUnselect, PF);
  end;
 end;

begin
 if (P^.Drive = nil) then Exit;
 FC := GetSelection(P, Off);
 if (FC = nil) then exit;

 L := ChangeNamesCaseOptions;

 if ExecResource(dlgNameCase, ChangeNamesCaseOptions) = cmCancel then begin
  FC^.DeleteAll;
  Dispose(FC, Done);
  Exit;
 end;

 if (L.Name <> ChangeNamesCaseOptions.Name)
    and (L.Ext <> ChangeNamesCaseOptions.Ext) then ConfigModified := On;

 if (ChangeNamesCaseOptions.Name = 0) and (ChangeNamesCaseOptions.Ext = 0) then
   begin
    FC^.DeleteAll;
    Dispose(FC, Done);
    Exit;
   end;

 case ChangeNamesCaseOptions.Name of
{LoNg FIle nAMe} 0: NameChange := CaseAsIs;
{long file name} 1: NameChange := CaseLow;
{Long file name} 2: NameChange := CaseCap;
{Long File Name} 3: NameChange := CaseCapAll;
{LONG FILE NAME} 4: NameChange := CaseUp;
  else Exit;
 end;

 case ChangeNamesCaseOptions.Ext of
{LoNg FIle nAMe} 0: ExtChange := CaseAsIs;
{long file name} 1: ExtChange := CaseLow;
{Long file name} 2: ExtChange := CaseCap;
{Long File Name} 3: ExtChange := CaseCapAll;
{LONG FILE NAME} 4: ExtChange := CaseUp;
  else Exit;
 end;

 R.Assign(0, 0, 20, 7);
 New(Info, Init(R));
 Info^.Write(1, Copy(GetString( dlPleaseStandBy ), 4, MaxStringLength));
 DeskTop^.Insert(Info);
 NewTimer(TT, 18);
 Abort := off;
 FC^.FirstThat(@DoChangeCase);
 Abort := off;
 MessageL(P, evCommand, cmPanelReread, 0);
 Info^.Free;
 FC^.DeleteAll;
 Dispose(FC,Done);
end;
{$IFNDEF REALMODE}
procedure CM_SelectColumn(AFP: Pointer); {JO}
 var
   I,J,K: Byte;
{$IFNDEF OS2}
   NameT: Array [1..10] of String[1];
{$ENDIF}
   ISFull: Array [1..10] of String[12];
   Descr: Array [1..10] of String[25];
   PanelDescr: String;
   P: PFilePanelRoot absolute AFP;
   Menu: PMenu;
   Items: PMenuItem;
   R: TRect;
   PV: PView;
   N, W: Word;

 begin

  case P^.Drive^.DriveType of
dtDisk,dtLink:
  begin
   for I:= 1 to 10 do
    begin
{$IFNDEF OS2}
      if (ColumnsDefaultsDisk.Params[I].Param and 1 <> 0)
         then NameT[I] := GetString(dlNameLong) else NameT[I] := GetString(dlNameShort);
{$ENDIF}
      if (ColumnsDefaultsDisk.Params[I].Param and 270 = 270)
         then ISFull[I] := GetString(dlFullInfo) else
         if (ColumnsDefaultsDisk.Params[I].Param and 16 <> 0) then
                  ISFull[I] := GetString(dlDescr) else ISFull[I] := '';
      Descr[I] := '';
    end;
    PanelDescr := PanelDescrDrive;
  end;
dtFind,dtList:
  begin
   for I:= 1 to 10 do
    begin
{$IFNDEF OS2}
      if (ColumnsDefaultsFind.Params[I].Param and 1 <> 0)
         then NameT[I] := GetString(dlNameLong) else NameT[I] := GetString(dlNameShort);
{$ENDIF}
      if (ColumnsDefaultsFind.Params[I].Param and 270 = 270)
         then ISFull[I] := GetString(dlFullInfo) else
         if (ColumnsDefaultsFind.Params[I].Param and 16 <> 0) then
                  ISFull[I] := GetString(dlPath) else ISFull[I] := '';
      Descr[I] := '';
    end;
    PanelDescr := PanelDescrFind;
  end;
dtTemp:
  begin
   for I:= 1 to 10 do
    begin
{$IFNDEF OS2}
      if (ColumnsDefaultsTemp.Params[I].Param and 1 <> 0)
         then NameT[I] := GetString(dlNameLong) else NameT[I] := GetString(dlNameShort);
{$ENDIF}
      if (ColumnsDefaultsTemp.Params[I].Param and 270 = 270)
         then ISFull[I] := GetString(dlFullInfo) else
         if (ColumnsDefaultsTemp.Params[I].Param and 16 <> 0) then
                  ISFull[I] := GetString(dlPath) else ISFull[I] := '';
      Descr[I] := '';
    end;
    PanelDescr := PanelDescrTemp;
  end;
dtArc:
  begin
   for I:= 1 to 10 do
    begin
{$IFNDEF OS2}
      if (ColumnsDefaultsArch.Params[I].Param and 1 <> 0)
         then NameT[I] := GetString(dlNameLong) else NameT[I] := GetString(dlNameShort);
{$ENDIF}
      if (ColumnsDefaultsArch.Params[I].Param and 62 = 62)
         then ISFull[I] := GetString(dlFullInfo) else ISFull[I] := '';
      Descr[I] := '';
    end;
    PanelDescr := PanelDescrArc;
  end;
dtArvid:
  begin
   for I:= 1 to 10 do
    begin
{$IFNDEF OS2}
      if (ColumnsDefaultsArvd.Params[I].Param and 1 <> 0)
         then NameT[I] := GetString(dlNameLong) else NameT[I] := GetString(dlNameShort);
{$ENDIF}
      if (ColumnsDefaultsArvd.Params[I].Param and 270 = 270)
         then ISFull[I] := GetString(dlFullInfo) else
         if (ColumnsDefaultsArvd.Params[I].Param and 16 <> 0) then
                  ISFull[I] := GetString(dlDescr) else ISFull[I] := '';
      Descr[I] := '';
    end;
    PanelDescr := PanelDescrArvid;
  end;
  end;

  if PanelDescr <> '' then
    begin
      I := 1;
      J := 1;
      for K := 1 to Length(PanelDescr) do
          if (PanelDescr[K] = '#') or (K = Length(PanelDescr)) then
             begin
               if I > 10 then continue;
               if (J = 1) and (PanelDescr[1] <> '#') then
                 begin
                  if (K = Length(PanelDescr)) and (PanelDescr[K] <> '#') then
                    Descr[I] := copy(PanelDescr, J, K-J+1)
                  else
                    Descr[I] := copy(PanelDescr, J, K-J);
                 end
               else
                 begin
                  if (K = Length(PanelDescr)) and (PanelDescr[K] <> '#') then
                    Descr[I] := copy(PanelDescr, J+1, K-J)
                  else
                    Descr[I] := copy(PanelDescr, J+1, K-J-1);
                 end;
               if I <= 10 then I := I+1;
               J := K;
             end;
    end;

  Items := nil;

  case P^.Drive^.DriveType of
dtDisk,dtLink:
  for I := 10 downto 1 do
    begin
      J := 48+I; if J = 58 then J := 48;
      Items := NewItem('~'+Char(J)+'~ '+ColumnsDefaultsDisk.Params[I].LFNLen+'.'+
        ColumnsDefaultsDisk.Params[I].EXTLen+' ' {$IFNDEF OS2} +NameT[I]+' ' {$ENDIF}
                                   +ISFull[I]+Descr[I],'',kbNoKey,16000+I,0,Items);
    end;
dtFind,dtList:
  for I := 10 downto 1 do
    begin
      J := 48+I; if J = 58 then J := 48;
      Items := NewItem('~'+Char(J)+'~ '+ColumnsDefaultsFind.Params[I].LFNLen+'.'+
        ColumnsDefaultsFind.Params[I].EXTLen+' ' {$IFNDEF OS2} +NameT[I]+' ' {$ENDIF}
                                   +ISFull[I]+Descr[I],'',kbNoKey,16000+I,0,Items);
    end;
dtTemp:
  for I := 10 downto 1 do
    begin
      J := 48+I; if J = 58 then J := 48;
      Items := NewItem('~'+Char(J)+'~ '+ColumnsDefaultsTemp.Params[I].LFNLen+'.'+
        ColumnsDefaultsTemp.Params[I].EXTLen+' ' {$IFNDEF OS2} +NameT[I]+' ' {$ENDIF}
                                   +ISFull[I]+Descr[I],'',kbNoKey,16000+I,0,Items);
    end;
dtArc:
  for I := 10 downto 1 do
    begin
      J := 48+I; if J = 58 then J := 48;
      Items := NewItem('~'+Char(J)+'~ '+ColumnsDefaultsArch.Params[I].LFNLen+'.'+
        ColumnsDefaultsArch.Params[I].EXTLen+' ' {$IFNDEF OS2} +NameT[I]+' ' {$ENDIF}
                                   +ISFull[I]+Descr[I],'',kbNoKey,16000+I,0,Items);
    end;
dtArvid:
  for I := 10 downto 1 do
    begin
      J := 48+I; if J = 58 then J := 48;
      Items := NewItem('~'+Char(J)+'~ '+ColumnsDefaultsArvd.Params[I].LFNLen+'.'+
        ColumnsDefaultsArvd.Params[I].EXTLen+' ' {$IFNDEF OS2} +NameT[I]+' '{$ENDIF}
                                   +ISFull[I]+Descr[I],'',kbNoKey,16000+I,0,Items);
    end;
  end;

  Menu := NewMenu(Items);

  N := 9;  W := 15;
  DeskTop^.GetExtent(R);
  R.A := P^.Origin; P^.Owner^.MakeGlobal(R.A, R.A); Desktop^.MakeLocal(R.A, R.A);
  if R.A.X < 0 then R.A.X := 0 else if R.A.X + W > R.B.X then R.A.X := R.B.X - W;
  if R.A.Y < 0 then R.A.Y := 0 else if R.A.Y + N + 2 > R.B.Y then R.A.Y := R.B.Y - N - 2;
  R.B.X := R.A.X + 100; R.B.Y := R.A.Y + 100; {R.B := R.A;}
                       PV := New(PMenuBox, Init(R, Menu, nil));
Menu^.Default := LookUpMenu(PMenuBox(PV)^.Menu, P^.Drive^.Param+16000, dfByCommand);
                       N := DeskTop^.ExecView(PV);
                       Dispose(PV, Done);
                       DisposeMenu(Menu);
                       if N >= 16001 then
                         begin
                           N := N - 16000;
                           P^.Drive^.GetParam(N);
                           P^.Owner^.ReDraw;
                         end;
 end;  {JO}
{$ENDIF}
{$IFDEF EAOP}
procedure CM_SetEALongname(AFP: Pointer; CurPos: Integer); {JO}
var  P: PFilePanelRoot absolute AFP;
     PF: PFileRec;
     S: String;
begin
 if (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArc) then Exit;
 PF := P^.Files^.At(CurPos);
 If PF^.Name = '..' then Exit;
 S := MakeNormName(PF^.Owner^, PF^.Name);
 SetEALongname(S);
end;

{$ENDIF}

END.
