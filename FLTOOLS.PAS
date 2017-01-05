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
uses
  FilesCol, FlPanelx
  ;

procedure CM_AdvancedFilter(AFP: Pointer);
function  GetSelection(P: PFilePanelRoot; Single: Boolean):
   PFilesCollection;
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
procedure CM_RenameSingleDialog(AFP, PEV: Pointer);
procedure CM_SortBy(AFP: Pointer);
function  CM_ChangeDirectory(AFP: Pointer): string;
procedure CM_MakeDir(AFP: Pointer);
procedure CM_LongCopy(AFP: Pointer);
procedure CM_ChangeCase(AFP: Pointer);
{$IFNDEF REALMODE}
procedure CM_SelectColumn(AFP: Pointer); {JO}
{$ENDIF}
{$IFDEF EAOP} {$IFDEF OS2}
procedure CM_SetEALongname(AFP: Pointer; CurPos: Integer); {JO}
{$ENDIF} {$ENDIF}

function FirstNameNum(P: PFilePanelRoot): longint;
  {AK155 Получение первого номера (от 1) в коллекции файлов панели,
  соответствующего не фиктивному имени '..', а настоящему имени.
  Возвращает либо 1, если это корень диска и элемента '..' нет,
  либо 2, если он есть.}

IMPLEMENTATION
uses
  FNotify,
  LFN, files, collect, xtime, dnini,
  advance, advance1, advance2, advance3, Dos, Objects, Dialogs,
  Views, DnApp, Commands, Drivers, FlPanel, Drives, FileCopy,
  Gauge, Gauges, Archiver, Startup, SWE, Validate, Messages, Menus, DnHelp,
  FileFind, Tree, FBB, DnUtil, filediz, filelst, FlTl,
  ErrMess
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
           l :=length(P1^.FlName[uLfn]);
           i := PosLastDot(P1^.FlName[uLfn]);
           FreeStr := Copy(P1^.FlName[uLfn], i+1, l-i);
             DelRight(FreeStr); UpStr(FreeStr); Insert(' *.', FreeStr, 1);
{AK155: только лишние тормоза, поскольку Duplicate=false
     if PC^.IndexOf(@FreeStr) < 0 then
/AK155}
           PC^.Insert(NewStr(FreeStr));
           end;
       end;

    var
      d: PDrive;
      fs: string;
      ti: TSize;
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
    I := Desktop^.ExecView(D);

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

function FirstNameNum(P: PFilePanelRoot): longint;
  begin
  FirstNameNum := 1;
  with P^.Files^ do
    if (Count > 0) and (PFileRec(At(0))^.TType = ttUpDir) then
      FirstNameNum := 2;
  end;
{/AK155}

{AK155: оформил эту процедуру по-человечески }
function  GetSelection(P: PFilePanelRoot; Single: Boolean):
   PFilesCollection;
 var FC: PFilesCollection absolute result;
     SourceFiles: PFilesCollection;
     I, N: LongInt;
     CurFile, FileI: PFileRec;
begin
  FC := nil; N := P^.Files^.Count;
  if N = 0 then Exit;
  SourceFiles := P^.Files;
  if (P^.SelNum = 0) or Single then
    begin
    CurFile := SourceFiles^.At(P^.ScrollBar^.Value);
    if CurFile^.TType = ttUpDir
    then
      begin {все файлы, кроме элемента 0, который есть UpDir}
      New(FC, Init(N-1,1)); FC^.Count := N-1;
      Move(SourceFiles^.Items^[1], FC^.Items^, (N-1)*SizeOf(Pointer));
      end
    else
      begin New(FC, Init(1,1)); FC^.Insert(CurFile); end;
    end
  else
    begin
    New(FC, Init(P^.SelNum, 1));
    for I := FirstNameNum(P)-1 to N-1 do
      begin
      FileI := SourceFiles^.At(I);
      if FileI^.Selected then
        FC^.AtInsert(FC^.Count, FileI);
      end;
    end;
//  FC^.SortMode := 200; {вроде, это не нужно}
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
   SN := PF^.FlName[uLfn];
   if ((PF^.Attr and Directory = 0){ or not Select}) and
       (InFilter(UpStrg(SN), S) xor XORs)
      then PF^.Selected := Select;
{JO}
   if (PF^.Attr and Directory <> 0) and
      (InDirFilter(UpStrg(SN), S) xor XORs)
      then PF^.Selected := Select;
{/JO}
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
        = LowStrg(GetName(GetDizOwner(PF^.Owner^, SD, false)))) then PF^.Selected := false;
     end;
*)
   if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) and (FMSetup.DIZ <> '')
    and (P^.Drive^.DriveType < dtArcFind) then
     begin
      K := 1;
      for J := 1 to Length(FMSetup.DIZ) do
      begin
        if (FMSetup.DIZ[J] = ';') then
          begin
           if (LowStrg(PF^.FlName[true]) = LowStrg(Copy(FMSetup.DIZ,K,J-K))) then PF^.Selected := false;
           K:=J+1;
          end;
        if J = Length(FMSetup.DIZ) then
           if (LowStrg(PF^.FlName[true]) = LowStrg(Copy(FMSetup.DIZ,K,J-K+1))) then PF^.Selected := false;
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
        = LowStrg(GetName(GetDizOwner(PF^.Owner^, SD, false)))) then PF^.Selected := false;
     end;
*)
   if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) and (FMSetup.DIZ <> '')
    and (P^.Drive^.DriveType < dtArcFind) then
     begin
      K := 1;
      for J := 1 to Length(FMSetup.DIZ) do
      begin
        if (FMSetup.DIZ[J] = ';') then
          begin
           if (LowStrg(PF^.FlName[true]) = LowStrg(Copy(FMSetup.DIZ,K,J-K))) then PF^.Selected := false;
           K:=J+1;
          end;
        if J = Length(FMSetup.DIZ) then
           if (LowStrg(PF^.FlName[true]) = LowStrg(Copy(FMSetup.DIZ,K,J-K+1))) then PF^.Selected := false;
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
     DialRes: Byte; {JO}
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
  if DT.S = 0 then PF^.Selected := false;
  if (PF^.Attr and Directory = 0) and
     (InFilter(PF^.FlName[uLfn] ,DT.FMask)) and {-$VOL}
     (InThat^.FirstThat(@Cmp) = nil) then PF^.Selected := DT.S = 0;
 end;

 label 1,2;

begin
 DT.FMask := ComareDirsOptions.FMask;
 DT.O := ComareDirsOptions.O;
 DT.S := ComareDirsOptions.S;
{$IFDEF Win32}
 D := PDialog(LoadResource(dlgCompareDirs));
   if D = nil then Exit;
  {JO: левое условие означает, что показ коротких имён включен в активной }
  {    панели, правое (закомментированное) - в пассивной                  }
 if (not PFilesCollection(IP)^.LFNActive) {or ((P^.Drive^.Flags and psShowLongName) = 0)} then
    begin  {JO: делаем недоступным чекбокс регистрочувствительности}
      P1 := D^.FirstThat(@IsCheckboxes);
      PCheckBoxes(P1)^.SetButtonState(16, False);
    end;
 D^.SetData(DT);
 DialRes := Desktop^.ExecView(D);
 if DialRes = cmCancel then begin Dispose(D,Done); Exit end;
 D^.GetData(DT);
{$ELSE}
 DialRes := ExecResource( dlgCompareDirs, DT);
 if DialRes = cmCancel then Exit;
{$ENDIF}
 if DialRes = cmYes then
   begin
     ConfigModified := True;
     ComareDirsOptions.FMask := DT.FMask;
     ComareDirsOptions.O := DT.O;
     ComareDirsOptions.S := DT.S;
   end;

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
  while SelectFiles(P, true,false) do;
 if (P^.SelNum = 0) then Exit;
 if (ActivePanel = P) and (P^.Drive^.DriveType = dtDisk)
   then CurrentDirectory := P^.DirectoryName;
 FC := GetSelection(P, false); if FC = nil then Exit;
 MakeListFile(P, FC);
 FC^.DeleteAll;
 Dispose(FC,Done);
 P^.SetDirName;
end;

        {-DataCompBoy-}
procedure CM_SetAttributes;
 var D: record
{$IFDEF USEANSISTRING}
          T, D, T_Cr, D_Cr, T_LAc, D_LAc:: String;
{$ELSE}
          T    : String[8];
          D    : String[10];
          T_Cr : String[8];
          D_Cr : String[10];
          T_LAc: String[8];
          D_LAc: String[10];
{$ENDIF}
          S, C: Word;
        end;
     I,UU,DD: LongInt;
     K: Word;
     S: String;
     R: TRect;
     PInfo: PWhileView;
     DTT, DTT1, DTT_Cr, DTT1_Cr, DTT_LAc, DTT1_LAc : LongInt;
     DT, DT1, DT_Cr, DT1_Cr, DT_LAc, DT1_LAc: DateTime;
     DateSet,TimeSet,CrDateSet,CrTimeSet,LAcDateSet,LAcTimeSet : boolean ;
     Res: Word;
     Dlg: PDialog;
     P: PFilePanelRoot absolute AFP;
     PF: PFileRec;
     ok: Boolean;
     CmdDlg: Word; {JO}
 label CurTime, CurTime1;    {JO}

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

begin
 if (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArcFind) then Exit;
 NotifySuspend; {Cat}
 FillChar(D, Sizeof(D), 0);
 if (P^.SelNum > 0) and not Single then
  begin
    Dlg := PDialog(LoadResource(dlgFilesAttr));
CurTime1:
    Dlg^.SetData(D);
    CmdDlg := Desktop^.ExecView(Dlg);
    case CmdDlg of
  cmYes:
       begin
         D.T     := GetDateTime(true);
         D.D     := GetDateTime(false);
         D.T_Cr  := GetDateTime(true);
         D.D_Cr  := GetDateTime(false);
         D.T_LAc := GetDateTime(true);
         D.D_LAc := GetDateTime(false);
         goto CurTime1;
       end;
 cmNo:
       begin
         D.T     := '';
         D.D     := '';
         D.T_Cr  := '';
         D.D_Cr  := '';
         D.T_LAc := '';
         D.D_LAc := '';
         goto CurTime1;
       end;
 cmCancel:
      begin
       Dispose(Dlg, Done);
       NotifyResume; {Cat}
       Exit;
      end;
   end;
   Dlg^.GetData(D);
   Dispose(Dlg, Done);
   NotifyResume; {Cat}
  end
 else
  begin
   Single := true;
   PF := P^.Files^.At(CurPos);
   If PF^.TType = ttUpDir then
    begin
     NotifyResume; {Cat}
     Exit;
    end;
   S := MakeNormName(PF^.Owner^, PF^.FlName[uLfn]);
   AddStr(S, #0); SetLength(S, Length(S)-1);
   Dlg := PDialog( LoadResource( dlgFileAttr ));
   D.C := GetFileAttr(S);
     if GetFileAges(S, DTT, DTT_Cr, DTT_LAc) <> 0 then
      begin
       NotifyResume; {Cat}
       Exit;
      end;
     UnpackTime(DTT, DT);
     UnpackTime(DTT_Cr, DT_Cr);
     UnpackTime(DTT_LAc, DT_LAc);
     D.T := SStr(DT.Hour, 2, '0')+':'+SStr(DT.Min, 2, '0')+':'+SStr(DT.Sec, 2, '0');
     D.D := ItoS(DT.Day)+'-'+SStr(DT.Month, 2, '0')+'-'+ItoS(DT.Year);
     D.T_Cr := SStr(DT_Cr.Hour, 2, '0')+':'+SStr(DT_Cr.Min, 2, '0')+':'+SStr(DT_Cr.Sec, 2, '0');
     D.D_Cr := ItoS(DT_Cr.Day)+'-'+SStr(DT_Cr.Month, 2, '0')+'-'+ItoS(DT_Cr.Year);
     D.T_LAc := SStr(DT_LAc.Hour, 2, '0')+':'+SStr(DT_LAc.Min, 2, '0')+':'+SStr(DT_LAc.Sec, 2, '0');
     D.D_LAc := ItoS(DT_LAc.Day)+'-'+SStr(DT_LAc.Month, 2, '0')+'-'+ItoS(DT_LAc.Year);
   D.S:=D.C;
CurTime:
   Dlg^.SetData(D);
   CmdDlg := Desktop^.ExecView( Dlg );
   case CmdDlg of
 cmYes:
       begin
         D.T     := GetDateTime(true);
         D.D     := GetDateTime(false);
         D.T_Cr  := GetDateTime(true);
         D.D_Cr  := GetDateTime(false);
         D.T_LAc := GetDateTime(true);
         D.D_LAc := GetDateTime(false);
         goto CurTime;
       end;
 cmNo:
       begin
         D.T     := '';
         D.D     := '';
         D.T_Cr  := '';
         D.D_Cr  := '';
         D.T_LAc := '';
         D.D_LAc := '';
         goto CurTime;
       end;
(*  работает, но падает после n-ного кол-ва раз
 cmSkip:
      begin
      {$IFNDEF OS2}
       Dlg^.EnableCommands([cmYes, cmNo]);
      {$ENDIF}
       Dispose( Dlg, Done );
       NotifyResume; {Cat}
       Message(Application, evKeyDown, kbDown, nil);
       Message(Application, evCommand, cmSingleAttr, nil);
       Exit;
      end;
*)
 cmCancel:
      begin
      {$IFNDEF OS2}
       Dlg^.EnableCommands([cmYes, cmNo]);
      {$ENDIF}
       Dispose( Dlg, Done );
       NotifyResume; {Cat}
       Exit;
      end;
   end;
   Dlg^.GetData(D);
  {$IFNDEF OS2}
   Dlg^.EnableCommands([cmYes, cmNo]);
  {$ENDIF}
   Dispose(Dlg, Done);
   D.C :={39}$FF;
  end;

 DTT1 := $FFFFFF; DTT1_Cr := $FFFFFF; DTT1_LAc := $FFFFFF;

 DateSet := (D.D <>''); CrDateSet := (D.D_Cr <>''); LAcDateSet := (D.D_LAc <>'');
 TimeSet := (D.T <>''); CrTimeSet := (D.T_Cr <>''); LAcTimeSet := (D.T_LAc <>'');

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

 with DT1_Cr do
  begin
   ok := CrTimeSet;
   Hour := CutNumber(D.T_Cr);
   Min  := CutNumber(D.T_Cr);
   if ok then
     begin
       Sec := CutNumber(D.T_Cr);
       ok := True;
     end;
   CrTimeSet := ok and not ((Hour > 24) or (Min>59) or (sec>59));


   ok := CrDateSet;
   Day  := CutNumber(D.D_Cr);
   Month:= CutNumber(D.D_Cr);
   Year := CutNumber(D.D_Cr);
   CrDateSet := ok and not ((Day>31 ) or ( Day<1 ) or (Month>12) or (Month<1));

   if CrDateSet then
     if Year < 80 then Inc(Year,2000)
        else if Year < 100 then Inc(Year,1900) ;

  end;

 with DT1_LAc do
  begin
   ok := LAcTimeSet;
   Hour := CutNumber(D.T_LAc);
   Min  := CutNumber(D.T_LAc);
   if ok then
     begin
       Sec := CutNumber(D.T_LAc);
       ok := True;
     end;
   LAcTimeSet := ok and not ((Hour > 24) or (Min>59) or (sec>59));


   ok := LAcDateSet;
   Day  := CutNumber(D.D_LAc);
   Month:= CutNumber(D.D_LAc);
   Year := CutNumber(D.D_LAc);
   LAcDateSet := ok and not ((Day>31 ) or ( Day<1 ) or (Month>12) or (Month<1));

   if LAcDateSet then
     if Year < 80 then Inc(Year,2000)
        else if Year < 100 then Inc(Year,1900) ;

  end;

 R.Assign(1,1,26,8);
 New(PInfo, Init(R));
 PInfo^.Top := GetString(dlSetAttr);
 PInfo^.Bottom := '';
 PInfo^.SetState(sfShadow, true);
 Desktop^.Insert(PInfo);

 if Single then begin UU := CurPos+1; DD := UU end
           else begin UU := 1; DD := P^.Files^.Count end;
 for I := UU to DD do
  begin
   PF := P^.Files^.At(I-1);
   if PF^.Selected or Single then
    begin
     S := Cut(PF^.FlName[uLfn], 40);
     PInfo^.Write(1, S);
     S := MakeNormName(PF^.Owner^, PF^.FlName[uLfn]);
     AddStr(S, #0); SetLength(S, Length(S)-1);
     K := (GetFileAttr(S) and (not D.C)) or D.S;
     if {$IFNDEF OS2} (K and Directory = 0) and {$ENDIF}
       TimeSet or DateSet or CrTimeSet or CrDateSet or LAcTimeSet or LAcDateSet
     then
      begin
       GetFileAges(S, DTT, DTT_Cr, DTT_LAc);
       UnPackTime(DTT, DT);
       UnpackTime(DTT_Cr, DT_Cr);
       UnpackTime(DTT_LAc, DT_LAc);
       if TimeSet then
         begin
           DT.Hour := DT1.Hour;
           DT.Sec := DT1.Sec;
           DT.Min := DT1.Min;
         end;
       if CrTimeSet then
         begin
           DT_Cr.Hour := DT1_Cr.Hour;
           DT_Cr.Sec := DT1_Cr.Sec;
           DT_Cr.Min := DT1_Cr.Min;
         end;
       if LAcTimeSet then
         begin
           DT_LAc.Hour := DT1_LAc.Hour;
           DT_LAc.Sec := DT1_LAc.Sec;
           DT_LAc.Min := DT1_LAc.Min;
         end;
       if DateSet then
         begin
           DT.Year := DT1.Year;
           DT.Day := DT1.Day;
           DT.Month := DT1.Month;
         end;
       if CrDateSet then
         begin
           DT_Cr.Year := DT1_Cr.Year;
           DT_Cr.Day := DT1_Cr.Day;
           DT_Cr.Month := DT1_Cr.Month;
         end;
       if LAcDateSet then
         begin
           DT_LAc.Year := DT1_LAc.Year;
           DT_LAc.Day := DT1_LAc.Day;
           DT_LAc.Month := DT1_LAc.Month;
         end;

       PackTime(DT, DTT1);
       PackTime(DT_Cr, DTT1_Cr);
       PackTime(DT_LAc, DTT1_LAc);
       DT.Sec := 0; DT_Cr.Sec := 0; DT_LAc.Sec := 0;
       UnPackTime(DTT1, DT);
       UnPackTime(DTT1_Cr, DT_Cr);
       UnPackTime(DTT1_LAc, DT_LAc);
       SetFileAges(S, DTT1, DTT1_Cr, DTT1_LAc);
      end;
     SetFileAttr(S, K and not Directory and not VolumeID);
     if not Single then PF^.Selected := false;
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
 NotifyResume; {Cat}
end;
        {-DataCompBoy-}

procedure CM_SetShowParms;
 var I: Record Prm: Word; LFNLen: String[3]; EXTLen: String[3] end;
     J,K,Old: Word;
     Idx: TDlgIdx;
     P: PFilePanelRoot absolute AFP;
begin
 with P^.Drive^ do
   begin
   Old := Flags;
   case DriveType of
    dtFind, dtTemp, dtList, dtArcFind: Idx := dlgFindParms;
    dtArc: Idx := dlgArcParms;
    else Idx := dlgDiskParms;
   end;
   I.Prm := Flags; I.LFNLen := ItoS(LFNLen); I.EXTLen := ItoS(EXTLen);
   J := ExecResource( Idx, I );
   case J of
    cmOK:
      begin
      J:=StoI(I.LFNLen);
      if J < 5 then exit;
      LFNLen:=J; K:=StoI(I.EXTLen);
      if (J-K>=5) then EXTLen:=K else EXTLen:=J-5;
      Flags := I.Prm;
      end;
    cmYes: GetParam(1);
    cmNo:  GetParam(2);
    else Exit;
   end;
   P^.DeltaX := 0;
   if ((DriveType = dtDisk) or (DriveType = dtArvid))
       and ((Flags xor Old) and (psShowDescript+psShowLongDesc) <> 0)
   then P^.RereadDir;
{$IFNDEF OS2}
   if not (P^.Drive^.DriveType in [dtArc,dtArcFind]) then
     P^.Files^.LFNActive := (Flags and psShowLongName) <> 0;
{$ENDIF}
   end;
 DrawViews(P);
 P^.SendLocated;
 DrawViews(P);
end;

procedure CM_CopyTemp;
 var FC: PFilesCollection;
     C: TCopyRec;
     P: PFilePanelRoot absolute AFP;
begin
 FC := GetSelection(P, false);
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
  if P^.Drive^.DriveType >= dtArcFind then Exit;
  PC := GetSelection(P, false);
  if PC = nil then Exit;
  if PC^.Count = 1 then
    S := PFileRec(PC^.At(0))^.FlName[uLfn]
  else if P^.Drive^.DriveType = dtDisk then
    S := GetName(P^.DirectoryName)
  else
    S := '';
{Cat}
  FSplit(S, Dir, Name, Ext); {AK155: не уверен, что это нужно }
  MakeArchive(Name, PC, false, false, P);
{/Cat}
  PC^.DeleteAll;
end;
        {-DataCompBoy-}

{$IFDEF Printer}
procedure CM_Print;
  var N: Integer;
      P: PFilePanelRoot absolute AFP;
begin
  if (P^.Drive^.DriveType  in [ dtDisk, dtFind, dtTemp, dtList ] ) then
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
if P^.Drive^.DriveType in [dtArc,dtArcFind] then Exit;
with P^.Drive^ do
  begin
{$IFNDEF OS2}
  Flags := Flags xor psShowLongName;
  P^.Files^.LFNActive := (Flags and psShowLongName) <> 0;
{$ELSE}
  ShowLogNames := not ShowLogNames;
{$ENDIF}
  end;
  P^.DeltaX := 0;
{  P^.RereadDir; } {AK155 22-07-2002
Само по себе перечитывание абсолютно не нужно. Может иметь смысл
только входящая в него сортировка. При сортировке не по имени.расширению,
а также если сортировки по длинным и коротким именам/расширениям
отделены, то перечитывание не дает ничего, кроме тормозов. А если
сортировки унифицированы, то есть отдельных сортировок по длинным и
коротким именам/расширениям нет, то при отсутствии перечитывания
все имена на панели остаются на тех же местах и переключение происходит
мгновенно. Ну а если хочется пересортировать - надо нажать Ctrl-R. }
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
  then FC := GetSelection(P, false)
  else
    if not (PFileRec(P^.Files^.At(CurPos))^.TType = ttUpDir)
    then begin
        New(FC, Init(1,1)); {AK155: заменил 10,10 на 1,1}
        FC^.Insert(P^.Files^.Items^[CurPos]);
       end else Exit;
 for I := 1 to FC^.Count do FC^.AtPut(I-1, CopyFileRec(PFileRec(FC^.Items^[I-1])));
 if FC^.Count = 1 then
   S := Cut(PFileRec(FC^.At(0))^.FlName[uLfn],20)
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
          SkipCopyDialog := false;
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
      S := MakeNormName(S, PFileRec(P^.Files^.At(I))^.FlName[uLfn]);
     CopyDirName := S;
    end;
   SkipCopyDialog := Confirms and cfMouseConfirm = 0;
   if SkipCopyDialog then
     begin
       S := CopyDirName; if (Length(S) > 3) and (S[Length(S)] = '\') then SetLength(S, Length(S)-1);
       UpStr(S);
       for J := 0 to PCopyRec(EI)^.FC^.Count-1 do
         with PFileRec(PCopyRec(EI)^.FC^.At(J))^ do
           if (Attr and Directory <> 0) and (UpStrg(MakeNormName(Owner^, FlName[uLfn])) = S) then Exit;
     end;
   P^.Drive^.CopyFilesInto(PCopyRec(EI)^.FC, PCopyRec(EI)^.Owner,
                           ShiftState and 7 <> 0);
   SkipCopyDialog := false;
end;

        {-DataCompBoy-}
procedure CM_RenameSingleL;
  var PIF: PInputFName;
      R: TRect;
      S, S2: String;
      DosE: Word;
      SSS: String;
      PF: PFileRec;
      Nm: String;
      Sn: String;
      P: PFilePanelRoot absolute AFP;
      Event: PEvent absolute PEV;
      ScrollBarValue: Integer;
      ReEnableCmdLine: boolean;
      DlgRes: word;

begin
  if (P^.Files = nil) then Exit;
  ScrollBarValue := P^.ScrollBar^.Value;
  if ScrollBarValue >= P^.Files^.Count then Exit;
  PF := P^.Files^.At(ScrollBarValue);
  if (PF^.TType = ttUpDir) or
{JO: для найденных в архиве файлов в панели поиска}
   PathFoundInArc(PF^.Owner^) or
{/JO}
     not (P^.Drive^.DriveType in [dtDisk, dtTemp, dtFind, dtList]) then Exit;
  S:= PF^.FlName[uLfn];
  S2 := S;
 {$IFNDEF OS2}
  if DescrByShortNames then Nm := PF^.FlName[false]
    else
 {$ENDIF}
      Nm := PF^.FlName[true];
 {$IFNDEF OS2}
  if (P^.Drive^.Flags and psShowLongName = 0)
    then R.Assign(0,0,13,1)
   else
 {$ENDIF}
    if (P^.Drive^.LFNLen < 250)
      then
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
  {$IFDEF RecodeWhenDraw}
  S := CharToOemStr(S);
  {$ENDIF}
  PIF^.SetData(S);
  PIF^.SetValidator(New(PFilterValidator, Init([#32..#255]-IllegalCharSet-['\','/','*','?','"'])));
  PIF^.SelectAll(false);

  {AK155 Чтобы комстрока не забирала курсор себе, отключаем
         ее на время работы }
  ReEnableCmdLine := (CommandLine <> nil) and
                    not CommandLine.GetState(sfDisabled);
  if ReEnableCmdLine then
    begin
    CommandLine^.SetState(sfDisabled, true);
    CommandLine^.Update;
    end;

  P^.Owner^.Insert(PIF);
  DlgRes := P^.Owner^.ExecView(PIF); PIF^.GetData(S);

  if ReEnableCmdLine then
    CommandLine^.SetState(sfDisabled, false);

  if S[Length(S)]='.' then SetLength(S, Length(S)-1);
  {$IFDEF RecodeWhenDraw}
  if S = CharToOemStr(S2) then DlgRes := cmCancel;
  S := OemToCharStr(S);
  {$ELSE}
  if S = S2 then DlgRes := cmCancel;
  {$ENDIF}
  if DlgRes <> cmCancel then
    begin
      lGetDir(0, SSS);
      lChDir(PF^.Owner^);
      ClrIO;
      lChangeFileName(S2, S);
      DOSE:=IOResult;
      if DOSE <> 0 then
          MessFileNotRename(S2, S, DOSE)

        else begin
{AK155 Так делать нельзя и не нужно  CopyShortString(S, PF^.FlName[uLfn]);}
{AK155 А таки нужно. Глючит, например, при переименовании в ветви каталога.}
             CopyShortString(S, PF^.FlName[uLfn]);
              {$IFNDEF OS2}
               if DescrByShortNames then
                 Sn := LowStrg(GetName(lfGetShortFileName(S)))
               else {$ENDIF} Sn := S;

               S2 := GetPossibleDizOwner(1);
               if S2 <> '' then begin
                S2 := GetDizOwner(PF^.Owner^, S2, false);
                if (PF^.DIZ <> nil) then
                  begin
                    ReplaceDIZ(S2, Nm, @Sn, PF^.DIZ^.DIZ);
                    DeleteDIZ(S2, Nm);
                  end;
               end;
             end;
      lChDir(SSS);
      P^.Owner^.Delete(PIF);
      Dispose(PIF,Done);
      if not DNIni.AutoRefreshPanels then
        GlobalMessage(evCommand, cmRereadDir, PF^.Owner);
      ClrIO;
      if DlgRes <> cmOK then
         begin
           Event^.What := evKeyDown; Event^.KeyCode := DlgRes;
           Event^.InfoPtr := nil; P^.PutEvent(Event^);
         end;
    end else PIF^.Free;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure CM_RenameSingleDialog;
  var R: TRect;
      S, S2: String;
      DosE: Word;
      SSS: String;
      PF: PFileRec;
      Nm: String;
      Sn: String;
      DlgRes: word;
      P: PFilePanelRoot absolute AFP;
      Event: PEvent absolute PEV;
      ScrollBarValue: Integer;
      U: lFile;

begin
  if (P^.Files = nil) then Exit;
  ScrollBarValue := P^.ScrollBar^.Value;
  if ScrollBarValue >= P^.Files^.Count then Exit;
  PF := P^.Files^.At(ScrollBarValue);
  if (PF^.TType = ttUpDir) or
{JO: для найденных в архиве файлов в панели поиска}
   PathFoundInArc(PF^.Owner^) or
{/JO}
     not (P^.Drive^.DriveType in [dtDisk, dtTemp, dtFind, dtList]) then Exit;
  S:= PF^.FlName[uLfn];
{$IFNDEF OS2}
  if DescrByShortNames then Nm := PF^.FlName[false]
  else {$ENDIF}
    Nm := PF^.FlName[true];
  S2 := S;
  {$IFDEF RecodeWhenDraw}
  S := CharToOemStr(S);
  {$ENDIF}
  DlgRes := ExecResource(dlgRenFl, S);
  {$IFDEF RecodeWhenDraw}
  S := OemToCharStr(S);
  {$ENDIF}
  If DlgRes = cmCancel then Exit;
  If DlgRes = cmYes then UpStr(S);
  If DlgRes = cmNo then LowStr(S);
  if S[Length(S)]='.' then SetLength(S, Length(S)-1);

  if S = S2 then DlgRes := cmCancel;
  if DlgRes <> cmCancel then
    begin
      lGetDir(0, SSS);
      lChDir(PF^.Owner^);
      ClrIO;
      lChangeFileName(S2, S);
      DOSE:=IOResult;
      if DOSE <> 0 then
          MessFileNotRename(S2, S, DOSE)

        else begin
               CopyShortString(S, PF^.FlName[uLfn]);
              {$IFNDEF OS2}
               if DescrByShortNames then
                 Sn := LowStrg(GetName(lfGetShortFileName(S)))
               else {$ENDIF} Sn := S;
               S2 := GetPossibleDizOwner(1);
               if S2 <> '' then begin
                S2 := GetDizOwner(PF^.Owner^, S2, false);
                if (PF^.DIZ <> nil) then
                  begin
                    ReplaceDIZ(S2, Nm, @Sn, PF^.DIZ^.DIZ);
                    DeleteDIZ(S2, Nm);
                  end;
               end;
             end;
      lChDir(SSS);
      if not DNIni.AutoRefreshPanels then
        GlobalMessage(evCommand, cmRereadDir, PF^.Owner);
      ClrIO;
      if DlgRes <> cmOK then
         begin
           Event^.What := evKeyDown; Event^.KeyCode := DlgRes;
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
    NewItem(GetString(dlSortExt),     '',kbNoKey,cmSortExt     ,0,
    NewItem(GetString(dlSortSize),    '',kbNoKey,cmSortSize    ,0,
    NewItem(GetString(dlSortDate),    '',kbNoKey,cmSortDate    ,0,
{$IFNDEF DPMI32}
    NewItem(GetString(dlSortCrDate),  '',kbNoKey,cmSortCrDate  ,0,
    NewItem(GetString(dlSortLADate),  '',kbNoKey,cmSortLADate  ,0,
{$ENDIF}
    NewItem(GetString(dlSortDIZ),     '',kbNoKey,cmSortDIZ     ,0,
    NewItem(GetString(dlSortType),    '',kbNoKey,cmSortType    ,0,
    NewItem(GetString(dlSortUnsorted),'',kbNoKey,cmSortUnsorted,0,
  nil)))))))){$IFNDEF DPMI32})){$ENDIF};
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
        if UpStrg(PF^.FlName[true]) = GetName(DN) then
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
 if (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArcFind) then Exit;
 PF := P^.Files^.At(P^.ScrollBar^.Value);
 if (PF^.Attr and Directory <> 0)
{JO: для найденных в архиве файлов в панели поиска}
   or PathFoundInArc(PF^.Owner^)
{/JO}
       then Exit;
 S := MakeNormName(PF^.Owner^, PF^.FlName[uLfn]);
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
     NM, Sn: String;
     i: integer;
     U: lFile;
 begin
  if TimerExpired(TT) then begin
   DispatchEvents(Info, Abort);
   NewTimerSecs(TT, 2);
  end;
  DoChangeCase := Abort;
  if Abort then exit;
{JO: для найденных в архиве файлов в панели поиска}
  if PathFoundInArc(PF^.Owner^) then Exit;
{/JO}
  S := PF^.FlName[true];
  NM := S;
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
  if S = PF^.FlName[true] then
    I := 0
  else
    begin
      lGetDir(0, SSS);
      lChDir(PF^.Owner^);
      lChangeFileName(S, S);
      I := IOResult;
      lChDir(SSS);
    end;
  if I <> 0 then begin
   Info^.Hide;
   MessFileNotRename(PF^.FlName[true], S, I);
   MessageBox(GetString(dlFCNoRename1)+GetString(dlDIFile)+^M^C+Cut(PF^.FlName[true],20)
             +GetString(dlFCNoRename2)+Cut(S,20), nil, mfError+mfOKButton);
   DoChangeCase := true;
  end else begin
   CopyShortString(GetName(S), PF^.FlName[true]);
 {JO: при изменении регистра файла имеет хоть какой-то смысл менять регистр }
 {    его имени в файле описаний только если описания даются по длинным     }
 {    именам, так как для коротких имён регистр не меняется                 }
  {$IFNDEF OS2}
   if not DescrByShortNames then
     begin
  {$ENDIF}
       Sn := PF^.FlName[true];
       S := GetPossibleDizOwner(1);
       if S <> '' then
         begin
           S := GetDizOwner(PF^.Owner^, S, false);
           ReplaceDIZ(S, Nm, @Sn, nil);
         end;
  {$IFNDEF OS2}
     end;
  {$ENDIF}
   Message(P, evCommand, cmCopyUnselect, PF);
  end;
 end;

begin
 if (P^.Drive = nil) then Exit;
 FC := GetSelection(P, false);
 if (FC = nil) then exit;

 L := ChangeNamesCaseOptions;

 if ExecResource(dlgNameCase, ChangeNamesCaseOptions) = cmCancel then begin
  FC^.DeleteAll;
  Dispose(FC, Done);
  Exit;
 end;

 if (L.Name <> ChangeNamesCaseOptions.Name)
    and (L.Ext <> ChangeNamesCaseOptions.Ext) then ConfigModified := true;

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
 Abort := false;
 FC^.FirstThat(@DoChangeCase);
 Abort := false;
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
dtFind,dtList,dtArcFind:
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
dtFind,dtList,dtArcFind:
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
{$IFNDEF OS2}
   if not (P^.Drive^.DriveType in [dtArc,dtArcFind]) then
     P^.Files^.LFNActive := (P^.Drive^.Flags and psShowLongName) <> 0;
{$ENDIF}
 end;  {JO}
{$ENDIF}
{$IFDEF EAOP} {$IFDEF OS2}
procedure CM_SetEALongname(AFP: Pointer; CurPos: Integer); {JO}
var  P: PFilePanelRoot absolute AFP;
     PF: PFileRec;
     S: String;
begin
 if (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArcFind) then Exit;
 PF := P^.Files^.At(CurPos);
 If (PF^.TType = ttUpDir)
{JO: для найденных в архиве файлов в панели поиска}
   or PathFoundInArc(PF^.Owner^)
{/JO}
     then Exit;
 S := MakeNormName(PF^.Owner^, PF^.FlName[true]);
 SetEALongname(S);
end;

{$ENDIF} {$ENDIF}

END.
