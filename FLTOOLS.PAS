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
unit FLTools;

interface
uses
  FilesCol, FlPanelX
  ;

procedure CM_AdvancedFilter(AFP: Pointer);
function GetSelection(P: PFilePanelRoot; Single: boolean):
PFilesCollection;
function SelectFiles(AFP: Pointer; Select, XORs: boolean): boolean;
procedure CM_CopyFiles(AFP: Pointer; MoveMode, Single: boolean);
procedure InvertSelection(AFP: Pointer; dr: boolean);
procedure CM_CompareDirs(AFP, IP: Pointer);
procedure CM_EraseFiles(AFP: Pointer; Single: boolean);
procedure CM_MakeList(AFP: Pointer);
procedure CM_SetAttributes(AFP: Pointer; Single: boolean; CurPos:
    integer);
procedure CM_SetShowParms(AFP: Pointer);
procedure CM_CopyTemp(AFP: Pointer);
procedure CM_ArchiveFiles(AFP: Pointer);
{$IFDEF Printer}
procedure CM_Print(AFP: Pointer);
{$ENDIF}
procedure CM_ToggleDescriptions(AFP: Pointer);
procedure CM_ToggleLongNames(AFP: Pointer);
procedure CM_ToggleShowMode(AFP: Pointer);
procedure CM_DragDropper(AFP: Pointer; CurPos: integer; EV: Pointer);
procedure CM_Dropped(AFP, EI: Pointer);
procedure DragMover(AP: Pointer; text: String; AFC, AC: Pointer);
procedure CM_RenameSingleL(AFP, PEV: Pointer);
procedure CM_RenameSingleDialog(AFP, PEV: Pointer);
procedure CM_SortBy(AFP: Pointer);
function CM_ChangeDirectory(AFP: Pointer): String;
procedure CM_MakeDir(AFP: Pointer);
procedure CM_LongCopy(AFP: Pointer);
procedure CM_ChangeCase(AFP: Pointer);
procedure CM_SelectColumn(AFP: Pointer); {JO}
{$IFDEF OS2}
procedure CM_SetEALongname(AFP: Pointer; CurPos: integer); {JO}
{$ENDIF}

function FirstNameNum(P: PFilePanelRoot): longInt;
{AK155 Получение первого номера (от 1) в коллекции файлов панели,
  соответствующего не фиктивному имени '..', а настоящему имени.
  Возвращает либо 1, если это корень диска и элемента '..' нет,
  либо 2, если он есть.}

implementation
uses
  fnotify,
  Lfn, Files, Collect, xTime, DnIni, HistList,
  advance, advance1, advance2, advance3, Dos, Objects, Dialogs,
  Views, DNApp, Commands, Drivers, FlPanel, Drives, FileCopy,
  Gauge, Gauges, Archiver, Startup, SWE, Validate, Messages, Menus,
    DNHelp,
  FileFind, Tree, FBB, DNUtil, Filediz, Filelst, FlTl,
  ErrMess
  ;

type
  PSelectList = ^TSelectList;
  TSelectList = object(TListBox)
    function IsSelected(i: longInt): boolean; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

procedure DrawViews(P: PFilePanelRoot);
  begin
    P^.DrawView;
    if P^.InfoView <> nil then
      P^.InfoView^.DrawView;
  end;

function TSelectList.IsSelected(i: longInt): boolean;
  begin
    IsSelected := (List <> nil) and (Copy(CnvString(List^.At(i)), 1, 1)
      = #251); {-$VIV}
  end;

procedure TSelectList.HandleEvent(var Event: TEvent);
  var
    P: PString;

  procedure CE;
    begin
      ClearEvent(Event);
      DrawView;
    end;

  procedure Invert;
    begin
      if (List <> nil) and (Focused < List^.Count) then
        P := List^.At(Focused)
      else
        P := nil;
      if P <> nil then
        if P^[1] = ' ' then
          P^[1] := #251 {-$VIV #254}
        else
          P^[1] := ' ';
      CE
    end;

  procedure DoSelect(P: PString);
    begin
      if P <> nil then
        case Event.CharCode of
          '*':
            if P^[1] = ' ' then
              P^[1] := #251 {-$VIV #254}
            else
              P^[1] := ' ';
          '+':
            P^[1] := #251; {-$VIV #254}
          '-':
            P^[1] := ' ';
        end {case};
    end;

  begin { TSelectList.HandleEvent }
    case Event.What of
      evMouseDown:
        if Event.Buttons and mbRightButton <> 0 then
          begin
            inherited HandleEvent(Event);
            Invert;
          end;
      evKeyDown:
        case Event.CharCode of
          ' ':
            begin
              Event.KeyCode := kbDown;
              PutEvent(Event);
              Invert;
            end;
          '+', '*', '-':
            if List <> nil then
              begin
                List^.ForEach(@DoSelect);
                CE
              end;
          else
            if Event.KeyCode = kbIns then
              begin
                Event.KeyCode := kbDown;
                PutEvent(Event);
                Invert;
              end;
        end {case};
    end {case};
    if Event.What <> evNothing then
      inherited HandleEvent(Event);
  end { TSelectList.HandleEvent };

procedure CM_AdvancedFilter;
  var
    P: PFilePanelRoot absolute AFP;
    i: byte;
    C: PStringCollection;
    ClrPlus, ClrMinus: boolean;
    s, FileMask: String;
    FItem: integer;

  function GetMaskSelection: String;
    var
      PC: PStringCollection;
      i: longInt;
      D: PDialog;
      PL: PListBox;
      IL: PInputline;

    function MakeDialog: PDialog;
      var
        Dlg: PDialog;
        R: TRect;
        Control, Labl, Histry: PView;
      begin
        Dlg := PDialog(LoadResource(dlgAdvancedFilter));
        with Dlg^ do
          begin
            R.Assign(Size.X-4, 2, Size.X-3, Size.Y-4);
            Control := New(PScrollBar, Init(R));
            Insert(Control);

            R.Assign(3, 2, Size.X-4, Size.Y-4);
            Control := New(PSelectList, Init(R, 3, PScrollBar(
              Control)));
            PL := PListBox(Control);
            PListBox(PL)^.NewLisT(PC);
            Insert(Control);
          end;
        MakeDialog := Dlg;
      end { MakeDialog: };

    procedure DoMake(P: PString);
      begin
        if (P <> nil) and (P^[1] <> ' ') then
          begin
            AddStr(FreeStr, ';');
            if i = cmNo then
              AddStr(FreeStr, '-');
            FreeStr := FreeStr+Copy(P^, 2, Length(P^)-1);
          end;
      end;

    procedure GetMasks;

      {AK155: эта процедура должна формировать маску по расширению
для текущего файла и добавлять ее в коллекцию. Но при этом не
учитывалась возможность длинного имени (в Name). }

      procedure DoInsert(P1: PFileRec); {DataCompBoy}
        var
          l, i: integer;
        begin
          if (P1 <> nil) and (P1^.Attr and Directory = 0) then
            begin
              l := Length(P1^.FlName[uLfn]);
              i := PosLastDot(P1^.FlName[uLfn]);
              FreeStr := Copy(P1^.FlName[uLfn], i+1, l-i);
              DelRight(FreeStr);
              UpStr(FreeStr);
              Insert(' *.', FreeStr, 1);
              {AK155: только лишние тормоза, поскольку Duplicate=false
     if PC^.IndexOf(@FreeStr) < 0 then
/AK155}
              PC^.Insert(NewStr(FreeStr));
            end;
        end;

      var
        D: PDrive;
        fs: String;
        ti: TSize;
        FC: PFilesCollection;
      begin { GetMasks }
        D := P^.Drive;
        FC := PFilesCollection(
        D^.GetDirectory(P^.SortMode, P^.PanelFlags, x_x, fs, ti));
        if (FC <> nil) and (not D^.NoMemory) then
          FC^.ForEach(@DoInsert);
        Dispose(FC, Done);
      end { GetMasks };

    begin { GetMaskSelection: }
      GetMaskSelection := '';
      PC := New(PStringCollection, Init(20, 20, False));
      GetMasks;
      if (PC^.Count > 0) and (not P^.Drive^.NoMemory) then
        begin
          PC^.AtInsert(0, NewStr(' *.*'));
          D := MakeDialog;
          D^.Options := D^.Options or ofCentered;
          PL^.FocusItem(FItem);
          i := Desktop^.ExecView(D);

          if i <> cmCancel then
            begin
              FItem := PL^.Focused;
              FreeStr := '';
              PC^.ForEach(@DoMake);
              if FreeStr = '' then
                begin
                  FreeStr := CnvString(PC^.At(FItem));
                  if i = cmNo then
                    Insert('-', FreeStr, 1);
                end
              else
                Delete(FreeStr, 1, 1); {DelFC(FreeStr);}
              if (i = cmNo) and (FreeStr <> '-*.*') and
                (FreeStr <> '-*')
              then
                Insert('*.*;', FreeStr, 1);
              DelLeft(FreeStr);
              GetMaskSelection := FreeStr;
            end;
          Dispose(D, Done);
        end;
      Dispose(PC, Done);
    end { GetMaskSelection: };


  procedure InS(const AAS: String);
    var
      Mns: boolean;
    begin
      if (AAS = '') then
        exit;
      Mns := (Copy(AAS, 1, 2) = '- ');
      if (ClrMinus and Mns) or
        (ClrPlus and not Mns) or
        (C^.IndexOf(@AAS) <> -1)
      then
        exit;
      C^.Insert(NewStr(AAS));
    end;

  procedure MakeMask(P: PString);
    begin
      if FileMask <> '' then
        FileMask := FileMask+';';
      FileMask := FileMask+P^;
    end;

  begin
      {------------------- CM_AdvancedFilter -----------------------}
    FileMask := P^.FileMask;
    FItem := 0;
    repeat
      s := GetMaskSelection;
      if (P^.Drive^.NoMemory) or (s = '') then
        break;
      if (s = '- *.*') or (s = x_x) then
        FileMask := s
      else
        begin
          if FileMask = x_x then
            begin
              FileMask := '';
              FreeStr := s+';'
            end
          else
            FreeStr := ';'+FileMask+';'+s+';';
          FileMask := s;
          s := FreeStr;
          ClrMinus := Pos(';- *.*;', s) > 0;
          if ClrMinus then
            Replace('- *.*', '', s);
          ClrPlus := Pos(';*.*;', s) > 0;
          if ClrPlus then
            Replace(x_x, '', s);
          if not (ClrPlus and ClrMinus) then
            begin
              if Copy(FileMask, 1, 2) = '- ' then
                Replace(';'+Copy(FileMask, 3, 5)+';', ';', s)
              else
                Replace(';- '+FileMask+';', ';', s);
              C := New(PStringCollection, Init(4, 4, False));
              repeat
                i := PosChar(';', s);
                if i = 0 then
                  begin
                    InS(s);
                    break;
                  end;
                InS(Copy(s, 1, i-1));
                Delete(s, 1, i);
              until False;
              FileMask := '';
              C^.ForEach(@MakeMask);
              Dispose(C, Done);
              if FileMask <> '' then
                FileMask := ';'+FileMask;
              if ClrPlus then
                FileMask := x_x+FileMask;
              if ClrMinus then
                FileMask := '- *.*'+FileMask;
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
  end { CM_AdvancedFilter };
  {------------------- CM_AdvancedFilter -----------------------}

function FirstNameNum(P: PFilePanelRoot): longInt;
  begin
    FirstNameNum := 1;
    with P^.Files^ do
      if (Count > 0) and (PFileRec(At(0))^.TType = ttUpDir) then
        FirstNameNum := 2;
  end;
{/AK155}

{AK155: оформил эту процедуру по-человечески }
function GetSelection(P: PFilePanelRoot; Single: boolean):
  PFilesCollection;
  var
    FC: PFilesCollection absolute Result;
    SourceFiles: PFilesCollection;
    i, n: longInt;
    CurFile, FileI: PFileRec;
  begin
    FC := nil;
    n := P^.Files^.Count;
    if n = 0 then
      exit;
    SourceFiles := P^.Files;
    if (P^.SelNum = 0) or Single then
      begin
        CurFile := SourceFiles^.At(P^.ScrollBar^.Value);
        if CurFile^.TType = ttUpDir
        then
          begin{все файлы, кроме элемента 0, который есть UpDir}
            New(FC, Init(n-1, 1));
            FC^.Count := n-1;
            Move(SourceFiles^.Items^[1], FC^.Items^, (n-1)*SizeOf(
              Pointer));
          end
        else
          begin
            New(FC, Init(1, 1));
            FC^.Insert(CurFile);
          end;
      end
    else
      begin
        New(FC, Init(P^.SelNum, 1));
        for i := FirstNameNum(P)-1 to n-1 do
          begin
            FileI := SourceFiles^.At(i);
            if FileI^.Selected then
              FC^.AtInsert(FC^.Count, FileI);
          end;
      end;
    //  FC^.SortMode := 200; {вроде, это не нужно}
  end { GetSelection };

{-DataCompBoy-}
function SelectFiles;
  var
    s, SN: String;
    i: longInt;
    P: PFilePanelRoot absolute AFP;
    PF: PFileRec; {DataCompBoy}
    j, k: byte; {JO}
    {SD: String;} {JO}

  begin
    SelectFiles := False;
    if P^.Files^.Count = 0 then
      exit;
    if not SelectDialog(Select, s, XORs) then
      exit;
    UpStr(s);
    SelectFiles := True;
    P^.SelNum := 0;
    P^.SelectedLen := 0;
    P^.PackedLen := 0;
    for i := FirstNameNum(P) to P^.Files^.Count do
      begin
        PF := P^.Files^.At(i-1);
        SN := PF^.FlName[uLfn];
        if ((PF^.Attr and Directory = 0) { or not Select}) and
          (InFilter(UpStrg(SN), s) xor XORs)
        then
          PF^.Selected := Select;
        {JO}
        if (PF^.Attr and Directory <> 0) and
          (InDirFilter(UpStrg(SN), s) xor XORs)
        then
          PF^.Selected := Select;
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
        if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) and (FMSetup.
            DIZ <> '')
          and (P^.Drive^.DriveType < dtArcFind)
        then
          begin
            k := 1;
            for j := 1 to Length(FMSetup.DIZ) do
              begin
                if (FMSetup.DIZ[j] = ';') then
                  begin
                    if (LowStrg(PF^.FlName[True]) = LowStrg(Copy(
                        FMSetup.DIZ, k, j-k)))
                    then
                      PF^.Selected := False;
                    k := j+1;
                  end;
                if j = Length(FMSetup.DIZ) then
                  if (LowStrg(PF^.FlName[True]) = LowStrg(Copy(
                      FMSetup.DIZ, k, j-k+1)))
                  then
                    PF^.Selected := False;
              end;
          end;
        {JO}
        if PF^.Selected then
          begin
            P^.PackedLen := P^.PackedLen+PF^.PSize;
            P^.SelectedLen := P^.SelectedLen+PF^.Size;
            Inc(P^.SelNum);
          end;
      end;
    DrawViews(P);
  end { SelectFiles };
{-DataCompBoy-}

procedure CM_CopyFiles;
  var
    FC: PFilesCollection;
    P: PFilePanelRoot absolute AFP;
  begin
    CurrentDirectory := P^.Drive^.GetRealName;
    if P^.Files^.Count = 0 then
      exit;
    FC := GetSelection(P, Single);
    if FC = nil then
      exit;
    P^.Drive^.CopyFiles(FC, P, MoveMode);
    FC^.DeleteAll;
    Dispose(FC, Done);
  end;

{-DataCompBoy-}
procedure InvertSelection;
  var
    i: longInt;
    PF: PFileRec;
    P: PFilePanelRoot absolute AFP;
    j, k: byte;
    {SD: String;} {JO}
  begin
    P^.SelNum := 0;
    P^.SelectedLen := 0;
    P^.PackedLen := 0;
    for i := FirstNameNum(P) to P^.Files^.Count do
      begin
        PF := PFileRec(P^.Files^.At(i-1));
        if dr or (PF^.Attr and Directory = 0) then
          PF^.Selected := not PF^.Selected;

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
        if (FMSetup.Options and fmoAlwaysCopyDesc <> 0) and (FMSetup.
            DIZ <> '')
          and (P^.Drive^.DriveType < dtArcFind)
        then
          begin
            k := 1;
            for j := 1 to Length(FMSetup.DIZ) do
              begin
                if (FMSetup.DIZ[j] = ';') then
                  begin
                    if (LowStrg(PF^.FlName[True]) = LowStrg(Copy(
                        FMSetup.DIZ, k, j-k)))
                    then
                      PF^.Selected := False;
                    k := j+1;
                  end;
                if j = Length(FMSetup.DIZ) then
                  if (LowStrg(PF^.FlName[True]) = LowStrg(Copy(
                      FMSetup.DIZ, k, j-k+1)))
                  then
                    PF^.Selected := False;
              end;
          end;
        {JO}

        if PF^.Selected then
          begin
            P^.SelectedLen := P^.SelectedLen+PF^.Size;
            P^.PackedLen := P^.PackedLen+PF^.PSize;
            Inc(P^.SelNum);
          end;
      end;
    DrawViews(P);
  end { InvertSelection };
{-DataCompBoy-}

{-DataCompBoy-}
procedure CM_CompareDirs(AFP, IP: Pointer);
  var
    DD, InThat: PFilesCollection;
    OSM1, OSM2: word;
    i, j: longInt;
    PF: PFileRec;
    DT: record
      o: word;
      FMask: String;
      s: word;
      end; {-$VOL added, DataCompBoy changed}
    Info: PView;
    P: PFilePanelRoot absolute AFP;
    {$IFDEF Win32}
    D: PDialog; {JO}
    P1: PView; {JO}
    {$ENDIF}
    DialRes: byte; {JO}
  const
    CompareName = 1;
    CompareDat = 2;
    CompareLength = 4;
    CompareContens = 8;
    {$IFDEF Win32}
  function IsCheckboxes(P: PView): boolean;
    begin
      IsCheckboxes := TypeOf(P^) = TypeOf(TCheckBoxes);
    end;
  {$ENDIF}
  procedure Cmp1(PF: PFileRec);
    function Cmp(AP: PFileRec): boolean;
      begin
        UpdateWriteView(Info);
        Cmp := (AP^.Attr and Directory = 0) and (P^.Files^.Compare(PF,
          AP) = 0);
      end;
    begin
      UpdateWriteView(Info);
      if DT.s = 0 then
        PF^.Selected := False;
      if (PF^.Attr and Directory = 0) and
        (InFilter(PF^.FlName[uLfn], DT.FMask)) and {-$VOL}
        (InThat^.FirstThat(@Cmp) = nil)
      then
        PF^.Selected := DT.s = 0;
    end;

  label 1, 2;

  begin { CM_CompareDirs }
    DT.FMask := ComareDirsOptions.FMask;
    DT.o := ComareDirsOptions.o;
    DT.s := ComareDirsOptions.s;
    {$IFDEF Win32}
    D := PDialog(LoadResource(dlgCompareDirs));
    if D = nil then
      exit;
    {JO: левое условие означает, что показ коротких имён включен в активной }
    {    панели, правое (закомментированное) - в пассивной                  }
    if (not PFilesCollection(IP)^.LFNActive)
        {or ((P^.Drive^.Flags and psShowLongName) = 0)}
    then
      begin{JO: делаем недоступным чекбокс регистрочувствительности}
        P1 := D^.FirstThat(@IsCheckboxes);
        PCheckBoxes(P1)^.SetButtonState(16, False);
      end;
    D^.SetData(DT);
    DialRes := Desktop^.ExecView(D);
    if DialRes = cmCancel then
      begin
        Dispose(D, Done);
        exit
      end;
    D^.GetData(DT);
    {$ELSE}
    DialRes := ExecResource(dlgCompareDirs, DT);
    if DialRes = cmCancel then
      exit;
    {$ENDIF}
    if DialRes = cmYes then
      begin
        ConfigModified := True;
        ComareDirsOptions.FMask := DT.FMask;
        ComareDirsOptions.o := DT.o;
        ComareDirsOptions.s := DT.s;
      end;

    if DelSpaces(DT.FMask) = '' then
      DT.FMask := x_x;

    DD := IP;
    OSM1 := PFilesCollection(P^.Files)^.SortMode;
    OSM2 := DD^.SortMode;
    PFilesCollection(P^.Files)^.SortMode := NumSortModes+DT.o;
    DD^.SortMode := NumSortModes+DT.o;

    Info := nil;
    if DT.o and 8 <> 0 then
      begin
        Info := WriteMsg(GetString(dlComparing));
      end;

    InThat := DD;
    P^.Files^.ForEach(@Cmp1);
    InThat := P^.Files;
    DD^.ForEach(@Cmp1);

    PFilesCollection(P^.Files)^.SortMode := OSM1;
    DD^.SortMode := OSM2;
    Info^.Free;
  end { CM_CompareDirs };
{-DataCompBoy-}

procedure CM_EraseFiles;
  var
    FC: PFilesCollection;
    P: PFilePanelRoot absolute AFP;
  begin
    FC := GetSelection(P, Single);
    if (FC = nil) or (P^.Drive = nil) then
      exit;
    P^.Drive^.EraseFiles(FC);
    FC^.DeleteAll;
    Dispose(FC, Done);
    P^.SetDirName;
    P^.SendLocated;
  end;

procedure CM_MakeList;
  var
    FC: PCollection;
    P: PFilePanelRoot absolute AFP;
  begin
    if (P^.Files^.Count = 0) then
      exit;
    if (P^.SelNum = 0) then
      while SelectFiles(P, True, False) do
        ;
    if (P^.SelNum = 0) then
      exit;
    if (ActivePanel = P) and (P^.Drive^.DriveType = dtDisk)
    then
      CurrentDirectory := P^.DirectoryName;
    FC := GetSelection(P, False);
    if FC = nil then
      exit;
    MakeListFile(P, FC);
    FC^.DeleteAll;
    Dispose(FC, Done);
    P^.SetDirName;
  end { CM_MakeList };

{-DataCompBoy-}
procedure CM_SetAttributes;
  var
    D: record
      t: String[8];
      D: String[10];
      T_Cr: String[8];
      D_Cr: String[10];
      T_LAc: String[8];
      D_LAc: String[10];
      s, C: word;
      end;
    i, UU, DD: longInt;
    k: word;
    s: String;
    R: TRect;
    PInfo: PWhileView;
    Dtt, DTT1, DTT_Cr, DTT1_Cr, DTT_LAc, DTT1_LAc: longInt;
    DT, DT1, DT_Cr, DT1_Cr, DT_LAc, DT1_LAc: DateTime;
    DateSet, TimeSet, CrDateSet, CrTimeSet, LAcDateSet, LAcTimeSet:
      boolean;
    Res: word;
    Dlg: PDialog;
    P: PFilePanelRoot absolute AFP;
    PF: PFileRec;
    Ok: boolean;
    CmdDlg: word; {JO}
  label CurTime, CurTime1; {JO}

  function CutNumber(var s: String): word;
    var
      s1: String[10];
      i, j: integer;
    begin
      CutNumber := 0;
      if not Ok then
        exit;
      if s = '' then
        begin
          Ok := False;
          exit;
        end;
      s1 := '';
      while (not (s[1] in ['0'..'9'])) and (s <> '') do
        Delete(s, 1, 1); {DelFC(S);}
      while (s[1] in ['0'..'9']) and (s <> '') do
        begin
          s1 := s1+s[1]; {AddStr(S1,S[1]);}
          Delete(s, 1, 1); {DelFC(S);}
        end;
      Val(s1, i, j);
      Ok := j = 0;
      CutNumber := i;
    end { CutNumber };

  begin { CM_SetAttributes }
    if (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArcFind)
    then
      exit;
    NotifySuspend; {Cat}
    FillChar(D, SizeOf(D), 0);
    if (P^.SelNum > 0) and not Single then
      begin
        Dlg := PDialog(LoadResource(dlgFilesAttr));
CurTime1:
        Dlg^.SetData(D);
        CmdDlg := Desktop^.ExecView(Dlg);
        case CmdDlg of
          cmYes:
            begin
              D.t := GetDateTime(True);
              D.D := GetDateTime(False);
              D.T_Cr := GetDateTime(True);
              D.D_Cr := GetDateTime(False);
              D.T_LAc := GetDateTime(True);
              D.D_LAc := GetDateTime(False);
              goto CurTime1;
            end;
          cmNo:
            begin
              D.t := '';
              D.D := '';
              D.T_Cr := '';
              D.D_Cr := '';
              D.T_LAc := '';
              D.D_LAc := '';
              goto CurTime1;
            end;
          cmCancel:
            begin
              Dispose(Dlg, Done);
              NotifyResume; {Cat}
              exit;
            end;
        end {case};
        Dlg^.GetData(D);
        Dispose(Dlg, Done);
        NotifyResume; {Cat}
      end
    else
      begin
        Single := True;
        PF := P^.Files^.At(CurPos);
        if PF^.TType = ttUpDir then
          begin
            NotifyResume; {Cat}
            exit;
          end;
        s := MakeNormName(PF^.Owner^, PF^.FlName[uLfn]);
        AddStr(s, #0);
        SetLength(s, Length(s)-1);
        Dlg := PDialog(LoadResource(dlgFileAttr));
        D.C := GetFileAttr(s);
        if GetFileAges(s, Dtt, DTT_Cr, DTT_LAc) <> 0 then
          begin
            NotifyResume; {Cat}
            exit;
          end;
        UnpackTime(Dtt, DT);
        UnpackTime(DTT_Cr, DT_Cr);
        UnpackTime(DTT_LAc, DT_LAc);
        D.t := SStr(DT.Hour, 2, '0')+':'+SStr(DT.Min, 2, '0')+':'+
          SStr(DT.Sec, 2, '0');
        D.D := ItoS(DT.Day)+'-'+SStr(DT.Month, 2, '0')+'-'+ItoS(DT.
          Year);
        D.T_Cr := SStr(DT_Cr.Hour, 2, '0')+':'+SStr(DT_Cr.Min, 2,
          '0')+':'+SStr(DT_Cr.Sec, 2, '0');
        D.D_Cr := ItoS(DT_Cr.Day)+'-'+SStr(DT_Cr.Month, 2, '0')+'-'+
          ItoS(DT_Cr.Year);
        D.T_LAc := SStr(DT_LAc.Hour, 2, '0')+':'+SStr(DT_LAc.Min, 2,
          '0')+':'+SStr(DT_LAc.Sec, 2, '0');
        D.D_LAc := ItoS(DT_LAc.Day)+'-'+SStr(DT_LAc.Month, 2, '0')+
          '-'+ItoS(DT_LAc.Year);
        D.s := D.C;
CurTime:
        Dlg^.SetData(D);
        CmdDlg := Desktop^.ExecView(Dlg);
        case CmdDlg of
          cmYes:
            begin
              D.t := GetDateTime(True);
              D.D := GetDateTime(False);
              D.T_Cr := GetDateTime(True);
              D.D_Cr := GetDateTime(False);
              D.T_LAc := GetDateTime(True);
              D.D_LAc := GetDateTime(False);
              goto CurTime;
            end;
          cmNo:
            begin
              D.t := '';
              D.D := '';
              D.T_Cr := '';
              D.D_Cr := '';
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
              Dispose(Dlg, Done);
              NotifyResume; {Cat}
              exit;
            end;
        end {case};
        Dlg^.GetData(D);
        {$IFNDEF OS2}
        Dlg^.EnableCommands([cmYes, cmNo]);
        {$ENDIF}
        Dispose(Dlg, Done);
        D.C := {39}$FF;
      end;

    DTT1 := $FFFFFF;
    DTT1_Cr := $FFFFFF;
    DTT1_LAc := $FFFFFF;

    DateSet := (D.D <> '');
    CrDateSet := (D.D_Cr <> '');
    LAcDateSet := (D.D_LAc <> '');
    TimeSet := (D.t <> '');
    CrTimeSet := (D.T_Cr <> '');
    LAcTimeSet := (D.T_LAc <> '');

    with DT1 do
      begin
        Ok := TimeSet;
        Hour := CutNumber(D.t);
        Min := CutNumber(D.t);
        if Ok then
          begin
            Sec := CutNumber(D.t);
            Ok := True;
          end;
        TimeSet := Ok and not ((Hour > 24) or (Min > 59) or (Sec > 59));

        Ok := DateSet;
        Day := CutNumber(D.D);
        Month := CutNumber(D.D);
        Year := CutNumber(D.D);
        DateSet := Ok and not ((Day > 31) or (Day < 1) or (Month > 12)
          or (Month < 1));

        if DateSet then
          if Year < 80 then
            Inc(Year, 2000)
          else if Year < 100 then
            Inc(Year, 1900);

      end;

    with DT1_Cr do
      begin
        Ok := CrTimeSet;
        Hour := CutNumber(D.T_Cr);
        Min := CutNumber(D.T_Cr);
        if Ok then
          begin
            Sec := CutNumber(D.T_Cr);
            Ok := True;
          end;
        CrTimeSet := Ok and not ((Hour > 24) or (Min > 59) or (Sec > 59)
          );

        Ok := CrDateSet;
        Day := CutNumber(D.D_Cr);
        Month := CutNumber(D.D_Cr);
        Year := CutNumber(D.D_Cr);
        CrDateSet := Ok and not ((Day > 31) or (Day < 1) or (Month > 12)
          or (Month < 1));

        if CrDateSet then
          if Year < 80 then
            Inc(Year, 2000)
          else if Year < 100 then
            Inc(Year, 1900);

      end;

    with DT1_LAc do
      begin
        Ok := LAcTimeSet;
        Hour := CutNumber(D.T_LAc);
        Min := CutNumber(D.T_LAc);
        if Ok then
          begin
            Sec := CutNumber(D.T_LAc);
            Ok := True;
          end;
        LAcTimeSet := Ok and not ((Hour > 24) or (Min > 59) or (Sec > 59
          ));

        Ok := LAcDateSet;
        Day := CutNumber(D.D_LAc);
        Month := CutNumber(D.D_LAc);
        Year := CutNumber(D.D_LAc);
        LAcDateSet := Ok and not ((Day > 31) or (Day < 1) or (Month > 12
          ) or (Month < 1));

        if LAcDateSet then
          if Year < 80 then
            Inc(Year, 2000)
          else if Year < 100 then
            Inc(Year, 1900);

      end;

    R.Assign(1, 1, 26, 8);
    New(PInfo, Init(R));
    PInfo^.Top := GetString(dlSetAttr);
    PInfo^.Bottom := '';
    PInfo^.SetState(sfShadow, True);
    Desktop^.Insert(PInfo);

    if Single then
      begin
        UU := CurPos+1;
        DD := UU
      end
    else
      begin
        UU := 1;
        DD := P^.Files^.Count
      end;
    for i := UU to DD do
      begin
        PF := P^.Files^.At(i-1);
        if PF^.Selected or Single then
          begin
            s := Cut(PF^.FlName[uLfn], 40);
            PInfo^.Write(1, s);
            s := MakeNormName(PF^.Owner^, PF^.FlName[uLfn]);
            AddStr(s, #0);
            SetLength(s, Length(s)-1);
            k := (GetFileAttr(s) and (not D.C)) or D.s;
            if {$IFNDEF OS2}(k and Directory = 0) and {$ENDIF}
              TimeSet or DateSet or CrTimeSet or CrDateSet or
                LAcTimeSet or LAcDateSet
            then
              begin
                GetFileAges(s, Dtt, DTT_Cr, DTT_LAc);
                UnpackTime(Dtt, DT);
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
                DT.Sec := 0;
                DT_Cr.Sec := 0;
                DT_LAc.Sec := 0;
                UnpackTime(DTT1, DT);
                UnpackTime(DTT1_Cr, DT_Cr);
                UnpackTime(DTT1_LAc, DT_LAc);
                SetFileAges(s, DTT1, DTT1_Cr, DTT1_LAc);
              end;
            SetFileAttr(s, k and not Directory and not VolumeID);
            if not Single then
              PF^.Selected := False;
          end;
      end;
    Desktop^.Delete(PInfo);
    Dispose(PInfo, Done);
    if (ActivePanel = P) and (P^.Drive^.DriveType = dtDisk)
    then
      CurrentDirectory := P^.DirectoryName;
    RereadDirectory(P^.DirectoryName);
    if P^.Drive^.DriveType = dtDisk then
      GlobalMessage(evCommand, cmRereadInfo, nil);
    GlobalMessage(evCommand, cmRereadTree, @P^.DirectoryName);
    P^.SetDirName;
    NotifyResume; {Cat}
  end { CM_SetAttributes };
{-DataCompBoy-}

procedure CM_SetShowParms;
  var
    i: record
      Prm: word;
      LFNLen: String[3];
      EXTLen: String[3]
      end;
    j, k, Old: word;
    Idx: TDlgIdx;
    P: PFilePanelRoot absolute AFP;
  begin
    with P^.Drive^ do
      begin
        Old := Flags;
        case DriveType of
          dtFind, dtTemp, dtList, dtArcFind:
            Idx := dlgFindParms;
          dtArc:
            Idx := dlgArcParms;
          else
            Idx := dlgDiskParms;
        end {case};
        i.Prm := Flags;
        i.LFNLen := ItoS(LFNLen);
        i.EXTLen := ItoS(EXTLen);
        j := ExecResource(Idx, i);
        case j of
          cmOK:
            begin
              j := StoI(i.LFNLen);
              if j < 5 then
                exit;
              if j > 252 then
                j := 252;
              LFNLen := j;
              k := StoI(i.EXTLen);
              if k > 247 then
                k := 247;
              if (j-k >= 5) then
                EXTLen := k
              else
                EXTLen := j-5;
              Flags := i.Prm;
            end;
          cmYes:
            GetParam(1);
          cmNo:
            GetParam(2);
          else
            exit;
        end {case};
        P^.DeltaX := 0;
        if ((DriveType = dtDisk) or (DriveType = dtArvid))
          and ((Flags xor Old) and (psShowDescript+psShowLongDesc) <> 0)
        then
          P^.RereadDir;
        {$IFNDEF OS2}
        if not (P^.Drive^.DriveType in [dtArc, dtArcFind]) then
          P^.Files^.LFNActive := (Flags and psShowLongName) <> 0;
        {$ENDIF}
      end;
    DrawViews(P);
    P^.SendLocated;
    DrawViews(P);
  end { CM_SetShowParms };

procedure CM_CopyTemp;
  var
    FC: PFilesCollection;
    C: TCopyRec;
    P: PFilePanelRoot absolute AFP;
  begin
    FC := GetSelection(P, False);
    C.Owner := P;
    C.FC := FC;
    if FC = nil then
      exit;
    GlobalEvent(evBroadcast, cmCopyToTemp, @C);
    FC^.DeleteAll;
    Dispose(FC, Done);
  end;

{-DataCompBoy-}
procedure CM_ArchiveFiles;
  var
    PC: PCollection;
    s: String;
    P: PFilePanelRoot absolute AFP;
    Dir: DirStr;
    Name: NameStr;
    ext: ExtStr;
  begin
    if P^.Drive^.DriveType >= dtArcFind then
      exit;
    PC := GetSelection(P, False);
    if PC = nil then
      exit;
    if PC^.Count = 1 then
      s := PFileRec(PC^.At(0))^.FlName[uLfn]
    else if P^.Drive^.DriveType = dtDisk then
      s := GetName(P^.DirectoryName)
    else
      s := '';
    {Cat}
    FSplit(s, Dir, Name, ext); {AK155: не уверен, что это нужно }
    MakeArchive(Name, PC, False, False, P);
    {/Cat}
    PC^.DeleteAll;
  end { CM_ArchiveFiles };
{-DataCompBoy-}

{$IFDEF Printer}
procedure CM_Print;
  var
    n: integer;
    P: PFilePanelRoot absolute AFP;
  begin
    if (P^.Drive^.DriveType in [dtDisk, dtFind, dtTemp, dtList])
    then
      PrintFiles(GetSelection(P, False), P);
  end;
{$ENDIF}

procedure CM_ToggleDescriptions;
  var
    P: PFilePanelRoot absolute AFP;
  begin
    case P^.Drive^.DriveType of
      dtDisk,
      dtArvid:
        begin
          P^.Drive^.Flags := P^.Drive^.Flags xor psShowDescript;
          P^.Drive^.Flags := P^.Drive^.Flags;
          P^.DeltaX := 0;
          P^.RereadDir;
          DrawViews(P);
        end;
      dtFind,
      dtList,
      dtArcFind,
      dtTemp:
        begin
          P^.Drive^.Flags := P^.Drive^.Flags xor psShowDir;
          DrawViews(P);
        end;

    end {case};
  end { CM_ToggleDescriptions };

procedure CM_ToggleLongNames;
  var
    P: PFilePanelRoot absolute AFP;
  begin
    if P^.Drive^.DriveType in [dtArc, dtArcFind] then
      exit;
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
    {  P^.RereadDir; }
      {AK155 22-07-2002
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
  var
    P: PFilePanelRoot absolute AFP;
  begin
    P^.DeltaX := 0;
    if P^.Drive^.Param <> 2 then
      begin
        P^.Drive^.OldParam := P^.Drive^.Param;
        P^.Drive^.GetParam(2)
      end
    else if P^.Drive^.OldParam <> 2 then
      P^.Drive^.GetParam(P^.Drive^.OldParam)
    else
      P^.Drive^.GetParam(1);
    P^.RereadDir;
    DrawViews(P);
    {Message(Desktop, evCommand, cmMaxi, nil);}
  end;

type
  PDragger = ^TDragger;
  TDragger = object(TView)
    text: PString;
    Constructor Init(R: TRect; AText: String);
    procedure Draw; virtual;
    destructor Done; virtual;
    end;

procedure DragMover;
  var
    R: TRect;
    Mover: PView;
    Event: TEvent;
    P: PPoint absolute AP;
    FC: PFilesCollection absolute AFC;
    C: PCopyRec absolute AC;

  function ContainsMouse(P: PView): boolean;
    begin
      ContainsMouse := (P^.State and sfVisible <> 0) and
      P^.MouseInView(R.A);
    end;

  begin
    {Application^.BFSpeed;}
    Desktop^.MakeLocal(P^, R.A);
    Mover := New(PDragger, Init(R, text));
    Desktop^.Insert(Mover);
    Desktop^.GetExtent(R);
    Event.Where := P^;
    Event.What := evMouseDown;
    Event.Buttons := mbLeftButton;
    Event.Double := False;
    Mover^.DragView(Event, dmDragMove, R, Mover^.Size, Mover^.Size);
    R.A := Mover^.Origin;
    Mover^.Free;
    if Event.What = evMouseUp then
      {AK155 13-08-2003 Может быть не evMouseUp, если во время
    перетаскивания нажали Esc, см. TView.DragView}
      begin
        C^.FC := FC;
        Desktop^.MakeGlobal(R.A, R.A);
        C^.Where := R.A;
        Message(Desktop^.FirstThat(@ContainsMouse), evBroadcast,
          cmDropped, C);
      end;
    Dispose(FC, Done);
  end { DragMover };

Constructor TDragger.Init;
  begin
    AText := ' '+AText+' ';
    R.B.X := R.A.X+Length(AText);
    R.B.Y := R.A.Y+1;
    inherited Init(R);
    Options := Options or ofTopSelect;
    text := NewStr(AText);
    SetState(sfShadow, True);
  end;

procedure TDragger.Draw;
  var
    B: TDrawBuffer;
    C: word;
  begin
    C := $3B30;
    MoveStr(B, text^, C);
    WriteLine(0, 0, Size.X, Size.Y, B);
  end;

destructor TDragger.Done;
  begin
    DisposeStr(text);
    inherited Done;
  end;

procedure CM_DragDropper;
  var
    C: TCopyRec;
    FC: PFilesCollection;
    i: longInt;
    s: String;
    P: PFilePanelRoot absolute AFP;

  begin
    C.Owner := P;
    if (P^.Files^.Count = 0) or (P^.Files^.Count <= CurPos) then
      exit;
    {if (StartupData.Slice2 and osuReleaseDrag = 0) then Application^.BFSpeed;}
    if (PFileRec(P^.Files^.At(CurPos))^.Selected) or
      (TypeOf(P^) <> TypeOf(TFilePanel))
    then
      FC := GetSelection(P, False)
    else if not (PFileRec(P^.Files^.At(CurPos))^.TType = ttUpDir)
    then
      begin
        New(FC, Init(1, 1)); {AK155: заменил 10,10 на 1,1}
        FC^.Insert(P^.Files^.Items^[CurPos]);
      end
    else
      exit;
    for i := 1 to FC^.Count do
      FC^.AtPut(i-1, CopyFileRec(PFileRec(FC^.Items^[i-1])));
    if FC^.Count = 1 then
      s := Cut(PFileRec(FC^.At(0))^.FlName[uLfn], 20)
    else
      s := ItoS(FC^.Count)+GetString(dlSelectedFiles);
    DragMover(@TEvent(EV^).Where, s, FC, @C);
    {if (StartupData.Slice2 and osuReleaseDrag = 0) then Application^.EFSpeed;}
  end { CM_DragDropper };

procedure CM_Dropped;
  var
    P: PFilePanelRoot absolute AFP;
    MPos: TPoint;
    i, j, k: longInt;
    EV: TEvent;
    s: String;
  begin
    if ReflectCopyDirection
    then
      RevertBar := (Message(Desktop, evBroadcast, cmIsRightPanel, P) <>
        nil)
    else
      RevertBar := False;
    MPos := PCopyRec(EI)^.Where;
    P^.MakeLocal(MPos, MPos);
    i := P^.Delta+(MPos.X div P^.LineLength)*(P^.Size.Y-byte(FMSetup.
      Show and fmsColumnTitles <> 0))
    +MPos.Y-byte(FMSetup.Show and fmsColumnTitles <> 0);
    if (PCopyRec(EI)^.Owner = AFP) and
      ((MPos.Y < 0) or (i < 0) or (i >= P^.Files^.Count) or
      ((MPos.Y = 0) and (FMSetup.Show and fmsColumnTitles > 0)) or
      (PFileRec(P^.Files^.At(i))^.Attr and Directory = 0))
    then
      exit;
    CopyDirName := P^.DirectoryName;
    if PCopyRec(EI)^.Owner <> nil then
      begin
        if (P^.Drive^.DriveType = dtArc) then
          begin
            CopyDirName := P^.Drive^.GetRealName;
            SkipCopyDialog := Confirms and cfMouseConfirm = 0;
            Message(PCopyRec(EI)^.Owner, evBroadcast,
              cmCopyCollection,
            PCopyRec(EI)^.FC);
            SkipCopyDialog := False;
            exit;
          end;
        EV.What := evBroadcast;
        EV.Command := cmUnArchive;
        EV.InfoPtr := EI;
        PCopyRec(EI)^.Owner^.HandleEvent(EV);
        if EV.What = evNothing then
          exit;
      end;
    if (i < P^.Files^.Count) and (i >= 0) then
      begin
        s := P^.DirectoryName;
        if ((MPos.Y > 0) or ((FMSetup.Show and fmsColumnTitles = 0)
            and (MPos.Y = 0)))
          and (PFileRec(P^.Files^.At(i))^.Attr and Directory <> 0)
        then
          s := MakeNormName(s, PFileRec(P^.Files^.At(i))^.FlName[
            uLfn]);
        CopyDirName := s;
      end;
    SkipCopyDialog := Confirms and cfMouseConfirm = 0;
    if SkipCopyDialog then
      begin
        s := CopyDirName;
        if (Length(s) > 3) and (s[Length(s)] = '\') then
          SetLength(s, Length(s)-1);
        UpStr(s);
        for j := 0 to PCopyRec(EI)^.FC^.Count-1 do
          with PFileRec(PCopyRec(EI)^.FC^.At(j))^ do
            if (Attr and Directory <> 0) and (UpStrg(MakeNormName(
                Owner^, FlName[uLfn])) = s)
            then
              exit;
      end;
    P^.Drive^.CopyFilesInto(PCopyRec(EI)^.FC, PCopyRec(EI)^.Owner,
    ShiftState and 7 <> 0);
    SkipCopyDialog := False;
  end { CM_Dropped };

{-DataCompBoy-}
procedure CM_RenameSingleL;
  var
    PIF: PInputFName;
    R: TRect;
    s, s2: String;
    DosE: word;
    SSS: String;
    PF: PFileRec;
    Nm: String;
    SN: String;
    P: PFilePanelRoot absolute AFP;
    Event: PEvent absolute PEV;
    ScrollBarValue: integer;
    ReEnableCmdLine: boolean;
    DlgRes: word;

  begin
    if (P^.Files = nil) then
      exit;
    ScrollBarValue := P^.ScrollBar^.Value;
    if ScrollBarValue >= P^.Files^.Count then
      exit;
    PF := P^.Files^.At(ScrollBarValue);
    if (PF^.TType = ttUpDir) or
      {JO: для найденных в архиве файлов в панели поиска}
      PathFoundInArc(PF^.Owner^) or
      {/JO}
      not (P^.Drive^.DriveType in [dtDisk, dtTemp, dtFind, dtList])
    then
      exit;
    s := PF^.FlName[uLfn];
    s2 := s;
    {$IFNDEF OS2}
    if DescrByShortNames then
      Nm := PF^.FlName[False]
    else
      {$ENDIF}
      Nm := PF^.FlName[True];
    {$IFNDEF OS2}
    if (P^.Drive^.Flags and psShowLongName = 0)
    then
      R.Assign(0, 0, 13, 1)
    else
      {$ENDIF} if (P^.Drive^.LFNLen < 250)
    then
      R.Assign(0, 0, P^.Drive^.LFNLen+1, 1)
    else
      R.Assign(P^.Drive^.CalcLengthWithoutName, 0, 255, 1);
    R.Move(P^.LastCurPos.X, P^.LastCurPos.Y);
    R.Move(P^.Origin.X, 0);
    Dec(R.A.X);
    if R.B.X > P^.Size.X+P^.Origin.X+1 then
      R.B.X := P^.Size.X+P^.Origin.X+1;
    if R.B.X-P^.Origin.X-P^.Size.X = 0 then
      Inc(R.B.X);
    New(PIF, Init(R, 255));
      {PIF^.State := PIF^.State or sfCursorIns;}
    if P^.Origin.X-R.A.X = 1
    then
      begin
        PIF^.LC := #186;
        PIF^.CL := RK;
      end
    else
      begin
        PIF^.LC := #179;
        PIF^.CL := P^.GetColor(2);
      end;
    if R.B.X-P^.Origin.X-P^.Size.X = 1
    then
      begin
        PIF^.rc := #186;
        PIF^.CR := RK;
      end
    else
      begin
        PIF^.rc := #179;
        PIF^.CR := P^.GetColor(2);
      end;
    if (PF^.TType = 0) or (P^.PanelFlags and fmiHiliteFiles = 0)
    then
      PIF^.C1 := P^.GetColor(1)
    else
      PIF^.C1 := P^.GetColor(6+PF^.TType);
    PIF^.C2 := PIF^.C1;
    PIF^.C3 := P^.GetColor(4);
    {$IFDEF RecodeWhenDraw}
    s := CharToOemStr(s);
    {$ENDIF}
    PIF^.SetData(s);
    PIF^.SetValidator(New(PFilterValidator, Init([#32..#255]-
      IllegalCharSet-['\', '/', '*', '?', '"'])));
    PIF^.SelectAll(False);

    {AK155 Чтобы комстрока не забирала курсор себе, отключаем
         ее на время работы }
    ReEnableCmdLine := (CommandLine <> nil) and
    not CommandLine.GetState(sfDisabled);
    if ReEnableCmdLine then
      begin
        CommandLine^.SetState(sfDisabled, True);
        CommandLine^.UpDate;
      end;

    NotifySuspend; {AK155 25-01-2004 Если не отключить автообновление
      на время редактирования, то при изменеии состава файлов панель
      перерисовывается очень странно, так как строка ввода имеет
      уже зафиксированную позицию }

    { Flash 09-06-2003 >>> }
    PIF^.HelpCtx := hcRenameFile;
    DlgRes := {P^.Owner}Desktop^.ExecView(PIF);
    PIF^.GetData(s);
    { Flash 09-06-2003 <<< }

    if ReEnableCmdLine then
      CommandLine^.SetState(sfDisabled, False);

    if s[Length(s)] = '.' then
      SetLength(s, Length(s)-1);
    {$IFDEF RecodeWhenDraw}
    if s = CharToOemStr(s2) then
      DlgRes := cmCancel;
    s := OemToCharStr(s);
    {$ELSE}
    if s = s2 then
      DlgRes := cmCancel;
    {$ENDIF}
    if DlgRes <> cmCancel then
      begin
        HistoryAdd(hsRenameFil, s);
          { Flash 09-06-2003, AK155 21-08-2003 }
        lGetDir(0, SSS);
        lChDir(PF^.Owner^);
        ClrIO;
        lChangeFileName(s2, s);
        DosE := IOResult;
        if DosE <> 0 then
          MessFileNotRename(s2, s, DosE)

        else
          begin
            {AK155 Так делать нельзя и не нужно  CopyShortString(S, PF^.FlName[uLfn]);}
            {AK155 А таки нужно. Глючит, например, при переименовании в ветви каталога.}
            CopyShortString(s, PF^.FlName[uLfn]);
            {$IFNDEF OS2}
            if DescrByShortNames then
              SN := LowStrg(GetName(lfGetShortFileName(s)))
            else{$ENDIF}
              SN := s;

            s2 := GetPossibleDizOwner(1);
            if s2 <> '' then
              begin
                s2 := GetDizOwner(PF^.Owner^, s2, False);
                if (PF^.DIZ <> nil) then
                  begin
                    ReplaceDiz(s2, Nm, @Sn, PF^.DIZ^.DIZ);
                    DeleteDiz(s2, Nm);
                  end;
              end;
          end;
        lChDir(SSS);
        P^.Owner^.Delete(PIF);
        Dispose(PIF, Done);
{AK155 25-01-2004
        if not DnIni.AutoRefreshPanels then}
{Поскольку автообновление отключено, надо перечитать. Кроме того,
даже если бы оно было, перечитывание с задержкой здорово раздражает}
        GlobalMessage(evCommand, cmRereadDir, PF^.Owner);
        NotifyResume; {AK155 25-01-2004}
        ClrIO;
        if DlgRes <> cmOK then
          begin
            Event^.What := evKeyDown;
            Event^.KeyCode := DlgRes;
            Event^.InfoPtr := nil;
            P^.PutEvent(Event^);
          end;
      end
    else
      PIF^.Free;
  end { CM_RenameSingleL };
{-DataCompBoy-}

{-DataCompBoy-}
procedure CM_RenameSingleDialog;
  var
    R: TRect;
    s, s2: String;
    DosE: word;
    SSS: String;
    PF: PFileRec;
    Nm: String;
    SN: String;
    DlgRes: word;
    P: PFilePanelRoot absolute AFP;
    Event: PEvent absolute PEV;
    ScrollBarValue: integer;
    U: lFile;

  begin
    if (P^.Files = nil) then
      exit;
    ScrollBarValue := P^.ScrollBar^.Value;
    if ScrollBarValue >= P^.Files^.Count then
      exit;
    PF := P^.Files^.At(ScrollBarValue);
    if (PF^.TType = ttUpDir) or
      {JO: для найденных в архиве файлов в панели поиска}
      PathFoundInArc(PF^.Owner^) or
      {/JO}
      not (P^.Drive^.DriveType in [dtDisk, dtTemp, dtFind, dtList])
    then
      exit;
    s := PF^.FlName[uLfn];
    {$IFNDEF OS2}
    if DescrByShortNames then
      Nm := PF^.FlName[False]
    else{$ENDIF}
      Nm := PF^.FlName[True];
    s2 := s;
    {$IFDEF RecodeWhenDraw}
    s := CharToOemStr(s);
    {$ENDIF}
    DlgRes := ExecResource(dlgRenFl, s);
    {$IFDEF RecodeWhenDraw}
    s := OemToCharStr(s);
    {$ENDIF}
    if DlgRes = cmCancel then
      exit;
    if DlgRes = cmYes then
      UpStr(s);
    if DlgRes = cmNo then
      LowStr(s);
    if s[Length(s)] = '.' then
      SetLength(s, Length(s)-1);

    if s = s2 then
      DlgRes := cmCancel;
    if DlgRes <> cmCancel then
      begin
        lGetDir(0, SSS);
        lChDir(PF^.Owner^);
        ClrIO;
        lChangeFileName(s2, s);
        DosE := IOResult;
        if DosE <> 0 then
          MessFileNotRename(s2, s, DosE)

        else
          begin
            CopyShortString(s, PF^.FlName[uLfn]);
            {$IFNDEF OS2}
            if DescrByShortNames then
              SN := LowStrg(GetName(lfGetShortFileName(s)))
            else{$ENDIF}
              SN := s;
            s2 := GetPossibleDizOwner(1);
            if s2 <> '' then
              begin
                s2 := GetDizOwner(PF^.Owner^, s2, False);
                if (PF^.DIZ <> nil) then
                  begin
                    ReplaceDiz(s2, Nm, @Sn, PF^.DIZ^.DIZ);
                    DeleteDiz(s2, Nm);
                  end;
              end;
          end;
        lChDir(SSS);
        if not DnIni.AutoRefreshPanels then
          GlobalMessage(evCommand, cmRereadDir, PF^.Owner);
        ClrIO;
        if DlgRes <> cmOK then
          begin
            Event^.What := evKeyDown;
            Event^.KeyCode := DlgRes;
            Event^.InfoPtr := nil;
            P^.PutEvent(Event^);
          end;
      end;
  end { CM_RenameSingleDialog };
{-DataCompBoy-}

procedure CM_SortBy;
  var
    Menu: PMenu;
    PM: PMenuItem;
    n, W: word;
    R: TRect;
    PV: PView;
    P: PFilePanelRoot absolute AFP;
    Cur: PFileRec;
    ScrollBarValue: longInt;
  begin
    Menu := NewMenu(
    NewItem(GetString(dlSortName), '', kbNoKey, cmSortName, 0,
    NewItem(GetString(dlSortExt), '', kbNoKey, cmSortExt, 0,
    NewItem(GetString(dlSortSize), '', kbNoKey, cmSortSize, 0,
    NewItem(GetString(dlSortDate), '', kbNoKey, cmSortDate, 0,
    {$IFNDEF DPMI32}
    NewItem(GetString(dlSortCrDate), '', kbNoKey, cmSortCrDate, 0,
    NewItem(GetString(dlSortLADate), '', kbNoKey, cmSortLADate, 0,
    {$ENDIF}
    NewItem(GetString(dlSortDIZ), '', kbNoKey, cmSortDIZ, 0,
    NewItem(GetString(dlSortType), '', kbNoKey, cmSortType, 0,
    NewItem(GetString(dlSortUnsorted), '', kbNoKey, cmSortUnsorted, 0,
    nil)))))))) {$IFNDEF DPMI32})) {$ENDIF};
    n := NumSortModes;
    W := 15;
    Desktop^.GetExtent(R);
    R.A := P^.Origin;
    P^.Owner^.MakeGlobal(R.A, R.A);
    Desktop^.MakeLocal(R.A, R.A);
    if R.A.X < 0 then
      R.A.X := 0
    else if R.A.X+W > R.B.X then
      R.A.X := R.B.X-W;
    if R.A.Y < 0 then
      R.A.Y := 0
    else if R.A.Y+n+2 > R.B.Y then
      R.A.Y := R.B.Y-n-2;
    R.B.X := R.A.X+100;
    R.B.Y := R.A.Y+100; {R.B := R.A;}
    PV := New(PMenuBox, Init(R, Menu, nil));
    PV^.HelpCtx := hcSortBy;
    Menu^.Default := LookUpMenu(PMenuBox(PV)^.Menu, P^.SortMode+
      cmSortUnsorted, dfByCommand);
    n := Desktop^.ExecView(PV);
    Dispose(PV, Done);
    DisposeMenu(Menu);
    if (n >= cmSortUnsorted) and (P^.SortMode+cmSortUnsorted <> n)
    then
      begin
        P^.SortMode := n-cmSortUnsorted;
        if P^.SortMode = psmUnsorted then
          P^.RereadDir;
        P^.Files^.SortMode := n-cmSortUnsorted;
        ScrollBarValue := P^.ScrollBar^.Value;
        if ScrollBarValue >= P^.Files^.Count then
          exit;
        Cur := P^.Files^.At(ScrollBarValue);
        if P^.Files^.SortMode <> psmUnsorted then
          P^.Files^.Sort;
        for ScrollBarValue := 0 to P^.Files^.Count-1 do
          if P^.Files^.At(ScrollBarValue) = Cur then
            begin
              P^.ScrollBar^.SetValue(ScrollBarValue);
              break;
            end;
        P^.DrawView;
        P^.SetDirName;
      end
  end { CM_SortBy };

{-DataCompBoy-}
function CM_ChangeDirectory;
  var
    P: PFilePanelRoot absolute AFP;
    s: String;
  begin
    CM_ChangeDirectory := '';
    if P^.Drive^.DriveType <> dtDisk then
      begin
        ClrIO; {!!!}
        lGetDir(0, s);
        if Abort then
          exit;
      end
    else
      s := P^.DirectoryName;
    CM_ChangeDirectory :=
    ChangeDir(GetString(dlChangeDir), byte(s[1])-64);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure CM_MakeDir;
  var
    dr: String;
    Nm: String;
    XT: String;
    PF: PFileRec;
    DN: String;
    i, j: longInt;
    P: PFilePanelRoot absolute AFP;
  begin
    P^.Drive^.MakeDir;
    if CreatedDir = '' then
      exit;
    lFSplit(CreatedDir, dr, Nm, XT);
    if (Length(dr) > 3) and (dr[Length(dr)] = '\') then
      SetLength(dr, Length(dr)-1);
    if dr <> P^.DirectoryName then
      exit;
    j := -1;
    DN := UpStrg(GetName(CreatedDir));
    for i := 1 to P^.Files^.Count do
      begin
        PF := P^.Files^.At(i-1);
        if UpStrg(PF^.FlName[True]) = GetName(DN) then
          begin
            j := i-1;
            break
          end;
      end;
    if j >= 0 then
      P^.ScrollBar^.SetValue(j);
    P^.ChDirName;
  end { CM_MakeDir };
{-DataCompBoy-}

{-DataCompBoy-}
procedure CM_LongCopy;
  var
    s: String;
    P: PFilePanelRoot absolute AFP;
    PF: PFileRec;
  begin
    if (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArcFind)
    then
      exit;
    PF := P^.Files^.At(P^.ScrollBar^.Value);
    if (PF^.Attr and Directory <> 0)
      {JO: для найденных в архиве файлов в панели поиска}
      or PathFoundInArc(PF^.Owner^)
      {/JO}
    then
      exit;
    s := MakeNormName(PF^.Owner^, PF^.FlName[uLfn]);
    LongCopy(s);
    if (ActivePanel = P) and (P^.Drive^.DriveType = dtDisk)
    then
      CurrentDirectory := P^.DirectoryName;
    RereadDirectory(P^.DirectoryName);
    if P^.Drive^.DriveType = dtDisk then
      begin
        GlobalMessage(evCommand, cmRereadInfo, nil);
        GlobalMessage(evCommand, cmRereadTree, @(P^.DirectoryName));
      end;
  end { CM_LongCopy };
{DataCompBoy}

procedure CaseAsIs(var s: String; B, E: byte);
  begin
  end;

procedure CaseLow(var s: String; B, E: byte);
  begin
    for B := B to E do
      s[B] := LowCase(s[B]);
  end;

procedure CaseCap(var s: String; B, E: byte);
  begin
    while (s[B] in BreakChars) and (B <= E) do
      Inc(B);
    s[B] := UpCase(s[B]);
    Inc(B);
    while B <= E do
      begin
        s[B] := LowCase(s[B]);
        Inc(B);
      end;
  end;

procedure CaseCapAll(var s: String; B, E: byte);
  begin
    repeat
      while (B <= E) and (s[B] in BreakChars) do
        Inc(B);
      if B > E then
        break;
      s[B] := UpCase(s[B]);
      while (B < E) and (not (s[B] in BreakChars)) do
        begin
          Inc(B);
          s[B] := LowCase(s[B]);
        end;
    until B >= E;
  end;

procedure CaseUp(var s: String; B, E: byte);
  begin
    for B := B to E do
      s[B] := UpCase(s[B]);
  end;

procedure CM_ChangeCase(AFP: Pointer);
  type
    ChPr = procedure (var s: String; B, E: byte);

  var
    FC: PFilesCollection;
    l: TNamesCaseOptions;
    P: PFilePanelRoot absolute AFP;
    Info: PWhileView;
    TT: TEventTimer;
    R: TRect;
    NameChange, ExtChange: ChPr;

  function DoChangeCase(PF: PFileRec): boolean;
    var
      s: String;
      SSS: String;
      Nm, SN: String;
      i: integer;
      U: lFile;
    begin
      if TimerExpired(TT) then
        begin
          DispatchEvents(Info, Abort);
          NewTimerSecs(TT, 2);
        end;
      DoChangeCase := Abort;
      if Abort then
        exit;
      {JO: для найденных в архиве файлов в панели поиска}
      if PathFoundInArc(PF^.Owner^) then
        exit;
      {/JO}
      s := PF^.FlName[True];
      Nm := s;
      for i := Length(s) downto 1 do
        if s[i] = '.' then
          break;
      {$IFDEF RecodeWhenDraw}
      if OemToCharStr(CharToOemStr(s)) = s then
        begin
          s := CharToOemStr(s);
          {$ENDIF}
          if s[i] = '.' then
            begin
              NameChange(s, 1, i);
              ExtChange(s, i, Length(s));
            end
          else
            NameChange(s, 1, Length(s));
          {$IFDEF RecodeWhenDraw}
          s := OemToCharStr(s);
        end;
      {$ENDIF}
      if s = PF^.FlName[True] then
        i := 0
      else
        begin
          lGetDir(0, SSS);
          lChDir(PF^.Owner^);
          lChangeFileName(s, s);
          i := IOResult;
          lChDir(SSS);
        end;
      if i <> 0 then
        begin
          Info^.Hide;
          MessFileNotRename(PF^.FlName[True], s, i);
          MessageBox(GetString(dlFCNoRename1)+GetString(dlDIFile)+
            ^M^C+Cut(PF^.FlName[True], 20)
          +GetString(dlFCNoRename2)+Cut(s, 20), nil, mfError+
            mfOKButton);
          DoChangeCase := True;
        end
      else
        begin
          CopyShortString(GetName(s), PF^.FlName[True]);
          {JO: при изменении регистра файла имеет хоть какой-то смысл менять регистр }
          {    его имени в файле описаний только если описания даются по длинным     }
          {    именам, так как для коротких имён регистр не меняется                 }
          {$IFNDEF OS2}
          if not DescrByShortNames then
            begin
              {$ENDIF}
              SN := PF^.FlName[True];
              s := GetPossibleDizOwner(1);
              if s <> '' then
                begin
                  s := GetDizOwner(PF^.Owner^, s, False);
                  ReplaceDiz(s, Nm, @Sn, nil);
                end;
              {$IFNDEF OS2}
            end;
          {$ENDIF}
          Message(P, evCommand, cmCopyUnselect, PF);
        end;
    end { DoChangeCase };

  begin { CM_ChangeCase }
    if (P^.Drive = nil) then
      exit;
    FC := GetSelection(P, False);
    if (FC = nil) then
      exit;

    l := ChangeNamesCaseOptions;

    if ExecResource(dlgNameCase, ChangeNamesCaseOptions) = cmCancel
    then
      begin
        FC^.DeleteAll;
        Dispose(FC, Done);
        exit;
      end;

    if (l.Name <> ChangeNamesCaseOptions.Name)
      and (l.ext <> ChangeNamesCaseOptions.ext)
    then
      ConfigModified := True;

    if (ChangeNamesCaseOptions.Name = 0) and (ChangeNamesCaseOptions.
        ext = 0)
    then
      begin
        FC^.DeleteAll;
        Dispose(FC, Done);
        exit;
      end;

    case ChangeNamesCaseOptions.Name of
      {LoNg FIle nAMe}0:
        NameChange := CaseAsIs;
      {long file name}1:
        NameChange := CaseLow;
      {Long file name}2:
        NameChange := CaseCap;
      {Long File Name}3:
        NameChange := CaseCapAll;
      {LONG FILE NAME}4:
        NameChange := CaseUp;
      else
        exit;
    end {case};

    case ChangeNamesCaseOptions.ext of
      {LoNg FIle nAMe}0:
        ExtChange := CaseAsIs;
      {long file name}1:
        ExtChange := CaseLow;
      {Long file name}2:
        ExtChange := CaseCap;
      {Long File Name}3:
        ExtChange := CaseCapAll;
      {LONG FILE NAME}4:
        ExtChange := CaseUp;
      else
        exit;
    end {case};

    R.Assign(0, 0, 20, 7);
    New(Info, Init(R));
    Info^.Write(1, Copy(GetString(dlPleaseStandBy), 4,
      MaxStringLength));
    Desktop^.Insert(Info);
    NewTimer(TT, 18);
    Abort := False;
    FC^.FirstThat(@DoChangeCase);
    Abort := False;
    MessageL(P, evCommand, cmPanelReread, 0);
    Info^.Free;
    FC^.DeleteAll;
    Dispose(FC, Done);
  end { CM_ChangeCase };
procedure CM_SelectColumn(AFP: Pointer); {JO}
  var
    i, j, k: byte;
    {$IFNDEF OS2}
    NameT: array[1..10] of String[1];
    {$ENDIF}
    ISFull: array[1..10] of String[12];
    Descr: array[1..10] of String[25];
    PanelDescr: String;
    P: PFilePanelRoot absolute AFP;
    Menu: PMenu;
    Items: PMenuItem;
    R: TRect;
    PV: PView;
    n, W: word;

  begin

    case P^.Drive^.DriveType of
      dtDisk, dtLink:
        begin
          for i := 1 to 10 do
            begin
              {$IFNDEF OS2}
              if (ColumnsDefaultsDisk.Params[i].Param and 1 <> 0)
              then
                NameT[i] := GetString(dlNameLong)
              else
                NameT[i] := GetString(dlNameShort);
              {$ENDIF}
              if (ColumnsDefaultsDisk.Params[i].Param and 270 = 270)
              then
                ISFull[i] := GetString(dlFullInfo)
              else if (ColumnsDefaultsDisk.Params[i].Param and 16 <> 0)
              then
                ISFull[i] := GetString(dlDescr)
              else
                ISFull[i] := '';
              Descr[i] := '';
            end;
          PanelDescr := PanelDescrDrive;
        end;
      dtFind, dtList, dtArcFind:
        begin
          for i := 1 to 10 do
            begin
              {$IFNDEF OS2}
              if (ColumnsDefaultsFind.Params[i].Param and 1 <> 0)
              then
                NameT[i] := GetString(dlNameLong)
              else
                NameT[i] := GetString(dlNameShort);
              {$ENDIF}
              if (ColumnsDefaultsFind.Params[i].Param and 270 = 270)
              then
                ISFull[i] := GetString(dlFullInfo)
              else if (ColumnsDefaultsFind.Params[i].Param and 16 <> 0)
              then
                ISFull[i] := GetString(dlPath)
              else
                ISFull[i] := '';
              Descr[i] := '';
            end;
          PanelDescr := PanelDescrFind;
        end;
      dtTemp:
        begin
          for i := 1 to 10 do
            begin
              {$IFNDEF OS2}
              if (ColumnsDefaultsTemp.Params[i].Param and 1 <> 0)
              then
                NameT[i] := GetString(dlNameLong)
              else
                NameT[i] := GetString(dlNameShort);
              {$ENDIF}
              if (ColumnsDefaultsTemp.Params[i].Param and 270 = 270)
              then
                ISFull[i] := GetString(dlFullInfo)
              else if (ColumnsDefaultsTemp.Params[i].Param and 16 <> 0)
              then
                ISFull[i] := GetString(dlPath)
              else
                ISFull[i] := '';
              Descr[i] := '';
            end;
          PanelDescr := PanelDescrTemp;
        end;
      dtArc:
        begin
          for i := 1 to 10 do
            begin
              {$IFNDEF OS2}
              if (ColumnsDefaultsArch.Params[i].Param and 1 <> 0)
              then
                NameT[i] := GetString(dlNameLong)
              else
                NameT[i] := GetString(dlNameShort);
              {$ENDIF}
              if (ColumnsDefaultsArch.Params[i].Param and 62 = 62)
              then
                ISFull[i] := GetString(dlFullInfo)
              else
                ISFull[i] := '';
              Descr[i] := '';
            end;
          PanelDescr := PanelDescrArc;
        end;
      dtArvid:
        begin
          for i := 1 to 10 do
            begin
              {$IFNDEF OS2}
              if (ColumnsDefaultsArvd.Params[i].Param and 1 <> 0)
              then
                NameT[i] := GetString(dlNameLong)
              else
                NameT[i] := GetString(dlNameShort);
              {$ENDIF}
              if (ColumnsDefaultsArvd.Params[i].Param and 270 = 270)
              then
                ISFull[i] := GetString(dlFullInfo)
              else if (ColumnsDefaultsArvd.Params[i].Param and 16 <> 0)
              then
                ISFull[i] := GetString(dlDescr)
              else
                ISFull[i] := '';
              Descr[i] := '';
            end;
          PanelDescr := PanelDescrArvid;
        end;
    end {case};

    if PanelDescr <> '' then
      begin
        i := 1;
        j := 1;
        for k := 1 to Length(PanelDescr) do
          if (PanelDescr[k] = '#') or (k = Length(PanelDescr)) then
            begin
              if i > 10 then
                continue;
              if (j = 1) and (PanelDescr[1] <> '#') then
                begin
                  if (k = Length(PanelDescr)) and (PanelDescr[k] <>
                      '#')
                  then
                    Descr[i] := Copy(PanelDescr, j, k-j+1)
                  else
                    Descr[i] := Copy(PanelDescr, j, k-j);
                end
              else
                begin
                  if (k = Length(PanelDescr)) and (PanelDescr[k] <>
                      '#')
                  then
                    Descr[i] := Copy(PanelDescr, j+1, k-j)
                  else
                    Descr[i] := Copy(PanelDescr, j+1, k-j-1);
                end;
              if i <= 10 then
                i := i+1;
              j := k;
            end;
      end;

    Items := nil;

    case P^.Drive^.DriveType of
      dtDisk, dtLink:
        for i := 10 downto 1 do
          begin
            j := 48+i;
            if j = 58 then
              j := 48;
            Items := NewItem('~'+Char(j)+'~ '+ColumnsDefaultsDisk.
              Params[i].LFNLen+'.'+
            ColumnsDefaultsDisk.Params[i].EXTLen+' ' {$IFNDEF OS2}+
              NameT[i]+' ' {$ENDIF}
            +ISFull[i]+Descr[i], '', kbNoKey, 16000+i, 0, Items);
          end;
      dtFind, dtList, dtArcFind:
        for i := 10 downto 1 do
          begin
            j := 48+i;
            if j = 58 then
              j := 48;
            Items := NewItem('~'+Char(j)+'~ '+ColumnsDefaultsFind.
              Params[i].LFNLen+'.'+
            ColumnsDefaultsFind.Params[i].EXTLen+' ' {$IFNDEF OS2}+
              NameT[i]+' ' {$ENDIF}
            +ISFull[i]+Descr[i], '', kbNoKey, 16000+i, 0, Items);
          end;
      dtTemp:
        for i := 10 downto 1 do
          begin
            j := 48+i;
            if j = 58 then
              j := 48;
            Items := NewItem('~'+Char(j)+'~ '+ColumnsDefaultsTemp.
              Params[i].LFNLen+'.'+
            ColumnsDefaultsTemp.Params[i].EXTLen+' ' {$IFNDEF OS2}+
              NameT[i]+' ' {$ENDIF}
            +ISFull[i]+Descr[i], '', kbNoKey, 16000+i, 0, Items);
          end;
      dtArc:
        for i := 10 downto 1 do
          begin
            j := 48+i;
            if j = 58 then
              j := 48;
            Items := NewItem('~'+Char(j)+'~ '+ColumnsDefaultsArch.
              Params[i].LFNLen+'.'+
            ColumnsDefaultsArch.Params[i].EXTLen+' ' {$IFNDEF OS2}+
              NameT[i]+' ' {$ENDIF}
            +ISFull[i]+Descr[i], '', kbNoKey, 16000+i, 0, Items);
          end;
      dtArvid:
        for i := 10 downto 1 do
          begin
            j := 48+i;
            if j = 58 then
              j := 48;
            Items := NewItem('~'+Char(j)+'~ '+ColumnsDefaultsArvd.
              Params[i].LFNLen+'.'+
            ColumnsDefaultsArvd.Params[i].EXTLen+' ' {$IFNDEF OS2}+
              NameT[i]+' ' {$ENDIF}
            +ISFull[i]+Descr[i], '', kbNoKey, 16000+i, 0, Items);
          end;
    end {case};

    Menu := NewMenu(Items);

    n := 9;
    W := 15;
    Desktop^.GetExtent(R);
    R.A := P^.Origin;
    P^.Owner^.MakeGlobal(R.A, R.A);
    Desktop^.MakeLocal(R.A, R.A);
    if R.A.X < 0 then
      R.A.X := 0
    else if R.A.X+W > R.B.X then
      R.A.X := R.B.X-W;
    if R.A.Y < 0 then
      R.A.Y := 0
    else if R.A.Y+n+2 > R.B.Y then
      R.A.Y := R.B.Y-n-2;
    R.B.X := R.A.X+100;
    R.B.Y := R.A.Y+100; {R.B := R.A;}
    PV := New(PMenuBox, Init(R, Menu, nil));
    Menu^.Default := LookUpMenu(PMenuBox(PV)^.Menu, P^.Drive^.Param+
      16000, dfByCommand);
    n := Desktop^.ExecView(PV);
    Dispose(PV, Done);
    DisposeMenu(Menu);
    if n >= 16001 then
      begin
        n := n-16000;
        P^.Drive^.GetParam(n);
        P^.Owner^.Redraw;
      end;
    {$IFNDEF OS2}
    if not (P^.Drive^.DriveType in [dtArc, dtArcFind]) then
      P^.Files^.LFNActive := (P^.Drive^.Flags and psShowLongName) <> 0;
    {$ENDIF}
  end { CM_SelectColumn }; {JO}
{$IFDEF OS2}
procedure CM_SetEALongname(AFP: Pointer; CurPos: integer); {JO}
  var
    P: PFilePanelRoot absolute AFP;
    PF: PFileRec;
    s: String;
  begin
    if (P^.Files^.Count = 0) or (P^.Drive^.DriveType >= dtArcFind)
    then
      exit;
    PF := P^.Files^.At(CurPos);
    if (PF^.TType = ttUpDir)
      {JO: для найденных в архиве файлов в панели поиска}
      or PathFoundInArc(PF^.Owner^)
      {/JO}
    then
      exit;
    s := MakeNormName(PF^.Owner^, PF^.FlName[True]);
    SetEALongname(s);
  end;

{$ENDIF}

end.
