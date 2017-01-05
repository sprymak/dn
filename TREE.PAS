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

unit Tree;

interface

uses
  Lfn, Files, Collect,
  Drivers, Objects, Memory, Startup, Dos, DnIni,
  Dialogs, Views, advance, advance1, advance2, advance3,
  FilesCol, HideView, DNHelp
  ;

type
  PTreeWindow = ^TTreeWindow;
  TTreeWindow = object( {TStd}TWindow)
    Constructor Init(var Bounds: TRect);
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    end;

  PTreeReader = ^TTreeReader;
  TTreeReader = object(TView)
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PTreeDialog = ^TTreeDialog;
  TTreeDialog = object(TDialog)
    Tree: PView;
    isValid: boolean;
    Constructor Init(R: TRect; const ATitle: String; ADrive: byte);
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function GetPalette: PPalette; virtual;
    function Valid(Command: word): boolean; virtual;
    end;

  PDirRec = ^TDirRec;
  TDirRec = record
    Level: byte;
    Cluster: word;
    Size: TSize;
    Attr: byte;
    NumFiles: longInt;
    Date: longInt;
    Number: integer;
    DirName: TFlName;
    Dummy: array[1..SizeOf(ShortString)-SizeOf(TShortName)] of Char;
    {см. комментарий к TFileRec}
    end;

  PTreeView = ^TTreeView;
  TTreeView = object(THideView)
    ScrollBar: PScrollBar;
    CurPtr: PDirRec;
    isValid {, QuickSearch}: boolean;
    Drive, SearchPos: byte;
    SearchParam: TQuickSearchData; {DataCompBoy}
    CurPath, LastPath: String; {DataCompBoy}
    DC, Dirs: PCollection;
    Delta: TPoint;
    CurNum: integer;
    Parital, DrawDisabled,
    LocateEnabled, MouseTracking, WasChanged: boolean;
    Constructor Init(R: TRect; ADrive: integer; ParitalView: boolean;
    ScrBar: PScrollBar);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    function Valid(Command: word): boolean; virtual;
    function Expanded(P: PDirRec; i: integer): boolean;
    procedure SetState(AState: word; Enable: boolean); virtual;
    procedure ReadTree(CountLen: boolean);
    procedure Reread(CountLen: boolean);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure HandleCommand(var Event: TEvent);
    function GetDirName(n: integer): String;
    procedure SetData(var Rec); virtual;
    procedure GetData(var Rec); virtual;
    procedure CollapseBranch(n: integer);
    function DataSize: word; virtual;
    function FindDir(Dir: String): integer; {DataCompBoy}
    procedure ReadAfterLoad;
    function GetPalette: PPalette; virtual;
    procedure Draw; virtual;
    destructor Done; virtual;
    end;

  PTreePanel = ^TTreePanel;
  TTreePanel = object(TTreeView)
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  PTreeInfoView = ^TTreeInfoView;
  TTreeInfoView = object(THideView)
    Tree: PTreeView;
    Down: String;
    Loaded: boolean;
    Constructor Init(R: TRect; ATree: PTreeView);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure MakeDown;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    function GetPalette: PPalette; virtual;
    end;

  PDTreeInfoView = ^TDTreeInfoView;
  TDTreeInfoView = object(TTreeInfoView)
    function GetPalette: PPalette; virtual;
    end;

  PHTreeView = ^THTreeView;
  THTreeView = object(TTreePanel)
    Info: PView;
    Constructor Init(R: TRect; ADrive: integer; ParitalView: boolean;
    ScrBar: PScrollBar);
    procedure ChangeBounds(var Bounds: TRect); virtual;
    procedure HideView; virtual;
    procedure ShowView; virtual;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    function GetPalette: PPalette; virtual;
    end;

  PDirCollection = ^TDirCollection;
  TDirCollection = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
    function GetItem(var s: TStream): Pointer; virtual;
    procedure PutItem(var s: TStream; Item: Pointer); virtual;
    end;

function ChangeDir(ATitle: TTitleStr; Drv: byte): String;
  {DataCompBoy}
procedure CreateDirectory(s: String; Del: boolean);
procedure CheckMkDir(const Path: String); { MkDir and check result }
  {AK155}
procedure MakeDirectory;
procedure FreeTree(C: Char);
function GetDirLen(Dir: String): TSize; {DataCompBoy}
function CreateDirInheritance(var s: String; Confirm: boolean): byte;

const
  CreatedDir: String = '';
  TreeError: boolean = False;
  SaveTrees: boolean = True;

  CHTreeView = #15#16#17#18#19#20#21;
  CTreeInfoView = #22;
  CDTreeInfoView = #30;
  CTreeView = #35#36#37#38#39#40#41;
  CTreeDialog = CDialog+#104#105#106#107#108#109#110;

var
  DrvTrees: array['A'..'Z'] of record
    C: PCollection;
    len: boolean;
    end;

implementation
uses
  FlPanelX, DNApp, Messages, Commands, Drives, Eraser, Menus,
    RStrings,
  xTime, FileCopy;

const
  trExpanded = $02;
  trHasBranch = $04;
  cmRevert = 200;
  cmDirChanged = 201;

function ESC_Pressed: boolean;
  var
    E: TEvent;
  begin
    Application^.Idle;
    GetKeyEvent(E);
    ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbESC)
  end;
{DataCompBoy
function NotStr(S: String): String;
 var I: Integer;
begin
  for I := 1 to Length(S) do S[I] := Char(not Byte(S[I]) xor I);
  NotStr := S;
end;
}
procedure ReadTree(C: Char; CountLen: boolean);
  label Rep, DRep;
  var
    P: PDirRec;
    DCEntry: integer;
    Idx: integer;
    D, s: String; {DataCompBoy}
    Lv, i: integer;
    DSize: word;
    dr: array[1..255] of integer;
    DC: PDirCollection;
    tmr: TEventTimer;

  procedure ChkESC;
    begin
      if Abort then
        exit;
      if TimerExpired(tmr) then
        begin
          NewTimer(tmr, 3);
          Abort := ESC_Pressed;
        end;
    end;

  {-DataCompBoy-}
  procedure DOSReadDirectory(PD: PDirRec);
    var
      i: integer;
      Lv: integer;
      s: String;
      SR: lSearchRec;
      P: PDirRec;
    begin
      i := DC^.IndexOf(PD);
      Lv := PD^.Level;
      PD^.Size := 0;
      P := PD;
      s := MakeNormName(PD^.DirName[uLfn], '');
      repeat
        while (i > 0) and (P^.Level <= PDirRec(DC^.At(i))^.Level) do
            Dec(i);
        if DC^.IndexOf(PD) <> i then
          begin
            P := DC^.At(i);
            s := MakeNormName(P^.DirName[uLfn], s);
          end;
      until i = 0;
      ClrIO;
      if Abort then
        exit;
      i := DC^.IndexOf(PD);
      lFindFirst(s+x_x, AnyFile, SR); {JO}
      while (DOSError = 0) and not Abort do
        begin
          if {$IFNDEF OS2}(SR.SR.Name[1] <> '.')
            and (SR.SR.Attr and VolumeID = 0) {$ELSE}(SR.SR.Name <>
              '..') and (SR.SR.Name <> '.') {$ENDIF}
          then
            begin
              if SR.SR.Attr and Directory <> 0 then
                begin
                  New(P);
                  CopyShortString(SR.SR.Name, P^.DirName[uLfn]);
                  P^.Cluster := 0;
                  P^.NumFiles := 0;
                  P^.Size := -1;
                  P^.Level := PD^.Level+1;
                  Inc(i);
                  DC^.AtInsert(i, P);
                end
              else
                begin
                  PD^.Size := PD^.Size+SR.FullSize;
                  Inc(PD^.NumFiles);
                end;
            end;
          lFindNext(SR);
        end;
      lFindClose(SR);
      DOSError := 0;
      ClrIO;
    end { DOSReadDirectory };
  {-DataCompBoy-}

  var
    PD: PDirRec;
    RemoteDrive: boolean;
    Info: PView;
    ST: TBufStream;

    {-DataCompBoy-}
  begin { ReadTree }
    {Cat:warn надо добавить вывод дерева для сетевых путей}
    NewTimer(tmr, 1);
    C := UpCase(C);
    if not (C in ['A'..'Z']) then
      exit;
    if DrvTrees[C].C <> nil then
      Dispose(DrvTrees[C].C, Done);
    DrvTrees[C].C := nil;
    TreeError := True;
    if (not CountLen) and ((SystemData.Drives[C] {$IFNDEF OS_DOS}
        shl 1 {$ENDIF}) and ossTree <> 0)
    then
      begin
        ST.Init(SourceDir+'Drive'+C+'.dir', stOpenRead, 1024);
        if ST.Status = stOK then
          begin
            DC := PDirCollection(ST.Get);
            if DC <> nil then
              begin
                DC^.Pack;
                for i := 1 to DC^.Count do
                  PDirRec(DC^.At(i-1))^.Number := i-1;
                DrvTrees[C].C := DC;
                DrvTrees[C].len := False;
                TreeError := False;
                ST.Done;
                exit;
              end;
          end;
        ST.Done;
      end;
    if LowMemory then
      exit;
    New(DC, Init(10, 10));
    New(P);
    FillChar(P^, SizeOf(P^), 0);
    P^.DirName[uLfn] := C+':\';
    P^.Cluster := 0;
    P^.Level := 0;
    P^.Size := -1;
    P^.Attr := 0;
    DC^.Insert(P);
    Abort := False;
    RemoteDrive := False;
    begin
      Info := WriteMsg(GetString(dlScanningDirs));
      DrvTrees[C].C := DC;
      DrvTrees[C].len := True;
DRep:
      ChkESC;
      if not Abort then
        for DCEntry := 1 to DC^.Count do
          begin
            UpdateWriteView(Info);
            P := DC^.At(DCEntry-1);
            if P^.Size < 0 then
              if (not Abort) and (P^.Level < 40) then
                begin
                  DOSReadDirectory(P);
                  goto DRep
                end
              else
                P^.Size := 0;
          end;
      Dispose(Info, Done);
      if Abort then
        begin
          Dispose(DC, Done);
          DrvTrees[C].C := nil;
          TreeError := True;
          exit;
        end;
    end
    ;
    for i := 1 to DC^.Count do
      begin
        P := DC^.At(i-1);
        Lv := i;
        P^.Number := i-1;
        P^.Attr := 0;
        while (Lv < DC^.Count) and (PDirRec(DC^.At(Lv))^.Level > P^.
            Level)
        do
          begin
            PD := DC^.At(Lv);
            Inc(P^.NumFiles, PD^.NumFiles);
            P^.Size := P^.Size+PD^.Size;
            Inc(Lv);
          end;
        if (Lv < DC^.Count) and (PDirRec(DC^.At(Lv))^.Level = P^.
            Level)
        then
          P^.Attr := 1;
        if (i < DC^.Count) and (PDirRec(DC^.At(i))^.Level > P^.Level)
        then
          P^.Attr := P^.Attr or trHasBranch;
      end;
    if ((SystemData.Drives[C] {$IFNDEF OS_DOS} shl 1 {$ENDIF}) and
        ossTree <> 0)
    then
      begin
        ST.Init(SourceDir+'Drive'+C+'.dir', stCreate, 1024);
        ST.Put(DC);
        ST.Done;
      end;
    TreeError := False;
  end { ReadTree };
{-DataCompBoy-}

function GetDirCollection(C: Char; CountLen: boolean): PCollection;
  begin
    GetDirCollection := nil;
    C := UpCase(C);
    if not (C in ['A'..'Z']) then
      exit;
    if (DrvTrees[C].C = nil) or (not DrvTrees[C].len and CountLen)
    then
      ReadTree(C, CountLen);
    GetDirCollection := DrvTrees[C].C;
  end;

{-DataCompBoy-}
function FindDir(DC: PCollection; const Dir: String): integer;
  var
    i, Lv: integer;
    s, D: String;
    P: PDirRec;
  begin
    FindDir := -1;
    if DC = nil then
      exit;
    D := Dir;
    Lv := 1;
    i := 1;
    Delete(D, 1, 3);
    if D = '' then
      begin
        FindDir := 0;
        exit
      end;
    repeat
      s := '';
      while (D[1] <> '\') and (D <> '') do
        begin
          s := s+D[1];
          Delete(D, 1, 1); {DelFC(D)}
        end;
      Delete(D, 1, 1); {DelFC(D);}
      UpStr(s);
      if (s <> '') and (i < DC^.Count) then
        begin
          repeat
            if i < DC^.Count then
              P := DC^.At(i);
            while (i < DC^.Count) and (P^.Level > Lv) do
              begin
                Inc(i);
                if i < DC^.Count then
                  P := DC^.At(i);
              end;
            while (i < DC^.Count) and (P^.Level = Lv)
              and (UpStrg(P^.DirName[uLfn]) <> s)
            do
              begin
                Inc(i);
                if i < DC^.Count then
                  P := DC^.At(i);
              end
          until (i >= DC^.Count) or
          ((UpStrg(P^.DirName[uLfn]) = s)
          and (P^.Level = Lv)) or (P^.Level < Lv)
        end
      else if s = '' then
        i := 1;
      if D <> '' then
        Inc(i);
      Inc(Lv);
    until (D = '') or (i >= DC^.Count) or (Lv > 128) { :) };
    if (i >= DC^.Count) or (UpStrg(P^.DirName[uLfn]) <> s)
    then
      FindDir := -1
    else
      FindDir := i
  end { FindDir };
{-DataCompBoy-}

{-DataCompBoy-}
procedure CreateDirectory;
  var
    dr: String;
    Nm: String;
    XT: String;
    ST: PBufStream;
    DC: PDirCollection;
    i, Lv: integer;
    P, PD: PDirRec;
    s1: String;
  label 1;
  begin
    DC := nil;
    ST := nil;
    if not SaveTrees then
      exit;
    s := lFExpand(s);
    lFSplit(s, dr, Nm, XT);
    if (dr[1] <= 'B') then
      exit;
    if ExistFile(SourceDir+'Drive'+dr[1]+'.DIR') then
      ST := New(PBufStream, Init(SourceDir+'Drive'+dr[1]+'.DIR',
        stOpen, 4096))
    else
      begin
        DOSError := 0; {Cat}
        exit;
      end;
    if ST^.Status = stOK then
      begin
        DC := PDirCollection(ST^.Get);
        if DC <> nil then
          begin
            if Length(dr) > 3 then
              SetLength(dr, Length(dr)-1);
            i := FindDir(DC, s);
            if not Del and (i >= 0) then
              goto 1;
            if not Del then
              i := FindDir(DC, dr);
            if i >= 0 then
              begin
                if Del then
                  begin
                    DrvTrees[dr[1]].len := False;
                    Lv := PDirRec(DC^.At(i))^.Level;
                    DC^.AtFree(i);
                    while (i < DC^.Count) and (PDirRec(DC^.At(i))^.
                        Level > Lv)
                    do
                      DC^.AtFree(i);
                  end
                else
                  begin
                    New(P);
                    FillChar(P^, SizeOf(P^), 0);
                    {$IFNDEF OS2}
                    P^.DirName[False] := GetName(lfGetShortFileName(
                      lFExpand(Nm+XT)));
                    {$ENDIF}
                    s1 := GetName(lFExpand(Nm+XT));
                    Move(s1, P^.DirName[True], Length(s1)+1);
                    P^.NumFiles := 0;
                    P^.Size := 0;
                    P^.Level := PDirRec(DC^.At(i))^.Level+1;
                    DC^.AtInsert(i+1, P);
                  end;
                for i := 1 to DC^.Count do
                  begin
                    P := DC^.At(i-1);
                    Lv := i;
                    P^.Number := i-1;
                    P^.Attr := 0;
                    while (Lv < DC^.Count) and (PDirRec(DC^.At(Lv))^.
                        Level > P^.Level)
                    do
                      begin
                        PD := DC^.At(Lv);
                        Inc(Lv);
                      end;
                    if (Lv < DC^.Count) and (PDirRec(DC^.At(Lv))^.
                        Level = P^.Level)
                    then
                      P^.Attr := 1;
                    if (i < DC^.Count) and (PDirRec(DC^.At(i))^.
                        Level > P^.Level)
                    then
                      P^.Attr := P^.Attr or trHasBranch;
                  end;
                ST^.Seek(0);
                ST^.Truncate;
                ST^.Put(DC);
                Dispose(DC, Done);
                DC := nil;
              end;
          end;
      end;
1:
    if DC <> nil then
      Dispose(DC, Done);
    DC := nil;
    if ST <> nil then
      Dispose(ST, Done);
    ST := nil;
  end { CreateDirectory };
{-DataCompBoy-}

function GetDirLen(Dir: String): TSize; {DataCompBoy}
  var
    DC: PCollection;
    i: integer;
    R: boolean;

  procedure MakeReread(P: PView);
    var
      Event: TEvent;
    begin
      Event.What := evCommand;
      Event.Command := cmRereadTree;
      Event.InfoPtr := @Dir;
      if P <> nil then
        P^.HandleEvent(Event);
    end;

  begin
    GetDirLen := 0;
    Dir[1] := UpCase(Dir[1]);
    R := (DrvTrees[Dir[1]].C <> nil) and not (DrvTrees[Dir[1]].len);
    DC := GetDirCollection(Dir[1], True);
    if DC = nil then
      exit;
    if R then
      Desktop^.ForEach(@MakeReread);
    i := FindDir(DC, Dir);
    if i < 0 then
      exit;
    GetDirLen := PDirRec(DC^.At(i))^.Size;
  end { GetDirLen };

procedure FreeTree(C: Char);
  begin
    C := UpCase(C);
    if not (C in ['A'..'Z']) then
      exit;
    Dispose(DrvTrees[C].C, Done);
    DrvTrees[C].C := nil;
  end;

procedure TTreeReader.HandleEvent;
  var
    C: Char;
    s: String;

  begin
    inherited HandleEvent(Event);

    if (Event.What = evCommand)
    then
      case Event.Command of
        cmRereadTree:
          begin
            s := UpStrg(PString(Event.InfoPtr)^);
            C := s[1];
            if (C in ['A'..'Z']) and (DrvTrees[C].C <> nil) then
              begin
                Dispose(DrvTrees[C].C, Done);
                DrvTrees[C].C := nil;
                GlobalMessage(evCommand, cmFindTree, @C);
                if C = #0 then
                  begin
                    C := s[1];
                    ReadTree(C, DrvTrees[C].len)
                  end
                else
                  DrvTrees[C].len := False;
              end;
          end;
      end {case}; { case }

end { TTreeReader.HandleEvent };

procedure CheckMkDir(const Path: String);
  var
    rc: longInt;
  label Start;
  begin
Start:
    lMkDir(Path);
    rc := IOResult;
    {AK155 29-05-2002
При создании существующего каталога (что не есть ошибка)
под OS/2 rc=5, а под WinNT - rc=183.
При создании каталога на CD (что есть ошибка)
под OS/2 rc=19, а под WinNT - rc=5.
Как оно будет под Win9x или DPMI - тоже еще вопрос.
Поэтому проще и аккуратнее проверить фактическое наличие, а не
анализировать rc }
    if not PathExist(Path) then
      (*  if (rc <> 0) and (rc <> 5 {каталог уже существует}) {$IFDEF Win32} and (rc <> 183) {$ENDIF} then*)
      begin
        if SysErrorFunc(rc, byte(Path[1])-byte('A')) = 1 then
          goto Start;
        rc := MessageBox(GetString(dlFCNoCreateDir)+Path, nil,
          mfError+mfOKButton);
        Abort := True;
      end;
  end;

{-DataCompBoy-}
procedure MakeDirectory;
  var
    s, s1: String;
    dr: String;
    Nm: String;
    XT: String;
    B: byte;
    i: integer;
    j: boolean;
    W: word;
  begin
    CreatedDir := '';
    if LowMemory then
      exit;
    s := '';
    W := ExecResource(dlgMkDir, s);
    if W = cmYes then
      UpStr(s)
    else if W = cmNo then
      LowStr(s)
    else if W <> cmOK then
      exit;
    DelRight(s);
    {$IFDEF OS_DOS}
    if not LFNPresent then
      DelLeft(s);
    {$ENDIF}
    if s = '' then
      exit;
    {$IFDEF RecodeWhenDraw}
    s := OemToCharStr(s);
    {$ENDIF}
    CreatedDir := '';
    while s <> '' do
      begin
        j := False;
        i := 0;
        while (i < Length(s)) do
          begin
            Inc(i);
            if not j and (s[i] = ';') then
              break;
            if s[i] = '"' then
              j := not j;
          end;
        if i = Length(s) then
          Inc(i);
        if i = 0 then
          i := Length(s)+1;
        s1 := DelSquashes(Copy(s, 1, i-1));
        Delete(s, 1, i);
        if s1 = '' then
          continue;
        B := CreateDirInheritance(s1, False);
        if Abort or (IOResult <> 0) then
          exit;
        if CreatedDir = '' then
          CreatedDir := s1;
        lFSplit(s1, dr, Nm, XT);
        if B > 0 then
          SetLength(dr, B)
        else if (Length(dr) > 3) and (dr[Length(dr)] = '\') then
            SetLength(dr, Length(dr)-1);
        RereadDirectory(dr);
        GlobalMessage(evCommand, cmRereadTree, @Dr);
        GlobalMessage(evCommand, cmRereadInfo, nil);
      end;
  end { MakeDirectory };
{-DataCompBoy-}

function ChangeDir;
  var
    D: PTreeDialog;
    s: String;
    R: TRect;
  begin
    R.Assign(1, 1, 50, 18);
    Abort := False;
    ChangeDir := '';
    New(D, Init(R, ATitle, Drv));
    D^.Options := D^.Options or ofCentered;
    s := '';
    D := PTreeDialog(Application^.ValidView(D));
    if D <> nil then
      if Desktop^.ExecView(D) = cmOK then
        D^.GetData(s);
    ChangeDir := s;
  end;

Constructor TTreeInfoView.Init;
  begin
    inherited Init(R);
    Tree := ATree;
    Options := Options or ofPostProcess;
    EventMask := evBroadcast;
    GrowMode := gfGrowHiX+gfGrowHiY+gfGrowLoY;
    MakeDown;
    Loaded := False;
  end;

procedure TTreeInfoView.HandleEvent;
  begin
    inherited HandleEvent(Event);
    if (Event.What = evBroadcast) and (Event.Command = cmDirChanged)
    then
      begin
        MakeDown;
        DrawView
      end;
  end;

Constructor TTreeInfoView.Load;
  begin
    inherited Load(s);
    GetPeerViewPtr(s, Tree);
    Loaded := True;
  end;

procedure TTreeInfoView.Store;
  begin
    inherited Store(s);
    PutPeerViewPtr(s, Tree);
  end;

function TTreeInfoView.GetPalette;
  const
    s: String[Length(CTreeInfoView)] = CTreeInfoView;
  begin
    GetPalette := @S;
  end;

function TDTreeInfoView.GetPalette;
  const
    s: String[Length(CDTreeInfoView)] = CDTreeInfoView;
  begin
    GetPalette := @S;
  end;

procedure TTreeInfoView.Draw;
  var
    B: TDrawBuffer;
    C: byte;
  begin
    C := GetColor(1);
    if Loaded then
      MakeDown;
    Loaded := False;
    MoveChar(B, ' ', C, Size.X);
    MoveStr(B[1], Cut(Tree^.CurPath, Size.X), C);
    WriteLine(0, 0, Size.X, 1, B);
    MoveChar(B, ' ', C, Size.X);
    MoveStr(B[1], Down, C);
    WriteLine(0, 1, Size.X, 1, B);
  end;

procedure TTreeInfoView.MakeDown;
  {var L: Array [1..5] of Longint;}
  var
    l1: longInt;
    L2: TSize;
  begin
    {L[1] := Tree^.CurPtr^.NumFiles;}
    {L[2] := Tree^.CurPtr^.Size;}
    if (Tree <> nil) and (Tree^.CurPtr <> nil) then
      begin
        l1 := Tree^.CurPtr^.NumFiles;
        L2 := Tree^.CurPtr^.Size;
        if l1 <> 1 then
          Down := ItoS(l1)+GetString(dlTreeFilesWith)
        else
          Down := GetString(dlTree1FileWith);
        if L2 <> 1 then
          Down := Down+FStr(L2)+' '+GetString(dlDIBytes)
        else
          Down := Down+' 1'+GetString(dlDIByte);
      end;
  end { TTreeInfoView.MakeDown };

Constructor TTreeDialog.Init;
  var
    R1, R2: TRect;
    P: PView;
  begin
    inherited Init(R, ATitle);
    HelpCtx := hcTreeDialog;
    isValid := True;
    if R.B.X-R.A.X < 24 then
      R.Grow(24+R.A.X-R.B.X, 0);
    if R.B.Y-R.A.Y < 8 then
      R.B.Y := R.A.Y+8;
    GetExtent(R);
    R.Grow(-1, -1);
    R1 := R;
    Dec(R1.B.X, 14);
    P := StandardScrollBar(sbVertical+sbHandleKeyboard);
    Dec(P^.Origin.X, 14);
    Dec(R1.B.Y);
    Tree := New(PTreeView, Init(R1, ADrive, False, PScrollBar(P)));
    if Tree^.Valid(0) then
      Insert(Tree)
    else
      begin
        Dispose(Tree, Done);
        Tree := nil;
        isValid := False;
        exit;
      end;

    R1.A.Y := R1.B.Y;
    Inc(R1.B.Y);
    P := New(PDTreeInfoView, Init(R1, PTreeView(Tree)));
    Insert(P);

    R1.Assign(R.B.X-13, R.A.Y+1, R.B.X-1, R.A.Y+3);
    P := New(PButton, Init(R1, GetString(dlOKButton), cmOK,
      bfDefault));
    Insert(P);
    R1.Assign(R.B.X-13, R.A.Y+4, R.B.X-1, R.A.Y+6);
    P := New(PButton, Init(R1, GetString(dlDriveButton),
      cmChangeDrive, bfBroadcast));
    {P^.Options := P^.Options and not ofSelectable;}
    Insert(P);
    R1.Assign(R.B.X-13, R.A.Y+7, R.B.X-1, R.A.Y+9);
    P := New(PButton, Init(R1, GetString(dlRereadButton),
      cmPanelReread, bfBroadcast));
    Insert(P);
    R1.Assign(R.B.X-13, R.A.Y+10, R.B.X-1, R.A.Y+12);
    P := New(PButton, Init(R1, GetString(dlMkDirButton),
      cmPanelMkDir, bfBroadcast));
    Insert(P);
    R1.Assign(R.B.X-13, R.A.Y+13, R.B.X-1, R.A.Y+15);
    P := New(PButton, Init(R1, GetString(dlCancelButton), cmCancel, 0));
    Insert(P);
    SelectNext(False);
    Options := Options or ofTopSelect;
  end { TTreeDialog.Init };

function TTreeDialog.GetPalette;
  const
    s: String[Length(CTreeDialog)] = CTreeDialog;
  begin
    GetPalette := @S;
  end;

function TTreeDialog.Valid;
  begin
    Valid := isValid and inherited Valid(Command)
  end;

(*procedure TTreeDialog.HandleEvent;
begin
 {if Event.What = evCommand then Tree^.HandleEvent(Event);}
 inherited HandleEvent(Event);
end;*)

Constructor TTreeWindow.Init;
  var
    R: TRect;
    P: PView;
    s: PScrollBar;
  begin
    inherited Init(Bounds, 'Directory Tree', 0);
    GetExtent(R);
    R.Grow(-1, -1);
    R.A.X := R.B.X;
    Inc(R.B.X);
    s := StandardScrollBar(sbVertical+sbHandleKeyboard);
    GetExtent(R);
    R.Grow(-1, -1);
    Dec(R.B.Y, 2);
    P := New(PTreePanel, Init(R, 0, True, s));
    Insert(P);

    GetExtent(R);
    R.Grow(-1, -1);
    R.A.Y := R.B.Y-2;
    P := New(PTreeInfoView, Init(R, PTreeView(P)));
    Insert(P);
  end { TTreeWindow.Init };

Constructor TTreeWindow.Load;
  begin
    inherited Load(s);
    PTreeView(Current)^.ReadAfterLoad;
  end;

procedure TTreeWindow.Store;
  begin
    inherited Store(s);
  end;

function TTreeWindow.GetPalette;
  const
    s: String[Length(CTreeDialog)] = CTreeDialog;
  begin
    GetPalette := @S;
  end;

procedure TTreeWindow.HandleEvent(var Event: TEvent);
  begin
    case Event.What of
      evKeyDown:
        if (Event.KeyCode = kbESC) then
          begin
            Message(Application, evCommand, cmClose, nil);
            ClearEvent(Event);
          end;
      evCommand:
        case Event.Command of
          cmGetName:
            PString(Event.InfoPtr)^:= 'Directory tree';
        end {case};
    end {case};
    inherited HandleEvent(Event);
  end;

Constructor TTreeView.Init;
  var
    i, Lv: integer;
    s, D: String;
    P: PDirRec;
  begin
    inherited Init(R);
    Abort := False;
    if ParitalView then
      HelpCtx := hcDirTree;
    EventMask := $FFFF;
    lGetDir(ADrive, CurPath); {DataCompBoy}
    ScrollBar := ScrBar;
    Drive := ADrive;
    LastPath := CurPath;
    Options := Options or ofSelectable {or ofTopSelect} or
      ofFirstClick;
    Parital := ParitalView;
    if Parital then
      Options := Options or ofTopSelect;
    GrowMode := gfGrowHiX+gfGrowHiY;
    WasChanged := False;
    Dirs := nil;
    DC := nil;
    MouseTracking := False;
    LocateEnabled := True;
    if not Abort then
      ReadTree(False);
    QuickSearch := False;
    DrawDisabled := False;
    isValid := not Abort and (ScrollBar <> nil);
  end { TTreeView.Init };

destructor TTreeView.Done;
  begin
    if DC <> nil then
      begin
        DC^.DeleteAll;
        Dispose(DC, Done);
        DC := nil;
      end;
    inherited Done;
  end;

function TTreeView.GetPalette;
  const
    s: String[Length(CTreeView)] = CTreeView;
  begin
    GetPalette := @S;
  end;

Constructor TTreeView.Load;
  var
    P: Pointer;
  begin
    inherited Load(s);
    GetPeerViewPtr(s, ScrollBar);
    s.Read(Parital, 1);
    s.ReadStrV(LastPath);
      {S.Read(LastPath[0], 1); S.Read(LastPath[1], Length(LastPath));}
    CurPath := LastPath;
    Drive := byte(LastPath[1])-64;
    QuickSearch := False;
    DrawDisabled := False;
    LocateEnabled := True;
    Dirs := nil;
    DC := nil;
    isValid := True;
    WasChanged := False;
    MouseTracking := False;
  end;

procedure TTreeView.ReadAfterLoad;
  begin
    Abort := False;
    ReadTree(False);
    isValid := not Abort and (ScrollBar <> nil);
  end;

function THTreeView.GetPalette;
  const
    s: String[Length(CHTreeView)] = CHTreeView;
  begin
    GetPalette := @S;
  end;

procedure TTreeView.Store;
  begin
    inherited Store(s);
    PutPeerViewPtr(s, ScrollBar);
    s.Write(Parital, 1);
    s.WriteStr(@LastPath); {S.Write(LastPath, Length(LastPath)+1);}
  end;

function TTreeView.Valid;
  begin
    Valid := isValid and inherited Valid(Command)
  end;

procedure TTreeView.SetData;
  begin
  end;

{-DataCompBoy-}
procedure TTreeView.GetData;
  var
    s: String;
  begin
    s := GetDirName(ScrollBar^.Value);
    String(Rec) := s;
    LastPath := s;
  end;
{-DataCompBoy-}

function TTreeView.GetDirName;
  var
    s: String;
    i: integer;
    P: PDirRec;
  begin
    i := n;
    P := DC^.At(i);
    if i = 0 then
      s := ''
    else
      s := P^.DirName[uLfn];
    repeat
      if i > 0 then
        repeat
          Dec(i)
        until (i < 0) or (PDirRec(DC^.At(i))^.Level < P^.Level);
      if i >= 0 then
        P := DC^.At(i);
      s := MakeNormName(P^.DirName[uLfn], s); {DataCompBoy}
    until i <= 0;
    GetDirName := s;
  end { TTreeView.GetDirName };

function TTreeView.DataSize;
  begin
    DataSize := 256;
  end;

procedure TTreeView.CollapseBranch;
  var
    l, i: integer;
    P, P1: PDirRec;

  function Find(n: integer): integer;
    var
      i: integer;
    begin
      Find := -1;
      for i := 1 to DC^.Count do
        if PDirRec(DC^.At(i-1))^.Number = n then
          begin
            Find := i-1;
            exit
          end;
    end;

  begin
    i := Find(n);
    if (i <= 0) or not Parital then
      exit;
    P := DC^.At(i);
    if Expanded(P, i+1) then
      begin
        while (i < DC^.Count-1) and (P^.Level < PDirRec(DC^.At(i+1))^.
            Level)
        do
          DC^.AtDelete(i+1);
      end
    else
      begin
        l := n+1;
        while (l < Dirs^.Count) and (PDirRec(Dirs^.At(l))^.Level > P^.
            Level)
        do
          begin
            if PDirRec(Dirs^.At(l))^.Level = P^.Level+1 then
              begin
                DC^.AtInsert(i+1, Dirs^.At(l));
                Inc(i);
              end;
            Inc(l);
          end;
      end;
    ScrollBar^.SetParams(ScrollBar^.Value, 0, DC^.Count-1, DC^.Count, 1)
      ;
  end { TTreeView.CollapseBranch };

procedure TTreeView.HandleEvent;
  begin
    if Valid(0) then
      begin
        inherited HandleEvent(Event);
        HandleCommand(Event);
        if not Valid(0) then
          Message(Owner, evCommand, cmCancel, nil)
      end;
  end;

{AK155}
function MkFcFromDirRec(D: PDirRec; var FullName: String):
    PFilesCollection;
  var
    l: longInt;
    fr: PFileRec;
  begin
    l := Length(FullName);
    while FullName[l] <> '\' do
      Dec(l);
    SetLength(FullName, l-1);
    New(Result, Init(1, 1));
    l := Length(D^.DirName[True]);
    GetMem(fr, TFileRecFixedSize+l);
    with fr^ do
      begin
        Move(D^.DirName, FlName, SizeOf(FlName[True])+1+l);
        Attr := Directory;
        Owner := @FullName;
        DIZ := nil;
      end;
    Result^.Insert(fr);
  end { MkFcFromDirRec };

procedure TTreeView.HandleCommand;
  label NoLoc;
  var
    CurPos, i: integer;
    PD: PDirRec;
    C: Char;
    dr: String;
    MP: TPoint;

  procedure CE;
    begin
      ClearEvent(Event)
    end;

  function SearchForMask(Delta: integer): boolean;
    var
      i: integer;
    begin
      SearchForMask := False;
      if SearchParam.Mask[1] = '\' then
        begin
          ScrollBar^.SetValue(0);
          SearchForMask := True;
          InitQuickSearch(SearchParam);
          exit;
        end;
      i := ScrollBar^.Value+Delta;
      if i = 0 then
        Inc(i);
      while (i < DC^.Count) and
        not {InFilter}InQSMask(PDirRec(DC^.At(i))^.DirName[uLfn],
          SearchParam.Mask)
      do
        Inc(i); {DataCompBoy}
      if i >= DC^.Count then
        begin
          i := 0;
          while (i < ScrollBar^.Value) and
            not {InFilter}InQSMask(PDirRec(DC^.At(i))^.DirName[uLfn],
              SearchParam.Mask)
          do
            Inc(i); {DataCompBoy}
          if i >= ScrollBar^.Value then
            i := 0;
        end;
      if i > 0 then
        begin
          SearchForMask := True;
          if i = ScrollBar^.Value then
            DrawView
          else
            ScrollBar^.SetValue(i);
        end;
    end { SearchForMask };

  procedure CancelSearch;
    begin
      if not QuickSearch then
        exit;
      QuickSearch := False;
      DrawView;
    end;

  procedure ChangeDrive;
    var
      t: TPoint;
      s: String;
    begin
      t.X := Size.X div 2;
      t.Y := 1;
      MakeGlobal(t, t);
      Desktop^.MakeLocal(t, t);
      s := SelectDrive(t.X, t.Y, CurPath[1], False);
      if s = '' then
        exit;
      ClrIO;
      lGetDir(byte(s[1])-64, s);
      if Abort then
        exit; {DataCompBoy}
      CurPath := s;
      LastPath := s;
      if not Abort then
        begin
          ReadTree(False);
          if isValid then
            DrawView;
        end;
    end { ChangeDrive };

  procedure SendLocated;
    begin
      if LocateEnabled then
        Message(Owner, evCommand, cmChangeDirectory, @CurPath);
    end;

  procedure ExpandBranches;
    var
      B: boolean;
      i: integer;
      s: String;
      P: PDirRec;
    begin
      B := True;
      GetData(s);
      for i := 1 to DC^.Count do
        begin
          P := DC^.At(i-1);
          B := B and Expanded(P, i);
        end;
      if not B then
        begin
          DC^.DeleteAll;
          for i := 1 to Dirs^.Count do
            DC^.Insert(Dirs^.At(i-1));
        end
      else
        Reread(False);
      if Valid(0) then
        begin
          i := FindDir(s);
          if i < 0 then
            i := 0;
          ScrollBar^.SetParams(i, 0, DC^.Count-1, DC^.Count, 1);
          DrawView;
        end;
    end { ExpandBranches };

  {-DataCompBoy-}
  procedure MkDirectory;
    var
      OldDir, NewDir: String;
    begin
      lGetDir(0, OldDir);
      GetData(NewDir);
      lChDir(NewDir);
      MakeDirectory;
      lChDir(OldDir);
      GlobalMessage(evCommand, cmRereadInfo, nil);
    end;
  {-DataCompBoy-}

  {-DataCompBoy-}
  procedure EraseDir;
    var
      FC: PFilesCollection;
      D: PDirRec;
      s: String;
      OldDir: String;
    begin
      CE;
      if ScrollBar^.Value < 1 then
        exit;
      GetData(s);
      ClrIO;
      lGetDir(0, OldDir);
      if Abort then
        exit;
      lChDir(GetPath(s));
      D := DC^.At(ScrollBar^.Value);
      FC := MkFcFromDirRec(D, s);
      EraseFiles(FC);
      Dispose(FC, Done);
      lChDir(OldDir);
      ClrIO;
    end { EraseDir };
  {-DataCompBoy-}

  procedure QuickChange(const SS: String);
    begin
      if SS <> '' then
        Message(@Self, evCommand, cmChangeTree, @SS);
    end;

  var
    EV: TEvent;

  begin { TTreeView.HandleCommand }
    CurPos := ScrollBar^.Value;
    {P := DC^.At(CurPos);}
    if QuickSearch and (Event.What = evKeyDown) and (Event.CharCode <
        #32)
      and (Event.KeyCode <> kbBack) and (Event.KeyCode <>
        kbCtrlEnter)
    then
      CancelSearch;
    case Event.What of
      evCommand:
        case Event.Command of

          cmPanelErase:
            EraseDir;
          cmDoSendLocated:
            begin
              SendLocated;
              CE
            end;
          cmPanelMkDir:
            begin
              MkDirectory;
              CE
            end;
          cmChangeDrive:
            begin
              ChangeDrive;
              CE;
            end;
          cmFindTree:
            if Char(Event.InfoPtr^) = LastPath[1] then
              Char(Event.InfoPtr^) := #0;
          cmRevert:
            begin
              ScrollBar^.SetValue(CurNum);
              CurPtr := DC^.At(ScrollBar^.Value);
              Message(Owner, evBroadcast, cmDirChanged, @CurPath);
              CE
            end;
          cmRereadTree:
            begin
              if Dirs = nil then
                exit;
              if CurPath[1] = PString(Event.InfoPtr)^[1] then
                Reread(False);
              if Valid(0) then
                begin
                  CurPtr := DC^.At(ScrollBar^.Value);
                  Message(Owner, evBroadcast, cmDirChanged, @CurPath);
                end;
            end;
          cmPanelReread, cmRereadForced, cmForceRescan
          :
            begin
              Reread(True);
              if Valid(0) then
                begin
                  CurPtr := DC^.At(ScrollBar^.Value);
                  Message(Owner, evBroadcast, cmDirChanged, @CurPath);
                  Message(Application, evBroadcast, cmTreeChanged,
                    @CurPath);
                end;
            end;
          cmChangeTree:
            begin
              if PString(Event.InfoPtr)^[2] <> ':' then
                {Cat:warn могут быть проблемы с сетевыми путями}
                begin
                  CE;
                  exit;
                end;
              CurPath[1] := UpCase(CurPath[1]);
              LastPath[1] := UpCase(LastPath[1]);
              LocateEnabled := True;
              LastPath := PString(Event.InfoPtr)^;
              if Dirs = nil then
                begin
                  WasChanged := CurPath[1] <> LastPath[1];
                  CurPath := LastPath;
                  exit;
                end;
              LocateEnabled := False;
              if ((LastPath[1] <> CurPath[1]) or WasChanged)
                and (ValidDrive(LastPath[1]))
              then
                begin
                  LocateEnabled := False;
                  WasChanged := False;
                  CurPath := LastPath;
                  Reread(False);
                  if DC = nil then
                    begin
                      CE;
                      exit;
                    end; {JO}
                  if Valid(0) then
                    begin
                      if Dirs = nil then
                        exit;
                      CurPtr := DC^.At(ScrollBar^.Value);
                      Message(Owner, evBroadcast, cmDirChanged,
                        @CurPath);
                      LocateEnabled := True;
                      CE;
                      exit;
                    end;
                end;
              CurPath := LastPath;
              CurNum := Tree.FindDir(Dirs, CurPath);
              i := FindDir(CurPath);
              if i >= 0 then
                ScrollBar^.SetValue(i);
              LocateEnabled := True;
              CE;
              DrawView;
              CurPtr := DC^.At(ScrollBar^.Value);
              Message(Owner, evBroadcast, cmDirChanged, @CurPath);
            end;
        end {case};
      evKeyDown:
        case Event.KeyCode of
          kbAlt1, kbAlt2, kbAlt3, kbAlt4, kbAlt5, kbAlt6, kbAlt7,
            kbAlt8, kbAlt9:
            begin
              QuickChange(CnvString(DirsToChange[Event.ScanCode-Hi(
                kbAlt1)]));
              CE
            end;
          kbEnter:
            if Parital then
              begin
                SendLocated;
                CE
              end;
          kbDel:
            if (FMSetup.Options and fmoDelErase <> 0) then
              EraseDir;
          kbGrayAst:
            begin
              if Parital then
                ExpandBranches;
              CE;
            end;
          kbSpace, kbGrayPlus, kbGrayMinus:
            begin
              QuickSearch := False;
              if Parital then
                begin
                  CollapseBranch(PDirRec(DC^.At(CurPos))^.Number);
                  DrawView;
                end;
              CE;
            end;
          kbRight:
            Event.KeyCode := kbDown;
          kbLeft:
            Event.KeyCode := kbUp;
          kbCtrlEnter:
            begin
              if QuickSearch then
                begin
                  SearchForMask(1);
                  CE;
                end;
              exit;
            end;
          {-DataCompBoy-}
          kbBack:
            if QuickSearch then
              if SearchParam.Mask = '' then
                CancelSearch
              else
                begin
                  DoQuickSearch(SearchParam, Event.KeyCode);
                  ScrollBar^.SetValue(0);
                  SearchForMask(0);
                  DrawView;
                  CE
                end;
          else
            if Event.CharCode >= #32 then
              begin
                if QuickSearch then
                  begin
                    if (Event.CharCode = '\') and (SearchParam.Mask <>
                        '')
                    then
                      begin
                        CE;
                        PD := DC^.At(CurPos);
                        while (PD^.Attr and trHasBranch = 0) do
                          begin
                            if not SearchForMask(1) then
                              exit;
                            PD := DC^.At(ScrollBar^.Value);
                          end;
                        ScrollBar^.SetValue(ScrollBar^.Value+1);
                        if Parital and (PD^.Attr and trExpanded = 0)
                        then
                          CollapseBranch(PDirRec(DC^.At(CurPos))^.
                            Number);
                        InitQuickSearch(SearchParam);
                        DrawView;
                        exit;
                      end;
                    DoQuickSearch(SearchParam, Event.KeyCode);
                    if not SearchForMask(0) then
                      DoQuickSearch(SearchParam, kbBack);
                  end
                else
                  begin
                    QuickSearch := True;
                    InitQuickSearch(SearchParam);
                    DoQuickSearch(SearchParam, Event.KeyCode);
                    SearchForMask(0);
                    if SearchParam.Mask = '\' then
                      InitQuickSearch(SearchParam);
                  end;
                CE;
              end;
        end {case};
      {-DataCompBoy-}
      evBroadcast:
        case Event.Command of
          cmPanelReread,
          cmPanelMkDir,
          cmChangeDrive:
            Message(@Self, evCommand, Event.Command, nil);
          cmTreeChanged:
            begin
              if (Dirs <> nil) and (PString(Event.InfoPtr)^[1] =
                  CurPath[1])
              then
                Reread(False);
            end;
          cmDropped:
            begin
              MP := PCopyRec(Event.InfoPtr)^.Where;
              if not MouseInView(MP) then
                begin
                  CE;
                  exit;
                end;
              MakeLocal(MP, MP);
              i := Delta.Y+MP.Y;
              if i >= DC^.Count then
                begin
                  CE;
                  exit;
                end;
              CopyDirName := GetDirName(i);
              if PCopyRec(Event.InfoPtr)^.Owner <> nil then
                begin
                  EV.What := evBroadcast;
                  EV.Command := cmUnArchive;
                  EV.InfoPtr := Event.InfoPtr;
                  PCopyRec(Event.InfoPtr)^.Owner^.HandleEvent(EV);
                  if EV.What = evNothing then
                    begin
                      CE;
                      exit;
                    end;
                end;
              if ReflectCopyDirection
              then
                RevertBar := not (Message(Desktop, evBroadcast,
                  cmIsRightPanel, @Self) <> nil)
              else
                RevertBar := False;
              CopyFiles(PCopyRec(Event.InfoPtr)^.FC, PCopyRec(Event.
                InfoPtr)^.Owner,
              ShiftState and 3 <> 0, 0);
              CE;
            end;
          cmScrollBarChanged:
            if ScrollBar = Event.InfoPtr then
              begin
                DrawView;
                CE;
                GetData(CurPath);
                CurPtr := DC^.At(CurPos);
                Message(Owner, evBroadcast, cmDirChanged, @CurPath);
                if not MouseTracking and (FMSetup.Options and
                    fmoAutoChangeDir <> 0)
                then
                  NeedLocated := GetSTime;
              end;
        end {case};
      evMouseDown:
        begin
          MakeLocal(Event.Where, MP);
          if MP.Y+Delta.Y < DC^.Count then
            begin
              PD := DC^.At(MP.Y+Delta.Y);
              if (PD^.Level > 0) and
                (MP.X >= PD^.Level*3+1+Delta.X) and
                (MP.X <= PD^.Level*3+3+Delta.X)
              then
                begin
                  ScrollBar^.SetValue(MP.Y+Delta.Y);
                  Message(@Self, evKeyDown, $3920, nil);
                  while MouseEvent(Event, evMouseAuto+evMouseMove) do
                    ;
                end
              else
                {if (MP.X >= P^.Level*3 + 1 + 3*Byte(P^.Level>0) + Delta.X) and
                      (MP.X <= P^.Level*3 + 2 + 3*Byte(P^.Level>0) + Delta.X + Length(P^.Name)) then}
                begin
                  if Event.Double and not Parital then
                    begin
                      ScrollBar^.SetValue(MP.Y+Delta.Y);
                      Message(Owner, evCommand, cmOK, nil);
                      CE
                    end;
                  CurPos := RepeatDelay;
                  RepeatDelay := 0;
                  MouseTracking := True;
                  repeat
                    MakeLocal(Event.Where, MP);
                    if (MP.X > 0) and (MP.X < Size.X) then
                      ScrollBar^.SetValue(MP.Y+Delta.Y);
                  until not MouseEvent(Event, evMouseAuto+
                    evMouseMove);
                  MouseTracking := False;
                  SendLocated;
                  RepeatDelay := CurPos;
                end;
              CE;
            end;
        end;
    end {case};
  end { TTreeView.HandleCommand };

function TTreeView.Expanded(P: PDirRec; i: integer): boolean;
  begin
    Expanded := True;
    if (i >= DC^.Count) then
      begin
        if (P^.Number+1 <> Dirs^.Count) then
          Expanded := False;
      end
    else if PDirRec(DC^.At(i))^.Number <> P^.Number+1 then
      Expanded := False;
  end;

procedure TTreeView.Draw;
  var
    Levels: array[0..255] of boolean;
    i, j, k, CurPos, Idx: integer;
    C: Char;
    s, Q: String;
    B: TDrawBuffer;
    C1, C2, C3, C4, CC: byte;
    P: PDirRec;
  begin
    if (DrawDisabled) or (DC = nil) then
      exit;
    FillChar(Levels, 255, 0);
    C1 := GetColor(1);
    C2 := GetColor(2);
    C3 := GetColor(3);
    C4 := GetColor(4);
    ScrollBar^.PgStep := Size.Y*((Size.X+1) div 13);
    if Owner^.GetState(sfActive) and GetState(sfSelected) then
      C3 := GetColor(3)
    else
      C3 := GetColor(6);
    CurPos := ScrollBar^.Value;
    if CurPos < Delta.Y then
      Delta.Y := CurPos;
    if CurPos >= Delta.Y+Size.Y then
      Delta.Y := CurPos-Size.Y+1;
    P := DC^.At(CurPos);
    if (P^.Number = CurNum) and not Parital then
      if Owner^.GetState(sfActive) and GetState(sfSelected) then
        C3 := GetColor(5)
      else
        C3 := GetColor(7);
    if P^.Level*3+6+Length(P^.DirName[uLfn])+Delta.X > Size.X then
      Delta.X := P^.Level*3+6+Length(P^.DirName[uLfn])-Size.X;
    if P^.Level*3-4 < Delta.X then
      Delta.X := P^.Level*3-4;
    if Delta.X < 0 then
      Delta.X := 0;
    for i := 0 to Delta.Y-1 do
      begin
        P := DC^.At(i);
        Levels[P^.Level] := (P^.Attr and 1 = 1);
      end;
    for i := 1 to Size.Y do
      begin
        MoveChar(B, ' ', C1, 200);
        Idx := i+Delta.Y-1;
        if Idx < DC^.Count then
          begin
            P := DC^.At(Idx);
            if P^.Attr and 1 = 1 then
              C := #195
            else
              C := #192;
            if P^.Level = 0 then
              s := ''
            else
              begin
                {if Parital then S := C + #196'[ ] ' else}s := C+
                  #196#196#196#196' ';
                if (P^.Attr and trHasBranch <> 0) and
                  (PDirRec(Dirs^.At(P^.Number+1))^.Level > P^.Level)
                then
                  if Parital then
                    if Expanded(P, Idx+1) then
                      s := C+#196'[-] '
                    else
                      s := C+#196'[+] '
                  else
                    s[4] := #194;
              end;
            k := 2;
            if P^.Level > 0 then
              for j := 1 to P^.Level-1 do
                begin
                  if Levels[j] then
                    MoveChar(B[k], #179, C1, 1);
                  Inc(k, 3);
                end;
            Levels[P^.Level] := (P^.Attr and 1 = 1);
            MoveStr(B[k], s, C1);
            if Idx = CurPos then
              CC := C3
            else if (P^.Number = CurNum) and not Parital then
              CC := C4
            else
              CC := C2;
            {-DataCompBoy-}
            Q := P^.DirName[uLfn];
            if CurPos = Idx then
              begin
                MoveStr(B[k+Length(s)], {' '#0+}Q {+#0' '}, CC);
                  {JO: зачем нужна была эта оторочка имён нулями - тайна,}
                if QuickSearch then
                    {    покрытая мраком, но из-за неё каталоги в панели}
                  begin
                      {    оторочены мерзкими точечками или чем ещё похуже}
                    ShowCursor;
                    NormalCursor;
                    SetCursor(k+Length(s)+GetCursorPos(SearchParam, Q,
                      Size.X, 0)-1, i-1)
                  end
                else
                  HideCursor;
              end
            else
              begin{q:=#0+q+#0;}
                MoveStr(B[k+Length(s)], Q, CC)
              end;
            {-DataCompBoy-}
          end;
        WriteLine(0, i-1, Size.X, 1, B[Delta.X]);
      end;
  end { TTreeView.Draw };

{-DataCompBoy-}
procedure TDirCollection.FreeItem;
  begin
    Dispose(PDirRec(P));
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDirCollection.PutItem;
  begin
    s.Write(Item^, SizeOf(TDirRec));
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TDirCollection.GetItem;
  var
    Item: PDirRec;
  begin
    New(Item);
    s.Read(Item^, SizeOf(TDirRec));
    if (Item^.NumFiles < 0) or (Item^.Size < 0) then
      begin
        Item^.NumFiles := 0;
        Item^.NumFiles := 0;
      end;
    GetItem := Item;
  end;
{-DataCompBoy-}

procedure TTreeView.ReadTree;
  label Rep;
  var
    P: PDirRec;
    DCEntry: integer;
    Idx: integer;
    D, s: String; {DataCompBoy}
    Lv, i: integer;
    dr: array[1..255] of integer;
    PD: PDirRec;
    iLFN: TUseLFN;
  begin
    Abort := False;
    if DC <> nil then
      begin
        DC^.DeleteAll;
        Dispose(DC, Done);
      end;
    DC := GetDirCollection(CurPath[1], CountLen);
    isValid := not Abort;
    if (DC = nil) then
      begin
        Abort := True;
        isValid := False;
        exit
      end;
    if DC^.Count = 0 then
      begin
        New(PD);
        PD^.Cluster := 0;
        PD^.Level := 0;
        PD^.Size := -1;
        PD^.Attr := 0;
        s := Copy(CurPath, 1, 3);
        for iLFN := Low(TUseLFN) to High(TUseLFN) do
          CopyShortString(s, PD^.DirName[iLFN]);
        DC^.Insert(PD);
      end;
    if LowMemory then
      begin
        Abort := True;
        exit
      end;
    if not Abort and (ScrollBar <> nil) and (DC^.Count > 0) then
      begin
        lGetDir(byte(CurPath[1])-64, CurPath);
        if Abort then
          exit; {DataCompBoy}
        D := CurPath;
        Delete(D, 1, 3);
        Dirs := DC;
        DC := New(PDirCollection, Init(Dirs^.Count, 10));
        for i := 1 to Dirs^.Count do
          DC^.Insert(Dirs^.At(i-1));
        DrawDisabled := True;
        if Parital then
          begin
            i := 1;
            while (i < DC^.Count) do
              begin
                CollapseBranch(PDirRec(DC^.At(i))^.Number);
                Inc(i);
              end;
          end;
        DrawDisabled := False;
        i := FindDir(CurPath);
        CurNum := i;
        Delta.Y := 0;
        Delta.X := 0;
        Lv := CurNum;
        ScrollBar^.SetParams(Lv, 0, DC^.Count-1, DC^.Count, 1);
        Lv := ScrollBar^.Value;
        if Lv < 0 then
          Lv := 0;
        CurPtr := DC^.At(Lv);
      end
    else
      isValid := False;
    isValid := not Abort;
  end { TTreeView.ReadTree };

function TTreeView.FindDir;
  var
    n, i: integer;

  function Find(n: integer): integer;
    var
      i: integer;
    begin
      Find := -1;
      for i := 1 to DC^.Count do
        if PDirRec(DC^.At(i-1))^.Number = n then
          begin
            Find := i-1;
            exit
          end;
    end;

  procedure ExpandFor(n: integer);
    var
      i, Lv, CurLv: integer;
      Lvs: array[1..255] of integer;
    begin
      i := n-1;
      CurLv := PDirRec(Dirs^.At(n))^.Level;
      Lvs[CurLv] := n;
      Lv := CurLv;
      repeat
        Dec(Lv);
        while (i >= 0) and (PDirRec(Dirs^.At(i))^.Level > Lv) do
          Dec(i);
        Lvs[Lv] := i;
      until (i = 0) or (Find(i) >= 0);
      for i := Lv to CurLv-1 do
        CollapseBranch(Lvs[i]);
    end;

  begin { TTreeView.FindDir }
    n := Tree.FindDir(Dirs, Dir);
    if Parital and (n >= 0) then
      begin
        i := Find(n);
        FindDir := i;
        if i >= 0 then
          exit;
        ExpandFor(n);
        FindDir := Find(n);
      end
    else
      FindDir := n;
  end { TTreeView.FindDir };

procedure TTreeView.SetState;
  begin
    inherited SetState(AState, Enable);
    if (AState and sfFocused <> 0) and not Enable then
      if Parital then
        DisableCommands([cmCopyFiles, cmPanelErase, cmMoveFiles,
          cmPanelMkDir,
        cmChangeDrive, cmPanelReread]);
    if AState and (sfFocused or sfActive or sfSelected) <> 0 then
      if Owner^.GetState(sfActive) and GetState(sfSelected) then
        begin
          if ScrollBar <> nil then
            ScrollBar^.Show;
          if Parital then
            EnableCommands([cmCopyFiles, cmPanelErase, cmMoveFiles,
              cmPanelReread,
            cmPanelMkDir, cmChangeDrive]);
          {EventMask := EventMask or evBroadcast;}
          DrawView
        end
      else
        begin
          if ScrollBar <> nil then
            ScrollBar^.Hide;
          {EventMask := EventMask and (not evBroadcast);}
          DrawView
        end;
  end { TTreeView.SetState };

procedure TTreeView.Reread;
  var
    s: String;
    i, M: integer;
  begin
    DrawDisabled := True;
    DC^.DeleteAll;
    Dispose(DC, Done);
    DC := nil;
    M := ScrollBar^.Value;
    LocateEnabled := False;
    ReadTree(CountLen);
    if Valid(0) then
      begin
        i := FindDir(LastPath);
        if i < 0 then
          i := M;
        LocateEnabled := True;
        ScrollBar^.SetValue(i);
        DrawDisabled := False;
        DrawView;
      end;
  end { TTreeView.Reread };

procedure TTreePanel.HandleEvent;
  procedure CE;
    begin
      ClearEvent(Event)
    end;

  {-DataCompBoy-}
  procedure CopyDir;
    var
      FC: PFilesCollection;
      D: PDirRec;
      s: String;
      OldDir, NewDir: String;
    begin
      lGetDir(0, OldDir);
      GetData(NewDir);
      lChDir(MakeNormName(NewDir, '..'));
      CE;
      if ScrollBar^.Value < 1 then
        exit;
      GetData(s);
      D := DC^.At(ScrollBar^.Value);
      FC := MkFcFromDirRec(D, s);
      if ReflectCopyDirection
      then
        RevertBar := (Message(Desktop, evBroadcast, cmIsRightPanel,
          @Self) <> nil)
      else
        RevertBar := False;
      CopyFiles(FC, nil, Event.Command = cmMoveFiles, 0);
      Dispose(FC, Done);
      FC := nil;
      lChDir(OldDir);
      GlobalMessage(evCommand, cmRereadInfo, nil);
    end { CopyDir };
  {-DataCompBoy-}

  begin { TTreePanel.HandleEvent }
    inherited HandleEvent(Event);
    case Event.What of
      evCommand:
        case Event.Command of
          cmMoveFiles, cmCopyFiles:
            CopyDir;
        end {case};
    end {case};
  end { TTreePanel.HandleEvent };

Constructor THTreeView.Init;
  begin
    inherited Init(R, ADrive, True, ScrBar);
    Info := nil;
  end;

Constructor THTreeView.Load;
  begin
    inherited Load(s);
    GetPeerViewPtr(s, Info);
  end;

procedure THTreeView.Store;
  begin
    inherited Store(s);
    PutPeerViewPtr(s, Info);
  end;

procedure THTreeView.HideView;
  begin
    if Info <> nil then
      PHideView(Info)^.HideView;
    if (ScrollBar <> nil) then
      ScrollBar^.Hide;
    inherited HideView;
  end;

procedure THTreeView.ShowView;
  begin
    if Info <> nil then
      PHideView(Info)^.ShowView;
    if (ScrollBar <> nil) and GetState(sfActive+sfSelected) then
        ScrollBar^.Show;
    if Dirs = nil then
      ReadAfterLoad;
    inherited ShowView;
  end;

procedure THTreeView.ChangeBounds;
  var
    R: TRect;
  begin
    Dec(Bounds.B.Y, 2);
    SetBounds(Bounds);
    R := Bounds;
    R.A.Y := R.B.Y;
    Inc(R.B.Y, 2);
    if Info <> nil then
      Info^.SetBounds(R);
    if ScrollBar <> nil then
      begin
        R := Bounds;
        R.A.X := R.B.X;
        Inc(R.B.X);
        ScrollBar^.SetBounds(R)
      end
  end;

{-DataCompBoy-}
function CreateDirInheritance;
  var
    i, j: integer;
    SR: lSearchRec;
    M: String;
    BB {, DEr}: boolean;
  begin
    CreateDirInheritance := 0;
    BB := True; {DEr := false;}
    ClrIO;
    s := lFExpand(s);
    if Abort then
      exit;
    i := PosChar(':', s);
    if i = 0 then
      i := GetShareEnd(s); //AK155 22-11-2003
    if i = 0 then
      exit;
    Inc(i);
    while i < Length(s) do
      begin
        j := i;
        repeat
          Inc(i);
          if i > Length(s) then
            exit
        until (s[i] = '\') or (i = Length(s));
        M := Copy(s, 1, i-byte(s[i] = '\'));
        if M[Length(M)] = '.' then
          SetLength(M, Length(M)-1);
        ClrIO;
        lFindFirst(M, AnyFile, SR); {JO}
        {if DOSError <> 0 then DEr := true;}
        lFindClose(SR);
        if Abort then
          exit;
        if {DEr}DOSError <> 0 then
          begin
            if BB and (s[i] = '\') then
              begin
                CreateDirInheritance := j-1;
                BB := False
              end;
            if Confirm and (Confirms and cfCreateSubdir <> 0) then
              begin
                if (Length(s) > 3) and (s[Length(s)] = '\') then
                    SetLength(s, Length(s)-1);
                if s[Length(s)] = '.' then
                  SetLength(s, Length(s)-1);
                if (MessageBox(GetString(dlQueryCreateDir)+Cut(s, 40)+
                    ' ?', nil, mfYesNoConfirm) <> cmYes)
                then
                  exit;
              end;
            ClrIO;
            CheckMkDir(M);
            if Abort then
              exit;
            CreateDirectory(M, False);
          end;
      end;
  end { CreateDirInheritance };
{-DataCompBoy-}

end.
