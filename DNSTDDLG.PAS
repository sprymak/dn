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
{AK155 = Alexey Korop, 2:461/155@fidonet}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

unit DNStdDlg;

interface

uses
  Lfn, Files, FilesCol,
  Collect, Objects, Drivers, Views,
  Dialogs, Dos, Memory, Messages, DNApp;

const

  { Commands }

  cmFileOpen = 800; { Returned from TFileDialog when Open pressed }
  cmFileReplace = 801;
    { Returned from TFileDialog when Replace pressed }
  cmFileClear = 802;
    { Returned from TFileDialog when Clear pressed }
  cmFileInit = 803; { Used by TFileDialog internally }
  (*cmChangeDir   = 804;   { Used by TChDirDialog internally }*)
  cmRevert = 805; { Used by TChDirDialog internally }

  { Messages }

  cmFileFocused = 806; { A new file was focused in the TFileList }
  cmFileDoubleClicked { A file was selected in the TFileList }
  = 807;

type

  { TSearchRec }

  {  Record used to store directory information by TFileDialog }

  {DataCompBoy}
  TSearchRec = record
    Attr: byte;
    Time: longInt;
    Size: TSize;
    Name: String;
    end;
  {DataCompBoy}

type

  { TFileInputLine is a special input line that is used by      }
  { TFileDialog that will update its contents in response to a  }
  { cmFileFocused command from a TFileList.                     }

  PFileInputLine = ^TFileInputLine;
  TFileInputLine = object(TInputLine)
    Constructor Init(var Bounds: TRect; AMaxLen: integer);
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  { TFileCollection is a collection of TSearchRec's.            }

  PFileCollection = ^TFileCollection;
  TFileCollection = object(TSortedCollection)
    function Compare(Key1, Key2: Pointer): integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var s: TStream): Pointer; virtual;
    procedure PutItem(var s: TStream; Item: Pointer); virtual;
    end;

  { TSortedListBox is a TListBox that assumes it has a          }
  { TSortedCollection instead of just a TCollection.  It will   }
  { perform an incremental search on the contents.              }

  PSortedListBox = ^TSortedListBox;
  TSortedListBox = object(TListBox)
    SearchPos: word;
    Constructor Init(var Bounds: TRect; ANumCols: word;
    AScrollBar: PScrollBar);
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetKey(var s: String): Pointer; virtual;
    procedure NewLisT(AList: PCollection); virtual;
    end;

  { TFileList is a TSortedList box that assumes it contains     }
  { a TFileCollection as its collection.  It also communicates  }
  { through broadcast messages to TFileInput and TInfoPane      }
  { what file is currently selected.                            }

  PFileList = ^TFileList;
  TFileList = object(TSortedListBox)
    Constructor Init(var Bounds: TRect; const AWildCard: String;
      {DataCompBoy}
    AScrollBar: PScrollBar);
    destructor Done; virtual;
    function DataSize: word; virtual;
    procedure FocusItem(Item: longInt); virtual;
    function GetText(Item: longInt; MaxLen: integer): String;
      virtual;
    function GetKey(var s: String): Pointer; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure ReadDirectory(AWildCard: String);
    procedure SetData(var Rec); virtual;
    procedure Select; virtual;
    end;

  { TDirectoryList }
  PDirectoryList = ^TDirectoryList;
  TDirectoryList = object(TFileList)
    function GetText(Item: longInt; MaxLen: integer): String;
      virtual;
    function GetKey(var s: String): Pointer; virtual;
    procedure ReadDirectory(AWildCard: String);
    end;

  { TFileInfoPane is a TView that displays the information      }
  { about the currently selected file in the TFileList          }
  { of a TFileDialog.                                           }

  PFileInfoPane = ^TFileInfoPane;
  TFileInfoPane = object(TView)
    s: TSearchRec; {DataCompBoy}
    Constructor Init(var Bounds: TRect);
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  { TFileDialog is a standard file name input dialog            }

const
  fdOKButton = $0001; { Put an OK button in the dialog }
  fdOpenButton = $0002; { Put an Open button in the dialog }
  fdReplaceButton = $0004; { Put a Replace button in the dialog }
  fdClearButton = $0008; { Put a Clear button in the dialog }
  fdHelpButton = $0010; { Put a Help button in the dialog }
  fdNoLoadDir = $0100; { Do not load the current directory }
  { contents into the dialog at Init. }
  { This means you intend to change the }
  { WildCard by using SetData or store }
  { the dialog on a stream. }

type

  PFileDialog = ^TFileDialog;
  TFileDialog = object(TDialog)
    FileName: PFileInputLine;
    FileList: PFileList;
    DirList: PDirectoryList;
    WildCard: PString; {DataCompBoy}
    Directory: PString;
    Constructor Init(const AWildCard: String; ATitle: String;
    InputName: String; AOptions: word; HistoryId: byte);
      {DataCompBoy}
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    procedure GetData(var Rec); virtual;
    procedure GetFileName(var s: String); {DataCompBoy}
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SetData(var Rec); virtual;
    procedure Store(var s: TStream);
    function Valid(Command: word): boolean; virtual;
    private
    procedure ReadDirectory;
    end;

  { TDirEntry }

  PDirEntry = ^TDirEntry;
  TDirEntry = record
    DisplayText: PString;
    Directory: PString;
    end;

  { TDirCollection is a collection of TDirEntry's used by       }
  { TDirListBox.                                                }

  PDirCollection = ^TDirCollection;
  TDirCollection = object(TCollection)
    function GetItem(var s: TStream): Pointer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    procedure PutItem(var s: TStream; Item: Pointer); virtual;
    end;

const

  CInfoPane = #30;

function GetFileNameDialog(Mask, Title, Name: String; Buttons,
    HistoryId: word): String; {DataCompBoy}
function GetFileNameMenu(Path, Mask, Default: String; PutNumbers:
    boolean; var More, None: boolean): String; {JO}
{DataCompBoy: Mask now an standard DN filter, like "*.Ops;-Pas.*;p*.d"}

implementation

uses
  HistList, advance, advance1, advance2, advance3, RStrings,
    Commands,
  Startup, DNHelp, {JO}Menus {JO};

{-DataCompBoy-}
function PathValid(var Path: String): boolean;
  var
    ExpPath: String;
    F: lFile;
    SR: lSearchRec;
  begin
    ClrIO;
    ExpPath := lFExpand(Path);
    if Abort then
      exit;
    if ExpPath[Length(ExpPath)] = '\' then
      SetLength(ExpPath, Length(ExpPath)-1);
    lFindFirst(ExpPath+'\*.*', AnyFile, SR);
    PathValid := DOSError = 0;
    lFindClose(SR);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function ValidFileName(var FileName: String): boolean;
  var
    Dir: String;
    Name: String;
    ext: String;

    { Contains returns true if S1 contains any characters in S2 }
  function Contains(const s1, s2: String): boolean;
    {$IFNDEF NOASM}
    near; assembler;
    {$Frame-} {$USES ESI, EDI, EDX, EBX, ECX}
  asm
        CLD
        MOV     ESI,S1
        MOV     EDI,S2
        MOV     EDX,EDI
        XOR     EAX,EAX
        LODSB
        MOV     EBX,EAX
        OR      EBX,EBX
        JZ      @@2
        MOV     AL,[EDI]
        XCHG    EAX,ECX
 @@1:   PUSH    ECX
        MOV     EDI,EDX
        LODSB
        REPNE   SCASB
        POP     ECX
        JE      @@3
        DEC     EBX
        JNZ     @@1
 @@2:   XOR     AL,AL
        JMP     @@4
 @@3:   MOV     AL,1
 @@4:
 end
    ;
    {$ELSE}
    var
      Q: integer;
    begin
      Contains := True;
      for Q := 1 to Length(s2) do
        if PosChar(s2[Q], s1) > 0 then
          exit;
      Contains := False;
    end;
  {$ENDIF}

  begin { ValidFileName }
    ValidFileName := True;
    lTrueName(FileName, FileName);
    lFSplit(FileName, Dir, Name, ext);
    if not ((Dir = '') or PathValid(Dir)) or Contains(Name,
        IllegalChars) or
      Contains(Dir, IllegalChars)
    then
      ValidFileName := False;
    lTrueName(FileName, Dir);
    if FileName <> Dir then
      ValidFileName := False;
  end { ValidFileName };
{-DataCompBoy-}

{-DataCompBoy-}
{DataCompBoy
function GetCurDir: String;
var
  CurDir: String;
begin
  lGetDir(0, CurDir);
  if Length(CurDir) > 3 then CurDir:=CurDir+'\';
  GetCurDir := CurDir;
end;
}
{-DataCompBoy-}

type
  PSearchRec = ^TSearchRec; {DataCompBoy}

function IsWild(var s: String): boolean;
  begin
    IsWild := (Pos('?', s) > 0) or (Pos('*', s) > 0);
  end;

{ TFileInputLine }

Constructor TFileInputLine.Init(var Bounds: TRect; AMaxLen: integer);
  begin
    TInputLine.Init(Bounds, AMaxLen);
    EventMask := EventMask or evBroadcast;
  end;

{-DataCompBoy-}
procedure TFileInputLine.HandleEvent(var Event: TEvent);
  var
    Dir: String;
    Name: String;
    ext: String;
  begin
    TInputLine.HandleEvent(Event);
    if (Event.What = evBroadcast) and (Event.Command = cmFileFocused)
        and
      (State and sfSelected = 0)
    then
      begin
        Dir := PSearchRec(Event.InfoPtr)^.Name+'\';
        if PSearchRec(Event.InfoPtr)^.Attr and Directory <> 0 then
          Data^:= Dir+
          PFileDialog(Owner)^.WildCard^
        else
          Data^:= PSearchRec(Event.InfoPtr)^.Name;
        DrawView;
      end;
  end;
{-DataCompBoy-}

{ TFileCollection }

{-DataCompBoy-}
function TFileCollection.Compare(Key1, Key2: Pointer): integer;
  var
    K1: PSearchRec absolute Key1;
    k2: PSearchRec absolute Key2;
    s1, s2: String;
  begin
    s1 := UpLowStrg(K1^.Name);
    s2 := UpLowStrg(k2^.Name);
    if s1 = s2 then
      Compare := 0
    else if s1[2] = ':' then
      Compare := +1
    else if s2[2] = ':' then
      Compare := -1
    else if s1 = '..' then
      Compare := -1
    else if s2 = '..' then
      Compare := 1
    else if (K1^.Attr and Directory <> 0) and
      (k2^.Attr and Directory = 0)
    then
      Compare := 1
    else if (k2^.Attr and Directory <> 0) and
      (K1^.Attr and Directory = 0)
    then
      Compare := -1
    else if s1 > s2 then
      Compare := 1
    else
      Compare := -1;
  end { TFileCollection.Compare };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileCollection.FreeItem(Item: Pointer);
  begin
    Dispose(PSearchRec(Item));
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TFileCollection.GetItem(var s: TStream): Pointer;
  var
    Item: PSearchRec;
    B: byte;
  begin
    New(Item);
    s.Read(Item^, SizeOf(TSearchRec));
    GetItem := Item;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileCollection.PutItem(var s: TStream; Item: Pointer);
  begin
    s.Write(Item^, SizeOf(TSearchRec));
  end;
{-DataCompBoy-}


{ TSortedListBox }

Constructor TSortedListBox.Init(var Bounds: TRect; ANumCols: word;
  AScrollBar: PScrollBar);
  begin
    TListBox.Init(Bounds, ANumCols, AScrollBar);
    SearchPos := 0;
    ShowCursor;
    SetCursor(1, 0);
  end;

procedure TSortedListBox.HandleEvent(var Event: TEvent);
  var
    CurString, NewString: String;
    k: Pointer;
    Value, OldPos, OldValue: CondInt;
    t: boolean;

  function Equal(var s1: String; var s2: String; Count: word):
      boolean;
    var
      i: word;
    begin
      Equal := False;
      if (Length(s1) < Count) or (Length(s2) < Count) then
        exit;
      for i := 1 to Count do
        if UpCase(s1[i]) <> UpCase(s2[i]) then
          exit;
      Equal := True;
    end;

  begin { TSortedListBox.HandleEvent }
    OldValue := Focused;
    TListBox.HandleEvent(Event);
    if OldValue <> Focused then
      SearchPos := 0;
    if Event.What = evKeyDown then
      begin
        if Event.CharCode <> #0 then
          begin
            Value := Focused;
            if Value < Range then
              CurString := GetText(Value, 255)
            else
              CurString := '';
            OldPos := SearchPos;
            if Event.KeyCode = kbBack then
              begin
                if SearchPos = 0 then
                  exit;
                Dec(SearchPos);
                SetLength(CurString, SearchPos);
              end
            else if (Event.CharCode = '.') then
              SearchPos := Pos('.', CurString)
            else
              begin
                Inc(SearchPos);
                SetLength(CurString, SearchPos);
                CurString[SearchPos] := Event.CharCode;
              end;
            k := GetKey(CurString);
            if List <> nil then
              t := PSortedCollection(List)^.Search(k, Value)
            else
              Value := Range;
            if Value < Range then
              begin
                if Value < Range then
                  NewString := GetText(Value, 255)
                else
                  NewString := '';
                if Equal(NewString, CurString, SearchPos) then
                  begin
                    if Value <> OldValue then
                      begin
                        FocusItem(Value);
                        { Assumes ListControl will set the cursor to the first character }
                        { of the sfFocused item }
                        SetCursor(Cursor.X+SearchPos, Cursor.Y);
                      end
                    else
                      SetCursor(Cursor.X+(SearchPos-OldPos), Cursor.Y);
                  end
                else
                  SearchPos := OldPos;
              end
            else
              SearchPos := OldPos;
            if (SearchPos <> OldPos) or (Event.CharCode in ['A'..'Z',
                'a'..'z'])
            then
              ClearEvent(Event);
          end;
      end;
  end { TSortedListBox.HandleEvent };

function TSortedListBox.GetKey(var s: String): Pointer;
  begin
    GetKey := @S;
  end;

procedure TSortedListBox.NewLisT(AList: PCollection);
  begin
    TListBox.NewLisT(AList);
    SearchPos := 0;
  end;

{ TFileList }

{-DataCompBoy-}
Constructor TFileList.Init(var Bounds: TRect; const AWildCard:
    String;
  AScrollBar: PScrollBar);
  begin
    TSortedListBox.Init(Bounds, 1, AScrollBar);
  end;
{-DataCompBoy-}

destructor TFileList.Done;
  begin
    if List <> nil then
      Dispose(List, Done);
    List := nil;
    TListBox.Done;
  end;

procedure TFileList.Select;
  var
    Event: TEvent;
  begin
    if List^.Count > 0
    then
      begin
        Event.What := evBroadcast;
        Event.Command := cmFileFocused;
        Event.InfoPtr := List^.At(Focused);
        Owner^.HandleEvent(Event);
        inherited Select;
      end
    else{Owner^.SelectNext(False)}
      ;
  end;

function TFileList.DataSize: word;
  begin
    DataSize := 0;
  end;

procedure TFileList.FocusItem(Item: longInt);
  begin
    TSortedListBox.FocusItem(Item);
    Message(Owner, evBroadcast, cmFileFocused, List^.At(Item));
  end;

{-DataCompBoy-}
function TFileList.GetKey(var s: String): Pointer;
  const
    SR: TSearchRec = ();
  begin
    SR.Name := s;
    SR.Attr := 0;
    UpStr(SR.Name);
    GetKey := @SR;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TFileList.GetText(Item: longInt; MaxLen: integer): String;
  var
    SR: PSearchRec;
    s: String;
  begin
    SR := PSearchRec(List^.At(Item));
    if IsMixedCase(SR^.Name) then
      s := SR^.Name
    else
      s := LowStrg(SR^.Name);
    if Length(s) > 13 then
      begin
        SetLength(s, 13);
        s[13] := FMSetup.RestChar[1];
      end;
    GetText := s;
  end;
{-DataCompBoy-}

procedure TFileList.HandleEvent(var Event: TEvent);
  begin
    if (Event.What = evMouseDown) and (Event.Double) then
      begin
        Event.What := evCommand;
        Event.Command := cmOK;
        PutEvent(Event);
        ClearEvent(Event);
      end
    else
      TSortedListBox.HandleEvent(Event);
  end;

{-DataCompBoy-}
procedure TFileList.ReadDirectory(AWildCard: String);
  const
    FindAttr = ReadOnly+Archive+SysFile;
  var
    s: lSearchRec;
    P: PSearchRec;
    FileList: PFileCollection;
    Dir: String;
    Name: String;
    ext: String;
    Event: TEvent;
  begin
    AWildCard := lFExpand(AWildCard);
    lFSplit(AWildCard, Dir, Name, ext);
    FileList := New(PFileCollection, Init(5, 5));
    lFindFirst(AWildCard, FindAttr+byte(SystemData.Options and
      ossShowHidden <> 0)*Hidden, s);
    P := @P;
    while (P <> nil) and (DOSError = 0) do
      begin
        if (s.SR.Attr and Directory = 0) then
          begin
            P := MemAlloc(SizeOf(P^));
            if P <> nil then
              begin
                with P^ do
                  begin
                    Attr := s.SR.Attr;
                    Time := s.SR.Time;
                    Size := s.FullSize;
                    Name := s.SR.Name;
                  end;
                FileList^.Insert(P);
              end;
          end;
        lFindNext(s);
      end;
    lFindClose(s);
    if P = nil then
      Msg(dlStdDlgManyFiles, nil, mfOKButton+mfWarning);
    NewLisT(FileList);
    {  if List^.Count > 0 then
  begin
    Event.What := evBroadcast;
    Event.Command := cmFileFocused;
    Event.InfoPtr := List^.At(0);
    Owner^.HandleEvent(Event);
  end;  }
  end { TFileList.ReadDirectory };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileList.SetData(var Rec);
  begin
    {with PFileDialog(Owner)^ do}
    ReadDirectory(PFileDialog(Owner)^.Directory^+PFileDialog(Owner)^.
      WildCard^);
  end;
{-DataCompBoy-}


{ TDirectoryList }

{-DataCompBoy-}
function TDirectoryList.GetKey(var s: String): Pointer;
  const
    SR: TSearchRec = ();
  begin
    SR.Name := s;
    SR.Attr := Directory;
    UpStr(SR.Name);
    GetKey := @SR;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TDirectoryList.GetText(Item: longInt; MaxLen: integer):
    String;
  var
    SR: PSearchRec;
    s: String;
  begin
    SR := PSearchRec(List^.At(Item));
    if IsMixedCase(SR^.Name) then
      s := SR^.Name
    else
      s := UpStrg(SR^.Name);
    if Length(s) > 12 then
      begin
        SetLength(s, 12);
        s[12] := FMSetup.RestChar[1];
      end;
    if SR^.Name[2] <> ':' then
      GetText := s+'\'
    else
      GetText := SR^.Name;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDirectoryList.ReadDirectory(AWildCard: String);
  const
    FindAttr = ReadOnly+Archive+SysFile;
  var
    s: lSearchRec;
    P: PSearchRec;
    FileList: PFileCollection;
    Dir: String;
    Name: String;
    ext: String;
    Tmp: String;
    Event: TEvent;
    C: Char;
  begin
    AWildCard := lFExpand(AWildCard);
    lFSplit(AWildCard, Dir, Name, ext);
    FileList := New(PFileCollection, Init(5, 5));

    { Диски помещаем в список заранее на случай проблем с памятью }
    for C := 'Z'downto'A' do
      if ValidDrive(C) then
        begin
          P := MemAlloc(SizeOf(P^));
          // JO: сделаем просто и без выпендрёжа с "красивыми" скобочками, что
          //     позволит избежать нелепых проблем на ровном месте
          P^.Name := C+':';
          P^.Size := 0;
          P^.Time := $210000;
          P^.Attr := Directory;
          FileList^.Insert(P);
        end;

    Tmp := Dir+x_x;
    lFindFirst(Tmp, Directory+byte(SystemData.Options and
      ossShowHidden <> 0)*Hidden, s);
    {AK155 Тут надо учитывать, что под WinNT каталога '.'
     может не быть вообще, и что FindNextFile приводит к падению,
     если FindFirstFile не был успешным из-за несуществующего
     каталога. Такое бывает, например, в диалоге загрузки палитры,
     если каталога COLORS вообще нет. }
    while DOSError = 0 do
      begin
        if (s.SR.Attr and Directory <> 0) and not (s.SR.Name = '.')
            and
          ((Length(Dir) > 4) or (s.SR.Name <> '..'))
        then
          begin
            P := MemAlloc(SizeOf(P^));
            if P = nil then
              begin
                Msg(dlStdDlgManyFiles, nil, mfOKButton+mfWarning);
                break;
              end;
            with P^ do
              begin
                Attr := s.SR.Attr;
                Time := s.SR.Time;
                Size := s.FullSize;
                Name := s.FullName;
              end;
            FileList^.Insert(P);
          end;
        lFindNext(s);
      end;
    lFindClose(s);
    NewLisT(FileList);
  end { TDirectoryList.ReadDirectory };
{-DataCompBoy-}

{ TFileInfoPane }

Constructor TFileInfoPane.Init(var Bounds: TRect);
  begin
    TView.Init(Bounds);
    EventMask := EventMask or evBroadcast;
  end;

{-DataCompBoy-}
procedure TFileInfoPane.Draw;
  var
    B: TDrawBuffer;
    D: String[10];
    M: String[20];
    PM: boolean;
    Color: word;
    Time: DateTime;
    Path: String;
    FmtId: String;
    Params: array[0..7] of longInt;
    Str: String;
  const
    {$IFNDEF OS2}
    sDirectoryLine = ' %-12s %-10s  %sm';
    sFileLine = ' %-12s %-10s  %sm';
    {$ELSE}
    sDirectoryLine = '          %-10s  %sm         ';
    sFileLine = '          %-10s  %sm         ';
    {$ENDIF}
  begin
    { Display path }
    {$IFDEF OS_DOS}
    Path := #0+Cut(lfGetLongFileName(lFExpand(PFileDialog(Owner)^.
      Directory^))+PFileDialog(Owner)^.WildCard^, Size.X-1)+#0;
    {$ELSE}
    Path := #0+Cut(lFExpand(PFileDialog(Owner)^.Directory^)+
      PFileDialog(Owner)^.WildCard^, Size.X-1)+#0;
    {$ENDIF}
    Color := GetColor($01);
    MoveChar(B, ' ', Color, Size.X);
    MoveCStr(B[1], Path, Color);
    WriteLine(0, 0, Size.X, 1, B);

    { Display file }
    {$IFNDEF OS2}
    Params[0] := longInt(@S.Name);
    MoveChar(B, ' ', Color, Size.X);
    Params[0] := longInt(@S.Name);
    {$ENDIF}
    if s.Attr and Directory <> 0 then
      begin
        FmtId := sDirectoryLine;
        if s.Name[2] <> ':' then
          D := GetString(dlDirectory)
        else
          D := GetString(dlDrive);
        Params {$IFNDEF OS2}[1] {$ELSE}[0] {$ENDIF}:= longInt(@D);

      end
    else
      begin
        FmtId := sFileLine;
        D := FileSizeStr(s.Size);
        Params {$IFNDEF OS2}[1] {$ELSE}[0] {$ENDIF}:= longInt(@D);
      end;
    UnpackTime(s.Time, Time);
    with Time do
      MakeDate(0, Day, Month, Year mod 100, Hour, Min, M);
    {  M := GetString( TStrIdx( Integer( dlJanuary ) + Time.Month - 1 ));}
    Params {$IFNDEF OS2}[2] {$ELSE}[1] {$ENDIF}:= longInt(@M);
    {  Params[3] := Time.Day;
  Params[4] := Time.Year;}
    {  PM := Time.Hour >= 12;
  Time.Hour := Time.Hour mod 12;
  if Time.Hour = 0 then Time.Hour := 12;
  Params[5] := Time.Hour;
  Params[6] := Time.Min;
  if PM then Params[7] := Byte('p')
  else Params[7] := Byte('a');}
    FormatStr(Str, FmtId, Params);
    if CountryInfo.TimeFmt = 1 then
      SetLength(Str, Length(Str)-1);
    MoveStr(B, Str, Color);
    WriteLine(0, 2, Size.X, 1, B);
    Path := Cut(GetName(s.Name), Size.X-1);
    Color := GetColor($01);
    MoveChar(B, ' ', Color, Size.X);
    MoveStr(B[1], Path, Color);
    WriteLine(0, 1, Size.X, 1, B);

    { Fill in rest of rectangle }
    MoveChar(B, ' ', Color, Size.X);
    WriteLine(0, 3, Size.X, Size.Y-2, B);

end { TFileInfoPane.Draw };
{-DataCompBoy-}

function TFileInfoPane.GetPalette: PPalette;
  const
    P: String[Length(CInfoPane)] = CInfoPane;
  begin
    GetPalette := @P;
  end;

procedure TFileInfoPane.HandleEvent(var Event: TEvent);
  begin
    TView.HandleEvent(Event);
    if (Event.What = evBroadcast) and (Event.Command = cmFileFocused)
    then
      begin
        s := PSearchRec(Event.InfoPtr)^;
        DrawView;
      end;
  end;

{ TFileDialog }

{-DataCompBoy-}
Constructor TFileDialog.Init;
  var
    Control: PView;
    R: TRect;
    s: String;
    OPT: word;
    ACurDir: String;
  begin
    R.Assign(15, 1, 63, 21);
    TDialog.Init(R, ATitle);
    Options := Options or ofCentered;
    if AOptions and fdOpenButton <> 0
    then
      HelpCtx := hcOpenFileDialog
    else
      HelpCtx := hcSelectFileDialog;
    WildCard := NewStr(AWildCard);
    Directory := nil;

    R.Assign(3, 3, 31, 4);
    FileName := New(PFileInputLine, Init(R, MaxPathLen));
    FileName^.Data^:= WildCard^;
    Insert(FileName);
    R.Assign(2, 2, 3+CStrLen(InputName), 3);
    Control := New(PLabel, Init(R, InputName, FileName));
    Insert(Control);
    R.Assign(31, 3, 34, 4);
    Control := New(PHistory, Init(R, FileName, HistoryId));
    Insert(Control);

    R.Assign(17, 6, 18, 15);
    Control := New(PScrollBar, Init(R));
    Insert(Control);
    R.Assign(3, 6, 17, 15);
    FileList := New(PFileList, Init(R, WildCard^, PScrollBar(Control)));
    FileList^.HelpCtx := {hcOpen_Files}0;
    Insert(FileList);
    R.Assign(2, 5, 8, 6);
    Control := New(PLabel, Init(R, GetString(dlFiles), FileList));
    Insert(Control);

    R.Assign(34, 6, 35, 15);
    Control := New(PScrollBar, Init(R));
    Insert(Control);
    R.Assign(20, 6, 34, 15);
    DirList := New(PDirectoryList, Init(R, WildCard^, PScrollBar(
      Control)));
    DirList^.HelpCtx := {hcOpen_Director}0;
    Insert(DirList);
    R.Assign(19, 5, 34, 6);
    Control := New(PLabel, Init(R, GetString(dlDirectoriesLabel),
      DirList));
    Insert(Control);

    R.Assign(35, 3, 47, 5);
    OPT := bfDefault;
    if AOptions and fdOpenButton <> 0 then
      begin
        Control := New(PButton, Init(R, GetString(dlOpen),
          cmFileOpen, OPT));
        Control^.HelpCtx := 0;
        Insert(Control);
        OPT := bfNormal;
        Inc(R.A.Y, 3);
        Inc(R.B.Y, 3);
      end;
    if AOptions and fdOKButton <> 0 then
      begin
        Control := New(PButton, Init(R, GetString(dlOKButton),
          cmFileOpen, OPT));
        Control^.HelpCtx := {hcOK}0;
        Insert(Control);
        OPT := bfNormal;
        Inc(R.A.Y, 3);
        Inc(R.B.Y, 3);
      end;
    if AOptions and fdReplaceButton <> 0 then
      begin
        Insert(New(PButton, Init(R, GetString(dlReplace),
          cmFileReplace, OPT)));
        OPT := bfNormal;
        Inc(R.A.Y, 3);
        Inc(R.B.Y, 3);
      end;
    if AOptions and fdClearButton <> 0 then
      begin
        Insert(New(PButton, Init(R, GetString(dlClear), cmFileClear,
          OPT)));
        OPT := bfNormal;
        Inc(R.A.Y, 3);
        Inc(R.B.Y, 3);
      end;
    Control := New(PButton, Init(R, GetString(dlCancelButton),
      cmCancel, bfNormal));
    Control^.HelpCtx := 0;
    Insert(Control);
    Inc(R.A.Y, 3);
    Inc(R.B.Y, 3);
    if AOptions and fdHelpButton <> 0 then
      begin
        Control := New(PButton, Init(R, GetString(dlHelpButton),
          cmHelp, bfNormal));
        Insert(Control);
        Inc(R.A.Y, 3);
        Inc(R.B.Y, 3);
      end;

    R.Assign(2, 16, 46, 19);
    Control := New(PFileInfoPane, Init(R));
    Insert(Control);

    SelectNext(False);

    if AOptions and fdNoLoadDir = 0 then
      ReadDirectory;
  end { TFileDialog.Init };
{-DataCompBoy-}

{-DataCompBoy-}
Constructor TFileDialog.Load(var s: TStream);
  begin
    TDialog.Load(s);
    WildCard := s.ReadStr;
    GetSubViewPtr(s, FileName);
    GetSubViewPtr(s, FileList);
    if (FileName = nil) or (FileList = nil) then{Cat}
      Fail;

    ReadDirectory;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
destructor TFileDialog.Done;
  begin
    DisposeStr(WildCard);
    DisposeStr(Directory);
    TDialog.Done;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileDialog.GetData(var Rec);
  begin
    GetFileName(String(Rec));
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileDialog.GetFileName(var s: String);
  var
    Path: String;
    Name: String;
    ext: String;
    TPath: String;
    TName: String;
    text: String;

  function LTrim(s: String): String;
    var
      i: integer;
    begin
      i := 1;
      while (i < Length(s)) and (s[i] = ' ') do
        Inc(i);
      LTrim := Copy(s, i, MaxStringLength);
    end;

  function RTrim(s: String): String;
    var
      i: integer;
    begin
      while s[Length(s)] = ' ' do
        SetLength(s, Length(s)-1);
      RTrim := s;
    end;

  function RelativePath(var s: String): boolean;
    var
      i, j: integer;
      P: String;
    begin
      s := LTrim(RTrim(s));
      if (s <> '') and ((s[1] = '\') or (s[2] = ':')) then
          RelativePath := False
      else
        RelativePath := True;
    end;

  (* function NoWildChars(S: String): String; assembler;
{$IFNDEF BIT_32}
 asm
        PUSH    DS
        LDS     SI,S
        XOR     AX,AX
        LODSB
        XCHG    AX,CX
        LES     DI,@Result
        INC     DI
 @@1:   LODSB
        CMP     AL,'?'
        JE      @@2
        CMP     AL,'*'
        JE      @@2
        STOSB
 @@2:   LOOP    @@1
        XCHG    AX,DI
        MOV     DI,WORD PTR @Result
        SUB     AX,DI
        DEC     AX
        STOSB
        POP     DS
 end;
{$ELSE BIT_32}{&Frame-}{$USES ESI, EDI, ECX}
 asm
        LEA     ESI,S
        XOR     EAX,EAX
        LODSB
        XCHG    EAX,ECX
        MOV     EDI,@Result
        INC     EDI
 @@1:   LODSB
        CMP     AL,'?'
        JE      @@2
        CMP     AL,'*'
        JE      @@2
        STOSB
 @@2:   LOOP    @@1
        XCHG    EAX,EDI
        MOV     EDI,@Result
        SUB     EAX,EDI
        DEC     EAX
        STOSB
 end;
{$ENDIF}
*)
  {AK155: тут было нечто ассемблерное с условной трансляцией,
глючившее под VP. Эта функция вызывается в единственном месте
в связи с диалогом, так что ассемблер здесь совершенно не нужен.
Поэтому переписал на Паскале: и понятно, и платформонезависимо.}
  function NoWildChars(s: String): String;
    var
      i, j: integer;
      C: Char;
      {Result: string;}
    begin
      j := 0;
      for i := 1 to Length(s) do
        begin
          C := s[i];
          if (C <> '*') and (C <> '?') then
            begin
              Inc(j);
              Result[j] := C;
            end;
        end;
      SetLength(Result, j);
      if Result = '.' then
        Result := '';
        {Cat: чтобы к именам без расширения не добавлялась точка}
      {NoWildChars := Result;}
    end { NoWildChars };
  {/AK155}

  begin { TFileDialog.GetFileName }
    s := FileName^.Data^;
    ClrIO;
    if RelativePath(s) then
      s := lFExpand(Directory^+s)
    else
      s := lFExpand(s);
    if Abort then
      exit;
    lFSplit(s, Path, Name, ext);
    if (ext = '') and not IsDir(s) then
      begin
        lFSplit(WildCard^, TPath, TName, text);
        if ((Name = '') and (ext = '')) then
          s := Path+TName+text
        else if ext = '' then
          begin
            if IsWild(Name) then
              s := Path+Name+text
            else
              s := Path+Name+NoWildChars(text);
          end;
      end;
    s := DelSquashes(s);
    {$IFDEF OS_DOS}
    s := lfGetLongFileName(lfGetLongFileName(GetPath(s))+GetName(s));
    {$ELSE}
    s := GetPath(s)+GetName(s);
    {$ENDIF}
  end { TFileDialog.GetFileName };
{-DataCompBoy-}

procedure TFileDialog.HandleEvent(var Event: TEvent);

  procedure Go;
    procedure SuperFocus(var List: PCollection; curent: integer);
      begin
        if List^.Count > 0 then
          begin
            Event.What := evBroadcast;
            Event.Command := cmFileFocused;
            Event.InfoPtr := List^.At(curent);
            Owner^.HandleEvent(Event);
          end;
      end;

    begin
      if (FileList^.State and sfSelected > 0) then
        SuperFocus(FileList^.List, FileList^.Focused);
      if (DirList^.State and sfSelected > 0) then
        SuperFocus(DirList^.List, DirList^.Focused);
      ClearEvent(Event);
    end;

  begin { TFileDialog.HandleEvent }
    if Event.What = evKeyDown then
      begin
        if (FileName^.State and sfSelected) = 0 then
          case Event.KeyCode of
            kbRight:
              begin
                if (Current = PView(FileName))
                then
                  if (FileList^.List^.Count = 0)
                  then
                    DirList^.Select
                  else
                    SelectNext(False)
                else
                  SelectNext(False);
                Go;
              end;
            kbShiftTab, kbLeft:
              begin
                if (Current = PView(DirList))
                then
                  if (FileList^.List^.Count = 0)
                  then
                    FileName^.Select
                  else
                    SelectNext(True)
                else
                  SelectNext(True);
                Go;
              end;
          end {case};
        case Event.KeyCode of
          kbTab:
            begin
              if (Current = PView(FileName))
              then
                if (FileList^.List^.Count = 0)
                then
                  DirList^.Select
                else
                  SelectNext(False)
              else
                SelectNext(False);
              Go;
            end;
        end {case};

      end;
    TDialog.HandleEvent(Event);

    if Event.What = evCommand then
      case Event.Command of
        cmFileOpen, cmFileReplace, cmFileClear:
          begin
            EndModal(Event.Command);
            ClearEvent(Event);
          end;
      end {case};
  end { TFileDialog.HandleEvent };

{-DataCompBoy-}
procedure TFileDialog.SetData(var Rec);
  begin
    FileName^.SetData(Rec);
    if (String(Rec) <> '') and (IsWild(String(Rec))) then
      begin
        Valid(cmFileInit);
        FileName^.Select;
      end;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileDialog.ReadDirectory;
  var
    D: String;
    n: String;
    X: String;
  begin
    FileList^.ReadDirectory(WildCard^);
    DirList^.ReadDirectory(WildCard^);
    lFSplit(lFExpand(WildCard^), D, n, X);
    DisposeStr(WildCard);
    WildCard := NewStr(n+X);
    DisposeStr(Directory);
    Directory := NewStr(D);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
procedure TFileDialog.Store(var s: TStream);
  begin
    TDialog.Store(s);
    s.WriteStr(WildCard);
    PutSubViewPtr(s, FileName);
    PutSubViewPtr(s, FileList);
  end;
{-DataCompBoy-}

{-DataCompBoy-}
function TFileDialog.Valid(Command: word): boolean;
  var
    t: boolean;
    lFName: String;
    Dir: String;
    Name: String;
    ext: String;

  function CheckDirectory(var s: String): boolean;
    begin
      if not PathValid(s) then
        begin
          ErrMsg(erInvalidDrive);
          FileName^.Select;
          CheckDirectory := False;
        end
      else
        CheckDirectory := True;
    end;
  var
    Event: TEvent;
  begin { TFileDialog.Valid }
    if Command = 0 then
      begin
        Valid := True;
        exit;
      end
    else
      Valid := False;
    if TDialog.Valid(Command) then
      begin
        if Command = cmCancel then
          begin
            Valid := True;
            exit;
          end;
        GetFileName(lFName);
        if Abort then
          begin
            Valid := False;
            exit;
          end;
        if (Command <> cmCancel) and (Command <> cmFileClear) then
          begin
            if IsWild(lFName) then
              begin
                lFSplit(lFName, Dir, Name, ext);
                if CheckDirectory(Dir) then
                  begin
                    DisposeStr(Directory);
                    Directory := NewStr(Dir);
                    DisposeStr(WildCard);
                    WildCard := NewStr(Name+ext);
                    DirList^.ReadDirectory(Directory^+WildCard^);
                    FileList^.ReadDirectory(Directory^+WildCard^);
                    if Command <> cmFileInit then
                        { FileList^.Select;}
                      if ((DirList^.State and sfSelected) <> 0) or
                        (FileList^.List^.Count = 0)
                      then
                        begin
                          begin
                            Event.What := evBroadcast;
                            Event.Command := cmFileFocused;
                            Event.InfoPtr := DirList^.List^.At(0);
                            Owner^.HandleEvent(Event);
                          end;
                        end
                      else
                        FileList^.Select;
                  end
              end
            else if IsDir(lFName) then
              begin
                if CheckDirectory(lFName) then
                  begin
                    DisposeStr(Directory);
                    Directory := NewStr(lFName+'\');
                    DirList^.ReadDirectory(Directory^+WildCard^);
                    FileList^.ReadDirectory(Directory^+WildCard^);
                    if Command <> cmFileInit then
                      if ((DirList^.State and sfSelected) <> 0) or
                        (FileList^.List^.Count = 0)
                      then
                        begin
                          begin
                            Event.What := evBroadcast;
                            Event.Command := cmFileFocused;
                            Event.InfoPtr := DirList^.List^.At(0);
                            Owner^.HandleEvent(Event);
                          end;
                        end
                      else
                        FileList^.Select;
                  end
              end
            else if ValidFileName(lFName) then
              Valid := True
            else
              begin
                ErrMsg(erInvalidFileName);
                Valid := False;
              end
          end
        else
          Valid := True;
      end;
  end { TFileDialog.Valid };

{ TDirCollection }

procedure TDirCollection.FreeItem(Item: Pointer);
  var
    DirItem: PDirEntry absolute Item;
  begin
    DisposeStr(DirItem^.DisplayText);
    DisposeStr(DirItem^.Directory);
    Dispose(DirItem);
  end;

function TDirCollection.GetItem(var s: TStream): Pointer;
  var
    DirItem: PDirEntry;
  begin
    New(DirItem);
    DirItem^.DisplayText := s.ReadStr;
    DirItem^.Directory := s.ReadStr;
    GetItem := DirItem;
  end;

procedure TDirCollection.PutItem(var s: TStream; Item: Pointer);
  var
    DirItem: PDirEntry absolute Item;
  begin
    s.WriteStr(DirItem^.DisplayText);
    s.WriteStr(DirItem^.Directory);
  end;

{-DataCompBoy-}
function GetFileNameDialog(Mask, Title, Name: String; Buttons,
    HistoryId: word): String;
  var
    s: String;
    D: PFileDialog;
    B: boolean;
  begin
    s := '';
    B := False;
    if Mask = '' then
      begin
        Mask := x_x;
        B := True
      end;
    D := PFileDialog(Application^.ValidView(New(PFileDialog,
    Init(Mask, Title, Name, Buttons, HistoryId))));
    if D = nil then
      exit;
    if B then
      D^.SetData(s);
    if Desktop^.ExecView(D) <> cmCancel then
      D^.GetFileName(s);
    Dispose(D, Done);
    if s <> '' then
      begin
        {$IFDEF OS_DOS}
        s := lFExpand(lfGetLongFileName(s));
        {$ENDIF}
        HistoryAdd(HistoryId, s);
      end;
    GetFileNameDialog := s
  end { GetFileNameDialog };

function GetFileNameMenu(Path, Mask, Default: String; PutNumbers:
    boolean; var More, None: boolean): String; {JO}

  var
    s: String;
    l: TLineCollection;
    SR: lSearchRec;
    Current, Def: PMenuItem;
    Menu: PMenu;
    HMB: PMenuBox;
    CurIdx, i: integer;
    R: TRect;
    W: word;

  function LngMixCase(P: String): String;
    var
      i: integer;
    begin
      for i := 1 to Length(P) do
        if (i = 1) or not (P[i-1] in ['a'..'z',
          'A'..'Z'])
        then
          P[i] := UpCase(P[i])
        else if P[i] in ['A'..'Z']
        then
          P[i] := Chr(Ord(P[i])+Ord('a')-Ord('A'));
      LngMixCase := P
    end;

  begin { GetFileNameMenu }
    UpStr(Default);
    l.Init(5, 5, False);
    lFindFirst(MakeNormName(Path, '*.*'), AnyFile, SR);
    while DOSError = 0 do
      begin
        if ((SR.SR.Attr and Directory) = 0) and (SR.FullSize > 0)
            and
          (InFilter(SR.FullName, Mask) or InFilter(SR.SR.Name, Mask))
        then
          begin
            SetLength(s, 0);
            s := SR.FullName;
            s := LngMixCase(s);
            l.Insert(NewStr(s))
          end;
        lFindNext(SR)
      end;
    lFindClose(SR);
    Def := nil;
    Current := nil;
    if l.Count > 0 then
      begin
        for i := l.Count-1 downto 0 do
          begin
            if PutNumbers then
              case i of
                0..8:
                  s := '~'+Chr(Ord('1')+i)+'~ ';
                9:
                  s := '~0~ ';
                10..35:
                  s := '~'+Chr(55+i)+'~ ';
                else
                  s := '  ';
              end
            else
              s := '';
            Current := NewItem(s+CutH(PString(l.At(i))^, 40), '',
              kbNoKey,
            cmSelectMenuCommands+i, hcNoContext, Current);
            if UpStrg(PString(l.At(i))^) = Default then
              Def := Current
          end;
      end;
    if More then
      Current := NewItem('  '+GetString(dlMore), '', kbSpace,
      cmMoreCommand, hcNoContext, Current);
    if None then
      Current := NewItem('~'#27'~ '+GetString(dlNone), '', kbBack,
      cmNoneCommand, hcNoContext, Current);
    Menu := NewMenu(Current);
    if Def <> nil then
      Menu^.Default := Def;
    Application^.GetExtent(R);
    R.A.X := ((R.A.X+R.B.X) div 2)-8;
    R.B.X := R.A.X+16;
    R.A.Y := ((R.A.Y+R.B.Y-l.Count) div 2)-1;
    R.B.Y := R.A.Y+l.Count+2;
    if R.A.Y < Application^.Origin.Y+1 then
      R.A.Y := Application^.Origin.Y+1;
    if R.B.Y > Application^.Origin.Y+Application^.Size.Y-1
    then
      R.B.Y := Application^.Origin.Y+Application^.Size.Y-1;
    New(HMB, Init(R, Menu, nil));
    W := Application^.ExecView(HMB);
    Dispose(HMB, Done);
    DisposeMenu(Menu);
    More := False;
    None := False;
    GetFileNameMenu := '';
    case W of
      cmCancel:
        ;
      cmClose:
        ;
      cmNoneCommand:
        None := True;
      cmMoreCommand:
        More := True;
      else
        begin
          CurIdx := W-cmSelectMenuCommands;
          if (CurIdx >= 0) and (CurIdx < l.Count) then
            GetFileNameMenu := MakeNormName(Path, PString(l.At(
              CurIdx))^);
        end;
    end {case};
    l.FreeAll;
    l.Done;
  end { GetFileNameMenu }; {JO}

end.
