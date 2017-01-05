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

{ TEditWindow palette layout
  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
  |  |  |  |  |  |  |  |  |  |  |  |  |  |  `-- 78 Dragging frame
  |  |  |  |  |  |  |  |  |  |  |  |  |  `----- 77 Selected text
  |  |  |  |  |  |  |  |  |  |  |  |  `-------- 76 Text normal
  |  |  |  |  |  |  |  |  |  |  |  `----------- 75 Scroll bar Icons
  |  |  |  |  |  |  |  |  |  |  `-------------- 74 Scroll bar Page
  |  |  |  |  |  |  |  |  |  `----------------- 73 Frame title
  |  |  |  |  |  |  |  |  `-------------------- 72 Frame icon
  |  |  |  |  |  |  |  `----------------------- 71 Frame active
  |  |  |  |  |  |  `-------------------------- 69 Menu selected Shortcut
  |  |  |  |  |  `----------------------------- 68 Menu selected Disabled
  |  |  |  |  `-------------------------------- 67 Menu selected Normal
  |  |  |  `----------------------------------- 66 Menu text Shortcut
  |  |  `-------------------------------------- 65 Menu text Disabled
  |  `----------------------------------------- 64 Menu text Normal
  `-------------------------------------------- 70 Frame passive
}

unit Microed;

interface

uses
  Dos, Lfn {DataCompBoy}, Objects, Drivers, Memory, Views, Messages,
    DNApp,
  advance, advance1, advance2, advance3, Startup, Menus, DNStdDlg,
    Dialogs,
  Commands, UniWin, RStrings, DNHelp, {SBlocks,}ObjType, U_KeyMap,
    Collect,
  {$IFDEF REGEXP}RegExp, {$ENDIF}
  ed2, highlite;

const
  ClipBoard: PCollection = nil;

  CFileEditor = #13#14#16#17#18#19#20#21#22#23#24#25;

type
  PEditOptions = ^TEditOptions;
  TEditOptions = record
    AutoIndent: boolean;
    AutoBrackets: boolean;
    BackIndent: boolean; {BACKUNINDENTS}
    HiLite: boolean;
    HiliteLine: boolean;
    HiliteColumn: boolean;
    AutoJustify: boolean; {WRAPJUSTIFY}
    AutoWrap: boolean; {AUTOWRAP}
    LeftSide: word; {LEFTMARGIN}
    RightSide: word; {RIGHTMARGIN}
    InSide: word; {PARAGRAPH}
    ForcedCRLF: TCRLF;
    SmartTab: boolean;
    end;

  { TFileEditor }

  PFileEditor = ^TFileEditor;
  TFileEditor = object(TView)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    HScroll, VScroll: PScrollBar;
    ReplaceAll: boolean;
    Delta: TPoint;
    EditName: String; {DataCompBoy}
    FileLines: PLineCollection {PCollector}; {-SBlocks}
    isValid, Marking: boolean;
    SmartPad: boolean;
    ClipBrd: boolean; {-$VOL}
    OptMenu: PMenu;
    Mark, Sel: TRect;
    Pos, LastPos {,BlockPos}: TPoint;
    MarkPos: TPosArray;
    LastLine: longInt;
      {Cat: на первый взгляд кажется, что это номер последней строки файла, а на самом деле это номер текущей строки}
    DrawMode: integer;
    WorkString, LastShape: LongString;
    OldBlockValid, SearchOnDisplay,
    InsertMode, VertBlock, Modified, WorkModified,
    JustSaved, {piwamoto}
    BlockVisible, SpecChar, WasDelete, MouseMark, UnMark,
    LineMarking, OptimalFill, RulerVisible, EnableMarking,
    TabReplace, SearchActive: boolean;
    UndoInfo: PCollection;
    RedoInfo: PCollection; {-$VOL}
    UndoTimes, LastSaveUndoTimes: longInt;
    ChPosition: boolean;
    Macros: PCollection;
    Locker: PStream;
    LastDir: integer;
    MemEnough: boolean;
    KeyMap: TKeyMap; {-$VIV}
    EdOpt: TEditOptions;

    HiLitePar: THighliteParams;

    InfoL, BMrk: PView;

    MenuItemStr: array[boolean] of PString;
    {$IFDEF REGEXP}
    RegExp: PRegExp;
    {$ENDIF}
    Constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
      PScrollBar;
    var FileName: String); {DataCompBoy}
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    procedure Store(var s: TStream);
    procedure Awaken; virtual;
    procedure DoHighlite(var B; const s: LongString; const Attr:
      String);
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; virtual;
    function Valid(Command: word): boolean; virtual;
    function GetPalette: PPalette; virtual;
    function GetLine(Index: longInt): LongString;
    function GetSelection: PCollection;
    procedure SetState(AState: word; Enable: boolean); virtual;
    function ValidBlock: boolean;
    procedure CalcMenu;
    function Search(StartX, StartY: word): boolean;
    procedure InsertBlock(ABlock: PCollection; SaveUndo: boolean);
    procedure ModifyLine(Index: longInt; s: LongString; DelSpaces:
      boolean);
    procedure SetLimits;
    procedure ChangeBounds(var R: TRect); virtual;
    procedure ScrollTo(DeltaX, DeltaY: longInt);
    function LimitX: longInt;
    function LimitY: longInt;
    procedure StoreUndoInfo(What: word; Where: TPoint; var Info);
    function HandleCommand(var Event: TEvent): boolean; virtual;
    function KeyMapConvertStr(s: LongString; toAscii: boolean):
      LongString; {-$VIV}
    procedure KeyMapAtInsert(n: longInt; P: PLongString); {-$VIV}
    procedure KeyMapAtReplace(n: longInt; P: PLongString); {-$VIV}
    procedure Convert4Do(IP: PUndoRec; DoKind: TDoKind); {-$VOL}
    end;

procedure OpenEditor;
procedure OpenSmartpad;
procedure OpenClipBoard;

const
  ClipBoardStream: PStream = nil; {-$VOL}

type
  TSearchData = record
    Options: word;
    Dir: word;
    Scope: word;
    Origin: word;
    Line, What: String[250];
    end;

  {Cat: параметры поиска (TSearchData.Options)}
  {необходимо согласовывать с константой SwapBits в модуле Editor}
const
  efoCaseSens = 1;
  efoWholeWords = 2;
  efoReplacePrompt = 4;
  efoRegExp = 8;
  efoAllCP = 16;
  {/Cat}

const
  SearchData: TSearchData = (Options: 4; Dir: 0; Scope: 0; Origin: 0;
    Line: ''; What: '');

type
  TEditCommand = record
    C, C1, C2: word;
    CC1, CC2: array[1..2] of Char;
    end;

const
  MaxCommands: AInt = 0;
var
  EditCommands: array[1..110] of TEditCommand;

implementation

uses
  {$IFDEF PLUGIN}Plugin, {$ENDIF}
  Gauge, FViewer, HistList, Macro, Editor, WinClp, DNUtil, Histries,
  xTime, FileCopy, ASCIITab, DnIni, U_SrchF, EdWin, MicroEd2 {-$VIV};

const
  cmNoCommand = 4000;

  {AK155 Тут коды с Ctrl и без него должны обязательно стоять рядом,
 чтобы их позиции отличались только младшим битом индекса }
const
  UpDnKey: array[0..7] of word =
  (kbUp, kbCtrlUp,
  kbShiftUp, kbCtrlShiftUp,
  kbDown, kbCtrlDown,
  kbShiftDown, kbCtrlShiftDown);

  {SmartWindow: PEditWindow = nil;}
    {Cat: перенёс эти переменные в модуль MicroEd2}
  {ClipboardWindow: PEditWindow = nil;}
    {Cat: внимание! появились указатели SmartWindowPtr и ClipboardWindowPtr}

function MemAvail: longInt;
  begin
    MemAvail := MemAdjust(System.MemAvail)
  end;

{Cat}
{/Cat}

function TFileEditor.KeyMapConvertStr(s: LongString; toAscii:
    boolean): LongString; {-$VIV}
  begin
    case KeyMap of
      kmAnsi:
        if toAscii then
          KeyMapConvertStr := ansi_ascii(s)
        else
          KeyMapConvertStr := ascii_ansi(s);
      kmKoi8r:
        if toAscii then
          KeyMapConvertStr := koi8r_ascii(s)
        else
          KeyMapConvertStr := ascii_koi8r(s);
      else
        KeyMapConvertStr := s;
    end {case};
  end; {-$VIV}

procedure TFileEditor.KeyMapAtInsert(n: longInt; P: PLongString);
    {-$VIV}
  begin
    if (P <> nil) then
      P^:= KeyMapConvertStr(P^, False);
    FileLines^.AtInsert(n, P);
  end; {-$VIV}

procedure TFileEditor.KeyMapAtReplace(n: longInt; P: PLongString);
    {-$VIV}
  begin
    if (P <> nil) then
      P^:= KeyMapConvertStr(P^, False);
    FileLines^.AtReplace(n, P);
  end; {-$VIV}

procedure TFileEditor.Convert4Do(IP: PUndoRec; DoKind: TDoKind);
  var
    P: PUndoRec;
    i, j: longInt;
    s: LongString;
  begin
    if IP = nil then
      exit;
    New(P);
    Move(IP^, P^, SizeOf(TUndoRec));
    case DoKind of
      dkUndo:
        begin
          if UndoInfo = nil then
            UndoInfo := New(PDoCollection, Init(dkUndo));
          with IP^ do
            case What of
              udDelChar:
                begin
                  Dec(P^.Where.X, Count);
                  P^.Str := NewLongStr(Copy(GetLine(Where.Y), P^.
                    Where.X+1, Count));
                  UndoInfo^.Insert(P);
                end;
              udInsChar:
                begin
                  if Str <> nil then
                    begin
                      P^.Count := Length(Str^);
                      Inc(P^.Where.X, P^.Count);
                      UndoInfo^.Insert(P);
                    end
                  else
                    Dispose(P);
                end;
              udDelLine:
                begin
                  P^.Lines := New(PLineCollection, Init(10, 10, True));
                  for i := 0 to Count-1 do
                    P^.Lines^.Insert(NewLongStr(GetLine(P^.Where.Y+i)));
                  UndoInfo^.Insert(P);
                end;
              udInsLine:
                begin
                  P^.Str := NewLongStr(GetLine(Where.Y));
                  UndoInfo^.Insert(P);
                end;
              udDelBlock:
                begin
                  P^.Lines := New(PLineCollection, Init(10, 10, True));
                  if Lines^.Count > 0 then
                    s := CnvString(Lines^.At(0))
                  else
                    s := '';
                  j := Length(s);
                  if Vertical then
                    begin
                      for i := 0 to Lines^.Count-1 do
                        begin
                          s := Copy(GetLine(Where.Y+i), Where.X+1, j);
                          P^.Lines^.Insert(NewLongStr(s));
                        end;
                    end
                  else
                    begin
                      for i := 0 to Lines^.Count-1 do
                        begin
                          s := GetLine(Where.Y+i);
                          if i = 0 then
                            s := Copy(s, Where.X+1,
                              MaxLongStringLength)
                          else if i = Lines^.Count-1 then
                            s := Copy(s, 1, Length(s)-(j-Where.X));
                          P^.Lines^.Insert(NewLongStr(s));
                        end;
                    end;
                  UndoInfo^.Insert(P);
                end;
              udInsBlock, udFormatBlock:
                begin
                  i := Lines^.Count;
                  P^.Str := NewLongStr(Char(Lo(i))+Char(Hi(i))+
                    GetLine(Delta.Y));
                  UndoInfo^.Insert(P);
                end;
              udBackDel:
                begin
                  P^.Str := NewLongStr(Copy(GetLine(Where.Y), Where.X+1,
                    Count));
                  UndoInfo^.Insert(P);
                end;
              udSubDel:
                begin
                  P^.Str := NewLongStr(GetLine(Where.Y));
                  UndoInfo^.Insert(P);
                end;
              udSubDelLine:
                begin
                  P^.Str := NewLongStr(GetLine(Where.Y+1));
                  UndoInfo^.Insert(P);
                end;
              udIndentBlock:
                begin
                  UndoInfo^.Insert(P);
                end;
              udUnindentBlock:
                begin
                  UndoInfo^.Insert(P);
                end;
              udInsVertBlock:
                begin
                  P^.Count := Lines^.Count;
                  if P^.Count > 0 then
                    s := CnvLongString(Lines^.At(0))
                  else
                    s := '';
                  P^.Width := Length(s);
                  for i := 1 to P^.Count do
                    begin
                      j := Length(CnvLongString(Lines^.At(i-1)));
                      if j > P^.Width then
                        P^.Width := j;
                    end;
                  UndoInfo^.Insert(P);
                end;
              udReplace, udReplaceAll:
                begin
                  s := GetLine(Where.Y);
                  P^.Str := NewLongStr(Char(Length(Str^)-1)+Copy(s,
                    Where.X+1, byte(Str^[1])));
                  UndoInfo^.Insert(P);
                end;
              udReplaceChar:
                begin
                  s := GetLine(Where.Y);
                  Inc(P^.Where.X, Length(Str^));
                  P^.Str := NewLongStr(Copy(s, Where.X+1, Length(Str^)))
                    ;
                  UndoInfo^.Insert(P);
                end;
              udReplaceBlock, udClearBlock:
                begin
                  P^.Lines := New(PLineCollection, Init(10, 10, True));
                  for i := Where.Y to Lines^.Count+Where.Y-1 do
                    begin
                      s := GetLine(i);
                      j := Length(CnvLongString(Lines^.At(i-Where.Y)));
                      P^.Lines^.Insert(NewLongStr(Copy(s, Where.X+1, j))
                        );
                    end;
                  UndoInfo^.Insert(P);
                end;
              udStrModified:
                begin
                  P^.Str := NewLongStr(GetLine(Where.Y));
                  UndoInfo^.Insert(P);
                end;
              udDupeLine:
                begin
                  UndoInfo^.Insert(P);
                end;
            end {case}; {case of iP^.What/dkUndo}
        end;
      dkRedo:
        begin
          if RedoInfo = nil then
            RedoInfo := New(PDoCollection, Init(dkRedo));
          with IP^ do
            case What of
              udDelChar:
                begin
                  P^.Count := Length(Str^);
                  Inc(P^.Where.X, P^.Count);
                  RedoInfo^.Insert(P);
                end;
              udInsChar:
                begin
                  Dec(P^.Where.X, Count);
                  P^.Str := NewLongStr(Copy(GetLine(Where.Y), P^.
                    Where.X+1, Count));
                  RedoInfo^.Insert(P);
                end;
              udDelLine:
                begin
                  P^.Count := Lines^.Count;
                  RedoInfo^.Insert(P);
                end;
              udInsLine:
                begin
                  P^.Lines := New(PLineCollection, Init(10, 10, True));
                  P^.Lines^.Insert(NewLongStr(GetLine(P^.Where.Y)));
                  P^.Lines^.Insert(NewLongStr(GetLine(P^.Where.Y+1)));
                  RedoInfo^.Insert(P);
                end;
              udDelBlock:
                begin
                  P^.Lines := New(PLineCollection, Init(10, 10, True));
                  if Lines^.Count > 0 then
                    s := CnvLongString(Lines^.At(0))
                  else
                    s := '';
                  j := Length(s);
                  if Vertical then
                    if InsM then
                      begin
                        for i := 0 to Lines^.Count-1 do
                          P^.Lines^.Insert(NewLongStr(s));
                      end
                    else
                      begin
                        for i := 0 to Lines^.Count-1 do
                          begin
                            s := Copy(GetLine(Where.Y+i), Where.X+1, j);
                            P^.Lines^.Insert(NewLongStr(s));
                          end;
                      end
                  else
                    begin
                      s := GetLine(P^.Where.Y);
                      for i := 0 to Lines^.Count-1 do
                        P^.Lines^.Insert(NewLongStr(s));
                    end;
                  RedoInfo^.Insert(P);
                end;
              udInsBlock, udFormatBlock:
                begin
                  P^.Lines := New(PLineCollection, Init(10, 10, True));
                  for i := 1 to byte(Str^[2])*256+byte(Str^[1]) do
                    P^.Lines^.Insert(NewLongStr(GetLine(Delta.Y+i-1)));
                  RedoInfo^.Insert(P);
                end;
              udBackDel:
                begin
                  P^.Count := Length(Str^);
                  RedoInfo^.Insert(P);
                end;
              udSubDel:
                begin
                  P^.Str := NewLongStr(GetLine(P^.Where.Y));
                  RedoInfo^.Insert(P);
                end;
              udSubDelLine:
                begin
                  s := GetLine(Where.Y);
                  P^.Str := NewLongStr(s);
                  RedoInfo^.Insert(P);
                end;
              udIndentBlock:
                begin
                  RedoInfo^.Insert(P);
                end;
              udUnindentBlock:
                begin
                  RedoInfo^.Insert(P);
                end;
              udInsVertBlock:
                begin
                  P^.Lines := New(PLineCollection, Init(10, 10, True));
                  for i := 1 to Count do
                    begin
                      s := Copy(GetLine(i+Where.Y-1), Where.X+1,
                        Width);
                      P^.Lines^.Insert(NewLongStr(s));
                    end;
                  P^.Vertical := True;
                  P^.InsM := True;
                  RedoInfo^.Insert(P);
                end;
              udReplace, udReplaceAll:
                begin
                  s := GetLine(Where.Y);
                  P^.Str := NewLongStr(Char(Length(Str^)-1)+Copy(s,
                    Where.X+1, byte(Str^[1])));
                  RedoInfo^.Insert(P);
                end;
              udReplaceChar:
                begin
                  s := GetLine(Where.Y);
                  Dec(P^.Where.X, Length(Str^));
                  P^.Str := NewLongStr(Copy(s, P^.Where.X+1, Length(
                    Str^)));
                  RedoInfo^.Insert(P);
                end;
              udReplaceBlock, udClearBlock:
                begin
                  P^.Lines := New(PLineCollection, Init(10, 10, True));
                  for i := Where.Y to Lines^.Count+Where.Y-1 do
                    begin
                      s := GetLine(i);
                      j := Length(CnvLongString(Lines^.At(i-Where.Y)));
                      P^.Lines^.Insert(NewLongStr(Copy(s, Where.X+1, j))
                        );
                    end;
                  RedoInfo^.Insert(P);
                end;
              udStrModified:
                begin
                  P^.Str := NewLongStr(GetLine(Where.Y));
                  RedoInfo^.Insert(P);
                end;
              udDupeLine:
                begin
                  RedoInfo^.Insert(P);
                end;
            end {case}; {case of iP^.What/dkRedo}
        end;
    end {case}; {case of DoKind}
  end { TFileEditor.Convert4Do }; {-$VOL}

Constructor TFileEditor.Load(var s: TStream);
  begin
    inherited Load(s);
    {/Cat}
    MILoad(@Self, s);
    {$IFDEF REGEXP}
    RegExp := New(PRegExp, Init);
    {$ENDIF}
  end;

procedure TFileEditor.Store(var s: TStream);
  begin
    inherited Store(s);
    MIStore(@Self, s);
  end;

procedure TFileEditor.Awaken;
  begin
    MIAwaken(@Self);
  end;

function TFileEditor.GetPalette;
  const
    s: String[Length(CFileEditor)] = CFileEditor;
  begin
    GetPalette := @s;
  end;

{ TFileEditor }
Constructor TFileEditor.Init;
  var
    { FileToView: Text; }
    Line: LongString;
    { MaxWidth: Integer; }
    { p: PString; }
    { Nm: String; }
    { Xt: String; }
    i: integer;
  begin
    inherited Init(Bounds);
    InsertMode := True; {Cat}
    DrawMode := 0; {Cat}
    BlockVisible := False; {Cat}
    Mark.Assign(0, 0, 0, 0); {Cat}
    LastDir := -1;
    HelpCtx := hcEditor;
    UndoInfo := nil;
    RedoInfo := nil;
    HScroll := AHScrollBar;
    VScroll := AVScrollBar;
    GrowMode := gfGrowHiX+gfGrowHiY;
    Options := Options or ofSelectable;
    SearchActive := False;
    UnMark := True;
    EventMask := $FFFF;
    isValid := True;

    EdOpt.HiliteColumn := EditorDefaults.EdOpt and ebfHCl <> 0;
    EdOpt.HiliteLine := EditorDefaults.EdOpt and ebfHLn <> 0;
    EdOpt.AutoIndent := EditorDefaults.EdOpt and ebfAId <> 0;
    VertBlock := EditorDefaults.EdOpt and ebfVBl <> 0;
    EdOpt.BackIndent := EditorDefaults.EdOpt and ebfBSU <> 0;
    OptimalFill := EditorDefaults.EdOpt and ebfOfl <> 0;
    EdOpt.AutoJustify := EditorDefaults.EdOpt and ebfJwr <> 0;
    EdOpt.AutoBrackets := EditorDefaults.EdOpt and ebfABr <> 0;
    EdOpt.AutoWrap := EditorDefaults.EdOpt and ebfAwr <> 0;
    TabReplace := (EditorDefaults.EdOpt {$IFNDEF OS_DOS} shl 2
      {$ENDIF}) and ebfTRp <> 0;
    EdOpt.SmartTab := EditorDefaults.EdOpt2 and ebfSmt <> 0;

    Val(EditorDefaults.RM, EdOpt.RightSide, i);
    if i <> 0 then
      EdOpt.RightSide := 76;
    Val(EditorDefaults.LM, EdOpt.LeftSide, i);
    if i <> 0 then
      EdOpt.LeftSide := 0;
    Val(EditorDefaults.PM, EdOpt.InSide, i);
    if i <> 0 then
      EdOpt.InSide := 5;
    SmartPad := FileName = 'SmartPad';
    ClipBrd := FileName = 'Clipboard'; {-$VOL}
    FillChar(MarkPos, SizeOf(MarkPos), $FF);
    if SmartPad then
      begin
        Line := GetEnv('SMARTPAD');
        if (Line = '') then
          Line := SourceDir;
        if (Line[Length(Line)] <> '\') then
          Line := Line+'\';
        FileName := Line+FileName+'.DN';
      end
    else if ClipBrd then
      FileName := ''
    else
      Line := FileName;
    {FileLines := GetCollector(3000, 100);}
    FileLines := New(PLineCollection, Init(300, 1000, True));
      {-SBlocks}
    Macros := New(PCollection, Init(10, 10));

    (*if UpperCase(DefCodePage) = 'WIN' then KeyMap := kmAnsi
   else if UpperCase(DefCodePage) = 'KOI' then KeyMap := kmKoi8r
    else KeyMap := kmAscii; {-$VIV}*)
    MenuItemStr[True] := NewStr(GetString(dlMenuItemOn));
    MenuItemStr[False] := NewStr(GetString(dlMenuItemOff));
    {$IFDEF REGEXP}
    RegExp := New(PRegExp, Init);
    {$ENDIF}
    {/Cat}
  end { TFileEditor.Init };

destructor TFileEditor.Done;
  begin
    DisposeStr(MenuItemStr[False]);
    DisposeStr(MenuItemStr[True]);
    {$IFDEF REGEXP}
    if RegExp <> nil then
      begin
        Dispose(RegExp, Done);
        RegExp := nil;
      end;
    {$ENDIF}
    if FileLines <> nil then
      begin
        Dispose(FileLines, Done);
        FileLines := nil;
      end;
    if UndoInfo <> nil then
      begin
        Dispose(UndoInfo, Done);
        UndoInfo := nil;
      end;
    if RedoInfo <> nil then
      begin
        Dispose(RedoInfo, Done);
        RedoInfo := nil;
      end;
    if Macros <> nil then
      begin
        Dispose(Macros, Done);
        Macros := nil;
      end;
    if Locker <> nil then
      begin
        Dispose(Locker, Done);
        Locker := nil;
      end;
    if SmartPad then
      begin
        SmartWindowPtr := @SmartWindow;
        SmartWindow := nil;
      end;
    if ClipBrd then
      begin
        ClipboardWindowPtr := @ClipboardWindow;
        ClipboardWindow := nil;
      end;
    {/Cat}
    inherited Done;
  end { TFileEditor.Done };

procedure TFileEditor.StoreUndoInfo;
  var
    P, P1: PUndoRec;
    s: LongString;
    bl: boolean;
  begin
    if not MemOK then
      begin
        Dispose(UndoInfo, Done);
        UndoInfo := nil
      end;
    if UndoInfo = nil then
      UndoInfo := New(PDoCollection, Init(dkUndo));
    if RedoInfo <> nil then
      RedoInfo^.FreeAll;
    if UndoInfo^.Count > 0
    then
      P1 := UndoInfo^.At(UndoInfo^.Count-1)
    else
      P1 := nil;
    if ValidBlock then
      case What of
        udDelChar:
          if not VertBlock then
            begin
              if (Mark.A.Y = Where.Y) and (Where.X < Mark.A.X) then
                  Dec(Mark.A.X);
              if (Mark.B.Y = Where.Y) and (Where.X < Mark.B.X) then
                  Dec(Mark.B.X);
            end;
        udReplaceChar:
          ;
        udBackDel:
          if not VertBlock then
            begin
              if (Mark.A.Y = Where.Y) and (Where.X <= Mark.A.X) then
                  Dec(Mark.A.X);
              if (Mark.B.Y = Where.Y) and (Where.X <= Mark.B.X) then
                  Dec(Mark.B.X);
            end;
        udInsChar:
          if not VertBlock then
            begin
              if (Mark.A.Y = Where.Y) and (Where.X < Mark.A.X) then
                  Inc(Mark.A.X);
              if (Mark.B.Y = Where.Y) and (Where.X < Mark.B.X) then
                  Inc(Mark.B.X);
            end;
        udInsLine, udDupeLine:
          begin
            if Mark.A.Y > Where.Y then
              Inc(Mark.A.Y);
            if Mark.B.Y > Where.Y then
              Inc(Mark.B.Y);
            if (Mark.A.Y = Where.Y) and (Where.X <= Mark.A.X) then
              begin
                Inc(Mark.A.Y);
                if What <> udDupeLine then
                  Mark.A.X := Mark.A.X-Where.X;
              end;
            if (Mark.B.Y = Where.Y) and (Where.X < Mark.B.X) then
              begin
                Inc(Mark.B.Y);
                if What <> udDupeLine then
                  Mark.B.X := Mark.B.X-Where.X;
              end;
          end;
        udSubDel,
        udSubDelLine,
        udDelLine:
          begin
            if (Where.Y = Mark.A.Y) and (Where.Y = Mark.B.Y) and
              (What = udDelLine)
            then
              Mark.A := Mark.B;
            if Mark.A.Y > Where.Y then
              begin
                Dec(Mark.A.Y);
                if (What <> udDelLine) and not VertBlock and
                  (Mark.A.Y = Where.Y) and (Where.X > Mark.A.X)
                then
                  Mark.A.X := Where.X;
              end;
            if Mark.B.Y > Where.Y then
              begin
                Dec(Mark.B.Y);
                if (What <> udDelLine) and not VertBlock and
                  (Mark.B.Y = Where.Y) and (Where.X > Mark.B.X)
                then
                  Mark.B.X := Where.X;
              end;
          end;
      end {case};

    case What of
      udDelChar:
        if (P1 <> nil) and (P1^.What = udDelChar) and (P1^.Where.Y =
            Where.Y) and
          (P1^.Where.X = Where.X)
        then
          begin
            s := P1^.Str^+Char(Info);
            DisposeLongStr(P1^.Str);
            P1^.Str := NewLongStr(s);
          end
        else
          begin
            s := ''+Char(Info);
            New(P);
            P^.What := What;
            P^.Where := Where;
            P^.Str := NewLongStr(s);
            P^.KeyMap := KeyMap; {-$VIV}
            UndoInfo^.Insert(P);
            Inc(UndoTimes);
          end;
      udReplaceChar:
        if (P1 <> nil) and (P1^.What = What) and (P1^.Where.Y =
            Where.Y) and
          (Where.X = P1^.Where.X)
        then
          begin
            s := P1^.Str^+Char(Info);
            Inc(P1^.Where.X);
            DisposeLongStr(P1^.Str);
            P1^.Str := NewLongStr(s);
          end
        else
          begin
            s := ''+Char(Info);
            New(P);
            P^.What := What;
            P^.Where := Where;
            P^.Str := NewLongStr(s);
            Inc(P^.Where.X);
            P^.KeyMap := KeyMap; {-$VIV}
            UndoInfo^.Insert(P);
            Inc(UndoTimes);
          end;
      udBackDel:
        if (P1 <> nil) and (P1^.What = What) and (P1^.Where.Y =
            Where.Y) and
          (P1^.Where.X = Where.X)
        then
          begin
            s := Char(Info)+P1^.Str^;
            Dec(P1^.Where.X);
            DisposeLongStr(P1^.Str);
            P1^.Str := NewLongStr(s);
          end
        else
          begin
            s := ''+Char(Info);
            New(P);
            P^.What := What;
            P^.Where := Where;
            P^.Str := NewLongStr(s);
            Dec(P^.Where.X);
            P^.KeyMap := KeyMap; {-$VIV}
            UndoInfo^.Insert(P);
            Inc(UndoTimes);
          end;
      udInsChar:
        if (P1 <> nil) and (P1^.What = What) and (P1^.Where.Y =
            Where.Y) and
          (P1^.Where.X = Where.X)
        then
          begin
            Inc(P1^.Count);
            Inc(P1^.Where.X)
          end
        else
          begin
            New(P);
            P^.What := What;
            P^.Where := Where;
            Inc(P^.Where.X);
            P^.Count := 1;
            P^.KeyMap := KeyMap; {-$VIV}
            UndoInfo^.Insert(P);
            Inc(UndoTimes);
          end;
      udDupeLine:
        begin
          New(P);
          P^.What := What;
          P^.Where := Where;
          P^.Count := 1;
          P^.KeyMap := KeyMap; {-$VIV}
          UndoInfo^.Insert(P);
          Inc(UndoTimes);
        end;
      udDelLine:
        if (P1 <> nil) and (P1^.What = What) and (P1^.Where.Y =
            Where.Y)
        then
          P1^.Lines^.Insert(NewLongStr(LongString(Info)))
        else
          begin
            New(P);
            P^.What := What;
            P^.Where := Where;
            P^.Lines := New(PLineCollection, Init(10, 10, True));
            P^.Lines^.Insert(NewLongStr(LongString(Info)));
            P^.KeyMap := KeyMap; {-$VIV}
            UndoInfo^.Insert(P);
            Inc(UndoTimes);
          end;
      udInsLine, udSubDel, udSubDelLine, udInsBlock, udReplace,
        udFormatBlock, udReplaceAll:
        begin
          New(P);
          P^.What := What;
          P^.Where := Where;
          P^.Str := NewLongStr(LongString(Info));
          P^.KeyMap := KeyMap; {-$VIV}
          UndoInfo^.Insert(P);
          Inc(UndoTimes);
        end;
      udReplaceBlock, udClearBlock:
        begin
          New(P);
          P^.What := What;
          P^.Where := Where;
          P^.Lines := PCollection(Info);
          P^.KeyMap := KeyMap; {-$VIV}
          UndoInfo^.Insert(P);
          Inc(UndoTimes);
        end;
      udInsVertBlock:
        begin
          New(P);
          P^.What := What;
          P^.Where := Where;
          P^.Count := longInt(Info) and $FFFF;
          P^.Width := longInt(Info) shr 16;
          P^.KeyMap := KeyMap; {-$VIV}
          UndoInfo^.Insert(P);
          Inc(UndoTimes);
        end;
      udDelBlock:
        begin
          New(P);
          P^.What := What;
          P^.Where := Where;
          P^.Vertical := VertBlock;
          P^.InsM := InsertMode; {-$VOL}
          P^.Lines := PCollection(Info);
          P^.KeyMap := KeyMap; {-$VIV}
          UndoInfo^.Insert(P);
          Inc(UndoTimes);
        end;
      udIndentBlock, udUnindentBlock
      :
        if (P1 <> nil) and (P1^.What = What) and (P1^.Block.A.Y =
            TRect(Info).A.Y) and
          (P1^.Block.A.Y = TRect(Info).B.Y)
        then
          begin
            Inc(P1^.Count);
          end
        else
          begin
            New(P);
            P^.What := What;
            P^.Where := Where;
            P^.Block := TRect(Info);
            P^.Count := 1;
            P^.KeyMap := KeyMap; {-$VIV}
            UndoInfo^.Insert(P);
            Inc(UndoTimes);
          end;
      udStrModified:
        begin
          New(P);
          P^.What := What;
          P^.Where := Where;
          Inc(P^.Where.X);
          P^.Count := 1;
          P^.Str := NewLongStr(LongString(Info));
          P^.KeyMap := KeyMap; {-$VIV}
          UndoInfo^.Insert(P);
          Inc(UndoTimes);
        end;
    end {case};
    CalcMenu;
  end { TFileEditor.StoreUndoInfo };

procedure TFileEditor.CalcMenu;
  var
    gc: TCommandSet;
    MI: PMenuItem;

  procedure SetM(B: boolean);
    var
      G: LongString;
    begin
      if MI = nil then
        exit;
      G := MenuItemStr[B]^;
      if MI^.Param^ <> G then
        begin
          DisposeStr(MI^.Param);
          MI^.Param := NewStr(G);
        end;
      MI := MI^.Next;
    end;

  var
    BlkC: TCommandSet;

  begin { TFileEditor.CalcMenu }
    if (Owner = nil) or (PEditWindow(Owner)^.MenuBar = nil) then
      exit;
    BlkC := [cmCopy, cmCut, cmClear, cmBlockWrite, cmFJustify,
      cmCopyBlock, cmMoveBlock,
    cmFRight, cmFLeft, cmFCenter, cmPrintBlock, cmCalcBlock,
      cmSortBlock,
    cmRevSortBlock, cmUpcaseBlock, cmLowcaseBlock, cmCapitalizeBlock,
    cmIndentBlock, cmUnIndentBlock, cmRusEngConv]; {-$VIV}
    OldBlockValid := (ValidBlock and BlockVisible);
    if OldBlockValid then
      EnableCommands(BlkC)
    else
      DisableCommands(BlkC);
    if (UndoInfo <> nil) and (UndoInfo^.Count > 0)
    then
      EnableCommands([cmUndo])
    else
      DisableCommands([cmUndo]);
    {-$VOL begin}
    if (RedoInfo <> nil) and (RedoInfo^.Count > 0)
    then
      EnableCommands([cmRedo])
    else
      DisableCommands([cmRedo]);
    {-$VOL end}
    if (ClipBoard <> nil) and (ClipBoard^.Count > 0) or
      ((SystemData.Options and ossUseSysClip <> 0) and
        GetWinClipSize)
    then
      EnableCommands([cmPaste])
    else
      DisableCommands([cmPaste]);
    if OptMenu <> nil then
      begin
        MI := OptMenu^.Items;
        SetM(EdOpt.BackIndent);
        SetM(EdOpt.AutoBrackets);
        SetM(EdOpt.AutoIndent);
        SetM(EdOpt.AutoWrap);
        SetM(EdOpt.AutoJustify);
        SetM(VertBlock);
        SetM(OptimalFill);
        SetM(EdOpt.HiliteLine);
        SetM(EdOpt.HiliteColumn);
        SetM(EdOpt.HiLite);
        SetM(TabReplace);
        SetM(EdOpt.SmartTab);
      end;
    GetCommands(gc);
    PEditWindow(Owner)^.MenuBar^.SetCommands(gc);
    SetCommands(gc);
  end { TFileEditor.CalcMenu };

procedure TFileEditor.SetState;
  begin
    inherited SetState(AState, Enable);
    if (AState and (sfActive+sfFocused+sfSelected+sfVisible) <> 0)
      or (OldBlockValid xor (ValidBlock and BlockVisible))
    then
      CalcMenu;
    {if not GetState(sfFocused) then CalcMenu;}
    if AState and sfActive <> 0 then
      if GetState(sfActive+sfSelected) then
        begin
          if HScroll <> nil then
            HScroll^.Show;
          HScroll^.MakeFirst;
          if VScroll <> nil then
            VScroll^.Show;
          DrawView;
          EnableCommands([cmViewFile]);
        end
      else
        begin
          if HScroll <> nil then
            HScroll^.Hide;
          if VScroll <> nil then
            VScroll^.Hide;
          DrawView;
        end;
    { if (InfoL<>nil) then InfoL^.Draw;}
    { if (BMrk<>nil)  then BMrk^.Draw; }
  end { TFileEditor.SetState };

procedure TFileEditor.ScrollTo;
  begin
    if HScroll <> nil then
      HScroll^.SetValue(DeltaX);
    if VScroll <> nil then
      VScroll^.SetValue(DeltaY);
  end;

{-DataCompBoy-}
function TFileEditor.LimitX;
  begin
    if HScroll <> nil then
      LimitX := HScroll^.Max
    else
      LimitX := 0;
  end;

function TFileEditor.LimitY;
  begin
    if VScroll <> nil then
      LimitY := VScroll^.Max
    else
      LimitY := 0;
  end;
{-DataCompBoy-}

{-DataCompBoy-}
{Cat: эта функция идентична имеющейся в DnStdDlg}
(*
function GetFileNameDialog(Mask, Title, Name: String; Buttons, HistoryID: Word): String;
 var S: String;
     D: PFileDialog;
     B: Boolean;
begin
  S := ''; B := false; if Mask = '' then begin Mask := x_x; B := true end;
  D := PFileDialog(Application^.ValidView(New(PFileDialog,
        Init(Mask, Title, Name, Buttons, HistoryID))));
  if D = nil then Exit;
  if B then D^.SetData(S);
  if Desktop^.ExecView(D) <> cmCancel then
    begin
      D^.GetFileName(S);
      HistoryAdd(HistoryID, S);
    end;
  Dispose(D,Done);
{$IFDEF OS_DOS}
  GetFileNameDialog := lfGetLongFileName(S);
{$ELSE}
  GetFileNameDialog := S;
{$ENDIF}
end;
*)
{-DataCompBoy-}

{-DataCompBoy-}
procedure OpenEditor;
  var
    R: TRect;
    P: PWindow;
    s: String;
  begin
    s := GetFileNameDialog(x_x, GetString(dlED_OpenFile),
    GetString(dlOpenFileName),
    fdOpenButton+fdHelpButton, hsEditOpen);
    if s = '' then
      exit;
    Desktop^.GetExtent(R);
    s := lFExpand(GetPath(s))+GetName(lFExpand(s));
    Application^.InsertWindow(New(PEditWindow, Init(R, s)));
  end;
{-DataCompBoy-}

procedure TFileEditor.ChangeBounds;
  begin
    inherited ChangeBounds(R);
    SetLimits;
  end;

procedure TFileEditor.SetLimits;
  begin
    if HScroll <> nil then
      HScroll^.SetParams(Delta.X, 0, MaxLongStringLength, 1, 1);
    if VScroll <> nil then
      VScroll^.SetParams(Delta.Y, 0, FileLines^.Count-1, Size.Y, 1);
  end;

function TFileEditor.GetSelection;
  var
    P: PCollection;
    s: LongString;
    i: longInt;
  begin
    GetSelection := nil;
    MemEnough := True;
    if not (BlockVisible and ValidBlock) or LowMemory then
      exit;
    P := New(PLineCollection, Init(Mark.B.Y-Mark.A.Y+1, 5, True));
    for i := Mark.A.Y to Mark.B.Y do
      begin
        s := GetLine(i);
        if VertBlock or (Mark.A.Y = Mark.B.Y) then
          begin
            s := Copy(s, Mark.A.X+1, Mark.B.X-Mark.A.X);
            if VertBlock and (Mark.B.X-Mark.A.X > Length(s)) then
              s := s+LongStrg(' ', Mark.B.X-Mark.A.X-Length(s));
          end
        else if i = Mark.A.Y then
          s := Copy(s, Mark.A.X+1, MaxLongStringLength)
        else if i = Mark.B.Y then
          s := Copy(s, 1, Mark.B.X);
        { P^.Insert(NewLongStr(S)); }
        with P^ do
          AtInsert(Count, NewLongStr(s)); {AK155}

        {AK155: MemOK - это нечто странное. Там, в конечном итоге,
проверяется на nil некий Linker, которому нигде ничего не
присваивается, так что он заведомо nil. А исключение этой
бессмысленной проверки ускоряет работу во _много_ раз (более
20, точно мерять не хватило терпения ). }

        if LowMemory { or not MemOK} then
          begin
            Dispose(P, Done);
            MemEnough := False;
            exit;
          end;
      end;
    GetSelection := P;
  end { TFileEditor.GetSelection };

function TFileEditor.Valid(Command: word): boolean;
  var
    i: word;
    P: Pointer;
    s: String;
    V: boolean;
  begin
    Valid := True;
    V := True;
    if Command = cmValid then
      Valid := isValid;
    if (SmartPad or ClipBrd) then
      begin
        if Modified then
          Message(@Self, evCommand, cmSaveText, nil);
        PEditWindow(Owner)^.ModalEnd := True;
        exit;
      end;
    if ((Command = cmClose) or (Command = cmQuit)) then
      begin
        if Modified then
          begin
            {ModifyLine(Delta.Y, WorkString, true);}
            { Commented by Flash at 05-03-2003 because of bug
            with pressing Esc button after sorting a block.
            This could cause erasing of a current line and
            copying of another one. }
            s := Cut(EditName, 30);
            P := @S;
            i := MessageBox(GetString(dlQueryModified), @P,
              mfWarning+mfYesNoCancel);
            if i = cmYes
            then
              Message(@Self, evCommand, cmSaveText, nil)
            else if i = cmNo then
              Modified := False;
            V := not ((i = cmCancel) or (i = cmYes) and Modified);
          end;
        Valid := V;
        if V and not (SmartPad or ClipBrd) and (Owner <> nil) then
            StoreEditInfo(Owner);
      end;
  end { TFileEditor.Valid };

function TFileEditor.ValidBlock;
  begin
    if VertBlock then
      ValidBlock := ((Mark.A.Y <= Mark.B.Y) and (Mark.A.X < Mark.B.X))
    else
      ValidBlock := (Mark.A.Y < Mark.B.Y) or
      ((Mark.A.Y = Mark.B.Y) and (Mark.A.X < Mark.B.X));
  end;

{Cat: переделал для поиска с регэкспами}
function TFileEditor.Search;
  label
    1, LExit, _LExit, EndReplace;
  var
    i, j, TrX, StX: longInt;
    D, DD: TPoint;
    t: TRect;
    W, s, s1: LongString;
    F, Prompt: boolean;
    OldMark: TRect;
    NumRep: longInt;
    Dir: integer;
    OldCache: boolean;
    tmr: TEventTimer;
    AllCP: boolean; {-$VIV 14.05.99}

  procedure _DrawViews;
    begin
      DrawView;
      HScroll^.DrawView;
      VScroll^.DrawView;
    end;

  begin { TFileEditor.Search }
    SearchActive := True;
    NewTimer(tmr, 0);
    Marking := False;
    OldMark := Mark;
    if (SearchData.Scope = 1 {отмеченный текст}) and not (
        BlockVisible and ValidBlock)
    then
      goto LExit;
    Search := False;
    F := True;
    Prompt := (SearchData.What <> #0) and (SearchData.Options and
      efoReplacePrompt <> 0); {запрос на замену}
    D.Assign(StartX, StartY);
    {AK155 6-10-2003}
    { при поиске в выделении сдвигаем точку старта поиска
  в направлении поиска к соответствующему концу выделения }
    if SearchData.Scope = 1 then
      begin
        if (SearchData.Dir = 0) then
          begin{вперёд}
            if (OldMark.A.Y > D.Y) then
              D := OldMark.A;
          end
        else
          begin{назад}
            if (OldMark.B.Y < D.Y) then
              D := OldMark.B;
          end;
      end;
    {/AK155}
    W := SearchData.Line;
    if W = '' then
      goto LExit;
    if SearchData.Options and efoCaseSens = 0 then
        {не различать регистр}
      UpLongStr(W);
    AllCP := (SearchData.What = #0) and (SearchData.Options and
      efoAllCP <> 0 {во всех кодировках});
    UnMark := False;
    NumRep := 0;
    Dir := 1-2*SearchData.Dir;
      {направление поиска: +1 вперёд, -1 назад}
    while (Dir > 0) and (D.Y < FileLines^.Count) or
      (Dir < 0) and (D.Y >= 0)
    do
      begin
1:
        s1 := GetLine(D.Y);
        TrX := Length(s1); {S := S1;}
        if (Dir < 0) then{поиск назад}
          begin
            if (D.X > Length(s1)) then
              D.X := Length(s1);
            StX := 1;
            TrX := D.X;
            if SearchData.Scope = 1 then{отмеченный текст}
              begin
                Mark := OldMark;
                if D.Y < Mark.A.Y then
                  break;
                if D.Y > Mark.B.Y then
                  begin
                    Dec(D.Y);
                    D.X := MaxLongStringLength;
                    continue;
                  end;
                if (D.Y = Mark.B.Y) and (D.Y = Mark.A.Y) or
                    VertBlock
                then
                  if StX < Mark.A.X then
                    StX := Mark.A.X+1;
                if (D.Y = Mark.B.Y) or VertBlock then
                  if TrX > Mark.B.X then
                    TrX := Mark.B.X;
              end;
            if TrX < StX then
              i := 0 {-$VIV 14.05.99}
            else
              {$IFDEF REGEXP} if SearchData.Options and efoRegExp <> 0
            then{поиск регэкспа - пока работает только вперёд}
              begin
                {Cat:todo}MessageBox(
                  'RegExp BackSearch is not implemented yet', nil,
                  mfError+mfOKButton);
                goto LExit;
              end
            else
              {$ENDIF} if AllCP then
              i := BackSearchForAllCP(W, s1[StX], TrX-StX+1,
                SearchData.Options and efoCaseSens <> 0)
            else
              i := BackSearchFor(W, s1[StX], TrX-StX+1, SearchData.
                Options and efoCaseSens <> 0);
          end
        else{поиск вперёд}
          begin
            StX := D.X+1;
            if SearchData.Scope = 1 then{отмеченный текст}
              begin
                Mark := OldMark;
                if D.Y < Mark.A.Y then
                  begin
                    Inc(D.Y);
                    D.X := 0;
                    continue;
                  end;
                if D.Y > Mark.B.Y then
                  break;
                if (D.Y = Mark.B.Y) and (D.Y = Mark.A.Y) or
                    VertBlock
                then
                  if StX < Mark.A.X then
                    StX := Mark.A.X+1;
                if (D.Y = Mark.B.Y) or VertBlock then
                  if TrX > Mark.B.X then
                    TrX := Mark.B.X;
              end;
            if TrX < StX then
              i := 0 {-$VIV 14.05.99}
            else
              {$IFDEF REGEXP} if SearchData.Options and efoRegExp <> 0
            then{поиск регэкспа}
              begin
                i := 0;
                if RegExp^.Execute(PChar(s1)+StX-1, TrX-StX+1) and (
                    RegExp^.FLength <> 0)
                then
                  i := RegExp^.FStart+1
                else if RegExp^.FStatus <> resOK then
                  begin
                    ScrollTo(D.X, D.Y);
                      { go to aborted line                 }
                    goto LExit;
                      { press Ctrl-L to continue searching }
                  end;
              end
            else
              {$ENDIF} if AllCP then
              i := SearchForAllCP(W, s1[StX], TrX-StX+1, SearchData.
                Options and efoCaseSens <> 0)
            else
              i := SearchFor(W, s1[StX], TrX-StX+1, SearchData.
                Options and efoCaseSens <> 0);
          end;

        if (i > 0) and (SearchData.Options and efoWholeWords <> 0)
        then{что-то нашли, надо проверить, отдельное ли это слово}
          if not (((StX-1+i = 1) or (s1[StX-1+i-1] in BreakChars))
              and
            ((StX-1+i+Length(W) > Length(s1)) or (s1[StX-1+i+Length(W)]
              in BreakChars)))
          then
            begin
              if Dir > 0 then
                D.X := StX+i
              else
                D.X := StX+i-1;
              goto 1;
            end;

        if i > 0 then{что-то нашли, надо нарисовать это на экране}
          begin
            Search := True;
            F := False;
            DD.Y := D.Y;
            DD.X := StX+i-2;
            D := DD;
            if Dir > 0 then
              {$IFDEF REGEXP}
              if SearchData.Options and efoRegExp <> 0 then
                Inc(D.X, RegExp^.FLength)
              else
                {$ENDIF}
                Inc(D.X, Length(W));
            Delta := D;
            HScroll^.Value := D.X;
            VScroll^.Value := D.Y;
            if TimerExpired(tmr) then
              begin
                VScroll^.DrawView;
                NewTimer(tmr, 1);
              end;
            if (D.Y-Pos.Y > Size.Y) then
              Pos.Y := D.Y-Size.Y div 2
            else if (D.Y < Pos.Y) then
              Pos.Y := Max(0, D.Y-5);
            if (D.X-Pos.X > Size.X-10) then
              Pos.X := D.X-Size.X div 2
            else if (D.X < Pos.X) then
              Pos.X := Max(0, D.X-5);
            if (SearchData.What = #0) or Prompt
            then
              _DrawViews;
            if SearchData.What <> #0 then
              begin
                j := cmYes;
                if Prompt then
                  begin
                    i := 50;
                    t.A.Y := Delta.Y-Pos.Y;
                    MakeGlobal(t.A, t.A);
                    Desktop^.MakeLocal(t.A, t.A);
                    t.A.X := (Desktop^.Size.X-i) div 2;
                    t.B.X := t.A.X+i;
                    if (t.A.Y <= (Desktop^.Size.Y) div 2) then
                      t.A.Y := (Desktop^.Size.Y+4) div 2
                    else
                      t.A.Y := (Desktop^.Size.Y-18) div 2;
                    t.B.Y := t.A.Y+8;
                    j := MessageBoxRect(t, GetString(dlQueryReplace),
                      nil,
                    mfQuery+mfYesButton+mfAllButton+mfNoButton+
                      mfCancelButton);
                    if j = cmOK then
                      begin
                        j := cmYes;
                        ReplaceAll := True;
                        Prompt := False;
                      end;
                  end;
                if j = cmYes then
                  begin
                    Modified := True;
                    Inc(NumRep); {piwamoto}
                    {$IFDEF REGEXP}
                    if SearchData.Options and efoRegExp <> 0 then
                      s := Copy(s1, DD.X+1, RegExp^.FLength)
                    else
                      {$ENDIF}
                      s := Copy(s1, DD.X+1, Length(W));
                    Insert(Char(Length(SearchData.What)), s, 1);
                    if ReplaceAll then
                      StoreUndoInfo(udReplaceAll, DD, s) {-$VOL}
                    else
                      StoreUndoInfo(udReplace, DD, s);
                    {$IFDEF REGEXP}
                    if SearchData.Options and efoRegExp <> 0 then
                      Delete(s1, DD.X+1, RegExp^.FLength)
                    else
                      {$ENDIF}
                      Delete(s1, DD.X+1, Length(W));
                    Insert(SearchData.What, s1, DD.X+1);
                    ModifyLine(Delta.Y, s1, True);
                    WorkModified := False;
                    if Dir > 0 then
                      {$IFDEF REGEXP}
                      if SearchData.Options and efoRegExp <> 0 then
                        Dec(Delta.X, RegExp^.FLength-Length(
                          SearchData.What))
                      else
                        {$ENDIF}
                        Dec(Delta.X, Length(W)-Length(SearchData.
                          What));
                    if (SearchData.What = #0) or Prompt then
                      ScrollTo(Delta.X, Delta.Y);
                    D := Delta;
                    if (SearchData.What = #0) or Prompt then
                      _DrawViews;
                  end;
                if ReplaceAll then
                  begin{AK155}
                    if j <> cmCancel then
                      goto 1;
                    Mark := OldMark;
                    goto EndReplace;
                  end
                else
                  begin
                    Mark := OldMark;
                    goto LExit;
                  end;
              end
            else
              begin
                Mark := OldMark;
                LastPos.X := -1;
                goto _LExit;
              end;
          end;

        if Dir > 0 then
          begin
            D.X := 0;
            Inc(D.Y);
          end
        else
          begin
            D.X := MaxLongStringLength;
            Dec(D.Y);
          end;
      end;

    if F then
      begin
        SearchActive := False;
        MessageBox(GetString(dlDBViewSearchNot), nil, mfInformation+
          mfOKButton);
      end
    else
      Mark := OldMark;
    UnMark := True;

EndReplace:
    if NumRep > 0 then
      begin
        MessageBox(GetString(dlReplacesMade), @NumRep, mfOKButton+
          mfInformation);
        ScrollTo(Delta.X, Delta.Y);
      end;

LExit:
    _DrawViews;

_LExit:
    SearchActive := False;
  end { TFileEditor.Search };
{/Cat}

procedure TFileEditor.InsertBlock;
  var
    i, Q: longInt;
    s, s1, s2: LongString;
    P: PLongString;
    l, j: longInt;
    LL: PCollection;
  begin
    if (ABlock = nil) or (ABlock^.Count = 0) then
      exit;
    Modified := True;
    if SaveUndo then
      begin
        Mark.B := Delta;
        Mark.A := Delta;
        BlockVisible := True;
        Inc(Mark.B.Y, Max(0, ABlock^.Count-1));
      end;
    if VertBlock then
      begin
        P := ABlock^.At(0);
        if P <> nil then
          s := P^
        else
          s := '';
        i := Length(s);
        for j := 1 to ABlock^.Count do
          begin
            Q := Length(CnvLongString(ABlock^.At(j-1)));
            if Q > i then
              i := Q;
          end;
        Q := i;
        l := longInt(i);
        l := l shl 16;
        l := l or ABlock^.Count;
        if SaveUndo then
          if InsertMode then
            StoreUndoInfo(udInsVertBlock, Delta, l)
          else
            begin
              P := ABlock^.At(0);
              if P = nil then
                s := ''
              else
                s := P^;
              LL := New(PLineCollection, Init(ABlock^.Count, 10,
                True));
              if LL <> nil then
                begin
                  for i := 0 to ABlock^.Count do
                    LL^.Insert(NewLongStr(LongAddSpace(Copy(GetLine(i+
                      Delta.Y), Delta.X+1, Length(s)), i)));
                  StoreUndoInfo(udReplaceBlock, Delta, LL)
                end;
            end;
        for i := 1 to ABlock^.Count do
          begin
            P := ABlock^.At(i-1);
            s1 := LongAddSpace(CnvLongString(P), Q);
            if SaveUndo then
              Mark.B.X := Mark.A.X+Length(s1);
            if i+Delta.Y-1 = FileLines^.Count then
              FileLines^.Insert(NewLongStr(LongAddSpace('', Q)));
            s2 := GetLine(i+Delta.Y-1);
            if not InsertMode then
              Delete(s2, Delta.X+1, Length(s1));
            if Length(s2) < Delta.X then
              s2 := s2+LongStrg(' ', Delta.X-Length(s2));
            Insert(s1, s2, Delta.X+1);
            ModifyLine(i+Delta.Y-1, s2, True);
          end
      end
    else
      begin
        i := ABlock^.Count;
        s := Char(Lo(i))+Char(Hi(i))+GetLine(Delta.Y);
        if SaveUndo then
          StoreUndoInfo(udInsBlock, Delta, s);
        Delete(s, 1, 2);
        if Delta.X > Length(s) then
          s := s+LongStrg(' ', Delta.X-Length(s));
        s1 := Copy(s, 1, Delta.X);
        s2 := Copy(s, Delta.X+1, MaxLongStringLength);
        FileLines^.AtFree(Delta.Y);
        for i := 1 to ABlock^.Count do
          begin
            P := ABlock^.At(i-1);
            if P <> nil then
              s := P^
            else
              s := '';
            if i = 1 then
              s := s1+s;
            if i = ABlock^.Count then
              begin
                if SaveUndo then
                  Mark.B.X := Length(s);
                s := s+s2;
              end;
            KeyMapAtInsert(Delta.Y+i-1, NewLongStr(s)); {-$VIV}
          end;
        j := ABlock^.Count-1;
        if j > 0 then
          for l := 1 to 9 do
            if MarkPos[l].Y >= Delta.Y then
              Inc(MarkPos[l].Y, j);
      end;
    SetLimits;
    if GetState(sfVisible) then
      DrawView;
  end { TFileEditor.InsertBlock };

procedure TFileEditor.Draw;
  var
    C, BC, Comments, C1: byte;
    HP: String[6];
    CC: array[1..12] of byte;
    i, A: longInt;
    s: LongString;
    P: PString;
    WM, BV: boolean;
    X1, X2: longInt;
    LPos: integer;
    Ch: Char;
    B: TDrawBuffer;
    BB: TPoint;
    {$IFDEF PLUGIN}
    Event: TEvent; {Cat}
    FillColorsData: TFillColorsData; {Cat}
    {$ENDIF}

  procedure DrawBlock;
    begin
      X1 := Mark.A.X-Pos.X;
      if X1 < 0 then
        X1 := 0;
      X2 := Mark.B.X-Pos.X;
      if X2 < 0 then
        X2 := 0;
      if VertBlock or (Mark.A.Y = Mark.B.Y) then
        begin
          if (X1 = X2) or (X1 > Size.X) then
            exit;
          if (X2 > Size.X) then
            X2 := Size.X;
          MoveColor(B[X1], X2-X1, BC);
        end
      else
        begin
          if A = Mark.A.Y then
            begin
              if X1 < Size.X then
                MoveColor(B[X1], Size.X-X1, BC)
            end
          else if A = Mark.B.Y then
            begin
              if (X2 > Size.X) then
                X2 := Size.X;
              MoveColor(B, X2, BC);
            end
          else
            MoveColor(B, Size.X, BC);
        end;
    end { DrawBlock };

  begin { TFileEditor.Draw }
    {Cat}
    {$IFDEF PLUGIN}
    Event.What := evCommand;
    Event.Command := 65001;
    ProcessEditorEventHook(Event, @Self);
    {$ENDIF}
    {/Cat}

    BB := Pos;
    SearchOnDisplay := False;
    if Pos.X < 0 then
      Pos.X := 0;
    if Pos.Y < 0 then
      Pos.Y := 0;
    if Delta.X < 0 then
      Delta.X := 0;
    if Delta.X > MaxLongStringLength then
      Delta.X := MaxLongStringLength;
    if Delta.Y >= FileLines^.Count then
      Delta.Y := FileLines^.Count-1;
    if Delta.Y < 0 then
      Delta.Y := 0;
    if Pos.X > Delta.X then
      Pos.X := Delta.X;
    if Delta.X-Pos.X >= Size.X then
      Pos.X := Delta.X-Size.X+8;
    if Pos.Y > Delta.Y then
      Pos.Y := Delta.Y;
    if Delta.Y-Pos.Y >= Size.Y then
      Pos.Y := Delta.Y-Size.Y+1;
    if (BB.X <> Pos.X) or (BB.Y <> Pos.Y) then
      ChPosition := False;
    if Marking then
      begin
        ChPosition := False;
        Sel.B.X := Delta.X;
        Sel.B.Y := Delta.Y;
        if LineMarking and (Sel.B.Y < FileLines^.Count-1) then
          begin
            if Sel.B.Y >= Sel.A.Y then
              Inc(Sel.B.Y);
            Sel.B.X := 0;
          end;
        Mark := Sel;
        if not VertBlock then
          if (Mark.A.Y > Mark.B.Y) or ((Mark.A.Y = Mark.B.Y) and (
              Mark.A.X > Mark.B.X))
          then
            begin
              Mark.A := Sel.B;
              Mark.B := Sel.A;
            end
          else
        else
          begin
            if Mark.A.X > Mark.B.X then
              begin
                Mark.B.X := Mark.A.X;
                Mark.A.X := Sel.B.X;
              end;
            if Mark.A.Y > Mark.B.Y then
              begin
                Mark.B.Y := Mark.A.Y;
                Mark.A.Y := Sel.B.Y;
              end;
          end;
      end;
    BV := BlockVisible and ValidBlock and not SearchActive;
    C := GetColor(1);
    WM := not WorkModified;
    BC := GetColor(2);
    for i := 1 to 12 do
      CC[i] := GetColor(i);
    Comments := GetColor(3);
    {/Cat}
    for i := 0 to Size.Y-1 do
      begin
        A := Pos.Y+i;
        if BV and not VertBlock then
          begin
            if (A = Mark.A.Y) then
              begin
                if WorkModified and (Mark.A.Y = Delta.Y) then
                  if Mark.A.X > Length(WorkString) then
                    Mark.A.X := Length(WorkString)
                  else
                else if Mark.A.X > Length(GetLine(Mark.A.Y)) then
                    Mark.A.X := Length(GetLine(Mark.A.Y));
              end;
            if (A = Mark.B.Y) then
              begin
                if WorkModified and (Mark.B.Y = Delta.Y) then
                  if Mark.B.X > Length(WorkString) then
                    Mark.B.X := Length(WorkString)
                  else
                else if Mark.B.X > Length(GetLine(Mark.B.Y)) then
                    Mark.B.X := Length(GetLine(Mark.B.Y));
              end;
          end;
        if ChPosition and not ((A = Delta.Y) or (A = LastPos.Y))
        then
          continue;
        if EdOpt.HiliteLine
        then
          if A = Delta.Y then
            begin
              C := CC[4];
              BC := CC[5];
            end
          else
            begin
              C := CC[1];
              BC := CC[2];
            end;
        MoveChar(B, ' ', C, Size.X);
        if A < FileLines^.Count then
          begin
            if WM or (A <> Delta.Y) then
              s := GetLine(A)
            else
              s := WorkString;
            MoveStr(B, Copy(s, Pos.X+1, Size.X), C);
            if EdOpt.HiLite then
              begin
                {Cat}
                {$IFDEF PLUGIN}
                Event.What := evCommand;
                Event.Command := 65002;
                Event.InfoPtr := @FillColorsData;
                FillColorsData.DrawBuffer := @B;
                FillColorsData.StrNum := A;
                FillColorsData.StartPos := Pos.X+1;
                FillColorsData.EndPos := Pos.X+Size.X+1;
                if not ProcessEditorEventHook(Event, @Self) then
                  {$ENDIF}
                  {/Cat}
                  begin
                    SetLength(s, Length(s));
                      {Cat: ...Kreks...Feks...Peks...Suxx...}
                    highlite.Highlites(Length(s), PChar(s),
                      HiLitePar); {PZ 2000.04.28}
                    C1 := C and $F0; { Background }
                    SetLength(HP, 6); { Attr. size }
                    if (A = Delta.Y) and (EdOpt.HiliteLine)
                    then
                      HP[Ord(hhComment)] := Chr(CC[6])
                        { Comments for current line}
                    else
                      HP[Ord(hhComment)] := Chr(Comments);
                      { Comments   }
                    HP[Ord(hhNumber)] := Chr(C1 or (CC[10] and 15));
                      { Numbers    }
                    HP[Ord(hhString)] := Chr(C1 or (CC[9] and 15));
                      { Strings    }
                    HP[Ord(hhSymbol)] := Chr(C1 or (CC[8] and 15));
                      { Symbols    }
                    HP[Ord(hhKeyword1)] := Chr(C1 or (CC[11] and 15));
                      { Keywords 1 }
                    HP[Ord(hhKeyword2)] := Chr(C1 or (CC[12] and 15));
                      { Keywords 2 }
                    DoHighlite(B, s, HP);
                  end;
              end;
            if RulerVisible and (A = Delta.Y)
            then
              begin
                FreeStr := '123456789';
                Ch := '1';
                while CStrLen(FreeStr) < Size.X do
                  begin
                    FreeStr := FreeStr+'~'+Ch+'~'+Copy(FreeStr, 1, 9);
                    Inc(Ch);
                    if Ch > '9' then
                      Ch := '0';
                  end;
                MoveCStr(B[Delta.X-Pos.X], FreeStr, Application^.
                  GetColor($4240));
              end;
          end;
        if BV and (A >= Mark.A.Y) and (A <= Mark.B.Y) then
          DrawBlock;
        { regular expression search }
        if SearchActive and (A = Delta.Y) then
          begin
            X1 := Delta.X-Pos.X;
            {$IFDEF REGEXP}
            if ((SearchData.Options and efoRegExp) <> 0) and (
                SearchData.What = #0)
            then
              begin
                X2 := RegExp^.FLength;
                Dec(X1, X2);
              end
            else
              {$ENDIF}
              begin
                X2 := Length(SearchData.Line);
                if SearchData.Dir = 0 then
                  Dec(X1, X2);
              end;
            if X1 < 0 then
              begin
                Inc(X2, X1);
                X1 := 0;
              end;
            MoveColor(B[X1], X2, BC);
            SearchOnDisplay := True;
          end;
        if EdOpt.HiliteColumn then
          WordRec(B[Delta.X-Pos.X]).Hi := CC[7];
        {$IFDEF OS2}
        if PMWindowed and (A = Delta.Y) then
            {JO: чтобы решить пробему курсора}
          begin{    на сером фоне в окне}
            if (WordRec(B[Delta.X-Pos.X]).Hi and $F0 = $80) then
                WordRec(B[Delta.X-Pos.X]).Hi := WordRec(B[Delta.X-
                Pos.X]).Hi and $0F;
            if (WordRec(B[Delta.X-Pos.X]).Hi and $FF = 0) then
                WordRec(B[Delta.X-Pos.X]).Hi := $07;
          end;
        {$ENDIF}
        WriteLine(0, i, Size.X, 1, B);
      end;
    SetCursor(Delta.X-Pos.X, Delta.Y-Pos.Y);
    if InsertMode xor (InterfaceData.Options and
        ouiBlockInsertCursor <> 0)
    then
      NormalCursor
    else
      BlockCursor;
    ShowCursor;
    LastPos.X := Delta.X;
    LastPos.Y := Delta.Y;
    ChPosition := False;
    if (InfoL <> nil) then
      InfoL^.Draw;
    if (BMrk <> nil) then
      BMrk^.Draw;
  end { TFileEditor.Draw };

function TFileEditor.GetLine(Index: longInt): LongString;
  (*
 var p: PLongString;
     s: LongString;
begin
 if (Index < FileLines^.Count) and (Index >= 0) then p := FileLines^.At(Index)
                                                else p := Nil;
 if p <> Nil then s := p^ else s := ''; {-$VIV* START}
 s := KeyMapConvertStr(s, true);
 GetLine := s;                          {-$VIV E-ND}
end;
*)
  {Cat}
  begin
    if (FileLines <> nil) and (Index < FileLines^.Count) and (Index >= 0
        )
    then
      GetLine := KeyMapConvertStr(CnvLongString(FileLines^.At(Index)),
        True)
    else
      GetLine := '';
  end;
{/Cat}

procedure TFileEditor.ModifyLine(Index: longInt; s: LongString;
    DelSpaces: boolean);
  begin
    if (Index < 0) or (Index >= FileLines^.Count) then
      exit;
    if DelSpaces then
      LongDelRight(s);
    KeyMapAtReplace(Index, NewLongStr(s)); {-$VIV}
  end;

function TFileEditor.HandleCommand;
  begin
  end;

procedure TFileEditor.HandleEvent;
  label 1, 2;
  var
    ChPos, WM, DelWord, WasMA, WasMB: boolean;
    P: PLongString;
    PL: PCollection;
    s: LongString;
    LastY, LastX, i: longInt;
    OldDelta,
    t: TPoint;
    l: longInt;

  procedure BlockOff;
    begin
      UnMark := True;
      Marking := False;
      ChPosition := False;
      if EditorDefaults.EdOpt and ebfPBl <> 0 then
        exit;
      Mark.B := Mark.A;
      BlockVisible := False;
    end;

  procedure InsertSpace;
    var
      P: TPoint;
    begin
      P.X := LastX;
      P.Y := LastY;
      if InsertMode then
        begin
          if LastX < Length(WorkString) then
            Insert(' ', WorkString, LastX+1)
          else
            WorkString := WorkString+LongStrg(' ', LastX-Length(
              WorkString)+1);
          StoreUndoInfo(udInsChar, P, P);
        end
      else
        begin
          if LastX < Length(WorkString) then
            begin
              P.X := LastX;
              StoreUndoInfo(udReplaceChar, P, WorkString[LastX+1]);
              WorkString[LastX+1] := ' ';
            end;
        end;
      Inc(LastX);
    end { InsertSpace };

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

  procedure MakeEnter;
    var
      i, j: longInt;
      OldS, s1, s2: LongString;
      bl: boolean;
      WBY, WAY: boolean;
      l: longInt;
    begin
      BlockOff;
      bl := ValidBlock and not VertBlock;
      if not InsertMode then
        begin
          ChangeLine;
          if Delta.Y = FileLines^.Count-1 then
            begin
              FileLines^.Insert(nil);
              SetLimits;
            end;
          ScrollTo(0, Delta.Y+1);
        end
      else
        begin
          ChangeLine;
          Modified := True;
          WorkModified := False;
          OldS := WorkString;
          WAY := bl and (Delta.Y = Mark.A.Y) and (Delta.X <= Mark.A.X);
          WBY := bl and (Delta.Y = Mark.B.Y) and (Delta.X <= Mark.B.X);
          StoreUndoInfo(udInsLine, Delta, WorkString);
          while (WorkString <> '') and (WorkString[Length(WorkString)] =
              ' ')
          do
            SetLength(WorkString, Length(WorkString)-1);
          s1 := WorkString;
          s2 := Copy(s1, 1, LastX);
          while (s2 <> '') and (s2[Length(s2)] = ' ') do
            SetLength(s2, Length(s2)-1);
          WorkString := s2;
          Delete(s1, 1, LastX);
          ModifyLine(LastY, s2, True);
          WorkString := s1;
          if EdOpt.AutoIndent then
            begin
              i := LastY-1;
              while (WorkString <> '') and (WorkString[1] = ' ') do
                begin
                  Delete(WorkString, 1, 1); {DelFC(WorkString);}
                  if WAY then
                    Dec(Mark.A.X);
                  if WBY then
                    Dec(Mark.B.X);
                end;
              if DelSpaces(s2) = '' then
                s2 := OldS;
              {While (S2 = '') and (I >= 0) do
          if I >= 0 then
           begin S2 := GetLine(I); Dec(I) end;}
              LastX := 0;
              while (LastX < Length(s2)) and (s2[LastX+1] = ' ')
              do
                begin
                  Inc(LastX);
                  WorkString := ' '+WorkString;
                  if WAY then
                    Inc(Mark.A.X);
                  if WBY then
                    Inc(Mark.B.X);
                end;
            end
          else
            LastX := 0;
          KeyMapAtInsert(LastY+1, NewLongStr(WorkString)); {-$VIV}
          SetLimits;
          Delta.X := LastX;
          Inc(Delta.Y);
        end;
      for l := 1 to 9 do
        if MarkPos[l].Y >= Delta.Y then
          Inc(MarkPos[l].Y);
    end { MakeEnter };

  procedure MakeBack;
    var
      i, j: longInt;
      P: TPoint;
      l: longInt;
    begin
      WorkModify;
      BlockOff;
      Modified := True;
      ClearEvent(Event);
      if LastX = 0 then
        begin
          if (LastY = 0) or not InsertMode then
            exit;
          EnableMarking := False;
          FileLines^.AtFree(LastY);
          s := GetLine(LastY-1);
          P.X := Length(s);
          P.Y := Delta.Y-1;
          StoreUndoInfo(udSubDelLine, P, WorkString);
          WorkString := s+WorkString;
          ModifyLine(LastY-1, WorkString, True);
          WorkModified := False;
          WorkModify;
          WorkModified := False;
          Delta.X := Length(s);
          ScrollTo(Delta.X, Delta.Y-1);
          SetLimits;
          ChangeLine;
          DrawView;
          EnableMarking := True;
          for l := 1 to 9 do
            if MarkPos[l].Y >= Delta.Y then
              Dec(MarkPos[l].Y);
          exit;
        end;
      EnableMarking := False;
      if Length(WorkString) < LastX then
        WorkString := LongAddSpace(WorkString, LastX+1);
      {Cat:warn проверить, нет ли тут ошибки}
      if EdOpt.BackIndent and ((WorkString[LastX+1] <> ' ') or (
          DelSpaces(WorkString) = ''))
        and (Copy(WorkString, 1, LastX) = LongStrg(' ', LastX))
        and (LastY > 0)
      then
        begin
          for i := LastY-1 downto 0 do
            begin
              s := GetLine(i);
              j := 0;
              while (j < Length(s)) and (s[j+1] = ' ') do
                Inc(j);
              if j < LastX then
                if Copy(s, 1, LastX) > LongStrg(' ', LastX) then
                  begin
                    t.X := LastX;
                    t.Y := LastY;
                    StoreUndoInfo(udSubDel, t, WorkString);
                    Delete(WorkString, 1, LastX);
                    LastX := j;
                    Insert(LongStrg(' ', j), WorkString, 1);
                    ScrollTo(LastX, Delta.Y);
                    ChangeLine;
                    DrawView;
                    EnableMarking := True;
                    exit;
                  end;
            end;
        end;
      t.X := LastX;
      t.Y := LastY;
      if LastX <= Length(WorkString) then
        if InsertMode then
          begin
            StoreUndoInfo(udBackDel, t, WorkString[LastX]);
            Delete(WorkString, LastX, 1);
          end
        else
          begin
            t.X := LastX-1;
            StoreUndoInfo(udReplaceChar, t, WorkString[LastX]);
            WorkString[LastX] := ' ';
          end;
      ScrollTo(Delta.X-1, Delta.Y);
      ChangeLine;
      EnableMarking := True;
    end { MakeBack };

  procedure BlockDelete(ChangePos: boolean);
    var
      i, j: longInt;
      s, s1, s2: LongString;
      l, k: longInt;
    begin
      Modified := True;
      if VertBlock then
        begin
          if ChangePos and InsertMode then
            begin
              if (Delta.Y >= Mark.A.Y) and (Delta.X >= Mark.A.X) and
                (Delta.Y <= Mark.B.Y)
              then
                begin
                  Delta.X := Max(Mark.A.X, Delta.X-(Mark.B.X-Mark.A.X));
                end;
            end;
          for i := Mark.A.Y to Mark.B.Y do
            begin
              s := GetLine(i);
              Delete(s, Mark.A.X+1, Mark.B.X-Mark.A.X);
              if not InsertMode then
                Insert(LongStrg(' ', Mark.B.X-Mark.A.X), s, Mark.A.X+1);
              ModifyLine(i, s, True);
            end;
        end
      else
        begin
          if ChangePos then
            begin
              if (Delta.Y = Mark.A.Y) then
                begin
                  if (Delta.X > Mark.A.X) then
                    if (Delta.Y = Mark.B.Y) and (Delta.X > Mark.B.X)
                    then
                      Dec(Delta.X, Mark.B.X-Mark.A.X)
                    else
                      Delta.X := Mark.A.X;
                end
              else if (Delta.Y = Mark.B.Y) then
                begin
                  Dec(Delta.X, Mark.B.X);
                end
              else if Delta.Y < Mark.A.Y then
              else if Delta.Y > Mark.B.Y then
                Dec(Delta.Y, Mark.B.Y-Mark.A.Y)
              else
                Delta := Mark.A;
            end;
          ModifyLine(Mark.A.Y, Copy(GetLine(Mark.A.Y), 1, Mark.A.X)+
          Copy(GetLine(Mark.B.Y), Mark.B.X+1, MaxLongStringLength),
            True);
          for i := Mark.A.Y to Mark.B.Y-1 do
            FileLines^.AtFree(Mark.A.Y+1);
          k := Mark.B.Y-Mark.A.Y-1;
          if k > 0 then
            for l := 1 to 9 do
              if MarkPos[l].Y >= Delta.Y then
                Dec(MarkPos[l].Y, k);
        end;
    end { BlockDelete };

  procedure MakeSmallBack;
    var
      t: TPoint;
    begin
      if LastX > 0 then
        begin
          t.X := LastX;
          t.Y := LastY;
          StoreUndoInfo(udBackDel, t, WorkString[LastX]);
          Delete(WorkString, LastX, 1);
        end;
      Dec(LastX);
    end;

  procedure DeleteBlock(ChangePos: boolean);
    var
      l: PCollection;
    label l1;
    begin
      if not BlockVisible then
        exit;
      EnableMarking := False;
      Marking := False;
      ChangeLine;
      l := GetSelection;
      if (l = nil) then
        begin
          if MemEnough then
            exit;
          if UndoInfo <> nil then
            UndoInfo^.FreeAll;
          l := GetSelection;
          if (l = nil) and (not MemEnough) then
            goto l1;
        end;

      if VertBlock and not InsertMode then
        StoreUndoInfo(udClearBlock, Mark.A, l)
      else
        StoreUndoInfo(udDelBlock, Mark.A, l);
l1:
      BlockDelete( {not ChangePos}True);
      if ChangePos then
        begin
          Delta := Mark.A;
          Mark.A := Mark.B;
        end;
      LastX := Delta.X;
      LastY := Delta.Y;
      BlockOff;
      SetLimits;
      ScrollTo(LastX, LastY);
      DrawView;
      ChangeLine;
      EnableMarking := True;
    end { DeleteBlock };

  procedure BMarking;
    begin
      if not EnableMarking or (DrawMode > 0) then
        exit;
      if (ShiftState and 3 <> 0) and not MouseMark then
        begin
          if not Marking then
            begin
              Sel.A := LastPos;
              Sel.B := Delta
            end
          else
            Sel.B := Delta;
          Marking := True;
          BlockVisible := True;
        end
      else
        begin
          Marking := MouseMark;
          if Marking then
            Sel.B := Delta;
        end;
      if not Marking and UnMark then
        BlockOff;
    end { BMarking };

  procedure MakeDel;
    var
      P: TPoint;
      s: LongString;
      l: longInt;
    begin
      WorkModify;
      ChangeLine;
      BlockOff;
      Modified := True;
      if LastX < Length(WorkString) then
        begin
          WorkModify;
          P.X := LastX;
          P.Y := LastY;
          StoreUndoInfo(udDelChar, P, WorkString[LastX+1]);
          Delete(WorkString, LastX+1, 1);
          exit
        end;
      if LastY+1 >= FileLines^.Count then
        exit;
      WorkModify;
      P.X := LastX;
      P.Y := LastY;
      s := GetLine(LastY+1);
      if LastY < FileLines^.Count then
        StoreUndoInfo(udSubDelLine, P, s);
      WorkString := WorkString+LongStrg(' ', LastX-Length(WorkString))+s
        ;
      ChangeLine;
      FileLines^.AtFree(LastY+1);
      for l := 1 to 9 do
        if MarkPos[l].Y >= LastY then
          Dec(MarkPos[l].Y);
      SetLimits;
    end { MakeDel };

  procedure PasteBlock;
    begin
      if SystemData.Options and ossUseSysClip <> 0 then
        SyncClipOut {(true)};
      EnableMarking := False;
      Marking := False;
      ChangeLine;
      if EditorDefaults.EdOpt and (ebfPBl+ebfObl) = ebfObl
      then
        DeleteBlock(True);
      BlockOff;
      InsertBlock(ClipBoard, True);
      ChangeLine;
      EnableMarking := True;
    end;

  procedure CopyBlock;
    var
      R: TRect;
    begin
      ChangeLine;
      if (ClipBoard <> nil) then
        Dispose(ClipBoard, Done);
      ClipBoard := GetSelection;
      if SystemData.Options and ossUseSysClip <> 0 then
        SyncClipIn;

      {AK155: не понял, зачем вообще нужен ClipBoardStream. Используется
он в команде просмотра Clipbioard, но зачем его из коллекции нужно
гонять в Stream, а потом обратно - непонятно. А тормозит он чудовищно.
Поэтому пока что ограничиваю размер, в результате чего просмотр Clipboard
при отключенном Use system clipboard для очень больших фрагментов
работать временно не будет }
      if (ClipBoardStream <> nil)
        {AK155} and (ClipBoard^.Count < 1000) {/AK155}
      then
        ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-4, 0));
      CopyLines2Stream(ClipBoard, ClipBoardStream);
    end;

  procedure CenterScreen;
    begin
      Pos.X := Delta.X-Size.X div 2;
      Pos.Y := Delta.Y-Size.Y div 2;
      DrawView;
    end;

  procedure DeleteLine;
    var
      l: longInt;
    begin
      Modified := True;
      t.Y := LastY;
      t.X := LastX;
      if Delta.Y+1 = FileLines^.Count then
        begin
          StoreUndoInfo(udSubDel, t, WorkString);
          ModifyLine(Delta.Y, '', True);
          SetLength(WorkString, 0);
        end
      else
        begin
          StoreUndoInfo(udDelLine, t, WorkString);
          WorkModified := False;
          FileLines^.AtFree(Delta.Y);
          ChangeLine
        end;
      for l := 1 to 9 do
        if MarkPos[l].Y >= Delta.Y then
          Dec(MarkPos[l].Y);
      SetLimits;
      ScrollTo(0, Delta.Y);
      DrawView;
      ChangeLine;
    end { DeleteLine };

  {Cat: переписал для устранения проблем со стеком при рекурсии,
      ускорения работы и ликвидации некоторых глюков}

  (*
 procedure WordLeft;
 begin
  if LastX > 0 then
   begin
    if LastX > Length(WorkString) then LastX := Length(WorkString);
    while (LastX >= 0) and (WorkString[LastX] in BreakChars) do Dec(LastX);
    while (LastX > 0) and not (WorkString[LastX] in BreakChars) do Dec(LastX);
    if (LastX >= 0) and not (WorkString[LastX+1] in BreakChars) then Exit;
   end;
  if LastY <= 0 then begin LastX := 0; Exit end;
  Dec(Delta.Y); Dec(LastY); ChangeLine; LastX := Length(WorkString); Delta.X := Length(WorkString); WordLeft;
 end;

 procedure WordRight;
  var B: Boolean;
 begin
  B := LastX < Length(WorkString);
  while (LastX < Length(WorkString)) and not (WorkString[LastX+1] in BreakChars) do Inc(LastX);
  while (LastX < Length(WorkString)) and (WorkString[LastX+1] in BreakChars) do Inc(LastX);
  if (LastX < Length(WorkString)) or B and (LastX = Length(WorkString)) then Exit;
  if LastY + 1 >= FileLines^.Count then begin LastX := Length(WorkString); Exit end;
  Inc(Delta.Y); Inc(LastY); LastX := 0; Delta.X := 0; ChangeLine;
  if (Length(WorkString) > 0) and not (WorkString[1] in BreakChars) then Exit;
  WordRight;
 end;
*)

  procedure WordLeft;

    procedure JumpUp;
      begin
        Dec(LastY);
        Dec(Delta.Y);
        ChangeLine;
        LastX := Length(WorkString);
        Delta.X := Length(WorkString);
      end;

    begin
      if (LastX = 0) and (LastY = 0) then
        exit;
      if LastX > Length(WorkString) then
        LastX := Length(WorkString);

      while LastY >= 0 do
        if WorkString = '' then
          JumpUp
        else
          begin
            while (LastX > 0) and (WorkString[LastX] in BreakChars) do
              Dec(LastX);
            if LastX = 0 then
              JumpUp
            else
              break;
          end;

      if LastY >= 0 then
        while (LastX > 0) and not (WorkString[LastX] in BreakChars) do
          Dec(LastX)
        else
        LastY := 0;
    end { WordLeft };

  procedure WordRight;

    procedure JumpDown;
      begin
        Inc(LastY);
        Inc(Delta.Y);
        ChangeLine;
        LastX := 0;
        Delta.X := 0;
      end;

    begin
      if (LastY = FileLines^.Count-1) and (LastX >= Length(GetLine(
          LastY)))
      then
        exit;

      if LastX >= Length(WorkString) then
        JumpDown;

      while (LastX < Length(WorkString)) and not (WorkString[LastX+1] in
          BreakChars)
      do
        Inc(LastX);

      while LastY < FileLines^.Count do
        if WorkString = '' then
          JumpDown
        else
          begin
            while (LastX < Length(WorkString)) and (WorkString[LastX+1]
                in BreakChars)
            do
              Inc(LastX);
            break;
          end;

      if LastY >= FileLines^.Count then
        begin
          LastY := FileLines^.Count-1;
          LastX := Length(GetLine(LastY));
        end;
    end { WordRight };
  {/Cat}

  procedure InputChar; forward;

  procedure MakeTab;
    begin
      TabStep := StoI(EditorDefaults.TabSize);
      if TabStep = 0 then
        TabStep := 8;
      WorkModify;
      repeat
        InsertSpace;
      until (LastX) mod TabStep = 0;
      ScrollTo(LastX, Delta.Y);
      DrawView;
    end;

  procedure DoTab;
    begin
      Modified := True;
      {-$VOL begin}
      if not TabReplace then
        begin
          Event.CharCode := #09;
          InputChar;
        end {-$VOL end}
      else if EdOpt.SmartTab then
        begin
          if LastY > 0 then
            s := GetLine(LastY-1)
          else
            s := '';
          if (LastY = 0) then
            MakeTab
          else if (LastY > 0) and (LastX < Length(s)) then
            begin
              WorkModify;
              repeat
                InsertSpace
              until (s[LastX+1] = ' ') or (LastX >= Length(s));
              {WM := (s[LastX + 2] = ' ') and (LastX < Length(s));}
              while (s[LastX+2] = ' ') and (LastX < Length(s)) do
                  InsertSpace;
              {if WM then }InsertSpace;
              ScrollTo(LastX, Delta.Y);
              DrawView;
            end
          else
            MakeTab;
        end
      else
        MakeTab;
    end { DoTab };

  {-DataCompBoy-}
  procedure BlockRead;
    var
      P: PCollection;
      s: String;
    begin
      ChangeLine;
      s := GetFileNameDialog(x_x, GetString(dlPasteFromTitle),
      GetString(dlPasteFromLabel),
      fdOKButton+fdHelpButton, hsEditPasteFrom);
      if s = '' then
        exit;
      s := lFExpand(s);
      P := MIReadBlock(@Self, s, False);
      if not isValid then
        begin
          isValid := True;
          exit;
        end;
      VertBlock := False;
      if P <> nil then
        begin
          InsertBlock(P, True);
          Dispose(P, Done)
        end;
    end { BlockRead };
  {-DataCompBoy-}

  {-DataCompBoy-}
  procedure BlockWrite;
    var
      P: PCollection;
      PS: PLineCollection {PStdCollector}; {-SBlocks}
      s, SST: LongString;
      R: PStream;
      i, j, k: longInt;
      CrLf: String[2];
      VB: boolean;
      PI: PView;
      A: word;

      {Cat: эта процедура теперь умеет работать с длинными строками
      и находится в модуле Advance1}
      (*
  procedure CompressString;
  var PP: Pointer;
      TSt: Integer;
  begin
   PP := @SST;
   TSt := StoI(EditorDefaults.TabSize);
   if TSt = 0 then TSt := 8;
   {$IFNDEF BIT_32}
   asm
      les bx, PP
      mov cl, es:[bx]
      inc bx
      xor ch, ch
      jcxz @@Ex
      xor di, di
      xor si, si
      mov byte ptr es:[bx-1], ch
    @@1:
      mov ah, byte ptr TSt
      xor dx, dx
    @@2:
      mov al, es:[bx][si]
      mov es:[bx][di], al
      inc si
      cmp si, cx
      ja  @@Ex
      inc di
      inc byte ptr es:[bx-1]
      cmp al, ' '
      jne @@3
      inc dl
      jmp @@4
     @@3:
      xor Dl, dl
     @@4:
      dec ah
      jnz @@2
      or  dl, dl
      jz @@5
      dec dl
      jz @@5
      sub di, dx
      sub byte ptr es:[bx-1], dl
      mov al, 9
      mov es:[bx][di-1], al
     @@5:
      jmp @@1
    @@Ex:
   end;
   {$ELSE BIT_32}
   asm
      push ebx
      push edx
      push edi
      push esi
      mov ebx, PP
      xor ecx, ecx
      mov cl, [ebx]
      inc ebx
      jcxz @@Ex
      xor edi, edi
      xor esi, esi
      mov byte ptr [ebx-1], ch
    @@1:
      mov ah, byte ptr TSt
      xor edx, edx
    @@2:
      mov al, [ebx+esi]
      mov [ebx+edi], al
      inc esi
      cmp esi, ecx
      ja  @@Ex
      inc edi
      inc byte ptr [ebx-1]
      cmp al, ' '
      jne @@3
      inc dl
      jmp @@4
     @@3:
      xor dl, dl
     @@4:
      dec ah
      jnz @@2
      or  dl, dl
      jz @@5
      dec dl
      jz @@5
      sub edi, edx
      sub byte ptr [ebx-1], dl
      mov al, 9
      mov [ebx+edi-1], al
     @@5:
      jmp @@1
    @@Ex:
      pop esi
      pop edi
      pop edx
      pop ebx
   end;
   {$ENDIF}
  end;
*)

    var
      qwe: byte;
    begin
      if not (BlockVisible and ValidBlock) then
        exit;
      ChangeLine;
      s := GetFileNameDialog(x_x, GetString(dlCopyTo),
      GetString(dlFileName),
      fdOKButton+fdHelpButton, hsEditPasteFrom);
      if s = '' then
        exit;
      s := lFExpand(s);
      A := GetFileAttr(s+#0);
      R := CheckForOver(s);
      if R = nil then
        exit;

      VB := VertBlock or (Mark.A.Y = Mark.B.Y);

      case EdOpt.ForcedCRLF of
        cfCRLF:
          CrLf := #13#10;
        cfCR:
          CrLf := #13;
        cfLF:
          CrLf := #10;
        else
          begin
            EdOpt.ForcedCRLF := cfNone;
            for qwe := 0 to EditorDefaults.NewLine do
              EdOpt.ForcedCRLF := succ(EdOpt.ForcedCRLF);
            if EditorDefaults.NewLine = 1 then
              CrLf := #13
            else if EditorDefaults.NewLine = 2 then
              CrLf := #10
            else
              CrLf := #13#10;
          end;
      end {case};

      PI := WriteMsg(^M^M^C+GetString(dlWritingFile));

      for i := Mark.A.Y to Mark.B.Y do
        begin
          if VB then
            SST := Copy(GetLine(i), Mark.A.X+1, Mark.B.X-Mark.A.X)
          else if i = Mark.A.Y then
            SST := Copy(GetLine(i), Mark.A.X+1, MaxLongStringLength)
          else if i = Mark.B.Y then
            SST := Copy(GetLine(i), 1, Mark.B.X)
          else
            SST := GetLine(i);
          if OptimalFill then
            CompressString(SST);
          R^.Write(SST[1], Length(SST));
          if (i <> Mark.B.Y)
          then
            R^.Write(CrLf[1], Length(CrLf));
        end;

      if PI <> nil then
        PI^.Free;

      Dispose(R, Done);
      if (A <> 0) and (A <> $FFFF) then
        SetFileAttr(s+#0, A);

      s := GetPath(lFExpand(s));
      GlobalMessage(evCommand, cmRereadDir, @S);
    end { BlockWrite };
  {-DataCompBoy-}

  procedure CE;
    begin
      if Event.What <> evNothing then
        ClearEvent(Event)
    end;
  procedure CED;
    begin
      ClearEvent(Event);
      DrawView
    end;

  var
    CFind: boolean;
    EvStr: array[1..2] of Char;
    PP: TPoint;

  procedure SplitString;
    var
      s: LongString;
      i, OldX: longInt;
      P: PLineCollection;
      TP: TPoint;
    begin
      ChangeLine;
      s := '';
      while (WorkString <> '') and (WorkString[Length(WorkString)] =
          ' ')
      do
        begin
          SetLength(WorkString, Length(WorkString)-1);
          if Length(WorkString) < LastX+1 then
            LastX := Length(WorkString)-1;
        end;
      OldX := 0;
      if Length(WorkString)-1 = LastX then
        OldX := 1
      else if Length(WorkString)-1 < LastX then
        OldX := 0
      else
        OldX := 1;
      if Length(WorkString) > EdOpt.RightSide then
        begin
          New(P, Init(1, 1, True));
          P^.Insert(NewLongStr(WorkString));
          TP.Y := Delta.Y;
          TP.X := 0;
          StoreUndoInfo(udDelBlock, TP, P);
          while (Length(WorkString) > 1) and (Length(WorkString) >
              EdOpt.RightSide)
          do
            begin
              s := WorkString[Length(WorkString)]+s;
              SetLength(WorkString, Length(WorkString)-1);
            end;
          while (Length(WorkString) > 1) and not (WorkString[Length(
              WorkString)] in [' ', ',', ':', '.', '?', '!', '+',
              ';'])
          do
            begin
              s := WorkString[Length(WorkString)]+s;
              SetLength(WorkString, Length(WorkString)-1);
            end;
          if DelSpaces(WorkString) = '' then
            begin
              WorkString := WorkString+s;
              s := '';
            end
          else
            OldX := Delta.X-Length(WorkString)+OldX;
          if Delta.X > Length(WorkString) then
            Delta.X := MaxLongStringLength;
          if System.Pos(DelSpaces(WorkString), WorkString) = 0 then
            begin
              while (WorkString <> '') and (WorkString[Length(
                  WorkString)] = ' ')
              do
                SetLength(WorkString, Length(WorkString)-1);
              i := 1;
              if EdOpt.AutoJustify then
                while Length(WorkString) < EdOpt.RightSide do
                  begin
                    while (WorkString[i] = ' ') do
                      Inc(i);
                    while (WorkString[i] <> ' ') and (i < Length(
                        WorkString))
                    do
                      Inc(i);
                    if (i < Length(WorkString)) then
                      begin
                        Insert(' ', WorkString, i);
                        if i <= Delta.X then
                          Inc(Delta.X);
                      end;
                    Inc(i);
                    if i > Length(WorkString) then
                      i := 1;
                  end;
            end;
          LongDelLeft(s);
          s := LongStrg(' ', EdOpt.LeftSide)+s;
        end;
      ModifyLine(LastY, WorkString, True);
      KeyMapAtInsert(LastY+1, NewLongStr(s)); {-$VIV}
      s := #2#0;
      StoreUndoInfo(udFormatBlock, TP, s);
      if Delta.X >= Length(WorkString) then
        begin
          VScroll^.SetParams(LastY+1, 0, FileLines^.Count-1, Size.Y, 1);
          if OldX > 0 then
            HScroll^.SetValue(EdOpt.LeftSide);
          HScroll^.SetValue(EdOpt.LeftSide+OldX);
        end
      else
        begin
          VScroll^.SetRange(0, FileLines^.Count-1);
          HScroll^.SetValue(Delta.X);
        end;
      DrawView;
    end { SplitString };

  procedure InputChar;
    var
      I1, I2, I3, i: byte;
      Ch: Char;
      s1: String[2];
    begin
      Ch := Event.CharCode;
      s1 := Ch;
      if EdOpt.AutoBrackets and ((LastX >= Length(WorkString)) or (
          WorkString[LastX+1] = ' '))
      then
        begin
          for i := 1 to Length(AutoBracketPairs) shr 1
          do
            if Ch = AutoBracketPairs[i*2-1] then
              begin
                s1 := Copy(AutoBracketPairs, i*2-1, 2);
                break
              end;
        end;

      EnableMarking := False;
      if EditorDefaults.EdOpt and (ebfPBl+ebfObl) = ebfObl
      then
        DeleteBlock(True);
      LastY := Delta.Y;
      LastX := Delta.X;
      BlockOff;
      EnableMarking := False;
      t := Delta;
      Inc(t.X);
      if LastX < MaxLongStringLength then
        begin
          WorkModify;
          if InsertMode then
            begin
              if LastX <= Length(WorkString) then
                Insert(s1, WorkString, LastX+1)
              else
                WorkString := WorkString+LongStrg(' ', LastX-Length(
                  WorkString))+s1;
              StoreUndoInfo(udInsChar, Delta, Delta);
              if Length(s1) = 2 then
                StoreUndoInfo(udInsChar, t, t);
            end
          else
            begin
              if LastX >= Length(WorkString) then
                WorkString := WorkString+LongStrg(' ', LastX-Length(
                  WorkString)+1);
              StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX+1]);
              WorkString[LastX+1] := Ch
            end;
          if (not EdOpt.AutoWrap or (LastX < EdOpt.RightSide)) and (
              LastX < MaxLongStringLength)
          then
            HScroll^.SetValue(LastX+1)
          else if LastX < MaxLongStringLength then
            SplitString
          else
            begin
              ChangeLine;
              FileLines^.AtInsert(LastY+1, nil);
              SetLimits;
              ScrollTo(0, LastY+1);
              DrawView;
            end;
        end;
      CE;
      EnableMarking := True;
    end { InputChar };

  function AskSave: word;
    var
      s: Pointer;
      P: String;
    begin
      P := Cut(EditName, 30);
      s := @EditName;
      AskSave := MessageBox(GetString(dlQueryModified), @S,
        mfWarning+mfYesNoCancel);
    end;

  procedure PasteWinBlock;
    begin
      EnableMarking := False;
      Marking := False;
      ChangeLine;
      if EditorDefaults.EdOpt and (ebfPBl+ebfObl) = ebfObl
      then
        DeleteBlock(True);
      BlockOff;

      if GetWinClip(PLineCollection(ClipBoard) {, On}) then
        begin
          InsertBlock(ClipBoard, True);

          ChangeLine;
          EnableMarking := True;
        end;
    end;

  procedure DrawLine(Dir: byte);
    const
      Line00 = '│─└││┌├─┘─┴┐┤┬┼';
      Line11 = '║═╚║║╔╠═╝═╩╗╣╦╬';
      Line01 = '│═╘││╒╞═╛═╧╕╡╤╪';
      Line10 = '║─╙║║╓╟─╜─╨╖╢╥╫';

      UpContact1 = '│├┼┤┌┬┐╞╪╡╒╤╕';
      UpContact2 = '║╠╬╣╔╦╗╟╫╢╓╥╖';
      DownContact1 = '│├┼┤└┴┘╞╪╡╘╧╛';
      DownContact2 = '║╠╬╣╚╩╝╟╫╢╙╨╜';
      LeftContact1 = '─├┼┌└┬┴╟╫╓╙╥╨';
      LeftContact2 = '═╠╬╔╚╦╩╞╪╒╘╤╧';
      RightContact1 = '─┤┼┐┘┬┴╢╫╖╜╥╨';
      RightContact2 = '═╣╬╗╝╦╩╡╪╕╛╤╧';

    var
      _Up, _In, _Down: LongString;
      VL: String[32];
      A, B: word;
      i, j, k: longInt;
      ResChar, C: Char;
      DMode: byte;

    function GetH0: byte;
      begin
        GetH0 := 2*byte((LastX+2 <= Length(_In)) and (PosChar(_In[
          LastX+2], RightContact1) > 0))+
        8*byte((LastX > 0) and (PosChar(_In[LastX], LeftContact1) > 0))
      end;

    function GetH1: byte;
      begin
        GetH1 := 2*byte((LastX+2 <= Length(_In)) and (PosChar(_In[
          LastX+2], RightContact2) > 0))+
        8*byte((LastX > 0) and (PosChar(_In[LastX], LeftContact2) > 0))
      end;

    function GetV0: byte;
      begin
        GetV0 := byte((LastX+1 <= Length(_Up)) and (PosChar(_Up[
          LastX+1], UpContact1) > 0))+
        4*byte((LastX+1 <= Length(_Down)) and (PosChar(_Down[LastX+1],
          DownContact1) > 0))
      end;

    function GetV1: byte;
      begin
        GetV1 := byte((LastX+1 <= Length(_Up)) and (PosChar(_Up[
          LastX+1], UpContact2) > 0))+
        4*byte((LastX+1 <= Length(_Down)) and (PosChar(_Down[LastX+1],
          DownContact2) > 0))
      end;

    function Modify(C: Char; Mask: byte): boolean;
      var
        i: integer;
      begin
        Modify := False;
        VL := Line00;
        i := PosChar(C, VL);
        if i = 0 then
          begin
            VL := Line01;
            i := PosChar(C, VL);
          end;
        if i = 0 then
          begin
            VL := Line10;
            i := PosChar(C, VL);
          end;
        if i = 0 then
          begin
            VL := Line11;
            i := PosChar(C, VL);
          end;
        if not (i in [7, 11, 13, 14, 15]) then
          exit;
        Mask := i and not Mask;
        if Mask = i then
          exit;
        if Mask > 0 then
          ResChar := VL[Mask]
        else
          ResChar := ' ';
        Modify := True;
      end { Modify };

    var
      WasShift: boolean; {-$VIV 18.05.99--}
    begin { DrawLine }
      {-$VIV 18.05.99--}
      WasShift := (ShiftState and kbRightShift > 0) and DrawRShift;
      if WasShift then
        begin
          if DrawMode = 1 then
            DrawMode := 2
          else
            DrawMode := 1;
        end;
      {-$VIV--}
      ChangeLine;
      if ShiftState and 3 <> 0 then
        DMode := 1
      else if ShiftState and 4 <> 0 then
        DMode := 2
      else
        DMode := 0;
      _In := LongAddSpace(WorkString, LastX+2);
      _Up := LongAddSpace(GetLine(LastY-1), LastX+2);
      _Down := LongAddSpace(GetLine(LastY+1), LastX+2);
      if DrawMode > 1 then
        VL := Line00
      else
        VL := Line11;
      if odd(Dir) then
        begin
          if DrawMode > 1 then
            begin
              A := GetV1;
              B := GetH1;
              VL := Line11;
              if A = 0 then
                begin
                  A := GetV0;
                  if A <> 0 then
                    VL := Line01
                end;
            end
          else
            begin
              A := GetV0;
              B := GetH0;
              VL := Line00;
              if A = 0 then
                begin
                  A := GetV1;
                  if A <> 0 then
                    VL := Line10
                end;
            end;
        end
      else
        begin
          if DrawMode > 1 then
            begin
              A := GetV1;
              B := GetH1;
              VL := Line11;
              if B = 0 then
                begin
                  B := GetH0;
                  if B <> 0 then
                    VL := Line10
                end;
            end
          else
            begin
              A := GetV0;
              B := GetH0;
              VL := Line00;
              if B = 0 then
                begin
                  B := GetH1;
                  if B <> 0 then
                    VL := Line01
                end;
            end;
        end;
      if (LastDir <> Dir) then
        case DMode of
          1:
            begin
              if LastDir >= 0 then
                A := A or (1 shl LastDir);
              A := A or B or (1 shl Dir);
              C := VL[A];
              WorkModify;
              if LastX >= Length(WorkString) then
                WorkString := LongAddSpace(WorkString, LastX+1);
              StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX+1]);
              WorkString[LastX+1] := C;
            end;
          2:
            begin
              if LastDir < 0 then
                LastDir := (Dir+2) mod 4;
              A := A or B or (1 shl LastDir) or (1 shl Dir);
              WorkModify;
              if LastX >= Length(WorkString) then
                WorkString := LongAddSpace(WorkString, LastX+1);
              if Modify(_Up[LastX+1], 4) then
                begin
                  Dec(Delta.Y);
                  StoreUndoInfo(udReplaceChar, Delta, _Up[LastX+1]);
                  _Up[LastX+1] := ResChar;
                  ModifyLine(Delta.Y, _Up, True);
                  Inc(Delta.Y);
                end;
              if Modify(_Down[LastX+1], 1) then
                begin
                  Inc(Delta.Y);
                  StoreUndoInfo(udReplaceChar, Delta, _Down[LastX+1]);
                  _Down[LastX+1] := ResChar;
                  ModifyLine(Delta.Y, _Down, True);
                  Dec(Delta.Y);
                end;
              if (LastX > 0) and Modify(WorkString[LastX], 2) then
                begin
                  Dec(Delta.X);
                  StoreUndoInfo(udReplaceChar, Delta, WorkString[
                    LastX]);
                  WorkString[LastX] := ResChar;
                  Inc(Delta.X);
                end;
              StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX+1]);
              WorkString[LastX+1] := ' ';
              if (Length(WorkString) > LastX+1) and (Modify(
                  WorkString[LastX+2], 8))
              then
                begin
                  Inc(Delta.X);
                  StoreUndoInfo(udReplaceChar, Delta, WorkString[
                    LastX+2]);
                  WorkString[LastX+2] := ResChar;
                  Dec(Delta.X);
                end;
            end;
        end {case};
      EnableMarking := False;
      case Dir of
        0:
          ScrollTo(LastX, LastY-1);
        2:
          begin
            if LastY = FileLines^.Count-1 then
              begin
                FileLines^.Insert(nil);
                SetLimits;
              end;
            ScrollTo(LastX, LastY+1);
          end;
        1:
          ScrollTo(LastX+1, LastY);
        3:
          ScrollTo(LastX-1, LastY);
      end {case};
      CED;
      if DMode > 0 then
        LastDir := (Dir+2) mod 4
      else
        LastDir := -1;
      EnableMarking := True;

      if WasShift then{-$VIV 18.05.99--}
        begin
          if DrawMode = 1 then
            DrawMode := 2
          else
            DrawMode := 1;
        end; {-$VIV--}

  end { DrawLine };

  {-DataCompBoy-}
  procedure OpenFileAtCursor;
    var
      Res, s: String;
      i, Min1, Max1, Min2, Max2, Min3, Max3, Min4, Max4, P: integer;
      Q: Pointer;
      Info: PWhileView;
      R: TRect;
    label ex;

    procedure Chk;
      begin
        DelLeft(s);
        DelRight(s);
        if s = '' then
          exit;
        if Res = '' then
          Res := FindFileWithSPF(s, Info);
        if Abort then
          exit;
        if Res = '' then
          Res := FindFileWithSPF(GetName(s), Info);
        if Abort then
          exit;
        if Res = '' then
          Res := FindFileWithSPF(GetSName(s), Info);
        if Abort then
          exit;
        if Res = '' then
          Res := FindFileWithSPF(GetSName(s)+GetExt(EditName), Info);
        if Abort then
          exit;
      end { Chk };

    begin { OpenFileAtCursor }
      if WorkString <> '' then
          {Cat: работаем только на непустой строчке}
        begin
          Res := '';
          s := '';
          R.Assign(0, 0, 20, 7);
          New(Info, Init(R));
          Info^.Write(1, Copy(GetString(dlPleaseStandBy), 4,
            MaxStringLength));
          Application^.Insert(Info);
          Abort := False;

          P := Delta.X+1;

          Min1 := 1;
          for i := P downto 1 do
            if WorkString[i] in IllegalCharSet+['*', '?']
            then
              begin
                Min1 := i+1;
                break
              end;
          DispatchEvents(Info, Abort);
          if Abort then
            goto ex;

          Min2 := 1;
          for i := P downto 1 do
            if WorkString[i] in IllegalCharSet-['\']
            then
              begin
                Min2 := i+1;
                break
              end;
          DispatchEvents(Info, Abort);
          if Abort then
            goto ex;

          Min3 := 1;
          for i := P downto 1 do
            if WorkString[i] in IllegalCharSetDos+['*', '?']
            then
              begin
                Min3 := i+1;
                break
              end;
          DispatchEvents(Info, Abort);
          if Abort then
            goto ex;

          Min4 := 1;
          for i := P downto 1 do
            if WorkString[i] in IllegalCharSetDos-['\']
            then
              begin
                Min4 := i+1;
                break
              end;
          DispatchEvents(Info, Abort);
          if Abort then
            goto ex;

          Max1 := Length(WorkString)+1;
          for i := P to Length(WorkString) do
            if WorkString[i] in IllegalCharSet+['*', '?']
            then
              begin
                Max1 := i;
                break
              end;
          DispatchEvents(Info, Abort);
          if Abort then
            goto ex;

          Max2 := Length(WorkString)+1;
          for i := P to Length(WorkString) do
            if WorkString[i] in IllegalCharSet-['\']
            then
              begin
                Max2 := i;
                break
              end;
          DispatchEvents(Info, Abort);
          if Abort then
            goto ex;

          Max3 := Length(WorkString)+1;
          for i := P to Length(WorkString) do
            if WorkString[i] in IllegalCharSetDos+['*', '?']
            then
              begin
                Max3 := i;
                break
              end;
          DispatchEvents(Info, Abort);
          if Abort then
            goto ex;

          Max4 := Length(WorkString)+1;
          for i := P to Length(WorkString) do
            if WorkString[i] in IllegalCharSetDos-['\']
            then
              begin
                Max4 := i;
                break
              end;
          DispatchEvents(Info, Abort);
          if Abort then
            goto ex;

          Res := '';
          s := Copy(WorkString, Min1, Max1-Min1);
          Chk;
          if Abort then
            goto ex;
          s := Copy(WorkString, Min1, Max2-Min1);
          Chk;
          if Abort then
            goto ex;
          s := Copy(WorkString, Min2, Max1-Min2);
          Chk;
          if Abort then
            goto ex;
          s := Copy(WorkString, Min2, Max2-Min2);
          Chk;
          if Abort then
            goto ex;
          s := Copy(WorkString, Min3, Max3-Min3);
          Chk;
          if Abort then
            goto ex;
          s := Copy(WorkString, Min3, Max4-Min3);
          Chk;
          if Abort then
            goto ex;
          s := Copy(WorkString, Min4, Max3-Min4);
          Chk;
          if Abort then
            goto ex;
          s := Copy(WorkString, Min4, Max4-Min4);
          Chk;
          if Abort then
            goto ex;

          s := Copy(WorkString, Min4, Max4-Min4);
          DelLeft(s);
          DelRight(s);
          if (PosChar('.', s) = 0) then
            s := s+GetExt(EditName);

          if Res = '' then
            begin
              Info^.Hide;
              Res := Cut(s, 20);
              Q := @Res;
              case ExecResource(dlgSrchFailed, Q) of
                cmYes:
                  PDNApplication(Application)^.EditFile(True,
                    SourceDir+'DN.SPF');
                cmNo:
                  PDNApplication(Application)^.EditFile(True, s);
              end
            end
          else
            PDNApplication(Application)^.EditFile(True, Res);

ex:
          Info^.Free;
          Abort := False;
        end;
    end { OpenFileAtCursor };
  {-DataCompBoy-}

  procedure ChangeCharCaseDirect; {-$VIV 21.05.99--}
    var
      Ch: Char;
      s, s2: LongString;
    begin
      s := WorkString;
      if (Delta.X >= 0) and (Delta.X < Length(s)) then
        begin
          WorkModify;
          StoreUndoInfo(udStrModified, Delta, WorkString);
          Ch := s[Delta.X+1];
          if Ch = UpCase(Ch)
          then
            s2 := LowLongStrg(''+Ch)
          else
            s2 := UpLongStrg(''+Ch);
          s[Delta.X+1] := s2[1];
          WorkString := s;
          ChangeLine;
          CED;
          ScrollTo(Delta.X+1, Delta.Y);
        end
      else
        begin
          if Delta.Y < FileLines^.Count-1 then
            ScrollTo(0, Delta.Y+1);
        end;
    end { ChangeCharCaseDirect };

  procedure WordEnd;
    var
      s: LongString;
      NewX: integer;
    begin
      s := WorkString;
      if (Delta.X < 0) or (Delta.X >= Length(s)) then
        exit;
      NewX := Delta.X;
      if not (s[NewX+1] in BreakChars) then
        repeat
          Inc(NewX);
          if (NewX >= Length(s)) or (s[NewX+1] in BreakChars) then
              break;
        until False
      else
        repeat
          Dec(NewX);
          if (NewX < 0) or not (s[NewX+1] in BreakChars) then
            begin
              Inc(NewX);
              break;
            end;
        until False;
      ScrollTo(NewX, Delta.Y);
    end { WordEnd };

  procedure ASCIITable;
    var
      P: PWindow;
      W: word;
      E: TEvent;
      R: TRect;
      CR: PView;
    function GetCH: boolean;
      begin
        {    Desktop^.Insert(P);
    Desktop^.UnLock;
    W:=P^.Execute;
    Desktop^.Lock;
    Desktop^.Delete(P);}
        W := Desktop^.ExecView(P);
        P^.GetData(charASCII);
        GetCH := True;
        if W in [cmOK, cmYes] then
          begin
            Event.What := evKeyDown;
            Event.KeyCode := byte(charASCII);
            PutEvent(E);
            ClearEvent(E);
            InputChar;
            Desktop^.UnLock;
          end;
        GetCH := W = cmYes;
      end;
    begin { ASCIITable }
      P := New(PASCIIChart, Init(R));
      P^.MoveTo(boundsASCII.X, boundsASCII.Y);
      P^.SetData(charASCII);
      CR := Desktop^.Current;
      {  Desktop^.Lock;}
      while GetCH do
        ;
      {  Desktop^.UnLock;}
      boundsASCII := P^.Origin;
      Dispose(P, Done);
      if CR <> nil then
        CR^.Select;
    end { ASCIITable };

  procedure SelectAll;
    begin{AK155 выделить весь текст}
      with Sel, FileLines^ do
        begin
          A.X := 0;
          A.Y := 0;
          B.Y := Count-1;
          B.X := Length(CnvLongString(At(B.Y)));
        end;
      Mark := Sel;
      Marking := False;
      BlockVisible := True;
      VertBlock := False;
    end; {/AK155}

  var
    PC: PCollection;
    R: TRect;

  begin { TFileEditor.HandleEvent }
    if (Event.What = evCommand) and (Event.Command = cmHelp2) then
        begin
        {Cat:warn почему закомментировали?}
        { if UpStrg(EditName)=UpStrg(SourceDir+'dn.ini')
   then HelpCtx:=hcDNIni
   else}HelpCtx := hcEditor;
        Event.Command := cmHelp;
        Application^.HandleEvent(Event);
        HelpCtx := hcEditor;
        ClearEvent(Event);
        exit;
      end;
    inherited HandleEvent(Event);
    {Cat}
    {$IFDEF PLUGIN}
    if ProcessEditorEventHook(Event, @Self) then
      exit;
    {$ENDIF}
    {/Cat}
    DelWord := False;
1:
    LastY := Delta.Y;
    LastX := Delta.X;
    i := FileLines^.Count;
    ChPos := False;
    case Event.What of
      evCommand:
        case Event.Command of
          cmMainMenu:
            begin
              Message(DNApp.MenuBar, evCommand, cmMenu, nil);
              CE;
            end;
          cmDuplicateLine:
            if F6_DuplicatesLine then
              begin
                ChangeLine;
                WorkModify;
                KeyMapAtInsert(LastY+1, NewLongStr(GetLine(LastY)));
                  {-$VIV}
                StoreUndoInfo(udDupeLine, Delta, s);
                SetLimits;
                for l := 1 to 9 do
                  if MarkPos[l].Y >= Delta.Y then
                    Inc(MarkPos[l].Y);
                CED;
              end
            else
              begin
                OptimalFill := not OptimalFill;
                DrawView;
                CE
              end;
          cmClose:
            begin
              if (SmartPad or ClipBrd) then
                begin
                  ChangeLine;
                  if Modified then
                    MISaveFile(@Self);
                  if not Owner^.GetState(sfModal) then
                    exit;
                  CE;
                  Owner^.Redraw;
                  PEditWindow(Owner)^.ModalEnd := True;
                  ClearEvent(Event);
                end;
            end;
          cmUpString, cmLowString, cmCapString,
          cmUpWord, cmLowWord, cmCapWord:
            begin
              if DelSpaces(WorkString) <> '' then
                begin
                  WorkModify;
                  while (WorkString <> '') and (WorkString[Length(
                      WorkString)] = ' ')
                  do
                    SetLength(WorkString, Length(WorkString)-1);
                  StoreUndoInfo(udStrModified, Delta, WorkString);
                  if (ShiftState and 3 <> 0)
                    or (Event.Command = cmUpString)
                    or (Event.Command = cmLowString)
                    or (Event.Command = cmCapString)
                  then
                    case Event.Command of
                      cmUpWord, cmUpString:
                        UpLongStr(WorkString);
                      cmLowWord, cmLowString:
                        LowLongStr(WorkString);
                      cmCapWord, cmCapString:
                        CapLongStr(WorkString);
                    end
                  else
                    begin
                      while (LastX > 0) and not (WorkString[LastX] in
                          BreakChars)
                      do
                        Dec(LastX);
                      s := '';
                      while (Length(WorkString) > LastX) and not (
                          WorkString[LastX+1] in BreakChars)
                      do
                        begin
                          s := s+WorkString[LastX+1];
                          Delete(WorkString, LastX+1, 1);
                        end;
                      if s <> '' then
                        case Event.Command of
                          cmUpWord:
                            UpLongStr(s);
                          cmLowWord:
                            LowLongStr(s);
                          cmCapWord:
                            CapLongStr(s);
                        end {case};
                      System.Insert(s, WorkString, LastX+1);
                    end;
                  ChangeLine; {-$VIV}
                end;
              CED
            end;
          cmMarkWord:
            begin
              ChangeLine;
              if (LastX < Length(WorkString)) and not (WorkString[
                  LastX+1] in BreakChars) and
                ((LastX = 0) or (WorkString[LastX] in BreakChars))
              then
              else
                Message(@Self, evCommand, cmWordLeft, nil);
              if (LastY <> Delta.Y) or (Delta.X < Length(WorkString))
                  and (WorkString[Delta.X+1] in BreakChars)
                or (Delta.X >= Length(WorkString))
              then
                begin
                  Message(@Self, evCommand, cmWordRight, nil);
                end;
              Mark.A := Delta;
              Mark.B := Delta;
              ChangeLine;
              while (Mark.B.X < Length(WorkString)) and not (
                  WorkString[Mark.B.X+1] in BreakChars)
              do
                Inc(Mark.B.X);
              R := Mark;
              ScrollTo(LastX, LastY);
              BlockVisible := True;
              Mark := R;
              CED;
            end;
          cmMarkLine:
            begin
              Mark.A.X := 0;
              Mark.A.Y := Delta.Y;
              if Delta.Y >= FileLines^.Count-1 then
                begin
                  Mark.B.Y := Delta.Y;
                  Mark.B.X := Length(WorkString)
                end
              else
                begin
                  Mark.B.Y := Delta.Y+1;
                  Mark.B.X := 0
                end;
              BlockVisible := True;
              CED
            end;
          cmSelectAll:
            begin
              SelectAll;
              CED
            end;
          cmMoveBlockStart:
            begin
              ScrollTo(Mark.A.X, Mark.A.Y);
              CE
            end;
          cmMoveBlockEnd:
            begin
              ScrollTo(Mark.B.X, Mark.B.Y);
              CE
            end;
          cmSwitchHiLine:
            begin
              EdOpt.HiliteLine := not EdOpt.HiliteLine;
              CED
            end;
          cmSwitchHiColumn:
            begin
              EdOpt.HiliteColumn := not EdOpt.HiliteColumn;
              CED
            end;
          cmSwitchWrap:
            begin
              EdOpt.AutoJustify := not EdOpt.AutoJustify;
              CE
            end;
          cmSwitchFill:
            begin
              OptimalFill := not OptimalFill;
              DrawView;
              CE
            end;
          cmSwitchTabReplace:
            begin
              TabReplace := not TabReplace;
              CE
            end;
          cmSwitchBack:
            begin
              EdOpt.BackIndent := not EdOpt.BackIndent;
              CE
            end;
          cmIndentOn, cmIndentOff:
            begin
              EdOpt.AutoIndent := Event.Command = cmIndentOn;
              CE
            end;
          cmSwitchIndent:
            begin
              EdOpt.AutoIndent := not EdOpt.AutoIndent;
              CE
            end;
          cmSwitchSmartTab:
            begin
              EdOpt.SmartTab := not EdOpt.SmartTab;
              CE
            end;
          cmSwitchHighLight:
            begin
              EdOpt.HiLite := not EdOpt.HiLite;
              {/Cat}
              DrawView;
            end;
          cmEditCrLfMode:
            begin
              EdOpt.ForcedCRLF := cfCRLF;
              DrawView;
            end;
          cmEditLfMode:
            begin
              EdOpt.ForcedCRLF := cfLF;
              DrawView;
            end;
          cmEditCrMode:
            begin
              EdOpt.ForcedCRLF := cfCR;
              DrawView;
            end;
          cmSwitchBrackets:
            begin
              EdOpt.AutoBrackets := not EdOpt.AutoBrackets;
              CE
            end;
          cmSwitchSave:
            begin
              EdOpt.AutoWrap := not EdOpt.AutoWrap;
              CE
            end;
          cmGetName:
            PString(Event.InfoPtr)^:= PWindow(Owner)^.Title^;
          cmCtrlHome:
            begin
              ScrollTo(Delta.X, Pos.Y);
              CED
            end;
          cmCtrlEnd:
            begin
              ScrollTo(Delta.X, Pos.Y+Size.Y-1);
              CED
            end;
          cmDeltoEOLN:
            begin
              WorkModify;
              StoreUndoInfo(udSubDel, Delta, WorkString);
              SetLength(WorkString, LastX);
              CED;
            end;
          cmCopyBlock:
            begin
              CE;
              if not ValidBlock then
                exit;
              ChangeLine;
              PC := GetSelection;
              if PC = nil then
                begin
                  Message(@Self, evCommand, cmCopy, nil);
                  Message(@Self, evCommand, cmPaste, nil);
                end
              else
                begin
                  ChangeLine;
                  InsertBlock(PC, True);
                  Dispose(PC, Done);
                end;
              ChangeLine;
            end;
          cmMoveBlock:
            begin
              ChangeLine;
              CE;
              if not ValidBlock then
                exit;
              PC := GetSelection;
              if PC = nil then
                begin
                  Message(@Self, evCommand, cmCopy, nil);
                  DeleteBlock(False);
                  Message(@Self, evCommand, cmPaste, nil);
                end
              else
                begin
                  ChangeLine;
                  DeleteBlock(False);
                  InsertBlock(PC, True);
                  Dispose(PC, Done);
                end;
              ChangeLine;
            end;
          cmReverseSearch:
            begin
              ChangeLine;
              SearchData.Dir := SearchData.Dir xor 1;
              if SearchOnDisplay then
                Search(Delta.X, Delta.Y);
              Search(Delta.X, Delta.Y);
              SearchData.Dir := SearchData.Dir xor 1;
              ChangeLine;
              CE;
              SearchOnDisplay := False;
            end;
          cmWindowsPaste:
            PasteWinBlock;
          cmChangeCharCase:
            ChangeCharCaseDirect; {-$VIV}
          cmBracketPair,
          cmPlayMacro,
          cmSelectMacro,
          cmSetMargins,
          cmGotoLineNumber,
          cmGotoLineNumber2,
          cmSortBlock,
          cmRevSortBlock,
          cmCalcBlock,
          cmInsertDate,
          cmInsertTime,
          cmPrintBlock,
          cmViewFile,
          cmPrintFile,
          cmPrintFileEd,
          cmFRight, cmFLeft, cmFJustify, cmFCenter,
          cmLRight, cmLLeft, cmLJustify, cmLCenter,
          cmIndentBlock,
          cmUnIndentBlock,
          cmUndo,
          cmRedo, {-$VOL}
          cmReplace,
          cmUpcaseBlock,
          cmLowcaseBlock,
          cmCapitalizeBlock,
          cmWindowsCopy,
          cmSyncClipIn,
          cmSyncClipOut,
          cmStartSearch,
          cmSwitchKeyMapping,
          cmRusEngConv:{-$VIV}
            begin
              if HandleCommand(Event) then
                DrawView;
              CE
            end;
          cmContSearch:
            begin
              ChangeLine;
              Search(Delta.X, Delta.Y);
              ChangeLine;
              CE
            end;
          cmSpecChar, cmASCIITable:
            begin
              ASCIITable;
              CE
            end;
          cmLoadText:
            begin
              CE;
              if Modified then
                begin
                  i := AskSave;
                  if i = cmYes then
                    Message(@Self, evCommand, cmSaveText, nil);
                  if i = cmCancel then
                    exit;
                end;
              MIOpenFile(@Self);
              exit;
            end;
          cmInsertOn, cmInsertOff:
            begin
              InsertMode := Event.Command = cmInsertOn;
              CE
            end;
          cmSwitchIns:
            begin
              InsertMode := not InsertMode;
              DrawView;
              CE
            end;
          cmSaveAll:
            begin
              GlobalMessage(evCommand, cmSaveText, nil);
              CE
            end;
          cmSaveText:
            begin
              ChangeLine;
              if EditName <> '' then
                MISaveFile(@Self)
              else
                MISaveFileAs(@Self);
              CE;
              Owner^.Redraw;
            end;
          cmSaveTextAs:
            begin
              ChangeLine;
              MISaveFileAs(@Self);
              CE;
              Owner^.Redraw;
            end;
          cmBlockRead:
            begin
              ChangeLine;
              BlockRead;
              ChangeLine;
              CE
            end;
          cmBlockWrite:
            begin
              BlockWrite;
              CE
            end;
          cmDelChar:
            begin
              if (EditorDefaults.EdOpt and (ebfPBl+ebfObl) = ebfObl)
                and ValidBlock
              then
                DeleteBlock(True)
              else
                MakeDel;
              CED
            end;
          cmEnter:
            begin
              BlockOff;
              MakeEnter;
              ScrollTo(Delta.X, Delta.Y);
              CED
            end;
          cmEnd:
            begin
              UnMark := True;
              while (WorkString <> '') and (WorkString[Length(
                  WorkString)] = ' ')
              do
                SetLength(WorkString, Length(WorkString)-1);
              ScrollTo(Length(WorkString), Delta.Y);
              CED
            end;
          cmInsLine:
            begin
              BlockOff;
              PP := Delta;
              MakeEnter;
              Delta := PP;
              ChangeLine;
              ScrollTo(PP.X, PP.Y);
              CED
            end;
          cmTab:
            begin
              BlockOff;
              DoTab;
              CE
            end;
          cmWordRight:
            begin
              UnMark := True;
              WordRight;
              ScrollTo(LastX, LastY);
              CED
            end;
          cmWordLeft:
            begin
              UnMark := True;
              WordLeft;
              ScrollTo(LastX, LastY);
              CED
            end;
          cmDeleteLine:
            begin
              BlockOff;
              DeleteLine;
              CE
            end;
          cmSwitchBlock:
            begin
              UnMark := True;
              VertBlock := not VertBlock;
              DrawView;
              CE
            end;
          cmClear:
            begin
              UnMark := True;
              if BlockVisible and ValidBlock then
                DeleteBlock(True);
              CE
            end;
          cmCopy:
            begin
              UnMark := True;
              if BlockVisible and ValidBlock then
                CopyBlock;
              CE
            end;
          cmCut:
            begin
              UnMark := True;
              if BlockVisible and ValidBlock then
                begin
                  CopyBlock;
                  DeleteBlock(True)
                end;
              CE
            end;
          cmPaste:
            begin
              UnMark := True;
              PasteBlock;
              CE
            end;
          cmMoveUp:
            begin
              UnMark := True;
              Event.What := evKeyDown;
              Event.KeyCode := kbUp
            end;
          cmPgUp:
            begin
              UnMark := True;
              Dec(Pos.Y, Size.Y);
              VScroll^.SetValue(Delta.Y-Size.Y);
              DrawView;
              CE
            end;
          cmPgDn:
            begin
              UnMark := True;
              Inc(Pos.Y, Size.Y);
              VScroll^.SetValue(Delta.Y+Size.Y);
              DrawView;
              CE
            end;
          cmMoveDown:
            begin
              UnMark := True;
              Event.What := evKeyDown;
              Event.KeyCode := kbDown
            end;
          cmMoveLeft:
            begin
              UnMark := True;
              Event.What := evKeyDown;
              Event.KeyCode := kbLeft
            end;
          cmMoveRight:
            begin
              UnMark := True;
              Event.What := evKeyDown;
              Event.KeyCode := kbRight
            end;
          cmHideBlock:
            begin
              BlockVisible := not BlockVisible;
              UnMark := False;
              CED
            end;
          cmScrollUp:
            if Pos.Y > 0 then
              begin
                UnMark := True;
                if not (Delta.Y-Pos.Y+1 < Size.Y) then
                  Dec(Delta.Y);
                Dec(Pos.Y);
                ScrollTo(Delta.X, Delta.Y);
                Delta.Y := VScroll^.Value;
                DrawView;
              end;
          cmScrollDn:
            if Pos.Y < FileLines^.Count-1 then
              begin
                UnMark := True;
                if not (Delta.Y > Pos.Y) then
                  Inc(Delta.Y);
                Inc(Pos.Y);
                ScrollTo(Delta.X, Delta.Y);
                Delta.Y := VScroll^.Value;
                DrawView;
              end;
          cmBlockStart:
            begin
              UnMark := True;
              Marking := False;
              Mark.A := Delta;
              BlockVisible := True;
              DrawView;
              CE
            end;
          cmBlockEnd:
            begin
              UnMark := True;
              Marking := False;
              Mark.B := Delta;
              BlockVisible := True;
              DrawView;
              CE
            end;
          cmPlaceMarker1..cmPlaceMarker9:
            begin
              UnMark := True;
              Event.InfoByte := Event.Command-cmPlaceMarker1+1;
              if (MarkPos[Event.InfoByte].Y = Delta.Y) then
                FillChar(MarkPos[Event.InfoByte], SizeOf(TPoint),
                  $FF)
              else
                MarkPos[Event.InfoByte] := Delta;
              CED;
            end; {-$VIV}
          cmGoToMarker1..cmGoToMarker9:
            begin
              ChangeLine;
              UnMark := True;
              Event.InfoByte := Event.Command-cmGoToMarker1+1;
              if not MarkPos[Event.InfoByte].EqualsXY(-1, -1) then
                begin
                  Delta := MarkPos[Event.InfoByte];
                  Pos.X := Delta.X-Size.X div 2;
                  Pos.Y := Delta.Y-Size.Y div 2;
                  ChangeLine; {WorkModify := false;}
                  ScrollTo(Delta.X, Delta.Y);
                  DrawView;
                  CenterScreen;
                end;
              CE
            end;
          cmDelBackChar:
            begin
              BlockOff;
              MakeBack;
              CE
            end;
          cmDelWordRight:
            begin
              BlockOff;
              if (LastX >= Length(WorkString)) then
                begin
                  MakeDel;
                  while (LastX >= Length(WorkString)) and (LastY+1 <
                      FileLines^.Count)
                    or (LastX < Length(WorkString)) and (WorkString[
                      LastX+1] = ' ')
                  do
                    MakeDel;
                  CED;
                  exit;
                end;
              WorkModify;
              if (WorkString[LastX+1] in BreakChars-[' ']) then
                begin
                  MakeDel;
                  CED;
                  exit;
                end;
              if (WorkString[LastX+1] = ' ') then
                while (LastX < Length(WorkString)) and (WorkString[
                    LastX+1] = ' ')
                do
                  MakeDel
                else
                begin
                  while (LastX < Length(WorkString)) and not (
                      WorkString[LastX+1] in BreakChars)
                  do
                    MakeDel;
                  while (LastX < Length(WorkString)) and (WorkString[
                      LastX+1] = ' ')
                  do
                    MakeDel
                end;
              CED;
            end;
          cmDelWordLeft:
            begin
              BlockOff;
              if LastX = 0 then
                begin
                  MakeBack;
                  CED;
                  exit
                end;
              if LastX >= Length(WorkString)
              then
                if Length(WorkString) > 0
                then
                  LastX := Length(WorkString)
                else
                  begin
                    MakeBack;
                    CED;
                    exit
                  end;
              if (WorkString[LastX] in BreakChars-[' '])
              then
                begin
                  MakeBack;
                  CED;
                  exit;
                end;
              WorkModify;
              if (LastX > 0) and (WorkString[LastX] = ' ') then
                while (LastX > 0) and
                  (WorkString[LastX] = ' ')
                do
                  MakeSmallBack;
              while (LastX > 0) and
                not (WorkString[LastX] in BreakChars)
              do
                MakeSmallBack;
              ScrollTo(LastX, LastY);
              CED;
            end;
          cmSwitchDrawMode:
            begin
              DrawMode := (DrawMode+1) mod 3;
              Owner^.Redraw;
              LastDir := -1;
              CE;
            end;
          cmOpenFileAtCursor:
            OpenFileAtCursor; {-$VIV 18.05.99--}
        end {case};
      evKeyDown:
        begin
          {AK155 Для вертикальных стрелок (возможно, с шифтом) ScrollLock
          и Ctrl берутся по xor. См. чуть ниже обработку kbCtrlUp и далее}
          if ShiftState and kbScrollState <> 0 then
            for i := Low(UpDnKey) to High(UpDnKey) do
              if Event.KeyCode = UpDnKey[i] then
                begin
                  Event.KeyCode := UpDnKey[i xor 1];
                  break;
                end;
          case Event.KeyCode of

            {JO: по Ctrl-Alt-Shift-цифра переходим к закладкам с пометкой текста}
            {    от текущего места до закладки                                  }
            kbCtrlAltShift1:
              Message(@Self, evCommand, cmGoToMarker1, nil);
            kbCtrlAltShift2:
              Message(@Self, evCommand, cmGoToMarker2, nil);
            kbCtrlAltShift3:
              Message(@Self, evCommand, cmGoToMarker3, nil);
            kbCtrlAltShift4:
              Message(@Self, evCommand, cmGoToMarker4, nil);
            kbCtrlAltShift5:
              Message(@Self, evCommand, cmGoToMarker5, nil);
            kbCtrlAltShift6:
              Message(@Self, evCommand, cmGoToMarker6, nil);
            kbCtrlAltShift7:
              Message(@Self, evCommand, cmGoToMarker7, nil);
            kbCtrlAltShift8:
              Message(@Self, evCommand, cmGoToMarker8, nil);
            kbCtrlAltShift9:
              Message(@Self, evCommand, cmGoToMarker9, nil);

            {AK155 27-12-2003
           CtrlUp, kbCtrlShiftUp  скроллируют текст с сохранением
           позиции курсора относительно окна. Down - аналогично}
            kbCtrlUp, kbCtrlShiftUp:
              begin
                UnMark := (Event.KeyCode = kbCtrlUp);
                Dec(Pos.Y);
                ChangeLine; {John_SW}
                ScrollTo(Delta.X, Delta.Y-1);
                DrawView;
                CED; {John_SW}
              end;
            kbCtrlDown, kbCtrlShiftDown:
              begin
                UnMark := (Event.KeyCode = kbCtrlDown);
                Inc(Pos.Y);
                ChangeLine; {John_SW}
                ScrollTo(Delta.X, Delta.Y+1);
                DrawView;
                CED; {John_SW}
              end;
            {/AK155}
            else
              if (Event.KeyCode <> kbCtrlAltX) then
                if (Event.KeyCode = kbCtrlU) then
                  WordEnd
                else{-$VIV}
                  begin
                    if DrawMode > 0 then
                      begin
                        case Event.KeyCode of
                          kbRight, kbCtrlD, kbShiftRight,
                            kbCtrlRight:
                            begin
                              DrawLine(1);
                              exit
                            end;
                          kbLeft, kbCtrlS, kbShiftLeft, kbCtrlLeft:
                            begin
                              DrawLine(3);
                              exit
                            end;
                          kbUp, kbCtrlE, kbShiftUp:
                            begin
                              DrawLine(0);
                              exit
                            end;
                          kbDown, kbCtrlX, kbShiftDown:
                            begin
                              DrawLine(2);
                              exit
                            end;
                        end {case};
                      end;
                    (*      if Event.KeyCode = kbCtrlEnter then ; *)
                      {-$VIV 18.05.99--}
                    (*      begin BlockOff; MakeEnter; ScrollTo(Delta.X, Delta.Y); CED; Exit end; *)
                    CFind := False;
                    EvStr := #0#0;
                    for i := 1 to MaxCommands do
                      with EditCommands[i] do
                        begin
                          if (CC1[1] = Event.CharCode) then
                            begin
                              EvStr[1] := CC1[1];
                              if CC1[2] <> #0 then
                                begin
                                  KeyEvent(Event);
                                  EvStr[2] := Event.CharCode
                                end;
                              break;
                            end;
                          if (CC2[1] = Event.CharCode) then
                            begin
                              EvStr[1] := CC2[1];
                              if CC2[2] <> #0 then
                                begin
                                  KeyEvent(Event);
                                  EvStr[2] := Event.CharCode
                                end;
                              break;
                            end;
                        end;
                    if EvStr[2] in ['A'..']', 'a'..'z'] then
                      EvStr[2] := Char(Ord(UpCase(EvStr[2]))-64);
                    for i := 1 to MaxCommands do
                      with EditCommands[i] do
                        if ((C1 = Event.KeyCode) or (C2 = Event.
                            KeyCode)) and (EvStr[2] = #0) or
                          (EvStr <> #0#0) and ((CC1 = EvStr) or (CC2 =
                            EvStr))
                        then
                          begin
                            CFind := True;
                            break
                          end;
                    if CFind then
                      begin
                        CE;
                        MessageL(Owner, evCommand, EditCommands[i].C,
                        byte(EvStr[2]));
                        exit
                      end;
                    { AK155 9-07-2002 После введения Козловым новой системы кодирования клавиш
серые + и - с шифтом (перелистывание однотипных окон) стали иметь
ненулевой CharCode, поэтому их приходится анализировать специально.
Интересно, больше подобных ситуаций нет?}
                    with Event do
                      if (CharCode > #31)
                        and (KeyCode <> kbShiftGrayPlus)
                        and (KeyCode <> kbShiftGrayMinus)
                        {Есть! Alt-BS. На всякий случай отсекаем все с Alt и Ctrl}
                        {JO: c Alt отсекать нельзя, т.к. не работает Alt-ввод}
                        and (KeyCode and $40000 = 0)
                      then
                        InputChar;
                  end;
          end {case};
        end;
      evBroadcast:
        case Event.Command of
          cmFindEdit:
            if PString(Event.InfoPtr)^ = EditName then
              begin
                if Owner <> nil then
                  begin
                    Owner^.Select;
                    ClearEvent(Event);
                  end;
              end;
          cmScrollBarChanged:
            begin
              if HScroll <> nil then
                Delta.X := HScroll^.Value;
              if VScroll <> nil then
                Delta.Y := VScroll^.Value;
              if LastY <> Delta.Y then
                ChangeLine;
              ChPosition := (not EdOpt.HiliteColumn or
              (LastPos.X = Delta.X)) and (LastPos.X >= 0);
              BMarking;
              DrawView;
            end;
          {else CalcMenu;}
        end {case};
      evMouseDown:
        begin
          if MouseButtons = 0 then
            exit;
          LineMarking := False;
          if Event.Double then
            begin
              Message(@Self, evCommand, cmMarkWord, nil);
              CE;
              exit;
            end;
          LineMarking := ShiftState and 4 <> 0;
          ChangeLine;
          EnableMarking := False;
          MakeLocal(Event.Where, Delta);
          Delta.X := Pos.X+Delta.X;
          Delta.Y := Pos.Y+Delta.Y;
          BlockOff;
          Sel.A := Delta;
          Sel.B := Delta;
          ScrollTo(Delta.X, Delta.Y);
          Sel.A := Delta;
          Sel.B := Delta;
          EnableMarking := True;
          if Event.Buttons and mbLeftButton <> 0 then
            begin
              BlockVisible := True;
              MouseMark := True;
            end
          else
            begin
              RulerVisible := True;
              DrawView;
            end;
          i := RepeatDelay;
          RepeatDelay := 0;
          if LineMarking then
            begin
              Sel.A.X := 0;
              Sel.B.Y := Sel.A.Y+1;
              Sel.B.X := 0;
              Mark := Sel;
              DrawView;
            end;
          repeat
            MakeLocal(Event.Where, t);
            if MouseInView(Event.Where) then
              begin
                Delta.X := Pos.X+t.X;
                Delta.Y := Pos.Y+t.Y;
                ScrollTo(Delta.X, Delta.Y);
                OldDelta := Delta;
              end
            else
              begin
                if t.X < 0 then
                  Dec(Delta.X)
                else if t.X >= Size.X then
                  Inc(Delta.X);
                if Delta.X < 1
                then
                  Delta.X := 0
                else if Delta.X > LimitX
                then
                  Delta.X := LimitX;
                if t.Y < 0 then
                  Dec(Delta.Y)
                else if t.Y >= Size.Y then
                  Inc(Delta.Y);
                if Delta.Y < 1
                then
                  Delta.Y := 0
                else if Delta.Y > LimitY
                then
                  Delta.Y := LimitY;
                ScrollTo(Delta.X, Delta.Y);
                OldDelta := Delta;
              end;
          until not MouseEvent(Event, evMouseAuto+evMouseMove);
          MouseMark := False;
          RulerVisible := False;
          RepeatDelay := i;
          ScrollTo(Delta.X, Delta.Y);
          ChangeLine;
          if LineMarking then
            begin
              if (Mark.B.X > 0) and (Mark.B.Y < FileLines^.Count-1)
              then
                begin
                  Inc(Mark.B.Y);
                  Mark.A.X := 0
                end;
              if (Mark.A.X > 0) then
                Mark.A.X := 0;
              CED;
              LineMarking := False;
              exit;
            end;
          CED;
        end;
    end {case};
  end { TFileEditor.HandleEvent };

procedure TFileEditor.DoHighlite
  (var B; const s: LongString; const Attr: String);
  var
    i: integer;
    j: integer;
    k: integer;
    l: integer;
    C: Char;
  begin
    i := Pos.X+1;
    j := 0;
    l := Min(Length(s), i+Size.X);
    while (i <= l) do
      begin
        C := s[i];
        k := i+1;
        while (k <= l) and (s[k] = C) do
          Inc(k);
        if (C <> #0) and (Ord(C) <= Length(Attr)) then
          MoveColor(TWordArray(B)[j], k-i, Ord(Attr[Ord(C)]));
        Inc(j, k-i);
        i := k;
      end;
  end { TFileEditor.DoHighlite };

procedure OpenSmartpad;
  var
    R: TRect;
    PV: Pointer;

  procedure InsertInfo; {-$VIV} {SYR}
    var
      Str: LongString;
      Idx: integer;
    begin
      with SmartWindow^.Intern^ do
        begin
          if SPInsertDate then
            begin
              Str := '';
              for Idx := 0 to 5 do
                Str := Str+Char(SPLineChar);
              Str := Str+'< '+GetDateTime(False)+' '+GetDateTime(
                True)+' >';
              for Idx := 0 to 35 do
                Str := Str+Char(SPLineChar);
              { Flash >>> }
              if (Copy(GetLine(FileLines^.Count-2), 1, 7) = Copy(Str, 1,
                  7)) and
                (Copy(GetLine(FileLines^.Count-2), 29, 37) = Copy(
                  Str, 29, 37)) and
                (((((Delta.Y > FileLines^.Count-2) and (WorkString = '')
                  ) or
                (Delta.Y = FileLines^.Count-2)) or
                (Delta.Y < FileLines^.Count-2))) and
                (GetLine(FileLines^.Count-1) = '')
              then
                begin
                  FileLines^.AtDelete(FileLines^.Count-2);
                  FileLines^.AtDelete(FileLines^.Count-1);
                end;
              if ((Delta.Y > FileLines^.Count-2) and (WorkString <> ''))
                  and not
                ((Copy(WorkString, 1, 7) = Copy(Str, 1, 7)) and
                (Copy(WorkString, 29, 37) = Copy(Str, 29, 37))) or
                (GetLine(FileLines^.Count-1) <> '')
              then
                FileLines^.Insert(nil);
              { Flash <<< }
              FileLines^.Insert(NewLongStr(Str));
            end;
          FileLines^.Insert(nil);
          SetLimits;
          ScrollTo(0, FileLines^.Count-1);
          Pos.X := Delta.X-Size.X div 2;
          Pos.Y := Delta.Y-Size.Y div 2;
          SmartWindow^.Redraw;
        end;
    end { InsertInfo };

  {--- start -------- Eugeny Zvyagintzev ---------}
  var
    PS: PString;
    V: PFileEditor;
    i: integer;
    P: PEditRecord;
    {--- finish -------- Eugeny Zvyagintzev ---------}
  begin { OpenSmartpad }
    SmartWindow := SmartWindowPtr^; {Cat}

    if (SmartWindow <> nil) and SmartWindow^.GetState(sfModal) then
        exit;
    PV := Application^.TopView;
    Desktop^.GetExtent(R);
    R.Grow(-2, -2);
    if (SmartWindow <> nil) then
      begin
        InsertInfo;
        if (PV <> Application) then
          begin
            {if PView(PV)^.Owner = Pointer(Desktop) then SmartWindow^.MakeFirst;}
            Desktop^.Delete(SmartWindow);
            Desktop^.ExecView(SmartWindow);
            Desktop^.InsertBefore(SmartWindow, Desktop^.Last);
            Desktop^.SetCurrent(PV, EnterSelect);
            {if PView(PV)^.Owner = Pointer(Desktop) then PView(PV)^.MakeFirst;}
          end
        else
          SmartWindow^.Select;
        exit;
      end;
    New(SmartWindow, Init(R, 'SmartPad'));
    {--- start -------- Eugeny Zvyagintzev ---------}
    {Now DN will save and load SmartPad edit history}
    V := SmartWindow^.Intern;
    FreeStr := V^.EditName;
    System.Insert(' ', FreeStr, 1);
    UpStr(FreeStr);
    PS := @FreeStr;
    if (InterfaceData.Options and ouiTrackEditors <> 0) and (
        EditHistory <> nil)
    then
      i := EditHistory^.IndexOf(@PS)
    else
      i := -1;
    if i >= 0 then
      begin
        P := EditHistory^.At(i);
        R.Assign(P^.fOrigin.X, P^.fOrigin.Y, P^.fOrigin.X+P^.fSize.X, P^
          .fOrigin.Y+P^.fSize.Y);
        AdjustToDesktopSize(R, P^.fDeskSize);
        SmartWindow^.Locate(R);
        with V^, P^ do
          begin
            MarkPos := fMarks;
            Mark.A := fBlockStart;
            Mark.B := fBlockEnd;
            ScrollTo(fDelta.X, fDelta.Y);
            Pos := fPos;
            BlockVisible := fBlockVisible;
            VertBlock := fVerticalBlock;
            EdOpt.HiLite := fHighlight;
            EdOpt.HiliteColumn := fHiliteColumn;
            EdOpt.HiliteLine := fHiliteLine;
            EdOpt.AutoIndent := fAutoIndent;
            EdOpt.AutoJustify := fAutoJustify;
            EdOpt.AutoBrackets := fAutoBrackets;
            EdOpt.LeftSide := fLeftSide;
            EdOpt.RightSide := fRightSide;
            EdOpt.InSide := fInSide;
            InsertMode := fInsMode;
            KeyMap := fKeyMap; {-$VIV}
            { Flash >>> }
            EdOpt.BackIndent := fBackIndent;
            EdOpt.AutoWrap := fAutoWrap;
            OptimalFill := fOptimalFill;
            TabReplace := fTabReplace;
            EdOpt.SmartTab := fSmartTab;
            { Flash <<< }
            WorkString := GetLine(fDelta.Y); {-$VIV 11.05.99}
          end;
      end
    else
      StoreEditInfo(SmartWindow);
    {--- finish -------- Eugeny Zvyagintzev ---------}

    InsertInfo;
    if (PV <> Application) then
      begin
        Desktop^.ExecView(SmartWindow);
        SmartWindow^.Free;
        SmartWindow := nil
      end
    else
      Desktop^.Insert(SmartWindow);
  end { OpenSmartpad };

procedure OpenClipBoard; {-$VOL begin}
  var
    R: TRect;
    PV: Pointer;
  begin
    ClipboardWindow := ClipboardWindowPtr^; {Cat}

    if SystemData.Options and ossUseSysClip <> 0 then
      SyncClipOut {(true)};
    if (ClipboardWindow <> nil) and ClipboardWindow^.GetState(
        sfModal)
    then
      exit;
    PV := Application^.TopView;
    Desktop^.GetExtent(R);
    if (ClipboardWindow <> nil) then
      begin
        if (PV <> Application) then
          begin
            Desktop^.Delete(ClipboardWindow);
            Desktop^.ExecView(ClipboardWindow);
            Desktop^.InsertBefore(ClipboardWindow, Desktop^.Last);
            Desktop^.SetCurrent(PV, EnterSelect);
          end
        else
          ClipboardWindow^.Select;
        exit;
      end;
    New(ClipboardWindow, Init(R, 'Clipboard'));
    if (PV <> Application) then
      begin
        Desktop^.ExecView(ClipboardWindow);
        ClipboardWindow^.Free;
        ClipboardWindow := nil
      end
    else
      Desktop^.Insert(ClipboardWindow);
  end { OpenClipBoard }; {-$VOL end}

end.
