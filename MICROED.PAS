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

unit MicroEd;

interface

uses
  Dos, LFN{DataCompBoy}, Objects, Drivers, Memory, Views, Messages, DNApp,
  Advance, Advance1, Advance2, Advance3, Startup, Menus, DNStdDlg, Dialogs,
  Commands, UniWin, RStrings, DNHelp, {SBlocks,} ObjType, U_KeyMap, Collect,
  {$IFDEF REGEXP} RegExp, {$ENDIF}
  {$IFDEF COLORER} Colorer, {$ENDIF}
  Ed2, HighLite;

const
      Clipboard   : PCollection = nil;

      CFileEditor = #13#14#16#17#18#19#20#21#22#23#24#25;

type
  PEditOptions = ^TEditOptions;
  TEditOptions = record
    AutoIndent:   Boolean;
    AutoBrackets: Boolean;
    BackIndent:   Boolean; {BACKUNINDENTS}
    HiLite:       Boolean;
    HiliteLine:   Boolean;
    HiliteColumn: Boolean;
    AutoJustify:  Boolean; {WRAPJUSTIFY}
    AutoWrap:     Boolean; {AUTOWRAP}
    LeftSide:     Word;    {LEFTMARGIN}
    RightSide:    Word;    {RIGHTMARGIN}
    InSide:       Word;    {PARAGRAPH}
    ForcedCrLf:   TCrLf;
    SmartTab:     Boolean;
  end;


  { TFileEditor }

  PFileEditor = ^TFileEditor;
  TFileEditor = object(TView)
  {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    HScroll, VScroll: PScrollBar;
    ReplaceAll: Boolean;
    Delta: TPoint;
    EditName: String; {DataCompBoy}
    FileLines: PLineCollection {PCollector}; {-SBlocks}
    IsValid,Marking: Boolean;
    SmartPad: Boolean;
    ClipBrd: Boolean; {-$VOL}
    OptMenu: PMenu;
    Mark,Sel: TRect;
    Pos, LastPos{,BlockPos}: TPoint;
    MarkPos: TPosArray;
    LastLine: LongInt; {Cat: на первый взгляд кажется, что это номер последней строки файла, а на самом деле это номер текущей строки}
    DrawMode: Integer;
    WorkString,LastShape: LongString;
    OldBlockValid, SearchOnDisplay,
    InsertMode,VertBlock,Modified,WorkModified,
    JustSaved, {piwamoto}
    BlockVisible,SpecChar,WasDelete,MouseMark, UnMark,
    LineMarking,OptimalFill,RulerVisible,EnableMarking,
    TabReplace, SearchActive: Boolean;
    UndoInfo: PCollection;
    RedoInfo: PCollection; {-$VOL}
    UndoTimes, LastSaveUndoTimes: LongInt;
    ChPosition: Boolean;
    Macros: PCollection;
    Locker: PStream;
    LastDir: Integer;
    MemEnough: Boolean;
    KeyMap: TKeyMap; {-$VIV}
    EdOpt: TEditOptions;

    HiLitePar:THighliteParams;

    InfoL,BMrk: PView;

    MenuItemStr: array [boolean] of PString;
    {$IFDEF REGEXP}
    RegExp: PRegExp;
    {$ENDIF}
    {$IFDEF COLORER}
    UseExternalColorer: Boolean; {Cat}
    ColorerCollection: PColorerCollection; {Cat}
    procedure SwitchExternalColorer(Switch: Boolean); {Cat}
    {$ENDIF}
    constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
      var FileName: String); {DataCompBoy}
    constructor Load(var S: TStream);
    destructor  Done; virtual;
    procedure Store(var S: TStream);
    procedure Awaken; virtual;
    procedure DoHighlite ( var B; const S : LongString; const Attr : String );
    procedure HandleEvent(var Event : TEvent);virtual;
    procedure Draw; virtual;
    function  Valid(Command: Word): Boolean; virtual;
    function  GetPalette : PPalette;virtual;
    function  GetLine(Index : LongInt) : LongString;
    function  GetSelection : PCollection;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    function  ValidBlock: Boolean;
    procedure CalcMenu;
    function  Search(StartX, StartY: Word): Boolean;
    procedure InsertBlock(ABlock: PCollection; SaveUndo: Boolean);
    procedure ModifyLine(Index : LongInt; S : LongString; DelSpaces: Boolean);
    procedure SetLimits;
    procedure ChangeBounds(var R: TRect); virtual;
    procedure ScrollTo(DeltaX, DeltaY: LongInt);
    function  LimitX:LongInt;
    function  LimitY:LongInt;
    procedure StoreUndoInfo(What: Word; Where: TPoint; var Info);
    function  HandleCommand(var Event: TEvent): Boolean; virtual;
    function  KeyMapConvertStr(S: LongString; toAscii: Boolean): LongString; {-$VIV}
    procedure KeyMapAtInsert(N: LongInt; P: PLongString); {-$VIV}
    procedure KeyMapAtReplace(N: LongInt; P: PLongString); {-$VIV}
    procedure Convert4do(iP: PUndoRec; DoKind: TDoKind); {-$VOL}
  end;

procedure OpenEditor;
procedure OpenSmartpad;
procedure OpenClipboard;

const
    ClipboardStream : PStream = nil; {-$VOL}

type
    TSearchData = record
                 Options: Word;
                 Dir: Word;
                 Scope: Word;
                 Origin: Word;
                 Line, What: String[250];
                end;

{Cat: параметры поиска (TSearchData.Options)}
{необходимо согласовывать с константой SwapBits в модуле Editor}
const
  efoCaseSens       =  1;
  efoWholeWords     =  2;
  efoReplacePrompt  =  4;
  efoRegExp         =  8;
  efoAllCP          = 16;
{/Cat}

const
      SearchData: TSearchData = (Options:4; Dir: 0; Scope:0; Origin:0; Line:''; What:'');

type TEditCommand = record
      C, C1, C2: Word;
      CC1, CC2: Array[1..2] of Char;
     end;

const MaxCommands: AInt= 0;
var   EditCommands : Array [1..110] of TEditCommand;

implementation

uses
  {$IFDEF PLUGIN} Plugin, {$ENDIF}
  Gauge, FViewer, Histlist, Macro, Editor, WinClp, DNUtil, Histries,
  xTime, FileCopy, ASCIItab, DnIni, U_SrchF, EdWin, Microed2{-$VIV};

const
    cmNoCommand = 4000;

   {SmartWindow: PEditWindow = nil;}      {Cat: перенёс эти переменные в модуль MicroEd2}
   {ClipboardWindow: PEditWindow = nil;}  {Cat: внимание! появились указатели SmartWindowPtr и ClipboardWindowPtr}

function MemAvail: LongInt; begin MemAvail := MemAdjust(System.MemAvail) end;

{Cat}
{$IFDEF COLORER}
procedure TFileEditor.SwitchExternalColorer(Switch: Boolean);
var
  Ed: LongInt;
begin
  if Switch<>UseExternalColorer then
    if Switch then
      begin
        UseExternalColorer:=InitColorer;
        if UseExternalColorer then
          begin
            Colorer.plgOpenPlugin(0, 0, 0); {???}
            Colorer.plgProcessEditorEvent(LongInt(@Self), EE_READ, nil);
            Colorer.plgProcessEditorEvent(LongInt(@Self), EE_REDRAW, nil); {а это здесь - чтобы colorer протормозился заранее}
            ColorerCollection:=New(PColorerCollection, Init(30,10));
          end;
      end
    else
      begin
        UseExternalColorer:=False;
        Ed := LongInt(@Self);
        Colorer.plgProcessEditorEvent(Ed, EE_CLOSE, @Ed);
        Colorer.plgClosePlugin(0);
        DoneColorer;
        if ColorerCollection<> nil then
          begin
            Dispose(ColorerCollection, Done);
            ColorerCollection:=nil;
          end;
      end;
end;
{$ENDIF}
{/Cat}

function TFileEditor.KeyMapConvertStr(S: LongString; toAscii: Boolean): LongString; {-$VIV}
begin
  case KeyMap of
    kmAnsi:
      if toAscii then
        KeyMapConvertStr := ansi_ascii(S) else
        KeyMapConvertStr := ascii_ansi(S);
    kmKoi8r:
      if toAscii then
        KeyMapConvertStr := koi8r_ascii(S) else
        KeyMapConvertStr := ascii_koi8r(S);
    else KeyMapConvertStr := S;
  end;
end; {-$VIV}

procedure TFileEditor.KeyMapAtInsert(N: LongInt; P: PLongString); {-$VIV}
begin
  if (P <> nil) then P^ := KeyMapConvertStr(P^, false);
  FileLines^.AtInsert(N, P);
end; {-$VIV}

procedure TFileEditor.KeyMapAtReplace(N: LongInt; P: PLongString); {-$VIV}
begin
  if (P <> nil) then P^ := KeyMapConvertStr(P^, false);
  FileLines^.AtReplace(N, P);
end; {-$VIV}

procedure TFileEditor.Convert4do(iP: PUndoRec;DoKind: TDoKind);
  var P: PUndoRec;
      I, J: LongInt;
      S: LongString;
begin
  if iP = nil then exit;
  New(P); Move(iP^, P^, sizeOf(TUndoRec));
  case DoKind of
    dkUndo:
      begin
      if UndoInfo = nil then UndoInfo:=New(PDoCollection, Init(dkUndo));
      with iP^ do
        case What of
          udDelChar:
             begin
               Dec(P^.Where.X,Count);
               P^.Str:=NewLongStr(copy(GetLine(Where.Y),P^.Where.X+1,Count));
               UndoInfo^.Insert(P);
             end;
          udInsChar:
             begin
               if Str <> nil then
                 begin
                   P^.Count:=Length(Str^);
                   Inc(P^.Where.X,P^.Count);
                   UndoInfo^.Insert(P);
                 end
               else Dispose(P);
             end;
          udDelLine:
             begin
               P^.Lines := New(PLineCollection, Init(10, 10, True));
               for I:=0 to Count-1 do
                 P^.Lines^.Insert(NewLongStr(GetLine(P^.Where.Y+I)));
               UndoInfo^.Insert(P);
             end;
          udInsLine:
             begin
               P^.Str:=NewLongStr(GetLine(Where.Y));
               UndoInfo^.Insert(P);
             end;
          udDelBlock:
             begin
               P^.Lines := New(PLineCollection, Init(10, 10, True));
               if Lines^.Count>0 then S:=CnvString(Lines^.At(0))else S:='';
               J:=Length(S);
               if Vertical then
                 begin
                   for I:=0 to Lines^.Count-1 do
                     begin
                       S:=copy(GetLine(Where.Y+I),Where.X+1,J);
                       P^.Lines^.Insert(NewLongStr(S));
                     end;
                 end
               else
                 begin
                   for I:=0 to Lines^.Count-1 do
                     begin
                       S:=GetLine(Where.Y+I);
                       if I=0 then
                         S:=copy(S,Where.X+1,MaxLongStringLength)
                       else if I=Lines^.Count-1 then
                         S:=copy(S,1,Length(S)-(J-Where.X));
                       P^.Lines^.Insert(NewLongStr(S));
                     end;
                 end;
               UndoInfo^.Insert(P);
             end;
          udInsBlock, udFormatBlock:
             begin
               I:=Lines^.Count;
               P^.Str:=NewLongStr(Char(Lo(I))+Char(Hi(I))+GetLine(Delta.Y));
               UndoInfo^.Insert(P);
             end;
          udBackDel:
             begin
               P^.Str:=NewLongStr(copy(GetLine(Where.Y),Where.X+1,Count));
               UndoInfo^.Insert(P);
             end;
          udSubDel:
             begin
               P^.Str:=NewLongStr(GetLine(Where.Y));
               UndoInfo^.Insert(P);
             end;
          udSubDelLine:
             begin
               P^.Str:=NewLongStr(GetLine(Where.Y+1));
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
               P^.Count:=Lines^.Count;
               if P^.Count>0 then S:=CnvLongString(Lines^.At(0)) else S:='';
               P^.Width:=Length(S);
               for i:=1 to P^.Count do begin
                j:=Length(CnvLongString(Lines^.At(i-1)));
                if j>P^.Width then P^.Width:=J;
               end;
               UndoInfo^.Insert(P);
             end;
          udReplace,udReplaceAll:
             begin
               S:=GetLine(Where.Y);
               P^.Str:=NewLongStr(Char(Length(Str^)-1)+copy(S,Where.X+1,byte(Str^[1])));
               UndoInfo^.Insert(P);
             end;
          udReplaceChar:
             begin
               S:=GetLine(Where.Y);
               Inc(P^.Where.X,Length(Str^));
               P^.Str:=NewLongStr(copy(S,Where.X+1,Length(Str^)));
               UndoInfo^.Insert(P);
             end;
          udReplaceBlock,udClearBlock:
             begin
               P^.Lines:=New(PLineCollection, Init(10, 10, True));
               for I := Where.Y to Lines^.Count + Where.Y - 1 do
                 begin
                   S := GetLine(I);
                   J := length(CnvLongString(Lines^.At(I-Where.Y)));
                   P^.Lines^.Insert(NewLongStr(copy(S,Where.X+1,J)));
                 end;
               UndoInfo^.Insert(P);
             end;
          udStrModified:
             begin
               P^.Str:=NewLongStr(GetLine(Where.Y));
               UndoInfo^.Insert(P);
             end;
          udDupeLine:
             begin
               UndoInfo^.Insert(P);
             end;
        end; {case of iP^.What/dkUndo}
      end;
    dkRedo:
      begin
      if RedoInfo = nil then RedoInfo:=New(PDoCollection, Init(dkRedo));
      with iP^ do
        case What of
          udDelChar:
             begin
               P^.Count:=Length(Str^);
               Inc(P^.Where.X,P^.Count);
               RedoInfo^.Insert(P);
             end;
          udInsChar:
             begin
               Dec(P^.Where.X,Count);
               P^.Str:=NewLongStr(copy(GetLine(Where.Y),P^.Where.X+1,Count));
               RedoInfo^.Insert(P);
             end;
          udDelLine:
             begin
               P^.Count:=Lines^.Count;
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
               if Lines^.Count>0 then S:=CnvLongString(Lines^.At(0))else S:='';
               J := Length(S);
               if Vertical then
                 if InsM then
                   begin
                     for I:= 0 to Lines^.Count-1 do
                       P^.Lines^.Insert(NewLongStr(S));
                   end
                 else
                   begin
                     for I:=0 to Lines^.Count-1 do
                       begin
                         S:=copy(GetLine(Where.Y+I),Where.X+1,J);
                         P^.Lines^.Insert(NewLongStr(S));
                       end;
                   end
               else
                 begin
                   S:=GetLine(P^.Where.Y);
                   for I:=0 to Lines^.Count-1 do
                     P^.Lines^.Insert(NewLongStr(S));
                 end;
               RedoInfo^.Insert(P);
             end;
          udInsBlock, udFormatBlock:
             begin
               P^.Lines := New(PLineCollection, Init(10, 10, True));
               for I:=1 to byte(Str^[2])*256+byte(Str^[1]) do
                 P^.Lines^.Insert(NewLongStr(GetLine(Delta.Y+I-1)));
               RedoInfo^.Insert(P);
             end;
          udBackDel:
             begin
               P^.Count:=Length(Str^);
               RedoInfo^.Insert(P);
             end;
          udSubDel:
             begin
               P^.Str:=NewLongStr(GetLine(P^.Where.Y));
               RedoInfo^.Insert(P);
             end;
          udSubDelLine:
             begin
               S:=GetLine(Where.Y);
               P^.Str:=NewLongStr(S);
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
               for I:=1 to Count do
               begin
                 S:=Copy(GetLine(I+Where.Y-1),Where.X+1,Width);
                 P^.Lines^.Insert(NewLongStr(S));
               end;
               P^.Vertical:=true; P^.InsM:=true;
               RedoInfo^.Insert(P);
             end;
          udReplace,udReplaceAll:
             begin
               S:=GetLine(Where.Y);
               P^.Str:=NewLongStr(Char(Length(Str^)-1)+copy(S,Where.X+1,byte(Str^[1])));
               RedoInfo^.Insert(P);
             end;
          udReplaceChar:
             begin
               S:=GetLine(Where.Y);
               Dec(P^.Where.X,Length(Str^));
               P^.Str:=NewLongStr(copy(S,P^.Where.X+1,Length(Str^)));
               RedoInfo^.Insert(P);
             end;
          udReplaceBlock,udClearBlock:
             begin
               P^.Lines:=New(PLineCollection, Init(10, 10, True));
               for I := Where.Y to Lines^.Count + Where.Y - 1 do
                 begin
                   S := GetLine(I);
                   J := length(CnvLongString(Lines^.At(I-Where.Y)));
                   P^.Lines^.Insert(NewLongStr(copy(S,Where.X+1,J)));
                 end;
               RedoInfo^.Insert(P);
             end;
          udStrModified:
             begin
               P^.Str:=NewLongStr(GetLine(Where.Y));
               RedoInfo^.Insert(P);
             end;
          udDupeLine:
             begin
               RedoInfo^.Insert(P);
             end;
        end; {case of iP^.What/dkRedo}
      end;
    end; {case of DoKind}
end; {-$VOL}

constructor TFileEditor.Load(var S: TStream);
begin
  inherited Load(S);
  {$IFDEF COLORER} {Cat}
  UseExternalColorer:=False;
  SwitchExternalColorer(EdOpt.HiLite);
  {$ENDIF} {/Cat}
  MILoad(@Self, S);
  {$IFDEF REGEXP}
  RegExp:=New(PRegExp, Init);
  {$ENDIF}
end;

procedure TFileEditor.Store(var S: TStream);
begin
 inherited Store(S);
 MIStore(@Self, S);
end;

procedure TFileEditor.Awaken;
begin
 MIAwaken(@Self);
end;

function TFileEditor.GetPalette;
 const s : String[Length(CFileEditor)] = CFileEditor;
begin
 GetPalette:=@s;
end;

{ TFileEditor }
constructor TFileEditor.Init;
var
{ FileToView: Text; }
  Line: LongString;
{ MaxWidth: Integer; }
{ p: PString; }
{ Nm: String; }
{ Xt: String; }
  I: Integer;
begin
  inherited Init(Bounds);
  InsertMode := True; {Cat}
  DrawMode := 0; {Cat}
  BlockVisible := false; {Cat}
  Mark.Assign(0,0,0,0); {Cat}
  LastDir := -1;
  HelpCtx := hcEditor;
  UndoInfo := nil;
  RedoInfo := nil;
  HScroll := AHScrollBar;
  VScroll := AVScrollBar;
  GrowMode := gfGrowHiX + gfGrowHiY;
  Options := Options or ofSelectable;
  SearchActive := false; UnMark := true;
  EventMask := $FFFF;
  IsValid := True;

  EdOpt.HiliteColumn := EditorDefaults.EdOpt  and ebfHCl <> 0;
  EdOpt.HiliteLine   := EditorDefaults.EdOpt  and ebfHLn <> 0;
  EdOpt.AutoIndent   := EditorDefaults.EdOpt  and ebfAId <> 0;
  VertBlock          := EditorDefaults.EdOpt  and ebfVBl <> 0;
  EdOpt.BackIndent   := EditorDefaults.EdOpt  and ebfBSU <> 0;
  OptimalFill        := EditorDefaults.EdOpt  and ebfOfl <> 0;
  EdOpt.AutoJustify  := EditorDefaults.EdOpt  and ebfJwr <> 0;
  EdOpt.AutoBrackets := EditorDefaults.EdOpt  and ebfAbr <> 0;
  EdOpt.AutoWrap     := EditorDefaults.EdOpt  and ebfAwr <> 0;
  TabReplace         := (EditorDefaults.EdOpt{$IFNDEF OS_DOS} shl 2{$ENDIF}) and ebfTRp <> 0;
  EdOpt.SmartTab     := EditorDefaults.EdOpt2 and ebfSmt <> 0;

  Val(EditorDefaults.RM, EdOpt.RightSide, I); if I <> 0 then EdOpt.RightSide := 76;
  Val(EditorDefaults.LM, EdOpt.LeftSide, I); if I <> 0 then EdOpt.LeftSide := 0;
  Val(EditorDefaults.PM, EdOpt.InSide, I); if I <> 0 then EdOpt.InSide := 5;
  SmartPad := FileName = 'SmartPad';
  ClipBrd := FileName = 'Clipboard'; {-$VOL}
  FillChar(MarkPos, SizeOf(MarkPos), $FF);
  if SmartPad then
    begin
      Line := GetEnv('SMARTPAD');
      if (Line = '') then Line := SourceDir;
      if (Line[Length(Line)]<>'\') then Line := Line+'\';
      FileName := Line+FileName+'.DN';
    end else if ClipBrd then FileName := ''
    else Line := FileName;
 {FileLines := GetCollector(3000, 100);}
  FileLines := New(PLineCollection, Init(300, 1000, True)); {-SBlocks}
  Macros := New(PCollection, Init(10,10));

(*if UpperCase(DefCodePage) = 'WIN' then KeyMap := kmAnsi
   else if UpperCase(DefCodePage) = 'KOI' then KeyMap := kmKoi8r
    else KeyMap := kmAscii; {-$VIV}*)
  MenuItemStr[True]:=NewStr(GetString(dlMenuItemOn));
  MenuItemStr[False]:=NewStr(GetString(dlMenuItemOff));
  {$IFDEF REGEXP}
  RegExp:=New(PRegExp, Init);
  {$ENDIF}
  {$IFDEF COLORER} {Cat}
  UseExternalColorer:=False;
  SwitchExternalColorer(EdOpt.HiLite);
  {$ENDIF} {/Cat}
end;

destructor TFileEditor.Done;
begin
  DisposeStr(MenuItemStr[False]);
  DisposeStr(MenuItemStr[True]);
  {$IFDEF REGEXP}
  if RegExp   <>nil then begin Dispose(RegExp,Done);    RegExp   :=nil; end;
  {$ENDIF}
  if FileLines<>nil then begin Dispose(FileLines,Done); FileLines:=nil; end;
  if UndoInfo <>nil then begin Dispose(UndoInfo,Done);  UndoInfo :=nil; end;
  if RedoInfo <>nil then begin Dispose(RedoInfo,Done);  RedoInfo :=nil; end;
  if Macros   <>nil then begin Dispose(Macros,Done);    Macros   :=nil; end;
  if Locker   <>nil then begin Dispose(Locker,Done);    Locker   :=nil; end;
  if SmartPad then begin SmartWindowPtr     := @SmartWindow;     SmartWindow     := nil; end;
  if ClipBrd  then begin ClipboardWindowPtr := @ClipboardWindow; ClipboardWindow := nil; end;
  {$IFDEF COLORER} {Cat}
  SwitchExternalColorer(False);
  {$ENDIF} {/Cat}
  inherited Done;
end;

procedure TFileEditor.StoreUndoInfo;
 var P, P1: PUndoRec;
     S: LongString;
     Bl: Boolean;
begin
  if not MemOK then begin Dispose(UndoInfo,Done); UndoInfo:=nil end;
  if UndoInfo = nil then UndoInfo := New(PDoCollection, Init(dkUndo));
  if RedoInfo <> nil then RedoInfo^.FreeAll;
  if UndoInfo^.Count > 0
   then P1:=UndoInfo^.At(UndoInfo^.Count-1)
   else P1:=nil;
  if ValidBlock then
  case What of
   udDelChar: if not VertBlock then
              begin
                if (Mark.A.Y = Where.Y) and (Where.X < Mark.A.X) then Dec(Mark.A.X);
                if (Mark.B.Y = Where.Y) and (Where.X < Mark.B.X) then Dec(Mark.B.X);
              end;
   udReplaceChar:;
   udBackDel: if not VertBlock then
              begin
                if (Mark.A.Y = Where.Y) and (Where.X <= Mark.A.X) then Dec(Mark.A.X);
                if (Mark.B.Y = Where.Y) and (Where.X <= Mark.B.X) then Dec(Mark.B.X);
              end;
   udInsChar: if not VertBlock then
              begin
                if (Mark.A.Y = Where.Y) and (Where.X < Mark.A.X) then Inc(Mark.A.X);
                if (Mark.B.Y = Where.Y) and (Where.X < Mark.B.X) then Inc(Mark.B.X);
              end;
   udInsLine, udDupeLine: begin
                 if Mark.A.Y > Where.Y then Inc(Mark.A.Y);
                 if Mark.B.Y > Where.Y then Inc(Mark.B.Y);
                 if (Mark.A.Y = Where.Y) and (Where.X <= Mark.A.X) then
                   begin
                     Inc(Mark.A.Y);
                     if What <> udDupeLine then Mark.A.X := Mark.A.X - Where.X;
                   end;
                 if (Mark.B.Y = Where.Y) and (Where.X < Mark.B.X) then
                   begin
                     Inc(Mark.B.Y);
                     if What <> udDupeLine then Mark.B.X := Mark.B.X - Where.X;
                   end;
               end;
   udSubDel,
   udSubDelLine,
   udDelLine:  begin
                 if (Where.Y = Mark.A.Y) and (Where.Y = Mark.B.Y) and
                    (What = udDelLine) then Mark.A := Mark.B;
                 if Mark.A.Y > Where.Y then
                    begin
                      Dec(Mark.A.Y);
                      if (What <> udDelLine) and not VertBlock and
                         (Mark.A.Y = Where.Y) and (Where.X > Mark.A.X) then Mark.A.X := Where.X;
                    end;
                 if Mark.B.Y > Where.Y then
                    begin
                      Dec(Mark.B.Y);
                      if (What <> udDelLine) and not VertBlock and
                         (Mark.B.Y = Where.Y) and (Where.X > Mark.B.X) then Mark.B.X := Where.X;
                    end;
               end;
  end;

  case What of
   udDelChar: if (P1 <> nil) and (P1^.What = udDelChar) and (P1^.Where.Y = Where.Y) and
                 (P1^.Where.X = Where.X) then
                  begin S := P1^.Str^+Char(Info); DisposeLongStr(P1^.Str); P1^.Str := NewLongStr(S); end
                  else begin
                        S := '' + Char(Info); New(P); P^.What := What;
                        P^.Where := Where; P^.Str := NewLongStr(S);
                        P^.KeyMap := KeyMap; {-$VIV}
                        UndoInfo^.Insert(P); Inc(UndoTimes);
                       end;
   udReplaceChar: if (P1 <> nil) and (P1^.What = What) and (P1^.Where.Y = Where.Y) and
                 (Where.X = P1^.Where.X) then
                  begin S := P1^.Str^+Char(Info); Inc(P1^.Where.X);
                        DisposeLongStr(P1^.Str); P1^.Str := NewLongStr(S); end
                  else begin
                        S := '' + Char(Info); New(P); P^.What := What;
                        P^.Where := Where; P^.Str := NewLongStr(S);
                        Inc(P^.Where.X);
                        P^.KeyMap := KeyMap; {-$VIV}
                        UndoInfo^.Insert(P); Inc(UndoTimes);
                       end;
   udBackDel: if (P1 <> nil) and (P1^.What = What) and (P1^.Where.Y = Where.Y) and
                 (P1^.Where.X = Where.X) then
                 begin S := Char(Info)+P1^.Str^; Dec(P1^.Where.X);
                       DisposeLongStr(P1^.Str); P1^.Str := NewLongStr(S); end
                 else begin
                        S := '' + Char(Info); New(P); P^.What := What;
                        P^.Where := Where; P^.Str := NewLongStr(S);
                        Dec(P^.Where.X);
                        P^.KeyMap := KeyMap; {-$VIV}
                        UndoInfo^.Insert(P); Inc(UndoTimes);
                       end;
   udInsChar: if (P1 <> nil) and (P1^.What = What) and (P1^.Where.Y = Where.Y) and
                 (P1^.Where.X = Where.X) then
                  begin Inc(P1^.Count); Inc(P1^.Where.X) end
                  else begin
                        New(P); P^.What := What;
                        P^.Where := Where; Inc(P^.Where.X);
                        P^.Count := 1;
                        P^.KeyMap := KeyMap; {-$VIV}
                        UndoInfo^.Insert(P); Inc(UndoTimes);
                       end;
   udDupeLine: begin
                   New(P); P^.What := What;
                   P^.Where := Where;
                   P^.Count := 1;
                   P^.KeyMap := KeyMap; {-$VIV}
                   UndoInfo^.Insert(P); Inc(UndoTimes);
               end;
   udDelLine: if (P1 <> nil) and (P1^.What = What) and (P1^.Where.Y = Where.Y) then
                   P1^.Lines^.Insert(NewLongStr(LongString(Info))) else
              begin
                New(P); P^.What := What;
                P^.Where := Where;
                P^.Lines := New(PLineCollection, Init(10, 10, True));
                P^.Lines^.Insert(NewLongStr(LongString(Info)));
                P^.KeyMap := KeyMap; {-$VIV}
                UndoInfo^.Insert(P); Inc(UndoTimes);
              end;
   udInsLine, udSubDel, udSubDelLine, udInsBlock, udReplace, udFormatBlock, udReplaceAll:
              begin
                New(P); P^.What := What;
                P^.Where := Where;
                P^.Str := NewLongStr(LongString(Info));
                P^.KeyMap := KeyMap; {-$VIV}
                UndoInfo^.Insert(P); Inc(UndoTimes);
              end;
   udReplaceBlock, udClearBlock:
                   begin
                    New(P); P^.What := What;
                    P^.Where := Where;
                    P^.Lines := PCollection(Info);
                    P^.KeyMap := KeyMap; {-$VIV}
                    UndoInfo^.Insert(P); Inc(UndoTimes);
                   end;
   udInsVertBlock: begin
                    New(P); P^.What := What;
                    P^.Where := Where;
                    P^.Count := LongInt(Info) and $FFFF;
                    P^.Width := LongInt(Info) shr 16;
                    P^.KeyMap := KeyMap; {-$VIV}
                    UndoInfo^.Insert(P); Inc(UndoTimes);
                   end;
   udDelBlock: begin
                New(P); P^.What := What;
                P^.Where := Where;
                P^.Vertical := VertBlock;
                P^.InsM := InsertMode; {-$VOL}
                P^.Lines := PCollection(Info);
                P^.KeyMap := KeyMap; {-$VIV}
                UndoInfo^.Insert(P); Inc(UndoTimes);
               end;
   udIndentBlock, udUnindentBlock
                : if (P1 <> nil) and (P1^.What = What) and (P1^.Block.A.Y = TRect(Info).A.Y) and
                     (P1^.Block.A.Y = TRect(Info).B.Y) then
                  begin Inc(P1^.Count); end else
                  begin
                   New(P); P^.What := What;
                   P^.Where := Where;
                   P^.Block := TRect(Info);
                   P^.Count := 1;
                   P^.KeyMap := KeyMap; {-$VIV}
                   UndoInfo^.Insert(P); Inc(UndoTimes);
                  end;
   udStrModified:  begin
                      New(P); P^.What := What;
                      P^.Where := Where; Inc(P^.Where.X);
                      P^.Count := 1;
                      P^.Str := NewLongStr(LongString(Info));
                      P^.KeyMap := KeyMap; {-$VIV}
                      UndoInfo^.Insert(P); Inc(UndoTimes);
                    end;
  end;
  CalcMenu;
end;

procedure TFileEditor.CalcMenu;
 var GC : TCommandSet;
     MI : PMenuItem;

 Procedure SetM( B : Boolean);
  var G : LongString;
 begin
  if MI = nil then Exit;
  G:=MenuItemStr[B]^;
  if MI^.Param^ <> G then
  begin
    DisposeStr(MI^.Param);
    MI^.Param := NewStr(G);
  end;
  MI := MI^.Next;
 end;

 var BlkC: TCommandSet;

begin
  if (Owner = nil) or (PEditWindow(Owner)^.MenuBar = nil) then Exit;
  BlkC := [cmCopy,cmCut,cmClear,cmBlockWrite,cmFJustify, cmCopyBlock, cmMoveBlock,
           cmFRight,cmFLeft,cmFCenter,cmPrintBlock, cmCalcBlock, cmSortBlock,
           cmRevSortBlock, cmUpcaseBlock, cmLowcaseBlock, cmCapitalizeBlock,
           cmIndentBlock, cmUnIndentBlock, cmRusEngConv]; {-$VIV}
  OldBlockValid := (ValidBlock and BlockVisible);
  if OldBlockValid then EnableCommands(BlkC)
                   else DisableCommands(BlkC);
  if (UndoInfo <> nil) and (UndoInfo^.Count > 0)
     then EnableCommands([cmUndo]) else DisableCommands([cmUndo]);
{-$VOL begin}
  if (RedoInfo <> nil) and (RedoInfo^.Count > 0)
     then EnableCommands([cmRedo]) else DisableCommands([cmRedo]);
{-$VOL end}
  if (Clipboard <> nil) and (Clipboard^.Count > 0) or
     ((SystemData.Options and ossUseSysClip <> 0) and GetWinClipSize)
  then EnableCommands([cmPaste])
  else DisableCommands([cmPaste]);
  if OptMenu <> Nil then
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
  GetCommands(GC);
  PEditWindow(Owner)^.MenuBar^.SetCommands(GC);
  SetCommands(GC);
end;

procedure TFileEditor.SetState;
begin
 inherited SetState(AState,Enable);
 if (AState and (sfActive + sfFocused + sfSelected + sfVisible) <> 0)
   or (OldBlockValid xor (ValidBlock and BlockVisible)) then CalcMenu;
  {if not GetState(sfFocused) then CalcMenu;}
 if AState and sfActive <> 0 then
  if GetState(sfActive+sfSelected) then
   begin
    if HScroll <> nil then HScroll^.Show; HScroll^.MakeFirst;
    if VScroll <> nil then VScroll^.Show;
    DrawView;
    EnableCommands([cmViewFile]);
   end else
   begin
    if HScroll <> nil then HScroll^.Hide;
    if VScroll <> nil then VScroll^.Hide;
    DrawView;
   end;
{ if (InfoL<>nil) then InfoL^.Draw;}
{ if (BMrk<>nil)  then BMrk^.Draw; }
end;

procedure TFileEditor.ScrollTo;
begin
 if HScroll <> nil then HScroll^.SetValue(DeltaX);
 if VScroll <> nil then VScroll^.SetValue(DeltaY);
end;

        {-DataCompBoy-}
function  TFileEditor.LimitX;
 begin if HScroll <> nil then LimitX:=HScroll^.Max else LimitX:=0; end;

function  TFileEditor.LimitY;
 begin if VScroll <> nil then LimitY:=VScroll^.Max else LimitY:=0; end;
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
 var R: TRect;
     P: PWindow;
     S: String;
begin
 S := GetFileNameDialog(x_x, GetString(dlED_OpenFile),
                               GetString(dlOpenFileName),
                        fdOpenButton + fdHelpButton, hsEditOpen);
 if S = '' then Exit;
 Desktop^.GetExtent(R);
 S := lFExpand(GetPath(S))+GetName(lFExpand(S));
 Application^.InsertWindow(New(PEditWindow, Init(R, S)));
end;
        {-DataCompBoy-}

procedure TFileEditor.ChangeBounds;
begin
 inherited ChangeBounds(R);
 SetLimits;
end;

procedure TFileEditor.SetLimits;
begin
 if HScroll <> nil then HScroll^.SetParams(Delta.X, 0, MaxLongStringLength, 1, 1);
 if VScroll <> nil then VScroll^.SetParams(Delta.Y, 0, FileLines^.Count-1, Size.Y, 1);
end;

function TFileEditor.GetSelection;
 var  P: PCollection;
      S: LongString;
      I: LongInt;
begin
 GetSelection := Nil; MemEnough := true ;
 if not (BlockVisible and ValidBlock) or LowMemory then Exit;
 P := New(PLineCollection, Init(Mark.B.Y-Mark.A.Y+1, 5, True));
 for I := Mark.A.Y to Mark.B.Y do
  begin
   S := GetLine(I);
   if VertBlock or (Mark.A.Y = Mark.B.Y) then
     begin
      S := Copy(S, Mark.A.X + 1, Mark.B.X - Mark.A.X);
      if VertBlock and (Mark.B.X - Mark.A.X > Length(S)) then
         S := S + LongStrg(' ', Mark.B.X - Mark.A.X - Length(S));
     end
    else if I = Mark.A.Y then S := Copy(S, Mark.A.X + 1, MaxLongStringLength)
         else if I = Mark.B.Y then S := Copy(S, 1, Mark.B.X);
{ P^.Insert(NewLongStr(S)); }
  with P^ do AtInsert(Count, NewLongStr(S)); {AK155}

{AK155: MemOK - это нечто странное. Там, в конечном итоге,
проверяется на nil некий Linker, которому нигде ничего не
присваивается, так что он заведомо nil. А исключение этой
бессмысленной проверки ускоряет работу во _много_ раз (более
20, точно мерять не хватило терпения ). }

   if LowMemory { or not MemOK} then
    begin
     Dispose(P,Done);
     MemEnough := false ;
     Exit;
    End;
  end;
 GetSelection := P;
end;

function TFileEditor.Valid(Command: Word): Boolean;
 var I: Word;
     P: Pointer;
     S: String;
     V: Boolean;
begin
  Valid := true; V := true;
  if Command = cmValid then Valid := isValid;
  if (SmartPad or ClipBrd) then
   begin
     if Modified then Message(@Self, evCommand, cmSaveText, nil);
     PEditWindow(Owner)^.ModalEnd := True;
     Exit;
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
          S := Cut(EditName, 30);
          P := @S;
          I := MessageBox(GetString(dlQueryModified), @P, mfWarning+mfYesNoCancel);
          if I = cmYes
           then Message(@Self, evCommand, cmSaveText, nil)
           else if I = cmNo then Modified := false;
          V := not ((I = cmCancel) or (I = cmYes) and Modified);
         end;
      Valid := V;
      if V and not(SmartPad or ClipBrd)and (Owner <> nil) then StoreEditInfo(Owner);
    end;
end;

function TFileEditor.ValidBlock;
begin
 if VertBlock then ValidBlock := ((Mark.A.Y <= Mark.B.Y) and (Mark.A.X < Mark.B.X))
              else ValidBlock := (Mark.A.Y < Mark.B.Y) or
                                 ((Mark.A.Y = Mark.B.Y) and (Mark.A.X < Mark.B.X));
end;

{Cat: переделал для поиска с регэкспами}
function TFileEditor.Search;
label
  1, LExit, _LExit, EndReplace;
var
  I, J, TrX, StX: LongInt;
  D, DD: TPoint;
  T: TRect;
  W, S, S1: LongString;
  F, Prompt: Boolean;
  OldMark: TRect;
  NumRep: LongInt;
  Dir: Integer;
  OldCache: Boolean;
  Tmr: TEventTimer;
  AllCP: Boolean; {-$VIV 14.05.99}

  procedure _DrawViews;
  begin
    DrawView;
    HScroll^.DrawView;
    VScroll^.DrawView;
  end;

begin
  SearchActive := true;
  NewTimer(Tmr, 0);
  Marking := false; OldMark := Mark;
  if (SearchData.Scope = 1 {отмеченный текст}) and not (BlockVisible and ValidBlock) then
    goto LExit;
  Search := false; F := true;
  Prompt := (SearchData.What <> #0) and (SearchData.Options and efoReplacePrompt <> 0); {запрос на замену}
  D.Assign(StartX, StartY);
  W := SearchData.Line;
  if W = '' then
    goto LExit;
  if SearchData.Options and efoCaseSens = 0 then {не различать регистр}
    UpLongStr(W);
  AllCP := (SearchData.What = #0) and (SearchData.Options and efoAllCP <> 0 {во всех кодировках});
  UnMark := false;
  NumRep := 0;
  Dir := 1 - 2 * SearchData.Dir; {направление поиска: +1 вперёд, -1 назад}
  while (Dir > 0) and (D.Y < FileLines^.Count) or
        (Dir < 0) and (D.Y >= 0) do
    begin
1:
      S1 := GetLine(D.Y); TrX := Length(S1); {S := S1;}
      if (Dir < 0) then {поиск назад}
        begin
          if (D.X > Length(S1)) then
            D.X := Length(S1);
          StX := 1; TrX := D.X;
          if SearchData.Scope = 1 then {отмеченный текст}
            begin
              Mark := OldMark;
              if D.Y < Mark.A.Y then
                Break;
              if D.Y > Mark.B.Y then
                begin
                  Dec(D.Y);
                  D.X := MaxLongStringLength;
                  Continue;
                end;
              if (D.Y = Mark.B.Y) and (D.Y = Mark.A.Y) or VertBlock then
                if StX < Mark.A.X then
                  StX := Mark.A.X + 1;
              if (D.Y = Mark.B.Y) or VertBlock then
                if TrX > Mark.B.X then
                  TrX := Mark.B.X;
            end;
          if TrX < StX then
            I := 0 {-$VIV 14.05.99}
          else
            {$IFDEF REGEXP}
            if SearchData.Options and efoRegExp <> 0 then {поиск регэкспа - пока работает только вперёд}
              begin
{Cat:todo}      MessageBox('RegExp BackSearch is not implemented yet', nil, mfError+mfOkButton);
                goto LExit;
              end
            else
            {$ENDIF}
              if AllCP then
                I := BackSearchForAllCP(W, S1[StX], TrX - StX + 1, SearchData.Options and efoCaseSens <> 0)
              else
                I := BackSearchFor(W, S1[StX], TrX - StX + 1, SearchData.Options and efoCaseSens <> 0);
        end
      else {поиск вперёд}
        begin
          StX := D.X + 1;
          if SearchData.Scope = 1 then {отмеченный текст}
            begin
              Mark := OldMark;
              if D.Y < Mark.A.Y then
                begin
                  Inc(D.Y);
                  D.X := 0;
                  Continue;
                end;
              if D.Y > Mark.B.Y then
                Break;
              if (D.Y = Mark.B.Y) and (D.Y = Mark.A.Y) or VertBlock then
                if StX < Mark.A.X then
                  StX := Mark.A.X + 1;
              if (D.Y = Mark.B.Y) or VertBlock then
                TrX := Mark.B.X;
            end;
          if TrX < StX then
            I := 0 {-$VIV 14.05.99}
          else
            {$IFDEF REGEXP}
            if SearchData.Options and efoRegExp <> 0 then {поиск регэкспа}
              begin
                I := 0;
                if RegExp^.Execute({$IFDEF USELONGSTRING} PChar(S1) + StX - 1 {$ELSE} @S1[StX] {$ENDIF}, TrX - StX + 1) and (RegExp^.FLength <> 0) then
                  I := RegExp^.FStart + 1
                else
                  if RegExp^.FStatus <> resOK then
                    begin
                      ScrollTo(D.X, D.Y); { go to aborted line                 }
                      goto LExit;         { press Ctrl-L to continue searching }
                    end;
              end
            else
            {$ENDIF}
              if AllCP then
                I := SearchForAllCP(W, S1[StX], TrX - StX + 1, SearchData.Options and efoCaseSens <> 0)
              else
                I := SearchFor(W, S1[StX], TrX - StX + 1, SearchData.Options and efoCaseSens <> 0);
        end;

      if (I > 0) and (SearchData.Options and efoWholeWords <> 0) then {что-то нашли, надо проверить, отдельное ли это слово}
        if not (((StX-1+I = 1) or (S1[StX-1+I-1] in BreakChars)) and
           ((StX - 1 + I + Length(W) > Length(S1)) or (S1[StX-1 + I + Length(W)] in BreakChars))) then
          begin
            if Dir > 0 then
              D.X := StX+I
            else
              D.X := StX+I-1;
            goto 1;
          end;

      if I > 0 then {что-то нашли, надо нарисовать это на экране}
        begin
          Search := true; F := false;
          DD.Y := D.Y; DD.X := StX + I - 2; D := DD;
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
          if TimerExpired(Tmr) then
            begin
              VScroll^.DrawView;
              NewTimer(Tmr, 1);
            end;
          if (D.Y - Pos.Y > Size.Y) then
            Pos.Y := D.Y - Size.Y div 2
          else
            if (D.Y < Pos.Y) then
              Pos.Y := Max(0, D.Y-5);
          if (D.X - Pos.X > Size.X - 10) then
            Pos.X := D.X - Size.X div 2
          else
            if (D.X < Pos.X) then
              Pos.X := Max(0, D.X-5);
          if (SearchData.What = #0) or Prompt
            then _DrawViews;
          if SearchData.What <> #0 then
            begin
              J := cmYes;
              if Prompt then
                begin
                  I := 50;
                  T.A.Y := Delta.Y - Pos.Y;
                  MakeGlobal(T.A, T.A);
                  Desktop^.MakeLocal(T.A, T.A);
                  T.A.X := (Desktop^.Size.X - I) div 2; T.B.X := T.A.X + I;
                  if (T.A.Y <= (Desktop^.Size.Y) div 2) then
                    T.A.Y := (Desktop^.Size.Y+4) div 2
                  else
                    T.A.Y := (Desktop^.Size.Y-18) div 2;
                  T.B.Y := T.A.Y + 8;
                  J := MessageBoxRect(T, GetString(dlQueryReplace), nil,
                    mfQuery+mfYesButton+mfAllButton+mfNoButton+mfCancelButton);
                  if J = cmOK then
                    begin
                      J := cmYes;
                      ReplaceAll := true;
                      Prompt := false;
                    end;
                end;
              if J = cmYes then
                begin
                  Modified := true;
                  Inc(NumRep); {piwamoto}
                  {$IFDEF REGEXP}
                  if SearchData.Options and efoRegExp <> 0 then
                    S := Copy(S1, DD.X+1, RegExp^.FLength)
                  else
                  {$ENDIF}
                    S := Copy(S1, DD.X+1, Length(W));
                  Insert(Char(Length(SearchData.What)), S, 1);
                  if ReplaceAll then
                    StoreUndoInfo(udReplaceAll, DD, S) {-$VOL}
                  else
                    StoreUndoInfo(udReplace, DD, S);
                  {$IFDEF REGEXP}
                  if SearchData.Options and efoRegExp <> 0 then
                    Delete(S1, DD.X+1, RegExp^.FLength)
                  else
                  {$ENDIF}
                    Delete(S1, DD.X+1, Length(W));
                  Insert(SearchData.What, S1, DD.X+1);
                  ModifyLine(Delta.Y, S1, true);
                  WorkModified := false;
                  if Dir > 0 then
                    {$IFDEF REGEXP}
                    if SearchData.Options and efoRegExp <> 0 then
                      Dec(Delta.X, RegExp^.FLength - Length(SearchData.What))
                    else
                    {$ENDIF}
                      Dec(Delta.X, Length(W) - Length(SearchData.What));
                  if (SearchData.What = #0) or Prompt then
                    ScrollTo(Delta.X, Delta.Y);
                  D := Delta;
                  if (SearchData.What = #0) or Prompt then
                    _DrawViews;
                end;
                if ReplaceAll then
                  begin  {AK155}
                    if J <> cmCancel then
                      goto 1;
                    Mark := OldMark;
                    goto EndReplace;
                  end
                else begin Mark := OldMark; goto LExit; end;
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
          D.X := 0; Inc(D.Y);
        end
      else
        begin
          D.X := MaxLongStringLength; Dec(D.Y);
        end;
    end;

  if F then
    begin
      SearchActive := false;
      MessageBox(GetString(dlDBViewSearchNot), nil, mfInformation+mfOKButton);
    end
  else
    Mark := OldMark;
  UnMark := true;

EndReplace:
  if NumRep > 0 then
    begin
      MessageBox(GetString(dlReplacesMade), @NumRep, mfOkButton+mfInformation);
      ScrollTo(Delta.X, Delta.Y);
    end;

LExit:
  _DrawViews;

_LExit:
  SearchActive := false;
end;
{/Cat}

procedure TFileEditor.InsertBlock;
 var I,Q: LongInt;
     S, S1, S2: LongString;
     P: PLongString;
     L,J: LongInt;
     LL: PCollection;
begin
 if (ABlock = nil) or (ABlock^.Count = 0) then Exit;
 Modified := true;
 if SaveUndo then
  begin
    Mark.B := Delta; Mark.A := Delta; BlockVisible := true;
    Inc(Mark.B.Y, Max(0, ABlock^.Count - 1));
  end;
 if VertBlock then
  begin
   P := ABlock^.At(0);
   if P <> nil then S := P^ else S := '';
   I := Length(S);
   for J:=1 to ABlock^.Count do begin
    Q:=Length(CnvLongString(ABlock^.At(J-1)));
    if Q>I then I:=Q;
   end;
   Q := I;
   L := LongInt(I);
   L := L shl 16;
   L := L or ABlock^.Count;
   if SaveUndo then
     if InsertMode then StoreUndoInfo(udInsVertBlock, Delta, L)
      else
       begin
        P := ABlock^.At(0); if P = nil then S := '' else S := P^;
        LL := New(PLineCollection, Init(ABlock^.Count, 10, True));
        if LL <> nil then
         begin
           for I := 0 to ABlock^.Count do
             LL^.Insert(NewLongStr(LongAddSpace(Copy(GetLine(I+Delta.Y), Delta.X+1, Length(S)),I)));
           StoreUndoInfo(udReplaceBlock, Delta, LL)
         end;
       end;
   for I := 1 to ABlock^.Count do
    begin
     P := ABlock^.At(I-1);
     S1 := LongAddSpace(CnvLongString(P),Q);
     if SaveUndo then Mark.B.X := Mark.A.X + Length(S1);
     if I + Delta.Y - 1 = FileLines^.Count then
      FileLines^.Insert(NewLongStr(LongAddSpace('',Q)));
     S2 := GetLine(I+Delta.Y-1);
     if not InsertMode then Delete(S2, Delta.X + 1, Length(S1));
     if Length(S2) < Delta.X then S2 := S2 + LongStrg(' ', Delta.X - Length(S2));
     Insert(S1, S2, Delta.X + 1);
     ModifyLine(I+Delta.Y-1, S2, true);
    end
   end
   else
  begin
   I := ABlock^.Count;
   S := Char(Lo(I)) + Char(Hi(I)) + GetLine(Delta.Y);
   if SaveUndo then StoreUndoInfo(udInsBlock, Delta, S);
   Delete(S, 1, 2);
   if Delta.X > Length(S) then S := S + LongStrg(' ', Delta.X - Length(S));
   S1 := Copy(S, 1, Delta.X); S2 := Copy(S, Delta.X+1, MaxLongStringLength);
   FileLines^.AtFree(Delta.Y);
   for I := 1 to ABlock^.Count do
   begin
     P := ABlock^.At(I-1); if P <> nil then S := P^ else S := '';
     if I = 1 then S := S1 + S;
     if I = ABlock^.Count then
       begin if SaveUndo then Mark.B.X := Length(S); S := S + S2; end;
     KeyMapAtInsert(Delta.Y + I - 1, NewLongStr(S)); {-$VIV}
    end;
   J:=ABlock^.Count-1;
   If J>0 then
    for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Inc(MarkPos[L].Y, J);
 end;
 SetLimits;
 if GetState(sfVisible) then DrawView;
end;

procedure TFileEditor.Draw;
var
  C, BC, Comments, C1: Byte;
  HP : String[6];
  CC: Array[1..12] of Byte;
  I, A: LongInt;
  S: LongString;
  P: PString;
  WM, BV: Boolean;
  X1, X2: LongInt;
  LPos: Integer;
  Ch: Char;
  B: TDrawBuffer;
  BB: TPoint;
  {$IFDEF PLUGIN}
  Event: TEvent; {Cat}
  FillColorsData: TFillColorsData; {Cat}
  {$ENDIF}

  procedure DrawBlock;
  begin
   X1 := Mark.A.X - Pos.X; if X1 < 0 then X1 := 0;
   X2 := Mark.B.X - Pos.X; if X2 < 0 then X2 := 0;
   if VertBlock or (Mark.A.Y = Mark.B.Y) then
    begin
      if (X1 = X2) or (X1 > Size.X) then Exit;
      if (X2 > Size.X) then X2 := Size.X;
      MoveColor(B[X1], X2-X1, BC);
    end else
        begin
         if A = Mark.A.Y then begin if X1 < Size.X then MoveColor( B[X1], Size.X - X1, BC) end
                else if A = Mark.B.Y then
                        begin
                         if (X2 > Size.X) then X2 := Size.X;
                         MoveColor(B, X2, BC);
                        end else MoveColor(B, Size.X, BC);
        end;
  end;

begin
  {Cat}
  {$IFDEF PLUGIN}
  Event.What := evCommand;
  Event.Command := 65001;
  ProcessEditorEventHook(Event, @Self);
  {$ENDIF}
  {/Cat}

  BB := Pos;
  SearchOnDisplay := false;
  if Pos.X < 0 then Pos.X := 0;
  if Pos.Y < 0 then Pos.Y := 0;
  if Delta.X < 0 then Delta.X := 0;
  if Delta.X > MaxLongStringLength then Delta.X := MaxLongStringLength;
  if Delta.Y >= FileLines^.Count then Delta.Y := FileLines^.Count - 1;
  if Delta.Y < 0 then Delta.Y := 0;
  if Pos.X > Delta.X then Pos.X := Delta.X;
  if Delta.X - Pos.X >= Size.X then Pos.X := Delta.X - Size.X + 8;
  if Pos.Y > Delta.Y then Pos.Y := Delta.Y;
  if Delta.Y - Pos.Y >= Size.Y then Pos.Y := Delta.Y - Size.Y + 1;
  if (BB.X <> Pos.X) or (BB.Y <> Pos.Y) then ChPosition := false;
  if Marking then
     begin
       ChPosition := false;
       Sel.B.X := Delta.X; Sel.B.Y := Delta.Y;
       if LineMarking and (Sel.B.Y < FileLines^.Count - 1) then
        begin
         if Sel.B.Y >= Sel.A.Y then Inc(Sel.B.Y);
         Sel.B.X := 0;
        end;
       Mark := Sel;
       if not VertBlock then
        if (Mark.A.Y > Mark.B.Y) or ((Mark.A.Y = Mark.B.Y) and (Mark.A.X > Mark.B.X))
        then begin Mark.A := Sel.B; Mark.B := Sel.A; end else
        else
        begin
         if Mark.A.X > Mark.B.X then begin Mark.B.X := Mark.A.X; Mark.A.X := Sel.B.X; end;
         if Mark.A.Y > Mark.B.Y then begin Mark.B.Y := Mark.A.Y; Mark.A.Y := Sel.B.Y; end;
        end;
     end;
  BV := BlockVisible and ValidBlock and not SearchActive;
  C := GetColor(1);WM:=not WorkModified;
  BC := GetColor(2);
  for I := 1 to 12 do CC[I] := GetColor(I);
  Comments := GetColor(3);
  {$IFDEF COLORER} {Cat}
  if UseExternalColorer then
    begin
      Dispose(ColorerCollection, Done);
      ColorerCollection := New(PColorerCollection, Init(30,10));
      Colorer.plgProcessEditorEvent(LongInt(@Self), EE_REDRAW, nil);
    end;
  {$ENDIF} {/Cat}
  for I := 0 to Size.Y - 1 do
  begin
    A := Pos.Y + I;
    if BV and not VertBlock then
     begin
        if (A = Mark.A.Y) then
          begin
            if WorkModified and (Mark.A.Y = Delta.Y) then
             if Mark.A.X > Length(WorkString) then Mark.A.X := Length(WorkString)
               else else
             if Mark.A.X > Length(GetLine(Mark.A.Y)) then Mark.A.X := Length(GetLine(Mark.A.Y));
          end;
        if (A = Mark.B.Y) then
         begin
           if WorkModified and (Mark.B.Y = Delta.Y) then
            if Mark.B.X > Length(WorkString) then Mark.B.X := Length(WorkString)
            else else
            if Mark.B.X > Length(GetLine(Mark.B.Y)) then Mark.B.X := Length(GetLine(Mark.B.Y));
         end;
     end;
    if ChPosition and not ((A = Delta.Y) or (A = LastPos.Y)) then Continue;
    if EdOpt.HiliteLine
     {$IFNDEF USELONGSTRING}
       or ((Length(WorkString) >= MaxLongStringLength) and RecombineLongLines) {JO}{!!!}
     {$ENDIF}
         then
     if A = Delta.Y then
     begin
       C:=CC[4];
       BC:=CC[5];
     end else
     begin
       C:=CC[1];
       BC:=CC[2];
     end;
    MoveChar(B, ' ', C, Size.X);
    if A < FileLines^.Count then
    begin
      if WM or (A <> Delta.Y) then S := GetLine(A) else S := WorkString;
      MoveStr(B, Copy(S, Pos.X + 1, Size.X), C);
      if EdOpt.HiLite then begin
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
        {$IFDEF COLORER}
        if UseExternalColorer then
          ColorerCollection^.Highlites(B, A, Pos.X+1, Pos.X+Size.X+1)
        else
        {$ENDIF}
        {/Cat}
          begin
            {$IFDEF USELONGSTRING}
            SetLength(S, Length(S)); {Cat: ...Kreks...Feks...Peks...Suxx...}
            {$ENDIF}
            HighLite.Highlites(Length(S), {$IFDEF USELONGSTRING} PChar(S) {$ELSE} @S[1] {$ENDIF}, HiLitePar); {PZ 2000.04.28}
            C1                  := C and $F0;                  { Background }
            SetLength(HP, 6);                                  { Attr. size }
            if (A = Delta.Y) and (EdOpt.HiliteLine)
             then HP[Ord(hhComment)]  := Chr(CC[6])            { Comments for current line}
             else HP[Ord(hhComment)]  := Chr(Comments);        { Comments   }
            HP[Ord(hhNumber)]   := Chr(C1 or (CC[10] and 15)); { Numbers    }
            HP[Ord(hhString)]   := Chr(C1 or (CC[ 9] and 15)); { Strings    }
            HP[Ord(hhSymbol)]   := Chr(C1 or (CC[ 8] and 15)); { Symbols    }
            HP[Ord(hhKeyword1)] := Chr(C1 or (CC[11] and 15)); { Keywords 1 }
            HP[Ord(hhKeyword2)] := Chr(C1 or (CC[12] and 15)); { Keywords 2 }
            DoHighlite(B, S, HP);
          end;
      end;
     if RulerVisible and (A = Delta.Y)
        then
          begin
             FreeStr := '123456789'; Ch := '1';
             while CStrLen(FreeStr) < Size.X do
               begin
                 FreeStr := FreeStr+'~'+Ch+'~'+Copy(FreeStr, 1, 9);
                 Inc(Ch); if Ch > '9' then Ch := '0';
               end;
             MoveCStr(B[Delta.X-Pos.X],FreeStr, Application^.GetColor($4240));
          end;
    end;
    if BV and (A >= Mark.A.Y) and (A <= Mark.B.Y) then DrawBlock;
    { regular expression search }
    if SearchActive and (A = Delta.Y) then
      begin
        X1 := Delta.X - Pos.X;
        {$IFDEF REGEXP}
        if ((SearchData.Options and efoRegExp) <> 0) and (SearchData.What = #0) then
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
      SearchOnDisplay := true;
    end;
    if EdOpt.HiliteColumn then WordRec(B[Delta.X - Pos.X]).Hi := CC[7];
{$IFDEF OS2}
    if PMWindowed and (A = Delta.Y) then  {JO: чтобы решить пробему курсора}
      begin                               {    на сером фоне в окне}
        if (WordRec(B[Delta.X - Pos.X]).Hi and $F0 = $80) then WordRec(B[Delta.X - Pos.X]).Hi := WordRec(B[Delta.X - Pos.X]).Hi and $0F;
        if (WordRec(B[Delta.X - Pos.X]).Hi and $FF = 0) then WordRec(B[Delta.X - Pos.X]).Hi := $07;
      end;
{$ENDIF}
    WriteLine(0, I, Size.X, 1, B);
  end;
 SetCursor(Delta.X - Pos.X, Delta.Y - Pos.Y);
 if InsertMode xor (InterfaceData.Options and ouiBlockInsertCursor <> 0)
    then NormalCursor else BlockCursor;
 ShowCursor;
 LastPos.X := Delta.X;
 LastPos.Y := Delta.Y;
 ChPosition := false;
 if (InfoL<>nil) then InfoL^.Draw;
 if (BMrk<>nil)  then BMrk^.Draw;
end;

function TFileEditor.GetLine(Index : LongInt) : LongString;
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
 if (FileLines <> nil) and (Index < FileLines^.Count) and (Index >= 0) then
   GetLine := KeyMapConvertStr(CnvLongString(FileLines^.At(Index)), True)
 else
   GetLine := '';
end;
{/Cat}

procedure TFileEditor.ModifyLine(Index : LongInt; S : LongString; DelSpaces: Boolean);
begin
 if (Index < 0) or (Index >= FileLines^.Count) then Exit;
 if DelSpaces then LongDelRight(S);
 KeyMapAtReplace(Index, NewLongStr(S)); {-$VIV}
end;

function TFileEditor.HandleCommand;
begin
end;

procedure TFileEditor.HandleEvent;
 label 1,2;
 var ChPos,WM,DelWord,WasMA,WasMB: Boolean;
     P: PLongString;
     PL: PCollection;
     S: LongString;
     LastY,LastX,i: LongInt;
     OldDelta,
     T: TPoint;
     L: longint;

 procedure BlockOff;
 begin
  UnMark := true; Marking := false; ChPosition := false;
  if EditorDefaults.EdOpt and ebfPbl <> 0 then Exit;
  Mark.B := Mark.A; BlockVisible := false;
 end;

 procedure InsertSpace;
  var P: TPoint;
 begin
  P.X := LastX; P.Y := LastY;
  if InsertMode then
    begin
     if LastX < Length(WorkString) then Insert(' ', WorkString, LastX + 1)
                   else WorkString := WorkString + LongStrg(' ', LastX - Length(WorkString) + 1);
     StoreUndoInfo(udInsChar, P, P);
    end
     else
      begin
       if LastX< Length(WorkString) then
        begin
         P.X := LastX;
         StoreUndoInfo(udReplaceChar, P, WorkString[LastX + 1]);
         WorkString[LastX + 1] := ' ';
        end;
      end;
  Inc(LastX);
 end;

 procedure WorkModify;
 begin
  if WorkModified then Exit;
  LastLine := Delta.Y;
  LastShape := GetLine(LastLine);
  WorkModified := true; Modified := true; WasDelete := false;
 end;

 procedure ChangeLine;
  label Ex;
 begin
   if not WorkModified then goto Ex;
   While (WorkString <> '') and (WorkString[Length(WorkString)] = ' ') do SetLength(WorkString, Length(WorkString)-1);
   if WorkModified then ModifyLine(LastLine, WorkString,true);
   Modified := Modified or WorkModified;
   WorkModified := false;
 Ex:
   WorkString := GetLine(Delta.Y);
 end;

 procedure MakeEnter;
  var I, J: LongInt;
      OldS,s1,s2: LongString;
      Bl: Boolean;
      WBY,WAY: Boolean;
      L: Longint;
 begin
   BlockOff;
   Bl := ValidBlock and not VertBlock;
   if not InsertMode then
   begin
    ChangeLine;
    if Delta.Y = FileLines^.Count - 1 then
      begin
        FileLines^.Insert(nil);
        SetLimits;
      end;
    ScrollTo(0, Delta.Y + 1);
   end else
       begin
        ChangeLine;
        Modified := true;
        WorkModified := false;
        OldS := WorkString;
        WAY := Bl and (Delta.Y = Mark.A.Y) and (Delta.X <= Mark.A.X);
        WBY := Bl and (Delta.Y = Mark.B.Y) and (Delta.X <= Mark.B.X);
        StoreUndoInfo(udInsLine, Delta, WorkString);
        While (WorkString <> '') and (WorkString[Length(WorkString)] = ' ') do SetLength(WorkString, Length(WorkString)-1);
        S1 := WorkString; S2 := Copy(S1, 1, LastX);
        While (S2 <> '') and (S2[Length(S2)] = ' ') do SetLength(S2, Length(S2)-1);
        WorkString := S2; Delete(S1, 1, LastX);
        ModifyLine(LastY,S2,true);
        WorkString := S1;
        if EdOpt.AutoIndent then
         begin
          I := LastY - 1;
          While (WorkString <> '') and (WorkString[1] = ' ') do
            begin
              Delete(WorkString, 1, 1); {DelFC(WorkString);}
              if WAY then Dec(Mark.A.X);
              if WBY then Dec(Mark.B.X);
            end;
          if DelSpaces(S2) = '' then S2 := OldS;
          {While (S2 = '') and (I >= 0) do
          if I >= 0 then
           begin S2 := GetLine(I); Dec(I) end;}
          LastX := 0;
          While (LastX < Length(S2)) and (S2[LastX + 1] = ' ')
           do begin
                 Inc(LastX);
                 WorkString := ' ' + WorkString;
                 if WAY then Inc(Mark.A.X);
                 if WBY then Inc(Mark.B.X);
              end;
         end else LastX := 0;
        KeyMapAtInsert(LastY + 1, NewLongStr(WorkString)); {-$VIV}
        SetLimits; Delta.X := LastX; Inc(Delta.Y);
       end;
  for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Inc(MarkPos[L].Y);
 end;

 procedure MakeBack;
 var
   I, J: LongInt;
   P: TPoint;
   L: Longint;
 begin
  WorkModify; BlockOff; Modified := true;
  ClearEvent(Event);
  if LastX = 0 then
   begin
    if (LastY = 0) or not InsertMode then Exit;
    EnableMarking := false;
    FileLines^.AtFree(LastY);
    S := GetLine(LastY - 1);
    P.X := Length(S); P.Y := Delta.Y - 1;
    StoreUndoInfo(udSubDelLine, P, WorkString);
    WorkString := S + WorkString; ModifyLine(LastY - 1, WorkString, true);
    WorkModified := false; WorkModify; WorkModified := false;
    Delta.X := Length(S);
    ScrollTo(Delta.X, Delta.Y - 1);
    SetLimits;
    ChangeLine;
    DrawView;
    EnableMarking := true;
    for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Dec(MarkPos[L].Y);
    Exit;
   end;
  EnableMarking := false;
  if Length(WorkString) < LastX then WorkString := LongAddSpace(WorkString, LastX + 1);
{Cat:warn проверить, нет ли тут ошибки}
  if EdOpt.BackIndent and ((WorkString[LastX + 1] <> ' ') or (DelSpaces(WorkString) = ''))
   and (Copy(WorkString, 1, LastX) = LongStrg(' ', LastX))
   and (LastY > 0) then
    begin
     for I := LastY-1 downto 0 do
      begin
       S := GetLine(I);
       J := 0;
       while (J < Length(S)) and (S[J+1] = ' ') do
         Inc(J);
       if J < LastX then
       if Copy(s, 1, LastX) > LongStrg(' ', LastX) then
         begin
          T.X := LastX; T.Y := LastY;
          StoreUndoInfo(udSubDel, T, WorkString);
          Delete(WorkString, 1, LastX); LastX := J;
          Insert(LongStrg(' ', J), WorkString, 1);
          ScrollTo(LastX, Delta.Y);
          ChangeLine;
          DrawView;
          EnableMarking := true;
          Exit;
         end;
      end;
    end;
  T.X := LastX; T.Y := LastY;
  if LastX <= Length(WorkString) then
    if InsertMode then
      begin
        StoreUndoInfo(udBackDel, T, WorkString[LastX]);
        Delete(WorkString, LastX, 1);
      end else
      begin
        T.X := LastX-1;
        StoreUndoInfo(udReplaceChar, T, WorkString[LastX]);
        WorkString[LastX] := ' ';
      end;
  ScrollTo(Delta.X - 1, Delta.Y);
  ChangeLine;
  EnableMarking := true;
 end;

 procedure BlockDelete(ChangePos: Boolean);
  var I, J: LongInt;
      S, S1, S2: LongString;
      L, K: Longint;
 begin
  Modified := true;
  if VertBlock then
   begin
    if ChangePos and InsertMode then
     begin
       if (Delta.Y >= Mark.A.Y) and (Delta.X >= Mark.A.X) and
          (Delta.Y <= Mark.B.Y) then
        begin
          Delta.X := Max(Mark.A.X, Delta.X-(Mark.B.X-Mark.A.X));
        end;
     end;
    for I := Mark.A.Y to Mark.B.Y do
    begin
     S := GetLine(I);
     Delete(S, Mark.A.X + 1, Mark.B.X - Mark.A.X);
     if not InsertMode then Insert(LongStrg(' ', Mark.B.X - Mark.A.X), S, Mark.A.X + 1);
     ModifyLine(I, S, true);
    end;
   end else
   begin
    if ChangePos then
     begin
       if (Delta.Y = Mark.A.Y)  then
         begin
           if (Delta.X > Mark.A.X) then
           if (Delta.Y = Mark.B.Y) and (Delta.X > Mark.B.X) then
             Dec(Delta.X, Mark.B.X-Mark.A.X) else
             Delta.X := Mark.A.X;
         end else
       if (Delta.Y = Mark.B.Y) then
        begin
          Dec(Delta.X, Mark.B.X);
        end else
        if Delta.Y < Mark.A.Y then else
        if Delta.Y > Mark.B.Y then Dec(Delta.Y, Mark.B.Y - Mark.A.Y)
         else Delta := Mark.A;
     end;
    ModifyLine(Mark.A.Y, Copy(GetLine(Mark.A.Y),1,Mark.A.X) +
                         Copy(GetLine(Mark.B.Y),Mark.B.X+1,MaxLongStringLength), true);
    for I := Mark.A.Y to Mark.B.Y - 1 do
        FileLines^.AtFree(Mark.A.Y + 1);
    K:=Mark.B.Y - Mark.A.Y - 1;
    If K>0 then
     for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Dec(MarkPos[L].Y, K);
   end;
 end;

 procedure MakeSmallBack;
  var T: TPoint;
 begin
  if LastX > 0 then
   begin
    T.X := LastX; T.Y := LastY;
    StoreUndoInfo(udBackDel, T, WorkString[LastX]);
    Delete(WorkString, LastX, 1);
   end;
  Dec(LastX);
 end;

 procedure DeleteBlock(ChangePos: Boolean);
  var L: PCollection;
  label L1;
 begin
  if not BlockVisible then Exit;
  EnableMarking := false; Marking := false;
  ChangeLine;
  L := GetSelection;
     if (L = nil) then
                     begin
                      if MemEnough then Exit;
                      if UndoInfo<>Nil then UndoInfo^.FreeAll;
                      L := GetSelection;
                      if (L = Nil) and ( not MemEnough ) then Goto L1;
                     end;

  if VertBlock and not InsertMode then StoreUndoInfo(udClearBlock, Mark.A, L)
    else StoreUndoInfo(udDelBlock, Mark.A, L);
L1:
  BlockDelete({not ChangePos} true);
  if ChangePos then
    begin
      Delta := Mark.A;
      Mark.A := Mark.B;
    end;
  LastX := Delta.X; LastY := Delta.Y;
  BlockOff;
  SetLimits;
  ScrollTo(LastX, LastY);
  DrawView;
  ChangeLine;
  EnableMarking := true;
 end;

 procedure BMarking;
 begin
  if not EnableMarking or (DrawMode>0) then Exit;
  if (ShiftState and 3 <> 0) and not MouseMark then
  begin
   if not Marking then begin Sel.A := LastPos; Sel.B := Delta end
                  else Sel.B := Delta;
   Marking := true;
   BlockVisible := true;
  end else begin Marking := MouseMark; if Marking then Sel.B := Delta; end;
  if not Marking and UnMark then BlockOff;
 end;

 procedure MakeDel;
  var P: TPoint;
      S: LongString;
      L: Longint;
 begin
  WorkModify;
  ChangeLine;
  BlockOff;
  Modified := true;
  if LastX < Length(WorkString) then
   begin WorkModify; P.X := LastX; P.Y := LastY;
         StoreUndoInfo(udDelChar, P, WorkString[LastX+1]);
         Delete(WorkString, LastX + 1, 1); Exit end;
  if LastY + 1 >= FileLines^.Count then Exit;
  WorkModify;
  P.X := LastX; P.Y := LastY;
  S := GetLine(LastY + 1);
  if LastY < FileLines^.Count then StoreUndoInfo(udSubDelLine, P, S);
  WorkString := WorkString + LongStrg(' ', LastX - Length(WorkString)) + S;
  ChangeLine;
  FileLines^.AtFree(LastY + 1);
  for L:=1 to 9 do if MarkPos[L].Y >= LastY then Dec(MarkPos[L].Y);
  SetLimits;
 end;

 procedure PasteBlock;
 begin
  if SystemData.Options and ossUseSysClip <> 0 then SyncClipOut{(true)};
  EnableMarking := false; Marking := false;
  ChangeLine;
  if EditorDefaults.EdOpt and (ebfPbl+ebfObl)=ebfObl
    then DeleteBlock(true);
  BlockOff;
  InsertBlock(Clipboard, true);
  ChangeLine;
  {$IFNDEF NONBP}
  memw[seg0040:$1C] := memw[seg0040:$1A];
  {$ENDIF}
  EnableMarking := true;
 end;

 procedure CopyBlock;
  var R: TRect;
 begin
  ChangeLine;
  if (Clipboard <> nil) then Dispose(Clipboard,Done);
  Clipboard := GetSelection;
  if SystemData.Options and ossUseSysClip <> 0 then SyncClipIn;

{AK155: не понял, зачем вообще нужен ClipBoardStream. Используется
он в команде просмотра Clipbioard, но зачем его из коллекции нужно
гонять в Stream, а потом обратно - непонятно. А тормозит он чудовищно.
Поэтому пока что ограничиваю размер, в результате чего просмотр Clipboard
при отключенном Use system clipboard для очень больших фрагментов
работать временно не будет }
  if (ClipBoardStream<>nil)
    {AK155} and (Clipboard^.Count < 1000) {/AK155}
  then
     ClipboardStream^.Seek(Max(ClipboardStream^.GetPos-{$IFDEF USELONGSTRING} 4 {$ELSE} 1 {$ENDIF}, 0));
  CopyLines2Stream(Clipboard, ClipboardStream);
 end;

 procedure CenterScreen;
 begin
  Pos.X := Delta.X - Size.X div 2;
  Pos.Y := Delta.Y - Size.Y div 2;
  DrawView;
 end;

 procedure DeleteLine;
 var L: longint;
 begin
  Modified := true;
  T.Y := LastY; T.X := LastX;
  if Delta.Y + 1 = FileLines^.Count then
   begin
    StoreUndoInfo(udSubDel, T, WorkString);
    ModifyLine(Delta.Y, '', true); SetLength(WorkString, 0);
   end else
   begin
    StoreUndoInfo(udDelLine, T, WorkString);
    WorkModified := false; FileLines^.AtFree(Delta.Y); ChangeLine
   end;
  for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Dec(MarkPos[L].Y);
  SetLimits; ScrollTo(0, Delta.Y); DrawView; ChangeLine;
 end;

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
     Exit;
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
           Break;
       end;

   if LastY >= 0 then
     while (LastX > 0) and not (WorkString[LastX] in BreakChars) do
       Dec(LastX)
   else
     LastY := 0;
 end;

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
   if (LastY = FileLines^.Count-1) and (LastX >= Length(GetLine(LastY))) then
     Exit;

   if LastX >= Length(WorkString) then JumpDown;

   while (LastX < Length(WorkString)) and not (WorkString[LastX+1] in BreakChars) do
     Inc(LastX);

   while LastY < FileLines^.Count do
     if WorkString = '' then
       JumpDown
     else
       begin
         while (LastX < Length(WorkString)) and (WorkString[LastX+1] in BreakChars) do
           Inc(LastX);
           Break;
       end;

   if LastY >= FileLines^.Count then
     begin
       LastY := FileLines^.Count-1;
       LastX := Length(GetLine(LastY));
     end;
 end;
{/Cat}

 procedure InputChar; forward;

 procedure MakeTab;
 begin
   TabStep := StoI(EditorDefaults.TabSize);
   if TabStep = 0 then TabStep := 8;
   WorkModify;
   repeat InsertSpace; until (LastX) mod TabStep = 0;
   ScrollTo(LastX, Delta.Y); DrawView;
 end;

 procedure DoTab;
 begin
  Modified := true;
  {-$VOL begin}
  if not TabReplace then begin
    Event.CharCode:=#09;
    InputChar;
  end {-$VOL end} else
  if EdOpt.SmartTab then begin
    if LastY > 0 then s := GetLine(LastY - 1)
                 else s := '';
    if (LastY = 0) then MakeTab
     else
     if (LastY > 0) and (LastX < Length(S)) then
     begin
      WorkModify;
      repeat InsertSpace until (s[LastX + 1] = ' ') or (LastX >= Length(S));
      {WM := (s[LastX + 2] = ' ') and (LastX < Length(s));}
      While (s[LastX + 2] = ' ') and (LastX < Length(s)) do InsertSpace;
      {if WM then }InsertSpace;
      ScrollTo(LastX, Delta.Y); DrawView;
     end else MakeTab;
   end else MakeTab;
 end;

        {-DataCompBoy-}
 procedure BlockRead;
  var P: PCollection;
      S: String;
 begin
  ChangeLine;
  S := GetFileNameDialog(x_x, GetString(dlPasteFromTitle),
                             GetString(dlPasteFromLabel),
                              fdOKButton + fdHelpButton, hsEditPasteFrom);
  if S = '' then Exit;
  S := lFExpand(S);
  P := MIReadBlock(@Self, S, false);
  if not IsValid then begin isValid := true; Exit; end;
  VertBlock := false;
  if P <> nil then begin InsertBlock(P, true); Dispose(P,Done) end;
 end;
        {-DataCompBoy-}

        {-DataCompBoy-}
 procedure BlockWrite;
  var P: PCollection;
      PS: PLineCollection {PStdCollector}; {-SBlocks}
      S, SST: LongString;
      R: PStream;
      I,J,K: LongInt;
      CRLF: String[2];
      VB: Boolean;
      PI: PView;
      A: Word;

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

var qwe: byte;
 begin
  if not (BlockVisible and ValidBlock) then Exit;
  ChangeLine;
  S := GetFileNameDialog(x_x, GetString(dlCopyTo),
                                GetString(dlFileName),
                               fdOKButton + fdHelpButton, hsEditPasteFrom);
  if S = '' then Exit;
  S := lFExpand(S);
  A := GetFileAttr(S+#0);
  R := CheckForOver(S);
  if R = nil then Exit;

  VB := VertBlock or (Mark.A.Y = Mark.B.Y);

  case EdOpt.ForcedCrLf of
   cfCrLf: crlf := #13#10;
   cfCr: crlf := #13;
   cfLf: crlf := #10;
   else
    begin
     EdOpt.ForcedCrLf:=cfNone;
     for qwe:=0 to EditorDefaults.NewLine do
      EdOpt.ForcedCrLf:=succ(EdOpt.ForcedCrLf);
     if EditorDefaults.NewLine = 1 then CrLf := #13 else
      if EditorDefaults.NewLine = 2 then CrLf := #10 else
       crlf := #13#10;
    end;
  end;

  PI := WriteMsg(^M^M^C+GetString(dlWritingFile));

  for I := Mark.A.Y to Mark.B.Y do
    begin
      if VB then SST := Copy(GetLine(I), Mark.A.X+1, Mark.B.X-Mark.A.X)
            else if I = Mark.A.Y then SST := Copy(GetLine(I), Mark.A.X+1, MaxLongStringLength)
                   else if I = Mark.B.Y then SST := Copy(GetLine(I), 1, Mark.B.X)
                     else SST := GetLine(I);
      if OptimalFill then CompressString(SST);
      R^.Write(SST[1], Length(SST));
      if (I <> Mark.B.Y)
          {$IFNDEF USELONGSTRING}
            and ((Length(SST) < MaxLongStringLength) or not RecombineLongLines) {JO}{!!!}
          {$ENDIF}
                then R^.Write(CrLf[1], Length(CrLf));
    end;

  if PI<>nil then PI^.Free;

  Dispose(R,Done);
  if (A <> 0) and (A <> $FFFF) then SetFileAttr(S+#0, A);

  S := GetPath(lFExpand(S));
  GlobalMessage(evCommand, cmRereadDir, @S);
 end;
        {-DataCompBoy-}

 procedure CE; begin if Event.What<>evNothing then ClearEvent(Event) end;
 procedure CED; begin ClearEvent(Event); DrawView end;

 var CFind: Boolean;
     EvStr: Array[1..2] of Char;
     PP: TPoint;

 procedure SplitString;
  var S: LongString;
      I,OldX: LongInt;
      P: PLineCollection;
      TP: TPoint;
 begin
  ChangeLine;
  S := '';
  While (WorkString <> '') and (WorkString[Length(WorkString)] = ' ') do
   begin SetLength(WorkString, Length(WorkString)-1); if Length(WorkString) < LastX + 1 then LastX := Length(WorkString) - 1; end;
  OldX := 0;
  if Length(WorkString) - 1 = LastX then OldX := 1
     else if Length(WorkString) - 1 < LastX then OldX := 0
       else OldX := 1;
  if Length(WorkString) > EdOpt.RightSide then
   begin
    New(P, Init(1,1,True)); P^.Insert(NewLongStr(WorkString));
    TP.Y := Delta.Y; TP.X := 0;
    StoreUndoInfo(udDelBlock, TP, P);
    while (Length(WorkString) > 1) and (Length(WorkString) > EdOpt.RightSide) do
     begin S := WorkString[Length(WorkString)] + S; SetLength(WorkString, Length(WorkString)-1); end;
    while (Length(WorkString) > 1) and not (WorkString[Length(WorkString)] in [' ',',',':','.','?','!','+',';']) do
     begin S := WorkString[Length(WorkString)] + S; SetLength(WorkString, Length(WorkString)-1); end;
    if DelSpaces(WorkString) = '' then begin WorkString := WorkString + S; S := ''; end else
    OldX := Delta.X - Length(WorkString) + OldX;
    if Delta.X > Length(WorkString) then Delta.X := MaxLongStringLength;
    if System.Pos(DelSpaces(WorkString), WorkString) = 0 then
    begin
     While (WorkString <> '') and (WorkString[Length(WorkString)] = ' ') do SetLength(WorkString, Length(WorkString)-1);
     I := 1;
     if EdOpt.AutoJustify then
     while Length(WorkString) < EdOpt.RightSide do
      begin
       while (WorkString[I] = ' ') do Inc(I);
       while (WorkString[I] <> ' ') and (I < Length(WorkString)) do Inc(I);
       if (I < Length(WorkString)) then
        begin
         Insert(' ', WorkString, I);
         if I <= Delta.X then Inc(Delta.X);
        end;
        Inc(I); if I > Length(WorkString) then I := 1;
      end;
     end;
    LongDelLeft(S);
    S := LongStrg(' ', EdOpt.LeftSide)+S;
   end;
   ModifyLine(LastY, WorkString, true);
   KeyMapAtInsert(LastY+1, NewLongStr(S)); {-$VIV}
   S := #2#0;
   StoreUndoInfo(udFormatBlock, TP, S);
   if Delta.X >= Length(WorkString) then
    begin
     VScroll^.SetParams(LastY+1, 0, FileLines^.Count-1, Size.Y, 1);
     if OldX>0 then HScroll^.SetValue(EdOpt.LeftSide);
     HScroll^.SetValue(EdOpt.LeftSide + OldX);
    end else
     begin
      VScroll^.SetRange(0, FileLines^.Count-1);
      HScroll^.SetValue(Delta.X);
     end;
   DrawView;
 end;

 procedure InputChar;
  var I1, I2, I3, i: Byte;
      Ch: Char;
      S1: String[2];
 begin
   Ch := Event.CharCode;
    S1 := Ch;
   if EdOpt.AutoBrackets and ((LastX >= Length(WorkString)) or (WorkString[LastX+1] = ' ')) then
    begin
        for i:=1 to Length(AutoBracketPairs) shr 1
        do if Ch=AutoBracketPairs[i*2-1] then begin
            S1:=Copy(AutoBracketPairs,i*2-1,2);
            break
        end;
    end;

   EnableMarking := false;
   if EditorDefaults.EdOpt and (ebfPbl+ebfObl)=ebfObl
     then DeleteBlock(true);
   LastY := Delta.Y;
   LastX := Delta.X;
   BlockOff;
   EnableMarking := false;
   T := Delta; Inc(T.X);
   if LastX < MaxLongStringLength then
    begin
     WorkModify;
     if InsertMode then
      begin
       if LastX <= Length(WorkString) then Insert(S1, WorkString, LastX + 1)
                      else WorkString := WorkString + LongStrg(' ', LastX - Length(WorkString)) + S1;
       StoreUndoInfo(udInsChar, Delta, Delta);
       if Length(S1) = 2 then
         StoreUndoInfo(udInsChar, T, T);
      end else
      begin
       if LastX >= Length(WorkString) then WorkString := WorkString + LongStrg(' ', LastX - Length(WorkString) + 1);
       StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX+1]);
       WorkString[LastX+1] := Ch
      end;
     if (not EdOpt.AutoWrap or (LastX < EdOpt.RightSide)) and (LastX < MaxLongStringLength) then HScroll^.SetValue(LastX + 1)
      else if LastX < MaxLongStringLength then SplitString else
             begin
              ChangeLine;
              FileLines^.AtInsert(LastY+1, nil); SetLimits;
              ScrollTo(0, LastY+1); DrawView;
             end;
    end;
  CE;
  EnableMarking := true;
 end;

 function AskSave: Word;
  var S: Pointer;
      P: String;
 begin
  P := Cut(EditName, 30);
  S := @EditName;
  AskSave := MessageBox(GetString(dlQueryModified), @S, mfWarning+mfYesNoCancel);
 end;

 procedure PasteWinBlock;
 begin
  EnableMarking := false; Marking := false;
  ChangeLine;
  if EditorDefaults.EdOpt and (ebfPbl+ebfObl)=ebfObl
    then DeleteBlock(true);
  BlockOff;

  if GetWinClip(PLineCollection(Clipboard){, On}) then
    begin
      InsertBlock(Clipboard, true);

      ChangeLine;
      {$IFNDEF NONBP}
      memw[seg0040:$1C] := memw[seg0040:$1A];
      {$ENDIF}
      EnableMarking := true;
    end;
 end;

 procedure DrawLine(Dir: Byte);
   const
      Line00 = '│─└││┌├─┘─┴┐┤┬┼';
      Line11 = '║═╚║║╔╠═╝═╩╗╣╦╬';
      Line01 = '│═╘││╒╞═╛═╧╕╡╤╪';
      Line10 = '║─╙║║╓╟─╜─╨╖╢╥╫';

      UpContact1 =   '│├┼┤┌┬┐╞╪╡╒╤╕';
      UpContact2 =   '║╠╬╣╔╦╗╟╫╢╓╥╖';
      DownContact1 = '│├┼┤└┴┘╞╪╡╘╧╛';
      DownContact2 = '║╠╬╣╚╩╝╟╫╢╙╨╜';
      LeftContact1 = '─├┼┌└┬┴╟╫╓╙╥╨';
      LeftContact2 = '═╠╬╔╚╦╩╞╪╒╘╤╧';
      RightContact1 = '─┤┼┐┘┬┴╢╫╖╜╥╨';
      RightContact2 = '═╣╬╗╝╦╩╡╪╕╛╤╧';

   var  _Up, _In, _Down: LongString;
        VL: String[32];
        A,B: Word;
        I,J,K: LongInt;
        ResChar, C: Char;
        DMode: Byte;

   function GetH0: Byte; begin GetH0 := 2*Byte((LastX+2<=Length(_In)) and (PosChar(_In[LastX+2], RightContact1)>0))+
                                        8*Byte((LastX>0) and (PosChar(_In[LastX], LeftContact1)>0)) end;

   function GetH1: Byte; begin GetH1 := 2*Byte((LastX+2<=Length(_In)) and (PosChar(_In[LastX+2], RightContact2)>0))+
                                        8*Byte((LastX>0) and (PosChar(_In[LastX], LeftContact2)>0)) end;

   function GetV0: Byte; begin GetV0 :=   Byte((LastX+1<=Length(_Up)) and (PosChar(_Up[LastX+1], UpContact1)>0))+
                                        4*Byte((LastX+1<=Length(_Down)) and (PosChar(_Down[LastX+1], DownContact1)>0)) end;

   function GetV1: Byte; begin GetV1 :=   Byte((LastX+1<=Length(_Up)) and (PosChar(_Up[LastX+1], UpContact2)>0))+
                                        4*Byte((LastX+1<=Length(_Down)) and (PosChar(_Down[LastX+1], DownContact2)>0)) end;

   function Modify(C: Char; Mask: Byte): Boolean;
     var I: Integer;
   begin
     Modify := false;
     VL := Line00; I := PosChar(C, VL);
     if I = 0 then begin VL := Line01; I := PosChar(C, VL); end;
     if I = 0 then begin VL := Line10; I := PosChar(C, VL); end;
     if I = 0 then begin VL := Line11; I := PosChar(C, VL); end;
     if not (I in [7,11,13,14,15]) then Exit;
     Mask := I and not Mask; if Mask = I then Exit;
     if Mask > 0 then ResChar := VL[Mask] else ResChar := ' ';
     Modify := true;
   end;

 var WasShift: Boolean; {-$VIV 18.05.99--}
 begin
   {-$VIV 18.05.99--}
   WasShift := (ShiftState and kbRightShift > 0) and DrawRShift;
   if WasShift then
   begin
     if DrawMode = 1 then DrawMode := 2 else DrawMode := 1;
   end;
   {-$VIV--}
   ChangeLine;
   if ShiftState and 3 <> 0 then DMode := 1
     else if ShiftState and 4 <> 0 then DMode := 2 else DMode := 0;
   _In := LongAddSpace(WorkString, LastX+2);
   _Up := LongAddSpace(GetLine(LastY-1), LastX+2);
   _Down := LongAddSpace(GetLine(LastY+1), LastX+2);
   if DrawMode > 1 then VL := Line00 else VL := Line11;
   if Odd(Dir) then
     begin
       if DrawMode > 1 then
         begin
           A := GetV1; B := GetH1; VL := Line11;
           if A = 0 then begin A := GetV0; if A <> 0 then VL := Line01 end;
         end else
         begin
           A := GetV0; B := GetH0; VL := Line00;
           if A = 0 then begin A := GetV1; if A <> 0 then VL := Line10 end;
         end;
     end else
     begin
       if DrawMode > 1 then
         begin
           A := GetV1; B := GetH1; VL := Line11;
           if B = 0 then begin B := GetH0; if B <> 0 then VL := Line10 end;
         end else
         begin
           A := GetV0; B := GetH0; VL := Line00;
           if B = 0 then begin B := GetH1; if B <> 0 then VL := Line01 end;
         end;
     end;
   if (LastDir <> Dir) then
   case DMode of
    1: begin
         if LastDir >= 0 then A := A or (1 shl LastDir);
         A := A or B or (1 shl Dir);
         C := VL[A];
         WorkModify;
         if LastX >= Length(WorkString) then WorkString := LongAddSpace(WorkString, LastX + 1);
         StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX+1]);
         WorkString[LastX+1] := C;
       end;
    2: begin
         if LastDir < 0 then LastDir := (Dir + 2) mod 4;
         A := A or B or (1 shl LastDir) or (1 shl Dir);
         WorkModify;
         if LastX >= Length(WorkString) then WorkString := LongAddSpace(WorkString, LastX + 1);
         if Modify(_Up[LastX+1], 4) then
           begin
             Dec(Delta.Y);
             StoreUndoInfo(udReplaceChar, Delta, _Up[LastX+1]);
             _Up[LastX+1] := ResChar; ModifyLine(Delta.Y, _Up, true);
             Inc(Delta.Y);
           end;
         if Modify(_Down[LastX+1], 1) then
           begin
             Inc(Delta.Y);
             StoreUndoInfo(udReplaceChar, Delta, _Down[LastX+1]);
             _Down[LastX+1] := ResChar; ModifyLine(Delta.Y, _Down, true);
             Dec(Delta.Y);
           end;
         if (LastX > 0) and Modify(WorkString[LastX], 2) then
           begin
             Dec(Delta.X);
             StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX]);
             WorkString[LastX] := ResChar;
             Inc(Delta.X);
           end;
         StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX+1]);
         WorkString[LastX+1] := ' ';
         if (Length(WorkString) > LastX+1) and (Modify(WorkString[LastX+2], 8)) then
           begin
             Inc(Delta.X);
             StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX+2]);
             WorkString[LastX+2] := ResChar;
             Dec(Delta.X);
           end;
       end;
   end;
   EnableMarking := false;
   case Dir of
     0: ScrollTo(LastX, LastY-1);
     2: begin
          if LastY = FileLines^.Count - 1 then
            begin
              FileLines^.Insert(nil);
              SetLimits;
            end;
          ScrollTo(LastX, LastY+1);
        end;
     1: ScrollTo(LastX+1, LastY);
     3: ScrollTo(LastX-1, LastY);
   end;
   CED;
   if DMode > 0 then LastDir := (Dir + 2) mod 4
                else LastDir := -1;
   EnableMarking := true;

   if WasShift then {-$VIV 18.05.99--}
   begin
     if DrawMode = 1 then DrawMode := 2 else DrawMode := 1;
   end;             {-$VIV--}

 end;

        {-DataCompBoy-}
 procedure OpenFileAtCursor;
 var Res, S: String;
     I, Min1, Max1, Min2, Max2, Min3, Max3, Min4, Max4, P: Integer;
     Q: Pointer;
     Info: PWhileView;
     R: TRect;
 label Ex;

   procedure Chk;
   begin
     DelLeft(S); DelRight(S);
     if S='' then exit;
     if Res='' then Res:=FindFileWithSPF(S, Info);                            if Abort then exit;
     if Res='' then Res:=FindFileWithSPF(GetName(S), Info);                   if Abort then exit;
     if Res='' then Res:=FindFileWithSPF(GetSName(S), Info);                  if Abort then exit;
     if Res='' then Res:=FindFileWithSPF(GetSName(S)+GetExt(EditName), Info); if Abort then exit;
   end;

 begin
   if WorkString <> '' then {Cat: работаем только на непустой строчке}
     begin
       Res:='';
       S:='';
       R.Assign(0, 0, 20, 7);
       New(Info, Init(R));
       Info^.Write(1, Copy(GetString(dlPleaseStandBy), 4, MaxStringLength));
       Application^.Insert(Info);
       Abort:=False;

       P:=Delta.X+1;

       Min1:=1;
       For i:=P downto 1 do
        if WorkString[i] in IllegalCharSet+['*','?']
         then begin Min1:=i+1; break end;
       DispatchEvents(Info, Abort);
       if Abort then goto ex;

       Min2:=1;
       For i:=P downto 1 do
        if WorkString[i] in IllegalCharSet-['\']
         then begin Min2:=i+1; break end;
       DispatchEvents(Info, Abort);
       if Abort then goto ex;

       Min3:=1;
       For i:=P downto 1 do
        if WorkString[i] in IllegalCharSetDos+['*','?']
         then begin Min3:=i+1; break end;
       DispatchEvents(Info, Abort);
       if Abort then goto ex;

       Min4:=1;
       For i:=P downto 1 do
        if WorkString[i] in IllegalCharSetDos-['\']
         then begin Min4:=i+1; break end;
       DispatchEvents(Info, Abort);
       if Abort then goto ex;

       Max1:=length(WorkString)+1;
       For i:=P to length(WorkString) do
        if WorkString[i] in IllegalCharSet+['*','?']
         then begin Max1:=i; break end;
       DispatchEvents(Info, Abort);
       if Abort then goto ex;

       Max2:=length(WorkString)+1;
       For i:=P to length(WorkString) do
        if WorkString[i] in IllegalCharSet-['\']
         then begin Max2:=i; break end;
       DispatchEvents(Info, Abort);
       if Abort then goto ex;

       Max3:=length(WorkString)+1;
       For i:=P to length(WorkString) do
        if WorkString[i] in IllegalCharSetDos+['*','?']
         then begin Max3:=i; break end;
       DispatchEvents(Info, Abort);
       if Abort then goto ex;

       Max4:=length(WorkString)+1;
       For i:=P to length(WorkString) do
        if WorkString[i] in IllegalCharSetDos-['\']
         then begin Max4:=i; break end;
       DispatchEvents(Info, Abort);
       if Abort then goto ex;

       Res:='';
       S:=Copy(WorkString, Min1, Max1-Min1); Chk; if Abort then goto ex;
       S:=Copy(WorkString, Min1, Max2-Min1); Chk; if Abort then goto ex;
       S:=Copy(WorkString, Min2, Max1-Min2); Chk; if Abort then goto ex;
       S:=Copy(WorkString, Min2, Max2-Min2); Chk; if Abort then goto ex;
       S:=Copy(WorkString, Min3, Max3-Min3); Chk; if Abort then goto ex;
       S:=Copy(WorkString, Min3, Max4-Min3); Chk; if Abort then goto ex;
       S:=Copy(WorkString, Min4, Max3-Min4); Chk; if Abort then goto ex;
       S:=Copy(WorkString, Min4, Max4-Min4); Chk; if Abort then goto ex;

       S:=Copy(WorkString, Min4, Max4-Min4); DelLeft(S); DelRight(S);
       If (PosChar('.',S)=0) Then S:=S+GetExt(EditName);

       if Res = '' then
         begin
           Info^.Hide;
           Res:=Cut(S, 20);
           Q:=@Res;
           case ExecResource(dlgSrchFailed, Q) of
             cmYes:
               PDNApplication(Application)^.EditFile(true, SourceDir + 'DN.SPF');
             cmNo:
               PDNApplication(Application)^.EditFile(true, S);
           end
         end
       else
         PDNApplication(Application)^.EditFile(true, Res);

       ex:
       Info^.Free;
       Abort:=false;
     end;
 end;
        {-DataCompBoy-}

 procedure ChangeCharCaseDirect; {-$VIV 21.05.99--}
 var Ch: Char;
     S, S2: LongString;
 begin
   S := WorkString;
   if (Delta.X >= 0) and (Delta.X < Length(S)) then
   begin
     WorkModify;
     StoreUndoInfo(udStrModified, Delta, WorkString);
     Ch := S[Delta.X+1];
     if Ch = UpCase(Ch)
      then S2 := LowLongStrg(''+Ch)
      else S2 := UpLongStrg(''+Ch);
     S[Delta.X+1] := S2[1];
     WorkString := S;
     ChangeLine;
     CED;
     ScrollTo(Delta.X+1, Delta.Y);
   end else begin
     if Delta.Y < FileLines^.Count - 1 then
       ScrollTo(0, Delta.Y + 1);
   end;
 end;

 procedure WordEnd;
 var S: LongString;
     NewX: Integer;
 begin
   S := WorkString;
   if (Delta.X < 0) or (Delta.X >= Length(S)) then Exit;
   NewX := Delta.X;
   if not (S[NewX+1] in BreakChars) then
     repeat
       Inc(NewX);
       if (NewX >= Length(S)) or (S[NewX+1] in BreakChars) then Break;
     until False
   else
     repeat
       Dec(NewX);
       if (NewX < 0) or not (S[NewX+1] in BreakChars) then
        begin Inc(NewX); Break; end;
     until False;
   ScrollTo(NewX, Delta.Y);
 end;

procedure ASCIITable;
 var
  P: PWindow;
  W: Word;
  E: TEvent;
  R: TRect;
  CR:PView;
   function GetCH: boolean;
   begin
{    Desktop^.Insert(P);
    Desktop^.UnLock;
    W:=P^.Execute;
    Desktop^.Lock;
    Desktop^.Delete(P);}
    W:=Desktop^.ExecView(P);
    P^.GetData(charASCII);
    GetCh:=true;
    if W in [cmOk, cmYes] then
     begin
      Event.What := evKeyDown;
      Event.KeyCode := Byte(charASCII);
      PutEvent(E);
      ClearEvent(E);
      InputChar;
      Desktop^.UnLock;
     end;
    GetCh:= W=cmYes;
  end;
 begin
  P:=New(PASCIIChart, Init(R));
  P^.MoveTo(boundsASCII.X, boundsASCII.Y);
  P^.SetData(charASCII);
  CR:=Desktop^.Current;
{  Desktop^.Lock;}
  while GetCH do ;
{  Desktop^.UnLock;}
  boundsASCII:=P^.Origin;
  Dispose(P,Done);
  if CR<>nil then CR^.Select;
 end;

procedure SelectAll;
  begin {AK155 выделить весь текст}
  with Sel, FileLines^ do
    begin
    A.X := 0; A.Y := 0; B.Y := Count-1;
    B.X := Length(CnvLongString(At(B.Y)));
    end;
  Mark := Sel; Marking := false; BlockVisible := true;
  VertBlock := false;
  end; {/AK155}

 var PC: PCollection;
     R: TRect;

begin
 if (Event.What = evCommand) and (Event.Command=cmHelp2) then begin
{Cat:warn почему закомментировали?}
{ if UpStrg(EditName)=UpStrg(SourceDir+'dn.ini')
   then HelpCtx:=hcDNIni
   else}HelpCtx:=hcEditor;
  Event.Command:=cmHelp;
  Application^.HandleEvent(Event);
  HelpCtx:=hcEditor;
  ClearEvent(Event);
  exit;
 end;
 inherited HandleEvent(Event);
 {Cat}
 {$IFDEF PLUGIN}
 if ProcessEditorEventHook(Event, @Self) then
   Exit;
 {$ENDIF}
 {/Cat}
 DelWord := false;
1:
 LastY := Delta.Y;
 LastX := Delta.X;
 i := FileLines^.Count;
 ChPos:=false;
 case Event.What of
  evCommand: case Event.Command of
              cmMainMenu: begin
                            Message(DNApp.MenuBar, evCommand, cmMenu, nil);
                            CE;
                          end;
              cmDuplicateLine: if F6_DuplicatesLine then
                                 begin
                                   ChangeLine;
                                   WorkModify;
                                   KeyMapAtInsert(LastY+1, NewLongStr(GetLine(LastY))); {-$VIV}
                                   StoreUndoInfo(udDupeLine, Delta, S);
                                   SetLimits;
                                   for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Inc(MarkPos[L].Y);
                                   CED;
                                 end
                                else
                                 begin
                                   OptimalFill := not OptimalFill;
                                   DrawView; CE
                                 end;
              cmClose: begin
                         if (SmartPad or ClipBrd) then
                           begin
                             ChangeLine;
                             if Modified then MISaveFile(@Self);
                             if not Owner^.GetState(sfModal) then Exit;
                             CE; Owner^.Redraw;
                             PEditWindow(Owner)^.ModalEnd := true;
                             ClearEvent(Event);
                           end;
                       end;
              cmUpString, cmLowString, cmCapString,
              cmUpWord, cmLowWord, cmCapWord: begin
                 if DelSpaces (WorkString) <> '' then
                  begin
                     WorkModify;
                     While (WorkString <> '') and (WorkString[Length(WorkString)] = ' ') do SetLength(WorkString, Length(WorkString)-1);
                     StoreUndoInfo(udStrModified, Delta, WorkString);
                     if (ShiftState and 3 <> 0)
                       or (Event.Command = cmUpString)
                       or (Event.Command = cmLowString)
                       or (Event.Command = cmCapString) then
                     case Event.Command of
                        cmUpWord,  cmUpString:  UpLongStr(WorkString);
                        cmLowWord, cmLowString: LowLongStr(WorkString);
                        cmCapWord, cmCapString: CapLongStr(WorkString);
                     end else
                     begin
                       While (LastX > 0) and not (WorkString[LastX] in BreakChars) do Dec(LastX);
                       S := '';
                       While (Length(WorkString) > LastX) and not (WorkString[LastX+1] in BreakChars) do
                          begin S := S + WorkString[LastX+1]; Delete(WorkString, LastX+1, 1); end;
                       if S <> '' then
                       case Event.Command of
                         cmUpWord:  UpLongStr(S);
                         cmLowWord: LowLongStr(S);
                         cmCapWord: CapLongStr(S);
                       end;
                       System.Insert(S, WorkString, LastX+1);
                     end;
                     ChangeLine; {-$VIV}
                  end;
                  CED
                end;
              cmMarkWord: begin
                            ChangeLine;
                            if (LastX < Length(WorkString)) and not (WorkString[LastX+1] in BreakChars) and
                              ((LastX=0) or (WorkString[LastX] in BreakChars)) then else
                             Message(@Self, evCommand, cmWordLeft, nil);
                            if (LastY <> Delta.Y) or (Delta.X < Length(WorkString)) and (WorkString[Delta.X+1] in BreakChars)
                              or (Delta.X >= Length(WorkString)) then
                              begin
                                Message(@Self, evCommand, cmWordRight, nil);
                              end;
                            Mark.A := Delta; Mark.B := Delta;
                            ChangeLine;
                            While (Mark.B.X < Length(WorkString)) and not (WorkString[Mark.B.X+1] in BreakChars) do Inc(Mark.B.X);
                            R := Mark;
                            ScrollTo(LastX, LastY);
                            BlockVisible := true;
                            Mark := R;
                            CED;
                          end;
              cmMarkLine: begin
                            Mark.A.X := 0; Mark.A.Y := Delta.Y;
                            if Delta.Y >= FileLines^.Count-1 then
                              begin Mark.B.Y := Delta.Y; Mark.B.X := Length(WorkString) end
                              else begin Mark.B.Y := Delta.Y+1; Mark.B.X := 0 end;
                            BlockVisible := true; CED
                           end;
              cmSelectAll: begin SelectAll; CED end;
              cmMoveBlockStart: begin ScrollTo(Mark.A.X, Mark.A.Y); CE end;
              cmMoveBlockEnd: begin ScrollTo(Mark.B.X, Mark.B.Y); CE end;
              cmSwitchHiLine: begin EdOpt.HiliteLine := not EdOpt.HiliteLine; CED end;
              cmSwitchHiColumn: begin EdOpt.HiliteColumn := not EdOpt.HiliteColumn; CED end;
              cmSwitchWrap: begin EdOpt.AutoJustify := not EdOpt.AutoJustify; CE end;
              cmSwitchFill: begin OptimalFill := not OptimalFill; DrawView; CE end;
              cmSwitchTabReplace: begin TabReplace := not TabReplace; CE end;
              cmSwitchBack: begin EdOpt.BackIndent := not EdOpt.BackIndent; CE end;
              cmIndentOn,cmIndentOff: begin EdOpt.AutoIndent := Event.Command = cmIndentOn; CE end;
              cmSwitchIndent: begin EdOpt.AutoIndent := not EdOpt.AutoIndent; CE end;
              cmSwitchSmartTab: begin EdOpt.SmartTab := not EdOpt.SmartTab; CE end;
              cmSwitchHighLight: begin
                                  EdOpt.HiLite:=not EdOpt.HiLite;
                                  {$IFDEF COLORER} {Cat}
                                  SwitchExternalColorer(EdOpt.HiLite);
                                  {$ENDIF} {/Cat}
                                  DrawView;
                                 end;
              cmEditCrLfMode:    begin
                                  EdOpt.ForcedCrLf:=cfCRLF;
                                  DrawView;
                                 end;
              cmEditLfMode:      begin
                                  EdOpt.ForcedCrLf:=cfLF;
                                  DrawView;
                                 end;
              cmEditCrMode:      begin
                                  EdOpt.ForcedCrLf:=cfCR;
                                  DrawView;
                                 end;
              cmSwitchBrackets: begin EdOpt.AutoBrackets := not EdOpt.AutoBrackets; CE end;
              cmSwitchSave: begin
                              EdOpt.AutoWrap := not EdOpt.AutoWrap; CE
                            end;
              cmGetName: PString(Event.InfoPtr)^ := PWindow(Owner)^.Title^;
              cmCtrlHome: begin ScrollTo(Delta.X, Pos.Y); CED end;
              cmCtrlEnd: begin ScrollTo(Delta.X, Pos.Y+Size.Y-1); CED end;
              cmDeltoEOLN: begin WorkModify; StoreUndoInfo(udSubDel, Delta, WorkString);
                                 SetLength(WorkString, LastX); CED; end;
              cmCopyBlock: begin
                             CE; if not ValidBlock then Exit;
                             ChangeLine;
                             PC := GetSelection;
                             if PC = nil then
                               begin
                                  Message(@Self, evCommand, cmCopy, nil);
                                  Message(@Self, evCommand, cmPaste, nil);
                               end else
                               begin
                                 ChangeLine;
                                 InsertBlock(PC, true);
                                 Dispose(PC,Done);
                               end;
                             ChangeLine;
                           end;
              cmMoveBlock: begin
                             ChangeLine;
                             CE; if not ValidBlock then Exit;
                             PC := GetSelection;
                             if PC = nil then
                               begin
                                  Message(@Self, evCommand, cmCopy, nil);
                                  DeleteBlock(false);
                                  Message(@Self, evCommand, cmPaste, nil);
                               end else
                               begin
                                 ChangeLine;
                                 DeleteBlock(false);
                                 InsertBlock(PC, true);
                                 Dispose(PC,Done);
                               end;
                               ChangeLine;
                           end;
              cmReverseSearch: begin
                                 ChangeLine;
                                 SearchData.Dir := SearchData.Dir xor 1;
                                 if SearchOnDisplay then Search(Delta.X, Delta.Y);
                                 Search(Delta.X, Delta.Y);
                                 SearchData.Dir := SearchData.Dir xor 1;
                                 ChangeLine; CE;
                                 SearchOnDisplay := false;
                               end;
              cmWindowsPaste: PasteWinBlock;
              cmChangeCharCase: ChangeCharCaseDirect; {-$VIV}
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
              cmRusEngConv: {-$VIV}
                begin if HandleCommand(Event) then DrawView; CE end;
              cmContSearch: begin ChangeLine; Search(Delta.X, Delta.Y); ChangeLine; CE end;
              cmSpecChar,cmAsciiTable: begin
                             ASCIITable;
                             CE
                          end;
              cmLoadText: begin
                           CE;
                           if Modified then
                            begin
                             I := AskSave;
                             if I = cmYes then Message(@Self, evCommand, cmSaveText, nil);
                             if I = cmCancel then Exit;
                            end;
                           MIOpenFile(@Self);
                           Exit;
                          end;
              cmInsertOn,cmInsertOff: begin InsertMode := Event.Command = cmInsertOn; CE end;
              cmSwitchIns: begin InsertMode := not InsertMode; DrawView; CE end;
              cmSaveAll: begin GlobalMessage(evCommand, cmSaveText, nil); CE end;
              cmSaveText: begin
                           ChangeLine;
                           if EditName<>'' then MISaveFile(@Self) else MISaveFileAs(@Self);
                           CE; Owner^.Redraw;
                          end;
              cmSaveTextAs: begin ChangeLine; MISaveFileAs(@Self); CE; Owner^.Redraw; end;
              cmBlockRead: begin ChangeLine; BlockRead; ChangeLine; CE end;
              cmBlockWrite: begin BlockWrite; CE end;
              cmDelChar: begin
                           if (EditorDefaults.EdOpt and (ebfPbl+ebfObl)=ebfObl)
                             and ValidBlock then DeleteBlock(true) else MakeDel;
                           CED
                         end;
              cmEnter: begin
                        BlockOff;
                        MakeEnter;
                        ScrollTo(Delta.X, Delta.Y);
                        CED
                       end;
              cmEnd: begin UnMark := true;
                           while (WorkString <> '') and (WorkString[Length(WorkString)] = ' ') do SetLength(WorkString, Length(WorkString)-1);
                           ScrollTo(Length(WorkString), Delta.Y); CED end;
              cmInsLine: begin
                          BlockOff;
                          PP := Delta; MakeEnter; Delta := PP;
                          ChangeLine; ScrollTo(PP.X, PP.Y); CED end;
              cmTab: begin BlockOff; DoTab; CE end;
              cmWordRight: begin UnMark := true; WordRight; ScrollTo(LastX, LastY); CED end;
              cmWordLeft: begin UnMark := true; WordLeft; ScrollTo(LastX, LastY); CED end;
              cmDeleteLine: begin BlockOff; DeleteLine; CE end;
              cmSwitchBlock: begin UnMark := true; VertBlock := not VertBlock; DrawView; CE end;
              cmClear: begin UnMark := true; if BlockVisible and ValidBlock then DeleteBlock(true); CE end;
              cmCopy: begin UnMark := true; if BlockVisible and ValidBlock then CopyBlock; CE end;
              cmCut: begin UnMark := true; if BlockVisible and ValidBlock then
                            begin CopyBlock; DeleteBlock(true) end; CE end;
              cmPaste: begin UnMark := true; PasteBlock; CE end;
              cmMoveUp: begin UnMark := true; Event.What := evKeyDown; Event.KeyCode := kbUp end;
              cmPgUp: begin UnMark := true; Dec(Pos.Y, Size.Y); VScroll^.SetValue(Delta.Y-Size.Y); DrawView; CE end;
              cmPgDn: begin UnMark := true; Inc(Pos.Y, Size.Y); VScroll^.SetValue(Delta.Y+Size.Y); DrawView; CE end;
              cmMoveDown: begin UnMark := true; Event.What := evKeyDown; Event.KeyCode := kbDown end;
              cmMoveLeft: begin UnMark := true; Event.What := evKeyDown; Event.KeyCode := kbLeft end;
              cmMoveRight: begin UnMark := true; Event.What := evKeyDown; Event.KeyCode := kbRight end;
              cmHideBlock: begin BlockVisible := not BlockVisible; UnMark := false; CED end;
              cmScrollUp: if Pos.Y > 0 then
                          begin
                           UnMark := true;
                           if not (Delta.Y - Pos.Y + 1 < Size.Y) then Dec(Delta.Y);
                           Dec(Pos.Y);
                           ScrollTo(Delta.X, Delta.Y);
                           Delta.Y := VScroll^.Value; DrawView;
                          end;
              cmScrollDn: if Pos.Y < FileLines^.Count - 1 then
                          begin
                           UnMark := true;
                           if not (Delta.Y > Pos.Y) then Inc(Delta.Y);
                           Inc(Pos.Y);
                           ScrollTo(Delta.X, Delta.Y);
                           Delta.Y := VScroll^.Value; DrawView;
                          end;
              cmBlockStart: begin UnMark := true; Marking := false; Mark.A := Delta;
                                  BlockVisible := true; DrawView; CE end;
              cmBlockEnd: begin UnMark := true; Marking := false; Mark.B := Delta;
                                BlockVisible := true; DrawView; CE end;
              cmPlaceMarker1..cmPlaceMarker9:
                begin
                  UnMark := true;
                  Event.InfoByte:=Event.Command-cmPlaceMarker1+1;
                  if (MarkPos[Event.InfoByte].Y = Delta.Y) then
                    FillChar(MarkPos[Event.InfoByte], SizeOf(TPoint), $FF)
                    else MarkPos[Event.InfoByte] := Delta;
                  CED;
                end; {-$VIV}
              cmGotoMarker1..cmGotoMarker9:
                begin ChangeLine;
                      UnMark := true;
                      Event.InfoByte:=Event.Command-cmGotoMarker1+1;
                      if not MarkPos[Event.InfoByte].EqualsXY(-1, -1) then
                        begin
                          Delta := MarkPos[Event.InfoByte];
                          Pos.X := Delta.X - Size.X div 2;
                          Pos.Y := Delta.Y - Size.Y div 2; ChangeLine; {WorkModify := false;}
                          ScrollTo(Delta.X, Delta.Y); DrawView; CenterScreen;
                        end;
                      CE
                end;
              cmDelBackChar: begin BlockOff; MakeBack; CE end;
              cmDelWordRight: begin
                               BlockOff;
                               if (LastX >= Length(WorkString)) then
                                begin
                                 MakeDel; While (LastX >= Length(WorkString)) and (LastY + 1 < FileLines^.Count)
                                                or (LastX < Length(WorkString)) and (WorkString[LastX+1] = ' ') do MakeDel;
                                 CED; Exit;
                                end;
                               WorkModify;
                               if (WorkString[LastX+1] in BreakChars-[' ']) then begin MakeDel; CED; Exit; end;
                               if (WorkString[LastX+1] = ' ') then
                                 while (LastX < Length(WorkString)) and (WorkString[LastX+1] = ' ') do MakeDel
                                else begin
                                      While (LastX < Length(WorkString)) and not (WorkString[LastX+1] in BreakChars) do MakeDel;
                                      while (LastX < Length(WorkString)) and (WorkString[LastX+1] = ' ') do MakeDel
                                     end;
                               CED;
                              end;
              cmDelWordLeft: begin
                              BlockOff;
                              if LastX = 0 then begin MakeBack; CED; Exit end;
                              if LastX >= Length(WorkString)
                               then if Length(WorkString) > 0
                                     then LastX := Length(WorkString)
                                     else begin MakeBack; CED; Exit end;
                              if (WorkString[LastX] in BreakChars-[' '])
                               then begin MakeBack; CED; Exit; end;
                              WorkModify;
                              if (LastX > 0) and (WorkString[LastX] = ' ') then
                               while (LastX > 0) and
                                     (WorkString[LastX] = ' ') do
                                MakeSmallBack;
                              While (LastX > 0) and
                                    not (WorkString[LastX] in BreakChars) do
                               MakeSmallBack;
                              ScrollTo(LastX, LastY); CED;
                             end;
              cmSwitchDrawMode: begin
                                  DrawMode := (DrawMode + 1) mod 3;
                                  Owner^.Redraw; LastDir := -1; CE;
                                end;
              cmOpenFileAtCursor: OpenFileAtCursor; {-$VIV 18.05.99--}
             end;
  evKeyDown:
   {JO: по Ctrl-Alt-Shift-цифра переходим к закладкам с пометкой текста}
   {    от текущего места до закладки                                  }
         case Event.KeyCode of
           kbCtrlAltShift1: Message(@Self, evCommand, cmGotoMarker1, nil);
           kbCtrlAltShift2: Message(@Self, evCommand, cmGotoMarker2, nil);
           kbCtrlAltShift3: Message(@Self, evCommand, cmGotoMarker3, nil);
           kbCtrlAltShift4: Message(@Self, evCommand, cmGotoMarker4, nil);
           kbCtrlAltShift5: Message(@Self, evCommand, cmGotoMarker5, nil);
           kbCtrlAltShift6: Message(@Self, evCommand, cmGotoMarker6, nil);
           kbCtrlAltShift7: Message(@Self, evCommand, cmGotoMarker7, nil);
           kbCtrlAltShift8: Message(@Self, evCommand, cmGotoMarker8, nil);
           kbCtrlAltShift9: Message(@Self, evCommand, cmGotoMarker9, nil);
         else
          if (Event.KeyCode <> kbCtrlAltX) then
          if (Event.KeyCode = kbCtrlU) then WordEnd else {-$VIV}
            begin
             if DrawMode > 0 then
               begin
                 case Event.KeyCode of
                    kbRight, kbCtrlD, kbShiftRight, kbCtrlRight:
                      begin DrawLine(1); Exit end;
                    kbLeft, kbCtrlS, kbShiftLeft, kbCtrlLeft:
                      begin DrawLine(3); Exit end;
                    kbUp, kbCtrlE, kbShiftUp:
                      begin DrawLine(0); Exit end;
                    kbDown, kbCtrlX, kbShiftDown:
                      begin DrawLine(2); Exit end;
                 end;
               end;
     (*      if Event.KeyCode = kbCtrlEnter then ; *) {-$VIV 18.05.99--}
     (*      begin BlockOff; MakeEnter; ScrollTo(Delta.X, Delta.Y); CED; Exit end; *)
             CFind := false; EvStr := #0#0;
             for I := 1 to MaxCommands do
              with EditCommands[I] do
               begin
                if (CC1[1] = Event.CharCode) then
                   begin EvStr[1] := CC1[1];
                         if CC1[2]<>#0 then
                          begin KeyEvent(Event); EvStr[2] := Event.CharCode end;
                         Break;
                   end;
                if (CC2[1] = Event.CharCode) then
                   begin EvStr[1] := CC2[1];
                         if CC2[2]<>#0 then
                          begin KeyEvent(Event); EvStr[2] := Event.CharCode end;
                         Break;
                   end;
               end;
             if EvStr[2] in ['A'..']','a'..'z'] then EvStr[2] := Char(Ord(UpCase(EvStr[2]))-64);
             for I := 1 to MaxCommands do
              with EditCommands[I] do
                if ((C1 = Event.KeyCode) or (C2 = Event.KeyCode)) and (EvStr[2]=#0) or
                   (EvStr<>#0#0) and ((CC1 = EvStr) or (CC2 = EvStr)) then
                    begin CFind := True; Break end;
             if CFind then
              begin CE; MessageL(Owner, evCommand, EditCommands[I].C,
                                Byte(EvStr[2])); Exit end;
{ AK155 9-07-2002 После введения Козловым новой системы кодирования клавиш
серые + и - с шифтом (перелистывание однотипных окон) стали иметь
ненулевой CharCode, поэтому их приходится анализировать специально.
Интересно, больше подобных ситуаций нет?}
             with Event do if (CharCode>#31)
                              and (KeyCode <> kbShiftGrayPlus)
                              and (KeyCode <> kbShiftGrayMinus)
 {Есть! Alt-BS. На всякий случай отсекаем все с Alt и Ctrl}
 {JO: c Alt отсекать нельзя, т.к. не работает Alt-ввод}
                              and (KeyCode and $40000 = 0)
                           then InputChar;
            end;
      end;
  evBroadcast: case Event.Command of
                cmFindEdit:
                  if PString(Event.InfoPtr)^=EditName then begin
                    if Owner<>nil then begin
                      Owner^.Select;
                      ClearEvent(Event);
                    end;
                  end;
                cmScrollBarChanged: begin
                                     if HScroll <> nil then Delta.X := HScroll^.Value;
                                     if VScroll <> nil then Delta.Y := VScroll^.Value;
                                     if LastY <> Delta.Y then ChangeLine;
                                     ChPosition := (not EdOpt.HiliteColumn or
                                                   (LastPos.X = Delta.X)) and (LastPos.X >= 0);
                                     BMarking;
                                     DrawView;
                                    end;
                {else CalcMenu;}
               end;
  evMouseDown: begin
                  if MouseButtons = 0 then Exit;
                  LineMarking := False;
                  if Event.Double then
                    begin
                      Message(@Self, evCommand, cmMarkWord, nil);
                      CE; Exit;
                    end;
                  LineMarking := ShiftState and 4 <> 0;
                  ChangeLine;
                  EnableMarking := false;
                  MakeLocal(Event.Where, Delta);
                  Delta.X := Pos.X + Delta.X;
                  Delta.Y := Pos.Y + Delta.Y;
                  BlockOff;
                  Sel.A := Delta; Sel.B := Delta;
                  ScrollTo(Delta.X, Delta.Y);
                  Sel.A := Delta; Sel.B := Delta;
                  EnableMarking := true;
                  if Event.Buttons and mbLeftButton <> 0 then
                   begin BlockVisible := true; MouseMark := true; end
                    else begin RulerVisible := true; DrawView; end;
                  I := RepeatDelay; RepeatDelay := 0;
                  if LineMarking then
                    begin
                      Sel.A.X := 0; Sel.B.Y := Sel.A.Y+1; Sel.B.X := 0;
                      Mark := Sel;
                      DrawView;
                    end;
                  repeat
                   MakeLocal(Event.Where, T);
                   if MouseInView(Event.Where) then
                    begin
                     Delta.X := Pos.X + T.X;
                     Delta.Y := Pos.Y + T.Y;
                     ScrollTo(Delta.X, Delta.Y);
                     OldDelta := Delta;
                    end else
                    begin
                     if T.X < 0 then Dec(Delta.X)
                      else if T.X >= Size.X then Inc(Delta.X);
                     if Delta.X<1
                      then Delta.X:=0
                      else if Delta.X>LimitX
                            then Delta.X:=LimitX;
                     if T.Y < 0 then Dec(Delta.Y)
                      else if T.Y >= Size.Y then Inc(Delta.Y);
                     if Delta.Y<1
                      then Delta.Y:=0
                      else if Delta.Y>LimitY
                            then Delta.Y:=LimitY;
                     ScrollTo(Delta.X, Delta.Y);
                     OldDelta := Delta;
                    end;
                  until not MouseEvent(Event, evMouseAuto + evMouseMove);
                  MouseMark := false; RulerVisible := false;
                  RepeatDelay := I;
                  ScrollTo(Delta.X, Delta.Y);
                  ChangeLine;
                  if LineMarking then
                    begin
                      if (Mark.B.X > 0) and (Mark.B.Y < FileLines^.Count-1)
                        then begin Inc(Mark.B.Y); Mark.A.X := 0 end;
                      if (Mark.A.X > 0) then Mark.A.X := 0;
                      CED;
                      LineMarking := False;
                      Exit;
                    end;
                  CED;
               end;
 end;
end;

procedure TFileEditor.DoHighlite
          ( var B; const S : LongString; const Attr : String );
var
  i : Integer;
  j : Integer;
  k : Integer;
  l : Integer;
  c : Char;
begin
  i := Pos.X + 1;
  j := 0;
  l := Min ( Length(S), i+Size.X );
  while (i <= l) do begin
    c := S[i];
    k := i + 1;
    while (k <= l) and (S[k] = c) do
      Inc ( k );
    if (c <> #0) and (Ord(c) <= Length(Attr)) then
      MoveColor ( TWordArray(B)[j], k-i, Ord(Attr[Ord(c)]) );
    Inc ( j, k-i );
    i := k;
  end;
end;

procedure OpenSmartpad;
  var R: TRect;
      PV: Pointer;

     procedure InsertInfo; {-$VIV} {SYR}
     Var
         Str:LongString;
         Idx:Integer;
     begin
         with SmartWindow^.Intern^ do
         begin
             if SPInsertDate then
             begin
                 Str:='';
                 For Idx:=0 To 5 Do Str:=Str+Char(SPLineChar);
                 Str:=Str+'< '+GetDateTime(false)+' '+GetDateTime(true)+' >';
                 For Idx:=0 To 35 Do Str:=Str+Char(SPLineChar);
                 { Flash >>> }
                 if (Copy(GetLine(FileLines^.Count-2),1,7)=Copy(Str,1,7)) and
                    (Copy(GetLine(FileLines^.Count-2),29,37)=Copy(Str,29,37)) and
                    (((((Delta.Y>FileLines^.Count-2) and (WorkString='')) or
                       (Delta.Y=FileLines^.Count-2)) or
                      (Delta.Y<FileLines^.Count-2))) and
                    (GetLine(FileLines^.Count-1)='') then
                 begin
                     FileLines^.AtDelete(FileLines^.Count-2);
                     FileLines^.AtDelete(FileLines^.Count-1);
                 end;
                 if ((Delta.Y>FileLines^.Count-2) and (WorkString<>'')) and not
                    ((Copy(WorkString,1,7)=Copy(Str,1,7)) and
                     (Copy(WorkString,29,37)=Copy(Str,29,37))) or
                    (GetLine(FileLines^.Count-1)<>'') then
                  FileLines^.Insert(nil);
                 { Flash <<< }
                 FileLines^.Insert(NewLongStr(Str));
             end;
             FileLines^.Insert(nil);
             SetLimits;
             ScrollTo(0, FileLines^.Count-1);
             Pos.X := Delta.X - Size.X div 2;
             Pos.Y := Delta.Y - Size.Y div 2;
             SmartWindow^.Redraw;
         end;
     end;

{--- start -------- Eugeny Zvyagintzev ---------}
Var
  PS: PString;
  V: PFileEditor;
  I: Integer;
  P: PEditRecord;
{--- finish -------- Eugeny Zvyagintzev ---------}
begin
  SmartWindow := SmartWindowPtr^; {Cat}

  if (SmartWindow <> nil) and SmartWindow^.GetState(sfModal) then Exit;
  PV := Application^.TopView;
  Desktop^.GetExtent(R);
  R.Grow(-2,-2);
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
        end else SmartWindow^.Select;
      Exit;
    end;
  New(SmartWindow, Init(R, 'SmartPad'));
{--- start -------- Eugeny Zvyagintzev ---------}
{Now DN will save and load SmartPad edit history}
  V:=SmartWindow^.Intern;
  FreeStr:=V^.EditName; System.Insert(' ', FreeStr, 1); UpStr(FreeStr);
  PS := @FreeStr;
  if (InterfaceData.Options and ouiTrackEditors <> 0) and (EditHistory <> nil)
      then I := EditHistory^.IndexOf(@PS) else I := -1;
  if I >= 0 then
    begin
     P:=EditHistory^.At(I);
     R.Assign(P^.fOrigin.X, P^.fOrigin.Y, P^.fOrigin.X + P^.fSize.X, P^.fOrigin.Y + P^.fSize.Y);
     AdjustToDesktopSize(R, P^.fDeskSize);
     SmartWindow^.Locate(R);
     With V^, P^ Do
      Begin
       MarkPos                := fMarks;
       Mark.A                 := fBlockStart;
       Mark.B                 := fBlockEnd;
       ScrollTo(fDelta.X, fDelta.Y);
       Pos                    := fPos;
       BlockVisible           := fBlockVisible;
       VertBlock              := fVerticalBlock;
       EdOpt.HiLite           := fHighlight;
       EdOpt.HiliteColumn     := fHiliteColumn;
       EdOpt.HiliteLine       := fHiliteLine;
       EdOpt.AutoIndent       := fAutoIndent;
       EdOpt.AutoJustify      := fAutoJustify;
       EdOpt.AutoBrackets     := fAutoBrackets;
       EdOpt.LeftSide         := fLeftSide;
       EdOpt.RightSide        := fRightSide;
       EdOpt.InSide           := fInSide;
       InsertMode             := fInsMode;
       KeyMap                 := fKeyMap; {-$VIV}
       { Flash >>> }
       EdOpt.BackIndent       := fBackIndent;
       EdOpt.AutoWrap         := fAutoWrap;
       OptimalFill            := fOptimalFill;
       TabReplace             := fTabReplace;
       EdOpt.SmartTab         := fSmartTab;
       { Flash <<< }
       WorkString := GetLine(fDelta.Y); {-$VIV 11.05.99}
      End;
   End
  Else
   StoreEditInfo(SmartWindow);
{--- finish -------- Eugeny Zvyagintzev ---------}

  InsertInfo;
  if (PV <> Application) then
     begin
       Desktop^.ExecView(SmartWindow);
       SmartWindow^.Free;
       SmartWindow := nil
     end else Desktop^.Insert(SmartWindow);
end;

procedure OpenClipboard; {-$VOL begin}
  var R: TRect;
      PV: Pointer;
begin
  ClipboardWindow := ClipboardWindowPtr^; {Cat}

  if SystemData.Options and ossUseSysClip <> 0 then SyncClipOut{(true)};
  if (ClipboardWindow <> nil) and ClipboardWindow^.GetState(sfModal) then Exit;
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
        end else ClipboardWindow^.Select;
      Exit;
    end;
  New(ClipboardWindow, Init(R, 'Clipboard'));
  if (PV <> Application) then
     begin
       Desktop^.ExecView(ClipboardWindow);
       ClipboardWindow^.Free;
       ClipboardWindow := nil
     end else Desktop^.Insert(ClipboardWindow);
end; {-$VOL end}

end.
