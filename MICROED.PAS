{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.12
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
  advance, advance1, advance2, advance3, Startup, Menus, DNStdDlg, Dialogs,
  Commands, UniWin, RStrings, DNHelp, SBlocks, ObjType, U_KeyMap, collect,
  ed2, highlite;


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
    HScroll, VScroll: PScrollBar;
    ReplaceAll: Boolean;
    Delta: TPoint;
    EditName: String; {DataCompBoy}
    FileLines: PCollector;
    IsValid,Marking: Boolean;
    SmartPad: Boolean;
    ClipBrd: Boolean; {-$VOL}
    OptMenu: PMenu;
    Mark,Sel: TRect;
    Pos, LastPos{,BlockPos}: TPoint;
    MarkPos: TPosArray;
    LastLine: LongInt;
    DrawMode: Integer;
    WorkString,LastShape: String;
    OldBlockValid, SearchOnDisplay,
    InsertMode,VertBlock,Modified,WorkModified,
    BlockVisible,SpecChar,WasDelete,MouseMark, UnMark,
    LineMarking,OptimalFill,RulerVisible,EnableMarking,
    TabReplace, SearchActive: Boolean;
    UndoInfo: PCollection;
    RedoInfo: PCollection; {-$VOL}
    UndoTimes: LongInt;
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
    constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
      var FileName: String); {DataCompBoy}
    constructor Load(var S: TStream);
    destructor  Done; virtual;
    procedure Store(var S: TStream);
    procedure Awaken; virtual;
    procedure DoHighlite ( var B; const S : String; const Attr : string );
    procedure HandleEvent(var Event : TEvent);virtual;
    procedure Draw; virtual;
    function  Valid(Command: Word): Boolean; virtual;
    function  GetPalette : PPalette;virtual;
    function  GetLine(Index : LongInt) : String;
    function  GetSelection : PCollection;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    function  ValidBlock: Boolean;
    procedure CalcMenu;
    function  Search: Boolean;
    procedure InsertBlock(ABlock: PCollection; SaveUndo: Boolean);
    procedure ModifyLine(Index : LongInt; S : String; DelSpaces: Boolean);
    procedure SetLimits;
    procedure ChangeBounds(var R: TRect); virtual;
    procedure ScrollTo(DeltaX, DeltaY: LongInt);
    function  LimitX:LongInt;
    function  LimitY:LongInt;
    procedure StoreUndoInfo(What: Word; Where: TPoint; var Info);
    function  HandleCommand(var Event: TEvent): Boolean; virtual;
    function  KeyMapConvertStr(S: String; toAscii: Boolean): String; {-$VIV}
    procedure KeyMapAtInsert(N: LongInt; P: PString); {-$VIV}
    procedure KeyMapAtReplace(N: LongInt; P: PString); {-$VIV}
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

const
      SearchData: TSearchData = (Options:4; Dir: 0; Scope:0; Origin:0; Line:''; What:'');

type TEditCommand = record
      C,C1,C2: AWord;
      CC1, CC2: Array[1..2] of Char;
     end;

const MaxCommands: AInt= 0;
var   EditCommands : Array [1..110] of TEditCommand;

implementation

uses Gauge, FViewer, Histlist, Macro, Editor, WinClp, DNUtil, Histries,
     EdWin, xTime, FileCopy, ASCIItab, DnIni, U_SrchF, Microed2; {-$VIV}

const
    On          = True;
    Off         = False;
    cmNoCommand = 4000;
    SmartWindow : PEditWindow = nil;

    ClipboardWindow : PEditWindow = nil;

function MemAvail: LongInt; begin MemAvail := MemAdjust(System.MemAvail) end;

function TFileEditor.KeyMapConvertStr(S: String; toAscii: Boolean): String; {-$VIV}
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

procedure TFileEditor.KeyMapAtInsert(N: LongInt; P: PString); {-$VIV}
begin
  if (P <> nil) then P^ := KeyMapConvertStr(P^, false);
  FileLines^.AtInsert(N, P);
end; {-$VIV}

procedure TFileEditor.KeyMapAtReplace(N: LongInt; P: PString); {-$VIV}
begin
  if (P <> nil) then P^ := KeyMapConvertStr(P^, false);
  FileLines^.AtReplace(N, P);
end; {-$VIV}

procedure TFileEditor.Convert4do(iP: PUndoRec;DoKind: TDoKind);
  var P: PUndoRec;
      I, J: LongInt;
      S: String;
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
               P^.Str:=NewStr(copy(GetLine(Where.Y),P^.Where.X+1,Count));
               UndoInfo^.Insert(P);
             end;
          udInsChar:
             begin
               P^.Count:=byte(Str^[0]);
               Inc(P^.Where.X,P^.Count);
               UndoInfo^.Insert(P);
             end;
          udDelLine:
             begin
               P^.Lines := New(PLineCollection, Init(10, 10));
               for I:=0 to Count-1 do
                 P^.Lines^.Insert(NewStr(GetLine(P^.Where.Y+I)));
               UndoInfo^.Insert(P);
             end;
          udInsLine:
             begin
               P^.Str:=NewStr(GetLine(Where.Y));
               UndoInfo^.Insert(P);
             end;
          udDelBlock:
             begin
               P^.Lines := New(PLineCollection, Init(10, 10));
               if Lines^.Count>0 then S:=CnvString(Lines^.At(0))else S:='';
               J:=byte(S[0]);
               if Vertical then
                 begin
                   for I:=0 to Lines^.Count-1 do
                     begin
                       S:=copy(GetLine(Where.Y+I),Where.X+1,J);
                       P^.Lines^.Insert(NewStr(S));
                     end;
                 end
               else
                 begin
                   for I:=0 to Lines^.Count-1 do
                     begin
                       S:=GetLine(Where.Y+I);
                       if I=0 then
                         S:=copy(S,Where.X+1,255)
                       else if I=Lines^.Count-1 then
                         S:=copy(S,1,byte(S[0])-(J-Where.X));
                       P^.Lines^.Insert(NewStr(S));
                     end;
                 end;
               UndoInfo^.Insert(P);
             end;
          udInsBlock, udFormatBlock:
             begin
               I:=Lines^.Count;
               P^.Str:=NewStr(Char(Lo(I))+Char(Hi(I))+GetLine(Delta.Y));
               UndoInfo^.Insert(P);
             end;
          udBackDel:
             begin
               P^.Str:=NewStr(copy(GetLine(Where.Y),Where.X+1,Count));
               UndoInfo^.Insert(P);
             end;
          udSubDel:
             begin
               P^.Str:=NewStr(GetLine(Where.Y));
               UndoInfo^.Insert(P);
             end;
          udSubDelLine:
             begin
               P^.Str:=NewStr(GetLine(Where.Y+1));
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
               if P^.Count>0 then S:=CnvString(Lines^.At(0)) else S:='';
               P^.Width:=byte(S[0]);
               for i:=1 to P^.Count do begin
                j:=Length(CnvString(Lines^.At(i-1)));
                if j>P^.Width then P^.Width:=J;
               end;
               UndoInfo^.Insert(P);
             end;
          udReplace,udReplaceAll:
             begin
               S:=GetLine(Where.Y);
               P^.Str:=NewStr(Char(byte(Str^[0])-1)+copy(S,Where.X+1,byte(Str^[1])));
               UndoInfo^.Insert(P);
             end;
          udReplaceChar:
             begin
               S:=GetLine(Where.Y);
               Inc(P^.Where.X,byte(Str^[0]));
               P^.Str:=NewStr(copy(S,Where.X+1,byte(Str^[0])));
               UndoInfo^.Insert(P);
             end;
          udReplaceBlock,udClearBlock:
             begin
               P^.Lines:=New(PLineCollection, Init(10, 10));
               for I := Where.Y to Lines^.Count + Where.Y - 1 do
                 begin
                   S := GetLine(I);
                   J := length(CnvString(Lines^.At(I-Where.Y)));
                   P^.Lines^.Insert(NewStr(copy(S,Where.X+1,J)));
                 end;
               UndoInfo^.Insert(P);
             end;
          udStrModified:
             begin
               P^.Str:=NewStr(GetLine(Where.Y));
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
               P^.Count:=byte(Str^[0]);
               Inc(P^.Where.X,P^.Count);
               RedoInfo^.Insert(P);
             end;
          udInsChar:
             begin
               Dec(P^.Where.X,Count);
               P^.Str:=NewStr(copy(GetLine(Where.Y),P^.Where.X+1,Count));
               RedoInfo^.Insert(P);
             end;
          udDelLine:
             begin
               P^.Count:=Lines^.Count;
               RedoInfo^.Insert(P);
             end;
          udInsLine:
             begin
               P^.Lines := New(PLineCollection, Init(10, 10));
               P^.Lines^.Insert(NewStr(GetLine(P^.Where.Y)));
               P^.Lines^.Insert(NewStr(GetLine(P^.Where.Y+1)));
               RedoInfo^.Insert(P);
             end;
          udDelBlock:
             begin
               P^.Lines := New(PLineCollection, Init(10, 10));
               if Lines^.Count>0 then S:=CnvString(Lines^.At(0))else S:='';
               J:=byte(S[0]);
               if Vertical then
                 if InsM then
                   begin
                     for I:= 0 to Lines^.Count-1 do
                       P^.Lines^.Insert(NewStr(S));
                   end
                 else
                   begin
                     for I:=0 to Lines^.Count-1 do
                       begin
                         S:=copy(GetLine(Where.Y+I),Where.X+1,J);
                         P^.Lines^.Insert(NewStr(S));
                       end;
                   end
               else
                 begin
                   S:=GetLine(P^.Where.Y);
                   for I:=0 to Lines^.Count-1 do
                     P^.Lines^.Insert(NewStr(S));
                 end;
               RedoInfo^.Insert(P);
             end;
          udInsBlock, udFormatBlock:
             begin
               P^.Lines := New(PLineCollection, Init(10, 10));
               for I:=1 to byte(Str^[2])*256+byte(Str^[1]) do
                 P^.Lines^.Insert(NewStr(GetLine(Delta.Y+I-1)));
               RedoInfo^.Insert(P);
             end;
          udBackDel:
             begin
               P^.Count:=byte(Str^[0]);
               RedoInfo^.Insert(P);
             end;
          udSubDel:
             begin
               P^.Str:=NewStr(GetLine(P^.Where.Y));
               RedoInfo^.Insert(P);
             end;
          udSubDelLine:
             begin
               S:=GetLine(Where.Y);
               P^.Str:=NewStr(S);
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
               P^.Lines := New(PLineCollection, Init(10, 10));
               for I:=1 to Count do
               begin
                 S:=Copy(GetLine(I+Where.Y-1),Where.X+1,Width);
                 P^.Lines^.Insert(NewStr(S));
               end;
               P^.Vertical:=true; P^.InsM:=true;
               RedoInfo^.Insert(P);
             end;
          udReplace,udReplaceAll:
             begin
               S:=GetLine(Where.Y);
               P^.Str:=NewStr(Char(byte(Str^[0])-1)+copy(S,Where.X+1,byte(Str^[1])));
               RedoInfo^.Insert(P);
             end;
          udReplaceChar:
             begin
               S:=GetLine(Where.Y);
               Dec(P^.Where.X,byte(Str^[0]));
               P^.Str:=NewStr(copy(S,P^.Where.X+1,byte(Str^[0])));
               RedoInfo^.Insert(P);
             end;
          udReplaceBlock,udClearBlock:
             begin
               P^.Lines:=New(PLineCollection, Init(10, 10));
               for I := Where.Y to Lines^.Count + Where.Y - 1 do
                 begin
                   S := GetLine(I);
                   J := length(CnvString(Lines^.At(I-Where.Y)));
                   P^.Lines^.Insert(NewStr(copy(S,Where.X+1,J)));
                 end;
               RedoInfo^.Insert(P);
             end;
          udStrModified:
             begin
               P^.Str:=NewStr(GetLine(Where.Y));
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
 MILoad(@Self, S);
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
  FileToView: Text;
  Line: String;
  MaxWidth: Integer;
  p: PString;
  Nm: String;
  Xt: String;
  I: Integer;
begin
  inherited Init(Bounds);
  LastDir := -1;
  HelpCtx := hcEditor;
  UndoInfo := nil;
  RedoInfo := nil;
  HScroll := AHScrollBar;
  VScroll := AVScrollBar;
  GrowMode := gfGrowHiX + gfGrowHiY;
  Options := Options or ofSelectable;
  SearchActive := Off; UnMark := On;
  EventMask := $FFFF;
  IsValid := True;

  EdOpt.HiliteColumn := EditorDefaults.Defaults and ebfHCl <> 0;
  EdOpt.HiliteLine   := EditorDefaults.Defaults and ebfHLn <> 0;
  EdOpt.AutoIndent   := EditorDefaults.Defaults and ebfAId <> 0;
  VertBlock          := EditorDefaults.Defaults and ebfVBl <> 0;
  EdOpt.BackIndent   := EditorDefaults.Defaults and ebfBSU <> 0;
  OptimalFill        := EditorDefaults.Defaults and ebfOfl <> 0;
  EdOpt.AutoJustify  := EditorDefaults.Defaults and ebfJwr <> 0;
  EdOpt.AutoBrackets := EditorDefaults.Defaults and ebfAbr <> 0;
  EdOpt.AutoWrap     := EditorDefaults.Defaults and ebfAwr <> 0;
  TabReplace         := EditorDefaults.Defaults and ebfTRp <> 0;
  EdOpt.SmartTab     := EditorDefaults.Defaults and ebfSmt <> 0;

  Val(EditorDefaults.RM, EdOpt.RightSide, I); if I <> 0 then EdOpt.RightSide := 76;
  Val(EditorDefaults.LM, EdOpt.LeftSide, I);  if I <> 0 then EdOpt.LeftSide := 0;
  Val(EditorDefaults.PM, EdOpt.InSide, I);    if I <> 0 then EdOpt.InSide := 5;
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
  FileLines := GetCollector(3000, 100);
  Macros := New(PCollection, Init(10,10));

(*if UpperCase(DefCodePage) = 'WIN' then KeyMap := kmAnsi
   else if UpperCase(DefCodePage) = 'KOI' then KeyMap := kmKoi8r
    else KeyMap := kmAscii; {-$VIV}*)
  MenuItemStr[True]:=NewStr(GetString(dlMenuItemOn));
  MenuItemStr[False]:=NewStr(GetString(dlMenuItemOff))
end;

destructor TFileEditor.Done;
begin
  DisposeStr(MenuItemStr[False]);
  DisposeStr(MenuItemStr[True]);
  if FileLines<>nil then Dispose(FileLines,Done); FileLines:=nil;
  if UndoInfo <>nil then Dispose(UndoInfo,Done);  UndoInfo :=nil;
  if RedoInfo <>nil then Dispose(RedoInfo,Done);  RedoInfo :=nil;
  if Macros   <>nil then Dispose(Macros,Done);    Macros   :=nil;
  if Locker   <>nil then Dispose(Locker,Done);    Locker   :=nil;
  if SmartPad then SmartWindow      :=nil;
  if ClipBrd  then ClipboardWindow  :=nil;
  inherited Done;
end;

procedure TFileEditor.StoreUndoInfo;
 var P, P1: PUndoRec;
     S: String;
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
                  begin S := P1^.Str^+Char(Info); DisposeStr(P1^.Str); P1^.Str := NewStr(S); end
                  else begin
                        S := Char(Info); New(P); P^.What := What;
                        P^.Where := Where; P^.Str := NewStr(S);
                        P^.KeyMap := KeyMap; {-$VIV}
                        UndoInfo^.Insert(P); Inc(UndoTimes);
                       end;
   udReplaceChar: if (P1 <> nil) and (P1^.What = What) and (P1^.Where.Y = Where.Y) and
                 (Where.X = P1^.Where.X) then
                  begin S := P1^.Str^+Char(Info); Inc(P1^.Where.X);
                        DisposeStr(P1^.Str); P1^.Str := NewStr(S); end
                  else begin
                        S := Char(Info); New(P); P^.What := What;
                        P^.Where := Where; P^.Str := NewStr(S);
                        Inc(P^.Where.X);
                        P^.KeyMap := KeyMap; {-$VIV}
                        UndoInfo^.Insert(P); Inc(UndoTimes);
                       end;
   udBackDel: if (P1 <> nil) and (P1^.What = What) and (P1^.Where.Y = Where.Y) and
                 (P1^.Where.X = Where.X) then
                 begin S := Char(Info)+P1^.Str^; Dec(P1^.Where.X);
                       DisposeStr(P1^.Str); P1^.Str := NewStr(S); end
                 else begin
                        S := Char(Info); New(P); P^.What := What;
                        P^.Where := Where; P^.Str := NewStr(S);
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
                P1^.Lines^.Insert(NewStr(String(Info))) else
              begin
                New(P); P^.What := What;
                P^.Where := Where;
                P^.Lines := New(PLineCollection, Init(10, 10));
                P^.Lines^.Insert(NewStr(String(Info)));
                P^.KeyMap := KeyMap; {-$VIV}
                UndoInfo^.Insert(P); Inc(UndoTimes);
              end;
   udInsLine, udSubDel, udSubDelLine, udInsBlock, udReplace, udFormatBlock, udReplaceAll:
              begin
                New(P); P^.What := What;
                P^.Where := Where;
                P^.Str := NewStr(String(Info));
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
                      P^.Str := NewStr(String(Info));
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
  var G : String;
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
  if (Clipboard <> nil) and (Clipboard^.Count > 0) or (GetWinClipSize)
     then EnableCommands([cmPaste]) else DisableCommands([cmPaste]);
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
function GetFileNameDialog(Mask, Title, Name: String; Buttons, HistoryID: Word): String;
 var S: String;
     D: PFileDialog;
     B: Boolean;
begin
  S := ''; B := Off; if Mask = '' then begin Mask := x_x; B := On end;
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
  GetFileNameDialog := {$IFNDEF OS2}lfGetLongFileName{$ENDIF}(S);
end;
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
 if HScroll <> nil then HScroll^.SetParams(Delta.X, 0, 255, 1, 1);
 if VScroll <> nil then VScroll^.SetParams(Delta.Y, 0, FileLines^.Count-1, Size.Y, 1);
end;

function TFileEditor.GetSelection;
 var  P: PCollection;
      S: String;
      I: LongInt;
begin
 GetSelection := Nil; MemEnough := On ;
 if not (BlockVisible and ValidBlock) or LowMemory then Exit;
 P := New(PLineCollection, Init(Mark.B.Y-Mark.A.Y+1,5));
 for I := Mark.A.Y to Mark.B.Y do
  begin
   S := GetLine(I);
   if VertBlock or (Mark.A.Y = Mark.B.Y) then
     begin
      S := Copy(S, Mark.A.X + 1, Mark.B.X - Mark.A.X);
      if VertBlock and (Mark.B.X - Mark.A.X > Byte(S[0])) then
         S := S + Strg(' ', Mark.B.X - Mark.A.X - Byte(S[0]));
     end
    else if I = Mark.A.Y then S := Copy(S, Mark.A.X + 1, 255)
         else if I = Mark.B.Y then S := Copy(S, 1, Mark.B.X);
   P^.Insert(NewStr(S));
   if LowMemory or not MemOK then
    begin
     Dispose(P,Done);
     MemEnough := Off ;
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
  Valid := On; V := On;
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
          ModifyLine(Delta.Y, WorkString, On);
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


function TFileEditor.Search;
 label 1;
 var I, J, TrX, StX: LongInt;
     D,DD: TPoint;
     T: TRect;
     W, S, S1: String;
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

label
  LExit, _LExit;

begin
 SearchActive := On;
 NewTimer(Tmr, 0);
{ OldCache := PBlocker(FileLines)^.UseCache;}
 Marking := Off;
 OldMark := Mark;
 if (SearchData.Scope = 1) and not (BlockVisible and ValidBlock) then goto LExit;
 Search := Off; F := On;
 Prompt := (SearchData.What <> #0) and (SearchData.Options and 4 <> 0);
 D := Delta; W := SearchData.Line; if W = '' then goto LExit;
 if SearchData.Options and 1 = 0 then UpStr(W);
 AllCP := (SearchData.What = #0) and (SearchData.Options and 8 <> 0);
 UnMark := Off;
 NumRep := 0;
 Dir := 1 - 2 * SearchData.Dir;
 while (Dir > 0) and (D.Y < FileLines^.Count) or
       (Dir < 0) and (D.Y >= 0) do
    begin
1:   S1 := GetLine(D.Y); TrX := Length(S1); S := S1;
{     PBlocker(FileLines)^.UseCache := True;}
     if (Dir < 0) then
       begin
        if (D.X > Length(S1)) then D.X := Length(S1);
        StX := 1; TrX := D.X;
        if SearchData.Scope = 1 then
         begin
            Mark := OldMark;
            if D.Y < Mark.A.Y then Break;
            if D.Y > Mark.B.Y then begin Dec(D.Y); D.X := 255; Continue; end;
            if (D.Y = Mark.B.Y) and (D.Y = Mark.A.Y) or VertBlock then
                if StX < Mark.A.X then StX := Mark.A.X + 1;
            if (D.Y = Mark.B.Y) or VertBlock then
               if TrX > Mark.B.X then TrX := Mark.B.X;
         end;
        if TrX < StX then I := 0 {-$VIV 14.05.99}
         else if AllCP then I := BackSearchForAllCP(W, S1[StX], TrX - StX + 1, SearchData.Options and 1 <> 0)
          else I := BackSearchFor(W, S1[StX], TrX - StX + 1, SearchData.Options and 1 <> 0);
       end else
           begin
             StX := D.X + 1;
             if SearchData.Scope = 1 then
              begin
               Mark := OldMark;
               if D.Y < Mark.A.Y then begin Inc(D.Y); D.X := 0; Continue; end;
               if D.Y > Mark.B.Y then Break;
               if (D.Y = Mark.B.Y) and (D.Y = Mark.A.Y) or VertBlock then
                   if StX < Mark.A.X then StX := Mark.A.X + 1;
               if (D.Y = Mark.B.Y) or VertBlock then TrX := Mark.B.X;
              end;
              if TrX < StX then I := 0 {-$VIV 14.05.99}
               else if AllCP then I := SearchForAllCP(W, S1[StX], TrX - StX + 1, SearchData.Options and 1 <> 0)
                else I := SearchFor(W, S1[StX], TrX - StX + 1, SearchData.Options and 1 <> 0);
           end;

     if (I > 0) and (SearchData.Options and 2 <> 0) then
         if not (((StX-1+I = 1) or (S1[StX-1+I-1] in BreakChars)) and
            ((StX - 1 + I + Length(W) > Length(S1)) or (S1[StX-1 + I + Length(W)] in BreakChars))) then
              begin
                if Dir > 0 then D.X := StX+I
                           else D.X := StX+I-1;
                Goto 1;
              end;

     if I > 0 then
      begin
       Search := On; F := Off;
       DD.Y := D.Y; DD.X := StX + I - 2; D := DD;
       if Dir > 0 then Inc(D.X, Length(W));
       Delta := D;
       HScroll^.Value := D.X; {HScroll^.DrawView;}
       VScroll^.Value := D.Y;
       if TimerExpired(Tmr) then
       begin
         VScroll^.DrawView;
         NewTimer(Tmr, 1);
       end;
       if (D.Y - Pos.Y > Size.Y) then Pos.Y := D.Y - Size.Y div 2
         else if (D.Y < Pos.Y) then Pos.Y := Max(0, D.Y-5);
       if (D.X - Pos.X > Size.X - 10) then Pos.X := D.X - Size.X div 2
         else if (D.X < Pos.X) then Pos.X := Max(0, D.X-5);
       if (SearchData.What = #0) or Prompt then _DrawViews;
       if SearchData.What <> #0 then
        begin
         J := cmYes;
         if Prompt then
          begin
           I := 50;
           T.A.Y := Delta.Y - Pos.Y; MakeGlobal(T.A, T.A);
           Desktop^.MakeLocal(T.A, T.A);
           T.A.X := (Desktop^.Size.X - I) div 2; T.B.X := T.A.X + I;
           if {(T.A.Y >= (Desktop^.Size.Y-12) div 2) and}
              (T.A.Y <= (Desktop^.Size.Y{+12}) div 2) then T.A.Y := (Desktop^.Size.Y+4) div 2
                   else T.A.Y := (Desktop^.Size.Y-18) div 2;
           T.B.Y := T.A.Y + 8;
           J := MessageBoxRect(T, GetString(dlQueryReplace), nil,
                    mfQuery+mfYesButton+mfAllButton+mfNoButton+mfCancelButton);
           if J = cmOK then begin J := cmYes; ReplaceAll := On; Prompt := Off end;
          end;
         if J = cmCancel then begin Mark := OldMark; _DrawViews; goto LExit; end;
         if J = cmYes then
          begin
           Modified := On;
           Inc(NumRep);{piwamoto}
           S := Copy(S1, DD.X + 1, Length(W));
           Insert(Char(Length(SearchData.What)), S, 1);
           if ReplaceAll then
             StoreUndoInfo(udReplaceAll, DD, S) {-$VOL}
           else StoreUndoInfo(udReplace, DD, S);
           Delete(S1, DD.X+1, Length(W));
           Insert(SearchData.What, S1, DD.X+1);
{           PBlocker(FileLines)^.UseCache := False;}
           ModifyLine(Delta.Y, S1, On); WorkModified := Off;
           if Dir > 0 then Dec(Delta.X, Length(W) - Length(SearchData.What));
           if (SearchData.What = #0) or Prompt then ScrollTo(Delta.X, Delta.Y);
           D := Delta;
           if (SearchData.What = #0) or Prompt then _DrawViews;
          end;
         if ReplaceAll then Goto 1
                       else begin
                              Mark := OldMark;
                              _DrawViews; goto LExit;
                            end;
        end
          else begin Mark := OldMark; LastPos.X := -1; {DrawView; }goto _LExit; end;
      end;
     if Dir > 0 then begin D.X := 0; Inc(D.Y); end
                else begin D.X := 255; Dec(D.Y); end;
  end;
  if F then MessageBox(GetString(dlDBViewSearchNot), nil, mfError+mfOKButton)
    else
      begin
        Mark := OldMark;
        _DrawViews;
      end;
  UnMark := On;
  if NumRep > 0 then
    begin
      MessageBox(GetString(dlReplacesMade), @NumRep, mfOkButton+mfInformation);
      ScrollTo(Delta.X, Delta.Y);
      _DrawViews;
    end;
LExit:
 _DrawViews;
_LExit:
{ PBlocker(FileLines)^.UseCache := OldCache;}
 SearchActive := Off;
end;

procedure TFileEditor.InsertBlock;
 var I,Q: LongInt;
     S, S1, S2: String;
     P: PString;
     L,J: LongInt;
     LL: PCollection;
begin
 if (ABlock = nil) or (ABlock^.Count = 0) then Exit;
 Modified := On;
 if SaveUndo then
  begin
    Mark.B := Delta; Mark.A := Delta; BlockVisible := On;
    Inc(Mark.B.Y, Max(0, ABlock^.Count - 1));
  end;
 if VertBlock then
  begin
   P := ABlock^.At(0);
   if P <> nil then S := P^ else S := '';
   I := Byte(S[0]);
   for J:=1 to ABlock^.Count do begin
    Q:=Length(CnvString(ABlock^.At(J-1)));
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
        LL := New(PLineCollection, Init(ABlock^.Count, 10));
        if LL <> nil then
         begin
           for I := 0 to ABlock^.Count do
             LL^.Insert(NewStr(AddSpace(Copy(GetLine(I+Delta.Y), Delta.X+1, Length(S)),I)));
           StoreUndoInfo(udReplaceBlock, Delta, LL)
         end;
       end;
   for I := 1 to ABlock^.Count do
    begin
     P := ABlock^.At(I-1);
     S1 := AddSpace(CnvString(P),Q);
     if SaveUndo then Mark.B.X := Mark.A.X + Length(S1);
     if I + Delta.Y - 1 = FileLines^.Count then
      FileLines^.Insert(NewStr(AddSpace('',Q)));
     S2 := GetLine(I+Delta.Y-1);
     if not InsertMode then Delete(S2, Delta.X + 1, Byte(S1[0]));
     if Byte(S2[0]) < Delta.X then S2 := S2 + Strg(' ', Delta.X - Byte(S2[0]));
     Insert(S1, S2, Delta.X + 1);
     ModifyLine(I+Delta.Y-1, S2, On);
    end
   end
   else
  begin
   I := ABlock^.Count;
   S := Char(Lo(I)) + Char(Hi(I)) + GetLine(Delta.Y);
   if SaveUndo then StoreUndoInfo(udInsBlock, Delta, S);
   Delete(S, 1, 2);
   if Delta.X > Byte(S[0]) then S := S + Strg(' ', Delta.X - Byte(S[0]));
   S1 := Copy(S, 1, Delta.X); S2 := Copy(S, Delta.X+1, 255);
   FileLines^.AtFree(Delta.Y);
   for I := 1 to ABlock^.Count do
   begin
     P := ABlock^.At(I-1); if P <> nil then S := P^ else S := '';
     if I = 1 then S := S1 + S;
     if I = ABlock^.Count then
       begin if SaveUndo then Mark.B.X := Length(S); S := S + S2; end;
     KeyMapAtInsert(Delta.Y + I - 1, NewStr(S)); {-$VIV}
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
  HP : string[6];
  CC: Array[1..12] of Byte;
  I, A: LongInt;
  S: String;
  P: PString;
  WM, BV: Boolean;
  X1, X2: LongInt;
  LPos: Integer;
  Ch: Char;
var B: TDrawBuffer;


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

  var BB: TPoint;

begin
  BB := Pos;
  SearchOnDisplay := Off;
  if Pos.X < 0 then Pos.X := 0;
  if Pos.Y < 0 then Pos.Y := 0;
  if Delta.X < 0 then Delta.X := 0;
  if Delta.X > 254 then Delta.X := 254;
  if Delta.Y >= FileLines^.Count then Delta.Y := FileLines^.Count - 1;
  if Delta.Y < 0 then Delta.Y := 0;
  if Pos.X > Delta.X then Pos.X := Delta.X;
  if Delta.X - Pos.X >= Size.X then Pos.X := Delta.X - Size.X + 8;
  if Pos.Y > Delta.Y then Pos.Y := Delta.Y;
  if Delta.Y - Pos.Y >= Size.Y then Pos.Y := Delta.Y - Size.Y + 1;
  if (BB.X <> Pos.X) or (BB.Y <> Pos.Y) then ChPosition := Off;
  if Marking then
     begin
       ChPosition := Off;
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
  for I := 0 to Size.Y - 1 do
  begin
    A := Pos.Y + I;
    if BV and not VertBlock then
     begin
        if (A = Mark.A.Y) then
          begin
            if WorkModified and (Mark.A.Y = Delta.Y) then
             if Mark.A.X > Byte(WorkString[0]) then Mark.A.X := Byte(WorkString[0])
               else else
             if Mark.A.X > Length(GetLine(Mark.A.Y)) then Mark.A.X := Length(GetLine(Mark.A.Y));
          end;
        if (A = Mark.B.Y) then
         begin
           if WorkModified and (Mark.B.Y = Delta.Y) then
            if Mark.B.X > Byte(WorkString[0]) then Mark.B.X := Byte(WorkString[0])
            else else
            if Mark.B.X > Length(GetLine(Mark.B.Y)) then Mark.B.X := Length(GetLine(Mark.B.Y));
         end;
     end;
    if ChPosition and not ((A = Delta.Y) or (A = LastPos.Y)) then Continue;
    if EdOpt.HiliteLine or ((Length(WorkString) >= 254) and RecombineLongLines) then {JO}
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
        Highlites ( Length(S), @S[1], HiLitePar ); {PZ 2000.04.28}
        C1                  := C and $F0;                  { Background }
        HP[0]               := #6;                         { Attr. size }
        if (A = Delta.Y) and (EdOpt.HiliteLine)
         then HP[Ord(hhComment)]  := Chr(CC[6])            { Comments for current line}
         else HP[Ord(hhComment)]  := Chr(Comments);        { Comments   }
        HP[Ord(hhNumber)]   := Chr(C1 or (CC[10] and 15)); { Numbers    }
        HP[Ord(hhString)]   := Chr(C1 or (CC[ 9] and 15)); { Strings    }
        HP[Ord(hhSymbol)]   := Chr(C1 or (CC[ 8] and 15)); { Symbols    }
        HP[Ord(hhKeyword1)] := Chr(C1 or (CC[11] and 15)); { Keywords 1 }
        HP[Ord(hhKeyword2)] := Chr(C1 or (CC[12] and 15)); { Keywords 2 }
        DoHighlite ( B, S, HP );
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
    if SearchActive and (A = Delta.Y) then
     begin
      X2 := Length(SearchData.Line);
      X1 := Delta.X - Pos.X;
      if SearchData.Dir = 0 then Dec(X1, X2);
      if X1 < 0 then begin Inc(X2, X1); X1 := 0; end;
      MoveColor(B[X1], X2, BC);
      SearchOnDisplay := On;
     end;
    if EdOpt.HiliteColumn then WordRec(B[Delta.X - Pos.X]).Hi := CC[7];
    WriteLine(0, I, Size.X, 1, B);
  end;
 SetCursor(Delta.X - Pos.X, Delta.Y - Pos.Y);
 if InsertMode xor (InterfaceData.Options and ouiBlockInsertCursor <> 0)
    then NormalCursor else BlockCursor;
 ShowCursor;
 LastPos.X := Delta.X;
 LastPos.Y := Delta.Y;
 ChPosition := Off;
 if (InfoL<>nil) then InfoL^.Draw;
 if (BMrk<>nil)  then BMrk^.Draw;
end;

function TFileEditor.GetLine(Index : LongInt) : String;
 var p: PString;
     s: String;
begin
 if (Index < FileLines^.Count) and (Index >= 0) then p := FileLines^.At(Index)
                                                else p := Nil;
 if p <> Nil then s := p^ else s := ''; {-$VIV* START}
 s := KeyMapConvertStr(s, true);
 GetLine := s;                          {-$VIV E-ND}
end;

procedure TFileEditor.ModifyLine(Index : LongInt; S : String; DelSpaces: Boolean);
begin
 if (Index < 0) or (Index >= FileLines^.Count) then Exit;
 if DelSpaces then DelRight(S);
 KeyMapAtReplace(Index, NewStr(S)); {-$VIV}
end;

function TFileEditor.HandleCommand;
begin
end;

procedure TFileEditor.HandleEvent;
 label 1,2;
 var ChPos,WM,DelWord,WasMA,WasMB: Boolean;
     P: PString;
     PL: PCollection;
     S: String;
     LastY,LastX,i: LongInt;
     WL: Byte absolute WorkString;
     OldDelta,
     T: TPoint;
     L: longint;

 procedure BlockOff;
 begin
  UnMark := On; Marking := Off; ChPosition := Off;
  if EditorDefaults.GlobalOpt and ebfPbl <> 0 then Exit;
  Mark.B := Mark.A; BlockVisible := Off;
 end;

 procedure InsertSpace;
  var P: TPoint;
 begin
  P.X := LastX; P.Y := LastY;
  if InsertMode then
    begin
     if LastX < WL then Insert(' ', WorkString, LastX + 1)
                   else WorkString := WorkString + Strg(' ', LastX - WL + 1);
     StoreUndoInfo(udInsChar, P, P);
    end
     else
      begin
       if LastX< WL then
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
  WorkModified := On; Modified := On; WasDelete := Off;
 end;

 procedure ChangeLine;
  label Ex;
 begin
   if not WorkModified then goto Ex;
   While WorkString[WL] = ' ' do Dec(WL);
   if WorkModified then ModifyLine(LastLine, WorkString,On);
   Modified := Modified or WorkModified;
   WorkModified := Off;
 Ex:
   WorkString := GetLine(Delta.Y);
 end;

 procedure MakeEnter;
  var I, J: LongInt;
      OldS,s1,s2: String;
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
        Modified := On;
        WorkModified := Off;
        OldS := WorkString;
        WAY := Bl and (Delta.Y = Mark.A.Y) and (Delta.X <= Mark.A.X);
        WBY := Bl and (Delta.Y = Mark.B.Y) and (Delta.X <= Mark.B.X);
        StoreUndoInfo(udInsLine, Delta, WorkString);
        While WorkString[WL] = ' ' do Dec(WL);
        S1 := WorkString; S2 := Copy(S1, 1, LastX);
        While S2[Byte(S2[0])] = ' ' do Dec(S2[0]);
        WorkString := S2; Delete(S1, 1, LastX);
        ModifyLine(LastY,S2,On);
        WorkString := S1;
        if EdOpt.AutoIndent then
         begin
          I := LastY - 1;
          While (WorkString[1] = ' ') and (WorkString <> '') do
            begin
              DelFC(WorkString);
              if WAY then Dec(Mark.A.X);
              if WBY then Dec(Mark.B.X);
            end;
          if DelSpaces(S2) = '' then S2 := OldS;
          {While (S2 = '') and (I >= 0) do
          if I >= 0 then
           begin S2 := GetLine(I); Dec(I) end;}
          LastX := 0;
          While (S2[LastX + 1] = ' ') and (LastX < Byte(S2[0]))
           do begin
                 Inc(LastX);
                 WorkString := ' ' + WorkString;
                 if WAY then Inc(Mark.A.X);
                 if WBY then Inc(Mark.B.X);
              end;
         end else LastX := 0;
        KeyMapAtInsert(LastY + 1, NewStr(WorkString)); {-$VIV}
        SetLimits; Delta.X := LastX; Inc(Delta.Y);
       end;
  for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Inc(MarkPos[L].Y);
 end;

 procedure MakeBack;
  var I, J : Byte;
      P: TPoint;
      L: Longint;
 begin
  WorkModify; BlockOff; Modified := On;
  ClearEvent(Event);
  if LastX = 0 then
   begin
    if (LastY = 0) or not InsertMode then Exit;
    EnableMarking := Off;
    FileLines^.AtFree(LastY);
    S := GetLine(LastY - 1);
    P.X := Length(S); P.Y := Delta.Y - 1;
    StoreUndoInfo(udSubDelLine, P, WorkString);
    WorkString := S + WorkString; ModifyLine(LastY - 1, WorkString, On);
    WorkModified := Off; WorkModify; WorkModified := Off;
    Delta.X := Byte(S[0]);
    ScrollTo(Delta.X, Delta.Y - 1);
    SetLimits;
    ChangeLine;
    DrawView;
    EnableMarking := On;
    for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Dec(MarkPos[L].Y);
    Exit;
   end;
  EnableMarking := Off;
  if WL < LastX then WorkString := AddSpace(WorkString, LastX + 1);
  if EdOpt.BackIndent and ((WorkString[LastX + 1] <> ' ') or (DelSpaces(WorkString) = ''))
   and (Copy(WorkString, 1, LastX) = Strg(' ', LastX))
   and (LastY > 0) then
    begin
     for i := LastY-1 downto 0 do
      begin
       s := GetLine(i);
       J := 0; while (S[J+1] = ' ') and (J < Byte(S[0])) do Inc(J);
       if J < LastX then
       if Copy(s, 1, LastX) > Strg(' ', LastX) then
         begin
          T.X := LastX; T.Y := LastY;
          StoreUndoInfo(udSubDel, T, WorkString);
          Delete(WorkString, 1, LastX); LastX := J;
          Insert(Strg(' ', J), WorkString, 1);
          ScrollTo(LastX, Delta.Y);
          ChangeLine;
          DrawView;
          EnableMarking := On;
          Exit;
         end;
      end;
    end;
  T.X := LastX; T.Y := LastY;
  if LastX <= WL then
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
  EnableMarking := On;
 end;

 procedure BlockDelete(ChangePos: Boolean);
  var I, J: LongInt;
      S, S1, S2: String;
      L, K: Longint;
 begin
  Modified := On;
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
     if not InsertMode then Insert(Strg(' ', Mark.B.X - Mark.A.X), S, Mark.A.X + 1);
     ModifyLine(I, S, On);
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
                         Copy(GetLine(Mark.B.Y),Mark.B.X+1,255), On);
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
  EnableMarking := Off; Marking := Off;
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
  BlockDelete({not ChangePos} On);
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
  EnableMarking := On;
 end;

 procedure BMarking;
 begin
  if not EnableMarking or (DrawMode>0) then Exit;
  if (ShiftState and 3 <> 0) and not MouseMark then
  begin
   if not Marking then begin Sel.A := LastPos; Sel.B := Delta end
                  else Sel.B := Delta;
   Marking := On;
   BlockVisible := On;
  end else begin Marking := MouseMark; if Marking then Sel.B := Delta; end;
  if not Marking and UnMark then BlockOff;
 end;

 procedure MakeDel;
  var P: TPoint;
      S: String;
      L: Longint;
 begin
  WorkModify;
  ChangeLine;
  BlockOff;
  Modified := On;
  if LastX < WL then
   begin WorkModify; P.X := LastX; P.Y := LastY;
         StoreUndoInfo(udDelChar, P, WorkString[LastX+1]);
         Delete(WorkString, LastX + 1, 1); Exit end;
  if LastY + 1 >= FileLines^.Count then Exit;
  WorkModify;
  P.X := LastX; P.Y := LastY;
  S := GetLine(LastY + 1);
  if LastY < FileLines^.Count then StoreUndoInfo(udSubDelLine, P, S);
  WorkString := WorkString + Strg(' ', LastX - WL) + S;
  ChangeLine;
  FileLines^.AtFree(LastY + 1);
  for L:=1 to 9 do if MarkPos[L].Y >= LastY then Dec(MarkPos[L].Y);
  SetLimits;
 end;

 procedure PasteBlock;
 begin
  if SystemData.Options and ossUseSysClip <> 0 then SyncClipOut(On);
  EnableMarking := Off; Marking := Off;
  ChangeLine;
  if EditorDefaults.GlobalOpt and (ebfPbl+ebfObl)=ebfObl
    then DeleteBlock(On);
  BlockOff;
  InsertBlock(Clipboard, On);
  ChangeLine;
  {$IFNDEF NONBP}
  memw[seg0040:$1C] := memw[seg0040:$1A];
  {$ENDIF}
  EnableMarking := On;
 end;

 procedure CopyBlock;
  var R: TRect;
 begin
  ChangeLine;
  if (Clipboard <> nil) then Dispose(Clipboard,Done);
  Clipboard := GetSelection;
  if SystemData.Options and ossUseSysClip <> 0 then SyncClipIn;
  if ClipBoardStream<>nil then
   ClipboardStream^.Seek(Max(ClipboardStream^.GetPos-1, 0));
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
  Modified := On;
  T.Y := LastY; T.X := LastX;
  if Delta.Y + 1 = FileLines^.Count then
   begin
    StoreUndoInfo(udSubDel, T, WorkString);
    ModifyLine(Delta.Y, '', On); WL := 0;
   end else
   begin
    StoreUndoInfo(udDelLine, T, WorkString);
    WorkModified := Off; FileLines^.AtFree(Delta.Y); ChangeLine
   end;
  for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Dec(MarkPos[L].Y);
  SetLimits; ScrollTo(0, Delta.Y); DrawView; ChangeLine;
 end;

 procedure WordLeft;
 begin
  if LastX > 0 then
   begin
    if LastX > WL then LastX := WL;
    while (LastX >= 0) and (WorkString[LastX] in BreakChars) do Dec(LastX);
    while (LastX > 0) and not (WorkString[LastX] in BreakChars) do Dec(LastX);
    if (LastX >= 0) and not (WorkString[LastX+1] in BreakChars) then Exit;
   end;
  if LastY <= 0 then begin LastX := 0; Exit end;
  Dec(Delta.Y); Dec(LastY); ChangeLine; LastX := WL; Delta.X := WL; WordLeft;
 end;

 procedure WordRight;
  var B: Boolean;
 begin
  B := LastX < WL;
  while (LastX < WL) and not (WorkString[LastX+1] in BreakChars) do Inc(LastX);
  while (LastX < WL) and (WorkString[LastX+1] in BreakChars) do Inc(LastX);
  if (LastX < WL) or B and (LastX = WL) then Exit;
  if LastY + 1 >= FileLines^.Count then begin LastX := WL; Exit end;
  Inc(Delta.Y); Inc(LastY); LastX := 0; Delta.X := 0; ChangeLine;
  if (WL > 0) and not (WorkString[1] in BreakChars) then Exit;
  WordRight;
 end;

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
  Modified := On;
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
     if (LastY > 0) and (LastX < Byte(S[0])) then
     begin
      WorkModify;
      repeat InsertSpace until (s[LastX + 1] = ' ') or (LastX >= Byte(S[0]));
      {WM := (s[LastX + 2] = ' ') and (LastX < Byte(s[0]));}
      While (s[LastX + 2] = ' ') and (LastX < Byte(s[0])) do InsertSpace;
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
  P := MIReadBlock(@Self, S, Off);
  if not IsValid then begin isValid := On; Exit; end;
  VertBlock := Off;
  if P <> nil then begin InsertBlock(P, On); Dispose(P,Done) end;
 end;
        {-DataCompBoy-}

        {-DataCompBoy-}
 procedure BlockWrite;
  var P: PCollection;
      PS: PStdCollector;
      S, SST: String;
      R: PStream;
      I,J,K: LongInt;
      CRLF: String[2];
      VB: Boolean;
      PI: PView;
      A: Word;

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
   cfLfCr: crlf := #10#13;
   else
    begin
     EdOpt.ForcedCrLf:=cfNone;
     for qwe:=0 to EditorDefaults.NewLine do
      EdOpt.ForcedCrLf:=succ(EdOpt.ForcedCrLf);
     if EditorDefaults.NewLine = 1 then CrLf := #13 else
      if EditorDefaults.NewLine = 2 then CrLf := #10 else
       if EditorDefaults.NewLine = 3 then CrLf := #10#13 else
        crlf := #13#10;
    end;
  end;

  PI := WriteMsg(^M^M^C+GetString(dlWritingFile));

  for I := Mark.A.Y to Mark.B.Y do
    begin
      if VB then SST := Copy(GetLine(I), Mark.A.X+1, Mark.B.X-Mark.A.X)
            else if I = Mark.A.Y then SST := Copy(GetLine(I), Mark.A.X+1, 255)
                   else if I = Mark.B.Y then SST := Copy(GetLine(I), 1, Mark.B.X)
                     else SST := GetLine(I);
      if OptimalFill then CompressString;
      R^.Write(SST[1], Length(SST));
      if (I <> Mark.B.Y) and ((Length(SST) < 254) or not RecombineLongLines) then R^.Write(CrLf[1], Length(CrLf)); {JO}
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
  var S: String;
      I,OldX: LongInt;
      P: PLineCollection;
      TP: TPoint;
 begin
  ChangeLine;
  S := '';
  While WorkString[WL] = ' ' do
   begin Dec(WL); if WL < LastX + 1 then LastX := WL - 1; end;
  OldX := 0;
  if WL - 1 = LastX then OldX := 1
     else if WL - 1 < LastX then OldX := 0
       else OldX := 1;
  if WL > EdOpt.RightSide then
   begin
    New(P, Init(1,1)); P^.Insert(NewStr(WorkString));
    TP.Y := Delta.Y; TP.X := 0;
    StoreUndoInfo(udDelBlock, TP, P);
    while (WL > 1) and (WL > EdOpt.RightSide) do
     begin S := WorkString[WL] + S; Dec(WL); end;
    while (WL > 1) and not (WorkString[WL] in [' ',',',':','.','?','!','+',';']) do
     begin S := WorkString[WL] + S; Dec(WL); end;
    if DelSpaces(WorkString) = '' then begin WorkString := WorkString + S; S := ''; end else
    OldX := Delta.X - WL + OldX;
    if Delta.X > WL then Delta.X := 255;
    if System.Pos(DelSpaces(WorkString), WorkString) = 0 then
    begin
     While WorkString[WL] = ' ' do Dec(WL);
     I := 1;
     if EdOpt.AutoJustify then
     while WL < EdOpt.RightSide do
      begin
       while (WorkString[I] = ' ') do Inc(I);
       while (WorkString[I] <> ' ') and (I < WL) do Inc(I);
       if (I < WL) then
        begin
         Insert(' ', WorkString, I);
         if I <= Delta.X then Inc(Delta.X);
        end;
        Inc(I); if I > WL then I := 1;
      end;
     end;
    DelLeft(S);
    S := Strg(' ', EdOpt.LeftSide)+S;
   end;
   ModifyLine(LastY, WorkString, On);
   KeyMapAtInsert(LastY+1, NewStr(S)); {-$VIV}
   S := #2#0;
   StoreUndoInfo(udFormatBlock, TP, S);
   if Delta.X >= WL then
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
   if EdOpt.AutoBrackets and ((LastX >= WL) or (WorkString[LastX+1] = ' ')) then
    begin
        for i:=1 to Length(AutoBracketPairs) shr 1
        do if Ch=AutoBracketPairs[i*2-1] then begin
            S1:=Copy(AutoBracketPairs,i*2-1,2);
            break
        end;
    end;

   EnableMarking := Off;
   if EditorDefaults.GlobalOpt and (ebfPbl+ebfObl)=ebfObl
     then DeleteBlock(On);
   LastY := Delta.Y;
   LastX := Delta.X;
   BlockOff;
   EnableMarking := Off;
   T := Delta; Inc(T.X);
   if LastX < 255 then
    begin
     WorkModify;
     if InsertMode then
      begin
       if LastX <= WL then Insert(S1, WorkString, LastX + 1)
                      else WorkString := WorkString + Strg(' ', LastX - WL) + S1;
       StoreUndoInfo(udInsChar, Delta, Delta);
       if S1[0] = #2 then
         StoreUndoInfo(udInsChar, T, T);
      end else
      begin
       if LastX >= WL then WorkString := WorkString + Strg(' ', LastX - WL + 1);
       StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX+1]);
       WorkString[LastX+1] := Ch
      end;
     if (not EdOpt.AutoWrap or (LastX < EdOpt.RightSide)) and (LastX < 254) then HScroll^.SetValue(LastX + 1)
      else if LastX < 254 then SplitString else
             begin
              ChangeLine;
              FileLines^.AtInsert(LastY+1, nil); SetLimits;
              ScrollTo(0, LastY+1); DrawView;
             end;
    end;
  CE;
  EnableMarking := On;
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
  EnableMarking := Off; Marking := Off;
  ChangeLine;
  if EditorDefaults.GlobalOpt and (ebfPbl+ebfObl)=ebfObl
    then DeleteBlock(On);
  BlockOff;

  GetWinClip(PLineCollection(Clipboard), On);
  InsertBlock(Clipboard, On);

  ChangeLine;
  {$IFNDEF NONBP}
  memw[seg0040:$1C] := memw[seg0040:$1A];
  {$ENDIF}
  EnableMarking := On;
 end;

 procedure DrawLine(Dir: Byte);
   const
      Line00 = '³ÄÀ³³ÚÃÄÙÄÁ¿´ÂÅ';
      Line11 = 'ºÍÈººÉÌÍ¼ÍÊ»¹ËÎ';
      Line01 = '³ÍÔ³³ÕÆÍ¾ÍÏ¸µÑØ';
      Line10 = 'ºÄÓººÖÇÄ½ÄÐ·¶Ò×';

      UpContact1 =   '³ÃÅ´ÚÂ¿ÆØµÕÑ¸';
      UpContact2 =   'ºÌÎ¹ÉË»Ç×¶ÖÒ·';
      DownContact1 = '³ÃÅ´ÀÁÙÆØµÔÏ¾';
      DownContact2 = 'ºÌÎ¹ÈÊ¼Ç×¶ÓÐ½';
      LeftContact1 = 'ÄÃÅÚÀÂÁÇ×ÖÓÒÐ';
      LeftContact2 = 'ÍÌÎÉÈËÊÆØÕÔÑÏ';
      RightContact1 = 'Ä´Å¿ÙÂÁ¶×·½ÒÐ';
      RightContact2 = 'Í¹Î»¼ËÊµØ¸¾ÑÏ';

   var  _Up, _In, _Down: String;
        L_Up, L_In, L_Down: Char;
        VL: String[32];
        A,B: Word;
        I,J,K: LongInt;
        ResChar, C: Char;
        DMode: Byte;

   function GetH0: Byte; begin GetH0 := 2*Byte(PosChar(_In[LastX+2], RightContact1)>0)+
                                        8*Byte(PosChar(_In[LastX], LeftContact1)>0) end;

   function GetH1: Byte; begin GetH1 := 2*Byte(PosChar(_In[LastX+2], RightContact2)>0)+
                                        8*Byte(PosChar(_In[LastX], LeftContact2)>0) end;

   function GetV0: Byte; begin GetV0 :=   Byte(PosChar(_Up[LastX+1], UpContact1)>0)+
                                        4*Byte(PosChar(_Down[LastX+1], DownContact1)>0) end;

   function GetV1: Byte; begin GetV1 :=   Byte(PosChar(_Up[LastX+1], UpContact2)>0)+
                                        4*Byte(PosChar(_Down[LastX+1], DownContact2)>0) end;

   function Modify(C: Char; Mask: Byte): Boolean;
     var I: Integer;
   begin
     Modify := Off;
     VL := Line00; I := PosChar(C, VL);
     if I = 0 then begin VL := Line01; I := PosChar(C, VL); end;
     if I = 0 then begin VL := Line10; I := PosChar(C, VL); end;
     if I = 0 then begin VL := Line11; I := PosChar(C, VL); end;
     if not (I in [7,11,13,14,15]) then Exit;
     Mask := I and not Mask; if Mask = I then Exit;
     if Mask > 0 then ResChar := VL[Mask] else ResChar := ' ';
     Modify := On;
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
   _In := AddSpace(WorkString, LastX+2);         L_In := _In[0]; _In[0] := ' ';
   _Up := AddSpace(GetLine(LastY-1), LastX+2);   L_Up := _Up[0]; _Up[0] := ' ';
   _Down := AddSpace(GetLine(LastY+1), LastX+2); L_Down := _Down[0]; _Down[0] := ' ';
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
         if LastX >= WL then WorkString := AddSpace(WorkString, LastX + 1);
         StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX+1]);
         WorkString[LastX+1] := C;
       end;
    2: begin
         if LastDir < 0 then LastDir := (Dir + 2) mod 4;
         A := A or B or (1 shl LastDir) or (1 shl Dir);
         WorkModify;
         if LastX >= WL then WorkString := AddSpace(WorkString, LastX + 1);
         if Modify(_Up[LastX+1], 4) then
           begin
             Dec(Delta.Y); _Up[0] := L_Up;
             StoreUndoInfo(udReplaceChar, Delta, _Up[LastX+1]);
             _Up[LastX+1] := ResChar; ModifyLine(Delta.Y, _Up, On);
             Inc(Delta.Y);
           end;
         if Modify(_Down[LastX+1], 1) then
           begin
             Inc(Delta.Y); _Down[0] := L_Down;
             StoreUndoInfo(udReplaceChar, Delta, _Down[LastX+1]);
             _Down[LastX+1] := ResChar; ModifyLine(Delta.Y, _Down, On);
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
         if (WL > LastX+1) and (Modify(WorkString[LastX+2], 8)) then
           begin
             Inc(Delta.X);
             StoreUndoInfo(udReplaceChar, Delta, WorkString[LastX+2]);
             WorkString[LastX+2] := ResChar;
             Dec(Delta.X);
           end;
       end;
   end;
   EnableMarking := Off;
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
   EnableMarking := On;

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
   Res:='';
   S:='';
   R.Assign(0, 0, 20, 7);
   New(Info, Init(R));
   Info^.Write(1, Copy(GetString( dlPleaseStandBy ), 4, 255));
   Desktop^.Insert(Info); (* X-Man *)
   Abort:=False;

   P:=Delta.X+1;

   Min1:=1;
   For i:=P downto 1 do
    if WorkString[i] in IllegalCharSet+['\','/','*','?'] (* X-Man *)
     then begin Min1:=i+1; break end;
   if (Min1>2) and (WorkString[Min1-1]=':')
   and (UpCase(WorkString[Min1-2])>='A')
   and (UpCase(WorkString[Min1-2])>='Z') then Dec(Min1,2); (* X-Man *)
   DispatchEvents(Info, Abort);
   if Abort then goto ex;

   Min2:=1;
   For i:=P downto 1 do
    if WorkString[i] in IllegalCharSet-['\','/']+['*','?'] (* X-Man *)
     then begin Min2:=i+1; break end;
   DispatchEvents(Info, Abort);
   if Abort then goto ex;

   Min3:=1;
   For i:=P downto 1 do
    if WorkString[i] in IllegalCharSetDos+[#32,'\','/','*','?'] (* X-Man *)
     then begin Min3:=i+1; break end;
   if (Min3>2) and (WorkString[Min3-1]=':')
   and (UpCase(WorkString[Min3-2])>='A')
   and (UpCase(WorkString[Min3-2])>='Z') then Dec(Min3,2); (* X-Man *)
   DispatchEvents(Info, Abort);
   if Abort then goto ex;

   Min4:=1;
   For i:=P downto 1 do
    if WorkString[i] in IllegalCharSetDos-['\','/']+[#32,'*','?'] (* X-Man *)
     then begin Min4:=i+1; break end;
   DispatchEvents(Info, Abort);
   if Abort then goto ex;

   Max1:=length(WorkString)+1;
   For i:=P to length(WorkString) do
    if WorkString[i] in IllegalCharSet+['\','/','*','?'] (* X-Man *)
     then begin Max1:=i; break end;
   DispatchEvents(Info, Abort);
   if Abort then goto ex;

   Max2:=length(WorkString)+1;
   For i:=P to length(WorkString) do
    if WorkString[i] in IllegalCharSet-['\','/']+['*','?'] (* X-Man *)
     then begin Max2:=i; break end;
   DispatchEvents(Info, Abort);
   if Abort then goto ex;

   Max3:=length(WorkString)+1;
   For i:=P to length(WorkString) do
    if WorkString[i] in IllegalCharSetDos+[#32,'\','/','*','?'] (* X-Man *)
     then begin Max3:=i; break end;
   DispatchEvents(Info, Abort);
   if Abort then goto ex;

   Max4:=length(WorkString)+1;
   For i:=P to length(WorkString) do
    if WorkString[i] in IllegalCharSetDos-['\','/']+[#32,'*','?'] (* X-Man *)
     then begin Max4:=i; break end;
   DispatchEvents(Info, Abort);
   if Abort then goto ex;

   Res:='';       S:=Copy(WorkString, Min1, Max1-Min1); Chk; if Abort then goto ex;
   if Res='' then S:=Copy(WorkString, Min1, Max2-Min1); Chk; if Abort then goto ex;
   if Res='' then S:=Copy(WorkString, Min2, Max1-Min2); Chk; if Abort then goto ex;
   if Res='' then S:=Copy(WorkString, Min2, Max2-Min2); Chk; if Abort then goto ex;
   if Res='' then S:=Copy(WorkString, Min3, Max3-Min3); Chk; if Abort then goto ex;
   if Res='' then S:=Copy(WorkString, Min3, Max4-Min3); Chk; if Abort then goto ex;
   if Res='' then S:=Copy(WorkString, Min4, Max3-Min4); Chk; if Abort then goto ex;
   if Res='' then S:=Copy(WorkString, Min4, Max4-Min4); Chk; if Abort then goto ex;

   S:=Copy(WorkString, Min4, Max4-Min4); DelLeft(S); DelRight(S);
   If (PosChar('.',S)=0) Then S:=S+GetExt(EditName);

   if Res = '' then begin
    Info^.Hide;
    Res:=Cut(S, 20);
    Q:=@Res;
    case ExecResource(dlgSrchFailed, Q) of
          cmYes: PDNAppl(Application)^.EditFile(On, SourceDir + 'DN.SPF');
          cmNo: PDNAppl(Application)^.EditFile(On, S);
         end
   end else PDNAppl(Application)^.EditFile(On, Res);

ex:
   Info^.Free;
   Abort:=false;
 end;
        {-DataCompBoy-}

 procedure ChangeCharCaseDirect; {-$VIV 21.05.99--}
 var Ch: Char;
     S, S2: String;
 begin
   S := WorkString;
   if (Delta.X >= 0) and (Delta.X < Length(S)) then
   begin
     WorkModify;
     StoreUndoInfo(udStrModified, Delta, WorkString);
     Ch := S[Delta.X+1];
     if Ch = UpStrg(Ch)
      then S2 := LowStrg(Ch)
      else S2 := UpStrg(Ch);
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
 var S: String;
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
      Event.CharCode := charASCII;
      Event.ScanCode := 0;
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

 var PC: PCollection;
     R: TRect;

begin
 if (Event.What = evCommand) and (Event.Command=cmHelp2) then begin
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
 DelWord := Off;
1:
 LastY := Delta.Y;
 LastX := Delta.X;
 i := FileLines^.Count;
 ChPos:=Off;
 case Event.What of
  evCommand: case Event.Command of
              cmMainMenu: begin
                            Message(DNApp.MenuBar, evCommand, cmMenu, nil);
                            CE;
                          end;
              cmDuplicateLine: begin
                                 ChangeLine;
                                 WorkModify;
                                 KeyMapAtInsert(LastY+1, NewStr(GetLine(LastY))); {-$VIV}
                                 StoreUndoInfo(udDupeLine, Delta, S);
                                 SetLimits;
                                 for L:=1 to 9 do if MarkPos[L].Y >= Delta.Y then Inc(MarkPos[L].Y);
                                 CED;
                               end;
              cmClose: begin
                         if (SmartPad or ClipBrd) then
                           begin
                             ChangeLine;
                             if Modified then MISaveFile(@Self);
                             if not Owner^.GetState(sfModal) then Exit;
                             CE; Owner^.Redraw;
                             PEditWindow(Owner)^.ModalEnd := On;
                             ClearEvent(Event);
                           end;
                       end;
              cmUpString, cmLowString, cmCapString,
              cmUpWord, cmLowWord, cmCapWord: begin
                 if DelSpaces (WorkString) <> '' then
                  begin
                     WorkModify;
                     While WorkString[WL] = ' ' do Dec(WL);
                     StoreUndoInfo(udStrModified, Delta, WorkString);
                     if (ShiftState and 3 <> 0)
                       or (Event.Command = cmUpString)
                       or (Event.Command = cmLowString)
                       or (Event.Command = cmCapString) then
                     case Event.Command of
                        cmUpWord,  cmUpString:  UpStr(WorkString);
                        cmLowWord, cmLowString: LowStr(WorkString);
                        cmCapWord, cmCapString: CapStr(WorkString);
                     end else
                     begin
                       While (LastX > 0) and not (WorkString[LastX] in BreakChars) do Dec(LastX);
                       S := '';
                       While (WL > LastX) and not (WorkString[LastX+1] in BreakChars) do
                          begin S := S + WorkString[LastX+1]; Delete(WorkString, LastX+1, 1); end;
                       if S <> '' then
                       case Event.Command of
                         cmUpWord:  UpStr(S);
                         cmLowWord: LowStr(S);
                         cmCapWord: CapStr(S);
                       end;
                       System.Insert(S, WorkString, LastX+1);
                     end;
                     ChangeLine; {-$VIV}
                  end;
                  CED
                end;
              cmMarkWord: begin
                            ChangeLine;
                            if (LastX < WL) and not (WorkString[LastX+1] in BreakChars) and
                              ((LastX=0) or (WorkString[LastX] in BreakChars)) then else
                             Message(@Self, evCommand, cmWordLeft, nil);
                            if (LastY <> Delta.Y) or (Delta.X < WL) and (WorkString[Delta.X+1] in BreakChars)
                              or (Delta.X >= WL) then
                              begin
                                Message(@Self, evCommand, cmWordRight, nil);
                              end;
                            Mark.A := Delta; Mark.B := Delta;
                            ChangeLine;
                            While (Mark.B.X < WL) and not (WorkString[Mark.B.X+1] in BreakChars) do Inc(Mark.B.X);
                            R := Mark;
                            ScrollTo(LastX, LastY);
                            BlockVisible := On;
                            Mark := R;
                            CED;
                          end;
              cmMarkLine: begin
                            Mark.A.X := 0; Mark.A.Y := Delta.Y;
                            if Delta.Y >= FileLines^.Count-1 then
                              begin Mark.B.Y := Delta.Y; Mark.B.X := WL end
                              else begin Mark.B.Y := Delta.Y+1; Mark.B.X := 0 end;
                            BlockVisible := On; CED
                           end;
              cmMoveBlockStart: begin ScrollTo(Mark.A.X, Mark.A.Y); CE end;
              cmMoveBlockEnd: begin ScrollTo(Mark.B.X, Mark.B.Y); CE end;
              cmSwitchHiLine: begin EdOpt.HiliteLine := not EdOpt.HiliteLine; CED end;
              cmSwitchHiColumn: begin EdOpt.HiliteColumn := not EdOpt.HiliteColumn; CED end;
              cmSwitchWrap: begin EdOpt.AutoJustify := not EdOpt.AutoJustify; CE end;
              cmSwitchFill: begin OptimalFill := not OptimalFill; CE end;
              cmSwitchTabReplace: begin TabReplace := not TabReplace; CE end;
              cmSwitchBack: begin EdOpt.BackIndent := not EdOpt.BackIndent; CE end;
              cmIndentOn,cmIndentOff: begin EdOpt.AutoIndent := Event.Command = cmIndentOn; CE end;
              cmSwitchIndent: begin EdOpt.AutoIndent := not EdOpt.AutoIndent; CE end;
              cmSwitchSmartTab: begin EdOpt.SmartTab := not EdOpt.SmartTab; CE end;
              cmSwitchHighLight: begin
                                  EdOpt.HiLite:=not EdOpt.HiLite;
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
              cmEditLfCrMode:    begin
                                  EdOpt.ForcedCrLf:=cfLFCR;
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
                                 WL := LastX; CED; end;
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
                                 InsertBlock(PC, On);
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
                                  DeleteBlock(Off);
                                  Message(@Self, evCommand, cmPaste, nil);
                               end else
                               begin
                                 ChangeLine;
                                 DeleteBlock(Off);
                                 InsertBlock(PC, On);
                                 Dispose(PC,Done);
                               end;
                               ChangeLine;
                           end;
              cmReverseSearch: begin
                                 ChangeLine;
                                 SearchData.Dir := SearchData.Dir xor 1;
                                 if SearchOnDisplay then Search;
                                 Search;
                                 SearchData.Dir := SearchData.Dir xor 1;
                                 ChangeLine; CE;
                                 SearchOnDisplay := Off;
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
              cmContSearch: begin ChangeLine; Search; ChangeLine; CE end;
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
                           if (EditorDefaults.GlobalOpt and (ebfPbl+ebfObl)=ebfObl)
                             and ValidBlock then DeleteBlock(On) else MakeDel;
                           CED
                         end;
              cmEnter: begin
                        BlockOff;
                        MakeEnter;
                        ScrollTo(Delta.X, Delta.Y);
                        CED
                       end;
              cmEnd: begin UnMark := On; while WorkString[WL] = ' ' do Dec(WL);
                           ScrollTo(WL, Delta.Y); CED end;
              cmInsLine: begin
                          BlockOff;
                          PP := Delta; MakeEnter; Delta := PP;
                          ChangeLine; ScrollTo(PP.X, PP.Y); CED end;
              cmTab: begin BlockOff; DoTab; CE end;
              cmWordRight: begin UnMark := On; WordRight; ScrollTo(LastX, LastY); CED end;
              cmWordLeft: begin UnMark := On; WordLeft; ScrollTo(LastX, LastY); CED end;
              cmDeleteLine: begin BlockOff; DeleteLine; CE end;
              cmSwitchBlock: begin UnMark := On; VertBlock := not VertBlock; DrawView; CE end;
              cmClear: begin UnMark := On; if BlockVisible and ValidBlock then DeleteBlock(On); CE end;
              cmCopy: begin UnMark := On; if BlockVisible and ValidBlock then CopyBlock; CE end;
              cmCut: begin UnMark := On; if BlockVisible and ValidBlock then
                            begin CopyBlock; DeleteBlock(On) end; CE end;
              cmPaste: begin UnMark := On; PasteBlock; CE end;
              cmMoveUp: begin UnMark := On; Event.What := evKeyDown; Event.KeyCode := kbUp end;
              cmPgUp: begin UnMark := On; Dec(Pos.Y, Size.Y); VScroll^.SetValue(Delta.Y-Size.Y); DrawView; CE end;
              cmPgDn: begin UnMark := On; Inc(Pos.Y, Size.Y); VScroll^.SetValue(Delta.Y+Size.Y); DrawView; CE end;
              cmMoveDown: begin UnMark := On; Event.What := evKeyDown; Event.KeyCode := kbDown end;
              cmMoveLeft: begin UnMark := On; Event.What := evKeyDown; Event.KeyCode := kbLeft end;
              cmMoveRight: begin UnMark := On; Event.What := evKeyDown; Event.KeyCode := kbRight end;
              cmHideBlock: begin BlockVisible := not BlockVisible; UnMark := Off; CED end;
              cmScrollUp: if Pos.Y > 0 then
                          begin
                           UnMark := On;
                           if not (Delta.Y - Pos.Y + 1 < Size.Y) then Dec(Delta.Y);
                           Dec(Pos.Y);
                           ScrollTo(Delta.X, Delta.Y);
                           Delta.Y := VScroll^.Value; DrawView;
                          end;
              cmScrollDn: if Pos.Y < FileLines^.Count - 1 then
                          begin
                           UnMark := On;
                           if not (Delta.Y > Pos.Y) then Inc(Delta.Y);
                           Inc(Pos.Y);
                           ScrollTo(Delta.X, Delta.Y);
                           Delta.Y := VScroll^.Value; DrawView;
                          end;
              cmBlockStart: begin UnMark := On; Marking := Off; Mark.A := Delta;
                                  BlockVisible := On; DrawView; CE end;
              cmBlockEnd: begin UnMark := On; Marking := Off; Mark.B := Delta;
                                BlockVisible := On; DrawView; CE end;
              cmPlaceMarker1..cmPlaceMarker9:
                begin
                  UnMark := On;
                  Event.InfoByte:=Event.Command-cmPlaceMarker1+1;
                  if (MarkPos[Event.InfoByte].Y = Delta.Y) then
                    FillChar(MarkPos[Event.InfoByte], SizeOf(TPoint), $FF)
                    else MarkPos[Event.InfoByte] := Delta;
                  CED;
                end; {-$VIV}
              cmGotoMarker1..cmGotoMarker9:
                begin ChangeLine;
                      UnMark := On;
                      Event.InfoByte:=Event.Command-cmGotoMarker1+1;
                      if not MarkPos[Event.InfoByte].EqualsXY(-1, -1) then
                        begin
                          Delta := MarkPos[Event.InfoByte];
                          Pos.X := Delta.X - Size.X div 2;
                          Pos.Y := Delta.Y - Size.Y div 2; ChangeLine; {WorkModify := Off;}
                          ScrollTo(Delta.X, Delta.Y); DrawView; CenterScreen;
                        end;
                      CE
                end;
              cmDelBackChar: begin BlockOff; MakeBack; CE end;
              cmDelWordRight: begin
                               BlockOff;
                               if (LastX >= WL) then
                                begin
                                 MakeDel; While (LastX >= WL) and (LastY + 1 < FileLines^.Count)
                                                or (LastX < WL) and (WorkString[LastX+1] = ' ') do MakeDel;
                                 CED; Exit;
                                end;
                               WorkModify;
                               if (WorkString[LastX+1] in BreakChars-[' ']) then begin MakeDel; CED; Exit; end;
                               if (WorkString[LastX+1] = ' ') then
                                 while (LastX < WL) and (WorkString[LastX+1] = ' ') do MakeDel
                                else begin
                                      While (LastX < WL) and not (WorkString[LastX+1] in BreakChars) do MakeDel;
                                      while (LastX < WL) and (WorkString[LastX+1] = ' ') do MakeDel
                                     end;
                               CED;
                              end;
              cmDelWordLeft: begin
                              BlockOff;
                              if LastX = 0 then begin MakeBack; CED; Exit end;
                              if LastX >= WL
                               then if WL > 0
                                     then LastX := WL
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
        if (Event.KeyCode <> kbCtrlAltX) then
        if (Event.KeyCode = kbCtrlU) then WordEnd else {-$VIV}
        begin
             if DrawMode > 0 then
               begin
                 case Event.KeyCode of
                    kbRight,kbCtrlD,kbCtrlRight: begin DrawLine(1); Exit end;
                    kbLeft,kbCtrlS,kbCtrlLeft: begin DrawLine(3); Exit end;
                    kbUp,kbCtrlE: begin DrawLine(0); Exit end;
                    kbDown,kbCtrlX: begin DrawLine(2); Exit end;
                 end;
               end;
     (*      if Event.KeyCode = kbCtrlEnter then ; *) {-$VIV 18.05.99--}
     (*      begin BlockOff; MakeEnter; ScrollTo(Delta.X, Delta.Y); CED; Exit end; *)
             CFind := Off; EvStr := #0#0;
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
             if EvStr[2] in ['A'..'Z','a'..'z'] then EvStr[2] := Char(Ord(UpCase(EvStr[2]))-64);
             for I := 1 to MaxCommands do
              with EditCommands[I] do
                if ((C1 = Event.KeyCode) or (C2 = Event.KeyCode)) and (EvStr[2]=#0) or
                   (EvStr<>#0#0) and ((CC1 = EvStr) or (CC2 = EvStr)) then
                    begin CFind := True; Break end;
             if CFind then
              begin CE; MessageL(Owner, evCommand, EditCommands[I].C,
                                Byte(EvStr[2])); Exit end;
             if (Event.CharCode>#31) and (Event.CharCode<#255) then InputChar;
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
                  EnableMarking := Off;
                  MakeLocal(Event.Where, Delta);
                  Delta.X := Pos.X + Delta.X;
                  Delta.Y := Pos.Y + Delta.Y;
                  BlockOff;
                  Sel.A := Delta; Sel.B := Delta;
                  ScrollTo(Delta.X, Delta.Y);
                  Sel.A := Delta; Sel.B := Delta;
                  EnableMarking := On;
                  if Event.Buttons and mbLeftButton <> 0 then
                   begin BlockVisible := On; MouseMark := On; end
                    else begin RulerVisible := On; DrawView; end;
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
                  MouseMark := Off; RulerVisible := Off;
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
          ( var B; const S : String; const Attr : string );
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
         Str:String;
         Idx:Integer;
     begin
         with SmartWindow^.Intern^ do
         begin
             if SPInsertDate then
             begin
                 Str:='';
                 For Idx:=0 To 5 Do Str:=Str+Char(SPLineChar);
                 Str:=Str+'< '+GetDateTime(Off)+' '+GetDateTime(On)+' >';
                 For Idx:=0 To 35 Do Str:=Str+Char(SPLineChar);
                 FileLines^.Insert(NewStr(Str));
             end;
             FileLines^.Insert(nil);
             SetLimits;
             ScrollTo(0, FileLines^.Count-1);
             Pos.X := Delta.X - Size.X div 2;
             Pos.Y := Delta.Y - Size.Y div 2;
             SmartWindow^.Redraw;
         end;
     end;

begin
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
  if SystemData.Options and ossUseSysClip <> 0 then SyncClipOut(On);
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
