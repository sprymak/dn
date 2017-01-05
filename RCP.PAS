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

{$M 131040}
program RCP;

{                                     }
{  Dos Navigator's Resource Compiler  }
{                                     }
{DataCompBoy: *.dnl and *.dnr now can contain constructions like this }
{ ";$IFDEF bla-bla"                                                   }
{here text                                                            }
{ ";$ELSE"                                                            }
{here also text                                                       }
{ ";$ENDIF"                                                           }
{but constructions                                                    }
{ ";$DEFINE bla-bla"                                                  }
{ ";$UNDEF bla-bla"                                                   }
{didn't work. Because I want so 8) All defines you can do in separate }
{file                                                                 }

uses
  Lfn, Collect, Drivers, Dos, Objects, advance, advance1, advance2,
    advance3,
  RStrings, GetConst, Menus, Commands, Setups, DNHelp, Views, DNApp,
    Startup,
  Dialogs, Validate, IniFiles, Scroller, ListMakr, ObjType, RegAll,
    DefColl,
  ColorSel,
  SWE;

const
  tidCommands = 'cm'#0'cd'#0;
  tidHelpCtx = 'hc'#0;
  tidHistory = 'hs'#0;
  tidKbdconst = 'kb'#0;
  tidOptions = 'of'#0;
  tidDLGs = 'TDlgIdx';
  tidDLs = 'TStrIdx';

var
  Types: PValuesHolder;
  LngFileName: String;
  OutLngFileName: String;
  DlgFileName: String;
  OutDlgFileName: String;
  OutDir: String;
  DLStream: TBufStream;
  DefineParser: TDefCollection;

procedure Error(const s: String);
  begin
    Writeln(#13, s);
    Halt(1);
  end;

function RemoveLeadSpaces(s: String): String;
  begin
    DelLeft(s);
    RemoveLeadSpaces := s;
  end;

const
  BreakChars = [#9, #10, #13, #12, ' ', #0];

function wSkipBlank(const s: String; i: integer): integer;
  near;
  begin
    while (i <= Length(s)) and (s[i] in BreakChars) do
      Inc(i);
    wSkipBlank := i;
  end;

function wSkipText(const s: String; i: integer): integer;
  near;
  begin
    while (i <= Length(s)) and not (s[i] in BreakChars) do
      Inc(i);
    wSkipText := i;
  end;

function SeekWord(const s: String; No: integer): integer;
    { Pos of word #No in S }
  var
    i: integer;
  begin
    i := 1;
    SeekWord := 0;
    if No >= 1 then
      repeat
        i := wSkipBlank(s, i);
        if i > Length(s) then
          break;
        Dec(No);
        if No <= 0 then
          begin
            SeekWord := i;
            break;
          end;
        i := wSkipText(s, i);
        if i > Length(s) then
          break;
      until False;
  end { SeekWord };

function GetWord(s: String; No: integer): String; { Word #No of S }
  var
    i: integer;
  begin
    i := SeekWord(s, No);
    if i > 0 then
      begin
        if i > 1 then
          begin
            SetLength(s, Length(s)-(i-1));
            Move(s[i], s[1], Length(s));
          end;
        SetLength(s, wSkipText(s, 1)-1);
        GetWord := s;
      end
    else
      GetWord := '';
  end;

const
  RStringList: TStreamRec =
  (ObjType: otStringList;
  VmtLink: (TypeOf(Collect.TStringList));
  Load: @Collect.TStringList.Load;
  Store: nil);

  {-DataCompBoy-}
procedure ProcessDLs(Enable: boolean);
  var
    DLS: PTypeHolder;
    SLM: PStrListMaker;
    Fail: boolean;
    F: lText;
    s, s1: String;
    P: PLngWord;
  function DoSeekID(P: PLngWord): boolean;
    begin
      DoSeekID := P^.Name = s1
    end;
  procedure DoTest(P: PLngWord);
    begin
      if P^.Mark = 0 then
        begin
          Writeln('Unresolved identifier "'+P^.Name+'"');
          Fail := True;
        end;
    end;
  function MakeStr(s: String): String;
    label 1, 2;
    var
      A, M: String;
      i, j: integer;
      B: boolean;
      NotFlag: boolean;
    begin
      MakeStr := '';
      DelLeft(s);
      if s = '' then
        exit;
      i := 0;
      B := False;
      A := '';
1:
      while i < Length(s) do
        begin
          Inc(i);
          case s[i] of
            '''':
              if (s[i+1] = '''') and B and (i < Length(s)) then
                begin
                  Inc(i);
                  A := A+'''' {AddStr(A, '''')}
                end
              else
                B := not B;
            '^':
              if B then
                A := A+s[i] {AddStr(A, S[I])}
              else
                begin
                  Inc(i);
                  A := A+Char(byte(UpCase(s[i]))-64)
                    {AddStr(A, Char(Byte(UpCase(S[I]))-64));}
                end;
            '#':
              if B then
                A := A+s[i] {AddStr(A, S[I])}
              else
                begin
                  j := 1;
                  M := '';
                  repeat
                    Inc(i);
                    M := M+s[i]; {AddStr(M, S[I]);}
                  until (i >= Length(s)) or not (s[i+1] in ['0'..'9']);
                  A := A+Char(StoI(M)) {AddStr(A, Char(StoI(M)));}
                end;
            else
              if B then
                A := A+s[i]; {AddStr(A, S[I]);}
          end {case};
        end;
      if s[Length(s)] = '+' then
        begin
2:
          readln(F.t, s);
          if s <> '' then
            begin
              s := DefineParser.ProceedStr2(s);
              if s = '' then
                goto 2;
            end;
          DelLeft(s);
          i := 0;
          goto 1;
        end;
      if Pos(^Z, UpStrg(A)) = 1
      then
        A := RemoveLeadSpaces(Copy(A, 2, MaxStringLength));
      MakeStr := A;
    end { MakeStr };
  begin { ProcessDLs }
    if Enable then
      begin
        DLS := Types^.GetType(tidDLs);
        New(SLM, Init($FFF0, $280));
        lAssignText(F, LngFileName);
        lResetText(F);
        if IOResult <> 0 then
          Error('Cannot open file '+LngFileName);
        Writeln('Reading ', LngFileName);
        while not Eof(F.t) do
          begin
            readln(F.t, s);
            s := DefineParser.ProceedStr2(s);
            DelLeft(s);
            s1 := GetWord(UpStrg(s), 1);
            while (s1 <> '') and (s1[Length(s1)] = ',') do
              SetLength(s1, Length(s1)-1);
            if (s1 = '') or (s1[1] = ';') then
              continue;
            P := DLS^.FirstThat(@DoSeekID);
            if P = nil then
              Error('Unknown identifier "'+GetWord(s, 1)+'"');
            if P^.Mark = 1 then
              Error('Duplicate identifier "'+GetWord(s, 1)+'"');
            SLM^.Put(P^.l, MakeStr(s));
            P^.Mark := 1;
          end;
        Close(F.t);
        Fail := False;
        DLS^.ForEach(@DoTest);
        if Fail then
          Halt(1);
        DLStream.Init(OutLngFileName, stCreate, 512);
        if DLStream.Status <> stOK then
          Error('Cannot create file '+OutLngFileName);
        Writeln('Writing ', OutLngFileName);
        DLStream.Put(SLM);
        if DLStream.Status <> stOK then
          begin
            DLStream.Done;
            EraseFile(OutLngFileName);
            Error('Error writing file '+OutLngFileName);
          end
        else
          DLStream.Done;
        Dispose(SLM, Done);
      end;
    ReRegisterType(RStringList);
    DLStream.Init(OutLngFileName, stOpenRead, 512);
    LStringList := PStringList(DLStream.Get);
    if (LStringList = nil) and Enable then
      begin
        DLStream.Done;
        EraseFile(OutLngFileName);
        Error('Error reading file '+OutLngFileName);
      end;
  end { ProcessDLs };
{-DataCompBoy-}

type
  TEditCommand = record
    C, C1, C2: word;
    CC1, CC2: array[1..2] of Char;
    end;

  {                            Resource Processing                             }
  {----------------------------------------------------------------------------}
var
  s: String;
  F: lText; {DataCompBoy}
  Line: longInt;
  IDs: PTypeHolder;
  DLGs: PTypeHolder;

const
  idSubMenu = 'SUBMENU ';
  idMenuItem = 'MENUITEM ';
  idMenuLine = 'MENULINE';
  idMenu = 'MENU ';
  idEND = 'END';
  idStatusDef = 'STATUSDEF ';
  idStatusItem = 'STATUSITEM ';
  idStatusLine = 'STATUSLINE';
  idDialog = 'DIALOG ';
  idLabel = 'LABEL ';
  idInputLine = 'INPUTLINE ';
  idHexLine = 'HEXLINE '; {DataCompBoy}
  idButton = 'BUTTON ';
  idCheckBoxes = 'CHECKBOXES ';
  idRadioButtons = 'RADIOBUTTONS ';
  idHistory = 'HISTORY ';
  idItem = 'ITEM ';
  idHelpCtx = 'HELPCTX ';
  idStaticText = 'STATICTEXT ';
  idListBox = 'LISTBOX ';
  idScrollBar = 'SCROLLBAR ';
  idMScrollBar = 'MOUSESCROLLBAR ';
  idParamText = 'PARAMTEXT ';
  idColorGroup = 'COLORGROUP ';
  idColorItem = 'COLORITEM ';
  idColorDialog = 'COLORDIALOG';
  idUpperTable = 'UPPERTABLE ';
  idSelectBack = 'SELECTBACK';
  idSelectForward = 'SELECTFORWARD';
  idCommand = 'COMMAND ';
  idEditorCommands = 'EDITOR COMMANDS';
  idDriveCheckBox = 'DRIVECHECKBOXES ';
  idColorPoint = 'COLORPOINT ';

  TheRF: PIdxMaker = nil;

var
  D: PColorDialog;
  PL, PP: PColorGroup;
  PI: PColorItem;
  i: longInt;
  NumCommands: AInt;

const
  MaxCommands = 200;

var
  EditCommands: array[1..MaxCommands] of TEditCommand;

type
  TEditSaver = object(TObject)
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    end;

const
  REditSaver: TStreamRec = (
  ObjType: 12335;
  VmtLink: (TypeOf(TEditSaver));
  Load: @TEditSaver.Load;
  Store: @TEditSaver.Store);

Constructor TEditSaver.Load(var s: TStream);
  begin
    s.Read(NumCommands, SizeOf(NumCommands));
    s.Read(EditCommands, SizeOf(TEditCommand)*NumCommands);
  end;

procedure TEditSaver.Store(var s: TStream);
  begin
    s.Write(NumCommands, SizeOf(NumCommands));
    s.Write(EditCommands, SizeOf(TEditCommand)*NumCommands);
  end;

{-DataCompBoy-}
function FSetExt(F: String; NewExt: String): String;
  var
    Dir: String;
    Name: String;
    ext: String;
  begin
    lFSplit(F, Dir, Name, ext);
    FSetExt := Dir+Name+NewExt;
  end;
{-DataCompBoy-}

procedure StoreResource(P: PObject; Id: TDlgIdx);
  var
    W: PLngWord;
  function HaveThisID(P: PLngWord): boolean;
    begin
      HaveThisID := TDlgIdx(P^.l) = Id
    end;
  begin
    W := DLGs^.FirstThat(@HaveThisID);
    if W = nil then
      Error('*ERROR*: Index #'+ItoS(word(Id))+' out of range.');
    if (TheRF^.Empty(TDlgIdx(Id))) and (W^.Mark = 0)
    then
      if P <> nil then
        TheRF^.Put(P, TDlgIdx(Id))
      else
    else
      Error(#13'*ERROR*: Duplicate definition for "'+W^.Name+'"');
    W^.Mark := 1;
  end;

function GetID(const s: String): longInt;
  var
    t: TLngWord;
    i: CondInt; {???}
    l: longInt;
    k: integer;
  begin
    FreeStr := s;
    DelRight(FreeStr);
    if s[Length(s)] = ',' then
      SetLength(FreeStr, Length(FreeStr)-1);
    DelRight(FreeStr);
    t.Init(0, FreeStr);
    if IDs^.Search(@T, i) then
      begin
        GetID := PLngWord(IDs^.At(i))^.l;
      end
    else
      begin
        Val(FreeStr, l, k);
        if k > 0 then
          Error('Unknown identifier ('+FreeStr+') in line '+ItoS(
            Line));
        GetID := l;
      end;
  end { GetID };

function Token(const s: String; var Pos: longInt): String;
  var
    A: String;
    B, Cmd: boolean;
    M: String;
    j: longInt;
  begin
    Token := '';
    A := '';
    B := False;
    Cmd := False;
    while (s[Pos] = ' ') and (Length(s) >= Pos) do
      Inc(Pos);
    while Pos <= Length(s) do
      begin
        case s[Pos] of
          '''':
            if (s[Pos+1] = '''') and B and (Pos < Length(s)) then
              begin
                Inc(Pos);
                A := A+''''; {AddStr(A, '''')}
              end
            else
              B := not B;
          ',':
            if not B or Cmd then
              break
            else
              A := A+s[Pos]; {AddStr( A, S[Pos] );}
          '^':
            if B then
              A := A+s[Pos] {AddStr(A, S[Pos])}
            else
              begin
                Inc(Pos);
                A := A+Char(byte(UpCase(s[Pos]))-64)
                  {AddStr(A, Char(Byte(UpCase(S[Pos]))-64));}
              end;
          '#':
            if B then
              A := A+s[Pos] {AddStr(A, S[Pos])}
            else
              begin
                j := 1;
                M := '';
                repeat
                  Inc(Pos);
                  M := M+s[Pos] {AddStr(M, S[Pos]);}
                until not (s[Pos+1] in ['0'..'9']) or (Pos >= Length(s))
                  ;
                A := A+Char(StoI(M)) {AddStr(A, Char(StoI(M)));}
              end;
          else
            if B then
              A := A+s[Pos] {AddStr(A, S[Pos])}
            else
              begin
                B := True;
                Cmd := True;
                A := A+s[Pos] {AddStr(A, S[Pos])}
              end;
        end {case};
        Inc(Pos);
      end;
    Inc(Pos);
    Token := A;
  end { Token };

procedure MakeGroup;
  var
    P: PColorGroup;
  begin
    i := Length(idColorGroup);
    P := ColorGroup(Token(s, i), nil, nil);
    if PL = nil then
      PP := P
    else
      PL^.Next := P;
    PL := P;
    PI := nil;
  end;

procedure MakeItem;
  var
    P: PColorItem;
  begin
    if PL = nil then
      Error('Color group should be defined first');
    i := Length(idColorItem);
    P := ColorItem(Token(s, i), GetID(Token(s, i)), nil);
    if PI = nil then
      PL^.Items := P
    else
      PI^.Next := P;
    PI := P;
  end;

{-DataCompBoy-}
procedure MakeColorDialog;
  begin
    PI := nil;
    PL := nil;
    PP := nil;
    while not Eof(F.t) do
      begin
        readln(F.t, s);
        s := DefineParser.ProceedStr2(s);
        Inc(Line);
        DelLeft(s);
        FreeStr := s;
        UpStr(FreeStr);
        if (s <> '') and (s[1] <> ';') then
          if Copy(FreeStr, 1, Length(idColorGroup)) = idColorGroup
          then
            MakeGroup
          else if Copy(FreeStr, 1, Length(idColorItem)) =
              idColorItem
          then
            MakeItem
          else if Copy(FreeStr, 1, Length(idEND)) = idEND then
            break
          else
            Error('Unknown identifier in line '+ItoS(Line));
      end;
    New(D, Init(PP));
    D^.HelpCtx := hcColorDialog;
    StoreResource(D, dlgColors);
    Dispose(D, Done);
  end { MakeColorDialog };
{-DataCompBoy-}

{-DataCompBoy-}
procedure MakeEditorCommands;
  var
    i, j: longInt;
    t: ^TEditSaver;

  procedure MakeCommand;
    begin
      if NumCommands = MaxCommands then
        Error('Editor Commands top limit ('+ItoS(MaxCommands)+
          ') reached');
      Inc(NumCommands);
      i := Length(idCommand);
      with EditCommands[NumCommands] do
        begin
          C := GetID(Token(s, i));
          C1 := GetID(Token(s, i));
          C2 := GetID(Token(s, i));
          FreeStr := Token(s, i)+#0#0;
          Move(FreeStr[1], CC1, 2);
          FreeStr := Token(s, i)+#0#0;
          Move(FreeStr[1], CC2, 2);
        end;
    end;

  begin { MakeEditorCommands }
    FillChar(EditCommands, SizeOf(EditCommands), 0);
    NumCommands := 0;
    while not Eof(F.t) do
      begin
        readln(F.t, s);
        s := DefineParser.ProceedStr2(s);
        Inc(Line);
        DelLeft(s);
        FreeStr := UpStrg(s);
        if (s <> '') and (s[1] <> ';') then
          if Copy(FreeStr, 1, Length(idCommand)) = idCommand then
              MakeCommand
          else if Copy(FreeStr, 1, Length(idEND)) = idEND then
            break
          else
            Error('Unknown identifier in line '+ItoS(Line));
      end;
    New(t, Init);
    StoreResource(t, dlgEditorCommands);
    t^.Free;
  end { MakeEditorCommands };
{-DataCompBoy-}

procedure ProcessDLGs;
  var
    TP: PTypeHolder;
    ST: PStream;
  function GetID(const s: String): longInt;
    var
      t: TLngWord;
      i: integer;
      l: CondInt;
    begin
      FreeStr := s;
      DelRight(FreeStr);
      if s[Length(s)] = ',' then
        SetLength(FreeStr, Length(FreeStr)-1);
      DelRight(FreeStr);
      t.Init(0, FreeStr);
      if IDs^.Search(@T, l) then
        begin
          GetID := PLngWord(IDs^.At(l))^.l;
        end
      else
        begin
          Val(FreeStr, l, i);
          if i > 0 then
            Error('Unknown identifier ('+FreeStr+') in line '+ItoS(
              Line));
          GetID := l;
        end;
    end { GetID };

  procedure SetSavers;
    {$IFDEF SS}
    var
      D: PDialog;
    begin
      D := MakeSaversDialog;
      StoreResource(D, dlgSaversSetup);
      Dispose(D, Done);
      {$ELSE}
      begin
        StoreResource(nil, dlgSaversSetup);
        {$ENDIF}
      end;

      function CompileStatus: PStatusDef;
        var
          PM, PL: PStatusItem;
          dM, DL: PStatusDef;
          i, j: longInt;
          s: String;

        procedure MakeStatusDef;
          var
            P: PStatusDef;
          begin
            i := Length(idStatusDef);
            P := NewStatusDef(GetID(Token(s, i)), GetID(Token(s, i)),
              nil, nil);
            if DL = nil then
              dM := P
            else
              DL^.Next := P;
            DL := P;
            PL := nil;
          end;

        procedure MakeStatusItem;
          var
            P: PStatusItem;
          begin
            if DL = nil then
              Error(
                'Could not make Status Item without Status Definition');
            i := Length(idStatusItem);
            P := NewStatusKey(Token(s, i), GetID(Token(s, i)), GetID(
              Token(s, i)), nil);
            if PL = nil then
              DL^.Items := P
            else
              PL^.Next := P;
            PL := P;
          end;

        {-DataCompBoy-}
        begin { CompileStatus: }
          dM := nil;
          DL := nil;
          PM := nil;
          PL := nil;
          while not Eof(F.t) do
            begin
              readln(F.t, s);
              s := DefineParser.ProceedStr2(s);
              Inc(Line);
              DelLeft(s);
              if (s <> '') and (s[1] <> ';') then
                if UpStrg(Copy(s, 1, Length(idStatusDef))) =
                    idStatusDef
                then
                  MakeStatusDef
                else if UpStrg(Copy(s, 1, Length(idStatusItem))) =
                    idStatusItem
                then
                  MakeStatusItem
                else if UpStrg(Copy(s, 1, Length(idEND))) = idEND
                then
                  break
                else if (s <> '') and (s[1] <> ';') then
                  Error('Unknown identifier in line '+ItoS(Line));
            end;
          CompileStatus := dM;
        end { CompileStatus: };
      {-DataCompBoy-}

      procedure MakeStatus;
        var
          PM: PStatusDef;
          R: TRect;
        begin
          PM := CompileStatus;
          R.Assign(0, 0, 80, 1);
          StatusLine := New(PStatusLine, Init(R, PM));
          StoreResource(StatusLine, dlgStatusLine);
          Dispose(StatusLine, Done);
          StatusLine := nil;
        end;

      function CompileMenu(s: String): PMenu;
        var
          PM, PL: PMenuItem;
          s1, s2: String;
          i, j: longInt;

        procedure AddItem(P: Pointer);
          begin
            if PM = nil then
              PM := P
            else
              PL^.Next := P;
            PL := P;
          end;

        procedure MakeSubMenu;
          begin
            i := Length(idSubMenu);
            s1 := Token(s, i);
            s2 := Token(s, i);
            AddItem(NewSubMenu(s1, GetID(s2), CompileMenu(s), nil));
          end;

        procedure MakeMenuItem;
          var
            HK, Cmd, Ctx: word;
          begin
            i := Length(idMenuItem);
            AddItem(NewItem(Token(s, i), Token(s, i), GetID(Token(s, i))
              ,
            GetID(Token(s, i)), GetID(Token(s, i)), nil));
          end;

        procedure MakeMenuLine;
          begin
            AddItem(NewLine(nil));
          end;

        {-DataCompBoy-}
        begin { CompileMenu }
          CompileMenu := nil;
          PL := nil;
          PM := nil;
          while not Eof(F.t) do
            begin
              readln(F.t, s);
              s := DefineParser.ProceedStr2(s);
              Inc(Line);
              DelLeft(s);
              if (s <> '') and (s[1] <> ';') then
                if UpStrg(Copy(s, 1, Length(idSubMenu))) = idSubMenu
                then
                  MakeSubMenu
                else if UpStrg(Copy(s, 1, Length(idMenuItem))) =
                    idMenuItem
                then
                  MakeMenuItem
                else if UpStrg(Copy(s, 1, Length(idMenuLine))) =
                    idMenuLine
                then
                  MakeMenuLine
                else if UpStrg(s) = idEND then
                  break
                else if (s <> '') and (s[1] <> ';') then
                  Error('Unknown identifier in line '+ItoS(Line));
            end;
          CompileMenu := NewMenu(PM);
        end { CompileMenu };
      {-DataCompBoy-}

      procedure MakeMenu(s: String);
        var
          Id: TDlgIdx;
          t: TLngWord;
          R: TRect;
          D: PMenuBar;
          i: longInt;
          j: CondInt; {???}
        begin
          i := Length(idMenu);
          FreeStr := Token(s, i);
          t.Init(0, FreeStr);
          if not DLGs^.Search(@T, j) then
            Error('Unknown Resource ID - '+t.Name);
          Id := TDlgIdx(PLngWord(DLGs^.At(j))^.l);
          FillChar(R, SizeOf(R), 0);
          New(D, Init(R, CompileMenu(s)));
          StoreResource(D, Id);
          Dispose(D, Done);
        end;

      procedure CompileDialog(s: String);
        var
          D: PDialog;
          R: TRect;
          i: longInt;
          j: CondInt;
          PV: PView;
          LastSB: PScrollBar;

        function IsThis(const s: String): boolean;
          begin
            i := Length(s);
            IsThis := Copy(FreeStr, 1, i) = s;
          end;

        procedure MakeInputLine;
          var
            P: PHistory;
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            R.B.Y := R.A.Y+1;
            PV := New(PInputline, Init(R, GetID(Token(s, i))));
            D^.Insert(PV);
            j := GetID(Token(s, i));
            if j > 0 then
              begin
                R.A.X := R.B.X;
                R.B.X := R.A.X+3;
                New(P, Init(R, PInputline(PV), j));
                D^.Insert(P);
              end;
          end;

        {-DataCompBoy-}
        procedure MakeHexLine;
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            R.B.Y := R.A.Y+1;
            PV := New(PHexLine, Init(R, PInputline(PV)));
            D^.Insert(PV);
          end;
        {-DataCompBoy-}

        procedure MakeLabel;
          var
            B: String;
            P: PLabel;
          begin
            R.A.X := GetID(Token(s, i));
            R.A.Y := GetID(Token(s, i));
            R.B.Y := R.A.Y+1;
            B := Token(s, i);
            R.B.X := R.A.X+2+CStrLen(B);
            New(P, Init(R, B, PV));
            D^.Insert(P);
            while i < Length(s) do
              P^.Options := P^.Options or GetID(Token(s, i));
          end;

        procedure MakeHistory;
          var
            B: String;
            P: PHistory;
          begin
            R.A.X := GetID(Token(s, i));
            R.B.X := R.A.X+3;
            R.A.Y := GetID(Token(s, i));
            R.B.Y := R.A.Y+1;
            New(P, Init(R, PInputline(PV), GetID(Token(s, i))));
            D^.Insert(P);
            while i < Length(s) do
              P^.Options := P^.Options or GetID(Token(s, i));
          end;

        {-DataCompBoy-}
        function GetItems: PSItem;
          var
            P, PP: PSItem;
            s: String;
            k: integer;
          begin
            PP := nil;
            k := i;
            while not Eof(F.t) do
              begin
                readln(F.t, s);
                s := DefineParser.ProceedStr2(s);
                Inc(Line);
                DelLeft(s);
                DelRight(s);
                FreeStr := s;
                UpStr(FreeStr);
                if IsThis(idItem) then
                  begin
                    if PP = nil then
                      begin
                        P := NewSItem(Token(s, i), nil);
                        PP := P
                      end
                    else
                      begin
                        P^.Next := NewSItem(Token(s, i), nil);
                        P := P^.Next
                      end;
                  end
                else if IsThis(idEND) then
                  break
                else if (s <> '') and (s[1] <> ';') then
                  Error('Unknown identifier in line '+ItoS(Line));
              end;
            GetItems := PP;
            i := k;
          end { GetItems: };
        {-DataCompBoy-}

        procedure MakeCheckBoxes;
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            PV := New(PCheckBoxes, Init(R, GetItems));
            D^.Insert(PV);
          end;

        procedure MakeDriveCheckBoxes;
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            PV := New(PCurrDriveInfo, Init(R, GetItems));
            D^.Insert(PV);
            with PV^ do
              begin
                Options := Options or ofPostProcess;
                EventMask := evBroadcast+evCommand+evKeyDown+
                  evMouseDown;
              end;
          end;

        procedure MakeRadioButtons;
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            PV := New(PRadioButtons, Init(R, GetItems));
            D^.Insert(PV);
          end;

        procedure MakeButton;
          var
            B, k: String;
            Flags, Options: word;
            Cmd: word;
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            k := Token(s, i);
            Cmd := GetID(Token(s, i));
            Flags := 0;
            Options := 0;
            while i < Length(s) do
              begin
                B := Token(s, i);
                UpStr(B);
                if B = 'BFDEFAULT' then
                  Flags := Flags or bfDefault
                else if B = 'BFNORMAL' then
                  Flags := Flags or bfNormal
                else if B = 'BFBROADCAST' then
                  Flags := Flags or bfBroadcast
                else if B = 'BFLEFTJUST' then
                  Flags := Flags or bfLeftJust
                else if B = 'BFGRABFOCUS' then
                  Flags := Flags or bfGrabFocus
                else
                  Options := Options or GetID(B);
              end;
            PV := New(PButton, Init(R, k, Cmd, Flags));
            PV^.Options := PV^.Options or Options;
            D^.Insert(PV);
          end { MakeButton };

        procedure MakeScrollBar(Mouse: boolean);
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            if Mouse then
              LastSB := New(PMouseBar, Init(R))
            else
              LastSB := New(PScrollBar, Init(R));
            D^.Insert(LastSB);
          end;

        procedure MakeListBox;
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            PV := New(PListBox, Init(R, GetID(Token(s, i)), LastSB));
            D^.Insert(PV);
          end;

        procedure MakeStaticText;
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            PV := New(PStaticText, Init(R, Token(s, i)));
            D^.Insert(PV);
          end;

        procedure MakeUpperTable;
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            PV := New(PUpperTable, Init(R));
            D^.Insert(PV);
          end;

        procedure MakeParamText;
          begin
            R.Assign(GetID(Token(s, i)), GetID(Token(s, i)), GetID(
              Token(s, i)), GetID(Token(s, i)));
            PV := New(PParamText, Init(R, Token(s, i), GetID(Token(s, i)
              )));
            D^.Insert(PV);
          end;

        procedure MakeColorPoint;
          var
            P: PColorPoint;
          begin
            R.A.X := GetID(Token(s, i));
            R.B.X := R.A.X+1;
            R.A.Y := GetID(Token(s, i));
            R.B.X := R.A.X+1;
            New(P, Init(R, GetID(Token(s, i))));
            D^.Insert(P);
            while i < Length(s) do
              P^.Options := P^.Options or GetID(Token(s, i));
          end;

        var
          Id: TDlgIdx;
          t: TLngWord;

        const
          DData: TSysData = (
          Options: 0;
          Mode1: '';
          Mode2: '';
          Temp: '';
          Drives: (List: nil; Focus: 0);
          Current: 0;
          CopyLimitBuf: '';
          ForceDefArch: ''
          );

          {-DataCompBoy-}
        begin { CompileDialog }
          i := Length(idDialog);
          PV := nil;
          LastSB := nil;
          FreeStr := Token(s, i);
          t.Init(0, FreeStr);
          if not DLGs^.Search(@T, j) then
            Error('Unknown Resource ID: '+t.Name);
          Id := TDlgIdx(PLngWord(DLGs^.At(j))^.l);
          R.Assign(0, 0, GetID(Token(s, i)), GetID(Token(s, i)));
          if Id = dlgSystemSetup then
            begin
              D := New(PSysDialog, Init(R, Token(s, i)));
              D^.Awaken;
            end
          else
            New(D, Init(R, Token(s, i)));
          D^.Options := D^.Options or ofCentered;
          while not Eof(F.t) do
            begin
              readln(F.t, s);
              s := DefineParser.ProceedStr2(s);
              Inc(Line);
              DelLeft(s);
              FreeStr := UpStrg(s);
              if (s <> '') and (s[1] <> ';') then
                begin
                  if IsThis(idInputLine) then
                    MakeInputLine
                  else if IsThis(idHexLine) then
                    MakeHexLine
                  else if IsThis(idHelpCtx) then
                    begin
                      if PV = nil then
                        D^.HelpCtx := GetID(Token(s, i))
                      else
                        PV^.HelpCtx := GetID(Token(s, i))
                    end
                  else if IsThis(idLabel) then
                    MakeLabel
                  else if IsThis(idStaticText) then
                    MakeStaticText
                  else if IsThis(idParamText) then
                    MakeParamText
                  else if IsThis(idListBox) then
                    MakeListBox
                  else if IsThis(idScrollBar) then
                    MakeScrollBar(False)
                  else if IsThis(idMScrollBar) then
                    MakeScrollBar(True)
                  else if IsThis(idUpperTable) then
                    MakeUpperTable
                  else if IsThis(idCheckBoxes) then
                    MakeCheckBoxes
                  else if IsThis(idDriveCheckBox) then
                      MakeDriveCheckBoxes
                  else if IsThis(idSelectForward) then
                    D^.SelectNext(False)
                  else if IsThis(idSelectBack) then
                    D^.SelectNext(True)
                  else if IsThis(idRadioButtons) then
                      MakeRadioButtons
                  else if IsThis(idButton) then
                    MakeButton
                  else if IsThis(idHistory) then
                    MakeHistory
                  else if IsThis(idColorPoint) then
                    MakeColorPoint
                  else if IsThis(idEND) then
                    break
                  else if (s <> '') and (s[1] <> ';') then
                    Error('Unknown identifier in line '+ItoS(Line));
                  while i < Length(s) do
                    PV^.Options := PV^.Options or GetID(Token(s, i));
                end;
            end;
          D^.SelectNext(False);
          StoreResource(D, Id);
          if Id = dlgSystemSetup then
            begin
              New(PCollection(DData.Drives.List), Init(0, 10));
              D^.SetData(DData);
            end;
          Dispose(D, Done);
        end { CompileDialog };
      {-DataCompBoy-}

      procedure DoInsert(P: PLngWord);
        begin
          IDs^.Insert(P);
        end;

      function FailCheck: boolean;
        procedure DoCheck(P: PLngWord);
          begin
            if P^.Mark <> 1 then
              begin
                Writeln('Unresolved identifier "', P^.Name, '"');
                FailCheck := True;
              end;
          end;
        begin
          FailCheck := False;
          DLGs^.ForEach(@DoCheck);
        end;

      {-DataCompBoy-}
      begin
        DLGs := Types^.GetType(tidDLGs);
        New(IDs, Init('', tmConst));
        TP := Types^.GetType(tidCommands);
        TP^.ForEach(@DoInsert);
        TP := Types^.GetType(tidHelpCtx);
        TP^.ForEach(@DoInsert);
        TP := Types^.GetType(tidHistory);
        TP^.ForEach(@DoInsert);
        TP := Types^.GetType(tidKbdconst);
        TP^.ForEach(@DoInsert);
        TP := Types^.GetType(tidOptions);
        TP^.ForEach(@DoInsert);

        Writeln('Reading ', DlgFileName);
        lAssignText(F, DlgFileName);
        ClrIO;
        lResetText(F);
        Line := 0;
        if IOResult <> 0 then
          Error('Could not open input file');
        ST := New(PBufStream, Init(OutDlgFileName, stCreate, 512));
        if ST^.Status <> stOK then
          begin
            Close(F.t);
            Dispose(ST, Done);
            EraseFile(OutDlgFileName);
            Error('Could not create output file '+OutDlgFileName);
          end;

        TheRF := New(PIdxMaker, Init(ST));
        while not Eof(F.t) do
          begin
            readln(F.t, s);
            s := DefineParser.ProceedStr2(s);
            Inc(Line);
            Write(#13'(', Line, ')');
            DelLeft(s);
            FreeStr := UpStrg(s);
            if (s <> '') and (s[1] <> ';') then
              if Copy(FreeStr, 1, Length(idColorDialog)) =
                  idColorDialog
              then
                MakeColorDialog
              else if Copy(FreeStr, 1, Length(idDialog)) = idDialog
              then
                CompileDialog(s)
              else if Copy(FreeStr, 1, Length(idMenu)) = idMenu then
                  MakeMenu(s)
              else if Copy(FreeStr, 1, Length(idStatusLine)) =
                  idStatusLine
              then
                MakeStatus
              else if Copy(FreeStr, 1, Length(idEditorCommands)) =
                  idEditorCommands
              then
                MakeEditorCommands
              else
                Error('Unknown identifier in line '+ItoS(Line));
          end;

        Close(F.t);
        SetSavers;
        if FailCheck then
          Halt(1);

        Writeln(#13'Writing ', OutDlgFileName);
        Dispose(TheRF, Done);

        IDs^.DeleteAll;
        Dispose(IDs, Done);
      end;
      {-DataCompBoy-}

      {-DataCompBoy-}
      function GetName(s: String): String;
        var
          P, n, E: String;
        begin
          lFSplit(s, P, n, E);
          GetName := n;
        end;
      {-DataCompBoy-}

      procedure CleanupTypes;
        procedure DoClean(P: PTypeHolder);
          procedure DoUnmark(P: PLngWord);
            begin
              P^.Mark := 0
            end;
          begin
            P^.ForEach(@DoUnmark);
          end;
        begin
          Types^.ForEach(@DoClean);
        end;

      function ReplaceChar(A, B: Char; s: String): String;
        begin
          Replace(A, B, s);
          ReplaceChar := s;
        end;

      var
        INI: TIniFile;
        INIs: integer;
        Lng: String;
        LList: String;

      procedure InitParser;
        var
          Sec: PIniSection;
          i: integer;
          k, s: String;
        begin
          Sec := INI.GetSection('Parser');
          if Sec <> nil then
            begin
              for i := 0 to Sec^.Count-1 do
                begin
                  k := UpStrg(Sec^.GetKeyAt(i));
                  if k <> '' then
                    begin
                      s := Sec^.GetValueAt(i);
                      DelDoubles('  ', s);
                      if k = 'CONST' then
                        Types^.Insert(New(PTypeHolder, Init(
                          ReplaceChar(' ', #0, s+' '), tmConst)))
                      else if k = 'TYPE' then
                        Types^.Insert(New(PTypeHolder, Init(s,
                          tmEnum)))
                      else
                        Error('Undefined keyword "'+k+
                          '" in [Parser] section');
                    end;
                end;
            end;
        end { InitParser };

      function CutWord(const s: String; No: integer): String;
          { S without word #No }
        var
          i, k: integer;
        label
          Same;
        begin
          if No < 1 then
            goto Same;
          i := SeekWord(s, No);
          if i > 0
          then
            CutWord := Copy(s, 1, i-1)+Copy(s, wSkipBlank(s,
              wSkipText(s, i)), MaxStringLength)
          else
Same:
            CutWord := s;
        end;

      begin
        Writeln(
          #13'Resource Compiler for DN OSP 1.51.07a+  Version 1.08');
        Writeln('Copyright(C) 1994,95 RIT Research Labs');
        Writeln('Copyright(C) 1995 AxoN(R)Soft');

        {Cat}
        {/Cat}

        {Cat: выбираем, какой конфигурационный файл использовать:
      - если в командной строке O или OS2 - RCPVPO.INI
      - если в командной строке W или W32 - RCPVPW.INI
      - если в командной строке D или D32 - RCPVPD.INI
      - если командная строка пуста - конфигурационный файл,
      соответствующий системе, для которой скомпилирован RCP.EXE}

        FreeStr := ParamStr(1);
        UpStr(FreeStr);
        if (FreeStr = 'O') or (FreeStr = 'OS2')
          or (FreeStr = 'W') or (FreeStr = 'W32')
          or (FreeStr = 'D') or (FreeStr = 'D32')
          or (FreeStr = '')
        then
          begin
            if FreeStr = '' then
              {$IFDEF OS2}
              FreeStr := 'O';
              {$ELSE}
              {$IFDEF WIN32}
              FreeStr := 'W';
              {$ELSE}
              {$IFDEF DPMI32}
              FreeStr := 'D';
              {$ELSE}
              FreeStr := '?';
            {$ENDIF}
            {$ENDIF}
            {$ENDIF}
            Writeln('Using config file RCPVP'+FreeStr[1]+'.INI');
            INI.Init('RCPVP'+FreeStr[1]+'.INI', INIs);
            if INIs <> stOK then
              Error('File RCPVP'+FreeStr[1]+'.INI not found.');
          end
        else
          Error('Invalid parameter "'+FreeStr+'".');
        {/Cat}

        RegisterType(RStrListMaker);
        RegisterAll;
        RegisterType(REditSaver);

        New(Types, Init(10, 10));
        InitParser;

        {-DataCompBoy-}
        DefineParser.Init($10, $10);
        LList := INI.Get('Controls', 'Defines');
        repeat
          Lng := GetWord(LList, 1);
          if Lng <> '' then
            begin
              DefineParser.ProceedFile(Lng);
              LList := CutWord(LList, 1);
            end
          else
            break
        until False;
        {-DataCompBoy-}

        LList := INI.Get('Controls', 'Indexes');
        repeat
          Lng := GetWord(LList, 1);
          if Lng <> '' then
            begin
              ProcessFile(Lng, Types);
              LList := CutWord(LList, 1);
            end
          else
            break
        until False;

        LList := INI.Get('Controls', 'Languages');
        repeat
          Lng := GetWord(LList, 1);
          if Lng <> '' then
            begin
              if UpStrg(INI.Get(Lng, 'ProcessDLs')) = 'YES' then
                  begin
                  LngFileName := INI.Get(Lng, 'LngInput');
                  if LngFileName = '' then
                    Error('No LNG input file defined for language "'+
                      Lng+'".');
                  OutLngFileName := INI.Get(Lng, 'LngOutput');
                  if OutLngFileName = '' then
                    Error(
                      'No LNG output file defined for language "'+
                      Lng+'".');
                  ProcessDLs(True);
                end
              else
                ProcessDLs(False);
              DlgFileName := INI.Get(Lng, 'DlgInput');
              if DlgFileName = '' then
                Error('No DLG input file defined for language "'+Lng+
                  '".');
              OutDlgFileName := INI.Get(Lng, 'DlgOutput');
              if OutDlgFileName = '' then
                Error('No DLG output file defined for language "'+
                  Lng+'".');
              ProcessDLGs;
              if LStringList <> nil then
                begin
                  Dispose(LStringList, Done);
                  LStringList := nil;
                  if StringCache <> nil then
                    Dispose(StringCache, Done);
                  StringCache := nil;
                end;
              DLStream.Done;
              ReRegisterType(RStrListMaker);
              CleanupTypes;
              LList := CutWord(LList, 1);
            end
          else
            break;
        until False;
        Dispose(Types, Done);
      end.
