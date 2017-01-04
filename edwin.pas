{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  1.6.RC1
//  dn31005-bp_to_vp_on_off_true_false.patch
//  dn31220-Kernel(i)-DN_changes_language_immediately.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}

{$I STDEFINE.INC}
unit EdWin;

interface
uses Objects, MicroEd, Menus, Ed2, UniWin;

type
  { TEditWindow }

  PEditWindow = ^TEditWindow;
  TEditWindow = object(TUniWindow)
    AInfo: PInfoLine;
    ABookLine: PBookmarkLine;
    Intern: PFileEditor;
    MenuBar: PMenuBar;
    UpMenu: PMenu;
    ModalEnd: Boolean;
    constructor Init(R: TRect; FileName: String);
    constructor Load(var S: TStream);
    procedure ChangeBounds(var R: TRect); virtual;
    procedure Store(var S: TStream);
    function Execute: Word; virtual;
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    procedure ChangeLanguage; virtual;  {John_SW  23-12-2003}
  end;

implementation
uses MicroEd2, DnApp, Commands, dnHelp, Views,
     Startup, Advance1, fViewer, drivers, editor, Advance;

type
       PEditSaver = ^TEditSaver;
       TEditSaver = object(TObject)
          constructor Load(var S: TStream);
          procedure Store(var S: TStream);
       end;

const
      REditSaver: TStreamRec = (
       ObjType: 12335;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(TEditSaver){$IFDEF OFFS}^{$ENDIF});
       Load: @TEditSaver.Load;
       Store: @TEditSaver.Store);

       Registered: Boolean = False;

constructor TEditSaver.Load(var S: TStream);
begin
  S.Read(MaxCommands, SizeOf(MaxCommands));
  S.Read(EditCommands, SizeOf(TEditCommand)*MaxCommands);
end;

procedure TEditSaver.Store(var S: TStream);
begin
  S.Write(MaxCommands, SizeOf(MaxCommands));
  S.Write(EditCommands, SizeOf(TEditCommand)*MaxCommands);
end;

procedure LoadCommands;
  var P: PEditSaver;
begin
  if MaxCommands = 0 then
    begin
      if not Registered then RegisterType(REditSaver);
      Registered := True;
      P := PEditSaver(LoadResource(dlgEditorCommands));
      P^.Free;
    end;
end;

constructor TEditWindow.Load(var S: TStream);
  var PI: PMenuItem;
      R: TRect;
begin
 inherited Load(S);
 GetSubViewPtr(S, Intern);
 GetSubViewPtr(S, AInfo);
 GetSubViewPtr(S, ABookLine); {-$VIV}
{ GetSubViewPtr(S, MenuBar);} (*X-Man*)
 GetExtent(R);
 R.Grow(-1, -1);
 R.B.Y := R.A.Y + 1;
 MenuBar := PMenuBar(LoadResource(dlgEditorMenu));
 if MenuBar <> nil then MenuBar^.Locate(R);
 Insert(MenuBar);
 UpMenu := MenuBar^.Menu;

  PI := MenuBar^.Menu^.Items;
  while (PI <> nil) and (PI^.HelpCtx <> hcedOptions) do
      PI := PI^.Next;
  if (PI <> nil) then PI := Pointer(PI^.SubMenu);
  PFileEditor(Intern)^.OptMenu := Pointer(Pi);
  if Title<>nil then DisposeStr(Title);
  if PFileEditor(Intern)^.SmartPad then begin
    Title := NewStr('SmartPad(TM) - ' + PFileEditor(Intern)^.EditName);
  end else if PFileEditor(Intern)^.ClipBrd then begin
    Title := NewStr('Clipboard');
  end else Title := NewStr(GetString(dlEditTitle) + ' - ' + PFileEditor(Intern)^.EditName);
 LoadCommands;
end;

procedure TEditWindow.ChangeBounds;
var rr: TRect;
begin
 inherited ChangeBounds(R);
 Intern^.HScroll^.GetBounds(rr); RR.B.X:=Size.X - 2;
 Intern^.HScroll^.SetBounds(rr);
 if not (Intern^.SmartPad or GetState(sfModal)) then
    begin
      GetBounds(TempBounds);
      LastEditDeskSize := Desktop^.Size;
    end;
end;

function TEditWindow.Execute;
 var Event: TEvent;
begin
  ModalEnd := False;
  repeat
    GetEvent(Event);
    if Event.What <> evNothing then HandleEvent(Event);
  until ModalEnd;
end;

procedure TEditWindow.Store(var S: TStream);
begin
 inherited Store(S);
 PutSubViewPtr(S, Intern);
 PutSubViewPtr(S, AInfo);
 PutSubViewPtr(S, ABookLine); {-$VIV}
{ PutSubViewPtr(S, MenuBar);} (*X-Man*)
end;

{ TEditWindow }
constructor TEditWindow.Init;
var
  pm: PMenu;
  Pi: PMenuItem;
begin
  inherited Init(R, '', 0);
  LoadCommands;
  Options := Options or ofTileable;
  Flags := Flags or wfMaxi;

  GetExtent(R);
  R.Grow(-1, -1);
  R.B.Y := R.A.Y + 1;
  MenuBar := PMenuBar(LoadResource(dlgEditorMenu));
  if MenuBar <> nil then MenuBar^.Locate(R);
  Insert(MenuBar);

  {MenuBar^.Options := MenuBar^.Options or ofPostProcess;}
  GetExtent(R);
  R.Grow(-1, -1);
  Inc(R.A.Y);

  Intern:=New(PXFileEditor, Init(R,
    MakeScrollBar(sbHorizontal + sbHandleKeyboard),
    MakeScrollBar(sbVertical + sbHandleKeyboard), FileName));

  {FreeObject(Intern);}

  PI := MenuBar^.Menu^.Items;
  while (PI <> nil) and (PI^.HelpCtx <> hcedOptions) do
      PI := PI^.Next;
  if (PI <> nil) then PI := Pointer(PI^.SubMenu);
  PFileEditor(Intern)^.OptMenu := Pointer(Pi);

  Insert(Intern);
  MILoadFile(Intern, FileName);
  if not Intern^.IsValid then
  begin
     Done;
     Fail;
  end;
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  R.A.X := 2;
  Dec(R.B.X, 2);
  AInfo := New(PInfoLine, Init(R));
  InsertBefore(AInfo, First);
  GetExtent(R);
  R.B.X := R.A.X + 1;
  Inc(R.A.Y, 2);
  Dec(R.B.Y);
  ABookLine := New(PBookmarkLine, Init(R));
  ABookLine^.GrowMode := gfGrowHiY;
  Insert(ABookLine);

  Intern^.InfoL:=AInfo;
  Intern^.BMrk:=ABookLine;
end;

procedure TEditWindow.SetState;
begin
 inherited SetState(AState,Enable);
 Redraw;
end;

{--- start -------- Eugeny Zvyagintzev ---- 23-12-2003 ----}
procedure TEditWindow.ChangeLanguage;
Var
  r: TRect;
  Pi: PMenuItem;
begin
  If MenuBar <> Nil Then Dispose(MenuBar,Done);
  GetExtent(R);
  R.Grow(-1, -1);
  R.B.Y := R.A.Y + 1;
  MenuBar := PMenuBar(LoadResource(dlgEditorMenu));
  if MenuBar <> nil then MenuBar^.Locate(R);
  Insert(MenuBar);

  PI := MenuBar^.Menu^.Items;
  while (PI <> nil) and (PI^.HelpCtx <> hcedOptions) do
      PI := PI^.Next;
  if (PI <> nil) then PI := Pointer(PI^.SubMenu);
  PFileEditor(Intern)^.OptMenu := Pointer(Pi);

  if Title<>nil then DisposeStr(Title);
  if PFileEditor(Intern)^.SmartPad then begin
    Title := NewStr('SmartPad(TM) - ' + PFileEditor(Intern)^.EditName);
  end else if PFileEditor(Intern)^.ClipBrd then begin
    Title := NewStr('Clipboard');
  end else Title := NewStr(GetString(dlEditTitle) + ' - ' + PFileEditor(Intern)^.EditName);
end;
{--- finish -------- Eugeny Zvyagintzev ---- 23-12-2003 ----}

END.
