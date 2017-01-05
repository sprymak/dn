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

unit Menus;

{-----------------------------------------------------}
{ This module is based on Turbo Vision Menus Unit     }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}


interface

uses
  Objects, Drivers, Views;

const

{ Color palettes }

  CMenuView   = #2#3#4#5#6#7;
  CStatusLine = #2#3#4#5#6#7;

type

{ TMenu types }

  TMenuStr = string[81];

  PMenu = ^TMenu;

  PMenuItem = ^TMenuItem;
  TMenuItem = record
    Next: PMenuItem;
    Name: PString;
    Command: Word;
    Disabled: Boolean;
    KeyCode: Word;
    HelpCtx: Word;
    case Integer of
      0: (Param: PString);
      1: (SubMenu: PMenu);
  end;

  TMenu = record
    Items: PMenuItem;
    Default: PMenuItem;
  end;

{ TMenuView object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuView = ^TMenuView;
  TMenuView = object(TView)
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    constructor Init(var Bounds: TRect);
    constructor Load(var S: TStream);
    function Execute: Word; virtual;
    function FindItem(Ch: Char): PMenuItem;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
    function GetHelpCtx: Word; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function HotKey(KeyCode: Word): PMenuItem;
    function NewSubView(var Bounds: TRect; AMenu: PMenu;
      AParentMenu: PMenuView): PMenuView; virtual;
    procedure Store(var S: TStream);
  end;

{ TMenuBar object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuBar = ^TMenuBar;
  TMenuBar = object(TMenuView)
    constructor Init(var Bounds: TRect; AMenu: PMenu);
    destructor Done; virtual;
    procedure Draw; virtual;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
  end;

{ TMenuBox object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuBox = ^TMenuBox;
  TMenuBox = object(TMenuView)
    TopItem: PMenuItem;
    constructor Init(var Bounds: TRect; AMenu: PMenu;
      AParentMenu: PMenuView);
    procedure Draw; virtual;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
  end;

{ TMenuPopup object }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PMenuPopup = ^TMenuPopup;
  TMenuPopup = object(TMenuBox)
    constructor Init(var Bounds: TRect; AMenu: PMenu);
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

{ TStatusItem }

  PStatusItem = ^TStatusItem;
  TStatusItem = record
    Next: PStatusItem;
    Text: PString;
    KeyCode: Word;
    Command: Word;
  end;

{ TStatusDef }

  PStatusDef = ^TStatusDef;
  TStatusDef = record
    Next: PStatusDef;
    Min, Max: Word;
    Items: PStatusItem;
  end;

{ TStatusLine }

  { Palette layout }
  { 1 = Normal text }
  { 2 = Disabled text }
  { 3 = Shortcut text }
  { 4 = Normal selection }
  { 5 = Disabled selection }
  { 6 = Shortcut selection }

  PStatusLine = ^TStatusLine;
  TStatusLine = object(TView)
    Items: PStatusItem;
    Defs: PStatusDef;
    constructor Init(var Bounds: TRect; ADefs: PStatusDef);
    constructor Load(var S: TStream);
    destructor Done; virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function Hint(AHelpCtx: Word): String; virtual;
    procedure Store(var S: TStream);
    procedure Update; virtual;
  private
    procedure DrawSelect(Selected: PStatusItem);
    procedure FindItems;
  end;

{ TMenuItem routines }

function NewItem(Name, Param: TMenuStr; KeyCode: Word; Command: Word;
  AHelpCtx: Word; Next: PMenuItem): PMenuItem;
function NewLine(Next: PMenuItem): PMenuItem;
function NewSubMenu(Name: TMenuStr; AHelpCtx: Word; SubMenu: PMenu;
  Next: PMenuItem): PMenuItem;

{ TMenu routines }

function NewMenu(Items: PMenuItem): PMenu;
procedure DisposeMenu(Menu: PMenu);

{ TStatusLine routines }

function NewStatusDef(AMin, AMax: Word; AItems: PStatusItem;
                      ANext: PStatusDef): PStatusDef;
function NewStatusKey(const AText: String; AKeyCode: Word; ACommand: Word;
                      ANext: PStatusItem): PStatusItem;

const
  dfByCommand = $000;
  dfByPosition = $100;

function LookUpMenu(Menu: PMenu; idCheckItem: Word; Flags: Word): PMenuItem;
function MenuIndexOf(Menu: PMenu; idCheckItem: PMenuItem): Word;

procedure StoreMenuDefaults(Menu: PMenu; var S: TStream);
procedure LoadMenuDefaults(Menu: PMenu; var S: TStream);

Const MenuActive: Boolean = false;

implementation
uses advance, advance1, advance2, advance3, Commands,DnHelp,DnApp;

const OldKbdState: Byte = 0;

{ TMenuItem routines }

function NewItem(Name, Param: TMenuStr; KeyCode: Word; Command: Word;
  AHelpCtx: Word; Next: PMenuItem): PMenuItem;
const
  T: PView = nil;
var
  P: PMenuItem;
begin
  if (Name <> '') and (Command <> 0) then
  begin
    New(P);
    P^.Next := Next;
    P^.Name := NewStr(Name);
    P^.Command := Command;
    P^.Disabled := not T^.MenuEnabled(Command); {-$VOL}
    P^.KeyCode := KeyCode;
    P^.HelpCtx := AHelpCtx;
    P^.Param := NewStr(Param);
    NewItem := P;
  end else
  NewItem := Next;
end;

function NewLine(Next: PMenuItem): PMenuItem;
var
  P: PMenuItem;
begin
  New(P); FillChar(P^, SizeOf(P^), 0);
  P^.Next := Next;
  P^.Name := nil;
  P^.HelpCtx := hcNoContext;
  NewLine := P;
end;

function NewSubMenu(Name: TMenuStr; AHelpCtx: Word; SubMenu: PMenu;
  Next: PMenuItem): PMenuItem;
var
  P: PMenuItem;
begin
  if (Name <> '') and (SubMenu <> nil) then
  begin
    New(P);
    P^.Next := Next;
    P^.Name := NewStr(Name);
    P^.Command := 0;
    P^.KeyCode := 0;
    P^.Disabled := False;
    P^.HelpCtx := AHelpCtx;
    P^.SubMenu := SubMenu;
    NewSubMenu := P;
  end else
  NewSubMenu := Next;
end;

{ TMenu routines }

function NewMenu(Items: PMenuItem): PMenu;
var
  P: PMenu;
begin
  New(P);
  P^.Items := Items;
  P^.Default := Items;
  NewMenu := P;
end;

procedure DisposeMenu(Menu: PMenu);
var
  P, Q: PMenuItem;
begin
  if Menu <> nil then
  begin
    P := Menu^.Items;
    while P <> nil do
    begin
      if P^.Name <> nil then
      begin
        DisposeStr(P^.Name);
        if P^.Command <> 0 then
          DisposeStr(P^.Param) else
          DisposeMenu(P^.SubMenu);
      end;
      Q := P;
      P := P^.Next;
      Dispose(Q);
    end;
    Dispose(Menu);
  end;
end;

{ TMenuView }

constructor TMenuView.Init(var Bounds: TRect);
begin
  Bounds.B.X := Bounds.A.X; {new}
  Bounds.B.Y := Bounds.A.Y; {new}
  TView.Init(Bounds);
  EventMask := EventMask or evBroadcast;
end;

constructor TMenuView.Load(var S: TStream);

function DoLoadMenu: PMenu;
var
  Item: PMenuItem;
  Last: ^PMenuItem;
  Menu: PMenu;
  Tok: Byte;
begin
  New(Menu);
  Last := @Menu^.Items;
  Item := nil;
  S.Read(Tok,1);
  while Tok <> 0 do
  begin
    New(Item);
    Last^ := Item;
    Last := @Item^.Next;
    with Item^ do
    begin
      Name := S.ReadStr;
      S.Read(Command, SizeOf(Command));
      S.Read(Disabled, SizeOf(Disabled));
      S.Read(KeyCode, SizeOf(KeyCode));
      S.Read(HelpCtx, SizeOf(HelpCtx));
      if (Name <> nil) then
        if Command = 0 then SubMenu := DoLoadMenu
        else Param := S.ReadStr;
    end;
    S.Read(Tok, 1);
  end;
  Last^ := nil;
  Menu^.Default := Menu^.Items;
  DoLoadMenu := Menu;
end;

begin
  TView.Load(S);
  Menu := DoLoadMenu;
end;

function TMenuView.Execute: Word;
type
  MenuAction = (DoNothing, DoSelect, DoReturn);
var
  AutoSelect: Boolean;
  Action: MenuAction;
  Ch: Char;
  ItemShown, P: PMenuItem;
  Target: PMenuView;
  R: TRect;
  E: TEvent;
  MouseActive: Boolean;
  MouseWalk: Boolean;
  Going: Boolean; {new}

procedure TrackKey(FindNext: Boolean); forward;

procedure TrackMouse;
var
  Mouse: TPoint;
  R: TRect;
label q;
begin
  MakeLocal(E.Where, Mouse);
  if Size.Y>1 then begin            {new begin}
   if Mouse.Y=0 then begin
    if not Going then begin
     Going := true;
     TrackKey(False);
    end else begin
     TrackKey(False);
     if (Current<>nil) and (Current^.Next = nil) then TrackKey(true);
    end
   end else if Mouse.Y>=Size.Y-1 then begin
    if not Going then begin
     Going := true;
     TrackKey(True);
    end else begin
     TrackKey(True);
     if Current = Menu^.Items then TrackKey(False);
    end
   end else goto q;             {new end}
   AutoSelect := false;
   Action := DoNothing;
   MouseWalk:=true;
   MouseActive:=true;
   exit;
  end;
q:
  MouseWalk:=false;
  Current := Menu^.Items;
  while Current <> nil do
  begin
    GetItemRect(Current, R);
    if R.Contains(Mouse) then
    begin
      MouseActive := True;
      Exit;
    end;
    Current := Current^.Next;
  end;
end;

procedure TrackKey(FindNext: Boolean);

procedure NextItem;
begin
  Current := Current^.Next;
  if Current = nil then Current := Menu^.Items;
end;

procedure PrevItem;
var
  P: PMenuItem;
begin
  P := Current;
  if P = Menu^.Items then P := nil;
  repeat NextItem until Current^.Next = P;
end;

begin
  if Current <> nil then
    repeat
      if FindNext then NextItem else PrevItem;
    until Current^.Name <> nil;
end;

function MouseInOwner: Boolean;
var
  Mouse: TPoint;
  R: TRect;
begin
  if (ParentMenu <> nil) and (ParentMenu^.Size.Y = 1) then begin
   ParentMenu^.MakeLocal(E.Where, Mouse);
   ParentMenu^.GetItemRect(ParentMenu^.Current, R);
   MouseInOwner := R.Contains(Mouse);
  end else MouseInOwner := False;
end;

function MouseInMenus: Boolean;
var
  P: PMenuView;
begin
  P := ParentMenu;
  while (P <> nil) and not P^.MouseInView(E.Where) do P := P^.ParentMenu;
  MouseInMenus := P <> nil;
end;

function TopMenu: PMenuView;
var
  P: PMenuView;
begin
  P := @Self;
  while P^.ParentMenu <> nil do P := P^.ParentMenu;
  TopMenu := P;
end;

var Frst: Boolean;

var
  W, H, L: Integer;
  R2: TRect;
begin
 If TypeOf(Self)=TypeOf(TMenuBox) then begin
  W := 10;
  H := 2;
  if Menu <> nil then begin
    P := Menu^.Items;
    while P <> nil do begin
      if P^.Name <> nil then begin
        L := CStrLen(P^.Name^) + 6;
        if P^.Command = 0 then Inc(L, 3) else
          if P^.Param <> nil then Inc(L, CStrLen(P^.Param^) + 2);
        if L > W then W := L;
      end;
      Inc(H);
      P := P^.Next;
    end;
  end;
  GetBounds(R);
  Owner^.GetExtent(R2);                      {new begin}
  if Options and ofCenterX <> 0 then begin
   R.A.X := (R2.B.X - R2.A.X - W) div 2;
  end;
  R.B.X := R.A.X + W;

  if Options and ofCenterY <> 0 then begin
   R.A.Y := (R2.B.Y - R2.A.Y - H) div 2;
  end;
  R.B.Y := R.A.Y + H;

  if R.B.Y > R2.B.Y then begin
   R.A.Y := R2.B.Y-H;
   If R.A.Y < 0 then R.A.Y:=0;
   R.B.Y := R.A.Y + H;
   if R.B.Y > R2.B.Y then R.B.Y := R2.B.Y;
  end;
  if R.B.X > R2.B.X then begin
   R.A.X := R2.B.X - (R.B.X - R.A.X);
   R.B.X := R2.B.X;
  end;
  SetBounds(R);
  Owner^.Redraw;
 end;
  if TypeOf(Self) = TypeOf(TMenuBar) then Message(Owner, evCommand, cmMenuOn, @Self);
  AutoSelect := False;
  Result := 0;
  ItemShown := nil;
  Current := Menu^.Default;
  MouseActive := False;
  Frst := False;
  repeat
    MenuActive:=True;           {new end}
    Action := DoNothing;
    GetEvent(E);
    case E.What of
      evMouseDown:
        if (MouseInView(E.Where) or MouseInOwner) then
        begin
          TrackMouse;
          if Size.Y = 1 then AutoSelect := True;
        end else Action := DoReturn;
      evMouseUp:
         if not MouseWalk then begin
          TrackMouse;
          if MouseInOwner
           then Current := Menu^.Default
           else if (Current <> nil) and (Current^.Name <> nil)
                 then Action := DoSelect
                 else If (Current <> nil) and (Current^.Name = nil)
                       then Action := DoNothing
                       else if MouseActive
                             then Action := DoReturn
                             else begin
                              Current := Menu^.Default;
                              if Current = nil then Current := Menu^.Items;
                              Action := DoNothing;
                             end;
         end else Going := false;     {new}
      evMouseMove:
        if (E.Buttons <> 0) then
        begin
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus then Action := DoReturn;
        end;
      evMouseAuto:
        if (E.Buttons <> 0) then
        begin
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus then Action := DoReturn;
        end;
      evKeyDown:
        case E.KeyCode of                                    {new begin}
         kbDownUp, kbUpUp: if Going then Going:=false;
         else
        case {CtrlToArrow}(E.KeyCode) of
          kbUp, kbDown:
            if Size.Y <> 1 then begin
              if {CtrlToArrow}(E.KeyCode) = kbDown then
               if not Going then begin
                Going := true;
                TrackKey(True);
               end else begin
                TrackKey(True);
               {if Current = Menu^.Items then TrackKey(False);} {JO - зачем здесь это???}
               end
              else
               if not Going then begin
                Going := true;
                TrackKey(False);
               end else begin
                TrackKey(False);
               {if Current^.Next = nil then TrackKey(true);} {JO - зачем здесь это???}
               end;
            end else if E.KeyCode = kbDown then AutoSelect := True;    {new end}
          kbLeft, kbRight:
            if ParentMenu = nil then
              TrackKey({CtrlToArrow}(E.KeyCode) = kbRight) else
              Action := DoReturn;
          kbHome, kbEnd:
            if Size.Y <> 1 then
            begin
              Current := Menu^.Items;
              if E.KeyCode = kbEnd then TrackKey(False);
            end;
          kbPgUp:                                  {new begin}
            if Size.Y <> 1 then
            begin
             For W := 1 to Size.Y-2 do
              if Current = Menu^.Items then break
              else begin
               P := Current;
               Current := Menu^.Items;
               repeat
                Current := Current^.Next;
                if Current = nil then Current := Menu^.Items;
               until Current^.Next = P;
              end;
             while Current^.Name = nil do begin
              Current := Current^.Next;
              if Current = nil then Current := Menu^.Items;
             end;
            end;
          kbPgDn:
            if Size.Y <> 1 then
            begin
             For W := 1 to Size.Y-2 do
              if (Current = Menu^.Items) {and (W>1)} {JO - зачем здесь это???}
               then begin TrackKey(False); break end
               else begin
                     Current := Current^.Next;
                     if Current = nil then Current := Menu^.Items;
                    end;
             if Current = nil then Current := Menu^.Items;
             while Current^.Name = nil do begin
              Current := Current^.Next;
              if Current = nil then Current := Menu^.Items;
             end;
            end;                                  {new end}
          kbEnter, kbShiftEnter:
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
            end;
          kbDoubleAlt:
            begin
              if not Frst then begin Frst := True; ClearEvent(E) end
               else
                begin
                 Action := DoReturn;
                 if (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then ClearEvent(E);
                end;
            end;
          kbEsc:
            begin
              Action := DoReturn;
              if (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then
                ClearEvent(E);
            end;
        else
          Target := @Self;
          Ch := GetAltChar(E.KeyCode);
          if Ch = #0 then Ch := E.CharCode else Target := TopMenu;
          P := Target^.FindItem(Ch);
          if ( Target = @Self ) and ( P = nil )
            then P := FindItem( GetAltChar( E.KeyCode and $FFFF00 ));
          if P = nil then
          begin
            P := TopMenu^.HotKey(E.KeyCode);
            if (P <> nil) and CommandEnabled(P^.Command) then
            begin
              Result := P^.Command;
              Action := DoReturn;
            end
          end else
            if Target = @Self then
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
              Current := P;
            end else
              if (ParentMenu <> Target) or (ParentMenu^.Current <> P) then
                Action := DoReturn;
        end end;                                  {new}
      evCommand:
        if (E.Command = cmMenu) or (E.Command = cmMainMenu) then
        begin
          AutoSelect := False;
          if not Frst then begin Frst := True; ClearEvent(E) end
           else
            begin
             Action := DoReturn;
             if (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then ClearEvent(E);
            end;
          {if ParentMenu <> nil then Action := DoReturn;}
        end else
          Action := DoReturn;
     end;
    if ItemShown <> Current then
    begin
      ItemShown := Current;
      DrawView;
    end;
    if (Action = DoSelect) or ((Action = DoNothing) and AutoSelect) then
      if Current <> nil then with Current^ do if Name <> nil then
        if Command = 0 then
        begin
          if E.What and (evMouseDown + evMouseMove) <> 0 then PutEvent(E);
          GetItemRect(Current, R);
          R.A.X := R.A.X + Origin.X;
          R.A.Y := R.B.Y + Origin.Y;
          R.B := Owner^.Size;
          if Size.Y = 1 then Dec(R.A.X);
          Target := TopMenu^.NewSubView(R, SubMenu, @Self);
          Result := Owner^.ExecView(Target);
          Dispose(Target,Done);
        end else if Action = DoSelect then Result := Command;
    if (Result <> 0) and CommandEnabled(Result) then
    begin
      Action := DoReturn;
      ClearEvent(E);
    end;
  until Action = DoReturn;
  if E.What <> evNothing then
    if (ParentMenu <> nil) or (E.What = evCommand) then PutEvent(E);
  if Current <> nil then
  begin
    Menu^.Default := Current;
    Current := nil;
    DrawView;
  end;
  if TypeOf(Self) = TypeOf(TMenuBar) then Message(Owner, evCommand, cmMenuOff, @Self);
  MenuActive:=False;
end;

function TMenuView.FindItem(Ch: Char): PMenuItem;
var
  P: PMenuItem;
  I: Integer;
begin
  Ch := UpCaseArray[Ch];
  P := Menu^.Items;
  while P <> nil do
  begin
    if (P^.Name <> nil) and not P^.Disabled then
    begin
      I := Pos('~', P^.Name^);
      if (I <> 0) and (Ch = UpCaseArray[P^.Name^[I + 1]]) then
      begin
        FindItem := P;
        Exit;
      end;
    end;
    P := P^.Next;
  end;
  FindItem := nil;
end;

procedure TMenuView.GetItemRect(Item: PMenuItem; var R: TRect);
begin
end;

function TMenuView.GetHelpCtx: Word;
var
  C: PMenuView;
label
  Loop;
begin
  C := @Self;
  GetHelpCtx := hcNoContext;
Loop:
  If C = NIL then Exit;
  If ( C^.Current <> NIL ) and ( C^.Current^.HelpCtx <> hcNoContext ) then begin
    GetHelpCtx := C^.Current^.HelpCtx;
    Exit;
  end;
  GetHelpCtx := C^.HelpCtx;
  C := C^.ParentMenu;
  goto Loop;
end;

function TMenuView.GetPalette: PPalette;
const
  P: string[Length(CMenuView)] = CMenuView;
begin
  GetPalette := @P;
end;

procedure TMenuView.HandleEvent(var Event: TEvent);
var
  CallDraw: Boolean;
  P: PMenuItem;

procedure UpdateMenu(Menu: PMenu);
var
  P: PMenuItem;
  CommandState: Boolean;
begin
  P := Menu^.Items;
  while P <> nil do
  begin
    if P^.Name <> nil then
      if P^.Command = 0 then UpdateMenu(P^.SubMenu)
      else
      begin
        CommandState := MenuEnabled(P^.Command);
        if P^.Disabled = CommandState then
        begin
          P^.Disabled := not CommandState;
          CallDraw := True;
        end;
      end;
    P := P^.Next;
  end;
end;

procedure DoSelect;
begin
  PutEvent(Event);
  Event.Command := Owner^.ExecView(@Self);
  if (Event.Command <> 0) and CommandEnabled(Event.Command) then
  begin
    Event.What := evCommand;
    Event.InfoPtr := nil;
    PutEvent(Event);
  end;
  ClearEvent(Event);
end;

var q: TPoint;
begin
  if Menu <> nil then
    case Event.What of
      evMouseDown: DoSelect;
      evKeyDown:
{ AK155 9-07-2002 То, что каждый символ протаскивается через все меню,
не добавляет скорости. Реально тут хоткеи обрабатываются только для
неактивного меню, поэтому имеет смысл анализировать только клавиши
с Ctrl или Alt, а также некоторые с Shift (те, которые не светятся
в StatusLine). Hо поскольку выискивать все шифтовые клавиши не хочется,
пусть проверяются все. Реально есть, как минимум, такие:
GrayPlus, GrayMinus, F2, F10, F11, F12 }
        if ((Event.KeyCode and $F0000) <> 0){AK155} and
         (FindItem(GetAltChar(Event.KeyCode)) <> nil) then
          DoSelect
        else
        begin
          P := HotKey(Event.KeyCode);
          if (P <> nil) and CommandEnabled(P^.Command) then
          begin
            Event.What := evCommand;
            Event.Command := P^.Command;
            Event.InfoPtr := nil;
            PutEvent(Event);
            ClearEvent(Event);
          end;
        end;
      evCommand:
        if Event.Command = cmMenu then DoSelect;
      evBroadcast:
        if Event.Command = cmCommandSetChanged then
        begin
          CallDraw := False;
          UpdateMenu(Menu);
          if CallDraw then DrawView;
        end;
    end;
end;

function TMenuView.HotKey(KeyCode: Word): PMenuItem;

function FindHotKey(P: PMenuItem): PMenuItem;
var
  T: PMenuItem;
begin
  while P <> nil do
  begin
    if P^.Name <> nil then
      if P^.Command = 0 then
      begin
        T := FindHotKey(P^.SubMenu^.Items);
        if T <> nil then
        begin
          FindHotKey := T;
          Exit;
        end;
      end
      else if not P^.Disabled and (P^.KeyCode <> kbNoKey) and
        (P^.KeyCode = KeyCode) then
      begin
        FindHotKey := P;
        Exit;
      end;
    P := P^.Next;
  end;
  FindHotKey := nil;
end;

begin
  HotKey := FindHotKey(Menu^.Items);
end;

function TMenuView.NewSubView(var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView): PMenuView;
begin
  NewSubView := New(PMenuBox, Init(Bounds, AMenu, AParentMenu));
end;

procedure TMenuView.Store(var S: TStream);

procedure DoStoreMenu(Menu: PMenu);
var
  Item: PMenuItem;
  Tok: Byte;
begin
  Tok := $FF;
  Item := Menu^.Items;
  while Item <> nil do
  begin
    with Item^ do
    begin
      S.Write(Tok, 1);
      S.WriteStr(Name);
      S.Write(Command, SizeOf(Command));
      S.Write(Disabled, SizeOf(Disabled));
      S.Write(KeyCode, SizeOf(KeyCode));
      S.Write(HelpCtx, SizeOf(HelpCtx));
      if (Name <> nil) then
        if Command = 0 then DoStoreMenu(SubMenu)
        else S.WriteStr(Param);
    end;
    Item := Item^.Next;
  end;
  Tok := 0;
  S.Write(Tok, 1);
end;

begin
  TView.Store(S);
  DoStoreMenu(Menu);
end;

{ TMenuBar }

constructor TMenuBar.Init(var Bounds: TRect; AMenu: PMenu);
var
  R: TRect;
begin
  TMenuView.Init(Bounds);
  GrowMode := gfGrowHiX;
  Menu := AMenu;
  Options := Options or ofPreProcess;
end;

destructor TMenuBar.Done;
begin
  TMenuView.Done;
  DisposeMenu(Menu);
end;

procedure TMenuBar.Draw;
var
  X, L: Integer;
  CNormal, CSelect, CNormDisabled, CSelDisabled, Color: Word;
  P: PMenuItem;
  B: TDrawBuffer;
begin
  CNormal := GetColor($0301);
  CSelect := GetColor($0604);
  CNormDisabled := GetColor($0202);
  CSelDisabled := GetColor($0505);
  MoveChar(B, ' ', Byte(CNormal), Size.X);
  if Menu <> nil then
  begin
    X := 1;
    P := Menu^.Items;
    while P <> nil do
    begin
      if P^.Name <> nil then
      begin
        L := CStrLen(P^.Name^);
        if X + L < Size.X then
        begin
          if P^.Disabled then
            if P = Current then
              Color := CSelDisabled else
              Color := CNormDisabled else
            if P = Current then
              Color := CSelect else
              Color := CNormal;
          MoveChar(B[X], ' ', Byte(Color), 1);
          MoveCStr(B[X + 1], P^.Name^, Color);
          MoveChar(B[X + L + 1], ' ', Byte(Color), 1);
        end;
        Inc(X, L + 2);
      end;
      P := P^.Next;
    end;
  end;
  WriteBuf(0, 0, Size.X, 1, B);
end;

procedure TMenuBar.GetItemRect(Item: PMenuItem; var R: TRect);
var
  P: PMenuItem;
begin
  R.Assign(1, 0, 1, 1);
  P := Menu^.Items;
  while True do
  begin
    R.A.X := R.B.X;
    if P^.Name <> nil then Inc(R.B.X, CStrLen(P^.Name^)+2);
    if P = Item then Exit;
    P := P^.Next;
  end;
end;

{ TMenuBox }

constructor TMenuBox.Init(var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView);
begin
  TMenuView.Init(Bounds);
  State := State or sfShadow;
  Options := Options or ofPreProcess;
  Menu := AMenu;
  ParentMenu := AParentMenu;
  If Menu <> nil then TopItem := Menu^.Items;
end;

procedure TMenuBox.Draw;
var
  CNormal, CSelect, CNormDisabled, CSelDisabled, Color: Word;
  Y: Integer;
  P: PMenuItem;
  B: TDrawBuffer;
  TopIndex: Integer;

Function PastEnd: boolean;
var P: PMenuItem;
    H: integer;
begin
 H := 1;
 P := TopItem;
 while (P <> nil) and (H < Size.Y) do begin
  Inc (H);
  P := P^.Next;
 end;
 PastEnd := H > Size.Y - 1;
end;

procedure FrameLine(N: Integer);
const
  FrameChars: array[0..19] of Char = #32#218#196#191#32#32#192#196#217#32#32#179#32#179#32#32#195#196#180#32;
                                    {'  |'' --  ''|       |.. --  ..|       |      |         |-  --  -|     '}
  UpArr: array[0..2] of char = #32#30#32;{ ^ }
  DnArr: array[0..2] of char = #32#31#32;{ v }
begin
  MoveBuf(B[0], FrameChars[N], Byte(CNormal), 2);
  MoveChar(B[2], FrameChars[N + 2], Byte(Color), Size.X - 4);
  MoveBuf(B[Size.X - 2], FrameChars[N + 3], Byte(CNormal), 2);
  if (N = 0) and (TopItem <> Menu^.Items)
   then MoveBuf(B[4], UpArr, Byte(Color), 3)
   else if (N = 5) and (PastEnd) then MoveBuf(B[4], DnArr, Byte(Color), 3);
end;

procedure DrawLine;
begin
  WriteBuf(0, Y, Size.X, 1, B);
  Inc(Y);
end;

{--- start -------- Eugeny Zvyagintzev ---- 28-09-2003 ----}
{Procedure was optimized: multiple recursion calls are removed}
begin
  CNormal := GetColor($0301);
  CSelect := GetColor($0604);
  CNormDisabled := GetColor($0202);
  CSelDisabled := GetColor($0505);
  Y := 0;
  Color := CNormal;
  FrameLine(0);
  DrawLine;

  if (Menu <> nil) And (Menu^.Items <> Nil) then
  begin
    P:=Menu^.Items;
    If TopItem = Nil Then TopItem:=Menu^.Items;
    TopIndex:=MenuIndexOf(Menu,TopItem);
    If Current <> Nil Then Y:=MenuIndexOf(Menu,Current)
    Else Y:=0;
    If TopIndex > Y Then TopIndex:=Y;
    If (Y - TopIndex) >= Size.Y-2 Then TopIndex:=Y-Size.Y+3;
    TopItem:=LookUpMenu(Menu,TopIndex,dfByPosition);
    Y:=0; FrameLine(0); DrawLine;

    P := TopItem;
    Y:=1;
    while (Y < Size.Y-1) do
    begin
      Color := CNormal;
      if P^.Name = nil then FrameLine(15) else
      begin
        if P^.Disabled then
          if P = Current then
            Color := CSelDisabled else
            Color := CNormDisabled else
          if P = Current then Color := CSelect;
        FrameLine(10);
        MoveCStr(B[3], P^.Name^, Color);
        if P^.Command = 0 then
          MoveChar(B[Size.X - 4], #16, Byte(Color), 1) else
          if P^.Param <> nil then
            MoveStr(B[Size.X - 3 - Length(P^.Param^)],
              P^.Param^, Byte(Color));
      end;
      DrawLine;
      P := P^.Next;
    end;
  end;
  Color := CNormal;
  FrameLine(5);
  DrawLine;
end;
{--- finish -------- Eugeny Zvyagintzev ---- 28-09-2003 ----}

procedure TMenuBox.GetItemRect(Item: PMenuItem; var R: TRect);
var
  Y: Integer;
  P: PMenuItem;
begin
  Y := 1;
  P := TopItem;
  while (P <> Item) And (P <> nil) do
  begin
    Inc(Y);
    P := P^.Next;
  end;
  If (P=nil) then R.Assign(0, 0, 0, 0)
             else R.Assign(2, Y, Size.X - 2, Y + 1);
end;

{TMenuPopup}

constructor TMenuPopup.Init(var Bounds: TRect; AMenu: PMenu);
begin
  inherited Init(Bounds, AMenu, nil);
end;

procedure TMenuPopup.HandleEvent(var Event: TEvent);
var
  P: PMenuItem;
begin
  case Event.What of
    evKeyDown:
      begin
        P := FindItem(GetCtrlChar(Event.KeyCode));
        if P = nil then
          P := HotKey(Event.KeyCode);
        if (P <> nil) and (CommandEnabled(P^.Command)) then
        begin
          Event.What := evCommand;
          Event.Command := P^.Command;
          Event.InfoPtr := nil;
          PutEvent(Event);
          ClearEvent(Event);
        end
        else
          if GetAltChar(Event.KeyCode) <> #0 then
            ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

{ TStatusLine }

constructor TStatusLine.Init(var Bounds: TRect; ADefs: PStatusDef);
begin
  TView.Init(Bounds);
  Options := Options or ofPreProcess;
  EventMask := EventMask or evBroadcast;
  GrowMode := gfGrowLoY + gfGrowHiX + gfGrowHiY;
  Defs := ADefs;
  FindItems;
end;

constructor TStatusLine.Load(var S: TStream);

function DoLoadStatusItems: PStatusItem;
var
  Count: AInt;
  Cur, First: PStatusItem;
  Last: ^PStatusItem;
begin
  Cur := nil;
  Last := @First;
  S.Read(Count, SizeOf(AInt));
  while Count > 0 do
  begin
    New(Cur);
    Last^ := Cur;
    Last := @Cur^.Next;
    Cur^.Text := S.ReadStr;
    S.Read(Cur^.KeyCode, SizeOf(Cur^.KeyCode));
    S.Read(Cur^.Command, SizeOf(Cur^.Command));
    Dec(Count);
  end;
  Last^ := nil;
  DoLoadStatusItems := First;
end;

function DoLoadStatusDefs: PStatusDef;
var
  AMin: Word;
  Cur, First: PStatusDef;
  Last: ^PStatusDef;
  Count: AInt;
begin
  Last := @First;
  S.Read(Count, SizeOf(AInt));
  while Count > 0 do
  begin
    New(Cur);
    Last^ := Cur;
    Last := @Cur^.Next;
    S.Read(Cur^.Min, 2 * SizeOf(Word));
    Cur^.Items := DoLoadStatusItems;
    Dec(Count);
  end;
  Last^ := nil;
  DoLoadStatusDefs := First;
end;

begin
  TView.Load(S);
  Defs := DoLoadStatusDefs;
  FindItems;
end;

destructor TStatusLine.Done;
var
  T: PStatusDef;

procedure DisposeItems(Item: PStatusItem);
var
  T: PStatusItem;
begin
  while Item <> nil do
  begin
    T := Item;
    Item := Item^.Next;
    DisposeStr(T^.Text);
    Dispose(T);
  end;
end;

begin
  while Defs <> nil do
  begin
    T := Defs;
    Defs := Defs^.Next;
    DisposeItems(T^.Items);
    Dispose(T);
  end;
  TView.Done;
end;

procedure TStatusLine.Draw;
begin
  DrawSelect(nil);
end;

procedure TStatusLine.DrawSelect(Selected: PStatusItem);
var
  B: TDrawBuffer;
  T: PStatusItem;
  I, L: Integer;
  CSelect, CNormal, CSelDisabled, CNormDisabled: Word;
  Color: Word;
  HintBuf: String;
  FirstChar: Char;
begin
  CNormal := GetColor($0301);
  CSelect := GetColor($0604);
  CNormDisabled := GetColor($0202);
  CSelDisabled := GetColor($0505);
  MoveChar(B, ' ', Byte(CNormal), Size.X);
  T := Items;
  I := 0;
  if OldKbdState and 12 =12 then FirstChar := '^' else        {new}
   if OldKbdState and 8 <> 0 then FirstChar := '-' else
    if OldKbdState and 4 <> 0 then FirstChar := '+' else
     if OldKbdState and 3 <> 0 then FirstChar := ':' else
      FirstChar := ' ';
  while T <> nil do
  begin
    if (T^.Text <> nil) and ((FirstChar = ' ') and not (T^.Text^[1] in [':','+','-','^'])
      or (T^.Text^[1] = FirstChar)) then
    begin
      L := CStrLen(T^.Text^) - Byte(FirstChar <> ' ');
      if I + L < Size.X then
      begin
        if CommandEnabled(T^.Command) then
          if T = Selected then
            Color := CSelect else
            Color := CNormal else
          if T = Selected then
            Color := CSelDisabled else
            Color := CNormDisabled;
        MoveChar(B[I], ' ', Byte(Color), 1);
        MoveCStr(B[I + 1], Copy(T^.Text^,1+Byte(FirstChar<>' '), MaxStringLength), Color);
        MoveChar(B[I + L + 1], ' ', Byte(Color), 1);
      end;
      Inc(I, L + 2);
    end;
    T := T^.Next;
  end;
  if I < Size.X - 2 then
  begin
    HintBuf := Hint(HelpCtx);
    if HintBuf <> '' then
    begin
      MoveChar(B[I], #179, Byte(CNormal), 1);
      Inc(I, 2);
      if I + Length(HintBuf) > Size.X then SetLength(HintBuf, Size.X - I);
      MoveStr(B[I], HintBuf, Byte(CNormal));
    end;
  end;
  WriteLine(0, 0, Size.X, 1, B);
end;

procedure TStatusLine.FindItems;
var
  P: PStatusDef;
begin
  P := Defs;
  while (P <> nil) and ((HelpCtx < P^.Min) or (HelpCtx > P^.Max)) do
    P := P^.Next;
  if P = nil then Items := nil else Items := P^.Items;
end;

function TStatusLine.GetPalette: PPalette;
const
  P: string[Length(CStatusLine)] = CStatusLine;
begin
  GetPalette := @P;
end;

procedure TStatusLine.HandleEvent(var Event: TEvent);
var
  Mouse: TPoint;
  T: PStatusItem;
  S: String[20];

function ItemMouseIsIn: PStatusItem;
var
  I,K: Word;
  T: PStatusItem;
  FirstChar: Char;
begin
  ItemMouseIsIn := nil;
  if Mouse.Y <> 0 then Exit;
  I := 0;
  T := Items;
  if OldKbdState and 12 =12 then FirstChar := '^' else       {new}
   if OldKbdState and 8 <> 0 then FirstChar := '-' else
     if OldKbdState and 4 <> 0 then FirstChar := '+' else
      if OldKbdState and 3 <> 0 then FirstChar := ':' else
     FirstChar := ' ';
  while T <> nil do
  begin
    if (T^.Text <> nil) and ((FirstChar = ' ') and not (T^.Text^[1] in [':','+','-','^'])
      or (T^.Text^[1] = FirstChar)) then
    begin
      K := I + CStrLen(T^.Text^) - Byte(FirstChar <> ' ') + 2;
      if (Mouse.X >= I) and (Mouse.X < K) then
      begin
        ItemMouseIsIn := T;
        Exit;
      end;
      I := K;
    end;
    T := T^.Next;
  end;
end;

begin
  TView.HandleEvent(Event);
  case Event.What of
    evMouseDown:
      begin
        T := nil;
        repeat
          MakeLocal(Event.Where, Mouse);
          if T <> ItemMouseIsIn then
          begin
            T := ItemMouseIsIn;
            DrawSelect(T);
          end;
        until not MouseEvent(Event, evMouseMove);
        if (T <> nil) and CommandEnabled(T^.Command) then
        begin
          Event.What := evCommand;
          Event.Command := T^.Command;
          Event.InfoPtr := nil;
          PutEvent(Event);
        end;
        ClearEvent(Event);
        DrawView;
      end;
    evKeyDown:
      begin
        T := Items;
        while T <> nil do
        begin
          if T^.Text <> nil then S := T^.Text^ else S := '';
          if (Event.KeyCode = T^.KeyCode) and
            CommandEnabled(T^.Command) or
            (T^.KeyCode = kbShortCut) and
            (UpCaseArray[S[PosChar('~', S)+1]] = UpCaseArray[Event.CharCode]) then
          begin
            Event.What := evCommand;
            Event.Command := T^.Command;
            Event.InfoPtr := nil;
            Exit;
          end;
          T := T^.Next;
        end;
      end;
    evBroadcast:
      if Event.Command = cmCommandSetChanged then DrawView;
  end;
end;

function TStatusLine.Hint(AHelpCtx: Word): String;
begin
  if AHelpCtx=hcTeam then Hint := GetString(dlTeamHint)
  else Hint := '';
end;

procedure TStatusLine.Store(var S: TStream);

procedure DoStoreStatusItems(Cur: PStatusItem);
var
  T: PStatusItem;
  Count: AInt;
begin
  Count := 0;
  T := Cur;
  while T <> nil do
  begin
    Inc(Count);
    T := T^.Next
  end;
  S.Write(Count, SizeOf(AInt));
  while Cur <> nil do
  begin
    S.WriteStr(Cur^.Text);
    S.Write(Cur^.KeyCode, SizeOf(Cur^.KeyCode));
    S.Write(Cur^.Command, SizeOf(Cur^.Command));
    Cur := Cur^.Next;
  end;
end;

procedure DoStoreStatusDefs(Cur: PStatusDef);
var
  Count: AInt;
  T: PStatusDef;
begin
  Count := 0;
  T := Cur;
  while T <> nil do
  begin
    Inc(Count);
    T := T^.Next
  end;
  S.Write(Count, SizeOf(AInt));
  while Cur <> nil do
  begin
    with Cur^ do
    begin
      S.Write(Min, SizeOf(Word) * 2);
      DoStoreStatusItems(Items);
    end;
    Cur := Cur^.Next;
  end;
end;

begin
  TView.Store(S);
  DoStoreStatusDefs(Defs);
end;


procedure TStatusLine.Update;
var
  H: Word;
  P: PView;
begin
  P := TopView;
  if P <> nil then
    H := P^.GetHelpCtx else
    H := hcNoContext;
  if (HelpCtx <> H) or (OldKbdState <> ShiftState) then
  begin
    OldKbdState := ShiftState;
    HelpCtx := H;
    FindItems;
    DrawView;
  end;
end;

function NewStatusDef(AMin, AMax: Word; AItems: PStatusItem;
  ANext:PStatusDef): PStatusDef;
var
  T: PStatusDef;
begin
  New(T);
  with T^ do
  begin
    Next := ANext;
    Min := AMin;
    Max := AMax;
    Items := AItems;
  end;
  NewStatusDef := T;
end;

function NewStatusKey(const AText: String; AKeyCode: Word; ACommand: Word;
  ANext: PStatusItem): PStatusItem;
var
  T: PStatusItem;
begin
  New(T);
  T^.Text := NewStr(AText);
  T^.KeyCode := AKeyCode;
  T^.Command := ACommand;
  T^.Next := ANext;
  NewStatusKey := T;
end;

function LookUpMenu(Menu: PMenu; idCheckItem: Word; Flags: Word): PMenuItem;
var
  Item, LookedUp: PMenuItem;
  i: Integer;
begin
  LookUpMenu := nil;
  If Menu <> nil then
  with Menu^ do begin
    Item := Items;
    If Flags and dfByPosition <> 0
    then begin
      For i := 1 to idCheckItem - 1 do
      begin
        Item := Item^.Next;
        If Item = nil then Break
      end;
      LookUpMenu := Item
    end
    else begin
      LookedUp := nil;
      If Item <> nil then
      repeat
        If (Item^.Command = 0) and (Item^.SubMenu <> nil) and (Item^.Name <> nil)
        then LookedUp := LookUpMenu(Item^.SubMenu, idCheckItem, Flags) else
        if (Item^.Command = idCheckItem)
        then LookedUp := Item;
        Item := Item^.Next;
      until (Item = nil) or (LookedUp <> nil);
      LookUpMenu := LookedUp
    end
  end
end;

function MenuIndexOf(Menu: PMenu; idCheckItem: PMenuItem): Word; {new}
var
  Item: PMenuItem;
  I: Word;
begin
  MenuIndexOf := 0;
  If Menu <> nil then
  with Menu^ do begin
    Item := Items;
    I := 1;
    while (Item <> idCheckItem) and (Item <> nil) do begin
     Inc(i);
     Item := Item^.Next;
    end;
    if Item = nil then MenuIndexOf := 0 else MenuIndexOf := i;
  end
end;

procedure StoreMenuDefaults(Menu: PMenu; var S: TStream);         {new}
var
  Item: PMenuItem;
  I: Word;
begin
  If Menu <> nil then
  with Menu^ do begin
    I := MenuIndexOf(Menu, Default);
    S.Write(I, SizeOf(I));
    Item := Items;
    If Item <> nil then
     repeat
      If (Item^.Command = 0) and (Item^.SubMenu <> nil) and (Item^.Name <> nil)
       then StoreMenuDefaults(Item^.SubMenu, S);
      Item := Item^.Next;
     until (Item = nil);
  end
end;

procedure LoadMenuDefaults(Menu: PMenu; var S: TStream);          {new}
var
  Item: PMenuItem;
  I: Word;
begin
  If Menu <> nil then
  with Menu^ do begin
    S.Read(I, SizeOf(I));
    Default := LookUpMenu(Menu, I, dfByPosition);
    Item := Items;
    If Item <> nil then
     repeat
      If (Item^.Command = 0) and (Item^.SubMenu <> nil) and (Item^.Name <> nil)
       then LoadMenuDefaults(Item^.SubMenu, S);
      Item := Item^.Next;
     until (Item = nil);
  end
end;

end.
