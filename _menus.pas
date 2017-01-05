unit _Menus;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Streams, _Views;

type
  PMenuView = ^TMenuView;
  TMenuView = object(TView)
    ParentMenu: PMenuView;
    Menu: PMenu;
    Current: PMenuItem;
    Constructor Init(var Bounds: TRect);
    Constructor Load(var s: TStream);
    {function Execute: Word; virtual;}
    function FindItem(Ch: Char): PMenuItem;
    procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;
    {function GetHelpCtx: Word; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function HotKey(KeyCode: word): PMenuItem;
    function NewSubView(var Bounds: TRect; AMenu: PMenu; AParentMenu:
      PMenuView): PMenuView; virtual;
    procedure Store(var s: TStream);
    end;

  PMenuBar = ^TMenuBar;
  TMenuBar = object(TMenuView)
    Constructor Init(var Bounds: TRect; AMenu: PMenu);
    {destructor Done; virtual;}
    {procedure Draw; virtual;}
    {procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;}
    end;

  PMenuBox = ^TMenuBox;
  TMenuBox = object(TMenuView)
    TopItem: PMenuItem;
    Constructor Init(var Bounds: TRect; AMenu: PMenu; AParentMenu:
      PMenuView);
    {procedure Draw; virtual;}
    {procedure GetItemRect(Item: PMenuItem; var R: TRect); virtual;}
    end;

  PMenuPopup = ^TMenuPopup;
  TMenuPopup = object(TMenuBox)
    Constructor Init(var Bounds: TRect; AMenu: PMenu);
    {procedure HandleEvent(var Event: TEvent); virtual;}
    end;

  PStatusLine = ^TStatusLine;
  TStatusLine = object(TView)
    Items: PStatusItem;
    Defs: PStatusDef;
    Constructor Init(var Bounds: TRect; ADefs: PStatusDef);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
    {procedure Draw; virtual;}
    {function GetPalette: PPalette; virtual;}
    {procedure HandleEvent(var Event: TEvent); virtual;}
    function Hint(AHelpCtx: word): String; virtual;
    procedure Store(var s: TStream);
    {procedure Update; virtual;}
    end;

implementation

uses
  _DNFuncs;

Constructor TMenuView.Init(var Bounds: TRect);
  begin
    _TMenuView^.Init(Bounds, nil, @Self);
  end;

Constructor TMenuView.Load(var s: TStream);
  begin
    _TMenuView^.Load(_Model1.TStream(s), nil, @Self);
  end;

function TMenuView.FindItem(Ch: Char): PMenuItem;
  begin
    Result := _TMenuView^.FindItem(Ch, @Self);
  end;

procedure TMenuView.GetItemRect(Item: PMenuItem; var R: TRect);
  assembler; {&Frame-}
asm
end;

function TMenuView.HotKey(KeyCode: word): PMenuItem;
  begin
    Result := _TMenuView^.HotKey(KeyCode, @Self);
  end;

function TMenuView.NewSubView(var Bounds: TRect; AMenu: PMenu;
    AParentMenu: PMenuView): PMenuView;
  assembler; {&Frame-}
asm
end;

procedure TMenuView.Store(var s: TStream);
  begin
    _TMenuView^.Store(_Model1.TStream(s), @Self);
  end;

Constructor TMenuBar.Init(var Bounds: TRect; AMenu: PMenu);
  begin
    _TMenuBar^.Init(Bounds, AMenu, nil, @Self);
  end;

Constructor TMenuBox.Init(var Bounds: TRect; AMenu: PMenu;
    AParentMenu: PMenuView);
  begin
    _TMenuBox^.Init(Bounds, AMenu, _Model1.PMenuView(AParentMenu),
      nil, @Self);
  end;

Constructor TMenuPopup.Init(var Bounds: TRect; AMenu: PMenu);
  begin
    _TMenuPopup^.Init(Bounds, AMenu, nil, @Self);
  end;

Constructor TStatusLine.Init(var Bounds: TRect; ADefs: PStatusDef);
  begin
    _TStatusLine^.Init(Bounds, ADefs, nil, @Self);
  end;

Constructor TStatusLine.Load(var s: TStream);
  begin
    _TStatusLine^.Load(_Model1.TStream(s), nil, @Self);
  end;

function TStatusLine.Hint(AHelpCtx: word): String;
  assembler; {&Frame-}
asm
end;

procedure TStatusLine.Store(var s: TStream);
  begin
    _TStatusLine^.Store(_Model1.TStream(s), @Self);
  end;

end.
