{$I STDEFINE.INC}

unit UserSavr;

interface

uses
  Views, Objects;

type

  PUserSaver = ^TUserSaver;
  TUserSaver = object(TView)
    Screen: Pointer;
    SSize, SWidth: AInt;
    CShape, CPos: AWord;
    CheckIO: boolean;
    isValid: boolean;
    Constructor Init(ACheck: boolean);
    destructor Done; virtual;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    function Valid(Command: word): boolean; virtual;
    end;

procedure InsertUserSaver(ACheck: boolean);

implementation

uses
  Memory, Drivers, DNUtil, Messages, Commands, DNApp;

{ ------------------------------------------------------------------------- }

Constructor TUserSaver.Init;
  var
    R: TRect;
  begin
    R.Assign(0, 0, 0, 0);
    inherited Init(R);
    CheckIO := ACheck;
    SetState(sfVisible, False);
    isValid := True;
    Screen := MemAlloc(UserScreenSize);
    if Screen = nil then
      Fail;
    Move(UserScreen^, Screen^, UserScreenSize);
    CLSAct := False;
    SSize := UserScreenSize;
    SWidth := UserScreenWidth;
    CShape := OldCursorShape;
    CPos := OldCursorPos;
  end;

function TUserSaver.Valid;
  begin
    Valid := isValid
  end;

Constructor TUserSaver.Load;
  var
    i: byte;
  begin
    inherited Load(s);
    DataSaver := @Self;
    s.Read(SSize, 4*SizeOf(AInt)+SizeOf(boolean));
    if UserScreen <> nil then
      FreeMem(UserScreen, UserScreenSize);
    UserScreenSize := SSize;
    UserScreenWidth := SWidth;
    UserScreen := MemAlloc(SSize);
    OldCursorShape := CShape;
    OldCursorPos := CPos;
    s.Read(UserScreen^, SSize);
    Screen := nil;
    isValid := False;
    i := 0;
    if i <> 0 then
      Msg(dlErrorsOccurred, nil, mfWarning+mfOKButton);
  end { TUserSaver.Load };

destructor TUserSaver.Done;
  begin
    if Screen <> nil then
      FreeMem(Screen, SSize);
    inherited Done;
  end;

procedure TUserSaver.Store;
  begin
    inherited Store(s);
    s.Write(SSize, 4*SizeOf(AInt)+SizeOf(boolean));
    s.Write(Screen^, SSize);
  end;

procedure InsertUserSaver;
  begin
    Desktop^.Insert(New(PUserSaver, Init(ACheck)));
    FreeMem(UserScreen, UserScreenSize);
    UserScreenSize := ScreenWidth*ScreenHeight*2;
    UserScreenWidth := ScreenWidth;
    OldCursorPos := 0;
    OldCursorShape := $FFFF;
    HideMouse;
    GetMem(UserScreen, UserScreenSize);
    System.Move(ScreenBuffer^, UserScreen^, UserScreenSize);
    ShowMouse;
  end;

end.
