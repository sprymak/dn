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
{AK155 = Alexey Korop, 2:461/155@fidonet}
{$I STDEFINE.INC}
{$DEFINE NOASM}
unit UserMenu;

interface

uses
  Objects, Views, Scroller, Drivers, FilesCol, Files, Collect;

var
  QuickExecExternalStr: String;

function QuickExecExternal(n: integer): boolean;
function ExecUserMenu(DoGlobal: boolean): boolean;
procedure ScreenGrabber(ShowMessage: boolean);
function MakeString(s: String; UserParams: PUserParams;
  HandleTildes: boolean; TM: PString): String;

type
  PUserView = ^TUserView;
  TUserView = object(TScroller)
    Grabbing: boolean;
    Constructor Init(var R: TRect; H, V: PScrollBar);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure ChangeBounds(var Bounds: TRect); virtual;
    end;

  PUserWindow = ^TUserWindow;
  TUserWindow = object(TWindow)
    OldScreenWidth: word;
    Constructor Init;
    procedure CalcBounds(var Bounds: TRect; Delta: TPoint); virtual;
    procedure SetState(AState: word; Enable: boolean); virtual;
    end;

const
  RunFrom2E: boolean = False;

implementation
uses
  Lfn, {DataCompBoy}
  DNApp, advance, advance1, advance2, advance3, Startup, Messages,
    Menus,
  Commands, Microed, WinClp, DNHelp, Dos, Memory, Dialogs, Tree,
    RStrings;

type
  PUserMenuItem = ^TUserMenuItem;
  TUserMenuItem = record
    text: PString;
    Level: word;
    Line: word;
    RunFrom2E: boolean;
    end;

  PUserCollection = ^TUserCollection;
  TUserCollection = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
    end;

Constructor TUserWindow.Init;
  var
    R: TRect;
  begin
    Desktop^.GetExtent(R);
    R.Grow(1, 1);
    inherited Init(R, GetString(dlOutputTitle), 0);
    OldScreenWidth := ScreenWidth;
    GetExtent(R);
    R.Grow(-1, -1);
    Insert(New(PUserView, Init(R, StandardScrollBar(sbHorizontal+
      sbHandleKeyboard),
    StandardScrollBar(sbVertical+sbHandleKeyboard))));
  end;

procedure TUserWindow.CalcBounds(var Bounds: TRect; Delta: TPoint);
  begin
    if (Size.X <= ScreenWidth) and (ScreenWidth = OldScreenWidth)
    then
      begin
        inherited CalcBounds(Bounds, Delta);
        exit
      end;
    Desktop^.GetExtent(Bounds);
    Bounds.Grow(1, 2);
    Inc(Bounds.B.Y);
    OldScreenWidth := ScreenWidth;
  end;

procedure TUserWindow.SetState(AState: word; Enable: boolean);
  var
    WindowCommands: TCommandSet;
  begin
    inherited SetState(AState, Enable);
    if AState = sfSelected then
      begin
        SetState(sfActive, Enable);
        if Enable then
          EnableCommands([cmHideLeft, cmHideRight, cmHideInactive])
        else
          DisableCommands([cmHideLeft, cmHideRight, cmHideInactive]);
      end;
  end;

Constructor TUserView.Init;
  begin
    inherited Init(R, H, V);
    SetLimit(ScreenWidth, ScreenHeight);
    GrowMode := gfGrowHiX+gfGrowHiY;
    EventMask := $FFFF;
    Options := Options or ofSelectable;
    Grabbing := False;
    ScrollTo(0,
    byte(CommandLine^.GetState(sfVisible))+
    byte(InterfaceData.Options and ouiHideMenu = 0)
      { +
   Byte(InterfaceData.Options and ouiHideStatus = 0)});
    Delta.Y := VScrollBar^.Value;
  end;

procedure TUserView.ChangeBounds;
  begin
    inherited ChangeBounds(Bounds);
  end;

procedure TUserView.Draw;
  var
    i: integer;
    B: TDrawBuffer;
  begin
    for i := 0 to Size.Y-1 do
      begin
        MoveChar(B, ' ', 07, SizeOf(B) div 2);
        if i+Delta.Y < UserScreenSize div (UserScreenWidth*2)
        then
          Move(PWordArray(UserScreen)^[(i+Delta.Y)*UserScreenWidth], B,
            UserScreenWidth*2);
        WriteLine(0, i, Size.X, 1, B[Delta.X]);
      end;
  end;

procedure TUserView.HandleEvent;
  begin
    inherited HandleEvent(Event);
    SetLimit(ScreenWidth, ScreenHeight);
    case Event.What of
      evCommand:
        case Event.Command of
          {cmGrabscreen: begin ClearEvent(Event); if Execute = cmOK then  end;}
          cmGetName:
            PString(Event.InfoPtr)^:= GetString(dlOutputTitle);
          cmShowOutput, cmHideRight, cmHideLeft:
            begin
              if Event.Command <> cmShowOutput then
                begin
                  if Event.Command = cmHideLeft then
                    Event.Command := cmPostHideLeft
                  else
                    Event.Command := cmPostHideRight;
                  PutEvent(Event);
                end;
              if Owner^.GetState(sfSelected) then
                Message(Owner, evCommand, cmClose, nil)
              else
                PView(Event.InfoPtr^) := Owner;
              ClearEvent(Event);
            end;
        end {case};
      evKeyDown:
        begin
          { Flash >>> }
          if (Event.KeyCode = kbShiftUp) or (Event.KeyCode =
              kbShiftDown)
          then
            CommandLine^.HandleEvent(Event);
          { Flash <<< }
          if Size.X = ScreenWidth then
            CommandLine^.HandleEvent(Event);
        end;
    end {case};
  end { TUserView.HandleEvent };

procedure TUserCollection.FreeItem(P: Pointer);
  begin
    DisposeStr(PUserMenuItem(P)^.text);
    Dispose(PUserMenuItem(P));
  end;

{AK155 17-12-2002}
procedure NameAndExt(HandleTildes: boolean;
  TS: String; Macro: Char; var s: String);
  var
    l: integer;
  begin
    if HandleTildes then
      Replace('~', #0'~', TS);
    Replace('$', #4, TS);
    Replace('!', #1, TS);
    Replace('&', #2, TS);
    Replace('#', #3, TS);
    l := PosLastDot(TS);
    Replace('.'+Macro, Copy(TS, l, 255), s);
    Replace(Macro, Copy(TS, 1, l-1), s);
  end;

{-DataCompBoy-}
{AK155 Тут была куча обкладываний строк символом #0,
непонятно зачем нужными. В ритлабовском DN этого не было,
так что я убрал, а вставил замену в именах файлов '~' на #0'~'
(см. MoveCStr в drivers._vp)}

function MakeString(s: String; UserParams: PUserParams; HandleTildes:
    boolean; TM: PString): String;
  var
    TS: String;
    tz: String;
    DA, DP: TDate4; { дата файла в активной и пассивной панели }
  begin
    {  if HandleTildes then zs:=#0 else zs:='';}
    Replace('!!', #1, s);
    Replace('&&', #2, s);
    Replace('##', #3, s);
    Replace('$$', #4, s);

    {$IFDEF OS2}
    Replace('!', '#', s);
    Replace('$', '&', s);
    {$ENDIF}

    if UserParams^.active <> nil then
      DA := TDate4(UserParams^.active^.FDate);
    if UserParams^.Passive <> nil then
      DP := TDate4(UserParams^.Passive^.FDate);

    {$IFNDEF OS2}
    if (Pos('!\', s) > 0) or (Pos('!:', s) > 0) or (Pos('!/', s) > 0)
    then
      begin
        if UserParams^.active = nil then
          TS := '' {KSNK}
        else
          begin
            if UserParams^.active^.TType = ttUpDir
            then
              TS := MakeNormName(lfGetShortFileName(UserParams^.
                active^.Owner^), '')
            else
              TS := MakeNormName(GetPath(lfGetShortFileName(
                MakeNormName(
              UserParams^.active^.Owner^, UserParams^.active^.FlName[
                False]))), '');
          end;
        if HandleTildes then
          Replace('~', #0'~', TS);
        Replace('$', #4, TS);
        Replace('!', #1, TS);
        Replace('&', #2, TS);
        Replace('#', #3, TS);
        Replace('!:', Copy(TS, 1, 2), s);
        Replace('!\', Copy(TS, 3, MaxStringLength), s);
        Replace('!/', Copy(TS, 3, Length(TS)-3), s);
      end;
    {$ENDIF}

    if (Pos('#\', s) > 0) or (Pos('#:', s) > 0) or (Pos('#/', s) > 0)
    then
      begin
        if UserParams^.active = nil then
          TS := '' {KSNK}
        else
 if UserParams^.active^.TType = ttUpDir
        then
          TS := MakeNormName(UserParams^.active^.Owner^, '')
        else
          TS := MakeNormName(GetPath(MakeNormName(
          UserParams^.active^.Owner^, UserParams^.active^.FlName[
            True])), '');
        if HandleTildes then
          Replace('~', #0'~', TS);
        Replace('$', #4, TS);
        Replace('!', #1, TS);
        Replace('&', #2, TS);
        Replace('#', #3, TS);
        Replace('#:', Copy(TS, 1, 2), s);
        Replace('#\', Copy(TS, 3, MaxStringLength), s);
        Replace('#/', Copy(TS, 3, Length(TS)-3), s);
      end;

    {$IFNDEF OS2}
    if (Pos('$\', s) > 0) or (Pos('$:', s) > 0) or (Pos('$/', s) > 0)
    then
      begin
        if UserParams^.Passive = nil then
          TS := '' {KSNK}
        else if UserParams^.Passive^.TType = ttUpDir
        then
          TS := MakeNormName(lfGetShortFileName(UserParams^.Passive^.
            Owner^), '')
        else
          TS := MakeNormName(GetPath(lfGetShortFileName(MakeNormName(
          UserParams^.Passive^.Owner^, UserParams^.Passive^.FlName[
            False]))), '');
        if HandleTildes then
          Replace('~', #0'~', TS);
        Replace('$', #4, TS);
        Replace('!', #1, TS);
        Replace('&', #2, TS);
        Replace('#', #3, TS);
        Replace('$:', Copy(TS, 1, 2), s);
        Replace('$\', Copy(TS, 3, MaxStringLength), s);
        Replace('$/', Copy(TS, 3, Length(TS)-3), s);
      end;
    {$ENDIF}

    if (Pos('&\', s) > 0) or (Pos('&:', s) > 0) or (Pos('&/', s) > 0)
    then
      begin
        if UserParams^.Passive = nil then
          TS := '' {KSNK}
        else if UserParams^.Passive^.TType = ttUpDir
        then
          TS := MakeNormName(UserParams^.Passive^.Owner^, '')
        else
          TS := MakeNormName(GetPath(MakeNormName(
          UserParams^.Passive^.Owner^, UserParams^.Passive^.FlName[
            True])), '');
        if HandleTildes then
          Replace('~', #0'~', TS);
        Replace('$', #4, TS);
        Replace('!', #1, TS);
        Replace('&', #2, TS);
        Replace('#', #3, TS);
        Replace('&:', Copy(TS, 1, 2), s);
        Replace('&\', Copy(TS, 3, MaxStringLength), s);
        Replace('&/', Copy(TS, 3, Length(TS)-3), s);
      end;

    if Replace('!%', #5, s) or Replace('#%', #5, s) then
      begin
        if (Pos(#5'A', s) > 0) or (Pos(#5'a', s) > 0) then
          begin
            if UserParams^.active = nil then
              TS := '' {KSNK}
            else
              TS := GetAttrStr(UserParams^.active^.Attr);
            Replace(#5'A', TS, s);
            Replace(#5'a', TS, s);
          end;

        if (Pos(#5'C', s) > 0) or (Pos(#5'c', s) > 0) then
          begin
            if UserParams^.active = nil then
              TS := '' {KSNK}
            else if UserParams^.active^.Attr and Directory <> 0
            then
              TS := ''
            else
              TS := ZtoS(UserParams^.active^.PSize);
            Replace(#5'C', TS, s);
            Replace(#5'c', TS, s);
          end;

        if (Pos(#5'D', s) > 0) or (Pos(#5'd', s) > 0) then
          begin
            if UserParams^.active = nil then
              TS := '' {KSNK}
            else
              with UserParams^.active^, DA do
                MakeDateFull(CountryInfo.DateFmt, Day, Month, Yr,
                  Hour, Minute, TS, True);
            SetLength(TS, 10);
            Replace(#5'D', TS, s);
            Replace(#5'd', TS, s);
          end;

        if (Pos(#5'R', s) > 0) or (Pos(#5'r', s) > 0) then
          begin
            if UserParams^.active = nil then
              TS := '' {KSNK}
            else
              TS := ItoS(Round(((UserParams^.active^.PSize/
                UserParams^.active^.Size)*100)+0.5))+'%';
            Replace(#5'R', TS, s);
            Replace(#5'r', TS, s);
          end;

        if (Pos(#5'S', s) > 0) or (Pos(#5's', s) > 0) then
          begin
            if UserParams^.active = nil then
              TS := '' {KSNK}
            else if UserParams^.active^.Attr and Directory <> 0
            then
              TS := ''
            else
              TS := ZtoS(UserParams^.active^.Size);
            Replace(#5'S', TS, s);
            Replace(#5's', TS, s);
          end;

        if (Pos(#5'T', s) > 0) or (Pos(#5't', s) > 0) then
          begin
            if UserParams^.active = nil then
              TS := '' {KSNK}
            else
              with UserParams^.active^, DA do
                TS := FormatTimeStr(Hour, Minute, Second);
            Replace(#5'T', TS, s);
            Replace(#5't', TS, s);
          end;

        if (Pos(#5'Z', s) > 0) or (Pos(#5'z', s) > 0) then
          begin
            if UserParams^.active = nil then
              TS := '' {KSNK}
            else if (UserParams^.active^.DIZ <> nil) and (UserParams^.
                active^.DIZ^.DIZ <> nil)
            then
              TS := UserParams^.active^.DIZ^.DIZ^
            else
              TS := '';
            if HandleTildes then
              Replace('~', #0'~', TS);
            Replace('$', #4, TS);
            Replace('!', #1, TS);
            Replace('&', #2, TS);
            Replace('#', #3, TS);
            Replace(#5'Z', TS, s);
            Replace(#5'z', TS, s);
          end;

        if (Pos(#5'F', s) > 0) or (Pos(#5'f', s) > 0) then
          begin
            {$IFNDEF OS2}
            TS := MakeNormName(lfGetShortFileName(SwpDir),
            GetName(CalcTmpFName(CalcTmpId, 'flt', True)));
            {$ELSE}
            TS := MakeNormName(SwpDir, GetName(CalcTmpFName(
              CalcTmpId, 'flt', True)));
            {$ENDIF}
            if TM <> nil then
              TM^:= TS;
            if HandleTildes then
              Replace('~', #0'~', TS);
            Replace('$', #4, TS);
            Replace('!', #1, TS);
            Replace('&', #2, TS);
            Replace('#', #3, TS);
            Replace(#5'F', TS, s);
            Replace(#5'f', TS, s);
          end;
      end;

    if Replace('$%', #6, s) or Replace('&%', #6, s) then
      begin
        if (Pos(#6'A', s) > 0) or (Pos(#6'a', s) > 0) then
          begin
            if UserParams^.Passive = nil then
              TS := '' {KSNK}
            else
              TS := GetAttrStr(UserParams^.Passive^.Attr);
            Replace(#6'A', TS, s);
            Replace(#6'a', TS, s);
          end;

        if (Pos(#6'C', s) > 0) or (Pos(#6'c', s) > 0) then
          begin
            if UserParams^.Passive = nil then
              TS := '' {KSNK}
            else if UserParams^.Passive^.Attr and Directory <> 0
            then
              TS := ''
            else
              TS := ZtoS(UserParams^.Passive^.PSize);
            if HandleTildes then
              Replace('~', #0'~', TS);
            Replace(#6'C', TS, s);
            Replace(#6'c', TS, s);
          end;

        if (Pos(#6'D', s) > 0) or (Pos(#6'd', s) > 0) then
          begin
            if UserParams^.Passive = nil then
              TS := '' {KSNK}
            else
              with UserParams^.Passive^, DP do
                MakeDateFull(CountryInfo.DateFmt, Day, Month, Yr,
                  Hour, Minute, TS, True);
            SetLength(TS, 10);
            Replace(#6'D', TS, s);
            Replace(#6'd', TS, s);
          end;

        if (Pos(#6'R', s) > 0) or (Pos(#6'r', s) > 0) then
          begin
            if UserParams^.Passive = nil then
              TS := '' {KSNK}
            else
              TS := ItoS(Round(((UserParams^.Passive^.PSize/
                UserParams^.Passive^.Size)*100)+0.5))+'%';
            Replace(#6'R', TS, s);
            Replace(#6'r', TS, s);
          end;

        if (Pos(#6'S', s) > 0) or (Pos(#6's', s) > 0) then
          begin
            if UserParams^.Passive = nil then
              TS := '' {KSNK}
            else if UserParams^.Passive^.Attr and Directory <> 0
            then
              TS := ''
            else
              TS := ZtoS(UserParams^.Passive^.Size);
            Replace(#6'S', TS, s);
            Replace(#6's', TS, s);
          end;

        if (Pos(#6'T', s) > 0) or (Pos(#6't', s) > 0) then
          begin
            if UserParams^.Passive = nil then
              TS := '' {KSNK}
            else
              with UserParams^.Passive^, DP do
                TS := FormatTimeStr(Hour, Minute, Second);
            Replace(#6'T', TS, s);
            Replace(#6't', TS, s);
          end;

        if (Pos(#6'Z', s) > 0) or (Pos(#6'z', s) > 0) then
          begin
            if UserParams^.Passive = nil then
              TS := '' {KSNK}
            else if (UserParams^.Passive^.DIZ <> nil) and (
                UserParams^.Passive^.DIZ^.DIZ <> nil)
            then
              TS := UserParams^.Passive^.DIZ^.DIZ^
            else
              TS := '';
            Replace('$', #4, TS);
            Replace('!', #1, TS);
            Replace('&', #2, TS);
            Replace('#', #3, TS);
            Replace(#6'Z', TS, s);
            Replace(#6'z', TS, s);
          end;
      end;

    {$IFNDEF OS2}
    if (PosChar('!', s) > 0) then
      begin
        if UserParams^.active = nil then
          TS := '' {KSNK}
        else
          TS := UserParams^.active^.FlName[False];
        NameAndExt(HandleTildes, TS, '!', s);
      end;

    if (PosChar('$', s) > 0) then
      begin
        if UserParams^.Passive = nil then
          TS := '' {KSNK}
        else
          TS := UserParams^.Passive^.FlName[False];
        NameAndExt(HandleTildes, TS, '$', s);
      end;
    {$ENDIF}

    if (PosChar('#', s) > 0) then
      begin
        if UserParams^.active = nil then
          TS := '' {KSNK}
        else
          TS := UserParams^.active^.FlName[True];
        NameAndExt(HandleTildes, TS, '#', s);
      end;

    if (PosChar('&', s) > 0) then
      begin
        if UserParams^.Passive = nil then
          TS := '' {KSNK}
        else
          TS := UserParams^.Passive^.FlName[True];
        NameAndExt(HandleTildes, TS, '&', s);
      end;

    Replace(#1, '!', s);
    Replace(#2, '&', s);
    Replace(#3, '#', s);
    Replace(#4, '$', s);

    MakeString := s;
  end { MakeString };
{/AK155}
{-DataCompBoy-}

function ExecUserMenu;
  var
    F: PTextReader;
    F1: lText; {DataCompBoy}
    P: PUserCollection;
    s, s1: String;
    i: longInt;
    UI: PUserMenuItem;
    StartPos: integer;
    Items, OItems, SItems: PMenuItem;
    Menu: PMenu;
    PV: PView;
    R: TRect;
    NI, NW, LL: word;
    EnterParms: boolean;
    TitleStr: String[30];
    DefStr: String;
    RF2E: boolean;
    Event: TEvent;
    {$IFDEF OS2}
    WriteEcho: boolean;
    {$ENDIF}
  label 1, 2;

  function CheckFKeys(s: String): word;
    var
      i: integer;
      s1: String;
    begin
      CheckFKeys := kbNoKey;
      for i := 1 to 12 do
        begin
          s1 := 'F'+ItoS(i)+' ';
          if UpStrg(Copy(s, 1, Length(s1))) = s1 then
            begin
              CheckFKeys := kbF1+(i-1) shl 8;
              exit
            end;
        end;
    end;

  var
    MenuLast: PMenuItem;

  function DoSubMenu(Main: boolean): PMenuItem;
    var
      P1, P2: PMenuItem;
      UI, PU: PUserMenuItem;
      KB: word;
    begin
      DoSubMenu := nil;
      if i = P^.Count then
        exit;
      UI := P^.At(i);
      {if UI^.Text = nil then begin DoSubMenu := NewLine(DoSubMenu); Exit end;}
      KB := kbNoKey;
      P2 := nil;
      if UI^.text <> nil then
        P1 := NewItem(UI^.text^, '', CheckFKeys(UI^.text^), 1000+i,
        hcLUserMenu+byte(DoGlobal), nil)
      else
        P1 := NewItem('Empty line', '', kbNoKey, 1000+i,
        hcLUserMenu+byte(DoGlobal), nil);
      DoSubMenu := P1;
      while (i < P^.Count-1) do
        begin
          Inc(i);
          PU := P^.At(i);
          MenuLast := P1;
          if PU^.Level < UI^.Level then
            begin
              Dec(i);
              exit;
            end;
          if PU^.Level > UI^.Level then
            begin
              P1^.SubMenu := NewMenu(DoSubMenu(False));
              P1^.Command := 0;
            end
          else
            begin
              if PU^.text <> nil
              then
                P1^.Next := NewItem(PU^.text^, '', CheckFKeys(PU^.
                  text^),
                1000+i, hcLUserMenu+byte(DoGlobal), nil)
              else
                P1^.Next := NewLine(nil);
              if Main then
                Inc(NI);
              P1 := P1^.Next;
            end;
          MenuLast := P1;
        end;
    end { DoSubMenu };

  {-DataCompBoy-}
  var
    UserParams: tUserParams;

  begin { ExecUserMenu }
    FillChar(UserParams, SizeOf(UserParams), 0);
    Message(Desktop, evBroadcast, cmGetUserParamsWL, @UserParams);
      {.11}
    RunFrom2E := False;
    ExecUserMenu := False;
    TitleStr := '';
    DefStr := '';
    EnterParms := False;
    if DoGlobal then
      goto 2;

    F := New(PTextReader, Init(SwpDir+'$dn'+ItoS(DNNumber)+'$.mnu'));
    if F <> nil then
      goto 1;

    s := lFExpand('dn.mnu');
    i := Length(s);
    if Abort then
      goto 2;
    while (i > 0) and (s[i] <> '\') do
      Dec(i);
    if i = 0 then
      goto 2;
    repeat
      F := New(PTextReader, Init(s));
      if F <> nil then
        goto 1;

      while (i > 2) and (s[i-1] <> '\') do
        begin
          Delete(s, i-1, 1);
          Dec(i);
        end;
      Delete(s, i-1, 1);
      Dec(i);
    until i < 3;
2:
    DoGlobal := True;
    F := New(PTextReader, Init(SourceDir+'dn.mnu'));
    if F = nil then
      begin
        ErrMsg(dlMNUNotFound);
        exit;
      end;
1:
    Message(Desktop, evBroadcast, cmGetUserParamsWL, @UserParams);
      ClrIO;
    New(P, Init(10, 10));
    i := 0;
    NW := 30;
    while (not F^.Eof) and (IOResult = 0) do
      begin
        s := F^.GetStr;
        Inc(i);
        DelLeft(s);
        DelRight(s);
        if (s <> '') and (s[1] = '>') then
          begin
            RF2E := False;
            Delete(s, 1, 1); {DelFC(S);}
            if s[1] = '>' then
              begin
                Delete(s, 1, 1); {DelFC(S);}RF2E := True
              end;
            s1 := '';
            repeat
              s1 := s1+s[1];
              Delete(s, 1, 1); {DelFC(S);}
            until (s = '') or (s[1] = ' ');
            Delete(s, 1, 1); {DelFC(S);}
            if (StoI(s1) > 0) then
              begin
                New(UI);
                s := MakeString(s, @UserParams, True, nil);
                UI^.text := NewStr(s);
                UI^.Level := StoI(s1);
                if (CStrLen(s) > NW) and (UI^.Level = 1) then
                  NW := CStrLen(s);
                UI^.Line := i;
                UI^.RunFrom2E := RF2E;
                P^.Insert(UI);
              end;
          end;
      end;
    s := F^.FileName;
    Dispose(F, Done);
    if P^.Count > 0 then
      begin
        StartPos := -1;
        repeat
          i := 0;
          OItems := nil;
          SItems := nil;
          NI := 3;
          SItems := DoSubMenu(True);
          if SItems = nil then
            break;
          Menu := NewMenu(SItems);
          Application^.GetExtent(R);
          R.A.X := 0;
          R.B.X := NW;
          R.B.Y := Min(R.B.Y, NI);
          PV := New(PMenuBox, Init(R, Menu, nil));
          PV^.Options := PV^.Options or ofCentered;
          i := Application^.ExecView(PV);
          Dispose(PV, Done);
          DisposeMenu(Menu);
          if i < 1000 then
            begin
              if UserParams.ActiveList <> '' then
                EraseFile(UserParams.ActiveList);
              if UserParams.PassiveList <> '' then
                EraseFile(UserParams.PassiveList);
              if i = cmEditMenu then
                begin
                  Event.What := evCommand;
                  Event.Command := byte(not DoGlobal)*
                    cmLocalMenuFileEdit+
                  byte(DoGlobal)*cmMenuFileEdit;
                  Event.InfoPtr := nil;
                  Application^.PutEvent(Event);
                end;
              break;
            end;
          StartPos := i-999;
          if (StartPos >= P^.Count-1) or
            (PUserMenuItem(P^.At(StartPos-1))^.Level >=
              PUserMenuItem(P^.At(StartPos))^.Level)
          then
            begin
              ExecUserMenu := True;
              i := i-1000;
              NW := i;
              UI := P^.At(i);
              LL := UI^.Line;
              if i >= P^.Count-1 then
                LL := 65535
              else
                LL := PUserMenuItem(P^.At(i+1))^.Line-1;
              F := New(PTextReader, Init(s));
              lAssignText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$'+CmdExt);
              lRewriteText(F1);
              {$IFNDEF OS2}
              Writeln(F1.t, '@Echo off');
              {$ELSE}
              WriteEcho := True;
              {$ENDIF}
              i := i-1001;
              if F <> nil then
                begin
                  for i := 0 to UI^.Line-1 do
                    s := F^.GetStr;
                  while not F^.Eof do
                    begin
                      s := F^.GetStr;
                      DelLeft(s);
                      DelRight(s);
                      if (s <> '') then
                        if (s[1] = '>') then
                          break
                        else if (s[1] = ';') then
                          continue
                        else if (s[1] = '<') then
                          begin
                            EnterParms := True;
                            Delete(s, 1, 1); {DelFC(S);}
                            if (s <> '') and (s[1] = '=') then
                              begin
                                Delete(s, 1, 1); {DelFC(S);}
                                {Knave 29.08.99}s := MakeString(s,
                                  @UserParams, False, nil);
                                DefStr := s;
                              end
                            else
                              begin
                                DelLeft(s);
                                TitleStr := s;
                              end;
                            continue;
                          end;
                      EnterParms := EnterParms or (Pos('%3', s) > 0);
                      {$IFDEF OS2}
                      {JO: под осью если в меню строка на REXX'е или Perl'е, то не нужно добавлять @Echo off}
                      if WriteEcho and (Copy(s, 1, 2) <> '/*') and (
                          Copy(s, 1, 2) <> '#!')
                      then
                        Writeln(F1.t, '@Echo off');
                      WriteEcho := False;
                      {$ENDIF}
                      Writeln(F1.t, MakeString(s, @UserParams, False,
                        nil));
                      RunFrom2E := UI^.RunFrom2E;
                    end;
                end;
              Close(F1.t);
              Dispose(F, Done);
              break;
            end;
        until False;
      end;
    Dispose(P, Done);
    FreeStr := DefStr;
    if TitleStr = '' then
      TitleStr := GetString(dlMenuParamLabel);
    if EnterParms then
      ExecUserMenu := InputBox(GetString(dlMenuParams), TitleStr,
        FreeStr, 125, hsInputParams) = cmOK;
    with UserParams do
      begin
        if ActiveList = '' then
          ActiveList := '-';
        if PassiveList = '' then
          PassiveList := '-';
        FreeStr := ActiveList+' '+PassiveList+' '+FreeStr;
      end;
    EraseFile(SwpDir+'$dn'+ItoS(DNNumber)+'$.mnu');
  end { ExecUserMenu };
{-DataCompBoy-}

type
  PGrabber = ^TGrabber;
  TGrabber = object(TView)
    ModalEnd: boolean;
    Screen: PWordArray;
    R: TRect;
    BufSize: word;
    Constructor Init;
    destructor Done; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Draw; virtual;
    function Execute: word; virtual;
    end;

const
  Top: TPoint = (X: 10; Y: 5);
  Bot: TPoint = (X: 21; Y: 6);
  NotMessage: boolean = True;
  Here: boolean = False;

procedure ScreenGrabber(ShowMessage: boolean);
  var
    P: PGrabber;
    B: boolean;
  begin
    if Here then
      exit;
    Here := True;
    if NotMessage and ShowMessage then
      begin
        MessageBox(GetString(dlGrabWelcome),
        nil, mfOKButton+mfInformation);
        NotMessage := False;
      end;
    B := MsgActive;
    MsgActive := False;
    P := New(PGrabber, Init);
    Application^.ExecView(P);
    Dispose(P, Done);
    Here := False;
    MsgActive := B;
  end { ScreenGrabber };

Constructor TGrabber.Init;
  var
    BB: TRect;
  begin
    Application^.GetExtent(BB);
    inherited Init(BB);
    R.A := Top;
    R.B := Bot;
    BufSize := Application^.Size.X*2*Application^.Size.Y;
    Screen := MemAlloc(BufSize);
    if Screen = nil then
      Fail;
    HideMouse;
    Move(ScreenBuffer^, Screen^, BufSize);
    ShowMouse;
    if Screen = nil then
      Fail;
    Options := Options or ofSelectable or ofTopSelect;
    {EventMask := $FFFF;}
  end;

destructor TGrabber.Done;
  begin
    if Screen <> nil then
      FreeMem(Screen, BufSize);
    inherited Done;
  end;

procedure TGrabber.Draw;

  var
    B: TDrawBuffer;
    i: integer;

    {$IFNDEF NOASM}
  procedure Invert(var B; k: integer);
    assembler;
    {&Frame-} {$USES EBX, ECX}
  asm
         mov ECX, K
         lea EBX, B
        @@1:
         mov AX, word ptr [EBX]
         xor AH, $7F
         and AH, $7F
         jnz @@2
         mov AH, 7
       @@2:
         mov word ptr [EBX], AX
         add EBX, 2
         loop @@1
       end
    ;
    {$ELSE}
  procedure Invert(K1, k2: integer);
    var
      i: integer;
    begin
      for i := K1 to K1+k2-1 do
        begin
          B[i] := ((B[i] xor $7F00) and $7F00) or Lo(B[i]);
        end
    end;
  {$ENDIF}
  begin { TGrabber.Draw }
    with R do
      begin
        if A.X < 0 then
          Move(-A.X, 0);
        if A.Y < 0 then
          Move(0, -A.Y);
        if B.X-A.X > Size.X then
          B.X := A.X+Size.X;
        if B.Y-A.Y > Size.Y then
          B.Y := A.Y+Size.Y;
        if B.X > Size.X then
          Move(Size.X-B.X, 0);
        if B.Y > Size.Y then
          Move(0, Size.Y-B.Y);
      end;
    Top := R.A;
    Bot := R.B;
    for i := 0 to Size.Y-1 do
      begin
        Move(Screen^[Size.X*i], B, Size.X*2);
        if (i >= R.A.Y) and (i < R.B.Y) and (R.A.X < R.B.X) then
          {$IFNDEF NOASM}
          Invert(B[R.A.X], R.B.X-R.A.X);
          {$ELSE}
          Invert(R.A.X, R.B.X-R.A.X);
        {$ENDIF}
        WriteLine(0, i, Size.X, 1, B);
      end;
  end { TGrabber.Draw };

function TGrabber.Execute;
  var
    E: TEvent;
  begin
    ModalEnd := False;
    DrawView;
    repeat
      GetEvent(E);
      if E.What <> evNothing then
        HandleEvent(E);
    until ModalEnd;
  end;

procedure TGrabber.HandleEvent;
  var
    B: boolean;
    Sh: boolean;
    Stp: integer;
  procedure CED;
    begin
      ClearEvent(Event);
      DrawView
    end;

  procedure MakeClip;
    var
      i, j: integer;
      s: String;
      C: Char;
    begin
      if ClipBoard <> nil then
        Dispose(ClipBoard, Done);
      ClipBoard := New(PLineCollection, Init(R.B.Y-R.A.Y, 10, True));
      for i := R.A.Y to R.B.Y-1 do
        begin
          s := '';
          for j := R.A.X to R.B.X-1 do
            begin
              C := Char(WordRec(Screen^[i*Size.X+j]).Lo);
              s := s+C;
            end;
          ClipBoard^.Insert(NewLongStr(s));
        end;
      if SystemData.Options and ossUseSysClip <> 0 then
        SyncClipIn;
      if ClipBoardStream <> nil
      then
        ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-4, 0));
      CopyLines2Stream(ClipBoard, ClipBoardStream);
    end { MakeClip };

  begin { TGrabber.HandleEvent }
    B := True;
    inherited HandleEvent(Event);
    case Event.What of
      evKeyDown:
        begin
          Sh := ShiftState and 3 <> 0;
          if ShiftState and 4 <> 0 then
            Stp := 8
          else
            Stp := 1;
          case Event.KeyCode and $FFFF of{Cat}
            kbPgUp and $FFFF, kbCtrlPgUp and $FFFF:
              if Sh then
                R.A.Y := 0
              else
                R.Move(0, -R.A.Y);
            kbHome and $FFFF, kbCtrlHome and $FFFF:
              if Sh then
                R.A.X := 0
              else
                R.Move(-R.A.X, 0);
            kbEnd and $FFFF, kbCtrlEnd and $FFFF:
              if Sh then
                R.B.X := Size.X
              else
                R.Move(Size.X, 0);
            kbPgDn and $FFFF, kbCtrlPgDn and $FFFF:
              if Sh then
                R.B.Y := Size.Y
              else
                R.Move(0, Size.Y);
            kbRight and $FFFF, kbCtrlRight and $FFFF:
              if Sh then
                begin
                  Inc(R.B.X, Stp);
                  R.B.X := Min(R.B.X, Size.X)
                end
              else
                R.Move(Stp, 0);
            kbLeft and $FFFF, kbCtrlLeft and $FFFF:
              if Sh then
                begin
                  if R.B.X > R.A.X+Stp then
                    Dec(R.B.X, Stp)
                  else
                    R.B.X := R.A.X+1;
                end
              else
                R.Move(-Stp, 0);
            kbEnter and $FFFF:
              begin
                ModalEnd := True;
                MakeClip;
              end;
            kbESC and $FFFF:
              begin
                ModalEnd := True;
              end;
            else
              begin
                if Stp > 1 then
                  Stp := Stp div 2;
                case Event.KeyCode and $FFFF of{Cat}
                  kbUp and $FFFF, kbCtrlUp and $FFFF:
                    if Sh then
                      begin
                        if R.B.Y > R.A.Y+Stp then
                          Dec(R.B.Y, Stp)
                        else
                          R.B.Y := R.A.Y+1;
                      end
                    else
                      R.Move(0, -Stp);
                  kbDown and $FFFF, kbCtrlDown and $FFFF:
                    if Sh then
                      begin
                        Inc(R.B.Y, Stp);
                        R.B.Y := Min(R.B.Y, Size.Y)
                      end
                    else
                      R.Move(0, Stp);
                  else
                    B := False;
                end {case};
              end;
          end {case};
        end
        else
        B := False;
    end {case};
    if B then
      CED;
  end { TGrabber.HandleEvent };

function QuickExecExternal(n: integer): boolean;

  label 1;

  var
    UserParams: tUserParams;
    TitleStr, DefStr, s: String;
    F: PTextReader;
    F1: lText;
    i: integer;
    OS2: Char;
    SR: lSearchRec;
    EnterParms, Local: boolean;
  label RL;
  begin
    QuickExecExternal := False;

    Local := True;
    F := New(PTextReader, Init('DN.XRN'));

    if F = nil then
      begin
RL:
        Local := False;
        F := New(PTextReader, Init(SourceDir+'DN.XRN'));
      end;
    if F = nil then
      exit;
    while not F^.Eof do
      begin
        OS2 := #0;
        TitleStr := F^.GetStr;

        i := PosChar(';', TitleStr);
        if i > 0 then
          SetLength(TitleStr, i-1);
        if TitleStr[1] = '>' then
          begin
            if PosChar(TitleStr[2], '[]><') > 0 then
              OS2 := TitleStr[2];
            {OS2 := Byte(TitleStr[2]=']')+2*Byte(TitleStr[2]='[');}
            if StoI(DelSpaces(Copy(TitleStr, 2+byte(OS2 <> #0),
                MaxStringLength))) = n
            then
              goto 1;
          end;
      end;
    Dispose(F, Done);
    if Local then
      goto RL;
    exit;
1:
    i := DNNumber;
    if OS2 = #0 then
      FreeStr := {$IFNDEF OS2}'BAT' {$ELSE}'CMD' {$ENDIF}
    else
      begin
        i := 0;
        repeat
          ClrIO;
          Inc(i);
          lFindFirst(SwpDir+'$DN'+ItoS(i)+'$.CMD', AnyFile, SR);
            {JO}
          lFindClose(SR);
        until (DOSError <> 0) or (i = 99) or Abort;
        ClrIO;
        FreeStr := 'CMD';
      end;

    FreeStr := SwpDir+'$DN'+ItoS(i)+'$.'+FreeStr;
    lAssignText(F1, FreeStr);
    ClrIO;
    lRewriteText(F1);

    if IOResult <> 0 then
      begin
        Dispose(F, Done);
        exit
      end;
    Writeln(F1.t, '@Echo off');

    if OS2 <> #0 then
      begin
        lGetDir(0, TitleStr); {GetDir(0, TitleStr);} {Cat}
        Writeln(F1.t, '@', Copy(TitleStr, 1, 2));
        Writeln(F1.t, '@cd ', TitleStr);
      end;

    TitleStr := '';
    DefStr := '';
    EnterParms := False;
    Message(Desktop, evBroadcast, cmGetUserParamsWL, @UserParams);
      ClrIO;
    while not F^.Eof do
      begin
        s := F^.GetStr;
        DelRight(s);
        DelLeft(s);
        if (s <> '') and (s[1] = ';') then
          continue; {!!SF}
        if (s <> '') then
          if (s[1] = '>') then
            break
          else if (s[1] = '<') then
            begin
              EnterParms := True;
              Delete(s, 1, 1); {DelFC(S);}
              if (s <> '') and (s[1] = '=') then
                begin
                  Delete(s, 1, 1); {DelFC(S);}
                  {Knave 29.08.99}s := MakeString(s, @UserParams,
                    False, nil);
                  DefStr := s;
                end
              else
                begin
                  DelLeft(s);
                  TitleStr := s;
                end;
              continue;
            end;
        EnterParms := EnterParms or (Pos('%3', s) > 0);
        Writeln(F1.t, MakeString(s, @UserParams, False, nil));
      end;

    with UserParams do
      begin
        if ActiveList <> '' then
          Writeln(F1.t, '@DEL ', ActiveList);
        if PassiveList <> '' then
          Writeln(F1.t, '@DEL ', PassiveList);
      end;

    Write(F1.t, '@DEL ', FreeStr);
    Close(F1.t);
    Dispose(F, Done);

    if OS2 <> #0 then
      s := OS2+'call '+FreeStr
    else
      s := FreeStr;

    if TitleStr = '' then
      TitleStr := GetString(dlMenuParamLabel);

    QuickExecExternal := not EnterParms or (InputBox(GetString(
      dlMenuParams), TitleStr, DefStr, 125, hsInputParams) = cmOK);

    with UserParams do
      begin
        if ActiveList = '' then
          ActiveList := '-';
        if PassiveList = '' then
          PassiveList := '-';
        QuickExecExternalStr := s+' '+ActiveList+' '+PassiveList+' '+
          DefStr;
      end;

end { QuickExecExternal };
{-DataCompBoy-}

end.
