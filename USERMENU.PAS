{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.09
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
unit UserMenu;

interface
uses Objects, Views, Scroller, Drivers, FilesCol, LFNCol, Collect,
       SBlocks;

var QuickExecExternalStr: string;

function QuickExecExternal(N: Integer): Boolean;
function ExecUserMenu(DoGlobal: Boolean): Boolean;
procedure ScreenGrabber(ShowMessage: Boolean);
function MakeString(S: String; UserParams: PUserParams;
                    HandleTildes: boolean; TM: PString): String;

type
     PUserView = ^TUserView;
     TUserView = object(TScroller)
      Grabbing: Boolean;
      constructor Init(var R: TRect; H, V: PScrollbar);
      procedure Draw; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
      procedure ChangeBounds(var Bounds: TRect); virtual;
     end;

     PUserWindow = ^TUserWindow;
     TUserWindow = object(TWindow)
      OldScreenWidth: Word;
      constructor Init;
      procedure CalcBounds(var Bounds: TRect; Delta: TPoint); virtual;
      procedure SetState(AState: Word; Enable: Boolean); virtual;
     end;

const RunFrom2E : Boolean = False;

implementation
uses LFN, {DataCompBoy}
     DNApp, advance, advance1, advance2, advance3, Startup, Messages, Menus,
     Commands, MicroEd, WinClp, DNHelp, DOS, Memory, Dialogs, Tree, RStrings;

type
     PUserMenuItem = ^TUserMenuItem;
     TUserMenuItem = record
      Text: PString;
      Level: Word;
      Line: Word;
      RunFrom2E: Boolean;
     end;

     PUserCollection = ^TUserCollection;
     TUserCollection = object(TCollection)
      procedure FreeItem(P: Pointer); virtual;
     end;

Constructor TUserWindow.Init;
 var R: TRect;
begin
 DeskTop^.GetExtent(R);
 R.Grow(1, 1);
 inherited Init(R, GetString( dlOutputTitle ), 0);
 OldScreenWidth := ScreenWidth;
 GetExtent(R); R.Grow(-1,-1);
 Insert(New(PUserView, Init(R, StandardScrollBar(sbHorizontal+sbHandleKeyboard),
                               StandardScrollBar(sbVertical+sbHandleKeyboard))));
end;

procedure TUserWindow.CalcBounds(var Bounds: TRect; Delta: TPoint);
begin
  if (Size.X <= ScreenWidth) and (ScreenWidth = OldScreenWidth) then
   begin inherited CalcBounds(Bounds, Delta); Exit end;
  DeskTop^.GetExtent(Bounds);
  Bounds.Grow(1, 2); Inc(Bounds.B.Y);
  OldScreenWidth := ScreenWidth;
end;

procedure TUserWindow.SetState(AState: Word; Enable: Boolean);
var
  WindowCommands: TCommandSet;
begin
  inherited SetState(AState, Enable);
  if AState = sfSelected then
  begin
    SetState(sfActive, Enable);
    if Enable then EnableCommands([cmHideLeft,cmHideRight,cmHideInactive])
    else DisableCommands([cmHideLeft,cmHideRight,cmHideInactive]);
  end;
end;

constructor TUserView.Init;
begin
 inherited Init(R, H, V);
 SetLimit(ScreenWidth, ScreenHeight);
 GrowMode := gfGrowHiX+gfGrowHiY;
 EventMask := $FFFF;
 Options := Options or ofSelectable;
 Grabbing := Off;
 ScrollTo( 0,
   Byte(InterfaceData.Options and ouiHideMenu = 0){ +
   Byte(InterfaceData.Options and ouiHideStatus = 0)});
 Delta.Y := VScrollBar^.Value;
end;

procedure TUserView.ChangeBounds;
begin
  inherited ChangeBounds(Bounds);
end;

procedure TUserView.Draw;
var
  I: Integer;
  B: TDrawBuffer;
begin
 for I := 0 to Size.Y - 1 do
  begin
   MoveChar(B, ' ', 07, SizeOf(B) div 2);
   if I + Delta.Y < UserScreenSize div (UserScreenWidth * 2)
     then Move( PWordArray(UserScreen)^[ ( I + Delta.Y ) * UserScreenWidth ], B, UserScreenWidth * 2 );
   WriteLine(0,I,Size.X,1,B[Delta.X]);
  end;
end;

procedure TUserView.HandleEvent;
begin
 inherited HandleEvent(Event);
 SetLimit(ScreenWidth, ScreenHeight);
 case Event.What of
  evCommand: case Event.Command of
              {cmGrabscreen: begin ClearEvent(Event); if Execute = cmOK then  end;}
              cmGetName: PString(Event.Infoptr)^ := GetString( dlOutputTitle );
              cmShowOutput, cmHideRight, cmHideLeft: begin
                             if Event.Command <> cmShowOutPut then
                                begin
                                  if Event.Command = cmHideLeft then Event.Command := cmPostHideLeft
                                                                else Event.Command := cmPostHideRight;
                                  PutEvent(Event);
                                end;
                             if Owner^.GetState(sfSelected) then Message(Owner, evCommand, cmClose, nil)
                               else PView(Event.InfoPtr^) := Owner;
                             ClearEvent(Event);
                            end;
             end;
  evKeyDown: begin
              if Size.X = ScreenWidth then CommandLine^.HandleEvent(Event);
             end;
 end;
end;

procedure TUserCollection.FreeItem(P: Pointer);
begin
 DisposeStr(PUserMenuItem(P)^.Text);
 Dispose(PUserMenuItem(P));
end;

        {-DataCompBoy-}
function MakeString(S: String; UserParams: PUserParams; HandleTildes: boolean; TM: PString): String;
var ts: string;
    tz: string;
    zs: string[1];
begin
  if HandleTildes then zs:=#0 else zs:='';
  Replace('!!',#1, S);
  Replace('&&',#2, S);
  Replace('##',#3, S);
  Replace('$$',#4, S);

  if (Pos('!\',S)>0) or (Pos('!:',S)>0) or (Pos('!/',S)>0) then
   begin
    if UserParams^.Active = nil then ts := '' {KSNK}
    else
    if UserParams^.Active^.Name[1]='.'
    then ts:=MakeNormName(lfGetShortFileName(UserParams^.Active^.Owner^),'')
    else ts:=MakeNormName(GetPath(lfGetShortFileName(MakeNormName(
      UserParams^.Active^.Owner^, MakeFileName(UserParams^.Active^.Name)))),'');
    Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
    Replace('!:', Copy(ts,1,2), S);
    Replace('!\', zs+Copy(ts,3,255)+zs, S);
    Replace('!/', zs+Copy(ts,3,Length(ts)-3)+zs, S);
   end;

  if (Pos('#\',S)>0) or (Pos('#:',S)>0) or (Pos('#/',S)>0) then
   begin
    if UserParams^.Active = nil then ts := '' {KSNK}
    else
    if UserParams^.Active^.Name[1]='.'
    then ts:=MakeNormName(lfGetLongFileName(UserParams^.Active^.Owner^),'')
    else ts:=MakeNormName(GetPath(lfGetLongFileName(MakeNormName(
     UserParams^.Active^.Owner^, MakeFileName(UserParams^.Active^.Name)))),'');
    Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
    Replace('#:', Copy(ts,1,2), S);
    Replace('#\', zs+Copy(ts,3,255)+zs, S);
    Replace('#/', zs+Copy(ts,3,Length(ts)-3)+zs, S);
   end;

  if (Pos('$\',S)>0) or (Pos('$:',S)>0) or (Pos('$/',S)>0) then
   begin
    if UserParams^.Passive = nil then ts := '' {KSNK}
    else
    if UserParams^.Passive^.Name[1]='.'
    then ts:=MakeNormName(lfGetShortFileName(UserParams^.Passive^.Owner^),'')
    else ts:=MakeNormName(GetPath(lfGetShortFileName(MakeNormName(
     UserParams^.Passive^.Owner^, MakeFileName(UserParams^.Passive^.Name)))),'');
    Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
    Replace('$:', Copy(ts,1,2), S);
    Replace('$\', zs+Copy(ts,3,255)+zs, S);
    Replace('$/', zs+Copy(ts,3,Length(ts)-3)+zs, S);
   end;

  if (Pos('&\',S)>0) or (Pos('&:',S)>0) or (Pos('&/',S)>0) then
   begin
    if UserParams^.Passive = nil then ts := '' {KSNK}
    else
    if UserParams^.Passive^.Name[1]='.'
    then ts:=MakeNormName(lfGetLongFileName(UserParams^.Passive^.Owner^),'')
    else ts:=MakeNormName(GetPath(lfGetLongFileName(MakeNormName(
     UserParams^.Passive^.Owner^, MakeFileName(UserParams^.Passive^.Name)))),'');
    Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
    Replace('&:', Copy(ts,1,2), S);
    Replace('&\', zs+Copy(ts,3,255)+zs, S);
    Replace('&/', zs+Copy(ts,3,Length(ts)-3)+zs, S);
   end;

  if Replace('!%', #5, S) or Replace('#%', #5, S) then
   begin
    if (Pos(#5'A', S)>0) or (Pos(#5'a', S)>0) then
     begin
      if UserParams^.Active = nil then ts := '' {KSNK}
      else
      ts:=GetAttrStr(UserParams^.Active^.Attr);
      Replace(#5'A', ts, S);
      Replace(#5'a', ts, S);
     end;

    if (Pos(#5'C', S)>0) or (Pos(#5'c', S)>0) then
     begin
      if UserParams^.Active = nil then ts := '' {KSNK}
      else
      if UserParams^.Active^.Attr and Directory<>0
       then ts:=''
       else ts:=ZtoS(UserParams^.Active^.PSize);
      Replace(#5'C', ts, S);
      Replace(#5'c', ts, S);
     end;

    if (Pos(#5'D', S)>0) or (Pos(#5'd', S)>0) then
     begin
      if UserParams^.Active = nil then ts := '' {KSNK}
      else
      with UserParams^.Active^ do
       MakeDateFull(CountryInfo.DateFmt,Day,Month,Yr,Hour,Minute,ts,True);
      ts[0] := #10;
      Replace(#5'D', ts, S);
      Replace(#5'd', ts, S);
     end;

    if (Pos(#5'R', S)>0) or (Pos(#5'r', S)>0) then
     begin
      if UserParams^.Active = nil then ts := '' {KSNK}
      else
      ts:=ItoS(round(((UserParams^.Active^.PSize/UserParams^.Active^.Size)*100)+0.5))+'%';
      Replace(#5'R', ts, S);
      Replace(#5'r', ts, S);
     end;

    if (Pos(#5'S', S)>0) or (Pos(#5's', S)>0) then
     begin
      if UserParams^.Active = nil then ts := '' {KSNK}
      else
      if UserParams^.Active^.Attr and Directory<>0
       then ts:=''
       else ts:=ZtoS(UserParams^.Active^.Size);
      Replace(#5'S', ts, S);
      Replace(#5's', ts, S);
     end;

    if (Pos(#5'T', S)>0) or (Pos(#5't', S)>0) then
     begin
      if UserParams^.Active = nil then ts := '' {KSNK}
      else
      with UserParams^.Active^ do
       ts:=FormatTimeStr(Hour, Minute, Second);
      Replace(#5'T', ts, S);
      Replace(#5't', ts, S);
     end;

    if (Pos(#5'Z', S)>0) or (Pos(#5'z', S)>0) then
     begin
      if UserParams^.Active = nil then ts := '' {KSNK}
      else
      if (UserParams^.Active^.DIZ<>nil) and (UserParams^.Active^.DIZ^.DIZ<>nil)
       then ts:=UserParams^.Active^.DIZ^.DIZ^ else ts:='';
      Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
      Replace(#5'Z', ts, S);
      Replace(#5'z', ts, S);
     end;

    if (Pos(#5'F', S)>0) or (Pos(#5'f', S)>0) then
     begin
      ts := MakeNormName(lfGetShortFileName(SwpDir),
                         GetName(CalcTmpFName(CalcTmpId, 'flt', True)));
      if TM<>nil then TM^:=ts;
      Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
      Replace(#5'F', ts, S);
      Replace(#5'f', ts, S);
     end;
   end;

  if Replace('$%', #6, S) or Replace('&%', #6, S) then
   begin
    if (Pos(#6'A', S)>0) or (Pos(#6'a', S)>0) then
     begin
      if UserParams^.Passive = nil then ts := '' {KSNK}
      else
      ts:=GetAttrStr(UserParams^.Passive^.Attr);
      Replace(#6'A', ts, S);
      Replace(#6'a', ts, S);
     end;

    if (Pos(#6'C', S)>0) or (Pos(#6'c', S)>0) then
     begin
      if UserParams^.Passive = nil then ts := '' {KSNK}
      else
      if UserParams^.Passive^.Attr and Directory<>0
       then ts:=''
       else ts:=ZtoS(UserParams^.Passive^.PSize);
      Replace(#6'C', ts, S);
      Replace(#6'c', ts, S);
     end;

    if (Pos(#6'D', S)>0) or (Pos(#6'd', S)>0) then
     begin
      if UserParams^.Passive = nil then ts := '' {KSNK}
      else
      with UserParams^.Passive^ do
       MakeDateFull(CountryInfo.DateFmt,Day,Month,Yr,Hour,Minute,ts,True);
      ts[0] := #10;
      Replace(#6'D', ts, S);
      Replace(#6'd', ts, S);
     end;

    if (Pos(#6'R', S)>0) or (Pos(#6'r', S)>0) then
     begin
      if UserParams^.Passive = nil then ts := '' {KSNK}
      else
      ts:=ItoS(round(((UserParams^.Passive^.PSize/UserParams^.Passive^.Size)*100)+0.5))+'%';
      Replace(#6'R', ts, S);
      Replace(#6'r', ts, S);
     end;

    if (Pos(#6'S', S)>0) or (Pos(#6's', S)>0) then
     begin
      if UserParams^.Passive = nil then ts := '' {KSNK}
      else
      if UserParams^.Passive^.Attr and Directory<>0
       then ts:=''
       else ts:=ZtoS(UserParams^.Passive^.Size);
      Replace(#6'S', ts, S);
      Replace(#6's', ts, S);
     end;

    if (Pos(#6'T', S)>0) or (Pos(#6't', S)>0) then
     begin
      if UserParams^.Passive = nil then ts := '' {KSNK}
      else
      with UserParams^.Passive^ do
       ts:=FormatTimeStr(Hour, Minute, Second);
      Replace(#6'T', ts, S);
      Replace(#6't', ts, S);
     end;

    if (Pos(#6'Z', S)>0) or (Pos(#6'z', S)>0) then
     begin
      if UserParams^.Passive = nil then ts := '' {KSNK}
      else
      if (UserParams^.Passive^.DIZ<>nil) and (UserParams^.Passive^.DIZ^.DIZ<>nil)
       then ts:=UserParams^.Passive^.DIZ^.DIZ^ else ts:='';
      Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
      Replace(#6'Z', ts, S);
      Replace(#6'z', ts, S);
     end;
   end;

  if (PosChar('!', S)>0) then
   begin
    if UserParams^.Active = nil then ts := '' {KSNK}
    else
    ts:=UserParams^.Active^.Name;
    ts[9]:='.';
    Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
    tz:=fDelRight(Copy(ts, 9, 4)); if tz='.' then tz:='';
    Replace('.!',zs+tz+zs, S);
    Replace('!', zs+fDelRight(Copy(ts, 1, 8))+zs, S);
   end;

  if (PosChar('$', S)>0) then
   begin
    if UserParams^.Passive = nil then ts := '' {KSNK}
    else
    ts:=UserParams^.Passive^.Name;
    ts[9]:='.';
    Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
    tz:=fDelRight(Copy(ts, 9, 4)); if tz='.' then tz:='';
    Replace('.$',zs+tz+zs, S);
    Replace('$', zs+fDelRight(Copy(ts, 1, 8))+zs, S);
   end;

  if (PosChar('#', S)>0) then
   begin
    if UserParams^.Active = nil then ts := '' {KSNK}
    else
    ts:=GetLFN(UserParams^.Active^.LFN);
    Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
    tz:=GetExt(ts); if tz='.' then tz:='';
    Replace('.#',zs+tz+zs, S);
    Replace('#', zs+GetSName(ts)+zs, S);
   end;

  if (PosChar('&', S)>0) then
   begin
    if UserParams^.Passive = nil then ts := '' {KSNK}
    else
    ts:=GetLFN(UserParams^.Passive^.LFN);
    Replace('$', #4, ts);Replace('!', #1, ts);Replace('&', #2, ts);Replace('#', #3, ts);
    tz:=GetExt(ts); if tz='.' then tz:='';
    Replace('.&',zs+tz+zs, S);
    Replace('&', zs+GetSName(ts)+zs, S);
   end;

  Replace(#1,'!', S);
  Replace(#2,'&', S);
  Replace(#3,'#', S);
  Replace(#4,'$', S);

  MakeString := S;
end;
        {-DataCompBoy-}

function ExecUserMenu;
 var F: PTextReader;
     F1: lText; {DataCompBoy}
     P: PUserCollection;
     S, S1: String;
     I: LongInt;
     UI: PUserMenuItem;
     StartPos: Integer;
     Items, OItems, SItems: PMenuItem;
     Menu: PMenu;
     PV: PView;
     R: TRect;
     NI, NW, LL: Word;
     EnterParms: Boolean;
     TitleStr: String[30];
     DefStr: String;
     RF2E: Boolean;
     Event: TEvent;

 Label 1, 2;

 function CheckFKeys(S: String): Word;
  var I: Integer;
      S1: String;
 begin
  CheckFKeys := kbNoKey;
  for I := 1 to 12 do
   begin
    S1 := 'F'+ItoS(I)+' ';
    if UpStrg(Copy(S, 1, Length(S1))) = S1 then begin CheckFKeys := kbF1+(I-1) shl 8; Exit end;
   end;
 end;

 var MenuLast: PMenuItem;

 function DoSubMenu(Main: boolean): PMenuItem;
   var P1, P2: PMenuItem;
       UI,PU: PUSerMenuItem;
       KB: Word;
 begin
   DoSubMenu := nil;
   if I = P^.Count then Exit;
   UI := P^.At(I);
   {if UI^.Text = nil then begin DoSubMenu := NewLine(DoSubMenu); Exit end;}
   KB := kbNoKey;
   P2 := nil;
   if UI^.Text <> nil then P1 := NewItem(UI^.Text^, '', CheckFKeys(UI^.Text^), 1000 + I,
                                 hcLUserMenu+Byte(DoGlobal), nil)
                      else P1 := NewItem('Empty line', '', kbNoKey, 1000 + I,
                                 hcLUserMenu+Byte(DoGlobal), nil);
   DoSubMenu := P1;
   while (I < P^.Count-1) do
    begin
     Inc(I);
     PU := P^.At(I);
     MenuLast := P1;
     if PU^.Level < UI^.Level then begin Dec(I); Exit; end;
     if PU^.Level > UI^.Level then begin P1^.SubMenu := NewMenu(DoSubMenu(False)); P1^.Command := 0; end
      else begin
             if PU^.Text <> nil
              then P1^.Next := NewItem(PU^.Text^, '', CheckFKeys(PU^.Text^),
                                   1000 + I, hcLUserMenu+Byte(DoGlobal), nil)
              else P1^.Next := NewLine(nil);
             if Main then inc(NI);
             P1 := P1^.Next;
           end;
     MenuLast := P1;
    end;
 end;

        {-DataCompBoy-}
 var UserParams: TUserParams;

begin
 FillChar(UserParams, SizeOf(UserParams), 0);
 Message(Desktop, evBroadcast, cmGetUserParams, @UserParams);
 RunFrom2E := Off;
 ExecUserMenu := False;
 TitleStr := '';
 DefStr := '';
 EnterParms := False;
 if DoGlobal then Goto 2;

 F := New(PTextReader, Init(SwpDir+'$dn'+ItoS(DNNumber)+'$.mnu'));
 if F <> nil then Goto 1;

 S := lFExpand('dn.mnu');
 I := Length(S); if Abort then Goto 2;
 While (I > 0) and (S[I] <> '\') do Dec(I);
 if I = 0 then Goto 2;
 repeat
   F := New(PTextReader, Init(S));
   if F <> nil then Goto 1;

   While (I > 2) and (S[I-1] <> '\') do
     begin Delete(S,I-1,1); Dec(I); end;
   Delete(S,I-1,1); Dec(I);
 until I < 3;
2:
 DoGlobal := True;
 F := New(PTextReader, Init(SourceDir+'dn.mnu'));
 if F = nil then
  begin
   ErrMsg(dlMNUNotFound);
   Exit;
  end;
1:
 Message(Desktop, evBroadcast, cmGetUserParams, @UserParams); ClrIO;
 New(P, Init(10, 10)); I := 0; NW := 30;
 while (not F^.EOF) and (IOResult = 0) do
  begin
   S := F^.GetStr; Inc(I);
   DelLeft(S); DelRight(S);
   if (S <> '') and (S[1]<>';') and (S[1] = '>') then
    begin
     RF2E := Off;
     DelFC(S);
     if S[1] = '>' then begin DelFC(S); RF2E := On end;
     S1 := ''; repeat S1 := S1+S[1]; DelFC(S); until (S = '') or (S[1] = ' ');
     DelFC(S);
     if (StoI(S1) > 0) then
      begin
       New(UI);
       S := MakeString(S, @UserParams, on, nil);
       UI^.Text := NewStr(S);
       UI^.Level := StoI(S1);
       if (CStrLen(S) > NW) and (UI^.Level=1) then NW := CStrLen(S);
       UI^.Line := I;
       UI^.RunFrom2E := RF2E;
       P^.Insert(UI);
      end;
    end;
  end;
 S := F^.FileName;
 Dispose(F,Done);
 if P^.Count > 0 then
  begin
   StartPos := -1;
   repeat
     I := 0; OItems := nil; SItems := nil; NI := 3;
     SItems := DoSubMenu(On);
     if SItems = nil then Break;
     Menu := NewMenu(SItems);
     Application^.GetExtent(R);
     R.A.X:=0; R.B.X:=NW;
     R.B.Y:=Min(R.B.Y, NI);
     PV := New(PMenuBox, Init(R, Menu, nil));
     PV^.Options := PV^.Options or ofCentered;
     I := Application^.ExecView(PV);
     Dispose(PV,Done);
     DisposeMenu(Menu);
     if I < 1000 then
      begin
        EraseFile(UserParams.ActiveList);
        EraseFile(UserParams.PassiveList);
        if I = cmEditMenu then
         begin
            Event.What := evCommand;
            Event.Command :=  Byte(not DoGlobal)*cmLocalMenuFileEdit +
                              Byte(DoGlobal)*cmMenuFileEdit;
            Event.InfoPtr := nil;
            Application^.PutEvent(Event);
         end;
        Break;
      end;
     StartPos := I - 999;
     if (StartPos >= P^.Count - 1) or
        (PUserMenuItem(P^.At(StartPos-1))^.Level >= PUserMenuItem(P^.At(StartPos))^.Level) then
        begin
         ExecUserMenu := True;
         I := I - 1000; NW := I;
         UI := P^.At(I); LL := UI^.Line;
         if I >= P^.Count - 1 then LL := 65535 else LL := PUserMenuItem(P^.At(I+1))^.Line - 1;
         F := New(PTextReader, Init(S));
         lAssignText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT');
         lRewriteText(F1);
         Writeln(F1.T, '@Echo off');
         I := I - 1001;
         if F<>nil then
         begin
           for I := 0 to UI^.Line-1 do S := F^.GetStr;
           while not F^.EOF do
            begin
             S := F^.GetStr;
             DelLeft(S); DelRight(S);
             if (S <> '') then
               if (S[1] = '>') then Break else
                  if (S[1] = '<') then
                    begin
                     EnterParms := On;
                     DelFC(S);
                     if (S <> '') and (S[1] = '=') then
                      begin DelFC(S);
{Knave 29.08.99}      S := MakeString(S, @UserParams, off, nil);
                      DefStr := S;
                      end
                      else
                       begin
                        DelLeft(S);
                        TitleStr := S;
                       end;
                     Continue;
                    end;
              EnterParms := EnterParms or (Pos('%3', S) > 0);
              WriteLn(F1.T, MakeString(S, @UserParams, off, nil));
              RunFrom2E := UI^.RunFrom2E;
            end;
         end;
         Close(F1.T);
         Dispose(F,Done);
         Break;
        end;
   until False;
  end;
 Dispose(P,Done);
 FreeStr := DefStr;
 if TitleStr = '' then TitleStr := GetString(dlMenuParamLabel);
 if EnterParms then
   ExecUserMenu := InputBox(GetString(dlMenuParams), TitleStr, FreeStr, 125, hsInputParams) = cmOK;
 with UserParams do
  begin
   if ActiveList = '' then ActiveList := '-';
   if PassiveList = '' then PassiveList := '-';
   FreeStr := ActiveList + ' ' + PassiveList + ' ' + FreeStr;
  end;
 EraseFile(SwpDir+'$dn'+ItoS(DNNumber)+'$.mnu');
end;
        {-DataCompBoy-}

type
     PGrabber = ^TGrabber;
     TGrabber = object(TView)
       ModalEnd: Boolean;
       Screen: PWordArray;
       R: TRect;
       BufSize: Word;
       constructor Init;
       destructor Done; virtual;
       procedure HandleEvent(var Event: TEvent); virtual;
       procedure Draw; virtual;
       function Execute: Word; virtual;
     end;

const Top: TPoint = (X:10;Y:5);
      Bot: TPoint = (X:21;Y:6);
      NotMessage: Boolean = On;
      Here: Boolean = Off;

procedure ScreenGrabber(ShowMessage: Boolean);
 var P: PGrabber;
     B: Boolean;
begin
  if Here then Exit;
  Here := On;
  if NotMessage and ShowMessage then
   begin
     MessageBox(GetString(dlGrabWelcome),
                nil, mfOKButton + mfInformation);
     NotMessage := Off;
   end;
  B := MsgActive; MsgActive := Off;
  P := New(PGrabber, Init);
  Application^.ExecView(P);
  Dispose(P,Done);
  Here := Off;
  MsgActive := B;
end;

constructor TGrabber.Init;
  var BB: TRect;
begin
  Application^.GetExtent(BB);
  inherited Init(BB);
  R.A := Top; R.B := Bot;
  BufSize := Application^.Size.X*2*Application^.Size.Y;
  Screen := memAlloc(BufSize);
  if Screen = nil then Fail;
  HideMouse;
  Move(ScreenBuffer^, Screen^, BufSize);
  ShowMouse;
  if Screen = nil then Fail;
  Options := Options or ofSelectable or ofTopSelect;
  {EventMask := $FFFF;}
end;


destructor TGrabber.Done;
begin
  if Screen <> nil then FreeMem(Screen, BufSize);
  inherited Done;
end;


procedure TGrabber.Draw;

 var B: TDrawBuffer;
     I: Integer;

      procedure Invert(var B; K: integer);
       assembler;
       {$IFNDEF BIT_32}
       asm
         mov cx, K
         les bx, B
        @@1:
         mov ax, word ptr es:[bx]
         xor ah, $7F
         and ah, $7F
         jnz @@2
         mov ah, 7
       @@2:
         mov word ptr es:[bx], ax
         add bx, 2
         loop @@1
       end;
       {$ELSE}{&Frame-}{$USES EBX, ECX}
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
       end;
       {$ENDIF}
begin
  with R do
   begin
     if A.X < 0 then Move(-A.X,0);
     if A.Y < 0 then Move(0,-A.Y);
     if B.X - A.X > Size.X then B.X := A.X + Size.X;
     if B.Y - A.Y > Size.Y then B.Y := A.Y + Size.Y;
     if B.X > Size.X then Move(Size.X-B.X, 0);
     if B.Y > Size.Y then Move(0, Size.Y-B.Y);
   end;
  Top := R.A; Bot := R.B;
  for I := 0 to Size.Y-1 do
   begin
     Move(Screen^[Size.X*I], B, Size.X*2);
     if (I >= R.A.Y) and (I < R.B.Y) and (R.A.X < R.B.X) then
          Invert(B[R.A.X], R.B.X - R.A.X);
     WriteLine(0,I,Size.X,1,B);
   end;
end;

function TGrabber.Execute;
 var E: TEvent;
begin
   ModalEnd := Off;
   DrawView;
   repeat
     GetEvent(E);
     if E.What <> evNothing then HandleEvent(E);
   until ModalEnd;
end;

procedure TGrabber.HandleEvent;
  var B: Boolean;
      Sh: Boolean;
      Stp: Integer;
  procedure CED; begin ClearEvent(Event); DrawView end;

  procedure MakeClip;
  var
    I, J: Integer;
    S: String;
    C: Char;
  begin
    if Clipboard <> nil then Dispose(Clipboard,Done);
    Clipboard := New(PLineCollection, Init(R.B.Y - R.A.Y,10));
    for I:=R.A.Y to R.B.Y-1 do
    begin
      S:='';
      for J:=R.A.X to R.B.X-1 do begin
        C:=Char(WordRec(Screen^[I*Size.X+J]).Lo);
        if C=#0 then C:=' ';
        S:=S+C;
      end;
      Clipboard^.Insert(NewStr(S));
    end;
    if SystemData.Options and ossUseSysClip <> 0 then SyncClipIn;
    if ClipBoardStream<>nil
     then ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-1, 0));
    CopyLines2Stream(Clipboard, ClipBoardStream);
   end;

begin
  B := On;
  inherited HandleEvent(Event);
  case Event.What of
    evKeyDown:
      begin
        Sh := ShiftState and 3 <> 0;
        if ShiftState and 4 <> 0 then Stp := 8 else Stp := 1;
        case Event.KeyCode of
          kbPgUp, kbCtrlPgUp:  R.Move(0, -R.A.Y);
          kbHome, kbCtrlHome:  R.Move(-R.A.X, 0);
          kbEnd,  kbCtrlEnd:   R.Move(Size.X, 0);
          kbPgDn, kbCtrlPgDn:  R.Move(0, Size.Y);
          kbRight,kbCtrlRight: if Sh then begin Inc(R.B.X,Stp);R.B.X := Min(R.B.X, Size.X) end else R.Move(Stp,0);
          kbLeft, kbCtrlLeft:  if Sh then
                               begin
                                 if R.B.X > R.A.X+Stp then Dec(R.B.X, Stp)
                                                      else R.B.X := R.A.X+1;
                               end else R.Move(-Stp,0);
          kbEnter: begin
                     ModalEnd := On;
                     MakeClip;
                   end;
          kbESC: begin
                     ModalEnd := On;
                   end;
          else
          begin
            if Stp>1 then Stp := Stp div 2;
            case Event.KeyCode of
              kbUp,   kbCtrlUp:    if Sh then
                                   begin
                                     if R.B.Y > R.A.Y+Stp then Dec(R.B.Y,Stp)
                                                          else R.B.Y := R.A.Y+1;
                                   end else R.Move(0, -Stp);
              kbDown, kbCtrlDown : if Sh then begin Inc(R.B.Y,Stp);R.B.Y := Min(R.B.Y, Size.Y) end else R.Move(0,Stp);
              else B := Off;
            end;
          end;
        end;
      end
    else B := Off;
  end;
  if B then CED;
end;


function QuickExecExternal(N: Integer): Boolean;

 label 1;

 var UserParams: TUserParams;
     TitleStr, DefStr, S: String;
     F: PTextReader;
     F1: lText;
     I: Integer;
     OS2: char;
     SR: lSearchRec;
     EnterParms, Local: Boolean;
  label RL;
begin
  QuickExecExternal := Off;

  Local := On;
  F := New(PTextReader, Init('DN.XRN'));

  if F = nil then
    begin
  RL:
      Local := Off;
      F := New(PTextReader, Init(SourceDir+'DN.XRN'));
    end;
  if F = nil then Exit;
  while not F^.EOF do
    begin
      OS2 := #0;
      TitleStr := F^.GetStr;

      I := PosChar(';', TitleStr);
      if I > 0 then TitleStr[0] := Char(I-1);
      if TitleStr[1] = '>' then
        begin
          if PosChar(TitleStr[2],'[]><')>0 then OS2 := TitleStr[2] ;
           {OS2 := Byte(TitleStr[2]=']')+2*Byte(TitleStr[2]='[');}
          if StoI(DelSpaces(Copy(TitleStr, 2+Byte(OS2<>#0), 255))) = N then Goto 1;
        end;
    end;
  Dispose(F,Done);
  if Local then Goto RL;
  Exit;
1:
  I := DNNumber;
  if OS2 = #0 then FreeStr := 'BAT'
    else
    begin
       I := 0;
       repeat
         ClrIO;
         Inc(I);
         lFindFirst(SwpDir+'$DN'+ItoS(I)+'$.CMD', $3F xor VolumeID, SR);
         lFindClose(SR);
       until (DOSError <> 0) or (I = 99) or Abort;
       ClrIO;
       FreeStr := 'CMD';
    end;

  FreeStr := SwpDir+'$DN'+ItoS(I)+'$.'+FreeStr;
  lAssignText(F1, FreeStr); ClrIO;
  lRewriteText(F1);

  if IOResult <> 0 then begin Dispose(F,Done); Exit end;
  WriteLn(F1.T, '@Echo off');

  if OS2 <> #0 then
    begin
       GetDir(0, TitleStr);
       WriteLn(F1.T, '@', Copy(TitleStr, 1, 2));
       WriteLn(F1.T, '@cd ', TitleStr);
    end;

  TitleStr := ''; DefStr := ''; EnterParms := Off;
  Message(Desktop, evBroadcast, cmGetUserParamsWL, @UserParams); ClrIO;
  while not F^.EOF do
   begin
     S := F^.GetStr;
     DelRight(S);
     DelLeft(S);
     if (S <> '') and (S[1]=';') then Continue; {!!SF}
     if (S <> '') then
       if (S[1] = '>') then Break else
          if (S[1] = '<') then
            begin
             EnterParms := On;
             DelFC(S);
             if (S <> '') and (S[1] = '=') then
                      begin DelFC(S);
{Knave 29.08.99}      S := MakeString(S, @UserParams, off, nil);
                      DefStr := S;
                      end
              else
               begin
                DelLeft(S);
                TitleStr := S;
               end;
             Continue;
            end;
         EnterParms := EnterParms or (Pos('%3', S) > 0);
         WriteLn(F1.T, MakeString(S, @UserParams, off, nil));
       end;

with UserParams do
 begin
  if ActiveList  <> '' then Writeln(F1.T, '@DEL ', ActiveList  );
  if PassiveList <> '' then Writeln(F1.T, '@DEL ', PassiveList );
 end;

  Write(F1.T, '@DEL ', FreeStr);
  Close(F1.T);
  Dispose(F,Done);

  if OS2<>#0 then S := Os2 + 'call ' + FreeStr
             else S :=FreeStr ;

  if TitleStr = '' then TitleStr := GetString(dlMenuParamLabel);

  QuickExecExternal := not EnterParms or (InputBox(GetString(dlMenuParams), TitleStr, DefStr, 125, hsInputParams) = cmOK);

  with UserParams do
   begin
    if ActiveList = '' then ActiveList := '-';
    if PassiveList = '' then PassiveList := '-';
    QuickExecExternalStr := S + ' ' + ActiveList + ' ' + PassiveList + ' ' + DefStr;
   end;

end;
        {-DataCompBoy-}

end.
