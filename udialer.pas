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
//
//////////////////////////////////////////////////////////////////////////}

{$I STDEFINE.INC}
unit uDialer;

interface
uses Dialogs, Views, Dnapp, StrView, xTime, Drivers, Objects, Phones,
     Advance1, Commands, ModemIO, Advance3, Messages, Startup, advance;

Type
     PDialBox = ^TDialBox;
     TDialBox = object(TListBox)
        function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
{        destructor Done; virtual;}
     end;

     PAutoDialer = ^TAutoDialer;
     TAutoDialer = object(TDialog)
      HoldButton,UnholdButton: PButton;

      List: PListBox;
      Trm: PDStringView;
      Phones: PPhoneCollection;
      Phone, Name: String;
      ModemS: String;
      SPosX: Integer;
      Static, Hold, Pause: Boolean;
      PauseTimer: TEventTimer;
      UpdTimer: TEventTimer;
      isValid: Boolean;
      L: Array [1..2] of Pointer;
      constructor Init(AName, APhone: String);
      procedure SetPhone(NewName, NewPhone: String);
      procedure HandleEvent(var Event: TEvent); virtual;
      function Valid(C: Word): Boolean; virtual;
      procedure Update; virtual;
      constructor Load(var S: TStream);
      procedure Store(var S: TStream);
      destructor Done; virtual;
     end;

const
      Dialer: PAutoDialer = nil;


implementation

uses Terminal, DnHelp;

function TDialBox.GetText;
 var S: String;
     P: PPhone;
begin
 P := List^.At(Item);
 S := AddSpace(P^.Name, 30);
 if TypeOf(P^)=TypeOf(TPhone) then S := S + ' '+AddSpace(P^.Number^,23);
 GetText := S;
end;

(*
destructor TDialBox.Done;
begin
 Inherited Done;
 if List<>Nil then Dispose(List,Done);
end; *)

constructor TAutoDialer.Init;
 var P: PView;
     R: TRect;
begin
  R.Assign(0,1,54,12);
  inherited Init(R, GetString(dlAutoDialer));
  HelpCtx := hcDialer;
  Number := GetNum;

  {Options := Options or ofCentered;}
  if (InitModem = moInitFail) or (COMPort = nil) then Fail;
  isValid := On;
  New(Phones, Init(10,10));
  R.Assign(34,2,35,7);
  P := New(PScrollBar, Init(R));
  Insert(P);

  R.Assign(2,2,34,7);
  List := New(PDialBox, Init(R, 1, PScrollBar(P)));
  Insert(List);

  R.Assign(2,1,34,2);
  Insert(New(PLabel, Init(R, GetString(dlAD_Queue), List)));

  R.Assign(2,8,35,10);
  New(Trm, Init(R));
  Insert(Trm);
  R.Assign(2,7,35,8);
  Insert(New(PLabel, Init(R, GetString(dlAD_Status), Trm)));

  List^.NewList(Phones);
  R.Assign(36,2,52,4);
  Insert(New(PButton, Init(R, GetString(dlAD_Redial), cmYes, 0)));
  R.Assign(36,4,52,6);
  Insert(New(PButton, Init(R, GetString(dlAD_ForceDial), cmNo, 0)));
  R.Assign(36,6,52,8);
  Insert(New(PButton, Init(R, GetString(dlAD_Delete), cmOk, 0)));
  R.Assign(36,8,52,10);
  HoldButton := New(PButton, Init(R, GetString(dlAD_Hold), cmHold, 0));
  Insert(HoldButton);
  UnholdButton := New(PButton, Init(R, GetString(dlAD_Unhold), cmUnhold, 0));
  UnholdButton^.Hide;
  Insert(UnholdButton);
  SelectNext(Off);
  SetPhone(AName, APhone);
  Dialer := @Self;
  ModemAnswer := '';
  ModemS := '';
  SPosX := 1;
  RegisterToPrior(@Self);
end;

function TAutoDialer.Valid;
begin
  Valid := inherited Valid(C) and isValid;
end;

constructor TAutoDialer.Load;
begin
   inherited Load(S);
   Phones := PPhoneCollection(S.Get);
   S.Read(Name, 1);
   S.Read(Name[1], Length(Name));
   S.Read(Phone, 1);
   S.Read(Phone[1], Length(Phone));
   S.Read(Hold, 1);
   GetSubViewPtr(S, List);
   GetSubViewPtr(S, Trm);
   GetSubViewPtr(S, HoldButton);
   GetSubViewPtr(S, UnholdButton);
   Trm^.S1 := '';
   Trm^.S2 := '';
   ModemS := '';
   SPosX := 0;
   if List <> nil then List^.NewList(Phones);
   Static := On;
   Dialer := @Self;
   isValid := (InitModem <> moInitFail) and (COMPort <> nil);
   RegisterToPrior( @Self );
end;

procedure TAutoDialer.Store;
begin
   if List <> nil then List^.List := nil;
   inherited Store(S);
   if List <> nil then List^.List := Phones;
   S.Put(Phones);
   S.Write(Name, 1+Length(Name));
   S.Write(Phone, 1+Length(Phone));
   S.Write(Hold, 1);
   PutSubViewPtr(S, List);
   PutSubViewPtr(S, Trm);
   PutSubViewPtr(S, HoldButton);
   PutSubViewPtr(S, UnholdButton);
end;

destructor TAutoDialer.Done;
begin
 Dialer := nil;
 if Trm    <> nil then Dispose(Trm,Done);   Trm   := nil;
 if List   <> nil then Dispose(List,Done);  List  := nil;
 if Phones <> nil then Dispose(Phones,Done);Phones:= nil;
 inherited Done;
end;

procedure TAutoDialer.SetPhone;
 var I: Integer;
     P: PPhone;
     C: Char;
     T: TEventTimer;
 label 1;
begin
 NewTimer(UpdTimer, 18);
 if ComPort^.CheckDCD then
  begin
    Static := On;
    Exit;
  end;
 if Phone <> '' then ModemWrite(ModemSetup.Escape+#13'ATE0'#13);
 NewTimerSecs(T, 1);
 repeat
   while not TimerExpired(T) and COMPort^.CharReady do COMPort^.GetChar(C);
 until TimerExpired(T);
 Phone := NewPhone;
 Name := NewName;
 for I := 1 to Phones^.Count do
  begin
    if PPhone(Phones^.At(I-1))^.Number^ = Phone then
      begin
        P := Phones^.At(I-1);
        Phones^.AtDelete(I-1);
        Phones^.AtInsert(0, P);
        Goto 1;
      end;
  end;
 if Phone <> '' then
  begin
    New(P, Init(Phone, Name, '', ''));
    Phones^.AtInsert(0, P);
  end;
 List^.SetRange(Phones^.Count);
1:
 List^.DrawView;
 if Phone <> '' then
  begin
   if Trm <> nil then
    begin
      Trm^.S1 := GetString(dlAD_Dialing)+Phone;
      ModemS := ''; SPosX := 1;
      Trm^.DrawView;
    end;
   Dial(Phone);
  end else Hold := True;
end;

procedure TAutoDialer.HandleEvent;
 var P, PP: PPhone;

 procedure CE; begin ClearEvent(Event) end;

  Procedure UnHold;
  begin
   with Trm^ do
    begin
      S1 := '';
      S2 := '';
      DrawView;
    end;
   HoldButton^.Show;
   UnholdButton^.Hide;
   DrawView;
  end;

  procedure Redial;
  begin
    UnHold;
    P := Phones^.At(0);
    SetPhone(P^.Name, P^.Number^);
    ModemAnswer := '';
    CE
  end;

Label 1;
begin
 case Event.What of
  evKeyDown: case Event.KeyCode of
               {kbUp: begin MoveTo(Origin.X, Origin.Y-1); CE end;
               kbDown: begin MoveTo(Origin.X, Origin.Y+1); CE end;
               kbLeft: begin MoveTo(Origin.X-1, Origin.Y); CE end;
               kbRight: begin MoveTo(Origin.X+1, Origin.Y); CE end;}
               kbESC: begin
                       Message(Application, evCommand, cmClose, nil);
                       ClearEvent(Event);
                       Exit;
                      end;
             end;
  evBroadcast: case Event.Command of
                 cmCloseDialer: begin CE; Close; Exit end;
               end;
  evCommand: case Event.Command of
               cmGetName: PString(Event.InfoPtr)^ := GetString(dlAD_Name) + Trm^.S1;
               cmYes: Redial;
               cmNo : begin
                        if List^.Focused <> 0 then
                          begin
                             UnHold;
                             PP := Phones^.At(List^.Focused);
                             Phones^.AtDelete(List^.Focused);
                             P := Phones^.At(0);
                             Phones^.AtDelete(0);
                             Phones^.AtInsert(Phones^.Count, P);
                             Phones^.AtInsert(0, PP);
                          end;
                        Redial;
                        List^.FocusItem(0);
                      end;
               cmOK : begin
                        if (Phones <> nil) and (Phones^.Count > 0) then
                         begin
                           Phones^.AtFree(List^.Focused);
                           List^.SetRange(Phones^.Count);
                         end;
                        if Phones^.Count = 0 then
                         begin
                           SetPhone('', '');
                           Message(@Self, evCommand, cmClose, nil);
                           Exit;
                         end;
                        Redial;
                      end;
              cmHold: begin
                        Hold := On;
    1:                  Static := On;
                        Phones^.At(0);
                        with Trm^ do
                         begin
                           P := Phones^.At(List^.Focused);
                           S1 := GetString(dlAD_Holded1) +' '+ P^.Number^;
                           S2 := GetString(dlAD_Holded2);
                           DrawView;
                         end;
                         SetPhone('', '');
                         HoldButton^.Hide;
                         UnholdButton^.Show;
                         DrawView;
                       end;
             cmUnhold: begin
                        if not Hold then Goto 1;
                        Hold := Off;
                        Static := COMPort^.CheckDCD;
                        UnHold;
                        Redial;
                        ModemS := '';
                        CE;
                       end;
             end;
 end;
 inherited HandleEvent(Event);
end;

procedure TAutoDialer.Update;
 const InZdes: Boolean = False;
 var Status: String;
     PP: PView;
     P: PPhone;
     I: Integer;
     C: Char;
     Tmr: TEventTimer;
     Event: TEvent;

  procedure Cheers;
    var I,J: Integer;
  begin
    for J := 1 to 2 do
      begin
        for I := 1 to 5 do
          begin
            {$IFDEF VIRTUALPASCAL}
            PlaySound(1000, 55);
            PlaySound(500, 55);
            {$ELSE}
            Sound(1000);
            DelayTics(1);
            Sound(500);
            DelayTics(1);
            {$ENDIF}
          end;
        NoSound;
        {$IFDEF VIRTUALPASCAL}
        PlaySound(200, 55);
        {$ELSE}
        Sound(200);DelayTics(1);NoSound;
        {$ENDIF}
      end;
  end;

begin
 if InZdes then Exit;
 if Hold then Exit;
 if Pause then
  begin
    if TimerExpired(PauseTimer) then
      begin
        Pause := False;
        Message(@Self, evCommand, cmYes, nil);
        Exit;
      end else Exit;
  end;
 if Static and not COMPort^.CheckDCD then
   begin
     Static := Off;
     Message(@Self, evCommand, cmYes, nil);
     Exit;
   end;
 I := 0;
 if Static then Exit;
 while (COMPort^.CharReady) and (I < 100) do
  begin
    Inc(I);
    COMPort^.GetChar(C);
    case C of
      #13: SPosX := 1;
      #10: begin
             SPosX := 1; {ModemS := '';}
             Break;
           end;
      #8: if SPosX > 1 then begin Dec(SPosX); System.Delete(ModemS, SPosX, 1) end;
      else begin
             if SPosX = 1 then ModemS := '';
             if SPosX > Length(ModemS) then AddStr(ModemS, C)
                                       else ModemS[SPosX] := C;
             Inc(SPosX);
           end;
    end;
    if ModemS[0] > #230 then System.Delete(ModemS, 1, 50);
  end;
  if (I > 0) then
    begin
     Trm^.S2 := ModemS;
     Trm^.DrawView;
    end;
  if TimerExpired(UpdTimer) and (SPosX = 1) and (ModemS <> '') then
    begin
      NewTimer(UpdTimer, 18);
      if (Pos(maNoCarrier, ModemS) <> 0) or (Pos(maBusy, ModemS) <> 0) or
         (Pos(maNoAnswer, ModemS) <> 0) then
        begin
          Phones^.AtInsert(Phones^.Count, Phones^.At(0));
          Phones^.AtDelete(0);
          Pause := True;
          NewTimerSecs(PauseTimer, Max(2, StoI(ModemSetup.Redial)));
          Trm^.S1 := '';
          Trm^.S2 := '';
          Trm^.DrawView;
          ModemS := '';
          List^.DrawView;
          InZdes := False;
          Exit;
        end else if (Pos(maNoDial, ModemS) <> 0) then
              begin
                 InZdes := On;
                 MessageBox(GetString(dlAD_NoDial), nil, mfError + mfOKButton);
                 InZdes := Off;
                 Message(@Self, evCommand, cmCancel, nil);
                 ModemS := '';
              end else if (Pos(maVoice, ModemS) <> 0) or (Pos(maConnect, ModemS) <> 0) then
                   begin
                     InZdes := On;
                     PP := _WriteMsg(GetString(dlAD_ModemSays)+ModemS);
                     NewTimer(Tmr, 36);
                     if TerminalDefaults.Options and toConnectSnd <> 0 then Cheers;
                     repeat
                        GetEvent(Event);
                     until (Event.What and (evKeyDown or evMouseDown) <> 0) or
                           (TimerExpired(Tmr));
                     InZdes := Off;
                     PP^.Free;
                     Phones^.AtFree(0);
                     List^.SetRange(Phones^.Count);
                     Static := On;
                     if (Pos(maVoice, ModemS) <> 0) then Message(@Self, evCommand, cmCancel, nil);
                     ModemS := '';
                     if Term = nil then Message(Application, evCommand, cmTerminal, nil);
                     if Phones^.Count = 0 then
                       begin
                         Event.What := evBroadcast;
                         Event.Command := cmCloseDialer;
                         Event.InfoPtr := nil;
                         PutEvent(Event);
                       end;
                   end;
    end;
end;

end.
