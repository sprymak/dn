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

unit Phones;

interface

uses
  xTime, Objects, DNApp, Drivers, Dialogs, Menus, Views, DNStdDlg,
    RStrings,
  DNHelp, advance, advance1, advance2, advance3, Messages, ObjType,
    Collect,
  StrView;

type
  PPhoneCollection = ^TPhoneCollection;
  TPhoneCollection = object(TSortedCollection)
    Constructor Init(ALimit, ADelta: longInt);
    Constructor Load(var s: TStream);
    Constructor ShortLoad(var s: TStream);
    procedure ShortStore(var s: TStream);
    function Compare(P1, P2: Pointer): integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    end;

  PPhoneDir = ^TPhoneDir;
  TPhoneDir = object(TObject)
    Name: String[30];
    Memo1: PString;
    Memo2: PString;
    Password: String[15];
    Phones: PCollection;
    Encrypted: boolean;
    Constructor Init(const APassword, AName, AMemo1, AMemo2: String);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    destructor Done; virtual;
    end;

  PPhone = ^TPhone;
  TPhone = object(TObject)
    Name: String[30];
    Memo1: PString;
    Memo2: PString;
    Number: PString;
    Constructor Init(const ANumber, AName, AMemo1, AMemo2: String);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    destructor Done; virtual;
    end;

  PPhoneBox = ^TPhoneBox;
  TPhoneBox = object(TSortedListBox)
    GroupLabel, ItemLabel: PLabel;
    AlphaMode, SearchMode: boolean;
    Phones: PCollection;
    active: PPhoneDir;
    Info: PDStringView;
    destructor Done; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetText(Item: longInt; MaxLen: integer): String;
      virtual;
    function GetKey(var s: String): Pointer; virtual;
    procedure SetList(Alpha: boolean);
    {      procedure SetState(AState: Word; Enable: Boolean); virtual;}
    end;

procedure PhoneBook(Manual: boolean);

implementation
uses
  Startup, Commands {$IFDEF Modem}, modemio, Terminal, uDialer
    {$ENDIF};

var
  EnterButton, ReturnButton: PButton;

function CryptWith(Pass, s: String): String;
  var
    i: integer;
  begin
    if Pass <> '' then
      begin
        for i := 1 to Length(s) do
          s[i] := Char(byte(s[i]) xor byte(Pass[1+i mod Length(Pass)])
            xor (i xor $AA));
      end;
    CryptWith := s;
  end;

var
  PPH: PPhone;

procedure PhoneBook(Manual: boolean);
  var
    D: PDialog;
    R: TRect;
    PV: PView;
    PL: PPhoneBox;
    PC: PCollection;
    s: TBufStream;
    DT: record
      C: PCollection;
      n: word;
      end;
    W: PWindow;
    i: integer;
    SS: String;

  procedure DoSearchButton(P: PView);
    begin
      if (TypeOf(P^) = TypeOf(TButton)) then
        if PButton(P)^.Command = cmOK then
          EnterButton := PButton(P)
        else if PButton(P)^.Command = cmNo then
          ReturnButton := PButton(P);
    end;

  begin { PhoneBook }
    PPH := nil;
    SS := '';
    if Manual then
      begin
        if (ExecResource(dlgManualDial, SS) <> cmOK) or (DelSpaces(SS) =
            '')
        then
          exit;
        New(PPH, Init(SS, GetString(dlMD_Manual)+SS, '', ''));
      end
    else
      begin
        D := PDialog(LoadResource(dlgPhoneBook));
        D^.ForEach(@DoSearchButton);
        ReturnButton^.Hide;
        PV := D^.StandardScrollBar(sbVertical+sbHandleKeyboard);
        R.Assign(D^.Size.X-3, 3, D^.Size.X-2, 12);
        PV^.Locate(R);

        R.Assign(2, 3, D^.Size.X-3, 12);
        PL := New(PPhoneBox, Init(R, 1, PScrollBar(PV)));
        PL^.Options := PL^.Options or ofPostProcess;
        s.Init(SourceDir+'DN.PHN', stOpenRead, 1024);
        PC := nil;
        PC := PCollection(s.Get);
        if PC = nil then
          begin
            s.Reset;
            s.Seek(0);
            PC := PCollection(New(PPhoneCollection, ShortLoad(s)));
          end;
        s.Done;
        if PC = nil then
          PC := New(PPhoneCollection, Init(10, 10));
        PC^.Pack;
        PL^.Phones := PC;
        PL^.active := nil;
        PL^.AlphaMode := False;
        PL^.SearchMode := False;
        PL^.NewLisT(PC);

        D^.Insert(PL);
        R.Assign(2, 2, 53, 3);

        {    PV := New(PLabel, Init(R, GetString(dlPhonesLabel),PL));}
        PL^.GroupLabel := New(PLabel, Init(R, GetString(
          dlPhonesLabelGroup), PL));
        D^.Insert(PL^.GroupLabel);

        PL^.ItemLabel := New(PLabel, Init(R, GetString(
          dlPhonesLabelPhones), PL));
        PL^.ItemLabel^.Hide;
        D^.Insert(PL^.ItemLabel);


        D^.Insert(PL);
        {D^.Insert(PV);}
        R.Assign(2, 12, D^.Size.X-2, 14);
        PV := New(PDStringView, Init(R));
        PDStringView(PV)^.s1 := '';
        PDStringView(PV)^.s2 := '';
        D^.Insert(PV);
        PL^.Info := PDStringView(PV);

        R.A.X := Desktop^.ExecView(D);
        D^.GetData(DT);
        Dispose(D, Done);
        if R.A.X <> cmDialPhone then
          exit;
      end;
    if PPH = nil then
      exit;
    {$IFDEF Modem}
    if Dialer = nil then
      begin
        W := New(PAutoDialer, Init(PPH^.Name, PPH^.Number^));
        Application^.InsertWindow(W);
      end
    else
      Dialer^.SetPhone(PPH^.Name, PPH^.Number^);
    {$ENDIF}
    Dispose(PPH, Done);
    PPH := nil;
  end { PhoneBook };

function TPhoneBox.GetKey;
  const
    B: byte = 0;
    ST: String[30] = '';
  begin
    ST := UpStrg(s);
    GetKey := @B;
  end;

{procedure TPhoneBox.SetState(AState: Word; Enable: Boolean);
begin
 Inherited SetState(AState, Enable);
 if (Active <> nil) and ((State And sfFocused)<>0) then
   if (Focused=0) then DisableCommands([cmDialPhone, cmImportPhones])
                  else EnableCommands([cmDialPhone, cmImportPhones])
end;
}
procedure TPhoneBox.SetList;
  var
    PC: PPhoneCollection;
    s: TBufStream;

  procedure DoPhones(Ph: PPhoneDir);

    procedure InsertPhone(P: PPhone);
      var
        PP: PPhoneDir;
        i: CondInt;
      begin
        if P <> nil then
          begin
            FreeStr := UpStrg(P^.Name);
            if (Length(FreeStr) = 2) and (FreeStr = '..') then
              exit;
            PP := New(PPhoneDir, Init('', FreeStr[1], '', ''));
            if PC^.Search(PP, i) then
              begin
                Dispose(PP, Done);
                PP := PC^.At(i);
              end
            else
              PC^.AtInsert(i, PP);
            if PP^.Phones = nil then
              PP^.Phones := New(PPhoneCollection, Init(10, 10));
            PSortedCollection(PP^.Phones)^.Duplicates := True;
            PP^.Phones^.Insert(P);
          end;
      end { InsertPhone };

    begin { DoPhones }
      if (Ph <> nil) and (Ph^.Phones <> nil) and ((Ph^.Password = '') or
          (Ph^.Encrypted))
      then
        Ph^.Phones^.ForEach(@InsertPhone);
      Ph^.Phones^.DeleteAll;
    end { DoPhones };

  begin { TPhoneBox.SetList }
    if Phones = nil then
      exit;
    if (AlphaMode xor Alpha) then
      begin
        if Alpha then
          begin
            PC := New(PPhoneCollection, Init(10, 10));
            Phones^.ForEach(@DoPhones);
          end
        else
          begin
            s.Init(SourceDir+'DN.PHN', stOpenRead, 1024);
            PC := nil;
            PC := PPhoneCollection(s.Get);
            if PC = nil then
              begin
                s.Reset;
                s.Seek(0);
                PC := New(PPhoneCollection, ShortLoad(s));
              end;
            s.Done;
            if PC = nil then
              PC := New(PPhoneCollection, Init(10, 10));
          end;
        Dispose(Phones, Done);
        Phones := PC;
      end;
    AlphaMode := Alpha;
    List := nil;
    NewLisT(Phones);
  end { TPhoneBox.SetList };

procedure TPhoneBox.HandleEvent;
  var
    DT: record
      Name: String[30];
      Number: String[100];
      Memo1, Memo2: String[50];
      end;
    C: PCollection;
    Stream: TBufStream;
    Ph: PPhone;

  function CheckPassword(Ph: PPhoneDir): boolean;
    var
      s: String;
    begin
      CheckPassword := True;
      if (Ph^.Password = '') or Ph^.Encrypted then
        exit;
      repeat
        s := '';
        if ExecResource(dlgSetPassword, s) = cmCancel then
          begin
            CheckPassword := False;
            exit
          end;
      until Ph^.Password = s;
      Ph^.Encrypted := True;
    end;

  procedure EditDirectory(Append: boolean);
    var
      D: PDialog;
      DT: record
        Name: String[30];
        Password: String[15];
        Memo1, Memo2: String[50];
        end;
      R: TRect;
      P: PView;
      Idx: TDlgIdx;
      Ph: PPhoneDir;
      {      PC: PCollection;}
    begin
      FillChar(DT, SizeOf(DT), 0);
      if not Append then
        begin
          Ph := List^.At(Focused);
          if not CheckPassword(Ph) then
            exit;
          DT.Name := Ph^.Name;
          DT.Password := Ph^.Password;
          if Ph^.Memo1 <> nil then
            DT.Memo1 := Ph^.Memo1^;
          if Ph^.Memo2 <> nil then
            DT.Memo2 := Ph^.Memo2^;
          Idx := dlgEditDirectory;
        end
      else
        Idx := dlgAppendDirectory;

      if ExecResource(Idx, DT) <> cmOK then
        exit;

      C := List;
      if C = nil then
        C := New(PPhoneCollection, Init(10, 10));
      List := nil;
      R.A.X := Focused;
      if Append then
        C^.Insert(New(PPhoneDir, Init(DT.Password, DT.Name, DT.Memo1, DT
          .Memo2)))
      else
        begin
          Ph^.Name := DT.Name;
          Ph^.Password := DT.Password;
          DisposeStr(Ph^.Memo1);
          Ph^.Memo1 := NewStr(DT.Memo1);
          DisposeStr(Ph^.Memo2);
          Ph^.Memo2 := NewStr(DT.Memo2);
          C^.AtDelete(Focused);
          PSortedCollection(C)^.Search(Ph, Focused);
          C^.AtInsert(Focused, Ph);
        end;
      Owner^.Lock;
      NewLisT(C);
      FocusItem(R.A.X);
      Owner^.UnLock;
      Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
      Phones := C;
      if Phones^.Count > 16380 then
        Stream.Put(Phones)
      else
        PPhoneCollection(Phones)^.ShortStore(Stream);
      Stream.Done;
    end { EditDirectory };

  procedure EditNumber(Append: boolean);
    var
      D: PDialog;
      R: TRect;
      P: PView;
      Idx: TDlgIdx;
    begin
      if active = nil then
        begin
          EditDirectory(Append);
          exit
        end;
      FillChar(DT, SizeOf(DT), 0);
      if not Append then
        begin
          Ph := List^.At(Focused);
          DT.Name := Ph^.Name;
          if Ph^.Number <> nil then
            DT.Number := Ph^.Number^;
          if Ph^.Memo1 <> nil then
            DT.Memo1 := Ph^.Memo1^;
          if Ph^.Memo2 <> nil then
            DT.Memo2 := Ph^.Memo2^;
          Idx := dlgEditNumber;
        end
      else
        Idx := dlgAppendNumber;

      repeat
        if ExecResource(Idx, DT) <> cmOK then
          exit;
      until (DT.Number <> '');

      if (DT.Number = '') or (DT.Name = '..') {or (DT.Name='')} then
          exit;
      C := List;
      if C = nil then
        C := New(PPhoneCollection, Init(10, 10));
      List := nil;
      R.A.X := Focused;
      if not Append and (C^.Count > Focused) then
        C^.AtFree(Focused);
      C^.Insert(New(PPhone, Init(DT.Number, DT.Name, DT.Memo1, DT.
        Memo2)));
      Owner^.Lock;
      NewLisT(C);
      FocusItem(R.A.X);
      Owner^.UnLock;
      active^.Phones := C;
      Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
      if Phones^.Count > 16380 then
        Stream.Put(Phones)
      else
        PPhoneCollection(Phones)^.ShortStore(Stream);
      Stream.Done;
    end { EditNumber };

  procedure CopyDirectory;
    var
      DT: record
        Name: String[30];
        Password: String[15];
        Memo1, Memo2: String[50];
        end;
      Ph, P1: PPhoneDir;
      i: integer;
    begin
      if (List = nil) or (Focused >= List^.Count) then
        exit;
      FillChar(DT, SizeOf(DT), 0);
      Ph := List^.At(Focused);
      if Ph = nil then
        exit;
      if not CheckPassword(Ph) then
        exit;
      DT.Name := Ph^.Name;
      DT.Password := Ph^.Password;
      if Ph^.Memo1 <> nil then
        DT.Memo1 := Ph^.Memo1^;
      if Ph^.Memo2 <> nil then
        DT.Memo2 := Ph^.Memo2^;

      C := List;
      if C = nil then
        C := New(PPhoneCollection, Init(10, 10));
      List := nil;
      i := Focused;

      P1 := New(PPhoneDir, Init(DT.Password, DT.Name, DT.Memo1, DT.
        Memo2));
      P1^.Encrypted := False;
      C^.Insert(P1);
      Owner^.Lock;
      NewLisT(C);
      FocusItem(i);
      Owner^.UnLock;
      Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
      Phones := C;
      if Phones^.Count > 16380 then
        Stream.Put(Phones)
      else
        PPhoneCollection(Phones)^.ShortStore(Stream);
      Stream.Done;
    end { CopyDirectory };

  procedure CopyPhones;
    var
      i: integer;
    begin
      if (List = nil) or (Focused >= List^.Count) then
        exit;
      if active = nil then
        begin
          CopyDirectory;
          exit
        end;
      FillChar(DT, SizeOf(DT), 0);
      Ph := List^.At(Focused);
      if Ph = nil then
        exit;
      DT.Name := Ph^.Name;
      if Ph^.Number <> nil then
        DT.Number := Ph^.Number^;
      if Ph^.Memo1 <> nil then
        DT.Memo1 := Ph^.Memo1^;
      if Ph^.Memo2 <> nil then
        DT.Memo2 := Ph^.Memo2^;

      C := List;
      if C = nil then
        C := New(PPhoneCollection, Init(10, 10));
      List := nil;
      i := Focused;
      C^.Insert(New(PPhone, Init(DT.Number, DT.Name, DT.Memo1, DT.
        Memo2)));
      Owner^.Lock;
      NewLisT(C);
      FocusItem(i);
      Owner^.UnLock;
      active^.Phones := C;
      Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
      if Phones^.Count > 16380 then
        Stream.Put(Phones)
      else
        PPhoneCollection(Phones)^.ShortStore(Stream);
      Stream.Done;
    end { CopyPhones };

  procedure CE;
    begin
      ClearEvent(Event)
    end;

  procedure DialPhone;
    var
      P: PPhone;
      W: PWindow;
    begin
      CE;
      if Focused = 0 then
        exit;
      if (active = nil) or (List = nil) or (List^.Count = 0) then
          exit;
      P := List^.At(Focused);
      New(PPH, Init(P^.Number^, P^.Name, '', ''));
      Owner^.EndModal(cmDialPhone);
    end;

  procedure EnterDir;
    var
      i: integer;
      s: String;
      PD: PPhoneDir;
      P: PPhone;
    begin
      if active = nil then
        begin
          if List^.Count = 0 then
            exit;

          PD := List^.At(Focused);
          if not CheckPassword(PD) then
            exit;

          DisableCommands([cmDialPhone, cmImportPhones]);
          GroupLabel^.Hide;
          ItemLabel^.Show;
          EnterButton^.Hide;
          ReturnButton^.Show;

          (*     if{ SearchMode and} (List <> nil) then
       begin
        List^.DeleteAll;
        Dispose(List,Done);
        Lisr:=nil;
       end;
  *)
          active := PD;
          List := nil;
          NewLisT(PD^.Phones);
          if (List = nil) then
            begin
              C := New(PPhoneCollection, Init(10, 10));
              C^.Insert(New(PPhone, Init(' ', '..', GetString(
                dlPhonesUpDir), '')));
              Owner^.Lock;
              NewLisT(C);
              Owner^.UnLock;
              active^.Phones := C;
              Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
              if Phones^.Count > 16380 then
                Stream.Put(Phones)
              else
                PPhoneCollection(Phones)^.ShortStore(Stream);
              Stream.Done;
              DrawView;
              exit;
            end;
          PSortedCollection(List)^.Sort;
          if (PPhone(List^.At(0))^.Name <> '..') and (List^.Count > 0)
          then
            begin
              P := New(PPhone, Init(' ', '..', GetString(
                dlPhonesUpDir), ''));
              List^.AtInsert(0, P);
              SetRange(List^.Count);
              DrawView;
            end;
          {HideCursor;}
        end
      else
        begin
          GroupLabel^.Show;
          ItemLabel^.Hide;
          EnterButton^.Show;
          ReturnButton^.Hide;
          if Focused > 0 then
            begin
              DialPhone;
              exit;
            end;

          if SearchMode and (List <> nil) then
            begin
              List^.DeleteAll;
              Dispose(List, Done);
              List := nil;
            end;
          SearchMode := False;
          List := nil;
          NewLisT(Phones);
          FocusItem(Phones^.IndexOf(active));
          active := nil;
          {HideCursor;}
        end;
      Message(@self, evBroadcast, cmValid, nil);
    end { EnterDir };

  procedure SearchPhone;
    var
      PC: PPhoneCollection;
      s: String;

    procedure SearchDir(Ph: PPhoneDir);
      procedure DoPhone(P: PPhone);
        var
          i: integer;
        begin
          if (P <> nil) and ((Pos(s, UpStrg(P^.Name)) <> 0) or (Pos(s,
              UpStrg(P^.Number^)) <> 0))
          then
            PC^.Insert(P);
        end;
      begin
        if (Ph <> nil) and (Ph^.Phones <> nil) and ((Ph^.Password = '')
            or (Ph^.Encrypted))
        then
          Ph^.Phones^.ForEach(@DoPhone);
      end;

    begin { SearchPhone }
      if (Phones = nil) or (Phones^.Count = 0) then
        exit;
      s := '';
      if InputBox(GetString(dlPB_SearchPhone), GetString(
          dlPB_S_earchString), s, 30, 0) <> cmOK
      then
        exit;
      UpStr(s);
      New(PC, Init(10, 10));
      PC^.Duplicates := True;
      Phones^.ForEach(@SearchDir);
      if PC^.Count = 0 then
        begin
          MessageBox(GetString(dlPB_NoFind), nil, mfError+mfOKButton);
          exit;
        end;
      if SearchMode then
        begin
          List^.DeleteAll;
          Dispose(List, Done);
          List := nil
        end;
      SearchMode := True;
      List := nil;
      if active = nil then
        active := Phones^.At(Focused);
      PC^.AtInsert(0, New(PPhone, Init(' ', '..', GetString(
        dlPhonesUpDir), '')));
      NewLisT(PC);
      HideCursor;
      EnterButton^.Hide;
      ReturnButton^.Show;
      GroupLabel^.Hide;
      ItemLabel^.Show;
    end { SearchPhone };

  procedure ImportPhones;
    var
      F: PTextReader;
      s: String;
      P: String;
      Ph: PPhone;
      C: Char;
      i, j, k, M: integer;
      PV: PView;
      fr: boolean;
      Template: String;

    procedure MakePhone;
      var
        i, j, k: integer;
      begin
        for i := 2 to Length(s) do
          if (s[i] >= '0') and (s[i] <= '9') and (s[i-1] = ' ') then
            begin
              P := '';
              k := i;
              j := i;
              while (j <= Length(s)) and (s[j] in ['0'..'9', 'W',
                  'w', '-', ',', '(', ')'])
              do
                begin
                  P := P+s[j];
                  Inc(j)
                end;
              if Length(P) < 3 then
                continue;
              s := Copy(s, 1, k-1);
              DelLeft(s);
              DelRight(s);
              if s <> '' then
                begin
                  New(Ph, Init(P, s, '', ''));
                  if active^.Phones = nil then
                    active^.Phones := New(PPhoneCollection, Init(10, 10)
                      );
                  active^.Phones^.Insert(Ph);
                  Inc(M);
                end;
              break;
            end;
      end { MakePhone };

    procedure MakeTPhone;
      begin
        MakePhone;
      end;

    begin { ImportPhones }
      M := 0;
      if (active = nil) or AlphaMode or SearchMode then
        exit;
      s := GetFileNameDialog(x_x, GetString(dlPB_ImportPhones),
      GetString(dlFileName), fdOKButton+fdHelpButton, hsImportPhones);
      if s = '' then
        exit;
      F := New(PTextReader, Init(s));
      if F = nil then
        begin
          MessageBox(GetString(dlFBBNoOpen)+s, nil, mfError+
            mfOKButton);
          exit
        end;
      PV := WriteMsg(GetString(dlPB_Working));
      fr := True;
      while not F^.Eof do
        begin
          UpdateWriteView(PV);
          s := F^.GetStr;
          if s <> '' then
            begin
              if fr and (s[1] = '|') then
                begin
                  fr := False;
                  Template := s;
                  continue;
                end;
              if Template = '' then
                MakePhone
              else
                MakeTPhone;
            end;
        end;
      if PV <> nil then
        PV^.Free;
      MessageBox(^C+ItoS(M)+GetString(dlPB_CnvReport), nil,
        mfInformation+mfOKButton);
      List := nil;
      NewLisT(active^.Phones);
      Dispose(F, Done);
      Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
      if Phones^.Count > 16380 then
        Stream.Put(Phones)
      else
        PPhoneCollection(Phones)^.ShortStore(Stream);
      Stream.Done;
    end { ImportPhones };

  procedure CopyItem;
    var
      s: String[230];
    begin
      {  FillChar(S[1], 230, 0);
  S[0]:=#230;
  FillChar(S[1], 30, 0);
  Ph := List^.At(Focused);
  Ph^.Name;              30
  Ph^.Number^;           100
  Ph^.Memo1^;            50
  Ph^.Memo2^;            50
  Message(Application, evCommand, cmPutInClipboard, @Str);}
    end;

  var
    WasBroad: boolean;

  begin { TPhoneBox.HandleEvent }
    WasBroad := Event.What = evBroadcast;
    case Event.What of
      evMouseDown:
        if Event.Double then
          begin
            if Event.Buttons and mbRightButton = 0 then
              Message(Owner, evKeyDown, kbEnter, nil)
            else
              Message(Owner, evKeyDown, kbSpace, nil);
            CE;
          end;
      evCommand:
        case Event.KeyCode of
          cmCopyPhone:
            begin
              CE;
              CopyPhones;
              CE;
            end;
          cmImportPhones:
            begin
              CE;
              ImportPhones;
              CE;
            end;
          cmDialPhone:
            begin
              CE;
              DialPhone
            end;
          cmSearchPhone:
            begin
              CE;
              SearchPhone;
              CE;
            end;
          cmOK, cmNo:
            begin
              if (EnterButton^.State and sfVisible) = 0 then
                Focused := 0;
              EnterDir;
              CE
            end;
          cmInsertPhone:
            begin
              if not AlphaMode or SearchMode then
                EditNumber(True);
              CE
            end;
          cmEditPhone:
            begin
              if List^.Count > 0 then
                if not AlphaMode or SearchMode then
                  if not ((active <> nil) and (Focused = 0)) then
                      EditNumber(False);
              CE
            end;
          cmPhoneBookMode:
            begin
              SetList(not AlphaMode);
              CE
            end;
          cmDeletePhone:
            begin
              CE;
              if (List = nil) or (List^.Count = 0) then
                exit;
              if AlphaMode or SearchMode then
                exit;
              if (active <> nil) and (Focused = 0) then
                exit;
              if active = nil then
                begin
                  if Msg(dlPhoneDirDelete, nil, mf2YesButton+
                      mfNoButton+mfConfirmation) <> cmYes
                  then
                    exit;
                  if not CheckPassword(List^.At(Focused)) then
                    exit;
                end
              else if (List^.Count > 0) and
                (MessageBox(GetString(dlPhoneDeleteQuery), nil,
                mfYesButton+mfNoButton+mfConfirmation) <> cmYes)
              then
                exit;
              if (List <> nil) and (Focused < List^.Count) then
                begin
                  List^.AtFree(Focused);
                  SetRange(List^.Count);
                  DrawView;
                end;
              Stream.Init(SourceDir+'DN.PHN', stCreate, 1024);
              if Phones^.Count > 16380 then
                Stream.Put(Phones)
              else
                PPhoneCollection(Phones)^.ShortStore(Stream);
              Stream.Done;
            end;
        end {case};
      evKeyDown:
        if Event.CharCode = ' ' then
          DialPhone
        else if (active <> nil) then
          case Event.KeyCode of
            kbCtrlIns:
              CopyItem;
            kbCtrlPgUp, kbCtrlSlash:
              begin
                Focused := 0;
                EnterDir;
                CE;
                exit;
              end;
            kbEnter:
              begin
                EnterDir;
                CE
              end;
          end {case};
    end {case};
    inherited HandleEvent(Event);
    if WasBroad then
      begin
        if (Info <> nil) and (List <> nil) and (List^.Count > 0)
        then
          begin
            Ph := List^.At(Focused);
            if Ph^.Memo1 <> nil then
              Info^.s1 := Ph^.Memo1^
            else
              Info^.s1 := '';
            if Ph^.Memo2 <> nil then
              Info^.s2 := Ph^.Memo2^
            else
              Info^.s2 := '';
            Info^.DrawView;
          end
        else if Info <> nil then
          begin
            Info^.s1 := '';
            Info^.s2 := '';
            Info^.DrawView;
          end;
      end;
    if active = nil then
      begin
        DisableCommands([cmDialPhone, cmImportPhones]);
        EnableCommands([cmPhoneBookMode, cmCopyPhone]);
        Owner^.Redraw
      end
    else
      begin
        DisableCommands([cmPhoneBookMode]);
        EnableCommands([cmImportPhones]);
        if Focused = 0 then
          DisableCommands([cmDialPhone, cmCopyPhone])
        else
          EnableCommands([cmDialPhone, cmCopyPhone]);
        Owner^.Redraw
      end;

    if SearchMode then
      DisableCommands([cmInsertPhone, cmDeletePhone, cmEditPhone,
        cmImportPhones])
    else
      EnableCommands([cmInsertPhone, cmDeletePhone, cmEditPhone]);

end { TPhoneBox.HandleEvent };

destructor TPhoneBox.Done;
  begin
    { if (List <> nil) and (List <> Phones) }
    {                  then List^.DeleteAll;}
    if Phones <> nil then
      Dispose(Phones, Done);
    Phones := nil;
    { if Active <> nil then Dispose(Active,Done); Active:=nil;}
    if Info <> nil then
      Dispose(Info, Done);
    Info := nil;

    inherited Done;
  end;

function TPhoneBox.GetText;
  var
    s: String;
    P: PPhone;
  begin
    P := List^.At(Item);
    s := AddSpace(P^.Name, 30);
    if TypeOf(P^) = TypeOf(TPhone) then
      s := s+'  '+AddSpace(P^.Number^, 23);
    GetText := s;
  end;

Constructor TPhoneCollection.Init;
  begin
    inherited Init(ALimit, ADelta);
    Duplicates := True;
  end;

Constructor TPhoneCollection.Load;
  begin
    if not inherited Load(s) then
      Fail;
    Duplicates := True;
    Sort;
  end;

const
  VObjType: AWord = otPhone;
  VObjType2: AWord = otPhoneDir;
  VObjType3: AWord = otPhoneCollection;
procedure TPhoneCollection.ShortStore(var s: TStream);
  var
    TCount, TLimit, TDelta: AInt;
  procedure DoPutItem(P: Pointer);
    begin
      if TypeOf((PObject(P)^)) = TypeOf(TPhone) then
        begin
          s.Write(VObjType, 2);
          PPhone(P)^.Store(s);
        end
      else if TypeOf((PObject(P)^)) = TypeOf(TPhoneDir) then
        begin
          s.Write(VObjType2, 2);
          PPhoneDir(P)^.Store(s);
        end;
    end;
  begin
    TCount := Count;
    TLimit := Limit;
    TDelta := Delta;
    s.Write(VObjType3, 2);
    s.Write(Count, 2);
    s.Write(Limit, 2);
    s.Write(Delta, 2);
    ForEach(@DoPutItem);
    s.Write(Duplicates, SizeOf(Duplicates));
  end { TPhoneCollection.ShortStore };

Constructor TPhoneCollection.ShortLoad(var s: TStream);
  var
    Q: AWord;
    C, i: integer;
    ACount, ALimit, ADelta: AInt;
  begin
    s.Read(Q, 2);
    if Q <> otPhoneCollection then
      Fail;
    s.Read(ACount, SizeOf(AInt));
    s.Read(ALimit, SizeOf(AInt));
    s.Read(ADelta, SizeOf(AInt));
    inherited Init(ALimit, ADelta);
    SetLimit(ACount);
    for i := 0 to ACount-1 do
      begin
        s.Read(Q, 2);
        if Q = otPhone then
          AtInsert(i, New(PPhone, Load(s)))
        else if Q = otPhoneDir then
          AtInsert(i, New(PPhoneDir, Load(s)))
        else
          break;
      end;
    s.Read(Duplicates, SizeOf(boolean));
    Sort;
  end { TPhoneCollection.ShortLoad };

function TPhoneCollection.Compare;
  begin
    if PPhone(P1)^.Name = PPhone(P2)^.Name then
      Compare := 0
    else if (PPhone(P1)^.Name = '..') or (PPhone(P1)^.Name < PPhone(P2)^
        .Name)
    then
      Compare := -1
    else
      Compare := 1;
  end;

procedure TPhoneCollection.FreeItem(Item: Pointer);
  var
    PP: PPhone;
  begin
    PP := Item;
    if Item <> nil then
      Dispose(PP, Done);
  end;

Constructor TPhone.Init;
  begin
    inherited Init;
    Name := AName;
    Number := NewStr(ANumber);
    Memo1 := NewStr(AMemo1);
    Memo2 := NewStr(AMemo2);
  end;

Constructor TPhone.Load;
  begin
    Number := s.ReadStr;
    s.ReadStrV(Name);
      {S.Read(Name[0],1); S.Read(Name[1], Byte(Name[0]));}
    Memo1 := s.ReadStr;
    Memo2 := s.ReadStr;
  end;

procedure TPhone.Store;
  begin
    s.WriteStr(Number);
    s.WriteStr(@Name);
    s.WriteStr(Memo1);
    s.WriteStr(Memo2);
  end;

destructor TPhone.Done;
  begin
    DisposeStr(Number);
    DisposeStr(Memo1);
    DisposeStr(Memo2);
    inherited Done;
  end;

Constructor TPhoneDir.Init;
  begin
    inherited Init;
    Name := AName;
    Memo1 := NewStr(AMemo1);
    Memo2 := NewStr(AMemo2);
    Password := APassword;
    Phones := nil;
  end;

procedure CryptCol(Col: PCollection; Pass: String);
  procedure CryptPhone(P: PPhone);
    begin
      if P <> nil then
        with P^ do
          begin
            Name := CryptWith(Pass, Name);
            if Number <> nil then
              Number^:= CryptWith(Pass, Number^);
            if Memo1 <> nil then
              Memo1^:= CryptWith(Pass, Memo1^);
            if Memo2 <> nil then
              Memo2^:= CryptWith(Pass, Memo2^);
          end;
    end;
  begin
    if Col <> nil then
      Col^.ForEach(@CryptPhone);
  end;

Constructor TPhoneDir.Load;
  var
    Q: longInt;
  begin
    s.ReadStrV(Name);
      {S.Read(Name[0],1); S.Read(Name[1], Byte(Name[0]));}
    Memo1 := s.ReadStr;
    Memo2 := s.ReadStr;
    s.Read(Password, 1);
    s.Read(Password[1], byte(Password[0]));
    Password := CryptWith('NaViGaToR', Password);
    Q := s.GetPos;
    Phones := nil;
    Phones := PCollection(s.Get);
    if Phones = nil then
      begin
        s.Status := stOK;
        s.Seek(Q);
        Phones := PCollection(New(PPhoneCollection, ShortLoad(s)));
      end;
    if Phones <> nil then
      CryptCol(Phones, Password);
    Encrypted := False;
  end { TPhoneDir.Load };

procedure TPhoneDir.Store;
  begin
    s.WriteStr(@Name);
    s.WriteStr(Memo1);
    s.WriteStr(Memo2);
    Password := CryptWith('NaViGaToR', Password);
    s.Write(Password, 1);
    s.Write(Password[1], byte(Password[0]));
    Password := CryptWith('NaViGaToR', Password);
    if Phones <> nil then
      CryptCol(Phones, Password);
    if Phones <> nil then
      if Phones^.Count > 16380 then
        s.Put(Phones)
      else
        PPhoneCollection(Phones)^.ShortStore(s);
    if Phones <> nil then
      CryptCol(Phones, Password);
  end;

destructor TPhoneDir.Done;
  begin
    DisposeStr(Memo1);
    DisposeStr(Memo2);
    if Phones <> nil then
      Dispose(Phones, Done);
    Phones := nil;
    inherited Done;
  end;

end.
