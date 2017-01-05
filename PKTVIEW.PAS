{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.6.RC1
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

{(c) NDN Team}
unit PktView;
{$F+,O+,V-}
interface

uses
  Objects, Strings, FViewer, Dialogs, Views, Drivers,
  Messages, Dos, Collect, advance1, advance, Lfn,
  HideView, Scroller
  , use16
  ;

type
  TNetAddr = record
    Net, Node: word;
    end;

  PCharArray = ^TCharArray;
  TCharArray = array[0..65500] of Char;

  PPktObj = ^TPktObj;
  TPktObj = object(TObject)
    FOfs: longInt;
    Fu, Tu, Su, DT: PString;
    FA, TA: TNetAddr;
    Constructor Init(o: longInt; Afu, Atu, Asu, Adt: String;
    FAddr, TAddr: TNetAddr);
    procedure GetMsgInfo(var FromUser, ToUser, Subj, Date: String);
    {!} procedure GetMsgTxt(var Buffer: PCharArray; var MsgLen:
      SmallWord);
    destructor Done; virtual;
    end;

  PPktCol = ^TPktCol;
  TPktCol = object(TCollection)
    FName: PString;
    Constructor Init(PktFile: String);
    destructor Done; virtual;
    end;

  PPktList = ^TPktList;
  TPktList = object(TListBox)
    function GetText(Item: longInt; MaxLen: longInt): String;
      virtual;
    end;

  PPktListDialog = ^TPktListDialog;
  TPktListDialog = object(TDialog)
    lb: PPktList;
    Constructor Init(FName: String; C: PPktCol);
    destructor Done; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    end;

  PLineViewer = ^TLineViewer;
  TLineViewer = object(TScroller)
    FileLines: PLineCollection;
    isValid: boolean;
    Constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
      PScrollBar; Buffer: PCharArray);
    destructor Done; virtual;
    procedure Draw; virtual;
    procedure InitCol(Buffer: PCharArray);
    procedure SetState(AState: longInt; Enable: boolean); virtual;
    function Valid(Command: longInt): boolean; virtual;
    end;

  PMsgViewer = ^TMsgViewer;
  TMsgViewer = object(TLineViewer)
    FromUser, ToUser, Date, Subj: PString;
    Constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
      PScrollBar; FName: String);
    destructor Done; virtual;
    end;

  PPktMsgViewer = ^TPktMsgViewer;
  TPktMsgViewer = object(TDialog)
    FV: PLineViewer;
    PS1, Ps2, Ps3, Ps4: PString;
    Constructor Init(Buf: PCharArray; s1, s2, S3, S4: String; MsgN:
      word; AllMsg: word;
    FA, TA: TNetAddr);
    destructor Done; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure SaveAsText;
    end;

  PMsgViewerDlg = ^TMsgViewerDlg;
  TMsgViewerDlg = object(TDialog)
    Lb1, Lb2, Lb3, Lb4: PLabel;
    CurMsg: PString;
    FV: PMsgViewer;
    Constructor Init(FName: String);
    destructor Done; virtual;
    function GetPalette: PPalette; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure GotoMsg;
    procedure SaveAsText;
    end;

function IsPktFile(FName: String): boolean;
function IsMsgFile(FName: String): boolean;

procedure ViewPktFile(FName: String);
procedure ViewMsgFile(FName: String);
function ViewPktFileE(FName: String; MsgVisible: boolean): boolean;
function ViewMsgFileE(FName: String): boolean;

const
  COrigin {: word} = 11;
  CArea {: word} = 10;
  CCitate {: word} = 14;
  CCluges {: word} = 4 {7};
  KillAfterUse: boolean = False;
  FileName: String = '';

implementation

uses
  DNHelp, advance2, U_KeyMap, xTime, Commands, DNApp

  ;

var
  PktFileName: String;
  PktFromAddr: String;

type
  PacketHeader = record
    origNode: word;
    destNode: word;
    Year: word;
    Month: word;
    Day: word;
    Hour: word;
    Minute: word;
    Second: word;
    baud: word;
    PacketType: word; {generally 0200H}
    origNet: word;
    destNet: word;
    ProductCode: Char;
    fill: array[0..32] of Char;
    end;

  PacketMessageHeader = record
    PacketType: word; {0200H}
    origNode: word;
    destNode: word;
    origNet: word;
    destNet: word;
    Attribute: word;
    cost: word;
    DateTime: array[0..19] of Char;
    {-----------
          toUserName   : array [0..35] of char;
          fromUserName : array [0..35] of char;
          subject      : array [0..71] of char;
-----------}
    end;
type
  MessageHeader = record
    case byte of
      0: (head: array[1..190] of Char);
      1: (fromUserName: array[0..35] of Char;
      toUserName: array[0..35] of Char;
      subject: array[0..71] of Char;
      DateTime: array[1..20] of Char;
      timedRead: word;
      destNode: word;
      origNode: word;
      cost: word;
      origNet: word;
      destNet: word;
      DateWritten: longInt;
      DateArriven: longInt;
      {fill         : array [1..8] of char;}
      replyTo: word;
      Attribute: word;
      nextReply: word);
  end;

function spaces(Num: integer): String;
  var
    sp1: integer;
    space: String;
  begin
    space := '';
    for sp1 := 1 to Num do
      space := space+' ';
    spaces := space;
  end;

function rpad(Ch: String; Num: integer): String;
  begin
    rpad := Copy(Ch+spaces(Num), 1, Num);
  end;

function DLS(s: String): String;
  begin
    if (s[Length(s)] = '\') then
      SetLength(s, Length(s)-1);
    DLS := s;
  end;

function lpad(Ch: String; Num: integer): String;
  var
    sp1: integer;
    sp2: integer;
  begin
    sp1 := Length(Ch);
    sp2 := Num-sp1;
    lpad := spaces(sp2)+Ch;
  end;

function just(s: String; len, LRC: longInt): String;
  begin
    if len > 255 then
      len := 255;
    if Length(s) > len then
      SetLength(s, Lo(len));
    case LRC of
      1:
        just := lpad(s, len); {'       '+s}
      2:
        just := rpad(s, len); {s+'       '}
      3:
        begin{'  '+s+'  '}
          while Length(s) < len do
            s := ' '+s+' ';
          if Length(s) > len then
            SetLength(s, Length(s)-1);
          just := s;
        end
        else
        just := s; {11-01-99,BDS}
    end {case};
  end { just };

function fns_z(n: integer): String;
  var
    C: String;
  begin
    C := ItoS(n);
    if Length(C) = 1
    then
      C := '0'+C;
    fns_z := C;
  end;

procedure Msg2Strs(FName: String; var FromUser, ToUser, Subj, Date:
    String;
  var C: PLineCollection);
  var
    Message: TBufStream; {file of char;}
    Header: MessageHeader;
    i, j: integer;
    Buf: Char;
    txtbuf: String;
  begin
    Message.Init(FName, stOpenRead, 4096);
    if Message.Status <> stOK then
      exit;
    for i := 1 to 190 do
      Message.Read(Header.head[i], 1);
    FromUser := '';
    for i := 0 to 35 do
      begin
        if Header.fromUserName[i] <> #0 then
          FromUser := FromUser+Header.fromUserName[i]
        else
          break;
      end;
    FromUser := FromUser+',( 2:'+ItoS(Header.origNet)+'/'+ItoS(
      Header.origNode)+' )';
    ToUser := '';
    for i := 0 to 35 do
      begin
        if Header.toUserName[i] <> #0 then
          ToUser := ToUser+Header.toUserName[i]
        else
          break;
      end;
    ToUser := ToUser+',( 2:'+ItoS(Header.destNet)+'/'+ItoS(Header.
      destNode)+' )';
    Subj := '';
    for i := 0 to 71 do
      begin
        if Header.subject[i] <> #0 then
          Subj := Subj+Header.subject[i]
        else
          break;
      end;
    Date := '';
    for i := 1 to 20 do
      begin
        if Header.DateTime[i] <> #0 then
          Date := Date+Header.DateTime[i]
        else
          break;
      end;
    i := 1;
    txtbuf := '';
    while Message.Status = stOK do
      begin
        Message.Read(Buf, 1);
        if (Buf = #13) or (Length(txtbuf) >= 76) then
          begin
            if Buf <> #13 then
              txtbuf := txtbuf+Buf;
            C^.Insert(NewStr(txtbuf));
            txtbuf := '';
          end
        else
          txtbuf := txtbuf+Buf;
      end;
    Message.Done;
  end { Msg2Strs };

function ReadPktHeader(FName: String; var PType: word; var DT,
    FromUser, ToUser: String): boolean;
  label l1;
  var
    Ph: PacketHeader;
    {F:File;}
    F: TDOSStream;
    {Rb:Word;}
    Res: boolean;
  begin
    F.Init(FName, stOpenRead);
    if F.Status <> stOK then
      goto l1;
    F.Read(Ph, SizeOf(PacketHeader));
    if F.Status > stOK then
      begin
l1:
        PType := 0;
        DT := 'Unknown date and time';
        FromUser := 'Unknown';
        ToUser := FromUser;
        Res := False;
      end
    else
      begin
        with Ph do
          begin
            PType := PacketType;
            DT := 'Date: '+fns_z(Day)+'-'+fns_z(Month)+'-'+ItoS(Year)+
            '  Time: '+fns_z(Hour)+':'+fns_z(Minute)+':'+fns_z(
              Second);
            FromUser := 'From: 2:'+ItoS(origNet)+'/'+ItoS(origNode);
            ToUser := 'To  : 2:'+ItoS(destNet)+'/'+ItoS(destNode);
          end;
        Res := True;
      end;
    F.Done;
    ReadPktHeader := Res;
  end { ReadPktHeader };

Constructor TPktObj.Init(o: longInt; Afu, Atu, Asu, Adt: String;
    FAddr, TAddr: TNetAddr);
  begin
    inherited Init;
    FOfs := o;
    Fu := NewStr(Afu);
    Su := NewStr(Asu);
    Tu := NewStr(Atu);
    DT := NewStr(Adt);
    FA := FAddr;
    TA := TAddr;
  end;

Constructor TPktCol.Init(PktFile: String);
  label l1;
  var
    Ph: PacketHeader;
    PMH: PacketMessageHeader;
    F: TBufStream;
    Rb: word;
    i: word;
    Ch: Char;
    LPos, LSize: longInt;
    ST: String;
    EmptyPkt: boolean;
    Date, FromUser, ToUser, Subj: String;
    AFrm, ATo: TNetAddr;
  begin
    inherited Init(0, 1);
    FName := NewStr(PktFile);
    F.Init(PktFile, stOpenRead, 4096);
    if F.Status <> stOK then
      goto l1;
    F.Read(Ph, SizeOf(PacketHeader));
    EmptyPkt := (F.GetPos = F.GetSize);
    if EmptyPkt then
      begin
        F.Done;
l1:
        exit;
      end;
    LSize := F.GetSize;
    repeat
      LPos := F.GetPos;
      F.Read(PMH, SizeOf(PacketMessageHeader));
      if F.Status <> stOK then
        break;
      Date := StrPas(PMH.DateTime);
      with PMH do
        begin
          AFrm.Net := origNet;
          AFrm.Node := origNode;
          ATo.Net := destNet;
          ATo.Node := destNode;
        end;
      ST := '';
      repeat
        F.Read(Ch, 1);
        if Ch <> #0 then
          ST := ST+Ch;
      until Ch = #0;
      ToUser := ST;
      ST := '';
      repeat{fromuser}
        F.Read(Ch, 1);
        if Ch <> #0 then
          ST := ST+Ch;
      until Ch = #0;
      FromUser := ST;
      ST := '';
      repeat
        F.Read(Ch, 1);
        if Ch <> #0 then
          ST := ST+Ch;
      until Ch = #0;
      if ST = '' then
        ST := ' ';
      Subj := ST;
      Insert(New(PPktObj, Init(LPos, FromUser, ToUser, Subj, Date,
        AFrm, ATo)));
      repeat
        F.Read(Ch, 1);
      until Ch = #0;
    until LPos = LSize;
    F.Done;
  end { TPktCol.Init };

procedure TPktObj.GetMsgInfo(var FromUser, ToUser, Subj, Date:
    String);
  begin
    FromUser := Fu^;
    ToUser := Tu^;
    Subj := Su^;
    Date := DT^;
  end;

destructor TPktObj.Done;
  begin
    DisposeStr(Tu);
    DisposeStr(Fu);
    DisposeStr(Su);
    DisposeStr(DT);
    if KillAfterUse then
      begin
        EraseTempFile(FileName);
        KillAfterUse := False;
        TempFile := '';
      end;
    inherited Done;
  end;

procedure TPktObj.GetMsgTxt(var Buffer: PCharArray; var MsgLen: word);
  var
    F: TBufStream;
    LPos: longInt;
    PktMsg: PPktObj;
    ST: String;
    i: word;
    Ph: PacketHeader;
    PMH: PacketMessageHeader;
    Rb: word;
    Ch: Char;
  begin
    MsgLen := 0;
    i := 0;
    F.Init(PktFileName, stOpenRead, 4096);
    LPos := FOfs;
    F.Seek(LPos);
    F.Read(PMH, SizeOf(PacketMessageHeader));
    if F.Status <> stOK then
      begin
        F.Done;
        exit;
      end;
    for i := 1 to 3 do
      begin
        repeat
          F.Read(Ch, 1);
        until Ch = #0;
      end;
    i := 0;
    repeat
      F.Read(Ch, 1);
      Buffer^[i] := Ch;
      Inc(i);
    until Ch = #0;
    MsgLen := i;
    F.Read(Ch, 1);
    F.Done;
  end { TPktObj.GetMsgTxt };

destructor TPktCol.Done;
  begin
    DisposeStr(FName);
    inherited Done;
  end;

var
  PktCol: PPktCol;

procedure InitPktCol(FName: String);
  begin
    PktCol := New(PPktCol, Init(FName));
  end;

procedure DonePktCOl;
  begin
    Dispose(PktCol, Done);
  end;

function DelChar(s: String; Ch: Char): String;
  begin
    while Pos(Ch, s) <> 0 do
      Delete(s, Pos(Ch, s), 1);
    DelChar := s;
  end;

procedure CutFromAddr(ST: String);
  begin
    while ST[1] <> ' ' do
      Delete(ST, 1, 1);
    DelRight(ST);
    DelLeft(ST);
    while ST[1] <> ' ' do
      begin
        PktFromAddr := PktFromAddr+ST[1];
        Delete(ST, 1, 1);
      end;
  end;

procedure Buffer2Strs(Buffer: PCharArray; var C: PLineCollection);
  var
    i: word;
    ST: String;
    Ch: Char;
  begin
    ST := '';
    PktFromAddr := '';
    i := 0;
    repeat
      Ch := Buffer^[i];
      if Ch in [#13, #10] then
        begin
          ST := DelChar(DelChar(ST, #13), #10);
          if (ST[1] = #1) and (Pos('MSGID', ST) <> 0) then
              CutFromAddr(ST);
          C^.Insert(NewStr(ST));
          ST := '';
        end;
      if Ch <> #0 then
        begin
          if Length(ST) > 76 then
            begin
              ST := ST+Ch;
              if not (Buffer^[i+1] in [' ', #13, #10, #0]) then
                begin
                  if Buffer^[i+2] in [' ', #13, #10, #0] then
                    ST := ST+Buffer^[i+1];
                  Inc(i);
                end;
              ST := DelChar(DelChar(ST, #13), #10);
              C^.Insert(NewStr(ST));
              ST := '';
            end
          else
            ST := ST+Ch;
        end
      else
        begin
          ST := DelChar(DelChar(ST, #13), #10);
          if (ST[1] = #1) and (Pos('MSGID', ST) <> 0) then
              CutFromAddr(ST);
          C^.Insert(NewStr(ST));
          ST := '';
        end;
      Inc(i);
    until Ch = #0;
  end { Buffer2Strs };

function TPktList.GetText(Item: longInt; MaxLen: longInt): String;
  var
    FromUser, ToUser, Subj, Date: String;
    PC: PPktObj;
    s1, s2, S3: String;
  begin
    PC := List^.At(Item);
    PC^.GetMsgInfo(FromUser, ToUser, Subj, Date);
    s1 := just(FromUser, 26, 2);
    s2 := just(ToUser, 26, 2);
    S3 := just(Subj, 26, 2);
    GetText := s1+s2+S3;
  end;

destructor TPktListDialog.Done;
  begin
    Dispose(lb, Done);
    inherited Done;
  end;

const
  CPktDialog = #112#113#114#115#116#117#117#119#120#121#122#123+
  #124#125#46#47#48#49#50#51#52#53#54#55#1#117#118+
  #1#1#61#62#63#178#179;

function TPktListDialog.GetPalette: PPalette;
  const
    Pal: String = CPktDialog;
  begin
    GetPalette := @Pal;
  end;

Constructor TPktListDialog.Init(FName: String; C: PPktCol);
  var
    R: TRect;
    View: PView;
  begin
    HelpCtx := hcPktListDialog;
    Desktop^.GetExtent(R);
    inherited Init(R, GetString(dlPktView)+FName);
    Options := Options or {ofCentered}ofSelectable;
    GrowMode := gfGrowHiX+gfGrowHiY;
    View := StandardScrollBar(sbVertical+sbHandleKeyboard);
    EventMask := $FFFF;
    Inc(View^.Origin.Y);
    Dec(View^.Size.Y);
    Insert(View);
    GetExtent(R);
    R.Grow(-1, -1);
    Inc(R.A.Y);
    lb := New(PPktList, Init(R, 1, PScrollBar(View)));
    lb^.Options := lb^.Options+ofFramed;
    Insert(lb);
    lb^.NewLisT(C);
    lb^.SetRange(C^.Count);
    R.Assign(2, 1, 8, 2);
    View := New(PLabel, Init(R, '~From~', nil));
    Insert(View);
    R.Assign(31, 1, 35, 2);
    View := New(PLabel, Init(R, '~To~', nil));
    Insert(View);
    R.Assign(52, 1, 58, 2);
    View := New(PLabel, Init(R, '~Subj~', nil));
    Insert(View);
    SelectNext(False);
  end { TPktListDialog.Init };

function PktHeaderDlg(AText: String): PDialog;
  var
    R: TRect;
    Dialog: PDialog;
    View: PView;
  begin
    R.Assign(13, 6, 66, 17);
    Dialog := New(PDialog, Init(R, GetString(dlPktHeader)));
    with Dialog^ do
      begin
        Options := Options+ofCenterX+ofCenterY;
        R.Assign(20, 8, 32, 10);
        View := New(PButton, Init(R, 'Ok', cmOK, bfDefault));
        View^.Options := View^.Options+ofCenterX;
        Insert(View);
        R.Assign(2, 1, 51, 8);
        View := New(PStaticText, Init(R, AText));
        Insert(View);
        SelectNext(False);
      end;
    PktHeaderDlg := Dialog
  end { PktHeaderDlg };

procedure ViewPktHeader(MsgCount: integer);
  var
    W: word;
    s1, s2, S3: String;
    s: String;
    D: PDialog;
  begin
    ReadPktHeader(PktFileName, W, s1, s2, S3);
    s := GetString(dlPktFile)+FExpand(PktFileName)+#13+s1+#13+s2+#13+S3+
      #13+
    {!!!'Packet Size: '+PrintUsing('###,###',GetFileSize(PktFileName))+' byte(s)'+#13+}
    GetString(dlPktMsg)+ItoS(MsgCount);
    D := PktHeaderDlg(s);
    Desktop^.ExecView(D);
    Dispose(D, Done);
  end;

procedure TPktListDialog.HandleEvent(var Event: TEvent);
  var
    PC: PPktObj;
    Buf: PCharArray;
    D: PPktMsgViewer;
    M: word;
    s1, s2, S3, S4: String;
  begin
    if (Event.What = evKeyDown) then
      case Event.KeyCode of
        kbEnter, kbF3:
          begin
            PC := lb^.List^.At(lb^.Focused);
            if PC = nil then
              exit;
            New(Buf);
            PC^.GetMsgTxt(Buf, M);
            s1 := PC^.Fu^;
            s2 := PC^.Tu^;
            S3 := PC^.Su^;
            S4 := PC^.DT^;
            D := New(PPktMsgViewer, Init(Buf, s1, s2, S3, S4, lb^.
              Focused+1, lb^.List^.Count,
            PC^.FA, PC^.TA));
            Dispose(Buf);
            Desktop^.ExecView(D);
            Dispose(D, Done);
          end;
        kbF2:
          ViewPktHeader(lb^.List^.Count);
      end {case};
    inherited HandleEvent(Event);
  end { TPktListDialog.HandleEvent };

Constructor TLineViewer.Init(var Bounds: TRect; AHScrollBar,
  AVScrollBar: PScrollBar; Buffer: PCharArray);
  begin
    HelpCtx := hcLineViewer;
    Options := Options or {ofCentered}ofSelectable;
    GrowMode := gfGrowHiX+gfGrowHiY;
    EventMask := $FFFF;
    TScroller.Init(Bounds, AHScrollBar, AVScrollBar);
    TLineViewer.InitCol(Buffer);
  end;

destructor TLineViewer.Done;
  begin
    Dispose(FileLines, Done);
    TScroller.Done;
  end;

procedure TLineViewer.Draw;
  var
    B: TDrawBuffer;
    CNormal: byte;
    i: integer;
    s: String;
    P: PString;
    Color: byte;
    Bkg: byte;
    OrigColor, AreaColor, Citate, WorkStrColor: word;
  begin
    CNormal := GetColor(1);
    Bkg := Lo((CNormal and $F0) shr 4);
    OrigColor := Bkg*16+COrigin;
    AreaColor := Bkg*16+CArea;
    Citate := Bkg*16+CCitate;
    WorkStrColor := Bkg*16+CCluges;

    for i := 0 to Size.Y-1 do
      begin
        MoveChar(B, ' ', CNormal, Size.X);
        if Delta.Y+i < FileLines^.Count then
          begin
            P := FileLines^.At(Delta.Y+i);
            if P <> nil then
              s := Copy(P^, Delta.X+1, Size.X)
            else
              s := '';
            Color := CNormal;
            if Pos('AREA', s) <> 0 then
              Color := AreaColor;
            if Pos('>', s) <> 0 then
              Color := Citate;
            if Pos('Origin', s) <> 0 then
              Color := OrigColor;
            if (Pos('SEEN-BY', s) <> 0) then
              Color := WorkStrColor;
            if (s[1] = #1) then
              begin
                System.Delete(s, 1, 1);
                Color := WorkStrColor;
              end;
            MoveStr(B, s, Color);
          end;
        WriteLine(0, i, Size.X, 1, B);
      end;
  end { TLineViewer.Draw };

procedure TLineViewer.InitCol(Buffer: PCharArray);
  begin
    isValid := True;
    FileLines := New(PLineCollection, Init(5, 5, False));
    Buffer2Strs(Buffer, FileLines);
    Limit.X := 255;
    Limit.Y := FileLines^.Count;
  end;

procedure TLineViewer.SetState(AState: longInt; Enable: boolean);
  begin
    TScroller.SetState(AState, Enable);
    if Enable and (AState and sfExposed <> 0) then
      SetLimit(Limit.X, Limit.Y);
  end;

function TLineViewer.Valid(Command: longInt): boolean;
  begin
    Valid := isValid;
  end;

Constructor TMsgViewer.Init(var Bounds: TRect; AHScrollBar,
  AVScrollBar: PScrollBar; FName: String);
  var
    s1, s2, S3, S4: String;
  begin
    HelpCtx := hcMsgViewer;
    TScroller.Init(Bounds, AHScrollBar, AVScrollBar);
    Options := Options or {ofCentered}ofSelectable;
    GrowMode := gfGrowHiX+gfGrowHiY;
    EventMask := $FFFF;
    FileLines := New(PLineCollection, Init(5, 5, False));
    Msg2Strs(FName, s1, s2, S3, S4, FileLines);
    FromUser := NewStr(s1);
    ToUser := NewStr(s2);
    Subj := NewStr(S3);
    Date := NewStr(S4);
    Limit.X := 255;
    Limit.Y := FileLines^.Count;
  end;

destructor TMsgViewer.Done;
  begin
    DisposeStr(FromUser);
    DisposeStr(ToUser);
    DisposeStr(Date);
    DisposeStr(Subj);
    if KillAfterUse then
      begin
        EraseTempFile(FileName);
        KillAfterUse := False;
        TempFile := '';
      end;
    inherited Done;
  end;

Constructor TPktMsgViewer.Init(Buf: PCharArray; s1, s2, S3, S4:
    String; MsgN: word; AllMsg: word;
  FA, TA: TNetAddr);
  var
    R: TRect;
    View: PView;
    VS: PScrollBar;
    s: String;
  begin
    HelpCtx := hcPktMsgViewer;
    Desktop^.GetExtent(R);
    inherited Init(R, GetString(dlViewMsg));
    Options := Options or ofCentered;
    VS := StandardScrollBar(sbVertical+sbHandleKeyboard);
    Inc(VS^.Origin.Y, 5);
    Dec(VS^.Size.Y, 5);
    Insert(VS);
    GetExtent(R);
    R.Grow(-1, -1);
    Inc(R.A.Y, 5);
    EventMask := $FFFF;
    FV := New(PLineViewer, Init(R, nil, VS, Buf));
    FV^.Options := FV^.Options or ofFramed;
    Insert(FV);
    R.Assign(1, 1, 79, 2);
    s := '~Msg~  :'+just(ItoS(MsgN), 5, 1)+'/'+just(ItoS(AllMsg), 5, 2)+
      spaces(30)+'~Date~ :'+S4;
    View := New(PLabel, Init(R, s, nil));
    Insert(View);
    PS1 := NewStr(DelChar(s, '~'));
    R.Assign(1, 2, 79, 3);
    s := '~From~ :'+just(s1, 50, 2)+PktFromAddr;
    View := New(PLabel, Init(R, s, nil));
    Insert(View);
    Ps2 := NewStr(DelChar(s, '~'));
    R.Assign(1, 3, 79, 4);
    s := '~To~   :'+just(s2, 50, 2)+'2:'+ItoS(TA.Net)+'/'+ItoS(TA.
      Node);
    View := New(PLabel, Init(R, s, nil));
    Insert(View);
    Ps3 := NewStr(DelChar(s, '~'));
    R.Assign(1, 4, 79, 5);
    s := '~Subj~ :'+S3;
    View := New(PLabel, Init(R, s, nil));
    Insert(View);
    Ps4 := NewStr(DelChar(s, '~'));
    SelectNext(False);
  end { TPktMsgViewer.Init };

function TPktMsgViewer.GetPalette: PPalette;
  const
    Pal: String = CPktDialog;
  begin
    GetPalette := @Pal;
  end;

destructor TPktMsgViewer.Done;
  begin
    DisposeStr(PS1);
    DisposeStr(Ps2);
    DisposeStr(Ps3);
    DisposeStr(Ps4);
    Dispose(FV, Done);
    inherited Done;
  end;

procedure TPktMsgViewer.SaveAsText;
  var
    TName: String;
    F: text;
    Area: String;
    {--------------------------------------------}
  procedure WriteFile(PS: PString);
    far;
    begin
      if PS = nil then
        Writeln(F, ' ')
      else
        Writeln(F, DelChar(PS^, #1));
    end;
  {--------------------------------------------}
  procedure FindArea(PS: PString);
    far;
    var
      s: String;
    begin
      if Area <> '' then
        exit;
      s := PS^;
      if Pos('AREA', s) <> 0 then
        begin
          while Pos('AREA', s) <> 1 do
            System.Delete(s, 1, 1);
          System.Delete(s, Pos('AREA', s), 4);
          DelLeft(s);
          DelRight(s);
          while (s[1] <> ' ') and (s <> '') do
            begin
              Area := Area+s[1];
              System.Delete(s, 1, 1);
            end;
        end;
    end { FindArea };
  {--------------------------------------------}
  begin { TPktMsgViewer.SaveAsText }
    TName := '';
    if InputBox(GetString(dlSaveMsg), GetString(dlMsgFile), TName,
        255, 0) =
      cmCancel
    then
      exit;
    Assign(F, TName);
    DoneSysError;
    if ExistFile(TName) then{$I-}
      Append(F) {$I+}
    else{$I-}
      rewrite(F); {$I+}
    if IOResult <> 0 then
      begin
        MessageBox(GetString(dlErrorWriting)+TName+'!', nil,
        mfError+mfOKButton);
        exit;
      end;
    InitSysError;
    Area := '';
    FV^.FileLines^.ForEach(@FindArea);
    Writeln(F, GetString(dlLine));
    Writeln(F, 'AREA: '+Area);
    Writeln(F, PS1^);
    Writeln(F, Ps2^);
    Writeln(F, Ps3^);
    Writeln(F, Ps4^);
    Writeln(F, GetString(dlLine));
    FV^.FileLines^.ForEach(@WriteFile);
    System.Close(F);
  end { TPktMsgViewer.SaveAsText };

procedure TPktMsgViewer.HandleEvent(var Event: TEvent);
  begin
    if (Event.What = evKeyDown) then
      case Event.KeyCode of
        kbF2:
          SaveAsText;
      end {case};
    inherited HandleEvent(Event);
  end;

Constructor TMsgViewerDlg.Init(FName: String);
  var
    R: TRect;
    View: PView;
    VS: PScrollBar;
  begin
    Desktop^.GetExtent(R);
    inherited Init(R, GetString(dlNetMailView)+FExpand(FName));
    Options := Options or ofCentered;
    GrowMode := gfGrowHiX+gfGrowHiY;
    EventMask := $FFFF;
    VS := StandardScrollBar(sbVertical+sbHandleKeyboard);
    Inc(VS^.Origin.Y, 5);
    Dec(VS^.Size.Y, 5);
    Insert(VS);
    GetExtent(R);
    R.Grow(-1, -1);
    Inc(R.A.Y, 5);
    FV := New(PMsgViewer, Init(R, nil, VS, FName));
    FV^.Options := FV^.Options or ofFramed;
    Insert(FV);
    R.Assign(1, 1, 79, 2);
    Lb1 := New(PLabel, Init(R, '~Msg~  :Private '+spaces(30)+
      '~Date~ :'+FV^.Date^, nil));
    Insert(Lb1);
    R.Assign(1, 2, 79, 3);
    Lb2 := New(PLabel, Init(R, '~From~ :'+FV^.FromUser^, nil));
    Insert(Lb2);
    R.Assign(1, 3, 79, 4);
    Lb3 := New(PLabel, Init(R, '~To  ~ :'+FV^.ToUser^, nil));
    Insert(Lb3);
    R.Assign(1, 4, 79, 5);
    Lb4 := New(PLabel, Init(R, '~Subj~ :'+FV^.Subj^, nil));
    Insert(Lb4);
    SelectNext(False);
    CurMsg := NewStr(FName);
  end { TMsgViewerDlg.Init };

function TMsgViewerDlg.GetPalette: PPalette;
  const
    Pal: String = CPktDialog;
  begin
    GetPalette := @Pal;
  end;

function GetPrevMsg(FName: String): String;
  var
    D, n, E: String;
    Er: longInt;
    Num: word;
    TM: TEventTimer;
    SR: lSearchRec;
    VM: PView;
  begin
    lFSplit(FName, D, n, E);
    while (n[1] = '0') and (n <> '') do
      System.Delete(n, 1, 1);
    if n = '' then
      n := '0';
    Val(n, Num, Er);
    NewTimer(TM, 4);
    VM := nil;
    if (Er <> 0) or (Num = 0) then
      GetPrevMsg := ''
    else
      begin
        while Num > 0 do
          begin
            if (VM = nil) and TimerExpired(TM) then
              VM := WriteMsg(GetString(dlPleaseStandBy));
            Dec(Num);
            lFindFirst(MakeNormName(D, ItoS(Num)+'.msg'), AnyFile-
              Directory, SR);
            if DOSError = 0 then
              begin
                GetPrevMsg := MakeNormName(D, SR.FullName);
                if VM <> nil then
                  Dispose(VM, Done);
                lFindClose(SR);
                exit;
              end;
            lFindClose(SR);
          end;
        if (Num = 0) then
          GetPrevMsg := '';
        if VM <> nil then
          Dispose(VM, Done);
      end;
  end { GetPrevMsg };

function GetNextMsg(FName: String): String;
  var
    D, n, E: String;
    Er: longInt;
    Num, ST: word;
    TM: TEventTimer;
    SR: lSearchRec;
    VM: PView;
  begin
    lFSplit(FName, D, n, E);
    while (n[1] = '0') and (n <> '') do
      System.Delete(n, 1, 1);
    if n = '' then
      n := '0';
    Val(n, Num, Er);
    ST := Num;
    NewTimer(TM, 4);
    VM := nil;
    if (Er <> 0) then
      GetNextMsg := ''
    else
      begin
        while Num < ST+1000 do
          begin
            if (VM = nil) and TimerExpired(TM) then
              VM := WriteMsg(GetString(dlPleaseStandBy));
            Inc(Num);
            lFindFirst(MakeNormName(D, ItoS(Num)+'.msg'), AnyFile-
              Directory, SR);
            if DOSError = 0 then
              begin
                GetNextMsg := MakeNormName(D, SR.FullName);
                if VM <> nil then
                  Dispose(VM, Done);
                lFindClose(SR);
                exit;
              end
            else
              begin
                GetNextMsg := '';
                lFindClose(SR);
                continue;
              end;
          end;
        if VM <> nil then
          Dispose(VM, Done);
      end;
  end { GetNextMsg };

procedure TMsgViewerDlg.GotoMsg;
  var
    VS: PScrollBar;
    R: TRect;
    View: PView;
  begin
    DisposeStr(Title);
    Title := NewStr(GetString(dlNetMailView)+CurMsg^);
    Dispose(FV, Done);
    VS := StandardScrollBar(sbVertical+sbHandleKeyboard);
    EventMask := $FFFF;
    Inc(VS^.Origin.Y, 5);
    Dec(VS^.Size.Y, 5);
    Insert(VS);
    GetExtent(R);
    R.Grow(-1, -1);
    Inc(R.A.Y, 5);
    FV := New(PMsgViewer, Init(R, nil, VS, CurMsg^));
    FV^.Options := FV^.Options or ofFramed;
    Insert(FV);
    Dispose(Lb1, Done);
    Lb1 := nil;
    Dispose(Lb2, Done);
    Lb2 := nil;
    Dispose(Lb3, Done);
    Lb3 := nil;
    Dispose(Lb4, Done);
    Lb4 := nil;
    R.Assign(1, 1, 79, 2);
    if FV <> nil then
      Lb1 := New(PLabel, Init(R, '~Msg  ~:Private '+spaces(30)+
        '~Date~ :'+FV^.Date^, nil));
    Insert(Lb1);
    R.Assign(1, 2, 79, 3);
    Lb2 := New(PLabel, Init(R, '~From ~:'+FV^.FromUser^, nil));
    Insert(Lb2);
    R.Assign(1, 3, 79, 4);
    Lb3 := New(PLabel, Init(R, '~To   ~:'+FV^.ToUser^, nil));
    Insert(Lb3);
    R.Assign(1, 4, 79, 5);
    Lb4 := New(PLabel, Init(R, '~Subj ~:'+FV^.Subj^, nil));
    Insert(Lb4);
    SelectNext(False);
    Redraw;
  end { TMsgViewerDlg.GotoMsg };

procedure TMsgViewerDlg.SaveAsText;
  var
    TName: String;
    F: text;
  procedure WriteFile(PS: PString);
    far;
    begin
      if PS = nil then
        Writeln(F, ' ')
      else
        Writeln(F, DelChar(PS^, #1));
    end;
  begin
    TName := '';
    if InputBox(GetString(dlSaveMsg), GetString(dlMsgFile), TName,
        255, 0) =
      cmCancel
    then
      exit;
    Assign(F, TName);
    DoneSysError;
    if ExistFile(TName) then{$I-}
      Append(F) {$I+}
    else{$I-}
      rewrite(F); {$I+}
    if IOResult <> 0 then
      begin
        MessageBox(GetString(dlErrorWriting)+TName+'!', nil,
        mfError+mfOKButton);
        exit;
      end;
    InitSysError;
    Writeln(F, #196#196#196#196#196#196#196#196#196'['+CenterStr(
      GetName(CurMsg^), 17)
    +']'#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196
    +
      #196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196
    +
      #196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196);
    Writeln(F, 'Msg  : Private '+spaces(30)+'Date : '+FV^.Date^);
    Writeln(F, 'From : '+FV^.FromUser^);
    Writeln(F, 'To   : '+FV^.ToUser^);
    Writeln(F, 'Subj : '+FV^.Subj^);
    Writeln(F, GetString(dlLine));
    FV^.FileLines^.ForEach(@WriteFile);
    System.Close(F);
  end { TMsgViewerDlg.SaveAsText };

procedure TMsgViewerDlg.HandleEvent(var Event: TEvent);
  var
    s: String;
    SOld: String;
    Er: byte;
  begin
    if Event.What = evKeyDown then
      case Event.KeyCode of
        kbGrayPlus, kbRight:
          begin
            s := GetNextMsg(CurMsg^);
            if s <> '' then
              begin
                DisposeStr(CurMsg);
                CurMsg := NewStr(s);
                GotoMsg;
              end
            else
              MessageBox(GetString(dlLastMsg), nil,
              mfError+mfOKButton);
          end;
        kbGrayMinus, kbLeft:
          begin
            s := GetPrevMsg(CurMsg^);
            if s <> '' then
              begin
                DisposeStr(CurMsg);
                CurMsg := NewStr(s);
                GotoMsg;
              end
            else
              MessageBox(GetString(dlFirstMsg), nil,
              mfError+mfOKButton);
          end;
        kbF8, kbDel:
          begin
            if MessageBox(GetString(dlDelMsg)+CurMsg^+'?', nil,
              mfConfirmation+mfOKButton+mfCancelButton) = cmOK
            then
              begin
                SOld := CurMsg^;
                s := GetPrevMsg(CurMsg^);
                if s <> '' then
                  begin
                    DisposeStr(CurMsg);
                    CurMsg := NewStr(s);
                    GotoMsg;
                    EraseFile(SOld);
                  end
                else
                  begin
                    s := GetNextMsg(CurMsg^);
                    if s <> '' then
                      begin
                        DisposeStr(CurMsg);
                        CurMsg := NewStr(s);
                        GotoMsg;
                        EraseFile(SOld);
                      end
                    else
                      begin
                        EraseFile(SOld);
                        EndModal(cmCancel);
                      end;
                  end;
              end;
          end;
        kbF2:
          SaveAsText;
      end {case}; {case}
    inherited HandleEvent(Event);
  end { TMsgViewerDlg.HandleEvent };

destructor TMsgViewerDlg.Done;
  begin
    Dispose(FV, Done);
    Dispose(Lb1, Done);
    Dispose(Lb2, Done);
    Dispose(Lb3, Done);
    Dispose(Lb4, Done);
    DisposeStr(CurMsg);
    inherited Done;
  end;

function TestDiap(Value, Min, Max: word): boolean;
  begin
    if (Value < Min) or (Value > Max) then
      TestDiap := False
    else
      TestDiap := True;
  end;

function IsPktFile(FName: String): boolean;
  label l_e;
  var
    s: TDOSStream;
    head: PacketHeader;
    Flag: boolean;
  begin
    s.Init(FName, stOpenRead);
    if s.Status <> stOK then
      begin
l_e:
        IsPktFile := False;
        s.Done;
        exit;
      end;
    s.Read(head, SizeOf(PacketHeader));
    if s.Status <> stOK then
      goto l_e;
    with head do
      begin
        Flag := TestDiap(Month, 1, 12);
        Flag := Flag and TestDiap(Day, 1, 31);
        Flag := Flag and (origNet > 0);
        Flag := Flag and TestDiap(Minute, 0, 60);
        Flag := Flag and TestDiap(Second, 0, 60);
        Flag := Flag and TestDiap(Hour, 0, 60);
      end;
    if not Flag then
      goto l_e;
    s.Done;
    IsPktFile := True;
  end { IsPktFile };

procedure ViewPktFile(FName: String);
  var
    C: PPktCol;
    D: PPktListDialog;
    W: PView;
    R: TRect;
  begin
    PktFileName := FName;
    if not IsPktFile(FName) then
      begin
        MessageBox(FName+#13+^C+GetString(dlNotPkt), nil,
        mfError+mfOKButton);
        exit;
      end;
    W := WriteMsg(^M^M^C+GetString(dlReadingPkt));
    UpdateWriteView(W);
    C := New(PPktCol, Init(FName));
    W^.Free;
    D := New(PPktListDialog, Init(FName, C));
    Desktop^.ExecView(D);
    Dispose(D, Done);
    if Assigned(C) then
      Dispose(C, Done);
  end { ViewPktFile };

type
  PLimitStream = ^TLimitStream;
  TLimitStream = object(TDOSStream)
    Constructor Init(FileName: FNameStr; Mode: word; Limit: longInt);
    end;

Constructor TLimitStream.Init(FileName: FNameStr; Mode: word; Limit:
    longInt);
  begin
    inherited Init(FileName, Mode);
    if Status = stOK then
      if Limit < StreamSize then
        StreamSize := Limit
  end;

function IsMsgFile(FName: String): boolean;
  label l_e;
  var
    F: PLimitStream;
    l: array[1..5] of longInt;
    i, n: shortint;
    XL: TXlat;
  begin
    NullXLAT(XL);
    New(F, Init(FName, stOpenRead, 16384));
    l[1] := -1;
    l[1] := SearchFileStr(F, XL, #1'MSGID: ', 0, False, False, False,
      False, True, False);
    l[2] := -1;
    l[2] := SearchFileStr(F, XL, #1'FMPT ', 0, False, False, False,
      False, True, False);
    l[3] := -1;
    l[3] := SearchFileStr(F, XL, #1'INTL ', 0, False, False, False,
      False, True, False);
    l[4] := -1;
    l[4] := SearchFileStr(F, XL, #1'TOPT ', 0, False, False, False,
      False, True, False);
    l[5] := -1;
    l[5] := SearchFileStr(F, XL, #1'PID: ', 0, False, False, False,
      False, True, False);
    Dispose(F, Done);
    n := 0;
    for i := 1 to 5 do
      if l[i] > 0 then
        Inc(n);
    if n = 0 then
      IsMsgFile := False
    else
      IsMsgFile := True;
  end { IsMsgFile };

procedure ViewMsgFile(FName: String);
  var
    D: PMsgViewerDlg;
  begin
    if not IsMsgFile(FName) then
      begin
        MessageBox(FName+#13+^C+GetString(dlNotPkt), nil,
        mfError+mfOKButton);
        exit;
      end;
    D := New(PMsgViewerDlg, Init(FName));
    Desktop^.ExecView(D);
    Dispose(D, Done);
  end;

function ViewMsgFileE(FName: String): boolean;
  var
    D: PMsgViewerDlg;
  begin
    if IsMsgFile(FName) then
      begin
        FileName := '';
        KillAfterUse := TempFile <> '';
        TempFile := '';
        FileName := FName;
        ViewMsgFileE := True;
        D := New(PMsgViewerDlg, Init(FName));
        Desktop^.ExecView(D);
        Dispose(D, Done);
      end
    else
      ViewMsgFileE := True;
  end;

function ViewPktFileE(FName: String; MsgVisible: boolean): boolean;
  var
    C: PPktCol;
    D: PPktListDialog;
    W: PView;
    R: TRect;
  begin
    PktFileName := FName;
    if IsPktFile(FName) then
      begin
        FileName := '';
        KillAfterUse := TempFile <> '';
        TempFile := '';
        FileName := FName;
        ViewPktFileE := True;
        W := WriteMsg(^M^M^C+GetString(dlReadingPkt));
        UpdateWriteView(W);
        C := New(PPktCol, Init(FName));
        W^.Free;
        D := New(PPktListDialog, Init(FName, C));
        Desktop^.ExecView(D);
        Dispose(D, Done);
        if Assigned(C) then
          Dispose(C, Done);
      end
    else
      ViewPktFileE := False;
  end { ViewPktFileE };
end.
