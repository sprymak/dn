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
//  dn16rc1-deleting_temporary_files_safety_fix.patch
//  dn16rc1-Bugfix_for_rc1-deleting_temporary_files_safety_fix_patch-diff152byMV.patch
//
//  2.0.0
//  dn200-delete_file_from_temp_fix-diff161byMV.patch
//
//  2.3.0
//  dn230-remove_STRINGS_TPU_dependence.patch
//  dn247-text_and_file_find_improve.patch
//
//  2.7.0
//  dn50208-cleanup.patch
//
//  5.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

{(c) NDN Team}
Unit PktView;
{$F+,O+,V-}
Interface

Uses Objects, FViewer, Dialogs, Views, Drivers,
     {$IFNDEF NONBP}BStrings{$ELSE}Strings{$ENDIF},
     Messages, Dos, collect, Advance1, Advance, Lfn,
     HideView, Scroller;

type
    TNetAddr=record
              Net,Node:word;
             end;

    PCharArray=^TCharArray;
    TCharArray=array[0..65500] of char;

    PPktObj=^TPktObj;
    TPktObj=object(TObject)
              FOfs:Longint;
              Fu,Tu,Su,Dt:PString;
              FA,TA:TNetAddr;
              Constructor Init(O:Longint; Afu,Atu,Asu,Adt:String;
                               FAddr,TAddr:TNetAddr);
              Procedure GetMsgInfo(var FromUser,ToUser,Subj,Date:string);
              Procedure GetMsgTxt(var Buffer:PCharArray; var MsgLen:word);
              Destructor Done; virtual;
            end;


     PPktCol=^TPktCol;
     TPktCol=object(TCollection)
              FName:PString;
              Constructor Init(PktFile:String);
              Destructor Done; virtual;
             end;

     PPktList=^TPktList;
     TPktList=object(TListBox)
                function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
              end;

    PPktListDialog=^TPktListDialog;
    TPktListDialog=object(TDialog)
                      lb:PPktList;
                      Constructor Init(FName:String; C:PPktCol);
                      Destructor Done; virtual;
                      Procedure HandleEvent(var Event:TEvent);  virtual;
                      Function GetPalette:PPalette; virtual;
                   end;

  PLineViewer = ^TLineViewer;
  TLineViewer = object(TScroller)
    FileLines:PLineCollection;
    IsValid: Boolean;
    constructor Init(var Bounds:TRect;AHScrollBar,AVScrollBar:PScrollBar;Buffer:PCharArray);
    destructor Done; virtual;
    procedure Draw; virtual;
    Procedure InitCol(Buffer:PCharArray);
    procedure SetState(AState: Word; Enable: Boolean); virtual;
    function Valid(Command: Word): Boolean; virtual;
  end;

  PMsgViewer=^TMsgViewer;
  TMsgViewer=Object(TLineViewer)
               FromUser,ToUser,Date,Subj:PString;
               Constructor Init(var Bounds:TRect; AHScrollBar,AVScrollBar:PScrollBar; FName:String);
               Destructor Done; virtual;
             end;

     PPktMsgViewer=^TPktMsgViewer;
     TPktMsgViewer=object(TDialog)
                    FV:PLineViewer;
                    Ps1,Ps2,Ps3,Ps4:PString;
                    Constructor Init(Buf:PCharArray;s1,s2,s3,s4:String; MsgN:Word; AllMsg:Word;
                                     FA,TA:TNetAddr);
                    Destructor Done; virtual;
                    Function GetPalette:PPalette; virtual;
                    Procedure HandleEvent(var Event:TEvent); virtual;
                    Procedure SaveAsText;
                   end;

     PMsgViewerDlg=^TMsgViewerDlg;
     TMsgViewerDlg=object(TDialog)
                    Lb1,Lb2,Lb3,Lb4:PLabel;
                    CurMsg:PString;
                    FV:PMsgViewer;
                    Constructor Init(FName:String);
                    Destructor Done; virtual;
                    Function GetPalette:PPalette; virtual;
                    Procedure HandleEvent(var Event:TEvent); virtual;
                    Procedure GotoMsg;
                    Procedure SaveAsText;
                   end;

Function IsPktFile(Fname:String):boolean;
Function IsMsgFile(FName:String):boolean;

Procedure ViewPktFile(FName:String);
Procedure ViewMsgFile(FName:String);
Function ViewPktFileE(FName:String; MsgVisible:Boolean) : Boolean;
Function ViewMsgFileE(FName:String) : Boolean;

const COrigin: word=11;
      CArea  : word=10;
      CCitate: word=14;
      CCluges: word=4{7};
      KillAfterUse : Boolean = false;
      FileName     : String = '';

Implementation

Uses DnHelp, Advance2, U_KeyMap, XTime, Commands, DnApp;

var PktFileName:String;
    PktFromAddr:String;

Type
  PacketHeader = record
          origNode     : word;
          destNode     : word;
          year         : word;
          month        : word;
          day          : word;
          hour         : word;
          minute       : word;
          second       : word;
          baud         : word;
          PacketType   : word;  {generally 0200H}
          origNet      : word;
          destNet      : word;
          ProductCode  : char;
          fill         : array [0..32] of char;
    end;

  PacketMessageHeader = record
          PacketType   : word;     {0200H}
          origNode     : word;
          destNode     : word;
          origNet      : word;
          destNet      : word;
          Attribute    : word;
          cost         : word;
          dateTime     : array [0..19] of char;
{-----------
          toUserName   : array [0..35] of char;
          fromUserName : array [0..35] of char;
          subject      : array [0..71] of char;
-----------}
        end;
type
  MessageHeader = record
    case byte of
    0: (  head : array [1..190] of char);
    1: (  fromUserName : array [0..35] of char;
          toUserName   : array [0..35] of char;
          subject      : array [0..71] of char;
          dateTime     : array [1..20] of char;
          timedRead    : word;
          destNode     : word;
          origNode     : word;
          cost         : word;
          origNet      : word;
          destNet      : word;
          DateWritten  : LongInt;
          DateArriven  : Longint;
          {fill         : array [1..8] of char;}
          replyTo      : word;
          Attribute    : word;
          nextReply    : word);
    end;

  function spaces(num : integer) : string;
  var
    sp1 : integer;
    space : string;
  begin
    space := '';
    for sp1 := 1 to num do
        space := space + ' ';
    spaces := space;
  end;

function rpad(ch : string; num : integer) : string;
begin
    rpad := copy(ch + spaces(num),1,num);
end;

Function DLS(S:string):string;
begin
 if (s[length(s)]='\') then system.dec(byte(s[0]));
 DLS:=S;
end;

function lpad(ch : string; num : integer) : string;
  var
    sp1 : integer;
    sp2 : integer;
  begin
    sp1 := length(ch);
    sp2 := num - sp1;
    lpad := spaces(sp2) + ch;
  end;

Function just(S:String;Len,LRC:longint):string;
begin
 if Len>255 then Len:=255;
 if length(s)>Len then s[0]:=Char(Lo(Len));
 case LRC of
  1: just:=lpad(s,Len);         {'       '+s}
  2: just:=rpad(s,Len);         {s+'       '}
  3: begin                      {'  '+s+'  '}
      while length(s)<Len do s:=' '+s+' ';
      if length(s)>Len then dec(byte(s[0]));
      just:=s;
     end
  else just:=s; {11-01-99,BDS}
 end;
end;

function fns_z(n : integer) : string;
var
 c : string;
begin
  c := ItoS(n);
  if length(c) = 1
   then
    c := '0' + c;
  fns_z := c;
end;

Procedure Msg2Strs(FName:String; var FromUser,ToUser,Subj,Date:String;
                   var C:PLineCollection);
var
    i:integer;
    header:MessageHeader;
    buf:char;
    message:TBufStream; {file of char;}
    txtbuf:string;
begin
  Message.Init(FName,stOpenRead,4096);
  if Message.Status<>stOk then exit;
  for i:=1 to 190 do Message.Read(header.head[i],1);
  FromUser:='';
  for i:=0 to 35 do
   begin
    if Header.FromUserName[i]<>#0 then FromUser:=FromUser+header.fromUserName[i]
    else break;
   end;
  FromUser:=FromUser+',( 2:'+ItoS(Header.orignet)+'/'+ItoS(header.orignode)+' )';
  ToUser:='';
  for i:=0 to 35 do
   begin
    if header.toUserName[i]<>#0 then ToUser:=ToUser+header.toUserName[i]
    else break;
   end;
  ToUser:=ToUser+',( 2:'+ItoS(Header.destNet)+'/'+ItoS(Header.destNode)+' )';
  Subj:='';
  for i:=0 to 71 do
   begin
    if header.subject[i]<>#0 then Subj:=Subj+header.subject[i]
    else break;
   end;
  Date:='';
  for i:=1 to 20 do
   begin
    if header.dateTime[i]<>#0 then Date:=Date+header.dateTime[i]
    else break;
   end;
  i:=1;
  TxtBuf:='';
  while Message.Status=stOk do
    begin
      Message.Read(buf,1);
      if (buf=#13) or (length(txtBuf)>=76) then
        begin
          if buf<>#13 then txtBuf:=txtBuf+Buf;
          C^.Insert(NewStr(TxtBuf));
          txtbuf:='';
        end
      else TxtBuf:=TxtBuf+Buf;
    end;
  Message.Done;
end;

Function ReadPktHeader(FName:String; var PType:Word; var DT,FromUser,ToUser:string):boolean;
label l1;
var
    PH:PacketHeader;
    {F:File;}
    {Rb:Word;}
    Res:boolean;
    F:TDosStream;
begin
 F.Init(FName,stOpenRead);
 if F.Status<>stOk then goto l1;
 F.Read(PH,SizeOf(PacketHeader));
 if F.Status>stOk then
  begin
  l1:
   PType:=0;
   Dt:='Unknown date and time';
   FromUser:='Unknown';
   ToUser:=FromUser;
   Res:=false;
  end
else begin
with PH do
 begin
  PType:=PacketType;
  Dt:='Date: '+fns_z(day)+'-'+fns_z(month)+'-'+ItoS(Year)+
      '  Time: '+fns_z(hour)+':'+fns_z(Minute)+':'+fns_z(second);
  FromUser:='From: 2:'+ItoS(origNet)+'/'+ItoS(origNode);
  ToUser:=  'To  : 2:'+ItoS(destNet)+'/'+ItoS(destNode);
 end;
  Res:=true;
  end;
 F.Done;
 ReadPktHeader:=Res;
end;

Constructor TPktObj.Init(O:Longint; Afu,Atu,Asu,Adt:String;FADdr,TAddr:TNetAddr);
begin
 Inherited Init;
 FOfs:=O;
 fu:=NewStr(Afu);
 su:=NewStr(Asu);
 tu:=NewStr(Atu);
 dt:=NewStr(Adt);
 FA:=FAddr; TA:=TAddr;
end;

Constructor TPktCol.Init(PktFile:String);
label l1;
var
    Rb:Word;
    i:Word;
    LPos,LSize:Longint;
    PH:PacketHeader;
    PMH:PacketMessageHeader;
    ch:char;
    EmptyPkt:boolean;
    AFrm,ATo:TNetAddr;
    St:String;
    Date,FromUser,ToUser,Subj:string;
    F:TBufStream;
begin
 Inherited Init(0,1);
 FName:=NewStr(PktFile);
 F.Init(PktFile,stOpenRead,4096);
 if F.Status<>stOk then goto l1;
 F.Read(PH,SizeOf(PacketHeader));
 EmptyPkt:=(F.GetPos=F.GetSize);
 if EmptyPkt then begin
                    F.Done;
                    l1:
                    exit;
                  end;
  LSize:=F.GetSize;
repeat
 LPos:=F.GetPos;
 F.Read(PMH,SizeOf(PacketMessageHeader));
 if F.Status<>stOk then break;
 Date:=StrPas(PMH.DateTime);
 with Pmh do
  begin
    AFrm.Net:=OrigNet;
    AFrm.Node:=OrigNode;
    Ato.Net:=DestNet;
    Ato.Node:=DestNode;
  end;
 st:='';
 repeat
  F.Read(Ch,1);
  if ch<>#0 then st:=st+Ch;
 until ch=#0;
 ToUser:=st; st:='';
 repeat                                     {fromuser}
  F.Read(Ch,1);
  if ch<>#0 then st:=st+ch;
 until ch=#0;
 FromUser:=st; st:='';
 repeat
  F.Read(Ch,1);
  if ch<>#0 then st:=st+ch;
 until ch=#0;
 if St='' then St:=' ';
 Subj:=St;
 Insert(New(PPktObj,Init(LPos,FromUSer,ToUser,Subj,Date,Afrm,Ato)));
 repeat
   F.Read(Ch,1);
 until ch=#0;
until LPos=LSize;
 F.Done;
end;

Procedure TPktObj.GetMsgInfo(var FromUser,ToUser,Subj,Date:string);
begin
 FromUser:=fu^;
 ToUser:=tu^;
 Subj:=su^;
 Date:=dt^;
end;

Destructor TPktObj.Done;
begin
 DisposeStr(tu);
 DisposeStr(fu);
 DisposeStr(su);
 DisposeStr(dt);
 if KillAfterUse then begin
   EraseTempFile(FileName);
   KillAfterUse:=False;
   TempFile:='';
 end;
 Inherited Done;
end;

Procedure TPktObj.GetMsgTxt(var Buffer:PCharArray; var MsgLen:Word);
var
    LPos:Longint;
    I:Word;
    PMH:PacketMessageHeader;
    ch:char;
    F:TBufStream;
begin
 MsgLen:=0;
 i:=0;
 F.Init(PktFileName,stOpenRead,4096);
 LPos:=FOfs;
 F.Seek(LPos);
 F.Read(PMH,SizeOf(PacketMessageHeader));
 if F.Status<>stOk then begin F.Done; exit; end;
 for i:=1 to 3 do
  begin
    repeat F.Read(Ch,1); until ch=#0;
  end;
 I:=0;
 repeat
  F.Read(Ch,1);
  Buffer^[i]:=ch;
  Inc(i);
 until ch=#0;
 MsgLen:=i;
 F.Read(ch,1);
 F.Done;
end;

Destructor TPktCol.Done;
begin
 DisposeStr(Fname);
 Inherited Done;
end;

var PktCol:PPktCol;

Procedure InitPktCol(FName:String);
begin
 PktCOl:=New(PPktCol,Init(FName));
end;

Procedure DonePktCOl;
begin
 Dispose(PktCOl,Done);
end;

Function DelChar(s:string; Ch:Char):string;
begin
 while Pos(Ch,s)<>0 do delete(s,Pos(Ch,s),1);
 DelChar:=s;
end;

Procedure CutFromAddr(st:String);
begin
 while st[1]<>' ' do delete(st,1,1);
 DelRight(st);
 DelLeft(st);
 while st[1]<>' ' do
  begin
   PktFromAddr:=PktFromAddr+st[1];
   delete(st,1,1);
  end;
end;

Procedure Buffer2Strs(Buffer:PCharArray; var C:PLineCollection);
var
    I:word;
    Ch:Char;
    st:string;
begin
 ST:='';
 PktFromAddr:='';
 i:=0;
 repeat
  Ch:=Buffer^[i];
  if ch in [#13,#10] then
    begin
       st:=DelChar(delChar(st,#13),#10);
       if (st[1]=#1) and (Pos('MSGID',st)<>0) then CutFromAddr(st);
       C^.Insert(NewStr(St));
       st:='';
    end;
  if ch<>#0 then begin
                  if length(st)>76 then begin
                                          st:=st+Ch;
                                          if not (Buffer^[i+1] in [' ',#13,#10,#0]) then
                                           begin
                                            if Buffer^[i+2] in [' ',#13,#10,#0] then
                                             st:=st+Buffer^[i+1];
                                             inc(i);
                                           end;
                                          st:=DelChar(DelChar(st,#13),#10);
                                          C^.Insert(NewStr(St));
                                          St:='';
                                        end
                  else st:=st+ch;
                 end
  else begin
         st:=DelChar(delChar(st,#13),#10);
         if (st[1]=#1) and (Pos('MSGID',st)<>0) then CutFromAddr(st);
         C^.Insert(NewStr(St));
         st:='';
       end;
  Inc(i);
 until ch=#0;
end;

Function TPktList.GetText(Item: LongInt; MaxLen: Integer): String;
var
    Pc:PPktObj;
    s1,s2,s3:string;
    FromUser,ToUser,Subj,Date:String;
begin
 Pc:=List^.At(Item);
 Pc^.GetMsgInfo(FromUser,ToUser,Subj,Date);
 s1:=just(FromUser,26,2); s2:=just(ToUser,26,2); s3:=just(Subj,26,2);
 GetText:=s1+s2+s3;
end;

Destructor TPktListDialog.Done;
begin
 Dispose(lb,Done);
 Inherited Done;
end;

 Const
  CPktDialog     = #112#113#114#115#116#117#117#119#120#121#122#123 +
                   #124#125#46#47#48#49#50#51#52#53#54#55#1#117#118 +
                   #1#1#61#62#63#178#179;

Function TPktListDialog.GetPalette:PPalette;
const Pal:String=CPktDialog;
begin
 GetPalette:=@Pal;
end;

Constructor TPktListDialog.Init(FName:String; C:PPktCol);
var R: TRect;
    View: PView;
begin
  HelpCtx:=hcPktListDialog;
  Desktop^.GetExtent(R);
  Inherited Init(R,GetString(dlPktView)+FName);
  Options:=Options or {ofCentered}ofSelectable;
  GrowMode:=gfGrowHiX + gfGrowHiY;
  View:=StandardScrollBar(sbVertical+sbHandleKeyBoard);
  EventMask:=$FFFF;
  inc(View^.Origin.Y); dec(View^.Size.Y);
  Insert(View);
   GetExtent(R);
   R.Grow(-1,-1);
   inc(R.A.Y);
   lb := New(PPktList, Init(R, 1,PScrollBar(View)));
   lb^.Options := lb^.Options + ofFramed;
   Insert(lb);
   lb^.NewList(C);
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
end;

function PktHeaderDlg(AText:String): PDialog;
var R:TRect;
    Dialog:PDialog;
    View: PView;
begin
  R.Assign(13, 6, 66, 17);
  Dialog := New(PDialog, Init(R, GetString(dlPktHeader)));
  with Dialog^ do
  begin
    Options := Options + ofCenterX + ofCenterY;
    R.Assign(20, 8, 32, 10);
    View := New(PButton, Init(R, 'Ok',cmOk,bfDefault));
    View^.Options := View^.Options + ofCenterX;
    Insert(View);
    R.Assign(2, 1, 51, 8);
    View := New(PStaticText, Init(R,AText));
    Insert(View);
    SelectNext(False);
  end;
  PktHeaderDlg:=Dialog
end;

Procedure ViewPktHeader(MsgCount:Integer);
var
    W:Word;
    D:PDialog;
    s1,s2,s3:String;
    S:String;
begin
 ReadPktHeader(PktFileName,W,s1,s2,s3);
 S:=GetString(dlPktFile)+FExpand(PktFileName)+#13+S1+#13+S2+#13+S3+#13+
    {!!!'Packet Size: '+PrintUsing('###,###',GetFileSize(PktFileName))+' byte(s)'+#13+}
    GetString(dlPktMsg)+ItoS(MsgCount);
 D:=PktHeaderDlg(S);
 Desktop^.ExecView(D);
 Dispose(D,Done);
end;

Procedure TPktListDialog.HandleEvent(var Event:TEvent);
var
    M:Word;
    Pc:PPktObj;
    Buf:PCharArray;
    D:PPktMsgViewer;
    s1,s2,s3,s4:string;
begin
 if (Event.What=evKeyDown) then
 case Event.KeyCode of
 kbEnter,kbF3:
  begin
   Pc:=Lb^.List^.At(lb^.focused);
   If PC = nil then exit;
   New(Buf);
   Pc^.GetMsgTxt(Buf,M);
   s1:=pc^.fu^; s2:=pc^.tu^; s3:=pc^.su^; s4:=pc^.dt^;
   D:=New(PPktMsgViewer,Init(Buf,s1,s2,s3,s4,lb^.Focused+1,lb^.List^.Count,
                             pc^.FA,pc^.TA));
   Dispose(Buf);
   Desktop^.ExecView(D);
   Dispose(D,Done);
  end;
 kbF2:ViewPktHeader(lb^.List^.Count);
 end;
 Inherited HandleEvent(Event);
end;

constructor TLineViewer.Init(var Bounds: TRect; AHScrollBar,
  AVScrollBar: PScrollBar; Buffer:PCharArray);
begin
  HelpCtx:=hcLineViewer;
  Options:=Options or {ofCentered}ofSelectable;
  GrowMode:=gfGrowHiX + gfGrowHiY;
  EventMask:=$FFFF;
  TScroller.Init(Bounds, AHScrollbar, AVScrollBar);
  TLineViewer.InitCol(Buffer);
end;

destructor TLineViewer.Done;
begin
  Dispose(FileLines, Done);
  TScroller.Done;
end;

procedure TLineViewer.Draw;
var
  I: Integer;
  OrigColor,AreaColor,Citate,WorkStrColor:Word;
  P: PString;
  CNormal: Byte;
  Color:Byte;
  Bkg:Byte;
  B: TDrawBuffer;
  S: String;
begin
  CNormal := GetColor(1);
  Bkg:=lo((CNormal and $F0) shr 4);
  OrigColor:=Bkg*16+COrigin;
  AreaColor:=Bkg*16+CArea;
  Citate:=Bkg*16+CCitate;
  WorkStrColor:=Bkg*16+CCluges;

  for I := 0 to Size.Y - 1 do
  begin
    MoveChar(B, ' ', CNormal, Size.X);
    if Delta.Y + I < FileLines^.Count then
    begin
      P := FileLines^.At(Delta.Y + I);
      if P <> nil then S := Copy(P^, Delta.X + 1, Size.X)
      else S := '';
      Color:=CNormal;
      if Pos('AREA',s)<>0 then Color:=AreaColor;
      if Pos('>',s)<>0 then Color:=Citate;
      if Pos('Origin',s)<>0 then Color:=OrigColor;
      if (Pos('SEEN-BY',s)<>0) then Color:=WorkStrColor;
      if (s[1]=#1) then begin
                         system.delete(s,1,1);
                         Color:=WorkStrColor;
                        end;
      MoveStr(B, S, Color);
    end;
    WriteLine(0, I, Size.X, 1, B);
  end;
end;

procedure TLineViewer.InitCol(Buffer:PCharArray);
begin
  IsValid := True;
  FileLines := New(PLineCollection, Init(5,5));
  Buffer2Strs(Buffer,FileLines);
  Limit.X := 255;
  Limit.Y := FileLines^.Count;
end;

procedure TLineViewer.SetState(AState: Word; Enable: Boolean);
begin
  TScroller.SetState(AState, Enable);
  if Enable and (AState and sfExposed <> 0) then
     SetLimit(Limit.X, Limit.Y);
end;

function TLineViewer.Valid(Command: Word): Boolean;
begin
  Valid := IsValid;
end;

constructor TMsgViewer.Init(var Bounds: TRect; AHScrollBar,
  AVScrollBar: PScrollBar; FName:String);
var s1,s2,s3,s4:string;
begin
  HelpCtx:=hcMsgViewer;
  TScroller.Init(Bounds, AHScrollbar, AVScrollBar);
  Options:=Options or {ofCentered}ofSelectable;
  GrowMode:=gfGrowHiX + gfGrowHiY;
  EventMask:=$FFFF;
  FileLines:=New(PLineCollection, Init(5,5));
  Msg2Strs(FName,s1,s2,s3,s4,FileLines);
  FromUser:=NewStr(s1);
  ToUser:=NewStr(s2);
  Subj:=NewStr(s3);
  Date:=NewStr(s4);
  Limit.X := 255;
  Limit.Y := FileLines^.Count;
end;

Destructor TMsgViewer.Done;
begin
 DisposeStr(FromUSer);
 DisposeStr(ToUser);
 DisposeStr(Date);
 DisposeStr(Subj);
 if KillAfterUse then begin
   EraseTempFile(FileName);
   KillAfterUse:=False;
   TempFile:='';
 end;
 Inherited Done;
end;


Constructor TPktMsgViewer.Init(Buf:PCharArray; s1,s2,s3,s4:String; MsgN:Word; AllMsg:Word;
                               FA,TA:TNetAddr);
var R: TRect;
    View:PView;
    VS:PScrollBar;
    S:String;
begin
  HelpCtx:=hcPktMsgViewer;
  Desktop^.GetExtent(R);
  Inherited Init(R,GetString(dlViewMsg));
  Options := Options or ofCentered;
  VS:=StandardScrollBar(sbVertical + sbHandleKeyboard);
  Inc(VS^.Origin.Y,5); dec(VS^.Size.Y,5);
  Insert(VS);
  GetExtent(R); R.Grow(-1,-1); inc(R.a.y,5);
  EventMask:=$FFFF;
  FV:=New(PLineViewer, Init(R,nil,VS,Buf));
  FV^.Options:=FV^.Options or ofFramed;
  Insert(FV);
    R.Assign(1, 1, 79, 2);
    S:='~Msg~  :'+just(ItoS(MsgN),5,1)+'/'+just(ItoS(AllMsg),5,2)+spaces(30)+'~Date~ :'+s4;
    View := New(PLabel, Init(R,S,nil));
    Insert(View);
    Ps1:=NewStr(DelChar(S,'~'));
    R.Assign(1, 2, 79, 3);
    S:='~From~ :'+just(s1,50,2)+PktFromAddr;
    View := New(PLabel, Init(R,S,nil));
    Insert(View);
    Ps2:=NewStr(DelChar(S,'~'));
    R.Assign(1, 3, 79, 4);
    S:='~To~   :'+just(s2,50,2)+'2:'+ItoS(TA.NET)+'/'+ItoS(TA.Node);
    View := New(PLabel, Init(R,S,nil));
    Insert(View);
    Ps3:=NewStr(DelChar(S,'~'));
    R.Assign(1, 4, 79, 5);
    S:='~Subj~ :'+s3;
    View := New(PLabel, Init(R,S,nil));
    Insert(View);
    Ps4:=NewStr(DelChar(S,'~'));
    SelectNext(False);
end;

Function TPktMsgViewer.GetPalette:PPalette;
const Pal:string=CPktDialog;
begin
 GetPalette:=@Pal;
end;

Destructor TPktMsgViewer.Done;
begin
 DisposeStr(Ps1);
 DisposeStr(Ps2);
 DisposeStr(Ps3);
 DisposeStr(Ps4);
 Dispose(fv,done);
 inherited done;
end;

Procedure TPktMsgViewer.SaveAsText;
var
    F:Text;
    TName:String;
    Area:String;
     {--------------------------------------------}
     Procedure WriteFile(Ps:PString); far;
     begin
      if Ps=nil then writeln(f,' ') else writeln(f,DelChar(Ps^,#1));
     end;
     {--------------------------------------------}
     Procedure FindArea(Ps:PString); far;
     var S:String;
     begin
      if Area<>'' then exit;
      S:=Ps^;
      if Pos('AREA',S)<>0 then
       begin
        while Pos('AREA',S)<>1 do system.delete(S,1,1);
        System.Delete(S,Pos('AREA',S),4);
        Delleft(S);
        DelRight(S);
        while (S[1]<>' ') and (S<>'') do
         begin Area:=Area+S[1]; system.delete(s,1,1); end;
       end;
     end;
     {--------------------------------------------}
begin
 TName:='';
 if InputBox(GetString(dlSaveMsg),GetString(dlMsgFile),TName,255,0)=
  cmCancel then exit;
 Assign(F,TName);
 DoneSysError;
 if ExistFile(TName) then {$I-} Append(f) {$I+}
 else {$I-} rewrite(f); {$I+}
 if IOResult<>0 then begin
                      MessageBox(GetString(dlErrorWriting)+TName+'!', nil,
                       mfError + mfOKButton );
                      exit;
                     end;
 InitSysError;
 Area:='';
 Fv^.FileLines^.ForEach(@FindArea);
 writeln(f,GetString(dlLine));
 writeln(f,'AREA: '+Area);
 writeln(f,Ps1^);
 writeln(f,Ps2^);
 writeln(f,Ps3^);
 writeln(f,Ps4^);
 writeln(f,GetString(dlLine));
 Fv^.FileLines^.ForEach(@WriteFile);
 system.close(f);
end;

Procedure TPktMsgViewer.HandleEvent(var Event:TEvent);
begin
 if (Event.What=evKeyDown) then
  case Event.KeyCode of
   kbF2:SaveAsText;
  end;
 Inherited HandleEvent(Event);
end;

Constructor TMsgViewerDlg.Init(FName:String);
var R: TRect;
    View:PView;
    VS:PScrollBar;
begin
  Desktop^.GetExtent(R);
  Inherited Init(R,GetString(dlNetMailView)+FExpand(FName));
  Options := Options or ofCentered;
  GrowMode:=gfGrowHiX + gfGrowHiY;
  EventMask:=$FFFF;
  VS:=StandardScrollBar(sbVertical + sbHandleKeyboard);
  Inc(VS^.Origin.Y,5); dec(VS^.Size.Y,5);
  Insert(VS);
  GetExtent(R); R.Grow(-1,-1); Inc(R.a.y,5);
  FV:=New(PMsgViewer, Init(R,nil,VS,FName));
  FV^.Options:=FV^.Options or ofFramed;
  Insert(FV);
  R.Assign(1, 1, 79, 2);
  Lb1 := New(PLabel, Init(R, '~Msg~  :Private '+spaces(30)+'~Date~ :'+fv^.date^,nil));
  Insert(Lb1);
  R.Assign(1, 2, 79, 3);
  lb2 := New(PLabel, Init(R, '~From~ :'+fv^.fromuser^, nil));
  Insert(lb2);
  R.Assign(1, 3, 79, 4);
  lb3 := New(PLabel, Init(R, '~To  ~ :'+fv^.touser^, nil));
  Insert(lb3);
  R.Assign(1, 4, 79, 5);
  lb4 := New(PLabel, Init(R, '~Subj~ :'+fv^.subj^, nil));
  Insert(lb4);
  SelectNext(False);
  CurMsg:=NewStr(FName);
end;

Function TMsgViewerDlg.GetPalette:PPalette;
const Pal:string=CPktDialog;
begin
 GetPalette:=@Pal;
end;

Function GetPrevMsg(FName:String):String;
var
    Er:integer;
    Num:Word;
    Tm: TEventTimer;
    VM: PView;
    Sr:lSearchRec;
    D,N,E:String;
begin
 lFSplit(FName,D,N,E);
 while (N[1]='0') and (N<>'') do system.delete(N,1,1);
 if N='' then N:='0';
 Val(N,Num,Er); NewTimer(Tm, 4); VM:=nil;
 if (Er<>0) or (Num=0) then GetPrevMsg:=''
 else begin
       while Num>0 do
        begin
         If (VM=nil) and TimerExpired(Tm) then
          vm:=WriteMsg(GetString(dlPleaseStandBy));
         dec(Num);
         lFindFirst(MakeNormName(D, ItoS(Num)+'.msg'),AnyFile-Directory,Sr);
         if DosError=0 then
          begin
           GetPrevMsg:=MakeNormName(D, Sr.FullName);
           If vm<>nil then Dispose(vm, Done);
           lFindClose(Sr);
           exit;
          end;
         lFindClose(Sr);
        end;
       if (Num=0) then GetPrevMsg:='';
       If vm<>nil then Dispose(vm, Done);
      end;
end;

Function GetNextMsg(FName:String):String;
var
    Er:integer;
    Num, St:Word;
    Tm: TEventTimer;
    VM: PView;
    Sr:lSearchRec;
    D,N,E:String;
begin
 lFSplit(FName,D,N,E);
 while (N[1]='0') and (N<>'') do system.delete(N,1,1);
 if N='' then N:='0';
 Val(N,Num,Er); St:=Num; NewTimer(Tm, 4); VM:=nil;
 if (Er<>0) then GetNextMsg:=''
 else begin
       while Num<St+1000 do
        begin
         If (VM=nil) and TimerExpired(Tm) then
          vm:=WriteMsg(GetString(dlPleaseStandBy));
         Inc(Num);
         lFindFirst(MakeNormName(D, ItoS(Num)+'.msg'),AnyFile-Directory,Sr);
         if DosError=0 then
          begin
           GetNextMsg:=MakeNormName(D, Sr.FullName);
           If vm<>nil then Dispose(vm, Done);
           lFindClose(Sr);
           exit;
          end else
           begin
            GetNextMsg:='';
            lFindClose(Sr);
            continue;
           end;
        end;
       If vm<>nil then Dispose(vm, Done);
     end;
end;

Procedure TMsgViewerDlg.GotoMsg;
var VS:PScrollBar;
    R:TRect;
begin
  DisposeStr(Title);
  Title:=NewStr(GetString(dlNetMailView)+CurMsg^);
  Dispose(FV,Done);
  VS:=StandardScrollBar(sbVertical + sbHandleKeyboard);
  EventMask:=$FFFF;
  Inc(VS^.Origin.Y,5); dec(VS^.Size.Y,5);
  Insert(VS);
  GetExtent(R); R.Grow(-1,-1); Inc(R.a.y,5);
  FV:=New(PMsgViewer, Init(R,nil,VS,CurMsg^));
  FV^.Options:=FV^.Options or ofFramed;
  Insert(FV);
  Dispose(lb1,Done); lb1 := nil;
  Dispose(lb2,Done); lb2 := nil;
  Dispose(lb3,Done); lb3 := nil;
  Dispose(lb4,Done); lb4 := nil;
  R.Assign(1, 1, 79, 2);
  If fv <> nil then
  lb1 := New(PLabel, Init(R, '~Msg  ~:Private '+spaces(30)+'~Date~ :'+fv^.date^,nil));
  Insert(lb1);
  R.Assign(1, 2, 79, 3);
  lb2 := New(PLabel, Init(R, '~From ~:'+fv^.fromuser^, nil));
  Insert(lb2);
  R.Assign(1, 3, 79, 4);
  lb3 := New(PLabel, Init(R, '~To   ~:'+fv^.touser^, nil));
  Insert(lb3);
  R.Assign(1, 4, 79, 5);
  lb4 := New(PLabel, Init(R, '~Subj ~:'+fv^.subj^, nil));
  Insert(lb4);
  SelectNext(False);
  Redraw;
end;

Procedure TMsgViewerDlg.SaveAsText;
var
    F:Text;
    TName:String;
     Procedure WriteFile(Ps:PString); far;
     begin
      if Ps=nil then writeln(f,' ') else writeln(f,DelChar(Ps^,#1));
     end;
begin
 TName:='';
 if InputBox(GetString(dlSaveMsg),GetString(dlMsgFile),TName,255,0)=
  cmCancel then exit;
 Assign(F,TName);
 DoneSysError;
 if ExistFile(TName) then {$I-} Append(f) {$I+}
 else {$I-} rewrite(f); {$I+}
 if IOResult<>0 then begin
                      MessageBox(GetString(dlErrorWriting)+TName+'!', nil,
                       mfError + mfOKButton );
                      exit;
                     end;
 InitSysError;
 writeln(f,#196#196#196#196#196#196#196#196#196'['+CenterStr(GetName(CurMsg^),17)
           +']'#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196
           +#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196
           +#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196);
 writeln(f,'Msg  : Private '+spaces(30)+'Date : '+fv^.date^);
 writeln(f,'From : '+fv^.fromuser^);
 writeln(f,'To   : '+fv^.touser^);
 writeln(f,'Subj : '+fv^.subj^);
 writeln(f,GetString(dlLine));
 Fv^.FileLines^.ForEach(@WriteFile);
 system.close(f);
end;

Procedure TMsgViewerDlg.HandleEvent(var Event:TEvent);
var
    Er:Byte;
    S,SOld:String;
begin
 if Event.What=evKeyDown then
  case Event.KeyCode of
   kbGrayPlus,kbRight:begin
               S:=GetNextMsg(CurMsg^);
               if S<>'' then begin
                              DisposeStr(CurMsg);
                              CurMsg:=NewStr(S);
                              GotoMsg;
                             end
               else MessageBox(GetString(dlLastMsg), nil,
                mfError + mfOKButton );
              end;
   kbGrayMinus,kbLeft:begin
                S:=GetPrevMsg(CurMsg^);
                if S<>'' then begin
                              DisposeStr(CurMsg);
                              CurMsg:=NewStr(S);
                              GotoMsg;
                             end
                else MessageBox(GetString(dlFirstMsg), nil,
                 mfError + mfOKButton );
               end;
   kbF8,kbDel:begin
   if MessageBox(GetString(dlDelMsg)+CurMsg^+'?', nil,
                 mfConfirmation + mfOKButton + mfCancelButton) = cmOk then
    BEGIN
               SOld:=CurMsg^;
               S:=GetPrevMsg(CurMsg^);
               if S<>'' then begin
                              DisposeStr(CurMsg);
                              CurMsg:=NewStr(S);
                              GotoMsg;
                              EraseFile(SOld);
                             end
               else begin
                     S:=GetNextMsg(CurMsg^);
                     if S<>'' then begin
                                    DisposeStr(CurMsg);
                                    CurMsg:=NewStr(S);
                                    GotoMsg;
                                    EraseFile(SOld);
                                   end
                     else begin
                           EraseFile(SOld);
                           EndModal(cmCancel);
                          end;
                   end;
     END;
          end;
   kbF2:SaveAsText;
  end; {case}
 Inherited HandleEvent(Event);
end;

Destructor TMsgViewerDlg.Done;
begin
 Dispose(fv,done);
 Dispose(lb1,Done); Dispose(lb2,Done);
 Dispose(lb3,Done); Dispose(lb4,Done);
 DisposeStr(CurMsg);
 inherited done;
end;

Function TestDiap(Value,Min,Max:word):boolean;
begin
 if (Value<Min) or (Value>Max) then TestDiap:=false
 else TestDiap:=true;
end;

Function IsPktFile(Fname:String):boolean;
label l_e;
var
    Head:PacketHeader;
    flag:boolean;
    S:TDosStream;
begin
 S.Init(FName,stOpenRead);
 if S.Status<>stOk then begin
                        l_e:
                         IsPktFile:=false;
                         S.Done;
                         exit;
                        end;
 S.Read(Head,SizeOf(PacketHeader));
 if S.Status<>stOk then goto l_e;
 with Head do
  begin
   flag:=TestDiap(Month,1,12);
   flag:=Flag and TestDiap(day,1,31);
   flag:=flag and (OrigNet > 0);
   flag:=Flag and TestDiap(Minute,0,60);
   flag:=Flag and TestDiap(Second,0,60);
   flag:=Flag and TestDiap(Hour,0,60);
  end;
 if not flag then goto l_e;
 S.Done;
 IsPktFile:=true;
end;

Procedure ViewPktFile(FName:String);
var C:PPktCol;
    D:PPktListDialog;
    W: PView;
    R:TRect;
begin
 PktFileName:=FName;
 if not IsPktFile(FName) then
  begin
    MessageBox( FName+#13+^C+GetString(dlNotPkt), nil,
     mfError + mfOKButton );
    exit;
  end;
 W:=WriteMsg(^M^M^C+GetString(dlReadingPkt));
 UpdateWriteView(W);
 C:=New(PPktCol,Init(FName));
 W^.Free;
 D:=New(PPktListDialog,Init(FName,C));
 Desktop^.ExecView(D);
 Dispose(d,Done);
 if Assigned(C) then Dispose(C,Done);
end;

type PLimitStream = ^TLimitStream;
     TLimitStream = object(TDosStream)
                     constructor Init(FileName: FNameStr; Mode: Word; Limit: Longint);
                    end;

constructor TLimitStream.Init(FileName: FNameStr; Mode: Word; Limit: Longint);
begin
 inherited Init(FileName, Mode);
 if Status=stOk then
  if Limit<StreamSize then StreamSize:=Limit
end;

Function IsMsgFile(FName:String):boolean;
label l_e;
var
    L:array[1..5] of Longint;
    i,N:shortint;
    XL: TXlat;
    F:PLimitStream;
begin
 NullXLAT(XL);
 New(F,Init(FName,stOpenRead,16384));
 L[1]:=-1;
 L[1]:=SearchFileStr(F,XL,#1'MSGID: ',0,false,false,false,false,True,Nil);
 L[2]:=-1;
 L[2]:=SearchFileStr(F,XL,#1'FMPT ',0,false,false,false,false,True,Nil);
 L[3]:=-1;
 L[3]:=SearchFileStr(F,XL,#1'INTL ',0,false,false,false,false,True,Nil);
 L[4]:=-1;
 L[4]:=SearchFileStr(F,XL,#1'TOPT ',0,false,false,false,false,True,Nil);
 L[5]:=-1;
 L[5]:=SearchFileStr(F,XL,#1'PID: ',0,false,false,false,false,True,Nil);
 Dispose(F,Done);
 N:=0;
 for i:=1 to 5 do if L[i]>0 then inc(N);
 if N=0 then IsMsgFile:=false else IsMsgFile:=true;
end;

Procedure ViewMsgFile(FName:String);
var D:PMsgViewerDlg;
begin
 if not IsMsgFile(FName) then
  begin
    MessageBox( FName+#13+^C+GetString(dlNotPkt), nil,
     mfError + mfOKButton );
    exit;
  end;
 D:=New(PMsgViewerDlg,Init(FName));
 Desktop^.ExecView(D);
 Dispose(D,Done);
end;

Function ViewMsgFileE(FName:String) : Boolean;
var D:PMsgViewerDlg;
begin
 if IsMsgFile(FName) then
      begin
       FileName := '';
       KillAfterUse:=TempFile <> '';
       TempFile:='';
       FileName := FName;
       ViewMsgFileE := True;
       D:=New(PMsgViewerDlg,Init(FName));
       Desktop^.ExecView(D);
       Dispose(D,Done);
      end else ViewMsgFileE := True;
end;

Function ViewPktFileE(FName:String; MsgVisible:boolean) : Boolean;
var C:PPktCol;
    D:PPktListDialog;
    W:PView;
    R:TRect;
begin
 PktFileName:=FName;
 if IsPktFile(FName) then
     Begin
       FileName := '';
       KillAfterUse:=TempFile <> '';
       TempFile:='';
       FileName := FName;
       ViewPktFileE := True;
       W:=WriteMsg(^M^M^C+GetString(dlReadingPkt));
       UpdateWriteView(W);
       C:=New(PPktCol,Init(FName));
       W^.Free;
       D:=New(PPktListDialog,Init(FName,C));
       Desktop^.ExecView(D);
       Dispose(d,Done);
       if Assigned(C) then Dispose(C,Done);
     End else ViewPktFileE := False;
 end;
end.
