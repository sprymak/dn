{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.12
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
unit DNUtil2;

interface
uses Menus, Objects;

 procedure OpenWindow(ChDrive: Boolean);
 procedure OpenTreeWindow;
 function NewVideoMenu ( Items : PMenuItem ) : PMenu;
 procedure SelectVideoModeDialog;
 procedure MessageBoxAbout;
 {$IFDEF SpreadSheet}
 procedure LoadSheet(const SheetName: String);
 procedure OpenSheet;
 {$ENDIF}
 procedure StoreColors;
 procedure LoadColors; {JO}
 function TryRunOS2(S: PString): Boolean;
 function ExecCommandLine: Boolean;
 procedure ExecDOSCmd;
 procedure GetUserScreen;
 procedure Rebound;
 procedure GetFromClip(PS: PString);
 procedure RSearchAdvance;
 procedure PutInClip(PS: PString);
 procedure DoQuickChange;
 procedure ExecTree;
 procedure ToggleCmdLine;
 procedure DoExecFile(PS: PString);
 procedure DoExecString(PS: PString);
 procedure ChLngId;
 procedure ShowTimeInfo;

implementation
uses DnApp, filescol, advance, gauges, views, xdblwnd, Tree, commands, dos,
     lfn, Drivers, Videoman, DnHelp, DnIni, Startup, Advance1, DnStdDlg,
     ColorSel, advance2, Drives, DnSvLd, advance4, cmdline, dnutil, dnexec,
     flpanelx, messages, usermenu, winclp, microed, filefind, collect,
     advance7
     {$IFDEF SpreadSheet},Calc{$ENDIF}
     ;

 procedure OpenWindow(ChDrive: Boolean);
  var S: String;
      R: TRect;
      P: PView;
 begin
  Desktop^.GetExtent(R);
  S[1] := #0;
  if ChDrive then
   begin
    S := SelectDrive(R.A.X + (R.B.X - R.A.X) div 2, R.A.Y, #0, Off);
    if S = '' then Exit;
    S[1] := Char(Byte(S[1])-64);
   end;
{$IFDEF TrashCan}
  if TrashCan^.ImVisible then Desktop^.Delete(TrashCan);
{$ENDIF}
  P := nil;
  P := New(PXDoubleWindow, Init(R, 0, Byte(S[1])));
  if Abort then Dispose(P,Done) else Application^.InsertWindow(PWindow(P));
{$IFDEF TrashCan}
  if TrashCan^.ImVisible then Desktop^.Insert(TrashCan);
{$ENDIF}
 end;

 procedure OpenTreeWindow;
  var S: String;
      r: TRect;
      P: PView;
 begin
   Desktop^.GetExtent(R);
   S := ChangeDir(GetString(dlSelectDirectory), 0);
   if S = '' then Exit; ClrIO;
   lChDir(S); ClrIO;
   S[1] := Char(Byte(S[1])-64);
   P := New(PXDoubleWindow, Init(R, 0, Byte(S[1])));
   Message(P, evCommand, cmDirTree, nil);
   if Abort then Dispose(P,Done) else Application^.InsertWindow(PWindow(P));
 end;

 function NewVideoMenu ( Items : PMenuItem ) : PMenu;
 var
   M : PMenu;
 begin
   M := NewMenu ( Items );
   while Items <> nil do begin
     if Items^.Command-cmSwitch = ScreenMode then begin
       M^.Default := Items;
       break;
     end;
     Items := Items^.Next;
   end;
   NewVideoMenu := M;
 end;

 procedure SelectVideoModeDialog;
 var
   P: PMenuBox;
   M: PMenu;
   R: TRect;
   W: Word;
   I: PMenuItem;
 begin
   I := nil;
   if VideoType < vtVGA then begin
     if VideoType = vtEGA then begin
       I := NewItem ( '~1~ 80 x 43', '', kbNoKey, sm80x43+cmSwitch, hcNoContext,
            nil);
     end;
     M := NewVideoMenu (
            NewItem ( '~1~ 40 x 25', '', kbNoKey, sm40x25+cmSwitch, hcNoContext,
            NewItem ( '~1~ 80 x 25', '', kbNoKey, sm40x25+cmSwitch, hcNoContext,
            I))
          );
   end else begin { VideoType >= vtVGA }
     if VideoType >= vtSVGA then begin
       I := NewSubMenu ( '~4~ 132 x ??', hcNoContext,
              NewVideoMenu (
              NewItem ( '~1~ 132 x 25', '', kbNoKey, $109+cmSwitch, hcNoContext,
              NewItem ( '~2~ 132 x 43', '', kbNoKey, $10A+cmSwitch, hcNoContext,
              NewItem ( '~3~ 132 x 50', '', kbNoKey, $10B+cmSwitch, hcNoContext,
              NewItem ( '~4~ 132 x 60', '', kbNoKey, $10C+cmSwitch, hcNoContext,
              nil))))),
            nil);
     end;
     M := NewMenu (
            NewSubMenu ( '~1~  40 x ??', hcNoContext,
              NewVideoMenu (
              NewItem ( '~1~ 40 x 12','', kbNoKey, sm40x12+cmSwitch, hcNoContext,
              NewItem ( '~2~ 40 x 14','', kbNoKey, sm40x14+cmSwitch, hcNoContext,
              NewItem ( '~3~ 40 x 25','', kbNoKey, sm40x25+cmSwitch, hcNoContext,
              NewItem ( '~4~ 40 x 30','', kbNoKey, sm40x30+cmSwitch, hcNoContext,
              NewItem ( '~5~ 40 x 34','', kbNoKey, sm40x34+cmSwitch, hcNoContext,
              NewItem ( '~6~ 40 x 43','', kbNoKey, sm40x43+cmSwitch, hcNoContext,
              NewItem ( '~7~ 40 x 50','', kbNoKey, sm40x50+cmSwitch, hcNoContext,
              NewItem ( '~8~ 40 x 60','', kbNoKey, sm40x60+cmSwitch, hcNoContext,
              nil))))))))),
            NewSubMenu ( '~2~  80 x ??', hcNoContext,
              NewVideoMenu (
              NewItem ( '~1~ 80 x 12','', kbNoKey, sm80x12+cmSwitch, hcNoContext,
              NewItem ( '~2~ 80 x 14','', kbNoKey, sm80x14+cmSwitch, hcNoContext,
              NewItem ( '~3~ 80 x 25','', kbNoKey, sm80x25+cmSwitch, hcNoContext,
              NewItem ( '~4~ 80 x 30','', kbNoKey, sm80x30+cmSwitch, hcNoContext,
              NewItem ( '~5~ 80 x 34','', kbNoKey, sm80x34+cmSwitch, hcNoContext,
              NewItem ( '~6~ 80 x 43','', kbNoKey, sm80x43+cmSwitch, hcNoContext,
              NewItem ( '~7~ 80 x 50','', kbNoKey, sm80x50+cmSwitch, hcNoContext,
              NewItem ( '~8~ 80 x 60','', kbNoKey, sm80x60+cmSwitch, hcNoContext,
              nil))))))))),
            NewSubMenu ( '~3~  94 x ??', hcNoContext,
              NewVideoMenu (
              NewItem ( '~1~ 94 x 12','', kbNoKey, sm94x12+cmSwitch, hcNoContext,
              NewItem ( '~2~ 94 x 14','', kbNoKey, sm94x14+cmSwitch, hcNoContext,
              NewItem ( '~3~ 94 x 25','', kbNoKey, sm94x25+cmSwitch, hcNoContext,
              NewItem ( '~4~ 94 x 30','', kbNoKey, sm94x30+cmSwitch, hcNoContext,
              NewItem ( '~5~ 94 x 34','', kbNoKey, sm94x34+cmSwitch, hcNoContext,
              NewItem ( '~6~ 94 x 43','', kbNoKey, sm94x43+cmSwitch, hcNoContext,
              NewItem ( '~7~ 94 x 50','', kbNoKey, sm94x50+cmSwitch, hcNoContext,
              NewItem ( '~8~ 94 x 60','', kbNoKey, sm94x60+cmSwitch, hcNoContext,
              nil))))))))),
            I)))
          );
   end;
   {PZ begin 2000.11.14}
   case ScreenMode of
     sm40x25..sm40x60: W := 1;
     sm80x25..sm80x60: W := 2;
     sm94x25..sm94x60: W := 3;
     $109..$10C:       W := 4;
   else                W := 2; { 80 columns }
   end;
   M^.Default:=LookUpMenu(M, W, dfByPosition);
   {PZ end 2000.11.14}
   R.Assign(1,1,20,4);
   P:=New(PMenuBox, Init(R, M, nil));
   P^.Options:=P^.Options or ofCentered;
   P^.HelpCtx:=hcCustomVideo;
   W:=Application^.ExecView(P);
   Dispose(P);
   DisposeMenu(M);
   if (W=0) or (W=cmCancel) then exit;
   Dec(W, cmSwitch);
   {PZ begin 2000.06.29}
   SetScrMode ( W );
   if StoreVideoMode <> 0 then begin
     case StoreVideoMode of
       1: smSVGALo := W;
       2: smSVGAHi := W;
     else
       StoreVideoMode := 0;
     end;
     SystemData.Mode1 := ItoS( smSVGALo );
     SystemData.Mode2 := ItoS( smSVGAHi );
     Message ( Application, evCommand, cmUpdateConfig, nil );
   end;
   {PZ end}
 end;

 procedure MessageBoxAbout;
 var D: array [1..2] of PString ;
 begin
 {$IFDEF DNPRG}
   D[1]:=NewStr(VersionName); D[2]:=NewStr(#13+#3+'Compiled '+VersionDate);
   MessageBox2( dlAbout , ^C'This product is a FREEWARE'#13,@D,Nil, mfAbout + mfOKButton );
   DisposeStr(D[1]);          DisposeStr(D[2]);
 {$ENDIF}
 end;

 {$IFDEF SpreadSheet}
         {-DataCompBoy-}
 procedure LoadSheet(const SheetName: String);
 var R: TRect;
 begin
  DeskTop^.GetExtent(R);
  DeskTop^.Insert(Application^.ValidView(New(PCalcWindow, Init(R, SheetName))));
 end;
        {-DataCompBoy-}

        {-DataCompBoy-}
 procedure OpenSheet;
  var FN: String;
 begin
  if GetFileName(FN,'*.WKZ', GetString(dlOpenFile), GetString(dlOpenFileName),
                     fdOpenButton) = cmFileOpen then  LoadSheet(FN);
 end;
        {-DataCompBoy-}
 {$ENDIF}

 procedure StoreColors;
 var FN: String; S: TDosStream; Pal: PString;
     vID: longint;
 begin
  FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL', GetString(dlStoreColorPal), GetString(dlFileName),
                          fdOKButton + fdHelpButton, hsColors);
  if FN = '' then Exit;
  S.Init(FN, stCreate);
  if S.Status = 0 then
   begin
    Pal := PString(Application^.GetPalette);
    S.WriteStr(Pal);
    StoreIndexes(S);
    vID := $50414756 ; { VGAP }
    S.Write( vID , SizeOF( vID )) ;
    S.Write( VGA_Palette, SizeOF( VGA_Palette )) ;
    vID := $4B4E4C42 ; { BLNK }
    S.Write( vID , SizeOF( vID )) ;
    S.Write( CurrentBlink , SizeOf( CurrentBlink ) );
   end;
  S.Done;
  FN := GetPath(FN); RereadDirectory(FN);
 end;

 procedure LoadColors; {JO}
   var More: Boolean;
       None: Boolean;
       FN: String;
 begin
  More := True;
  None := False;
  FN := GetFileNameMenu(SourceDir+'COLORS\', '*.PAL', '', On, More, None);
     if More then
       FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL', GetString(dlLoadColorPal), GetString(dlFileName),
                              fdOKButton + fdHelpButton, hsColors);
  if FN = '' then Exit;
  LoadPalFromFile(FN);
 end; {JO}

 function TryRunOS2(S: PString): Boolean;
  var B: Boolean;
      ST: SessionType;
 begin
   TryRunOS2 := On;
   DelLeft(S^); DelRight(S^);
   if S^ = '' then Exit;
   if S^ = '' then Exit;
   TryRunOS2 := Off;
   if not OS2exec then Exit;
   case S^[1] of
    '>': begin B := Off; ST := stOS2FullScreen end;
    '<': begin B := On;  ST := stOS2FullScreen end;
    ']': begin B := Off; ST := stOS2Windowed end;
    '[': begin B := On;  ST := stOS2Windowed end;
     else if (CmdLine.Str <> '') and (PCommandLine(CommandLine)^.LineType in [ltOS2Window,ltOS2FullScreen]) then
            begin
              S^ := ' '+S^; B := ShiftState and 3 <> 0;
              if PCommandLine(CommandLine)^.LineType = ltOS2Window then ST := stOS2Windowed
                                                                   else ST := stOS2FullScreen
            end else
             Exit;
   end;
   TryRunOS2 := On;
   RunOS2Command(Copy(S^,2,255), B, ST);
   CmdLine.StrModified := On;
   Message(CommandLine, evKeyDown, kbDown, nil);
 end;

 function ExecCommandLine: Boolean;
  var S: String;
      up: TUserParams;
 begin
  ExecCommandLine := Off;
  S := '';
  CommandLine^.GetData(S);
  if (DelSpaces(S) = '') then Exit;
  ExecCommandLine := On;
  if HandleChDirCommand then
   begin
    DelLeft(S); DelRight(S);
    if (S[1] in ['c','C']) and (S[2] in ['d','D']) and (S[3]=' ')
    then
     begin
      DelFC(S); DelFC(S); DelLeft(S); S:=DelSquashes(S);
      if PathExist(S) then
       Message(Application, evBroadcast, cmChangeDirectory, @S);
      S:='';CommandLine^.SetData(S);CommandLine^.DrawView;
      exit;
     end;
    if (S[0]=#2) and ValidDrive(UpCase(S[1])) and (S[2]=':') then
    begin
     if s[1]='*' then s:=cTEMP_;
     Message(Desktop, evBroadcast, cmChangeDrv, @S);
     S:='';CommandLine^.SetData(S);CommandLine^.DrawView;
     exit;
    end;
    CommandLine^.GetData(S);
   end;
  if not CheckExit then begin ExecCommandLine:=true; Exit end;
  if TryRunOS2(@S) then Exit;
  Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
  ExecString(@S, #13 + ActiveDir+'>' + S);
  S:='';CommandLine^.SetData(S);CommandLine^.DrawView;
 end;

 procedure ExecDOSCmd;
   var Nm: String;
       Xt: String;
       ST: String;
 begin
   ST := '';
   Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
   lFSplit(CnvString(CurFileActive), FreeStr, Nm, Xt);
   if InExtFilter(CnvString(CurFileActive), Executables) then ST := Nm+Xt+' ';
   if InputBox(GetString(dlExecDOScmdHdr), GetString(dlExecDOScmdLine), ST, 128, hsExecDOSCmd) <> cmOK then Exit;
    if not TryRunOS2(@ST) and CheckExit
     then ExecString(@St, #13#10 + ActiveDir+'>' + ST);
   DisposeStr(CurFileActive); CurFileActive := nil;
   DisposeStr(CurFilePassive); CurFilePassive := nil;
 end;

 procedure GetUserScreen;
  var P: PView;
      PP: PView;
 begin
  PP := nil;
  P := ViewPresent(cmShowOutput, @PP);
  if PP <> nil then PP^.Select else Desktop^.Insert(New(PUserWindow, Init));
 end;

 procedure Rebound;
 var R: TRect;
 begin
  Application^.GetExtent(R);
  if InterfaceData.Options and ouiHideMenu = 0 then Inc(R.A.Y);
  if SystemData.Options and ouiHideStatus = 0 then Dec(R.B.Y);
  Dec(R.B.Y);
  Desktop^.Locate(R);
  R.A.Y := R.B.Y; Inc(R.B.Y);
  CommandLine^.Locate(R);
 end;

 procedure GetFromClip(PS: PString);
 begin
   if SystemData.Options and ossUseSysClip <> 0 then SyncClipOut(On);
   if (MicroEd.Clipboard = nil) or (MicroEd.Clipboard^.At(0) = nil) then
    PS^ := '' else
     PS^ := PString(MicroEd.Clipboard^.At(0))^;
 end;

 procedure RSearchAdvance;
 begin
  if ExecResource(dlgAdvanceSearch, AdvanceSearchData) = cmOK then begin
   PView(Desktop^.Current {Event.InfoPtr})^.GetData(FindRec);
   FindRec.Options := FindRec.Options or ffoAdvanced;
   PView(Desktop^.Current {Event.InfoPtr})^.SetData(FindRec);
  end;
 end;

 procedure PutInClip(PS: PString);
 begin
   if MicroEd.Clipboard <> nil then Dispose(MicroEd.Clipboard,Done);
   MicroEd.Clipboard := New(PLineCollection, Init(1,1));
   MicroEd.Clipboard^.Insert(NewStr(PS^));
   if SystemData.Options and ossUseSysClip <> 0 then SyncClipIn;
   if ClipBoardStream<>nil then
    ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-1, 0));
   CopyLines2Stream(MicroEd.Clipboard, ClipBoardStream);
 end;

 procedure DoQuickChange;
  var R: TRect;
      P: PMenuBox;
      Menu: PMenu;
      Items: PMenuItem;
      C: Char;
      N,Q,J: Integer;
 begin
  Items := nil;
  Q:=0; J:=0;
  for N := 8 downto 0 do
    if DirsToChange[N] <> nil then begin
      FreeStr:='~'+Itos(N+1)+'~ '+ CutH(DirsToChange[N]^,40);
      Items := NewItem(FreeStr, 'Alt-'+Itos(N+1), kbAlt1, cmQuickChange1 + N,
        hcNoContext, Items);
      J:=Max(CStrLen(FreeStr), J);
      inc(q);
    end;
  If Items = NIL then begin
    Msg( erNoQuickDirs, NIL, mfWarning + mfCancelButton );
    Exit;
  end;
  R.Assign(Application^.Size.X div 2 - J div 2,
           Application^.Size.Y div 2 - Q div 2,
           Application^.Size.X div 2 + J div 2 + J mod 2,
           Application^.Size.Y div 2 + Q div 2 + Q mod 2);
  Menu := NewMenu(Items);
  P := New(PMenuBox, Init(R, Menu, nil));
  P^.Options := P^.Options or ofCentered;
  P^.HelpCtx := hcQuickDirs;

  N := Application^.ExecView(P);
  Dispose(P,Done);
  DisposeMenu(Menu);
  if N >= cmQuickChange1 then Message(Desktop, evCommand, N, nil);
 end;

  procedure ExecTree;
    type TGR = record
                S: String;
                Dummy: Array[0..32767] of byte;
               end;
    var GR: ^TGR;
  begin
    New(GR);
    Desktop^.Current^.GetData(GR^);
    {GR^.S := lFExpand(GR^.S);}
    GR^.S := ChangeDir(GetString(dlChooseDir), 0);
    if GR^.S <> '' then Desktop^.Current^.SetData(GR^.S);
    Dispose(GR);
  end;

    procedure ToggleCmdLine;
      var R: TRect;
          B: Boolean;
    begin
      if CommandLine = nil then Exit;
      B := (not CommandLine^.GetState(sfVisible)) and (Str <> '');
      Application^.GetExtent( R );
      if InterfaceData.Options and ouiHideMenu = 0 then Inc(R.A.Y);
      if InterfaceData.Options and ouiHideStatus = 0 then Dec(R.B.Y);
      if B then Dec( R.B.Y );
      Desktop^.Locate( R );
      R.A.Y := R.B.Y;
      R.B.Y := R.A.Y + Byte(B);
      CommandLine^.Locate( R );
      CommandLine^.SetState( sfVisible, B);
    end;

    procedure DoExecFile(PS: PString);
    begin
      if not TryRunOS2(PS) and CheckExit and
             not ExecCommandLine then ExecFile(PS^);
    end;

    procedure DoExecString(PS: PString);
    begin
      if not TryRunOS2(PS) and CheckExit then ExecString(PS, '');
    end;

    procedure ChLngId;
    var S:string; SS:string[9]; L:TStringCollection; SR:lSearchRec;
        Current,Default:PMenuItem; Menu:PMenu; CurIdx,i:integer;
        HMB:PMenuBox; R:TRect; V:PView;
        function LngMixCase(P:string):string;
        var i:integer;
        begin
            for i:=1 to Length(P) do if (i=1) or not (P[i-1] in ['a'..'z',
            'A'..'Z']) then P[i]:=UpCase(P[i]) else if P[i] in ['A'..'Z']
            then P[i]:=Chr(Ord(P[i])+Ord('a')-Ord('A'));
            LngMixCase:=P
        end;
    begin
        L.Init(5,5);
        S:=SourceDir; if S[Byte(S[0])]<>'\' then S:=S+'\';
        lFindFirst(S+'*.LNG',AnyFile,SR);
        while DosError=0 do begin
            if ((SR.SR.Attr and Directory)=0) and (SR.FullSize>0) then begin
                SS[0]:=#0;
                while SR.FullName[Byte(SS[0])+1]<>'.'
                do SS:=SS+SR.FullName[Byte(SS[0])+1];
                SS:=LngMixCase(SS);
                if ValidLngId(SS,False) then L.Insert(NewStr(SS))
            end;
            lFindNext(SR)
        end;
        lFindClose(SR);
        S:=StartupDir; if S[Byte(S[0])]<>'\' then S:=S+'\';
        lFindFirst(S+'*.LNG',AnyFile,SR);
        while DosError=0 do begin
            if ((SR.SR.Attr and Directory)=0) and (SR.FullSize>0) then begin
                SS[0]:=#0;
                while SR.FullName[Byte(SS[0])+1]<>'.'
                do SS:=SS+SR.FullName[Byte(SS[0])+1];
                SS:=LngMixCase(SS);
                if ValidLngId(SS,False) and (L.IndexOf(@SS)=-1) then
                L.Insert(NewStr(SS));
            end;
            lFindNext(SR)
        end;
        lFindClose(SR);
        S:=LngMixCase(LngId);
        CurIdx:=L.IndexOf(@S);
        if CurIdx=-1 then begin
            S:='ENGLISH';
            CurIdx:=L.IndexOf(@S)
        end;
        if CurIdx=-1 then CurIdx:=0;
        if L.Count>0 then begin
            if ShowLanguageMenu then begin
                Current:=nil;
                for i:=L.Count-1 downto 0 do begin
                    if i=9 then S:='~0~ ' else if i<9 then
                    S:='~'+Chr(Ord('1')+i)+'~ ' else S:='  ';
                    Current:=NewItem(S+CutH(PString(L.At(i))^,40),'',kbNoKey,
                    cmCancel+1+i,hcChLngId,Current);
                    if i=CurIdx then Default:=Current
                end;
                Menu:=NewMenu(Current);
                Menu^.Default:=Default;
                Desktop^.GetExtent(R);
                R.A.X:=((R.A.X+R.B.X) div 2)-8;
                R.B.X:=R.A.X+16;
                R.A.Y:=((R.A.Y+R.B.Y-L.Count) div 2)-1;
                R.B.Y:=R.A.Y+L.Count+2;
                if R.A.Y<Desktop^.Origin.Y+1 then R.A.Y:=Desktop^.Origin.Y+1;
                if R.B.Y>Desktop^.Origin.Y+Desktop^.Size.Y-1
                then R.B.Y:=Desktop^.Origin.Y+Desktop^.Size.Y-1;
                New(HMB,Init(R,Menu,nil));
                CurIdx:=Desktop^.ExecView(HMB)-cmCancel-1;
                Dispose(HMB,Done);
                DisposeMenu(Menu)
            end else CurIdx:=(CurIdx+1) mod L.Count;
            if (CurIdx>=0) and (PString(L.At(CurIdx))^<>
            LngMixCase(ActiveLanguage)) and CheckExit then begin
                ActiveLanguage:=PString(L.At(CurIdx))^;
                if ActiveLanguage=LngMixCase(GetEnv('DNLNG'))
                then ActiveLanguage:='';
                SaveDnIniSettings ( @ActiveLanguage );
                DoneIniEngine;
                ProbeINI(INIstoredtime,INIstoredsize,INIstoredcrc);
                ConfigModified:=True;
                StringCache^.FreeAll;
                ExecString(@NullStr,'');
            end;
        end;
        L.FreeAll;
        L.Done;
    end;

procedure ShowTimeInfo;
begin
   FreeStr := SStr(DDTimer div 360000, 2, '0');
   AddStr(FreeStr, ':');
   DDTimer := DDTimer mod 360000;
   Insert(SStr(DDTimer div 6000, 2, '0'), FreeStr, Length(FreeStr)+1);
   AddStr(FreeStr, ':');
   DDTimer := DDTimer mod 6000;
   Insert(SStr(DDTimer div 100, 2, '0'), FreeStr, Length(FreeStr)+1);
   AddStr(FreeStr, '.');
   Insert(SStr(DDTimer mod 100, 2, '0'), FreeStr, Length(FreeStr)+1);
   MessageBox(GetString(dlElapsedTime)+FreeStr, nil, mfOKButton+mfInformation);
end;


end.
