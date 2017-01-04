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

UNIT
  FLPanelX;


INTERFACE

USES
  LFN, LFNCol, {DataCompBoy} advance, advance1, advance2, advance3, Objects,
  Views, Drivers, Dos, FilesCol, HideView, Drives, filediz, collect, Arvid;

TYPE
  PFilePanelRoot = ^TFilePanelRoot;
  TFilePanelRoot = object(THideView)
   isValid, MSelect, SelectFlag, Loaded, ChangeLocked: Boolean;
   InfoView, DirView, DriveLine: PHideView;
   Delta, OldDelta, OldPos, DeltaX: LongInt;
   Files: PFilesCollection;
   SortMode: Byte;
   DirectoryName, OldDirectory: String;
   FileMask: String;
   SearchParam: TQuickSearchData; {DataCompBoy}
   ScrollBar: PMyScrollBar;
   DrawDisableLvl, SelNum, LineLength: LongInt;
   SelectedLen, PackedLen: TSize;
   WasActive, PosChanged, CommandEnabling,
   QuickSearch, ViewEnabled: Boolean;
   TotalInfo, FreeSpace: String[50];
   PanelFlags: Word;
   LastDriveFlags: Word;
   Drive: PDrive;
   ForceReading: Boolean;
   DriveState: Word;
   LastCurPos: TPoint;

   constructor Init(var Bounds: TRect; ADrive: Integer; AScrBar: PMyScrollBar);
   constructor Load(var S: TStream);
   procedure Store(var S: TStream);
   destructor Done; virtual;
   procedure Awaken; virtual;
   procedure ChDirName;
   procedure ShowView; virtual;
   procedure HideView; virtual;
   procedure CommandHandle(var Event: TEvent);
   procedure ChangeBounds(var Bounds: TRect); virtual;
   function  Valid(Command: Word): Boolean; virtual;
   procedure GetUserParams(var FileRec: PFileRec; var List: String; BuildList: boolean);
   procedure ReadDirectory;
   procedure RereadDir;
   procedure SetDirName; virtual;
   procedure SendLocated;
   procedure IncDrawDisabled;
   procedure DecDrawDisabled;
   procedure ChkNoMem;
   procedure ChDir(Dir: string);
  end;

{                                                                        }
{ WARNING: The following vars are mirrored in FLPANEL.PAS via ABSOLUTEs! }
{                                                                        }

CONST
  ActivePanel: Pointer = NIL;
  CtrlWas: Boolean = False;
  DirsToChange: Array [0..9] of PString = (nil,nil,nil,nil,nil,nil,nil,nil,nil,nil);
  CurFileActive: PString = nil;
  CurFilePassive: PString = nil;


VAR
  CurrentDirectory: String;
  PShootState: Word;


IMPLEMENTATION

uses
  Messages, DnApp, DnHelp, Startup, Commands, Histries, HistList, FlTools,
  FileFind, CmdLine, ArcView, Archiver, DiskImg, DiskInfo, FileCopy, DnUtil,
  dnini {-$VIV}
  {$IFDEF MODEM}{$IFDEF LINK}, NavyLink{$ENDIF}{$ENDIF}
{$IFDEF UUENCODE},UUCode{$ELSE}
{$IFDEF UUDECODE},UUCode{$ENDIF}{$ENDIF}
  ;

procedure TFilePanelRoot.IncDrawDisabled;
begin
  Inc(DrawDisableLvl);
end;

procedure TFilePanelRoot.DecDrawDisabled;
begin
  Dec(DrawDisableLvl);
  ChkNoMem;
end;

procedure TFilePanelRoot.ChkNoMem;
begin
  if Drive^.NoMemory and (DrawDisableLvl = 0) then
  begin
    Application^.OutOfMemory;
    Drive^.NoMemory := False;
  end;
end;

constructor TFilePanelRoot.Init;
begin
 inherited Init(Bounds);
 ForceReading := Off;
 CommandEnabling := On;
 ChangeLocked := Off;
 Abort := Off;
 HelpCtx := hcFilePanel;
 GrowMode := gfGrowHiY + gfGrowHiX;
 Options := Options or ofSelectable or ofTopSelect or ofFirstClick;
 PanelFlags := PanelDefaults.Show;

 EventMask := $FFFF;

 ScrollBar := AScrBar; Loaded := Off; OldDelta := -1; PosChanged := Off;
 isValid := On;

 if (ADrive <= 0) or not ValidDrive(Char(ADrive+64)) then ADrive := 0;
 Drive := New(PDrive, Init(ADrive, @Self, 0));
 DeltaX := 0;

 if Abort then begin IsValid := False; Exit end;
 {GetDir(ADrive, DirectoryName);}

 {Drive^.Owner := @Self;}
 FileMask := x_x; Files := nil;

 if PanelDefaults.Sort < 6 then SortMode := PanelDefaults.Sort + 1 else
 if PanelDefaults.Sort = 6 then SortMode := 30
                           else SortMode := 0;
 IncDrawDisabled; MSelect := Off;
 QuickSearch := Off; ViewEnabled := Off;
 if not Abort then ReadDirectory;
 isValid := isValid and not Abort; DecDrawDisabled;
end;

constructor TFilePanelRoot.Load;
 var I: LongInt;
     dumm: array[1..20] of byte;
begin
 inherited Load(S);
 ChangeLocked := Off;
 GetPeerViewPtr(S, ScrollBar);
 GetPeerViewPtr(S, DirView);
 GetPeerViewPtr(S, InfoView);
 GetPeerViewPtr(S, DriveLine);
 Drive := PDrive(S.Get);
 if Drive = nil then New(Drive, Init(0, @Self, 0)) else Drive^.Owner:=@Self;
 DeltaX := 0;
 Drive^.RereadDirectory('');
 if Drive <> nil then begin
  Drive^.SizeX := Size.X;
  DirectoryName := Drive^.GetDir;
 end;
 S.Read(FileMask[0], 1); S.Read(FileMask[1], Length(FileMask));
 S.Read(SortMode, 1);
 S.Read(PanelFlags, 2);
 S.Read(Delta, 2); OldDelta := -1; PosChanged := Off;
 S.Read(ForceReading, 1);
 if Drive<>nil then begin
  S.Read(Drive^.LFNLen,    SizeOf(Drive^.LFNLen   ));
  S.Read(Drive^.EXTLen,    SizeOf(Drive^.EXTLen   ));
  S.Read(Drive^.DirFLP,    SizeOf(Drive^.DirFLP   ));
  S.Read(Drive^.FilFLP,    SizeOf(Drive^.FilFLP   ));
  S.Read(Drive^.Param,     SizeOf(Drive^.Param    ));
  S.Read(Drive^.Flags,     SizeOf(Drive^.Flags    ));
 end else
  S.Read(dumm,SizeOf(Drive^.LFNLen)+SizeOf(Drive^.EXTLen)+
              SizeOf(Drive^.DirFLP)+SizeOf(Drive^.FilFLP)+
              SizeOf(Drive^.Param) +SizeOf(Drive^.Flags));
 Files := PFilesCollection(S.Get);
 if Files <> nil then
  for I := 0 to Files^.Count-1 do PFileRec(Files^.At(I))^.Owner := @Drive^.CurDir; {DataCompBoy}
 Loaded := On; MSelect := Off; isValid := True; QuickSearch := Off; ViewEnabled := Off;
 CommandEnabling := On;
end;

procedure TFilePanelRoot.Awaken;
begin
 RereadDir;
 if (DriveLine <> nil) then
   if (FMSetup.Show and fmsDriveLine <> 0) then
     begin
       if not DriveLine^.GetState(sfVisible) then DriveLine^.Show;
     end else
          if DriveLine^.GetState(sfVisible) then DriveLine^.Hide;
end;

procedure TFilePanelRoot.Store;
begin
 inherited Store(S);
 PutPeerViewPtr(S, ScrollBar);
 PutPeerViewPtr(S, DirView);
 PutPeerViewPtr(S, InfoView);
 PutPeerViewPtr(S, DriveLine);
 if (ActivePanel <> @Self) or (Drive^.DriveType <> dtDisk)
    or (StartupData.UnLoad and osuPreserveDir <> 0) then S.Put(Drive) else S.Put(nil);
 S.Write(FileMask, 1 + Length(FileMask));
 S.Write(SortMode, 1);
 S.Write(PanelFlags, 2);
 S.Write(Delta, 2);
 S.Write(ForceReading, 1);
 S.Write(Drive^.LFNLen,    SizeOf(Drive^.LFNLen   ));
 S.Write(Drive^.EXTLen,    SizeOf(Drive^.EXTLen   ));
 S.Write(Drive^.DirFLP,    SizeOf(Drive^.DirFLP   ));
 S.Write(Drive^.FilFLP,    SizeOf(Drive^.FilFLP   ));
 S.Write(Drive^.Param,     SizeOf(Drive^.Param    ));
 S.Write(Drive^.Flags,     SizeOf(Drive^.Flags    ));
 PFilesCollection(Files)^.Selected := ScrollBar^.Value;
 S.Put(Files);
end;

function TFilePanelRoot.Valid;
begin
 Valid := isValid;
 if (Command = cmClose) then Valid := Drive^.Disposable;
end;

destructor TFilePanelRoot.Done;
 var P: PDrive;
begin
 if Files <> nil then Dispose(Files,Done); Files:=nil;
 if Drive <> nil then Dispose(Drive,Done); Drive:=nil;
 inherited Done;
end;

procedure TFilePanelRoot.ShowView;
begin
 if InfoView <> nil then InfoView^.ShowView;
 if DirView <> nil then DirView^.ShowView;
 if (ScrollBar <> nil) and GetState(sfActive+sfSelected) then
   ScrollBar^.Show;
 if Loaded then RereadDir;
 inherited ShowView;
end;

procedure TFilePanelRoot.HideView;
begin
 if InfoView <> nil then InfoView^.HideView;
 if DirView <> nil then DirView^.HideView;
 if (ScrollBar <> nil) then ScrollBar^.Hide;
 inherited HideView;
end;

procedure TFilePanelRoot.ChangeBounds;
 var R: TRect;
     I: LongInt;
begin
 I := Byte(( Drive^.DriveType = dtFind ) or ( Drive^.DriveType = dtTemp ) or ( Drive^.DriveType = dtArc ));
 I := Byte(FMSetup.Show and fmsDivider <> 0) +
      Byte(PanelFlags and fmiCurrent <> 0)*(1 + I) +
      Byte(PanelFlags and fmiSelected <> 0) +
      Byte(PanelFlags and fmiTotals <> 0) +
      Byte(PanelFlags and fmiFree <> 0)*Byte(I=0);
 Dec(Bounds.B.Y, I);
 SetBounds(Bounds);
 R := Bounds; R.B.Y := R.A.Y; Dec(R.A.Y);
 if DirView <> nil then DirView^.SetBounds(R);
 R := Bounds; R.A.Y := R.B.Y; Inc(R.B.Y, I);
 if InfoView <> nil then InfoView^.SetBounds(R);
 if DriveLine <> nil then
   begin
     R.A.Y := R.B.Y + Byte(FMSetup.Show and fmsDriveLine = 0);
     Inc(R.B.Y);
     DriveLine^.SetBounds(R);
     if (FMSetup.Show and fmsDriveLine <> 0) then
       begin
         if not DriveLine^.GetState(sfVisible) then DriveLine^.Show;
       end else
            if DriveLine^.GetState(sfVisible) then DriveLine^.Hide;
   end;
 if ScrollBar <> nil then
  begin R :=Bounds; R.A.X := R.B.X; Inc(R.B.X); ScrollBar^.SetBounds(R) end;
 If Drive <> nil then begin
  Drive^.SizeX := Size.X;
  DirectoryName := Drive^.GetDir;
 end;
end;

procedure TFilePanelRoot.SendLocated;
 var S: String;
     R: LongInt;
begin
 if (Files^.Count = 0) or (Drive^.DriveType = dtArc) or
    (State and sfVisible = 0) then Exit;
 R := ScrollBar^.Value;
 if R >= Files^.Count then Exit;
 if not ViewEnabled and (Drive^.Flags and psShowLongDesc = 0) then Exit;
 S := MakeNormName(PFileRec(Files^.At(R))^.Owner^,
                   GetLFN(PFileRec(Files^.At(R))^.LFN));
 if ViewEnabled or (Drive^.Drivetype < dtArc)
    then Message(Owner, evCommand, cmLoadViewFile, @S);
end;


        {-DataCompBoy-}
procedure TFilePanelRoot.RereadDir;
 var P,PP,CurP: PFileRec;
     I, J, Pos: LongInt;
     FC {,FCC}: PFilesCollection;
     NewDir: String;
     BB: Boolean;
     TF: TFileRec;
     WasLoaded, B: Boolean;

begin
 Abort := Off; OldDelta := Delta; PosChanged := Off; WasLoaded := Loaded;
 if Loaded then begin
  LineLength := Drive^.CalcLength;
  ScrollBar^.SetParams(PFilesCollection(Files)^.Selected, 0, Files^.Count - 1,
                      (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0)) *
                      ((Size.X+1) div LineLength), 1);
  CurrentDirectory := ActiveDir;
  Loaded := Off;
  if WasActive and (UpStrg(ActiveDir) <> UpStrg(DirectoryName)) then
   begin DirectoryName := ActiveDir; ReadDirectory;
         ScrollBar^.SetValue(0); Exit end;
 end;
 if (Files = nil) or (Files^.Count = 0) then begin OldDelta := -1; Delta := 0 end;
 ClrIO; Loaded := Off; ChDirName;
 if Abort then begin Dispose(Files,Done); New(Files, Init(10, 10)); Abort := Off; Exit; end;
 NewDir := Drive^.GetRealDir;
 if Abort then begin Dispose(Files,Done); New(Files, Init(10, 10)); Abort := Off; Exit; end;
 if GetState(sfFocused) then CurrentDirectory := DirectoryName;
 if Files^.Count = 0 then
  begin  DirectoryName := NewDir; ReadDirectory; ChDirName; Exit end;
 FC := PFilesCollection(Files); Files := nil; IncDrawDisabled;
 Pos := ScrollBar^.Value;
 if UpStrg(NewDir) <> UpStrg(DirectoryName) then
  begin Pos := 0; DirectoryName := NewDir end;
 I := 0; CurP := FC^.At(Pos);
 while (I < FC^.Count) do
   begin
     P := FC^.At(I); {-$VOL}
     if (P <> CurP) and (P <> nil) and not P^.Selected then
       FC^.AtReplace(I,nil); {-$VOL}
     Inc(I);
   end;
 ReadDirectory;
 B := (Drive^.DriveType <> dtTemp) and (Drive^.DriveType <> dtFind);
 if (FC^.Count > 0) then
  begin
   SelNum := 0; SelectedLen := 0; PackedLen := 0; BB := Off;
   for I := 0 to FC^.Count-1 do
    begin
      PP := FC^.At(I);
      if (PP <> nil) and PP^.Selected or (PP = CurP) then
        for J := 0 to Files^.Count - 1 do
         begin
           P := Files^.At(J); {-$VOL}
           if (UpStrg(GetLFN(P^.LFN)) = UpStrg(GetLFN(PP^.LFN)))
              and (WasLoaded or (UpStrg(P^.Owner^) = UpStrg(PP^.Owner^))) then
             begin
              TF := PP^;
              PP^ := P^;
              P^ := TF;
              PP^.Selected := TF.Selected;
              FC^.AtPut(I,P); {-$VOL}
              Files^.AtPut(J,PP); {-$VOL}
              if PP^.Name[1] = '.' then PP^.Selected := Off;
              if PP^.Selected then
              begin
                SelectedLen := SelectedLen + PP^.Size;
                PackedLen := PackedLen + PP^.PSize;
                Inc(SelNum);
              end;
              Break;
           end;
         end;
    end;
   if not BB then
    begin
     for I := 0 to Files^.Count-1 do
      begin
        P := Files^.At(I);
        if (UpStrg(GetLFN(CurP^.LFN)) = UpStrg(GetLFN(P^.LFN))) and (UpStrg(CurP^.Owner^) = UpStrg(P^.Owner^)) then
           begin Pos := I; break; end;
      end;
    end;
  end else Pos := 0;
 Dispose(FC,Done);
 ScrollBar^.SetValue(Pos);
 DecDrawDisabled;
 DirectoryName := NewDir;
 SetDirName;
 if GetState(sfFocused) and (Drive^.DriveType = dtDisk) then
   GlobalMessage(evCommand, cmRereadInfo, nil);
 SendLocated;
end;
        {-DataCompBoy-}

procedure TFilePanelRoot.SetDirName;
begin
{ if (ActivePanel = @Self) then
  begin Drive^.lChDir(DirectoryName); if TypeOf(Drive^) = TypeOf(TDrive) then CurrentDirectory := DirectoryName; end;}
 DrawView;
 if InfoView <> nil then InfoView^.DrawView;
 if DirView <> nil then DirView^.DrawView;
end;

procedure TFilePanelRoot.ChDirName;
begin
 SetDirName;
end;

        {-DataCompBoy-}
procedure TFilePanelRoot.ReadDirectory;
begin
 LineLength := Drive^.CalcLength;
 Drive^.Owner := @Self;
 case Drive^.DriveType of
   dtArc: HelpCtx := hcArchives;
   dtLink: HelpCtx := hcLinkPanel;
    else HelpCtx := hcFilePanel;
 end;
 DOSError := 0;
 PosChanged := Off; Loaded := Off;
 Abort := Off; ClrIO;
 FreeSpace := ''; TotalInfo := '';
 if Files <> nil then Dispose(Files,Done);
 Files := PFilesCollection(Drive^.GetDirectory(SortMode, PanelFlags, FileMask, FreeSpace, TotalInfo));
 if Files = nil then Files := New(PFilesCollection, Init($10,$10));
 PFilesCollection(Files)^.Owner := @Self;
 SelNum := 0; SelectedLen := 0; PackedLen := 0;
 PFilesCollection(Files)^.SortMode := SortMode;
 if Abort then Exit;

 if DriveState and dsInvalid > 0 then Exit ;

 Drive^.SizeX := Size.X;
 if Drive^.DriveType = dtDisk
  then DirectoryName := Drive^.GetRealDir
  else DirectoryName := Drive^.GetDir;
 if GetState(sfSelected) then Message(Owner, evCommand, cmChangeTree, @DirectoryName);

 ScrollBar^.SetParams(ScrollBar^.Value, 0, Files^.Count - 1, (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0))
                                        * ((Size.X+1) div LineLength), 1);
 if (ActivePanel = @Self) and (Drive^.DriveType = dtDisk)
    then CurrentDirectory := DirectoryName
    else LFN.lChDir(CurrentDirectory);
 if Drive^.DriveType = dtDisk then Message(CommandLine, evCommand, cmRereadInfo, nil);
 Message(Owner, evCommand, cmRereadInfo, nil);
 if DriveLine <> nil then DriveLine^.DrawView;
 ChkNoMem;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TFilePanelRoot.GetUserParams;
  var PF: PFileRec;
      S: String[1];

 procedure MakeTempList;
  var T: lText;
      I: LongInt;
 begin
  lAssignText(T, List); ClrIO;
  lRewriteText(T); if Abort then Exit;
  if (SelNum=0) then begin
   PF := Files^.At(ScrollBar^.Value);
   WriteLn(T.T, MakeFileName(PF^.Name));
  end else
  for I := 1 to Files^.Count do begin
   PF := Files^.At(I-1);
   if (PF^.Selected) then WriteLn(T.T, MakeFileName(PF^.Name));
  end;
  Close(T.T);
 end;

begin
  FileRec := nil; List := '';
  if not GetState(sfVisible) or (Files = nil) or (Files^.Count = 0) then Exit;
  FileRec := Files^.At(ScrollBar^.Value);
  if BuildList then begin
   if GetState(sfSelected) then S := '$' else S := '';
   List := SwpDir+'$dn'+ItoS(DNNumber)+S+'.lst';
   MakeTempList;
  end else List := '';
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
PROCEDURE TFilePanelRoot.CommandHandle;
var
  PF: PFileRec;
  CurPos: LongInt;
  MPos: TPoint;
  LastRDelay: Word;

 procedure CE;begin ClearEvent(Event) end;
 procedure CED;begin ClearEvent(Event); DrawView end;

 procedure Rebound;
  var R: TRect;
 begin
  GetBounds(R);
  R.A.Y := 1; R.B.Y := Owner^.Size.Y - 1;
  ChangeBounds(R);
  Owner^.Redraw;
 end;

 procedure GotoSingle(Name: String);
  var A: LongInt;
 begin
  UpStr(Name);
  A := 0;
  While (A < Files^.Count) and
        (UpStrg(GetLFN(PFileRec(Files^.At(A))^.LFN)) <> Name) do Inc(A);
  if A >= Files^.Count then A := 0; IncDrawDisabled;
  OldDirectory := DirectoryName;
  ScrollBar^.SetValue(A);
  DecDrawDisabled;
  Rebound;
 end;

 function ChangeUp: Boolean;
  var S: String;
 begin
  if ChangeLocked then Exit;
  ChangeUp := Drive^.IsUp;
  DeltaX := 0;
  if Drive^.IsUp then Drive^.ChangeUp(S) else Exit;
  IncDrawDisabled; ReadDirectory;
  AddToDirectoryHistory(DirectoryName,integer(Drive^.DriveType));
  if S <> '' then GotoSingle(S);
  DecDrawDisabled;
  Rebound;
 end;

 procedure GotoFile(FileName: String);
  var Dr: String;
      Nm: String;
      Xt: String;
      Name: String;
      A: LongInt;
 begin
  if ChangeLocked then Exit;
  lFSplit(FileName, Dr, Nm, Xt);
  if Copy(Dr, 1, 5) = cLINK_ then  Delete(Dr, 1, 5);
  if (Dr[Length(Dr)] = '\') and ((Dr[2] <> ':') or (Dr[0] > #3)) then Dec(Dr[0]);
  if (Dr[2] = ':') and not(Drive^.DriveType in [dtDisk,dtLink,dtArc]) then
    begin
     Dispose(Drive,Done);
     New(Drive, Init(Byte(Upcase(Dr[1]))-64, @Self, Drive^.Param));
    end;
  DirectoryName := GetNormPath(Dr); IncDrawDisabled; OldDirectory := Dr;
  DeltaX := 0;
  Drive^.lChDir(DirectoryName);
  Drive^.SizeX := Size.X;
  DirectoryName := Drive^.GetDir;
  OldDelta := -1; Delta := 0;
  ReadDirectory;
  AddToDirectoryHistory(DirectoryName,integer(Drive^.DriveType));
  DecDrawDisabled;
  if (Nm = '') and (Xt='') then Name := '..'
                           else Name := Nm+Xt;
   GotoSingle(Name);
 end;

 function  isSeldir : boolean ;
   begin
      isSelDir := False ;
      if Files^.Count = 0 then Exit;
      if CurPos >= Files^.Count then Exit;
      isSelDir := PFileRec(Files^.At(CurPos))^.Attr and Directory <> 0;
   end;

 procedure ViewFile(Command: Word);
  var S: String;
      P: PFileRec;
 begin
  if Files^.Count = 0 then Exit;
  CE; if CurPos >= Files^.Count then Exit;
  P := Files^.At(CurPos);
  if P^.Attr and Directory <> 0 then Exit;
  {$IFNDEF BIT_32}
  asm
   mov ax,Command
   cmp ax,cmIntEditFile
   jnz @@1
   mov ax,cmIntFileEdit;
   jmp @@0
@@1:
   cmp ax,cmIntViewFile
   jnz @@2
   mov ax,cmIntFileView;
   jmp @@0
@@2:
   cmp ax,cmEditFile
   jnz @@3
   mov ax,cmFileEdit;
   jmp @@0
@@3:
   mov ax,cmFileView;
@@0:
   mov Command, ax
  end;
  {$ELSE BIT_32}{&Frame-}
  asm
   mov eax,Command
   cmp eax,cmIntEditFile
   jnz @@1
   mov eax,cmIntFileEdit;
   jmp @@0
@@1:
   cmp eax,cmIntViewFile
   jnz @@2
   mov eax,cmIntFileView;
   jmp @@0
@@2:
   cmp eax,cmEditFile
   jnz @@3
   mov eax,cmFileEdit;
   jmp @@0
@@3:
   mov eax,cmFileView;
@@0:
   mov Command, eax
  end;
  {$ENDIF}
  Drive^.UseFile(P, Command);
 end;

 procedure Recount;
  var I: LongInt;
      PF: PFileRec;
 begin
  SelectedLen := 0; PackedLen := 0; SelNum := 0;
  for I := 1 to Files^.Count do
   begin
    PF := Files^.At(I-1);
    if PF^.Selected then
    begin
      SelectedLen := SelectedLen + PF^.Size;
      PackedLen := PackedLen + PF^.PSize;
      Inc(SelNum);
    end;
   end;
   DrawView;
   if InfoView <> nil then InfoView^.DrawView;
 end;

 procedure Setup;
  var R: record Sort,Show: Integer; S: String; end;
      BB: TRect;
 begin
  R.Sort := SortMode;
  if R.Sort = 0 then R.Sort := 7 else
    if R.Sort = 30 then R.Sort := 6 else Dec(R.Sort);
  R.Show := PanelFlags;
  R.S := FileMask;
  if ExecResource(dlgPanelSetup, R) <> cmOK then Exit;
  if R.Sort = 7 then R.Sort := 0 else
   if R.Sort = 6 then R.Sort := 30 else Inc(R.Sort);

  if (ActivePanel = @Self) and (Drive^.DriveType = dtDisk)
    then CurrentDirectory := DirectoryName;
  SortMode := R.Sort;
  PanelFlags := R.Show;
  FileMask := R.S; RereadDir;
  Rebound;
  ChDirName;
 end;

 procedure CountLen;

  procedure DoCount(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  begin
    if (P^.Attr and Directory <> 0) and (P^.Selected) and (not Abort) then Drive^.GetDirLength(P);
  end;

 begin
  CE; if (PF = nil) or ((PF^.Attr and Directory = 0) and (SelNum = 0)) then Exit;
  if PF^.Attr and Directory <> 0 then Drive^.GetDirLength(PF);
  if SelNum > 0 then Files^.ForEach(@DoCount); Abort := False;
  Recount;
  DrawView;
  if InfoView <> nil then InfoView^.DrawView;
 end;

 var WasFull: Boolean;

 procedure InsertDrive;
 begin
  if ChangeLocked then Exit;
  if (PDrive(Event.InfoPtr)^.DriveType = dtTemp) and
     (Drive^.DriveType = dtTemp) then begin CE; Exit end;
  if PDrive(Event.InfoPtr)^.DriveType in [dtDisk, dtLink] then
      begin if Drive <> nil then Dispose(Drive,Done); Drive:=nil end;
  PDrive(Event.InfoPtr)^.Prev := Drive;
  Drive := Event.InfoPtr;
  if (Drive^.DriveType = dtArc) or (Drive^.DriveType = dtArvid) then Drive^.lChDir(#0);
  {if WasFull then Drive^.Flags := Drive^.GetFullFlags else Drive^.Flags := 0;}
  Drive^.Owner := @Self;
  CE;
  DeltaX := 0;
  ReadDirectory;
  AddToDirectoryHistory(DirectoryName,integer(Drive^.DriveType));
  ScrollBar^.SetValue(0);
  Rebound;
 end;

 procedure EraseGroup;
 begin
   Drive^.EraseFiles(Event.InfoPtr);
   CE;
 end;

 procedure SelecType(S: Boolean);
  procedure Sel(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  begin
   if (P^.TType = PF^.TType) and (P^.Name[1] <> '.') then P^.Selected := S;
  end;
 procedure SelAll(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
 begin
  P^.Selected:=S;
 end;
 begin
  if (CurPos >= Files^.Count) {or (PanelFlags and fmiHiliteFiles = 0)} then Exit;
  PF := Files^.At(CurPos);
  if PF^.Name[1] <> '.' then
    Files^.ForEach(@Sel) else
  begin
    Files^.ForEach(@SelAll);
    PF^.Selected:=False;
  end;
  Recount; Owner^.Redraw;
 end;

 procedure SelectExt(S: Boolean; G1, G2: LongInt; Invert: Boolean);
  var SS: String;
      a: boolean;

  procedure Sel(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  begin
   if a then
    begin
     if ((SS = UpStrg(GetExt (GetLFN(P^.LFN)))) xor Invert) and (P^.Name[1] <> '.') then P^.Selected := S;
    end else
    begin
     if ((SS = UpStrg(GetSName(GetLFN(P^.LFN)))) xor Invert) and (P^.Name[1] <> '.') then P^.Selected := S;
    end
  end;

 begin
  if (CurPos >= Files^.Count) then Exit;
  PF := Files^.At(CurPos);
  if g1=10 then a:=true else a:=false;
  if a then SS := UpStrg(GetExt  (GetLFN(PF^.LFN)))
       else SS := UpStrg(GetSName(GetLFN(PF^.LFN)));
  Files^.ForEach(@Sel);
  Recount; Owner^.Redraw;
 end;

 procedure HandleCommand;
  var FC: PCollection;
      W: Word;
 begin
   W := Event.Command; CE; FC := nil;
   case W of
    cmExtractTo, cmArcTest: begin FC := GetSelection(@Self, Off); if FC = nil then Exit; end;
   end;
   Drive^.HandleCommand(W, FC);
   if FC <> nil then
    begin
      FC^.DeleteAll;
      Dispose(FC,Done);
    end;
 end;

 function ReplaceDrive(C: char): Boolean;
 var
   PDr: PDrive;
 begin
    ReplaceDrive := Off;
    New(PDr, Init(Byte(C)-64, @Self, Drive^.Param));
    if Abort then
     begin Dispose(PDr,Done); Exit; end;
    Dispose(Drive,Done);
    Drive := PDr;
    DeltaX := 0;
    {if WasFull then Drive^.Flags := Drive^.GetFullFlags
               else Drive^.Flags := 0;}
    Drive^.SizeX := Size.X;
    DirectoryName := Drive^.GetDir;
    if GetState(sfActive+sfSelected) then
       SetState(sfActive+sfSelected, On);
    Rebound;
    ReplaceDrive := On;
 end;

 function MakeFName(Idx: LongInt; Mode: Boolean; Sq: Boolean): String;
 begin
   PF := Files^.At(Idx);
   if (ShiftState and kbAltShift <> 0)
    then FreeStr := GetLFN(PF^.LFN)
    else FreeStr := MakeFileName(PF^.Name);
   if PosChar('.', FreeStr) = 0 then AddStr(FreeStr, '.');
   if (Drive^.DriveType = dtDisk) then
    If (ShiftState and (kbLeftShift+kbRightShift) <> 0) then
     if (ShiftState and kbAltShift <> 0)
      then FreeStr := MakeNormName(lfGetLongFileName(PF^.Owner^), FreeStr)
      else FreeStr := MakeNormName(lfGetShortFileName(PF^.Owner^), FreeStr)
    else
   else FreeStr := MakeNormName(PF^.Owner^, FreeStr);
   if Sq then
    if Mode
     then MakeFName := ' '+SquashesName(FreeStr)
     else MakeFName := SquashesName(FreeStr)
   else
    if Mode
     then MakeFName := ' '+FreeStr
     else MakeFName := FreeStr;
 end;

 procedure DoChange(SS: String);
   var SR: lSearchRec;
 begin
   ClrIO;
   lFindFirst(SS, $3f xor VolumeID, SR);
   lFindClose(SR);
   if Abort then Exit;
   if ((DOSError <> 0) or (SR.SR.Attr and Directory = 0)) and
      (Copy(SS,Length(SS)-1,2) <> ':\') then
     Exit;
   if SS = '' then Exit;
   if Drive^.DriveType <> dtDisk then
      begin
        if not ReplaceDrive(SS[1]) then Exit;
      end;
   SS := MakeNormName(SS,'.'); OldDelta := -1;
   GotoFile(SS);
   Rebound;
 end;
(*DataCompBoy
 procedure DoExcept(P: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
 begin
   if (P <> nil) and (P^.Attr and Directory = 0) then
    if not InFilter(GetLFN(P^.LFN), FileMask) then P^.Size := -100;
 end;
*)
 procedure _GetFName;
 var
   S: string;
   dum: PFileRec;
 begin
   GetUserParams(dum, PString(Event.InfoPtr)^, False);
   S := MakeFileName(PF^.Name);
   PString(Event.InfoPtr)^ := S + ' ' + CnvString(Event.InfoPtr);
 end;

 procedure _DoPush(a: Boolean);
 var
   S: string;
   B,C: Boolean;
 begin
   if not GetState(sfVisible) then Exit;
   if a and (Drive^.DriveType<>dtDisk) then PString(Event.InfoPtr)^ := CurrentDirectory
   else
   begin
     S := Drive^.GetRealName;
     if (S <> '') and (S <> CurrentDirectory) then PString(Event.InfoPtr)^ := S;
   end;
 end;
        {-DataCompBoy-}
 procedure _DoRereadDir;
 var
   S: string;
   I: LongInt;
 begin
   S := UpStrg(CnvString(Event.InfoPtr));
   if (S[0] > #3) and (S[Length(S)] = '\') then Dec(S[0]);
   I := Length(S);
   if (Drive^.DriveType <> dtDisk) or (S = Copy(UpStrg(lfGetLongFileName(DirectoryName)), 1, Length(s)) ) then
       begin
        Drive^.RereadDirectory(S);
        RereadDir;
       end
   else
        if (Drive^.DriveType = dtDisk) and (UpStrg(S[1]) = UpStrg(DirectoryName[1])) then
         begin
           if PanelFlags and fmiFree <> 0 then
              Drive^.GetFreeSpace(FreeSpace);
           if InfoView <> nil then InfoView^.DrawView;
         end;
 end;
        {-DataCompBoy-}

 procedure _PushName;
 var
   S: string; ps:PString;
 begin
   S := Drive^.GetRealName;
   if S <> '' then HistoryAdd(Event.InfoByte, S);
 end;

 procedure _ChangeDrive;
 var
   S: string;
 begin
   if ChangeLocked then Exit;
   CE;
   Abort := Off; OldDelta := -1;
   MPos.X := Origin.X + Size.X div 2;
   if DriveSelectVCenter then                {-$X-Man}
   MPos.Y := Origin.Y + Size.Y div 2 {-$VIV} {-$X-Man}
   else MPos.Y := Origin.Y+1;                {-$X-Man}
   Owner^.MakeGlobal(MPos, MPos);
   if (DirectoryName[2] = ':') then S[1] := DirectoryName[1]
       else if (Drive^.DriveType = dtLink) then S[1] := '+'
            else S[1] := CurrentDirectory[1];
   S := SelectDrive(MPos.X, MPos.Y, S[1], On);
   if S = cTEMP_ then
   begin
     Event.InfoPtr := New(PTempDrive, Init(Drive^.Param));
     InsertDrive; Exit;
   end else
   {$IFDEF MODEM}
   {$IFDEF LINK}
   if S[1] = '+' then
   begin
     Event.InfoPtr := NewLinkDrive(s[2],@Self, Drive^.Param);
     InsertDrive; Exit;
   end else
   {$ENDIF}
   {$ENDIF}
   if S <> '' then
    begin
     ClrIO;
     Abort:=False;
     if Drive^.DriveType <> dtDisk then
      begin
       if not ReplaceDrive(S[1]) then Exit;
      end else
      begin
        Drive^.lChDir(S);
        if Abort then
         begin  ClrIO; SetDirName; LFN.lChDir(CurrentDirectory); Exit; end;
        Drive^.SizeX := Size.X;
        S := Drive^.GetDir;
      end;
     IncDrawDisabled; DirectoryName := S; ReadDirectory;
     AddToDirectoryHistory(DirectoryName,integer(Drive^.DriveType));
     DecDrawDisabled; SetDirName;
    end;
   ChDirName;
 end;

 procedure _ChangeDrv;
 var
   S: string;
 begin
   if ChangeLocked then Exit;
   S := CnvString(Event.InfoPtr); CE;
   if S = cTEMP_ then
   begin
    Event.InfoPtr := New(PTempDrive, Init(Drive^.Param));
    InsertDrive; Exit;
   end
   {$IFDEF MODEM}
   {$IFDEF LINK}
   else if S[1] = '+' then
   begin
     Event.InfoPtr := NewLinkDrive(S[2],@Self, Drive^.Param);
     InsertDrive; Exit;
   end
   {$ENDIF};
   {$ENDIF};
   lGetDir(Byte(S[1])-64, S);
   S:=GetNormPath(S);
   If (S[Length(S)]='\') and (S[Length(S)-1]<>':') then Dec(S[0]);
   DoChange(S);
 end;

 procedure _DoDirHistory;
 var
   S: string;
   Q: string;
   Dr: PDrive;
 begin
   CE;
   S := DirHistoryMenu;
   if S <> '' then
   begin
     if Drive^.DriveType <> dtDisk then ReplaceDrive(S[1]);
     FreeByte:=PosChar(':', Copy(S, 3, 255))+2;
     if FreeByte>2 then begin
      Q:=Copy(S, 1, FreeByte-1);
      if ((Drive^.DriveType = dtArc) and
         (UpStrg(PArcDrive(Drive)^.ArcName)=UpStrg(Q)))
         or
         ((Drive^.DriveType = dtArvid) and
         (UpStrg(PArvidDrive(Drive)^.Name^)=UpStrg(Q))) then begin
       Q:=Copy(S, FreeByte+1, 255);
       Message(@Self, evCommand, cmChangeDirectory, @Q)
      end else begin
       Dr:=nil;
       Dr:=New(PArcDrive,Init(S, Drive^.innum));
       if Dr=nil then Dr:=New(PArvidDrive,Init(S, Drive^.innum));
       if Dr=nil then exit;
       Message(@Self, evCommand, cmInsertDrive, Dr);
      end
     end else
     if S[Length(S)]='\'
      then Message(@Self, evCommand, cmChangeDirectory, @S)
      else begin
            FreeByte := Drive^.innum;
            Message(@Self, evCommand, cmInsertDrive, New(PFindDrive,InitList(S)))
           end;
   end;
 end;

 procedure _CloseLinked;
 var
   S: string;
 begin
   if Drive^.DriveType = dtLink then
   begin
     lGetDir(0, S);
     ReplaceDrive(S[1]);
     RereadDir;
   end;
 end;

 procedure _DoCtrl;
 var
   S: string;
 begin
   if ((ShiftState and 3 <> 0) or (CmdLine.Str = ''))
      and (FMSetup.Show and fmsDriveLine <> 0) then
   begin
     if (DirectoryName = cTEMP_) then DirectoryName[1] := '*';
     if Drive^.DriveType = dtArc then
       begin
          DirectoryName[1] := PArcDrive(Drive)^.ArcName[1];
       end;
     DirectoryName[2] := ':';
     repeat
       if Event.What = evKeyDown then
        begin
          case Event.KeyCode of
            kbCtrlLeft: repeat
                          if DirectoryName[1] > #65 then Dec(DirectoryName[1])
                           else if DirectoryName[1] = #65 then DirectoryName[1] := '*'
                                                    else DirectoryName[1] := 'Z';
                         until ValidDrive(DirectoryName[1]) or (DirectoryName[1] = '*');
            kbCtrlRight: repeat
                           if DirectoryName[1] in ['A'..'Y'] then INC(DirectoryName[1])
                            else if DirectoryName[1] = 'Z' then DirectoryName[1] := '*'
                                                     else DirectoryName[1] := 'A';
                          until ValidDrive(DirectoryName[1]) or (DirectoryName[1] = '*');
          end;
          DriveLine^.DrawView;
        end;
       GetEvent(Event);
     until ShiftState and kbCtrlShift = 0;
     if DirectoryName[1] = '*' then S := cTEMP_
                               else S := DirectoryName[1]+':';
     DirectoryName := S;
     Message(@Self, evCommand, cmChangeDrv, @S);
     Drive^.SizeX := Size.X;
     DirectoryName := Drive^.GetDir;
     DriveLine^.DrawView;
     Abort := Off;
     DriveLine^.DrawView;
     CE;
   end;
 end;

 procedure _CtrlIns;
 var
   S: string;
   I: LongInt;
   b: boolean;
 begin
   S := ''; CE;
   b := false;
   if (Files = nil) or (Files^.Count <= 0) then Exit;
   if SelNum = 0
    then S := MakeFName(CurPos, Off, Off)
    else
     for I := 0 to Files^.Count-1 do
      if PFileRec(Files^.At(I))^.Selected then begin
       S := S + MakeFName(I, b, On);
       b:=true;
      end;
    Message(Application, evCommand, cmPutInClipboard, @S);
 end;

 function _AltEnter: boolean; forward;
 procedure _CtrlEnter;
 var
   S: string;
   PF: PFileRec;
 begin
   CE;
   if Files^.Count = 0 then Exit;
   PF := Files^.At(CurPos);
   if (ShiftState and
      (kbAltShift+kbCtrlShift+kbLeftShift+kbRightShift)=kbAltShift+kbCtrlShift
      ) and OS2exec then begin _AltEnter; exit end;
   if (PF <> nil) then
   begin
    if (ShiftState and kbAltShift <> 0)
     then S:= GetLFN(PF^.LFN)
     else S:= MakeFileName(PF^.Name);
    if (PF^.Attr and Directory = 0) and
       (PosChar('.', S) = 0) then AddStr(S, '.');
    if Copy(S, 1, 2) = '..'  then
     if (ShiftState and kbAltShift <> 0)
      then S := lfGetLongFileName(PF^.Owner^)+'\'
      else S := lfGetShortFileName(PF^.Owner^)+'\'
    else
     if ShiftState and 3 <> 0 then
      if (ShiftState and kbAltShift <> 0)
       then S := MakeNormName(lfGetLongFileName(PF^.Owner^), S)
       else S := MakeNormName(lfGetShortFileName(PF^.Owner^), S);
    S:=SquashesName(S);
    Message(CommandLine, evCommand, cmInsertName, @S);
   end;
 end;

 function _AltEnter: Boolean;
 var
   S: string;
 begin
   if OS2exec and (Drive^.DriveType = dtDisk) and
      InExtFilter(MakeFileName(PF^.Name), Executables) and
      (ShiftState and (kbCtrlShift or 3) <> 0) then
   begin
     PShootState := ShiftState;
     S := MakeFileName(PF^.Name);
     if ShiftState and kbCtrlShift = 0 then S := '>'+S
                                         else S := '<'+S;
     Message(Application, evCommand, cmExecString, @S);
     CE;
     _AltEnter := True;
   end else
   begin
     PShootState := ShiftState;
     CE;
     _AltEnter := False;
   end;
 end;

 procedure _GotoExt;
 var
   S: string;
 begin
   if (Drive^.DriveType <> dtArvid) and (Drive^.DriveType <> dtArc) then
   begin
      if Drive^.DriveType = dtDisk then S := MakeFileName(PF^.Name)
        else
             begin CE; Exit; end;
      if OS2exec and (Copy(PF^.Name,10,3) = 'cmd') then
       begin
          S := 'call "'+S+'"';
          if ShiftState and 3 <> 0 then S := '<'+S else S := '>'+S;
          Message(Application, evCommand, cmExecString, @S);
          Exit;
       end;
      Message(Application, evCommand, cmExecFile, @S);
   end else ViewFile(cmViewFile)
   {else Message(Application, evCommand, cmExecCommandLine, nil);}
 end;

 procedure _CtrlPgUp;
 begin
   CE;
   if ChangeLocked then Exit;
   if not ChangeUp then
     if Drive^.DriveType in [dtDisk, dtLink] then GotoFile(DirectoryName)
 end;

 procedure _CtrlPgDn;
 var
   S: string;
 begin
   CE; if ChangeLocked then Exit;
   if (Files^.Count = 0) then Exit;
   PF := Files^.At(CurPos);
   if PF^.Name[1] = '.' then
   begin
     _CtrlPgUp;
     Exit;
   end;
   if not ( Drive^.DriveType in [ dtFind, dtTemp, dtList ] ) then
    begin
      if PF^.Attr and Directory = 0 then Exit;
      S := MakeNormName(MakeNormName(PF^.Owner^,GetLFN(PF^.LFN)),'.');
      GotoFile(S);
    end else
   if Drive^.DriveType <= dtList then
    begin
      S := MakeNormName(PF^.Owner^, GetLFN(PF^.LFN));
      if ShiftState and 3 <> 0 then
        begin
          Message(Owner, evCommand, cmChangeInactive, @S);
          Exit;
        end;
      Dispose(Drive,Done);
      New(Drive, Init(Byte(S[1])-64, @Self, Drive^.Param));
      {Drive^.Owner := @Self;}
      DeltaX := 0;
      {if WasFull then Drive^.Flags := Drive^.GetFullFlags
                 else Drive^.Flags := 0;}
      GotoFile(S);
      Rebound;
    end;
 end;

 procedure FindNextSelFile(pNext: Boolean); {-$VIV,VOL 19.05.99---}
 var I: LongInt;

    function CheckIt(I: LongInt): Boolean;
    begin
      CheckIt := False;
      if PFileRec(Files^.At(I))^.Selected then
      begin
        ScrollBar^.SetValue( I );
        CED;
        CheckIt := True;
      end;
    end;

 begin
   if Files = nil then Exit;
   if pNext then begin for I := CurPos + 1 to Files^.Count - 1 do if CheckIt(I) then Break; end
     else begin for I := CurPos - 1 downto 0 do if CheckIt(I) then Break; end;
 end;

 procedure _CheckKB;
 var
   I: LongInt;
   aaa: word;
   bbb, ccc: byte;
   l: longint;
   FC: PFilesCollection;
   s, s2: string;
   line: longint;
 begin
   case Event.KeyCode of
    kbAlt1..kbAlt9: begin
                      I := (Event.KeyCode shr 8)-(kbAlt1 shr 8);
                      CE;
                      if ShiftState and 7 <> 0 then
                        begin
                          if Drive^.DriveType <> dtDisk then Exit;
                          if Msg( dlPromptForQDir, nil, mfQuery+mfYesButton+mfNoButton) <> cmYes then Exit;
                          DisposeStr(DirsToChange[I]);
                          DirsToChange[I] := NewStr(DirectoryName);
                          Message(Application, evCommand, cmUpdateConfig, nil);
                        end else begin
                      if DirsToChange[I] <> nil then
                           DoChange(CnvString(DirsToChange[I]));
                      end;
                    end;
    kbCtrl1..kbCtrl0: Begin
                       Drive^.GetParam((Event.KeyCode shr 8)-(kbCtrl1 shr 8)+1);
                       Owner^.ReDraw;
                      End;
    kbAltIns: if ShiftState and kbCtrlShift <>0 then _CtrlIns else
              begin
               if (Drive^.Flags and psShowDescript = 0) and (PF^.DIZ = nil) then
                begin
                 if (Drive^.DriveType = dtDisk) then
                 begin
                  s2:='';
                  i:=1;
                  PF:=Files^.At(CurPos);
                  repeat
                   s:=GetPossibleDizOwner(i);
                   Inc(i);
                   if s<>'' then S2:=GetDIZ(s, Line, MakeFileName(PF^.Name), GetLFN(PF^.LFN));
                  until (s='') or (s2<>'');
                  if s2<>'' then begin
                   New(PF^.DIZ);
                   PF^.DIZ^.Owner:=NewStr(S);
                   PF^.DIZ^.DIZ:=NewStr(S2);
                   PF^.DIZ^.Line:= Line;
                   PF^.DIZ^.isDisposable:=True;
                  end;
                  Drive^.EditDescription(PF);
                 end else
                 if (Drive^.DriveType = dtArvid) then
                  begin
                   IncDrawDisabled;
                   Drive^.Flags:=Drive^.Flags or psShowDescript;
                   FC:=Files;
                   Files:=PFilesCollection(Drive^.GetDirectory(SortMode,
                           PanelFlags, FileMask, FreeSpace, TotalInfo));
                   if Files = nil then RunError(152);
                   PFilesCollection(Files)^.Owner:=@Self;
                   PFilesCollection(Files)^.SortMode:=SortMode;
                   PF:=Files^.At(CurPos);
                   Drive^.EditDescription(PF);
                   Dispose(Files,Done);
                   Drive^.Flags:=Drive^.Flags and not psShowDescript;
                   Files:=PFilesCollection(Drive^.GetDirectory(SortMode,
                          PanelFlags, FileMask, FreeSpace, TotalInfo));
                   for I:=0 to Files^.Count-1 do
                    PFileRec(Files^.At(I))^.Selected:=
                     PFileRec(FC^.At(I))^.Selected;
                   Dispose(FC,Done);
                   DecDrawDisabled;
                   DrawView;
                  end;
                 end else
                 Drive^.EditDescription(PF);
                CE;
              end;
    kbBack: if (FMSetup.Options and fmoBackGoesBack <> 0) and
               ((ShiftState and 3 <> 0) or ((CmdLine.Str = '') and
                (not CmdLine.StrCleared) and (not QuickSearch))) then
               begin
                 _CtrlPgUp;
                 Exit;
               end;
    kbUp, kbDown, kbUpUp, kbDownUp: begin
                     CtrlWas := ShiftState and kbCtrlShift <> 0;
                   end;
    kbCtrlR: begin Message(@Self, evCommand, cmForceRescan, nil); CE end;
    kbGrayAst, kbCtrlGAst: begin InvertSelection(@Self, Event.KeyCode = kbCtrlGAst); CE end;
    kbAltGPlus, kbAltGMinus: begin SelectExt(Event.KeyCode = kbAltGPlus, 1, 8,
                                             ShiftState and 3 <> 0); CE end;
    kbCtrlGPlus, kbCtrlGMinus:
       begin
         if ShiftState and 3 = 0 then SelecType(Event.KeyCode = kbCtrlGPlus)
                                   else SelectExt(Event.KeyCode = kbCtrlGPlus, 10, 3, Off); CE
       end;
    kbCtrlBSlash:begin
                 Drive^.ChangeRoot;
                 ReadDirectory;
                 AddToDirectoryHistory(DirectoryName,integer(Drive^.DriveType));
                 ScrollBar^.SetValue(0);
                 DecDrawDisabled;
                 if Drive^.DriveType = dtArc then {-$VOL}
                 begin _CtrlPgUp; Exit; end;{-$VOL}
                 Owner^.Redraw; CE end;
kbCtrlRight, kbCtrlLeft: _DoCtrl;
kbCtrlDel: if Files^.Count > 0 then
            begin
              I := 0;
              if SelNum = 0 then Files^.AtFree(CurPos) else
                while I < Files^.Count do
                 if PFileRec(Files^.At(I))^.Selected then
                  begin
                    Files^.AtFree(I);
                    if I < CurPos then Dec(CurPos);
                  end else Inc(I);
              ScrollBar^.SetParams(CurPos, 0, Files^.Count - 1,
                                 (Size.Y-Byte(FMSetup.Show and fmsColumnTitles<> 0)) *
                                 ((Size.X+1) div LineLength), 1);
              Recount;
              CE;
            end;
kbCtrlIns,kbShiftIns: if ShiftState and kbCtrlShift<>0 then _CtrlIns;
kbCtrlEnter: _CtrlEnter;
    kbLeft: if (Size.X > LineLength) then
             if (Delta>0) or
                (ScrollBar^.Value >
                 Size.Y - Byte(FMSetup.Show and fmsColumnTitles<>0)) then begin
              if ScrollBar^.Value - Delta < Size.Y - Byte(FMSetup.Show and fmsColumnTitles<>0)
               then Dec(Delta, Size.Y - Byte(FMSetup.Show and fmsColumnTitles<>0));
              ScrollBar^.SetValue(ScrollBar^.Value - Size.Y + Byte(FMSetup.Show and fmsColumnTitles<> 0));
              CE;
             end else begin
              if ScrollBar^.Value > 0 then
              ScrollBar^.SetValue(0);
              CE;
             end
            else if DeltaX > 0 then begin
                  Dec(DeltaX);
                  DrawView;
                  if InfoView <> nil then InfoView^.DrawView;
                  CE;
                 end;
    kbRight: if Size.X > LineLength then begin
              aaa := Delta;
              Owner^.Lock;
              ScrollBar^.SetValue(ScrollBar^.Value + Size.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0));
              if aaa<>delta then
               Delta := aaa + Size.Y -
                        Byte(FMSetup.Show and fmsColumnTitles<>0);
               CED;
               Owner^.Unlock;
              end else if DeltaX < LineLength - Size.X - 1 then begin
                        Inc(DeltaX);
                        DrawView;
                        if InfoView <> nil then InfoView^.DrawView;
                        CE;
                       end;
    kbHome: begin CE; DeltaX := 0; OldDelta := -1; ScrollBar^.SetValue(0); Owner^.ReDraw end;
    kbEnd:  begin CE; OldDelta := -1; ScrollBar^.SetValue(Files^.Count-1) end;
    kbAltEnter: if _AltEnter then Exit else
    begin
      _GotoExt;
      Exit;
    end;
    kbEnter: if CurPos < Files^.Count then
             begin
               PShootState := ShiftState;
               PF := Files^.At(CurPos);
               if ChangeLocked or (Message(Application, evCommand, cmExecCommandLine, nil) <> nil) then
               begin CE; Exit; end;
               CE;
               FreeStr := 'AUTOEXEC BAT'; FreeStr[9] := PF^.Name[9];
               if (Drive^.DriveType = dtDisk) and
                  (PF^.Attr and Directory = 0) and {-$VOL}
                  (UpStrg(PF^.Name) = FreeStr) and
                  (Msg(dlAutoexecWarning,nil,mfYesNoConfirm) <> cmYes) then Exit;
               if (Drive^.DriveType in [dtFind, dtTemp, dtList]) then
               begin
                if PF^.Name[1] = '.' then _CtrlPgUp else _CtrlPgDn;
                Exit;
               end;
               if (FMSetup.Options and fmoEnterArchives <> 0) and
                  (Event.KeyCode = kbEnter) and (ShiftState and 3 = 0)
                  and (PF^.Attr and Directory = 0) and (Drive^.DriveType <> dtArc )
                  and (Drive^.DriveType <> dtFind ) and (Drive^.DriveType <> dtArvid )
                  and (ArcViewer(MakeNormName(PF^.Owner^, GetLFN(PF^.LFN)), Drive^.Param))
                  then begin CE; Exit; end;
               if (PF <> nil) then
               if (PF^.Attr and Directory <> 0) then
               begin
                if PF^.Name[1] = '.' then
                    begin
                     if not ChangeUp then GotoFile(PF^.Owner^);
                     end
                   else begin
                         PF^.Name := MakeFileName(PF^.Name);
                         GotoFile(MakeNormName(MakeNormName(PF^.Owner^,GetLFN(PF^.LFN)),'.'));
                        end;
                Exit;
               end else
               _GotoExt;
             end;
    kbCtrlPgUp: _CtrlPgUp;
    kbCtrlPgDn: _CtrlPgDn;
    kbDel: if ((CmdLine.Str = '') and (FMSetup.Options and fmoDelErase <> 0)) then
              Message(@Self, evCommand, cmPanelErase, nil);
    {
    kbShiftDel: if ((CmdLine.Str = '') and (FMSetup.Options and fmoDelErase <> 0)) then
              Message(@Self, evCommand, cmSingleDel, nil);
    }
    kbIns,kbSpace: if CurPos < Files^.Count then
           begin
            QuickSearch := Off;
            if (Event.CharCode = ' ') and ((CmdLine.Str <> '') or
               (FMSetup.Options and fmoSpaceToggle = 0)) then Exit;
            CE; if Files^.Count = 0 then Exit;
            PF := Files^.At(CurPos);
            if PF^.Name[1] <> '.' then
            begin
             PF^.Selected := not PF^.Selected;
             SelectedLen := SelectedLen - (1-2*Integer(PF^.Selected))*PF^.Size;
             PackedLen := PackedLen - (1-2*Integer(PF^.Selected))*PF^.PSize;
             Dec(SelNum, 1-2*Integer(PF^.Selected));
            end;
            ScrollBar^.SetValue(CurPos + 1);
            if CurPos = ScrollBar^.Value then DrawView;
            if InfoView <> nil then InfoView^.DrawView;
           end;
    kbGrayPlus: begin SelectFiles(@Self, On, ShiftState and 3 <> 0); CE end;
    kbGrayMinus: begin SelectFiles(@Self, Off, ShiftState and 3 <> 0); CE end;
    kbAltUp, kbAltDown: FindNextSelFile( Event.KeyCode = kbAltDown ); {-$VIV}
    else if ShowKeyCode and 1>0 then
          begin
           l:=Event.KeyCode;
           MessageBox('%x',@l,0);
          end;
   end;
 end;

procedure _GetCurrentPosFiles;
var
  S: string;
begin
  if (Drive^.DriveType < dtArc) and (PF <> nil) then
    S := MakeNormName(PF^.Owner^, MakeFileName(PF^.Name))
    else S := '';
  if ActivePanel = @Self then CurFileActive := NewStr(S) else
  begin
    CurFilePassive := NewStr(S);
    CE;
  end;
end;

 procedure _TagUntag;
 var
   I: LongInt;
 begin
   if (PF <> nil) and (PF^.Selected xor (Event.Command = cmSingleTag))
    then I := kbIns else I := kbDown;
   Message(Owner, evKeyDown, I, nil);CE;
 end;

 procedure _ForceRescan;
 var
   PDr: PDrive;
 begin
   if Drive^.DriveType = dtArc then
   begin
     if PArcDrive(Drive)^.ReadArchive then RereadDir else
     begin
       PDr := Drive;
       Drive := Drive^.Prev;
       PDr^.Prev := nil;
       Dispose(PDr,Done);
       DeltaX := 0;
       Drive^.SizeX := Size.X;
       DirectoryName := Drive^.GetDir;
       ReadDirectory;
       AddToDirectoryHistory(DirectoryName,integer(Drive^.DriveType));
       Rebound;
     end;
   end else RereadDir;
   CE;
 end;

 procedure _FindForced;
 var
   PDr: PDrive;
 begin
   ForceReading := Off;
   DeltaX := 0;
   PDr := Drive;
   Drive := Event.InfoPtr;
   Drive^.Prev := PDr^.Prev;
   PDr^.Prev := nil;
   Dispose(PDr,Done);
   RereadDir;
   CE;
 end;

 {$IFDEF UUDECODE}
 procedure CallUuDecode;
 var
   FC: PCollection;
 begin
   FC:=GetSelection(@Self, Off);
   if FC = nil then Exit;
   if FC^.Count = 0 then Exit;
   if (FC^.Count = 1) and
      (TFileRec(FC^.At(0)^).Attr and Directory <> 0)
   then Exit;
   UuDecode(FC);
   FC^.DeleteAll;
   Dispose(FC,Done);
 end;
 {$ENDIF}

var
  FC: PFilesCollection;
  I: LongInt;
  PSDEL: byte;
begin
 if Drive<>nil
  then WasFull := Drive^.GetFullFlags = Drive^.Flags
  else WasFull := false;
 if ScrollBar<>nil
  then CurPos := ScrollBar^.Value
  else CurPos := 0;
 if Files<>nil
  then if Files^.Count > CurPos
        then PF := Files^.At(CurPos)
        else PF := nil
  else PF := nil;
 case Event.What of
  evCommand: case Event.Command of
              cmToggleDescriptions: begin CM_ToggleDescriptions(@Self); CE end;
              cmToggleShowMode: begin CM_ToggleShowMode(@Self); CE end;
              cmFastRename: begin
                             if QuickRenameInDialog xor
                                (ShiftState and (kbLeftShift+kbRightShift)<>0)
                              then CM_RenameSingleDialog(@Self, @Event)
                             else
                              if Drive^.Flags and psShowLongName>0
                               then CM_RenameSingleL(@Self, @Event)
                               else CM_RenameSingleS(@Self, @Event);
                              CE
                            end;
              cmSingleTag, cmSingleUnTag : _TagUntag;
              cmQuickChange1..cmQuickChange9:
                               begin
                                 DoChange(CnvString(DirsToChange[Event.Command-cmQuickChange1]));
                                 CE;
                               end;
              cmPanelMakeList: begin
                                 Message(@Self, evCommand, cmInsertDrive,
                                        New(PFindDrive, InitList(MakeNormName(PF^.Owner^,MakeFileName(PF^.Name)))));
                                 CE;
                               end;
              cmPanelArcFiles:  begin CM_ArchiveFiles(@Self); CE end;
              cmExtractArchive: begin
                                   if (Drive^.DriveType <> dtArc) and
                                      (Drive^.DriveType <> dtArvid) and
                                      (PF <> nil) and
                                      (PF^.Attr and Directory = 0) then
                                     UnarchiveFiles(MakeNormName(PF^.Owner^, MakeFileName(PF^.Name)));
                                   CE
                                end;
              cmReboundPanel: Rebound;
{$IFDEF PRINTMANAGER}
              cmPrintFile: begin CM_Print(@Self); CE end;
{$ENDIF}
              cmSetPassword, cmExtractTo,
              cmArcTest, cmRereadForced,
              cmMakeForced
              : HandleCommand;
              cmEraseGroup: EraseGroup;
              cmFindTree: if (Drive^.DriveType = dtDisk) and
                             (PanelFlags and fmiDirLen <> 0) and
                             (Char(Event.InfoPtr^) = DirectoryName[1])
                              then Char(Event.InfoPtr^) := #0;
              cmInsertFile: if Files<>nil then begin
                             Files^.AtInsert(Files^.Count, Event.InfoPtr);
                             ScrollBar^.SetParams(ScrollBar^.Value, 0, Files^.Count - 1,
                                                  (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0)) *
                                                  ((Size.X+1) div LineLength), 1);
                             DrawView;
                             CE;
                            end;
              cmViewText, cmViewHex, cmViewDBF, cmViewWKZ:
               begin
                 if Files<>nil then
                  if Files^.Count >= CurPos then Drive^.UseFile(Files^.At(CurPos), 22000 + Event.Command);
                 CE;
               end;
              cmInsertDrive: if Event.InfoPtr <> nil then InsertDrive else CE;

              cmEditFile, cmIntViewFile, cmIntEditFile, cmViewFile:
                         begin
                           FreeByte:=Drive^.Param;
                           if isSelDir then
                             case Event.Command of
                              cmViewFile,cmIntViewFile :  if Drive^.DriveType <> dtArc then CountLen else CE;
                              cmEditFile,cmIntEditFile :  begin CM_SetAttributes(@Self, On, CurPos); CE end;
                             end { case }
                           else
                           begin
                             ViewFile(Event.Command); CE;
                           end;
                         end;
              cmCountLen: if Drive^.DriveType <> dtArc then CountLen else CE;
              cmSingleDel: CM_EraseFiles(@Self, On);
              cmGetFileName: begin _GetFName; CE end;
              cmTempCopyFiles: begin CM_CopyTemp(@Self); CE end;
              cmSortBy: begin CM_SortBy(@Self); CE end;
              cmPanelSetup: begin Setup; CE end;
              cmSetupColumns: begin CM_SetShowParms(@Self); CE end;
              cmTouchFile: begin
                            if (PF <> nil) then
                              if (PF^.Attr and Directory <> 0) then
                                  Message(@Self, evKeyDown, kbCtrlPgDn, nil) else
                                  if InExtFilter(MakeFileName(PF^.Name), Executables) then
                                      Message(@Self, evKeyDown, kbEnter, nil) else
                                      ViewFile(cmViewFile);
                            CE;
                           end;
              cmCompareDir: begin
                             Message(Owner, evCommand, cmPanelCompare, Files);
                             Recount; CE;
                            end;
              cmPushName: if UpStrg(CurrentDirectory) <> UpStrg(Drive^.GetRealName) then _PushName;
              cmPushFirstName: _DoPush(False);
              cmPushFullName: _DoPush(True);
              cmPanelCompare: begin CM_CompareDirs(@Self, Event.InfoPtr); Recount; CE end;
              cmPanelSelect: begin SelectFiles(@Self, On,Off); CE end;
              cmPanelUnselect: begin SelectFiles(@Self, Off,Off); CE end;
              cmPanelXSelect: begin SelectFiles(@Self, On,On); CE end;
              cmPanelXUnselect: begin SelectFiles(@Self, Off,On); CE end;
              cmCopyFiles,cmMoveFiles,cmSingleCopy,cmSingleRename:
                begin
                  CM_CopyFiles(@Self,
                    (Event.Command = cmMoveFiles) or
                     (Event.Command = cmSingleRename),
                    (Event.Command = cmSingleCopy) or
                     (Event.Command = cmSingleRename));
                  CE
                end;
              cmPanelInvertSel: begin InvertSelection(@Self, Off); CE end;
              cmPanelErase: begin CM_EraseFiles(@Self, Off); CE end;
              cmPanelMkDir: begin CM_MakeDir(@Self); CE end;
              cmRereadDir: if GetState(sfVisible) then _DoRereadDir;
              cmTotalReread: if GetState(sfVisible) or (Drive^.DriveType <> dtDisk) then
                                begin
                                  Drive^.RereadDirectory('');
                                  RereadDir;
                                end;
              cmForceRescan: _ForceRescan;
              cmPanelReread: begin RereadDir; CE end;
              cmMakeList: begin CM_MakeList(@Self); CE end;
              cmChangeDrive: _ChangeDrive;
              cmChangeDrv: _ChangeDrv;

              cmDirBranch: begin CM_Branch(@Self); CE end;
{$IFDEF UUDECODE}
              cmUUDecodeFile:
                begin
                  if Drive^.DriveType < dtArc then CallUuDecode;
                  CE;
                end;
{$ENDIF}
{$IFDEF UUENCODE}
              cmUUEncodeFile: begin
                          if (PF <> nil) and (PF^.Attr and Directory = 0)
                             and (Drive^.DriveType < dtArc) then
                              UUEncode(MakeNormName(PF^.Owner^, MakeFileName(PF^.Name)));
                          CE;
                        end;
{$ENDIF}
           cmChangeNameCase: if (PF <> nil) and (Drive^.DriveType < dtArc)
                              then CM_ChangeCase(@Self);
               cmUnpDiskImg: begin
                              if (Drive^.DriveType < dtArc) then
                               UnpackDiskImages(@Self,
                                                GetSelection(@Self, False));
                              CE;
                             end;
              cmChangeDirectory: begin ChDir(PString(Event.InfoPtr)^); CE; end;
              cmChangeDir: begin
                            FreeStr := CM_ChangeDirectory(@Self);
                            if FreeStr <> '' then GotoFile(MakeNormName(FreeStr,'.'));
                            CE
                           end;
              cmGetDirInfo: if Drive <> nil then
                              begin
                                Drive^.GetDirInfo(PDiskInfoRec(Event.InfoPtr)^);
                                CE;
                              end;
                  cmDirHistory: _DoDirHistory;
              cmCloseLinked: _CloseLinked;
              cmGetDirName: begin PString(Event.InfoPtr)^ := DirectoryName end;
              cmSetFAttr,cmSingleAttr: begin CM_SetAttributes(@Self, Event.Command=cmSingleAttr,CurPos); CE end;
              cmPanelLongCopy: begin CM_LongCopy(@Self); CE end;
              cmEnableView: begin ViewEnabled := On; SendLocated; end;
              cmDisableView: begin ViewEnabled := Off; end;
              cmLViewFile: begin SendLocated; CE end;
              cmFindFile: if Drive^.DriveType = dtArvid then
                            begin Drive^.HandleCommand(cmFindFile, nil); CE end;
              cmFindGotoFile: begin FileMask := x_x;
                                    GotoFile(PString(Event.InfoPtr)^); CE;
                                    Owner^.Select; Select;
                                    GlobalMessage(evCommand, cmRereadInfo, nil);
                              end;
              cmAdvFilter: begin CM_AdvancedFilter(@Self); CE end;
             end;
  evKeyDown: _CheckKB;
  evBroadcast: case Event.Command of
                 cmGetCurrentPosFiles: _GetCurrentPosFiles;
                 cmFindForced: if ForceReading then _FindForced;
                 cmInsertDrive: InsertDrive;
                 cmUnarchive: if Drive^.DriveType = dtArc then begin
                                Drive^.CopyFiles(PCopyRec(Event.InfoPtr)^.FC, @Self, On);
                                CE;
                              end;
                 cmCopyCollection: if Drive^.DriveType < dtArc then begin
                                Drive^.CopyFiles(Event.InfoPtr, @Self, ShiftState and 4 = 0);
                                CE;
                              end;
                 cmDropped: if MouseInView(PCopyRec(Event.InfoPtr)^.Where) then
                            begin
                              case Drive^.DriveType of
                                dtFind :;
                                else CM_Dropped(@Self, Event.InfoPtr);
                              end;
                              CE;
                            end;
                 cmScrollBarChanged: if ScrollBar = Event.InfoPtr then begin
                                      if MSelect then
                                        begin
                                         if Files<>nil then begin
                                          CE; if Files^.Count = 0 then Exit;
                                          PF := Files^.At(ScrollBar^.Value);
                                          if (PF^.Name[1] <> '.') and (PF^.Selected xor SelectFlag) then
                                           begin
                                            PF^.Selected := SelectFlag;
                                            if SelectFlag then begin Inc(SelNum); SelectedLen := SelectedLen + PF^.Size;
                                                                     PackedLen := PackedLen + PF^.PSize end
                                                          else begin Dec(SelNum); SelectedLen := SelectedLen - PF^.Size;
                                                                     PackedLen := PackedLen - PF^.PSize end;
                                           end;
                                         end;
                                        end;
                                      PosChanged := On;
                                      if InfoView <> nil then InfoView^.DrawView;
                                      CED; PosChanged := Off;
                                      if (RepeatDelay <> 0) and ViewEnabled then
                                        NeedLocated := GetSTime;
                                     end;
               end;
  evMouseDown: begin
                if Files^.Count = 0 then Exit;
                MSelect := Event.Buttons and mbRightButton <> 0;
                MakeLocal(Event.Where, MPos);
                if (Drive^.Flags and psShowLongName = 0) or ((MPos.X mod LineLength) <> Drive^.LFNLen) then
(*              if (Drive^.Flags and psShowLongName = 0) or ((MPos.X mod LineLength) <> Drive^.LFNLen-Drive^.EXTLen-1) then*)
                 begin
                  if (MPos.Y = 0) and (FMSetup.Show and fmsColumnTitles <> 0) then
                   begin
                    LastRDelay := RepeatDelay; RepeatDelay := 0;
                    if MSelect then
                     begin
                      PF := Files^.At(CurPos);
                      SelectFlag := not PF^.Selected;
                     end;
                    repeat
                     Message(Owner, evKeyDown, kbUp, nil)
                    until not MouseEvent(Event, evMouseMove + evMouseAuto);
                    RepeatDelay := LastRDelay; CE; MSelect := Off;
                    Exit;
                   end;
                  MakeLocal(Event.Where, MPos);
                  CurPos := Delta + (MPos.X div LineLength)* (Size.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0))
                            + MPos.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0);
                  if Event.Double then
                   begin
                    MSelect := Off;
                    CE; if Files^.Count = 0 then Exit;
                    if CurPos < Files^.Count then
                      begin
                       if ScrollBar^.Value <> CurPos then ScrollBar^.SetValue(CurPos)
                       else
                         begin
                            PF := Files^.At(ScrollBar^.Value);
                            if PF^.Attr and Directory <> 0 then
                             begin Message(@Self, evKeyDown, kbCtrlPgDn, nil); CE; Exit end;
                            if ShiftState and kbCtrlShift <> 0 then Message(@Self, evKeyDown, kbCtrlEnter, nil)
                                else begin
                                        Message(CommandLine, evKeyDown, kbDown, nil);
                                        Message(@Self, evKeyDown, kbEnter, nil);
                                        CE
                                     end;
                         end;
                      end
                   end
                  else
                  begin
                   LastRDelay := RepeatDelay; RepeatDelay := 0;
                   if MSelect and (CurPos < Files^.Count) then
                    begin
                     CurPos := Delta + (MPos.X div LineLength)* (Size.Y-Byte(FMSetup.Show and fmsColumnTitles <> 0))
                               + MPos.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0);
                     if (CurPos >= Files^.Count) then CurPos := Files^.Count - 1;
                     if (CurPos < 0) then CurPos := 0;
                     PF := Files^.At(CurPos);
                     SelectFlag := not PF^.Selected;
                     ScrollBar^.SetValue(CurPos);
                     Message(@Self, evBroadcast, cmScrollBarChanged, ScrollBar);
                     if PF^.Name[1] = '.' then PF^.Selected := False;
                    end;
                  if not MSelect and (FMSetup.Options and fmoDragAndDrop <> 0)
                    then begin
                           MSelect := Off; RepeatDelay := LastRDelay;
                           CurPos := Delta + (MPos.X div LineLength) * (Size.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0))
                                     + MPos.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0);
                           if CurPos < Files^.Count then
                            begin
                             ScrollBar^.SetValue(CurPos);
                             if MouseEvent(Event, evMouseMove) then CM_DragDropper(@Self, CurPos, @Event);
                            end;
                           CE; Exit;
                         end;
                   repeat
                    MakeLocal(Event.Where, MPos);
                    if MouseInView(Event.Where) then
                     begin
                      CurPos := Delta + (MPos.X div LineLength) * (Size.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0))
                                + MPos.Y - Byte(FMSetup.Show and fmsColumnTitles <> 0);
                      if CurPos < Files^.Count then ScrollBar^.SetValue(CurPos);
                     end else
                     begin
                      if MPos.Y < 0 then ScrollBar^.SetValue(ScrollBar^.Value - 1)
                       else if MPos.Y >= (Size.Y - 1) then ScrollBar^.SetValue(ScrollBar^.Value + 1);
                     end;
                   until not MouseEvent(Event, evMouseAuto + evMouseMove + evMouseDown);
                   RepeatDelay := LastRDelay; SendLocated;
                  end; CE; MSelect := Off;
                 end
(*               else
                 begin
                  psdel:=(MPos.X div LineLength) + 1;
                  Repeat
                   if (Drive^.LFNLen - (LineLength*psdel - MPos.X) > 5) and
                      (LineLength*psdel - MPos.X >=0) then
                    Drive^.EXTLen:=LineLength*psdel - MPos.X;
                   {Owner^.ReDraw;}
                   DrawView;
                   MouseEvent(Event, evMouseMove+evMouseUp);
                   MakeLocal(Event.Where, MPos);
                  Until Event.What=evMouseUp;
                  CE;
                 end*)
                 else
                 begin
                  psdel:=MPos.X div LineLength;
                  Repeat
                   if MPos.X - LineLength*psdel - Drive^.EXTLen >= 5 then
                    begin
                     Drive^.LFNLen:=MPos.X div (psdel+1);
                     LineLength:=Drive^.CalcLength;
                    end;
                   Owner^.ReDraw;
                   MouseEvent(Event, evMouseMove+evMouseUp);
                   MakeLocal(Event.Where, MPos);
                  Until Event.What=evMouseUp;
                  CE;
                 end;
              end;
 end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
 procedure TFilePanelRoot.ChDir;
 begin
   if Drive^.DriveType <> dtDisk then Exit;
   if (Dir[0] > #3) and (Dir[Length(Dir)] = '\') then Dec(Dir[0]);
   Drive^.lChDir(Dir);
   IncDrawDisabled;
   ReadDirectory;
   AddToDirectoryHistory(DirectoryName,integer(Drive^.DriveType));
   ScrollBar^.SetValue(0);
   DecDrawDisabled; DrawView;
   GlobalMessage(evCommand, cmRereadInfo, nil);
   Owner^.Redraw;
 end;
        {-DataCompBoy-}

END.
