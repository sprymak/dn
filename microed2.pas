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
UNIT Microed2;

INTERFACE
uses Objects, MicroEd;

const TabStep     : Integer = 8;

procedure MISaveFileAs(AED: PFileEditor);
procedure MISaveFile  (AED: PFileEditor);
procedure MIOpenFile  (AED: PFileEditor);
procedure MILoadFile  (AED: PFileEditor; Name: String);
function  MIReadBlock (AED: PFileEditor; var FileName: String;
                       RetCollector: Boolean): Pointer;
procedure MILockFile  (AED: PFileEditor);
procedure MIUnLockFile(AED: PFileEditor);

procedure MIStore     (AED: PFileEditor; var S: TStream);
procedure MILoad      (AED: PFileEditor; var S: TStream);
procedure MIAwaken    (AED: PFileEditor);

IMPLEMENTATION
uses DnStdDlg, Advance, DnApp, Commands, Lfn, Advance2, Ed2, Advance1, Views,
     Collect, WinClp, Dos, Messages, Startup, DnIni, SBlocks, u_keymap, Macro,
     XTime, Memory, Drivers;

type  ByteArray = Array[1..MaxBytes] of Byte;
const FBufSize = 4096;

function ESC_Pressed: Boolean; var E: TEvent;
begin
  Application^.Idle;
  GetKeyEvent(E); ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbEsc)
end;

        {-DataCompBoy-}
procedure MISaveFileAs(AED: PFileEditor);
 var
  FileName: String;
  S: PStream;
begin with AED^ do begin
 FileName := GetFileNameDialog(x_x, GetString(dlSaveFileAs),
                                   GetString(dlSaveFileAsName),
                               fdOKButton + fdHelpButton, hsEditSave);
  if FileName <> '' then
    begin
      MIUnlockFile(AED);
      FileName := lFExpand(GetPath(FileName))+GetName(lFExpand(FileName));
      S := CheckForOver(FileName);
      if S = nil then Exit;
      EditName := lFExpand(FileName);
      if EditName[Byte(EditName[0])]='.' then Dec(EditName[0]);
      WriteBlock(EditName, S, FileLines, EdOpt.ForcedCrLf, OptimalFill); Dispose(S,Done);
      FileChanged(EditName);
      DisposeStr(PWindow(Owner)^.Title);
      If EditName = ''
       then PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle))
       else PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle)+' - '+EditName);
      Owner^.Redraw;
      Modified := Off;
      MILockFile(AED);
    end;
end end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure MIOpenFile(AED: PFileEditor);
 var
  FileName: String;
begin with AED^ do begin
 FileName := GetFileNameDialog(x_x, GetString(dlOpenFile),
                               GetString(dlOpenFileName),
                        fdOpenButton + fdHelpButton, hsEditOpen);
 if FileName <> '' then
    begin
      MIUnlockFile(AED);
      MILoadFile(AED, lFExpand(FileName));
      if not isValid then
        begin
          isValid := On;
          Message(Owner, evCommand, cmClose, nil);
          Exit;
        end;
      ScrollTo(0,0);
      Owner^.ReDraw;
    end;
end end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure MISaveFile(AED: PFileEditor);
 var S: PBufStream;
     I: Longint;
     F: lFile;
     Dr: String;
     Nm: String;
     Xt: String;
     OldAttr: Word;
     L: array[0..0] of LongInt;
     PC: PLineCollection;
begin with AED^ do begin
 MIUnlockFile(AED);
 if ClipBrd then begin {-$VOL begin}
   if ClipboardStream<>nil then Dispose(ClipboardStream,Done);
   ClipboardStream:=nil;
   PC := New(PLineCollection, Init(100, 5) );
   for i := 0 to FileLines^.Count - 1 do
     PC^.Insert( NewStr( CnvString(FileLines^.At(i)) ) );
   CopyLines2Stream(PC, ClipboardStream);
   Dispose(PC,Done);
 end else begin                  {-$VOL end}
   if EditName = '' then begin MISaveFileAs(AED); Exit end;
   lAssignFile(F, EditName); ClrIO;
   OldAttr:=0;
   lGetFAttr(F, OldAttr);
   if (DosError = 0) and (OldAttr and ReadOnly <> 0) then
   begin
     Pointer(L[0]) := @EditName;
     if Msg(dlED_ModifyRO, @L, mfConfirmation+mfOKCancel)<>cmOK then Exit;
   end;
   ClrIO; lSetFAttr(F, Archive); if Abort then Exit;
   if EditorDefaults.GlobalOpt and ebfCBF <> 0 then
    begin
     lFSplit(EditName, Dr, Nm, Xt); ClrIO;
     EraseFile( Dr+Nm+'.BAK' );
     lAssignFile(F, EditName); lRenameFile(F, Dr+Nm+'.BAK'); ClrIO;
    end;
   New(S, Init(EditName, stCreate, 4096));
   if S = nil then Exit;
   if S^.Status <> stOK then
     begin
      CantWrite(EditName);
      Dispose(S,Done);
      MILockFile(AED);
      Exit;
     end;
   WriteBlock(EditName, S, FileLines, EdOpt.ForcedCrLf, OptimalFill);
   Dispose(S,Done);
   lAssignFile(F, EditName); ClrIO;
   if (OldAttr <> Archive) and (OldAttr <> $FFFF) then
    lSetFAttr(F, OldAttr or Archive); ClrIO;
 end;
 MILockFile(AED);
 Modified := Off;
 Owner^.Redraw;
 if not(SmartPad or ClipBrd) then FileChanged(EditName);
 if UpStrg(EditName)=UpStrg(MakeNormName(SourceDir, 'DN.INI')) then begin
   LoadDnIniSettings;
   DoneIniEngine;
   ProbeINI(INIstoredtime,INIstoredsize,INIstoredcrc);
   InterfaceData.DrvInfType := DriveInfoType;
   ConfigModified:=True;
   ShowIniErrors;
 end;
end end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure MILoadFile(AED: PFileEditor; Name: String);
 label 1;
 var  S: String;
     Nm: String;
     Xt: String;
      PC: PCollection; {-$VOL}
begin with AED^ do begin
  If FileLines<>nil then Dispose(FileLines,Done); FileLines:=nil;
  If UndoInfo <>nil then Dispose(UndoInfo,Done);  UndoInfo :=nil;
  If RedoInfo <>nil then Dispose(RedoInfo,Done);  RedoInfo :=nil;
  UndoTimes := 0; LastDir := -1; DrawMode := 0;
  SearchOnDisplay := Off; EdOpt.ForcedCrLf := cfNone;
1:
  if Name = '' then begin
    FileLines := GetCollector(1000, 100);
    if ClipBrd then begin {-$VOL begin}
      PC := nil;
      CopyStream2Lines(ClipboardStream, PC);
      if PC <> nil then with PC^ do
        while Count > 0 do begin
          FileLines^.Insert( At(Count - 1) );
          AtDelete(Count - 1);
        end;
      Dispose( PC ,Done);
    end;                  {-$VOL end}
    if FileLines^.Count = 0 then FileLines^.Insert(NewStr(''))
  end else begin
    if StartupData.Slice2 and osuReleaseOpen = 0 then Application^.BFSpeed;
    FileLines := MIReadBlock(AED, Name, On);
    if StartupData.Slice2 and osuReleaseOpen = 0 then Application^.EFSpeed;
    if not IsValid then Exit;
    if FileLines = nil then
     begin
      FileLines := GetCollector(1000, 100);
      FileLines^.Insert(NewStr(''));
      KeyMap:=kmAscii;
     end;
    if Name <> '' then Name := lFExpand(Name);
  end;
  SetLimits;
  Pos.X := 0; Pos.Y := 0;
  Delta := Pos;
  InsertMode := On;
  BlockVisible := Off;
  Mark.Assign(0,0,0,0);
  Modified := Off; DrawMode := 0; WorkModified := Off; LastLine:=-1; LastShape := '';
  SpecChar := Off; WasDelete := Off;
  Marking := Off; MouseMark := Off;
  RulerVisible := Off;
  EnableMarking := On;
  WorkString := GetLine(0);
  if Name <> '' then Name := lFExpand(Name);
  EditName := Name; DisposeStr(PWindow(Owner)^.Title);
  if '*^&'+EditName=TempFile then begin
   EditName:='';
   TempFile:='';
  end;
  if SmartPad then begin
    PWindow(Owner)^.Title := NewStr('SmartPad(TM) - ' + EditName);
  end else if ClipBrd then begin
    PWindow(Owner)^.Title := NewStr('Clipboard');
  end else if EditName <> ''
            then PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle) + ' - ' + EditName)
            else PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle));
  MILockFile(AED);
  lFSplit(EditName, FreeStr, Nm, Xt);
{PZ 2000.06.09}
  EdOpt.HiLite := Macro.InitHighLight ( Nm+Xt, HilitePar, Macros, @EdOpt );
  if EdOpt.HiLite then
    { check if highlighting is not disabled }
    EdOpt.HiLite := (EditorDefaults.Defaults and ebfHlt) <> 0;
{PZ e,nd}
end end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function MIReadBlock(AED: PFileEditor; var FileName: String;
                     RetCollector: Boolean): Pointer;
 var S: PDosStream;
     B: ^ByteArray;
     I, FFSize: LongInt;
     J: LongInt;
     K: Word;
     LCount: LongInt;
     Lines: PCollector;
     S1, ST, S2: String;
     L: Longint;
     KeyMapDetecting: Boolean;
     CodePageDetector: TCodePageDetector;
     OD, OA, ODOA, OAOD, MaxCount: longint; (* X-Man *)

 procedure CountLines;
  var K: Word;
 begin
  For k:=1+Byte(I>0) to J do
   if (B^[k]=$0D) and ((K<=J-1) and (B^[k+1]=$0A)) then begin
    inc(ODOA); inc(LCount); inc(k);
   end else
   if (B^[k]=$0A) and ((K<=J-1) and (B^[k+1]=$0D)) then begin
    inc(OAOD); inc(LCount); inc(k);
   end else
   if (B^[k]=$0D) then begin
    inc(OD); inc(LCount)
   end else
   if (B^[k]=$0A) then begin
    inc(OA); inc(LCount)
   end;
  (* X-Man >>> *) with AED^ do begin
  MaxCount:=Max(Max(ODOA,OAOD),Max(OD,OA));
  if MaxCount=ODOA then EdOpt.ForcedCRLF:=cfCRLF else
  if MaxCount=OD then EdOpt.ForcedCRLF:=cfCR else
  if MaxCount=OA then EdOpt.ForcedCRLF:=cfLF else
  EdOpt.ForcedCRLF:=cfLFCR;
  (* X-Man <<< *) end;
 end;

 procedure SearchLines;
  var P, P2: Pointer;
      K, L, M: Word;
      QQQ: LongInt;
      WL: Byte absolute ST;
      MMM: String;
      C: Char;
      TS: word;
      {$IFDEF BIT_32}LLL: boolean;{$ENDIF}
  label 1, L2;
 begin with AED^ do begin
  if (EditorDefaults.Defaults and ebfTRp)=0 then TabStep:=0
  else begin
    TabStep:=StoI(EditorDefaults.TabSize);
    if TabStep=0 then TabStep:=8;
  end;
  K := 1; P := B; L := J; TS := TabStep;
  repeat
   MMM := ST;
   {$IFNDEF BIT_32}
   asm
    les di, P
    add di, K
    dec di
    lea bx, MMM
    xor ah, ah
    mov al, ss:[bx]
    inc al
    mov si, ax
   @@1:
    mov al, es:[di]
    cmp al, 10
    jne @@n
    mov al, es:[di+1]
    cmp al, 13
    jne L2
    inc K
    jmp L2
  @@n:
    cmp al, 13
    jne  @@5
    mov al, es:[di+1]
    cmp al, 10
    jnz L2
    inc K
    jmp L2
  @@5:
    cmp al, 9
    jnz @@3
    {-$VOL begin}
    mov cx, TS
    or  cx, cx
    jz  @@3
    {-$VOL end}
   @@4:
    mov CX, Ts
    mov AX, SI
    add AX, CX
    xor DX, DX
    idiv CX
    xor DX, DX
    imul CX
    mov CX, AX
    inc CX
    sub CX, SI
    mov al, ' '
   @@4_l:
    mov ss:[bx+si], al
    inc byte ptr ss:[bx]
    mov ax, si
    inc si
    mov cx, ts
    div cl
    or  ah, ah
    jz  @@2
    mov al, ss:[bx]
    cmp al, 254
    je  L2
    jmp @@4
   @@3:
    mov ss:[bx+si], al
    inc si
    inc byte ptr ss:[bx]
    mov al, ss:[bx]
    cmp al, 254
    je  L2
   @@2:
    inc K
    inc di
    mov ax, K
    cmp ax, L
    jng @@1
   end;
   {$ELSE BIT_32}(*{$Uses Edi Ebx Esi Edx}*){&Frame-}
   asm
    push edi
    push ebx
    push esi
    push edx
    push ecx
    mov edi, P
    add edi, K
    dec edi
    lea ebx, MMM
    xor eax, eax
    mov al, [ebx]
    inc al
    mov esi, eax
   @@1:
    mov al, [edi]
    cmp al, 10
    jne  @@L
    xor eax, eax
    inc eax
    mov LLL, al
    jmp @@E
@@L:cmp al, 13
    jne  @@5
    mov al, [edi+1]
    cmp al, 10
    jz  @@O
    xor eax, eax
    inc eax
    mov LLL, al
    jmp @@E
@@O:inc K
    xor eax, eax
    inc eax
    mov LLL, al
    jmp @@E
  @@5:
    cmp al, 9
    jnz @@3
    {-$VOL begin}
    mov ecx, TS
    or  ecx, ecx
    jz  @@3
    {-$VOL end}
   @@4:
    mov ECX, Ts
    mov EAX, ESI
    add EAX, ECX
    xor EDX, EDX
    idiv ECX
    xor EDX, EDX
    imul ECX
    mov ECX, EAX
    inc ECX
    sub ECX, ESI
    mov al, ' '
   @@4_l:
    mov [ebx+esi], al
    inc byte ptr [ebx]
    mov eax, esi
    inc esi
    mov ecx, ts
    div cl
    or  ah, ah
    jz  @@2
    mov al, [ebx]
    cmp al, 254
    jne @@4
    xor eax, eax
    inc eax
    mov LLL, al
    jmp @@E
   @@3:
    mov [ebx+esi], al
    inc esi
    inc byte ptr [ebx]
    mov al, [ebx]
    cmp al, 254
    jne @@2
    xor eax, eax
    inc eax
    mov LLL, al
    jmp @@E
   @@2:
    inc K
    inc edi
    mov eax, K
    cmp eax, L
    jng @@1
    xor eax, eax
    mov LLL, al
@@E:
    pop ecx
    pop edx
    pop esi
    pop ebx
    pop edi
   end;
   if LLL then goto L2;
   {$ENDIF}
   ST := MMM;
   Exit;
L2:ST := MMM;
   while (ST[WL]=' ') do Dec(WL);
   if KeyMapDetecting then CodePageDetector.CheckString(ST);
   Lines^.AddStr(ST);
   ST := '';
   Inc(K);
  until K > J;
 end end;

 var Info: PView;
     ep : Boolean;
     tmr: TEventTimer;
begin with AED^ do begin
 MIReadBlock := nil; Abort := Off;
 if LowMemory then begin
   FileName:=''; IsValid:=False; Exit;
 end;
 CodePageDetector.Init; KeyMapDetecting:=(KeyMap=kmNone);
 ODOA:=0; OD:=0; OA:=0; OAOD:=0;
 S := New(PBufStream, Init(FileName, stOpenRead, 1024));
 if (S^.Status <> stOK) then
    begin
      {$IFDEF OS_DOS}
      asm
         mov ax, $5900
         xor bx, bx
         int 21h
         mov K, ax
      end;
      {$ELSE}
      K := DosError;
      {$ENDIF}
      isValid := (K = 2) or (K = 3);
      if (K <> 2) and (K <> 3) then MessageBox(GetString(dlFBBNoOpen)+FileName, nil, mfError + mfOKButton);
      Dispose(S,Done); CodePageDetector.Done;
      Exit
    end;
 if (S^.GetSize > MemAvail - $4000) and
    (EditorDefaults.GlobalOpt and (ebfEMS+ebfXMS) = 0) then
 begin
   Application^.OutOfMemory; Dispose(S,Done); FileName := ''; isValid := Off;
   Exit
 end;
 B := MemAlloc(FBufSize+1); if B = nil then
 begin
   Dispose(S,Done);
   FileName := '';
   CodePageDetector.Done;
   Exit
 end;
 B^[FBufSize]:=0; {For prevent crash}
 Info := WriteMsg(^M^M^C+GetString(dlReadingFile));
 I := 0; FFSize := S^.GetSize; LCount := 1;
 if FFSize - I > FBufSize then J := FBufSize else J := FFSize - I;
 ep := False; NewTimer(Tmr,1);
 While I < FFSize do
  begin
   S^.Read(B^[1+Byte(I>0)], J);
   UpdateWriteView(Info);
   if TimerExpired(tmr) then
   begin
     NewTimer(Tmr, 3);
     ep := ESC_Pressed;
   end;
   if (S^.Status <> stOK) or Abort or ep then
    begin
      FreeMem(B, FBufSize); Dispose(S,Done); Info^.Free; FileName := '';
      IsValid := False;
      CodePageDetector.Done;
      Exit
    end;
   CountLines;
   if S^.Eof then break; {-$VOL}
   B^[1] := B^[J+Byte(I>0)-1]; I := S^.GetPos;
   if FFSize - I > FBufSize-1 then J := FBufSize-1 else J := FFSize - I;
  end;
 if (LCount > MaxCollectionSize) or
    ((MemAvail-$4000 < 4*(LCount+50)+FFSize) and
     (EditorDefaults.GlobalOpt and (ebfXMS+ebfEMS) = 0)) then
  begin
    Application^.OutOfMemory;
    FreeMem(B, FBufSize);
    Dispose(S,Done);
    FileName := '';
    Info^.Free;
    isValid := Off;
    CodePageDetector.Done;
    Exit
  end;
 if RetCollector then Lines := GetCollector(LCount*11, LCount+50)
                 else Lines := New(PStdCollector, Init(LCount+50));
 {-$VOL begin}
 if EditorDefaults.Defaults and ebfTRp = 0 then TabStep:=0
 else begin
   TabStep:=StoI(EditorDefaults.TabSize);
   if TabStep = 0 then TabStep := 8;
 end;
 if TabStep > 100 then TabStep := 100;
 {-$VOL end}
 S^.Seek(0);
 I := 0;
 FFSize := S^.GetSize;
 LCount := 1;
 ST := '';
 if FFSize - I > FBufSize then J := FBufSize
                          else J := FFSize - I;
 While I < FFSize do
   begin
     UpdateWriteView(Info);
     S^.Read(B^, J);
     if TimerExpired(tmr) then
     begin
       NewTimer(Tmr, 3);
       ep := ESC_Pressed;
     end;
     if (S^.Status <> stOK) or ep or Abort or (MemAvail < $4000) or
        LowMemory or (Lines^.Count > MaxCollectionSize) then
       begin
         Dispose(Lines,Done); Lines:=nil;
         FileName := '';
         FreeMem(B, FBufSize);
         Dispose(S,Done);
         Info^.Free;
         CodePageDetector.Done;
         Application^.OutOfMemory;
         isValid := Off;
         Exit
       end;
     if (((EdOpt.ForcedCRLF = cfCRLF) and (B^[J]=13)) or
         ((EdOpt.ForcedCRLF = cfLFCR) and (B^[J]=10)))
        and (S^.GetPos < S^.GetSize) then begin
       Dec(J);
       S^.Seek(S^.GetPos-1);
     end;
     SearchLines;
     I := S^.GetPos;
     if FFSize - I > FBufSize then J := FBufSize
                              else J := FFSize - I;
   end;
  if ST[1] in [#10, #13] then DelFC(ST);
  while (ST[Byte(ST[0])]=' ') do Dec(ST[0]);
  Lines^.Insert(NewStr(ST));
  {KOI KeyMap:=kmKoi8r}{WIN KeyMap:=kmAnsi}{DOS KeyMap:=kmAscii}
  if UpStrg(DefCodePage) = 'DOS' then KeyMap:=kmAscii else
  if UpStrg(DefCodePage) = 'WIN' then KeyMap:=kmAnsi  else
  if UpStrg(DefCodePage) = 'KOI' then KeyMap:=kmKoi8r else
  if UpStrg(DefCodePage) = 'AUTO'then begin
   KeyMap:=CodePageDetector.DetectedCodePage;
  end else KeyMap:=kmAscii;
  if RetCollector then MIReadBlock := Lines
                else begin
                       MIReadBlock := PStdCollector(Lines)^.Collection;
                       PStdCollector(Lines)^.Collection := nil;
                       Dispose(Lines,Done); Lines:=nil;
                     end;
  Dispose(S,Done); CodePageDetector.Done;
  FreeMem(B, FBufSize); Info^.Free;
end end;
        {-DataCompBoy-}

procedure MILockFile(AED: PFileEditor);
begin with AED^ do begin
   if EditorDefaults.GlobalOpt and ebfLck = 0 then Exit;
   if Locker<>nil then Dispose(Locker,Done);
   Locker := New(PDOSStream, Init(EditName, (stOpenRead and fmDeny) or fmDenyWrite));
end end;

procedure MIUnLockFile(AED: PFileEditor);
begin with AED^ do begin
   if EditorDefaults.GlobalOpt and ebfLck = 0 then Exit;
   Dispose(Locker,Done); Locker:=nil;
end end;

procedure MIStore(AED: PFileEditor; var S: TStream);
begin with AED^ do begin
 PutPeerViewPtr(S, HScroll);
 PutPeerViewPtr(S, VScroll);
 PutPeerViewPtr(S, InfoL);
 PutPeerViewPtr(S, BMrk);
 S.WriteStr(@EditName);
 S.Write(SmartPad, Sizeof(SmartPad));
 S.Write(ClipBrd, Sizeof(ClipBrd));
 S.Write(MarkPos, Sizeof(MarkPos));
 S.Write(EdOpt.LeftSide, 6);
 S.Write(EdOpt.HiLite,1);
 S.Write(EdOpt.HiliteColumn, SizeOf(EdOpt.HiliteColumn));
 S.Write(EdOpt.HiliteLine, SizeOf(EdOpt.HiliteLine));
 S.Write(EdOpt.AutoIndent, SizeOf(EdOpt.AutoIndent));
 S.Write(VertBlock, SizeOf(VertBlock));
 S.Write(EdOpt.BackIndent, SizeOf(EdOpt.BackIndent));
 S.Write(EdOpt.AutoJustify, SizeOf(EdOpt.AutoJustify));
 S.Write(OptimalFill, SizeOf(OptimalFill));
 S.Write(EdOpt.AutoWrap, SizeOf(EdOpt.AutoWrap));
 S.Write(EdOpt.AutoBrackets, SizeOf(EdOpt.AutoBrackets));
 S.Write(KeyMap, SizeOf(KeyMap)); {-$VIV}
 S.Write(TabReplace, SizeOf(TabReplace)); {-$VOL}
 S.Write(EdOpt.SmartTab, SizeOf(EdOpt.SmartTab)); {-$VOL}
end end;

procedure MILoad(AED: PFileEditor; var S: TStream);
 var SS: PString;
begin with AED^ do begin
 GetPeerViewPtr(S, HScroll);
 GetPeerViewPtr(S, VScroll);
 GetPeerViewPtr(S, InfoL);
 GetPeerViewPtr(S, BMrk);
 SS := S.ReadStr;
 EditName := SS^;
 DisposeStr(SS);
 S.Read(SmartPad, Sizeof(SmartPad));
 S.Read(ClipBrd, Sizeof(ClipBrd));
 S.Read(MarkPos, Sizeof(MarkPos));
 S.Read(EdOpt.LeftSide, 6);
 S.Read(EdOpt.HiLite, 1);
 S.Read(EdOpt.HiliteColumn, SizeOf(EdOpt.HiliteColumn));
 S.Read(EdOpt.HiliteLine, SizeOf(EdOpt.HiliteLine));
 S.Read(EdOpt.AutoIndent, SizeOf(EdOpt.AutoIndent));
 S.Read(VertBlock, SizeOf(VertBlock));
 S.Read(EdOpt.BackIndent, SizeOf(EdOpt.BackIndent));
 S.Read(EdOpt.AutoJustify, SizeOf(EdOpt.AutoJustify));
 S.Read(OptimalFill, SizeOf(OptimalFill));
 S.Read(EdOpt.AutoWrap, SizeOf(EdOpt.AutoWrap));
 S.Read(EdOpt.AutoBrackets, SizeOf(EdOpt.AutoBrackets));
 S.Read(KeyMap, SizeOf(KeyMap)); {-$VIV}
 S.Read(TabReplace, SizeOf(TabReplace)); {-$VOL}
 S.Read(EdOpt.SmartTab, SizeOf(EdOpt.SmartTab)); {-$VOL}
 isValid := True;
 Macros := New(PCollection, Init(10,10));
 LastDir := -1;
 MenuItemStr[True]:=NewStr(GetString(dlMenuItemOn));
 MenuItemStr[False]:=NewStr(GetString(dlMenuItemOff));
end end;

procedure MIAwaken(AED: PFileEditor);
 var X, Y: LongInt;
     XD: TPoint;
     Hi: Boolean;
begin with AED^ do begin
 X := HScroll^.Value;
 Y := VScroll^.Value;
 XD := Pos;
 Hi := EdOpt.HiLite;
 MILoadFile(AED, EditName);
 EdOpt.HiLite := Hi;
 ScrollTo(X,Y);
 Pos := XD;
 ChPosition := Off;
 Owner^.Redraw;
end end;


END.
