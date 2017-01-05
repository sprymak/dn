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
unit MicroEd2;

interface

uses
  Objects, Microed, EdWin;

const
  TabStep: integer = 8;

procedure MISaveFileAs(AED: PFileEditor);
procedure MISaveFile(AED: PFileEditor);
procedure MIOpenFile(AED: PFileEditor);
procedure MILoadFile(AED: PFileEditor; Name: String);
function MIReadBlock(AED: PFileEditor; var FileName: String;
  RetCollector: boolean): Pointer;
procedure MILockFile(AED: PFileEditor);
procedure MIUnLockFile(AED: PFileEditor);

procedure MIStore(AED: PFileEditor; var s: TStream);
procedure MILoad(AED: PFileEditor; var s: TStream);
procedure MIAwaken(AED: PFileEditor);

const
  SmartWindow: PEditWindow = nil;
  ClipboardWindow: PEditWindow = nil;
  SmartWindowPtr: ^PEditWindow = @SmartWindow;
  ClipboardWindowPtr: ^PEditWindow = @ClipboardWindow;

implementation
uses
  DNStdDlg, advance, DNApp, Commands, Lfn, advance2, ed2, advance1,
    Views,
  Collect, WinClp, Dos, Messages, Startup, DnIni, CopyIni,
  {SBlocks,}U_KeyMap, Macro,
  xTime, Memory, Drivers,
  FlTl,
  fnotify,
  {$IFDEF PLUGIN}Plugin, {$ENDIF}
  ErrMess;

type
  ByteArray = array[1..MaxBytes] of byte;
const
  FBufSize = 8192;

function ESC_Pressed: boolean;
  var
    E: TEvent;
  begin
    Application^.Idle;
    GetKeyEvent(E);
    ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbESC)
  end;

{-DataCompBoy-}
procedure MISaveFileAs(AED: PFileEditor);
  var
    FileName: String;
    s: PStream;
    {$IFDEF PLUGIN}
    Event: TEvent; {Cat}
    {$ENDIF}
  begin
    {Cat}
    {$IFDEF PLUGIN}
    Event.What := evCommand;
    Event.Command := 65005;
    ProcessEditorEventHook(Event, AED);
    {$ENDIF}
    {/Cat}

    with AED^ do
      begin
        FileName := GetFileNameDialog(x_x, GetString(dlSaveFileAs),
        GetString(dlSaveFileAsName),
        fdOKButton+fdHelpButton, hsEditSave);
        if FileName <> '' then
          begin
            MIUnLockFile(AED);
            FileName := lFExpand(GetPath(FileName))+GetName(lFExpand(
              FileName));
            s := CheckForOver(FileName);
            if s = nil then
              begin
                MILockFile(AED);
                exit;
              end;
            EditName := lFExpand(FileName);
            if EditName[Length(EditName)] = '.' then
              SetLength(EditName, Length(EditName)-1);
            WriteBlock(EditName, s, FileLines, EdOpt.ForcedCRLF,
              OptimalFill);
            Dispose(s, Done);
            FileChanged(EditName);
            DisposeStr(PWindow(Owner)^.Title);
            if EditName = ''
            then
              PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle))
            else
              PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle)+
                ' - '+ {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(
                EditName));
            Owner^.Redraw;
            Modified := False;
            MILockFile(AED);
          end;
      end
  end { MISaveFileAs };
{-DataCompBoy-}

{-DataCompBoy-}
procedure MIOpenFile(AED: PFileEditor);
  var
    FileName: String;
  begin
    with AED^ do
      begin
        FileName := GetFileNameDialog(x_x, GetString(dlOpenFile),
        GetString(dlOpenFileName),
        fdOpenButton+fdHelpButton, hsEditOpen);
        if FileName <> '' then
          begin
            MIUnLockFile(AED);
            MILoadFile(AED, lFExpand(FileName));
            BlockVisible := False; {Cat}
            Mark.Assign(0, 0, 0, 0); {Cat}
            if not isValid then
              begin
                isValid := True;
                Message(Owner, evCommand, cmClose, nil);
                MILockFile(AED);
                exit;
              end;
            ScrollTo(0, 0);
            Owner^.Redraw;
          end;
      end
  end { MIOpenFile };
{-DataCompBoy-}

{-DataCompBoy-}
procedure MISaveFile(AED: PFileEditor);
  var
    s: PBufStream;
    i: longInt;
    F: lFile;
    dr: String;
    Nm: String;
    XT: String;
    OldAttr: word;
    l: array[0..0] of longInt;
    PC: PLineCollection;
    FileExist: boolean;
    TempEAContainerName: String;
    TempEAContainer: lFile;
    {$IFDEF PLUGIN}
    Event: TEvent; {Cat}
    {$ENDIF}
  begin
    {Cat}
    {$IFDEF PLUGIN}
    Event.What := evCommand;
    Event.Command := 65004;
    ProcessEditorEventHook(Event, AED);
    {$ENDIF}
    {/Cat}

    with AED^ do
      begin
        MIUnLockFile(AED);
        if ClipBrd then
          begin{-$VOL begin}
            if ClipBoardStream <> nil then
              Dispose(ClipBoardStream, Done);
            ClipBoardStream := nil;
            PC := New(PLineCollection, Init(100, 5, True));
            for i := 0 to FileLines^.Count-1 do
              PC^.Insert(NewLongStr(CnvLongString(FileLines^.At(i))));
            CopyLines2Stream(PC, ClipBoardStream);
            Dispose(PC, Done);
          end
        else
          begin{-$VOL end}
            if EditName = '' then
              begin
                MISaveFileAs(AED);
                exit
              end;
            FileExist := False;
            lAssignFile(F, EditName);
            ClrIO;
            OldAttr := 0;
            lGetFAttr(F, OldAttr);
            if (DOSError = 0) then
              FileExist := True;
            if FileExist and (OldAttr and ReadOnly <> 0) then
              begin
                Pointer(l[0]) := @EditName;
                if Msg(dlED_ModifyRO, @L, mfConfirmation+mfOKCancel) <>
                    cmOK
                then
                  begin
                    MILockFile(AED);
                    exit;
                  end;
              end;
            ClrIO;
            lSetFAttr(F, Archive);
            if Abort then
              begin
                MILockFile(AED);
                exit;
              end;
            if FileExist then
              begin
                TempEAContainerName := SwpDir+'DN'+ItoS(DNNumber)+
                  '.EA_';
                lAssignFile(TempEAContainer, TempEAContainerName);
                lReWriteFile(TempEAContainer, 0);
                Close(TempEAContainer.F);
                {$IFNDEF DPMI32}
                CopyEAs(EditName, TempEAContainerName);
                {$ENDIF}
                {$IFDEF WIN32}
                CopySAs(EditName, TempEAContainerName); {Cat}
                {$ENDIF}
              end;
            if EditorDefaults.EdOpt and ebfCBF <> 0 then
              begin
                lFSplit(EditName, dr, Nm, XT);
                ClrIO;
                EraseFile(dr+Nm+'.BAK');
                lChangeFileName(EditName, dr+Nm+'.BAK');
                ClrIO;
              end;
            New(s, Init(EditName, stCreate, 4096));
            if s = nil then
              begin
                MILockFile(AED);
                exit;
              end;
            {Cat: раньше почему-то проверка статуса происходила в этом месте,
      т.е. считалось, что если поток создан успешно, то и записан
      он также успешно, что неверно (например, когда место на диске
      заканчивается); теперь проверка осуществляется после записи блока
      кроме того, добавил вызов FileChanged - в случае неуспешной записи
      содержимое файла может поменяться}
            WriteBlock(EditName, s, FileLines, EdOpt.ForcedCRLF,
              OptimalFill);
            if s^.Status <> stOK then
              begin
                CantWrite(EditName);
                Dispose(s, Done);
                MILockFile(AED);
                if not (SmartPad or ClipBrd) then
                  FileChanged(EditName);
                exit;
              end;
            {/Cat}
            Dispose(s, Done);
            lAssignFile(F, EditName);
            ClrIO;
            if (OldAttr <> Archive) and (OldAttr <> $FFFF) then
              lSetFAttr(F, OldAttr or Archive);
            ClrIO;
            if FileExist then
              begin
                {$IFNDEF DPMI32}
                CopyEAs(TempEAContainerName, EditName);
                {$ENDIF}
                {$IFDEF WIN32}
                CopySAs(TempEAContainerName, EditName); {Cat}
                {$ENDIF}
                EraseFile(TempEAContainerName);
              end;
          end;
        MILockFile(AED);
        Modified := False;
        JustSaved := True;
        LastSaveUndoTimes := UndoTimes; {piwamoto}
        Owner^.Redraw;
        if not (SmartPad or ClipBrd) then
          FileChanged(EditName);
        if UpStrg(EditName) = UpStrg(MakeNormName(SourceDir,
            'DN.INI'))
        then
          begin
            LoadDnIniSettings;
            DoneIniEngine;
            ProbeINI(INIstoredtime, INIstoredsize, INIstoredcrc);

            CopyIniVarsToCfgVars;

            ConfigModified := True;
            ShowIniErrors;
            GlobalMessage(evCommand, cmReboundPanel, nil);
            {AK155 Это временно, пока есть переменные, управляющие подвалом панели.
      Конкретно - PackingInfoInBottom и PathInfoInBottom }
          end;
      end
  end { MISaveFile };
{-DataCompBoy-}

{-DataCompBoy-}
procedure MILoadFile(AED: PFileEditor; Name: String);
  label
    1;
  var
    Nm: String;
    XT: String;
    PC: PCollection; {-$VOL}
    {$IFDEF PLUGIN}
    Event: TEvent; {Cat}
    {$ENDIF}
  begin
    with AED^ do
      begin
        if FileLines <> nil then
          Dispose(FileLines, Done);
        FileLines := nil;
        if UndoInfo <> nil then
          Dispose(UndoInfo, Done);
        UndoInfo := nil;
        if RedoInfo <> nil then
          Dispose(RedoInfo, Done);
        RedoInfo := nil;
        UndoTimes := 0;
        LastDir := -1; {DrawMode := 0;}
        LastSaveUndoTimes := 0;
        JustSaved := False; {piwamoto}
        SearchOnDisplay := False;
        EdOpt.ForcedCRLF := cfNone;
1:
        if Name = '' then
          begin
            {FileLines := GetCollector(1000, 100);}
            FileLines := New(PLineCollection, Init(300, 1000, True));
              {-SBlocks}
            if ClipBrd then
              begin{-$VOL begin}
                {Cat:warn лишние переприсваивания}
                PC := nil;
                CopyStream2Lines(ClipBoardStream, PC);
                if PC <> nil then
                  with PC^ do
                    begin
                      while Count > 0 do
                        begin
                          FileLines^.Insert(At(Count-1));
                          AtDelete(Count-1);
                        end;
                      Dispose(PC, Done);
                    end;
              end; {-$VOL end}
            if FileLines^.Count = 0 then
              FileLines^.Insert(NewLongStr(''))
          end
        else
          begin
            if (StartupData.Slice2 and osuReleaseOpen) = 0 then
                Application^.BFSpeed;
            FileLines := MIReadBlock(AED, Name, True);
            {/Cat}
            if (StartupData.Slice2 and osuReleaseOpen) = 0 then
                Application^.EFSpeed;
            if not isValid then
              exit;
            if FileLines = nil then
              begin
                {FileLines := GetCollector(1000, 100);}
                FileLines := New(PLineCollection, Init(300, 1000,
                  True)); {-SBlocks}
                FileLines^.Insert(NewLongStr(''));
                KeyMap := kmAscii;
              end;
            if Name <> '' then
              Name := lFExpand(Name);
          end;
        SetLimits;
        Pos.X := 0;
        Pos.Y := 0;
        Delta := Pos;
        {InsertMode := true;}
        {BlockVisible := false;}
        {Mark.Assign(0,0,0,0);}
        Modified := False; {DrawMode := 0;}WorkModified := False;
          LastLine := -1;
        LastShape := '';
        SpecChar := False;
        WasDelete := False;
        Marking := False;
        MouseMark := False;
        RulerVisible := False;
        EnableMarking := True;
        WorkString := GetLine(0);
        if Name <> '' then
          Name := lFExpand(Name);
        EditName := Name;
        DisposeStr(PWindow(Owner)^.Title);
        {Cat:warn а не бред ли это?}
        if '*^&'+EditName = TempFile then
          begin
            EditName := '';
            TempFile := '';
          end;
        if SmartPad then
          begin
            PWindow(Owner)^.Title := NewStr('SmartPad(TM) - '+
              EditName);
          end
        else if ClipBrd then
          begin
            PWindow(Owner)^.Title := NewStr('Clipboard');
          end
        else if EditName <> ''
        then
          PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle)+
            ' - '+ {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(
            EditName))
        else
          PWindow(Owner)^.Title := NewStr(GetString(dlEditTitle));
        MILockFile(AED);
        lFSplit(EditName, FreeStr, Nm, XT);
        {PZ 2000.06.09}
        EdOpt.HiLite := Macro.InitHighLight(Nm+XT, HiLitePar, Macros,
          @EdOpt);
        if EdOpt.HiLite then
          { check if highlighting is not disabled }
          EdOpt.HiLite := (EditorDefaults.EdOpt2 and ebfHlt) <> 0;
        {PZ end}
      end;

    {Cat}
    {$IFDEF PLUGIN}
    Event.What := evCommand;
    Event.Command := 65003;
    ProcessEditorEventHook(Event, AED);
    {$ENDIF}
    {/Cat}
  end { MILoadFile };
{-DataCompBoy-}

{-DataCompBoy-}
function MIReadBlock(AED: PFileEditor; var FileName: String;
  RetCollector: boolean): Pointer;
  var
    s: PDosStream;
    B: ^ByteArray;
    i, FFSize: longInt;
    j: longInt;
    k: longInt;
    LCount: longInt;
    Lines: PLineCollection {PCollector}; {-SBlocks}
    s1, ST, s2: LongString;
    l: longInt;
    KeyMapDetecting: boolean;
    CodePageDetector: TCodePageDetector;
    OD, OA, ODOA: longInt;

  procedure CountLines;
    var
      k: longInt;
    begin
      for k := 1+byte(i > 0) to j do
        if (B^[k] = $0D) and ((k <= j-1) and (B^[k+1] = $0A)) then
            begin
            Inc(ODOA);
            Inc(LCount);
            Inc(k);
          end
        else if (B^[k] = $0D) then
          begin
            Inc(OD);
            Inc(LCount)
          end
        else if (B^[k] = $0A) then
          begin
            Inc(OA);
            Inc(LCount)
          end;
    end { CountLines };

  {Cat: добавил поддержку длинных строк}
  {Cat:todo исправить замену табуляций пробелами}
  procedure SearchLines;
    var
      P, P2: Pointer;
      k, l, M: longInt;
      QQQ: longInt;
      MMM: ShortString;
      C: Char;
      TS: longInt;
      LLL: boolean;
      NNN: boolean;
    label 1, L2;
    begin
      with AED^ do
        begin
          if ((EditorDefaults.EdOpt {$IFNDEF OS_DOS} shl 2 {$ENDIF})
              and ebfTRp) = 0
          then
            TabStep := 0
          else
            begin
              TabStep := StoI(EditorDefaults.TabSize);
              if TabStep = 0 then
                TabStep := 8;
            end;
          k := 1;
          P := B;
          l := j;
          TS := TabStep;
          repeat
            MMM := '';
            (*{$Uses Edi Ebx Esi Edx}*) {&Frame-}
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
    jmp @@EL
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
    jmp @@EL
   @@2:
    inc K
    inc edi
    mov eax, K
    cmp eax, L
    jng @@1
    xor eax, eax
    mov LLL, al
{Cat}
     jmp @@E
@@EL:
    mov NNN,1
    jmp @@EX
@@E:
    mov NNN,0
@@EX:
 {/Cat}
    pop ecx
    pop edx
    pop esi
    pop ebx
    pop edi
   end
              ;
            if LLL then
              goto L2;
            ST := ST+MMM;
            exit;
L2:
            ST := ST+MMM;
            if not NNN then
              begin
                while (ST <> '') and (ST[Length(ST)] = ' ') do
                    SetLength(ST, Length(ST)-1);
                if KeyMapDetecting then
                  CodePageDetector.CheckString(ST);
                Lines^.Insert(NewLongStr(ST)); {Lines^.AddStr(ST);}
                  {-SBlocks}
                ST := '';
              end;
            Inc(k);
          until k > j;
        end
    end { SearchLines };

  var
    Info: PView;
    ep: boolean;
    tmr: TEventTimer;
  begin { MIReadBlock }
    with AED^ do
      begin
        MIReadBlock := nil;
        Abort := False;
        if LowMemory then
          begin
            FileName := '';
            isValid := False;
            exit;
          end;
        CodePageDetector.Init;
        KeyMapDetecting := (KeyMap = kmNone);
        ODOA := 0;
        OD := 0;
        OA := 0;
        s := New(PBufStream, Init(FileName, stOpenRead, 1024));
        if (s^.Status <> stOK) then
          begin
            {$IFDEF OS_DOS}
            asm
         mov ax, $5900
         xor bx, bx
         int 21h
         mov K, ax
      end
              ;
            {$ELSE}
            k := s^.ErrorInfo;
            {$ENDIF}
            isValid := (k = 2) or (k = 3) or (k = 110);
            if (k <> 2) and (k <> 3) and (k <> 110) then
              MessFileNotOpen(FileName, k); {JO, AK155}
            Dispose(s, Done);
            CodePageDetector.Done;
            exit
          end;
        if (s^.GetSize > MemAvail-$4000)
          {$IFDEF OS_DOS} and (EditorDefaults.EdOpt and (ebfEMS+
            ebfXMS) = 0) {$ENDIF}
        then
          begin
            Application^.OutOfMemory;
            Dispose(s, Done);
            FileName := '';
            isValid := False;
            exit
          end;
        B := MemAlloc(FBufSize);
        if B = nil then
          begin
            Dispose(s, Done);
            FileName := '';
            CodePageDetector.Done;
            exit
          end;
        Info := WriteMsg(^M^M^C+GetString(dlReadingFile));
        i := 0;
        FFSize := s^.GetSize;
        LCount := 1;
        if FFSize-i > FBufSize then
          j := FBufSize
        else
          j := FFSize-i;
        ep := False;
        NewTimer(tmr, 1);
        while i < FFSize do
          begin
            s^.Read(B^[1+byte(i > 0)], j);
            UpdateWriteView(Info);
            if TimerExpired(tmr) then
              begin
                NewTimer(tmr, 3);
                ep := ESC_Pressed;
              end;
            if (s^.Status <> stOK) or Abort or ep then
              begin
                FreeMem(B, FBufSize);
                Dispose(s, Done);
                Info^.Free;
                FileName := '';
                isValid := False;
                CodePageDetector.Done;
                exit
              end;
            CountLines;
            if s^.Eof then
              break; {-$VOL}
            B^[1] := B^[j+byte(i > 0)-1];
            i := s^.GetPos;
            if FFSize-i > FBufSize-1 then
              j := FBufSize-1
            else
              j := FFSize-i;
          end;
        if (LCount > MaxCollectionSize) or
          ((MemAvail-$4000 < 4*(LCount+50)+FFSize)
          {$IFDEF OS_DOS} and (EditorDefaults.EdOpt and (ebfXMS+
            ebfEMS) = 0) {$ENDIF})
        then
          begin
            Application^.OutOfMemory;
            FreeMem(B, FBufSize);
            Dispose(s, Done);
            FileName := '';
            Info^.Free;
            isValid := False;
            CodePageDetector.Done;
            exit
          end;
        {if RetCollector then Lines := GetCollector(LCount*11, LCount+50)}
        {else Lines := New(PStdCollector, Init(LCount+50));}
        Lines := New(PLineCollection, Init(LCount+250, 1000, True));
          {-SBlocks}
        {-$VOL begin}
        if (EditorDefaults.EdOpt {$IFNDEF OS_DOS} shl 2 {$ENDIF})
            and ebfTRp = 0
        then
          TabStep := 0
        else
          begin
            TabStep := StoI(EditorDefaults.TabSize);
            if TabStep = 0 then
              TabStep := 8;
          end;
        if TabStep > 100 then
          TabStep := 100;
        {-$VOL end}
        s^.Seek(0);
        i := 0;
        FFSize := s^.GetSize;
        LCount := 1;
        ST := '';
        if FFSize-i > FBufSize then
          j := FBufSize
        else
          j := FFSize-i;
        while i < FFSize do
          begin
            UpdateWriteView(Info);
            s^.Read(B^, j);
            if TimerExpired(tmr) then
              begin
                NewTimer(tmr, 3);
                ep := ESC_Pressed;
              end;
            if (s^.Status <> stOK) or ep or Abort or (MemAvail <
                $4000) or
              LowMemory or (Lines^.Count > MaxCollectionSize)
            then
              begin
                Dispose(Lines, Done);
                Lines := nil;
                FileName := '';
                FreeMem(B, FBufSize);
                Dispose(s, Done);
                Info^.Free;
                CodePageDetector.Done;
                Application^.OutOfMemory;
                isValid := False;
                exit
              end;
            if (B^[j] = 13) and (s^.GetPos < s^.GetSize) then
              begin
                Dec(j);
                s^.Seek(s^.GetPos-1);
              end;
            SearchLines;
            i := s^.GetPos;
            if FFSize-i > FBufSize then
              j := FBufSize
            else
              j := FFSize-i;
          end;
        if (ST <> '') and (ST[1] = #10) then
          System.Delete(ST, 1, 1);
        while (ST <> '') and (ST[Length(ST)] = ' ') do
          SetLength(ST, Length(ST)-1);
        Lines^.Insert(NewLongStr(ST));
        {KOI KeyMap:=kmKoi8r} {WIN KeyMap:=kmAnsi}
          {DOS KeyMap:=kmAscii}
        if UpStrg(DefCodePage) = 'DOS' then
          KeyMap := kmAscii
        else if UpStrg(DefCodePage) = 'WIN' then
          KeyMap := kmAnsi
        else if UpStrg(DefCodePage) = 'KOI' then
          KeyMap := kmKoi8r
        else if UpStrg(DefCodePage) = 'AUTO' then
          begin
            KeyMap := CodePageDetector.DetectedCodePage;
          end
        else
          KeyMap := kmAscii;
        if (ODOA shl 1 >= OD+OA) then
          EdOpt.ForcedCRLF := cfCRLF
        else if (OD shl 1 >= ODOA+OA) then
          EdOpt.ForcedCRLF := cfCR
        else if (OA shl 1 >= ODOA+OD) then
          EdOpt.ForcedCRLF := cfLF;
        if RetCollector then
          MIReadBlock := Lines
        else
          begin
            {MIReadBlock := PStdCollector(Lines)^.Collection;}
            {PStdCollector(Lines)^.Collection := nil;}
            {Dispose(Lines,Done); Lines:=nil;}
            MIReadBlock := PLineCollection(Lines); {-SBlocks}
            Lines := nil; {-SBlocks}
          end;
        Dispose(s, Done);
        CodePageDetector.Done;
        FreeMem(B, FBufSize);
        Info^.Free;
      end
  end { MIReadBlock };
{-DataCompBoy-}

procedure MILockFile(AED: PFileEditor);
  begin
    with AED^ do
      begin
        if EditorDefaults.EdOpt and ebfLck = 0 then
          exit;
        if Locker <> nil then
          Dispose(Locker, Done);
        Locker := New(PDosStream, Init(EditName, (stOpenRead and
          fmDeny) or fmDenyWrite));
      end
  end;

procedure MIUnLockFile(AED: PFileEditor);
  begin
    with AED^ do
      begin
        if EditorDefaults.EdOpt and ebfLck = 0 then
          exit;
        if Locker = nil then
          exit; { на всякий случай }
        Dispose(Locker, Done);
        Locker := nil;
      end
  end;

procedure MIStore(AED: PFileEditor; var s: TStream);
  begin
    with AED^ do
      begin
        PutPeerViewPtr(s, HScroll);
        PutPeerViewPtr(s, VScroll);
        PutPeerViewPtr(s, InfoL);
        PutPeerViewPtr(s, BMrk);
        s.WriteStr(@EditName);
        s.Write(SmartPad, SizeOf(SmartPad));
        s.Write(ClipBrd, SizeOf(ClipBrd));
        s.Write(MarkPos, SizeOf(MarkPos));
        s.Write(EdOpt.LeftSide, 6);
        s.Write(EdOpt.HiLite, 1);
        s.Write(EdOpt.HiliteColumn, SizeOf(EdOpt.HiliteColumn));
        s.Write(EdOpt.HiliteLine, SizeOf(EdOpt.HiliteLine));
        s.Write(EdOpt.AutoIndent, SizeOf(EdOpt.AutoIndent));
        s.Write(VertBlock, SizeOf(VertBlock));
        s.Write(EdOpt.BackIndent, SizeOf(EdOpt.BackIndent));
        s.Write(EdOpt.AutoJustify, SizeOf(EdOpt.AutoJustify));
        s.Write(OptimalFill, SizeOf(OptimalFill));
        s.Write(EdOpt.AutoWrap, SizeOf(EdOpt.AutoWrap));
        s.Write(EdOpt.AutoBrackets, SizeOf(EdOpt.AutoBrackets));
        s.Write(KeyMap, SizeOf(KeyMap)); {-$VIV}
        s.Write(TabReplace, SizeOf(TabReplace)); {-$VOL}
        s.Write(EdOpt.SmartTab, SizeOf(EdOpt.SmartTab)); {-$VOL}
        s.Write(Pos, SizeOf(Pos)); {Cat}
        s.Write(InsertMode, SizeOf(InsertMode)); {Cat}
        s.Write(DrawMode, SizeOf(DrawMode)); {Cat}
        s.Write(Mark, SizeOf(Mark)); {Cat}
        s.Write(BlockVisible, SizeOf(BlockVisible)); {Cat}
        s.Write(EdOpt.ForcedCRLF, SizeOf(EdOpt.ForcedCRLF)); {Cat}
      end
  end { MIStore };

procedure MILoad(AED: PFileEditor; var s: TStream);
  var
    SS: PString;
  begin
    with AED^ do
      begin
        GetPeerViewPtr(s, HScroll);
        GetPeerViewPtr(s, VScroll);
        GetPeerViewPtr(s, InfoL);
        GetPeerViewPtr(s, BMrk);
        SS := s.ReadStr;
        if SS = nil then
          EditName := ''
        else
          begin
            EditName := SS^;
            DisposeStr(SS);
          end;
        s.Read(SmartPad, SizeOf(SmartPad));
        s.Read(ClipBrd, SizeOf(ClipBrd));
        {Cat}
        if SmartPad then
          SmartWindowPtr := @Owner;
        if ClipBrd then
          ClipboardWindowPtr := @Owner;
        {/Cat}
        s.Read(MarkPos, SizeOf(MarkPos));
        s.Read(EdOpt.LeftSide, 6);
        s.Read(EdOpt.HiLite, 1);
        s.Read(EdOpt.HiliteColumn, SizeOf(EdOpt.HiliteColumn));
        s.Read(EdOpt.HiliteLine, SizeOf(EdOpt.HiliteLine));
        s.Read(EdOpt.AutoIndent, SizeOf(EdOpt.AutoIndent));
        s.Read(VertBlock, SizeOf(VertBlock));
        s.Read(EdOpt.BackIndent, SizeOf(EdOpt.BackIndent));
        s.Read(EdOpt.AutoJustify, SizeOf(EdOpt.AutoJustify));
        s.Read(OptimalFill, SizeOf(OptimalFill));
        s.Read(EdOpt.AutoWrap, SizeOf(EdOpt.AutoWrap));
        s.Read(EdOpt.AutoBrackets, SizeOf(EdOpt.AutoBrackets));
        s.Read(KeyMap, SizeOf(KeyMap)); {-$VIV}
        s.Read(TabReplace, SizeOf(TabReplace)); {-$VOL}
        s.Read(EdOpt.SmartTab, SizeOf(EdOpt.SmartTab)); {-$VOL}
        s.Read(Pos, SizeOf(Pos)); {Cat}
        s.Read(InsertMode, SizeOf(InsertMode)); {Cat}
        s.Read(DrawMode, SizeOf(DrawMode)); {Cat}
        s.Read(Mark, SizeOf(Mark)); {Cat}
        s.Read(BlockVisible, SizeOf(BlockVisible)); {Cat}
        s.Read(EdOpt.ForcedCRLF, SizeOf(EdOpt.ForcedCRLF)); {Cat}
        isValid := True;
        Macros := New(PCollection, Init(10, 10));
        LastDir := -1;
        MenuItemStr[True] := NewStr(GetString(dlMenuItemOn));
        MenuItemStr[False] := NewStr(GetString(dlMenuItemOff));
      end
  end { MILoad };

procedure MIAwaken(AED: PFileEditor);
  var
    X, Y: longInt;
    XD: TPoint;
    Hi: boolean;
    _KeyMap: TKeyMap;
    _ForcedCrLf: TCRLF;
  begin
    with AED^ do
      begin
        X := HScroll^.Value;
        Y := VScroll^.Value;
        XD := Pos;
        Hi := EdOpt.HiLite;
        _KeyMap := KeyMap; {Cat}
        _ForcedCrLf := EdOpt.ForcedCRLF; {Cat}
        MILoadFile(AED, EditName);
        KeyMap := _KeyMap; {Cat}
        EdOpt.ForcedCRLF := _ForcedCrLf; {Cat}
        EdOpt.HiLite := Hi;
        ScrollTo(X, Y);
        Pos := XD;
        ChPosition := False;
        Owner^.Redraw;
      end
  end { MIAwaken };

end.
