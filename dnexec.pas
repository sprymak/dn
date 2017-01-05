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
unit DNExec;

interface

uses
  files,
  UserMenu, Startup, Objects, FilesCol, Commands, TitleSet
  {$IFDEF DPMI}, DPMI {$ENDIF}
  ;

{$IFNDEF VIRTUALPASCAL}
procedure SaveDsk;
{$ENDIF}
procedure ExecString(S: PString; WS: String);
procedure ExecStringRR(S: PString; WS: String; RR: Boolean); {JO}
    {JO:  отличается от ExecString наличием булевской переменной RR, которая}
    {     указывает, перечитывать панель после выполнения или нет           }

function  SearchExt(FileRec: PFileRec; var HS: String): Boolean; {DataCompBoy}
function  ExecExtFile(const ExtFName: string; UserParams: PUserParams; SIdx: TStrIdx): Boolean; {DataCompBoy}
procedure ExecFile(const FileName: string); {DataCompBoy}

procedure AnsiExec(const Path: String; const ComLine: AnsiString); {JO}

const
  fExec: boolean = false; {выполняется внешняя программа}

implementation

uses
{$IFDEF OS2}
  Os2Base, dnini,
{$ENDIF}
{$IFDEF WIN32}
  Windows,
{$ENDIF}
  DnUtil, Advance, DnApp, Advance1, LFN,
  Dos, Advance3, FlPanelX, CmdLine, Views, Advance2, Drivers, Advance4,
{$IFDEF VIRTUALPASCAL}
  Videoman, Memory, VPSysLow, VpSysLo2,
{$ENDIF}
{$IFDEF UserSaver}
  UserSavr,
{$ENDIF}
  Messages, Strings;

{$IFDEF OS_DOS}
procedure SaveDsk;
begin
 ClrIO;
 if ((OpSys <> opDos) or (StartupData.Unload and osuAutosave = 0))
    and not (TottalExit) then
  PDNApplication(Application)^.SaveDesktop(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
end;
{$ENDIF}

{JO}
{$IFDEF VIRTUALPASCAL}
{ AnsiExec - аналог DOS.Exec , который в качестве }
{ коммандлайна использует строку типа Ansistring }
{ и соответственно не имеет ограничения в 255 символов}
procedure AnsiExec(const Path: String; const ComLine: AnsiString);
var
  PathBuf: array [0..255] of Char;
  Ans1: AnsiString;
begin
  Ans1 := ComLine + #0;

  SysTVKbdDone; {JO: см. VpSysLo2 ; если этого не делать - при вызове     }
                {    консольных программ вроде архиваторов без командного }
                {    процессора или с "неправильным" командным процессором}
                {    (например, 4OS2) внешние программы не видят ввода с  }
                {    клавиатуры                                           }

  {Cat: чтобы ДН в Win9x не тормозил, повышаем ему приоритет. Однако, незачем
        оставлять этот повышенный приоритет при запуске внешних программ}
  {$IFDEF WIN95_HIGHPRIORITY}
  if SysPlatformId = 1 then {Win9x}
    begin
      SetPriorityClass(GetCurrentProcess, Normal_Priority_Class);
      DosError := SysExecute(StrPCopy(PathBuf, Path), PChar(Ans1), nil, ExecFlags = efAsync, nil, -1, -1, -1);
      SetPriorityClass(GetCurrentProcess, High_Priority_Class);
    end
  else {WinNT}
  {$ENDIF}
  {/Cat}
    DosError := SysExecute(StrPCopy(PathBuf, Path), PChar(Ans1), nil, ExecFlags = efAsync, nil, -1, -1, -1);

  SysTVKbdInit; {Cat: в OS/2: боремся с интерпретацией Ctrl-C как Ctrl-Break
                      в WinNT: боремся с пропаданием мышиного курсора}
end;
{$ENDIF}
{/JO}

{AK155 30-12-2001
Это попытка определить тип вызываемой программы, чтобы GUI-программу
вызывать без ожидания завершения, а все прочие - с ожиданием.
Если расширение не указано, то никаких попыток распознать GUI-программу
не делается. В частности не производится, поиск по переменной окружения
Path, так как это трудно сделать не криво. Например, если запускается
некая prog, то неправильно искать в путях файл prog.exe. Может случиться,
что найдем, а где-то раньше, например, в текущем каталоге, есть prog.com
или prog.cmd, а мы его вызовем, будто он GUI. Кстати, Far глючит именно
так. Запишите в текщий каталог notepad.cmd и введите в комстроке notepad.
А потом нажмите Enter  на этом самом notepad.cmd.
}
{Результат - код подсистемы или 0}
function Win32Program(S: PString): SmallWord;
  const
    PETag = $00004550; {'PE'#0#0}
  var
    f: file;
    PathEnv: string;
    Dir: DirStr;
    Name: NameStr;
    Ext: ExtStr;
    RealName: string;
    NewExeOffs: SmallWord;
    NewHeader: record
      Signature: longint;
      dummy1: array[1..16] of byte;
      SizeOfOptionalHeader: SmallWord;
      Characteristics: SmallWord;
   {OptionalHeader}
      dummy2: array[1..68] of byte;
      Subsystem: SmallWord;
      end;
    l: longint;
  begin
  result := 0;
  FSplit(S^, Dir, Name, Ext);
  UpStr(Ext);
  if Ext <> '.EXE' then
    exit;
  RealName := S^;
  assign(f, RealName); reset(f, 1);
  if (IOResult <> 0) and (Dir = '') then
    begin
    PathEnv := GetEnv('PATH');
    RealName := FSearch(S^, PathEnv);
    if RealName = '' then
      exit;
    assign(f, RealName); reset(f, 1);
    if IOResult <> 0 then
      exit; {вообще-то, так быть не должно, раз мы ее нашли}
    end;
  Seek(f, $3C);
  BlockRead(f, NewExeOffs, 2, l);
  if (NewExeOffs=0) or (l <> 2) then
    begin
      close(f); {Cat}
      exit;
    end;
  Seek(f, NewExeOffs);
  BlockRead(f, NewHeader, SizeOf(NewHeader), l);
  with NewHeader do
    begin
    if (l < 70) or (Signature <> PETag) or (SizeOfOptionalHeader < 70) then
      begin
        close(f); {Cat}
        exit;
      end;
    result := Subsystem;
    end;
  close(f);
  end;

{$IFDEF Win32}
function GUIProgram(S: PString): boolean;
  begin
  result := Win32Program(S) = 2 {IMAGE_SUBSYSTEM_WINDOWS_GUI};
  end;
{$ENDIF}

{$IFDEF OS2}
{Cat 12-01-2002
Это попытка определить тип вызываемой программы, чтобы GUI-программу
вызывать без ожидания завершения, а все прочие - с ожиданием.
Для OS/2 запрашиваем DosQueryAppType, причём, как показывают эксперименты,
оно успешно работает не только по имени файла, как сказано в документации,
но ей удаётся скормить и целую командную строку (то есть, система сама
отбрасывает из переданной строки параметры и осуществляет поиск по
переменной окружения Path). Конечно, нет гарантии, что это будет всегда
работать правильно, но вряд ли вручную это удастся сделать лучше.
}
function GUIProgram(S: PString): Boolean;
var
  SS: String;
  Flags: LongInt;
begin
  SS := S^+#0;
  if DosQueryAppType(@SS[1], Flags) <> 0 then
  { Возможные особые случаи, приводящие к ошибке, и связанные с ними проблемы:
    2    Error_File_Not_Found
           Если не указано расширение, система предполагает EXE, поэтому эта
           ошибка сообщается на COM, BAT, CMD-файлы, если EXE с таким же
           именем не найден. Если же найдётся соответствующий EXE, то всё ещё
           хуже, поскольку просмотрен будет EXE где-то далеко, а запущен COM,
           BAT, CMD из текущего каталога.
    191  Error_Invalid_Exe_Signature
           Это сообщается на все файлы, которые не получится запустить "сами
           по себе", то есть и на BAT и CMD тоже - они интерпретируются
           командным процессором. Однако, для COM-файлов, хотя они и не
           содержат этой самой Exe_Signature, ошибка не возникает.
  }
    GUIProgram := False
  else
  { Просматриваем интересующие нас флаги в битах 2-0:
      000   fapptyp_NotSpec
      001   fapptyp_NotWindowCompat
      010   fapptyp_WindowCompat
      011   fapptyp_WindowApi
  }
    GUIProgram := (Flags and 3 = 3);
end;
{/Cat}
{$ENDIF}

        {-DataCompBoy-}
procedure ExecStringRR(S: PString; WS: String; RR: Boolean);
 var I: Integer;
{$IFNDEF VIRTUALPASCAL}
     F1: lText;
     M: String;
{$ELSE}
     EV: TEvent;
     X, Y: SmallWord; {Cat}
     ScreenSize: TSysPoint; {Cat}
{$ENDIF}
{$IFNDEF VIRTUALPASCAL}
 label 1;
{$ENDIF}

begin
{$IFNDEF VIRTUALPASCAL}
 M:=S^;
 DelRight(M);
 if {$IFDEF OS_DOS}not Chk4Dos and {$ENDIF}(Pos('||', M) <> 0) then
  begin
    lAssignText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT'); ClrIO;
    lRewriteText(F1); if IOResult <> 0 then begin Close(F1.T); Exit; end;
    I := 0;
    repeat
      I := Pos('||', M);
      if I = 0 then I := Length(M)+1;
      WriteLn(F1.T, MakeCMDParams(Copy(M, 1, I-1),
                                CnvString(CurFileActive),
                                CnvString(CurFilePassive)));
      Delete(M, 1, I+1);
    until (M = '');
    Close(F1.T);
    M := SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT ';
  end else M := MakeCMDParams(M, CnvString(CurFileActive), CnvString(CurFilePassive));
1:
 M := ' '+M+#13; SetLength(M, Length(M)-1);
{ ----- }
 SaveDsk;
 Exiting:=True;
 Application^.Done;
 if WS<>'' then Writeln(WS);
 if (LoaderSeg <> 0) then Move(M, mem[LoaderSeg:CommandOfs], Length(M)+2);
 if (SystemData.Options and ossFastExec <> 0) or RunFrom2E
 then asm
  mov ax, 9903h
  mov cl, 1
  int 2Fh
 end;
 if TimerMark then DDTimer := Get100s
              else DDTimer := 0;
 asm
    mov ax, 9904h
    mov dx, word ptr DDTimer
    mov cx, word ptr DDTimer+2
    int 2Fh
    mov ax, 9902h
    mov cl, 1
    int 2Fh
 end;
 Halt(1);
{$ELSE}
  DoneSysError;
  DoneEvents;
  DoneVideo;
  DoneDOSMem;
  DoneMemory;
  {$IFDEF OS_DOS}asm cld; mov eax,3; int $10; end;{$ENDIF}
  SwapVectors;
  if TimerMark then DDTimer := Get100s
               else DDTimer := 0;
  if WS<>'' then Writeln(WS);
  SetTitle(S^);
  fExec := True;
{$IFNDEF DPMI32}
  if GUIProgram(S) then
    S^ := 'start ' + S^
  {$IFDEF OS2}
  else
    case Win32Program(S) of
     2 {IMAGE_SUBSYSTEM_WINDOWS_GUI}:  S^ := ExecWin32GUI + ' ' + S^;
     3 {IMAGE_SUBSYSTEM_WINDOWS_CUI}:  S^ := ExecWin32CUI + ' ' + S^;
    end {case};
  {$ENDIF}
    ;
{$ENDIF}
  AnsiExec(GetEnv('COMSPEC'),'/c '+S^);
  fExec := False;
{AK155, Cat: чтобы комстрока и меню не налазили на вывод}
  SysGetCurPos(X, Y);
  if InterfaceData.Options and ouiHideStatus = 0 then
    inc(Y);
  if X <> 0 then
    writeln;
  SysTvGetScrMode(@ScreenSize);
  if Y >= ScreenSize.Y then
    writeln;
{/AK155, Cat}
  if TimerMark then begin
   DDTimer := Get100s-DDTimer;
   ev.what:=evCommand;
   ev.command:=cmShowTimeInfo;
   ev.infoptr:=nil;
   Application^.PutEvent(ev);
  end;
  I := DosError; ClrIO;
  SwapVectors;
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.LST'); {DataCompBoy}
  InitDOSMem;
  InitMemory;
  InitVideo;
  InitEvents;
  InitSysError;
  Application^.Redraw;
 {JO}
  if RR then
    begin
      GlobalMessage(evCommand, cmPanelReread, nil);
      GlobalMessage(evCommand, cmRereadInfo, nil);
    end;
 {/JO}
{$ENDIF}

{AK155 без этого курсор комстроки не становится на место}
{$IFDEF Win32}
  if (CommandLine <> nil) then
    begin
    SysTVSetCurPos(-2, -2);
    SysCtrlSleep(1); // это для нитки курсора
    CommandLine^.Update;
    end;
{$ENDIF}
{/AK155}
end;
        {-DataCompBoy-}

{JO}
procedure ExecString(S: PString; WS: String);
begin
  ExecStringRR(S, WS, True);
end;
{/JO}
        {-DataCompBoy-}
function SearchExt(FileRec: PFileRec; var HS: String): Boolean;
var
  AllRight : Boolean;
  f        : PTextReader;
  F1       : lText;
  s,s1     : String;
  BgCh,EnCh: Char;
  EF, First: Boolean;
  I        : Integer;
  Local    : Boolean;
  FName: string;
  UserParam: TUserParams;
  D        : TMaskData;
 {$IFDEF OS2}
  WriteEcho: Boolean;
 {$ENDIF}
label RL;

begin
  fillchar(D, sizeof(d), 0);
  First := True;
  Message(Desktop, evBroadcast, cmGetUserParams, @UserParam);
  UserParam.Active:=FileRec;
  FName:=FileRec^.FlName[true];
  {lGetDir(0, ActiveDir);} {Cat:warn закомментировал это в процессе отлова багов, но надо будет проверить, не добавил ли новых}
  SearchExt:=False;
  Local := true;
  f := New(PTextReader, Init('DN.EXT'));
  if f=nil then
   begin
  RL:
     Local := false;
     f := New(PTextReader, Init(SourceDir+'DN.EXT'));
   end;
  if f=nil then exit; AllRight:=False;
  BgCh:='{';EnCh:='}'; Abort := false; EF:=false;
  if PShootState and 8 > 0 then begin BgCh:='[';EnCh:=']';end else
  if PShootState and 3 > 0 then begin BgCh:='(';EnCh:=')';end;
  While (not f^.EOF) and (not AllRight) do begin
    S := f^.GetStr;
    if S[1] <> ' ' then
     begin
      I := PosChar(BgCh, S); if (I = 0) or (S[I+1]=BgCh) then Continue;
      S1 := Copy(S, 1, I-1);
      DelLeft(S1);
      DelRight(S1);
      if S1[1]<>';' then begin
       D.Filter := S1;
       MakeTMaskData(D);
       if InExtFilter(FName, D) then begin
        lAssignText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$'+CmdExt); ClrIO;
        lRewriteText(F1); if IOResult <> 0 then
          begin
            Dispose(F,Done);
            FreeTMaskData(D); {Cat}
            Exit;
          end;
       {$IFNDEF OS2}
        Writeln(F1.T, '@echo off');
       {$ELSE}
        WriteEcho := true;
       {$ENDIF}
        System.Delete(S, 1, PosChar(BgCh, S));
        repeat
         Replace(']]', #0, S);
         Replace('))', #1, S);
         Replace('}}', #2, S);
         DelLeft(S); DelRight(S);
         if S[Length(S)] = EnCh then
          begin SetLength(S, Length(S)-1); EF := true; if S <> '' then
           begin
             Replace(#0, ']', S);
             Replace(#1, ')', S);
             Replace(#2, '}', S);
             S := MakeString(S, @UserParam, false, nil);
             HS := S;
             {$IFDEF OS2}
             {JO: под осью если строка на REXX'е или Perl'е, то не нужно добавлять @Echo off}
              if WriteEcho and (Copy(S, 1, 2) <> '/*') and (Copy(S, 1, 2) <> '#!') then Writeln(F1.T, '@Echo off');
              WriteEcho := false;
             {$ENDIF}
             WriteLn(F1.T, S); Break
           end;
          end;
         if S <> '' then
          begin
           Replace(#0, ']', S);  Replace(#1, ')', S);  Replace(#2, '}', S);
           if (BgCh <> '[') then S := MakeString(S, @UserParam, false, nil);
           if First and (BgCh <> '[') then HS := S;
             {$IFDEF OS2}
             {JO: под осью если строка на REXX'е или Perl'е, то не нужно добавлять @Echo off}
              if WriteEcho and (Copy(S, 1, 2) <> '/*') and (Copy(S, 1, 2) <> '#!') then Writeln(F1.T, '@Echo off');
              WriteEcho := false;
             {$ENDIF}
           WriteLn(F1.T, S);
           First := False;
          end;
         if (F^.EOF) then Break;
         if not EF then S := F^.GetStr;
        until (IOResult <> 0) or Abort or EF;
        Close(F1.T);
        AllRight := true;
       end;
      end;
     end;
  end;
  Dispose(F,Done);
  {D.Filter:=''; MakeTMaskData(D);}
  FreeTMaskData(D); {Cat}
  if not EF and not Abort and Local then Goto RL;
  if EF and (BgCh = '[') then
   begin
    EraseFile( SwpDir+'$DN'+ItoS(DNNumber)+'$.MNU' );
    lRenameText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$.MNU');
    EF := ExecUserMenu(false);
    if not EF then lEraseText(F1);
   end;
  SearchExt:=not Abort and EF;
  FreeTMaskData(D); {Cat}
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function ExecExtFile(const ExtFName: string; UserParams: PUserParams; SIdx: TStrIdx): Boolean;
 var F: PTextReader;
     S,S1: String;
     FName, LFN: String;
     Event: TEvent;
     I,J: Integer;
{$IFNDEF VIRTUALPASCAL}
     PP: PView;
{$ENDIF}
     Success, CD: Boolean;
     Local: Boolean;
     D: TMaskData;
 label 1,1111, RepeatLocal;

begin
 FillChar(D, sizeof(D), 0);
 ExecExtFile := false;
 FileMode := $40;
 Local := true;
 LFN := UserParams^.Active^.FlName[true];
 if CharCount('.', LFN)=0 then LFN:=LFN+'.';
 FName := UserParams^.Active^.FlName[true xor InvLFN];

 F := New(PTextReader, Init(ExtFName));

 if F = nil then
  begin
RepeatLocal:
    Local := false;
    F := New(PTextReader, Init(SourceDir+ExtFName));
  end;
 if F = nil then Exit;
 While not F^.EOF do
   begin
      S := F^.GetStr;
      DelLeft(S);
      S1:=fDelLeft(Copy(S, 1, pred(PosChar(':', S))));
      if (S1='') or (S1[1]=';') then continue;
      D.Filter := S1; MakeTMaskData(D);
      if InExtFilter(FName, D) or InExtFilter(LFN, D) then goto 1111;
   end;
   ExecExtFile := false;
   Dispose(F, Done);
   {D.Filter := ''; MakeTMaskData(D);}
   FreeTMaskData(D); {Cat}
   if Local then Goto RepeatLocal;
   FreeTMaskData(D); {Cat}
   Exit;
1111:
   Delete(S, 1, succ(Length(S1)));
   Dispose(F, Done);
   if not Application^.Valid(cmQuit) then
     begin
       FreeTMaskData(D); {Cat}
       Exit;
     end;
   ClrIO;
   S1:='';
   S:=MakeString(S, UserParams, false, @S1);
   if S1<>''
    then TempFile:='!'+S1+'|'+MakeNormName(UserParams^.Active^.Owner^, LFN)
    else if TempFile<>''
          then TempFile:=MakeNormName(UserParams^.Active^.Owner^, LFN);
{$IFDEF VIRTUALPASCAL}
  {if TempFile <> '' then SaveDsk;}
   TempFileSWP := TempFile;
   TempFile := '';
{$ENDIF}
   if Abort then
     begin
       FreeTMaskData(D); {Cat}
       Exit;
     end;
   if S[1] = '*' then Delete(S, 1, 1); {DelFC(S);}
{$IFNDEF VIRTUALPASCAL}
   PP := _WriteMsg(' '+GetString(SIdx));
   if not CheckExit then
     begin
       ExecExtFile:=false;
       FreeTMaskData(D); {Cat}
       Exit;
     end;
{$ENDIF}
   lGetDir(0, S1);
{$IFDEF OS_DOS}
   if UpStrg(MakeNormName(lfGetLongFileName(UserParams^.Active^.Owner^),'.'))<>
      UpStrg(MakeNormName(S1,'.'))
    then begin
          DirToChange:=S1;
          lChDir(lfGetLongFileName(UserParams^.Active^.Owner^));
         end;
{$ELSE}
   if UpStrg(MakeNormName(UserParams^.Active^.Owner^,'.'))<>
      UpStrg(MakeNormName(S1,'.'))
    then begin
          DirToChange:=S1;
          lChDir(UserParams^.Active^.Owner^);
         end;
{$ENDIF}
   ExecExtFile := true;
   Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
{$IFDEF UserSaver}
   InsertUserSaver(false); {JO}
{$ENDIF}
   ExecString(@S, '');
{$IFDEF VIRTUALPASCAL}
   if TempFileSwp <> '' then Message(Application, evCommand, cmRetrieveSwp, nil);
{$ENDIF}
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure ExecFile(const FileName: string);
 var S,
{$IFNDEF OS2}
     L,
{$ENDIF}
     M: String;
     fr: PFileRec;

 procedure PutHistory(B: Boolean);
 begin
  if M = '' then Exit;
  CmdLine.Str := M;
  CmdLine.StrModified := true;
  CmdDisabled := B;
  Message(CommandLine, evKeyDown, kbDown, nil);
  Message(CommandLine, evKeyDown, kbUp, nil);
 end;

 procedure RunCommand(B: Boolean);
   var ST: SessionType;
    S: string; {//AK155}
 begin
{$IFNDEF Win32}
    if (PCommandLine(CommandLine)^.LineType in [ltOS2Window,ltOS2FullScreen]) then
     begin
       if PCommandLine(CommandLine)^.LineType = ltOS2FullScreen then
         ST := stOS2FullScreen
       else ST := stOS2Windowed;
       RunOS2Command(M, false, ST);
       CmdLine.StrModified := true;
       Message(CommandLine, evKeyDown, kbDown, nil);
       Exit;
     end;
{$ENDIF}
{AK155, см. dnutil.ExecCommandLine}
    S:='';CommandLine^.SetData(S);
{/AK155}
    if B then ExecString(@M, #13#10 + {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(ActiveDir) + '>' + {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(M))
     else ExecString(@M, '');
 end;

label ex;
begin
 fr:= CreateFileRec(FileName);
 S := fr^.FlName[true];
{$IFNDEF OS2}
 L := S;
{$ENDIF}
 FreeStr := '';
 M := '';
 if (ShiftState and (3 or kbAltShift) <> 0) or
    (not InExtFilter(S, Executables)
     {$IFNDEF OS2}and not InExtFilter(L, Executables){$ENDIF}) then
  begin
   if SearchExt(fr, M) then
   begin
   {PutHistory(true);}
    M := SwpDir+'$DN'+ItoS(DNNumber)+'$' + CmdExt + ' ' + FreeStr;
    RunCommand(false);
   {M := S; PutHistory(false);}
   {CmdDisabled := false;}
    GlobalMessage(evCommand, cmClearCommandLine, nil);
   end;
   goto ex;
  end;
{$IFNDEF Win32}
 M := S;
{$ELSE}
 M := {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(L);
{$ENDIF}
 PutHistory(false);
{$IFNDEF Win32}
 M := S;
{$ELSE}
 M := L;
{$ENDIF}

 if Pos(' ', M) <> 0 then {AK155}
{$IFDEF OS2}
   M := '""' + M + '""';
{$ELSE}
   M := '"' + M + '"';
{$ENDIF}

 RunCommand(true);
ex:
 DelFileRec(fr);
end;

end.
