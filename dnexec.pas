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
unit DnExec;

interface

uses
  Files,
  UserMenu, Startup, Objects, FilesCol, Commands, TitleSet
  {$IFDEF OS2}, Dn2PmApi {$ENDIF} {AK155 для перерисовки иконки}
  ;

procedure ExecString(s: PString; WS: String);
procedure ExecStringRR(s: PString; WS: String; RR: boolean); {JO}
{JO:  отличается от ExecString наличием булевской переменной RR, которая}
{     указывает, перечитывать панель после выполнения или нет           }

function SearchExt(FileRec: PFileRec; var HS: String): boolean;
  {DataCompBoy}
function ExecExtFile(const ExtFName: String; UserParams: PUserParams;
    SIdx: TStrIdx): boolean; {DataCompBoy}
procedure ExecFile(const FileName: String); {DataCompBoy}

procedure AnsiExec(const Path: String; const ComLine: AnsiString);
  {JO}

const
  fExec: boolean = False; {выполняется внешняя программа}

implementation

uses
  {$IFDEF OS2}
  Os2Base, DnIni,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  DNUtil, advance, DNApp, advance1, Lfn,
  Dos, advance3, FlPanelX, CmdLine, Views, advance2, Drivers,
    advance4,
  VideoMan, Memory, VpSysLow, VPSysLo2,
  {$IFDEF UserSaver}
  UserSavr,
  {$ENDIF}
  Messages, Strings;

{$IFDEF OS_DOS}
procedure SaveDsk;
  begin
    ClrIO;
    if ((opSys <> opDOS) or (StartupData.Unload and osuAutosave = 0))
      and not (TottalExit)
    then
      PDNApplication(Application)^.SaveDesktop(SwpDir+'DN'+ItoS(
        DNNumber)+'.SWP');
  end;
{$ENDIF}

{JO}
{ AnsiExec - аналог DOS.Exec , который в качестве }
{ коммандлайна использует строку типа Ansistring }
{ и соответственно не имеет ограничения в 255 символов}
procedure AnsiExec(const Path: String; const ComLine: AnsiString);
  var
    PathBuf: array[0..255] of Char;
    Ans1: AnsiString;
  begin
    Ans1 := ComLine+#0;

    SysTVKbdDone;
      {JO: см. VpSysLo2 ; если этого не делать - при вызове     }
    {    консольных программ вроде архиваторов без командного }
    {    процессора или с "неправильным" командным процессором}
    {    (например, 4OS2) внешние программы не видят ввода с  }
    {    клавиатуры                                           }

    {Cat: чтобы ДН в Win9x не тормозил, повышаем ему приоритет. Однако, незачем
        оставлять этот повышенный приоритет при запуске внешних программ}
    {$IFDEF WIN95_HIGHPRIORITY}
    if SysPlatformId = 1 then{Win9x}
      begin
        SetPriorityClass(GetCurrentProcess, Normal_Priority_Class);
        DOSError := SysExecute(StrPCopy(PathBuf, Path), PChar(Ans1),
          nil, ExecFlags = efAsync, nil, -1, -1, -1);
        SetPriorityClass(GetCurrentProcess, High_Priority_Class);
      end
    else{WinNT}
      {$ENDIF}
      {/Cat}
      DOSError := SysExecute(StrPCopy(PathBuf, Path), PChar(Ans1),
        nil, ExecFlags = efAsync, nil, -1, -1, -1);

    SysTVKbdInit;
      {Cat: в OS/2: боремся с интерпретацией Ctrl-C как Ctrl-Break
                      в WinNT: боремся с пропаданием мышиного курсора}
  end { AnsiExec };
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
{Результат - код подсистемы для Win32 PE, или 100 для Win16 NE,
 или 0 для прочих }
function Win32Program(s: PString): SmallWord;
  const
    PETag = $00004550; {'PE'#0#0}
    NETag = $454E; {'NE'}
  var
    F: file;
    PathEnv: String;
    Dir: DirStr;
    Name: NameStr;
    ext: ExtStr;
    RealName: String;
    NewExeOffs: SmallWord;
    NewHeader: record
      signature: longInt;
      dummy1: array[1..16] of byte;
      SizeOfOptionalHeader: SmallWord;
      Characteristics: SmallWord;
      {OptionalHeader}
      dummy2: array[1..68] of byte;
      Subsystem: SmallWord;
      end;
    l: longInt;
  begin { Win32Program }
    Result := 0;
    RealName := s^;
    if (RealName[1] = '"') and ((RealName[Length(RealName)] = '"'))
    then
      RealName := Copy(RealName, 2, Length(RealName)-2);
    DelRight(RealName);
    FSplit(RealName, Dir, Name, ext);
    UpStr(ext);
    if ext <> '.EXE' then
      exit;
    FileMode := Open_Access_ReadOnly or open_share_DenyNone;
    ClrIO;
    Assign(F, RealName);
    Reset(F, 1);
    if (IOResult <> 0) and (Dir = '') then
      begin
        PathEnv := GetEnv('PATH');
        RealName := FSearch(s^, PathEnv);
        if RealName = '' then
          exit;
        Assign(F, RealName);
        Reset(F, 1);
        if IOResult <> 0 then
          exit; {вообще-то, так быть не должно, раз мы ее нашли}
      end;
    Seek(F, $3C);
    BlockRead(F, NewExeOffs, 2, l);
    if (NewExeOffs = 0) or (l <> 2) then
      begin
        Close(F); {Cat}
        exit;
      end;
    Seek(F, NewExeOffs);
    BlockRead(F, NewHeader, SizeOf(NewHeader), l);
    Close(F);
    with NewHeader do
      begin
        if SmallWord(signature) = NETag then
          Result := 100
        else if (l >= 70) and (signature = PETag) and (
            SizeOfOptionalHeader >= 70)
        then
          Result := Subsystem;
      end;
  end { Win32Program };

{$IFDEF Win32}
function GUIProgram(s: PString): boolean;
  begin
    Result := Win32Program(s) in [2 {IMAGE_SUBSYSTEM_WINDOWS_GUI},
      100];
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
function GUIProgram(s: PString): boolean;
  var
    SS: String;
    Flags: longInt;
    l: integer;
  begin
    SS := s^;
    l := 0;
    while SS[1+l] = '"' do
      Inc(l);
    SS := Copy(SS, 1+l, Length(SS)-2*l)+#0;
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
procedure ExecStringRR(s: PString; WS: String; RR: boolean);
  var
    i: integer;
    EV: TEvent;
    X, Y: SmallWord; {Cat}
    ScreenSize: TSysPoint; {Cat}

  begin
    DoneSysError;
    DoneEvents;
    DoneVideo;
    DoneDOSMem;
    DoneMemory;
    {$IFDEF OS_DOS} asm cld; mov eax,3; int $10; end; {$ENDIF}
    SwapVectors;
    if TimerMark then
      DDTimer := Get100s
    else
      DDTimer := 0;
    if WS <> '' then
      Writeln(WS);
    SetTitle(s^);
    fExec := True;
    {$IFNDEF DPMI32}
    if GUIProgram(s) then
      {$IFDEF OS2}
      s^:= 'start /PGM '+s^
    else
      case Win32Program(s) of
        2 {IMAGE_SUBSYSTEM_WINDOWS_GUI}:
          s^:= ExecWin32GUI+' '+s^;
        3 {IMAGE_SUBSYSTEM_WINDOWS_CUI}:
          s^:= ExecWin32CUI+' '+s^;
      end {case};
      {$ELSE}
      begin
        if opSys = opWNT then
          s^:= 'start "" '+s^
        else
          s^:= 'start '+s^
      end
      {$ENDIF}
      ;
    {$ENDIF}
    AnsiExec(GetEnv('COMSPEC'), '/c '+s^);
    fExec := False;
    {AK155, Cat: чтобы комстрока и меню не налазили на вывод}
    SysGetCurPos(X, Y);
    if InterfaceData.Options and ouiHideStatus = 0 then
      Inc(Y);
    if X <> 0 then
      Writeln;
    SysTvGetScrMode(@ScreenSize, True);
    if Y >= ScreenSize.Y then
      Writeln;
    {/AK155, Cat}
    if TimerMark then
      begin
        DDTimer := Get100s-DDTimer;
        EV.What := evCommand;
        EV.Command := cmShowTimeInfo;
        EV.InfoPtr := nil;
        Application^.PutEvent(EV);
      end;
    i := DOSError;
    ClrIO;
    SwapVectors;
    EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.LST'); {DataCompBoy}
    InitDOSMem;
    InitMemory;
    InitVideo;
    InitEvents;
    InitSysError;
    {$IFDEF OS2}
    DN_WinSetTitleAndIcon('DN/2', @DN_IconFile[1]);
    {AK155 без этого иконка в заголовке окна не восстанавливается }
    {$ENDIF}
    Application^.Redraw;
    {JO}
    if RR then
      begin
        GlobalMessage(evCommand, cmPanelReread, nil);
        GlobalMessage(evCommand, cmRereadInfo, nil);
      end;
    {/JO}

    {AK155 без этого курсор комстроки не становится на место}
    {$IFDEF Win32}
    if (CommandLine <> nil) then
      begin
        SysTVSetCurPos(-2, -2);
        SysCtrlSleep(1); // это для нитки курсора
        CommandLine^.UpDate;
      end;
    {$ENDIF}
    {/AK155}
  end { ExecStringRR };
{-DataCompBoy-}

{JO}
procedure ExecString(s: PString; WS: String);
  begin
    ExecStringRR(s, WS, True);
  end;
{/JO}
{-DataCompBoy-}
function SearchExt(FileRec: PFileRec; var HS: String): boolean;
  var
    AllRight: boolean;
    F: PTextReader;
    F1: lText;
    s, s1: String;
    BgCh, EnCh: Char;
    EF, First: boolean;
    i: integer;
    Local: boolean;
    FName: String;
    UserParam: tUserParams;
    D: TMaskData;
    {$IFDEF OS2}
    WriteEcho: boolean;
    {$ENDIF}
  label RL;

  begin
    FillChar(D, SizeOf(D), 0);
    First := True;
    Message(Desktop, evBroadcast, cmGetUserParams, @UserParam);
    UserParam.active := FileRec;
    FName := FileRec^.FlName[True];
    {lGetDir(0, ActiveDir);}
      {Cat:warn закомментировал это в процессе отлова багов, но надо будет проверить, не добавил ли новых}
    SearchExt := False;
    Local := True;
    F := New(PTextReader, Init('DN.EXT'));
    if F = nil then
      begin
RL:
        Local := False;
        F := New(PTextReader, Init(SourceDir+'DN.EXT'));
      end;
    if F = nil then
      exit;
    AllRight := False;
    BgCh := '{';
    EnCh := '}';
    Abort := False;
    EF := False;
    if PShootState and 8 > 0 then
      begin
        BgCh := '[';
        EnCh := ']';
      end
    else if PShootState and 3 > 0 then
      begin
        BgCh := '(';
        EnCh := ')';
      end;
    while (not F^.Eof) and (not AllRight) do
      begin
        s := F^.GetStr;
        if s[1] <> ' ' then
          begin
            i := PosChar(BgCh, s);
            if (i = 0) or (s[i+1] = BgCh) then
              continue;
            s1 := Copy(s, 1, i-1);
            DelLeft(s1);
            DelRight(s1);
            if s1[1] <> ';' then
              begin
                D.Filter := s1;
                MakeTMaskData(D);
                if InExtFilter(FName, D) then
                  begin
                    lAssignText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$'+
                      CmdExt);
                    ClrIO;
                    lRewriteText(F1);
                    if IOResult <> 0 then
                      begin
                        Dispose(F, Done);
                        FreeTMaskData(D); {Cat}
                        exit;
                      end;
                    {$IFNDEF OS2}
                    Writeln(F1.t, '@echo off');
                    {$ELSE}
                    WriteEcho := True;
                    {$ENDIF}
                    System.Delete(s, 1, PosChar(BgCh, s));
                    repeat
                      Replace(']]', #0, s);
                      Replace('))', #1, s);
                      Replace('}}', #2, s);
                      DelLeft(s);
                      DelRight(s);
                      if s[Length(s)] = EnCh then
                        begin
                          SetLength(s, Length(s)-1);
                          EF := True;
                          if s <> '' then
                            begin
                              Replace(#0, ']', s);
                              Replace(#1, ')', s);
                              Replace(#2, '}', s);
                              s := MakeString(s, @UserParam, False,
                                nil);
                              HS := s;
                              {$IFDEF OS2}
                              {JO: под осью если строка на REXX'е или Perl'е, то не нужно добавлять @Echo off}
                              if WriteEcho and (Copy(s, 1, 2) <>
                                  '/*') and (Copy(s, 1, 2) <> '#!')
                              then
                                Writeln(F1.t, '@Echo off');
                              WriteEcho := False;
                              {$ENDIF}
                              Writeln(F1.t, s);
                              break
                            end;
                        end;
                      if s <> '' then
                        begin
                          Replace(#0, ']', s);
                          Replace(#1, ')', s);
                          Replace(#2, '}', s);
                          if (BgCh <> '[') then
                            s := MakeString(s, @UserParam, False,
                              nil);
                          if First and (BgCh <> '[') then
                            HS := s;
                          {$IFDEF OS2}
                          {JO: под осью если строка на REXX'е или Perl'е, то не нужно добавлять @Echo off}
                          if WriteEcho and (Copy(s, 1, 2) <> '/*')
                              and (Copy(s, 1, 2) <> '#!')
                          then
                            Writeln(F1.t, '@Echo off');
                          WriteEcho := False;
                          {$ENDIF}
                          Writeln(F1.t, s);
                          First := False;
                        end;
                      if (F^.Eof) then
                        break;
                      if not EF then
                        s := F^.GetStr;
                    until (IOResult <> 0) or Abort or EF;
                    Close(F1.t);
                    AllRight := True;
                  end;
              end;
          end;
      end;
    Dispose(F, Done);
    {D.Filter:=''; MakeTMaskData(D);}
    FreeTMaskData(D); {Cat}
    if not EF and not Abort and Local then
      goto RL;
    if EF and (BgCh = '[') then
      begin
        EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.MNU');
        lRenameText(F1, SwpDir+'$DN'+ItoS(DNNumber)+'$.MNU');
        EF := ExecUserMenu(False);
        if not EF then
          lEraseText(F1);
      end;
    SearchExt := not Abort and EF;
    FreeTMaskData(D); {Cat}
  end { SearchExt };
{-DataCompBoy-}

{-DataCompBoy-}
function ExecExtFile(const ExtFName: String; UserParams: PUserParams;
    SIdx: TStrIdx): boolean;
  var
    F: PTextReader;
    s, s1: String;
    FName, Lfn: String;
    Event: TEvent;
    i, j: integer;
    Success, CD: boolean;
    Local: boolean;
    D: TMaskData;
  label 1, 1111, RepeatLocal;

  begin
    FillChar(D, SizeOf(D), 0);
    ExecExtFile := False;
    FileMode := $40;
    Local := True;
    Lfn := UserParams^.active^.FlName[True];
    if CharCount('.', Lfn) = 0 then
      Lfn := Lfn+'.';
    FName := UserParams^.active^.FlName[True xor InvLFN];

    F := New(PTextReader, Init(ExtFName));

    if F = nil then
      begin
RepeatLocal:
        Local := False;
        F := New(PTextReader, Init(SourceDir+ExtFName));
      end;
    if F = nil then
      exit;
    while not F^.Eof do
      begin
        s := F^.GetStr;
        DelLeft(s);
        s1 := fDelLeft(Copy(s, 1, pred(PosChar(':', s))));
        if (s1 = '') or (s1[1] = ';') then
          continue;
        D.Filter := s1;
        MakeTMaskData(D);
        if InExtFilter(FName, D) or InExtFilter(Lfn, D) then
          goto 1111;
      end;
    ExecExtFile := False;
    Dispose(F, Done);
    {D.Filter := ''; MakeTMaskData(D);}
    FreeTMaskData(D); {Cat}
    if Local then
      goto RepeatLocal;
    FreeTMaskData(D); {Cat}
    exit;
1111:
    Delete(s, 1, succ(Length(s1)));
    Dispose(F, Done);
    if not Application^.Valid(cmQuit) then
      begin
        FreeTMaskData(D); {Cat}
        exit;
      end;
    ClrIO;
    s1 := '';
    s := MakeString(s, UserParams, False, @S1);
    if s1 <> ''
    then
      TempFile := '!'+s1+'|'+MakeNormName(UserParams^.active^.Owner^,
        Lfn)
    else if TempFile <> ''
    then
      TempFile := MakeNormName(UserParams^.active^.Owner^, Lfn);
    {if TempFile <> '' then SaveDsk;}
    TempFileSWP := TempFile;
    TempFile := '';
    if Abort then
      begin
        FreeTMaskData(D); {Cat}
        exit;
      end;
    if s[1] = '*' then
      Delete(s, 1, 1); {DelFC(S);}
    lGetDir(0, s1);
    {$IFDEF OS_DOS}
    if UpStrg(MakeNormName(lfGetLongFileName(UserParams^.active^.
        Owner^), '.')) <>
      UpStrg(MakeNormName(s1, '.'))
    then
      begin
        DirToChange := s1;
        lChDir(lfGetLongFileName(UserParams^.active^.Owner^));
      end;
    {$ELSE}
    if UpStrg(MakeNormName(UserParams^.active^.Owner^, '.')) <>
      UpStrg(MakeNormName(s1, '.'))
    then
      begin
        DirToChange := s1;
        lChDir(UserParams^.active^.Owner^);
      end;
    {$ENDIF}
    ExecExtFile := True;
    Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
    {$IFDEF UserSaver}
    InsertUserSaver(False); {JO}
    {$ENDIF}
    ExecString(@S, '');
    if TempFileSWP <> '' then
      Message(Application, evCommand, cmRetrieveSwp, nil);
  end { ExecExtFile };
{-DataCompBoy-}

{-DataCompBoy-}
procedure ExecFile(const FileName: String);
  var
    s,
    {$IFNDEF OS2}
    l,
    {$ENDIF}
    M: String;
    fr: PFileRec;

  procedure PutHistory(B: boolean);
    begin
      if M = '' then
        exit;
      CmdLine.Str := M;
      CmdLine.StrModified := True;
      CmdDisabled := B;
      Message(CommandLine, evKeyDown, kbDown, nil);
      Message(CommandLine, evKeyDown, kbUp, nil);
    end;

  procedure RunCommand(B: boolean);
    var
      ST: SessionType;
      s: String; {//AK155}
    begin
      {$IFNDEF Win32}
      if (PCommandLine(CommandLine)^.LineType in [ltOS2Window,
          ltOS2FullScreen])
      then
        begin
          if PCommandLine(CommandLine)^.LineType = ltOS2FullScreen
          then
            ST := stOS2FullScreen
          else
            ST := stOS2Windowed;
          RunOS2Command(M, False, ST);
          CmdLine.StrModified := True;
          Message(CommandLine, evKeyDown, kbDown, nil);
          exit;
        end;
      {$ENDIF}
      {AK155, см. dnutil.ExecCommandLine}
      s := '';
      CommandLine^.SetData(s);
      {/AK155}
      if B then
        ExecString(@M, #13#10+ {$IFDEF RecodeWhenDraw}CharToOemStr
          {$ENDIF}(ActiveDir)+'>'+ {$IFDEF RecodeWhenDraw}
          CharToOemStr {$ENDIF}(M))
      else
        ExecString(@M, '');
    end { RunCommand };

  label ex;
  begin { ExecFile }
    fr := CreateFileRec(FileName);
    s := fr^.FlName[True];
    {$IFNDEF OS2}
    l := s;
    {$ENDIF}
    FreeStr := '';
    M := '';
    if (ShiftState and (3 or kbAltShift) <> 0) or
      (not InExtFilter(s, Executables)
      {$IFNDEF OS2} and not InExtFilter(l, Executables) {$ENDIF})
    then
      begin
        if SearchExt(fr, M) then
          begin
            {PutHistory(true);}
            M := SwpDir+'$DN'+ItoS(DNNumber)+'$'+CmdExt+' '+FreeStr;
            RunCommand(False);
            {M := S; PutHistory(false);}
            {CmdDisabled := false;}
            GlobalMessage(evCommand, cmClearCommandLine, nil);
          end;
        goto ex;
      end;
    {$IFNDEF Win32}
    M := s;
    {$ELSE}
    M := {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(l);
    {$ENDIF}
    PutHistory(False);
    {$IFNDEF Win32}
    M := s;
    {$ELSE}
    M := l;
    {$ENDIF}

    if Pos(' ', M) <> 0 then{AK155}
      M := '"'+M+'"';

    RunCommand(True);
ex:
    DelFileRec(fr);
  end { ExecFile };

end.
