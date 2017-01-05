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
{.$DEFINE GRABPalette}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

unit DN1;

interface

procedure InvalidateTempDir;
procedure UpdateConfig;
procedure CheckForOldFiles;
procedure DoStartup;

procedure RUN_IT;
{$IFDEF LINEPOSIT}
procedure Error(const FileName: String; LineNo, Addr, Code: LongInt);
{$ENDIF}

implementation
uses
  {$IFDEF DEBUGMEM}DebugMem, {$ENDIF} {Cat}
  {$IFDEF OS2}Os2Def, Os2Base, {AK155 for killer} {$ENDIF}
  {$IFDEF WIN95_HIGHPRIORITY}Windows, {Cat for SetPriority} {$ENDIF}
  advance, advance1, advance2, advance3, advance4, Startup, Defines,
   Streams,
  Setups, DNUtil, Drivers, Commands, DNApp, Messages, Lfn, Dos, FlPanelX,
  UserMenu, CmdLine, FilesCol, Views, ArcView, DnIni, CopyIni, Archiver,
  U_MyApp, Microed, ArchSet, advance6, RegAll, DnExec, Histries, Menus,
   VideoMan,
  fnotify,
  {$IFNDEF DPMI32}Killer, {$ENDIF}
  Tree
  ;

{AK155 Мало проверить, что имя временного каталога непусто, надо
еще проверить, что он существует, и что в нем можно создавать и
уничтожать файлы }
function BadTemp(var s: String): Boolean;
  var
    f: file;
  begin
  BadTemp := True;
  if  (s = '') then
    exit;
  if not (s[Length(s)] in ['\', '/']) then
    s := s+'\';
  ClrIO;
  if not PathExist(s) then
    exit;
  Assign(f, s+'$DNTEST.SWP');
  Rewrite(f);
  if IOResult = 0 then
    begin
    Close(f);
    Erase(f);
    { Под Win NT бывает и так, что создать файл можно, а удалить - нет}
    if IOResult = 0 then
      BadTemp := False;
    end;
  end { BadTemp };

{-DataCompBoy-}
procedure InvalidateTempDir;
  var
    I: integer;
  begin
  NoTempDir := False;
  TempDir := SystemData.Temp;
  I := PosChar('%', TempDir);
  if I > 0 then
    begin
    Delete(TempDir, 1, I);
    I := PosChar('%', TempDir);
    if I > 0 then
      Delete(TempDir, I, 255);
    TempDir := GetEnv(TempDir);
    end;
  if not BadTemp(TempDir) then
    exit;
  TempDir := GetEnv('TEMP');
  if not BadTemp(TempDir) then
    exit;
  TempDir := GetEnv('TMP');
  if not BadTemp(TempDir) then
    exit;
  TempDir := SourceDir;
  if not BadTemp(TempDir) then
    begin
    if TempDir[Length(TempDir)] <> '\' then
      TempDir := TempDir+'\';
    TempDir := TempDir+'TEMP\';
    MkDir(TempDir);
    ClrIO;
    if not BadTemp(TempDir) then
      exit;
    end;
  NoTempDir := True;
  end { InvalidateTempDir };
{-DataCompBoy-}

procedure UpdateConfig;
  var
    OldSecurity: Boolean;
    TempInteger: integer; {DataCompBoy}
    R: TRect;
  begin
  InvalidateTempDir;
  OldSecurity := Security;
  smSVGALo := StoI(SystemData.Mode1);
  smSVGAHi := StoI(SystemData.Mode2);
  if smSVGALo = 0 then
    smSVGALo := sm80x25;
  if smSVGAHi = 0 then
    smSVGAHi := sm80x25;
  SystemData.Mode1 := ItoS(smSVGALo);
  SystemData.Mode2 := ItoS(smSVGAHi);
  {$IFDEF SS}Val(SaversData.Time, SkyDelay, TempInteger); {$ENDIF}
  if SkyDelay = 0 then
    SkyDelay := 255; {DataCompBoy}

  {TempDir := SystemData.Temp;}

  MouseReverse := MouseData.Options and omsReverse <> 0;
  Security := SystemData.Options and ossShowHidden = 0;

  if OldSecurity xor Security then
    begin
    if Application <> nil then
      GlobalMessage(evCommand, cmPanelReread, nil);
    end;

  SetBlink(CurrentBlink);
  end { UpdateConfig };

{-DataCompBoy-}
procedure CheckForOldFiles;
  type
    TSearchDirSet = set of 0..2;
  var
    OldFiles: array[0..8] of String;
    OldFileSearchDirs: array[0..8] of TSearchDirSet;
    OldFilesCount: integer;
    Dirs: array[0..2] of String;
    S: String;
    i, j: integer;
  procedure ProbeOldFile(Name: String; SearchDirs: TSearchDirSet);
    var
      k: integer;
    begin
    for k := 0 to 2 do
      if  (k in SearchDirs) and ExistFile(Dirs[k]+Name)
      then
        begin
        OldFiles[OldFilesCount] := Name;
        OldFileSearchDirs[OldFilesCount] := SearchDirs;
        Inc(OldFilesCount);
        break
        end;
    end;
  begin { CheckForOldFiles }
  if not IgnoreOldFiles then
    ConfigModified := True;
  IgnoreOldFiles := True;
  Dirs[0] := GetEnv('DNDLG');
  Dirs[1] := SourceDir;
  Dirs[2] := StartupDir;
  if Dirs[0] = '' then
    Dirs[0] := Dirs[1];
  for i := 0 to 2 do
    if not (Dirs[i][Length(Dirs[i])] in ['\', '/'])
    then
      Dirs[i] := Dirs[i]+'\';
  OldFilesCount := 0;
  ProbeOldFile('DN.DLG', [0, 2]);
  ProbeOldFile('DNENG.DLG', [0, 2]);
  ProbeOldFile('DNRUS.DLG', [0, 2]);
  ProbeOldFile('DN.LNG', [0, 2]);
  ProbeOldFile('DNENG.LNG', [0, 2]);
  ProbeOldFile('DNRUS.LNG', [0, 2]);
  ProbeOldFile('DN.HLP', [1, 2]);
  ProbeOldFile('DNENG.HLP', [1, 2]);
  ProbeOldFile('DNRUS.HLP', [1, 2]);
  if OldFilesCount = 0 then
    exit;
  S := '';
  for i := 0 to OldFilesCount-1 do
    S := S+#3+OldFiles[i]+#13;
  if MessageBox2(S+#13, GetString(dlOldFiles), nil, nil,
      mfWarning+mfYesButton+mfNoButton) = cmYes
  then
    begin
    for i := 0 to OldFilesCount-1 do
      for j := 0 to 2
      do
        if j in OldFileSearchDirs[i] then
          EraseFile(Dirs[j]+OldFiles[i]);
    {SaveDsk; Exiting:=true; Application^.Done; ExecString(@NullStr)}
    ExecString(@NullStr, '')
    end
  else
    MessageBox(GetString(dlOldFilesNoWarn), nil,
      mfInformation+mfOKButton);
  end { CheckForOldFiles };
{-DataCompBoy-}

{-DataCompBoy-}
procedure DoStartup;
  var
    SavePos, SPos1, INIdatapos: LongInt;

    {JO}
  procedure ReadHighlite;
    var
      F: PTextReader;
    begin
    FileMode := $40;
    F := New(PTextReader, Init(SourceDir+'dnhgl.grp'));
    if F = nil then
      exit;
    if not F^.Eof then
      CustomMask1^.Filter := F^.GetStr;
    if not F^.Eof then
      CustomMask2^.Filter := F^.GetStr;
    if not F^.Eof then
      CustomMask3^.Filter := F^.GetStr;
    if not F^.Eof then
      CustomMask4^.Filter := F^.GetStr;
    if not F^.Eof then
      CustomMask5^.Filter := F^.GetStr;
    if not F^.Eof then
      CustomMask6^.Filter := F^.GetStr;
    if not F^.Eof then
      CustomMask7^.Filter := F^.GetStr;
    if not F^.Eof then
      CustomMask8^.Filter := F^.GetStr;
    if not F^.Eof then
      CustomMask9^.Filter := F^.GetStr;
    if not F^.Eof then
      CustomMask10^.Filter := F^.GetStr;
    if not F^.Eof then
      Startup.Archives^.Filter := F^.GetStr;
    Dispose(F, Done);
    end { ReadHighlite };
  {JO}

  function ReadConfig: LongInt;
    var
      S: TBufStream;
      CFGVer: AWord;
      ID: AWord;
      L: AWord;
      p: Pointer;
      I: integer;

    procedure SRead(var Buf);
      begin
      S.Read(Buf, l);
      end;

    procedure GetVer;
      var
        i: Byte;
        Chk: String;
      begin
      CFGVer := 0;
      for i := NumSupportedConfigs DownTo 1 do
        begin
        S.Seek(0);
        S.Read(Chk[1], ConfigSigns[i].SignLen);
        SetLength(Chk, ConfigSigns[i].SignLen);
        {Chk[0] := Char( ConfigSigns[i].SignLen );}
        if Chk = ConfigSigns[i].Sign then
          begin
          CFGVer := ConfigSigns[i].SignVer;
          if ConfigSigns[i].HavVer then
            S.Read(CFGVer, SizeOf(VersionWord));
          break;
          end;
        end;
      end;

    begin { ReadConfig: }
    ReadConfig := -1;
    INIdatapos := -1;
    S.Init(SourceDir+'DN'+GetEnv('DNCFG')+'.CFG', stOpenRead, 16384);
    if  (S.Status <> stOK) or (S.GetSize = 0) then
      begin
      S.Done;
      Virgin := True;
      exit;
      end;
    GetVer;
    {If (CfgVer=0) or (CfgVer>VersionWord) then begin}
    {JO - временно, в релизе вернём}
    if  (CFGVer = 0) or (CFGVer <> VersionWord) then
      begin
      S.Done;
      Virgin := True;
      exit
      end;
    while S.GetPos < S.GetSize do
      begin
      S.Status := stOK;
      {S.Read(ID, SizeOf(Word));
        S.Read(L, SizeOf(Word));}
      S.Read(ID, SizeOf(AWord));
      S.Read(L, SizeOf(AWord));

      case ID of
        0:
          begin
          {Virgin := True;}
          break;
          end;
        cfgNewSystemData:
          SRead(SystemData);
        cfgOld2SystemData:
          begin
          GetMem(p, SizeOf(TOld2SystemData));
          SRead(p^);
          with TOld2SystemData(p^) do
            begin
            SystemData.Options := Options;
            SystemData.Mode1 := Mode1;
            SystemData.Mode2 := Mode2;
            SystemData.Temp := Temp;
            {SystemData.LFNContainer := LFNContainer;}
            Move(Drives, SystemData.Drives, SizeOf(Drives));
            end;
          FreeMem(p, SizeOf(TOld2SystemData));
          end;
        cfgOldSystemData:
          begin
          GetMem(p, SizeOf(TOldSystemData));
          SRead(p^);
          with TOldSystemData(p^) do
            begin
            SystemData.Options := Options;
            SystemData.Mode1 := Mode1;
            SystemData.Mode2 := Mode2;
            SystemData.Temp := Temp;
            Move(Drives, SystemData.Drives, SizeOf(Drives));
            end;
          FreeMem(p, SizeOf(TOldSystemData));
          end;
        cfgSystemData:
          begin
          GetMem(p, SizeOf(TOld1SystemData));
          SRead(p^);
          with TOld1SystemData(p^) do
            begin
            SystemData.Options := Options;
            SystemData.Mode1 := Mode1;
            SystemData.Mode2 := Mode2;
            SystemData.Temp := Temp;
            Move(Drives, SystemData.Drives, SizeOf(Drives));
            end;
          FreeMem(p, SizeOf(TOld1SystemData));
          end;
        cfgStartupData:
          begin
          GetMem(p, SizeOf(TOldStartupData));
          SRead(p^);
          with TOldStartupData(p^) do
            begin
            StartupData.Load := Load;
            StartupData.Unload := Unload;
            end;
          FreeMem(p, SizeOf(TOldStartupData));
          end;
        cfgNewStartupData:
          SRead(StartupData);
        cfgMouseData:
          begin
          SRead(MouseData);
          XSens := MouseData.HSense;
          YSens := MouseData.VSense;
          end;
        cfgShowScrollBar:
          SRead(ShowScrollBar);
        cfgInterfaceData:
          SRead(InterfaceData);
        {$IFDEF SS}
        cfgNewSaversData:
          S.Read(SaversData.Time,
               SizeOf(SaversData)-SizeOf(SaversData.Selected)*2);
        cfgSaversData:
          begin
          GetMem(p, SizeOf(TOldSaversData));
          S.Read(TOldSaversData(p^).Time,






             SizeOf(TOldSaversData)-SizeOf((TOldSaversData(p^).Selected))*2);
          with TOldSaversData(p^) do
            begin
            SaversData.Time := ItoS(Time);
            SaversData.Mouse := Mouse;
            SaversData._ := _;
            end;
          end;
        {$ENDIF}
        cfgSystemColors:
          S.Read(SystemColors, SizeOf(SystemColors));
        cfgNewPanelDefaults:
          SRead(PanelDefaults);
        cfgPanelDefaults:
          begin
          SRead(PanelDefaults);
          case PanelDefaults.Sort of
            0:
              ;
            1:
              Inc(PanelDefaults.Sort);
            else {case}
              Inc(PanelDefaults.Sort, 2);
          end {case};
          end;
        cfgEditorDefaults:
          SRead(EditorDefaults);
        cfgOldEditorDefaults:
          begin
          GetMem(p, SizeOf(TOldEditorDefaultsData));
          SRead(p^);
          with TOldEditorDefaultsData(p^) do
            begin
            EditorDefaults.EdOpt := EdOpt;
            EditorDefaults.EdOpt2 := ebfHlt+ebfSmt;
            EditorDefaults.ViOpt := ViOpt;
            EditorDefaults.LM := LM;
            EditorDefaults.RM := RM;
            EditorDefaults.NewLine := NewLine;
            EditorDefaults.TabSize := TabSize;
            end;
          FreeMem(p, SizeOf(TOldEditorDefaultsData));
          end;
        cfgFFindOptions:
          SRead(FileFind.FindRec.Options);
        cfgTetrisRec:
          S.Read(TetrisRec, SizeOf(TetrisRec));
        {$IFDEF PrintManager}
        cfgPrinterSetup:
          S.Read(RPrinterSetup, SizeOf(RPrinterSetup));
        {$ENDIF}
        cfgArcCustomMasks:
          begin
          S.Read(Startup.Archives^.Filter,
            SizeOf(Startup.Archives^.Filter));
          end;

        cfgOldCustomMasks:
          begin
          S.Read(CustomMask1^.Filter,
            SizeOf(CustomMask1^.Filter));
          Replace(#0, ';', CustomMask1^.Filter);
          Delete(CustomMask1^.Filter, 1, 1);
          {DelFC(CustomMask1^.Filter);}
          SetLength(CustomMask1^.Filter, Length(CustomMask1^.Filter)-1);

          S.Read(CustomMask2^.Filter,
            SizeOf(CustomMask2^.Filter));
          Replace(#0, ';', CustomMask2^.Filter);
          Delete(CustomMask2^.Filter, 1, 1); {DelFC();}
          SetLength(CustomMask2^.Filter, Length(CustomMask2^.Filter)-1);

          S.Read(CustomMask3^.Filter,
            SizeOf(CustomMask3^.Filter));
          Replace(#0, ';', CustomMask3^.Filter);
          Delete(CustomMask3^.Filter, 1, 1);
          {DelFC(CustomMask3^.Filter);}
          SetLength(CustomMask3^.Filter, Length(CustomMask3^.Filter)-1);

          S.Read(CustomMask4^.Filter,
            SizeOf(CustomMask4^.Filter));
          Replace(#0, ';', CustomMask4^.Filter);
          Delete(CustomMask4^.Filter, 1, 1);
          {DelFC(CustomMask4^.Filter);}
          SetLength(CustomMask4^.Filter, Length(CustomMask4^.Filter)-1);

          S.Read(CustomMask5^.Filter,
            SizeOf(CustomMask5^.Filter));
          Replace(#0, ';', CustomMask5^.Filter);
          Delete(CustomMask5^.Filter, 1, 1);
          {DelFC(CustomMask5^.Filter);}
          SetLength(CustomMask5^.Filter, Length(CustomMask5^.Filter)-1);
          end;

        cfgOldCustomMasks2:
          begin
          S.Read(CustomMask6^.Filter,
            SizeOf(CustomMask6^.Filter)); {JO}
          Replace(#0, ';', CustomMask6^.Filter);
          Delete(CustomMask6^.Filter, 1, 1);
          {DelFC(CustomMask6^.Filter);}
          SetLength(CustomMask6^.Filter, Length(CustomMask6^.Filter)-1);

          S.Read(CustomMask7^.Filter,
            SizeOf(CustomMask7^.Filter));
          Replace(#0, ';', CustomMask7^.Filter);
          Delete(CustomMask7^.Filter, 1, 1);
          {DelFC(CustomMask7^.Filter);}
          SetLength(CustomMask7^.Filter, Length(CustomMask7^.Filter)-1);

          S.Read(CustomMask8^.Filter,
            SizeOf(CustomMask8^.Filter));
          Replace(#0, ';', CustomMask8^.Filter);
          Delete(CustomMask8^.Filter, 1, 1);
          {DelFC(CustomMask8^.Filter);}
          SetLength(CustomMask8^.Filter, Length(CustomMask8^.Filter)-1);

          S.Read(CustomMask9^.Filter,
            SizeOf(CustomMask9^.Filter));
          Replace(#0, ';', CustomMask9^.Filter);
          Delete(CustomMask9^.Filter, 1, 1);
          {DelFC(CustomMask9^.Filter);}
          SetLength(CustomMask9^.Filter, Length(CustomMask9^.Filter)-1);

          S.Read(CustomMask10^.Filter,
            SizeOf(CustomMask10^.Filter)); {JO}
          Replace(#0, ';', CustomMask10^.Filter);
          Delete(CustomMask10^.Filter, 1, 1);
          {DelFC(CustomMask10^.Filter);}
          SetLength(CustomMask10^.Filter, Length(CustomMask10^.Filter)-1);
          end;

        cfgCustomMasks:
          begin
          S.Read(CustomMask1^.Filter,
            SizeOf(CustomMask1^.Filter));

          S.Read(CustomMask2^.Filter,
            SizeOf(CustomMask2^.Filter));

          S.Read(CustomMask3^.Filter,
            SizeOf(CustomMask3^.Filter));

          S.Read(CustomMask4^.Filter,
            SizeOf(CustomMask4^.Filter));

          S.Read(CustomMask5^.Filter,
            SizeOf(CustomMask5^.Filter));
          end;

        cfgCustomMasks2:
          begin
          S.Read(CustomMask6^.Filter,
            SizeOf(CustomMask6^.Filter)); {JO}

          S.Read(CustomMask7^.Filter,
            SizeOf(CustomMask7^.Filter));

          S.Read(CustomMask8^.Filter,
            SizeOf(CustomMask8^.Filter));

          S.Read(CustomMask9^.Filter,
            SizeOf(CustomMask9^.Filter));

          S.Read(CustomMask10^.Filter,
            SizeOf(CustomMask10^.Filter)); {JO}
          end;

        cfgColumnsDefaultsDisk:
          S.Read(ColumnsDefaultsDisk, SizeOf(ColumnsDefaultsDisk));
        cfgColumnsDefaultsFind:
          S.Read(ColumnsDefaultsFind, SizeOf(ColumnsDefaultsFind));
        cfgColumnsDefaultsTemp:
          S.Read(ColumnsDefaultsTemp, SizeOf(ColumnsDefaultsTemp));
        cfgColumnsDefaultsArch:
          S.Read(ColumnsDefaultsArch, SizeOf(ColumnsDefaultsArch));
        cfgColumnsDefaultsArvd:
          S.Read(ColumnsDefaultsArvd, SizeOf(ColumnsDefaultsArvd));
        cfgColumnDefaults:
          begin
          SRead(OldColumnsDefaults);
          with OldColumnsDefaults do
            begin
            ColumnsDefaultsDisk.Params[1].Param := DiskDrive shl 1;
            ColumnsDefaultsFind.Params[1].Param := FindDrive shl 1;
            ColumnsDefaultsTemp.Params[1].Param := Temp shl 1;
            ColumnsDefaultsArch.Params[1].Param := Arc shl 1;
            ColumnsDefaultsArvd.Params[1].Param := Arvid;
            end;
          end;
        cfgDriveInfoData:
          SRead(DriveInfoData);
        cfgCountryInfo:
          begin
          S.Read(CountryInfo, SizeOf(CountryInfo));
          InitUpcase;
          end;
        cfgConfirms:
          begin
          S.Read(Confirms, SizeOf(Confirms));
          {if CfgVer<$15106 then
                              Confirms:=Confirms or cfFmtOs2Warning;}
          end;
        cfgUUEData:
          SRead(UUDecodeOptions);
        cfgMakeListFile:
          SRead(MakeListFileOptions);
        cfgTermDefaults:
          S.Read(TerminalDefaults, SizeOf(TerminalDefaults));
        cfgOldFMSetup:
          begin
          GetMem(p, SizeOf(TOldFMSetup));
          S.Read(p^, SizeOf(TOldFMSetup));
          with TOldFMSetup((p^)) do
            begin
            Startup.FMSetup.Options :=
              Options and $001FF+
              Options and $0FE00 shl 1;
            Startup.FMSetup.Show := Show;
            Startup.FMSetup.Quick := Quick;
            Startup.FMSetup.TagChar := TagChar;
            Startup.FMSetup.DIZ := DIZ;
            end;
          FreeMem(p, SizeOf(TOldFMSetup));
          end;
        cfgNewFMSetup:
          begin
          S.Read(Startup.FMSetup, SizeOf(Startup.FMSetup));
          with Startup.FMSetup do
            Options :=
              Options and $001FF+
              Options and $0FE00 shl 1;
          end;
        cfgFMSetup:
          S.Read(Startup.FMSetup, SizeOf(Startup.FMSetup));
        cfgBlink:
          begin
          S.Read(CurrentBlink, SizeOf(CurrentBlink));
          SetBlink(CurrentBlink);
          end;
        cfgVGAPalette:
          begin
          SRead(VGA_palette);
          if  (StartupData.Load and osuResetPalette <> 0)
            and VGASystem
          then
            SetPalette(VGA_palette);
          end;
        cfgDefaultArchiver:
          SRead(DefaultArchiver);
        cfgDefaultArchiverMode:
          SRead(DefaultArcMode);
        cfgDirsToChange:
          for I := 0 to 8 do
            DirsToChange[I] := S.ReadStr;
        {$IFDEF SS}
        cfgSavers:
          SaversData.Selected.List := PTextCollection(S.Get);
        {$ENDIF}
        {cfgINIcrc:  SRead(INIstoredcrc);}
        cfgINIdata:
          begin
          if L-SizeOf(INIstoredtime)-SizeOf(INIstoredsize)
            = Ofs(iniparamblock_END)-Ofs(iniparamblock_START)
          then
            begin
            S.Read(INIstoredtime, SizeOf(INIstoredtime));
            S.Read(INIstoredsize, SizeOf(INIstoredsize));
            INIdatapos := S.GetPos;
            S.Seek(S.GetPos+L-SizeOf(INIstoredtime)-SizeOf(INIstoredsize))
            end
          else
            S.Seek(S.GetPos+L);
          end;
        cfgExtractOptions:
          S.Read(UnarchiveOpt, SizeOf(UnarchiveOpt)); {JO}
        cfgChangeCaseOptions:
          SRead(ChangeNamesCaseOptions);
        cfgAppPalette:
          SRead(appPalette);
        cfgComareDirsOptions:
          SRead(ComareDirsOptions);
        cfgIgnoreOldFiles:
          IgnoreOldFiles := True;
        else {case}
          S.Seek(S.GetPos+L);
      end {case};
      end;
    S.Done;
    Security := SystemData.Options and ossShowHidden = 0;

    SystemDataOpt := SystemData.Options;
    InterfaceDataOpt := InterfaceData.Options;
    DriveInfoType := InterfaceData.DrvInfType;
    FMSetupOpt := Startup.FMSetup.Options;
    EditorDefaultsOpt := EditorDefaults.EdOpt;
    EditorDefaultsOpt2 := EditorDefaults.EdOpt2;
    ViewerOpt := EditorDefaults.ViOpt;
    StartupDataLoad := StartupData.Load;
    StartupDataUnload := StartupData.Unload;
    StartupDataSlice2 := StartupData.Slice2;
    ConfirmsOpt := Confirms;
    CopyLimit := SystemData.CopyLimitBuf;
    ForceDefaultArchiver := SystemData.ForceDefArch;
    QDirs1 := CnvString(DirsToChange[0]);
    QDirs2 := CnvString(DirsToChange[1]);
    QDirs3 := CnvString(DirsToChange[2]);
    QDirs4 := CnvString(DirsToChange[3]);
    QDirs5 := CnvString(DirsToChange[4]);
    QDirs6 := CnvString(DirsToChange[5]);
    QDirs7 := CnvString(DirsToChange[6]);
    QDirs8 := CnvString(DirsToChange[7]);
    QDirs9 := CnvString(DirsToChange[8]);
    end { ReadConfig: };

  procedure SetOverlay;
    var
      S: String;
      I: LongInt;
    begin
    SwpDir := GetEnv('DNSWP');
    if BadTemp(SwpDir) then
      begin
      I := FindParam('/S');
      if I > 0 then
        SwpDir := Copy(ParamStr(I), 3, MaxStringLength);
      end;
    if BadTemp(SwpDir) and NoTempDir then
      SwpDir := ''
    else
      SwpDir := TempDir;
    {$IFDEF OS_DOS}
    if RunFirst then
      EraseFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
    {$ENDIF}
    end;

  procedure ReadIni;
    var
      INIavailtime, INIavailsize, INIavailcrc: LongInt;
    begin
    if ProbeINI(INIavailtime, INIavailsize, INIavailcrc) then
      begin {ini есть}
      LoadDnIniSettings;
      DoneIniEngine;
      CopyIniVarsToCfgVars;
      if  (INIdatapos >= 0) and
          (INIavailtime = INIstoredtime) and (INIavailsize = INIstoredsize)
      then
        exit; { ini не изменился, читать не нужно }
      if DnIni.AutoSave then
        SaveDnIniSettings(nil);
      ConfigModified := True;
      end
    else
      SaveDnIniSettings(nil); {создаем ini}
    ProbeINI(INIstoredtime, INIstoredsize, INIstoredcrc);
    {новые дата размер}
    end { ReadIni };

  begin { DoStartup }
  InitMaskData;
  (*  RegisterType( RTextCollection );*)
  IgnoreOldFiles := False;

  SavePos := ReadConfig;
  ReadHighlite; {JO}
  UpdateConfig;

  MouseVisible := MouseData.Options and omsCursor <> 0;
  if OS2exec or (opSys and opWNT <> 0)
  then
    Executables.Filter := Executables.Filter+';cmd';
  {$IFDEF OS_DOS}
  if Chk4Dos
  then
    Executables.Filter := Executables.Filter+';btm';
  {$ENDIF}
  MakeTMaskData(Executables);
  MakeTMaskData(CustomMask1^);
  MakeTMaskData(CustomMask2^);
  MakeTMaskData(CustomMask3^);
  MakeTMaskData(CustomMask4^);
  MakeTMaskData(CustomMask5^);
  MakeTMaskData(CustomMask6^);
  MakeTMaskData(CustomMask7^);
  MakeTMaskData(CustomMask8^);
  MakeTMaskData(CustomMask9^);
  MakeTMaskData(CustomMask10^);
  MakeTMaskData(Archives^);
  MakeTMaskData(AddArchives); {JO: для поиска в архивах}

  RunMenu := (StartupData.Load and osuAutoMenu <> 0);
  SetOverlay;
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT');
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.MNU');
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'.LST');
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.LST');
  {$IFDEF OS2}
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.CMD');
  {$ENDIF}
  ReadIni;
  {$IFDEF SS}Val(SaversData.Time, SkyDelay, integer(SPos1)); {$ENDIF}
  if SkyDelay = 0 then
    SkyDelay := 255; { X-Man }
  {ExecDNAutoexec;}
  {Cat}
  if DnIni.AutoRefreshPanels then
    NotifyInit;
  {/Cat}
  end { DoStartup };
{-DataCompBoy-}

procedure CrLf;
  {$IFNDEF OS_DOS}
  begin
  Writeln;
  end;
  {$ELSE}
  assembler;
asm
  MOV  DL,0DH
  MOV  AH,2
  INT  21H
  MOV  DL,0AH
  MOV  AH,2
  INT  21H
end;
{$ENDIF}

procedure RUN_IT;
  var
    ShiftRec: record
      ScrH, CurY: Byte
      end absolute FreeStr; { just to reduce DS }
    {$IFDEF OS2}
  var
    RegRec: ExceptionRegistrationRecord; {AK155 for killer}
    {this data MUST be located in stack}
    {$ENDIF}
  var
    Ev: TEvent;
  begin
  {if memAvail < 100000 then begin}
  {WriteLn(#10#13'Not enough memory for Navigator.');}
  {WriteLn(      'Please check if 400K memory is available');}
  {Halt(203);}
  {end}
  {else}
  Writeln
    ('Dos Navigator /2 Open Source  '+VersionName+'  Based on DN by RIT Labs'
    )
  ;

  {$IFDEF WIN95_HIGHPRIORITY}
  if opSys = opWin then
    {Win9x}
    SetPriorityClass(GetCurrentProcess, High_Priority_Class);
  {Cat: чтоб не тормозило}
  {$ENDIF}

  Randomize;

  LoaderSeg := 0;
  LSliceCnt := -3;

  RunFirst := True;

  if DDTimer > 0 then
    DDTimer := Get100s-DDTimer;

  TempBounds.Assign(0, 0, 0, 0);

  RegisterAll;
  DoStartup;
  {$IFNDEF DPMI32}
  SetKillHandler {$IFDEF OS2}(RegRec) {$ENDIF}; {AK155}
  {$ENDIF}
  with ShiftRec do
    begin
    if  (CurY = ScrH) and (InterfaceData.Options and ouiHideStatus = 0)
    then
      begin
      CrLf;
      end
    end;

  SetBlink(CurrentBlink);

  (* InitLFNCol; *)
  MyApplication.Init;

  if RunFirst then
    ShowIniErrors;

  if RunFirst then
    if  (StartupData.Load and osuKillHistory <> 0) then
      ClearHistories;
  {$IFDEF OS_DOS}
  if not RunFirst then
    EraseFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
  {$ENDIF}
  if RunFirst then
    begin
    if  (Message(@MyApplication, evBroadcast, cmLookForPanels, nil) = nil)
    then
      Message(@MyApplication, evCommand, cmFirstTimePanel, nil);

    FreeStr[1] := Char(FindParam('/P'));
    if  (FreeStr[1] > #0) then
      LoadPalFromFile(Copy(ParamStr(Byte(FreeStr[1])), 3,
         MaxStringLength));
    if Virgin then
      Message(@MyApplication, evCommand, cmAbout, nil); {JO}
    if NoTempDir then
      begin
      CreateDirInheritance(TempDir, False);
      NoTempDir := False;
      end; {JO}
    ExecDNAutoexec;
    end;

  if DDTimer > 0 then
    begin
    Ev.What := evCommand;
    Ev.Command := cmShowTimeInfo;
    MyApplication.PutEvent(Ev);
    end;

  if not IgnoreOldFiles then
    CheckForOldFiles;
  with MyApplication do
    begin
    Lock;
    MenuBar^.MakeFirst;
    Desktop^.MakeFirst;
    Clock^.MakeFirst;
    UnLock;
    end;
  {$IFDEF OS_DOS}
  w95QuitInit; {Gimly}
  {$ENDIF}
  MyApplication.Run;
  {$IFNDEF DPMI32}
  UnsetKillHandler {$IFDEF OS2}(RegRec) {$ENDIF}; {AK155}
  {$ENDIF}
  ClearIniErrors;
  GlobalMessage(evCommand, cmKillUsed, nil);
  TottalExit := True;
  MyApplication.Done;
  {Cat}
  FreeTMaskData(Executables);
  FreeTMaskData(CustomMask1^);
  FreeTMaskData(CustomMask2^);
  FreeTMaskData(CustomMask3^);
  FreeTMaskData(CustomMask4^);
  FreeTMaskData(CustomMask5^);
  FreeTMaskData(CustomMask6^);
  FreeTMaskData(CustomMask7^);
  FreeTMaskData(CustomMask8^);
  FreeTMaskData(CustomMask9^);
  FreeTMaskData(CustomMask10^);
  FreeTMaskData(Startup.Archives^);
  FreeTMaskData(AddArchives); {JO}
  Dispose(CustomMask1);
  Dispose(CustomMask2);
  Dispose(CustomMask3);
  Dispose(CustomMask4);
  Dispose(CustomMask5);
  Dispose(CustomMask6);
  Dispose(CustomMask7);
  Dispose(CustomMask8);
  Dispose(CustomMask9);
  Dispose(CustomMask10);
  Dispose(Startup.Archives);
  if StringCache <> nil then
    Dispose(StringCache, Done);
  DoneHistories;
  if ColorIndexes <> nil then
    FreeMem(ColorIndexes, 2+ColorIndexes^.ColorSize);
  {/Cat}
  end { RUN_IT };

{Cat}
{$IFDEF LINEPOSIT}
procedure Error(const FileName: String; LineNo, Addr, Code: LongInt);
  begin
  if Addr <> 0 then
    begin
    SourceFileName := @FileName;
    SourceLineNo := LineNo;
    ErrorAddr := Pointer(Addr);
    ExitCode := Code;
    EndFatalError;
    end;
  end;
{$ENDIF}
{/Cat}

end.
