{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.05/DOS
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
//      LFNизация выполнена Антоном Федоpовым aka DataCompBoy'ем.
//                  В честь Матаpыкиной Ульяны...
//////////////////////////////////////////////////////////////////////////}

{$o+}
UNIT DN1;

INTERFACE
uses Advance, Advance1, Advance2, Advance3, Advance4, Startup, Objects,
     Setups, DnUtil, Drivers, commands, dnApp, Messages, Lfn, Dos, FlPanelX,
     UserMenu, cmdline, filescol, views, lfncol, arcview, dnini, archiver;

Procedure InvalidateTempDir;
Procedure UpdateConfig;
Procedure CheckForOldFiles;
Procedure DoStartup;
{$IFDEF GRABPalette}
procedure GrabPalette;
{$ENDIF}

IMPLEMENTATION
{$I RunCmd.Inc}

        {-DataCompBoy-}
procedure InvalidateTempDir;
var
  I: Integer;
begin
  TempDir := SystemData.Temp;
  I := PosChar('%', TempDir);
  if I > 0 then
   begin
     Delete(TempDir, 1, I);
     I := PosChar('%', TempDir);
     if I > 0 then Delete(TempDir, I, 255);
     if TempDir <> '' then TempDir := GetEnv(TempDir);
   end;
  if TempDir = '' then TempDir := GetEnv('TEMP');
  if TempDir = '' then TempDir := GetEnv('TMP');
  if TempDir = '' then TempDir := SourceDir;
  if (TempDir <> '') and (not (TempDir[Byte(TempDir[0])] in ['\','/'])) then TempDir := TempDir + '\';
end;
        {-DataCompBoy-}

procedure UpdateConfig;
var
  OldSecurity: Boolean;
  TempInteger: Integer; {DataCompBoy}
  R: TRect;
begin
  InvalidateTempDir;
  OldSecurity := Security;
  smSVGALo := StoI(SystemData.Mode1);
  smSVGAHi := StoI(SystemData.Mode2);
  if smSVGALo = 0 then smSVGALo := StartupMode;
  if smSVGAHi = 0 then smSVGAHi := StartupMode;
  SystemData.Mode1 := ItoS(smSVGALo);
  SystemData.Mode2 := ItoS(smSVGAHi);
  Val(SaversData.Time, SkyDelay, TempInteger);   {DataCompBoy}
  If SkyDelay=0 then SkyDelay:=255; {DataCompBoy}

  {TempDir := SystemData.Temp;}

  MouseReverse := MouseData.Options and omsReverse <> 0;
  Security := SystemData.Options and ossShowHidden = 0;

  if OldSecurity xor Security then
   begin
    if Application <> nil then GlobalMessage(evCommand, cmPanelReread, nil);
   end;

  SetBlink(CurrentBlink);
  UpcaseInit := Off;
end;


        {-DataCompBoy-}
procedure CheckForOldFiles;
type TSearchDirSet=set of 0..2;
var OldFiles:array [0..8] of string;
    OldFileSearchDirs:array [0..8] of TSearchDirSet;
    OldFilesCount:integer;
    Dirs:array [0..2] of string;
    S:string; i,j:integer;
    procedure ProbeOldFile(Name:string; SearchDirs:TSearchDirSet);
    var k:integer;
    begin
        for k:=0 to 2 do if (k in SearchDirs) and ExistFile(Dirs[k]+Name)
        then begin
            OldFiles[OldFilesCount]:=Name;
            OldfileSearchDirs[OldFilesCount]:=SearchDirs;
            Inc(OldFilesCount);
            break
        end;
    end;
begin
    if not IgnoreOldFiles then ConfigModified:=True;
    IgnoreOldFiles:=True;
    Dirs[0]:=GetEnv('DNDLG');
    Dirs[1]:=SourceDir;
    Dirs[2]:=StartupDir;
    if Dirs[0]='' then Dirs[0]:=Dirs[1];
    for i:=0 to 2 do if not (Dirs[i][Byte(dirs[i][0])]in['\','/'])
                      then Dirs[i]:=Dirs[i]+'\';
    OldFilesCount:=0;
    ProbeOldFile('DN.DLG',[0,2]);
    ProbeOldFile('DNENG.DLG',[0,2]);
    ProbeOldFile('DNRUS.DLG',[0,2]);
    ProbeOldFile('DN.LNG',[0,2]);
    ProbeOldFile('DNENG.LNG',[0,2]);
    ProbeOldFile('DNRUS.LNG',[0,2]);
    ProbeOldFile('DN.HLP',[1,2]);
    ProbeOldFile('DNENG.HLP',[1,2]);
    ProbeOldFile('DNRUS.HLP',[1,2]);
    if OldFilesCount=0 then Exit;
    S:='';
    for i:=0 to OldFilesCount-1 do S:=S+#3+OldFiles[i]+#13;
    if MessageBox2(S+#13,GetString(dlOldFiles),nil,nil,
    mfWarning+mfYesButton+mfNoButton)=cmYes then begin
        for i:=0 to OldFilesCount-1 do for j:=0 to 2
        do if j in OldFileSearchDirs[i] then EraseFile(Dirs[j]+OldFiles[i]);
        SaveDsk; Application^.Done; ExecString(@NullStr)
    end else MessageBox(GetString(dlOldFilesNoWarn),nil,
    mfInformation+mfOkButton);
 end;
        {-DataCompBoy-}

        {-DataCompBoy-}
PROCEDURE DoStartup;
var
  SavePos, SPos1, INIdatapos: LongInt;

  function ReadConfig: LongInt;
  var
    S: TBufStream;
    CFGVer: Word;
    ID,L: Word;
    p: pointer;

    procedure SRead(var Buf);
    begin
      S.Read(Buf, L);
    end;

   Procedure GetVer;
    var i: byte;
        Chk: String;
    begin
     CfgVer:=0;
     For i:=NumSupportedConfigs downto 1 do
      begin
       S.Seek( 0 );
       S.Read( Chk[1], ConfigSigns[i].SignLen );
       Chk[ 0 ] := Char( ConfigSigns[i].SignLen );
       if Chk=ConfigSigns[i].Sign then
        begin CfgVer:=ConfigSigns[i].SignVer; Break; End;
      end;
    end;

  begin
    ReadConfig := -1;
    SPos1 := -1;
    INIdatapos := -1;
    S.Init(SourceDir+'DN'+GetEnv('DNCFG')+'.CFG', stOpenRead, 16384);
    if ( S.Status <> stOK ) or ( S.GetSize = 0 ) then begin S.Done; Exit; end;
    GetVer; If CfgVer=0 then begin S.Done; Exit end;
    while S.GetPos < S.GetSize do
      begin
        S.Status := stOK;
        S.Read(ID, SizeOf(Word));
        S.Read(L, SizeOf(Word));
        case ID of
          0: Break;
          cfgNewSystemData: SRead(SystemData);
             cfgSystemData: Begin
                             GetMem(p, SizeOf(TOldSystemData));
                             SRead(p^);
                             With TOldSystemData(p^) do begin
                              SystemData.Options := Options;
                              SystemData.Mode1   := Mode1;
                              SystemData.Mode2   := Mode2;
                              SystemData.Temp    := Temp;
                              move(Drives, SystemData.Drives, sizeof(Drives));
                             end;
                             FreeMem(p, SizeOf(TOldSystemData));
                            End;
            cfgStartupData: Begin
                             GetMem(p, SizeOf(TOldStartupData));
                             SRead(p^);
                             With TOldStartupData(p^) do begin
                              StartupData.Load:=Load;
                              StartupData.Unload:=Unload;
                              StartupData.Slice:=Slice;
                              StartupData.OvrSize:=OvrSize;
                             end;
                             FreeMem(p, SizeOf(TOldStartupData));
                            end;
         cfgNewStartupData: SRead(StartupData);
              cfgMouseData: SRead(MouseData);
          cfgShowScrollBar: SRead(ShowScrollBar);
          cfgInterfaceData: SRead(InterfaceData);
          cfgNewSaversData: S.Read(SaversData.Time, SizeOf(SaversData)-SizeOf(SaversData.Selected)*2);
             cfgSaversData: Begin
                             GetMem(p, SizeOf(TOldSaversData));
                             S.Read(TOldSaversData(p^).Time, SizeOf(TOldSaversData)-SizeOf((TOldSaversData(p^).Selected))*2);
                             With TOldSaversData(p^) do begin
                              SaversData.Time:=ItoS(Time);
                              SaversData.Mouse:=Mouse;
                              SaversData._:=_;
                             end;
                            End;
           cfgSystemColors: S.Read(SystemColors, SizeOf(SystemColors));
          cfgPanelDefaults: SRead(PanelDefaults);
         cfgEditorDefaults: SRead(EditorDefaults);
           cfgFFindOptions: SRead(FileFind.FindRec.Options);
              cfgTetrisRec: S.Read(TetrisRec, SizeOf(TetrisRec));
           cfgPrinterSetup: S.Read(RPrinterSetup, SizeOf(RPrinterSetup));
         cfgArcCustomMasks: S.Read(Startup.Archives, SizeOf(Startup.Archives));
            cfgCustomMasks: begin
                              S.Read(CustomMask1, SizeOf(CustomMask1));
                              S.Read(CustomMask2, SizeOf(CustomMask2));
                              S.Read(CustomMask3, SizeOf(CustomMask3));
                              S.Read(CustomMask4, SizeOf(CustomMask4));
                              S.Read(CustomMask5, SizeOf(CustomMask5));
                            end;
    cfgColumnsDefaultsDisk: S.Read(ColumnsDefaultsDisk,SizeOf(ColumnsDefaultsDisk));
    cfgColumnsDefaultsFind: S.Read(ColumnsDefaultsFind,SizeOf(ColumnsDefaultsFind));
    cfgColumnsDefaultsTemp: S.Read(ColumnsDefaultsTemp,SizeOf(ColumnsDefaultsTemp));
    cfgColumnsDefaultsArch: S.Read(ColumnsDefaultsArch,SizeOf(ColumnsDefaultsArch));
    cfgColumnsDefaultsArvd: S.Read(ColumnsDefaultsArvd,SizeOf(ColumnsDefaultsArvd));
         cfgColumnDefaults: begin
                             SRead(OldColumnsDefaults);
                             with OldColumnsDefaults do
                              begin
                               ColumnsDefaultsDisk.Params[1].Param:=DiskDrive shl 1;
                               ColumnsDefaultsFind.Params[1].Param:=FindDrive shl 1;
                               ColumnsDefaultsTemp.Params[1].Param:=Temp shl 1;
                               ColumnsDefaultsArch.Params[1].Param:=Arc shl 1;
                               ColumnsDefaultsArvd.Params[1].Param:=Arvid;
                              end;
                            end;
          cfgDriveInfoData: SRead(DriveInfoData);
            cfgCountryInfo: S.Read(CountryInfo, SizeOf(CountryInfo));
               cfgConfirms: S.Read(Confirms, SizeOf(Confirms));
                cfgUUEData: SRead(UUDecodeOptions);
           cfgMakeListFile: SRead(MakeListFileOptions);
           cfgTermDefaults: S.Read(TerminalDefaults, SizeOf(TerminalDefaults));
                cfgFMSetup: begin
                             GetMem(p, SizeOf(TOldFMSetup));
                             S.Read(P^, SizeOf(TOldFMSetup));
                             with TOldFMSetup((P^)) do
                              begin
                               Startup.FMSetup.Options:=Options;
                               Startup.FMSetup.Show:=Show;
                               Startup.FMSetup.Quick:=Quick;
                               Startup.FMSetup.TagChar:=TagChar;
                               Startup.FMSetup.DIZ:=DIZ;
                              end;
                             FreeMem(p, SizeOf(TOldFMSetup));
                            end;
             cfgNewFMSetup: S.Read(Startup.FMSetup, SizeOf(Startup.FMSetup));
{$IFDEF CDPLAYER}
               cfgCDParams: SRead(CDPlace);
{$ENDIF}
                  cfgBlink: begin
                              S.Read(CurrentBlink, SizeOf(CurrentBlink));
                              SetBlink(CurrentBlink);
                            end;
             cfgVGApalette: begin
                              SRead(VGA_Palette);
                      {Knave} if FadeDelay >0 then BlackPalette else
                              if (StartupData.Load and osuResetPalette <> 0) and VGASystem then
                                  SetPalette(VGA_Palette);
                            end;
           cfgDirsToChange: begin SPos1 := S.GetPos; S.Seek(S.GetPos+L); end;
                 cfgSavers: begin ReadConfig := S.GetPos; S.Seek(S.GetPos+L); end;
                cfgINIdata: begin
                                if L-sizeof(INIstoredtime)-sizeof(INIstoredsize)
                                =Ofs(iniparamblock_END)-Ofs(iniparamblock_START)
                                then begin
                                    S.Read(INIstoredtime,sizeof(INIstoredtime));
                                    S.Read(INIstoredsize,sizeof(INIstoredsize));
                                    INIdatapos:=S.GetPos;
                                    S.Seek(S.GetPos+L-sizeof(INIstoredtime)-sizeof(INIstoredsize))
                                end else S.Seek(S.GetPos+L)
                            end;
         cfgExtractOptions: SRead(ExtractWithPathNames);
         cfgIgnoreOldFiles: IgnoreOldFiles:=True;
            else S.Seek(S.GetPos+L);
        end;
      end;

    S.Done;
  end;

  procedure ReadSavers( Pos: LongInt );
  var
    S: TBufStream;
    I: Integer;
    CFGVer: Word;
    Chk: String;

   Procedure GetVer;
    var i: byte;
    begin
     CfgVer:=0;
     For i:=NumSupportedConfigs downto 1 do
      begin
       S.Seek( 0 );
       S.Read( Chk[1], ConfigSigns[i].SignLen );
       Chk[ 0 ] := Char( ConfigSigns[i].SignLen );
       if Chk=ConfigSigns[i].Sign then
        begin CfgVer:=ConfigSigns[i].SignVer; Break; End;
      end;
    end;

  begin
    I := FindParam('/C');
    if I = 0 then Chk := SourceDir + 'DN' + GetEnv('DNCFG') + '.CFG'
      else begin
             Chk := Copy(ParamStr(I), 3, 255);
             if PosChar('\', Chk) = 0 then Insert(SourceDir, Chk, 1);
           end;
    S.Init(Chk, stOpenRead, 16384);
    if ( S.Status <> stOK ) or ( S.GetSize = 0 ) then begin S.Done; Exit; end;
    GetVer; If CfgVer=0 then begin S.Done; Exit end;
    S.Seek( SavePos );
    If S.Status = 0 then SaversData.Selected.List := PTextCollection(S.Get);
    if SPos1 > 0 then
      begin
        S.Seek(SPos1);
        for I := 0 to 8 do
           DirsToChange[I] := S.ReadStr;
      end;
    S.Done;
  end;

  procedure SetOverlay;
  var S: String;
      I: LongInt;
  begin
{    InitExtraMem; }
    SwpDir := GetEnv('DNSWP');
    if SwpDir = '' then
      begin
        I := FindParam('/S');
        if I > 0 then SwpDir := Copy(ParamStr(I), 3, 255);
      end;
    if SwpDir = '' then SwpDir := TempDir;
    if not (SwpDir[Length(SwpDir)] in ['\','/']) then SwpDir := SwpDir+'\'; ClrIO;
    if RunFirst then EraseFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
  end;

  procedure ReadINI;
  var INIavailtime,INIavailsize:longint;
      S:TBufStream;
  begin
      if (not ProbeINI(INIavailtime,INIavailsize)) or (INIdatapos<0) or
      (INIavailtime>INIstoredtime) or (INIavailsize<>INIstoredsize) then begin
          InitIniEngine; {-$VIV start}
          LoadDnIniSettings;
          if DnIni.AutoSave then SaveDnIniSettings;
          DoneIniEngine; {-$VIV stop}
          ProbeINI(INIstoredtime,INIstoredsize);
          UpdateConfig; WriteConfig
      end else begin
          S.Init(SourceDir+'DN'+GetEnv('DNCFG')+'.CFG', stOpenRead, 16384);
          S.Seek(INIdatapos);
          S.Read(iniparamblock_START,Ofs(iniparamblock_END)-Ofs(iniparamblock_START));
          S.Done;
      end;
  end;

begin
  RegisterType( RTextCollection );
  IgnoreOldFiles:=False;
  SavePos := ReadConfig;

  if SavePos >= 0 then UpdateConfig else InvalidateTempDir;
  StdMouse := MouseData.Options and omsCursor <> 0;
  if OS210 then Executables := Executables + 'cmd'#0;
  if Chk4Dos then Executables := Executables + 'btm'#0;
  RunMenu := (StartupData.Load and osuAutoMenu <> 0);
  SetOverlay;
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT');
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.MNU');
  EraseFile(SwpDir+'$$$DN$$.LST');
  EraseFile(SwpDir+'$$$DN$$$.LST');
  If SavePos >= 0 then ReadSavers( SavePos );
  ReadINI;
end;
        {-DataCompBoy-}

{$IFDEF GRABPalette}
procedure GrabPalette;
  var F: Text;
      I: Integer;
begin
  Assign(F, 'Pallete.PAS');
  Rewrite(F);
  Write(F, '    ');
  FreeStr := Application^.GetPalette^;
  for I := 1 to Length(FreeStr) do
    begin
      Write(F, '#$', Hex2(Byte(FreeStr[I])));
      if I mod 16 = 0 then WriteLn(F, ' + ');
    end;
  WriteLn(F, ';');
  Close(F);
end;
{$ENDIF}

END.