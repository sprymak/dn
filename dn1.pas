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

UNIT DN1;

INTERFACE

Procedure InvalidateTempDir;
Procedure UpdateConfig;
Procedure CheckForOldFiles;
Procedure DoStartup;
{$IFDEF GRABPalette}
procedure GrabPalette;
{$ENDIF}

procedure RUN_IT;
{$IFDEF LINEPOSIT}
procedure ERROR(const FileName: String; LineNo, Addr, Code: LongInt);
{$ENDIF}

IMPLEMENTATION
uses
  {$IFDEF DEBUGMEM} DebugMem, {$ENDIF} {Cat}
  {$IFDEF OS2} Os2Def, Os2Base,  {AK155 for killer} {$ENDIF}
  {$IFDEF WIN95_HIGHPRIORITY} Windows,  {Cat for SetPriority} {$ENDIF}
  Advance, Advance1, Advance2, Advance3, Advance4, Startup, Objects,
  Setups, DnUtil, Drivers, commands, dnApp, Messages, Lfn, Dos, FlPanelX,
  UserMenu, cmdline, filescol, views, {$IFNDEF OS2}LFNCol,{$ENDIF} arcview, dnini, archiver,
  U_MyApp, Microed, ArchSet, Advance6, RegAll, DnExec, Histries, Menus, VideoMan,
{$IFDEF FileNotify}
  {$IFDEF Win32} FNoteW32, {$ENDIF} {$IFDEF OS2} FNoteOS2, {$ENDIF} {Cat}
{$ENDIF}
  {$IFNDEF DPMI32} Killer, {$ENDIF}
  {$IFDEF CDPLAYER} CDPlayer, {$ENDIF}
  {$IFDEF DPMI} DPMI, {$ENDIF}
  {$IFDEF VIRTUALPASCAL} {$IFDEF Win32} VpSysLow, {AK155 for RecodeAnsiNames} {$ENDIF}
  {$ELSE} ExtraMem, {$ENDIF}
  Tree;

{AK155 ���� �஢����, �� ��� �६������ ��⠫��� ������, ����
�� �஢����, �� �� �������, � �� � ��� ����� ᮧ������ �
㭨�⮦��� 䠩�� }
function BadTemp(var s: string): boolean;
  var
    f: file;
  begin
  BadTemp := true;
  if (s = '') then exit;
  if not (s[Length(s)] in ['\','/']) then s := s+'\';
  ClrIO;
  if not PathExist(s) then exit;
  Assign(f, s + '$DNTEST.SWP'); rewrite(f);
  if IOResult = 0 then
    begin
    Close(f); Erase(f);
     { ��� Win NT �뢠�� � ⠪, �� ᮧ���� 䠩� �����, � 㤠���� - ���}
    if IOResult = 0 then
      BadTemp := false;
    end;
  end;

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
     TempDir := GetEnv(TempDir);
     if not BadTemp(TempDir) then exit;
   end;
  TempDir := GetEnv('TEMP');
  if not BadTemp(TempDir) then exit;
  TempDir := GetEnv('TMP');
  if not BadTemp(TempDir) then exit;
  TempDir := SourceDir;
  if not BadTemp(TempDir) then exit;
  NoTempDir := True;
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
{$IFDEF SS}Val(SaversData.Time, SkyDelay, TempInteger);{$ENDIF}
  If SkyDelay=0 then SkyDelay:=255; {DataCompBoy}

  {TempDir := SystemData.Temp;}

  MouseReverse := MouseData.Options and omsReverse <> 0;
  Security := SystemData.Options and ossShowHidden = 0;

  if OldSecurity xor Security then
   begin
    if Application <> nil then GlobalMessage(evCommand, cmPanelReread, nil);
   end;

  SetBlink(CurrentBlink);
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
    for i:=0 to 2 do if not (Dirs[i][Length(dirs[i])]in['\','/'])
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
       {SaveDsk; Exiting:=true; Application^.Done; ExecString(@NullStr)}
        ExecString(@NullStr,'')
    end else MessageBox(GetString(dlOldFilesNoWarn),nil,
    mfInformation+mfOkButton);
 end;
        {-DataCompBoy-}

        {-DataCompBoy-}
PROCEDURE DoStartup;
var
  SavePos, SPos1, INIdatapos: LongInt;

{JO}
  procedure ReadHighlite;
   var F: PTextReader;
  begin
   FileMode := $40;
   F := New(PTextReader, Init(SourceDir+'dnhgl.grp'));
   if F = nil then Exit;
   if not F^.EOF then CustomMask1^.Filter := F^.GetStr;
   if not F^.EOF then CustomMask2^.Filter := F^.GetStr;
   if not F^.EOF then CustomMask3^.Filter := F^.GetStr;
   if not F^.EOF then CustomMask4^.Filter := F^.GetStr;
   if not F^.EOF then CustomMask5^.Filter := F^.GetStr;
   if not F^.EOF then CustomMask6^.Filter := F^.GetStr;
   if not F^.EOF then CustomMask7^.Filter := F^.GetStr;
   if not F^.EOF then CustomMask8^.Filter := F^.GetStr;
   if not F^.EOF then CustomMask9^.Filter := F^.GetStr;
   if not F^.EOF then CustomMask10^.Filter := F^.GetStr;
   if not F^.EOF then Startup.Archives^.Filter := F^.GetStr;
   Dispose(F, Done);
  end;
{JO}

  function ReadConfig: LongInt;
  var
    S: TBufStream;
    CFGVer: AWord;
    ID: AWord;
    L: AWord;
    p: pointer;
    I: integer;

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
       SetLength(Chk, ConfigSigns[i].SignLen); {Chk[0] := Char( ConfigSigns[i].SignLen );}
       if Chk=ConfigSigns[i].Sign then begin
        CfgVer:=ConfigSigns[i].SignVer;
        if ConfigSigns[i].HavVer then S.Read(CFGVer, SizeOf(VersionWord));
        Break;
       End;
      end;
    end;

  begin
    ReadConfig := -1;
    INIdatapos := -1;
    S.Init(SourceDir+'DN'+GetEnv('DNCFG')+'.CFG', stOpenRead, 16384);
    if ( S.Status <> stOK ) or ( S.GetSize = 0 ) then
      begin
        S.Done;
        {$IFDEF VIRTUALPASCAL}
        Virgin := True;
        {$ENDIF}
        Exit;
      end;
    GetVer;
   {If (CfgVer=0) or (CfgVer>VersionWord) then begin} {JO - �६����, � ५��� ����}
    If (CfgVer=0) or (CfgVer<>VersionWord) then begin
     S.Done;
     {$IFDEF VIRTUALPASCAL} Virgin := True; {$ENDIF}
     Exit
    end;
    while S.GetPos < S.GetSize do
      begin
        S.Status := stOK;
       {S.Read(ID, SizeOf(Word));
        S.Read(L, SizeOf(Word));}
        S.Read(ID, SizeOf(AWord));
        S.Read(L, SizeOf(AWord));

        case ID of
          0: begin {$IFDEF VIRTUALPASCAL}
                  {Virgin := True;}
                   {$ENDIF} Break; end;
          cfgNewSystemData: SRead(SystemData);
         cfgOld2SystemData: Begin
                             GetMem(p, SizeOf(TOld2SystemData));
                             SRead(p^);
                             With TOld2SystemData(p^) do begin
                              SystemData.Options := Options;
                              SystemData.Mode1   := Mode1;
                              SystemData.Mode2   := Mode2;
                              SystemData.Temp    := Temp;
                             {SystemData.LFNContainer := LFNContainer;}
                              move(Drives, SystemData.Drives, sizeof(Drives));
                             end;
                             FreeMem(p, SizeOf(TOld2SystemData));
                            End;
          cfgOldSystemData: Begin
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
             cfgSystemData: Begin
                             GetMem(p, SizeOf(TOld1SystemData));
                             SRead(p^);
                             With TOld1SystemData(p^) do begin
                              SystemData.Options := Options;
                              SystemData.Mode1   := Mode1;
                              SystemData.Mode2   := Mode2;
                              SystemData.Temp    := Temp;
                              move(Drives, SystemData.Drives, sizeof(Drives));
                             end;
                             FreeMem(p, SizeOf(TOld1SystemData));
                            End;
            cfgStartupData: Begin
                             GetMem(p, SizeOf(TOldStartupData));
                             SRead(p^);
                             With TOldStartupData(p^) do begin
                              StartupData.Load:=Load;
                              StartupData.Unload:=Unload;
                              {$IFNDEF VIRTUALPASCAL}
                              StartupData.Slice:=Slice;
                              StartupData.OvrSize:=OvrSize;
                              {$ENDIF}
                             end;
                             FreeMem(p, SizeOf(TOldStartupData));
                            end;
         cfgNewStartupData: SRead(StartupData);
              cfgMouseData: begin
                             SRead(MouseData);
                             XSens := MouseData.HSense;
                             YSens := MouseData.VSense;
                            end;
          cfgShowScrollBar: SRead(ShowScrollBar);
          cfgInterfaceData: SRead(InterfaceData);
{$IFDEF SS}
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
{$ENDIF}
           cfgSystemColors: S.Read(SystemColors, SizeOf(SystemColors));
       cfgNewPanelDefaults: SRead(PanelDefaults);
          cfgPanelDefaults: begin
                             SRead(PanelDefaults);
                             Case PanelDefaults.Sort of
                              0: ;
                              1: inc(PanelDefaults.Sort);
                              else Inc(PanelDefaults.Sort, 2);
                             end;
                            end;
         cfgEditorDefaults: SRead(EditorDefaults);
      cfgOldEditorDefaults: begin
                             GetMem(p, SizeOf(TOldEditorDefaultsData));
                             SRead(p^);
                             With TOldEditorDefaultsData(p^) do begin
                              EditorDefaults.EdOpt   := EdOpt;
                              EditorDefaults.EdOpt2  := ebfHlt+ebfSmt;
                              EditorDefaults.ViOpt   := ViOpt;
                              EditorDefaults.LM      := LM;
                              EditorDefaults.RM      := RM;
                              EditorDefaults.NewLine := NewLine;
                              EditorDefaults.TabSize := TabSize;
                             end;
                             FreeMem(p, SizeOf(TOldEditorDefaultsData));
                            end;
           cfgFFindOptions: SRead(FileFind.FindRec.Options);
              cfgTetrisRec: S.Read(TetrisRec, SizeOf(TetrisRec));
{$IFDEF PrintManager}
           cfgPrinterSetup: S.Read(RPrinterSetup, SizeOf(RPrinterSetup));
{$ENDIF}
         cfgArcCustomMasks: begin
                             S.Read(Startup.Archives^.Filter,
                                    SizeOf(Startup.Archives^.Filter));
                            end;

         cfgOldCustomMasks: begin
                              S.Read(CustomMask1^.Filter,
                                     SizeOf(CustomMask1^.Filter));
                              Replace(#0,';',CustomMask1^.Filter);
                              Delete(CustomMask1^.Filter, 1, 1); {DelFC(CustomMask1^.Filter);}
                              SetLength(CustomMask1^.Filter, Length(CustomMask1^.Filter)-1);

                              S.Read(CustomMask2^.Filter,
                                     SizeOf(CustomMask2^.Filter));
                              Replace(#0,';',CustomMask2^.Filter);
                              Delete(CustomMask2^.Filter, 1, 1); {DelFC();}
                              SetLength(CustomMask2^.Filter, Length(CustomMask2^.Filter)-1);

                              S.Read(CustomMask3^.Filter,
                                     SizeOf(CustomMask3^.Filter));
                              Replace(#0,';',CustomMask3^.Filter);
                              Delete(CustomMask3^.Filter, 1, 1); {DelFC(CustomMask3^.Filter);}
                              SetLength(CustomMask3^.Filter, Length(CustomMask3^.Filter)-1);

                              S.Read(CustomMask4^.Filter,
                                     SizeOf(CustomMask4^.Filter));
                              Replace(#0,';',CustomMask4^.Filter);
                              Delete(CustomMask4^.Filter, 1, 1); {DelFC(CustomMask4^.Filter);}
                              SetLength(CustomMask4^.Filter, Length(CustomMask4^.Filter)-1);

                              S.Read(CustomMask5^.Filter,
                                     SizeOf(CustomMask5^.Filter));
                              Replace(#0,';',CustomMask5^.Filter);
                              Delete(CustomMask5^.Filter, 1, 1); {DelFC(CustomMask5^.Filter);}
                              SetLength(CustomMask5^.Filter, Length(CustomMask5^.Filter)-1);
                            end;

        cfgOldCustomMasks2: begin
                              S.Read(CustomMask6^.Filter,
                                     SizeOf(CustomMask6^.Filter)); {JO}
                              Replace(#0,';',CustomMask6^.Filter);
                              Delete(CustomMask6^.Filter, 1, 1); {DelFC(CustomMask6^.Filter);}
                              SetLength(CustomMask6^.Filter, Length(CustomMask6^.Filter)-1);

                              S.Read(CustomMask7^.Filter,
                                     SizeOf(CustomMask7^.Filter));
                              Replace(#0,';',CustomMask7^.Filter);
                              Delete(CustomMask7^.Filter, 1, 1); {DelFC(CustomMask7^.Filter);}
                              SetLength(CustomMask7^.Filter, Length(CustomMask7^.Filter)-1);

                              S.Read(CustomMask8^.Filter,
                                     SizeOf(CustomMask8^.Filter));
                              Replace(#0,';',CustomMask8^.Filter);
                              Delete(CustomMask8^.Filter, 1, 1); {DelFC(CustomMask8^.Filter);}
                              SetLength(CustomMask8^.Filter, Length(CustomMask8^.Filter)-1);

                              S.Read(CustomMask9^.Filter,
                                     SizeOf(CustomMask9^.Filter));
                              Replace(#0,';',CustomMask9^.Filter);
                              Delete(CustomMask9^.Filter, 1, 1); {DelFC(CustomMask9^.Filter);}
                              SetLength(CustomMask9^.Filter, Length(CustomMask9^.Filter)-1);

                              S.Read(CustomMask10^.Filter,
                                     SizeOf(CustomMask10^.Filter));{JO}
                              Replace(#0,';',CustomMask10^.Filter);
                              Delete(CustomMask10^.Filter, 1, 1); {DelFC(CustomMask10^.Filter);}
                              SetLength(CustomMask10^.Filter, Length(CustomMask10^.Filter)-1);
                            end;

            cfgCustomMasks: begin
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

           cfgCustomMasks2: begin
                              S.Read(CustomMask6^.Filter,
                                     SizeOf(CustomMask6^.Filter)); {JO}

                              S.Read(CustomMask7^.Filter,
                                     SizeOf(CustomMask7^.Filter));

                              S.Read(CustomMask8^.Filter,
                                     SizeOf(CustomMask8^.Filter));

                              S.Read(CustomMask9^.Filter,
                                     SizeOf(CustomMask9^.Filter));

                              S.Read(CustomMask10^.Filter,
                                     SizeOf(CustomMask10^.Filter));{JO}
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
            cfgCountryInfo: begin
                             S.Read(CountryInfo, SizeOf(CountryInfo));
                             InitUpcase;
                            end;
               cfgConfirms: begin
                             S.Read(Confirms, SizeOf(Confirms));
                            {if CfgVer<$15106 then
                              Confirms:=Confirms or cfFmtOs2Warning;}
                            end;
                cfgUUEData: SRead(UUDecodeOptions);
           cfgMakeListFile: SRead(MakeListFileOptions);
           cfgTermDefaults: S.Read(TerminalDefaults, SizeOf(TerminalDefaults));
             cfgOldFMSetup: begin
                             GetMem(p, SizeOf(TOldFMSetup));
                             S.Read(P^, SizeOf(TOldFMSetup));
                             with TOldFMSetup((P^)) do
                              begin
                               Startup.FMSetup.Options:=
                                Options and $001FF +
                                Options and $0FE00 shl 1;
                               Startup.FMSetup.Show:=Show;
                               Startup.FMSetup.Quick:=Quick;
                               Startup.FMSetup.TagChar:=TagChar;
                               Startup.FMSetup.DIZ:=DIZ;
                              end;
                             FreeMem(p, SizeOf(TOldFMSetup));
                            end;
             cfgNewFMSetup: begin
                             S.Read(Startup.FMSetup, SizeOf(Startup.FMSetup));
                             With Startup.FMSetup do
                              Options:=
                                Options and $001FF +
                                Options and $0FE00 shl 1;
                            end;
                cfgFMSetup: S.Read(Startup.FMSetup, SizeOf(Startup.FMSetup));
{$IFDEF CDPLAYER}
               cfgCDParams: SRead(CDPlace);
{$ENDIF}
                  cfgBlink: begin
                              S.Read(CurrentBlink, SizeOf(CurrentBlink));
                              SetBlink(CurrentBlink);
                            end;
             cfgVGApalette: begin
                              SRead(VGA_Palette);
                              if (StartupData.Load and osuResetPalette <> 0)
                                 and VGASystem
                               then SetPalette(VGA_Palette);
                            end;
        cfgDefaultArchiver: SRead(DefaultArchiver);
    cfgDefaultArchiverMode: SRead(DefaultArcMode);
           cfgDirsToChange: for I := 0 to 8 do DirsToChange[I]:=S.ReadStr;
{$IFDEF SS}
                 cfgSavers: SaversData.Selected.List := PTextCollection(S.Get);
{$ENDIF}
                {cfgINIcrc:  SRead(INIstoredcrc);}
                cfgINIdata: begin
                                if L-sizeof(INIstoredtime)-sizeof(INIstoredsize)
                                =Ofs(iniparamblock_END)-Ofs(iniparamblock_START)
                                then begin
                                    S.Read(INIstoredtime,sizeof(INIstoredtime));
                                    S.Read(INIstoredsize,sizeof(INIstoredsize));
                                    INIdatapos:=S.GetPos;
                                    S.Seek(S.GetPos+L-sizeof(INIstoredtime)-sizeof(INIstoredsize))
                                end else S.Seek(S.GetPos+L);
                            end;
         cfgExtractOptions: SRead(ExtractWithPathNames);
      cfgChangeCaseOptions: SRead(ChangeNamesCaseOptions);
         cfgIgnoreOldFiles: IgnoreOldFiles:=True;
            else S.Seek(S.GetPos+L);
        end;
      end;

    S.Done;
    Security := SystemData.Options and ossShowHidden = 0;
    HandleChDirCommand := ((SystemData.Options{$IFDEF VIRTUALPASCAL}shl 3{$ENDIF}) and ossHandleChDirCommand) <> 0;

    SystemDataOpt      := SystemData.Options;
    InterfaceDataOpt   := InterfaceData.Options;
    DriveInfoType      := InterfaceData.DrvInfType;
    FMSetupOpt         := Startup.FMSetup.Options;
    EditorDefaultsOpt  := EditorDefaults.EdOpt;
    EditorDefaultsOpt2 := EditorDefaults.EdOpt2;
    ViewerOpt          := EditorDefaults.ViOpt;
    StartupDataLoad    := StartupData.Load;
    StartupDataUnload  := StartupData.Unload;
    StartupDataSlice2  := StartupData.Slice2;
    ConfirmsOpt        := Confirms;
    CopyLimit          := SystemData.CopyLimitBuf;
    ForceDefaultArchiver := SystemData.ForceDefArch;
    QDirs1             := CnvString(DirsToChange[0]);
    QDirs2             := CnvString(DirsToChange[1]);
    QDirs3             := CnvString(DirsToChange[2]);
    QDirs4             := CnvString(DirsToChange[3]);
    QDirs5             := CnvString(DirsToChange[4]);
    QDirs6             := CnvString(DirsToChange[5]);
    QDirs7             := CnvString(DirsToChange[6]);
    QDirs8             := CnvString(DirsToChange[7]);
    QDirs9             := CnvString(DirsToChange[8]);
  end;

  procedure SetOverlay;
  var S: String;
      I: LongInt;
  begin
    SwpDir := GetEnv('DNSWP');
    if BadTemp(SwpDir) then
      begin
        I := FindParam('/S');
        if I > 0 then SwpDir := Copy(ParamStr(I), 3, MaxStringLength);
      end;
    if BadTemp(SwpDir) and NoTempDir then
      SwpDir := ''
    else SwpDir := TempDir;
    {$IFDEF OS_DOS}
    if RunFirst then EraseFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
    {$ENDIF}
  end;

  procedure ReadINI;
  var INIavailtime,INIavailsize,INIavailcrc:longint;
      S:TBufStream;
      I: Byte;
      VirginIni: Boolean; {JO}
  begin
      VirginIni := not ProbeINI(INIavailtime,INIavailsize,INIavailcrc);
      if VirginIni or
         (INIdatapos<0) or
         (INIavailtime<>INIstoredtime) or
         (INIavailsize<>INIstoredsize) {or
         (INIavailcrc <>INIstoredcrc )} then begin
          {-$VIV start}
          LoadDnIniSettings;
          if DnIni.AutoSave then SaveDnIniSettings(nil);
          DoneIniEngine; {-$VIV stop}
          ProbeINI(INIstoredtime,INIstoredsize,INIstoredcrc);

          if Virgin and not VirginIni then
            begin
              SystemData.Options        := SystemDataOpt;
              InterfaceData.Options     := InterfaceDataOpt;
              InterfaceData.DrvInfType  := DriveInfoType;
              Startup.FMSetup.Options   := FMSetupOpt;
              EditorDefaults.EdOpt      := EditorDefaultsOpt;
              EditorDefaults.EdOpt2     := EditorDefaultsOpt2;
              EditorDefaults.ViOpt      := ViewerOpt;
              StartupData.Load          := StartupDataLoad;
              StartupData.Unload        := StartupDataUnload;
              StartupData.Slice2        := StartupDataSlice2;
              Confirms                  := ConfirmsOpt;
              SystemData.CopyLimitBuf   := CopyLimit;
              SystemData.ForceDefArch   := ForceDefaultArchiver;
              for I := 0 to 8 do DisposeStr(DirsToChange[I]);
              DirsToChange[0]           := NewStr(QDirs1);
              DirsToChange[1]           := NewStr(QDirs2);
              DirsToChange[2]           := NewStr(QDirs3);
              DirsToChange[3]           := NewStr(QDirs4);
              DirsToChange[4]           := NewStr(QDirs5);
              DirsToChange[5]           := NewStr(QDirs6);
              DirsToChange[6]           := NewStr(QDirs7);
              DirsToChange[7]           := NewStr(QDirs8);
              DirsToChange[8]           := NewStr(QDirs9);
            end;

          if HandleChDirCommand then SystemData.Options := SystemData.Options or (ossHandleChDirCommand {$IFDEF VIRTUALPASCAL}shr 3{$ENDIF})
            else SystemData.Options := SystemData.Options xor (ossHandleChDirCommand {$IFDEF VIRTUALPASCAL}shr 3{$ENDIF});
          UpdateConfig; WriteConfig
      end else begin
          S.Init(SourceDir+'DN'+GetEnv('DNCFG')+'.CFG', stOpenRead, 16384);
          S.Seek(INIdatapos);
          S.Read(iniparamblock_START,Ofs(iniparamblock_END)-Ofs(iniparamblock_START));
          S.Done;
      end;
{$IFDEF Win32}
      RecodeAnsiNames := RecodeCyrillicNames; {AK155}

    {$IFNDEF RecodeWhenDraw}
      if not RecodeAnsiNames then
        Windows.SetFileApisToOEM
           {AK155 18-10-2001
            �᫨ �� ������ ������� 䮪�ᮢ, � � 䠩���� ࠡ�⠥� ����
            � ����஢�� OEM, ����� ��⥬� ��४������, �᫨ �� ����.
           }
      else
    {$ENDIF}
        Windows.SetFileApisToANSI;
{$ENDIF}
  end;

begin
  InitMaskData;
(*  RegisterType( RTextCollection );*)
  IgnoreOldFiles:=False;

  SavePos := ReadConfig;
  ReadHighlite; {JO}
  UpdateConfig;

  MouseVisible := MouseData.Options and omsCursor <> 0;
  if OS2exec or (OpSys and opWNT<>0)
   then Executables.Filter := Executables.Filter + ';cmd';
  {$IFDEF OS_DOS}
  if Chk4Dos
   then Executables.Filter := Executables.Filter + ';btm';
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

  RunMenu := (StartupData.Load and osuAutoMenu <> 0);
  SetOverlay;
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT');
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.MNU');
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'.LST');
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.LST');
{$IFDEF OS2}
  EraseFile(SwpDir+'$DN'+ItoS(DNNumber)+'$.CMD');
{$ENDIF}
  ReadINI;
{$IFDEF SS}Val(SaversData.Time, SkyDelay, Integer(SPos1));{$ENDIF}
  if SkyDelay=0 then SkyDelay:=255; { X-Man }
 {ExecDNAutoexec;}
{$IFDEF FileNotify}
{Cat}
  if DNIni.AutoRefreshPanels then
    NotifyInit;
{/Cat}
{$ENDIF}
end;
        {-DataCompBoy-}

{$IFDEF GRABPalette}
procedure GrabPalette;
  var F: Text;
      I: Integer;
begin
  Assign(F, MakeNormName(Tempdir, 'Pallete.PAS'));
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

procedure CrLf;
{$IFNDEF OS_DOS}
begin Writeln; end;
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
  ShiftRec: record ScrH, CurY: Byte end absolute FreeStr; { just to reduce DS }
{$IFDEF OS2}var RegRec: ExceptionRegistrationRecord; {AK155 for killer}
 {this data MUST be located in stack}
{$ENDIF}
var Ev: TEvent;
{$IFDEF DPMI} R: DPMIRegisters; {$ENDIF}
begin
 {if memAvail < 100000 then begin}
   {WriteLn(#10#13'Not enough memory for Navigator.');}
   {WriteLn(      'Please check if 400K memory is available');}
   {Halt(203);}
 {end}
{$IFDEF VIRTUALPASCAL}
 {else}
    WriteLn('Dos Navigator /2 Open Source  '+ VersionName + '  Based on DN by RIT Labs')
{$ENDIF}
;

{$IFDEF WIN95_HIGHPRIORITY}
 if SysPlatformId = 1 then {Win9x}
  SetPriorityClass(GetCurrentProcess, High_Priority_Class); {Cat: �⮡ �� �ମ����}
{$ENDIF}

 Randomize;

 LoaderSeg := 0;
 LSliceCnt := -3;

{$IFNDEF NONBP}

{$IFNDEF DPMI}
 asm
  mov  AX, $9900
  int  2Fh
  cmp  BX, 'DN'
  jne  @@1
  mov  DNNumber, AL
  mov  RunFirst, AH
  xor  AX, AX
  mov  ES, AX
  mov  AX, $9901
  int  2Fh
  mov  CommandOfs, BX
  mov  AX, ES
  mov  LoaderSeg, AX
 @@1:
  mov  AH,1
  xor  BL, BL
  mov  CX, $0607
  push BP
  int  10h
  pop  BP
  mov  AX, $9905
  xor  DX, DX
  xor  CX, CX
  push BP
  int  2Fh
  mov  word ptr DDTimer, DX
  mov  word ptr DDTimer+2, CX
  pop  BP
 end;
{$ELSE}
 FillChar(R, SizeOf(R), 0);
 R.AX:=$9900;
 SimulateRealmodeInt($2F, R);
 If R.BX=$444E{'DN'} then begin
  DNNumber:=R.AL;
  RunFirst:=Boolean(R.AH);
  R.AX:=$9901; R.ES:=0;
  SimulateRealmodeInt($2F, R);
  CommandOfs:=R.BX;
  LoaderSeg:=RSeg(R.ES);
  R.AX:=$9905;
  R.DX:=0; R.CX:=0;
  SimulateRealmodeInt($2F, R);
  DDTimer:=R.CX;
  DDTimer:=DDTimer shl 16 + R.DX;
 end else DDTimer := 0;
 R.AH:=1; R.BL:=0; R.CX:=$0607;
 SimulateRealmodeInt($10, R);
{$ENDIF}

{$ENDIF}

{$IFDEF VIRTUALPASCAL} RunFirst:=true; {$ENDIF}

 if DDTimer > 0 then DDTimer := Get100s-DDTimer;

 TempBounds.Assign(0,0,0,0);

{$IFNDEF NONBP}
 asm
   MOV  AH, 03 { Get cursor position }
   MOV  BX, 0  { DL - X }
   INT  10H    { DH - Y }
   OR   DL,DL
   JE   @Pass
   CALL CrLf   { if WhereX <> 0 then WriteLn }
   MOV  AH, 03
   MOV  BX, 0
   INT  10H
 @Pass:
   MOV  ShiftRec.CurY,DH
   CALL GetCrtMode { DL - ScreenHeight }
   MOV  ShiftRec.ScrH,DL
   {PZ 2000.07.31 check for VESA when DN's configuration is read }
   MOV  VideoType,vtUnknown
 end;
{$ENDIF}

 {$IFDEF DPMI}
 InitExtraMem;
 {$ENDIF}
 RegisterAll;
 DoStartUp;
 {$IFNDEF DPMI32}
 SetKillHandler{$IFDEF OS2}(RegRec){$ENDIF}; {AK155}
 {$ENDIF}
 with ShiftRec do begin
   If ( CurY = ScrH ) and ( InterfaceData.Options and ouiHideStatus = 0 ) then begin
     CrLf;
     {$IFNDEF NONBP}
     asm
       XOR  DX,DX
       MOV  DH,ShiftRec.CurY
       DEC  DH
       MOV  AH,2
       XOR  BX,BX
       INT  10H
     end
     {$ENDIF}
   end
 end;

 SetBlink(CurrentBlink);

 {$IFNDEF VIRTUALPASCAL}NoSound;
 if (FadeDelay > 0) and RunFirst then BlackPalette;{Knave}
 {$ENDIF}
 (* InitLFNCol; *)
 MyApplication.Init;

 if RunFirst then
   ShowIniErrors;

 if RunFirst then
  if (StartupData.Load and osuKillHistory <> 0) then ClearHistories;
 {$IFDEF OS_DOS}
 if not RunFirst then EraseFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
 {$ENDIF}
 If RunFirst then
   begin
    if (Message( @MyApplication, evBroadcast, cmLookForPanels, NIL ) = NIL)
       then Message( @MyApplication, evCommand, cmFirstTimePanel, NIL );

    FreeStr[1] := Char(FindParam('/P'));
    if (FreeStr[1] > #0) then LoadPalFromFile(Copy(ParamStr(Byte(FreeStr[1])), 3, MaxStringLength));
    if Virgin then Message( @MyApplication, evCommand, cmAbout , nil ); {JO}
    if NoTempDir then begin CreateDirInheritance(TempDir, Off); NoTempDir := False; end; {JO}
    ExecDNAutoexec;
   end;

 {$IFNDEF VIRTUALPASCAL}
 if (FadeDelay >0) and RunFirst then GlowPalette;{Knave}
 {$ENDIF}

{$IFDEF GRABPalette}
GrabPalette;
{$ENDIF}
 if DDTimer > 0 then
  begin
   Ev.What := evCommand;
   Ev.Command := cmShowTimeInfo;
   MyApplication.PutEvent(Ev);
  end;

 if not IgnoreOldFiles then CheckForOldFiles;
 with MyApplication do begin
  Lock;
  MenuBar^.MakeFirst;
  Desktop^.MakeFirst;
  Clock^.MakeFirst;
  Unlock;
 end;
 {$IFDEF OS_DOS}
 w95QuitInit; {Gimly}
 {$ENDIF}
 MyApplication.Run;
 {$IFNDEF DPMI32}
 UnsetKillHandler{$IFDEF OS2}(RegRec){$ENDIF}; {AK155}
 {$ENDIF}
 ClearIniErrors;
 GlobalMessage(evCommand, cmKillUsed, nil);
 TottalExit := On;
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
   Dispose(StringCache,Done);
 DoneHistories;
 if ColorIndexes <> nil then
   FreeMem(ColorIndexes, 2 + ColorIndexes^.ColorSize);
{/Cat}
end;

{Cat}
{$IFDEF LINEPOSIT}
procedure ERROR(const FileName: String; LineNo, Addr, Code: LongInt);
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

END.
