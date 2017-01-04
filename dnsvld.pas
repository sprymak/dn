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

unit DNSvLd;
interface

uses Objects, Views, DnUtil;

procedure SaveRealDsk;
Procedure WriteConfig;
Procedure LoadPalFromFile(const FN: String); {DataCompBoy}

procedure LoadIndexes(var S: TStream);
procedure StoreIndexes(var S: TStream);

procedure SaveDsk;

const
     cfgMouseData           =  3;
     cfgInterfaceData       =  4;
     cfgSaversData          =  5;
     cfgSystemColors        =  6;
     cfgPanelDefaults       =  7;
     cfgTetrisRec           =  9;
     cfgOldCustomMasks      = 10; {DataCompBoy}
     cfgPrinterSetup        = 11;
     cfgColumnDefaults      = 12;
    {cfgSavers              = 13; Obsolete}
     cfgCountryInfo         = 14;
     cfgConfirms            = 15;
     cfgTermDefaults        = 16;
     cfgDirsToChange        = 17;
     cfgFindDlgPos          = 20;
     cfgCDParams            = 21;
     cfgUUEData             = 22;
     cfgVGAPalette          = 23;
     cfgBlink               = 24;
     cfgFFindOptions        = 26;
     cfgOldFMSetup          = 27; {DataCompBoy}
     cfgDriveInfoData       = 28;
     cfgMakeListFile        = 29;
     cfgOldEditorDefaults   = 30; {DataCompBoy}
     cfgStartupData         = 31;
     cfgSystemData          = 32;
     cfgINIdata             = 33;
     cfgIgnoreOldFiles      = 34;
     cfgExtractOptions      = 35;
{New Record Versions}
     cfgOldSystemData       = 36; {DataCompBoy}
     cfgNewSaversData       = 37;
     cfgColumnsDefaultsDisk = 38;
     cfgColumnsDefaultsFind = 39;
     cfgColumnsDefaultsTemp = 40;
     cfgColumnsDefaultsArch = 41;
{    cfgColumnsDefaultsArvd = 42; Do not use 42 - old version!!!}
     cfgNewFMSetup          = 43; {DataCompBoy}
     cfgOldArcCustomMasks   = 44; {DataCompBoy}
     cfgColumnsDefaultsArvd = 45; {DataCompBoy}
     cfgShowScrollBar       = 47; {DataCompBoy}
     cfgNewStartupData      = 48; {DataCompBoy}
     cfgDefaultArchiver     = 49; {DataCompBoy}
     cfgDefaultArchiverMode = 50; {DataCompBoy}
     cfgOld2SystemData      = 51; {DataCompBoy}
     cfgOldCustomMasks2     = 52; {DataCompBoy}
     cfgCustomMasks         = 53; {DataCompBoy}
     cfgCustomMasks2        = 54; {DataCompBoy}
     cfgArcCustomMasks      = 55; {DataCompBoy}
     cfgFMSetup             = 56; {DataCompBoy}
     {!!!!!!!!!!!!!!!!!!!!! = 57; !DO NOT USE!}
     cfgNewSystemData       = 58; {DataCompBoy}
     cfgOldEditorDefaults2  = 59; {DataCompBoy}
     cfgNewPanelDefaults    = 60; {DataCompBoy}
     cfgSavers              = 61; {DataCompBoy}
     cfgChangeCaseOptions   = 62; {DataCompBoy}
     cfgINIcrc              = 63; {DataCompBoy}
     cfgEditorDefaults      = 64; {DataCompBoy}
     cfgViewerDefaults      = 65; {DataCompBoy}

     dlAbout = #13#3'Dos Navigator Open Source'#13+
     {$IFDEF LITE}+#3'Light version'+{$ELSE}+#13+{$ENDIF}
     +#13#3'Version %s, %s'+
     +#13#3#0'http://www.nsk.su/~dnosp/'#0+
     {$IFDEF VIRTUALPASCAL}
     +#13+
     +#13#3'Thanks to Allan Mertner for copy of VP'+
     +#13#3#0'http://www.vpascal.com'#0#13+
     {$ELSE}
     +#13#13+
     {$ENDIF}
     +#13#3'Based on Dos Navigator'+
     +#13#3'Copyright (C) 1991-99 RIT Research Labs'#13#13;


type
      PDataSaver = ^TDataSaver;
      TDataSaver = object(TView)
         constructor Init;
         constructor Load(var S: TStream);
         procedure Store(var S: TStream);
         destructor Done; virtual;
      end;

const
       DataSaver: PDataSaver = nil;


type PDNApplication = ^TDNApplication;
     TDNApplication = object(TDNAppl)
        Constructor Init;
        Destructor  Done; virtual;
        procedure InitMenuBar; virtual;
        procedure InitCommandLine; virtual;
        procedure InitDesktop; virtual;
        procedure InitStatusLine; virtual;
        procedure RetrieveDesktop(const FileName: String; LS: PStream; LoadColors: Boolean); {DataCompBoy}
        procedure SaveDesktop(const FileName: String); {DataCompBoy}
        procedure LoadDesktop(var S: TStream);
        procedure StoreDesktop(var S: TStream);
     end;

implementation
uses Advance, FStorage, FilesCol, Dos, Lfn, FViewer, CmdLine, DnApp, Drives,
     Collect, Commands, Advance1, VideoMan, Drivers, Startup, Memory, HelpFile,
     Advance2, Messages, Gauges, DnIni, Menus, Tree, Histries, Microed, WinClp,
     FileFind
     {$IFDEF Modem},ScrollBk,ModemIO,ApPort,ApUart{$ENDIF}
     {$IFDEF CDPlayer},CdPlayer{$ENDIF}
     {$IFDEF DNPRG},Archiver,FlPanelx,Advance7{$ENDIF};

procedure ClearSwap;
  var DT: DOS.DateTime;
      L: LongInt;
      SR: lSearchRec;

  procedure ClearFiles(const FileSpec: String);
  begin
    lFindFirst(SwpDir+FileSpec, Archive, SR);
    while DOSError = 0 do
      begin
        if SR.SR.Time < L then EraseFile(SR.FullName);
        ClrIO;
        lFindNext(SR);
      end;
    lFindClose(SR);
  end;


begin
  FillChar(DT, SizeOf(DT), 0);
  GetDate(DT.Year, DT.Month, DT.Day, Word(L));

  PackTime(DT, L);
  SetFileAttr(SwpDir+'DN.FLG', 0);
  ClrIO;
  ClearFiles('DN*.SWP');
  ClearFiles('$DN*.*');
  ClearFiles('$$DN*.*');
  ClearFiles('$$$DN*.*');
end;

{ Load and Store Palette routines }

procedure LoadIndexes(var S: TStream);
var
  ColorSize: byte;
begin
  S.Read(ColorSize, sizeof(ColorSize));
  if ColorSize > 0 then
  begin
    if ColorIndexes <> nil then
      FreeMem(ColorIndexes, 2 + ColorIndexes^.ColorSize);
    getmem(ColorIndexes, ColorSize);
    S.Read(ColorIndexes^, ColorSize);
    ColorIndexes^.ColorSize := (ColorSize-2);
  end;
end;

procedure StoreIndexes(var S: TStream);
var
  ColorSize: Byte;
begin
  if ColorIndexes <> nil then
    ColorSize := 2 + ColorIndexes^.ColorSize
  else
    ColorSize := 0;
  S.Write(ColorSize, sizeof(ColorSize));
  if ColorSize > 0 then
    S.Write(ColorIndexes^, ColorSize);
end;


constructor TDataSaver.Init;
  var R: TRect;
begin
  R.Assign(0,0,0,0);
  inherited Init(R);
  SetState(sfVisible, Off);
  Options := Options and not ofSelectable;
  EventMask := 0;
  DataSaver := @Self;
end;


const
     dskViewerFind   = 2;
     dskEditorFind   = 3;
     dskHideCmdLine  = 4;
     dskViewerBounds = 5;
     dskTempContents = 6;
     dskTempContents2= 7;

constructor TDataSaver.Load;
  var D, L: AWord;
      fs: PDirStorage;
  var P: PFileRec;
      DT: DateTime;
      I, Q, Q2: LongInt;
      DrNm: String;
      OW: PString;
      AS: String;

begin
  if DataSaver <> nil then Dispose(DataSaver,Done); DataSaver:=nil;
  inherited Load(S);
  DataSaver := @Self;
  repeat
    S.Read(D, SizeOf(D));
    if D = 0 then break;
    S.Read(L, SizeOf(L));
    case D of
      dskViewerBounds: S.Read(LastViewerBounds, L);
      dskViewerFind: S.Read(FViewer.SearchString, L);
      dskEditorFind: S.Read(MicroEd.SearchData, L);
      dskHideCmdLine: begin
                        S.Read(HideCommandLine, L);
                        if (CommandLine <> nil) and (CommandLine^.GetState(sfVisible) and HideCommandLine) then
                              ToggleCommandLine(not HideCommandLine);
                      end;
      dskTempContents2: if TempFiles = nil then
                         begin
                           TempDirs := PStringCollection(S.Get);
                           if TempDirs = nil then continue;
                           S.Read(Q, SizeOf(Q));
                           If Q >= 0 then begin
                            TempFiles := New(PFilesCollection, Init(Q+1, $10));
                            TempFiles^.SortMode := psmOrdered;
                            TempFiles^.Duplicates := False;
                            for Q2:=0 to Q do TempFiles^.AtInsert(Q2, LoadFileRecOwn(S, TempDirs));
                           end;
                         end else S.Seek(S.GetPos + L);
       else S.Seek(S.GetPos + L);
    end;
  until D = 0;
end;

procedure TDataSaver.Store;

 var D: AWord;
     I, Q, Q2, SPos: LongInt;

  procedure StoreBlock(I: AWord; var B; Sz: AWord);
  begin
    S.Write(I, SizeOf(I));
    S.Write(Sz, SizeOf(Sz));
    S.Write(B, Sz);
  end;

  procedure MarkP(Blk: AWord);
  begin
    S.Write(Blk, SizeOf(Blk));
    S.Write(Blk, SizeOf(Blk));
    SPos := S.GetPos;
  end;

  procedure UnMark;
  begin
    I := S.GetPos - SPos;
    S.Seek(SPos-SizeOf(AWord));
    S.Write(I, SizeOf(AWord));
    S.Seek(S.GetSize);
  end;

begin
  inherited Store(S);
  HideCommandLine := (CommandLine <> nil) and not CommandLine^.GetState(sfVisible);
  StoreBlock(dskViewerFind, FViewer.SearchString, SizeOf(FViewer.SearchString));
  StoreBlock(dskEditorFind, MicroEd.SearchData, SizeOf(MicroEd.SearchData));
  StoreBlock(dskViewerBounds, LastViewerBounds, SizeOf(LastViewerBounds)+SizeOf(TPoint)*2);
  StoreBlock(dskHideCmdLine, HideCommandLine, SizeOf(HideCommandLine));
  if (TempFiles <> nil) and (TempFiles^.Count <> 0) then begin
   MarkP(dskTempContents2);
   S.Put(TempDirs);
   if TempDirs <> nil then begin
    Q := TempFiles^.Count - 1;
    S.Write(Q, SizeOf(Q));
    for Q2 := 0 to Q do StoreFileRecOwn(S, TempFiles^.At(Q2), TempDirs);
    UnMark;
   end;
  end;
  D := 0;
  S.Write(D, SizeOf(D));
end;

destructor TDataSaver.Done;
begin
  DataSaver := nil;
  inherited Done;
end;

procedure SaveRealDsk;
begin
  PDNApplication(Application)^.SaveDesktop(SourceDir+'DN'+GetEnv('DNDSK')+'.DSK');
end;

procedure WriteConfig;
{$IFDEF DNPRG}
var
  S: TBufStream;
  I: AWord;
  SPos: LongInt;

  procedure StoreBlock(I: AWord; var B; Sz: AWord);
  begin
    S.Write(I, SizeOf(I));
    S.Write(Sz, SizeOf(Sz));
    S.Write(B, Sz);
  end;

  procedure MarkP(Blk: AWord);
  begin
    S.Write(Blk, SizeOf(Blk));
    S.Write(Blk, SizeOf(Blk));
    SPos := S.GetPos;
  end;

  procedure UnMark;
  begin
    I := S.GetPos - SPos;
    S.Seek(SPos-SizeOf(AWord));
    S.Write(I, SizeOf(AWord));
    S.Seek(S.GetSize);
  end;

begin
 ConfigModified := Off;
 S.Init(SourceDir+'DN'+GetEnv('DNCFG')+'.CFG', stCreate, 16384);
 if S.Status <> stOK then begin Msg(erCantOpenConfig, nil, mfError+mfOKButton); S.Done; Exit; end;
 S.Write(ConfigSigns[NumSupportedConfigs].Sign[1], ConfigSigns[NumSupportedConfigs].SignLen);
 if ConfigSigns[NumSupportedConfigs].HavVer then S.Write(VersionWord, SizeOf(VersionWord));
 StoreBlock(cfgNewSystemData, SystemData, SizeOf(SystemData));
 StoreBlock(cfgNewStartupData, StartupData, SizeOf(StartupData));
 StoreBlock(cfgMouseData, MouseData, SizeOf(MouseData));
 StoreBlock(cfgInterfaceData, InterfaceData, SizeOf(InterfaceData));
 StoreBlock(cfgShowScrollBar, ShowScrollBar, SizeOf(ShowScrollBar));
{$IFDEF SS}
 StoreBlock(cfgNewSaversData, SaversData.Time, SizeOf(SaversData)-SizeOf(SaversData.Selected)*2);
 MarkP(cfgSavers); S.Put(SaversData.Selected.List); UnMark;
{$ENDIF}
 StoreBlock(cfgSystemColors, SystemColors, SizeOf(SystemColors));
 StoreBlock(cfgNewPanelDefaults, PanelDefaults, SizeOf(PanelDefaults));
 StoreBlock(cfgEditorDefaults, EditorDefaults, SizeOf(EditorDefaults));
 StoreBlock(cfgViewerDefaults, ViewerDefaults, SizeOf(ViewerDefaults));
 StoreBlock(cfgTetrisRec, TetrisRec, SizeOf(TetrisRec));
 StoreBlock(cfgArcCustomMasks, Startup.Archives^.Filter, SizeOf(Startup.Archives^.Filter));
 MarkP(cfgCustomMasks);
 S.Write(CustomMask1^.Filter, SizeOf(CustomMask1^.Filter));
 S.Write(CustomMask2^.Filter, SizeOf(CustomMask2^.Filter));
 S.Write(CustomMask3^.Filter, SizeOf(CustomMask3^.Filter));
 S.Write(CustomMask4^.Filter, SizeOf(CustomMask4^.Filter));
 S.Write(CustomMask5^.Filter, SizeOf(CustomMask5^.Filter));
 UnMark;
 MarkP(cfgCustomMasks2);
 S.Write(CustomMask6^.Filter, SizeOf(CustomMask6^.Filter)); {JO}
 S.Write(CustomMask7^.Filter, SizeOf(CustomMask7^.Filter));
 S.Write(CustomMask8^.Filter, SizeOf(CustomMask8^.Filter));
 S.Write(CustomMask9^.Filter, SizeOf(CustomMask9^.Filter));
 S.Write(CustomMask10^.Filter, SizeOf(CustomMask10^.Filter));{JO}
 UnMark;
{$IFDEF PRINTMANAGER}
 StoreBlock(cfgPrinterSetup, RPrinterSetup, SizeOf(RPrinterSetup));
{$ENDIF}
 StoreBlock(cfgColumnsDefaultsDisk, ColumnsDefaultsDisk, SizeOf(ColumnsDefaultsDisk));
 StoreBlock(cfgColumnsDefaultsFind, ColumnsDefaultsFind, SizeOf(ColumnsDefaultsFind));
 StoreBlock(cfgColumnsDefaultsTemp, ColumnsDefaultsTemp, SizeOf(ColumnsDefaultsTemp));
 StoreBlock(cfgColumnsDefaultsArch, ColumnsDefaultsArch, SizeOf(ColumnsDefaultsArch));
 StoreBlock(cfgColumnsDefaultsArvd, ColumnsDefaultsArvd, SizeOf(ColumnsDefaultsArvd));
 StoreBlock(cfgCountryInfo, CountryInfo, SizeOf(CountryInfo));
 StoreBlock(cfgConfirms, Confirms, SizeOf(Confirms));
 StoreBlock(cfgUUEData, UUDecodeOptions, SizeOf(UUDecodeOptions)+SizeOf(TUUEncodeData));
 StoreBlock(cfgMakeListFile, MakeListFileOptions, SizeOf(MakeListFileOptions));
 StoreBlock(cfgTermDefaults, TerminalDefaults, SizeOf(TerminalDefaults));
 StoreBlock(cfgFMSetup, Startup.FMSetup, SizeOf(Startup.FMSetup));
{$IFDEF CDPlayer}
 StoreBlock(cfgCDParams, CDPlace, LongInt(@CD_Player)-LongInt(@CDPlace));
{$ENDIF}
 StoreBlock(cfgVGApalette, VGA_palette , SizeOf(VGA_palette)) ;
 StoreBlock(cfgBlink, CurrentBlink, SizeOf(CurrentBlink)) ;
 StoreBlock(cfgFFindOptions, FileFind.FindRec.Options, SizeOf(Word) * 2);
 StoreBlock(cfgDriveInfoData, DriveInfoData, SizeOf(DriveInfoData));
 StoreBlock(cfgExtractOptions, ExtractWithPathNames, sizeof(ExtractWithPathNames));
 StoreBlock(cfgDefaultArchiver, DefaultArchiver, SizeOf(DefaultArchiver));
 StoreBlock(cfgDefaultArchiverMode, DefaultArcMode, SizeOf(DefaultArcMode));
 StoreBlock(cfgExtractOptions, ExtractWithPathNames, sizeof(ExtractWithPathNames));
 StoreBlock(cfgChangeCaseOptions, ChangeNamesCaseOptions, Sizeof(ChangeNamesCaseOptions));

 {StoreBlock(cfgINIcrc, INIstoredcrc, SizeOf(INIstoredcrc));}
 MarkP(cfgINIdata);
 S.Write(INIstoredtime,sizeof(INIstoredtime));
 S.Write(INIstoredsize,sizeof(INIstoredsize));
 S.Write(iniparamblock_START,Ofs(iniparamblock_END)-Ofs(iniparamblock_START));
 UnMark;

 if IgnoreOldFiles then StoreBlock(cfgIgnoreOldFiles,IgnoreOldFiles,0);

 MarkP(cfgDirsToChange);for I := 0 to 8 do S.WriteStr(DirsToChange[I]);UnMark;

 StoreBlock(0,I,0);
 S.Done;
{$ELSE}{!DNPRG}
begin
{$ENDIF}{DNPRG}
end;

procedure LoadPalFromFile(const FN: String);
 var  P : longint ;
      LoadPalette,LoadVGAPalette : Boolean;
      St: String;
      S: TDOSStream;
      Pal: PString;
begin
  LoadPalette    := Off;
  LoadVGAPalette := Off;
  S.Init(FN, stOpenRead);
  if S.Status = 0 then
   begin
    Pal := S.ReadStr;
    if Pal <> nil then
    begin
      St := Pal^; DisposeStr(Pal);
      While St[0] < Char(Length(CColor)) do St := St + #$3F;
      Application^.GetPalette^ := Copy(St,1,Length(CColor));
      {SetSysColors(Application^.GetPalette^);}
      LoadPalette := On;
    end;
    LoadIndexes(S);
    P := 0;
    S.Read(P, SizeOF( P ));
    if P = $50414756 then { VGAP }
     begin
       S.Read( VGA_Palette, SizeOF( VGA_Palette )) ;
         S.Read(P, SizeOF( P ));
         if P = $4B4E4C42 then { BLNK }
          begin
            S.Read( CurrentBlink, SizeOf( CurrentBlink ) );
            SetBlink( CurrentBlink );
          end;
       LoadVGAPalette := On;
       if StartupData.Load and osuResetPalette <> 0 then SetPalette( VGA_Palette );

     end;{ else
      if Msg(dlRestoreVGAPalette, nil, mfYesNoConfirm) = cmYes then
          ResetVGAPalette( True ) ;}
   end;
  S.Done;
  WriteConfig;

  if LoadPalette then
   begin
    DoneMemory;
    Application^.ReDraw;
   end;
end;

        {-DataCompBoy-}
            procedure TDNApplication.LoadDesktop(var S: TStream);
var
  P: PView;
  PP: PView;
  Pal: PString;
  SaveState: AWord;
begin
  lGetDir(0, ActiveDir);
  if Desktop^.Valid(cmClose) then begin
    Desktop^.Clear;
    repeat
     P := PView(S.Get);
     if P <> nil then
      begin
       if DataSaver <> nil then
        begin
         Dispose(DataSaver,Done);
         DataSaver:=nil;
         Continue
        end;
       P := ValidView(P);
       if P = nil then Continue;
       SaveState := P^.State;
       P^.Hide;
       Desktop^.InsertView(P, DeskTop^.Last);
       if SaveState and sfVisible <> 0 then P^.Show;
      end;
    until P = nil;
  end;
 P := DeskTop^.Current;
 if P <> nil then P^.SetState(sfActive, On);
end;
        {-DataCompBoy-}

procedure TDNApplication.StoreDesktop(var S: TStream);
var
  Pal: PString;

procedure WriteView(P: PView); {$IFDEF BIT_16}far;{$ENDIF}
type TLongInt = array[0..10] of longint;
     PLongInt = ^TLongInt;
var l:array[0..5] of longint;
{$IFDEF StreamDebug} Q: Text; {$ENDIF}
begin
  if (P <> Desktop^.Last)
     {$IFDEF TrashCan} and (P <> PView(TrashCan)) {$ENDIF}
     and (P <> PView(Desktop^.Background))
     and (TypeOf(P^) <> TypeOf(TWriteWin))
     and (TypeOf(P^) <> TypeOf(THelpWindow))
   then S.Put(P);
{$IFDEF StreamDebug}
 if S.Status=-6 then begin
  assign(Q,'qwer');
  append(Q); if ioresult<>0 then rewrite(Q);
  writeln(Q, PWindow(P)^.Title^);
  close(Q);
 end;
{$ENDIF}
end;

begin
  if DataSaver = nil then New(DataSaver, Init);
  if DataSaver <> nil then
   begin
    S.Put(DataSaver);
    Dispose(DataSaver,Done);
    DataSaver:=nil;
   end;
  Desktop^.ForEach(@WriteView);
  S.Put(nil);
end;

function CacheLngId: string;
begin
{$IFDEF DNPRG}CacheLngId := versionName + LngId + versionDate + '123';{$ENDIF}
end;

        {-DataCompBoy-}
procedure TDNApplication.RetrieveDesktop;
var
  S: PStream;
  Sign: String[MaxSignLen];
  B, BB: Boolean;
  SM,CC: Word;
  R: TRect;
  SaveBounds: TRect;
  PS: PString;
  PJ: PString;
  Event: TEvent;
  F: lFile;
  OldAppSize, OldDskSize: TPoint;
  Q: String; QQ: integer;

procedure GetStringCache;
var
  Ss: string;
  P: PCollection;
begin
  Ss := CnvString(S^.ReadStr);
  if Ss <> #0#0#0 then
  begin
    P := PCollection(S^.Get);
    if (StringCache = nil)
     then if (Ss = CacheLngId)
           then StringCache:=P
           else begin InitStringCache; Dispose(P,Done) end
     else Dispose(P,Done);
  end;
end;

function GetGLUKName(s: string):string;
 Begin
  GetGLUKName:=GetPath(s)+Copy(GetName(s), 1, 12);
 end;

FUNCTION GetfURZ2(const s: string): string;
 var a,aa,aaa:string;
 Begin
  lFSplit(S, a, aa, aaa);
  aa:=aa+aaa;
  GetfURZ2:=a+Copy(aa, 1, 8)+'.'+Copy(aaa, 9, 3);
 End;

label
  Err;
begin
  HideCommandLine := (CommandLine <> nil) and (not CommandLine^.GetState(sfVisible));
  SetState(sfActive, True);
  if LS = nil
    then S := New(PBufStream, Init(FileName, stOpenRead, 4096))
    else S := LS;
  if not Desktop^.Valid( cmClose ) then Exit;
  if LowMemory
    then OutOfMemory
    else
  if ( S^.Status <> stOk ) or ( S^.GetSize < SizeOf( DskSign ))
    then
      Err: ErrMsg(erCantReadDesktop)
    else
  begin
    S^.Read( Sign[ 1 ], DSKSign.SignLen );
    Sign[ 0 ] := Char( DSKSign.SignLen );
    If Sign <> DSKSign.Sign then goto Err;
    GetStringCache;
    if S^.Status <> stOK then goto Err;
    PS := S^.ReadStr;
    if PS <> nil then lChDir(PS^);
    DisposeStr(PS);
    PJ := S^.ReadStr;
    if (PJ <> nil) and (PJ^ <> '') then
     if PosChar(PJ^[1], '+-=<>|[!') = 0  then
      begin lAssignFile(F, PJ^); lSetFAttr(F, $20);  lEraseFile(F); end
      else
       begin
        TempFile := Copy(PJ^,2,255);
        QQ := Pos('|', TempFile); Dec(QQ);
        if QQ>0 then Q:=Copy(TempFile, QQ+2, 255) else Q:='';
        QQ := QQ and 255; {BP bugfix by piwamoto}
        If Not ExistFile(TempFile) then TempFile:=GetfURZ(Copy(PJ^,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=GetfURZ2(Copy(PJ^,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=GetGLUKName(Copy(PJ^,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=Copy(PJ^,2,QQ);
        case PJ^[1] of
          '-': CC := cmIntFileView;
          '=': CC := cmDBFView;
          '>': CC := cmWKZView;
          '<': CC := cmTextView;
          '|': CC := cmHexView;
          '[': CC := cmReadArchive;
          '!': CC := cmViewFilter;
          else CC := cmFileView;
        end;
        if CC <> 0 then
                    begin
                      if Q<>'' then TempFile:=TempFile+'|'+Q;
                      Event.What := evCommand;
                      Event.Command := CC;
                      Event.InfoPtr := @TempFile;
                      PutEvent(Event);
                     end;
        RunMenu := Off;
       end;
(*  S^.Read(NewArchiveName, 1); S^.Read(NewArchiveName[1],length(NewArchiveName));
    S^.Read(OldArchiveName, 1); S^.Read(OldArchiveName[1],length(OldArchiveName));
*)S^.Read(FreeStr,1); S^.Read(FreeStr[1],length(FreeStr));
  S^.Read(FreeStr,1); S^.Read(FreeStr[1],length(FreeStr));
    S^.Read(SM, Sizeof(SM));
    S^.Read( OldAppSize, SizeOf( Size ));
    S^.Read( OldDskSize, SizeOf( Size ));
    BB := not (OldAppSize.Equals(Size));
    LoadIndexes(S^);
    GetExtent( SaveBounds );
    Desktop^.Clear;
    if BB and ((SM <> ScreenMode) and
       (StartupData.Load and osuRestoreScrMode <> 0))
      then begin
      SetScrMode(SM);
      SetBlink(CurrentBlink);
    end;
    BB := not (OldDskSize.Equals(Desktop^.Size));
    If BB then begin
      Desktop^.Hide;
      R.A := Desktop^.Origin;
      R.B.X := R.A.X + OldDskSize.X;
      R.B.Y := R.A.Y + OldDskSize.Y;
      Desktop^.ChangeBounds(R);
    end;
    LoadDesktop(S^);
(*    If OldArchiveName<>NewArchiveName then
     begin
      lAssignFile(F, OldArchiveName);
      lRenameFile(F, NewArchiveName);
      OldArchiveName:=GetPath(OldArchiveName);
      NewArchiveName:=GetPath(NewArchiveName);
      RereadDirectory(OldArchiveName);
      If UpStrg(OldArchiveName)<>UpStrg(OldArchiveName) then
       RereadDirectory(NewArchiveName);
      OldArchiveName:='';
      NewArchiveName:='';
     end;*)
    DisposeStr(PJ);
    S^.Read(TempBounds, SizeOf(TempBounds));
    {S^.Read(ArcBounds, SizeOf(TempBounds));}
{$IFDEF TrashCan}S^.Read(TrashCan^.ImVisible, 1);{$ENDIF}
    KeyMacroses := PCollection(S^.Get);
    S^.Read(R,SizeOf(R));
    if not ShowSeconds then
     if R.A.X>(ScreenWidth shr 1) then Inc(R.A.X,3) else Dec(R.B.X,3);
    Clock^.Locate(R);
    {S^.Read(ArcFlags, 4);}
    If BB then
       begin
         R.Assign(0, Byte( InterfaceData.Options and ouiHideMenu = 0 ), Size.X, Size.Y
                  - Byte( InterfaceData.Options and ouiHideStatus = 0 )
                  - Byte( not HideCommandLine) );
         Desktop^.ChangeBounds(R);
         Desktop^.Show;
       end;
{$IFDEF TrashCan}
    if TrashCan^.ImVisible then
     begin
      TrashCan^.Show;
      S^.Read(R, SizeOf(R));
      TrashCan^.Locate(R)
     end;
{$ENDIF}
    if PreserveMenuPositions then LoadMenuDefaults(MenuBar^.Menu, S^);
  end;
  Desktop^.Redraw;
  GetBounds(R);
  if (Clock^.Size.X <= 0) or (Clock^.Size.Y <= 0) or not R.Contains(Clock^.Origin) then
    begin
      if ShowSeconds then R.Assign(Size.X-10,0,Size.X,1)
      else R.Assign(Size.X-7,0,Size.X,1);
      Clock^.Locate(R);
    end;
  Dispose(S,Done);
  ActivateView(Desktop^.Current);
  GlobalMessage(evCommand, cmRereadForced, nil);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDNApplication.SaveDesktop(const FileName: String);
var
  S: PStream;
  F: lFile;
  B: Boolean;
  PP: Pointer;
  R: TRect;

procedure PutStringCache;
var
  Ss: pString;
begin
  new(ss);
  lFSplit(FileName, FreeStr, FreeStr, Ss^);
  if (UpStrg(Ss^) = '.DSK') or
     (StringCache = nil) or
     (StringCache^.Count = 0) then
  begin
    Ss^ := #0#0#0;
    S^.WriteStr(Ss);
  end else
  begin
    Ss^ := CacheLngId;
    S^.WriteStr(Ss);
    S^.Put(StringCache);
  end;
  dispose(ss);
end;

begin
  ClrIO;
  S := New(PBufStream, Init(FileName, stCreate, 2048));
  if not LowMemory and (S^.Status = stOk) then
  begin
    S^.Write( DSKSign.Sign[1], DSKSign.SignLen );
    PutStringCache;
    S^.WriteStr(@DirToChange);
    S^.WriteStr(@TempFile);
    S^.WriteStr(@NullStr);
    S^.WriteStr(@NullStr);
    S^.Write(ScreenMode, sizeof(Word));
    S^.Write( Size, SizeOf( Size ));
    S^.Write( Desktop^.Size, SizeOf( Size ));
    StoreIndexes(S^);
    StoreDesktop(S^);
    S^.Write(TempBounds, SizeOf(TempBounds));
{$IFDEF TrashCan} S^.Write(TrashCan^.ImVisible, 1); {$ENDIF}
    S^.Put(KeyMacroses);
    Clock^.GetBounds(R);
    if R.A.X>(ScreenWidth shr 1) then R.A.X:=R.B.X-10 else R.B.X:=R.A.X+10;
    S^.Write(R, SizeOf(R));
{$IFDEF TrashCan}
    TrashCan^.GetBounds(R);
    if TrashCan^.ImVisible then S^.Write(R, SizeOf(R));
{$ENDIF}
    if PreserveMenuPositions then StoreMenuDefaults(MenuBar^.Menu, S^);
    if S^.Status <> stOk then
    begin
      PP := @FileName;
      Msg(erCantCreateFile, @PP, mfOkButton + mfError);
      MessageBox('S^.Status    = '+ItoS(S^.Status)+#13+
                 'S^.ErrorInfo = '+ItoS(S^.ErrorInfo), nil, mfOkButton);
      Dispose(S,Done);
      lAssignFile(F, FileName);
      lEraseFile(F);
      Exit;
    end;
  end;
  Dispose(S,Done);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
constructor TDNApplication.Init;
var
  R: TRect;
  I: Integer; C: Char absolute I { let's so };
  FileName: String;
  LoadStream: PStream;
  Event: TEvent;

var flj: boolean;
begin
  for C := 'A' to 'Z' do DrvTrees[C].C := nil;
  LastViewerBounds.Assign(0,0,0,0);

  if RunFirst then ClearSwap;

  TApplication.Init;

  LoadHistories;

  GetExtent(R);
  if ShowSeconds then R.A.X := R.B.X - 10 else R.A.X := R.B.X - 7;
  R.B.Y := R.A.Y + 1;
  Clock := New(PClockView , Init( R ));
  if InterfaceData.Options and ouiClock = 0 then Clock^.Hide;
  PClockView(Clock)^.UpDate;

  Desktop^.GetExtent(R);
  Dec(R.B.Y); Dec(R.B.X);
  R.A.Y := R.B.Y - 3;
  R.A.X := R.B.X - 5;
{$IFDEF TrashCan}
  TrashCan := New(PTrashCan, Init(R));
  TrashCan^.ImVisible:=false;
  TrashCan^.Hide;
  DeskTop^.Insert(TrashCan);
{$ENDIF}
  if HideCommandLine then CommandLine^.Hide;
(*
  If RunFirst then
   begin
    OldArchiveName:='';
    NewArchiveName:='';
   end;
*)
  LoadStream := PresentFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
  if LoadStream = nil then LoadStream := PresentFile(SourceDir+'DN'+GetEnv('DNDSK')+'.DSK');
  if LoadStream <> nil then RetrieveDesktop('', LoadStream, True);
  InitDrivers;

  {-$VOL begin}
  if not RunFirst or CBAutoSave then begin
    ClipBoardStream := GetMeMemoStream;
    LoadStream := PresentFile(SourceDir+'CLIPBOAR'+'.DN');
    if (LoadStream <> nil) and (ClipBoardStream <> nil) then begin
      LoadStream^.Seek(0);
      ClipBoardStream^.CopyFrom(LoadStream^, LoadStream^.GetSize);
      if SystemData.Options and ossUseSysClip <> 0 then SyncClipOut(Off);
    end;
    FreeObject( LoadStream );
  end;
  {-$VOL end}

  Insert(Clock);

  flj:=false;
  if RunFirst then
  for I := 1 to ParamCount do
   begin
    if flj then FileName := FileName + ParamStr(I) else FileName := ParamStr(I);
    if Pos('"',FileName)<>0 then flj:=not flj;
    if (FileName[1] <> '/') and not flj then EditFile(On, DelSquashes(FileName));
   end;

 if RunMenu then begin
   Event.What := evCommand;
   Event.Command := cmUserMenu;
   PutEvent(Event);
 end;

 If ExistFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP') then EraseByName(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
end;
        {-DataCompBoy-}

procedure TDNApplication.InitCommandLine;
 var R: TRect;
     HideCL: Boolean;
begin
  HideCL := (InterfaceData.Options and ouiHideCmdline <> 0) or HideCommandLine;

  R.Assign(0,0,0,0);
  TreeReader := New(PTreeReader, Init(R));
  Insert(TreeReader);
  TreeReader^.Hide;

  GetExtent(R);

  R.A.Y := R.B.Y - 1 - Byte( InterfaceData.Options and ouiHideStatus = 0 );
  R.B.Y := R.A.Y + 1 - Byte(HideCL);
  CommandLine := New(PCommandLine, Init(R));
  Insert(CommandLine);
  if not HideCL then ActivateView(CommandLine);
end;

        {-DataCompBoy-}
destructor TDNApplication.Done;
  var B: Word;
      SaveStream: PStream;
begin
 {-$VOL begin}{if CBAutoSave added by piwamoto}
 if CBAutoSave then begin
   SaveStream := New(PBufStream, Init(SourceDir+'CLIPBOAR'+'.DN', stCreate, 2048));
   if (SaveStream<>nil)and(SaveStream^.Status=stOk)and(ClipBoardStream<>nil) then begin
     ClipBoardStream^.Seek(0);
     SaveStream^.CopyFrom(ClipBoardStream^, ClipBoardStream^.GetSize);
   end;
   Dispose(SaveStream,Done);SaveStream:=nil;
 end;
 If ClipBoard<>nil then Dispose(ClipBoard,Done); ClipBoard:=nil;
 if ConfigModified then WriteConfig;
 if (StartupData.Unload and osuAutosave <> 0)
  then SaveRealDsk
  else SaveDsk;
 SaveHistories;
 HideCommandLine := (CommandLine <> nil) and not CommandLine^.GetState(sfVisible);
 B := $8000 or (Byte(HideCommandLine));
 {$IFNDEF NONBP}
 asm
   mov ax, $9906
   mov dx, B
   push BP
   int  2Fh
   pop  BP
 end;
 {$ENDIF}
 inherited Done;
{$IFDEF Modem}
 SetBlink(StartupData.Unload and osuBlinking <> 0);
 DeallocateScrollBk ;
 if StartupData.Unload and osuRestorePal <> 0 then ResetVGApalette(On);
 if COMPort <> nil then
  begin
    B := COMPort^.GetBaseAddr;
    if not TottalExit then begin
      COMPort^.ptOptionsOff(ptDropModemOnClose);
      COMPort^.ptOptionsOff(ptRestoreOnClose);
    end;
    Dispose(COMPort,Done);
    SetFIFOBuffering(B, Off, 0);
  end;
{$ENDIF}
end;
        {-DataCompBoy-}

PROCEDURE TDNApplication.InitMenuBar;
var
  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y;
  MenuBar := PMenuBar(LoadResource(dlgMainMenu));
  if MenuBar = nil then FatalError( 'Invalid resource file.' );
  MenuBar^.Menu^.Items^.Name^:='~'+Char(SystemMenuChar)+'~';
  MenuBar^.Locate(R);
  MenuBar^.Options := MenuBar^.Options and (not ofPreProcess) or ofPostProcess;
end;

PROCEDURE TDNApplication.InitStatusLine;
var
  R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y;
  R.Move( 0, -1 );
  StatusLine := PStatusLine(LoadResource(dlgStatusLine));
  StatusLine^.Locate(R);
end;

PROCEDURE TDNApplication.InitDesktop;
var
  R: TRect;
  WS: Word;
begin
  {$IFNDEF NONBP}
  asm
    mov  ax, 9906h
    xor  dx, dx
    xor  cx, cx
    push bp
    int  2Fh
    pop  bp
    mov  WS, cx
  end;
  {$ELSE}
  WS := 0;
  {$ENDIF}
  HideCommandLine := (WS and cdnHideCmdLine <> 0);
  DisableCommands(DblWndCommands);
  GetExtent(R);
  if InterfaceData.Options and ouiHideMenu = 0 then Inc(R.A.Y);
  if InterfaceData.Options and ouiHideStatus = 0 then Dec(R.B.Y);
  if (InterfaceData.Options and ouiHideCmdline = 0) and not HideCommandLine then Dec(R.B.Y);
  New( Desktop, Init( R ));
end;

procedure SaveDsk;
begin
 ClrIO;
 if ((OpSys <> opDos) or (StartupData.Unload and osuAutosave = 0))
    and not (TottalExit) then
  PDNApplication(Application)^.SaveDesktop(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
end;


end.
