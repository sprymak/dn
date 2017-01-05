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
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

unit DNUtil;

interface
uses Dos,

     LFN,    {DataCompBoy}
     {$IFNDEF OS2}LFNCol,{$ENDIF} {DataCompBoy}

     SBlocks,
     Drivers,
     Objects,
     UniWin,
     Views,
     Histries,
     Scroller,
     DNApp,
     Menus,
     FlPanel,
     FileCopy,
     Eraser,
     DiskInfo,
{$IFDEF NETINFO}
     NetInfo,
{$ENDIF}
     advance,
     advance1,
     advance2,
     advance3,
     advance4,
     advance7,
     Startup,
     Dialogs,
     Gauges,
     Memory,
     DblWnd,
     Messages,
     HistList,
     FileFind,
     Commands,
     Tree,
     FViewer,
     CmdLine,
     FBB,
     DNStdDlg,
{$IFDEF MEMINFO}MemInfo,{$ENDIF}
{$IFDEF SYSINFO}SysInfo,{$ENDIF}
     FilesCol,
     UserMenu,
     RStrings,
     Colors,
     Microed,
     ed2,
     Editor,
     EdWin,

     {$IFDEF DiskFormat}
     DNFormat,
     Format,
     {$ENDIF}
{$IFDEF Calendar}
     Calendar,    {JO}
{$ENDIF}
    {FStorage,}

{$IFDEF SpreadSheet}
     Calc,
     CellsCol,
{$ENDIF}

{$IFDEF DBView}
     DBView,
     DBWatch,
{$ENDIF}

{$IFDEF CdPlayer}
     CdPlayer ,
     CdUtil ,
{$ENDIF}

     {$IFDEF Reanimator}
     Reanimat,
     {$ENDIF}
     {$IFDEF DiskEdit}
     Fixer,
     {$ENDIF}
     ArcView,
     HelpFile,
     Validate,


     xTime ,


{$IFDEF Modem}
{$IFDEF LINK} NavyLink, {$ENDIF}
     Terminal,
     ScrollBk,
{$IFDEF PHONES} uDialer,{$ENDIF}
     apPort,
     apUART,
{$ENDIF}
{$IFDEF PHONES}
     Phones,
{$ENDIF}

     ASCIITab,
{$IFDEF Game}
     Tetris,
{$ENDIF}
     Drives,
     Archiver,
     Archset,
{$IFNDEF MINARCH}
{$IFNDEF OS2}
     Archread,
{$ENDIF}
{$ENDIF}
     Idlers,
     Arvid,
     FlPanelX,
     WinClp,


{$IFDEF Printer}
     PrintMan,
{$ENDIF}

     Setups,
     ExtraMem,
     RegAll,
     XDblWnd,
     {$IFNDEF NONBP} DiskTool, {$ENDIF}
     CCalc,
     collect,
     DnIni, {-$VIV}
     DnExec
     ;

const
{$IFDEF TrashCan}TrashCan: PTrashCan = nil;{$ENDIF}
  HelpWnd: PHelpWindow = nil;
  HelpInUse: Boolean = False;
  RunMenu: Boolean = Off;
  IdleCounter: Word = 0;
  StartTicks: Word = 0;
  NullStr: byte=0;

var
  LoaderSeg: Word;
  CommandOfs: Word;
  RunFirst: Boolean;
  IgnoreOldFiles: Boolean;
const
  Virgin: Boolean = False; {JO}
  NoTempDir: Boolean = False; {JO}

type
     PDNApplication = ^TDNApplication;
     TDNApplication = object(TApplication)
        IdleClick : TEventTimer ;
        IdleEvt   : TEvent ;
        TreeReader: PTreeReader;

        Pk1,Pk2,Pk3,Pk4 : PView ;

        Constructor Init;
        destructor Done; virtual;
        procedure InitMenuBar; virtual;
        procedure InitCommandLine; virtual;
        procedure InitDesktop; virtual;
        procedure InitStatusLine; virtual;
        procedure ViewFile(AltExt, NoExtFile: boolean; FileName: String); {AK155}
        procedure AddFormat;
        procedure EditFile(Intern: Boolean; FileName: String); {DataCompBoy}
        procedure OutOfMemory; virtual;
        procedure RetrieveDesktop(const FileName: String; LS: PStream; LoadColors: Boolean); {DataCompBoy}
        procedure SaveDesktop(const FileName: String); {DataCompBoy}
        procedure LoadDesktop(var S: TStream);
        procedure StoreDesktop(var S: TStream);
        procedure ChgColors;
        procedure EventError(var Event: TEvent); virtual;
        {$IFDEF DiskFormat}
        procedure RunFormat;
        {$ENDIF}
        procedure HandleCommand(var Event: TEvent);
     end;

function  CheckExit: Boolean;
function  PresentFile(Name: String): PStream; {DataCompBoy}
procedure WriteConfig;
procedure ShowTimeInfo;
Procedure SetSysColors( var Pal);
procedure LoadPalFromFile(const FN: String); {DataCompBoy}
{$IFDEF OS_DOS}
procedure w95QuitInit;     {Gimly}
function  w95QuitCheck:boolean; {Gimly}
procedure w95QuitCancel;
{$ENDIF}

const w95locked:boolean=FALSE; {Gimly}


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
     {                      = 57; !DO NOT USE!}
     cfgNewSystemData       = 58; {DataCompBoy}
     cfgEditorDefaults      = 59; {DataCompBoy}
     cfgNewPanelDefaults    = 60; {DataCompBoy}
     cfgSavers              = 61; {DataCompBoy}
     cfgChangeCaseOptions   = 62; {DataCompBoy}
     cfgINIcrc              = 63; {DataCompBoy}

     dlAbout = {$IFDEF VIRTUALPASCAL} #13#3'DN/2 Open Source'+
     #13#3'Version %s, %s'+
     #13#3'http://os2.ru/projects/dn2/'#13+
     #13#3'Based on: '+ {$ELSE}#13#3+{$ENDIF}
     'Dos Navigator Open Source 1.51.08'#13+
     {$IFDEF LITE}+#3'Light version'#13+{$ENDIF}
     {$IFNDEF VIRTUALPASCAL}
     +#3'Version %s, %s'#13+
     {$ENDIF}
     +#3'http://www.nsk.su/~dnosp/'+
     {$IFDEF VIRTUALPASCAL}
     +#13+
     +#13#3'Thanks to fPrint (UK) Ltd for copy of VP'+
     +#13#3'http://www.vpascal.com'#13
     {$ELSE}
     +#13#13
     {$ENDIF}
     +#13#13 {#3'Based on Dos Navigator' +
     +#13#3'Copyright (C) 1991-99 RIT Research Labs'#13#13};


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

procedure ExecDNAutoexec;

implementation

uses
  HelpKern,
  DNHelp,
  VideoMan,
  {$ifdef modem}
  modemio,
  {$endif}
{$IFDEF CHCOL}
  ColorSel,
{$ENDIF}
{$IFDEF PKTVIEW}
  PktView,
{$ENDIF}
  UFNMatch{, Crt};

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
     {fs: PDirStorage;} {??? зачем нужно - непонятно}
{ var P: PFileRec;}     { куча лишних переменных}
     {DT: DateTime;}
     {I,} Q, Q2: LongInt;
     {DrNm: String;}
     {OW: PString;}
     {AS: String;}

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
                         {$IFDEF Win32}
                            TempFiles^.LFNActive := True;
                         {$ENDIF}
                           {TempFiles^.Owner := @Self;}
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

        {-DataCompBoy-}
function PresentFile(Name: String): PStream;
 var S: PStream;
begin
 if ExistFile(Name) then
  begin
   S := New(PBufStream, Init(Name, stOpenRead, 2048));
   if S^.Status <> stOK then begin Dispose(S,Done); S:=nil; end
  end
 else s:=nil;
 PresentFile := S;
end;
        {-DataCompBoy-}

function CheckExit: Boolean;
 var Event: TEvent;
 function FindQuit(P: PView): Boolean; {$IFDEF BIT_16}far;{$ENDIF}
 begin
  P^.HandleEvent(Event);
  FindQuit := (Event.What = evNothing) or not P^.Valid(cmQuit);
 end;
begin
{ if FormatWindow <> nil then begin CheckExit := False; Exit; end;}
 Event.What := evCommand; Event.Command := cmQuit;
 CheckExit := Desktop^.FirstThat(@FindQuit) = nil;
end;

        {-DataCompBoy-}
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
{$IFDEF OS_DOS}
  ClearFiles('DN*.SWP');
{$ENDIF}
  ClearFiles('$DN*.*');
  ClearFiles('$$DN*.*');
  ClearFiles('$$$DN*.*');
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
  {$IFDEF OS_DOS}
  LoadStream := PresentFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
  if LoadStream = nil then
  {$ENDIF}
    LoadStream := PresentFile(SourceDir+'DN'+GetEnv('DNDSK')+'.DSK');
  if LoadStream <> nil then RetrieveDesktop('', LoadStream, True);
  InitDrivers;

  {-$VOL begin}
  if not RunFirst or CBAutoSave then begin
    ClipBoardStream := GetMeMemoStream;
    LoadStream := PresentFile(SourceDir+'CLIPBOAR'+'.DN');
    if (LoadStream <> nil) and (ClipBoardStream <> nil) then begin
      LoadStream^.Seek(0);
      ClipBoardStream^.CopyFrom(LoadStream^, LoadStream^.GetSize);
      if SystemData.Options and ossUseSysClip <> 0 then SyncClipOut{(Off)};
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
{$IFDEF OS_DOS}
 If ExistFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP') then EraseByName(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
{$ENDIF}
end;
        {-DataCompBoy-}

{$IFDEF DiskFormat}
procedure TDNApplication.RunFormat;
  const FormatInit: Boolean = Off;
  type PR  = record
            OFS,SG : word ;
           end;
       PPR = ^PR ;

  var  P : Pdialog ;
       R : integer;

    Tab : array [0..3] of longint ;
    ADD : pointer ;
    AD  : array[1..2] of word absolute ADD ;

begin

 if FormatBox <> nil then
    begin
      FormatBox^.Select;
      Exit;
    end;

 if not FormatInit then
   begin
      FormatInit := Off;
      DrivesType[0] := DrvType(0); { 5'25 HD }
      DrivesType[1] := DrvType(1); { 3'5  HD }

       MediaListBox := Nil ;
        with FormatDialogData do
          begin
             Ftypes.Data := Nil;
             Field3:=Copy(HistoryStr(hsFmtVolumeL,0),1,11);
          end;
       FillChar( AdvFDDR, 0 , SizeOf(AdvFDDR));
   end;

{$IFNDEF VIRTUALPASCAL}
  if (opSys and opOS2 <> 0) and
     (Confirms and cfFmtOs2Warning <> 0) then
          Msg(dlFmtOs2Warning, nil , mfWarning or mfOkButton);
{$ENDIF}

  FmtUserDefined := False;

  TAB[0] := $83FE9B00 ;
  Tab[1] := $16bacde7 ;
  Tab[2] := $08c7a1c5 ;
  Tab[3] := $19b86e07 ;

  repeat
    P :=  Nil ;
    P := BuildFormatDialog ;
    if P = Nil then Exit ;
    R := ExecuteDialog(P,@FormatDialogData) ;
    DisposeFormatDialogData;
    case R of
       CMok  :  begin
                  Inc(SkyEnabled);
                  FormatDisk;
                  Dec(SkyEnabled);
                  break;
                end;
       CmCancel :  break;
        else {if Registered and rfArchives <> 0 then}
             begin
                P := BuildAdvFormatDialog ;
                R := ExecuteDialog(P,@AdvFormatDialogData) ;
                if R = CmOk then
                  begin
                    FmtUserDefined := True;
                    AdvFormatWarning;
                  end;
                DisposeAdvFormatDialogData;
             end;
         end ; { c.ase }
     until false;
end;
{$ENDIF DiskFormat}

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
procedure SaveRealDsk;
begin
  PDNApplication(Application)^.SaveDesktop(SourceDir+'DN'+GetEnv('DNDSK')+'.DSK');
end;
        {-DataCompBoy-}

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
  {$IFNDEF VIRTUALPASCAL}else SaveDsk{$ENDIF};
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

procedure TDNApplication.EventError;
begin
{$IFDEF SS}
 if (Event.What = evMouseMove) and (Event.Where.Y = 0) and (Event.Where.X = Size.X - 1)
    and (SSaver = nil) and SaversData.Mouse then InsertIdler;
{$ENDIF}
end;

procedure TDNApplication.ChgColors;
begin
{$IFDEF CHCOL}
 ChangeColors;
 WriteConfig;
{$ENDIF}
end;

{JO}
procedure WriteHighlite;
 var F: lText;
begin
 lAssignText(F,SourceDir+'dnhgl.grp');
 lRewriteText(F);
 Writeln(F.T, CustomMask1^.Filter);
 Writeln(F.T, CustomMask2^.Filter);
 Writeln(F.T, CustomMask3^.Filter);
 Writeln(F.T, CustomMask4^.Filter);
 Writeln(F.T, CustomMask5^.Filter);
 Writeln(F.T, CustomMask6^.Filter);
 Writeln(F.T, CustomMask7^.Filter);
 Writeln(F.T, CustomMask8^.Filter);
 Writeln(F.T, CustomMask9^.Filter);
 Writeln(F.T, CustomMask10^.Filter);
 Writeln(F.T, Startup.Archives^.Filter);
 Close(F.T);
end;
{JO}

        {-DataCompBoy-}
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
 WriteHighlite; {JO}
{$ELSE}{!DNPRG}
begin
{$ENDIF}{DNPRG}
end;
        {-DataCompBoy-}

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
  SM: Word;
  CC: Word;
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
  CC := 0;
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
        TempFile := Copy(PJ^,2,MaxStringLength);
        QQ := Pos('|', TempFile); Dec(QQ);
        if QQ>0 then Q:=Copy(TempFile, QQ+2, MaxStringLength) else Q:='';
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
    S^.ReadStrV(FreeStr); {S^.Read(FreeStr,1); S^.Read(FreeStr[1],length(FreeStr));} {Cat}
    S^.ReadStrV(FreeStr); {S^.Read(FreeStr,1); S^.Read(FreeStr[1],length(FreeStr));} {Cat}
    S^.Read(SM, Sizeof(SM));
    S^.Read( OldAppSize, SizeOf( Size ));
    S^.Read( OldDskSize, SizeOf( Size ));
    BB := not (OldAppSize.Equals(Size));
    LoadIndexes(S^);
    GetExtent( SaveBounds );
    Desktop^.Clear;
{$IFDEF OS2}
    if not PMWindowed then
        begin
{$ENDIF}
          if NonVIOScreenMode <> $FFFF then SM := NonVIOScreenMode;
{$IFDEF OS2}
        end
      else
        if VIOScreenMode <> $FFFF then SM := VIOScreenMode;
{$ENDIF}
    if BB and ((SM <> ScreenMode) and
       (StartupData.Load and osuRestoreScrMode <> 0))
      then begin
      SetScrMode(SM);
      {$IFDEF DPMI}
      SetScrMode(SM);
      {$ENDIF}
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

procedure TDNApplication.OutOfMemory;
begin
  Msg(erNotEnoughMemory, nil, mfError + mfOKButton);
end;

const
  ExtFileName: array[boolean] of string[10] =
    ('dn.vwr', 'dnalt.vwr');

        {-DataCompBoy-} {AK155}
procedure TDNApplication.ViewFile(AltExt, NoExtFile: boolean; FileName: String); {AK155}
var
  W : PWindow;
  R : TRect;
  fr: PFileRec;
  up: tUserParams;
  P : PViewRecord;
  I,J : Integer;
  PS: PString;
  q : Integer;
  RN: String;

  function SpecialIntView: Boolean;
    var
      XT: String;
    label
      db;
    begin
    SpecialIntView := false;
    XT := ' '+RN;
    UpStr(XT);
    PS := @XT;
    if (InterfaceData.Options and ouiTrackViewers <> 0) and
       (ViewHistory <> nil)
     then I := ViewHistory^.IndexOf(@PS) else I := -1;
    if I >= 0 then begin
     P := ViewHistory^.At(I);
     Q := P^.fViewMode;
     if Q and vmInternal = vmInternal then
       Q := -1;
    end else Q:=-1;
    XT := UpStrg(GetExt(FileName));
    {$IFDEF DBView}
    if (XT = '.DBF') or (Q=vmDB) then begin
     W := New(PDBWindow,Init(FileName));
     if W = nil then
       exit;
     if (Q=vmDB) and (W<>nil) then
      with P^, W^ do begin
        R.Assign(fOrigin.X, fOrigin.Y, fOrigin.X + fSize.X, fOrigin.Y + fSize.Y);
        AdjustToDesktopSize(R, fDeskSize);
        Locate(R);
       with PDBWindow(W)^.P^ do begin
        Delta := fdDelta;
        Pos   := fdPos;
        KeyMap := fdKeyMap; SetKeyMap; {AL155}
       end;
      end;
    goto db;
    end;
    {$ENDIF}

    {$IFDEF SpreadSheet}
     if (XT = '.WKZ') or (Q=vmSpread) then begin
      W := New(PCalcWindow,Init(R, FileName));
      if (Q=vmSpread) and (W<>nil) then
       with P^, W^ do begin
         R.Assign(fOrigin.X, fOrigin.Y, fOrigin.X + fSize.X, fOrigin.Y + fSize.Y);
         AdjustToDesktopSize(R, fDeskSize);
         Locate(R);
        with PCalcWindow(W)^.CalcView^ do begin
         Delta       := fsDelta;
         Cur         := fsCur;
         Mark        := fsMark;
         CurrentCalc := fsCurrentCalc;
         SearchPos   := fsSearchPos;
         ErrorCell   := fsErrorCell;
         HScroll^.Value := Delta.X + Cur.X;{AK155}
         VScroll^.Value := Delta.Y + Cur.Y;{AK155}
         end;
       end;
     goto db;
     end;
    {$ENDIF}

    {$IFDEF PKTView}
    if (IsPktFile(FileName) or (Q=vmPKT)) and (XT = '.PKT') then
      begin
      SpecialIntView := true;
      ViewPktFile(FileName);
      exit;
      end;
    if (IsMsgFile(FileName) or (Q=vmMSG)) and (XT = '.MSG') then
      begin
      SpecialIntView := true;
      ViewMSGFile(FileName);
      exit;
      end;
    {$ENDIF}
    SpecialIntView := ArcViewer(FileName, RN, FreeByte);
    exit;

db:
    if (W <> nil) and not W^.Valid(0) then
      begin
      Dispose(W,Done); exit;
      end;
{    StoreViewInfo(W);  AK155: сохранять бессмысленно (еще нечего)
        и излишне (сохранит Done) }
    InsertWindow(W);
    SpecialIntView := true;
    end;

  procedure TextView;
   var S: String;
       V: PFileViewer;
  begin
    W := New(PFileWindow,Init(FileName, RN, EditorDefaults.ViOpt and 1 <> 0));
    if (W <> nil) and (W^.Valid(cmValid)) then
      begin
        V := PFileViewer(W^.Current); S := V^.VFileName; System.Insert(' ', S, 1); UpStr(S);
        PS := @S;
        if (InterfaceData.Options and ouiTrackViewers <> 0) and (ViewHistory <> nil)
          then I := ViewHistory^.IndexOf(@PS) else I := -1;
        If I>=0 then
          if (PViewRecord(ViewHistory^.At(I))^.fViewMode and not vmInternal)>vmAsm then I:=-1;
        if I >= 0 then
           begin
             P := ViewHistory^.At(I);
             R.Assign(P^.fOrigin.X, P^.fOrigin.Y,
                      P^.fOrigin.X + P^.fSize.X,
                      P^.fOrigin.Y + P^.fSize.Y);
             AdjustToDesktopSize(R, P^.fDeskSize);
             W^.Locate(R);
             with V^, P^ do
              begin
                if fPos+fBufPos > FileSize then
                begin
                  fPos := 0;
                  fBufPos := 0;
                end;
                Seek(fPos);
                BufPos     := fBufPos;
                if XLatFile<>nil then DisposeStr(XLatFile);
                XLatFile   := NewStr(fXlatFile);
                ViewMode   := fViewMode and not vmInternal;
                Filter     := fFilter;
                HexEdit    := fHexEdit;
                Wrap       := fWrap;
                XDelta     := fXDelta;
                HexPos     := fHexPos;
                Cur        := fCur;
                UseXLat    := fUseXLat;
                KeyMap     := fKeyMap;
                if UseXLat
                 then SetXlatFile(XLatFile^, On)
                 else SetXlatFile('', On);
                MarkPos   := fMarks;
                Loaded    := true;
                MakeLines;
              end;
           end;
      InsertWindow(W);
      end;
  end;

begin
   I:=PosChar('|', FileName);
   If I>0 then begin
    RN:=Copy(FileName, I+1, MaxStringLength);
    SetLength(FileName, I-1);
    J:=PosChar(';', FileName);
    If J>0 then
     SetLength(FileName, J-1); {AK155 удалить номер версии файла в uc2-архиве }
   end;
   FileName:=lFExpand(FileName);
   if I<=0 then RN:=FileName;
   fr:=CreateFileRec(FileName);
   fillchar(up, sizeof(up), 0);
   up.active:=fr;
   Desktop^.GetExtent(R);
   Abort := Off;
   if (Message(DeskTop, evBroadcast, cmFindView, @FileName) <> nil)
     and ((ReuseViewers=2) or
          ((ReuseViewers=1) and
           (MessageBox(GetString(dlOpenNewWindow), nil, mfYesButton+mfNoButton)<>cmYes)
          )
         )
     then begin TempFile:=''; Exit; end;

{ Логика выбора просмотра такая:
  по F3 (not AltExt) - согласно dn.vwr, затем специальный внутренний, затем текстовый;
  по Alt-F3 (AltExt) - согласно dnalt.vwr, затем текстовый;
  по Alt-Shift-F3 (это вообще не здесь) - безусловно текстовый. }

   if not NoExtFile and
       ExecExtFile(ExtFileName[AltExt], @up, dlLoadingViewer) then
     exit;
   if not AltExt and SpecialIntView then
     exit;
   TextView;

end;
        {-DataCompBoy-} {AK155}

        {-DataCompBoy-}
procedure TDNApplication.EditFile;
var
  W: PWindow;
  R: TRect;
  PS: PString;
  V: PFileEditor;
  I: Integer;
  P: PEditRecord;
  fr: PFileRec;
  up: tUserParams;
  s: string;

  function Edit: Boolean;
  begin
    Edit := ExecExtFile('DN.EDT', @up, dlLoadingEditor);
  end;

label ex;
begin
  FileName:=lFExpand(FileName);
  fr:=CreateFileRec(FileName);
  fillchar(up, sizeof(up), 0);
  up.active:=fr;
   if (Intern and (SystemData.Options and ossEditor <> 0)) or
      (not Intern and (SystemData.Options and ossEditor = 0)) or
     not Edit then
      begin
        if (Message(DeskTop, evBroadcast, cmFindEdit, @FileName) <> nil)
           and ((ReuseEditors=2) or
                ((ReuseEditors=1) and
                 (MessageBox(GetString(dlOpenNewWindow), nil, mfYesButton+mfNoButton)<>cmYes)
                )
               )
        then
         begin TempFile:=''; Exit; end;
        if (InterfaceData.Options and ouiStoreEditorPosition <> 0) then
         begin
           if not TempBounds.Empty then R := TempBounds
                                    else begin Desktop^.GetExtent(R); LastEditDeskSize := Desktop^.Size end;
           AdjustToDesktopSize(R, LastEditDeskSize);
         end else Desktop^.GetExtent(R);
        W := New(PEditWindow,Init(R, FileName));
        if (W <> nil) and (W^.Valid(cmValid)) then
          begin
            V := PEditWindow(W)^.Intern;
            FreeStr := V^.EditName; System.Insert(' ', FreeStr, 1); UpStr(FreeStr);
            PS := @FreeStr;
            if (InterfaceData.Options and ouiTrackEditors <> 0) and (EditHistory <> nil)
                then I := EditHistory^.IndexOf(@PS) else I := -1;
            if I >= 0 then
              begin
                P := EditHistory^.At(I);
                R.Assign(P^.fOrigin.X, P^.fOrigin.Y, P^.fOrigin.X + P^.fSize.X, P^.fOrigin.Y + P^.fSize.Y);
                AdjustToDesktopSize(R, P^.fDeskSize);
                W^.Locate(R);
                with V^, P^ do
                  begin
                    MarkPos                := fMarks;
                    Mark.A                 := fBlockStart;
                    Mark.B                 := fBlockEnd;
                    ScrollTo(fDelta.X, fDelta.Y);
                    Pos                    := fPos;
                    BlockVisible           := fBlockVisible;
                    VertBlock              := fVerticalBlock;
                    EdOpt.HiLite           := fHighlight;
                    EdOpt.HiliteColumn     := fHiliteColumn;
                    EdOpt.HiliteLine       := fHiliteLine;
                    EdOpt.AutoIndent       := fAutoIndent;
                    EdOpt.AutoJustify      := fAutoJustify;
                    EdOpt.AutoBrackets     := fAutoBrackets;
                    EdOpt.LeftSide         := fLeftSide;
                    EdOpt.RightSide        := fRightSide;
                    EdOpt.InSide           := fInSide;
                    InsertMode             := fInsMode;
                    KeyMap                 := fKeyMap; {-$VIV}
                    WorkString := GetLine(fDelta.Y); {-$VIV 11.05.99}
                  end;
              end else StoreEditInfo(W);

            InsertWindow(W);
          end;
      end;
ex: DelFileRec(fr);
end;
        {-DataCompBoy-}

procedure TDNApplication.AddFormat;
begin
{ if AddFormatDialog <> cmOK then Exit;
 New(FormatWindow,Init);
 DeskTop^.Insert(FormatWindow);}
end;

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

        {-DataCompBoy-}
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
      While Length(St) < Length(CColor) do St := St + #$3F;
      Application^.GetPalette^ := Copy(St,1,Length(CColor));
      SetSysColors(Application^.GetPalette^);
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

        {-DataCompBoy-}
PROCEDURE TDNApplication.HandleCommand;

 var R: TRect;
     P, P1: PView;
     ST: String;

     HFile: PHelpFile;
     HelpStrm: PDosStream;
     T : Tevent ;
{$IFDEF VIRTUALPASCAL}
     CC: Word;
     F: lFile;
     PJ,Q: String; QQ: integer;
{$ENDIF}
 procedure OpenWindow(ChDrive: Boolean);
  var S: String;
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
  if Abort then Dispose(P,Done) else InsertWindow(PWindow(P));
{$IFDEF TrashCan}
  if TrashCan^.ImVisible then Desktop^.Insert(TrashCan);
{$ENDIF}
 end;


 procedure OpenTreeWindow;
  var S: String;
 begin
   Desktop^.GetExtent(R);
   S := ChangeDir(GetString(dlSelectDirectory), 0);
   if S = '' then Exit; ClrIO;
   lChDir(S); ClrIO;
   S[1] := Char(Byte(S[1])-64);
   P := New(PXDoubleWindow, Init(R, 0, Byte(S[1])));
   Message(P, evCommand, cmDirTree, nil);
   if Abort then Dispose(P,Done) else InsertWindow(PWindow(P));
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
{$IFNDEF OS2}
   P: PMenuBox;
   M: PMenu;
   R: TRect;
   I: PMenuItem;
{$ELSE}
   R: record
        VM: Integer;
        Cols, Rows: String[3];
      end;
{$ENDIF}
   W: Word;
 begin
{$IFNDEF VIRTUALPASCAL}
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
   M^.Default:=LookUpMenu(M, (ScreenMode shr 4) - $7, dfByPosition);
   R.Assign(1,1,20,4);
   P:=New(PMenuBox, Init(R, M, nil));
   P^.Options:=P^.Options or ofCentered;
   P^.HelpCtx:=hcCustomVideo;
   W:=Application^.ExecView(P);
   Dispose(P);
   DisposeMenu(M);
   if (W=0) or (W=cmCancel) then exit;
   Dec(W, cmSwitch);
{$ELSE}
 {$IFNDEF OS2}
   I := nil;
   M := NewVideoMenu (
          NewItem ( '~1~ 80 x 25', '', kbNoKey, sm80x25+cmSwitch, hcNoContext,
          NewItem ( '~2~ 80 x 30', '', kbNoKey, sm80x30+cmSwitch, hcNoContext,
          NewItem ( '~3~ 80 x 34', '', kbNoKey, sm80x34+cmSwitch, hcNoContext,
          NewItem ( '~4~ 80 x 43', '', kbNoKey, sm80x43+cmSwitch, hcNoContext,
          NewItem ( '~5~ 80 x 50', '', kbNoKey, sm80x50+cmSwitch, hcNoContext,
          NewItem ( '~6~ 80 x 60', '', kbNoKey, sm80x60+cmSwitch, hcNoContext,
          I))))))
        );
   R.Assign(1,1,20,4);
   P:=New(PMenuBox, Init(R, M, nil));
   P^.Options:=P^.Options or ofCentered;
   P^.HelpCtx:=hcCustomVideo;
   W:=Application^.ExecView(P);
   Dispose(P);
   DisposeMenu(M);
   if (W=0) or (W=cmCancel) then exit;
   Dec(W, cmSwitch);
 {$ELSE}
   case ScreenMode of
 sm80x30: R.VM := 1;
 sm80x34: R.VM := 2;
 sm80x43: R.VM := 3;
 sm80x50: R.VM := 4;
 sm80x60: R.VM := 5;
$140A..$FFFE: R.VM := 6;
     else R.VM := 0;
   end;
   R.Cols := ItoS(ScreenWidth);
   R.Rows := ItoS(ScreenHeight);
   if ExecResource(dlgChScreenMode, R) <> cmOK then Exit;
   case R.VM of
 0: W := sm80x25;
 1: W := sm80x30;
 2: W := sm80x34;
 3: W := sm80x43;
 4: W := sm80x50;
 5: W := sm80x60;
 6: W := StoI(R.Cols) * 256 + StoI(R.Rows);
   end;
 {$ENDIF}
{$ENDIF}
   {PZ begin 2000.06.29}
   SetScrMode (W);
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
   MessageBox2( dlAbout , ^C'Based on Dos Navigator'#13+
                + ^C'Copyright (C) 1991-99 RIT Research Labs'#13#13+
                + ^C'This product is a FREEWARE',@D,Nil, mfAbout + mfOKButton );
   DisposeStr(D[1]);          DisposeStr(D[2]);
 {$ENDIF}
 end;

 {$IFDEF SpreadSheet}
         {-DataCompBoy-}
 procedure LoadSheet(const SheetName: String);
 begin
  DeskTop^.GetExtent(R);
  DeskTop^.Insert(ValidView(New(PCalcWindow, Init(R, SheetName))));
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

 var S: TDOSStream;
     FN: String;
     Pal: PString;
     W: Word;
    {WW: Word;
     PW: PWindow;}
     vId: LongInt;

 procedure StoreColors;
 begin
  FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL', GetString(dlStoreColorPal), GetString(dlFileName),
                          fdOKButton + fdHelpButton, hsColors);
  if FN = '' then Exit;
  S.Init(FN, stCreate);
  if S.Status = 0 then
   begin
    Pal := PString(GetPalette);
    S.WriteStr(Pal);
    StoreIndexes(S);
    vID := $50414756 ; { VGAP }
    S.Write( vID , SizeOF( vID )) ;
    S.Write( VGA_Palette, SizeOF( VGA_Palette )) ;
    vID := $4B4E4C42 ; { BLNK }
    S.Write( vID , SizeOF( vID )) ;
    S.Write( CurrentBlink , SizeOf( CurrentBlink ) );
   end;
 {else PlaySound(1559, 30);}
  S.Done;
  FN := GetPath(FN); RereadDirectory(FN);
 end;

{ procedure LoadColors;
 begin
  asm int 3 end;
  FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL', GetString(dlLoadColorPal), GetString(dlFileName),
                          fdOKButton + fdHelpButton, hsColors);
  if FN = '' then Exit;
  LoadPalFromFile(FN);
 end;}

 procedure LoadColors; {JO}
   var More: Boolean;
       None: Boolean;
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

{$IFNDEF Win32}
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
   RunOS2Command(Copy(S^,2,MaxStringLength), B, ST);
   CmdLine.StrModified := On;
   Message(CommandLine, evKeyDown, kbDown, nil);
   ClearEvent(Event);
 end;
{$ENDIF}

 function ExecCommandLine: Boolean;
  var S, {$IFDEF RecodeWhenDraw}S1,{$ENDIF}SD: String; {AK155}
      up: TUserParams;
 begin
  ExecCommandLine := Off;
 {S := '';}
  CommandLine^.GetData(S);

  if (DelSpaces(S) = '') then
    begin
      SD:=''; CommandLine^.SetData(SD); CommandLine^.DrawView; {JO}
      Exit;
    end;
  ExecCommandLine := On;
  if HandleChDirCommand then
   begin
    SD := S; {AK155}
    DelLeft(S); DelRight(S);
    if (S[1] in ['c','C']) and (S[2] in ['d','D']) and (S[3]=' ')
    then
     begin
      System.Delete(S, 1, 2); {DelFC(S); DelFC(S);} DelLeft(S); S:=DelSquashes(S);
      if PathExist(S) then
       Message(Application, evBroadcast, cmChangeDirectory, @S);
      SD:=''; CommandLine^.SetData(SD); CommandLine^.DrawView; {JO}
      exit;
     end;
    if (Length(S) = 2) and ValidDrive(UpCase(S[1])) and (S[2]=':') then
    begin
     if s[1]='*' then s:=cTEMP_;
     {Cat} S[1]:=UpCase(S[1]); {/Cat}
     Message(Desktop, evBroadcast, cmChangeDrv, @S);
     SD:=''; CommandLine^.SetData(SD); CommandLine^.DrawView; {JO}
     exit;
    end;
    S := SD; {AK155 CommandLine^.GetData(S);}
   end;
{$IFDEF OS_DOS} {AK155}
  if not CheckExit then begin ClearEvent(Event); Exit end;
{$ENDIF}
{$IFNDEF Win32}
  if TryRunOS2(@S) then Exit;
{$ENDIF}
{AK155 очистка комстроки нужна всегда, поэтому она вынесена в общую         }
{      часть. Очищать лучше перед выполнением, а не после, так как в        }
{      ExecString она запоминается и восстанавливается. Аналогичная вставка }
{      сделана в dnexec.ExecFile.RunCommand                                 }
{JO:  перенёс очистку комстроки после TryRunOS2, так как иначе оно но срабатывает, и при }
{     запуске набранного в коммандлайне не получается запустить в отдельной сессии       }
  SD:=''; CommandLine^.SetData(SD); CommandLine^.DrawView;
{AK155}
  Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
{$IFDEF RecodeWhenDraw}
  S1 := OemToCharStr(S);
  ExecString(@S1, #13 + CharToOemStr(ActiveDir)+'>' + S);
{$ELSE}
  ExecString(@S, #13 + ActiveDir+'>' + S);
{$ENDIF}
{AK155     S:='';CommandLine^.SetData(S);CommandLine^.DrawView;}
 end;

 procedure ExecDOSCmd;
   var Nm: String;
       Xt: String;
 begin
   ST := '';
   Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
   lFSplit(CnvString(CurFileActive), FreeStr, Nm, Xt);
   if InExtFilter(CnvString(CurFileActive), Executables) then ST := Nm+Xt+' ';
   if InputBox(GetString(dlExecDOScmdHdr), GetString(dlExecDOScmdLine), ST, 128, hsExecDOSCmd) <> cmOK then Exit;
  {$IFNDEF Win32}
    if not TryRunOS2(@ST) {$IFDEF OS_DOS} and CheckExit {$ENDIF} {JO}
     then
  {$ENDIF}
          ExecString(@St, #13#10 + ActiveDir+'>' + ST);
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

 procedure Denie;
 begin
   ExecResource( dlgDenie, FreeStr );
 end;

 procedure Rebound;
 begin
  GetExtent(R);
  if InterfaceData.Options and ouiHideMenu = 0 then Inc(R.A.Y);
  if SystemData.Options and ouiHideStatus = 0 then Dec(R.B.Y);
  Dec(R.B.Y);
  Desktop^.Locate(R);
  R.A.Y := R.B.Y; Inc(R.B.Y);
  CommandLine^.Locate(R);
 end;

 procedure RSearchAdvance;
     begin
      if ExecResource(dlgAdvanceSearch, AdvanceSearchData) = cmOK then
        begin
          PView(Desktop^.Current {Event.InfoPtr})^.GetData(FindRec);
          FindRec.Options := FindRec.Options or ffoAdvanced;
          PView(Desktop^.Current {Event.InfoPtr})^.SetData(FindRec);
        end;
       ClearEvent(Event);
     end;

{Cat: переписал для поддержки длинных строк в Clipboard-е}
 procedure GetFromClip;
 begin
   if SystemData.Options and ossUseSysClip <> 0 then
     SyncClipOut{(On)};
   if (MicroEd.Clipboard = nil) or (MicroEd.Clipboard^.At(0) = nil) then
     PString(Event.InfoPtr)^ := ''
   else
     PString(Event.InfoPtr)^ := PLongString(MicroEd.Clipboard^.At(0))^;
 end;

 procedure GetFromClipLong;
 begin
   if SystemData.Options and ossUseSysClip <> 0 then
     SyncClipOut{(On)};
   if (MicroEd.Clipboard = nil) or (MicroEd.Clipboard^.At(0) = nil) then
     PLongString(Event.InfoPtr)^ := ''
   else
     PLongString(Event.InfoPtr)^ := PLongString(MicroEd.Clipboard^.At(0))^;
 end;

 procedure PutInClip;
 begin
   if MicroEd.Clipboard <> nil then
     Dispose(MicroEd.Clipboard,Done);
   MicroEd.Clipboard := New(PLineCollection, Init(1,1,True));
   MicroEd.Clipboard^.Insert(NewLongStr(PString(Event.InfoPtr)^));
   if SystemData.Options and ossUseSysClip <> 0 then
     SyncClipIn;
   if ClipBoardStream<>nil then
     ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-{$IFDEF USELONGSTRING} 4 {$ELSE} 1 {$ENDIF}, 0));
   CopyLines2Stream(MicroEd.Clipboard, ClipBoardStream);
 end;

 procedure PutInClipLong;
 begin
   if MicroEd.Clipboard <> nil then
     Dispose(MicroEd.Clipboard,Done);
   MicroEd.Clipboard := New(PLineCollection, Init(1,1,True));
   MicroEd.Clipboard^.Insert(NewLongStr(PLongString(Event.InfoPtr)^));
   if SystemData.Options and ossUseSysClip <> 0 then
     SyncClipIn;
   if ClipBoardStream<>nil then
     ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-{$IFDEF USELONGSTRING} 4 {$ELSE} 1 {$ENDIF}, 0));
   CopyLines2Stream(MicroEd.Clipboard, ClipBoardStream);
 end;
{/Cat}

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
    ClearEvent(Event);
  end;

  Procedure SearchAdvance;
  begin
    RSearchAdvance
  end;

    procedure ToggleCmdLine;
      var R: TRect;
          B: Boolean;
    begin
      if CommandLine = nil then Exit;
      B := (not CommandLine^.GetState(sfVisible)) and (Str <> '');
      GetExtent( R );
      if InterfaceData.Options and ouiHideMenu = 0 then Inc(R.A.Y);
      if InterfaceData.Options and ouiHideStatus = 0 then Dec(R.B.Y);
      if B then Dec( R.B.Y );
      Desktop^.Locate( R );
      R.A.Y := R.B.Y;
      R.B.Y := R.A.Y + Byte(B);
      CommandLine^.Locate( R );
      CommandLine^.SetState( sfVisible, B);
    end;

    procedure CE; begin ClearEvent(Event) end;

    procedure DoExecFile;
    begin
      if
       {$IFNDEF Win32}
        not TryRunOS2(PString(Event.InfoPtr)) {$IFDEF OS_DOS} and CheckExit {$ENDIF} {JO}
         and
       {$ENDIF}
             not ExecCommandLine then ExecFile(PString(Event.InfoPtr)^);
      CE;
    end;

    procedure DoExecString;
    begin
     {$IFNDEF Win32}
      if not TryRunOS2(PString(Event.InfoPtr)) {$IFDEF OS_DOS} and CheckExit {$ENDIF} {JO}
       then
     {$ENDIF}
            ExecString(PString(Event.InfoPtr), '');
      CE;
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
{$IFDEF VIRTUALPASCAL}
       if (Confirms and cfChaneLngWarning <> 0) then
          Msg(dlChangeLngWarning, nil , mfWarning or mfOkButton);
{$ENDIF}
        L.Init(5,5,False);
        S:=SourceDir; if S[Length(S)]<>'\' then S:=S+'\';
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
        S:=StartupDir; if S[Length(S)]<>'\' then S:=S+'\';
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
            LngMixCase(ActiveLanguage)) {$IFDEF OS_DOS} and CheckExit {$ENDIF}{JO} then begin
                ActiveLanguage:=PString(L.At(CurIdx))^;
                if ActiveLanguage=LngMixCase(GetEnv('DNLNG'))
                then ActiveLanguage:='';
                SaveDnIniSettings(@ActiveLanguage);
                DoneIniEngine;
                ProbeINI(INIstoredtime,INIstoredsize,INIstoredcrc);
                ConfigModified:=True;
{$IFNDEF VIRTUALPASCAL}
                StringCache^.FreeAll;
                ExecString(@NullStr,'');
{$ENDIF}
            end;
        end;
        L.FreeAll;
        L.Done;
    end;

{$IFDEF VIRTUALPASCAL}
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

{$ENDIF}

begin
   case Event.Command of
     cmFirstTimePanel: OpenWindow( False );
     cmGetFromClipboard: GetFromClip;
     cmGetFromClipboardLong: GetFromClipLong;
     cmPutInClipboard: PutInClip;
     cmPutInClipboardLong: PutInClipLong;
     cmSearchAdvance: SearchAdvance;
     cmListOfDirs: DoQuickChange;
     {$IFDEF DiskFormat}
       cmFormatDisk: RunFormat;
     {$ENDIF}
     {$IFDEF Printer}
       cmFilePrint: PrintFile(PString(Event.InfoPtr)^);
     {$ENDIF}
     {$IFDEF PrintManager}
       cmSetupPrinter: SetupPrinter;
     {$ENDIF}
     cmOpenSmartpad: OpenSmartpad;
     cmOpenClipBoard: OpenClipBoard; {-$VOL}
     cmExecGrabber: ScreenGrabber(On);
     {$IFDEF MODEM}
     cmTerminalDefaults: if TerminalSetup and (Term <> nil) then
                           Move(TerminalDefaults, Term^.Emulator, 4);
     {$ENDIF}
     cmSetupConfirmation: ConfirmSetup;
     cmCountrySetup: SetupCountryInfo;
     cmColumnDefaultsDisk: if SetupColumnDefaultsDisk then begin WriteConfig; Application^.Redraw; end;
     cmColumnDefaultsTemp: if SetupColumnDefaultsTemp then begin WriteConfig; Application^.Redraw; end;
     cmColumnDefaultsFind: if SetupColumnDefaultsFind then begin WriteConfig; Application^.Redraw; end;
     cmColumnDefaultsArch: if SetupColumnDefaultsArch then begin WriteConfig; Application^.Redraw; end;
     cmColumnDefaultsArvd: if SetupColumnDefaultsArvd then begin WriteConfig; Application^.Redraw; end;
     cmHighlightGroups: SetHighlightGroups;
     {$IFDEF DBView}
        cmDBFView: InsertWindow(New(PDBWindow,Init(CnvString(Event.InfoPtr))));
     {$ENDIF}
     {$IFDEF SpreadSheet}
        cmNewTable: LoadSheet('');
        cmSheetLoad: OpenSheet;
        cmWKZView: begin
                     DeskTop^.GetExtent(R);
                     InsertWindow(New(PCalcWindow,Init(R, CnvString(Event.InfoPtr))));
                   end;
     {$ENDIF}
     {$IFDEF PHONES}
        cmPhoneBook: PhoneBook(Off);
     {$ENDIF}
     {$IFDEF Modem}
     {$IFDEF LINK}
        cmNavyLink: StartLink;
     {$ENDIF}
     {$IFDEF PHONES}
        cmUndial: PhoneBook(On);
     {$ENDIF}
        cmSetupModem: SetupModem;
        cmTerminal: OpenTerminal;
        cmHangUp: if COMPort <> nil then
                   begin
                    P := WriteMsg(GetString(dlDisconnect));
                    HangUp;
                    {$IFDEF PHONES}
                    if Dialer <> nil then Dialer^.Free;
                    {$ENDIF}
                    if Term <> nil then Term^.Owner^.Redraw;
                    if P <> nil then P^.Free;
                   end;
     {$ENDIF}
     {$IFDEF DiskEdit}
        cmDiskEdit: if CheckDisk(GetCurDrive)
                      then InsertWindow(New(PFixerWindow, Init))
                      else Denie;
     {$ENDIF}
     cmTextView, cmHexView: begin
                             ST:=CnvString(Event.InfoPtr);
                             W:=PosChar('|', ST);
                             if W>0
                              then InsertWindow(New(PFileWindow,
                                                    Init(Copy(ST, 1, W-1),
                                                         Copy(ST,W+1,MaxStringLength),
                                                         Event.Command = cmHexView)))
                              else InsertWindow(New(PFileWindow,
                                                    Init(ST, ST,
                                                         Event.Command = cmHexView)))
                            end;
     {$IFDEF MODEM}
     cmAdvancePortSetup: if ExecResource( dlgAdvancedCOMSetup, AdvModemData) = cmOK then
                            begin
                              StoreModemSetup;
                            end;
     {$ENDIF}
     cmSystemSetup: SystemSetup;
     cmRSearchAdvance : RSearchAdvance ;
     cmInterfaceSetup: InterfaceSetup;
     cmStartup: StartupSetup;
     cmSetupMouse: MouseSetup;
{$IFDEF SS}
     cmSaversSetup: SaversSetup;
{$ENDIF}
{$IFDEF TrashCan}
     cmHideShowTools: if TrashCan^.ImVisible
                        then begin TrashCan^.Hide; TrashCan^.ImVisible:=false; end
                        else begin TrashCan^.Show; TrashCan^.MakeFirst;
                                   TrashCan^.ImVisible:=true;                  end;
{$ENDIF}
{$IFDEF Reanimator}
     cmReanimator: if CheckDisk(GetCurDrive) then OpenReanimator else Denie;
{$ENDIF}
     cmEditHistory: EditHistoryMenu;
     cmViewHistory: ViewHistoryMenu;
{$IFDEF DiskEdit}
     cmSetVolumeLabel: if CheckDisk(GetCurDrive) then SetVLabel else Denie;
{$ENDIF}
{$IFDEF OS2}
     cmSetVolumeLabel: SetVLabel;
{$ENDIF}
     cmXEditFile: OpenEditor;
     cmWindowManager: WindowManager;
     cmUserMenu, cmGlobalUserMenu: if (ExecUserMenu(Event.Command=cmGlobalUserMenu)) then
                begin
                  ST := FreeStr;
                  {$IFDEF OS_DOS} if CheckExit then {$ENDIF} {JO}
                  begin
                  {$IFNDEF OS2}
                   ST:=SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT ' + ST;
                  {$ELSE}
                   ST:=SwpDir+'$DN'+ItoS(DNNumber)+'$.CMD ' + ST;
                  {$ENDIF}
                   ExecString(@ST,'');
                  end;
                end;
{$IFNDEF MINARCH}
{$IFNDEF OS2}
     cmReadArchive: begin ReadArcList; end;
{$ENDIF}
{$ENDIF}
{$IFDEF SS}
     cmScreenRest: InsertIdler;
{$ENDIF}
     cmChScreenMode:SelectVideoModeDialog;
     cmASCIITable: ASCIITable;
     cmAbout: begin
                ClearEvent(Event);
                MessageBoxAbout;
                Exit;
              end;
     cmCalculator: InsertCalc;
{$IFDEF Calendar}
     cmCalendar: InsertCalendar;    {JO}
{$ENDIF}
     cmRefresh: Redraw;
     cmClearDesktop: GlobalMessage(evCommand, cmClose, nil);
     cmFileView: ViewFile(Off, Off, CnvString(Event.InfoPtr));
     cmFileEdit: EditFile(On, CnvString(Event.InfoPtr));
    {cmViewFilter: ViewFile(On, On, CnvString(Event.InfoPtr));}
     cmViewFilter: ViewFile(Off, On, CnvString(Event.InfoPtr)); {JO для распознания фильтрованных файлов как архивов}
     cmIntFileView: ViewFile(On, Off, CnvString(Event.InfoPtr));
     cmIntFileEdit: EditFile(Off, CnvString(Event.InfoPtr));
     cmEditQuickRun: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*MaxStringLength)+'dn.xrn');
     cmExtFileEdit: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*MaxStringLength)+'dn.ext');
     cmMenuFileEdit: EditFile(True, SourceDir+'dn.mnu');
     cmLocalMenuFileEdit: EditFile(True, 'dn.mnu');
     cmEditHGL: EditFile(True, SourceDir+'dn.hgl');
     cmEditSPF: EditFile(True, SourceDir+'dn.spf');
     cmEditINI: EditFile(True, SourceDir+'dn.ini');
     cmExternalViewers: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*MaxStringLength)+'dn.vwr');
     cmExternalEditors: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*MaxStringLength)+'dn.edt');
     cmShowUserScreen: ShowUserScreen;
{$IFDEF OS_DOS}
      {-DataCompBoy-}
     cmRestart: if not CheckExit
                 then begin ClearEvent(Event); Exit end
                 else begin
                  StringCache^.FreeAll;
                  ExecString(@NullStr,'');
                 end;
      {-DataCompBoy-}
{$ENDIF}
     cmCreatePanel: OpenWindow(True);
     cmCreateTree: OpenTreeWindow;
     cmFormatDisk: AddFormat;
     cmFindFile: FindFile;
{$IFDEF SYSINFO}
     cmSystemInfo: SystemInfo;
{$ENDIF}
{$IFDEF MEMINFO}
     cmMemoryInfo: MemoryInfo;
{$ENDIF}
     cmShowOutput: GetUserScreen;
     cmHistoryList: CmdHistory;
     cmLoadDesk:  RetrieveDesktop(SourceDir+'DN'+GetEnv('DNDSK')+'.DSK', nil, True);
{$IFDEF VIRTUALPASCAL}
     cmRetrieveSwp: {begin
                     LoadStream := PresentFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
                     if LoadStream <> nil then RetrieveDesktop('', LoadStream, True);
                     If ExistFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP') then EraseByName(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
                    end;}
     if PosChar(TempFileSWP[1], '+-=<>|[!') = 0  then
      begin lAssignFile(F, TempFileSWP); lSetFAttr(F, $20);  lEraseFile(F); end
      else
  begin
        CC := 0;
        PJ := TempFileSWP;
        TempFile := Copy(PJ,2,MaxStringLength);
        QQ := Pos('|', TempFile); Dec(QQ);
        if QQ>0 then Q:=Copy(TempFile, QQ+2, MaxStringLength) else Q:='';
        QQ := QQ and 255; {BP bugfix by piwamoto}
        If Not ExistFile(TempFile) then TempFile:=GetfURZ(Copy(PJ,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=GetfURZ2(Copy(PJ,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=GetGLUKName(Copy(PJ,2,QQ));
        If Not ExistFile(TempFile) then TempFile:=Copy(PJ,2,QQ);
        case PJ[1] of
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
{$ENDIF}
     cmSaveDesk: SaveRealDsk;
     cmExecFile: DoExecFile;
     cmExecString: DoExecString;
     cmExecCommandLine: if ExecCommandLine then CE;
     cmShowTimeInfo: ShowTimeInfo; {DataCompBoy}
     {$IFDEF CHCOL}
     cmChangeColors: ChgColors;
     {$ENDIF}
{$IFNDEF VIRTUALPASCAL}
     cmChangeUserMode1: if ScreenMode <> smSVGALo then SetScrMode(smSVGALo) else SetScrMode(StartupMode);
     cmChangeUserMode2: if ScreenMode <> smSVGAHi then SetScrMode(smSVGAHi) else SetScrMode(StartupMode);
{$ELSE}
     cmChangeUserMode1: if ScreenMode <> smSVGALo then SetScrMode(smSVGALo) else SetScrMode(sm80x25);
     cmChangeUserMode2: if ScreenMode <> smSVGAHi then SetScrMode(smSVGAHi) else SetScrMode(sm80x25);
{$ENDIF}
     cmChangeMode: begin
                     case ScreenMode of
                       sm80x25: if VideoType = vtEGA then
                                  SetScrMode ( sm80x43 )
                                else
                                  SetScrMode ( sm80x50 );
                     else SetScrMode ( sm80x25 );
                     end;
                   end;

     cmLoadColors: LoadColors;
     cmSetupArchive: SetupArchive(DefaultArchiver);
     cmUpdateArcFile: UpdateARH(nil);
     cmLoConfigArchiver..cmHiConfigArchiver: SetupArchive(Event.Command);
     cmFMDefaults: FMDefaults;
     cmFMSetup: FMSetup;
     cmDriveInfoSetup: DriveInfoSetup;
     cmExecuteDOScmd: ExecDOSCmd;
     cmEditorDefaults: SetupEditorDefaults;
    {$ifdef cdplayer}
     cmPlayCD: RunPlayer;
    {$endif}
     cmStoreColors: StoreColors;
     {$IFDEF Game}
     cmGame: if Game = nil then InsertWindow(New(PGameWindow, Init))
                           else Game^.Owner^.Select;
     {$ENDIF}
     {From  GetEvent}

     cmHideCmdLine: ToggleCmdLine;

     cmGetTeam: if Desktop^.TopView^.HelpCtx = hcAboutDialog then
                     begin
                       Desktop^.TopView^.GetExtent(R);
                       R.Grow(-1,-2); Dec(R.B.Y,2);
                       New(TeamView, Init(R));
                       PGroup(Desktop^.TopView)^.Insert(TeamView);
                       Desktop^.TopView^.HelpCtx := hcTeam;
                     end;
        cmQuit: begin
                  if Confirms and cfExitConfirm <> 0 then
                   W := Msg(dlQueryExit, nil, mfYesNoConfirm)
                   else W := cmYes;
                  if (W <> cmYes) or not CheckExit then
                  begin
                    {$IFDEF OS_DOS}
                    if w95locked then w95QuitCancel; {Gimly}
                    {$ENDIF}
                    ClearEvent(Event);
                  end;
                end;
        cmGetCmpNfo:
                begin
                  ClearEvent(Event);
                  Event.InfoPtr := WriteMsg( GetString( dlComparing ));
                end;
        cmNewStrColl:
                   begin
                      ClearEvent(Event);
                      Event.InfoPtr := New(PStringCollection, Init($80,$40,False));
                   end;
{$IFDEF DiskFormat}
        cmRFormat: begin
                      ClearEvent(Event);
                      Event.InfoPtr := BuildAdvFormatDialog;
                   end;
{$ENDIF}
 cmHelp,cmHelp2: Begin
                  ClearEvent(Event);
                  if (not HelpInUse) or (HelpWnd=nil) then
                  begin
                   HelpInUse := True;
                   HelpStrm := New(PDosStream,
                                   Init(SourceDir+HelpLngId + '.HLP', stOpenRead{stOpenPacked})
                                  );
                   if HelpStrm^.Status <> stOk then
                     begin
                       Dispose(HelpStrm,Done);
                       HelpStrm := New(PDosStream,
                                       Init(StartupDir+HelpLngId + '.HLP', stOpenRead{stOpenPacked})
                                      );
                     end;
                   HFile := New(PHelpFile, Init(HelpStrm));
                   if HelpStrm^.Status <> stOk then
                   begin
                     Msg(erCantOpenHelp, nil, mfError + mfOkButton);
                     Dispose(HFile,Done);
                     HFile:=nil;
                     HelpStrm:=nil;
                   end
                   else
                   begin
                    if Event.Command=cmHelp
                     then HelpWnd := PHelpWindow(ValidView(New(PHelpWindow,Init(HFile, GetHelpCtx))))
                     else HelpWnd := PHelpWindow(ValidView(New(PHelpWindow,Init(HFile, Word(Event.InfoWord)))));
                    if HelpWnd<>nil then begin
                     if ((DeskTop^.Current<>nil)
                         and (
                          (Desktop^.Current^.GetState(sfModal)) or
                          (Desktop^.Current^.GetState(sfDragging))
                         )
                        ) or MenuActive
                      then ExecView(HelpWnd)
                      else begin
                            HelpWnd^.SetState(sfModal, false);
                            HelpWnd^.Options := HelpWnd^.Options or ofTileable or ofTopSelect;
                            Application^.InsertWindow(HelpWnd);
                            Exit;
                           end;
                    end;
                   end;
                   HelpWnd^.Free;
                   HelpInUse := False;
                  end else begin
                   if Event.Command=cmHelp2 then HelpWnd^.GotoContext(Word(Event.InfoWord));
                   if ((DeskTop^.Current<>nil)
                       and (Desktop^.Current^.GetState(sfModal))
                      ) or MenuActive
                    then begin
                          HelpWnd^.SetState(sfModal, true);
                          P:=HelpWnd^.Next;
                          P1:=DeskTop^.Current;
                          Desktop^.Delete(HelpWnd);
                          ExecView(HelpWnd);
                          Desktop^.InsertBefore(HelpWnd, P);
                          Desktop^.SetCurrent(P1, EnterSelect);
                         end
                    else HelpWnd^.Focus;
                  end;
                 end;
        cmTree: ExecTree;
        cmClearData: begin
                       FillChar(FreeStr, SizeOf(FreeStr), 0);
                       PView(Event.InfoPtr)^.Owner^.SetData(FreeStr);
                       ClearEvent(Event);
                     end;
         cmChLngId: ChLngId;
   end;
end;
        {-DataCompBoy-}

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

Procedure SetSysColors( var Pal);
type
 PLL = array[0..255] of byte;
  begin
     {!!!
       move(Pll(Pal)[224],ShadowAttr,1);
       move(Pll(Pal)[225],SysColorAttr,2);
       move(Pll(Pal)[227],SysColorButtonAttr,2);
     }
  end;

{$IFDEF OS_DOS}
        {Gimly}
const w95QuitEnabled:BYTE=1;

procedure w95QuitInit; assembler;
asm
   MOV AL, OpSys {Check Win95}
   AND AL, opWin
   JNZ @@EnableEvent

   MOV AL, OpSys {Check WinNT and W2K}
   AND AL, opWNT
   JNZ @@EnableEvent

   XOR AL, AL
   MOV w95QuitEnabled, AL
   JMP @@E

@@EnableEvent:
{ Enablind close event handling... }
   mov ax, 168fh
   mov dx, 1
   int 2fh

@@E:
end;

function w95QuitCheck:boolean; assembler;
asm
   MOV AL, w95QuitEnabled
   OR  AL, AL
   JZ  @@No

{Checking DN.INI setting...}
   MOV AL, SmartWindowsBoxClose
   OR  AL, AL
   JZ  @@No

{ Checking the close button }
   MOV AX, 168Fh
   MOV CX, AX
   MOV DX, 100h
   INT 2fh
   CMP AX, CX
   JZ  @@No

{ Pressed - preventing the Windows message from appearing }
   MOV AX, 168Fh
   MOV DX, 200h
   INT 2fh

   MOV AL, 1
   MOV w95locked, AL

{ Just disabling close ;) }
   MOV AX, 168Fh
   MOV DX, 0h
   INT 2fh

   MOV AX, 1 { "X" button pressed }
   JMP @@E
@@No:
   XOR AX, AX
@@E:;
end;

procedure w95QuitCancel;
begin
  asm
{ Enabling the cancel button ;) }
   mov ax, 168Fh
   mov dx, 1
   int 2fh

{ Cancelling close }
   mov ax, 168Fh
   mov dx, 300h
   int 2fh
  end;
  w95locked := FALSE;
end;
     {Gimly}
{$ENDIF}

procedure ExecDNAutoexec;
var I:integer; S: String;
begin
 if RunFirst then
 begin
  I := FindParam('/E');
  if I > 0 then
   begin
    S := Copy(ParamStr(I), 3, MaxStringLength);
    ExecString(@S, '');
   end
 end;
end;

end.
