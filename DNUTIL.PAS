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

{$I STDEFINE.INC}
{$I DN.DEF}

unit DNUtil;

interface
uses
     Dos,

     LFN,    {DataCompBoy}
     LFNCol, {DataCompBoy}

     Drivers,
     Objects,
     Views,
     Histries,
     Scroller,
     DNApp,
     Menus,
     FlPanel,
     FileCopy,
     Eraser,
     DiskInfo,
     NetInfo,
     advance,
     advance1,
     advance2,
     advance3,
     advance4,
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
     MemInfo,
     FilesCol,
     UserMenu,
     RStrings,
     {ColorSel,}
     Colors,
     Editor,
     Microed,
     DNFormat,
     Format,
     FStorage,
     ObjType,

{$IFDEF DEMO}
     Demo,
{$ENDIF}
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
{$ENDIF}

     Reanimator,
     Fixer,
     ArcView,
     HelpFile,
     Validate,


     xTime ,


{$IFDEF Modem}
     NavyLink,
     Terminal,
     ScrollBk,
     apPort,
     apUART,
{$ENDIF}
     Phones,

     ASCIITab,
{$IFDEF Game}
     Tetris,
{$ENDIF}
     Drives,
     Archiver,
     Idlers,
     Arvid,
     FlPanelX,
     WinClp,


{$IFDEF PrintManager}
     PrintManager,
{$ENDIF}

     Setups,
     ExtraMemory,
     XDblWnd,
     DiskTool,
     CCalc,
     DnIni {-$VIV}
     ;

const
  TrashCan: PView = nil;
  RunMenu: Boolean = Off;
  IdleCounter: Word = 0;
  StartTicks: Word = 0;
  NullStr: byte=0;

var
  LoaderSeg: Word;
  CommandOfs: Word;
  RunFirst: Boolean;
  IgnoreOldFiles: Boolean;

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
        procedure ViewFile(Intern: Boolean; FileName: String); {DataCompBoy}
        procedure AddFormat;
        procedure EditFile(Intern: Boolean; FileName: String); {DataCompBoy}
        procedure OutOfMemory; virtual;
        procedure RetrieveDesktop(const FileName: String; LS: PStream; LoadColors: Boolean); {DataCompBoy}
        procedure SaveDesktop(const FileName: String); {DataCompBoy}
        procedure LoadDesktop(var S: TStream);
        procedure StoreDesktop(var S: TStream);
        procedure ChgColors;
        procedure EventError(var Event: TEvent); virtual;
        procedure RunFormat;
        procedure HandleCommand(var Event: TEvent);
     end;

procedure LoadHistories;
procedure SaveHistories;
procedure ClearHistories;
function CheckExit: Boolean;
function PresentFile(Name: String): PStream; {DataCompBoy}
procedure WriteConfig;
procedure ExecString(S: PString);
function SearchExt(FileRec: PFileRec; var HS: String): Boolean; {DataCompBoy}
function ExecExtFile(const ExtFName: string; UserParams: PUserParams; SIdx: TStrIdx): Boolean; {DataCompBoy}
procedure ExecFile(const FileName: string); {DataCompBoy}
function ParseAddress(const Address: String; var Zone, Net, Node, Point: Word): Boolean;
procedure ShowTimeInfo;
Procedure SetSysColors( var Pal);
procedure LoadPalFromFile(const FN: String); {DataCompBoy}

const
     cfgMouseData           =  3;
     cfgInterfaceData       =  4;
     cfgSaversData          =  5;
     cfgSystemColors        =  6;
     cfgPanelDefaults       =  7;
     cfgTetrisRec           =  9;
     cfgCustomMasks         = 10;
     cfgPrinterSetup        = 11;
     cfgColumnDefaults      = 12;
     cfgSavers              = 13;
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
     cfgFMSetup             = 27;
     cfgDriveInfoData       = 28;
     cfgMakeListFile        = 29;
     cfgEditorDefaults      = 30;
     cfgStartupData         = 31;
     cfgSystemData          = 32;
     cfgINIdata             = 33;
     cfgIgnoreOldFiles      = 34;
     cfgExtractOptions      = 35;
{New Record Versions}
     cfgNewSystemData       = 36;
     cfgNewSaversData       = 37;
     cfgColumnsDefaultsDisk = 38;
     cfgColumnsDefaultsFind = 39;
     cfgColumnsDefaultsTemp = 40;
     cfgColumnsDefaultsArch = 41;
{    cfgColumnsDefaultsArvd = 42; Do not use 42 - old version!!!}
     cfgNewFMSetup          = 43;
     cfgArcCustomMasks      = 44;
     cfgColumnsDefaultsArvd = 45;
     cfgShowScrollBar       = 47;
     cfgNewStartupData      = 48;

     dlAbout = #3'Dos Navigator Open Source'#13#13#3'Version %s, %s'#13#3+
     #0'http://dn_osp.web.com/'#0#13#13#3'Based on Dos Navigator'#13#3+
     'Copyright (C) 1991-99 RIT Research Labs'#13#13;


type
      PDataSaver = ^TDataSaver;
      TDataSaver = object(TView)
         constructor Init;
         constructor Load(var S: TStream);
         procedure Store(var S: TStream);
         destructor Done; virtual;
      end;

const
      RDataSaver: TStreamRec = (
       ObjType: otDataSaver;
       VmtLink: Ofs(TypeOf(TDataSaver)^);
       Load: @TDataSaver.Load;
       Store: @TDataSaver.Store);

       DataSaver: PDataSaver = nil;

implementation

uses
  HelpKernel,
  DNHelp,
  {$ifdef modem}
  modemio,
  {$endif}
  ColorSel,
  UFNMatch;

{$I RunCmd.INC}
{$I Version.INC}

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

constructor TDataSaver.Load;
  var D, L: Word;
begin
  if DataSaver <> nil then Dispose(DataSaver, Done);
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
      dskTempContents: if TempFiles = nil then
                         begin
                           TempFiles := PDirStorage(S.Get);
                           TempDirs := PStringCollection(S.Get);
                         end else S.Seek(S.GetPos + L);
       else S.Seek(S.GetPos + L);
    end;
  until D = 0;
end;

procedure TDataSaver.Store;

 var D: Word;
     I, SPos: LongInt;

  procedure StoreBlock(I: Word; var B; Sz: Word);
  begin
    S.Write(I, SizeOf(Integer));
    S.Write(Sz, SizeOf(Integer));
    S.Write(B, Sz);
  end;

  procedure MarkP(Blk: Integer);
  begin
    S.Write(Blk, SizeOf(Blk));
    S.Write(Blk, SizeOf(Blk));
    SPos := S.GetPos;
  end;

  procedure UnMark;
  begin
    I := S.GetPos - SPos;
    S.Seek(SPos-SizeOf(Word));
    S.Write(I, SizeOf(Word));
    S.Seek(S.GetSize);
  end;

begin
  inherited Store(S);
  HideCommandLine := (CommandLine <> nil) and not CommandLine^.GetState(sfVisible);
  StoreBlock(dskViewerFind, FViewer.SearchString, SizeOf(FViewer.SearchString));
  StoreBlock(dskEditorFind, MicroEd.SearchData, SizeOf(MicroEd.SearchData));
  StoreBlock(dskViewerBounds, LastViewerBounds, SizeOf(LastViewerBounds)+SizeOf(TPoint)*2);
  StoreBlock(dskHideCmdLine, HideCommandLine, SizeOf(HideCommandLine));
  if (TempFiles <> nil) and (TempFiles^.Files <> 0) then
    begin
      MarkP(dskTempContents);
      S.Put(TempFiles);
      S.Put(TempDirs);
      UnMark;
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
   if S^.Status <> stOK then begin FreeObject(S); S:=nil; end
  end
 else s:=nil;
 PresentFile := S;
end;
        {-DataCompBoy-}

function CheckExit: Boolean;
 var Event: TEvent;
 function FindQuit(P: PView): Boolean; far;
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
  ClearFiles('DN*.SWP');
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

const
  NumRElms = 136
  {$IFDEF MODEM}       +9 {$ENDIF}
  {$IFDEF SpreadSheet} +5 {$ENDIF}
  {$IFDEF Game}        +3 {$ENDIF}
  {$IFDEF PrintManager}+4 {$ENDIF}
  {$IFDEF CDPlayer}    +2 {$ENDIF}
  {$IFDEF DBView}      +4 {$ENDIF}
  ;
  RegArray : array[1..NumRElms] of Word = (

  Ofs(RScroller),
  Ofs(RMyScrollBar),
  Ofs(RListViewer),

  { Register Tree }
  Ofs(RTreeView),
  Ofs(RHTreeView),
  Ofs(RTreeWindow),
  Ofs(RTreePanel),
  Ofs(RTreeDialog),
  Ofs(RTreeInfoView),
  Ofs(RTreeReader),
  Ofs(RDirCollection),
  { Register DblWnd }
  Ofs(RDoubleWindow),
  { Register DiskInfo }
  Ofs(RDiskInfo),
  Ofs(RNetInfo),  {-$VIV}
  { Register FilesCol }
  Ofs(RFilesCollection),
  { Register FlPanel }
  Ofs(RFilePanel),
  Ofs(FlPanel.RInfoView),
  Ofs(RTopView),
  Ofs(RSeparator),
  Ofs(RSpecScroll),
  Ofs(RDriveLine),
  Ofs(RArvidDrive),
  Ofs(RDirStorage),
  { Register FViewer}
  Ofs(RFileViewer),
  Ofs(RViewInfo),
  Ofs(RFileWindow),
  Ofs(RViewScroll),
  Ofs(RHFileViewer),
  { Register Menus }
  Ofs(RMenuBar),
  Ofs(RMenuBox),
  Ofs(RStatusLine),
  Ofs(RMenuPopup),
  { Register Dialogs }
  Ofs(RDialog),
  Ofs(RInputLine),
  Ofs(RButton),
  Ofs(RCluster),
  Ofs(RRadioButtons),
  Ofs(RCheckBoxes),
  Ofs(RMultiCheckBoxes),
  Ofs(RListBox),
  Ofs(RStaticText),
  Ofs(RLabel),
  Ofs(RHistory),
  Ofs(RParamText),
  { Register ColorSel }

  Ofs(RColorSelector),
  Ofs(RMonoSelector),
  Ofs(RColorDisplay),
  Ofs(RColorGroupList),
  Ofs(RColorItemList),
  Ofs(RColorDialog),
  Ofs(R_BWSelector),

  { Register HelpFile }
  Ofs(RHelpTopic),
  Ofs(RHelpIndex),
  { Register Trash }
  Ofs(RTrashCan),
  Ofs(RKeyMacros),
  { Validate registration }
  Ofs(RRangeValidator),
  Ofs(RPhoneCollection),
  Ofs(RPhone),
  Ofs(RPhoneDir),
  Ofs(RDStringView),
{$IFDEF Modem}
  Ofs(RLinker),
  Ofs(RLinkDrive),
  Ofs(RDialBox),
  Ofs(RAutoDialer),
  Ofs(RScrollBackWindow),
  Ofs(RScrollBack),
{$ENDIF}
  { Register MicroEd }
  Ofs(RXFileEditor),
  Ofs(REditWindow),
  Ofs(RInfoLine),
  Ofs(RFileEditor),
  Ofs(RBookmarkLine), {-$VIV}
  Ofs(REditScrollBar),
  Ofs(REditFrame),
  Ofs(RLineCollection),

{$IFDEF SpreadSheet}
  Ofs(RCellCollection),
  Ofs(RCalcWindow),
  Ofs(RCalcView),
  Ofs(RCalcInfo),
  Ofs(Calc.RInfoView),
{$ENDIF}

{ Register Claculator }
  Ofs(RCalcLine),
  Ofs(RIndicator),

  { Register Terminal }
{$IFDEF Modem}
  Ofs(RTerminal),
  Ofs(RPortInfo),
  Ofs(RTerminalWindow),
{$ENDIF}
  { Register User Screen }
  Ofs(RUserView),
  Ofs(RUserWindow),
  { Register Game }
{$IFDEF Game}
  Ofs(RGameInfo),
  Ofs(RGameView),
  Ofs(RGameWindow),
{$ENDIF}

  Ofs(RDrive),
  Ofs(RFindDrive),
  Ofs(RArcDrive),
  Ofs(RFileInfo),
  Ofs(RTempDrive),

{$IFDEF PrintManager}
  { Register PrintManager}
  Ofs(RStringCol),
  Ofs(RPrintManager),
  Ofs(RPrintStatus),
  Ofs(RPMWindow),
{$ENDIF}

  { Register Setups }
  Ofs(RSysDialog),
  Ofs(RCurrDriveInfo),
  Ofs(RMouseDialog),
  Ofs(RMouseBar),
  Ofs(RSaversDialog),
  Ofs(RSaversListBox),
  Ofs(RUpperTable),

  Ofs(RUserSaver),

  { Register Archivers }
  Ofs(RARJArchiver),
  Ofs(RZIPArchiver),
  Ofs(RSQZArchiver),
  Ofs(RHAPArchiver),
  Ofs(RHAArchiver),
  Ofs(RRARArchiver),
  Ofs(RTARArchiver),
  Ofs(RHYPArchiver),
  Ofs(RHPKArchiver),
  Ofs(RZOOArchiver),
  Ofs(RARCArchiver),
  Ofs(RLHAArchiver),
  Ofs(RUC2Archiver),
  Ofs(RBSAArchiver),
  Ofs(RBS2Archiver),
  Ofs(RCHZArchiver),
  Ofs(RLIMArchiver),
  Ofs(RCABArchiver),
  Ofs(RZXZArchiver),
  Ofs(RAINArchiver),
  Ofs(RACEArchiver),
  Ofs(RUFAArchiver),
  Ofs(RQUARKArchiver),
  Ofs(RArchiveCollection),

  Ofs(RTable),
  Ofs(RReport),
  Ofs(RASCIIChart),

  {$IFDEF CDPlayer}
  Ofs( RCdPlayer ),
  Ofs( RCdCounter ),
  {$ENDIF}

  {$IFDEF DBView}
  Ofs(RTFieldListBox),
  Ofs(RDBWindow),
  Ofs(RDBViewer),
  Ofs(RDBIndicator),
  {$ENDIF}

  Ofs(RFilterValidator),
  Ofs(RRangeValidator),

  Ofs(REditHistoryCol),
  Ofs(RViewHistoryCol),

  Ofs(RDataSaver),
  Ofs(RStringList),

 {Objects}
  Ofs(RCollection),
  Ofs(RStringCollection),
  Ofs(RStrCollection),

 {Views}
  Ofs(RView),
  Ofs(RFrame),
  Ofs(RScrollBar),
  Ofs(RGroup),
  Ofs(RWindow),

 {App}
  Ofs(RBackground),
  Ofs(RDesktop),
  Ofs(RStringCache),

 {DnStdDlg}
  Ofs(RFileCollection),
  Ofs(RFileInputLine),
  Ofs(RFileList),
  Ofs(RFileInfoPane),
  Ofs(RFileDialog),
  Ofs(RSortedListBox),
  Ofs(RTextCollection)
 );

procedure RegisterAll;
var
  I: Integer;
begin
  for I := 1 to NumRElms do RegisterType(PStreamRec(Ptr(DSEG,RegArray[I]))^);
end;

var flj: boolean;
begin
  for C := 'A' to 'Z' do DrvTrees[C].C := nil;
  RegisterAll;
  LastViewerBounds.Assign(0,0,0,0);

  if RunFirst then ClearSwap;

  TApplication.Init;

  LoadHistories;

  GetExtent( R );
  Clock := New(PClockView , Init( R ));
  GetExtent(R);
  if ShowSeconds then R.A.X := R.B.X - 10 else R.A.X := R.B.X - 7;
  R.B.Y := R.A.Y + 1;
  Clock^.Locate( R );
  if InterfaceData.Options and ouiClock = 0 then Clock^.Hide;
  PClockView(Clock)^.UpDate;

  Desktop^.GetExtent(R);
  Dec(R.B.Y); Dec(R.B.X);
  R.A.Y := R.B.Y - 3;
  R.A.X := R.B.X - 5;
  TrashCan := New(PTrashCan, Init(R));
  Desktop^.Insert(TrashCan);
  if HideCommandLine then CommandLine^.Hide;

  If RunFirst then
   begin
    OldArchiveName:='';
    NewArchiveName:='';
   end;

  LoadStream := PresentFile(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
  if LoadStream = nil then LoadStream := PresentFile(SourceDir+'DN'+GetEnv('DNDSK')+'.DSK');
  if LoadStream <> nil then RetrieveDesktop('', LoadStream, True);
  InitDrivers;

  {-$VOL begin}
  if not RunFirst or CBAutoSave then begin
    ClipBoardStream := GetMeMemoStream;
    LoadStream := PresentFile(SourceDir+'ClipBoar'+'.DN');
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

  if (Lo(DosVersion)>=20) and (Hi(DosVersion)>=10) then
         Msg(dlFmtOs2Warning, nil , mfWarning or mfOkButton ) ;

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
         end ; { case }
     until false;
end;

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
      SaveStream: PStream; {-$VOL}
begin
 {-$VOL begin}{if CBAutoSave added by piwamoto}
 if CBAutoSave then begin
   SaveStream := New(PBufStream, Init(SourceDir+'ClipBoar'+'.DN', stCreate, 2048));
   if (SaveStream<>nil)and(SaveStream^.Status=stOk)and(ClipBoardStream<>nil) then begin
     ClipBoardStream^.Seek(0);
     SaveStream^.CopyFrom(ClipBoardStream^, ClipBoardStream^.GetSize);
   end;
   FreeObject(SaveStream);
 end;
 FreeObject(ClipBoardStream);
 {-$VOL end}

 if ConfigModified then WriteConfig;
 if StartupData.Unload and osuAutosave <> 0 then
  SaveRealDsk
 else
  PDNApplication(Application)^.SaveDesktop(SwpDir+'DN'+ItoS(DNNumber)+'.SWP');
 SaveHistories;
 HideCommandLine := (CommandLine <> nil) and not CommandLine^.GetState(sfVisible);
 B := $8000 or (Byte(HideCommandLine));
 asm
   mov ax, $9906
   mov dx, B
   push BP
   int  2Fh
   pop  BP
 end;
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
    Dispose(COMPort, Done);
    SetFIFOBuffering(B, Off, 0);
  end;
{$ENDIF}
end;
        {-DataCompBoy-}

procedure TDNApplication.EventError;
begin
 if (Event.What = evMouseMove) and (Event.Where.Y = 0) and (Event.Where.X = Size.X - 1)
    and (SSaver = nil) and SaversData.Mouse then InsertIdler;
end;

procedure TDNApplication.ChgColors;
begin
 ChangeColors;
 WriteConfig;
end;

        {-DataCompBoy-}
procedure WriteConfig;
var
  S: TBufStream;
  I: Integer;
  SPos: LongInt;

  procedure StoreBlock(I: Integer; var B; Sz: Integer);
  begin
    S.Write(I, SizeOf(Integer));
    S.Write(Sz, SizeOf(Integer));
    S.Write(B, Sz);
  end;

  procedure MarkP(Blk: Integer);
  begin
    S.Write(Blk, SizeOf(Blk));
    S.Write(Blk, SizeOf(Blk));
    SPos := S.GetPos;
  end;

  procedure UnMark;
  begin
    I := S.GetPos - SPos;
    S.Seek(SPos-SizeOf(Word));
    S.Write(I, SizeOf(Word));
    S.Seek(S.GetSize);
  end;

begin
 ConfigModified := Off;
 S.Init(SourceDir+'DN'+GetEnv('DNCFG')+'.CFG', stCreate, 16384);
 if S.Status <> stOK then begin Msg(erCantOpenConfig, nil, mfError+mfOKButton); S.Done; Exit; end;
 S.Write(ConfigSigns[NumSupportedConfigs].Sign[1], ConfigSigns[NumSupportedConfigs].SignLen);
 StoreBlock(cfgNewSystemData, SystemData, SizeOf(SystemData));
 StoreBlock(cfgNewStartupData, StartupData, SizeOf(StartupData));
 StoreBlock(cfgMouseData, MouseData, SizeOf(MouseData));
 StoreBlock(cfgInterfaceData, InterfaceData, SizeOf(InterfaceData));
 StoreBlock(cfgShowScrollBar, ShowScrollBar, SizeOf(ShowScrollBar));
 StoreBlock(cfgNewSaversData, SaversData.Time, SizeOf(SaversData)-SizeOf(SaversData.Selected)*2);
 StoreBlock(cfgSystemColors, SystemColors, SizeOf(SystemColors));
 StoreBlock(cfgPanelDefaults, PanelDefaults, SizeOf(PanelDefaults));
 StoreBlock(cfgEditorDefaults, EditorDefaults, SizeOf(EditorDefaults));
 StoreBlock(cfgTetrisRec, TetrisRec, SizeOf(TetrisRec));
 StoreBlock(cfgArcCustomMasks, Startup.Archives, SizeOf(Startup.Archives));
 MarkP(cfgCustomMasks);
 S.Write(CustomMask1, SizeOf(CustomMask1));
 S.Write(CustomMask2, SizeOf(CustomMask1));
 S.Write(CustomMask3, SizeOf(CustomMask1));
 S.Write(CustomMask4, SizeOf(CustomMask1));
 S.Write(CustomMask5, SizeOf(CustomMask1));
 UnMark;
 StoreBlock(cfgPrinterSetup, RPrinterSetup, SizeOf(RPrinterSetup));
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
 StoreBlock(cfgNewFMSetup, Startup.FMSetup, SizeOf(Startup.FMSetup));
{$IFDEF CDPlayer}
 StoreBlock(cfgCDParams, CDPlace, LongInt(@CD_Player)-LongInt(@CDPlace));
{$ENDIF}
 StoreBlock(cfgVGApalette, VGA_palette , SizeOf(VGA_palette)) ;
 StoreBlock(cfgBlink, CurrentBlink, SizeOf(CurrentBlink)) ;
 StoreBlock(cfgFFindOptions, FileFind.FindRec.Options, SizeOf(Word) * 2);
 StoreBlock(cfgDriveInfoData, DriveInfoData, SizeOf(DriveInfoData));
 StoreBlock(cfgExtractOptions, ExtractWithPathNames, sizeof(ExtractWithPathNames));

 MarkP(cfgINIdata);
 S.Write(INIstoredtime,sizeof(INIstoredtime));
 S.Write(INIstoredsize,sizeof(INIstoredsize));
 S.Write(iniparamblock_START,Ofs(iniparamblock_END)-Ofs(iniparamblock_START));
 UnMark;

 if IgnoreOldFiles then StoreBlock(cfgIgnoreOldFiles,IgnoreOldFiles,0);

 MarkP(cfgDirsToChange);
 for I := 0 to 8 do S.WriteStr(DirsToChange[I]);
 UnMark;
 MarkP(cfgSavers);
 S.Put(SaversData.Selected.List);
 UnMark;
 StoreBlock(0,I,0);
 S.Done;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TDNApplication.LoadDesktop(var S: TStream);
var
  P: PView;
  PP: PView;
  Pal: PString;
  SaveState: Word;
begin
  lGetDir(0, ActiveDir);
  if Desktop^.Valid(cmClose) then begin
    Desktop^.Clear;
    repeat
     P := PView(S.Get);
     if P <> nil then
      begin
       if DataSaver <> nil then begin Dispose(DataSaver, Done); Continue end;
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

procedure WriteView(P: PView); far;
type TLongInt = array[0..10] of longint;
     PLongInt = ^TLongInt;
var l:array[0..5] of longint;
begin
  if (P <> Desktop^.Last) and (P <> TrashCan) and
     (P <> PView(Desktop^.Background)) and (TypeOf(P^) <> TypeOf(TWriteWin))
   then S.Put(P);
end;

begin
  if DataSaver = nil then New(DataSaver, Init);
  if DataSaver <> nil then
   begin
    S.Put(DataSaver);
    Dispose(DataSaver, Done);
    DataSaver:=nil;
   end;
  Desktop^.ForEach(@WriteView);
  S.Put(nil);
end;

function CacheLngId: string;
begin
  CacheLngId := versionName + LngId + versionDate + '123';
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

procedure GetStringCache;
var
  Ss: string;
  P: PCollection;
begin
  Ss := CnvString(S^.ReadStr);
  if Ss <> #0#0#0 then
  begin
    P := PCollection(S^.Get);
    if StringCache = nil then
    begin
      StringCache := P;
      if StringCache <> nil then if Ss <> CacheLngId then StringCache^.FreeAll;
    end else FreeObject(P);
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
    S^.Read(NewArchiveName, 1); S^.Read(NewArchiveName[1],length(NewArchiveName));
    S^.Read(OldArchiveName, 1); S^.Read(OldArchiveName[1],length(OldArchiveName));
    S^.Read(SM, Sizeof(SM));
    S^.Read( OldAppSize, SizeOf( Size ));
    S^.Read( OldDskSize, SizeOf( Size ));
    BB := ( LongInt( OldAppSize ) <> LongInt( Size ));
    LoadIndexes(S^);
    GetExtent( SaveBounds );
    Desktop^.Clear;
    if BB and ((SM <> ScreenMode) and (StartupData.Load and osuRestoreScrMode <> 0))
      then begin
      SetScreenMode(SM);
      SetBlink(CurrentBlink);
    end;
    BB := ( LongInt( OldDskSize ) <> LongInt( Desktop^.Size ));
    If BB then begin
      Desktop^.Hide;
      R.A := Desktop^.Origin;
      R.B.X := R.A.X + OldDskSize.X;
      R.B.Y := R.A.Y + OldDskSize.Y;
      Desktop^.ChangeBounds(R);
    end;
    LoadDesktop(S^);
    If OldArchiveName<>NewArchiveName then
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
     end;
    if (PJ <> nil) and (PJ^ <> '') then
     if PosChar(PJ^[1], '+-=<>|[') = 0  then
      begin lAssignFile(F, PJ^); lSetFAttr(F, $20);  lEraseFile(F); end
      else
       begin
        TempFile := Copy(PJ^,2,255);
        If Not ExistFile(TempFile) then TempFile:=GetfURZ(TempFile);
        If Not ExistFile(TempFile) then TempFile:=GetfURZ2(TempFile);
        If Not ExistFile(TempFile) then TempFile:=GetGLUKName(Copy(PJ^,2,255));
        If Not ExistFile(TempFile) then TempFile:=Copy(PJ^,2,255);
        case PJ^[1] of
          '-': CC := cmIntFileView;
          '=': CC := cmDBFView;
          '>': CC := cmWKZView;
          '<': CC := cmTextView;
          '|': CC := cmHexView;
          '[': CC := cmReadArchive;
          else CC := cmFileView;
        end;
        if CC <> 0 then
                    begin
                      Event.What := evCommand;
                      Event.Command := CC;
                      Event.InfoPtr := @TempFile;
                      PutEvent(Event);
                     end;
        RunMenu := Off;
       end;
    DisposeStr(PJ);
    S^.Read(TempBounds, SizeOf(TempBounds));
    {S^.Read(ArcBounds, SizeOf(TempBounds));}
    S^.Read(B, 1);
    KeyMacroses := PCollection(S^.Get);
    S^.Read(R.A,2 * SizeOf(LongInt));
    if not ShowSeconds then if R.A.X>(ScreenWidth shr 1) then Inc(R.A.X,3) else Dec(R.B.X,3);
    Clock^.Locate(R);
    {S^.Read(ArcFlags, 4);}
    if B then begin TrashCan^.Show; {Lister^.Show;} end;
    If BB then
       begin
         R.Assign(0, Byte( InterfaceData.Options and ouiHideMenu = 0 ), Size.X, Size.Y
                  - Byte( InterfaceData.Options and ouiHideStatus = 0 )
                  - Byte( not HideCommandLine) );
         Desktop^.ChangeBounds(R);
         Desktop^.Show;
       end;
    {
    if S^.Status <> stOk then
    begin
      Msg(erDesktopError, nil, mfOkButton + mfError);
    end;
    }
    if B then begin S^.Read(R.A, 2 * SizeOf(LongInt)); TrashCan^.Locate(R) end;
  end;
  Desktop^.Redraw;
  GetBounds(R);
  if (Clock^.Size.X <= 0) or (Clock^.Size.Y <= 0) or not R.Contains(Clock^.Origin) then
    begin
      if ShowSeconds then R.Assign(Size.X-10,0,Size.X,1)
      else R.Assign(Size.X-7,0,Size.X,1);
      Clock^.Locate(R);
    end;
  Dispose(S, Done);
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
  if (UpStrg(Ss^) = '.DSK') or (StringCache = nil) or (StringCache^.Count = 0) then
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
    S^.WriteStr(@NewArchiveName);
    S^.WriteStr(@OldArchiveName);
    S^.Write(ScreenMode, sizeof(Word));
    S^.Write( Size, SizeOf( Size ));
    S^.Write( Desktop^.Size, SizeOf( Size ));
    StoreIndexes(S^);
    StoreDesktop(S^);
    S^.Write(TempBounds, SizeOf(TempBounds));
    B := TrashCan^.GetState(sfVisible);
    S^.Write(B, 1);
    S^.Put(KeyMacroses);
    Clock^.GetBounds(R);
    if R.A.X>(ScreenWidth shr 1) then R.A.X:=R.B.X-10 else R.B.X:=R.A.X+10;
    S^.Write(R.A, 2 * SizeOf(LongInt));
    TrashCan^.GetBounds(R);
    if B then S^.Write(R.A, 2 * SizeOf(LongInt));
    if S^.Status <> stOk then
    begin
      PP := @FileName;
      Msg(erCantCreateFile, @PP, mfOkButton + mfError);
      MessageBox('S^.Status = '+ItoS(S^.Status), nil, 0);
      Dispose(S, Done);
      lAssignFile(F, FileName);
      lEraseFile(F);
      Exit;
    end;
  end;
  Dispose(S, Done);
end;
        {-DataCompBoy-}

procedure TDNApplication.OutOfMemory;
begin
{$IFDEF DPMI}
  DoDump(1);
  MessageBox(GetString(erNotEnoughMemory), nil, mfError + mfOKButton);
{$ELSE}
  Msg(erNotEnoughMemory, nil, mfError + mfOKButton);
{$ENDIF}
end;

        {-DataCompBoy-}
procedure TDNApplication.ViewFile;
var
  DR: String;
  NM: String;
  XT: String;
  W: PWindow;
  R: TRect;
  fr: PFileRec;
  up: tUserParams;

  function View: Boolean;
  begin
    View := ExecExtFile('DN.VWR', @up, dlLoadingViewer);
  end;

  procedure InternalView;
   var S: String;
       I: Integer;
       P: PViewRecord;
       PS: PString;
       V: PFileViewer;
  begin
    W := New(PFileWindow,Init(FileName, EditorDefaults.ViOpt and 1 <> 0));
    if (W <> nil) and (W^.Valid(cmValid)) then
      begin
        V := PFileViewer(W^.Current); S := V^.FileName; System.Insert(' ', S, 1); UpStr(S);
        PS := @S;
        if (InterfaceData.Options and ouiTrackViewers <> 0) and (ViewHistory <> nil)
          then I := ViewHistory^.IndexOf(@PS) else I := -1;
        if I >= 0 then
           begin
             P := ViewHistory^.At(I);
             R.Assign(P^.fOrigin.X, P^.fOrigin.Y, P^.fOrigin.X + P^.fSize.X, P^.fOrigin.Y + P^.fSize.Y);
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
                ViewMode   := fViewMode;
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
                MarkPos   :=fMarks;
                Loaded    := true;
                MakeLines;
              end;
           end else StoreViewInfo(W);
      end;
  end;

label ex;
begin
   FileName:=lFExpand(FileName);
   lFSplit(FileName, DR, NM, XT);
   fr:=CreateFileRec(DR+NM+XT);
   fillchar(up, sizeof(up), 0);
   up.active:=fr;
   UpStr(XT);
   Desktop^.GetExtent(R);
   Abort := Off;
   if (Intern and (SystemData.Options and ossViewer = 0)) and
       View then goto ex;
   if Message(DeskTop, evBroadcast, cmFindView, @FileName) <> nil then Exit;
   if Intern then
    begin
     {$ifdef DBView}
     if (UpStrg(XT) = '.DBF') then W := New(PDBWindow,Init(FileName)) else
     {$ENDIF}
     {$IFDEF SpreadSheet}
      if (UpStrg(XT) = '.WKZ') then W := New(PCalcWindow,Init(R, FileName)) else
     {$ENDIF}
        if ArcViewer(FileName, FreeByte) or Abort then goto ex
         else if View then goto ex
          else InternalView;
    end else InternalView;
   if (W <> nil) and not W^.Valid(0) then begin Dispose(W, Done); InternalView end;
   InsertWindow(W);
ex: DelFileRec(fr);
end;
        {-DataCompBoy-}

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
  fr:=CreateFileRec(lFExpand(FileName));
  fillchar(up, sizeof(up), 0);
  up.active:=fr;
   if (Intern and (SystemData.Options and ossEditor <> 0)) or
      (not Intern and (SystemData.Options and ossEditor = 0)) or
     not Edit then
      begin
        if Message(DeskTop, evBroadcast, cmFindEdit, @FileName) <> nil then Exit;
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
                    MarkPos      := fMarks;
                    Mark.A       := fBlockStart;
                    Mark.B       := fBlockEnd;
                    ScrollTo(fDelta.X, fDelta.Y);
                    Pos          := fPos;
                    BlockVisible := fBlockVisible;
                    VertBlock    := fVerticalBlock;
                    Hilite       := fHighlight;
                    HiliteColumn := fHiliteColumn;
                    HiliteLine   := fHiliteLine;
                    AutoIndent   := fAutoIndent;
                    AutoJustify  := fAutoJustify;
                    AutoBrackets := fAutoBrackets;
                    LeftSide     := fLeftSide;
                    RightSide    := fRightSide;
                    InSide       := fInSide;
                    InsertMode   := fInsMode;
                    KeyMap       := fKeyMap; {-$VIV}
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
  asm
    mov  ax, 9906h
    xor  dx, dx
    xor  cx, cx
    push bp
    int  2Fh
    pop  bp
    mov  WS, cx
  end;
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
      While St[0] < Char(Length(CColor)) do St := St + #$3F;
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
     P: PView;
     ST: String;

     HFile: PHelpFile;
     HelpStrm: PDosStream;
     T : Tevent ;

 const
     HelpInUse: Boolean = False;

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
  if TrashCan^.GetState(sfVisible) then Desktop^.Delete(TrashCan);
  P := nil;
  P := New(PXDoubleWindow, Init(R, 0, Byte(S[1])));
  if Abort then Dispose(P, Done) else InsertWindow(PWindow(P));
  if TrashCan^.GetState(sfVisible) then Desktop^.Insert(TrashCan);
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
   if Abort then Dispose(P, Done) else InsertWindow(PWindow(P));
 end;

 procedure SetScrMode(Mode: Word);
   var R1, A: TRect;
 begin
  if (Mode = ScreenMode) and not
     ((ScreenMode and smFont8x8 = 0) and (Size.Y <> 25))
   then Exit;
  GetExtent(R1);
  Clock^.GetBounds(A);
  SetScreenMode(Mode);
  GetExtent(R);
  A.A.X := Round(A.A.X*R.B.X/R1.B.X);
  if A.B.Y = R1.B.Y then A.A.Y := R.B.Y-1
     else A.A.Y := Round(A.A.Y*R.B.Y/R1.B.Y);
  if ShowSeconds then A.B.X := A.A.X + 10 else A.B.X := A.A.X + 7;
  A.B.Y := A.A.Y + 1;
  Clock^.Locate(A);
  SetBlink(CurrentBlink); ReDraw;
 end;


 procedure MessageBoxAbout;
 var D: array [1..2] of PString ;
 begin
   D[1]:=NewStr(VersionName); D[2]:=NewStr(#13+#3+'Compiled '+VersionDate);
   MessageBox2( dlAbout , ^C'This product is a FREEWARE',@D,Nil, mfAbout + mfOKButton );
   DisposeStr(D[1]);          DisposeStr(D[2]);
 end;

 Procedure ChScreenMode;
  var P: PMenuBox;
      M: PMenu;
      R: TRect;
      W: Word;
      tmp:word;
 begin
  M := NewMenu(NewItem('~1~ 40 x 25 ','', kbNoKey, 1, hcNoContext,
               NewItem('~2~ 80 x 25 ','', kbNoKey, 3, hcNoContext,
               NewItem('~3~ 80 x 30 ','', kbNoKey, 256, hcNoContext,
               NewItem('~4~ 80 x 50','',  kbNoKey, 259, hcNoContext,
               NewItem('~5~ 80 x 60','',  kbNoKey, 264, hcNoContext,
               NewItem('~6~ 132 x 25','', kbNoKey, 265, hcNoContext,
               NewItem('~7~ 132 x 43','', kbNoKey, 266, hcNoContext,
               NewItem('~8~ 132 x 50','', kbNoKey, 267, hcNoContext,
               NewItem('~9~ 132 x 60','', kbNoKey, 268, hcNoContext,
               nil))))))))));
  R.Assign(1,1,20,4);
  P := New(PMenuBox, Init(R, M, nil));
  P^.Options := P^.Options or ofCentered;
  W := Application^.ExecView(P);
  Dispose(P);
  DisposeMenu(M);
  if w=0 then exit;
  tmp:=smSVGALo;
  smSVGALo:=w;
  SetScrMode(smSVGALo);
  smSVGALo:=tmp;
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
     WW: Word;
     PW: PWindow;
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
  S.Done;
  FN := GetPath(FN); RereadDirectory(FN);
 end;

 procedure LoadColors;
 begin
  FN := GetFileNameDialog(SourceDir+'COLORS\*.PAL', GetString(dlLoadColorPal), GetString(dlFileName),
                          fdOKButton + fdHelpButton, hsColors);
  if FN = '' then Exit;
  LoadPalFromFile(FN);
 end;


{$IFDEF Modem}
 procedure OpenTerminal;
 begin
  if Term <> nil then Term^.Select else InsertWindow(New(PTerminalWindow, Init));
 end;
{$ENDIF}

 function TryRunOS2(S: PString): Boolean;
  var B: Boolean;
      ST: SessionType;
 begin
   TryRunOS2 := On;
   DelLeft(S^); DelRight(S^);
   if S^ = '' then Exit;
   if S^ = '' then Exit;
   TryRunOS2 := Off;
   if not OS210 then Exit;
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
   RunOS2Command(Copy(S^,2,255), B, ST);
   CmdLine.StrModified := On;
   Message(CommandLine, evKeyDown, kbDown, nil);
   ClearEvent(Event);
 end;

 function ExecCommandLine: Boolean;
  var S: String;
      up: TUserParams;
 begin
  ExecCommandLine := Off;
  S := '';
  CommandLine^.GetData(S);
  if (DelSpaces(S) = '') then Exit;
  ExecCommandLine := On;
  if not CheckExit then begin ClearEvent(Event); Exit end;
  if HandleChDirCommand then
   begin
    DelLeft(S); DelRight(S);
    if (S[1] in ['c','C']) and (S[2] in ['d','D']) and (S[3]=' ')
    then
     begin
      DelFC(S); DelFC(S); DelLeft(S); S:=DelSquashes(S);
      Message(Application, evBroadcast, cmChangeDirectory, @S);
      S:='';CommandLine^.SetData(S);CommandLine^.DrawView;
      exit;
     end;
    if (S[0]=#2) and ValidDrive(UpCase(S[1])) and (S[2]=':') then
    begin
     if s[1]='*' then s:=cTEMP_;
     Message(Desktop, evBroadcast, cmChangeDrv, @S);
     S:='';CommandLine^.SetData(S);CommandLine^.DrawView;
     exit;
    end;
    CommandLine^.GetData(S);
   end;
  if TryRunOS2(@S) then Exit;
  Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
  SaveDsk;
  Application^.Done;
  DosWrite(#13 + ActiveDir+'>' + S +#13#10);
  ExecString(@S);
 end;

 procedure ExecDOSCmd;
   var Nm: String;
       Xt: String;
 begin
   ST := '';
   Message(Desktop, evBroadcast, cmGetCurrentPosFiles, nil);
   lFSplit(CnvString(CurFileActive), FreeStr, Nm, Xt);
   if Pos(#0+LowStrg(Copy(Xt, 2, 3))+#0, Executables) > 0 then ST := Nm+Xt+' ';
   if InputBox(GetString(dlExecDOScmdHdr), GetString(dlExecDOScmdLine), ST, 128, hsExecDOSCmd) <> cmOK then Exit;
    if not TryRunOS2(@ST) and CheckExit then
                      begin
                        SaveDsk; Done;
                        WriteLn(#13#10 + ActiveDir+'>' + ST +#13#10);
                        ExecString(@St);
                      end;
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


 {$IFDEF PrintManager}
 procedure PrintFile(const S: String);
  var W: PView;
 begin
   R.Assign(0,0,50,9);
   if Printer = nil then
    begin
     W := ValidView(New(PPMWindow, Init(R)));
     if W <> nil then
      begin
       W^.Hide;
       Desktop^.InsertView(W, DeskTop^.Last);
       W^.Show;
      end;
    end;
   if Printer <> nil then Printer^.PrintFile(S);
 end;
 {$ENDIF}

 procedure GetFromClip;
 begin
   if SystemData.Options and ossUseSysClip <> 0 then SyncClipOut(On);
   if (MicroEd.Clipboard = nil) or (MicroEd.Clipboard^.At(0) = nil) then
    PString(Event.InfoPtr)^ := '' else
     PString(Event.InfoPtr)^ := PString(MicroEd.Clipboard^.At(0))^;
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

 procedure PutInClip;
 begin
   if MicroEd.Clipboard <> nil then Dispose(MicroEd.Clipboard, Done);
   MicroEd.Clipboard := New(PLineCollection, Init(1,1));
   MicroEd.Clipboard^.Insert(NewStr(PString(Event.InfoPtr)^));
   if SystemData.Options and ossUseSysClip <> 0 then SyncClipIn;
   CopyLines2Stream(MicroEd.Clipboard, ClipBoardStream);
 end;


 procedure DoQuickChange;
  var R: TRect;
      P: PMenuBox;
      Menu: PMenu;
      Items: PMenuItem;
      C: Char;
      N: Integer;
 begin
  Items := nil;
  for N := 8 downto 0 do
    if DirsToChange[N] <> nil then begin
      Items := NewItem(
        '~'+Itos(N+1)+'~ '+ CutH(DirsToChange[N]^,40),
        'Alt-'+Itos(N+1), kbAlt1, cmQuickChange1 + N,
        hcNoContext, Items);
    end;
  If Items = NIL then begin
    Msg( erNoQuickDirs, NIL, mfWarning + mfCancelButton );
    Exit;
  end;
  Menu := NewMenu(Items);
  P := New(PMenuBox, Init(R, Menu, nil));
  P^.Options := P^.Options or ofCentered;
  P^.HelpCtx := hcQuickDirs;

  N := DeskTop^.ExecView(P);
  Dispose(P, Done);
  DisposeMenu(Menu);
  if N >= cmQuickChange1 then Message(Desktop, evCommand, N, nil);
 end;

  procedure ExecTree;
    var S: String;
  begin
    Desktop^.Current^.GetData(S);
    {S := FExpand(S);}
    S := ChangeDir(GetString(dlChooseDir), 0);
    if S <> '' then Desktop^.Current^.SetData(S);
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
      if not TryRunOS2(PString(Event.InfoPtr)) and CheckExit and
             not ExecCommandLine then ExecFile(PString(Event.InfoPtr)^);
      CE;
    end;

    procedure DoExecString;
    begin
      if not TryRunOS2(PString(Event.InfoPtr)) and CheckExit then
                      begin
                        SaveDsk; Done;
                        ExecString(PString(Event.InfoPtr));
                      end;
      CE;
    end;

    procedure ChLngId;
    var S:string; SS:string[9]; L:TStringCollection; SR:lSearchRec;
        Current,Default:PMenuItem; Menu:PMenu; CurIdx,i:integer;
        HMB:PHugeMenuBox; R:TRect; V:PView;
        function LngMixCase(P:string):string;
        var i:integer;
        begin
            for i:=1 to Length(P) do if (i=1) or not (P[i-1] in ['a'..'z',
            'A'..'Z']) then P[i]:=UpCase(P[i]) else if P[i] in ['A'..'Z']
            then P[i]:=Chr(Ord(P[i])+Ord('a')-Ord('A'));
            LngMixCase:=P
        end;
    begin
        L.Init(5,5);
        S:=SourceDir; if S[Byte(S[0])]<>'\' then S:=S+'\';
        lFindFirst(S+'*.lng',AnyFile,SR);
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
        S:=StartupDir; if S[Byte(S[0])]<>'\' then S:=S+'\';
        lFindFirst(S+'*.lng',AnyFile,SR);
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
            S:='English';
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
            LngMixCase(ActiveLanguage)) and CheckExit then begin
                ActiveLanguage:=PString(L.At(CurIdx))^;
                if ActiveLanguage=LngMixCase(GetEnv('DNLNG'))
                then ActiveLanguage:='';
                InitIniEngine;
                SaveDnIniSettings;
                DoneIniEngine;
                ProbeINI(INIstoredtime,INIstoredsize);
                ConfigModified:=True;
                StringCache^.FreeAll;
                SaveDsk;
                Done;
                ExecString(@NullStr);
            end;
        end;
        L.FreeAll;
        L.Done;
    end;
begin
   If Event.InfoPtr<>nil then ST := PString(Event.InfoPtr)^
                         else ST := '';
   case Event.Command of
     cmFirstTimePanel: OpenWindow( False );
     cmPutInClipboard: PutInClip;
     cmGetFromClipboard: GetFromClip;
     cmSearchAdvance: SearchAdvance;
     cmListOfDirs: DoQuickChange;
     cmFormatDisk: RunFormat;
     {$IFDEF PrintManager}
       cmFilePrint: PrintFile(PString(Event.InfoPtr)^);
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
     cmColumnDefaultsDisk: if SetupColumnDefaultsDisk then WriteConfig;
     cmColumnDefaultsTemp: if SetupColumnDefaultsTemp then WriteConfig;
     cmColumnDefaultsFind: if SetupColumnDefaultsFind then WriteConfig;
     cmColumnDefaultsArch: if SetupColumnDefaultsArch then WriteConfig;
     cmColumnDefaultsArvd: if SetupColumnDefaultsArvd then WriteConfig;
     cmHighlightGroups: SetHighlightGroups;
     {$IFDEF DBView}
        cmDBFView: InsertWindow(New(PDBWindow,Init(ST)));
     {$ENDIF}
     {$IFDEF SpreadSheet}
        cmNewTable: LoadSheet('');
        cmSheetLoad: OpenSheet;
        cmWKZView: begin
                     DeskTop^.GetExtent(R);
                     InsertWindow(New(PCalcWindow,Init(R, ST)));
                   end;
     {$ENDIF}
        cmPhoneBook: PhoneBook(Off);
     {$IFDEF Modem}
        cmNavyLink: StartLink;
        cmUndial: PhoneBook(On);
        cmSetupModem: SetupModem;
        cmTerminal: OpenTerminal;
        cmHangUp: if COMPort <> nil then
                   begin
                    P := WriteMsg(GetString(dlDisconnect));
                    HangUp;
                    if Dialer <> nil then Dialer^.Free;
                    if Term <> nil then Term^.Owner^.Redraw;
                    if P <> nil then P^.Free;
                   end;
     {$ENDIF}
     {$IFDEF DiskEdit}
        cmDiskEdit: if (SystemData.Drives[ GetCurDrive ] and ossDirectAccess <> 0) and CheckDisk(GetCurDrive)

                      then InsertWindow(New(PFixerWindow, Init))
                      else Denie;
     {$ENDIF}
     cmTextView, cmHexView:
                   begin
                      InsertWindow(New(PFileWindow,Init(ST, Event.Command = cmHexView)));
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
     cmSaversSetup: SaversSetup;
     cmHideShowTools: if TrashCan^.GetState(sfVisible)
                        then TrashCan^.Hide
                        else TrashCan^.Show;
     cmReanimator: if (SystemData.Drives[ GetCurDrive ] and ossDirectAccess <> 0)  and CheckDisk(GetCurDrive)
                     then OpenReanimator
                     else Denie;

     cmEditHistory: EditHistoryMenu;
     cmViewHistory: ViewHistoryMenu;

     cmSetVolumeLabel: if (SystemData.Drives[ GetCurDrive ] and ossDirectAccess <> 0) and CheckDisk(GetCurDrive)
                         then SetVLabel
                         else Denie;
     cmXEditFile: OpenEditor;
     cmWindowManager: WindowManager;
     cmUserMenu, cmGlobalUserMenu: if (ExecUserMenu(Event.Command=cmGlobalUserMenu)) then
                begin
                  ST := FreeStr;
                  if CheckExit then
                  begin
                   SaveDsk; Application^.Done;
                   ST:=SwpDir+'$DN'+ItoS(DNNumber)+'$.BAT ' + ST;
                   ExecString(@ST);
                  end;
                end;
     cmReadArchive: begin ReadArcList; end;
     cmScreenRest: InsertIdler;
     cmChScreenMode:ChScreenMode;
     cmASCIITable: ASCIITable;
     cmAbout: begin
                ClearEvent(Event);
                MessageBoxAbout;
                Exit;
              end;
     cmCalculator: InsertCalc;
     cmRefresh: Redraw;
     cmClearDesktop: GlobalMessage(evCommand, cmClose, nil);
     cmFileView: ViewFile(On, PString(Event.InfoPtr)^);
     cmFileEdit: EditFile(On, PString(Event.InfoPtr)^);
     cmIntFileView: ViewFile(Off, PString(Event.InfoPtr)^);
     cmIntFileEdit: EditFile(Off, PString(Event.InfoPtr)^);
     cmEditQuickRun: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*255)+'dn.xrn');
     cmExtFileEdit: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*255)+'dn.ext');
     cmMenuFileEdit: EditFile(True, SourceDir+'dn.mnu');
     cmLocalMenuFileEdit: EditFile(True, 'dn.mnu');
     cmEditHGL: EditFile(True, SourceDir+'dn.hgl');
     cmEditSPF: EditFile(True, SourceDir+'dn.spf');
     cmEditINI: EditFile(True, SourceDir+'dn.ini');
     cmExternalViewers: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*255)+'dn.vwr');
     cmExternalEditors: EditFile(True, Copy(SourceDir,1,Byte(ShiftState and 3 = 0)*255)+'dn.edt');
     cmShowUserScreen: ShowUserScreen;
      {-DataCompBoy-}
     cmRestart: Begin
                 StringCache^.FreeAll;
                 SaveDsk;
                 Done;
                 ExecString(@NullStr);
                end;
      {-DataCompBoy-}
     cmCreatePanel: OpenWindow(True);
     cmCreateTree: OpenTreeWindow;
     cmFindFile: FindFile;
     cmFormatDisk: AddFormat;
     cmSystemInfo: SystemInfo;
     cmMemoryInfo: MemoryInfo;
     cmShowOutput: GetUserScreen;
     cmHistoryList: CmdHistory;
     cmLoadDesk: RetrieveDesktop(SourceDir+'DN'+GetEnv('DNDSK')+'.DSK', nil, True);
     cmSaveDesk: SaveRealDsk;
     cmExecFile: DoExecFile;
     cmExecString: DoExecString;
     cmExecCommandLine: if ExecCommandLine then CE;
     cmShowTimeInfo: ShowTimeInfo; {DataCompBoy}
     cmChangeColors: ChgColors;
     cmChangeUserMode1: if ScreenMode <> smSVGALo then SetScrMode(smSVGALo) else SetScrMode(StartupMode);
     cmChangeUserMode2: if ScreenMode <> smSVGAHi then SetScrMode(smSVGAHi) else SetScrMode(StartupMode);
     cmChangeMode: begin
                    if (ScreenMode <> smSVGALo) and
                       (ScreenMode <> smSVGAHi) then
                        if Size.Y = 25 then SetScrMode(ScreenMode or smFont8x8)
                                       else SetScrMode(ScreenMode and 255)
                      else SetScrMode(3);
                   end;

     cmLoadColors: LoadColors;
     cmSetupArchive:
     begin
       LoadArchiveSet;
       SetupArchive(DefaultArchiver);
     end;
     cmLoConfigArchiver..cmHiConfigArchiver: SetupArchive(Event.Command);
     cmFMDefaults: FMDefaults;
     cmFMSetup: FMSetup;
     cmDriveInfoSetup: DriveInfoSetup;
     {cmRunAmin : RunMusic ;}
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
                  if (W <> cmYes) or not CheckExit then ClearEvent(Event);
                end;
        cmGetCmpNfo:
                begin
                  ClearEvent(Event);
                  Event.InfoPtr := WriteMsg( GetString( dlComparing ));
                end;
        cmNewStrColl:
                   begin
                      ClearEvent(Event);
                      Event.InfoPtr := New(PStringCollection, Init($80,$40));
                   end;
        cmRFormat: begin
                      ClearEvent(Event);
                      Event.InfoPtr := BuildAdvFormatDialog;
                   end;
 cmHelp,cmHelp2: if not HelpInUse then
                 begin
                   HelpInUse := True;
                   HelpStrm := New(PDosStream, Init(SourceDir+HelpLngId + '.hlp', stOpenRead));
                   if HelpStrm^.Status <> stOk then
                     begin
                       Dispose(HelpStrm, Done);
                       HelpStrm := New(PDosStream, Init(StartupDir+HelpLngId + '.hlp', stOpenRead));
                     end;
                   HFile := New(PHelpFile, Init(HelpStrm));
                   if HelpStrm^.Status <> stOk then
                   begin
                     Msg(erCantOpenHelp, nil, mfError + mfOkButton);
                     Dispose(HFile, Done);
                   end
                   else
                   begin
                     if Event.Command=cmHelp
                      then PW := New(PHelpWindow,Init(HFile, GetHelpCtx))
                      else PW := New(PHelpWindow,Init(HFile, Word(Event.InfoWord)));
                     if ValidView(PW) <> nil then begin
                       ExecView(PW);
                       Dispose(PW, Done);
                     end;
                     ClearEvent(Event);
                   end;
                   HelpInUse := False;
                 end;
        cmTree: ExecTree;
        {$ifdef demo}
        cmRunDemo: RunDemo;
        {$endif}
        cmClearData: begin
                       FillChar(FreeStr, SizeOf(FreeStr), 0);
                       PView(Event.InfoPtr)^.Owner^.SetData(FreeStr);
                       ClearEvent(Event);
                     end;
         cmChLngId: ChLngId;
   end;
end;
        {-DataCompBoy-}

function ParseAddress(const Address: String; var Zone, Net, Node, Point: Word): Boolean;
  const Signs = ':/.';
  var S1, S2: String[6];
      I,J,N,K: Integer;
      KK: Integer;
begin
  ParseAddress := Off; Point := 0;
  I := 1; N := 0;
  KK := PosChar('@', Address);
  if KK = 0 then KK := Length(Address)+1;
  while I < KK do
    begin
      S1 := '';
      while (I < KK) and (Length(S1) < 5) and
            (Address[I] >= '0') and (Address[I] <= '9') do
             begin AddStr(S1, Address[I]); Inc(I); end;
      {if I <= Length(Address) then}
        begin
          J := PosChar(Address[I], Signs); K := StoI(S1);
          if (J <> 0) and (N <> 0) and (J-1 <> N) then Exit;
          case J of
            0: if N = 3 then Point := K
                 else if (N = 0) or (N = 2) then
                  begin if S1 <> '' then Node := K end
                   else Exit;
            1: begin if S1 <> '' then Zone := K; Net := 0; Node := 0; end;
            2: begin if S1 <> '' then Net := K; Node := 0; end;
            3: if S1 <> '' then Node := K;
          end;
          N := J;
          Inc(I);
        end;
    end;
  ParseAddress := On;
end;

procedure ShowTimeInfo;
begin
   FreeStr := SStr(DDTimer div 360000, 2, '0');
   AddStr(FreeStr, ':');
   DDTimer := DDTimer mod 3600000;
   Insert(SStr(DDTimer div 6000, 2, '0'), FreeStr, Length(FreeStr)+1);
   AddStr(FreeStr, ':');
   DDTimer := DDTimer mod 6000;
   Insert(SStr(DDTimer div 100, 2, '0'), FreeStr, Length(FreeStr)+1);
   AddStr(FreeStr, '.');
   Insert(SStr(DDTimer mod 100, 2, '0'), FreeStr, Length(FreeStr)+1);
   MessageBox(GetString(dlElapsedTime)+FreeStr, nil, mfOKButton+mfInformation);
end;

const
      HistoryFileSign = 'DN OSP History file'#13#10#26#1#51#05;

        {-DataCompBoy-}
procedure LoadHistories;
  var S: TBufStream;
      W: Word;
begin
    S.Init(SourceDir+'DN'+GetEnv('DNDSK')+'.HIS', stOpenRead, 2048);
    if S.Status = stOK then
      begin
        S.Read(FreeStr[1], Length(HistoryFileSign));
        FreeStr[0] := Char(Length(HistoryFileSign));
        if FreeStr = HistoryFileSign then
          begin
             S.Read(HistoryUsed, sizeof(Word));
             if HistorySize < HistoryUsed + 256 then
               begin
                  DoneHistory; W := HistoryUsed;
                  HistorySize := HistoryUsed + 256;
                  InitHistory;
                  HistoryUsed := W;
               end;
             S.Read(HistoryBlock^, HistoryUsed);
             LoadCommands(S);
          end
        else MessageBox('Can''t load histories!', nil, mfOkButton);
      end;
    S.Done;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure SaveHistories;
  var S: TBufStream;
begin
  S.Init(SourceDir+'DN'+GetEnv('DNDSK')+'.HIS', stCreate, 2048);
  if S.Status = stOK then
    begin
      FreeStr := HistoryFileSign;
      S.Write(FreeStr[1], Length(HistoryFileSign));
      S.Write(HistoryUsed, sizeof(Word));
      S.Write(HistoryBlock^, HistoryUsed);
      SaveCommands(S);
    end;
  S.Done;
end;
        {-DataCompBoy-}

procedure ClearHistories;
  var I, J, K: Integer;
      B: PChar;

  procedure ClearStrCollection(C: PCollection);
  begin
    if C = nil then Exit;
    C^.Pack;
    I := 0;
    while I < C^.Count do
      begin
        FreeStr := CnvString(C^.At(I));
        if FreeStr[1] = ' ' then C^.AtFree(I)
                            else Inc(I);
      end;
  end;

  procedure ClearCollection(C: PCollection);
  begin
    if C = nil then Exit;
    C^.Pack;
    I := 0;
    while I < C^.Count do
      begin
        FreeStr := CnvString(PEditRecord(C^.At(I))^.fName);
        if FreeStr[1] = ' ' then C^.AtFree(I)
                            else Inc(I);
      end;
  end;

begin
  B := HistoryBlock;
  I := 1;
  while I < HistoryUsed do
    begin
      if B[I+1] = #0 then Break;
      Move(B[I+2], FreeStr, Byte(B[I+2])+1);
      J := I + Byte(B[I+2])+3;
      if FreeStr[Length(FreeStr)] <> ' ' then I := J
       else
         begin
           Move(B[J], B[I], HistoryUsed - J + 1);
           Dec(HistoryUsed, J - I);
         end;
    end;
  if HistoryUsed < 4 then HistoryUsed := 0;
  I := 0;
  ClearStrCollection(CmdStrings);
  ClearStrCollection(DirHistory);
  ClearCollection(EditHistory);
  ClearCollection(ViewHistory);
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

end.
