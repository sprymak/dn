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
//      LFN¨§ æ¨ï ¢ë¯®«­¥­  €­â®­®¬ ”¥¤®p®¢ë¬ aka DataCompBoy'¥¬.
//                  ‚ ç¥áâì Œ â pëª¨­®© “«ìï­ë...
//////////////////////////////////////////////////////////////////////////}

{$DEFINE AsUnit}

{$IFDEF AsUnit} UNIT Setups; INTERFACE {$ENDIF}

USES
  ObjType,
  Dos,
  LFN, {DataCompBoy}
  Objects,
  Drivers,
  Views,
  Dialogs,
{$IFNDEF AsUnit}
  Menus,
  DNApp,
{$ELSE}
  DNApp,
{$ENDIF}
  Commands,
  RStrings,
  Startup,
  Validate;

{$IFNDEF AsUnit}
TYPE
  TMyApp = Object( TApplication )
    procedure InitMenuBar; virtual;
    procedure HandleEvent( var Event: TEvent ); virtual;
  end;

CONST
  cmInterface = 300;
  cmMouse = 301;
  cmSavers = 302;
  cmStartUp = 303;
  cmSystem = 304;
  cmCountry = 305;
{$ENDIF}

{                                System Setup
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
TYPE
  TSysData = record
    Options: Word;
    Mode1: String[5];
    Mode2: String[5];
    Drives: TTextListboxRec;
    Current: Word;
    Temp: String; {DataCompBoy}
  end;

  PSysDialog =^TSysDialog;
  TSysDialog = Object( TDialog )
    LocalData: TSystemData;
    SysData: TSysData;
    {constructor Init;}
    procedure Awaken; virtual;
    destructor Done; virtual;
    procedure GetData( var Rec ); virtual;
  end;

  PCurrDriveInfo =^TCurrDriveInfo;
  TCurrDriveInfo = Object( TCheckBoxes )
    procedure HandleEvent( var Event: TEvent ); virtual;
    procedure Press( Item: Integer ); virtual;
  end;

  PMouseDialog =^TMouseDialog;
  TMouseDialog = Object( TDialog )
    constructor Init;
  end;

  PMouseBar =^TMouseBar;
  TMouseBar = Object( TScrollBar )
    constructor Init( var Bounds: TRect );
    procedure SetData( var Rec ); virtual;
    procedure GetData( var Rec ); virtual;
    function DataSize: Word; virtual;
    procedure HandleEvent( var Event: TEvent ); virtual;
  end;

  PSaversDialog =^TSaversDialog;
  TSaversDialog = Object( TDialog )
    constructor Init;
    procedure HandleEvent( var Event: TEvent ); virtual;
    destructor Done; virtual;
    procedure Awaken; virtual;
  end;

  PSaversListBox =^TSaversListBox;
  TSaversListBox = Object( TListBox )
    procedure HandleEvent( var Event: TEvent ); virtual;
  end;

  PUpperTable = ^TUpperTable;
  TUpperTable = object(TView)
    Table: Array[0..127] of Char;
    CurrentChar: Integer;
    Delta: Integer;
    constructor Init(var R: TRect);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function DataSize: Word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    function GetPalette: PPalette; virtual;
  end;


CONST
  RSysDialog: TStreamRec = (
     ObjType: otSysDialog;
     VmtLink: Ofs(TypeOf(TSysDialog)^);
     Load:    @TSysDialog.Load;
     Store:   @TSysDialog.Store
  );

  RCurrDriveInfo: TStreamRec = (
     ObjType: otCurrDriveInfo;
     VmtLink: Ofs(TypeOf(TCurrDriveInfo)^);
     Load:    @TCurrDriveInfo.Load;
     Store:   @TCurrDriveInfo.Store
  );

  RMouseDialog: TStreamRec = (
     ObjType: otMouseDialog;
     VmtLink: Ofs(TypeOf(TMouseDialog)^);
     Load:    @TMouseDialog.Load;
     Store:   @TMouseDialog.Store
  );

  RMouseBar: TStreamRec = (
     ObjType: otMouseBar;
     VmtLink: Ofs(TypeOf(TMouseBar)^);
     Load:    @TMouseBar.Load;
     Store:   @TMouseBar.Store
  );

  RSaversDialog: TStreamRec = (
     ObjType: otSaversDialog;
     VmtLink: Ofs(TypeOf(TSaversDialog)^);
     Load:    @TSaversDialog.Load;
     Store:   @TSaversDialog.Store
  );

  RSaversListBox: TStreamRec = (
     ObjType: otSaversListBox;
     VmtLink: Ofs(TypeOf(TSaversListBox)^);
     Load:    @TSaversListBox.Load;
     Store:   @TSaversListBox.Store
  );

  RUpperTable: TStreamRec = (
     ObjType: otUpperTable;
     VmtLink: Ofs(TypeOf(TUpperTable)^);
     Load:    @TUpperTable.Load;
     Store:   @TUpperTable.Store
  );

procedure SetupCountryInfo;
procedure FMDefaults;
procedure FMSetup;
procedure DriveInfoSetup;
procedure SetupEditorDefaults;
procedure SystemSetup;
procedure InterfaceSetup;
procedure StartupSetup;
procedure MouseSetup;
procedure SaversSetup;
procedure ConfirmSetup;
function TerminalSetup: boolean;
function SetupColumnDefaultsDisk: boolean;
function SetupColumnDefaultsFind: boolean;
function SetupColumnDefaultsTemp: boolean;
function SetupColumnDefaultsArch: boolean;
function SetupColumnDefaultsArvd: boolean;

{$IFDEF AsUnit}
FUNCTION MakeSaversDialog: PDialog;
{
FUNCTION MakeStartupDialog: PDialog;
FUNCTION MakeInterfaceDialog: PDialog;
FUNCTION MakeMouseDialog: PDialog;
}
IMPLEMENTATION
uses Tree, Drives, advance, advance1, advance2, advance3, Messages, DNHelp;
{$ENDIF}

procedure ConfirmSetup;
begin
  If ExecResource( dlgConfirmations, Confirms ) <> cmOK then Exit;
  ConfigModified := On;
end;

function TerminalSetup;
begin
  TerminalSetup := Off;
  If ExecResource( dlgSetupTerminal, TerminalDefaults ) <> cmOK then Exit;
  TerminalSetup := On;
  Message(Application, evCommand, cmUpdateConfig, nil);
end;

procedure SystemSetup;
 var W: Word;
     D: PDialog;
     B: Boolean;
     Data: TSysData;
     i: char;
begin
 OpenResource; if Resource = nil then Exit;
 D := PDialog(Application^.ValidView(PDialog(Resource^.Get( dlgSystemSetup ))));
 If D = nil then Exit;
 W := Desktop^.ExecView(D);
 If W <> cmCancel then begin
   D^.GetData(Data);
   SystemData := PSysDialog( D )^.LocalData;
   Message(Application, evCommand, cmUpdateConfig, nil);
 end;
 Dispose(D, Done);
end;

procedure InterfaceSetup;
  var AltTab: Boolean;
      R: TRect;
begin
  if ShowScrollBar then InterfaceData.Options:=InterfaceData.Options or ouiShowScrollBar
                   else InterfaceData.Options:=InterfaceData.Options and not ouiShowScrollBar;
  with PApplication(Application)^ do
  If ExecResource( dlgInterfaceSetup, InterfaceData ) <> cmCancel then begin
    GetExtent( R );
    if InterfaceData.Options and ouiHideMenu = 0 then Inc(R.A.Y);
    if InterfaceData.Options and ouiHideStatus = 0 then Dec(R.B.Y);
    if InterfaceData.Options and ouiHideCmdline = 0 then Dec( R.B.Y );
    if (InterfaceData.Options and ouiShowScrollBar<>0)<>ShowScrollBar then
    begin
     Desktop^.Lock;
     ShowScrollBar:=InterfaceData.Options and ouiShowScrollBar<>0;
     if ShowScrollBar then begin
                            Desktop^.Hide;
                            Desktop^.Show;
                           end else begin
      Message(Application, evKeyDown, kbTab, nil);
      Message(Application, evKeyDown, kbTab, nil);
     end;
     Desktop^.UnLock;
     Desktop^.Redraw;
    end;
    Desktop^.Locate( R );
    R.A.Y := R.B.Y;
    R.B.Y := R.A.Y + Byte(InterfaceData.Options and ouiHideCmdline = 0);
    CommandLine^.Locate( R );
    CommandLine^.SetState( sfVisible, InterfaceData.Options and ouiHideCmdline = 0 );
    Message(Application, evCommand, cmUpdateConfig, nil);
    if InterfaceData.Options and ouiClock <> 0 then
      if not Clock^.GetState(sfVisible) then Clock^.Show;
    if InterfaceData.Options and ouiClock = 0 then
      if Clock^.GetState(sfVisible) then Clock^.Hide;
  end;
end;

procedure StartupSetup;
var
  Data: Record
    Load, Unload,
    Slice,Slice2: Word;
    OvrBuf: String[ 3 ];
  end;
  W: Word;
  R: Integer;
label
  Retry;
begin
  Data.Load := StartupData.Load;
  Data.Unload := StartupData.Unload;
  Data.Slice := StartupData.Slice;
  Data.Slice2:= StartupData.Slice2;
Retry:
  Data.OvrBuf := ItoS( StartupData.OvrSize );
  If ExecResource( dlgStartupSetup, Data ) <> cmCancel then begin
    Val( Data.OvrBuf, W, R );
    If ( W < 64 ) or ( W > 200 ) or ( R > 0 ) then begin
      Msg( erInvalidOvrSize, NIL, mfError + mfOKButton );
      goto Retry;
    end;
    StartupData.Load := Data.Load;
    StartupData.Unload := Data.Unload;
    StartupData.Slice := Data.Slice;
    StartupData.Slice2:= Data.Slice2;
    StartupData.OvrSize := W;
    LSliceCnt := -3;
    Message(Application, evCommand, cmUpdateConfig, nil);
  end;
end;

procedure MouseSetup;
begin
  if ExecResource( dlgMouseSetup, MouseData ) <> cmOK then Exit;
  if StdMouse xor (MouseData.Options and omsCursor <> 0) then begin
    DoneEvents;
    InitEvents;
  end;
  MouseReverse := MouseData.Options and omsReverse <> 0;
  SetMouseSpeed(MouseData.HSense, MouseData.VSense);
  Message(Application, evCommand, cmUpdateConfig, nil);
end;

procedure SaversSetup;
 var W: Word;
     D: PDialog;
     B: Boolean;
begin
  OpenResource; if Resource = nil then Exit;
  D := PDialog(Application^.ValidView(PDialog(Resource^.Get( dlgSaversSetup ))));
  If D = nil then Exit;
  W := Desktop^.ExecView( D );
  If W <> cmCancel then begin
    D^.GetData( SaversData );
    Message(Application, evCommand, cmUpdateConfig, nil);
  end;
  Dispose( D, Done );
end;

procedure SetupCountryInfo;
begin
  if ExecResource(dlgCountrySetup, CountryInfo) <> cmOK then Exit;
  GlobalMessage(evCommand, cmReboundPanel, nil);
  ConfigModified := On;
end;

procedure FMDefaults;
begin
  if ExecResource(dlgFMDefaults, Startup.PanelDefaults ) <> cmOK then Exit;
  Message(Application, evCommand, cmUpdateConfig, nil);
  GlobalMessage(evCommand, cmReboundPanel, nil);
end;

procedure FMSetup;
begin
  if ExecResource(dlgFMSetup, Startup.FMSetup) <> cmOK then Exit;
  Message(Application, evCommand, cmUpdateConfig, nil);
  GlobalMessage(evCommand, cmReboundPanel, nil);
end;

procedure DriveInfoSetup;
begin
  if ExecResource(dlgDriveInfoSetup, Startup.DriveInfoData) <> cmOK then Exit;
  Message(Application, evCommand, cmUpdateConfig, nil);
  GlobalMessage(evCommand, cmReboundPanel, nil);
end;

procedure SetupEditorDefaults;
begin
  if ExecResource(dlgEditorDefaults, EditorDefaults) = cmOK then begin
    if StoI(EditorDefaults.TabSize)<2 then EditorDefaults.TabSize:='2'; {-$VOL}
    Message(Application, evCommand, cmUpdateConfig, nil);
  end;
end;

 {-DataCompBoy-}
function SetupColumnDefaultsDisk: Boolean;
var
  Dlg : PDialog;
  R : TRect;
  Control, Labl, Histry : PView;
Begin
R.Assign(5,2,75,20);
New(Dlg, Init(R, GetString(dlSetupColumnsDisk)));

R.Assign(18,2,23,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(18,8,23,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(18,9,23,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(23,2,28,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(23,8,28,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(23,9,28,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(28,2,33,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(28,8,33,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(28,9,33,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(33,2,38,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(33,8,38,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(33,9,38,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(38,2,43,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(38,8,43,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(38,9,43,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(43,2,48,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(43,8,48,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(43,9,48,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(48,2,53,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(48,8,53,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(48,9,53,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(53,2,58,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(53,8,58,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(53,9,58,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(58,2,63,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(58,8,63,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(58,9,63,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(63,2,68,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(63,8,68,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(63,9,68,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(2,12,68,13);
Control := New(PRadioButtons, Init(R,
  NewSItem(GetString(dlSetupColumnsAsIs),
  NewSItem(GetString(dlSetupColumnsLower),
  NewSItem(GetString(dlSetupColumnsFromUpper),
  NewSItem(GetString(dlSetupColumnsUpper),
  NewSItem(GetString(dlSetupColumnsCombine),Nil)))))));
PCluster(Control)^.Value := 1;
Dlg^.Insert(Control);

  R.Assign(2,11,23,12);
  Labl := New(PLabel, Init(R, GetString(dlSetupColumnsFilsCase), Control));
  Dlg^.Insert(Labl);

R.Assign(2,14,68,15);
Control := New(PRadioButtons, Init(R,
  NewSItem(GetString(dlSetupColumnsAsIs),
  NewSItem(GetString(dlSetupColumnsLower),
  NewSItem(GetString(dlSetupColumnsFromUpper),
  NewSItem(GetString(dlSetupColumnsUpper),
  NewSItem(GetString(dlSetupColumnsCombine),Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

  R.Assign(2,13,26,14);
  Labl := New(PLabel, Init(R, GetString(dlSetupColumnsDirsCase), Control));
  Dlg^.Insert(Labl);

R.Assign(17,15,27,17);
Control := New(PButton, Init(R, GetString(dlOkButton), cmOK, bfDefault));
Dlg^.Insert(Control);

R.Assign(29,15,39,17);
Control := New(PButton, Init(R, GetString(dlCancelButton), cmCancel, bfNormal));
Dlg^.Insert(Control);

R.Assign(41,15,51,17);
Control := New(PButton, Init(R, GetString(dlHelpButton), cmHelp, bfNormal));
Dlg^.Insert(Control);

R.Assign(2,2,17,3);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsLongName)));
Dlg^.Insert(Control);

R.Assign(2,3,17,4);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsSize)));
Dlg^.Insert(Control);

R.Assign(2,4,17,5);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsDate)));
Dlg^.Insert(Control);

R.Assign(2,5,17,6);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsTime)));
Dlg^.Insert(Control);

R.Assign(2,6,17,7);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsDescription)));
Dlg^.Insert(Control);

R.Assign(2,7,17,8);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsFullDescription)));
Dlg^.Insert(Control);

R.Assign(2,8,17,9);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsForName)));
Dlg^.Insert(Control);

R.Assign(2,9,17,10);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsForExtention)));
Dlg^.Insert(Control);

R.Assign(20,1,66,2);
Control := New(PStaticText, Init(R, '1    2    3    4    5    6    7    8    9    0'));
Dlg^.Insert(Control);

Dlg^.SelectNext(False);

SetupColumnDefaultsDisk :=
  Application^.ExecuteDialog(Dlg, @ColumnsDefaultsDisk) = cmOK;
end;

function SetupColumnDefaultsFind: Boolean;
var
  Dlg : PDialog;
  R : TRect;
  Control, Labl, Histry : PView;
Begin
R.Assign(5,3,75,20);
New(Dlg, Init(R, GetString(dlSetupColumnsFind)));

R.Assign(18,2,23,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(18,7,23,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(18,8,23,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(23,2,28,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(23,7,28,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(23,8,28,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(28,2,33,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(28,7,33,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(28,8,33,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(33,2,38,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(33,7,38,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(33,8,38,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(38,2,43,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(38,7,43,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(38,8,43,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(43,2,48,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(43,7,48,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(43,8,48,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(48,2,53,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(48,7,53,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(48,8,53,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(53,2,58,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(53,7,58,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(53,8,58,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(58,2,63,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(58,7,63,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(58,8,63,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(63,2,68,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(63,7,68,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(63,8,68,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(2,11,68,12);
Control := New(PRadioButtons, Init(R,
  NewSItem(GetString(dlSetupColumnsAsIs),
  NewSItem(GetString(dlSetupColumnsLower),
  NewSItem(GetString(dlSetupColumnsFromUpper),
  NewSItem(GetString(dlSetupColumnsUpper),
  NewSItem(GetString(dlSetupColumnsCombine),Nil)))))));
PCluster(Control)^.Value := 1;
Dlg^.Insert(Control);

  R.Assign(2,10,23,11);
  Labl := New(PLabel, Init(R, GetString(dlSetupColumnsFilsCase), Control));
  Dlg^.Insert(Labl);

R.Assign(2,13,68,14);
Control := New(PRadioButtons, Init(R,
  NewSItem(GetString(dlSetupColumnsAsIs),
  NewSItem(GetString(dlSetupColumnsLower),
  NewSItem(GetString(dlSetupColumnsFromUpper),
  NewSItem(GetString(dlSetupColumnsUpper),
  NewSItem(GetString(dlSetupColumnsCombine),Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

  R.Assign(2,12,26,13);
  Labl := New(PLabel, Init(R, GetString(dlSetupColumnsDirsCase), Control));
  Dlg^.Insert(Labl);

R.Assign(17,14,27,16);
Control := New(PButton, Init(R, GetString(dlOKButton), cmOk, bfDefault));
Dlg^.Insert(Control);

R.Assign(29,14,39,16);
Control := New(PButton, Init(R, GetString(dlCancelButton), cmCancel, bfNormal));
Dlg^.Insert(Control);

R.Assign(41,14,51,16);
Control := New(PButton, Init(R, GetString(dlHelpButton), cmHelp, bfNormal));
Dlg^.Insert(Control);

R.Assign(2,2,17,3);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsLongName)));
Dlg^.Insert(Control);

R.Assign(2,3,17,4);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsSize)));
Dlg^.Insert(Control);

R.Assign(2,4,17,5);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsDate)));
Dlg^.Insert(Control);

R.Assign(2,5,17,6);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsTime)));
Dlg^.Insert(Control);

R.Assign(2,6,17,7);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsPath)));
Dlg^.Insert(Control);

R.Assign(2,7,17,8);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsForName)));
Dlg^.Insert(Control);

R.Assign(2,8,17,9);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsForExtention)));
Dlg^.Insert(Control);

R.Assign(20,1,66,2);
Control := New(PStaticText, Init(R, '1    2    3    4    5    6    7    8    9    0'));
Dlg^.Insert(Control);

Dlg^.SelectNext(False);

SetupColumnDefaultsFind :=
  Application^.ExecuteDialog(Dlg, @ColumnsDefaultsFind) = cmOK;
end;

function SetupColumnDefaultsTemp: Boolean;
var
  Dlg : PDialog;
  R : TRect;
  Control, Labl, Histry : PView;
Begin
R.Assign(5,3,75,20);
New(Dlg, Init(R, GetString(dlSetupColumnsTemp)));

R.Assign(18,2,23,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(18,7,23,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(18,8,23,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(23,2,28,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(23,7,28,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(23,8,28,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(28,2,33,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(28,7,33,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(28,8,33,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(33,2,38,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(33,7,38,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(33,8,38,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(38,2,43,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(38,7,43,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(38,8,43,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(43,2,48,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(43,7,48,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(43,8,48,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(48,2,53,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(48,7,53,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(48,8,53,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(53,2,58,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(53,7,58,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(53,8,58,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(58,2,63,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(58,7,63,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(58,8,63,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(63,2,68,7);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(63,7,68,8);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(63,8,68,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(2,11,68,12);
Control := New(PRadioButtons, Init(R,
  NewSItem(GetString(dlSetupColumnsAsIs),
  NewSItem(GetString(dlSetupColumnsLower),
  NewSItem(GetString(dlSetupColumnsFromUpper),
  NewSItem(GetString(dlSetupColumnsUpper),
  NewSItem(GetString(dlSetupColumnsCombine),Nil)))))));
PCluster(Control)^.Value := 1;
Dlg^.Insert(Control);

  R.Assign(2,10,23,11);
  Labl := New(PLabel, Init(R, GetString(dlSetupColumnsFilsCase), Control));
  Dlg^.Insert(Labl);

R.Assign(2,13,68,14);
Control := New(PRadioButtons, Init(R,
  NewSItem(GetString(dlSetupColumnsAsIs),
  NewSItem(GetString(dlSetupColumnsLower),
  NewSItem(GetString(dlSetupColumnsFromUpper),
  NewSItem(GetString(dlSetupColumnsUpper),
  NewSItem(GetString(dlSetupColumnsCombine),Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

  R.Assign(2,12,26,13);
  Labl := New(PLabel, Init(R, GetString(dlSetupColumnsDirsCase), Control));
  Dlg^.Insert(Labl);

R.Assign(17,14,27,16);
Control := New(PButton, Init(R, GetString(dlOkButton), cmOk, bfDefault));
Dlg^.Insert(Control);

R.Assign(29,14,39,16);
Control := New(PButton, Init(R, GetString(dlCancelButton), cmCancel, bfNormal));
Dlg^.Insert(Control);

R.Assign(41,14,51,16);
Control := New(PButton, Init(R, GetString(dlHelpButton), cmHelp, bfNormal));
Dlg^.Insert(Control);

R.Assign(2,2,17,3);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsLongName)));
Dlg^.Insert(Control);

R.Assign(2,3,17,4);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsSize)));
Dlg^.Insert(Control);

R.Assign(2,4,17,5);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsDate)));
Dlg^.Insert(Control);

R.Assign(2,5,17,6);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsTime)));
Dlg^.Insert(Control);

R.Assign(2,6,17,7);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsPath)));
Dlg^.Insert(Control);

R.Assign(2,7,17,8);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsForName)));
Dlg^.Insert(Control);

R.Assign(2,8,17,9);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsForExtention)));
Dlg^.Insert(Control);

R.Assign(20,1,66,2);
Control := New(PStaticText, Init(R, '1    2    3    4    5    6    7    8    9    0'));
Dlg^.Insert(Control);

Dlg^.SelectNext(False);
 SetupColumnDefaultsTemp :=
  Application^.ExecuteDialog(Dlg, @ColumnsDefaultsTemp) = cmOK;
end;

function SetupColumnDefaultsArch: Boolean;
var
  Dlg : PDialog;
  R : TRect;
  Control, Labl, Histry : PView;
Begin
R.Assign(5,2,75,20);
New(Dlg, Init(R, GetString(dlSetupColumnsArch)));

R.Assign(18,2,23,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(18,8,23,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(18,9,23,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(23,2,28,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(23,8,28,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(23,9,28,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(28,2,33,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(28,8,33,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(28,9,33,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(33,2,38,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(33,8,38,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(33,9,38,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(38,2,43,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(38,8,43,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(38,9,43,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(43,2,48,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(43,8,48,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(43,9,48,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(48,2,53,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(48,8,53,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(48,9,53,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(53,2,58,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(53,8,58,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(53,9,58,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(58,2,63,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(58,8,63,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(58,9,63,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(63,2,68,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(63,8,68,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(63,9,68,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(2,12,68,13);
Control := New(PRadioButtons, Init(R,
  NewSItem(GetString(dlSetupColumnsAsIs),
  NewSItem(GetString(dlSetupColumnsLower),
  NewSItem(GetString(dlSetupColumnsFromUpper),
  NewSItem(GetString(dlSetupColumnsUpper),
  NewSItem(GetString(dlSetupColumnsCombine),Nil)))))));
PCluster(Control)^.Value := 1;
Dlg^.Insert(Control);

  R.Assign(2,11,23,12);
  Labl := New(PLabel, Init(R, GetString(dlSetupColumnsFilsCase), Control));
  Dlg^.Insert(Labl);

R.Assign(2,14,68,15);
Control := New(PRadioButtons, Init(R,
  NewSItem(GetString(dlSetupColumnsAsIs),
  NewSItem(GetString(dlSetupColumnsLower),
  NewSItem(GetString(dlSetupColumnsFromUpper),
  NewSItem(GetString(dlSetupColumnsUpper),
  NewSItem(GetString(dlSetupColumnsCombine),Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

  R.Assign(2,13,26,14);
  Labl := New(PLabel, Init(R, GetString(dlSetupColumnsDirsCase), Control));
  Dlg^.Insert(Labl);

R.Assign(17,15,27,17);
Control := New(PButton, Init(R, GetString(dlOKButton), cmOk, bfDefault));
Dlg^.Insert(Control);

R.Assign(29,15,39,17);
Control := New(PButton, Init(R, GetString(dlCancelButton), cmCancel, bfNormal));
Dlg^.Insert(Control);

R.Assign(41,15,51,17);
Control := New(PButton, Init(R, GetString(dlHelpButton), cmHelp, bfNormal));
Dlg^.Insert(Control);

R.Assign(2,2,17,3);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsLongName)));
Dlg^.Insert(Control);

R.Assign(2,3,17,4);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsSourceSize)));
Dlg^.Insert(Control);

R.Assign(2,4,17,5);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsDate)));
Dlg^.Insert(Control);

R.Assign(2,5,17,6);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsTime)));
Dlg^.Insert(Control);

R.Assign(2,6,17,7);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsPackedSize)));
Dlg^.Insert(Control);

R.Assign(2,7,17,8);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsRatio)));
Dlg^.Insert(Control);

R.Assign(2,8,17,9);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsForName)));
Dlg^.Insert(Control);

R.Assign(2,9,17,10);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsForExtention)));
Dlg^.Insert(Control);

R.Assign(20,1,66,2);
Control := New(PStaticText, Init(R, '1    2    3    4    5    6    7    8    9    0'));
Dlg^.Insert(Control);

Dlg^.SelectNext(False);

 SetupColumnDefaultsArch :=
  Application^.ExecuteDialog(Dlg, @ColumnsDefaultsArch) = cmOK;
end;

function SetupColumnDefaultsArvd: Boolean;
var
  Dlg : PDialog;
  R : TRect;
  Control, Labl, Histry : PView;
Begin
R.Assign(5,2,75,20);
New(Dlg, Init(R, GetString(dlSetupColumnsArvd)));

R.Assign(18,2,23,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(18,8,23,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(18,9,23,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(23,2,28,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(23,8,28,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(23,9,28,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(28,2,33,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(28,8,33,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(28,9,33,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(33,2,38,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(33,8,38,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(33,9,38,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(38,2,43,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(38,8,43,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(38,9,43,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(43,2,48,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(43,8,48,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(43,9,48,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(48,2,53,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(48,8,53,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(48,9,53,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(53,2,58,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(53,8,58,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(53,9,58,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(58,2,63,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(58,8,63,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(58,9,63,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(63,2,68,8);
Control := New(PCheckboxes, Init(R,
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',
  NewSItem('',Nil))))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

R.Assign(63,8,68,9);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(63,9,68,10);
Control := New(PInputLine, Init(R, 2));
Dlg^.Insert(Control);

R.Assign(2,12,68,13);
Control := New(PRadioButtons, Init(R,
  NewSItem(GetString(dlSetupColumnsAsIs),
  NewSItem(GetString(dlSetupColumnsLower),
  NewSItem(GetString(dlSetupColumnsFromUpper),
  NewSItem(GetString(dlSetupColumnsUpper),
  NewSItem(GetString(dlSetupColumnsCombine),Nil)))))));
PCluster(Control)^.Value := 1;
Dlg^.Insert(Control);

  R.Assign(2,11,23,12);
  Labl := New(PLabel, Init(R, GetString(dlSetupColumnsFilsCase), Control));
  Dlg^.Insert(Labl);

R.Assign(2,14,68,15);
Control := New(PRadioButtons, Init(R,
  NewSItem(GetString(dlSetupColumnsAsIs),
  NewSItem(GetString(dlSetupColumnsLower),
  NewSItem(GetString(dlSetupColumnsFromUpper),
  NewSItem(GetString(dlSetupColumnsUpper),
  NewSItem(GetString(dlSetupColumnsCombine),Nil)))))));
PCluster(Control)^.Value := 0;
Dlg^.Insert(Control);

  R.Assign(2,13,26,14);
  Labl := New(PLabel, Init(R, GetString(dlSetupColumnsDirsCase), Control));
  Dlg^.Insert(Labl);

R.Assign(17,15,27,17);
Control := New(PButton, Init(R, GetString(dlOKButton), cmOk, bfDefault));
Dlg^.Insert(Control);

R.Assign(29,15,39,17);
Control := New(PButton, Init(R, GetString(dlCancelButton), cmCancel, bfNormal));
Dlg^.Insert(Control);

R.Assign(41,15,51,17);
Control := New(PButton, Init(R, GetString(dlHelpButton), cmHelp, bfNormal));
Dlg^.Insert(Control);

R.Assign(2,2,17,3);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsLongName)));
Dlg^.Insert(Control);

R.Assign(2,3,17,4);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsSourceSize)));
Dlg^.Insert(Control);

R.Assign(2,4,17,5);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsDate)));
Dlg^.Insert(Control);

R.Assign(2,5,17,6);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsTime)));
Dlg^.Insert(Control);

R.Assign(2,6,17,7);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsPackedSize)));
Dlg^.Insert(Control);

R.Assign(2,7,17,8);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsRatio)));
Dlg^.Insert(Control);

R.Assign(2,8,17,9);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsForName)));
Dlg^.Insert(Control);

R.Assign(2,9,17,10);
Control := New(PStaticText, Init(R, GetString(dlSetupColumnsForExtention)));
Dlg^.Insert(Control);

R.Assign(20,1,66,2);
Control := New(PStaticText, Init(R, '1    2    3    4    5    6    7    8    9    0'));
Dlg^.Insert(Control);

Dlg^.SelectNext(False);

 SetupColumnDefaultsArvd :=
  Application^.ExecuteDialog(Dlg, @ColumnsDefaultsArvd) = cmOK;
end;

PROCEDURE TCurrDriveInfo.HandleEvent;
var
  W: Word;
  Data: TSysData;
begin
  inherited HandleEvent( Event );
  If ( Event.What = evBroadcast ) and ( Event.Command = cmScrollBarChanged ) then begin
    W := PSysDialog( Owner )^.LocalData.Drives[ Char( Byte( 'A' ) + PScrollBar( Event.InfoPtr )^.Value ) ];
      SetData( W );
  end else
  If ( Event.What = evKeydown ) and ( Event.CharCode = ' ' ) and ( TypeOf( Owner^.Current^ ) = TypeOf( TListBox ))
    then Press( 0 );
end;

PROCEDURE TCurrDriveInfo.Press;
var
  Data: TSysData;
begin
  inherited Press( Item );
  Owner^.GetData( Data );
  PSysDialog( Owner )^.LocalData.Drives[ Char( Byte( 'A' ) + Data.Drives.Focus ) ] := Value;
end;

PROCEDURE TSysDialog.Awaken;
var
  C: Char;
begin
  LocalData := SystemData;
  New( SysData.Drives.List, Init( 26, 1 ));
  For C := 'A' to 'Z' do SysData.Drives.List^.Insert( NewStr( C + ':' ));
  Move( SystemData, SysData, SizeOf( SysData.Options ) + SizeOf( SysData.Mode1 )*2 );
  SysData.Temp := SystemData.Temp;
  SysData.Drives.Focus := 2;
  SysData.Current := LocalData.Drives[ 'C' ];
  SetData( SysData );
end;

DESTRUCTOR TSysDialog.Done;
var
  Data: TSysData;
begin
  GetData( Data );
  inherited Done;
  Dispose( Data.Drives.List, Done );
end;

PROCEDURE TSysDialog.GetData;
var
  Data: TSysData;
begin
  inherited GetData( Data );
  TSysData( Rec ) := Data;
  LocalData.Options := Data.Options;
  LocalData.Mode1 := Data.Mode1;
  LocalData.Mode2 := Data.Mode2;
  LocalData.Temp := Data.Temp;
end;


{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
function MakeStartupDialog : PDialog;
var
  Dlg : PDialog;
  R : TRect;
  Control, Labl, Histry : PView;
begin
  R.Assign(5,0,34,13);
  New(Dlg, Init(R, 'Startup'));
  With Dlg^ do begin
    Options := Options or ofCentered;

    R.Assign(2,2,27,8);
    Control := New(PCheckboxes, Init(R,
      NewSItem('Autosave ~D~esktop',
      NewSItem('Auto run ~U~ser Menu',
      NewSItem('Disable ~b~linking',
      NewSItem('~R~estore screen mode',
      NewSItem('~K~ill History',
      NewSItem('OS/~2~ support',
      Nil))))))));
    Insert(Control);

    R.Assign(4,9,14,11);
    Control := New(PButton, Init(R, 'O~K~', cmOK, bfDefault));
    Insert(Control);

    R.Assign(15,9,25,11);
    Control := New(PButton, Init(R, 'Cancel', cmCancel, bfNormal));
    Insert(Control);

    SelectNext(False);
  end;
  MakeStartupDialog := Dlg;
end;

{                                 Mouse Setup
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
CONSTRUCTOR TMouseBar.Init;
begin
  inherited Init( Bounds );
  Options := Options or ofSelectable;
  SetParams( 0, 0, Size.X-1, 3, 1 );
end;

const
  HSenseY = 3;

FUNCTION TMouseBar.DataSize; assembler;
asm mov ax,2 end;

PROCEDURE TMouseBar.SetData;
begin SetValue( Integer( Rec )) end;

PROCEDURE TMouseBar.GetData;
begin Integer( Rec ) := Value end;

PROCEDURE TMouseBar.HandleEvent;
begin
  inherited HandleEvent( Event );
end;

CONSTRUCTOR TMouseDialog.Init;
var
  R: TRect;
  Control, Labl: PView;
begin
  R.Assign(0,0,39,13);
  inherited Init( R, 'Mouse Setup' );
  Options := Options or ofCentered;

  R.Assign(3,HSenseY,26,HSenseY + 1);
  Control := New( PMouseBar, Init( R ));
  Insert(Control);

  R.Assign(3,2,26,3);
  Labl := New(PLabel, Init(R, '~X~-Sensitivity', Control));
  Insert(Labl);

  R.Assign(3,HSenseY + 3,26,HSenseY + 4);
  Control := New(PMouseBar, Init(R));
  Insert(Control);

  R.Assign(3,5,24,6);
  Labl := New(PLabel, Init(R, '~Y~-sensitivity', Control));
  Insert(Labl);

  R.Assign(3,9,26,11);
  Control := New(PCheckboxes, Init(R,
    NewSItem('~R~everse buttons',
    NewSItem('~S~tandard cursor',Nil))));
  Insert(Control);

  R.Assign(3,8,12,9);
  Labl := New(PLabel, Init(R, '~O~ptions', Control));
  Insert(Labl);

  R.Assign(27,3,37,5);
  Control := New(PButton, Init(R, 'O~K~', cmOK, bfDefault));
  Insert(Control);

  R.Assign(27,5,37,7);
  Control := New(PButton, Init(R, 'Cancel', cmCancel, bfNormal));
  Insert(Control);

  R.Assign(27,9,37,11);
  Control := New(PButton, Init(R, '~H~elp', cmHelp, bfNormal));
  Insert(Control);

  SelectNext(False);
end;

function MakeMouseDialog: PDialog;
begin
  MakeMouseDialog := New( PMouseDialog, Init );
end;

{                               Interface Setup
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
function MakeInterfaceDialog : PDialog;
var
  Dlg : PDialog;
  R : TRect;
  Control, Labl, Histry : PView;
begin
  R.Assign(0,0,46,17);
  New(Dlg, Init(R, 'Interface Setup'));
  With Dlg^ do begin
    Options := Options or ofCentered;
    R.Assign(3,3,33,7);
    Control := New(PCheckboxes, Init(R,
      NewSItem('~C~lock',
      NewSItem('Hide ~m~enu bar',
      NewSItem('Hide ~s~tatus line',
      NewSItem('~E~SC for user screen',
{     NewSItem('~Q~uiet archive processing',}
      Nil))))));
    Insert(Control);

    R.Assign(3,2,11,3);
    Labl := New(PLabel, Init(R, '~O~ptions', Control));
    Insert(Labl);

    R.Assign(3,09,33,11);
    Control := New(PRadioButtons, Init(R,
      NewSItem('Alt-Tab and Ctrl-Tab',
      NewSItem('F9 and Shift-F9',Nil))));
    Insert(Control);

    R.Assign(3,8,21,09);
    Labl := New(PLabel, Init(R, '~W~indows switching', Control));
    Insert(Labl);

    R.Assign(3,13,33,15);
    Control := New(PRadioButtons, Init(R,
      NewSItem('~F~ile Panel',
      NewSItem('Command ~l~ine',Nil))));
    Insert(Control);

    R.Assign(3,12,20,13);
    Labl := New(PLabel, Init(R, '/'#26' ~a~rrows usage', Control));
    Insert(Labl);

    R.Assign(34,3,44,5);
    Control := New(PButton, Init(R, 'O~K~', cmOK, bfDefault));
    Insert(Control);

    R.Assign(34,5,44,7);
    Control := New(PButton, Init(R, 'Cancel', cmCancel, bfNormal));
    Insert(Control);

    R.Assign(34,13,44,15);
    Control := New(PButton, Init(R, '~H~elp', cmHelp, bfNormal));
    Insert(Control);

    SelectNext(False);
  end;
  MakeInterfaceDialog := Dlg;
end;

{                                Savers Setup
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
PROCEDURE TSaversListBox.HandleEvent;
var
  PS: PString;
  F: Integer;
  A, S: PCollection;
  LocalData: TSaversData;
  function SeekStr( P: PString ): boolean; far;
  begin
    SeekStr := ( P <> NIL ) and ( P^ = PS^ );
  end;
begin
  If Event.What = evBroadcast then
    case Event.Command of
      cmYes: begin
          Owner^.GetData( LocalData );
          A := LocalData.Available.List;
          If A^.Count > 0 then begin
            PS := A^.At( LocalData.Available.Focus );
            If ( PS <> NIL ) and ( List^.FirstThat( @SeekStr ) = NIL ) then begin
              List^.Insert( NewStr( PS^ ));
              S := List;
              List := NIL;
              NewList( S );
            end;
          end;
          ClearEvent( Event );
        end;
      cmNo: begin
          F := Focused;
          If F < List^.Count then begin
            S := List;
            S^.AtFree( F );
            List := NIL;
            Owner^.Lock;
            NewList( S );
            If ( F > 0 ) and ( F >= List^.Count ) then Dec( F );
            FocusItem( F );
            Owner^.Unlock;
          end;
          ClearEvent( Event );
        end;
    end;
  inherited HandleEvent( Event );
end;

CONSTRUCTOR TSaversDialog.Init;
var
  R: TRect;
  D: PDialog;
  Control, Labl, Histry: PView;
begin
  R.Assign(0,0,57,20);
  inherited Init(R, GetString(dlScreenSaverSetup));
  Options := Options or ofCentered or ofValidate;
  HelpCtx := hcSavers;
  R.Assign(19,3,20,13);
  Control := New(PScrollbar, Init(R));
  Insert(Control);

  R.Assign(2,3,19,13);
  Control := New(PSaversListBox, Init(R, 1, PScrollbar(Control)));
  Insert(Control);

  R.Assign(2,2,18,3);
  Labl := New(PLabel, Init(R, GetString(dlSS_S_electedSavers), Control));
  Insert(Labl);

  R.Assign(20,6,36,8);
  Control := New(PButton, Init(R, GetString(dlSS_A_dd), cmYes, bfNormal + bfBroadcast ));
  Insert(Control);

  R.Assign(20,8,36,10);
  Control := New(PButton, Init(R, GetString(dlSS_R_emove), cmNo, bfNormal + bfBroadcast ));
  Insert(Control);

  R.Assign(54,3,55,13);
  Control := New(PScrollbar, Init(R));
  Insert(Control);

  R.Assign(37,3,54,13);
  Control := New(PListBox, Init(R, 1, PScrollbar(Control)));
  Insert(Control);

  R.Assign(37,2,54,3);
  Labl := New(PLabel, Init(R, GetString(dlSSA_v_ailableSavers), Control));
  Insert(Labl);

  R.Assign(2,15,18,16);
  Control := New(PInputLine, Init(R, 3));
  PInputLine(Control)^.SetValidator(New(PRangeValidator, Init(0, 254)));
  Control^.Options := Control^.Options or ofValidate;
  Insert(Control);

  R.Assign(2,14,18,15);
  Labl := New(PLabel, Init(R, GetString(dlSS_T_ime), Control));
  Insert(Labl);

  R.Assign(20,15,55,16);
  Control := New(PCheckboxes, Init(R,
    NewSItem(GetString(dlSSUse_M_ouse),Nil)));
  Insert(Control);

  R.Assign(10,17,20,19);
  Control := New(PButton, Init(R, GetString(dlOKButton), cmOK, bfDefault));
  Insert(Control);

  R.Assign(20,17,30,19);
  Control := New(PButton, Init(R, GetString(dlCancelButton), cmCancel, bfNormal));
  Insert(Control);

  R.Assign(30,17,40,19);
  Control := New(PButton, Init(R, GetString(dlHelpButton), cmHelp, bfNormal));
  Insert(Control);

  R.Assign(40,17,50,19);
  Control := New(PButton, Init(R, GetString(dlTestButton), cmTest, bfNormal));
  Insert(Control);

  SelectNext(False);
{$IFNDEF AsUnit} Awaken {$ENDIF}
end;

procedure TSaversDialog.HandleEvent( var Event: TEvent );
begin
 inherited HandleEvent(Event);
 case Event.What of
  evCommand: case Event.Command of
              cmTest: begin
                       ClearEvent(Event);
                       Application^.InsertIdlerN(SaversData.Selected.Focus);
                      end;
             end;
 end;
end;

        {-DataCompBoy-}
PROCEDURE TSaversDialog.Awaken;
var
  lSR: lSearchRec;
  Data: TSaversData;
begin
  Data := SaversData;
  Data.Available.Focus := 0;
  Data.Selected.Focus := 0;
  New( Data.Available.List, Init( 5, 5 ));
  If Data.Selected.List = NIL
    then New( Data.Selected.List, Init( 5, 5 ));
  With Data.Available.List^ do begin
    Insert( NewStr( 'ù Star flight' ));
    Insert( NewStr( 'ù Flash-light' ));
    Insert( NewStr( 'ù Clock' ));
    Insert( NewStr( 'ù Blackness' ));
  {$IFDEF AsUnit}
    lFindFirst( SourceDir + 'SSAVERS\*.SS', AnyFile, lSR );
  {$ELSE}
    lFindFirst( '*.PAS', AnyFile, lSR );
  {$ENDIF}
    While DosError = 0 do begin
      Insert( NewStr( lSR.FullName ));
      lFindNext( lSR );
    end;
    lFindClose(lSR);
  end;
  SetData( Data );
end;
        {-DataCompBoy-}

DESTRUCTOR TSaversDialog.Done;
var
  Data: TSaversData;
begin
  GetData( Data );
  inherited Done;
  if (Data.Available.List <> nil) then Dispose( Data.Available.List, Done );
end;

FUNCTION MakeSaversDialog : PDialog;
begin
  MakeSaversDialog := New( PSaversDialog, Init );
end;

constructor TUpperTable.Init;
 var I: Integer;
begin
  inherited Init(R);
  for I := 0 to 127 do Table[I] := Char(I+128);
  CurrentChar := 0; Delta := 0;
  Options := Options or ofSelectable;
  EventMask := evKeyDown or evMouse;
end;

function TUpperTable.GetPalette;
 const S: String[Length(CInputLine)] = CInputLine;
begin
  GetPalette := @S;
end;

function TUpperTable.DataSize; begin DataSize := 128; end;
procedure TUpperTable.GetData; begin Move(Table, Rec, 128); end;
procedure TUpperTable.SetData; begin Move(Rec, Table, 128); end;

procedure TUpperTable.Draw;
  var B: TDrawBuffer;
      I: Integer;
      C: Word;
begin
  if CurrentChar < Delta then Delta := CurrentChar;
  if CurrentChar - Delta > Size.X - 1 then Delta := CurrentChar - Size.X + 1;
  C := GetColor(1);
  MoveChar(B, ' ', C, Size.X);
  for I := 0 to Size.X - 1 do
      WordRec(B[I]).Lo := I + Delta + 128;
  WordRec(B[CurrentChar - Delta]).Hi := GetColor(3);
  WriteLine(0,0,Size.X,1,B);
  MoveChar(B, ' ', C, Size.X);
  for I := 0 to Size.X - 1 do
      WordRec(B[I]).Lo := Byte(Table[I + Delta]);
  WordRec(B[CurrentChar - Delta]).Hi := GetColor(3);
  WriteLine(0,1,Size.X,1,B);
  ShowCursor; SetCursor(CurrentChar - Delta, 1); NormalCursor;
end;

procedure TUpperTable.HandleEvent;
  procedure CE; begin ClearEvent(Event); DrawView; end;
begin
  inherited HandleEvent(Event);
  case Event.What of
    evKeyDown: case Event.KeyCode of
                 kbLeft: begin if CurrentChar > 0 then Dec(CurrentChar); CE end;
                 kbRight: begin if CurrentChar < 127 then Inc(CurrentChar); CE end;
                 else
                 if Event.CharCode in ['A'..'Z','a'..'z',#128..#255] then begin
                          Table[CurrentChar] := Event.CharCode;
                          Message(@Self, evKeyDown, kbRight, nil); CE
                        end;
               end;
  end;
end;

{$IFNDEF AsUnit}
{                                 Application
{ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ}
VAR
  T: TMyApp;

procedure TMyApp.InitMenuBar;
var
  R: TRect;
begin
  GetExtent( R );
  R.B.Y := R.A.Y + 1;
  New( PMenuBar( MenuBar ), Init( R, NewMenu(
    NewItem( '~I~nterface', '', kbF1, cmInterface, 0,
    NewItem( '~M~ouse', '', kbF2, cmMouse, 0,
    NewItem( '~S~avers', '', kbF3, cmSavers, 0,
    NewItem( 'Start~U~p', '', kbF4, cmStartUp, 0,
    NewItem( 'S~y~stem', '', kbF5, cmSystem, 0,
  NIL ) )))))));
end;

procedure TMyApp.HandleEvent;
var
  D: PDialog;
begin
  inherited HandleEvent( Event );
  If Event.What = evCommand then begin
    case Event.Command of
      cmInterface: D := MakeInterfaceDialog;
      cmMouse: D := MakeMouseDialog;
      cmSavers: D := MakeSaversDialog;
      cmStartUp: D := MakeStartupDialog;
      cmSystem: begin CountrySetup; exit end;
      else Exit
    end;
    If D <> NIL then begin
      Desktop^.ExecView( D );
      Dispose( D, Done );
    end;
  end;
end;


BEGIN
  FillChar( SystemData, SizeOf( SystemData ), 0 );
  SystemData.Options := 3;
  SystemData.Mode1 := '29';
  SystemData.Mode2 := '30';
  SystemData.Drives[ 'A' ] := ossDirectAccess;
  SystemData.Drives[ 'B' ] := ossDirectAccess;
  SystemData.Drives[ 'C' ] := ossTree;
  SystemData.Drives[ 'D' ] := ossTree;
  T.Init;
  MenuBar^.MoveTo(0,0); MenuBar^.GrowTo(80, 1);
  Desktop^.MoveTo(0,1);
  T.Run;
  T.Done;
{$ENDIF}
END.
