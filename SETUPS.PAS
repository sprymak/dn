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

unit Setups;

interface

uses
  Dos, Lfn, {DataCompBoy}Objects, Drivers, Views, Dialogs, Collect,
    DNApp,
  Commands, RStrings, Startup, Validate;

{                                System Setup                                }
{----------------------------------------------------------------------------}
type
  TSysData = record
    Options: word;
    Mode1: String[5];
    Mode2: String[5];
    Temp: String;
    Drives: TTextListboxRec;
    Current: word;
    CopyLimitBuf: String[5];
    ForceDefArch: String[3];
    end;

  PSysDialog = ^TSysDialog;
  TSysDialog = object(TDialog)
    LocalData: TSystemData;
    SysData: TSysData;
    {constructor Init;}
    procedure Awaken; virtual;
    destructor Done; virtual;
    procedure GetData(var Rec); virtual;
    end;

  PCurrDriveInfo = ^TCurrDriveInfo;
  TCurrDriveInfo = object(TCheckBoxes)
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure Press(Item: integer); virtual;
    end;

  PMouseBar = ^TMouseBar;
  TMouseBar = object(TScrollBar)
    Constructor Init(var Bounds: TRect);
    procedure SetData(var Rec); virtual;
    procedure GetData(var Rec); virtual;
    function DataSize: word; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    end;

  {$IFDEF SS}
  PSaversDialog = ^TSaversDialog;
  TSaversDialog = object(TDialog)
    Constructor Init;
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    procedure Awaken; virtual;
    end;

  PSaversListBox = ^TSaversListBox;
  TSaversListBox = object(TListBox)
    procedure HandleEvent(var Event: TEvent); virtual;
    end;
  {$ENDIF}

  PUpperTable = ^TUpperTable;
  TUpperTable = object(TView)
    Table: array[0..127] of Char;
    CurrentChar: integer;
    Delta: integer;
    Constructor Init(var R: TRect);
    procedure Draw; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function DataSize: word; virtual;
    procedure GetData(var Rec); virtual;
    procedure SetData(var Rec); virtual;
    function GetPalette: PPalette; virtual;
    end;

procedure SetupCountryInfo;
procedure FMDefaults;
procedure FMSetup;
procedure DriveInfoSetup;
procedure SetupEditorDefaults;
procedure SystemSetup;
procedure InterfaceSetup;
procedure StartupSetup;
procedure MouseSetup;
procedure ConfirmSetup;
function TerminalSetup: boolean;
function SetupColumnDefaultsDisk: boolean;
function SetupColumnDefaultsFind: boolean;
function SetupColumnDefaultsTemp: boolean;
function SetupColumnDefaultsArch: boolean;
function SetupColumnDefaultsArvd: boolean;
{$IFDEF SS}
procedure SaversSetup;
function MakeSaversDialog: PDialog;
{$ENDIF}
implementation
uses
  Tree, Drives, advance, advance1, advance2, advance3, Messages,
    DNHelp,
  advance6, DnIni;

procedure ConfirmSetup;
  var
    D: word;
  begin
    Confirms := Confirms and $7F+
    (Confirms and cfFmtOs2Warning) shr 1+
    (Confirms and cfExitConfirm) shl 1;
    D := ExecResource(dlgConfirmations, Confirms);
    Confirms := Confirms and $7F+
    (Confirms and cfFmtOs2Warning) shr 1+
    (Confirms and cfExitConfirm) shl 1;
    if D <> cmOK then
      exit;
    ConfigModified := True;

    ConfirmsOpt := Confirms;
    SaveDnIniSettings(@ConfirmsOpt);
    DoneIniEngine;
  end;

function TerminalSetup;
  begin
    TerminalSetup := False;
    if ExecResource(dlgSetupTerminal, TerminalDefaults) <> cmOK then
        exit;
    TerminalSetup := True;
    Message(Application, evCommand, cmUpdateConfig, nil);
  end;

procedure SystemSetup;
  var
    W: word;
    D: PDialog;
    B: boolean;
    Data: TSysData;
    i: Char;
  begin
    OpenResource;
    if Resource = nil then
      exit;
    D := PDialog(Application^.ValidView(PDialog(Resource^.Get(
      dlgSystemSetup))));
    if D = nil then
      exit;
    W := Desktop^.ExecView(D);
    if W <> cmCancel then
      begin
        D^.GetData(Data);
        SystemData := PSysDialog(D)^.LocalData;
        Message(Application, evCommand, cmUpdateConfig, nil);
      end;
    Dispose(D, Done);
    HandleChDirCommand := ((SystemData.Options shl 3) and
      ossHandleChDirCommand) <> 0;
    SystemDataOpt := SystemData.Options;
    CopyLimit := SystemData.CopyLimitBuf;
    ForceDefaultArchiver := SystemData.ForceDefArch;

    SaveDnIniSettings(@HandleChDirCommand);
    SaveDnIniSettings(@SystemDataOpt);
    SaveDnIniSettings(@CopyLimit);
    SaveDnIniSettings(@ForceDefaultArchiver);
    DoneIniEngine;
  end { SystemSetup };

procedure InterfaceSetup;
  var
    AltTab: boolean;
    R: TRect;
  begin
    if ShowScrollBar then
      InterfaceData.Options := InterfaceData.Options or
        ouiShowScrollBar
    else
      InterfaceData.Options := InterfaceData.Options and not
        ouiShowScrollBar;
    with PApplication(Application)^ do
      if ExecResource(dlgInterfaceSetup, InterfaceData) <> cmCancel
      then
        begin
          GetExtent(R);
          if InterfaceData.Options and ouiHideMenu = 0 then
            Inc(R.A.Y);
          if InterfaceData.Options and ouiHideStatus = 0 then
            Dec(R.B.Y);
          if InterfaceData.Options and ouiHideCmdline = 0 then
            Dec(R.B.Y);
          if (InterfaceData.Options and ouiShowScrollBar <> 0) <>
              ShowScrollBar
          then
            begin
              Desktop^.Lock;
              ShowScrollBar := InterfaceData.Options and
                ouiShowScrollBar <> 0;
              if ShowScrollBar then
                begin
                  Desktop^.Hide;
                  Desktop^.Show;
                end
              else
                begin
                  Message(Application, evKeyDown, kbTab, nil);
                  Message(Application, evKeyDown, kbTab, nil);
                end;
              Desktop^.UnLock;
              Desktop^.Redraw;
            end;
          Desktop^.Locate(R);
          R.A.Y := R.B.Y;
          R.B.Y := R.A.Y+byte(InterfaceData.Options and
            ouiHideCmdline = 0);
          CommandLine^.Locate(R);
          CommandLine^.SetState(sfVisible, InterfaceData.Options and
            ouiHideCmdline = 0);
          Message(Application, evCommand, cmUpdateConfig, nil);
          if InterfaceData.Options and ouiClock <> 0 then
            if not Clock^.GetState(sfVisible) then
              Clock^.Show;
          if InterfaceData.Options and ouiClock = 0 then
            if Clock^.GetState(sfVisible) then
              Clock^.Hide;
        end;
    InterfaceDataOpt := InterfaceData.Options;
    DriveInfoType := InterfaceData.DrvInfType;
    SaveDnIniSettings(@InterfaceDataOpt);
    SaveDnIniSettings(@DriveInfoType);
    DoneIniEngine;
  end { InterfaceSetup };

procedure StartupSetup;
  var
    Data: record
      Load, Unload, Slice2: word;
      end;
  begin
    Data.Load := StartupData.Load;
    Data.Unload := StartupData.Unload;
    Data.Slice2 := StartupData.Slice2;
    if ExecResource(dlgStartupSetup, Data) <> cmCancel then
      begin
        StartupData.Load := Data.Load;
        StartupData.Unload := Data.Unload;
        StartupData.Slice2 := Data.Slice2;
        LSliceCnt := -3;
        Message(Application, evCommand, cmUpdateConfig, nil);

        StartupDataLoad := StartupData.Load;
        StartupDataUnload := StartupData.Unload;
        StartupDataSlice2 := StartupData.Slice2;
        SaveDnIniSettings(@StartupDataLoad);
        SaveDnIniSettings(@StartupDataUnload);
        SaveDnIniSettings(@StartupDataSlice2);
        DoneIniEngine;
      end;
  end { StartupSetup };

procedure MouseSetup;
  begin
    if ExecResource(dlgMouseSetup, MouseData) <> cmOK then
      exit;
    if MouseVisible xor (MouseData.Options and omsCursor <> 0) then
        begin
        DoneEvents;
        InitEvents;
      end;
    MouseReverse := MouseData.Options and omsReverse <> 0;
    SetMouseSpeed(MouseData.HSense, MouseData.VSense);
    Message(Application, evCommand, cmUpdateConfig, nil);
  end;

{$IFDEF SS}
procedure SaversSetup;
  var
    W: word;
    D: PDialog;
    B: boolean;
  begin
    OpenResource;
    if Resource = nil then
      exit;
    D := PDialog(Application^.ValidView(PDialog(Resource^.Get(
      dlgSaversSetup))));
    if D = nil then
      exit;
    W := Desktop^.ExecView(D);
    if W <> cmCancel then
      begin
        D^.GetData(SaversData);
        Message(Application, evCommand, cmUpdateConfig, nil);
      end;
    Dispose(D, Done);
  end;
{$ENDIF}

procedure SetupCountryInfo;
  begin
    if ExecResource(dlgCountrySetup, CountryInfo) <> cmOK then
      exit;
    InitUpcase;
    GlobalMessage(evCommand, cmReboundPanel, nil);
    ConfigModified := True;
  end;

procedure FMDefaults;
  begin
    if ExecResource(dlgFMDefaults, Startup.PanelDefaults) <> cmOK
    then
      exit;
    Message(Application, evCommand, cmUpdateConfig, nil);
    GlobalMessage(evCommand, cmReboundPanel, nil);
  end;

procedure FMSetup;
  begin
    if ExecResource(dlgFMSetup, Startup.FMSetup) <> cmOK then
      exit;
    Message(Application, evCommand, cmUpdateConfig, nil);
    GlobalMessage(evCommand, cmReboundPanel, nil);

    FMSetupOpt := Startup.FMSetup.Options;
    SaveDnIniSettings(@FMSetupOpt);
    DoneIniEngine;
  end;

procedure DriveInfoSetup;
  var
    W: word;
  begin
    Startup.DriveInfoData := Startup.DriveInfoData and $001F+
    Startup.DriveInfoData and $07E0 shl 2+
    Startup.DriveInfoData and $1800 shr 6;

    if ExecResource(dlgDriveInfoSetup, Startup.DriveInfoData) <>
        cmOK
    then
      exit;
    Startup.DriveInfoData := Startup.DriveInfoData and $001F+
    Startup.DriveInfoData and $FF80 shr 2+
    Startup.DriveInfoData and $0060 shl 6;
    Message(Application, evCommand, cmUpdateConfig, nil);
    GlobalMessage(evCommand, cmReboundPanel, nil);
  end;

procedure SetupEditorDefaults;
  begin
    if ExecResource(dlgEditorDefaults, EditorDefaults) = cmOK then
        begin
        if StoI(EditorDefaults.TabSize) < 2 then
          EditorDefaults.TabSize := '2'; {-$VOL}
        Message(Application, evCommand, cmUpdateConfig, nil);
        EditorDefaultsOpt := EditorDefaults.EdOpt;
        EditorDefaultsOpt2 := EditorDefaults.EdOpt2;
        ViewerOpt := EditorDefaults.ViOpt;
        SaveDnIniSettings(@EditorDefaultsOpt);
        SaveDnIniSettings(@EditorDefaultsOpt2);
        SaveDnIniSettings(@ViewerOpt);
        DoneIniEngine;
      end;
  end;

function SetupColumnDefaultsDisk: boolean;
  var
    i: byte;
  begin
    SetupColumnDefaultsDisk :=
    ExecResource(dlgSetupColumnsDisk, ColumnsDefaultsDisk) = cmOK;
    for i := 1 to 10 do
      begin
        if StoI(ColumnsDefaultsDisk.Params[i].LFNLen) > 252 then
          ColumnsDefaultsDisk.Params[i].LFNLen := '252';
        if StoI(ColumnsDefaultsDisk.Params[i].EXTLen) > 247 then
          ColumnsDefaultsDisk.Params[i].EXTLen := '247';
      end;
  end;

function SetupColumnDefaultsFind: boolean;
  var
    i: byte;
  begin
    SetupColumnDefaultsFind :=
    ExecResource(dlgSetupColumnsFind, ColumnsDefaultsFind) = cmOK;
    for i := 1 to 10 do
      begin
        if StoI(ColumnsDefaultsFind.Params[i].LFNLen) > 252 then
          ColumnsDefaultsFind.Params[i].LFNLen := '252';
        if StoI(ColumnsDefaultsFind.Params[i].EXTLen) > 247 then
          ColumnsDefaultsFind.Params[i].EXTLen := '247';
      end;
  end;

function SetupColumnDefaultsTemp: boolean;
  var
    i: byte;
  begin
    SetupColumnDefaultsTemp :=
    ExecResource(dlgSetupColumnsTemp, ColumnsDefaultsTemp) = cmOK;
    for i := 1 to 10 do
      begin
        if StoI(ColumnsDefaultsTemp.Params[i].LFNLen) > 252 then
          ColumnsDefaultsTemp.Params[i].LFNLen := '252';
        if StoI(ColumnsDefaultsTemp.Params[i].EXTLen) > 247 then
          ColumnsDefaultsTemp.Params[i].EXTLen := '247';
      end;
  end;

function SetupColumnDefaultsArch: boolean;
  var
    i: byte;
  begin
    SetupColumnDefaultsArch :=
    ExecResource(dlgSetupColumnsArch, ColumnsDefaultsArch) = cmOK;
    for i := 1 to 10 do
      begin
        if StoI(ColumnsDefaultsArch.Params[i].LFNLen) > 252 then
          ColumnsDefaultsArch.Params[i].LFNLen := '252';
        if StoI(ColumnsDefaultsArch.Params[i].EXTLen) > 247 then
          ColumnsDefaultsArch.Params[i].EXTLen := '247';
      end;
  end;

function SetupColumnDefaultsArvd: boolean;
  var
    i: byte;
  begin
    {JO}
    with ColumnsDefaultsArvd do
        {JO: поскольку у панели арвида нет параметров }
      begin{    "показ даты и времени создания и последнего }
        for i := 1 to 10 do
            {    доступа", и нет соотв. чекбоксов, то надо пропустить соотв. байты}
          if (Params[i].Param and $100 <> 0) then
            Params[i].Param := (Params[i].Param and $F) or $10
          else
            Params[i].Param := (Params[i].Param and $F);
      end;
    {/JO}
    SetupColumnDefaultsArvd :=
    ExecResource(dlgSetupColumnsArvd, ColumnsDefaultsArvd) = cmOK;
    for i := 1 to 10 do
      begin
        if StoI(ColumnsDefaultsArvd.Params[i].LFNLen) > 252 then
          ColumnsDefaultsArvd.Params[i].LFNLen := '252';
        if StoI(ColumnsDefaultsArvd.Params[i].EXTLen) > 247 then
          ColumnsDefaultsArvd.Params[i].EXTLen := '247';
      end;
    {JO}
    with ColumnsDefaultsArvd do
      begin
        for i := 1 to 10 do
          if (Params[i].Param and $10 <> 0) then
            Params[i].Param := (Params[i].Param and $F) or $100
          else
            Params[i].Param := (Params[i].Param and $F);
      end;
    {/JO}
  end { SetupColumnDefaultsArvd: };

procedure TCurrDriveInfo.HandleEvent;
  var
    W: word;
    Data: TSysData;
  begin
    inherited HandleEvent(Event);
    if (Event.What = evBroadcast) and (Event.Command =
        cmScrollBarChanged)
    then
      begin
        W := PSysDialog(Owner)^.LocalData.Drives[Char(byte('A')+
          PScrollBar(Event.InfoPtr)^.Value)];
        SetData(W);
      end
    else if (Event.What = evKeyDown) and (Event.CharCode = ' ') and (
        TypeOf(Owner^.Current^) = TypeOf(TListBox))
    then
      Press(0);
  end;

procedure TCurrDriveInfo.Press;
  var
    Data: TSysData;
  begin
    inherited Press(Item);
    Owner^.GetData(Data);
    PSysDialog(Owner)^.LocalData.Drives[Char(byte('A')+Data.Drives.
      Focus)] := Value;
  end;

procedure TSysDialog.Awaken;
  var
    C: Char;
  begin
    LocalData := SystemData;
    New(SysData.Drives.List, Init(26, 1, False));
    for C := 'A'to'Z' do
      SysData.Drives.List^.Insert(NewStr(C+':'));
    Move(SystemData, SysData, SizeOf(SysData.Options)+SizeOf(SysData.
      Mode1)*2);
    SysData.Temp := SystemData.Temp;
    SysData.Drives.Focus := 2;
    SysData.Current := LocalData.Drives['C'];
    SysData.CopyLimitBuf := ItoS(SystemData.CopyLimitBuf);
    SysData.ForceDefArch := SystemData.ForceDefArch;
    SetData(SysData);
  end;

destructor TSysDialog.Done;
  var
    Data: TSysData;
  begin
    GetData(Data);
    inherited Done;
    Dispose(Data.Drives.List, Done);
  end;

procedure TSysDialog.GetData;
  var
    Data: TSysData;
  begin
    inherited GetData(Data);
    TSysData(Rec) := Data;
    LocalData.Options := Data.Options;
    LocalData.Mode1 := Data.Mode1;
    LocalData.Mode2 := Data.Mode2;
    LocalData.Temp := Data.Temp;
    LocalData.CopyLimitBuf := StoI(Data.CopyLimitBuf);
    LocalData.ForceDefArch := Data.ForceDefArch;
  end;

{----------------------------------------------------------------------------}
{                                 Mouse Setup                                }
{----------------------------------------------------------------------------}
Constructor TMouseBar.Init;
  begin
    inherited Init(Bounds);
    Options := Options or ofSelectable;
    SetParams(0, 0, Size.X-1, 3, 1);
  end;

const
  HSenseY = 3;

function TMouseBar.DataSize; assembler;
asm mov eax,4 end;

procedure TMouseBar.SetData;
  begin
    SetValue(integer(Rec))
  end;

procedure TMouseBar.GetData;
  begin
    integer(Rec) := Value
  end;

procedure TMouseBar.HandleEvent;
  begin
    inherited HandleEvent(Event);
  end;

{$IFDEF SS}
{----------------------------------------------------------------------------}
{                                Savers Setup                                }
{----------------------------------------------------------------------------}
procedure TSaversListBox.HandleEvent;
  var
    PS: PString;
    F: integer;
    A, s: PCollection;
    LocalData: TSaversData;
  function SeekStr(P: PString): boolean;
    begin
      SeekStr := (P <> nil) and (P^ = PS^);
    end;
  begin
    if Event.What = evBroadcast then
      case Event.Command of
        cmYes:
          begin
            Owner^.GetData(LocalData);
            A := LocalData.Available.List;
            if A^.Count > 0 then
              begin
                PS := A^.At(LocalData.Available.Focus);
                if (PS <> nil) and (List^.FirstThat(@SeekStr) = nil)
                then
                  begin
                    List^.Insert(NewStr(PS^));
                    s := List;
                    List := nil;
                    NewLisT(s);
                  end;
              end;
            ClearEvent(Event);
          end;
        cmNo:
          begin
            F := Focused;
            if F < List^.Count then
              begin
                s := List;
                s^.AtFree(F);
                List := nil;
                Owner^.Lock;
                NewLisT(s);
                if (F > 0) and (F >= List^.Count) then
                  Dec(F);
                FocusItem(F);
                Owner^.UnLock;
              end;
            ClearEvent(Event);
          end;
      end {case};
    inherited HandleEvent(Event);
  end { TSaversListBox.HandleEvent };

Constructor TSaversDialog.Init;
  var
    R: TRect;
    D: PDialog;
    Control, Labl, Histry: PView;
  begin
    R.Assign(0, 0, 57, 20);
    inherited Init(R, GetString(dlScreenSaverSetup));
    Options := Options or ofCentered or ofValidate;
    HelpCtx := hcSavers;
    R.Assign(19, 3, 20, 13);
    Control := New(PScrollBar, Init(R));
    Insert(Control);

    R.Assign(2, 3, 19, 13);
    Control := New(PSaversListBox, Init(R, 1, PScrollBar(Control)));
    Insert(Control);

    R.Assign(2, 2, 18, 3);
    Labl := New(PLabel, Init(R, GetString(dlSS_S_electedSavers),
      Control));
    Insert(Labl);

    R.Assign(20, 6, 36, 8);
    Control := New(PButton, Init(R, GetString(dlSS_A_dd), cmYes,
      bfNormal+bfBroadcast));
    Insert(Control);

    R.Assign(20, 8, 36, 10);
    Control := New(PButton, Init(R, GetString(dlSS_R_emove), cmNo,
      bfNormal+bfBroadcast));
    Insert(Control);

    R.Assign(54, 3, 55, 13);
    Control := New(PScrollBar, Init(R));
    Insert(Control);

    R.Assign(37, 3, 54, 13);
    Control := New(PListBox, Init(R, 1, PScrollBar(Control)));
    Insert(Control);

    R.Assign(37, 2, 54, 3);
    Labl := New(PLabel, Init(R, GetString(dlSSA_v_ailableSavers),
      Control));
    Insert(Labl);

    R.Assign(2, 15, 18, 16);
    Control := New(PInputline, Init(R, 3));
    PInputline(Control)^.SetValidator(New(PRangeValidator, Init(1,
      254))); { X-Man }
    Control^.Options := Control^.Options or ofValidate;
    Insert(Control);

    R.Assign(2, 14, 18, 15);
    Labl := New(PLabel, Init(R, GetString(dlSS_T_ime), Control));
    Insert(Labl);

    R.Assign(20, 15, 55, 16);
    Control := New(PCheckBoxes, Init(R,
    NewSItem(GetString(dlSSUse_M_ouse), nil)));
    Insert(Control);

    R.Assign(7, 17, 17, 19);
    Control := New(PButton, Init(R, GetString(dlOKButton), cmOK,
      bfDefault));
    Insert(Control);

    R.Assign(17, 17, 28, 19);
    Control := New(PButton, Init(R, GetString(dlCancelButton),
      cmCancel, bfNormal));
    Insert(Control);

    R.Assign(28, 17, 40, 19);
    Control := New(PButton, Init(R, GetString(dlHelpButton), cmHelp,
      bfNormal));
    Insert(Control);

    R.Assign(40, 17, 50, 19);
    Control := New(PButton, Init(R, GetString(dlTestButton), cmTest,
      bfNormal));
    Insert(Control);

    SelectNext(False);
  end { TSaversDialog.Init };

procedure TSaversDialog.HandleEvent(var Event: TEvent);
  var
    Data: TSaversData;
  begin
    inherited HandleEvent(Event);
    case Event.What of
      evCommand:
        case Event.Command of
          cmTest:
            begin
              ClearEvent(Event);
              GetData(Data);
              Application^.InsertAvIdlerN(Data, Data.Available.Focus);
            end;
        end {case};
    end {case};
  end;

{-DataCompBoy-}
procedure TSaversDialog.Awaken;
  var
    lSR: lSearchRec;
    Data: TSaversData;
  begin
    Data := SaversData;
    Data.Available.Focus := 0;
    Data.Selected.Focus := 0;
    New(Data.Available.List, Init(5, 5, False));
    if Data.Selected.List = nil
    then
      New(Data.Selected.List, Init(5, 5, False));
    with Data.Available.List^ do
      begin
        Insert(NewStr(#249' Star flight'));
        Insert(NewStr(#249' Flash-light'));
        Insert(NewStr(#249' Clock'));
        Insert(NewStr(#249' Blackness'));
        lFindFirst(SourceDir+'SSAVERS\*.SS', AnyFile, lSR);
        while DOSError = 0 do
          begin
            Insert(NewStr(lSR.FullName));
            lFindNext(lSR);
          end;
        lFindClose(lSR);
      end;
    SetData(Data);
  end { TSaversDialog.Awaken };
{-DataCompBoy-}

destructor TSaversDialog.Done;
  var
    Data: TSaversData;
  begin
    GetData(Data);
    inherited Done;
    if (Data.Available.List <> nil) then
      Dispose(Data.Available.List, Done);
  end;

function MakeSaversDialog: PDialog;
  begin
    MakeSaversDialog := New(PSaversDialog, Init);
  end;
{$ENDIF}

Constructor TUpperTable.Init;
  var
    i: integer;
  begin
    inherited Init(R);
    for i := 0 to 127 do
      Table[i] := Char(i+128);
    CurrentChar := 0;
    Delta := 0;
    Options := Options or ofSelectable;
    EventMask := evKeyDown or evMouse or evBroadcast;
      {PZ - 2000.03.26}
  end;

function TUpperTable.GetPalette;
  const
    s: String[Length(CInputLine)] = CInputLine;
  begin
    GetPalette := @S;
  end;

function TUpperTable.DataSize;
  begin
    DataSize := 128;
  end;
procedure TUpperTable.GetData;
  begin
    Move(Table, Rec, 128);
  end;
procedure TUpperTable.SetData;
  begin
    Move(Rec, Table, 128);
  end;

procedure TUpperTable.Draw;
  var
    B: TDrawBuffer;
    i: integer;
    C: word;
  begin
    if CurrentChar < Delta then
      Delta := CurrentChar;
    if CurrentChar-Delta > Size.X-1 then
      Delta := CurrentChar-Size.X+1;
    C := GetColor(1);
    MoveChar(B, ' ', C, Size.X);
    for i := 0 to Size.X-1 do
      WordRec(B[i]).Lo := i+Delta+128;
    WordRec(B[CurrentChar-Delta]).Hi := GetColor(3);
    WriteLine(0, 0, Size.X, 1, B);
    MoveChar(B, ' ', C, Size.X);
    for i := 0 to Size.X-1 do
      WordRec(B[i]).Lo := byte(Table[i+Delta]);
    WordRec(B[CurrentChar-Delta]).Hi := GetColor(3);
    WriteLine(0, 1, Size.X, 1, B);
    ShowCursor;
    SetCursor(CurrentChar-Delta, 1);
    NormalCursor;
  end { TUpperTable.Draw };

procedure TUpperTable.HandleEvent;
  procedure CE;
    begin
      ClearEvent(Event);
      DrawView;
    end;
  begin
    inherited HandleEvent(Event);
    case Event.What of
      evKeyDown:
        case Event.KeyCode of
          kbLeft:
            begin
              if CurrentChar > 0 then
                Dec(CurrentChar);
              CE
            end;
          kbRight:
            begin
              if CurrentChar < 127 then
                Inc(CurrentChar);
              CE
            end;
          else
            if Event.CharCode in ['A'..'Z', 'a'..'z', #128..#255]
            then
              begin
                Table[CurrentChar] := Event.CharCode;
                Message(@Self, evKeyDown, kbRight, nil);
                CE
              end;
        end {case};
    end {case};
  end { TUpperTable.HandleEvent };

end.
