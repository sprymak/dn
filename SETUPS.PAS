{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.6.RC1
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

UNIT Setups;
INTERFACE

USES
  Dos,
  LFN, {DataCompBoy}
  Objects,
  Drivers,
  Views,
  Dialogs,
  collect,
  DNApp,
  Commands,
  RStrings,
  Startup,
  Validate;

{                                System Setup
{----------------------------------------------------------------------------}
TYPE
  TSysData = record
    Options: Word;
    Mode1: String[5];
    Mode2: String[5];
    Temp: String;
    Drives: TTextListboxRec;
    Current: Word;
    LFNContainer : Word;  {Radiobuttons}
    LFNCache : String[3]; {Inputline}
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

  PMouseBar =^TMouseBar;
  TMouseBar = Object( TScrollBar )
    constructor Init( var Bounds: TRect );
    procedure SetData( var Rec ); virtual;
    procedure GetData( var Rec ); virtual;
    function DataSize: Word; virtual;
    procedure HandleEvent( var Event: TEvent ); virtual;
  end;

{$IFDEF SS}
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
{$ENDIF}

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

procedure SetupCountryInfo;
procedure FMDefaults;
procedure FMSetup;
procedure DriveInfoSetup;
procedure SetupEditorDefaults;
procedure SetupViewerDefaults;
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
FUNCTION MakeSaversDialog: PDialog;
{$ENDIF}
IMPLEMENTATION
uses Tree, Drives, advance, advance1, advance2, advance3, Messages, DNHelp,
     Advance6, DNIni;

procedure ConfirmSetup;
var D: Word;
begin
  Confirms := Confirms and $7F +
             (Confirms and cfFmtOs2Warning) shr 1 +
             (Confirms and cfExitConfirm)   shl 1;
  D:= ExecResource( dlgConfirmations, Confirms );
  Confirms := Confirms and $7F +
             (Confirms and cfFmtOs2Warning) shr 1 +
             (Confirms and cfExitConfirm)   shl 1;
  If D <> cmOK then Exit;
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
 Dispose(D,Done);
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
  DriveInfoType := InterfaceData.DrvInfType;
  SaveDnIniSettings ( @DriveInfoType );
  DoneIniEngine;
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
  if MouseVisible xor (MouseData.Options and omsCursor <> 0) then begin
    DoneEvents;
    InitEvents;
  end;
  MouseReverse := MouseData.Options and omsReverse <> 0;
  SetMouseSpeed(MouseData.HSense, MouseData.VSense);
  Message(Application, evCommand, cmUpdateConfig, nil);
end;

{$IFDEF SS}
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
{$ENDIF}

procedure SetupCountryInfo;
begin
  if ExecResource(dlgCountrySetup, CountryInfo) <> cmOK then Exit;
  Initupcase;
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
var W: Word;
begin
  Startup.DriveInfoData := Startup.DriveInfoData and $001F +
                           Startup.DriveInfoData and $07E0 shl 2 +
                           Startup.DriveInfoData and $1800 shr 6;

  if ExecResource(dlgDriveInfoSetup, Startup.DriveInfoData) <> cmOK then Exit;
  Startup.DriveInfoData := Startup.DriveInfoData and $001F +
                           Startup.DriveInfoData and $FF80 shr 2 +
                           Startup.DriveInfoData and $0060 shl 6;
  Message(Application, evCommand, cmUpdateConfig, nil);
  GlobalMessage(evCommand, cmReboundPanel, nil);
end;

procedure SetupEditorDefaults;
begin
  if ExecResource(dlgEditorDefaults, EditorDefaults) = cmOK then begin
    if StoI(EditorDefaults.TabSize)<2 then EditorDefaults.TabSize:='2';
    Message(Application, evCommand, cmUpdateConfig, nil);
  end;
end;

procedure SetupViewerDefaults;
begin
  if ExecResource(dlgViewerDefaults, ViewerDefaults) = cmOK then begin
    if StoI(ViewerDefaults.TabSize)<2 then ViewerDefaults.TabSize:='2';
    Message(Application, evCommand, cmUpdateConfig, nil);
  end;
end;


function SetupColumnDefaultsDisk: Boolean;
Begin
 SetupColumnDefaultsDisk :=
  ExecResource(dlgSetupColumnsDisk, ColumnsDefaultsDisk) = cmOK;
end;

function SetupColumnDefaultsFind: Boolean;
Begin
 SetupColumnDefaultsFind :=
  ExecResource(dlgSetupColumnsFind, ColumnsDefaultsFind) = cmOK;
end;

function SetupColumnDefaultsTemp: Boolean;
Begin
 SetupColumnDefaultsTemp :=
  ExecResource(dlgSetupColumnsTemp, ColumnsDefaultsTemp) = cmOK;
end;

function SetupColumnDefaultsArch: Boolean;
Begin
 SetupColumnDefaultsArch :=
  ExecResource(dlgSetupColumnsArch, ColumnsDefaultsArch) = cmOK;
end;

function SetupColumnDefaultsArvd: Boolean;
Begin
 SetupColumnDefaultsArvd :=
  ExecResource(dlgSetupColumnsArvd, ColumnsDefaultsArvd) = cmOK;
End;

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
  SysData.LFNContainer := SystemData.LFNContainer;
  SysData.LFNCache := SystemData.LFNCache;
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
  TSysData( Rec )        := Data;
  LocalData.Options      := Data.Options;
  LocalData.Mode1        := Data.Mode1;
  LocalData.Mode2        := Data.Mode2;
  LocalData.Temp         := Data.Temp;
  LocalData.LFNContainer := Data.LFNContainer;
  LocalData.LFNCache     := Data.LFNCache;
end;


{----------------------------------------------------------------------------}
{                                 Mouse Setup
{----------------------------------------------------------------------------}
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

{$IFDEF SS}
{                                Savers Setup
{----------------------------------------------------------------------------}
PROCEDURE TSaversListBox.HandleEvent;
var
  PS: PString;
  F: Integer;
  A, S: PCollection;
  LocalData: TSaversData;
  function SeekStr( P: PString ): boolean; {$IFDEF BIT_16}far;{$ENDIF}
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

  R.Assign(2,15,10,16);
  Control := New(PInputLine, Init(R, 3));
  PInputLine(Control)^.SetValidator(New(PRangeValidator, Init(1, 254))); { X-Man }
  Control^.Options := Control^.Options or ofValidate;
  Insert(Control);

  R.Assign(10,15,18,16);
  Labl := New(PLabel, Init(R, GetString(dlSSMin), Control));
  Insert(Labl);

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
end;

procedure TSaversDialog.HandleEvent( var Event: TEvent );
var
  Data: TSaversData;
begin
 inherited HandleEvent(Event);
 case Event.What of
  evCommand: case Event.Command of
              cmTest: begin
                       ClearEvent(Event);
                       GetData( Data );
                       Application^.InsertAvIdlerN(Data, Data.Available.Focus );
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
    Insert( NewStr( #249' Star flight' ));
    Insert( NewStr( #249' Flash-light' ));
    Insert( NewStr( #249' Clock' ));
    Insert( NewStr( #249' Blackness' ));
    lFindFirst( SourceDir + 'SSAVERS\*.SS', AnyFile, lSR );
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
{$ENDIF}

constructor TUpperTable.Init;
 var I: Integer;
begin
  inherited Init(R);
  for I := 0 to 127 do Table[I] := Char(I+128);
  CurrentChar := 0; Delta := 0;
  Options := Options or ofSelectable;
  EventMask := evKeyDown or evMouse or evBroadcast; {PZ - 2000.03.26}
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
    {$IFNDEF NONBP}
  {PZ - 2000.03.26 - begin}
  procedure LoadDosCountryInfo;
  var
    CountryInfo   : TCountryInfo;
    CountryInfoEx : TCountryInfoEx;
  begin
    CE;
    if GetCountryInfoEx ( CountryInfoEx ) = 0 then begin
      FixCountryInfoEx ( CountryInfoEx );
      CopyCountryInfo ( CountryInfoEx, CountryInfo );
      if Owner <> nil then begin
        Owner^.SetData ( CountryInfo );
        DrawView;
      end;
    end;
  end;
  {PZ - end}
    {$ENDIF}
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
    {$IFNDEF NONBP}
    {PZ - 2000.03.26 - begin}
    evBroadcast: case Event.Command of
                   cmYes: LoadDosCountryInfo;
                 end;
    {PZ - end}
    {$ENDIF}
  end;
end;

END.
