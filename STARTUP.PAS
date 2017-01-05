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

UNIT Startup;

INTERFACE

USES
  Objects,
  Dos,
  LFN, {DataCompBoy}
  Commands,
  advance,
  advance1,
  advance2,
  advance3,
  Drivers,
  Collect;

PROCEDURE FatalError( const S: String );

Const MaxSignLen = 30;
Type TSignRec = Record
                 Sign: String[MaxSignLen];
                 SignLen: Byte;
                 SignVer: AWord;
                 HavVer: boolean; {Is DN version word written after sign}
                End;

CONST
  FBRPBSignature: Array[1..5] of Char = 'FBRPB';
  NumSupportedConfigs = 9;
  ConfigSigns: Array[1..NumSupportedConfigs] of TSignRec =
   ((Sign:'DN2 Configuration'#26#001#36;SignLen:20;SignVer:00136),
                                                    {Base configuration}
    (Sign:'DN2 Configuration'#26#151#05;SignLen:20;SignVer:15104),
                                                    {Configuration for 1.51.04-0}
    (Sign:'DN2 Configuration'#26#151#42;SignLen:20;SignVer:15104),
                                                    {Configuration for 1.51.04-1}
    (Sign:'DN2 Configuration'#26#151#42;SignLen:20;SignVer:15104),
                                                    {Configuration for 1.51.04-2}
    (Sign:'DN OSP Configuration'#26#151#42;SignLen:23;SignVer:15104),
    (Sign:'DN OSP Configuration'#26#151#43;SignLen:23;SignVer:15104),
    (Sign:'DN OSP Configuration'#26#151#50;SignLen:23;SignVer:15105),
                                                    {Configuration for 1.51.05}
    (Sign:'DN OSP Configuration'#26#151#51;SignLen:23;SignVer:15105),
                                                    {Configuration for 1.51.05-2}
    (Sign:'DN OSP Configuration'#26;SignLen:21;SignVer:15106;HavVer:true));
                                                    {Configuration for 1.51.06+}
  DSKSign: TSignRec =
{$IFDEF LITE}(Sign:'Lite DN OSP Desktop'#26#151#80;SignLen:22;SignVer:15180);
{$ELSE}      (Sign:'DN OSP Desktop'#26#151#80;SignLen:17;SignVer:15180);{$ENDIF}
                                                    {Desktop for 1.51.08}
  Security: Boolean = True;
  ConfigModified: Boolean = Off;
  DateMode: Byte = 0;
  SkyDelay: Byte = 1;


TYPE
  PTextCollection =^TTextCollection;
  TTextCollection = Object( TCollection )
    procedure FreeItem( Item: Pointer ); virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    function GetItem(var S: TStream): Pointer; virtual;
  end;

  TListBoxRec = record List: PCollection; Focus: Integer end;
  TTextListBoxRec = record List: PTextCollection; Focus: Integer end;

  TSystemData = record
    Options      : Word;
    Mode1        : String[5];
    Mode2        : String[5];
    Temp         : String;
    LFNContainer : Word;
    LFNCache     : String[3];
    Drives: Array [ 'A'..'Z' ] of Byte;
  end;

  TOld2SystemData = record
    Options: AWord;
    LFNContainer: AWord;
    Mode1: String[5];
    Mode2: String[5];
    Temp: String; {DataCompBoy}
    Drives: Array [ 'A'..'Z' ] of Byte;
  end;

  TOldSystemData = record
    Options: AWord;
    Mode1: String[5];
    Mode2: String[5];
    Temp: String; {DataCompBoy}
    Drives: Array [ 'A'..'Z' ] of Byte;
  end;

  TOld1SystemData = record
    Options: AWord;
    Mode1: String[5];
    Mode2: String[5];
    Temp: PathStr;
    Drives: Array [ 'A'..'Z' ] of Byte;
  end;

  TMouseData = record
    HSense: Word;
    VSense: Word;
    Options: Word;
  end;

{$IFDEF SS}
  TSaversData = record
    Selected: TTextListBoxRec;
    Available: TTextListBoxRec;
    Time: String[3]; {DataCompBoy}
    Mouse: Boolean;
    _: Byte;
  end;

  TOldSaversData = record
    Selected: TTextListBoxRec;
    Available: TTextListBoxRec;
    Time: AWord;
    Mouse: Boolean;
    _: Byte;
  end;
{$ENDIF}

  TInterfaceData = record
    Options: Word;
    DrvInfType: Word;
  end;

  TPanelDefaultsData = record
    Sort: Word;
    Show: Word;
    LeftPanel: Word;
  end;

  TFMSetup = record
    Options: Word;
    Show: Word;
    Quick: Word;
    TagChar:  String[1];
    RestChar: String[1];
    DIZ: String;
  end;

  TOldFMSetup = record
    Options: AWord;
    Show: AWord;
    Quick: AWord;
    TagChar:  String[1];
    DIZ: String;
  end;

  TEditorDefaultsData = record
    EdOpt: Word;
    EdOpt2:Word;
    ViOpt: Word;
    LM,
    RM,
    PM: String[3];
    NewLine: Word;
    TabSize: String[3];
  end;

  TOldEditorDefaultsData = record
    EdOpt: AWord;
    ViOpt: AWord;
    LM,
    RM,
    PM: String[3];
    NewLine: AWord;
    TabSize: String[3];
  end;

  TTerminalDefaults = record
    Emulation: Word;
    Options: Word;
  end;

  TOldStartupData = record
    Load, Unload, Slice, OvrSize: AWord;
  end;

{$IFNDEF VIRTUALPASCAL}
  TStartupData = record
    Load, Unload, Slice, Slice2, OvrSize: Word;
  end;
{$ELSE}
  TStartupData = record
    Load, Unload, Slice2: Word;
  end;
{$ENDIF}

  TUUEncodeData = record
    Name: String;
    Prefix, Checksum: Word;
    NLines: String[4];
    Format : Word ;
  end;

  TNamesCaseOptions = record
    Name: Word;
     Ext: Word;
  end;

var TempBounds: TRect;


CONST
  ChangeNamesCaseOptions: TNamesCaseOptions = (Name: 0; Ext: 0); {JO} {было Longint и делилось на два Word,}
                                                                 {но это не годится для 32-битной платформы}
  UUDecodeOptions: AWord = 3;
  MakeListFileOptions: AWord = 0;

  UUEncodeData : TUUEncodeData =
    ( Name:'';
    Prefix: ckFileTime+ckStatistic;
    Checksum: ckStd;
    NLines: '100';
    Format: 0 );

  DriveInfoData: AWord = $FFFF;


{$IFNDEF VIRTUALPASCAL}
  StartupData: TStartupData =
    ( Load: osuRestoreScrMode + osuOverlayXMS + osuOverlayEMS;
      Unload: 0;
      Slice: osuInt28+osuInt2F+osuInt15;
      Slice2: osuOnceRelease;
      OvrSize: 95
    );
{$ELSE}
  StartupData: TStartupData =
    ( Load: osuRestoreScrMode;
      Unload: 0;
      Slice2: osuOnceRelease
    );
{$ENDIF}

  Confirms: Word = cfSingleErase + cfMultiErase + cfEraseReadonly +
                    cfEraseSubdir + cfMouseConfirm + cfExitConfirm + cfChaneLngWarning;

  TerminalDefaults: TTerminalDefaults = (Emulation:emANSIBBS;Options:toCtrlCLS+toAutoZModem);

{-DataCompBoy-}{piwamoto}{JO}
{ note: BIN, DAT, RAW are NOT included in any group, }
{ becouse there is TOO MUCH filetypes with these extensions }

  Archives:    PMaskData = nil;
{sources}
  CustomMask1: PMaskData = nil;
{raw text}
  CustomMask2: PMaskData = nil;
{databases & speadsheets}
  CustomMask3: PMaskData = nil;
{kill'em!}
  CustomMask4: PMaskData = nil;
{graphics}
  CustomMask5: PMaskData = nil;
{config}
  CustomMask6: PMaskData = nil;
{audio & video}
  CustomMask7: PMaskData = nil;
{fonts}
  CustomMask8: PMaskData = nil;
{Office documents}
  CustomMask9: PMaskData = nil;
{system}
  CustomMask10:PMaskData = nil;
        {-DataCompBoy-}
procedure InitMaskData;

 const
  InterfaceData: TInterfaceData = (
    Options: ouiClock +
             ouiEsc
          {+ ouiStoreEditorPosition +
             ouiStoreViewerPosition +
             ouiTrackEditors +
             ouiTrackViewers +
             ouiTrackDirs +
             ouiHideMenu};
    DrvInfType: 0
  );

  PanelDefaults: TPanelDefaultsData = (
    Sort:  psmLongExt;
    Show:  fmiCurrent +
          {fmiSelected +}
           fmiHiliteFiles +
           fmiFree +
           fmiExeFirst +
           fmiArchivesFirst;
    LeftPanel: fdoDriveDrive
  );

  FMSetup: TFMSetup =
    ( Options: fmoDragAndDrop +
               fmoBeep +
               fmoEnterArchives +
               fmoSpaceToggle +
               fmoUseArrows +
               fmoBackGoesBack +
               fmoPreserveDesc +
               fmoAlwaysCopyDesc;
      Show: fmsColumnTitles +
            fmsDriveLine +
            fmsTagChar +
            fmsDivider;
      Quick: pqsAlt;
      TagChar : #251;
      RestChar: #16;
      DIZ: 'descript.ion;files.bbs'
    );

  EditorDefaults: TEditorDefaultsData = (
    EdOpt: ebfBSU +
          {$IFDEF OS_DOS}
           ebfEMS +
           ebfXMS +
          {$ENDIF}
           ebfTRp{$IFNDEF OS_DOS} shr 2 {$ENDIF}; {-$VOL}
    EdOpt2:ebfHlt+ebfSmt;
    ViOpt: vbfScrollAfterEOF;
    LM: '0';
    RM: '78';
    PM: '5';
    NewLine: 0;
    TabSize: '8'
  );

  MouseData: TMouseData = (
    HSense: 22;
    VSense: 22;
    Options: omsCursor
  );

{$IFDEF SS}
  SaversData: TSaversData = (
    Selected: ( List: NIL; Focus: 0 );
    Available: ( List: NIL; Focus: 0 );
    Time: '5';
    Mouse: True
  );
{$ENDIF}

  SystemData: TSystemData = (
    Options: ossEditor +
             ossUseSysClip +
{$IFDEF OS_DOS}
             ossCheckDDA +
{$ENDIF}
             ossRemoveCD_RO{$IFDEF VIRTUALPASCAL}shr 3{$ENDIF};
    Mode1: '147';
{$IFNDEF VIRTUALPASCAL}
    Mode2: '265';
{$ELSE}
    Mode2: '148';
{$ENDIF}
    Temp: '';
    LFNContainer: 0;
    LFNCache: '3';
    Drives: (
      {A}(ossCheckFreeSpace{$IFNDEF OS_DOS}shr 1{$ENDIF})+(ossVerify{$IFNDEF OS_DOS}shr 1{$ENDIF}),
      {B}(ossCheckFreeSpace{$IFNDEF OS_DOS}shr 1{$ENDIF})+(ossVerify{$IFNDEF OS_DOS}shr 1{$ENDIF}),
      {C}0, {D}0, {E}0, {F}0, {G}0, {H}0, {I}0, {J}0, {K}0, {L}0, {M}0, {N}0,
      {O}0, {P}0, {Q}0, {R}0, {S}0, {T}0, {U}0, {V}0, {W}0, {X}0, {Y}0, {Z}0
    )
  );

  ColumnsDefaultsDisk: record
   Params: array [1..10] of
            record Param: Word; LFNLen: string[3]; EXTLen: string[3]; end;
   FileCase: Word;
   DirsCase: Word;
  end = (
{$IFDEF OS2}
    Params: ( (Param:  1; LFNLen: '12'; EXTLen: '3'),(Param:1023; LFNLen: '12'; EXTLen: '3'),
              (Param:  1; LFNLen: '18'; EXTLen: '3'),(Param:   1; LFNLen: '18'; EXTLen: '0'),
              (Param:  1; LFNLen: '38'; EXTLen: '4'),(Param:   1; LFNLen:'253'; EXTLen: '0'),
              (Param:257; LFNLen: '12'; EXTLen: '3'),(Param:   1; LFNLen: '12'; EXTLen: '3'),
              (Param:  1; LFNLen: '12'; EXTLen: '3'),(Param:   1; LFNLen: '12'; EXTLen: '3'));
{$ELSE}
    Params: ( (Param:  1; LFNLen: '12'; EXTLen: '3'),(Param:1023; LFNLen: '13'; EXTLen: '3'),
              (Param:  0; LFNLen: '12'; EXTLen: '3'),(Param:   1; LFNLen: '18'; EXTLen: '3'),
              (Param:  1; LFNLen: '18'; EXTLen: '0'),(Param:   1; LFNLen: '38'; EXTLen: '4'),
              (Param:  1; LFNLen:'253'; EXTLen: '0'),(Param: 257; LFNLen: '12'; EXTLen: '3'),
              (Param:  1; LFNLen: '12'; EXTLen: '3'),(Param:   1; LFNLen: '12'; EXTLen: '3'));
{$ENDIF}
    FileCase:4;
    DirsCase:4
  );

  ColumnsDefaultsFind: record
   Params: array [1..10] of
            record Param: Word; LFNLen: string[3]; EXTLen: string[3]; end;
   FileCase: Word;
   DirsCase: Word;
  end = (
{$IFDEF OS2}
    Params: ( (Param:257; LFNLen: '12'; EXTLen: '3'),(Param:511; LFNLen: '12'; EXTLen: '3'),
              (Param:257; LFNLen: '18'; EXTLen: '0'),(Param:  1; LFNLen: '18'; EXTLen: '3'),
              (Param:  1; LFNLen: '18'; EXTLen: '0'),(Param:  1; LFNLen: '38'; EXTLen: '4'),
              (Param:  1; LFNLen:'253'; EXTLen: '0'),(Param:  1; LFNLen: '12'; EXTLen: '3'),
              (Param:  1; LFNLen: '12'; EXTLen: '3'),(Param:  1; LFNLen: '12'; EXTLen: '3'));
{$ELSE}
    Params: ( (Param:257; LFNLen: '12'; EXTLen: '3'),(Param:511; LFNLen: '12'; EXTLen: '3'),
              (Param:  0; LFNLen: '12'; EXTLen: '3'),(Param:257; LFNLen: '18'; EXTLen: '0'),
              (Param:  1; LFNLen: '18'; EXTLen: '3'),(Param:  1; LFNLen: '18'; EXTLen: '0'),
              (Param:  1; LFNLen: '38'; EXTLen: '4'),(Param:  1; LFNLen:'253'; EXTLen: '0'),
              (Param:  1; LFNLen: '12'; EXTLen: '3'),(Param:  1; LFNLen: '12'; EXTLen: '3'));
{$ENDIF}
    FileCase:4;
    DirsCase:4
  );
  ColumnsFindLast: byte = 1;

  ColumnsDefaultsTemp: record
   Params: array [1..10] of
            record Param: Word; LFNLen: string[3]; EXTLen: string[3]; end;
   FileCase: Word;
   DirsCase: Word;
  end = (
{$IFDEF OS2}
    Params: ( (Param:257; LFNLen: '12'; EXTLen: '3'),(Param:511; LFNLen: '12'; EXTLen: '3'),
              (Param:257; LFNLen: '18'; EXTLen: '0'),(Param:  1; LFNLen: '18'; EXTLen: '3'),
              (Param:  1; LFNLen: '18'; EXTLen: '0'),(Param:  1; LFNLen: '38'; EXTLen: '4'),
              (Param:  1; LFNLen:'253'; EXTLen: '0'),(Param:  1; LFNLen: '12'; EXTLen: '3'),
              (Param:  1; LFNLen: '12'; EXTLen: '3'),(Param:  1; LFNLen: '12'; EXTLen: '3'));
{$ELSE}
    Params: ( (Param:257; LFNLen: '12'; EXTLen: '3'),(Param:511; LFNLen: '12'; EXTLen: '3'),
              (Param:  0; LFNLen: '12'; EXTLen: '3'),(Param:257; LFNLen: '18'; EXTLen: '0'),
              (Param:  1; LFNLen: '18'; EXTLen: '3'),(Param:  1; LFNLen: '18'; EXTLen: '0'),
              (Param:  1; LFNLen: '38'; EXTLen: '4'),(Param:  1; LFNLen:'253'; EXTLen: '0'),
              (Param:  1; LFNLen: '12'; EXTLen: '3'),(Param:  1; LFNLen: '12'; EXTLen: '3'));
{$ENDIF}
    FileCase:4;
    DirsCase:4
  );
  ColumnsTempLast: byte = 1;

  ColumnsDefaultsArch: record
   Params: array [1..10] of
            record Param: Word; LFNLen: string[3]; EXTLen: string[3]; end;
   FileCase: Word;
   DirsCase: Word;
  end = (
{$IFDEF OS2}
    Params: ( (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param:63; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '18'; EXTLen: '3'),(Param: 1; LFNLen: '18'; EXTLen: '0'),
              (Param: 1; LFNLen: '38'; EXTLen: '4'),(Param: 1; LFNLen:'253'; EXTLen: '0'),
              (Param:51; LFNLen: '12'; EXTLen: '3'),(Param:33; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'));
{$ELSE}
    Params: ( (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param:63; LFNLen: '13'; EXTLen: '3'),
              (Param: 0; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '18'; EXTLen: '3'),
              (Param: 1; LFNLen: '18'; EXTLen: '0'),(Param: 1; LFNLen: '38'; EXTLen: '4'),
              (Param: 1; LFNLen:'253'; EXTLen: '0'),(Param:51; LFNLen: '12'; EXTLen: '3'),
              (Param:33; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'));
{$ENDIF}
    FileCase:4;
    DirsCase:4
  );

  ColumnsDefaultsArvd: record
   Params: array [1..10] of
            record Param: Word; LFNLen: string[3]; EXTLen: string[3]; end;
   FileCase: Word;
   DirsCase: Word;
  end = (
    Params: ( (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param:63; LFNLen: '18'; EXTLen: '3'),
              (Param: 1; LFNLen: '18'; EXTLen: '4'),(Param: 1; LFNLen: '18'; EXTLen: '0'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'));
    FileCase:4;
    DirsCase:4
  );

  OldColumnsDefaults: record
    DiskDrive,
    FindDrive,
    Temp,
    Arc,
    Arvid: aword
  end = (
    DiskDrive:0;
    FindDrive:0;
    Temp:0;
    Arc: 0;
    Arvid: 0
  );


  {$IFDEF PRINTMANAGER}
  RPrinterSetup: record Device: aword; InitPrinter, AfterFile: String; end =
    (Device: 0; InitPrinter:''; AfterFile:#12);
  {$ENDIF}

  TetrisRec : record
    L : Word; S : Word; P: Word;
  end = (L:4; S:0; P:0);

  rfDescriptions = $0002;
  rfPhoneBook    = $0004;
  rfDiskCopy     = $0008;
  rfFormat       = $0010;
  rfArchives     = $0020;
  rfAdvFind      = $0040;

IMPLEMENTATION
{$IFDEF DNPRG}
Uses ExtraMem

{$IFNDEF NO_OVERLAY}
     ,Overlay1
     ,Overlays
{$ENDIF NO_OVERLAY}
     ;

{$ENDIF}

PROCEDURE TTextCollection.FreeItem;
begin
  DisposeStr( PString(Item) );
end;

PROCEDURE TTextCollection.PutItem;
begin
  S.WriteStr(Item);
end;

FUNCTION TTextCollection.GetItem;
begin
  GetItem := S.ReadStr
end;

PROCEDURE FatalError;
begin
  InOutRes := 0;
  WriteLn( S );
  Halt( 1 );
end;

{$IFNDEF NO_OVERLAY}
{$IFDEF DNPRG }
  procedure SetOverlay;
  var S: String;
  begin
    InitExtraMem;
    S := Dos.GetEnv('DNOVR');
    if S = '' then S := StartupDir + 'DN.OVR'
              else S := MakeNormName(S, 'DN.OVR');
    OvrFileMode := $40;
    InitOverlay(S);
  end;
{$ENDIF}
{$ENDIF NO_OVERLAY}

procedure InitMaskData;
begin
 New(Archives); FillChar(Archives^, SizeOf(Archives^), 0);
 Archives^.Filter    :=
{archive volumes for ace,ain,arj,rar,zxzip}
                        'a[0-9][0-9];r[0-9][0-9];$z[0-9];'+
{supported archives in order of appearance}
                        'ace;ain;arc;pak;arj;bs2;bsa;cab;chz;ha;hap;hpk;hyp;z;'+
                        'lha;ice;lzh;lim;qrk;rar;sqz;tar;taz;tgz;gz;uc2;ufa;'+
                        'zip;zoo;$z;zxz;'+
{arvid}
                        'avt;tdr;'+
{unsupported archives}
                        'j;777;acb;boa;imp';

{sources}
 New(CustomMask1);  FillChar(CustomMask1^, SizeOf(CustomMask1^), 0);
 CustomMask1^.Filter := 'pas;asm;inc;c;cpp;hdr;def;pp;h;rc;idb';

{raw text}
 New(CustomMask2);  FillChar(CustomMask2^, SizeOf(CustomMask2^), 0);
 CustomMask2^.Filter := '.;asc;dok;txt;ans;diz;dif;inf;info;patch;pat;nfo;uue;uu[1-9];u[0-9][1-9]';

{databases & speadsheets}
 New(CustomMask3);  FillChar(CustomMask3^, SizeOf(CustomMask3^), 0);
 CustomMask3^.Filter := 'dbf;wkz;wks;123;wk1;xls;mdb;mde';

{kill'em!}
 New(CustomMask4);  FillChar(CustomMask4^, SizeOf(CustomMask4^), 0);
 CustomMask4^.Filter := 'tmp;$$$;bak;qab;swp;log;lut;c2t;err;old';

{graphics}
 New(CustomMask5);  FillChar(CustomMask5^, SizeOf(CustomMask5^), 0);
 CustomMask5^.Filter := 'pcx;bmp;gif;rle;ico;jpg;jpe;jpeg;png;tif;tga;wpg;psd;'+
                       'pcd';

{config}
 New(CustomMask6);  FillChar(CustomMask6^, SizeOf(CustomMask6^), 0);
 CustomMask6^.Filter := 'ini;cfg;ctl;nif;lst;m3u;pls;reg';

{audio & video}
 New(CustomMask7);  FillChar(CustomMask7^, SizeOf(CustomMask7^), 0);
 CustomMask7^.Filter :=
  {audio streams}
                        'wav;voc;ym;aud;snd;wma;'+
  {compressed audio streams}
                        'mp[1-3g];vqf;vtx;asf;as4;'+
  {sampled music}
                        'mod;stm;s3m;xm;it;mdl;mtm;mid;cmf;sid;'+
  {video}
                        'avi;mpeg;mpe;smk;bik;gl;mvy;mve;fli;flc;tgv';

{fonts}
 New(CustomMask8);  FillChar(CustomMask8^, SizeOf(CustomMask8^), 0);
 CustomMask8^.Filter := 'fon;fot;fnt;ttf;ofm;pfm;afm;sft;eft;pft;xft';

{Office documents}
 New(CustomMask9);  FillChar(CustomMask9^, SizeOf(CustomMask9^), 0);
 CustomMask9^.Filter := 'doc;rtf;sdw;lwp;sam;html;htm';

{system}
 New(CustomMask10); FillChar(CustomMask10^,SizeOf(CustomMask10^),0);
 CustomMask10^.Filter:= 'dll;drv;ifs;flt;add;sys;vxd;386;rtl;ovr;ovl';
end;

begin
  StartupDir:='';
  SourceDir:='';
  TempDir:='';
  TempFile:='';

  SourceDir := lFExpand(ParamStr(0));
  while SourceDir[Length(SourceDir)] <> '\' do SetLength(SourceDir, Length(SourceDir)-1);
  StartupDir := SourceDir;
  {SourceDir := Dos.GetEnv('DN')}
{$IFNDEF VIRTUALPASCAL}
{ VK/ }
  if ExistDir(Dos.GetEnv('DN')) or (Dos.GetEnv('DN')='') then
      SourceDir := Dos.GetEnv('DN')
     else
      Writeln('Warning! Path specified in DN environment variable does not exist!');
{ /VK }
{$ELSE}
  if ExistDir(Dos.GetEnv('DNVP')) or (Dos.GetEnv('DNVP')='') then
      SourceDir := Dos.GetEnv('DNVP')
     else
      Writeln('Warning! Path specified in DNVP environment variable does not exist!');
{$ENDIF}

  for FreeByte:=1 to length(SourceDir) do
   if SourceDir[FreeByte]<>' ' then break;
  if SourceDir[FreeByte]<>' ' then SourceDir:=Copy(SourceDir, FreeByte, 255);
  if SourceDir = '' then SourceDir := StartupDir;
  if SourceDir[Length(SourceDir)] <> '\' then AddStr(SourceDir, '\');

{$IFDEF OS_DOS}
  StartupDir:=lfGetLongFileName(StartupDir);
  SourceDir:= lfGetLongFileName(SourceDir);
  TempDir:=   lfGetLongFileName(TempDir);
  TempFile:=  lfGetLongFileName(TempFile);
{$ENDIF}

{$IFNDEF NO_OVERLAY}
{$IFDEF DNPRG}
 SetOverlay;
{$ENDIF}
{$ENDIF NO_OVERLAY}
END.
