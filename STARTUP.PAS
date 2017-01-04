{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.07/DOS
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
                 SignVer: Word;
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
{$IFDEF LITE}(Sign:'Lite DN OSP Desktop'#26#151#70;SignLen:22;SignVer:15107);
{$ELSE}      (Sign:'DN OSP Desktop'#26#151#70;SignLen:17;SignVer:15107);{$ENDIF}
                                                    {Desktop for 1.51.07}
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
    Options: Word;
    LFNContainer: Word;
    Mode1: String[5];
    Mode2: String[5];
    Temp: String; {DataCompBoy}
    Drives: Array [ 'A'..'Z' ] of Byte;
  end;

  TOldSystemData = record
    Options: Word;
    Mode1: String[5];
    Mode2: String[5];
    Temp: String; {DataCompBoy}
    Drives: Array [ 'A'..'Z' ] of Byte;
  end;

  TOld1SystemData = record
    Options: Word;
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
    Time: Word;
    Mouse: Boolean;
    _: Byte;
  end;
{$ENDIF}

  TInterfaceData = record
    Options: Word;
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
    Options: Word;
    Show: Word;
    Quick: Word;
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
    EdOpt: Word;
    ViOpt: Word;
    LM,
    RM,
    PM: String[3];
    NewLine: Word;
    TabSize: String[3];
  end;

  TTerminalDefaults = record
    Emulation: Word;
    Options: Word;
  end;

  TOldStartupData = record
    Load, Unload, Slice, OvrSize: Word;
  end;

  TStartupData = record
    Load, Unload, Slice, Slice2, OvrSize: Word;
  end;

  TUUEncodeData = record
    Name: String;
    Prefix, Checksum: Word;
    NLines: String[4];
    Format : Word ;
  end;


var TempBounds: TRect;


CONST
  ChangeNamesCaseOptions: LongInt = 0;
  UUDecodeOptions: Word = 3;
  MakeListFileOptions: Word = 0;

  UUEncodeData : TUUEncodeData =
    ( Name:'';
    Prefix: ckFileTime+ckStatistic;
    Checksum: ckStd;
    NLines: '100';
    Format: 0 );

  DriveInfoData: Word = $FFFF;

  StartupData: TStartupData =
    ( Load: osuRestoreScrMode + osuOverlayXMS + osuOverlayEMS;
      Unload: 0;
      Slice: osuInt28+osuInt2F+osuInt15;
      Slice2: osuOnceRelease;
      OvrSize: 95
    );

  Confirms: Word = cfSingleErase + cfMultiErase + cfEraseReadonly +
                   cfEraseSubdir + cfExitConfirm + cfFmtOs2Warning;

  TerminalDefaults: TTerminalDefaults = (Emulation:emANSIBBS;Options:toCtrlCLS+toAutoZModem);

{-DataCompBoy-}{piwamoto}{JO}
{ note: BIN, DAT, RAW are NOT included in any group, }
{ becouse there is TOO MUCH filetypes with these extensions }

  Archives:string =
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
  CustomMask1: String = 'pas;asm;inc;c;cpp;hdr;def;pp;h;rc;idb';
{raw text}
  CustomMask2: String = '.;asc;dok;txt;ans;diz;dif;inf;info;patch';
{databases & speadsheets}
  CustomMask3: String = 'dbf;wkz;wks;123;wk1;xls;mdb;mde';
{kill'em!}
  CustomMask4: String = 'tmp;$$$;bak;qab;swp;log;lut;c2t;err;old';
{graphics}
  CustomMask5: String = 'pcx;bmp;gif;rle;ico;jpg;jpe;jpeg;png;tif;tga;wpg;psd;pcd';
{config}
  CustomMask6: String = 'ini;cfg;ctl;nif;lst;m3u;pls';
{audio & video}
  CustomMask7: String =
  {audio streams}
                        'wav;voc;ym;aud;snd;'+
  {compressed audio streams}
                        'mp[1-3g];vqf;vtx;asf;as4;'+
  {sampled music}
                        'mod;stm;s3m;xm;it;mdl;mtm;mid;cmf;sid;'+
  {video}
                        'avi;mpeg;mpe;smk;bik;gl;mvy;mve;fli;flc;tgv';
{fonts}
  CustomMask8: String = 'fon;fot;fnt;ttf;ofm;pfm;afm;sft;eft;pft;xft';
{Office documents}
  CustomMask9: String = 'doc;rtf;sdw;lwp;sam;html;htm';
{system}
  CustomMask10: String= 'dll;drv;ifs;flt;add;sys;vxd;386;rtl;ovr;ovl';
        {-DataCompBoy-}

  InterfaceData: TInterfaceData = (
    Options: ouiClock +
             ouiStoreEditorPosition +
             ouiStoreViewerPosition +
             ouiTrackEditors +
             ouiTrackViewers +
             ouiTrackDirs +
             ouiHideMenu
  );

  PanelDefaults: TPanelDefaultsData = (
    Sort:  psmLongExt;
    Show:  fmiCurrent +
           fmiSelected +
           fmiHiliteFiles +
           fmiFree +
           fmiExeFirst +
           fmiArchivesFirst;
    LeftPanel: fdoDriveDrive
  );

  FMSetup: TFMSetup =
    ( Options: fmoAutoChangeDir +
               fmoEnterArchives +
               fmoSpaceToggle +
               fmoDelErase +
               fmoUseArrows;
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
           ebfAId +
           ebfPBl +
           ebfEMS +
           ebfXMS +
           ebfTRp; {-$VOL}
    EdOpt2:ebfHlt+ebfSmt;
    ViOpt: 0;
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
    Time: '1';
    Mouse: False
  );
{$ENDIF}

  SystemData: TSystemData = (
    Options: ossEditor + ossViewer + ossAdvCopy + ossFlushDsk;
    Mode1: '256';
    Mode2: '265';
    Temp: '';
    LFNContainer: 0;
    LFNCache: '3';
    Drives: (
      {A}ossCheckFreeSpace+ossVerify,     {B}ossCheckFreeSpace+ossVerify,
      {C}0, {D}0, {E}0, {F}0, {G}0, {H}0, {I}0, {J}0, {K}0, {L}0, {M}0, {N}0,
      {O}0, {P}0, {Q}0, {R}0, {S}0, {T}0, {U}0, {V}0, {W}0, {X}0, {Y}0, {Z}0
    )
  );

  ColumnsDefaultsDisk: record
   Params: array [1..10] of
            record Param: word; LFNLen: string[2]; EXTLen: string[2]; end;
   FileCase: word;
   DirsCase: word;
  end = (
    Params: ( (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param:63; LFNLen: '18'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'));
    FileCase:1;
    DirsCase:0
  );

  ColumnsDefaultsFind: record
   Params: array [1..10] of
            record Param: word; LFNLen: string[2]; EXTLen: string[2]; end;
   FileCase: word;
   DirsCase: word;
  end = (
    Params: ( (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param:31; LFNLen: '18'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'));
    FileCase:1;
    DirsCase:0
  );
  ColumnsFindLast: byte = 1;

  ColumnsDefaultsTemp: record
   Params: array [1..10] of
            record Param: word; LFNLen: string[2]; EXTLen: string[2]; end;
   FileCase: word;
   DirsCase: word;
  end = (
    Params: ( (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param:31; LFNLen: '18'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'));
    FileCase:1;
    DirsCase:0
  );
  ColumnsTempLast: byte = 1;

  ColumnsDefaultsArch: record
   Params: array [1..10] of
            record Param: word; LFNLen: string[2]; EXTLen: string[2]; end;
   FileCase: word;
   DirsCase: word;
  end = (
    Params: ( (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param:63; LFNLen: '18'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'));
    FileCase:1;
    DirsCase:0
  );

  ColumnsDefaultsArvd: record
   Params: array [1..10] of
            record Param: word; LFNLen: string[2]; EXTLen: string[2]; end;
   FileCase: word;
   DirsCase: word;
  end = (
    Params: ( (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param:63; LFNLen: '18'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'),
              (Param: 1; LFNLen: '12'; EXTLen: '3'),(Param: 1; LFNLen: '12'; EXTLen: '3'));
    FileCase:1;
    DirsCase:0
  );

  OldColumnsDefaults: record
    DiskDrive,
    FindDrive,
    Temp,
    Arc,
    Arvid: Word
  end = (
    DiskDrive:0;
    FindDrive:0;
    Temp:0;
    Arc: 0;
    Arvid: 0
  );


  {$IFDEF PRINTMANAGER}
  RPrinterSetup: record Device: Word; InitPrinter, AfterFile: String; end =
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
Uses extramemory

{$IFNDEF DPMI}

{$IFDEF PackOverlay}
     ,DnOverl {DataCompBoy}
{$ELSE}
     ,Overlay
{$ENDIF}
     ,Overlays

{$ENDIF}{DPMI}
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

{$IFNDEF DPMI}
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
{$ENDIF}{DPMI}

begin
  StartupDir:='';
  SourceDir:='';
  TempDir:='';
  TempFile:='';

  SourceDir := lFExpand(ParamStr(0));
  while SourceDir[Length(SourceDir)] <> '\' do Dec(SourceDir[0]);
  StartupDir := SourceDir;
  SourceDir :=  Dos.GetEnv('DN');
  for FreeByte:=1 to length(SourceDir) do
   if SourceDir[FreeByte]<>' ' then break;
  if SourceDir[FreeByte]<>' ' then SourceDir:=Copy(SourceDir, FreeByte, 255);
  if SourceDir = '' then SourceDir := StartupDir;
  if SourceDir[Length(SourceDir)] <> '\' then AddStr(SourceDir, '\');

  StartupDir:=lfGetLongFileName(StartupDir);
  SourceDir:= lfGetLongFileName(SourceDir);
  TempDir:=   lfGetLongFileName(TempDir);
  TempFile:=  lfGetLongFileName(TempFile);

{$IFNDEF DPMI}
{$IFDEF DNPRG}
 SetOverlay;
{$ENDIF}
{$ENDIF}{DPMI}
END.
