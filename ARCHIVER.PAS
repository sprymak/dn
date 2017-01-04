{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//  Version history:
//
//  1.6.RC1
//  dn16rc1-Archives_without_extension_fix_diff131byMV.patch
//  dn16rc1-improved_HA_AIN_ZXZip_and_SFX_detection_diff138byMV.patch
//  dn16rc1-Archivers_Optimization-diff154byMV.patch
//
//  2.0.0
//  dn200-RAR_3_0_SFX_detection.patch
//  dn223-Archivers_Optimization.patch
//  dn200-NE_SFX_detection.patch
//  dn2215-dont_change_default_archiver_2.patch
//
//  2.3.0
//  dn230-remove_GZip_compression_and_change_external_filter_view.patch
//  dn269-NE_SFX_detection-jo20620b.patch
//
//  2.7.0
//  dn270-delete_diz_after_moving_to_archive.patch
//  dn270-password_with_spaces_warning.patch
//  dn270-ACE_SFX_detection.patch
//  dn21116-more_strict_ARJ_detection.patch
//  dn21128-archive_name_depends_on_Use_LFN-jo20604a.patch
//  dn21202-archive(i)-passwords_differ_warning.patch
//  dn21225-PackageForTheWeb_(CAB-sfx)_detection.patch
//  dn21225-WinZIP_SFX-detection.patch
//  dn328-ARJ_ACE_defaults_remove_EXE_from_names.patch
//  dn3216-archives(f)-unselect_files_only_when_needed.patch
//
//  3.7.0
//  dn370-archives(if)-improve_and_fix.patch
//  dn31005-bp_to_vp_on_off_true_false.patch
//  dn31220-kernel(if)-work_with_temporary_files.patch
//  dn31220-packagefortheweb_detection.patch
//  dn40205-arc_add_options_fix.patch
//  dn40225-LHA_COM_SFX_detection.patch
//  dn40307-define_DN_Micro.patch
//  dn40328-7Zip.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Archiver;

interface uses Views, Objects, DOS, LFN, LFNCol, tree, ExtraMem,
               collect, drivers, profile, FileDIZ;

Const DNARC = 'ARCHIVER.INI'#0;

type
     TStr4 = String[4];
     TStr5 = String[5];

    PArcFile   = ^TArcFile;
    TArcFile   = Record
                  LFN    : TLFNIndex; {DataCompBoy}
                  FName  : PString;
                  Select : boolean;
                  Attr   : AWord;
                  USize,PSize,
                  Date   : Longint;
                 end;

    PFInfo     = ^TFInfo;
    TFInfo     = Record
                  LFN     : TLFNIndex; {DataCompBoy}
                  FName   : String; {DataCompBoy}
                  USize   : LongInt;
                  PSize   : LongInt;
                  Date    : LongInt;
                  Attr    : Byte;
                  Last    : Byte;
                  { 0 - not last    }
                  { 1 - archive end }
                  { 2 - broken arc  }
                 end;

     PARJArchive = ^TARJArchive;
     TARJArchive = object(TObject)
        Packer,
        UnPacker,
        Extract,
        ExtractWP,
        Add,Move,Garble,
        Delete,
        Test,
        IncludePaths,
        ExcludePaths,
        ForceMode,
        RecoveryRec,
        SelfExtract,
        Solid,
        RecurseSubDirs,
        StoreCompression,
        FastestCompression,
        FastCompression,
        NormalCompression,
        GoodCompression,
        UltraCompression: PString;
        ListChar: Char;
        PassDirNames: Boolean;
        UseLFN: Boolean;
        constructor Load(var S: TStream);
        procedure Store(var S: TStream);
        procedure Save;
        function GetVal(const Sign, AFile, Name, Default: PChar): String;
        constructor Init;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
        destructor Done; virtual;
     end;

    PFileInfo    = ^TFileInfo;
    TFileInfo    = object(TSortedCollection)
                    function   Compare(P1, P2: Pointer): Integer; virtual;
                    Procedure  FreeItem(Item : Pointer); virtual;
                    Procedure  PutItem(var S: TStream; Item: Pointer); virtual;
                    function   GetItem(var S: TStream): Pointer; virtual;
                   end;

    PUserSaver = ^TUserSaver;
    TUserSaver = object(TView)
      Screen: Pointer;
      SSize, SWidth: AInt;
      CShape, CPos: AWord;
      CheckIO: Boolean;
      isValid: Boolean;
      constructor Init(ACheck: Boolean);
      destructor Done; virtual;
      constructor Load(var S: TStream);
      procedure Store(var S: TStream);
      function Valid(Command: Word): Boolean; virtual;
    end;

const
    arcACE  = 0;  sigACE  {: TStr4} = 'ACE:';
    arcARJ  = 3;  sigARJ  {: TStr4} = 'ARJ:';
    arcCAB  = 6;  sigCAB  {: TStr4} = 'CAB:';
    arcHA   = 8;  sigHA   {: TStr4} = 'HA:';
    arcLHA  = 13; sigLHA  {: TStr4} = 'LHA:';
    arcRAR  = 16; sigRAR  {: TStr4} = 'RAR:';
    arcZIP  = 22; sigZIP  {: TStr4} = 'ZIP:';
{$IFNDEF MINARCH}         {       }
    arcAIN  = 1;  sigAIN  {: TStr4} = 'AIN:';
    arcARC  = 2;  sigARC  {: TStr4} = 'ARC:';
    arcBS2  = 4;  sigBS2  {: TStr4} = 'BS2:';
    arcBSA  = 5;  sigBSA  {: TStr4} = 'BSA:';
    arcCHZ  = 7;  sigCHZ  {: TStr4} = 'CHZ:';
    arcHAP  = 9;  sigHAP  {: TStr4} = 'HAP:';
    arcHPK  = 10; sigHPK  {: TStr4} = 'HPK:';
    arcHYP  = 11; sigHYP  {: TStr4} = 'HYP:';
    arcIS3  = 12; sigIS3  {: TStr4} = 'IS3:';
    arcLIM  = 14; sigLIM  {: TStr4} = 'LIM:';
    arcQUARK= 15; sigQUARK{: TStr4} = 'QRK:';
    arcSQZ  = 17; sigSQZ  {: TStr4} = 'SQZ:';
    arcTAR  = 18; sigTAR  {: TStr4} = 'TAR:';
    arcTGZ  = 19; sigTGZ  {: TStr4} = 'TGZ:';
    arcUC2  = 20; sigUC2  {: TStr4} = 'UC2:';
    arcUFA  = 21; sigUFA  {: TStr4} = 'UFA:';
    arcZOO  = 23; sigZOO  {: TStr4} = 'ZOO:';
    arcZXZ  = 24; sigZXZ  {: TStr4} = 'ZXZ:';
    arc7Z   = 25; sig7Z   {: TStr4} = '7Z:';
{$ENDIF}
    arcUNK  =255; {UNKNOWN Archiver}

    NumSupportedArchs = 26;

    PPacker:PChar =             'Packer';
    PUnPacker:PChar =           'Unpacker';
    PExtract:PChar =            'Extract';
    PExtractWP:PChar =          'ExtractWithPathnames';
    PAdd:PChar =                'Add';
    PMove:PChar =               'Move';
    PDelete:PChar =             'Delete';
    PGarble:PChar =             'Garble';
    PTest:PChar =               'Test';
    PIncludePaths:PChar =       'IncludePaths';
    PExcludePaths:PChar =       'ExcludePaths';
    PForceMode:PChar =          'ForceMode';
    PRecoveryRec:PChar =        'RecoveryRecord';
    PSelfExtract:PChar =        'SFX';
    PSolid:PChar =              'Solid';
    PRecurseSubDirs:PChar =     'RecurseSubDirs';
    PStoreCompression:PChar =   'StoreCompression';
    PFastestCompression:PChar = 'FastestCompression';
    PFastCompression:PChar =    'FastCompression';
    PNormalCompression:PChar =  'NormalCompression';
    PGoodCompression:PChar =    'GoodCompression';
    PUltraCompression:PChar =   'BestCompression';
    PListChar:PChar =           'ListChar';
    PPassDirNames:PChar =       'PassDirNames';
    PUseLFN:PChar =             'UseLFN';

    DefaultArchiver: AWord = arcARJ;
    DefaultArcMode:  AWord = 256*(64+1)+5;
    ExtractWithPathNames: boolean=true;
(*
    OldArchiveName: String = '';
    NewArchiveName: String = '';
*)
const ArcFile    : PBufStream = nil;
var   FileInfo   : TFInfo;
      ArcPos     : LongInt;

const
    ArcFileName: String = ''; {DataCompBoy}
    VArcFileName: String = ''; {JO's idea moved from DN/2 by piwamoto}
    PReader: PView = nil;

Function  ArchiveFiles(const S: String; Files: PCollection; MoveMode: Boolean; Owner: Pointer; Quiet: Boolean): Boolean;
procedure MakeArchive(S: String; Files: PCollection; MoveMode: Boolean; Owner: Pointer; Quiet: Boolean);
procedure UnarchiveFiles(const FName: String);
procedure InsertUserSaver(ACheck: Boolean);
procedure SkipSFX;
procedure Check4ArcId;
Function  _Cardinal(L: LongInt): Real;
Function  FromOct(S: String): LongInt; {fixed by piwamoto}
Function CheckForSpaces(S: string): boolean; { Flash }

{$IFNDEF MINARCH}
 const CDir: String = '';
{$ENDIF}

implementation

 uses DnUtil, advance, advance1, advance2, advance3, DNApp, Commands,
      Dialogs, FilesCol, FViewer, StartUp, RStrings, Memory, Messages,
      ArcView, FileCopy, HistList,FStorage, Menus, ArchDet, DnSvLd, FlPanel;


{ ------------------------------- Collections ----------------------------- }

Procedure TFileInfo.FreeItem;
begin
  if Item <> nil then
   begin
    DelLFN(PArcFile(Item)^.LFN);
    DisposeStr(PArcFile(Item)^.FName);
    Dispose(PArcFile(Item));
   end;
end;

function TFileInfo.Compare;
 var F1: PArcFile absolute P1;
     F2: PArcFile absolute P2;
begin
  if UpStrg(F1^.FName^) = UpStrg(F2^.FName^) then Compare := 0
    else Compare := 1 - 2*Integer(UpStrg(F1^.FName^) > UpStrg(F2^.FName^));
end;

Procedure TFileInfo.PutItem;
begin
 StoreLFN(S, PArcFile(Item)^.LFN);
 S.WriteStr(PArcFile(Item)^.FName);
 S.Write(PArcFile(Item)^.Select, SizeOf(Boolean)+SizeOf(AWord)+3*SizeOf(Longint));
end;

function TFileInfo.GetItem;
 var P: PArcFile;
begin
 New(P);
 P^.LFN:=LoadLFN(S);
 P^.FName := S.ReadStr;
 S.Read(P^.Select, SizeOf(Boolean)+SizeOf(AWord)+3*SizeOf(Longint));
 GetItem := P;
end;

{ --------------------------- All archives -------------------------------- }

constructor TARJArchive.Load;
begin
  Packer            := S.ReadStr;
  UnPacker          := S.ReadStr;
  Extract           := S.ReadStr;
  ExtractWP         := S.ReadStr;
  Add               := S.ReadStr;
  Move              := S.ReadStr;
  Delete            := S.ReadStr;
  Garble            := S.ReadStr;
  Test              := S.ReadStr;
  IncludePaths      := S.ReadStr;
  ExcludePaths      := S.ReadStr;
  ForceMode         := S.ReadStr;
  RecoveryRec       := S.ReadStr;
  SelfExtract       := S.ReadStr;
  Solid             := S.ReadStr;
  RecurseSubDirs    := S.ReadStr;
  StoreCompression  := S.ReadStr;
  FastestCompression:= S.ReadStr;
  FastCompression   := S.ReadStr;
  NormalCompression := S.ReadStr;
  GoodCompression   := S.ReadStr;
  UltraCompression  := S.ReadStr;
  S.Read(ListChar, SizeOf(ListChar) +
                   SizeOf(PassDirNames) +
                   SizeOf(UseLFN));
end;

procedure TARJArchive.Store;
begin
  S.WriteStr(Packer);
  S.WriteStr(UnPacker);
  S.WriteStr(Extract);
  S.WriteStr(ExtractWP);
  S.WriteStr(Add);
  S.WriteStr(Move);
  S.WriteStr(Delete);
  S.WriteStr(Garble);
  S.WriteStr(Test);
  S.WriteStr(IncludePaths);
  S.WriteStr(ExcludePaths);
  S.WriteStr(ForceMode);
  S.WriteStr(RecoveryRec);
  S.WriteStr(SelfExtract);
  S.WriteStr(Solid);
  S.WriteStr(RecurseSubDirs);
  S.WriteStr(StoreCompression);
  S.WriteStr(FastestCompression);
  S.WriteStr(FastCompression);
  S.WriteStr(NormalCompression);
  S.WriteStr(GoodCompression);
  S.WriteStr(UltraCompression);
  S.Write(ListChar, SizeOf(ListChar) +
                    SizeOf(PassDirNames) +
                    SizeOf(UseLFN));
end;

procedure TARJArchive.Save;
var Sign: TStr5;
    q: string;
 function StoS(P: PString): PChar;
 begin if p<>nil then q:=P^+#0 else q:=#0; StoS:=@q[1]; end;
begin
 FreeStr:=SourceDir + DNARC;
 Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
 if UseLFN then q:='1'#0 else q:='0'#0;
 WritePrivateProfileString(@Sign[1], PUseLFN, @q[1], @FreeStr[1]);
 if PassDirNames then q:='1'#0 else q:='0'#0;
 WritePrivateProfileString(@Sign[1], PPassDirNames, @q[1], @FreeStr[1]);
 q:=ListChar+#0;
 WritePrivateProfileString(@Sign[1], PListChar, @q[1], @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PUltraCompression, StoS(UltraCompression), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PGoodCompression, StoS(GoodCompression), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PNormalCompression, StoS(NormalCompression), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PFastCompression, StoS(FastCompression), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PFastestCompression, StoS(FastestCompression), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PStoreCompression, StoS(StoreCompression), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PRecurseSubDirs, StoS(RecurseSubDirs), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PSolid, StoS(Solid), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PRecoveryRec, StoS(RecoveryRec), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PSelfExtract, StoS(SelfExtract), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PForceMode, StoS(ForceMode), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PExcludePaths, StoS(ExcludePaths), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PIncludePaths, StoS(IncludePaths), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PTest, StoS(Test), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PDelete, StoS(Delete), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PGarble, StoS(Garble), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PMove, StoS(Move), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PAdd, StoS(Add), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PExtractWP, StoS(ExtractWP), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PExtract, StoS(Extract), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PUnPacker, StoS(UnPacker), @FreeStr[1]);
 WritePrivateProfileString(@Sign[1], PPacker, StoS(Packer), @FreeStr[1]);
end;

function TARJArchive.GetVal(const Sign, AFile, Name, Default: PChar): String;
var S: String;
begin
 S[0] := Char(GetPrivateProfileString(Sign, Name, Default, @S[1], 255, AFile));
 GetVal:=S;
end;

destructor TARJArchive.Done;
begin
  DisposeStr(Packer);
  DisposeStr(UnPacker);
  DisposeStr(Extract);
  DisposeStr(ExtractWP);
  DisposeStr(Add);
  DisposeStr(Move);
  DisposeStr(Delete);
  DisposeStr(Garble);
  DisposeStr(Test);
  DisposeStr(IncludePaths);
  DisposeStr(ExcludePaths);
  DisposeStr(ForceMode);
  DisposeStr(RecoveryRec);
  DisposeStr(SelfExtract);
  DisposeStr(Solid);
  DisposeStr(RecurseSubDirs);
  DisposeStr(StoreCompression);
  DisposeStr(FastestCompression);
  DisposeStr(FastCompression);
  DisposeStr(NormalCompression);
  DisposeStr(GoodCompression);
  DisposeStr(UltraCompression);
{ inherited Done;}
end;

{ ----------------------------- ARJ ------------------------------------}

constructor TARJArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  inherited Init;
{$IFNDEF OS2}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'ARJ'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'ARJ'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'e -p1 -hf4 -v -jycv'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'x -p1 -hf4 -v -jycv'));
{$ELSE}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'ARJ'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'ARJ'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'e -p1 -v -jycv'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'x -p1 -v -jycv'));
{$ENDIF}
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'a -a -a1'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'm -a -a1 -p1'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'd -p1'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               't -v -jycv'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-g'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       '-e'));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          '-y'));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        '-je1'));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-m0'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-m4'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-m3'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-m1'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-jm1 -jh65535'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-jm -jh65535'));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, '!');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
  q := GetVal(@Sign[1], @FreeStr[1], PPassDirNames, '1');
  if q='0' then PassDirNames := False else PassDirNames := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  if q='0' then UseLFN := False else UseLFN := True;
end;

function TARJArchive.GetID;
begin
  GetID := arcARJ;
end;

function TARJArchive.GetSign;
begin
  GetSign := sigARJ;
end;

procedure TARJArchive.GetFile;
const
   GARBLED_FLAG      = $01;
   OLD_SECURED_FLAG  = $02;
   VOLUME_FLAG       = $04;
   EXTFILE_FLAG      = $08;
   PATHSYM_FLAG      = $10;
   BACKUP_FLAG       = $20;
   SECURED_FLAG      = $40;

var i    : AWord;
    FP   : Longint;
    Extr : LongInt;
    C    : Char;
    h    : record
             First_Hdr_Size  : Byte;
             Version         : Byte;
             MinVer          : Byte;
             Host_OS         : Byte;
             ARJ_Flags       : Byte;
             Method          : Byte;
             File_Type       : Byte;
             Reserved        : Byte;
             Date_Time       : LongInt;
             Compressed_Size : LongInt;
             Original_Size   : LongInt;
             Original_CRC    : LongInt;
             Filespec_Pos    : AWord;
             File_Acs_Mode   : AWord;
             Host_Data       : AWord;
           end;
begin
   ArcFile^.Read(i,2);
   if (i<>60000) or (ArcFile^.Status <> 0) then begin FileInfo.Last:=2;Exit;end;
   ArcFile^.Read(i,2);
   if (i=0) then begin FileInfo.Last:=1;Exit;end;
   FP := ArcFile^.GetPos;

   ArcFile^.Read(h, SizeOf(h));

   with FileInfo do
   begin
     Date   := h.Date_Time;
     PSize  := h.Compressed_Size;
     USize  := h.Original_Size;
     if (h.ARJ_Flags and
       ((OLD_SECURED_FLAG or GARBLED_FLAG or SECURED_FLAG)))=0
         then Attr := 0 else Attr := Hidden;
     if h.File_Type = 3 then Attr := Attr or Directory;
   end;

   if h.ARJ_Flags and EXTFILE_FLAG<>0 then
   begin
     ArcFile^.Read(Extr, 4);
     if Extr<>0 then with FileInfo do Attr := Attr or SysFile;
   end else Extr := 0;

   FileInfo.FName := '';
   ArcFile^.Seek(FP + h.First_Hdr_Size);
   repeat
     ArcFile^.Read(C,1);
     if C <> #0 then FileInfo.FName := FileInfo.FName + C else Break;
   until ArcFile^.Status <> stOK;
   if (ArcFile^.Status <> stOK) then begin FileInfo.Last:=2;Exit;end;
   FileInfo.Last := 0;
   ArcFile^.Seek(FP + i + 4);
   Repeat
    ArcFile^.Read(i,2);
    if i>0 then ArcFile^.Seek(ArcFile^.GetPos+i+4);
   Until (i=0) or (ArcFile^.Status <> stOK);
   ArcFile^.Seek(ArcFile^.GetPos+Fileinfo.PSize);
end;

{ ------------------------------------------------------------------------- }

constructor TUserSaver.Init;
 var R: TRect;
begin
  R.Assign(0,0,0,0);
  inherited Init(R);
  CheckIO := ACheck;
  SetState(sfVisible, False);
  isValid := True;
  Screen := MemAlloc(UserScreenSize);
  if Screen = nil then Fail;
  Move(UserScreen^, Screen^, UserScreenSize);
  ClsAct := False;
  SSize := UserScreenSize;
  SWidth := UserScreenWidth;
  CShape := OldCursorShape;
  CPos := OldCursorPos;
end;

function TUserSaver.Valid; begin Valid := isValid end;

constructor TUserSaver.Load;
 var I: Byte;
begin
  inherited Load(S);
  DataSaver:=@Self;
  S.Read(SSize, 4*SizeOf(AInt)+SizeOf(Boolean));
  if UserScreen <> nil then FreeMem(UserScreen, UserScreenSize);
  UserScreenSize := SSize;
  UserScreenWidth := SWidth;
  UserScreen := MemAlloc(SSize);
  OldCursorShape := CShape;
  OldCursorPos := CPos;
  S.Read(UserScreen^, SSize);
  Screen := nil;
  isValid := False;
  I := 0;
  {$IFNDEF NONBP}
  if CheckIO then
    asm
      mov ax, $9900
      int 2Fh
      cmp bx, 'DN'
      jne @@1
      mov I, cl
    @@1:
    end;
  {$ENDIF}
  if I <> 0 then Msg(dlErrorsOccurred, nil, mfWarning+mfOKButton);
end;

destructor TUserSaver.Done;
begin
  if Screen <> nil then FreeMem(Screen, SSize);
  inherited Done;
end;

procedure TUserSaver.Store;
begin
  inherited Store(S);
  S.Write(SSize, 4*SizeOf(AInt)+SizeOf(Boolean));
  S.Write(Screen^, SSize);
end;

function ArchiveFiles;
  var C: TStr4;
      q, deftmp: byte;
begin
  ArchiveFiles := False;
  if PosChar(':',S) < 3 then Exit;
  C := UpStrg(Copy(S,1,PosChar(':', S)));
  q:=GetArchiveTagBySign(C);
  if q=arcUNK then exit;
  deftmp := DefaultArchiver;
  DefaultArchiver := q;
  ArchiveFiles := True;
  MakeArchive(Copy(S, PosChar(':',S)+1, 255), Files, MoveMode, Owner, Quiet);
  DefaultArchiver := deftmp;
end;

{ Flash >>> }
function CheckForSpaces(S: string): boolean;
begin
CheckForSpaces:=(Pos(' ',S)=0);
end;
{ Flash <<< }

        {-DataCompBoy-}
procedure MakeArchive;

   var
     AID: Word;
     C: String[40];
     CurDir: String;
     Arc: PARJArchive;
     f: lFile;
     D : record
       Name : String;
       Password : String[40];
       Add : String[80];
       Options : Word;
       Archiver : Word;
       Mode : Word;
     end;
     Dlg: PDialog; { Flash 25-12-2002 }
     BFile: String;
     F1: lText;


  function MakeListFile: String;
   var F: lText;
       PF: PFileRec;
       I: Integer;
       S, S1: String;
       B: Boolean;

    procedure PutDir(const SS: String);
     var I: Integer;
         S1: String;
         SR: lSearchRec;
    begin
       ClrIO;
{piwamoto.src.begin}
       if Arc^.PassDirNames then
        if not ((PF^.Attr and Directory <> 0) and (D.Options and 1 = 0))
           then if B then S := S + ' ' + SS else WriteLn(F.T, SS);
{piwamoto.src.end}
       ClrIO;
       lFindFirst(MakeNormName(SS, x_x), AnyFile, SR); {JO}
       While (DOSError = 0) and not Abort and (S[0] < #100) do
        begin
 {$IFNDEF OS2}
         if Arc^.UseLFN
          then
 {$ENDIF}
               S1:=GetLongRelPath(MakeNormName(SS,SR.FullName))
 {$IFNDEF OS2}
          else S1:=GetShortRelPath(MakeNormName(SS,SR.SR.Name))
 {$ENDIF}
 ;
         if (SR.SR.Attr and (Directory {$IFNDEF OS2} + VolumeID{$ENDIF}) = 0) then   {JO}
           if B then S := S + ' ' + SquashesName(S1)
                else WriteLn(F.T, S1);
{piwamoto.src.begin}
         if (SR.SR.Attr and Directory <> 0) and (SR.SR.Name[1] <> '.') and
            (D.Options and 64 <> 0) then
{piwamoto.src.end}
 {$IFNDEF OS2}
          if Arc^.UseLFN
           then
 {$ENDIF}
                PutDir(MakeNormName(SS,SR.FullName))
 {$IFNDEF OS2}
           else PutDir(MakeNormName(SS,SR.SR.Name))
 {$ENDIF}
 ;
         DOSError := 0;
         lFindNext(SR);
        end;
       lFindClose(SR);
    end;

  begin
   B := Arc^.ListChar = ' ';
   if Quiet and not B then
    begin
    MakeListFile := Arc^.ListChar + SwpDir + '$DN'+ItoS(DNNumber)+'$.LST';
    Exit;
    end
   else if Quiet then B := True;
   if B then S := '' else
    begin
     S := SwpDir + '$DN'+ItoS(DNNumber)+'$.LST';
     lAssignText(F, S); ClrIO;
     lRewriteText(F);
     B := IOResult <> 0;
     if B then S := '' else S := Arc^.ListChar + S;
    end;
   for I := 0 to Files^.Count - 1 do
    begin
     PF := Files^.At(I);
     { Flash >>> }
     if (PF^.DIZ <> nil) and (FMSetup.Options and fmoPreserveDesc = 0)
        and (MoveMode) then
      DeleteDIZ(CalcDPath(PF^.DIZ, PF^.Owner), MakeFileName(PF^.Name), GetLFN(PF^.LFN));
     { Flash <<< }

 {$IFNDEF OS2}
     if Arc^.UseLFN
      then
 {$ENDIF}
           S1 := GetLongRelPath(MakeNormName(PF^.Owner^, GetLFN(PF^.LFN)))
 {$IFNDEF OS2}
      else S1 := GetShortRelPath(MakeNormName(PF^.Owner^, MakeFileName(PF^.Name)))
 {$ENDIF}
 ;
     if PF^.Attr and Directory = 0
      then if B then S := S + ' ' + SquashesName(S1) else WriteLn(F.T, S1)
{piwamoto.src.begin}
      else if B then S := S + ' ' + SquashesName(S1+'\*.*') else PutDir(S1);
{piwamoto.src.end}
     MakeListFile := S;
     if S[0] > #100 then Exit;
    end;
   MakeListFile := S;
   if not B then Close(F.T);
  end;

  procedure Unselect(PF: PFileRec); {$IFDEF BIT_16}far;{$ENDIF}
  begin
    Message(Owner, evCommand, cmCopyUnselect, PF);
  end;

  function AddString(P: PString): String;
  begin
   if P = nil then AddString := '' else
    begin
      if Copy(P^,1,1) = '+' then
       if Copy(P^,1,2) <> '++' then AddString := Copy(P^,2,255)
                               else AddString := ' '+Copy(P^,2,255)
       else AddString := ' '+P^;
    end;
  end;

  { Flash 25-12-2002 >>> }
  function AcceptedPassword: Boolean;
  begin
  if (ExecResource(dlgReenterPassword, S) = cmOK) and (S = D.Password) then
   AcceptedPassword:=true
  else
   begin
   MessageBox(GetString(dlPasswordsDiffer),nil,mfOKButton);
   AcceptedPassword:=false;
   end;
  end;
  { Flash 25-12-2002 <<< }

label Ex,TryAgain;
begin
  FillChar(D, SizeOf(D), 0);
  D.Name := S;
  D.Archiver := DefaultArchiver;
  D.Options := (DefaultArcMode shr 8) and not 2 or (Byte(MoveMode)*2);
  D.Mode := DefaultArcMode and 255;
  D.Add := '';
  { Flash 25-12-2002 >>> }
  Dlg := PDialog(LoadResource(dlgArchiveFiles));
  Dlg^.SetData(D);
  repeat
TryAgain:
    if not Quiet and (Desktop^.ExecView(Dlg) <> cmOK) then Exit
     else Dlg^.GetData(D);
  { Flash 25-12-2002 <<< }
    AID := (D.Mode and 255) or (D.Options shl 8);
    if (D.Archiver <> DefaultArchiver) or
       (AID <> DefaultArcMode)         then
     begin
       DefaultArchiver := D.Archiver;
       DefaultArcMode := AID;
       Message(Application, evCommand, cmUpdateConfig, nil);
     end;
    S := '';
    AID := D.Archiver;
    Arc := GetArchiveByTag(AID);
    if Arc = nil then Exit;
  { Flash >>> }
  if D.Password <> '' then
   if not CheckForSpaces(D.Password) then
    if not Arc^.UseLFN then
     begin
     MessageBox(GetString(dlSpacesInPassword), nil, mfWarning + mfOkButton);
     goto TryAgain;
     end;
  until (D.Password = '') or AcceptedPassword;
  if (Arc^.UseLFN) and not CheckForSpaces(D.Password) then D.Password:='"'+D.Password+'"';
  { Flash <<< }
  lGetDir(0, CurDir);
  if Abort then goto Ex;
  if CurDir[Length(CurDir)] <> '\' then CurDir := CurDir + '\';
  if D.Options and 2 = 2 then C := CnvString(Arc^.Move)
                         else C := CnvString(Arc^.Add);
  if D.Options and 1 = 0 then C := C + AddString(Arc^.ExcludePaths)
                         else C := C + AddString(Arc^.IncludePaths);
  if D.Options and 64 <> 0 then C := C + AddString(Arc^.RecurseSubDirs);

  if D.Password <> '' then C := C + ' '+CnvString(Arc^.Garble)+D.Password;
  if D.Options and 4 <> 0 then C := C + AddString(Arc^.ForceMode);
  if D.Options and 8 <> 0 then C := C + AddString(Arc^.Solid);
  if D.Options and 16 <> 0 then C := C + AddString(Arc^.RecoveryRec);
  if D.Options and 32 <> 0 then C := C + AddString(Arc^.SelfExtract);
  if D.Mode = 0 then C := C + AddString(Arc^.StoreCompression) else
  if D.Mode = 1 then C := C + AddString(Arc^.FastestCompression) else
  if D.Mode = 2 then C := C + AddString(Arc^.FastCompression) else
  if D.Mode = 3 then C := C + AddString(Arc^.NormalCompression) else
  if D.Mode = 4 then C := C + AddString(Arc^.GoodCompression) else
  if D.Mode = 5 then C := C + AddString(Arc^.UltraCompression);

{ If LFNPresent then
   begin
    NewArchiveName:=D.Name;
    If ExistFile(NewArchiveName)
     then OldArchiveName:=lfGetShortFileName(NewArchiveName)
     else begin
           lAssignFile(f, NewArchiveName);
           lRewriteFile(f,1);
           Close(f.F);
           OldArchiveName:=lfGetShortFileName(NewArchiveName);
           lEraseFile(f);
          end;
   end
  else
   begin
    NewArchiveName:=D.Name;
    OldArchiveName:=D.Name;
   end;}

{  S := C + ' ' + D.Add + ' ' + OldArchiveName + ' ' + MakeListFile;}
  S := CnvString(Arc^.Packer) + ' ' + C + ' ' + D.Add + ' ' +
       SquashesName(D.Name) + ' ' + MakeListFile;
  GlobalMessage(evCommand, cmMakeForced, nil);
  if (Owner <> nil)
     and (PFilePanel(ActivePanel)^.SelNum > 0) then { Flash 17-02-2003 }
   Files^.ForEach(@Unselect);
  if not Quiet then Message(Application, evCommand, cmExecString, @S) else
   begin
   BFile := SwpDir+'$DN'+ItoS(DNNumber)+'$'+BatchExtension;
   lAssignText(F1, BFile);
   lRewriteText(F1);
   WriteLn(F1.T, S);
   Close(F1.T);
   end;
Ex:
  Dispose(Arc, Done);
  Dispose(Dlg, Done); { Flash }
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure UnarchiveFiles;
  var AType: PARJArchive;
      DT: Record S: String; W: Word; Psw: String[30]; end;
      ExtrDir: String;
      ExtrChar: String[40];
      S: String;
      Dr: String;
      Nm: String;
      Xt: String;
label ex,TryAgain;
begin
   lFSplit(FName, Dr, Nm, Xt);
   ArcFileName := FName;
   New(ArcFile,Init(FName, stOpenRead, 512));
   if (ArcFile=nil) or (ArcFile^.Status <> stOK) then
    begin
      if TempFile <> '' then TempFile := '';
      StdMsg(4); FreeObject(ArcFile); Abort := True; Exit;
    end;
   ArcPos := 0;
   SkipSFX;
   AType := DetectArchive;
   FreeObject(ArcFile);
   if AType = nil then Exit;
   ExtrDir := '';
   DT.S := '';
   DT.Psw := '';
{   DT.W := byte(ExtractWithPathNames);}
   DT.W := 1;
   Message(Application, evCommand, cmPushFullName, @DT.S);
   if CopyDirName <> '' then DT.S := CopyDirName;
   if DT.S = cTEMP_ then DT.S := '';
   if DT.S = '' then GlobalMessageL(evCommand, cmPushName, hsExtract);
   if DT.S = '' then DT.S := HistoryStr(hsExtract, 0);
   if DT.S = cTEMP_ then DT.S := '';
   if (DT.S[0] > #3) and (DT.S[Length(DT.S)] <> '\') then DT.S := DT.S + '\';
   CopyDirName := '';
TryAgain:
   if ExecResource( dlgExtract, DT) <> cmOK then goto Ex;
{
   if ((DT.W and 1)<>0)<>ExtractWithPathNames then ConfigModified:=True;
   ExtractWithPathNames:=(DT.W and 1) <> 0;
}
   if (DT.S = '') or (DT.S = '.') then DT.S := GetPath(FName);
   CreateDirInheritance(DT.S, True);
   ExtrDir := DT.S;
   ExtrChar := CnvString(AType^.ExtractWP);
   if DT.W and 1 = 0 then ExtrChar := CnvString(AType^.Extract);
   if DT.W and 2 <> 0 then ExtrChar := CnvString(AType^.Test);
   S := '';
   if DT.Psw <> '' then
   { Flash >>> }
    if CheckForSpaces(DT.Psw) then
     S := S + ' ' + CnvString(AType^.Garble) + DT.Psw + ' '
    else
    if AType^.UseLFN then
     S := S + ' ' + CnvString(AType^.Garble) + '"'+ DT.Psw + '"' + ' '
    else
     begin
     MessageBox(GetString(dlSpacesInPassword), nil, mfWarning + mfOkButton);
     goto TryAgain;
     end;
   { Flash <<< }
{$IFNDEF OS2}
   if AType^.UseLFN then
{$ENDIF}
     S := S + FName
{$IFNDEF OS2}
     else S := S + lfGetShortFileName(FName)
{$ENDIF}
     ;
   S := CnvString(AType^.unPacker)+' '+ExtrChar+' '+SquashesName(S);
   if Xt = '' then S := S + '.';{piwamoto: extracting from extensionless archives}
   lGetDir(0, DirToChange);
   Advance2.lChDir(ExtrDir);
{   DelDoubles('  ', S);}

   Message(Application, evCommand, cmExecString, @S);
   Advance2.lChDir(DirToChange); DirToChange := '';

   {Password := DT.Psw;
   ExtractFiles(AFiles, ExtrDir, Own, DT.W);}

Ex:
   FreeObject(AType);
end;
        {-DataCompBoy-}

procedure InsertUserSaver;
begin
  Desktop^.Insert(New(PUserSaver, Init(ACheck)));
  FreeMem(UserScreen, UserScreenSize);
  UserScreenSize := ScreenWidth*ScreenHeight*2;
  UserScreenWidth := ScreenWidth;
  OldCursorPos := 0; OldCursorShape := $FFFF;
  HideMouse;
  GetMem(UserScreen, UserScreenSize);
  System.Move(ScreenBuffer^, UserScreen^, UserScreenSize);
  ShowMouse;
end;

procedure SkipSFX; {piwamoto}
var
   TempWord: AWord;
{used for MZExeRec.RelOffs, MZExeRec.NewExe.Type, NEhdrRec.SegmentOffset, PEhdrRec.ObjNum}
   MZExeRec: record
              ExeID:   AWord;
              LastB:   AWord;
              TotalP:  AWord;
              NewExe:  LongInt;
             end;
   PEhdrRec: record
              ObjNum:    AWord;
              MaxObjNum: AWord;
              MaxOffset: Longint;
             end;
   PEobjRec: record
              Name:        Array [0..7] of char;
              VirtualSize: LongInt;
              RVA:         LongInt;
              Size:        Longint;
              Offset:      Longint;
              Reserved1:   Longint;
              Reserved2:   Longint;
              Reserved3:   Longint;
              Reserved4:   Longint;
             end;
   LXhdrRec: record
              NonResTableOffs : Longint;
              TableLenght : Byte;
             end;
   NEhdrRec: record
              NumOfSegments   : AWord;    {1c}
              SegTableOffset  : AWord;    {22}
              ResTableOffset  : AWord;    {24}
              ResNamesOffset  : AWord;    {26}
              AlignShiftCount : AWord;    {32}
              SegmentSize     : AWord;

              ResourceAlign   : AWord;
              ResourceType    : AWord;
              ResourceTypeNum : AWord;
              ResourceOffset  : AWord;
              ResourceLenght  : AWord;
             end;
   LEhdrRec: record
              PageCount       : Longint; {14}
              PageSize        : Longint; {28}
              LastPageSize    : Longint; {2c}
              DataPagesOffs   : Longint; {80}
             end;
begin
   ArcFile^.Read(MZExeRec.ExeID, SizeOf(MZExeRec.ExeID) + SizeOf(MZExeRec.LastB) + SizeOf(MZExeRec.TotalP));
   if (MZExeRec.ExeID = $5a4d{'MZ'}) or (MZExeRec.ExeID = $4d5a{'ZM'}) then
    begin {MZ}
     ArcFile^.Seek($18);
     ArcFile^.Read(TempWord, SizeOf(TempWord));
     ArcFile^.Seek($3c);
     ArcFile^.Read(MZExeRec.NewExe, SizeOf(MZExeRec.NewExe));
     if MZExeRec.LastB <> 0 then Dec(MZExeRec.TotalP);
     ArcPos := LongInt(MZExeRec.TotalP)*512 + MZExeRec.LastB;
     if (TempWord >= $40) and (_Cardinal(MZExeRec.NewExe + $100) < ArcFile^.GetSize) then
      begin
       ArcFile^.Seek(MZExeRec.NewExe);
       ArcFile^.Read(TempWord, SizeOf(TempWord));
       Case TempWord of
        $4550 :
         begin {PE}
          ArcFile^.Seek(MZExeRec.NewExe + 6);
          ArcFile^.Read(PEhdrRec.ObjNum, SizeOf(PEhdrRec.ObjNum));
          PEhdrRec.MaxObjNum := 0;
          PEhdrRec.MaxOffset := 0;
          ArcFile^.Seek(MZExeRec.NewExe + $F8); {offset 2 first object}
          for TempWord := 1 to PEhdrRec.ObjNum do
           begin
            ArcFile^.Read(PEobjRec, SizeOf(PEobjRec));
            if PEobjRec.Offset > PEhdrRec.MaxOffset then
              begin
               PEhdrRec.MaxOffset := PEobjRec.Offset;
               PEhdrRec.MaxObjNum := TempWord;
              end;
           end;
          ArcFile^.Seek(MZExeRec.NewExe + $28*PEhdrRec.MaxObjNum + $0D0); {offset 2 object with MaxOffset}
          ArcFile^.Read(PEobjRec, SizeOf(PEobjRec));
          if PEobjRec.Name = '_winzip_' then
            begin
             ArcFile^.Seek(ArcFile^.GetPos - 2*SizeOf(PEobjRec));
             ArcFile^.Read(PEobjRec, SizeOf(PEobjRec));
            end;
          ArcPos := PEobjRec.Offset + PEobjRec.Size;
         end;
        $454e :
         begin {NE}
          ArcFile^.Seek(MZExeRec.NewExe + $1c);
          ArcFile^.Read(NEhdrRec.NumOfSegments, SizeOf(NEhdrRec.NumOfSegments));
          ArcFile^.Seek(MZExeRec.NewExe + $22);
          ArcFile^.Read(NEhdrRec.SegTableOffset, SizeOf(NEhdrRec.SegTableOffset)
                                                + SizeOf(NEhdrRec.ResTableOffset)
                                                + SizeOf(NEhdrRec.ResNamesOffset));
          ArcFile^.Seek(MZExeRec.NewExe + $32);
          ArcFile^.Read(NEhdrRec.AlignShiftCount, SizeOf(NEhdrRec.AlignShiftCount));
          if NEhdrRec.AlignShiftCount = 0 then NEhdrRec.AlignShiftCount := 9;
          if NEhdrRec.ResTableOffset = NEhdrRec.ResNamesOffset then
           begin {no resources present: search 4 last segment}
            ArcFile^.Seek(LongInt(NEhdrRec.SegTableOffset) + MZExeRec.NewExe + 8*NEhdrRec.NumOfSegments - 8);{last segment}
            ArcFile^.Read(TempWord, SizeOf(TempWord));{NEhdrRec.SegmentOffset}
            ArcFile^.Read(NEhdrRec.SegmentSize, SizeOf(NEhdrRec.SegmentSize));
            ArcPos := LongInt(TempWord) shl NEhdrRec.AlignShiftCount + NEhdrRec.SegmentSize;
           end
          else
           begin {exe with resources: search 4 last resource}
            ArcFile^.Seek(MZExeRec.NewExe + NEhdrRec.ResTableOffset);
            ArcFile^.Read(NEhdrRec.ResourceAlign, SizeOf(NEhdrRec.ResourceAlign));
            ArcPos := ArcFile^.GetPos - 8; {-8=compensation}
            NEhdrRec.ResourceTypeNum := 0;
            Repeat {search for last resource}
             ArcPos := ArcPos + NEhdrRec.ResourceTypeNum*12 + 8;
             ArcFile^.Seek(ArcPos);
             ArcFile^.Read(NEhdrRec.ResourceType, SizeOf(NEhdrRec.ResourceType) + SizeOf(NEhdrRec.ResourceTypeNum));
            Until (NEhdrRec.ResourceType = 0) or (ArcFile^.Status <> stOK);
            Repeat {search for non-empty resource}
             ArcPos := ArcPos - 12;
             ArcFile^.Seek(ArcPos);
             ArcFile^.Read(NEhdrRec.ResourceOffset, SizeOf(NEhdrRec.ResourceOffset) + SizeOf(NEhdrRec.ResourceLenght));
            Until (NEhdrRec.ResourceOffset <> 0) or (ArcFile^.Status <> stOK);
            ArcPos := LongInt(NEhdrRec.ResourceOffset + NEhdrRec.ResourceLenght) shl NEhdrRec.ResourceAlign;
           end;
        end;
        $584c :
         begin {LX}
          ArcFile^.Seek(MZExeRec.NewExe + $88);
          ArcFile^.Read(LXhdrRec.NonResTableOffs, SizeOf(LXhdrRec.NonResTableOffs));
          ArcFile^.Seek(LXhdrRec.NonResTableOffs);
          ArcFile^.Read(LXhdrRec.TableLenght, SizeOf(LXhdrRec.TableLenght));
          ArcPos := LXhdrRec.NonResTableOffs + LXhdrRec.TableLenght + 4; {4=1+3}
         end;
        $454c :
         begin {LE}
          ArcFile^.Seek(MZExeRec.NewExe + $14);
          ArcFile^.Read(LEhdrRec.PageCount, SizeOf(LEhdrRec.PageCount));
          ArcFile^.Seek(MZExeRec.NewExe + $28);
          ArcFile^.Read(LEhdrRec.PageSize, SizeOf(LEhdrRec.PageSize) + SizeOf(LEhdrRec.LastPageSize));
          ArcFile^.Seek(MZExeRec.NewExe + $80);
          ArcFile^.Read(LEhdrRec.DataPagesOffs, SizeOf(LEhdrRec.DataPagesOffs));
          ArcPos := LEhdrRec.DataPagesOffs + (LEhdrRec.PageCount - 1)*LEhdrRec.PageSize + LEhdrRec.LastPageSize;
         end;
        {other EXE types}
       end;{Cas.e EXEtype}
      end;{??-EXE}
    end; {it isn't an EXE}
   Check4ArcId;
   ArcFile^.Seek(ArcPos);
end; {SkipSFX}

procedure Check4ArcId; {piwamoto}
var
   ArcId, ArcPosID : LongInt;
begin
   ArcPosID := ArcPos;
   Repeat
    ArcFile^.Seek(ArcPosID);
    ArcFile^.Read(ArcId, SizeOf(ArcId));
    Inc (ArcPosID);
   Until ((ArcPosID - ArcPos) = $1fd) or {1fch = SizeOf(DN's internal ArcFile^.Read cache size) - SizeOf(ArcId)}
         (ArcFile^.EOF) or
   {ZIP} (ArcId = $04034b50 {'PK'#3#4}) or
   {RAR} (ArcId = $21726152 {'Rar!'}) or
   {ARJ} (((ArcId and $ffff) = $ea60) and (Byte(ArcId shr 24) < $b)) or

{$IFNDEF MINARCH}
   {IS3} (ArcId = $8c655d13 {#$13#$5D#$65#$8C}) or
   {ZOO} (ArcId = $fdc4a7dc {#$DC#$A7#$C4#$FD}) or
   {CHZ} (ArcId = $46684353 {'SChF'}) or
   {CHZ} (ArcId = $44684353 {'SChD'}) or
   {LHA} ((ArcId and $f8ffffff) = $30686C2D{-lh0...-lh7}) or {LHA/LZH}
   {LHA} (ArcId = $736F5920) or {' Yos' LHA/LZH .COM SFX}
   {7Z } (ArcId = $AFBC7A37) or {7-Zip}
{$ENDIF}

  {COFF} (ArcId = $0003014c {#$4c#$01#$03#$00}) or {unix-style COFF executable - GNU C / go32stub 2.02 // DJ Delorie}
   {ACE} (ArcID = $4543412a {'*ACE'}) or
   {ACE} (ArcID = $78667321 {'!sfx'}) or {!sfx - ACE-SFX script}
   {ACE} (ArcID = $5846532a {'*SFX'}) or {**SFX** - ACE-SFX script}
   {CAB} (ArcId = $4643534d {'MSCF'});
   if ((ArcPosID - ArcPos) < $1fd) and not (ArcFile^.EOF) then ArcPos := ArcPosID - 1;
{$IFNDEF MINARCH}
   if (ArcId and $f8ffffff) = $30686C2D {LHA/LZH} then ArcPos := ArcPos - 2;
   if ArcId = $736F5920 {LHA .COM SFX} then
     begin {must be after LHA/LZH ArcPos correction}
      ArcPosID := ArcPos - 8;
      ArcFile^.Seek(ArcPosID);
      repeat
        ArcFile^.Read(ArcID, 1);
        Inc (ArcPosID);
      until (Byte(ArcID) = $29{')'}) or (ArcPosID = ArcPos);
      {don't care if ArcID <> $29}
      ArcFile^.Seek(ArcPosID - 5);
      ArcFile^.Read(ArcId, SizeOf(ArcId));{DataSize}
      if (ArcId = $63282053) or (ArcId = $6328204C) then
        begin
         ArcPos := ArcPos + $4c0;{SFX 'S' edition minlen=4ef, max=686}
         if Byte(ArcId) = $4C {'L'}
           then ArcPos := ArcPos + $200;{SFX 'L' edition minlen=72d, max=7ba}
         Check4ArcId;
         Exit;
        end;
     end;
{$ENDIF}
   if ArcId = $0003014c {#$4c#$01#$03#$00} then
     begin {unix-style COFF executable - GNU C / go32stub 2.02 // DJ Delorie}
      ArcFile^.Seek(ArcPos + $68);
      ArcFile^.Read(ArcPosID, SizeOf(ArcPosID));{DataOffset}
      ArcFile^.Read(ArcID, SizeOf(ArcID));{DataSize}
      ArcPos := ArcPos + ArcPosID + ArcID;
      Exit;
     end;
   if ArcID = $78667321 {!sfx - ACE-SFX script} then
     begin {must be before '*ACE' & '*SFX' ArcPos correction}
      ArcFile^.Read(ArcPosID, 2);{skip 2 bytes}
      ArcFile^.Read(ArcPosID, SizeOf(ArcPosID));{size of !sfx! script}
      ArcPos := ArcPos + ArcPosID + 8;
      ArcFile^.Read(ArcPosID, SizeOf(ArcPosID));{skip 4 bytes}
      ArcFile^.Read(ArcPosID, SizeOf(ArcPosID));{script is not empty?}
      ArcFile^.Seek(ArcPos);
      ArcFile^.Read(ArcId, SizeOf(ArcId));
      if (ArcPosId <> 0) and (ArcId = $4543412a) then ArcID := $5846532a;
     end;
   if ArcID = $4543412a {'*ACE'} then ArcPos := ArcPos - 8;
   if ArcID = $5846532a {**SFX** - ACE-SFX script} then
     begin
      ArcPos := ArcPos - 6;
      ArcFile^.Seek(ArcPos);
      ArcPosID := 0;
      ArcFile^.Read(ArcPosID, 2);{only 2 bytes}
      ArcPos := ArcPos + ArcPosID + 4;
      ArcFile^.Seek(ArcPos);
      ArcFile^.Read(ArcPosID, 2);{only 2 bytes}
      ArcPos := ArcPos + ArcPosID + 2;
      ArcFile^.Read(ArcPosID, 3);{skip 3 bytes}
      ArcFile^.Read(ArcPosID, SizeOf(ArcPosID));{size of **SFX** script}
      ArcPos := ArcPos + ArcPosID;
      Exit;
     end;
{last check - uses ArcId}
   if (ArcPosID - ArcPos) = $1fd {No known ArcId found} then
     begin {check for PackageForTheWeb script}
      ArcFile^.Seek(ArcPos);
      ArcFile^.Read(ArcPosID, SizeOf(ArcPosID));{size of script}
      if ((ArcPosID and $fffc0000) = 0{<$40000}{feel free to change it}) then
        begin
         ArcPosID := ArcPosID + ArcPos + 4;
         if ArcPosID < ArcFile^.GetSize then
           begin
            ArcFile^.Seek(ArcPosID);
            ArcFile^.Read(ArcId, SizeOf(ArcId));
            if ArcID = $4643534d {'MSCF'} then ArcPos := ArcPosID;
           end;
        end;
     end;
end; {Check4ArcId}

function _Cardinal(L : LongInt) : Real;
  {-Return the unsigned equivalent of L as a real}
begin
  if L < 0 then
    _Cardinal := 4294967296.0+L
  else
    _Cardinal := L;
end; {_Cardinal}

function FromOct(S: String): LongInt; {fixed by piwamoto}
 var I,L: LongInt;
begin
  L := 0;
  for I := 1 to Length(S) do
    if S[I] in['0'..'7'] then L := L shl 3 + Byte(S[I]) - 48;
  FromOct := L;
end;

end.
