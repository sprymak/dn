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
//  dn16rc1-Archivers_Optimization-diff154byMV.patch
//
//  2.0.0
//  dn200-ZIP_processing_fix.patch
//
//  2.3.0
//  dn247-text_and_file_find_improve.patch
//  dn247-read_ZIP_central_directory.patch
//
//  2.7.0
//  dn328-ARJ_ACE_defaults_remove_EXE_from_names.patch
//
//  3.7.0
//  dn370-archives(if)-improve_and_fix.patch
//  dn31005-bp_to_vp_on_off_true_false.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
unit Arc_Zip; {ZIP}

interface
 uses Archiver, Advance, Advance1, Objects, LFNCol, Dos;

type
 PZIPArchive = ^TZIPArchive;
 TZIPArchive = object(TARJArchive)
     constructor Init;
     procedure GetFile; virtual;
     function GetID: Byte; virtual;
     function GetSign: TStr4; virtual;
 end;

type
    TZIPLocalHdr = record
     ID: LongInt;
     Extract: AWord;
     GeneralPurpose: AWord;
     Method: AWord;
     LastModDate: LongInt;
     CRC32: LongInt;
     CompressedSize: LongInt;
     OriginalSize: LongInt;
     FNameLength: AWord;
     ExtraField: AWord;
    end;

    TZIPCentralFileRec = record
     ID: Longint;
     VersionMade : AWord;
     Version2Extr : AWord;
     GeneralPurpose: AWord;
     Method : AWord;
     LastModDate : LongInt;
     CRC32 : LongInt;
     CompressedSize: LongInt;
     OriginalSize: LongInt;
     FNameLength : AWord;
     ExtraField : AWord;
     FileCommLength : AWord;
     DiskNumStart : AWord;
     InternalFAttr : AWord;
     ExternalFAttr : LongInt;
     OffsetLocHeader : LongInt;
    end;

var
  CentralDirRecPresent: Boolean;

implementation
uses U_Keymap, FViewer;

{ ----------------------------- ZIP ------------------------------------}

constructor TZIPArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
{$IFNDEF OS2}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'PKZIP'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'PKUNZIP'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            ''));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          '-d'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                '-a -wsh'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               '-m -wsh'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             '-d'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-s'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               '-t'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       '-P'));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       '-p'));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          ''));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-e0'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-es'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-ef'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-en'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-ex'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-exx'));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, '@');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
{$ELSE}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'ZIP'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'UNZIP'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            '-j -C'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          '-C'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                '-S'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               '-m -S'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             '-d'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-P'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               '-t -C'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       '-j'));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          '-q'));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     '-r'));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-0'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-1'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-3'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-6'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-8'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-9'));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, ' ');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
{$ENDIF}

  q := GetVal(@Sign[1], @FreeStr[1], PPassDirNames, '0');
  if q='0' then PassDirNames := False else PassDirNames := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  if q='0' then UseLFN := False else UseLFN := True;
end;

function TZIPArchive.GetID;
begin
  GetID := arcZIP;
end;

function TZIPArchive.GetSign;
begin
  GetSign := sigZIP;
end;

Procedure TZIPArchive.GetFile;
var
    P:   TZIPLocalHdr;
    HCF: TZIPCentralFileRec;
    FP, FPP: Longint;
    nxl: TXLat;
label 1;
begin
 if CentralDirRecPresent then
  begin
   ArcFile^.Read(HCF.ID, SizeOf(HCF.ID));
   if (ArcFile^.Status <> stOK) or (HCF.ID and $FFFF <> $4B50) then
     begin FileInfo.Last:=2; Exit; end;
   if (HCF.ID = $06054B50) or (HCF.ID = $06064B50) then
     begin FileInfo.Last:=1; Exit; end;
   ArcFile^.Read(HCF.VersionMade, SizeOf(HCF) - SizeOf(HCF.ID));
   if HCF.FNameLength > 255 then HCF.FNameLength := 255;
   FileInfo.FName[0] := Char(HCF.FNameLength);
   ArcFile^.Read(FileInfo.FName[1], HCF.FNameLength);
   FileInfo.Last := 0;
   FileInfo.Attr := (HCF.GeneralPurpose and 1) * Hidden;
   FileInfo.Date := HCF.LastModDate;
   FileInfo.USize := HCF.OriginalSize;
   FileInfo.PSize := HCF.CompressedSize;
   ArcFile^.Seek(ArcFile^.GetPos + HCF.ExtraField + HCF.FileCommLength);
  end
 else {CentralDirRecPresent}
  begin
 1:
   ArcFile^.Read(P.ID,4);
   if P.ID = $02014b50 then begin FileInfo.Last:=1;Exit;end;
   if P.ID = $08074B50 then {skip Spanned/Split block}
     begin
      ArcFile^.Read(P.ID,12);
      Goto 1;
     end;
   ArcFile^.Read(P.Extract,SizeOf(P)-4);
   if (ArcFile^.Status <> stOK) or (P.ID <> $04034B50) then begin FileInfo.Last:=2;Exit;end;
   if P.FNameLength > 255 then P.FNameLength := 255;
   ArcFile^.Read(FileInfo.FName[1], P.FNameLength);
   FileInfo.FName[0] := Char(P.FNameLength);
   FileInfo.Last := 0;
   FileInfo.Attr := (P.GeneralPurpose and 1) * Hidden;
   FileInfo.Date := P.LastModDate;
   FP := ArcFile^.GetPos + P.ExtraField + P.CompressedSize;
   if ((P.GeneralPurpose and 8) <> 0) and (P.CompressedSize = 0) then
     begin
      FPP := FP;
      NullXLAT ( NXL );
      FP := SearchFileStr(@ArcFile^,NXL,'PK'#03#04,FPP,True,False,False,False,False,Nil);{local file header signature}
      if FP < 0 then
        begin
         FP := SearchFileStr(@ArcFile^,NXL,'PK'#01#02,FPP,True,False,False,False,False,Nil);{central file header signature}
         if FP < 0 then begin FileInfo.Last:=2;Exit;end;
        end;
      ArcFile^.Seek(FP - 8);
      ArcFile^.Read(P.CompressedSize, 8);
     end;
   FileInfo.USize := P.OriginalSize;
   FileInfo.PSize := P.CompressedSize;
   ArcFile^.Seek(FP);
  end;
 end;
end.
