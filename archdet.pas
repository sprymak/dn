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
//  dn16rc1-improved_HA_AIN_ZXZip_and_SFX_detection_diff138byMV.patch
//  dn16rc1-Archivers_Optimization-diff154byMV.patch
//
//  2.0.0
//  dn223-Archivers_Optimization.patch
//
//  2.3.0
//  dn230-remove_GZip_compression_and_change_external_filter_view.patch
//  dn247-read_ZIP_central_directory.patch
//  dn269-bugfixed_ZIP_detection.patch
//  dn269-GZip_detection.patch
//  dn269-rar3-hp-fix-jo20624a.patch
//
//  2.7.0
//  dn270-more_strict_ZIP_detection.patch
//  dn281-more_strict_ZIP_detection.patch
//
//  3.7.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//  dn40328-7Zip.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
unit ArchDet;

interface
uses archiver, Arc_Zip, Arc_LHA, arc_RAR, arc_ACE, arc_HA, arc_CAB,
{$IFNDEF MINARCH}
       arc_arc, arc_bsa, arc_bs2, arc_hyp, arc_lim, arc_hpk, arc_TAR, arc_TGZ,
       arc_ZXZ, arc_QRK, arc_UFA, arc_IS3, arc_SQZ, arc_HAP, arc_ZOO, arc_CHZ,
       arc_UC2, arc_AIN, arc_7Z,
{$ENDIF}
     profile, objects, advance, advance1, advance2;

function DetectArchive: PARJArchive;
function GetArchiveTagBySign(Sign: TStr4): Byte;
function GetArchiveByTag (ID: Byte): PARJArchive;


implementation

uses FViewer, U_Keymap;

function ZIPDetect: Boolean;
var
 ID, FP: LongInt;
    nxl: TXLat;
begin
  ZIPDetect := False;
  CentralDirRecPresent := False;
  ArcFile^.Read(ID, SizeOf(ID));
{somewhat lame code: check for span archive}
{todo: look VC 4.99a8}
  ArcFile^.Seek(ArcFile^.GetSize - 22);
  ArcFile^.Read(FP, SizeOf(FP));
  if (ID = $04034b50) or ((FP = $06054b50) and (ArcFile^.GetPos > 4)) then
  begin
    if ID = $04034b50 then ZIPDetect := True;
    NullXLAT (NXL);
    FP := ArcFile^.GetPos;
    repeat
     FP := SearchFileStr(@ArcFile^, NXL, 'PK', FP, False{piwamoto:we need it FALSE}, False, False, True, False, Nil);
     ArcFile^.Seek(FP);
     ArcFile^.Read(ID, SizeOf(ID));
    until (FP < 0) or
          (ArcFile^.GetSize - FP > $10016) or {bug: it wouldn't work with files without 'pk' string}
          (ID = $02014B50) or
          (ID = $04034B50) or
          (ID = $06054B50) or
          (ID = $06064B50) or
          (ArcFile^.Status <> stOK);
    if (ID = $06054B50) or (ID = $06064B50) then
      begin {central directory found}
        ArcFile^.Seek(FP + 16 + 32*Byte(ID = $06064B50));{offset of start of central directory}
        ArcFile^.Read(FP, 4);
        ArcFile^.Seek(FP);
        ArcFile^.Read(ID, SizeOf(ID));
        if ID = $02014B50 then
          begin {piwamoto: only if offset to Central Directory is correct}
           ArcPos := FP;
           ZIPDetect := True;
           CentralDirRecPresent := True;
          end;
      end;
  end;
  ArcFile^.Seek(ArcPos);
end;

function LHADetect: Boolean;
var
    P: LHAHdr;
begin
 ArcFile^.Read(P, SizeOf(P) - SizeOf(P.Name));
 LHADetect:=((Pos(P.MethodID,'-lh0--lh1--lh2--lh3--lh4--lh5--lh6--lh7--lzs--lz5--lz4--lhd-')+4) mod 5 = 0);
 ArcFile^.Seek(ArcPos);
end;

Function RARDetect: Boolean;
var
 ID: LongInt;
 M2: MainRAR2Hdr;
begin
 RAR2 := False;
 RARDetect:=False;
 ArcFile^.Read(ID, SizeOf(ID));
 if (ArcFile^.Status = stOK) and (ID = $5e7e4552{'RE~^'}{RAR until v1.50})
    then
     begin
      RARDetect := True;
      ArcFile^.Read(ID, 2);{MainRARHdr.HeadLen: AWord;}
      ArcPos := ArcPos + ID and $ffff;
     end;
 if (ArcFile^.Status = stOK) and (ID = $21726152{Rar!}{RAR 1.50+})
    then
     begin
      ArcFile^.Read(ID, 3);{skip 3 bytes}
      ArcFile^.Read(M2, SizeOf(M2));
      if M2.HeadType = $73 then
        begin
          RARDetect := True;
          RAR2 := True;
          ArcPos := ArcPos + M2.HeadLen + 7{Rar ID};
          if M2.HeadFlags and $80 <> 0 {headers are encrypted} then
             ArcPos := ArcFile^.GetSize;
        end;
     end;
 ArcFile^.Seek(ArcPos);
end;

function ACEDetect: Boolean;
var
    ACESign: Array [0..6] of Char;
    P: ACEFileHdr;
begin
 ACEDetect:=False;
 ArcFile^.Read(P, 7);
 ArcFile^.Read(ACESign, SizeOf(ACESign));
 if (ACESign='**ACE**') and (P.HeadType=0) then
  begin
   ACEDetect:=True;
   ArcPos := ArcPos + P.HeadSize + 4;
  end;
 ArcFile^.Seek(ArcPos);
end;

Function HADetect: Boolean;
var
    S: Array[0..3] of Char;
    P: HAHdr;
begin
 HADetect:=False;
 ArcFile^.Read(S, SizeOf(S));
 ArcFile^.Read(P, SizeOf(P));
 if (ArcFile^.Status = stOK) and
    (S[0] = 'H') and
    (S[1] = 'A') and
    (_Cardinal(SizeOf(S) + SizeOf(P) + P.PackedSize) < ArcFile^.GetSize)
    then begin
          HADetect:=True;
          ArcPos := ArcPos + 4;
         end;
 ArcFile^.Seek(ArcPos);
end;

function ARJDetect: Boolean; {fixed by piwamoto}
 var ID, CommentLen: AWord;
begin
 ARJDetect := False;
 ArcFile^.Read(ID, 2);
 if ID = $0ea60 then
  begin
   ArcFile^.Read(CommentLen, 2);
   ArcFile^.Seek(ArcPos + CommentLen + 10); {skip archive comment}
   ArcFile^.Read(ID, 2);
   if (ID = $0ea60) and (ArcFile^.Status = stOK) then
    begin
     ArcPos := ArcPos + CommentLen + 10;
     ARJDetect := True;
    end;
  end;
 ArcFile^.Seek(ArcPos);
end;

function CABDetect: Boolean;
var
  CFHEADER: TCFHEADER;
begin
  CABDetect := false;
  ArcFile^.Read(CFHEADER,SizeOf(CFHEADER.signature));
  if (ArcFile^.Status = stOK) and (CFHEADER.signature = $4643534d {'MSCF'})
   then begin
   ArcFile^.Read(CFHEADER.reserved1, SizeOf(CFHEADER) - SizeOf(CFHEADER.signature));
   if (CFHeader.cbCabinet > sizeof(TCFHeader)) and
      (CFHeader.coffFiles > sizeof(TCFHeader)) and
      (CFHeader.coffFiles < LongInt($ffff)) and
      (CFHeader.versionMajor > 0) and
      (CFHeader.versionMajor < $20 ) and
      (CFHeader.cFolders > 0) then
    CABDetect := true;
  end;
  ArcFile^.Seek(ArcPos);
end;

{$IFNDEF MINARCH}
function ARCDetect: Boolean;
var P: ARCHdr;

procedure More;
var i: byte;
    c: char;
begin
  i := 0;
  while i<13 do
  begin
    Inc(i);
    c := P.Name[i];
    if c = #0 then Break;
    if c <#32 then Exit;
  end;
  if i <= 2 then Exit;
  if P.PackedSize<0 then Exit;
  if P.OriginSize<0 then Exit;
  ARCDetect := True;
end;

begin
 ArcFile^.Read(P,SizeOf(P));
 ARCDetect := False;
 if (ArcFile^.Status = stOK) and (P.Mark = $1a{^Z}) and (P.Version < 20) then More;
 ArcFile^.Seek(ArcPos);
end;

Function BSADetect: Boolean;
var
    M: Array[1..4] of Char;
begin
 ArcFile^.Read(M, 4);
 BSADetect:=((ArcFile^.Status = stOK) and (M[4] in [#0,#7]) and (M[2]=#0) and (M[3]=#$AE));
 ArcFile^.Seek(ArcPos);
end;

Function BS2Detect: Boolean;
var
 ID: LongInt;
begin
 ArcFile^.Read(ID, SizeOf(ID));
 BS2Detect:=((ArcFile^.Status = stOK) and (ID = $425303D4));
 ArcFile^.Seek(ArcPos);
end;

Function HYPDetect: boolean;
var
 ID: LongInt;
begin
 ArcFile^.Read(ID, SizeOf(ID));
 HYPDetect:=((ArcFile^.Status = stOK) and ((ID = $2550481A{^Z'HP%'}) OR (ID = $2554531A {^Z'ST%'})));
 ArcFile^.Seek(ArcPos);
end;

Function LIMDetect: Boolean;
var
    M: Array[1..8] of Char;
begin
 LIMDetect:=False;
 ArcFile^.Read(M, 8);
 if (ArcFile^.Status = stOK) and (Copy(M,1,5) = 'LM'#26#8#0)
   then begin
     ArcFile^.Read(M, 7);
     if (M[6] = #35) and (M[7] = #241) then
       begin
         LIMDetect := True;
         ArcPos:=ArcPos + 13;
       end;
     end;
 ArcFile^.Seek(ArcPos);
end;

Function HPKDetect: Boolean;
var
    C: Char;
    I: LongInt;
    W,J: Word;
    P: Record NumFiles, Margin: LongInt; I,n,f: Byte; S: Array[1..4] of Char; end;
    R: PHPKRec;
    S: String;

  function GetLong(Num4: Boolean): LongInt;
   var A: LongInt;
       R: Record A1, A2, A3, A4: Byte end;
  begin
   ArcFile^.Read(R, 2 + 2*Byte(Num4));
   if Num4 then A := LongInt(R.A1) shl 24 + LongInt(R.A2) shl 16 +
                     LongInt(R.A3) shl 8 + LongInt(R.A4)
           else A := R.A1 * 256 + R.A2;
   GetLong := A;
  end;

begin
 HPKDetect:=False;
 ArcFile^.Read(I, SizeOf(I));
 if (ArcFile^.Status = stOK) and (I = $4B415048{'HPAK'})
    then
     begin
      ArcFile^.Seek(ArcFile^.GetSize - SizeOf(P));
      P.NumFiles := GetLong(True);
      P.Margin := GetLong(True);
      ArcFile^.Read(P.I,7);
      if P.S = 'HPAK' then
       begin
        ArcFile^.Seek(ArcFile^.GetSize - SizeOf(P) - P.Margin);
        HPKDetect := True;
        New(HPKCol, Init(P.NumFiles,10));
        for I := 1 to P.NumFiles do
         begin
          New(R); R^.Name := nil;
          ArcFile^.Read(W,2);
          if W and $10 <> 0 then ArcFile^.Read(J,2);
          R^.Date := GetLong(True);
          R^.USize := GetLong(W and $0080 <> 0);
          R^.PSize := GetLong(W and $0040 <> 0);
          HPKCol^.Insert(R);
         end;
        for I := 0 to P.NumFiles - 1 do
         begin
          S := '';
          repeat ArcFile^.Read(C, 1); if C <> #0 then S := S + C until C = #0;
          PHPKRec(HPKCol^.At(I))^.Name := NewStr(S);
         end;
        Exit;
       end;
     end;
  ArcFile^.Seek(ArcPos);
end;

Function TARDetect: Boolean;
  var
      SumTar, SumCalc: LongInt;
      i : integer;
      Buffer: Array [0..BlkSize - 1] of Char;
      P: TARHdr absolute Buffer;
begin
  TARDetect:=False;
  if ArcFile^.GetSize < SizeOf(P) then Exit;
  ArcFile^.Read(P, SizeOf(P));
  SumTar := FromOct(P.chksum);
  P.chksum := '        '; {8 spaces}
  SumCalc := 0;
  for i:=0 to BLKSIZE-1 do SumCalc := SumCalc + Byte(Buffer[i]);
  TARDetect := (SumTar = SumCalc);
  ArcFile^.Seek(ArcPos);
end;

function TGZDetect: boolean;
var
    W: AWord;
begin
 ArcFile^.Read(W, SizeOf(W));
 TGZDetect := (ArcFile^.Status = stOK) and
              ((W = $8b1f) or (W = $9d1f))
{              and (InFilter(ArcFileName, '*.TAR;*.TAZ;*.TGZ;*.GZ;*.Z'))}
              ;
 ArcFile^.Seek(ArcPos);
end;

function ZXZDetect: boolean;
var
    P: ZXZHdr;
    W: AWord;
begin
 ArcFile^.Read(P.Name, SizeOf(P.Name) + SizeOf(P.Extension) + SizeOf(P.OriginSize));
 ArcFile^.Read(W, SizeOf(W));
 ZXZDetect:=False;
 if (P.Extension = 'ZIP') and (W-P.OriginSize<=255) and
    (W and $0ff = 0) and (ArcFile^.Status = stOK) then
   begin
    ArcPos := ArcPos + $11;
    ZXZDetect:=True;
   end;
 ArcFile^.Seek(ArcPos);
end;

function QuArkDetect: boolean;
var
 ID: LongInt;
begin
  QuArkDetect := false;
  ArcFile^.Read(ID, SizeOf(ID));
  if (ArcFile^.Status = stOK) and (ID=$100437) then
    begin
      ArcPos := ArcPos + 8;
      QuArkDetect := true;
    end;
  ArcFile^.Seek(ArcPos);
end;

function UFADetect: boolean;
var
 ID: LongInt;
begin
  UFADetect := false;
  ID := 0;
  ArcFile^.Read(ID, 3);{only 3 bytes!}
  if (ArcFile^.Status = stOK) and (ID = $414655{'UFA'}) then
    begin
      ArcFile^.Read(ID, SizeOf(ID));
      if (ID and $1000000) = 0 then {archive <> solid}
        begin
          ArcPos := ArcPos + 8;
          UFADetect := true;
        end;
    end;
  ArcFile^.Seek(ArcPos);
end;

function IS3Detect: boolean;
var
 ID: LongInt;
begin
 IS3Detect:=False;
 ArcFile^.Read(ID, SizeOf(ID));
 if ID = $8C655D13
  then IS3Detect:=True
  else ArcFile^.Seek(ArcPos);
end;

function SQZDetect: Boolean;
var
    S: Array[0..4] of Char;
begin
 ArcFile^.Read(S, 5);
 SQZDetect := False;
 if (ArcFile^.Status = stOK) and (S = 'HLSQZ')
    then begin
          SQZDetect := True;
          ArcPos := ArcPos + 8;
         end;
 ArcFile^.Seek(ArcPos);
end;

Function HAPDetect: Boolean;
var
 ID: LongInt;
begin
 HAPDetect:=False;
 ArcFile^.Read(ID, SizeOf(ID));
 if (ArcFile^.Status = stOK) and (ID = $46483391)
   then begin
     HAPDetect:=True;
     ArcPos := ArcPos + 14;
   end;
 ArcFile^.Seek(ArcPos);
end;

Function ZOODetect: Boolean;
var
 ID: LongInt;
begin
 ZOODetect:=False;
 ArcFile^.Read(ID, SizeOf(ID));
 if (ArcFile^.Status = stOK) and (ID = $FDC4A7DC) then
   begin
    ArcFile^.Read(ArcPos,SizeOf(ArcPos));
    ZOODetect := True;
   end;
 ArcFile^.Seek(ArcPos);
end;

Function CHZDetect: Boolean;
var
 ID: LongInt;
begin
 CHZDetect:=False;
 ID := 0;
 ArcFile^.Read(ID, 3);{last byte isn't needed, coz it checked in Check4ArcId}
 if (ArcFile^.Status = stOK) and (ID = $684353{'SCh'}) then CHZDetect := True;
 ArcFile^.Seek(ArcPos);
end;

Function UC2Detect: Boolean;
var
 ID: LongInt;
begin
 UC2Detect:=False;
 ArcFile^.Read(ID, SizeOf(ID));
 if (ArcFile^.Status = stOK) and (ID = $1A324355{'UC2'#26})
   then UC2Detect := True
   else ArcFile^.Seek(ArcPos);
end;

Function AINDetect: Boolean;
var
    AinHdr : Array[0..21] of Byte;
    I, AinSum, ChkSum : AWord;
begin
 AINdetect:=False;
 ArcFile^.Read(AinHdr, SizeOf(AinHdr));
 ArcFile^.Read(AinSum, SizeOf(AinSum));
 if (ArcFile^.Status = stOK) and
    (_Cardinal(AinHdr[14] + 256*AinHdr[15] + 65536*AinHdr[16] + 16777216*AinHdr[17]) < ArcFile^.GetSize)
   then begin
     ChkSum := 0;
     for I:=0 to 21 do ChkSum := ChkSum + AinHdr[I];
     AINDetect:=(ChkSum=(AinSum xor $5555));
   end;
 ArcFile^.Seek(ArcPos);
end;

Function S7ZDetect: Boolean;
var
 ID: LongInt;
begin
 S7ZDetect:=False;
 ArcFile^.Read(ID, SizeOf(ID));
 if (ArcFile^.Status = stOK) and (ID = $AFBC7A37)
   then S7ZDetect := True
   else ArcFile^.Seek(ArcPos);
end;
{$ENDIF}

function DetectArchive;
begin
 if ACEDetect   then DetectArchive:=New(PACEArchive,  Init) else
 if ARJDetect   then DetectArchive:=New(PARJArchive,  Init) else
 if CABDetect   then DetectArchive:=New(PCABArchive,  Init) else
 if  HADetect   then DetectArchive:=New(PHAArchive,   Init) else
 if LHADetect   then DetectArchive:=New(PLHAArchive,  Init) else
 if RARDetect   then DetectArchive:=New(PRARArchive,  Init) else
 if ZIPDetect   then DetectArchive:=New(PZIPArchive,  Init) else
{$IFNDEF MINARCH}
 if AINDetect   then DetectArchive:=New(PAINArchive,  Init) else
 if ARCDetect   then DetectArchive:=New(PARCArchive,  Init) else
 if BS2Detect   then DetectArchive:=New(PBS2Archive,  Init) else
 if BSADetect   then DetectArchive:=New(PBSAArchive,  Init) else
 if CHZDetect   then DetectArchive:=New(PCHZArchive,  Init) else
 if HAPDetect   then DetectArchive:=New(PHAPArchive,  Init) else
 if HPKDetect   then DetectArchive:=New(PHPKArchive,  Init) else
 if HYPDetect   then DetectArchive:=New(PHYPArchive,  Init) else
 if LIMDetect   then DetectArchive:=New(PLIMArchive,  Init) else
 if QuarkDetect then DetectArchive:=New(PQuarkArchive,Init) else
 if SQZDetect   then DetectArchive:=New(PSQZArchive,  Init) else
 if TARDetect   then DetectArchive:=New(PTARArchive,  Init) else
 if TGZDetect   then DetectArchive:=New(PTGZArchive,  Init) else
 if UC2Detect   then DetectArchive:=New(PUC2Archive,  Init) else
 if UFADetect   then DetectArchive:=New(PUFAArchive,  Init) else
 if ZOODetect   then DetectArchive:=New(PZOOArchive,  Init) else
 if ZXZDetect   then DetectArchive:=New(PZXZArchive,  Init) else
 if IS3Detect   then DetectArchive:=New(PIS3Archive,  Init) else
 if S7ZDetect   then DetectArchive:=New(PS7ZArchive,  Init) else
{$ENDIF}
 DetectArchive:=nil;
 CloseProfile;
end;

function GetArchiveTagBySign;
begin
  if sign=sigACE   then GetArchiveTagBySign := arcACE   else
  if sign=sigARJ   then GetArchiveTagBySign := arcARJ   else
  if sign=sigCAB   then GetArchiveTagBySign := arcCAB   else
  if sign=sigHA    then GetArchiveTagBySign := arcHA    else
  if sign=sigLHA   then GetArchiveTagBySign := arcLHA   else
  if sign=sigRAR   then GetArchiveTagBySign := arcRAR   else
  if sign=sigZIP   then GetArchiveTagBySign := arcZIP   else
{$IFNDEF MINARCH}
  if sign=sigAIN   then GetArchiveTagBySign := arcAIN   else
  if sign=sigARC   then GetArchiveTagBySign := arcARC   else
  if sign=sigBS2   then GetArchiveTagBySign := arcBS2   else
  if sign=sigBSA   then GetArchiveTagBySign := arcBSA   else
  if sign=sigCHZ   then GetArchiveTagBySign := arcCHZ   else
  if sign=sigHAP   then GetArchiveTagBySign := arcHAP   else
  if sign=sigHPK   then GetArchiveTagBySign := arcHPK   else
  if sign=sigHYP   then GetArchiveTagBySign := arcHYP   else
  if sign=sigLIM   then GetArchiveTagBySign := arcLIM   else
  if sign=sigQUARK then GetArchiveTagBySign := arcQUARK else
  if sign=sigSQZ   then GetArchiveTagBySign := arcSQZ   else
  if sign=sigTAR   then GetArchiveTagBySign := arcTAR   else
  if sign=sigTGZ   then GetArchiveTagBySign := arcTGZ   else
  if sign=sigUC2   then GetArchiveTagBySign := arcUC2   else
  if sign=sigUFA   then GetArchiveTagBySign := arcUFA   else
  if sign=sigZOO   then GetArchiveTagBySign := arcZOO   else
  if sign=sigZXZ   then GetArchiveTagBySign := arcZXZ   else
  if sign=sigIS3   then GetArchiveTagBySign := arcIS3   else
  if sign=sig7Z    then GetArchiveTagBySign := arc7Z    else
{$ENDIF}
  GetArchiveTagBySign := arcUNK;
end;

function GetArchiveByTag;
begin
 if ID=arcACE   then GetArchiveByTag:=New(PACEArchive,  Init) else
 if ID=arcARJ   then GetArchiveByTag:=New(PARJArchive,  Init) else
 if ID=arcCAB   then GetArchiveByTag:=New(PCABArchive,  Init) else
 if ID=arcHA    then GetArchiveByTag:=New(PHAArchive,   Init) else
 if ID=arcLHA   then GetArchiveByTag:=New(PLHAArchive,  Init) else
 if ID=arcRAR   then GetArchiveByTag:=New(PRARArchive,  Init) else
 if ID=arcZIP   then GetArchiveByTag:=New(PZIPArchive,  Init) else
{$IFNDEF MINARCH}
 if ID=arcAIN   then GetArchiveByTag:=New(PAINArchive,  Init) else
 if ID=arcARC   then GetArchiveByTag:=New(PARCArchive,  Init) else
 if ID=arcBS2   then GetArchiveByTag:=New(PBS2Archive,  Init) else
 if ID=arcBSA   then GetArchiveByTag:=New(PBSAArchive,  Init) else
 if ID=arcCHZ   then GetArchiveByTag:=New(PCHZArchive,  Init) else
 if ID=arcHAP   then GetArchiveByTag:=New(PHAPArchive,  Init) else
 if ID=arcHPK   then GetArchiveByTag:=New(PHPKArchive,  Init) else
 if ID=arcHYP   then GetArchiveByTag:=New(PHYPArchive,  Init) else
 if ID=arcLIM   then GetArchiveByTag:=New(PLIMArchive,  Init) else
 if ID=arcQUARK then GetArchiveByTag:=New(PQuarkArchive,Init) else
 if ID=arcSQZ   then GetArchiveByTag:=New(PSQZArchive,  Init) else
 if ID=arcTAR   then GetArchiveByTag:=New(PTARArchive,  Init) else
 if ID=arcTGZ   then GetArchiveByTag:=New(PTGZArchive,  Init) else
 if ID=arcUC2   then GetArchiveByTag:=New(PUC2Archive,  Init) else
 if ID=arcUFA   then GetArchiveByTag:=New(PUFAArchive,  Init) else
 if ID=arcZOO   then GetArchiveByTag:=New(PZOOArchive,  Init) else
 if ID=arcZXZ   then GetArchiveByTag:=New(PZXZArchive,  Init) else
 if ID=arcIS3   then GetArchiveByTag:=New(PIS3Archive,  Init) else
 if ID=arc7Z    then GetArchiveByTag:=New(PS7ZArchive,  Init) else
{$ENDIF}
 GetArchiveByTag:=nil;
 CloseProfile;
end;

end.
