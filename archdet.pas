{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.11
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
unit ArchDet;

interface
uses archiver, Arc_Zip, Arc_LHA, arc_RAR, arc_ACE, arc_HA, arc_CAB,
{$IFNDEF MINARCH}
       arc_arc, arc_bsa, arc_bs2, arc_hyp, arc_lim, arc_hpk, arc_TAR, arc_TGZ,
       arc_ZXZ, arc_QRK, arc_UFA, arc_IS3, arc_SQZ, arc_HAP, arc_ZOO, arc_CHZ,
       arc_UC2, arc_AIN,
{$ENDIF}
     profile, objects, advance, advance1, advance2;

function DetectArchive: PARJArchive;
function GetArchiveTagBySign(Sign: TStr4): Byte;
function GetArchiveByTag (ID: Byte): PARJArchive;


implementation

function ZIPDetect: Boolean;
  var i: LongInt;
begin
  ArcFile^.Read(i,4);
  ZIPDetect:=(i=$04034b50);
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
label 1;
var
    M2: MainRAR2Hdr;
    K: Array[1..7] of Char absolute M2;
    M: MainRARHdr absolute M2;
    C: Char;
    L: LongInt;
    S: String;
begin
 RAR2 := Off;
 RARDetect:=False;
 L := ArcFile^.GetPos;
 ArcFile^.Read(K, SizeOf(K));
 if (ArcFile^.Status = stOK) and (M.ID = #$52#$45#$7E#$5E)
    then
     begin
      RARDetect := True;
      ArcFile^.Seek(L + M.HeadLen);
      Exit;
     end;
 if (ArcFile^.Status = stOK) and (K = #$52#$61#$72#$21#$1A#$07#$00)
    then
     begin
      RAR2 := On;
      ArcFile^.Read(M2, SizeOf(M2));
      if M2.HeadType <> $73 then Goto 1;
      ArcFile^.Seek(ArcFile^.GetPos + M2.HeadLen - SizeOf(M2));
      RARDetect := True;
      Exit;
     end;
1:ArcFile^.Seek(ArcPos);
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
   ArcFile^.Seek(ArcPos+P.HeadSize+4);
  end
  else ArcFile^.Seek(ArcPos);
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
    (SizeOf(S) + SizeOf(P) + P.PackedSize <= ArcFile^.GetSize)
    then begin
          HADetect:=True;
          ArcFile^.Seek(ArcPos+4);
         end
    else ArcFile^.Seek(ArcPos);
end;

function ARJDetect: Boolean;
 var I: AWord;
begin
 ArcFile^.Read(I, 2);
{piwamoto.change.begin}
 ARJDetect:=(I=$0ea60);
 if I=$0ea60
    then begin
          ArcFile^.Read(I,2);
          ArcFile^.Seek(ArcPos + I + 10); {skip archive comment}
         end
    else ArcFile^.Seek(ArcPos);
{piwamoto.change.end}
end;

function CABDetect: Boolean;
var
  CFHEADER: TCFHEADER;
begin
  CABDetect := false;
{piwamoto.change.begin}
  ArcFile^.Read(CFHEADER,SizeOf(CFHEADER.signature));
  if (ArcFile^.Status = stOK) and (CFHEADER.signature = 'MSCF') then begin
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
{piwamoto.change.end}
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
 if (ArcFile^.Status = stOK) and (P.Mark = ^Z) and (P.Version < 20) then More;
 ArcFile^.Seek(ArcPos);
end;

Function BSADetect: Boolean;
var
    M: Array[1..4] of Char;
begin
 ArcFile^.Read(M, 4);
 BSADetect:=((ArcFile^.Status = stOK) and (M[4] in [#0,#7]) and (Copy(M,2,2) = #0#$AE));
 ArcFile^.Seek(ArcPos);
end;

Function BS2Detect: Boolean;
var
    M: Array[1..4] of Char;
begin
 ArcFile^.Read(M, 4);
 BS2Detect:=((ArcFile^.Status = stOK) and (M = #$D4#$03'SB'));
 ArcFile^.Seek(ArcPos);
end;

Function HYPDetect: boolean;
var
    M: Array[1..4] of Char;
begin
 ArcFile^.Read(M, 4);
 HYPDetect:=((ArcFile^.Status = stOK) and ((M = ^Z'HP%') OR (M = ^Z'ST%')));
 ArcFile^.Seek(ArcPos);
end;

Function LIMDetect: Boolean;
var
    M: Array[1..8] of Char;
    C: Char;
    L: LongInt;
    S: String;
begin
 LIMDetect:=False;
 ArcFile^.Read(M, 8);
 if (ArcFile^.Status = stOK) and (Copy(M,1,5) = 'LM'#26#8#0)
    then
     begin
      ArcFile^.Read(M, 7);
      if Copy(M, 6, 2) = #35#241 then
       begin
        LIMDetect := True;
        CDir := '';
        ArcFile^.Seek(ArcPos+13);
        Exit;
       end;
     end;
  ArcFile^.Seek(ArcPos);
end;

Function HPKDetect: Boolean;
var
    M: Array[1..4] of Char;
    C: Char;
    L,J,I: LongInt;
    W: Word;
    B: Byte;
    S: String;
    P: Record NumFiles, Margin: LongInt; I,n,f: Byte; S: Array[1..4] of Char; end;
    R: PHPKRec;

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
 ArcFile^.Read(M, 4);
 if (ArcFile^.Status = stOK) and (M = 'HPAK')
    then
     begin
      ArcFile^.Seek(ArcFile^.GetSize - SizeOf(P));
      P.NumFiles := GetLong(On);
      P.Margin := GetLong(On);
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
          R^.Date := GetLong(On);
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
 {piwamoto.change.begin}
  var P: TARHdr;
      SumTar, SumCalc: LongInt;
      i : integer;

   function FromOct(S: String): LongInt;
    var I,L: LongInt;
   begin
     L := 0;
     for I := 0 to Length(S)-1 do
         Inc(L, (Byte(S[Length(S)-I])-48) shl (I * 3));
     FromOct := L;
   end;

 begin
  TARDetect:=False;
  if ArcFile^.GetSize < SizeOf(P) then Exit;
  ArcFile^.Read(P, SizeOf(P));
  for i:=1 to TXT_WORD do if (P.chksum[i] < '0') or (P.chksum[i] > '9') then P.chksum[i] := ' ';
  SumTar := FromOct(DelSpaces(P.chksum));
  P.chksum := '        '; {8 spaces}
  SumCalc := 0;
  for i:=0 to BLKSIZE-1 do SumCalc := SumCalc + mem[Seg(P.FName):Ofs(P.FName)+i];
  TARDetect := (SumTar = SumCalc);
 {piwamoto.change.end}
  ArcFile^.Seek(ArcPos);
 end;

function TGZDetect: boolean;
{changed by piwamoto}
var
    W: AWord;
begin
 ArcFile^.Read(W, SizeOf(W));
 TGZDetect := (ArcFile^.Status = stOK) and
              (W = $8b1f) and
              (InFilter(ArcFileName, '*.TAZ;*.TGZ;*.GZ;*.Z;*.RPM'));
 ArcFile^.Seek(ArcPos);
end;

function ZXZDetect: boolean;
var
    P: ZXZHdr;
    W: Word;
begin
 ArcFile^.Read(P.Name, SizeOf(P.Name) + SizeOf(P.Extension) + SizeOf(P.OriginSize));
 ArcFile^.Read(W, SizeOf(W));
 ZXZDetect:=False;
 ArcFile^.Seek(ArcPos);
 if not ( (P.Extension = 'ZIP') and (W-P.OriginSize<=255) and (W and $0ff = 0) ) then Exit;
 ZXZDetect:=True;
 ArcFile^.Seek(ArcPos+$11);
end;

function QuArkDetect: boolean;
var
  ID : LongInt;
begin
  ArcFile^.Read(ID, SizeOf(ID));
  if (ArcFile^.Status = stOK) and (ID=$100437) then
    begin
      ArcFile^.Seek(ArcPos + 8);
      QuArkDetect := true;
    end
  else
    begin
      ArcFile^.Seek(ArcPos);
      QuArkDetect := false;
    end;
end;

function UFADetect: boolean;
var
  ID : Array[1..3]of Char;
begin
  ArcFile^.Read(ID, SizeOf(ID));
  if (ArcFile^.Status = stOK) and (ID = 'UFA') then
    begin
      ArcFile^.Seek(ArcPos + 8);
      UFADetect := true;
    end
  else
    begin
      ArcFile^.Seek(ArcPos);
      UFADetect := false;
    end;
end;

function IS3Detect: boolean;
var
    IS3Sign: LongInt;
begin
 IS3Detect:=False;
 ArcFile^.Read(IS3Sign, SizeOf(IS3Sign));
 if IS3Sign=$8C655D13
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
          ArcFile^.Seek(ArcPos+8);
         end
    else ArcFile^.Seek(ArcPos);
end;

Function HAPDetect: Boolean;
var
    S: Array[1..4] of Char;
    P: HAHdr;
begin
 ArcFile^.Read(S, 4);
 HAPDetect:=False;
 if (ArcFile^.Status = stOK) and (S = #145#51#72#70)
    then begin HAPDetect:=True; ArcFile^.Seek(ArcPos+14) end
    else ArcFile^.Seek(ArcPos);
end;

Function ZOODetect: Boolean;
label 1;
var
    S: Array[1..3] of Char;
    C: Char;
    L: LongInt;
begin
 ArcFile^.Read(S, 3);
 ZOODetect:=False;
 if (ArcFile^.Status = stOK) and (S = 'ZOO')
    then
     begin
      L := 0;
      repeat Inc(L); ArcFile^.Read(C, 1) until (C = ^Z) or (L = 1024);
      ArcFile^.Read(L,2);
      ArcFile^.Read(L,4);
      if L <> ZOOID then Goto 1;
      ZOODetect := True;
      ArcFile^.Read(L,4);
      ArcFile^.Seek(L);
      Exit;
     end;
1:ArcFile^.Seek(ArcPos);
end;

Function CHZDetect: Boolean;
label 1;
var
    M: Array[1..3] of Char;
    C: Char;
    L: LongInt;
    S: String;
begin
 CHZDetect:=False;
 if ArcPos > 0 then
   begin
    S[0] := #128;
    ArcFile^.Read(S[1],128);
    L := Pos('SChF', S);
    if L = 0 then L := Pos('SChD', S);
    if L = 0 then Goto 1;
    Inc(ArcPos, L-1);
    ArcFile^.Seek(ArcPos);
   end;
 ArcFile^.Read(M, 3);
 if (ArcFile^.Status = stOK) and (M = 'SCh')
    then
     begin
      CHZDetect := True;
      CDir := '';
      ArcFile^.Seek(ArcPos);
      Exit;
     end;
1:ArcFile^.Seek(ArcPos);
end;

Function UC2Detect: Boolean;
var
    M: Array[1..4] of Char;
begin
 UC2Detect:=False;
 ArcFile^.Read(M, 4);
 if (ArcFile^.Status = stOK) and (M = 'UC2'#26) then
     begin
        UC2Detect:= True;
        Exit;
     end;
  ArcFile^.Seek(ArcPos);
end;

Function AINDetect: Boolean;
var
    AinHdr : Array[0..21] of Byte;
    AinSum, ChkSum : Word;
    I : Integer;
begin
 AINdetect:=False;
 ArcFile^.Read(AinHdr, SizeOf(AinHdr));
 ArcFile^.Read(AinSum, SizeOf(AinSum));
 if (ArcFile^.Status = stOK) and
    ((AinHdr[14] + 256*AinHdr[15] + 65536*AinHdr[16] + 16777216*AinHdr[17]) < ArcFile^.GetSize)
   then begin
     ChkSum := 0;
     for I:=0 to 21 do ChkSum := ChkSum + AinHdr[I];
     ChkSum := ChkSum xor $5555;
     AINDetect:=(ChkSum=AinSum);
   end;
 ArcFile^.Seek(ArcPos);
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
{$ENDIF}
 GetArchiveByTag:=nil;
 CloseProfile;
end;

end.
