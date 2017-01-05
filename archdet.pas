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
unit ArchDet;

interface

uses
  Archiver, Arc_Zip, Arc_LHA, arc_RAR, arc_ACE, arc_HA, arc_CAB,
  {$IFNDEF MINARCH}
  arc_arc, arc_bsa, arc_bs2, arc_hyp, arc_lim, arc_hpk, arc_TAR,
    arc_TGZ,
  arc_ZXZ, arc_QRK, arc_UFA, arc_IS3, arc_SQZ, arc_HAP, arc_ZOO,
    arc_CHZ,
  arc_UC2, arc_AIN,
  {$ENDIF}
  profile, Objects, advance, advance1, advance2;

function DetectArchive: PARJArchive;
function GetArchiveTagBySign(Sign: TStr4): byte;
function GetArchiveByTag(Id: byte): PARJArchive;

implementation

uses{$IFDEF PLUGIN}
  Plugin, {$ENDIF}Messages,
  FViewer, U_KeyMap, DNApp, Commands; {JO}

function ZIPDetect: boolean;
  var
    Id, fp: longInt;
    nxl: TXlat;
  begin
    ZIPDetect := False;
    CentralDirRecPresent := False;
    ArcFile^.Read(Id, SizeOf(Id));
    {somewhat lame code: check for span archive}
    {todo: look VC 4.99a8}
    ArcFile^.Seek(ArcFile^.GetSize-22);
    ArcFile^.Read(fp, SizeOf(fp));
    if (Id = $04034b50) or ((fp = $06054b50) and (ArcFile^.GetPos > 4))
    then
      begin
        if Id = $04034b50 then
          ZIPDetect := True;
        NullXLAT(nxl);
        fp := ArcFile^.GetPos;
        repeat
          fp := SearchFileStr(@ArcFile^, nxl, 'PK', fp, False
            {piwamoto:we need it OFF}, True, False, True, False,
            False);
          ArcFile^.Seek(fp);
          ArcFile^.Read(Id, SizeOf(Id));
        until (fp < 0) or
        (ArcFile^.GetSize-fp > $10016) or
          {bug: it wouldn't work with files without 'pk' string}
        (Id = $02014B50) or
        (Id = $04034B50) or
        (Id = $06054B50) or
        (Id = $06064B50) or
        (ArcFile^.Status <> stOK);
        if (Id = $06054B50) or (Id = $06064B50) then
          begin{central directory found}
            ArcFile^.Seek(fp+16+32*byte(Id = $06064B50));
              {offset of start of central directory}
            ArcFile^.Read(fp, 4);
            ArcFile^.Seek(fp);
            ArcFile^.Read(Id, SizeOf(Id));
            if Id = $02014B50 then
              begin
                  {piwamoto: only if offset to Central Directory is correct}
                ArcPos := fp;
                ZIPDetect := True;
                CentralDirRecPresent := True;
              end
            else
              MsgHelpCtx := hcZIPWithoutCentralDir;
          end
        else
          MsgHelpCtx := hcZIPWithoutCentralDir;
      end;
    ArcFile^.Seek(ArcPos);
  end { ZIPDetect: };

function LHADetect: boolean;
  var
    P: LHAHdr;
  begin
    ArcFile^.Read(P, SizeOf(P)-SizeOf(P.Name));
    LHADetect := ((Pos(P.MethodID,
      '-lh0--lh1--lh2--lh3--lh4--lh5--lh6--lh7--lzs--lz5--lz4--lhd-')+4)
      mod 5 = 0);
    ArcFile^.Seek(ArcPos);
  end;

function RARDetect: boolean;
  var
    Id: longInt;
    M2: MainRAR2Hdr;
  begin
    RAR2 := False;
    Encrypted := False;
    RARDetect := False;
    ArcFile^.Read(Id, SizeOf(Id));
    if (ArcFile^.Status = stOK) and (Id = $5e7e4552 {'RE~^'}
        {RAR until v1.50})
    then
      begin
        RARDetect := True;
        ArcFile^.Read(Id, 2); {MainRARHdr.HeadLen: AWord;}
        ArcPos := ArcPos+Id and $ffff;
      end;
    if (ArcFile^.Status = stOK) and (Id = $21726152 {Rar!}
        {RAR 1.50+})
    then
      begin
        ArcFile^.Read(Id, 3); {skip 3 bytes}
        ArcFile^.Read(M2, SizeOf(M2));
        if M2.HeadType = $73 then
          begin
            RARDetect := True;
            RAR2 := True;
            ArcPos := ArcPos+M2.HeadLen+7 {Rar ID};
            if M2.HeadFlags and $80 <> 0 {headers are encrypted}
            then
              begin
                Encrypted := True;
                ArcPos := ArcFile^.GetSize;
              end;
          end;
      end;
    ArcFile^.Seek(ArcPos);
  end { RARDetect: };

function ACEDetect: boolean;
  var
    ACESign: array[0..6] of Char;
    P: ACEFileHdr;
  begin
    ACEDetect := False;
    ArcFile^.Read(P, 7);
    ArcFile^.Read(ACESign, SizeOf(ACESign));
    ArcFile^.Read(ACEVerToExtr, 1);
    if (ACESign = '**ACE**') and (P.HeadType = 0) then
      begin
        ACEDetect := True;
        ArcPos := ArcPos+P.HeadSize+4;
      end;
    ArcFile^.Seek(ArcPos);
  end;

function HADetect: boolean;
  var
    s: array[0..3] of Char;
    P: HAHdr;
  begin
    HADetect := False;
    ArcFile^.Read(s, SizeOf(s));
    ArcFile^.Read(P, SizeOf(P));
    if (ArcFile^.Status = stOK) and
      (s[0] = 'H') and
      (s[1] = 'A') and
      (_Cardinal(SizeOf(s)+SizeOf(P)+P.PackedSize) < ArcFile^.
        GetSize)
    then
      begin
        HADetect := True;
        ArcPos := ArcPos+4;
      end;
    ArcFile^.Seek(ArcPos);
  end;

function ARJDetect: boolean; {fixed by piwamoto}
  var
    Id, CommentLen: AWord;
  begin
    ARJDetect := False;
    ArcFile^.Read(Id, 2);
    if Id = $0ea60 then
      begin
        ArcFile^.Read(CommentLen, 2);
        ArcFile^.Seek(ArcPos+CommentLen+10); {skip archive comment}
        ArcFile^.Read(Id, 2);
        if (Id = $0ea60) and (ArcFile^.Status = stOK) then
          begin
            ArcPos := ArcPos+CommentLen+10;
            ARJDetect := True;
          end;
      end;
    ArcFile^.Seek(ArcPos);
  end;

function CABDetect: boolean;
  var
    CFHEADER: TCFHEADER;
  begin
    CABDetect := False;
    ArcFile^.Read(CFHEADER, SizeOf(CFHEADER.signature));
    if (ArcFile^.Status = stOK) and (CFHEADER.signature = $4643534d
        {'MSCF'})
    then
      begin
        ArcFile^.Read(CFHEADER.reserved1, SizeOf(CFHEADER)-SizeOf(
          CFHEADER.signature));
        if (CFHEADER.cbCabinet > SizeOf(TCFHEADER)) and
          (CFHEADER.coffFiles > SizeOf(TCFHEADER)) and
          (CFHEADER.coffFiles < longInt($ffff)) and
          (CFHEADER.versionMajor > 0) and
          (CFHEADER.versionMajor < $20) and
          (CFHEADER.cFolders > 0)
        then
          CABDetect := True;
      end;
    ArcFile^.Seek(ArcPos);
  end { CABDetect: };

{$IFNDEF MINARCH}
function ARCDetect: boolean;
  var
    P: ARCHdr;

  procedure More;
    var
      i: byte;
      C: Char;
    begin
      i := 0;
      while i < 13 do
        begin
          Inc(i);
          C := P.Name[i];
          if C = #0 then
            break;
          if C < #32 then
            exit;
        end;
      if i <= 2 then
        exit;
      if P.PackedSize < 0 then
        exit;
      if P.OriginSize < 0 then
        exit;
      ARCDetect := True;
    end { More };

  begin { ARCDetect: }
    ArcFile^.Read(P, SizeOf(P));
    ARCDetect := False;
    if (ArcFile^.Status = stOK) and (P.Mark = $1a {^Z}) and (P.
        Version < 20)
    then
      More;
    ArcFile^.Seek(ArcPos);
  end { ARCDetect: };

function BSADetect: boolean;
  var
    M: array[1..4] of Char;
  begin
    ArcFile^.Read(M, 4);
    BSADetect := ((ArcFile^.Status = stOK) and (M[4] in [#0, #7])
      and (M[2] = #0) and (M[3] = #$AE));
    ArcFile^.Seek(ArcPos);
  end;

function BS2Detect: boolean;
  var
    Id: longInt;
  begin
    ArcFile^.Read(Id, SizeOf(Id));
    BS2Detect := ((ArcFile^.Status = stOK) and (Id = $425303D4));
    ArcFile^.Seek(ArcPos);
  end;

function HYPDetect: boolean;
  var
    Id: longInt;
  begin
    ArcFile^.Read(Id, SizeOf(Id));
    HYPDetect := ((ArcFile^.Status = stOK) and ((Id = $2550481A
      {^Z'HP%'}) or (Id = $2554531A {^Z'ST%'})));
    ArcFile^.Seek(ArcPos);
  end;

function LIMDetect: boolean;
  var
    M: array[1..8] of Char;
  begin
    LIMDetect := False;
    ArcFile^.Read(M, 8);
    if (ArcFile^.Status = stOK) and (Copy(M, 1, 5) = 'LM'#26#8#0)
    then
      begin
        ArcFile^.Read(M, 7);
        if (M[6] = #35) and (M[7] = #241) then
          begin
            LIMDetect := True;
            ArcPos := ArcPos+13;
          end;
      end;
    ArcFile^.Seek(ArcPos);
  end;

function HPKDetect: boolean;
  var
    C: Char;
    i: longInt;
    W, j: AWord;
    P: record
      NumFiles, Margin: longInt;
      i, n, F: byte;
      s: array[1..4] of Char;
      end;
    R: PHPKRec;
    s: String;

  function GetLong(Num4: boolean): longInt;
    var
      A: longInt;
      R: record
        A1, A2, A3, A4: byte
        end;
    begin
      ArcFile^.Read(R, 2+2*byte(Num4));
      if Num4 then
        A := longInt(R.A1) shl 24+longInt(R.A2) shl 16+
        longInt(R.A3) shl 8+longInt(R.A4)
      else
        A := R.A1*256+R.A2;
      GetLong := A;
    end;

  begin { HPKDetect: }
    HPKDetect := False;
    ArcFile^.Read(i, SizeOf(i));
    if (ArcFile^.Status = stOK) and (i = $4B415048 {'HPAK'})
    then
      begin
        ArcFile^.Seek(ArcFile^.GetSize-SizeOf(P));
        P.NumFiles := GetLong(True);
        P.Margin := GetLong(True);
        ArcFile^.Read(P.i, 7);
        if P.s = 'HPAK' then
          begin
            ArcFile^.Seek(ArcFile^.GetSize-SizeOf(P)-P.Margin);
            HPKDetect := True;
            New(HPKCol, Init(P.NumFiles, 10));
            for i := 1 to P.NumFiles do
              begin
                New(R);
                R^.Name := nil;
                ArcFile^.Read(W, 2);
                if W and $10 <> 0 then
                  ArcFile^.Read(j, 2);
                R^.Date := GetLong(True);
                R^.USize := GetLong(W and $0080 <> 0);
                R^.PSize := GetLong(W and $0040 <> 0);
                HPKCol^.Insert(R);
              end;
            for i := 0 to P.NumFiles-1 do
              begin
                s := '';
                repeat
                  ArcFile^.Read(C, 1);
                  if C <> #0 then
                    s := s+C
                until C = #0;
                PHPKRec(HPKCol^.At(i))^.Name := NewStr(s);
              end;
            exit;
          end;
      end;
    ArcFile^.Seek(ArcPos);
  end { HPKDetect: };

function TARDetect: boolean;
  var
    SumTar, SumCalc: longInt;
    i: integer;
    Buffer: array[0..BlkSize-1] of Char;
    P: TARHdr absolute Buffer;
  begin
    TARDetect := False;
    if ArcFile^.GetSize < SizeOf(P) then
      exit;
    ArcFile^.Read(P, SizeOf(P));
    SumTar := FromOct(P.chksum);
    P.chksum := '        '; {8 spaces}
    SumCalc := 0;
    for i := 0 to BlkSize-1 do
      SumCalc := SumCalc+byte(Buffer[i]);
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
    {              and (InFilter(VArcFileName, '*.TAR;*.TAZ;*.TGZ;*.GZ;*.Z'))}
    ;
    ArcFile^.Seek(ArcPos);
  end;

function ZXZDetect: boolean;
  var
    P: ZXZHdr;
    W: AWord;
  begin
    ArcFile^.Read(P.Name, SizeOf(P.Name)+SizeOf(P.Extension)+SizeOf(P.
      OriginSize));
    ArcFile^.Read(W, SizeOf(W));
    ZXZDetect := False;
    if (P.Extension = 'ZIP') and (W-P.OriginSize <= 255) and
      (W and $0ff = 0) and (ArcFile^.Status = stOK)
    then
      begin
        ArcPos := ArcPos+$11;
        ZXZDetect := True;
      end;
    ArcFile^.Seek(ArcPos);
  end;

function QuArkDetect: boolean;
  var
    Id: longInt;
  begin
    QuArkDetect := False;
    ArcFile^.Read(Id, SizeOf(Id));
    if (ArcFile^.Status = stOK) and (Id = $100437) then
      begin
        ArcPos := ArcPos+8;
        QuArkDetect := True;
      end;
    ArcFile^.Seek(ArcPos);
  end;

function UFADetect: boolean;
  var
    Id: longInt;
  begin
    UFADetect := False;
    Id := 0;
    ArcFile^.Read(Id, 3); {only 3 bytes!}
    if (ArcFile^.Status = stOK) and (Id = $414655 {'UFA'}) then
      begin
        ArcFile^.Read(Id, SizeOf(Id));
        if (Id and $1000000) = 0 then{archive <> solid}
          begin
            ArcPos := ArcPos+8;
            UFADetect := True;
          end;
      end;
    ArcFile^.Seek(ArcPos);
  end;

function IS3Detect: boolean;
  var
    Id: longInt;
  begin
    IS3Detect := False;
    ArcFile^.Read(Id, SizeOf(Id));
    if Id = $8C655D13
    then
      IS3Detect := True
    else
      ArcFile^.Seek(ArcPos);
  end;

function SQZDetect: boolean;
  var
    s: array[0..4] of Char;
  begin
    ArcFile^.Read(s, 5);
    SQZDetect := False;
    if (ArcFile^.Status = stOK) and (s = 'HLSQZ')
    then
      begin
        SQZDetect := True;
        ArcPos := ArcPos+8;
      end;
    ArcFile^.Seek(ArcPos);
  end;

function HAPDetect: boolean;
  var
    Id: longInt;
  begin
    HAPDetect := False;
    ArcFile^.Read(Id, SizeOf(Id));
    if (ArcFile^.Status = stOK) and (Id = $46483391)
    then
      begin
        HAPDetect := True;
        ArcPos := ArcPos+14;
      end;
    ArcFile^.Seek(ArcPos);
  end;

function ZOODetect: boolean;
  var
    Id: longInt;
  begin
    ZOODetect := False;
    ArcFile^.Read(Id, SizeOf(Id));
    if (ArcFile^.Status = stOK) and (Id = $FDC4A7DC) then
      begin
        ArcFile^.Read(ArcPos, SizeOf(ArcPos));
        ZOODetect := True;
      end;
    ArcFile^.Seek(ArcPos);
  end;

function CHZDetect: boolean;
  var
    Id: longInt;
  begin
    CHZDetect := False;
    Id := 0;
    ArcFile^.Read(Id, 3);
      {last byte isn't needed, coz it checked in Check4ArcId}
    if (ArcFile^.Status = stOK) and (Id = $684353 {'SCh'}) then
        CHZDetect := True;
    ArcFile^.Seek(ArcPos);
  end;

function UC2Detect: boolean;
  var
    Id: longInt;
  begin
    UC2Detect := False;
    ArcFile^.Read(Id, SizeOf(Id));
    if (ArcFile^.Status = stOK) and (Id = $1A324355 {'UC2'#26})
    then
      UC2Detect := True
    else
      ArcFile^.Seek(ArcPos);
  end;

function AINDetect: boolean;
  var
    AinHdr: array[0..21] of byte;
    i, AinSum, chksum: AWord;
  begin
    AINDetect := False;
    ArcFile^.Read(AinHdr, SizeOf(AinHdr));
    ArcFile^.Read(AinSum, SizeOf(AinSum));
    if (ArcFile^.Status = stOK) and
      (_Cardinal(AinHdr[14]+256*AinHdr[15]+65536*AinHdr[16]+16777216*
        AinHdr[17]) < ArcFile^.GetSize)
    then
      begin
        chksum := 0;
        for i := 0 to 21 do
          chksum := chksum+AinHdr[i];
        AINDetect := (chksum = (AinSum xor $5555));
      end;
    ArcFile^.Seek(ArcPos);
  end;
{$ENDIF}

function DetectArchive;
  begin
    if ACEDetect then
      DetectArchive := New(PACEArchive, Init)
    else if ARJDetect then
      DetectArchive := New(PARJArchive, Init)
    else if CABDetect then
      DetectArchive := New(PCABArchive, Init)
    else if HADetect then
      DetectArchive := New(PHAArchive, Init)
    else if LHADetect then
      DetectArchive := New(PLHAArchive, Init)
    else if RARDetect then
      DetectArchive := New(PRARArchive, Init)
    else if ZIPDetect then
      DetectArchive := New(PZIPArchive, Init)
    else
      {$IFNDEF MINARCH} if AINDetect then
      DetectArchive := New(PAINArchive, Init)
    else if ARCDetect then
      DetectArchive := New(PARCArchive, Init)
    else if BS2Detect then
      DetectArchive := New(PBS2Archive, Init)
    else if BSADetect then
      DetectArchive := New(PBSAArchive, Init)
    else if CHZDetect then
      DetectArchive := New(PCHZArchive, Init)
    else if HAPDetect then
      DetectArchive := New(PHAPArchive, Init)
    else if HPKDetect then
      DetectArchive := New(PHPKArchive, Init)
    else if HYPDetect then
      DetectArchive := New(PHYPArchive, Init)
    else if LIMDetect then
      DetectArchive := New(PLIMArchive, Init)
    else if QuArkDetect then
      DetectArchive := New(PQuarkArchive, Init)
    else if SQZDetect then
      DetectArchive := New(PSQZArchive, Init)
    else if TARDetect then
      DetectArchive := New(PTARArchive, Init)
    else if TGZDetect then
      DetectArchive := New(PTGZArchive, Init)
    else if UC2Detect then
      DetectArchive := New(PUC2Archive, Init)
    else if UFADetect then
      DetectArchive := New(PUFAArchive, Init)
    else if ZOODetect then
      DetectArchive := New(PZOOArchive, Init)
    else if ZXZDetect then
      DetectArchive := New(PZXZArchive, Init)
    else if IS3Detect then
      DetectArchive := New(PIS3Archive, Init)
    else
      {$ENDIF}
      {$IFDEF PLUGIN}
      DetectArchive := Plugin.DetectCreateArchiveObject;
      {$ELSE}
      DetectArchive := nil;
    {$ENDIF}
    CloseProfile;
  end { DetectArchive };

function GetArchiveTagBySign;
  begin
    if Sign = sigACE then
      GetArchiveTagBySign := arcACE
    else if Sign = sigARJ then
      GetArchiveTagBySign := arcARJ
    else if Sign = sigCAB then
      GetArchiveTagBySign := arcCAB
    else if Sign = sigHA then
      GetArchiveTagBySign := arcHA
    else if Sign = sigLHA then
      GetArchiveTagBySign := arcLHA
    else if Sign = sigRAR then
      GetArchiveTagBySign := arcRAR
    else if Sign = sigZIP then
      GetArchiveTagBySign := arcZIP
    else
      {$IFNDEF MINARCH} if Sign = sigAIN then
      GetArchiveTagBySign := arcAIN
    else if Sign = sigARC then
      GetArchiveTagBySign := arcARC
    else if Sign = sigBS2 then
      GetArchiveTagBySign := arcBS2
    else if Sign = sigBSA then
      GetArchiveTagBySign := arcBSA
    else if Sign = sigCHZ then
      GetArchiveTagBySign := arcCHZ
    else if Sign = sigHAP then
      GetArchiveTagBySign := arcHAP
    else if Sign = sigHPK then
      GetArchiveTagBySign := arcHPK
    else if Sign = sigHYP then
      GetArchiveTagBySign := arcHYP
    else if Sign = sigLIM then
      GetArchiveTagBySign := arcLIM
    else if Sign = sigQUARK then
      GetArchiveTagBySign := arcQUARK
    else if Sign = sigSQZ then
      GetArchiveTagBySign := arcSQZ
    else if Sign = sigTAR then
      GetArchiveTagBySign := arcTAR
    else if Sign = sigTGZ then
      GetArchiveTagBySign := arcTGZ
    else if Sign = sigUC2 then
      GetArchiveTagBySign := arcUC2
    else if Sign = sigUFA then
      GetArchiveTagBySign := arcUFA
    else if Sign = sigZOO then
      GetArchiveTagBySign := arcZOO
    else if Sign = sigZXZ then
      GetArchiveTagBySign := arcZXZ
    else if Sign = sigIS3 then
      GetArchiveTagBySign := arcIS3
    else
      {$ENDIF}
      {$IFDEF PLUGIN}
      GetArchiveTagBySign := Plugin.GetArchiveTagBySign(Sign);
      {$ELSE}
      GetArchiveTagBySign := arcUNK;
    {$ENDIF}
  end { GetArchiveTagBySign };

function GetArchiveByTag;
  begin
    if Id = arcACE then
      GetArchiveByTag := New(PACEArchive, Init)
    else if Id = arcARJ then
      GetArchiveByTag := New(PARJArchive, Init)
    else if Id = arcCAB then
      GetArchiveByTag := New(PCABArchive, Init)
    else if Id = arcHA then
      GetArchiveByTag := New(PHAArchive, Init)
    else if Id = arcLHA then
      GetArchiveByTag := New(PLHAArchive, Init)
    else if Id = arcRAR then
      GetArchiveByTag := New(PRARArchive, Init)
    else if Id = arcZIP then
      GetArchiveByTag := New(PZIPArchive, Init)
    else
      {$IFNDEF MINARCH} if Id = arcAIN then
      GetArchiveByTag := New(PAINArchive, Init)
    else if Id = arcARC then
      GetArchiveByTag := New(PARCArchive, Init)
    else if Id = arcBS2 then
      GetArchiveByTag := New(PBS2Archive, Init)
    else if Id = arcBSA then
      GetArchiveByTag := New(PBSAArchive, Init)
    else if Id = arcCHZ then
      GetArchiveByTag := New(PCHZArchive, Init)
    else if Id = arcHAP then
      GetArchiveByTag := New(PHAPArchive, Init)
    else if Id = arcHPK then
      GetArchiveByTag := New(PHPKArchive, Init)
    else if Id = arcHYP then
      GetArchiveByTag := New(PHYPArchive, Init)
    else if Id = arcLIM then
      GetArchiveByTag := New(PLIMArchive, Init)
    else if Id = arcQUARK then
      GetArchiveByTag := New(PQuarkArchive, Init)
    else if Id = arcSQZ then
      GetArchiveByTag := New(PSQZArchive, Init)
    else if Id = arcTAR then
      GetArchiveByTag := New(PTARArchive, Init)
    else if Id = arcTGZ then
      GetArchiveByTag := New(PTGZArchive, Init)
    else if Id = arcUC2 then
      GetArchiveByTag := New(PUC2Archive, Init)
    else if Id = arcUFA then
      GetArchiveByTag := New(PUFAArchive, Init)
    else if Id = arcZOO then
      GetArchiveByTag := New(PZOOArchive, Init)
    else if Id = arcZXZ then
      GetArchiveByTag := New(PZXZArchive, Init)
    else if Id = arcIS3 then
      GetArchiveByTag := New(PIS3Archive, Init)
    else
      {$ENDIF}
      {$IFDEF PLUGIN}
      GetArchiveByTag := Plugin.GetArchiveByTag(Id);
      {$ELSE}
      GetArchiveByTag := nil;
    {$ENDIF}
    CloseProfile;
  end { GetArchiveByTag };

end.
