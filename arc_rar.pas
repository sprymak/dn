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
unit arc_RAR; {RAR}

interface

uses
  Archiver, advance, advance1, Objects, Dos;

type
  PRARArchive = ^TRARArchive;
  TRARArchive = object(TARJArchive)
    VersionToExtr: byte;
    Constructor Init;
    procedure GetFile; virtual;
    function GetID: byte; virtual;
    function GetSign: TStr4; virtual;
    end;

type
  MainRARHdr = record
    Id: longInt;
    HeadLen: AWord;
    HeadFlags: byte;
    end;

  MainRAR2Hdr = record
    HeadCRC: AWord;
    HeadType: byte;
    HeadFlags: AWord;
    HeadLen: AWord;
    reserved1: AWord;
    Reserved2: longInt;
    end;
  // HeadFags:
  // $01 - ��ਡ�� ⮬� (⮬ �����⮬���� ��娢�)
  // $02 - ��������� ��娢�� �������਩
  // $04 - ��ਡ�� �����஢�� ��娢�
  // $08 - ��ਡ�� �����뢭��� (solid) ��娢�
  // $10 - ����� �奬� ���������� ⮬�� ('volname.partN.rar')
  // $20 - ��������� ���ଠ�� �� ���� ��� ���஭��� ������� (AV)
  // $40 - ��������� ���ଠ�� ��� ����⠭�������
  // $80 - ��������� ������ ����஢���

var
  RAR2: boolean;
  Encrypted: boolean;

implementation

uses
  Messages, Commands;

{ ----------------------------- RAR ------------------------------------}

Constructor TRARArchive.Init;
  var
    Sign: TStr5;
    Q: String;
  begin
    Sign := GetSign;
    SetLength(Sign, Length(Sign)-1);
    Sign := Sign+#0;
    FreeStr := SourceDir+DNARC;
    TObject.Init;
    {$IFNDEF OS2}
    Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,
      'RAR.EXE'));
    UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,
      'RAR.EXE'));
    {$ELSE}
    Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,
      'RAR.EXE'));
    UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,
      'RAR.EXE;RAR32.EXE'));
    {$ENDIF}
    Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,
      'e -c-'));
    ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,
      'x -c-'));
    Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'a'));
    Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'm'));
    Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'd'));
    Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, '-p'));
    Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 't'));
    IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PIncludePaths, ''));
    ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExcludePaths, '-ep'));
    ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,
      '-y'));
    RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,
      '-rr'));
    SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,
      '-sfx'));
    Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, '-s'));
    RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PRecurseSubDirs, '-r0'));
    SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PSetPathInside, '-ap'));
    StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PStoreCompression, '-m0'));
    FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastestCompression, '-m1'));
    FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastCompression, '-m2'));
    NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PNormalCompression, '-m3'));
    GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PGoodCompression, '-m4'));
    UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PUltraCompression, '-m5'));
    ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PComprListChar, '@'));
    ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExtrListChar, '@'));

    Q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
    AllVersion := Q <> '0';
    Q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '0');
    PutDirs := Q <> '0';
    {$IFDEF OS_DOS}
    Q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
    Swap := Q <> '0';
    {$ELSE}
    Q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '0');
    ShortCmdLine := Q <> '0';
    {$ENDIF}
    {$IFNDEF OS2}
    Q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
    UseLFN := Q <> '0';
    {$ENDIF}
    VersionToExtr := 0;
  end { TRARArchive.Init };

function TRARArchive.GetID;
  begin
    GetID := arcRAR;
  end;

function TRARArchive.GetSign;
  begin
    GetSign := sigRAR;
  end;

type
  LocRarHdr = record
    PSize: longInt;
    USize: longInt;
    CRC: AWord;
    HdrLen: AWord;
    Date: longInt;
    Attr: byte;
    Flags: byte;
    Ver: byte;
    NameLen: byte;
    Method: byte;
    CommLen: AWord;
    end;

  LocRar2Hdr = record
    HeadCRC: AWord;
    HeadType: byte;
    HeadFlags: AWord;
    HeadSize: AWord;
    PSize: longInt;
    USize: longInt;
    OSVer: byte;
    CRC: longInt;
    Date: longInt;
    Ver: byte;
    Method: byte;
    NameLen: AWord;
    Attr: longInt;
    end;
  // HeadFlags:
  // $01 - 䠩� �த�������� �� �।��饣� ⮬�
  // $02 - 䠩� �த�������� � ᫥���饬 ⮬�
  // $04 - 䠩� ����஢�� ��஫��
  // $08 - ��������� �������਩ 䠩��
  // $10 - �ᯮ������ ���ଠ�� �� �।���� 䠩���
  //       (䫠� �����뢭���) (��� RAR 2.0 � ����)
  //
  //       ���� 7 6 5 (��� RAR 2.0 � ���):
  //            0 0 0    - ࠧ��� ᫮����   64 ��
  // $20        0 0 1    - ࠧ��� ᫮����  128 ��
  // $40        0 1 0    - ࠧ��� ᫮����  256 ��
  // $60        0 1 1    - ࠧ��� ᫮����  512 ��
  // $80        1 0 0    - ࠧ��� ᫮���� 1024 ��
  // $A0        1 0 1    - ࠧ��� ᫮���� 2048 KB
  // $C0        1 1 0    - ࠧ��� ᫮���� 4096 KB
  // $E0        1 1 1    - 䠩� ���� ��⠫����
  //
  // $100 - ���������� ���� HIGH_PACK_SIZE � HIGH_UNP_SIZE.
  //        �� ���� �ᯮ������� ⮫쪮 ��� ��娢�஢���� �祭� ������ 䠩���
  //        (����� 2 ��), ��� 䠩��� ����襣� ��ꥬ� �� ���� ����������
  // $200 - FILE_NAME ᮤ�ন� ����� � ���筮� �ଠ� � � Unicode,
  //        ࠧ����� ���. � �⮬ ��砥 ���� NAME_SIZE ࠢ�� �����
  //        ���筮�� ����� ���� ����� ����� � �ଠ� Unicode ���� 1
  // $400 - ��᫥ ����� 䠩�� � ��������� ��室���� 8 �������⥫��� ����,
  //        ����� ����室��� ��� 㢥��祭�� ���񦭮�� ��஢���� ("᮫�")
  // $800 - 䫠� ���ᨨ. �� ���� ����� 䠩��, �����
  //        ���ᨨ �������� � ����� 䠩�� ��� ';n'
  // $8000 - ��� ��� �ᥣ�� ��⠭�����, ⠪ ��� ��騩 ࠧ���
  //         ����� HEAD_SIZE + PACK_SIZE (� ���� HIGH_PACK_SIZE,
  //         �᫨ ��⠭����� ��� 0x100)

procedure TRARArchive.GetFile;
  var
    fp: longInt;
    PS: integer;
    P: LocRarHdr;
    P2: LocRar2Hdr;
    DirMask: longInt;
  label 1;
  begin
1:
    if (ArcFile^.GetPos = ArcFile^.GetSize) then
      begin
        if Encrypted then
          Msg(dlArcEncrypted, nil, mfOKButton);
        FileInfo.Last := 1;
        exit;
      end;
    if (ArcFile^.Status <> stOK) then
      begin
        FileInfo.Last := 2;
        exit;
      end;
    if RAR2 then
      begin
        fp := ArcFile^.GetPos;
        ArcFile^.Read(P2, 7);
        if (ArcFile^.Status <> stOK) then
          begin
            FileInfo.Last := 2;
            exit;
          end;
        {piwamoto}
        {we must skip garbage (digital sign as example) at the end of archive}
        {check for valid HeadType: $72 can't be valid here, $7a..7f reserved for future RAR versions}
        if not (P2.HeadType in [$73..$7f]) then
          begin
            FileInfo.Last := 1;
            exit;
          end;
        {/piwamoto}
        if P2.HeadType = $74 then
          begin
            ArcFile^.Read(P2.PSize, SizeOf(P2)-7);
            if (ArcFile^.Status <> stOK) then
              begin
                FileInfo.Last := 2;
                exit;
              end;
            FileInfo.Last := 0;
            FileInfo.Date := P2.Date;
            FileInfo.PSize := P2.PSize;
            FileInfo.USize := P2.USize;
            FileInfo.Attr := byte(P2.HeadFlags and $04 <> 0)*Hidden;
            if P2.OSVer = 3 then
              DirMask := $4000 {Unix}
            else
              DirMask := Directory {DOS compatible};
            if P2.Attr and DirMask <> 0
            then
              FileInfo.Attr := FileInfo.Attr or Directory;
            if P2.NameLen > 255 then
              P2.NameLen := 255;
            ArcFile^.Read(FileInfo.FName[1], P2.NameLen);
            SetLength(FileInfo.FName, P2.NameLen);
            if P2.HeadFlags and $200 <> 0 then
              SetLength(FileInfo.FName, (PosChar(#0, FileInfo.FName)-1))
                ;
            {piwamoto: skip unicode names from winrar2.80beta1+ archives}
            repeat
              PS := System.Pos('.\', FileInfo.FName);
              if PS = 0 then
                break;
              System.Delete(FileInfo.FName, PS, 1);
            until False;
            if (ArcFile^.Status <> stOK) then
              begin
                FileInfo.Last := 2;
                exit;
              end;
            ArcFile^.Seek(fp+P2.HeadSize+P2.PSize);
            if P2.Ver > VersionToExtr then
              VersionToExtr := P2.Ver;
            exit;
          end;
        if P2.HeadSize = 0 then
          P2.HeadSize := 7;
        if P2.HeadFlags and $8000 <> 0
        then
          begin
            ArcFile^.Read(fp, 4);
            ArcFile^.Seek(ArcFile^.GetPos-4);
          end
        else
          fp := 0;
        ArcFile^.Seek(ArcFile^.GetPos+fp+P2.HeadSize-7);
        goto 1;
      end
    else
      begin
        ArcFile^.Read(P, SizeOf(P)-2);
        if (ArcFile^.Status <> stOK) or (P.NameLen = 0) then
          begin
            FileInfo.Last := 2;
            exit;
          end;
        FileInfo.Last := 0;
        FileInfo.Date := P.Date;
        FileInfo.PSize := P.PSize;
        FileInfo.USize := P.USize;
        FileInfo.Attr := byte(P.Flags and $04 <> 0)*Hidden;
        if P.Flags and $08 <> 0 then
          begin
            {ArcFile^.Read(P, P.CommLen);}
            ArcFile^.Seek(ArcFile^.GetPos+P.CommLen);
            if (ArcFile^.Status <> stOK) then
              begin
                FileInfo.Last := 2;
                exit;
              end;
          end;
        SetLength(FileInfo.FName, P.NameLen);
        ArcFile^.Read(FileInfo.FName[1], P.NameLen);
        if (ArcFile^.Status <> stOK) then
          begin
            FileInfo.Last := 2;
            exit;
          end;
        ArcFile^.Seek(ArcFile^.GetPos+P.PSize);
        if P.Ver > VersionToExtr then
          VersionToExtr := P.Ver;
      end;
  end { TRARArchive.GetFile };

end.
