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
unit Arc_RAR; {RAR}

interface

uses
  Archiver, Advance, Advance1, Objects, {$IFNDEF OS2}LFNCol,{$ENDIF} Dos;

type
    PRARArchive = ^TRARArchive;
    TRARArchive = object(TARJArchive)
        VersionToExtr: Byte;
        constructor Init;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
    end;

type
     MainRARHdr = record
      ID: Longint;
      HeadLen: AWord;
      HeadFlags: Byte;
     end;

     MainRAR2Hdr = record
       HeadCRC: AWord;
       HeadType: Byte;
       HeadFlags: AWord;
       HeadLen: AWord;
       Reserved1: AWord;
       Reserved2: LongInt;
     end;

var RAR2: Boolean;

implementation

{ ----------------------------- RAR ------------------------------------}

constructor TRARArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; SetLength(Sign, Length(Sign)-1); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
{$IFNDEF OS2}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'RAR.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'RAR.EXE'));
{$ELSE}
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'RAR.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'RAR.EXE;RAR32.EXE'));
{$ENDIF}
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'e -c-'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'x -c-'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'a'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'm'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'd'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-p'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               't'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       '-ep'));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          '-y'));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        '-rr'));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        '-sfx'));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              '-s'));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     '-r0'));
  SetPathInside         := NewStr(GetVal(@Sign[1], @FreeStr[1], PSetPathInside,      '-ap'));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-m0'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-m1'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-m2'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-m3'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-m4'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-m5'));
  ComprListchar         := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListchar,      '@'));
  ExtrListchar          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListchar,       '@'));

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '0');
  PutDirs := q <> '0';
{$IFDEF OS_DOS}
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  Swap := q <> '0';
{$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '0');
  ShortCmdLine := q <> '0';
{$ENDIF}
{$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '1');
  UseLFN := q <> '0';
{$ENDIF}
  VersionToExtr := 0;
end;

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
       PSize: LongInt;
       USize: LongInt;
       CRC: AWord;
       HdrLen: AWord;
       Date: LongInt;
       Attr: Byte;
       Flags: Byte;
       Ver: Byte;
       NameLen: Byte;
       Method: Byte;
       CommLen: AWord;
     end;

     LocRar2Hdr = record
       HeadCRC: AWord;
       HeadType: Byte;
       HeadFlags: AWord;
       HeadSize: AWord;
       PSize: LongInt;
       USize: LongInt;
       OSVer: Byte;
       CRC: LongInt;
       Date: LongInt;
       Ver: Byte;
       Method: Byte;
       NameLen: AWord;
       Attr: LongInt;
     end;

Procedure TRARArchive.GetFile;
var
    FP   : Longint;
    Ps   : Integer;
    P    : LocRARHdr;
    P2   : LocRAR2Hdr;
    label 1;
begin
1: if (ArcFile^.GetPos = ArcFile^.GetSize) then begin FileInfo.Last:=1;Exit;end;
   if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2;Exit;end;
   if RAR2 then begin
      FP := ArcFile^.GetPos;
      ArcFile^.Read(P2, 7);
      if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2;Exit;end;
      if P2.HeadType = $74 then
       begin
         ArcFile^.Read(P2.PSize, SizeOf(P2)-7);
         if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2;Exit;end;
         FileInfo.Last := 0;
         FileInfo.Date := P2.Date;
         FileInfo.PSize := P2.PSize;
         FileInfo.USize := P2.USize;
         FileInfo.Attr := Byte(P2.HeadFlags and $04 <> 0) * Hidden;
         if ((P2.OSVer=3) {Unix} and (P2.Attr and $4000 <> 0) {directory})
           or (P2.Attr and Directory <> 0) {DOS}
           then FileInfo.Attr := FileInfo.Attr or Directory;
         if P2.NameLen > 255 then P2.NameLen := 255;
         ArcFile^.Read(FileInfo.FName[1], P2.NameLen);
         SetLength(FileInfo.FName, P2.NameLen);
         if P2.HeadFlags and $200 <> 0 then SetLength(FileInfo.FName, (PosChar(#0, FileInfo.FName) - 1));
            {piwamoto: skip unicode names from winrar2.80beta1+ archives}
         repeat
           Ps := System.Pos('.\', FileInfo.FName);
           if Ps = 0 then Break;
           System.Delete(FileInfo.FName, Ps, 1);
         until False;
         if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2;Exit;end;
         ArcFile^.Seek(FP+P2.HeadSize+P2.PSize);
         if P2.Ver > VersionToExtr then VersionToExtr := P2.Ver;
         Exit;
       end;
      if P2.HeadSize = 0 then P2.HeadSize := 7;
      if P2.HeadFlags and $8000 <> 0
         then begin ArcFile^.Read(FP,4);Arcfile^.Seek(Arcfile^.GetPos-4);end
         else FP := 0;
      ArcFile^.Seek(ArcFile^.GetPos + FP + P2.HeadSize - 7);
      Goto 1;
    end else begin
      ArcFile^.Read(P, SizeOf(P)-2);
      if (ArcFile^.Status <> stOK) or (P.NameLen = 0) then begin FileInfo.Last := 2;Exit;end;
      FileInfo.Last := 0;
      FileInfo.Date := P.Date;
      FileInfo.PSize := P.PSize;
      FileInfo.USize := P.USize;
      FileInfo.Attr := Byte(P.Flags and $04 <> 0) * Hidden;
      if P.Flags and $08 <> 0 then
       begin
        {ArcFile^.Read(P, P.CommLen);}
        ArcFile^.Seek(ArcFile^.GetPos + P.CommLen);
        if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2;Exit; end;
       end;
      SetLength(FileInfo.FName, P.NameLen);
      Arcfile^.Read(FileInfo.FName[1], P.NameLen);
      if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2;Exit; end;
      ArcFile^.Seek(ArcFile^.GetPos + P.PSize);
      if P.Ver > VersionToExtr then VersionToExtr := P.Ver;
   end;
end;

end.
