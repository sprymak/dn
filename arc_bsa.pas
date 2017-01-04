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
unit Arc_bsa; {BSA}

interface
 uses Archiver, Advance1, Objects, FViewer, Advance, LFNCol, Dos;

type
    PBSAArchive = ^TBSAArchive;
    TBSAArchive = object(TARJArchive)
        constructor Init;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
    end;

type
     BSAHdr = record
      ID: Array [1..4] of Char;
      PackedSize: LongInt;
      OriginSize: LongInt;
      Date: LongInt;
      Data: Array[1..6] of Byte;
      NameLen: Byte;
     end;

implementation
{ ----------------------------- BSA ------------------------------------}

constructor TBSAArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'BSARC.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'BSARC.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            '-xy'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          '-xy'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                '-ar'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               '-am'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             '-D'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-xg'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               '-t'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       ''));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          ''));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        '+s'));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   ''));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '+q'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    ''));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  ''));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    ''));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   ''));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, ' ');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  if q='0' then Swap := False else Swap := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  if q='0' then UseLFN := False else UseLFN := True;
end;

function TBSAArchive.GetID;
begin
  GetID := arcBSA;
end;

function TBSAArchive.GetSign;
begin
  GetSign := sigBSA;
end;

Procedure TBSAArchive.GetFile;
var HS,i : Word;
    FP   : Longint;
    P    : BSAHdr;
    Q    : Array [1..40] of Char absolute P;
    S    : String;
    C    : Char;
    label 1;
begin
1:
 if ArcFile^.GetPos = ArcFile^.GetSize then begin FileInfo.Last := 1; Exit;end;
 ArcFile^.Read(P, 4);
 if (Copy(P.ID,1,2) = #0#0)
  then begin FileInfo.Last := 1;Exit;end;
 if (ArcFile^.Status <> stOK) or not ((P.ID[4] in [#0,#7]) and (Copy(P.ID,2,2) = #0#$AE))
  then begin FileInfo.Last := 2;Exit;end;
 ArcFile^.Read(P.PackedSize, Sizeof(P)-4);
 if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2;Exit;end;
 {if (P.Method > 20) then begin FileInfo.Last:=2;Exit;end;}
 FileInfo.Last := 0;
 FileInfo.Attr := 0;
 FileInfo.USize := P.OriginSize;
 FileInfo.PSize := P.PackedSize;
 FileInfo.Date  := P.Date{P.Date shl 16) or (P.Date shr 16)};
 i := 1;
 S[0] := Char(P.NameLen);
 ArcFile^.Read(S[1], P.NameLen and 255);
 While Pos('/', S) > 0 do S[Pos('/', S)] := '\';
 While Pos(#255, S) > 0 do S[Pos(#255, S)] := '\';
 FileInfo.LFN  := AddLFN(CDir+S);    {DataCompBoy}
 FileInfo.FName := CDir + S; {DataCompBoy}
 FP := ArcFile^.GetPos;
 ArcFile^.Seek(FP + P.PackedSize + 1);
end;

end.
