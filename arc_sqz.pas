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
unit Arc_SQZ; {SQZ}

interface
 uses Archiver, Advance1, Objects{, FViewer}, Advance, LFNCol, Dos, lfn;

type
    PSQZArchive = ^TSQZArchive;
    TSQZArchive = object(TARJArchive)
        constructor Init;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
    end;

type
     SQZHdr = record
      Size: Byte;
      SUM: Byte;
      Method: Byte;
      PackedSize: LongInt;
      OriginSize: LongInt;
      Date: LongInt;
      Attr: Byte;
      CRC: LongInt;
      Name: Array[0..127] of Char;
     end;

implementation
{ ----------------------------- SQZ ------------------------------------}

constructor TSQZArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'SQZ.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'SQZ.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'e'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'x'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'a'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'a'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'd'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             ''));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               't'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       ''));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          ''));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        's'));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-m0'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-m1'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-m2'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-m4'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-m4'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-m4'));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, '@');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  if q='0' then Swap := False else Swap := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  if q='0' then UseLFN := False else UseLFN := True;
end;

function TSQZArchive.GetID;
begin
  GetID := arcSQZ;
end;

function TSQZArchive.GetSign;
begin
  GetSign := sigSQZ;
end;

Procedure TSQZArchive.GetFile;
label 1;
var
    HS,i : AWord;
    FP   : Longint;
    P    : SQZHdr;
    S    : String;
    C    : Char;
begin
1:
 ArcFile^.Read(P,1);
 if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2; Exit; end;
 if (P.Size = 0) then begin FileInfo.Last := 1; Exit;end;
 if P.Size < $19 then
  begin
   ArcFile^.Read(I, 2);
   ArcFile^.Seek(ArcFile^.GetPos + I);
   Goto 1;
  end;
 ArcFile^.Read(P.SUM, P.Size + 1);
 {if (P.Method > 20) then begin FileInfo.Last:=2;Exit;end;}
 FileInfo.Last := 0;
 FileInfo.Attr := P.Attr and not Hidden;
 FileInfo.USize := P.OriginSize;
 FileInfo.PSize := P.PackedSize;
 FileInfo.Date := P.Date;
 i := 1;
 S[0] := Char(P.Size - 18); System.Move(P.Name, S[1], P.Size - 18);
 if Length(S) > 79 then begin FileInfo.Last := 2; Exit; end;
 While Pos('/', S) > 0 do S[Pos('/', S)] := '\';
 FileInfo.LFN  := AddLFN(S);  {DataCompBoy}
 FileInfo.FName := S; {DataCompBoy}
 ArcFile^.Seek(ArcFile^.GetPos + P.PackedSize);
end;

end.
