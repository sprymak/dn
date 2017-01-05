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
unit Arc_HAP; {HAP}

interface
 uses Archiver, Advance1, Objects{, FViewer}, Advance, {$IFNDEF OS2}LFNCol,{$ENDIF} Dos, Arc_HA;

type
    PHAPArchive = ^THAPArchive;
    THAPArchive = object(TARJArchive)
        constructor Init;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
    end;

type
     HAPHdr = record
      C: Char;
      ID: Array[1..4] of Char;
      PackedSize: LongInt;
      Reserved: Array[1..9] of Byte;
      Attr: Byte;
      Date: LongInt;
      OriginSize: LongInt;
     end;

implementation
{ ----------------------------- HAP ------------------------------------}

constructor THAPArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; SetLength(Sign, Length(Sign)-1); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'HAP3.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'PAH3.EXE'));
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
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   ''));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, ''));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    ''));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  ''));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    ''));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   ''));
  ComprListchar         := NewStr(GetVal(@Sign[1], @FreeStr[1], PComprListchar,      ' '));
  ExtrListchar          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtrListchar,       ' '));

  q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
  AllVersion := q <> '0';
  q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '0');
  PutDirs := q <> '0';
{$IFDEF OS_DOS}
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  Swap := q <> '0';
{$ELSE}
  q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '1');
  ShortCmdLine := q <> '0';
{$ENDIF}
{$IFNDEF OS2}
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  UseLFN := q <> '0';
{$ENDIF}
end;

function THAPArchive.GetID;
begin
  GetID := arcHAP;
end;

function THAPArchive.GetSign;
begin
  GetSign := sigHAP;
end;

Procedure THAPArchive.GetFile;
var HS,i : AWord;
    FP   : Longint;
    P    : HAPHdr;
    S    : String;
    C    : Char;
begin
 ArcFile^.Read(P,1);
 if (ArcFile^.GetPos = ArcFile^.GetSize)
   then begin FileInfo.Last := 1;Exit;end;
 ArcFile^.Read(P.ID,SizeOf(P)-1);
 if (ArcFile^.Status <> stOK) or (P.ID <> #142#104#74#87)
   then begin FileInfo.Last := 2;Exit;end;
 {if (P.Method > 20) then begin FileInfo.Last:=2;Exit;end;}
 FileInfo.Last := 0;
 FileInfo.Attr := P.Attr and not Hidden;;
 FileInfo.USize := P.OriginSize;
 FileInfo.PSize := P.PackedSize;
 FileInfo.Date := P.Date;
 i := 1;
 SetLength(S, 0);
 repeat ArcFile^.Read(C, 1); if C <> #0 then S := S + C; until (C = #0) or (Length(S) > 77);
 repeat ArcFile^.Read(C, 1); until (C in [#$15,#$16]) or (ArcFile^.Status <> stOK);
 if (ArcFile^.Status <> stOK) or (Length(S) > 79) then
  begin FileInfo.Last := 2; Exit; end;
 While Pos('/', S) > 0 do S[Pos('/', S)] := '\';
 While Pos(#255, S) > 0 do S[Pos(#255, S)] := '\';
{$IFNDEF OS2}
 FileInfo.LFN  := AddLFN(S);  {DataCompBoy}
{$ENDIF}
 FileInfo.FName := S; {DataCompBoy}
 ArcFile^.Seek(ArcFile^.GetPos + P.PackedSize-1);
end;

end.
