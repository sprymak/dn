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
unit arc_UC2; {UC2}

interface

uses
  Archiver;

type
  PUC2Archive = ^TUC2Archive;
  TUC2Archive = object(TARJArchive)
    ListFileName: String;
    ListFile: System.text;
    BaseDir: String;
    Constructor Init;
    procedure GetFile; virtual;
    function GetID: byte; virtual;
    function GetSign: TStr4; virtual;
    destructor Done; virtual;
    end;

implementation

uses
  Objects, advance2, advance, DNApp, DnExec, Commands, advance1,
    Messages,
  Dos;

{ ----------------------------- UC2 ------------------------------------}

Constructor TUC2Archive.Init;
  var
    Sign: TStr5;
    Q: String;
  begin
    Sign := GetSign;
    SetLength(Sign, Length(Sign)-1);
    Sign := Sign+#0;
    FreeStr := SourceDir+DNARC;
    TObject.Init;
    Packer := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker, 'UC.EXE'));
    UnPacker := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,
      'UC.EXE'));
    Extract := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract, 'E'));
    ExtractWP := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,
      'E !NOF ##.'));
    Add := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd, 'A'));
    Move := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove, 'AM'));
    Delete := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete, 'D'));
    Garble := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble, ''));
    Test := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest, 'T'));
    IncludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PIncludePaths, ''));
    ExcludePaths := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExcludePaths, ''));
    ForceMode := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,
      '-F'));
    RecoveryRec := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec, ''
      ));
    SelfExtract := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract, ''
      ));
    Solid := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid, ''));
    RecurseSubDirs := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PRecurseSubDirs, ''));
    SetPathInside := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PSetPathInside, ''));
    StoreCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PStoreCompression, ''));
    FastestCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastestCompression, '-TF'));
    FastCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PFastCompression, '-TF'));
    NormalCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PNormalCompression, '-TN'));
    GoodCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PGoodCompression, '-TT'));
    UltraCompression := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PUltraCompression, '-TT'));
    ComprListChar := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PComprListChar, '@'));
    ExtrListChar := NewStr(GetVal(@Sign[1], @FreeStr[1],
      PExtrListChar, '@'));

    Q := GetVal(@Sign[1], @FreeStr[1], PAllVersion, '0');
    AllVersion := Q <> '0';
    Q := GetVal(@Sign[1], @FreeStr[1], PPutDirs, '1');
    PutDirs := Q <> '0';
    {$IFDEF OS_DOS}
    Q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
    Swap := Q <> '0';
    {$ELSE}
    Q := GetVal(@Sign[1], @FreeStr[1], PShortCmdLine, '1');
    ShortCmdLine := Q <> '0';
    {$ENDIF}
    {$IFNDEF OS2}
    Q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
    UseLFN := Q <> '0';
    {$ENDIF}
  end { TUC2Archive.Init };

function TUC2Archive.GetID;
  begin
    GetID := arcUC2;
  end;

function TUC2Archive.GetSign;
  begin
    GetSign := sigUC2;
  end;

procedure TUC2Archive.GetFile;
  const
    FuckName = 'U$~RESLT.OK';
  var
    s: String;
    s1: String;

  procedure ReadName;
    var
      FName: String;
    begin
      System.readln(ListFile, s); {NAME=[*]}
      FName := Copy(s, 13, Length(s)-13);
      System.readln(ListFile, s);
      if s[7] = 'L' then
        begin
          {$IFNDEF OS2}
          if UseLFN then
            FName := Copy(s, 17, Length(s)-17);
          {$ENDIF}
          System.readln(ListFile, s);
        end;
      FileInfo.FName := BaseDir+FName;
    end;

  procedure ReadDTA;
    var
      DT: DateTime;
    begin
      { DATE(MDY)= }
      DT.Month := StoI(Copy(s, 17, 2));
      DT.Day := StoI(Copy(s, 20, 2));
      DT.Year := StoI(Copy(s, 23, 4));
      System.readln(ListFile, s);
      { TIME(HMS)= }
      DT.Hour := StoI(Copy(s, 17, 2));
      DT.Min := StoI(Copy(s, 20, 2));
      DT.Sec := StoI(Copy(s, 23, 4));
      PackTime(DT, FileInfo.Date);
      System.readln(ListFile, s);
      { ATTRIB= }
      FileInfo.Attr := 0;
    end;
  label
    NextRecord;
  begin { TUC2Archive.GetFile }
    if TextRec(ListFile).Handle = 0 then
      begin{ первый вызов: вызов архиватора для вывода оглавления }
        ListFileName := MakeNormName(TempDir, '!!!DN!!!.TMP');
        s := '/C '
        {$IFDEF OS2}
        +SourceDir+'dndosout.bat '+ListFileName+' '
        {$ENDIF}
        +UnPacker^+' ~D '+ArcFileName
        {$IFNDEF OS2}
        +' > '+ListFileName
        {$ENDIF}
        ;
        if Length(s) < 100 then
          AnsiExec(GetEnv('COMSPEC'), s)
        else
          MessageBox(^C+GetString(dlCmdLineTooLong), nil, mfOKButton+
            mfError);
        if not ExistFile(FuckName) then
          begin
            FileInfo.Last := 1;
            MessageBox(GetString(dlArcMsg6), nil, mfOKButton or
              mfError);
            exit;
          end;
        EraseFile(FuckName);
        System.Assign(ListFile, ListFileName);
        System.Reset(ListFile);
      end;
    FileInfo.Last := 0;
    { чтение данных об очередном файле}
NextRecord:
    repeat
      System.readln(ListFile, s);
      s1 := Copy(s, 1, 7);
    until s1 <> '      T'; {TAG=[]}
    //  if s1 = 'LIST [\' then
    while s1 = 'LIST [\' do
      begin
        BaseDir := Copy(s, 7, Length(s)-7);
        System.readln(ListFile, s);
        s1 := Copy(s, 1, 7);
        {    FileInfo.FName := BaseDir;
    FileInfo.USize := 0;
    FileInfo.PSize := 0;
    exit;}
      end;
    if s1 = 'END' then
      begin
        Close(ListFile);
        EraseFile(ListFileName);
        TextRec(ListFile).Handle := 0;
        FileInfo.Last := 1;
        exit;
      end;
    if s1 = '   DIR' then
      begin
        ReadName;
        ReadDTA;
        //    FileInfo.Attr := Directory;
        FileInfo.FName := FileInfo.FName+'\';
        FileInfo.USize := 0;
        FileInfo.PSize := 0;
      end
    else if s1 = '   FILE' then
      begin
        ReadName;
        {VERSION=}
        System.Delete(s, 1, 14);
        if s <> '0' then
          begin
            if not AllVersion then
              begin{ игнорируем этот файл }
                repeat
                  readln(ListFile, s);
                until s[7] = 'A';
                goto NextRecord;
              end;
            if Length(s) = 1 then
              s := '0'+s;
            FileInfo.FName := FileInfo.FName+';'+s;
          end;
        {SIZE=}
        System.readln(ListFile, s);
        System.Delete(s, 1, 11);
        FileInfo.USize := StoI(s);
        FileInfo.PSize := FileInfo.USize;
        System.readln(ListFile, s);
        if s[7] = 'C' then
          System.readln(ListFile, s); {CHECK=...}
        { uc 2.0 этой строки не формирует }
        ReadDTA;
      end
    else
      FileInfo.Last := 2;

end { TUC2Archive.GetFile };

destructor TUC2Archive.Done;
  begin
    if TextRec(ListFile).Handle <> 0 then
      begin
        System.Close(ListFile);
        EraseFile(ListFileName);
      end;
    inherited Done;
  end;

end.

