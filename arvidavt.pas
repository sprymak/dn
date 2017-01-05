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
unit ArvidAvt;

interface

uses
  Arvid, Objects, advance1, Messages, DNApp, Commands, Collect,
  Views, Drivers, Startup, U_KeyMap, advance, Lfn, Files, Dos, Tree,
  FilesCol, advance2, Drives, FlPanel, Memory;

function AvtCMP(s1, s2: String): integer;
function AvtCellText(Offset: longInt; var s: TStream): String;
function AvtCellName(const C: TAvtFileCell; var AStream: TStream):
  String;
function AvtCellDesc(const C: TAvtFileCell; var AStream: TStream):
  String;

function AvtGetCell(AvtDr: PArvidDrive): longInt;
procedure AvtFreeCell(AvtDr: PArvidDrive; const l: longInt);
function AvtCellRotateLeft(AvtDr: PArvidDrive; Loc: longInt):
  longInt;
function AvtCellRotateRight(AvtDr: PArvidDrive; Loc: longInt):
  longInt;
{procedure AvtCheckTree(AvtDr:PArvidDrive);}
function AvtDelFile(AvtDr: PArvidDrive; AName: String): boolean;
function AvtNewFile(AvtDr: PArvidDrive;
  AName: String;
  ADescription: String;
  AIsDir: boolean;
  AChildOrSize: longInt;
  ATime: longInt;
  AStartSector: longInt;
  AAttr: word): word;
procedure AvtSeekDirectory(AvtDr: PArvidDrive);
procedure AvtGetDirectory(AvtDr: PArvidDrive; var ALocation: longInt;
  var FC: PFilesCollection; const FileMask: String);
function CopyFilesToArvid(const s: String; Files: PCollection;
    MoveMode: boolean; Owner: Pointer): boolean;
procedure AvtCopyFilesInto(AvtDr: PArvidDrive; AFiles: PCollection;
  Own: PView; MoveMode: boolean);
procedure AvtEraseFiles(AvtDr: PArvidDrive; AFiles: PCollection);
procedure AvtMakeDir(AvtDr: PArvidDrive);
procedure AvtEditDescription(AvtDr: PArvidDrive; var s, Nam: String);
procedure AvtCalcTotal(AvtDr: PArvidDrive; const Offset: longInt;
    var LL: TSize);
function AvtInit(AvtDr: PArvidDrive): boolean;

implementation

var
  FCtemp, FCLtemp, FCRtemp: TAvtFileCell;
  ArvidDeleteAllFiles: boolean;

function AvtCMP(s1, s2: String): integer;
  begin
    UpStr(s1);
    UpStr(s2);
    if s1 = s2 then
      AvtCMP := 0
    else if s1 < s2 then
      AvtCMP := -1
    else
      AvtCMP := 1;
  end;

function AvtCellText(Offset: longInt; var s: TStream): String;
  var
    t: TAvtTextCell;
    s1: String;
  begin
    s1 := '';
    while Offset <> 0 do
      begin
        s.Seek(Offset);
        s.Read(t, SizeOf(t));
        if s.Status <> stOK then
          break;
        s1 := s1+Copy(t.Data, 1, 36);
        Offset := t.NextTextCell;
      end;
    AvtCellText := s1;
  end;

function AvtCellName(const C: TAvtFileCell; var AStream: TStream):
    String;
  var
    s: String;
  begin
    case C.Flags and avtCellFormat of
      avtCell0:
        s := Copy(C.Name0, 1, 16);
      avtCell1:
        s := Copy(C.Name1, 1, 12);
      avtCell2:
        s := Copy(C.Name2, 1, 12);
      avtCell3:
        s := AvtCellText(C.NamePtr, AStream);
    end {case};
    while (Length(s) <> 0) and (s[Length(s)] = #0) do
      SetLength(s, Length(s)-1);
    AvtCellName := s;
  end;

function AvtCellDesc(const C: TAvtFileCell; var AStream: TStream):
    String;
  var
    s: String;
  begin
    case C.Flags and avtCellFormat of
      avtCell2:
        s := AvtCellText(C.DescPtr2, AStream);
      avtCell3:
        s := AvtCellText(C.DescPtr3, AStream);
      else
        s := '' {avtCell0,avtCell1};
    end {case};
    while (Length(s) <> 0) and (s[Length(s)] = #0) do
      SetLength(s, Length(s)-1);
    AvtCellDesc := s;
  end;

function AvtGetCell(AvtDr: PArvidDrive): longInt;
  var
    t: TAvtTextCell;
    M: TAvtMediaCell;
    B: PChar;
  begin
    with AvtDr^ do
      begin
        if AVT.FreeCell = 0 then
          begin
            AvtGetCell := AVT.AfterLastCell;
            Inc(AVT.AfterLastCell, SizeOf(t));
            if (PosTableOfs <> 0) and (AVT.AfterLastCell >=
                PosTableOfs)
            then
              begin
                Stream^.Seek(AVT.AvtMediaCell);
                Stream^.Read(M, SizeOf(M));
                GetMem(B, M.PositionTableSize);
                Stream^.Seek(PosTableOfs);
                Stream^.Read(B^, M.PositionTableSize);
                Inc(PosTableOfs, 512);
                M.PositionTable := PosTableOfs;
                Stream^.Seek(PosTableOfs);
                Stream^.Write(B^, M.PositionTableSize);
                FreeMem(B, M.PositionTableSize);
                Stream^.Seek(AVT.AvtMediaCell);
                Stream^.Write(M, SizeOf(M));
              end;
          end
        else
          begin
            AvtGetCell := AVT.FreeCell;
            Stream^.Seek(AVT.FreeCell);
            Stream^.Read(t, SizeOf(t));
            AVT.FreeCell := t.NextTextCell;
          end;
      end
  end { AvtGetCell };

procedure AvtFreeCell(AvtDr: PArvidDrive; const l: longInt);
  var
    t: TAvtTextCell;
  begin
    with AvtDr^ do
      begin
        if l = 0 then
          exit;
        t.NextTextCell := AVT.FreeCell;
        Stream^.Seek(l);
        Stream^.Write(t, SizeOf(t));
        AVT.FreeCell := l;
      end
  end;

function AvtCellRotateLeft(AvtDr: PArvidDrive; Loc: longInt):
    longInt;
  var
    Right: longInt;
  begin
    with AvtDr^ do
      begin
        Stream^.Seek(Loc);
        Stream^.Read(FCtemp, SizeOf(FCtemp));
        Stream^.Seek(FCtemp.RightFileCell);
        Stream^.Read(FCRtemp, SizeOf(FCRtemp));
        if (FCRtemp.Flags and avtBalance) = $00000300 then
          begin
            Right := AvtCellRotateRight(AvtDr, FCtemp.RightFileCell);
            Stream^.Seek(Loc);
            Stream^.Read(FCtemp, SizeOf(FCtemp));
            FCtemp.RightFileCell := Right;
            Stream^.Seek(FCtemp.RightFileCell);
            Stream^.Read(FCRtemp, SizeOf(FCRtemp));
          end;
        Right := FCtemp.RightFileCell;
        FCtemp.RightFileCell := FCRtemp.LeftFileCell;
        FCRtemp.LeftFileCell := Loc;
        if FCRtemp.Flags and avtBalance = 0
        then
          FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
            {or $00000000}
        else
          FCtemp.Flags := (FCtemp.Flags and (not avtBalance)) or
            $00000300;
        FCRtemp.Flags := (FCRtemp.Flags and (not avtBalance)) or
          $00000300;
        Stream^.Seek(Loc);
        Stream^.Write(FCtemp, SizeOf(FCtemp));
        Stream^.Seek(Right);
        Stream^.Write(FCRtemp, SizeOf(FCRtemp));
        AvtCellRotateLeft := Right;
      end
  end { AvtCellRotateLeft };

function AvtCellRotateRight(AvtDr: PArvidDrive; Loc: longInt):
    longInt;
  var
    Left: longInt;
  begin
    with AvtDr^ do
      begin
        Stream^.Seek(Loc);
        Stream^.Read(FCtemp, SizeOf(FCtemp));
        Stream^.Seek(FCtemp.LeftFileCell);
        Stream^.Read(FCLtemp, SizeOf(FCLtemp));
        if (FCLtemp.Flags and avtBalance) = $00000100 then
          begin
            Left := AvtCellRotateLeft(AvtDr, FCtemp.LeftFileCell);
            Stream^.Seek(Loc);
            Stream^.Read(FCtemp, SizeOf(FCtemp));
            FCtemp.LeftFileCell := Left;
            Stream^.Seek(FCtemp.LeftFileCell);
            Stream^.Read(FCLtemp, SizeOf(FCLtemp));
          end;
        Left := FCtemp.LeftFileCell;
        FCtemp.LeftFileCell := FCLtemp.RightFileCell;
        FCLtemp.RightFileCell := Loc;
        if (FCLtemp.Flags and avtBalance) = 0
        then
          FCtemp.Flags := (FCtemp.Flags and (not avtBalance))
            {or $00000000}
        else
          FCtemp.Flags := (FCtemp.Flags and (not avtBalance)) or
            $00000100;
        FCLtemp.Flags := (FCLtemp.Flags and (not avtBalance)) or
          $00000100;
        Stream^.Seek(Loc);
        Stream^.Write(FCtemp, SizeOf(FCtemp));
        Stream^.Seek(Left);
        Stream^.Write(FCLtemp, SizeOf(FCLtemp));
        AvtCellRotateRight := Left;
      end
  end { AvtCellRotateRight };

{
procedure AvtCheckTree(AvtDr: PArvidDrive);
  function AvtNodeHight(const Loc: LongInt): LongInt;
  var
    LeftHight:  LongInt;
    RightHight: LongInt;
  begin with AvtDr^ do begin
    AvtNodeHight:=0;
    if Loc <> 0 then begin
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      LeftHight:=AvtNodeHight(FCtemp.LeftFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      RightHight:=AvtNodeHight(FCtemp.RightFileCell);
      if LeftHight >= RightHight then
        AvtNodeHight:=LeftHight + 1 else
        AvtNodeHight:=RightHight + 1;
    end;
  end end;

  procedure AvtNodeCheck(const Loc: LongInt);
  var
    LeftHight:  LongInt;
    RightHight: LongInt;
    Balance:    LongInt;
  begin with AvtDr^ do begin
    if Loc <> 0 then begin
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      if (FCtemp.Flags and avtIsDir) <> 0 then begin
        AvtNodeCheck(FCtemp.ChildOrSize);
        Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      end;
      AvtNodeCheck(FCtemp.LeftFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      AvtNodeCheck(FCtemp.RightFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));

      LeftHight:=AvtNodeHight(FCtemp.LeftFileCell);
      Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
      RightHight:=AvtNodeHight(FCtemp.RightFileCell);
      Balance:=RightHight-LeftHight;
      if (Balance < -1) or (Balance > 1) then
        MessageBox(GetString(erInvalidFileFormat), nil, mfError + mfOKButton)
      else begin
        Balance:=(Balance shl 8) and $00000300;
        Stream^.Seek(Loc); Stream^.Read(FCtemp, SizeOf(FCtemp));
        if Balance <> (FCtemp.Flags and $00000300) then
          MessageBox(GetString(erInvalidFileFormat), nil, mfError + mfOKButton);
      end;
    end;
  end end;

begin
  AvtNodeCheck(AvtDr^.AVT.RootDirCell);
end;
}

function AvtDelFile(AvtDr: PArvidDrive; AName: String): boolean;
  var
    SaveCurDir: String;
    CurDir2: String;
    dr: String;
    Nm: String;
    XT: String;
    SN: String;
    FC: TAvtFileCell;
    FCL: TAvtFileCell;
    FCR: TAvtFileCell;
    NewCurDirPos: longInt;
    DelDirAnswer: word;

  label 1;

  procedure AvtDelCell(Loc: longInt);
    var
      FC: TAvtFileCell;
      t: TAvtTextCell;
      l: longInt;
    begin
      with AvtDr^ do
        begin
          Stream^.Seek(Loc);
          Stream^.Read(FC, SizeOf(FC));
          case FC.Flags and avtCellFormat of
            avtCell2:
              l := FC.DescPtr2;
            avtCell3:
              l := FC.DescPtr3;
            else
              l := 0 {avtCell0,avtCell1};
          end {case};
          while l <> 0 do
            begin
              Stream^.Seek(l);
              Stream^.Read(t, SizeOf(t));
              AvtFreeCell(AvtDr, l);
              l := t.NextTextCell;
            end;
          if FC.Flags and avtCellFormat = avtCell3
          then
            l := FC.NamePtr
          else
            l := 0 {avtCell0,avtCell1,avtCell2};
          while l <> 0 do
            begin
              Stream^.Seek(l);
              Stream^.Read(t, SizeOf(t));
              AvtFreeCell(AvtDr, l);
              l := t.NextTextCell;
            end;
          AvtFreeCell(AvtDr, Loc);
        end
    end { AvtDelCell };

  procedure AvtFreeTree(const Loc: longInt);
    begin
      with AvtDr^ do
        begin
          if Loc = 0 then
            exit;
          Stream^.Seek(Loc);
          Stream^.Read(FC, SizeOf(FC));
          if FC.Flags and avtIsDir <> 0 then
            begin
              AvtFreeTree(FC.ChildOrSize);
              Stream^.Seek(Loc);
              Stream^.Read(FC, SizeOf(FC));
            end;
          AvtFreeTree(FC.LeftFileCell);
          Stream^.Seek(Loc);
          Stream^.Read(FC, SizeOf(FC));
          AvtFreeTree(FC.RightFileCell);
          AvtDelCell(Loc);
        end
    end { AvtFreeTree };

  function AvtTreeNodeRemove(const Loc: longInt): longInt;
    var
      OldBalance: longInt;
      l0, l1, L2: longInt;

    function AvtRestoreLeftBalance(Loc: longInt;
      const OldBalance: longInt): longInt;
      var
        LeftBalance: longInt;
        CurBalance: longInt;
        RightZeroBal: boolean;
      begin
        with AvtDr^ do
          begin
            Stream^.Seek(Loc);
            Stream^.Read(FC, SizeOf(FC));
            CurBalance := FC.Flags and avtBalance;
            if FC.LeftFileCell <> 0 then
              begin
                Stream^.Seek(FC.LeftFileCell);
                Stream^.Read(FCL, SizeOf(FCL));
                LeftBalance := FCL.Flags and avtBalance;
              end;
            if (FC.LeftFileCell = 0) or
              ((LeftBalance <> OldBalance) and (LeftBalance = 0))
            then
              begin
                FC.Flags := FC.Flags and (not avtBalance);
                {       if CurBalance = $00000300 then FC.Flags:=FC.Flags or $00000000 else}
                if CurBalance = $00000000 then
                  FC.Flags := FC.Flags or $00000100
                else
                  {       if CurBalance = $00000100}
                  begin
                    Stream^.Seek(FC.RightFileCell);
                    Stream^.Read(FCR, SizeOf(FCR));
                    RightZeroBal := (FCR.Flags and avtBalance) = 0;
                    Loc := AvtCellRotateLeft(AvtDr, Loc);
                    Stream^.Seek(Loc);
                    Stream^.Read(FC, SizeOf(FC));
                    Stream^.Seek(FC.LeftFileCell);
                    Stream^.Read(FCL, SizeOf(FCL));
                    if RightZeroBal then
                      begin
                        FC.Flags := (FC.Flags and (not avtBalance)) or
                          $00000300;
                        FCL.Flags := (FCL.Flags and (not avtBalance)) or
                          $00000100;
                      end
                    else
                      begin
                        FC.Flags := (FC.Flags and (not avtBalance))
                          {or $00000000};
                        FCL.Flags := (FCL.Flags and (not avtBalance))
                          {or $00000000};
                      end;
                    Stream^.Seek(FC.LeftFileCell);
                    Stream^.Write(FCL, SizeOf(FCL));
                  end;
                Stream^.Seek(Loc);
                Stream^.Write(FC, SizeOf(FC));
              end;
            AvtRestoreLeftBalance := Loc;
          end
      end { AvtRestoreLeftBalance };

    function AvtRestoreRightBalance(Loc: longInt;
      const OldBalance: longInt): longInt;
      var
        RightBalance: longInt;
        CurBalance: longInt;
        LeftZeroBal: boolean;
      begin
        with AvtDr^ do
          begin
            Stream^.Seek(Loc);
            Stream^.Read(FC, SizeOf(FC));
            CurBalance := FC.Flags and avtBalance;
            if FC.RightFileCell <> 0 then
              begin
                Stream^.Seek(FC.RightFileCell);
                Stream^.Read(FCR, SizeOf(FCR));
                RightBalance := FCR.Flags and avtBalance;
              end;
            if (FC.RightFileCell = 0) or
              ((RightBalance <> OldBalance) and (RightBalance = 0))
            then
              begin
                FC.Flags := FC.Flags and $FFFFFCFF;
                {       if CurBalance = $00000100 then FC.Flags:=FC.Flags or $00000000 else}
                if CurBalance = $00000000 then
                  FC.Flags := FC.Flags or $00000300
                else
                  {       if CurBalance = $00000300}
                  begin
                    Stream^.Seek(FC.LeftFileCell);
                    Stream^.Read(FCL, SizeOf(FCL));
                    LeftZeroBal := (FCL.Flags and avtBalance) = 0;
                    Loc := AvtCellRotateRight(AvtDr, Loc);
                    Stream^.Seek(Loc);
                    Stream^.Read(FC, SizeOf(FC));
                    Stream^.Seek(FC.RightFileCell);
                    Stream^.Read(FCR, SizeOf(FCR));
                    if LeftZeroBal then
                      begin
                        FC.Flags := (FC.Flags and (not avtBalance)) or
                          $00000100;
                        FCR.Flags := (FCR.Flags and (not avtBalance)) or
                          $00000300;
                      end
                    else
                      begin
                        FC.Flags := (FC.Flags and (not avtBalance))
                          {or $00000000};
                        FCR.Flags := (FCR.Flags and (not avtBalance))
                          {or $00000000};
                      end;
                    Stream^.Seek(FC.RightFileCell);
                    Stream^.Write(FCR, SizeOf(FCR));
                  end;
                Stream^.Seek(Loc);
                Stream^.Write(FC, SizeOf(FC));
              end;
            AvtRestoreRightBalance := Loc;
          end
      end { AvtRestoreRightBalance };

    function AvtTreeRemoveLeftmost(const Loc: longInt): longInt;
      var
        OldBalance: longInt;
        l1, L2: longInt;
      begin
        with AvtDr^ do
          begin
            Stream^.Seek(Loc);
            Stream^.Read(FC, SizeOf(FC));
            if FC.LeftFileCell = 0 then
              begin
                l0 := Loc;
                AvtTreeRemoveLeftmost := FC.RightFileCell;
                exit;
              end;
            l1 := FC.LeftFileCell;
            Stream^.Seek(l1);
            Stream^.Read(FC, SizeOf(FC));
            OldBalance := FC.Flags and avtBalance;
            L2 := AvtTreeRemoveLeftmost(l1);
            if l1 <> L2 then
              begin
                Stream^.Seek(Loc);
                Stream^.Read(FC, SizeOf(FC));
                FC.LeftFileCell := L2;
                Stream^.Seek(Loc);
                Stream^.Write(FC, SizeOf(FC));
              end;
            AvtTreeRemoveLeftmost := AvtRestoreLeftBalance(Loc,
              OldBalance);
          end
      end { AvtTreeRemoveLeftmost };

    function AvtTreeRemoveRightmost(const Loc: longInt): longInt;
      var
        OldBalance3: longInt;
        l1, L2: longInt;
      begin
        with AvtDr^ do
          begin
            Stream^.Seek(Loc);
            Stream^.Read(FC, SizeOf(FC));
            if FC.RightFileCell = 0 then
              begin
                l0 := Loc;
                AvtTreeRemoveRightmost := FC.LeftFileCell;
                exit;
              end;
            l1 := FC.RightFileCell;
            Stream^.Seek(l1);
            Stream^.Read(FC, SizeOf(FC));
            OldBalance := FC.Flags and avtBalance;
            L2 := AvtTreeRemoveRightmost(l1);
            if l1 <> L2 then
              begin
                Stream^.Seek(Loc);
                Stream^.Read(FC, SizeOf(FC));
                FC.RightFileCell := L2;
                Stream^.Seek(Loc);
                Stream^.Write(FC, SizeOf(FC));
              end;
            AvtTreeRemoveRightmost := AvtRestoreRightBalance(Loc,
              OldBalance);
          end
      end { AvtTreeRemoveRightmost };

    { function AvtTreeNodeRemove(const Loc: LongInt): LongInt; }
    begin { AvtTreeNodeRemove }
      with AvtDr^ do
        begin
          AvtTreeNodeRemove := Loc;
          if Loc = 0 then
            exit;
          Stream^.Seek(Loc);
          Stream^.Read(FC, SizeOf(FC));
          SN := AvtCellName(FC, Stream^);
          if AvtCMP(AName, SN) < 0 then
            begin
              l1 := FC.LeftFileCell;
              if l1 <> 0 then
                begin
                  Stream^.Seek(l1);
                  Stream^.Read(FC, SizeOf(FC));
                  OldBalance := FC.Flags and avtBalance;
                  L2 := AvtTreeNodeRemove(l1);
                  if l1 <> L2 then
                    begin
                      Stream^.Seek(Loc);
                      Stream^.Read(FC, SizeOf(FC));
                      FC.LeftFileCell := L2;
                      Stream^.Seek(Loc);
                      Stream^.Write(FC, SizeOf(FC));
                    end;
                  AvtTreeNodeRemove := AvtRestoreLeftBalance(Loc,
                    OldBalance);
                end;
            end
          else if AvtCMP(AName, SN) > 0 then
            begin
              l1 := FC.RightFileCell;
              if l1 <> 0 then
                begin
                  Stream^.Seek(l1);
                  Stream^.Read(FC, SizeOf(FC));
                  OldBalance := FC.Flags and avtBalance;
                  L2 := AvtTreeNodeRemove(l1);
                  if l1 <> L2 then
                    begin
                      Stream^.Seek(Loc);
                      Stream^.Read(FC, SizeOf(FC));
                      FC.RightFileCell := L2;
                      Stream^.Seek(Loc);
                      Stream^.Write(FC, SizeOf(FC));
                    end;
                  AvtTreeNodeRemove := AvtRestoreRightBalance(Loc,
                    OldBalance);
                end;
            end
          else
            begin
              if (FC.Flags and avtIsDir) <> 0 then
                begin
                  DelDirAnswer := cmOK;
                  if (not ArvidDeleteAllFiles) then
                    begin
                      Dec(SkyEnabled);
                      if Confirms and cfEraseSubDir = 0 then
                        DelDirAnswer := cmYes
                      else
                        begin
                          DelDirAnswer := cmCancel;
                          if FC.ChildOrSize = 0 then
                            DelDirAnswer := cmOK
                          else
                            begin
                              Stream^.Seek(FC.ChildOrSize);
                              Stream^.Read(FCR, SizeOf(FCR));
                              if (FCR.LeftFileCell = 0) and (FCR.
                                  RightFileCell = 0)
                              then
                                DelDirAnswer := cmOK
                              else
                                begin
                                  if AvtCellName(FCR, Stream^) =
                                      '..'
                                  then
                                    DelDirAnswer := cmOK
                                  else
                                    DelDirAnswer := MessageBox(^C+
                                      GetString(dlDirectory)+' '+
                                    ansi_ascii(Cut(AName, 40))+
                                    GetString(dlEraseDirNotEmpty),
                                      nil,
                                    mfConfirmation+mfNoButton+
                                      mfAllButton+
                                    mf2YesButton+mfCancelButton);
                                end;
                            end;
                        end;
                      Inc(SkyEnabled);
                      ArvidDeleteAllFiles := DelDirAnswer = cmOK;
                      Abort := DelDirAnswer = cmCancel;
                    end;
                  if not (DelDirAnswer in [cmYes, cmOK]) then
                    exit;
                  AvtFreeTree(FC.ChildOrSize);
                  Stream^.Seek(Loc);
                  Stream^.Read(FC, SizeOf(FC));
                end;
              AvtDelFile := True;
              if FC.RightFileCell = 0 then
                AvtTreeNodeRemove := FC.LeftFileCell
              else
                case (FC.Flags and avtBalance) of
                  0, $00000100:
                    begin
                      l1 := FC.RightFileCell;
                      Stream^.Seek(l1);
                      Stream^.Read(FCR, SizeOf(FCR));
                      OldBalance := FCR.Flags and avtBalance;
                      L2 := AvtTreeRemoveLeftmost(l1);
                      Stream^.Seek(Loc);
                      Stream^.Read(FC, SizeOf(FC));
                      FC.RightFileCell := L2;
                      Stream^.Seek(l0);
                      Stream^.Read(FCL, SizeOf(FCL));
                      FCL.LeftFileCell := FC.LeftFileCell;
                      FCL.RightFileCell := FC.RightFileCell;
                      FCL.Flags := (FCL.Flags and (not avtBalance)) or (
                        FC.Flags and $00000300);
                      Stream^.Seek(l0);
                      Stream^.Write(FCL, SizeOf(FCL));
                      AvtTreeNodeRemove := AvtRestoreRightBalance(l0,
                        OldBalance);
                    end;
                  $00000300:
                    begin
                      l1 := FC.LeftFileCell;
                      Stream^.Seek(l1);
                      Stream^.Read(FCL, SizeOf(FCL));
                      OldBalance := FCL.Flags and avtBalance;
                      L2 := AvtTreeRemoveRightmost(l1);
                      Stream^.Seek(Loc);
                      Stream^.Read(FC, SizeOf(FC));
                      FC.LeftFileCell := L2;
                      Stream^.Seek(l0);
                      Stream^.Read(FCR, SizeOf(FCR));
                      FCR.LeftFileCell := FC.LeftFileCell;
                      FCR.RightFileCell := FC.RightFileCell;
                      FCR.Flags := (FCR.Flags and (not avtBalance)) or (
                        FC.Flags and $00000300);
                      Stream^.Seek(l0);
                      Stream^.Write(FCR, SizeOf(FCR));
                      AvtTreeNodeRemove := AvtRestoreLeftBalance(l0,
                        OldBalance);
                    end;
                end {case};
              AvtDelCell(Loc);
            end;
        end
    end { AvtTreeNodeRemove };

  { function AvtDelFile( AName: PathStr): Boolean; }
  begin { AvtDelFile }
    with AvtDr^ do
      begin
        AvtDelFile := False;
        SaveCurDir := CurDir;
        if (filetype <> avdAvt) or (Length(AName) = 0) then
          goto 1;
        CurDir2 := CurDir;
        if (CurDir2[Length(CurDir2)] <> '\') then
          CurDir2 := CurDir2+'\';
        if AName[1] <> '\' then
          begin
            AName := CurDir2+AName;
          end;
        lFSplit(AName, dr, Nm, XT);
        if (dr[1] = '\') and (Length(dr) > 1) then
          Delete(dr, 1, 1); {DelFC(Dr);}
        if CurDir2 <> dr then
          begin
            CurDir := dr;
            SeekDirectory;
            CurDir2 := CurDir;
            if (CurDir2[Length(CurDir2)] <> '\') then
              CurDir2 := CurDir2+'\';
            if CurDir <> dr then
              goto 1;
          end;
        AName := Nm+XT;
        if Length(AName) = 0 then
          goto 1;
        Stream^.Status := stOK;
        AName := ascii_ansi(AName);
        NewCurDirPos := AvtTreeNodeRemove(CurDirPos);
        if NewCurDirPos <> CurDirPos then
          begin
            if CurDirCellPos = 0 then
              begin
                AVT.RootDirCell := NewCurDirPos;
                Stream^.Seek(0);
                Stream^.Write(AVT, SizeOf(AVT));
              end
            else
              begin
                Stream^.Seek(CurDirCellPos);
                Stream^.Read(FC, SizeOf(FC));
                FC.ChildOrSize := NewCurDirPos;
                Stream^.Seek(CurDirCellPos);
                Stream^.Write(FC, SizeOf(FC));
              end;
            CurDirPos := NewCurDirPos;
          end;
1:
        CurDir := SaveCurDir;
        SeekDirectory;
      end
  end { AvtDelFile };

function AvtNewFile(
  AvtDr: PArvidDrive;
  AName: String;
  ADescription: String;
  AIsDir: boolean;
  AChildOrSize: longInt;
  ATime: longInt;
  AStartSector: longInt;
  AAttr: word): word;

  var
    SaveCurDir: String;
    CurDir2: String;
    dr: String;
    Nm: String;
    XT: String;
    SN: String;
    SS: String;
    FC: TAvtFileCell;
    NewCurDirPos: longInt;
    NewCell: longInt;
    Lv: integer;
    NewCellFailed: boolean;
    FailedCellIsDir: boolean;
    CreatedCellIsDir: boolean;
    NewCellIsDir: boolean;
    FirstNameProcessed: boolean;

  function AvtPutText(s: String): longInt;
    var
      t: TAvtTextCell;
      l, L2: longInt;
      i: integer;
    begin
      with AvtDr^ do
        begin
          if s = '' then
            begin
              AvtPutText := 0;
              exit;
            end;
          l := AvtGetCell(AvtDr);
          AvtPutText := l;
          while l <> 0 do
            begin
              for i := 1 to 36 do
                begin
                  if Length(s) <> 0 then
                    begin
                      t.Data[i] := s[1];
                      Delete(s, 1, 1); {DelFC(S);}
                    end
                  else
                    t.Data[i] := #0;
                end;
              L2 := 0;
              if Length(s) <> 0 then
                L2 := AvtGetCell(AvtDr);
              t.NextTextCell := L2;
              Stream^.Seek(l);
              l := L2;
              Stream^.Write(t, SizeOf(t));
            end;
        end
    end { AvtPutText };

  procedure AvtPutName(s: String);
    var
      i: integer;
    begin
      with AvtDr^ do
        begin
          if Length(s) < 13 then
            begin
              FC.Flags := (FC.Flags and (not avtCellFormat)) or
                avtCell2;
              for i := 1 to 12 do
                begin
                  if Length(s) <> 0 then
                    begin
                      FC.Name2[i] := s[1];
                      Delete(s, 1, 1); {DelFC(S);}
                    end
                  else
                    FC.Name2[i] := #0;
                end;
            end
          else
            begin
              FC.Flags := (FC.Flags and (not avtCellFormat)) or
                avtCell3;
              FC.NamePtr := AvtPutText(s);
            end;
        end
    end { AvtPutName };

  procedure AvtPutDesc(s: String);
    begin
      with AvtDr^ do
        begin
          if Length(s) <> 0 then
            FC.DescPtr2 := AvtPutText(s)
          else
            FC.DescPtr2 := 0;
        end
    end;

  procedure AvtNewCell;
    begin
      with AvtDr^ do
        begin
          NewCell := AvtGetCell(AvtDr);
          FC.Flags := 0;
          if (AName[1] = '\') or AIsDir then
            AAttr := AAttr or Directory
          else
            AAttr := AAttr and not Directory;
          FC.Attr := AAttr;
          FC.LeftFileCell := 0;
          FC.RightFileCell := 0;
          FC.StartSector := 0;
          FC.ChildOrSize := AChildOrSize;
          FC.Time := ATime;
          if (AName[1] = '\') or AIsDir then
            begin
              FC.Flags := (FC.Flags and (not avtIsDir)) or avtIsDir;
            end;
          if (AName = '') or (AName = '\') then
            AvtPutDesc(ADescription)
          else
            AvtPutDesc('');
          AvtPutName(SS);
          CreatedCellIsDir := True;
          if (FC.Flags and avtIsDir) = 0 then
            begin
              FC.StartSector := AStartSector;
              CreatedCellIsDir := False;
            end;
          Stream^.Seek(NewCell);
          Stream^.Write(FC, SizeOf(FC));
          AvtNewFile := 1;
        end
    end { AvtNewCell };

  function AvtTreeNodeInsert(APos: longInt): longInt;
    var
      OldBalance: longInt;
      NewPointer: longInt;
    begin
      with AvtDr^ do
        begin
          if APos = 0 then
            begin
              AvtNewCell;
              AvtTreeNodeInsert := NewCell;
              exit;
            end;
          Stream^.Seek(APos);
          Stream^.Read(FCtemp, SizeOf(FCtemp));
          SN := AvtCellName(FCtemp, Stream^);
          AvtTreeNodeInsert := APos;
          if SS = SN then
            begin
              FailedCellIsDir := (FCtemp.Flags and avtIsDir) <> 0;
              NewCellFailed := True;
              exit;
            end;
          if AvtCMP(SS, SN) < 0 then
            begin
              if FCtemp.LeftFileCell <> 0 then
                begin
                  Stream^.Seek(FCtemp.LeftFileCell);
                  Stream^.Read(FCLtemp, SizeOf(FCLtemp));
                  OldBalance := FCLtemp.Flags and avtBalance;
                  NewPointer := AvtTreeNodeInsert(FCtemp.
                    LeftFileCell);
                  Stream^.Seek(APos);
                  Stream^.Read(FCtemp, SizeOf(FCtemp));
                  if NewPointer <> FCtemp.LeftFileCell then
                    begin
                      FCtemp.LeftFileCell := NewPointer;
                      Stream^.Seek(APos);
                      Stream^.Write(FCtemp, SizeOf(FCtemp));
                    end;
                  Stream^.Seek(FCtemp.LeftFileCell);
                  Stream^.Read(FCLtemp, SizeOf(FCLtemp));
                  if ((FCLtemp.Flags and avtBalance) <> OldBalance)
                      and
                    ((FCLtemp.Flags and avtBalance) <> 0)
                  then
                    begin
                      if (FCtemp.Flags and avtBalance) = $00000300
                      then
                        begin
                          APos := AvtCellRotateRight(AvtDr, APos);
                          AvtTreeNodeInsert := APos;

                          Stream^.Seek(APos);
                          Stream^.Read(FCtemp, SizeOf(FCtemp));
                          FCtemp.Flags := (FCtemp.Flags and (not
                            avtBalance)) {or $00000000};
                          Stream^.Seek(APos);
                          Stream^.Write(FCtemp, SizeOf(FCtemp));

                          Stream^.Seek(FCtemp.RightFileCell);
                          Stream^.Read(FCRtemp, SizeOf(FCRtemp));
                          FCRtemp.Flags := (FCRtemp.Flags and (not
                            avtBalance)) {or $00000000};
                          Stream^.Seek(FCtemp.RightFileCell);
                          Stream^.Write(FCRtemp, SizeOf(FCRtemp));
                        end
                      else
                        begin
                          if (FCtemp.Flags and avtBalance) =
                              $00000100
                          then
                            FCtemp.Flags := (FCtemp.Flags and (not
                              avtBalance)) {or $00000000}
                          else
                            FCtemp.Flags := (FCtemp.Flags and (not
                              avtBalance)) or $00000300;
                          Stream^.Seek(APos);
                          Stream^.Write(FCtemp, SizeOf(FCtemp));
                        end;
                    end;
                end
              else
                begin{FCtemp.LeftFileCell = 0}
                  AvtNewCell;
                  FCtemp.LeftFileCell := NewCell;
                  if (FCtemp.Flags and avtBalance) = $00000100 then
                    FCtemp.Flags := (FCtemp.Flags and (not
                      avtBalance)) {or $00000000}
                  else
                    FCtemp.Flags := (FCtemp.Flags and (not
                      avtBalance)) or $00000300;
                  Stream^.Seek(APos);
                  Stream^.Write(FCtemp, SizeOf(FCtemp));
                end;
            end
          else
            begin{SS > SN}
              if FCtemp.RightFileCell <> 0 then
                begin
                  Stream^.Seek(FCtemp.RightFileCell);
                  Stream^.Read(FCRtemp, SizeOf(FCRtemp));
                  OldBalance := FCRtemp.Flags and avtBalance;
                  NewPointer := AvtTreeNodeInsert(FCtemp.
                    RightFileCell);
                  Stream^.Seek(APos);
                  Stream^.Read(FCtemp, SizeOf(FCtemp));
                  if NewPointer <> FCtemp.RightFileCell then
                    begin
                      FCtemp.RightFileCell := NewPointer;
                      Stream^.Seek(APos);
                      Stream^.Write(FCtemp, SizeOf(FCtemp));
                    end;
                  Stream^.Seek(FCtemp.RightFileCell);
                  Stream^.Read(FCRtemp, SizeOf(FCRtemp));
                  if ((FCRtemp.Flags and avtBalance) <> OldBalance)
                      and
                    ((FCRtemp.Flags and avtBalance) <> 0)
                  then
                    begin
                      if (FCtemp.Flags and avtBalance) = $00000100
                      then
                        begin
                          APos := AvtCellRotateLeft(AvtDr, APos);
                          AvtTreeNodeInsert := APos;

                          Stream^.Seek(APos);
                          Stream^.Read(FCtemp, SizeOf(FCtemp));
                          FCtemp.Flags := (FCtemp.Flags and (not
                            avtBalance)) {or $00000000};
                          Stream^.Seek(APos);
                          Stream^.Write(FCtemp, SizeOf(FCtemp));

                          Stream^.Seek(FCtemp.LeftFileCell);
                          Stream^.Read(FCLtemp, SizeOf(FCLtemp));
                          FCLtemp.Flags := (FCLtemp.Flags and (not
                            avtBalance)) {or $00000000};
                          Stream^.Seek(FCtemp.LeftFileCell);
                          Stream^.Write(FCLtemp, SizeOf(FCLtemp));
                        end
                      else
                        begin
                          if (FCtemp.Flags and avtBalance) =
                              $00000300
                          then
                            FCtemp.Flags := (FCtemp.Flags and (not
                              avtBalance)) {or $00000000}
                          else
                            FCtemp.Flags := (FCtemp.Flags and (not
                              avtBalance)) or $00000100;
                          Stream^.Seek(APos);
                          Stream^.Write(FCtemp, SizeOf(FCtemp));
                        end;
                    end;
                end
              else
                begin{FCtemp.RightFileCell = 0}
                  AvtNewCell;
                  FCtemp.RightFileCell := NewCell;
                  if (FCtemp.Flags and avtBalance) = $00000300 then
                    FCtemp.Flags := (FCtemp.Flags and (not
                      avtBalance)) {or $00000000}
                  else
                    FCtemp.Flags := (FCtemp.Flags and (not
                      avtBalance)) or $00000100;
                  Stream^.Seek(APos);
                  Stream^.Write(FCtemp, SizeOf(FCtemp));
                end;
            end;
        end
    end { AvtTreeNodeInsert };

  procedure AvtMakeNew;
    var
      NewCurDirPos: longInt;
      DD: TAvtFileCell;
      i: integer;
    begin
      with AvtDr^ do
        begin
          NewCellFailed := False;
          Lv := CurLevel+1;
          Stream^.Status := stOK;
          if AName[1] = '\' then
            Delete(AName, 1, 1); {DelFC(AName);}
          while AName <> '' do
            begin
              SS := '';
              while (AName[1] <> '\') and (AName <> '') do
                begin
                  AddStr(SS, AName[1]);
                  Delete(AName, 1, 1); {DelFC(AName);}
                  if Length(SS) = 12 then
                    break;
                end;
              if Length(SS) = 12 then
                while (AName[1] <> '\') and (AName <> '') do
                  Delete(AName, 1, 1); {DelFC(AName);}
              SN := ansi_ascii(CurDir2+SS);
              if (SS = '.') or (SS = '..') or (Pos('?', SS) > 0) or (
                  Pos('*', SS) > 0)
                or (Pos(':', SS) > 0) or (SS = '')
              then
                begin
                  MessageBox(GetString(dlFCNoCreateDir)+SN, nil,
                    mfError+mfOKButton);
                  exit;
                end;
              NewCurDirPos := AvtTreeNodeInsert(CurDirPos);
              NewCellIsDir := (AName[1] = '\') or AIsDir;
              Delete(AName, 1, 1); {DelFC(AName);}
              if NewCellFailed then
                begin
                  if NewCellIsDir <> FailedCellIsDir then
                    begin
                      SN := ansi_ascii(CurDir2+SS);
                      if NewCellIsDir then
                        MessageBox(GetString(dlFCNoCreateDir)+SN,
                          nil, mfError+mfOKButton)
                      else
                        MessageBox(GetString(dlleCantCreate)+SN, nil,
                          mfError+mfOKButton);
                      exit;
                    end;
                end;
              if FirstNameProcessed = False then
                begin
                  FirstNameProcessed := True;
                  if NewCellIsDir then
                    if (CurDir[Length(CurDir)] <> '\') and (CurDir <> ''
                        )
                    then
                      CreatedDir := GetDir+'\'+SS
                    else
                      CreatedDir := GetDir+SS;
                end;
              if NewCurDirPos <> CurDirPos then
                begin
                  if CurDirCellPos = 0 then
                    begin
                      AVT.RootDirCell := NewCurDirPos;
                      Stream^.Seek(0);
                      Stream^.Write(AVT, SizeOf(AVT));
                    end
                  else
                    begin
                      Stream^.Seek(CurDirCellPos);
                      Stream^.Read(FC, SizeOf(FC));
                      FC.ChildOrSize := NewCurDirPos;
                      Stream^.Seek(CurDirCellPos);
                      Stream^.Write(FC, SizeOf(FC));
                    end;
                  CurDirPos := NewCurDirPos;
                end;
              if CreatedCellIsDir then
                begin
                  if (CurDir[Length(CurDir)] <> '\') and
                    (CurDir <> '')
                  then
                    AddStr(CurDir, '\');
                  CurDir := CurDir+SS;
                  CurLevel := Lv;
                  Inc(Lv);
                  CurDirCellPos := NewCell;
                  CurDirPos := 0;
                end;
            end;
          CurDate := ATime;
        end
    end { AvtMakeNew };

  { function AvtNewFile; }
  begin { AvtNewFile }
    with AvtDr^ do
      begin
        AvtNewFile := 0;
        if (filetype <> avdAvt) or (Length(AName) = 0) then
          exit;
        NewCell := 0;
        CurDir2 := CurDir;
        if (CurDir2[Length(CurDir2)] <> '\') then
          CurDir2 := CurDir2+'\';
        SaveCurDir := CurDir2;
        if AName[1] <> '\' then
          begin
            AName := CurDir2+AName;
          end;
        if (AName <> '') and (AName[1] = '\') then
          Delete(AName, 1, 1); {DelFC(AName);}
        lFSplit(AName, dr, Nm, XT);
        Stream^.Status := stOK;
        if CurDir2 <> dr then
          begin
            CurDir := dr;
            SeekDirectory;
            CurDir2 := CurDir;
            if (CurDir2[Length(CurDir2)] <> '\') then
              CurDir2 := CurDir2+'\';
          end;
        if CurDir2 <> '\' then
          dr := Copy(dr, Length(CurDir2)+1, Length(dr));
        AName := dr+Nm+XT;

        FirstNameProcessed := False;
        SN := Copy(CurDir2, 1, Length(SaveCurDir));
        if SN = SaveCurDir then
          begin
            SN := Copy(CurDir2, Length(SaveCurDir)+1, Length(CurDir2));
            SS := '';
            while (SN <> '\') and (SN <> '') do
              begin
                AddStr(SS, SN[1]);
                Delete(SN, 1, 1); {DelFC(SN);}
              end;
            if SS <> '' then
              begin
                SN := CurDir;
                CurDir := SaveCurDir;
                if (CurDir[Length(CurDir)] <> '\') and (CurDir <> '')
                then
                  CreatedDir := GetDir+'\'+SS
                else
                  CreatedDir := GetDir+SS;
                CurDir := SN;
                FirstNameProcessed := True;
              end;
          end;
        if AName <> '' then
          begin
            AName := ascii_ansi(AName);
            ADescription := ascii_ansi(ADescription);
            CurDir2 := ascii_ansi(CurDir2);
            AvtMakeNew;
          end;
        CurDir := SaveCurDir;
        SeekDirectory;
      end
  end { AvtNewFile };

procedure AvtSeekDirectory(AvtDr: PArvidDrive);
  var
    i, j: longInt;
    Lv: integer;
    DD: TAvtFileCell;
    SavedCurDirPos: longInt;
    SeekFailed: boolean;
    s, SS, s2: String;
  begin
    with AvtDr^ do
      begin
        {  S:=Ascii_Ansi(CurDir);}
        s := CurDir;
        CurDirPos := AVT.RootDirCell;
        SavedCurDirPos := CurDirPos;
        CurDir := '';
        CurLevel := 0;
        Lv := 1;
        CurDirCellPos := 0;
        Stream^.Status := stOK;
        SeekFailed := False;
        if s[1] = '\' then
          Delete(s, 1, 1); {DelFC(S);}
        while s <> '' do
          begin
            SS := '';
            while (s[1] <> '\') and (s <> '') do
              begin
                AddStr(SS, s[1]);
                Delete(s, 1, 1); {DelFC(S);}
                {          if SS[0] = #12 then Break;}
              end;
            {      if SS[0] = #12 then while (S[1] <> '\') and (S <> '') do DelFC(S);}
            while (s[1] <> '\') and (s <> '') do
              Delete(s, 1, 1); {DelFC(S);}
            Delete(s, 1, 1); {DelFC(S);}
            while True do
              begin
                if CurDirPos = 0 then
                  begin
                    SeekFailed := True;
                    break;
                  end;
                Stream^.Seek(CurDirPos);
                Stream^.Read(DD, SizeOf(DD));
                if Stream^.Status <> stOK then
                  break;
                s2 := AvtCellName(DD, Stream^);
                if AvtCMP(SS, s2) < 0 then
                  begin
                    CurDirPos := DD.LeftFileCell;
                    if CurDirPos = 0 then
                      SeekFailed := True;
                  end
                else if AvtCMP(SS, s2) > 0 then
                  begin
                    CurDirPos := DD.RightFileCell;
                    if CurDirPos = 0 then
                      SeekFailed := True;
                  end
                else
                  begin
                    if (DD.Flags and avtIsDir) = 0 then
                      begin
                        SeekFailed := True;
                        break;
                      end;
                    CurDirCellPos := CurDirPos;
                    CurDirPos := DD.ChildOrSize;
                    SavedCurDirPos := CurDirPos;
                    break;
                  end;
              end;
            if Stream^.Status <> stOK then
              break;
            if SeekFailed then
              begin
                CurDirPos := SavedCurDirPos;
                break;
              end;
            if (CurDir[Length(CurDir)] <> '\') and
              (CurDir <> '')
            then
              AddStr(CurDir, '\');
            {      CurDir:=CurDir + Ansi_Ascii(SS);}
            CurDir := CurDir+SS;
            CurLevel := Lv;
            Inc(Lv);
          end;
        CurDate := DD.Time;
      end
  end { AvtSeekDirectory };

procedure AvtGetDirectory(AvtDr: PArvidDrive; var ALocation: longInt;
  var FC: PFilesCollection; const FileMask: String);
  var
    TAttr: word;
    F: PFileRec;
    Cell: TAvtFileCell;
    IsDir: boolean;
    Str: String;
  begin
    with AvtDr^ do
      begin
        if ALocation = 0 then
          exit;
        Stream^.Status := stOK;
        Stream^.Seek(ALocation);
        if Stream^.Status <> stOK then
          exit;
        Stream^.Read(Cell, SizeOf(Cell));
        if Stream^.Status <> stOK then
          exit;
        AvtGetDirectory(AvtDr, Cell.LeftFileCell, FC, FileMask);
        Str := ansi_ascii(AvtCellName(Cell, Stream^));
        IsDir := Cell.Flags and avtIsDir <> 0;
        if IsDir then
          TAttr := Cell.Attr or Directory
        else
          TAttr := Cell.Attr and not Directory;
        if
          not (ArvidWithDN and Security and (TAttr and (Hidden+
            SysFile) <> 0))
          and (AllFiles or IsDir or InFilter(Str, FileMask))
        then
          begin
            if IsDir then
              F := NewFileRec(Str, {$IFNDEF OS2}Str, {$ENDIF}0, Cell.
                Time, 0, 0, TAttr, @CurDir)
            else
              F := NewFileRec(Str, {$IFNDEF OS2}Str, {$ENDIF}Cell.
                ChildOrSize, Cell.Time, 0, 0, TAttr, @CurDir);
            New(F^.DIZ);
            F^.DIZ^.Owner := nil;
            F^.DIZ^.isDisposable := True;
            F^.DIZ^.Line := ALocation;
            F^.DIZ^.DIZ := NewStr(ansi_ascii(AvtCellDesc(Cell,
              Stream^)));
            if not IsDir then
              begin
                Inc(TotFiles);
                TotLen := TotLen+Cell.ChildOrSize;
              end;
            F^.PSize := ALocation;
              {non standard use: location in AVT file}
            FC^.Insert(F);
          end;
        AvtGetDirectory(AvtDr, Cell.RightFileCell, FC, FileMask);
      end
  end { AvtGetDirectory };

function CopyFilesToArvid(const s: String; Files: PCollection;
    MoveMode: boolean; Owner: Pointer): boolean;
  var
    i, j: integer;
    PF: PFileRec;
    PD: PDrive;
    PAD: PArvidDrive;
    s2: String;
    ToStr: String;
    OldDir: String;

  function FindDrive(P: PArvidDrive): boolean;
    begin
      FindDrive := P^.Name^ = s2;
    end;

  begin
    CopyFilesToArvid := False;
    if Owner = nil then
      exit;
    PD := PFilePanel(Owner)^.Drive;
    if PD^.DriveType = dtArvid then
      exit;
    if ArvidDrives = nil then
      exit;
    i := Pos('\AVT:', s);
    j := Pos('\TDR:', s);
    if ((i = 0) and (j = 0)) or
      ((i <> 0) and (j <> 0))
    then
      exit;
    if i = 0 then
      i := j; {TDR found}
    s2 := Copy(s, 1, i);
    Inc(i, 5);
    while (i <= Length(s)) and (s[i] <> '\') do
      begin
        AddStr(s2, s[i]);
        Inc(i);
      end;
    if j <> 0 {TDR found} then
      s2 := s2+'.TDR'
    else
      s2 := s2+'.AVT';
    PAD := ArvidDrives^.FirstThat(@FindDrive);
    if PAD = nil then
      exit;
    CopyFilesToArvid := True;
    ToStr := Copy(s, i, 256);
    if ToStr = '' then
      ToStr := '\';
    OldDir := PAD^.CurDir;
    PAD^.CurDir := ToStr;
    PAD^.SeekDirectory;
    PAD^.CopyFilesInto(Files, Owner, MoveMode);
    PAD^.CurDir := OldDir;
    PAD^.SeekDirectory;
  end { CopyFilesToArvid };

procedure AvtCopyFilesInto(AvtDr: PArvidDrive; AFiles: PCollection;
  Own: PView; MoveMode: boolean);
  var
    PD: PDrive;
    i, j: integer;
    PF: PFileRec;
    t: lText;
    CmdFileCreated: boolean;
    P: PView;
    dr: String;
    Nm: String;
    XT: String;
    Desc: String;
    OldDir: String;
    From, s1, s2, S3, S4, CmdFileNam: String;

  procedure AvtWalkTree;
    var
      i: integer;
      PC: PCollection;
      Dummy: TSize;
    begin
      with AvtDr^ do
        begin
          PD^.lChDir(s2);
          PC := PD^.GetDirectory(141, 0, x_x, FreeStr, Dummy);
          for i := 0 to PC^.Count-1 do
            begin
              PF := PC^.At(i);
              S3 := MakeNormName(s1, PF^.FlName[True]);
              S4 := MakeNormName(s2, PF^.FlName[True]);

              Desc := '';
              if (PF^.DIZ <> nil) and (PF^.DIZ^.DIZ <> nil) then
                Desc := PF^.DIZ^.DIZ^;
              if (PF^.Attr and Directory) = 0 then
                AvtNewFile(AvtDr, S3, Desc, False, {Cat:warn}Round(PF^.
                  Size), PackedDate(PF), 0, PF^.Attr)
              else
                begin
                  Nm := PF^.FlName[True];
                  if (Nm <> '.') and (Nm <> '..') then
                    begin
                      if AvtNewFile(AvtDr, S3, Desc, True, 0,
                          PackedDate(PF), 0, PF^.Attr) <> 0
                      then
                        begin
                          s1 := S3+'\';
                          s2 := S4+'\';
                          AvtWalkTree;
                          SetLength(s1, Length(s1)-1);
                          SetLength(s2, Length(s2)-1);
                          while (s1 <> '') and (s1[Length(s1)] <>
                              '\')
                          do
                            begin
                              SetLength(s1, Length(s1)-1);
                              SetLength(s2, Length(s2)-1);
                            end;
                        end;
                    end;
                end;
            end;
          Dispose(PC, Done);
        end
    end { AvtWalkTree };

  begin { AvtCopyFilesInto }
    with AvtDr^ do
      begin
        if Own = nil then
          exit;
        PD := PFilePanel(Own)^.Drive;
        if PD^.DriveType = dtArvid then
          exit;
        From := PD^.GetRealName;
        s2 := From;
        j := 0;
        i := Pos(':', s2);
        while i > 0 do
          begin
            Inc(j);
            s2[i] := ' ';
            i := Pos(':', s2);
          end;
        if (j > 1) or MoveMode then
          begin
            MessageBox(GetString(dlArvidNeedDisk), nil, mfError+
              mfOKButton);
            exit;
          end;
        s2 := UpStrg(From);
        i := Pos(':', s2);
        if (i <> 2) or (s2[1] < 'A') or (s2[1] > 'Z') then
          begin
            MessageBox(GetString(dlArvidNeedDisk), nil, mfError+
              mfOKButton);
            exit;
          end;
        if TapeFmt <> 0 then
          begin
            lFSplit(Name^, dr, Nm, XT);
            CmdFileNam := MakeNormName(dr, Nm+'.WR');
            lAssignText(t, CmdFileNam);
            lAppendText(t);
            CmdFileCreated := False;
            if IOResult <> 0 then
              begin
                lRewriteText(t);
                Writeln(t.t, 'IDENT');
                CmdFileCreated := True;
              end;
            Writeln(t.t, '');
            P := WriteMsg(GetString(dlPleaseStandBy));
            for i := 0 to AFiles^.Count-1 do
              begin
                PF := AFiles^.At(i);
                s1 := MakeNormName('\'+CurDir, PF^.FlName[True]);
                s2 := MakeNormName(From, PF^.FlName[True]);
                if (PF^.Attr and Directory) = 0 then
                  Writeln(t.t, 'COPY '+SquashesName(s2)+' TP:'+
                    SquashesName(s1)+' /O/R/C/H')
                else
                  begin
                    Writeln(t.t, 'COPYDIR '+SquashesName(s2)+' TP:'+
                      SquashesName(s1)+' /I/O/R/C/H')
                  end;
              end;
            P^.Free;
            if CmdFileCreated then
              MessageBox(GetString(dlArvidCmdFileCreated)+CmdFileNam,
              nil, mfInformation+mfOKButton)
            else
              MessageBox(GetString(dlArvidCmdFileAppended)+
                CmdFileNam,
              nil, mfInformation+mfOKButton);
            Close(t.t);
          end
        else
          begin
            if filetype <> avdAvt then
              begin
                MessageBox(GetString(dlArvidVolumeIsNotTape), nil,
                  mfInformation+mfOKButton);
                MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil,
                  mfInformation+mfOKButton);
                exit;
              end;
            P := WriteMsg(GetString(dlPleaseStandBy));
            for i := 0 to AFiles^.Count-1 do
              begin
                PF := AFiles^.At(i);
                s1 := MakeNormName('\'+CurDir, PF^.FlName[True]);
                s2 := MakeNormName(From, PF^.FlName[True]);
                Desc := '';
                if (PF^.DIZ <> nil) and (PF^.DIZ^.DIZ <> nil) then
                    Desc := PF^.DIZ^.DIZ^;
                if (PF^.Attr and Directory) = 0 then
                  AvtNewFile(AvtDr, s1, Desc, False, {Cat:warn}Round(PF^
                    .Size), PackedDate(PF), 0, PF^.Attr)
                else
                  begin
                    AvtNewFile(AvtDr, s1, Desc, True, 0, PackedDate(PF),
                      0, PF^.Attr);
                    s1 := s1+'\';
                    s2 := s2+'\';
                    OldDir := PD^.GetDir;
                    AvtWalkTree;
                    PD^.lChDir(OldDir);
                  end;
              end;
            P^.Free;
            Stream^.Seek(0);
            Stream^.Write(AVT, SizeOf(AVT));
            Dispose(Stream, Done);
            Stream := New(PBufStream, Init(Name^, stOpen, 2048));
          end;
        GlobalMessage(evCommand, cmPanelReread, nil);
      end
  end { AvtCopyFilesInto };

procedure AvtEraseFiles(AvtDr: PArvidDrive; AFiles: PCollection);
  var
    PF: PFileRec;
    i: word;
    P: PView;
    R: boolean;
    s: String;
  begin
    with AvtDr^ do
      begin
        if filetype <> avdAvt then
          begin
            MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil,
              mfInformation+mfOKButton);
            exit;
          end;
        if AFiles^.Count = 0 then
          exit;
        if AFiles^.Count = 1 then
          begin
            PF := AFiles^.At(0);
            if (PF^.Attr and Directory) <> 0
            then
              s := GetString(dlEraseConfirmDir)+Cut(PF^.FlName[True], 40
                )+' ?'
            else
              s := GetString(dlEraseConfirm1)+Cut(PF^.FlName[True], 40)+
                ' ?';
          end
        else
          s := GetString(dlEraseConfirms1);
        i := MessageBox(s, nil, mfConfirmation+mfYesButton+
          mfNoButton {+mfFastButton});
        if (i <> cmYes) then
          exit;
        if AFiles^.Count > 1 then
          begin
            s := GetString(dlEraseConfirm2)+ItoS(AFiles^.Count)+' '+
              GetString(dlDIFiles)+' ?';
            i := MessageBox(s, nil, mfConfirmation+mfYesButton+
              mfNoButton);
            if (i <> cmYes) then
              exit;
          end;
        ArvidDeleteAllFiles := False;
        P := WriteMsg(GetString(dlPleaseStandBy));
        for i := 0 to AFiles^.Count-1 do
          begin
            PF := AFiles^.At(i);
            s := PF^.FlName[True];
            R := AvtDelFile(AvtDr, s);
            if R = False then
              MessageBox(GetString(dlErasingNoFile)+s, nil, mfError+
                mfOKButton);
          end;
        { AvtCheckTree(AvtDr); }
        P^.Free;
        Stream^.Seek(0);
        Stream^.Write(AVT, SizeOf(AVT));
        Dispose(Stream, Done);
        Stream := New(PBufStream, Init(Name^, stOpen, 2048));
        GlobalMessage(evCommand, cmPanelReread, nil);
      end
  end { AvtEraseFiles };

procedure AvtMakeDir(AvtDr: PArvidDrive);
  var
    s: String;
  begin
    with AvtDr^ do
      begin
        if LowMemory then
          exit;
        if filetype <> avdAvt then
          begin
            MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil,
              mfInformation+mfOKButton);
            exit;
          end;
        s := '';
        if ExecResource(dlgMkDir, s) <> cmOK then
          exit;
        DelLeft(s);
        DelRight(s);
        if s = '' then
          exit;
        if (s[Length(s)] <> '\') then
          s := s+'\';
        AvtNewFile(AvtDr, s, '', True, 0, 0, 0, 0);
        { AvtCheckTree(AvtDr); }
        Stream^.Seek(0);
        Stream^.Write(AVT, SizeOf(AVT));
        Dispose(Stream, Done);
        Stream := New(PBufStream, Init(Name^, stOpen, 2048));
        GlobalMessage(evCommand, cmPanelReread, nil);
      end
  end { AvtMakeDir };

procedure AvtEditDescription(AvtDr: PArvidDrive; var s, Nam: String);
  var
    C: TAvtFileCell;
    t: TAvtTextCell;
    l1, L2, L3, l: longInt;
    i: word;

  function AvtPutText(s: String): longInt;
    var
      t: TAvtTextCell;
      l, L2: longInt;
      i: integer;
    begin
      with AvtDr^ do
        begin
          if s = '' then
            begin
              AvtPutText := 0;
              exit;
            end;
          l := AvtGetCell(AvtDr);
          AvtPutText := l;
          while l <> 0 do
            begin
              for i := 1 to 36 do
                begin
                  if Length(s) <> 0 then
                    begin
                      t.Data[i] := s[1];
                      Delete(s, 1, 1); {DelFC(S);}
                    end
                  else
                    t.Data[i] := #0;
                end;
              L2 := 0;
              if Length(s) <> 0 then
                L2 := AvtGetCell(AvtDr);
              t.NextTextCell := L2;
              Stream^.Seek(l);
              l := L2;
              Stream^.Write(t, SizeOf(t));
            end;
        end
    end { AvtPutText };

  procedure AvtPutName(s: String);
    var
      i: integer;
    begin
      with AvtDr^ do
        begin
          if Length(s) < 13 then
            begin
              C.Flags := (C.Flags and (not avtCellFormat)) or
                avtCell2;
              for i := 1 to 12 do
                begin
                  if Length(s) <> 0 then
                    begin
                      C.Name2[i] := s[1];
                      Delete(s, 1, 1); {DelFC(S);}
                    end
                  else
                    C.Name2[i] := #0;
                end;
            end
          else
            begin
              C.Flags := (C.Flags and (not avtCellFormat)) or
                avtCell3;
              C.NamePtr := AvtPutText(s);
            end;
        end
    end { AvtPutName };

  begin { AvtEditDescription }
    with AvtDr^ do
      begin
        FreeStr := ascii_ansi(s);
        l1 := Stream^.GetPos;
        Stream^.Read(C, SizeOf(C));
        if Stream^.Status <> stOK then
          exit;
        case C.Flags and avtCellFormat of
          avtCell2:
            l := C.DescPtr2;
          avtCell3:
            l := C.DescPtr3;
          else
            l := 0 {avtCell0,avtCell1};
        end {case};
        while l <> 0 do
          begin
            Stream^.Seek(l);
            Stream^.Read(t, SizeOf(t));
            if Stream^.Status <> stOK then
              exit;
            AvtFreeCell(AvtDr, l);
            l := t.NextTextCell;
          end;
        if Length(FreeStr) = 0 then
          L2 := 0
        else
          begin
            L2 := AvtGetCell(AvtDr);
            l := L2;
            while l <> 0 do
              begin
                for i := 1 to 36 do
                  begin
                    if Length(FreeStr) <> 0 then
                      begin
                        t.Data[i] := FreeStr[1];
                        Delete(FreeStr, 1, 1); {DelFC(FreeStr);}
                      end
                    else
                      t.Data[i] := #0;
                  end;
                L3 := 0;
                if Length(FreeStr) <> 0 then
                  L3 := AvtGetCell(AvtDr);
                t.NextTextCell := L3;
                Stream^.Seek(l);
                l := L3;
                Stream^.Write(t, SizeOf(t));
              end;
          end;
        case C.Flags and avtCellFormat of
          avtCell0..avtCell1:
            begin
              Nam := AvtCellName(C, Stream^);
              C.Flags := (C.Flags and not avtCellFormat) or avtCell3;
              C.NonUsed1 := 0;
              C.NonUsed2 := 0;
              C.DescPtr2 := L2;
              AvtPutName(Nam);
            end;
          avtCell2:
            C.DescPtr2 := L2;
          avtCell3:
            C.DescPtr3 := L2;
        end {case};
        Stream^.Seek(l1);
        Stream^.Write(C, SizeOf(C));
      end
  end { AvtEditDescription };

procedure AvtCalcTotal(AvtDr: PArvidDrive; const Offset: longInt;
    var LL: TSize);
  var
    aa: TAvtFileCell;
  begin
    with AvtDr^ do
      begin
        if Offset = 0 then
          exit;
        Stream^.Seek(Offset);
        Stream^.Read(aa, SizeOf(aa));
        if Stream^.Status <> stOK then
          exit;
        AvtCalcTotal(AvtDr, aa.LeftFileCell, LL);
        if aa.Flags and avtIsDir <> 0
        then
          AvtCalcTotal(AvtDr, aa.ChildOrSize, LL)
        else
          LL := LL+aa.ChildOrSize;
        AvtCalcTotal(AvtDr, aa.RightFileCell, LL);
      end
  end { AvtCalcTotal };

function AvtInit;
  var
    A: TAvtMediaCell;
    i: longInt;
    j: word;
  begin
    with AvtDr^ do
      begin
        AvtInit := False;
        filetype := avdAvt;
        Stream^.Seek(AVT.AvtMediaCell);
        Stream^.Read(A, SizeOf(A));
        if Stream^.Status <> stOK then
          exit;
        TapeFmt := A.TapeFmt;
        if TapeFmt <> 0 then
          begin
            TapeTotalTime := A.TapeLen*60;
            PosTableOfs := A.PositionTable;
            Stream^.Seek(A.PositionTable);
            if A.PositionTableSize <> 4656 then
              begin
                Stream^.Read(i, SizeOf(i));
                if Stream^.Status <> stOK then
                  exit;
                TapeRecordedTime := i*4;
              end
            else
              begin
                Stream^.Read(j, SizeOf(j));
                if Stream^.Status <> stOK then
                  exit;
                TapeRecordedTime := j*8;
              end;
          end
        else
          begin
            TapeTotalTime := 0;
            TapeRecordedTime := 0;
            PosTableOfs := 0;
          end;
        AvtInit := True;
      end
  end { AvtInit };

end.
