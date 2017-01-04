{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.6.RC1
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
program TDR2AVT;

{$T+}
{$M 45480,0,655350}

Uses
     LFN,       { lFSplit }
     Objects,   { PBufStream }
     Advance3,  { ExistFile, CalcTmpFName }
     FilesCol,  { PFileRec }
     Commands,  { psShowDescript }
     Arvid
     ArvidAvt;

 procedure Usage;
 begin
  WriteLn('Program can:');
  WriteLn(' - convert ArVid TDR file to AVT. Then you can edit AVT volume contents with');
  WriteLn('   DOS Navigator (DN): copy, rename, delete files and directories inside');
  WriteLn('   volume.');
  WriteLn(' - restore converted AVT to TDR.');
  WriteLn(' - make psevdo AVT (tape format 0). This AVT may be used for disk contents');
  WriteLn('   saving for later browsing with DN. When you try copy files from disk to');
  WriteLn('   such AVT, then DN do not make command file for AVCOMSTR but saves in AVT');
  WriteLn('   information about files and directories.');
  WriteLn;
  WriteLn('Usage: tdr2avt FILE-NAME [/r] [/n]');
  WriteLn('- Without flags FILE-NAME would be converted to AVT format.');
  WriteLn('- With /r flag FILE-NAME would be restored to TDR format.');
  WriteLn('- With /n flag psevdo AVT file with FILE-NAME would be created.');
  Halt(254);
 end;

procedure do_tdr_to_avt;
var
  Path, Path2, Dr,Nm,Ex,S1,S2,Desc: String;
  PTDR: PArvidDrive;
  PAVT: PArvidDrive;
  PF:   PFileRec;
  FC:   TTdrFileCell;
const
  FilesCounter: LongInt = 0;

    procedure prepare_avt;
    var
     IStream: PBufStream;
     OStream: PBufStream;
     D:       TTdrHeader;
     PT:      array[1..4656] of char;
    const
     AVT: TAvtHeader =
           (Signature:     'AVTP';
            AvtFmt:         1;
            CheckSum:       0;
            AfterLastCell:  80+4656;
            FreeCell:       0;
            RootDirCell:    0;
            NewSector:      0;
            LastNewSector:  0;
            AvtMediaCell:   40;
            Undefined1:     0);
      M: TAvtMediaCell =
           (NextMediaCell:  0;
            TapeFmt:        0;
            TapeLen:        0;
            TapeID:         0;
            Undefined1:     0;
            Undefined2:     0;
            Undefined3:     0;
            FirstSectorNum: 0;
            Sectors:        0;
            PositionTable:       80;
            PositionTableSize:   4656);
    label
      1;
    { procedure prepare_avt }
    begin
     lFSplit(ParamStr(1), Dr, Nm, Ex);
     if Ex = '' then Ex:='.TDR';
     Path:=Dr+Nm+Ex;
     if ExistFile(Path) = False then begin
       WriteLn('Error: file not exist "', Path, '"');
       Halt(254);
     end;
     IStream:=New(PBufStream, Init(Path, stOpenRead, 2048));
     if IStream^.Status <> stOK then begin
       WriteLn('Error: can not open ', Path);
       Halt(254);
     end;
     IStream^.Seek(0);
     IStream^.Read(D, SizeOf(D));
     if D.PosTableLen <> 4656 then begin
   1:  WriteLn('Error: invalid format of input TDR "', Path,'"');
       Halt(254);
     end;
     if D.FileTableOfs <> sizeof(TTdrHeader) then Goto 1;
     if D.DirTableOfs <> (D.FileTableOfs+D.FileTableLen) then Goto 1;
     if D.DescTableOfs < (D.DirTableOfs+D.DirTableLen) then Goto 1;
     if D.PosTableOfs <> ((D.DescTableOfs+D.DescTableLen+511) div 512 * 512)
         then Goto 1;
     if D.TapeLen < D.RecordLen then Goto 1;
     if D.FileTableLen <> (D.FileTableLen div sizeof(TTdrFileCell)
         * sizeof(TTdrFileCell)) then Goto 1;
     Path2:=Dr+Nm+'.AVT';
     if ExistFile(Path2) then begin
       WriteLn('Error: file exist "', Path2, '"');
       Halt(254);
     end;
     OStream:=New(PBufStream, Init(Path2, stCreate, 2048));
     if OStream^.Status <> stOK then begin
       WriteLn('Error: can not create ', Path2);
       Halt(254);
     end;
     AVT.NewSector:=D.NewRecordSector;
     AVT.LastNewSector:=D.LastNewRecordSector;
     OStream^.Write(AVT, SizeOf(AVT));
     M.TapeFmt:=D.TapeFmt;
     M.TapeLen:=D.TapeLen div 60;
     M.TapeID:=D.TapeID;
     M.Sectors:=AVT.NewSector;
     OStream^.Write(M, SizeOf(M));
     IStream^.Seek(D.PosTableOfs); IStream^.Read(PT, SizeOf(PT));
     OStream^.Write(PT, SizeOf(PT));
     Dispose(IStream,Done);
     Dispose(OStream,Done);
    end;
    procedure WalkTree;
    var
      I:  Integer;
      PC: PCollection;
    const
      DirCounter: LongInt = 0;
    begin
      PTDR^.lChDir(S1);
      PC:=PTDR^.GetDirectory(4, 0, x_x, FreeStr, FreeStr);
      for I:=0 to PC^.Count - 1 do begin
        PF:=PC^.at(i);
        S2:=UpStrg(MakeNormName(S1, MakeFileName(PF^.Name)));
        Desc:='';
        if (PF^.DIZ <> nil) and (PF^.DIZ^.DIZ <> nil) then Desc:=PF^.DIZ^.DIZ^;
        if (PF^.Attr and Directory) = 0 then begin
          PTDR^.Stream^.Seek(Round(PF^.PSize)); PTDR^.Stream^.Read(FC, SizeOf(FC));
          AvtNewFile(PAVT, S2, Desc, False, PF^.Size, PackedDate(PF), FC.StartSector, PF^.Attr);
          Inc(FilesCounter);
        end else begin
          Nm:=MakeFileName(PF^.Name);
          if (Nm <> '.') and (Nm <> '..') then begin
            if AvtNewFile(PAVT, S2, Desc, True, 0, PackedDate(PF), 0, PF^.Attr) <> 0 then begin
              S1:=S2+'\';
              WalkTree;
              Dec(S1[0]);
              while (S1<>'') and (S1[Length(S1)] <> '\') do begin
                 Dec(S1[0]);
              end;
            end;
          end;
        end;
      end;
      Dispose(PC,Done);
      Inc(DirCounter);
      Write('Directories processed: ',DirCounter,#13);
    end;
{ procedure do_tdr_to_avt }
begin
   prepare_avt;
   ARVID.ArvidWithDN:=False;
   PTDR:=New(PArvidDrive, Init(Path, 0));
   PAVT:=New(PArvidDrive, Init(Path2,0));
   PAVT^.Owner:=nil;
   PTDR^.Flags:= PTDR^.Flags or psShowDescript;
   S1:='\';
   WalkTree; WriteLn;
   WriteLn('Files processed: ',FilesCounter);
   PAVT^.Stream^.Seek(0); PAVT^.Stream^.Write(PAVT^.AVT, SizeOf(PAVT^.AVT));
   Dispose(PTDR,Done);
   Dispose(PAVT,Done);
   WriteLn('ArVid AVT "'+Path2+'" created by converting "',Path,'"');
end;

type
 PTempFile = ^TTempFile;
 TTempFile = object(TBufStream)
   FName: string;
   constructor Init;
   destructor Done; virtual;
 end;

constructor TTempFile.Init;
var
  s: string;
begin
  s:=CalcTmpFName(CalcTmpId, '$$$',false);
  inherited Init(s, stCreate, 4096);
  FName:=s;
end;

destructor TTempFile.Done;
var
  s: string;
begin
  s:=FName;
  inherited Done;
  EraseFile(s);
end;

procedure do_avt_to_tdr;
var
  Path, Path2, Dr,Nm,Ex,S1,S2,Desc: String;
  FileStream, DirStream, DescStream: PTempFile;
  PAVT: PArvidDrive;
   PF:  PFileRec;
   TFC: TTdrFileCell;
   TDC: TTdrDirCell;
   AFC: TAvtFileCell;
   DL:  Word;
   AVT: TAvtHeader;
   M:   TAvtMediaCell;
   RecordLen: LongInt;
   PosTable: array[1..4656] of char;
const
  DirsCounter:  LongInt = 0;
  FilesCounter: LongInt = 0;

   procedure check_avt;
   var
     IStream: PBufStream;
   label
     1, 2;
   begin
     lFSplit(ParamStr(1), Dr, Nm, Ex);
     if Ex = '' then Ex:='.AVT';
     Path:=Dr+Nm+Ex;
     if ExistFile(Path) = False then begin
       WriteLn('Error: file not exist "', Path, '"');
       Halt(254);
     end;
     IStream:=New(PBufStream, Init(Path, stOpenRead, 2048));
     if IStream^.Status <> stOK then begin
       WriteLn('Error: can not open ', Path);
       Halt(254);
     end;
     IStream^.Seek(0); IStream^.Read(AVT, SizeOf(AVT));
     if (AVT.Signature <> 'AVTP') or (AVT.AvtFmt <> 1) then begin
       1:  WriteLn('Error: invalid format of input AVT "', Path,'"');
           Halt(254);
     end;
     IStream^.Seek(AVT.AvtMediaCell); IStream^.Read(M, SizeOf(M));
     if ((M.TapeFmt <> 2) and (M.TapeFmt <> 4) and (M.TapeFmt <> 8))
          or (M.PositionTableSize <> 4656) then begin
       WriteLn('Error: "', Path,'" is not converted AVT.');
       Halt(254);
     end;
     IStream^.Seek(M.PositionTable); IStream^.Read(RecordLen, SizeOf(RecordLen));
     RecordLen:=RecordLen * 8;
     IStream^.Seek(M.PositionTable); IStream^.Read(PosTable, SizeOf(PosTable));
     Dispose(IStream,Done);
     Path2:=Dr+Nm+'.TDR';
     if ExistFile(Path2) = True then begin
       WriteLn('Error: file already exist "', Path2, '"');
       Halt(254);
     end;
   end;

   procedure WalkTree;
   var
      I:   Integer;
      PC:  PCollection;
   const
      CurDirFirstFileNum: LongInt = 0;
      DirLevel:           LongInt = 0;
      CurDirName:        Str12   = '            ';
      CurDirDescription: String  = '';
      CurDirTime:        LongInt = 0;
      CurDirAttr:        Byte    = Directory;
      J:                 Integer = 0;

   begin
      PAVT^.lChDir(S1);
      PC:=PAVT^.GetDirectory(31, 0, x_x, FreeStr, FreeStr);
      CurDirFirstFileNum:=FilesCounter;
      for I:=0 to PC^.Count - 1 do begin
        PF:=PC^.at(i);
        if (PF^.Attr and Directory) = 0 then begin
          for J:=1  to 8  do TFC.Name[J  ]:=UpCaseArray[PF^.Name[J]];
          for J:=10 to 12 do TFC.Name[J-1]:=UpCaseArray[PF^.Name[J]];
          TFC.Attr:=PF^.Attr;
          PAVT^.Stream^.Seek(Round(PF^.PSize)); PAVT^.Stream^.Read(AFC, SizeOf(AFC));
          TFC.StartSector:=AFC.StartSector;
          TFC.Description:=0;
          if (PF^.DIZ <> nil) and (PF^.DIZ^.DIZ <> nil) then begin
            TFC.Description:=DescStream^.GetPos + 1;
            Desc:=PF^.DIZ^.DIZ^;
            DL:=Length(Desc); DescStream^.Write(DL, SizeOf(DL));
            DescStream^.Write(Desc[1], DL);
          end;
          TFC.Res:=0;
          TFC.Time:=AFC.Time;
          TFC.Cluster:=0;
          TFC.Size:=AFC.ChildOrSize;
          FileStream^.Write(TFC, SizeOf(TFC));
          Inc(FilesCounter);
        end;
      end;
      TDC.Level:=DirLevel;
      for J:=1  to 8  do TDC.Name[J  ]:=UpCaseArray[CurDirName[J]];
      for J:=10 to 12 do TDC.Name[J-1]:=UpCaseArray[CurDirName[J]];
      TDC.Attr:=CurDirAttr;
      TDC.StartSector:=0;
      TDC.Description:=0;
      if CurDirDescription <> '' then begin
        TDC.Description:=DescStream^.GetPos + 1;
        DL:=Length(CurDirDescription); DescStream^.Write(DL, SizeOf(DL));
        DescStream^.Write(CurDirDescription[1], DL);
      end;
      TDC.Res:=0;
      TDC.Time:=CurDirTime;
      TDC.Cluster:=0;
      TDC.Size:=0;
      TDC.Files:=CurDirFirstFileNum;
      TDC.NumFiles:=FilesCounter - CurDirFirstFileNum;
      TDC.LastFile:=TDC.Files + TDC.NumFiles - 1;
      DirStream^.Write(TDC, SizeOf(TDC));
      Inc(DirsCounter);
      Inc(DirLevel);
      for I:=0 to PC^.Count - 1 do begin
        PF:=PC^.at(i);
        if ((PF^.Attr and Directory) <> 0) and (MakeFileName(PF^.Name) <> '..')
        and (MakeFileName(PF^.Name) <> '.') then begin
          CurDirName:=PF^.Name;
          CurDirDescription:='';
          if (PF^.DIZ <> nil) and (PF^.DIZ^.DIZ <> nil) then
            CurDirDescription:=PF^.DIZ^.DIZ^;
          CurDirTime:=PackedDate(PF);
          CurDirAttr:=PF^.Attr;
          S1:=UpStrg(MakeNormName(S1, MakeFileName(PF^.Name))) + '\';
          WalkTree;
          Dec(S1[0]);
          while (S1 <> '') and (S1[Length(S1)]<>'\') do Dec(S1[0]);
        end;
      end;
      Dec(DirLevel);
      Dispose(PC,Done);
      Write('Directories processed: ',DirsCounter,#13);
      if DirLevel = 0 then begin
        WriteLn('Directories processed: ',DirsCounter);
        WriteLn('Files processed: ',FilesCounter);
        TDC.Level:=0;
        DirStream^.Write(TDC.Level, SizeOf(TDC.Level));
      end;
   end;

   procedure link_tdr;
   var
     OStream: PBufStream;
     TDR:     TTdrHeader;
     Buf:     array[1..512] of char;
     I:       Integer;
     Len:     LongInt;
   begin
     OStream:=New(PBufStream, Init(Path2, stCreate, 2048));
     if OStream^.Status <> stOK then begin
       WriteLn('Error: can not create "', Path2, '"');
       Halt(254);
     end;
     TDR.FileTableLen:=FilesCounter*SizeOf(TTdrFileCell);
     TDR.DirTableLen :=DirsCounter*SizeOf(TTdrDirCell) + 2;
     TDR.DescTableLen:=DescStream^.GetPos;
     TDR.PosTableLen :=M.PositionTableSize;
     TDR.FileTableOfs:=SizeOf(TTdrHeader);
     TDR.DirTableOfs :=TDR.FileTableOfs + TDR.FileTableLen;
     TDR.DescTableOfs:=TDR.DirTableOfs  + TDR.DirTableLen;
     TDR.PosTableOfs :=((TDR.DescTableOfs + TDR.DescTableLen + 511) div 512) * 512;
     TDR.TapeFmt     :=M.TapeFmt;
     TDR.TapeID      :=M.TapeID;
     TDR.TapeLen     :=M.TapeLen * 60;
     TDR.RecordLen   :=RecordLen;
     TDR.NewRecordSector:=AVT.NewSector;
     for I:=0 to 16 do TDR.Res01[I] := 0;
     for I:=0 to 16 do TDR.Res02[I] := 0;
     for I:=0 to 16 do TDR.Res03[I] := 0;
     TDR.LastNewRecordSector := AVT.LastNewSector;
     OStream^.Write(TDR, SizeOf(TDR));
     Len:=FileStream^.GetPos; FileStream^.Seek(0);
     while Len <> 0 do begin
       if Len > SizeOf(Buf) then I:=SizeOf(Buf) else I:=Len;
       FileStream^.Read(Buf, I); OStream^.Write(Buf, I);
       Dec(Len,I);
     end;
     Len:=DirStream^.GetPos; DirStream^.Seek(0);
     while Len <> 0 do begin
       if Len > SizeOf(Buf) then I:=SizeOf(Buf) else I:=Len;
       DirStream^.Read(Buf, I); OStream^.Write(Buf, I);
       Dec(Len,I);
     end;
     Len:=DescStream^.GetPos; DescStream^.Seek(0);
     while Len <> 0 do begin
       if Len > SizeOf(Buf) then I:=SizeOf(Buf) else I:=Len;
       DescStream^.Read(Buf, I); OStream^.Write(Buf, I);
       Dec(Len,I);
     end;
     I:=TDR.PosTableOfs - (TDR.DescTableOfs + TDR.DescTableLen);
     OStream^.Write(Buf, I);
     OStream^.Write(PosTable, SizeOf(PosTable));
     Dispose(OStream,Done);
   end;

{ procedure do_avt_to_tdr }
begin
   check_avt;
   FileStream:=New(PTempFile, Init);
   DirStream:=New(PTempFile, Init);
   DescStream:=New(PTempFile, Init);
   ARVID.ArvidWithDN:=False;
   PAVT:=New(PArvidDrive, Init(Path,0));
   PAVT^.Owner:=nil;
   PAVT^.Flags:= PAVT^.Flags or psShowDescript;
   S1:='\';
   WalkTree;
   link_tdr;
   Dispose(FileStream,Done);
   Dispose(DirStream,Done);
   Dispose(DescStream,Done);
   Dispose(PAVT,Done);
   WriteLn('ArVid TDR "'+Path2+'" created by converting "',Path,'"');
end;

procedure do_new_avt;
var
  Path, Dr,Nm,Ex: String;
  Stream: PBufStream;
const
  AVT: TAvtHeader =
        (Signature:     'AVTP';
         AvtFmt:         0;
         CheckSum:       0;
         AfterLastCell:  40;
         FreeCell:       0;
         RootDirCell:    0;
         NewSector:      0;
         LastNewSector:  0;
         AvtMediaCell:   0;
         Undefined1:     0);
{ procedure do_new_avt }
begin
  lFSplit(ParamStr(1), Dr, Nm, Ex);
  if Ex = '' then Ex:='.AVT';
  Path:=Dr+Nm+Ex;
  if ExistFile(Path) then begin
    WriteLn('Error: file exist "', Path, '"');
    Halt(254);
  end;
  Stream:=New(PBufStream, Init(Path, stCreate, 2048));
  if Stream^.Status <> stOK then begin
    WriteLn('Error: can not create ', Path);
    Halt(254);
  end;
  Stream^.Write(AVT, SizeOf(AVT));
  Dispose(Stream,Done);
  WriteLn('Psevdo AVT "'+Path+'" created.');
end;

Var
 S:   String;
 C:   Integer;

{ program TDR2AVT }
begin
 WriteLn('tdr2avt, v0.1, written by SeYKo 19-Jun-1999');
 C:=ParamCount;
 if (C < 1) or (C > 2) then Usage;
 if C = 2 then begin
   S:=ParamStr(2);
   if S[1] = '-'  then S[1]:='/';
   if S[1] <> '/' then Usage;
   case S[2] of
     'r','R': do_avt_to_tdr;
     'n','N': do_new_avt;
   else
     Usage;
   end;
 end else
  do_tdr_to_avt;
end.
