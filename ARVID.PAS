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

unit Arvid;

interface

uses
  Collect, Objects, Dos, Drives, FilesCol, Views, DiskInfo, Tree,
    Histries,
  Lfn, Files, FindObj;

type
  TTdrHeader = record
    FileTableOfs: longInt;
    DirTableOfs: longInt;
    PosTableOfs: longInt;

    FileTableLen: longInt;
    DirTableLen: longInt;
    PosTableLen: longInt;

    TapeFmt: AWord;
    TapeID: AWord;
    TapeLen: AWord;
    RecordLen: AWord;
    NewRecordSector: longInt;

    DescTableOfs: longInt;
    Res01: array[1..16] of byte;
    DescTableLen: longInt;
    Res02: array[1..16] of byte;
    LastNewRecordSector: longInt;
    Res03: array[1..36] of byte;
    end;

  PTdrFileCell = ^TTdrFileCell;
  TTdrFileCell = record
    Name: array[1..11] of Char;
    Attr: byte;
    StartSector: longInt;
    Description: longInt;
    Res: AWord;
    Time: longInt;
    Cluster: AWord;
    Size: longInt;
    end;

  TTdrDirCell = record
    Level: AWord;
    Name: array[1..11] of Char;
    Attr: byte;
    StartSector: longInt;
    Description: longInt;
    Res: AWord;
    Time: longInt;
    Cluster: AWord;
    Size: longInt;

    Files: AWord;
    LastFile: AWord;
    NumFiles: AWord;

  end;

type
  TAvtHeader = record
    signature: longInt; {'AVTP' = $50545641}
    AvtFmt: longInt;
    CheckSum: longInt;
    AfterLastCell: longInt;
    FreeCell: longInt;
    RootDirCell: longInt;
    NewSector: longInt;
    LastNewSector: longInt;
    AvtMediaCell: longInt;
    Undefined1: longInt;
    end;

  TAvtMediaCell = record
    NextMediaCell: longInt;
    TapeFmt: AWord; {16,32}
    TapeLen: AWord;
    TapeID: longInt;
    Undefined1: longInt;
    Undefined2: longInt;
    Undefined3: longInt;
    FirstSectorNum: longInt;
    Sectors: longInt;
    PositionTable: longInt;
    PositionTableSize: longInt;
    end;

  TAvtFileCell = record
    LeftFileCell: longInt;
    RightFileCell: longInt;
    ChildOrSize: longInt;
    Time: longInt;
    StartSector: longInt;
    Flags: AWord;
    Attr: AWord;
    case Char of
      0: (Name0: array[1..16] of Char);
      1: (Name1: array[1..12] of Char; Next1: longInt);
      2: (Name2: array[1..12] of Char; DescPtr2: longInt);
      3: (NamePtr, NonUsed1, NonUsed2, DescPtr3: longInt);
  end;

TAvtTextCell = record
  NextTextCell: longInt;
  Data: array[1..36] of Char;
  end;

const
  avtLogSectors = $00FF; { Mask's for TAvtFileCell.Flags }
  avtBalance = $0300;
  avtCellFormat = $0C00;
  avtIsDir = $1000;

  avtCell0 = $0000;
  avtCell1 = $0400;
  avtCell2 = $0800;
  avtCell3 = $0C00;

type
  TAvdType = (avdTdr, avdAvt);

  PArvidDrive = ^TArvidDrive;
  TArvidDrive = object(TDrive)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Name: PString;
    Stream: PStream;
    CurFile: longInt;
    CurDirPos: longInt;
    PosTableOfs: longInt;
    CurFileNum: AWord;
    CurLevel: AWord;
    CurDate: longInt;
    KillAfterUse: boolean;
    filetype: TAvdType;
    D: TTdrHeader;
    AVT: TAvtHeader;
    TapeFmt: AWord;
    TapeTotalTime: AWord;
    TapeRecordedTime: AWord;
    TotFiles: longInt; {in directory}
    TotLen: longInt;
    CurDirCellPos: longInt;
      {this for AVT: location of directory cell.
                               CurDirPos for AVT is position of a root node}

    Constructor Init(const AName: String; Num: byte);
    procedure lChDir(ADir: String); virtual;
    function GetDir: String; virtual;
    function GetDirectory(SortMode, PanelFlags: integer; const
      FileMask: String;
    var FreeSpace: String; var TotalInfo: TSize): PCollection;
      virtual;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream); virtual;
    procedure RereadDirectory(s: String); virtual;
    procedure KillUse; virtual;
    procedure UseFile(P: PFileRec; Command: word); virtual;
    procedure CopyFiles(AFiles: PCollection; Own: PView; MoveMode:
      boolean); virtual;
    procedure CopyFilesInto(AFiles: PCollection; Own: PView;
      MoveMode: boolean); virtual;
    procedure EraseFiles(AFiles: PCollection); virtual;
    function GetRealName: String; virtual;
    function GetInternalName: String; virtual;
    procedure HandleCommand(Command: word; InfoPtr: Pointer);
      virtual;
    procedure MakeDir; virtual;
    function isUp: boolean; virtual;
    procedure ChangeUp(var s: String); virtual;
    procedure ChangeRoot; virtual;
    procedure GetFreeSpace(var s: String); virtual;
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    procedure EditDescription(PF: PFileRec); virtual;
    procedure GetDirLength(PF: PFileRec); virtual;
    procedure GetParam(n: byte); virtual; {DataCompBoy}
    destructor Done; virtual;
    procedure SeekDirectory;
    function OpenDirectory(const Dir: String): PDrive; virtual;
    private
    procedure Kill;
    end;

const
  ArvidDrives: PCollection = nil;
  ArvidWithDN: boolean = True;

var
  AllFiles: boolean;

implementation
uses
  advance, advance2, advance1, advance3, FlPanel, Commands, Startup,
    DNApp,
  Drivers, Messages, Dialogs, Memory, FileFind, DNUtil, FileCopy,
    U_KeyMap,
  ArvidAvt, ArvidTdr;

{-DataCompBoy-}
procedure TArvidDrive.GetParam;
  begin
    Param := n;
    with ColumnsDefaultsArvd do
      begin
        Flags := Params[n].Param;
        LFNLen := StoI(Params[n].LFNLen);
        if (LFNLen = 0) or (LFNLen > 252) then
          LFNLen := 252;
        EXTLen := StoI(Params[n].EXTLen);
        if EXTLen > 252 then
          EXTLen := 252;
        case FileCase of
          0:
            FilFLP := 0;
          1:
            FilFLP := flnLowCase;
          2:
            FilFLP := flnCapitalCase;
          3:
            FilFLP := flnUpCase;
          4:
            FilFLP := 65535;
        end {case};
        case DirsCase of
          0:
            DirFLP := 0;
          1:
            DirFLP := flnLowCase;
          2:
            DirFLP := flnCapitalCase;
          3:
            DirFLP := flnUpCase;
          4:
            DirFLP := 65535;
        end {case};
      end;
  end { TArvidDrive.GetParam };
{-DataCompBoy-}

Constructor TArvidDrive.Init;
  var
    Attrb: word;
    i: integer;
    Q: String;

  procedure GetAttr;
    var
      F: lFile;
    begin
      lAssignFile(F, AName);
      lGetFAttr(F, Attrb)
    end;

  procedure SetAttr(A: word);
    var
      F: lFile;
    begin
      lAssignFile(F, AName);
      lSetFAttr(F, A)
    end;

  label 1;

  begin { TArvidDrive.Init }
    TObject.Init;
    if Num < 1 then
      GetParam(1)
    else
      GetParam(Num);
    innum := Num;

    i := PosChar(':', Copy(AName, 3, MaxStringLength))+2;
    if i > 2 then
      begin
        Q := Copy(AName, i+1, MaxStringLength);
        if Q[Length(Q)] in ['\', '/'] then
          SetLength(Q, Length(Q)-1);
        Stream := New(PBufStream, Init(Copy(AName, 1, i-1), stOpen,
          2048));
      end
    else
      Stream := New(PBufStream, Init(AName, stOpen, 2048));

    if Stream^.Status <> stOK then
      begin
        GetAttr;
        if Attrb and ReadOnly <> 0 then
          begin
            SetAttr(Archive);
            if (IOResult <> 0) then
              begin
1:
                Dispose(Stream, Done);
                Stream := nil;
                Fail;
              end;
          end;
        if i > 0
        then
          Stream := New(PBufStream, Init(Copy(AName, 1, i-1), stOpen,
            2048))
        else
          Stream := New(PBufStream, Init(AName, stOpen, 2048));
        if Stream^.Status <> stOK then
          goto 1;
      end;
    Stream^.Read(AVT, SizeOf(AVT));
    if Stream^.Status <> stOK then
      goto 1;
    if AVT.signature <> $50545641 {'AVTP'}
    then
      if not TdrInit(@Self) then
        goto 1
      else
    else if not AvtInit(@Self) then
      goto 1;
    Name := NewStr(lFExpand(AName));
    DriveType := dtArvid;
    KillAfterUse := TempFile <> '';
    TempFile := '';
    if i > 2 then
      CurDir := Q
    else
      CurDir := '\';
    SeekDirectory;
    if ArvidDrives = nil then
      New(ArvidDrives, Init($100, $100));
    ArvidDrives^.Insert(@Self);
    AddToDirectoryHistory(Name^+':'+CurDir, integer(DriveType));
  end { TArvidDrive.Init };

procedure TArvidDrive.SeekDirectory;
  begin
    if filetype = avdTdr then
      TdrSeekDirectory(@Self)
    else
      AvtSeekDirectory(@Self);
  end;

procedure TArvidDrive.Kill;
  begin
    if not KillAfterUse then
      exit;
    if Stream <> nil then
      Dispose(Stream, Done);
    Stream := nil;
    EraseTempFile(Name^);
  end;

function TArvidDrive.GetDirectory;
  var
    FC: PFilesCollection;
    P: PString;
    DT: DateTime;
    TAttr: word;
  begin
    New(FC, Init($80, $40));
    FC^.SortMode := SortMode;
    {$IFDEF Win32}
    FC^.LFNActive := False;
    {$ENDIF}
    if ArvidWithDN then
      GetFreeSpace(FreeSpace);
    TotFiles := 0;
    TotLen := 0;

    GetDirectory := FC;
    AllFiles := (FileMask = x_x) or (FileMask = '*');

    if filetype = avdTdr then
      TdrGetDirectory(@Self, CurDirPos, FC, FileMask)
    else
      AvtGetDirectory(@Self, CurDirPos, FC, FileMask);

    if CurDir = '' then
      P := Name
    else
      P := @CurDir;

    TotalInfo := TotLen;
    {$IFNDEF OS2}
    FC^.AtInsert(0, NewFileRec('..', '..', 0, CurDate, 0, 0,
      Directory, P));
    {$ELSE}
    FC^.AtInsert(0, NewFileRec('..', 0, CurDate, 0, 0, Directory, P));
    {$ENDIF}
  end { TArvidDrive.GetDirectory };

destructor TArvidDrive.Done;
  begin
    if ArvidDrives <> nil then
      begin
        ArvidDrives^.Delete(@Self);
        if ArvidDrives^.Count = 0 then
          Dispose(ArvidDrives, Done);
        ArvidDrives := nil;
      end;
    if Stream <> nil then
      Dispose(Stream, Done);
    Stream := nil;
    DisposeStr(Name);
    inherited Done;
  end;

procedure TArvidDrive.lChDir;
  var
    dr: String;
    Nm: String;
    XT: String;
  begin
    if ADir = #0 then
      exit;
    lFSplit(ADir, dr, Nm, XT);
    if (Nm = '.') and (XT = '.') then
      begin
        if dr <> '' then
          CurDir := dr;
        if CurDir[1] <> '\' then
          Insert('\', CurDir, 1);
        repeat
          SetLength(CurDir, Length(CurDir)-1)
        until (CurDir = '') or (CurDir[Length(CurDir)] = '\');
        if CurDir <> '' then
          SetLength(CurDir, Length(CurDir)-1);
      end
    else
      CurDir := ADir;
    SeekDirectory;
    while (PosChar(CurDir[Length(CurDir)], ' .\') > 0) do
      SetLength(CurDir, Length(CurDir)-1);
    AddToDirectoryHistory(Name^+':'+CurDir, integer(DriveType));
  end { TArvidDrive.lChDir };

function TArvidDrive.GetDir;
  var
    dr: String;
    Nm: String;
    XT: String;
  begin
    lFSplit(Name^, dr, Nm, XT);
    if filetype = avdTdr
    then
      GetDir := 'TDR:'+Nm+'\'+CurDir
    else
      GetDir := 'AVT:'+Nm+'\'+CurDir;
  end;

Constructor TArvidDrive.Load;
  label 1;
  begin
    TObject.Init;
    inherited Load(s);
    DriveType := dtArvid;
    s.Read(KillAfterUse, 1);
    Name := s.ReadStr;
    Stream := New(PBufStream, Init(Name^, stOpen, 2048));
    if Stream^.Status <> stOK then
      begin
1:
        Done;
        {      Drives.DriveLoadingError:=True;}
        Fail;
      end;
    Stream^.Read(AVT, SizeOf(AVT));
    if Stream^.Status <> stOK then
      goto 1;
    if AVT.signature <> $50545641 {'AVTP'}
    then
      if not TdrInit(@Self) then
        goto 1
      else
    else if not AvtInit(@Self) then
      goto 1;
    SeekDirectory;
    if ArvidDrives = nil then
      New(ArvidDrives, Init($100, $100));
    ArvidDrives^.Insert(@Self);
  end { TArvidDrive.Load };

procedure TArvidDrive.Store;
  begin
    inherited Store(s);
    s.Write(KillAfterUse, 1);
    s.WriteStr(Name);
  end;

procedure TArvidDrive.RereadDirectory;
  begin
    if Prev <> nil then
      Prev^.RereadDirectory(s);
    if filetype = avdAvt then
      begin
        Dispose(Stream, Done);
        Stream := New(PBufStream, Init(Name^, stOpen, 2048));
        Stream^.Seek(0);
        Stream^.Read(AVT, SizeOf(AVT));
        SeekDirectory;
      end;
  end;

procedure TArvidDrive.KillUse;
  begin
    if Prev <> nil then
      Prev^.KillUse;
    Kill;
  end;

procedure TArvidDrive.UseFile;
  begin
    {  MessageBox('TArvidDrive.UseFile', nil, mfError + mfOKButton);}
  end;

{ CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual; }
procedure TArvidDrive.CopyFiles;
  var
    i, j: integer;
    CopyOpt: word;
    CopyMode: word;
    CopyPrn: boolean;
    CmdFileCreated: boolean;
    OwnerPtr: PView;
    P: PView;
    t: lText;
    PF: PFileRec;
    FC, FC2: TAvtFileCell;
    Nam, Desc: String;
    CopyDir: String;
    Mask: String;
    s1, s2, S3: String;
    CmdFileNam: String;
    dr: String;
    Nm: String;
    XT: String;

  procedure CopyTree(Pos: longInt);
    begin
      if Pos = 0 then
        exit;
      Stream^.Seek(Pos);
      Stream^.Read(FC, SizeOf(FC));
      if (FC.Flags and avtIsDir) <> 0 then
        begin
          FC2 := FC;
          FC.ChildOrSize := 0;
        end;
      Nam := AvtCellName(FC, Stream^);
      Desc := AvtCellDesc(FC, Stream^);
      if AvtNewFile(@Self, s2+Nam, Desc, (FC.Flags and avtIsDir) <> 0,
          FC.ChildOrSize,
        FC.Time, FC.StartSector, FC.Attr) = 0
      then
        begin
          MessageBox(GetString(dlFBBOver1)+s2+Nam, nil, mfError+
            mfOKButton);
        end
      else
        begin
          if (FC.Flags and avtIsDir) <> 0 then
            begin
              s1 := s1+Nam+'\';
              s2 := s2+Nam+'\';
              CopyTree(FC2.ChildOrSize);
              SetLength(s1, Length(s1)-1);
              SetLength(s2, Length(s2)-1);
              while (s1 <> '') and (s1[Length(s1)] <> '\') do
                begin
                  SetLength(s1, Length(s1)-1);
                  SetLength(s2, Length(s2)-1);
                end;
              Stream^.Seek(Pos);
              Stream^.Read(FC, SizeOf(FC));
            end;
          Stream^.Seek(Pos);
          Stream^.Read(FC, SizeOf(FC));
          CopyTree(FC.LeftFileCell);
          Stream^.Seek(Pos);
          Stream^.Read(FC, SizeOf(FC));
          CopyTree(FC.RightFileCell);
        end;
    end { CopyTree };

  { CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual; }
  begin { TArvidDrive.CopyFiles }
    CtrlBreakHit := False;
    AFiles^.Pack;
    if AFiles^.Count <= 0 then
      exit;
    lFSplit(Name^, dr, Nm, XT);
    if filetype = avdTdr then
      s1 := dr+'TDR:'+Nm+'\'
    else
      s1 := dr+'AVT:'+Nm+'\';
    Message(Application, evCommand, cmPushFirstName, @CopyDirName);
    if not CopyDialog(CopyDir, Mask, CopyOpt, CopyMode, CopyPrn,
      MoveMode, AFiles, 0, Owner, True)
    then
      exit;

    if CopyDir[Length(CopyDir)] <> '\' then
      CopyDir := CopyDir+'\';
    if Pos(s1, CopyDir) = 1 then
      begin
        if filetype = avdTdr then
          begin
            MessageBox(GetString(dlArvidCanChangeOnlyAVT), nil,
              mfInformation+mfOKButton);
            exit;
          end;
        P := WriteMsg(GetString(dlPleaseStandBy));
        while Length(s1) <> 1 do
          begin
            Delete(s1, 1, 1); {DelFC(S1);}Delete(CopyDir, 1, 1);
              {DelFC(CopyDir);}
          end;
        s2 := CurDir;
        for i := 0 to AFiles^.Count-1 do
          begin
            PF := AFiles^.At(i);
            Stream^.Seek( {Cat:warn}Round(PF^.PSize));
            Stream^.Read(FC, SizeOf(FC));
            Desc := AvtCellDesc(FC, Stream^);
            s1 := MakeNormName('\'+CurDir, PF^.FlName[True]);
            s2 := MakeNormName(CopyDir, MkName(PF^.FlName[True],
              Mask));
            if Pos(s1, s2) = 1 then
              begin
                MessageBox(GetString(erIntoItself), nil, mfError+
                  mfOKButton);
              end
            else
              begin
                if MoveMode then
                  begin{move}
                    FC2 := FC;
                    FC2.ChildOrSize := 0;
                    Stream^.Seek( {Cat:warn}Round(PF^.PSize));
                    Stream^.Write(FC2, SizeOf(FC2));
                    AvtDelFile(@Self, s1);
                    if AvtNewFile(@Self, s2, Desc, (FC.Flags and
                        avtIsDir) <> 0,
                      FC.ChildOrSize, FC.Time, FC.StartSector, FC.
                        Attr) = 0
                    then
                      begin
                        AvtNewFile(@Self, s1, Desc, (FC.Flags and
                          avtIsDir) <> 0,
                        FC.ChildOrSize, FC.Time, FC.StartSector, FC.
                          Attr);
                        MessageBox(GetString(dlFBBOver1)+s2, nil,
                          mfError+mfOKButton);
                      end;
                  end
                else
                  begin{copy}
                    if (PF^.Attr and Directory <> 0) then
                      begin
                        FC2 := FC;
                        FC.ChildOrSize := 0;
                      end;
                    if AvtNewFile(@Self, s2, Desc, (FC.Flags and
                        avtIsDir) <> 0,
                      FC.ChildOrSize, FC.Time, FC.StartSector, FC.
                        Attr) = 0
                    then
                      begin
                        MessageBox(GetString(dlFBBOver1)+s2, nil,
                          mfError+mfOKButton);
                      end
                    else if (PF^.Attr and Directory <> 0) then
                      begin
                        s1 := s1+'\';
                        s2 := s2+'\';
                        CopyTree(FC2.ChildOrSize);
                      end;
                  end;
              end;
          end;
        P^.Free;
        Stream^.Seek(0);
        Stream^.Write(AVT, SizeOf(AVT));
        Dispose(Stream, Done);
        Stream := New(PBufStream, Init(Name^, stOpen, 2048));
        GlobalMessage(evCommand, cmPanelReread, nil);
        exit;
      end
    else
      begin
        s2 := CopyDir;
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
        if TapeFmt = 0 then
          begin
            MessageBox(GetString(dlArvidVolumeIsNotTape), nil,
              mfError+mfOKButton);
            exit;
          end;
        s2 := UpStrg(CopyDir);
        i := Pos(':', s2);
        if (i <> 2) or (s2[1] < 'A') or (s2[1] > 'Z') then
          begin
            MessageBox(GetString(dlArvidNeedDisk), nil, mfError+
              mfOKButton);
            exit;
          end;
        CmdFileNam := MakeNormName(dr, Nm+'.RD');
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
            s2 := MakeNormName(CopyDir, MkName(Mask, PF^.FlName[True]));
            if (PF^.Attr and Directory) = 0 then
              Writeln(t.t, 'COPY TP:'+SquashesName(s1)+' '+
                SquashesName(s2)+' /O/R/C/H')
            else
              begin
                MkDir(s2);
                Writeln(t.t, 'COPYDIR TP:'+SquashesName(s1)+' '+
                  SquashesName(s2)+' /I/O/R/C/H')
              end;
          end;
        P^.Free;
        if CmdFileCreated then
          MessageBox(GetString(dlArvidCmdFileCreated)+CmdFileNam,
          nil, mfInformation+mfOKButton)
        else
          MessageBox(GetString(dlArvidCmdFileAppended)+CmdFileNam,
          nil, mfInformation+mfOKButton);
        Close(t.t);
        GlobalMessage(evCommand, cmPanelReread, nil);
        exit;
      end;
  end { TArvidDrive.CopyFiles };

{ CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;}
procedure TArvidDrive.CopyFilesInto;
  begin
    AvtCopyFilesInto(@Self, AFiles, Own, MoveMode);
  end;

procedure TArvidDrive.EraseFiles;
  begin
    AvtEraseFiles(@Self, AFiles);
  end;

procedure TArvidDrive.MakeDir;
  begin
    AvtMakeDir(@Self)
  end;

function TArvidDrive.isUp;
  begin
    isUp := True;
  end;

procedure TArvidDrive.ChangeUp;
  begin
    if CurDir <> '' then
      begin
        s := GetName(CurDir);
        lChDir('..');
        exit
      end;
    if Owner = nil then
      exit;
    if Prev = nil then
      begin
        New(Prev, Init(0, Owner, Flags));
        if Prev = nil then
          exit;
      end;
    PFilePanel(Owner)^.Drive := Prev;
    Prev^.lChDir(Prev^.CurDir);
    if (Prev^.DriveType = dtDisk) and
      (PView(Owner)^.GetState(sfSelected+sfActive))
    then
      ActivePanel := Owner;
    GlobalMessage(evCommand, cmRereadInfo, nil);
    Prev := nil;
    s := GetName(Name^);
    Kill;
    Dispose(PDrive(@Self), Done);
  end { TArvidDrive.ChangeUp };

procedure TArvidDrive.ChangeRoot;
  begin
    CurDir := '';
    SeekDirectory;
  end;

procedure TArvidDrive.GetFreeSpace;
  var
    M, R: longInt;
    l: array[1..2] of longInt;
    A: TAvtMediaCell;
  begin
    case TapeFmt of
      0:
        M := 0;
      2, 4:
        M := 100;
      8:
        M := 200;
      16:
        M := 200;
      32:
        M := 325;
      else
        M := 325;
    end {case};

    if filetype = avdTdr
    then
      l[1] := Max(0, (longInt(D.TapeLen)-D.RecordLen) div 60)
    else
      l[1] := Max(0, (longInt(TapeTotalTime)-TapeRecordedTime) div 60);
    l[2] := (l[1]*60*M) div 1024;
    FormatStr(s, GetString(dlArvid_TimeLeft), l);
  end { TArvidDrive.GetFreeSpace };

function TArvidDrive.GetRealName;
  var
    dr: String;
    Nm: String;
    XT: String;
  begin
    lFSplit(Name^, dr, Nm, XT);
    GetRealName := dr+GetDir;
  end;

function TArvidDrive.GetInternalName;
  begin
    GetInternalName := '';
  end;

procedure TArvidDrive.GetDirInfo;
  var
    l: array[1..2] of longInt;
    s, NF, Sz: longInt;
    s1, s2: String;
    A: TAvtMediaCell;
  begin
    B.Title := NewStr(GetString(dlArvid_Title));
    FreeStr := GetName(Name^);
    SetLength(FreeStr, PosChar('.', FreeStr)-1);
    B.Dir := NewStr(FreeStr);

    case TapeFmt of
      0:
        begin
          s := 000;
          B.VolumeID := NewStr(GetString(dlArvid_Type)+GetString(
            dlArvidNoReal))
        end;
      2:
        begin
          s := 100;
          B.VolumeID := NewStr(GetString(dlArvid_Type)+
            '~100~ Kb/s, CRC-16')
        end;
      4:
        begin
          s := 100;
          B.VolumeID := NewStr(GetString(dlArvid_Type)+
            '~100~ Kb/s, CRC-32')
        end;
      8:
        begin
          s := 200;
          B.VolumeID := NewStr(GetString(dlArvid_Type)+
            '~200~ Kb/s, CRC-32')
        end;
      16:
        begin
          s := 200;
          B.VolumeID := NewStr(GetString(dlArvid_Type)+
            '~200..300~ Kb/s')
        end;
      32:
        begin
          s := 325;
          B.VolumeID := NewStr(GetString(dlArvid_Type)+
            '~325..500~ Kb/s')
        end;
      else
        begin
          s := 325;
          B.VolumeID := NewStr(GetString(dlArvid_Type)+
            '~325..500~ Kb/s')
        end;
    end {case};

    if filetype = avdTdr then
      begin
        Sz := D.RecordLen*s;
        NF := D.FileTableLen div SizeOf(TTdrFileCell);
      end
    else
      begin
        Sz := TotLen div 1024;
        NF := TotFiles;
      end;

    if NF = 0 then
      B.Files := NewStr(GetString(dlDINoFiles))
    else
      begin
        if NF = 1 then
          s1 := GetString(dlDIFile)
        else
          s1 := GetString(dlDIFiles);
        s2 := GetString(dlDIBytes);
        B.Files := NewStr('~'+FStr(NF)+'~ '+s1+GetString(dlDIWith)+
          '~'+FStr(Sz)+'~K '+s2);
      end;

    if filetype = avdTdr then
      begin
        l[1] := longInt(TapeTotalTime) div 60;
        l[2] := (TapeTotalTime*s) div 1024;
        FormatStr(FreeStr, GetString(dlArvid_TimeUsed), l);
        B.Total := NewStr(FreeStr);
        l[1] := Max(0, (longInt(TapeTotalTime)-TapeRecordedTime) div 60)
          ;
        l[2] := (l[1]*60*s) div 1024;
        FormatStr(FreeStr, GetString(dlArvid_TimeLeft), l);
        B.Free := NewStr(FreeStr);
      end
    else
      begin
        l[1] := longInt(D.TapeLen) div 60;
        l[2] := (D.TapeLen*s) div 1024;
        FormatStr(FreeStr, GetString(dlArvid_TimeUsed), l);
        B.Total := NewStr(FreeStr);
        l[1] := Max(0, (longInt(D.TapeLen)-D.RecordLen) div 60);
        l[2] := (l[1]*60*s) div 1024;
        FormatStr(FreeStr, GetString(dlArvid_TimeLeft), l);
        B.Free := NewStr(FreeStr);
      end;
  end { TArvidDrive.GetDirInfo };

procedure TArvidDrive.EditDescription;
  var
    s: String;
    Nam: String;

  label 1;

  var
    RealAttr, Attrb: word;

  procedure GetAttr;
    var
      F: lFile;
    begin
      lAssignFile(F, Name^);
      lGetFAttr(F, Attrb)
    end;

  procedure SetAttr(A: word);
    var
      F: lFile;
    begin
      lAssignFile(F, Name^);
      lSetFAttr(F, A)
    end;

  procedure Err;
    begin
      CantWrite(Name^)
    end;

  { procedure TArvidDrive.EditDescription; }
  begin { TArvidDrive.EditDescription }
    if (PF^.DIZ = nil) then
      exit;
    s := CnvString(PF^.DIZ^.DIZ);
    if BigInputBox(GetString(dlEditDesc), GetString(dl_D_escription), s,
        255, hsEditDesc) <> cmOK
    then
      exit;
    Dispose(Stream, Done);
    Stream := nil;
    RealAttr := $FFFF;
    ClrIO;
    Abort := False;
    GetAttr;
    if Attrb and ReadOnly <> 0 then
      begin
        SetAttr(Archive);
        if Abort or (IOResult <> 0) then
          begin
            Err;
            goto 1
          end;
        RealAttr := Attrb or Archive;
      end;
    Stream := New(PBufStream, Init(Name^, stOpen, 2048));
    if Abort or (Stream^.Status <> stOK) then
      begin
        Err;
        goto 1
      end;
    Stream^.Seek(PF^.DIZ^.Line);
    if filetype = avdTdr then
      TdrEditDescription(@Self, s, Nam, PF)
    else
      AvtEditDescription(@Self, s, Nam);
1:
    ClrIO;
    Abort := False;
    Stream^.Seek(0);
    if filetype = avdAvt then
      Stream^.Write(AVT, SizeOf(AVT))
    else
      Stream^.Write(D, SizeOf(D));
    Dispose(Stream, Done);
    if RealAttr <> $FFFF then
      SetAttr(RealAttr);
    Stream := New(PBufStream, Init(Name^, stOpen, 2048));
    GlobalMessage(evCommand, cmPanelReread, nil);
  end { TArvidDrive.EditDescription };

procedure TArvidDrive.HandleCommand;
  var
    DT: record
      Mask: String;
      text: String;
      o: word;
      end;
    SDesc: boolean;
    P: PView;
    DateAfter,
    DateBefore,
    SizeGreat,
    SizeLess: longInt;
    Attr: byte;
    OOM: boolean;
    WasTape: boolean;
    WasDir: boolean;
    dr: String;
    LP: longInt;
    St0: PStream;
    NName: String;
    AA0: TAvtFileCell;
    S0: String;

  label 1;

  procedure Add(P: Pointer; const Name: String);
    begin
      if FindList = nil then
        New(FindList, Init($100, $100));
      if OOM or (not MemOK) or (MaxAvail < (FindList^.Count+$200)*4)
      then
        begin
          Dispose(PObject(P), Done);
          OOM := True;
          exit;
        end;
      if not WasTape then
        FindList^.Insert(New(PFindObject, Init(Name)));
      if not WasDir then
        FindList^.Insert(New(PFindDir, Init(dr, LP)));
      WasTape := True;
      WasDir := True;
      FindList^.Insert(P);
    end;

  procedure TdrSearchInStream(ST: PStream; var D: TTdrHeader; const
      Name: String);
    var
      DD: TTdrDirCell;
      FF: TTdrFileCell;
      i, j: longInt;
      Lv0: boolean;
      LastLv: integer;
      SS: String[12];
    begin
      Lv0 := False;
      LastLv := 0;
      LP := D.DirTableOfs;
      repeat
        ST^.Seek(LP);
        ST^.Read(DD, SizeOf(DD));
        if DD.Level = 0 then
          if Lv0 then
            break
          else
            Lv0 := True;
        SS := DD.Name;
        Insert('.', SS, 9);
        if DD.Level > LastLv then
          dr := MakeNormName(dr, TdrMakeFileName(SS))
        else if DD.Level <= LastLv then
          begin
            repeat
              while (dr[Length(dr)] <> '\') and (dr <> '') do
                  SetLength(dr, Length(dr)-1);
              if dr <> '' then
                SetLength(dr, Length(dr)-1);
              Dec(LastLv);
            until DD.Level > LastLv;
            if dr = '' then
              dr := '\';
            dr := MakeNormName(dr, TdrMakeFileName(SS))
          end;
        LastLv := DD.Level;
        WasDir := False;
        if InSpaceFilter(SS, DT.Mask) then
          begin
            if SDesc and (DD.Description <> 0) then
              begin
                ST^.Seek(D.DescTableOfs+DD.Description-1);
                {Cat:warn AnsiString}
                ST^.Read(FreeStr, 2);
                ST^.Read(FreeStr[1], Length(FreeStr));
                UpStr(FreeStr);
              end
            else
              FreeStr := '';
            if (not SDesc or (Pos(DT.text, FreeStr) > 0)) and
              ((DT.o and 1 = 0) or
              (FF.Time <= DateBefore) and (FF.Time >= DateAfter) and
              (FF.Size >= SizeGreat) and (FF.Size <= SizeLess) and
              ((Attr = 0) or (FF.Attr and Attr <> 0)))
            then
              begin
                WasDir := True;
                Add(New(PFindDir, Init(dr, LP)), Name);
              end;
          end;
        if not OOM then
          begin
            ST^.Seek(D.FileTableOfs+longInt(DD.Files)*SizeOf(
              TTdrFileCell));
            for i := 1 to DD.NumFiles do
              begin
                ST^.Read(FF, SizeOf(FF));
                SS := FF.Name;
                Insert('.', SS, 9);
                if InSpaceFilter(SS, DT.Mask) then
                  begin
                    if SDesc and (FF.Description <> 0) then
                      begin
                        j := ST^.GetPos;
                        ST^.Seek(D.DescTableOfs+FF.Description-1);
                        {Cat:warn AnsiString}
                        ST^.Read(FreeStr, 2);
                        ST^.Read(FreeStr[1], Length(FreeStr));
                        ST^.Seek(j);
                        UpStr(FreeStr);
                      end
                    else
                      FreeStr := '';
                    if (not SDesc or (Pos(DT.text, FreeStr) > 0))
                        and
                      ((DT.o and 1 = 0) or
                      (FF.Time <= DateBefore) and (FF.Time >=
                        DateAfter) and
                      (FF.Size >= SizeGreat) and (FF.Size <=
                        SizeLess) and
                      ((Attr = 0) or (FF.Attr and Attr <> 0)))
                    then
                      begin
                        Add(New(PFindFile, Init(TdrMakeFileName(SS), FF.
                          Size, FF.Time)), Name);
                      end;
                  end;
              end;
          end;
        Inc(LP, SizeOf(DD));
      until (LP > D.DirTableOfs+D.DirTableLen) or OOM;
      if OOM then
        Application^.OutOfMemory;
    end { TdrSearchInStream };

  procedure AvtSearchInStream(l: longInt);
    var
      SaveWasDir: boolean;
      SaveLP: longInt;
    begin
      if l = 0 then
        exit;
      St0^.Seek(l);
      St0^.Read(AA0, SizeOf(AA0));
      if St0^.Status <> stOK then
        exit;
      AvtSearchInStream(AA0.LeftFileCell);
      St0^.Seek(l);
      St0^.Read(AA0, SizeOf(AA0));
      if St0^.Status <> stOK then
        exit;
      if AA0.Flags and avtIsDir <> 0 then
        begin
          SaveWasDir := WasDir;
          WasDir := False;
          SaveLP := LP;
          LP := l;
          dr := dr+AvtCellName(AA0, St0^)+'\';
          AvtSearchInStream(AA0.ChildOrSize);
          WasDir := SaveWasDir;
          LP := SaveLP;
          SetLength(dr, Length(dr)-1);
          while (dr[Length(dr)] <> '\') and (dr <> '') do
            SetLength(dr, Length(dr)-1);
        end
      else
        begin
          S0 := AvtCellName(AA0, St0^);
          if InSpaceFilter(S0, DT.Mask) then
            begin
              if SDesc then
                FreeStr := UpStrg(AvtCellDesc(AA0, St0^));
              if (not SDesc or (Pos(DT.text, FreeStr) > 0)) and
                ((DT.o and 1 = 0) or
                (AA0.Time <= DateBefore) and (AA0.Time >= DateAfter)
                  and
                (AA0.ChildOrSize >= SizeGreat) and (AA0.ChildOrSize <=
                  SizeLess))
              then
                begin
                  if WasDir = False then
                    begin
                      WasDir := True;
                      Add(New(PFindDir, Init(dr, LP)), NName);
                    end;
                  Add(New(PFindFile, Init(S0, AA0.ChildOrSize, AA0.
                    Time)), NName);
                end;
            end;
        end;
      St0^.Seek(l);
      St0^.Read(AA0, SizeOf(AA0));
      if St0^.Status <> stOK then
        exit;
      AvtSearchInStream(AA0.RightFileCell);
    end { AvtSearchInStream };

  procedure SearchInStream(ST: PStream; const Name: String);
    var
      DD: TTdrHeader;
      aa: TAvtHeader;
    begin
      WasTape := False;
      dr := '\';
      ST^.Seek(0);
      ST^.Read(aa, SizeOf(aa));
      if aa.signature = $50545641 {'AVTP'} then
        begin
          St0 := ST;
          NName := Name;
          AvtSearchInStream(aa.RootDirCell);
        end
      else
        begin
          ST^.Seek(0);
          ST^.Read(DD, SizeOf(DD));
          if DD.PosTableLen <> 4656 then
            exit;
          if DD.FileTableOfs <> SizeOf(TTdrHeader) then
            exit;
          if DD.DirTableOfs <> (DD.FileTableOfs+DD.FileTableLen)
          then
            exit;
          if DD.DescTableOfs < (DD.DirTableOfs+DD.DirTableLen) then
              exit;
          if DD.PosTableOfs <>
            ((DD.DescTableOfs+DD.DescTableLen+511) div 512*512)
          then
            exit;
          if DD.TapeLen < DD.RecordLen then
            exit;
          if DD.FileTableLen <> (DD.FileTableLen div SizeOf(
              TTdrFileCell)
            *SizeOf(TTdrFileCell))
          then
            exit;
          TdrSearchInStream(ST, DD, Name);
        end;
    end { SearchInStream };

  procedure SearchInAllFiles;
    var
      SR: SearchRec;
      ST: PBufStream;
      D: TTdrHeader;
    begin
      ClrIO;
      FindFirst(MakeNormName(GetPath(Name^), '*.TDR'), Archive+byte(
        Security)*Hidden+ReadOnly+SysFile, SR);
      while (DOSError = 0) and not Abort and not LowMemory do
        begin
          New(ST, Init(MakeNormName(GetPath(Name^), SR.Name),
            stOpenRead, 2048));
          if ST^.Status = stOK then
            begin
              SearchInStream(ST, SR.Name);
            end;
          Dispose(ST, Done);
          ClrIO;
          FindNext(SR);
        end;
      FindFirst(MakeNormName(GetPath(Name^), '*.AVT'), Archive+byte(
        Security)*Hidden+ReadOnly+SysFile, SR);
      while (DOSError = 0) and not Abort and not LowMemory do
        begin
          New(ST, Init(MakeNormName(GetPath(Name^), SR.Name),
            stOpenRead, 2048));
          if ST^.Status = stOK then
            begin
              SearchInStream(ST, SR.Name);
            end;
          Dispose(ST, Done);
          ClrIO;
          FindNext(SR);
        end;
    end { SearchInAllFiles };

  procedure ExecuteFindDialog;
    label 1;
    var
      D: PDialog;
      R: TRect;
      PL: PFindBox;
      P: PView;
      F: PFindObject;

    procedure DoCount(P: PFindObject);
      begin
        Inc(R.A.X, byte(P^.TT = ttFile));
      end;

    begin

      D := PDialog(LoadResource(dlgArvidFindResults));
      if D = nil then
        exit;

      R.Assign(58, 1, 59, 13);
      P := New(PScrollBar, Init(R));
      D^.Insert(P);

      R.Assign(2, 1, 58, 13);
      New(PL, Init(R, 1, PScrollBar(P)));
      PL^.NewLisT(FindList);
      D^.Insert(PL);

      R.A.X := 0;
      if (FindList <> nil) then
        FindList^.ForEach(@DoCount);

      FreeStr := FStr(R.A.X)+GetString(dlFilesFound);
      R.Assign(1, 13, 1+Length(FreeStr), 14);
      P := New(PStaticText, Init(R, FreeStr));
      P^.Options := P^.Options or ofCenterX;
      D^.Insert(P);

      PL^.Select;
      R.A.X := Desktop^.ExecView(D);
      R.A.Y := PL^.Focused;

      PL^.List := nil;

      Dispose(D, Done);

      if R.A.X = cmNo then
        FreeObject(FindList);

      if (R.A.X = cmOK) and (FindList <> nil) and (FindList^.Count > 0)
      then
        begin
          F := FindList^.At(R.A.Y);
          if F = nil then
            exit;
          FreeStr := '';
          for R.B.X := R.A.Y downto 0 do
            begin
              F := FindList^.At(R.B.X);
              if (F^.TT = ttDir) and (FreeStr = '') then
                FreeStr := CnvString(F^.text);
              if (F^.TT = ttTape) then
                break;
            end;
          if UpStrg(F^.text^) <> UpStrg(GetName(Name^)) then
            begin
              FreeObject(Stream);
              CurDir := FreeStr;
              FreeStr := MakeNormName(GetPath(Name^), F^.text^);
              DisposeStr(Name);
              Name := NewStr(FreeStr);
              Stream := New(PBufStream, Init(FreeStr, stOpenRead,
                2048));
              FreeStr := CurDir;
              CurDir := '';
              if Stream^.Status <> stOK then
                begin
1:
                  ChangeUp(FreeStr);
                  exit;
                end;
              Stream^.Read(Self.D, SizeOf(Self.D));
              if Stream^.Status <> stOK then
                goto 1;
            end;
          CurDir := FreeStr;
          if CurDir[1] = '\' then
            Delete(CurDir, 1, 1); {DelFC(CurDir);}
          SeekDirectory;
          F := FindList^.At(R.A.Y);
          if F^.TT = ttFile then
            FreeStr := MakeNormName(FreeStr, CnvString(PFindFile(F)^.
              Name));
          Message(Owner, evCommand, cmFindGotoFile, @FreeStr);
        end;

  end { ExecuteFindDialog };

  begin { TArvidDrive.HandleCommand }
    OOM := False;
    if Command = cmFindFile then
      begin
        DT.Mask := '';
        DT.text := '';
        DT.o := 0;
        if ExecResource(dlgArvidFileFind, DT) <> cmOK then
          goto 1;
        if DT.o and 1 <> 0 then
          begin
            DateAfter := ParseTime(AdvanceSearchData.After);
            DateBefore := ParseTime(AdvanceSearchData.Before);
            if DateBefore = 0 then
              DateBefore := $7FFFFFFF;
            SizeGreat := StoI(AdvanceSearchData.Greater);
            SizeLess := StoI(AdvanceSearchData.Less);
            if SizeLess = 0 then
              SizeLess := $7FFFFFFF;
            Attr := 0;
            if AdvanceSearchData.Attr and 1 <> 0 then
              Attr := Archive;
            if AdvanceSearchData.Attr and 2 <> 0 then
              Attr := Attr or SysFile;
            if AdvanceSearchData.Attr and 4 <> 0 then
              Attr := Attr or Hidden;
            if AdvanceSearchData.Attr and 8 <> 0 then
              Attr := Attr or ReadOnly;
          end;
        if DT.Mask = '' then
          DT.Mask := x_x;
        SDesc := DT.text <> '';
        UpStr(DT.text);
        if FindList <> nil then
          Dispose(FindList, Done);
        FindList := nil;
        P := WriteMsg(GetString(dlPleaseStandBy));
        if DT.o and 2 <> 0 then
          SearchInAllFiles
        else
          SearchInStream(Stream, GetName(Name^));
        P^.Free;
        if FindList = nil then
          begin
            MessageBox(^C+GetString(dlNoFilesFound), nil,
            mfInformation+mfOKButton);
            exit;
          end;
1:
        if FindList <> nil then
          ExecuteFindDialog;
      end;
    if FindList <> nil then
      Dispose(FindList, Done);
    FindList := nil;
  end { TArvidDrive.HandleCommand };

procedure TArvidDrive.GetDirLength(PF: PFileRec);
  var
    SaveDir: String;
    LL: TSize;
    P: PView;
  begin
    if (PF^.Attr and $80 <> 0) or (PF^.Attr and Directory = 0) then
        exit;
    SaveDir := CurDir;
    LL := 0;
    CurDir := MakeNormName(PF^.Owner^, PF^.FlName[True]);
    SeekDirectory;
    P := WriteMsg(GetString(dlPleaseStandBy));
    if filetype = avdTdr then
      TdrCalcTotal(@Self, CurDirPos, LL)
    else
      AvtCalcTotal(@Self, CurDirPos, LL);
    P^.Free;
    PF^.Size := LL;
    PF^.Attr := PF^.Attr or $80;
    CurDir := SaveDir;
    SeekDirectory;
  end { TArvidDrive.GetDirLength };

function TArvidDrive.OpenDirectory(const Dir: String): PDrive;
  begin
    OpenDirectory := nil;
  end;

end.
