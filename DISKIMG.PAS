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

unit DiskImg;

interface

uses
  FilesCol, Tree;

procedure UnpackDiskImages(AOwner: Pointer; Files: PFilesCollection);

implementation

uses
  Dos, Lfn, Messages, Views, Commands, Objects, DNApp, Drivers,
    advance,
  advance1, advance2, advance3, FileCopy, {DiskTool,}Gauge, xTime
  , Files
  ;

{Cat: добавил сюда эти типы, вместо того, чтобы подключать модуль DiskTool}
type
  PBootRec = ^TBootRec;
  TBootRec = record
    JMP: array[0..2] of byte;
    OEM: array[0..7] of Char;
    ByteSec: AWord;
    SecClu: byte;
    ResSect: AWord;
    FATCps: byte;
    RootEnt: AWord;
    TotalSc: AWord;
    BPB: byte;
    SecFAT: AWord;
    SecTrack: AWord;
    Sides: AWord;
    HiddenSc: longInt;
    NumSec: longInt;
    PhysNum: AWord;
    ExtBootS: byte;
    VolumeN: array[0..4] of byte;
    VolumeL: array[0..10] of Char;
    FilSysID: array[0..5] of Char;
    end;

  TDirRec = record
    Name: array[0..10] of Char;
    Attr: byte;
    Data: array[0..9] of byte;
    Date: longInt;
    Clus: AWord;
    len: longInt;
    end;
  {/Cat}

  {-DataCompBoy-}
function UnpackImage(const FName, DestDir: String; PInfo: PWhileView):
    boolean;
  var
    F: PBufStream;
    Error, Cancel: boolean;
    tmr: TEventTimer;

  procedure CheckCancel;
    begin
      if Cancel then
        exit;
      if TimerExpired(tmr) then
        begin
          DispatchEvents(PInfo, Cancel);
          Cancel := (Cancel or CtrlBreakHit) and (MessageBox(
            GetString(dlQueryAbort), nil, mfYesNoConfirm) = cmYes);
          CtrlBreakHit := False;
          NewTimer(tmr, 1);
        end;
    end;

  procedure DoIt;
    var
      BRec: PBootRec;
      FAT: PByteArray absolute BRec;

      Reserved: word;
      FatCopies: byte;
      SectPerClust: byte;
      BytesPerClu: word;
      SectPerFAT: word;
      SectLen: word;
      FirstClust: word;
      RootEntries: word;
      TotalClu: word;
      FATSize: longInt;
      NumSectors: longInt;

      VolumeLabel: String[13];
      fPos: longInt;
      SearchVolume: boolean; {piwamoto}
      DSKoffset: AWord; {piwamoto}

    procedure fSeek(Pos: longInt);
      begin
        if fPos = Pos then
          exit;
        fPos := Pos;
        F^.Seek(Pos);
      end;

    procedure fRead(var Buf; Count: word);
      begin
        F^.Read(Buf, Count);
        Inc(fPos, Count);
      end;

    function NextFAT(n: word): word;
      var
        D: word;
      begin
        if Error or (n < 2) or (n > TotalClu+2) then
          begin
            Error := True;
            exit
          end;
        D := (n*3) shr 1;
        D := FAT^[D]+FAT^[D+1] shl 8;
        if n and 1 = 0 then
          D := D and $FFF
        else
          D := D shr 4;
        NextFAT := D;
      end;

    function Fat2Sec(W: word): longInt;
      begin
        Fat2Sec := longInt(FirstClust+(W-2)*SectPerClust);
      end;

    procedure WriteFile(const ADir: String; const ANm: Str12; Clus:
        word;
      len, Date: longInt; Attr: byte);
      var
        B: PBufStream;
        Dir, FName: String;

      procedure TryInit;
        begin
          B := New(PBufStream, Init(FName, stCreate, $8000));
          if B^.Status <> stOK then
            begin
              Dispose(B, Done);
              B := nil;
            end;
        end;

      var
        CpyBuf: PByteArray;

      procedure Cpy;
        var
          A: word;
          LocErr: boolean;
        begin
          if Error or Cancel then
            exit;
          CtrlBreakHit := False;
          LocErr := True;
          repeat
            CheckCancel;
            if Cancel then
              break;
            if F^.Status <> stOK then
              exit;
            fSeek(Fat2Sec(Clus)*SectLen+DSKoffset);
            if F^.Status <> stOK then
              exit;
            A := Min(len, BytesPerClu);
            Dec(len, A);
            fRead(CpyBuf^, A);
            if F^.Status <> stOK then
              exit;
            B^.Write(CpyBuf^, A);
            if B^.Status <> stOK then
              exit;
            if len = 0 then
              begin
                LocErr := False;
                break;
              end;
            Clus := NextFAT(Clus);
          until Error;
          Error := Error or LocErr;
        end { Cpy };

      procedure SetFData;
        var
          F: lFile;
        begin
          FileMode := $22
            {open_access_ReadWrite + open_share_DenyWrite};
          lAssignFile(F, FName);
          lResetFile(F, 1);
          SetFTime(F, Date);
          Close(F.F);
          if Attr <> Archive then
            lSetFAttr(F, Attr);
        end;

      begin { WriteFile }
        if Error then
          exit;
        Dir := FmtStr(ADir, VolumeLabel);
        FName := MakeNormName(Dir, ANm);
        PInfo^.Write(3, LowStrg(GetName(FName)));
        TryInit;
        if B = nil then
          begin
            CreateDirInheritance(Dir, False);
            TryInit;
            if B = nil then
              exit;
          end;
        GetMem(CpyBuf, BytesPerClu);
        Cpy;
        FreeMem(CpyBuf, BytesPerClu);
        Dispose(B, Done);
        B := nil;
        if Error then
          EraseFile(FName)
        else
          SetFData;
      end { WriteFile };

    procedure ProcessDir(const Dir: String; Ofst: word;
        OfstIsCluster: boolean);
      var
        _Dir: PByteArray;
        dr: TDirRec;
        Nm: Str12;
        j, i, FileCount, MemSize: word;
      begin
        CtrlBreakHit := False;
        if OfstIsCluster then
          begin{subdir}
            MemSize := 0;
            FileCount := Ofst;
              {use FileCount for counting subdir's FAT entries}
            repeat{calc memsize for this subdir}
              FileCount := NextFAT(FileCount);
              Inc(MemSize);
            until (FileCount > $ff7);
            MemSize := MemSize*BytesPerClu;
            GetMem(_Dir, MemSize);
            if _Dir = nil then
              exit;
            i := 0;
            repeat
              fSeek(Fat2Sec(Ofst)*SectLen+DSKoffset);
              fRead(_Dir^[i], BytesPerClu);
              if F^.Status <> stOK then
                exit;
              i := i+BytesPerClu;
              Ofst := NextFAT(Ofst);
            until (Ofst > $ff7);
            FileCount := i div 32;
          end
        else
          begin{root dir}
            fSeek(longInt(Ofst)*SectLen+DSKoffset);
            MemSize := RootEntries*32;
            GetMem(_Dir, MemSize);
            fRead(_Dir^, MemSize);
            if F^.Status <> stOK then
              exit;
            FileCount := RootEntries;
          end;
        for i := 0 to FileCount-1 do
          begin
            CheckCancel;
            if Error or Cancel then
              break;
            Move(_Dir^[i*SizeOf(dr)], dr, SizeOf(dr));
            case dr.Name[0] of
              #$E5:
                continue;
              #0:
                break;
              #5:
                dr.Name[0] := #$E5;
            end {case};
            if dr.Attr and (VolumeID+SysFile+Hidden+ReadOnly) =
                VolumeID
            then{LFN bugfix by piwamoto}
              if SearchVolume then
                begin
                  SetLength(VolumeLabel, 11);
                    {VolumeLabel[0] := #11}
                  Move(dr.Name[0], VolumeLabel[1], 11);
                  DelRight(VolumeLabel);
                  VolumeLabel := VolumeLabel+'\';
                  break;
                end
              else
                continue;
            if SearchVolume then
              continue;
            if dr.Attr and VolumeID <> 0 then
              continue; {i'll skip VFAT names}
            SetLength(Nm, 12); {Nm[0] := #12;}
            Nm[9] := '.';
            Move(dr.Name[0], Nm[1], 8);
            Move(dr.Name[8], Nm[10], 3);
            j := 8;
            while (j > 0) and (Nm[j] = ' ') do
              begin
                Delete(Nm, j, 1);
                Dec(j);
              end;
            while Nm[Length(Nm)] = ' ' do
              SetLength(Nm, Length(Nm)-1); {Dec(Nm[0]);}
            if Nm[Length(Nm)] = '.' then
              SetLength(Nm, Length(Nm)-1); {Dec(Nm[0]);}

            if dr.Attr and Directory <> 0 then
              begin
                if not IsDummyDir(Nm) then
                  ProcessDir(Dir+Nm+'\', dr.Clus, True); {JO}
                continue;
              end;
            WriteFile(Dir, Nm, dr.Clus, dr.len, dr.Date, dr.Attr);
          end;
        FreeMem(_Dir, MemSize);
        _Dir := nil;
      end { ProcessDir };

    procedure ChkValid;
      begin
        if Error then
          exit;
        Error := True;
        case SectLen of
          128, 256, 512, 1024, 2048, 4096:
            ;
          else
            exit
        end {case};
        case FatCopies of
          1, 2:
            ;
          else
            exit
        end {case};
        Error := False;
      end;

    begin { DoIt }
      VolumeLabel := '';
      fPos := 0;
      SearchVolume := True;
      DSKoffset := 0;
      if F^.Status <> stOK then
        exit;
      GetMem(BRec, 4096);
      fRead(BRec^, 4096);
      if (BRec^.JMP[0] = $49) and (BRec^.JMP[1] = $4D) then
          {.DDI support by piwamoto}
        case BRec^.OEM[7] of
          #2:
            DSKoffset := $1E00;
          #4:
            DSKoffset := $2400;
          else
            DSKoffset := $1200; {#1,#3}
        end {case};

      {IBM diskimages support by piwamoto}
      if (BRec^.JMP[0] = $AA) and
        (BRec^.JMP[1] in [$58..$5a]) and
        (BRec^.JMP[2] = $f0)
      then
        DSKoffset := BRec^.ExtBootS+256*BRec^.VolumeN[0];

      if DSKoffset <> 0 then
        begin
          fSeek(DSKoffset);
          fRead(BRec^, 4096);
        end;

      SectPerFAT := BRec^.SecFAT;
      SectLen := BRec^.ByteSec;
      FatCopies := BRec^.FATCps;
      Reserved := BRec^.ResSect;
      RootEntries := BRec^.RootEnt;
      SectPerClust := BRec^.SecClu;
      if BRec^.TotalSc = 0 then
        NumSectors := BRec^.NumSec
      else
        NumSectors := BRec^.TotalSc;
      FreeMem(BRec, 4096);
      BRec := nil;
      if F^.Status <> stOK then
        exit;

      ChkValid;
      if Error then
        exit;

      FATSize := longInt(SectPerFAT)*SectLen;

      FirstClust := (SectPerFAT*FatCopies)+Reserved
      +((longInt(RootEntries)*32) div SectLen);

      BytesPerClu := SectPerClust*SectLen;
      TotalClu := longInt(NumSectors-FirstClust+1) div SectPerClust;
      if FATSize > $8000 then
        exit;

      fSeek(Reserved*SectLen+DSKoffset);
      if F^.Status <> stOK then
        exit;

      GetMem(FAT, FATSize);
      fRead(FAT^, FATSize);
      if F^.Status = stOK then
        begin{piwamoto}
          ProcessDir(DestDir+'%s', SectPerFAT*FatCopies+Reserved,
            False);
          SearchVolume := False;
          ProcessDir(DestDir+'%s', SectPerFAT*FatCopies+Reserved,
            False);
        end;
      FreeMem(FAT, FATSize);

  end { DoIt };

  begin { UnpackImage }
    PInfo^.Write(1, GetName(FName));
    Error := False;
    Cancel := False;
    NewTimer(tmr, 0);
    F := New(PBufStream, Init(FName, stOpenRead, $8000));
    DoIt;
    Error := Error or (F^.Status <> stOK);
    Dispose(F, Done);
    F := nil;
    if Error then
      MessageBox(GetString(dlImageError)+' '+Cut(FName, 40), nil,
        mfError+mfOKButton);
    UnpackImage := not (Error or Cancel);
  end { UnpackImage };
{-DataCompBoy-}

procedure RereadGlobal(const OutputDir: String);
  begin
    GlobalMessage(evCommand, cmPanelReread, @OutputDir);
    GlobalMessage(evCommand, cmRereadTree, @OutputDir);
  end;

procedure DoIt(AOwner: Pointer; AFiles: PFilesCollection; var
    ADestPath: String);
  var
    i: integer;
    PF: PFileRec;
    PInfo: PWhileView;
    R: TRect;
  begin
    if ADestPath = '' then
      lGetDir(0, ADestPath);
    ADestPath := MakeNormName(ADestPath, '');

    R.Assign(1, 1, 26, 10);
    New(PInfo, Init(R));
    PInfo^.Top := GetString(dlImage);
    Desktop^.Insert(PInfo);

    CreateDirInheritance(ADestPath, False);
    for i := 0 to AFiles^.Count-1 do
      begin
        PF := AFiles^.At(i);
        if not UnpackImage(MakeNormName(CnvString(PF^.Owner), PF^.
            FlName[True]), ADestPath, PInfo)
        then
          break;
        if AOwner <> nil then
          Message(AOwner, evCommand, cmCopyUnselect, PF);
      end;

    Dispose(PInfo, Done);
    PInfo := nil;
    RereadGlobal(ADestPath);
  end { DoIt };

procedure UnpackDiskImages;
  var
    DestPath: String;
  begin
    if (Files = nil) or (Files^.Count = 0) then
      exit;
    DestPath := '';
    Message(Application, evCommand, cmPushFullName, @DestPath);
    if InputBox(GetString(dlUnpackImages), GetString(dlTargetDir),
        DestPath, 255, 0) {DataCompBoy}
      = cmOK
    then
      DoIt(AOwner, Files, DestPath);
    Files^.DeleteAll;
    Dispose(Files, Done);
    Files := nil;
  end;

end.
