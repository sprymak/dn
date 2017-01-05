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

unit FStorage;

interface

uses
  Lfn, Files,
  Objects, advance, advance1, advance2, advance3, Dos, FilesCol;

type
  TStoredID = (siUndefined, siEmpty, siDirectory, siFile, siFileDir,
    siEnd);

  TStored = record
    Id: TStoredID;
    len: word; {DataCompBoy} { 8((( }
    end;

  {-DataCompBoy-}
  TStoredFile = record
    t: TStored;
    Size: TSize;
    CSize: TSize;
    Date: longInt;
    Attr: byte;
    Name: String;
    end;
  {JO: имя здесь должно обязательно быть в конце, т.к. мы сохраняем не всю }
  {    длину String, а только реальную длину имени                         }
  {-DataCompBoy-}

  TSwapLevel = (slNone,
  {$IFDEF OS_DOS}
  slXMS, slEMS, slDsk, slCnv,
    {DataCompBoy: For DPMI no EMS/XMS needed}
  {$ELSE}
  slCnv,
  {$ENDIF}
  slFail);

  PDirStorage = ^TDirStorage;
  TDirStorage = object(TObject)
    SwapLevel: TSwapLevel;
    Dirs: longInt;
    Files: longInt;
    Items: longInt;
    Stream: PStream;
    FilePtr: longInt;
    CurFile: TStoredFile;
    Last: boolean;
    CurDir, LastDir: String;
    CurPos: longInt;
    TotalLength, TotalCLength: TSize;
    vSavePos, vSize: longInt;
    Constructor Init;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    destructor Done; virtual;
    procedure AddFile(FName: String; Size, CSize: TSize; Date:
      longInt; Attr: byte);
    procedure ResetPointer(const Dir: String);
    function GetNextFile: boolean;
    procedure UpdateRecord;
    procedure DosReread;
    procedure DeleteFile(P: PFileRec);
    procedure InitStream(X, E, M: longInt);
    procedure Truncate(n: longInt);
    procedure WipeCur;
    procedure vWrite(var Buf; Count: word);
    procedure vRead(var Buf; Count: word);
    procedure vSeek(Pos: longInt);
    procedure vTruncate;
    procedure FixError;
    procedure TryStream(Sz: longInt);
    end;

const
  TStoredFixLength = SizeOf(TStoredFile)-SizeOf(String);

implementation
uses
  DNApp, Startup;

procedure __Error;
  begin
    RunError(224);
  end;

procedure TDirStorage.TryStream;
  begin
    repeat
      SwapLevel := succ(SwapLevel);
      if SwapLevel = slFail then
        __Error;
      if Sz = -1 then
        InitStream(4096, 16384, 2048)
      else
        InitStream(Sz, Sz, Sz);
    until Stream <> nil;
  end;

Constructor TDirStorage.Init;
  begin
    inherited Init;
    SwapLevel := slNone;

    TryStream(-1);

    TotalLength := 0;
    TotalCLength := 0;
    vSize := 0;
    vSavePos := 0;
  end;

procedure TDirStorage.FixError;
  label
    LLL;
  const
    BufSize = 512;
  var
    OldStream: PStream;
    SavePos, Count: longInt;
    Buffer: array[0..BufSize-1] of byte;
    n: word;
  begin
    OldStream := Stream;
    OldStream^.Status := stOK;

LLL:

    TryStream(-1);

    Count := vSize;
    OldStream^.Seek(0);
    while Count > 0 do
      begin
        if Count > BufSize then
          n := BufSize
        else
          n := Count;
        OldStream^.Read(Buffer, n);
        if OldStream^.Status <> stOK then
          __Error;
        Stream^.Write(Buffer, n);
        if Stream^.Status <> stOK then
          begin
            Dispose(Stream, Done);
            Stream := nil;
            goto LLL
          end;
        Dec(Count, n);
      end;
    Dispose(OldStream, Done);
    Stream^.Seek(vSavePos);
  end { TDirStorage.FixError };

procedure TDirStorage.InitStream;
  begin
    Stream := nil;
    Last := True;
    CurPos := -1;
    case SwapLevel of
      slCnv:
        begin
          Stream := New(PMemoryStream, Init(M, 2048));
          if Stream^.Status <> stOK then
            begin
              Dispose(Stream, Done);
              Stream := nil
            end;
        end;
      {$IFDEF OS_DOS}
        {JO: невозможно представить ситуацию под OS/2 или Win32, когда бы это понадобилось}
      slDsk:
        begin
            {    да и де факто это в VP-версиях не работало, что впрочем ни разу не привело к ошибке}
          Stream := New(PTempFile, Init('SWD', 4096));
          if Stream^.Status <> stOK then
            begin
              Dispose(Stream, Done);
              Stream := nil
            end;
        end;
      {$ENDIF}
    end {case};
  end { TDirStorage.InitStream };

destructor TDirStorage.Done;
  begin
    if Stream <> nil then
      Dispose(Stream, Done);
    Stream := nil;
  end;

{-DataCompBoy-}
procedure TDirStorage.AddFile;
  var
    dr: String;
    Nm: String;
    XT: String;
    SF: TStoredFile;
    TS: TStored absolute SF;
    B: array[0..542] of byte;
    l: word;
  begin
    if Stream = nil then
      exit;
    Stream^.Status := stOK;
    vSeek(Max(0, (vSize)-TStoredFixLength));
    l := 0;
    if (FName <> #0) and (FName <> '') then
      begin
        lFSplit(FName, dr, Nm, XT);
        if dr[Length(dr)] <> '\' then
          AddStr(dr, '\');
        if (UpStrg(CurDir) <> UpStrg(dr)) then
          begin
            CurDir := dr;
            SF.t.Id := siDirectory;
            l := TStoredFixLength+1+Length(dr);
            SF.Name := dr;
            SF.t.len := l;
            Move(SF, B, l);
            Inc(Items);
          end;
        if Attr and Directory <> 0
        then
          SF.t.Id := siFileDir
        else
          SF.t.Id := siFile;
        SF.Name := GetName(FName);
        SF.t.len := TStoredFixLength+1+Length(SF.Name);
        SF.Size := Size;
        TotalLength := TotalLength+Size;
        TotalCLength := TotalCLength+CSize;
        SF.CSize := CSize;
        SF.Date := Date;
        SF.Attr := Attr;
        Move(SF, B[l], SF.t.len);
        Inc(l, SF.t.len);
        Inc(Files);
        Inc(Items);
        if Attr and Directory <> 0 then
          Inc(Dirs);
      end;

    TS.Id := siEnd;
    TS.len := TStoredFixLength;
    Move(TS, B[l], TS.len);
    Inc(l, TS.len);

    vWrite(B, l);
    Last := True;
  end { TDirStorage.AddFile };
{-DataCompBoy-}

procedure TDirStorage.ResetPointer;
  begin
    if Stream = nil then
      exit;
    CurPos := 0;
    FilePtr := -1;
    CurPos := -1;
    Last := vSize = 0;
    vSeek(0);
    FillChar(CurFile, SizeOf(CurFile), 0);
    CurDir := Dir;
    LastDir := '';
    if (CurDir <> '') and (CurDir[Length(CurDir)] <> '\') then
      AddStr(CurDir, '\');
  end;

{-DataCompBoy-}
function TDirStorage.GetNextFile;
  var
    SF: TStoredFile;
    dr: String;
  begin
    dr := LastDir;
    GetNextFile := False;
    FillChar(CurFile, SizeOf(CurFile), 0);
    Last := True;
    CurPos := -1;
    if Stream = nil then
      exit;
    while Stream^.Status = stOK do
      begin
        FilePtr := vSavePos;
        vRead(SF, TStoredFixLength);
        if SF.t.len > TStoredFixLength then
          vRead(SF.Name, SF.t.len-TStoredFixLength);
        case SF.t.Id of
          siDirectory:
            begin
              dr := SF.Name;
              LastDir := dr
            end;
          siEnd:
            break;
          siFile, siFileDir:
            begin
              CurPos := vSavePos-SF.t.len;
              if (CurDir = '') or (UpStrg(dr) = UpStrg(CurDir)) then
                begin
                  CurFile := SF;
                  Last := vSize-vSavePos <= TStoredFixLength;
                  LastDir := dr;
                  GetNextFile := True;
                  if CurFile.Name <> '' then
                    break;
                end;
            end;
        end {case};
      end;
  end { TDirStorage.GetNextFile };
{-DataCompBoy-}

procedure TDirStorage.UpdateRecord;
  var
    s: TStored;
  begin
    if Stream = nil then
      exit;
    if (CurPos > 0) and (CurFile.Name <> '') then
      begin
        Stream^.Status := stOK;
        vSeek(CurPos);
        vRead(s, SizeOf(s));
        vSeek(CurPos);
        vWrite(CurFile, s.len);
      end;
  end;

Constructor TDirStorage.Load;
  var
    P, l: longInt;
  begin
    TObject.Init;
    s.Read(Dirs, SizeOf(Dirs));
    s.Read(Files, SizeOf(Files));
    s.Read(TotalLength, SizeOf(TotalLength));
    s.Read(TotalCLength, SizeOf(TotalCLength));
    s.Read(l, SizeOf(l));
    vSize := l;
    TryStream(l);
    P := s.GetPos;
    repeat
      Stream^.CopyFrom(s, l);
      if Stream^.Status = stOK then
        break
      else
        begin
          Dispose(Stream, Done);
          Stream := nil;
          TryStream(l);
          s.Seek(P);
        end;
    until False;
  end { TDirStorage.Load };

procedure TDirStorage.Store;
  var
    l: longInt;
  begin
    AddFile(#0, 0, 0, 0, 0);
    s.Write(Dirs, SizeOf(Dirs));
    s.Write(Files, SizeOf(Files));
    s.Write(TotalLength, SizeOf(TotalLength));
    s.Write(TotalCLength, SizeOf(TotalCLength));
    l := vSize;
    s.Write(l, SizeOf(l));
    vSeek(0);
    Stream^.Status := stOK;
    s.CopyFrom(Stream^, vSize);
  end;

{-DataCompBoy-}
procedure TDirStorage.DosReread;
  var
    SF: TStoredFile;
    ST: TStored absolute SF;
    dr: String;
    l, B: longInt;
    SR: lSearchRec;
  begin
    Last := False;
    dr := '';
    Stream^.Status := stOK;
    vSeek(0);
    while Stream^.Status = stOK do
      begin
        l := vSavePos;
        vRead(SF, TStoredFixLength);
        if ST.len > TStoredFixLength then
          vRead(SF.Name, ST.len-TStoredFixLength);
        case ST.Id of
          siDirectory:
            dr := SF.Name;
          siEnd:
            break;
          siFile, siFileDir:
            begin
              Abort := False;
              DOSError := 0;
              lFindFirst(dr+SF.Name, AnyFile, SR); {JO}
              lFindClose(SR);
              TotalLength := TotalLength-SF.Size;
              if (DOSError <> 0) or Abort then
                begin
                  Dec(Files);
                  if ST.Id = siFileDir then
                    Dec(Dirs);
                  ST.Id := siEmpty;
                  vSeek(l);
                  vWrite(SF, ST.len);
                end
              else if (SR.FullSize <> SF.Size) or (SR.SR.Time <> SF.
                  Date) or (SR.SR.Attr <> SF.Attr)
              then
                begin
                  TotalLength := TotalCLength-SR.FullSize;
                  SF.Size := SR.FullSize;
                  SF.CSize := SR.FullSize;
                  SF.Date := SR.SR.Time;
                  SF.Attr := SR.SR.Attr;
                  vSeek(l);
                  vWrite(SF, ST.len);
                end;
            end;
        end {case};
      end;
  end { TDirStorage.DosReread };
{-DataCompBoy-}

{-DataCompBoy-}
procedure TDirStorage.DeleteFile;
  var
    dr: String;
    Nm: String;
  begin
    dr := P^.Owner^;
    if dr[Length(dr)] <> '\' then
      AddStr(dr, '\');
    ResetPointer('');
    Nm := UpStrg(P^.FlName[True]);
    while not Last and GetNextFile do
      if (UpStrg(CurFile.Name) = Nm) and (LastDir = dr) then
        WipeCur;
  end;
{-DataCompBoy-}

procedure TDirStorage.Truncate(n: longInt);
  var
    ST: TStored;
    l, NF, ND, NI: longInt;
  begin
    ResetPointer('');
    NF := 0;
    ND := 0;
    NI := 0;
    while Stream^.Status = stOK do
      begin
        Inc(NI);
        l := vSavePos;
        vRead(ST, TStoredFixLength);
        if ST.len > TStoredFixLength then
          vSeek(l+ST.len);
        case ST.Id of
          siEnd:
            break;
          siFile, siFileDir:
            begin
              Dec(n);
              if n > 0 then
                begin
                  Inc(NF);
                  if ST.Id = siFileDir then
                    Inc(ND);
                end
              else
                begin
                  ST.Id := siEnd;
                  ST.len := TStoredFixLength;
                  vSeek(l);
                  vWrite(ST, ST.len);
                  vTruncate;
                  Dirs := ND;
                  Files := NF;
                  Items := NI;
                  break;
                end
            end;
        end {case};
      end;
  end { TDirStorage.Truncate };

procedure TDirStorage.WipeCur;
  var
    l: longInt;
    s: TStored;
  begin
    l := vSavePos;
    Stream^.Status := stOK;
    vSeek(FilePtr);
    vRead(s, SizeOf(s)); {DataCompBoy}
    vSeek(FilePtr); {DataCompBoy}
    CurFile.t.Id := siEmpty;
    vWrite(CurFile, s.len); {DataCompBoy}
    vSeek(l);
    Dec(Files);
    if s.Id = siFileDir then
      Dec(Dirs);
  end;

procedure TDirStorage.vWrite;
  begin
    repeat
      Stream^.Write(Buf, Count);
      if Stream^.Status = stOK
      then
        begin
          vSize := Max(vSize, vSavePos+Count);
          Inc(vSavePos, Count);
          break;
        end
      else
        FixError;
    until False;
  end;

procedure TDirStorage.vSeek;
  begin
    Stream^.Seek(Pos);
    vSavePos := Pos;
  end;

procedure TDirStorage.vTruncate;
  begin
    Stream^.Truncate;
    if Stream^.Status = stOK
    then
      begin
        vSize := Stream^.GetPos;
        vSavePos := vSize
      end
    else
      FixError;
  end;

procedure TDirStorage.vRead;
  begin
    Stream^.Read(Buf, Count);
    if Stream^.Status = stOK
    then
      Inc(vSavePos, Count)
    else
      __Error;
  end;

end.
