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

unit DiskInfo;

interface

uses
  Lfn, Objects, Drivers, Views, Dialogs, Dos, HideView, RStrings,
  advance, advance1, advance2, advance3, Collect
  , VPUtils; {JO}

type
  PDiskInfoRec = ^TDiskInfoRec;
  TDiskInfoRec = record
    Title: PString;
    Dir: PString;
    Files: PString;
    Free: PString;
    Total: PString;
    VolumeID: PString;
    SerialNo: PString; { Rainbow }
    FileSys: PString; { Rainbow }
    DirInfo: PCollection;
    Limit: TPoint;
    InfoFile: byte;
    end;

  PDiskInfo = ^TDiskInfo;
  TDiskInfo = object(THideView)
    Dir: String;
    FreeSpace, TotalSpace, DirLen: TSize;
    FreeMemory, TotalMemory: TSize;
    VolumeLabel: String[12];
    SerialNo: String[9]; { Rainbow }
    NumFiles: integer;
    Info: TDiskInfoRec;
    Delta: TPoint;
    Constructor Init(R: TRect);
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream); virtual;
    procedure ReadData(Loc: boolean);
    procedure ShowView; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    procedure Draw; virtual;
    destructor Done; virtual;
    end;

procedure DispInfo(var Info: TDiskInfoRec); {DataCompBoy}

function CountDirLen(const Dir: String; Recurse: boolean;
    {DataCompBoy, AK155}
  var ClusterLen: TSize; var NumFiles, NumDirs: integer): TSize;
procedure GetDrInfo(Drv: byte);
procedure ReadDiskInfo(const dr: String; var B: TDiskInfoRec;
    ShowFlags: integer);
{JO}
function Disk_Free(Drive: byte): TSize;

const
  CDiskInfo = #23#24;
  ifDirInfo = 0;
  ifFileID = 1;
  ifDescription = 2;
  ifReadMe = 3;
  ifReadTxt = 4;
  ifReadMeTxt = 5;

  sDirinfo = 'DirInfo';
  sFileID = 'File_ID.DIZ';
  sReadMe = 'Read.Me';
  sReadTxt = 'Read.Txt';
  sReadMeTxt = 'ReadMe.Txt';

type
  PTeamView = ^TTeamView;
  TTeamView = object(TView)
    LastTick: longInt;
    Strings: array[1..20] of integer;
    Constructor Init(var R: TRect);
    procedure Draw; virtual;
    procedure UpDate; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    end;

var
  SerFileSys: boolean; { Rainbow }
const
  TeamView: PTeamView = nil;
function GetRndBytes(B, Rnd: longInt): longInt;

var
  FreeSpc, TotalSpc: TSize;
  {-DataCompBoy-}
  DriveData: record
    {00}RecSize: word;
    {02}StructVersion: word;
    {04}SectorsPerCluster: longInt;
    {08}BytesPerSector: longInt;
    {0C}AvailClusters: longInt;
    {10}TotalClusters: longInt;
    {14}NumPhisSectorsAvail: longInt;
    {18}NumPhisSectorsTotal: longInt;
    {1C}AvailAllocUnits: longInt;
    {20}TotalAllocUnits: longInt;
    {24}Reserved: array[0..7] of byte;
    end;
  BytesPerCluster: longInt;
  {-DataCompBoy-}

implementation
uses
  Startup, DNApp, Commands, DNHelp, Tree, xTime
  , VpSysLow
  , FlTl
  ;

const
  MaxTeam = 158;

function ESC_Pressed: boolean;
  var
    E: TEvent;
  begin
    Application^.Idle;
    GetKeyEvent(E);
    ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbESC)
  end;

{ Rainbow BEGIN }
procedure GetSerFileSys(var SerialNo, FileSys: String);
  {JO}
  {$IFDEF OS2}
  var
    Ser1: longInt;
    DWord: array[1..2] of AWord absolute Ser1;
    Vol1: String;
    rc: longInt;

  function Word2Hex(i: AWord): String;
    const
      Values: array[0..15] of Char = '0123456789ABCDEF';
    var
      s, s1: String;
      A: byte;
    begin
      s := '';
      s1 := '';
      repeat
        s := s+Values[i mod 16];
        i := i div 16;
      until i = 0;
      for A := 3 downto Length(s) do
        s1 := s1+'0';
      for A := Length(s) downto 1 do
        s1 := s1+s[A];
      Word2Hex := s1;
    end;
  {$ENDIF}
  begin { GetSerFileSys }
    {$IFDEF OS2}
    rc := GetVolSer((GetDrive+1), Ser1, Vol1);
    if (rc = 0) then
      SerialNo := Word2Hex(DWord[2])+'-'+Word2Hex(DWord[1])
    else
      SerialNo := '';
    FileSys := GetDriveTypeString(GetCurDrive);
    SerFileSys := rc = 0;
    {$ELSE} {$IFDEF Win32}
    FileSys := GetFSString(GetCurDrive);
    SerFileSys := FileSys <> '';
    {$ELSE} {DPMI32}
    FileSys := '';
    SerFileSys := False;
    {$ENDIF}
    {$ENDIF}
  end { GetSerFileSys };
{/JO}

function GetRndBytes(B, Rnd: longInt): longInt;
  begin
    GetRndBytes := (B div Rnd)*Rnd+Rnd*longInt(B mod Rnd <> 0);
  end;

Constructor TTeamView.Init;
  begin
    inherited Init(R);
    UpdTicks := 9;
    Options := Options or ofPreProcess;
    RegisterToBackground(@Self);
  end;

procedure TTeamView.Draw;
  var
    B: TDrawBuffer;
    s: String;
    i: integer;
    C: word;
  begin
    C := Owner^.GetColor($0807);
    MoveChar(B, ' ', C, Size.X);
    s := GetString(dlTeamAll);
    MoveCStr(B[(Size.X-CStrLen(s)) div 2], s, C);
    WriteLine(0, 0, Size.X, 1, B);
    for i := 1 to Size.Y-1 do
      begin
        MoveChar(B, ' ', C, Size.X);
        if (Strings[i] > 0) and (Strings[i] < MaxTeam) then
          begin
            s := GetString(TStrIdx(integer(dlTeamAll)+Strings[i]));
            MoveCStr(B[(Size.X-CStrLen(s)) div 2], s, C);
          end;
        WriteLine(0, i, Size.X, 1, B);
      end;
  end { TTeamView.Draw };

procedure TTeamView.UpDate;
  var
    l, i: longInt;
  begin
    Move(Strings[2], Strings[1], (Size.Y-2)*SizeOf(integer));
    if (Strings[Size.Y-1] > 0) and (Strings[Size.Y-1] < MaxTeam)
    then
      Inc(Strings[Size.Y-1]);
    if Strings[Size.Y-1] = MaxTeam then
      Strings[Size.Y-1] := 0;
    if (Strings[Size.Y-1] = 0) and (Strings[1] = 0) then
      Strings[Size.Y-1] := 1;
    DrawView;
  end;

procedure TTeamView.HandleEvent(var Event: TEvent);
  begin
    if (Event.What = evKeyDown) and (Event.KeyCode = kbDown) then
        begin
        ClearEvent(Event);
        UpDate;
        NewTimer(UpTmr, 36)
      end
    else if (Event.What = evKeyDown) and (Event.KeyCode = kbUp) then
        begin
        ClearEvent(Event);
        Move(Strings[1], Strings[2], (Size.Y-2)*SizeOf(integer));
        if Strings[1] > 0 then
          Dec(Strings[1]);
        if (Strings[1] = 0) and (Strings[Size.Y-1] = 0) then
          Strings[1] := MaxTeam;
        DrawView;
        NewTimer(UpTmr, 36)
      end
  end;

destructor TTeamView.Done;
  begin
    TeamView := nil;
    inherited Done;
  end;

procedure DispInfo;
  begin
    with Info do
      begin
        if Title <> nil then
          DisposeStr(Title);
        if Dir <> nil then
          DisposeStr(Dir);
        if Files <> nil then
          DisposeStr(Files);
        if Free <> nil then
          DisposeStr(Free);
        if Total <> nil then
          DisposeStr(Total);
        if VolumeID <> nil then
          DisposeStr(VolumeID);
        if SerialNo <> nil then
          DisposeStr(SerialNo); { Rainbow }
        if FileSys <> nil then
          DisposeStr(FileSys); { Rainbow }
        if DirInfo <> nil then
          Dispose(DirInfo, Done);
      end;
    FillChar(Info, SizeOf(Info), 0);
  end { DispInfo };

Constructor TDiskInfo.Load;
  begin
    TObject.Init;
    inherited Load(s);
    s.Read(Delta, SizeOf(Delta));
  end;

procedure TDiskInfo.Store;
  begin
    inherited Store(s);
    s.Write(Delta, SizeOf(Delta));
  end;

Constructor TDiskInfo.Init;
  begin
    TView.Init(R);
    HelpCtx := hcDiskInfo;
    EventMask := evCommand or evKeyDown;
    Options := Options or ofSelectable or ofTopSelect;
    Delta.Assign(0, 0);
    FillChar(Info, SizeOf(Info), 0);
  end;

destructor TDiskInfo.Done;
  begin
    DispInfo(Info);
    inherited Done;
  end;

procedure TDiskInfo.ShowView;
  begin
    ReadData(False);
    inherited ShowView;
  end;

function TDiskInfo.GetPalette;
  const
    s: String[Length(CDiskInfo)] = CDiskInfo;
  begin
    GetPalette := @S;
  end;

procedure TDiskInfo.ReadData;
  var
    MemBlocks, i: word;
  begin
    Abort := False;
    {$IFNDEF OS_DOS}
    TotalMemory := MemAvail;
    {$ELSE}
    asm
  int $12
  mov word ptr memBlocks,ax
 end;
    TotalMemory := longInt(longInt(MemBlocks) shl 10);
    {$ENDIF}
    DispInfo(Info);
    Message(Owner, evCommand, cmGetDirInfo, @Info);
    Delta.Assign(0, 0);
  end;

procedure TDiskInfo.HandleEvent;
  var
    s: String; {DataCompBoy}

    {-DataCompBoy-}
  procedure ReadInfoBBS;
    var
      F: PTextReader;
      i: integer;
      Nm: String;
      DZ: String;
    begin
      if (Info.DirInfo = nil) then
        Info.DirInfo := New(PStringCollection, Init(20, 10, False))
      else
        Info.DirInfo^.FreeAll;
      Info.InfoFile := ifDescription;
      i := -1;
      F := nil;
      DZ := FMSetup.DIZ;
      while (i <> 0) and (F = nil) do
        begin
          i := Pos(';', DZ);
          if i = 0 then
            FreeStr := DZ
          else
            begin
              FreeStr := Copy(DZ, 1, i-1);
              Delete(DZ, 1, i);
            end;
          F := New(PTextReader, Init(GetPath(s)+FreeStr));
        end;
      if F = nil then
        exit;
      {$IFNDEF OS2}
      Nm := GetName(lfGetShortFileName(s));
      {$ELSE}
      Nm := GetName(s);
      {$ENDIF}
      while (IOResult = 0) and not F^.Eof do
        begin
          FreeStr := F^.GetStr;
          if (FreeStr[1] <> ' ') then
            begin
              i := PosChar(' ', FreeStr);
              if i = 0 then
                continue;
              if UpStrg(Copy(FreeStr, 1, i-1)) = UpStrg(Nm) then
                begin
                  Delete(FreeStr, 1, i);
                  repeat
                    DelLeft(FreeStr);
                    Info.DirInfo^.AtInsert(Info.DirInfo^.Count,
                      NewStr(FreeStr));
                    FreeStr := F^.GetStr;
                  until F^.Eof or (Info.DirInfo^.Count >= 100) or (
                    FreeStr = '') or (FreeStr[1] <> ' ');
                  break;
                end;
            end;
        end;
      ClrIO;
      Dispose(F, Done);
      F := nil;
    end { ReadInfoBBS };
  {-DataCompBoy-}

  begin { TDiskInfo.HandleEvent }
    inherited HandleEvent(Event);
    if (Event.What = evKeyDown) then
      case Event.KeyCode of
        kbLeft:
          begin
            if Delta.X > 0 then
              begin
                Dec(Delta.X);
                SetState(sfExposed, True);
                DrawView
              end;
            ClearEvent(Event);
          end;
        kbRight:
          begin
            if Delta.X+Size.X-2 < Info.Limit.X then
              begin
                Inc(Delta.X);
                SetState(sfExposed, True);
                DrawView
              end;
            ClearEvent(Event);
          end;
        kbDown:
          begin
            if Delta.Y < Info.Limit.Y then
              begin
                Inc(Delta.Y);
                SetState(sfExposed, True);
                DrawView
              end;
            ClearEvent(Event);
          end;
        kbUp:
          begin
            if Delta.Y > 0 then
              begin
                Dec(Delta.Y);
                SetState(sfExposed, True);
                DrawView
              end;
            ClearEvent(Event);
          end;
      end
    else if (Event.What = evCommand) then
      case Event.Command of
        cmEditFile:
          begin
            ClearEvent(Event);
            case Info.InfoFile of
              ifFileID:
                s := sFileID;
              ifDirInfo:
                s := sDirinfo;
              ifReadMe:
                s := sReadMe;
              ifReadTxt:
                s := sReadTxt;
              ifReadMeTxt:
                s := sReadMeTxt;
              else
                exit;
            end {case};
            s := MakeNormName(Dir, s);
            Message(Application, evCommand, cmFileEdit, @S);
          end;
        cmInfoPresent:
          begin
            ClearEvent(Event);
            Event.InfoPtr := Owner
          end;
        cmRereadInfo:
          begin
            ReadData(False);
            DrawView;
            ClearEvent(Event);
          end;
        cmLocalReread:
          begin
            ReadData(True);
            DrawView;
            ClearEvent(Event);
          end;
        cmLoadViewFile:
          begin
            s := PString(Event.InfoPtr)^; {DataCompBoy}
            ReadInfoBBS;
            DrawView;
            ClearEvent(Event);
          end;
      end {case};
  end { TDiskInfo.HandleEvent };

procedure TDiskInfo.Draw;
  var
    B: TDrawBuffer;
    C, Y, X: word;
    s1, s2: String[20];
    CC: boolean;

  procedure Wrt(s: String);
    var
      i: integer;
    begin
      MoveChar(B, ' ', C, Size.X);
      i := (Size.X-CStrLen(s)) div 2;
      if (i < 0) or CC then
        i := 0;
      MoveCStr(B[i], s, C);
      WriteLine(0, Y, Size.X, 1, B);
      Inc(Y);
    end;

  procedure Wrt_(s: String); {временно!!!}
    var
      i: integer;
    begin
      MoveChar(B, ' ', C, Size.X);
      i := (Size.X-CStrLen(s)) div 2;
      if (i < 0) or CC then
        i := 0;
      Replace('~', #0'~', s);
      s := '~'#0+s;
      {MoveStr(B[I], S, Lo(C));}
      MoveCStr(B[i], s, C);
      WriteLine(0, Y, Size.X, 1, B);
      Inc(Y);
    end;

  procedure XWrt(P: PString; Flag: word);
    begin
      if (P <> nil) and (DriveInfoData and Flag <> 0) then
        Wrt(P^);
    end;

  begin { TDiskInfo.Draw }
    C := (GetColor(1) shl 8) or (GetColor(2) and 255);
    Y := 0;
    CC := False;
    if ((Info.Title <> Info.Dir) or (Info.Title <> Info.Dir)) and
      (DriveInfoData and (fdiTitle+fdiTotals) <> 0)
    then
      begin
        XWrt(Info.Title, fdiTitle);
        {временно!!!}
        if (Info.Dir <> nil) and (DriveInfoData and fdiTitle <> 0)
        then
          Wrt_(Cut(Info.Dir^, Size.X));
        XWrt(Info.Files, fdiTotals);
        Wrt('');
      end;
    if ((Info.Total <> Info.Free) or (Info.Total <> Info.VolumeID))
        and
      (DriveInfoData and (fdiVolumeSize+fdiVolumeFree+fdiVolumeLabel) <>
        0)
    then
      begin
        XWrt(Info.Total, fdiVolumeSize);
        XWrt(Info.Free, fdiVolumeFree);
        XWrt(Info.VolumeID, fdiVolumeLabel);
      end;
    { Rainbow BEGIN }
    if SerFileSys then
      begin
        {$IFNDEF WIN32} {временно!!!}
        XWrt(Info.SerialNo, fdiSerialNo);
        {$ENDIF}
        XWrt(Info.FileSys, fdiFileSys);
      end;
    Wrt('');
    { Rainbow END }
    if DriveInfoData and fdiMemAvail <> 0 then
      Wrt('~'+FStr(MemAvail/1024)+GetString(dlDIMemoryForDN));
    if DriveInfoData and fdiLowMemory <> 0 then
      Wrt('~'+FStr( {TotalMemory}PhysMemAvail/1024)+GetString(
        dlDIMemoryTotal));
    {$IFDEF OS_DOS}
    if (EMSFound or XMSFound) and (Y > 0)
      and (DriveInfoData and (fdiEMSFree+fdiXMSFree) <> 0)
    then
      Wrt(
        #196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196
        );
    if (DriveInfoData and fdiEMSFree <> 0) and EMSFound then
      begin
        Wrt('EMS: ~'+ItoS(EMSSize)+GetString(dlDIEMS1)+ItoS(
          EMSFreePages*16)+GetString(dlDIEMS2));
      end;
    if (DriveInfoData and fdiXMSFree <> 0) and XMSFound then
      Wrt('~'+ItoS(XMSFree)+GetString(dlDIXMSFree));
    {$ENDIF}
    if (Y > 0) and ((DriveInfoData {$IFNDEF OS_DOS} shl 2 {$ENDIF})
        and fdiDIZ <> 0) and
      (Info.DirInfo <> nil) and (Info.DirInfo^.Count > 0)
    then
      begin
        if Owner <> nil then
          C := Owner^.GetColor(1+byte(Owner^.GetState(sfActive)));
        FillChar(FreeStr[1], Size.X, 196);
        SetLength(FreeStr, Size.X);
        case Info.InfoFile of
          ifDirInfo:
            s1 := ' '+sDirinfo+' ';
          ifFileID:
            s1 := ' '+sFileID+' ';
          ifReadMe:
            s1 := ' '+sReadMe+' ';
          ifReadTxt:
            s1 := ' '+sReadTxt+' ';
          ifReadMeTxt:
            s1 := ' '+sReadMeTxt+' ';
          else
            s1 := '';
        end {case};
        X := (Size.X-Length(s1)) div 2;
        if X < 0 then
          X := 0;
        Move(s1[1], FreeStr[X+1], Length(s1));
        Wrt(FreeStr);
        C := (GetColor(1) shl 8) or (GetColor(2) and 255);
        CC := True;
        for X := Delta.Y to Info.DirInfo^.Count-1 do
          begin
            Wrt(#0+Copy(CnvString(Info.DirInfo^.At(X)), Delta.X,
              MaxStringLength)+#0);
            if Y > Size.Y then
              break;
          end;
      end;
    MoveChar(B, ' ', C, Size.X);
    if Y <= Size.Y-1 then
      WriteLine(0, Y, Size.X, Size.Y-Y+1, B);
  end { TDiskInfo.Draw };

{-DataCompBoy-}
{AK155: NumFiles включает в себя NumDirs, то есть число
чисто файлов равно NumFiles-NumDirs}
function CountDirLen;
  var
    tmr: TEventTimer;
    l: TSize;
    SR: lSearchRec;
    DC: PDirCol;
  begin
    NewTimer(tmr, 1);
    ClusterLen := 0;
    l := 0;
    NumFiles := 0;
    NumDirs := 0;

    New(DC, Init($10, $10, False));
    DC^.Insert(NewStr(Dir));
    Abort := False;
    while DC^.Count > 0 do
      begin
        FreeStr := PString(DC^.At(0))^;
        DC^.AtFree(0);
        if Abort then
          break;

        if TimerExpired(tmr) then
          begin
            NewTimer(tmr, 3);
            if ESC_Pressed then
              begin
                Abort := True;
                l := 0;
                break
              end;
          end;

        if word(Length(FreeStr)) > MaxPathLen-1 then
          begin
            Abort := True;
            break
          end;

        DOSError := 0;
        Abort := False;

        lFindFirst(MakeNormName(FreeStr, x_x), AnyFile, SR); {JO}
        while (DOSError = 0) and not Abort do
          begin
            if not IsDummyDir( {$IFDEF OS_DOS}SR.SR.Name {$ELSE}SR.
                FullName {$ENDIF})
            then
              begin
                Inc(NumFiles);
                if SR.SR.Attr and Directory <> 0 then
                  begin
                    Inc(NumDirs); {AK155}
                    if Recurse then
                      begin
                        if (word(Length(FreeStr))+word(Length(SR.
                            FullName)) > MaxPathLen-1)
                          or (MaxAvail < Length(FreeStr+SR.FullName)+
                            $41)
                        then
                          begin
                            Abort := True;
                            l := 0;
                            break
                          end;
                        DC^.Insert(NewStr(MakeNormName(FreeStr, SR.
                          FullName)));
                      end;
                  end
                else
                  begin
                    l := l+SR.FullSize;
                    if BytesPerCluster > 0 then
                      begin
                        if Round(SR.FullSize/BytesPerCluster)-(SR.
                            FullSize/BytesPerCluster) >= 0
                        then
                          ClusterLen := ClusterLen+Round(SR.FullSize/
                            BytesPerCluster)*BytesPerCluster
                        else
                          ClusterLen := ClusterLen+Round(SR.FullSize/
                            BytesPerCluster)*BytesPerCluster+
                            BytesPerCluster;
                      end;
                  end;
              end;
            lFindNext(SR);
          end;
        lFindClose(SR);
      end;
    DC^.FreeAll;
    Dispose(DC, Done);
    CountDirLen := l;
  end { CountDirLen };
{-DataCompBoy-}

{-DataCompBoy-}
{-DataCompBoy-}

{-DataCompBoy-}
procedure GetDrInfo(Drv: byte);
  var
    dr: Char;
  begin
    if Drv = 0 then
      dr := GetCurDrive
    else
      dr := Char(Drv+64);
    FreeSpc := SysDiskFreeLong(Drv);
    TotalSpc := SysDiskSizeLong(Drv);
    BytesPerCluster := GetBytesPerCluster(Drv);
  end;
{-DataCompBoy-}

function Disk_Free(Drive: byte): TSize;
  begin
    GetDrInfo(Drive);
    Disk_Free := FreeSpc;
  end;

{-DataCompBoy-}
procedure ReadDiskInfo;
  var
    VolumeLabel: String[12];
    SerialNo: String[9]; { Rainbow }
    FileSys: String[8]; { Rainbow }
    s: String;
    s1, s2: String[30];
    Drv: byte;
    F: PTextReader;
    DirLen: TSize;
    NumFiles, NumDirs: integer;
  begin
    ClrIO;
    if dr = '' then
      lGetDir(0, s) {GetDir(0, S)} {Cat}
    else
      s := dr;
    if Abort then
      exit;

    DispInfo(B);

    B.Title := NewStr(GetString(dlDICurDir)+':');
    B.Dir := NewStr(s);
    Drv := byte(s[1])-64;

    GetDrInfo(Drv);
    if Abort then
      exit;
    BytesPerCluster := GetBytesPerCluster(Drv);

    DirLen := 0;
    NumFiles := 0;
    CountDirLen(s, False, DirLen, NumFiles, NumDirs);

    B.Total := NewStr('~'+FStr(TotalSpc)+GetString(dlDITotalDisk)+
      Copy(s, 1, 2)+'~');
    B.Free := NewStr('~'+FStr(FreeSpc)+GetString(dlDIFreeDisk)+Copy(s, 1
      , 2)+'~');

    if NumFiles = 0 then
      B.Files := NewStr(GetString(dlDINoFiles))
    else
      begin
        if NumFiles = 1 then
          s1 := GetString(dlDIFile)
        else
          s1 := GetString(dlDIFiles);
        if DirLen = 1 then
          s2 := GetString(dlDIByte)
        else
          s2 := GetString(dlDIBytes);
        B.Files := NewStr('~'+FStr(NumFiles)+'~ '+s1+GetString(
          dlDIWith)+'~'+FStr(DirLen)+'~ '+s2);
      end;

    B.InfoFile := ifDirInfo;
    if (ShowFlags and psShowLongDesc) <> 0 then
      begin
        B.DirInfo := nil;
      end
    else
      begin
        B.DirInfo := New(PStringCollection, Init($10, $10, False));
        F := New(PTextReader, Init(MakeNormName(s, sDirinfo)));
        if F = nil then
          begin
            B.InfoFile := ifFileID;
            F := New(PTextReader, Init(MakeNormName(s, sFileID)));
          end;
        if F = nil then
          begin
            B.InfoFile := ifReadMe;
            F := New(PTextReader, Init(MakeNormName(s, sReadMe)));
          end;
        if F = nil then
          begin
            B.InfoFile := ifReadTxt;
            F := New(PTextReader, Init(MakeNormName(s, sReadTxt)));
          end;
        if F = nil then
          begin
            B.InfoFile := ifReadMeTxt;
            F := New(PTextReader, Init(MakeNormName(s, sReadMeTxt)));
          end;
        if F = nil then
          B.InfoFile := ifFileID; {JO}
        B.Limit.X := 0;
        B.Limit.Y := 0;
        if F <> nil then
          begin
            while not F^.Eof and (B.DirInfo^.Count < 100) do
              begin
                s := F^.GetStr;
                if Length(s) > B.Limit.X then
                  B.Limit.X := Length(s);
                B.DirInfo^.AtInsert(B.DirInfo^.Count, NewStr(s));
                Inc(B.Limit.Y);
              end;
            Dispose(F, Done);
            F := nil;
          end;
      end;
    VolumeLabel := {$IFDEF WIN32}CharToOemStr {$ENDIF}(
      GetVolumeLabel(GetCurDrive)); {JO, Cat}
    if VolumeLabel = '' then
      VolumeLabel := GetString(dlDINone); {JO}
    B.VolumeID := NewStr(GetString(dlDIVolumeID)+Char(Drv+64)+': '+
      VolumeLabel+'~');
    { Rainbow BEGIN }
    GetSerFileSys(SerialNo, FileSys);
    B.SerialNo := NewStr(GetString(dlDISerialNo)+Char(Drv+64)+': '+
      SerialNo+'~');
    B.FileSys := NewStr(GetString(dlDIFileSys)+Char(Drv+64)+': '+
      FileSys+'~');
    { Rainbow END }
  end { ReadDiskInfo };
{-DataCompBoy-}

end.
