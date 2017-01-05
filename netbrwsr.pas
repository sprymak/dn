
{ The Network Browser Panel unit }
{ Based on p_net plugin library by Cat (2:5030/1326.13) }

{JO: 1-07-2005 - перенёс панельку броузера сети из плагина в ядро DN/2}

{$I STDEFINE.INC}

unit NetBrwsr;

interface

uses
  Drives, Objects2, Streams, Collect, Views, FilesCol, Windows, Messages,
  Advance, Advance1, DiskInfo;

type
  PNetResourceArray = ^TNetResourceArray;
  TNetResourceArray = array[0..1023] of TNetResource;
  PNetResourceNamesArray = ^TNetResourceNamesArray;
  TNetResourceNamesArray = array[0..1023] of String;

type
  PNetDrive = ^TNetDrive;
  TNetDrive = object(TDrive)
    RootState: Boolean;
    RootDirSaved: String;

    { low level (working with network) }
    RootResources: PNetResourceArray;
    RootResourceNames: PNetResourceNamesArray;
    RootCount, RootCountAlloced: LongInt;
    ConnectedResources: PNetResourceArray;
    ConnectedCount: LongInt;
    NetResources: PNetResourceArray;
    NetCount: LongInt;
    procedure ClearNetResources;
    procedure ClearRootResources;
    procedure AddRootResource(var NetResource: TNetResource; const DirName: String);
    procedure RemoveRootResource(var DirName: String);
    procedure GetNetList(hEnum: THandle; var Res: PNetResourceArray; var EnumCount: LongInt);
    function AddConnection(var NetResource: TNetResource): Boolean;
    procedure FindNetResources;
    {function GetRootName(I: LongInt): String;}

    { high level (working with file panel) }
    constructor Init(AOwner: Pointer);
    constructor Load(var S: TStream);
    procedure Store(var S: TStream); virtual;
    procedure lChDir(ADir: String); virtual;
    function GetDir: String; virtual;
    function GetDirectory(const FileMask: String;
        var TotalInfo: TSize): PFilesCollection; virtual;
    procedure CopyFiles(Files: PCollection; Own: PView; MoveMode: Boolean)
      ; virtual;
    procedure CopyFilesInto(Files: PCollection; Own: PView;
         MoveMode: Boolean); virtual;
    procedure EraseFiles(Files: PCollection); virtual;
    {procedure UseFile(P: PFileRec; Command: Word); virtual;}
    procedure GetFreeSpace(var S: String); virtual;
    {function  Disposable: Boolean; virtual;}
    function  GetRealName: String; virtual;
    {procedure GetFull(var B; P:PFileRec; C, SC:Word); virtual;}
    {procedure MakeTop(var S: String); virtual;}
    {procedure RereadDirectory(S: String); virtual;}
    procedure GetDown(var B; C: Word; P: PFileRec;
        var LFN_inCurFileLine: Boolean); virtual;
    {procedure HandleCommand(Command: Word; InfoPtr: Pointer); virtual;}
    procedure GetDirInfo(var B: TDiskInfoRec); virtual;
    function  GetRealDir: String; virtual;
    procedure MakeDir; virtual;
    {function  isUp: Boolean; virtual;}
    {procedure ChangeUp(var S: String); virtual;}
    procedure ChangeRoot; virtual;
    {function  GetFullFlags: Word; virtual;}
    {procedure EditDescription(PF: PFileRec); virtual;}
    procedure GetDirLength(PF: PFileRec); virtual;
    destructor Done; virtual;
    function OpenDirectory(const Dir: String;
                                 PutDirs: Boolean): PDrive; virtual;
    procedure DrvFindFile(FC: PFilesCollection); virtual;
    procedure ReadDescrptions(FilesC: PFilesCollection); virtual;
  end;

implementation

uses Lfn, Drivers, Dos, Strings, Commands, PDSetup, Advance2;

procedure AddError(ErrorCode: Integer);
begin
{сделать локализацию!}
  MessageBox('Network error ('+ItoS(ErrorCode)+')', nil, mfError+mfOkButton);
end;

procedure CopyNetResource(var Source, Dest: TNetResource);
begin
  Dest.dwScope       := Source.dwScope;
  Dest.dwType        := Source.dwType;
  Dest.dwDisplayType := Source.dwDisplayType;
  Dest.dwUsage       := Source.dwUsage;
  Dest.lpLocalName   := StrNew(Source.lpLocalName);
  Dest.lpRemoteName  := StrNew(Source.lpRemoteName);
  Dest.lpComment     := StrNew(Source.lpComment);
  Dest.lpProvider    := StrNew(Source.lpProvider);
end;

procedure ClearNetResource(var NetResource: TNetResource);
begin
  with NetResource do
    begin
      StrDispose(lpLocalName);
      StrDispose(lpRemoteName);
      StrDispose(lpComment);
      StrDispose(lpProvider);
    end;
end;

function GetNetResourceName(var NetResource: TNetResource): String;
begin
  with NetResource do
    if lpRemoteName = nil then
      if lpComment = nil then
        Result := ''
      else
        Result := StrPas(lpComment)
    else
      Result := StrPas(lpRemoteName);
{$IFDEF Win32}
  CharToOemBuff(@Result[1], @Result[1], Length(Result));
{$ENDIF}
end;

procedure TNetDrive.ClearNetResources;
var
  I: LongInt;
begin
  if ConnectedCount <> 0 then
    begin
      for I := 0 to ConnectedCount-1 do
        ClearNetResource(ConnectedResources^[I]);
      FreeMem(ConnectedResources, ConnectedCount*SizeOf(TNetResource));
      ConnectedCount := 0;
      ConnectedResources := nil;
    end;
  if NetCount <> 0 then
    begin
      for I := 0 to NetCount-1 do
        ClearNetResource(NetResources^[I]);
      FreeMem(NetResources, NetCount*SizeOf(TNetResource));
      NetCount := 0;
      NetResources := nil;
    end;
end;

procedure TNetDrive.ClearRootResources;
var
  I: LongInt;
begin
  if RootCountAlloced <> 0 then
    begin
      for I := 0 to RootCount-1 do
        ClearNetResource(RootResources^[I]);
      FreeMem(RootResources, RootCountAlloced*SizeOf(TNetResource));
      FreeMem(RootResourceNames, RootCountAlloced*SizeOf(String));
      RootCount := 0;
      RootCountAlloced := 0;
      RootResources := nil;
      RootResourceNames := nil;
    end;
end;

procedure TNetDrive.AddRootResource(var NetResource: TNetResource; const DirName: String);
begin
  CopyNetResource(NetResource, RootResources^[RootCount]);
  RootResourceNames^[RootCount] := DirName;
  Inc(RootCount);
  if RootCount > RootCountAlloced then
    begin
      Inc(RootCountAlloced, 8);
      ReAllocMem(RootResources, RootCountAlloced*SizeOf(TNetResource));
      ReAllocMem(RootResourceNames, RootCountAlloced*SizeOf(String));
    end;
end;

procedure TNetDrive.RemoveRootResource(var DirName: String);
begin
  Dec(RootCount);
  ClearNetResource(RootResources^[RootCount]);
  if RootCount = 0 then
    DirName := cNET_
  else
    DirName := RootResourceNames^[RootCount-1];
end;

procedure TNetDrive.GetNetList(hEnum: THandle; var Res: PNetResourceArray; var EnumCount: LongInt);
var
  NetResource: TNetResourceArray;
  EnumCode, I: LongInt;
  TempNetCount: LongInt;
  TempNetSize: LongInt;
begin
{$IFDEF Win32}
  repeat
    TempNetCount := SizeOf(NetResource) div SizeOf(TNetResource);
    TempNetSize := SizeOf(NetResource);
    EnumCode := WNetEnumResource(hEnum, TempNetCount, @NetResource, TempNetSize);
    if (TempNetCount = 0) or (EnumCode = ERROR_NO_MORE_ITEMS) then
      Break;
    if EnumCode <> NO_ERROR then
      begin
        AddError(2);
        Exit;
      end;
    if TempNetCount > 0 then
      begin
        ReAllocMem(Res, (EnumCount+TempNetCount)*SizeOf(TNetResource));
        for I := 0 to TempNetCount-1 do
          CopyNetResource(NetResource[I], PNetResourceArray(Res)^[EnumCount+I]);
        Inc(EnumCount, TempNetCount);
      end;
  until False;
{$ENDIF}
end;

function TNetDrive.AddConnection(var NetResource: TNetResource): Boolean;
begin
{$IFDEF Win32}
  AddConnection := (WNetAddConnection2(NetResource, nil, nil, 0) = NO_ERROR);
{$ELSE}
  AddConnection := False; //вpеменно!
{$ENDIF}
end;

procedure TNetDrive.FindNetResources;
var
  hConnectedEnum: THandle;
  hEnum: THandle;
  CurResource: PNetResource;
begin
{$IFDEF Win32}
  if WNetOpenEnum(RESOURCE_CONNECTED, RESOURCETYPE_DISK, 0, nil, hConnectedEnum) <> NO_ERROR then
    begin
      AddError(1);
      Exit;
    end;
  GetNetList(hConnectedEnum, ConnectedResources, ConnectedCount);
  WNetCloseEnum(hConnectedEnum);

  if RootCount = 0 then
    CurResource := nil
  else
    CurResource := @RootResources^[RootCount-1];

  if WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_ANY, 0, CurResource, hEnum) <> NO_ERROR then
    if (CurResource = nil) or not AddConnection(CurResource^) or (WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_ANY, 0, CurResource, hEnum) <> NO_ERROR) then
      if CurResource = nil then
        begin
          AddError(3);
          Exit;
        end
      else
        begin
          hEnum := 0;
          NetCount := 0;
          Exit;
        end;
  GetNetList(hEnum, NetResources, NetCount);
  WNetCloseEnum(hEnum);
{$ENDIF}
end;

{
function TNetDrive.GetRootName(I: LongInt): String;
begin
  GetRootName := GetNetResourceName(RootResources^[I]);
end;
}

procedure TNetDrive.GetFreeSpace;
begin
  if RootState then
    S := ''
  else
    inherited GetFreeSpace(S);
end;

function  TNetDrive.GetRealName: String;
begin
  if RootState then
    Result := ''
  else
    Result := inherited GetRealName;
end;

constructor TNetDrive.Init;
begin
  inherited Init(2, AOwner);
  CurDir := cNET_;
  DriveType := dtNet;
  RootState := True;
  RootCountAlloced := 8;
  GetMem(RootResources, RootCountAlloced*SizeOf(TNetResource));
  GetMem(RootResourceNames, RootCountAlloced*SizeOf(String));
  FillChar(ColAllowed, SizeOf(ColAllowed), 0); //JO: доп. колонки не нужны
end;

constructor TNetDrive.Load(var S: TStream);
begin
  inherited Load(S);
  DriveType := dtNet;
  S.Read(RootState, SizeOf(RootState));
  if RootState then
    begin
      CurDir := cNET_;
      RootCountAlloced := 8;
      GetMem(RootResources, RootCountAlloced*SizeOf(TNetResource));
      GetMem(RootResourceNames, RootCountAlloced*SizeOf(String));
    end
  else
    S.ReadStrV(RootDirSaved);
end;

procedure TNetDrive.Store(var S: TStream);
begin
  inherited Store(S);
  S.Write(RootState, SizeOf(RootState));
  if not RootState then
    S.WriteStr(@RootDirSaved);
end;

destructor TNetDrive.Done;
begin
  ClearNetResources;
  inherited Done;
end;

procedure TNetDrive.ChangeRoot;
begin
  if RootState then
    lChDir(cNET_)
  else
    inherited ChangeRoot;
end;

procedure TNetDrive.lChDir;

  function IsReadable(const PathName: String): Boolean;
  var
    SR: lSearchRec;
  begin
  lFindFirst(PathName+'\*', AnyFileDir, SR);
  IsReadable := (DosError = 0);
  lFindClose(SR);
  end;

  procedure ChangeToSubDir(const LastDir: String);
  var
    I: LongInt;
    S: String;
  begin
    for I :=0 to NetCount-1 do
      with NetResources^[I] do
        begin
          S := GetNetResourceName(NetResources^[I]);
          if S = LastDir then
            begin
              if ((dwUsage and RESOURCEUSAGE_CONTAINER) = 0)
              and ((dwType and RESOURCETYPE_DISK) <> 0)
              and (lpRemoteName <> nil) then
                begin
                  if not IsReadable(S) then
                    begin
                      AddConnection(NetResources^[I]);
                      if IsReadable(S) then
                        begin
                          RootDirSaved := CurDir;
                          CurDir := S;
                          RootState := False;
                        end;
                    end
                  else
                    begin
                      RootDirSaved := CurDir;
                      CurDir := S;
                      RootState := False;
                    end;
                end
              else
                begin
                  CurDir := ADir;
                  AddRootResource(NetResources^[I], CurDir);
                end;
              Exit;
            end;
        end;
  end;

begin
  if ADir = '' then
    Exit;

  if Pos(cNET_, ADir) = 1 then
    if RootState then
      if Pos(ADir+'\', CurDir) = 1 then
        if Length(ADir) = Length(cNET_) then
          while RootCount > 0 do
            RemoveRootResource(CurDir)
        else
          RemoveRootResource(CurDir)
      else
        if Pos(CurDir+'\', ADir) = 1 then
          ChangeToSubDir(Copy(ADir, Length(CurDir)+2, MaxLongInt))
        else
          Exit
    else
      Exit
  else
    if RootState then
      begin
        RootDirSaved := CurDir;
        RootState := False;
        inherited lChDir(ADir);
      end
    else
      if (ADir[1] = '\') and (ADir[2] = '\') and (Pos('\', Copy(ADir, 3, MaxLongInt)) = 0) then
        begin
          CurDir := RootDirSaved;
          RootState := True;
        end
      else
        inherited lChDir(ADir);
end;

procedure TNetDrive.GetDown;
 var S: String;
begin
  if RootState then
    begin
    S :=  P^.FlName[True];
    MoveStr(B, S, C);
    LFN_inCurFileLine := true;
    end
  else
    inherited GetDown(B, C, P, LFN_inCurFileLine);
end;

function TNetDrive.GetDir;
begin
  GetDir := CurDir;
end;

function TNetDrive.GetRealDir: String;
begin
  GetRealDir := CurDir;
end;

procedure TNetDrive.MakeDir;
begin
 if not RootState then
   inherited MakeDir;
end;

function TNetDrive.GetDirectory;
var
  Files: PFilesCollection;
  P: PFileRec;
  S: String;
  I: LongInt;
{$IFDEF OS2} {отладочный код временно!!!}
  SR: lSearchRec;
const
  TempStr: String = 'D:\';
{$ENDIF}
begin
  if not RootState then
    begin
      GetDirectory := inherited GetDirectory(FileMask, TotalInfo);
      Exit;
    end;
  Files := New(PFilesCollection, Init($10, $10));
  Files^.Panel := Panel;
{ Files^.SortMode := SortMode;}

  if RootCount > 0 then
    begin
      {$IFNDEF OS2}
      P := NewFileRec('..', '..', 0, 0, 0, 0, Directory, @CurDir);
      {$ELSE}
      P := NewFileRec('..', 0, 0, 0, 0, 0, @CurDir);
      {$ENDIF}
      with Files^ do
        AtInsert(Count, P);
    end;

{$IFDEF OS2} {отладочный код временно!!!}
  lFindFirst('D:\*', (Directory shl 8) or Directory, SR);
  while (DosError = 0) do
    begin
    P := NewFileRec(SR.FullName, 0, 0, 0, 0, Directory, @TempStr);
    with Files^ do
      AtInsert(Count, P);
    lFindNext(SR);
    end;
  lFindClose(SR);
{$ELSE} {конец отладочного кода!!!}
  ClearNetResources;
  FindNetResources;
  for I := 0 to NetCount-1 do
    begin
      S := GetNetResourceName(NetResources^[I]);
      {$IFNDEF OS2}
      P := NewFileRec(S, S, 0, 0, 0, 0, Directory, @CurDir);
      {$ELSE}
      P := NewFileRec(S, 0, 0, 0, 0, 0, @CurDir);
      {$ENDIF}
      with Files^ do
        AtInsert(Count, P);
    end;
{$ENDIF}

{if PanelFlags and fmiFree <> 0 then
   GetFreeSpace(FreeSpace);}

{
  if Descriptions <> nil then
    begin
      TossDescriptions(@DizOwner, Files, Descriptions);
      Dispose(Descriptions, Done);
      if Files^.Sortmode = psmDIZ then
        Files^.Sort;
    end;
}

 GetDirectory := Files;
end;

procedure TNetDrive.CopyFiles(Files: PCollection; Own: PView; MoveMode: Boolean);
begin
  if not RootState then
    inherited CopyFiles(Files, Own, MoveMode);
end;

procedure TNetDrive.CopyFilesInto(Files: PCollection; Own: PView; MoveMode: Boolean);
begin
  if not RootState then
    inherited CopyFilesInto(Files, Own, MoveMode);
end;

procedure TNetDrive.EraseFiles(Files: PCollection);
begin
  if not RootState then
    inherited EraseFiles(Files);
end;

function TNetDrive.OpenDirectory(const Dir: String;
                                 PutDirs: Boolean): PDrive;
begin
  if RootState then
    Result := nil
  else
    Result := inherited OpenDirectory(Dir, PutDirs);
end;

procedure TNetDrive.DrvFindFile(FC: PFilesCollection);
begin
end;

procedure TNetDrive.GetDirInfo(var B: TDiskInfoRec);
begin
  if not RootState then
    inherited GetDirInfo(B);
end;

procedure TNetDrive.GetDirLength(PF: PFileRec);
begin
  if not RootState then
    inherited GetDirLength(PF);
end;

procedure TNetDrive.ReadDescrptions(FilesC: PFilesCollection);
  begin
  end;

end.
