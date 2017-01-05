{ Win32-specific file tools unit by J.Osadtchiy (JO)}
{ Optimise-}

{$I STDEFINE.INC}
unit FlW32Tl;
interface

function GetBytesPerCluster(Drive: Byte): Longint;

implementation

 uses Windows;

function GetBytesPerCluster(Drive: Byte): Longint;
var
  RootPath: array[0..3] of Char;
  RootPtr: PChar;
  SectorsPerCluster,BytesPerSector,FreeClusters,TotalClusters: DWord;
begin
  RootPtr := nil;
  if Drive > 0 then
  begin
    RootPath[0] := Char(Drive + (Ord('A') - 1));
    RootPath[1] := ':';
    RootPath[2] := '\';
    RootPath[3] := #0;
    RootPtr := RootPath;
  end;
  if GetDiskFreeSpace(RootPtr, SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters) then
      GetBytesPerCluster := SectorsPerCluster * BytesPerSector
    else GetBytesPerCluster := 0;
end;

begin
end.