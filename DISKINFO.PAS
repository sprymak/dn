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
uses LFN, Objects, Drivers, Views, Dialogs, Dos, HideView, RStrings,
     advance, advance1, advance2, advance3, Collect;

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
       InfoFile: Byte;
     end;

     PDiskInfo = ^TDiskInfo;
     TDiskInfo = object(THideView)
      Dir: string;
      FreeSpace, TotalSpace, DirLen: LongInt;
      FreeMemory, TotalMemory: LongInt;
      VolumeLabel: String[12];
      SerialNo: String[9]; { Rainbow }
      NumFiles: Integer;
      Info: TDiskInfoRec;
      Delta: TPoint;
      constructor Init(R: TRect);
      constructor Load(var S: TStream);
      procedure Store(var S: TStream); virtual;
      procedure ReadData(Loc: Boolean);
      procedure ShowView; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
      function GetPalette: PPalette; virtual;
      procedure Draw; virtual;
      destructor Done; virtual;
     end;

 procedure DispInfo(var Info: TDiskInfoRec); {DataCompBoy}

 function CountDirLen(const Dir: String; Recurse: Boolean; {DataCompBoy}
                      var ClusterLen: TSize; var NumFiles: Integer): TSize;
 procedure GetDrInfo(Drv: Byte);
 procedure ReadDiskInfo(const Dr: String; var B: TDiskInfoRec; ShowFlags: Integer);
 function  GetVolumeLabel: Str12;
 function Disk_Free(Drive: Byte): TSize;

const
       CDiskInfo     = #23#24;
       ifDirInfo     = 0;
       ifFileID      = 1;
       ifDescription = 2;
       ifReadMe      = 3;
       ifReadTxt     = 4;
       ifReadMeTxt   = 5;

       sDirinfo      = 'DirInfo';
       sFileID       = 'File_ID.DIZ';
       sReadMe       = 'Read.Me';
       sReadTxt      = 'Read.Txt';
       sReadMeTxt    = 'ReadMe.Txt';

type
     PTeamView = ^TTeamView;
     TTeamView = object(TView)
       LastTick: LongInt;
       Strings: Array [1..20] of Integer;
       Str:  Array[1..10] of String[32];
       Logo: Array[1..10] of String[32];
       OSP:  Array[1..10] of String[32];
       CState: (_Logo, _OSP, _Team);
       Q: Word;
       constructor Init(var R: TRect);
       procedure Draw; virtual;
       procedure Update; virtual;
       procedure HandleEvent(var Event: TEvent); virtual;
       destructor Done; virtual;
     end;

var SerFileSys:Boolean; { Rainbow }
const TeamView: PTeamView = nil;
function GetRndBytes(B, Rnd: LongInt): LongInt;

var
   FreeSpc, TotalSpc: TSize;
        {-DataCompBoy-}
   DriveData: Record
           {00}RecSize:             Word;
           {02}StructVersion:       Word;
           {04}SectorsPerCluster:   LongInt;
           {08}BytesPerSector:      Longint;
           {0C}AvailClusters:       LongInt;
           {10}TotalClusters:       LongInt;
           {14}NumPhisSectorsAvail: LongInt;
           {18}NumPhisSectorsTotal: LongInt;
           {1C}AvailAllocUnits:     LongInt;
           {20}TotalAllocUnits:     LongInt;
           {24}Reserved:            Array[0..7] of byte;
              end;
   BytesPerCluster: LongInt;
        {-DataCompBoy-}

implementation
uses Startup, ExtraMem, DNApp, Commands, DNHelp, Tree, xTime
     {$IFDEF DPMI}, Dpmi, Dosmem {$ENDIF}
     {$IFDEF VIRTUALPASCAL}, VpSysLow {$ENDIF}
     ;

const MaxTeam = 152;

const
      Logo1 = '          ,     sSSs     s$$s   ' ;
      Logo2 = '       ,s$$$Ss.  $$s.     $$    ' ;
      Logo3 = '     sS$$''  `SSb.$$$$.    $$    ';
      Logo4 = '    $$''$$     `$S.``$$.   $$    ';
      Logo5 = '       $$      `$$. `$$.  $$    ' ;
      Logo6 = '       $$       `$$  `$$. $$    ' ;
      Logo7 = '       $$       ,$$   `$$b$$    ' ;
      Logo8 = '    .s.$$.     ,$S''    `$$$$    ';
      Logo9 = '     `S$$$$$$$$$''s$     `$$$    ';
      Logo0 = '   0xeL`~~~~~~~ s$$s     s$$s   ' ;
      OSP1  = '                  $$.           ';
      OSP2  = '            ,Òy%@S$$.          ';
      OSP3  = '  .Öy%@%y·.j$$ß"^~``$$ $$$$$$b, ';
      OSP4  = '.dSS"^~^"$;7$l         $$''  `?$;';
      OSP5  = 'A$$''     `b`$S@%sÒ.   $$    l$$';
      OSP6  = '$$$       $bA`~^"ßÐ$$b,`$    j$$';
      OSP7  = '$$$       $$U       `$$ $s%@S$$P';
      OSP8  = 'U$$.     ,$;SS,     ,$$ $Ðßü^"` ';
      OSP9  = '`?$$bsssd$$P`$$$$$$$$P'',$       ';
      OSP0  = '  `''øÐÐÐø''`  `$$''      $$ [0xeL]';

function ESC_Pressed: Boolean; var E: TEvent;
begin
  Application^.Idle;
  GetKeyEvent(E); ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbEsc)
end;

function GetVolumeLabel;
var
  sr: SearchRec;
  VolumeLabel: Str12;

begin
  DOSError := 0;
  FindFirst('\*.*',VolumeID,sr);
  if DosError=0 then VolumeLabel:=sr.Name
                else VolumeLabel:=GetString(dlDINone);
  while PosChar('.',VolumeLabel) > 0 do Delete(VolumeLabel,PosChar('.',VolumeLabel),1);
  GetVolumeLabel := VolumeLabel;
end;

{ Rainbow BEGIN }
procedure GetSerFileSys(var SerialNo,FileSys:String);
{$IFNDEF VIRTUALPASCAL}
Type TDiskInfo=Record
      Nothing:Word;
      SerialNo:LongInt;
      _Label:Array[1..11] of Char;
      FileSystem:Array[1..8] of char;
     end;
var DWord:Array[1..2] of Word;
    R:{$IFDEF DPMI}DPMIRegisters{$ELSE}Registers{$ENDIF};
    A:Byte;
    DiskInfo: ^TDiskInfo;

  function Word2Hex(I:Word):String;
  const Values:Array[0..15] of Char='0123456789ABCDEF';
  var S,S1:String;
      A:Byte;
  begin
    S:='';
    S1:='';
    repeat
      S:=S+Values[I mod 16];
      I:=I div 16;
    until I=0;
    for A:=3 downto Length(S) do S1:=S1+'0';
    for A:=Length(S) downto 1 do S1:=S1+S[A];
    Word2Hex:=S1;
  end;

begin
  FillChar(R,SizeOf(R),0);
  {$IFDEF DPMI}
  DiskInfo := GetDosMem(SizeOf(DiskInfo^), R.DS);
  R.DX:=0;
  {$ELSE}
  New(DiskInfo);
  R.DS:=Seg(DiskInfo^);
  R.DX:=Ofs(DiskInfo^);
  {$ENDIF}
  FillChar(DiskInfo^,SizeOf(DiskInfo^),0);
  R.AH:=$69;
  R.AL:=$0;
  R.BX:=$0;
  R.Flags:=R.Flags or fCarry;
  {$IFDEF DPMI}SimulateRealModeInt{$ELSE}Intr{$ENDIF}($21,R);
  if R.Flags and fCarry=0 then
   begin
     SerFileSys:=True;
     Move(DiskInfo^.SerialNo,DWord,4);
     SerialNo:=Word2Hex(DWord[2])+'-'+Word2Hex(DWord[1]);
     FileSys:='';
     A:=1;
     while DiskInfo^.FileSystem[A]>#47 do
      begin
        FileSys:=FileSys+DiskInfo^.FileSystem[A];
        Inc(A);
      end;
   end
  else SerFileSys:=False;
  {$IFDEF DPMI}
  FreeDosMem(DiskInfo);
  {$ELSE}
  Dispose(DiskInfo);
  {$ENDIF}
end;
{ Rainbow END }
{$ELSE VIRTUALPASCAL}
begin
 SerFileSys:=False;
end;
{$ENDIF}

function GetRndBytes(B, Rnd: LongInt): LongInt;
begin
  GetRndBytes := (B div Rnd) * Rnd + Rnd * LongInt(B mod Rnd <> 0);
end;


constructor TTeamView.Init;
begin
  inherited Init(R);
  CState   := _Logo; Q := 0;
  UpdTicks := 1;
  Options  := Options or ofPreProcess;
  RegisterToBackground(@Self);
  FillChar(Str, SizeOf(Str), #32);
  Strings[ 1] := 00; Strings[ 2] := 00; Strings[ 3] := 00; Strings[ 4] := 00;
  Strings[ 5] := 00; Strings[ 6] := 00; Strings[ 7] := 00; Strings[ 8] := 00;
  Strings[ 9] := 00; Strings[10] := 00; Strings[11] := 00; Strings[12] := 00;
  Strings[13] := 00; Strings[14] := 00; Strings[15] := 00; Strings[16] := 00;
  Strings[17] := 00; Strings[18] := 00; Strings[19] := 00; Strings[20] := 00;
  Logo[ 1] := Logo1; Logo[ 2] := Logo2; Logo[ 3] := Logo3; Logo[ 4] := Logo4;
  Logo[ 5] := Logo5; Logo[ 6] := Logo6; Logo[ 7] := Logo7; Logo[ 8] := Logo8;
  Logo[ 9] := Logo9; Logo[10] := Logo0;
  OSP[ 1] := OSP1; OSP[ 2] := OSP2; OSP[ 3] := OSP3; OSP[ 4] := OSP4;
  OSP[ 5] := OSP5; OSP[ 6] := OSP6; OSP[ 7] := OSP7; OSP[ 8] := OSP8;
  OSP[ 9] := OSP9; OSP[10] := OSP0;
end;

procedure TTeamView.Draw;
  var B: TDrawBuffer;
      S: String;
      I: Integer;
      C: Word;
begin
  C := Owner^.GetColor($0807);
  Case CState of
   _Logo, _OSP: For I:=1 to 10 do begin
                 MoveChar(B, ' ', C, Size.X);
                 MoveStr(B[(Size.X-Length(Str[I])) div 2], Str[I], C);
                 WriteLine(0,I,Size.X,1,B);
                end;
   _Team: Begin
           S := GetString(dlTeam000);
           MoveChar(B, ' ', C, Size.X);
           MoveCStr(B[(Size.X-CStrLen(S)) div 2], S, C);
           WriteLine(0,0,Size.X,1,B);
           for I := 1 to Size.Y-1 do begin
            MoveChar(B, ' ', C, Size.X);
            if (Strings[I] > 0) and (Strings[I] < MaxTeam) then begin
             S := GetString(TStrIdx(Integer(dlTeam000)+Strings[I]));
             MoveCStr(B[(Size.X-CStrLen(S)) div 2], S, C);
            end;
            WriteLine(0,I,Size.X,1,B);
           end;
          end;
  end;
end;

procedure TTeamView.Update;
 var L, I, J: LongInt;
begin
 case CState of
  _Logo: Begin
          For I := 1 to 10 do If Logo[I] <> Str[I] then break;
          if Logo[I] = Str[I] then begin
           UpdTicks := 36;
           CState := _OSP;
          end else begin
           Repeat I := Random(10) + 1 until Str[I] <> Logo[I];
           L := Length(Logo[I]);
           Repeat J := Random(L) + 1 until Str[I][J] <> Logo[I][J];
           Str[I][J] := Logo[I][J];
           DrawView;
           If Q = 0 Then begin
            Inc(Q);
            Update;
           end else Q := 0;
          end;
         end;
  _OSP: Begin
          UpdTicks := 1;
          For I := 1 to 10 do If OSP[I] <> Str[I] then break;
          if OSP[I] = Str[I] then begin
           UpdTicks := 36;
           CState := _Team;
          end else begin
           Repeat I := Random(10) + 1 until Str[I] <> OSP[I];
           L := Length(OSP[I]);
           Repeat J := Random(L) + 1 until Str[I][J] <> OSP[I][J];
           Str[I][J] := OSP[I][J];
           DrawView;
           If Q = 0 Then begin
            Inc(Q);
            Update;
           end else Q := 0;
          end;
         end;
  _Team: begin
   UpdTicks := 9;
   Move(Strings[2], Strings[1], (Size.Y-2) * SizeOf(Integer));
   if (Strings[Size.Y-1] > 0) and (Strings[Size.Y-1] < MaxTeam)
   then Inc(Strings[Size.Y-1]);
   if Strings[Size.Y-1] = MaxTeam then Strings[Size.Y-1]:=0;
   if (Strings[Size.Y-1] = 0) and (Strings[1]=0) then Strings[Size.Y-1]:=1;
   DrawView;
  end;
 end;
end;

procedure TTeamView.HandleEvent(var Event: TEvent);
var t:integer;
begin
  t:=9;
  if (Event.What=evKeyDown) and (Event.KeyCode=kbDown) then begin
   ClearEvent(Event);
   if CState<>_Team then CState:=_Team else t:=36;
   Update;
   NewTimer(UpTmr,t)
  end else if (Event.What=evKeyDown) and (Event.KeyCode=kbUp) then begin
   ClearEvent(Event);
   Move(Strings[1], Strings[2], (Size.Y-2) * SizeOf(Integer));
   if Strings[1]>0 then Dec(Strings[1]);
   if (Strings[1]=0) and (Strings[Size.Y-1]=0) then Strings[1]:=MaxTeam;
   if CState<>_Team then CState:=_Team else t:=36;
   DrawView;
   NewTimer(UpTmr,t)
  end
end;

destructor TTeamView.Done;
begin
  TeamView := nil;
  inherited Done;
end;

procedure DispInfo;
begin
 with Info do begin
  if Title    <> nil then DisposeStr (Title);
  if Dir      <> nil then DisposeStr (Dir);
  if Files    <> nil then DisposeStr (Files);
  if Free     <> nil then DisposeStr (Free);
  if Total    <> nil then DisposeStr (Total);
  if VolumeID <> nil then DisposeStr (VolumeID);
  if SerialNo <> nil then DisposeStr (SerialNo); { Rainbow }
  if FileSys  <> nil then DisposeStr (FileSys); { Rainbow }
  if DirInfo  <> nil then Dispose    (DirInfo,Done);
 end;
 fillchar(Info, sizeof(Info), 0);
end;

constructor TDiskInfo.Load;
begin
  TObject.Init;
  inherited Load(S);
  S.Read(Delta, SizeOf(Delta));
end;

procedure TDiskInfo.Store;
begin
  Inherited Store(S);
  S.Write(Delta, SizeOf(Delta));
end;

constructor TDiskInfo.Init;
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
 ReadData(Off);
 inherited ShowView;
end;

function TDiskInfo.GetPalette;
 const S: String[Length(CDiskInfo)] =  CDiskInfo;
begin
 GetPalette := @S;
end;

procedure TDiskInfo.ReadData;
 var
     MemBlocks, I: Word;
begin
 Abort:=Off;
 {$IFNDEF OS_DOS}
 TotalMemory := MemAvail;
 {$ELSE}
 asm
  int $12
  mov word ptr memBlocks,ax
 end;
 TotalMemory := Longint(LongInt(MemBlocks) shl 10);
 {$ENDIF}
{$IFNDEF NONBP}
 FreeMemory := TotalMemory-Longint(PrefixSeg) shl 4;
{$ENDIF}
 DispInfo(Info);
 Message(Owner, evCommand, cmGetDirInfo, @Info);
 Delta.Assign(0, 0);
end;

procedure TDiskInfo.HandleEvent;
var S: String; {DataCompBoy}

        {-DataCompBoy-}
 procedure ReadInfoBBS;
   var F: PTextReader;
       I: Integer;
       Nm: String;
       DZ: string;
 begin
   if (Info.DirInfo = nil) then Info.DirInfo := New(PStringCollection , Init(20, 10))
                           else Info.DirInfo^.FreeAll;
   Info.InfoFile := ifDescription;
   I := -1;
   F := nil;
   DZ := FMSetup.DIZ;
   while (I<>0) and (F=nil) do
   begin
     I := Pos(';',DZ);
     if I = 0 then FreeStr := DZ else
     begin
       FreeStr := Copy(DZ, 1, I-1);
       Delete(DZ, 1, I);
     end;
     F := New(PTextReader, Init(GetPath(S)+FreeStr));
   end;
   if F = nil then Exit;
   Nm := GetName(lfGetShortFileName(S));
   while (IOResult = 0) and not F^.EOF do
     begin
       FreeStr := F^.GetStr;
       if (FreeStr[1] <> ' ') then
         begin
           I := PosChar(' ', FreeStr);
           if I = 0 then Continue;
           if UpStrg(Copy(FreeStr, 1, I-1)) = UpStrg(Nm) then
            begin
              Delete(FreeStr, 1, I);
              repeat
                DelLeft(FreeStr);
                Info.DirInfo^.AtInsert(Info.DirInfo^.Count, NewStr(FreeStr));
                FreeStr := F^.GetStr;
              until F^.EOF or (Info.DirInfo^.Count >= 100) or (FreeStr = '') or (FreeStr[1] <> ' ');
              Break;
            end;
         end;
     end;
   ClrIO; Dispose(F,Done); F:=nil;
 end;
        {-DataCompBoy-}

begin
 inherited HandleEvent(Event);
 if (Event.What = evKeyDown) then
  case Event.KeyCode of
   kbLeft : begin
             if Delta.X>0 then begin
              dec(Delta.X);
              SetState(sfExposed, On);
              DrawView
             end;
             ClearEvent(Event);
            end;
   kbRight: begin
             if Delta.X+Size.X-2<Info.Limit.X then begin
              Inc(Delta.X);
              SetState(sfExposed, On);
              DrawView
             end;
             ClearEvent(Event);
            end;
   kbDown : begin
             if Delta.Y<Info.Limit.Y then begin
              inc(Delta.Y);
              SetState(sfExposed, On);
              DrawView
             end;
             ClearEvent(Event);
            end;
   kbUp   : begin
             if Delta.Y>0 then begin
              Dec(Delta.Y);
              SetState(sfExposed, On);
              DrawView
             end;
             ClearEvent(Event);
            end;
  end
 else
  if (Event.What = evCommand) then
   case Event.Command of
    cmEditFile: begin
                  ClearEvent(Event);
                  case Info.InfoFile of
                    ifFileID: S := sFileID;
                   ifDirInfo: S := sDirInfo;
                    ifReadMe: S := sReadMe;
                   ifReadTxt: S := sReadTxt;
                 ifReadMeTxt: S := sReadMeTxt;
                      else Exit;
                  end;
                   S := MakeNormName(Dir, S);
                 Message(Application, evCommand, cmFileEdit, @S);
                end;
    cmInfoPresent: begin ClearEvent(Event); Event.InfoPtr := Owner end;
     cmRereadInfo: begin
                    ReadData(Off); DrawView; ClearEvent(Event);
                  end;
    cmLocalReread: begin
                    ReadData(On); DrawView; ClearEvent(Event);
                   end;
    cmLoadViewFile: begin
                      S := PString(Event.InfoPtr)^; {DataCompBoy}
                      ReadInfoBBS; DrawView; ClearEvent(Event);
                    end;
   end;
end;

procedure TDiskInfo.Draw;
 var B: TDrawBuffer;
     C,Y,X: Word;
     S1,S2: String[20];
     CC: Boolean;

procedure Wrt(S: String);
 var I: Integer;
begin
 MoveChar(B, ' ', C, Size.X);
 I := (Size.X-CStrLen(S)) div 2;
 if (I < 0) or CC then I := 0;
 MoveCStr(B[I], S, C);
 WriteLine(0,Y,Size.X, 1, B);
 Inc(Y);
end;

  procedure XWrt(P: PString; Flag: Word);
  begin
    if (P <> nil) and (DriveInfoData and Flag <> 0) then Wrt(P^);
  end;

begin
 C := (GetColor(1) shl 8) or (GetColor(2) and 255); Y := 0; CC := Off;
 if ((Info.Title <> Info.Dir) or (Info.Title <> Info.Dir)) and
    (DriveInfoData and (fdiTitle+fdiTotals) <> 0) then
   begin
     XWrt(Info.Title, fdiTitle);
     if (Info.Dir <> nil) and (DriveInfoData and fdiTitle <> 0) then Wrt('~'#0+Cut(Info.Dir^,Size.X)+#0'~');
     XWrt(Info.Files, fdiTotals);
     Wrt('');
   end;
 if ((Info.Total <> Info.Free) or (Info.Total <> Info.VolumeID)) and
    (DriveInfoData and (fdiVolumeSize+fdiVolumeFree+fdiVolumeLabel) <> 0) then
   begin
     XWrt(Info.Total, fdiVolumeSize);
     XWrt(Info.Free, fdiVolumeFree);
     XWrt(Info.VolumeID, fdiVolumeLabel);
   end;
 { Rainbow BEGIN }
 if SerFileSys then
  begin
    XWrt(Info.SerialNo, fdiSerialNo);
    XWrt(Info.FileSys, fdiFileSys);
  end;
 Wrt('');
 { Rainbow END }
 if DriveInfoData and fdiLowMemory <> 0 then
   Wrt('~'+FStr(TotalMemory shr 10)+GetString(dlDIMemoryTotal));
 if DriveInfoData and fdiAvailLowMemory <> 0 then
   Wrt('~'+FStr(FreeMemory)+GetString(dlDIMemoryForUser));
 if DriveInfoData and fdiMemAvail <> 0 then
   Wrt('~'+FStr(MemAvail)+GetString(dlDIMemoryForDN));
 if (EMSFound or XMSFound) and (Y > 0)
   and (DriveInfoData and (fdiEMSFree+fdiXMSFree) <> 0) then
  Wrt(#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196);
 if (DriveInfoData and fdiEMSFree <> 0) and EMSFound then
  begin
    Wrt('EMS: ~'+ItoS(EMSSize)+GetString(dlDIEMS1)+ItoS(EMSFreePages * 16)+GetString(dlDIEMS2));
  end;
 if (DriveInfoData and fdiXMSFree <> 0) and XMSFound then Wrt('~'+ItoS(XMSFree)+GetString(dlDIXMSFree));
 if (Y > 0) and (DriveInfoData and fdiDIZ <> 0) and
   (Info.DirInfo <> nil) and (Info.DirInfo^.Count > 0) then
   begin
     if Owner <> nil then C := Owner^.GetColor(1+Byte(Owner^.GetState(sfActive)));
     FillChar(FreeStr[1], Size.X, 196); FreeStr[0] := Char(Size.X);
     case Info.InfoFile of
       ifDirInfo: S1 := ' '+ sDirInfo   +' ';
        ifFileID: S1 := ' '+ sFileID    +' ';
        ifReadMe: S1 := ' '+ sReadMe    +' ';
       ifReadTxt: S1 := ' '+ sReadTxt   +' ';
     ifReadMeTxt: S1 := ' '+ sReadMeTxt +' ';
         else S1 := '';
     end;
     X := (Size.X - Length(S1)) div 2; if X < 0 then X := 0;
     Move(S1[1], FreeStr[X+1], Length(S1));
     Wrt(FreeStr);
     C := (GetColor(1) shl 8) or (GetColor(2) and 255);
     CC := On;
     for X := Delta.Y to Info.DirInfo^.Count - 1 do begin
      Wrt(#0+Copy(CnvString(Info.DirInfo^.At(X)), Delta.X, 255)+#0);
      if Y > Size.Y then Break;
     end;
   end;
 MoveChar(B, ' ', C, Size.X);
 if Y <= Size.Y - 1 then WriteLine(0, Y, Size.X, Size.Y - Y + 1, B);
end;

        {-DataCompBoy-}
function CountDirLen;
var
  Tmr: TEventTimer;
  L: TSize;
  SR: lSearchRec;
  DC: PDirCol;
begin
  NewTimer(Tmr, 1);
  ClusterLen:=0;
  L:=0;
  NumFiles:=0;

  new(DC, Init($10, $10));
  DC^.Insert(NewStr(Dir));
  Abort:=false;
  while DC^.Count>0 do
   begin
    FreeStr:=PString(DC^.At(0))^;
    DC^.AtFree(0);
    If Abort then break;

    if TimerExpired(Tmr) then
    begin
      NewTimer(Tmr, 3);
      if ESC_Pressed then begin Abort := True; L:=0; Break end;
    end;

    If Word(Length(FreeStr))>MaxPathLen-1 then Begin Abort := True; Break End;

    DosError := 0; Abort := Off;

    lFindFirst(MakeNormName(FreeStr, x_x), $3F xor VolumeID, SR);
    While (DOSError = 0) and not Abort do
    begin
     if (SR.SR.Name[1] <> '.') then
      begin
       Inc(NumFiles);
       if SR.SR.Attr and Directory <> 0 then
        begin
         if Recurse then
          begin
           If (Word(Length(FreeStr))+Word(Length(SR.FullName))>MaxPathLen-1)
           or (MaxAvail<Length(FreeStr+SR.FullName)+$41) then
           Begin Abort := True; L:=0; Break End;
           DC^.Insert(NewStr(MakeNormName(FreeStr, SR.FullName)));
          end;
        end
       else
        begin
         L:=L+SR.FullSize;
         if BytesPerCluster > 0 then
          begin
           ClusterLen:=ClusterLen+(SR.FullSize / BytesPerCluster)*BytesPerCluster;
           if Round(SR.FullSize/BytesPerCluster)-(SR.FullSize/BytesPerCluster)<>0 then ClusterLen:=ClusterLen+BytesPerCluster;
          end;
        end;
      end;
     lFindNext(SR);
    end;
    lFindClose(SR);
   end;
  DC^.FreeAll;
  Dispose(DC,Done);
  CountDirLen := L;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
{$IFNDEF VIRTUALPASCAL}
const DriveConstant: array[0..3] of Char = ('?',':','\',#0);
      SizeOfDriveData: Word = SizeOf(DriveData);
procedure GetSpace(Drive : Char);
assembler;
asm
  pusha

  push ES
  push DI
  mov CX, SizeOfDriveData
  shr CX, 1
  mov AX, SEG DriveData
  mov ES, AX
  mov DI, OFFSET DriveData
  mov AX, 0
  cld
@@REP:
  rep stosw
{i286 REP bug fix begin}
  jcxz  @@next
  loop  @@REP
@@NEXT:
{i286 REP bug fix end}
  pop DI
  pop ES

  mov AH,$36
  mov DL,Drive
  sub DL,64
  int $21
  push AX
  mov AX, SEG DriveData
  mov ES, AX
  mov SI, OFFSET DriveData
  pop AX
  mov ES:[SI+004h], AX {sectors per cluster}
  mov ES:[SI+008h], CX {Bytes per sector}
  mov ES:[SI+00Ch], BX {avail clusters}
  mov ES:[SI+010h], DX {total clusters}

  mov CX, SizeOfDriveData
  mov AX, SEG DriveConstant
  mov DS, AX
  mov DI, OFFSET DriveConstant
  mov DX, DI
  mov AL, Drive
  mov byte ptr DS:[DI], AL
  mov AX, SEG DriveData
  mov ES, AX
  mov DI, OFFSET DriveData
  mov AX, 7303h
  int 21h
  popa
end;
{$ENDIF}
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure GetDrInfo(Drv: Byte);
 var Dr : Char;
begin
 if Drv = 0 then Dr := GetCurDrive else Dr := Char(Drv+64);
 {$IFNDEF VIRTUALPASCAL}
 GetSpace(Dr);
 BytesPerCluster:=DriveData.SectorsPerCluster;
 BytesPerCluster:=BytesPerCluster*DriveData.BytesPerSector;
 FreeSpc:=BytesPerCluster;
 FreeSpc:=FreeSpc*DriveData.AvailClusters;
 TotalSpc:=BytesPerCluster;
 TotalSpc:=TotalSpc*DriveData.TotalClusters;
 {$ELSE}
 BytesPerCluster:=0;
 FreeSpc :=SysDiskFreeLong(Drv);
 TotalSpc:=SysDiskSizeLong(Drv);
 {$ENDIF}
end;
        {-DataCompBoy-}

function Disk_Free(Drive: Byte): TSize;
begin
  GetDrInfo(Drive);
  Disk_Free := FreeSpc;
end;

        {-DataCompBoy-}
procedure ReadDiskInfo;
 var  VolumeLabel: String[12];
      SerialNo: String[9]; { Rainbow }
      FileSys: String[8]; { Rainbow }
      L: Byte absolute VolumeLabel;
      S: String;
      S1, S2: String[30];
      Drv: Byte;
      F: PTextReader;
      DirLen: TSize;
      NumFiles: Integer;
begin
  ClrIO;
  if Dr = '' then GetDir(0, S)
              else S := Dr;
  if Abort then Exit;

  DispInfo(B);

  B.Title := NewStr(GetString(dlDICurDir)+':');
  B.Dir := NewStr(S);
  Drv := Byte(S[1])-64;

  GetDrInfo(Drv);if Abort then Exit;
  BytesPerCluster:=DriveData.BytesPerSector;
  BytesPerCluster:=BytesPerCluster*DriveData.SectorsPerCluster;

  DirLen := 0; NumFiles := 0;
  CountDirLen(S, False, DirLen, NumFiles);

  B.Total := NewStr('~'+FStr(TotalSpc)+GetString(dlDITotalDisk)+Copy(S,1,2)+'~');
  B.Free := NewStr('~'+FStr(FreeSpc)+GetString(dlDIFreeDisk)+Copy(S,1,2)+'~');

  if NumFiles = 0 then B.Files := NewStr(GetString(dlDINoFiles))
   else
    begin
     if NumFiles = 1 then S1 := GetString(dlDIFile) else S1 := GetString(dlDIFiles);
     if DirLen = 1 then S2 := GetString(dlDIByte) else S2 := GetString(dlDIBytes);
     B.Files := NewStr('~'+FStr(NumFiles)+'~ '+S1+GetString(dlDIWith)+'~'+FStr(DirLen)+'~ '+S2);
    end;

  B.InfoFile := ifDirInfo;
  if (ShowFlags and psShowLongDesc) <> 0 then
  begin
    B.DirInfo  := nil;
  end else
  begin
    B.DirInfo := New(PStringCollection, Init($10, $10));
    F := New(PTextReader, Init(MakeNormName(S, sDIRINFO)));
    if F = nil then
    begin
      B.InfoFile := ifFileID;
      F := New(PTextReader, Init(MakeNormName(S, sFileID)));
    end;
    if F = nil then
    begin
      B.InfoFile := ifReadMe;
      F := New(PTextReader, Init(MakeNormName(S, sReadMe)));
    end;
    if F = nil then
    begin
      B.InfoFile := ifReadTxt;
      F := New(PTextReader, Init(MakeNormName(S, sReadTxt)));
    end;
    if F = nil then
    begin
      B.InfoFile := ifReadMeTxt;
      F := New(PTextReader, Init(MakeNormName(S, sReadMeTxt)));
    end;
    B.Limit.X:=0; B.Limit.Y:=0;
    if F <> nil then
    begin
      while not F^.EOF and (B.DirInfo^.Count < 100) do
        begin
          S := F^.GetStr;
          if Length(S) > B.Limit.X then B.Limit.X:=Length(S);
          B.DirInfo^.AtInsert(B.DirInfo^.Count, NewStr(S));
          inc(B.Limit.Y);
        end;
      Dispose(F,Done); F:=nil;
    end;
  end;
  VolumeLabel := GetVolumeLabel;
  B.VolumeID := NewStr(GetString(dlDIVolumeID) + Char(Drv+64) + ': ' + VolumeLabel + '~');
  { Rainbow BEGIN }
  GetSerFileSys(SerialNo,FileSys);
  B.SerialNo := NewStr(GetString(dlDISerialNo) + Char(Drv+64) + ': ' + SerialNo + '~');
  B.FileSys := NewStr(GetString(dlDIFileSys) + Char(Drv+64) + ': ' + FileSys + '~');
  { Rainbow END }
end;
        {-DataCompBoy-}

end.
