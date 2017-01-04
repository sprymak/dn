{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.05/DOS
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
//////////////////////////////////////////////////////////////////////////
//
//      LFNизация выполнена Антоном Федоpовым aka DataCompBoy'ем.
//                  В честь Матаpыкиной Ульяны...
//////////////////////////////////////////////////////////////////////////}

{$I STDEFINE.INC}
{$I DN.DEF}

unit FilesCol;

interface
 uses LFN, LFNCol, {DataCompBoy} drvtypes, Objects, Memory, DOS, Startup,
 DNHelp, advance, advance1, advance2, advance3, ObjType;

type
    PDIZ = ^TDIZ;
    TDIZ = record
      Owner: PString;
      DIZ: PString;
      Line: LongInt;
      isDisposable: Boolean;
    end;

    PFileRec = ^TFileRec;
    TFileRec = record
     Size: TSize;
     PSize: TSize;
     Owner: PString;
     OwnerDisposible: Boolean;
     Diz: PDIZ;
     Name: Str12;
     LFN: TLFNIndex; {DataCompBoy}
     Yr: Word;
     TType: Byte;
     Attr: Word;
     Second: Byte;
     Selected: Boolean;
     case Boolean of
       True : (FDate: LongInt);
       False: (Minute, Hour, Day, Month: Byte);
    end;

    TMakeListRec = record
     FileName  : String; {DataCompBoy}
     Header    : String;
     HeaderMode: Word;
     Action    : String;
     Footer    : String;
     FooterMode: Word;
     Options   : Word;
    end;

        {-DataCompBoy-}
    PUserParams = ^TUserParams;
    TUserParams = record
      Active, Passive: PFileRec;
      ActiveList, PassiveList: String;
    end;
        {-DataCompBoy-}

    PLineCollection = PTextCollection;
    TLineCollection = TTextCollection;

    PFilesCollection = ^TFilesCollection;
    TFilesCollection = object(TSortedCollection)
     SortMode: Byte;
     Selected: Integer;
     Owner: Pointer;
     procedure FreeItem(Item: Pointer); virtual;
     function Compare(Key1, Key2: Pointer): Integer; virtual;
     constructor Load(var S: TStream);
     procedure Store(var S: TStream); virtual;
    end;

const
      cmlPathNames     = 1;
      cmlAutoDetermine = 2;

      hfmAuto          = 0;
      hfmInsertText    = 1;
      hfmInsertFiles   = 2;

      RFilesCollection: TStreamRec = (
       ObjType: otFilesCollection;
       VmtLink: Ofs(TypeOf(TFilesCollection)^);
       Load: @TFilesCollection.Load;
       Store: @TFilesCollection.Store);

function  SelectDrive(X, Y: Integer; Default: Char; IncludeTemp: Boolean): String;
function  CopyFileRec(FR: PFileRec): PFileRec; {DataCompBoy}
function  CreateFileRec(Name: string): PFileRec;
function  NewFileRec(const LFN, Name: String; Size:TSize; Date: LongInt; Attr: Word; AOwner: PString): PFileRec; {DataCompBoy}
procedure DelFileRec(var FR: PFileRec); {DataCompBoy}
function  LoadFileRec(var s: TStream): PFileRec; {DataCompBoy}
procedure StoreFileRec(var s: TStream; fr: PFileRec); {DataCompBoy}
function  PackedDate(P: PFileRec): LongInt; {DataCompBoy}
function GetFileType(const S: String; Attr: Byte): Integer;

 const Executables: String[21] = #0'exe'#0'bat'#0'com'#0;

implementation
uses DNApp, Menus, Drivers, Views, FlPanel, Drives,
     Commands, DiskTool, Messages
     {$IFDEF MODEM}
     ,NavyLink
     {$ENDIF}
     , Novell, DnIni

;

const
    pfrPacked   = $80;
    pfrSelect   = $40;

type
    TPackedFileRec = record
      Time: LongInt;
      Attr: Byte;
      NLen: Byte;
    end;

        {-DataCompBoy-}
function  CreateFileRec(Name: string): PFileRec;
 var fr: PFileRec;
     lsr: lSearchRec;
     D: DateTime;
 begin
  new(fr);
  CreateFileRec:=fr;
  fillchar(fr^, sizeof(fr^), 0);
  Name:=lFExpand(Name);
  lFindFirst(Name+'*', AnyFile, lsr);
  while (lsr.FullName<>Name) and (DosError=0) do lFindNext(lsr);
  lFindClose(lsr);
  if DosError=0 then
   begin
    fr^.name:=Norm12(lsr.sr.name);
    fr^.LFN:=AddLFN(lsr.FullName);
    fr^.Size:=lsr.FullSize;
    fr^.PSize:=lsr.FullSize;
    fr^.Owner:=NewStr(GetPath(Name)+'\');
    fr^.OwnerDisposible:=true;
    fr^.Diz:=nil;
    UnpackTime(lsr.sr.Time, D);
    fr^.Yr := D.Year;
    fr^.Month := D.Month;
    fr^.Day := D.Day;
    fr^.Hour := D.Hour;
    fr^.Minute := D.Min;
    fr^.Second := D.Sec;
    fr^.Attr:=lsr.sr.attr;
    fr^.TType := GetFileType(fr^.Name, fr^.Attr);
    if fr^.Attr and Directory <> 0 then UpStr(fr^.Name) else LowStr(fr^.Name);
   end
  else
   begin
    fr^.Owner:=NewStr(GetPath(Name));
    fr^.OwnerDisposible:=true;
    fr^.Name:=Norm12(GetURZ(GetName(lfGetShortFileName(Name))));
    fr^.LFN:=AddLFN(GetName(Name));
   end;
 end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function CopyFileRec;
 var P: PFileRec;
begin
 New(P);
 P^ := FR^;
 P^.LFN := UseLFN(FR^.LFN);
 if P^.DIZ <> nil then P^.DIZ^.isDisposable := Off;
 CopyFileRec := P;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure DelFileRec(var FR: PFileRec);
begin
 if FR <> nil then
   begin
     if (FR^.DIZ <> nil) and FR^.DIZ^.isDisposable then
      begin
        DisposeStr(FR^.DIZ^.DIZ);
        Dispose(FR^.DIZ);
      end;
     if FR^.OwnerDisposible then DisposeStr(FR^.Owner);
     DelLFN(FR^.LFN);
     Dispose(FR);
   end;
 FR:=Nil;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function LoadFileRec(var s: TStream): PFileRec;
var P: PFileRec;
begin
 new(P); fillchar(p^, sizeof(p^), 0);
 with P^ do begin
  s.Read(Size, Sizeof(Size));
  s.Read(PSize, Sizeof(PSize));
  S.Read(Name, SizeOf(Name));
  LFN:=LoadLFN(S);
  S.Read(TType, SizeOf(TType));
  S.Read(Attr, SizeOf(Attr));
  S.Read(Second, SizeOf(Second));
  S.Read(Selected, SizeOf(Selected));
  S.Read(FDate, SizeOf(FDate));
  S.Read(OwnerDisposible, SizeOf(OwnerDisposible));
  if OwnerDisposible then Owner:=S.ReadStr;
 end;
 LoadFileRec:=P;
end;

procedure StoreFileRec(var s: TStream; fr: PFileRec);
begin with fr^ do begin
  s.Write(Size, Sizeof(Size));
  s.Write(PSize, Sizeof(PSize));
  S.Write(Name, SizeOf(Name));
  StoreLFN(S, LFN);
  S.Write(TType, SizeOf(TType));
  S.Write(Attr, SizeOf(Attr));
  S.Write(Second, SizeOf(Second));
  S.Write(Selected, SizeOf(Selected));
  S.Write(FDate, SizeOf(FDate));
  S.Write(OwnerDisposible, SizeOf(OwnerDisposible));
  if OwnerDisposible then S.WriteStr(Owner);
end end;
        {-DataCompBoy-}

        {-DataCompBoy-}
constructor TFilesCollection.Load;
 var I: Integer;
begin
 inherited Init($10,$10);
 S.Read(Count, SizeOf(Integer)*3);
 I := Limit;
 Limit := 0;
 SetLimit(I);
 for I := 0 to Count-1 do
   AtPut(I, LoadFileRec(S));
 S.Read(Selected, SizeOf(Selected));
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TFilesCollection.Store;
 var I, J, Sel: Integer;
begin
 J := 0;
 for I := 1 to Count do
  if (I - 1 = Selected) or (PFileRec(At(I-1))^.Selected) then
   begin if I - 1 = Selected then Sel := J; Inc(J) end;
 I := Count; Count := J;
 S.Write(Count, SizeOf(Integer)*3); Count := I;
 for I := 1 to Count do
  if (I - 1 = Selected) or (PFileRec(At(I-1))^.Selected) then
   StoreFileRec(S, At(I-1));
 S.Write(Sel, SizeOf(Sel));
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TFilesCollection.FreeItem;
var P: PFileRec absolute Item; begin DelFileRec(P); end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function TFilesCollection.Compare;
 var T1: TFileRec;
     P2: PFileRec absolute Key2;
     C: Integer;
     SM, I1, I2: Integer;
     P1P, P2P: Boolean;
     PanelFlags: Word;
     OneIsFile: Boolean;
begin
 PanelFlags := PFilePanel(Owner)^.PanelFlags;
 Move(Key1^,T1,SizeOf(T1));
 with P2^ do
   begin
     P1P := T1.Name[1] = '.';
     P2P := Name[1] = '.';

     if P1P and not P2P then begin Compare := -1; Exit; end;
     if P2P and not P1P then begin Compare := +1; Exit; end;

     OneIsFile:=True;
{    if (T1.Attr and Directory <> 0) and (Attr and Directory <> 0) then
       OneIsFile:=False;}

     if GetLFN(T1.LFN)=GetLFN(LFN) then C := 0 else
      if GetLFN(T1.LFN)>GetLFN(LFN) then C := 1
                                     else C := -1;

     SM := SortMode;
     if (SM >= 5) and (SM <= 20) then begin
       if (T1.Name = Name) and ((T1.Attr or Attr) and Directory = 0) and
          (((SM-5) and 1 = 0) or (T1.Size = Size)) and
          (((SM-5) and 2 = 0) or (T1.Yr < Yr) or
           ((T1.Yr = Yr) and (T1.FDate <= FDate)) or
           ((T1.Yr = Yr) and (T1.FDate = FDate) and (T1.Second < Second))) and
          (((SM-5) and 4 = 0) or (T1.Attr = Attr)) and
          (((SM-5) and 8 = 0) or
                  CompareFiles(MakeNormName(T1.Owner^, MakeFileName(T1.Name)),
                               MakeNormName(Owner^, MakeFileName(Name))) )
           then C := 0 else if (T1.Name < Name) then C := -1
                                                 else C := 1;
       Compare := C;
       Exit;
     end;

     if (Owner <> nil) and (SM <> 3) and (SM <> 4) then
     begin
       if PanelFlags and fmiExeFirst <> 0 then
       begin
         if ((T1.Attr xor Attr) and Directory = 0) then
         begin
           if (T1.TType = ttExec) and (TType <> ttExec) then begin Compare := -1; Exit; end;
           if (T1.TType <> ttExec) and (TType = ttExec) then begin Compare := 1; Exit; end;
         end;
       end;
       if PanelFlags and fmiArchivesFirst <> 0 then
       begin
         if ((T1.Attr xor Attr) and Directory = 0) then
         begin
           if (T1.TType = ttArc) and (TType <> ttArc) then begin Compare := -1; Exit; end;
           if (T1.TType <> ttArc) and (TType = ttArc) then begin Compare := 1; Exit; end;
         end;
       end;
     end;

     case SM of
      0: if T1.Owner^ = Owner^ then
          if (T1.Name = Name) and (GetLFN(T1.LFN) = GetLFN(LFN)) then C := 0
                                                                 else C := -1
         else if T1.Owner^ < Owner^ then C := -1 else C := 1;
      30: begin
           I1 := T1.TType; I2 := TType;
           if I1 = 0 then I1 := 100;
           if I2 = 0 then I2 := 100;
           if I1 < I2 then C := -1 else
           if I1 > I2 then C := 1 else
              if T1.Name < Name then C := -1
                else if T1.Name > Name then C := 1
                  else if T1.Owner^ < Owner^ then C := -1
                    else if T1.Owner^ > Owner^ then C := 1;
          end;
      1: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if T1.Name < Name then C := -1
            else if T1.Name > Name then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
     141: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if T1.Name < Name then C := -1
            else if (T1.Name > Name) or (GetLFN(T1.LFN)<>GetLFN(LFN))
                  then C := 1
                  else C := 0;
      2: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C:=-1
          else                               C:=1
         else
         if OneIsFile and (Copy(T1.Name, 10, 3) < Copy(Name, 10, 3)) then
           C:=-1
         else
         if OneIsFile and (Copy(Name, 10, 3) < Copy(T1.Name, 10, 3)) then
           C:=1
         else
         if T1.Name   < Name   then C:=-1 else
         if T1.Name   > Name   then C:= 1 else
         if T1.Owner^ < Owner^ then C:=-1 else
         if T1.Owner^ > Owner^ then C:= 1;
      3: if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if T1.Size > Size then C := -1 else
           if T1.Size < Size then C := 1 else
           if T1.Name < Name then C := -1
            else if T1.Name > Name then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
      4: begin
         if (T1.Attr xor Attr) and Directory <> 0 then
          if T1.Attr and Directory <> 0 then C := -1 else C := 1 else
           if (T1.Yr < Yr) then C := 1 else
           if (T1.Yr > Yr) then C := -1 else
           if (T1.FDate < FDate) then C := 1 else
           if (T1.FDate > FDate) then C := -1 else
           if (T1.Second < Second) then C := 1 else
           if (T1.Second > Second) then C := -1 else
           if T1.Name < Name then C := -1
            else if T1.Name > Name then C := 1
             else if T1.Owner^ < Owner^ then C := -1
               else if T1.Owner^ > Owner^ then C := 1;
         end;
      else if (T1.Attr and $3F = Attr and $3F) and (T1.Yr=Yr) and (T1.FDate = FDate) and (T1.Second = Second)
              and (T1.Name=Name) and (T1.Owner^ = Owner^) and (GetLFN(T1.LFN)=GetLFN(LFN))
               then C := 0 else C := -1;
     end;
  end;
 Compare := C;
end;
        {-DataCompBoy-}

function SelectDrive;
 var R: TRect;
     P: PHugeMenuBox;
     Menu: PMenu;
     Items, Lnk: PMenuItem;
     C: Char;
     N, MaxRY: Integer;
     SC: CharSet;
     Server_Num, Handle_Num, RetCode, DriveNum, MaxL: integer; {-$VIV start}
     FullS, Server, PathName, TmpS: String;       {-$VIV end}
     pSaveNeedAbort,ShowDir: Boolean;

  function CutLongString(S: String): String;
  var P, L: Integer;
      S1: String;
  begin
    CutLongString := #0+S+#0;
    if not CutDriveInfo then Exit;
    (* X-Man *)
    CutLongString:=FormatLongName(S,30,0,flnPreferName+flnHighlight+
    flnUseCutChar+flnHandleTildes,nfmNull,nil)
  end;

begin
 Items := nil; N := 0; Lnk := nil;
 MaxL := 8; {-$VIV}

 {$IFDEF MODEM}
 if IncludeTemp and (Linker <> nil) then
   begin
     CL_GetLinkDrives(SC);
     if SC <> [] then
       begin
         for C := 'Z' downto 'A' do
           if C in SC then
             Items := NewItem('  ~'+C+':~  ', '', kbNoKey, 2000 + Byte(C),
                               hcNoContext, Items);
         Items := NewSubMenu(' LINK', hcNoContext, NewMenu(Items), nil);
         Lnk := Items;
         Inc(N)
       end;
   end;
 {$ENDIF}

 if IncludeTemp then begin
     Items := NewItem(' TEMP ', '', kbSpace, 1200, hcTempList, Items);
     Inc(N)
 end;

 for C := 'Z' downto 'A' do
  if ValidDrive(C) then
   begin
    FullS := '  ~'+C+':~  ';                          {-$VIV start}
    if InterfaceData.Options and ouiHideDriveInfo = 0 then
    begin
      DriveNum := Byte(C)-64;
      if DriveInfoType and ditMediaType <> 0 then
       begin
        case getDriveType(DriveNum) of
         dtError     : FullS := FullS + sdtError     ;
         dtFixed     : FullS := FullS + sdtFixed     ;
         dtRemovable : FullS := FullS + sdtRemovable ;
         dtRemote    : FullS := FullS + sdtRemote    ;
         dtCDROM     : FullS := FullS + sdtCDROM     ;
         dtDblSpace  : FullS := FullS + sdtDblSpace  ;
         dtSUBST     : FullS := FullS + sdtSUBST     ;
         dtStacker   : FullS := FullS + sdtStacker   ;
         dtRAMDrive  : FullS := FullS + sdtRAMDrive  ;
         dtDublDisk  : FullS := FullS + sdtDublDisk  ;
         dtBernoully : FullS := FullS + sdtBernoully ;
         dtDiskreet  : FullS := FullS + sdtDiskreet  ;
         dtSuperStor : FullS := FullS + sdtSuperStor ;
        end;
       end;
      Get_drive_connection_id(DriveNum, Server_num);
      if (Server_num > 0) and (DriveInfoType and ditNetDir <> 0) then
      begin
        Get_File_Server_Name(Server_Num, Server);
        Set_Preferred_Connection_Id(Server_Num);
        Get_Drive_Handle_Id(DriveNum, Handle_Num);
        Get_Directory_Path(Handle_Num, PathName, RetCode);
        lGetDir(DriveNum, TmpS); {DataCompBoy}
        if Length(TmpS) > 3 then
        begin
          TmpS := ' <' + Copy(PathName, Length(PathName) - Length(TmpS) + 4, 255) + '>';
          Delete(PathName, Length(PathName) - Length(TmpS) + 4, 255);
          if PathName[Length(PathName)] = '/' then Delete(PathName, Length(PathName), 1);
        end else TmpS := '';
        PathName := CutLongString(Server + '/' + PathName);
        PathName := PathName + TmpS;
        FullS := FullS + PathName;
      end else begin
          if (DriveNum<=2) or ((DriveInfoType and ditCDDir=0)
          and (DriveInfoType and ditLocalDir=0)) then ShowDir:=False
          else if (DriveInfoType and ditCDDir<>0)
          and (DriveInfoType and ditLocalDir<>0) then ShowDir:=True
          else if (DriveInfoType and ditLocalDir<>0)
          then ShowDir:=not IsDriveCDROM(Char(DriveNum+64))
          else ShowDir:=IsDriveCDROM(Char(DriveNum+64));
      if ShowDir then
      begin
        pSaveNeedAbort := NeedAbort;
        NeedAbort := True;
        lGetDir(DriveNum, PathName);{DataCompBoy}
        if IOResult = 0 then
        begin
          if Length(PathName) = 3 then PathName := '' else
          begin
            Delete(PathName, 1, 3);
            PathName := '<' + CutLongString(PathName) + '>';
          end;
          FullS := FullS + PathName;
        end;
        NeedAbort := pSaveNeedAbort;
      end
      end;
      if Length(FullS) > MaxL then MaxL := Length(FullS);
    end;                                                {-$VIV end}

    Items := NewItem(FullS, '', kbNoKey, 1000 + Byte(C),
                      hcNoContext, Items);
    Inc(N);
   end;
 if not (Default in ['A'..'Z']) and not ((Default = '+') and (Lnk <> nil)) then
 asm
  mov ah, 19h
  int 21h
  add al, 'A'
  mov C, al
 end else C := Default;
 Menu := NewMenu(Items);
 DeskTop^.GetExtent(R);
 {-$VIV start}
 X := X - (MaxL div 2);
 if (X + MaxL + 4) > R.B.X then X := R.B.X - MaxL - 4;
 if (X < 0) then X := 0;
 Y := Y - (N div 2) + 1;
 if (Y + N + 2) > R.B.Y then Y := R.B.Y - N - 2;
 if (Y < 0) then Y := 0;
 R.A.X := X; R.A.Y := Y;
 R.B.X := R.A.X + MaxL + 4;
 MaxRY := R.B.Y;
 R.B.Y := R.A.Y + N + 2;
 if R.A.Y=0 then begin Inc(R.A.Y); Inc(R.B.Y) end;
 if (R.B.Y > MaxRY) then R.B.Y := MaxRY;
 {-$VIV end}
 P := New(PHugeMenuBox, Init(R, Menu, nil)); {-$VIV}
 if (C='+') then Items := Lnk else Items := P^.FindItem(C);
 if Items <> nil then Menu^.Default := Items;
 P^.HelpCtx := hcSelectDrive + Byte( IncludeTemp = True );

 N := DeskTop^.ExecView(P);
 Dispose(P, Done);
 DisposeMenu(Menu);
 if N > 1000 then SelectDrive := Char(N - 1000)+':' else SelectDrive := '';
 if N = 1200 then SelectDrive := cTEMP_;
 if N > 2000 then SelectDrive := '+' + Char(N - 2000);
end;

 var Q: Str5;

function GetFileType(const S: String; Attr: Byte): Integer;
begin
  if Attr and Directory <> 0 then GetFileType := ttDirectory else
  begin
    GetFileType := 0;
    asm
      cld
      les   si, S
      mov   di, offset Q + 2
      mov   ah, [es:si]
      sub   ah, 9
      ja    @o
   @t:mov   word ptr [Q+0],0003h
      mov   word ptr [Q+2],002Eh {'.'}
      jmp   @e
   @o:add   si,10
   @1:mov   al, [es:si]
      inc   si
      cmp   al, ' '
      je    @S
      mov   ds:[di], al
      inc   di
   @S:dec   ah
      jnz   @1
      mov   byte ptr ds:[di],0
      sub   di, offset Q
      cmp   di,3
      jl    @t
      mov   word ptr Q, di
   @e:
    end;

   if Pos(LowStrg(Q), Executables) <> 0 then GetFileType := ttExec else
    if (Pos(Q, Archives) <> 0) or
          (((Q[2] = 'a') or (Q[2] = 'r'))
            and (Q[3] >= '0') and (Q[4] >= '0')
            and (Q[3] <= '9') and (Q[4] <= '9')) then GetFileType := ttArc
    else if Pos(LowStrg(Q), CustomMask1) <> 0 then GetFileType := ttCust1
    else if Pos(LowStrg(Q), CustomMask2) <> 0 then GetFileType := ttCust2
    else if Pos(LowStrg(Q), CustomMask3) <> 0 then GetFileType := ttCust3
    else if Pos(LowStrg(Q), CustomMask4) <> 0 then GetFileType := ttCust4
    else if Pos(LowStrg(Q), CustomMask5) <> 0 then GetFileType := ttCust5
  end;
end;

        {-DataCompBoy-}
function NewFileRec;
  var PR: PFileRec;
      T:  TFileRec;
      D: DateTime;
      I: Integer;
begin
  if Name[1] <> '.' then
    begin
      I := PosChar('.', Name);
      T.Name := AddSpace(Name, 12);
      if I > 0 then Insert(Strg(' ', 9 - I), T.Name, I);
    end else T.Name := '..          ';
  T.Name[9] := ' ';
  if Attr and Directory <> 0 then UpStr(T.Name) else LowStr(T.Name);
  if Attr and Hidden <> 0 then T.Name[9] := '▒';
  if Attr and SysFile <> 0 then begin T.Name[9] := '▓'; T.Name[1] := Upcase(T.Name[1]); end;
  T.LFN := AddLFN(GetName(LFN));
  T.Size := Size; T.PSize := Size;
  if Date = 0 then
  begin
    T.Yr     := 1980;
    T.Month  := 1;
    T.Day    := 1;
    T.Hour   := 1;
    T.Minute := 1;
    T.Second := 1;
  end else
  begin
    UnpackTime(Date, D);
    T.Yr := D.Year;
    T.Month := D.Month;
    T.Day := D.Day;
    T.Hour := D.Hour;
    T.Minute := D.Min;
    T.Second := D.Sec;
  end;
  T.Attr := Attr and $7FFF;
  T.Selected := Off;
  T.DIZ := nil;
  T.TType := GetFileType(T.Name, T.Attr);
  T.Owner := AOwner;
  T.OwnerDisposible := false;
  New(PR); PR^ := T;
  NewFileRec := PR;
end;
        {-DataCompBoy-}

function PackedDate(P: PFileRec {DataCompBoy} ): LongInt;
var
  DT: DateTime;
  L: LongInt;
begin
  with P^ do
  begin
    DT.Year   := Yr;
    DT.Month  := Month;
    DT.Day    := Day;
    DT.Hour   := Hour;
    DT.Min    := Minute;
    DT.Sec    := Second;
  end;
  PackTime(DT, L);
  PackedDate := L;
end;

end.
