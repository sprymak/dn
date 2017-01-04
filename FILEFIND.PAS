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
unit FileFind;

interface
uses Objects, Views, Dialogs, Drivers, Menus, FileCopy, FilesCol, Eraser,
     Drives, HideView, DOS, Gauge, DiskInfo, FStorage, ObjType, LFNCol;

type
    TFindRec = record
     Mask: String; {DataCompBoy}
     What: String;
     Options: Word;
     Where: Word;
     AddChar: String[1];
    end;

    PListItem  = ^TListItem;
    TListItem = record
     Next: PListItem;
     Item: Pointer;
    end;

    TAdvanceSearchData = record
                    After: String[17]; {Inputline}
                    Before: String[17]; {Inputline}
                    Greater : String[12]; {Inputline}
                    Less : String[12]; {Inputline}
                    Attr : Word; {Checkbox}
                  end;

const
     CFindWindow = #126#127#128#129#130#131#132#133#134#135#136#137#138;
     CFileFinder = #6#7#8#9;
     CFindInfo   = #10#11#12#13;

     ffSeFnd    = $01; {Файл найден}                {-$VOL}
     ffSeD2Lng  = $02; {Переполнение строки поиска} {-$VOL}
     ffSeNotFnd = $FE; {Файл не найден}             {-$VOL}

     ffoAdvanced    = 1;
     ffoCaseSens    = 2;
     ffoRecursive   = 4;
     ffoWholeWords  = 8; {-$VIV 14.05.99}
     ffoAllCP       = 16; {-$VIV 14.05.99}

     FindRec: TFindRec = ( Mask : '*.*';
                           What : '';
                           Options : 4;
                           Where : 0;
                           AddChar : ''
                         );


    AdvanceSearchData : TAdvanceSearchData =
                    (After: '';
                     Before: '';
                     Greater: '';
                     Less: '';
                     Attr: 0
                     );

{    TempStored: Boolean = False;}


procedure FindFile;
function FindFiles(var Files: PDirStorage; var Directories: PCollection;var FindRec: TFindRec; PInfo: PWhileView): byte;
function ReadList(const AName: String; var DC: PSortedCollection; var FC: PDirStorage): Boolean; {DataCompBoy}
function OpenDirectory(const Dir: String): PDrive;
function ParseTime(S: String): LongInt;

type

  PFindDrive = ^TFindDrive;
  TFindDrive = object(TDrive)
   isDisposable: Boolean;
   Files: PDirStorage;
   Dirs: PSortedCollection;
   ListFile: PString;
   UpFile: PFileRec; {DataCompBoy}
   AllowPack: Boolean;
   constructor Load(var S: TStream);
   procedure Store(var S: TStream); virtual;
   procedure NewUpFile;
   constructor Init(const AName: String; ADirs: PCollection; AFiles: PDirStorage; num: byte); {DataCompBoy}
   constructor InitList(const AName: String); {DataCompBoy}
   procedure lChDir(ADir: String); virtual; {DataCompBoy}
   function GetDirectory(SortMode, PanelFlags: Integer; const FileMask: String;
                         var FreeSpace, TotalInfo: String ): PCollection; virtual;
   procedure CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
   function Disposable: Boolean; virtual;
   function GetRealName: String; virtual;
   procedure MakeDir; virtual;
   destructor Done; virtual;
   procedure GetFull(var B; P:PFileRec; C, SC:Word); virtual; {DataCompBoy}
   procedure GetEmpty(var B; SC: Word); virtual;
   procedure GetFreeSpace(var S: String); virtual;
   function CalcLength: Integer; virtual;
   procedure MakeTop(var S: String); virtual;
   function isUp: Boolean; virtual;
   procedure ChangeUp(var S: String); virtual;
   procedure ChangeRoot; virtual;
   procedure RereadDirectory(S: String); virtual;
   function GetFullFlags: Word; virtual;
   procedure GetDirInfo(var B: TDiskInfoRec); virtual;
   procedure GetParam(n: byte); virtual; {DataCompBoy}
  end;

  PTempDrive = ^TTempDrive;
  TTempDrive = object(TFindDrive)
   constructor Init(num: byte);
   constructor Load(var S: TStream);
   procedure   Store(var S: TStream); virtual;
   procedure   CopyFilesInto(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
   function    GetRealName: String; virtual;
   procedure   CopyFiles(AFiles: PCollection; Own: PView; MoveMode: Boolean); virtual;
   procedure   EraseFiles(AFiles: PCollection); virtual;
   procedure   GetDirInfo(var B: TDiskInfoRec); virtual;
   procedure   GetParam(n: byte); virtual; {DataCompBoy}
   destructor  Done; virtual;
  end;

  procedure CopyToTempDrive(AFiles: PCollection; Own: PView);

const
      RFindDrive: TStreamRec = (
       ObjType: otFindDrive;
       VmtLink: Ofs(TypeOf(TFindDrive)^);
       Load: @TFindDrive.Load;
       Store: @TFindDrive.Store);

      RTempDrive: TStreamRec = (
       ObjType: otTempDrive;
       VmtLink: Ofs(TypeOf(TTempDrive)^);
       Load: @TTempDrive.Load;
       Store: @TTempDrive.Store);


implementation
uses LFN{DataCompBoy}, DNApp, advance, advance1, advance2, advance3, Startup,
     Memory, Messages, HistList, Commands, RStrings, FlPanel, FViewer, MicroEd,
     Tree, xTime, DnUtil; {-$VIV}


const
  LowMemSize = $6000;  {Local setting}

var  FAdr   : Word;
     UsM    : Word;
     Rd,Ofs : Word;

function ESC_Pressed: Boolean; var E: TEvent;
begin
  Application^.Idle;
  GetKeyEvent(E); ESC_Pressed := (E.What = evKeyDown) and (E.KeyCode = kbEsc)
end;

function ReadingListMsg: PView; begin ReadingListMsg := WriteMsg(GetString(dlReadingList)) end;

        {-DataCompBoy-}
procedure FindFile;
 var R: TRect;
     D: PDialog;
     P1, P2: PView;
     FN: String;
     Dr: String;
     Nm: String;
     Xt: String;
     PInfo: PWhileView;
     Files: PDirStorage;
     Directories: PCollection;
     C: Char;
     CancelSearch: Boolean;
     AMask: String;
     Event: TEvent;
     BB: byte; {-$VOL}
     II,I,J: Word;
begin
 FindRec.AddChar:='';
 II := ExecResource(dlgFileFind, FindRec);
 if II = cmCancel then Exit;
 ConfigModified := On;
 DelLeft(FindRec.Mask); DelRight(FindRec.Mask);
 if FindRec.Mask = '' then FindRec.Mask := x_x;
 if (Pos('*', FindRec.Mask)=0) and
    (Pos('.', FindRec.Mask)=0) and
    (Pos(';', FindRec.Mask)=0) and
    (Pos('?', FindRec.Mask)=0) then FindRec.AddChar := '*'
                               else FindRec.AddChar := '';
 AMask := FindRec.Mask+FindRec.AddChar;
 New(Files, Init);
 Directories := New(PStringCollection, Init(30,30));
 R.Assign(1, 1, 25, 10); Inc(SkyEnabled);
 New(PInfo, Init(R)); PInfo^.Options := PInfo^.Options or ofSelectable;
 PInfo^.Top := GetString(dlDBViewSearch)+Cut(FindRec.Mask, 50);
 PInfo^.Bottom := GetString(dlNoFilesFound);
 PInfo^.Write(1, GetString(dlDBViewSearchingIn));
 Desktop^.Insert(PInfo);
 if StartupData.Slice2 and osuReleaseFind = 0 then Application^.FullSpeed:=True;
 BB := FindFiles(Files, Directories, FindRec, PInfo);
 if StartupData.Slice2 and osuReleaseFind = 0 then Application^.FullSpeed:=False;
 Desktop^.Delete(PInfo); Dec(SkyEnabled);
 Dispose(PInfo, Done);
 if (BB and ffSeD2Lng) <> 0 then MessageBox( GetString(dlSE_Dir2Long), nil, mfWarning + mfOkButton);
 if (BB and ffSeNotFnd) = BB then MessageBox(^C+GetString(dlNoFilesFound), nil, mfError + mfOkButton);
end;
        {-DataCompBoy-}

function ParseTime(S: String): LongInt;
 label 1, 2;
 var DT: DateTime;
     S1,S2,S3: String[5];
     TM: String[10];
     I, J: Integer;
     M: LongInt;
begin
  ParseTime := 0;
  Dt.Sec := 0; Dt.Hour := 0; Dt.Min := 0;
  I := PosChar(' ', S);
  if I > 0 then
    begin
      Tm := DelSpaces(Copy(S, I+1, 10));
      if Tm <> '' then
        begin
          I := Pos(CountryInfo.TimeSep, Tm);
          if I = 0 then Exit;
          DT.Hour := StoI(Copy(Tm, 1, I-1));
          J := 0;
          2:
          case UpCase(Tm[Length(Tm)]) of
            'M': begin J := 12; Goto 2 end;
            'A': Dec(Tm[0]);
            'P': begin J := 12; Dec(Tm[0]); end;
            '0'..'9':;
            else Goto 1;
          end;
          Inc(DT.Hour, J);
          DT.Min := StoI(Copy(Tm, I+Length(CountryInfo.TimeSep), 10));
        end;
 1: Byte(S[0]) := PosChar(' ', S) - 1;
    end else Tm := '';
  I := Pos(CountryInfo.DateSep, S);
  if I = 0 then Exit;
  S1 := Copy(S, 1, I-1);
  Delete(S, 1, I+Length(CountryInfo.DateSep)-1);
  I := Pos(CountryInfo.DateSep, S);
  if I = 0 then Exit;
  S2 := Copy(S, 1, I-1);
  Delete(S, 1, I+Length(CountryInfo.DateSep)-1);
  case CountryInfo.DateFmt of
    0: begin
         DT.Year := Stoi(S); if DT.Year < 100 then Inc(DT.Year, 1900);
         DT.Day := Stoi(S2); if DT.Day > 31 then Exit;
         DT.Month := Stoi(S1); if DT.Month > 12 then Exit;
       end;
    1: begin
         DT.Year := Stoi(S); if DT.Year < 100 then Inc(DT.Year, 1900);
         DT.Day := Stoi(S1); if DT.Day > 31 then Exit;
         DT.Month := Stoi(S2); if DT.Month > 12 then Exit;
       end;
    2: begin
         DT.Year := Stoi(S1); if DT.Year < 100 then Inc(DT.Year, 1900);
         DT.Day := Stoi(S); if DT.Day > 31 then Exit;
         DT.Month := Stoi(S2); if DT.Month > 12 then Exit;
       end;
  end;
  PackTime(DT, M);
  ParseTime := M;
end;

        {-DataCompBoy-}
function FindFiles;
 var FN: String;
     Dr: String;
     Nm: String;
     Xt: String;
     C: Char;
     Drv: PFindDrive;
     Pnl: PView;
     CancelSearch: Boolean;
     DateAfter, DateBefore,
     SizeGreat, SizeLess: LongInt;
     MaskSearch: Boolean;
     Attr: Byte;
     Mask: String;
     T: TEventTimer;
     FF: string;
     TotalNum: Longint; {-$VIV}
     FFResult: byte; {-$VOL}

 procedure InitPanel;
 begin
  Pnl := Message(Desktop, evBroadcast, cmInsertDrive, Drv);
  if Pnl <> nil then PFilePanel(Pnl)^.ChangeLocked := On;
 end;

 procedure DispatchEvents;
  var Event: TEvent;
 begin
  if PInfo = nil then Exit;
  Application^.GetEvent(Event);
  if (Event.What = evCommand) and (Event.Command = cmCancel) and
     (Event.InfoPtr = PInfo^.But) then
      begin
       PInfo^.ClearEvent(Event);
       CancelSearch := MessageBox(GetString(dlQueryCancelSearch),nil,mfYesNoConfirm)=cmYes;
      end;
  if (Event.What <> evNothing) and not ((Event.What = evCommand) and (Event.Command = cmQuit))
     then Application^.HandleEvent(Event);
 end;

 procedure SearchData(Path: string);
  label Skip;
  var PDir: PString;
      SR: lSearchRec;
      P: PFileRec;
      I: Byte;
      D: DateTime;
      DirCol: PDirCol;

 function SearchF: Boolean;
   var S: TBUFStream;
 begin
   PInfo^.Write(2, Cut( Path+SR.FullName, 50 ));
   Inc(TotalNum); {-$VIV}
   PInfo^.Write(3, GetString(dlLookedFiles) + ItoS(TotalNum)); {-$VIV}
   S.Init(Path+SR.FullName, stOpenRead, $4000);
   SearchF := (S.Status = stOK) and
              ( FViewer.SearchFileStr(@S, FindRec.What, 0,
                                          FindRec.Options and ffoCaseSens <> 0, {-$VIV 14.05.99}
                                          Off,
                                          FindRec.Options and ffoWholeWords <> 0, Off,
                                          FindRec.Options and ffoAllCP <> 0) >= 0 ); {-$VIV 14.05.99}
   S.Done;
 end;

 begin
  if (length(Path) + length(Mask)) > (MaxPathLen) then {-$VOL} {DataCompBoy}
    begin FFResult := FFResult or ffSeD2Lng; Exit; end;
  if CancelSearch or not MemOK then Exit;
  New(DirCol,Init($10,$10));
  DirCol^.Insert(NewStr(Path));
  while DirCol^.Count>0 do
   begin
    if CancelSearch then break;
    Path:=PString(DirCol^.At(0))^;
    DirCol^.AtFree(0);
    PDir := nil;
    DosError := 0;
    if PInfo <> nil then PInfo^.Write(2, Cut(Path, 50));
    if (FindRec.What = '') then
    begin
      Inc(TotalNum); {-$VIV}
      PInfo^.Write(3, GetString(dlLookedDirs) + ItoS(TotalNum)); {-$VIV}
    end;

    lFindFirst(Path + Mask, $FF and not VolumeID, SR);

    While (DosError = 0) {and not LowMemory} and not CancelSearch and MemOK do
     begin
      if (SR.SR.Attr and VolumeID = 0) and (SR.SR.Name[1] <> '.') and
       (not Security or (Security and (SR.SR.Attr and Hidden = 0)))
       and (MaskSearch or InFilter(SR.FullName, FindRec.Mask+FindRec.AddChar))
       and ((FindRec.Options and ffoAdvanced = 0) or (SR.SR.Time >= DateAfter)
            and (SR.SR.Time <= DateBefore) and (SR.FullSize >= SizeGreat) and
            (SR.FullSize <= SizeLess) and ((Attr = 0) or (SR.SR.Attr and Attr <> 0)))
       and ((FindRec.What='') or (SR.SR.Attr and Directory <> 0) and (FindRec.What='')
            or (FindRec.What<>'') and SearchF)
           then
       begin
        if PDir = nil then PDir := NewStr(Path);
        P := NewFileRec(SR.FullName, SR.SR.Name, SR.FullSize, SR.SR.Time, SR.SR.Attr, PDir);
        if Pnl = nil then InitPanel;
        if Pnl <> nil then Message(Pnl, evCommand, cmInsertFile, P)
                      else DelFileRec(P);
        Files^.AddFile(PDir^+SR.FullName, PDir^+SR.SR.Name, SR.FullSize, SR.FullSize, SR.SR.Time, SR.SR.Attr);
        if PInfo <> nil then
        PInfo^.Bottom := ItoS(Files^.Files) + FF;
        if TimerExpired(T) then
          begin
            DispatchEvents;
            if PInfo <> nil then PInfo^.DrawView;
            NewTimer(T, 1);
          end;
       end;
      if TimerExpired(T) then
        begin
          DispatchEvents;
          NewTimer(T, 1);
        end;
      DosError := 0;
      lFindNext(SR);
     end;
    lFindClose(SR);

    if not MemOK then Drv^.NoMemory := True;

    if TimerExpired(T) then
    begin
      DispatchEvents;
      NewTimer(T, 1);
    end;
    if FindRec.Options and 4 <> 4 then goto Skip;
    DosError := 0;
    lFindFirst(Path + x_x, $FF and not VolumeID, SR);
    While (DosError = 0) and not LowMemory and not CancelSearch do
     begin
      if (SR.SR.Attr and VolumeID = 0) and (SR.SR.Name[1] <> '.') and
         (SR.SR.Attr and Directory <> 0) then DirCol^.Insert(NewStr(Path + SR.FullName + '\'));
      lFindNext(SR);
     end;
    lFindClose(SR);
Skip:
    if PDir <> nil then Directories^.Insert(PDir);
    DosError := 0;
    if PInfo <> nil then PInfo^.DrawView;
   end;
  Dispose(DirCol, done);
 end;

function SlowDrive(C: Char): Boolean;
begin
  case C of
     'C'..'Z' : SlowDrive := IsDriveCDROM(C);
    else SlowDrive := True;
  end;
end;

begin
 TotalNum := 0; {-$VIV}
 NewTimer(T, 0);
 FF := GetString(dlFilesFound);
 if FindRec.Options and ffoAdvanced <> 0 then
   begin
     DateAfter := ParseTime(AdvanceSearchData.After);
     DateBefore := ParseTime(AdvanceSearchData.Before);
     if DateBefore = 0 then DateBefore := $7FFFFFFF;
     SizeGreat := Stoi(AdvanceSearchData.Greater);
     SizeLess := Stoi(AdvanceSearchData.Less);
     if SizeLess = 0 then SizeLess := $7FFFFFFF;
     Attr := 0;
     if AdvanceSearchData.Attr and 1 <> 0 then Attr := Archive;
     if AdvanceSearchData.Attr and 2 <> 0 then Attr := Attr or SysFile;
     if AdvanceSearchData.Attr and 4 <> 0 then Attr := Attr or Hidden;
     if AdvanceSearchData.Attr and 8 <> 0 then Attr := Attr or ReadOnly;
   end;
 CancelSearch := Off; FFResult := ffSeFnd;
 SearchString.What := FindRec.What;
 SearchString.Opts := ((FindRec.Options and ffoCaseSens) shr 1) or
                      ((FindRec.Options and ffoWholeWords) shr 2) or
                      ((FindRec.Options and ffoAllCP) shr 2); {-$VIV 14.05.99}
 Microed.SearchData.Line := FindRec.What;
 Microed.SearchData.What := #0;
 Microed.SearchData.Options := SearchString.Opts;
 Microed.SearchData.Scope := 0;
 MaskSearch := Pos(';', FindRec.Mask+FindRec.AddChar) = 0;
 if MaskSearch then
   begin
     Mask := FindRec.Mask+FindRec.AddChar;
     if PosChar('.', FindRec.Mask) = 0 then Replace(' ', '*', Mask);
   end else Mask := x_x;
 New(Drv, Init('Find: ' + FindRec.Mask, Directories, Files, FreeByte));
 Drv^.isDisposable := Off;
 Pnl := nil;
 {FN := DelSpaces(FExpand(FindRec.Mask));
 lFSplit(FN, Dr, Nm, Xt);}
 lGetDir(0, FN);
 if FN[Length(FN)] <> '\' then FN := FN + '\';
 case FindRec.Where of
  1: SearchData(FN);
  2: for C := 'A' to 'Z' do
         if ValidDrive(C) then
           if (C = UpCase(FN[1])) or (not SlowDrive(C)) then SearchData(C + ':\');
  {3: SearchData(Dr)}
   else SearchData(Copy(FN, 1, 3));
 end;
 if not MemOK then Drv^.NoMemory := True;
 if Pnl <> nil then
  begin
    if Pnl <> nil then PFilePanel(Pnl)^.ChangeLocked := Off;
    Drv^.isDisposable := On;
    Dispose(PFilePanel(Pnl)^.Files, Done);
    PFilePanel(Pnl)^.Files := New(PFilesCollection, Init($100,$80));
    Application^.Redraw;
    Message(Pnl, evCommand, cmPanelReread, nil);
  end;
 if (Files^.Files = 0) or (Pnl = nil) then
  begin
   Dispose(Drv, Done);
   FFResult := FFResult and ffSeNotFnd; {-$VOL}
  end;
 FindFiles := FFResult;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function ReadList;
  var F: PTextReader;
      I: Integer;
      S: String;
      Dir: String;
      Nm: String;
      Xt: String;
      SR: lSearchRec;
      Dr: PString;
      P: PFileRec;
      D: DateTime;
      WW: PView;
begin
  ReadList := Off;

  F := New(PTextReader, Init(AName));
  if F = nil then Exit;


  FC := nil; DC := nil;
  WW := ReadingListMsg;
  while not F^.EOF and (IOResult = 0) and not Abort do
   begin
     UpdateWriteView(WW);
     S := F^.GetStr;
     DelLeft(S);
     I := Pos(':\', S);
     if I > 1 then Delete(S, 1, I-2);
     I := PosChar(' ',S);
     if I > 0 then S[0] := Char(I-1);
     UpStr(S);
     if S <> '' then
       begin
         ClrIO;
         lFindFirst(S, $3F xor VolumeID, SR);
         if (DOSError = 0) and not Abort then
          begin
            lFSplit(lFExpand(S), Dir, Nm, Xt);
            if DC = nil then DC := New(PStringCollection, Init(50,50));
            I := DC^.IndexOf(@Dir);
            if I < 0 then
              begin
                if Dir[Length(Dir)] <> '\' then AddStr(Dir, '\');
                Dr := NewStr(Dir);
                DC^.Insert(Dr);
              end else Dr := DC^.At(I);
          end;
         while (DOSError = 0) and not Abort do
          begin
            if SR.SR.Name[1] <> '.' then
             begin
               if FC = nil then New(FC, Init);
               FC^.AddFile(Dr^+SR.FullName, Dr^+SR.SR.Name, SR.FullSize, SR.FullSize, SR.SR.Time, SR.SR.Attr);
             end;
            ClrIO;
            lFindNext(SR);
          end;
       end;
   end;

  Dispose(F, Done);
  WW^.Free;
  if (FC = nil) or (FC^.Files = 0) or (DC = nil) or (DC^.Count = 0) then
    begin
      if FC <> nil then Dispose(FC, Done);
      if DC <> nil then Dispose(DC, Done);
      MessageBox(^C+GetString(dlNoFilesFound), nil, mfError+mfOKButton);
      Exit
    end;
  ReadList := On;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
function OpenDirectory(const Dir: String): PDrive;
  var I: Integer;
      PI: PView;
      PDrv: PDrive;
      Dirs: PStringCollection;
      Files: PDirStorage;
      P: PString;
      tmr: TEventTimer;
      MemReq: LongInt;

  procedure AddDirectory(S: String);
  begin
     if S[Length(S)] <> '\' then AddStr(S, '\');
     S[1]:=LowCase(S[1]);
     Dirs^.Insert(NewStr(S));
  end;

  procedure ReadDir(Dr: PString);
    var SR: lSearchRec;
        P: PFileRec;
        D: DateTime;
  begin
     ClrIO;
     lFindFirst(Dr^+x_x, $3F xor VolumeID, SR);
     while not Abort and (DOSError = 0) do
       begin
         if (SR.SR.Attr and Hidden = 0) or (not Security) then
           if SR.SR.Attr and Directory = 0 then
             begin
               Files^.AddFile(Dr^+SR.FullName, Dr^+SR.SR.Name, SR.FullSize, SR.FullSize, SR.SR.Time, SR.SR.Attr);
               Inc(MemReq, SizeOf(TFileRec));
               Inc(MemReq, Length(Dr^+SR.FullName)+1);
               {if Files^.Count = 2000 then
                begin
                  MessageBox(GetString(dlFindCantKeep), nil, mfError + mfOKButton);
                  Exit;
                end;}
             end else
             if SR.SR.Name[1] <> '.' then AddDirectory(Dr^+SR.FullName);
         lFindNext(SR);
       end;
    lFindClose(SR);
  end;

begin
  NewTimer(Tmr,0);
  Dirs := New(PStringCollection, Init($80,$40));

  PI := ReadingListMsg;
  New(Files, Init);

  AddDirectory(FExpand(Dir));
  I := Dirs^.Count-1;
  MemReq := LowMemSize;
  while (I >= 0) and (not Abort) and (System.MemAvail > MemReq) do
    begin
      I := Dirs^.Count-1;
      {if Files^.Count >= 2000 then Break;}
      while (I >= 0) and (not Abort) and (System.MemAvail > MemReq) do
        begin
          UpdateWriteView(PI);
          P := Dirs^.At(I);
          if (P^[1] >= 'a') and (P^[1] <= 'z') then
            begin
              P^[1]:=UpCase(P^[1]);
              Dirs^.AtDelete(I);
              Dirs^.Insert(P);
              ReadDir(P);
              if TimerExpired(Tmr) then
              begin
                NewTimer(Tmr, 1);
                if ESC_Pressed then Abort := True;
              end;
              Break;
            end;
          Dec(I);
        end;
    end;
  PI^.Free;
  PDrv := New(PFindDrive, Init(GetString( dlBranch )+Dir, Dirs, Files, FreeByte));
  PDrv^.NoMemory := System.MemAvail <= MemReq;
  OpenDirectory := PDrv;
end;
        {-DataCompBoy-}

{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ TFindDrive }
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}

        {-DataCompBoy-}
procedure TFindDrive.GetParam;
begin
 Param:=n;
 with ColumnsDefaultsFind do
  begin
   Flags := Params[n].Param;
   LFNLen:=StoI(Params[n].LFNLen);
   EXTLen:=StoI(Params[n].EXTLen);
   case FileCase of
    0: FilFLP := 0;
    1: FilFLP := flnLowCase;
    2: FilFLP := flnCapitalCase;
    3: FilFLP := flnUpCase;
    4: FilFLP := 65535;
   end;
   case DirsCase of
    0: DirFLP := 0;
    1: DirFLP := flnLowCase;
    2: DirFLP := flnCapitalCase;
    3: DirFLP := flnUpCase;
    4: DirFLP := 65535;
   end;
  end;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
constructor TFindDrive.Init;
 var S: PString;
     SS: String;
     I: Integer;
begin
 TObject.Init;
 if num<1 then GetParam(1)
          else GetParam(num);
 innum:=num;
 AllowPack := True;
 ListFile := nil;
 isDisposable := On;
 DriveType := dtFind;
 CurDir := AName;
 Dirs := PSortedCollection(ADirs);
 Files := AFiles;
 lGetDir(0, SS); ClrIO;
 S := NewStr(SS);
 I := Dirs^.IndexOf(S);
 if I >= 0 then begin DisposeStr(S); S := Dirs^.At(I); end
           else Dirs^.Insert(S);
 NewUpFile;
 UpFile^.Owner := S;
end;
        {-DataCompBoy-}

procedure TFindDrive.NewUpFile;
begin
  UpFile := NewFileRec('..','..',0,0,Directory,nil); {DataCompBoy}
end;

constructor TFindDrive.Load(var S: TStream);
   {-DataCompBoy-}
 var I, J, K: Integer;
   {-DataCompBoy-}
begin
 inherited Load(S);
 AllowPack := True;
 isDisposable := On;
 S.Read(DriveType,SizeOf(DriveType));
 Dirs := PSortedCollection(S.Get);
 S.Read(I, SizeOf(I)); if I < 0 then I := 0;
 if Dirs = nil then Fail;
 Files := PDirStorage(S.Get);
 ListFile := S.ReadStr;
 NewUpFile;
 UpFile^.Owner := Dirs^.At(I);
end;

procedure TFindDrive.Store(var S: TStream);
  {-DataCompBoy-}
 var I, J: Integer;
  {-DataCompBoy-}
begin
  inherited Store(S);
  S.Write(DriveType,SizeOf(DriveType));
  S.Put(Dirs);
  I := Dirs^.IndexOf(UpFile^.Owner);
  S.Write(I, SizeOf(I));
  S.Put(Files);
  S.WriteStr(ListFile);
end;

destructor TFindDrive.Done;
begin
 if Files <> nil then Dispose(Files, Done);
 if Dirs <> nil then Dispose(Dirs, Done);
 if UpFile <> nil then DelFileRec(UpFile);
 DisposeStr(ListFile);
 inherited Done;
end;

procedure TFindDrive.MakeDir;
begin
end;

procedure TFindDrive.lChDir;
begin
end;

        {-DataCompBoy-}
function TFindDrive.GetDirectory;
 label LLL;
 var AFiles: PFilesCollection;
     SR: lSearchRec;
     P: PFileRec;
     D: DateTime;
     I: Integer;
     DrNm: String;
     AllFiles: Boolean;
     FreeSpc, TotalLen: TSize;
     PD: PString;
     OW: Pointer;
     S: String;
     OOM: Boolean;

function CMatches: Boolean;
begin
  OOM := OOM or (MaxAvail < (AFiles^.Count+$200)*4);
  CMatches := not OOM;
end;

begin

LLL:

 OOM := False;
 DOSError := 0;
 Abort := Off; AllFiles := FileMask = x_x; ClrIO; FreeSpc := 0;
 FreeSpace := ''; TotalInfo := ''; TotalLen := 0;

 Files^.ResetPointer('');

 AFiles := New(PFilesCollection, Init($10, $10));
 PFilesCollection(AFiles)^.Owner := Owner;
 AFiles^.Duplicates := True;

 PFilesCollection(AFiles)^.SortMode := SortMode; S := ''; PD := nil;
 ClrIO;
 While not Files^.Last and Files^.GetNextFile and MemOK and CMatches do
  begin

    if (S = Files^.LastDir) then OW := PD else
      if Dirs^.Search(@Files^.LastDir, I) then
      begin
        PD := Dirs^.At(I);
        OW := PD;
        S := PD^
      end else
      begin
        OW := Dirs^.At(0);
      end;


   with Files^.CurFile do P := NewFileRec(LFN, Name, Size, Date, Attr, OW);

   if AllFiles or (P^.Attr and Directory <> 0) or InFilter(GetLFN(P^.LFN), FileMask)
    then with AFiles^ do AtInsert(Count, P)
    else begin DelFileRec(P); Continue end;
   TotalLen := TotalLen + P^.Size;
   if P^.Attr and Directory <> 0 then
    begin
       P^.Attr := P^.Attr and $7FFF;
       P^.Size := 0;
       if PanelFlags and fmiDirLen <> 0 then
        begin
          if SR.SR.Name[1] <> '.' then
             P^.Size := GetDirLen(MakeNormName(P^.Owner^, GetLFN(P^.LFN)));
          if Abort then begin PanelFlags := PanelFlags and (not fmiDirLen); Abort := Off; end;
        end;
    end;
  end;

  if AFiles^.Count > 0 then
  begin
    if (OOM) or (not MemOK) then
    begin
      NoMemory := True;
      if AllowPack then
      begin
        Files^.Truncate(Max((AFiles^.Count-100), 0));
        Dispose(AFiles, Done);
        goto LLL;
      end;
    end;
    AFiles^.QSort;
 end else
 begin
   if (OOM) or (not MemOK) then NoMemory := True;
 end;

 if DriveType <> dtTemp then AFiles^.AtInsert(0, CopyFileRec(UpFile));

 TotalInfo := CalcTotalInfo(AFiles^.Count, AFiles^.Count, TotalLen);

 GetDirectory := AFiles;
end;
        {-DataCompBoy-}

function TFindDrive.CalcLength;
begin
 if Flags and psShowLongName <> 0 then
  CalcLength := LFNLen + 1 +
                (10 + CountryInfo.TimeFmt) * Byte(Flags and psShowSize <> 0) +
                 9 * Byte(Flags and psShowDate <> 0) +
                (7 - CountryInfo.TimeFmt) * Byte(Flags and psShowTime <> 0) +
                80 * Byte(Flags and psShowDir <> 0)
 else
  CalcLength := 13 +
                (10 + CountryInfo.TimeFmt) * Byte(Flags and psShowSize <> 0) +
                 9 * Byte(Flags and psShowDate <> 0) +
                (7 - CountryInfo.TimeFmt) * Byte(Flags and psShowTime <> 0) +
                80 * Byte(Flags and psShowDir <> 0);
end;

procedure TFindDrive.MakeTop;
 var Q: String;
begin
 if Flags and psShowLongName <> 0 then S := CenterStr(GetString(dlTopLFN), LFNLen)+GetString(dlTopSplit)
                                  else S:=GetString(dlTopName);
 if Flags and psShowSize     <> 0 then S := S + Copy(GetString(dlTopSize),2-CountryInfo.TimeFmt, 255);
 if Flags and psShowDate     <> 0 then S := S + GetString(dlTopDate);
 if Flags and psShowTime     <> 0 then S := S + Copy(' '+GetString(dlTopTime), 1+CountryInfo.TimeFmt, 255);
 if Flags and psShowDir      <> 0 then
  begin
   Q := GetString(dlTopPath);
   S := S + Q + Strg(' ', 80-Length(Q)) + '~│~';
  end;
end;

procedure TFindDrive.GetFull;
 var X: Byte;
begin
  inherited GetFull(B, P, C, SC);
  if Flags and psShowLongName <> 0 then X:=LFNLen+1 else X:=13;
  if Flags and psShowSize <> 0 then X := X + 11;
  if Flags and psShowDate <> 0 then Inc(X, 9);
  if Flags and psShowTime <> 0 then Inc(X, 6);
  if Flags and psShowDir <> 0 then
   begin
    MoveStr(TWordArray(B)[X], P^.Owner^, C);
    Inc(X, 80);
    TWordArray(B)[X-1]:=SC;
   end;
end;

procedure TFindDrive.GetEmpty;
 var
  X: Byte;
begin
  inherited GetEmpty(B, SC);
  if Flags and psShowLongName <> 0 then X:=LFNLen+1 else X:=13;
  if Flags and psShowSize <> 0 then X := X + 11;
  if Flags and psShowDate <> 0 then Inc(X, 9);
  if Flags and psShowTime <> 0 then Inc(X, 6);
  if Flags and psShowDir <> 0 then begin Inc(X, 80); TWordArray(B)[X-1]:=SC; end;
  TWordArray(B)[X-1]:=SC;
end;

procedure TFindDrive.ChangeUp;
 var P: PDrive;
begin
  if ListFile = nil then S := '' else S := GetName(ListFile^);
  if Owner = nil then Exit;
  if Prev = nil then
   begin
     New(Prev, Init(0, Owner, innum));
     if Prev = nil then Exit;
     GlobalMessage(evCommand, cmRereadInfo, nil);
   end;
  PDrive(PFilePanel(Owner)^.Drive) := PDrive(Prev);
  Prev^.lChDir(Prev^.CurDir);
  if (Prev^.DriveType = dtDisk) and
     (PView(Owner)^.GetState(sfSelected+sfActive)) then
      ActivePanel := Owner;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  Prev := nil;
  Dispose(PDrive(@Self), Done);
end;

procedure TFindDrive.ChangeRoot;
  var P: PDrive;
begin
  if Prev <> nil then
    begin
      PDrive(PFilePanel(Owner)^.Drive) := PDrive(Prev);
      Prev^.ChangeRoot;
      GlobalMessage(evCommand, cmRereadInfo, nil);
      Prev := nil;
      Dispose(PDrive(@Self), Done);
      Exit;
    end;
  New(Prev, Init(0, Owner, innum));
  if Prev = nil then Exit;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  {Prev^.Owner := Owner;}
  {Prev^.Flags := PFilePanel(Owner)^.Drive^.Flags;}
  PDrive(PFilePanel(Owner)^.Drive) := PDrive(Prev);
  Prev^.ChangeRoot;
  if (PView(Owner)^.GetState(sfSelected+sfActive)) then
      ActivePanel := Owner;
  GlobalMessage(evCommand, cmRereadInfo, nil);
  Prev := nil;

end;

function TFindDrive.isUp;
begin
 isUp := True;
end;

function TFindDrive.Disposable;
begin
 Disposable := isDisposable;
end;

procedure TFindDrive.RereadDirectory;
var PV: PView;
begin
{ if (Prev <> nil) or (Prev^.Prev = nil) then Exit;}
 PV := ReadingListMsg;
 ForceWriteShow(PV);
 if Prev <> nil then Prev^.RereadDirectory(S);
 Files^.DosReread;
 Dispose(PV, Done);
end;


function TFindDrive.GetRealName;
begin
 GetRealName := '';
end;

procedure NewTemp;
begin
 if TempFiles = nil then New(TempFiles, Init);
end;

        {-DataCompBoy-}
procedure CopyToTempDrive;
 var I: Integer;
     PS: PString;
     PF: PFileRec;
     Info: PView;

 procedure AddRec(P: PFileRec); far;
   var Dr: String;
       Nmm: String;
       DT: DateTime;
 begin
   UpdateWriteView(Info);
   Dr := P^.Owner^; if Dr[Length(Dr)] <> '\' then AddStr(Dr, '\');
   TempFiles^.ResetPointer(''); Nmm := GetLFN(P^.LFN);
   While not TempFiles^.Last and TempFiles^.GetNextFile do
    with TempFiles^ do
      if (CurFile.Name = Nmm) and (LastDir = Dr) then Exit;
   TempFiles^.AddFile(Dr+Nmm, Dr+MakeFileName(P^.Name), P^.Size, P^.Size, PackedDate(P), P^.Attr);
   if TempDirs = nil then TempDirs := New(PStringCollection, Init(10, 10));
   if TempDirs^.IndexOf(@Dr) < 0 then TempDirs^.Insert(NewStr(Dr));
 end;

begin
 if TempDirs = nil then TempDirs := New(PStringCollection, Init(10, 10));
 if TempFiles = nil then NewTemp;
 Info := WriteMsg(GetString(dlPleaseStandBy));
 AFiles^.ForEach(@AddRec);
 Dispose(Info, Done);
 Drives.RereadDirectory(cTEMP_);
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure TTempDrive.GetParam;
begin
 Param:=n;
 with ColumnsDefaultsTemp do
  begin
   Flags := Params[n].Param;
   LFNLen:=StoI(Params[n].LFNLen);
   EXTLen:=StoI(Params[n].EXTLen);
   case FileCase of
    0: FilFLP := 0;
    1: FilFLP := flnLowCase;
    2: FilFLP := flnCapitalCase;
    3: FilFLP := flnUpCase;
    4: FilFLP := 65535;
   end;
   case DirsCase of
    0: DirFLP := 0;
    1: DirFLP := flnLowCase;
    2: DirFLP := flnCapitalCase;
    3: DirFLP := flnUpCase;
    4: DirFLP := 65535;
   end;
  end;
end;
        {-DataCompBoy-}

constructor TTempDrive.Init;
  var S: PString;
      I: Integer;
begin
 TObject.Init;
 if num<1 then GetParam(1)
          else GetParam(num);
 innum:=num;
 if TempDirs = nil then TempDirs := New(PStringCollection, Init(10, 10));
 if TempFiles = nil then NewTemp;
 isDisposable := On;
 Files := TempFiles;
 Dirs := TempDirs;
 CurDir := cTEMP_;
 DriveType := dtTemp;
 ListFile := nil;
 System.GetDir(0, FreeStr); ClrIO;
 S := NewStr(FreeStr);
 I := Dirs^.IndexOf(S);
 if I >= 0 then begin DisposeStr(S); S := Dirs^.At(I); end
           else Dirs^.Insert(S);
 NewUpFile;
 UpFile^.Owner := S; {DataCompBoy}
end;

constructor TTempDrive.Load(var S: TStream);
begin
  TDrive.Load(S);
  S.Read(DriveType,SizeOf(DriveType));
  Dirs := PSortedCollection(S.Get);
  Files := PDirStorage(S.Get);
  TempFiles := Files;
  TempDirs := Dirs;
  NewUpFile;
  UpFile^.Owner := S.ReadStr;
end;

procedure TTempDrive.Store;
begin
  TDrive.Store(S);
  S.Write(DriveType,SizeOf(DriveType));
  S.Put(Dirs);
  S.Put(Files);
  S.WriteStr(UpFile^.Owner)
end;

procedure TTempDrive.CopyFilesInto;
begin
 CopyToTempDrive(AFiles, Own);
end;

procedure TTempDrive.EraseFiles;
 var I: Integer;

        {-DataCompBoy-}
 procedure Erase(P: PFileRec); far;
 begin
   Files^.DeleteFile(P);
 end;
        {-DataCompBoy-}

begin
 AFiles^.ForEach(@Erase);
 Drives.RereadDirectory(cTEMP_);
end;

function TTempDrive.GetRealName;
begin
 GetRealName := cTEMP_;
end;

destructor TTempDrive.Done;
begin
 TDrive.Done;
end;

procedure TFindDrive.GetFreeSpace;
begin
  S := '';
end;

function TFindDrive.GetFullFlags;
begin
 GetFullFlags := psShowSize + psShowDate + psShowTime + psShowDir;
end;

        {-DataCompBoy-}
constructor TFindDrive.InitList;
  var FC: PSortedCollection;
      DC: PDirStorage;
begin
  if not ReadList(AName, FC, DC) then Fail;
  Init(AName, PCollection(FC), DC, FreeByte);
  ListFile := NewStr(AName);
end;
        {-DataCompBoy-}

procedure TTempDrive.CopyFiles;
begin
 FileCopy.CopyFiles(AFiles, Own, MoveMode, 1);
end;

procedure TFindDrive.CopyFilesInto;
begin
end;

procedure TFindDrive.GetDirInfo;
  var Fl: Integer;
      Sz: TSize;
      S1,S2: String[40];

        {-DataCompBoy-}
  procedure DoCount(P: PFileRec); far;
  begin
    if (P <> nil) and (P^.Attr and Directory = 0) then
      begin
        Inc(Fl);
        Sz:=Sz+P^.Size;
      end;
  end;
        {-DataCompBoy-}

begin
  B.Title := NewStr( GetString( dlDIFileFind ));
  B.Dir := NewStr(GetString( dlDIFFMask )+Copy(CurDir, PosChar(':', CurDir)+1, 255)); {DataCompBoy}
  Fl := 0; Sz := 0;
  {!!!if Files <> nil then Files^.ForEach(@DoCount);}

  if Fl = 0 then B.Files := NewStr(GetString(dlDINoFiles))
   else
    begin
     if Fl = 1 then S1 := GetString(dlDIFile) else S1 := GetString(dlDIFiles);
     if Sz = 1 then S2 := GetString(dlDIByte) else S2 := GetString(dlDIBytes);
     B.Files := NewStr('~'+FStr(Fl)+'~ '+S1+GetString(dlDIWith)+'~'+FStr(Sz)+'~ '+S2);
    end;

end;

procedure TTempDrive.GetDirInfo;
  var Fl: Integer;
      Sz: TSize;
      S1,S2: String[40];

        {-DataCompBoy-}
  procedure DoCount(P: PFileRec); far;
  begin
    if (P <> nil) and (P^.Attr and Directory = 0) then
      begin
        Inc(Fl);
        Sz:=Sz+P^.Size;
      end;
  end;
        {-DataCompBoy-}

begin
  B.Title := NewStr( GetString( dlDITemporary ));
  B.Dir := NewStr(cTEMP_);
  Fl := 0; Sz := 0;
  {!!!
  if Files <> nil then Files^.ForEach(@DoCount);
  }
  if Fl = 0 then B.Files := NewStr(GetString(dlDINoFiles))
   else
    begin
     if Fl = 1 then S1 := GetString(dlDIFile) else S1 := GetString(dlDIFiles);
     if Sz = 1 then S2 := GetString(dlDIByte) else S2 := GetString(dlDIBytes);
     B.Files := NewStr('~'+FStr(Fl)+'~ '+S1+GetString(dlDIWith)+'~'+FStr(Sz)+'~ '+S2);
    end;
end;


end.
