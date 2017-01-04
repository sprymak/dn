{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//  Version history:
//
//  1.6.RC1
//  dn2510-resource_and_help_fixes_20020516.patch
//
//  2.7.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//  dn31220-Kernel(i)-DN_changes_language_immediately.patch
//  dn40412-BackSearch(f)-back_search_fix.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit Fixer;

interface

uses Objects, Views, Drivers, Dos, Memory, Menus, Messages, HideView, advance,
     advance1, advance2, advance3, Dialogs, Commands, DNApp, DNStdDlg, FViewer,
     RStrings, SWE, Tree, FilesCol, Validate, UniWin, DNHelp, collect
     {$IFNDEF NONBP}, DiskTool{$ENDIF}
     ;

type
    PDiskStream = ^TDiskStream;
    TDiskStream = object(TStream)
     Drv: PDiskDriver;
     FileSize, FilePos: LongInt;
     Start: LongInt;
     Num: LongInt;
     Mode: Byte;
     BlockSize: Word;
     constructor Init(ADrive: Byte);
     function GetPos: Longint; virtual;
     function GetSize: Longint; virtual;
     procedure Read(var Buf; Count: Word); virtual;
     procedure Seek(Pos: Longint); virtual;
     procedure Write(const Buf; Count: Word); virtual;
     function GetPosInfo(Pos: LongInt): String;
     procedure SetMode(AMode: Byte; AStart, ANumber: LongInt);
     destructor Done; virtual;
    end;

    PFixerInfo = ^TFixerInfo;
    TFixerInfo = object(TViewInfo)
     procedure Draw; virtual;
    end;

    PFixerWindow = ^TFixerWindow;
    TFixerWindow = object(TUniWindow)
     MenuBar: PView;
     constructor Init;
     function GetPalette: PPalette; virtual;
     procedure ChangeLanguage; virtual;  {John_SW  23-12-2003}
    end;

    PDiskFixer = ^TDiskFixer;
    TDiskFixer = object(TFileViewer)
     DrawDisabled: Boolean;
     XPos, LastChars: LongInt;
     XXPos: Boolean;
     LastR: TRect;
     BlockStart, BlockEnd: LongInt;
     constructor Init(R: TRect);
     procedure MakeLines; virtual;
     procedure Draw; virtual;
     function GetPalette: PPalette; virtual;
     procedure HandleEvent(var Event: TEvent); virtual;
     procedure CountDown(ANumber: Integer); virtual;
    end;

    PCharBox = ^TCharBox;
    TCharBox = object(TListBox)
     function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    end;

implementation

const
     Clipboard: Pointer = nil;
     ClipSize: Word = 0;

     CDiskFixer = #13#14#15#16;
     CFixerWindow = #153#147#148#149#150#151#152#154#155#156#157+
                    #158#159#160#161#162;

procedure TFixerInfo.Draw;
 var I, J: LongInt;
     B: TDrawBuffer;
     S: String;
begin
 if Viewer = nil then begin inherited Draw; Exit; end;
 With PDiskFixer(Viewer)^ do
  begin
   I := FilePos + BufPos + XPos;
   if BlockStart <> BlockEnd then
    begin
     if BlockStart < BlockEnd then S := Hex8(BlockStart) + '-' + Hex8(BlockEnd)
                              else S := Hex8(BlockEnd) + '-' + Hex8(BlockStart);
     S := ' [' + S + ':' + Hex4(Abs(BlockStart - BlockEnd) + 1) + ']';
    end else S := '';
  end;
 S := ' ' + Hex8(I) + S + ' ';
 if Byte(S[0]) <> Size.X then GrowTo(Byte(S[0]), 1);
 MoveStr(B, S, Owner^.GetColor(8));
 WriteLine(0, 0, Size.X, Size.Y, B);
end;

function TCharBox.GetText;
begin
 GetText := Hex2(Item) + ' ' + Char(Item) + ' '
            + SStr(Item, 3, '0')
end;

constructor TDiskStream.Init;
 var S: String;
     CL: Word;
begin
 inherited Init; Abort := False;
 GetDir(ADrive, S); if Abort then Exit;
 New(Drv, Init(Byte(S[1])-64, True));
 if Drv = nil then Fail;
 CL := Drv^.GetPath(S);
 if Cl > 0 then SetMode(2,Cl,430) else
  SetMode(0,Drv^.FATCopies*Drv^.SectPerFAT+Drv^.Reserved, (Drv^.RootEntries*32) div Drv^.SectLen);
end;

destructor TDiskStream.Done;
begin
 if Drv <> nil then Dispose(Drv,Done); Drv:=nil;
 inherited Done;
end;

function TDiskStream.GetPos: Longint;
begin
 GetPos := FilePos;
end;

function TDiskStream.GetSize: Longint;
begin
 GetSize := FileSize
end;

procedure TDiskStream.Read(var Buf; Count: Word);
 var Cl, Pos, M, T, I, J,
     L: LongInt;
     Buffer: PByteArray;
begin
 if (Count = 0) or (FilePos >= FileSize) then Exit; Abort := False;
 if FilePos + LongInt(Count) >= FileSize then Count := FileSize - FilePos;
 Pos := 0; L := Count; T := BlockSize;
 Buffer := MemAlloc(BlockSize);
 if Buffer = nil then Exit;
 case Mode of
  1: begin
      Cl := FilePos div (BlockSize); Inc(Cl, Start);
      M :=  LongInt(FilePos) mod LongInt(BlockSize);
      repeat
       if L + M < BlockSize then T := M + L else T := BlockSize;
       Drv^.ClusterRead(Buffer^, Cl);
       Move(Buffer^[M], TByteArray(Buf)[Pos], T - M);
       Inc(Cl);
       Inc(Pos, T - M);
       Dec(L, T - M);
       M := 0;
      until L <= 0;
    end;
  2: begin
      Cl := FilePos div (BlockSize);
      J := Start;
      for I := 1 to Cl do J := Drv^.GetFAT(J);
      if J < 2 then J := 2; Cl := J;
      M :=  LongInt(FilePos) mod LongInt(BlockSize);
      repeat
       if L + M < BlockSize then T := M + L else T := BlockSize;
       Drv^.ClusterRead(Buffer^, Cl);
       Move(Buffer^[M], TByteArray(Buf)[Pos], T - M);
       Cl := Drv^.GetFAT(Cl);
       if Cl < 2 then Cl := 2;
       Inc(Pos, T - M);
       Dec(L, T - M);
       M := 0;
      until (L <= 0) or (Cl >= Drv^.EOFMark);
    end;
  else
   begin
      Cl := FilePos div (BlockSize); Inc(Cl, Start);
      M :=  LongInt(FilePos) mod LongInt(BlockSize);
      repeat
       if L + M < BlockSize then T := M + L else T := BlockSize;
       SectorRead(Drv^.Drive, Buffer, Cl, 1);
       Move(Buffer^[M], TByteArray(Buf)[Pos], T - M);
       Inc(Cl);
       Inc(Pos, T - M);
       Dec(L, T - M);
       M := 0;
      until (L <= 0) or Abort;
   end;
 end;
 FreeMem(Buffer, BlockSize);
 Inc(FilePos, Count);
end;

procedure TDiskStream.Write(const Buf; Count: Word);
 var Cl, Pos, M, T, I, J,
     L: LongInt;
     Buffer: PByteArray;
begin
 if (Count = 0) or (FilePos >= FileSize) then Exit; Abort := False;
 if FilePos + LongInt(Count) >= FileSize then Count := FileSize - FilePos;
 Pos := 0; L := Count; T := BlockSize;
 Buffer := MemAlloc(BlockSize);
 if Buffer = nil then Exit;
 case Mode of
  1: begin
      Cl := FilePos div (BlockSize); Inc(Cl, Start);
      M :=  LongInt(FilePos) mod LongInt(BlockSize);
      repeat
       if L + M < BlockSize then T := M + L else T := BlockSize;
       Drv^.ClusterRead(Buffer^, Cl);
       Move(TByteArray(Buf)[Pos], Buffer^[M], T - M);
       Drv^.ClusterWrite(Buffer^, Cl);
       Inc(Cl);
       Inc(Pos, T - M);
       Dec(L, T - M);
       M := 0;
      until L <= 0;
    end;
  2: begin
      Cl := FilePos div (BlockSize);
      J := Start;
      for I := 1 to Cl do J := Drv^.GetFAT(J);
      if J < 2 then J := 2; Cl := J;
      M :=  LongInt(FilePos) mod LongInt(BlockSize);
      repeat
       if L + M < BlockSize then T := M + L else T := BlockSize;
       Drv^.ClusterRead(Buffer^, Cl);
       Move(TByteArray(Buf)[Pos], Buffer^[M], T - M);
       Drv^.ClusterWrite(Buffer^, Cl);
       Cl := Drv^.GetFAT(Cl);
       if Cl < 2 then Cl := 2;
       Inc(Pos, T - M);
       Dec(L, T - M);
       M := 0;
      until (L <= 0) or (Cl >= Drv^.EOFMark);
    end;
  else
   begin
      Cl := FilePos div LongInt(BlockSize); Inc(Cl, Start);
      M :=  LongInt(FilePos) mod LongInt(BlockSize);
      repeat
       if L + M < BlockSize then T := M + L else T := BlockSize;
       SectorRead(Drv^.Drive, Buffer, Cl, 1);
       Move(TByteArray(Buf)[Pos], Buffer^[M], T - M);
       SectorWrite(Drv^.Drive, Buffer, Cl, 1);
       Inc(Cl);
       Inc(Pos, T - M);
       Dec(L, T - M);
       M := 0;
      until (L <= 0) or Abort;
   end;
 end;
 FreeMem(Buffer, BlockSize);
 Inc(FilePos, Count);
end;

procedure TDiskStream.Seek(Pos: Longint);
begin
 FilePos := Pos;
end;

procedure TDiskStream.SetMode;
begin
 Mode := AMode;
 Start := AStart;
 Num := ANumber;
 case Mode of
  0: begin FileSize := Num*Drv^.SectLen; BlockSize := Drv^.SectLen; end;
  1: begin
      if Start < 2 then Start := 2;
      if Start > Drv^.TotalClu + 2 then Start := Drv^.TotalClu + 2;
      if Start + Num - 2 > Drv^.TotalClu then Num := Drv^.TotalClu - Start + 2;
      BlockSize := Drv^.ClusterSize(2); FileSize := Num*BlockSize;
     end;
  2: begin
      if Start < 2 then Start := 2;
      if Start > Drv^.TotalClu + 2 then Start := Drv^.TotalClu + 2;
      Num := Drv^.ChainLen(Start);
      BlockSize := Drv^.ClusterSize(2); FileSize := Num*BlockSize;
     end;
 end;
 FilePos := 0;
end;

function TDiskStream.GetPosInfo;
 var S: String;
     N, I, J: LongInt;
begin
 S := '';
 if (Pos < 0) or (Pos >= FileSize) then Exit;
 case Mode of
   0,1,2: begin
        case Mode of
         0: N := Pos div LongInt(BlockSize) + Start;
         1: begin
             N := Drv^.MadeSec(Pos div LongInt(BlockSize) + Start);
             Inc(N, (Pos mod LongInt(BlockSize)) div Drv^.SectLen);
            end;
         2: begin
             N := Pos div LongInt(BlockSize); J := Start;
             for I := 1 to N do J := Drv^.GetFAT(J);
             N := Drv^.MadeSec(J);
             Inc(N, (Pos mod LongInt(BlockSize)) div Drv^.SectLen);
            end;
        end;
       if N = 0 then S := GetString(dlBootSector)
        else if N < Drv^.Reserved then S := GetString(dlReservedSector)
         else if N < Drv^.Reserved+Drv^.SectPerFAT*Drv^.FATCopies then
                begin
                 I := 1 + (N - Drv^.Reserved) div Drv^.SectPerFAT;
                 if I = 1 then S := GetString(dl1st) else
                  if I = 2 then S := GetString(dl2nd) else
                   if I = 3 then S := GetString(dl3rd) else S := ItoS(I)+GetString(dlSometh);
                 S := S + GetString(dlCopyOfFAT)
                end
          else if N < Drv^.FirstClust then S := GetString(dlRootDirectory) else
           S := GetString(dlCluster) + ' ' + ItoS(2+(N - Drv^.FirstClust) div Drv^.SectPerClust);
       S := GetString(dlSector) + ' ' + ItoS(N) + ', ' + S;
      end;
 end;
 GetPosInfo := S;
end;

constructor TDiskFixer.Init;
 var P: PDiskStream;
begin
 New(P, Init(0));
 inherited Init(R, P, '', '', nil, False, True);
 ViewMode := 2; HexEdit := True;
 HelpCtx := hcDiskEditor;
end;


procedure TDiskFixer.MakeLines;
 var P1, P2: Pointer;
     I, J, K, Hx: Integer;
     B: Byte;
begin
 if Buf = nil then Exit;
 if ViewMode = 0 then begin inherited MakeLines; Exit end;
 P1 := Buf; P2 := @Lines; I := BufSize - J; J := BufPos;
 I := BufPos;
 MaxLines := 200;
 for K := 0 to MaxLines do
    begin Lines[K].Pos := I; Lines[K].Len := HexPos; Inc(I, HexPos); end;
end;

procedure TDiskFixer.CountDown;
 var K, Q, M: Integer;
     SL: LongInt;
     P: Pointer;
begin
 if Buf = nil then Exit;
 if ViewMode = 0 then begin inherited CountDown(ANumber); Exit end;
 SL := PDiskStream(Fl)^.Drv^.SectLen; Q := K;
 if (FilePos + LongInt(BufPos) + LongInt(HexPos)*LongInt(Size.Y) > FileSize) then Exit;
 if ViewMode > 0 then
  begin
   while (BufPos + HexPos < BufSize) and (ANumber > 0) do
    begin Inc(BufPos, HexPos); Dec(ANumber) end;
   if BufPos + HexPos*Size.Y > BufSize then
     Seek(FilePos+LongInt(BufPos));
   while (ANumber > 0) do begin Inc(BufPos, HexPos); Dec(ANumber) end;
   DrawView;
   MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
   Exit;
  end;
end;


function DirStr(var B): String;
 var D: DirRec Absolute B;
     S, S1: String;
     T: DateTime;
 type Str3 = Array [1..3] of Char;
 const Dr: Array[1..6] of Str3 = ('Arc','Dir','Vol','Sys','Hid','R/O');
begin
 if D.Name[0] = #0 then
  begin DirStr := GetString(dlUnusedDirEntry); Exit; end;
 S := D.Name; if D.Attr and Directory = 0 then LowStr(S); Insert(' ', S, 9);
 UnpackTime(D.Date, T);
 MakeDate(0, T.Day, T.Month, T.Year mod 100, T.Hour, T.Min, S1);
 S := S +PredSpace(ItoS(D.Len), 11) + Copy('  ',1,1+Byte(CountryInfo.TimeFmt <> 0))+
      S1 + PredSpace(FStr(D.Clus), 8);
 for T.Year := 1 to 6 do
  if D.Attr and ($40 shr T.Year) <> 0 then S := S + '  ' + Dr[T.Year] else S := S + '     ';
 DirStr := S;
end;

procedure TDiskFixer.HandleEvent;

 var P: TPoint;
     LR: Integer;
     I, BS, BE: LongInt;
     C: Char;
     S: String;
     PL: PInputLine;
     W: LongInt;
     Drv: PDiskDriver;

 procedure CE; begin ClearEvent(Event) end;
 procedure CED; begin CE; DrawView end;

(* procedure SeekEOF;
 begin
{  if Lines[Size.Y].Pos < 0 then Exit;
  Owner^.Lock;
  Seek(FileSize - BufSize); BufPos := BufSize-1;
  CountUp(Size.Y+3);
  CountDown(3);
  MakeLines; DrawView;
  Owner^.UnLock;}
 end;*)

 procedure EditField;
  var AA: Word;
      L: LongInt;
      DT: DateTime;
      D: ^DirRec;
 begin
    AA := BufPos+(XPos div 32)*32;
    D := @Buf^[AA]; UnPackTime(D^.Date, DT);
    case XPos mod 32 of
     1: begin PL := New(PInputString, Init(LastR, 3));
              S := Copy(D^.Name,9,3); PL^.SetData(S); end;
     2: begin PL := New(PInputLongInt, Init(LastR, 0, $7FFFFFFF));
              PL^.SetData(D^.Len); end;
     3: begin PL := New(PInputWord, Init(LastR, 1, 31));
              PL^.SetData(DT.Day); end;
     4: begin PL := New(PInputWord, Init(LastR, 1, 12));
              PL^.SetData(DT.Month); end;
     5: begin PL := New(PInputWord, Init(LastR, 80, 99));
              DT.Year := DT.Year mod 100; PL^.SetData(DT.Year); end;
     6: begin PL := New(PInputWord, Init(LastR, 0, 23));
              PL^.SetData(DT.Hour); end;
     7: begin PL := New(PInputWord, Init(LastR, 0, 59));
              PL^.SetData(DT.Min); end;
     8: begin PL := New(PInputWord, Init(LastR, 0, PDiskStream(Fl)^.Drv^.TotalClu+2));
              PL^.SetData(D^.Clus); end;
     else begin PL := New(PInputString, Init(LastR, 8));
                S := Copy(D^.Name,1,8); PL^.SetData(S); end;
    end;
    PutEvent(Event);
    I := Owner^.ExecView(PL);
    if I <> cmCancel then
     begin
      BufModified := True;
      case XPos mod 32 of
       1: begin PL^.GetData(S); UpStr(S); While (S[0] < #3) do S := S + ' ';
                  Move(S[1], D^.Name[8], 3); end;
       2: PL^.GetData(D^.Len);
       8: PL^.GetData(D^.Clus);
       3..7: begin
              case XPos mod 32 of
               3: PL^.GetData(DT.Day);
               4: PL^.GetData(DT.Month);
               5: begin PL^.GetData(DT.Year); Inc(DT.Year, 1900); end;
               6: PL^.GetData(DT.Hour);
               7: PL^.GetData(DT.Min);
              end;
              PackTime(DT, D^.Date);
             end;
       else begin PL^.GetData(S); UpStr(S); While (S[0] < #8) do S := S + ' ';
                  Move(S[1], D^.Name, 8); end;
      end;
      DrawView;
      if I <> cmOK then
       begin
        Event.What := evKeyDown; Event.KeyCode := I;
        PutEvent(Event);
       end;
     end;
    Dispose(PL,Done);
 end;

 procedure BlockOff; begin BlockStart := FilePos+BufPos+XPos; BlockEnd := BlockStart; end;

 procedure CheckMark;
  begin
   if ShiftState and 3 <> 0 then BlockEnd := FilePos+BufPos+XPos else BlockOff;
   DrawDisabled := False; DrawView;
  end;

 function ChDrive(N: Byte): Boolean;
  var P: PStream;
 begin
  ChDrive := True;
  if N = PDiskStream(Fl)^.Drv^.Drive+1 then Exit;
  P := Fl;
  Fl := New(PDiskStream, Init(N));
  if (Fl = nil) or (PDiskStream(Fl)^.Drv = nil) then begin Fl := P; ChDrive := False end else Dispose(P,Done);
 end;

 procedure FixFile;
  var S: String;
      W: Word;
 begin
  S := GetFileNameDialog(x_x, GetString(dlChooseFile), GetString(dlFileName), fdOKButton + fdHelpButton, 100);
  if S = '' then Exit;
  if not ChDrive(Byte(S[1])-64) then Exit;
  W := PDiskStream(Fl)^.Drv^.GetPath(S);
  if W <> 0 then
   begin
    ViewMode := 1; XPos := 0; BufPos := 0;
    PDiskStream(Fl)^.SetMode(2, W, 0);
    Seek(0); DrawView;
   end;
 end;

 procedure FixDir;
  var S: String;
      W: Word;
 begin
  S := ChangeDir(GetString(dlChooseDir), 0);
  if S = '' then Exit;
  if not ChDrive(Byte(S[1])-64) then Exit;
  Drv := PDiskStream(Fl)^.Drv;
  W := Drv^.GetPath(S);
  if W <> 0 then
   begin
    ViewMode := 2; XPos := 0; BufPos := 0;
    PDiskStream(Fl)^.SetMode(2, W, 0);
    Seek(0); DrawView;
   end else
   begin
    ViewMode := 2; XPos := 0; BufPos := 0;
    PDiskStream(Fl)^.SetMode(0, (Drv^.SectPerFat * Drv^.FatCopies) + Drv^.Reserved,
                                                              Drv^.MadeLen(0));
    Seek(0); DrawView;
   end;
 end;

 procedure FixDrive;
  var S: String;
      T: TPoint;
      P: PStream;
 begin
  T.Y := 0; T.X := (Size.X-8) div 2; MakeGlobal(T,T);
  S := SelectDrive(T.X, T.Y, Char(PDiskStream(Fl)^.Drv^.Drive+65), False);
  if S = '' then Exit;
  WriteModify;
  ChDrive(Byte(S[1])-64);
  BufPos := 0; XPos := 0; ViewMode := 2;
  Seek(0); DrawView;
 end;

 procedure FixSector;
  var D: PDialog;
      P: PView;
      R: TRect;
      I: LongInt;
      S: String;
      DR: Record S1, S2: String[11]; end;
 begin
  Drv := PDiskStream(Fl)^.Drv;
  R.Assign(1,1,40,17);
  D := New(PDialog, Init(R, GetString(dlSectorRange)));
  D^.Options := D^.Options or ofCentered;
  R.Assign(1,1,38,3);
  S := GetString(dlValidSectors)+ItoS(Drv^.NumSectors-1);
  P := New(PStaticText, Init(R, S));
  P^.Options := P^.Options or ofCenterX;
  D^.Insert(P);

  R.Assign(20, 4, 33, 5);
  P := New(PInputLine, Init(R, 11));
  P^.Options := P^.Options or ofVersion20;
  PInputLine(P)^.SetValidator(New(PRangeValidator, Init(0, Drv^.NumSectors - 1)));
  D^.Insert(P);
  S := GetString(dlStartingSector);
  R.Assign(20-Byte(S[0]), 4, 19, 5);
  P := New(PLabel, Init(R, S, P));
  D^.Insert(P);

  R.Assign(20, 6, 33, 7);
  P := New(PInputLine, Init(R, 11));
  P^.Options := P^.Options or ofVersion20;
  PInputLine(P)^.SetValidator(New(PRangeValidator, Init(1, Drv^.NumSectors - 1)));
  D^.Insert(P);
  S := GetString(dlNumberSectors);
  R.Assign(20-Byte(S[0]), 6, 19, 7);
  P := New(PLabel, Init(R, S, P));
  D^.Insert(P);

  R.Assign(3,8,37,12);
  S := GetString(dlSBootSector)+^M+
       SStr(Drv^.Reserved, 5, ' ')+' - '+AddSpace(ItoS(Drv^.Reserved+Drv^.SecFAT-1), 5)+GetString(dlS1stFAT)+^M;
  if Drv^.FATCopies > 1 then
    S := S+SStr(Drv^.Reserved+Drv^.SecFAT, 5, ' ')+' - '
          +AddSpace(ItoS(Drv^.Reserved+2*Drv^.SectPerFAT-1), 5)+GetString(dlS2ndFAT)+^M;
  S := S + +SStr(Drv^.Reserved+Drv^.FATCopies*Drv^.SectPerFAT, 5, ' ')+' - '
          +AddSpace(ItoS(Drv^.Reserved+Drv^.FATCopies*Drv^.SectPerFAT+(LongInt(Drv^.RootEntries)*32)
                       div Drv^.SectLen - 1), 5)+GetString(dlRootDirectory);
  P := New(PStaticText, Init(R, S));
  P^.Options := P^.Options or ofFramed;
  D^.Insert(P);

  R.Assign(8, 13, 18, 15);
  P := New(PButton, Init(R, GetString(dlOKButton), cmOK, bfDefault));
  D^.Insert(P);
  R.Assign(22, 13, 32, 15);
  P := New(PButton, Init(R, GetString(dlCancelButton), cmCancel, 0));
  D^.Insert(P);

  D^.SelectNext(False);
  I := Desktop^.ExecView(D);
  D^.GetData(DR); Dispose(D,Done);
  if I <> cmOK then Exit;

  PDiskStream(Fl)^.SetMode(0, StoI(DR.S1), StoI(DR.S2));
  BufPos := 0; XPos := 0; XXPos := True; Seek(0); DrawView;

 end;

 procedure FixCluster;
  var D: PDialog;
      P: PView;
      R: TRect;
      I: LongInt;
      S: String;
      DR: Record S1, S2: String[11]; end;
 begin
  Drv := PDiskStream(Fl)^.Drv;
  R.Assign(1,1,40,12);
  D := New(PDialog, Init(R, GetString(dlClusterRange)));
  D^.Options := D^.Options or ofCentered;
  R.Assign(1,1,38,3);
  S := GetString(dlValidClusters)+ItoS(Drv^.TotalClu+1);
  P := New(PStaticText, Init(R, S));
  P^.Options := P^.Options or ofCenterX;
  D^.Insert(P);

  R.Assign(24, 4, 37, 5);
  P := New(PInputLine, Init(R, 11));
  P^.Options := P^.Options or ofVersion20;
  PInputLine(P)^.SetValidator(New(PRangeValidator, Init(2, Drv^.TotalClu + 1)));
  D^.Insert(P);
  S := GetString(dlStartingCluster);
  R.Assign(24-Byte(S[0]), 4, 23, 5);
  P := New(PLabel, Init(R, S, P));
  D^.Insert(P);

  R.Assign(24, 6, 37, 7);
  P := New(PInputLine, Init(R, 11));
  P^.Options := P^.Options or ofVersion20;
  PInputLine(P)^.SetValidator(New(PRangeValidator, Init(2, Drv^.TotalClu + 1)));
  D^.Insert(P);
  S := GetString(dlNumberClusters);
  R.Assign(24-Byte(S[0]), 6, 23, 7);
  P := New(PLabel, Init(R, S, P));
  D^.Insert(P);

  R.Assign(8, 8, 18, 10);
  P := New(PButton, Init(R, GetString(dlOKButton), cmOK, bfDefault));
  D^.Insert(P);
  R.Assign(22, 8, 32, 10);
  P := New(PButton, Init(R, GetString(dlCancelButton), cmCancel, 0));
  D^.Insert(P);

  D^.SelectNext(False);
  I := Desktop^.ExecView(D);
  D^.GetData(DR); Dispose(D,Done);
  if I <> cmOK then Exit;

  PDiskStream(Fl)^.SetMode(1, StoI(DR.S1), StoI(DR.S2));
  BufPos := 0; XPos := 0; XXPos := True; Seek(0); DrawView;
 end;

 procedure FillBlock;
  var R: TRect;
      D: PDialog;
      SC: PStringCollection;
      PV: PView;
      C: Char;
      I, J: Word;
      DT: record P: Pointer; C: Word; end;
      P: Pointer;
      L: LongInt;
 begin
  if BlockStart = BlockEnd then Exit;
  R.Assign(1,1,30,12);
  New(D, Init(R, GetString(dlFixerFill)));
  D^.Options := D^.Options or ofCentered;

  PV := D^.StandardScrollBar(sbVertical+sbHandleKeyboard);
  R.Assign(23,2,24,7);
  PV^.Locate(R);

  R.Assign(5,2,23,7);
  PV := New(PCharBox, Init(R, 1, PScrollBar(PV)));
  New(SC, Init(256,10));
  for C := #0 to #255 do SC^.Insert(NewStr(C));
  PListBox(PV)^.NewList(SC);
  D^.Insert(PV);
  R.Assign(5,1,23,2);
  PV := New(PLabel, Init(R, 'Hex   Dec', PV));
  D^.Insert(PV);

  R.Assign(3, 8, 13, 10);
  PV := New(PButton, Init(R, GetString(dlOKButton), cmOK, bfDefault));
  D^.Insert(PV);
  R.Assign(13, 8, 23, 10);
  PV := New(PButton, Init(R, GetString(dlCancelButton), cmCancel, 0));
  D^.Insert(PV);
  D^.SelectNext(False);

  I := Desktop^.ExecView(D);
  D^.GetData(DT);
  Dispose(D,Done);
  if I <> cmOK then Exit;
  if MessageBox(GetString(dlFillQuery), nil, mfWarning + mfYesButton + mfNoButton)
     <> cmYes then Exit;
  if BlockStart < BlockEnd then begin BS := BlockStart; BE := BlockEnd end
                           else begin BE := BlockStart; BS := BlockEnd end;
  L := BE - BS + 1;
  I := $8000; if I > MaxAvail then I := MaxAvail;
  if L < I then I := L;
  GetMem(P, I);
  FillChar(P^, I, DT.C);
  Fl^.Seek(FilePos);
  Fl^.Write(Buf^, BufSize);
  Fl^.Seek(BS);
  repeat
   if I < L then J := L else J := I;
   Fl^.Write(P^, J);
   Dec(L, J);
  until L <= 0;
  BufModified := False;
  I := BufPos;
  Seek(FilePos);
  BufPos := I;
  DrawView;
 end;

 procedure WriteBlock;
  var S: String;
      P: PStream;
      PP: Pointer;
 begin
  PP := @S;
  if BlockStart < BlockEnd then begin BS := BlockStart; BE := BlockEnd end
                           else begin BE := BlockStart; BS := BlockEnd end;
  S := GetFileNameDialog(x_x, GetString(dlCopyTo), GetString(dlFileName), fdOKButton+fdHelpButton, 100);
  if S = '' then Exit;
  P := New(PBufStream, Init(S, stOpen, 5000));
  if P^.Status <> stOK then
   begin
    Dispose(P,Done);
    P := New(PBufStream, Init(S, stCreate, 5000));
    if P^.Status <> stOK then
     begin
      ErrMsg(dlCanNotWrite);
      Dispose(P,Done);
      Exit;
     end;
   end else
   begin
    if Msg(dlFileExist, @PP, mfYesNoConfirm) <> cmYes then Exit;
    P^.Truncate;
   end;
  Fl^.Seek(BS);
  P^.CopyFrom(Fl^, Abs(BE-BS+1));
  Dispose(P,Done);
 end;


begin
 TView.HandleEvent(Event);
 if Event.What = evNothing then exit;
 if Clipboard <> nil then EnableCommands([cmPaste]) else DisableCommands([cmPaste]);
 if BlockStart <> BlockEnd then EnableCommands([cmCopy, cmFixBlockWrite, cmClear])
                           else DisableCommands([cmCopy, cmFixBlockWrite, cmClear]);
 case ViewMode of
  1: begin HexPos := (Size.X - 12) div 4; if HexPos <= 0 then HexPos := 1; end;
  2: HexPos := 32;
  3: begin
      if Fl<>nil then begin
       case PDiskStream(Fl)^.Drv^.FATType of
        FAT12: W := 3;
        FAT16: W := 4;
       end;
       W := W * ((Size.X - 3) div (2*8));
       if W = 0 then
        case PDiskStream(Fl)^.Drv^.FATType of
         FAT12: W := 3;
         FAT16: W := 4;
        end;
       HexPos := W;
      end;
     end;
  else HexPos := 255;
 end;
 if Loaded then
  begin
   if SB <> nil then PViewScroll(SB)^.MaxV := FileSize;
   MessageL(SB, evCommand, cmChangeValue, FilePos+BufPos);
   Loaded := False;
  end;
 W := BufPos+(XPos div 32)*32;
 case Event.What of
  evCommand: case Event.Command of
              cmGetName: begin PString(Event.InfoPtr)^ := GetString(dlDiskFixer); Event.What := 0; end;
              cmUndo: begin Fl^.Seek(FilePos); Fl^.Read(Buf^, BufSize); BufModified := False; CED; end;
              cmFixFile: begin CE; FixFile; end;
              cmFixDir: begin CE; FixDir; end;
              cmFixDrive: begin CE; FixDrive; end;
              cmFixSector: begin CE; FixSector; end;
              cmFixCluster: begin CE; FixCluster; end;
              cmFixBlockWrite: begin CE; WriteBlock; end;
              cmClear: begin CE; FillBlock; end;
              cmCopy: begin
                       CE;
                       if BlockStart = BlockEnd then Exit;
                       if Clipboard <> nil then
                        begin FreeMem(Clipboard, ClipSize); Clipboard := nil; end;
                       if BlockStart < BlockEnd then begin BS := BlockStart; BE := BlockEnd end
                           else begin BE := BlockStart; BS := BlockEnd end;
                       if ViewMode = 2 then
                        begin
                         BE := (BE div 32) * 32 + 31;
                         BS := (BS div 32) * 32;
                        end;
                       if MaxAvail > 60000 then ClipSize := 60000 else ClipSize := MaxAvail - 4096;
                       if LongInt(ClipSize) < BE-BS then
                         begin Application^.OutOfMemory; Exit; end;
                       ClipSize := Abs(BE-BS);
                       GetMem(Clipboard, ClipSize);
                       WriteModify;
                       Fl^.Seek(BS);
                       Fl^.Read(Clipboard^, ClipSize);
                      end;
              cmFATMode: begin
                          ViewMode := 3; XPos := 0; CE; DrawView;
                         end;
              cmPaste: begin
                        CE; if Clipboard = nil then Exit;
                        WriteModify;
                        case ViewMode of
                         1: Fl^.Seek(FilePos+BufPos+XPos);
                         2: Fl^.Seek(((FilePos+BufPos+XPos) div 32) * 32);
                        end;
                        Fl^.Write(ClipBoard^, ClipSize);
                        Fl^.Seek(FilePos); Fl^.Read(Buf^, BufSize);
                        DrawView;
                       end;
              cmLoadViewFile: CE;
              cmClose: if WriteModify then CE;
              cmTextMode: begin ViewMode := 0; MakeLines; BlockOff; DrawView; CE end;
              cmHexMode: begin if ViewMode = 0 then XPos := 0; XXPos := True;
                               BlockOff;
                               W := (XPos div 32) * 32;
                               if ViewMode = 2 then
                                case XPos mod 32 of
                                 0: XPos := W;
                                 1: XPos := W+8;
                                 2: XPos := W+12+10+4+2;
                                 3..7: XPos := W+12+10;
                                 8: XPos := W+22+4;
                                  else XPos := W+11;
                                end;
                                ViewMode := 1; HexEdit := True; DrawView; CE end;
              cmDirMode: begin if ViewMode = 0 then XPos := 0 else
                               BlockOff;
                               W := (XPos div 32) * 32;
                               if ViewMode = 1 then
                                case XPos mod 32 of
                                 0..7: XPos := W;
                                 8..10: XPos := W+1;
                                 11: XPos := W+9;
                                 22..25: XPos := W+3;
                                 26..27: XPos := W+2;
                                 28..29: XPos := W+8;
                                  else XPos := W;
                                end;
                                ViewMode := 2; DrawView; CE end;
              {cmFATMode: begin ViewMode := 3; DrawView; CE end;}
              cmMainMenu: begin
                           Message(DNApp.MenuBar, evCommand, cmMenu, nil); CE;
                          end;
             end;
  evKeyDown: case Event.KeyCode of
               kbESC: begin CE; Message(Application, evCommand, cmClose, nil); end;
               kbEnter: begin
                         BlockOff;
                         case ViewMode of
                          1: begin
                              HexEdit := not HexEdit;
                              XXPos := True;
                              DrawView;
                              end;
                          2: if (Buf^[W] > 32) and (Buf^[W] <> $E5) then
                             begin
                              Drv := PDiskStream(Fl)^.Drv;
                              W := BufPos+(XPos div 32)*32;
                              I := Buf^[W+11];
                              W := LongInt(Buf^[W+11+1+10+4]) + LongInt(Buf^[W+11+1+10+5]) * 256;
                              if (W = 0) and (I and (Directory + VolumeID) = Directory)
                                 or (W > 1) and (W < Drv^.TotalClu+2) and
                                 (I and (Directory + VolumeID) = Directory) then
                               begin
                                WriteModify;
                                if W = 0 then
                                  PDiskStream(Fl)^.SetMode(0, (Drv^.SectPerFat * Drv^.FatCopies) + Drv^.Reserved,
                                                              Drv^.MadeLen(0))
                                  else PDiskStream(Fl)^.SetMode(2, W, 0);
                                ViewMode := 2; XPos := 0; BufPos := 0;
                                Seek(0); DrawView;
                               end else
                               if I and VolumeID = 0 then
                                begin
                                 WriteModify;
                                 PDiskStream(Fl)^.SetMode(2, W, 0);
                                 ViewMode := 1; HexEdit := True; XPos := 0; BufPos := 0;
                                 Seek(0); DrawView;
                                end;
                             end;
                         end;
                         CE
                        end;
               kbHome: CE;
               kbEnd: CE;
               kbDown: begin CE; if (ViewMode > 0) and (XPos < LastChars - HexPos - 1)
                                   then begin Inc(XPos, HexPos); XXPos := True; end else
                                         begin DrawDisabled := True; CountDown(1); end; CheckMark; end;
               kbPgDn: begin CE; DrawDisabled := True; CountDown(Size.Y-1); CheckMark; end;
               kbUp: begin CE; if (ViewMode > 0) and (XPos >= HexPos) then
                                 begin XXPos := True; Dec(XPos, HexPos); end else
                                 begin DrawDisabled := True; CountUp(1); end; CheckMark; end;
               kbPgUp: begin CE; DrawDisabled := True; CountUp(Size.Y-1); CheckMark; end;
               kbCtrlPgUp: begin Seek(0); XPos := 0; CheckMark; CE end;
               kbLeft: begin
                         CE; DrawDisabled := True;
                         case ViewMode of
                          0: if XDelta > 0 then begin Dec(XDelta); DrawView end;
                          1: if HexEdit and not XXPos then XXPos := True else
                             if XPos > 0 then begin Dec(XPos); {if HexEdit then XXPos := False} end  else
                              begin CountUp(1); XPos := HexPos - 1; end;
                          2: if (XPos mod 32 <> 0) {and (Buf^[BufPos+(XPos and not $31)]<>0)} then Dec(XPos)
                               else begin
                                     if XPos = 0 then begin CountUp(1); XPos := 14 end
                                                 else XPos := ((XPos div 32)-1)*32 + 14;
                                    end;
                          3: begin
                              case PDiskStream(Fl)^.Drv^.FATType of
                               FAT12: if (XPos mod 3 = 0)
                                       then Dec(XPos, 2)
                                       else Dec(XPos);
                               FAT16: Dec(XPos, 2);
                              end;
                              if XPos < 0 then begin
                               Inc(XPos, HexPos);
                               CountUp(1);
                              end;
                             end;
                         end;
                         CheckMark;
                       end;
               kbRight: begin
                         CE; DrawDisabled := True;
                         case ViewMode of
                          0: if (XDelta < 255 - Size.X + 1) then begin Inc(XDelta); DrawView; end;
                          1: {if HexEdit and XXPos then XXPos := False else}
                             if XPos < LastChars-1 then begin Inc(XPos); XXPos := True;{if HexEdit then XXPos := True} end else
                              begin CountDown(1); Dec(XPos, HexPos - 1); end;
                          2: if (XPos mod 32 < 14) and (Buf^[W]<>0) then Inc(XPos)
                               else begin
                                     if XPos + 32 > LastChars then
                                      begin
                                       CountDown(1); XPos := (XPos div 32)*32;
                                       if XPos + 32 > LastChars then Dec(XPos, 32);
                                      end else
                                       XPos := ((XPos div 32)+1)*32;
                                       if XPos + 32 > LastChars then Dec(XPos, 32);
                                    end;
                          3: begin
                              case PDiskStream(Fl)^.Drv^.FATType of
                               FAT12: if XPos mod 3 = 0
                                       then Inc(XPos)
                                       else Inc(XPos,2);
                               FAT16: else Inc(XPos, 2);
                              end;
                              if XPos >= Size.Y * HexPos then begin
                               Dec(XPos, HexPos);
                               CountDown(1);
                              end;
                             end;
                         end;
                         CheckMark;
                        end;
               {kbCtrlPgDn: begin SeekEOF; CE end;}
               else
                    if (Event.CharCode > #31) then
                    begin
                     BlockOff;
                     C := Event.CharCode;
                     case ViewMode of
                      1: begin
                          if HexEdit then
                            begin
                             C := UpCase(C);
                             if C in ['0'..'9','A'..'F'] then
                              begin
                               if XXPos then Buf^[BufPos+XPos] := (Buf^[BufPos+XPos] and 15) or
                                                                  (Pos(C, '123456789ABCDEF') shl 4)
                                        else Buf^[BufPos+XPos] := (Buf^[BufPos+XPos] and $F0) or (Pos(C, '123456789ABCDEF'));
                               if XXPos then begin XXPos := False; DrawView end
                                 else Message(@Self, evKeyDown, kbRight, nil);
                                BufModified := True;
                              end
                            end else
                                 begin
                                  Buf^[BufPos+XPos] := Byte(C);
                                  BufModified := True;
                                  Message(@Self, evKeyDown, kbRight, nil);
                                 end;
                         end;
                      3: begin
                          case Event.CharCode of
                           'E', 'e': begin
                                      BufModified := True;
                                      W := $FFFF;
                                      case PDiskStream(Fl)^.Drv^.FATType of
                                       FAT12: begin
                                               W := W and $FFF;
                                               if XPos mod 3 <> 0
                                                then W := W shl 4 or (Buf^[BufPos+XPos] and $F)
                                                else W := W or (Buf^[BufPos+XPos+1] and $F0) shl 8;
                                               Move(W, Buf^[BufPos+XPos], 2);
                                              end;
                                       FAT16: Move(W, Buf^[BufPos+XPos], 2);
                                      end;
                                      CE; DrawView;
                                end;
                           '0'..'9': begin
                                      PL := New(PInputWord, Init(LastR, 0, 65535));
                                      W := BufPos + XPos;
                                      case PDiskStream(Fl)^.Drv^.FATType of
                                       FAT12: if XPos mod 3 <> 0
                                               then W := (Buf^[W]+Buf^[W+1] shl 8) shr 4
                                               else W := (Buf^[W]+Buf^[W+1] shl 8) and $FFF;
                                       FAT16: W := Buf^[W] + Buf^[W+1] shl 8;
                                      end;
                                      {PL^.SetData(W);}
                                      PutEvent(Event);
                                      I := Owner^.ExecView(PL);
                                      PL^.GetData(W);
                                      Dispose(PL,Done);
                                      CE;
                                      if I = cmCancel then Exit;
                                      BufModified := True;
                                      Case PDiskStream(Fl)^.Drv^.FATType of
                                       FAT12: begin
                                               W := W and $FFF;
                                               if XPos mod 3 <> 0
                                                then W := W shl 4 or (Buf^[BufPos+XPos] and $F)
                                                else W := W or (Buf^[BufPos+XPos+1] and $F0) shl 8;
                                               Move(W, Buf^[BufPos+XPos], 2);
                                              end;
                                       FAT16: Move(W, Buf^[BufPos+XPos], 2);
                                      end;
                                      if I <> cmOK then begin
                                       Event.What := evKeyDown;
                                       Event.KeyCode := I;
                                       PutEvent(Event);
                                      end;
                                      DrawView;
                                     end;
                          end;
                         end;
                      2: begin
                          case XPos mod 32 of
                            9..14: begin
                                    if Event.CharCode = #32 then
                                     begin
                                      BufModified := True;
                                      I := (XPos mod 32 - 9);
                                      Buf^[BufPos+(XPos div 32)*32+11] :=
                                         Buf^[BufPos+(XPos div 32)*32+11] xor ($20 shr I);
                                      DrawView;
                                     end;
                                   end;
                            else if LastR.A.X <> LastR.B.X then EditField;
                          end;
                         end;
                     end;
                     CE
                    end;
             end;
  evMouseDown: begin
                CE
               end;
 end;
 if Event.What <> evNothing then
   inherited HandleEvent(Event);
end;

procedure TDiskFixer.Draw;
var
  B: TDrawBuffer;
  C, C2, C3, C4: Byte;
  I, J, K, M: Integer;
  L: LongInt;
  S: String;

procedure MoveStr(var B, S; C: Byte);
begin
 asm
  push ds
  lds  si,B
  les  di,S
  mov  cl, es:[di]
  xor ch,ch
  jcxz @@Exit
  mov bx, 0
  mov ah, C
@@1:
  mov al, es:[di+1]
  cmp al, 9
  jne @@3
  mov al, ' '
@@2:
  mov ds:[si], ax
  add si,2
  inc bx
  mov dx, bx
  and dx, 7
  jnz @@2
  jmp @@4
@@3:
  mov ds:[si], ax
  add si,2
  inc bx
@@4:
  inc di
  loop @@1
@@Exit:
  pop  ds
 end;
end;

var W: Integer;

procedure DrawHex;
 var I, J, K, M, Q, QQ, QQQ: Integer;
     Ps, L: Integer;
     CL1, Cl2: Word;
     SL: LongInt;
     P: Pointer;
     Entered, Marked: Boolean;
     BE, BS: LongInt;
begin
  Ps := 0; Marked := BlockStart <> BlockEnd;
  if BlockStart < BlockEnd then begin BS := BlockStart; BE := BlockEnd end
                           else begin BE := BlockStart; BS := BlockEnd end;
  if ViewMode = 2 then
   begin
    HideCursor;
    case XPos mod 32 of
     1: begin L := 3;  Ps := 9; end;
     2: begin L := 10; Ps := 13; end;
     3: begin L := 2;  Ps := 25; end;
     4: begin L := 2;  Ps := 28; end;
     5: begin L := 2;  Ps := 31; end;
     6: begin L := 2;  Ps := 34; end;
     7: begin L := 2;  Ps := 37; end;
     8: begin L := 6;  Ps := 41; end;
     9: begin L := 3;  Ps := 49; end;
    10: begin L := 3;  Ps := 54; end;
    11: begin L := 3;  Ps := 59; end;
    12: begin L := 3;  Ps := 64; end;
    13: begin L := 3;  Ps := 69; end;
    14: begin L := 3;  Ps := 74; end;
     else begin L := 8; Ps := 0; end;
    end;
   end;
   W := BufPos;
   J := 0; K := BufPos; P := Buf; M := Size.X; if M <= 0 then Exit;
   SL := PDiskStream(Fl)^.Drv^.SectLen; Q := K;
   QQ := (LongInt(K) + FilePos) mod LongInt(SL);
   XDelta := 0;
   if (Ps <> 0) and (Ps + L > Size.X) then XDelta := Ps + L - Size.X + 1;
   Entered := False;
   while XPos + BufPos + FilePos >= FileSize do
    if XPos > 0 then Dec(XPos) else if BufPos > 0 then Dec(BufPos) else Break;
   while J < Size.Y do
    begin
     S := '';
     MoveChar(B, ' ', C, 200);
     if ((LongInt(K) + FilePos) mod LongInt(SL) = 0) and (Q = K) and
        (LongInt(K) + FilePos < FileSize)
      then
      begin
       S := PDiskStream(Fl)^.GetPosInfo((LongInt(K) + FilePos)); Q := K+1; QQ := 0;
       MoveStr(B, S, C3);
      end else if (LongInt(K) + FilePos) < FileSize then
      begin
       if QQ + HexPos > SL then Q := SL - QQ
                           else Q := HexPos;
       case ViewMode of
        1: begin
              Entered := True;
            S := DumpStr(Buf^[K], QQ, Q, 0);
            if Q <> HexPos then Insert(Strg(' ', 3*HexPos+12+Q-Length(S)), S, Length(S)-Q-2)
             else if Q and 3 = 0 then
                   for I := 1 to Q div 4 - 1 do S[10+I*12] := '-';
            if (XPos >= K - BufPos) and (XPos < K - BufPos + Q) then
             begin
              if HexEdit then SetCursor((XPos - K + BufPos)*3 + 10 + Byte(not XXPos), J)
                         else SetCursor((XPos - K + BufPos) + HexPos*3 + 12, J);
              ShowCursor;
             end;
             Drivers.MoveStr(B, S, C);
             if Marked then
               if (BS <= K+FilePos) and (BE >= K+FilePos)
                then begin
                      if BE >= K+Q+FilePos then I := Q else I := BE - K - FilePos + 1;
                      MoveColor(B[10], I*3-1, C2);
                      MoveColor(B[HexPos*3 + 12], I, C2);
                     end else
               if (BS > K+FilePos) and (BS <= K+Q+FilePos)
                then begin
                      QQQ := BS - LongInt(K) - FilePos;
                      if QQQ > HexPos then QQQ := HexPos;
                      if QQQ > 0 then
                       begin
                        if BE >= K+Q+FilePos then I := Q else I := BE - K - FilePos + 1;
                        MoveColor(B[10+QQQ*3], (I-QQQ)*3-1, C2);
                        MoveColor(B[HexPos*3 + 12 + QQQ], I-QQQ, C2);
                       end;
                     end;
           end;
        2: begin
            S := DirStr(Buf^[K]);
            Drivers.MoveStr(B[1], S, C);
            if Marked and (BS <= FilePos + K) and
               (BE >= FilePos + K) then
                MoveColor(B, Size.X, C2);
            if (XPos >= K - BufPos) and (XPos < K - BufPos + Q) then
             begin
              LastR.A := LastR.B;
              if Buf^[K] = 0 then begin Ps := 0; L := Size.X; end else
              LastR.Assign(Ps+Origin.X, J + Origin.Y, Ps+Origin.X+L+2, J + Origin.Y + 1);
              MoveColor(B[Ps+1], L, C4); Entered := True;
             end;
          end;
       end;
       Inc(QQ, Q); Inc(K, Q);
       Q := K;
      end;
     WriteLine(0, J, Size.X, 1, B[XDelta]);
     Inc(J);
   end;
   if not Entered then if XPos < 0 then begin XPos := 0; Draw; end
    else begin Inc(BufPos, 32); Dec(XPos, 32); if XPos < 0 then XPos := 0; Draw; end;
   LastChars := K - BufPos;
end;

procedure DrawFAT;
 var I,J: LongInt;
     CL1, CL2: Word;
     Entered: Boolean;
begin
 BlockEnd := BlockStart;
 case PDiskStream(Fl)^.Drv^.FATType of
  FAT12: begin
          While (FilePos + BufPos + XPos + 1 >= FileSize) do
           if XPos > 0
            then if (XPos mod 3 = 0)
                  then Dec(XPos, 2)
                  else Dec(XPos)
            else Break;
          if FilePos + Longint(BufPos) + LongInt(XPos) < 3 then begin
           if FilePos > 0 then Seek(0);
           XPos := 3 - BufPos;
          end;
         end;
  FAT16: Begin
          While (FilePos + BufPos + XPos + 1 >= FileSize) do
           if XPos > 0 then Dec(XPos, 2) else Break;
          if FilePos + Longint(BufPos) + LongInt(XPos) < 4 then begin
           if FilePos > 0 then Seek(0);
           XPos := 4 - BufPos;
          end;
         end;
 end;
 XDelta := 0; K := BufPos; Entered := False;
 for J := 0 to Size.Y - 1 do begin
  MoveChar(B, ' ', C, 200);
  S := '';
  if LongInt(K) + FilePos < FileSize then
  Case PDiskStream(Fl)^.Drv^.FATType of
    FAT12: for I := 1 to HexPos div 3 do begin
            CL2 := K+(I-1)*3;
            if LongInt(CL2) + FilePos < FileSize - 2 then begin
             CL1 := Buf^[CL2]+(Buf^[CL2+1] and 15) shl 8;
             CL2 := (Buf^[CL2+1]+Buf^[CL2+2] shl 8) shr 4;
             if CL1 >= PDiskStream(Fl)^.Drv^.EOFMark
              then S := S + '   <EOF>'
              else S := S + SStr(CL1, 8, ' ');
             if CL2 >= PDiskStream(Fl)^.Drv^.EOFMark
              then S := S + '   <EOF>'
              else S := S + SStr(CL2, 8, ' ');
            end;
           end;
    FAT16: for I := 1 to HexPos div 4 do begin
            CL2 := K+(I-1)*4;
            if LongInt(CL2) + FilePos < FileSize - 3 then begin
             CL1 := Buf^[CL2]+Buf^[CL2+1] shl 8;
             CL2 := Buf^[CL2+2]+Buf^[CL2+3] shl 8;
             if CL1 >= PDiskStream(Fl)^.Drv^.EOFMark
              then S := S + '   <EOF>'
              else if CL1 = PDiskStream(Fl)^.Drv^.BADMark
                    then S := S + '   <BAD>'
                    else S := S + SStr(CL1, 8, ' ');
             if CL2 >= PDiskStream(Fl)^.Drv^.EOFMark
              then S := S + '   <EOF>'
              else if CL2 = PDiskStream(Fl)^.Drv^.BADMark
                    then S := S + '   <BAD>'
                    else S := S + SStr(CL2, 8, ' ');
            end;
           end;
  end;
  if FilePos+K = 0 then S := '     FAT ID     '+Copy(S, 17, 255);
  Inc(K, HexPos);
  Drivers.MoveStr(B[1], S, C);
  if (XPos div HexPos) = J then begin
   case PDiskStream(Fl)^.Drv^.FATType of
    FAT12: I := (2*((XPos mod HexPos)) div 3 + Byte(XPos mod 3 <> 0))*8+4;
    FAT16: I := (XPos mod HexPos) * 4 + 4;
   end;
   LastR.Assign(I-1+Origin.X, J+Origin.Y, I+6+Origin.X, J+1+Origin.Y);
   MoveColor(B[I], 5, C4); Entered := True;
  end;
  WriteLine(0, J, Size.X, 1, B[XDelta]);
 end;
 LastChars := Size.Y * HexPos;
end;

begin
  if BufPos > BufSize + 255 then BufPos := 0;
  if DrawDisabled then Exit;
  case ViewMode of
   1: begin
       HexPos := (Size.X - 12) div 4; if HexPos <= 0 then HexPos := 1;
       if HexPos and 3 = 0 then
        while (((LongInt(BufPos) + FilePos) mod HexPos) <> 0) and
              (BufPos > 0) do Dec(BufPos);
      end;
   2: begin
       while ((LongInt(BufPos) + FilePos) and 31 <> 0) and (BufPos > 0) do Dec(BufPos);
       HexPos := 32;
      end;
   3: begin
       case PDiskStream(Fl)^.Drv^.FATType of
        FAT12: I := 3;
        FAT16: I := 4;
       end;
       I := I * ((Size.X - 3) div (2*8));
       if I = 0 then
        case PDiskStream(Fl)^.Drv^.FATType of
         FAT12: I := 3;
         FAT16: I := 4;
        end;
       HexPos := I;
       case PDiskStream(Fl)^.Drv^.FATType of
        FAT12: while ((LongInt(BufPos) + FilePos) mod 3 <> 0) and (BufPos > 0)
                do Dec(BufPos);
        FAT16: while ((LongInt(BufPos) + FilePos) mod 4 <> 0) and (BufPos > 0)
                do Dec(BufPos);
       end;
      end;
   else HexPos := 255;
  end;
  C := GetColor(1); C2 := GetColor(2); C3 := GetColor(3); C4 := GetColor(4);
  if (Buf = nil) then
   begin HideCursor; inherited Draw; Exit end;
 case ViewMode of
  1,2: DrawHex;
  3: DrawFAT;
  else Inherited Draw;
 end;
 if Info <> nil then Info^.Draw;
end;

function TDiskFixer.GetPalette;
 const S: String[Length(CDiskFixer)] = CDiskFixer;
begin
 GetPalette := @S;
end;

constructor TFixerWindow.Init;
 var R: TRect;
     P: PView;
     PV: PDiskFixer;
     UpMenu: PMenu;
begin
 Desktop^.GetExtent(R);
 inherited Init(R, GetString(dlDiskFixer), 0);
 GetExtent(R); R.Grow(-1, -1); R.B.Y := R.A.Y + 1;
 MenuBar := PView(LoadResource(dlgFixerMenu));
 if MenuBar = nil then Fail;
 MenuBar^.Locate(R);
 MenuBar^.GrowMode := gfGrowHiX;
 Insert(MenuBar);

 GetExtent(R); R.Grow(-1, -1); Inc(R.A.Y);
 PV := New(PDiskFixer, Init(R));
 Insert(PV);

 GetExtent(R);
 Inc(R.A.X, 2); R.A.Y := R.B.Y - 1; R.B.X := R.A.X + 8;
 P := New(PFixerInfo, Init(R, PV));
 Insert(P);
 PV^.Info := P;
end;

function TFixerWindow.GetPalette;
 const  p : String[Length(CFixerWindow)] = CFixerWindow;
begin
 GetPalette:=@P;
end;

{--- start -------- Eugeny Zvyagintzev ---- 23-12-2003 ----}
procedure TFixerWindow.ChangeLanguage;
 var R: TRect;
begin
 If MenuBar <> Nil Then Dispose(MenuBar,Done);

 GetExtent(R); R.Grow(-1, -1); R.B.Y := R.A.Y + 1;
 MenuBar := PView(LoadResource(dlgFixerMenu));
 MenuBar^.Locate(R);
 MenuBar^.GrowMode := gfGrowHiX;
 Insert(MenuBar);

end;
{--- finish -------- Eugeny Zvyagintzev ---- 23-12-2003 ----}

end.
