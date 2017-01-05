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

unit FBB;

interface

uses
  Dos, Tree;

function LongCopy(Fn1: String): boolean; {DataCompBoy}

implementation
uses
  Lfn, {DataCompBoy}
  advance, advance1, advance2, advance3, FileCopy, DNApp, Messages,
    Views,
  Objects, Dialogs, Commands, Drivers, Memory, FilesCol, RStrings,
  HistList, Gauge, Startup, DNUtil, xTime, DiskInfo
  , ErrMess
  ;

type
  Ar65000 = array[1..65000] of byte;
  ArPtr = ^Ar65000;

  HRec = record
    Num: word;
    NR: word;
    NW: word;
    Buf: ArPtr;
    end;

const
  MaxBF = 20;
var
  Bf: array[1..MaxBF] of HRec;
  dr1, dr2: byte;
  FreeSpc: TSize;
  Timer: TEventTimer;
const
  NBf: byte = 0;

function IsLabel(P: PView): boolean;
  begin
    IsLabel := TypeOf(P^) = TypeOf(TLabel);
  end;

procedure GetMaxMem;
  var
    l: longInt;
  begin
    NBf := 0;
    repeat
      l := MaxAvail-15000;
      if l > 0 then
        begin
          Inc(NBf);
          if l > 65000 then
            l := 65000;
          Bf[NBf].Buf := MemAlloc(l);
          Bf[NBf].Num := l;
          if Bf[NBf].Buf = nil then
            Dec(NBf);
        end;
    until (MaxAvail < 16000) or LowMemory or (NBf >= MaxBF);
  end;

procedure ClearMem;
  var
    i: byte;
  begin
    for i := 1 to NBf do
      FreeMem(Bf[i].Buf, Bf[i].Num);
    NBf := 0;
  end;
{DataCompBoy
 procedure DskOn;
 begin
  if dr1<=NumFloppy then DiskOn(dr1);
  if dr2<=NumFloppy then DiskOn(dr2);
 end;
}
{-DataCompBoy-}
function LongCopy(Fn1: String): boolean;
  label ex, _Abort_;
  type
    OHdr = record
      Nm: array[1..23] of Char;
      XT: array[1..3] of Char;
      ln: longInt;
      end;
    NHdr = record
      Nm: array[1..23] of Char;
      ln: longInt;
      XT: String;
      end;

  const
    OldStdHdr: OHdr = (Nm: 'Navigator Long Copy '#13#10#26; XT:
      '   '; ln: 0);
    NewStdHdr: NHdr = (Nm: 'Navigator NLong Copy'#13#10#26; ln: 0; XT:
      '');
    NStr: String[40] = '123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

  var
    F1, F2: lFile;
    Cs, CD: longInt;
    LS: longInt;
    Tuda: boolean;
    i: byte;
    NumW: word;
    Count: byte;
    wx, wy: integer;
    wl: byte;
    D, Fn2: String;
    n, X: String;
    nhd: NHdr;
    ohd: OHdr absolute nhd;
    PInfo: PWhileView;
    CopyCancelled: boolean;
    BinarySplit: boolean;
    NewMode: boolean;
    DiskVerify: boolean;
    LastPos: longInt;
    Olddate: longInt;
    AlarmTimer: TEventTimer;
    PartSize: longInt;
    PartWrt: longInt;

  procedure DsplInfo;
    begin
      PInfo^.Write(5, GetString(dlRead)+
      Copy(Strg(#219, (longInt(wl)*Cs) div LS)+Strg(#177, wl), 1, wl));
      PInfo^.Write(6, GetString(dlWrite)+
      Copy(Strg(#219, (longInt(wl)*CD) div LS)+Strg(#177, wl), 1, wl));
    end;

  procedure MaxRead;
    var
      i: byte;
      B: boolean;
    begin
      B := True;
      DsplInfo;
      for i := 1 to NBf do
        begin
          {DataCompBoy      DskOn;}
          if B then
            BlockRead(F1.F, Bf[i].Buf^, Bf[i].Num, Bf[i].NR)
          else
            Bf[i].NR := 0;
          if IOResult <> 0 then
            begin
              MessageBox(GetString(dlFBBNotReadSource)+Fn1, nil,
                mfError+mfOKButton);
              Abort := True;
              NewTimer(Timer, 0);
              NewTimerSecs(AlarmTimer, 20);
            end;
          {DataCompBoy      DskOn;}
          DispatchEvents(PInfo, CopyCancelled);
          if Abort or CopyCancelled then
            begin
              Close(F2.F);
              lEraseFile(F2);
              exit;
            end;
          Bf[i].NW := 0;
          Inc(Cs, Bf[i].NR);
          DsplInfo;
          B := Bf[i].NR <> 0;
        end;
    end { MaxRead };

  function OverWr: boolean;
    var
      SR: lSearchRec;
      lF: lFile;
    begin
      FreeSpc := Disk_Free(byte(D[1])-64);
      DOSError := 0;
      OverWr := True;
      Abort := False;
      lFindFirst(D+n+'.d??', AnyFile, SR); {JO}
      while (DOSError = 0) and not (SR.FullName[Length(SR.FullName)] in
          ['0'..'9'])
        and not (SR.FullName[Length(SR.FullName)-1] in ['0'..'9'])
      do
        lFindNext(SR);
      if (DOSError = 0) and ((PartSize = 0) or (FreeSpc < PartSize))
      then
        begin
          i := MessageBox(GetString(dlFBBOver1)+SR.FullName+
            GetString(dlFBBOver2), nil,
          mfQuery+mfYesButton+mfNextDButton+mfCancelButton);
          NewTimer(Timer, 0);
          NewTimerSecs(AlarmTimer, 20);
          Abort := i = cmCancel;
          if i = cmYes
          then
            EraseFile(D+SR.FullName)
          else
            OverWr := i <> cmCancel;
        end;
      lFindClose(SR);
    end { OverWr: };

  procedure RequireDisk;
    begin
      Abort := MessageBox(GetString(dlFBBInsertDisk)+SStr(Count, 2,
        '0'), nil,
      mfConfirmation+mfOKButton+mfCancelButton) <> cmOK;
      NewTimer(Timer, 0);
      NewTimerSecs(AlarmTimer, 20);
    end;

  procedure RequireNew;
    label 1;
    begin
      PartWrt := 0;
      Inc(Count); {DataCompBoy DskOn;}
1:
      repeat
        if Count > 1 then
          begin
            SetFTime(F2.F, Olddate);
            Close(F2.F);
            ClrIO;
            FreeSpc := Disk_Free(byte(D[1])-64);
            if (PartSize = 0) or (FreeSpc < PartSize) then
                RequireDisk;
          end;
        lFSplit(Fn2, D, n, X);
        Fn2 := D+n+'.d'+SStr(Count, 2, '0');
      until OverWr or Abort or CopyCancelled;
      if Abort or CopyCancelled then
        exit;
      FreeSpc := Disk_Free(byte(D[1])-64);
      while (FreeSpc < 512) do
        begin
          i := MessageBox(GetString(dlFBBDiskFull1), nil, mfWarning+
            mfOKButton+mfCancelButton);
          NewTimer(Timer, 0);
          FreeSpc := Disk_Free(byte(D[1])-64);
          if i <> cmOK then
            begin
              Abort := True;
              exit;
            end;
          {DataCompBoy     DskOn;}
        end;
      {DataCompBoy   DskOn;}
      CreateDirInheritance(D, False);
      lAssignFile(F2, Fn2);
      PInfo^.Write(3, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(
        Fn2));
      ClrIO;
      lReWriteFile(F2, 1);
      if not BinarySplit then
        if not NewMode then
          BlockWrite(F2.F, ohd, SizeOf(OHdr))
        else
          BlockWrite(F2.F, nhd, SizeOf(NHdr)-254+Length(nhd.XT));
      {DataCompBoy   DskOn;}
      if IOResult <> 0 then
        begin
          i := MessageBox(GetString(dlFBBNoCreate)+SStr(Count, 2,
            '0'), nil,
          mfError+mfOKButton+mfCancelButton);
          NewTimer(Timer, 0);
          NewTimerSecs(AlarmTimer, 20);
          if i <> cmOK then
            begin
              Abort := True;
              exit;
            end;
          goto 1;
        end;
    end { RequireNew };

  procedure MaxWrite;
    label Rep, 1, 2, 33;
    var
      i: byte;
      wr: longInt;
    begin
Rep:
      if Abort or CopyCancelled then
        exit;
      for i := 1 to NBf do
        if Bf[i].NW < Bf[i].NR then
          begin
            {DataCompBoy      DskOn;}
            wr := longInt(Bf[i].NR)-longInt(Bf[i].NW);
2:
            LastPos := FilePos(F2.F);
            if PartSize <> 0
            then
              BlockWrite(F2.F, Bf[i].Buf^[Bf[i].NW+1], Min(wr,
                PartSize-PartWrt), NumW)
            else
              BlockWrite(F2.F, Bf[i].Buf^[Bf[i].NW+1], word(wr),
                NumW);
            if DiskVerify then
              begin
                {$IFDEF OS_DOS}
                asm
             mov ah,0dh
             int 21h
           end;
                {$ENDIF}
                Seek(F2.F, LastPos);
                if PartSize <> 0
                then
                  BlockRead(F2.F, Bf[i].Buf^[Bf[i].NW+1], Min(wr,
                    PartSize-PartWrt), NumW)
                else
                  BlockRead(F2.F, Bf[i].Buf^[Bf[i].NW+1], word(wr),
                    NumW);
              end;
            if (IOResult <> 0) then
              begin
                MessageBox(GetString(dlFBBNoWrite)+Fn2, nil, mfError+
                  mfOKButton);
                Abort := True;
                NewTimer(Timer, 0);
              end;
            if Abort then
              goto 33;
            {DataCompBoy      DskOn;}
            if (NumW = 0) or ((PartSize <> 0) and (PartWrt =
                PartSize))
            then
              begin
                {Cat:warn}
                wr := DiskFree(dr2);
                if (wr = 0) or (PartWrt = PartSize) then
                  goto 1
                else
                  goto 2;
              end;
            Inc(Bf[i].NW, NumW);
            Inc(CD, NumW);
            Inc(PartWrt, NumW);
            DsplInfo;
            DispatchEvents(PInfo, CopyCancelled);
            if Abort or CopyCancelled then
              begin
33:
                CopyCancelled := True;
                Close(F2.F);
                lEraseFile(F2);
                exit;
              end;
1:
            if (NumW = 0) or ((PartWrt = PartSize) and (PartSize <> 0))
            then
              if Tuda then
                begin
                  MessageBox(GetString(dlFBBDiskFull2), nil, mfError+
                    mfOKButton);
                  NewTimer(Timer, 0);
                  Abort := True;
                  exit;
                end
              else
                begin
                  if TimerExpired(AlarmTimer) and
                    (FMSetup.Options and fmoBeep <> 0)
                  then
                    BeepAftercopy;
                  RequireNew;
                  goto Rep;
                end;
            if Bf[i].NR > Bf[i].NW then
              Dec(i);
          end;
    end { MaxWrite };

  function CheckSvoi: byte;
    var
      B: byte;
    begin
      B := 0;
      FillChar(nhd, SizeOf(NHdr), 0);
      Seek(F1.F, 0);
      BlockRead(F1.F, nhd, SizeOf(NHdr));
      if IOResult = 0 then
        begin
          if ohd.Nm = OldStdHdr.Nm then
            B := 1
          else if nhd.Nm = NewStdHdr.Nm then
            B := 2
          else
            B := 0;
        end
      else
        B := 0;
      case B of
        1:
          Seek(F1.F, SizeOf(OHdr));
        2:
          Seek(F1.F, SizeOf(NHdr)-254+Length(nhd.XT));
      end {case};
      CheckSvoi := B;
    end { CheckSvoi: };

  procedure RequirePath;
    var
      D: PDialog;
      V: PLabel;
      s, k, l: String;
    begin
      D := PDialog(LoadResource(dlgNextSection));
      V := PLabel(D^.FirstThat(@IsLabel));
      s := V^.text^;
      DisposeStr(V^.text);
      Replace('%D', ItoS(Count), s);
      V^.text := NewStr(s);
      lFSplit(Fn1, s, k, l);
      D^.SetData(s);
      if Desktop^.ExecView(D) = cmCancel then
        begin
          Dispose(D, Done);
          Abort := True;
          exit
        end;
      Abort := False;
      D^.GetData(s);
      Dispose(D, Done);
      Fn1 := MakeNormName(s, k+l);
      NewTimer(Timer, 0);
      NewTimerSecs(AlarmTimer, 20);
    end { RequirePath };

  procedure InsCor;
    var
      s, j: byte;
    begin
      Inc(Count);
      j := 0;
      repeat
        Close(F1.F);
        if (Count > 1) and (j > 0) then
          begin
            ClrIO;
            RequirePath;
            if Abort then
              exit;
          end;
        Inc(j);
        ClrIO;
        lFSplit(Fn1, D, n, X);
        Fn1 := D+n+'.d'+SStr(Count, 2, '0');
        FileMode := 0;
        lAssignFile(F1, Fn1);
        lResetFile(F1, 1);
        PInfo^.Write(1, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(
          Fn1));
        s := CheckSvoi;
      until ((IOResult = 0) and ((s = 1) or (s = 2))) or Abort or
        CopyCancelled;
    end { InsCor };

  procedure Nedochit;
    var
      i: byte;
      B: boolean;
    begin
      B := False;
      for i := 1 to NBf do
        B := B or (Bf[i].NR = 0);
      if B and (CD < LS) then
        InsCor;
    end;

  var
    CopyDir: String;

  function CopyDialog: boolean;
    var
      D: PDialog;
      P: PView;
      R: TRect;
      s, Mask: String;
      SR: lSearchRec;
      i: integer;
      l: longInt;
      F: lFile;
      DTA: record
        s: String;
        W: word;
        SPLSiz, NumSect: String[10]
        end;
      A, aa, aaa: String;

    const
      x_x = '*.';
    begin
      lFSplit(Fn1, A, aa, aaa);
      CopyDialog := False;
      if Tuda
      then
        D := PDialog(LoadResource(dlgCombineFile))
      else
        D := PDialog(LoadResource(dlgSplitFile));
      P := D^.LastThat(@IsLabel);
      R.Assign(P^.Origin.X+P^.Size.X-1, P^.Origin.Y, (D^.Size.X-1), P^.
        Origin.Y+1);
      D^.Insert(New(PLabel, Init(R, '~'+LowStrg(
        {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(aa+aaa))+'~'+
      GetString(dlSplit_to), PLabel(P)^.Link)));
      s := '';
      HistoryAdd(hsFBBCopy, lFExpand(A));
      GlobalMessage(evCommand, cmPushFirstName, @S);
      GlobalMessageL(evCommand, cmPushName, hsFBBCopy);
      if s[2] <> ':' then
        s := '';
      if s = '' then
        s := HistoryStr(hsFBBCopy, 0);
      DTA.s := {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(s);
      {    DTA.W := 0; }
      DTA.SPLSiz := '';
      DTA.NumSect := '';

      i := byte(UpCase(s[1]));
      if i in [65..90] then
        begin
          if (SystemData.Drives[Char(i)] and 16) > 0 then
            DTA.W := 2 or 4
          else
            DTA.W := 0 or 4;
        end;

      D^.SetData(DTA);
      if Desktop^.ExecView(D) = cmCancel then
        begin
          Dispose(D, Done);
          exit
        end;
      D^.GetData(DTA);
      BinarySplit := DTA.W and 1 <> 0;
      DiskVerify := DTA.W and 2 <> 0;
      if not Tuda then
        begin
          NewMode := DTA.W and 4 <> 0;
          Val(DTA.SPLSiz, PartSize, i);
          if i <> 0 then
            begin
              PartSize := 0;
              Val(DTA.NumSect, PartSize, i);
              if (i = 0) and (PartSize > 0) then
                begin
                  ClrIO;
                  lAssignFile(F, Fn1);
                  lResetFile(F, 1);
                  l := FileSize(F.F);
                  PartSize := (l div PartSize)+byte((l mod PartSize) <>
                    0);
                  Close(F.F);
                end;
            end
          else
            begin
              if BinarySplit
              then
                if PartSize < 0
                then
                  PartSize := 0
                else
              else if NewMode
              then
                if PartSize <= SizeOf(NHdr)-253+Length(aaa)
                then
                  PartSize := 0
                else
                  Dec(PartSize, SizeOf(NHdr)-253+Length(aaa))
              else if PartSize <= SizeOf(OHdr)
              then
                PartSize := 0
              else
                Dec(PartSize, SizeOf(OHdr));
            end;
        end
      else
        PartSize := 0;
      s := {$IFDEF RecodeWhenDraw}OemToCharStr {$ENDIF}(DTA.s);
      Dispose(D, Done);
      if s = '' then
        s := '.';
      s := lFExpand(s);
      if IsDir(s) then
        if s[Length(s)] <> '\' then
          s := s+'\';
      if s[Length(s)] = '\' then
        begin
          CopyDir := s;
          Mask := x_x
        end
      else if (Pos('*', s) = 0) and (Pos('?', s) = 0) then
        begin
          DOSError := 0;
          lFindFirst(s+'\*.*', $3E {$IFNDEF OS2} xor VolumeID
            {$ENDIF}, SR); {JO}
          lFindClose(SR);
          if Abort or CopyCancelled then
            exit;
          if (DOSError = 0)
          then
            begin
              CopyDir := s+'\';
              Mask := x_x
            end
          else
            begin
              lFSplit(s, CopyDir, n, X);
              Mask := n+X
            end
        end
      else
        begin
          lFSplit(s, CopyDir, n, X);
          Mask := n+X
        end;
      Fn1 := lFExpand(Fn1);
      lFSplit(Fn1, s, n, X);
      s := n+X;
      Fn2 := MakeNormName(CopyDir, MkName(s, Mask));
      CopyDialog := True;
      s := Copy(GetExt(Fn1), 2, MaxStringLength);
      if Tuda then
        begin
          if GetExt(Fn2) = '.' then
            if NewMode then
              if Fn2[Length(Fn2)] = '.' then
                Fn2 := Fn2+nhd.XT
              else
                Fn2 := Fn2+'.'+nhd.XT
            else if Fn2[Length(Fn2)] = '.' then
              Fn2 := Fn2+ohd.XT
            else
              Fn2 := Fn2+'.'+ohd.XT;
        end
      else if NewMode then
        begin
          nhd.XT := s;
          nhd.Nm := NewStdHdr.Nm;
        end
      else
        begin
          Move(s[1], ohd.XT, 3);
          ohd.Nm := OldStdHdr.Nm;
        end;
    end { CopyDialog: };

  var
    R: TRect;
    PP: Pointer;
    rc: longInt;
    csv: byte;
  begin { LongCopy }
    NewTimer(Timer, 0);
    LongCopy := True;
    Count := 0;
    Tuda := False;
    Abort := False;
    CopyCancelled := False;
    BinarySplit := False;
    NewMode := True;
    Fn1 := lFExpand(Fn1);
    lFSplit(Fn1, D, n, X);
    dr1 := byte(Fn1[1])-64;
    Cs := 0;
    CD := 0; {DataCompBoy DskOn;}
    FileMode := $40;
    lAssignFile(F1, Fn1);
    ClrIO;
    lResetFile(F1, 1);
    rc := IOResult;
    if rc <> 0 then
      begin
        MessFileNotOpen(Fn1, rc);
        LongCopy := False;
        exit;
      end;
    GetFtime(F1.F, Olddate);
    lFSplit(Fn1, D, n, X);

    Tuda := (Length(X) = 4) and (UpStrg(X[2]) = 'D') and (StoI(Copy(X, 3
      , 2)) > 0);
    csv := CheckSvoi;
    if Tuda and (csv = 0) then
      begin
        Close(F1.F);
        Message(Desktop, evCommand, cmSingleCopy, nil);
        LongCopy := False;
        exit;
      end;

    if Tuda then
      Count := 1;
    if Tuda and ((X[4] <> '1') or (X[3] <> '0')) then
      begin
        X[3] := '0';
        X[4] := '1';
        Fn1 := D+n+X;
        repeat
          Close(F1.F);
          ClrIO;
          Count := 1;
          RequirePath;
          lAssignFile(F1, Fn1);
          if Abort then
            begin
              LongCopy := False;
              exit;
            end;
          FileMode := 0;
          lResetFile(F1, 1);
          csv := CheckSvoi;
        until (IOResult = 0) and (csv <> 0);
      end;
    if csv = 1 then
      NewMode := False
    else
      NewMode := True;
    if not Tuda then
      begin
        LS := FileSize(F1.F);
        Seek(F1.F, 0);
      end
    else if NewMode then
      LS := nhd.ln
    else
      LS := ohd.ln;

    if (not CopyDialog) or Abort then
_Abort_:
      begin
        Close(F1.F);
        LongCopy := False;
        exit;
      end;

    if NewMode then
      nhd.ln := LS
    else
      ohd.ln := LS;

    Fn2 := lFExpand(Fn2);
    if Abort then
      goto _Abort_;
    dr2 := byte(Fn2[1])-64;
    if Fn1 = Fn2 then
      goto _Abort_;

    R.Assign(1, 1, 36, 13);
    New(PInfo, Init(R));
    if PInfo = nil then
      goto _Abort_;
    if Tuda then
      PInfo^.Top := GetString(dlFBBDEFragment)
    else
      PInfo^.Top := GetString(dlFBBFragment);
    PInfo^.Bottom := '';
    PInfo^.SetState(sfShadow, True);
    Desktop^.Insert(PInfo);
    PInfo^.Write(1, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Fn1));
    lFSplit(Fn2, D, n, X);
    if Tuda and (UpCase(X[2]) = 'D') and (StoI(Copy(X, 3, 2)) > 0)
    then
      case csv of
        1:
          Fn2 := D+n+'.'+ohd.XT;
        2:
          Fn2 := D+n+'.'+nhd.XT;
      end {case};
    lAssignFile(F2, Fn2);
    ClrIO;
    PInfo^.Write(2, GetString(dlFBBFragmentTo));
    PInfo^.Write(3, {$IFDEF RecodeWhenDraw}CharToOemStr {$ENDIF}(Fn2));
    wl := PInfo^.Size.X-8;
    if not Tuda then
      RequireNew
    else
      begin
        CreateDirInheritance(D, False);
        lResetFile(F2, 1);
        if IOResult = 0 then
          begin
            PP := @Fn2;
            if MessageBox(GetString(dlFileExist)+^M, @PP, mfQuery+
                mfYesButton+mfNoButton+mfCancelButton)
              <> cmYes
            then
              begin
                Desktop^.Delete(PInfo);
                Dispose(PInfo, Done);
                Close(F2.F);
                goto _Abort_;
              end;
          end;
        ClrIO;
        lReWriteFile(F2, 1);
        if (IOResult <> 0) or Abort then
          begin
            MessageBox(GetString(erCantCreateFile)+Fn2, nil, mfError+
              mfOKButton);
            Desktop^.Delete(PInfo);
            Dispose(PInfo, Done);
            goto _Abort_;
          end;
      end;
    GetMaxMem;
    if NBf = 0 then
      begin
        Close(F2.F);
        lEraseFile(F2);
        goto _Abort_;
      end;
    Inc(SkyEnabled);
    PartWrt := 0;
    repeat
      MaxRead;
      MaxWrite;
      if Tuda then
        Nedochit;
    until (CD >= LS) or Abort or CopyCancelled;
    Dec(SkyEnabled);
ex:
    Desktop^.Delete(PInfo);
    Dispose(PInfo, Done);
    Close(F1.F);
    SetFTime(F2.F, Olddate);
    Close(F2.F);
    if (Abort or CopyCancelled) and Tuda then
      lEraseFile(F2);
    ClearMem;
    Abort := False;
    if not DnIni.AutoRefreshPanels then
      begin
        GlobalMessage(evCommand, cmPanelReread, @CopyDir);
        {GlobalMessage(evCommand, cmRereadInfo, nil);}
        GlobalMessage(evCommand, cmRereadTree, @CopyDir);
      end;
    if not (Abort or CopyCancelled) then

      if (FMSetup.Options and fmoBeep <> 0) and
        (ElapsedTimeInSecs(Timer) > 20)
      then
        BeepAftercopy;
  end { LongCopy };
{-DataCompBoy-}

end.
