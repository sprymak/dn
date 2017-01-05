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

UNIT FBB;

interface

uses
  Dos, tree;

 Function LongCopy(Fn1: String): Boolean; {DataCompBoy}

implementation
uses LFN, {DataCompBoy}
     advance, advance1, advance2, advance3, FileCopy, DNApp, Messages, Views,
     Objects, Dialogs, Commands, Drivers, Memory, FilesCol, RStrings,
     HistList, Gauge, StartUp, DNutil, xTime, DiskInfo
     {$IFNDEF NONBP},DiskTool{$ENDIF}
     , ErrMess
     ;

 type Ar65000 = Array [1..65000] of Byte;
      ArPtr = ^Ar65000;

      HRec = record
       Num : Word;
       NR  : Word;
       NW  : Word;
       Buf : ArPtr;
      end;

 {$IFDEF DPMI}
 Const MaxBF=50;
 {$ELSE}
 Const MaxBF=20;
 {$ENDIF}
 var Bf      : Array [1..MaxBF] of HRec;
     dr1,dr2 : Byte;
     FreeSpc : TSize;
     Timer: TEventTimer;
 const NBf   : Byte = 0;

 function MaxAvail: LongInt;
 begin
   MaxAvail := MemAdjust(System.MaxAvail);
 end;

 procedure GetMaxMem;
  var l : LongInt;
 begin
  NBf:=0;
  repeat
   l:=MaxAvail-15000;
   if l>0 then
    begin
     Inc(NBf);
     if l>65000 then l:=65000;
     Bf[NBF].Buf := MemAlloc(l);
     Bf[NBf].Num:=l;
     if Bf[NBF].Buf = nil then Dec(NBF);
    end;
  until (MaxAvail<16000) or LowMemory or (NBf>=MaxBf);
 end;

 procedure ClearMem;
  var i : Byte;
 begin
  for i:=1 to NBf do
   FreeMem(Bf[i].Buf,Bf[i].Num);
  NBf:=0;
 end;
{DataCompBoy
 procedure DskOn;
 begin
  if dr1<=NumFloppy then DiskOn(dr1);
  if dr2<=NumFloppy then DiskOn(dr2);
 end;
}
        {-DataCompBoy-}
 Function LongCopy(Fn1: String): Boolean;
  label Ex, _Abort_;
  type OHdr=record
        nm : array[1..23] of Char;
        xt : array[1..3] of Char;
        ln : LongInt;
       end;
       NHdr=record
        nm : array[1..23] of Char;
        ln : LongInt;
        xt : String;
       end;

  const OldStdHdr : OHdr=(nm:'Navigator Long Copy '#13#10#26;xt:'   ';ln:0);
        NewStdHdr : NHdr=(nm:'Navigator NLong Copy'#13#10#26;ln:0    ;xt:'');
        NStr : String[40]='123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

  var F1,F2 : lFile;
      Cs,Cd : LongInt;
      Ls    : LongInt;
      Tuda  : Boolean;
      i     : Byte;
      NumW  : Word;
      Count : Byte;
      wx,wy : Integer;
      wl    : Byte;
      d,Fn2 : String;
      n,x    : String;
      nhd    : nHdr;
      ohd    : oHdr absolute nhd;
      PInfo : PWhileView;
      CopyCancelled: Boolean;
      BinarySplit: Boolean;
      NewMode  : Boolean;
      DiskVerify: Boolean;
      LastPos: LongInt;
      Olddate: LongInt;
      AlarmTimer: TEventTimer;
      PartSize: LongInt;
      PartWrt:  LongInt;

  procedure DsplInfo;
  begin
    PInfo^.Write(5, GetString(dlRead)+
                 Copy(Strg(#219,(LongInt(wl)*Cs) div Ls)+Strg(#177, wl),1,wl));
    PInfo^.Write(6, GetString(dlWrite)+
                 Copy(Strg(#219,(LongInt(wl)*Cd) div Ls)+Strg(#177, wl),1,wl));
  end;

  procedure MaxRead;
   var  i : Byte;
        b : Boolean;
  begin
    b:=On;
    DsplInfo;
    for i:=1 to NBf do
     begin
{DataCompBoy      DskOn;}
      if b then BlockRead(F1.f,Bf[i].Buf^,Bf[i].Num,Bf[i].NR)
           else Bf[i].Nr:=0;
      if IOResult <> 0 then
       begin
        MessageBox(GetString(dlFBBNotReadSource)+Fn1, nil, mfError+mfOKButton);
        Abort := On;
        NewTimer(Timer, 0);
        NewTimerSecs(AlarmTimer, 20);
       end;
{DataCompBoy      DskOn;}
      DispatchEvents(PInfo, CopyCancelled);
      if Abort or CopyCancelled then begin
       Close(F2.f);
       lEraseFile(F2);
       Exit;
      end;
      Bf[i].NW:=0;
      Inc(Cs,Bf[i].NR);
      DsplInfo;
      b:=Bf[i].NR<>0;
     end;
  end;

  function OverWr : Boolean;
   var SR : lSearchRec;
       lF  : lFile;
  begin
   FreeSpc:=Disk_Free(Byte(d[1])-64);
   DosError:=0; OverWr:=On; Abort := Off;
   lFindFirst(d+n+'.d??',Anyfile {$IFNDEF OS2}XOR VolumeID{$ENDIF},SR);             {JO}
   while (DOSError=0) and not (SR.FullName[Length(SR.FullName)] in ['0'..'9'])
   and not (SR.FullName[Length(SR.FullName)-1] in ['0'..'9']) do lFindNext(SR);
   if (DOSError=0) and ((PartSize=0) or (FreeSpc<PartSize)) then
    begin
     i:=MessageBox(GetString(dlFBBOver1) + SR.FullName + GetString(dlFBBOver2), nil,
                   mfQuery+mfYesButton+mfNextDButton+mfCancelButton);
     NewTimer(Timer, 0);
     NewTimerSecs(AlarmTimer, 20);
     Abort:=i=cmCancel;
     if i=cmYes
       then EraseFile( d+SR.fullname )
       else OverWr:= I <> cmCancel;
    end;
   lFindClose(SR);
  end;

  procedure RequireDisk;
  begin
   Abort:=MessageBox(GetString(dlFBBInsertDisk)+SStr(Count,2,'0'), nil,
                     mfConfirmation + mfOKButton + mfCancelButton)<>cmOK;
   NewTimer(Timer, 0);
   NewTimerSecs(AlarmTimer, 20);
  end;

  procedure RequireNew;
   label 1;
  begin
   PartWrt := 0;
   Inc(Count);{DataCompBoy DskOn;}
1: repeat
    if Count>1 then
    begin
     SetFTime(F2.F, OldDate);
     Close(F2.F);ClrIO;
     FreeSpc:=Disk_Free(Byte(d[1])-64);
     If (PartSize=0) or (FreeSpc<PartSize) then RequireDisk;
    end;
    lFSplit(Fn2,d,n,x);
    Fn2:=d+n+'.d'+SStr(Count,2,'0');
   Until OverWr or Abort or CopyCancelled;
   if Abort or CopyCancelled then Exit;
   FreeSpc:=Disk_Free(Byte(d[1])-64);
   While (FreeSpc<512) do
    begin
     i:=MessageBox(GetString(dlFBBDiskFull1),nil,mfWarning + mfOKButton + mfCancelButton);
     NewTimer(Timer, 0);
     FreeSpc:=Disk_Free(Byte(d[1])-64);
     if i<>cmOK then
       begin
        Abort:=On;
        Exit;
       end;
{DataCompBoy     DskOn;}
    end;
{DataCompBoy   DskOn;}
   CreateDirInheritance(d, Off);
   lAssignFile(F2,Fn2); PInfo^.Write(3,  {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(Fn2));
   ClrIO; lReWriteFile(F2,1);
   if not BinarySplit then
    if Not NewMode then BlockWrite(F2.f,ohd,SizeOf(OHdr))
                   else BlockWrite(F2.f,nhd,SizeOf(NHdr)-254+length(nhd.xt));
{DataCompBoy   DskOn;}
   if IOResult<>0 then
    begin
    i:=MessageBox(GetString(dlFBBNoCreate)+SStr(Count,2,'0'), nil,
                  mfError + mfOKButton + mfCancelButton);
    NewTimer(Timer, 0);
    NewTimerSecs(AlarmTimer, 20);
    if i<>cmOK then
       begin
        Abort:=On;
        Exit;
       end;
     GoTo 1;
    end;
  end;

  procedure MaxWrite;
   label Rep,1,2, 33;
   var i  : Byte;
       wr : LongInt;
  begin
   Rep:
    if Abort or CopyCancelled then Exit;
    for i:=1 to NBf do
     if BF[i].NW<BF[i].NR then
     begin
{DataCompBoy      DskOn;}
      wr:=LongInt(Bf[i].NR)-LongInt(Bf[i].NW);
2:    LastPos := FilePos(F2.f);
      if PartSize<>0
       then BlockWrite(F2.f,Bf[i].Buf^[Bf[i].NW+1], Min(wr, PartSize-PartWrt), NumW)
       else BlockWrite(F2.f,Bf[i].Buf^[Bf[i].NW+1], Word(wr), NumW);
      if DiskVerify then
        begin
           {$IFDEF OS_DOS}
           asm
             mov ah,0dh
             int 21h
           end;
           {$ENDIF}
           Seek(F2.f, LastPos);
           if PartSize<>0
            then BlockRead(F2.f,Bf[i].Buf^[Bf[i].NW+1],Min(wr, PartSize-PartWrt),NumW)
            else BlockRead(F2.f,Bf[i].Buf^[Bf[i].NW+1],Word(wr),NumW);
        end;
      if (IOResult <> 0) then
       begin
        MessageBox(GetString(dlFBBNoWrite)+Fn2, nil, mfError+mfOKButton);
        Abort := On;
        NewTimer(Timer, 0);
       end;
      if Abort then Goto 33;
{DataCompBoy      DskOn;}
      if (NumW=0) or ((PartSize<>0) and (PartWrt=PartSize)) then
       begin
{Cat:warn}
        wr:=DiskFree(dr2);
        if (wr=0) or (PartWrt=PartSize) then GoTo 1 else GoTo 2;
       end;
      Inc(Bf[i].NW, NumW);
      Inc(Cd, NumW);
      Inc(PartWrt, NumW);
      DsplInfo;
      DispatchEvents(PInfo, CopyCancelled);
      if Abort or CopyCancelled then
         begin
33:       CopyCancelled := On;
          Close(F2.f);
          lEraseFile(F2);
          Exit;
         end;
1:    if (NumW=0) or ((PartWrt=PartSize) and (PartSize<>0)) then
       if Tuda then begin
        MessageBox(GetString(dlFBBDiskFull2), nil, mfError+mfOKButton);
        NewTimer(Timer, 0);
        Abort:=On;
        Exit;
       end else begin
        if TimerExpired(AlarmTimer) and
           (FMSetup.Options and fmoBeep <> 0) then
         BeepAftercopy;
        ReQuireNew;
        GoTo Rep;
       end;
      if Bf[i].NR>Bf[i].NW then Dec(i);
     end;
  end;

  function CheckSvoi : Byte;
  var b: byte;
  begin
   b:=0;
   FillChar(nhd,SizeOf(nHdr),0);
   Seek(F1.f, 0);
   BlockRead(F1.f,nhd,SizeOf(NHdr));
   if IOResult=0 then
    begin
     if ohd.nm=OldStdHdr.nm then b:=1
      else if nhd.nm=NewStdHdr.nm then b:=2
       else b:=0;
    end
   else b:=0;
   Case b of
    1: Seek(F1.f,SizeOf(OHdr));
    2: Seek(F1.f,SizeOf(NHdr)-254+Length(nhd.xt));
   end;
   CheckSvoi:=b;
  end;

  procedure RequirePath;
   var D: PDialog;
       V: PLabel;
       S, K, L: String;
    function IsLabel( P: PView ): boolean; {$IFDEF BIT_16}far;{$ENDIF}
    begin
      IsLabel := TypeOf( P^ ) = TypeOf( TLabel );
    end;
  begin
   D := PDialog( LoadResource( dlgNextSection ));
   V := PLabel(D^.FirstThat(@IsLabel));
   S := V^.Text^; DisposeStr(V^.Text);
   Replace('%D', ItoS(Count), S);
   V^.Text := NewStr(S);
   lFSplit(Fn1, S, K, L);
   D^.SetData(S);
   if Desktop^.ExecView(D) = cmCancel then
    begin Dispose(D,Done); Abort := True; Exit end;
   Abort := False;
   D^.GetData(S);
   Dispose(D,Done);
   Fn1:=MakeNormName(S, K+L);
   NewTimer(Timer, 0);
   NewTimerSecs(AlarmTimer, 20);
  end;

  procedure InsCor;
  var s,j: Byte;
  begin
   Inc(Count);
   j:=0;
   repeat
    Close(F1.f);
    if (Count>1) and (j>0) then begin
     ClrIO;
     RequirePath;
     if Abort then Exit;
    end;
    inc(j);
    ClrIO;
    lFSplit(Fn1,d,n,x);
    Fn1:=d+n+'.d'+SStr(Count, 2, '0');
    FileMode := 0;
    lAssignFile(F1,Fn1); lResetFile(F1,1); PInfo^.Write(1,  {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(Fn1));
    s:=CheckSvoi;
   until ((IOResult=0) and ((s=1) or (s=2))) or Abort or CopyCancelled;
  end;

  procedure Nedochit;
   var i : Byte;
       b : Boolean;
  begin
   b:=Off;
   for i:=1 to NBF do b:=b or (BF[i].NR=0);
   if b and (cd<ls) then InsCor;
  end;

  var CopyDir: String;

  function CopyDialog: Boolean;
   var D: PDialog;
       P: PView;
       R: TRect;
       S,Mask: String;
       SR: lSearchRec;
       I: Integer;
       L: Longint;
       F: lFile;
       DTA: record S: String; W: Word; SPLSiz, NumSect: String[10] end;
       a,aa,aaa:string;

   const x_x = '*.';
    function IsLabel( P: PView ): boolean; {$IFDEF BIT_16}far;{$ENDIF}
    begin
      IsLabel := TypeOf( P^ ) = TypeOf( TLabel );
    end;
  begin
    lFSplit(Fn1, a, aa, aaa);
    CopyDialog := Off;
    If Tuda
      then D := PDialog( LoadResource( dlgCombineFile ))
      else D := PDialog( LoadResource( dlgSplitFile ));
    P := D^.LastThat( @IsLabel );
    R.Assign(P^.Origin.X + P^.Size.X-1, P^.Origin.Y, (D^.Size.X - 1), P^.Origin.Y + 1);
    D^.Insert( New( PLabel, Init( R, '~'+LowStrg({$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(aa+aaa))+'~'+
               GetString(dlSplit_To), PLabel(P)^.Link)));
    S := '';
    HistoryAdd(hsFBBCopy, lFExpand(a));
    GlobalMessage(evCommand, cmPushFirstName, @S);
    GlobalMessageL(evCommand, cmPushName, hsFBBCopy);
    if S[2] <> ':' then S := '';
    if S = '' then S := HistoryStr(hsFBBCopy, 0);
    DTA.S := {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(S);
{    DTA.W := 0; }
    DTA.SPLSiz  := '';
    DTA.NumSect := '';

        i := Byte(UpCase(s[1]));
        if i in [65..90] then
         begin
           if (SystemData.Drives[Char(i)] and 16) > 0 then
              DTA.W := 2 or 4  else DTA.W := 0 or 4 ;
         end;


    D^.SetData(DTA);
    if Desktop^.ExecView(D) = cmCancel then begin Dispose(D,Done); Exit end;
    D^.GetData(DTA);
    BinarySplit := DTA.W and 1 <> 0;
    DiskVerify := DTA.W and 2 <> 0;
    If not Tuda then begin
     NewMode := DTA.W and 4<>0;
     Val(DTA.SPLSiz, PartSize, I);
     if I<>0 then begin
      PartSize:=0;
      Val(DTA.NumSect, PartSize, I);
      if (I=0) and (PartSize>0) then begin
       ClrIO;
       lAssignFile(F, Fn1);
       lResetFile(F, 1);
       L:=FileSize(F.F);
       PartSize := (L div PartSize) + Byte((L mod PartSize)<>0);
       Close(F.F);
      end;
     end else begin
      if BinarySplit
       then
        if PartSize<0
         then PartSize:=0
         else
       else
        if NewMode
         then if PartSize<=SizeOf(NHdr)-253+length(aaa)
               then PartSize:=0
               else Dec(PartSize, SizeOf(NHdr)-253+length(aaa))
         else if PartSize<=SizeOf(OHdr)
               then PartSize:=0
               else Dec(PartSize, SizeOf(OHdr));
     end;
    end else PartSize:=0;
    S := {$IFDEF RecodeWhenDraw}OemToCharStr{$ENDIF}(DTA.S);
    Dispose(D,Done);
    if S = '' then S:='.';
    S := lFExpand(S);
    If IsDir(S) then if S[Length(S)]<>'\' then S:=S+'\';
    if S[Length(S)] = '\' then begin
      CopyDir := S; Mask := x_x
    end else
    if (pos('*',S)=0) and (pos('?',S)=0) then
     begin
      DosError := 0;
      lFindFirst(S+'\*.*', $3E {$IFNDEF OS2}XOR VolumeID{$ENDIF}, SR); {JO}
      lFindClose(SR);
      if Abort or CopyCancelled then Exit;
      if (DOSError = 0)
        then begin CopyDir := S+'\'; Mask := x_x end
        else begin lFSplit(S,CopyDir,N,X); Mask:=N+X end
      end
    else begin
      lFSplit(S,CopyDir,N,X); Mask := N+X
    end;
    Fn1 := lFExpand(Fn1);
    lFSplit(Fn1, S, N, X);
    S := N + X;
    Fn2 := MakeNormName(CopyDir, MkName(S, Mask));
    CopyDialog := True;
    S :=Copy(GetExt(Fn1), 2, MaxStringLength);
    If Tuda then
     begin
      If GetExt(Fn2)='.' then
       If NewMode then if Fn2[Length(Fn2)]='.' then Fn2:=Fn2+nhd.xt else Fn2:=Fn2+'.'+nhd.xt
                  else if Fn2[Length(Fn2)]='.' then Fn2:=Fn2+ohd.xt else Fn2:=Fn2+'.'+ohd.xt;
     end
    else
     If NewMode then
      begin
       nhd.xt:=S;
       nhd.nm:=NewStdHdr.nm;
      end
     else
      begin
       move(s[1], ohd.xt, 3);
       ohd.nm:=OldStdHdr.nm;
      end;
  end;

  var R: TRect;
      PP: Pointer;
      RC: longint;
      csv: byte;
  begin
    NewTimer(Timer, 0);
    LongCopy := On;
    Count:=0;
    Tuda:=Off;
    Abort:=Off;
    CopyCancelled := False;
    BinarySplit := Off;
    NewMode := On;
    Fn1:=lFExpand(Fn1);
    lFSplit(Fn1,d,n,x);
    dr1:=Byte(Fn1[1])-64;
    Cs:=0; Cd:=0; {DataCompBoy DskOn;}
    FileMode := $40;
    lAssignFile(F1,Fn1); ClrIO;
    lResetFile(F1,1);
    RC := IOResult;
    if RC<>0 then
     begin MessFileNotOpen(Fn1, RC); LongCopy := Off; Exit; end;
    GetFTime(F1.F, OldDate);
    lFSplit(Fn1,d,n,x);

    Tuda := (Length(X) = 4) and (UpStrg(x[2])='D') and (StoI(Copy(x,3,2)) > 0);
    csv:=CheckSvoi;
    if Tuda and (csv=0) then
           begin
            Close(F1.f);
            Message(Desktop, evCommand, cmSingleCopy, nil);
            LongCopy := Off;
            Exit;
           end;

    if Tuda then Count:=1;
    if Tuda and ((x[4]<>'1') or (x[3]<>'0')) then
     begin
      x[3] := '0'; x[4] := '1'; Fn1 := d+n+x;
      repeat
       close(F1.f); ClrIO;
       Count := 1;
       RequirePath;
       lAssignFile(F1, Fn1);
       if Abort then begin LongCopy := Off; Exit; end;
       FileMode := 0;
       lResetFile(F1,1);
       csv := CheckSvoi;
      until (IOresult = 0) and (csv<>0);
     end;
    If csv=1 then NewMode:=false else NewMode:=True;
    if not Tuda then
      begin
       Ls:=FileSize(F1.F);
       Seek(F1.F, 0);
      end
    else
     if NewMode then Ls:=nhd.Ln
                else Ls:=ohd.Ln;

    if (not CopyDialog) or Abort then
_Abort_:    begin Close(F1.F); LongCopy := Off; Exit; end;

    if NewMode then nhd.Ln:=Ls
               else ohd.Ln:=Ls;

    Fn2:=lFExpand(Fn2);
    if Abort then Goto _Abort_;
    dr2:=Byte(Fn2[1])-64;
    if Fn1=Fn2 then Goto _Abort_;

    R.Assign(1,1,36,13);
    New(PInfo, Init(R));
    if PInfo = nil then Goto _Abort_;
    if Tuda then PInfo^.Top := GetString(dlFBBDeFragment)
            else PInfo^.Top := GetString(dlFBBFragment);
    PInfo^.Bottom := '';
    PInfo^.SetState(sfShadow, On);
    Desktop^.Insert(PInfo);
    PInfo^.Write(1, {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(Fn1));
    lFSplit(Fn2,d,n,x);
    if Tuda and (UpCase(x[2])='D') and (StoI(Copy(x,3,2)) > 0)  then
     case csv of
      1: Fn2 := d+n+'.'+ohd.xt;
      2: Fn2 := d+n+'.'+nhd.xt;
     end;
    lAssignFile(F2,Fn2);ClrIO;
    PInfo^.Write(2, GetString(dlFBBFragmentTo));
    PInfo^.Write(3, {$IFDEF RecodeWhenDraw}CharToOemStr{$ENDIF}(Fn2));
    wl := PInfo^.Size.X - 8;
    if not Tuda then RequireNew
                else
                 begin
                  CreateDirInheritance(d, Off);
                  lResetFile(F2,1);
                  if IOResult=0 then
                   begin
                   PP:=@Fn2;
                   if MessageBox(GetString(dlFileExist)+^M, @PP, mfQuery+mfYesButton+mfNoButton+mfCancelButton)
                    <> cmYes then
                       begin
                        Desktop^.Delete(PInfo);
                        Dispose(PInfo,Done);
                        Close(F2.F);
                        Goto _Abort_;
                       end;
                   end;
                  ClrIO;
                  lRewriteFile(F2,1);
                  if (IOResult<>0) or Abort then
                   begin
                    MessageBox(GetString(erCantCreateFile)+Fn2, nil, mfError + mfOKButton);
                    Desktop^.Delete(PInfo);
                    Dispose(PInfo,Done);
                    Goto _Abort_;
                   end;
                 end;
    GetMaxMem;
    if NBf=0 then begin Close(F2.F); lEraseFile(F2); Goto _Abort_; end;
    Inc(SkyEnabled);
    PartWrt := 0;
    Repeat
     MaxRead;
     MaxWrite;
     if Tuda then NedoChit;
    until (Cd>=Ls) or Abort or CopyCancelled;
    Dec(SkyEnabled);
  Ex:
    Desktop^.Delete(PInfo);
    Dispose(PInfo,Done);
    Close(F1.F);
    SetFTime(F2.F, OldDate);
    Close(F2.F);
    if (Abort or CopyCancelled) and Tuda then lEraseFile(F2);
    ClearMem;
    Abort := Off;
{$IFDEF FileNotify}
    if not DNIni.AutoRefreshPanels then
      begin
{$ENDIF}
        GlobalMessage(evCommand, cmPanelReread, @CopyDir);
       {GlobalMessage(evCommand, cmRereadInfo, nil);}
        GlobalMessage(evCommand, cmRereadTree, @CopyDir);
{$IFDEF FileNotify}
      end;
{$ENDIF}
    if not (Abort or CopyCancelled) then

    if (FMSetup.Options and fmoBeep <> 0) and
       (ElapsedTimeInSecs(Timer) > 20) then BeepAfterCopy;
  end;
        {-DataCompBoy-}

end.
