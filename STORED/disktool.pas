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

UNIT DISKTOOL;

interface

uses
{$IFNDEF NONBP}
  LFN, {DataCompBoy}   advance, advance1, advance2, advance3, Dos,
  Objects, Drivers;
{$ELSE}
  Objects;
{$ENDIF}

type

{$IFNDEF NONBP}
      FATRec = Array [0..64000] of Byte;

      ArBte  = Array[0..64000] of Byte;
{$ENDIF}

    PBootRec = ^TBootRec;
    TBootRec = Record
      Jmp      : Array [0..2] of Byte;
      OEM      : Array [0..7] of Char;
      ByteSec  : AWord;
      SecClu   : Byte;
      ResSect  : AWord;
      FATCps   : Byte;
      RootEnt  : AWord;
      TotalSc  : AWord;
      BPB      : Byte;
      SecFAT   : AWord;
      SecTrack : AWord;
      Sides    : AWord;
      HiddenSc : LongInt;
      NumSec   : LongInt;
      PhysNum  : AWord;
      ExtBootS : Byte;
      VolumeN  : Array [0..4] of Byte;
      VolumeL  : Array [0..10] of Char;
      FilSysID : Array [0..5] of Char;
     end;

    DirRec = record
     Name : Array[0..10] of Char;
     Attr : Byte;
     Data : Array[0..9] of Byte;
     Date : LongInt;
     Clus : AWord;
     Len  : LongInt;
    end;

{$IFNDEF NONBP}
    DirPtr = Array [0..2000] of DirRec;

    ShDirRec = record
     Name : Array[0..10] of Char;
     Attr : Byte;
     Date : LongInt;
     Clus : Word;
     Len  : LongInt;
    end;


    ShDirPtr = Array [0..2048] of ShDirRec;

    PDiskDriver = ^TDiskDriver;

    TDiskDriver = object
     FAT           : ^FATRec;
     BADmark,EOFmark,Entry,DirClu,LastEl
                   : word;
     FATSize       : LongInt;
     FATType       : (Fat12, Fat16);
     Drive         : Byte;
     SectLen       : word;
     SectPerClust  : byte;
     Reserved      : word;
     FatCopies     : byte;
     ROOTentries   : word;
     TotalSect     : word;
     BPB           : byte;
     SecFAT        : Word;
     SecTrack      : Word;
     Sides         : Word;
     SectPerFat    : word;
     FirstClust    : word;
     TotalClu      : Word;
     NumSectors    : Longint;
     FATSect       : Word;
     FATSectNum    : Word;
     FATCurSectNum : Word;
     FATModified   : Boolean;

     constructor Init(Drv: Integer; FATRead: Boolean);
     procedure SeekFAT(N: Word);
     procedure SetFAT(N,Value : Word);
     function  GetFAT(N : Word) : Word;
     function  ChainLen(N : Word):Word;
     procedure ClusterRead(var Buf;N : Word);
     procedure ClusterWrite(var Buf;N : Word);
     function  MadeSec(t : LongInt) : LongInt;
     function  MadeLen(t : LongInt) : LongInt;
     function  MadeClust(t : word) : word;
     function  GetPath(s : string) : word;
     function  ClusterSize(Cluster: Word): Word;
     procedure ReadFAT;
     procedure SetVLabel(Lb : String);
     procedure WriteFAT;
     procedure FreeFAT;
     destructor Done;
    end;

 var BiosError: Byte;

    procedure SectorRead(Drive : Byte;q : pointer;t : LongInt; l : word);
    procedure SectorWrite(Drive : Byte;q : pointer;t : LongInt; l : word);
{DataCompBoy
    procedure DiskOn(Drv : Byte);
    procedure DiskOff;
}
    function  StrEq(Var a,b;Count : Word) : Boolean;
    function  CheckDisk(C : Char) : Boolean;
{$ENDIF}

implementation
{$IFNDEF NONBP}
uses
     {$IFDEF DPMI} Dpmi, DosMem, {$ENDIF}
     Startup, Messages, Memory, DNApp, RStrings, Commands;

 type SecRec = record
       Num   : LongInt;
       Count : Word;
       Buf   : Pointer;
      end;

function MaxAvail: LongInt;
begin
  MaxAvail := MemAdjust(System.MaxAvail);
end;


function XlatError(I: Word): Word;
begin
 case I of
  $03: XlatError := 0;
  $80: XlatError := 3;
  $40: XlatError := 6;
  $08: XlatError := 4;
  $04: XlatError := 8;
  else XlatError := 1;
 end;
end;

{$IFNDEF DPMI}
procedure SectorRead;
 label Rep;
 var Prm : SecRec;
     ppr : pointer;
begin
Rep:
  if not DOS40 then
  asm
       push  ds
       mov   al,Drive
       mov   cx,l
       mov   dx,word ptr t
       lds   bx,q
       push  bp
       int   $25
       pop   bp
       pop   bp
       pop   ds
       mov   BiosError,0
       jnc   @LocEx
       mov   BiosError,ah
@LocEx:
  end else
  begin
   Prm.Num:=t;
   Prm.Count:=l;
   Prm.Buf:=q;
   ppr:=@Prm;
   asm
      push ds
      mov  al,Drive
      mov  cx,$ffff
      lds  bx,ppr
      push bp
      int  $25
      pop  bp
      pop  bp
      pop  ds
      mov   BiosError,0
      jnc   @LocEx
      mov   BiosError,ah
@LocEx:
   end;
  end;
 Abort := Off;
 if BiosError > 0 then
  begin
   Abort := SysErrorFunc(XlatError(BiosError), Drive) = 3;
   if not Abort then Goto Rep;
  end;
end;

procedure SectorWrite;
 label Rep;
 var Prm : SecRec;
     ppr : pointer;
begin
Rep:
 if not DOS40 then
  asm
       push  ds
       mov   al,Drive
       mov   cx,l
       mov   dx,word ptr t
       lds   bx,q
       push  bp
       int   $26
       pop   bp
       pop   bp
       pop   ds
       mov   BiosError,0
       jnc   @LocEx
       mov   BiosError,ah
@LocEx:
  end
  else
  begin
   Prm.Num:=t;
   Prm.Count:=l;
   Prm.Buf:=q;
   ppr:=@Prm;
   asm
      push ds
      mov  al,Drive
      mov  cx,$ffff
      lds  bx,Ppr
      push bp
      int  $26
      pop  bp
      pop  bp
      pop  ds
      mov   BiosError,0
      jnc   @LocEx
      mov   BiosError,ah
@LocEx:
   end;
  end;
 Abort := Off;
 if BiosError > 0 then
  begin
   Abort := SysErrorFunc(XlatError(BiosError), Drive) = 3;
   if not Abort then Goto Rep;
  end;
 asm
  mov ah,0dh
  int 21h
 end;
end;
{$ELSE}

procedure SectorRead(Drive: Byte; Q: Pointer; T: LongInt; L: Word);
var
  R: DPMIRegisters;
  P: Pointer;
  P2: ^SecRec;
  S, S2: Word;
label
  Rep;
begin
  P:=GetDosMem(L*512,S); if P=nil then begin Abort:=True; Exit; end;
  if DOS40 then begin
    P2:=GetDosMem(SizeOf(P2^),S2);
    if P2=nil then begin
      FreeDosMem(P); Abort:=True; Exit;
    end;
  end;
  Rep:
  FillChar(R, SizeOf(R), #0);
  if not DOS40 then begin
    R.AL:=Drive;
    R.CX:=L;
    R.DX:=LongRec(T).Lo;
    R.DS:=S; R.BX:=0;
    if SimulateRealModeInt($25, R) <> 0 then
      BiosError:=R.AH;
  end else
  begin
    P2^.Num:=T;
    P2^.Count:=L;
    P2^.Buf:=Ptr(S,0);
    R.AL:=Drive;
    R.CX:=$FFFF;
    R.DS:=S2; R.BX:=0;
    if SimulateRealModeInt($25, R) <> 0 then
      BiosError:=R.AH;
  end;
  Abort:=Off;
  if BiosError > 0 then
  begin
   Abort:=SysErrorFunc(XlatError(BiosError), Drive) = 3;
   if not Abort then Goto Rep;
  end;
  Move(P^,Q^,L*512); FreeDosMem(P);
  if DOS40 then FreeDosMem(P2);
end;

procedure SectorWrite(Drive: Byte; Q: pointer; T: LongInt; L: word);
var
  R: DPMIRegisters;
  P: Pointer;
  P2: ^SecRec;
  S, S2: Word;
label
  Rep;
begin
  P:=GetDosMem(L*512,S); if P=nil then begin Abort:=True; Exit; end;
  if DOS40 then begin
    P2:=GetDosMem(SizeOf(P2^),S2);
    if P2=nil then begin
      FreeDosMem(P); Abort:=True; Exit;
    end;
  end;
  Move(Q^,P^,L*512);
  Rep:
  FillChar(R, SizeOf(R), #0);
  if not DOS40 then begin
    R.AL:=Drive;
    R.CX:=L;
    R.DX:=LongRec(T).Lo;
    R.DS:=S; R.BX:=0;
    if SimulateRealModeInt($26, R) <> 0 then
      BiosError:=R.AH;
  end else
  begin
    P2^.Num:=T;
    P2^.Count:=L;
    P2^.Buf:=Ptr(S,0);
    R.AL:=Drive;
    R.CX:=$FFFF;
    R.DS:=S2; R.BX:=0;
    if SimulateRealModeInt($26, R) <> 0 then
      BiosError:=R.AH;
  end;
  Abort:=Off;
  if BiosError > 0 then
  begin
   Abort:=SysErrorFunc(XlatError(BiosError), Drive) = 3;
   if not Abort then Goto Rep;
  end;
  asm
   mov ah,0dh (* reset disk *)
   int 21h
  end;
  FreeDosMem(P);
  if DOS40 then FreeDosMem(P2);
end;
{$ENDIF}

constructor TDiskDriver.Init;
var S: string;
    BRec: PBootRec;
    P: PDiskDriver;
    C: Char;
    RemoteDrive: Boolean;
    Removable: Boolean;
begin
  Abort := False;
  lGetDir(Drv, s); Drv := Byte(S[1]) - 64;
  RemoteDrive := False;
  C := S[1];
{piwamoto.change.begin}
  if SystemData.Drives[C] and ossDirectAccess = 0 then Fail;
  asm
   mov ax, 4409H
   xor bh,bh
   mov bl,C
   sub bl,64
   int 21H
   test dh, 10h
   jnz @Remote
   test dh, 80h
   jz  @BlockDevice
 @Remote:
   mov RemoteDrive, 1
 @BlockDevice:
  end;

  if Abort or RemoteDrive then Fail;

  if SystemData.Options and ossCheckDDA <> 0 then
    begin
      if opSys and not opDos = 0 then Removable := on else Removable := off;
{piwamoto.change.end}
      if not Removable then
        begin
          asm
           mov ax, $4408
           mov bl, C
           sub bl, 64
           push bp
           int  21h
           pop  bp
           or   ax, ax
           jnz  @@1
           mov Removable, 1
          @@1:
          end;
          if not Removable then Fail
        end;
    end;

  GetMem(BRec,4096);
  FillChar(BRec^, 4096, 0);
  SectorRead(Drv-1, BRec, 0, 1);
  if Abort then begin FreeMem(BRec, 4096); Fail end;
  Move(BRec^.ByteSec, SectLen, 19);
  if BRec^.TotalSc = 0 then NumSectors := BRec^.NumSec
                       else NumSectors := BRec^.TotalSc;
  FreeMem(BRec,4096);
  if (SectLen = 0) or (SectPerClust = 0) then Fail;
  Drive := Drv - 1; FAT := Nil;
  SectPerFAT := SecFAT;
  FATSize := SectPerFAT * SectLen;
  FirstClust := (SectPerFat * FatCopies) + Reserved
                + (ROOTentries * 32 div SectLen);
  if TotalSect <> 0 then NumSectors := TotalSect;
  TotalClu := LongInt(NumSectors - FirstClust + 1) div SectPerClust;

  if TotalClu < 4087
   then FATType := FAT12
   else FATType := FAT16;

  case FatType of
   FAT12: begin
           EOFmark := $FF8;
           BADmark := $FF7;
           LastEl  := $FF0;
          end;
   FAT16: begin
           EOFmark := $FFF8;
           BADmark := $FFF7;
           LastEl  := $FFF0;
          end;
  end;

  FATModified := Off;
  if FATRead then
    begin
     SeekFAT(0);
     if (FATSectNum=0) or (FAT=nil) then Fail;
     if Abort and (FAT <> nil) then begin FreeFAT; Fail; end;
    end;
end;

procedure FreeMem(var p : Pointer;Size : Word);
begin
 if p = Nil then Exit;
 System.FreeMem(p,Size);
 p:=Nil;
end;

function StrEq(Var a,b;Count : Word) : Boolean;
 var f : Boolean;
begin
 asm
   push ds
   les  si,a
   lds  di,b
   xor  dx,dx
   mov  cx,Count
  @Loop:
   mov al,es:[si]
   cmp al,ds:[di]
   jnz @Ex
   inc di
   inc si
   loop @Loop
   inc dl
  @Ex:
   pop ds
   mov f,dl
 end;
 StrEq:=f;
end;


procedure TDiskDriver.SeekFAT;
begin
 WriteFAT;
 if N < 0 then N := 0; if N >= SectPerFAT then N := SectPerFAT - 1;
 FATSect := N;
 ReadFAT;
end;

function TDiskDriver.GetPath(s : string) : word;
var
    ts,vs : string[12];
    cl,i  : word;
    Made  : boolean;
    QQP   : ^DirPtr;
    QSize : Word;

procedure SetCatalog;
 Label 1;
begin
  FillChar(vs[1],11,32); ts := AddSpace(ts, 12);
  if PosChar('.',ts) > 0 then begin
                          Move(ts[1],vs[1],PosChar('.',ts)-1);
                          Move(ts[PosChar('.',ts)+1],vs[9],3);
                        end else
                          Move(ts[1],vs[1],8);
  Byte(ts[0]):=11;Byte(vs[0]):=11;
  ts:=vs;
1:
  ClusterRead(QQP^,cl);i:=0;Made:=Off;
  while (i<MadeLen(cl)*SectLen div 32) and not Made do
        begin
          if (QQP^[i].Attr and VolumeID = 0) and StrEq(QQP^[i].Name,ts[1],11) then
             begin
              cl:=QQP^[i].clus;
              Made:=On;
             end;
          Inc(i);
        end;
  if not Made then
   if GetFat(cl)<LastEl then
     begin
      cl:=GetFAT(cl);
      GoTo 1;
     end;
end;

begin
  GetPath:=$FFFF;
  if FAT=Nil then begin Abort := On; Exit; end;
  QSize:=SectLen*SectPerClust;
  if QSize<RootEntries*32 then QSize:=RootEntries*32;
  QQP := MemAlloc(QSize);cl:=0;GetPath:=0;
  if (QQP=Nil) or (QSize=0) then Exit;
  UpStr(s);if not (s[Byte(s[0])] in ['\','/']) then s:=s+'\';
  Delete(s,1,posChar(':',s));if s[1] in ['\','/'] then Delete(s, 1, 1); {DelFC(s);}
  while (Byte(s[0])>0) and (PosChar('\',s)>0) do
        begin
          FillChar(ts[1],11,32);
          if PosChar('\',s)>0 then ts:=Copy(s,1,PosChar('\',s)-1)
                          else ts:=s;
          s:=Copy(s,PosChar('\',s)+1,MaxStringLength);
          SetCatalog;
        end;
  FreeMem(Pointer(QQP),QSize);
  GetPath:=cl;
end;

function TDiskDriver.GetFAT(N : Word) : Word;
 var d: Word;
     DD: LongInt;
begin
 GetFAT:=LastEl+$F;
 if (FAT=Nil) or (N<2) or (n>TotalClu+2) then Exit;
 case FATType of
  FAT12: begin
          d:=(n*3) shr 1;
          if (D div SectLen < FATSect) or
             (D div SectLen >= FATSect+FATCurSectNum)
           then SeekFAT(D div SectLen);
          D := D - FATSect*SectLen;
          D := FAT^[d]+FAT^[d+1] shl 8;
          if N and 1=0
           then d:=d and $FFF
           else d:=d shr 4;
         end;
  FAT16: begin
          DD := LongInt(N)*2 div LongInt(SectLen);
          if (DD < FATSect) or (DD >= FATSect+FATCurSectNum) then SeekFAT(DD);
          DD := LongInt(N)*2 - LongInt(FATSect)*LongInt(SectLen);
          d:=FAT^[DD]+FAT^[DD+1] shl 8;
         end;
 end;
 GetFAT:=d;
end;

procedure TDiskDriver.SetFAT(N,Value : Word);
 var d,v : Word;
     DD: LongInt;
begin
 if (N<2) or (n>TotalClu+2) or (FAT=Nil) then Exit;
 Case FATType of
  FAT12: begin
          d:=(n*3) shr 1;
          if (D div SectLen < FATSect) or
             (D div SectLen >= FATSect+FATCurSectNum)
           then SeekFAT(D div SectLen);
          D := D - FATSect*SectLen;
          V := FAT^[d]+FAT^[d+1] shl 8;
          FATModified := On;
          if N and 1=0 then begin
           v:=(v and $F000) or (Value and $FFF);
           Move(v,FAT^[d],2);
          end else begin
           v:=(v and 15) or (Value shl 4);
           Move(v,FAT^[d],2);
          end;
         end;
  FAT16: begin
          DD := LongInt(N)*2 div LongInt(SectLen);
          if (DD < FATSect) or (DD >= FATSect+FATCurSectNum) then SeekFAT(DD);
          DD := LongInt(N)*2 - LongInt(FATSect)*LongInt(SectLen);
          FATModified := On;
          FAT^[DD]:=Value and 255;
          FAT^[DD+1]:=Value shr 8;
         end;
 end;
end;

procedure TDiskDriver.ClusterRead(var Buf;N : word);
 var p : Pointer;
begin
 p:=@Buf;SectorRead(Drive,p,MadeSec(n),MadeLen(n));
end;

procedure TDiskDriver.ClusterWrite(var Buf;N : Word);
 var p : Pointer;
begin
 p:=@Buf;SectorWrite(Drive,p,MadeSec(n),MadeLen(n));
end;

function TDiskDriver.ChainLen(N : Word):Word;
 var Next : Word;
     Len  : Word;
begin
  ChainLen:=0;
  if FAT=Nil then Exit;
  if n=0 then begin
                n:=ROOTentries*32 div (SectLen*SectPerClust);
                ChainLen:=n;exit;
              end;
  Next:=GetFAT(N);Len:=1;
  While (Next<EOFmark) and (Len<TotalClu) do
    begin
      Next:=GetFAT(Next);
      Inc(Len);
    end;
 ChainLen:=Len;
end;

function TDiskDriver.MadeSec(t : LongInt) : LongInt;
 var MS: LongInt;
begin
  if t>=2 then MS:=LongInt(FirstClust+(t-2)*LongInt(SectPerClust))
          else MS:=LongInt(LongInt(SectPerFat)*LongInt(FatCopies)+Reserved);
  MadeSec := MS;
end;

function TDiskDriver.MadeLen;
begin
  if t>=2 then MadeLen:=SectPerClust
          else MadeLen:=(RootEntries*32) div SectLen;
end;

function TDiskDriver.MadeClust(t : word) : word;
begin
  if t>=FirstClust then MadeClust:=(t-FirstClust) div SectPerClust+2
                   else MadeClust:=0;
end;

procedure TDiskDriver.ReadFAT;
 var N: Word;
begin
 Abort := On;
 if FAT = nil then begin
     {if MAXAvail > 40*1024 then N := 32768 else }N := 1024*5;
     if LongInt(N) > MaxAvail then Exit;
     FATSectNum := N div SectLen;
     FAT := MemAlloc(FATSectNum*SectLen);
  end;
 if FAT = nil then Exit;
 if FATSect + FATSectNum > SectPerFAT
    then FATCurSectNum := SectPerFAT - FATSect
    else FATCurSectNum := FATSectNum;
 SectorRead(Drive,FAT,Reserved+FATSect,FATSectNum);
 FATModified := Off;
 Abort := Off;
end;

procedure TDiskDriver.WriteFAT;
 var I: Byte;
     N: Word;
begin
 if (FAT=Nil) or not FATModified then Exit;
 for i:=0 to FATCopies-1 do
  begin
   NeedAbort := On;
   SectorWrite(Drive,FAT,Reserved+FATSect+SectPerFAT*i, FATCurSectNum);
  end;
 NeedAbort := Off;
 FATModified := Off;
end;

procedure TDiskDriver.FreeFAT;
begin
 WriteFAT;
 FreeMem(Pointer(FAT),FATSectNum*SectLen);
 FAT:=Nil;
end;

destructor TDiskDriver.Done;
begin
 FreeFAT;
end;

{DataCompBoy
procedure DiskOn;
begin
 if not DiskOperation then Exit;
 Port[$3F2]:=($08 shl Drv) or $0C or (Drv-1);
 mem[seg00040:$40]:=$FF;
end;
}
{DataCompBoy
procedure DiskOff;
begin
 Port[$3F2]:=$0C;
end;
}

procedure TDiskDriver.SetVLabel;
  label   Ex;
  var i   : Integer;
      QQP : ^DirPtr;
      dt  : DateTime;
      ye,mh,dd,dw,hh,mm,ss,s1
          : Word;
begin
 Abort:=Off;
 I := Length(Lb);
 while (I > 1) and (Lb[I] <> '.') do Dec(I);
 if Lb[I] = '.' then
   begin
     Delete(Lb, I, 1);
     while I < 9 do begin Insert(' ', Lb, I); Inc(I) end;
   end;
 QQP := MemAlloc(RootEntries*32);
 if QQP=Nil then Exit;
 ClusterRead(QQP^,0);
 if Abort then GoTo Ex;
 i:=0;
 While (i<RootEntries) and (QQP^[i].Name[0]<>#0) and
       (QQP^[i].Name[0]<>#$E5) and (QQP^[i].attr and VolumeID=0) do Inc(i);
 if (QQP^[i].Name[0]=#0) or (QQP^[i].Name[0]=#$E5)
     or (QQP^[i].attr and VolumeID<>0) then QQP^[i]:=QQP^[3]
 else
 begin
  MessageBox(GetString(dlDTDiskFull1)+Char(Drive+64)+':'+GetString(dlDTDiskFull2), nil, mfError + mfOKButton);
  GoTo Ex;
 end;
 if Lb <> '' then Lb:=Lb+'            ' else Lb := #$E5'          ';
 GetDate(ye,mh,dd,dw);
 GetTime(hh,mm,ss,s1);
 with dt do
  begin
   Year:=ye;
   Month:=mh;
   Day:=dd;
   Hour:=hh;
   Min:=mm;
   Sec:=ss;
  end;
 PackTime(dt,QQP^[3].Date);
 Move(Lb[1],QQP^[3].Name,11);
 QQP^[3].Attr:=8;
 QQP^[3].Clus:=0;
 QQP^[3].Len:=0;
 ClusterWrite(QQP^,0);
Ex:
 System.FreeMem(QQP,RootEntries*32);
end;

function TDiskDriver.ClusterSize;
begin
 if Cluster = 0 then ClusterSize := ROOTEntries * 32
                else ClusterSize := SectLen * SectPerClust
end;


 function CheckDisk;
   var RemoteDrive: Boolean;
       Removable: Boolean;
 begin
   ClrIO;
   CheckDisk := Off;
{piwamoto.change.begin}
   if SystemData.Drives[C] and ossDirectAccess = 0 then Exit;
   RemoteDrive := Off;
   asm
    mov ax, 4409H
    xor bh,bh
    mov bl, C
    sub bl, 64
    int 21H
    test dh, 10h
    jnz @Remote
    test dh, 80h
    jz  @BlockDevice
  @Remote:
    mov RemoteDrive, 1
  @BlockDevice:
   end;

   if Abort or RemoteDrive then Exit;

   if SystemData.Options and ossCheckDDA <> 0 then
     begin
       if opSys and not opDos = 0 then Removable := on else Removable := off;
{piwamoto.change.end}
       if not Removable then
         begin
           asm
            mov ax, $4408
            mov bl, C
            sub bl, 64
            push bp
            int  21h
            pop  bp
            or   ax, ax
            jnz  @@1
            mov Removable, 1
           @@1:
           end;
           if not Removable then Exit;
         end;
     end;
    CheckDisk := On;
 end;
{$ENDIF NONBP}
end.
