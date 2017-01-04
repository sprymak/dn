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
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

UNIT MemInfo;

{$F+,A+}

INTERFACE

{$IFDEF MEMINFO}
USES
  Startup, CPUType, Advance;
{$ENDIF}

    PROCEDURE MemoryInfo;

IMPLEMENTATION

{$IFDEF MEMINFO}
USES
  {$IFDEF DPMI} Dpmi,{$ENDIF}
  Dos,
  Objects,
  Views,
  DNApp,
  Drivers,
  Commands,
  RStrings,
  Messages,
  Dialogs,
  ExtraMem,
  DNHelp,
  Collect,

  Advance1,
  Advance3;

type
    PTextColl    = ^TTextColl;
    TTextColl    = object(TCollection)
                    Procedure   AddStr(s : String);
                    Procedure   FreeItem(Item : Pointer);        virtual;
                   end;

Procedure TTextColl.AddStr;
begin Insert(NewStr(s)); end;

Procedure TTextColl.FreeItem;
begin DisposeStr(PString(Item)); end;

function RMem(ASeg, ROfs: word): byte;
{$IFDEF DPMI}
var q: word;
begin
 q:=ASeg mod $1000;
 ASeg:=RSeg((ASeg div $1000)*$1000);
 ROfs:=ROfs+q * $10;
 RMem:=Mem[ASeg:ROfs];
end;
{$ELSE}
inline($5F/         {POP DI}
       $07/         {POP ES}
       $26/$8A/$05);{MOV AL,ES:[DI]}
{$ENDIF}

function RMemW(ASeg, ROfs: word): word;
{$IFDEF DPMI}
var q: word;
begin
 q:=ASeg mod $1000;
 ASeg:=RSeg((ASeg div $1000)*$1000);
 ROfs:=ROfs+q * $10;
 RMemW:=MemW[ASeg:ROfs];
end;
{$ELSE}
inline($5F/         {POP DI}
       $07/         {POP ES}
       $26/$8B/$05);{MOV AX,ES:[DI]}
{$ENDIF}

var TSR : PTextColl;

{------------------------------------------------------}
{-------------------}{$IFDEF DPMI}{--------------------}
{------------------------------------------------------}
Procedure AddTSRs; {$IFDEF BIT_16}far;{$ENDIF}
var CurMCB,DevMCB,i,j : Word;
    s,s1              : String;
    LastMCB           : Boolean;
    isUMB             : Boolean;
    HiDosMem          : Word;

function aaa: string;
begin
  aaa := s+Strg(' ',9-Length(s))+#179' ';
end;

var Regs : DPMIRegisters;
begin
 FillWord(Regs, SizeOf(Regs) shr 1, $00);
 Regs.Ax:=$5200;
 SimulateRealModeInt($21, Regs);
 CurMCB:=RMemW(Regs.ES, Regs.BX-2);
 SimulateRealModeInt($12, Regs);
 HiDosMem:=Regs.AX shl 6;
 isUMB := ExistUMBMem;
 j:=RMem(CurMCB,3);DevMCB:=CurMCB+1;
 While DevMCB<CurMCB+RMemW(CurMCB,3)+1 do
       begin
        j:=RMemW(DevMCB,3);
        if j<$18C0 then s1:=Sstr(LongInt(j)*16,6,' ')+'b'
                   else s1:=Sstr(LongInt(j) div 64,6,' ')+'k';
        s:='Sys area';
        case char(RMem(DevMCB,0)) of
         'T','D'
             : begin
                s:='';i:=8;
                While (i<16) and (RMem(DevMCB,i)<>0) do
                      begin s:=s+Char(RMem(DevMCB,i));Inc(i);end;
                s:=aaa;
                if char(RMem(DevMCB,0))='T'
                   then s:=s+'Install'
                   else s:=s+'Driver ';
               end;
         'F' : s:=aaa+'Files  ';
         'B' : s:=aaa+'Buffers';
         'X' : s:=aaa+'FCBs   ';
         'L' : s:=aaa+'LastDrv';
         'S' : s:=aaa+'Stacks ';
         'I' : s:=aaa+'IFS    ';
         'E' : s:=aaa+'Buffs/X';
         else s:='';
        end;
        if s<>'' then TSR^.AddStr(s+' '#179' '+Hex4(DevMCB+1)+' '#179+s1+
                      ' '+Char(DevMCB)+Char(DevMCB shr 8));
        DevMCB:=(DevMCB+1+RMemW(DevMCB,3));
       end;
 LastMCB:=False;
 While (not LastMCB) and (CurMCB<$FF00) do begin
   if (RMemW(CurMCB+1,0)=$20CD) and (RMemW(CurMCB,1)<>0) then begin
     s:='';i:=8;
     While (i<16) and (RMem(CurMCB,i)<>0) do begin
       s:=s+Char(RMem(CurMCB,i));
       Inc(i);
     end;
     j:=RMemW(CurMCB,3);
     if j<$18C0
       then s1:=Sstr(LongInt(j)*16,6,' ')+'b'
       else s1:=Sstr(LongInt(j) div 64,6,' ')+'k';
     s:=aaa+'Program '#179' '+Hex4(CurMCB+1)+' '#179+s1;
     TSR^.AddStr(s+' '+Char(CurMCB)+Char(CurMCB shr 8));
   end;
   {
     Exit if:
       MCB = HiDosMem
       ( MCB > HiDosMem or MCB = 'Z' ) and no UMB
       ( MCB = 'Z' and MCB > HiDosMem )
       MCB >= 0xFF00
   }
   LastMCB := ( CurMCB = HiDosMem ) or
              (( CurMCB > HiDosMem ) or ( Char(RMem(CurMCB,0))='Z' )) and not isUMB or
              (( CurMCB > HiDosMem ) and ( Char(RMem(CurMCB,0))='Z' ));
   CurMCB:=(CurMCB+1+RMemW(CurMCB,3));
 end;
end;

{------------------------------------------------------}
{-------------------}{$ELSE !DPMI}{--------------------}
{------------------------------------------------------}

Procedure AddTSRs; {$IFDEF BIT_16}far;{$ENDIF}
var CurMCB,DevMCB,i,j : Word;
    s,s1              : String;
    LastMCB           : Boolean;
    isUMB             : Boolean;
    HiDosMem          : Word;

function aaa: string;
begin
  aaa := s+Strg(' ',9-Length(s))+#179' ';
end;
begin
 asm mov ah,52h
     int $21
     mov ax,es:[bx-2]
     mov CurMCB,ax
     int 12h
     mov cl,6
     shl ax,cl
     mov word ptr HiDosMem,ax
 end;
 isUMB := ExistUMBMem;
 j:=Mem[CurMCB:3];DevMCB:=CurMCB+1;
 While DevMCB<CurMCB+memw[CurMCB:3]+1 do
       begin
        j:=memw[DevMCB:3];
        if j<$18C0 then s1:=Sstr(LongInt(j)*16,6,' ')+'b'
                   else s1:=Sstr(LongInt(j) div 64,6,' ')+'k';
        s:='Sys area';
        case char(mem[DevMCB:0]) of
         'T','D'
             : begin
                s:='';i:=8;
                While (i<16) and (mem[DevMCB:i]<>0) do
                      begin s:=s+Char(mem[DevMCB:i]);Inc(i);end;
                s:=aaa;
                if char(mem[DevMCB:0])='T'
                   then s:=s+'Install'
                   else s:=s+'Driver ';
               end;
         'F' : s:=aaa+'Files  ';
         'B' : s:=aaa+'Buffers';
         'X' : s:=aaa+'FCBs   ';
         'L' : s:=aaa+'LastDrv';
         'S' : s:=aaa+'Stacks ';
         'I' : s:=aaa+'IFS    ';
         'E' : s:=aaa+'Buffs/X';
         else s:='';
        end;
        if s<>'' then TSR^.AddStr(s+' '#179' '+Hex4(DevMCB+1)+' '#179+s1+
                      ' '+Char(DevMCB)+Char(DevMCB shr 8));
        DevMCB:=(DevMCB+1+memw[DevMCB:3]);
       end;
 LastMCB:=False;
 While (not LastMCB) and (CurMCB<$FF00) do begin
   if (memw[CurMCB+1:0]=$20CD) and (memw[CurMCB:1]<>0) then begin
     s:='';i:=8;
     While (i<16) and (mem[CurMCB:i]<>0) do begin
       s:=s+Char(mem[CurMCB:i]);
       Inc(i);
     end;
     j:=memw[CurMCB:3];
     if j<$18C0
       then s1:=Sstr(LongInt(j)*16,6,' ')+'b'
       else s1:=Sstr(LongInt(j) div 64,6,' ')+'k';
     s:=aaa+'Program '#179' '+Hex4(CurMCB+1)+' '#179+s1;
     TSR^.AddStr(s+' '+Char(CurMCB)+Char(CurMCB shr 8));
   end;
   LastMCB := ( CurMCB = HiDosMem ) or
              (( CurMCB > HiDosMem ) or ( Char(mem[CurMCB:0])='Z' )) and not isUMB or
              (( CurMCB > HiDosMem ) and ( Char(mem[CurMCB:0])='Z' ));
   CurMCB:=(CurMCB+1+memw[CurMCB:3]);
 end;
end;

{------------------------------------------------------}
{-------------------}{$ENDIF DPMI}{--------------------}
{------------------------------------------------------}

Procedure ShowInfo(Segm : Word); {$IFDEF BIT_16}far;{$ENDIF}
label Unkn,BadCmdL;
var i,j,k : Word;
    s,q,w : String;
    l,b,e : LongInt;
begin
 s:='';
 case Char(RMem(Segm,0)) of
  'F','B','X','L',
  'S','I','E' : begin
                 s:= GetString(dlMI_ResArea);
                 case Char(RMem(Segm,0)) of
                  'F' : s:=s+GetString(dlMI_Files);
                  'B' : s:=s+GetString(dlMI_Buffers);
                  'X' : s:=s+GetString(dlMI_FCBs);
                  'L' : s:=s+GetString(dlMI_Drives);
                  'S' : s:=s+GetString(dlMI_Stacks);
                  'I' : s:=s+GetString(dlMI_IFS);
                  'E' : s:=s+GetString(dlMI_XBuffers);
                 end;
                end;
  'D'         : begin
                 s:=GetString(dlMI_DeviceFlags)+Hex4(RMemW(Segm,$14))+#13#3;
                 if RMemW(Segm,$14) and $8000>0
                    then begin
                          s:=s+GetString(dlMI_DeviceName);
                          for i:=0 to 7 do s:=s+char(RMem(Segm,$1A+i));
                          While s[Byte(s[0])]<#33 do Dec(Byte(s[0]));
                         end
                    else s:=s+GetString(dlMI_DeviceHandle1)+
                              SStr(RMem(Segm,$1A),0,' ')+
                              GetString(dlMI_DeviceHandle2);
                end;
  'T','M','Z' : begin
                 q:='';i:=8;
                 While (i<16) and (RMem(Segm,i)<>0) do
                       begin q:=q+Char(RMem(Segm,i));Inc(i);end;
                 i:=RMemw(Segm,$3C);
                 w:=GetString(dlMI_PgmPath);
                 if i<>0
                    then begin
                          s:='';j:=0;
                          While ((RMem(i,j)<>0) or (RMem(i,j+1)<>0))
                                and (j<$8000) do Inc(j);
                          Inc(j,4);
                          While RMem(i,j)<>0 do
                                begin s:=s+Char(RMem(i,j));Inc(j);end;
                          If Pos(q,s)=0 then GoTo Unkn;
                          s:=w+s;
                         end
                    else Unkn: s:=w+GetString(dlUnknown);
                 s:=s+GetString(dlMI_CmdLine);
                 i:=RMem(Segm,$90);
                 if i<$80
                    then begin
                          if i=0 then s:=s+GetString(dlDINone)
                                 else begin
                                       q:='"';
                                       For i:=$91 to $90+i do
                                           if (RMem(Segm,i)>31)
                                              then q:=q+char(RMem(Segm,i))
                                              else GoTo BadCmdL;
                                       s:=s+q+'"';
                                      end;
                         end
                    else BadCmdL: s:=s+GetString(dlUnknown);
                end;
 end;
 case Char(RMem(Segm,0)) of
  'M','Z','T','D' : begin
                     s:=s+GetString(dlHookVectors);
                     q:='';
                     for i:=0 to $FF do
                         begin
                          j:=RMemW(0,i*4);
                          k:=RMemW(0,i*4+2);
                          l:=LongInt(k)*16+j;
                          b:=LongInt(Segm)*16;
                          e:=b+LongInt(RMemW(Segm,3))*16+16;
                          if (l>=b) and (l<=e) then q:=q+Hex2(i)+',';
                         end;
                     if q<>''
                        then begin
                              Dec(Byte(q[0]));
                              s:=s+q
                             end
                        else s:=s+GetString(dlDINone);
                    end;
 end;
 if s<>'' then MessageBox(s,NIL,mfOkButton or mfInformation);
end;

Procedure MemoryInfo;
var
  R        : TRect;
  PD       : PDialog;
  i,j      : Integer;
  VS       : PScrollBar;
  ListData : TListBoxRec;
  TSRlist  : PListBox;
  w        : Word;
  Ps       : PString;

begin
 R.Assign(0,0,52,20);
 New(PD,Init(R,GetString(dlMemoryInfo)));
 PD^.Options := PD^.Options or ofCentered;
 PD^.HelpCtx := hcMemoryInfo;

 R.Assign(38,2,39,18);
 New(VS,Init(R));
 PD^.Insert(VS);
 R.Assign(2,2,38,18);
 New(TSRlist,Init(R,1,VS));
 New(TSR,Init(8,8));
 AddTSRs;
 ListData.Focus:=0;
 ListData.List:=TSR;
 TSRlist^.SetData(ListData);
 PD^.Insert(TSRlist);
 R.Assign(2,1,31,2);
 PD^.Insert(New(PLabel,Init(R, GetString(dlTSRTitle),TSRlist)));

 R.Assign(40,2,50,4);
 PD^.Insert(New(PButton, Init(R, GetString(dlDetailButton), cmOK, bfDefault)));
 Inc(R.A.Y,2);Inc(R.B.Y,2);
 PD^.Insert(New(PButton, Init(R, GetString(dlOKButton), cmCancel, bfNormal)));
 PD^.SelectNext(False);
 repeat
  w:=DeskTop^.ExecView(PD);
  if w<>cmCancel then
     begin
      TSRlist^.GetData(ListData);
      PS:=TSR^.At(ListData.Focus);
      ShowInfo(memw[Seg(PS^[36]):Ofs(PS^[36])]);
     end;
 until (w=cmCancel);
 Dispose(TSR,Done);
 Dispose(PD,Done);
end;

{$ELSE}
PROCEDURE MemoryInfo; begin end;
{$ENDIF}

end.
