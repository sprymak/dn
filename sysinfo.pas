{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.11
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

UNIT SysInfo;

INTERFACE

{$IFDEF SYSINFO}
USES
  Startup, CPUType, Advance;
{$ENDIF}

    PROCEDURE SystemInfo;

IMPLEMENTATION

{$IFDEF SYSINFO}
USES Dos,      Objects, Views,  DNApp,   Drivers,  Commands, RStrings,
     Messages, Dialogs, DNHelp, Collect, Advance1, Advance3, ExtraMem;

Var PCmodel,PCsubModel : byte ;

type PModel = ^TModel;
     TModel = record
      Len: Word;
      Model: Byte;
      SubModel: Byte;
      BIOSRevision: Byte;
     end;

Type TCPUFlagInfo = record
                      case Item: integer of
                         0: (longWord: longint);
                         1: (HiWord,
                             LoWord: word)
                    end;
Var  CPUFlag: TCPUFlagInfo;

procedure GetMType(var S: String);
 var Model, SubModel: Byte;
     P: PModel;
begin
 asm
  mov ah, $C0
  int 15h
  mov word ptr P, bx
  mov word ptr P+2, es
 end;
 Model := P^.Model;
 if P^.Model = 0 then Model := mem[$F000:$FFFE];
 SubModel := P^.SubModel;

 PCModel    := P^.Model ;
 PCSubModel := P^.SubModel ;

 case Model of
  $FF: S := GetString(dlMachineTypeFF);
  $FE: S := GetString(dlMachineTypeFE);
  $FD: S := GetString(dlMachineTypeFD);
  $FC: case SubModel of
       {1: S := GetString(dlMachineTypeFC_1);}
        2: S := GetString(dlMachineTypeFC_2);
        4: S := GetString(dlMachineTypeFC_4);
        5: S := GetString(dlMachineTypeFC_5);
        6: S := GetString(dlMachineTypeFC_6);
        8: S := GetString(dlMachineTypeFC_8);
      $0b: S := GetString(dlMachineTypeFC_0b);
      $20: S := GetString(dlMachineTypeFC_20);
      $42: S := GetString(dlMachineTypeFC_42);
      $45: S := GetString(dlMachineTypeFC_45);
      $48: S := GetString(dlMachineTypeFC_48);
      $4F: S := GetString(dlMachineTypeFC_4F);
      $50: S := GetString(dlMachineTypeFC_50);
      $51: S := GetString(dlMachineTypeFC_51);
      $52: S := GetString(dlMachineTypeFC_52);
      $94: S := GetString(dlMachineTypeFC_94);
        else S := GetString(dlMachineTypeFC_else);
       end;
  $FB,$86: S := GetString(dlMachineTypeFB86);
  $80: S := GetString(dlMachineType80);
  $FA: if SubModel = 1 then S := GetString(dlMachineTypeFA_1)
                       else S := GetString(dlMachineTypeFA_else);
  $F9: S := GetString(dlMachineTypeF9);
  $F8: if SubModel in [4,9,$B]
                            then S := GetString(dlMachineTypeF8_4_9_B)
                            else S := GetString(dlMachineTypeF8_else);
  $B6: S := GetString(dlMachineTypeB6);
  $9A: S := GetString(dlMachineType9A);
  $2D: S := GetString(dlMachineType2D);
  $E1: S := GetString(dlMachineTypeE1);
  $30: S := GetString(dlMachineType30);
  else S := GetString(dlMachineTypeElse);
 end;
end;

Procedure GetInfo(var MType, CPU, MHz, Family, Model, Step, CoCPU: String);
var id : cpuid1Layout;
begin

  GetMType(MType);
  CPU    := cpu_Type;
  MHz    := ItoS(ncpu_Speed) + ' ' + GetString(dlMHz);
  Family := ItoS((cpuid1 and $0F00) shr 8);
  Model  := ItoS((cpuid1 and $00F0) shr 4);
  Step   := ItoS((cpuid1 and $000F));
  CoCPU  := fpu_Type;
  CpuFlag.longWord:=0;
  if (cpuid1 and $0F00) shr 8 >= 5 then
  asm
   push dx

   db   $66
   xor  AX,AX
   inc  AX
   db   $0F,$A2         {CPUID}
   db   $66
   mov  word ptr CpuFlag.LoWord, DX
   db   $66
   shr  DX, 16
   mov  word ptr CpuFlag.HiWord, DX

   pop  DX
  end;
end;

        {-DataCompBoy-}
procedure GetHDDInfo(N: Byte; var S: String); {changed by piwamoto}
{one more bugfix from piwamoto: work fine under pure DOS or OS2 now}
 Type tHDPT=Record
             Cyl: Word;     { ,-  Bitfields for AT fixed disk control byte:}
             Hds: Byte;     { ;   Bit(s)  Description     (Table 03198)    }
             Reserv1: Word; { ;    0      unused                           }
             Reserv2: Word; { ;    1      reserved (0)  (disable IRQ)      }
             Reserv3: Byte; {,'    2      reserved (0)  (no reset)         }
             Control: Byte;{<      3      set if more than 8 heads         }
             Reserv4: Byte; {',    4      always 0                         }
             Reserv5: Byte; { ;    5      set if manufacturer's defect map }
             Reserv6: Byte; { ;           on max cylinder+1                }
             LandZone:Word; { ;    6      disable ECC retries              }
             Sct: Byte;     { `-   7      disable access retries           }
             WordAlingByte: Byte;{For align record size to 10h bytes}
            end;
    {DataCompBoy: All "reserved" variables are defined for XT only, so I labeld}
    {             them as reserved                                             }

 var
     NumCylinders, NumSect, NumHeads : LongInt;
     HDDint : Word;
     Param : ^tHDPT;
     Reg : Registers;
{$IFDEF DPMI}
type OS = record O, S : Word; end;
Label Er;
{$ENDIF}
BEGIN
 Case N of
  1 : HDDint := $46
 else HDDint := $41
 end;
{$IFNDEF DPMI}
 GetIntVec(HDDint, Pointer(Param));
{$ELSE}
 asm
  mov  AX, 0200h
  mov  BX, HDDint
  int  31h
  jc   Er
  mov  Param.OS.O, DX
  mov  BX, CX
  mov  AX, 0002h
  int  31h
  jc   Er
  mov  Param.OS.S, AX
 end;
{$ENDIF}
 NumCylinders := Param^.Cyl;
 NumHeads := Param^.Hds;
 NumSect := Param^.Sct;

 if (NumCylinders=0) or (NumSect=0) or (NumHeads=0) then
    begin
{$IFDEF DPMI}
Er:
{$ENDIF}
     Reg.AH := 8;
     Reg.DL := $80 + N;
     Intr ($13, Reg);
     NumCylinders := LongInt(Reg.CH) + LongInt((Reg.CL shr 6))*256 + 1;
     NumSect := LongInt(Reg.CL and $3F);
     NumHeads := LongInt(Reg.DH) + 1;
    end;
 S := FStr(NumCylinders*NumSect*NumHeads div 2048);
 S := S + 'M, ' + ItoS(NumHeads)+GetString(dlSI_Heads)+ItoS(NumCylinders)+
      GetString(dlSI_Tracks)+ItoS(NumSect)+GetString(dlSI_SectTrack);
END;

procedure GetDisks(var FDD: String);
 var NumFD: Byte;
     NumHD, NumCyl, NumSect, NumHeads: Byte;
     S: String;
     NumCylinders: Word;
     I: Byte;
begin
 NumFD := (mem[seg0040:$10] and 1)*(1+mem[seg0040:$10] shr 6);
 FDD := '';
 if NumFD = 0 then FDD := GetString(dlSI_NotPresent) else
 for I := 1 to NumFD do
  begin
   asm
    mov ah, 08h
    mov dl, I
    dec dl
    int 13h
    mov NumHD, bl
   end;
   case NumHD of
    1: S := '5.25" 360K';
    2: S := '5.25" 1.2M';
    3: S := '3.5" 720K';
    4: S := '3.5" 1.44M';
    6: S := '3.5" 2.88M';
    else S := GetString(dlUnknownDiskDriverType);
   end;
   if FDD <> '' then FDD := FDD + ', ';
   FDD := FDD + S;
  end;
end;

Procedure ShowCPUFlagInfo(var CPUFlagInfo: TCPUFlagInfo);

Const cFlag10 = 9;
      cFlag11 = 11;
      cFlag20 = 2;
      cFlag21 = 7;
      cFlag22 = 9;

var  s: string;
     D: PDialog;
     i: integer;

     MM: record
           S: array[0..20] of longint;
         end;

     cPresent,
     cAbsent: string[10];

begin

{  s:=  long2str(CpuFlagInfo.LoWord,8)+chr(13);
   s:=s+long2str(CpuFlagInfo.HiWord,8)+chr(13);

  s:=Long2Str(CPUFlagInfo.LongWord, 16); }

  cPresent := GetString(dlPresent);
  cAbsent  := GetString(dlAbsent);

  for i:=0 to cFlag10 do begin
    if (CpuFlagInfo.LoWord and (1 shl i))>0 then
      MM.S[i]:=longint(@cPresent)
    else
      MM.S[i]:=longint(@cAbsent);
  end;

  for i:=cFlag11 to 15 do begin
    if (CpuFlagInfo.LoWord and (1 shl i))>0 then
      MM.S[i-1]:=longint(@cPresent)
    else
      MM.S[i-1]:=longint(@cAbsent);
  end;

  for i:=0 to cFlag20 do begin
    if (CpuFlagInfo.HiWord and (1 shl i))>0 then
      MM.S[i+15]:=longint(@cPresent)
    else
      MM.S[i+15]:=longint(@cAbsent);
  end;

  for i:=cFlag21 to cFlag22 do begin
{    s:=Long2Str(i,2); }
    if (CpuFlagInfo.HiWord and (1 shl i))>0 then
      MM.S[i+11]:=longint(@cPresent)
    else
      MM.S[i+11]:=longint(@cAbsent);
  end;

  D := PDialog(LoadResource(dlgCpuFlagInfo));
  if D <> nil then begin
     D^.SetData(MM);
     DeskTop^.ExecView(D);
     Dispose(D,Done);
  end;
end;


procedure SystemInfo;
var
    D: PDialog;
    R: TRect;
    P: PView;
    MType,
    Mhz,
    CPU,
    CoCPU,
    Family,
    Model,
    Step,
    stHD,
    ndHD,
    FDDs: String[80];
    Btype: String[30];
    S: String;
    S2: String;
    Prc: String[20];
    EQList, Y, XMSSize: Word;
    LL: Array[1..10] of LongInt;
    w: word;

function DosVendor: string; {piwamoto}
var
     Reg         : Registers;
begin
 Reg.AX := $3000;
 Intr ($21, Reg);
 case Reg.BH of
  $00 : DosVendor := 'PC-';
  $66 : DosVendor := 'PTS-';
  $fd : DosVendor := 'Free';
  $ff : DosVendor := 'MS-';
  else  DosVendor := '';
 end;
end; {DosVendor}

function OS2Build: string; {piwamoto}
var
     Reg   : Registers;
     Build : String;
begin
 Reg.AX := $3306;
 Intr ($21, Reg);
 if (Reg.BX = $1e14{Warp 3.0}) and (Reg.DL < $c0)
    then Reg.DH := 1 else Reg.DH := 0;
 str(Reg.DX, Build);
 while length(Build) < 3 do Build := '0' + Build;
 OS2Build := Build;
end; {OS2Build}

function WinVer: string; {piwamoto}
var
     Reg   : Registers;
     Build : String;
begin
 Reg.AX := $1600;
 Intr ($2f, Reg);
 case Reg.AX of
  $0004 : Build := '95'; {OSR1=4.00.950B & OSR2=4.00.1111 returns same code}
  $0304 : Build := '95 OSR 2.1'; {4.00.1212}
  $0a04 : Build := '98';         {4.10.1998}
 else
  Build := ItoS(LongInt(Reg.AL)) + '.';
  if Reg.AH < 10 then Build := Build + '0' + ItoS(LongInt(Reg.AH))
   else Build := Build + ItoS(LongInt(Reg.AH));
 end;
 WinVer := Build;
end;

function DosEMUver: string; {DosEMU detection by DataCompBoy}
var P: Pointer;
    Reg: Registers;
type OS = record O, S : Word; end;
begin
 DosEMUver:='';
 GetIntVec($0E6, P);
 if OS(P).S<>$0F000 then exit;
 Reg.AX:=0;
 Intr($0E6, Reg);
 if Reg.AX<>$0AA55 then exit;
 DosEMUver:='DosEMU '+ItoS(Reg.BH)+'.'+ItoS(Reg.BL)+'.'+ItoS(Reg.CX);
end;

begin
 R.Assign(1,1,73,22);
 New(D, Init(R, GetString(dlSystemInfo)));
 D^.Options := D^.Options or ofCentered;
 D^.HelpCtx := hcSystemInfo;

 GetInfo(MType, CPU, MHz, Family, Model, Step, CoCPU); {Denis Shamov, 2:5057/40}

 LL[1] := LongInt(@MType);
 LL[2] := LongInt(@CPU);
 LL[3] := LongInt(@MHz);
 LL[4] := LongInt(@Family);
 LL[5] := LongInt(@Model);
 LL[6] := LongInt(@Step);
 LL[7] := LongInt(@CoCPU);

 FormatStr(S, GetString(dlSI_Main), LL);
 R.Assign(3,2,69,7);
 P := New(PStaticText, Init(R, S));
 P^.Options := P^.Options or ofFramed;
 D^.Insert(P);
 S := GetString(dlSI_MainBoard);
 R.Assign(4,1,6+Length(S),2);
 P := New(PLabel, Init(R, S,P));
 D^.Insert(P);

 Y := 1;
 GetDisks(FDDs); S := GetString(dlSI_FloppyDrives)+FDDs+^M;
 if mem[seg0040:$75] <> 0 then begin
  GetHDDInfo(0, stHD);
  S := S + GetString(dlSI_1stHard) + stHD+^M;
  Inc(Y);
  if mem[seg0040:$75] > 1 then begin
   GetHDDInfo(1, ndHD);
   S := S + GetString(dlSI_2ndHard) + ndHD;
   Inc(Y);
  end;
 end;
 R.Assign(3,9,69,9+Y);
 P := New(PStaticText, Init(R, S));
 P^.Options := P^.Options or ofFramed;
 D^.Insert(P);
 S := GetString(dlSI_DiskDrivers);
 R.Assign(4,8,6+Length(S),9);
 P := New(PLabel, Init(R, S,P));
 D^.Insert(P);

{new memory detection scheme by piwamoto}
 XMSSize := 0;
 EQList  := 0;
 asm
  mov   ax,$0e801
  int   $15
  jc    @@ReadMemFromCMOS
  cmp   ax,$3c00
  ja    @@ReadMemFromCMOS
  push  ax
  or    ax,bx
  pop   ax
  jnz   @@AXBXvalid
  mov   ax,cx
  mov   bx,dx
 @@AXBXvalid:
  mov   EQList,bx
  jmp   @@ExitMemDet

 @@ReadMemFromCMOS:
  mov al, $18
  mov dx, $70
  out dx, al
  inc dx
  in  al, dx
  mov ah, al
  dec dx
  mov al, $17
  out dx, al
  inc dx
  in  al, dx
 @@ExitMemDet:
  mov XMSSize, ax
 end;
 S := GetString(dlSI_TotalMem) + ItoS(LongInt(EQList)*64 + XMSSize + 1024) + 'K'^M;
 if XMSFound then S := S + GetString(dlSI_Extended) + ItoS(XMSFree) + 'K'^M;
 if EMSFound then S := S + GetString(dlSI_Expanded) + ItoS(EMSSize) + 'K';
 R.Assign(3,11+Y,25,14+Y);
 P := New(PStaticText, Init(R, S));
 P^.Options := P^.Options or ofFramed;
 D^.Insert(P);
 S := GetString(dlSI_Memory);
 R.Assign(4,10+Y,6+Length(S),11+Y);
 P := New(PLabel, Init(R, S,P));
 D^.Insert(P);

 asm
  xor ax, ax
  int 11h
  mov EQList, ax
 end;
 S := GetString(dlSI_COM);
 if EQList and $0E00 <> 0 then S := S + ItoS((EQList and $0E00) shr 9) else S := S + GetString( dlSI_NotPresent );
 S := S + GetString(dlSI_Lpt);
{LPT detection by piwamoto}
 EQList := (EQList and $C000) shr 14;
 if ((opSys and (opWin or opOS2))=opWin) and (EQList = 3) then
  begin
   inc (EQList);
   repeat
    dec (EQList);
   until (EQList = 0) or (memw[seg0040:$06+EQList*2] <> 0);
  end;
 if EQList = 0 then S := S + GetString( dlSI_NotPresent ) else S := S + ItoS(EQList);

{advanced OS detection by piwamoto}
{DosEMU detection by DataCompBoy}
 S := S + GetString(dlSI_OSVer);
 EQList := DOSVersion;
 S2 := DosEMUver;
 if S2 = '' then
  if (opSys and opOS2)<>0 then begin
   S := S + 'OS/2 ';
   case EQList of
    $1e14 : S := S + 'Warp 3 (8.' + OS2Build + ')';
    $2814 : S := S + 'Warp 4 (9.' + OS2Build + ')';
    $2d14 : S := S + 'WSeB';  {does anybody know revision codes for WSeB?}
    else    S := S + ItoS(WordRec(EQList).Lo div 10) + '.' +
                     ItoS(WordRec(EQList).Hi)
   end;
  end
  else if (opSys and opWNT)=opWNT then S := S + 'Windows NT or Windows 2000'
   else if (opSys and opWin)<>0 then S := S + 'Windows ' + WinVer
    else S := S + DosVendor + 'DOS ' + ItoS(WordRec(EQList).Lo) + '.' +
              ItoS(WordRec(EQList).Hi)
 else S := S + S2;
 R.Assign(27,11+Y,69,14+Y);
 P := New(PStaticText, Init(R, S));
 P^.Options := P^.Options or ofFramed;
 D^.Insert(P);
 S := GetString(dlSI_Ports);
 R.Assign(29,10+Y,31+Length(S),11+Y);
 P := New(PLabel, Init(R, S,P));
 D^.Insert(P);

 R.Assign(1, 18, 11, 20);
 P := New(PButton, Init(R, GetString(dlOKButton), cmOk, bfDefault));
 P^.Options := P^.Options or ofCenterX;
 D^.Insert(P);

 { Added by Excelence }
 if CPUFlag.longWord<>0 then
 begin
   D^.InsertBefore(New(PButton, Init(R, GetString(dlDetailButton), cmYes, bfNormal)), P);
   D^.FocusNext(False);
   repeat
    w:=DeskTop^.ExecView(D);
    if w=cmYes then ShowCPUFlagInfo(CPUFlag);
   until (w<>cmYes);
 end else w:=DeskTop^.ExecView(D);

 Dispose(D,Done);
end;

{$ELSE}
PROCEDURE SystemInfo; begin end;
{$ENDIF}

end.
