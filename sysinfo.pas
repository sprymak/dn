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
//  dn16rc1-Bugfixed_8Gb_HDD_sysinfo.patch
//
//  2.0.0
//  dn200-remove_Machine_Type_and_CPU_flag_info_from_sysinfo.patch
//
//  2.3.0
//  dn3323-HDD_in_sysinfo.patch
//
//  3.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

UNIT SysInfo;

INTERFACE

{$IFDEF SYSINFO}
USES
  CPUType, Advance;
{$ENDIF}

    PROCEDURE SystemInfo;

IMPLEMENTATION

{$IFDEF SYSINFO}
USES Dos,      Objects, Views,  DNApp,   Drivers,  Commands, RStrings,
     Messages, Dialogs, DNHelp, Collect, Advance1, Advance3, ExtraMem
     {$IFDEF DPMI},DPMI,DosMem{$ENDIF}
     ;


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
 Type
  PDriveInfo = ^TDriveInfo;
  TDriveInfo = record
     Size: Word;
     Flags: Word;
     Cylinders: LongInt;
     Heads: LongInt;
     Sectors: LongInt;
     LowLBASectors: LongInt;
     HiLBASectors: LongInt;
     SectorSize: Word;
  end;

 var
     NumCylinders, NumSect, NumHeads, numLBASectors : LongInt;
     HDDint : Word;
     Param : ^tHDPT;
     Reg : {$IFDEF DPMI}DPMIRegisters{$ELSE}Registers{$ENDIF};
{$IFDEF DPMI}
type OS = record O, S : Word; end;
Label Er;
{$ENDIF}

function GetExtendedInfo(N:Word; var numLBASectors : LongInt): Boolean;
{ original version by Max Morozov }
{ fixed by Max Piwamoto }
{ get number of total sectors on drive }
{ CHS information, returned by function 0x48 is useless }
var
{$IFDEF DPMI}
   Info:  PDriveInfo;
{$ELSE}
   Info:  TDriveInfo;
   Drive, Status: Byte;
{$ENDIF}
begin
   GetExtendedInfo := False;
   NumLBASectors := 0;
   {$IFDEF DPMI}FillChar(Reg, SizeOf(Reg), 0);{$ENDIF}
{ test BIOS extension presence }
   Reg.AX := $4100;
   Reg.DL := $80 + N;
   Reg.BX := $55aa;
   Reg.Flags := fCarry;
   asm stc end;
   {$IFDEF DPMI}SimulateRealModeInt{$ELSE}Intr{$ENDIF}($13,Reg);
   if ((Reg.Flags and fCarry)<>0) or (Reg.BX <> $aa55) then Exit;

{$IFDEF DPMI}
   Info := PDriveInfo(GetDosMem( SizeOf(TDriveInfo), Reg.DS ));
   if Info = nil then Exit;
   Info^.Size := sizeof(TDriveInfo);
   Reg.AX := $4800;
   Reg.DL := $80 + N;
   Reg.SI := 0;
   Reg.Flags := fCarry;
   asm stc end;
   SimulateRealModeInt( $13, Reg );
   if (Reg.Flags and fCarry) = 0 then NumLBASectors := Info^.LowLBASectors;
   FreeDosMem( Info );
{$ELSE}
   Info.Size := sizeof(TDriveInfo);
   Drive:=$80 + N;
   Status := 0;
   asm
      mov ax,4800h
      mov dl,Drive
      push ds
      push ss
      lea si, Info
      pop  ds
      stc
      int 13h
      pop ds
      jnc @1
      mov Status,ah
   @1:
   end;
   if Status = 0 then NumLBASectors := Info.LowLBASectors;
{$ENDIF}
   GetExtendedInfo := (NumLBASectors <> 0);
end;


BEGIN
 Case N of
  1 : HDDint := $46
 else HDDint := $41
 end;
 if N < 2 then
 begin
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
  {$IFDEF DPMI}
  Er:
  {$ENDIF}
 end;{N<2}

 Reg.AH := 8;
 Reg.DL := $80 + N;
 {$IFDEF DPMI}SimulateRealModeInt{$ELSE}Intr{$ENDIF}($13,Reg);
 if Reg.AH <> 0 then begin S := ''; Exit; end {no HDD found}
 else if (NumCylinders=0) or (NumSect=0) or (NumHeads=0) then
        begin
         NumCylinders := LongInt(Reg.CH) + LongInt((Reg.CL shr 6))*256 + 1;
         NumSect := LongInt(Reg.CL and $3F);
         NumHeads := LongInt(Reg.DH) + 1;
        end;
 if (NumCylinders*NumSect*NumHeads = $fb0400){8Gb limit} and
    (GetExtendedInfo(N, NumLBASectors)) then
    NumCylinders := NumLBASectors div NumHeads div NumSect;
 S := FStr(NumCylinders*NumSect*NumHeads div 2048);
 S := GetString(dlSI_HardDrive) + Chr($31{'1'} + N) + ' : ' + S + 'M, ' +
      ItoS(NumHeads) + GetString(dlSI_Heads) +
      ItoS(NumCylinders) + GetString(dlSI_Tracks) +
      ItoS(NumSect) + GetString(dlSI_SectTrack);
END;

procedure GetDisks(var FDD: String);
 var
     NumFD, NumHD, I: Byte;
     S: String;
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
    5,6 : S := '3.5" 2.88M'; { Rainbow }
    $10 : S := 'ATAPI';
    else S := GetString(dlUnknownDiskDriverType);
   end;
   if FDD <> '' then FDD := FDD + ', ';
   FDD := FDD + S;
  end;
end;


procedure SystemInfo;
var
    EQList, Y, XMSSize: Word;
    LL: Array[1..6] of LongInt;
    D: PDialog;
    R: TRect;
    P: PView;
    HDDcount: Byte;
    Mhz,
    CPU,
    CoCPU,
    Family,
    Model,
    Step,
    S2: String[64];
    S: String;

function DosVendor: string; {piwamoto}
var
     Reg   : Registers;
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
  $0a04 : Build := '98'; {4.10.1998}{4.10.2222}
  $5a04 : Build := 'Millennium Edition'; {4.90.3000} { Rainbow }
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

 CPU    := cpu_Type;
 MHz    := ItoS(ncpu_Speed) + ' ' + GetString(dlMHz);
 Family := ItoS((cpuid1 and $0F00) shr 8);
 Model  := ItoS((cpuid1 and $00F0) shr 4);
 Step   := ItoS((cpuid1 and $000F));
 CoCPU  := fpu_Type;

 LL[1]  := LongInt(@CPU);
 LL[2]  := LongInt(@MHz);
 LL[3]  := LongInt(@Family);
 LL[4]  := LongInt(@Model);
 LL[5]  := LongInt(@Step);
 LL[6]  := LongInt(@CoCPU);

 FormatStr(S, GetString(dlSI_Main), LL);
 R.Assign(3,2,69,5);
 P := New(PStaticText, Init(R, S));
 P^.Options := P^.Options or ofFramed;
 D^.Insert(P);

 S := GetString(dlSI_MainBoard);
 R.Assign(4,1,6+Length(S),2);
 P := New(PLabel, Init(R, S,P));
 D^.Insert(P);

 Y := 1;
 GetDisks(S2); S := GetString(dlSI_FloppyDrives) + S2;

 for HDDcount := 0 to 3 do
 begin
   GetHDDInfo(HDDcount, S2);
   if S2 <> '' then
     begin
      S := S + S2;
      Inc(Y);
     end;
 end;

 R.Assign(3,7,69,7+Y);
 P := New(PStaticText, Init(R, S));
 P^.Options := P^.Options or ofFramed;
 D^.Insert(P);
 S := GetString(dlSI_DiskDrivers);
 R.Assign(4,6,6+Length(S),7);
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
 R.Assign(3,9+Y,25,12+Y);
 P := New(PStaticText, Init(R, S));
 P^.Options := P^.Options or ofFramed;
 D^.Insert(P);
 S := GetString(dlSI_Memory);
 R.Assign(4,8+Y,6+Length(S),9+Y);
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
 if (EQList = 3) then
   while (EQList > 0) and (memw[seg0040:$06+EQList*2] = 0) do dec (EQList);
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
    $2d14 : S := S + 'WSeB (14.' + OS2Build + ')'; {tested by piwamoto}
    else    S := S + ItoS(WordRec(EQList).Lo div 10) + '.' +
                     ItoS(WordRec(EQList).Hi)
   end;
  end
  else if (opSys and opWNT)=opWNT then S := S + 'Windows NT/2000/XP'
   else if (opSys and opWin)<>0 then S := S + 'Windows ' + WinVer
    else S := S + DosVendor + 'DOS ' + ItoS(WordRec(EQList).Lo) + '.' +
              ItoS(WordRec(EQList).Hi)
 else S := S + S2;
 R.Assign(27,9+Y,69,12+Y);
 P := New(PStaticText, Init(R, S));
 P^.Options := P^.Options or ofFramed;
 D^.Insert(P);
 S := GetString(dlSI_Ports);
 R.Assign(28,8+Y,30+Length(S),9+Y);
 P := New(PLabel, Init(R, S,P));
 D^.Insert(P);

 R.Assign(1, 18, 11, 20);
 P := New(PButton, Init(R, GetString(dlOKButton), cmOk, bfDefault));
 P^.Options := P^.Options or ofCenterX;
 D^.Insert(P);

 DeskTop^.ExecView(D);
 Dispose(D,Done);
end;

{$ELSE}
PROCEDURE SystemInfo; begin end;
{$ENDIF}

end.
