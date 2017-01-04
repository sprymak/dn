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
//  dn16rc1-Novell_compatibility_related_fix_diff145byMV.patch
//  dn16rc1-OS2_compatibility_related_fix_diff147byMV.patch
//  dn16rc1-rename_file_dpmi_lfn.patch
//  dn16rc1-W2K_compatibility_fix-diff156byMV.patch
//
//  2.0.0
//  dn21225-FileCopy(f)-copy_more_then_2Gb_fix.patch
//
//  3.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

{This unit based on LFN.PAS unit from "Connect"}
{w95api error handlers fixed by piwamoto}
{lSIM95 api by DataCompBoy 19.07.2000 04:04:29}

unit LFN;

interface

uses Dos, DnINI;

Procedure LfnInitUnit;

type
  lAPIType = (lDOS, lWIN95{$IFDEF DPMI}, lSIM95{$ENDIF});  {API currently used}

  TSize = Comp; {64 bit integer type for file sizes}

   {Extended search structure to be used instead of SearchRec}
  lSearchRec = record
    SR: SearchRec;     {Basic field set (SR.Name - short name)}
    FileHandle: Word;  {Search handle, undefined in lDOS mode}
    FullSize: TSize; {True file size}
    LoCreationTime: Longint; {Time created (low-order byte)}
    HiCreationTime: Longint; {Time created (high-order byte)}
    LoLastAccessTime: Longint; {Time accessed (low-order byte)}
    HiLastAccessTime: Longint; {Time accessed (high-order byte)}
    LoLastModificationTime: Longint; {Time modified (low-order byte)}
    HiLastModificationTime: Longint; {Time modified (high-order byte)}
    FullName: string;  {True file name or short name if LFNs not available}
    FindFirstMode: lAPIType; {Type of service used for search}
    { Other fields will be added later }
  end;

  TNameZ = Array[0..259] of Char;
  {The type for file names. (String is suitable for most purpuses)}

  lFile = record {Extended file record to be used instead of File}
    F: File;
    FullName: TNameZ;
    AssignFileMode: lAPIType;
    { Other fields will be added later }
  end;

  lText = record {Extended text file record to be used instead of Text}
    T: Text;
    FullName: TNameZ;
    AssignTextMode: lAPIType;
    { Other fields will be added later }
  end;

        {   Basic parameters   }
const
  lAPI: lAPIType = lDOS;  {  Current API  }

  ltMod = 0;    {Store time modified}
  ltAcc = 1;    {Store time accessed}
  ltCre = 2;    {Store time created}
  LFNTimes: Byte = ltMod; { What time info to store in lSearchRec.SR.Time? }

  MaxPathLen: Byte = 79;  { Maximum name length for the present moment }

  LFNPresent: Boolean = false; { Are LFNs available }

  Const IllegalChars:String[15] = ';,=+<>|"[]'; { Characters invalid for short names }
      IllegalCharSet:Set Of Char = [';',',','=','+','<','>','|','"','[',']']; { Characters invalid for short names }

  Const IllegalCharsDos:String[15] = ';,=+<>|"[]'; { Characters invalid for DOS names }
      IllegalCharSetDos:Set Of Char = [';',',','=','+','<','>','|','"','[',']']; { Characters invalid for DOS names }

           { Returns true if LFNs are unavailable near the given path }
function LFNDisabled(const Path: String): Boolean;

       { File searching routines. lFindClose must be called /in all cases/ }
procedure lFindFirst(const Path: String; Attr: Word; var R: lSearchRec);
procedure lFindNext(var R: lSearchRec);
procedure lFindClose(var R: lSearchRec);

        { Name retrieval functions }
procedure lGetShortFileName(const Name: String; var ShortName: String);
function lfGetShortFileName(const Name: String): String;
procedure lGetLongFileName(const Name: String; var LongName: String);
function lfGetLongFileName(const Name: String): String;

        { Name correction routine }
procedure lTrueName(const Name: String; var S: String);

   { Basic file operation routines. To use IO functions from standard units,
              specify lFile.F or lText.T }
procedure lAssignFile(var F: lFile; const Name: String);
procedure lAssignText(var T: lText; const Name: String);
procedure lResetFile(var F: lFile; RecSize: Word);
procedure lResetFileReadOnly(var F: lFile; RecSize: Word);
procedure lRewriteFile(var F: lFile; RecSize: Word);
procedure lResetText(var F: lText);
procedure lResetTextReadOnly(var F: lText);
procedure lRewriteText(var F: lText);
procedure lAppendText (var T: lText);
procedure lEraseFile(var F: lFile);
procedure lEraseText(var T: lText);
procedure lRenameFile(var F: lFile; const NewName: String);
procedure lRenameText(var T: lText; const NewName: String);
function  lFileNameOf(var lF: lFile): string;
function  lTextNameOf(var lT: lText): string;

                 { Extended file attributes manipulation }
procedure lGetFAttr(var F: lFile; var Attr: Word);
procedure lSetFAttr(var F: lFile; Attr: Word);
procedure lGetTAttr(var T: lText; var Attr: Word);
procedure lSetTAttr(var T: lText; Attr: Word);

                           { Directory manipulation }
procedure lMkDir(const Path: String);
procedure lRmDir(const Path: String);
procedure lChDir(const Path: String);
procedure lGetDir(D: Byte; var Path: String);

         { Name expansion and splitting }
function lFExpand(const Path: String): String;
procedure lFSplit(const Path: String; var Dir, Name, Ext: String);

{--- start -------- Eugeny Zvyagintzev ---- 01-02-2003 -----}
function lFileSize(var F: lFile): TSize;
Function lSeek(var F: lfile;NewPos: TSize;Direction: Byte): TSize;
Function lFilePos(var F: lfile): TSize;
{--- finish -------- Eugeny Zvyagintzev ---- 01-02-2003 -----}

const AnyFile = Dos.AnyFile and not VolumeID;

implementation

uses advance1,
     Startup,
     Commands
     {$IFDEF DPMI}
     ,Dpmi, DosMem, Advance
     {$ENDIF}
     ;

 {$IFDEF DPMI}
 Const LowSeg: Word = 0;
       LowBuf: Pointer = nil;
 Var
       RMR: DPMIRegisters;
 {$ENDIF}

 Function StrPas(S: Array Of Char): String;
  var ss: string;
      i: word;
  Begin
   ss:='';
   For i:=Low(S) to High(S) do
    If (i<255) and (s[i]<>#0)
     then ss:=ss+s[i]
     else break;
   StrPas:=ss;
  End;

Const CheckingApi: Boolean = False;
      DriveBuffer: array[1..4] of char = ('?',':','\',#0);
function LFNDisabled(const Path: String): Boolean;
var
  Drive: Char;
  Regs: Registers;
begin
  LFNDisabled:=true;
  if not CanUseLFN then exit;
  if CheckingApi then begin LFNDisabled:=False; exit; end;
  if not LFNPresent then exit;
  if (Length(Path) > 1) and
     ((word((@Path[1])^)=$2F2F{//}) or (word((@Path[1])^)=$5C5C{\\}))
   then begin LFNDisabled := False; exit end
   else begin
         if (Length(Path) > 1) and (Path[2] = ':')
          then Drive := Upcase(Path[1])
{$IFDEF DPMI}
          else with RMR do begin
                AH := $19;
                SimulateRealModeInt($21, RMR);
{$ELSE}
          else with Regs do begin
                AH := $19;
                Intr($21, Regs);
{$ENDIF}
                Drive := Char(AL + $41);
               end;
         if SystemData.Drives[Drive] and ossNoLFN <> 0 then exit;
        end;
  case lAPI of
   lDOS: LFNDisabled:=true;
   lWIN95: begin
            DriveBuffer[1]:=Drive;
            with regs do begin
             ax    := $71a0;
             ds    := seg(DriveBuffer);
             dx    := ofs(DriveBuffer);
             es    := 0;
             di    := 0;
             cx    := 0;
             bx    := 0;
             flags := fCarry;
             asm stc end;
             intr($21, regs);
             if (flags and fCarry=0) then LFNDisabled:=((Bx and $4000)=0);
            end;
           end;
   {$IFDEF DPMI}
   lSIM95: begin
            DriveBuffer[1]:=Drive;
            Move(DriveBuffer, LowBuf^, SizeOf(DriveBuffer));
            with RMR do begin
             ax    := $71a0;
             ds    := LowSeg;
             dx    := 0;
             es    := 0;
             di    := 0;
             cx    := 0;
             bx    := 0;
             flags := fCarry;
             asm stc end;
             SimuLateRealModeInt($21, RMR);
             if (flags and fCarry=0) then LFNDisabled:=((Bx and $4000)=0);
            end;
           end;
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure NameToNameZ(const Name: String; var NameZ: TNameZ);
begin
  Move(Name[1], NameZ, Length(Name));
  NameZ[Length(Name)] := #0;
end;

procedure CheckColonAndSlash(const Name: String; var S: String);
var
  ColonPos: Integer;
begin
  ColonPos := Pos(':', S);
  if (ColonPos > 2) and (Name[2] = ':') then
  begin
    Delete(S, 1, ColonPos - 1);
    S := Name[1] + S;
  end;

  if Name[Length(Name)] <> '\' then
    while S[Length(S)] = '\' do Dec(S[0])
  else if (Name[Length(Name)] = '\') and
    (S[Length(S)] <> '\') and (Length(S) < 255) then
  begin
    Inc(S[0]);
    S[Length(S)] := '\';
  end;
end;

(*
 Offset  Size    Description
  00h    DWORD   file attributes
                 bits 0-6 standard DOS attributes
                 bit 8: temporary file
  04h    QWORD   file creation time
                 (number of 100ns intervals since 1/1/1601)
  0Ch    QWORD   last access time
  14h    QWORD   last modification time
  1Ch    DWORD   file size (high 32 bits)
  20h    DWORD   file size (low 32 bits)
  24h  8 BYTEs   reserved
  2Ch 260 BYTEs  ASCIZ full filename
 130h 14 BYTEs   ASCIZ short filename (for backward compatibility)
*)

type
  lFindDataRec = record
    LoAttr: Word;
    HiAttr: Word;
    LoCreationTime: Longint;
    HiCreationTime: Longint;
    LoLastAccessTime: Longint;
    HiLastAccessTime: Longint;
    LoLastModificationTime: Longint;
    HiLastModificationTime: Longint;
    HiSize: Longint;
    LoSize: Longint;
    Reserved: Array[0..7] of Byte;
    FullName: TNameZ;
    ShortName: Array[0..13] of Char;
  end;

{--- start -------- Eugeny Zvyagintzev ---- 01-02-2003 -----}
type
  lFileInfoDataRec = record
    Attr: Longint;
    LoCreationTime: Longint;
    HiCreationTime: Longint;
    LoLastAccessTime: Longint;
    HiLastAccessTime: Longint;
    LoLastWriteTime: Longint;
    HiLastWriteTime: Longint;
    VolumeSerialNumber: Longint;
    HiSize: Longint;
    LoSize: Longint;
    LinksNumber: Longint;
    HiUniqueId: Longint;
    LoUniqueId: Longint;
  end;
{--- finish -------- Eugeny Zvyagintzev ---- 01-02-2003 -----}

procedure FindDataToSearchRec(var FindData: lFindDataRec; var R: lSearchRec);
begin
  R.SR.Attr := FindData.LoAttr;
  if LFNTimes = ltCre then R.SR.Time := FindData.LoCreationTime
   else if LFNTimes = ltAcc then R.SR.Time := FindData.LoLastAccessTime
    else R.SR.Time := FindData.LoLastModificationTime;
  if FindData.HiSize=0 then R.SR.Size := FindData.LoSize
                       else R.SR.Size := MaxLongInt;
  R.SR.Name := StrPas(FindData.ShortName);
  R.FullName := StrPas(FindData.FullName);
  if R.SR.Name = '' then
    R.SR.Name := R.FullName;
  R.LoCreationTime:= FindData.LoCreationTime;
  R.HiCreationTime:= FindData.HiCreationTime;
  R.LoLastAccessTime:= FindData.LoLastAccessTime;
  R.HiLastAccessTime:= FindData.HiLastAccessTime;
  R.LoLastModificationTime:= FindData.LoLastModificationTime;
  R.HiLastModificationTime:= FindData.HiLastModificationTime;
{--- start -------- Eugeny Zvyagintzev ---- 01-02-2003 -----}
{LoSize must be unsigned, but LongInt is signed}
{So we have to write LoSize and HiSize to FullSize directly}
  R.FullSize:=0;
  Move(FindData.LoSize,R.FullSize,SizeOf(FindData.LoSize));
  Move(FindData.HiSize,Ptr(Seg(R.FullSize),Ofs(R.FullSize)+SizeOf(FindData.LoSize)+1)^,SizeOf(FindData.HiSize));
{--- finish -------- Eugeny Zvyagintzev ---- 01-02-2003 -----}
end;

{$I LFN._WN}
{$IFDEF DPMI}{$I LFN._WS}{$ENDIF}

{--- start -------- Eugeny Zvyagintzev ---- 01-02-2003 -----}
Function lFileSize(var F: lfile): TSize;
Begin
 Case lApi Of
  lDOS: lFileSize:=FileSize(F.F);
  lWIN95: lFileSize:=lWIN95FileSize(F);
  {$IFDEF DPMI}
  lSIM95: lFileSize:=lSIM95FileSize(F);
  {$ENDIF}
  { Other APIs will be added later }
 End;
End;

Function lSeek(var F: lfile;NewPos: TSize;Direction: Byte): TSize;
Begin
 Case lApi Of
  lDOS,lWIN95: lSeek:=lWIN95Seek(F,NewPos,Direction);
  {$IFDEF DPMI}
  lSIM95: lSeek:=lSIM95Seek(F,NewPos,Direction);
  {$ENDIF}
  { Other APIs will be added later }
 End;
End;

Function lFilePos(var F: lfile): TSize;
Begin
 lFilePos:=lSeek(F,0,1);
End;
{--- finish -------- Eugeny Zvyagintzev ---- 01-02-2003 -----}

procedure lFindFirst(const Path: String; Attr: Word; var R: lSearchRec);
begin
  R.FullName := '';

  case lAPI of
    lDOS:
      begin
        R.FindFirstMode := lDOS;
        FindFirst(Path, Attr, R.SR);
        R.FullName := R.SR.Name;
        R.FullSize := R.SR.Size;
      end;
    lWIN95:
      if LFNDisabled(Path) then
      begin
        R.FindFirstMode := lDOS;
        FindFirst(Path, Attr, R.SR);
        R.FullName := R.SR.Name;
        R.FullSize := R.SR.Size;
      end
      else
      begin
        R.FindFirstMode := lWIN95;
        lWIN95FindFirst(Path, Attr, R);
      end;
    {$IFDEF DPMI}
    lSIM95:
      if LFNDisabled(Path) then
      begin
        R.FindFirstMode := lDOS;
        FindFirst(Path, Attr, R.SR);
        R.FullName := R.SR.Name;
        R.FullSize := R.SR.Size;
      end
      else
      begin
        R.FindFirstMode := lSIM95;
        lSIM95FindFirst(Path, Attr, R);
      end;
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lFindNext(var R: lSearchRec);
begin
  R.FullName := '';

  case R.FindFirstMode of
    lDOS: Begin
           FindNext(R.SR);
           R.FullName := R.SR.Name;
           R.FullSize := R.SR.Size;
         End;
    lWIN95: lWIN95FindNext(R);
    {$IFDEF DPMI}
    lSIM95: lSIM95FindNext(R);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lFindClose(var R: lSearchRec);
begin
  case R.FindFirstMode of
    lDOS: {In DOS no FindClose function...};
    lWIN95: lWIN95FindClose(R);
    {$IFDEF DPMI}
    lSIM95: lSIM95FindClose(R);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lGetShortFileName(const Name: String; var ShortName: String);
begin
  case lAPI of
    lDOS: ShortName := Name;
    lWIN95:
      if LFNDisabled(Name) then ShortName := Name
      else lWIN95GetFileNameFunc(Name, ShortName, 1);
    {$IFDEF DPMI}
    lSIM95:
      if LFNDisabled(Name) then ShortName := Name
      else lSIM95GetFileNameFunc(Name, ShortName, 1);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

function lfGetShortFileName(const Name: String): String;
 var s: String;
begin
  lGetShortFileName(Name, s);
 lfGetShortFileName:=s;
end;

procedure lGetLongFileName(const Name: String; var LongName: String);
begin
  case lAPI of
    lDOS: LongName := Name;
    lWIN95:
      if LFNDisabled(Name) then LongName := Name
      else lWIN95GetFileNameFunc(Name, LongName, 2);
    {$IFDEF DPMI}
    lSIM95:
      if LFNDisabled(Name) then LongName := Name
      else lSIM95GetFileNameFunc(Name, LongName, 2);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

function lfGetLongFileName(const Name: String): String;
 var s: String;
begin
  lGetLongFileName(Name, s);
 lfGetLongFileName:=s;
end;

(*
 INT 21h  AH=60
 INT 21 - DOS 3.0+ - "TRUENAME" - CANONICALIZE FILENAME OR PATH
         AH = 60h
         DS:SI -> ASCIZ filename or path
         ES:DI -> 128-byte buffer for canonicalized name
 Return: CF set on error
             AX = error code
                 02h invalid component in directory path
                     or drive letter only
                 03h malformed path or invalid drive letter
             ES:DI buffer unchanged
         CF clear if successful
             AH = 00h or 3Ah (DOS 6.1/6.2 for character device)
             AL = destroyed (00h or 2Fh or 5Ch or last character
                  of current directory on drive)
             buffer filled with qualified name of form
               D:\PATH\FILE.EXT or \\MACHINE\PATH\FILE.EXT
 Desc:   determine the canonical name of the specified
         filename or path, corresponding to the undocumented
         TRUENAME command in COMMAND.COM
 Notes:  the input path need not actually exist
         letters are uppercased, forward slashes converted
         to backslashes, asterisks converted to appropriate
         number of question marks, and file and directory
         names are truncated to 8.3 if necessary.
*)

procedure DOSTrueName(const Name: String; var S: String);
{$IFNDEF DPMI}
var
  Regs: Registers;
  NameZ, GetNameZ: TNameZ;
begin
  NameToNameZ(Name, NameZ);

  with Regs do
  begin
    AH    := $60;
    DS    := Seg(NameZ);
    SI    := Ofs(NameZ);
    ES    := Seg(GetNameZ);
    DI    := Ofs(GetNameZ);
    Flags := fCarry;
    asm stc end;
    Intr($21, Regs);
    if Flags and fCarry <> 0 then S := Name
    else
    begin
      S := StrPas(GetNameZ);
      CheckColonAndSlash(Name, S);
    end;
  end;
{$ELSE}
var
  NameZ, GetNameZ: ^TNameZ;
begin
  FillChar(RMR, SizeOf(RMR), 0);
  NameZ    := GetDosMem(SizeOf(TNameZ), RMR.DS);
  GetNameZ := GetDosMem(SizeOf(TNameZ), RMR.ES);
  NameToNameZ(Name, NameZ^);

  with RMR do
  begin
    AH    := $60;
    Flags := fCarry;
    SimulateRealModeInt($21, RMR);
    if Flags and fCarry <> 0 then S := Name
    else
    begin
      S := StrPas(GetNameZ^);
      CheckColonAndSlash(Name, S);
    end;
  end;

  FreeDosMem(NameZ);
  FreeDosMem(GetNameZ);
{$ENDIF}
end;

procedure lTrueName(const Name: String; var S: String);
begin
  case lAPI of
    lDOS: DOSTrueName(Name, S);
    lWIN95:
      if LFNDisabled(Name) then DOSTrueName(Name, S)
      else WIN95TrueName(Name, S);
    {$IFDEF DPMI}
    lSIM95:
      if LFNDisabled(Name) then DOSTrueName(Name, S)
      else SIM95TrueName(Name, S);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lAssignFile(var F: lFile; const Name: String);
begin
  case lAPI of
    lDOS:
      begin
        F.AssignFileMode := lDOS;
        Assign(F.F, Name);
      end;
    lWIN95:
     if LFNDisabled(Name) then
      begin
        F.AssignFileMode := lDOS;
        Assign(F.F, Name);
      end
     else
      begin
        F.AssignFileMode := lWIN95;
        FileRec(F.F).Handle := 0;
        FileRec(F.F).Mode := fmClosed;
        NameToNameZ(Name, F.FullName);
      end;
    {$IFDEF DPMI}
    lSIM95:
     if LFNDisabled(Name) then
      begin
        F.AssignFileMode := lDOS;
        Assign(F.F, Name);
      end
     else
      begin
        F.AssignFileMode := lSIM95;
        FileRec(F.F).Handle := 0;
        FileRec(F.F).Mode := fmClosed;
        NameToNameZ(Name, F.FullName);
      end;
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

{$L W95TFIO.OBJ}
{$L W95TCTL.OBJ}
procedure FileRead;   {$IFDEF BIT_16}far;{$ENDIF} external;
procedure FileWrDev;  {$IFDEF BIT_16}far;{$ENDIF} external;
procedure FileWrite;  {$IFDEF BIT_16}far;{$ENDIF} external;
procedure FileClose;  {$IFDEF BIT_16}far;{$ENDIF} external;
procedure FileOpen;   {$IFDEF BIT_16}far;{$ENDIF} external;
{$IFDEF DPMI}
{$L S95TFIO.OBP}
procedure SimFileOpen;{$IFDEF BIT_16}far;{$ENDIF} external;
{$ENDIF}

procedure lAssignText(var T: lText; const Name: String);
begin
  case lAPI of
    lDOS:
      begin
        T.AssignTextMode := lDOS;
        Assign(T.T, Name);
      end;
    lWIN95:
     if LFNDisabled(Name) then
      begin
        T.AssignTextMode := lDOS;
        Assign(T.T, Name);
      end
     else
      begin
        T.AssignTextMode := lWIN95;
        Assign(T.T, '');
        TextRec(T.T).Private := 1;
        TextRec(T.T).OpenFunc := @FileOpen;
        NameToNameZ(Name, T.FullName);
      end;
    {$IFDEF DPMI}
    lSIM95:
     if LFNDisabled(Name) then
      begin
        T.AssignTextMode := lDOS;
        Assign(T.T, Name);
      end
     else
      begin
        T.AssignTextMode := lSIM95;
        Assign(T.T, '');
        TextRec(T.T).Private := 1;
        TextRec(T.T).OpenFunc := @SimFileOpen;
        NameToNameZ(Name, T.FullName);
      end;
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lResetFile(var F: lFile; RecSize: Word);
begin
  case F.AssignFileMode of
    lDOS: Reset(F.F, RecSize);
    lWIN95: lWIN95OpenFile(F, RecSize, faOpen);
    {$IFDEF DPMI}
    lSIM95: lSIM95OpenFile(F, RecSize, faOpen);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lResetText(var F: lText);
begin
  case F.AssignTextMode of
    lDOS: Reset(F.T);
    lWIN95: Reset(F.T);
    {$IFDEF DPMI}
    lSIM95: Reset(F.T);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lAppendText(var T: lText);
begin
  case T.AssignTextMode of
    lDOS: Append(T.T);
    lWIN95: Append(T.T);
    {$IFDEF DPMI}
    lSIM95: Append(T.T);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lResetFileReadOnly(var F: lFile; RecSize: Word);
var
  SaveMode: Byte;
begin
  SaveMode := FileMode;
  FileMode := 64;
  lResetFile(F, RecSize);
  FileMode := SaveMode;
end;

procedure lResetTextReadOnly(var F: lText);
var
  SaveMode: Byte;
begin
  SaveMode := FileMode;
  FileMode := 64;
  lResetText(F);
  FileMode := SaveMode;
end;

procedure lRewriteFile(var F: lFile; RecSize: Word);
var OldMode: byte;
begin
  OldMode:= FileMode;
  FileMode:=FileMode and $FC or 2;
  case F.AssignFileMode of
    lDOS: Rewrite(F.F, RecSize);
    lWIN95: lWIN95OpenFile(F, RecSize, faRewrite);
    {$IFDEF DPMI}
    lSIM95: lSIM95OpenFile(F, RecSize, faRewrite);
    {$ENDIF}
    { Other APIs will be added later }
  end;
  FileMode:=OldMode;
end;

procedure lRewriteText(var F: lText);
var OldMode: byte;
begin
  OldMode:= FileMode;
  FileMode:=FileMode and $FC or 2;
  case F.AssignTextMode of
    lDOS: Rewrite(F.T);
    lWIN95: Rewrite(F.T);
    {$IFDEF DPMI}
    lSIM95: Rewrite(F.T);
    {$ENDIF}
    { Other APIs will be added later }
  end;
  FileMode:=OldMode;
end;

procedure lEraseFile(var F: lFile);
begin
  case F.AssignFileMode of
    lDOS: Erase(F.F);
    lWIN95: lWIN95EraseFile(F);
    {$IFDEF DPMI}
    lSIM95: lSIM95EraseFile(F);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lEraseText(var T: lText);
begin
  case T.AssignTextMode of
    lDOS: Erase(T.T);
    lWIN95: lWIN95EraseText(T);
    {$IFDEF DPMI}
    lSIM95: lSIM95EraseText(T);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lRenameFile(var F: lFile; const NewName: String);
begin
  case F.AssignFileMode of
    lDOS: Rename(F.F, NewName);
    lWIN95: lWIN95RenameFile(F, NewName);
    {$IFDEF DPMI}
    lSIM95: lSIM95RenameFile(F, NewName);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lRenameText(var T: lText; const NewName: String);
begin
  case T.AssignTextMode of
    lDOS: Rename(T.T, NewName);
    lWIN95: lWIN95RenameText(T, NewName);
    {$IFDEF DPMI}
    lSIM95: lSIM95RenameText(T, NewName);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lGetFAttr(var F: lFile; var Attr: Word);
begin
  case F.AssignFileMode of
    lDOS: GetFAttr(F.F, Attr);
    lWIN95: lWIN95FileAttrFunc(F, Attr, faGetAttr);
    {$IFDEF DPMI}
    lSIM95: lSIM95FileAttrFunc(F, Attr, faGetAttr);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lSetFAttr(var F: lFile; Attr: Word);
begin
  case F.AssignFileMode of
    lDOS: SetFAttr(F.F, Attr);
    lWIN95: lWIN95FileAttrFunc(F, Attr, faSetAttr);
    {$IFDEF DPMI}
    lSIM95: lSIM95FileAttrFunc(F, Attr, faSetAttr);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lGetTAttr(var T: lText; var Attr: Word);
begin
  case T.AssignTextMode of
    lDOS: GetFAttr(T.T, Attr);
    lWIN95: lWIN95TextAttrFunc(T, Attr, faGetAttr);
    {$IFDEF DPMI}
    lSIM95: lSIM95TextAttrFunc(T, Attr, faGetAttr);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lSetTAttr(var T: lText; Attr: Word);
begin
  case T.AssignTextMode of
    lDOS: SetFAttr(T.T, Attr);
    lWIN95: lWIN95TextAttrFunc(T, Attr, faSetAttr);
    {$IFDEF DPMI}
    lSIM95: lSIM95TextAttrFunc(T, Attr, faSetAttr);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lMkDir(const Path: String);
begin
  case lAPI of
    lDOS: MkDir(Path);
    lWIN95:
      if LFNDisabled(Path) then MkDir(Path)
      else lWIN95DirFunc(Path, $7139);
    {$IFDEF DPMI}
    lSIM95:
      if LFNDisabled(Path) then MkDir(Path)
      else lSIM95DirFunc(Path, $7139);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lRmDir(const Path: String);
begin
  case lAPI of
    lDOS: RmDir(Path);
    lWIN95:
      if LFNDisabled(Path) then RmDir(Path)
      else lWIN95DirFunc(Path, $713A);
    {$IFDEF DPMI}
    lSIM95:
      if LFNDisabled(Path) then RmDir(Path)
      else lSIM95DirFunc(Path, $713A);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lChDir(const Path: String);
begin
  case lAPI of
    lDOS: ChDir(Path);
    lWIN95:
      if LFNDisabled(Path) then ChDir(Path)
      else lWIN95ChDir(Path);
    {$IFDEF DPMI}
    lSIM95:
      if LFNDisabled(Path) then ChDir(Path)
      else lSIM95ChDir(Path);
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lGetDir(D: Byte; var Path: String);
var
  Regs: Registers;
begin
  case lAPI of
    lDOS: GetDir(D, Path);
    lWIN95:
      begin
        if D = 0 then with Regs do
        begin
          AH := $19;
          Intr($21, Regs);
          D := AL + 1;
        end;
        if LFNDisabled(Char(D + $40) + ':') then
        begin
          GetDir(D, Path);
          Exit;
        end;
        lWIN95GetDir(D, Path);
      end;
    {$IFDEF DPMI}
    lSIM95:
      begin
        if D = 0 then with RMR do
        begin
          AH := $19;
          SimulateRealModeInt($21, RMR);
          D := AL + 1;
        end;
        if LFNDisabled(Char(D + $40) + ':') then
        begin
          GetDir(D, Path);
          Exit;
        end;
        lSIM95GetDir(D, Path);
      end;
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

{ FExpand expands the file name in Path into a fully qualified  }
{ file name. The resulting name consists of a drive letter, a   }
{ colon, a root relative directory path, and a file name.       }
{ Embedded '.' and '..' directory references are removed.       }

{$L W95EXPN.OBJ}           { File name expansion routine }
function LFNFExpand(Path: String): String; external {EXPN};

function lFExpand(const Path: String): String;
{$IFDEF DPMI}var S: String;{$ENDIF}
begin
  case lAPI of
    lDOS: lFExpand := FExpand(Path);
    lWIN95:
      if LFNDisabled(Path) then lFExpand := FExpand(Path)
      else if Copy(Path, 1, 2)='\\' then lFExpand :=Path
                                    else lFExpand := LFNFExpand(Path);
    {$IFDEF DPMI}
    lSIM95:
      if LFNDisabled(Path) then lFExpand := FExpand(Path)
      else if Copy(Path, 1, 2)='\\' then lFExpand :=Path
                                    else begin
       SIM95TrueName(Path, S);
       if S = '' then lGetDir(0,S); { Kirill }
       lFExpand := S;
      end;
    {$ENDIF}
    { Other APIs will be added later }
  end;
end;

procedure lFSplit(const Path: String; var Dir, Name, Ext: String);
var
  HasColon: Boolean;
  DotPos, SlashPos, L, B: Byte;
  D:String;
  N:String;
  E:String;
begin
  Dir := '';
  Name := '';
  Ext := '';
  DotPos := 255;
  SlashPos := 0;
  HasColon := (Length(Path) > 1) and
              ((Path[2] = ':') or ((Word((@Path[1])^)=$2F2F{//}) or (
               Word((@Path[1])^)=$5C5C{\\})));
  if ((Word((@Path[1])^)=$2F2F{//}) or (Word((@Path[1])^)=$5C5C{\\})) then begin
    B:=4; L:=0;
    while (L<Length(Path)) and (B>0) do begin
      inc(l);
      if Path[l] in ['\', '/'] then dec(b);
    end;
  end else L:=1;

  for B := Length(Path) downto L do
  begin
    if (Path[B] = '.') and (DotPos = 255) and (B>1) then DotPos := B;
    if ((Path[B] = ':') or (Path[B] in ['\','/'])) and (SlashPos = 0) then SlashPos := B;
    if SlashPos <> 0 then Break;
  end;

  if (DotPos=255) and (SlashPos = 0) then
    if HasColon then Dir := Path
    else Name := Path
  else
  begin
    if DotPos > SlashPos then Ext := Copy(Path, DotPos, 255)
    else DotPos := 255;

    if SlashPos <> 0 then Dir := Copy(Path, 1, SlashPos);

    Name := Copy(Path, SlashPos + 1, DotPos - SlashPos - 1);
  end;
end;

function CheckLFNApi(Api: lAPIType): Boolean;
 var SaveApi: lAPIType;
     lsr: lSearchRec;
 begin
  SaveApi:=lAPI;
  lAPI:=Api;
  lFindFirst(ParamStr(0), AnyFile, lsr);
  lFindClose(lsr);
  CheckLFNApi := (DosError=0000);
  lAPI:=SaveApi;
 end;

function lFileNameOf(var lF: lFile): string;
var
  S: string;
  k: byte;
begin
  Case lF.AssignFileMode of
   lDOS: Begin
          S := FileRec(lF.F).Name;
          S[0] := Char(Pos(#0, S)-1);
         End;
   lWIN95{$IFDEF DPMI}, lSIM95{$ENDIF}
         : Begin
            k:=0;
            s:='';
            repeat
             s:=s+lF.FullName[k];
             inc (k);
            until (lF.FullName[k]=#0) or (k>255);
           end;
  end;
  lFileNameOf := S;
end;

function lTextNameOf(var lT: lText): string;
var
  S: string;
  k: byte;
begin
  Case lT.AssignTextMode of
   lDOS: Begin
          S := TextRec(lT.T).Name;
          S[0] := Char(Pos(#0, S)-1);
         End;
   lWIN95{$IFDEF DPMI}, lSIM95{$ENDIF}
         : Begin
            k:=0;
            s:='';
            repeat
             s:=s+lT.FullName[k];
             inc (k);
            until (lT.FullName[k]=#0) or (k>255);
           end;
  end;
  lTextNameOf := S;
end;

{$IFDEF DPMI}
var OldEx: Pointer;
procedure ExitLFN;
begin
 ExitProc := OldEx;
 FreeDosMem(LowBuf);
 LowBuf := nil;
 LowSeg := 0;
end;
{$ENDIF}

const Inited: boolean = false;
Procedure LfnInitUnit;
{!!!!!!!!}
{.$DEFINE LFNAPIDEBUG}
{!!!!!!!!}
{$IFDEF LFNAPIDEBUG}
var L: Text;
{$ENDIF}
begin
 if Inited then exit;
 Inited := true;
 CheckingApi:=True;
 if CanUseLFN then begin
   if CheckLFNApi(lWIN95) then begin
                                lAPI:=lWIN95;
                                MaxPathLen:=255;
                                LFNPresent:=true;
                                IllegalChars:='<>|:';
                                IllegalCharSet:=['<','>','|',':'];
                               end
   {$IFDEF DPMI}
   else begin
    LowBuf:=GetDosMem(65520, LowSeg);
    if CheckLFNApi(lSIM95) then begin
                                 lAPI:=lSIM95;
                                 MaxPathLen:=255;
                                 LFNPresent:=true;
                                 IllegalChars:='<>|:';
                                 IllegalCharSet:=['<','>','|',':'];
                                 OldEx := ExitProc;
                                 ExitProc := @ExitLFN;
                                end
    else begin
     FreeDosMem(LowBuf);
     LowBuf := nil;
    end;
   end;
   {$ENDIF}
  end;
  {check for other API's will added later}
 {$IFDEF LFNAPIDEBUG}
 Assign(L, 'LFNDEBUG.LOG');
 Append(L); if IOResult<>0 then Rewrite(L);
 case lAPI of
    lDOS: Writeln(L, 'lDOS');
  lWIN95: Writeln(L, 'lWIN95');
  {$IFDEF DPMI}
  lSIM95: Writeln(L, 'lSIM95');
  {$ENDIF}
  else    Writeln(L, '!ELSE!');
 end;
 Close(L);
 {$ENDIF}
 CheckingApi:=False;
end;

end.
