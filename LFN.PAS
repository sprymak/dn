{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.07/DOS
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
unit LFN;

{This unit based on LFN.PAS unit from "Connect"}

interface

uses Dos, DnINI;

type
  lAPIType = (lDOS, lWIN95);  {API currently used}

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
procedure lGetFileNameForChangedAPI(FromAPI: lAPIType;
  const Name: String; var NewName: String);

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

implementation

uses advance1,
     Startup,
     Commands
     {$IFDEF DPMI}
     ,Dpmi, DosMem
     {$ENDIF}
     ;

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
  if (Length(Path) > 1) and (Path[1] = '\') and (Path[2] = '\')
   then LFNDisabled := False
   else begin
         if (Length(Path) > 1) and (Path[2] = ':')
          then Drive := Upcase(Path[1])
          else with Regs do begin
                AH := $19;
                Intr($21, Regs);
                Drive := Char(AL + $41);
               end;
         if SystemData.Drives[Drive] and ossNoLFN <> 0 then exit;
        end;
  case lAPI of
   lDOS: LFNDisabled:=false;
   lWIN95: begin
            DriveBuffer[1]:=Drive;
            with regs do begin
             ax:=$71a0;
             ds:=seg(DriveBuffer);
             dx:=ofs(DriveBuffer);
             es:=0;
             di:=0;
             cx:=0;
             intr($21, regs);
             if Regs.flags and fCarry=0 then
              LFNDisabled:=((Regs.Bx and $4000)=0);
            end;
           end;
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
  R.FullSize:=MaxLongInt;
  R.FullSize:=R.FullSize*2;
  R.FullSize:=R.FullSize*FindData.HiSize + FindData.LoSize;
end;

(*
 INT 21h  AX=714E
 INT 21 - Windows95 - LONG FILENAME - FIND FIRST MATCHING FILE
         AX = 714Eh
         CL = allowable-attributes mask (bits 0 and 5 ignored)
         CH = required-attributes mask
         SI = date/time format
         DS:DX -> ASCIZ filespec (both "*" and "*.*" match any filename)
         ES:DI -> FindData record
 Return: CF clear if successful
             AX = filefind handle (needed to continue search)
             CX = Unicode conversion flags
         CF set on error
             AX = error code
                 7100h if function not supported
 Notes:  this function is only available when IFSMgr is running,
         not under bare MS-DOS 7
         the application should close the filefind handle
         with AX=71A1h as soon as it has completed its search
*)

procedure lWIN95FindFirst(const Path: String; Attr: Word; var R: lSearchRec);
var
  Regs: Registers;
  FindData: lFindDataRec;
begin
  NameToNameZ(Path, FindData.FullName);

  with Regs do
  begin
    AX := $714E;
    CX := Attr;
    SI := 1;
    DS := Seg(FindData.FullName);
    DX := Ofs(FindData.FullName);
    ES := Seg(FindData);
    DI := Ofs(FindData);
    asm
     stc
    end;
    Intr($21, Regs);
    if Flags and fCarry <> 0 then
    begin
      R.FileHandle := $FFFF;
      DosError := AX;
    end
    else
    begin
      R.FileHandle := AX;
      FindDataToSearchRec(FindData, R);
      DosError := 0;
    end;
  end;
end;

(*
 INT 21h  AX=714F
 INT 21 - Windows95 - LONG FILENAME - FIND NEXT MATCHING FILE
         AX = 714Fh
         BX = filefind handle (from AX=714Eh)
         SI = date/time format
         ES:DI -> buffer for FindData record
 Return: CF clear if successful
             CX = Unicode conversion flags
         CF set on error
             AX = error code
                 7100h if function not supported
 Note:   this function is only available when IFSMgr is running,
         not under bare MS-DOS 7
*)

procedure lWIN95FindNext(var R: lSearchRec);
var
  Regs: Registers;
  FindData: lFindDataRec;
begin
  with Regs do
  begin
    AX := $714F;
    BX := R.FileHandle;
    SI := 1;
    ES := Seg(FindData);
    DI := Ofs(FindData);
    asm
     stc
    end;
    Intr($21, Regs);
    if Flags and fCarry <> 0 then DosError := AX
    else
    begin
      FindDataToSearchRec(FindData, R);
      DosError := 0;
    end;
  end;
end;

(*
 INT 21h  AX=71A1
 INT 21 - Windows95 - LONG FILENAME - "FindClose" -
          TERMINATE DIRECTORY SEARCH
         AX = 71A1h
         BX = filefind handle (from AX=714Eh)
 Return: CF clear if successful
         CF set on error
            AX = error code
                 7100h if function not supported
 Notes:  this function must be called after starting a search
         with AX=714Eh, to indicate that the search handle
         returned by that function will no longer be used
         this function is only available when IFSMgr is running,
         not under bare MS-DOS 7
*)

procedure lWIN95FindClose(var R: lSearchRec);
var
  Regs: Registers;
begin
  if R.FileHandle <> $FFFF then with Regs do
  begin
    AX := $71A1;
    BX := R.FileHandle;
    Intr($21, Regs);
  end;
end;

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
    { Other APIs will be added later }
  end;
end;

procedure lFindClose(var R: lSearchRec);
begin
  case R.FindFirstMode of
    lDOS: {In DOS no FindClose function...};
    lWIN95: lWIN95FindClose(R);
    { Other APIs will be added later }
  end;
end;

(*
 INT 21h  AX=7160 CL=01
 INT 21 - Windows95 - LONG FILENAME - GET SHORT (8.3) FILENAME FOR FILE
         AX = 7160h
         CL = 01h
         CH = SUBST expansion flag
             00h return a path containing true path
                 for a SUBSTed drive letter
             80h return a path containing the SUBSTed drive letter
         DS:SI -> ASCIZ long filename or path
         ES:DI -> 67-byte buffer for short filename
 Return: CF set on error
             AX = error code
                 02h invalid component in directory path
                     or drive letter only
                 03h malformed path or invalid drive letter
             ES:DI buffer unchanged
         CF clear if successful
             ES:DI buffer filled with equivalent short filename
                   (full path, even if relative path given,
                    and all uppercase)
 Note:   this call returns the short name for any
         long-filename portions of the provided pathname or filename
*)

(*
 INT 21h  AX=7160 CL=02
 INT 21 - Windows95 - LONG FILENAME - GET CANONICAL LONG FILENAME OR PATH
         AX = 7160h
         CL = 02h
         CH = SUBST expansion flag
             00h return a path containing true path
                 for a SUBSTed drive letter
             80h return a path containing the SUBSTed drive letter
         DS:SI -> ASCIZ short filename or path
         ES:DI -> 261-byte buffer for canonicalized long name
 Return: CF set on error
             AX = error code
                 02h invalid component in directory path
                     or drive letter only
                 03h malformed path or invalid drive letter
             ES:DI buffer unchanged
         CF clear if successful
             ES:DI buffer filled with qualified long name
                   (can contain lowercase letters)
*)

procedure lWIN95GetFileNameFunc(const Name: String;
  var S: String; AFunction: Byte);
var
  Regs: Registers;
  NameZ, GetNameZ: TNameZ;
begin
  NameToNameZ(Name, NameZ);

  with Regs do
  begin
    AX := $7160;
    CL := AFunction;
    CH := $80;
    DS := Seg(NameZ);
    SI := Ofs(NameZ);
    ES := Seg(GetNameZ);
    DI := Ofs(GetNameZ);
    Intr($21, Regs);
    if Flags and fCarry <> 0 then S := Name
    else
    begin
      S := StrPas(GetNameZ);
      CheckColonAndSlash(Name, S);
    end;
  end;
end;

procedure lGetShortFileName(const Name: String; var ShortName: String);
begin
  case lAPI of
    lDOS: ShortName := Name;
    lWIN95:
      if LFNDisabled(Name) then ShortName := Name
      else lWIN95GetFileNameFunc(Name, ShortName, 1);
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
    { Other APIs will be added later }
  end;
end;

function lfGetLongFileName(const Name: String): String;
 var s: String;
begin
  lGetLongFileName(Name, s);
 lfGetLongFileName:=s;
end;

procedure lGetFileNameForChangedAPI(FromAPI: lAPIType;
  const Name: String; var NewName: String);
var
  SaveAPI: lAPIType;
begin
  SaveAPI := lAPI;
  if lAPI = lDOS then lAPI := FromAPI;
  if FromAPI = lDOS then lGetLongFileName(Name, NewName)
  else lGetShortFileName(Name, NewName);
  lAPI := SaveAPI;
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
    AH := $60;
    DS := Seg(NameZ);
    SI := Ofs(NameZ);
    ES := Seg(GetNameZ);
    DI := Ofs(GetNameZ);
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
  Regs: DPMIRegisters;
  NameZ, GetNameZ: ^TNameZ;
begin
  FillChar(Regs, SizeOf(Regs), 0);
  NameZ    := GetDosMem(SizeOf(TNameZ), Regs.DS);
  GetNameZ := GetDosMem(SizeOf(TNameZ), Regs.ES);
  NameToNameZ(Name, NameZ^);

  with Regs do
  begin
    AH := $60;
    SimulateRealModeInt($21, Regs);
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

(*
 INT 21h  AX=7160 CL=00
 INT 21 - Windows95 - LONG FILENAME - "TRUENAME" - CANONICALIZE PATH
         AX = 7160h
         CL = 00h
         CH = SUBST expansion flag
             00h return a path containing true path
                 for a SUBSTed drive letter
             80h return a path containing the SUBSTed drive letter
         DS:SI -> ASCIZ filename or path (either long name or short name)
         ES:DI -> 261-byte buffer for canonicalized name
 Return: CF set on error
             AX = error code
                 02h invalid component in directory path
                     or drive letter only
                 03h malformed path or invalid drive letter
             ES:DI buffer unchanged
         CF clear if successful
             ES:DI buffer filled with fully qualified name
             AX destroyed
 Desc:   determine the canonical name of the specified
         filename or path, corresponding to the undocumented
         TRUENAME command in COMMAND.COM
 Note:   if a complete path is given, the result will be
         a short-form complete path; otherwise, the given
         relative path is appended to the short-form
         current directory name, '.'/'..'/'...'/etc. are resolved,
         and the final result uppercased without converting
         any remaining long-form names to short-form
*)

procedure WIN95TrueName(const Name: String; var S: String);
var
  Regs: Registers;
  NameZ, GetNameZ: TNameZ;
begin
  NameToNameZ(Name, NameZ);

  with Regs do
  begin
    AX := $7160;
    CL := 0;
    CH := $00;
    DS := Seg(NameZ);
    SI := Ofs(NameZ);
    ES := Seg(GetNameZ);
    DI := Ofs(GetNameZ);
    asm
     stc
    end;
    Intr($21, Regs);
    if Flags and fCarry <> 0 then S := Name
    else
    begin
      S := StrPas(GetNameZ);
      CheckColonAndSlash(Name, S);
    end;
  end;
end;

procedure lTrueName(const Name: String; var S: String);
begin
  case lAPI of
    lDOS: DOSTrueName(Name, S);
    lWIN95:
      if LFNDisabled(Name) then DOSTrueName(Name, S)
      else WIN95TrueName(Name, S);
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
    { Other APIs will be added later }
    else
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
  end;
end;

{$L W95TFIO.OBJ}
{$L W95TCTL.OBJ}
procedure FileRead; {$IFDEF BIT_16}far;{$ENDIF} external;
procedure FileWrDev; {$IFDEF BIT_16}far;{$ENDIF} external;
procedure FileWrite; {$IFDEF BIT_16}far;{$ENDIF} external;
procedure FileClose; {$IFDEF BIT_16}far;{$ENDIF} external;
procedure FileOpen; {$IFDEF BIT_16}far;{$ENDIF} external;

procedure lAssignText(var T: lText; const Name: String);
begin
  case lAPI of
    lDOS:
      begin
        T.AssignTextMode := lDOS;
        Assign(T.T, Name);
      end;
    { Other APIs will be added later }
    else
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
  end;
end;

(*
 INT 21h  AX=716C
 INT 21 - Windows95 - LONG FILENAME - CREATE OR OPEN FILE
         AX = 716Ch
         BX = access mode and sharing flags
         CX = attributes
         DX = action
         DS:SI -> ASCIZ filename
         DI = alias hint
              (number to append to short filename for disambiguation)
 Return: CF clear if successful
             AX = file handle
             CX = action taken
                 0001h file opened
                 0002h file created
                 0003h file replaced
         CF set on error
             AX = error code
                 7100h if function not supported
*)

const
  faOpen = 1;
  faTruncate = 2;
  faCreate = $10;
  faRewrite = faTruncate + faCreate;

procedure lWIN95OpenFile(var F: lFile; RecSize: Word; Action: Byte);
var
  Regs: Registers;
begin
  if FileRec(F.F).Mode <> fmClosed then Close(F.F);

  if F.FullName[0] = #0 then
  begin
    FileRec(F.F).Mode := fmInOut;
    FileRec(F.F).RecSize := RecSize;
  end
  else with Regs do
  begin
    AX := $716C;
    BX := FileMode;
    CX := 0;
    DX := Action;
    DS := Seg(F.FullName);
    SI := Ofs(F.FullName);
    DI := 0;
    Intr($21, Regs);
    if Flags and fCarry <> 0 then InOutRes := AX
    else
    begin
      FileRec(F.F).Mode:=fmInOut;
      FileRec(F.F).Handle := AX;
      FileRec(F.F).RecSize := RecSize;
      InOutRes := 0;
    end;
  end;
end;

procedure lResetFile(var F: lFile; RecSize: Word);
begin
  case F.AssignFileMode of
    lDOS: Reset(F.F, RecSize);
    lWIN95: lWIN95OpenFile(F, RecSize, faOpen);
    { Other APIs will be added later }
  end;
end;

procedure lResetText(var F: lText);
begin
  case F.AssignTextMode of
    lDOS: Reset(F.T);
    lWIN95: Reset(F.T);
    { Other APIs will be added later }
  end;
end;

procedure lAppendText(var T: lText);
var r: registers;
begin
  case T.AssignTextMode of
    lDOS: Append(T.T);
    lWIN95: Append(T.T);
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
    { Other APIs will be added later }
  end;
  FileMode:=OldMode;
end;

(*
 INT 21h  AX=7141
 INT 21 - Windows95 - LONG FILENAME - DELETE FILE
         AX = 7141h
         DS:DX -> ASCIZ long name of file to delete
         SI = wildcard and attributes flag
                 0000h wildcards are not allowed,
                   and search attributes are ignored
                 0001h wildcards are allowed,
                   and only files with matching
                   names and attributes are deleted
         CL = search attributes
         CH = must-match attributes
 Return: CF clear if successful
         CF set on error
             AX = error code
                 7100h if function not supported
*)

procedure lWIN95EraseFile(var F: lFile);
var
  Regs: Registers;
begin
  with Regs do
  begin
    AX := $7141;
    DS := Seg(F.FullName);
    DX := Ofs(F.FullName);
    SI := 0;
    Intr($21, Regs);
    if Flags and fCarry <> 0 then InOutRes := AX
                             else InOutRes := 0;
  end;
end;

procedure lEraseFile(var F: lFile);
begin
  case F.AssignFileMode of
    lDOS: Erase(F.F);
    lWIN95: lWIN95EraseFile(F);
    { Other APIs will be added later }
  end;
end;

procedure lWIN95EraseText(var T: lText);
var
  Regs: Registers;
begin
  with Regs do
  begin
    AX := $7141;
    DS := Seg(T.FullName);
    DX := Ofs(T.FullName);
    SI := 0;
    Intr($21, Regs);
    if Flags and fCarry <> 0 then InOutRes := AX
                             else InOutRes := 0;
  end;
end;

procedure lEraseText(var T: lText);
begin
  case T.AssignTextMode of
    lDOS: Erase(T.T);
    lWIN95: lWIN95EraseText(T);
    { Other APIs will be added later }
  end;
end;

(*
 INT 21h  AX=7156
 INT 21 - Windows95 - LONG FILENAME - RENAME FILE
         AX = 7156h
         DS:DX -> ASCIZ old file or directory name (long names allowed)
         ES:DI -> ASCIZ new name (long names allowed)
 Return: CF clear if successful
         CF set on error
             AX = error code
                 7100h if function not supported
 Note:   the file may be renamed into a different directory,
         but not across disks
*)

procedure lWIN95RenameFile(var F: lFile; const NewName: String);
var
  Regs: Registers;
  NameZ: TNameZ;
begin
  NameToNameZ(NewName, NameZ);

  with Regs do
  begin
    AX := $7156;
    DS := Seg(F.FullName);
    DX := Ofs(F.FullName);
    ES := Seg(NameZ);
    DI := Ofs(NameZ);
    Intr($21, Regs);
    if Flags and fCarry <> 0 then InOutRes := AX
                             else InOutRes := 0;
  end;
end;

procedure lRenameFile(var F: lFile; const NewName: String);
begin
  case F.AssignFileMode of
    lDOS: Rename(F.F, NewName);
    lWIN95: lWIN95RenameFile(F, NewName);
    { Other APIs will be added later }
  end;
end;

procedure lWIN95RenameText(var T: lText; const NewName: String);
var
  Regs: Registers;
  NameZ: TNameZ;
begin
  NameToNameZ(NewName, NameZ);

  with Regs do
  begin
    AX := $7156;
    DS := Seg(T.FullName);
    DX := Ofs(T.FullName);
    ES := Seg(NameZ);
    DI := Ofs(NameZ);
    Intr($21, Regs);
    if Flags and fCarry <> 0 then InOutRes := AX
                             else InOutRes := 0;
  end;
end;

procedure lRenameText(var T: lText; const NewName: String);
begin
  case T.AssignTextMode of
    lDOS: Rename(T.T, NewName);
    lWIN95: lWIN95RenameText(T, NewName);
    { Other APIs will be added later }
  end;
end;

(*
 INT 21h  AX=7143
 INT 21 - Windows95 - LONG FILENAME - EXTENDED GET/SET FILE ATTRIBUTES
         AX = 7143h
         DS:DX -> ASCIZ filename
         BL = action
             00h retrieve attributes
                 Return: CX = file attributes
             01h set attributes
                 CX = attributes
             02h get physical size of compressed file
                 Return: DX:AX = actual disk usage of file, in bytes
             03h set last write date/time
                 DI = new last-write date
                 CX = new last-write time
             04h get last write date/time
                 Return: CX = last write time
                         DI = last write date
             05h set last access date
                 DI = new last-access date
             06h get last access date
                 Return: DI = last access date
             07h set creation date/time
                 DI = new creation date
                 CX = new creation time
                 SI = hundredths
                   (10-millisecond units past time in CX, 0-199)
             08h get creation date/time
                 Return: CX = creation time
                         DI = creation date
                         SI = hundredths
                           (10-millisecond units past time in CX)
 Return: CF clear if successful
         CF set on error
             AX = error code
                 7100h if function not supported
*)

const
  faGetAttr = 0;
  faSetAttr = 1;

procedure lWIN95FileAttrFunc(var F: lFile; var Attr: Word; Action: Byte);
var
  Regs: Registers;
begin
  with Regs do
  begin
    AX := $7143;
    BL := Action;
    if Action = faSetAttr then CX := Attr;
    DS := Seg(F.FullName);
    DX := Ofs(F.FullName);
    Intr($21, Regs);
    if Flags and fCarry <> 0 then DosError := AX
    else
    begin
      if Action = faGetAttr then Attr := CX;
      DosError := 0;
    end;
  end;
end;

procedure lGetFAttr(var F: lFile; var Attr: Word);
begin
  case F.AssignFileMode of
    lDOS: GetFAttr(F.F, Attr);
    lWIN95: lWIN95FileAttrFunc(F, Attr, faGetAttr);
    { Other APIs will be added later }
  end;
end;

procedure lSetFAttr(var F: lFile; Attr: Word);
begin
  case F.AssignFileMode of
    lDOS: SetFAttr(F.F, Attr);
    lWIN95: lWIN95FileAttrFunc(F, Attr, faSetAttr);
    { Other APIs will be added later }
  end;
end;

procedure lWIN95TextAttrFunc(var T: lText; var Attr: Word; Action: Byte);
var
  Regs: Registers;
begin
  with Regs do
  begin
    AX := $7143;
    BL := Action;
    if Action = faSetAttr then CX := Attr;
    DS := Seg(T.FullName);
    DX := Ofs(T.FullName);
    Intr($21, Regs);
    if Flags and fCarry <> 0 then DosError := AX
    else
    begin
      if Action = faGetAttr then Attr := CX;
      DosError := 0;
    end;
  end;
end;

procedure lGetTAttr(var T: lText; var Attr: Word);
begin
  case T.AssignTextMode of
    lDOS: GetFAttr(T.T, Attr);
    lWIN95: lWIN95TextAttrFunc(T, Attr, faGetAttr);
    { Other APIs will be added later }
  end;
end;

procedure lSetTAttr(var T: lText; Attr: Word);
begin
  case T.AssignTextMode of
    lDOS: SetFAttr(T.T, Attr);
    lWIN95: lWIN95TextAttrFunc(T, Attr, faSetAttr);
    { Other APIs will be added later }
  end;
end;

(*
 INT 21h  AX=7139
 INT 21 - Windows95 - LONG FILENAME - MAKE DIRECTORY
         AX = 7139h
         DS:DX -> ASCIZ long directory name (including path)
 Return: CF clear if successful
         CF set on error
             AX = error code
                 7100h if function not supported
*)

(*
 INT 21h  AX=713A
 INT 21 - Windows95 - LONG FILENAME - REMOVE DIRECTORY
         AX = 713Ah
         DS:DX -> ASCIZ long name of directory to remove
 Return: CF clear if successful
         CF set on error
             AX = error code
                 7100h if function not supported
*)

procedure lWIN95DirFunc(const Path: String; AFunction: Word);
var
  Regs: Registers;
  NameZ: TNameZ;
begin
  NameToNameZ(Path, NameZ);

  with Regs do
  begin
    AX := AFunction;
    DS := Seg(NameZ);
    DX := Ofs(NameZ);
    asm
     stc
    end;
    Intr($21, Regs);
    if Flags and fCarry <> 0 then InOutRes := AX
                             else InOutRes := 0;
  end;
end;

procedure lMkDir(const Path: String);
begin
  case lAPI of
    lDOS: MkDir(Path);
    lWIN95:
      if LFNDisabled(Path) then MkDir(Path)
      else lWIN95DirFunc(Path, $7139);
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
    { Other APIs will be added later }
  end;
end;

(*
 INT 21h  AX=713B
 INT 21 - Windows95 - LONG FILENAME - CHANGE DIRECTORY
         AX = 713Bh
         DS:DX -> ASCIZ long name of directory to make current
 Return: CF clear if successful
         CF set on error
             AX = error code
                 7100h if function not supported
*)

procedure lWIN95ChDir(const Path: String);
var
  C: Char;
  Regs: Registers;
  NameZ: TNameZ;
begin
  NameToNameZ(Path, NameZ);

  with Regs do
  begin
    C := Upcase(NameZ[0]);
    if (C in ['A'..'Z']) and (NameZ[1] = ':') then
    begin
      AH := $0E;
      DL := Byte(C) - $41;
      Intr($21, Regs);
    end;

    AX := $713B;
    DS := Seg(NameZ);
    DX := Ofs(NameZ);
    asm
     stc
    end;
    Intr($21, Regs);
    if Flags and fCarry <> 0 then InOutRes := AX
                             else InOutRes := 0;
  end;
end;

procedure lChDir(const Path: String);
begin
  case lAPI of
    lDOS: ChDir(Path);
    lWIN95:
      if LFNDisabled(Path) then ChDir(Path)
      else lWIN95ChDir(Path);
    { Other APIs will be added later }
  end;
end;

(*
 INT 21h  AX=7147
 INT 21 - Windows95 - LONG FILENAME - GET CURRENT DIRECTORY
         AX = 7147h
         DL = drive number (00h = current, 01h = A:, etc.)
         DS:SI -> buffer for ASCIZ directory name
 Return: CF clear if successful
         CF set on error
             AX = error code
                 7100h if function not supported
 Notes:  the returned pathname does not include
         the drive letter, colon, or leading backslash
*)

procedure lWIN95GetDir(D: Byte; var Path: String);
var
  C: Char;
  Regs: Registers;
  NameZ: TNameZ;
begin
  with Regs do
  begin
    C := Char(D + $40);

    AX := $7147;
    DL := D;
    DS := Seg(NameZ);
    SI := Ofs(NameZ);
    asm
      stc
    end;
    Intr($21, Regs);
    if Flags and fCarry <> 0 then NameZ[0] := #0;

    Path := Concat(C, ':\', StrPas(NameZ));
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
begin
  case lAPI of
    lDOS: lFExpand := FExpand(Path);
    lWIN95:
      if LFNDisabled(Path) then lFExpand := FExpand(Path)
      else if Copy(Path, 1, 2)='\\' then lFExpand :=Path
                                    else lFExpand := LFNFExpand(Path);
    { Other APIs will be added later }
  end;
end;

procedure lFSplit(const Path: String; var Dir, Name, Ext: String);
var
  HasColon: Boolean;
  DotPos, SlashPos, B: Byte;
  D:String;
  N:String;
  E:String;
begin
{ case lAPI of
    lDOS: Begin
           FSplit(Path, D, N, E);
           Dir:=D;
           Name:=N;
           Ext:=E;
          End;
    lWIN95:}
      begin
        Dir := '';
        Name := '';
        Ext := '';
        DotPos := 0;
        SlashPos := 0;
        HasColon := (Length(Path) > 1) and (Path[2] = ':');

        for B := Length(Path) downto 1 do
        begin
          if (Path[B] = '.') and (DotPos = 0) and (B>1) then DotPos := B;
          if ((Path[B] = ':') or (Path[B] = '\')) and (SlashPos = 0) and
             ((B > 2) or not HasColon) then SlashPos := B;
          if (DotPos <> 0) and (SlashPos <> 0) then Break;
        end;

        if DotPos + SlashPos = 0 then
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
    { Other APIs will be added later }
{ end;}
end;

function CheckLFNApi(Api: lAPIType): Boolean;
 var SaveApi: lAPIType;
     lsr: lSearchRec;
 begin
  SaveApi:=lAPI;
  lAPI:=Api;
  lFindFirst(ParamStr(0), AnyFile, lsr);
  lFindClose(lsr);
  CheckLFNApi:=(DosError=0000);
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
   lWIN95: Begin
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
   lWIN95: Begin
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

begin
 CheckingApi:=True;
 if CanUseLFN then begin
   if CheckLFNApi(lWIN95) then begin
                                lAPI:=lWIN95;
                                MaxPathLen:=255;
                                LFNPresent:=true;
                                IllegalChars:='<>|:';
                                IllegalCharSet:=['<','>','|',':'];
                               end;
  end;
  {check for other API's will added later}
 CheckingApi:=False;
end.
