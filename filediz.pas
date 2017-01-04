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
//  dn16rc1-description_for_longname_workaround-diff160byMV.patch
//
//  2.0.0
//  dn200-new_description_creation_fix-diff162byMV.patch
//
//  2.3.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//
//  4.9.0
//  dn50208-cleanup.patch
//
//  5.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit FileDiz;
interface
uses filescol, objects;

function  GetPossibleDizOwner(N: Integer): String;
function  GetDIZOwner(const Path, LastOwner: String; Add: Boolean): String;
function  CalcDPath(P: PDIZ; Owen: PString): String;
procedure ReplaceDIZ(const DPath, Name1, Name2: String;
                     ANewName: PString; ANewDescription: PString);
                {When (ANewName= nil) and (ANewDesc= nil) - erasing a string}
                {When (ANewName= nil) and (ANewDesc<>nil) - change a description}
                {When (ANewName<>nil) and (ANewDesc= nil) - change a name}
                {When (ANewName<>nil) and (ANewDesc<>nil) - change a name & desc}
procedure DeleteDIZ(const DPath, Name1, Name2: String); {DataCompBoy}
function  GetDIZ(const DPath: string; var Line: longint; const Name, Name2: String): string; {DataCompBoy}
procedure SetDescription(PF: PFileRec; DIZOwner: String);

implementation
uses startup, advance1, advance2, advance, dos, lfn, lfncol,
     messages, dnapp, commands, drives, advance3;

function GetPossibleDizOwner(N: Integer): String;
  var
      I: Integer;
      DIZ: String;
begin
  GetPossibleDizOwner := '';
  DIZ := FMSetup.DIZ;
  while (N > 0) and (Diz <> '') do begin
   I := PosChar(';', DIZ); if I = 0 then I := Length(DIZ)+1;
   if CorrectFile(Copy(DIZ,1,I-1)) then begin
    Dec(N);
    if N = 0 then begin
     GetPossibleDizOwner := Copy(DIZ,1,I-1);
     Exit;
    end;
   end;
   Delete(DIZ, 1, I);
  end;
end;

 {-DataCompBoy: Descriptions remain with short names!!!-}
        {-DataCompBoy-}
function GetDIZOwner;
  var
      I: Integer;
      lAdd: Boolean;
      SR: lSearchRec;

  procedure Prepare;
    var
        J: Word;
        C: Char;
        F: lFile;
  begin
     if lAdd then Exit;
     lAssignFile(F, MakeNormName(Path, SR.FullName)); ClrIO;
     FileMode := $42;
     lResetFile(F, 1);
     if IOResult <> 0 then Exit;
     System.Seek(F.F, FileSize(F.F)-1);
     BlockRead(F.F, C, 1, J);
     if (IOResult = 0) and (C <> #13) and (C <> #10) then
       begin
         C := #13; BlockWrite(F.F, C, 1, J);
         C := #10; BlockWrite(F.F, C, 1, J);
       end;
     ClrIO; Close(F.F); ClrIO;
  end;

begin
  lAdd := Add; SR.SR.Name := LastOwner; SR.FullName := LastOwner; Add := False;
  I := 1;
  if LastOwner = '' then GetDIZOwner := '' else
    begin
      GetDIZOwner := MakeNormName(Path, LastOwner);
      lFindFirst(MakeNormName(Path, LastOwner), Archive+ReadOnly+Hidden+SysFile, SR);
      I:=DosError;
      lFindClose(SR);
      if I = 0 then begin Prepare; Exit; end;
      I := 1;
    end;
  repeat
    SR.FullName := GetPossibleDizOwner(I); Inc(I);
    if SR.FullName = '' then Exit;
    lFindFirst(MakeNormName(Path, SR.FullName), Archive+ReadOnly+Hidden+SysFile, SR);
    if DOSError = 0 then begin Prepare; GetDIZOwner := MakeNormName(Path, SR.FullName); lFindClose(SR); Exit end;
    lFindClose(SR);
  until False;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure ReplaceT(P: PTextReader; var F: lText; Del: boolean; Attr: Word);
var
  I: Integer;
  FName: String;
begin
  FName := P^.FileName;
  Dispose(P,Done); ClrIO;
  Close(F.T); ClrIO; lSetTAttr(F, Attr); ClrIO;
  EraseByName(FName);
  I := IOResult; ClrIO;
  if not Del then begin
   lRenameText(F, FName);
   if (IOResult <> 0) then
   begin
     if (I<>0) then Erase(F.T);
     CantWrite(FName);
   end;
  end else lEraseText(F);
  ClrIO;
end;
        {-DataCompBoy-}

        {-DataCompBoy-}
procedure SetDescription(PF: PFileRec; DIZOwner: String);
var
  DIZ: String;
begin
  if (PF = nil) then Exit;
  if DIZOwner = '' then begin
   Diz:=GetPossibleDizOwner(1);
   if DIZ = '' then begin
    MessageBox(GetString(dlNoPossibleName), nil, mfError);
    Exit;
   end;
   DIZOwner:=DIZ;
  end else DIZOwner:=GetName(DizOwner);
  DIZOwner := GetDizOwner(PF^.Owner^, DIZOwner, False);
  if (PF^.DIZ = nil) or (PF^.DIZ^.DIZ = nil) then DIZ := ''
                                             else DIZ := PF^.DIZ^.DIZ^;
  if BigInputBox(GetString(dlEditDesc),GetString(dl_D_escription), DIZ, 255, hsEditDesc) <> cmOK then Exit;
  DelLeft(DIZ);
  ReplaceDIZ(DIZOwner, MakeFileName(PF^.Name), GetLFN(PF^.LFN), nil, @DIZ);
  (* if IOResult <> 0 then CantWrite(DIZOwner); *)
  RereadDirectory(PF^.Owner^);
end;
        {-DataCompBoy-}


        {-DataCompBoy-}
procedure ReplaceDIZ(const DPath, Name1, Name2: String;
            ANewName: PString; ANewDescription: PString);
    {When (ANewName= nil) and (ANewDesc= nil) - erasing a string}
    {When (ANewName= nil) and (ANewDesc<>nil) - change a description}
    {When (ANewName<>nil) and (ANewDesc= nil) - change a name}
    {When (ANewName<>nil) and (ANewDesc<>nil) - change a name & desc}
var
  OrigAttr: Word;
  I, j: Integer;
  zz: boolean;
  WasFilesInDIZ: Boolean;
  FNd: Boolean;
  F1: PTextReader;
  F2: lText;
  S, NewName: string;
begin
  NewName:=Name1;
  if ANewName<>nil then begin
    NewName:=ANewName^;
  end;
{$IFNDEF NONBP}DisableAppend;{$ENDIF}
  OrigAttr:=GetFileAttr(DPath);
  F1 := New(PTextReader, Init(DPath));
  if F1 = nil then begin
    if (ANewDescription<>nil) and
       (ANewDescription^<>'') then begin
     ClrIO; lAssignText(F2, DPath); lRewriteText(F2);
     if Abort then begin Close(F2.T); Exit; end;
     if IOResult <> 0 then (* begin CantWrite(DPath); *) Exit; (* end; *)
     if Length(NewName)<12 then NewName:=AddSpace(NewName,12);
     WriteLn(F2.T, NewName+' '+ANewDescription^);
    end;
    Close(F2.T);
    Exit;
  end;
  lAssignText(F2, GetPath(DPath) + '$DN'+ItoS(DNNumber)+'$.DIZ');
  {$IFNDEF NONBP}DisableAppend;{$ENDIF}
  lRewriteText(F2);
  if IOResult <> 0 then begin Dispose(F1,Done); Exit; end;
  Fnd := False; WasFilesInDIZ := False;
  repeat
    S := F1^.GetStr;
    if s='' then continue;
    if not (s[1] in [' ',#9]) then begin
     zz:=false;
     for i:=1 to length(s) do
      if (s[i] in [' ',#9]) and not zz
       then break
       else if s[i]='"' then zz:=not zz;
     if s[i] in [' ',#9] then dec(i);
     j:=i+1; while (s[j] in [' ',#9]) do inc(j);

     zz:=false;
     if UpStrg(Copy(S, 1, i))=UpStrg(Name1) then zz:=true else
     if UpStrg(Copy(S, 1, i))=UpStrg(Name2) then
     begin if ANewName=nil then NewName:=Name2; zz:=true end;

     if zz
      then begin
       Fnd := True;

       if ((ANewDescription=nil) or (ANewDescription^='')) and
          (ANewName=nil) and
          (FMSetup.Options and fmoPreserveDesc = 0)
        then begin {delete multiline description}{piwamoto}
         repeat
          S := F1^.GetStr;
         until (S[1]<>' ') or F1^.EOF or (IOResult <> 0);
         if not F1^.EOF then begin WasFilesInDIZ:=True; Writeln(F2.T, S); end;
         continue;
        end;

       Delete(S, 1, j-1);

       if Length(NewName)<12 then NewName:=AddSpace(NewName,12);
       if ANewDescription<>nil
        then S:=NewName + ' ' + ANewDescription^
        else S:=NewName + ' ' + S;
     end;
    end;
    WasFilesInDIZ:=True;
    Writeln(F2.T, S);
  until F1^.EOF or (IOResult <> 0);
  if not Fnd then begin
   if ANewDescription<>nil
    then WriteLn(F2.T, AddSpace(NewName,13) + ANewDescription^);
   WasFilesInDIZ:=True;
  end;
  ReplaceT(F1, F2, (not WasFilesInDIZ) and (FMSetup.Options and fmoKillContainer <> 0), OrigAttr);
end;
        {-DataCompBoy-}

procedure DeleteDIZ(const DPath, Name1, Name2: String);
begin
  ReplaceDIZ(DPath, Name1, Name2, nil, nil);
end;

function  GetDIZ(const DPath: string; var Line: longint; const Name, Name2: String): string; {DataCompBoy}
var
  i,j: integer;
  zz: boolean;
  F1: PTextReader;
  S: string;
begin
 GetDIZ:='';
 Line:=0;
 {$IFNDEF NONBP}DisableAppend;{$ENDIF}
 F1 := New(PTextReader, Init(DPath));
 if F1 = nil then exit;
 repeat
  S := F1^.GetStr;
  Inc(Line);
  if s='' then continue;
  if not (s[1] in [' ',#9]) then begin
   zz:=false;
   for i:=1 to length(s) do
    if (s[i] in [' ',#9]) and not zz
     then break
      else if s[i]='"' then zz:=not zz;
    if s[i] in [' ',#9] then dec(i);
    j:=i+1; while (s[j] in [' ',#9]) do inc(j);

    if (UpStrg(Copy(S, 1, i))=UpStrg(Name)) or (UpStrg(Copy(S, 1, i))=UpStrg(Name2))
     then begin
      Delete(S, 1, j-1);
      While S[1] in [' ', #9] do DelFC(S);
      GetDIZ:=S;
      Break;
     End;
    end;
 until F1^.EOF or (IOResult <> 0);
 Dispose(F1,Done);
end;
        {-DataCompBoy-}
function CalcDPath(P: PDIZ; Owen: PString): String;
var
  I: Integer;
  SR: lSearchRec;
  DPath: String;
begin
  if (P = nil) or (P^.Owner = nil) then
  begin
    for I := 1 to 128 do
    begin
      DPath := GetPossibleDizOwner(I);
      if DPath = '' then Exit;
      DPath := MakeNormName(CnvString(Owen), DPath);
      lFindFirst(DPath, Archive+ReadOnly+Hidden, SR);
      if DOSError = 0 then Break;
      lFindClose(SR);
    end;
    lFindClose(SR);
  end else DPath := P^.Owner^;
  CalcDPath := DPath;
end;
        {-DataCompBoy-}

end.
