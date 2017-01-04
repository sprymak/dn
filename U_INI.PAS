{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.04/DOS
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
//     "Based on TinyWeb Server by RIT Research Labs."
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

Unit u_ini;
interface
uses u_crt, objects;

Type
  TBoolMode = (bmTrueFalse, bm10, bmYesNo);
  TConvert = (cNone, cUpper, cLower);
  TFloat = Double;

Const
  BoolStr: array[bmTrueFalse..bmYesNo, false..true] of String[5] =
   (('FALSE', 'TRUE'), ('0', '1'), ('NO', 'YES'));

  {Режим записи в файл данных типа Bool, читаются ВСЕ типы нормально}
  BoolMode: TBoolMode = bm10;
  FloatPrecision: Shortint = -1; {если меньше нуля, то в формате: 0.3454E+34}

  Convert: TConvert = cNone;
  WriteEmptySection: Boolean = False;

Type
  PIniItem = ^TIniItem;
  TIniItem = Object
    NextItem: PIniItem;
    Name: PString;
    constructor Init(AName, AData: PString);
    destructor  Done; Virtual;
    procedure   SetName(AName: PString);
    procedure   SetChar(C: Char); {SYR}
    procedure   SetString(S: String);
    procedure   SetBool(B: Boolean);
    procedure   SetInt(L: Longint);
    procedure   SetFloat(F: TFloat);
    function    GetStr: String;
    function    GetChar(Default: Char): Char; {SYR}
    function    GetBool(Default: Boolean): Boolean;
    function    GetInt(Default: Longint): Longint;
    function    GetFloat(Default: TFloat): TFloat;
    function    GetStrForWriting: String;
  private
    Data: PString;
  end;

  PIniSection = ^TIniSection;
  TIniSection = Object
    NextSection: PIniSection;
    Name: PString;
    constructor Init(AName: PString);
    destructor  Done; Virtual;
    procedure   SetName(AName: PString);
    function    RenameItem(OldName, NewName: String): Boolean;
    procedure   WriteChar(AName:String; C: Char); {SYR}
    procedure   WriteStr(AName, S: String);
    procedure   WriteBool(AName: String; B: Boolean);
    procedure   WriteInt(AName: String; L: Longint);
    procedure   WriteFloat(AName: String; F: TFloat);
    function    ReadChar(AName:String; Default: Char):Char; {SYR}
    function    ReadStr(AName: String; Default: String): String;
    function    ReadBool(AName: String; Default: Boolean): Boolean;
    function    ReadInt(AName: String; Default: Longint): Longint;
    function    ReadFloat(AName: String; Default: TFloat): TFloat;
    procedure   DeleteItem(AName: String);
  private
    FirstItem: PIniItem;
    procedure   FreeSection;
    function    GetItem(AName: String; Create: Boolean): PIniItem; {Create item if not found it in list}
    function    GetLastItem: PIniItem;
    procedure   GetItemPointer(AName: String; var PPrev, PSelf: PIniItem);
    procedure   WriteSelf(var F: Text);
  end;

  PIniFile = ^TIniFile;
  TIniFile = Object
    FileName: PString;
    constructor Init(AFileName: String);
    destructor  Done(SaveChanges: Boolean); Virtual;
    procedure   SetFileName(AFileName: PString);
    function    RenameItem(ASection, OldName, NewName: String): Boolean;
    function    RenameSection(OldName, NewName: String): Boolean;
    procedure   WriteChar(ASection, AName: String; C:Char); {SYR}
    procedure   WriteStr(ASection, AName, S: String);
    procedure   WriteBool(ASection, AName: String; B: Boolean);
    procedure   WriteInt(ASection, AName: String; L: Longint);
    procedure   WriteFloat(ASection, AName: String; F: TFloat);
    function    ReadChar(ASection, AName: String; Default: Char): Char; {SYR}
    function    ReadStr(ASection, AName: String; Default: String): String;
    function    ReadBool(ASection, AName: String; Default: Boolean): Boolean;
    function    ReadInt(ASection, AName: String; Default: Longint): Longint;
    function    ReadFloat(ASection, AName: String; Default: TFloat): TFloat;
    procedure   DeleteItem(ASection, AName: String);
    procedure   DeleteSection(ASection: String);
  private
    FirstSection: PIniSection;
    procedure   FreeContent;
    function    GetSection(ASection: String; Create: Boolean): PIniSection; {Create section if not found it in list}
    function    GetLastSection: PIniSection;
    procedure   GetSectionPointer(ASection: String; var PPrev, PSelf: PIniSection);
    procedure   SplitLine(S: String; var AName, AValue: String); {split string for name and it value}
    procedure   ShortLine(var S: String); {cut empty space from S (in the begining and at the end of line}
    procedure   ReadContent;
    procedure   WriteContent;
  end;

implementation

{***************************************************************************}
{****                              TIniItem                             ****}
{***************************************************************************}

constructor TIniItem.Init(AName, AData: PString);
begin
  Name := AName;
  Data := AData;
  NextItem := Nil;
end;

destructor  TIniItem.Done;
begin
  DisposeStr(Name);
  DisposeStr(Data);
end;

procedure   TIniItem.SetName(AName: PString);
begin
  DisposeStr(Name);
  Name := AName;
end;

procedure   TIniItem.SetString(S: String);
begin
  DisposeStr(Data);
  Data := NewStr(S);
end;

procedure   TIniItem.SetChar(C: Char); {SYR}
begin
  SetString(C);
end;

procedure   TIniItem.SetBool(B: Boolean);
begin
  SetString(BoolStr[BoolMode, B]);
end;

procedure   TIniItem.SetInt(L: Longint);
begin
  SetString(Int2Str(L, 1));
end;

procedure   TIniItem.SetFloat(F: TFloat);
var S: String;
begin
  if FloatPrecision < 0 then begin
    Str(F, S);
    SetString(S);
  end else SetString(Rel2Str(F, FloatPrecision + 2, FloatPrecision));
end;

function    TIniItem.GetStr: String;
begin
  GetStr := Data^;
end;

function    TIniItem.GetChar(Default: Char): Char; {SYR}
Var
    S: String;
begin
    S := UpperCase(Data^);
    GetChar := Default;
    if(Length(S)>0)Then
    Begin
        GetChar := S[1];
    End;
end;

function    TIniItem.GetBool(Default: Boolean): Boolean;
var S: String;
begin
  S := UpperCase(Data^);
  GetBool := Default;
  if (S = '1') or (S = 'TRUE') or (S = 'YES') or (S = 'OK') or (S = 'ДА') then GetBool := True else
  if (S = '0') or (S = 'FALSE') or (S = 'NO') or (S = 'НЕТ') then GetBool := False;
end;

function    TIniItem.GetInt(Default: Longint): Longint;
var Error: Integer;
    L: Longint;
begin
  Val(Data^, L, Error);
  if Error <> 0 then GetInt := Default else GetInt := L;
end;

function    TIniItem.GetFloat(Default: TFloat): TFloat;
var Error: Integer;
    R: TFloat;
begin
  Val(Data^, R, Error);
  if Error <> 0 then GetFloat := Default else GetFloat := R;
end;

function    TIniItem.GetStrForWriting: String;
var S: String;
begin
  S := '';
  if Name <> Nil then S := S + Name^;
  S := S + '=';
  if Data <> Nil then S := S + Data^;
  case Convert of
    cUpper: S := UpperCase(S);
    cLower: S := LowerCase(S);
  end;
  GetStrForWriting := S;
end;

{***************************************************************************}
{****                           TIniSection                             ****}
{***************************************************************************}

constructor TIniSection.Init(AName: PString);
begin
  Name := AName;
  FirstItem := Nil;
  NextSection := Nil;
end;

destructor  TIniSection.Done;
begin
  DisposeStr(Name);
  FreeSection;
end;

procedure   TIniSection.SetName(AName: PString);
begin
  DisposeStr(Name);
  Name := AName;
end;

function    TIniSection.RenameItem(OldName, NewName: String): Boolean;
var P: PIniItem;
begin
  P := GetItem(OldName, False);
  if P <> Nil then begin
    if GetItem(NewName, False) = Nil then begin
      P^.SetName(NewStr(NewName));
      RenameItem := True;
    end else RenameItem := False;
  end;
end;

procedure TIniSection.WriteChar(AName:String; C: Char); {SYR}
begin
  GetItem(AName, True)^.SetChar(C);
end;

procedure   TIniSection.WriteStr(AName, S: String);
begin
  GetItem(AName, True)^.SetString(S);
end;

procedure   TIniSection.WriteBool(AName: String; B: Boolean);
begin
  GetItem(AName, True)^.SetBool(B);
end;

procedure   TIniSection.WriteInt(AName: String; L: Longint);
begin
  GetItem(AName, True)^.SetInt(L);
end;

procedure   TIniSection.WriteFloat(AName: String; F: TFloat);
begin
  GetItem(AName, True)^.SetFloat(F);
end;

function TIniSection.ReadChar(AName: String; Default: Char): Char; {SYR}
var P: PIniItem;
begin
  P := GetItem(AName, False);
  if P = Nil then ReadChar := Default else ReadChar := P^.GetChar(Default);
end;

function    TIniSection.ReadStr(AName: String; Default: String): String;
var P: PIniItem;
begin
  P := GetItem(AName, False);
  if P = Nil then ReadStr := Default else ReadStr := P^.GetStr;
end;

function    TIniSection.ReadBool(AName: String; Default: Boolean): Boolean;
var P: PIniItem;
begin
  P := GetItem(AName, False);
  if P = Nil then ReadBool := Default else ReadBool := P^.GetBool(Default);
end;

function    TIniSection.ReadInt(AName: String; Default: Longint): Longint;
var P: PIniItem;
begin
  P := GetItem(AName, False);
  if P = Nil then ReadInt := Default else ReadInt := P^.GetInt(Default);
end;

function    TIniSection.ReadFloat(AName: String; Default: TFloat): TFloat;
var P: PIniItem;
begin
  P := GetItem(AName, False);
  if P = Nil then ReadFloat := Default else ReadFloat := P^.GetFloat(Default);
end;

procedure   TIniSection.DeleteItem(AName: String);
var PPrev, PSelf: PIniItem;
begin
  GetItemPointer(AName, PPrev, PSelf);
  if PSelf = Nil then Exit;
  if PPrev = Nil then FirstItem := PSelf^.NextItem
  else PPrev^.NextItem := PSelf^.NextItem;
  Dispose(PSelf, Done);
end;

procedure   TIniSection.FreeSection;
var P: PIniItem;
begin
  P := FirstItem;
  while P <> Nil do begin
    FirstItem := P^.NextItem;
    Dispose(P, Done);
    P := FirstItem;
  end;
end;

function    TIniSection.GetItem(AName: String; Create: Boolean): PIniItem;
var PPrev, PSelf: PIniItem;
begin
  GetItemPointer(AName, PPrev, PSelf);
  if (PSelf = Nil) and (Create) then begin
    PSelf := New(PIniItem, Init(NewStr(AName), Nil));
    if FirstItem = Nil then FirstItem := PSelf
    else GetLastItem^.NextItem := PSelf;
  end;
  GetItem := PSelf;
end;

function    TIniSection.GetLastItem: PIniItem;
var P: PIniItem;
begin
  if FirstItem = Nil then begin
    GetLastItem := Nil;
    Exit;
  end;
  P := FirstItem;
  while P^.NextItem <> Nil do P := P^.NextItem;
  GetLastItem := P;
end;

procedure   TIniSection.GetItemPointer(AName: String; var PPrev, PSelf: PIniItem);
begin
  PPrev := Nil;
  if FirstItem = Nil then begin
    PSelf := Nil;
    Exit;
  end;
  PSelf := FirstItem;
  while PSelf <> Nil do
    if UpperCase(PSelf^.Name^) <> UpperCase(AName) then begin
      PPrev := PSelf;
      PSelf := PSelf^.NextItem;
    end else Break;
  if PSelf = Nil then PPrev := Nil;
end;

procedure   TIniSection.WriteSelf(var F: Text);
var P: PIniItem;
    S: String;
begin
  if (FirstItem = Nil) and (not WriteEmptySection) then Exit;
  S := '[' + Name^ + ']';
  case Convert of
    cUpper: S := UpperCase(S);
    cLower: S := LowerCase(S);
  end;
  Writeln(F, S);
  P := FirstItem;
  while P <> Nil do begin
    Writeln(F, P^.GetStrForWriting);
    P := P^.NextItem;
  end;
end;

{***************************************************************************}
{****                             TIniFile                              ****}
{***************************************************************************}

constructor TIniFile.Init(AFileName: String);
begin
  FileName := NewStr(AFileName);
  FirstSection := Nil;
  ReadContent;
end;

destructor  TIniFile.Done(SaveChanges: Boolean);
begin
  if SaveChanges then WriteContent;
  FreeContent;
  DisposeStr(FileName);
end;

procedure   TIniFile.SetFileName(AFileName: PString);
begin
  DisposeStr(FileName);
  FileName := AFileName;
end;

function    TIniFile.RenameItem(ASection, OldName, NewName: String): Boolean;
var P: PIniSection;
begin
  P := GetSection(ASection, False);
  if P <> Nil then RenameItem := P^.RenameItem(OldName, NewName);
end;

function    TIniFile.RenameSection(OldName, NewName: String): Boolean;
var P: PIniSection;
begin
  P := GetSection(OldName, False);
  if P <> Nil then begin
     if GetSection(NewName, False) = Nil then begin
       P^.SetName(NewStr(NewName));
       RenameSection := True;
     end else RenameSection := False;
  end;
end;

procedure TIniFile.WriteChar(ASection, AName: String;C:Char); {SYR}
begin
    GetSection(ASection, True)^.WriteChar(AName, C);
end;

procedure   TIniFile.WriteStr(ASection, AName, S: String);
begin
  GetSection(ASection, True)^.WriteStr(AName, S);
end;

procedure   TIniFile.WriteBool(ASection, AName: String; B: Boolean);
begin
  GetSection(ASection, True)^.WriteBool(AName, B);
end;

procedure   TIniFile.WriteInt(ASection, AName: String; L: Longint);
begin
  GetSection(ASection, True)^.WriteInt(AName, L);
end;

procedure   TIniFile.WriteFloat(ASection, AName: String; F: TFloat);
begin
  GetSection(ASection, True)^.WriteFloat(AName, F);
end;

Function TIniFile.ReadChar(ASection, AName: String;
                           Default: Char): Char; {SYR}
Var
    P: PIniSection;
Begin
    P := GetSection(ASection, False);
    if P = Nil then
    ReadChar := Default
    else
    ReadChar := P^.ReadChar(AName, Default);
End;

function    TIniFile.ReadStr(ASection, AName: String; Default: String): String;
var P: PIniSection;
begin
  P := GetSection(ASection, False);
  if P = Nil then ReadStr := Default else ReadStr := P^.ReadStr(AName, Default);
end;

function    TIniFile.ReadBool(ASection, AName: String; Default: Boolean): Boolean;
var P: PIniSection;
begin
  P := GetSection(ASection, False);
  if P = Nil then ReadBool := Default else ReadBool := P^.ReadBool(AName, Default);
end;

function    TIniFile.ReadInt(ASection, AName: String; Default: Longint): Longint;
var P: PIniSection;
begin
  P := GetSection(ASection, False);
  if P = Nil then ReadInt := Default else ReadInt := P^.ReadInt(AName, Default);
end;

function    TIniFile.ReadFloat(ASection, AName: String; Default: TFloat): TFloat;
var P: PIniSection;
begin
  P := GetSection(ASection, False);
  if P = Nil then ReadFloat := Default else ReadFloat := P^.ReadFloat(AName, Default);
end;

procedure   TIniFile.DeleteItem(ASection, AName: String);
var P: PIniSection;
begin
  P := GetSection(ASection, False);
  if P <> Nil then P^.DeleteItem(AName);
end;

procedure   TIniFile.DeleteSection(ASection: String);
var PPrev, PSelf: PIniSection;
begin
  GetSectionPointer(ASection, PPrev, PSelf);
  if PSelf = Nil then Exit;
  if PPrev = Nil then FirstSection := PSelf^.NextSection
  else PPrev^.NextSection := PSelf^.NextSection;
  Dispose(PSelf, Done);
end;

procedure   TIniFile.FreeContent;
var P: PIniSection;
begin
  P := FirstSection;
  while P <> Nil do begin
    FirstSection := P^.NextSection;
    Dispose(P, Done);
    P := FirstSection;
  end;
end;

function    TIniFile.GetSection(ASection: String; Create: Boolean): PIniSection;
var PPrev, PSelf: PIniSection;
begin
  GetSectionPointer(ASection, PPrev, PSelf);
  if (PSelf = Nil) and (Create) then begin
    PSelf := New(PIniSection, Init(NewStr(ASection)));
    if FirstSection = Nil then FirstSection := PSelf
    else GetLastSection^.NextSection := PSelf;
  end;
  GetSection := PSelf;
end;

function    TIniFile.GetLastSection: PIniSection;
var P: PIniSection;
begin
  if FirstSection = Nil then begin
    GetLastSection := Nil;
    Exit;
  end;
  P := FirstSection;
  while P^.NextSection <> Nil do P := P^.NextSection;
  GetLastSection := P;
end;

procedure   TIniFile.GetSectionPointer(ASection: String; var PPrev, PSelf: PIniSection);
begin
  PPrev := Nil;
  if FirstSection = Nil then begin
    PSelf := Nil;
    Exit;
  end;
  PSelf := FirstSection;
  while PSelf <> Nil do
    if UpperCase(PSelf^.Name^) <> UpperCase(ASection) then begin
      PPrev := PSelf;
      PSelf := PSelf^.NextSection;
    end else Break;
  if PSelf = Nil then PPrev := Nil;
end;

procedure   TIniFile.SplitLine(S: String; var AName, AValue: String);
var DevPos: Integer;
begin
  DevPos := Pos('=', S);
  if DevPos = 0 then begin
    AName := '';
    AValue := '';
  end else begin
    AName := Copy(S, 1, DevPos - 1);
    AValue := Copy(S, DevPos + 1, Length(S) - DevPos);
  end;
end;

procedure   TIniFile.ShortLine(var S: String);
var I, N1, N2:Integer;
begin
  N1 := 0;
  for I := 1 to Length(S) do if S[I] = ' ' then Inc(N1) else Break;
  N2 := Length(S) - N1;
  for I := Length(S) downto N1 + 1 do if S[I] = ' ' then Dec(N2) else Break;
  S := Copy(S, N1, N2);
end;

procedure   TIniFile.ReadContent;
var F: Text;
    S, Sec, AName, AValue: String;
begin
  Assign(F, FileName^);
  {$I-}
  Reset(F);
  {$I+}
  if IOResult <> 0 then Exit;
  Sec := '';
  while not EOF(f) do begin
    Readln(F, S);
    ShortLine(S);
    if S = '' then Continue;
    if (S[1] = '[') and (S[Length(S)] = ']') then Sec := Copy(S, 2, Length(S) - 2)
    else begin
      SplitLine(S, AName, AValue);
      if (Sec <> '') and (AName <> '') then WriteStr(Sec, AName, AValue);
    end;
  end;
  Close(F);
end;

procedure   TIniFile.WriteContent;
var F: Text;
    P: PIniSection;
begin
  Assign(F, FileName^);
  {$I-}
  Rewrite(F);
  {$I+}
  if IOResult <> 0 then Exit;
  P := FirstSection;
  while P <> Nil do begin
    P^.WriteSelf(F);
    P := P^.NextSection;
    if P <> Nil then Writeln(F);
  end;
  Close(F);
end;

end.
