{$I STDEFINE.INC}
unit LFNCol;

interface

uses Advance1, Advance2, Objects {$IFNDEF STRINGLFN}, Strings{$ENDIF};

{$IFNDEF STRINGLFN}
type
 TLFNIndex = Pointer;
{$ELSE}
type
 TLFNIndex = String;
{$ENDIF}


function    AddLFN(const Name: string): TLFNIndex;   {Add name}
procedure   DelLFN(var ind: TLFNIndex);              {Delere name}
function    UseLFN(ind: TLFNIndex):TLFNIndex;        {Increase name usage}
function    GetLFN(ind: TLFNIndex): string;          {Return name as String}
procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);{Store name}
function    LoadLFN(var S: TStream): TLFNIndex;      {Load name}


implementation

{$IFNDEF STRINGLFN}

function    AddLFN(const Name: string): TLFNIndex;
var P:TLFNIndex;
begin
  P := NewStr(Name+#0);
  AddLFN := P;
end;

procedure   DelLFN(var ind: TLFNIndex);
begin DisposeStr(PString(ind)) end;

function    GetLFN(ind: TLFNIndex): string;
begin
 if ind<>nil
  then GetLFN := Copy(PString(ind)^, 1, Length(PString(ind)^)-1)
  else GetLFN := ''
end;

function    UseLFN(ind: TLFNIndex): TLFNIndex;
begin UseLFN := AddLFN(GetLFN(ind)) end;

procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);
begin
  S.WriteStr(LFN);
end;

function    LoadLFN(var S: TStream): TLFNIndex;
begin
  LoadLFN := S.ReadStr;
end;

{$ELSE}

function    AddLFN(const Name: string): TLFNIndex;
begin
  AddLFN := Name;
end;

procedure   DelLFN(var ind: TLFNIndex);
begin end;


function    GetLFN(ind: TLFNIndex): string;
begin
  GetLFN := ind;
end;

function    UseLFN(ind: TLFNIndex): TLFNIndex;
begin UseLFN := ind; end;

procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);
begin
  S.WriteStr(@LFN);
end;

function    LoadLFN(var S: TStream): TLFNIndex;
begin
  LoadLFN := (S.ReadStr)^;
end;

{$ENDIF}

end.
