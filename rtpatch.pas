unit RTPatch;
(******

Runtime Patch (for Virtual Pascal)
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}

interface

{$IFDEF VIRTUALPASCAL}
//function UnlockMemory(Base: Pointer; Size: LongInt): Boolean;
function RuntimePatch(OldFunc, NewFunc: Pointer): Boolean;
{$ENDIF}

implementation

{$IFDEF VIRTUALPASCAL}

{$IFDEF OS2}
uses
  OS2Base;

function UnlockMemory(Base: Pointer; Size: LongInt): Boolean;
inline;
begin
  UnlockMemory := (DosSetMem(Base, Size, pag_Execute+pag_Read+pag_Write) = 0);
end;
{$ENDIF}

{$IFDEF Win32}
uses
  Windows;

function UnlockMemory(Base: Pointer; Size: LongInt): Boolean;
inline;
var
  OldProtect: LongInt;
begin
  UnlockMemory := VirtualProtect(Base, Size, page_Execute_ReadWrite, @OldProtect);
end;
{$ENDIF}

type
  PJMP = ^TJMP;
  TJMP = packed record
    JMP: Byte;
    Offset: LongInt;
  end;

function RuntimePatch(OldFunc, NewFunc: Pointer): Boolean;
begin
  Result := UnlockMemory(OldFunc, SizeOf(TJMP));
  if Result then
    with PJMP(OldFunc)^ do
      begin
        JMP := $E9;
        Offset := LongInt(NewFunc)-LongInt(OldFunc)-5;
      end;
end;
{$ENDIF}

end.
