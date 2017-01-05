unit RTPatch;
(******

Runtime Patch (for Virtual Pascal)
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}

interface

//function UnlockMemory(Base: Pointer; Size: LongInt): Boolean;
function RuntimePatch(OldFunc, NewFunc: Pointer): boolean;

implementation

{$IFDEF OS2}
uses
  Os2Base;

function UnlockMemory(Base: Pointer; Size: longInt): boolean;
  inline;
  begin
    UnlockMemory := (DosSetMem(Base, Size, pag_Execute+pag_Read+
      pag_Write) = 0);
  end;
{$ENDIF}

{$IFDEF Win32}
uses
  Windows;

function UnlockMemory(Base: Pointer; Size: longInt): boolean;
  inline;
  var
    OldProtect: longInt;
  begin
    UnlockMemory := VirtualProtect(Base, Size,
      page_Execute_ReadWrite, @OldProtect);
  end;
{$ENDIF}

type
  PJMP = ^TJMP;
  TJMP = packed record
    JMP: byte;
    Offset: longInt;
    end;

function RuntimePatch(OldFunc, NewFunc: Pointer): boolean;
  begin
    Result := UnlockMemory(OldFunc, SizeOf(TJMP));
    if Result then
      with PJMP(OldFunc)^ do
        begin
          JMP := $E9;
          Offset := longInt(NewFunc)-longInt(OldFunc)-5;
        end;
  end;

end.
