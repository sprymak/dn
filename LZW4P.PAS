unit LZW4P;

interface

function InitLZW(AllocP : Pointer;
                BitCode : Integer) : Integer;  (* Initializes LZW4P *)
function TermLZW(FreeP : Pointer)  : Integer;  (* Terminates LZW4P *)
function Compress(ReaderP, WriterP : Pointer) : Integer;  (* Compresses *)
function Expand(ReaderP, WriterP   : Pointer) : Integer;  (* Expands *)

function LZAllocMemory(Size : Word) : Pointer;
function LZFreeMemory(P : Pointer; Size : Word) : Integer;

implementation

{$L LZW4PLIB}

function InitLZW;  external;
function TermLZW;  external;
function Compress; external;
function Expand;   external;

function LZAllocMemory(Size : Word) : Pointer;
var
  P     : Pointer;
begin
 GetMem(P, Size);
 (*writeln('Allocating ',Size,' bytes');*)
 LZAllocMemory := P;
end;

function LZFreeMemory(P : Pointer; Size : Word) : Integer;
begin
 FreeMem(P, Size);
 (*writeln('Freeing ',Size,' bytes');*)
 LZFreeMemory := 0;
end;



end.