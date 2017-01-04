(*   RW_IO.PAS
**
**   Reader/Writer Buffered I/O
**
**   Reader() and Writer() are called directly by the LZW4PLIB.ASM code.
**   They should never be called by your application code.
**
**   The other functions are never called by the LZW4PLIB.ASM code, but are
*    called only by your application routines.
**
**   Note that only a Reader() and Writer() function is required by the
**   LZW4PLIB.ASM code. This means that you have complete control over data
**   coming into and out of the compression/expansion code. Instead of
**   reading or writing to disk, you can just as easily read/write to a
**   buffer, serial port, etc. You just have to write the Reader() and
**   Writer() code.
*)

unit RW_IO;

interface

function ReaderOpen(Filename : String) : Integer;
function Reader : Integer;
function ReaderClose : Integer;
function ReaderCount : LongInt;
function ReaderEnd : Boolean;

function WriterOpen(Filename : String) : Integer;
function Writer(TheByte : Byte) : Integer;
function WriterBuf(var Buf; BufSize: word ) : Integer;
function WriterClose : Integer;
function WriterCount : LongInt;
function DummyWrite(TheByte : Byte) : Integer;

implementation

const
   BUFFER_SIZE = 2048;

type
   IOstruct = record
      Handle   : File;
      Left     : Integer;
      Right    : Integer;
      Count    : LongInt;
      Buffer   : array[0..BUFFER_SIZE-1] of Byte;
   end;

var
   InpCtrl : IOstruct;
   OutCtrl : IOstruct;

function ReaderOpen(Filename : String) : Integer;
begin
   Assign(InpCtrl.Handle,Filename);
   Reset(InpCtrl.Handle,1);
   InpCtrl.Left := 0;
   InpCtrl.Right := 0;
   InpCtrl.Count := 0;
   ReaderOpen := IOResult;
end;

function Reader : Integer;
var
  TheByte : Byte;
begin
  if InpCtrl.Left=InpCtrl.Right then
     begin
        (* read next buffer *)
        InpCtrl.Left := 0;
        BlockRead(InpCtrl.Handle,InpCtrl.Buffer,BUFFER_SIZE,InpCtrl.Right);
        if InpCtrl.Right <= 0 then
           begin
             Reader := -1;
             exit;
           end;
     end;
  (* return next byte in buffer *)
  TheByte := InpCtrl.Buffer[InpCtrl.Left];
  InpCtrl.Left := InpCtrl.Left + 1;
  InpCtrl.Count := InpCtrl.Count + 1;
  Reader := TheByte;
end;

function ReaderClose : Integer;
begin
  close(InpCtrl.Handle);
end;

function ReaderCount : LongInt;
begin
  ReaderCount := InpCtrl.Count;
end;

function ReaderEnd : Boolean;
begin
  ReaderEnd := EOF(InpCtrl.Handle);
end;

function WriterOpen(Filename : String) : Integer;
begin
   Assign(OutCtrl.Handle,Filename);
   Rewrite(OutCtrl.Handle,1);
   OutCtrl.Left := 0;
   OutCtrl.Right := 0;
   OutCtrl.Count := 0;
   WriterOpen := IOResult;
end;

function Writer(TheByte : Byte) : Integer;
begin
  OutCtrl.Count := OutCtrl.Count + 1;
{ if (OutCtrl.Count and $0fff) = 0 then write('.');}
  OutCtrl.Buffer[OutCtrl.Right] := TheByte;
  OutCtrl.Right := OutCtrl.Right + 1;
  if OutCtrl.Right = BUFFER_SIZE then
    begin
      (* write buffer to disk *)
       BlockWrite(OutCtrl.Handle,OutCtrl.Buffer,BUFFER_SIZE);
       OutCtrl.Right := 0;
    end;
  Writer := 0;
end;

function WriterBuf(var Buf; BufSize: word ) : Integer;
var Cnt: word;
begin
 BlockWrite(OutCtrl.Handle,OutCtrl.Buffer,OutCtrl.Right);
 OutCtrl.Right := 0;
 BlockWrite(OutCtrl.Handle,Buf,BufSize, Cnt);
 OutCtrl.Count := OutCtrl.Count + Cnt;
end;

function WriterClose : Integer;
begin
  if OutCtrl.Right > 0 then
    begin
       BlockWrite(OutCtrl.Handle,OutCtrl.Buffer,OutCtrl.Right);
       OutCtrl.Right := 0;
    end;
  close(OutCtrl.Handle);
end;

function WriterCount : LongInt;
begin
  WriterCount := OutCtrl.Count;
end;

function DummyWrite(TheByte : Byte) : Integer;
begin
  DummyWrite := 0;
end;

end.