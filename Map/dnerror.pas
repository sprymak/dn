{⁄¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬ø}
{√≈≈èpÆ£p†¨¨† Ø•p•p†°Æ‚™® DN.ERR ‰†©´† ¢ y§Æ°≠Î© ¢®§ ≈≈¥}
{√≈≈  Ä¢‚Æp: Ä≠‚Æ≠ î•§ÆpÆ¢ aka DataCompBoy           ≈≈¥}
{√≈≈  Ç†Ô´ ¢ Á•·‚Ï å†‚†pÎ™®≠Æ© ì´ÏÔ≠Î...             ≈≈¥}
{¿¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡Ÿ}
{$i-}{$m 8192, 102400, 655360}{$g+}
program DnError;

uses Objects, Search, KeyBoard, Dos, Crt;

const HexCh: Set Of Char = ['0'..'9','A'..'F', 'a'..'f'];

function FromHex(S: string): longint;
 var b: byte;
     l: longint;
 begin
  l:=0;
  for b:=1 to length(s) do
   if s[b] in ['0'..'9']
    then l:=l shl 4+byte(s[b])-byte('0')
    else
     if UpCase(s[b]) in ['A'..'F']
      then l:=l shl 4+byte(UpCase(s[b]))-byte('A')+10;
  FromHex:=l;
 end;

function ToDec(i: word): string; var s: string[5]; begin Str(i, s); ToDec:=s; end;

function Expl(s: string; n: byte): string;
 var i: byte;
 begin
  for i:=length(s) to n do s:=s+' ';
  Expl:=s;
 end;

var FileNames: TStringCollection;

Type TSR = Record
            FileInd:     Word;
            FuncInd:     Word;
            LineNum:     Word;
            StrtAddrSeg: Word;
            StrtAddrOfs: Word;
           End;

var Delta: longint;
    Mpp: TBufStream;
    Err, TS: Text;
    HasMap: Boolean;
    s,s2: string;
    sr: TSR;
    SS, SO: Word;
    c: char;
    FFC: String[5];
    IS: Byte;
    np: longint;
    ERC: Word;
    NumEl: longint;
    i: integer;
    r: registers;

Function Compare(a: longint): longint; Far;
 var fr: TSR;
 begin
  Mpp.Seek(a*SizeOf(TSR)+Delta);
  Mpp.Read(fr, SizeOf(TSR));
  Compare:=0;
  if fr.StrtAddrSeg<SS then Compare:=-1 else
  if fr.StrtAddrSeg>SS then Compare:=+1 else
  if fr.StrtAddrOfs<SO then Compare:=-1 else
  if fr.StrtAddrOfs>SO then Compare:=+1;
 end;

begin
 Writeln(' DNError v2.2 (c) Anton Fedorov aka DataCompBoy 1999-2000');
 Assign(Err, 'DN.ERR');
 Reset(Err);
 if IOResult<>0 then
  begin
   Writeln('Nothing to do - DN.ERR not found.');
   halt;
  end;
 Assign(TS, 'DN2SEND.ERR');
 Append(TS);
 If IOResult<>0 then Rewrite(TS);
 if IOResult<>0 then
  begin
   writeln('Can''t create DN2SEND.ERR');
   halt(1);
  end;

 FileNames.Init($8, $8);
 Mpp.Init('DN.MPP', stOpenRead, $1000);
 if Mpp.Status<>stOk then
  begin
   HasMap:=false;
   Mpp.Init('DN.MAP', stOpenRead, $1000);
   if Mpp.Status=stOk then
    begin
     Mpp.Done;
     writeln('Found uncompiled DN.MAP. Recommend to compile them before run DNError.');
     write('Continue (y/n) ?');
     repeat
      C:=ReadKey;
     until c in ['y','Y','n','N'];
     if c in ['n','N'] then halt;
    end;
  end
 else
  begin
   repeat
    S:='';
    c:=' ';
    while c<>#0 do
     begin
      Mpp.Read(C, SizeOf(C));
      if c<>#0 then S:=S+C;
     end;
    if s<>'' then FileNames.AtInsert(FileNames.Count, NewStr(S));
   until s='';
   repeat
    S:='';
    c:=' ';
    while c<>#0 do
     begin
      Mpp.Read(C, SizeOf(C));
      if c<>#0 then S:=S+C;
     end;
   until s='';
   Delta:=Mpp.GetPos;
   NumEl:=(Mpp.GetSize-Delta) div SizeOf(TSR);
   HasMap:=True;
  end;
 s2:='='; S:='=';
 While (Not EOF(ERR)) or (IS>0) do
  begin
   S2:=S;
   if not EOF(Err) then ReadLn(ERR, S) else s:='';
   FFC:=Copy(S, 1, 5);
   if IS>0 then
    begin
     if (s<>'') and (FFC<>'DESC:') then
      begin
       if length(S)<80
        then Writeln(S)
        else if Length(S)=80 then Write(S)
         else begin
          s2:='';
          For i:=1 to 80 do s2:=s2+s[i]+Chr(FromHex(Copy(s, ((i-1)*2)+81, 2)));
          s2:=s2+#0;
          r.ax:=$1303;
          r.bx:=0;
          r.cx:=80;
          r.dh:=WhereY-1;
          r.dl:=WhereX-1;
          r.es:=Seg(s2);
          r.bp:=Ofs(s2)+1;
          intr($10,r);
         end;
       inc(IS);
      end
     else
      begin
       IS:=0;
       if FFC<>'DESC:' then
        begin
         write(' Enter description of problem:');
         readln(S);
         S:='DESC:'+S;
        end;
      end;
    end
   else if FFC='ERR :' then
    begin
     ERC:=FromHex(Copy(S, 6, 2));
     case erc of
        0 : S:=S + ' State dump';
        1 : S:=S + ' Invalid function number ';
        2 : S:=S + ' File not found ';
        3 : S:=S + ' Path not found ';
        4 : S:=S + ' Too many open files ';
        5 : S:=S + ' File access denied ';
        6 : S:=S + ' Invalid file handle ';
       12 : S:=S + ' Invalid file access code ';
       15 : S:=S + ' Invalid drive number ';
       16 : S:=S + ' Cannot remove current directory ';
       17 : S:=S + ' Cannot rename across drives ';
       18 : S:=S + ' No more files ';
      100 : S:=S + ' Disk read error ';
      101 : S:=S + ' Disk write error ';
      102 : S:=S + ' File not assigned ';
      103 : S:=S + ' File not open ';
      104 : S:=S + ' File not open for input ';
      105 : S:=S + ' File not open for output ';
      106 : S:=S + ' Invalid numeric format ';
      150 : S:=S + ' Disk is write-protected ';
      151 : S:=S + ' Bad drive request struct length ';
      152 : S:=S + ' Drive not ready ';
      154 : S:=S + ' CRC error in data ';
      156 : S:=S + ' Disk seek error ';
      157 : S:=S + ' Unknown media type ';
      158 : S:=S + ' Sector Not Found ';
      159 : S:=S + ' Printer out of paper ';
      160 : S:=S + ' Device write fault ';
      161 : S:=S + ' Device read fault ';
      162 : S:=S + ' Hardware failure ';
      200 : S:=S + ' Division by zero ';
      201 : S:=S + ' Range check error ';
      202 : S:=S + ' Stack overflow error ';
      203 : S:=S + ' Heap overflow error ';
      204 : S:=S + ' Invalid pointer operation ';
      205 : S:=S + ' Floating point overflow ';
      206 : S:=S + ' Floating point underflow ';
      207 : S:=S + ' Invalid floating point operation ';
      208 : S:=S + ' Overlay manager not installed ';
      209 : S:=S + ' Overlay file read error ';
      210 : S:=S + ' Object not initialized ';
      211 : S:=S + ' Call to abstract method ';
      212 : S:=S + ' Stream registration error ';
      213 : S:=S + ' Collection index out of range ';
      214 : S:=S + ' Collection overflow error ';
      215 : S:=S + ' Arithmetic overflow error ';
      216 : S:=S + ' General Protection fault ';
      else  S:=S + ' Unknown error';
     end;
    end
   else if FFC='ADDR:' then
    begin
     SS:=FromHex(Copy(S, 6, 4));
     SO:=FromHex(Copy(S, 10,4));
     If HasMap then
      begin
       np:=BinSearch(Compare, NumEl);
       Mpp.Seek(np*SizeOf(TSR)+Delta);
       Mpp.Read(sr, SizeOf(TSR));
       S:=S+' File - '+Expl(PString(FileNames.At(sr.FileInd))^, 15)+';'
           +' Line - '+Expl(ToDec(sr.LineNum), 15);
      end;
    end
   else if (FFC[1] in hexch) and
           (FFC[2] in hexch) and
           (FFC[3] in hexch) and
           (FFC[4] in hexch) then
    begin
     SS:=FromHex(Copy(s, 1, 4));
     SO:=FromHex(Copy(S, 6, 4));
     If HasMap then
      begin
       np:=BinSearch(Compare, NumEl);
       Mpp.Seek(np*SizeOf(TSR)+Delta);
       Mpp.Read(sr, SizeOf(TSR));
       S:=S+' File - '+Expl(PString(FileNames.At(sr.FileInd))^, 15)
           +' Line - '+Expl(ToDec(sr.LineNum), 15);
      end;
    end
   else if FFC='SCR :' then
    IS:=1;
   ;
   InOutRes:=0;
   Writeln(TS, S);
  end;

 Close(ERR); Erase(ERR);
 Close(TS);

 FileNames.Done;
 Mpp.Done;
end.