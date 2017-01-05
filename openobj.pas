
program OpenObj;
 uses DOS, Os2PMAPI, Strings;
 var Handle: LongInt;
         PS: PChar;
      PSArr: array[0..255] of Char;
begin
 if Paramcount <> 1 then
   begin
     writeln('WPS file object opening utility for DN/2');
     writeln('(c) J.Osadtchiy, 2001');
     writeln;
     writeln('Usage: OPENOBJ <FileToOpen>');
     Halt(1);
   end;
 PS := PSArr;
 PS := StrPCopy (PS, Paramstr(1));
 Handle :=  WinQueryObject(PS);
{Writeln('Handle = ', Handle);}
 If WinOpenObject(Handle, OPEN_DEFAULT, False) then Writeln('object successfully opened')
   else Writeln('failed to open object');
end.