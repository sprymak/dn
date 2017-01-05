
uses Dos;

 Var I: Longint;
     F: Text;
     K: Byte;
     C: Char;

 function  ItoS(A:longint):string;
  var S : string[12];
      J: Byte;
 begin
  Str(A:5, S);
  for J := 1 to Byte(S[0]) do if S[J] = ' ' then S[J] := #$30;
  ItoS := S;
 end;

 function  ItoS1(A:longint):string;
  var S : string[12];
      J: Byte;
 begin
  Str(A, S);
  ItoS1 := S;
 end;

begin
 Writeln('Do you really want to create 5000 test files in current directory (Y/N)?');
 Readln(C);
 if (C = 'y') or (C = 'Y') then
  for K := 1 to 5 do
   for I := 1 to 1000 do
    begin
     Assign(F,'f_'+ItoS(I)+'.ex'+ItoS1(K));
     Rewrite(F);
     Write(F,'-');
     Close(F)
    end;
end.