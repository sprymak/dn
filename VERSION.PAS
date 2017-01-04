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

Uses Advance ;

{$I STDEFINE.INC}

CONST
  VersionName = '1.51.04/DOS';
  VersionDate = 'September 19, 1999';
  VersionWord = 15104;


  SourceDir  = '';

{
 function CrStr( S : String ) : String ;
  var I,Y : word ;
      L : array[0..$ff] of byte absolute S;
      B : byte ;
      Ok : boolean ;
 begin

  B := L[0];
     for I := 1 to L[0] do
      begin

         OK := false ;
      for Y :=0 to $FF do
        if L[I] = Y xor ( ( B xor $ff ) and ( I ) )
         then
         begin
          B := L[i];
          L[I] := Y;
          Ok := true ;
          Break ;
         end;
        if not Ok then Writeln('ERR');
      end;

  Crstr := S ;
 end;
 }
{


 function CrStr( S : String ) : String ;
 var I : integer ;
     W,Y : word ;
     BB : array[1..2] of byte absolute W ;
     C : byte ;
     D : byte ;
     OK: boolean ;
  begin

   C := byte(S[0]);
   W := C or $100 ;

     for I := 1 to Length(S) do
      begin

        OK := False ;

        for Y:=1 to $FF do
         begin

         D:= W mod Y ;

          if D = byte(S[I]) then
             begin
              OK := true ;
              Break;
             end;
         end;

         if Not ok then Writeln('ERR');



         C := byte(S[i]);
         inc(W,C);

         byte(S[i]) := Y ;

      end;


     CrStr := S;
  end;


 function DeStr( S : String ) : String ;
 var I : Integer ;
  begin
     for I := 1 to Length(S) do
      begin
       byte( S[I] ) :=  (byte(S[I-1]) shl 8) mod
                        (byte(S[I]) );
      end;

    DeStr := S;
  end;

 }

var F : Text ;
    S,S1 : string ;

 begin
  {
  S:=' This is sample test string an nothing else .... ±±±±±±±±±±±±±±±±±±±±±±±±±±±±' ;
  S:='                                                                             ';

  Writeln(S);
  S1:=CrStr(S);

  }
  {
  Writeln(S);
  Writeln(S1);
  S1:=DeStr(S1);
  Writeln(S1);
  halt(1);
  }


  Writeln(' þ Creating ',SourceDir + 'VERSION.INC');
  Assign(F,SourceDir + 'VERSION.INC');
  ClrIo;
  Rewrite(F);
  Writeln(F,'  Const');
  Writeln(F,'     VersionName =''',VersionName,''';');
  Writeln(F,'     VersionDate =''',VersionDate,''';');
  Writeln(F,'     VersionWord = $', Hex4(VersionWord) , ';');

  Close(F);
 end .
