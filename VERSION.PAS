{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
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
//     "Based on Dos Navigator by RIT Research Labs."
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
{$I STDEFINE.INC}

Uses LFN, {DataCompBoy}
     Dos,
     advance1, advance, objects;


CONST
  VersionName:string = '1.51.10b5';
  VersionDate:string = '';
  VersionWord:Aword   = 15198;

var F : lText ;
    Day, Month, Year, DOW: word;

CONST
 DOWs:   Array[0..6] of string[3]=
   ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
 Months: Array[1..12]of string[3]=
   ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
 Days:   Array[1..31]of string[2]=
   ('01','02','03','04','05','06','07','08','09','10',
    '11','12','13','14','15','16','17','18','19','20',
    '21','22','23','24','25','26','27','28','29','30','31');

 begin
  if (ParamCount=0) or (ParamCount>3) then halt(1);
  if ParamStr(2) = '' then
    VersionName := VersionName + '/' +
                         {$IFNDEF NONBP}
                           {$IFDEF DPMI}
                             +'DPMI'
                           {$ELSE}
                             +'DOS'
                           {$ENDIF}
                         {$ELSE}
                           {$IFDEF OS2}
                             +'OS2'
                           {$ELSE}
                           {$IFDEF WIN32}
                             +'Win32'
                           {$ELSE}
                           {$IFDEF DPMI32}
                             +'DPMI32'
                           {$ELSE}
                             +'32bit'
                           {$ENDIF}
                           {$ENDIF}
                           {$ENDIF}
                         {$ENDIF}
  else
    VersionName := VersionName + ParamStr(2);
  {Cat: компиляция плагинской версии:
        - либо если задан параметр PLUGIN (компиляция посредством _DNC.CMD)
        - либо если в STDEFINE.INC установлена директива PLUGIN (компиляция вручную)}
  {$IFNDEF PLUGIN}
  if ParamStr(ParamCount) = 'PLUGIN' then
  {$ENDIF}
    VersionName := VersionName + '/Plugin';

  Writeln(' '#254' Creating ', ParamStr(1) + ' ' + VersionName);

  GetDate(Year, Month, Day, DOW);
  Str(Year, VersionDate);
  VersionDate:=DOWs[DOW]+', '+Days[Day]+' '+Months[Month]+' '+VersionDate+' at '+GetDateTime(true);
  lAssignText(F, ParamStr(1));
  ClrIo;
  lRewriteText(F);
  Writeln(F.T,'{/////////////////////////////////////////////////////////////////////////');
  Writeln(F.T,'//');
  Writeln(F.T,'//  Dos Navigator Open Source '+VersionName);
  Writeln(F.T,'//  Based on Dos Navigator (C) 1991-99 RIT Research Labs');
  Writeln(F.T,'//');
  Writeln(F.T,'//  This programs is free for commercial and non-commercial use as long as');
  Writeln(F.T,'//  the following conditions are aheared to.');
  Writeln(F.T,'//');
  Writeln(F.T,'//  Copyright remains RIT Research Labs, and as such any Copyright notices');
  Writeln(F.T,'//  in the code are not to be removed. If this package is used in a');
  Writeln(F.T,'//  product, RIT Research Labs should be given attribution as the RIT Research');
  Writeln(F.T,'//  Labs of the parts of the library used. This can be in the form of a textual');
  Writeln(F.T,'//  message at program startup or in documentation (online or textual)');
  Writeln(F.T,'//  provided with the package.');
  Writeln(F.T,'//');
  Writeln(F.T,'//  Redistribution and use in source and binary forms, with or without');
  Writeln(F.T,'//  modification, are permitted provided that the following conditions are');
  Writeln(F.T,'//  met:');
  Writeln(F.T,'//');
  Writeln(F.T,'//  1. Redistributions of source code must retain the copyright');
  Writeln(F.T,'//     notice, this list of conditions and the following disclaimer.');
  Writeln(F.T,'//  2. Redistributions in binary form must reproduce the above copyright');
  Writeln(F.T,'//     notice, this list of conditions and the following disclaimer in the');
  Writeln(F.T,'//     documentation and/or other materials provided with the distribution.');
  Writeln(F.T,'//  3. All advertising materials mentioning features or use of this software');
  Writeln(F.T,'//     must display the following acknowledgement:');
  Writeln(F.T,'//     "Based on Dos Navigator by RIT Research Labs.');
  Writeln(F.T,'//');
  Writeln(F.T,'//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS');
  Writeln(F.T,'//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED');
  Writeln(F.T,'//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE');
  Writeln(F.T,'//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR');
  Writeln(F.T,'//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL');
  Writeln(F.T,'//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE');
  Writeln(F.T,'//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS');
  Writeln(F.T,'//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER');
  Writeln(F.T,'//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR');
  Writeln(F.T,'//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF');
  Writeln(F.T,'//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.');
  Writeln(F.T,'//');
  Writeln(F.T,'//  The licence and distribution terms for any publically available');
  Writeln(F.T,'//  version or derivative of this code cannot be changed. i.e. this code');
  Writeln(F.T,'//  cannot simply be copied and put under another distribution licence');
  Writeln(F.T,'//  (including the GNU Public Licence).');
  Writeln(F.T,'//');
  Writeln(F.T,'//////////////////////////////////////////////////////////////////////////}');
  Writeln(F.T,'  Const');
  Writeln(F.T,'     VersionName =''',VersionName,''';');
  Writeln(F.T,'     VersionDate =''',VersionDate,''';');
  Writeln(F.T,'     VersionWord:AWord = $', Hex4(VersionWord), ';');

  Close(F.T);
 end.
