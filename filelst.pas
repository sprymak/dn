{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.10
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
unit filelst;

interface
uses collect;

procedure MakeListFile(APP: Pointer; Files: PCollection);
function  ParseAddress(Address: String; var Zone, Net, Node, Point: Word): Boolean;

implementation
uses Startup, Lfn, Messages, objects, filescol, advance2, advance1, usermenu,
     advance, histlist, commands, dnapp, dnutil, tree, views, drivers, drives
     {$IFDEF VIRTUALPASCAL}, Dos{$ENDIF};

function ParseAddress(Address: String; var Zone, Net, Node, Point: Word): Boolean;
  var I,J: Integer;
begin
  ParseAddress := Off;
  Point := 0;
  I := PosChar('@', Address);
  if I > 0 then Delete(Address, I, 255);
  I := PosChar(':', Address);
  if I>0 then begin
   Val(Copy(Address, 1, I-1), Zone, J);
   If J<>0 then exit;
   Delete(Address, 1, I);
  end;
  I := PosChar('/', Address);
  if I>0 then begin
   Val(Copy(Address, 1, I-1), Net, J);
   If J<>0 then exit;
   Delete(Address, 1, I);
  end;
  I := PosChar('.', Address);
  if I>0 then begin
   Val(Copy(Address, I+1, 255), Point, J);
   If J<>0 then exit;
   Delete(Address, I, 255);
  end;
  If Length(Address)>0 then begin
   Val(Address, Node, J);
   If J<>0 then exit;
  end;
  ParseAddress := On;
end;


procedure MakeListFile;
 label AddrError, Retry;
 var I, J, K: Integer;
     D, Dr, SR, Sn: String;
     P: PFileRec;
     S: TMakeListRec;
     T,HF: lText;
     Nm: String;
     Xt: String;
     FidoMode: Boolean;
     SS: String;
     PP: PString;
     ZZ, NN, ND, PT: Word;
     BB: Boolean;
     FLD, Dr_: String;
     CurPos: integer;
     UPr: TUserParams;

  function GetNextName(var TheString,TheName:string):boolean;
  var j: boolean;
  begin
      while (CurPos<=Length(TheString)) and (TheString[CurPos] in [';',' '])
      do Inc(CurPos);
      TheName:='';
      j:=false;
      while (CurPos<=Length(TheString)) and ((TheString[CurPos]<>';') or j) do begin
          If TheString[CurPos]='"' then j:=not j;
          TheName:=TheName+TheString[CurPos];
          Inc(CurPos)
      end;
      while TheName[Length(TheName)]=' ' do Dec(TheName[0]);
      GetNextName:=TheName<>''
  end;

  function SomeFilesExist(var NameList:string):boolean;
  var TheName:string;
  begin
      SomeFilesExist:=False;
      CurPos:=1;
      while GetNextName(NameList,TheName) do if ExistFile(TheName)
      then begin
          SomeFilesExist:=True;
          Exit
      end
  end;

  procedure MakeStr(D: String);
  label Fail;
    var Drr: Boolean;
  begin
  { BB means "Is filename occurs in Action?" }
    Replace('!!', #1, D);Replace('##', #2, D);
    Replace('$$', #3, D);Replace('&&', #4, D);
    if {$IFNDEF OS2}(PosChar('!',D)=0) and {$ENDIF}
       (PosChar('#',D)=0) then
     if (D[Length(D)] in [#8,' ']) or ((Length(D)=1) and (D[1] in ['^',#2]))
      then D:=D+ '!.!'
      else D:=D+' !.!';
    {$IFDEF OS2}Replace('!', '#', D); Replace('$', '&', D);{$ENDIF}
    If ( S.Options and 2 = 2 ) and
       {$IFNDEF OS2}
       ( lfGetShortFileName(Sr) <> lfGetShortFileName(Dr) ) and
       ( Pos( '!\', D ) = 0 ) and
       ( Pos( '!/', D ) = 0 ) and
       ( Pos( '!:', D ) = 0 ) and
       {$ELSE}
       ( Sr <> Dr ) and
       {$ENDIF}
       ( Pos( '#\', D ) = 0 ) and
       ( Pos( '#/', D ) = 0 ) and
       ( Pos( '#:', D ) = 0 ) then begin { Need to force insert !:!\ }
         {$IFNDEF OS2}
         K := Pos( '.!', D );
         If K = 0 then begin
           K := PosChar( '!', D );
         {$ENDIF}
           If K = 0 then begin
            K := Pos( '.#', D );
            If K = 0 then begin
             K := PosChar( '#', D );
             If K = 0 then goto Fail;
            end else
            If ( K <> 1 ) and ( D[ K - 1 ] in ['!','#'] ) then Dec( K )
                                                          else goto Fail;
           end else
           If ( K <> 1 ) and ( D[ K - 1 ] in ['!','#'] ) then Dec( K )
                                                         else goto Fail;
          {$IFNDEF OS2}
          end else
          If ( K <> 1 ) and ( D[ K - 1 ] in ['!','#'] ) then Dec( K )
                                                        else goto Fail;
          {$ENDIF}
      Insert( '!:!\', D, K );
    end;
  Fail: { Cannot find place to insert !\ }
    Replace(#1, '!!', D);Replace(#2, '##', D);
    Replace(#3, '$$', D);Replace(#4, '&&', D);
    D:=MakeString(D, @UPr, off, nil);
    WriteLn(T.T, D);
  end;

begin
 if Files^.Count = 0 then Exit;
 Message(APP, evBroadcast, cmGetUserParams, @UPr);
 FillChar(S, SizeOf(S), 0);
 S.FileName := HistoryStr(hsMakeList, 0);
 if S.FileName = '' then S.FileName := 'DNLIST.BAT';
 S.Action := HistoryStr(hsExecDOSCmd, 0);
 S.Header := ''; S.HeaderMode:=hfmAuto;
 S.Footer := ''; S.FooterMode:=hfmAuto;
 S.Options := MakeListFileOptions;
 if S.Options and cmlPathNames <> 0 then
 begin
   BB := Off;
   PP := PFileRec(Files^.At(0))^.Owner;
   for I := 1 to Files^.Count - 1 do
   begin
     BB := BB or (PFileRec(Files^.At(I))^.Owner <> PP);
     if BB then Break;
   end;
   if BB then S.Options := S.Options or cmlPathNames
         else S.Options := S.Options and not cmlPathNames;
 end;
 if (ExecResource(dlgMakeList, S) <> cmOK) then Exit;
 MakeListFileOptions := S.Options;
 while S.Action[Length(S.Action)] = ' ' do Dec(S.Action[0]);
 while S.Header[Length(S.Header)] = ' ' do Dec(S.Header[0]);
 while S.Footer[Length(S.Footer)] = ' ' do Dec(S.Footer[0]);
 if S.Action <> '' then S.Action := S.Action + ' '; FileMode := 2;
 Abort := Off;
 if S.FileName[1] in ['+', '%', '/'] then
   begin
Retry:
     FidoMode := True;
     SS := FMSetup.DIZ;
     I := Pos('/FIDO=', SS);
     if I = 0 then
       begin
          SS:='';
AddrError:
          If ExecResource(dlgFTNInfo, SS)=cmCancel then exit;
          if PosChar(',',SS)>0 then
          I := Pos('/FIDO=', FMSetup.DIZ);
          if I>0 then begin
           For J:=I to 255 do if FMSetup.DIZ[J]=';' then break;
           Delete(FMSetup.DIZ, I, J-I+1);
          end;
          if FMSetup.DIZ[Length(FMSetup.DIZ)]<>';' then
           FMSetup.DIZ:=FMSetup.DIZ+';';
          FMSetup.DIZ:=FMSetup.DIZ+'/FIDO='+SS;
          Message(Application, evCommand, cmUpdateConfig, nil);
          Goto Retry;
       end;
     Delete(SS, 1, I+5);
     I := PosChar(';', SS); if I = 0 then I := Length(SS)+1; SS[0] := Char(I-1);
     I := PosChar(',', SS); if I = 0 then Goto AddrError;
     ParseAddress(Copy(SS, 1, I-1), ZZ, NN, ND, PT); K := ZZ;
     ParseAddress(Copy(S.FileName, 2, 255), ZZ, NN, ND, PT);
     Delete(SS, 1, I); if SS[Length(SS)] = '\' then Dec(SS[0]);
     lFSplit(SS, Dr, Nm, Xt);
     if K <> ZZ then Xt := '.'+Copy(Hex4(ZZ), 2, 3);
     SS := Dr+Nm+Xt;
     if PT = 0 then Nm := Hex4(NN)+Hex4(ND)
        else begin
               SS := MakeNormName(SS , Hex4(NN) + Hex4(ND) + '.PNT\');
               Nm := Hex8(PT);
             end;
     Xt := '.hlo';
     case S.FileName[1] of
       '+': Xt[2] := 'c';
       '%': Xt[2] := 'f';
     end;
     SS := MakeNormName(SS, Nm + Xt);
     S.FileName := SS;
     S.Options := S.Options or cmlPathNames;
   end else FidoMode:=False;
 D := lFExpand(S.FileName); lFSplit(D, Dr, Nm, Xt); ClrIO;
 FLD := Dr;
 CreateDirInheritance(Dr, Off);
 if Abort then Exit;
 lAssignText(T, D); ClrIO;
 lResetText(T);
 if IOResult = 0 then
   begin
     Close(T.T);
     PP := @SS; SS := Cut(D, 40);
     if FidoMode then I := cmOK
                 else I := MessageBox(GetString(dlED_OverQuery)
                                      , @PP, mfYesButton+mfCancelButton+mfAppendButton+mfWarning);
     case I of
       cmOK: lAppendText(T);
       cmYes: lRewriteText(T);
         else Exit;
     end;
   end else lRewriteText(T);
 if Abort then begin Close(T.T); Exit; end;
 if IOResult<>0 then
  begin
   MessageBox(GetString(dlFBBNoOpen)+Cut(S.FileName, 40), nil, mfError + mfOKButton);
   Exit;
  end;
 if (S.Header<>'') and not FidoMode then begin
     if (S.HeaderMode=hfmInsertText) or
     ((S.HeaderMode=hfmAuto) and not SomeFilesExist(S.Header))
     then WriteLn(T.T,S.Header) else begin
         CurPos:=1;
         while GetNextName(S.Header,SS) do begin
             lAssignText(HF,SS); ClrIO;
             lResetText(HF);
             if (IOResult<>0) and (S.HeaderMode=hfmInsertFiles) then begin
                 MessageBox(GetString(dlFBBNoOpen)+Cut(SS,40),
                 nil, mfError + mfOKButton);
             end;
             while (IOResult=0) and not EOF(HF.T) do begin
                 ReadLn(HF.T,SS);
                 WriteLn(T.T,SS);
             end;
             Close(HF.T)
         end;
         ClrIO;
     end;
 end;
 Message(Desktop, evBroadcast, cmGetUserParams, @UPr);
 for I := 1 to Files^.Count do
  begin
    P := Files^.At(I-1);
    UPr.Active:=P;
    Message(APP, evCommand, cmCopyUnselect, P);
    BB := False;
    SR := P^.Owner^;
    Replace('!', #0, SR);
    if SR[Byte(Sr[0])] <> '\' then SR := SR +'\';
    SS := S.Action;
    if SS <> '' then
      begin
        Replace(';;', #1, SS);
        while SS <> '' do
          begin
            J := PosChar(';', SS);
            if J = 0 then J := Length(SS)+1;
            MakeStr(fReplace(#1, ';', Copy(SS, 1, J-1)));
            Delete(SS, 1, J);
          end;
      end else
    If (S.Options and cmlPathNames <> 0) or (S.Options and 2 <> 0) and (SR <> FLD)
      then MakeStr( '!:!\!.!' )
      else MakeStr('!.!');
  end;
 if (S.Footer<>'') and not FidoMode then begin
     if (S.FooterMode=hfmInsertText) or
     ((S.FooterMode=hfmAuto) and not SomeFilesExist(S.Footer))
     then WriteLn(T.T,S.Footer) else begin
         CurPos:=1;
         while GetNextName(S.Footer,SS) do begin
             lAssignText(HF,SS); ClrIO;
             lResetText(HF);
             if (IOResult<>0) and (S.FooterMode=hfmInsertFiles) then begin
                 MessageBox(GetString(dlFBBNoOpen)+cut(SS,40),
                 nil, mfError + mfOKButton);
             end;
             while (IOResult=0) and not EOF(HF.T) do begin
                 ReadLn(HF.T,SS);
                 WriteLn(T.T,SS);
             end;
             Close(HF.T)
         end;
         ClrIO;
     end;
 end;
 Close(T.T);
 RereadDirectory(Dr);
 GlobalMessage(evCommand, cmRereadInfo, nil);
 GlobalMessage(evCommand, cmRereadTree, @Dr);
end;
        {-DataCompBoy-}

end.
