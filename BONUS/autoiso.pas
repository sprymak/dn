program Autoiso;

 uses Dos, VPSysLow;
 var F: Text;
     S, TempDir, Par, Opt, PathToUse: String;
     Dev: String[5];
     I, IOR: Byte;
     J,K,L: Longint;

 procedure ClrIO;
  begin
   InOutRes := 0;
   DosError := 0;
  end;

begin

  if Paramstr(1) = '' then
    begin
      Writeln;
      Writeln('AtoISO - multisession ISO creation with MKISOFS automatizer');
      Writeln('(C) Jaroslaw Osadtchiy, 2002');
      Writeln;
      Writeln('usage: AtoISO [/t:TempDir] <dev=x,x,x> [options] <PathToUse>');
      Writeln('where:');
      Writeln('TempDir - path to temporary directory (TEMP environment by default)');
      Writeln('PathToUse - directory branch or file to be placed to ISO image');
      Writeln('options - any options for MKISOFS');
      Writeln('dev=x,x,x - your CD-RW device');
      Writeln('(for more detailed information see CDRECORD and MKISOFS documentation)');
      Exit;
    end;
  TempDir := '';
  Dev := '';
  PathToUse := '';
  Opt := '';

  for I := 1 to 9 do
   begin
    Par := Paramstr(I);
    if Par <> '' then
      begin
        if (Copy(Par, 1, 3) = '-t:') or
           (Copy(Par, 1, 3) = '-T:') or
           (Copy(Par, 1, 3) = '/t:') or
           (Copy(Par, 1, 3) = '/T:') then
             begin TempDir := Copy(Par, 4, 255); Continue; end;
        if Copy(Par, 1, 4) = 'dev=' then begin Dev := Copy(Par, 5, 5); Continue; end;
        if (Par[1] = '-') and (Par[3] <> ':') and
           (Par <> '-o') and (Par <> '-C') and (Par <> '-M') then
              begin Opt := Opt + ' ' + Par; Continue; end;
        if (Par[1] <> '-') and (Par[2] = ':') then PathToUse := Par;
      end;
   end;

  if Dev = '' then begin Writeln('No device set'); Exit; end;
  if PathToUse = '' then begin Writeln('No source path'); Exit; end;
  if TempDir = '' then TempDir := GetEnv('TEMP');
  if TempDir = '' then TempDir := GetEnv('TMP');
  if TempDir = '' then begin Writeln('No temporary directory set'); Exit; end;
  if Tempdir[Length(Tempdir)] <> '\' then Tempdir := Tempdir + '\';
  if Opt <> '' then Opt := Opt + ' ';

  Exec(GetEnv('COMSPEC'),'/c cdrecord.exe -msinfo dev=' + Dev + ' > ' + Tempdir + 'sesnum.tmp');

  Writeln;

  S := '';

  Assign(F, Tempdir + 'sesnum.tmp'); ClrIO;
  Reset(F);
  IOR := IOResult;
  if IOR <> 0 then Writeln('File open error ', IOR);
  Read(F, S);
  IOR := IOResult;
  if IOR <> 0 then Writeln('File read ', IOR);
  Close(F);
  Erase(F);
  ClrIO;

  I := Pos(',', S);
  if I = 0 then begin Writeln('No last session parameters revealed'); Exit; end;
  Val(Copy(S, 1, I-1), K, J); if J <> 0 then begin Writeln('Wrong last session parameters revealed'); Exit; end;
  Val(Copy(S, I+1, 255), L, J); if J <> 0 then begin Writeln('Wrong last session parameters revealed'); Exit; end;
  if L < K then begin Writeln('Wrong last session values revealed'); Exit; end;

  if (K = 0) and (L = 0) then
    begin
      Writeln('No previous sessions revealed, creating first session image');
      Writeln;
      Exec('mkisofs.exe', '-o ' + Tempdir + 'session.iso ' + Opt + PathToUse);
    end
   else
    begin
      Writeln('Last session numbers are: '+S+ '; creating next session image');
      Writeln;
      Exec('mkisofs.exe', '-o ' + Tempdir + 'session.iso ' + Opt + '-C ' + S + ' -M '+ Dev + ' ' + PathToUse);
    end;
end.
