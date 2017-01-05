unit CopyIni;
{AK155}

interface

procedure CopyIniVarsToCfgVars;

implementation
uses
  DnIni, Startup, Advance1, FlPanelX
  , fnotify
  {$IFDEF Win32}
  , VpKbdW32 {for AltGreyAsAlt}
  {$ENDIF}
  , Commands
  ;

procedure CopyIniVarsToCfgVars;
  var
    i: Integer;
    hcd: Word;
  begin
  SystemData.Options := SystemDataOpt;
  InterfaceData.Options := InterfaceDataOpt;
  InterfaceData.DrvInfType := DriveInfoType;
  Startup.FMSetup.Options := FMSetupOpt;
  EditorDefaults.EdOpt := EditorDefaultsOpt;
  EditorDefaults.EdOpt2 := EditorDefaultsOpt2;
  EditorDefaults.ViOpt := ViewerOpt;
  StartupData.Load := StartupDataLoad;
  StartupData.Unload := StartupDataUnload;
  StartupData.Slice2 := StartupDataSlice2;
  Confirms := ConfirmsOpt;
  SystemData.CopyLimitBuf := CopyLimit;
  SystemData.ForceDefArch := ForceDefaultArchiver;
  for i := 0 to 8 do
    DisposeStr(DirsToChange[i]);
  DirsToChange[0] := NewStr(QDirs1);
  DirsToChange[1] := NewStr(QDirs2);
  DirsToChange[2] := NewStr(QDirs3);
  DirsToChange[3] := NewStr(QDirs4);
  DirsToChange[4] := NewStr(QDirs5);
  DirsToChange[5] := NewStr(QDirs6);
  DirsToChange[6] := NewStr(QDirs7);
  DirsToChange[7] := NewStr(QDirs8);
  DirsToChange[8] := NewStr(QDirs9);
  hcd := ossHandleChDirCommand shr 3;
  with SystemData do
    if HandleChDirCommand then
      Options := Options or hcd
    else
      Options := Options and not hcd;
    {$IFDEF Win32}
    AltGreyAsAlt := AltGrAsAlt; {JO}
    {$ENDIF}
  if DnIni.AutoRefreshPanels then
    NotifyInit; {JO}
  end { CopyIniVarsToCfgVars };

end.
