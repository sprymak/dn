unit CopyIni;
{AK155}

interface

procedure CopyIniVarsToCfgVars;

implementation
uses
  dnini, startup, advance1, flpanelx
  {$IFDEF FileNotify} , fnotify {$ENDIF}
  {$IFDEF Win32}, VpSysLow {for RecodeAnsiNames} {$ENDIF}
  , Commands
 ;

procedure CopyIniVarsToCfgVars;
  var
    i: integer;
    hcd: word;
  begin
  SystemData.Options        := SystemDataOpt;
  InterfaceData.Options     := InterfaceDataOpt;
  InterfaceData.DrvInfType  := DriveInfoType;
  Startup.FMSetup.Options   := FMSetupOpt;
  EditorDefaults.EdOpt      := EditorDefaultsOpt;
  EditorDefaults.EdOpt2     := EditorDefaultsOpt2;
  EditorDefaults.ViOpt      := ViewerOpt;
  StartupData.Load          := StartupDataLoad;
  StartupData.Unload        := StartupDataUnload;
  StartupData.Slice2        := StartupDataSlice2;
  Confirms                  := ConfirmsOpt;
  SystemData.CopyLimitBuf   := CopyLimit;
  SystemData.ForceDefArch   := ForceDefaultArchiver;
  for I := 0 to 8 do DisposeStr(DirsToChange[I]);
  DirsToChange[0]           := NewStr(QDirs1);
  DirsToChange[1]           := NewStr(QDirs2);
  DirsToChange[2]           := NewStr(QDirs3);
  DirsToChange[3]           := NewStr(QDirs4);
  DirsToChange[4]           := NewStr(QDirs5);
  DirsToChange[5]           := NewStr(QDirs6);
  DirsToChange[6]           := NewStr(QDirs7);
  DirsToChange[7]           := NewStr(QDirs8);
  DirsToChange[8]           := NewStr(QDirs9);
  hcd := ossHandleChDirCommand shr 3;
  with SystemData do
    if HandleChDirCommand then
      Options := Options or hcd
    else
      Options := Options and not hcd;
{$IFDEF Win32}
  RecodeAnsiNames := (RecodeCyrillicNames = 1); {AK155}
{$IFNDEF RecodeWhenDraw}
  if RecodeCyrillicNames = 0 then
    Windows.SetFileApisToOEM
       {AK155 18-10-2001
        Если не задано никаких фокусов, то с файлами работаем просто
        в кодировке OEM, пусть система перекодирует, если ей надо.
       }
  else
{$ENDIF}
    Windows.SetFileApisToANSI;
{$ENDIF}
{$IFDEF FileNotify}
   if DNIni.AutoRefreshPanels then NotifyInit; {JO}
{$ENDIF}
  end;

end.
