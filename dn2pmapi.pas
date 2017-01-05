(*€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€
//€                                                       €
//€          Dos Navigator/2 runtime library              €
//€      OS/2 Presentation Manager API interface          €
//€      ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ€
//€      by Jaroslaw Osadtchiy (JO), 2:5030/1082.53       €
//€      modified by Aleksej Kozlov (Cat), 2:5030/1326.13 €
//€      modified by Alexey Korop (AK155), 2:461/155      €
//€                                                       €
//ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
*)

{$X+,T-,Cdecl+,AlignRec-,OrgName+,V-,Use32-,Delphi+}

unit Dn2PmApi;

interface

uses
  Os2Def, Os2Base;

var
  DN_WinQueryObject : function(pszObjectID: PChar): lHandle;
  DN_WinOpenObject : function(hObject: lHandle; ulView: ULong; Flag: Boolean): Boolean;
  DN_WinCreateObject : function(pszClassName: PChar; pszTitle: PChar; pszSetupString: PChar; pszLocation: PChar): lHandle;
  DN_WinSetTitleAndIcon : function(szTitle, szIconPath: PChar): Integer;
  DN_IsBGWindow: function: boolean;
  DN_XSetTitle : procedure(Title: String);
  DN_XClipCopy : function(P: PChar; Size: LongInt): Boolean;
  DN_XClipPaste : function(var Size: LongInt): Pointer;
  DN_XClipCanPaste : function: Boolean;
  DN_WinSwitchToProgram : function(Pid: LongInt): Longint;
  DN_WinQueryTaskTitle : function(PSessId: LongInt; var Title: String): Longint;

implementation

uses Commands, Advance, DNApp, Strings, Messages, VPSysLow;

var
  hMod: hModule;
  Initialized: Boolean;

Function Fake_WinQueryObject(pszObjectID: PChar): lHandle;
begin
  Result := 0;
end;

Function Fake_WinOpenObject(hObject: lHandle; ulView: ULong; Flag: Boolean): Boolean;
begin
  Result := False;
  MessageBox(GetString(dlCantLoad)+'DNPMAPIL.DLL', nil, mfError+mfOKButton);
end;

Function Fake_WinCreateObject(pszClassName: PChar; pszTitle: PChar; pszSetupString: PChar; pszLocation: PChar): lHandle;
begin
  Result := 0;
  MessageBox(GetString(dlCantLoad)+'DNPMAPIL.DLL', nil, mfError+mfOKButton);
end;

Function Fake_WinSetTitleAndIcon(szTitle, szIconPath: PChar): Integer;
begin
  Result := 0;
end;

function Fake_IsBGWindow: boolean;
begin
  Result := False;
end;

Procedure Fake_XSetTitle(Title: String);
begin
end;

Function Fake_XClipCopy(P: PChar; Size: LongInt): Boolean;
begin
  Result := False;
end;

Function Fake_XClipPaste(var Size: LongInt): Pointer;
begin
  Size := 0;
  Result := nil;
end;

Function Fake_XClipCanPaste: Boolean;
begin
  Result := False;
end;

Function Fake_WinSwitchToProgram: Longint;
begin
  Result := 1;
end;

Function Fake_WinQueryTaskTitle: Longint;
begin
  Result := 0;
end;

{&Cdecl-}
Procedure Done;
begin
  if hMod <> 0 then
    DosFreeModule(hMod);
end;
{&Cdecl+}

Procedure Init;
var
  S: String;
  DN_XCheckPM: Function: Byte;
  P: Procedure;

  procedure SetFakeProcs;
  begin
    @DN_WinQueryObject := @Fake_WinQueryObject;
    @DN_WinOpenObject := @Fake_WinOpenObject;
    @DN_WinCreateObject := @Fake_WinCreateObject;
    @DN_WinSetTitleAndIcon := @Fake_WinSetTitleAndIcon;
    @DN_IsBGWindow := @Fake_IsBGWindow;
    @DN_XSetTitle := @Fake_XSetTitle;
    @DN_XClipCopy := @Fake_XClipCopy;
    @DN_XClipPaste := @Fake_XClipPaste;
    @DN_XClipCanPaste := @Fake_XClipCanPaste;
    @DN_WinSwitchToProgram := @Fake_WinSwitchToProgram;
    @DN_WinQueryTaskTitle := @Fake_WinQueryTaskTitle;
  end;

{AK155 èÆØÎ‚™† ØÆ´Á®‚Ï †§‡•· Ø‡ÆÊ•§„‡Î, Ø‡® ≠•„§†Á•
†§‡•· ≠• ¨•≠Ô•‚·Ô. }
  procedure DN_proc(ProcName: PChar; var Proc: pointer);
  var
    P: Pointer;
  begin
    if DosQueryProcAddr(
      hMod,          {DLL module handle}
      0,             {function ordinal value}
      ProcName,      {function name}
      P              {address of function pointer}
    ) = 0 then
      Proc := P;
  end;

begin
  if Initialized then
    Exit;
  Initialized := True;

  SetFakeProcs; { ≠† ·´„Á†© ´Ó°ÎÂ ≠•„‡Ô§®Ê }
{$IFDEF DNPRG} {AK155}
  S := StartupDir+'DNPMAPIL.DLL'#0;
  if (DosLoadModule(
    nil,                        {failed module name}
    0,                          {size of buffer}
    @S[1],                      {name of DLL}
    hMod                        {module handle here}
  ) <> 0) then
    begin
      WriteLn('Cannot load module '+S);
      SetFakeProcs;
      Exit;
    end;

  if (DosQueryProcAddr(
    hMod,                       {DLL module handle}
    0,                          {function ordinal value}
    'XCheckPM',                 {function name}
    @DN_XCheckPM                {address of function pointer}
  ) <> 0) then
    begin
      WriteLn('Cannot load module '+S);
      SetFakeProcs;
      Exit;
    end;

  if DN_XCheckPM <= 1 then
    begin
      WriteLn('Cannot find Presentation Manager');
      SetFakeProcs;
      Exit;
    end;
  PMWindowed := (DN_XCheckPM = 3);
  if PMWindowed then
    NoMouseMove := true;

  DN_proc('WinQueryObjectSh', @DN_WinQueryObject);
  DN_proc('WinOpenObjectSh', @DN_WinOpenObject);
  DN_proc('WinCreateObjectSh', @DN_WinCreateObject);
  DN_proc('WinSetTitleAndIconSh', @DN_WinSetTitleAndIcon);
  DN_proc('IsBGWindow', @DN_IsBGWindow);
  DN_proc('XSetTitle', @DN_XSetTitle);
  DN_proc('XClipCopy', @DN_XClipCopy);
  DN_proc('XClipPaste', @DN_XClipPaste);
  DN_proc('XClipCanPaste', @DN_XClipCanPaste);
  DN_proc('WinSwitchToProgramSh', @DN_WinSwitchToProgram);
  DN_proc('WinQueryTaskTitleSh', @DN_WinQueryTaskTitle);

  S := StartupDir+'dn_pm.ico'#0;
  DN_WinSetTitleAndIcon('DN/2', @S[1]);
  {§•©·‚¢„•‚ ‚Æ´Ï™Æ •·´® Ø‡Æ£‡†¨¨† ß†Ø„·™†•‚·Ô ™Æ¨†≠§Æ© "start dn.exe"
   ®´® ¢ VIO Æ™≠• ®ß Ø‡Æ£‡†¨¨≠Æ£Æ Æ°Í•™‚†,
   •·´® ß†Ø„·™†‚Ï ¢ Full Screen ®ß Ø‡Æ£‡†¨¨≠Æ£Æ Æ°Í•™‚† - ≠• §•©·‚¢„•‚}
{$ENDIF}
end;

begin
  Init;
end.
