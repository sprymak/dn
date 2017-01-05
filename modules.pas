unit Modules;
(******

LoadModule & GetProcAddress
Written by Cat 2:5030/1326.13
(for use in DN/2)

******)

{$I STDEFINE.INC}
{&Delphi+}

interface

type
  PPointer = ^Pointer;

  { обёртки для вызова системных функций }
function LoadModule(ModuleName: PChar; var LibHandle: integer):
  boolean;
procedure FreeModule(LibHandle: integer);
function GetProcAddress(LibHandle: integer; ProcName: PChar; var
    ProcAddr: Pointer): boolean;

{ сначала пытается загрузить DLL из каталога ДН-а, если не удаётся, }
{ то грузит из подкаталога, имеющего то же имя, что и модуль        }
{ т.е. если передана строка PLUGIN, то делается попытка загрузки:   }
{    C:\DN\PLUGIN.DLL           }
{    C:\DN\PLUGIN\PLUGIN.DLL    }
function LoadPluginModule(const ModuleName: String; var LibHandle:
    integer): boolean;
function LoadPluginModuleAndGetProcAddress(const ModuleName: String;
    var LibHandle: integer; ProcNames: array of PChar; ProcAddrs:
    array of PPointer): boolean;

implementation

{$IFDEF OS2}
uses
  Os2Def, Os2Base,
  advance;

function LoadModule(ModuleName: PChar; var LibHandle: integer):
    boolean;
  begin
    Result := (DosLoadModule(nil, 0, ModuleName, LibHandle) = 0);
    if not Result then
      LibHandle := 0;
  end;

procedure FreeModule(LibHandle: integer);
  begin
    if LibHandle <> 0 then
      begin
        DosFreeModule(LibHandle);
        LibHandle := 0;
      end;
  end;

function GetProcAddress(LibHandle: integer; ProcName: PChar; var
    ProcAddr: Pointer): boolean;
  begin
    Result := (DosQueryProcAddr(LibHandle, 0, ProcName, ProcAddr) = 0);
    if not Result then
      ProcAddr := nil;
  end;
{$ENDIF}

{$IFDEF Win32}
uses
  Windows,
  advance;

function LoadModule(ModuleName: PChar; var LibHandle: integer):
    boolean;
  begin
    LibHandle := Windows.LoadLibrary(ModuleName);
    Result := (LibHandle > HINSTANCE_ERROR);
    if not Result then
      LibHandle := 0;
  end;

procedure FreeModule(LibHandle: Handle);
  begin
    if LibHandle <> 0 then
      begin
        Windows.FreeLibrary(LibHandle);
        LibHandle := 0;
      end;
  end;

function GetProcAddress(LibHandle: Handle; ProcName: PChar; var
    ProcAddr: Pointer): boolean;
  begin
    ProcAddr := Windows.GetProcAddress(LibHandle, ProcName);
    Result := (ProcAddr <> nil);
  end;
{$ENDIF}

function LoadPluginModule(const ModuleName: String; var LibHandle:
    integer): boolean;
  var
    s: String;
  begin
    s := SourceDir+ModuleName+'.DLL'#0;
    Result := LoadModule(@S[1], LibHandle);
    if not Result then
      begin
        s := SourceDir+ModuleName+'\'+ModuleName+'.DLL'#0;
        Result := LoadModule(@S[1], LibHandle);
      end;
  end;

function LoadPluginModuleAndGetProcAddress(const ModuleName: String;
    var LibHandle: integer; ProcNames: array of PChar; ProcAddrs:
    array of PPointer): boolean;
  var
    i: integer;
  begin
    Result := LoadPluginModule(ModuleName, LibHandle);
    if Result then
      for i := Low(ProcAddrs) to High(ProcAddrs) do
        Result := Result and GetProcAddress(LibHandle, ProcNames[i],
          ProcAddrs[i]^)
      else
      for i := Low(ProcAddrs) to High(ProcAddrs) do
        ProcAddrs[i]^:= nil;

    if not Result then
      LibHandle := 0;
  end;

end.
