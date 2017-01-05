(*ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÛ
//Û                                                       Û
//Û          Dos Navigator/2 runtime library              Û
//Û      OS/2 Presentation Manager API interface          Û
//Û      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÛ
//Û      by Jaroslaw Osadtchiy (JO), 2:5030/1082.53       Û
//Û                                                       Û
//ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*)

{$X+,T-,Cdecl+,AlignRec-,OrgName+,V-,Use32-,Delphi+}

unit Dn2PmApi;

interface

uses Os2Def, Os2Base;

function DN_WinQueryObject(pszObjectID: PChar): lHandle;
function DN_WinOpenObject(hObject: lHandle; ulView: ULong; Flag: Boolean): Boolean;
function DN_WinCreateObject(pszClassName: PChar; pszTitle: PChar; pszSetupString: PChar; pszLocation: PChar): lHandle;

implementation

uses Commands, Advance, DNApp, Strings, Messages;

type
  TWinQueryObject = function(pszObjectID: PChar): lHandle;
  TWinOpenObject  = function(hObject: lHandle; ulView: ULong; Flag: Boolean): Boolean;
  TWinCreateObject = function(pszClassName: PChar; pszTitle: PChar; pszSetupString: PChar; pszLocation: PChar): lHandle;

function DN_WinQueryObject;
 var hMod: hModule;
       RC: Longint;
 SzFailName : Array[0..cchMaxPath] of Char;
       P1:TWinQueryObject;
       PS: PChar;
    PSArr: array[0..255] of Char;

 begin
    PS := PSArr;
    PS := StrPCopy (PS, StartupDir+'dnpmapil.dll');
   RC := DosLoadModule(szFailName,  {failed module name}
     sizeof(szFailName),            {size of buffer}
     PS,                            {name of DLL}
     hmod);                         {module handle here}
   if RC <> 0 then begin messagebox(GetString(dlCantLoad)+StrPas(@szFailName)+ ', RC = %d', @RC, mfError+mfOKButton);
                         DN_WinQueryObject := 0; Exit;
                   end;
   RC := DosQueryProcAddr(
     hmod,                          {DLL module handle}
     0,                             {function ordinal value}
     'WinQueryObjectSh',            {function name}
     @P1);                          {address of function pointer}

    if RC = 0 then DN_WinQueryObject := P1(pszObjectID)
      else begin messagebox('RC = %d', @RC, mfOKButton); DN_WinQueryObject := 0; end;
   DosFreeModule(hmod);
 end;

function DN_WinOpenObject;
 var hMod: hModule;
       RC: Longint;
  SzFailName : Array[0..cchMaxPath] of Char;
   P1: TWinOpenObject;
       PS: PChar;
    PSArr: array[0..255] of Char;
 begin
    PS := PSArr;
    PS := StrPCopy (PS, StartupDir+'dnpmapil.dll');
   RC := DosLoadModule(szFailName,  {failed module name}
     sizeof(szFailName),            {size of buffer}
     PS,                            {name of DLL}
     hmod);                         {module handle here}
   if RC <> 0 then begin messagebox(GetString(dlCantLoad)+StrPas(@szFailName)+ ', RC = %d', @RC, mfError+mfOKButton);
                         DN_WinOpenObject := False; Exit;
                   end;
   RC := DosQueryProcAddr(
     hmod,                          {DLL module handle}
     0,                             {function ordinal value}
     'WinOpenObjectSh',             {function name}
     @P1);                          {address of function pointer}
   if RC = 0 then DN_WinOpenObject := P1(hObject, ulView, Flag)
     else begin messagebox('RC = %d', @RC, mfOKButton); DN_WinOpenObject := False; end;
   DosFreeModule(hmod);
 end;

function DN_WinCreateObject;
 var hMod: hModule;
       RC: Longint;
  SzFailName : Array[0..cchMaxPath] of Char;
   P1: TWinCreateObject;
       PS: PChar;
    PSArr: array[0..255] of Char;
 begin
    PS := PSArr;
    PS := StrPCopy (PS, StartupDir+'dnpmapil.dll');
   RC := DosLoadModule(szFailName,  {failed module name}
     sizeof(szFailName),            {size of buffer}
     PS,                            {name of DLL}
     hmod);                         {module handle here}
   if RC <> 0 then begin messagebox(GetString(dlCantLoad)+StrPas(@szFailName)+ ', RC = %d', @RC, mfError+mfOKButton);
                         DN_WinCreateObject := 0; Exit;
                   end;
   RC := DosQueryProcAddr(
     hmod,                          {DLL module handle}
     0,                             {function ordinal value}
     'WinCreateObjectSh',           {function name}
     @P1);                          {address of function pointer}
   if RC = 0 then DN_WinCreateObject := P1(pszClassName, pszTitle, pszSetupString, pszLocation)
     else begin messagebox('RC = %d', @RC, mfOKButton); DN_WinCreateObject := 0; end;
   DosFreeModule(hmod);
 end;


begin
end.
