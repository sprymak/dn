(*ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÛ
//Û                                                       Û
//Û      Dos Navigator/2 Runtime Dymanic Link Library     Û
//Û      OS/2 Presentation Manager API interface          Û
//Û      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÛ
//Û      by Jaroslaw Osadtchiy (JO), 2:5030/1082.53       Û
//Û                                                       Û
//ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*)

{$X+,T-,Cdecl+,AlignRec-,OrgName+,V-,Use32-,Delphi+}

library DnPmApil;

{$Linker
DESCRIPTION 'Dos Navigator/2 OS/2 PM API interface'}

uses Os2Def, OS2PMApi;

function WinQueryObjectSh(pszObjectID: PChar): lHandle;
 begin
  WinQueryObjectSh := WinQueryObject(pszObjectID);
 end;

function WinOpenObjectSh(hObject: lHandle; ulView: ULong; Flag: Boolean): Boolean;
 begin
  WinOpenObjectSh := WinOpenObject(hObject, ulView, Flag);
 end;

function WinCreateObjectSh(pszClassName: PChar; pszTitle: PChar;
  pszSetupString: PChar; pszLocation: PChar): lHandle;
 begin
  WinCreateObjectSh := WinCreateObject(pszClassName, pszTitle,
               pszSetupString, pszLocation, 0);
 end;

exports
  WinQueryObjectSh name 'WinQueryObjectSh',
  WinOpenObjectSh  name 'WinOpenObjectSh',
  WinCreateObjectSh name 'WinCreateObjectSh';


initialization
end.
