(*ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÛ
//Û                                                       Û
//Û      Dos Navigator/2 Runtime Dymanic Link Library     Û
//Û      OS/2 Presentation Manager API interface          Û
//Û      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÛ
//Û      by Jaroslaw Osadtchiy (JO), 2:5030/1082.53       Û
//Û      modified by Aleksej Kozlov (Cat), 2:5030/1326.13 Û
//Û      modified by Alexey Korop (AK155), 2:461/155      Û
//Û                                                       Û
//ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*)

{$X+,T-,Cdecl+,AlignRec-,OrgName+,V-,Use32-,Delphi+}
{$I STDEFINE.INC}

library DnPmApil;

{$Linker
DESCRIPTION 'Dos Navigator/2 OS/2 PM API interface'}

uses
  Os2Def, Os2Base, OS2PMApi, Strings;

{$IFDEF PLUGIN}
{$DYNAMIC DN2CAT.LIB}
{$ENDIF}

const
  PMStatus: Byte = 0; {Cat}
  { 0 = Not checked yet                          }
  { 1 = Full Screen without Presentation Manager }
  { 2 = Full Screen with Presentation Manager    }
  { 3 = Windowed                                 }

var
  Tib: PTib;
  Pib: PPib;
  Anchor: HAB;
  Queue: HMQ;
  hSw: LHandle;
  SwData: SwCntrl;

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

function WinSetTitleAndIconSh(szTitle, szIconPath: PChar): Integer;
begin
  WinSetTitleAndIconSh := WinSetTitleAndIcon(szTitle, szIconPath);
end;

function IsBGWindow: Boolean; {AK155}
var
  ActiveWin: ULong;  // System Information Data Buffer
begin
  IsBGWindow := false;
  if PMStatus = 3 {¢ ®ª®èª¥} then
    begin
      DosQuerySysInfo(25, 25, ActiveWin, sizeof(ActiveWin));
      if ActiveWin <> SwData.idProcess then
        IsBGWindow := true;
    end;
end;

procedure XSetTitle(Title: String);
var
  I: Byte;
begin
  if hSw <> 0 then
    begin
      with SwData do
        begin
          if Length(Title) > MaxNameL then
            begin
              Move(Title[1], szSwTitle[0], MaxNameL);
              szSwTitle[MaxNameL] := #0;
            end
          else
            begin
              Move(Title[1], szSwTitle[0], Length(Title));
              szSwTitle[Length(Title)] := #0;
            end;
          WinSetWindowText(hWnd, @szSwTitle[0]);
        end;
      WinChangeSwitchEntry(hSw, @SwData);
    end;
end;

function XClipCanPaste: Boolean;
var
  Fmt: ULong;
begin
  Result := WinQueryClipBrdFmtInfo(WinInitialize(0), cf_Text, Fmt);
end;

function XClipCopy(P: PChar; Size: Longint): Boolean;
var
  Q: PChar;
begin
  Result := False;
  if WinOpenClipBrd(Anchor) then
    begin
      // Allocate giveable block of memory
      DosAllocSharedMem(Pointer(Q), nil, Size+1, pag_Write+pag_Commit+obj_Giveable);
      if Q <> nil then
        begin
          // Copy clipboard data across
          Move(P^, Q^, Size);
          Q[Size]:=#0;
          // Insert data into clipboard
          Result := WinSetClipBrdData(Anchor, ULong(Q), cf_Text, cfi_Pointer);
        end;
      WinCloseClipBrd(Anchor);
    end;
end;

function XClipPaste(var Size: LongInt): Pointer;
var
  P: PChar;
  Flags: Longint;
  l: longint;
begin
  Result := nil;
  Size := 0;
  if WinOpenClipBrd(Anchor) then
    begin
      P := PChar(WinQueryClipBrdData(Anchor, cf_Text));
      if P <> nil then
       begin
        l := StrLen(P) + 1;
        if (DosQueryMem(P, l, Flags) = 0) and (Flags and 1 <> 0) then
          begin
            if DosAllocMem(Result, l, pag_Write or pag_Commit) = 0 then
              begin
              Size := l; Move(P^, Result^, Size);
              end;
          end;
       end;
      WinCloseClipBrd(Anchor);
    end;
end;

function XCheckPM: Byte;
begin
  XCheckPM := PMStatus;
end;

{&Cdecl-}
function CheckPM: Boolean; {Cat}
{
label
  L;
}
const
  ModuleInfoSize = 400000;
  PMSHELL = 'PMSHELL.EXE'#0;
{ PMSHELL: array[0..Length('PMSHELL.EXE')-1] of char = 'PMSHELL.EXE'; }
var
  FailedModule: array[0..255] of char;
  LibHandle: HModule;
  Dos32QuerySysState: function(func,arg1,pid,_res_:ulong;buf:pointer;bufsz:ulong):apiret cdecl;
  ModuleInfo: PChar;
  SearchPos, I: LongInt;
begin
  CheckPM := False;
  if DosLoadModule(FailedModule, SizeOf(FailedModule), 'DOSCALLS', LibHandle) = 0 then
    if DosQueryProcAddr(LibHandle, 368, nil, @Dos32QuerySysState)=0 then
      begin
        GetMem(ModuleInfo, ModuleInfoSize);
        FillChar(ModuleInfo^,ModuleInfoSize, 0);
        Dos32QuerySysState(
          $00000004,          // module data
          0,                  // reserved
          0,                  // all processes
          0,                  // reserved
          ModuleInfo,
          ModuleInfoSize);

        // search PMSHELL.EXE
        SearchPos:=0;
        while SearchPos+Length(PMSHELL)<ModuleInfoSize do
          if StrComp(@ModuleInfo[SearchPos], PMSHELL) = 0 then
            begin
              CheckPM := True;
              Break;
            end
          else
            Inc(SearchPos);
(*
        SearchPos:=0;
        while SearchPos+Length(PMSHELL)<ModuleInfoSize do
          for I := 1 to Length(PMSHELL) do
            begin
              if ModuleInfo[SearchPos+I-1] <> PMSHELL[I] then
                begin
                  Inc(SearchPos);
                  Break;
                end;
              CheckPM := True;
              goto L;
            end;
        L:
*)

        FreeMem(ModuleInfo, ModuleInfoSize);
        DosFreeModule(LibHandle);
      end;
end;

procedure Done;
begin
  if Queue <> 0 then
    WinDestroyMsgQueue(Queue);
  if Anchor <> 0 then
    WinTerminate(Anchor);
end;
{&Cdecl+}

exports
  WinQueryObjectSh name 'WinQueryObjectSh',
  WinOpenObjectSh  name 'WinOpenObjectSh',
  WinCreateObjectSh name 'WinCreateObjectSh',
  WinSetTitleAndIconSh name 'WinSetTitleAndIconSh',
  XSetTitle name 'XSetTitle',
  XClipCopy name'XClipCopy',
  XClipPaste name 'XClipPaste',
  XClipCanPaste name 'XClipCanPaste',
  XCheckPM name 'XCheckPM',
  IsBGWindow name 'IsBGWindow'; {AK155}

initialization {Cat}
  if PMStatus = 0 {¥éñ ­¥ ¯à®¢¥àï«¨} then
    begin
      FillChar(SwData, SizeOf(SwData), 0);
      AddExitProc(Done);
      if DosGetInfoBlocks(Tib, Pib) = 0 then
        with Pib^ do
          begin
            {¯à®¢¥àï¥¬ ­ «¨ç¨¥ Presentation Manager}
            if Pib_ulType = 2 then
              PMStatus := 3 {¢ ®ª®èª¥}
            else
              if CheckPM then
                PMStatus := 2 {FS ¯®¤ PM}
              else
                PMStatus := 1; {FS ¡¥§ PM}

            {¯à¨â¢®àï¥¬áï ¯à¨«®¦¥­¨¥¬ ¤«ï Presentation Manager}
            if PMStatus >= 2 then
              begin
                Pib_ulType := 3;
                Anchor := WinInitialize(0);
                if Anchor <> 0 then
                  Queue := WinCreateMsgQueue(Anchor, 0);
                hSw := WinQuerySwitchHandle(0, Pib_ulPid);

                {AK155: ¯®«ãç¨âì SwData ¤®áâ â®ç­® ®¤¨­ à § }
                if hSw <> 0 then
                  WinQuerySwitchEntry(hSw, @SwData);
              end
          end
      else
        PMStatus := 1;
    end;
end.
