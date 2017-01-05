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
{.$I ZCONF.INC}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

program DN;
{$IFNDEF DPMI32}
{$R DN.res}
{$ENDIF}

{$IFNDEF NONBP}
{$M 40000,0,655350}
{$ENDIF}

{Cat: методы поддержки плагинов поменялись, поэтому этот кусок
      закомментировал; прежний код убивать пока жалко}

uses
    {$IFNDEF VIRTUALPASCAL} ExtKBD, {$ENDIF}
     Drivers,  LFN,      Files,
     DN1,      Dos,      DnApp,
    {$IFNDEF NO_OVERLAY} Overlay1, Overlays, {$ENDIF}
     Menus,    FlPanelX, FlPanel,  FileCopy, Filediz,  Filelst,  Eraser,
     DiskInfo, Advance,  Advance1, Advance2, Advance3, Advance4, highlite,
     Startup,  Dialogs,  Gauges,   Memory,   DblWnd,   Messages, HistList,
     FileFind, Commands, Tree,     FViewer,  CmdLine,  FBB,      DNStdDlg,
     FilesCol, UserMenu, RStrings, Colors,   Microed,  Editor,   Macro,
     ArcView,  HelpFile, Validate, ASCIITab, xTime,    Drives,   Archiver,
     Archset,  Archdet,  Setups,   DNUtil,   XDblWnd,  Histries, CCalc,
     DnIni,    Collect,  UFNMatch, Objects,  Views,    Scroller, Calculat,
     HelpKern, VideoMan
{$IFNDEF VIRTUALPASCAL} ,DrvTypes ,ExtraMem            {$ENDIF}
{$IFDEF DiskFormat}     ,DNFormat ,Format              {$ENDIF}
{$IFDEF CdPlayer}       ,CdPlayer ,CdUtil              {$ENDIF}
{$IFDEF SpreadSheet}    ,Calc     ,CellsCol            {$ENDIF}
{$IFDEF DBView}         ,DBView   ,DBWatch             {$ENDIF}
{$IFDEF Modem}          ,ModemIO  ,Terminal
                        ,apTimer  ,apUART    ,apPort
                   {$IFDEF LINK}  ,NavyLink  {$ENDIF}
                   {$IFDEF PHONES},uDialer {$ENDIF}    {$ENDIF}
{$IFDEF Game}           ,Tetris                        {$ENDIF}
{$IFDEF Printer}        ,PrintMan                      {$ENDIF}
{$IFDEF Calendar}      ,Calendar                       {$ENDIF}{JO}
{$IFDEF PHONES}        ,Phones                         {$ENDIF}
{$IFDEF SS}            ,Idlers                         {$ENDIF}
{$IFDEF CHCOL}         ,ColorSel, ColorVGA             {$ENDIF}
{$IFDEF NETINFO}       ,Novell                         {$ENDIF}
{$IFDEF DiskEdit}      ,Fixer                          {$ENDIF}
{$IFDEF REANIMATOR}    ,Reanimat                       {$ENDIF}
{$IFDEF LINEPOSIT} {$IFDEF USESYSUTILS} ,SysUtils {$ENDIF} ,VPUtils {$ENDIF}
{$IFDEF EAOP}      {$IFDEF OS2} ,EAOper                    {$ENDIF} {$ENDIF}
{$IFDEF VIRTUALPASCAL} ,VPSysLow                       {$ENDIF}
     ;


{$IFDEF LINEPOSIT}

{&Delphi+}
{$IFDEF LOGOBJLOADSTORE}
var
  F: Text;
{$ENDIF}
{$IFDEF USESYSUTILS}
var
  E: Exception;
  FileName: ShortString;
  LineNo: LongInt;
{$IFDEF VPBETA}
  DNErrFile: Text;
{$ENDIF}
{$ELSE}
procedure DebugInfoExitProc;
var
  S: String;
begin
  if ErrorAddr <> nil then
    begin
      SourceFileName := @S;
      GetLocationInfo(ErrorAddr, S, SourceLineNo);
      {EndFatalError;} {in Drivers.ExitDrivers}
    end;
end;
{$ENDIF}

{$ENDIF}

begin
{$IFDEF VIRTUALPASCAL}
SysDisableHardErrors;

{CtrlBreakHandler := TVCtrlBreak;
SysCtrlSetCBreakHandler;}
{$ENDIF}
{$IFDEF LINEPOSIT}
{$IFNDEF USESYSUTILS}
 AddExitProc(DebugInfoExitProc);
{$ELSE}
 try
{$ENDIF}
  {Init09Handler;}
{Cat: проверяем на выходе, сколько памяти скушалось
      создаём лог, куда будут записываться все вызовы методов Load и Store}
   begin
{$IFDEF LOGOBJLOADSTORE}
  if IOResult <> 0 then
    ;
  System.Assign(F, 'stream.log');
  System.Rewrite(F);
  System.Close(F);
  if IOResult <> 0 then
    ;
{$ENDIF}
     RUN_IT;
   end
{/Cat}
{$IFDEF USESYSUTILS}
 except
{Cat: закомментировал - всё равно не работает
   on E: EControlC do
     begin
     ClearScreen;
     end;
}
   on E: exception do
     begin
     ClearScreen;
     SysTvShowBuf(0, ScreenWidth*ScreenHeight);
 {$IFDEF VPBETA}
      SourceDir := SourceDir + 'DN.ERR';
      Writeln('Fatal Error'^M^J'-----------'^M^J^M^J+
              'Exception 0',Hex2(ExitCode),'h at address ',Hex8(LongInt(ExceptAddr)));
      Writeln(E.Message);
      if GetLocationInfo( ExceptAddr, FileName, LineNo ) <> nil then
          Writeln('Source location: ' + FileName + ' line ', LineNo );
      Writeln('Please report to RU.SHELL.DN'^M^J+
              '( file ' + SourceDir + ' )'^M^J^M^J+
              'Press any key...');
  if IOResult <> 0 then
    ;
       Assign(DNErrFile, SourceDir);
       rewrite(DNErrFile);
       Writeln(DNErrFile,'');
       Writeln(DNErrFile, 'Exception at addr ' + Ptr2Hex(ExceptAddr));
       Writeln(DNErrFile, E.Message);
       if GetLocationInfo( ExceptAddr, FileName, LineNo ) <> nil then
         Writeln(DNErrFile, 'Source location: ' + FileName + ' line ', LineNo );
       Close(DNErrFile);
 {$ELSE}
       Writeln('Exception at addr ' + Ptr2Hex(ExceptAddr));
       Writeln(E.Message);
       if GetLocationInfo( ExceptAddr, FileName, LineNo ) <> nil then
          Writeln('Source location: ' + FileName + ' line ', LineNo );
 {$ENDIF}
     repeat
      SysCtrlSleep(1); {JO}
     until SysKeyPressed;
     while SysKeyPressed do SysReadKey;
     SysTVInitCursor;
     end;
 end;
{$ENDIF}
{$ELSE}
{$IFNDEF VIRTUALPASCAL}
 Init09Handler;
{$ENDIF}
 RUN_IT;
{$ENDIF}

(*{$ENDIF PLUGIN}*)

end.
