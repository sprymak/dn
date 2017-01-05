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
{$IFDEF OS2}
{$R DN.res}
{$ENDIF}

{$IFNDEF NONBP}
{$M 40000,0,655350}
{$ENDIF}

{Cat: методы поддержки плагинов поменялись, поэтому этот кусок
      закомментировал; прежний код убивать пока жалко}

(*
{Cat: версия с поддержкой плагинов:
      TRUE_DYNAMIC (для OS/2): весь ДН лежит в DLL-ке, которую и подключаем
      через директиву $DYNAMIC. Стараемся использовать в Uses только то, что
      реально нужно, тогда уменьшаются размер EXE-шника и количество глюков.
      не TRUE_DYNAMIC (для Win32): весь ДН лежит в DLL-ке, которую подключаем
      "по старинке", т.е. импортируем оттуда только функции DN1.Run_It и
      DN1.Error. Таким образом, свои копии модуля System есть и у DN.EXE,
      и у DN2CAT.DLL. В Win32 тоже можно использовать TRUE_DYNAMIC, но тогда
      при возникновении исключений ДН подвисает, поэтому для Win32 способ с
      TRUE_DYNAMIC используется только в случае выключенной VPBETA - надеюсь,
      в релизе исключений возникать не должно.
}
{$IFDEF PLUGIN}
  {$IFDEF TRUE_DYNAMIC}
  uses
    DN1;

  {$DYNAMIC DN2CAT.LIB}

  {$ELSE}
  procedure Run_It;
    external 'dn2cat' name 'DN1@RUN_IT';
  procedure Error(const SourceFileName: String; SourceLineNo, Addr, Code: LongInt);
    external 'dn2cat' name 'DN1@ERROR';
  {$ENDIF}

  {$IFDEF LINEPOSIT}
  procedure DebugInfoExitProc;
  var
    S: String;
    N: LongInt;
  begin
    GetLocationInfo(ErrorAddr, S, N);
    Error(S, N, LongInt(ErrorAddr), ExitCode);
  end;
  {$ENDIF}

  begin
    {$IFDEF LINEPOSIT}
    AddExitProc(DebugInfoExitProc);
    {$ENDIF}
    Run_It;
  {end.}

{$ELSE PLUGIN}
*)
uses
    {$IFNDEF VIRTUALPASCAL} ExtKBD, {$ENDIF}
     Drivers,  LFN,     {$IFNDEF OS2} LFNCol, {$ENDIF}
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
     HelpKern
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

{$IFNDEF NO_OVERLAY}

  {!$O DNApp} {!$O Drivers} {!$O Overlays} {!$O Startup}
  {$O DN1}

{$IFDEF FULLOVR}
  {$O RStrings} {$O UniWin} {$O FlPanelX} {$O FlPanel} {$O XDBlWnd}
  {$O FStorage} {$O Menus}  {$O Drives}   {$O DblWnd}  {$O CmdLine}
  {$O Gauge}    {$O Gauges} {$O CallSpcB}
{$ENDIF}

{$IFDEF FULLOVR2}
  {$O UFNMATCH} {$O XTIME}
{$ENDIF}

  {$O Profile}  {$O U_SRCHF}  {$O HistList} {$O Histries} {$O FlTools}
  {$O Scroller} {$O DiskTool} {$O HideView} {$O Setups}   {$O FViewer}
  {$O Macro}    {$O FileCopy} {$O FileDiz}  {$O FileLst}  {$O CCalc}
  {$O DiskInfo} {$O Eraser}   {$O FileFind} {$O Dialogs}  {$O DNUtil}
  {$O ArcView}  {$O Archiver} {$O Archset}  {$O Archdet}  {$O Arc_Zip}
  {$O Arc_LHA}  {$O Arc_RAR}  {$O Arc_ACE}  {$O Arc_HA}   {$O Arc_CAB}
{$IFNDEF MINARCH}
  {$O Arc_arc}  {$O Arc_bsa}  {$O Arc_bs2}  {$O Arc_hyp}  {$O Arc_lim}
  {$O Arc_hpk}  {$O Arc_TAR}  {$O Arc_ZXZ}  {$O Arc_QRK}  {$O Arc_UFA}
  {$O Arc_IS3}  {$O Arc_SQZ}  {$O Arc_HAP}  {$O Arc_ZOO}  {$O Arc_CHZ}
  {$O Arc_UC2}  {$O Arc_AIN}  {$O Arc_TGZ}  {$O Archread}
(* we do NOT need GZip compression inside
  {$O GZIO}     {$O ZUtil}    {$O ZInflate} {$O ZLib}
  {$O ADler}    {$O InfBlock} {$O InfCodes} {$O InfFast}
  {$O InfTrees} {$O InfUtil}  {$IFNDEF NO_DEFLATE}{$O Trees}{$ENDIF}
*)
{$ENDIF !MINARCH}
  {$O ASCIITab} {$O DNStdDlg} (*{$O SBlocks}*)  {$O FBB}      {$O Tree}
  {$O Messages} {$O Colors}   {$O UserMenu} {$O HelpFile} {$O Validate}
{$IFDEF ARVID}
  {$O Arvid}    {$O ArvidAvt} {$O ArvidTdr}
{$ENDIF}
  {$O Advance6} {$O WinClp}   {$O DiskImg}  {$O DNIni}
  {$O FindObj}  {$O DNExec}   {$O advance4} {$O RegAll}
  {$O Advance7} {$O Calculat} {$O HelpKern} {$O UserSavr}

{$IFNDEF VIRTUALPASCAL} {$O DRVTYPES} {$ENDIF}

{$IFDEF DiskFormat}           {$O DNFormat} {$O Format}   {$ENDIF}
{$IFDEF MicroedOvr}{$O Editor}   {$O MicroEd}  {!$O U_KeyMap}
                   {$O Ed2}      {$O EdWin}    {$O MicroEd2}  {$ENDIF}
{$IFDEF ADVANCEOVR}{.$O advance1} {.$O advance2} {.$O advance3}
                   {$O highlite} {$O FilesCol} {$O LFNCol}    {$ENDIF}
{$IFDEF SYSINFO}   {$O SysInfo}  {$O CpuType}                 {$ENDIF}
{$IFDEF MEMINFO}   {$O MemInfo}                               {$ENDIF}
{$IFDEF Game}      {$O Tetris}                                {$ENDIF}
{$IFDEF Printer}                 {$O PrintMan}                {$ENDIF}
{$IFDEF DiskEdit}  {$O Fixer}    {$O SWE}                     {$ENDIF}
{$IFDEF CDPLAYER}  {$O CdPlayer} {$O CdUtil}                  {$ENDIF}
{$IFDEF DBView}    {$O DBView}   {$O DBWatch}                 {$ENDIF}
{$IFDEF SpreadSheet}             {$O Calc}     {$O CellsCol}  {$ENDIF}
{$IFDEF Modem}     {$O ApFossil} {$O ApInt14}  {$O ApMisc}
                   {$O ModemIO}  {$O Terminal} {$O ApTimer}
                   {$O ScrollBk} {$O OOCOM}    {$O OOAbsPcl}
                   {$O OOXmodem} {$O OOYmodem} {$O OOZmodem}
                   {$O OOKermit}
    {$IFDEF PHONES}{$O uDialer}  {$ENDIF}
    {$IFDEF LINK}  {$O NavyLink} {$O LinkUtil}
                   {$O ComLnk}                 {$ENDIF}       {$ENDIF}
{$IFDEF VIEWSOVR}  {$O VIEWS}                                 {$ENDIF}
{$IFDEF CollectOVR}{$O Collect}                               {$ENDIF}
{$IFDEF Calendar}  {$O Calendar}                              {$ENDIF} {JO}
{$IFDEF UUENCODE}  {$O UUCode}  {$ELSE}
{$IFDEF UUDECODE}  {$O UUCode}  {$ENDIF}                      {$ENDIF}
{$IFDEF NETINFO}   {$O NETINFO}                               {$ENDIF}
{$IFDEF PHONES}    {$O PHONES}                                {$ENDIF}
{$IFDEF SS}        {$O Idlers}                                {$ENDIF}
{$IFDEF CHCOL}     {$O ColorSel} {$O ColorVGA}                {$ENDIF}
{$IFDEF NETINFO}   {$O Novell}                                {$ENDIF}
{$IFDEF DisAsm}    {$O Decoder}                               {$ENDIF}
{$IFDEF VIDEOOVR}  {$O VideoMan}                              {$ENDIF}
{$IFDEF PKTVIEW}   {$O PKTVIEW}                               {$ENDIF}
{$IFDEF REANIMATOR}{$O Reanimat}                              {$ENDIF}

{$IFDEF MODEM} {$O StrView}{$ELSE}{$IFDEF PHONES}{$O StrView} {$ENDIF}{$ENDIF}

{$ENDIF NO_OVERLAY}


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
   on E: EControlC do
     begin
     SysTvClrScr;
     end;
   on E: exception do
     begin
 {$IFDEF VPBETA}
       ErrorAddr := ExceptAddr;
       EndFatalError;
       SourceDir := SourceDir + 'DN.ERR';
       Assign(DNErrFile, SourceDir);
       Append(DNErrFile);
       Writeln(DNErrFile,'');
      {ClrEol;}
       Writeln(DNErrFile, 'Exception at addr ' + Ptr2Hex(ExceptAddr));
      {ClrEol;}
       Writeln(DNErrFile, E.Message);
       if GetLocationInfo( ExceptAddr, FileName, LineNo ) <> nil then
         begin
          {ClrEol;}
           Writeln(DNErrFile, 'Source location: ' + FileName + ' line ', LineNo );
         end;
       Close(DNErrFile);
 {$ELSE}
       SysTVClrScr;
       ClrEol;
       Writeln('Exception at addr ' + Ptr2Hex(ExceptAddr));
       ClrEol;
       Writeln(E.Message);
       if GetLocationInfo( ExceptAddr, FileName, LineNo ) <> nil then
         begin
           ClrEol;
           Writeln('Source location: ' + FileName + ' line ', LineNo );
         end;
       repeat
       until keypressed;
 {$ENDIF}
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
