{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.07/DOS
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
{$I ZCONF.INC}

program DN;

{$M 40000,0,655350}
{$DEFINE DN}
{.$DEFINE GRABPalette}
uses ExtKBD,   Drivers,  LFN,      LFNCol,   DN1,      Dos,      DnApp,
{$IFNDEF DPMI}
{$IFDEF PackOverlay}     DnOverl,  {$ELSE}   Overlay,  {$ENDIF}  Overlays,
{$ENDIF}{DPMI}
     Menus,    FlPanelX, FlPanel,  FileCopy, Filediz,  Filelst,  Eraser,
     DiskInfo, Advance,  Advance1, Advance2, Advance3, Advance4, highlite,
     Startup,  Dialogs,  Gauges,   Memory,   DblWnd,   Messages, HistList,
     FileFind, Commands, Tree,     FViewer,  CmdLine,  FBB,      DNStdDlg,
     MemInfo,  FilesCol, UserMenu, RStrings, Colors,   Microed,  Editor,
     Macro,    DNFormat, Format,   Fixer,    ArcView,  HelpFile, Validate,
     ASCIITab, xTime,    Drives,   Archiver, Archset,  Archdet,  Setups,
     DNUtil,   XDblWnd,  Histries, CCalc,    DnIni,    DrvTypes, Collect,
     UFNMatch, Objects,  Views,    Scroller, Calculator,
     HelpKernel,         Reanimator,         extramemory
{$IFDEF FPC}  ,CallSpec  {$ELSE}  ,Callspcb            {$ENDIF}
{$IFDEF CdPlayer}       ,CdPlayer ,CdUtil              {$ENDIF}
{$IFDEF SpreadSheet}    ,Calc     ,CellsCol            {$ENDIF}
{$IFDEF DBView}         ,DBView   ,DBWatch             {$ENDIF}
{$IFDEF Modem}          ,ModemIO  ,Terminal
                        ,apTimer  ,apUART    ,apPort
                   {$IFDEF LINK}  ,NavyLink  {$ENDIF}
                   {$IFDEF PHONES},uDialer {$ENDIF}    {$ENDIF}
{$IFDEF Game}           ,Tetris                        {$ENDIF}
{$IFDEF PrintManager}   ,PrintManager                  {$ENDIF}
{$IFDEF Calendar}      ,Calendar                       {$ENDIF}{JO}
{$IFDEF PHONES}        ,Phones                         {$ENDIF}
{$IFDEF SS}            ,Idlers                         {$ENDIF}
{$IFDEF CHCOL}         ,ColorSel, ColorVGA             {$ENDIF}
{$IFDEF NETINFO}       ,Novell                         {$ENDIF}
      ;

{$IFNDEF DPMI}

  {!$O DNApp} {!$O Drivers} {!$O Overlays} {!$O Startup}
  {$O DN1}

{$IFDEF FULLOVR}
  {$O RStrings} {$O UniWin} {$O FlPanelX} {$O FlPanel} {$O XDBlWnd}
  {$O FStorage} {$O Menus}  {$O Drives}   {$O DblWnd}  {$O CmdLine}
  {$O Gauge}    {$O Gauges} {$O CallSpcB}
 {$IFNDEF PackOverlay}{$O BStrings}{$ENDIF}
{$ENDIF}

{$IFDEF FULLOVR2}
  {$O UFNMATCH} {$O XTIME}  {$O Collect}
{$ENDIF}

  {$O Profile}  {$O U_SRCHF}  {$O HistList} {$O Histries} {$O FlTools}
  {$O Scroller} {$O DiskTool} {$O HideView} {$O Setups}   {$O FViewer}
  {$O Macro}    {$O MicroEd}  {$O FileCopy} {$O FileDiz}  {$O FileLst}
  {$O Decoder}  {$O DiskInfo} {$O Eraser}   {$O FileFind} {$O Dialogs}
  {$O DNUtil}   {$O DNFormat} {$O Format}   {$O CCalc}    {$O ArcView}
  {$O Archiver} {$O Archset}  {$O Archdet}  {$O Arc_Zip}  {$O Arc_LHA}
  {$O Arc_RAR}  {$O Arc_ACE}  {$O Arc_HA}   {$O Arc_CAB}
{$IFNDEF MINARCH}
  {$O Arc_arc}  {$O Arc_bsa}  {$O Arc_bs2}  {$O Arc_hyp}  {$O Arc_lim}
  {$O Arc_hpk}  {$O Arc_TAR}  {$O Arc_ZXZ}  {$O Arc_QRK}  {$O Arc_UFA}
  {$O Arc_IS3}  {$O Arc_SQZ}  {$O Arc_HAP}  {$O Arc_ZOO}  {$O Arc_CHZ}
  {$O Arc_UC2}  {$O Arc_AIN}  {$O Arc_TGZ}  {$O Archread}
{$IFNDEF PackOverlay}
  {$O GZIO}     {$O ZUtil}    {$O ZInflate} {$O ZLib}
  {$O ADler}    {$O InfBlock} {$O InfCodes} {$O InfFast}
  {$O InfTrees} {$O InfUtil}  {$IFNDEF NO_DEFLATE}{$O Trees}{$ENDIF}
{$ENDIF !PackOverlay}
{$ENDIF !MINARCH}
  {$O ASCIITab} {$O DNStdDlg} {$O SBlocks}  {$O FBB}      {$O Tree}
  {$O Messages} {$O Colors}   {$O UserMenu} {$O HelpFile} {$O Validate}
  {$O Arvid}    {$O ArvidAvt} {$O ArvidTdr} {$O Advance6} {$O WinClp}
  {$O DiskImg}  {$O DRVTYPES} {$O DNIni}    {$O FindObj}  {$O DNEXEC}
  {$O advance4} {$O Editor}   {$O Ed2}      {$O EdWin}    {$O RegAll}
  {$O U_KeyMap} {$O advance7} {$O HelpKernel}             {$O Reanimator}
  {$O calculator}

{$IFDEF ADVANCEOVR}{$O advance1} {$O advance2} {$O advance3}
                   {$O highlite}                              {$ENDIF}
{$IFDEF SYSINFO}   {$O SysInfo}  {$O CpuType}                 {$ENDIF}
{$IFDEF MEMINFO}   {$O MemInfo}                               {$ENDIF}
{$IFDEF Game}      {$O Tetris}                                {$ENDIF}
{$IFDEF PrintManager}            {$O PrintManager}            {$ENDIF}
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
{$IFDEF Calendar}  {$O Calendar}                              {$ENDIF} {JO}
{$IFDEF UUENCODE}  {$O UUCode}  {$ELSE}
{$IFDEF UUDECODE}  {$O UUCode}  {$ENDIF}                      {$ENDIF}
{$IFDEF NETINFO}   {$O NETINFO}                               {$ENDIF}
{$IFDEF PHONES}    {$O PHONES}                                {$ENDIF}
{$IFDEF SS}        {$O Idlers}                                {$ENDIF}
{$IFDEF CHCOL}     {$O ColorSel} {$O ColorVGA}                {$ENDIF}
{$IFDEF NETINFO}   {$O Novell}                                {$ENDIF}

{$IFDEF MODEM} {$O StrView}{$ELSE}{$IFDEF PHONES}{$O StrView} {$ENDIF}{$ENDIF}

{$ENDIF}{DPMI}

BEGIN
 Init09Handler;
 RUN_IT;
END.
