{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  1.6.RC1
//  dn230-remove_GZip_compression_and_change_external_filter_view.patch
//
//  2.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
{$I ZCONF.INC}

program DN;

{$IFNDEF NONBP}
{$M 40000,0,655350}
{$ENDIF}

{.$DEFINE GRABPalette}
uses DN1;

{$IFNDEF NO_OVERLAY}

  {!$O DNApp} {!$O Drivers} {!$O Overlays} {!$O Startup}
  {$O DN1}    {$O DnSvLd}   {$O DnUtil2}

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
  {$O DiskInfo} {$O Eraser}   {$O FileFind} {$O Dialogs}  {!$O DNUtil}
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
  {$O ASCIITab} {$O DNStdDlg} {$O SBlocks}  {$O FBB}      {$O Tree}
  {$O Messages} {$O Colors}   {$O UserMenu} {$O HelpFile} {$O Validate}
  {$O Arvid}    {$O ArvidAvt} {$O ArvidTdr} {$O Advance6} {$O WinClp}
  {$O DiskImg}  {$O DRVTYPES} {$O DNIni}    {$O FindObj}  {$O DNEXEC}
  {$O advance4} {$O RegAll}   {$O advance7}
  {$O calculat} {$O HelpKern}

{$IFDEF DiskFormat}           {$O DNFormat} {$O Format}   {$ENDIF}
{$IFDEF MicroedOvr}{$O Editor}   {$O MicroEd}  {!$O U_KeyMap}
                   {$O Ed2}      {$O EdWin}    {$O MicroEd2}  {$ENDIF}
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

BEGIN
 RUN_IT;
END.
