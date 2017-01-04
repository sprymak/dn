{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.09
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

{/$DEFINE LFNCache}
  {$IFDEF VIRTUALPASCAL}{$DEFINE NONBP}{$ENDIF}
  {$IFDEF FPC}{$DEFINE NONBP}{$ENDIF}


  {$IFNDEF NONBP}{TEMPORARY COMPILE LITE VERSION FOR NON BP}
{$DEFINE UUENCODE}
{$DEFINE UUDECODE}
{.$DEFINE Modem}
{$DEFINE Link}
{$DEFINE SpreadSheet}
{$DEFINE DiskEdit}
{$DEFINE DiskFormat}
{$DEFINE Reanimator}
{$DEFINE DBView}
{$DEFINE PrintManager}
{$DEFINE Game}
{$DEFINE CdPlayer}
{$DEFINE Calendar}
{$DEFINE SYSINFO}
{$DEFINE MEMINFO}
{$DEFINE TrashCan}
{$DEFINE NETINFO}
{$DEFINE PHONES}
{$DEFINE SS}
{$DEFINE CHCOL}
{.$DEFINE DisAsm}
{.$DEFINE PKTVIEW}
{.$DEFINE UPLOWSTRSINDLG}

{$IFDEF DPMI}
 {$DEFINE BIGCOLLECTION}
 {.$DEFINE QSort}{$DEFINE POGLSORT}
 {$DEFINE NO_OVERLAY}
 {$DEFINE NOEXTRA}
{$ELSE}
 {.$DEFINE BIGCOLLECTION}
 {.$DEFINE QSort}{$DEFINE POGLSORT}
 {$UNDEF NO_OVERLAY}
 {$UNDEF NOEXTRA}
 {$DEFINE OFFS}
{$ENDIF}

{$DEFINE BIT_16}
{$DEFINE OS_DOS}

{.$DEFINE StreamDebug}
{$DEFINE SORTDEBUG}
{$DEFINE CHDIRDEBUG}
{.$DEFINE CHDIRTIMER}

  {$ELSE VIRTUALPASCAL}
{$DEFINE NO_OVERLAY}
{$DEFINE NOEXTRA}
{$DEFINE BIT_32}
{$DEFINE FILE_32}
{$DEFINE Win32}
{$DEFINE MINARCH}
{$Use32+}
{$DEFINE POGLSORT}
  {$ENDIF VIRTUALPASCAL}

{$IFDEF BPIDE}
{$UNDEF CHCOL}
{$UNDEF SYSINFO}
{$UNDEF MEMINFO}
{$UNDEF UUENCODE}
{$UNDEF UUDECODE}
{$UNDEF Modem}
{$UNDEF Link}
{$UNDEF DisAsm}
{$ENDIF}

{.$DEFINE LITE}

{$IFDEF LITE}
 {$UNDEF TrashCan}
 {$UNDEF Game}
 {$UNDEF UUENCODE}
 {$UNDEF UUDECODE}
 {$UNDEF Calendar}
 {$UNDEF CdPlayer}
 {$UNDEF NETINFO}
 {$UNDEF PHONES}
 {$UNDEF SS}
 {$UNDEF PrintManager}
 {$UNDEF CHCOL}
 {$UNDEF SpreadSheet}
 {$UNDEF DBView}
 {$UNDEF Link}
 {$UNDEF DisAsm}
 {$DEFINE MINARCH}
{$ENDIF}

{.$DEFINE BANK}

{$IFDEF BANK}
  {$UNDEF Modem}
  {$UNDEF DiskEdit}
  {$UNDEF DBView}
  {$UNDEF DisAsm}
{$ENDIF}

{.$DEFINE COMBINE_MIXED}

{$DEFINE FULLOVR}
{$DEFINE FULLOVR2}
{$DEFINE VIEWSOVR}
{.$DEFINE ADVANCEOVR}
{$DEFINE CollectOVR}
{$DEFINE MicroedOvr}
{$DEFINE VIDEOOVR}

{ Only TPW uses "WObjects"; BP 7 calls the same unit "Objects" }
{$IFDEF windows}
  {$IFNDEF ver70}
    {$DEFINE wobjects}
  {$ENDIF}
{$ENDIF}

{$IFDEF Modem}
  {$IFDEF DN}
    {$DEFINE CL_MASTER}
  {$ENDIF}
  {$IFDEF NLCLIENT}
    {$DEFINE CL_MASTER}
  {$ENDIF}
{$ENDIF}
