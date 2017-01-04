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
//  dn16rc1-move_COMBINE_MIXED_from_sources_to_Options_menu_diff140byMV.patch
//
//  2.0.0
//  dn31029-Compile_by_VP.patch
//  dn31214-DBFView(f)-view_dbf_works_in_VP.patch
//  dn31214-PhoneBook(f)-phone_book_works_in_VP.patch
//  dn31214-Calendar(f)-calendar_works_in_VP.patch
//  dn31214-WKZ(f)-wkz_works_in_VP.patch
//  dn31214-StdDefine(f)-another_modules_works_in_VP.patch
//  dn31220-UUE(f)-uueCode_uueEncode_works_in_VP.patch
//  dn31220-Game(f)-Tetris_works_in_VP.patch
//  dn31220-built-in_result_variable.patch
//  dn40307-define_DN_Micro.patch
//
//  4.9.0
//
/////////////////////////////////////////////////////////////////////////}

{/$DEFINE LFNCache}
  {$IFDEF VER80}{$DEFINE DELPHI}{$DEFINE RESULT}{$ENDIF}
  {$IFDEF VIRTUALPASCAL}{$DEFINE NONBP}{$DEFINE RESULT}{$ENDIF}
  {$IFDEF FPC}{$DEFINE NONBP}{$ENDIF}

{$DEFINE LongCopy}
{$DEFINE DiskImg}
{$DEFINE ARVID}
{$DEFINE DBView}
{$DEFINE PHONES}
{$DEFINE Calendar}
{$DEFINE SpreadSheet}
{$DEFINE TrashCan}
{$DEFINE COLUMNSMENU}
{$DEFINE UUENCODE}
{$DEFINE UUDECODE}
{$DEFINE Game}
  {$IFNDEF NONBP}{TEMPORARY COMPILE LITE VERSION FOR NON BP}
{$DEFINE Modem} {For enabling modem features you must have ASyncPro}
{$DEFINE Link}
{$DEFINE DiskEdit}
{$DEFINE DiskFormat}
{$DEFINE Reanimator}
{$DEFINE PrintManager}
{$DEFINE Game}
{$DEFINE CdPlayer}
{$DEFINE SYSINFO}
{$DEFINE MEMINFO}
{$DEFINE NETINFO}
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
 {$DEFINE QSort}{.$DEFINE POGLSORT}
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

  {$ELSE NONBP}
{$DEFINE NO_OVERLAY}
{$DEFINE NOEXTRA}
{$DEFINE BIT_32}
{$DEFINE Win32}
{.$DEFINE MINARCH}
{$Use32+}
{$DEFINE POGLSORT}
{$DEFINE COLUMNSMENU}
  {$ENDIF NONBP}

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
  {$UNDEF Reanimator}
  {$UNDEF DBView}
  {$UNDEF DisAsm}
{$ENDIF}

{.$DEFINE MICRO}

{$IFDEF MICRO}
  {$DEFINE MINARCH}

  {$UNDEF LongCopy}
  {$UNDEF SYSINFO}
  {$UNDEF MEMINFO}
  {$UNDEF DiskImg}
  {$UNDEF ARVID}
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
  {$UNDEF Modem}
  {$UNDEF DiskEdit}
  {$UNDEF Reanimator}
  {$UNDEF COLUMNSMENU}
  {$UNDEF SORTDEBUG}
  {$UNDEF CHDIRDEBUG}
  {$UNDEF DiskFormat}

{$ENDIF}{MICRO}


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

{$IFDEF VIRTUALPASCAL}
 {$DEFINE USESYSUTILS}   {Cat: если хочется подключать модуль SysUtils или}
                         {использовать механизм исключений; в плагинной версии ни на что не влияет}

{$ENDIF}
