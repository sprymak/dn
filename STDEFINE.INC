{/$DEFINE LFNCache}
  {$IFDEF VIRTUALPASCAL}{$DEFINE NONBP}{$DEFINE LINEPOSIT}{$ENDIF}
  {$IFDEF FPC}{$DEFINE NONBP}{$ENDIF}

 {$IFNDEF VIRTUALPASCAL} {$IFNDEF DPMI} {$DEFINE REALMODE} {$ENDIF} {$ENDIF}
 {$IFNDEF REALMODE} {.$DEFINE TGZ} {$ENDIF}

{$DEFINE Calendar}
{$DEFINE Game}
{$DEFINE TrashCan}
{$DEFINE CHCOL}
{$DEFINE SS}

{.$DEFINE DN}
{.$DEFINE DNPRG}

{$IFDEF REALMODE}
 {$DEFINE LITE}
{$ENDIF}

{$DEFINE UUENCODE}
{$DEFINE UUDECODE}
{$DEFINE DBView}
  {$IFNDEF NONBP}{TEMPORARY COMPILE LITE VERSION FOR NON BP}
{$DEFINE UUENCODE}
{$DEFINE UUDECODE}
{$DEFINE TARDETECT}
{.$DEFINE Modem}
{.$DEFINE Link}
{$DEFINE DiskEdit}
{$DEFINE DiskFormat}
{$DEFINE Reanimator}
{$DEFINE DBView}
{$DEFINE PrintManager}
{$DEFINE SpreadSheet}
{$DEFINE CdPlayer}
{$DEFINE MEMINFO}
{$DEFINE SYSINFO}
{$DEFINE NETINFO}
{.$DEFINE PHONES}

{.$DEFINE DisAsm}
{.$DEFINE UPLOWSTRSINDLG}

{$IFDEF DPMI}
 {$DEFINE BIGCOLLECTION}
 {.$DEFINE QSort}{$DEFINE POGLSORT}
 {$DEFINE NO_OVERLAY}
 {$DEFINE NOEXTRA}
 {$DEFINE PKTVIEW}
 {.$DEFINE OldCollection}
{$ELSE}
 {.$DEFINE BIGCOLLECTION}
 {.$DEFINE QSort}{$DEFINE POGLSORT}
 {$UNDEF NO_OVERLAY}
 {$UNDEF NOEXTRA}
 {$DEFINE OFFS}
 {$DEFINE OldCollection} {старые ассемблерные методы *Collection}
{$ENDIF}

{$DEFINE BIT_16}
{$DEFINE OS_DOS}

{.$DEFINE StreamDebug}
{.$DEFINE SORTDEBUG}
{.$DEFINE CHDIRDEBUG}
{.$DEFINE CHDIRTIMER}
{$DEFINE UserSaver}

  {$ELSE VIRTUALPASCAL}
{$DEFINE NO_OVERLAY}
{$DEFINE NOEXTRA}
{$DEFINE BIT_32}
{$DEFINE FILE_32}
{.$DEFINE Win32}     {не требуется, так как включено в _dnc.bat}
{$IFDEF Win32}
 {$DEFINE STRINGLFN} {включает более быстрый, но жрущий много памяти алгоритм работы с виндовыми LFN}
{$ENDIF}
{.$DEFINE OS2}       {не требуется, так как включено в _dnc.cmd}
{$DEFINE VPBETA}    {отключает те фишки, которые пока не работают в VP-версии}
                     {и включает создание DN.ERR при критических ошибках}
{.$DEFINE OldCollection} {старые ассемблерные методы *Collection}
{$DEFINE NoCallspcb} {внимание! конфиги версий, скомпилённых с и без NoCallspcb несовместимы!}
{$IFDEF OS2}
 {$DEFINE EAOP}      {операции с расширенными атрибутами}
 {$DEFINE USEWPS}    {открытие фолдеров WPS}
{$ENDIF}
{$Use32+}
{.$DEFINE QSORT}
{$DEFINE POGLSORT}
{$DEFINE SpreadSheet}
{.$DEFINE PrintManager}
{.$DEFINE Printer}
{$DEFINE PrintFromEditor}
{.$DEFINE PKTVIEW} {нормально компилится и работает, но надо ли}
{$DEFINE DisAsm}  {нормально компилится и работает, но надо ли}
{$DEFINE PHONES}
{.$DEFINE NETINFO}
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

{$IFDEF LITE}
 {.$UNDEF TrashCan}
 {$UNDEF Game}
 {.$UNDEF UUENCODE}
 {.$UNDEF UUDECODE}
 {.$UNDEF Calendar}
 {.$UNDEF CdPlayer}
 {$UNDEF NETINFO}
 {.$UNDEF PHONES}
 {.$UNDEF SS}
 {.$UNDEF PrintManager}
 {.$UNDEF CHCOL}
 {$UNDEF SpreadSheet}
 {.$UNDEF DBView}
 {.$UNDEF Link}
 {$UNDEF DisAsm}
 {.$DEFINE MINARCH}
 {$UNDEF DiskEdit}
 {$UNDEF Reanimator}
{$ENDIF}

{.$DEFINE BANK}

{$IFDEF BANK}
  {$UNDEF Modem}
  {$UNDEF DiskEdit}
  {$UNDEF DBView}
  {$UNDEF DisAsm}
{$ENDIF}

{$DEFINE COMBINE_MIXED} {это чтобы case filter работал по-человечески}

{$DEFINE FULLOVR}
{$DEFINE FULLOVR2}
{$DEFINE VIEWSOVR}
{$DEFINE ADVANCEOVR}
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
