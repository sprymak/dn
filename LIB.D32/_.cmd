@ECHO OFF

SET VPBASE=D:\VP20
SET HOST=OS2

REM � delete old copy
DEL OS2.LIB
DEL IMPORT32.LIB

REM � assemble dummy obj
C:\BP\BIN\TASM OS2.ASM /q /oi /m
C:\BP\BIN\TASM IMPORT32.ASM /q /oi /m

REM � create lib
%VPBASE%\BIN.%HOST%\LIB OS2.LIB +OS2.OBJ
%VPBASE%\BIN.%HOST%\LIB IMPORT32.LIB +IMPORT32.OBJ

REM � remove obj
DEL OS2.OBJ
DEL IMPORT32.OBJ
