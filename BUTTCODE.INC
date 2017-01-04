{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.05/DOS
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

const

    {                       BUTTONS CODE'S TABLE

                              bc - button code                             }

    bcNoKey = 0;

    {Standart}
    bcTab=9;            bcBackspace=8;  bcCtrlEnter=10;
    bcSpace=32;         bcEnter=13;     bcEsc=27;
    bcCtrlBackspace=127; bcCtrlBackSlash=28;
    bcCtrlA = 1;     bcCtrlB = 2;    bcCtrlC = 3;    bcCtrlD = 4;
    bcCtrlE = 5;     bcCtrlF = 6;    bcCtrlG = 7;    bcCtrlH = 8;
    bcCtrlI = 9;     bcCtrlJ = 10;   bcCtrlK = 11;   bcCtrlL = 12;
    bcCtrlM = 13;    bcCtrlN = 14;   bcCtrlO = 15;   bcCtrlP = 16;
    bcCtrlQ = 17;    bcCtrlR = 18;   bcCtrlS = 19;   bcCtrlT = 20;
    bcCtrlU = 21;    bcCtrlV = 22;   bcCtrlW = 23;   bcCtrlX = 24;
    bcCtrlY = 25;    bcCtrlZ = 26;

    {Extended F1..F10}
    {Normal              Shift                   Ctrl                    Alt}
    bcF1=59 shl 8;       bcShiftF1=84 shl 8;     bcCtrlF1=94 shl 8;      bcAltF1=104 shl 8;
    bcF2=60 shl 8;       bcShiftF2=85 shl 8;     bcCtrlF2=95 shl 8;      bcAltF2=105 shl 8;
    bcF3=61 shl 8;       bcShiftF3=86 shl 8;     bcCtrlF3=96 shl 8;      bcAltF3=106 shl 8;
    bcF4=62 shl 8;       bcShiftF4=87 shl 8;     bcCtrlF4=97 shl 8;      bcAltF4=107 shl 8;
    bcF5=63 shl 8;       bcShiftF5=88 shl 8;     bcCtrlF5=98 shl 8;      bcAltF5=108 shl 8;
    bcF6=64 shl 8;       bcShiftF6=89 shl 8;     bcCtrlF6=99 shl 8;      bcAltF6=109 shl 8;
    bcF7=65 shl 8;       bcShiftF7=90 shl 8;     bcCtrlF7=100 shl 8;     bcAltF7=110 shl 8;
    bcF8=66 shl 8;       bcShiftF8=91 shl 8;     bcCtrlF8=101 shl 8;     bcAltF8=111 shl 8;
    bcF9=67 shl 8;       bcShiftF9=92 shl 8;     bcCtrlF9=102 shl 8;     bcAltF9=112 shl 8;
    bcF10=68 shl 8;      bcShiftF10=93 shl 8;    bcCtrlF10=103 shl 8;    bcAltF10=113 shl 8;

    {Alt+A..Z}
    bcAltA=30 shl 8;     bcAltH=35 shl 8;        bcAltO=24 shl 8;        bcAltV=47 shl 8;
    bcAltB=48 shl 8;     bcAltI=23 shl 8;        bcAltP=25 shl 8;        bcAltW=17 shl 8;
    bcAltC=46 shl 8;     bcAltJ=36 shl 8;        bcAltQ=16 shl 8;        bcAltX=45 shl 8;
    bcAltD=32 shl 8;     bcAltK=37 shl 8;        bcAltR=19 shl 8;        bcAltY=21 shl 8;
    bcAltE=18 shl 8;     bcAltL=38 shl 8;        bcAltS=31 shl 8;        bcAltZ=44 shl 8;
    bcAltF=33 shl 8;     bcAltM=50 shl 8;        bcAltT=20 shl 8;
    bcAltG=34 shl 8;     bcAltN=49 shl 8;        bcAltU=22 shl 8;
    {Alt+1..0,-=}
    bcAlt1=120 shl 8;    bcAlt4=123 shl 8;       bcAlt7=126 shl 8;       bcAlt0=129 shl 8;
    bcAlt2=121 shl 8;    bcAlt5=124 shl 8;       bcAlt8=127 shl 8;       bcAltMinus=130 shl 8;
    bcAlt3=122 shl 8;    bcAlt6=125 shl 8;       bcAlt9=128 shl 8;       bcAltEq=131 shl 8;
    {*}
    bcIns=82 shl 8;      bcHome=71 shl 8;        bcPgUp=73 shl 8;
    bcDel=83 shl 8;      bcEnd=79 shl 8;         bcPgDn=81 shl 8;
    {**}
    bcLeft=75 shl 8;     bcUp=72 shl 8;          bcDown=80 shl 8;        bcRight=77 shl 8;
    {Ctrl+ *}
    bcCtrlHome=119 shl 8; bcCtrlPgUp=132 shl 8;  bcCtrlEnd=117 shl 8;    bcCtrlPgDn=118 shl 8;
    {Ctrl+ **}
    bcCtrlLeft=115 shl 8; bcCtrlRight=116 shl 8;
    {other}
    bcCtrl2=3 shl 8;     bcShiftTab=15 shl 8;    bcCtrlPrnScr=114 shl 8;
