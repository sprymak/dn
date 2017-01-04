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
Unit Crc;

{
  crc32.c -- compute the CRC-32 of a data stream
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}

uses
  zutil, zlib;


function crc32(crc : uLong; buf : pBytef; len : uInt) : uLong;

{  Update a running crc with the bytes buf[0..len-1] and return the updated
   crc. If buf is NULL, this function returns the required initial value
   for the crc. Pre- and post-conditioning (one's complement) is performed
   within this function so it shouldn't be done by the application.
   Usage example:

    var
      crc : uLong;
    begin
      crc := crc32(0, Z_NULL, 0);

      while (read_buffer(buffer, length) <> EOF) do
        crc := crc32(crc, buffer, length);

      if (crc <> original_crc) then error();
    end;

}

function get_crc_table : puLong;  { can be used by asm versions of crc32() }


implementation

{$IFDEF DYNAMIC_CRC_TABLE}

{local}
const
  crc_table_empty : boolean = TRUE;
{local}
var
  crc_table : array[0..256-1] of uLongf;


{
  Generate a table for a byte-wise 32-bit CRC calculation on the polynomial:
  x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x+1.

  Polynomials over GF(2) are represented in binary, one bit per coefficient,
  with the lowest powers in the most significant bit.  Then adding polynomials
  is just exclusive-or, and multiplying a polynomial by x is a right shift by
  one.  If we call the above polynomial p, and represent a byte as the
  polynomial q, also with the lowest power in the most significant bit (so the
  byte 0xb1 is the polynomial x^7+x^3+x+1), then the CRC is (q*x^32) mod p,
  where a mod b means the remainder after dividing a by b.

  This calculation is done using the shift-register method of multiplying and
  taking the remainder.  The register is initialized to zero, and for each
  incoming bit, x^32 is added mod p to the register if the bit is a one (where
  x^32 mod p is p+x^32 = x^26+...+1), and the register is multiplied mod p by
  x (which is shifting right by one and adding x^32 mod p if the bit shifted
  out is a one).  We start with the highest power (least significant bit) of
  q and repeat for all eight bits of q.

  The table is simply the CRC of all possible eight bit values.  This is all
  the information needed to generate CRC's on data a byte at a time for all
  combinations of CRC register values and incoming bytes.
}
{local}
procedure make_crc_table;
var
 c    : uLong;
 n,k  : int;
 poly : uLong; { polynomial exclusive-or pattern }

const
 { terms of polynomial defining this crc (except x^32): }
 p: array [0..13] of Byte = (0,1,2,4,5,7,8,10,11,12,16,22,23,26);

begin
  { make exclusive-or pattern from polynomial ($EDB88320) }
  poly := Long(0);
  for n := 0 to (sizeof(p) div sizeof(Byte))-1 do
    poly := poly or (Long(1) shl (31 - p[n]));

  for n := 0 to 255 do
  begin
    c := uLong(n);
    for k := 0 to 7 do
    begin
      if (c and 1) <> 0 then
        c := poly xor (c shr 1)
      else
        c := (c shr 1);
    end;
    crc_table[n] := c;
  end;
  crc_table_empty := FALSE;
end;

{$ELSE}

{ ========================================================================
  Table of CRC-32's of all single-byte values (made by make_crc_table) }

{local}
uses Advance;

{$ENDIF}

{ =========================================================================
  This function can be used by asm versions of crc32() }

function get_crc_table : {const} puLong;
begin
{$ifdef DYNAMIC_CRC_TABLE}
  if (crc_table_empty) then
    make_crc_table;
{$endif}
  get_crc_table :=  {const} puLong(@crc_table);
end;

{ ========================================================================= }

function crc32 (crc : uLong; buf : pBytef; len : uInt): uLong;
begin
  if (buf = Z_NULL) then
    crc32 := Long(0)
  else
  begin

{$IFDEF DYNAMIC_CRC_TABLE}
    if crc_table_empty then
      make_crc_table;
{$ENDIF}

    crc := crc xor Long($ffffffff);
    while (len >= 8) do
    begin
      {DO8(buf)}
      crc := crc_table^[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table^[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table^[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table^[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table^[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table^[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table^[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);
      crc := crc_table^[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);

      Dec(len, 8);
    end;
    if (len <> 0) then
    repeat
      {DO1(buf)}
      crc := crc_table^[(int(crc) xor buf^) and $ff] xor (crc shr 8);
      inc(buf);

      Dec(len);
    until (len = 0);
    crc32 := crc xor Long($ffffffff);
  end;
end;


end.
