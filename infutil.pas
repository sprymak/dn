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

Unit infutil;

{ types and macros common to blocks and codes
  Copyright (C) 1995-1998 Mark Adler

   WARNING: this file should *not* be used by applications. It is
   part of the implementation of the compression library and is
   subject to change.

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}

uses
  zutil, zlib;

{ copy as much as possible from the sliding window to the output area }
function inflate_flush(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : int) : int;

{ And'ing with mask[n] masks the lower n bits }
const
  inflate_mask : array[0..17-1] of uInt = (
    $0000,
    $0001, $0003, $0007, $000f, $001f, $003f, $007f, $00ff,
    $01ff, $03ff, $07ff, $0fff, $1fff, $3fff, $7fff, $ffff);

{procedure GRABBITS(j : int);}
{procedure DUMPBITS(j : int);}
{procedure NEEDBITS(j : int);}

implementation

{ macros for bit input with no checking and for returning unused bytes }
procedure GRABBITS(j : int);
begin
  {while (k < j) do
  begin
    Dec(z^.avail_in);
    Inc(z^.total_in);
    b := b or (uLong(z^.next_in^) shl k);
    Inc(z^.next_in);
    Inc(k, 8);
  end;}
end;

procedure DUMPBITS(j : int);
begin
  {b := b shr j;
  Dec(k, j);}
end;

procedure NEEDBITS(j : int);
begin
 (*
          while (k < j) do
          begin
            {NEEDBYTE;}
            if (n <> 0) then
              r :=Z_OK
            else
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, LongInt(p)-LongInt(z.next_in));
              z.next_in := p;
              s.write := q;
              result := inflate_flush(s,z,r);
              exit;
            end;
            Dec(n);
            b := b or (uLong(p^) shl k);
            Inc(p);
            Inc(k, 8);
          end;
 *)
end;

procedure NEEDOUT;
begin
 (*
  if (m = 0) then
  begin
    {WRAP}
    if (q = s.zend) and (s.read <> s.window) then
    begin
      q := s.window;
      if LongInt(q) < LongInt(s.read) then
        m := uInt(LongInt(s.read)-LongInt(q)-1)
      else
        m := uInt(LongInt(s.zend)-LongInt(q));
    end;

    if (m = 0) then
    begin
      {FLUSH}
      s.write := q;
      r := inflate_flush(s,z,r);
      q := s.write;
      if LongInt(q) < LongInt(s.read) then
        m := uInt(LongInt(s.read)-LongInt(q)-1)
      else
        m := uInt(LongInt(s.zend)-LongInt(q));

      {WRAP}
      if (q = s.zend) and (s.read <> s.window) then
      begin
        q := s.window;
        if LongInt(q) < LongInt(s.read) then
          m := uInt(LongInt(s.read)-LongInt(q)-1)
        else
          m := uInt(LongInt(s.zend)-LongInt(q));
      end;

      if (m = 0) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, LongInt(p)-LongInt(z.next_in));
        z.next_in := p;
        s.write := q;
        result := inflate_flush(s,z,r);
        exit;
      end;
    end;
  end;
  r := Z_OK;
 *)
end;

{ copy as much as possible from the sliding window to the output area }
function inflate_flush(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : int) : int;
var
  n : uInt;
  p : pBytef;
  q : pBytef;
begin
  { local copies of source and destination pointers }
  p := z.next_out;
  q := s.read;

  { compute number of bytes to copy as far as end of window }
  if ptr2int(q) <= ptr2int(s.write) then
    n := uInt(ptr2int(s.write) - ptr2int(q))
  else
    n := uInt(ptr2int(s.zend) - ptr2int(q));
  if (n > z.avail_out) then
    n := z.avail_out;
  if (n <> 0) and (r = Z_BUF_ERROR) then
    r := Z_OK;

  { update counters }
  Dec(z.avail_out, n);
  Inc(z.total_out, n);


  { update check information }
  if Assigned(s.checkfn) then
  begin
    s.check := s.checkfn(s.check, q, n);
    z.adler := s.check;
  end;

  { copy as far as end of window }
  zmemcpy(p, q, n);
  Inc(p, n);
  Inc(q, n);

  { see if more to copy at beginning of window }
  if (q = s.zend) then
  begin
    { wrap pointers }
    q := s.window;
    if (s.write = s.zend) then
      s.write := s.window;

    { compute bytes to copy }
    n := uInt(ptr2int(s.write) - ptr2int(q));
    if (n > z.avail_out) then
      n := z.avail_out;
    if (n <> 0) and (r = Z_BUF_ERROR) then
      r := Z_OK;

    { update counters }
    Dec( z.avail_out, n);
    Inc( z.total_out, n);

    { update check information }
    if Assigned(s.checkfn) then
    begin
      s.check := s.checkfn(s.check, q, n);
      z.adler := s.check;
    end;

    { copy }
    zmemcpy(p, q, n);
    Inc(p, n);
    Inc(q, n);
  end;


  { update pointers }
  z.next_out := p;
  s.read := q;

  { done }
  inflate_flush := r;
end;


{$IFDEF THESE_MACROS_ARE_DEFINED_OUT}
/* defines for inflate input/output */
/*   update pointers and return */
#define UPDBITS {s->bitb=b;s->bitk=k;}
#define UPDIN {z->avail_in=n;z->total_in+=p-z->next_in;z->next_in=p;}
UPDOUT
{
  s.write := q;
}
#define UPDATE {UPDBITS UPDIN UPDOUT}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;

LEAVE
begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            result := inflate_flush(s,z,r);
            exit;
end;
/*   get bytes and bits */
#define LOADIN {p=z->next_in;n=z->avail_in;b=s->bitb;k=s->bitk;}
NEEDBYTE
begin
          if (n <> 0) then
            r :=Z_OK
          else
          begin
            {UPDATE}
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
            z.next_in := p;
            s.write := q;
            result := inflate_flush(s,z,r);
            exit;
          end;
end;

NEXTBYTE(n--,*p++)
begin
  Dec(n);
  p^
  Inc(p);
end;
{ Macro }
function NEXTBYTE : int;
begin
 Dec(z^.avail_in);
 Inc(z^.total_in);
 NEXTBYTE := z^.next_in^;
 Inc(z^.next_in);
end;


NEEDBITS(j)
begin
          while (k < j) do
          begin
            {NEEDBYTE;}
            if (n <> 0) then
              r :=Z_OK
            else
            begin
              {UPDATE}
              s.bitb := b;
              s.bitk := k;
              z.avail_in := n;
              Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
              z.next_in := p;
              s.write := q;
              result := inflate_flush(s,z,r);
              exit;
            end;
            Dec(n);
            b := b or (uLong(p^) shl k);
            Inc(p);
            Inc(k, 8);
          end;
end;

/*   output bytes */
WAVAIL (uInt)(q<s->read?s->read-q-1:s->end-q)
  if ptr2int(q) < ptr2int(s.read) then
    := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    := uInt(ptr2int(s.zend)-ptr2int(q));
LOADOUT {q=s->write;m=(uInt)WAVAIL;}
begin
  q := s.write;
  if ptr2int(q) < ptr2int(s.read) then
    m := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    m := uInt(ptr2int(s.zend)-ptr2int(q));
end;
WRAP {if(q==s->end&&s->read!=s->window){q=s->window;m=(uInt)WAVAIL;}}
begin
  if (q = s.zend) and (s.read <> s.window) then
  begin
    q := s.window;
    if ptr2int(q) < ptr2int(s.read) then
      m := uInt(ptr2int(s.read)-ptr2int(q)-1)
    else
      m := uInt(ptr2int(s.zend)-ptr2int(q));
  end;
end;
FLUSH {UPDOUT r=inflate_flush(s,z,r); LOADOUT}
begin
  s.write := q;
  r := inflate_flush(s,z,r);
  q := s.write;
  if ptr2int(q) < ptr2int(s.read) then
    m := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    m := uInt(ptr2int(s.zend)-ptr2int(q));
end;
NEEDOUT {if(m==0){WRAP if(m==0){FLUSH WRAP if(m==0) LEAVE}}r=Z_OK;}
begin
  if (m = 0) then
  begin
    {WRAP}
    if (q = s.zend) and (s.read <> s.window) then
    begin
      q := s.window;
      if ptr2int(q) < ptr2int(s.read) then
        m := uInt(ptr2int(s.read)-ptr2int(q)-1)
      else
        m := uInt(ptr2int(s.zend)-ptr2int(q));
    end;

    if (m = 0) then
    begin
      {FLUSH}
      s.write := q;
      r := inflate_flush(s,z,r);
      q := s.write;
      if ptr2int(q) < ptr2int(s.read) then
        m := uInt(ptr2int(s.read)-ptr2int(q)-1)
      else
        m := uInt(ptr2int(s.zend)-ptr2int(q));

      {WRAP}
      if (q = s.zend) and (s.read <> s.window) then
      begin
        q := s.window;
        if ptr2int(q) < ptr2int(s.read) then
          m := uInt(ptr2int(s.read)-ptr2int(q)-1)
        else
          m := uInt(ptr2int(s.zend)-ptr2int(q));
      end;

      if (m = 0) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;
        result := inflate_flush(s,z,r);
        exit;
      end;
    end;
  end;
  r := Z_OK;
end;
OUTBYTE(a) {*q++=(Byte)(a);m--;}
begin
  q^ := Byte(a);
  Inc(q);
  Dec(m);
end;
/*   load local pointers */
#define LOAD {LOADIN LOADOUT}
Load---
  { copy input/output information to locals }
  p := z.next_in;
  n := z.avail_in;
  b := s.bitb;
  k := s.bitk;
  q := s.write;
  if ptr2int(q) < ptr2int(s.read) then
    m := uInt(ptr2int(s.read)-ptr2int(q)-1)
  else
    m := uInt(ptr2int(s.zend)-ptr2int(q));

{
#include "zutil.h"
#include "infblock.h"
#include "inftrees.h"
#include "infcodes.h"
#include "infutil.h"

struct inflate_codes_state {int dummy; }
{$ENDIF}

end.
