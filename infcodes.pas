{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.10
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

Unit InfCodes;

{ infcodes.c -- process literals and length/distance pairs
  Copyright (C) 1995-1998 Mark Adler

  Pascal tranlastion
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}

uses
  {$IFDEF DEBUG}
  strutils,
  {$ENDIF}
  zutil, zlib;

function inflate_codes_new (bl : uInt;
                            bd : uInt;
                            tl : pInflate_huft;
                            td : pInflate_huft;
                            var z : z_stream): pInflate_codes_state;

function inflate_codes(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : int) : int;

procedure inflate_codes_free(c : pInflate_codes_state;
                             var z : z_stream);

implementation

uses
  infutil, inffast;


function inflate_codes_new (bl : uInt;
                            bd : uInt;
                            tl : pInflate_huft;
                            td : pInflate_huft;
                            var z : z_stream): pInflate_codes_state;
var
 c : pInflate_codes_state;
begin
  c := pInflate_codes_state( ZALLOC(z,1,sizeof(inflate_codes_state)) );
  if (c <> Z_NULL) then
  begin
    c^.mode := START;
    c^.lbits := Byte(bl);
    c^.dbits := Byte(bd);
    c^.ltree := tl;
    c^.dtree := td;
    {$IFDEF DEBUG}
    Tracev('inflate:       codes new');
    {$ENDIF}
  end;
  inflate_codes_new := c;
end;


function inflate_codes(var s : inflate_blocks_state;
                       var z : z_stream;
                       r : int) : int;
var
  j : uInt;               { temporary storage }
  t : pInflate_huft;      { temporary pointer }
  e : uInt;               { extra bits or operation }
  b : uLong;              { bit buffer }
  k : uInt;               { bits in bit buffer }
  p : pBytef;             { input data pointer }
  n : uInt;               { bytes available there }
  q : pBytef;             { output window write pointer }
  m : uInt;               { bytes to end of window or read pointer }
  f : pBytef;             { pointer to copy strings from }
var
  c : pInflate_codes_state;
begin
  c := s.sub.decode.codes;  { codes state }

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

  { process input and output based on current state }
  while True do
  case (c^.mode) of
    { waiting for "i:"=input, "o:"=output, "x:"=nothing }
  START:         { x: set up for LEN }
    begin
{$ifndef SLOW}
      if (m >= 258) and (n >= 10) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;

        r := inflate_fast(c^.lbits, c^.dbits, c^.ltree, c^.dtree, s, z);
        {LOAD}
        p := z.next_in;
        n := z.avail_in;
        b := s.bitb;
        k := s.bitk;
        q := s.write;
        if ptr2int(q) < ptr2int(s.read) then
          m := uInt(ptr2int(s.read)-ptr2int(q)-1)
        else
          m := uInt(ptr2int(s.zend)-ptr2int(q));

        if (r <> Z_OK) then
        begin
          if (r = Z_STREAM_END) then
            c^.mode := WASH
          else
            c^.mode := BADCODE;
          continue;    { break for switch-statement in C }
        end;
      end;
{$endif} { not SLOW }
      c^.sub.code.need := c^.lbits;
      c^.sub.code.tree := c^.ltree;
      c^.mode := LEN;  { falltrough }
    end;
  LEN:           { i: get length/literal/eob next }
    begin
      j := c^.sub.code.need;
      {NEEDBITS(j);}
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
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      t := c^.sub.code.tree;
      Inc(t, uInt(b) and inflate_mask[j]);
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);

      e := uInt(t^.exop);
      if (e = 0) then            { literal }
      begin
        c^.sub.lit := t^.base;
       {$IFDEF DEBUG}
        if (t^.base >= $20) and (t^.base < $7f) then
          Tracevv('inflate:         literal '+char(t^.base))
        else
          Tracevv('inflate:         literal '+IntToStr(t^.base));
        {$ENDIF}
        c^.mode := LIT;
        continue;  { break switch statement }
      end;
      if (e and 16 <> 0) then            { length }
      begin
        c^.sub.copy.get := e and 15;
        c^.len := t^.base;
        c^.mode := LENEXT;
        continue;         { break C-switch statement }
      end;
      if (e and 64 = 0) then             { next table }
      begin
        c^.sub.code.need := e;
        c^.sub.code.tree := @huft_ptr(t)^[t^.base];
        continue;         { break C-switch statement }
      end;
      if (e and 32 <> 0) then            { end of block }
      begin
        {$IFDEF DEBUG}
        Tracevv('inflate:         end of block');
        {$ENDIF}
        c^.mode := WASH;
        continue;         { break C-switch statement }
      end;
      c^.mode := BADCODE;        { invalid code }
      z.msg := 'invalid literal/length code';
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  LENEXT:        { i: getting length extra (have base) }
    begin
      j := c^.sub.copy.get;
      {NEEDBITS(j);}
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
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      Inc(c^.len, uInt(b and inflate_mask[j]));
      {DUMPBITS(j);}
      b := b shr j;
      Dec(k, j);

      c^.sub.code.need := c^.dbits;
      c^.sub.code.tree := c^.dtree;
      {$IFDEF DEBUG}
      Tracevv('inflate:         length '+IntToStr(c^.len));
      {$ENDIF}
      c^.mode := DIST;
      { falltrough }
    end;
  DIST:          { i: get distance next }
    begin
      j := c^.sub.code.need;
      {NEEDBITS(j);}
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
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      t := @huft_ptr(c^.sub.code.tree)^[uInt(b) and inflate_mask[j]];
      {DUMPBITS(t^.bits);}
      b := b shr t^.bits;
      Dec(k, t^.bits);

      e := uInt(t^.exop);
      if (e and 16 <> 0) then            { distance }
      begin
        c^.sub.copy.get := e and 15;
        c^.sub.copy.dist := t^.base;
        c^.mode := DISTEXT;
        continue;     { break C-switch statement }
      end;
      if (e and 64 = 0) then     { next table }
      begin
        c^.sub.code.need := e;
        c^.sub.code.tree := @huft_ptr(t)^[t^.base];
        continue;     { break C-switch statement }
      end;
      c^.mode := BADCODE;        { invalid code }
      z.msg := 'invalid distance code';
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  DISTEXT:       { i: getting distance extra }
    begin
      j := c^.sub.copy.get;
      {NEEDBITS(j);}
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
          inflate_codes := inflate_flush(s,z,r);
          exit;
        end;
        Dec(n);
        b := b or (uLong(p^) shl k);
        Inc(p);
        Inc(k, 8);
      end;
      Inc(c^.sub.copy.dist, uInt(b) and inflate_mask[j]);
      {DUMPBITS(j);}
      b := b shr j;
      Dec(k, j);
      {$IFDEF DEBUG}
      Tracevv('inflate:         distance '+ IntToStr(c^.sub.copy.dist));
      {$ENDIF}
      c^.mode := COPY;
      { falltrough }
    end;
  COPY:          { o: copying bytes in window, waiting for space }
    begin
      f := q;
      Dec(f, c^.sub.copy.dist);
      if (uInt(ptr2int(q) - ptr2int(s.window)) < c^.sub.copy.dist) then
      begin
        f := s.zend;
        Dec(f, c^.sub.copy.dist - uInt(ptr2int(q) - ptr2int(s.window)));
      end;

      while (c^.len <> 0) do
      begin
        {NEEDOUT}
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
              inflate_codes := inflate_flush(s,z,r);
              exit;
            end;
          end;
        end;
        r := Z_OK;

        {OUTBYTE( *f++)}
        q^ := f^;
        Inc(q);
        Inc(f);
        Dec(m);

        if (f = s.zend) then
          f := s.window;
        Dec(c^.len);
      end;
      c^.mode := START;
      { C-switch break; not needed }
    end;
  LIT:           { o: got literal, waiting for output space }
    begin
      {NEEDOUT}
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
            inflate_codes := inflate_flush(s,z,r);
            exit;
          end;
        end;
      end;
      r := Z_OK;

      {OUTBYTE(c^.sub.lit);}
      q^ := c^.sub.lit;
      Inc(q);
      Dec(m);

      c^.mode := START;
      {break;}
    end;
  WASH:          { o: got eob, possibly more output }
    begin
      {FLUSH}
      s.write := q;
      r := inflate_flush(s,z,r);
      q := s.write;
      if ptr2int(q) < ptr2int(s.read) then
        m := uInt(ptr2int(s.read)-ptr2int(q)-1)
      else
        m := uInt(ptr2int(s.zend)-ptr2int(q));

      if (s.read <> s.write) then
      begin
        {UPDATE}
        s.bitb := b;
        s.bitk := k;
        z.avail_in := n;
        Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
        z.next_in := p;
        s.write := q;
        inflate_codes := inflate_flush(s,z,r);
        exit;
      end;
      c^.mode := ZEND;
      { falltrough }
    end;

  ZEND:
    begin
      r := Z_STREAM_END;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  BADCODE:       { x: got error }
    begin
      r := Z_DATA_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  else
    begin
      r := Z_STREAM_ERROR;
      {UPDATE}
      s.bitb := b;
      s.bitk := k;
      z.avail_in := n;
      Inc(z.total_in, ptr2int(p)-ptr2int(z.next_in));
      z.next_in := p;
      s.write := q;
      inflate_codes := inflate_flush(s,z,r);
      exit;
    end;
  end;
{NEED_DUMMY_RETURN - Delphi2+ dumb compilers complain without this }
  inflate_codes := Z_STREAM_ERROR;
end;


procedure inflate_codes_free(c : pInflate_codes_state;
                             var z : z_stream);
begin
  ZFREE(z, c);
  {$IFDEF DEBUG}
  Tracev('inflate:       codes free');
  {$ENDIF}
end;

end.
