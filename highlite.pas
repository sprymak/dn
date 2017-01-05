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

(*****************************************************************
 *
 * SOURCE FILE: highlite.pas
 *
 * MODULE NAME: Highlighter Unit
 *
 * PURPOSE:     This unit consists of routines that allow to
 *              highlitght a source text in file editor window
 *              accordingly with user specified parameters.
 *
 *              The parameters are read from *.HGL file by MACRO
 *              unit.
 *
 * AUTHOR:      Pawel Ziemian (PZ)
 *
 * HISTORY:     Ver   Date       Sign   Description
 *
 *              1.01  2000.04.12 PZ     Created
 *              1.02  2000.04.14 PZ     Updated
 *              1.03  2000.04.25 PZ     Updated/Fixed
 *              1.04  2000.04.27 DCB    Changed Integer to Word in
 *                                      THighliteParams
 *              1.05  2000.04.28 PZ     Changed interface from
 *                                      'string' to 'PChar'.
 *                                      Fixed bugs.
 *              1.06  2000.04.28 PZ     Fixed bug: end of comment
 *                                      is already treated as
 *                                      break character.
 *              1.07  2000.04.29 PZ     Fixed bugs.
 *              1.08  2000.05.02 PZ     Fixed bugs.
 *              1.09  2000.05.12 PZ     Improved capacity for
 *                                      keywords.
 *              1.10  2000.06.10 PZ     Better strings and float
 *                                      handling.
 *              1.11  2000.07.16 DCB    Make unit 32-bit compatable
 *
 *****************************************************************)
{$DEFINE NOASM}

unit highlite;

interface

const

  {General flags}

  hoCaseSensitive = $01;
  hoNoNumbers = $02;
  hoNoSymbols = $04;
  hoNoStrings = $08;
  ho0xPrefixHex = $10; {   0x####  C-Style }
  hoDollarPrefixHex = $20; {    $####  Pascal }
  hoFloatNumbers = $40; {    #.#e#  C or Pascal }
  hoAllowShortFloat = $80; {     .#e#  (option) C }

  {Number flags}

  hoSuffix = $01; {    ####x  Assembler }
  hoAmpersandPrefix = $08; {   &X####  Basic }
  hoAmpersandText = $10; { &X'####'  Strange Basic/Assembler }
  hoPrefix = $20; {   X'####  IAR assembler style }
  hoPrefixText = $40; {  X'####'  PIC assembler style }

  {String flags}

  hoEscDoubleQuate = $01; { \" represents " (C-Style) }
  hoEscSingleQuate = $02; { \' represents ' (C-Style) }
  hoHashCharacter = $04; { #number represents a character (Pascal) }
  hoCtrlCharacter = $08;
    { ^char represents a control character (Pascal) }
  hoOctalCharacter = $10; { octal_numberC represents a character }
  hoNoSQuotedStrings = $20;
    { Do not highlight single quoted strings }
  hoNoDQuotedStrings = $40;
    { Do not highlight double quoted strings }
  hoStrictCtrlChar = $80;
    { Do not highlight ^char followed by '0..9' or 'A..Z' }

type

  PHighliteParams = ^THighliteParams;
  THighliteParams = record
    GenFlags: word;
    HexFlags: word;
    DecFlags: word;
    OctFlagsQ: word;
    OctFlagsO: word;
    BinFlags: word;
    StrFlags: word;
    RulesBuffer: array[1..$800] of Char;
    end;

  THighliteRule = (
  hrCommentStarts,
  hrCommentStrings,
  hrKeywords1,
  hrKeywords2
  );

procedure Highlites(len: integer; s: PChar; const Params:
    THighliteParams);
procedure FixHighliteParams(var Params: THighliteParams);
function InsertHighliteRule(var Params: THighliteParams; Index:
    THighliteRule; const Rule: String): boolean;

const

  (*****************************************************************
 *
 * A colour codes returned by Highlites procedure.
 *
 *****************************************************************)

  hhNothing = #$00; {No highlighted text}
  hhNumber = #$01; {The text that seems to be a number}
  hhString = #$02; {The text that seems to be a string}
  hhComment = #$03; {The text that seems to be a comment}
  hhSymbol = #$04; {The text that seems to be a symbol}
  hhKeyword1 = #$05; {Keyword from bank 1}
  hhKeyword2 = #$06; {Keyword from bank 2}

implementation

uses
  {Consts,} {Cat: зачем? и без этого отлично компилится}
  Objects, { TCharSet }
  advance, { BreakChars }
  advance1; { UpStr, UpCase }

const

  FirstRule = Low(THighliteRule);
  LastRule = High(THighliteRule);

type
  THiliteRules = array[FirstRule..LastRule] of PChar;

  (*****************************************************************
 *
 * FUNCTION:    GetHighliteRules
 *
 * PURPOSE:     This function scans highlite rules in parameters
 *              and fills rules array with addresses of the rules.
 *
 * INPUTS:      Params - Highlite parameters.
 *              Rules  - Buffer for rules addresses.
 *
 * OUTPUTS:     Rules  - Buffer with rules addresses.
 *
 * RETURNS:     Pointer to next/free space in parameters.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function GetHighliteRules(const Params: THighliteParams; var Rules:
    array of PChar): PChar;

  function NextStr(P, PEnd: PChar): PChar;
    {$IFDEF NOASM}
    var
      C: Char;
    begin
      while P < PEnd do
        begin
          C := P^;
          Inc(P);
          if C = #0 then
            break;
        end;
      NextStr := P;
    end;
    {$ELSE}
    assembler;
    {&Frame-} {$USES ESI, EDI}
  asm              { Kirill }
        CLD
        MOV     ESI,P
        MOV     EDI,PEnd
  @@1:
        CMP     ESI,EDI
        JAE     @@2
        LODSB
        OR      AL,AL
        JNE     @@1
  @@2:
        MOV     EAX,ESI
  end
    ;
  {$ENDIF}

  var
    PEnd: PChar;
    P: PChar;
    i: integer;
  begin { GetHighliteRules }
    PEnd := @Params.RulesBuffer[High(Params.RulesBuffer)];
    P := @Params.RulesBuffer;
    for i := Low(Rules) to High(Rules) do
      begin
        Rules[i] := P;
        P := NextStr(P, PEnd);
      end;
    GetHighliteRules := P;
  end { GetHighliteRules };

(*****************************************************************
 *
 * PROCEDURE:   Highlites
 *
 * PURPOSE:     This procedure converts given string into colours
 *              accordingly with given highlight parameters.
 *
 * INPUTS:      Len    - The size of passed string buffer.
 *              S      - The string buffer to convert.
 *              Params - The parameters of colouring.
 *
 * OUTPUTS:     S - A colours of the string.
 *
 * NOTES:       Each character in a string is converted to its
 *              colour code.
 *
 * HISTORY:
 *
 *****************************************************************)

procedure Highlites(len: integer; s: PChar; const Params:
    THighliteParams);

  const
    HexDigits = ['0'..'9', 'A'..'F', 'a'..'f'];
    DecDigits = ['0'..'9'];
    OctDigits = ['0'..'7'];
    BinDigits = ['0'..'1'];

  function CheckEmpty(len: integer; s: PChar): boolean;
    {$IFDEF NOASM}
    var
      i: integer;
    begin
      CheckEmpty := False;
      while len > 0 do
        begin
          if not (s^ in [' ', #9]) then
            exit;
          Inc(s);
          Dec(len);
        end;
      CheckEmpty := True;
    end;
    {$ELSE}
    assembler;
    {&Frame-} {$USES ESI, ECX}
  asm
        CLD
        XOR     EAX,EAX
        MOV     ECX,Len
        MOV     ESI,S { Kirill: LEA->MOV }
        JCXZ    @@3
   @@1:
        LODSB
        CMP     AL,9
        JE      @@2
        CMP     AL,' '
        JNE     @@3
   @@2:
        LOOP    @@1
        INC     AH
   @@3:
        XCHG    AL,AH
   end
    ;
  {$ENDIF}

  function CheckStartComment(len: integer; s: PChar; t: PChar):
      boolean;
    {$IFDEF NOASM}
    var
      i: integer;
      j: integer;
      C: Char;
    begin
      CheckStartComment := False;
      while s^ in [#32, #9] do
        begin
          if s^ = #0 then
            exit;
          Inc(s)
        end;
      CheckStartComment := True;
      i := 0;
      j := 0;
      repeat
        C := t^;
        Inc(t);
        if (C in [#0, ',']) and (j > 0) then
          exit
        else if (j < len) and (C = s[j]) then
          Inc(j)
        else
          begin
            { skip current comment word }
            repeat
              C := t^;
              Inc(t);
            until C in [#0, ','];
            j := 0;
          end;
      until C = #0;
      CheckStartComment := False;
    end { CheckStartComment };
    {$ELSE}
    assembler;
    {&Frame-} {$USES ESI, EDI, EBX, ECX}
  asm
        CLD
        MOV     ESI,T   { Kirill: LEA->MOV }
        MOV     EDI,S   { Kirill: LEA->MOV }
        MOV     ECX,Len
   @@0:
        XOR     EBX,EBX
   @@1:
        LODSB
        OR      EBX,EBX
        JE      @@3
        CMP     AL,0
        JE      @@2
        CMP     AL,','
        JNE     @@3
   @@2:
        MOV     AL,1
        JMP     @@5
   @@3:
        CMP     EBX,ECX
        JAE     @@4
        CMP     AL,[EDI+EBX]
        JNE     @@4
        INC     EBX
        JMP     @@1
   @@4:
        LODSB
        CMP     AL,','
        JE      @@0
        OR      AL,AL
        JNE     @@4
   @@5:
   end
    ;
  {$ENDIF}

  function CheckPattern(i: integer; len: integer; s: PChar; const P:
      String; CaseSensitive: boolean): boolean;
    {$IFDEF NOASM}
    var
      j: integer;
      C: Char;
    begin
      CheckPattern := False;
      j := 1;
      while j <= Length(P) do
        begin
          if (i >= len) then
            exit;
          C := s[i];
          if not CaseSensitive then
            C := UpCase(C);
          if (C <> P[j]) then
            exit;
          Inc(j);
          Inc(i);
        end;
      CheckPattern := True;
    end { CheckPattern };
    {$ELSE}
    assembler;
    {&Frame-} {$USES ESI, EDI, EBX, ECX}
  asm
        CLD
        XOR     EAX,EAX
        MOV     ESI,P  { Kirill: LEA->MOV }
        LODSB
        MOV     ECX,EAX
        JCXZ    @@3
        MOV     EDI,S  { Kirill: LEA->MOV }
        MOV     EBX,Len
        ADD     EBX,EDI
        ADD     EDI,I
   @@1:
        CMP     EDI,EBX
        JA      @@4
        LODSB
        CMP     [EDI],AL
        JE      @@2
        CMP     CaseSensitive,AH
        JNE     @@4
        CMP     AL,'A'
        JB      @@4
        CMP     AL,'Z'
        JA      @@4
        ADD     AL,' '
        CMP     [EDI],AL
        JNE     @@4
   @@2:
        INC     EDI
        LOOP    @@1
   @@3:
        INC     AH
   @@4:
        XCHG    AL,AH
   end
    ;
  {$ENDIF}

  function ParseChars(i: integer; const Prefix: String; const
      Allowed: TCharSet; const Suffix: String): integer;
    var
      j: integer;
      k: integer;
      {   t : String; }
    begin
      ParseChars := 0;
      j := i;
      if not CheckPattern(j, len, s, Prefix, False) then
        exit;
      Inc(j, Length(Prefix));
      k := 0;
      while (j < len) and (s[j] in Allowed) do
        begin
          Inc(j);
          Inc(k);
        end;
      if k <= 0 then
        exit;
      if not CheckPattern(j, len, s, Suffix, False) then
        exit;
      Inc(j, Length(Suffix));
      ParseChars := j-i;
    end { ParseChars };

  function CheckNumber(i: integer): integer;

    function ParseNumber(Max: integer; Mode: Char; Options: word;
        const Digits: TCharSet): integer;
      const
        strSuffix: String[1] = 'X';
        strAmpersandPrefix: String[2] = '&X';
        strAmpersandText: String[3] = '&X''';
        strPrefix: String[2] = 'X''';
      var
        j: integer;
      begin
        if ((hoSuffix and Options) <> 0) and (s[i] in DecDigits)
        then
          begin
            strSuffix[1] := Mode;
            j := ParseChars(i, '', Digits, strSuffix);
            if j > Max then
              Max := j;
          end;
        if (hoAmpersandPrefix and Options) <> 0 then
          begin
            strAmpersandPrefix[2] := Mode;
            j := ParseChars(i, strAmpersandPrefix, Digits, '');
            if j > Max then
              Max := j;
          end;
        if (hoAmpersandText and Options) <> 0 then
          begin
            strAmpersandText[2] := Mode;
            j := ParseChars(i, strAmpersandText, Digits, '''');
            if j > Max then
              Max := j;
          end;
        if (hoPrefix and Options) <> 0 then
          begin
            strPrefix[1] := Mode;
            j := ParseChars(i, strPrefix, Digits, '');
            if j > Max then
              Max := j;
          end;
        if (hoPrefixText and Options) <> 0 then
          begin
            strPrefix[1] := Mode;
            j := ParseChars(i, strPrefix, Digits, '''');
            if j > Max then
              Max := j;
          end;
        ParseNumber := Max;
      end { ParseNumber };

    function ParseFloat: integer;
      var
        Max: integer;
        j: integer;
        k: integer;
      begin
        ParseFloat := 0;
        j := i;
        while (j < len) and (s[j] in DecDigits) do
          Inc(j);
        if (i = j) and ((Params.GenFlags and hoAllowShortFloat) = 0)
        then
          exit;
        Max := j-i;
        k := j;
        if s[j] = '.' then
          Inc(j)
        else if j = i then
          begin
            ParseFloat := Max;
            exit;
          end;
        if j > k then
          begin
            k := j;
            while (j < len) and (s[j] in DecDigits) do
              Inc(j);
            if j = k then
              begin
                ParseFloat := Max;
                exit;
              end;
          end;
        Max := j-i;
        if s[j] in ['e', 'E'] then
          begin
            Inc(j);
            if (j < len) and (s[j] in ['+', '-']) then
              Inc(j);
            k := j;
            while (j < len) and (s[j] in DecDigits) do
              Inc(j);
            if j > k then
              Max := j-i;
          end;
        ParseFloat := Max;
      end { ParseFloat: };

    var
      Max: integer;
      j: integer;
    begin { CheckNumber }

      {Default is float or decimal}

      if (hoFloatNumbers and Params.GenFlags) <> 0 then
        Max := ParseFloat
      else
        Max := ParseChars(i, '', DecDigits, '');

      {   0x####  C-Style }

      if (ho0xPrefixHex and Params.GenFlags) <> 0 then
        begin
          j := ParseChars(i, '0X', HexDigits, '');
          if j >= Max then
            Max := j;
        end;

      {    $####  Pascal-Style }

      if (hoDollarPrefixHex and Params.GenFlags) <> 0 then
        begin
          j := ParseChars(i, '$', HexDigits, '');
          if j >= Max then
            Max := j;
        end;

      {Hex numbers}

      if Params.HexFlags <> 0 then
        Max := ParseNumber(Max, 'H', Params.HexFlags, HexDigits);

      {Decimal numbers}

      if Params.DecFlags <> 0 then
        Max := ParseNumber(Max, 'D', Params.DecFlags, DecDigits);

      {Octal numbers Q}

      if Params.OctFlagsQ <> 0 then
        Max := ParseNumber(Max, 'Q', Params.OctFlagsQ, OctDigits);

      {Octal numbers O}

      if Params.OctFlagsO <> 0 then
        Max := ParseNumber(Max, 'O', Params.OctFlagsO, OctDigits);

      {Binary numbers}

      if Params.BinFlags <> 0 then
        Max := ParseNumber(Max, 'B', Params.BinFlags, BinDigits);

      CheckNumber := Max;

  end { CheckNumber };

  function CheckString(i: integer): integer;
    var
      Opts: word;
      j: integer;
      k: integer;
      l: integer;
      Term: Char;
      esc: boolean;
    begin
      CheckString := 0;
      Opts := Params.StrFlags;
      j := i;

      repeat
        if ((s[j] = '''') and ((hoNoSQuotedStrings and Opts) = 0))
          or ((s[j] = '"') and ((hoNoDQuotedStrings and Opts) = 0))
        then
          begin
            Term := s[j];
            esc := False;
            k := j+1;
            while k < len do
              begin
                if (s[k] = '\') and not esc then
                  begin
                    esc := True;
                  end
                else if s[k] = Term then
                  begin
                    if not ((Term = '"') and ((hoEscDoubleQuate and
                        Opts) <> 0) and esc)
                      and not ((Term = '''') and ((hoEscSingleQuate
                        and Opts) <> 0) and esc)
                    then
                      begin
                        Inc(k);
                        break;
                      end;
                  end
                else
                  begin
                    esc := False;
                  end;
                Inc(k);
              end;
            Dec(k, j);
          end
        else if ((hoHashCharacter and Opts) <> 0) and (s[j] = '#')
        then
          begin
            k := CheckNumber(j+1);
            if k > 0 then
              Inc(k);
          end
        else if ((hoCtrlCharacter and Opts) <> 0) and (s[j] = '^')
        then
          begin
            k := j+1;
            if (k < len) and (UpCase(s[k]) in ['@'..'_'])
              and (((Opts and hoStrictCtrlChar) = 0) or ((k+1) = len) or
                not (UpCase(s[k+1]) in ['0'..'9', 'A'..'Z']))
            then
              k := 2
            else
              k := 0;
          end
        else if ((hoOctalCharacter and Opts) <> 0) and (s[j] in
            OctDigits)
        then
          begin
            k := ParseChars(j, '', OctDigits, 'C');
            if k > 0 then
              Inc(k);
          end
        else
          begin
            k := 0;
          end;
        Inc(j, k);
      until k = 0;

      CheckString := j-i;

  end { CheckString };

  function CheckComment(i: integer; len: integer; s: PChar; t: PChar):
      integer;
    {$IFDEF NOASM}
    var
      j: integer;
      k: integer;
      o: integer;
      C: Char;
    begin
      k := i;
      repeat
        C := t^;
        Inc(t);
        if (C in [#0, #13, ',']) and (k > i) then
          begin
            CheckComment := len-i+1;
            { parse comment }
            if C = #13 then
              begin
                j := 0;
                o := k;
                while not (t[j] in [#0, ',']) and (k < len) do
                  begin
                    if t[j] = s[k] then
                      begin
                        { move pointer if everything matches }
                        Inc(j);
                        Inc(k);
                      end
                    else
                      begin
                        { search again but one character further }
                        Inc(o);
                        k := o;
                        j := 0;
                      end;
                  end;
                if k > o then
                  CheckComment := k-i;
              end;
            exit;
          end;
        if C <> #0 then
          begin
            if (k < len) and (C = s[k]) then
              { going down into a comment }
              Inc(k)
            else
              begin
                { skip current comment }
                repeat
                  C := t^;
                  Inc(t);
                until (C = #0) or (C = ',');
                { back to start }
                k := i;
              end;
          end;
      until C = #0;
      CheckComment := 0;
    end { CheckComment };
    {$ELSE}
    assembler;
    {&Frame-} {$USES ESI, EDI, EDX, ECX, EBX}
      { Kirill: add save ebx }
  asm
        CLD
        XOR     EAX,EAX
        MOV     ESI,T  { Kirill: LEA->MOV }
        MOV     EDI,S  { Kirill: LEA->MOV }
        MOV     EDX,Len
        ADD     EDX,EDI         { j }
        ADD     EDI,I           { k }
        MOV     ECX,EDI         { I }
        JMP     @@1
   @@0:
        LODSB
        OR      AL,AL           { e.nd of comments list }
        JE      @@4
        CMP     AL,','          { next comment }
        JNE     @@0
        MOV     EDI,ECX         { back to start }
   @@1:                         { repeat }
        LODSB
        CMP     EDI,ECX
        JBE     @@3             { nothing is found }
        OR      AL,AL           { e.nd of comments list }
        JE      @@2f
        CMP     AL,','
        JE      @@2f            { e.nd of line comment }
        CMP     AL,13
        JNE     @@3
        MOV     EBX,EDI
        MOV     I,ESI
   @@2:
        LODSB
        OR      AL,AL
        JE      @@2_2
        CMP     AL,','
        JE      @@2_2
        CMP     EDI,EDX
        JA      @@2_2
        CMP     AL,[EDI]
        JNE     @@2_1
        INC     EDI
        JMP     @@2
   @@2_1:
        INC     EBX
        MOV     ESI,I
        MOV     EDI,EBX
        JMP     @@2
   @@2_2:
        CMP     EDI,I
        JBE     @@2f
        SUB     EDI,ECX
        MOV     EAX,EDI
        JMP     @@4
   @@2f:
        MOV     EAX,EDX
        SUB     EAX,ECX
        INC     EAX
        JMP     @@4
   @@3:
        OR      AL,AL           { e.nd of comments list }
        JE      @@4
        CMP     EDI,EDX
        JA      @@0
        CMP     AL,[EDI]
        JNE     @@0
        INC     EDI
        JMP     @@1
   @@4:
   end
    ;
  {$ENDIF}

  function CheckKeyword(i: integer; len: integer; s: PChar; Keywords:
      PChar): integer;
    {$IFDEF NOASM}
    var
      j: integer;
      k: integer;
      C: Char;
    begin
      j := i;
      k := i;
      repeat
        C := Keywords^;
        Inc(Keywords);
        if C in [#0, ','] then
          begin
            { get the longest keyword }
            if k > j then
              j := k;
            { back to start }
            k := i;
          end
        else
          begin
            if (k < len) and (C = s[k]) then
              { going down into a keyword }
              Inc(k)
            else
              begin
                { skip current keyword }
                repeat
                  C := Keywords^;
                  Inc(Keywords);
                until C in [#0, ','];
                { back to start }
                k := i;
              end;
          end;
      until C = #0;
      CheckKeyword := j-i;
    end { CheckKeyword };
    {$ELSE}
    assembler;
    {&Frame-} {$USES ESI, EDI, EDX, EBX, ECX}
  asm
        CLD
        XOR     EAX,EAX
        MOV     ESI,Keywords    { Kirill: LEA->MOV }
        MOV     EDI,S           { Kirill: LEA->MOV }
        MOV     EDX,Len
        ADD     EDX,EDI         { Length(S) }
        ADD     EDI,I           { k }
        MOV     ECX,EDI         { I }
        MOV     EBX,EDI         { j }
        JMP     @@2
   @@0:                         { skip keyword }
        LODSB
        OR      AL,AL
        JE      @@5
        CMP     AL,','          { next keyword }
        JNE     @@0
   @@1:                         { back to start }
        MOV     EDI,ECX
   @@2:                         { repeat }
        LODSB
        OR      AL,AL
        JZ      @@3
        CMP     AL,','
        JE      @@3
        CMP     EDI,EDX
        JA      @@0
        CMP     AL,[EDI]
        JNE     @@0
        INC     EDI
        JMP     @@2
   @@3:
        CMP     EDI,EBX
        JBE     @@4
        MOV     EBX,EDI         { get new longer keyword }
   @@4:
        OR      AL,AL
        JNE     @@1
   @@5:
        MOV     EAX,EBX
        SUB     EAX,ECX
   end
    ;
  {$ENDIF}

  var
    i: integer;
    Max: integer;
    j: integer;
    k: integer;
    l: integer;
    C: Char;
    D: Char;
    B: boolean;
    Rules: THiliteRules;
    BC: Set of Char;
  begin { Highlites }
    GetHighliteRules(Params, Rules);
    if (Params.GenFlags and hoCaseSensitive) = 0 then
      begin
        for i := 0 to len-1 do
          s[i] := UpCase(s[i]);
      end;
    if CheckEmpty(len, s) then
      begin
        FillChar(s^, len, hhSymbol);
      end
    else if CheckStartComment(len, s, Rules[hrCommentStarts]) then
        begin
        FillChar(s^, len, hhComment);
      end
    else
      begin
        BC := BreakChars;
        if (Params.GenFlags and hoNoStrings) = 0 then
          begin
            if (Params.StrFlags and hoNoSQuotedStrings) = 0 then
              Include(BC, '''');
            if (Params.StrFlags and hoNoDQuotedStrings) = 0 then
              Include(BC, '"');
          end;
        B := True;
        i := 0;
        while i < len do
          begin
            Max := 1;
            if s[i] in BC then
              C := hhSymbol
            else
              C := hhNothing;
            if B then
              begin
                k := Max;
                D := C;
                if (Params.GenFlags and hoNoNumbers) = 0 then
                  begin
                    j := CheckNumber(i);
                    if j >= k then
                      begin
                        k := j;
                        D := hhNumber;
                      end;
                  end;
                j := CheckKeyword(i, len, s, Rules[hrKeywords1]);
                if j >= k then
                  begin
                    k := j;
                    D := hhKeyword1;
                  end;
                j := CheckKeyword(i, len, s, Rules[hrKeywords2]);
                if j >= k then
                  begin
                    k := j;
                    D := hhKeyword2;
                  end;
                if ((i+k) >= len) or (s[i+k] in BC) or (s[i+k-1] in BC)
                then
                  begin
                    Max := k;
                    C := D;
                  end;
              end;
            B := False;
            j := CheckComment(i, len, s, Rules[hrCommentStrings]);
            if j >= Max then
              begin
                Max := j;
                C := hhComment;
                B := True;
              end;
            if (Params.GenFlags and hoNoStrings) = 0 then
              begin
                j := CheckString(i);
                if j >= Max then
                  begin
                    Max := j;
                    C := hhString;
                    B := True;
                  end;
              end;
            if C = hhSymbol then
              begin
                B := True;
                if (Params.GenFlags and hoNoSymbols) <> 0 then
                  C := hhNothing;
              end;
            FillChar(s[i], Max, C);
            Inc(i, Max);
          end;
      end;
  end { Highlites };

(*****************************************************************
 *
 * PROCEDURE:   FixHighliteParams
 *
 * PURPOSE:     This procedure calculates some fields in highlight
 *              parameters record accordingly with the other
 *              fields and makes the record usable for Highlites
 *              procedure.
 *
 * INPUTS:      Params - The highlight parameters.
 *
 * OUTPUTS:     Params - Updated parameters.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure FixHighliteParams(var Params: THighliteParams);

  procedure UpPStr(s: PChar);
    begin
      while s^ <> #0 do
        begin
          s^:= UpCase(s^);
          Inc(s);
        end;
    end;

  var
    Rules: THiliteRules;
    i: THighliteRule;
  begin
    Params.RulesBuffer[High(Params.RulesBuffer)] := #0;
    GetHighliteRules(Params, Rules);
    if (Params.GenFlags and hoCaseSensitive) = 0 then
      begin
        for i := Low(Rules) to High(Rules) do
          begin
            UpPStr(Rules[i]);
          end;
      end;
  end { FixHighliteParams };

(*****************************************************************
 *
 * FUNCTION:    InsertHighliteRule
 *
 * PURPOSE:     This function insert a new rule into highlite
 *              parameters.
 *
 * INPUTS:      Params - Buffer with highlite parameters.
 *              Index  - A rule type.
 *              Rule   - A rule to insert.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - Successful.
 *              False - Out of space or invalid rule.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function InsertHighliteRule(var Params: THighliteParams; Index:
    THighliteRule; const Rule: String): boolean;
  var
    exrules: array[Low(THighliteRule)..succ(High(THighliteRule))] of
      PChar;
    Rules: THiliteRules absolute exrules;
    space: integer;
    comma: boolean;
    needed: integer;
  const
    l = Low(exrules);
    H = High(exrules);
  begin
    InsertHighliteRule := False;
    if (Index in [Low(Rules)..High(Rules)]) and (Rule <> '') and (
        Pos(#0, Rule) = 0)
    then
      begin
        exrules[H] := GetHighliteRules(Params, Rules);
        space := @Params.RulesBuffer[High(Params.RulesBuffer)]-
          exrules[H]+1;
        needed := Length(Rule);
        comma := Rules[Index]^ <> #0;
        if comma then
          Inc(needed);
        if needed <= space then
          begin
            exrules[l] := exrules[succ(Index)]-1;
            Move(exrules[l][0], exrules[l][needed], exrules[H]-
              exrules[l]);
            if comma then
              begin
                exrules[l]^:= ',';
                Inc(exrules[l]);
              end;
            Move(Rule[1], exrules[l]^, Length(Rule));
            InsertHighliteRule := True;
          end;
      end;
  end { InsertHighliteRule };

end.
