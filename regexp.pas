(*****************************************************************
 * Copyright (C), 2001 Pawel Ziemian, All rights reserved        *
 *****************************************************************)

(*****************************************************************
 *
 * SOURCE FILE: REGEXP.PAS
 *
 * MODULE NAME: Regular Expression for Objects
 *
 * PURPOSE:
 *
 * AUTHOR:      Pawel Ziemian (PZ)
 *
 * REVIEWED BY:
 *
 * ORIGINAL NOTES:
 * ===============================================================
 * Based on: REGEX.C, REGEX.H                           2001.09.16
 * ---------------------------------------------------------------
 * Copyright (c) 1986 by University of Toronto.
 * Written by Henry Spencer.  Not derived from licensed software.
 *
 * Permission is granted to anyone to use this software for any
 * purpose on any computer system, and to redistribute it freely,
 * subject to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of
 *    this software, no matter how awful, even if they arise
 *    from defects in it.
 *
 * 2. The origin of this software must not be misrepresented, either
 *    by explicit claim or by omission.
 *
 * 3. Altered versions must be plainly marked as such, and must not
 *    be misrepresented as being the original software.
 *
 * Beware that some of this code is subtly aware of the way operator
 * precedence is structured in regular expressions.  Serious changes
 * in regular-expression syntax might require a total rethink.
 * ===============================================================
 * Based on: REGEXPR.PAS v. 0.942                       2001.10.24
 * ---------------------------------------------------------------
 * Copyright (c) 1999-00 by Andrey V. Sorokin <anso@mail.ru>
 *
 * This software is provided as it is, without any kind of warranty
 * given. Use it at your own risk.
 *
 * You may use this software in any kind of development, including
 * comercial, redistribute, and modify it freely, under the
 * following restrictions :
 * 1. The origin of this software may not be mispresented, you must
 *    not claim that you wrote the original software. If you use
 *    this software in any kind of product, it would be appreciated
 *    that there in a information box, or in the documentation would
 *    be an acknowledgmnent like this
 *           Partial Copyright (c) 2000 by Andrey V. Sorokin
 * 2. You may not have any income from distributing this source
 *    to other developers. When you use this product in a comercial
 *    package, the source may not be charged seperatly.
 * ===============================================================
 *
 * HISTORY:
 *
 * Ver   Date       Sign   Description
 *
 * 0.01  2001.09.16 PZ     Created based on REGEX.C
 * 0.02  2001.09.20 PZ     Object version
 * 0.03  2001.09.23 PZ     Optimized
 * 0.04  2001.09.26 PZ     Added '\:#' escapes
 * 0.05  2001.09.27 PZ     Optimized
 * 0.06  2001.10.01 PZ     Fixed bugs in '\:#'
 * 0.07  2001.10.02 PZ     Added '\a', '\b' and '\v'
 * 0.08  2001.10.03 PZ     Added posibility to define
 *                         user '\:#' escapes ('#' is 0..9)
 * 0.09  2001.10.18 PZ     Added Substitution methods
 *                         ('\0'..'\9' for '(..)')
 * 0.10  2001.10.22 PZ     Added tagged expressions
 *                         - '(?dX)' where d is 1..9
 *                         - '(?:X)' no tagged
 *                         - '\1'..'\9' escapes
 * 0.11  2001.10.24 PZ     Added '{n,m}' syntax (by REGEXPR.PAS).
 * 0.12  2001.11.13 PZ     Fixed bugs
 *
 *****************************************************************)

{$I STDEFINE.INC}
{$UNDEF DEBUG}
{$DEFINE ComplexBraces}
  { define for beta-version of braces                  }
{ (in stable version it works only for simple cases) }

unit RegExp;

interface

uses
  Objects;

type

  TRegExpStatus = (
  resOK,
  resCanceled,
  resNilArgument,
  resInvalidArgument,
  resRegExpTooBig,
  resOutOfSpace,
  resCorruptedProgram,
  resUnmatchedParenthesis,
  resJunkOnEnd,
  resStarPlusOperandCouldBeEmpty,
  resNestedStarQuotePlus,
  resInvalidEscape,
  resInvalidPredefinedExp,
  resUndefinedPredefinedExp,
  resStackOverflow,
  resInvalidSetRange,
  resUnmatchedSquareBracket,
  resInternalUrp,
  resOperatorFollowsNothing,
  resTrailingBackSlash,
  resInternalDisaster,
  resNoExpression,
  resMemoryCorruption,
  resCorruptedPointers,
  resInternalFoulup,

  resDuplicatedTaggedExp,
  resInvalidTaggedExp,
  resComplexBracesNotImplemented,
  resInvalidBraces,
  resLoopStackExceeded,
  resLoopWithoutEntry
  );

type

  (*
   * Flags to be passed up and down.
   *)
  TRegExpFlag = (
  refHasWidth, (* Known never to match null string.      *)
  refSimple, (* Simple enough to be STAR/PLUS operand. *)
  refSpStart (* Starts with * or +.                    *)
  );
  TRegExpFlags = Set of TRegExpFlag;

const
  LoopStackMax = 10; { max depth of loops stack }

type

  PRegExp = ^TRegExp;
  TRegExp = object(TObject)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    {$IFDEF DEBUG}
    Narrate: boolean;
    {$ENDIF DEBUG}
    FStatus: TRegExpStatus;
    FStart: integer;
    FLength: integer;
    Constructor Init;
    destructor Done; virtual;
    procedure Reset;
    function CompileString(const AExpression: String): boolean;
    function CompileStr(AExpression: PChar): boolean;
    function Compile(AExpression: PChar; ALength: integer): boolean;
    function Execute(AString: PChar; ALength: integer): boolean;
    function SubstituteString(ASrc: PChar; const AReplace: String;
      var ADest: String): boolean;
    function SubstituteStr(ASrc, AReplace: PChar; ADest: PChar; var
      ALength: integer): boolean;
    function Substitute(ASrc, AReplace: PChar; ARLen: integer; ADest:
      PChar; var ADLen: integer): boolean;
    procedure Error(AStatus: TRegExpStatus); virtual;
    function CheckBreak: boolean; virtual;
    procedure Escape(AChar: Char; var ASubExp: PChar; var ALen:
      integer); virtual;
    {$IFDEF DEBUG}
    procedure Dump;
    {$ENDIF DEBUG}
    private
    (*
   * The "internal use only" fields in regexp.h are present to pass info
   * from compile to execute that permits the execute phase to run lots
   * faster on simple cases.  They are:
   *
   * regstart char that must begin a match; '\0' if none obvious
   * reganch  is the match anchored (at beginning-of-line only)?
   * regmust  string (pointer into program) that match must include, or NULL
   * regmlen  length of regmust string
   *
   * Regstart and reganch permit very fast decisions on suitable starting
   * points for a match, cutting down the work a lot.  Regmust permits fast
   * rejection of lines that cannot possibly match.  The regmust tests are
   * costly enough that regcomp() supplies a regmust only if the r.e. contains
   * something potentially expensive (at present, the only such thing detected
   * is * or + at the start of the r.e., which can involve a lot of backup).
   * Regmlen is supplied because the test in regexec() needs it and regcomp()
   * is computing it anyway.
   *)
    FFlags: Set of (
    ffCompiled, { expression is compiled }
    ffAnchored, { expression is anchored }
    ffStart, { expression starts with FStartCh }
    ffParsing, { calculating code size }
    ffMatchNext,
    ffBreak, { 'Execute' is cancelled }
    ffAutoTag { automatic tagged expressions }
    );
    FCodeSize: word; { *1* }
    FCodeData: PChar; { *2* }
    FStartP: array[1..9] of integer;
      { founded expr starting points }
    FEndP: array[1..9] of integer; { founded expr end points      }
    FStartCh: Char;
    FMust: PString; { *3* }
    FInput: PChar; { *4* }
    FInputBol: PChar; { *5* }
    FInputEol: PChar; { *6* }
    FLStack: array[1..LoopStackMax] of integer;
      { state before entering loop }
    FLStackId: integer; { 0 - out of all loops       }
    {
    // The following variables have different meaning while parsing or
    // compiling the regular expression:
    //
    // *1* - Calculates the program size while parsing.
    // *2* - Not used while parsing except it must be non-nil.
    //       Points to the next emited code byte while compiling.
    // *3* - Points to internal stack while parsing or compiling.
    // *4* - Points to the next character of being parsed or compiled
    //       (sub)expression.
    // *5* - Points to the being parsed or compiled (sub)expression.
    // *6* - Points just beyond the being parsed/compiled expression.
  }
    { Compilation routines }
    function RegCompile(AExpression: PChar; ALength: integer):
      boolean;
    procedure IncRegSize(i: integer);
    function Reg(Paren: integer; var FlagP: TRegExpFlags): PChar;
    function RegBranch(var FlagP: TRegExpFlags): PChar;
    function RegPiece(var FlagP: TRegExpFlags): PChar;
    function RegAtom(var FlagP: TRegExpFlags): PChar;
    function RegEnter(Op: Char; Tagged: boolean): boolean;
    procedure RegLeave;
    function RegLoadNumber(Base: integer; Digits: integer; var
      AResult): boolean;
    function RegEscape(var AResult: Char): boolean;
    function RegSet: PChar;
    function RegExactly(var FlagP: TRegExpFlags): PChar;
    function RegNode(Op: Char): PChar;
    procedure RegC(B: Char);
    function RegInsert(Op: Char; Opnd: PChar; ASize: integer): PChar;
    procedure RegTail(P: PChar; Val: PChar);
    procedure RegOpTail(P: PChar; Val: PChar);
    { Execute routines }
    function RegExecute(AString: PChar; ALength: integer): boolean;
    function RegTry(AString: PChar): boolean;
    function RegMatch(Prog: PChar): boolean;
    function RegRepeat(P: PChar; AMax: integer): integer;
    { Common routines }
    function RegNext(P: PChar): PChar;
    procedure RegClearTags;
    { Replace support }
    function RegSub(ASrc, AReplace: PChar; ARLen: integer; ADest:
      PChar; var ADLen: integer): boolean;
    end;

implementation

uses
  Strings,
  Memory;

(*****************************************************************
 *
 * FUNCTION:    StrScan2
 *
 * PURPOSE:     This function returns a pointer to the first
 *              occurrence of a character in a string.
 *
 * INPUTS:      P  - A string to scan.
 *              C  - A character to scan.
 *              P2 - An address just beyond the string.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     nil   - A character not found.
 *              Other - An address of the character in P.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function StrScan2(P: PChar; C: Char; P2: PChar): PChar;
  {$IFDEF NOASM}
  begin
    StrScan2 := nil;
    while P < P2 do
      begin
        if P[0] = C then
          begin
            StrScan2 := P;
            exit;
          end;
        Inc(P);
      end;
  end;
  {$ELSE ASM}
  assembler;
  {&Frame-} {&USES EDI, ECX}
asm
        MOV     EDI,P
        MOV     ECX,P2
        XOR     EAX,EAX         { default nil result }
        SUB     ECX,EDI         { calculate length }
        JBE     @@1             { P2 must be greater than P }
        MOV     AL,C
        CLD
        REPNE   SCASB
        MOV     AL,0            { restore the nil }
        JNE     @@1
        LEA     EAX,[EDI-1]     { get the address }
@@1:
end
  ;
{$ENDIF ASM}

(*****************************************************************
 *
 * TRegExp support types and routines.
 *
 *****************************************************************)

(*****************************************************************
 *
 * Structure for regexp "program".  This is essentially a linear
 * encoding of a nondeterministic finite-state machine (aka syntax
 * charts or "railroad normal form" in parsing technology).  Each
 * node is an opcode plus a "next" pointer, possibly plus an operand.
 * "Next" pointers of all nodes except BRANCH implement concatenation;
 * a "next" pointer with a BRANCH on both ends of it is connecting
 * two alternatives.  (Here we have one of the subtle syntax
 * dependencies:  an individual BRANCH (as opposed to a collection of
 * them) is never concatenated with anything because of operator
 * precedence.)  The operand of some types of node is a literal
 * string; for others, it is a node leading into a sub-FSM.
 * In particular, the operand of a BRANCH node is the first node
 * of the branch. (NB this is *not* a tree structure:  the tail of
 * the branch connects to the thing following the set of BRANCHes.)
 * The opcodes are:
 *)

const
  (* definition    number   opnd? meaning *)
  ropEND = #00; (* no    End of program. *)
  ropBOL = #01; (* no    Match "" at beginning of line. *)
  ropEOL = #02; (* no    Match "" at end of line. *)
  ropANY = #03; (* no    Match any one character. *)
  ropANYOF = #04; (* str   Match any character in this string. *)
  ropANYBUT = #05;
    (* str   Match any character not in this string. *)
  ropBRANCH = #06;
    (* node  Match this alternative, or the next... *)
  ropBACK = #07; (* no    Match "", "next" ptr points backward. *)
  ropEXACTLY = #08; (* str   Match this string. *)
  ropNOTHING = #09; (* no    Match empty string. *)
  ropSTAR = #10;
    (* node  Match this (simple) thing 0 or more times. *)
  ropPLUS = #11;
    (* node  Match this (simple) thing 1 or more times. *)
  ropOPEN = #12; (* no    Mark this point in input as start of #. *)
  ropCLOSE = #13; (* no    Analogous to OPEN. *)
  ropBRACES = #14;
    {  n,m   Match this (simple) thing from n to m times. }
  ropSTARNG = #15; {        Same as START but in non-greedy mode  }
  ropPLUSNG = #16; {        Same as PLUS but in non-greedy mode }
  ropBRACESNG = #17;
    {        Same as BRACES but in non-greedy mode }
  ropLOOPENTRY = #18;
    {  node  Start of loop (LOOP - Node for this loop) }
  ropLOOP = #19; {  n,m   Back jump for LOOPENTRY. }
  ropLOOPNG = #20; {        Same as LOOP but in non-greedy mode }
  ropOPEN0 = #30;
    (* no    Mark this point in input as start of #n. *)
  ropOPEN9 = #39;
  ropCLOSE0 = #40; (* no    Analogous to OPEN0. *)
  ropCLOSE9 = #49;

  (*
 * Opcode notes:
 *
 * BRANCH   The set of branches constituting a single choice are hooked
 *    together with their "next" pointers, since precedence prevents
 *    anything being concatenated to any individual branch.  The
 *    "next" pointer of the last BRANCH in a choice points to the
 *    thing following the whole choice.  This is also where the
 *    final "next" pointer of each individual branch points; each
 *    branch starts with the operand node of a BRANCH node.
 *
 * BACK     Normal "next" pointers all implicitly point forward; BACK
 *    exists to make loop structures possible.
 *
 * STAR,PLUS   '?', and complex '*' and '+', are implemented as circular
 *    BRANCH structures using BACK.  Simple cases (one character
 *    per match) are implemented with STAR and PLUS for speed
 *    and to minimize recursive plunges.
 *
 * OPEN,CLOSE
 *
 *****************************************************************)

  (*****************************************************************
 *
 * The first byte of the regexp internal "program" is actually
 * this magic number; the start node begins in the second byte.
 *
 *****************************************************************)

const
  magic = #$9C; { 0234 }

  (*****************************************************************
 *
 * This is the maximum size of an compiled regexp program.
 * The limit is caused by size of "Next" pointer in the regexp
 * instruction which takes only two bytes.
 *
 *****************************************************************)

const
  MaxRegSize = 32768;

  (*****************************************************************
 *
 * The internal stack is introduced to allow compile '\:#' escape
 * sequences. The predefined sequence can be called recursively
 * in regular expression. There is already defined one recursive
 * call for '\:p' which uses '\:f' in their definition.
 *
 * The current stack size is two items, which is enough for '\:p'
 * (One for entry to subexpression and one for nested '\:f').
 *
 * 2001.10.03
 * The current stack is 10 due to user escapes '\:0'..'\:9' are
 * introduced. Any of them can call recursively other escape.
 *
 *****************************************************************)

const
  MaxRegExpStack = 10;

const
  ParseNumBaseMask = $00FF;
  ParseNumInteger = $0100;

const
  MaxBracesArg = Ord(High(Char));

type
  TRegExpStackItem = record
    Entry: integer;
    bol: PChar;
    Cur: PChar;
    eol: PChar;
    end;

  PRegExpStack = ^TRegExpStack;
  TRegExpStack = record
    Entries: integer;
    Entry: integer;
    FNPar: integer;
    Level: integer;
    Stack: array[0..MaxRegExpStack-1] of TRegExpStackItem;
    TagStarts: array[1..9] of PChar;
    TagEnds: array[1..9] of PChar;
    end;

  {$IFDEF DEBUG}

  (*****************************************************************
 *
 * FUNCTION:    RegProp
 *
 * PURPOSE:     This function returns printable representation of
 *              opcode.
 *
 * INPUTS:      Op - An opcode.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function RegProp(Op: PChar): String;
  var
    s: String;
  begin
    case Op[0] of
      ropBOL:
        s := 'BOL';
      ropEOL:
        s := 'EOL';
      ropANY:
        s := 'ANY';
      ropANYOF:
        s := 'ANYOF';
      ropANYBUT:
        s := 'ANYBUT';
      ropBRANCH:
        s := 'BRANCH';
      ropEXACTLY:
        s := 'EXACTLY';
      ropNOTHING:
        s := 'NOTHING';
      ropBACK:
        s := 'BACK';
      ropEND:
        s := 'END';
      ropOPEN:
        s := 'OPEN';
      ropCLOSE:
        s := 'CLOSE';
      ropSTAR:
        s := 'STAR';
      ropPLUS:
        s := 'PLUS';
      ropBRACES:
        s := 'BRACES';
      ropSTARNG:
        s := 'STARNG';
      ropPLUSNG:
        s := 'PLUSNG';
      ropBRACESNG:
        s := 'BRACESNG';
      ropLOOPENTRY:
        s := 'LOOPENTRY';
      ropLOOP:
        s := 'LOOP';
      ropLOOPNG:
        s := 'LOOPNG';
      else
        if Op[0] in [ropOPEN0..ropOPEN9] then
          begin
            Str(Ord(Op[0])-Ord(ropOPEN0), s);
            s := 'OPEN'+s;
          end
        else if Op[0] in [ropCLOSE0..ropCLOSE9] then
          begin
            Str(Ord(Op[0])-Ord(ropCLOSE0), s);
            s := 'CLOSE'+s;
          end
        else
          begin
            Str(Ord(Op[0]), s);
            s := '#'+s;
          end;
    end {case};
    RegProp := ':'+s;
  end { RegProp };

{$ENDIF DEBUG}

(*****************************************************************
 *
 * TRegExp
 *
 *****************************************************************)

Constructor TRegExp.Init;
  begin
    inherited Init;
    FCodeSize := 0;
    FCodeData := nil;
    FFlags := [];
    FStatus := resOK;
  end;

destructor TRegExp.Done;
  begin
    Reset;
    inherited Done;
  end;

procedure TRegExp.Reset;
  begin
    if FCodeSize > 0 then
      begin
        if FCodeData <> nil then
          FreeMem(FCodeData, FCodeSize);
        FCodeSize := 0;
        FCodeData := nil;
        Exclude(FFlags, ffCompiled);
      end;
    FStatus := resOK;
  end;

function TRegExp.CompileString(const AExpression: String): boolean;
  begin
    CompileString := Compile(@AExpression[1], Length(AExpression));
  end;

function TRegExp.CompileStr(AExpression: PChar): boolean;
  begin
    CompileStr := Compile(AExpression, StrLen(AExpression));
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.Compile
 *
 * PURPOSE:     This function compile a regular expression into
 *              internal code and invoke Error if required.
 *
 * INPUTS:      AExpression - A regular expspression string.
 *              ALength     - A length of the expression string.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     False - Error.
 *              True  - Successful.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.Compile(AExpression: PChar; ALength: integer):
    boolean;
  begin
    Compile := RegCompile(AExpression, ALength);
    if FStatus <> resOK then
      Error(FStatus);
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.Execute
 *
 * PURPOSE:     This function match a regexp against a string
 *              and invokes Error method if required.
 *
 * INPUTS:      AString - A source string.
 *              ALength - A string length.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - The given string match.
 *              False - The given string does not match.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.Execute(AString: PChar; ALength: integer): boolean;
  begin
    Execute := RegExecute(AString, ALength);
    if FStatus <> resOK then
      Error(FStatus);
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.Error
 *
 * PURPOSE:     This function is called is status is not resOK
 *              after compiling or executing regular expression.
 *
 * INPUTS:      AStatus - The status of an operation.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.Error(AStatus: TRegExpStatus);
  begin
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.CheckBreak
 *
 * PURPOSE:     This function is called periodically to check if
 *              if current searching is canceled.
 *
 * INPUTS:      None.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - Cancel the search.
 *              False - Continue search.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.CheckBreak: boolean;
  begin
    CheckBreak := False;
  end;

(*****************************************************************
 *
 * PROCEDURE:   Escape
 *
 * PURPOSE:     This procedure is called to obtain user defined
 *              escape sequence.
 *
 * INPUTS:      AChar   - A '#' character in '\:#' escape code.
 *              ASubExp - Buffer for an expression address.
 *              ALen    - Buffer for a length of the expression.
 *
 * OUTPUTS:     ASubExp - Buffer with address of the expression.
 *              ALen    - Buffer with length of the expression.
 *
 * NOTES:       1. If ASubExp is nil then the escape code is
 *                 invalid.
 *              2. If ALen is zero then expression is not defined.
 *              3. The method must be overide in derived objects
 *                 to provide its own escapes.
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.Escape(AChar: Char; var ASubExp: PChar; var ALen:
    integer);
  begin
    ASubExp := nil;
    ALen := 0;
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegCompile
 *
 * PURPOSE:     This function compile a regular expression into
 *              internal code.
 *
 * INPUTS:      AExpression - A regular expspression string.
 *              ALength     - A length of the expression string.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - Successful.
 *              False - Error.
 *
 * NOTES:       We can't allocate space until we know how big the
 *              compiled form will be, but we can't compile it
 *              (and thus know how big it is) until we've got a
 *              place to put the code.  So we cheat:  we compile
 *              it twice, once with code generation turned off and
 *              size counting turned on, and once "for real".
 *              This also means that we don't allocate space until
 *              we are sure that the thing really will compile
 *              successfully, and we never have to move the code
 *              and thus invalidate pointers into it.
 *              (Note that it has to be in one piece because free()
 *              must be able to free it all.)
 *
 *              Beware that the optimization-preparation code in
 *              here knows about some of the structure of the
 *              compiled regexp.
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegCompile(AExpression: PChar; ALength: integer):
    boolean;
  var
    Scan: PChar;
    len: integer;
    Flags: TRegExpFlags;
    P: Pointer;
    Dummy: Char;
    Stack: TRegExpStack;
  begin
    {$IFDEF DEBUG}
    FillChar(Stack, SizeOf(Stack), 0);
    {$ENDIF DEBUG}
    RegCompile := False;
    Reset;
    if AExpression = nil then
      begin
        FStatus := resNilArgument;
        exit;
      end;
    if ALength <= 0 then
      begin
        FStatus := resInvalidArgument;
        exit;
      end;
    FMust := @stack;
    FInputBol := AExpression;
    FInputEol := AExpression+ALength;
    (* First pass: determine size, legality. *)
    FInput := AExpression;
    FCodeSize := 0;
    FCodeData := @dummy; { must be non-nil }
    Stack.Entries := 0;
    Stack.Entry := 0;
    Stack.Level := 0;
    Stack.FNPar := 1;
    RegC(magic);
    FFlags := FFlags+[ffParsing, ffAutoTag];
    RegClearTags;
    P := Reg(0, Flags);
    Exclude(FFlags, ffParsing);
    if P = nil then
      begin
        FCodeSize := 0;
        FCodeData := nil;
        exit;
      end;
    (* Small enough for pointer-storage convention? *)
    if FCodeSize >= MaxRegSize then
      begin(* Probably could be 65535. *)
        FStatus := resRegExpTooBig;
        exit;
      end;
    (* Allocate space. *)
    P := MemAlloc(FCodeSize);
    if P = nil then
      begin
        FStatus := resOutOfSpace;
        exit;
      end;
    {$IFDEF DEBUG}
    FillChar(P^, FCodeSize, $FF);
    {$ENDIF DEBUG}
    (* Second pass: emit code. *)
    FInput := AExpression;
    FCodeData := PChar(P);
    Stack.Entries := 0;
    Stack.Entry := 0;
    Stack.Level := 0;
    Stack.FNPar := 1;
    RegC(magic);
    RegClearTags;
    Reg(0, Flags); { this should return with non-nil result }
    FCodeData := P;

    (* Dig out information for optimizations. *)
    FFlags := [];
    FMust := nil;
    Scan := @FCodeData[1]; (* First BRANCH. *)
    if RegNext(Scan)[0] = ropEND then
      begin
        (* Only one top-level choice. *)
        Scan := @scan[3];

        (* Starting-point info. *)
        if Scan[0] = ropEXACTLY then
          begin
            FStartCh := Scan[4];
            Include(FFlags, ffStart);
          end
        else if Scan[0] = ropBOL then
          begin
            Include(FFlags, ffAnchored);
          end;

        (*
     * If there's something expensive in the r.e., find the
     * longest literal string that must appear and make it the
     * regmust.  Resolve ties in favor of later strings, since
     * the regstart check works with the beginning of the r.e.
     * and avoiding duplication strengthens checking.  Not a
     * strong reason, but sufficient in the absence of others.
     *)
        if refSpStart in Flags then
          begin
            P := nil;
            len := 0;
            while Scan <> nil do
              begin
                if (Scan[0] = ropEXACTLY) and (Ord(Scan[3]) >= len)
                then
                  begin
                    P := @scan[3];
                    len := Ord(Scan[3]);
                  end;
                Scan := RegNext(Scan);
              end;
            FMust := PString(P);
          end;
      end;
    Include(FFlags, ffCompiled);
    RegCompile := True;
  end { TRegExp.RegCompile };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.IncRegSize
 *
 * PURPOSE:     This function is used to calculate size of compiled
 *              expression.
 *
 * INPUTS:      I - Size of next chunk of code.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.IncRegSize(i: integer);
  begin
    if FCodeSize <= MaxRegSize then
      Inc(FCodeSize, i);
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.Reg
 *
 * PURPOSE:     This function parses regular expression, i.e.
 *              main body or parenthesized thing.
 *
 * INPUTS:      Paren - 0    = main body;
 *                      -1   = parenthesized (auto-tag);
 *                      1..9 = tagged expression 1..9;
 *              FlagP - Buffer with flags.
 *
 * OUTPUTS:     FlagP - Buffer with flags.
 *
 * RETURNS:     nil  - If error.
 *              !nil - Address of RegExp byte code.
 *
 * NOTES:       1. Caller must absorb opening parenthesis.
 *
 *              2. Combining parenthesis handling with the base
 *                 level of regular expression is a trifle forced,
 *                 but the need to tie the tails of the branches
 *                 to what follows makes it hard to avoid.
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.Reg(Paren: integer; var FlagP: TRegExpFlags): PChar;
  var
    ret: PChar;
    br: PChar;
    ender: PChar;
    Flags: TRegExpFlags;
    parno: integer;
    Stack: PRegExpStack absolute FMust;
  begin
    Reg := nil;
    FlagP := [refHasWidth]; (* Tentatively. *)

    (* Make an OPEN node, if parenthesized. *)
    if Paren <> 0 then
      begin
        parno := Paren; { use selected tagged expression }
        if ffAutoTag in FFlags then
          begin
            if parno = -1 then
              begin
                parno := Stack^.FNPar;
                Inc(Stack^.FNPar);
              end
            else
              begin
                RegClearTags; { clear currently defined tags }
                Exclude(FFlags, ffAutoTag);
              end;
          end;
        if not (parno in [1..9]) then
          begin
            ret := RegNode(ropOPEN);
          end
        else
          begin
            ret := RegNode(Char(Ord(ropOPEN0)+parno));
            if Stack^.TagStarts[parno] <> nil then
              begin
                FStatus := resDuplicatedTaggedExp;
                exit;
              end;
            Stack^.TagStarts[parno] := FInput;
            FStartP[parno] := Stack^.Entry;
          end;
      end
    else
      begin
        ret := nil;
      end;

    (* Pick up the branches, linking them together. *)
    br := RegBranch(Flags);
    if br = nil then
      exit;
    if ret <> nil then
      RegTail(ret, br) (* OPEN -> first. *)
    else
      ret := br;
    if not (refHasWidth in Flags) then
      Exclude(FlagP, refHasWidth);
    FlagP := FlagP+(Flags*[refSpStart]);
    while (FInput < FInputEol) and (FInput[0] = '|') do
      begin
        Inc(FInput);
        RegLeave;
        br := RegBranch(Flags);
        if br = nil then
          exit;
        RegTail(ret, br); (* BRANCH -> BRANCH. *)
        if not (refHasWidth in Flags) then
          Exclude(FlagP, refHasWidth);
        FlagP := FlagP+(Flags*[refSpStart]);
      end;

    (* Make a closing node, and hook it on the end. *)
    if Paren <> 0 then
      begin
        if not (parno in [1..9]) then
          begin
            ender := RegNode(ropCLOSE)
          end
        else
          begin
            ender := RegNode(Char(Ord(ropCLOSE0)+parno));
            if Stack^.TagEnds[parno] <> nil then
              begin
                FStatus := resDuplicatedTaggedExp;
                exit;
              end;
            Stack^.TagEnds[parno] := FInput;
            FEndP[parno] := Stack^.Entry;
          end;
      end
    else
      begin
        ender := RegNode(ropEND);
      end;
    RegTail(ret, ender);

    (* Hook the tails of the branches to the closing node. *)
    br := ret;
    while br <> nil do
      begin
        RegOpTail(br, ender);
        br := RegNext(br);
      end;

    (* Check for proper termination. *)
    if Paren <> 0 then
      begin
        if (FInput >= FInputEol) or (FInput[0] <> ')') then
          begin
            FStatus := resUnmatchedParenthesis;
            exit;
          end;
        Inc(FInput);
        RegLeave;
      end
    else
      begin
        if FInput < FInputEol then
          begin
            if FInput[0] = ')' then
              FStatus := resUnmatchedParenthesis
            else(* "Can't happen". *)
              FStatus := resJunkOnEnd; (* NOTREACHED *)
            exit;
          end;
      end;

    Reg := ret;
  end { TRegExp.Reg };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegBranch
 *
 * PURPOSE:     This function implements the concatenation
 *              operator. It parses one alternative of an '|'
 *              operator.
 *
 * INPUTS:      FlagP - Buffer with flags.
 *
 * OUTPUTS:     FlagP - Buffer with flags.
 *
 * RETURNS:     nil  - If error.
 *              !nil - Address of RegExp byte code.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegBranch(var FlagP: TRegExpFlags): PChar;
  var
    ret: PChar;
    chain: PChar;
    latest: PChar;
    Flags: TRegExpFlags;
  begin
    RegBranch := nil;
    FlagP := []; (* Tentatively. *)

    ret := RegNode(ropBRANCH);
    chain := nil;
    while (FInput < FInputEol) and (FInput[0] <> '|') and (FInput[0] <>
        ')')
    do
      begin
        latest := RegPiece(Flags);
        if latest = nil then
          exit;
        FlagP := FlagP+(Flags*[refHasWidth]);
        if chain = nil then(* First piece. *)
          FlagP := FlagP+(Flags*[refSpStart])
        else
          RegTail(chain, latest);
        chain := latest;
      end;
    if chain = nil then(* Loop ran zero times. *)
      RegNode(ropNOTHING);

    RegBranch := ret;
  end { TRegExp.RegBranch };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegPiece
 *
 * PURPOSE:     This function parses something followed by
 *              possible '*', '+', '?' or '{n,m}' in both  maximal
 *              and minimal (followed by additional '?') variants.
 *
 * INPUTS:      FlagP - Buffer with flags.
 *
 * OUTPUTS:     FlagP - Buffer with flags.
 *
 * RETURNS:     nil  - If error.
 *              !nil - Address of RegExp byte code.
 *
 * NOTES:       The branching code sequences used for '?' and the
 *              general cases of '*' and '+' are somewhat optimized:
 *              they use the same NOTHING node as both the endmarker
 *              for their branch list and the body of the last branch.
 *              It might seem that this node could be dispensed with
 *              entirely, but the endmarker role is not redundant.
 *
 * HISTORY:
 *
 * Ver   Date       Description
 *
 * 0.11  2001.10.24 Added '{n,m}' syntax is based on REGEXPR.PAS
 *
 *****************************************************************)

function TRegExp.RegPiece(var FlagP: TRegExpFlags): PChar;
  label
    braces_error;
  var
    ret: PChar;
    Op: Char;
    ngop: boolean; { non-greedy operand }
    Next: PChar;
    Flags: TRegExpFlags;
    n: integer;
    M: integer;
    sinput: PChar;

  procedure EmitComplexBraces(AMin, AMax: integer; ANonGreedy:
      boolean);
    var
      rop: Char;
      R: PChar;
      off: integer;
      Next: PChar;
    begin
      rop := ropLOOP;
      if ANonGreedy then
        rop := ropLOOPNG;
      R := RegInsert(ropLOOPENTRY, ret, 0);
      Next := RegNode(rop);
      off := Next-R; { back to atom after LOOPENTRY }
      RegC(Chr((off shr 8) and 255));
      RegC(Chr(off and 255));
      RegC(Chr(AMin));
      RegC(Chr(AMax));
      RegTail(ret, Next); { LOOPENTRY -> LOOP }
      if R <> nil then
        RegTail(R, Next); { Atom -> LOOP }
    end { EmitComplexBraces };

  procedure EmitSimpleBraces(AMin, AMax: integer; ANonGreedy:
      boolean);
    var
      rop: Char;
      args: PChar;
    begin
      rop := ropBRACES;
      if ANonGreedy then
        rop := ropBRACESNG;
      args := RegInsert(rop, ret, 2);
      if args <> nil then
        begin
          ret[3] := Chr(AMin);
          ret[4] := Chr(AMax);
        end;
    end;

  begin { TRegExp.RegPiece }
    RegPiece := nil;
    ret := RegAtom(Flags);
    if ret = nil then
      exit;

    { remember the source               }
    { it will be restored on error exit }
    sinput := FInput;

    Op := #0;
    if FInput < FInputEol then
      begin
        Op := FInput[0];
        Inc(FInput);
      end;
    if not ((Op = '*') or (Op = '+') or (Op = '?') or (Op = '{'))
    then
      begin
        FInput := sinput;
        FlagP := Flags;
        RegPiece := ret;
        exit;
      end;

    if not (refHasWidth in Flags) and (Op <> '?') then
      begin
        FInput := sinput;
        FStatus := resStarPlusOperandCouldBeEmpty;
        exit;
      end;

    (* parse {n,m} statement for further use          *)
    {  'while' is used instead of 'if' - do not change }
    {  the pseudo-if statement ends with 'Break'       }
    {  after that is error handling code               }
    while Op = '{' do
      begin
        (* case {,m} *)
        if not RegLoadNumber(ParseNumInteger or 10, 5, n) then
          begin
            { assume the lowest limit }
            n := 0;
            { a comma must follow }
            if (FInput >= FInputEol) or (FInput[0] <> ',') then
              goto braces_error;
            Inc(FInput);
            if not RegLoadNumber(ParseNumInteger or 10, 5, M) then
              goto braces_error;
            (* case {n} or {n,} or {n,m} *)
          end
        else
          begin
            (* assume simple case {n} *)
            M := n;
            { eat a followed comma }
            if (FInput < FInputEol) and (FInput[0] = ',') then
              begin
                Inc(FInput);
                { assign maximum }
                if RegLoadNumber(ParseNumInteger or 10, 5, M) then
                    begin
                    if n > M then
                      goto braces_error;
                  end
                else
                  begin
                    M := -1; { temporary use for infinity }
                  end;
              end;
          end;
        (* at this stage '}' must follow *)
        if (FInput >= FInputEol) or (FInput[0] <> '}') or (M = 0) or (M
            > MaxBracesArg)
        then
          goto braces_error;
        Inc(FInput);
        { some optimisations }
        if M = -1 then
          begin
            if n = 0 then
              Op := '*'
            else if n = 1 then
              Op := '+'
            else
              M := MaxBracesArg;
              { convert infinity to a proper value }
          end;
        break; { skip error - exit loop }
braces_error:
        FInput := sinput;
        FStatus := resInvalidBraces;
        exit;
      end;

    ngop := False;
    if (FInput < FInputEol) and (FInput[0] = '?') then
      begin
        ngop := True;
        Inc(FInput);
      end;

    case Op of

      '*':
        begin
          FlagP := [refSpStart];
          if refSimple in Flags then
            begin
              if ngop then
                RegInsert(ropSTARNG, ret, 0)
              else
                RegInsert(ropSTAR, ret, 0);
            end
          else
            begin
              if ngop then
                begin
                  EmitComplexBraces(0, MaxBracesArg, True)
                end
              else
                begin
                  (* Emit x* as (x&|), where & means "self". *)
                  RegInsert(ropBRANCH, ret, 0); (* Either x *)
                  RegOpTail(ret, RegNode(ropBACK)); (* and loop *)
                  RegOpTail(ret, ret); (* back *)
                  RegTail(ret, RegNode(ropBRANCH)); (* or *)
                  RegTail(ret, RegNode(ropNOTHING)); (* null. *)
                end;
            end;
        end;

      '+':
        begin
          FlagP := [refSpStart, refHasWidth];
          if refSimple in Flags then
            begin
              if ngop then
                RegInsert(ropPLUSNG, ret, 0)
              else
                RegInsert(ropPLUS, ret, 0);
            end
          else
            begin
              if ngop then
                begin
                  EmitComplexBraces(1, MaxBracesArg, True)
                end
              else
                begin
                  (* Emit x+ as x(&|), where & means "self". *)
                  Next := RegNode(ropBRANCH); (* Either *)
                  RegTail(ret, Next);
                  RegTail(RegNode(ropBACK), ret); (* loop back *)
                  RegTail(Next, RegNode(ropBRANCH)); (* or *)
                  RegTail(ret, RegNode(ropNOTHING)); (* null. *)
                end;
            end;
        end;

      '?':
        begin
          if ngop then
            begin
              if refSimple in Flags then
                EmitSimpleBraces(0, 1, True)
              else
                EmitComplexBraces(0, 1, True);
            end
          else
            begin
              (* Emit x? as (x|) *)
              RegInsert(ropBRANCH, ret, 0); (* Either x *)
              RegTail(ret, RegNode(ropBRANCH)); (* or *)
              Next := RegNode(ropNOTHING); (* null. *)
              RegTail(ret, Next);
              RegOpTail(ret, Next);
            end;
        end;

      '{':
        begin
          if n > 0 then
            FlagP := [];
          if M > 0 then
            FlagP := FlagP+[refSpStart, refHasWidth];
          if refSimple in Flags then
            EmitSimpleBraces(n, M, ngop)
          else
            EmitComplexBraces(n, M, ngop);
        end;

    end {case};
    RegLeave;

    Op := #0;
    if FInput < FInputEol then
      Op := FInput[0];
    if (Op = '*') or (Op = '+') or (Op = '?') or (Op = '{') then
        begin
        FStatus := resNestedStarQuotePlus;
        exit;
      end;

    RegPiece := ret;
  end { TRegExp.RegPiece };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegAtom
 *
 * PURPOSE:     This function parses lowest level of regular
 *              expression.
 *
 * INPUTS:      FlagP - Buffer with flags.
 *
 * OUTPUTS:     FlagP - Buffer with flags.
 *
 * RETURNS:     nil  - If error.
 *              !nil - Address of RegExp byte code.
 *
 * NOTES:       Optimization:  gobbles an entire sequence of ordinary
 *              characters so that it can turn them into a single node,
 *              which is smaller to store and faster to run.
 *              Backslashed characters are exceptions, each becoming
 *              a separate node; the code is simpler that way and
 *              it's not worth fixing.
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegAtom(var FlagP: TRegExpFlags): PChar;
  label
    1, exactly;
  var
    ret: PChar;
    Flags: TRegExpFlags;
    Op: Char;
  begin
    RegAtom := nil; { initialize result }
    FlagP := []; (* Tentatively. *)
1: { restart '\:#' subexpresion }
    Op := FInput[0];
    Inc(FInput);
    case Op of

      '^':
        begin
          ret := RegNode(ropBOL);
        end;

      '$':
        begin
          ret := RegNode(ropEOL);
        end;

      '.':
        begin
          ret := RegNode(ropANY);
          FlagP := FlagP+[refHasWidth, refSimple];
        end;

      '[':
        begin
          ret := RegSet;
          if ret = nil then
            exit;
          FlagP := FlagP+[refHasWidth, refSimple];
        end;

      '(':
        begin
          if (FInput < FInputEol) and (FInput[0] = '?') then
            begin
              Inc(FInput);
              if FInput > FInputEol then
                begin
                  FStatus := resInvalidTaggedExp;
                  exit;
                end;
              Op := FInput[0];
              Inc(FInput);
              if Op = ':' then
                begin
                  ret := Reg(-2, Flags)
                end
              else if Op in ['1'..'9'] then
                begin
                  ret := Reg(Ord(Op)-Ord('0'), Flags)
                end
              else
                begin
                  FStatus := resInvalidTaggedExp;
                  exit;
                end;
            end
          else
            begin
              ret := Reg(-1, Flags);
            end;
          if ret = nil then
            exit;
          FlagP := FlagP+(Flags*[refHasWidth, refSpStart]);
        end;

      '|', ')':
        begin
          FStatus := resInternalUrp;
            (* Supposed to be caught earlier. *)
          exit;
        end;

      '?', '+', '*', '{':
        begin
          FStatus := resOperatorFollowsNothing;
          exit;
        end;

        else{ EXACTLY or PREDEFINED or TAGGED }
exactly:
          if (Op = '\') and (FInput < FInputEol) then
            begin
              Op := FInput[0];
              Inc(FInput);
              { predefined }
              if Op = ':' then
                begin
                  if FInput >= FInputEol then
                    begin
                      FStatus := resInvalidPredefinedExp;
                      exit;
                    end;
                  Op := FInput[0];
                  Inc(FInput);
                  if not RegEnter(Op, False) then
                    exit;
                  goto 1;
                end;
              { tagged }
              if Op in ['1'..'9'] then
                begin
                  if not RegEnter(Op, True) then
                    exit;
                  goto 1;
                end;
              { exactly }
              Dec(FInput);
            end;
      Dec(FInput);
      ret := RegExactly(FlagP);
      if ret = nil then
        exit;
    end {case};
    RegLeave;

    RegAtom := ret;
  end { TRegExp.RegAtom };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegEnter
 *
 * PURPOSE:     This function saves current expression parsing
 *              state and switches to the new one.
 *
 * INPUTS:      Op     - The escape code of the predefined
 *                       sequence.
 *              Tagged - Use tagged expression 'Op'.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     False - Invalid escape code or stack overflow.
 *              True  - Successful.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegEnter(Op: Char; Tagged: boolean): boolean;
  {$IFNDEF REGENTER_ASM}
  const
    eAlpha = '[A-Za-z0-9]';
    eBlanks = '([ \t]+)';
    eAlphabetic = '[A-Za-z]';
    eDigit = '[0-9]';
    eFilename = '([^\[\]\:\\/<>|=+;, \t]+)';
    eHex = '([0-9A-Fa-f]+)';
    eInteger = '([0-9]+)';
    eFloat = '(([0-9]+(\.[0-9]+|)|\.[0-9]+)([Ee](\+|-|)[0-9]+|))';
    ePath = '(([A-Za-z]:|)(\\|/|)(\:f(\\|/|))*\:f)';
    eQuotedStr = '(\"[^\"]*\")|''[^'']*'')';
    eCVariable = '([A-Za-z_$][A-Za-z0-9_$]*)';
    eWord = '([A-Za-z]+)';
    {$ENDIF !REGENTER_ASM}
  var
    Stack: PRegExpStack absolute FMust;
    P: PChar;
    l: integer;
  begin { TRegExp.RegEnter }
    RegEnter := False;
    P := nil;
    l := 0;
    if not Tagged then
      begin
        {$IFDEF REGENTER_ASM}
        { This peace of code is provided only to move constant strings from DATA }
        { segment (there is a limit to 64KB for that) to CODE segment (there is  }
        { also limit to 64KB but there can be more than one such a segment).     }
        asm
                MOV     AL,Op
                MOV     DX,OFFSET @@A
                CMP     AL,'a'
                JE      @@1
                MOV     DX,OFFSET @@B
                CMP     AL,'b'
                JE      @@1
                MOV     DX,OFFSET @@C
                CMP     AL,'c'
                JE      @@1
                MOV     DX,OFFSET @@D
                CMP     AL,'d'
                JE      @@1
                MOV     DX,OFFSET @@F
                CMP     AL,'f'
                JE      @@1
                MOV     DX,OFFSET @@H
                CMP     AL,'h'
                JE      @@1
                MOV     DX,OFFSET @@I
                CMP     AL,'i'
                JE      @@1
                MOV     DX,OFFSET @@N
                CMP     AL,'n'
                JE      @@1
                MOV     DX,OFFSET @@P
                CMP     AL,'p'
                JE      @@1
                MOV     DX,OFFSET @@Q
                CMP     AL,'q'
                JE      @@1
                MOV     DX,OFFSET @@V
                CMP     AL,'v'
                JE      @@1
                MOV     DX,OFFSET @@W
                CMP     AL,'w'
                JNE     @@2
        @@1:
                MOV     AX,CS
                MOV     WORD PTR p[0],DX
                MOV     WORD PTR p[2],CS
                JMP     @@2
        @@A:    DB      '[A-Za-z0-9]',0
        @@B:    DB      '([ \t]+)',0
        @@C:    DB      '[A-Za-z]',0
        @@D:    DB      '[0-9]',0
        @@F:    DB      '([^\[\]\:\\/<>|=+;, \t]+)',0
        @@H:    DB      '([0-9A-Fa-f]+)',0
        @@I:    DB      '([0-9]+)',0
        @@N:    DB      '(([0-9]+(\.[0-9]+|)|\.[0-9]+)([Ee](\+|-|)[0-9]+|))',0
        @@P:    DB      '(([A-Za-z]:|)(\\|/|)(\:f(\\|/|))*\:f)',0
        @@Q:    DB      '(\"[^\"]*\")|''[^'']*'')',0
        @@V:    DB      '([A-Za-z_$][A-Za-z0-9_$]*)',0
        @@W:    DB      '([A-Za-z]+)',0
        @@2:
    end;
  {$ELSE !REGENTER_ASM}
    case Op of
      'a': p := eAlpha;
      'b': p := eBlanks;
      'c': p := eAlphabetic;
      'd': p := eDigit;
      'f': p := eFilename;
      'h': p := eHex;
      'i': p := eInteger;
      'n': p := eFloat;
      'p': p := ePath;
      'q': p := eQuotedStr;
      'v': p := eCVariable;
      'w': p := eWord;
    end;
  {$ENDIF !REGENTER_ASM}
    if p = nil then begin
      Escape ( Op, p, l );
      if (p = nil) or (l < 0) then begin
        FStatus := resInvalidPredefinedExp;
        Exit;
      end;
      if l = 0 then begin
        FStatus := resUndefinedPredefinedExp;
        Exit;
      end;
    end;
    if l = 0 then
      l := StrLen ( p );
  end else begin
    l := Ord(op) - Ord('0');
    if (l < 1) or (l > 9) then begin
      FStatus := resInvalidTaggedExp;
      Exit;
    end;
    if (stack^.TagStarts[l] = nil)          { check if the start  }
    or (stack^.TagEnds[l] = nil)            { and the end
          }
        or (FStartP[l] <> FEndP[l])
      then
        begin{ have the same entry }
          FStatus := resInvalidTaggedExp;
          exit;
        end;
    P := Stack^.TagStarts[l];
    l := Stack^.TagEnds[l]-P;
  end { TRegExp.RegEnter };
with Stack^ do
  begin
    if Level >= MaxRegExpStack then
      begin
        FStatus := resStackOverflow;
        exit;
      end;
    Stack[Level].Entry := Entry;
    Stack[Level].Cur := FInput;
    Stack[Level].bol := FInputBol;
    Stack[Level].eol := FInputEol;
    Inc(Level);
    Inc(Entries);
  end;
FInput := P;
FInputBol := P;
FInputEol := P+l;
RegEnter := True;
end;

(*****************************************************************
 *
 * PROCEDURE:   TRegExp.RegLeave
 *
 * PURPOSE:     This function exits from nested expression entered
 *              previously via RegEnter.
 *
 * INPUTS:      None.
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.RegLeave;
  var
    Stack: PRegExpStack absolute FMust;
  begin
    with Stack^ do
      begin
        while (Level > 0) and (FInput = FInputEol) do
          begin
            Dec(Level);
            Entry := Stack[Level].Entry;
            FInput := Stack[Level].Cur;
            FInputBol := Stack[Level].bol;
            FInputEol := Stack[Level].eol;
          end;
      end;
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegLoadNumber
 *
 * PURPOSE:     This function parses a number of any base between
 *              2 and 16 and treats it as an character opcode.
 *
 * INPUTS:      Base    - A number between 2 and 16 + Flags.
 *              Digits  - Maximum number of digits.
 *              AResult - Buffer for result.
 *
 * OUTPUTS:     AResult - Buffer with result.
 *
 * RETURNS:     False - Error.
 *              True  - Successful.
 *
 * NOTES:       Flags:
 *
 *              ParseNumInteger - Return Integer instead of Char
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegLoadNumber(Base: integer; Digits: integer; var
    AResult): boolean;
  var
    V: integer;
    i: integer;
    j: integer;
    H: integer;
  begin
    RegLoadNumber := False;
    V := 0;
    i := 0;
    H := Ord(High(Char));
    if (Base and ParseNumInteger) <> 0 then
      H := High(integer);
    repeat
      if FInput >= FInputEol then
        exit;
      j := Pos(UpCase(FInput[0]), '0123456789ABCDEF');
      if (j = 0) or (j > (Base and $FF)) then
        break;
      V := V*(Base and ParseNumBaseMask)+j-1;
      Inc(FInput);
      Inc(i);
    until i >= Digits;
    if (i = 0) or (V > H) or (V < 0) then
      begin
        FStatus := resInvalidEscape;
        exit;
      end;
    if (Base and ParseNumInteger) = 0 then
      Char(AResult) := Chr(V)
    else
      integer(AResult) := V;
    RegLoadNumber := True;
  end { TRegExp.RegLoadNumber };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegEscape
 *
 * PURPOSE:     This function parses an escaped character.
 *
 * INPUTS:      AResult - Buffer for result.
 *
 * OUTPUTS:     AResult - Buffer with result.
 *
 * RETURNS:     False - Error.
 *              True  - Successful.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegEscape(var AResult: Char): boolean;
  var
    C: Char;
  begin
    RegEscape := False;
    if FInput >= FInputEol then
      begin
        FStatus := resTrailingBackSlash;
        exit;
      end;
    C := FInput[0];
    Inc(FInput);
    case C of
      'a':
        AResult := #7; { alarm/bell      }
      'b':
        AResult := #8; { backspace       }
      't':
        AResult := #9; { tabulator       }
      'n':
        AResult := #10; { line feed       }
      'v':
        AResult := #11; { vertical tab    }
      'f':
        AResult := #12; { form feed       }
      'r':
        AResult := #13; { carriage return }
      'x':
        if not RegLoadNumber(16, 2, AResult) then
          exit;
      'd':
        if not RegLoadNumber(10, 3, AResult) then
          exit;
      else
        AResult := C;
    end {case};
    RegEscape := True;
  end { TRegExp.RegEscape };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegSet
 *
 * PURPOSE:     This function parses a set expression.
 *
 * INPUTS:      None.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     non-nil - The new node.
 *              nil     - Error.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegSet: PChar;
  var
    ret: PChar;
    len: integer;
    Op: Char;
    opset: Char;
    charset: Set of Char;

  function ParseSet: boolean;
    var
      char1: Char;
      char2: Char;
      C: Char;
    begin
      ParseSet := False;
      if (FInput < FInputEol) and (FInput[0] = '^') then
        begin(* Complement of range. *)
          opset := ropANYBUT;
          Inc(FInput);
        end
      else
        begin
          opset := ropANYOF;
        end;
      charset := [];
      { Exception for '[]]' }
      if (FInput+1 < FInputEol) and (FInput[0] = ']') and (FInput[1] =
          ']')
      then
        begin
          ParseSet := True;
          Inc(FInput, 2);
          Include(charset, ']');
          exit;
        end;
      { standard parsing }
      while (FInput < FInputEol) and (FInput[0] <> ']') do
        begin
          char1 := FInput[0];
          Inc(FInput);
          if (char1 = '\') and not RegEscape(char1) then
            exit;
          if (FInput < FInputEol) and (FInput[0] = '-') then
            begin
              Inc(FInput);
              if (FInput >= FInputEol) or (FInput[0] = ']') then
                  begin
                  FStatus := resInvalidSetRange;
                  exit;
                end;
              char2 := FInput[0];
              Inc(FInput);
              if (char2 = '\') and not RegEscape(char2) then
                exit;
            end
          else
            begin
              char2 := char1;
            end;
          if char1 > char2 then
            begin
              FStatus := resInvalidSetRange;
              exit;
            end;
          for C := char1 to char2 do
            Include(charset, C);
        end;
      if (FInput >= FInputEol) or (FInput[0] <> ']') then
        begin
          FStatus := resUnmatchedSquareBracket;
          exit;
        end;
      Inc(FInput);
      { Exception for '[^]' }
      if (charset = []) and (opset = ropANYBUT) then
        begin
          opset := ropANYOF;
          Include(charset, '^');
        end;
      ParseSet := True;
    end { ParseSet: };

  begin { TRegExp.RegSet: }
    RegSet := nil;
    if not ParseSet then
      exit;
    len := 0;
    for Op := Low(Char) to High(Char) do
      begin
        if Op in charset then
          Inc(len);
      end;
    if ((len = 0) and (opset = ropANYOF))
      or ((len = 256) and (opset = ropANYBUT))
    then
      begin
        FStatus := resInvalidSetRange;
        exit;
      end;
    if ((len = 256) and (opset = ropANYOF))
      or ((len = 0) and (opset = ropANYBUT))
    then
      begin
        ret := RegNode(ropANY);
      end
    else
      begin
        ret := RegNode(opset);
        RegC(Chr(len));
        for Op := Low(Char) to High(Char) do
          begin
            if Op in charset then
              RegC(Op);
          end;
      end;
    RegSet := ret;
  end { TRegExp.RegSet: };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegExactly
 *
 * PURPOSE:     This function parses a directly defined string.
 *
 * INPUTS:      FlagP - Buffer with flags.
 *
 * OUTPUTS:     FlagP - Buffer with flags.
 *
 * RETURNS:     non-nil - The new node.
 *              nil     - Error.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegExactly(var FlagP: TRegExpFlags): PChar;
  var
    ret: PChar;
    len: integer;
    exactly: String;

  function ParseExactly: boolean;
    var
      C: Char;
      b0: PChar;
      B1: PChar;
    begin
      ParseExactly := False;
      exactly := '';
      B1 := nil;
      repeat
        b0 := B1;
        B1 := FInput;
        if B1 >= FInputEol then
          break;
        C := B1[0];
        if (C = '\') then
          begin
            if (B1+1 < FInputEol) and (B1[1] = ':') then
              break;
            Inc(FInput);
            if not RegEscape(C) then
              exit;
          end
        else
          begin
            { METACHARS }
            if C in ['^', '$', '.', '[', '(', ')', '|', '?', '+',
                '*', '{']
            then
              begin
                if (Length(exactly) > 1) and (C in ['*', '+', '?',
                    '{'])
                then
                  begin
                    (* Back off clear of ?+* operand. *)
                    Delete(exactly, Length(exactly), 1);
                    if b0 <> nil then
                      FInput := b0;
                  end;
                break;
              end;
            Inc(FInput);
          end;
        exactly := exactly+C;
      until Length(exactly) = Ord(High(Char));
      ParseExactly := True;
    end { ParseExactly: };

  begin { TRegExp.RegExactly }
    RegExactly := nil;
    if not ParseExactly then
      exit;
    Include(FlagP, refHasWidth);
    if Length(exactly) = 1 then
      Include(FlagP, refSimple);
    ret := RegNode(ropEXACTLY);
    for len := 0 to Length(exactly) do
      RegC(exactly[len]);
    RegExactly := ret;
  end { TRegExp.RegExactly };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegNode
 *
 * PURPOSE:     This function emits a node.
 *
 * INPUTS:      Op - Operand (byte code).
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     An address of a node in a byte code.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegNode(Op: Char): PChar;
  begin
    RegNode := FCodeData;
    if ffParsing in FFlags then
      begin
        IncRegSize(3);
        exit;
      end;
    FCodeData[0] := Op;
    FCodeData[1] := #0; (* Null "next" pointer. *)
    FCodeData[2] := #0;
    Inc(FCodeData, 3);
  end;

(*****************************************************************
 *
 * PROCEDURE:   TRegExp.RegC
 *
 * PURPOSE:     This procedure emits (if appropriate a byte of
 *              code.
 *
 * INPUTS:      B - A byte of code (argument of operand).
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.RegC(B: Char);
  begin
    if not (ffParsing in FFlags) then
      begin
        FCodeData[0] := B;
        Inc(FCodeData);
      end
    else
      begin
        IncRegSize(1);
      end;
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegInsert
 *
 * PURPOSE:     This procedure inserts an operator in front of
 *              already-emitted operand. Means relocating the
 *              operand.
 *
 * INPUTS:      Op    - An operator.
 *              Opnd  - Destination address.
 *              ASize - Size of auxiliary data.
 *
 * OUTPUTS:     None.
 *
 * RESULT:      Address of auxiliary buffer (ASize bytes)
 *              or next operand (if ASize = 0).
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegInsert(Op: Char; Opnd: PChar; ASize: integer):
    PChar;
  begin
    RegInsert := nil;

    if ffParsing in FFlags then
      begin
        IncRegSize(3+ASize);
        exit;
      end;

    Move(Opnd[0], Opnd[3+ASize], FCodeData-Opnd);
    Inc(FCodeData, 3+ASize);

    FillChar(Opnd[0], 3+ASize, 0);
    Opnd[0] := Op; (* Op node, where operand used to be. *)
    RegInsert := Opnd+3; { Reserved data or next operand }
  end;

(*****************************************************************
 *
 * PROCEDURE:   TRegExp.RegTail
 *
 * PURPOSE:     This procedure sets the next-pointer at the end
 *              of a node chain.
 *
 * INPUTS:      P   - The current node.
 *              Val - The target node.
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.RegTail(P: PChar; Val: PChar);
  var
    Scan: PChar;
    Temp: PChar;
    Offset: integer;
  begin
    if ffParsing in FFlags then
      exit;

    (* Find last node. *)
    Scan := P;
    while True do
      begin
        Temp := RegNext(Scan);
        if Temp = nil then
          break;
        Scan := Temp;
      end;

    if Scan[0] = ropBACK then
      Offset := Scan-Val
    else
      Offset := Val-Scan;
    Scan[1] := Chr((Offset shr 8) and 255);
    Scan[2] := Chr(Offset and 255);
  end { TRegExp.RegTail };

(*****************************************************************
 *
 * PROCEDURE:   TRegExp.RegOpTail
 *
 * PURPOSE:     This procedure perform RegTail on operand of first
 *              argument; nop if operandless.
 *
 * INPUTS:      P   - The current node.
 *              Val - The target node.
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.RegOpTail(P: PChar; Val: PChar);
  begin
    (* "Operandless" and "op != BRANCH" are synonymous in practice. *)
    if (ffParsing in FFlags) or (P = nil) or (P[0] <> ropBRANCH)
    then
      exit;
    RegTail(@P[3], Val);
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegExecute
 *
 * PURPOSE:     This function match a regexp against a string.
 *
 * INPUTS:      AString - A source string.
 *              ALength - A string length.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - The given string match.
 *              False - The given string does not match.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegExecute(AString: PChar; ALength: integer):
    boolean;
  var
    s: PChar;
    t: PChar;
  begin
    RegExecute := False;
    FStatus := resOK;
    Exclude(FFlags, ffBreak);

    { cannot execute if there was an error }
    if not (ffCompiled in FFlags) then
      begin
        FStatus := resNoExpression;
        exit;
      end;

    (* Be paranoid... *)
    if AString = nil then
      begin
        FStatus := resNilArgument;
        exit;
      end;
    if ALength = -1 then
      ALength := StrLen(AString);

    (* Check validity of program. *)
    if (FCodeData[0] <> magic) then
      begin
        FStatus := resCorruptedProgram;
        exit;
      end;

    (* If there is a "must appear" string, look for it. *)
    if (FMust <> nil) and (ALength >= Length(FMust^)) then
      begin
        s := AString;
        t := AString+ALength-Length(FMust^);
        repeat
          s := StrScan2(s, FMust^[1], t);
          if s = nil then
            exit; (* Not present. *)
          if StrLComp(s, @FMust^[1], Length(FMust^)) = 0 then
            break; (* Found it. *)
          Inc(s);
        until False;
      end;

    (* Remember the source *)
    FInputBol := AString;
    FInputEol := AString+ALength;

    (* Simplest case:  anchored match need be tried only once. *)
    if ffAnchored in FFlags then
      begin
        RegExecute := RegTry(AString);
        exit;
      end;

    (* Messy cases:  unanchored match. *)
    s := AString;
    t := AString+ALength;
    if (ffStart in FFlags) then
      begin
        (* We know what char it must start with. *)
        repeat
          s := StrScan2(s, FStartCh, t);
          if s = nil then
            exit; (* Not present. *)
          if RegTry(s) then
            begin
              RegExecute := True;
              exit;
            end;
          Inc(s);
        until False;
      end
    else
      begin
        (* We don't -- general case. *)
        repeat
          if RegTry(s) then
            begin
              RegExecute := True;
              exit;
            end;
          { Check the limit }
          if s >= t then
            exit;
          Inc(s);
        until False;
      end;
    (* Failure. *)
  end { TRegExp.RegExecute };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegTry
 *
 * PURPOSE:     This function tries match at specific point.
 *
 * INPUTS:      AString - The string to match.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     True  - Success.
 *              False - Failure.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegTry(AString: PChar): boolean;
  var
    i: integer;
  begin
    for i := 1 to 9 do
      begin
        FStartP[i] := -1;
        FEndP[i] := -1;
      end;
    FInput := AString;
    FLStackId := 0;
    if RegMatch(@FCodeData[1]) then
      begin
        FStart := AString-FInputBol;
        FLength := FInput-AString;
        RegTry := True;
        exit;
      end;
    RegTry := False;
  end;

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegMatch
 *
 * PURPOSE:     This function is a main matching routine.
 *
 * INPUTS:      Prog - Program (byte code) pointer.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     None.
 *
 * NOTES:       Conceptually the strategy is simple:  check to see
 *              whether the current node matches, call self
 *              recursively to see whether the rest matches,
 *              and then act accordingly.  In practice we make
 *              some effort to avoid recursion, in particular by
 *              going through "ordinary" nodes (that don't need to
 *              know whether the rest of the match failed) by a
 *              loop instead of by recursion.
 *
 * HISTORY:
 *
 * Ver   Date       Description
 *
 * 0.11  2001.10.24 Added '{n,m}' syntax is based on REGEXPR.PAS
 *
 *****************************************************************)

function TRegExp.RegMatch(Prog: PChar): boolean;
  var
    Scan: PChar; (* Current node. *)
    Next: PChar; (* Next node. *)
    No: integer; { EXACTLY, STAR, PLUS }
    Save: PChar; { EXACTLY, BRANCH, STAR, PLUS }
    Min: integer; { STAR, PLUS }
    Max: integer;
    nextch: Char; { STAR, PLUS }
    Opnd: PChar;

  function BracesMatch: boolean;
    var
      sloops: array[1..LoopStackMax] of integer;
        { :(( very bad for recursion }
      sloopi: integer;
    begin
      BracesMatch := False;
      if (ffMatchNext in FFlags) or (FInput[0] = nextch) then
        begin
          Move(FLStack, sloops, SizeOf(FLStack));
          sloopi := FLStackId;
          if RegMatch(Next) then
            begin
              RegMatch := True;
              BracesMatch := True;
              exit;
            end;
          Move(sloops, FLStack, SizeOf(FLStack));
          FLStackId := sloopi;
          if ffBreak in FFlags then
            BracesMatch := True;
        end;
    end { BracesMatch: };

  function LoopMatch: boolean;
    begin
      Inc(FLStack[FLStackId]);
      LoopMatch := False;
      No := FLStackId;
      if RegMatch(Opnd) then
        begin
          RegMatch := True;
          LoopMatch := True;
        end;
      FLStackId := No;
    end;

  begin { TRegExp.RegMatch }
    RegMatch := False;
    Scan := Prog;
    {$IFDEF DEBUG}
    if (Scan <> nil) and Narrate then
      Writeln(RegProp(Scan), '(');
    {$ENDIF DEBUG}
    while Scan <> nil do
      begin
        {$IFDEF DEBUG}
        if Narrate then
          Writeln(RegProp(Scan), '...');
        {$ENDIF DEBUG}
        if ffBreak in FFlags then
          exit;
        if CheckBreak then
          begin
            FStatus := resCanceled;
            Include(FFlags, ffBreak);
            exit;
          end;
        Next := RegNext(Scan);

        case Scan[0] of

          ropBOL:
            begin
              if FInput <> FInputBol then
                exit;
            end;

          ropEOL:
            begin{ should be checked for new-line }
              if FInput <> FInputEol then
                exit;
            end;

          ropANY:
            begin{ should be any except new-line }
              if FInput = FInputEol then
                exit;
              Inc(FInput);
            end;

          ropEXACTLY:
            begin
              Save := @scan[4];
              (* Inline the first character, for speed. *)
              if Save[0] <> FInput[0] then
                exit;
              No := Ord(Scan[3]);
              { input is too short }
              if FInput+No > FInputEol then
                exit;
              if (No > 1) and (StrLComp(Save, FInput, No) <> 0) then
                exit;
              Inc(FInput, No);
            end;

          ropANYOF:
            begin
              if FInput = FInputEol then
                exit;
              No := Ord(Scan[3]);
              if StrScan2(@scan[4], FInput[0], @scan[4+No]) = nil
              then
                exit;
              Inc(FInput);
            end;

          ropANYBUT:
            begin
              if FInput = FInputEol then
                exit;
              No := Ord(Scan[3]);
              if StrScan2(@scan[4], FInput[0], @scan[4+No]) <> nil
              then
                exit;
              Inc(FInput);
            end;

          ropNOTHING:
            begin
            end;

          ropBACK:
            begin
            end;

          ropOPEN:
            begin
              if RegMatch(Next) then
                RegMatch := True;
              exit;
            end;

          succ(ropOPEN0)..ropOPEN9:
            begin
              No := Ord(Scan[0])-Ord(ropOPEN0);
              Save := FInput;
              if RegMatch(Next) then
                begin
                  RegMatch := True;
                  { Don't set startp if some later invocation of the same }
                  { parentheses already has.                              }
                  if FStartP[No] = -1 then
                    FStartP[No] := Save-FInputBol;
                end;
              exit;
            end;

          ropCLOSE:
            begin
              if RegMatch(Next) then
                RegMatch := True;
              exit;
            end;

          succ(ropCLOSE0)..ropCLOSE9:
            begin
              No := Ord(Scan[0])-Ord(ropCLOSE0);
              Save := FInput;
              if RegMatch(Next) then
                begin
                  RegMatch := True;
                  { Don't set startp if some later invocation of the same }
                  { parentheses already has.                              }
                  if FEndP[No] = -1 then
                    FEndP[No] := Save-FInputBol;
                end;
              exit;
            end;

          ropBRANCH:
            begin
              if Next[0] <> ropBRANCH then
                begin(* No choice. *)
                  Next := @scan[3]; (* Avoid recursion. *)
                end
              else
                begin
                  repeat
                    Save := FInput;
                    if RegMatch(@scan[3]) then
                      begin
                        RegMatch := True;
                        exit;
                      end;
                    FInput := Save;
                    Scan := RegNext(Scan);
                  until (ffBreak in FFlags) or (Scan = nil) or (Scan[0]
                    <> ropBRANCH);
                  exit;
                  (* NOTREACHED *)
                end;
            end;

          ropLOOPENTRY:
            begin
              No := FLStackId;
              Inc(FLStackId);
              if FLStackId > LoopStackMax then
                begin
                  FStatus := resLoopStackExceeded;
                  exit;
                end;
              Save := FInput;
              FLStack[FLStackId] := 0; { init loop counter }
              if not RegMatch(Next) then{ execute loop      }
                FInput := Save;
              FLStackId := No; { cleanup           }
              exit;
            end;

          ropLOOP, ropLOOPNG:
            begin
              if FLStackId <= 0 then
                begin
                  FStatus := resLoopWithoutEntry;
                  exit;
                end;
              Opnd := Scan-(Ord(Scan[3])*256+Ord(Scan[4]));
                { back atom }
              Min := Ord(Scan[5]);
              Max := Ord(Scan[6]);
              Save := FInput;
              if FLStack[FLStackId] >= Min then
                begin
                  { Min alredy matched - we can work }
                  if Scan[0] = ropLOOP then
                    begin
                      { greedy way - first try to max deep of greed ;) }
                      if FLStack[FLStackId] < Max then
                        begin
                          if LoopMatch then
                            exit;
                          FInput := Save;
                        end;
                      { Fail. May be we are too greedy? ;) }
                      Dec(FLStackId);
                      if RegMatch(Next) then
                        begin
                          RegMatch := True;
                        end
                      else
                        begin
                          FInput := Save;
                        end;
                      exit;
                    end
                  else
                    begin
                      { non-greedy - try just now }
                      if RegMatch(Next) then
                        exit;
                      { failed - move next and try again }
                      FInput := Save;
                      if FLStack[FLStackId] < Max then
                        begin
                          if LoopMatch then
                            exit;
                          FInput := Save;
                        end;
                      { Failed - back up }
                      Dec(FLStackId);
                      exit;
                    end;
                end
              else
                begin
                  { first match a min_cnt times }
                  if LoopMatch then
                    exit;
                  Dec(FLStack[FLStackId]);
                  FInput := Save;
                  exit;
                end;
            end;

          ropSTAR, ropPLUS, ropBRACES, ropSTARNG, ropPLUSNG,
            ropBRACESNG:
            begin
              (*
         * Lookahead to avoid useless match attempts
         * when we know what character comes next.
         *)
              Include(FFlags, ffMatchNext);
              if Next[0] = ropEXACTLY then
                begin
                  nextch := Next[4];
                  Exclude(FFlags, ffMatchNext);
                end;
              Max := MaxInt; { infinite loop for * and + }
              if (Scan[0] = ropSTAR) or (Scan[0] = ropSTARNG) then
                Min := 0 { STAR }
              else if (Scan[0] = ropPLUS) or (Scan[0] = ropPLUSNG)
              then
                Min := 1 { PLUS }
              else
                begin{ BRACES }
                  Min := Ord(Scan[3]);
                  Max := Ord(Scan[4]);
                end;
              Save := FInput;
              Opnd := @scan[3];
              if (Scan[0] = ropBRACES) or (Scan[0] = ropBRACESNG)
              then
                Inc(Opnd, 2);
              if (Scan[0] = ropPLUSNG) or (Scan[0] = ropSTARNG) or (
                  Scan[0] = ropBRACESNG)
              then
                begin
                  { non-greedy mode }
                  Max := RegRepeat(Opnd, Max);
                    { don't repeat more than Max }
                  { Now we know real Max limit to move forward (for recursion 'back up') }
                  { In some cases it can be faster to check only Min positions first,    }
                  { but after that we have to check every position separtely instead     }
                  { of fast scannig in loop.                                             }
                  No := Min;
                  while No <= Min do
                    begin
                      FInput := Save+No;
                      { If it could work, try it. }
                      if BracesMatch then
                        exit;
                      { Couldn't or didn't - move forward. }
                      Inc(No);
                    end;
                end
              else
                begin
                  No := RegRepeat(Opnd, Max);
                    { don't repeat more than Max }
                  while No >= Min do
                    begin
                      { If it could work, try it. }
                      if BracesMatch then
                        exit;
                      (* Couldn't or didn't -- back up. *)
                      Dec(No);
                      FInput := Save+No;
                    end;
                end;
              exit;
            end;

          ropEND:
            begin
              RegMatch := True; (* Success! *)
              exit;
            end;

            else
            FStatus := resMemoryCorruption;
          exit;
        end {case};

        Scan := Next;
      end;

    (*
   * We get here only if there's trouble -- normally "case END" is
   * the terminating point.
   *)
    FStatus := resCorruptedPointers;
  end { TRegExp.RegMatch };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegRepeat
 *
 * PURPOSE:     This function repeatedly matches something simple
 *              and reports how many.
 *
 * INPUTS:      P    - Program (byte code) pointer.
 *              AMax - Maximum number or repetition.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     Number of repetitions.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegRepeat(P: PChar; AMax: integer): integer;
  var
    Count: integer;
    Scan: PChar;
    Opnd: PChar;
    opnde: PChar;
    Max: integer;
  begin
    Count := 0;
    Scan := FInput;
    if P[0] <> ropANY then
      begin
        Opnd := @P[4];
        opnde := Opnd+Ord(P[3]);
      end;
    Max := FInputEol-Scan;
    if Max > AMax then
      Max := AMax;
    case P[0] of

      ropANY:
        begin
          Count := Max;
          Inc(Scan, Max);
        end;

      ropEXACTLY:
        begin
          while (Count < Max) and (Opnd[0] = Scan[0]) do
            begin
              Inc(Count);
              Inc(Scan);
            end;
        end;

      ropANYOF:
        begin
          while (Count < Max) and (StrScan2(Opnd, Scan[0], opnde) <>
              nil)
          do
            begin
              Inc(Count);
              Inc(Scan);
            end;
        end;

      ropANYBUT:
        begin
          while (Count < Max) and (StrScan2(Opnd, Scan[0], opnde) =
              nil)
          do
            begin
              Inc(Count);
              Inc(Scan);
            end;
        end;

        else(* Oh dear.  Called inappropriately. *)
        FStatus := resInternalFoulup;
      Count := 0; (* Best compromise. *)
    end {case};
    FInput := Scan;

    RegRepeat := Count;
  end { TRegExp.RegRepeat };

(*****************************************************************
 *
 * FUNCTION:    TRegExp.RegNext
 *
 * PURPOSE:     This function digs the "next" pointer out of a
 *              node.
 *
 * INPUTS:      P - Program (byte code) pointer.
 *
 * OUTPUTS:     None.
 *
 * RETURNS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

function TRegExp.RegNext(P: PChar): PChar;
  var
    Offset: integer;
  begin
    RegNext := nil; { initialize result }

    if ffParsing in FFlags then
      exit;

    Offset := (((Ord(P[1]) and 255) shl 8)+(Ord(P[2]) and 255));
    if Offset = 0 then
      exit;

    if P[0] = ropBACK then
      RegNext := P-Offset
    else
      RegNext := P+Offset;
  end;

(*****************************************************************
 *
 * PROCEDURE:   TRegExp.RegClearTags
 *
 * PURPOSE:     This procedure sets all tags to 'uninitialized'.
 *
 * INPUTS:      None.
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.RegClearTags;
  var
    i: integer;
    Stack: PRegExpStack absolute FMust;
  begin
    for i := 1 to 9 do
      with Stack^ do
        begin
          TagStarts[i] := nil;
          TagEnds[i] := nil;
        end;
  end;

{$IFDEF DEBUG}

(*****************************************************************
 *
 * PROCEDURE:   Dump
 *
 * PURPOSE:     This procedure dumps a regular expression byte
 *              code onto standard output in vaguely comprehensible
 *              form.
 *
 * INPUTS:      None.
 *
 * OUTPUTS:     None.
 *
 * NOTES:
 *
 * HISTORY:
 *
 *****************************************************************)

procedure TRegExp.Dump;
  var
    s: PChar;
    Op: Char;
    Next: PChar;
    off: integer;
  begin
    if FCodeSize = 0 then
      exit;
    Op := ropEXACTLY; (* Arbitrary non-END op. *)

    s := @FCodeData[1];
    while Op <> ropEND do
      begin
        (* While that wasn't END last time... *)
        Op := s[0];
        off := s-FCodeData;
        Write(s-FCodeData: 4, RegProp(s)); (* Where, what. *)
        Next := RegNext(s);
        if Next = nil then(* Next ptr. *)
          Write('(0)')
        else
          Write('(', (s-FCodeData)+(Next-s), ')');
        Inc(s, 3);
        if (Op = ropANYOF) or (Op = ropANYBUT) or (Op = ropEXACTLY)
        then
          begin
            (* Literal string, where present. *)
            Write(PString(s)^);
            Inc(s, Length(PString(s)^)+1);
          end
        else if (Op = ropLOOP) or (Op = ropLOOPNG) then
          begin
            Write('[', off-(Ord(s[0])*256+Ord(s[1])), ']{', Ord(s[2]),
              ',', Ord(s[3]), '}');
            Inc(s, 4);
          end
        else if (Op = ropBRACES) or (Op = ropBRACESNG) then
          begin
            Write('{', Ord(s[0]), ',', Ord(s[1]), '}');
            Inc(s, 2);
          end;
        Writeln;
      end;

    (* Header fields of interest. *)
    if ffStart in FFlags then
      Write('start `', FStartCh, ''' ');
    if ffAnchored in FFlags then
      Write('anchored ');
    if FMust <> nil then
      Write('must have "', FMust^, '"');
    Writeln;
  end { TRegExp.Dump };

{$ENDIF DEBUG}

function TRegExp.SubstituteString(ASrc: PChar; const AReplace:
    String; var ADest: String): boolean;
  var
    len: integer;
    ret: boolean;
  begin
    SubstituteString := False;
    len := High(ADest);
    if Substitute(ASrc, @AReplace[1], Length(AReplace), @ADest[1],
        len)
    then
      begin
        ADest[0] := Chr(len);
        SubstituteString := True;
      end;
  end;

function TRegExp.SubstituteStr(ASrc, AReplace: PChar; ADest: PChar;
    var ALength: integer): boolean;
  begin
    SubstituteStr := Substitute(ASrc, AReplace, -1, ADest, ALength);
  end;

function TRegExp.Substitute(ASrc, AReplace: PChar; ARLen: integer;
    ADest: PChar; var ADLen: integer): boolean;
  begin
    Substitute := RegSub(ASrc, AReplace, ARLen, ADest, ADLen);
    if FStatus <> resOK then
      Error(FStatus);
  end;

function TRegExp.RegSub(ASrc, AReplace: PChar; ARLen: integer; ADest:
    PChar; var ADLen: integer): boolean;
  var
    j: integer;
    l: integer;
    C: Char;
    No: integer;
    len: integer;
    Start: integer;
  begin
    RegSub := False;
    FStatus := resOK;

    { cannot execute if there was an error }
    if not (ffCompiled in FFlags) then
      begin
        FStatus := resNoExpression;
        exit;
      end;

    (* Be paranoid... *)
    if AReplace = nil then
      begin
        FStatus := resNilArgument;
        exit;
      end;
    if ARLen = -1 then
      ARLen := StrLen(AReplace);

    { destination buffer must have valid size }
    if (ADest <> nil) and (ADLen <= 0) then
      begin
        FStatus := resInvalidArgument;
        exit;
      end;

    (* Check validity of program. *)
    if (FCodeData[0] <> magic) then
      begin
        FStatus := resCorruptedProgram;
        exit;
      end;

    FInput := AReplace;
    FInputEol := AReplace+ARLen;

    l := ADLen;
    j := 0;
    while FInput < FInputEol do
      begin
        C := FInput[0];
        Inc(FInput);
        No := -1;
        if C = '&' then
          begin
            No := 0;
          end
        else if (C = '\') then
          begin
            if (FInput < FInputEol) and (FInput[0] in ['0'..'9'])
            then
              begin
                No := Ord(FInput[0])-Ord('0');
                Inc(FInput);
              end
            else
              begin
                if not RegEscape(C) then
                  exit;
              end;
          end;
        if No < 0 then
          begin
            { ordinary character }
            if ADest <> nil then
              begin
                if j < l then
                  ADest[j] := C;
              end;
            Inc(j);
          end
        else
          begin
            { copy subexpression }
            Start := -1;
            len := 0;
            if No = 0 then
              begin
                { a whole expression }
                Start := FStart;
                len := FLength;
              end
            else if (FStartP[No] <> -1) and (FEndP[No] <> -1) then
                begin
                { a subexpression }
                Start := FStartP[No];
                len := FEndP[No]-Start;
              end;
            if (Start >= 0) and (len > 0) then
              begin
                if ADest <> nil then
                  begin
                    if j < l then
                      begin
                        No := len;
                        if j+len > l then
                          No := l-j;
                        Move(ASrc[Start], ADest[j], No);
                      end;
                  end;
                Inc(j, len);
              end;
          end;
      end;

    { return number of characters to replace }
    ADLen := j;

    RegSub := True;
  end { TRegExp.RegSub };

end.
