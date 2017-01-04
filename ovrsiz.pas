{
OVRSIZ generates a report describing an overlaid Turbo Pascal 5.0 program.

To use it, first compile the overlaid application to create both an EXE file
and a corresponding MAP file. OVRSIZ must read the first section of the MAP
file (the segment map) to get certain information. It also reads from the EXE
file to get detailed information about the overlays. It doesn't need to access
the OVR file.

Call OVRSIZ as follows:

  OVRSIZ [Options] ProgName [>Output]

OVRSIZ forces the extension 'EXE' onto ProgName to find the executable file,
and the extension 'MAP' onto ProgName to find the MAP file. The overlay report
is written to the standard output and may be redirected to a file or to the
printer. The only option at present is /Q, which stops OVRSIZ from writing
status messages while it works.

The OVRSIZ reports is largely self-explanatory. See OVRSIZ.DOC for more
information.

Written by Kim Kokkonen, TurboPower Software
Copyright (c) 1989, TurboPower Software. All rights reserved.
May be distributed freely, but not for a profit.

Version 1.0, 2/12/89
  Initial release.
Version 1.1  2/16/89
  Fix word overflow problem with overlay units approaching 64K
}

{$R-,S-,I-,V-,F-,B-}

program OvrSize;
  {-Determine unit sizes in overlaid TP5 EXE files}

uses
  Dos;

const
  Version = '1.1';                {Version number}
  MaxUnits = 255;                 {Maximum units in a program}
  OvrInstr : Word = $3FCD;        {INT 3Fh instruction}
  ShowStatus : Boolean = True;    {True to keep status running during operation}
  BufSize = 1024;                 {Size of text I/O buffer}
  NameSize = 15;                  {Maximum reported segment name length}

  Digits : array[0..$F] of Char = '0123456789ABCDEF';
  DosDelimSet : set of Char = ['\', ':', #0];

type
  Pathname = String[79];

  Long =
    record
      LowWord, HighWord : Word;
    end;

  ExeHeaderRec =                  {Information describing EXE file}
    record
      Signature : Word;           {EXE file signature}
      LengthRem : Word;           {Number of bytes in last page of EXE image}
      LengthPages : Word;         {Number of 512 byte pages in EXE image}
      NumReloc : Word;            {Number of relocation items}
      HeaderSize : Word;          {Number of paragraphs in EXE header}
      MinHeap : Word;             {Minimum extra paragraphs to allow}
      MaxHeap : Word;             {Paragraphs to keep beyond end of image}
      StackSeg : Word;            {Initial stack seg relative to image base}
      StackPtr : Word;            {Initial SP}
      CheckSum : Word;            {EXE file check sum, not used}
      IpInit : Word;              {Initial IP}
      CodeSeg : Word;             {Initial code seg relative to image base}
      RelocOfs : Word;            {Bytes into EXE for first relocation item}
      OverlayNum : Word;          {Overlay number, not used here}
    end;

  OverlayRecord =                 {At start of each overlay stub segment}
    record
      Sig : Word;
      Init : Word;
      FileOfs : LongInt;
      CodeSize : Word;
      FixupSize : Word;
      EntryPts : Word;
    end;

  UnitRecord =                    {Description of each unit in program}
    record
      SegClass : Word;            {0 for code, 1 for data, 2 for stack, 3 for heap}
      StatStart : LongInt;        {Start position within EXE image in bytes}
      StatLen : LongInt;          {Length of unit image in bytes}
      Name : String[NameSize];    {Name of the unit}
      Ovr : OverlayRecord;
    end;
  UnitArray = array[1..MaxUnits] of UnitRecord;


var
  Units : UnitArray;              {Describes all segments}
  UnitCount : Word;               {Number of units}
  Iname : Pathname;               {Input EXE file name}
  Mname : Pathname;               {Input MAP file name}
  EXEHeader : ExeHeaderRec;       {Header for input EXE file}
  InF : file;                     {Input EXE file}
  StdErr : Text;                  {Standard error device}
  Buffer : array[1..BufSize] of Char; {Text buffer for map file}

  procedure WriteCopyRight;
    {-Copyright notice in object code and on screen}
  begin                           {WriteCopyRight}
    WriteLn(StdErr, 'TP5 Overlay Size Analyzer, by TurboPower Software. Version ', Version);
  end;                            {WriteCopyRight}

  procedure OpenStdErr;
    {-Open standard error device}
  begin
    Assign(StdErr, '');
    Rewrite(StdErr);
    with TextRec(StdErr) do begin
      Handle := 2;
      BufSize := 1;
    end;
  end;

  procedure Error(Msg : String);
    {-Report error and halt}
  begin
    if Msg <> '' then
      WriteLn(Msg);
    Halt(1);
  end;

  procedure InvalidMapError;
    {-Common error}
  begin
    Error('Invalid MAP file format');
  end;

  procedure ErrorExeRead;
    {-Common error}
  begin
    Error('Error reading EXE file');
  end;

  function HexW(W : Word) : String;
    {-Return hex string for word}
  begin
    HexW[0] := #4;
    HexW[1] := Digits[Hi(W) shr 4];
    HexW[2] := Digits[Hi(W) and $F];
    HexW[3] := Digits[Lo(W) shr 4];
    HexW[4] := Digits[Lo(W) and $F];
  end;

  function HexL(L : LongInt) : String;
    {-Return hex string for LongInt}
  begin
    with Long(L) do
      HexL := HexW(HighWord)+HexW(LowWord);
  end;

  function Long2Str(L : LongInt) : String;
    {-Convert a long/word/integer/byte/shortint to a string}
  var
    S : String;
  begin
    Str(L, S);
    Long2Str := S;
  end;

  function StUpcase(S : String) : String;
    {-Return the uppercase of a string}
  var
    I : Integer;
  begin
    for I := 1 to Length(S) do
      S[I] := Upcase(S[I]);
    StUpcase := S;
  end;

  function Pad(S : String; Len : Byte) : String;
    {-Return a string right-padded to length len with ch}
  var
    O : String;
  begin
    if Length(S) >= Len then
      Pad := S
    else begin
      O[0] := Chr(Len);
      Move(S[1], O[1], Length(S));
      FillChar(O[Succ(Length(S))], Len-Length(S), ' ');
      Pad := O;
    end;
  end;

  function TrimLead(S : String) : String;
    {-Return a string with leading white space removed}
  begin
    while (Length(S) > 0) and (S[1] <= ' ') do
      Delete(S, 1, 1);
    TrimLead := S;
  end;

  function Trim(S : String) : String;
    {-Return a string with leading and trailing white space removed}
  begin
    while (Length(S) > 0) and (S[Length(S)] <= ' ') do
      Dec(S[0]);
    while (Length(S) > 0) and (S[1] <= ' ') do
      Delete(S, 1, 1);
    Trim := S;
  end;

  function HasExtension(Name : String; var DotPos : Word) : Boolean;
    {-Return whether and position of extension separator dot in a pathname}
  var
    I : Word;
  begin
    DotPos := 0;
    for I := Length(Name) downto 1 do
      if (Name[I] = '.') and (DotPos = 0) then
        DotPos := I;
    HasExtension := (DotPos > 0) and (Pos('\', Copy(Name, Succ(DotPos), 64)) = 0);
  end;

  function ForceExtension(Name, Ext : String) : String;
    {-Return a pathname with the specified extension attached}
  var
    DotPos : Word;
  begin
    if HasExtension(Name, DotPos) then
      ForceExtension := Copy(Name, 1, DotPos)+Ext
    else
      ForceExtension := Name+'.'+Ext;
  end;

  function BlkRead(var F : file; var Buffer; Size : Word) : Boolean;
    {-Convenient shell around BlockRead}
  var
    BytesRead : Word;
  begin
    BlockRead(F, Buffer, Size, BytesRead);
    BlkRead := (IoResult = 0) and (BytesRead = Size);
  end;

  function FileNewer(FileA, FileB : String) : Boolean;
    {-Return true if FileA is newer than FileB, both known to exist}
  var
    FA : file;
    FB : file;
    TA : LongInt;
    TB : LongInt;
  begin
    Assign(FA, FileA);
    Reset(FA);
    Assign(FB, FileB);
    Reset(FB);
    GetFtime(FA, TA);
    GetFtime(FB, TB);
    Close(FA);
    Close(FB);
    FileNewer := (TA > TB);
  end;

  procedure WriteHelp;
    {-Display help information and halt}
  begin
    WriteLn;
    WriteLn('Usage: OVRSIZ [Options] InputName [>OutputFile]');
    WriteLn;
    WriteLn('  OVRSIZ must read:');
    WriteLn('    InputName.EXE - overlaid executable file.');
    WriteLn('    InputName.MAP - symbol file for segment information.');
    WriteLn;
    WriteLn('Options:');
    WriteLn('  /Q    Quiet mode. No status output while processing.');
    Halt(1);
  end;

  function ExistFile(FName : String) : Boolean;
    {-Return true if file exists}
  var
    F : file;
  begin
    Assign(F, FName);
    Reset(F);
    if IoResult = 0 then begin
      ExistFile := True;
      Close(F);
    end else
      ExistFile := False;
  end;

  procedure ValidateInput;
    {-Get working filenames and assure files exist}
  var
    Iroot : Pathname;
    Arg : String;
    I : Integer;
  begin
    {Get parameters}
    Iroot := '';
    I := 1;
    while I <= ParamCount do begin
      Arg := StUpcase(ParamStr(I));
      if (Arg = '/Q') or (Arg = '-Q') then
        ShowStatus := False
      else if Iroot = '' then
        Iroot := Arg
      else
        Error('Too many filenames on command line');
      Inc(I);
    end;
    if (Iroot = '') then
      WriteHelp;

    {Build working filenames}
    Iname := ForceExtension(Iroot, 'EXE');
    Mname := ForceExtension(Iroot, 'MAP');

    {Make sure files are OK}
    if not ExistFile(Iname) then
      Error('EXE file '+Iname+' not found');
    if not ExistFile(Mname) then
      Error('MAP file '+Mname+' not found');
    if FileNewer(Iname, Mname) then
      Error('MAP file is older than EXE file');
  end;

  function GetLong(var S : String; var L : LongInt) : Boolean;
    {-Parse next longint out of line S}
  var
    Num : String[8];
    Code : Word;
  begin
    S := TrimLead(S);
    Num := '';
    while (Length(S) > 0) and (Pos(S[1], Digits) <> 0) do begin
      Num := Num+S[1];
      Delete(S, 1, 1);
    end;
    if Length(Num) = 0 then begin
      GetLong := False;
      Exit;
    end;
    if (Length(S) > 0) and (Upcase(S[1]) = 'H') then begin
      Num := '$'+Num;
      Delete(S, 1, 1);
    end;
    Val(Num, L, Code);
    GetLong := (Code = 0);
  end;

  function GetName(var S, Name : String) : Boolean;
    {-Parse next alphanumeric name from string s}
  begin
    S := TrimLead(S);
    Name := '';
    while (Length(S) > 0) and (S[1] > ' ') do begin
      if Length(Name) < NameSize then
        Name := Name+S[1];
      Delete(S, 1, 1);
    end;
    GetName := (Name <> '');
  end;

  function NextPara(Bytes : LongInt) : LongInt;
    {-Round up to next paragraph}
  begin
    NextPara := (Bytes+15) and $FFFFFFF0;
  end;

  procedure ParseMapFile(FName : String);
    {-Read and parse the MAP file, guaranteed to exist}
  var
    F : Text;
    S : String;
    SegType : String;
    Tlong : LongInt;
    ParseState : (Unknown, Segments, Done);
  begin

    {Open up the MAP file for reading}
    Assign(F, FName);
    SetTextBuf(F, Buffer, BufSize);
    Reset(F);
    if IoResult <> 0 then
      Error('Error opening '+FName);

    if ShowStatus then
      WriteLn(StdErr, 'Parsing MAP file');

    {Parse the segment description section only}
    UnitCount := 0;
    ParseState := Unknown;
    repeat
      ReadLn(F, S);
      if IoResult <> 0 then
        Error('Error reading '+FName);
      S := StUpcase(Trim(S));
      if S <> '' then
        if Pos('START', S) = 1 then
          ParseState := Segments
        else if Pos('ADDRESS', S) = 1 then
          ParseState := Done
        else if ParseState = Segments then begin
          {Parse the line to get the unit description}
          Inc(UnitCount);
          if UnitCount > MaxUnits then
            Error('Cannot exceed '+Long2Str(MaxUnits)+' segments');
          FillChar(Units[UnitCount], SizeOf(UnitRecord), 0);

          with Units[UnitCount] do begin

            {Get the position and size of the unit in the EXE image}
            if not GetLong(S, StatStart) then
              InvalidMapError;
            {Ignore the end of the segment}
            if not GetLong(S, Tlong) then
              InvalidMapError;
            {Get the length of the segment}
            if not GetLong(S, StatLen) then
              InvalidMapError;

            {Get the name of the segment}
            if not GetName(S, Name) then
              InvalidMapError;

            {Some segments are not really in the EXE file}
            if not GetName(S, SegType) then
              InvalidMapError;
            if SegType = 'CODE' then
              SegClass := 0
            else if SegType = 'DATA' then
              SegClass := 1
            else if SegType = 'STACK' then
              SegClass := 2
            else if SegType = 'HEAP' then
              SegClass := 3
            else
              SegClass := 4;
          end;
        end;
    until (ParseState = Done) or EoF(F);
    Close(F);
  end;

  procedure GetEXEInfo(FName : String);
    {-Open the EXE file, read its header, read the overlay information}
  var
    V : Integer;
  begin
    Assign(InF, FName);
    Reset(InF, 1);
    if IoResult <> 0 then
      Error('Error opening EXE file '+FName);

    if ShowStatus then
      WriteLn(StdErr, 'Reading EXE header information');

    {Read the existing EXE header}
    if not BlkRead(InF, EXEHeader, SizeOf(ExeHeaderRec)) then
      ErrorExeRead;

    with EXEHeader do begin
      {Assure it's a valid EXE file}
      if Signature <> $5A4D then
        Error('Invalid EXE format for '+FName);

      if ShowStatus then
        WriteLn(StdErr, 'Collecting overlay information');

      for V := 1 to UnitCount do
        with Units[V] do
          if (SegClass = 0) and (StatLen > SizeOf(OverlayRecord)) then begin
            {Get to right spot in old EXE file}
            Seek(InF, (LongInt(HeaderSize) shl 4)+StatStart);

            {Read the start of unit into the unit element}
            if not BlkRead(InF, Ovr, SizeOf(OverlayRecord)) then
              ErrorExeRead;
          end else
            FillChar(Ovr, SizeOf(OverlayRecord), 0);
    end;
    Close(InF);
  end;

  procedure WriteOvrInfo;
    {-Write information about the overlays}
  var
    V : Word;
    OverBuf : LongInt;
    OverSiz : LongInt;
    OverSizMax : LongInt;
    OverFixMax : LongInt;
    StatSiz : LongInt;
    DataSiz : LongInt;
    StakSiz : LongInt;
    StatTotal : LongInt;
  begin
    OverBuf := 00;
    OverFixMax := 00;
    OverSizMax := 00;
    StatSiz := 00;
    DataSiz := 00;
    StakSiz := 00;

    WriteLn;
    WriteLn('UNIT STATISTICS');
    WriteLn('                Static  Static  Overlay  Fixup   Entry   Overlay');
    WriteLn('Segment name   Segment    Size    Size    Size  Points   FilePos');
    WriteLn('==============  ======   =====   =====   =====   =====   =======');
    {        xxxxxxxxxxxxxxx 0FFFFh   ddddd   ddddd   ddddd   ddddd   0FFFFFh}

    for V := 1 to UnitCount do
      with Units[V], Ovr do
        if StatLen > 0 then begin
          {Segments are paragraph-aligned}
          StatLen := NextPara(StatLen);
          Write(Pad(Name, NameSize+1),
                '0', HexW(StatStart shr 4), 'h   ',
                StatLen:5, '   ');
          if Sig = OvrInstr then begin
            {Overlaid unit}
            {Segments are paragraph aligned}
            CodeSize := NextPara(CodeSize);
            FixupSize := NextPara(FixupSize);

            Write(CodeSize:5, '   ',
                  FixupSize:5, '   ',
                  EntryPts:5, '   ',
                  Copy(HexL(FileOfs), 3, 6), 'h');

            {Compute overlay buffer information}
            OverSiz := LongInt(CodeSize)+FixupSize;
            if OverSiz > OverBuf then
              OverBuf := OverSiz;
            if FixupSize > OverFixMax then
              OverFixMax := FixupSize;
            inc(OverSizMax, CodeSize);
            inc(StatSiz, StatLen);

          end else begin
            {Non-overlaid unit or other segment}
            Write('    -       -       -        -');
            case SegClass of
              0: inc(StatSiz, StatLen);
              1: inc(DataSiz, StatLen);
              2: inc(StakSiz, StatLen);
            end;
          end;
          WriteLn;
        end;

    StatTotal := StatSiz+DataSiz+StakSiz+256;
    WriteLn;
    WriteLn('Program segment prefix  ', 256:6);
    WriteLn('Static code size        ', StatSiz:6);
    WriteLn('Static data size        ', DataSiz:6);
    WriteLn('Stack size              ', StakSiz:6);
    WriteLn('Overlay buffer size     ', OverBuf:6, '..',
                                        (OverSizMax+OverFixMax):6);
    WriteLn('                        ==============');
    WriteLn('Total non-heap memory   ', StatTotal+OverBuf:6, '..',
                                        (StatTotal+OverSizMax+OverFixMax):6);
  end;

begin
  {Open standard error device}
  OpenStdErr;

  {Display copyright}
  WriteCopyRight;

  {Get filenames and assure they exist}
  ValidateInput;

  {Parse MAP file to get segment names and locations}
  ParseMapFile(Mname);

  {Read overlay information from EXE file}
  GetEXEInfo(Iname);

  {Write information}
  WriteOvrInfo;
end.