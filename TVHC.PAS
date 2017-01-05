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
{-----------------------------------------------------}
{ Turbo Vision Help Compiler Unit                     }
{ Copyright (c) 1990 by Borland International         }
{ Portions Copyright (c) 1991-99 by RIT Research Labs }
{-----------------------------------------------------}

{===== TVHC ============================================================}
{  Turbo Vision help file compiler documentation.                       }
{=======================================================================}
{                                                                       }
{    Refer to DEMOHELP.TXT for an example of a help source file.        }
{                                                                       }
{    This program takes a help script and produces a help file (.HLP)   }
{    and a help context file (.PAS).  The format for the help file is   }
{    very simple.  Each context is given a symbolic name (i.e FileOpen) }
{    which is then put in the context file (i.e. hcFileOpen).  The text }
{    following the topic line is put into the help file.  Since the     }
{    help file can be resized, some of the text will need to be wrapped }
{    to fit into the window.  If a line of text is flush left with      }
{    no preceeding white space, the line will be wrapped.  All adjacent }
{    wrappable lines are wrapped as a paragraph.  If a line begins with }
{    a space it will not be wrapped. For example, the following is a    }
{    help topic for a File|Open menu item.                              }
{                                                                       }
{       |.topic FileOpen                                                }
{       |  File|Open                                                    }
{       |  ---------                                                    }
{       |This menu item will bring up a dialog...                       }
{                                                                       }
{    The "File|Open" will not be wrapped with the "----" line since     }
{    they both begin with a space, but the "This menu..." line will     }
{    be wrapped.                                                        }
{      The syntax for a ".topic" line is:                               }
{                                                                       }
{        .topic symbol[=number][, symbol[=number][...]]                 }
{                                                                       }
{    Note a topic can have multiple symbols that define it so that one  }
{    topic can be used by multiple contexts.  The number is optional    }
{    and will be the value of the hcXXX context in the context file     }
{    Once a number is assigned all following topic symbols will be      }
{    assigned numbers in sequence.  For example,                        }
{                                                                       }
{       .topic FileOpen=3, OpenFile, FFileOpen                          }
{                                                                       }
{    will produce the follwing help context number definitions,         }
{                                                                       }
{        hcFileOpen  = 3;                                               }
{        hcOpenFile  = 4;                                               }
{        hcFFileOpen = 5;                                               }
{                                                                       }
{    Cross references can be imbedded in the text of a help topic which }
{    allows the user to quickly access related topics.  The format for  }
{    a cross reference is as follows,                                   }
{                                                                       }
(*        {text[:alias]}                                               *)
{                                                                       }
{    The text in the brackets is highlighted by the help viewer.  This  }
{    text can be selected by the user and will take the user to the     }
{    topic by the name of the text.  Sometimes the text will not be     }
{    the same as a topic symbol.  In this case you can use the optional }
{    alias syntax.  The symbol you wish to use is placed after the text }
{    after a ':'. The following is a paragraph of text using cross      }
{    references,                                                        }
{                                                                       }
(*      |The {file open dialog:FileOpen} allows you specify which      *)
{       |file you wish to view.  If it also allow you to navigate       }
{       |directories.  To change to a given directory use the           }
(*      |{change directory dialog:ChDir}.                              *)
{                                                                       }
{    The user can tab or use the mouse to select more information about }
{    the "file open dialog" or the "change directory dialog". The help  }
{    compiler handles forward references so a topic need not be defined }
{    before it is referenced.  If a topic is referenced but not         }
{    defined, the compiler will give a warning but will still create a  }
{    useable help file.  If the undefined reference is used, a message  }
{    ("No help available...") will appear in the help window.           }
{=======================================================================}

program TVHC;

{$S-}

{$M 8192,8192,655360}

uses
  Lfn, {DataCompBoy}
  Drivers, Objects, Dos, Strings,
  HelpKern, advance, advance1, advance2, advance3, Collect, ObjType
  , Commands {Cat};
{$I Version.Inc}

{=======================================================================}

const
  HlpT: array[1..2] of TStreamRec = (
  (ObjType: otHelpTopic;
  VmtLink: (TypeOf(HelpKern.THelpTopic));
  Load: @HelpKern.THelpTopic.Load;
  Store: @HelpKern.THelpTopic.Store),
  (ObjType: otHelpIndex;
  VmtLink: (TypeOf(HelpKern.THelpIndex));
  Load: @HelpKern.THelpIndex.Load;
  Store: @HelpKern.THelpIndex.Store)
  );

  {======================= File Management ===============================}

procedure Error(text: String); forward;

type
  PProtectedStream = ^TProtectedStream;
  TProtectedStream = object(TBufStream)
    FileName: String; {DataCompBoy}
    Mode: word;
    Constructor Init(AFileName: String; AMode, ASize: word);
      {DataCompBoy}
    destructor Done; virtual;
    procedure Error(Code, Info: integer); virtual;
    end;

var
  TextStrm,
  SymbStrm: TProtectedStream;

const
  HelpStrm: PProtectedStream = nil;

Constructor TProtectedStream.Init(AFileName: String; AMode, ASize:
    word); {DataCompBoy}
  begin
    inherited Init(AFileName, AMode, ASize);
    FileName := AFileName;
    Mode := AMode;
  end;

{-DataCompBoy-}
destructor TProtectedStream.Done;
  var
    F: lFile;
  begin
    inherited Done;
    if (Mode = stCreate) and ((Status <> stOK) or (ExitCode <> 0))
    then
      begin
        lAssignFile(F, FileName);
        lEraseFile(F);
      end;
  end;
{-DataCompBoy-}

procedure TProtectedStream.Error(Code, Info: integer);
  begin
    case Code of
      stError:
        TVHC.Error('Error encountered in file '+FileName);
      stInitError:
        if Mode = stCreate then
          TVHC.Error('Could not create '+FileName)
        else
          TVHC.Error('Could not find '+FileName);
      stReadError:
        Status := Code; {EOF is "ok"}
      stWriteError:
        TVHC.Error('Disk full encountered writting file '+FileName);
      else
        TVHC.Error('Internal error.');
    end {case};
  end;

{----- UpStr(Str) ------------------------------------------------------}
{  Returns a string with Str uppercased.                                }
{-----------------------------------------------------------------------}

{-DataCompBoy-} {!Russian support!}
  {letter #241 (yo) NOT supported!}
function UpStr(Str: String): String;
  var
    i: AInt;
  begin
    for i := 1 to Length(Str) do
      case Str[i] of
        #97..#122, #160..#175:
          Str[i] := Char(byte(Str[i])-$20);
        #224..#239:
          Str[i] := Char(byte(Str[i])-$50);
      end {case};
    UpStr := Str;
  end;
{-DataCompBoy-}

{----- ReplaceExt(FileName, NExt, Force) -------------------------------}
{  Replace the extension of the given file with the given extension.    }
{  If the an extension already exists Force indicates if it should be   }
{  replaced anyway.                                                     }
{-----------------------------------------------------------------------}

{-DataCompBoy-}
function ReplaceExt(FileName: String; Next: String; Force: boolean):
    String;
  var
    Dir: String;
    Name: String;
    ext: String;
  begin
    lFSplit(FileName, Dir, Name, ext);
    if Force or (ext = '') then
      ReplaceExt := Dir+Name+Next
    else
      ReplaceExt := FileName;
  end;
{-DataCompBoy-}

{----- FExist(FileName) ------------------------------------------------}
{  Returns true if the file exists false otherwise.                     }
{-----------------------------------------------------------------------}

{-DataCompBoy-}
function FExists(FileName: String): boolean;
  var
    F: lFile;
    Attr: word;
  begin
    lAssignFile(F, FileName);
    lGetFAttr(F, Attr);
    FExists := DOSError = 0;
  end;
{-DataCompBoy-}

{======================== Line Management ==============================}

{----- GetLine(S) ------------------------------------------------------}
{  Return the next line out of the stream.                              }
{-----------------------------------------------------------------------}

const
  Line: String = '';
  LineInBuffer: boolean = False;
  Count: AInt = 0;

function GetLine(var s: TStream): String;
  label 1;
  var
    C, i: byte;
  begin
1:
    if s.Status <> stOK then
      begin
        GetLine := #26;
        exit;
      end;
    if not LineInBuffer then
      begin
        Line := '';
        C := 0;
        i := 0;
        while (Line[i] <> #13) and (i < 254) and (s.Status = stOK) do
          begin
            Inc(i);
            s.Read(Line[i], 1);
          end;
        Dec(i);
        s.Read(C, 1); { Skip #10 }
        SetLength(Line, i);
      end;
    Inc(Count);

    { Return a blank line if the line is a comment }
    if Line[1] = ';' then
      begin
        SetLength(Line, 0);
        goto 1;
      end;

    GetLine := Line;
    LineInBuffer := False;
  end { GetLine };

{----- UnGetLine(S) ----------------------------------------------------}
{  Return given line into the stream.                                   }
{-----------------------------------------------------------------------}

procedure UnGetLine(s: String);
  begin
    Line := s;
    LineInBuffer := True;
    Dec(Count);
  end;

{========================= Error routines ==============================}

{----- PrntMsg(Text) ---------------------------------------------------}
{  Used by Error and Warning to print the message.                      }
{-----------------------------------------------------------------------}

procedure PrntMsg(Pref: String; var text: String);
  const
    Blank: String[1] = '';
  var
    s: String;
    l: array[0..3] of longInt;
  begin
    l[0] := longInt(@Pref);
    if HelpStrm <> nil then
      l[1] := longInt(@HelpStrm^.FileName)
    else
      l[1] := longInt(@Blank);
    l[2] := Count;
    l[3] := longInt(@Text);
    if Count > 0 then
      FormatStr(s, '%s: %s(%d): %s'#13#10, l)
    else
      FormatStr(s, '%s: %s %3#%s', l);
    PrintStr(s);
  end;

{----- Error(Text) -----------------------------------------------------}
{  Used to indicate an error.  Terminates the program                   }
{-----------------------------------------------------------------------}

procedure Error(text: String);
  begin
    PrntMsg('Error', text);
    Halt(1);
  end;

{----- Warning(Text) ---------------------------------------------------}
{  Used to indicate an warning.                                         }
{-----------------------------------------------------------------------}

procedure Warning(text: String);
  begin
    PrntMsg('Warning', text);
  end;

{================ Built-in help context number managment ===============}

type
  TBuiltInContext = record
    text: PChar;
    Number: AWord;
    end;

  { A list of all the help contexts defined in APP }
const
  BuiltInContextTable: array[0..21] of TBuiltInContext = (
  (text: 'Cascade'; Number: $FF21),
  (text: 'ChangeDir'; Number: $FF06),
  (text: 'Clear'; Number: $FF14),
  (text: 'Close'; Number: $FF27),
  (text: 'CloseAll'; Number: $FF22),
  (text: 'Copy'; Number: $FF12),
  (text: 'Cut'; Number: $FF11),
  (text: 'DosShell'; Number: $FF07),
  (text: 'Dragging'; Number: 1),
  (text: 'Exit'; Number: $FF08),
  (text: 'New'; Number: $FF01),
  (text: 'Next'; Number: $FF25),
  (text: 'Open'; Number: $FF02),
  (text: 'Paste'; Number: $FF13),
  (text: 'Prev'; Number: $FF26),
  (text: 'Resize'; Number: $FF23),
  (text: 'Save'; Number: $FF03),
  (text: 'SaveAll'; Number: $FF05),
  (text: 'SaveAs'; Number: $FF04),
  (text: 'Tile'; Number: $FF20),
  (text: 'Undo'; Number: $FF10),
  (text: 'Zoom'; Number: $FF24)
  );

function IsBuiltInContext(text: String; var Number: AWord): boolean;
  var
    Hi, Lo, Mid, Cmp: AInt;
  begin
    { Convert Text into a #0 terminted PChar }
    SetLength(text, Length(text)+1);
    text[Length(text)] := #0;

    Hi := High(BuiltInContextTable);
    Lo := Low(BuiltInContextTable);
    while Lo <= Hi do
      begin
        Mid := (Hi+Lo) div 2;
        Cmp := StrComp(@Text[1], BuiltInContextTable[Mid].text);
        if Cmp > 0 then
          Lo := Mid+1
        else if Cmp < 0 then
          Hi := Mid-1
        else
          begin
            Number := BuiltInContextTable[Mid].Number;
            IsBuiltInContext := True;
            exit;
          end;
      end;
    IsBuiltInContext := False;
  end { IsBuiltInContext };

{====================== Topic Reference Management =====================}

type
  PFixUp = ^TFixUp;
  TFixUp = record
    Pos: longInt;
    Next: PFixUp;
    end;

  PReference = ^TReference;
  TReference = record
    Topic: PString;
    case Resolved: boolean of
      True: (Value: AWord);
      False: (FixUpList: PFixUp);
  end;

PRefTable = ^TRefTable;
TRefTable = object(TSortedCollection)
  function Compare(Key1, Key2: Pointer): integer; virtual;
  procedure FreeItem(Item: Pointer); virtual;
  function GetReference(var Topic: String): PReference;
  function KeyOf(Item: Pointer): Pointer; virtual;
  end;

const
  RefTable: PRefTable = nil;

procedure DisposeFixUps(P: PFixUp);
  var
    Q: PFixUp;
  begin
    while P <> nil do
      begin
        Q := P^.Next;
        Dispose(P);
        P := Q;
      end;
  end;

{----- TRefTable -------------------------------------------------------}
{  TRefTable is a collection of PReference's used as a symbol table.    }
{  If the topic has not been seen, a forward reference is inserted and  }
{  a fix-up list is started.  When the topic is seen all forward        }
{  references are resolved.  If the topic has been seen already the     }
{  value it has is used.                                                }
{-----------------------------------------------------------------------}

function TRefTable.Compare(Key1, Key2: Pointer): integer;
  var
    K1, k2: String;
  begin
    K1 := UpStr(PString(Key1)^);
    k2 := UpStr(PString(Key2)^);
    if K1 > k2 then
      Compare := 1
    else if K1 < k2 then
      Compare := -1
    else
      Compare := 0;
  end;

procedure TRefTable.FreeItem(Item: Pointer);
  var
    Ref: PReference absolute Item;
    P, Q: PFixUp;
  begin
    if not Ref^.Resolved then
      DisposeFixUps(Ref^.FixUpList);
    DisposeStr(Ref^.Topic);
    Dispose(Ref);
  end;

function TRefTable.GetReference(var Topic: String): PReference;
  var
    Ref: PReference;
    i: CondInt;
  begin
    if Search(@Topic, i) then
      Ref := At(i)
    else
      begin
        New(Ref);
        Ref^.Topic := NewStr(Topic);
        Ref^.Resolved := False;
        Ref^.FixUpList := nil;
        Insert(Ref);
      end;
    GetReference := Ref;
  end;

function TRefTable.KeyOf(Item: Pointer): Pointer;
  begin
    KeyOf := PReference(Item)^.Topic;
  end;

{----- InitRefTable ----------------------------------------------------}
{  Make sure the reference table is initialized.                        }
{-----------------------------------------------------------------------}

procedure InitRefTable;
  begin
    if RefTable = nil then
      RefTable := New(PRefTable, Init(5, 5));
  end;

{----- RecordReference -------------------------------------------------}
{  Record a reference to a topic to the given stream.  This routine     }
{  handles forward references.                                          }
{-----------------------------------------------------------------------}

procedure RecordReference(var Topic: String; var s: TStream);
  var
    i: AInt;
    Ref: PReference;
    FixUp: PFixUp;
  begin
    InitRefTable;
    Ref := RefTable^.GetReference(Topic);
    if Ref^.Resolved then
      s.Write(Ref^.Value, SizeOf(Ref^.Value))
    else
      begin
        New(FixUp);
        FixUp^.Pos := s.GetPos;
        i := -1;
        s.Write(i, SizeOf(i));
        FixUp^.Next := Ref^.FixUpList;
        Ref^.FixUpList := FixUp;
      end;
  end;

{----- ResolveReference ------------------------------------------------}
{  Resolve a reference to a topic to the given stream.  This routine    }
{  handles forward references.                                          }
{-----------------------------------------------------------------------}

procedure ResolveReference(var Topic: String; Value: AWord; var s:
    TStream);
  var
    Ref: PReference;

  procedure DoFixUps(P: PFixUp);
    var
      Pos: longInt;
    begin
      Pos := s.GetPos;
      while P <> nil do
        begin
          s.Seek(P^.Pos);
          s.Write(Value, SizeOf(Value));
          P := P^.Next;
        end;
      s.Seek(Pos);
    end;

  begin
    InitRefTable;
    Ref := RefTable^.GetReference(Topic);
    if Ref^.Resolved then
      Error('Redefinition of '+Ref^.Topic^)
    else
      begin
        DoFixUps(Ref^.FixUpList);
        DisposeFixUps(Ref^.FixUpList);
        Ref^.Resolved := True;
        Ref^.Value := Value;
      end;
  end { ResolveReference };

{======================== Help file parser =============================}

{----- GetWord ---------------------------------------------------------}
{   Extract the next word from the given line at offset I.              }
{-----------------------------------------------------------------------}

function GetWord(var Line: String; var i: AInt): String;
  var
    j: AInt;
  const
    {DataCompBoy} {!Russian support!}
      {letter #241 (yo) NOT supported!}
    WordChars = [#65..#90, #97..#122, #128..#175, #224..#239, #48..
      #57, #95];

  procedure SkipWhite;
    begin
      while (i <= Length(Line)) and (Line[i] = ' ') or (Line[i] = #9) do
          Inc(i);
    end;

  procedure SkipToNonWord;
    begin
      while (i <= Length(Line)) and (Line[i] in WordChars) do
        Inc(i);
    end;

  begin { GetWord }
    SkipWhite;
    j := i;
    if j > Length(Line) then
      GetWord := ''
    else
      begin
        Inc(i);
        if Line[j] in WordChars then
          SkipToNonWord;
        GetWord := Copy(Line, j, i-j);
      end;
  end { GetWord };

{----- TopicDefinition -------------------------------------------------}
{  Extracts the next topic definition from the given line at I.         }
{-----------------------------------------------------------------------}

type
  PTopicDefinition = ^TTopicDefinition;
  TTopicDefinition = object(TObject)
    Topic: PString;
    Value: AWord;
    Next: PTopicDefinition;
    Constructor Init(var ATopic: String; AValue: AWord);
    destructor Done; virtual;
    end;

Constructor TTopicDefinition.Init(var ATopic: String; AValue: AWord);
  begin
    Topic := NewStr(ATopic);
    Value := AValue;
    Next := nil;
  end;

destructor TTopicDefinition.Done;
  begin
    DisposeStr(Topic);
    if Next <> nil then
      Dispose(Next, Done);
  end;

function TopicDefinition(var Line: String; var i: AInt):
    PTopicDefinition;
  var
    j: AInt;
    k: integer;
    TopicDef: PTopicDefinition;
    Value: AWord;
    Topic, W: String;
    HelpNumber: AWord;
  const
    HelpCounter: AWord = 2; {1 is hcDragging}
  begin
    Topic := GetWord(Line, i);
    if Topic = '' then
      begin
        Error('Expected topic definition');
        TopicDefinition := nil;
      end
    else
      begin
        j := i;
        W := GetWord(Line, j);
        if W = '=' then
          begin
            i := j;
            W := GetWord(Line, i);
            Val(W, j, k);
            if k <> 0 then
              Error('Expected numeric')
            else
              begin
                HelpCounter := j;
                HelpNumber := j;
              end
          end
        else if not IsBuiltInContext(Topic, HelpNumber) then
          begin
            Inc(HelpCounter);
            HelpNumber := HelpCounter;
          end;
        TopicDefinition := New(PTopicDefinition, Init(Topic,
          HelpNumber));
      end;
  end { TopicDefinition };

{----- TopicDefinitionList----------------------------------------------}
{  Extracts a list of topic definitions from the given line at I.       }
{-----------------------------------------------------------------------}

function TopicDefinitionList(var Line: String; var i: AInt):
  PTopicDefinition;
  var
    j: AInt;
    W: String;
    TopicList, P: PTopicDefinition;
  begin
    j := i;
    TopicList := nil;
    repeat
      i := j;
      P := TopicDefinition(Line, i);
      if P = nil then
        begin
          if TopicList <> nil then
            Dispose(TopicList, Done);
          TopicList := nil;
          TopicDefinitionList := nil;
          exit;
        end;
      P^.Next := TopicList;
      TopicList := P;
      j := i;
      W := GetWord(Line, j);
    until W <> ',';
    TopicDefinitionList := TopicList;
  end { TopicDefinitionList };

{----- TopicHeader -----------------------------------------------------}
{  Parse a the Topic header                                             }
{-----------------------------------------------------------------------}

const
  CommandChar = '.';

function TopicHeader(var Line: String): PTopicDefinition;
  var
    i, j: AInt;
    W: String;
    TopicDef: PTopicDefinition;

  begin
    i := 1;
    W := GetWord(Line, i);
    if W <> CommandChar then
      begin
        TopicHeader := nil;
        exit;
      end;
    W := UpStr(GetWord(Line, i));
    if W = 'TOPIC' then
      TopicHeader := TopicDefinitionList(Line, i)
    else
      begin
        Error('TOPIC expected');
        TopicHeader := nil;
      end;
  end { TopicHeader };

{----- ReadParagraph ---------------------------------------------------}
{ Read a paragraph of the screen.  Returns the paragraph or nil if the  }
{ paragraph was not found in the given stream.  Searches for cross      }
{ references and updates the XRefs variable.                            }
{-----------------------------------------------------------------------}
type
  PCrossRefNode = ^TCrossRefNode;
  TCrossRefNode = record
    Topic: PString;
    Offset: AInt;
    Length: byte;
    Next: PCrossRefNode;
    end;
const
  BufferSize = 4096;
var
  Buffer: array[0..BufferSize-1] of byte;
  Ofs: AInt;

function ReadParagraph(var TextFile: TStream; var XRefs:
    PCrossRefNode;
  var Offset: AInt): PParagraph;
  var
    Line: String;
    State: (Undefined, Wrapping, NotWrapping);
    P: PParagraph;

  procedure CopyToBuffer(var Line: String; Wrapping: boolean);
      assembler;
    {$USES esi,edi,ecx} {$FRAME-}
  asm
    cld
    xor     eax,eax
    mov     edi,OFFSET Buffer
    mov      ax,Ofs
    add     edi,eax
    mov     esi,Line
    xor     eax,eax
    lodsb
    add     Ofs,ax
    xchg    eax,ecx
    rep     movsb
    xor     al,al
    test    Wrapping,1
    je      @@1
    mov     al,' '-13
  @@1:
    add     al,13
  @@2:
    stosb
    inc     Ofs
end
    ;

  procedure AddToBuffer(var Line: String; Wrapping: boolean);
    begin
      if Length(Line)+Ofs > BufferSize-1 then
        Error('Topic too large.');
      CopyToBuffer(Line, Wrapping);
    end;

  procedure ScanForCrossRefs(var Line: String);
    var
      i, BegPos, EndPos, Alias: AInt;
    const
      BegXRef = '{';
      EndXRef = '}';
      AliasCh = ':';

    procedure AddXRef(XRef: String; Offset: AInt; Length: byte);
      var
        P: PCrossRefNode;
        PP: ^PCrossRefNode;
      begin
        New(P);
        P^.Topic := NewStr(XRef);
        P^.Offset := Offset;
        P^.Length := Length;
        P^.Next := nil;
        PP := @XRefs;
        while PP^ <> nil do
          PP := @PP^^.Next;
        PP^:= P;
      end;

    procedure ReplaceSpacesWithFF(var Line: String; Start: AInt;
      Length: byte);
      var
        i: AInt;
      begin
        for i := Start to Start+Length do
          if Line[i] = ' ' then
            Line[i] := #$FF;
      end;

    begin { ScanForCrossRefs }
      i := 1;
      repeat
        BegPos := Pos(BegXRef, Copy(Line, i, MaxStringLength));
        if BegPos = 0 then
          i := 0
        else
          begin
            Inc(i, BegPos);
            if Line[i] = BegXRef then
              begin
                Delete(Line, i, 1);
                if Line[i] <> BegXRef then
                  continue;
              end;

            {------ Look for alias ------}

            { Alias := Pos(AliasCh, Copy(Line, I, 255)); }
            Alias := i-1;
            EndPos := -1;
            repeat
              Inc(Alias);
              if Alias > Length(Line) then
                break;
              if (Line[Alias] = EndXRef) then
                if (succ(Alias) <= Length(Line)) and
                  (Line[succ(Alias)] = EndXRef)
                then
                  begin
                    Delete(Line, Alias, 1);
                    continue;
                  end
                else
                  begin
                    EndPos := Alias-i+1;
                    Alias := 255;
                    break;
                  end;
              if Line[Alias] = AliasCh then
                if (succ(Alias) <= Length(Line)) and
                  (Line[succ(Alias)] = AliasCh)
                then
                  begin
                    Delete(Line, Alias, 1);
                    continue;
                  end
                else
                  break;
            until False;
            if Alias > Length(Line)
            then
              Alias := 0
            else
              Dec(Alias, i-1);

            {------- Look for EndPos -------}

            { EndPos := Pos(EndXRef, Copy(Line, I, 255)); }
            if EndPos = -1 then
              begin
                EndPos := i-1;
                if Alias > EndPos then
                  EndPos := Alias;
                repeat
                  Inc(EndPos);
                  if EndPos > Length(Line) then
                    break;
                  if Line[EndPos] = EndXRef then
                    if (succ(EndPos) <= Length(Line)) and
                      (Line[succ(EndPos)] = EndXRef)
                    then
                      begin
                        Delete(Line, EndPos, 1);
                        continue
                      end
                    else
                      break;
                until False;
                if EndPos > Length(Line)
                then
                  EndPos := 0
                else
                  Dec(EndPos, i-1);
              end;

            {------- Create XRef -------}

            if EndPos = 0 then
              begin
                Error('Unterminated topic reference.');
                Inc(i);
              end;

            if (Alias = 0) or (Alias > EndPos) then
              AddXRef(Copy(Line, i, EndPos-1), Offset+Ofs+i-1,
                EndPos-1)
            else
              begin
                AddXRef(Copy(Line, i+Alias, EndPos-Alias-1),
                Offset+Ofs+i-1, Alias-1);
                Delete(Line, i+Alias-1, EndPos-Alias);
                EndPos := Alias;
              end;
            ReplaceSpacesWithFF(Line, i, EndPos-1);
            Delete(Line, i+EndPos-1, 1);
            Delete(Line, i-1, 1);
            Inc(i, EndPos-2);
          end;
      until i = 0;
    end { ScanForCrossRefs };

  function IsEndParagraph: boolean;
    begin
      IsEndParagraph :=
      (Line = '') or
      (Line[1] = CommandChar) or
      (Line = #26) or
      ((Line[1] = ' ') and (State = Wrapping)) or
      ((Line[1] <> ' ') and (State = NotWrapping));
    end;

  function RemoveLeadSpaces(const s: String): String;
    var
      i: AInt;
    begin
      i := 1;
      while (i <= Length(s)) and (s[i] = ' ') do
        Inc(i);
      RemoveLeadSpaces := Copy(s, i, MaxStringLength);
    end;

  var
    CurrentTitle: String;

  begin { ReadParagraph }
    Ofs := 0;
    ReadParagraph := nil;
    State := Undefined;
    CurrentTitle := '';
    Line := GetLine(TextFile);

    while Line = '' do
      begin
        AddToBuffer(Line, State = Wrapping);
        Line := GetLine(TextFile);
      end;

    if Pos('.TITLE', RemoveLeadSpaces(UpStr(Line))) = 1 then
      begin
        Line := RemoveLeadSpaces(Copy(RemoveLeadSpaces(Line), 7,
          MaxStringLength));
        FreeStr := #218+Strg(#196, Length(Line)+2);
        AddToBuffer(FreeStr, False);
        FreeStr := #179#32+Line+#32#219;
        AddToBuffer(FreeStr, False);
        FreeStr := #192+Strg(#220, Length(Line)+2)+#219;
        AddToBuffer(FreeStr, False);
        FreeStr := '';
        AddToBuffer(FreeStr, False);
        Line := GetLine(TextFile);
      end
    else if IsEndParagraph then
      begin
        ReadParagraph := nil;
        UnGetLine(Line);
        exit;
      end;
    while not IsEndParagraph do
      begin
        if State = Undefined then
          if Line[1] = ' '
          then
            State := NotWrapping
          else
            State := Wrapping;
        if (State = NotWrapping) and (Line[1] = ' ')
        then
          Line := Copy(Line, 2, MaxStringLength);
        ScanForCrossRefs(Line);
        AddToBuffer(Line, State = Wrapping);
        Line := GetLine(TextFile);
      end;
    UnGetLine(Line);
    GetMem(P, SizeOf(P^)+Ofs);
    P^.Size := Ofs;
    P^.Wrap := State = Wrapping;
    Move(Buffer, P^.text, Ofs);
    Inc(Offset, Ofs);
    ReadParagraph := P;
  end { ReadParagraph };

{----- ReadTopic -------------------------------------------------------}
{ Read a topic from the source file and write it to the help file       }
{-----------------------------------------------------------------------}
var
  XRefs: PCrossRefNode;

procedure HandleCrossRefs(var s: TStream; XRefValue: AInt);
  var
    P: PCrossRefNode;
  begin
    P := XRefs;
    while XRefValue > 1 do
      begin
        if P <> nil then
          P := P^.Next;
        Dec(XRefValue);
      end;
    if P <> nil then
      RecordReference(P^.Topic^, s);
  end;

procedure ReadTopic(var TextFile: TStream; var HelpFile: THelpFile);
  var
    Line: String;
    P: PParagraph;
    Topic: PHelpTopic;
    TopicDef: PTopicDefinition;
    i, j, Offset: AInt;
    Ref: TCrossRef;
    RefNode: PCrossRefNode;

  procedure SkipBlankLines(var s: TStream);
    var
      Line: String;
    begin
      Line := '';
      while Line = '' do
        Line := GetLine(s);
      UnGetLine(Line);
    end;

  function XRefCount: AInt;
    var
      i: AInt;
      P: PCrossRefNode;
    begin
      i := 0;
      P := XRefs;
      while P <> nil do
        begin
          Inc(i);
          P := P^.Next;
        end;
      XRefCount := i;
    end;

  procedure DisposeXRefs(P: PCrossRefNode);
    var
      Q: PCrossRefNode;
    begin
      while P <> nil do
        begin
          Q := P;
          P := P^.Next;
          if Q^.Topic <> nil then
            DisposeStr(Q^.Topic);
          Dispose(Q);
        end;
    end;

  procedure RecordTopicDefinitions(P: PTopicDefinition);
    begin
      while P <> nil do
        begin
          ResolveReference(P^.Topic^, P^.Value, HelpFile.Stream^);
          HelpFile.RecordPositionInIndex(P^.Value);
          P := P^.Next;
        end;
    end;

  begin { ReadTopic }
    { Get Screen command }
    SkipBlankLines(TextFile);
    Line := GetLine(TextFile);

    TopicDef := TopicHeader(Line);

    Topic := New(PHelpTopic, Init);

    { Read paragraphs }
    XRefs := nil;
    Offset := 0;
    P := ReadParagraph(TextFile, XRefs, Offset);
    while P <> nil do
      begin
        Topic^.AddParagraph(P);
        P := ReadParagraph(TextFile, XRefs, Offset);
      end;

    i := XRefCount;
    Topic^.SetNumCrossRefs(i);
    RefNode := XRefs;
    for j := 1 to i do
      begin
        Ref.Offset := RefNode^.Offset;
        Ref.Length := RefNode^.Length;
        Ref.Ref := j;
        Topic^.SetCrossRef(j, Ref);
        RefNode := RefNode^.Next;
      end;

    RecordTopicDefinitions(TopicDef);

    CrossRefHandler := HandleCrossRefs;
    HelpFile.PutTopic(Topic);

    if Topic <> nil then
      Dispose(Topic, Done);
    Topic := nil;
    if TopicDef <> nil then
      Dispose(TopicDef, Done);
    TopicDef := nil;
    DisposeXRefs(XRefs);

    SkipBlankLines(TextFile);
  end { ReadTopic };

{----- WriteSymbFile ---------------------------------------------------}
{ Write the .PAS file containing all screen titles as constants.        }
{-----------------------------------------------------------------------}

procedure WriteSymbFile(var SymbFile: TProtectedStream);
  const
    HeaderText1 =
    'unit ';
    HeaderText2 =
    ';'#13#10+
    #13#10+
    'interface'#13#10+
    #13#10+
    'const'#13#10+
    #13#10;
    FooterText =
    #13#10+
    'implementation'#13#10+
    #13#10+
    'end.'#13#10;
    Header1: array[1..Length(HeaderText1)] of Char = HeaderText1;
    Header2: array[1..Length(HeaderText2)] of Char = HeaderText2;
    Footer: array[1..Length(FooterText)] of Char = FooterText;
  var
    i, Count: AInt;
    Dir: String; {DataCompBoy}
    Name: String; {DataCompBoy}
    ext: String; {DataCompBoy}
    Line: String;

  procedure DoWriteSymbol(P: PReference);
    var
      l: array[0..1] of longInt;
      i: AWord;
    begin
      if (P^.Resolved) then
        begin
          if not IsBuiltInContext(P^.Topic^, i) then
            begin
              l[0] := longInt(P^.Topic);
              l[1] := P^.Value;
              FormatStr(Line, ' hc%-20s = %5d;'#13#10, l);
              SymbFile.Write(Line[1], Length(Line));
            end
        end
      else
        Warning('Unresolved forward reference "'+P^.Topic^+'"');
    end;

  begin { WriteSymbFile }
    SymbFile.Write(Header1, SizeOf(Header1));
    lFSplit(SymbFile.FileName, Dir, Name, ext); {DataCompBoy}
    SymbFile.Write(Name[1], Length(Name));
    SymbFile.Write(Header2, SizeOf(Header2));

    RefTable^.ForEach(@DoWriteSymbol);

    SymbFile.Write(Footer, SizeOf(Footer));
  end { WriteSymbFile };

{----- ProcessText -----------------------------------------------------}
{ Compile the given stream, and output a help file.                     }
{-----------------------------------------------------------------------}

procedure ProcessText(var TextFile, HelpFile, SymbFile:
    TProtectedStream);
  var
    HelpRez: THelpFile;
  begin
    HelpRez.Init(@HelpFile);
    while TextFile.Status = stOK do
      ReadTopic(TextFile, HelpRez);
    WriteSymbFile(SymbFile);
    HelpRez.Done;
  end;

{========================== Program Block ==========================}

var
  TextName,
  HelpName,
  SymbName: String; {DataCompBoy}

procedure ExitClean;
  begin
    { Print a message if an out of memory error encountered }
    if ExitCode = 201 then
      begin
        Writeln('Error: Out of memory.');
        ErrorAddr := nil;
        ExitCode := 1;
      end;

    { Clean up files }
    TextStrm.Done;
    SymbStrm.Done;
  end;

begin
  { Banner messages }
  PrintStr(
    'Help Compiler  Version 1.1  Copyright (c) 1990 Borland International.'#13#10
    );
  if ParamCount < 1 then
    begin
      PrintStr(
      #13#10+
      '  Syntax:  TVHC <Help text>[.TXT] [<Help file>[.HLP] [<Symbol file>[.PAS]]'#13#10
        +
      #13#10+
      '     Help text   = Help file source'#13#10+
      '     Help file   = Compiled help file'#13#10+
      '     Symbol file = A Pascal file containing all the screen names as CONST''s'#13#10
        );
      Halt(0);
    end;

  {Cat}
  {/Cat}

  { Calculate file names }
  TextName := ReplaceExt(ParamStr(1), '.TXT', False);
  if not FExists(TextName) then
    Error('File "'+TextName+'" not found.');
  if ParamCount >= 2 then
    HelpName := ReplaceExt(ParamStr(2), '.HLP', False)
  else
    HelpName := ReplaceExt(TextName, '.HLP', True);
  if ParamCount >= 3 then
    SymbName := ReplaceExt(ParamStr(3), '.PAS', False)
  else
    SymbName := ReplaceExt(HelpName, '.PAS', True);

  ExitProc := @ExitClean;

  RegisterType(HlpT[1]);
  RegisterType(HlpT[2]);

  TextStrm.Init(TextName, stOpenRead, $8000);
  SymbStrm.Init(SymbName, stCreate, $8000);
  {-DataCompBoy-}
  if ParamStr(ParamCount) = '/4DN_OSP' then
    begin
      Line :=
        '{/////////////////////////////////////////////////////////////////////////'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//  Dos Navigator Open Source '+versionName+''#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  Based on Dos Navigator (C) 1991-99 RIT Research Labs'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  This programs is free for commercial and non-commercial use as long as'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//  the following conditions are aheared to.'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  Copyright remains RIT Research Labs, and as such any Copyright notices'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  in the code are not to be removed. If this package is used in a'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  product, RIT Research Labs should be given attribution as the RIT Research'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  Labs of the parts of the library used. This can be in the form of a textual'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  message at program startup or in documentation (online or textual)'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//  provided with the package.'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  Redistribution and use in source and binary forms, with or without'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  modification, are permitted provided that the following conditions are'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//  met:'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  1. Redistributions of source code must retain the copyright'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//     notice, this list of conditions and the following disclaimer.'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  2. Redistributions in binary form must reproduce the above copyright'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//     notice, this list of conditions and the following disclaimer in the'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//     documentation and/or other materials provided with the distribution.'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  3. All advertising materials mentioning features or use of this software'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//     must display the following acknowledgement:'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//     "Based on Dos Navigator by RIT Research Labs"'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  The licence and distribution terms for any publically available'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  version or derivative of this code cannot be changed. i.e. this code'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//  cannot simply be copied and put under another distribution licence'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//  (including the GNU Public Licence).'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line := '//'#13#10;
      SymbStrm.Write(Line[1], Length(Line));
      Line :=
        '//////////////////////////////////////////////////////////////////////////}'#13#10
        ;
      SymbStrm.Write(Line[1], Length(Line));
    end;
  {-DataCompBoy-}
  HelpStrm := New(PProtectedStream, Init(HelpName, stCreate, $8000));
  ProcessText(TextStrm, PProtectedStream(HelpStrm)^, SymbStrm);
end.
