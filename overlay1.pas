{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.12
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

(* modified for supporting packed overlay 'overlay.pas' from BP 7.0 RTL *)

{*******************************************************}
{                                                       }
{       Turbo Pascal Runtime Library                    }
{       Overlay Interface Unit                          }
{                                                       }
{       Copyright (C) 1988,92 Borland International     }
{                                                       }
{*******************************************************}

unit Overlay1;

{.$I-,S-}

interface

const
  ovrOk = 0;
  ovrError = -1;
  ovrNotFound = -2;
  ovrNoMemory = -3;
  ovrIOError = -4;
  ovrNoEMSDriver = -5;
  ovrNoEMSMemory = -6;

const
  OvrResult: Integer = 0;
  OvrEmsPages: Word = 0;
  OvrTrapCount: Word = 0;
  OvrLoadCount: Word = 0;
  OvrFileMode: Byte = 0;

type
  OvrReadFunc = function(OvrSeg: Word): Integer;

var
  OvrReadBuf: OvrReadFunc;

procedure OvrInit(FileName: String);
procedure OvrInitEMS;
procedure OvrSetBuf(Size: LongInt);
function  OvrGetBuf: LongInt;
procedure OvrSetRetry(Size: LongInt);
function  OvrGetRetry: LongInt;
procedure OvrClearBuf;

implementation
uses
  PackF_RS;

{$L OVERLAY.OBJ}
{$L OVEREMS.OBJ}

const
  OvrRetrySize: Word = 0;
  OvrFileBase: Longint = 0;

procedure OvrInit(FileName: String); external;
procedure OvrInitEMS; external;
procedure OvrSetBuf(Size: LongInt); external;
function  OvrGetBuf: LongInt; external;
procedure OvrSetRetry(Size: LongInt); external;
function  OvrGetRetry: LongInt; external;
procedure OvrClearBuf; external;

procedure OverlayHalt;
begin
  RunError(209);
end;

end.
