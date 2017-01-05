{$I STDEFINE.INC} {Cat}

unit ErrMess;

interface
{AK155}
procedure MessFileNotOpen(const Path: String; rc: longInt);
procedure MessFileNotRename(const s1, s2: String; ErrCode: longInt);
procedure MessFileNotRead(const Path: String; rc: longInt);

implementation

uses
  DNApp, Messages, Commands, advance;

procedure MessFileNotOpen(const Path: String; rc: longInt);
  begin
    MessageBox(GetString(dlFBBNoOpen)+Path+^M^C'(RC=%d)',
    @RC, mfError+mfOKButton);
  end;

procedure MessFileNotRename(const s1, s2: String; ErrCode: longInt);
  begin
    MessageBox(GetString(dlFCNoRename1)+GetString(dlDIFile)
    +^M^C'(RC=%d)'+^M^C+Cut(s1, 60)
    +GetString(dlFCNoRename2)+Cut(s2, 60),
    @ErrCode, mfError+mfOKButton)
  end;

procedure MessFileNotRead(const Path: String; rc: longInt);
  begin
    MessageBox(GetString(dlFBBNotReadSource)+Path+^M^C'(RC=%d)',
    @RC, mfError+mfOKButton);
  end;

end.

