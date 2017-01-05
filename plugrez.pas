unit PlugRez;
(******

Access *.REZ files from plugins
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)

{$I STDEFINE.INC}

{Cat
   17/01/2002 - начало сооружения этого модуля
                Версия плагинов EventCatcher - 2.0
}

{$IFNDEF OS2}
{$IFNDEF WIN32}
{$ERROR Only OS2 or WIN32!}
{$ENDIF}
{$ENDIF}

interface

uses
  Objects;

function OpenRez(const PluginName: ShortString): longInt;
procedure CloseRez(RezId: longInt);
function GetRezString(RezId: longInt; ItemId: SmallWord): String;
function GetRezObject(RezId: longInt; ItemId: SmallWord): PObject;

implementation

uses
  advance, advance1, advance7;

type
  PPluginRezData = ^TPluginRezData;
  TPluginRezData = record
    Stream: PStream;
    StringsCount: SmallWord;
    ObjectsCount: SmallWord;
    Offset: array[0..0] of longInt;
    end;

function OpenRez(const PluginName: ShortString): longInt;
  const
    RezLabel = 'Dos Navigator /2 Plugin Resource File'#26;
  var
    P: PPluginRezData;
    s: PStream;
    Sc, OC: SmallWord;
    l: String;
    OffsetTableOffset: longInt;
  begin
    s := New(PBufStream, Init(SourceDir+PluginName+'\'+LngId+'.REZ',
      stOpenRead, 16384));
    s^.Read(l[1], Length(RezLabel));
    s^.Read(Sc, SizeOf(Sc));
    s^.Read(OC, SizeOf(OC));
    l[0] := Char(Length(RezLabel));
    if (s^.Status <> stOK) or (l <> RezLabel) or (MaxAvail < SizeOf(
        TPluginRezData)+(Sc+OC)*SizeOf(longInt))
    then
      begin
        Dispose(s, Done);
        OpenRez := 0;
        exit;
      end;

    GetMem(P, SizeOf(TPluginRezData)-SizeOf(longInt)+(Sc+OC)*SizeOf(
      longInt));
    with P^ do
      begin
        Stream := s;
        StringsCount := Sc;
        ObjectsCount := OC;
        s^.Read(OffsetTableOffset, SizeOf(OffsetTableOffset));
        s^.Seek(OffsetTableOffset);
        s^.Read(Offset, (Sc+OC)*SizeOf(longInt));
        if s^.Status <> stOK then
          begin
            Dispose(s, Done);
            FreeMem(P
              {, SizeOf(TPluginRezData)-SizeOf(LongInt)+(SC+OC)*SizeOf(LongInt)}
              );
            OpenRez := 0;
            exit;
          end;
      end;

    OpenRez := longInt(P);
  end { OpenRez };

procedure CloseRez(RezId: longInt);
  begin
    if RezId <> 0 then
      with PPluginRezData(RezId)^ do
        begin
          Dispose(Stream, Done);
          FreeMem(Pointer(RezId)
            {, SizeOf(TPluginRezData)-SizeOf(LongInt)+(StringsCount+ObjectsCount)*SizeOf(LongInt)}
            )
        end;
  end;

function GetRezString(RezId: longInt; ItemId: SmallWord): String;
  var
    s: String;
  begin
    with PPluginRezData(RezId)^, Stream^ do
      if ItemId >= StringsCount then
        GetRezString := '?'
      else
        begin
          Seek(Offset[ItemId]);
          ReadStrV(s);
          if (Status <> stOK) or (s = '') then
            GetRezString := '?'
          else
            GetRezString := s;
        end;
  end;

function GetRezObject(RezId: longInt; ItemId: SmallWord): PObject;
  var
    P: PObject;
  begin
    with PPluginRezData(RezId)^, Stream^ do
      if ItemId >= ObjectsCount then
        GetRezObject := nil
      else
        begin
          Seek(Offset[ItemId+StringsCount]);
          P := Get;
          if Status <> stOK then
            GetRezObject := nil
          else
            GetRezObject := P;
        end;
  end;

end.
