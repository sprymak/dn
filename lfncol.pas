{┌┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┐}
{├┼┼ Коллекция длинных имен, пpизванная понизить мозгожеpность ДH'а! ┼┼┤}
{├┼┼  Автоp: Антон Федоpов aka DataCompBoy                           ┼┼┤}
{├┼┼  Ваял в честь Матаpыкиной Ульяны...                             ┼┼┤}
{└┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┘}
unit LFNCol;

interface

uses Hash, Objects, Strings, ExtraMemory;

type
 TLFNIndex = pointer;

function    AddLFN(const Name: string): TLFNIndex;   {Добавляет имя}
function    AddCFN(Name: pchar):  TLFNIndex;         {Добавляет имя}
procedure   DelLFN(var ind: TLFNIndex);              {Удаляет имя}
function    UseLFN(ind: TLFNIndex):TLFNIndex;        {Добавляет пользователя именем}
function    GetLFN(ind: TLFNIndex): string;          {Возвpащает имя в String'е}
function    GetCFN(ind: TLFNIndex): pchar;           {Возвpащает имя в PChar'е}
procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);{Сохpаняет дл. имя}
function    LoadLFN(var S: TStream): TLFNIndex;      {Загpyжает его}

const HashRange = $0FFFF;

type
 PLFNRecord = ^TLFNRecord;
 TLFNRecord = record
               UsageCount: longint;
               Hash: word;
               Name: PChar;
              end;

Const CLFNRecMax = 65520 div sizeof(PLFNRecord);
Type  TLFNSRec   = array [0..CLFNRecMax] of PLFNRecord;
      PLFNSRec   = ^TLFNSRec;

type
 PLFNCollection = ^TLFNCollection;
 TLFNCollection = object(TSortedCollection)
                   constructor Init(AL, AD: Integer);          {Создает коллекцию}
                   function    AddLFN(const Name: string): TLFNIndex;
                                                               {Добавляет имя}
                   function    AddCFN(Name: pchar):  TLFNIndex;{Добавляет имя}
                   procedure   DelLFN(var ind: TLFNIndex);     {Удаляет имя}
                   function    UseLFN(ind: TLFNIndex):TLFNIndex;{Добавляет пользователя именем}
                   function    GetLFN(ind: TLFNIndex): string; {Возвpащает имя в String'е}
                   function    GetCFN(ind: TLFNIndex): pchar;  {Возвpащает имя в PChar'е}
                   procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);
                                                               {Сохpаняет дл. имя}
                   function    LoadLFN(var S: TStream): TLFNIndex;
                                                               {Загpyжает его}
                  private { Далее не интеpесно 9)) }
                   procedure   FreeItem(Item: pointer);               virtual;
                   Function    Search(Key: pointer;
                                      var Index: Integer): Boolean;   virtual;
                   function    Compare(p1, p2: pointer): integer;     virtual;
                  end;

type
 PSTMRec = ^TSTMRec;
 TSTMRec = record
            Size: Word;
            IsAvail: boolean;
           end;

type
 PEFNCollection = ^TEFNCollection; {LFNs in EMS}
 TEFNCollection = object(TObject)
                   constructor Init;                           {Создает коллекцию}
                   destructor  Done; virtual;                  {Уничтожает ее}
                   function    AddLFN(const Name: string): TLFNIndex;
                                                               {Добавляет имя}
                   function    AddCFN(Name: pchar):  TLFNIndex;{Добавляет имя}
                   procedure   DelLFN(var ind: TLFNIndex);     {Удаляет имя}
                   function    UseLFN(ind: TLFNIndex):TLFNIndex;{Добавляет пользователя именем}
                   function    GetLFN(ind: TLFNIndex): string; {Возвpащает имя в String'е}
                   function    GetCFN(ind: TLFNIndex): pchar;  {Возвpащает имя в PChar'е}
                   procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);
                                                               {Сохpаняет дл. имя}
                   function    LoadLFN(var S: TStream): TLFNIndex;
                                                               {Загpyжает его}
                  private { Далее не интеpесно 9)) }
                   stream:     PStream;
                   procedure   FreeItem(Item: pointer);               virtual;
                   Function    Search(Key: pointer; var Index: LongInt;
                                      var Size: word): Boolean;       virtual;
                  end;

type
 PXFNCollection = ^TXFNCollection; {LFNs in XMS}
 TXFNCollection = object(TEFNCollection)
                   constructor Init;                           {Создает коллекцию}
                  end;

implementation

 constructor TLFNCollection.Init;
 begin inherited Init(AL, AD); Duplicates:=true; end;

 procedure   TLFNCollection.FreeItem(Item: pointer);
 begin
  if Item=nil then exit;
  if PLFNRecord(Item)^.Name<>nil then StrDispose(PLFNRecord(Item)^.Name);
  Dispose((PLFNRecord(Item)));
 end;

 function    TLFNCollection.Search(Key: pointer; var Index: Integer): Boolean;
 var L, H, I, C: Integer;
 begin
  Search := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(KeyOf(Items^[I]), Key);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Search := True;
        if not Duplicates then L := I;
      end;
    end;
  end;
  Index := L;
 end;

 function    TLFNCollection.Compare(p1, p2: pointer): integer;
 begin
  if (PLFNRecord(p1)^.hash<PLFNRecord(p2)^.hash) then Compare:=-1 else
   if (PLFNRecord(p1)^.hash>PLFNRecord(p2)^.hash) then Compare:=1 else
    Compare:=StrComp(PLFNRecord(p1)^.Name, PLFNRecord(p2)^.Name);
 end;

 function    TLFNCollection.AddLFN(const Name: string): TLFNIndex;
 var l: byte; z: byte;
 begin
  l:=length(Name); z:=0;
  move(Name[1], (@(Name[0]))^, l);
  move(z, (@(Name[l]))^, 1);
  AddLFN:=AddCFN(@Name[0]);
  move(Name[0], (@(Name[1]))^, l);
  move(l, (@(Name[0]))^, 1);
 end;

 function    TLFNCollection.AddCFN(Name: pchar):  TLFNIndex;
 var plr: PLFNRecord; i: integer;
 begin
  New(plr);
  plr^.UsageCount:=1;
  plr^.Hash:=HashString(Name, StrLen(Name), HashRange);
  plr^.Name:=StrNew(Name);
  if Search(plr, i) then
   begin
    inc(PLFNRecord(At(I))^.UsageCount);
    FreeItem(plr);
    AddCFN:=At(i);
   end
  else
   begin
    Insert(plr);
    AddCFN:=plr;
   end;
 end;

 procedure   TLFNCollection.DelLFN(var ind: TLFNIndex);
 var i: integer;
 begin
  if ind=nil then exit;
  Dec(PLFNRecord(ind)^.UsageCount);
  if PLFNRecord(ind)^.UsageCount=0
   then if Search(ind, i) then AtFree(i) else FreeItem(ind);
  If Limit-Count>Delta then SetLimit((Count div Delta)*Delta);
  ind:=nil;
 end;

 function    TLFNCollection.UseLFN(ind: TLFNIndex): TLFNIndex;
 begin
  UseLFN:=ind;
  if ind<>nil then inc(PLFNRecord(ind)^.UsageCount);
 end;

 function    TLFNCollection.GetLFN(ind: TLFNIndex): string;
 begin if ind<>nil then GetLFN:=StrPas(PLFNRecord(ind)^.Name); end;

 function    TLFNCollection.GetCFN(ind: TLFNIndex): pchar;
 begin if ind<>nil then GetCFN:=PLFNRecord(ind)^.Name; end;

 procedure   TLFNCollection.StoreLFN(var S: TStream; LFN: TLFNIndex);
 begin S.StrWrite(PLFNRecord(LFN)^.Name); end;

 function    TLFNCollection.LoadLFN(var S: TStream): TLFNIndex;
 var pc: pchar;
 begin
  pc:=S.StrRead;
  LoadLFN:=AddCFN(pc);
  StrDispose(pc);
 end;

 constructor TEFNCollection.Init;
 var hdr: TSTMRec;
 begin
  inherited Init;
  Stream := new(PEMSStream, Init($100000, $1000000));
  hdr.size:=0;
  stream^.write(hdr, sizeof(hdr));
 end;

 destructor  TEFNCollection.Done;
 begin
  dispose(Stream, done);
  inherited done;
 end;

 procedure   TEFNCollection.FreeItem(Item: pointer);
 var hdr: TSTMRec;
 begin
  if longint(Item)=$0FFFFFFFF then exit;
  stream^.status:=stOk;
  stream^.Seek(longint(Item));
  stream^.Read(hdr, sizeof(hdr));
  hdr.IsAvail:=true;
  stream^.Seek(longint(Item));
  stream^.Write(hdr, sizeof(hdr));
 end;

 function    TEFNCollection.Search(Key: pointer; var Index: Longint; var Size: Word): Boolean;
 var hdr: TSTMRec;
 begin
  Search := False;
  Index := 0;
  stream^.status:=stOk;
  repeat
   stream^.seek(Index);
   stream^.read(hdr, sizeof(hdr));
   if (hdr.IsAvail) and (hdr.size>=Size)
    then begin
          Search:=true;
          Size:=hdr.Size;
          exit;
         end;
   inc(Index, hdr.size+SizeOf(TSTMRec));
  until hdr.size=0;
  dec(Index, SizeOf(TSTMRec));
 end;

 function    TEFNCollection.AddLFN(const Name: string): TLFNIndex;
 var l: byte; z: byte;
 begin
  l:=length(Name); z:=0;
  move(Name[1], (@(Name[0]))^, l);
  move(z, (@(Name[l]))^, 1);
  AddLFN:=AddCFN(@Name[0]);
  move(Name[0], (@(Name[1]))^, l);
  move(l, (@(Name[0]))^, 1);
 end;

 function    TEFNCollection.AddCFN(Name: pchar):  TLFNIndex;
 var plr: TLFNIndex;
     hdr: TSTMRec;
     b: boolean;
 begin
  hdr.Size:=StrLen(Name)+1;
  hdr.IsAvail:=false;
  b:=Search(plr, longint(plr), hdr.size);
  Stream^.status:=stOk;
  Stream^.Seek(longint(plr));
  Stream^.Write(hdr, sizeof(hdr));
  Stream^.Write(Name^, hdr.size);
  if not b then
   begin
    hdr.size:=0;
    Stream^.Write(hdr, sizeof(hdr));
   end;
  AddCFN:=plr;
 end;

 procedure   TEFNCollection.DelLFN(var ind: TLFNIndex);
 begin
  if longint(ind)=$0FFFFFFFF then exit;
  FreeItem(ind);
  longint(ind):=$0FFFFFFFF;
 end;

 function    TEFNCollection.UseLFN(ind: TLFNIndex): TLFNIndex;
 begin
  if longint(ind)=$0FFFFFFFF then begin UseLFN:=ind; exit end;
  UseLFN:=AddCFN(GetCFN(ind));
 end;

 function    TEFNCollection.GetLFN(ind: TLFNIndex): string;
 var p: pchar;
 begin
  if longint(ind)<>$0FFFFFFFF then
   begin
    p:=GetCFN(ind);
    GetLFN:=StrPas(p);
   end else GetLFN:='';
 end;

 var NameBuf: array[0..300] of char;
 function    TEFNCollection.GetCFN(ind: TLFNIndex): pchar;
 var hdr: TSTMRec;
 begin
  if longint(ind)<>$0FFFFFFFF then
   begin
    stream^.status:=stOk;
    stream^.seek(longint(ind));
    stream^.read(hdr, sizeof(hdr));
    stream^.read(NameBuf, hdr.size);
    GetCFN:=@NameBuf;
   end else GetCFN:=nil;
 end;

 procedure   TEFNCollection.StoreLFN(var S: TStream; LFN: TLFNIndex);
  var pc: pchar;
  begin
   if longint(LFN)<>$0FFFFFFFF then S.StrWrite(GetCFN(LFN));
  end;

 function    TEFNCollection.LoadLFN(var S: TStream): TLFNIndex;
 var pc: pchar;
 begin
  pc:=S.StrRead;
  LoadLFN:=AddCFN(pc);
  StrDispose(pc);
 end;

 constructor TXFNCollection.Init;
 var hdr: TSTMRec;
 begin
  TObject.Init;
  Stream := new(PXMSStream, Init($100000, $1000000));
  hdr.size:=0;
  stream^.write(hdr, sizeof(hdr));
 end;

 var EFNs: PEFNCollection;
 var MFNs: PLFNCollection;

function    AddLFN(const Name: string): TLFNIndex;   {Добавляет имя}
begin
 if EFNs<>nil then AddLFN:=EFNs^.AddLFN(Name)
              else AddLFN:=MFNs^.AddLFN(Name)
end;

function    AddCFN(Name: pchar):  TLFNIndex;         {Добавляет имя}
begin
 if EFNs<>nil then AddCFN:=EFNs^.AddCFN(Name)
              else AddCFN:=MFNs^.AddCFN(Name)
end;

procedure   DelLFN(var ind: TLFNIndex);              {Удаляет имя}
begin
 if EFNs<>nil then EFNs^.DelLFN(ind)
              else MFNs^.DelLFN(ind)
end;

function    UseLFN(ind: TLFNIndex):TLFNIndex;        {Добавляет пользователя именем}
begin
 if EFNs<>nil then UseLFN:=EFNs^.UseLFN(ind)
              else UseLFN:=MFNs^.UseLFN(ind)
end;

function    GetLFN(ind: TLFNIndex): string;          {Возвpащает имя в String'е}
begin
 if EFNs<>nil then GetLFN:=EFNs^.GetLFN(ind)
              else GetLFN:=MFNs^.GetLFN(ind)
end;

function    GetCFN(ind: TLFNIndex): pchar;           {Возвpащает имя в PChar'е}
begin
 if EFNs<>nil then GetCFN:=EFNs^.GetCFN(ind)
              else GetCFN:=MFNs^.GetCFN(ind)
end;

procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);{Сохpаняет дл. имя}
begin
 if EFNs<>nil then EFNs^.StoreLFN(S, LFN)
              else MFNs^.StoreLFN(S, LFN)
end;

function    LoadLFN(var S: TStream): TLFNIndex;      {Загpyжает его}
begin
 if EFNs<>nil then LoadLFN:=EFNs^.LoadLFN(S)
              else LoadLFN:=MFNs^.LoadLFN(S)
end;

var EP: pointer;
procedure EXP; far; begin
 ExitProc:=ep;
 if EFNs<>nil then dispose(EFNs, done);
 if MFNs<>nil then dispose(MFNs, done);
end;

begin
 if EMSFound and (EMSFreePages>$10)
  then begin New(EFNs, Init); MFNs:=nil end
  else
   if XMSFound and (XMSFree>$100)
    then begin EFNs:=New(PXFNCollection, Init); MFNs:=nil end
    else begin New(MFNs, Init($40, $40)); EFNs:=nil end;
 EP:=ExitProc; ExitProc:=@EXP;
end.