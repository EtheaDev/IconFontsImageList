unit Icons.Utils;

interface

{$if compilerversion >= 21}
  {$define Delphi2010_UP}
{$ifend}

uses Classes, SysUtils, Generics.Collections;

type
  TIconEntry = record
    name: string;
    codepoint: Integer;
    index: Integer;
    aliases: string;

    {$IFDEF Delphi2010_UP}
    class operator Implicit(Entry: TIconEntry): Integer;
    class operator Implicit(Entry: TIconEntry): string;
    {$ENDIF}
  end;

  TIconEntryFunc = reference to function(const AEntry: TIconEntry): Boolean;
  TIconCollectionEnumProc = reference to procedure(const AFunc: TIconEntryFunc);

  TIconCollection = record
    Name: string;
    Source: string;
    Description: string;
    Version: string;
    EnumProc: TIconCollectionEnumProc;

    procedure ForEach(const ADoSomething: TIconEntryFunc);
    function Contains(const AName: string): Boolean; overload;
    function Contains(const ACodepoint: Integer): Boolean; overload;

    function FindEntry(const AName: string; out AEntry: TIconEntry;
      const ACaseInsensitive: Boolean = False): Boolean; overload;
    function FindEntry(const ACodepoint: Integer; out AEntry: TIconEntry): Boolean; overload;

    function ToString: string;

    constructor Create(const AName: string; const ASource: string;
      const ADescription: string; const AVersion: string;
      const AEnumProc: TIconCollectionEnumProc);
  end;

  TIconManager = class
  private
    class var _Instance: TIconManager;
  private
    FCollections: TList<TIconCollection>;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function FindCollection(const AFontName: string;
      out AIconCollection: TIconCollection): boolean;

    procedure AddCollection(const ACollection: TIconCollection);

    property Collections: TList<TIconCollection> read FCollections;

    class function Instance: TIconManager; static;
    class procedure FinalizeInstance; static;
  end;

implementation

{ TIconManager }

procedure TIconManager.AddCollection(const ACollection: TIconCollection);
begin
  FCollections.Add(ACollection);
end;

constructor TIconManager.Create;
begin
  inherited Create;
  FCollections := TList<TIconCollection>.Create;
end;

destructor TIconManager.Destroy;
begin
  FCollections.Free;
  inherited;
end;

class procedure TIconManager.FinalizeInstance;
begin
  if Assigned(_Instance) then
    FreeAndNil(_Instance);
end;

function TIconManager.FindCollection(const AFontName: string;
  out AIconCollection: TIconCollection): boolean;
var
  LIconCollection: TIconCollection;
begin
  Result := False;
  for LIconCollection in Collections do
    if SameText(LIconCollection.Name, AFontName) then
    begin
      AIconCollection := LIconCollection;
      Result := True;
      break;
    end;
end;

class function TIconManager.Instance: TIconManager;
begin
  if not Assigned(_Instance) then
    _Instance := TIconManager.Create;
  Result := _Instance;
end;

{ TIconCollection }

function TIconCollection.Contains(const ACodepoint: Integer): Boolean;
var
  LDummy: TIconEntry;
begin
  Result := FindEntry(ACodepoint, LDummy);
end;

function TIconCollection.Contains(const AName: string): Boolean;
var
  LDummy: TIconEntry;
begin
  Result := FindEntry(AName, LDummy);
end;

constructor TIconCollection.Create(const AName, ASource, ADescription, AVersion: string;
  const AEnumProc: TIconCollectionEnumProc);
begin
  Name := AName;
  Source := ASource;
  Description := ADescription;
  Version := AVersion;
  EnumProc := AEnumProc;
end;

function TIconCollection.FindEntry(const ACodepoint: Integer;
  out AEntry: TIconEntry): Boolean;
var
  LResult: Boolean;
  LEntry: TIconEntry;
begin
  LResult := False; // not found by default

  ForEach(
    function (const Entry: TIconEntry): Boolean
    begin
      Result := True;
      if Entry.codepoint = ACodepoint then
      begin
        Result := False; // break iteration over collection
        LEntry := Entry;
        LResult := True; // found
      end;
    end
  );

  Result := LResult;
  if Result then
    AEntry := LEntry;
end;

function TIconCollection.FindEntry(const AName: string;
  out AEntry: TIconEntry; const ACaseInsensitive: Boolean = False): Boolean;
var
  LResult: Boolean;
  LEntry: TIconEntry;
begin
  LResult := False; // not found by default

  ForEach(
    function (const Entry: TIconEntry): Boolean
    begin
      Result := True;
      if ((not ACaseInsensitive) and (Entry.name = AName))
        or (ACaseInsensitive and SameText(Entry.name, AName))
      then
      begin
        Result := False; // break iteration over collection
        LEntry := Entry;
        LResult := True; // found
      end;
    end
  );

  Result := LResult;
  if Result then
    AEntry := LEntry;
end;

procedure TIconCollection.ForEach(const ADoSomething: TIconEntryFunc);
begin
  if Assigned(EnumProc) then
    EnumProc(ADoSomething);
end;

function TIconCollection.ToString: string;
begin
  Result := Name;
  if Version <> '' then
    Result := Result + ' (' + Version + ')';
end;

{ TIconEntry }

{$IFDEF Delphi2010_UP}
class operator TIconEntry.Implicit(Entry: TIconEntry): string;
begin
  Result := Entry.name;
end;

class operator TIconEntry.Implicit(Entry: TIconEntry): Integer;
begin
  Result := Entry.codepoint;
end;
{$ENDIF}

initialization

finalization
  TIconManager.FinalizeInstance;



end.
