unit M_Unit;

interface

uses
  System.SysUtils, System.Classes;

type
  // TUnit class represents a record from the 'Unit' database table.
  // It holds the ID, name of the unit, and an optional description.
  TUnit = class
  private
    FId: Integer;
    FName: string;
    FDescription: string;
  public
    // Constructor to initialize a new TUnit instance.
    constructor Create;
    // Properties to access and modify the unit data.
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
  end;

implementation

{ TUnit }

constructor TUnit.Create;
begin
  // Initialize default values for properties.
  FId := 0;
  FName := '';
  FDescription := '';
end;

end.
