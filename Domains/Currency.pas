unit M_Unit;

interface

uses
  System.SysUtils, System.Classes;

type
  // TCurrency class represents a record from the 'Unit' database table.
  // It holds the ID, name of the unit, and an optional description.
  TCurrency = class
  private
    FId: Integer;
    FName: string;
  
  public
    // Constructor to initialize a new TCurrency instance.
    constructor Create;
    // Properties to access and modify the unit data.
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
   
  end;

implementation

{ TCurrency }

constructor TCurrency.Create;
begin
  // Initialize default values for properties.
  FId := 0;
  FName := '';

end;

end.
