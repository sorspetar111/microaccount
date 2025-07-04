unit M_Part;

interface

uses
  System.SysUtils, System.Classes;

type
  // TPart class represents a record from the 'Part' database table.
  // It holds details about a component, its price, and unit of measure.
  TPart = class
  private
    FId: Integer;
    FPartName: string;
    FIsConsumable: Boolean;
    FDescription: string;
    FPriceId: Integer; // Foreign Key to Price table
    FUnitId: Integer;   // Foreign Key to Unit table
  public
    // Constructor to initialize a new TPart instance.
    constructor Create;
    // Properties to access and modify the part data.
    property Id: Integer read FId write FId;
    property PartName: string read FPartName write FPartName;
    property IsConsumable: Boolean read FIsConsumable write FIsConsumable;
    property Description: string read FDescription write FDescription;
    property PriceId: Integer read FPriceId write FPriceId;
    property UnitId: Integer read FUnitId write FUnitId;
  end;

implementation

{ TPart }

constructor TPart.Create;
begin
  // Initialize default values for properties.
  FId := 0;
  FPartName := '';
  FIsConsumable := True; // Default to consumable
  FDescription := '';
  FPriceId := 0; // Default to 0, indicating no linked price yet
  FUnitId := 0;   // Default to 0, indicating no linked unit yet
end;

end.
