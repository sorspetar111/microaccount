unit M_Material;

interface

uses
  System.SysUtils, System.Classes;

type
  // TMaterial class represents a record from the 'Material' database table.
  // It holds details about raw materials, their stock levels, price, and unit.
  // It also includes a ProductID FK as per the specified schema.
  TMaterial = class
  private
    FId: Integer;
    FMaterialName: string;
    FQuantity: Currency;       // Represents the 'Quantity' column in your schema.
                               // Its exact meaning (e.g., default order quantity, batch size)
                               // should be clarified by business logic.
    FCurrentStock: Currency;
    FMinStockLevel: Currency;
    FDescription: string;
    FPriceId: Integer;      // Foreign Key to Price table
    FUnitId: Integer;       // Foreign Key to Unit table
    FProductId: Integer;    // Foreign Key to Product table (as per schema, can be NULL in DB)
  public
    // Constructor to initialize a new TMaterial instance.
    constructor Create;
    // Properties to access and modify the material data.
    property Id: Integer read FId write FId;
    property MaterialName: string read FMaterialName write FMaterialName;
    property Quantity: Currency read FQuantity write FQuantity;
    property CurrentStock: Currency read FCurrentStock write FCurrentStock;
    property MinStockLevel: Currency read FMinStockLevel write FMinStockLevel;
    property Description: string read FDescription write FDescription;
    property PriceId: Integer read FPriceId write FPriceId;
    property UnitId: Integer read FUnitId write FUnitId;
    property ProductId: Integer read FProductId write FProductId;
  end;

implementation

{ TMaterial }

constructor TMaterial.Create;
begin
  // Initialize default values for properties.
  FId := 0;
  FMaterialName := '';
  FQuantity := 0.0;
  FCurrentStock := 0.0;
  FMinStockLevel := 0.0;
  FDescription := '';
  FPriceId := 0;
  FUnitId := 0;
  FProductId := 0; // Default to 0, indicating no linked product initially
end;

end.
