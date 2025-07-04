unit M_Product;

interface

uses
  System.SysUtils, System.Classes;

type
  // TProduct class represents a record from the 'Product' database table.
  // It holds details about a product (final or intermediate), its stock, price, and associated entities.
  TProduct = class
  private
    FId: Integer;
    FProductName: string;
    FProductCode: string;
    FCurrentStock: Currency;
    FIsFinalProduct: Boolean;
    FDescription: string;
    FPriceId: Integer;      // Foreign Key to Price table
    FUnitId: Integer;       // Foreign Key to Unit table
    FPartId: Integer;       // Foreign Key to Part table (as per schema, can be NULL in DB)
    FServiceId: Integer;    // Foreign Key to Service table (as per schema, can be NULL in DB)
  public
    // Constructor to initialize a new TProduct instance.
    constructor Create;
    // Properties to access and modify the product data.
    property Id: Integer read FId write FId;
    property ProductName: string read FProductName write FFProductName;
    property ProductCode: string read FProductCode write FProductCode;
    property CurrentStock: Currency read FCurrentStock write FCurrentStock;
    property IsFinalProduct: Boolean read FIsFinalProduct write FIsFinalProduct;
    property Description: string read FDescription write FDescription;
    property PriceId: Integer read FPriceId write FPriceId;
    property UnitId: Integer read FUnitId write FUnitId;
    property PartId: Integer read FPartId write FPartId;
    property ServiceId: Integer read FServiceId write FServiceId;
  end;

implementation

{ TProduct }

constructor TProduct.Create;
begin
  // Initialize default values for properties.
  FId := 0;
  FProductName := '';
  FProductCode := '';
  FCurrentStock := 0.0;
  FIsFinalProduct := False; // Default to intermediate product
  FDescription := '';
  FPriceId := 0;
  FUnitId := 0;
  FPartId := 0;
  FServiceId := 0;
end;

end.
