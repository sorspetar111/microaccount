unit M_Product;

interface

uses
  System.SysUtils, System.Classes;

type
  TProduct = class
  private
    FId: Integer;
    FProductName: string;
    FProductCode: string;
    FUnitOfMeasure: string; 
    FCurrentStock: Currency;
    FIsFinalProduct: Boolean;
    FDescription: string;
    FPriceId: Integer;
    FMaterialId: Integer;  
  public
    constructor Create;
    property Id: Integer read FId write FId;
    property ProductName: string read FProductName write FProductName;  
    property ProductCode: string read FProductCode write FProductCode;
    property UnitOfMeasure: string read FUnitOfMeasure write FUnitOfMeasure;
    property CurrentStock: Currency read FCurrentStock write FCurrentStock;
    property IsFinalProduct: Boolean read FIsFinalProduct write FIsFinalProduct;
    property Description: string read FDescription write FDescription;
    property PriceId: Integer read FPriceId write FPriceId;
    property MaterialId: Integer read FMaterialId write FMaterialId;  
  end;

implementation

constructor TProduct.Create;
begin
  FId := 0;
  FProductName := '';
  FProductCode := '';
  FUnitOfMeasure := '';
  FCurrentStock := 0.0;
  FIsFinalProduct := False;
  FDescription := '';
  FPriceId := 0;
  FMaterialId := 0;
end;

end.