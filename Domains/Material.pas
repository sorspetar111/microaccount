unit M_Material;

interface

uses
  System.SysUtils, System.Classes;

type
  TMaterial = class
  private
    FId: Integer;
    FMaterialName: string;
    FQuantity: Currency;
    FUnitOfMeasure: string; 
    FCurrentStock: Currency;
    FMinStockLevel: Currency;
    FDescription: string;
    FPriceId: Integer;
  public
    constructor Create;
    property Id: Integer read FId write FId;
    property MaterialName: string read FMaterialName write FMaterialName;
    property Quantity: Currency read FQuantity write FQuantity;
    property UnitOfMeasure: string read FUnitOfMeasure write FUnitOfMeasure;
    property CurrentStock: Currency read FCurrentStock write FCurrentStock;
    property MinStockLevel: Currency read FMinStockLevel write FMinStockLevel;
    property Description: string read FDescription write FDescription;
    property PriceId: Integer read FPriceId write FPriceId;
  end;

implementation

constructor TMaterial.Create;
begin
  FId := 0;
  FMaterialName := '';
  FQuantity := 0.0;
  FUnitOfMeasure := '';
  FCurrentStock := 0.0;
  FMinStockLevel := 0.0;
  FDescription := '';
  FPriceId := 0;
end;

end.