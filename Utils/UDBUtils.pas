unit UDBUtils;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Product, M_Part, M_Service, M_Material, M_Price, M_Unit, M_Currency,
  BLC_Product, BLC_Material;

type
  TAppUtils = class
  public
    class function CreateProductFromPartAndService(
      AConnection: TADOConnection;
      const AProductName: string;
      const AProductCode: string;
      AIsFinalProduct: Boolean;
      const ADescription: string;
      APriceId: Integer;
      AMaterialId: Integer;
      APart: TPart;
      AService: TService
    ): TProduct;

    class function CreateMaterialFromPriceAndProduct(
      AConnection: TADOConnection;
      const AMaterialName: string;
      AQuantity: Currency;
      ACurrentStock: Currency;
      AMinStockLevel: Currency;
      const ADescription: string;
      APrice: TPrice;
      AUnit: TUnit;
      AProduct: TProduct
    ): TMaterial;
  end;

implementation

class function TAppUtils.CreateProductFromPartAndService(
  AConnection: TADOConnection;
  const AProductName: string;
  const AProductCode: string;
  AIsFinalProduct: Boolean;
  const ADescription: string;
  APriceId: Integer;
  AMaterialId: Integer;
  APart: TPart;
  AService: TService
): TProduct;
var
  LProduct: TProduct;
  LProductManager: TProductManager;
begin
  Result := nil;

  if (AConnection = nil) or not AConnection.Connected then Exit;
  if AProductName.Trim = '' then Exit;
  if AProductCode.Trim = '' then Exit;

  LProduct := nil;
  LProductManager := nil;
  try
    LProduct := TProduct.Create;
    LProduct.ProductName := AProductName;
    LProduct.ProductCode := AProductCode;
    LProduct.IsFinalProduct := AIsFinalProduct;
    LProduct.Description := ADescription;
    LProduct.PriceId := APriceId;
    LProduct.MaterialId := AMaterialId;

    LProductManager := TProductManager.Create(AConnection);
    if LProductManager.SaveProduct(LProduct) then
      Result := LProduct
    else
      FreeAndNil(LProduct);
  except
    FreeAndNil(LProduct);
    Result := nil;
  finally
    FreeAndNil(LProductManager);
  end;
end;

class function TAppUtils.CreateMaterialFromPriceAndProduct(
  AConnection: TADOConnection;
  const AMaterialName: string;
  AQuantity: Currency;
  ACurrentStock: Currency;
  AMinStockLevel: Currency;
  const ADescription: string;
  APrice: TPrice;
  AUnit: TUnit;
  AProduct: TProduct
): TMaterial;
var
  LMaterial: TMaterial;
  LMaterialManager: TMaterialManager;
begin
  Result := nil;

  if (AConnection = nil) or not AConnection.Connected then Exit;
  if AMaterialName.Trim = '' then Exit;
  if (APrice = nil) or (APrice.Id = 0) then Exit;
  if (AUnit = nil) or (AUnit.Id = 0) then Exit;

  LMaterial := nil;
  LMaterialManager := nil;
  try
    LMaterial := TMaterial.Create;
    LMaterial.MaterialName := AMaterialName;
    LMaterial.Quantity := AQuantity;
    LMaterial.CurrentStock := ACurrentStock;
    LMaterial.MinStockLevel := AMinStockLevel;
    LMaterial.Description := ADescription;
    LMaterial.PriceId := APrice.Id;
    LMaterial.UnitOfMeasure := AUnit.Name;

    LMaterialManager := TMaterialManager.Create(AConnection);
    if LMaterialManager.SaveMaterial(LMaterial) then
      Result := LMaterial
    else
      FreeAndNil(LMaterial);
  except
    FreeAndNil(LMaterial);
    Result := nil;
  finally
    FreeAndNil(LMaterialManager);
  end;
end;

end.