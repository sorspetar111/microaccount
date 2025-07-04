unit UAppUtils;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Product,         // Model for Product
  M_Part,            // Model for Part
  M_Service,         // Model for Service
  M_Material,        // Model for Material (NEW)
  M_Price,           // Model for Price (for CreateMaterialFromPriceAndProduct)
  M_Unit,            // Model for Unit (for CreateMaterialFromPriceAndProduct)
  BLC_Product,       // Business Logic for Product
  BLC_Material;      // Business Logic for Material (NEW)

type
  // TAppUtils provides general utility functions for the application.
  // This includes helper functions that might orchestrate operations
  // involving multiple business logic managers or provide common services.
  TAppUtils = class
  public
    // CreateProductFromPartAndService:
    // Creates a new product record in the database, associating it with
    // a specific part and a specific service, along with other product details.
    //
    // Parameters:
    //   AConnection: The active TADOConnection to the database.
    //   AProductName: The name of the new product.
    //   AProductCode: The unique code for the new product.
    //   AIsFinalProduct: Boolean indicating if it's a final product (True) or intermediate (False).
    //   ADescription: An optional description for the product.
    //   APriceId: The ID of the price associated with this product.
    //   AUnitId: The ID of the unit of measure for this product.
    //   APart: The TPart object to associate with this product (can be nil if no specific part).
    //   AService: The TService object to associate with this product (can be nil if no specific service).
    //
    // Returns:
    //   A TProduct object representing the newly created product if successful,
    //   otherwise nil. The returned TProduct object will have its ID populated.
    class function CreateProductFromPartAndService(
      AConnection: TADOConnection;
      const AProductName: string;
      const AProductCode: string;
      AIsFinalProduct: Boolean;
      const ADescription: string;
      APriceId: Integer;
      AUnitId: Integer;
      APart: TPart;     // Can be nil
      AService: TService // Can be nil
    ): TProduct;

    // CreateMaterialFromPriceAndProduct:
    // Creates a new material record in the database, associating it with
    // a specific price, unit, and product (optional), along with other material details.
    //
    // Parameters:
    //   AConnection: The active TADOConnection to the database.
    //   AMaterialName: The name of the new material.
    //   AQuantity: The quantity value for the material (meaning depends on business logic, e.g., batch size).
    //   ACurrentStock: The initial current stock level for the material.
    //   AMinStockLevel: The minimum stock level for the material.
    //   ADescription: An optional description for the material.
    //   APrice: The TPrice object to associate with this material.
    //   AUnit: The TUnit object to associate with this material.
    //   AProduct: The TProduct object to associate with this material (can be nil if no specific product).
    //
    // Returns:
    //   A TMaterial object representing the newly created material if successful,
    //   otherwise nil. The returned TMaterial object will have its ID populated.
    class function CreateMaterialFromPriceAndProduct(
      AConnection: TADOConnection;
      const AMaterialName: string;
      AQuantity: Currency;
      ACurrentStock: Currency;
      AMinStockLevel: Currency;
      const ADescription: string;
      APrice: TPrice;
      AUnit: TUnit;
      AProduct: TProduct // Can be nil
    ): TMaterial;
  end;

implementation

{ TAppUtils }

class function TAppUtils.CreateProductFromPartAndService(
  AConnection: TADOConnection;
  const AProductName: string;
  const AProductCode: string;
  AIsFinalProduct: Boolean;
  const ADescription: string;
  APriceId: Integer;
  AUnitId: Integer;
  APart: TPart;
  AService: TService
): TProduct;
var
  LProduct: TProduct;
  LProductManager: TProductManager;
begin
  Result := nil; // Default to nil in case of failure

  // Input validation (basic checks)
  if (AConnection = nil) or not AConnection.Connected then
  begin
    ShowMessage('Database connection is not active.');
    Exit;
  end;

  if AProductName.Trim = '' then
  begin
    ShowMessage('Product Name cannot be empty.');
    Exit;
  end;

  if AProductCode.Trim = '' then
  begin
    ShowMessage('Product Code cannot be empty.');
    Exit;
  end;

  // Validate FKs exist (optional, but good practice)
  // This would require managers for Price and Unit to be called here.
  // For simplicity, we'll assume valid IDs are passed for PriceId and UnitId.
  // If APart/AService are not nil, their IDs are used.

  LProduct := nil;
  LProductManager := nil;
  try
    // 1. Create a new TProduct object and populate its properties
    LProduct := TProduct.Create;
    LProduct.ProductName := AProductName;
    LProduct.ProductCode := AProductCode;
    LProduct.IsFinalProduct := AIsFinalProduct;
    LProduct.Description := ADescription;
    LProduct.PriceId := APriceId;
    LProduct.UnitId := AUnitId;

    // Set PartId and ServiceId from the provided TPart and TService objects
    if Assigned(APart) then
      LProduct.PartId := APart.Id
    else
      LProduct.PartId := 0; // Indicate NULL for database

    if Assigned(AService) then
      LProduct.ServiceId := AService.Id
    else
      LProduct.ServiceId := 0; // Indicate NULL for database

    // 2. Instantiate the Product Business Logic Manager
    LProductManager := TProductManager.Create(AConnection);

    // 3. Save the new product to the database
    if LProductManager.SaveProduct(LProduct) then
    begin
      Result := LProduct; // Return the newly created product object (with its new ID)
    end
    else
    begin
      // Save failed, free the product object
      FreeAndNil(LProduct);
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Error creating product: ' + E.Message);
      FreeAndNil(LProduct); // Ensure product object is freed on error
      Result := nil;
    end;
  finally
    // Always free the manager instance
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
  Result := nil; // Default to nil in case of failure

  // Input validation (basic checks)
  if (AConnection = nil) or not AConnection.Connected then
  begin
    ShowMessage('Database connection is not active.');
    Exit;
  end;

  if AMaterialName.Trim = '' then
  begin
    ShowMessage('Material Name cannot be empty.');
    Exit;
  end;

  // Validate required FK objects
  if not Assigned(APrice) or (APrice.Id = 0) then
  begin
    ShowMessage('A valid Price object must be provided for the material.');
    Exit;
  end;

  if not Assigned(AUnit) or (AUnit.Id = 0) then
  begin
    ShowMessage('A valid Unit object must be provided for the material.');
    Exit;
  end;

  LMaterial := nil;
  LMaterialManager := nil;
  try
    // 1. Create a new TMaterial object and populate its properties
    LMaterial := TMaterial.Create;
    LMaterial.MaterialName := AMaterialName;
    LMaterial.Quantity := AQuantity;
    LMaterial.CurrentStock := ACurrentStock;
    LMaterial.MinStockLevel := AMinStockLevel;
    LMaterial.Description := ADescription;
    LMaterial.PriceId := APrice.Id; // Use ID from the provided TPrice object
    LMaterial.UnitId := AUnit.Id;   // Use ID from the provided TUnit object

    // Set ProductId from the provided TProduct object (can be nil)
    if Assigned(AProduct) then
      LMaterial.ProductId := AProduct.Id
    else
      LMaterial.ProductId := 0; // Indicate NULL for database

    // 2. Instantiate the Material Business Logic Manager
    LMaterialManager := TMaterialManager.Create(AConnection);

    // 3. Save the new material to the database
    if LMaterialManager.SaveMaterial(LMaterial) then
    begin
      Result := LMaterial; // Return the newly created material object (with its new ID)
    end
    else
    begin
      // Save failed, free the material object
      FreeAndNil(LMaterial);
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Error creating material: ' + E.Message);
      FreeAndNil(LMaterial); // Ensure material object is freed on error
      Result := nil;
    end;
  finally
    // Always free the manager instance
    FreeAndNil(LMaterialManager);
  end;
end;

end.
