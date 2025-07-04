unit BLC_Product;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Product,         // Include the Product Model class
  BLC_BaseService,   // Include the Base Service class
  BLC_Price,         // Include Price Manager for FK relation
  BLC_Unit,          // Include Unit Manager for FK relation
  BLC_Part,          // Include Part Manager for FK relation
  BLC_Service;       // Include Service Manager for FK relation

type
  // TProductManager handles the business logic and database interactions
  // for the 'Product' entity. It manages CRUD operations and interacts
  // with related managers (Price, Unit, Part, Service) for validation or lookup.
  TProductManager = class(TBaseService)
  private
    // Private fields to hold instances of related business logic managers.
    // These are used to interact with the tables referenced by foreign keys.
    FPriceManager: TPriceManager;
    FUnitManager: TUnitManager;
    FPartManager: TPartManager;
    FServiceManager: TServiceManager;
  public
    // Constructor: Initializes the TProductManager and its dependent managers.
    constructor Create(AConnection: TADOConnection);
    // Destructor: Frees the instances of dependent managers.
    destructor Destroy; override;

    // Loads a TProduct object from the database based on its ID.
    // Returns nil if the product is not found.
    function LoadProductById(AProductId: Integer): TProduct;

    // Saves a TProduct object to the database.
    // If Product.Id is 0, it performs an INSERT (creates a new record).
    // If Product.Id is > 0, it performs an UPDATE (modifies an existing record).
    // Returns True on success, False on failure.
    // On successful insert, updates Product.Id with the new generated ID.
    function SaveProduct(AProduct: TProduct): Boolean;

    // Deletes a product record from the database based on its ID.
    // Returns True on success, False on failure.
    function DeleteProduct(AProductId: Integer): Boolean;

    // Optional: Add validation methods here if needed, e.g.,
    // function IsPriceIdValid(APriceId: Integer): Boolean;
    // function IsUnitIdValid(AUnitId: Integer): Boolean;
    // function IsPartIdValid(APartId: Integer): Boolean;
    // function IsServiceIdValid(AServiceId: Integer): Boolean;
  end;

implementation

{ TProductManager }

constructor TProductManager.Create(AConnection: TADOConnection);
begin
  // Call the base class constructor to initialize the TADOQuery with the connection.
  inherited Create(AConnection);

  // Create instances of the managers for related entities.
  // They all share the same ADOConnection to ensure consistency.
  FPriceManager := TPriceManager.Create(AConnection);
  FUnitManager := TUnitManager.Create(AConnection);
  FPartManager := TPartManager.Create(AConnection);
  FServiceManager := TServiceManager.Create(AConnection);
end;

destructor TProductManager.Destroy;
begin
  // Free the instances of the dependent managers to prevent memory leaks.
  FreeAndNil(FServiceManager);
  FreeAndNil(FPartManager);
  FreeAndNil(FUnitManager);
  FreeAndNil(FPriceManager);
  inherited Destroy;
end;

function TProductManager.LoadProductById(AProductId: Integer): TProduct;
var
  LProduct: TProduct;
begin
  Result := nil; // Default to nil, indicating not found

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for selecting a product by ID.
    Query.SQL.Text :=
      'SELECT ID, ProductName, ProductCode, CurrentStock, IsFinalProduct, ' +
      'Description, PriceID, UnitID, PartID, ServiceID ' +
      'FROM Product ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AProductId;

    // Execute the query to open the dataset.
    Query.Open;

    // Check if a record was found.
    if not Query.EOF then
    begin
      // Create a new TProduct object and populate its properties
      // from the query's current record.
      LProduct := TProduct.Create;
      LProduct.Id := Query.FieldByName('ID').AsInteger;
      LProduct.ProductName := Query.FieldByName('ProductName').AsString;
      LProduct.ProductCode := Query.FieldByName('ProductCode').AsString;
      LProduct.CurrentStock := Query.FieldByName('CurrentStock').AsCurrency;
      LProduct.IsFinalProduct := Query.FieldByName('IsFinalProduct').AsBoolean;
      LProduct.Description := Query.FieldByName('Description').AsString;
      LProduct.PriceId := Query.FieldByName('PriceID').AsInteger;
      LProduct.UnitId := Query.FieldByName('UnitID').AsInteger;

      // Handle nullable foreign keys (PartID, ServiceID)
      if not Query.FieldByName('PartID').IsNull then
        LProduct.PartId := Query.FieldByName('PartID').AsInteger
      else
        LProduct.PartId := 0; // Or some other indicator for NULL

      if not Query.FieldByName('ServiceID').IsNull then
        LProduct.ServiceId := Query.FieldByName('ServiceID').AsInteger
      else
        LProduct.ServiceId := 0; // Or some other indicator for NULL

      Result := LProduct; // Return the loaded product object
    end;
  except
    on E: Exception do
    begin
      // Log or handle the exception appropriately.
      ShowMessage('Error loading product: ' + E.Message);
      // Ensure Result is nil if an error occurs.
      FreeAndNil(LProduct);
      Result := nil;
    end;
  end;
end;

function TProductManager.SaveProduct(AProduct: TProduct): Boolean;
var
  LPartIDValue: Variant;
  LServiceIDValue: Variant;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Determine if PartID and ServiceID should be NULL in the database
    if AProduct.PartId = 0 then
      LPartIDValue := Null
    else
      LPartIDValue := AProduct.PartId;

    if AProduct.ServiceId = 0 then
      LServiceIDValue := Null
    else
      LServiceIDValue := AProduct.ServiceId;

    // Check if it's an INSERT (new record) or UPDATE (existing record).
    if AProduct.Id = 0 then // INSERT operation
    begin
      Query.SQL.Text :=
        'INSERT INTO Product (ProductName, ProductCode, CurrentStock, IsFinalProduct, ' +
        'Description, PriceID, UnitID, PartID, ServiceID) ' +
        'VALUES (:ProductName, :ProductCode, :CurrentStock, :IsFinalProduct, ' +
        ':Description, :PriceID, :UnitID, :PartID, :ServiceID); ' +
        'SELECT SCOPE_IDENTITY() AS NewID;'; // For SQL Server to get the new ID
    else // UPDATE operation
    begin
      Query.SQL.Text :=
        'UPDATE Product ' +
        'SET ProductName = :ProductName, ProductCode = :ProductCode, ' +
        'CurrentStock = :CurrentStock, IsFinalProduct = :IsFinalProduct, ' +
        'Description = :Description, PriceID = :PriceID, UnitID = :UnitID, ' +
        'PartID = :PartID, ServiceID = :ServiceID ' +
        'WHERE ID = :ID;';
    end;

    // Clear existing parameters.
    Query.Parameters.Clear;

    // Set parameters from the AProduct object.
    Query.Parameters.ParamByName('ProductName').Value := AProduct.ProductName;
    Query.Parameters.ParamByName('ProductCode').Value := AProduct.ProductCode;
    Query.Parameters.ParamByName('CurrentStock').Value := AProduct.CurrentStock;
    Query.Parameters.ParamByName('IsFinalProduct').Value := AProduct.IsFinalProduct;
    Query.Parameters.ParamByName('Description').Value := AProduct.Description;
    Query.Parameters.ParamByName('PriceID').Value := AProduct.PriceId;
    Query.Parameters.ParamByName('UnitID').Value := AProduct.UnitId;
    Query.Parameters.ParamByName('PartID').Value := LPartIDValue;    // Assign Variant for NULL handling
    Query.Parameters.ParamByName('ServiceID').Value := LServiceIDValue; // Assign Variant for NULL handling

    // If it's an UPDATE, add the ID parameter for the WHERE clause.
    if AProduct.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := AProduct.Id;

    // Execute the SQL command.
    Query.ExecSQL;

    // If it was an INSERT, retrieve the newly generated ID.
    if (AProduct.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open; // Open to read the result of SCOPE_IDENTITY()
      if not Query.EOF then
      begin
        AProduct.Id := Query.FieldByName('NewID').AsInteger; // Update the object's ID
      end;
      Query.Close; // Close the query after reading the ID
    end;

    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error saving product: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TProductManager.DeleteProduct(AProductId: Integer): Boolean;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for deleting a product by ID.
    Query.SQL.Text :=
      'DELETE FROM Product ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AProductId;

    // Execute the SQL command.
    Query.ExecSQL;
    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error deleting product: ' + E.Message);
      Result := False;
    end;
  end;
end;

end.
