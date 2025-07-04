unit BLC_Material;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Material,        // Include the Material Model class
  BLC_BaseService,   // Include the Base Service class
  BLC_Price,         // Include Price Manager for FK relation
  BLC_Unit,          // Include Unit Manager for FK relation
  BLC_Product;       // Include Product Manager for FK relation (for ProductID FK)

type
  // TMaterialManager handles the business logic and database interactions
  // for the 'Material' entity. It manages CRUD operations and interacts
  // with related managers (Price, Unit, Product) for validation or lookup.
  TMaterialManager = class(TBaseService)
  private
    // Private fields to hold instances of related business logic managers.
    // These are used to interact with the tables referenced by foreign keys.
    FPriceManager: TPriceManager;
    FUnitManager: TUnitManager;
    FProductManager: TProductManager; // For the ProductID FK
  public
    // Constructor: Initializes the TMaterialManager and its dependent managers.
    constructor Create(AConnection: TADOConnection);
    // Destructor: Frees the instances of dependent managers.
    destructor Destroy; override;

    // Loads a TMaterial object from the database based on its ID.
    // Returns nil if the material is not found.
    function LoadMaterialById(AMaterialId: Integer): TMaterial;

    // Saves a TMaterial object to the database.
    // If Material.Id is 0, it performs an INSERT (creates a new record).
    // If Material.Id is > 0, it performs an UPDATE (modifies an existing record).
    // Returns True on success, False on failure.
    // On successful insert, updates Material.Id with the new generated ID.
    function SaveMaterial(AMaterial: TMaterial): Boolean;

    // Deletes a material record from the database based on its ID.
    // Returns True on success, False on failure.
    function DeleteMaterial(AMaterialId: Integer): Boolean;

    // Optional: Add validation methods here if needed, e.g.,
    // function IsPriceIdValid(APriceId: Integer): Boolean;
    // function IsUnitIdValid(AUnitId: Integer): Boolean;
    // function IsProductIdValid(AProductId: Integer): Boolean;
  end;

implementation

{ TMaterialManager }

constructor TMaterialManager.Create(AConnection: TADOConnection);
begin
  // Call the base class constructor to initialize the TADOQuery with the connection.
  inherited Create(AConnection);

  // Create instances of the managers for related entities.
  // They all share the same ADOConnection to ensure consistency.
  FPriceManager := TPriceManager.Create(AConnection);
  FUnitManager := TUnitManager.Create(AConnection);
  FProductManager := TProductManager.Create(AConnection);
end;

destructor TMaterialManager.Destroy;
begin
  // Free the instances of the dependent managers to prevent memory leaks.
  FreeAndNil(FProductManager);
  FreeAndNil(FUnitManager);
  FreeAndNil(FPriceManager);
  inherited Destroy;
end;

function TMaterialManager.LoadMaterialById(AMaterialId: Integer): TMaterial;
var
  LMaterial: TMaterial;
begin
  Result := nil; // Default to nil, indicating not found

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for selecting a material by ID.
    Query.SQL.Text :=
      'SELECT ID, MaterialName, Quantity, CurrentStock, MinStockLevel, ' +
      'Description, PriceID, UnitID, ProductID ' +
      'FROM Material ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AMaterialId;

    // Execute the query to open the dataset.
    Query.Open;

    // Check if a record was found.
    if not Query.EOF then
    begin
      // Create a new TMaterial object and populate its properties
      // from the query's current record.
      LMaterial := TMaterial.Create;
      LMaterial.Id := Query.FieldByName('ID').AsInteger;
      LMaterial.MaterialName := Query.FieldByName('MaterialName').AsString;
      LMaterial.Quantity := Query.FieldByName('Quantity').AsCurrency;
      LMaterial.CurrentStock := Query.FieldByName('CurrentStock').AsCurrency;
      LMaterial.MinStockLevel := Query.FieldByName('MinStockLevel').AsCurrency;
      LMaterial.Description := Query.FieldByName('Description').AsString;
      LMaterial.PriceId := Query.FieldByName('PriceID').AsInteger;
      LMaterial.UnitId := Query.FieldByName('UnitID').AsInteger;

      // Handle nullable foreign key (ProductID)
      if not Query.FieldByName('ProductID').IsNull then
        LMaterial.ProductId := Query.FieldByName('ProductID').AsInteger
      else
        LMaterial.ProductId := 0; // Or some other indicator for NULL

      Result := LMaterial; // Return the loaded material object
    end;
  except
    on E: Exception do
    begin
      // Log or handle the exception appropriately.
      ShowMessage('Error loading material: ' + E.Message);
      // Ensure Result is nil if an error occurs.
      FreeAndNil(LMaterial);
      Result := nil;
    end;
  end;
end;

function TMaterialManager.SaveMaterial(AMaterial: TMaterial): Boolean;
var
  LProductIDValue: Variant;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Determine if ProductID should be NULL in the database
    if AMaterial.ProductId = 0 then
      LProductIDValue := Null
    else
      LProductIDValue := AMaterial.ProductId;

    // Check if it's an INSERT (new record) or UPDATE (existing record).
    if AMaterial.Id = 0 then // INSERT operation
    begin
      Query.SQL.Text :=
        'INSERT INTO Material (MaterialName, Quantity, CurrentStock, MinStockLevel, ' +
        'Description, PriceID, UnitID, ProductID) ' +
        'VALUES (:MaterialName, :Quantity, :CurrentStock, :MinStockLevel, ' +
        ':Description, :PriceID, :UnitID, :ProductID); ' +
        'SELECT SCOPE_IDENTITY() AS NewID;'; // For SQL Server to get the new ID
    else // UPDATE operation
    begin
      Query.SQL.Text :=
        'UPDATE Material ' +
        'SET MaterialName = :MaterialName, Quantity = :Quantity, ' +
        'CurrentStock = :CurrentStock, MinStockLevel = :MinStockLevel, ' +
        'Description = :Description, PriceID = :PriceID, UnitID = :UnitID, ' +
        'ProductID = :ProductID ' +
        'WHERE ID = :ID;';
    end;

    // Clear existing parameters.
    Query.Parameters.Clear;

    // Set parameters from the AMaterial object.
    Query.Parameters.ParamByName('MaterialName').Value := AMaterial.MaterialName;
    Query.Parameters.ParamByName('Quantity').Value := AMaterial.Quantity;
    Query.Parameters.ParamByName('CurrentStock').Value := AMaterial.CurrentStock;
    Query.Parameters.ParamByName('MinStockLevel').Value := AMaterial.MinStockLevel;
    Query.Parameters.ParamByName('Description').Value := AMaterial.Description;
    Query.Parameters.ParamByName('PriceID').Value := AMaterial.PriceId;
    Query.Parameters.ParamByName('UnitID').Value := AMaterial.UnitId;
    Query.Parameters.ParamByName('ProductID').Value := LProductIDValue; // Assign Variant for NULL handling

    // If it's an UPDATE, add the ID parameter for the WHERE clause.
    if AMaterial.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := AMaterial.Id;

    // Execute the SQL command.
    Query.ExecSQL;

    // If it was an INSERT, retrieve the newly generated ID.
    if (AMaterial.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open; // Open to read the result of SCOPE_IDENTITY()
      if not Query.EOF then
      begin
        AMaterial.Id := Query.FieldByName('NewID').AsInteger; // Update the object's ID
      end;
      Query.Close; // Close the query after reading the ID
    end;

    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error saving material: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TMaterialManager.DeleteMaterial(AMaterialId: Integer): Boolean;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for deleting a material by ID.
    Query.SQL.Text :=
      'DELETE FROM Material ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AMaterialId;

    // Execute the SQL command.
    Query.ExecSQL;
    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error deleting material: ' + E.Message);
      Result := False;
    end;
  end;
end;

end.
