unit BLC_Unit;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Unit,            // Include the Unit Model class
  BLC_BaseService;   // Include the Base Service class

type
  // TUnitManager handles the business logic and database interactions
  // for the 'Unit' entity. It inherits TADOQuery functionality from TBaseService.
  TUnitManager = class(TBaseService)
  public
    // Constructor: Passes the ADOConnection to the base class.
    constructor Create(AConnection: TADOConnection);

    // Loads a TUnit object from the database based on its ID.
    // Returns nil if the unit is not found.
    function LoadUnitById(AUnitId: Integer): TUnit;

    // Saves a TUnit object to the database.
    // If Unit.Id is 0, it performs an INSERT (creates a new record).
    // If Unit.Id is > 0, it performs an UPDATE (modifies an existing record).
    // Returns True on success, False on failure.
    // On successful insert, updates Unit.Id with the new generated ID.
    function SaveUnit(AUnit: TUnit): Boolean;

    // Deletes a unit record from the database based on its ID.
    // Returns True on success, False on failure.
    function DeleteUnit(AUnitId: Integer): Boolean;
  end;

implementation

{ TUnitManager }

constructor TUnitManager.Create(AConnection: TADOConnection);
begin
  // Call the base class constructor to initialize the TADOQuery with the connection.
  inherited Create(AConnection);
end;

function TUnitManager.LoadUnitById(AUnitId: Integer): TUnit;
var
  LUnit: TUnit;
begin
  Result := nil; // Default to nil, indicating not found

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for selecting a unit by ID.
    Query.SQL.Text :=
      'SELECT ID, Name, Description ' +
      'FROM Unit ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AUnitId;

    // Execute the query to open the dataset.
    Query.Open;

    // Check if a record was found.
    if not Query.EOF then
    begin
      // Create a new TUnit object and populate its properties
      // from the query's current record.
      LUnit := TUnit.Create;
      LUnit.Id := Query.FieldByName('ID').AsInteger;
      LUnit.Name := Query.FieldByName('Name').AsString;
      LUnit.Description := Query.FieldByName('Description').AsString;
      Result := LUnit; // Return the loaded unit object
    end;
  except
    on E: Exception do
    begin
      // Log or handle the exception appropriately.
      ShowMessage('Error loading unit: ' + E.Message);
      // Ensure Result is nil if an error occurs.
      FreeAndNil(LUnit);
      Result := nil;
    end;
  end;
end;

function TUnitManager.SaveUnit(AUnit: TUnit): Boolean;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Check if it's an INSERT (new record) or UPDATE (existing record).
    if AUnit.Id = 0 then // INSERT operation
    begin
      Query.SQL.Text :=
        'INSERT INTO Unit (Name, Description) ' +
        'VALUES (:Name, :Description); ' +
        'SELECT SCOPE_IDENTITY() AS NewID;'; // For SQL Server to get the new ID
    else // UPDATE operation
    begin
      Query.SQL.Text :=
        'UPDATE Unit ' +
        'SET Name = :Name, Description = :Description ' +
        'WHERE ID = :ID;';
    end;

    // Clear existing parameters.
    Query.Parameters.Clear;

    // Set parameters from the AUnit object.
    Query.Parameters.ParamByName('Name').Value := AUnit.Name;
    Query.Parameters.ParamByName('Description').Value := AUnit.Description;

    // If it's an UPDATE, add the ID parameter for the WHERE clause.
    if AUnit.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := AUnit.Id;

    // Execute the SQL command.
    Query.ExecSQL;

    // If it was an INSERT, retrieve the newly generated ID.
    if (AUnit.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open; // Open to read the result of SCOPE_IDENTITY()
      if not Query.EOF then
      begin
        AUnit.Id := Query.FieldByName('NewID').AsInteger; // Update the object's ID
      end;
      Query.Close; // Close the query after reading the ID
    end;

    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error saving unit: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TUnitManager.DeleteUnit(AUnitId: Integer): Boolean;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for deleting a unit by ID.
    Query.SQL.Text :=
      'DELETE FROM Unit ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AUnitId;

    // Execute the SQL command.
    Query.ExecSQL;
    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error deleting unit: ' + E.Message);
      Result := False;
    end;
  end;
end;

end.
