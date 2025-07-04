unit BLC_Part;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Part,            // Include the Part Model class
  BLC_BaseService;   // Include the Base Service class

type
  // TPartManager handles the business logic and database interactions
  // for the 'Part' entity. It inherits TADOQuery functionality from TBaseService.
  TPartManager = class(TBaseService)
  public
    // Constructor: Passes the ADOConnection to the base class.
    constructor Create(AConnection: TADOConnection);

    // Loads a TPart object from the database based on its ID.
    // Returns nil if the part is not found.
    function LoadPartById(APartId: Integer): TPart;

    // Saves a TPart object to the database.
    // If Part.Id is 0, it performs an INSERT (creates a new record).
    // If Part.Id is > 0, it performs an UPDATE (modifies an existing record).
    // Returns True on success, False on failure.
    // On successful insert, updates Part.Id with the new generated ID.
    function SavePart(APart: TPart): Boolean;

    // Deletes a part record from the database based on its ID.
    // Returns True on success, False on failure.
    function DeletePart(APartId: Integer): Boolean;
  end;

implementation

{ TPartManager }

constructor TPartManager.Create(AConnection: TADOConnection);
begin
  // Call the base class constructor to initialize the TADOQuery with the connection.
  inherited Create(AConnection);
end;

function TPartManager.LoadPartById(APartId: Integer): TPart;
var
  LPart: TPart;
begin
  Result := nil; // Default to nil, indicating not found

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for selecting a part by ID.
    Query.SQL.Text :=
      'SELECT ID, PartName, IsConsumable, Description, PriceID, UnitID ' +
      'FROM Part ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := APartId;

    // Execute the query to open the dataset.
    Query.Open;

    // Check if a record was found.
    if not Query.EOF then
    begin
      // Create a new TPart object and populate its properties
      // from the query's current record.
      LPart := TPart.Create;
      LPart.Id := Query.FieldByName('ID').AsInteger;
      LPart.PartName := Query.FieldByName('PartName').AsString;
      LPart.IsConsumable := Query.FieldByName('IsConsumable').AsBoolean;
      LPart.Description := Query.FieldByName('Description').AsString;
      LPart.PriceId := Query.FieldByName('PriceID').AsInteger;
      LPart.UnitId := Query.FieldByName('UnitID').AsInteger;
      Result := LPart; // Return the loaded part object
    end;
  except
    on E: Exception do
    begin
      // Log or handle the exception appropriately.
      ShowMessage('Error loading part: ' + E.Message);
      // Ensure Result is nil if an error occurs.
      FreeAndNil(LPart);
      Result := nil;
    end;
  end;
end;

function TPartManager.SavePart(APart: TPart): Boolean;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Check if it's an INSERT (new record) or UPDATE (existing record).
    if APart.Id = 0 then // INSERT operation
    begin
      Query.SQL.Text :=
        'INSERT INTO Part (PartName, IsConsumable, Description, PriceID, UnitID) ' +
        'VALUES (:PartName, :IsConsumable, :Description, :PriceID, :UnitID); ' +
        'SELECT SCOPE_IDENTITY() AS NewID;'; // For SQL Server to get the new ID
    else // UPDATE operation
    begin
      Query.SQL.Text :=
        'UPDATE Part ' +
        'SET PartName = :PartName, IsConsumable = :IsConsumable, ' +
        'Description = :Description, PriceID = :PriceID, UnitID = :UnitID ' +
        'WHERE ID = :ID;';
    end;

    // Clear existing parameters.
    Query.Parameters.Clear;

    // Set parameters from the APart object.
    Query.Parameters.ParamByName('PartName').Value := APart.PartName;
    Query.Parameters.ParamByName('IsConsumable').Value := APart.IsConsumable;
    Query.Parameters.ParamByName('Description').Value := APart.Description;
    Query.Parameters.ParamByName('PriceID').Value := APart.PriceId;
    Query.Parameters.ParamByName('UnitID').Value := APart.UnitId;

    // If it's an UPDATE, add the ID parameter for the WHERE clause.
    if APart.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := APart.Id;

    // Execute the SQL command.
    Query.ExecSQL;

    // If it was an INSERT, retrieve the newly generated ID.
    if (APart.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open; // Open to read the result of SCOPE_IDENTITY()
      if not Query.EOF then
      begin
        APart.Id := Query.FieldByName('NewID').AsInteger; // Update the object's ID
      end;
      Query.Close; // Close the query after reading the ID
    end;

    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error saving part: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TPartManager.DeletePart(APartId: Integer): Boolean;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for deleting a part by ID.
    Query.SQL.Text :=
      'DELETE FROM Part ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := APartId;

    // Execute the SQL command.
    Query.ExecSQL;
    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error deleting part: ' + E.Message);
      Result := False;
    end;
  end;
end;

end.
