unit BLC_Service;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Service,         // Include the Service Model class
  BLC_BaseService;   // Include the Base Service class

type
  // TServiceManager handles the business logic and database interactions
  // for the 'Service' entity. It inherits TADOQuery functionality from TBaseService.
  TServiceManager = class(TBaseService)
  public
    // Constructor: Passes the ADOConnection to the base class.
    constructor Create(AConnection: TADOConnection);

    // Loads a TService object from the database based on its ID.
    // Returns nil if the service is not found.
    function LoadServiceById(AServiceId: Integer): TService;

    // Saves a TService object to the database.
    // If Service.Id is 0, it performs an INSERT (creates a new record).
    // If Service.Id is > 0, it performs an UPDATE (modifies an existing record).
    // Returns True on success, False on failure.
    // On successful insert, updates Service.Id with the new generated ID.
    function SaveService(AService: TService): Boolean;

    // Deletes a service record from the database based on its ID.
    // Returns True on success, False on failure.
    function DeleteService(AServiceId: Integer): Boolean;
  end;

implementation

{ TServiceManager }

constructor TServiceManager.Create(AConnection: TADOConnection);
begin
  // Call the base class constructor to initialize the TADOQuery with the connection.
  inherited Create(AConnection);
end;

function TServiceManager.LoadServiceById(AServiceId: Integer): TService;
var
  LService: TService;
begin
  Result := nil; // Default to nil, indicating not found

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for selecting a service by ID.
    Query.SQL.Text :=
      'SELECT ID, ServiceName, Description, PriceID, UnitID ' +
      'FROM Service ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AServiceId;

    // Execute the query to open the dataset.
    Query.Open;

    // Check if a record was found.
    if not Query.EOF then
    begin
      // Create a new TService object and populate its properties
      // from the query's current record.
      LService := TService.Create;
      LService.Id := Query.FieldByName('ID').AsInteger;
      LService.ServiceName := Query.FieldByName('ServiceName').AsString;
      LService.Description := Query.FieldByName('Description').AsString;
      LService.PriceId := Query.FieldByName('PriceID').AsInteger;
      LService.UnitId := Query.FieldByName('UnitID').AsInteger;
      Result := LService; // Return the loaded service object
    end;
  except
    on E: Exception do
    begin
      // Log or handle the exception appropriately.
      ShowMessage('Error loading service: ' + E.Message);
      // Ensure Result is nil if an error occurs.
      FreeAndNil(LService);
      Result := nil;
    end;
  end;
end;

function TServiceManager.SaveService(AService: TService): Boolean;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Check if it's an INSERT (new record) or UPDATE (existing record).
    if AService.Id = 0 then // INSERT operation
    begin
      Query.SQL.Text :=
        'INSERT INTO Service (ServiceName, Description, PriceID, UnitID) ' +
        'VALUES (:ServiceName, :Description, :PriceID, :UnitID); ' +
        'SELECT SCOPE_IDENTITY() AS NewID;'; // For SQL Server to get the new ID
    else // UPDATE operation
    begin
      Query.SQL.Text :=
        'UPDATE Service ' +
        'SET ServiceName = :ServiceName, Description = :Description, ' +
        'PriceID = :PriceID, UnitID = :UnitID ' +
        'WHERE ID = :ID;';
    end;

    // Clear existing parameters.
    Query.Parameters.Clear;

    // Set parameters from the AService object.
    Query.Parameters.ParamByName('ServiceName').Value := AService.ServiceName;
    Query.Parameters.ParamByName('Description').Value := AService.Description;
    Query.Parameters.ParamByName('PriceID').Value := AService.PriceId;
    Query.Parameters.ParamByName('UnitID').Value := AService.UnitId;

    // If it's an UPDATE, add the ID parameter for the WHERE clause.
    if AService.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := AService.Id;

    // Execute the SQL command.
    Query.ExecSQL;

    // If it was an INSERT, retrieve the newly generated ID.
    if (AService.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open; // Open to read the result of SCOPE_IDENTITY()
      if not Query.EOF then
      begin
        AService.Id := Query.FieldByName('NewID').AsInteger; // Update the object's ID
      end;
      Query.Close; // Close the query after reading the ID
    end;

    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error saving service: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TServiceManager.DeleteService(AServiceId: Integer): Boolean;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for deleting a service by ID.
    Query.SQL.Text :=
      'DELETE FROM Service ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AServiceId;

    // Execute the SQL command.
    Query.ExecSQL;
    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error deleting service: ' + E.Message);
      Result := False;
    end;
  end;
end;

end.
