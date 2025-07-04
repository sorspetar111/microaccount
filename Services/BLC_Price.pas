unit BLC_Price;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Price,           // Include the Price Model class
  BLC_BaseService;   // Include the Base Service class

type
  // TPriceManager handles the business logic and database interactions
  // for the 'Price' entity. It inherits TADOQuery functionality from TBaseService.
  TPriceManager = class(TBaseService)
  public
    // Constructor: Passes the ADOConnection to the base class.
    constructor Create(AConnection: TADOConnection);

    // Loads a TPrice object from the database based on its ID.
    // Returns nil if the price is not found.
    function LoadPriceById(APriceId: Integer): TPrice;

    // Saves a TPrice object to the database.
    // If Price.Id is 0, it performs an INSERT (creates a new record).
    // If Price.Id is > 0, it performs an UPDATE (modifies an existing record).
    // Returns True on success, False on failure.
    // On successful insert, updates Price.Id with the new generated ID.
    function SavePrice(APrice: TPrice): Boolean;

    // Deletes a price record from the database based on its ID.
    // Returns True on success, False on failure.
    function DeletePrice(APriceId: Integer): Boolean;
  end;

implementation

{ TPriceManager }

constructor TPriceManager.Create(AConnection: TADOConnection);
begin
  // Call the base class constructor to initialize the TADOQuery with the connection.
  inherited Create(AConnection);
end;

function TPriceManager.LoadPriceById(APriceId: Integer): TPrice;
var
  LPrice: TPrice;
begin
  Result := nil; // Default to nil, indicating not found

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for selecting a price by ID.
    Query.SQL.Text :=
      'SELECT ID, Price, Currency, Description ' +
      'FROM Price ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := APriceId;

    // Execute the query to open the dataset.
    Query.Open;

    // Check if a record was found.
    if not Query.EOF then
    begin
      // Create a new TPrice object and populate its properties
      // from the query's current record.
      LPrice := TPrice.Create;
      LPrice.Id := Query.FieldByName('ID').AsInteger;
      LPrice.Price := Query.FieldByName('Price').AsCurrency;
      LPrice.Currency := Query.FieldByName('Currency').AsString;
      LPrice.Description := Query.FieldByName('Description').AsString;
      Result := LPrice; // Return the loaded price object
    end;
  except
    on E: Exception do
    begin
      // Log or handle the exception appropriately.
      // For now, we'll just show a message.
      ShowMessage('Error loading price: ' + E.Message);
      // Ensure Result is nil if an error occurs.
      FreeAndNil(LPrice);
      Result := nil;
    end;
  end;
end;

function TPriceManager.SavePrice(APrice: TPrice): Boolean;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Check if it's an INSERT (new record) or UPDATE (existing record).
    if APrice.Id = 0 then // INSERT operation
    begin
      Query.SQL.Text :=
        'INSERT INTO Price (Price, Currency, Description) ' +
        'VALUES (:Price, :Currency, :Description); ' +
        'SELECT SCOPE_IDENTITY() AS NewID;'; // For SQL Server to get the new ID
    else // UPDATE operation
    begin
      Query.SQL.Text :=
        'UPDATE Price ' +
        'SET Price = :Price, Currency = :Currency, Description = :Description ' +
        'WHERE ID = :ID;';
    end;

    // Clear existing parameters.
    Query.Parameters.Clear;

    // Set parameters from the APrice object.
    Query.Parameters.ParamByName('Price').Value := APrice.Price;
    Query.Parameters.ParamByName('Currency').Value := APrice.Currency;
    Query.Parameters.ParamByName('Description').Value := APrice.Description;

    // If it's an UPDATE, add the ID parameter for the WHERE clause.
    if APrice.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := APrice.Id;

    // Execute the SQL command.
    Query.ExecSQL;

    // If it was an INSERT, retrieve the newly generated ID.
    if (APrice.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open; // Open to read the result of SCOPE_IDENTITY()
      if not Query.EOF then
      begin
        APrice.Id := Query.FieldByName('NewID').AsInteger; // Update the object's ID
      end;
      Query.Close; // Close the query after reading the ID
    end;

    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error saving price: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TPriceManager.DeletePrice(APriceId: Integer): Boolean;
begin
  Result := False; // Default to failure

  // Ensure the query is closed before preparing a new one.
  if Query.Active then
    Query.Close;

  try
    // Set the SQL text for deleting a price by ID.
    Query.SQL.Text :=
      'DELETE FROM Price ' +
      'WHERE ID = :ID';

    // Clear existing parameters and add the ID parameter.
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := APriceId;

    // Execute the SQL command.
    Query.ExecSQL;
    Result := True; // Operation successful
  except
    on E: Exception do
    begin
      ShowMessage('Error deleting price: ' + E.Message);
      Result := False;
    end;
  end;
end;

end.
