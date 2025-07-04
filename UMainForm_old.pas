unit UMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.DBGrids,
  Vcl.StdCtrls, Data.Win.ADODB, Data.DB, // Data.DB is needed for TDataSource and TDataSet
  M_Price, M_Unit, M_Part, M_Service, M_Material, M_Product, // Model classes
  BLC_Price, BLC_Unit, BLC_Part, BLC_Service, BLC_Material, BLC_Product, // Business Logic classes
  UAppUtils; // Utility functions

type
  TMainForm = class(TForm)
    Label1: TLabel;
    DBGrid_Material: TDBGrid;
    Label2: TLabel;
    DBGrid_Product: TDBGrid;
    Label3: TLabel;
    DBGrid_Part: TDBGrid;
    Label4: TLabel;
    DBGrid_Service: TDBGrid;
    ButtonNew: TButton;
    ButtonDelete: TButton;
    ButtonSave: TButton;
    ADOConnection1: TADOConnection;
    ADOQuery_Material: TADOQuery;
    DataSource_Material: TDataSource;
    ADOQuery_Product: TADOQuery;
    DataSource_Product: TDataSource;
    ADOQuery_Part: TADOQuery;
    DataSource_Part: TDataSource;
    ADOQuery_Service: TADOQuery;
    DataSource_Service: TDataSource;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    // Event handlers for master-detail filtering
    procedure ADOQuery_MaterialAfterScroll(DataSet: TDataSet);
    procedure ADOQuery_ProductAfterScroll(DataSet: TDataSet);
  private
    { Private declarations }
    FPriceManager: TPriceManager;
    FUnitManager: TUnitManager;
    FPartManager: TPartManager;
    FServiceManager: TServiceManager;
    FMaterialManager: TMaterialManager;
    FProductManager: TProductManager;

    // Helper function to get the currently active DBGrid
    function GetActiveDBGrid: TDBGrid;
    // Helper function to get the TADOQuery associated with a DBGrid
    function GetQueryForDBGrid(ADbGrid: TDBGrid): TADOQuery;
    // Helper function to get the TDataSource associated with a DBGrid
    function GetDataSourceForDBGrid(ADbGrid: TDBGrid): TDataSource;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // 1. Establish Database Connection
  try
    ADOConnection1.Connected := True;
    ShowMessage('Database connection established successfully!');
  except
    on E: Exception do
    begin
      ShowMessage('Failed to connect to database: ' + E.Message);
      Application.Terminate; // Exit application if connection fails
      Exit;
    end;
  end;

  // 2. Instantiate Business Logic Managers
  // Pass the shared ADOConnection to each manager.
  FPriceManager := TPriceManager.Create(ADOConnection1);
  FUnitManager := TUnitManager.Create(ADOConnection1);
  FPartManager := TPartManager.Create(ADOConnection1);
  FServiceManager := TServiceManager.Create(ADOConnection1);
  FMaterialManager := TMaterialManager.Create(ADOConnection1);
  FProductManager := TProductManager.Create(ADOConnection1);

  // 3. Configure and Open ADOQueries
  // Ensure CursorLocation is clUseClient for better editing/appending
  // Set ParamCheck to False for detail queries to avoid errors when params are not yet set.
  ADOQuery_Material.CursorLocation := clUseClient;
  ADOQuery_Material.Open;

  ADOQuery_Product.CursorLocation := clUseClient;
  ADOQuery_Product.ParamCheck := False; // Important for parameterized queries
  // ADOQuery_Product will be opened by ADOQuery_MaterialAfterScroll

  ADOQuery_Part.CursorLocation := clUseClient;
  ADOQuery_Part.ParamCheck := False; // Important for parameterized queries
  // ADOQuery_Part will be opened by ADOQuery_ProductAfterScroll

  ADOQuery_Service.CursorLocation := clUseClient;
  ADOQuery_Service.ParamCheck := False; // Important for parameterized queries
  // ADOQuery_Service will be opened by ADOQuery_ProductAfterScroll

  // Manually trigger the first filter for Product, Part, and Service grids
  // This ensures they are populated correctly when the form loads.
  ADOQuery_MaterialAfterScroll(ADOQuery_Material);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // 1. Free Business Logic Managers
  FreeAndNil(FProductManager);
  FreeAndNil(FMaterialManager);
  FreeAndNil(FServiceManager);
  FreeAndNil(FPartManager);
  FreeAndNil(FUnitManager);
  FreeAndNil(FPriceManager);

  // 2. Close Database Connection
  if ADOConnection1.Connected then
    ADOConnection1.Connected := False;
end;

function TMainForm.GetActiveDBGrid: TDBGrid;
begin
  // Determine which DBGrid has focus to apply actions to it.
  if DBGrid_Material.Focused then Result := DBGrid_Material
  else if DBGrid_Product.Focused then Result := DBGrid_Product
  else if DBGrid_Part.Focused then Result := DBGrid_Part
  else if DBGrid_Service.Focused then Result := DBGrid_Service
  else Result := nil;
end;

function TMainForm.GetQueryForDBGrid(ADbGrid: TDBGrid): TADOQuery;
begin
  if Assigned(ADbGrid.DataSource) and Assigned(ADbGrid.DataSource.DataSet) then
  begin
    Result := ADbGrid.DataSource.DataSet as TADOQuery;
  end
  else
    Result := nil;
end;

function TMainForm.GetDataSourceForDBGrid(ADbGrid: TDBGrid): TDataSource;
begin
  Result := ADbGrid.DataSource;
end;

procedure TMainForm.ButtonNewClick(Sender: TObject);
var
  ActiveQuery: TADOQuery;
begin
  ActiveQuery := GetQueryForDBGrid(GetActiveDBGrid);
  if Assigned(ActiveQuery) then
  begin
    try
      ActiveQuery.Append; // Puts the dataset in dsInsert state, ready for new row
      // Set default values for new row, especially for FKs if they are NOT NULL in DB
      // User will need to manually enter valid IDs for FKs in the grid.
      // For example:
      if ActiveQuery.Name = 'ADOQuery_Material' then
      begin
        ActiveQuery.FieldByName('MaterialName').AsString := 'New Material';
        ActiveQuery.FieldByName('Quantity').AsCurrency := 0.0;
        ActiveQuery.FieldByName('CurrentStock').AsCurrency := 0.0;
        ActiveQuery.FieldByName('MinStockLevel').AsCurrency := 0.0;
        ActiveQuery.FieldByName('PriceID').AsInteger := 0; // Placeholder, user must change
        ActiveQuery.FieldByName('UnitID').AsInteger := 0;   // Placeholder, user must change
        ActiveQuery.FieldByName('ProductID').AsVariant := Null; // Can be NULL
      end
      else if ActiveQuery.Name = 'ADOQuery_Product' then
      begin
        ActiveQuery.FieldByName('ProductName').AsString := 'New Product';
        ActiveQuery.FieldByName('ProductCode').AsString := '';
        ActiveQuery.FieldByName('CurrentStock').AsCurrency := 0.0;
        ActiveQuery.FieldByName('IsFinalProduct').AsBoolean := False;
        ActiveQuery.FieldByName('PriceID').AsInteger := 0;
        ActiveQuery.FieldByName('UnitID').AsInteger := 0;
        ActiveQuery.FieldByName('PartID').AsVariant := Null;
        ActiveQuery.FieldByName('ServiceID').AsVariant := Null;
      end
      else if ActiveQuery.Name = 'ADOQuery_Part' then
      begin
        ActiveQuery.FieldByName('PartName').AsString := 'New Part';
        ActiveQuery.FieldByName('IsConsumable').AsBoolean := True;
        ActiveQuery.FieldByName('PriceID').AsInteger := 0;
        ActiveQuery.FieldByName('UnitID').AsInteger := 0;
      end
      else if ActiveQuery.Name = 'ADOQuery_Service' then
      begin
        ActiveQuery.FieldByName('ServiceName').AsString := 'New Service';
        ActiveQuery.FieldByName('PriceID').AsInteger := 0;
        ActiveQuery.FieldByName('UnitID').AsInteger := 0;
      end;
      // The user can now type into the new row in the DBGrid.
    except
      on E: Exception do
        ShowMessage('Error creating new row: ' + E.Message);
    end;
  else
    ShowMessage('No DBGrid is active.');
end;

procedure TMainForm.ButtonDeleteClick(Sender: TObject);
var
  ActiveQuery: TADOQuery;
  LID: Integer;
  Success: Boolean;
begin
  ActiveQuery := GetQueryForDBGrid(GetActiveDBGrid);
  if Assigned(ActiveQuery) and not ActiveQuery.EOF and not ActiveQuery.BOF then
  begin
    if MessageDlg('Are you sure you want to delete the selected record?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      // Get the ID of the current record before deleting it from the dataset
      LID := ActiveQuery.FieldByName('ID').AsInteger;
      Success := False;

      // Use the appropriate business logic manager to delete from the database
      try
        if ActiveQuery.Name = 'ADOQuery_Material' then
          Success := FMaterialManager.DeleteMaterial(LID)
        else if ActiveQuery.Name = 'ADOQuery_Product' then
          Success := FProductManager.DeleteProduct(LID)
        else if ActiveQuery.Name = 'ADOQuery_Part' then
          Success := FPartManager.DeletePart(LID)
        else if ActiveQuery.Name = 'ADOQuery_Service' then
          Success := FServiceManager.DeleteService(LID);

        if Success then
        begin
          ActiveQuery.Delete; // Delete the row from the client-side dataset
          ActiveQuery.Post;   // Post the deletion to the database (ADO handles this via LockType=ltOptimistic)
          ShowMessage('Record deleted successfully.');
        end
        else
        begin
          ShowMessage('Failed to delete record from database.');
          ActiveQuery.Cancel; // Revert changes if DB operation failed
        end;
      except
        on E: Exception do
        begin
          ShowMessage('Error deleting record: ' + E.Message);
          ActiveQuery.Cancel; // Revert changes on error
        end;
      end;
    end;
  else
    ShowMessage('No record selected to delete.');
end;

procedure TMainForm.ButtonSaveClick(Sender: TObject);
var
  ActiveQuery: TADOQuery;
  LMaterial: TMaterial;
  LProduct: TProduct;
  LPart: TPart;
  LService: TService;
  Success: Boolean;
begin
  ActiveQuery := GetQueryForDBGrid(GetActiveDBGrid);
  if Assigned(ActiveQuery) then
  begin
    // Check if the dataset is in edit or insert mode
    if (ActiveQuery.State = dsEdit) or (ActiveQuery.State = dsInsert) then
    begin
      try
        // Attempt to post changes from the DBGrid to the dataset buffer
        ActiveQuery.Post;
        Success := False;

        // Create model object from current row data and save using manager
        if ActiveQuery.Name = 'ADOQuery_Material' then
        begin
          LMaterial := TMaterial.Create;
          try
            LMaterial.Id := ActiveQuery.FieldByName('ID').AsInteger;
            LMaterial.MaterialName := ActiveQuery.FieldByName('MaterialName').AsString;
            LMaterial.Quantity := ActiveQuery.FieldByName('Quantity').AsCurrency;
            LMaterial.CurrentStock := ActiveQuery.FieldByName('CurrentStock').AsCurrency;
            LMaterial.MinStockLevel := ActiveQuery.FieldByName('MinStockLevel').AsCurrency;
            LMaterial.Description := ActiveQuery.FieldByName('Description').AsString;
            LMaterial.PriceId := ActiveQuery.FieldByName('PriceID').AsInteger;
            LMaterial.UnitId := ActiveQuery.FieldByName('UnitID').AsInteger;
            // Handle nullable ProductID
            if not ActiveQuery.FieldByName('ProductID').IsNull then
              LMaterial.ProductId := ActiveQuery.FieldByName('ProductID').AsInteger
            else
              LMaterial.ProductId := 0;

            Success := FMaterialManager.SaveMaterial(LMaterial);
            if Success and (LMaterial.Id <> ActiveQuery.FieldByName('ID').AsInteger) then
            begin
              // If it was a new record, update the ID in the grid after successful save
              ActiveQuery.FieldByName('ID').AsInteger := LMaterial.Id;
            end;
          finally
            FreeAndNil(LMaterial);
          end;
        end
        else if ActiveQuery.Name = 'ADOQuery_Product' then
        begin
          LProduct := TProduct.Create;
          try
            LProduct.Id := ActiveQuery.FieldByName('ID').AsInteger;
            LProduct.ProductName := ActiveQuery.FieldByName('ProductName').AsString;
            LProduct.ProductCode := ActiveQuery.FieldByName('ProductCode').AsString;
            LProduct.CurrentStock := ActiveQuery.FieldByName('CurrentStock').AsCurrency;
            LProduct.IsFinalProduct := ActiveQuery.FieldByName('IsFinalProduct').AsBoolean;
            LProduct.Description := ActiveQuery.FieldByName('Description').AsString;
            LProduct.PriceId := ActiveQuery.FieldByName('PriceID').AsInteger;
            LProduct.UnitId := ActiveQuery.FieldByName('UnitID').AsInteger;
            // Handle nullable PartID, ServiceID
            if not ActiveQuery.FieldByName('PartID').IsNull then
              LProduct.PartId := ActiveQuery.FieldByName('PartID').AsInteger
            else
              LProduct.PartId := 0;
            if not ActiveQuery.FieldByName('ServiceID').IsNull then
              LProduct.ServiceId := ActiveQuery.FieldByName('ServiceID').AsInteger
            else
              LProduct.ServiceId := 0;

            Success := FProductManager.SaveProduct(LProduct);
            if Success and (LProduct.Id <> ActiveQuery.FieldByName('ID').AsInteger) then
            begin
              ActiveQuery.FieldByName('ID').AsInteger := LProduct.Id;
            end;
          finally
            FreeAndNil(LProduct);
          end;
        end
        else if ActiveQuery.Name = 'ADOQuery_Part' then
        begin
          LPart := TPart.Create;
          try
            LPart.Id := ActiveQuery.FieldByName('ID').AsInteger;
            LPart.PartName := ActiveQuery.FieldByName('PartName').AsString;
            LPart.IsConsumable := ActiveQuery.FieldByName('IsConsumable').AsBoolean;
            LPart.Description := ActiveQuery.FieldByName('Description').AsString;
            LPart.PriceId := ActiveQuery.FieldByName('PriceID').AsInteger;
            LPart.UnitId := ActiveQuery.FieldByName('UnitID').AsInteger;

            Success := FPartManager.SavePart(LPart);
            if Success and (LPart.Id <> ActiveQuery.FieldByName('ID').AsInteger) then
            begin
              ActiveQuery.FieldByName('ID').AsInteger := LPart.Id;
            end;
          finally
            FreeAndNil(LPart);
          end;
        end
        else if ActiveQuery.Name = 'ADOQuery_Service' then
        begin
          LService := TService.Create;
          try
            LService.Id := ActiveQuery.FieldByName('ID').AsInteger;
            LService.ServiceName := ActiveQuery.FieldByName('ServiceName').AsString;
            LService.Description := ActiveQuery.FieldByName('Description').AsString;
            LService.PriceId := ActiveQuery.FieldByName('PriceID').AsInteger;
            LService.UnitId := ActiveQuery.FieldByName('UnitID').AsInteger;

            Success := FServiceManager.SaveService(LService);
            if Success and (LService.Id <> ActiveQuery.FieldByName('ID').AsInteger) then
            begin
              ActiveQuery.FieldByName('ID').AsInteger := LService.Id;
            end;
          finally
            FreeAndNil(LService);
          end;
        end;

        if Success then
          ShowMessage('Record saved successfully.')
        else
          ShowMessage('Failed to save record to database.');

      except
        on E: Exception do
        begin
          ShowMessage('Error saving record: ' + E.Message);
          ActiveQuery.Cancel; // Revert changes in the grid if error occurs
        end;
      end;
    else
      ShowMessage('No changes to save or dataset is not in edit/insert mode.');
    end;
  else
    ShowMessage('No DBGrid is active.');
end;

procedure TMainForm.ADOQuery_MaterialAfterScroll(DataSet: TDataSet);
var
  LProductID: Integer;
begin
  // This event fires when the current record in ADOQuery_Material changes.
  // We use it to filter the ADOQuery_Product.

  // Close the Product query before setting new parameters and opening.
  if ADOQuery_Product.Active then
    ADOQuery_Product.Close;

  // Check if the current Material record has a ProductID and if it's not NULL.
  if not DataSet.FieldByName('ProductID').IsNull then
  begin
    LProductID := DataSet.FieldByName('ProductID').AsInteger;

    // Set the SQL for ADOQuery_Product to filter by ProductID.
    ADOQuery_Product.SQL.Text :=
      'SELECT ID, ProductName, ProductCode, CurrentStock, IsFinalProduct, ' +
      'Description, PriceID, UnitID, PartID, ServiceID ' +
      'FROM Product ' +
      'WHERE ID = :ProductID';

    // Set the parameter for the ProductID.
    ADOQuery_Product.Parameters.ParamByName('ProductID').Value := LProductID;

    // Open the Product query to display the filtered data.
    ADOQuery_Product.Open;
  end
  else
  begin
    // If ProductID is NULL, show an empty Product grid.
    // We can set a condition that will never be true, or just clear the SQL.
    ADOQuery_Product.SQL.Text := 'SELECT * FROM Product WHERE 1 = 0'; // Returns no rows
    ADOQuery_Product.Open;
  end;

  // After filtering products, also trigger the product after scroll to filter parts/services.
  // This ensures that when a material is selected, and its related product is loaded,
  // the part and service details for that product are also updated.
  ADOQuery_ProductAfterScroll(ADOQuery_Product);
end;

procedure TMainForm.ADOQuery_ProductAfterScroll(DataSet: TDataSet);
var
  LPartID: Integer;
  LServiceID: Integer;
begin
  // This event fires when the current record in ADOQuery_Product changes.
  // We use it to filter ADOQuery_Part and ADOQuery_Service.

  // Close Part and Service queries before setting new parameters and opening.
  if ADOQuery_Part.Active then
    ADOQuery_Part.Close;
  if ADOQuery_Service.Active then
    ADOQuery_Service.Close;

  // Check if a product record is selected (not BOF/EOF)
  if not DataSet.EOF and not DataSet.BOF then
  begin
    // --- Filter Part Grid ---
    if not DataSet.FieldByName('PartID').IsNull then
    begin
      LPartID := DataSet.FieldByName('PartID').AsInteger;
      ADOQuery_Part.SQL.Text := 'SELECT ID, PartName, IsConsumable, Description, PriceID, UnitID FROM Part WHERE ID = :PartID';
      ADOQuery_Part.Parameters.ParamByName('PartID').Value := LPartID;
      ADOQuery_Part.Open;
    end
    else
    begin
      ADOQuery_Part.SQL.Text := 'SELECT * FROM Part WHERE 1 = 0'; // No Part linked, show empty
      ADOQuery_Part.Open;
    end;

    // --- Filter Service Grid ---
    if not DataSet.FieldByName('ServiceID').IsNull then
    begin
      LServiceID := DataSet.FieldByName('ServiceID').AsInteger;
      ADOQuery_Service.SQL.Text := 'SELECT ID, ServiceName, Description, PriceID, UnitID FROM Service WHERE ID = :ServiceID';
      ADOQuery_Service.Parameters.ParamByName('ServiceID').Value := LServiceID;
      ADOQuery_Service.Open;
    end
    else
    begin
      ADOQuery_Service.SQL.Text := 'SELECT * FROM Service WHERE 1 = 0'; // No Service linked, show empty
      ADOQuery_Service.Open;
    end;
  end
  else
  begin
    // If no product record is selected (e.g., product grid is empty),
    // clear the Part and Service grids.
    ADOQuery_Part.SQL.Text := 'SELECT * FROM Part WHERE 1 = 0';
    ADOQuery_Part.Open;
    ADOQuery_Service.SQL.Text := 'SELECT * FROM Service WHERE 1 = 0';
    ADOQuery_Service.Open;
  end;
end;

end.
