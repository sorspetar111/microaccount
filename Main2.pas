unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, Grids, DBGrids, StdCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    // Master Grid (Materials)
    dbgMaterials: TDBGrid;
    ADOConnection1: TADOConnection;
    qryMaterials: TADOQuery;
    dsMaterials: TDataSource;
    
    // Detail Grids
    dbgProducts: TDBGrid;
    qryProducts: TADOQuery;
    dsProducts: TDataSource;
    
    dbgServices: TDBGrid;
    qryServices: TADOQuery;
    dsServices: TDataSource;
    
    dbgParts: TDBGrid;
    qryParts: TADOQuery;
    dsParts: TDataSource;

    // Control Panels
    Panel1: TPanel;
    btnAddMaterial: TButton;
    btnEditMaterial: TButton;
    btnDeleteMaterial: TButton;
    btnSaveMaterial: TButton;
    btnCancelMaterial: TButton;
    btnRefresh: TButton;
    
    Panel2: TPanel;
    btnAddProduct: TButton;
    btnEditProduct: TButton;
    btnDeleteProduct: TButton;
    btnSaveProduct: TButton;
    btnCancelProduct: TButton;
    
    Panel3: TPanel;
    btnAddService: TButton;
    btnEditService: TButton;
    btnDeleteService: TButton;
    btnSaveService: TButton;
    btnCancelService: TButton;
    
    Panel4: TPanel;
    btnAddPart: TButton;
    btnEditPart: TButton;
    btnDeletePart: TButton;
    btnSavePart: TButton;
    btnCancelPart: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    
    // Material buttons
    procedure btnAddMaterialClick(Sender: TObject);
    procedure btnEditMaterialClick(Sender: TObject);
    procedure btnDeleteMaterialClick(Sender: TObject);
    procedure btnSaveMaterialClick(Sender: TObject);
    procedure btnCancelMaterialClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    
    // Product buttons
    procedure btnAddProductClick(Sender: TObject);
    procedure btnEditProductClick(Sender: TObject);
    procedure btnDeleteProductClick(Sender: TObject);
    procedure btnSaveProductClick(Sender: TObject);
    procedure btnCancelProductClick(Sender: TObject);
    
    // Service buttons
    procedure btnAddServiceClick(Sender: TObject);
    procedure btnEditServiceClick(Sender: TObject);
    procedure btnDeleteServiceClick(Sender: TObject);
    procedure btnSaveServiceClick(Sender: TObject);
    procedure btnCancelServiceClick(Sender: TObject);
    
    // Part buttons
    procedure btnAddPartClick(Sender: TObject);
    procedure btnEditPartClick(Sender: TObject);
    procedure btnDeletePartClick(Sender: TObject);
    procedure btnSavePartClick(Sender: TObject);
    procedure btnCancelPartClick(Sender: TObject);
    
    // Grid events
    procedure dbgMaterialsCellClick(Column: TColumn);
    procedure dbgProductsCellClick(Column: TColumn);
    procedure dbgServicesCellClick(Column: TColumn);
    procedure dbgPartsCellClick(Column: TColumn);

  private
    { Private declarations }
    procedure SetMaterialControls(Editing: Boolean);
    procedure SetProductControls(Editing: Boolean);
    procedure SetServiceControls(Editing: Boolean);
    procedure SetPartControls(Editing: Boolean);
    procedure RefreshDetails;
    procedure SelectFirstMaterial;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Set up the ADO connection
  ADOConnection1.ConnectionString := 
    'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=MaterialsDB.mdb;Persist Security Info=False';
    
  try
    ADOConnection1.Connected := True;
    
    // Master query (Materials)
    qryMaterials.SQL.Text := 'SELECT * FROM material';
    qryMaterials.Active := True;
    
    // Configure DBGrids for single row selection
    dbgMaterials.Options := dbgMaterials.Options + [dgRowSelect] - [dgMultiSelect];
    dbgProducts.Options := dbgProducts.Options + [dgRowSelect] - [dgMultiSelect];
    dbgServices.Options := dbgServices.Options + [dgRowSelect] - [dgMultiSelect];
    dbgParts.Options := dbgParts.Options + [dgRowSelect] - [dgMultiSelect];
    
    // Detail queries
    qryProducts.SQL.Text := 'SELECT * FROM product WHERE MaterialID = :MaterialID';
    qryProducts.Active := True;
    
    qryServices.SQL.Text := 'SELECT s.* FROM service s ' +
                            'INNER JOIN product p ON s.ProductID = p.ID  ' +
                            'WHERE p.MaterialID = :MaterialID';
    qryServices.Active := True;
    
    qryParts.SQL.Text := 'SELECT pt.* FROM part pt ' +
                         'INNER JOIN product p ON pt.ProductID = p.ID  ' +
                         'WHERE  p.MaterialID = :MaterialID';
    qryParts.Active := True;
    
    // Set initial control states
    SetMaterialControls(False);
    SetProductControls(False);
    SetServiceControls(False);
    SetPartControls(False);
    
    // Select first material by default
    SelectFirstMaterial;
    
  except
    on E: Exception do
      ShowMessage('Error connecting to database: ' + E.Message);
  end;
end;

procedure TMainForm.SelectFirstMaterial;
begin
  if qryMaterials.RecordCount > 0 then
  begin
    qryMaterials.First;
    RefreshDetails;
  end;
end;

procedure TMainForm.SetMaterialControls(Editing: Boolean);
begin
  btnAddMaterial.Enabled := not Editing;
  btnEditMaterial.Enabled := not Editing and (qryMaterials.RecordCount > 0);
  btnDeleteMaterial.Enabled := not Editing and (qryMaterials.RecordCount > 0);
  btnSaveMaterial.Enabled := Editing;
  btnCancelMaterial.Enabled := Editing;
  btnRefresh.Enabled := not Editing;
  
  dbgMaterials.ReadOnly := not Editing;
end;

procedure TMainForm.SetProductControls(Editing: Boolean);
begin
  btnAddProduct.Enabled := not Editing and (qryMaterials.RecordCount > 0) and not qryMaterials.IsEmpty;
  btnEditProduct.Enabled := not Editing and (qryProducts.RecordCount > 0);
  btnDeleteProduct.Enabled := not Editing and (qryProducts.RecordCount > 0);
  btnSaveProduct.Enabled := Editing;
  btnCancelProduct.Enabled := Editing;
  
  dbgProducts.ReadOnly := not Editing;
end;

procedure TMainForm.SetServiceControls(Editing: Boolean);
begin
  btnAddService.Enabled := not Editing and (qryProducts.RecordCount > 0);
  btnEditService.Enabled := not Editing and (qryServices.RecordCount > 0);
  btnDeleteService.Enabled := not Editing and (qryServices.RecordCount > 0);
  btnSaveService.Enabled := Editing;
  btnCancelService.Enabled := Editing;
  
  dbgServices.ReadOnly := not Editing;
end;

procedure TMainForm.SetPartControls(Editing: Boolean);
begin
  btnAddPart.Enabled := not Editing and (qryProducts.RecordCount > 0);
  btnEditPart.Enabled := not Editing and (qryParts.RecordCount > 0);
  btnDeletePart.Enabled := not Editing and (qryParts.RecordCount > 0);
  btnSavePart.Enabled := Editing;
  btnCancelPart.Enabled := Editing;
  
  dbgParts.ReadOnly := not Editing;
end;

procedure TMainForm.RefreshDetails;
var
  MaterialID, ProductID: Integer;
begin
  if qryMaterials.Active and not qryMaterials.IsEmpty then
  begin
    MaterialID := qryMaterials.FieldByName('ID').AsInteger;
    
    // Refresh Products
    qryProducts.DisableControls;
    try
      qryProducts.Close;
      qryProducts.Parameters.ParamByName('MaterialID').Value := MaterialID;
      qryProducts.Open;
    finally
      qryProducts.EnableControls;
    end;
    
    ProdyctID := qryProducts.FieldByName('ID').AsInteger;
    
    // Refresh Services
    qryServices.DisableControls;
    try
      qryServices.Close;
      // qryServices.Parameters.ParamByName('ProdyctID').Value := ProdyctID;
      qryServices.Parameters.ParamByName('MaterialID').Value := MaterialID;
      qryServices.Open;
    finally
      qryServices.EnableControls;
    end;
    
    // Refresh Parts
    qryParts.DisableControls;
    try
      qryParts.Close;
      // qryParts.Parameters.ParamByName('ProdyctID').Value := ProdyctID;
      qryParts.Parameters.ParamByName('MaterialID').Value := MaterialID;
      qryParts.Open;
    finally
      qryParts.EnableControls;
    end;
  end
  else
  begin
    qryProducts.Close;
    qryServices.Close;
    qryParts.Close;
  end;
  
  SetProductControls(False);
  SetServiceControls(False);
  SetPartControls(False);
end;

// Material button events
procedure TMainForm.btnAddMaterialClick(Sender: TObject);
begin
  qryMaterials.Append;
  SetMaterialControls(True);
end;

procedure TMainForm.btnEditMaterialClick(Sender: TObject);
begin
  qryMaterials.Edit;
  SetMaterialControls(True);
end;

procedure TMainForm.btnDeleteMaterialClick(Sender: TObject);
begin
  if MessageDlg('Delete this material and all related products?', 
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    qryMaterials.Delete;
    SelectFirstMaterial;
  end;
end;

procedure TMainForm.btnSaveMaterialClick(Sender: TObject);
begin
  try
    qryMaterials.Post;
    SetMaterialControls(False);
    RefreshDetails;
  except
    on E: Exception do
    begin
      ShowMessage('Error saving material: ' + E.Message);
      qryMaterials.Cancel;
      SetMaterialControls(False);
    end;
  end;
end;

procedure TMainForm.btnCancelMaterialClick(Sender: TObject);
begin
  qryMaterials.Cancel;
  SetMaterialControls(False);
end;

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  qryMaterials.Requery;
  SelectFirstMaterial;
end;

// Product button events
procedure TMainForm.btnAddProductClick(Sender: TObject);
begin
  qryProducts.Append;
  qryProducts.FieldByName('MaterialID').AsInteger := qryMaterials.FieldByName('ID').AsInteger;
  SetProductControls(True);
end;

procedure TMainForm.btnEditProductClick(Sender: TObject);
begin
  qryProducts.Edit;
  SetProductControls(True);
end;

procedure TMainForm.btnDeleteProductClick(Sender: TObject);
begin
  if MessageDlg('Delete this product?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    qryProducts.Delete;
    RefreshDetails;
  end;
end;

procedure TMainForm.btnSaveProductClick(Sender: TObject);
begin
  try
    qryProducts.Post;
    SetProductControls(False);
    RefreshDetails;
  except
    on E: Exception do
    begin
      ShowMessage('Error saving product: ' + E.Message);
      qryProducts.Cancel;
      SetProductControls(False);
    end;
  end;
end;

procedure TMainForm.btnCancelProductClick(Sender: TObject);
begin
  qryProducts.Cancel;
  SetProductControls(False);
end;

// Service button events
procedure TMainForm.btnAddServiceClick(Sender: TObject);
begin
  qryServices.Append;
  SetServiceControls(True);
end;

procedure TMainForm.btnEditServiceClick(Sender: TObject);
begin
  qryServices.Edit;
  SetServiceControls(True);
end;

procedure TMainForm.btnDeleteServiceClick(Sender: TObject);
begin
  if MessageDlg('Delete this service?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    qryServices.Delete;
    RefreshDetails;
  end;
end;

procedure TMainForm.btnSaveServiceClick(Sender: TObject);
begin
  try
    qryServices.Post;
    SetServiceControls(False);
  except
    on E: Exception do
    begin
      ShowMessage('Error saving service: ' + E.Message);
      qryServices.Cancel;
      SetServiceControls(False);
    end;
  end;
end;

procedure TMainForm.btnCancelServiceClick(Sender: TObject);
begin
  qryServices.Cancel;
  SetServiceControls(False);
end;

// Part button events
procedure TMainForm.btnAddPartClick(Sender: TObject);
begin
  qryParts.Append;
  SetPartControls(True);
end;

procedure TMainForm.btnEditPartClick(Sender: TObject);
begin
  qryParts.Edit;
  SetPartControls(True);
end;

procedure TMainForm.btnDeletePartClick(Sender: TObject);
begin
  if MessageDlg('Delete this part?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    qryParts.Delete;
    RefreshDetails;
  end;
end;

procedure TMainForm.btnSavePartClick(Sender: TObject);
begin
  try
    qryParts.Post;
    SetPartControls(False);
  except
    on E: Exception do
    begin
      ShowMessage('Error saving part: ' + E.Message);
      qryParts.Cancel;
      SetPartControls(False);
    end;
  end;
end;

procedure TMainForm.btnCancelPartClick(Sender: TObject);
begin
  qryParts.Cancel;
  SetPartControls(False);
end;

// Grid events
procedure TMainForm.dbgMaterialsCellClick(Column: TColumn);
begin
  RefreshDetails;
end;

procedure TMainForm.dbgProductsCellClick(Column: TColumn);
begin
  // Optional: Implement if you need to react to product selection
end;

procedure TMainForm.dbgServicesCellClick(Column: TColumn);
begin
  // Optional: Implement if you need to react to service selection
end;

procedure TMainForm.dbgPartsCellClick(Column: TColumn);
begin
  // Optional: Implement if you need to react to part selection
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Check for unsaved changes in all datasets
  if qryMaterials.State in [dsEdit, dsInsert] then
  begin
    if MessageDlg('Save changes to material before closing?', 
       mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      qryMaterials.Post
    else
      qryMaterials.Cancel;
  end;
  
  if qryProducts.State in [dsEdit, dsInsert] then
  begin
    if MessageDlg('Save changes to product before closing?', 
       mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      qryProducts.Post
    else
      qryProducts.Cancel;
  end;
  
  if qryServices.State in [dsEdit, dsInsert] then
  begin
    if MessageDlg('Save changes to service before closing?', 
       mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      qryServices.Post
    else
      qryServices.Cancel;
  end;
  
  if qryParts.State in [dsEdit, dsInsert] then
  begin
    if MessageDlg('Save changes to part before closing?', 
       mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      qryParts.Post
    else
      qryParts.Cancel;
  end;
end;

end.

