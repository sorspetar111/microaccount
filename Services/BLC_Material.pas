unit BLC_Material;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Material, BLC_BaseService, BLC_Price, BLC_Unit;

type
  TMaterialManager = class(TBaseService)
  private
    FPriceManager: TPriceManager;
    FUnitManager: TUnitManager;
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    function LoadMaterialById(AMaterialId: Integer): TMaterial;
    function SaveMaterial(AMaterial: TMaterial): Boolean;
    function DeleteMaterial(AMaterialId: Integer): Boolean;
  end;

implementation

constructor TMaterialManager.Create(AConnection: TADOConnection);
begin
  inherited Create(AConnection);
  FPriceManager := TPriceManager.Create(AConnection);
  FUnitManager := TUnitManager.Create(AConnection);
end;

destructor TMaterialManager.Destroy;
begin
  FreeAndNil(FUnitManager);
  FreeAndNil(FPriceManager);
  inherited Destroy;
end;

function TMaterialManager.LoadMaterialById(AMaterialId: Integer): TMaterial;
var
  LMaterial: TMaterial;
begin
  Result := nil;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'SELECT ID, MaterialName, Quantity, UnitOfMeasure, CurrentStock, MinStockLevel, Description, PriceID FROM Material WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AMaterialId;
    Query.Open;

    if not Query.EOF then
    begin
      LMaterial := TMaterial.Create;
      LMaterial.Id := Query.FieldByName('ID').AsInteger;
      LMaterial.MaterialName := Query.FieldByName('MaterialName').AsString;
      LMaterial.Quantity := Query.FieldByName('Quantity').AsCurrency;
      LMaterial.UnitOfMeasure := Query.FieldByName('UnitOfMeasure').AsString;
      LMaterial.CurrentStock := Query.FieldByName('CurrentStock').AsCurrency;
      LMaterial.MinStockLevel := Query.FieldByName('MinStockLevel').AsCurrency;
      LMaterial.Description := Query.FieldByName('Description').AsString;
      LMaterial.PriceId := Query.FieldByName('PriceID').AsInteger;
      Result := LMaterial;
    end;
  except
    FreeAndNil(LMaterial);
    Result := nil;
  end;
end;

function TMaterialManager.SaveMaterial(AMaterial: TMaterial): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    if AMaterial.Id = 0 then
      Query.SQL.Text := 'INSERT INTO Material (MaterialName, Quantity, UnitOfMeasure, CurrentStock, MinStockLevel, Description, PriceID) VALUES (:MaterialName, :Quantity, :UnitOfMeasure, :CurrentStock, :MinStockLevel, :Description, :PriceID); SELECT SCOPE_IDENTITY() AS NewID;'
    else
      Query.SQL.Text := 'UPDATE Material SET MaterialName = :MaterialName, Quantity = :Quantity, UnitOfMeasure = :UnitOfMeasure, CurrentStock = :CurrentStock, MinStockLevel = :MinStockLevel, Description = :Description, PriceID = :PriceID WHERE ID = :ID;';

    Query.Parameters.Clear;
    Query.Parameters.ParamByName('MaterialName').Value := AMaterial.MaterialName;
    Query.Parameters.ParamByName('Quantity').Value := AMaterial.Quantity;
    Query.Parameters.ParamByName('UnitOfMeasure').Value := AMaterial.UnitOfMeasure;
    Query.Parameters.ParamByName('CurrentStock').Value := AMaterial.CurrentStock;
    Query.Parameters.ParamByName('MinStockLevel').Value := AMaterial.MinStockLevel;
    Query.Parameters.ParamByName('Description').Value := AMaterial.Description;
    Query.Parameters.ParamByName('PriceID').Value := AMaterial.PriceId;

    if AMaterial.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := AMaterial.Id;

    Query.ExecSQL;

    if (AMaterial.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open;
      if not Query.EOF then AMaterial.Id := Query.FieldByName('NewID').AsInteger;
      Query.Close;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TMaterialManager.DeleteMaterial(AMaterialId: Integer): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'DELETE FROM Material WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AMaterialId;
    Query.ExecSQL;
    Result := True;
  except
    Result := False;
  end;
end;

end.