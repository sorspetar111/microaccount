unit BLC_Product;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Product, BLC_BaseService, BLC_Price, BLC_Material;

type
  TProductManager = class(TBaseService)
  private
    FPriceManager: TPriceManager;
    FMaterialManager: TMaterialManager;
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    function LoadProductById(AProductId: Integer): TProduct;
    function SaveProduct(AProduct: TProduct): Boolean;
    function DeleteProduct(AProductId: Integer): Boolean;
  end;

implementation

constructor TProductManager.Create(AConnection: TADOConnection);
begin
  inherited Create(AConnection);
  FPriceManager := TPriceManager.Create(AConnection);
  FMaterialManager := TMaterialManager.Create(AConnection);
end;

destructor TProductManager.Destroy;
begin
  FreeAndNil(FMaterialManager);
  FreeAndNil(FPriceManager);
  inherited Destroy;
end;

function TProductManager.LoadProductById(AProductId: Integer): TProduct;
var
  LProduct: TProduct;
begin
  Result := nil;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'SELECT ID, ProductName, ProductCode, UnitOfMeasure, CurrentStock, IsFinalProduct, Description, PriceID, MaterialID FROM Product WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AProductId;
    Query.Open;

    if not Query.EOF then
    begin
      LProduct := TProduct.Create;
      LProduct.Id := Query.FieldByName('ID').AsInteger;
      LProduct.ProductName := Query.FieldByName('ProductName').AsString;
      LProduct.ProductCode := Query.FieldByName('ProductCode').AsString;
      LProduct.UnitOfMeasure := Query.FieldByName('UnitOfMeasure').AsString;
      LProduct.CurrentStock := Query.FieldByName('CurrentStock').AsCurrency;
      LProduct.IsFinalProduct := Query.FieldByName('IsFinalProduct').AsBoolean;
      LProduct.Description := Query.FieldByName('Description').AsString;
      LProduct.PriceId := Query.FieldByName('PriceID').AsInteger;
      LProduct.MaterialId := Query.FieldByName('MaterialID').AsInteger;
      Result := LProduct;
    end;
  except
    FreeAndNil(LProduct);
    Result := nil;
  end;
end;

function TProductManager.SaveProduct(AProduct: TProduct): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    if AProduct.Id = 0 then
      Query.SQL.Text := 'INSERT INTO Product (ProductName, ProductCode, UnitOfMeasure, CurrentStock, IsFinalProduct, Description, PriceID, MaterialID) VALUES (:ProductName, :ProductCode, :UnitOfMeasure, :CurrentStock, :IsFinalProduct, :Description, :PriceID, :MaterialID); SELECT SCOPE_IDENTITY() AS NewID;'
    else
      Query.SQL.Text := 'UPDATE Product SET ProductName = :ProductName, ProductCode = :ProductCode, UnitOfMeasure = :UnitOfMeasure, CurrentStock = :CurrentStock, IsFinalProduct = :IsFinalProduct, Description = :Description, PriceID = :PriceID, MaterialID = :MaterialID WHERE ID = :ID;';

    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ProductName').Value := AProduct.ProductName;
    Query.Parameters.ParamByName('ProductCode').Value := AProduct.ProductCode;
    Query.Parameters.ParamByName('UnitOfMeasure').Value := AProduct.UnitOfMeasure;
    Query.Parameters.ParamByName('CurrentStock').Value := AProduct.CurrentStock;
    Query.Parameters.ParamByName('IsFinalProduct').Value := AProduct.IsFinalProduct;
    Query.Parameters.ParamByName('Description').Value := AProduct.Description;
    Query.Parameters.ParamByName('PriceID').Value := AProduct.PriceId;
    Query.Parameters.ParamByName('MaterialID').Value := AProduct.MaterialId;

    if AProduct.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := AProduct.Id;

    Query.ExecSQL;

    if (AProduct.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open;
      if not Query.EOF then AProduct.Id := Query.FieldByName('NewID').AsInteger;
      Query.Close;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TProductManager.DeleteProduct(AProductId: Integer): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'DELETE FROM Product WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AProductId;
    Query.ExecSQL;
    Result := True;
  except
    Result := False;
  end;
end;

end.