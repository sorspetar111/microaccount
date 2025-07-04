unit BLC_Price;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Price, BLC_BaseService;

type
  TPriceManager = class(TBaseService)
  public
    constructor Create(AConnection: TADOConnection);
    function LoadPriceById(APriceId: Integer): TPrice;
    function SavePrice(APrice: TPrice): Boolean;
    function DeletePrice(APriceId: Integer): Boolean;
  end;

implementation

constructor TPriceManager.Create(AConnection: TADOConnection);
begin
  inherited Create(AConnection);
end;

function TPriceManager.LoadPriceById(APriceId: Integer): TPrice;
var
  LPrice: TPrice;
begin
  Result := nil;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'SELECT ID, Price, CurrencyID, Description FROM Price WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := APriceId;
    Query.Open;

    if not Query.EOF then
    begin
      LPrice := TPrice.Create;
      LPrice.Id := Query.FieldByName('ID').AsInteger;
      LPrice.Price := Query.FieldByName('Price').AsCurrency;
      LPrice.CurrencyId := Query.FieldByName('CurrencyID').AsInteger;
      LPrice.Description := Query.FieldByName('Description').AsString;
      Result := LPrice;
    end;
  except
    FreeAndNil(LPrice);
    Result := nil;
  end;
end;

function TPriceManager.SavePrice(APrice: TPrice): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    if APrice.Id = 0 then
      Query.SQL.Text := 'INSERT INTO Price (Price, CurrencyID, Description) VALUES (:Price, :CurrencyID, :Description); SELECT SCOPE_IDENTITY() AS NewID;'
    else
      Query.SQL.Text := 'UPDATE Price SET Price = :Price, CurrencyID = :CurrencyID, Description = :Description WHERE ID = :ID;';

    Query.Parameters.Clear;
    Query.Parameters.ParamByName('Price').Value := APrice.Price;
    Query.Parameters.ParamByName('CurrencyID').Value := APrice.CurrencyId;
    Query.Parameters.ParamByName('Description').Value := APrice.Description;

    if APrice.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := APrice.Id;

    Query.ExecSQL;

    if (APrice.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open;
      if not Query.EOF then APrice.Id := Query.FieldByName('NewID').AsInteger;
      Query.Close;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TPriceManager.DeletePrice(APriceId: Integer): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'DELETE FROM Price WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := APriceId;
    Query.ExecSQL;
    Result := True;
  except
    Result := False;
  end;
end;

end.