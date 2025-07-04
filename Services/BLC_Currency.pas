unit BLC_Currency;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Currency, BLC_BaseService;

type
  TCurrencyManager = class(TBaseService)
  public
    constructor Create(AConnection: TADOConnection);
    function LoadCurrencyById(ACurrencyId: Integer): TCurrency;
    function SaveCurrency(ACurrency: TCurrency): Boolean;
    function DeleteCurrency(ACurrencyId: Integer): Boolean;
  end;

implementation

constructor TCurrencyManager.Create(AConnection: TADOConnection);
begin
  inherited Create(AConnection);
end;

function TCurrencyManager.LoadCurrencyById(ACurrencyId: Integer): TCurrency;
var
  LCurrency: TCurrency;
begin
  Result := nil;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'SELECT ID, Name, Description FROM Currency WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := ACurrencyId;
    Query.Open;

    if not Query.EOF then
    begin
      LCurrency := TCurrency.Create;
      LCurrency.Id := Query.FieldByName('ID').AsInteger;
      LCurrency.Name := Query.FieldByName('Name').AsString;
      LCurrency.Description := Query.FieldByName('Description').AsString;
      Result := LCurrency;
    end;
  except
    FreeAndNil(LCurrency);
    Result := nil;
  end;
end;

function TCurrencyManager.SaveCurrency(ACurrency: TCurrency): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    if ACurrency.Id = 0 then
      Query.SQL.Text := 'INSERT INTO Currency (Name, Description) VALUES (:Name, :Description); SELECT SCOPE_IDENTITY() AS NewID;'
    else
      Query.SQL.Text := 'UPDATE Currency SET Name = :Name, Description = :Description WHERE ID = :ID;';

    Query.Parameters.Clear;
    Query.Parameters.ParamByName('Name').Value := ACurrency.Name;
    Query.Parameters.ParamByName('Description').Value := ACurrency.Description;

    if ACurrency.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := ACurrency.Id;

    Query.ExecSQL;

    if (ACurrency.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open;
      if not Query.EOF then ACurrency.Id := Query.FieldByName('NewID').AsInteger;
      Query.Close;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TCurrencyManager.DeleteCurrency(ACurrencyId: Integer): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'DELETE FROM Currency WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := ACurrencyId;
    Query.ExecSQL;
    Result := True;
  except
    Result := False;
  end;
end;

end.