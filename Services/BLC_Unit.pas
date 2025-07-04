unit BLC_Unit;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Unit, BLC_BaseService;

type
  TUnitManager = class(TBaseService)
  public
    constructor Create(AConnection: TADOConnection);
    function LoadUnitById(AUnitId: Integer): TUnit;
    function SaveUnit(AUnit: TUnit): Boolean;
    function DeleteUnit(AUnitId: Integer): Boolean;
  end;

implementation

constructor TUnitManager.Create(AConnection: TADOConnection);
begin
  inherited Create(AConnection);
end;

function TUnitManager.LoadUnitById(AUnitId: Integer): TUnit;
var
  LUnit: TUnit;
begin
  Result := nil;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'SELECT ID, Name, Description FROM Unit WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AUnitId;
    Query.Open;

    if not Query.EOF then
    begin
      LUnit := TUnit.Create;
      LUnit.Id := Query.FieldByName('ID').AsInteger;
      LUnit.Name := Query.FieldByName('Name').AsString;
      LUnit.Description := Query.FieldByName('Description').AsString;
      Result := LUnit;
    end;
  except
    FreeAndNil(LUnit);
    Result := nil;
  end;
end;

function TUnitManager.SaveUnit(AUnit: TUnit): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    if AUnit.Id = 0 then
      Query.SQL.Text := 'INSERT INTO Unit (Name, Description) VALUES (:Name, :Description); SELECT SCOPE_IDENTITY() AS NewID;'
    else
      Query.SQL.Text := 'UPDATE Unit SET Name = :Name, Description = :Description WHERE ID = :ID;';

    Query.Parameters.Clear;
    Query.Parameters.ParamByName('Name').Value := AUnit.Name;
    Query.Parameters.ParamByName('Description').Value := AUnit.Description;

    if AUnit.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := AUnit.Id;

    Query.ExecSQL;

    if (AUnit.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open;
      if not Query.EOF then AUnit.Id := Query.FieldByName('NewID').AsInteger;
      Query.Close;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TUnitManager.DeleteUnit(AUnitId: Integer): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'DELETE FROM Unit WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AUnitId;
    Query.ExecSQL;
    Result := True;
  except
    Result := False;
  end;
end;

end.