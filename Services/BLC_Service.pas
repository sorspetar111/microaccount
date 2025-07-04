unit BLC_Service;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Service, BLC_BaseService;

type
  TServiceManager = class(TBaseService)
  public
    constructor Create(AConnection: TADOConnection);
    function LoadServiceById(AServiceId: Integer): TService;
    function SaveService(AService: TService): Boolean;
    function DeleteService(AServiceId: Integer): Boolean;
  end;

implementation

constructor TServiceManager.Create(AConnection: TADOConnection);
begin
  inherited Create(AConnection);
end;

function TServiceManager.LoadServiceById(AServiceId: Integer): TService;
var
  LService: TService;
begin
  Result := nil;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'SELECT ID, ServiceName, UnitOfMeasure, Description, PriceID, UnitID FROM Service WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AServiceId;
    Query.Open;

    if not Query.EOF then
    begin
      LService := TService.Create;
      LService.Id := Query.FieldByName('ID').AsInteger;
      LService.ServiceName := Query.FieldByName('ServiceName').AsString;
      LService.UnitOfMeasure := Query.FieldByName('UnitOfMeasure').AsString;
      LService.Description := Query.FieldByName('Description').AsString;
      LService.PriceId := Query.FieldByName('PriceID').AsInteger;
      LService.UnitId := Query.FieldByName('UnitID').AsInteger;
      Result := LService;
    end;
  except
    FreeAndNil(LService);
    Result := nil;
  end;
end;

function TServiceManager.SaveService(AService: TService): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    if AService.Id = 0 then
      Query.SQL.Text := 'INSERT INTO Service (ServiceName, UnitOfMeasure, Description, PriceID, UnitID) VALUES (:ServiceName, :UnitOfMeasure, :Description, :PriceID, :UnitID); SELECT SCOPE_IDENTITY() AS NewID;'
    else
      Query.SQL.Text := 'UPDATE Service SET ServiceName = :ServiceName, UnitOfMeasure = :UnitOfMeasure, Description = :Description, PriceID = :PriceID, UnitID = :UnitID WHERE ID = :ID;';

    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ServiceName').Value := AService.ServiceName;
    Query.Parameters.ParamByName('UnitOfMeasure').Value := AService.UnitOfMeasure;
    Query.Parameters.ParamByName('Description').Value := AService.Description;
    Query.Parameters.ParamByName('PriceID').Value := AService.PriceId;
    Query.Parameters.ParamByName('UnitID').Value := AService.UnitId;

    if AService.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := AService.Id;

    Query.ExecSQL;

    if (AService.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open;
      if not Query.EOF then AService.Id := Query.FieldByName('NewID').AsInteger;
      Query.Close;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TServiceManager.DeleteService(AServiceId: Integer): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'DELETE FROM Service WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := AServiceId;
    Query.ExecSQL;
    Result := True;
  except
    Result := False;
  end;
end;

end.