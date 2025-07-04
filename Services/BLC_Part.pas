unit BLC_Part;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB,
  M_Part, BLC_BaseService;

type
  TPartManager = class(TBaseService)
  public
    constructor Create(AConnection: TADOConnection);
    function LoadPartById(APartId: Integer): TPart;
    function SavePart(APart: TPart): Boolean;
    function DeletePart(APartId: Integer): Boolean;
  end;

implementation

constructor TPartManager.Create(AConnection: TADOConnection);
begin
  inherited Create(AConnection);
end;

function TPartManager.LoadPartById(APartId: Integer): TPart;
var
  LPart: TPart;
begin
  Result := nil;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'SELECT ID, PartName, IsConsumable, Description, PriceID, UnitID FROM Part WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := APartId;
    Query.Open;

    if not Query.EOF then
    begin
      LPart := TPart.Create;
      LPart.Id := Query.FieldByName('ID').AsInteger;
      LPart.PartName := Query.FieldByName('PartName').AsString;
      LPart.IsConsumable := Query.FieldByName('IsConsumable').AsBoolean;
      LPart.Description := Query.FieldByName('Description').AsString;
      LPart.PriceId := Query.FieldByName('PriceID').AsInteger;
      LPart.UnitId := Query.FieldByName('UnitID').AsInteger;
      Result := LPart;
    end;
  except
    FreeAndNil(LPart);
    Result := nil;
  end;
end;

function TPartManager.SavePart(APart: TPart): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    if APart.Id = 0 then
      Query.SQL.Text := 'INSERT INTO Part (PartName, IsConsumable, Description, PriceID, UnitID) VALUES (:PartName, :IsConsumable, :Description, :PriceID, :UnitID); SELECT SCOPE_IDENTITY() AS NewID;'
    else
      Query.SQL.Text := 'UPDATE Part SET PartName = :PartName, IsConsumable = :IsConsumable, Description = :Description, PriceID = :PriceID, UnitID = :UnitID WHERE ID = :ID;';

    Query.Parameters.Clear;
    Query.Parameters.ParamByName('PartName').Value := APart.PartName;
    Query.Parameters.ParamByName('IsConsumable').Value := APart.IsConsumable;
    Query.Parameters.ParamByName('Description').Value := APart.Description;
    Query.Parameters.ParamByName('PriceID').Value := APart.PriceId;
    Query.Parameters.ParamByName('UnitID').Value := APart.UnitId;

    if APart.Id <> 0 then
      Query.Parameters.ParamByName('ID').Value := APart.Id;

    Query.ExecSQL;

    if (APart.Id = 0) and (Query.SQL.Text.Contains('SCOPE_IDENTITY()')) then
    begin
      Query.Open;
      if not Query.EOF then APart.Id := Query.FieldByName('NewID').AsInteger;
      Query.Close;
    end;

    Result := True;
  except
    Result := False;
  end;
end;

function TPartManager.DeletePart(APartId: Integer): Boolean;
begin
  Result := False;
  if Query.Active then Query.Close;

  try
    Query.SQL.Text := 'DELETE FROM Part WHERE ID = :ID';
    Query.Parameters.Clear;
    Query.Parameters.ParamByName('ID').Value := APartId;
    Query.ExecSQL;
    Result := True;
  except
    Result := False;
  end;
end;

end.