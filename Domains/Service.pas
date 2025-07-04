unit M_Service;

interface

uses
  System.SysUtils, System.Classes;

type
  // TService class represents a record from the 'Service' database table.
  // It holds details about a manufacturing operation, its cost, and unit.
  TService = class
  private
    FId: Integer;
    FServiceName: string;
    FDescription: string;
    FPriceId: Integer; // Foreign Key to Price table
    FUnitId: Integer;   // Foreign Key to Unit table
  public
    // Constructor to initialize a new TService instance.
    constructor Create;
    // Properties to access and modify the service data.
    property Id: Integer read FId write FId;
    property ServiceName: string read FServiceName write FServiceName;
    property Description: string read FDescription write FDescription;
    property PriceId: Integer read FPriceId write FPriceId;
    property UnitId: Integer read FUnitId write FUnitId;
  end;

implementation

{ TService }

constructor TService.Create;
begin
  // Initialize default values for properties.
  FId := 0;
  FServiceName := '';
  FDescription := '';
  FPriceId := 0;
  FUnitId := 0;
end;

end.
