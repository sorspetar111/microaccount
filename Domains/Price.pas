unit M_Price;

interface

uses
  System.SysUtils, System.Classes;

type
  // TPrice class represents a record from the 'Price' database table.
  // It holds the ID, price value, currency, and an optional description.
  TPrice = class
  private
    FId: Integer;
    FPrice: Currency; // Use Currency type for monetary values
    FCurrency: string;
    FDescription: string;
  public
    // Constructor to initialize a new TPrice instance.
    constructor Create;
    // Properties to access and modify the price data.
    // Read/write access allows setting and getting values.
    property Id: Integer read FId write FId;
    property Price: Currency read FPrice write FPrice;
    property Currency: string read FCurrency write FCurrency;
    property Description: string read FDescription write FDescription;
  end;

implementation

{ TPrice }

constructor TPrice.Create;
begin
  // Initialize default values for properties.
  // This ensures that new instances start with predictable states.
  FId := 0; // 0 typically indicates a new, unsaved record
  FPrice := 0.0;
  FCurrency := 'BGN';
  FDescription := '';
end;

end.
