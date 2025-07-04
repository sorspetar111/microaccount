unit M_Price;

interface

uses
  System.SysUtils, System.Classes;

type
  TPrice = class
  private
    FId: Integer;
    FPrice: Currency;
    FCurrencyId: Integer; 
    FDescription: string;
  public
    constructor Create;
    property Id: Integer read FId write FId;
    property Price: Currency read FPrice write FPrice;
    property CurrencyId: Integer read FCurrencyId write FCurrencyId;  
    property Description: string read FDescription write FDescription;
  end;

implementation

constructor TPrice.Create;
begin
  FId := 0;
  FPrice := 0.0;
  FCurrencyId := 0;  
  FDescription := '';
end;

end.