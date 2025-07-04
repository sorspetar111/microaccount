unit M_Currency; 

interface

uses
  System.SysUtils, System.Classes;

type
  TCurrency = class
  private
    FId: Integer;
    FName: string;
    FDescription: string;  
  public
    constructor Create;
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;  
  end;

implementation

constructor TCurrency.Create;
begin
  FId := 0;
  FName := '';
  FDescription := '';
end;

end.