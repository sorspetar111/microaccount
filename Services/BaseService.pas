unit BLC_BaseService;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB;

type
  TBaseService = class(TObject)
  private
    FQuery: TADOQuery;
  protected
    property Query: TADOQuery read FQuery;
  public
    constructor Create(AConnection: TADOConnection);
    destructor Destroy; override;
    procedure SetConnection(AConnection: TADOConnection);
  end;

implementation

constructor TBaseService.Create(AConnection: TADOConnection);
begin
  inherited Create;
  FQuery := TADOQuery.Create(nil);
  FQuery.Connection := AConnection;
end;

destructor TBaseService.Destroy;
begin
  FreeAndNil(FQuery);
  inherited Destroy;
end;

procedure TBaseService.SetConnection(AConnection: TADOConnection);
begin
  FQuery.Connection := AConnection;
end;

end.