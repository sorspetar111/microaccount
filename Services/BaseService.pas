unit BLC_BaseService;

interface

uses
  System.SysUtils, System.Classes, Data.Win.ADODB;

type
  // TBaseService is an abstract base class for all business logic managers.
  // It encapsulates a TADOQuery component, providing a common interface
  // for executing SQL commands and managing database interactions.
  TBaseService = class(TObject)
  private
    FQuery: TADOQuery; // The TADOQuery instance used for database operations
  protected
    // Protected property to allow inherited classes to access the TADOQuery.
    property Query: TADOQuery read FQuery;
  public
    // Constructor for the base service.
    // It creates the internal TADOQuery instance.
    constructor Create(AConnection: TADOConnection);
    // Destructor to free the TADOQuery instance when the service is destroyed.
    destructor Destroy; override;

    // Method to assign the ADOConnection to the internal TADOQuery.
    // This is crucial as TADOQuery needs a connection to operate.
    procedure SetConnection(AConnection: TADOConnection);
  end;

implementation

{ TBaseService }

constructor TBaseService.Create(AConnection: TADOConnection);
begin
  inherited Create;
  // Create a new TADOQuery instance.
  // The 'nil' owner means it's not owned by a form or data module visually,
  // so we are responsible for its lifetime (freeing it in Destroy).
  FQuery := TADOQuery.Create(nil);
  // Assign the provided ADOConnection to the query.
  FQuery.Connection := AConnection;
end;

destructor TBaseService.Destroy;
begin
  // Free the TADOQuery instance to prevent memory leaks.
  FreeAndNil(FQuery);
  inherited Destroy;
end;

procedure TBaseService.SetConnection(AConnection: TADOConnection);
begin
  // Update the connection for the internal TADOQuery.
  // This might be useful if the connection object changes or needs to be reassigned.
  FQuery.Connection := AConnection;
end;

end.
