program BicycleManufacturingSystem;

uses
  Vcl.Forms,
  UMainForm in 'UMainForm.pas' {MainForm},
  M_Price in 'Models\M_Price.pas',
  M_Unit in 'Models\M_Unit.pas',
  M_Part in 'Models\M_Part.pas',
  M_Service in 'Models\M_Service.pas',
  M_Material in 'Models\M_Material.pas',
  M_Product in 'Models\M_Product.pas',
  BLC_BaseService in 'BusinessLogic\BLC_BaseService.pas',
  BLC_Price in 'BusinessLogic\BLC_Price.pas',
  BLC_Unit in 'BusinessLogic\BLC_Unit.pas',
  BLC_Part in 'BusinessLogic\BLC_Part.pas',
  BLC_Service in 'BusinessLogic\BLC_Service.pas',
  BLC_Material in 'BusinessLogic\BLC_Material.pas',
  BLC_Product in 'BusinessLogic\BLC_Product.pas',
  UAppUtils in 'Utils\UAppUtils.pas';

// {$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
