object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Material Database (Master-Detail)'
  ClientHeight = 700
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  
  { Master Grid - Materials }
  object dbgMaterials: TDBGrid
    Left = 0
    Top = 0
    Width = 900
    Height = 150
    Align = alTop
    DataSource = dsMaterials
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnCellClick = dbgMaterialsCellClick
  end
  
  { Material Controls Panel }
  object Panel1: TPanel
    Left = 0
    Top = 150
    Width = 900
    Height = 40
    Align = alTop
    TabOrder = 1
    object btnAddMaterial: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Add Material'
      TabOrder = 0
      OnClick = btnAddMaterialClick
    end
    object btnEditMaterial: TButton
      Left = 97
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Edit Material'
      TabOrder = 1
      OnClick = btnEditMaterialClick
    end
    object btnDeleteMaterial: TButton
      Left = 178
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Delete Material'
      TabOrder = 2
      OnClick = btnDeleteMaterialClick
    end
    object btnSaveMaterial: TButton
      Left = 340
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 3
      OnClick = btnSaveMaterialClick
    end
    object btnCancelMaterial: TButton
      Left = 421
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 4
      OnClick = btnCancelMaterialClick
    end
    object btnRefresh: TButton
      Left = 800
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 5
      OnClick = btnRefreshClick
    end
  end
  
  { Detail Grid - Products }
  object dbgProducts: TDBGrid
    Left = 0
    Top = 190
    Width = 900
    Height = 120
    Align = alTop
    DataSource = dsProducts
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnCellClick = dbgProductsCellClick
  end
  
  { Product Controls Panel }
  object Panel2: TPanel
    Left = 0
    Top = 310
    Width = 900
    Height = 40
    Align = alTop
    TabOrder = 3
    object btnAddProduct: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Add Product'
      TabOrder = 0
      OnClick = btnAddProductClick
    end
    object btnEditProduct: TButton
      Left = 97
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Edit Product'
      TabOrder = 1
      OnClick = btnEditProductClick
    end
    object btnDeleteProduct: TButton
      Left = 178
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Delete Product'
      TabOrder = 2
      OnClick = btnDeleteProductClick
    end
    object btnSaveProduct: TButton
      Left = 340
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 3
      OnClick = btnSaveProductClick
    end
    object btnCancelProduct: TButton
      Left = 421
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 4
      OnClick = btnCancelProductClick
    end
  end
  
  { Detail Grid - Services }
  object dbgServices: TDBGrid
    Left = 0
    Top = 350
    Width = 900
    Height = 120
    Align = alTop
    DataSource = dsServices
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnCellClick = dbgServicesCellClick
  end
  
  { Service Controls Panel }
  object Panel3: TPanel
    Left = 0
    Top = 470
    Width = 900
    Height = 40
    Align = alTop
    TabOrder = 5
    object btnAddService: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Add Service'
      TabOrder = 0
      OnClick = btnAddServiceClick
    end
    object btnEditService: TButton
      Left = 97
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Edit Service'
      TabOrder = 1
      OnClick = btnEditServiceClick
    end
    object btnDeleteService: TButton
      Left = 178
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Delete Service'
      TabOrder = 2
      OnClick = btnDeleteServiceClick
    end
    object btnSaveService: TButton
      Left = 340
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 3
      OnClick = btnSaveServiceClick
    end
    object btnCancelService: TButton
      Left = 421
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 4
      OnClick = btnCancelServiceClick
    end
  end
  
  { Detail Grid - Parts }
  object dbgParts: TDBGrid
    Left = 0
    Top = 510
    Width = 900
    Height = 120
    Align = alTop
    DataSource = dsParts
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 6
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnCellClick = dbgPartsCellClick
  end
  
  { Part Controls Panel }
  object Panel4: TPanel
    Left = 0
    Top = 630
    Width = 900
    Height = 40
    Align = alTop
    TabOrder = 7
    object btnAddPart: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Add Part'
      TabOrder = 0
      OnClick = btnAddPartClick
    end
    object btnEditPart: TButton
      Left = 97
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Edit Part'
      TabOrder = 1
      OnClick = btnEditPartClick
    end
    object btnDeletePart: TButton
      Left = 178
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Delete Part'
      TabOrder = 2
      OnClick = btnDeletePartClick
    end
    object btnSavePart: TButton
      Left = 340
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 3
      OnClick = btnSavePartClick
    end
    object btnCancelPart: TButton
      Left = 421
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 4
      OnClick = btnCancelPartClick
    end
  end
  
  { Database Components }
  object ADOConnection1: TADOConnection
    Left = 432
    Top = 104
  end
  
  object qryMaterials: TADOQuery
    Connection = ADOConnection1
    Parameters = <>
    Left = 432
    Top = 160
  end
  
  object dsMaterials: TDataSource
    DataSet = qryMaterials
    Left = 432
    Top = 216
  end
  
  object qryProducts: TADOQuery
    Connection = ADOConnection1
    Parameters = <
      item
        Name = 'MaterialID'
        Attributes = [paSigned]
        DataType = ftInteger
        Precision = 10
        Size = 4
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * FROM product WHERE FK_Material_ID = :MaterialID')
    Left = 528
    Top = 160
  end
  
  object dsProducts: TDataSource
    DataSet = qryProducts
    Left = 528
    Top = 216
  end
  
  object qryServices: TADOQuery
    Connection = ADOConnection1
    Parameters = <
      item
        Name = 'MaterialID'
        Attributes = [paSigned]
        DataType = ftInteger
        Precision = 10
        Size = 4
        Value = Null
      end>
    SQL.Strings = (
      'SELECT s.* FROM service s '
      'INNER JOIN product p ON s.FK_Product_ID = p.ID '
      'WHERE p.FK_Material_ID = :MaterialID')
    Left = 624
    Top = 160
  end
  
  object dsServices: TDataSource
    DataSet = qryServices
    Left = 624
    Top = 216
  end
  
  object qryParts: TADOQuery
    Connection = ADOConnection1
    Parameters = <
      item
        Name = 'MaterialID'
        Attributes = [paSigned]
        DataType = ftInteger
        Precision = 10
        Size = 4
        Value = Null
      end>
    SQL.Strings = (
      'SELECT pt.* FROM part pt '
      'INNER JOIN product p ON pt.FK_ProductID = p.ID '
      'WHERE p.FK_Material_ID = :MaterialID')
    Left = 720
    Top = 160
  end
  
  object dsParts: TDataSource
    DataSet = qryParts
    Left = 720
    Top = 216
  end
end
