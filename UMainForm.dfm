object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Bicycle Manufacturing System'
  ClientHeight = 700
  ClientWidth = 1000
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 105
    Height = 15
    Caption = 'Materials (Stock)'
  end
  object DBGrid_Material: TDBGrid
    Left = 16
    Top = 24
    Width = 473
    Height = 150
    DataSource = DataSource_Material
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    Columns = <
      item
        FieldName = 'ID'
        Visible = False
      end
      item
        FieldName = 'MaterialName'
        Title.Caption = 'Material Name'
        Width = 150
        Visible = True
      end
      item
        FieldName = 'Quantity'
        Title.Caption = 'Quantity'
        Width = 80
        Visible = True
      end
      item
        FieldName = 'CurrentStock'
        Title.Caption = 'Current Stock'
        Width = 80
        Visible = True
      end
      item
        FieldName = 'MinStockLevel'
        Title.Caption = 'Min Stock'
        Width = 80
        Visible = True
      end
      item
        FieldName = 'PriceID'
        Title.Caption = 'Price ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'UnitID'
        Title.Caption = 'Unit ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'ProductID'
        Title.Caption = 'Product ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'Description'
        Visible = True
      end>
  end
  object Label2: TLabel
    Left = 512
    Top = 8
    Width = 48
    Height = 15
    Caption = 'Products'
  end
  object DBGrid_Product: TDBGrid
    Left = 512
    Top = 24
    Width = 473
    Height = 150
    DataSource = DataSource_Product
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    Columns = <
      item
        FieldName = 'ID'
        Visible = False
      end
      item
        FieldName = 'ProductName'
        Title.Caption = 'Product Name'
        Width = 150
        Visible = True
      end
      item
        FieldName = 'ProductCode'
        Title.Caption = 'Product Code'
        Width = 80
        Visible = True
      end
      item
        FieldName = 'CurrentStock'
        Title.Caption = 'Current Stock'
        Width = 80
        Visible = True
      end
      item
        FieldName = 'IsFinalProduct'
        Title.Caption = 'Is Final'
        Width = 50
        Visible = True
      end
      item
        FieldName = 'PriceID'
        Title.Caption = 'Price ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'UnitID'
        Title.Caption = 'Unit ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'PartID'
        Title.Caption = 'Part ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'ServiceID'
        Title.Caption = 'Service ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'Description'
        Visible = True
      end>
  end
  object Label3: TLabel
    Left = 16
    Top = 192
    Width = 27
    Height = 15
    Caption = 'Parts'
  end
  object DBGrid_Part: TDBGrid
    Left = 16
    Top = 208
    Width = 473
    Height = 150
    DataSource = DataSource_Part
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    Columns = <
      item
        FieldName = 'ID'
        Visible = False
      end
      item
        FieldName = 'PartName'
        Title.Caption = 'Part Name'
        Width = 150
        Visible = True
      end
      item
        FieldName = 'IsConsumable'
        Title.Caption = 'Consumable'
        Width = 70
        Visible = True
      end
      item
        FieldName = 'PriceID'
        Title.Caption = 'Price ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'UnitID'
        Title.Caption = 'Unit ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'Description'
        Visible = True
      end>
  end
  object Label4: TLabel
    Left = 512
    Top = 192
    Width = 44
    Height = 15
    Caption = 'Services'
  end
  object DBGrid_Service: TDBGrid
    Left = 512
    Top = 208
    Width = 473
    Height = 150
    DataSource = DataSource_Service
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    Columns = <
      item
        FieldName = 'ID'
        Visible = False
      end
      item
        FieldName = 'ServiceName'
        Title.Caption = 'Service Name'
        Width = 150
        Visible = True
      end
      item
        FieldName = 'PriceID'
        Title.Caption = 'Price ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'UnitID'
        Title.Caption = 'Unit ID'
        Width = 60
        Visible = True
      end
      item
        FieldName = 'Description'
        Visible = True
      end>
  end
  object ButtonNew: TButton
    Left = 16
    Top = 384
    Width = 75
    Height = 25
    Caption = 'New'
    TabOrder = 4
    OnClick = ButtonNewClick
  end
  object ButtonDelete: TButton
    Left = 96
    Top = 384
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 5
    OnClick = ButtonDeleteClick
  end
  object ButtonSave: TButton
    Left = 176
    Top = 384
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 6
    OnClick = ButtonSaveClick
  end
  object ADOConnection1: TADOConnection
    ConnectionString = 'Provider=SQLOLEDB.1;Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=BicycleProductionDB_TSQL;Data Source=.\SQLEXPRESS'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Left = 96
    Top = 448
  end
  object ADOQuery_Material: TADOQuery
    Connection = ADOConnection1
    CursorType = ctOpenStatic
    LockType = ltOptimistic
    SQL.Strings = (
      'SELECT * FROM Material')
    Left = 248
    Top = 448
  end
  object DataSource_Material: TDataSource
    DataSet = ADOQuery_Material
    Left = 328
    Top = 448
  end
  object ADOQuery_Product: TADOQuery
    Connection = ADOConnection1
    CursorType = ctOpenStatic
    LockType = ltOptimistic
    SQL.Strings = (
      'SELECT * FROM Product')
    Left = 248
    Top = 496
  end
  object DataSource_Product: TDataSource
    DataSet = ADOQuery_Product
    Left = 328
    Top = 496
  end
  object ADOQuery_Part: TADOQuery
    Connection = ADOConnection1
    CursorType = ctOpenStatic
    LockType = ltOptimistic
    SQL.Strings = (
      'SELECT * FROM Part')
    Left = 248
    Top = 544
  end
  object DataSource_Part: TDataSource
    DataSet = ADOQuery_Part
    Left = 328
    Top = 544
  end
  object ADOQuery_Service: TADOQuery
    Connection = ADOConnection1
    CursorType = ctOpenStatic
    LockType = ltOptimistic
    SQL.Strings = (
      'SELECT * FROM Service')
    Left = 248
    Top = 592
  end
  object DataSource_Service: TDataSource
    DataSet = ADOQuery_Service
    Left = 328
    Top = 592
  end
end
