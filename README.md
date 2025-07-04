# microaccount
m-a.bg microaccount



Delphi 7 не позволява разделянето на папки, но аз ги отделих, за да е по-разбираемо. 
Добавих две различни главни форми, които реално правят едно и също нещо.


Tables:

Price 
 ID (INT, Primary Key)
 Price (DECIMAL(10, 2), NOT NULL)
 Currency (VARCHAR(10), DEFAULT 'BGN' NOT NULL)
 
 Unit (UnitOfMeasure)
  ID (INT, Primary Key)
  Name (VARCHAR(100), UNIQUE, NOT NULL)

Material 
 ID (INT, Primary Key)
 PriceID (INT, FK to Price.ID)
 ProductID (INT, FK to Product.ID)

 MaterialName (VARCHAR(100), UNIQUE, NOT NULL)
 Quantity (DECIMAL(10, 2), NOT NULL)
 UnitOfMeasure (VARCHAR(20))
 CurrentStock (DECIMAL(10, 2), DEFAULT 0, NOT NULL)
 MinStockLevel (DECIMAL(10, 2), DEFAULT 0) 
 Description (VARCHAR(255))


Product
 ID (INT, Primary Key)
 PriceID (INT, FK to Price.ID)
 PartID (INT, FK to Part.ID)
 ServiceID (INT, FK to Service.ID)  
 
 ProductName (VARCHAR(100), UNIQUE, NOT NULL)
 ProductCode (VARCHAR(50), UNIQUE)
 UnitOfMeasure (VARCHAR(20))
 CurrentStock (DECIMAL(10, 2), DEFAULT 0, NOT NULL)
 IsFinalProduct (BOOLEAN, DEFAULT FALSE, NOT NULL) -- TRUE for bicycles, FALSE for frames/handlebars
 Description (VARCHAR(255))


Part
 ID (INT, Primary Key)
 PriceID (INT, FK to Price.ID)
 UnitID (INT, FK to Unit.ID)

 PartName (VARCHAR(100), UNIQUE, NOT NULL)
 IsConsumable (BOOLEAN, DEFAULT TRUE, NOT NULL) -- If FALSE, it's a tool/equipment
 Description (VARCHAR(255))


Service
 ID (INT, Primary Key)
 PriceID (INT, FK to Price.ID)
 UnitID (INT, FK to Unit.ID)

 ServiceName (VARCHAR(100), UNIQUE, NOT NULL) -- e.g., 'Cutting Pipe', 'Welding', 'Painting', 'Assembly'
 UnitOfMeasure (VARCHAR(20)) -- e.g., 'per item', 'per hour'
 Description (VARCHAR(255))




Project: BicycleManufacturingSystem.dpr
├── Forms
│   ├── UMainForm.pas 
│   │   └── UMainForm.dfm
│   │
│   
├── DataModules
│   ├── UDM_Main.pas 
│   │   ├── TADOConnection 
│   │   └── TDataSource components 
│   │   └── UDM_Main.dfm
│   │
│   └── UDM_Entities.pas 
│       ├── TADOQuery :
│       │   ├── Materials
│       │   ├── Products
│       │   ├── Parts
│       │   ├── Services
│       │   ├── Units
│       │   └── Prices
│       └── UDM_Entities.dfm
│
├── BusinessLogic
│   ├── BLC_Material.pas
│   │   ├── Methods for CRUD operations on Material
│   │   ├── Methods for stock management (e.g., UpdateStock, CheckMinStock)
│   │   └── Validation rules for Material data
│   │
│   ├── BLC_Product.pas 
│   │   ├── Methods for CRUD operations on Product
│   │   ├── Methods for product costing (calculating based on parts/services prices)
│   │   └── Validation rules for Product data
│   │
│   ├── BLC_Part.pas 
│   │   ├── Methods for CRUD operations on Part
│   │   └── Validation rules for Part data
│   │
│   ├── BLC_Service.pas 
│   │   ├── Methods for CRUD operations on Service
│   │   └── Validation rules for Service data
│   │
│   ├── BLC_Unit.pas 
│   │   ├── Methods for CRUD operations on Unit
│   │   └── Validation rules for Unit data
│   │
│   └── BLC_Price.pas
│       ├── Methods for CRUD operations on Price
│       └── Validation rules for Price data
│
├── Models 
│   ├── M_Material.pas (TMaterial: Class representing a Material object)
│   ├── M_Product.pas (TProduct: Class representing a Product object)
│   ├── M_Part.pas (TPart: Class representing a Part object)
│   ├── M_Service.pas (TService: Class representing a Service object)
│   ├── M_Unit.pas (TUnit: Class representing a Unit object)
│   └── M_Price.pas (TPrice: Class representing a Price object)
│
└── Utils
    ├── UConstants.pas (Constants for database connection strings, paths, etc.)
    ├── UAppMessages.pas (Helper for displaying standardized messages/errors)
    └── UDBUtils.pas (Generic database utility functions, e.g., for transactions)
