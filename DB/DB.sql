
SQL:
-- T-SQL Script for Bicycle Manufacturing Database Schema
-- Database: BicycleProductionDB_TSQL
-- Purpose: Creates all specified tables, primary keys, foreign keys, and unique constraints.

-- Use a specific database or create a new one
-- IF NOT EXISTS (SELECT * FROM sys.databases WHERE name = 'BicycleProductionDB_TSQL')
-- BEGIN
--     CREATE DATABASE BicycleProductionDB_TSQL;
-- END;
-- GO

-- USE BicycleProductionDB_TSQL;
-- GO

-- -------------------------------------------------------------------
-- Table: Price
-- Description: Centralized table for all prices (materials, parts, services, products).
-- -------------------------------------------------------------------
CREATE TABLE Price (
    ID          INT IDENTITY(1,1) PRIMARY KEY,
    Price       DECIMAL(10, 2) NOT NULL,
    Currency    NVARCHAR(10) NOT NULL DEFAULT 'BGN',
    Description NVARCHAR(255)
);
GO

-- -------------------------------------------------------------------
-- Table: Unit
-- Description: Defines various units of measure (e.g., 'kg', 'meter', 'piece', 'hour').
-- -------------------------------------------------------------------
CREATE TABLE Unit (
    ID          INT IDENTITY(1,1) PRIMARY KEY,
    Name        NVARCHAR(100) UNIQUE NOT NULL,
    Description NVARCHAR(255)
);
GO

-- -------------------------------------------------------------------
-- Table: Part
-- Description: Represents distinct components or sub-assemblies.
-- These can be raw materials, processed materials, or purchased components.
-- -------------------------------------------------------------------
CREATE TABLE Part (
    ID            INT IDENTITY(1,1) PRIMARY KEY,
    PartName      NVARCHAR(100) UNIQUE NOT NULL,
    IsConsumable  BIT NOT NULL DEFAULT 1, -- 1 (TRUE) if consumed in production, 0 (FALSE) if a tool/equipment
    Description   NVARCHAR(255),
    PriceID       INT NOT NULL, -- FK to Price.ID for the cost of this part
    UnitID        INT NOT NULL, -- FK to Unit.ID for the unit of measure for this part
    CONSTRAINT FK_Part_PriceID FOREIGN KEY (PriceID) REFERENCES Price(ID) ON DELETE NO ACTION ON UPDATE CASCADE,
    CONSTRAINT FK_Part_UnitID FOREIGN KEY (UnitID) REFERENCES Unit(ID) ON DELETE NO ACTION ON UPDATE CASCADE
);
GO

-- -------------------------------------------------------------------
-- Table: Service
-- Description: Defines various manufacturing operations or services performed.
-- -------------------------------------------------------------------
CREATE TABLE Service (
    ID            INT IDENTITY(1,1) PRIMARY KEY,
    ServiceName   NVARCHAR(100) UNIQUE NOT NULL, -- e.g., 'Cutting Pipe', 'Welding', 'Painting', 'Assembly'
    Description   NVARCHAR(255),
    PriceID       INT NOT NULL, -- FK to Price.ID for the cost of this service
    UnitID        INT NOT NULL, -- FK to Unit.ID for the unit of measure for this service (e.g., 'per item', 'per hour')
    CONSTRAINT FK_Service_PriceID FOREIGN KEY (PriceID) REFERENCES Price(ID) ON DELETE NO ACTION ON UPDATE CASCADE,
    CONSTRAINT FK_Service_UnitID FOREIGN KEY (UnitID) REFERENCES Unit(ID) ON DELETE NO ACTION ON UPDATE CASCADE
);
GO

-- -------------------------------------------------------------------
-- Table: Material
-- Description: Stores details about raw materials that are stocked and consumed.
-- Note: The relationship to ProductID here is unusual for a typical BOM structure.
-- In a standard design, materials are linked to products via a Bill of Materials table.
-- This FK implies a material might be primarily associated with one product, or it's a simplified model.
-- -------------------------------------------------------------------
CREATE TABLE Material (
    ID              INT IDENTITY(1,1) PRIMARY KEY,
    MaterialName    NVARCHAR(100) UNIQUE NOT NULL,
    Quantity        DECIMAL(10, 2) NOT NULL, -- This column's purpose is ambiguous (e.g., default order quantity? batch size?)
    CurrentStock    DECIMAL(10, 2) NOT NULL DEFAULT 0,
    MinStockLevel   DECIMAL(10, 2) DEFAULT 0,
    Description     NVARCHAR(255),
    PriceID         INT NOT NULL, -- FK to Price.ID for the cost of this material
    UnitID          INT NOT NULL, -- FK to Unit.ID for the unit of measure for this material
    ProductID       INT NULL,     -- FK to Product.ID (as requested, but note: unusual for direct material-product link)
    CONSTRAINT FK_Material_PriceID FOREIGN KEY (PriceID) REFERENCES Price(ID) ON DELETE NO ACTION ON UPDATE CASCADE,
    CONSTRAINT FK_Material_UnitID FOREIGN KEY (UnitID) REFERENCES Unit(ID) ON DELETE NO ACTION ON UPDATE CASCADE
    -- CONSTRAINT FK_Material_ProductID FOREIGN KEY (ProductID) REFERENCES Product(ID) ON DELETE NO ACTION ON UPDATE CASCADE
    -- The FK_Material_ProductID needs the Product table to exist first. It will be added after Product table.
);
GO

-- -------------------------------------------------------------------
-- Table: Product
-- Description: Lists all defined products, including final products (e.g., "Bicycle Model A")
-- and potentially major intermediate assemblies (e.g., "Painted Frame").
-- Note: The direct FKs to PartID and ServiceID are unusual for a typical BOM structure.
-- In a standard design, products are composed of multiple parts and involve multiple services
-- defined in a Bill of Materials or routing table. These FKs imply a single primary part/service.
-- -------------------------------------------------------------------
CREATE TABLE Product (
    ID              INT IDENTITY(1,1) PRIMARY KEY,
    ProductName     NVARCHAR(100) UNIQUE NOT NULL,
    ProductCode     NVARCHAR(50) UNIQUE,
    CurrentStock    DECIMAL(10, 2) NOT NULL DEFAULT 0,
    IsFinalProduct  BIT NOT NULL DEFAULT 0, -- 1 (TRUE) for finished goods, 0 (FALSE) for intermediate products
    Description     NVARCHAR(255),
    PriceID         INT NOT NULL, -- FK to Price.ID for the cost/price of this product
    UnitID          INT NOT NULL, -- FK to Unit.ID for the unit of measure for this product
    PartID          INT NULL,     -- FK to Part.ID (as requested, but note: unusual for direct product-part link)
    ServiceID       INT NULL,     -- FK to Service.ID (as requested, but note: unusual for direct product-service link)
    CONSTRAINT FK_Product_PriceID FOREIGN KEY (PriceID) REFERENCES Price(ID) ON DELETE NO ACTION ON UPDATE CASCADE,
    CONSTRAINT FK_Product_UnitID FOREIGN KEY (UnitID) REFERENCES Unit(ID) ON DELETE NO ACTION ON UPDATE CASCADE,
    CONSTRAINT FK_Product_PartID FOREIGN KEY (PartID) REFERENCES Part(ID) ON DELETE NO ACTION ON UPDATE CASCADE,
    CONSTRAINT FK_Product_ServiceID FOREIGN KEY (ServiceID) REFERENCES Service(ID) ON DELETE NO ACTION ON UPDATE CASCADE
);
GO

-- Add the FK from Material to Product now that Product table exists
ALTER TABLE Material
ADD CONSTRAINT FK_Material_ProductID FOREIGN KEY (ProductID) REFERENCES Product(ID) ON DELETE NO ACTION ON UPDATE CASCADE;
GO



 INSERT INTO Price (Price, Currency, Description) VALUES
 (1.50, 'BGN', 'Cost per meter of metal pipe'),
 (5.00, 'BGN', 'Cost per kg of paint'),
 (10.00, 'BGN', 'Cost per piece of tire'),
 (20.00, 'BGN', 'Cost for cutting service per item'),
 (30.00, 'BGN', 'Cost for welding service per item'),
 (50.00, 'BGN', 'Cost for painting service per item'),
 (150.00, 'BGN', 'Cost for assembly service per item'),
 (250.00, 'BGN', 'Estimated cost of Raw Frame'),
 (350.00, 'BGN', 'Estimated cost of Painted Frame'),
 (700.00, 'BGN', 'Sales price of Bicycle Model A');

 INSERT INTO Unit (Name) VALUES
 ('meter'), ('kg'), ('piece'), ('hour'), ('unit');

 INSERT INTO Part (PartName, IsConsumable, PriceID, UnitID, Description) VALUES
 ('Metal Pipe', 1, 1, (SELECT ID FROM Unit WHERE Name = 'meter'), 'Standard metal pipe for frames and handlebars'),
 ('Tire 26 inch', 1, 3, (SELECT ID FROM Unit WHERE Name = 'piece'), 'Bicycle tire'),
 ('Pedal Set', 1, 3, (SELECT ID FROM Unit WHERE Name = 'piece'), 'Set of bicycle pedals'),
 ('Chain', 1, 3, (SELECT ID FROM Unit WHERE Name = 'piece'), 'Bicycle chain'),
 ('Seat', 1, 3, (SELECT ID FROM Unit WHERE Name = 'piece'), 'Bicycle seat');

 INSERT INTO Service (ServiceName, PriceID, UnitID, Description) VALUES
 ('Cutting Pipe', 4, (SELECT ID FROM Unit WHERE Name = 'unit'), 'Service for cutting metal pipes'),
 ('Welding Frame', 5, (SELECT ID FROM Unit WHERE Name = 'unit'), 'Service for welding bicycle frames'),
 ('Painting', 6, (SELECT ID FROM Unit WHERE Name = 'unit'), 'Service for painting components'),
 ('Assembly', 7, (SELECT ID FROM Unit WHERE Name = 'unit'), 'Service for assembling bicycles');

 INSERT INTO Material (MaterialName, Quantity, CurrentStock, MinStockLevel, PriceID, UnitID, ProductID, Description) VALUES
 ('Raw Metal Pipe Stock', 100.00, 1000.00, 100.00, 1, (SELECT ID FROM Unit WHERE Name = 'meter'), NULL, 'Raw stock of metal pipes');

 INSERT INTO Product (ProductName, ProductCode, CurrentStock, IsFinalProduct, PriceID, UnitID, PartID, ServiceID, Description) VALUES
 ('Raw Frame', 'RF001', 0.00, 0, 8, (SELECT ID FROM Unit WHERE Name = 'piece'), NULL, (SELECT ID FROM Service WHERE ServiceName = 'Welding Frame'), 'Unpainted bicycle frame'),
 ('Painted Frame', 'PF001', 0.00, 0, 9, (SELECT ID FROM Unit WHERE Name = 'piece'), NULL, (SELECT ID FROM Service WHERE ServiceName = 'Painting'), 'Painted bicycle frame, ready for assembly'),
 ('Bicycle Model A', 'BMA001', 0.00, 1, 10, (SELECT ID FROM Unit WHERE Name = 'piece'), (SELECT ID FROM Part WHERE PartName = 'Tire 26 inch'), (SELECT ID FROM Service WHERE ServiceName = 'Assembly'), 'Finished bicycle product');


