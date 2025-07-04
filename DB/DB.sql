
-- Create Currency table
CREATE TABLE Currency (
    ID INT PRIMARY KEY IDENTITY(1,1),
    Name VARCHAR(100) UNIQUE NOT NULL,
    Description VARCHAR(255)
);

-- Create Unit table
CREATE TABLE Unit (
    ID INT PRIMARY KEY IDENTITY(1,1),
    Name VARCHAR(100) UNIQUE NOT NULL,
    Description VARCHAR(255)
);

-- Create Price table
CREATE TABLE Price (
    ID INT PRIMARY KEY IDENTITY(1,1),
    CurrencyID INT NOT NULL,
    Price DECIMAL(10, 2) NOT NULL,
    Description VARCHAR(255),
    FOREIGN KEY (CurrencyID) REFERENCES Currency(ID)
);

-- Create Material table
CREATE TABLE Material (
    ID INT PRIMARY KEY IDENTITY(1,1),
    PriceID INT NOT NULL,
    MaterialName VARCHAR(100) UNIQUE NOT NULL,
    Quantity DECIMAL(10, 2) NOT NULL,
    UnitOfMeasure VARCHAR(20),
    CurrentStock DECIMAL(10, 2) DEFAULT 0 NOT NULL,
    MinStockLevel DECIMAL(10, 2) DEFAULT 0,
    Description VARCHAR(255),
    FOREIGN KEY (PriceID) REFERENCES Price(ID)
);

-- Create Product table
CREATE TABLE Product (
    ID INT PRIMARY KEY IDENTITY(1,1),
    PriceID INT NOT NULL,
    MaterialID INT NOT NULL,
    ProductName VARCHAR(100) UNIQUE NOT NULL,
    ProductCode VARCHAR(50) UNIQUE,
    UnitOfMeasure VARCHAR(20),
    CurrentStock DECIMAL(10, 2) DEFAULT 0 NOT NULL,
    IsFinalProduct BIT DEFAULT 0 NOT NULL,
    Description VARCHAR(255),
    FOREIGN KEY (PriceID) REFERENCES Price(ID),
    FOREIGN KEY (MaterialID) REFERENCES Material(ID)
);

-- Create Part table
CREATE TABLE Part (
    ID INT PRIMARY KEY IDENTITY(1,1),
    PriceID INT NOT NULL,
    UnitID INT NOT NULL,
    ProductID INT NOT NULL,
    PartName VARCHAR(100) UNIQUE NOT NULL,
    IsConsumable BIT DEFAULT 1 NOT NULL,
    Description VARCHAR(255),
    FOREIGN KEY (PriceID) REFERENCES Price(ID),
    FOREIGN KEY (UnitID) REFERENCES Unit(ID),
    FOREIGN KEY (ProductID) REFERENCES Product(ID)
);

-- Create Service table
CREATE TABLE Service (
    ID INT PRIMARY KEY IDENTITY(1,1),
    PriceID INT NOT NULL,
    UnitID INT NOT NULL,
    ProductID INT NOT NULL,
    ServiceName VARCHAR(100) UNIQUE NOT NULL,
    UnitOfMeasure VARCHAR(20),
    Description VARCHAR(255),
    FOREIGN KEY (PriceID) REFERENCES Price(ID),
    FOREIGN KEY (UnitID) REFERENCES Unit(ID),
    FOREIGN KEY (ProductID) REFERENCES Product(ID)
);




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


