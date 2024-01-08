select Product.Name As Product, Code, MeasurementUnit, Price, Cost, ProductGroup.Name AS ProductGroup from Product
LEFT join ProductGroup
on ProductGroupId = ProductGroup.Id
