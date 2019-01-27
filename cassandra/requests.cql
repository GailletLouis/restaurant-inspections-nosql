---- requests for restaurants_inspections keyspace and restaurants table
-- how many elements in the table, useful to know how many elements were able to be inserted
SELECT count(*) FROM restaurants;

-- restaurant names for certain borough and cuisine
SELECT name FROM restaurants WHERE cuisine = 'Japanese' AND borough = 'Brooklyn' ALLOW FILTERING;
-- but allowing filtering can be unstable, so we prefer to index the attributes
CREATE INDEX cusineI ON restaurants(cusisine);
CREATE INDEX boroughI ON restaurants(borough);
SELECT name FROM restaurants WHERE cuisine = 'Japenese' AND borough = 'Brooklyn';

-- restaurant name and cuisine for a certain restaurant name
SELECT cuisine, borough FROM restaurants WHERE name = 'Kasumi' ALLOW FILTERING;