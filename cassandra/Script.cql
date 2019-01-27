CREATE KEYSPACE IF NOT EXISTS restaurants_inspections WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor' : 3};
USE restaurants_inspections;

CREATE TYPE IF NOT EXISTS coord (type VARCHAR, coordinates list<Double>);
CREATE TYPE IF NOT EXISTS address (building VARCHAR, coord frozen <coord>, street VARCHAR, zipcode VARCHAR);
CREATE TYPE IF NOT EXISTS dateType (date1 bigInt);
CREATE TYPE IF NOT EXISTS gradeType(date frozen<dateType>,grade VARCHAR, score INT);
CREATE TABLE IF NOT EXISTS restaurants(address frozen<address>, borough VARCHAR, cuisine VARCHAR, grades list<frozen<gradeType>>,name VARCHAR, restaurant_id VARCHAR, PRIMARY KEY (restaurant_id));
