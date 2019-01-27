CREATE KEYSPACE IF NOT EXISTS restaurants_inspections WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor' : 3};
USE restaurants_inspections;

CREATE TYPE IF NOT EXISTS coord (type VARCHAR, coordinates list<Double>);
CREATE TYPE IF NOT EXISTS address (building VARCHAR, coord frozen <coord>, street VARCHAR, zipcode VARCHAR);
CREATE TYPE IF NOT EXISTS dateType (date1 bigInt);
CREATE TYPE IF NOT EXISTS gradeType(date frozen<dateType>,grade VARCHAR, score INT);
CREATE TABLE IF NOT EXISTS restaurants(address frozen<address>, borough VARCHAR, cuisine VARCHAR, grades list<frozen<gradeType>>,name VARCHAR, restaurant_id VARCHAR, PRIMARY KEY (restaurant_id));
INSERT INTO restaurants JSON '{"address": {"building": "1007", "coord":{"type":"Point", "coordinates" : [-73.856077, 40.848447]}, "street": "Morris Park Ave", "zipcode": "10462"}, "borough": "Bronx", "cuisine": "Bakery", "grades": [{"date": {"date1": 1393804800000}, "grade": "A", "score": 2}, {"date": {"date1": 1378857600000}, "grade": "A", "score": 6}, {"date": {"date1": 1358985600000}, "grade": "A", "score": 10}, {"date": {"date1": 1322006400000}, "grade": "A", "score": 9}, {"date": {"date1": 1299715200000}, "grade": "B", "score": 14}], "name": "Morris Park Bake Shop", "restaurant_id": "30075445"}';
