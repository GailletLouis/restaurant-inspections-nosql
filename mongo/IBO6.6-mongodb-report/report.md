---
title:
- Requests for MongoDB - Advanced Topics for NoSQL
author:
- Amine MILIANI
- Louis GAILLET
- Dimitrije DJOKIC
- Nathan IMMACOLATO
---

---

# Datamodel and Import

No unusual difficulty, MongoDB was very easy to work with and import the dataset, and the JSON format of the dataset made all the process a breeze.

---

# Requests

## Easy requests

- **Counting all restaurants**

```javascript
db.restaurants.count();
```

- **Names of the restaurants in the Bronx**

```javascript
db.restaurants.find({"borough":"Bronx"}, {"name":1, "_id":0});
```

- **Names of restaurants where the grade is A**

```javascript
db.restaurants.find({"grades.grade":"A"}, {"name":1, "_id":0});
```

- **Address of a restaurant when we know its name**

```javascript
db.restaurants.find({"name":"Wendy'S"}, {"address":1, "_id":0});
```

- **Restaurants name for a specific cuisine in a certain borough**

```javascript
db.restaurants.find({"borough":"Bronx", "cuisine":"Bakery"}, {"name":1, "_id":0});
```

- **Insert a new restaurant**

```javascript
db.restaurants.insert({"address": {"building": "9999", "coord":{"type":"Point",
    "coordinates" : [-12.345678, 12.345678]}, "street": "My Street",
    "zipcode": "77777"}, "borough": "Bronx", "cuisine": "Bakery",
    "grades": [{"date": {"$date": 1292804800000}, "grade": "A", "score": 2},
    {"date": {"$date": 1378857600000}, "grade": "B", "score": 4},
    {"date": {"$date": 1358985600000}, "grade": "A", "score": 10},
    {"date": {"$date": 1322006400000}, "grade": "A", "score": 9},
    {"date": {"$date": 1299715200000}, "grade": "B", "score": 14}],
    "name": "My New Restaurant", "restaurant_id": "12345678"});
```

- **Update a restaurant name by name**

```javascript
db.restaurants.update({"name":"Shun Lee Palace Restaurant"},
    {$set:{"name" : "The Dragon Restaurant"}});
```

- **Update a restaurant location by name**

```javascript
db.restaurants.update({"name" : "The Dragon Restaurant"},
    {$set:{"coord.coordinates" : [-73.5290789, 41.1594184]}});
```

- **Update the grade of all the restaurants in Manhattan**

```javascript
db.restaurants.update({"borough":"Manhattan"}, {$set:{"grades.grade":"B"}});
```

---

## Medium request

- **Displays the restaurants near the given position, which provide a French cusine, and all its grades are A**

```javascript
db.restaurants.ensureIndex({"address.coord":"2dsphere"}); 

db.restaurants.aggregate([
   {
     $geoNear: {
        near: { type: "Point", coordinates: [ -73.935242 , 40.730610 ] },
        distanceField: "dist.calculated",
        maxDistance: 10000,
        query: {"cuisine": "French","grades.grade": "A" },
        includeLocs: "dist.location",
        num: 5,
        spherical: true
     }
   }
]);
```

---

## Hard request

- **Grades are unwinded and regrouped by restaurant_id in order to count the number of inspections and obtain an average of the grade scores, and finally the grades regrouped by restaurants are sorted by number of inspections**

```javascript
group = {$group:{"_id" : "$restaurant_id", "Number of inspection" : { "$sum" : 1},
    "Score Average": {$avg:"$grades.score"}, "Name" : {"$first": "$name"}}}
        sort = {$sort : { "Number of inspection" : -1}}
        unwind = {$unwind:"$grades"}
        db.restaurants.aggregate([unwind,group,sort]);
```

---
