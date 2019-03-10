---
title:
- Requests for Elastic Search / Kibana - Advanced Topics in NoSQL
author:
- Amine MILIANI
- Louis GAILLET
- Dimitrije DJOKIC
- Nathan IMMACOLATO
---

---

# Datamodel and Import

- As for the work done on Cassandra, we decided to edit every line of the JSON data to make each its own individual insert.

```json
"index":{"_index": "restaurants","_type":"restaurant","_id":1}}
{"fields" : {"address": {"building": "1007", "coord":{"type":"Point", "coordinates" : [-73.856077, 40.848447]}, "street": "Morris Park Ave", "zipcode": "10462"}, "borough": "Bronx", "cuisine": "Bakery", "grades": [{"date": {"$date": 1393804800000}, "grade": "A", "score": 2}, {"date": {"$date": 1378857600000}, "grade": "A", "score": 6}, {"date": {"$date": 1358985600000}, "grade": "A", "score": 10}, {"date": {"$date": 1322006400000}, "grade": "A", "score": 9}, {"date": {"$date": 1299715200000}, "grade": "B", "score": 14}], "name": "Morris Park Bake Shop", "restaurant_id": "30075445"}}
```

*for example here a first line*

- Once have our fixed JSON file containing all the data in a way that can be given to Elastic Search, we do so with 

```
curl -XPUT localhost:9200/_bulk -H"Content-Type: application/json" --data-binary @fixed-restaurants.json
```

---

# Requests

## Easy requests

---

## Medium requests

---

## Hard requests
