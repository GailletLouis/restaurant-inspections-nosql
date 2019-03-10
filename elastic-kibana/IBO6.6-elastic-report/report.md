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
{"fields" :
  {"address": {"building": "1007",
    "coord":{"type":"Point", "coordinates" : [-73.856077, 40.848447]},
    "street": "Morris Park Ave", "zipcode": "10462"},
    "borough": "Bronx", "cuisine": "Bakery",
    "grades": [{"date": {"$date": 1393804800000},
      "grade": "A", "score": 2}, {"date": {"$date": 1378857600000},
      "grade": "A", "score": 6}, {"date": {"$date": 1358985600000},
      "grade": "A", "score": 10}, {"date": {"$date": 1322006400000},
      "grade": "A", "score": 9}, {"date": {"$date": 1299715200000},
      "grade": "B", "score": 14}], "name": "Morris Park Bake Shop",
    "restaurant_id": "30075445"}}
```

*for example here a first line*

- Once have our fixed JSON file containing all the data in a way that can be given to Elastic Search, we do so with 

```
curl -XPUT localhost:9200/_bulk -H"Content-Type: application/json" \
  --data-binary @fixed-restaurants.json
```

---

# Requests

## Easy requests

---

## Medium requests

---

## Hard requests

**Get all restaurants from a group of cuisines that we define**

- We first close the database, to be able to change settings.

- We then add in the settings some synonyms. We use the synonyms to group different cuisines together. So by giving "Asian" as a synonym of "Japanese" or "Korean", we can just search for "Asian" without having to specify "Japanese and Korean".

- We reopen the database.

- Then we have 2 examples of the actual request, which just searches for the group of cuisines we defined.

```sh
POST restaurants/_close

PUT /restaurants/_settings
{
  "settings": {
    "analysis": {
      "filter": {
        "my_synonym_filter": {
          "type": "synonym", 
          "synonyms": [ 
            "ave,avenue,av","street,st","road,rd","boulevard,blvd,bvd",
            "Asian,Korean","Asian,Japanese","Asian,Chinese","Asian,Thai",
            "Asian,Indian" ,"European,Mediterranean","European,Italian",
            "European,Irish","European,French","European,English",
            "European,Pizza","European,Spanish","European,Russian",
            "European,Greek"
          ]
        }
      },
      "analyzer": {
        "my_synonyms": {
          "tokenizer": "standard",
          "filter": [
            "my_synonym_filter" 
          ]
        }
      }
    }
  }
}

POST restaurants/_open

POST restaurants/_search
{
  "query": {
    "match" : {
      "fields.cuisine.keyword": {
        "query" : "Asian",
        "analyzer": "my_synonyms"
      }
    }
  }
}

POST restaurants/_search
{
  "query": {
    "match": {
      "fields.cuisine.keyword": {
        "query": "European",
        "analyzer": "my_synonyms"
      }
    }
  }
}
```
