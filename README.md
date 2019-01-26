# cassandra

Create a volume
`sudo docker volume create nosql`

Inspect to find where the data is
`sudo docker volume inspect nosql`

Put the .cql scripts inside that place

Run the cqlsh with volume mounted
```
sudo docker run \
--name nosql-cqlsh \
-it \
--link nosql-cass:cassandra \
--mount source=nosql,target=nosql-data \
--rm cassandra \
cqlsh cassandra
```
