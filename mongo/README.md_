# MongoDB

Create a volume:
`$ docker volume create nosql`

Inspect the volume:
`$ docker volume inspect nosql`

This should bring up the place where the volume is. Put your data there

Build the container:
`$ docker run --name nosql-mongo -d --mount source=nosql,target=/nosql-data mongo`

Start the container (it will already be started the first time you build):
`$ docker start nosql-mongo`

Connect to bash in the container:
`$ docker exec -it nosql-mongo bash`

Now you should be inside the container, the prompt should have changed.

Import data:
`$ mongoimport --db DATABASE_NAME --collection COLLECTION_NAME /data/db/FILE`

Run the mango shell:
`$ mango`

The prompt should have changed again

`> use DATABASE_NAME`
