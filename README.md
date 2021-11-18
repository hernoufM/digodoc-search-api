# Digodoc Search API

Digodoc Search API is a server that makes different type of search throughout [Digodoc](https://ocamlpro.github.io/digodoc/) DB.
<!-- TODO : OcamlPro API link -->

## Documentation

[Digodoc Search API](https://hernoufm.github.io/digodoc-search-api)

## Setup

You need to have postgresql installed (>= 9.5): [Manual](https://www.postgresqltutorial.com/install-postgresql-linux/).
You need to have a working setup of opam/ocaml (>=4.08.1): [Manual](https://opam.ocaml.org/doc/Install.html), with sandbox mode disabled ([article](https://camlspotter.gitlab.io/blog/2019-04-16-pgocaml/)) for postgresql to be working.
In postgresql you should have a user with the same name as your computer user, with some specific rights:

```text
sudo -i -u postgres
psql
CREATE USER <user>;
ALTER ROLE <user> CREATEDB;
ALTER ROLE <user> SET search_path TO db, public;
```

## Building Dependencies

```bash
make init
```

## Building

```bash
make
```

## Launching

To launch the API server:

```bash
./bin/api-server config/api_conf.json
```

It should prints to the terminal :

```bash
Starting servers on ports [11001]
Starting COHTTP server (port: 11001)

```
<!-- Link to Digodoc index api -->
If you haven't installed Digodoc Index API, then your DB tables are not filled yet. So every API service will return an empty result.

### Launching on background

One day, you would like probably to launch your API on the background. For this, you should remplace variable ```<wd-path>``` by ```.``` inside */script/api.sh*.
Then run in your shell command to start

```bash
scripts/api.sh start
```

and to stop your server.

```bash
scripts/api.sh stop
```

Launching server with script *api.sh* creates directory *logs/* with two files *api.log* (stdout of server) and *api.pid*
(pid of server that is used to stop server).

### Launching as a Linux service

To be able launch your server with system boot you should define a corresponding Linux service. To do that:

- Remplace variables ```<wd-path>,<user>,<group>``` with proper data inside */script/digodoc_search.service*.
- Remplace variable ```<wd-path>``` with your path **absolute** to *digodoc_search_api* workspace inside */script/api.sh*.
- Run :

    ```bash
    # copy to other user/system services
    sudo cp scripts/api.service /etc/systemd/system/
    # to boot service with linux
    systemctl enable digodoc_search_api.service
    # to start service
    systemctl start digodoc_search_api.service
    ```

To stop service run :

```bash
systemctl stop digodoc_search_api.service
```

Launching server as a Linux service will also create directory *logs/*.

## DB versions

Server has a mechanism that allows to switch between different versions of DB. Actually there are just two versions : 0 and 1. When DB is on version 0 it's empty (without tables). Passing on version 1 creates a entire schema.
To know current version of DB, run:

```bash
make db-version 
```

To update DB (current_version+1), run:

```bash
make db-update 
```

To downgrade :

```bash
make db_downgrade
```

> Warning : Passing to version 0 delete all entries in DB. You will be forced then to refill tables with <!-- TODO: link to digodoc index api --> index api.

## Api viewer

**Redoc-cli** is an application that constructs site that helps to explore API, its services, arguments and results. It will helps you to test and to request your server outside of digodoc and OCaml.  To open it in your default browser do :

- Install Node js (>=12.0) [Manual](https://computingforgeeks.com/how-to-install-nodejs-on-ubuntu-debian-linux-mint/)
- Run :

    ```bash
    #just for the first time
    sudo apt-get install xdg-utils nodejs npm
    sudo npm i -g redoc-cli

    make view-api 
    ```

> Note that you should update page one time to open API viewer.

## Local documentation

You can also construct your own site of documentation for current version of server with **odoc**. To generate documentation site inside *docs/* directory and open it with your default browser, run:

```bash
#just for the first time
sudo apt-get install xdg-utils php

make view-docs
```

> Note that only public libraries and their modules will appear on the site despite that private ones are still could be found inside *docs/* directory.
