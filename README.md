## A Shiny web-app for visualizing trait data in BETYdb

Time series box, line plots and heatmaps for every available season


### Set up a local development environment

#### Database connection

This application requires a connection to a BETYdb database. It requires a BETYdb database that uses the experiments table to associate time ranges and sites or plots with specific experiments or seasons.

You can connect to a local instance of a BETYdb database using the TERRA-REF BRAPI API. You will need to clone the [terraref/brapi github repository](https://github.com/terraref/brapi) and run some Docker commands to create a connection. 

Clone terraref/brapi repository:

```sh
git clone https://github.com/terraref/brapi.git
cd brapi
```

Within the brapi folder, you will need to create a `docker-compose.override.yml` to expose a port on your local machine. The following chunk will expose port 5433 and should be added to the file:

```sh
version: '3'
services:
  postgres:
    ports:
      - '5433:5432'
```

Initialize the database:

```sh
docker-compose up -d postgres
docker-compose run --rm bety initialize
docker-compose run --rm bety sync
```

### R packages

This application requires the following R packages to be installed: `shiny`, `shinythemes`, `scales`, `lubridate`, `dplyr`, `ggplot2`, `timevis`, `rgeos`, `leaflet`, `cronR`, `stringr`, `kableExtra`, `raster`, `mapview`, `leafem`, and `sf`.

### Download image thumbnails

Thumbnails to be displayed under heatmap can be downloaded from the `/ua-mac/Level_2/rgb_fullfield/_thumbs/` endpoint on [Globus](https://www.globus.org/).

These thumbs should be saved to the following path in your home directory `~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs`.

## Deploy application using Docker

```sh
git clone https://github.com/terraref/traitvis-webapp
cd traitvis-webapp

docker build -t shiny-traits .
## after deploying app, launch so it can be accessed by browser at localhost:3838
docker run --rm -t -i -p 3838:3838 shiny-traits
```

To set one of the environment variables (see "Setup and Notes" section), you can use the `-e` flag, for example to connect to the TERRA REF trait database (bety6), run:

```sh
docker run --rm -t -i -e bety_host=bety6 -p 3838:3838 shiny-traits
```

To enter the container bash shell as root, e.g. for development, testing, reviewing logs:

```sh
docker run --rm -t -i shiny-traits /bin/bash
```

The shiny-server.conf file is used to increase the default timeout to avoid errors on initialization if BETYdb is taking time to load.
