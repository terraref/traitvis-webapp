## A Shiny web-app for visualizing trait data in BETYdb

This shiny application provides a visual summary of the available data in BETYdb. You can view time series box, line plots, and heatmaps available for every season by trait. Descriptions of selected trait and methods of measurement are provided. Instructions on how to download selected data can be found in the downloads tab.

## Development

A local development environment can be set up on your computer. In order to run this shiny application, you will need to download R, install required R packages, and download Docker. Docker will be used in creating a connection to a BETYdb database. 

You will also need to create a globus account at [globus.org](https://www.globus.org/) and download [Globus Connect Personal](https://www.globus.org/globus-connect-personal). The `terraref` endpoint contains RGB fullfield image thumbnails that can be displayed in the map tab of the application.

More detailed information on required R packages, image thumbnails, how to set up a database connection, and how to get the shiny application running is described in the following sections.

### Database connection

This application requires a connection to a BETYdb database. It requires a BETYdb database that uses the experiments table to associate time ranges and sites or plots with specific experiments or seasons.

A connection to a local instance of BETYdb can be created using the TERRA-REF BRAPI API. Clone the [terraref/brapi repository](https://github.com/terraref/brapi) and run Docker commands to create a connection. 

Clone terraref/brapi repository:

```sh
git clone https://github.com/terraref/brapi.git
cd brapi
```

Within the brapi folder, create a `docker-compose.override.yml` file to expose a port on your local machine. The following chunk can be used to expose port 5432 and should be added to the file:

```sh
version: '3'
services:
  postgres:
    ports:
      - '5432:5432'
```

Note: If you are already running Postgres, it is likely that port 5432 is being used on your computer. 
In this case, you can assign another number such as 5433. 
Just make sure to change the first number in the pairing to the number you choose, e.g. 5433:5432 means you are mapping your computer's port 5433 to the Docker container's Postgres port 5432.

Initialize the database:

```sh
docker-compose up -d postgres
docker-compose run --rm bety initialize
docker-compose run --rm bety sync
```

### R packages

This application depends on a number of R packages; this is how you can install them:

```
install.packages(c('shiny', 'shinythemes', 'scales', 'lubridate', 'dplyr', 'ggplot2', 'timevis', 'rgeos', 'leaflet', 'cronR', 'stringr', 'kableExtra', 'raster', 'mapview', 'leafem', 'sf'))
```

### Download image thumbnails

Fullfield image thumbnails are available to be displayed under heatmaps. These thumbnails can be downloaded from the `/ua-mac/Level_2/rgb_fullfield/_thumbs/` endpoint on [Globus](https://www.globus.org/).

These thumbs should be saved to the following path in your home directory `~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs`.

### How to run Shiny Application

Clone the [terraref/traitvis](https://github.com/terraref/traitvis-webapp) repository:

```sh
git clone https://github.com/terraref/traitvis-webapp.git
cd traitvis-webapp
```

The `cache-refresh.R` script needs to be run and requires a database connection. To connect to the local instance of BETYdb that you set up above, you will need to set the following connection parameters as environment variables.

In your R console, run the following commands:

```sh
Sys.setenv(bety_host = 'localhost')
Sys.setenv(bety_port = '5433')
source('cache-refresh.R')
```

Running the cache-refresh script should create a `cache` folder containing a `cache.RData` file.

Next, start up the application by running:

```sh
shiny::runAPP()
```

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
