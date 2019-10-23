## A Shiny web-app for visualizing trait data in BETYdb

This shiny application provides a visual summary of the available data in BETYdb. You can view time series box, line plots, and heatmaps available for every season. Descriptions of selected trait and methods of measurement are provided as well as instructions on how to download data.

## Development

A local development environment can be set up on your computer. In order to run this shiny application, you will need to download R, install required R packages, and download Docker. Docker will be used in creating a connection to a BETYdb database. 

You will also need to create a globus account at [globus.org](https://www.globus.org/) and download [Globus Connect Personal](https://www.globus.org/globus-connect-personal). The `terraref` endpoint contains RGB fullfield image thumbnails that can be displayed in the map tab of the application.

More detailed information on required R packages, image thumbnails, how to set up a database connection, and how to get the shiny application running is described in the following sections.

### Clone the repository

Clone the [terraref/traitvis](https://github.com/terraref/traitvis-webapp) repository:

```sh
git clone https://github.com/terraref/traitvis-webapp.git
cd traitvis-webapp
```

### Database connection

This application requires a connection to a BETYdb database. It requires a BETYdb database that uses the experiments table to associate time ranges and sites or plots with specific experiments or seasons.

A connection to a local instance of BETYdb can be created by running the following Docker commands (you can ignore premission errors):

```sh
docker-compose -p bety up  -d postgres

# ignore lots of errors in the following step
docker run -ti --rm --network bety_bety -e BETY_INITIALIZE_URL='-w https://terraref.ncsa.illinois.edu/bety/dump/bety0/bety.tar.gz' pecan/bety:develop initialize

docker run -ti --rm --network bety_bety -e REMOTE_SERVERS=6  pecan/bety:terra sync
```

Note: In the docker-compose.override.yml file, `5433:5432` maps your computer's port 5433 to the Docker container's Postgres port 5432. If you would like to expose another port, change the first number in the pairing to whatever number you want. Just make sure to update the port number when setting the `bety_port` environment variable below.

### R packages

This application depends on a number of R packages; this is how you can install them:

```
install.packages(c('shiny', 'shinythemes', 'scales', 'lubridate', 'dplyr', 'ggplot2', 'timevis', 'rgeos', 'leaflet', 'cronR', 'stringr', 'kableExtra', 'raster', 'mapview', 'leafem', 'sf'))
```

### Download image thumbnails

Fullfield image thumbnails are available to be displayed under heatmaps. These thumbnails can be downloaded from the TERRAREF endpoint on [Globus](https://www.globus.org/), which you will need permission to access. If you have not already done so:

1. sign up for terraref.org/beta 
2. send your globus user email to dlebauer@arizona.edu.
3. install the [Globus connect personal](https://www.globus.org/globus-connect-personal) application and register your computer.

Then transfer the directory  `/ua-mac/Level_2/rgb_fullfield/_thumbs/` to the following path in your home directory: `~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs`.

### How to run Shiny Application

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
