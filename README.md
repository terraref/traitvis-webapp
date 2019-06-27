## A Shiny web-app for visualizing trait data in BETYdb

Time series box, line plots and heatmaps for every available season

## Installation

### Easy deploy with Docker

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

### Database Connection

This application requires a connection to a BETYdb database. It requires a BETYdb database that uses the experiments table to associate time ranges and sites or plots with specific experiments or seasons.

Connection parameters can be set as environment variables. By default they are:

```
bety_dbname   = bety
bety_password = SECRET
bety_host     = bety6.ncsa.illinois.edu
bety_port     = 5432
bety_user     = viewer
```

There are three methods that you can use access to the TERRAREF instance of BETYdb:

1. If running on the NCSA Nebula cluster within the terraref domain, can be accessed by setting the environment variable `bety_host=bety6`
    1. This can be done within the NDS Workbench at terraref.ndslabs.org
2. Otherwise, use an ssh tunnel. Developers can get ssh access to the TERRA REF database server, you can [request here](https://identity.ncsa.illinois.edu/join/TU49BUUEDM) and the connect with
    ```sh
    ssh -Nf -L 5432:localhost:5432 bety6.ncsa.illinois.edu
    ```    
3. Install a local instance of BETYdb
    ```sh
    load.bety.sh -c -u -m 99 -r 0 -w https://terraref.ncsa.illinois.edu/bety/dump/bety0/bety.tar.gz
    load.bety.sh -m 99 -r 6 -w https://terraref.ncsa.illinois.edu/bety/dump/bety6/bety.tar.gz
    ```

#### Deploy Updates on Nebula / Kubernetes

After updating the master branch on GitHub, a new Docker image is built on [Docker hub](https://hub.docker.com/r/terraref/traitvis-webapp/builds/). The following commands can be used to deploy and inspect this app as it is being served at traitvis.workbench.org

Login to server (requires authentication)

```sh
ssh core@282.284.420.38 # = ip x 2
```

See what is running in the terraref namespace, including extractors and traitvis

```sh
kubectl get pods --namespace=terraref
``` 


The following restarts the container, which triggers the most recent image from [docker hub](https://hub.docker.com/r/terraref/traitvis-webapp/builds/) to be downloaded. 

```sh
kubectl delete pod terraref-traitvis-xxxx --namespace=terraref
#xxxx is a random string.
``` 


Show status (i.e. ContainerCreating, Running, etc)

```sh
kubectl get pods --namespace=terraref
```

to see logs:

```sh
kubectl logs -f terraref-traitvis-xxxx --namespace=terraref
```


to get an interactive terminal 

```sh
kubectl --namespace=terraref exec -it terraref-traitvis-xxxx bash
```

### Configuring a Server

This is the manual version of what is automated using Docker.

#### System Dependencies

##### OSX 

```sh
brew install geos
```

##### Ubuntu 

cron (Ubuntu)

```sh
apt-get update
apt-get install cron
```

Shiny server: see https://www.rstudio.com/products/shiny/download-server/

#### R packages

- shiny
- shinythemes
- lubridate
- dplyr
- ggplot2
- timevis
- rgeos
- leaflet
- cronR
- stringr
- kableExtra
- mapview
- sf

```r
lapply(list('shiny', 'shinythemes', 'lubridate', 'dplyr', 'ggplot2', 'timevis', 'rgeos', 'leaflet', 'cronR', 'stringr', 'kableExtra', 'mapview', 'sf'),
       install.packages)
```

#### Download image thumbnails

Thumbnails to be displayed under heatmap can be downloaded from the `/ua-mac/Level_2/rgb_fullfield/_thumbs/` endpoint on [Globus](https://www.globus.org/).

These thumbs should be saved to the following path in your home directory `~/data/terraref/sites/ua-mac/Level_2/rgb_fullfield/_thumbs`.

#### Deploy

```r
shiny::runGitHub(repo = 'reference-data', 
                 username= 'terraref', 
                 subdir = 'experiment-trait-data-visualizer',
                 launch.browser = FALSE
                 )
```

