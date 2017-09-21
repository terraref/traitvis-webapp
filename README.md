## A Shiny web-app for visualizing trait data in BETYdb

Time series box, line plots and heatmaps for every available season

## Installation

### Database Connection

This application requires a connection to a BETYdb database. It requires a BETYdb database that uses the experiments table to associate time ranges and sites or plots with specific experiments or seasons.

There are three methods that you can use access to the TERRAREF instance of BETYdb:

1. Use the NDS Workbench at terraref.ndslabs.org (preferred)
2. Use an ssh tunnel. Developers can get ssh access to the TERRA REF database server, you can [request here](https://identity.ncsa.illinois.edu/join/TU49BUUEDM) and the connect with
    ```
    ssh -Nf -L 5432:localhost:5432 bety6.ncsa.illinois.edu
    ```    
3. Install a local instance of BETYdb
    ```
    load.bety.sh -c -u -m 99 -r 0 -w https://terraref.ncsa.illinois.edu/bety/dump/bety0/bety.tar.gz
    load.bety.sh -m 99 -r 6 -w https://terraref.ncsa.illinois.edu/bety/dump/bety6/bety.tar.gz
    ```

### Easy way using Docker

```sh
docker build -t shiny-traits .
## enter bash as root to setup
docker run --rm -t -i shiny-traits /bin/bash
## after deploying app, launch so it can be accessed by browser at localhost:3838
docker run --rm -t -i -p 3838:3838 shiny-traits
```

### Configuring a Server

This is the manual version of what is automated using Docker.

#### Setup and Notes

* Data are cached in a file `cache.RData` and updated using the cronR R interface to cron
* Database connection parameters can be set as environment variables. By default they are:

```
bety_dbname=bety
bety_password=bety
bety_host-localhost
bety_port=5432
bety_user=bety
```

These will work with the ssh tunnel described above as well as with a local installation of BETYdb.

#### Dependencies

- shiny
- shinythemes
- lubridate
- dplyr
- ggplot2
- timevis
- rgeos
- leaflet
- cronR

```r
lapply(list('shiny', 'shinythemes', 'lubridate', 'dplyr', 'ggplot2', 'timevis', 'rgeos', 'leaflet', 'cronR'),
       install.packages)
```

to install cron (Ubuntu)

```sh
apt-get update
apt-get install cron
```

#### Deploy

```r
shiny::runGitHub(repo = 'reference-data', 
                 username= 'terraref', 
                 subdir = 'experiment-trait-data-visualizer',
                 launch.browser = FALSE
                 )
```

