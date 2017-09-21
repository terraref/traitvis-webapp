FROM rocker/shiny

MAINTAINER "Nicholas Heyek and David LeBauer"

ENV bety_dbname=${bety_dbname:-bety}
ENV bety_password=${bety_password:-bety}
ENV bety_host=${bety_host:-localhost}
ENV bety_user=${bety_user:-bety}
ENV bety_port=${bety_port:-5432}

RUN apt-get update -qq \
         && apt-get -y install --no-install-recommends \
         cron \
         libgeos-dev \
         libpq-dev \
      && install2.r --error \
         cronR \
         dplyr \
         ggplot2 \
         leaflet \
         lubridate \
         shiny \
         shinythemes \
         rgeos \
         RPostgreSQL \
         timevis

COPY . /srv/shiny-server/