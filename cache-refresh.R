library(dplyr)
library(tools)
library(lubridate)
options(scipen = 999)

print(getwd())
print("that is the current directory")

# set up remote connection to BETYdb
# bety_src <- src_postgres(
#   dbname   = ifelse(Sys.getenv('bety_dbname')   == '', 'bety', Sys.getenv('bety_dbname')),
#   password = ifelse(Sys.getenv('bety_password') == '', 'bety', Sys.getenv('bety_password')),
#   host     = ifelse(Sys.getenv('bety_host')     == '', 'bety.terraref', Sys.getenv('bety_host')),
#   port     = ifelse(Sys.getenv('bety_port')     == '', '5432', Sys.getenv('bety_port')),
#   user     = ifelse(Sys.getenv('bety_user')     == '', 'bety', Sys.getenv('bety_user'))
# )
# 
bety_src <- src_postgres(
  dbname   = ifelse(Sys.getenv('bety_dbname')   == '', 'bety', Sys.getenv('bety_dbname')),
  password = ifelse(Sys.getenv('bety_password') == '', 'DelchevskoOro', Sys.getenv('bety_password')),
  host     = ifelse(Sys.getenv('bety_host')     == '', 'bety6.ncsa.illinois.edu', Sys.getenv('bety_host')),
  port     = ifelse(Sys.getenv('bety_port')     == '', '5432', Sys.getenv('bety_port')),
  user     = ifelse(Sys.getenv('bety_user')     == '', 'viewer', Sys.getenv('bety_user'))
)
# get all relevant data from BETYdb for a given subexperiment, write to cache file
get_data_for_subexp <- function(subexp, exp_name) {

  # destination for all data for given subexperiment
  subexp_data <- list(start_date = subexp[[ 'start_date' ]], end_date = subexp[[ 'end_date' ]])
  
  site_ids <- tbl(bety_src, 'experiments_sites') %>% 
    filter(experiment_id == subexp[[ 'id' ]]) %>%
    select(site_id) %>%
    collect() %>% unlist(use.names = FALSE)
  if (is.null(site_ids)){
    return()
  }
  
  # only use trait records associated with the relevant sites
  traits_table <- tbl(bety_src, 'traits', n = Inf) %>%
    filter(date >= subexp[[ 'start_date' ]] & date <= subexp[[ 'end_date' ]] & checked >= 0) %>%
    filter(site_id %in% site_ids) %>%
    select(date, mean, variable_id, cultivar_id, treatment_id, site_id, method_id)
  
  n_traits <- traits_table %>% summarize(n = n()) %>% collect(n = Inf)
  if(n_traits == 0){
    return()
  } 
  
  sites_table <- tbl(bety_src, sql("select ST_AsText(sites.geometry) AS geometry, id from sites")) %>% 
    filter(!is.na(geometry)) %>% 
    filter(id %in% site_ids) %>%
    dplyr::rename(site_id = id)
  
  cultivars_table <- tbl(bety_src, 'cultivars') %>%
    # dplyr::rename(cultivar_id = id, cultivar_name = name) %>% 
    # workaround bug in dplyr https://github.com/tidyverse/dplyr/issues/2943 that causes bug
    #    has been fixed now ... 
    rename(cultivar_id = id) %>% 
    rename(cultivar_name = name) %>%
    select(cultivar_id, cultivar_name) 
  
  methods_table <- tbl(bety_src, 'methods') %>% 
    dplyr::rename(method_id = id) %>% 
    dplyr::rename(method = name) %>% 
    select(method_id, method)
    
  
  traits <- traits_table %>% 
    left_join(sites_table, by = 'site_id') %>%
    left_join(cultivars_table, by = 'cultivar_id') %>% 
    left_join(methods_table, by = 'method_id')
  
  
  variables <- tbl(bety_src, 'variables') %>%
    rename(variable_id = id)  %>% 
    semi_join(traits, by = 'variable_id')
  
  variable_ids <- variables %>% select(variable_id) %>% collect
  
  trait_data <- list()
  for (curr_variable_id in variable_ids$variable_id) {

    variable_data <- list()
    
    variable_record <- variables %>% filter(variable_id == curr_variable_id) %>% 
      select(variable_id, name, units) %>% collect
    
    variable_name <- tools::toTitleCase(gsub('_', ' ', variable_record %>% select(name) %>% collect))
    variable_traits <- traits %>% filter(variable_id == curr_variable_id) %>% collect(n = Inf)
    
    variable_data[[ 'units' ]] <- variable_record %>% select(units) 
    variable_data[[ 'id' ]] <- variable_record %>% select(variable_id)
    variable_data[[ 'traits' ]] <- variable_traits
    
    # subset trait data by variable
    trait_data[[ variable_name ]] <- variable_data
  }
  # save trait data for all variables
  subexp_data[[ 'trait_data' ]] <- trait_data
  
  # only use management records associated with the relevant treatments
  management_ids <- tbl(bety_src, 'managements_treatments') %>%
    semi_join(traits, by = 'treatment_id') %>%
    collect() %>% unlist(use.names = FALSE)
  
  if (!is.null(management_ids)) {
    subexp_data[[ 'managements' ]] <- tbl(bety_src, 'managements') %>%
    filter(date >= subexp[[ 'start_date' ]] & date <= subexp[[ 'end_date' ]]) %>%
    filter(id %in% management_ids) %>%
    select(id, date, mgmttype, notes) %>%
    collect()
  }
  
  # load existing full_cache_data object if exists, otherwise use empty list object
  full_cache_data <- list()
  if (file.exists("cache.RData")){
    print("cache.RData already exists!")
    load("cache.RData")
  }
  
  if (!(exp_name %in% names(full_cache_data))) {
    full_cache_data[[ exp_name ]] <- list()
  }
  
  # save data for given subexp
  full_cache_data[[ exp_name ]][[ subexp[['name']] ]] <- subexp_data
  print("saving new data to file")
  file.create("/srv/shiny-server/cache/cache.RData.temp")
  save(full_cache_data, file = "/srv/shiny-server/cache/cache.RData.temp", compress = FALSE)
  # copy file to cache.RData
  file.copy("/srv/shiny-server/cache/cache.RData.temp","/srv/shiny-server/cache/cache.RData",overwrite=TRUE)

}

# get data for each experiment by subexperiment
get_data_for_exp <- function(exp_name, experiments) {
  subexp = subset(experiments, gsub(":.*$","", name) == exp_name)
  subexp[[ 'name' ]] = gsub(".*: ","", subexp[['name']])
  apply(subexp, 1, get_data_for_subexp, exp_name)
}

# get all experiments in BETYdb
experiments <- tbl(bety_src, 'experiments') %>% 
  select(id, name, start_date, end_date) %>% 
  collect() %>% as.data.frame()

exp_names <- unique(gsub(":.*$","", experiments[[ 'name' ]]))
lapply(exp_names, get_data_for_exp, experiments)
