library(tidyverse)
library(rmuppet)

fs::dir_create(paste(md,'progn_files',sep='/'))
harv_rates <- seq(0.01,1,by=0.01) 

jnk <- 
  harv_rates %>% 
  ## setup HCR files
  map(function(x) 
    paste(md,'Files','hadprognosis.dat.biorule.adyear',sep='/') %>% 
      read_lines() %>% 
      rmuppet:::line_replace(x, "# HarvestRate") %>% 
      rmuppet:::line_replace(5, "# Btrigger") %>% 
      rmuppet:::line_replace(2, "# MaxHarvestRate") %>% 
      write_lines(paste(md,'progn_files',sprintf('progn_adyear_%s',x),sep='/'))) 
  
jnk <- 
  harv_rates %>% 
  ##setup run files
  map(function(x) 
    paste(md,'params','icehad.dat.prog.template',sep='/') %>% 
      read_lines() %>% 
      rmuppet:::line_replace(paste('progn_files',sprintf('progn_adyear_%s',x),sep='/'), "#Prognosis file") %>% 
      write_lines(paste(md,'progn_files',sprintf('icehad_adyear_%s',x),sep='/'))) 
  
jnk <- 
harv_rates %>% 
  map(function(x) {
    print(x)
    callMuppet(ind=paste('progn_files',sprintf('icehad_adyear_%s',x),sep='/'),'mceval',run_dir = md)
  #  if(fs::file_exists(paste(md,'all.mcmc',sep='/')))
      fs::file_delete(paste(md,'all.mcmc',sep='/'))
    fs::dir_ls(md,regexp = '.mcmc') %>%
      map(~fs::file_move(path = ., new_path = paste0(md,'/','progn_files/', stringr::str_extract(.,'([a-z0-9]+).mcmc$'),x)))
  })

res <- 
  fs::dir_ls(paste(md,'progn_files',sep='/'),regexp = '.mcmc') %>%
  purrr::set_names(.,stringr::str_extract(.,'([a-z0-9]+).mcmc(1$|0.[0-9]+)$') %>% 
                     stringr::str_replace('.mcmc','_')) %>%  
  as.list() %>% 
  purrr::map(readr::read_delim, delim=' ') %>% 
  purrr::map(dplyr::mutate, iter=1:n()) %>% 
  purrr::map(tidyr::gather, key='variable',value='value',-iter) %>% 
  purrr::map(tidyr::separate, variable, c('variable','year'), convert = TRUE) %>% 
  dplyr::bind_rows(.id = 'filename') %>% 
  tidyr::separate(filename,c('file','rate'),sep = '_',convert = TRUE)

