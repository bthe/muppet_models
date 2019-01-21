library(tidyverse)
library(rmuppet)

#btrigger <- round(b_ref$b_pa,2)
#ass_error <- TRUE

btrigger <- 5
ass_error <- TRUE


fs::dir_create(paste(md,sprintf('progn_files_%s',if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_')),sep='/'))
fs::dir_create(paste(md,sprintf('progn_out_%s',if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_')),sep='/'))
harv_rates <- seq(0.01,1,by=0.01) 


jnk <- 
  harv_rates %>% 
  ## setup HCR files
  map(function(x) {
    paste(md,'Files','hadprognosis.dat.biorule.adviceyear',sep='/') %>% 
      read_lines() %>% 
      rmuppet:::line_replace(x, "# HarvestRate") %>% 
      rmuppet:::line_replace(btrigger, "# Btrigger") %>% 
      rmuppet:::line_replace(2, "# MaxHarvestRate") %>%
      rmuppet:::line_replace(if(ass_error) 0.18 else 0.01, "# AssessmentCV") %>% ## hvaðan kemur þetta gildi
      rmuppet:::line_replace(if(ass_error) 0.45 else 0, "# AssessmentCorr") %>%
      write_lines(paste(md,sprintf('progn_files_%s',if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_')),
                        sprintf('progn_adyear_%s',x),sep='/'))
    
    readr::read_lines(paste(md,'Files','outputparameters.dat',sep='/')) %>% 
      rmuppet:::line_replace(sprintf('progn_out_%s/',if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_')), '# Prefix none') %>% 
      rmuppet:::line_replace(sprintf('.hcr_%s',x), '# Postfix none') %>% 
      readr::write_lines(paste(md,sprintf('progn_files_%s',if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_')),sprintf('out_%s.dat',x),sep='/'))
    
    paste(md,'params','icehad.prognosis',sep='/') %>% 
      read_lines() %>% 
      rmuppet:::line_replace(paste(sprintf('progn_files_%s',if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_')), 
                                   sprintf('progn_adyear_%s',x),sep='/'), "#Prognosis file") %>% 
      rmuppet:::line_replace(sprintf('progn_files_%s/out_%s.dat',if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_'), x),
                             '# Fbar range output biomasses etc') %>% 
      write_lines(paste(md,sprintf('progn_files_%s',if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_')),
                        sprintf('icehad_adyear_%s',x),sep='/'))
    }) 



  

  
jnk <- 
harv_rates %>% 
  parallel::mclapply(function(x) {
    #print(x)
    callMuppet(ind=paste(sprintf('progn_files_%s',if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_')),sprintf('icehad_adyear_%s',x),sep='/'),'mceval',run_dir = md)
  #  if(fs::file_exists(paste(md,'all.mcmc',sep='/')))
  #    fs::file_delete(paste(md,'all.mcmc',sep='/'))
  #  fs::dir_ls(md,regexp = '.mcmc') %>%
  #    map(~fs::file_move(path = ., new_path = paste0(md,'/','progn_files/', stringr::str_extract(.,'([a-z0-9]+).mcmc$'),x)))
  }, mc.cores = parallel::detectCores(logical = TRUE))

res <- 
  fs::dir_ls(paste(md,sprintf('progn_out_%s',if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_')),sep='/'),regexp = '.mcmc') %>%
  purrr::set_names(.,stringr::str_extract(.,'([a-z0-9]+).mcmc.hcr_(1$|0.[0-9]+)$') %>% 
                     stringr::str_replace('.mcmc.hcr_','_')) %>%  
  as.list() %>% 
  purrr::map(readr::read_delim, delim=' ') %>% 
  purrr::map(dplyr::mutate, iter=1:n()) %>% 
  purrr::map(tidyr::gather, key='variable',value='value',-iter) %>% 
  purrr::map(dplyr::mutate, value = as.numeric(value)) %>% 
  #purrr::map(tidyr::separate, variable, c('variable','year'), convert = TRUE) %>% 
  dplyr::bind_rows(.id = 'filename') %>% 
  tidyr::separate(filename,c('filename','rate'),sep = '_',convert = TRUE) %>% 
  dplyr::mutate(year = ifelse(grepl('parameter',filename),NA,
                              stringr::str_extract(variable,'\\.([0-9]+)')),
                variable = ifelse(grepl('parameter',filename),NA,
                                  stringr::str_remove(variable,'\\.[0-9]+')),
                year = as.numeric(gsub('\\.','',year)))



res_hcr_agg <- 
  res_hcr %>% 
  filter(filename != 'parameter') %>% 
  bind_rows(res_hcr %>%
              select(-filename) %>% 
              filter(variable %in% c('CalcCatchIn1000tons','HCRrefbio')) %>% 
              spread(variable, value) %>% 
              mutate(variable = 'HR',
                     value = CalcCatchIn1000tons/HCRrefbio) %>% 
              select(-c(CalcCatchIn1000tons,HCRrefbio))) %>% 
  group_by(variable,year,rate) %>% 
  summarise(m = median(value,na.rm = TRUE),
            u = quantile(value,0.95,na.rm = TRUE),
            uu = quantile(value,0.75,na.rm = TRUE),
            ll = quantile(value,0.25,na.rm = TRUE),
            l  = quantile(value,0.05,na.rm = TRUE)) 
 




save(res_agg,res_hcr_agg,res_no_trigger_agg, old_res_agg,file='02-haddock/99-docs/res_agg.Rdata')

