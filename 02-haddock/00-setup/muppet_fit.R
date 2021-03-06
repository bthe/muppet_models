read_output <- function(file='resultsbyyear',out_dir = paste(md,'out',sep='/')){
  fs::dir_ls(out_dir,regexp = paste0(file,'\\.')) %>%
    purrr::set_names(.,stringr::str_extract(string = .,pattern = '\\..+$')) %>% 
    purrr::map(~readr::read_table2(.) %>% 
                 purrr::set_names(.,str_trim(names(.))) %>% 
                 dplyr::mutate_all(function(x) ifelse(x==-1,NA,x))) %>% 
    dplyr::bind_rows(.id='model') %>% 
    dplyr::mutate(model = stringr::str_remove(model,'\\.')) 
}


## read the output from the assessment
fit <- list(rby = read_output('resultsbyyear'),
            rbyage = read_output('resultsbyyearandage') %>% 
              mutate(CalcCno = ifelse(model == 'vpa',NA,CalcCno)),
            rbage = read_output('resultsbyage'),
            params = read_output('params') %>% 
              mutate(value = ifelse(grepl('ln|log|estSSBRecParameters',name),exp(value),value),
                     variable = gsub('ln|log','',name) %>% 
                       gsub('([a-zA-Z]+)\\[([0-9])\\]','\\1.\\2',.),
                     variable = ifelse(grepl('estSSBRecParameters',variable),
                                       forcats::fct_recode(gsub('estSSBRecParameters.','',variable,fixed = TRUE),
                                                           Rmax="1",ssbbreak = "2",
                                                           `Recruitment CV`='3',
                                                           rho='4') %>% as.character(),
                                       variable)),
            mcmc_results = fs::dir_ls(out_dir,regexp = '.mcmc') %>%
              purrr::set_names(.,stringr::str_extract(.,'([a-z0-9]+).mcmc.+$') %>% 
                                 stringr::str_remove('.mcmc')) %>% 
              as.list() %>% 
              #purrr::list_modify(all=NULL,par) %>% 
              purrr::map(readr::read_table2) %>% 
              purrr::map(dplyr::mutate, iter=1:n()) %>% 
              purrr::map(tidyr::gather, key='variable',value='value',-iter) %>% 
              purrr::map(dplyr::mutate, value = as.numeric(value)) %>% 
              #purrr::map(tidyr::separate, variable, c('variable','year'), convert = TRUE) %>% 
              dplyr::bind_rows(.id = 'filename') %>% 
              dplyr::mutate(year = ifelse(grepl('parameter',filename),NA,
                                          stringr::str_extract(variable,'\\.([0-9]+)')),
                            year = as.numeric(gsub('\\.','',year)),
                            variable = ifelse(grepl('parameter',filename),variable,
                                              stringr::str_remove(variable,'\\.[0-9]+'))) %>% 
              tidyr::separate(filename,c('filename','model')) %>% 
              dplyr::mutate(value = ifelse(grepl('estSSBRecParameters',variable),exp(value),value),
                            variable = ifelse(grepl('estSSBRecParameters',variable),
                                              forcats::fct_recode(gsub('estSSBRecParameters.','',variable,fixed = TRUE),
                                                                  Rmax="1",ssbbreak = "2",
                                                                  `Recruitment CV`='3',
                                                                  rho='4') %>% as.character(),
                                              variable)))






save(fit,file='02-haddock/99-docs/fit.Rdata')
