library(tidyverse)
library(rmuppet)

#btrigger <- round(b_ref$b_pa,2)
#ass_error <- TRUE



b_ref <- 
  fit$rby %>% 
  filter(model == 'logit_length') %>% 
  summarise(b_loss=min(Spawningstock),
            b_lim = b_loss,
            b_pa = b_lim*exp(1.645*0.15)) 


run_muppet_hcr <- function(btrigger=5,ass_error=TRUE,impl_error=TRUE,harv_rates = seq(0,1,by=0.01)){
  
  progn_files <- sprintf('progn_files_%s_%s',
                         if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_'),
                         if(impl_error) 'impl_error' else 'no_impl_error')
  progn_out <- sprintf('progn_out_%s_%s',
                         if(ass_error) btrigger else paste(btrigger, 'no_ass_error',sep='_'),
                         if(impl_error) 'impl_error' else 'no_impl_error')
  
  
  
  fs::dir_create(paste(md,progn_files,sep='/'))
  fs::dir_create(paste(md,progn_out,sep='/'))
  
  
  
  
  jnk <- 
    harv_rates %>% 
    ## setup HCR files
    map(function(x) {
      paste(md,'Files','hadprognosis.dat.biorule.adviceyear',sep='/') %>% 
        read_lines() %>% 
        rmuppet:::line_replace(x, "# HarvestRate") %>% 
        rmuppet:::line_replace(btrigger, "# Btrigger") %>% 
        rmuppet:::line_replace(2, "# MaxHarvestRate") %>%
        rmuppet:::line_replace(if(ass_error) 0.15 else 0.01, "# AssessmentCV") %>% ## hvaðan kemur þetta gildi
        rmuppet:::line_replace(if(ass_error) 0.67 else 0, "# AssessmentCorr") %>%
        rmuppet:::line_replace(if(impl_error) 0.065 else 0.01, "# ImplementationCV") %>% ## hvaðan kemur þetta gildi
        rmuppet:::line_replace(if(impl_error) 0.65 else 0, "# ImplementationCorr") %>%
        write_lines(paste(md,progn_files,
                          sprintf('progn_adyear_%s',x),sep='/'))
      
      readr::read_lines(paste(md,'Files','outputparameters.dat',sep='/')) %>% 
        rmuppet:::line_replace(paste0(progn_out,'/'), '# Prefix none') %>% 
        rmuppet:::line_replace(sprintf('.hcr_%s',x), '# Postfix none') %>% 
        readr::write_lines(paste(md,progn_files,
                                 sprintf('out_%s.dat',x),sep='/'))
      
      paste(md,'params','icehad.prognosis',sep='/') %>% 
        read_lines() %>% 
        rmuppet:::line_replace(paste(progn_files, 
                                     sprintf('progn_adyear_%s',x),sep='/'), "#Prognosis file") %>% 
        rmuppet:::line_replace(sprintf('%s/out_%s.dat',
                                       progn_files,
                                       x),
                               '# Fbar range output biomasses etc') %>% 
        write_lines(paste(md,progn_files,
                          sprintf('icehad_adyear_%s',x),sep='/'))
    }) 
  
  
  
  
  
  
  jnk <- 
    harv_rates %>% 
    parallel::mclapply(function(x) {
      #print(x)
      callMuppet(ind=paste(progn_files,
                           sprintf('icehad_adyear_%s',x),sep='/'),'mceval',run_dir = md)
      #  if(fs::file_exists(paste(md,'all.mcmc',sep='/')))
      #    fs::file_delete(paste(md,'all.mcmc',sep='/'))
      #  fs::dir_ls(md,regexp = '.mcmc') %>%
      #    map(~fs::file_move(path = ., new_path = paste0(md,'/','progn_files/', stringr::str_extract(.,'([a-z0-9]+).mcmc$'),x)))
    }, mc.cores = parallel::detectCores(logical = TRUE))
  
  res <- 
    fs::dir_ls(paste(md,progn_out,
                     sep='/'),regexp = '.mcmc') %>%
    purrr::set_names(.,stringr::str_extract(.,'([a-z0-9]+).mcmc.hcr_(1$|0.[0-9]+|0$)$') %>% 
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
  
  
  
  res_agg <- 
    res %>% 
    filter(filename != 'parameter') %>% 
    bind_rows(res %>%
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
              l  = quantile(value,0.05,na.rm = TRUE),
              r1 = value[100],
              r2 = value[200],
              r3 = value[300],
              btrigger = btrigger,
              ass_error = ass_error,
              impl_error = impl_error) 
  
  return(list(res=res,res_agg=res_agg))
  
}

#res_ass_error_5 <- run_muppet_hcr(btrigger = 5,impl_error = FALSE,harv_rates = .4)
res_no_ass_error <- run_muppet_hcr(btrigger = 5,ass_error = FALSE,impl_error = FALSE)
res_ass_error <- run_muppet_hcr(btrigger = round(b_ref$b_pa,1),impl_error = FALSE)
#res_assimpl_error <- run_muppet_hcr(btrigger = 55.1,harv_rates = .4)
res_assimpl_error <- run_muppet_hcr(btrigger = round(b_ref$b_pa,1))



res_agg <-
  
  res_no_ass_error$res_agg %>% 
 # bind_rows(res_ass_error_5$res_agg %>% mutate(btrigger = 5,ass_error = TRUE, impl_error = FALSE)) %>% 
  bind_rows(res_ass_error$res_agg)  %>% 
  bind_rows(res_assimpl_error$res_agg) 
  #bind_rows(res_newbp$res_agg %>% mutate(btrigger = 62.9,ass_error = TRUE, impl_error = TRUE)) 
res_hcr <- 
  res_assimpl_error$res %>% 
  filter(rate == 0.4)
  

hcr_below_ref_p <-
  res_assimpl_error$res %>% 
  filter(variable == 'Spawningstock',year>tyr) %>% 
  group_by(rate,year) %>% 
  summarise(p = mean(value < b_ref$b_lim),
            p_pa = mean(value < b_ref$b_pa)) %>% 
  group_by(rate) %>% 
  summarise(p1 = mean(p),
            p3 = max(p),
            p1_pa = mean(p_pa),
            p3_pa = max(p_pa)) %>% 
  left_join(res_assimpl_error$res %>% 
              filter(variable == 'Spawningstock',year>tyr) %>% 
              group_by(rate,iter) %>% 
              summarise(below = max(value < b_ref$b_lim),
                        below_pa = max(value < b_ref$b_pa)) %>% 
              group_by(rate) %>% 
              summarise(p2 = mean(below),
                        p2_pa = mean(below_pa))) %>%  
  mutate(period = 'Year > 2018') %>% 
  bind_rows(res_assimpl_error$res %>% 
              filter(variable == 'Spawningstock',year %in% tyr:(tyr+5)) %>% 
              group_by(rate,year) %>% 
              summarise(p = mean(value < b_ref$b_lim),
                        p_pa = mean(value < b_ref$b_pa)) %>% 
              group_by(rate) %>% 
              summarise(p1 = mean(p),
                        p3 = max(p),
                        p1_pa = mean(p_pa),
                        p3_pa = max(p_pa)) %>% 
              left_join(res_assimpl_error$res %>% 
                          filter(variable == 'Spawningstock', year %in% tyr:(tyr+5)) %>% 
                          group_by(rate,iter) %>% 
                          summarise(below = max(value < b_ref$b_lim),
                                    below_pa = max(value < b_ref$b_pa)) %>% 
                          group_by(rate) %>% 
                          summarise(p2 = mean(below),
                                    p2_pa = mean(below_pa))) %>% 
              mutate(period = '2023 > Year > 2018'))


hcr_rate <- 0.4

yield_by_rate <- 
  res_agg %>% 
  filter(variable == 'CalcCatchIn1000tons',
         year > 2030, btrigger == 5) %>%
  group_by(rate) %>% 
  summarise(c = median(m)) 

F_lim <- 
  res_agg %>% 
  filter(year==2060,variable=='RefF',btrigger == 5) %>% 
  semi_join(res_agg %>%
              ungroup() %>% 
              filter(variable == 'Spawningstock',year == 2060, m>b_ref$b_lim,btrigger == 5) %>% 
              tail(1) %>% 
              select(rate))

F_pa <- 
  res_agg %>% 
  filter(year==2060,variable=='RefF', btrigger == 5) %>% 
  semi_join(res_agg %>%
              ungroup() %>% 
              filter(variable == 'RefF',year == 2060, m<F_lim$m/1.4,btrigger == 5) %>% 
              tail(1) %>% 
              select(rate))

F_p5 <- 
  res_agg %>% 
  filter(year==2060,variable=='RefF',btrigger > 5, impl_error == TRUE) %>% 
  semi_join(hcr_below_ref_p %>% 
              #group_by(period) %>% 
              filter(period == 'Year > 2018',p3<0.05) %>% 
              filter(p3 == max(p3))) %>% 
  filter(rate == max(rate))



F_msy <- 
  res_agg %>% 
  filter(year==2060,variable == 'RefF',btrigger == 5) %>% 
  semi_join(yield_by_rate %>% 
              ungroup() %>% 
              filter(c==max(c)) %>% 
              mutate(rate = min(rate,F_p5$rate)))


ref_points <- 
  b_ref %>% 
  bind_cols(tibble(Flim = F_lim$m, 
                   HR_lim = F_lim$rate,
                   Fpa = F_pa$m,
                   HR_pa = F_pa$rate,
                   Fmsy = F_msy$m,
                   HR_msy = F_msy$rate,
                   Fp5 = F_p5$m,
                   HR_p5 = F_p5$rate,
                   HR_mgmt = 0.4))

  
  save(res_no_ass_error,res_ass_error,res_assimpl_error,old_res,file='02-haddock/99-docs/full_res.Rdata')
  save(res_agg,old_res_agg,res_hcr,ref_points,hcr_below_ref_p,file='02-haddock/99-docs/res_agg.Rdata')
  
  