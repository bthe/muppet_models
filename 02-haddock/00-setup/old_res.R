theme_set(theme_bw())
old_res <- 
  fs::dir_ls('/u2/reikn/hoski/Haddock2013/msyresult_recrcorr10.25_asscv1_0.22_asscorr1_0.5_paratio1_1.45') %>% 
  purrr::set_names(.,stringr::str_extract(.,'([a-z0-9]+).mcmc(1$|0.[0-9]+)$') %>% 
                     stringr::str_replace('.mcmc','_')) %>%  
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


old_res_agg <- 
  old_res %>% 
  group_by(variable,year,rate) %>% 
  summarise(m = median(value,na.rm = TRUE),
            u = quantile(value,0.95,na.rm = TRUE),
            uu = quantile(value,0.75,na.rm = TRUE),
            ll = quantile(value,0.25,na.rm = TRUE),
            l  = quantile(value,0.05,na.rm = TRUE),
            r1 = value[100],
            r2 = value[200],
            r3 = value[300]) 
old_res <- list(res=old_res,res_agg = old_res_agg)


old_res %>% 
  filter(filename =='catch',rate==0.4,year<tyr) %>% 
  group_by(year) %>% 
  summarise(m=median(value),u=quantile(value,0.95),l=quantile(value,0.05)) %>% 
  ggplot(aes(year,m)) + 
  geom_ribbon(aes(ymin=l,ymax=u),fill='gold') + 
  geom_line() +
  geom_line(data=fit$rby%>% filter(model == 'logit_length'),aes(year,CatchIn1000tons),col='red') +
  geom_vline(xintercept = 2012) +
  expand_limits(y=0) + 
  labs(x = '',y='Catch') + 



old_res %>% 
  filter(filename =='ssb',rate==0.4,year<tyr) %>% 
  group_by(year) %>% 
  summarise(m=median(value),u=quantile(value,0.95),l=quantile(value,0.05)) %>% 
  ggplot(aes(year,m)) + 
  geom_ribbon(aes(ymin=l,ymax=u),fill='gold') + 
  geom_line() +
  geom_line(data=fit$rby %>% filter(model == 'logit_length',year < tyr),aes(year,Spawningstock),col='red') +
  geom_vline(xintercept = 2012) +
  expand_limits(y=0) +
  labs(x = '',y='SSB') + 

old_res %>% 
  filter(filename =='f',rate==0.4,year<tyr) %>% 
  group_by(year) %>% 
  summarise(m=median(value),u=quantile(value,0.95),l=quantile(value,0.05)) %>% 
  ggplot(aes(year,m)) + 
  geom_ribbon(aes(ymin=l,ymax=u),fill='gold') + 
  geom_line() +
  geom_line(data=fit$rby %>% filter(model == 'logit_length',year < tyr),aes(year,RefF),col='red') +
  geom_vline(xintercept = 2012) +
  expand_limits(y=0) +
labs(x = '',y='F') +

old_res %>% 
  filter(filename =='n3',rate==0.4,year<tyr) %>% 
  group_by(year) %>% 
  summarise(m=median(value,na.rm = TRUE),u=quantile(value,0.95,na.rm = TRUE),l=quantile(value,0.05,na.rm = TRUE)) %>% 
  ggplot(aes(year,m)) + 
  geom_ribbon(aes(ymin=l,ymax=u),fill='gold') + 
  geom_line() +
  geom_line(data=fit$rby %>% filter(model == 'logit_length',year < tyr),aes(year,N3),col='red') +
  geom_vline(xintercept = 2012) +
  expand_limits(y=0)  +
  
  labs(x = '',y='Rec') 




  read_csv('../historical_retro.csv') %>% 
  ggplot(aes(year,ssb)) + 
  geom_line(lty=2) + 
  geom_line(aes(y=ssb_true))+
  expand_limits(y=0)+
  theme_bw()

  
