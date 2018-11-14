library(tidyverse)

tmp_func <- function(x){
  val = gsub('.csv','',x)
  read_csv(paste0('/u2/reikn/Tac/2018/02-ysa/tables/',x)) %>% 
    gather('age',value=value,-year,convert = TRUE) %>% 
    mutate(data=val)
}
## year age cno cwt swt mat ssbwt
dat <- 
  tmp_func('catage.csv') %>% 
  bind_rows(tmp_func("wcatch.csv")) %>% 
  bind_rows(tmp_func("maturity.csv")) %>% 
  bind_rows(tmp_func("smb.csv")) %>%
  bind_rows(tmp_func("smh.csv")) %>% 
  bind_rows(tmp_func("wstock.csv")) %>% 
  spread(data,value,fill = -1) 

rby <- 
  read_csv('/u2/reikn/Tac/2018/02-ysa/tables/summary.csv') %>% 
  select_all(tolower) %>% 
  filter(year<2018)

## write catchandstockdata
write('# year age cno cwt swt mat ssbwt',
      file = 'HCR/Haddock/Files/catchandstockdata.dat')
dat %>% 
  select(year,age,cno=catage,cwt=wcatch,swt=wstock,mat=maturity,ssbwt=wstock) %>% 
  write_delim('HCR/Haddock/Files/catchandstockdata.dat',
              col_names = FALSE, 
              append = TRUE)
  
## write totcatch
rby %>% 
  select(year,landings) %>% 
  write_delim('HCR/Haddock/Files/totcatch.txt')

## spring survey data
dat %>% 
  select(year,age,smb) %>% 
  filter(year>1984,age %in% 1:11) %>% 
  write_delim('HCR/Haddock/Files/smbdata.dat')

## spring survey data
dat %>% 
  select(year,age,smh) %>% 
  filter(year>1995,age %in% 1:11) %>% 
  write_delim('HCR/Haddock/Files/smhdata.dat')


