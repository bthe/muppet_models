library(tidyverse)
library(icesSAG)
options(icesSAG.use_token = FALSE)
load('02-haddock/99-docs/icesSS.RData')

tmp <- getListStocks(0) 
retro <- 
  tmp %>% 
  filter(StockKeyLabel %in% c('had-iceg','had.27.5a')) %>%
  select(AssessmentKey) %>% 
  split(.$AssessmentKey) %>% 
  map(~getSummaryTable(.$AssessmentKey)) %>% 
  map(~bind_rows(.) %>% select(-c(stockSizeUnits,fishingPressureUnits))) %>% 
  bind_rows()


retro %>% 
  select(year=Year,rec=recruitment,ssb=SSB,landings = landings, F, assyear = AssessmentYear) %>% 
  bind_rows(icesSS %>% filter(stock == 'had-iceg') %>% select(year,rec = r, ssb,landings=yield,F=f, assyear = assYear)) %>% 
  bind_rows(icesSS2012 %>% mutate(assyear = 2012) %>% filter(stock == 'had-iceg') %>% select(year,rec = r, ssb,landings=yield,F=f)) %>% 
  left_join(retro %>% 
              filter(AssessmentYear == 2018) %>% 
              select(year=Year,rec_true=recruitment,ssb_true=SSB, F_true=F)) %>% 
  as_data_frame() -> hist_retro 

hist_retro %>% 
  filter(year == assyear-1) %>% write_csv('02-haddock/99-docs/historical_retro.csv')
ggplot(aes(year,ssb)) + geom_line(lty=2) + geom_line(aes(y=ssb_true))


save(hist_retro, file='02-haddock/99-docs/hist_retro.Rdata')

