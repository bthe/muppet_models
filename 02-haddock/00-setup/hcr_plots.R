ssb_rec_plot <- 
  fit$rby %>% 
  filter(year < tyr,model == 'logit_length') %>% 
  group_by(model) %>% 
  mutate(ssbl = lead(Spawningstock) ) %>% 
  ggplot(aes(ssbl,Recruitment)) + geom_text(aes(label=year+1)) + 
  expand_limits(x=0, y=0) 

b_ref <- 
  fit$rby %>% 
  filter(model == 'logit_length') %>% 
  summarise(b_loss=min(Spawningstock),
            b_lim = b_loss,
            b_pa = b_lim*exp(1.645*0.17)) 


## calulate probabilities 
hcr_below_ref_p <-
  res_hcr %>% 
  filter(variable == 'Spawningstock',year>tyr) %>% 
  group_by(rate,year) %>% 
  summarise(p = mean(value < b_ref$b_lim),
            p_pa = mean(value < b_ref$b_pa)) %>% 
  group_by(rate) %>% 
  summarise(p1 = mean(p),
            p3 = max(p),
            p1_pa = mean(p_pa),
            p3_pa = max(p_pa)) %>% 
  left_join(res_hcr %>% 
              filter(variable == 'Spawningstock',year>tyr) %>% 
              group_by(rate,iter) %>% 
              summarise(below = max(value < b_ref$b_lim),
                        below_pa = max(value < b_ref$b_pa)) %>% 
              group_by(rate) %>% 
              summarise(p2 = mean(below),
                        p2_pa = mean(below_pa))) %>%  
  mutate(period = 'Year > 2018') %>% 
  bind_rows(res_hcr %>% 
              filter(variable == 'Spawningstock',year %in% tyr:(tyr+5)) %>% 
              group_by(rate,year) %>% 
              summarise(p = mean(value < b_ref$b_lim),
                        p_pa = mean(value < b_ref$b_pa)) %>% 
              group_by(rate) %>% 
              summarise(p1 = mean(p),
                        p3 = max(p),
                        p1_pa = mean(p_pa),
                        p3_pa = max(p_pa)) %>% 
              left_join(res_hcr %>% 
                          filter(variable == 'Spawningstock', year %in% tyr:(tyr+5)) %>% 
                          group_by(rate,iter) %>% 
                          summarise(below = max(value < b_ref$b_lim),
                                    below_pa = max(value < b_ref$b_pa)) %>% 
                          group_by(rate) %>% 
                          summarise(p2 = mean(below),
                                    p2_pa = mean(below_pa))) %>% 
              mutate(period = '2023 > Year > 2018'))


yield_by_rate <- 
  res_agg %>% 
  filter(variable == 'CalcCatchIn1000tons',
         year > 2030) %>%
  group_by(rate) %>% 
  summarise(c = median(m)) 
#yield_by_rate %>% ungroup() %>% filter(c > 0.95*max(c))


hcr_rate <- 0.4

F_lim <- 
  res_no_trigger_agg %>% 
  filter(year==2060,variable=='RefF') %>% 
  semi_join(res_no_trigger_agg %>%
              ungroup() %>% 
              filter(variable == 'Spawningstock',year == 2060, m>49) %>% 
              tail(1) %>% 
              select(rate))

F_pa <- 
  res_no_trigger_agg %>% 
  filter(year==2060,variable=='RefF') %>% 
  semi_join(res_no_trigger_agg %>%
              ungroup() %>% 
              filter(variable == 'RefF',year == 2060, m<F_lim$m/1.4) %>% 
              tail(1) %>% 
              select(rate))

F_p5 <- 
  res_no_trigger_agg %>% 
  filter(year==2060,variable=='RefF') %>% 
  semi_join(hcr_below_ref_p %>% 
              #group_by(period) %>% 
              filter(period == 'Year > 2018',p3<0.05) %>% 
              filter(p3 == max(p3)))

  

F_msy <- 
  res_agg %>% 
  filter(year==2060,variable == 'RefF') %>% 
  semi_join(yield_by_rate %>% 
              ungroup() %>% 
              filter(c==max(c)) %>% 
              mutate(rate = min(rate,F_p5$rate)))


ref_points <- 
  b_ref %>% 
  bind_cols(data_frame(Flim = F_lim$m, 
                       HR_lim = F_lim$rate,
                       Fpa = F_pa$m,
                       HR_pa = F_pa$rate,
                       Fmsy = F_msy$m,
                       HR_msy = F_msy$rate,
                       Fp5 = F_p5$m,
                       HR_p5 = F_p5$rate,
                       HR_mgmt = 0.4))


res_agg %>% 
  filter(variable == 'CalcCatchIn1000tons',
         year == 2060) %>% 
  ggplot(aes(rate,m)) + 
  geom_ribbon(aes(ymin=l,ymax=u),fill='gold') +
  geom_ribbon(aes(ymin=ll,ymax=uu),fill='gray',alpha=0.3) +
  geom_line() + 
  geom_vline(xintercept = ref_points$HR_mgmt) +
  geom_vline(xintercept = ref_points$HR_msy) +
  geom_vline(xintercept = ref_points$HR_pa,lty=2,col = 'red') + 
  geom_vline(xintercept = ref_points$HR_lim,col = 'red') + 
  labs(title='Afli',x='Veiðihlutfall',y='') +


res_agg %>% 
  filter(variable == 'Spawningstock',
         year == 2060) %>% 
  ggplot(aes(rate,m)) + 
  geom_ribbon(aes(ymin=l,ymax=u),fill='gold') +
  geom_ribbon(aes(ymin=ll,ymax=uu),fill='gray',alpha=0.3) +
  geom_line() + 
  geom_hline(yintercept = ref_points$b_lim,col = 'red') + 
  geom_hline(yintercept = ref_points$b_pa,lty=2, col = 'red') + 
  geom_vline(xintercept = ref_points$HR_mgmt) +
  geom_vline(xintercept = ref_points$HR_msy) +
  geom_vline(xintercept = ref_points$HR_pa,lty=2,col = 'red') + 
  geom_vline(xintercept = ref_points$HR_lim, col='red') + 
  labs(title='Hrygningarstofn',x='Veiðihlutfall',y='')




save(ref_points, hcr_below_ref_p, file = '02-haddock/99-docs/refpoint.Rdata')
