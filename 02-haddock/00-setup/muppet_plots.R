
## Diganostic plots

## model diagnostics
model_bubble_plot <- 
  rbyage %>% 
  filter(age < 11, year < tyr) %>% 
  mutate(#`Catch in numbers` = ifelse(ObsCno==-1,NA,ObsCno)-CalcCno,
    `Spring survey in numbers` = ObsSurveyNr1 - CalcSurveyNr1,
    `Autumn survey in numbers` = ObsSurveyNr2 - CalcSurveyNr2,
    ` Catch in numbers` = ObsCno - CalcCno) %>% 
  select(model,year,age, contains('in numbers')) %>% 
  gather(source,delta,-c(year,age,model)) %>%
  group_by(source,age) %>% 
  mutate(sign = ifelse(delta>0,1,-1),
         delta = abs(delta)/sd(delta,na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(year,age,size=delta,col = as.factor(sign))) + 
  geom_abline(slope = 1, intercept=-1980 + -100:100,col='gray',lty=2) + 
  geom_point(shape=1) + 
  facet_grid(model~source) +
  scale_color_manual(values = c('red','blue'))+
  scale_size_area(max_size = 10) + 
  scale_y_continuous(breaks = 2*1:5,minor_breaks = 1:10) +
  theme_bw() +
  theme(legend.position = 'none',
        strip.background = element_blank()) + 
  labs(y='Age',x='Year')


## model output
mcmc_summary <- 
  mcmc_results %>% 
  filter(filename %in% c('ssb','hcrrefbio','n1st'),year <= tyr) %>% 
  group_by(year,filename) %>% 
  summarise(val=median(value),
            u=quantile(value,0.95),
            l=quantile(value,0.05)) 

model_bio_plot <- 
  rby %>% 
  #filter(model %in% c('logit_length','vpa'),year <= tyr) %>% 
  filter(model %in% c('logit_length'),year <= tyr) %>% 
  #mutate(`B45cm+`= lag(RefBio1,1)) %>% 
  select(model,year,SSB=Spawningstock,CbioR) %>% #,`B45cm+`) %>% 
  gather(type,val,-c(year,model)) %>% 
  ggplot() + 
  geom_ribbon(aes(year,ymin=l,ymax=u,fill = filename),
              data=mcmc_summary %>% filter(filename != 'n1st'),
              alpha = 0.5) +
  geom_line(aes(year,val,col = type)) + 
  geom_line(aes(year,val,col = type),lty=2,
            data = old_rby %>% 
              filter(type %in% c('ssb','refbio')) %>% 
              mutate(type = ifelse(type == 'refbio','CbioR','SSB'))) +
  geom_vline(xintercept = tyr,lty=2,col='gray') +
  geom_hline(yintercept = 45) + 
  geom_hline(yintercept = 59,lty=2) + 
  geom_label(data=data_frame(x=tyr + 2,y=45,label='Blim = 49 kt'),aes(x,y,label=label),col='black') +
  geom_label(data=data_frame(x=tyr + 2,y=59,label='Bpa = 59 kt'),aes(x,y,label=label),col='black') +
  theme_light() + 
  labs(x='Year',y='SSB/B45cm+',col='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.2,0.2))

model_catch_plot <- 
  rby %>% 
  select(model,
         year,
         obscatch = CatchIn1000tons,
         calccatch=CalcCatchIn1000tons) %>%
  gather(source,val,-c(model,year)) %>% 
  #mutate(val = ifelse(year < tyr, obscatch, calccatch)) %>% 
  ggplot() + geom_line(aes(year,val,col=source,lty=model)) + 
  geom_vline(xintercept = tyr-1,lty=2,col='gray') + 
  theme_light() + 
  labs(x='Year',y='Catch (in kt)') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.1,0.1))

model_F_plot <- 
  rby %>% 
  mutate(HR = CatchIn1000tons/lag(RefBio1,1)) %>% 
  select(year, model, RefF,HR) %>% 
  gather(type,val,-c(year, model)) %>% 
  ggplot(aes(year,val,col=type,lty=model)) + geom_line() + 
  geom_vline(xintercept = tyr-1,lty=2,col='gray') + 
  theme_light() + 
  labs(x='',y='F4-7/HR',col='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.2,0.2))

model_rec_plot <- 
  rby %>% 
  ggplot(aes(year,Recruitment/1e3,lty=model)) + geom_line() + 
  geom_vline(xintercept = tyr+1,lty=2,col='gray') + 
  theme_light() + 
  labs(x='',y='Recruitment (in millions)',col='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.1,0.1))

ices_plot_1 <- 
  model_catch_plot + 
  model_rec_plot +  plot_layout(ncol=2)
ices_plot_2 <-   
  model_F_plot + 
  model_bio_plot + plot_layout(ncol = 2)

ssb_rec_plot <- 
  rby %>% 
  filter(year < tyr) %>% 
  group_by(model) %>% 
  mutate(ssbl = lead(Spawningstock) ) %>% 
  ggplot(aes(ssbl,Recruitment)) + geom_text(aes(label=year-1)) + 
  expand_limits(x=0, y=0) + 
  facet_wrap(~model)


mcmc_results %>% 
  filter(filename == 'parameter',
         !(variable %in% c('AbundanceMultiplier','Catchlogitage50','Catchlogitslope')),
         !(year %in% 5:6)) %>% 
  mutate(value = ifelse(variable == 'estSSBRecParameters',exp(value),value),
         variable = ifelse(variable == 'estSSBRecParameters',
                           forcats::fct_recode(as.character(year),
                                               Rmax="1",ssbbreak = "2",
                                               `Recruitment CV`='3',
                                               rho='4') %>% as.character(),
                           variable)) %>% 
  ggplot(aes(value)) + geom_histogram() + facet_wrap(~variable,scale='free')


mcmc_results %>% 
  filter(filename == 'parameter',
         !(variable %in% c('AbundanceMultiplier','Catchlogitage50','Catchlogitslope')),
         !(year %in% 5:6)) %>% 
  mutate(value = ifelse(variable == 'estSSBRecParameters',exp(value),value),
         variable = ifelse(variable == 'estSSBRecParameters',
                           forcats::fct_recode(as.character(year),
                                               Rmax="1",ssbbreak = "2",
                                               `Recruitment CV`='3',
                                               rho='4') %>% as.character(),
                           variable)) %>% 
  select(iter,variable, value) %>% 
  spread(variable,value) %>%
  select(-iter) %>% 
  GGally::ggpairs()
