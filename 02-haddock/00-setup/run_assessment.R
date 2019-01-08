library(rmuppet)
library(tidyverse)

tyr <- 2018
out_dir <- paste(md,'out',sep='/')
 
fs::dir_create(out_dir)


clean_up <- function(app='vpa'){
  fs::dir_ls(md,regexp = 'results') %>%
    stringr::str_remove(.,md) %>% 
    stringr::str_remove(.,'/') %>% 
    purrr::map(~fs::file_move(paste(md,.,sep='/'),
                              paste0(out_dir,'/',.,'.',app)))
  fs::file_move(paste(md,'muppet.std',sep='/'),
                paste0(out_dir,'/','params.',app)) %>% 
    invisible()
}

read_output <- function(file='resultsbyyear'){
  fs::dir_ls(out_dir,regexp = paste0(file,'\\.')) %>%
    purrr::set_names(.,stringr::str_extract(string = .,pattern = '\\..+$')) %>% 
    purrr::map(~readr::read_table2(.) %>% 
                 purrr::set_names(.,str_trim(names(.))) %>% 
                 dplyr::mutate_all(function(x) ifelse(x==-1,NA,x))) %>% 
    dplyr::bind_rows(.id='model') %>% 
    dplyr::mutate(model = stringr::str_remove(model,'\\.')) 
}

## Statistical catch-at-age models:
## Size based selection (logit on weight-at-age) tuned with both surveys

callMuppet(ind='params/icehad.dat.opt','nox',run_dir = md)
clean_up('logit_length')

## Size based selection (logit on weight-at-age) tuned with the spring survey

readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
  rmuppet:::line_replace('Files/lik.smb', '# Weight of likelihood components etc.') %>% 
  readr::write_lines(paste(md, 'params','icehad.dat.smb',sep='/'))

readr::read_lines(paste(md,'Files','likelihoodparameters.dat',sep='/')) %>% 
  rmuppet:::line_replace('1\t0.000001', '# Weight on surveys.') %>% 
  readr::write_lines(paste(md, 'Files','lik.smb',sep='/'))

callMuppet(ind='params/icehad.dat.opt.smb','nox',run_dir = md)
clean_up('smb')

## Size based selection (logit on weight-at-age) tuned with the spring survey

readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
  rmuppet:::line_replace('Files/lik.smb', '# Weight of likelihood components etc.') %>% 
  readr::write_lines(paste(md, 'params','icehad.dat.smh',sep='/'))

readr::read_lines(paste(md,'Files','likelihoodparameters.dat',sep='/')) %>% 
  rmuppet:::line_replace('0.000001\t1', '# Weight on surveys.') %>% 
  readr::write_lines(paste(md, 'Files','lik.smh',sep='/'))


callMuppet(ind='params/icehad.dat.opt.smh','nox',run_dir = md)
clean_up('smh')

## VPA/backwards calculation
## set up backwards calculation model (i.e. the adapt-type model)
readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
  rmuppet:::line_replace(1, '# 0 Forward, 1 Backward vpa') %>% 
  readr::write_lines(paste(md, 'params','icehad.dat.vpa',sep='/'))

callMuppet(ind='params/icehad.dat.vpa','nox',run_dir = md)
clean_up('vpa')



callMuppet(ind='params/icehad.dat.opt','nox',run_dir = md,
           mcmc=100000,mcsave=200,
           'mcscale')
# muppet -ind  icehad.dat.prog -mceval

readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
  rmuppet:::line_replace(60, '# Number of simulations years') %>% 
  rmuppet:::line_replace('Files/hadprognosis.dat.biorule.assyear', '#Prognosis file') %>% 
  readr::write_lines(paste(md, 'params','icehad.prognosis',sep='/'))

readr::read_lines(paste(md,'Files','outputparameters.dat',sep='/')) %>% 
  rmuppet:::line_replace(1, '# 6. Some estimated parametes for example ssb-rec') %>% 
  readr::write_lines(paste(md, 'Files','outputparameters.dat',sep='/'))

callMuppet(ind='params/icehad.prognosis','mceval',run_dir = md)

## read the output from the assessment

rby <- 
  read_output('resultsbyyear')
  
rbyage <- 
  read_output('resultsbyyearandage') %>% 
  mutate(CalcCno = ifelse(model == 'vpa',NA,CalcCno))

rbage <- 
  read_output('resultsbyage') 
  
params <- 
  read_output('params')

## read the uncertainty (mcmc) results
mcmc_results <- 
  fs::dir_ls(md,regexp = '.mcmc') %>%
  purrr::set_names(.,stringr::str_extract(.,'([a-z0-9]+).mcmc$') %>% 
                     stringr::str_remove('.mcmc')) %>% 
  as.list() %>% 
  purrr::list_modify(all=NULL) %>% 
  purrr::map(readr::read_delim, delim=' ') %>% 
  purrr::map(dplyr::mutate, iter=1:n()) %>% 
  purrr::map(tidyr::gather, key='variable',value='value',-iter) %>% 
  purrr::map(tidyr::separate, variable, c('variable','year'), convert = TRUE) %>% 
  dplyr::bind_rows(.id = 'filename')

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

mcmc_results %>% 
  filter(filename %in% c('ssb','hcrrefbio')) %>% 
  group_by(year,filename) %>% 
  summarise(m=median(value),
            u=quantile(value,0.95),
            l=quantile(value,0.05)) %>% 
  ggplot(aes(year,m,col=filename)) + 
  geom_ribbon(aes(ymax=u,ymin=l,fill=filename),alpha=0.1) +
  geom_line() + 
  theme_minimal()

model_bio_plot <- 
  rby %>% 
  filter(model != 'vpa',year <= tyr) %>% 
  #mutate(`B45cm+`= lag(RefBio1,1)) %>% 
  select(model,year,SSB=Spawningstock) %>% #,`B45cm+`) %>% 
  gather(type,val,-c(year,model)) %>% 
  ggplot() + geom_line(aes(year,val,col=type,lty=model)) + 
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
