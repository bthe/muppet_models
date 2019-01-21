retro_dir <- paste(md,'retro',sep='/')
fs::dir_create(retro_dir)
num_peels <- 10

for(retro in 1:num_peels){

  readr::read_lines(paste(md,'Files','outputparameters.dat',sep='/')) %>% 
    rmuppet:::line_replace('retro/', '# Prefix none') %>% 
    rmuppet:::line_replace(sprintf('.r_%s',retro), '# Postfix none') %>% 
    readr::write_lines(paste(md,'Files',sprintf('out_r_%s.dat',retro),sep='/'))
  
  readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
    rmuppet:::line_replace(sprintf('Files/out_r_%s.dat',retro), '# Fbar range output biomasses etc') %>% 
    rmuppet:::line_replace(tyr-1-retro,'# Last opt year') %>% 
    rmuppet:::line_replace(tyr-1-retro,'# Last data year') %>% 
    readr::write_lines(paste(md,'params',sprintf('icehad.dat.r_%s',retro),sep='/'))
  
  
  ## Statistical catch-at-age models:
  ## Size based selection (logit on weight-at-age) tuned with both surveys
  
  
  callMuppet(ind=sprintf('params/icehad.dat.r_%s',retro),'nox',run_dir = md)
  
}

retro.fit <- list(rby = read_output('resultsbyyear',retro_dir),
                  rbyage = read_output('resultsbyyearandage',retro_dir) %>% 
                    mutate(CalcCno = ifelse(model == 'vpa',NA,CalcCno)),
                  rbage = read_output('resultsbyage',retro_dir))


retro.fit$rby %>% 
  mutate(ass.year = 1 + tyr - as.numeric(gsub('r_','',model))) %>% 
  #filter(model %in% c('logit_length','vpa'),year <= tyr) %>% 
  filter(year <= ass.year) %>% 
  #mutate(`B45cm+`= lag(RefBio1,1)) %>% 
  select(model,year,SSB=Spawningstock,CbioR) %>% #,`B45cm+`) %>% 
  gather(type,val,-c(year,model)) %>% 
  ggplot() + 
  #geom_ribbon(aes(year,ymin=l,ymax=u,fill = filename),
  #            data=mcmc_summary %>% filter(filename != 'n1st'),
  #            alpha = 0.5) +
  geom_line(aes(year,val,col = type,lty=model)) + 
#  geom_line(aes(year,val,col = type),lty=2,
#            data = old_rby %>% 
#              filter(type %in% c('ssb','refbio')) %>% 
#              mutate(type = ifelse(type == 'refbio','CbioR','SSB'))) +
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
