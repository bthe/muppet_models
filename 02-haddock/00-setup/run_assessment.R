library(rmuppet)
library(tidyverse)

#old_rby <- 
#  read_csv('https://data.hafro.is/assmt/2018/haddock/summary.csv') %>% 
#  set_names(.,tolower(names(.))) %>% 
#  gather(type,val, -year)

theme_set(theme_bw())
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


## overall setup 
readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
  stringr::str_remove('../') %>% 
  readr::write_lines(paste(md,'params','icehad.dat.opt',sep='/'))

readr::read_lines(paste(md,'Files','autsurveypar.dat',sep='/')) %>% 
  rmuppet:::line_replace('0.392226 0.295314 0.180818 0.179292 0.251839 0.233800 0.383959 0.419390 0.407381',
                         '#pattern in CV with age. Common multiplier estimated') %>% 
  readr::write_lines(paste(md,'Files','autsurveypar.dat',sep='/'))

readr::read_lines(paste(md,'Files','marsurveypar.dat',sep='/')) %>% 
  rmuppet:::line_replace('0.335784 0.337252 0.273987 0.264007 0.256767 0.345298 0.327136 0.344655 0.345801 0.307099',
                         '#pattern in CV with age. Common multiplier estimated') %>% 
  readr::write_lines(paste(md,'Files','marsurveypar.dat',sep='/'))


## output control

readr::read_lines(paste(md,'Files','outputparameters.dat',sep='/')) %>% 
  rmuppet:::line_replace('out/', '# Prefix none') %>% 
  rmuppet:::line_replace('.logit_length', '# Postfix none') %>% 
  readr::write_lines(paste(md,'Files','out_logit_length.dat',sep='/'))

readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
  rmuppet:::line_replace('Files/out_logit_length.dat', '# Fbar range output biomasses etc') %>% 
  readr::write_lines(paste(md,'params','icehad.dat.opt',sep='/'))

## Statistical catch-at-age models:
## Size based selection (logit on weight-at-age) tuned with both surveys


callMuppet(ind='params/icehad.dat.opt','nox',run_dir = md)
fs::file_move(paste(md,'muppet.std',sep='/'),
              paste0(out_dir,'/','params.','logit_length'))
#clean_up('logit_length')

## Size based selection (logit on weight-at-age) tuned with the spring survey

readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
  rmuppet:::line_replace('Files/lik.smb', '# Weight of likelihood components etc.') %>% 
  rmuppet:::line_replace('Files/out_smb.dat', '# Fbar range output biomasses etc') %>% 
  readr::write_lines(paste(md, 'params','icehad.dat.smb',sep='/'))

readr::read_lines(paste(md,'Files','outputparameters.dat',sep='/')) %>% 
  rmuppet:::line_replace('out/', '# Prefix none') %>% 
  rmuppet:::line_replace('.smb', '# Postfix none') %>% 
  readr::write_lines(paste(md,'Files','out_smb.dat',sep='/'))

readr::read_lines(paste(md,'Files','likelihoodparameters.dat',sep='/')) %>% 
  rmuppet:::line_replace('1\t0.000001', '# Weight on surveys.') %>% 
  readr::write_lines(paste(md, 'Files','lik.smb',sep='/'))

callMuppet(ind='params/icehad.dat.smb','nox',run_dir = md)
fs::file_move(paste(md,'muppet.std',sep='/'),
              paste0(out_dir,'/','params.','smb'))

#clean_up('smb')

## Size based selection (logit on weight-at-age) tuned with the spring survey

readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
  rmuppet:::line_replace('Files/lik.smh', '# Weight of likelihood components etc.') %>% 
  rmuppet:::line_replace('Files/out_smh.dat', '# Fbar range output biomasses etc') %>% 
  readr::write_lines(paste(md, 'params','icehad.dat.smh',sep='/'))

readr::read_lines(paste(md,'Files','outputparameters.dat',sep='/')) %>% 
  rmuppet:::line_replace('out/', '# Prefix none') %>% 
  rmuppet:::line_replace('.smh', '# Postfix none') %>% 
  readr::write_lines(paste(md,'Files','out_smh.dat',sep='/'))

readr::read_lines(paste(md,'Files','likelihoodparameters.dat',sep='/')) %>% 
  rmuppet:::line_replace('0.000001\t1', '# Weight on surveys.') %>% 
  readr::write_lines(paste(md, 'Files','lik.smh',sep='/'))


callMuppet(ind='params/icehad.dat.smh','nox',run_dir = md)
fs::file_move(paste(md,'muppet.std',sep='/'),
              paste0(out_dir,'/','params.','smh'))
#clean_up('smh')

## VPA/backwards calculation
## set up backwards calculation model (i.e. the adapt-type model)
readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
  rmuppet:::line_replace('Files/out_vpa.dat', '# Fbar range output biomasses etc') %>% 
  rmuppet:::line_replace(1, '# 0 Forward, 1 Backward vpa') %>% 
  readr::write_lines(paste(md, 'params','icehad.dat.vpa',sep='/'))

readr::read_lines(paste(md,'Files','outputparameters.dat',sep='/')) %>% 
  rmuppet:::line_replace('out/', '# Prefix none') %>% 
  rmuppet:::line_replace('.vpa', '# Postfix none') %>% 
  readr::write_lines(paste(md,'Files','out_vpa.dat',sep='/'))


callMuppet(ind='params/icehad.dat.vpa','nox',run_dir = md)
fs::file_move(paste(md,'muppet.std',sep='/'),
              paste0(out_dir,'/','params.','vpa'))
#clean_up('vpa')



callMuppet(ind='params/icehad.dat.opt','nox',run_dir = md,
           mcmc=100000,mcsave=200,
           'mcscale')
# muppet -ind  icehad.dat.prog -mceval

readr::read_lines(paste(md,'params','icehad.dat.opt',sep='/')) %>% 
  rmuppet:::line_replace(60, '# Number of simulations years') %>% 
  rmuppet:::line_replace('Files/hadprognosis.dat.biorule.adviceyear', '#Prognosis file') %>% 
  rmuppet:::line_replace('Files/out_mcmc.dat', '# Fbar range output biomasses etc') %>% 
  readr::write_lines(paste(md, 'params','icehad.prognosis',sep='/'))

readr::read_lines(paste(md,'Files','out_logit_length.dat',sep='/')) %>% 
  rmuppet:::line_replace(1, '# 6. Some estimated parametes for example ssb-rec') %>% 
  rmuppet:::line_replace('out/', '# Prefix none') %>% 
  rmuppet:::line_replace('.logit_length', '# Postfix none') %>% 
  readr::write_lines(paste(md, 'Files','out_mcmc.dat',sep='/'))

callMuppet(ind='params/icehad.prognosis','mceval',run_dir = md)

