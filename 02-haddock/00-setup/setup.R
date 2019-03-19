library(tidyverse)
library(rmuppet)
library(fs)

## setup the model 
md <- '02-haddock/02-updated'
dir.create(md,showWarnings = FALSE)

fs::dir_create(paste(md,'params',sep='/'))
fs::dir_ls('~/Documents/Aflareglur/Muppet_HCR/HCR/Had/StdSettings/',regexp = 'dat') %>% 
  fs::file_copy(paste(md,'params',sep='/'),overwrite = TRUE)

fs::dir_create(paste(md,'Files',sep='/'))
fs::dir_ls('~/Documents/Aflareglur/Muppet_HCR/HCR/Had/Files/',regexp = 'dat') %>% 
  fs::file_copy(paste(md,'Files',sep='/'),overwrite = TRUE)

source('02-haddock/00-setup/data.R')
