
#setwd(path)

source('functions.R')

lookup <- read.csv('lookup.csv')
pkg.list <- c("text2vec", 
              "dplyr", 
              'FeatureHashing', 
              'klaR', 
              'xgboost', 
              'glmnet', 
              'ggplot2',
              'ROCR')
install.all(pkg.list, path, lookup)



