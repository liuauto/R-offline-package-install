#setwd(path)
source('functions.R')

pkg.list <- c("text2vec", 
              "dplyr", 
              'FeatureHashing', 
              'klaR', 
              'xgboost', 
              'glmnet', 
              'ggplot2',
              'ROCR')
packages <- getPackages(pkg.list)
packages <- union(pkg.list, packages)
lookup <- dep.lookup.all(packages)
write.csv(lookup, 'lookup.csv')
# remove the packages

