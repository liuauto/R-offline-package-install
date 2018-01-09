getPackages <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(),
                                which=c("Depends", "Imports", "LinkingTo"), recursive=TRUE))
  #packages <- union(packs, packages)
  packages
}

dep.lookup.single <- function(pkg){
  packages <- getPackages(pkg)
  df <- data.frame()
  total.pkg <- length(packages)
  for(i in 1:total.pkg){
    df[i,1] <- pkg
    df[i,2] <- packages[i]
  }
  colnames(df) <- c('pkg', 'dep')
  return(df)
}

dep.lookup.all <- function(pkgs){
  
  records <- list()
  total <- length(pkgs)
  for( i in 1:total){
    pkg <- pkgs[i]
    df <- dep.lookup.single(pkg)  
    records[[i]] <- df
  }
  
  lookup.all <- do.call('rbind', records)
}



get.dep <- function(pkg, lookup){
  
  return(lookup[lookup$pkg == pkg, 'dep'])
  
}



pkg.exists <- function(pkg, dir){
  total.files <- list.files(dir)
  pattern <- paste0('^', pkg, '_')
  for(file in total.files){
    if(grepl(pattern, file) == TRUE)
      return(TRUE)
  }
  return(FALSE)
}

pkg.file <- function(pkg, dir){
  total.files <- list.files(dir)
  pattern <- paste0('^', pkg, '_')
  for(file in total.files){
    if(grepl(pattern, file) == TRUE)
      return(file)
  }
  return('')
}


download.all <- function(pkgs, path){
  for(pkg in pkgs){
    if( (! pkg %in% c('tools','utils', 'stats', 'methods', 'grDevices', 'graphics')) &
         pkg.exists(pkg, dir = path) == FALSE){
      download.packages(pkg, destdir = path)
    }
    deps <- getPackages(pkg)
    for(dep in deps){
      if((! dep %in% c('tools','utils', 'stats', 'methods', 'grDevices', 'graphics')) &
         pkg.exists(dep, dir = path) == FALSE){
        download.packages(dep, destdir = path)
        download.all(dep, path)
      }
    }
  }
}

install.check <- function(dep, path){
	if(is.na(dep)) return(FALSE)
        installed <- require(dep, character.only=TRUE)
	if(installed) print(paste0('dep package installed:', dep))
	pkg.file.exists <- pkg.exists(dep,dir=path)
	if(!pkg.file.exists & !installed) 
	stop(paste0('needed package not installed but no pkg file:', dep))
	
	pass <- pkg.file.exists & !installed

	return(pass) 
}
install.all <-  function(pkgs, path, lookup){
  for(pkg in pkgs){
   
    deps <- get.dep(pkg, lookup)
    if(!is.na(deps)){
      for(dep in deps){
        
       if(install.check(dep, path)){
          install.all(dep, path, lookup)
          # file <- pkg.file(dep, path)
          # install.packages(file, repos = NULL, destdir = path)
          #print(paste0('installing:', file))
        }
      }  
    }
   if(install.check(pkg,path) ){
      file <- pkg.file(pkg, path)
      install.packages(file, repos = NULL, destdir = path)
      #print(paste0('installing:', file))
    }
  }
}



packagesExtra <- function(pkg.retained){
  # create a list of all installed packages
  ip <- as.data.frame(installed.packages())
  head(ip)
  # if you use MRO, make sure that no packages in this library will be removed
  ip <- subset(ip, !grepl("ORE", ip$LibPath))
  # we don't want to remove base or recommended packages either\
  ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
  # determine the library where the packages are installed
  path.lib <- unique(ip$LibPath)
  # create a vector with all the names of the packages you want to remove
  pkgs.to.remove <- ip[,1]
  
  nlp.pkgs <- c('slam', 'NLP', 'tm')
  pkgs.to.remove <- pkgs.to.remove[!pkgs.to.remove %in% nlp.pkgs & !pkgs.to.remove %in% pkg.retained & !grepl('ORE*', pkgs.to.remove)]
  as.character(pkgs.to.remove)
}
