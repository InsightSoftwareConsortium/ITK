mutatemean <- function(mean, sigma, random, extent)
  {
    offsets <- sqrt(sigma) * (extent * random) ;
    newmean <- mean + offsets ;
    newmean ;
  }

mvmutatemean <- function(mean, sigma, random, extent)
  {
    nmeasurements = length(mean) ;
    vars <- diag(sigma) ; 
    stddev <- sqrt(vars) ;
    offsets <- stddev * (extent * random) ;
    newmean <- mean + offsets ;
    newmean ;
  }


mvgenparam <- function(classStatistics, outputFile, noCases, noMeasurements, mutationScale)
  {
    noCases <- 100 ;
    nclasses <- nrow(classStatistics) ;
    
    tempmean <- array(dim=c(noMeasurements)) ;
    tempsigma <- matrix(nrow=noMeasurements, ncol=noMeasurements) ;
    
    means <- list() ;
    sigmas <- list() ;
    
    for (i in 1:nclasses)
      {
        for (j in 1:noMeasurements)
          {
            tempmean[j] <- classStatistics[i, 2+j] ;
            for (k in 1:noMeasurements)
              {
                tempsigma[j,k] = classStatistics[i, 2 + noMeasurements + j*k] ;
              }
          }
        means[[i]] <- tempmean ;
        sigmas[[i]] <- tempsigma ;
      }
    
    unifrans <- array(dim=c(noCases, nclasses, noMeasurements)) ;
    for (i in 1:noCases)
      {
        for (j in 1:nclasses)
          {
            unifrans[i,j,] <- runif(n=noMeasurements, min=-1, max=1) ;
          }
      }

    if ( file.access(outputFile, mode=0) == 0 )
      {
        unlink(outputFile) ;
      }
    
    header = paste("\"run\"", "\"class\"") ;
    for (i in 1:noMeasurements)
      {
        header = paste(header, " \"mean.", i, "\"", sep="") ;
      }
    
    for (i in 1:(noMeasurements * noMeasurements) )
      {
        header = paste(header, " \"sigma.", i, "\"", sep="") ;
      }
    
    write(header, file=outputFile, append=TRUE) ;
    
    for (i in 1:noCases)
      {
        for (j in 1:nclasses)
          {
            record <- paste(i) ;
            record <- paste(record, j) ;
            newmean <- 
              mvmutatemean(means[[j]], sigmas[[j]], unifrans[i,j,], mutationScale) ;
            for (k in 1:length(newmean))
              {
                record <- paste(record, newmean[k]) ;
              }
            
            for (k in 1:length(sigmas[[j]]))
              {
                record <- paste(record, sigmas[[j]][k]) ;
              }
            
            write(record, file=outputFile, append=TRUE) ;
          }
      }
  }

genparam <- function(classStatistics, outputFile, noCases, noMeasurements, mutationScale)
  {
    noClasses <- nrow(classStatistics) ;
    
    means <- array(dim=c(noClasses)) ;
    sigmas <- array(dim=c(noClasses)) ;
    proportions <- array(dim=c(noClasses)) ;
    
    for (i in 1:noClasses)
      {
        means[i] <- classStatistics[i, 3] ;
        sigmas[i] <- classStatistics[i, 4] ;
      }
    
    unifrans <- array(dim=c(noCases, noClasses)) ;
    for (i in 1:noCases)
      {
        for (j in 1:noClasses)
          {
            unifrans[i,j] <- runif(1, min=-1, max=1) ;
          }
      }

    if ( file.access(outputFile, mode=0) == 0 )
      {
        unlink(outputFile) ;
      }
    
    header = paste("\"run\" \"class\" \"mean.1\" \"sigma.1\"", sep="") ;
    
    write(header, file=outputFile, append=TRUE) ;
    
    for (i in 1:noCases)
      {
        for (j in 1:noClasses)
          {
            record <- paste(i) ;
            record <- paste(record, classStatistics$class[j]) ;
            newmean <- 
              mutatemean(means[j], sigmas[j], unifrans[i,j], mutationScale) ;
            record <- paste(record, newmean) ;
            record <- paste(record, sigmas[j]) ;
            write(record, file=outputFile, append=TRUE) ;
          }
      }
  }


countmeasurements <- function(classStatistics)
  {
    count = 0 ;
    cols = strsplit(colnames(classStatistics), c("\\.")) ;
    for ( i in 1:length(colnames(classStatistics)) )
      {
        if (cols[[i]][1] == "mean")
          { 
            count = count + 1 ;
          }
      }
    count ;
  }

genparamfile <- function(inputFile, outputFile, noCases, mutationScale)
  {
    classStatTable <- read.table(inputFile, header=TRUE) ;
    
    noMeasurements = countmeasurements(classStatTable) ;
                                        #generates parameters based on the class statisics
                                        # and the parameter mutation scales.
    if ( file.access(outputFile, mode=0) == -1 )
      {
        if ( noMeasurements > 1 )
          {
            mvgenparam(classStatTable, outputFile,
                     noCases, noMeasurements, mutationScale) ;
          }
        else
          {
            genparam(classStatTable, outputFile,
                     noCases, noMeasurements, mutationScale) ;
          }
      }
  }

