imagePath <- "/share/work/data/brainweb" ;
binPath <- "/share/bin-linux/insight/bin" ;
validationRoot <- "/share/work/src/Insight/Validation/StatisticalClustering" ;
scriptPath <- getwd() ;
iteration <- 4000 ;
parametersMutationScales <- c(1, 2) ;
noiseLevels = c(0, 3, 9) ; 
imageSets <- list() ; 
imageSets[[1]] <- c("brainweb.T1.1mm.0.0.mhd",
                    "brainweb.PD.1mm.0.0.mhd") ;
imageSets[[2]] <- c("brainweb.T1.1mm.3.0.mhd",
                    "brainweb.PD.1mm.3.0.mhd") ;
imageSets[[3]] <- c("brainweb.T1.1mm.9.0.mhd",
                    "brainweb.PD.1mm.9.0.mhd") ;
classMask <- "phantom_discrete.mhd" ;

kdTreeBucketSize = 100 ;

for ( i in 1:length(noiseLevels) )
  {
    for ( j in 1:length(parametersMutationScales) )
      {
                                        # prepare common command arguments for EM & GOF
        imageFiles <- paste(imagePath, "/", imageSets[[i]], sep="") ;
        paramFile <- paste(validationRoot, "/Inputs/BrainWeb/params.",
                       parametersMutationScales[j], "ext.dat", sep="") ;
        maskFile <- paste(imagePath, "/", classMask, sep="") ;

        options.files <- "--images" ;

        for ( k in 1:length(imageFiles) )
          {
            options.files <- paste(options.files, imageFiles[k]) ;
          }

        options.maskfile <- paste("--mask", maskFile) ;
        
        options.parameters <- paste("--parameters", paramFile) ;

        options.iteration <- paste("--iteration", iteration) ;

                                        # prepare the command arguments for EM
        resultfile <- paste(validationRoot, "/Results/BrainWeb.EM.", noiseLevels[i], "pn.",
                         parametersMutationScales[j], "ext.dat", sep="") ;

        options.resultfile <- paste("--result", resultfile) ;
        
        options <- paste(options.files, options.maskfile,
                         options.parameters,
                         options.resultfile, 
                         options.iteration) ;
        
                                        #run EM experiment
        command <- paste(binPath, "/ExpectationMaximizationBrainWebValidationApp", sep="") ;
        command <- paste(command, options) ;
        if ( file.access(resultfile, mode=0) == -1 )
          {
            print(command) ;
            system(command) ;
          }
                                        # prepare the command arguments for Kmeans
        resultfile <- paste(validationRoot, "/Results/BrainWeb.Kmeans.", noiseLevels[i], "pn.",
                         parametersMutationScales[j], "ext.dat", sep="") ;

        options.resultfile <- paste("--result", resultfile) ;

        options.bucketsize <- paste("--bucket-size", kdTreeBucketSize) ;
        options <- paste(options.files, options.maskfile,
                         options.parameters, options.resultfile,
                         options.iteration, options.bucketsize) ;
        
                                        #run GOF experiment
        command <- paste(binPath, "/KdTreeBasedKmeansBrainWebValidationApp", sep="") ;
        command <- paste(command, options) ;
        if ( file.access(resultfile, mode=0) == -1 )
          {
            print(command) ;
            system(command) ;
          }
      }
  }


