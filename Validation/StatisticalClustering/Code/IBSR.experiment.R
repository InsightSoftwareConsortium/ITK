validationRoot <- "/share/work/src/Insight/Validation/StatisticalClustering" ;
imagePath <- "/share/work/data/ibsr" ;
binPath <- "/share/bin-linux/insight/bin" ;
paramPath <- paste(validationRoot, "/Inputs/ibsr", sep="") ;
scriptPath <- getwd() ;
iteration <- 4000 ;
parametersMutationScales <- c(1, 2) ;
scans <- c("100_23", "111_2", "11_3", "13_3", "16_3",
            "191_3", "202_3", "2_4", "5_8", "7_8") ;

kdTreeBucketSize = 100 ;

sliceOffsetTable = read.table(paste(imagePath, "/20Normals_T1_seg/offset.dat", sep = ""),
  header=TRUE) ;
for ( i in 1:length(scans) )
  {
    for ( j in 1:length(parametersMutationScales) )
      {
                                        # prepare common command arguments for EM & GOF
        imageFiles <- paste(imagePath, "/20Normals_T1_brain/", scans[i], ".mhd", sep="") ;
        paramFile <- paste(paramPath, "/", scans[i], ".params.",
                           parametersMutationScales[j], "ext.dat", sep="") ;
        maskFile <- paste(imagePath, "/20Normals_T1_seg/", scans[i], ".mhd", sep="") ;

        options.files <- paste("--images", imageFiles) ;

        options.maskfile <- paste("--mask", maskFile) ;
        
        options.parameters <- paste("--parameters", paramFile) ;

        options.sliceOffset <- paste("--slice-offset",
                                     sliceOffsetTable$Offset[which(sliceOffsetTable$Scan == scans[i])]) ;
        
        options.iteration <- paste("--iteration", iteration) ;

                                        # prepare the command arguments for EM
        resultfile <- paste(validationRoot, "/Results/IBSR.EM.", scans[i], ".",
                            parametersMutationScales[j], "ext.dat", sep="") ;

        options.resultfile <- paste("--result", resultfile) ;
        
        options <- paste(options.files, options.maskfile,
                         options.parameters,
                         options.resultfile,
                         options.sliceOffset,
                         options.iteration) ;
        
                                        #run EM experiment
        command <- paste(binPath, "/ExpectationMaximizationIBSRValidationApp", sep="") ;
        command <- paste(command, options) ;
        if ( file.access(resultfile, mode=0) == -1 )
          {
            print(command) ;
            system(command) ;
          }
                                        # prepare the command arguments for Kmeans
        resultfile <- paste(validationRoot, "/Results/IBSR.Kmeans.", scans[i], ".",
                            parametersMutationScales[j], "ext.dat", sep="") ;

        options.resultfile <- paste("--result", resultfile) ;

        options.bucketsize <- paste("--bucket-size", kdTreeBucketSize) ;
        options <- paste(options.files, options.maskfile,
                         options.parameters, options.resultfile,
                         options.sliceOffset,
                         options.iteration, options.bucketsize) ;
        
                                        #run GOF experiment
        command <- paste(binPath, "/KdTreeBasedKmeansIBSRValidationApp", sep="") ;
        command <- paste(command, options) ;
        if ( file.access(resultfile, mode=0) == -1 )
          {
            print(command) ;
            system(command) ;
          }
      }
  }


