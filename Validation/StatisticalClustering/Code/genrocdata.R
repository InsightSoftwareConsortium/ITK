genrocdata <- function(inputFile, classLabels)
  {
    noRuns = 100 ;
    data = read.table(inputFile, header=T) ;
    colBegin = which(colnames(data) == classLabels[1]) ;
    colEnd = colBegin + length(classLabels) - 1 ;
    sensitivity <- array(dim=c(noRuns, length(classLabels))) ;
    specificity <- array(dim=c(noRuns, length(classLabels))) ;
    for ( i in 1:noRuns )
      {
        classificationMatrix = as.matrix(data[(3*i - 2):(3*i), colBegin:colEnd]) ;
        for ( j in 1:nrow(classificationMatrix) )
          {
            truePositive = classificationMatrix[j,j] ;
            falsePositive = sum(classificationMatrix[j,]) - truePositive ;
            falseNegative = sum(classificationMatrix[,j]) - truePositive ;
            trueNegative = sum(classificationMatrix) - (truePositive + falsePositive + falseNegative) ;
            sensitivity[i, j] = truePositive / ( truePositive + falseNegative) ;
            specificity[i, j] = trueNegative / ( falsePositive + trueNegative) ;
          }
      }
    ret = data.frame(sens=sensitivity, spec=specificity) ;
    fileName = strsplit(inputFile, "\\.dat") ;
    output = paste(fileName[[1]], ".roc.dat", sep="") ;
    write.table(ret, output, row.names=F) ;
  }



    
