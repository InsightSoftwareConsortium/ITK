/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkExpectationMaximizationMixtureModelEstimatorTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkWin32Header.h"

#include <fstream>

#include "itkPoint.h"
#include "itkPointSet.h"
#include "itkArray.h"
#include "itkVector.h"
#include "itkPointSetToListAdaptor.h"
#include "itkSubsample.h"

#include "itkGaussianMixtureModelComponent.h"
#include "itkExpectationMaximizationMixtureModelEstimator.h"

int itkExpectationMaximizationMixtureModelEstimatorTest(int argc, char** argv)
{
  namespace stat = itk::Statistics ;
  typedef itk::PointSet< double, 2 > PointSetType ;
  typedef stat::PointSetToListAdaptor< PointSetType >
    DataSampleType;
  typedef stat::ExpectationMaximizationMixtureModelEstimator< DataSampleType >
    EstimatorType ;
  typedef stat::GaussianMixtureModelComponent< DataSampleType > 
    ComponentType ;

  if (argc < 2)
    {
      std::cout << "ERROR: data file name argument missing." 
                << std::endl ;
      return EXIT_FAILURE;
    }


  int i ;
  char* dataFileName = argv[1] ;
  int dataSize = 2000 ;
  int bucketSize = 10 ;
  int maximumIteration = 200 ;
  typedef itk::Array< double > ParametersType ;
  double minStandardDeviation =28.54746 ;
  int numberOfClasses = 2 ;
  std::vector< ParametersType > trueParameters(numberOfClasses) ;
  ParametersType params(6) ;
  params[0] = 99.261 ;
  params[1] = 100.078 ;
  params[2] = 814.95741 ;
  params[3] = 38.40308 ;
  params[4] = 38.40308 ;
  params[5] = 817.64446 ;
  trueParameters[0] = params ;

  params[0] = 200.1 ;
  params[1] = 201.3 ;
  params[2] = 859.785295 ;
  params[3] = -3.617316 ;
  params[4] = -3.617316 ;
  params[5] = 848.991508 ;
  trueParameters[1] = params ;

  // only the means are altered
  std::vector< ParametersType > initialParameters(numberOfClasses) ;
  params[0] = 80.0 ;
  params[1] = 80.0 ;
  params[2] = 814.95741 ;
  params[3] = 38.40308 ;
  params[4] = 38.40308 ;
  params[5] = 817.64446 ;
  initialParameters[0] = params ;

  params[0] = 180.0 ;
  params[1] = 180.0 ;
  params[2] = 859.785295 ;
  params[3] = -3.617316 ;
  params[4] = -3.617316 ;
  params[5] = 848.991508 ;
  initialParameters[1] = params ;

  itk::Array< double > trueProportions(numberOfClasses) ;
  trueProportions[0] = 0.5 ;
  trueProportions[1] = 0.5 ;

  itk::Array< double > initialProportions(numberOfClasses) ;
  initialProportions[0] = 0.5 ;
  initialProportions[1] = 0.5 ;

  /* Loading point data */
  PointSetType::Pointer pointSet = PointSetType::New() ;
  PointSetType::PointsContainerPointer pointsContainer = 
    PointSetType::PointsContainer::New() ;
  pointsContainer->Reserve(dataSize) ;
  pointSet->SetPoints(pointsContainer.GetPointer()) ;

  PointSetType::PointsContainerIterator p_iter = pointsContainer->Begin() ;
  PointSetType::PointType point ;
  double temp ;
  std::ifstream dataStream(dataFileName) ;
  if ( !dataStream )
    {
      std::cout << "ERROR: fail to open the data file." << std::endl ;
      return EXIT_FAILURE ;
    }

  while (p_iter != pointsContainer->End())
    {
      for ( i = 0 ; i < PointSetType::PointDimension ; i++)
        {
          dataStream >> temp ;
          point[i] = temp ;
        }
      p_iter.Value() = point ;
      ++p_iter ;
    }

  dataStream.close() ;
  
  /* Importing the point set to the sample */
  DataSampleType::Pointer sample =
    DataSampleType::New() ;
  
  sample->SetPointSet(pointSet.GetPointer());

  /* Preparing the gaussian mixture components */
  typedef ComponentType::Pointer ComponentPointer ;
  std::vector< ComponentPointer > components ;
  components.resize(numberOfClasses) ;

  for (int i = 0 ; i < numberOfClasses ; i++)
    {
      components[i] = ComponentType::New() ;
      (components[i])->SetSample(sample.GetPointer()) ;
      (components[i])->SetParameters(initialParameters[i]) ;
    }
  
  /* Estimating */
  EstimatorType::Pointer estimator = EstimatorType::New() ;
  estimator->SetSample(sample) ;
  estimator->SetMaximumIteration(maximumIteration) ;
  estimator->SetInitialProportions(initialProportions) ;

  for (unsigned int i = 0 ; i < numberOfClasses ; i++)
    {
      estimator->AddComponent((ComponentType::Superclass*) 
                              (components[i]).GetPointer()) ;
    }

  estimator->Update() ;

  std::cout << "DEBUG: current iteration = " 
            << estimator->GetCurrentIteration() << std::endl ;

  bool passed = true ;
  double displacement ;
  for (unsigned int i = 0 ; i < numberOfClasses ; i++)
    {
      std::cout << "Cluster[" << i << "]" << std::endl ;
      std::cout << "    Parameters:" << std::endl ;
      std::cout << "         " << (components[i])->GetFullParameters() << std::endl ;
      std::cout << "    Proportion: " ;
      std::cout << "         " << (*estimator->GetProportions())[i] << std::endl ;
      displacement = 0.0 ;
      for (unsigned int j = 0 ; j < DataSampleType::MeasurementVectorSize ;
           j++)
        {
          temp = (components[i])->GetFullParameters()[j] - trueParameters[i][j] ;
          displacement += (temp * temp) ;
        }
      displacement = sqrt(displacement) ;
      std::cout << "    Mean displacement: " << std::endl ;
      std::cout << "        " << displacement 
                << std::endl << std::endl ;
      if ( displacement > (minStandardDeviation / 100.0) * 3 )
        {
          passed = false ;
        }
    }

  if( !passed )
    {
      std::cout << "Test failed." << std::endl;
      return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}







