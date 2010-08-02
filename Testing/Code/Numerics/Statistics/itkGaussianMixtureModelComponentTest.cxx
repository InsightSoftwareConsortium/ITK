/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGaussianMixtureModelComponentTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkWin32Header.h"

#include <fstream>

#include "itkPoint.h"
#include "itkPointSet.h"
#include "itkArray.h"
#include "itkVector.h"
#include "itkPointSetToListSampleAdaptor.h"
#include "itkSubsample.h"

#include "itkGaussianMixtureModelComponent.h"
#include "itkExpectationMaximizationMixtureModelEstimator.h"

int itkGaussianMixtureModelComponentTest(int argc, char* argv[] )
{
  typedef itk::PointSet< double, 2 > PointSetType;
  typedef itk::Statistics::PointSetToListSampleAdaptor< PointSetType >
    DataSampleType;
  typedef itk::Statistics::GaussianMixtureModelComponent< DataSampleType >
    ComponentType;

  if (argc < 2)
    {
      std::cout << "ERROR: data file name argument missing."
                << std::endl;
      return EXIT_FAILURE;
    }

  char* dataFileName = argv[1];
  int dataSize = 2000;
  typedef itk::Array< double > ParametersType;
  unsigned int numberOfClasses = 2;
  std::vector< ParametersType > trueParameters(numberOfClasses);
  ParametersType params(6);
  params[0] = 99.261;
  params[1] = 100.078;
  params[2] = 814.95741;
  params[3] = 38.40308;
  params[4] = 38.40308;
  params[5] = 817.64446;
  trueParameters[0] = params;

  params[0] = 200.1;
  params[1] = 201.3;
  params[2] = 859.785295;
  params[3] = -3.617316;
  params[4] = -3.617316;
  params[5] = 848.991508;
  trueParameters[1] = params;

  // only the means are altered
  std::vector< ParametersType > initialParameters(numberOfClasses);
  params[0] = 80.0;
  params[1] = 80.0;
  params[2] = 814.95741;
  params[3] = 38.40308;
  params[4] = 38.40308;
  params[5] = 817.64446;
  initialParameters[0] = params;

  params[0] = 180.0;
  params[1] = 180.0;
  params[2] = 859.785295;
  params[3] = -3.617316;
  params[4] = -3.617316;
  params[5] = 848.991508;
  initialParameters[1] = params;

  itk::Array< double > trueProportions(numberOfClasses);
  trueProportions[0] = 0.5;
  trueProportions[1] = 0.5;

  itk::Array< double > initialProportions(numberOfClasses);
  initialProportions[0] = 0.5;
  initialProportions[1] = 0.5;

  /* Loading point data */
  PointSetType::Pointer pointSet = PointSetType::New();
  PointSetType::PointsContainerPointer pointsContainer =
    PointSetType::PointsContainer::New();
  pointsContainer->Reserve(dataSize);
  pointSet->SetPoints(pointsContainer.GetPointer());

  PointSetType::PointsContainerIterator p_iter = pointsContainer->Begin();
  PointSetType::PointType point;
  double temp;
  std::ifstream dataStream(dataFileName);
  if ( !dataStream )
    {
    std::cout << "ERROR: fail to open the data file." << std::endl;
    return EXIT_FAILURE;
    }

  while (p_iter != pointsContainer->End())
    {
    for (unsigned int i = 0; i < PointSetType::PointDimension; i++)
      {
      dataStream >> temp;
      point[i] = temp;
      }
    p_iter.Value() = point;
    ++p_iter;
    }

  dataStream.close();

  /* Importing the point set to the sample */
  DataSampleType::Pointer sample =
    DataSampleType::New();

  sample->SetPointSet(pointSet.GetPointer());

  /* Preparing the gaussian mixture components */
  typedef ComponentType::Pointer ComponentPointer;
  std::vector< ComponentPointer > components;
  for ( unsigned int i = 0; i < numberOfClasses; i++ )
    {
    components.push_back(ComponentType::New());
    (components[i])->SetSample(sample.GetPointer());
    (components[i])->SetParameters(initialParameters[i]);
    }

  ComponentPointer testComponent = ComponentType::New();

  std::cout << testComponent->GetNameOfClass() << std::endl;
  testComponent->Print( std::cout );

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
