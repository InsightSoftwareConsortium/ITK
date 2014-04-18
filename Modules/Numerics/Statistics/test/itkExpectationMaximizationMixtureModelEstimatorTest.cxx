/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/


#include <fstream>

#include "itkPointSetToListSampleAdaptor.h"

#include "itkGaussianMixtureModelComponent.h"
#include "itkExpectationMaximizationMixtureModelEstimator.h"

int itkExpectationMaximizationMixtureModelEstimatorTest(int argc, char* argv[] )
{
  namespace stat = itk::Statistics;
  typedef itk::PointSet< double, 2 >                        PointSetType;
  typedef stat::PointSetToListSampleAdaptor< PointSetType > DataSampleType;

  typedef stat::ExpectationMaximizationMixtureModelEstimator< DataSampleType >
    EstimatorType;

  typedef stat::GaussianMixtureModelComponent< DataSampleType >
    ComponentType;

  if (argc < 2)
    {
    std::cout << "ERROR: data file name argument missing."
              << std::endl;
    return EXIT_FAILURE;
    }


  unsigned int i, j;
  char* dataFileName = argv[1];
  int dataSize = 2000;
  int maximumIteration = 200;
  typedef itk::Array< double > ParametersType;
  double minStandardDeviation =28.54746;
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
    for ( i = 0; i < PointSetType::PointDimension; i++)
      {
      dataStream >> temp;
      point[i] = temp;
      }
    p_iter.Value() = point;
    ++p_iter;
    }

  dataStream.close();

  /* Importing the point set to the sample */
  DataSampleType::Pointer sample = DataSampleType::New();

  sample->SetPointSet(pointSet.GetPointer());

  /* Preparing the gaussian mixture components */
  typedef ComponentType::Pointer ComponentPointer;
  std::vector< ComponentPointer > components;
  for ( i = 0; i < numberOfClasses; i++ )
    {
    components.push_back(ComponentType::New());
    (components[i])->SetSample(sample.GetPointer());
    (components[i])->SetParameters(initialParameters[i]);
    }

  /* Estimating */
  EstimatorType::Pointer estimator = EstimatorType::New();
  estimator->SetSample(sample.GetPointer());
  estimator->SetMaximumIteration(maximumIteration);
  estimator->SetInitialProportions(initialProportions);

  for ( i = 0; i < numberOfClasses; i++)
    {
    estimator->AddComponent((ComponentType::Superclass*)
                              (components[i]).GetPointer());
    }

  estimator->Update();

  std::cout << "DEBUG: current iteration = "
            << estimator->GetCurrentIteration() << std::endl;

  bool passed = true;
  double displacement;
  const unsigned int measurementVectorSize = sample->GetMeasurementVectorSize();
  for ( i = 0; i < numberOfClasses; i++)
    {
    std::cout << "Cluster[" << i << "]" << std::endl;
    std::cout << "    Parameters:" << std::endl;
    std::cout << "         " << (components[i])->GetFullParameters() << std::endl;
    std::cout << "    Proportion: ";
    std::cout << "         " << (estimator->GetProportions())[i] << std::endl;
    displacement = 0.0;
    for ( j = 0; j < measurementVectorSize; j++)
      {
      temp = (components[i])->GetFullParameters()[j] - trueParameters[i][j];
      displacement += (temp * temp);
      }
    displacement = std::sqrt(displacement);
    std::cout << "    Mean displacement: " << std::endl;
    std::cout << "        " << displacement
              << std::endl << std::endl;
    if ( displacement > (minStandardDeviation / 100.0) * 3 )
      {
      passed = false;
      }
    }

  if( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  estimator->Print(std::cout);

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
