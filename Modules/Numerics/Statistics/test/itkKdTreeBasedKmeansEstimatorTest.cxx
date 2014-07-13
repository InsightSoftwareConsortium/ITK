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
#include "itkWeightedCentroidKdTreeGenerator.h"
#include "itkKdTreeBasedKmeansEstimator.h"

int itkKdTreeBasedKmeansEstimatorTest(int argc, char* argv[] )
{
  namespace stat = itk::Statistics;

  if (argc < 4)
    {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "inputFileName  bucketSize minStandardDeviation tolerancePercent" << std::endl;
    return EXIT_FAILURE;
    }

  unsigned int i;
  unsigned int j;
  char* dataFileName = argv[1];
  int dataSize = 2000;
  int bucketSize = atoi( argv[3] );
  double minStandardDeviation = atof( argv[2] );

  itk::Array< double > trueMeans(4);
  trueMeans[0] = 99.261;
  trueMeans[1] = 100.078;
  trueMeans[2] = 200.1;
  trueMeans[3] = 201.3;

  itk::Array< double > initialMeans(4);
  initialMeans[0] = 80.0;
  initialMeans[1] = 80.0;
  initialMeans[2] = 180.0;
  initialMeans[3] = 180.0;
  int maximumIteration = 200;

  /* Loading point data */
  typedef itk::PointSet< double, 2 > PointSetType;
  PointSetType::Pointer pointSet = PointSetType::New();
  PointSetType::PointsContainerPointer pointsContainer =
    PointSetType::PointsContainer::New();
  pointsContainer->Reserve(dataSize);
  pointSet->SetPoints(pointsContainer.GetPointer());

  PointSetType::PointsContainerIterator p_iter = pointsContainer->Begin();
  PointSetType::PointType point;
  double temp;
  std::ifstream dataStream(dataFileName);
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
  typedef stat::PointSetToListSampleAdaptor< PointSetType >
    DataSampleType;

  DataSampleType::Pointer sample =
    DataSampleType::New();

  sample->SetPointSet(pointSet);

  /* Creating k-d tree */
  typedef stat::WeightedCentroidKdTreeGenerator< DataSampleType > Generator;
  Generator::Pointer generator = Generator::New();

  generator->SetSample(sample.GetPointer());
  generator->SetBucketSize(bucketSize);
  generator->GenerateData();

  /* Searching kmeans */
  typedef stat::KdTreeBasedKmeansEstimator< Generator::KdTreeType > Estimator;
  Estimator::Pointer estimator = Estimator::New();
  std::cout << estimator->GetNameOfClass() << std::endl;
  estimator->Print( std::cout );


  //Set the initial means
  estimator->SetParameters(initialMeans);

  //Set the maximum iteration
  estimator->SetMaximumIteration(maximumIteration);
  if ( estimator->GetMaximumIteration() != maximumIteration )
    {
    std::cerr << "Error in Set/GetMaximum Iteration" << std::endl;
    return EXIT_FAILURE;
    }

  estimator->SetKdTree(generator->GetOutput());

  //Set the centroid position change threshold
  estimator->SetCentroidPositionChangesThreshold(0.0);
  const double tolerance = 0.1;
  if( std::fabs(estimator->GetCentroidPositionChangesThreshold() - 0.0) > tolerance )
    {
    std::cerr << "Set/GetCentroidPositionChangesThreshold() " << std::endl;
    return EXIT_FAILURE;
    }


  estimator->StartOptimization();
  Estimator::ParametersType estimatedMeans = estimator->GetParameters();

  bool passed = true;
  int index;
  const unsigned int numberOfMeasurements = sample->GetMeasurementVectorSize();
  const unsigned int numberOfClasses = trueMeans.size() / numberOfMeasurements;
  for (i = 0; i < numberOfClasses; i++)
    {
    std::cout << "cluster[" << i << "] " << std::endl;
    double displacement = 0.0;
    std::cout << "    true mean :" << std::endl;
    std::cout << "        ";
    index = numberOfMeasurements * i;
    for (j = 0; j < numberOfMeasurements; j++)
      {
      std::cout << trueMeans[index] << " ";
      ++index;
      }
    std::cout << std::endl;
    std::cout << "    estimated mean :" << std::endl;
    std::cout << "        ";

    index = numberOfMeasurements * i;
    for (j = 0; j < numberOfMeasurements; j++)
      {
      std::cout << estimatedMeans[index] << " ";
      temp = estimatedMeans[index] - trueMeans[index];
      ++index;
      displacement += (temp * temp);
      }
    std::cout << std::endl;
    displacement = std::sqrt(displacement);
    std::cout << "    Mean displacement: " << std::endl;
    std::cout << "        " << displacement
              << std::endl << std::endl;

    double tolearancePercent = atof( argv[3] );

    // if the displacement of the estimates are within tolearancePercent% of
    // standardDeviation then we assume it is successful
    if( displacement > ( minStandardDeviation * tolearancePercent ) )
      {
      std::cerr << "displacement is larger than tolerance ";
      std::cerr << minStandardDeviation * tolearancePercent << std::endl;
      passed = false;
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
