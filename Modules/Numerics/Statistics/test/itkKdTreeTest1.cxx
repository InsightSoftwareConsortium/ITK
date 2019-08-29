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

#include "itkMath.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkListSample.h"
#include "itkKdTreeGenerator.h"
#include <fstream>

int
itkKdTreeTest1(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " numberOfDataPoints numberOfTestPoints bucketSize [graphvizDotOutputFile]" << std::endl;
    return EXIT_FAILURE;
  }

  // Random number generator
  using NumberGeneratorType = itk::Statistics::MersenneTwisterRandomVariateGenerator;

  NumberGeneratorType::Pointer randomNumberGenerator = NumberGeneratorType::GetInstance();
  randomNumberGenerator->Initialize();

  using MeasurementVectorType = itk::Array<double>;
  using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;

  constexpr SampleType::MeasurementVectorSizeType measurementVectorSize = 2;

  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize(measurementVectorSize);

  //
  // Generate a sample of random points
  //
  const unsigned int    numberOfDataPoints = std::stoi(argv[1]);
  MeasurementVectorType mv(measurementVectorSize);
  for (unsigned int i = 0; i < numberOfDataPoints; ++i)
  {
    mv[0] = randomNumberGenerator->GetNormalVariate(0.0, 1.0);
    mv[1] = randomNumberGenerator->GetNormalVariate(0.0, 1.0);
    sample->PushBack(mv);
    std::cout << "Add measurement vector: " << mv << std::endl;
  }

  using TreeGeneratorType = itk::Statistics::KdTreeGenerator<SampleType>;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();

  const unsigned int bucketSize = std::stoi(argv[3]);

  treeGenerator->SetSample(sample);
  treeGenerator->SetBucketSize(bucketSize);
  treeGenerator->Update();

  using TreeType = TreeGeneratorType::KdTreeType;

  TreeType::Pointer tree = treeGenerator->GetOutput();

  MeasurementVectorType queryPoint(measurementVectorSize);

  unsigned int                           numberOfNeighbors = 1;
  TreeType::InstanceIdentifierVectorType neighbors;

  MeasurementVectorType result(measurementVectorSize);
  MeasurementVectorType test_point(measurementVectorSize);
  MeasurementVectorType min_point(measurementVectorSize);

  unsigned int numberOfFailedPoints1 = 0;

  const unsigned int numberOfTestPoints = std::stoi(argv[2]);

  //
  //  Check that for every point in the sample, its closest point is itself.
  //
  using DistanceMetricType = itk::Statistics::EuclideanDistanceMetric<MeasurementVectorType>;

  using OriginType = DistanceMetricType::OriginType;

  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New();

  OriginType origin(measurementVectorSize);
  for (unsigned int k = 0; k < sample->Size(); k++)
  {

    queryPoint = sample->GetMeasurementVector(k);

    for (unsigned int i = 0; i < sample->GetMeasurementVectorSize(); ++i)
    {
      origin[i] = queryPoint[i];
    }

    distanceMetric->SetOrigin(origin);

    // First, get distance from the search API
    std::vector<double> searchDistance;
    tree->Search(queryPoint, numberOfNeighbors, neighbors, searchDistance);

    // We can also get distance from the distance metric
    tree->Search(queryPoint, numberOfNeighbors, neighbors);

    for (unsigned int i = 0; i < numberOfNeighbors; ++i)
    {
      const double distanceFromMetric = distanceMetric->Evaluate(tree->GetMeasurementVector(neighbors[i]));

      if (distanceFromMetric > itk::Math::eps || searchDistance[i] > itk::Math::eps ||
          itk::Math::NotAlmostEquals(distanceFromMetric, searchDistance[i]))
      {
        std::cerr << "kd-tree knn search result:" << std::endl
                  << "query point = [" << queryPoint << "]" << std::endl
                  << "k = " << numberOfNeighbors << std::endl;
        std::cerr << "measurement vector : distance_by_distMetric : distance_by_tree" << std::endl;
        std::cerr << "[" << tree->GetMeasurementVector(neighbors[i]) << "] : " << distanceFromMetric << " : "
                  << searchDistance[i] << std::endl;
        numberOfFailedPoints1++;
      }
    }
  }

  unsigned int numberOfFailedPoints2 = 0;

  //
  // Generate a second sample of random points
  // and use them to query the tree
  //
  for (unsigned int j = 0; j < numberOfTestPoints; ++j)
  {

    double min_dist = itk::NumericTraits<double>::max();

    queryPoint[0] = randomNumberGenerator->GetNormalVariate(0.0, 1.0);
    queryPoint[1] = randomNumberGenerator->GetNormalVariate(0.0, 1.0);

    tree->Search(queryPoint, numberOfNeighbors, neighbors);

    //
    // The first neighbor should be the closest point.
    //
    result = tree->GetMeasurementVector(neighbors[0]);

    //
    // Compute the distance to the "presumed" nearest neighbor
    //
    double result_dist = std::sqrt((result[0] - queryPoint[0]) * (result[0] - queryPoint[0]) +
                                   (result[1] - queryPoint[1]) * (result[1] - queryPoint[1]));

    //
    // Compute the distance to all other points, to verify
    // whether the first neighbor was the closest one or not.
    //
    for (unsigned int i = 0; i < numberOfDataPoints; ++i)
    {
      test_point = tree->GetMeasurementVector(i);

      const double dist = std::sqrt((test_point[0] - queryPoint[0]) * (test_point[0] - queryPoint[0]) +
                                    (test_point[1] - queryPoint[1]) * (test_point[1] - queryPoint[1]));

      if (dist < min_dist)
      {
        min_dist = dist;
        min_point = test_point;
      }
    }

    if (std::fabs(min_dist - result_dist) > 10.0 * itk::NumericTraits<double>::epsilon() * min_dist)
    {
      std::cerr << "Problem found " << std::endl;
      std::cerr << "Query point " << queryPoint << std::endl;
      std::cerr << "Reported closest point " << result << " distance " << result_dist << std::endl;
      std::cerr << "Actual   closest point " << min_point << " distance " << min_dist << std::endl;
      std::cerr << std::endl;
      std::cerr << "Test FAILED." << std::endl;
      numberOfFailedPoints2++;
    }
  }


  if (argc > 4)
  {
    //
    // Plot out the tree structure to the console in the format used by Graphviz dot
    //
    std::ofstream plotFile;
    plotFile.open(argv[4]);
    tree->PlotTree(plotFile);
    plotFile.close();
  }


  if (numberOfFailedPoints1)
  {
    std::cerr << numberOfFailedPoints1 << " out of " << sample->Size();
    std::cerr << " points failed to find themselves as closest-point" << std::endl;
  }

  if (numberOfFailedPoints2)
  {
    std::cerr << numberOfFailedPoints2 << " out of " << numberOfTestPoints;
    std::cerr << " points failed to find the correct closest point." << std::endl;
  }


  if (numberOfFailedPoints1 || numberOfFailedPoints2)
  {
    return EXIT_FAILURE;
  }


  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}
