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

#include "itkListSample.h"
#include "itkKdTreeGenerator.h"
#include <fstream>

int itkKdTreeTestSamplePoints(int , char *[] )
{
  typedef itk::Array< double > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  const SampleType::MeasurementVectorSizeType measurementVectorSize = 2;

  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize( measurementVectorSize );

  const unsigned int numberOfDataPoints = 5;
  MeasurementVectorType mv( measurementVectorSize );
  mv[0] = 0.0342;
  mv[1] = 0.5175;
  sample->PushBack( mv );

  MeasurementVectorType mv2( measurementVectorSize );
  mv2[0] = 0.9650;
  mv2[1] = -0.9379;
  sample->PushBack( mv2 );

  MeasurementVectorType mv3( measurementVectorSize );
  mv3[0] = -0.0471;
  mv3[1] = 0.8177;
  sample->PushBack( mv3 );

  MeasurementVectorType mv4( measurementVectorSize );
  mv4[0] = 0.4737;
  mv4[1] = -1.0447;
  sample->PushBack( mv4 );

  MeasurementVectorType mv5( measurementVectorSize );
  mv5[0] = -0.6307;
  mv5[1] = -2.7600;
  sample->PushBack( mv5 );

  typedef itk::Statistics::KdTreeGenerator< SampleType > TreeGeneratorType;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();

  const unsigned int bucketSize = 1;

  treeGenerator->SetSample( sample );
  treeGenerator->SetBucketSize( bucketSize );
  treeGenerator->Update();

  typedef TreeGeneratorType::KdTreeType TreeType;

  TreeType::Pointer tree = treeGenerator->GetOutput();

  MeasurementVectorType queryPoint( measurementVectorSize );

  unsigned int numberOfNeighbors = 1;
  TreeType::InstanceIdentifierVectorType neighbors;

  MeasurementVectorType result( measurementVectorSize );
  MeasurementVectorType test_point( measurementVectorSize );
  MeasurementVectorType min_point( measurementVectorSize );

  //
  //  Check that for every point in the sample, its closest point is itself.
  //
  typedef itk::Statistics::EuclideanDistanceMetric< MeasurementVectorType > DistanceMetricType;
  typedef DistanceMetricType::OriginType OriginType;

  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New();

  OriginType origin( measurementVectorSize );
  for( unsigned int k = 0; k < sample->Size(); k++ )
    {

    queryPoint = sample->GetMeasurementVector(k);

    for ( unsigned int i = 0; i < sample->GetMeasurementVectorSize(); ++i )
      {
      origin[i] = queryPoint[i];
      }

    distanceMetric->SetOrigin( origin );

    tree->Search( queryPoint, numberOfNeighbors, neighbors );

    for ( unsigned int i = 0; i < numberOfNeighbors; ++i )
      {
      const double distance =
        distanceMetric->Evaluate( tree->GetMeasurementVector( neighbors[i] ));

      if( distance > itk::Math::eps )
        {
        std::cerr << "kd-tree knn search result:" << std::endl
                  << "query point = [" << queryPoint << "]" << std::endl
                  << "k = " << numberOfNeighbors << std::endl;
        std::cerr << "measurement vector : distance" << std::endl;
        std::cerr << "[" << tree->GetMeasurementVector( neighbors[i] )
                  << "] : "
                  << distance << std::endl;
        }
      }
    }

  double min_dist = itk::NumericTraits< double >::max();

  /*
  queryPoint[0] = 1.16651;
  queryPoint[1] = 0.16395;
  */

  /*
  queryPoint[0] = 1.0;
  queryPoint[1] = 0.12;
  */

  queryPoint[0] = 1.0;
  queryPoint[1] = 0.1;


  tree->Search( queryPoint, numberOfNeighbors, neighbors );

  //
  // The first neighbor should be the closest point.
  //
  result = tree->GetMeasurementVector( neighbors[0] );

  //
  // Compute the distance to the "presumed" nearest neighbor
  //
  double result_dist = std::sqrt(
        (result[0] - queryPoint[0]) *
        (result[0] - queryPoint[0]) +
        (result[1] - queryPoint[1]) *
        (result[1] - queryPoint[1])
        );

  //
  // Compute the distance to all other points, to verify
  // whether the first neighbor was the closest one or not.
  //
  for( unsigned int i = 0; i < numberOfDataPoints; ++i )
    {
    test_point = tree->GetMeasurementVector( i );

    std::cout << "Compute distance with: " << test_point;

    const double dist = std::sqrt(
        (test_point[0] - queryPoint[0]) *
        (test_point[0] - queryPoint[0]) +
        (test_point[1] - queryPoint[1]) *
        (test_point[1] - queryPoint[1])
        );

    std::cout << "\t" << dist << std::endl;

    if( dist < min_dist )
      {
      min_dist = dist;
      min_point = test_point;
      }
    }

  if( min_dist < result_dist )
    {
    std::cerr << "Problem found " << std::endl;
    std::cerr << "Query point " << queryPoint << std::endl;
    std::cerr << "Reported closest point " << result
              << " distance " << result_dist << std::endl;
    std::cerr << "Actual   closest point " << min_point
              << " distance " << min_dist << std::endl;
    std::cerr << std::endl;
    std::cerr << "Test FAILED." << std::endl;
    }

  //
  // Plot out the tree structure to the console in the format used by Graphviz dot
  //
  std::ofstream plotFile;
  plotFile.open( "plot.dot" );
  tree->PlotTree( plotFile );
  plotFile.close();

  return EXIT_SUCCESS;
}
