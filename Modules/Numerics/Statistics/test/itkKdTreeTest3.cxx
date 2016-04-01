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

#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkListSample.h"
#include "itkKdTreeGenerator.h"
#include <fstream>
#include <algorithm>

int itkKdTreeTest3( int argc , char * argv [] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " numberOfDataPoints numberOfTestPoints "
      << "numberOfNeighbors bucketSize [graphvizDotOutputFile]" << std::endl;
    return EXIT_FAILURE;
    }

  // Random number generator
  typedef itk::Statistics::MersenneTwisterRandomVariateGenerator
    NumberGeneratorType;

  NumberGeneratorType::Pointer randomNumberGenerator =
    NumberGeneratorType::GetInstance();
  randomNumberGenerator->Initialize();

  typedef itk::Array< double > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  const SampleType::MeasurementVectorSizeType measurementVectorSize = 2;

  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize( measurementVectorSize );

  //
  // Generate a sample of random points
  //
  const unsigned int numberOfDataPoints = atoi( argv[1] );
  MeasurementVectorType mv( measurementVectorSize );
  for ( unsigned int i = 0; i < numberOfDataPoints; ++i )
    {
    mv[0] = randomNumberGenerator->GetNormalVariate( 0.0, 1.0 );
    mv[1] = randomNumberGenerator->GetNormalVariate( 0.0, 1.0 );
    sample->PushBack( mv );
    }

  typedef itk::Statistics::KdTreeGenerator< SampleType > TreeGeneratorType;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();

  const unsigned int bucketSize = atoi( argv[4] );

  treeGenerator->SetSample( sample );
  treeGenerator->SetBucketSize( bucketSize );
  treeGenerator->Update();

  typedef TreeGeneratorType::KdTreeType TreeType;

  TreeType::Pointer tree = treeGenerator->GetOutput();

  MeasurementVectorType queryPoint( measurementVectorSize );

  unsigned int numberOfNeighbors = atoi( argv[3] );
  if (numberOfNeighbors > numberOfDataPoints)
    {
    numberOfNeighbors = numberOfDataPoints;
    }
  TreeType::InstanceIdentifierVectorType neighbors1;
  TreeType::InstanceIdentifierVectorType neighbors2;

  MeasurementVectorType result( measurementVectorSize );
  MeasurementVectorType test_point( measurementVectorSize );
  MeasurementVectorType min_point( measurementVectorSize );

  unsigned int numberOfFailedPoints1 = 0;

  const unsigned int numberOfTestPoints = atoi( argv[2] );

  //
  //  Check that for every point in the sample, its closest point is itself.
  //
  typedef itk::Statistics::EuclideanDistanceMetric<MeasurementVectorType>
    DistanceMetricType;

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

    tree->Search( queryPoint, numberOfNeighbors, neighbors1 );
    double max_distance = 0.;

    for ( size_t i = 0; i < neighbors1.size(); ++i )
      {
      const double distance =
        distanceMetric->Evaluate( tree->GetMeasurementVector( neighbors1[i] ) );

      max_distance = std::max( distance, max_distance );
      }

    max_distance += itk::NumericTraits<double>::epsilon() * 10.0;
    tree->Search( queryPoint, max_distance, neighbors2 );

    for( size_t i = 0; i < neighbors2.size(); ++i )
      {
      TreeType::InstanceIdentifierVectorType::iterator temp_it =
        std::find( neighbors1.begin(), neighbors1.end(), neighbors2[i] );
      if( temp_it == neighbors1.end() )
        {
        std::cerr << "neighbors2[" <<i << "] = " << neighbors2[i]
                  << " is not in neighbors1" << std::endl;
        numberOfFailedPoints1++;
        }
      }

    for( size_t i = 0; i < neighbors1.size(); ++i )
      {
      TreeType::InstanceIdentifierVectorType::iterator temp_it =
        std::find( neighbors2.begin(), neighbors2.end(), neighbors1[i] );
      if( temp_it == neighbors2.end() )
        {
        std::cerr << "neighbors1[" <<i << "] = " << neighbors1[i]
                  << " is not in neighbors2" << std::endl;
        numberOfFailedPoints1++;
        }
      }
    }

  //
  // Generate a second sample of random points
  // and use them to query the tree
  //
  unsigned int numberOfFailedPoints2 = 0;

  for( unsigned int j = 0; j < numberOfTestPoints; ++j )
    {
    double max_distance = 0.;

    queryPoint[0] = randomNumberGenerator->GetNormalVariate( 0.0, 1.0 );
    queryPoint[1] = randomNumberGenerator->GetNormalVariate( 0.0, 1.0 );

    for ( unsigned int i = 0; i < sample->GetMeasurementVectorSize(); ++i )
      {
      origin[i] = queryPoint[i];
      }

    distanceMetric->SetOrigin( origin );

    tree->Search( queryPoint, numberOfNeighbors, neighbors1 );

    for ( size_t i = 0; i < neighbors1.size(); ++i )
      {
      const double distance =
        distanceMetric->Evaluate( tree->GetMeasurementVector( neighbors1[i] ) );

      max_distance = std::max( distance, max_distance );
      }

    max_distance += itk::NumericTraits<double>::epsilon() * 10.0;
    tree->Search( queryPoint, max_distance, neighbors2 );

    for( size_t i = 0; i < neighbors2.size(); ++i )
      {
      const double distance =
        distanceMetric->Evaluate( tree->GetMeasurementVector( neighbors2[i] ) );
      if( distance <= max_distance )
        {
        TreeType::InstanceIdentifierVectorType::iterator temp_it =
          std::find( neighbors1.begin(), neighbors1.end(), neighbors2[i] );
        if( temp_it == neighbors1.end() )
          {
          std::cerr << "neighbors2[" << i << "] = " << neighbors2[i]
                    << " is not in neighbors1" << std::endl;
          numberOfFailedPoints2++;
          }
        }
      }
    }

  if( argc > 5 )
    {
    //
    // Plot out the tree structure to the console in the format used by Graphviz dot
    //
    std::ofstream plotFile;
    plotFile.open( argv[5] );
    tree->PlotTree( plotFile );
    plotFile.close();
    }


  if( numberOfFailedPoints1 )
    {
    std::cerr << numberOfFailedPoints1 << " out of " << sample->Size();
    std::cerr << " points failed to find themselves as closest-point" << std::endl;
    }

  if( numberOfFailedPoints2 )
    {
    std::cerr << numberOfFailedPoints2 << " out of " << numberOfTestPoints;
    std::cerr << " points failed to find the correct closest point." << std::endl;
    }


  if( numberOfFailedPoints1 || numberOfFailedPoints2 )
    {
    return EXIT_FAILURE;
    }


  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}
