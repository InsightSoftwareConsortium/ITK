/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeTest1.cxx
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

#include "itkVector.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkListSample.h"
#include "itkKdTree.h"
#include "itkKdTreeGenerator.h"
#include "itkEuclideanDistance.h"

int itkKdTreeTest1(int argc, char * argv[] )
{
  // Random number generator
  typedef itk::Statistics::MersenneTwisterRandomVariateGenerator NumberGeneratorType;

  NumberGeneratorType::Pointer randomNumberGenerator = NumberGeneratorType::New();
  randomNumberGenerator->Initialize();
  
  typedef itk::Array< double > MeasurementVectorType ;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;

  const SampleType::MeasurementVectorSizeType measurementVectorSize = 2;

  SampleType::Pointer sample = SampleType::New() ;
  sample->SetMeasurementVectorSize( measurementVectorSize );

  MeasurementVectorType mv( measurementVectorSize ) ;
  for (unsigned int i = 0 ; i < 1000 ; ++i )
    {
    mv[0] = randomNumberGenerator->GetNormalVariate( 0.0, 1.0 );
    mv[1] = randomNumberGenerator->GetNormalVariate( 0.0, 1.0 );
    sample->PushBack( mv ) ;
    }

  typedef itk::Statistics::KdTreeGenerator< SampleType > TreeGeneratorType ;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New() ;

  treeGenerator->SetSample( sample ) ;
  treeGenerator->SetBucketSize( 16 ) ;
  treeGenerator->Update() ;

  typedef TreeGeneratorType::KdTreeType TreeType ;
  typedef TreeType::NearestNeighbors NeighborsType ;
  typedef TreeType::KdTreeNodeType NodeType ;

  TreeType::Pointer tree = treeGenerator->GetOutput() ;

  MeasurementVectorType queryPoint( measurementVectorSize ) ;

  unsigned int numberOfNeighbors = 1 ;
  TreeType::InstanceIdentifierVectorType neighbors ;

  MeasurementVectorType result( measurementVectorSize ) ;
  MeasurementVectorType test_point( measurementVectorSize ) ;

  for (unsigned int i = 0 ; i < 1000 ; ++i )
    {

    double min_dist = itk::NumericTraits< double >::max();

    queryPoint[0] = randomNumberGenerator->GetNormalVariate( 0.0, 1.0 );
    queryPoint[1] = randomNumberGenerator->GetNormalVariate( 0.0, 1.0 );
    tree->Search( queryPoint, numberOfNeighbors, neighbors ) ;
    result = tree->GetMeasurementVector( neighbors[0] );

    double result_dist = sqrt(
          (result[0] - queryPoint[0]) *
          (result[0] - queryPoint[0]) +
          (result[1] - queryPoint[1]) *
          (result[1] - queryPoint[1])
          );

    for( unsigned int i = 0 ; i < 1000 ; ++i )
      {
      double dist;
      test_point = tree->GetMeasurementVector( i );
      dist = sqrt(
          (test_point[0] - queryPoint[0]) *
          (test_point[0] - queryPoint[0]) +
          (test_point[1] - queryPoint[1]) *
          (test_point[1] - queryPoint[1])
          );
      if( dist < min_dist )
        {
        min_dist = dist;
        }
      }
    if( min_dist < result_dist )
      {
      std::cout << min_dist << " " << result_dist << std::endl;
      std::cout << "Test FAILED." << std::endl;
      return EXIT_FAILURE;
      }

    }
  
  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}
