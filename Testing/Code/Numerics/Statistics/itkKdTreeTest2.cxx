/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeTest2.cxx
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
#include "itkListSample.h"
#include "itkKdTree.h"
#include "itkKdTreeGenerator.h"
#include "itkEuclideanDistanceMetric.h"

#include <iostream>
#include <fstream>


int itkKdTreeTest2( int argc, char * argv [] )
{

  if( argc < 4 )
    {
    std::cerr << "Missing argument" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " pointsInputFile  bucketSize graphvizDotOutputFile" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;
  typedef float MeasurementValueType;

  typedef itk::Vector< MeasurementValueType, Dimension > MeasurementVectorType;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;
  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize( Dimension );

  MeasurementVectorType mv;
  MeasurementVectorType queryPoint;

  std::ifstream pntFile;

  pntFile.open( argv[1], std::ios_base::in|std::ios_base::binary );

  pntFile >> mv[0] >> mv[1];

  do
    {
    sample->PushBack( mv );
    pntFile >> mv[0] >> mv[1];
    }
  while( !pntFile.eof() );

  pntFile.close();

  typedef itk::Statistics::KdTreeGenerator< SampleType > TreeGeneratorType;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();

  const unsigned int bucketSize = atoi( argv[2] );

  treeGenerator->SetSample( sample );
  treeGenerator->SetBucketSize( bucketSize );
  treeGenerator->Update();

  typedef TreeGeneratorType::KdTreeType TreeType;
  typedef TreeType::NearestNeighbors    NeighborsType;
  typedef TreeType::KdTreeNodeType      NodeType;

  bool testFailed = false;

  TreeType::Pointer tree = treeGenerator->GetOutput();

  typedef itk::Statistics::EuclideanDistanceMetric< MeasurementVectorType >
    DistanceMetricType;
  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New();

  DistanceMetricType::OriginType origin( Dimension );

  //
  // Print out the tree structure to the console
  //
  tree->PrintTree( std::cout );


  for( unsigned int k = 0; k < sample->Size(); k++ )
    {

    queryPoint = sample->GetMeasurementVector(k);

    for ( unsigned int i = 0; i < sample->GetMeasurementVectorSize(); ++i )
      {
      origin[i] = queryPoint[i];
      }

    std::cout << "----------------------------------" << std::endl;

    std::cout << "Origin = " << origin << std::endl;

    distanceMetric->SetOrigin( origin );

    unsigned int numberOfNeighbors = 1;
    TreeType::InstanceIdentifierVectorType neighbors;

    tree->Search( queryPoint, numberOfNeighbors, neighbors );

    std::cout << "kd-tree knn search result:" << std::endl
              << "query point = [" << queryPoint << "]" << std::endl
              << "k = " << numberOfNeighbors << std::endl;
    std::cout << "measurement vector : distance" << std::endl;

    for ( unsigned int i = 0; i < numberOfNeighbors; ++i )
      {
      const double distance =
        distanceMetric->Evaluate( tree->GetMeasurementVector( neighbors[i] ));

      std::cout << "[" << tree->GetMeasurementVector( neighbors[i] )
                << "] : "
                << distance << std::endl;

      if( distance > vnl_math::eps )
        {
        testFailed = true;
        }
      }
    }


  //
  // Plot out the tree structure to the console in the format used by Graphviz dot
  //
  std::ofstream plotFile;
  plotFile.open( argv[3] );
  tree->PlotTree( plotFile );
  plotFile.close();


  if( testFailed )
    {
    std::cerr << "Incorrect distance was found" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
