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
#include "itkWeightedCentroidKdTreeGenerator.h"
#include "itkEuclideanDistance.h"

#include <iostream>
#include <fstream>


int itkKdTreeTest2( int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing argument" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " pointsInputFile " << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 3;

  typedef itk::Vector< float, Dimension > MeasurementVectorType;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;
  SampleType::Pointer sample = SampleType::New();
  sample->SetMeasurementVectorSize( Dimension );

  MeasurementVectorType mv;
  MeasurementVectorType queryPoint;

  std::ifstream pntFile;

  pntFile.open( argv[1], std::ios_base::in|std::ios_base::binary );

  for (unsigned int i = 0 ; i < 131854 ; ++i )
    {
    pntFile.read( (char*)(&mv[0]), sizeof(float) );
    pntFile.read( (char*)(&mv[1]), sizeof(float) );
    pntFile.read( (char*)(&mv[2]), sizeof(float) );
    sample->PushBack( mv );
    if( i==653 )
      {
      queryPoint = mv;  // -> the exact query point is part of the tree
      }
    }
  pntFile.close();

  typedef itk::Statistics::KdTreeGenerator< SampleType > TreeGeneratorType;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();

  treeGenerator->SetSample( sample );
  treeGenerator->SetBucketSize( 16 );
  treeGenerator->Update();

  typedef TreeGeneratorType::KdTreeType TreeType;
  typedef TreeType::NearestNeighbors NeighborsType;
  typedef TreeType::KdTreeNodeType NodeType;

  TreeType::Pointer tree = treeGenerator->GetOutput();
    
  typedef itk::Statistics::EuclideanDistance< MeasurementVectorType > 
    DistanceMetricType;
  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New();

  DistanceMetricType::OriginType origin( Dimension );

  for ( unsigned int i = 0 ; i < sample->GetMeasurementVectorSize() ; ++i )
    {
    origin[i] = queryPoint[i];
    }

  distanceMetric->SetOrigin( origin );
  
  unsigned int numberOfNeighbors = 1;
  TreeType::InstanceIdentifierVectorType neighbors;
  
  // this search has to return the query point itself and a distance of 0, 
  // but it does return a point ~0.74 units away:
  tree->Search( queryPoint, numberOfNeighbors, neighbors ) ; 
  
  std::cout << "kd-tree knn search result:" << std::endl 
            << "query point = [" << queryPoint << "]" << std::endl
            << "k = " << numberOfNeighbors << std::endl;
  std::cout << "measurement vector : distance" << std::endl;

  for ( unsigned int i = 0 ; i < numberOfNeighbors ; ++i )
    {
    std::cout << "[" << tree->GetMeasurementVector( neighbors[i] )
              << "] : "  
              << distanceMetric->Evaluate( 
                  tree->GetMeasurementVector( neighbors[i] )) 
              << std::endl;
    }

  return EXIT_SUCCESS;
}
