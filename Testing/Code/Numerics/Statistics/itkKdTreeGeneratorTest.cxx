/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeGeneratorTest.cxx
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
#include "itkEuclideanDistance.h"

int itkKdTreeGeneratorTest(int, char* [])
{
  // Testing KdTreeGenerator with Arrays as the measurement vectors
  {
  typedef itk::Array< float > MeasurementVectorType ;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
  
  const SampleType::MeasurementVectorSizeType measurementVectorSize = 2;
  
  SampleType::Pointer sample = SampleType::New() ;
  sample->SetMeasurementVectorSize( measurementVectorSize );

  MeasurementVectorType mv( measurementVectorSize ) ;
  for (unsigned int i = 0 ; i < 1000 ; ++i )
    {
    mv[0] = (float) i ;
    mv[1] = (float) ((1000 - i) / 2 ) ;
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

  NodeType* root = tree->GetRoot() ;

  if ( root->IsTerminal() )
    {
    std::cout << "Root node is a terminal node." << std::endl ;
    }
  else
    {
    std::cout << "Root node is not a terminal node." << std::endl ;
    }

  unsigned int partitionDimension ;
  float partitionValue ;
  root->GetParameters( partitionDimension, partitionValue) ;
  std::cout << "Dimension chosen to split the space = " 
            << partitionDimension << std::endl ;
  std::cout << "Split point on the partition dimension = "
            << partitionValue << std::endl ;

  std::cout << "Address of the left chile of the root node = "
            << root->Left() << std::endl ;
  
  std::cout << "Address of the right chile of the root node = "
            << root->Right() << std::endl ;

  MeasurementVectorType queryPoint( measurementVectorSize ) ;
  queryPoint[0] = 10.0 ;
  queryPoint[1] = 7.0 ;

  typedef itk::Statistics::EuclideanDistance< MeasurementVectorType > DistanceMetricType ;
  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New() ;
  DistanceMetricType::OriginType origin( measurementVectorSize );
  for ( unsigned int i = 0 ; i < measurementVectorSize; ++i )
    {
    origin[i] = queryPoint[i] ;
    }
  distanceMetric->SetOrigin( origin ) ;

  unsigned int numberOfNeighbors = 3 ;
  TreeType::InstanceIdentifierVectorType neighbors ;
  tree->Search( queryPoint, numberOfNeighbors, neighbors ) ; 
  
  std::cout << "kd-tree knn search result:" << std::endl 
            << "query point = [" << queryPoint << "]" << std::endl
            << "k = " << numberOfNeighbors << std::endl ;
  std::cout << "measurement vector : distance" << std::endl ;
  for ( unsigned int i = 0 ; i < numberOfNeighbors ; ++i )
    {
    std::cout << "[" << tree->GetMeasurementVector( neighbors[i] )
              << "] : "  
              << distanceMetric->Evaluate( tree->GetMeasurementVector( neighbors[i])) << std::endl ;
    }

  double radius = 437.0 ;

  tree->Search( queryPoint, radius, neighbors ) ; 
  
  std::cout << "kd-tree radius search result:" << std::endl
            << "query point = [" << queryPoint << "]" << std::endl
            << "search radius = " << radius << std::endl ;
  std::cout << "measurement vector : distance" << std::endl ;
  for ( unsigned int i = 0 ; i < neighbors.size() ; ++i )
    {
    std::cout << "[" << tree->GetMeasurementVector( neighbors[i] )
              << "] : "  
              << distanceMetric->Evaluate( tree->GetMeasurementVector( neighbors[i])) << std::endl ;
    }    
  }

  // Testing KdTreeGenerator with Fixed length vectors as the measurement vectors.
  {
  typedef itk::Vector< float, 2 > MeasurementVectorType ;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;
  sample->SetMeasurementVectorSize( 2 );

  MeasurementVectorType mv ;
  for (unsigned int i = 0 ; i < 1000 ; ++i )
    {
    mv[0] = (float) i ;
    mv[1] = (float) ((1000 - i) / 2 ) ;
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

  NodeType* root = tree->GetRoot() ;

  if ( root->IsTerminal() )
    {
    std::cout << "Root node is a terminal node." << std::endl ;
    }
  else
    {
    std::cout << "Root node is not a terminal node." << std::endl ;
    }

  unsigned int partitionDimension ;
  float partitionValue ;
  root->GetParameters( partitionDimension, partitionValue) ;
  std::cout << "Dimension chosen to split the space = " 
            << partitionDimension << std::endl ;
  std::cout << "Split point on the partition dimension = "
            << partitionValue << std::endl ;

  std::cout << "Address of the left chile of the root node = "
            << root->Left() << std::endl ;
  
  std::cout << "Address of the right chile of the root node = "
            << root->Right() << std::endl ;

  MeasurementVectorType queryPoint ;
  queryPoint[0] = 10.0 ;
  queryPoint[1] = 7.0 ;

  typedef itk::Statistics::EuclideanDistance< MeasurementVectorType > DistanceMetricType ;
  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New() ;
  DistanceMetricType::OriginType origin( 2 ) ;
  for ( unsigned int i = 0 ; i < MeasurementVectorType::Length ; ++i )
    {
    origin[i] = queryPoint[i] ;
    }
  distanceMetric->SetOrigin( origin ) ;

  unsigned int numberOfNeighbors = 3 ;
  TreeType::InstanceIdentifierVectorType neighbors ;
  tree->Search( queryPoint, numberOfNeighbors, neighbors ) ; 
  
  std::cout << "kd-tree knn search result:" << std::endl 
            << "query point = [" << queryPoint << "]" << std::endl
            << "k = " << numberOfNeighbors << std::endl ;
  std::cout << "measurement vector : distance" << std::endl ;
  for ( unsigned int i = 0 ; i < numberOfNeighbors ; ++i )
    {
    std::cout << "[" << tree->GetMeasurementVector( neighbors[i] )
              << "] : "  
              << distanceMetric->Evaluate( tree->GetMeasurementVector( neighbors[i])) << std::endl ;
    }

  double radius = 437.0 ;

  tree->Search( queryPoint, radius, neighbors ) ; 
  
  std::cout << "kd-tree radius search result:" << std::endl
            << "query point = [" << queryPoint << "]" << std::endl
            << "search radius = " << radius << std::endl ;
  std::cout << "measurement vector : distance" << std::endl ;
  for ( unsigned int i = 0 ; i < neighbors.size() ; ++i )
    {
    std::cout << "[" << tree->GetMeasurementVector( neighbors[i] )
              << "] : "  
              << distanceMetric->Evaluate( tree->GetMeasurementVector( neighbors[i])) << std::endl ;
    }    
  }
  
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
