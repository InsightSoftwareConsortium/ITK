/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKPolyDataWriterTest2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


#include "itkMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkDefaultStaticMeshTraits.h"
#include "itkVTKPolyDataWriter.h"

#include <iostream>

int itkVTKPolyDataWriterTest2( int argc, char * argv [] )
{
  if( argc != 2 )
    {
    std::cerr << "Usage: itkVTKPolyDataWriter outputFileName" << std::endl;
    return EXIT_FAILURE;
    }


  typedef itk::Mesh<float, 3>   MeshType;

  typedef itk::RegularSphereMeshSource< MeshType >  SphereMeshSourceType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();

  typedef SphereMeshSourceType::PointType   PointType;
  typedef SphereMeshSourceType::VectorType  VectorType;

  PointType center; 
  center.Fill( 7.4 );

  const double radius = 1.5;
  const double tolerance = 1e-5;

  VectorType scale;
  scale.Fill( radius );
  
  mySphereMeshSource->SetCenter( center );
  mySphereMeshSource->SetResolution( 1 );
  mySphereMeshSource->SetScale( scale );

  mySphereMeshSource->Modified();

  try
    {
    mySphereMeshSource->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    }

  std::cout << "mySphereMeshSource: " << mySphereMeshSource;
  
  MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType pt;
  pt.Fill( 0. );

  bool testPassed = true;

  std::cout << "Testing itk::RegularSphereMeshSource "<< std::endl;

  for( unsigned int i=0; i< myMesh->GetNumberOfPoints(); i++) 
    {
    myMesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << std::endl;

    const double distanceToCenter = pt.EuclideanDistanceTo( center );
 
    if( vnl_math_abs( distanceToCenter - radius ) > tolerance )
      {
      std::cerr << "Distance to center " << distanceToCenter;
      std::cerr << " is too different from radius " << radius << std::endl;
      testPassed = false;
      }
    }

  typedef MeshType::CellsContainerPointer  CellsContainerPointer;
  typedef MeshType::CellsContainerIterator CellsContainerIterator;
  typedef MeshType::CellType               CellType;

  CellsContainerPointer cells = myMesh->GetCells();

  unsigned faceId = 0;

  MeshType::CellsContainerIterator cellsItr = cells->Begin();


  while( cellsItr != cells->End() )
    {
    CellType * cellPointer = cellsItr.Value();

    if( cellPointer->GetType() != 1 )
      {
      const unsigned int numberOfPoints = cellPointer->GetNumberOfPoints();

      std::cout <<"Face " << faceId << " has " << numberOfPoints <<" points" << std::endl;

      if( numberOfPoints != 3 )
        {
        std::cerr << "Face with wrong number of points" << std::endl;
        testPassed = false;
        }
      }

    ++cellsItr;
    ++faceId;
    }


  typedef itk::VTKPolyDataWriter<MeshType>   WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( myMesh  );
  writer->SetFileName( argv[1] );
  writer->Write();


  if( !testPassed )
    {
    std::cout << "Test FAILED! "<< std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED! "<< std::endl;

  return EXIT_SUCCESS;

}
