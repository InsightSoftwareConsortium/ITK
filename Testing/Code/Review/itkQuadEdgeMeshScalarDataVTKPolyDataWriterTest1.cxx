/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1.cxx
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


#include "itkQuadEdgeMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkDefaultStaticMeshTraits.h"
#include "itkQuadEdgeMeshScalarDataVTKPolyDataWriter.h"

#include <iostream>

int itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1( int argc, char * argv [] )
{
  if( argc < 2 )
    {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage" << std::endl;
    std::cerr << argv[0] << " outputFileName.vtk" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::QuadEdgeMesh<float, 3>   MeshType;

  typedef itk::RegularSphereMeshSource< MeshType >  SphereMeshSourceType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();

  typedef SphereMeshSourceType::PointType   PointType;
  typedef SphereMeshSourceType::VectorType  VectorType;

  PointType center; 
  center.Fill( 0.0 );

  VectorType scale;
  scale.Fill( 1.0 );
  
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
    std::cerr << "Error during source Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "mySphereMeshSource: " << mySphereMeshSource;
  
  MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType pt;
  pt.Fill( 0. );

  std::cout << "Testing itk::RegularSphereMeshSource "<< std::endl;

  myMesh->Print( std::cout );

  for( unsigned int i=0; i < myMesh->GetNumberOfPoints(); i++ )
    {
    myMesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << std::endl;
    }

  typedef MeshType::CellsContainerPointer  CellsContainerPointer;
  typedef MeshType::CellsContainerIterator CellsContainerIterator;
  typedef MeshType::CellType               CellType;

  CellsContainerPointer cells = myMesh->GetCells();

  unsigned faceId = 0;

  for( MeshType::CellsContainerIterator cells_it = cells->Begin();
       cells_it != cells->End();
       ++cells_it, faceId++ )
    {
    CellType* cellPointer = cells_it.Value();
    if( cellPointer->GetType() != 1 )
      {
      std::cout <<"Face " << faceId << " has " << cellPointer->GetNumberOfPoints() 
                <<" points" << std::endl;
      }
    }

  std::cout << "Test End "<< std::endl;

  typedef itk::QuadEdgeMeshScalarDataVTKPolyDataWriter< MeshType >   WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( mySphereMeshSource->GetOutput() );
  writer->SetFileName( argv[1] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during writer Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;

}
