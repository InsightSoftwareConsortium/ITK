/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularSphereMeshSourceTest.cxx
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

#include <iostream>

int itkRegularSphereMeshSourceTest(int, char* [] )
{

  typedef itk::Mesh<float, 3>   MeshType;

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
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    }

  std::cout << "mySphereMeshSource: " << mySphereMeshSource;
  
  MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType  pt;

  std::cout << "Testing itk::RegularSphereMeshSource "<< std::endl;

  for(unsigned int i=0; i<myMesh->GetNumberOfPoints(); i++) 
    {
    myMesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << std::endl;
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
      std::cout <<"Face " << faceId << " has " << cellPointer->GetNumberOfPoints() 
                <<" points" << std::endl;
      }

    ++cellsItr;
    ++faceId;
    }


  std::cout << "Test End "<< std::endl;

  return EXIT_SUCCESS;

}

