/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegularSphereQuadEdgeMeshSourceTest.cxx
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

#include <iostream>

int itkRegularSphereQuadEdgeMeshSourceTest(int, char* [] )
{

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
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    }

  std::cout << "mySphereMeshSource: " << mySphereMeshSource;
  
  MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType  pt;

  std::cout << "Testing itk::RegularSphereMeshSource "<< std::endl;

  for(unsigned int i=0; i<myMesh->GetNumberOfPoints(); i++) 
    {
    bool dummy = myMesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << std::endl;
    }

  std::cout << "Test End "<< std::endl;

  return EXIT_SUCCESS;

}
