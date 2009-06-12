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
#include "itkVTKPolyDataWriter.h"

#include <iostream>

int itkRegularSphereQuadEdgeMeshSourceTest(int argc, char * argv [] )
{
  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0] << " outputFileName.vtk" << std::endl;
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
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "mySphereMeshSource: " << mySphereMeshSource;
  
  MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType pt;
  pt.Fill( 0. );

  std::cout << "Testing itk::RegularSphereMeshSource "<< std::endl;

  for(unsigned int i=0; i<myMesh->GetNumberOfPoints(); i++) 
    {
    myMesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << std::endl;
    }

  typedef itk::VTKPolyDataWriter<MeshType>   WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( myMesh );
  writer->SetFileName( argv[1] );
  writer->Write();

  std::cout << "Test End "<< std::endl;

  return EXIT_SUCCESS;

}
