/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractMeshConnectedRegionsTest.cxx
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
#include "itkMesh.h"
#include "itkConnectedRegionsMeshFilter.h"
#include "itkSphereMeshSource.h"

#include <iostream>
#include <string>


/*
 * Test the mesh connectivity class.
 */
int itkExtractMeshConnectedRegionsTest(int, char* [])
{

  /**
   * Some typedefs to make things easier.
   */

  // A mesh with no pixel data.
  typedef itk::Mesh< int >  MeshType;

  typedef itk::ConnectedRegionsMeshFilter<MeshType,MeshType> ConnectFilterType;
  typedef itk::Point<float,3> PointType;



  // Define a simple mesh of three connected pieces. The mesh consists
  // of several different cell types.
  //
  MeshType::Pointer inMesh  = MeshType::New();
  MeshType::Pointer outMesh = MeshType::New();
  
  // Pass the mesh through the filter in a variety of ways.
  //
  PointType::ValueType pInit[3] = {1,2,3};
  PointType p = pInit;
  ConnectFilterType::Pointer connect(ConnectFilterType::New());

  connect->SetInput(inMesh);
  connect->SetClosestPoint(p);
  connect->AddSeed(0);
  connect->InitializeSeedList();
  connect->AddSeed(1);
  connect->AddSeed(2);
  connect->DeleteSeed(1);
  connect->Update();


  // Create a Sphere for running the filter on real input data.  
  typedef itk::SphereMeshSource< MeshType >  SphereMeshSourceType;

  SphereMeshSourceType::Pointer meshSource = SphereMeshSourceType::New();

  PointType center; center.Fill(0);
  PointType::ValueType scaleInit[3] = {1,1,1};
  PointType scale = scaleInit;
  
  meshSource->SetCenter(center);
  meshSource->SetResolutionX( 10 );
  meshSource->SetResolutionY( 10 );
  meshSource->SetScale(scale);
  meshSource->Modified();
  meshSource->Update();


  connect->SetInput( meshSource->GetOutput() );

  try
    {
    connect->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;  
}

