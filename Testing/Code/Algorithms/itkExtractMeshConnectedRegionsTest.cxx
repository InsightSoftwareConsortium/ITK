/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractMeshConnectedRegionsTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMesh.h"
#include "itkConnectedRegionsMeshFilter.h"

#include <iostream>
#include <string>

/**
 * Some typedefs to make things easier.
 */

// A mesh with no pixel data.
typedef itk::Mesh<int>  Mesh;
typedef itk::ConnectedRegionsMeshFilter<Mesh,Mesh> Connect;
typedef itk::Point<float,3> Point;

/*
 * Test the mesh connectivity class.
 */
int itkExtractMeshConnectedRegionsTest(int, char* [])
{
  // Define a simple mesh of three connected pieces. The mesh consists
  // of several different cell types.
  //
  Mesh::Pointer inMesh(Mesh::New());
  Mesh::Pointer outMesh(Mesh::New());
  
  // Pass the mesh through the filter in a variety of ways.
  //
  Point::ValueType pInit[3] = {1,2,3};
  Point p = pInit;
  Connect::Pointer connect(Connect::New());
  connect->SetInput(inMesh);
  connect->SetClosestPoint(p);
  connect->AddSeed(0);
  connect->InitializeSeedList();
  connect->AddSeed(1);
  connect->AddSeed(2);
  connect->DeleteSeed(1);
  connect->Update();
  

  return 0;  
}

