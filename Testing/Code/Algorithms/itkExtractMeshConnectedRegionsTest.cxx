/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkExtractMeshConnectedRegionsTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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


/*
 * Test the mesh connectivity class.
 */
int main(void)
{
  // Define a simple mesh of three connected pieces. The mesh consists
  // of several different cell types.
  //
  Mesh::Pointer inMesh(Mesh::New());
  Mesh::Pointer outMesh(Mesh::New());
  
  // Pass the mesh through the filter in a variety of ways.
  //
  Connect::Pointer connect(Connect::New());
  connect->SetInput(inMesh);

  return 0;  
}

