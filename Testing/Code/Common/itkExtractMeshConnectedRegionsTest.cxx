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
#include <iostream>
#include <string>

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkMesh.h"
#include "itkExtractMeshConnectedRegions.h"

/**
 * Some typedefs to make things easier.
 */

// A mesh with no pixel data.
typedef itk::Mesh<int>  Mesh;
typedef itk::ExtractMeshConnectedRegions<Mesh,Mesh> Connect;


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
  Connect::Pointer connect;
  connect->SetInput(inMesh);

  return 0;  
}

