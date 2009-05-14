/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleMeshToSimplexMeshFilterTest.cxx
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
#pragma warning ( disable : 4503 )
#endif
 
#include <math.h>
#include <iostream>
#include <time.h>

#include "itkSimplexMesh.h"
#include "itkSimplexMeshGeometry.h"
#include "itkRegularSphereMeshSource.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkTimeProbe.h"

int itkTriangleMeshToSimplexMeshFilterTest(int , char * [] )
{  
  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3> TriangleMeshTraits;
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3> SimplexMeshTraits;
  typedef itk::Mesh<double,3, TriangleMeshTraits> TriangleMeshType;
  typedef itk::SimplexMesh<double,3, SimplexMeshTraits> SimplexMeshType;

  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType>  SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType PointType;
  typedef SphereMeshSourceType::VectorType VectorType;

  // declare the triangle to simplex mesh filter
  typedef itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType> SimplexFilterType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center; center.Fill(0);
  PointType::ValueType scaleInit[3] = {5,5,5};
  VectorType scale = scaleInit;
  
  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(0); 
  mySphereMeshSource->SetScale(scale);

  std::cout << "Triangle mesh created. " << std::endl;
  
  SimplexFilterType::Pointer simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput( mySphereMeshSource->GetOutput() );
  simplexFilter->Update();
  simplexFilter->Print(std::cout);
  
  SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  
  std::cout << "Simplex Mesh: " << simplexMesh << std::endl;

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;
}




