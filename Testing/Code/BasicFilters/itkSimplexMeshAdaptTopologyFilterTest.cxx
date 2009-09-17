/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimplexMeshAdaptTopologyFilterTest.cxx
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

#include "itkMesh.h"
#include "itkSimplexMesh.h"
#include "itkRegularSphereMeshSource.h"
#include <itkTriangleMeshToSimplexMeshFilter.h>
#include "itkSimplexMeshAdaptTopologyFilter.h"
#include "itkDefaultDynamicMeshTraits.h"

int itkSimplexMeshAdaptTopologyFilterTest( int , char * [] )
{ 

  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3,double,double> TriangleMeshTraits;
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3, double,double> SimplexMeshTraits;
  typedef itk::Mesh<double,3, TriangleMeshTraits> TriangleMeshType;
  typedef itk::SimplexMesh<double,3, SimplexMeshTraits> SimplexMeshType;

  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType>  SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType PointType;
  typedef SphereMeshSourceType::VectorType VectorType;

  // declare the triangle to simplex mesh filter
  typedef itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType> SimplexFilterType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center; 
  center.Fill(10);
  PointType::ValueType scaleInit[3] = {3,3,3};
  VectorType scale = scaleInit;
  
  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(2); 
  mySphereMeshSource->SetScale(scale);

  std::cout << "Triangle mesh created. " << std::endl;
  
  SimplexFilterType::Pointer simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput( mySphereMeshSource->GetOutput() );
  simplexFilter->Update();
  
  SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  simplexMesh->DisconnectPipeline();
  
  std::cout << "Simplex Mesh: " << simplexMesh << std::endl;

  typedef itk::SimplexMeshAdaptTopologyFilter< SimplexMeshType, SimplexMeshType > FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(simplexMesh);
  filter->Update();
  filter->Print(std::cout);

  std::cout << "[TEST DONE]" << std::endl;

  return EXIT_SUCCESS;

}
