/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimplexMeshToTriangleMeshFilterTest.cxx
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
#include "itkSimplexMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkSimplexMeshToTriangleMeshFilter.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkDefaultStaticMeshTraits.h"

int itkSimplexMeshToTriangleMeshFilterTest( int , char * [] )
{ 
  
  // Declare the type of the input and output mesh
  typedef itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double> TriangleMeshTraits;
  typedef itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double> SimplexMeshTraits;
  typedef itk::Mesh<double,3,TriangleMeshTraits> TriangleMeshType;
  typedef itk::SimplexMesh<double,3, SimplexMeshTraits> SimplexMeshType;


  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType>  SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType PointType;
  typedef SphereMeshSourceType::VectorType VectorType;

  // Declare the type of the gradient image
  typedef itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType>  SimplexFilterType;

  typedef itk::SimplexMeshToTriangleMeshFilter<SimplexMeshType,TriangleMeshType>  TriangleFilterType;
  typedef TriangleMeshType::Pointer                                               TriangleMeshPointer;
  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();
  PointType center; center.Fill(0);
  PointType::ValueType scaleInit[3] = {5,5,5};
  VectorType scale = scaleInit;
  
  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(1);
  mySphereMeshSource->SetScale(scale);

  SimplexFilterType::Pointer simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput( mySphereMeshSource->GetOutput() );
  
  TriangleFilterType::Pointer backFilter = TriangleFilterType::New();
  backFilter->SetInput( simplexFilter->GetOutput() );
  backFilter->Update();
  backFilter->Print(std::cout);

  SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  TriangleMeshPointer originalTriangleMesh = mySphereMeshSource->GetOutput();
 
  std::cout << "Original triangle mesh: " << std::endl;
  std::cout << originalTriangleMesh << std::endl;
 
  std::cout << "Simplex Mesh: " << simplexMesh << std::endl;
  TriangleMeshType::Pointer triangleMesh = backFilter->GetOutput();

  std::cout << "Back filtered Triangle Mesh: " << triangleMesh << std::endl;

  std::cout << "[TEST DONE]" << std::endl;
  return EXIT_SUCCESS;

}
