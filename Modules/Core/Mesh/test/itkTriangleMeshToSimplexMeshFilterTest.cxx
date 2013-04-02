/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>

#include "itkRegularSphereMeshSource.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkDefaultDynamicMeshTraits.h"

int itkTriangleMeshToSimplexMeshFilterTest(int , char * [] )
{
  // Declare the type of the input and output mesh
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3>   TriangleMeshTraits;
  typedef itk::DefaultDynamicMeshTraits<double, 3, 3>   SimplexMeshTraits;
  typedef itk::Mesh<double,3, TriangleMeshTraits>       TriangleMeshType;
  typedef itk::SimplexMesh<double,3, SimplexMeshTraits> SimplexMeshType;

  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType> SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType                PointType;
  typedef SphereMeshSourceType::VectorType               VectorType;

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
