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

#include "itkRegularSphereMeshSource.h"
#include "itkTriangleMeshToSimplexMeshFilter.h"
#include "itkSimplexMeshToTriangleMeshFilter.h"

int itkSimplexMeshToTriangleMeshFilterTest( int , char * [] )
{

  // Declare the type of the input and output mesh
  typedef itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double>
                                                        TriangleMeshTraits;
  typedef itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double>
                                                        SimplexMeshTraits;
  typedef itk::Mesh<double,3,TriangleMeshTraits>        TriangleMeshType;
  typedef itk::SimplexMesh<double,3, SimplexMeshTraits> SimplexMeshType;


  // declare triangle mesh source
  typedef itk::RegularSphereMeshSource<TriangleMeshType> SphereMeshSourceType;
  typedef SphereMeshSourceType::PointType                PointType;
  typedef SphereMeshSourceType::VectorType               VectorType;

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
  originalTriangleMesh = ITK_NULLPTR;
  simplexFilter = ITK_NULLPTR;
  return EXIT_SUCCESS;

}
