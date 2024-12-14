/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkTriangleMeshToBinaryImageFilter.h"
#include "itkImageFileWriter.h"

int
itkTriangleMeshToBinaryImageFilterTest2(int argc, char * argv[])
{

  // Declare the type of the input and output mesh
  using TriangleMeshTraits = itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double>;
  using SimplexMeshTraits = itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double>;
  using TriangleMeshType = itk::Mesh<double, 3, TriangleMeshTraits>;
  using SimplexMeshType = itk::SimplexMesh<double, 3, SimplexMeshTraits>;

  using ImageType = itk::Image<unsigned char, 3>;
  // declare triangle mesh source
  using SphereMeshSourceType = itk::RegularSphereMeshSource<TriangleMeshType>;
  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  // Declare the type of the gradient image
  using SimplexFilterType = itk::TriangleMeshToSimplexMeshFilter<TriangleMeshType, SimplexMeshType>;

  using TriangleFilterType = itk::SimplexMeshToTriangleMeshFilter<SimplexMeshType, TriangleMeshType>;
  using TriangleMeshPointer = TriangleMeshType::Pointer;
  auto                 mySphereMeshSource = SphereMeshSourceType::New();
  auto                 center = itk::MakeFilled<PointType>(-5);
  PointType::ValueType scaleInit[3] = { 10, 10, 10 };
  const VectorType     scale = scaleInit;

  mySphereMeshSource->SetCenter(center);
  mySphereMeshSource->SetResolution(3);
  mySphereMeshSource->SetScale(scale);

  auto simplexFilter = SimplexFilterType::New();
  simplexFilter->SetInput(mySphereMeshSource->GetOutput());

  auto backFilter = TriangleFilterType::New();
  backFilter->SetInput(simplexFilter->GetOutput());
  backFilter->Update();

  const SimplexMeshType::Pointer simplexMesh = simplexFilter->GetOutput();
  const TriangleMeshPointer      originalTriangleMesh = mySphereMeshSource->GetOutput();

  std::cout << " Number of Points and Cells in Original Triangle Mesh" << '\n';
  std::cout << originalTriangleMesh->GetNumberOfPoints() << '\n';
  std::cout << originalTriangleMesh->GetNumberOfCells() << '\n';
  std::cout << "Original triangle mesh: " << '\n';
  std::cout << originalTriangleMesh << '\n';


  std::cout << "Simplex Mesh: " << simplexMesh << '\n';
  const TriangleMeshType::Pointer triangleMesh = backFilter->GetOutput();

  std::cout << " Number of Points and Cells in Back Filtered Triangle Mesh" << '\n';
  std::cout << triangleMesh->GetNumberOfPoints() << '\n';
  std::cout << triangleMesh->GetNumberOfCells() << '\n';

  std::cout << "Back filtered Triangle Mesh: " << triangleMesh << '\n';


  triangleMesh->DisconnectPipeline();

  using TriangleImageType = itk::TriangleMeshToBinaryImageFilter<TriangleMeshType, ImageType>;

  auto imageFilter = TriangleImageType::New();

  imageFilter->SetInput(triangleMesh);

  ImageType::SizeType size;

  size[0] = 100;
  size[1] = 100;
  size[2] = 100;
  imageFilter->SetSize(size);

  auto index = ImageType::IndexType::Filled(-50);
  imageFilter->SetIndex(index);

  // Testing PrintSelf
  std::cout << imageFilter << '\n';

  // Update the filter
  imageFilter->Update();

  if (argc > 1)
  {
    itk::WriteImage(imageFilter->GetOutput(), argv[1], true);
  }

  std::cout << "[TEST DONE]" << '\n';
  return EXIT_SUCCESS;
}
