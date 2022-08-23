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

#include "itkWarpMeshFilter.h"
#include "itkRegularSphereMeshSource.h"
#include "itkImage.h"
#include "itkTestingMacros.h"

int
itkWarpMeshFilterTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;

  // Declare the type of the input and output mesh
  using MeshTraits = itk::DefaultStaticMeshTraits<double, Dimension, Dimension, double, double, double>;
  using MeshType = itk::Mesh<double, Dimension, MeshTraits>;


  // declare triangle mesh source
  using SphereMeshSourceType = itk::RegularSphereMeshSource<MeshType>;
  using PointType = SphereMeshSourceType::PointType;
  using VectorType = SphereMeshSourceType::VectorType;

  auto sphereMeshSource = SphereMeshSourceType::New();

  PointType center;
  center[0] = 25.0;
  center[1] = 25.0;
  center[2] = 25.0;

  VectorType scale;
  scale[0] = 20.0;
  scale[1] = 20.0;
  scale[2] = 20.0;

  sphereMeshSource->SetCenter(center);
  sphereMeshSource->SetScale(scale);
  sphereMeshSource->SetResolution(2);


  // Declare the deformation field
  using VectorType = itk::Vector<double, Dimension>;
  using DisplacementFieldType = itk::Image<VectorType, Dimension>;

  auto deformationField = DisplacementFieldType::New();

  DisplacementFieldType::IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  DisplacementFieldType::SizeType size;
  size[0] = 25;
  size[1] = 25;
  size[2] = 25;

  DisplacementFieldType::RegionType region;
  region.SetSize(size);
  region.SetIndex(start);

  deformationField->SetRegions(region);

  DisplacementFieldType::PointType origin;
  origin[0] = 0.0;
  origin[1] = 0.0;
  origin[2] = 0.0;

  deformationField->SetOrigin(origin);

  DisplacementFieldType::SpacingType spacing;
  spacing[0] = 2.0;
  spacing[1] = 2.0;
  spacing[2] = 2.0;

  deformationField->SetSpacing(spacing);

  deformationField->Allocate();

  VectorType simpleVector; // set up an uniform vector field
  simpleVector[0] = 3.0;
  simpleVector[1] = 5.0;
  simpleVector[2] = 7.0;

  deformationField->FillBuffer(simpleVector);

  // Declare the Warping filter
  using WarpFilterType = itk::WarpMeshFilter<MeshType, MeshType, DisplacementFieldType>;

  auto warpFilter = WarpFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(warpFilter, WarpMeshFilter, MeshToMeshFilter);


  // Test exceptions
  ITK_TRY_EXPECT_EXCEPTION(warpFilter->Update());

  warpFilter->SetInput(sphereMeshSource->GetOutput());

  // Test exceptions
  ITK_TRY_EXPECT_EXCEPTION(warpFilter->Update());


  warpFilter->SetDisplacementField(deformationField);

  ITK_TRY_EXPECT_NO_EXCEPTION(warpFilter->Update());


  MeshType::Pointer      outputMesh = warpFilter->GetOutput();
  MeshType::ConstPointer inputMesh = warpFilter->GetInput();

  MeshType::PointsContainerPointer      outPoints = outputMesh->GetPoints();
  MeshType::PointsContainerConstPointer inPoints = inputMesh->GetPoints();

  MeshType::PointsContainer::ConstIterator inputPoint = inPoints->Begin();
  MeshType::PointsContainer::ConstIterator outputPoint = outPoints->Begin();

  MeshType::PointsContainer::ConstIterator lastInputPoint = inPoints->End();
  MeshType::PointsContainer::ConstIterator lastOutputPoint = outPoints->End();

  const double tolerance = 1e-8;

  while (inputPoint != lastInputPoint && outputPoint != lastOutputPoint)
  {
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      const double distance = outputPoint.Value()[i] - inputPoint.Value()[i];
      if (itk::Math::abs(distance - simpleVector[i]) > tolerance)
      {
        std::cerr << "Filter failed" << std::endl;
        std::cerr << "Expected displacement = " << simpleVector[i] << std::endl;
        std::cerr << "but got  = " << distance << std::endl;
        return EXIT_FAILURE;
      }
    }
    ++inputPoint;
    ++outputPoint;
  }

  return EXIT_SUCCESS;
}
