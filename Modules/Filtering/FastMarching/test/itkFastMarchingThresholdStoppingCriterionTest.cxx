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

#include "itkFastMarchingThresholdStoppingCriterion.h"
#include "itkTestingMacros.h"

int
itkFastMarchingThresholdStoppingCriterionTest(int, char *[])
{
  using PixelType = float;
  constexpr unsigned int Dimension2D = 2;

  using ImageType = itk::Image<PixelType, Dimension2D>;

  using ImageStoppingCriterionType = itk::FastMarchingThresholdStoppingCriterion<ImageType, ImageType>;

  auto imageCriterion = ImageStoppingCriterionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    imageCriterion, FastMarchingThresholdStoppingCriterion, FastMarchingStoppingCriterionBase);


  std::cout << "Description: " << imageCriterion->GetDescription() << std::endl;

  constexpr unsigned int Dimension3D = 3;
  using MeshType = itk::QuadEdgeMesh<PixelType, Dimension3D>;

  using MeshStoppingCriterionType = itk::FastMarchingThresholdStoppingCriterion<MeshType, MeshType>;

  auto meshCriterion = MeshStoppingCriterionType::New();

  if (meshCriterion.IsNull())
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
