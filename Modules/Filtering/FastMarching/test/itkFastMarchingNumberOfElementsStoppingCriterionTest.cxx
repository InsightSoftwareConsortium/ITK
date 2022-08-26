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

#include "itkFastMarchingNumberOfElementsStoppingCriterion.h"
#include "itkTestingMacros.h"

int
itkFastMarchingNumberOfElementsStoppingCriterionTest(int, char *[])
{
  using ImageType = itk::Image<float, 2>;

  using ImageStoppingCriterionType = itk::FastMarchingNumberOfElementsStoppingCriterion<ImageType, ImageType>;

  auto imageCriterion = ImageStoppingCriterionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    imageCriterion, FastMarchingNumberOfElementsStoppingCriterion, FastMarchingStoppingCriterionBase);


  itk::IdentifierType targetNumberOfElements = 10;
  imageCriterion->SetTargetNumberOfElements(targetNumberOfElements);
  ITK_TEST_SET_GET_VALUE(targetNumberOfElements, imageCriterion->GetTargetNumberOfElements());

  std::cout << "Description: " << imageCriterion->GetDescription() << std::endl;

  using MeshType = itk::QuadEdgeMesh<float, 3>;

  using MeshStoppingCriterionType = itk::FastMarchingNumberOfElementsStoppingCriterion<MeshType, MeshType>;

  auto meshCriterion = MeshStoppingCriterionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    meshCriterion, FastMarchingNumberOfElementsStoppingCriterion, FastMarchingStoppingCriterionBase);


  targetNumberOfElements = 8;
  meshCriterion->SetTargetNumberOfElements(targetNumberOfElements);
  ITK_TEST_SET_GET_VALUE(targetNumberOfElements, meshCriterion->GetTargetNumberOfElements());

  std::cout << "Description: " << meshCriterion->GetDescription() << std::endl;


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
