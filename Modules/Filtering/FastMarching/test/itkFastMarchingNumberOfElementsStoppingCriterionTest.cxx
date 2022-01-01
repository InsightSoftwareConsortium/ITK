/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkFastMarchingNumberOfElementsStoppingCriterion.h"
#include "itkTestingMacros.h"

int
itkFastMarchingNumberOfElementsStoppingCriterionTest(int, char *[])
{
  using ImageType = itk::Image<float, 2>;

  using ImageStoppingCriterionType = itk::FastMarchingNumberOfElementsStoppingCriterion<ImageType, ImageType>;

  auto image_criterion = ImageStoppingCriterionType::New();
  if (image_criterion.IsNull())
  {
    return EXIT_FAILURE;
  }

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    image_criterion, FastMarchingNumberOfElementsStoppingCriterion, FastMarchingStoppingCriterionBase);


  itk::IdentifierType targetNumberOfElements = 10;
  image_criterion->SetTargetNumberOfElements(targetNumberOfElements);
  ITK_TEST_SET_GET_VALUE(targetNumberOfElements, image_criterion->GetTargetNumberOfElements());

  using MeshType = itk::QuadEdgeMesh<float, 3>;

  using MeshStoppingCriterionType = itk::FastMarchingNumberOfElementsStoppingCriterion<MeshType, MeshType>;

  auto mesh_criterion = MeshStoppingCriterionType::New();
  if (mesh_criterion.IsNull())
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
