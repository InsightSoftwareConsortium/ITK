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

int
itkFastMarchingNumberOfElementsStoppingCriterionTest(int, char *[])
{
  using ImageType = itk::Image<float, 2>;

  using ImageStoppingCriterionType = itk::FastMarchingNumberOfElementsStoppingCriterion<ImageType, ImageType>;

  ImageStoppingCriterionType::Pointer image_criterion = ImageStoppingCriterionType::New();
  if (image_criterion.IsNull())
  {
    return EXIT_FAILURE;
  }

  using MeshType = itk::QuadEdgeMesh<float, 3>;

  using MeshStoppingCriterionType = itk::FastMarchingNumberOfElementsStoppingCriterion<MeshType, MeshType>;

  MeshStoppingCriterionType::Pointer mesh_criterion = MeshStoppingCriterionType::New();
  if (mesh_criterion.IsNull())
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
