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
#include "itkDefaultDynamicMeshTraits.h"
#include "itkDeformableSimplexMesh3DFilter.h"

int
itkSimplexMeshWithFloatCoordRepTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;

  using PixelType = float;
  using CoordRepType = float;
  using MeshTraits = itk::DefaultDynamicMeshTraits<PixelType, Dimension, Dimension, CoordRepType>;
  using MeshType = itk::SimplexMesh<PixelType, Dimension, MeshTraits>;
  using DeformType = itk::DeformableSimplexMesh3DFilter<MeshType, MeshType>;

  DeformType::Pointer deform = DeformType::New();
  deform->Print(std::cout);
  return EXIT_SUCCESS;
}
