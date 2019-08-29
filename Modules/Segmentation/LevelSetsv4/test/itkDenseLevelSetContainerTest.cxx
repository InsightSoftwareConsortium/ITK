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

#include "itkLevelSetContainer.h"

int
itkDenseLevelSetContainerTest(int, char *[])
{
  constexpr unsigned int Dimension = 2;

  using ImageType = itk::Image<float, Dimension>;
  using LevelSetType = itk::LevelSetDenseImage<ImageType>;

  using NameType = std::string;

  using NamedLevelSetContainerType = itk::LevelSetContainer<NameType, LevelSetType>;

  NamedLevelSetContainerType::Pointer name_container = NamedLevelSetContainerType::New();

  name_container->AddLevelSet("Lung", LevelSetType::New());
  name_container->AddLevelSet("Heart", LevelSetType::New());

  if ((name_container->GetLevelSet("Vessel")).IsNotNull())
  {
    return EXIT_FAILURE;
  }

  if ((name_container->GetLevelSet("Heart")).IsNull())
  {
    return EXIT_FAILURE;
  }

  if (name_container->RemoveLevelSet("Lung") == false)
  {
    return EXIT_FAILURE;
  }

  using LevelSetContainerType = itk::LevelSetContainer<unsigned int, LevelSetType>;
  LevelSetContainerType::Pointer container = LevelSetContainerType::New();

  container->AddLevelSet(1, LevelSetType::New());
  container->AddLevelSet(3, LevelSetType::New());

  return EXIT_SUCCESS;
}
