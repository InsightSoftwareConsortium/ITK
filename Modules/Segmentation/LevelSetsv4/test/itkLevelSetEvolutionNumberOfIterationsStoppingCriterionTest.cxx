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

#include "itkWhitakerSparseLevelSetImage.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

int
itkLevelSetEvolutionNumberOfIterationsStoppingCriterionTest(int, char *[])
{
  constexpr unsigned int Dimension = 2;
  using ValueType = float;

  using LevelSetType = itk::WhitakerSparseLevelSetImage<ValueType, Dimension>;

  using LevelSetContainerType = itk::LevelSetContainerBase<itk::IdentifierType, LevelSetType>;

  using StoppingCriterionType = itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion<LevelSetContainerType>;
  auto criterion = StoppingCriterionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    criterion, LevelSetEvolutionNumberOfIterationsStoppingCriterion, LevelSetEvolutionStoppingCriterion);

  constexpr typename StoppingCriterionType::IterationIdType numberOfIterations = 5;
  criterion->SetNumberOfIterations(numberOfIterations);
  ITK_TEST_SET_GET_VALUE(numberOfIterations, criterion->GetNumberOfIterations());

  constexpr typename StoppingCriterionType::OutputRealType rmsChangeAccumulator = 0.1;
  criterion->SetRMSChangeAccumulator(rmsChangeAccumulator);
  ITK_TEST_SET_GET_VALUE(rmsChangeAccumulator, criterion->GetRMSChangeAccumulator());

  for (StoppingCriterionType::IterationIdType iter = 0; iter < 10; ++iter)
  {
    criterion->SetCurrentIteration(iter);

    if (criterion->GetCurrentIteration() != iter)
    {
      return EXIT_FAILURE;
    }

    if (criterion->IsSatisfied() != (iter >= 5))
    {
      return EXIT_FAILURE;
    }
  }

  std::cout << "Description :" << criterion->GetDescription() << std::endl;

  return EXIT_SUCCESS;
}
