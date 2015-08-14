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

#include "itkWhitakerSparseLevelSetImage.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"
#include "itkMath.h"

int itkLevelSetEvolutionNumberOfIterationsStoppingCriterionTest( int , char* [] )
{
  const unsigned int Dimension = 2;
  typedef float ValueType;

  typedef itk::WhitakerSparseLevelSetImage< ValueType, Dimension > LevelSetType;

  typedef itk::LevelSetContainerBase< itk::IdentifierType, LevelSetType >
                                                            LevelSetContainerType;

  typedef itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion< LevelSetContainerType >
      StoppingCriterionType;
  StoppingCriterionType::Pointer criterion = StoppingCriterionType::New();
  criterion->SetNumberOfIterations( 5 );

  if( criterion->GetNumberOfIterations() != 5 )
    {
    return EXIT_FAILURE;
    }

  criterion->SetRMSChangeAccumulator( 0.1 );

  if( itk::Math::NotExactlyEquals(criterion->GetRMSChangeAccumulator(), 0.1) )
    {
    return EXIT_FAILURE;
    }

  for( StoppingCriterionType::IterationIdType iter = 0; iter < 10; iter++ )
    {
    criterion->SetCurrentIteration( iter );

    if( criterion->GetCurrentIteration() != iter )
      {
      return EXIT_FAILURE;
      }

    if( criterion->IsSatisfied() != ( iter >= 5 ) )
      {
      return EXIT_FAILURE;
      }
    }

  std::cout << "Description :" << criterion->GetDescription() <<std::endl;

  return EXIT_SUCCESS;
}
