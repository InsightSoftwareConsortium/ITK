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

#include "itkMinimalStandardRandomVariateGenerator.h"

#include "itkTestingMacros.h"
#include "itkMath.h"

int
itkMinimalStandardRandomVariateGeneratorTest(int, char *[])
{
  typedef itk::Statistics::MinimalStandardRandomVariateGenerator GeneratorType;
  GeneratorType::Pointer                                         generator = GeneratorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(generator, MinimalStandardRandomVariateGenerator, RandomVariateGeneratorBase);

  generator->Initialize(324);

  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(generator->GetVariate(), 1.35581, 4, 0.0001));

  return EXIT_SUCCESS;
}
