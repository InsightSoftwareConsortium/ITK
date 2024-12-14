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

#include "itkNormalVariateGenerator.h"

int
itkNormalVariateGeneratorTest1(int, char *[])
{
  using NormalGeneratorType = itk::Statistics::NormalVariateGenerator;

  auto normalGenerator = NormalGeneratorType::New();

  normalGenerator->Initialize(101);

  std::cout << normalGenerator->GetNameOfClass() << '\n';

  normalGenerator->Print(std::cout);

  constexpr unsigned int numberOfSamples = 1000;

  double sum = 0.0;
  double sum2 = 0.0;

  for (unsigned int i = 0; i < numberOfSamples; ++i)
  {
    const double value = normalGenerator->GetVariate();
    sum += value;
    sum2 += value * value;
  }

  const double average = sum / numberOfSamples;

  std::cout << "Average = " << average << '\n';

  const double variance = sum2 / numberOfSamples - sum * sum;

  std::cout << "Variance = " << variance << '\n';

  //
  // FIXME: Add here numerical verification (regression testing)
  //


  std::cerr << "[PASSED]" << '\n';
  return EXIT_SUCCESS;
}
