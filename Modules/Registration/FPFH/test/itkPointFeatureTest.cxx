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

#include "itkPointFeature.h"
#include "itkPointSet.h"
#include "itkTestingMacros.h"

int
itkPointFeatureTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputImage";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  const char * outputImageFileName = argv[1];

  constexpr unsigned int Dimension = 3;
  using PixelType = float;
  using PointSetType = itk::PointSet<PixelType, Dimension>;

  using FilterType = itk::PointFeature<PointSetType, PointSetType>;
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, PointFeature, MeshToMeshFilter);

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
