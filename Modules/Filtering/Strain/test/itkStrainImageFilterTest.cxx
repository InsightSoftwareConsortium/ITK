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

#include "itkStrainImageFilter.h"
#include "itkTestingMacros.h"

#include "ReadInDisplacements.h"
#include "WriteOutStrains.h"

int
itkStrainImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputDisplacementImage outputPrefix strainForm";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }


  const char * inputDisplacementImageFileName = argv[1];
  const char * outputFileNamePrefix = argv[2];

  constexpr unsigned int Dimension = 2;
  using PixelType = float;
  using DisplacementVectorType = itk::Vector<PixelType, Dimension>;
  using InputImageType = itk::Image<DisplacementVectorType, Dimension>;

  using StrainFilterType = itk::StrainImageFilter<InputImageType, PixelType, PixelType>;
  using TensorImageType = StrainFilterType::OutputImageType;

  StrainFilterType::Pointer strainFilter = StrainFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(strainFilter, StrainImageFilter, ImageToImageFilter);


  InputImageType::Pointer inputDisplacements;
  if (ReadInDisplacements<InputImageType>(inputDisplacementImageFileName, inputDisplacements) == EXIT_FAILURE)
  {
    std::cerr << "Test failed!" << std::endl;
    return EXIT_FAILURE;
  }

  strainFilter->SetInput(inputDisplacements);


  // Test the unknown strain form exception
  int strainForm = -1;

  strainFilter->SetStrainForm(static_cast<StrainFilterType::StrainFormType>(strainForm));

  ITK_TRY_EXPECT_EXCEPTION(strainFilter->Update());


  // Get the input strain form
  if (!strcmp(argv[3], "INFINITESIMAL"))
  {
    strainForm = 0;
  }
  else if (!strcmp(argv[3], "GREENLAGRANGIAN"))
  {
    strainForm = 1;
  }
  else if (!strcmp(argv[3], "EULERIANALMANSI"))
  {
    strainForm = 2;
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Unknown strain form: " << argv[3] << std::endl;
    return EXIT_FAILURE;
  }

  strainFilter->SetStrainForm(static_cast<StrainFilterType::StrainFormType>(strainForm));
  ITK_TEST_SET_GET_VALUE(static_cast<StrainFilterType::StrainFormType>(strainForm), strainFilter->GetStrainForm());

  ITK_TRY_EXPECT_NO_EXCEPTION(strainFilter->Update());


  if (WriteOutStrains<PixelType, Dimension, TensorImageType>(outputFileNamePrefix, strainFilter->GetOutput()) ==
      EXIT_FAILURE)
  {
    std::cerr << "Test failed!" << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
