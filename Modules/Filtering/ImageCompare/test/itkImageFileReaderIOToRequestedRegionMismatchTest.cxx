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

#include "itkSTAPLEImageFilter.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"


int
itkImageFileReaderIOToRequestedRegionMismatchTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << "file1 file2 ... fileN" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputImageType = itk::Image<unsigned short, Dimension>;
  using OutputImageType = itk::Image<double, Dimension>;

  using STAPLEImageFilterType = itk::STAPLEImageFilter<InputImageType, OutputImageType>;
  auto stapleImageFilter = STAPLEImageFilterType::New();

  typename itk::ImageFileReader<InputImageType>::Pointer reader;
  for (int i = 1; i < argc; ++i)
  {
    // Instantiate a new reader for each image
    reader = itk::ImageFileReader<InputImageType>::New();

    reader->SetFileName(argv[i]);
    stapleImageFilter->SetInput(itk::Math::CastWithRangeCheck<unsigned int>(i - 1), reader->GetOutput());
  }

  // Expect an image region to requested region size mismatch exception
  ITK_TRY_EXPECT_EXCEPTION(stapleImageFilter->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
