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

#include "itkWatershedImageFilter.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkMacro.h"
#include "itkTestingMacros.h"
#include <iostream>
/// Uncovered a heap error or something by putting in bad values to WatershedImageFilter
/// This tests an added check in itkWaterhedSegmentTreeGenerator
/// At the time of this test, it's hard to predict what inputs are going to put the WatershedImageFilter in exception
/// state.


int
itkWatershedImageFilterBadValuesTest(int argc, char * argv[])
{
  int result = 0;

  if (argc < 2)
  {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImageFile" << std::endl;
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  char * path = argv[1];
  std::cout << "The file path is: " << path << std::endl;

  using ImageType = itk::Image<float, 2>;
  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetFileName(path);
  auto img = reader->GetOutput();

  auto watershed = itk::WatershedImageFilter<ImageType>::New();
  watershed->SetInput(img);

  // this should work
  watershed->SetLevel(0.5);
  watershed->SetThreshold(0.25);
  ITK_TRY_EXPECT_NO_EXCEPTION(watershed->Update());

  // this should cause an exception from itkWatershedSegmentTreeGenerator::CompileMergeList
  // this was previously causing an assert error, on Windows, (which would result in a non-zero exit code as the
  // application hard crashes) if this test fails to throw an exception, then maybe a fix has been added to increase the
  // stability of the algorithm
  watershed->SetLevel(0.45886916616845724);
  watershed->SetThreshold(0.7697413395336108);
  ITK_TRY_EXPECT_EXCEPTION(watershed->Update());

  return result;
}
