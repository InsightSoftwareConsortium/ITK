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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkSimpleFilterWatcher.h"
#include "itkFFTShiftImageFilter.h"
#include "itkTestingMacros.h"

int
itkFFTShiftImageFilterTest(int argc, char * argv[])
{

  if (argc != 4)
  {
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage outputImage inverse" << std::endl;
    std::cerr << "  inputImage: The input image." << std::endl;
    std::cerr << "  outputImage: The output image." << std::endl;
    std::cerr << "  inverse: 0, to perform a forward transform, or 1 to perform" << std::endl;
    std::cerr << "           an inverse transform." << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int dim = 3;

  using PType = itk::RGBPixel<unsigned char>;
  using IType = itk::Image<PType, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using FilterType = itk::FFTShiftImageFilter<IType, IType>;
  auto filter = FilterType::New();
  // test default values
  ITK_TEST_EXPECT_TRUE(!filter->GetInverse());


  //
  // Tests for raising code coverage
  //
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  filter->SetInput(reader->GetOutput());

  auto inverse = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(filter, Inverse, inverse);


  itk::SimpleFilterWatcher watcher(filter, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
