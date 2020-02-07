/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkUnsharpMaskImageFilter.h"
#include "itkTestingMacros.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


namespace
{
template <typename OutPixelType>
int
RunTest(int argc, char * argv[])
{
  constexpr unsigned int Dimension = 2;
  using InputImagePixelType = unsigned char;

  using InImageType = itk::Image<InputImagePixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<InImageType>;

  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  using OutImageType = itk::Image<OutPixelType, Dimension>;
  using WriterType = itk::ImageFileWriter<OutImageType>;

  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[3]);

  using FilterType = itk::UnsharpMaskImageFilter<InImageType, OutImageType>;
  typename FilterType::Pointer filter = FilterType::New();

  // this does not work from within a templated method (GCC gives an error)
  // ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, UnsharpMaskImageFilter, ImageToImageFilter);

  filter->SetInput(reader->GetOutput());

  if (argc > 4)
  {
    filter->SetAmount(std::stod(argv[4]));
  }
  if (argc > 5)
  {
    filter->SetSigma(std::stod(argv[5]));
  }
  if (argc > 6)
  {
    filter->SetThreshold(std::stod(argv[6]));
  }

  bool clamp = itk::NumericTraits<typename FilterType::OutputPixelType>::IsInteger;
  filter->SetClamp(clamp);
  ITK_TEST_SET_GET_VALUE(clamp, filter->GetClamp());

  if (clamp)
  {
    filter->ClampOn();
    ITK_TEST_SET_GET_VALUE(true, filter->GetClamp());
  }
  else
  {
    filter->ClampOff();
    ITK_TEST_SET_GET_VALUE(false, filter->GetClamp());
  }


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  writer->SetInput(filter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << std::endl << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
} // namespace

int
itkUnsharpMaskImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage:\n itkUnsharpMaskImageFilterTest";
    std::cerr << " float | uchar in.png out.nrrd [amount [sigma [threshold]]]" << std::endl;
    return EXIT_FAILURE;
  }

  int testStatus = EXIT_SUCCESS;

  if (!strcmp(argv[1], "float"))
  {
    testStatus = RunTest<float>(argc, argv);
  }
  else if (!strcmp(argv[1], "uchar"))
  {
    testStatus = RunTest<unsigned char>(argc, argv);
  }
  else
  {
    std::cerr << "Error" << std::endl;
    std::cerr << "Unable to run test with " << argv[1];
    std::cerr << " pixel type entered as input argument" << std::endl;
    std::cerr << "Expected float or uchar" << std::endl;
    std::cerr << "TEST FAILED!" << std::endl;
    testStatus = EXIT_FAILURE;
  }

  return testStatus;
}
