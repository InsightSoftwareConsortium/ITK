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

#include <iostream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkLaplacianImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


inline std::ostream &
operator<<(std::ostream & o, const itk::Vector<float, 3> & v)
{
  o << "[" << v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int
itkLaplacianImageFilterTest(int argc, char * argv[])
{
  if (argc != 4)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName outputFileName useImageSpacing"
              << std::endl;
    return EXIT_FAILURE;
  }


  constexpr unsigned int Dimension = 2;

  using InputPixelType = float;
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputPixelType = unsigned char;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  // Assign a spacing other than [1,1] for testing
  itk::Vector<float, 2> spacing{ { { 0.5, 5.0 } } };
  reader->GetOutput()->SetSpacing(spacing);

  // Set up filter
  itk::LaplacianImageFilter<InputImageType, InputImageType>::Pointer filter =
    itk::LaplacianImageFilter<InputImageType, InputImageType>::New();

  auto useImageSpacing = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);

  itk::SimpleFilterWatcher watch(filter);

  // Run test
  itk::Size<Dimension> sz;
  sz[0] = 100;
  sz[1] = 100;
  itk::NullImageToImageFilterDriver<InputImageType, InputImageType> test1;
  test1.SetImageSize(sz);
  test1.SetFilter(filter);

  ITK_TRY_EXPECT_NO_EXCEPTION(test1.Execute());

  // The following code should throw an exception and not crash.
  filter->SetInput(nullptr);

  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  filter->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  using RescaleType = itk::RescaleIntensityImageFilter<InputImageType, OutputImageType>;
  auto rescaler = RescaleType::New();
  rescaler->SetInput(filter->GetOutput());
  rescaler->SetOutputMinimum(itk::NumericTraits<OutputPixelType>::min());
  rescaler->SetOutputMaximum(itk::NumericTraits<OutputPixelType>::max());

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(rescaler->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
