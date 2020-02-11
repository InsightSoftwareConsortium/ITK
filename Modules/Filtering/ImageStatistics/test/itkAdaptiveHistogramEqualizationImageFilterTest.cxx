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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkAdaptiveHistogramEqualizationImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkAdaptiveHistogramEqualizationImageFilterTest(int argc, char * argv[])

{
  if (argc < 6)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << "  inputImageFile  outputImageFile radius alpha beta"
              << std::endl;
    return EXIT_FAILURE;
  }


  using InputPixelType = float;
  static constexpr int ImageDimension = 2;

  using InputImageType = itk::Image<InputPixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<InputImageType>;
  using FilterType = itk::AdaptiveHistogramEqualizationImageFilter<InputImageType>;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  FilterType::ImageSizeType radius;
  radius.Fill(std::stoi(argv[3]));

  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, AdaptiveHistogramEqualizationImageFilter, MovingHistogramImageFilter);

  itk::SimpleFilterWatcher watcher(filter);

  filter->SetInput(reader->GetOutput());
  filter->SetRadius(radius);

  float alpha = std::stod(argv[4]);
  filter->SetAlpha(alpha);
  ITK_TEST_SET_GET_VALUE(alpha, filter->GetAlpha());

  float beta = std::stod(argv[5]);
  filter->SetBeta(beta);
  ITK_TEST_SET_GET_VALUE(beta, filter->GetBeta());

  //
  //  The output of the filter is connected here to a intensity rescaler filter
  //  and then to a writer. Invoking \code{Update()} on the writer triggers the
  //  execution of both filters.
  //

  using WritePixelType = unsigned char;

  using WriteImageType = itk::Image<WritePixelType, ImageDimension>;

  using RescaleFilterType = itk::RescaleIntensityImageFilter<InputImageType, WriteImageType>;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);
  rescaler->SetInput(filter->GetOutput());


  using WriterType = itk::ImageFileWriter<WriteImageType>;

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);

  writer->SetInput(rescaler->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
