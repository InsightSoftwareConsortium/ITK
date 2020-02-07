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
#include "itkSimpleFilterWatcher.h"
#include "itkIntensityWindowingImageFilter.h"
#include "itkMorphologicalWatershedImageFilter.h"
#include "itkLabelOverlayImageFilter.h"
#include "itkTestingMacros.h"


int
itkMorphologicalWatershedImageFilterTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImageFile"
              << " outputImageFile"
              << " markWatershedLine"
              << " fullyConnected"
              << " level"
              << " [ovelayOutput [alpha]]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using FilterType = itk::MorphologicalWatershedImageFilter<ImageType, ImageType>;
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, MorphologicalWatershedImageFilter, ImageToImageFilter);

  bool markWatershedLine = std::stoi(argv[3]);
  ITK_TEST_SET_GET_BOOLEAN(filter, MarkWatershedLine, markWatershedLine);

  bool fullyConnected = std::stoi(argv[4]);
  ITK_TEST_SET_GET_BOOLEAN(filter, FullyConnected, fullyConnected);

  auto level = static_cast<FilterType::InputImagePixelType>(std::stod(argv[5]));
  filter->SetLevel(level);
  ITK_TEST_SET_GET_VALUE(level, filter->GetLevel());


  filter->SetInput(reader->GetOutput());

  itk::SimpleFilterWatcher watcher(filter, "MorphologicalWatershedImageFilter");

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Rescale the output to have a better display
  using MaxCalculatorType = itk::MinimumMaximumImageCalculator<ImageType>;
  MaxCalculatorType::Pointer minMaxCalculator = MaxCalculatorType::New();
  minMaxCalculator->SetImage(filter->GetOutput());
  minMaxCalculator->Compute();

  using RescaleType = itk::IntensityWindowingImageFilter<ImageType, ImageType>;
  RescaleType::Pointer rescaler = RescaleType::New();
  rescaler->SetInput(filter->GetOutput());
  rescaler->SetWindowMinimum(itk::NumericTraits<PixelType>::ZeroValue());
  rescaler->SetWindowMaximum(minMaxCalculator->GetMaximum());
  rescaler->SetOutputMaximum(itk::NumericTraits<PixelType>::max());
  rescaler->SetOutputMinimum(itk::NumericTraits<PixelType>::ZeroValue());

  // Write output image
  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(rescaler->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  if (argc > 6)
  {
    using RGBPixelType = itk::RGBPixel<PixelType>;
    using RGBImageType = itk::Image<RGBPixelType, Dimension>;

    using OverlayType = itk::LabelOverlayImageFilter<ImageType, ImageType, RGBImageType>;

    OverlayType::Pointer overlay = OverlayType::New();
    overlay->SetInput(reader->GetOutput());
    overlay->SetLabelImage(filter->GetOutput());

    using RGBWriterType = itk::ImageFileWriter<RGBImageType>;
    RGBWriterType::Pointer rgbwriter = RGBWriterType::New();
    rgbwriter->SetInput(overlay->GetOutput());
    rgbwriter->SetFileName(argv[6]);

    if (argc > 7)
    {
      overlay->SetOpacity(std::stod(argv[7]));
    }

    ITK_TRY_EXPECT_NO_EXCEPTION(rgbwriter->Update());
  }

  std::cerr << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
