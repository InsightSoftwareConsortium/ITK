/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkImage.h"
#include "itkFFTNormalizedCorrelationImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkFFTNormalizedCorrelationImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv)
              << " fixedImageName movingImageName outputImageName [requiredFractionOfOverlappingPixels]" << std::endl;
    return EXIT_FAILURE;
  }

  using InputImageType = itk::Image<unsigned short, 2>;
  using OutputImageType = itk::Image<unsigned char, 2>;
  // We need the internal type to be either float or double since
  // the correlation image contains values between -1 and 1.
  using RealImageType = itk::Image<double, 2>;
  using FilterType = itk::FFTNormalizedCorrelationImageFilter<InputImageType, RealImageType>;

  char *                    fixedImageFileName = argv[1];
  char *                    movingImageFileName = argv[2];
  const char *              outputImageFileName = argv[3];
  FilterType::SizeValueType requiredNumberOfOverlappingPixels = 0;
  FilterType::RealPixelType requiredFractionOfOverlappingPixels = 0;
  if (argc > 4)
  {
    requiredFractionOfOverlappingPixels = std::stod(argv[4]);
  }

  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer fixedImageReader = ReaderType::New();
  fixedImageReader->SetFileName(fixedImageFileName);

  ReaderType::Pointer movingImageReader = ReaderType::New();
  movingImageReader->SetFileName(movingImageFileName);

  FilterType::Pointer filter = FilterType::New();
  filter->SetFixedImage(fixedImageReader->GetOutput());
  filter->SetMovingImage(movingImageReader->GetOutput());
  // Larger values zero-out pixels on a larger border around the correlation image.
  // Thus, larger values remove less stable computations but also limit the capture range.
  filter->SetRequiredNumberOfOverlappingPixels(requiredNumberOfOverlappingPixels);
  filter->SetRequiredFractionOfOverlappingPixels(requiredFractionOfOverlappingPixels);

  itk::SimpleFilterWatcher watcher(filter, "FFTNormalizedCorrelation");

  // Shift the correlation values so they can be written out as a png.
  // The original range is [-1,1], and the new range is [0,255].
  // Shift is computed before scale, so we shift by 1 and then scale by 255/2.
  // The values very close to 0 in the correlation map are not exactly zero
  // because of precision issues.
  // In the shift/scale process, 0 gets mapped to 127.5.  If it were later rounded, it
  // would become 128.
  // But because of precision issues, numbers that are very close to 0 will get
  // mapped to 127 or 128, depending on whether they are slightly negative or positive.
  // Therefore, we truncate instead so that all values near 0 get mapped to 127.
  using RescaleType = itk::ShiftScaleImageFilter<RealImageType, OutputImageType>;
  RescaleType::Pointer rescaler = RescaleType::New();
  rescaler->SetInput(filter->GetOutput());
  rescaler->SetShift(1);
  rescaler->SetScale(255.0 / 2.0);

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(outputImageFileName);
  writer->SetInput(rescaler->GetOutput());
  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & excep)
  {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
  }

  std::cout << "Maximum overlapping pixels: " << filter->GetMaximumNumberOfOverlappingPixels() << std::endl;
  std::cout << "Required fraction of overlapping pixels: " << filter->GetRequiredFractionOfOverlappingPixels()
            << std::endl;
  std::cout << "Required number of overlapping pixels: " << filter->GetRequiredNumberOfOverlappingPixels() << std::endl;

  return EXIT_SUCCESS;
}
