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
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkStochasticFractalDimensionImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

namespace StochasticFractalDimensionImageFilterTest
{

template <unsigned int VDimension>
class Helper
{
public:
  static int
  Run(int argc, char * argv[])
  {
    using PixelType = float;
    using ImageType = itk::Image<PixelType, VDimension>;

    using ReaderType = itk::ImageFileReader<ImageType>;
    auto imageReader = ReaderType::New();
    imageReader->SetFileName(argv[2]);
    imageReader->Update();

    using FractalFilterType = itk::StochasticFractalDimensionImageFilter<ImageType>;
    auto fractalFilter = FractalFilterType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(fractalFilter, StochasticFractalDimensionImageFilter, ImageToImageFilter);


    fractalFilter->SetInput(imageReader->GetOutput());

    itk::SimpleFilterWatcher watcher(fractalFilter, "FractalDimensionFilter");

    typename FractalFilterType::RadiusType radius;

    radius.Fill(5);
    fractalFilter->SetNeighborhoodRadius(radius);
    ITK_TEST_SET_GET_VALUE(radius, fractalFilter->GetNeighborhoodRadius());

    radius.Fill(2);
    fractalFilter->SetNeighborhoodRadius(radius);
    ITK_TEST_SET_GET_VALUE(radius, fractalFilter->GetNeighborhoodRadius());

    if (argc > 4)
    {
      radius.Fill(std::stoi(argv[4]));
      fractalFilter->SetNeighborhoodRadius(radius);
    }

    if (argc > 5)
    {
      PixelType maskLabel = 1.0;
      if (argc > 6)
      {
        maskLabel = static_cast<PixelType>(std::stod(argv[6]));
      }

      auto labelImageReader = ReaderType::New();
      labelImageReader->SetFileName(argv[5]);
      labelImageReader->Update();

      using MaskImageType = typename FractalFilterType::MaskImageType;

      using ThresholderType = itk::BinaryThresholdImageFilter<ImageType, MaskImageType>;

      auto thresholder = ThresholderType::New();
      thresholder->SetInput(labelImageReader->GetOutput());
      thresholder->SetInsideValue(1);
      thresholder->SetOutsideValue(0);
      thresholder->SetLowerThreshold(maskLabel);
      thresholder->SetUpperThreshold(maskLabel);
      thresholder->Update();

      fractalFilter->SetMaskImage(thresholder->GetOutput());
    }

    itk::TimeProbe timer;

    timer.Start();
    std::cout << "/" << std::flush;

    ITK_TRY_EXPECT_NO_EXCEPTION(fractalFilter->Update());

    std::cout << "/" << std::flush;
    timer.Stop();

    std::cout << "   (elapsed time: " << timer.GetMean() << ")" << std::endl;

    using WriterType = itk::ImageFileWriter<ImageType>;
    auto writer = WriterType::New();
    writer->SetInput(fractalFilter->GetOutput());
    writer->SetFileName(argv[3]);
    writer->UseCompressionOn();

    ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


    return EXIT_SUCCESS;
  }
};

} // namespace StochasticFractalDimensionImageFilterTest

int
itkStochasticFractalDimensionImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cout << " imageDimension inputImage outputImage [radius] [labelImage] [label]" << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int imageDimension = std::stoi(argv[1]);

  switch (imageDimension)
  {
    case 2:
    {
      StochasticFractalDimensionImageFilterTest::Helper<2>::Run(argc, argv);
      break;
    }
    case 3:
    {
      StochasticFractalDimensionImageFilterTest::Helper<3>::Run(argc, argv);
      break;
    }
    default:
      std::cerr << "Image Dimension " << imageDimension;
      std::cerr << " is not supported by this test " << std::endl;
      return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
