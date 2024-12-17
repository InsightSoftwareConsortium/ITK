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
#include "itkRescaleIntensityImageFilter.h"
#include "itkDiscreteGaussianDerivativeImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


/** Calculate the Gaussian derivatives at non-zero points of a Gaussian
 * input image. For derivative calculation the class
 * itkDiscreteGaussianDerivativeImageFilter is used.
 * This example operates on 2D images.
 */
int
itkDiscreteGaussianDerivativeImageFilterTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << "inputFileName"
                 " outputFileName"
                 " orderX"
                 " orderY"
                 " sigma"
                 " [maximumError]"
                 " [maximumKernelWidth]"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = float;
  using OutputPixelType = unsigned short;

  using ImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  using DerivativeFilterType = itk::DiscreteGaussianDerivativeImageFilter<ImageType, ImageType>;
  auto derivativeFilter = DerivativeFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(derivativeFilter, DiscreteGaussianDerivativeImageFilter, ImageToImageFilter);


  const itk::SimpleFilterWatcher watcher(derivativeFilter, "DiscreteGaussianDerivativeImageFilter");

  derivativeFilter->SetInput(reader->GetOutput());

  // Now proceed to apply the Gaussian derivative filter in both directions

  DerivativeFilterType::OrderArrayType order;
  order[0] = std::stoi(argv[3]);
  order[1] = std::stoi(argv[4]);

  derivativeFilter->SetOrder(order);
  ITK_TEST_SET_GET_VALUE(order, derivativeFilter->GetOrder());

  const double sigma = std::stod(argv[5]);

  DerivativeFilterType::ArrayType::ValueType maxErrorVal = 0.001;
  int                                        maxKernelWidth = 100;

  if (argc > 7)
  {
    maxErrorVal = static_cast<DerivativeFilterType::ArrayType::ValueType>(std::stod(argv[6]));
  }
  else if (argc > 8)
  {
    maxKernelWidth = std::stoi(argv[7]);
  }

  auto variance = itk::MakeFilled<DerivativeFilterType::ArrayType>(sigma * sigma);

  derivativeFilter->SetVariance(variance);
  ITK_TEST_SET_GET_VALUE(variance, derivativeFilter->GetVariance());

  auto maxError = itk::MakeFilled<DerivativeFilterType::ArrayType>(maxErrorVal);

  derivativeFilter->SetMaximumError(maxErrorVal);
  ITK_TEST_SET_GET_VALUE(maxError, derivativeFilter->GetMaximumError());

  derivativeFilter->SetMaximumKernelWidth(maxKernelWidth);
  ITK_TEST_SET_GET_VALUE(maxKernelWidth, derivativeFilter->GetMaximumKernelWidth());

  constexpr bool useImageSpacing = true;
  ITK_TEST_SET_GET_BOOLEAN(derivativeFilter, UseImageSpacing, useImageSpacing);

  constexpr bool normalizeAcrossScale = false;
  ITK_TEST_SET_GET_BOOLEAN(derivativeFilter, NormalizeAcrossScale, normalizeAcrossScale);

  constexpr unsigned int internalNumberOfStreamDivisions = DerivativeFilterType::InputImageType::GetImageDimension() *
                                                           DerivativeFilterType::InputImageType::GetImageDimension();
  derivativeFilter->SetInternalNumberOfStreamDivisions(internalNumberOfStreamDivisions);
  ITK_TEST_SET_GET_VALUE(internalNumberOfStreamDivisions, derivativeFilter->GetInternalNumberOfStreamDivisions());


  using RescaleFilterType = itk::RescaleIntensityImageFilter<ImageType, OutputImageType>;
  auto rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(itk::NumericTraits<OutputPixelType>::min());
  rescaler->SetOutputMaximum(itk::NumericTraits<OutputPixelType>::max());
  rescaler->SetInput(derivativeFilter->GetOutput());


  // Write the output image
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(rescaler->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
