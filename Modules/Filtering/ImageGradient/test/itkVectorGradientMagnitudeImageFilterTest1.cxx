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

#include <fstream>
#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkRGBToVectorImageAdaptor.h"
#include "itkTestingMacros.h"

int
itkVectorGradientMagnitudeImageFilterTest1(int argc, char * argv[])
{
  if (argc != 7)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage"
              << " outputImage"
              << " useImageSpacing"
              << " derivativeWeightsValue"
              << " componentsWeightsValue"
              << " mode" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using RGBPixelType = itk::RGBPixel<unsigned short>;
  using CharImageType = itk::Image<unsigned char, Dimension>;
  using RGBImageType = itk::Image<RGBPixelType, Dimension>;

  // Create a reader and filter
  using ReaderType = itk::ImageFileReader<RGBImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using AdaptorType = itk::RGBToVectorImageAdaptor<RGBImageType>;
  auto adaptor = AdaptorType::New();
  adaptor->SetImage(reader->GetOutput());

  using FilterType = itk::VectorGradientMagnitudeImageFilter<AdaptorType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, VectorGradientMagnitudeImageFilter, ImageToImageFilter);


  filter->SetInput(adaptor);

  auto useImageSpacing = static_cast<bool>(std::stoi(argv[3]));
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  if (useImageSpacing)
  {
    filter->SetUseImageSpacingOn();
  }
  else
  {
    filter->SetUseImageSpacingOff();
  }
#endif
  ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);

  auto derivativeWeightsValue = static_cast<FilterType::DerivativeWeightsType::ValueType>(std::stod(argv[4]));
  FilterType::DerivativeWeightsType derivativeWeights;
  derivativeWeights.Fill(derivativeWeightsValue);
  filter->SetDerivativeWeights(derivativeWeights);
  ITK_TEST_SET_GET_VALUE(derivativeWeights, filter->GetDerivativeWeights());

  auto componentWeightsValue = static_cast<FilterType::ComponentWeightsType::ValueType>(std::stod(argv[5]));
  FilterType::ComponentWeightsType componentWeights;
  componentWeights.Fill(componentWeightsValue);
  filter->SetComponentWeights(componentWeights);
  ITK_TEST_SET_GET_VALUE(componentWeights, filter->GetComponentWeights());

  auto mode = static_cast<bool>(std::stoi(argv[6]));
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  if (mode)
  {
    filter->SetUsePrincipleComponentsOn();
  }
  else
  {
    filter->SetUsePrincipleComponentsOff();
  }
#endif
  ITK_TEST_SET_GET_BOOLEAN(filter, UsePrincipleComponents, mode);

  using RescaleFilterType = itk::RescaleIntensityImageFilter<FilterType::OutputImageType, CharImageType>;
  auto rescale = RescaleFilterType::New();
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);
  rescale->SetInput(filter->GetOutput());

  using WriterType = itk::ImageFileWriter<CharImageType>;
  auto writer = WriterType::New();
  writer->SetInput(rescale->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "The gradient image range was (low, high) = (" << rescale->GetInputMinimum() << ", "
            << rescale->GetInputMaximum() << ")" << std::endl;
  std::cout << "Output was scaled, shifted = " << rescale->GetScale() << ", " << rescale->GetShift() << std::endl;


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
