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
#include "itkCoocurrenceTextureFeaturesImageFilter.h"

#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"

int
CoocurrenceTextureFeaturesImageFilterInstantiationTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << " inputImageFile"
              << " maskImageFile" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 3;
  constexpr unsigned int VectorComponentDimension = 8;

  // Declare types
  using InputPixelType = int;
  using MaskPixelType = unsigned char;
  using OutputPixelComponentType = float;
  using OutputPixelType = itk::Vector<OutputPixelComponentType, VectorComponentDimension>;

  using InputImageType = itk::Image<InputPixelType, ImageDimension>;
  using MaskImageType = itk::Image<MaskPixelType, ImageDimension>;
  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<InputImageType>;
  using MaskReaderType = itk::ImageFileReader<MaskImageType>;
  using NeighborhoodType = itk::Neighborhood<InputImageType::PixelType, InputImageType::ImageDimension>;


  // Create and set up a reader
  ReaderType::Pointer reader = ReaderType::New();
  std::string         inputFilename = argv[1];
  reader->SetFileName(argv[1]);

  // Create and set up a maskReader
  MaskReaderType::Pointer maskReader = MaskReaderType::New();
  maskReader->SetFileName(argv[2]);

  // Create the filter
  using FilterType = itk::Statistics::CoocurrenceTextureFeaturesImageFilter<InputImageType, OutputImageType>;
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, CoocurrenceTextureFeaturesImageFilter, ImageToImageFilter);


  filter->SetInput(reader->GetOutput());

  filter->SetMaskImage(maskReader->GetOutput());
  ITK_TEST_SET_GET_VALUE(maskReader->GetOutput(), filter->GetMaskImage());

  unsigned int numberOfBinsPerAxis = 15;
  filter->SetNumberOfBinsPerAxis(numberOfBinsPerAxis);
  ITK_TEST_SET_GET_VALUE(numberOfBinsPerAxis, filter->GetNumberOfBinsPerAxis());


  FilterType::PixelType min = -62;
  FilterType::PixelType max = 2456;
  filter->SetHistogramMinimum(min);
  filter->SetHistogramMaximum(max);
  ITK_TEST_SET_GET_VALUE(min, filter->GetHistogramMinimum());
  ITK_TEST_SET_GET_VALUE(max, filter->GetHistogramMaximum());

  NeighborhoodType::SizeValueType neighborhoodRadius = 3;
  NeighborhoodType                hood;
  hood.SetRadius(neighborhoodRadius);
  filter->SetNeighborhoodRadius(hood.GetRadius());
  ITK_TEST_SET_GET_VALUE(hood.GetRadius(), filter->GetNeighborhoodRadius());

  FilterType::MaskPixelType insidePixelValue = 0;
  filter->SetInsidePixelValue(insidePixelValue);
  ITK_TEST_SET_GET_VALUE(insidePixelValue, filter->GetInsidePixelValue());

  FilterType::OffsetType            offset = { { -1, 0, 1 } };
  FilterType::OffsetVector::Pointer offsetVector = FilterType::OffsetVector::New();
  offsetVector->push_back(offset);
  filter->SetOffsets(offsetVector);
  ITK_TEST_SET_GET_VALUE(offsetVector, filter->GetOffsets());

  filter->SetOffsets(offsetVector);
  ITK_TEST_SET_GET_VALUE(offsetVector, filter->GetOffsets());


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
