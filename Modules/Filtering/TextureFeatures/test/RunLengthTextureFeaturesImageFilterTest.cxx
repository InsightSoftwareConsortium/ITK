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
#include "itkRunLengthTextureFeaturesImageFilter.h"

#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNeighborhood.h"
#include "itkTestingMacros.h"

int
RunLengthTextureFeaturesImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << " inputImageFile"
              << " maskImageFile"
              << " outputImageFile"
              << " [numberOfBinsPerAxis]"
              << " [pixelValueMin]"
              << " [pixelValueMax]"
              << " [minDistance]"
              << " [maxDistance]"
              << " [neighborhoodRadius]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 3;
  constexpr unsigned int VectorComponentDimension = 10;

  // Declare types
  using InputPixelType = float;
  using OutputPixelComponentType = float;
  using OutputPixelType = itk::Vector<OutputPixelComponentType, VectorComponentDimension>;

  using InputImageType = itk::Image<InputPixelType, ImageDimension>;
  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;
  using ReaderType = itk::ImageFileReader<InputImageType>;
  using NeighborhoodType = itk::Neighborhood<InputImageType::PixelType, InputImageType::ImageDimension>;

  // Create and set up a reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Create and set up a maskReader
  ReaderType::Pointer maskReader = ReaderType::New();
  maskReader->SetFileName(argv[2]);

  // Create the filter
  using FilterType =
    itk::Statistics::RunLengthTextureFeaturesImageFilter<InputImageType, OutputImageType, InputImageType>;

  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, RunLengthTextureFeaturesImageFilter, ImageToImageFilter);


  filter->SetInput(reader->GetOutput());
  filter->SetMaskImage(maskReader->GetOutput());
  ITK_TEST_SET_GET_VALUE(maskReader->GetOutput(), filter->GetMaskImage());

  if (argc >= 5)
  {
    unsigned int numberOfBinsPerAxis = std::stoi(argv[4]);
    filter->SetNumberOfBinsPerAxis(numberOfBinsPerAxis);
    ITK_TEST_SET_GET_VALUE(numberOfBinsPerAxis, filter->GetNumberOfBinsPerAxis());

    FilterType::PixelType pixelValueMin = std::stod(argv[5]);
    FilterType::PixelType pixelValueMax = std::stod(argv[6]);
    filter->SetHistogramValueMinimum(pixelValueMin);
    filter->SetHistogramValueMaximum(pixelValueMax);
    ITK_TEST_SET_GET_VALUE(pixelValueMin, filter->GetHistogramValueMinimum());
    ITK_TEST_SET_GET_VALUE(pixelValueMax, filter->GetHistogramValueMaximum());

    FilterType::RealType minDistance = std::stod(argv[7]);
    FilterType::RealType maxDistance = std::stod(argv[8]);
    filter->SetHistogramDistanceMinimum(minDistance);
    filter->SetHistogramDistanceMaximum(maxDistance);
    ITK_TEST_SET_GET_VALUE(minDistance, filter->GetHistogramDistanceMinimum());
    ITK_TEST_SET_GET_VALUE(maxDistance, filter->GetHistogramDistanceMaximum());

    NeighborhoodType::SizeValueType neighborhoodRadius = std::stoi(argv[9]);
    NeighborhoodType                hood;
    hood.SetRadius(neighborhoodRadius);
    filter->SetNeighborhoodRadius(hood.GetRadius());
    ITK_TEST_SET_GET_VALUE(hood.GetRadius(), filter->GetNeighborhoodRadius());
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Create and set up a writer
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(filter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
