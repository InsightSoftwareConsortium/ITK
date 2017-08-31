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

  const unsigned int ImageDimension = 3;
  const unsigned int VectorComponentDimension = 8;

  // Declare types
  typedef int                                                             InputPixelType;
  typedef float                                                           OutputPixelComponentType;
  typedef itk::Vector<OutputPixelComponentType, VectorComponentDimension> OutputPixelType;

  typedef itk::Image<InputPixelType, ImageDimension>                                   InputImageType;
  typedef itk::Image<OutputPixelType, ImageDimension>                                  OutputImageType;
  typedef itk::ImageFileReader<InputImageType>                                         ReaderType;
  typedef itk::Neighborhood<InputImageType::PixelType, InputImageType::ImageDimension> NeighborhoodType;


  // Create and set up a reader
  ReaderType::Pointer reader = ReaderType::New();
  std::string         inputFilename = argv[1];
  reader->SetFileName(argv[1]);

  // Create and set up a maskReader
  ReaderType::Pointer maskReader = ReaderType::New();
  maskReader->SetFileName(argv[2]);

  // Create the filter
  typedef itk::Statistics::CoocurrenceTextureFeaturesImageFilter<InputImageType, OutputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(filter, CoocurrenceTextureFeaturesImageFilter, ImageToImageFilter);


  filter->SetInput(reader->GetOutput());

  filter->SetMaskImage(maskReader->GetOutput());
  TEST_SET_GET_VALUE(maskReader->GetOutput(), filter->GetMaskImage());

  unsigned int numberOfBinsPerAxis = 15;
  filter->SetNumberOfBinsPerAxis(numberOfBinsPerAxis);
  TEST_SET_GET_VALUE(numberOfBinsPerAxis, filter->GetNumberOfBinsPerAxis());


  FilterType::PixelType min = -62;
  FilterType::PixelType max = 2456;
  filter->SetHistogramMinimum(min);
  filter->SetHistogramMaximum(max);
  TEST_SET_GET_VALUE(min, filter->GetHistogramMinimum());
  TEST_SET_GET_VALUE(max, filter->GetHistogramMaximum());

  NeighborhoodType::SizeValueType neighborhoodRadius = 3;
  NeighborhoodType                hood;
  hood.SetRadius(neighborhoodRadius);
  filter->SetNeighborhoodRadius(hood.GetRadius());
  TEST_SET_GET_VALUE(hood.GetRadius(), filter->GetNeighborhoodRadius());

  FilterType::PixelType insidePixelValue = 0;
  filter->SetInsidePixelValue(insidePixelValue);
  TEST_SET_GET_VALUE(insidePixelValue, filter->GetInsidePixelValue());

  FilterType::OffsetType            offset = { { -1, 0, 1 } };
  FilterType::OffsetVector::Pointer offsetVector = FilterType::OffsetVector::New();
  offsetVector->push_back(offset);
  filter->SetOffsets(offsetVector);
  TEST_SET_GET_VALUE(offsetVector, filter->GetOffsets());

  filter->SetOffsets(offsetVector);
  TEST_SET_GET_VALUE(offsetVector, filter->GetOffsets());


  TRY_EXPECT_NO_EXCEPTION(filter->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
