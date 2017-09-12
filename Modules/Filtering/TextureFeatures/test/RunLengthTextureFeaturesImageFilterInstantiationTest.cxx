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
#include "itkTestingMacros.h"

int
RunLengthTextureFeaturesImageFilterInstantiationTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << " inputImageFile"
              << " maskImageFile" << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int ImageDimension = 3;
  const unsigned int VectorComponentDimension = 10;

  // Declare types
  typedef int                                                             InputPixelType;
  typedef unsigned char                                                   MaskPixelType;
  typedef float                                                           OutputPixelComponentType;
  typedef itk::Vector<OutputPixelComponentType, VectorComponentDimension> OutputPixelType;

  typedef itk::Image<InputPixelType, ImageDimension>                                   InputImageType;
  typedef itk::Image<MaskPixelType, ImageDimension>                                    MaskImageType;
  typedef itk::Image<OutputPixelType, ImageDimension>                                  OutputImageType;
  typedef itk::ImageFileReader<InputImageType>                                         ReaderType;
  typedef itk::ImageFileReader<MaskImageType>                                          MaskReaderType;
  typedef itk::Neighborhood<InputImageType::PixelType, InputImageType::ImageDimension> NeighborhoodType;


  // Create and set up a reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Create and set up a maskReader
  MaskReaderType::Pointer maskReader = MaskReaderType::New();
  maskReader->SetFileName(argv[2]);

  // Create the filter
  typedef itk::Statistics::RunLengthTextureFeaturesImageFilter<InputImageType, OutputImageType, MaskImageType>
    FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(filter, RunLengthTextureFeaturesImageFilter, ImageToImageFilter);


  filter->SetInput(reader->GetOutput());

  filter->SetMaskImage(maskReader->GetOutput());
  TEST_SET_GET_VALUE(maskReader->GetOutput(), filter->GetMaskImage());

  unsigned int numberOfBinsPerAxis = 15;
  filter->SetNumberOfBinsPerAxis(numberOfBinsPerAxis);
  TEST_SET_GET_VALUE(numberOfBinsPerAxis, filter->GetNumberOfBinsPerAxis());

  FilterType::PixelType pixelValueMin = -62;
  FilterType::PixelType pixelValueMax = 2456;
  filter->SetHistogramValueMinimum(pixelValueMin);
  filter->SetHistogramValueMaximum(pixelValueMax);
  TEST_SET_GET_VALUE(pixelValueMin, filter->GetHistogramValueMinimum());
  TEST_SET_GET_VALUE(pixelValueMax, filter->GetHistogramValueMaximum());

  FilterType::RealType minDistance = 0.15;
  FilterType::RealType maxDistance = 1.5;
  filter->SetHistogramDistanceMinimum(minDistance);
  filter->SetHistogramDistanceMaximum(maxDistance);
  TEST_SET_GET_VALUE(minDistance, filter->GetHistogramDistanceMinimum());
  TEST_SET_GET_VALUE(maxDistance, filter->GetHistogramDistanceMaximum());

  NeighborhoodType::SizeValueType neighborhoodRadius = 3;
  NeighborhoodType                hood;
  hood.SetRadius(neighborhoodRadius);
  filter->SetNeighborhoodRadius(hood.GetRadius());
  TEST_SET_GET_VALUE(hood.GetRadius(), filter->GetNeighborhoodRadius());

  FilterType::MaskPixelType insidePixelValue = 0;
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
