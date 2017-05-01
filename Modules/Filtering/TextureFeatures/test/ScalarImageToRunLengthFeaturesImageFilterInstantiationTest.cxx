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
#include "itkScalarImageToRunLengthFeaturesImageFilter.h"

#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"

int
ScalarImageToRunLengthFeaturesImageFilterInstantiationTest(int argc, char * argv[])
{
  // Setup types
  typedef itk::Image<int, 3>                                                                    InputImageType;
  typedef itk::Image<itk::Vector<float, 10>, 3>                                                 OutputImageType;
  typedef itk::ImageFileReader<InputImageType>                                                  readerType;
  typedef itk::Neighborhood<typename InputImageType::PixelType, InputImageType::ImageDimension> NeighborhoodType;
  NeighborhoodType                                                                              hood;

  // Create and setup a reader
  readerType::Pointer reader = readerType::New();
  std::string         inputFilename = argv[1];
  reader->SetFileName(inputFilename.c_str());

  // Create and setup a maskReader
  readerType::Pointer maskReader = readerType::New();
  std::string         maskFilename = argv[2];
  maskReader->SetFileName(maskFilename.c_str());

  // Apply the filter
  typedef itk::Statistics::ScalarImageToRunLengthFeaturesImageFilter<InputImageType, OutputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(filter, ScalarImageToRunLengthFeaturesImageFilter, ImageToImageFilter);

  hood.SetRadius(3);
  NeighborhoodType::OffsetType offset = { { -1, 0, 1 } };

  filter->SetInput(reader->GetOutput());
  filter->SetMaskImage(maskReader->GetOutput());
  filter->SetNumberOfBinsPerAxis(15);
  filter->SetPixelValueMinMax(-62, 2456);
  filter->SetDistanceValueMinMax(0.15, 1.5);
  filter->SetNeighborhoodRadius(hood.GetRadius());
  filter->SetInsidePixelValue(0);
  filter->SetOffset(offset);

  filter->UpdateLargestPossibleRegion();

  TEST_SET_GET_VALUE(reader->GetOutput(), filter->GetInput());
  TEST_SET_GET_VALUE(maskReader->GetOutput(), filter->GetMaskImage());
  TEST_SET_GET_VALUE(15, filter->GetNumberOfBinsPerAxis());
  TEST_SET_GET_VALUE(-62, filter->GetMin());
  TEST_SET_GET_VALUE(2456, filter->GetMax());
  TEST_SET_GET_VALUE(0.15, filter->GetMinDistance());
  TEST_SET_GET_VALUE(1.5, filter->GetMaxDistance());
  TEST_SET_GET_VALUE(hood.GetRadius(), filter->GetNeighborhoodRadius());
  TEST_SET_GET_VALUE(0, filter->GetInsidePixelValue());

  filter->UpdateLargestPossibleRegion();


  return EXIT_SUCCESS;
}
