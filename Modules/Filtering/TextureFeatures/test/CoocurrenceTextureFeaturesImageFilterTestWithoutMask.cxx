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
#include "itkImageFileWriter.h"
#include "itkNeighborhood.h"
#include "itkTestingMacros.h"

int
CoocurrenceTextureFeaturesImageFilterTestWithoutMask(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << " inputImageFile"
              << " outputImageFile"
              << " [numberOfBinsPerAxis]"
              << " [pixelValueMin]"
              << " [pixelValueMax]"
              << " [neighborhoodRadius]" << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int ImageDimension = 3;
  const unsigned int VectorComponentDimension = 8;

  // Declare types
  typedef int                                                             InputPixelType;
  typedef float                                                           OutputPixelComponentType;
  typedef itk::Vector<OutputPixelComponentType, VectorComponentDimension> OutputPixelType;

  typedef itk::Image<InputPixelType, ImageDimension>                                            InputImageType;
  typedef itk::Image<OutputPixelType, ImageDimension>                                           OutputImageType;
  typedef itk::ImageFileReader<InputImageType>                                                  ReaderType;
  typedef itk::Neighborhood<typename InputImageType::PixelType, InputImageType::ImageDimension> NeighborhoodType;

  // Create and set up a reader
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Create the filter
  typedef itk::Statistics::CoocurrenceTextureFeaturesImageFilter<InputImageType, OutputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(reader->GetOutput());

  if (argc >= 4)
  {
    unsigned int numberOfBinsPerAxis = std::atoi(argv[3]);
    filter->SetNumberOfBinsPerAxis(numberOfBinsPerAxis);

    FilterType::PixelType pixelValueMin = std::atof(argv[4]);
    FilterType::PixelType pixelValueMax = std::atof(argv[5]);
    filter->SetPixelValueMinMax(pixelValueMin, pixelValueMax);

    NeighborhoodType::SizeValueType neighborhoodRadius = std::atoi(argv[6]);
    NeighborhoodType                hood;
    hood.SetRadius(neighborhoodRadius);
    filter->SetNeighborhoodRadius(hood.GetRadius());
  }

  TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Create and set up a writer
  typedef itk::ImageFileWriter<OutputImageType> WriterType;
  WriterType::Pointer                           writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(filter->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
