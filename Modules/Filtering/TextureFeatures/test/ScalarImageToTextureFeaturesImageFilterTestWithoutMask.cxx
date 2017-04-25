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
#include "itkScalarImageToTextureFeaturesImageFilter.h"

#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNeighborhood.h"

int
ScalarImageToTextureFeaturesImageFilterTestWithoutMask(int argc, char * argv[])
{
  // Setup types
  typedef itk::Image<int, 3>                                                                    InputImageType;
  typedef itk::Image<itk::Vector<float, 8>, 3>                                                  OutputImageType;
  typedef itk::ImageFileReader<InputImageType>                                                  readerType;
  typedef itk::Neighborhood<typename InputImageType::PixelType, InputImageType::ImageDimension> NeighborhoodType;
  NeighborhoodType                                                                              hood;

  // Create and setup a reader
  readerType::Pointer reader = readerType::New();
  std::string         inputFilename = argv[1];
  reader->SetFileName(inputFilename.c_str());

  // Apply the filter
  typedef itk::Statistics::ScalarImageToTextureFeaturesImageFilter<InputImageType, OutputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());
  if (argc >= 4)
  {
    filter->SetNumberOfBinsPerAxis(std::atoi(argv[3]));
    filter->SetPixelValueMinMax(std::atof(argv[4]), std::atof(argv[5]));
    hood.SetRadius(std::atoi(argv[6]));
    filter->SetNeighborhoodRadius(hood.GetRadius());
  }
  filter->UpdateLargestPossibleRegion();

  // Create and setup a writter
  typedef itk::ImageFileWriter<OutputImageType> WriterType;
  WriterType::Pointer                           writer = WriterType::New();
  std::string                                   outputFilename = argv[2];
  writer->SetFileName(outputFilename.c_str());
  writer->SetInput(filter->GetOutput());
  writer->UpdateLargestPossibleRegion();

  return EXIT_SUCCESS;
}
