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
#include "itkVectorImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNeighborhood.h"

int
ScalarImageToRunLengthFeaturesImageFilterTestWithVectorImage(int argc, char * argv[])
{
  // Setup types
  typedef itk::Image<int, 3>                                                                    InputImageType;
  typedef itk::VectorImage<float, 3>                                                            OutputImageType;
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
  filter->SetInput(reader->GetOutput());
  filter->SetMaskImage(maskReader->GetOutput());
  if (argc >= 5)
  {
    filter->SetNumberOfBinsPerAxis(std::atoi(argv[4]));
    filter->SetPixelValueMinMax(std::atof(argv[5]), std::atof(argv[6]));
    filter->SetDistanceValueMinMax(std::atof(argv[7]), std::atof(argv[8]));
    hood.SetRadius(std::atoi(argv[9]));
    filter->SetNeighborhoodRadius(hood.GetRadius());
  }
  filter->UpdateLargestPossibleRegion();

  // Create and setup a writter
  typedef itk::ImageFileWriter<OutputImageType> WriterType;
  WriterType::Pointer                           writer = WriterType::New();
  std::string                                   outputFilename = argv[3];
  writer->SetFileName(outputFilename.c_str());
  writer->SetInput(filter->GetOutput());
  writer->UpdateLargestPossibleRegion();

  return EXIT_SUCCESS;
}
