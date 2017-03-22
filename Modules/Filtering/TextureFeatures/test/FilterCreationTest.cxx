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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScalarImageToRunLengthFeaturesImageFilter.h"

int
FilterCreationTest(int argc, char * argv[])
{
  // Parse command line argumentsa
  std::string inputFilename = argv[1];

  // Setup types
  typedef itk::Image<float, 3> ImageType;

  typedef itk::ImageFileReader<ImageType> readerType;

  // Create and setup a reader
  readerType::Pointer reader = readerType::New();
  reader->SetFileName(inputFilename.c_str());

  // Apply the filter
  typedef itk::ScalarImageToRunLengthFeaturesImageFilter<ImageType, ImageType> FilterType;
  FilterType::Pointer                                                          filter = FilterType::New();

  // Create and setup a writter
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer                     writer = WriterType::New();
  writer->SetFileName("result.nrrd");
  writer->SetInput(filter->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}
