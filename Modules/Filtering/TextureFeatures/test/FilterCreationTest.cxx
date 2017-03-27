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
#include "itkHistogramToRunLengthFeaturesFilter.h"
#include "itkScalarImageToRunLengthMatrixFilter.h"
#include "itkScalarImageToRunLengthFeaturesFilter.h"

#include "itkNeighborhood.h"
#include "itkMath.h"

int
FilterCreationTest(int argc, char * argv[])
{
  // Parse command line argumentsa
  std::string inputFilename = argv[1];
  // Setup types
  typedef itk::Image<int, 3>                   InputImageType;
  typedef itk::Image<float, 3>                 OutputImageType;
  typedef itk::ImageFileReader<InputImageType> readerType;

  // Create and setup a reader
  readerType::Pointer reader = readerType::New();
  reader->SetFileName(inputFilename.c_str());
  reader->UpdateLargestPossibleRegion();
  InputImageType::Pointer im = reader->GetOutput();

  // Apply the filter
  typedef itk::Statistics::ScalarImageToRunLengthFeaturesImageFilter<InputImageType, OutputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(im); // reader->GetOutput());
  filter->UpdateLargestPossibleRegion();
  OutputImageType::Pointer output = filter->GetOutput();

  std::cout << "@@@@@@@@@@@@@@@@@@" << std::endl;

  // Create and setup a writter
  typedef itk::ImageFileWriter<OutputImageType> WriterType;
  WriterType::Pointer                           writer = WriterType::New();
  writer->SetFileName("result.nrrd");
  writer->SetInput(output); // filter->GetOutput());
  writer->UpdateLargestPossibleRegion();

  return EXIT_SUCCESS;
}
