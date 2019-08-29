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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkDanielssonDistanceMapImageFilter.h"

int
itkDanielssonDistanceMapImageFilterTest1(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0] << " InputImage OutputImage\n";
    return -1;
  }

  constexpr unsigned int ImageDimension = 2;
  using InputPixelType = unsigned char;
  using OutputPixelType = float;

  using InputImageType = itk::Image<InputPixelType, ImageDimension>;
  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  using FilterType = itk::DanielssonDistanceMapImageFilter<InputImageType, OutputImageType>;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());
  filter->Update();
  filter->Print(std::cout);

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();
  writer->Update();

  return EXIT_SUCCESS;
}
