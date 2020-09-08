/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkConnectedComponentImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"

#include "itkSignedDanielssonDistanceMapImageFilter.h"

int
itkSignedDanielssonDistanceMapImageFilterTest2(int argc, char * argv[])
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

  using ConnectedType = itk::ConnectedComponentImageFilter<InputImageType, InputImageType>;
  ConnectedType::Pointer connectedComponents = ConnectedType::New();
  connectedComponents->SetInput(reader->GetOutput());

  using FilterType = itk::SignedDanielssonDistanceMapImageFilter<InputImageType, OutputImageType>;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(connectedComponents->GetOutput());
  filter->Update();
  filter->Print(std::cout);

  // Extract the Voronoi map from the distance map filter, rescale it,
  // and write it out.
  using RescaleType = itk::RescaleIntensityImageFilter<InputImageType, OutputImageType>;
  RescaleType::Pointer rescaler = RescaleType::New();
  rescaler->SetInput(filter->GetVoronoiMap());
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(rescaler->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();
  writer->Update();

  return EXIT_SUCCESS;
}
