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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkStreamingImageFilter.h"
#include "itkMedianImageFilter.h"
#include "itkMetaImageIO.h"

int
itkMetaImageStreamingIOTest(int ac, char * av[])
{
  //  Image types are defined below.
  using InputPixelType = unsigned char;
  using OutputPixelType = unsigned char;
  constexpr unsigned int Dimension = 3;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  using IOType = itk::MetaImageIO;

  using FilterType = itk::MedianImageFilter<OutputImageType, OutputImageType>;

  using StreamingFilterType = itk::StreamingImageFilter<OutputImageType, OutputImageType>;

  FilterType::Pointer filter = FilterType::New();

  StreamingFilterType::Pointer streamer = StreamingFilterType::New();

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  IOType::Pointer metaIn = IOType::New();
  IOType::Pointer metaOut = IOType::New();
  reader->SetImageIO(metaIn);
  writer->SetImageIO(metaOut);

  const std::string inputFilename = av[1];
  const std::string outputFilename = av[2];

  reader->SetFileName(inputFilename);
  reader->SetUseStreaming(true);

  writer->SetFileName(outputFilename);

  InputImageType::SizeType indexRadius;

  indexRadius[0] = 1; // radius along x
  indexRadius[1] = 1; // radius along y
  indexRadius[2] = 1; // radius along Z

  filter->SetRadius(indexRadius);

  // filter->SetInput( reader->GetOutput() );
  streamer->SetInput(reader->GetOutput());
  writer->SetInput(streamer->GetOutput());

  // test streaming check methods
  if (!metaIn->CanStreamRead())
  {
    std::cerr << "Failed stream read check" << std::endl;
    return EXIT_FAILURE;
  }
  if (!metaOut->CanStreamWrite())
  {
    std::cerr << "Failed stream write check" << std::endl;
    return EXIT_FAILURE;
  }

  // By default we decide to use 4 pieces, but this value can
  // be changed from the command line.
  unsigned int numberOfDataPieces = 4;
  if (ac > 3)
  {
    numberOfDataPieces = std::stoi(av[3]);
  }

  streamer->SetNumberOfStreamDivisions(numberOfDataPieces);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
