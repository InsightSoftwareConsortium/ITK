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

#include "itkLaplacianSharpeningImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

int
main(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  const char * inputFilename = argv[1];
  const char * outputFilename = argv[2];

  using CharPixelType = unsigned char;
  constexpr unsigned int Dimension = 2;

  using CharImageType = itk::Image<CharPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<CharImageType>;
  using WriterType = itk::ImageFileWriter<CharImageType>;

  using RescaleFilter = itk::RescaleIntensityImageFilter<CharImageType, CharImageType>;

  using LaplacianSharpeningFilter =
    itk::LaplacianSharpeningImageFilter<CharImageType, CharImageType>;


  // Setting the IO
  ReaderType::Pointer    reader = ReaderType::New();
  WriterType::Pointer    writer = WriterType::New();
  RescaleFilter::Pointer rescale = RescaleFilter::New();

  // Setting the ITK pipeline filter

  LaplacianSharpeningFilter::Pointer lapFilter = LaplacianSharpeningFilter::New();

  reader->SetFileName(inputFilename);
  writer->SetFileName(outputFilename);

  // Sharpen with the laplacian
  lapFilter->SetInput(reader->GetOutput());

  // Rescale and cast to unsigned char
  rescale->SetInput(lapFilter->GetOutput());
  writer->SetInput(rescale->GetOutput());

  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
