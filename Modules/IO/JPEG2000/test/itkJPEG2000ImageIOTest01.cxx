/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkJPEG2000ImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"


int
itkJPEG2000ImageIOTest01(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImageFile  outputImageFile " << std::endl;
    return EXIT_FAILURE;
  }

  //  Register the factory
  itk::JPEG2000ImageIOFactory::RegisterOneFactory();


  //  Image types are defined below.
  using InputPixelType = unsigned char;
  using OutputPixelType = unsigned char;
  constexpr unsigned int Dimension = 2;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  const std::string inputFilename = argv[1];
  const std::string outputFilename = argv[2];

  reader->SetFileName(inputFilename);
  writer->SetFileName(outputFilename);

  writer->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
