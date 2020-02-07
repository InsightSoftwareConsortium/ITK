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
#include "itkLSMImageIO.h"
#include "itkTestingMacros.h"


// Specific ImageIO test

int
itkLSMImageIOTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " LSM.lsm OutputImage.lsm\n";
    return EXIT_FAILURE;
  }

  using InputPixelType = itk::RGBPixel<unsigned char>;
  using InputImageType = itk::Image<InputPixelType, 2>;
  using ReaderType = itk::ImageFileReader<InputImageType>;
  using ImageIOType = itk::LSMImageIO;

  const char * filename = argv[1];
  const char * outfilename = argv[2];

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename);

  ImageIOType::Pointer lsmImageIO = ImageIOType::New();
  reader->SetImageIO(lsmImageIO);

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
  }

  //
  using WriterType = itk::ImageFileWriter<InputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(outfilename);
  writer->SetInput(reader->GetOutput());

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
  }

  lsmImageIO->Print(std::cout);

  return EXIT_SUCCESS;
}
