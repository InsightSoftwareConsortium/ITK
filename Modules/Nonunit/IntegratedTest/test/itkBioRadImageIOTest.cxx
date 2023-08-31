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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkBioRadImageIO.h"
#include "itkImage.h"
#include "itkTestingMacros.h"


int
itkBioRadImageIOTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " BioRad.pic OutputImage.pic" << std::endl;
    return EXIT_FAILURE;
  }

  using InputPixelType = unsigned char;
  using InputImageType = itk::Image<InputPixelType, 2>;
  using ReaderType = itk::ImageFileReader<InputImageType>;
  using ImageIOType = itk::BioRadImageIO;

  const char * filename = argv[1];
  const char * outfilename = argv[2];

  auto reader = ReaderType::New();
  reader->SetFileName(filename);

  auto bioradImageIO = ImageIOType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(bioradImageIO, BioRadImageIO, ImageIOBase);


  reader->SetImageIO(bioradImageIO);
  bioradImageIO->DebugOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  using WriterType = itk::ImageFileWriter<InputImageType>;
  auto writer = WriterType::New();
  writer->SetImageIO(bioradImageIO);
  writer->SetFileName(outfilename);
  writer->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
