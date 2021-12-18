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

#include "itkJPEGImageIO.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"

int
itkJPEGImageIODegenerateCasesTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters.\nUsage: " << itkNameOfTestExecutableMacro(argv) << " inputFilename\n";
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<unsigned char, 2>;

  itk::JPEGImageIO::Pointer io = itk::JPEGImageIO::New();

  itk::ImageFileReader<ImageType>::Pointer reader = itk::ImageFileReader<ImageType>::New();

  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);

  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cout << "Corrupted file " << argv[1] << " triggered exception\n" << ex.GetDescription() << std::endl;
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
