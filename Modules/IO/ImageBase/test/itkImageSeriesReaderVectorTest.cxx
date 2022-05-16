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

#include "itkImageSeriesReader.h"
#include "itkTestingMacros.h"

int
itkImageSeriesReaderVectorTest(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName(s)" << std::endl;
    return EXIT_FAILURE;
  }

  using VectorImageType = itk::VectorImage<unsigned short, 3>;

  using VectorImageSeriesReader = itk::ImageSeriesReader<VectorImageType>;

  VectorImageSeriesReader::FileNamesContainer fnames;
  for (int i = 1; i < argc; ++i)
    fnames.push_back(argv[i]);


  std::cout << "testing reading an image series into VecorImage" << std::endl;
  try
  {
    auto reader = VectorImageSeriesReader::New();
    reader->SetFileNames(fnames);
    reader->Update();
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex;
    return EXIT_FAILURE;
  }

  std::cout << "testing reading image series into ImageOfVectors " << std::endl;
  try
  {
    auto reader = VectorImageSeriesReader::New();
    reader->SetFileNames(fnames);
    reader->Update();
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
