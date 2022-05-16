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
itkImageSeriesReaderDimensionsTest(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName(s)" << std::endl;
    return EXIT_FAILURE;
  }


  using Image2DType = itk::Image<short, 2>;
  using Image3DType = itk::Image<short, 3>;
  using Image4DType = itk::Image<short, 4>;
  using Image5DType = itk::Image<short, 5>;

  using Reader2DType = itk::ImageSeriesReader<Image2DType>;
  using Reader3DType = itk::ImageSeriesReader<Image3DType>;
  using Reader4DType = itk::ImageSeriesReader<Image4DType>;
  using Reader5DType = itk::ImageSeriesReader<Image5DType>;

  Reader2DType::FileNamesContainer fname;
  fname.push_back(argv[1]);

  Reader2DType::FileNamesContainer fnames;
  for (int i = 1; i < argc; ++i)
    fnames.push_back(argv[i]);


  std::cout << "testing reading a single 2D image to 2D" << std::endl;
  try
  {
    auto reader = Reader2DType::New();
    reader->SetFileNames(fname);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex;
    return EXIT_FAILURE;
  }

  std::cout << "testing reading a single 2D image to 3D" << std::endl;
  try
  {
    auto reader = Reader3DType::New();
    reader->SetFileNames(fname);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex;
    return EXIT_FAILURE;
  }

  std::cout << "testing reading a single 2D image to 4D" << std::endl;
  try
  {
    auto reader = Reader4DType::New();
    reader->SetFileNames(fname);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex;
    return EXIT_FAILURE;
  }

  //////////

  std::cout << "testing reading a series of 2D images to 2D" << std::endl;
  try
  {
    auto reader = Reader2DType::New();
    reader->SetFileNames(fnames);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex;
    // return EXIT_FAILURE;
  }

  std::cout << "testing reading a series of 2D images to 3D" << std::endl;
  try
  {
    auto reader = Reader3DType::New();
    reader->SetFileNames(fnames);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex;
    return EXIT_FAILURE;
  }

  std::cout << "testing reading a series of 2D images to 4D" << std::endl;
  try
  {
    auto reader = Reader4DType::New();
    reader->SetFileNames(fnames);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex;
    return EXIT_FAILURE;
  }

  std::cout << "testing reading a series of 2D images to 5D" << std::endl;
  try
  {
    auto reader = Reader5DType::New();
    reader->SetFileNames(fnames);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
