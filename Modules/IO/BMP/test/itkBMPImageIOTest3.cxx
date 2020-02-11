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
#include "itkBMPImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageRegionConstIterator.h"
#include "itkTestingMacros.h"
#include <fstream>


// Specific ImageIO test

/* This test checks that a lower-left bitmap and an upper-left bitmap
 * representing the same grayscale image contains the same data.
 */
int
itkBMPImageIOTest3(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0] << " lowerLeftImage upperLeftImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using IteratorType = itk::ImageRegionConstIterator<ImageType>;


  ReaderType::Pointer lowerLeftImageReader = ReaderType::New();

  itk::BMPImageIO::Pointer lowerLeftImageIO = itk::BMPImageIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(lowerLeftImageIO, BMPImageIO, ImageIOBase);

  lowerLeftImageReader->SetImageIO(lowerLeftImageIO);
  lowerLeftImageReader->SetFileName(argv[1]);

  ReaderType::Pointer upperLeftImageReader = ReaderType::New();

  itk::BMPImageIO::Pointer upperLeftImageIO = itk::BMPImageIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(lowerLeftImageIO, BMPImageIO, ImageIOBase);

  upperLeftImageReader->SetImageIO(upperLeftImageIO);
  upperLeftImageReader->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(lowerLeftImageReader->Update());

  ITK_TRY_EXPECT_NO_EXCEPTION(upperLeftImageReader->Update());


  if (!lowerLeftImageIO->GetFileLowerLeft())
  {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Expecting a lower-left bitmap, got an upper-left." << std::endl;
    return EXIT_FAILURE;
  }

  if (upperLeftImageIO->GetFileLowerLeft())
  {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Expecting an upper-left bitmap, got a lower-left." << std::endl;
    return EXIT_FAILURE;
  }


  ImageType::RegionType loweLeftImageRegion = lowerLeftImageReader->GetOutput()->GetLargestPossibleRegion();
  ImageType::RegionType upperLeftImageRegion = upperLeftImageReader->GetOutput()->GetLargestPossibleRegion();

  if (loweLeftImageRegion != upperLeftImageRegion)
  {
    std::cout << "Test failed!" << std::endl;
    std::cout << "The images must have the same size." << std::endl;
    return EXIT_FAILURE;
  }

  IteratorType it1(lowerLeftImageReader->GetOutput(), loweLeftImageRegion);
  IteratorType it2(upperLeftImageReader->GetOutput(), upperLeftImageRegion);

  it1.GoToBegin();
  it2.GoToBegin();
  while (!it1.IsAtEnd())
  {
    if (it1.Value() != it2.Value())
    {
      std::cout << "Test failed!" << std::endl;
      std::cout << "An image stored in a lower-left bitmap is different than \
                   the same image stored in a upper-left bitmap."
                << std::endl;
      return EXIT_FAILURE;
    }

    ++it1;
    ++it2;
  }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
