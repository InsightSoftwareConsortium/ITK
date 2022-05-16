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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkNrrdImageIO.h"
#include "itkTestingMacros.h"

// Specific ImageIO test

int
itkNrrdVectorImageReadTest(int argc, char * argv[])
{
  if (argc < 1)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " Input\n";
    return EXIT_FAILURE;
  }

  using PixelType = itk::Vector<float, 4>;
  using myImage = itk::Image<PixelType, 3>;

  using ReaderType = itk::ImageFileReader<myImage>;

  auto reader = ReaderType::New();

  reader->SetImageIO(itk::NrrdImageIO::New());

  reader->SetFileName(argv[1]);

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

  myImage::Pointer   image = reader->GetOutput();
  myImage::IndexType coord;
  PixelType          sample;

  // The test image has been constructed so that the vector coefficients
  // coincide with sample coordinates
  double       err = 0;
  unsigned int idx = 0;
  for (unsigned int zi = 0; zi < 5; ++zi)
  {
    coord[2] = zi;
    for (unsigned int yi = 0; yi < 5; ++yi)
    {
      coord[1] = yi;
      for (unsigned int xi = 0; xi < 5; ++xi)
      {
        coord[0] = xi;
        sample = image->GetPixel(coord);
        err += itk::Math::abs(sample[0] - coord[0]);
        err += itk::Math::abs(sample[1] - coord[1]);
        err += itk::Math::abs(sample[2] - coord[2]);
        err += itk::Math::abs(sample[3] - idx);
        idx++;
      }
    }
  }

  if (err)
  {
    std::cout << "test FAILED because values not as expected\n";
    return EXIT_FAILURE;
  }
  else
  {
    return EXIT_SUCCESS;
  }
}
