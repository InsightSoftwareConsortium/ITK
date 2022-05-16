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
itkNrrdComplexImageReadTest(int argc, char * argv[])
{
  if (argc < 1)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " Input\n";
    return EXIT_FAILURE;
  }

  using PixelType = std::complex<float>;
  using myImage = itk::Image<PixelType, 2>;

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

  myImage::Pointer image = reader->GetOutput();

  // Pick off some pixels in the test image (the first, the last, and
  // a few in between) and make sure that the values are very close to
  // what we know to be correct, given that we know exactly what
  // volume is being read.  I/O errors will generate more than tiny
  // differences near representational precision.

  float              err = 0;
  myImage::IndexType coord;
  PixelType          sample;
  coord[0] = 0;
  coord[1] = 0;
  sample = image->GetPixel(coord);
  err += itk::Math::abs(sample.real() - 27.985973);
  err += itk::Math::abs(sample.imag() - 0.0);

  coord[0] = 53;
  coord[1] = 43;
  sample = image->GetPixel(coord);
  err += itk::Math::abs(sample.real() - -0.94961888);
  err += itk::Math::abs(sample.imag() - 0.409872);

  coord[0] = 10;
  coord[1] = 43;
  sample = image->GetPixel(coord);
  err += itk::Math::abs(sample.real() - -0.096564025);
  err += itk::Math::abs(sample.imag() - 0.0094992276);

  coord[0] = 10;
  coord[1] = 0;
  sample = image->GetPixel(coord);
  err += itk::Math::abs(sample.real() - 0.036231704);
  err += itk::Math::abs(sample.imag() - -0.016659589);

  coord[0] = 42;
  coord[1] = 42;
  sample = image->GetPixel(coord);
  err += itk::Math::abs(sample.real() - -0.027012844);
  err += itk::Math::abs(sample.imag() - -0.012217643);

  coord[0] = 50;
  coord[1] = 40;
  sample = image->GetPixel(coord);
  err += itk::Math::abs(sample.real() - 0.44949868);
  err += itk::Math::abs(sample.imag() - -0.033380687);

  coord[0] = 8;
  coord[1] = 9;
  sample = image->GetPixel(coord);
  err += itk::Math::abs(sample.real() - -0.036674671);
  err += itk::Math::abs(sample.imag() - -0.0061681992);

  double thresh = 0.00000038;
  if (err > thresh)
  {
    std::cout << "failure because err == " << err << "> " << thresh << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    return EXIT_SUCCESS;
  }
}
