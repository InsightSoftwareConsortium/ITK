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

#include <iostream>

#include "itkRGBPixel.h"

int
itkRGBPixelTest(int, char *[])
{
  // Test it all

  float                        val[3] = { 1, 0, .5 };
  itk::RGBPixel<float>         pixel(val);
  unsigned char                pixelInit0[3] = { 255, 255, 255 };
  unsigned char                pixelInit1[3] = { 255, 255, 244 };
  itk::RGBPixel<unsigned char> pixelArray[2];
  pixelArray[0] = pixelInit0;
  pixelArray[1] = pixelInit1;

  std::cout << "sizeof(pixel) = " << sizeof(pixel) << '\n';
  if (sizeof(pixel) != 3 * sizeof(itk::RGBPixel<float>::ComponentType))
  {
    std::cerr << "ERROR: sizeof(pixel) == " << sizeof(pixel) << " but is shopuld be "
              << 3 * sizeof(itk::RGBPixel<float>::ComponentType) << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "pixel.GetNumberOfComponents = " << pixel.GetNumberOfComponents() << '\n';
  std::cout << "pixel.GetScalarValue() = " << pixel.GetScalarValue() << '\n';
  std::cout << "pixel.GetNthComponent()" << '\n';
  for (unsigned int i = 0; i < pixel.GetNumberOfComponents(); ++i)
  {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << '\n';
  }
  pixel.SetRed(11.0);
  pixel.SetGreen(22.0);
  pixel.SetBlue(33.0);
  std::cout << "pixel.SetRed (11.0); pixel.SetGreen (22.0); pixel.SetBlue (33.0);" << '\n';
  for (unsigned int i = 0; i < pixel.GetNumberOfComponents(); ++i)
  {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << '\n';
  }
  std::cout << "pixel.GetRed() = " << pixel.GetRed() << '\n';
  std::cout << "pixel.GetGreen() = " << pixel.GetGreen() << '\n';
  std::cout << "pixel.GetBlue() = " << pixel.GetBlue() << '\n';

  std::cout << "pixel[0] = 111; pixel[1] = 222; pixel[2] = 333;" << '\n';

  pixel[0] = 111;
  pixel[1] = 222;
  pixel[2] = 333;
  for (unsigned int i = 0; i < pixel.GetNumberOfComponents(); ++i)
  {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << '\n';
  }

  std::cout << "std::cout << pixel << '\n';" << '\n';
  std::cout << '\t' << pixel << '\n';

  for (unsigned int j = 0; j < 2; ++j)
  {
    std::cout << "pixelArray[" << j << "].GetNumberOfComponents = " << pixelArray[j].GetNumberOfComponents() << '\n';
    std::cout << "pixelArray[" << j << "].GetScalarValue() = " << static_cast<int>(pixelArray[j].GetScalarValue())
              << '\n';
    std::cout << "pixelArray[" << j << "].GetNthComponent()" << '\n';
    for (unsigned int i = 0; i < pixelArray[j].GetNumberOfComponents(); ++i)
    {
      std::cout << "\tpixelArray[" << j << "].GetNthComponent(" << i
                << ") = " << static_cast<int>(pixelArray[j].GetNthComponent(i)) << '\n';
    }
  }

  std::cout << "Testing arithmetic methods" << '\n';
  itk::RGBPixel<float> pa;
  itk::RGBPixel<float> pb;

  pa[0] = 1.25;
  pa[1] = 3.25;
  pa[2] = 5.25;

  pb[0] = 1.55;
  pb[1] = 3.55;
  pb[2] = 5.55;

  itk::RGBPixel<float> pc;

  pc = pa + pb;
  std::cout << "addition = " << pc << '\n';

  pc = pa - pb;
  std::cout << "subtraction = " << pc << '\n';

  pc += pb;
  std::cout << "in-place addition = " << pc << '\n';

  pc -= pb;
  std::cout << "in-place subtraction = " << pc << '\n';

  pc = pa * 3.2;
  std::cout << "product by scalar = " << pc << '\n';


  std::cout << "Test luminance conversion" << '\n';

  itk::RGBPixel<float> rgbl;
  rgbl[0] = 100;
  rgbl[1] = 150;
  rgbl[2] = 120;

  const float luminance = rgbl.GetLuminance();
  const float realLuminance = rgbl[0] * 0.30 + rgbl[1] * 0.59 + rgbl[2] * 0.11;
  const float tolerance = 1e-4;

  if (itk::Math::abs(luminance - realLuminance) > tolerance)
  {
    std::cerr << "Error in luminance conversion" << '\n';
    return EXIT_FAILURE;
  }

  itk::RGBPixel<float> rgbBase;
  rgbBase[0] = 100;
  rgbBase[1] = 150;
  rgbBase[2] = 120;

  itk::RGBPixel<float> rgbEqual;
  rgbEqual[0] = 100;
  rgbEqual[1] = 150;
  rgbEqual[2] = 120;
  if (!(rgbBase == rgbEqual))
  {
    std::cerr << "Error in test for equality" << '\n';
    return EXIT_FAILURE;
  }

  itk::RGBPixel<float> rgbLess;
  rgbLess[0] = 99;
  rgbLess[1] = 149;
  rgbLess[2] = 119;
  if (!(rgbLess < rgbBase))
  {
    std::cerr << "Error in test for less than" << '\n';
    return EXIT_FAILURE;
  }

  if (rgbBase < rgbEqual)
  {
    std::cerr << "Error in test for (not) less than" << '\n';
    return EXIT_FAILURE;
  }

  if (rgbBase == rgbLess)
  {
    std::cerr << "Error in test for (not) equal" << '\n';
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
