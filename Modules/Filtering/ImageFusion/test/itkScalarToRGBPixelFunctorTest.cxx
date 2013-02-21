/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include <iostream>

#include "itkScalarToRGBPixelFunctor.h"

int itkScalarToRGBPixelFunctorTest(int, char* [] )
{
  itk::RGBPixel<uint8_t> pixel;

  // Test with unsigned long.
  itk::Functor::ScalarToRGBPixelFunctor<unsigned long> ulf;

  std::cout << "Testing unsigned long integers in big endian mode"
            << std::endl;
  ulf.SetBigEndian();
  unsigned long ul;
  for (ul = 0; ul < 100; ++ul)
    {
      pixel = ulf(ul);
      std::cout << ul << "->" << static_cast<int>(pixel[0]) << " "
                << static_cast<int>(pixel[1]) << " " << static_cast<int>(pixel[2])
                << std::endl;
    }
  std::cout << "Testing unsigned long integers in little endian mode"
            << std::endl;

  ulf.SetLittleEndian();
  for (ul = 0; ul < 100; ++ul)
    {
      pixel = ulf(ul);
      std::cout << ul << "->" << static_cast<int>(pixel[0]) << " "
                << static_cast<int>(pixel[1]) << " " << static_cast<int>(pixel[2])
                << std::endl;
    }

  // Test with uint8_t.
  itk::Functor::ScalarToRGBPixelFunctor<uint8_t> ucf;

  std::cout << "Testing uint8_t" << std::endl;
  for (char c = 0; c < 100; ++c)
    {
      pixel = ucf(c);
      std::cout << static_cast<int>(c) << "->"  << static_cast<int>(pixel[0]) << " "
                << static_cast<int>(pixel[1]) << " " << static_cast<int>(pixel[2])
                << std::endl;
    }

  // Test with float
  itk::Functor::ScalarToRGBPixelFunctor<float> ff;
  float f;
  std::cout << "Testing float in big endian mode" << std::endl;
  ff.SetBigEndian();
  for (f = 0; f < 100; ++f)
    {
      pixel = ff(f);

      std::cout << f << "->" << static_cast<int>(pixel[0]) << " "
                << static_cast<int>(pixel[1]) << " " << static_cast<int>(pixel[2])
                << std::endl;
    }
  std::cout << "Testing float in little endian mode"
            << std::endl;

  ff.SetLittleEndian();
  for (f = 0; f < 100; ++f)
    {
      pixel = ff(f);
      std::cout << f << "->" << static_cast<int>(pixel[0]) << " "
                << static_cast<int>(pixel[1]) << " " << static_cast<int>(pixel[2])
                << std::endl;
    }
  return EXIT_SUCCESS;
}
