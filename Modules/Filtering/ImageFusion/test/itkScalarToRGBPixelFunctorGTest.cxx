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

#include "itkScalarToRGBPixelFunctor.h"
#include "itkGTest.h"

TEST(ScalarToRGBPixelFunctor, ConvertedLegacyTest)
{
  itk::RGBPixel<unsigned char> pixel;

  // Test with unsigned long.
  itk::Functor::ScalarToRGBPixelFunctor<unsigned long> ulf;

  ulf.SetBigEndian();
  for (unsigned long ul = 0; ul < 100; ++ul)
  {
    pixel = ulf(ul);
  }

  ulf.SetLittleEndian();
  for (unsigned long ul = 0; ul < 100; ++ul)
  {
    pixel = ulf(ul);
  }

  // Test with unsigned char.
  const itk::Functor::ScalarToRGBPixelFunctor<unsigned char> ucf;

  for (char c = 0; c < 100; ++c)
  {
    pixel = ucf(c);
  }

  // Test with float
  itk::Functor::ScalarToRGBPixelFunctor<float> ff;
  ff.SetBigEndian();
  for (float f = 0; f < 100; ++f)
  {
    pixel = ff(f);
  }

  ff.SetLittleEndian();
  for (float f = 0; f < 100; ++f)
  {
    pixel = ff(f);
  }

  ff.SetUseMSBForHashing(true);
  EXPECT_TRUE(ff.GetUseMSBForHashing());

  ff.SetUseMSBForHashing(false);
  EXPECT_FALSE(ff.GetUseMSBForHashing());

  ff.UseMSBForHashingOn();
  EXPECT_TRUE(ff.GetUseMSBForHashing());

  ff.UseMSBForHashingOff();
  EXPECT_FALSE(ff.GetUseMSBForHashing());
}
