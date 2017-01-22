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

#include "itkImageFileReader.h"
#include <iostream>
#include "itkTestingMacros.h"

int itkConvertBufferTest(int, char* [])
{
  int piInit[3] = {3,1,4};
  itk::RGBPixel<int> pi = piInit;
  int piaInit[4] = {3,1,4,1};
  itk::RGBAPixel<int> pia = piaInit;
  std::cerr << "RGBPixel<int>: " <<  pi << "\n";
  std::cerr << "RGBAPixel<int>: " << pia << "\n";

  itk::RGBAPixel<unsigned char> ucpa[3];
  itk::RGBAPixel<float> pa[3];

  int ipa[] = {1, 2, 3};
  itk::RGBPixel<int> p[3];
  // convert from int to RGB<int>
  itk::ConvertPixelBuffer<int, itk::RGBPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBPixel<int> > >::
    Convert(ipa, 1, p, 3);
  std::cerr << "RGB 111 222 333 = ";
  for(int j=0; j < 3; ++j)
    {
    std::cerr << p[j] << ", ";
    for(unsigned long k=0; k < sizeof(p) / sizeof(itk::RGBPixel<int>); ++k)
      {
      TEST_EXPECT_EQUAL(p[j][k], ipa[j]);
      }
    }
  std::cerr << "\n";
  float ipa3com[] = {1.f,1.f,1.f, 2.f,2.f,2.f, 3.f,3.f,3.f};
  itk::RGBPixel<float> pf[3];
  // convert from float[] to RGB<float>
  itk::ConvertPixelBuffer<float, itk::RGBPixel<float>,
    itk::DefaultConvertPixelTraits<itk::RGBPixel<float> > >::
    Convert(ipa3com, 3, pf, 3);
  std::cerr << "itk::RGBPixel<float> array converted from float\n";
  for(unsigned int j = 0; j < 3; ++j)
    {
    std::cerr << pf[j] << " ";
    for(unsigned int k = 0; k < sizeof(pf) / sizeof(itk::RGBPixel<float>); ++k)
      {
      TEST_EXPECT_EQUAL(pf[k][j], ipa3com[j+k*3]);
      }
    }
  std::cerr << "\n";
  // convert from float[] to RGBA<float>
  itk::ConvertPixelBuffer<float, itk::RGBAPixel<float>,
    itk::DefaultConvertPixelTraits<itk::RGBAPixel<float> > >::
    Convert(ipa3com, 3, pa, 3);
  std::cerr << "itk::RGBAPixel<float> array \n";
  for(unsigned int j = 0; j < 3; ++j)
    {
    std::cerr << pa[j] << " ";
    for(unsigned int k = 0;
        k < sizeof(pa) / sizeof(itk::RGBAPixel<float>);
        ++k
       )
      {
      TEST_EXPECT_EQUAL(pa[k][j], ipa3com[j+k*3]);
      }
    }
  TEST_EXPECT_EQUAL(pa[0][3], 1.f); // Alpha must be 1.0f for float input pixel.
  std::cerr << "\n";
  unsigned char ucipa3com[] = {1,1,1, 2,2,2, 3,3,3};
  // convert from unsigned char[3] to RGBA<unsigned char>
  itk::ConvertPixelBuffer<unsigned char, itk::RGBAPixel<unsigned char>,
    itk::DefaultConvertPixelTraits<itk::RGBAPixel<unsigned char> > >::
    Convert(ucipa3com, 3, ucpa, 3);
  std::cerr << "itk::RGBAPixel<unsigned char> array \n";
  for(unsigned int j = 0; j < 3; ++j)
    {
    std::cerr << ucpa[j] << " ";
    for(unsigned int k = 0;
        k < sizeof(ucpa) / sizeof(itk::RGBAPixel<unsigned char>);
        ++k
       )
      {
      TEST_EXPECT_EQUAL(ucpa[k][j], ucipa3com[j+k*3]);
      }
    }
  TEST_EXPECT_EQUAL(ucpa[0][3], 255); // Alpha must be 255 for unsigned char input pixel type
  std::cerr << "\n";
  // create an initial array of floats
  float farray[] = {1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 6.4f, 7.4f, 8.8f, 9.9f  };
  // set the size of the array in number of elements
  const int arraySize = sizeof(farray)/sizeof(farray[0]);
  double darray[arraySize];     // create a double array
  int iarray[arraySize];        // create an int array
  // convert the float array to a double array
  itk::ConvertPixelBuffer<float, double,
    itk::DefaultConvertPixelTraits<double> >::
    Convert(farray, 1, darray, arraySize);
  std::cerr << "\nfloat array  : ";
  for(int i =0; i < arraySize; ++i)
    {
    std::cerr << farray[i] << " ";
    TEST_EXPECT_EQUAL(darray[i], static_cast<double>(farray[i]))
    }
  // convert a float array to an int array
  itk::ConvertPixelBuffer<float, int,
    itk::DefaultConvertPixelTraits<int> >::
    Convert(farray, 1, iarray, arraySize);
  std::cerr << "\nint array   : ";
  for(int i =0; i < arraySize; ++i)
    {
    std::cerr << iarray[i] << " ";
    TEST_EXPECT_EQUAL(iarray[i], static_cast<int>(farray[i]));
    }
  // convert the int array to the float array
  itk::ConvertPixelBuffer<int, float,
    itk::DefaultConvertPixelTraits<float> >::
    Convert(iarray, 1, farray, arraySize);
  std::cerr << "\ndouble array : ";
  for(int i =0; i < arraySize; ++i)
    {
    std::cerr << darray[i] << " ";
    TEST_EXPECT_EQUAL(farray[i], static_cast<float>(iarray[i]));
    }
  std::cerr << "\n";
  return EXIT_SUCCESS;
}
