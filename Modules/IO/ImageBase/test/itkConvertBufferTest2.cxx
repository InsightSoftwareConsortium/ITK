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

int itkConvertBufferTest2(int, char* [])
{
  // inputs
  int gray1[] = {1, 2, 3, 4, 5};
  int gray2[] = {1, 1, 2, 2, 3, 3, 4, 4, 5, 5};
  int gray3[] = {1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5};
  int gray4[] = {1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5};
  int gray5[] = {1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5};

  // outputs
  int gray[5];
  itk::RGBPixel<int> rgbpixel[5];
  itk::RGBAPixel<int> rgbapixel[5];

  itk::ConvertPixelBuffer<int, int,
    itk::DefaultConvertPixelTraits<int> >::
    Convert(gray1, 1, gray, 5);

  itk::ConvertPixelBuffer<int, int,
    itk::DefaultConvertPixelTraits<int> >::
    Convert(gray2, 2, gray, 5);

  itk::ConvertPixelBuffer<int, int,
    itk::DefaultConvertPixelTraits<int> >::
    Convert(gray3, 3, gray, 5);

  itk::ConvertPixelBuffer<int, int,
    itk::DefaultConvertPixelTraits<int> >::
    Convert(gray4, 4, gray, 5);

  itk::ConvertPixelBuffer<int, int,
    itk::DefaultConvertPixelTraits<int> >::
    Convert(gray5, 5, gray, 5);

//
  itk::ConvertPixelBuffer<int, itk::RGBPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBPixel<int> > >::
    Convert(gray1, 1, rgbpixel, 5);

  itk::ConvertPixelBuffer<int, itk::RGBPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBPixel<int> > >::
    Convert(gray2, 2, rgbpixel, 5);

  itk::ConvertPixelBuffer<int, itk::RGBPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBPixel<int> > >::
    Convert(gray3, 3, rgbpixel, 5);

  itk::ConvertPixelBuffer<int, itk::RGBPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBPixel<int> > >::
    Convert(gray4, 4, rgbpixel, 5);

  itk::ConvertPixelBuffer<int, itk::RGBPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBPixel<int> > >::
    Convert(gray5, 5, rgbpixel, 5);

//
  itk::ConvertPixelBuffer<int, itk::RGBAPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBAPixel<int> > >::
    Convert(gray1, 1, rgbapixel, 5);

  itk::ConvertPixelBuffer<int, itk::RGBAPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBAPixel<int> > >::
    Convert(gray2, 2, rgbapixel, 5);

  itk::ConvertPixelBuffer<int, itk::RGBAPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBAPixel<int> > >::
    Convert(gray3, 3, rgbapixel, 5);

  itk::ConvertPixelBuffer<int, itk::RGBAPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBAPixel<int> > >::
    Convert(gray4, 4, rgbapixel, 5);

  itk::ConvertPixelBuffer<int, itk::RGBAPixel<int>,
    itk::DefaultConvertPixelTraits<itk::RGBAPixel<int> > >::
    Convert(gray5, 5, rgbapixel, 5);

  return EXIT_SUCCESS;
}
