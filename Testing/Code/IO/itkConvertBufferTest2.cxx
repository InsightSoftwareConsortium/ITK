/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConvertBufferTest2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkImageFileReader.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkConvertPixelBuffer.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkImage.h"
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


