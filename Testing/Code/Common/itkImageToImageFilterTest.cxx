/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageFilterTest.cxx
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

#include <iostream>

#include "itkImage.h"
#include "itkImageToImageFilter.h"


int itkImageToImageFilterTest(int, char* [] )
{

  const unsigned int      ImageDimension = 3;
  typedef unsigned char   InputPixelType;
  typedef signed short    OutputPixelType;

  typedef itk::Image< InputPixelType,  ImageDimension >  InputImageType;
  typedef itk::Image< OutputPixelType, ImageDimension >  OutputImageType;

  typedef itk::ImageToImageFilter< InputImageType, OutputImageType > FilterType;

  return EXIT_SUCCESS;
}
