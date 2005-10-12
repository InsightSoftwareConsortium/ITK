/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGetAverageSliceImageFilterTest.cxx
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

#include "itkGetAverageSliceImageFilter.h"
#include "itkImage.h"


int itkGetAverageSliceImageFilterTest(int, char* [] ) 
{
  const int Dimension = 3;
  typedef   itk::Image< unsigned char, Dimension >        InputImageType;
  typedef   itk::Image< unsigned char, Dimension >        OutputImageType;

  typedef   itk::GetAverageSliceImageFilter<InputImageType, OutputImageType>
                                           FilterType;

  FilterType::Pointer  filter = FilterType::New();

  return EXIT_SUCCESS;
}




