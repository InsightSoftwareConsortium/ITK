/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReviewPrintTest.cxx
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

#include "itkThresholdMaximumConnectedComponentsImageFilter.h"

int main(int , char* [])
{
  typedef itk::Image<float,2> InputType; 
  typedef itk::Image<float,2> OutputType;
  typedef itk::Image<unsigned short,2> UShortImageType;
  typedef itk::Image<unsigned char,2> CharType;

  typedef itk::Vector<float,2> VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;

  itk::ThresholdMaximumConnectedComponentsImageFilter<InputType>::Pointer ThresholdMaximumConnectedComponentsImageFilterObj =
  itk::ThresholdMaximumConnectedComponentsImageFilter<InputType>::New();
  std:: cout << "-------------ThresholdMaximumConnectedComponentsImageFilter " << ThresholdMaximumConnectedComponentsImageFilterObj;

  return 0;

}
