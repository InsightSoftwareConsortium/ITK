/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsRGBPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraits.h"

namespace itk
{

const RGBPixel<unsigned char> NumericTraits< RGBPixel<unsigned char> >::Zero()
{
  RGBPixel<unsigned char> value;
  value.Fill( NumericTraits<unsigned char>::Zero );
  return value;
};

const RGBPixel<unsigned char> NumericTraits< RGBPixel<unsigned char> >::One()
{
  RGBPixel<unsigned char> value;
  value.Fill( NumericTraits<unsigned char>::One );
  return value;
};



 
} // end namespace itk
