/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsRGBPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsRGBPixel.h"

namespace itk
{

const RGBPixel<unsigned char>  NumericTraits<RGBPixel<unsigned char> >::Zero = RGBPixel<unsigned char>( NumericTraits<unsigned char>::Zero );
const RGBPixel<unsigned char>  NumericTraits<RGBPixel<unsigned char> >::One = RGBPixel<unsigned char>( NumericTraits<unsigned char>::One );
const RGBPixel<unsigned short>  NumericTraits<RGBPixel<unsigned short> >::Zero = RGBPixel<unsigned short>( NumericTraits<unsigned short>::Zero );
const RGBPixel<unsigned short>  NumericTraits<RGBPixel<unsigned short> >::One = RGBPixel<unsigned short>( NumericTraits<unsigned short>::One );
 
} // end namespace itk
