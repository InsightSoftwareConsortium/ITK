/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsFixedArrayPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsFixedArrayPixel.h"

namespace itk
{

itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, char );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, unsigned char );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, signed char );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, short );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, unsigned short );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, int );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, unsigned int );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, long );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, unsigned long );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, float );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, double );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, long double );
#ifdef ITK_TYPE_USE_LONG_LONG
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, long long );
itkStaticNumericTraitsGenericArrayDimensionsMacro( FixedArray, unsigned long long );
#endif // ITK_TYPE_USE_LONG_LONG

} // end namespace itk
