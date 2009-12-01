/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsVectorPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsVectorPixel_h
#define __itkNumericTraitsVectorPixel_h

#include "itkNumericTraitsFixedArrayPixel.h"
#include "itkVector.h"


namespace itk
{


//
// Instantiate the macros to declare the NumericTraits for the
// Vector types.
//
#ifdef ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

itkNumericTraitsGenericArrayScalarsDimensionsMacro( Vector );

#else // ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

itkNumericTraitsGenericArrayDimensionsMacro( Vector, char );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, unsigned char );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, signed char );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, short );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, unsigned short );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, int );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, unsigned int );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, long );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, unsigned long );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, float );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, double );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, long double );
#ifdef ITK_TYPE_USE_LONG_LONG
itkNumericTraitsGenericArrayDimensionsMacro( Vector, long long );
itkNumericTraitsGenericArrayDimensionsMacro( Vector, unsigned long long );
#endif // ITK_TYPE_USE_LONG_LONG

#endif // ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

} // end namespace itk

#endif // __itkNumericTraitsVectorPixel_h  
