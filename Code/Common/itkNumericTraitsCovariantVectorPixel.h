/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsCovariantVectorPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsCovariantVectorPixel_h
#define __itkNumericTraitsCovariantVectorPixel_h

#include "itkNumericTraitsFixedArrayPixel.h"
#include "itkCovariantVector.h"

namespace itk
{


//
// Instantiate the macros to declare the NumericTraits for the
// CovariantVector types.
//
#ifdef ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

itkNumericTraitsGenericArrayScalarsDimensionsMacro( CovariantVector );

#else // ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, char );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, unsigned char );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, signed char );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, short );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, unsigned short );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, int );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, unsigned int );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, long );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, unsigned long );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, float );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, double );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, long double );
#ifdef ITK_TYPE_USE_LONG_LONG
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, long long );
itkNumericTraitsGenericArrayDimensionsMacro( CovariantVector, unsigned long long );
#endif // ITK_TYPE_USE_LONG_LONG

#endif // ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

} // end namespace itk

#endif // __itkNumericTraitsCovariantVectorPixel_h  
