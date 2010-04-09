/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsTensorPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsTensorPixel_h
#define __itkNumericTraitsTensorPixel_h

#include "itkNumericTraitsFixedArrayPixel.h"
#include "itkSymmetricSecondRankTensor.h"

// This file is meant to define numeric traits for tensor pixels types in itk

namespace itk
{


//
// Instantiate the macros to declare the NumericTraits for the
// SymmetricSecondRankTensor types.
//
#ifdef ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

itkNumericTraitsGenericArrayScalarsDimensionsMacro( SymmetricSecondRankTensor );

#else // ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, char );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, unsigned char );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, signed char );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, short );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, unsigned short );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, int );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, unsigned int );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, long );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, unsigned long );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, float );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, double );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, long double );
#ifdef ITK_TYPE_USE_LONG_LONG
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, long long );
itkNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, unsigned long long );
#endif // ITK_TYPE_USE_LONG_LONG

#endif // ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

} // end namespace itk

#endif // __itkNumericTraitsTensorPixel_h  
