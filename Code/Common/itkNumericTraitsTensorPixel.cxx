/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsTensorPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsTensorPixel.h"

namespace itk
{

itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, char );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, unsigned char );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, signed char );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, short );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, unsigned short );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, int );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, unsigned int );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, long );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, unsigned long );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, float );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, double );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, long double );
#ifdef ITK_TYPE_USE_LONG_LONG
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, long long );
itkStaticNumericTraitsGenericArrayDimensionsMacro( SymmetricSecondRankTensor, unsigned long long );
#endif // ITK_TYPE_USE_LONG_LONG

} // end namespace itk
