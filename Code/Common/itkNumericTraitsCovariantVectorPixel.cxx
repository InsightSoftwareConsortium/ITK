/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsCovariantVectorPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsCovariantVectorPixel.h"

namespace itk
{
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, unsigned char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, signed char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, unsigned short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, unsigned int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, unsigned long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, float);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, double);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, long double);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, long long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(CovariantVector, unsigned long long);
} // end namespace itk
