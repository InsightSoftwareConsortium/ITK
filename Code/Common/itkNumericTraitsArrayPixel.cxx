/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsArrayPixel.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkNumericTraitsArrayPixel.h"

namespace itk
{
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, char);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, unsigned char);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, signed char);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, short);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, unsigned short);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, int);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, unsigned int);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, long);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, unsigned long);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, float);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, double);
itkStaticNumericTraitsGenericArrayNoDimensionMacro(Array, long double);
} // end namespace itk
