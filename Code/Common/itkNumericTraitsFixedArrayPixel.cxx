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
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, unsigned char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, signed char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, unsigned short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, unsigned int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(FixedArray, unsigned long);

//
//    Subsequent Types can be found in
//
//               itkNumericTraitsFixedArrayPixel2.cxx
//
//    This was split in two files in order to help the Sun CC 5.6 compiler to
//    manage the size of the compilation unit.
//
} // end namespace itk
