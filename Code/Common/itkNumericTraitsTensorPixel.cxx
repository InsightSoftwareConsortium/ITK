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
itkStaticNumericTraitsGenericArrayDimensionsMacro(SymmetricSecondRankTensor, char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(SymmetricSecondRankTensor, unsigned char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(SymmetricSecondRankTensor, signed char);
itkStaticNumericTraitsGenericArrayDimensionsMacro(SymmetricSecondRankTensor, short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(SymmetricSecondRankTensor, unsigned short);
itkStaticNumericTraitsGenericArrayDimensionsMacro(SymmetricSecondRankTensor, int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(SymmetricSecondRankTensor, unsigned int);
itkStaticNumericTraitsGenericArrayDimensionsMacro(SymmetricSecondRankTensor, long);
itkStaticNumericTraitsGenericArrayDimensionsMacro(SymmetricSecondRankTensor, unsigned long);

//
//    Subsequent Types can be found in the file
//
//                itkNumericTraitsTensorPixel2.cxx
//
//    This was split in two files in order to help the Sun CC 5.6 compiler to
//    manage the size of the compilation unit.
//
} // end namespace itk
