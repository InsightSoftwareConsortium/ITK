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
#include "itkDiffusionTensor3D.h"

// This file is meant to define numeric traits for tensor pixels types in itk

namespace itk
{
itkNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 1);
itkNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 2);
itkNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 3);
itkNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 4);
itkNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 5);
itkNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 6);
itkNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 7);
itkNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 8);
itkNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 9);
itkNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 10);

} // end namespace itk

#endif // __itkNumericTraitsTensorPixel_h  
