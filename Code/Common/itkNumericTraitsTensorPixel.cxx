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
itkStaticNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 1);
itkStaticNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 2);
itkStaticNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 3);
itkStaticNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 4);
itkStaticNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 5);
itkStaticNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 6);
itkStaticNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 7);
itkStaticNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 8);
itkStaticNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 9);
itkStaticNumericTraitsGenericArrayScalarsMacro( SymmetricSecondRankTensor, 10);

} // end namespace itk
