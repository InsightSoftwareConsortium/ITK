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
itkStaticNumericTraitsGenericArrayScalarsMacro( CovariantVector, 1);
itkStaticNumericTraitsGenericArrayScalarsMacro( CovariantVector, 2);
itkStaticNumericTraitsGenericArrayScalarsMacro( CovariantVector, 3);
itkStaticNumericTraitsGenericArrayScalarsMacro( CovariantVector, 4);
itkStaticNumericTraitsGenericArrayScalarsMacro( CovariantVector, 5);
itkStaticNumericTraitsGenericArrayScalarsMacro( CovariantVector, 6);
itkStaticNumericTraitsGenericArrayScalarsMacro( CovariantVector, 7);
itkStaticNumericTraitsGenericArrayScalarsMacro( CovariantVector, 8);
itkStaticNumericTraitsGenericArrayScalarsMacro( CovariantVector, 9);
itkStaticNumericTraitsGenericArrayScalarsMacro( CovariantVector, 10);

} // end namespace itk
