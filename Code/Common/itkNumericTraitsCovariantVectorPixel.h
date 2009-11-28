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

// This file defines numeric traits for vector pixels types in itk
// TODO: Add doxygen tags..

namespace itk
{

itkNumericTraitsGenericArrayScalarsMacro( CovariantVector, 1);
itkNumericTraitsGenericArrayScalarsMacro( CovariantVector, 2);
itkNumericTraitsGenericArrayScalarsMacro( CovariantVector, 3);
itkNumericTraitsGenericArrayScalarsMacro( CovariantVector, 4);
itkNumericTraitsGenericArrayScalarsMacro( CovariantVector, 5);
itkNumericTraitsGenericArrayScalarsMacro( CovariantVector, 6);
itkNumericTraitsGenericArrayScalarsMacro( CovariantVector, 7);
itkNumericTraitsGenericArrayScalarsMacro( CovariantVector, 8);
itkNumericTraitsGenericArrayScalarsMacro( CovariantVector, 9);
itkNumericTraitsGenericArrayScalarsMacro( CovariantVector, 10);

} // end namespace itk

#endif // __itkNumericTraitsCovariantVectorPixel_h  
