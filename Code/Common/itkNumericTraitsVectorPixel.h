 /*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsVectorPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsVectorPixel_h
#define __itkNumericTraitsVectorPixel_h

#include "itkNumericTraitsFixedArrayPixel.h"
#include "itkVector.h"

// This file defines numeric traits for vector pixels types in itk
// TODO: Add doxygen tags..

namespace itk
{
itkNumericTraitsGenericArrayScalarsMacro( Vector, 1);
itkNumericTraitsGenericArrayScalarsMacro( Vector, 2);
itkNumericTraitsGenericArrayScalarsMacro( Vector, 3);
itkNumericTraitsGenericArrayScalarsMacro( Vector, 4);
itkNumericTraitsGenericArrayScalarsMacro( Vector, 5);
itkNumericTraitsGenericArrayScalarsMacro( Vector, 6);
itkNumericTraitsGenericArrayScalarsMacro( Vector, 7);
itkNumericTraitsGenericArrayScalarsMacro( Vector, 8);
itkNumericTraitsGenericArrayScalarsMacro( Vector, 9);
itkNumericTraitsGenericArrayScalarsMacro( Vector, 10);

} // end namespace itk

#endif // __itkNumericTraitsVectorPixel_h  
