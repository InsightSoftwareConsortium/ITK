/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFloodFilledSpatialFunctionConditionalConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFloodFilledSpatialFunctionConditionalConstIterator_txx
#define _itkFloodFilledSpatialFunctionConditionalConstIterator_txx

#include "itkFloodFilledSpatialFunctionConditionalConstIterator.h"

namespace itk
{
template<class TImage, class TFunction>
bool
FloodFilledSpatialFunctionConditionalConstIterator<TImage, TFunction>
::IsPixelIncluded(const IndexType & index) const
{
  FunctionInputType position;

  // Get the physical location of this point
  m_Image->TransformIndexToPhysicalPoint(index, position);

  // Evaluate the function at this point
  return m_Function->Evaluate(position);
}

} // end namespace itk

#endif
