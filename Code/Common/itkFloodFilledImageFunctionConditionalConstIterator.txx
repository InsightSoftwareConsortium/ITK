/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFloodFilledImageFunctionConditionalConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFloodFilledImageFunctionConditionalConstIterator_txx
#define _itkFloodFilledImageFunctionConditionalConstIterator_txx

#include "itkFloodFilledImageFunctionConditionalConstIterator.h"

namespace itk
{
template<class TImage, class TFunction>
bool
FloodFilledImageFunctionConditionalConstIterator<TImage, TFunction>
::IsPixelIncluded(const IndexType & index) const
{
  return this->m_Function->EvaluateAtIndex(index);
}

} // end namespace itk

#endif
