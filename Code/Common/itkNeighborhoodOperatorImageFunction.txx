/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNeighborhoodOperatorImageFunction_txx
#define _itkNeighborhoodOperatorImageFunction_txx

#include "itkNeighborhoodOperatorImageFunction.h"

#include "itkNeighborhoodInnerProduct.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{

/** Set the Input Image */
template <class TInputImage,class TOutput>
NeighborhoodOperatorImageFunction<TInputImage,TOutput>
::NeighborhoodOperatorImageFunction()
{
}

/** Print self method */
template <class TInputImage,class TOutput>
void
NeighborhoodOperatorImageFunction<TInputImage,TOutput>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "Applying Operator Function:" << std::endl;
}

/** Evaluate the function at the specifed point */
template <class TInputImage,class TOutput>
TOutput
NeighborhoodOperatorImageFunction<TInputImage,TOutput>
::EvaluateAtIndex(const IndexType& index) const
{
  NeighborhoodInnerProduct<InputImageType> smartInnerProduct;
  ConstNeighborhoodIterator<InputImageType> bit;
  bit = ConstNeighborhoodIterator<InputImageType>(m_Operator.GetRadius(),m_Image,m_Image->GetRequestedRegion());
  bit.SetLocation(index);
  
  return smartInnerProduct(bit, m_Operator);
}



} // namespace itk

#endif
