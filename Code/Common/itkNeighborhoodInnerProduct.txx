/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodInnerProduct.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNeighborhoodInnerProduct_txx
#define _itkNeighborhoodInnerProduct_txx
#include "itkNeighborhoodInnerProduct.h"

#include "itkNumericTraits.h"

namespace itk {

template<class TImage, class TOperator, class TComputation>
typename NeighborhoodInnerProduct<TImage, TOperator, TComputation>::OutputPixelType
NeighborhoodInnerProduct<TImage, TOperator, TComputation>
::operator()(const std::slice &s,
             /*           const ImageBoundaryCondition<TImage> *,*/
             const ConstNeighborhoodIterator<TImage> &it,
             const OperatorType &op) const
{
  typename OperatorType::ConstIterator o_it;
  OutputPixelType sum = NumericTraits<OutputPixelType>::Zero;

  o_it = op.Begin();
  const typename OperatorType::ConstIterator op_end = op.End();

  for ( unsigned int i = s.start(); o_it < op_end; i+=s.stride(), ++o_it )
    {
    sum += *o_it * it.GetPixel(i);
    }
  

   return sum;
} 

}// end namespace itk
#endif
