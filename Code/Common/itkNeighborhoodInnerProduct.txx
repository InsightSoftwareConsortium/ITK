/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodInnerProduct.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNeighborhoodInnerProduct_txx
#define __itkNeighborhoodInnerProduct_txx
#include "itkNeighborhoodInnerProduct.h"

#include "itkNumericTraits.h"

namespace itk
{
template< class TImage, class TOperator, class TComputation >
typename NeighborhoodInnerProduct< TImage, TOperator, TComputation >::OutputPixelType
NeighborhoodInnerProduct< TImage, TOperator, TComputation >
::operator()(const std::slice & s,
             const ConstNeighborhoodIterator< TImage > & it,
             const OperatorType & op) const
{
  typename OperatorType::ConstIterator o_it;
  OutputPixelType sum = NumericTraits< OutputPixelType >::Zero;

  o_it = op.Begin();
  const typename OperatorType::ConstIterator op_end = op.End();

  const unsigned int start  = static_cast< unsigned int >( s.start() );
  const unsigned int stride = static_cast< unsigned int >( s.stride() );
  for ( unsigned int i = start; o_it < op_end; i += stride, ++o_it )
    {
    sum += static_cast< OutputPixelType >( *o_it )
           * static_cast< OutputPixelType >( it.GetPixel(i) );
    }

  return sum;
}

template< class TImage, class TOperator, class TComputation >
typename NeighborhoodInnerProduct< TImage, TOperator, TComputation >::OutputPixelType
NeighborhoodInnerProduct< TImage, TOperator, TComputation >
::operator()(const std::slice & s,
             /*           const ImageBoundaryCondition<TImage> *,*/
             const NeighborhoodType & N,
             const OperatorType & op) const
{
  typename OperatorType::ConstIterator o_it;
  OutputPixelType sum = NumericTraits< OutputPixelType >::Zero;

  o_it = op.Begin();
  const typename OperatorType::ConstIterator op_end = op.End();

  const unsigned int start  = static_cast< unsigned int >( s.start() );
  const unsigned int stride = static_cast< unsigned int >( s.stride() );
  for ( unsigned int i = start; o_it < op_end; i += stride, ++o_it )
    {
    sum += static_cast< OutputPixelType >( *o_it )
           * static_cast< OutputPixelType >( N[i] );
    }

  return sum;
}
} // end namespace itk
#endif
