/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
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
