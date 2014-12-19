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
#ifndef itkVectorNeighborhoodInnerProduct_hxx
#define itkVectorNeighborhoodInnerProduct_hxx
#include "itkVectorNeighborhoodInnerProduct.h"

namespace itk
{
template< typename TImage >
typename VectorNeighborhoodInnerProduct< TImage >::PixelType
VectorNeighborhoodInnerProduct< TImage >
::operator()(const std::slice & s,
             const ConstNeighborhoodIterator< TImage > & it,
             const OperatorType & op) const
{
  PixelType    sum;
  unsigned int j;

  typename OperatorType::ConstIterator o_it;

  for ( j = 0; j < VectorDimension; ++j )
    {
    sum[j] = NumericTraits< ScalarValueType >::ZeroValue();
    }

  o_it = op.Begin();
  const typename OperatorType::ConstIterator op_end = op.End();

  const unsigned int start  = static_cast< unsigned int >( s.start() );
  const unsigned int stride = static_cast< unsigned int >( s.stride() );
  for ( unsigned int i = start; o_it < op_end; i += stride, ++o_it )
    {
    for ( j = 0; j < VectorDimension; ++j )
      {
      sum[j] += *o_it * ( it.GetPixel(i) )[j];
      }
    }

  return sum;
}

template< typename TImage >
typename VectorNeighborhoodInnerProduct< TImage >::PixelType
VectorNeighborhoodInnerProduct< TImage >
::operator()(const std::slice & s,
             const NeighborhoodType & it,
             const OperatorType & op) const
{
  PixelType    sum;
  unsigned int j;

  typename OperatorType::ConstIterator o_it;

  for ( j = 0; j < VectorDimension; ++j )
    {
    sum[j] = NumericTraits< ScalarValueType >::ZeroValue();
    }

  o_it = op.Begin();
  const typename OperatorType::ConstIterator op_end = op.End();

  const unsigned int start  = static_cast< unsigned int >( s.start() );
  const unsigned int stride = static_cast< unsigned int >( s.stride() );
  for ( unsigned int i = start; o_it < op_end; i += stride, ++o_it )
    {
    for ( j = 0; j < VectorDimension; ++j )
      {
      sum[j] += *o_it * it[i][j];
      }
    }

  return sum;
}
} // end namespace itk
#endif
