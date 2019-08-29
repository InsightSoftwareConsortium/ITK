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
#ifndef itkNeighborhoodInnerProduct_hxx
#define itkNeighborhoodInnerProduct_hxx
#include "itkNeighborhoodInnerProduct.h"

#include "itkNumericTraits.h"

namespace itk
{
template <typename TImage, typename TOperator, typename TComputation>
typename NeighborhoodInnerProduct<TImage, TOperator, TComputation>::OutputPixelType
NeighborhoodInnerProduct<TImage, TOperator, TComputation>::Compute(const ConstNeighborhoodIterator<TImage> & it,
                                                                   const OperatorType &                      op,
                                                                   const unsigned                            start,
                                                                   const unsigned                            stride)
{
  typename OperatorType::ConstIterator o_it;

  using InputPixelType = typename TImage::PixelType;
  using InputPixelRealType = typename NumericTraits<InputPixelType>::RealType;
  using AccumulateRealType = typename NumericTraits<InputPixelRealType>::AccumulateType;

  AccumulateRealType sum = NumericTraits<AccumulateRealType>::ZeroValue();

  using OutputPixelValueType = typename NumericTraits<OutputPixelType>::ValueType;

  o_it = op.Begin();
  const typename OperatorType::ConstIterator op_end = op.End();

  for (unsigned int i = start; o_it < op_end; i += stride, ++o_it)
  {
    sum += static_cast<AccumulateRealType>(static_cast<OutputPixelValueType>(*o_it) *
                                           static_cast<InputPixelRealType>(it.GetPixel(i)));
  }

  return static_cast<OutputPixelType>(sum);
}

template <typename TImage, typename TOperator, typename TComputation>
typename NeighborhoodInnerProduct<TImage, TOperator, TComputation>::OutputPixelType
NeighborhoodInnerProduct<TImage, TOperator, TComputation>::Compute(
  /*           const ImageBoundaryCondition<TImage> *,*/
  const NeighborhoodType & N,
  const OperatorType &     op,
  const unsigned           start,
  const unsigned           stride)
{
  typename OperatorType::ConstIterator o_it;

  using InputPixelType = typename TImage::PixelType;
  using InputPixelRealType = typename NumericTraits<InputPixelType>::RealType;
  using AccumulateRealType = typename NumericTraits<InputPixelRealType>::AccumulateType;

  AccumulateRealType sum = NumericTraits<AccumulateRealType>::ZeroValue();

  using OutputPixelValueType = typename NumericTraits<OutputPixelType>::ValueType;

  o_it = op.Begin();
  const typename OperatorType::ConstIterator op_end = op.End();

  for (unsigned int i = start; o_it < op_end; i += stride, ++o_it)
  {
    sum +=
      static_cast<AccumulateRealType>(static_cast<OutputPixelValueType>(*o_it) * static_cast<InputPixelRealType>(N[i]));
  }

  return static_cast<OutputPixelType>(sum);
}
} // end namespace itk
#endif
