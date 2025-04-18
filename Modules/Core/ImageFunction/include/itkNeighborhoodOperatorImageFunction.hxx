/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkNeighborhoodOperatorImageFunction_hxx
#define itkNeighborhoodOperatorImageFunction_hxx

#include "itkNeighborhoodInnerProduct.h"
#include "itkConstNeighborhoodIterator.h"

#include <cassert>

namespace itk
{

template <typename TInputImage, typename TOutput>
void
NeighborhoodOperatorImageFunction<TInputImage, TOutput>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Operator: " << static_cast<typename NumericTraits<NeighborhoodType>::PrintType>(m_Operator)
     << std::endl;
}

template <typename TInputImage, typename TOutput>
TOutput
NeighborhoodOperatorImageFunction<TInputImage, TOutput>::EvaluateAtIndex(const IndexType & index) const
{
  const NeighborhoodInnerProduct<InputImageType, TOutput, TOutput> smartInnerProduct;

  const TInputImage * const image = this->GetInputImage();
  assert(image != nullptr);
  ConstNeighborhoodIterator<InputImageType> bit(m_Operator.GetRadius(), image, image->GetRequestedRegion());
  bit.SetLocation(index);

  return smartInnerProduct(bit, m_Operator);
}
} // end namespace itk

#endif
