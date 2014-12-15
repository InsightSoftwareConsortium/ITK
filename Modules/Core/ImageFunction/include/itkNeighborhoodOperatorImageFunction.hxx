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
#ifndef itkNeighborhoodOperatorImageFunction_hxx
#define itkNeighborhoodOperatorImageFunction_hxx

#include "itkNeighborhoodOperatorImageFunction.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
/** Set the Input Image */
template< typename TInputImage, typename TOutput >
NeighborhoodOperatorImageFunction< TInputImage, TOutput >
::NeighborhoodOperatorImageFunction()
{}

/** Print self method */
template< typename TInputImage, typename TOutput >
void
NeighborhoodOperatorImageFunction< TInputImage, TOutput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Applying Operator Function:" << std::endl;
}

/** Evaluate the function at the specifed point */
template< typename TInputImage, typename TOutput >
TOutput
NeighborhoodOperatorImageFunction< TInputImage, TOutput >
::EvaluateAtIndex(const IndexType & index) const
{
  NeighborhoodInnerProduct< InputImageType, TOutput, TOutput > smartInnerProduct;
  ConstNeighborhoodIterator< InputImageType >                  bit;
  bit = ConstNeighborhoodIterator< InputImageType >( m_Operator.GetRadius(), this->GetInputImage(), this->GetInputImage(
                                                       )->GetRequestedRegion() );
  bit.SetLocation(index);

  return smartInnerProduct(bit, m_Operator);
}
} // end namespace itk

#endif
