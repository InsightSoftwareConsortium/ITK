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
#ifndef __itkCentralDifferenceImageFunction_hxx
#define __itkCentralDifferenceImageFunction_hxx

#include "itkCentralDifferenceImageFunction.h"

namespace itk
{
/**
 * Constructor
 */
template< class TInputImage, class TCoordRep >
CentralDifferenceImageFunction< TInputImage, TCoordRep >
::CentralDifferenceImageFunction()
{
  this->m_UseImageDirection = true;
}

/**
 *
 */
template< class TInputImage, class TCoordRep >
void
CentralDifferenceImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "UseImageDirection = " << this->m_UseImageDirection << std::endl;
}

/**
 *
 */
template< class TInputImage, class TCoordRep >
typename CentralDifferenceImageFunction< TInputImage, TCoordRep >::OutputType
CentralDifferenceImageFunction< TInputImage, TCoordRep >
::EvaluateAtIndex(const IndexType & index) const
{
  OutputType derivative;

  derivative.Fill(0.0);

  IndexType neighIndex = index;

  const InputImageType *inputImage = this->GetInputImage();

  const typename InputImageType::RegionType region =
    inputImage->GetBufferedRegion();

  const typename InputImageType::SizeType & size   = region.GetSize();
  const typename InputImageType::IndexType & start = region.GetIndex();

  const unsigned int MaxDims = Self::ImageDimension;
  for ( unsigned int dim = 0; dim < MaxDims; dim++ )
    {
    // bounds checking
    if ( index[dim] < start[dim] + 1
         || index[dim] > ( start[dim] + static_cast< OffsetValueType >( size[dim] ) - 2 ) )
      {
      derivative[dim] = 0.0;
      continue;
      }

    // compute derivative
    neighIndex[dim] += 1;
    derivative[dim] = inputImage->GetPixel(neighIndex);

    neighIndex[dim] -= 2;
    derivative[dim] -= inputImage->GetPixel(neighIndex);

    derivative[dim] *= 0.5 / inputImage->GetSpacing()[dim];
    neighIndex[dim] += 1;
    }

  if ( this->m_UseImageDirection )
    {
    OutputType orientedDerivative;
    inputImage->TransformLocalVectorToPhysicalVector(derivative, orientedDerivative);
    return orientedDerivative;
    }

  return ( derivative );
}
} // end namespace itk

#endif
