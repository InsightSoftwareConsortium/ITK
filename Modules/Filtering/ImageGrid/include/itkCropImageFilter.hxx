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
#ifndef __itkCropImageFilter_hxx
#define __itkCropImageFilter_hxx

#include "itkCropImageFilter.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
void
CropImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  const TInputImage *inputPtr = this->GetInput();

  if ( !inputPtr )
    {
    return;
    }

  // Compute the new region size.
  OutputImageRegionType croppedRegion;
  SizeType              sz;
  OutputImageIndexType  idx;

  InputImageSizeType input_sz =
    inputPtr->GetLargestPossibleRegion().GetSize();
  InputImageIndexType input_idx =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  idx = input_idx + m_LowerBoundaryCropSize;
  sz  = input_sz  - ( m_UpperBoundaryCropSize + m_LowerBoundaryCropSize );

  croppedRegion.SetSize(sz);
  croppedRegion.SetIndex(idx);

  // Set extraction region in the superclass.
  this->SetExtractionRegion(croppedRegion);

  //
  Superclass::GenerateOutputInformation();
}

/**
 *
 */
template< class TInputImage, class TOutputImage >
void
CropImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "UpperBoundaryCropSize: " << m_UpperBoundaryCropSize
     << std::endl;
  os << indent << "LowerBoundaryCropSize: " << m_LowerBoundaryCropSize
     << std::endl;
}
} // end namespace itk

#endif
