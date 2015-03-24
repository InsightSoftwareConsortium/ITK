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
#ifndef itkFFTShiftImageFilter_hxx
#define itkFFTShiftImageFilter_hxx

#include "itkFFTShiftImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
FFTShiftImageFilter< TInputImage, TOutputImage >
::FFTShiftImageFilter()
{
  m_Inverse = false;
}

template< typename TInputImage, typename TOutputImage >
void
FFTShiftImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // The size of the image is needed to compute the shift.
  const SizeType size = this->GetOutput()->GetLargestPossibleRegion().GetSize();

  // Compute the shift.
  typename Superclass::OffsetType shift;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    shift[i] = (size[i] / 2);
    if ( m_Inverse )
      {
      shift[i] = -shift[i];
      }
    }

  this->m_Shift = shift;

  this->Superclass::GenerateData();
}

template< typename TInputImage, typename TOutputImage >
void
FFTShiftImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Inverse: "  << m_Inverse << std::endl;
}

} // end namespace itk
#endif
