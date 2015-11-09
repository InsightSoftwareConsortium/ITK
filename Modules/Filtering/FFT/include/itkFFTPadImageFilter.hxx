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

#ifndef itkFFTPadImageFilter_hxx
#define itkFFTPadImageFilter_hxx

#include "itkFFTPadImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkNumericTraits.h"
#include "itkPadImageFilter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkMath.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
FFTPadImageFilter<TInputImage, TOutputImage>
::FFTPadImageFilter()
{
  typedef Image< float, TInputImage::ImageDimension > RealImageType;
  typedef ForwardFFTImageFilter< RealImageType >      FFTFilterType;
  m_SizeGreatestPrimeFactor = FFTFilterType::New()->GetSizeGreatestPrimeFactor();
  Self::InternalSetBoundaryCondition( &m_DefaultBoundaryCondition );
}


template <class TInputImage, class TOutputImage>
void
FFTPadImageFilter<TInputImage, TOutputImage>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  const InputImageType * input0 = this->GetInput();
  OutputImageType * output0 = this->GetOutput();

  RegionType region0 = input0->GetLargestPossibleRegion();
  SizeType size;
  IndexType index;
  for( unsigned int i=0; i<ImageDimension; ++i )
    {
    SizeValueType padSize = 0;
    if( m_SizeGreatestPrimeFactor > 1 )
      {
      while( Math::GreatestPrimeFactor( region0.GetSize()[i] + padSize ) > m_SizeGreatestPrimeFactor )
        {
        ++padSize;
        }
      }
    else if( m_SizeGreatestPrimeFactor == 1 )
      {
      // make sure the total size is even
      padSize += ( region0.GetSize()[i] + padSize ) % 2;
      }
    index[i] = region0.GetIndex()[i] - padSize/2;
    size[i] = region0.GetSize()[i] + padSize;
    }
  RegionType region( index, size );
  output0->SetLargestPossibleRegion( region );
}


template<class TInputImage, class TOutputImage>
void
FFTPadImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "SizeGreatestPrimeFactor: "  << m_SizeGreatestPrimeFactor << std::endl;
}

}// end namespace itk
#endif
