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
#ifndef itkNoiseBaseImageFilter_hxx
#define itkNoiseBaseImageFilter_hxx

#include "itkNoiseBaseImageFilter.h"
#include <ctime>

namespace itk
{

template <class TInputImage, class TOutputImage>
NoiseBaseImageFilter<TInputImage, TOutputImage>
::NoiseBaseImageFilter()
  : m_Seed(0)
{
  Self::SetSeed();

  this->InPlaceOff();
}

template <class TInputImage, class TOutputImage>
NoiseBaseImageFilter<TInputImage, TOutputImage>
::~NoiseBaseImageFilter()
{
}

template <class TInputImage, class TOutputImage>
void
NoiseBaseImageFilter<TInputImage, TOutputImage>
::SetSeed()
{
  time_t t;

  time(&t);
  this->SetSeed(Hash(t,clock() ) );
}

template <class TInputImage, class TOutputImage>
typename NoiseBaseImageFilter<TInputImage, TOutputImage>::OutputImagePixelType
NoiseBaseImageFilter<TInputImage, TOutputImage>
::ClampCast(const double &value)
{
  if (value >= (double)NumericTraits<OutputImagePixelType>::max() )
    {
    return NumericTraits<OutputImagePixelType>::max();
    }
  else if(value <= (double)NumericTraits<OutputImagePixelType>::NonpositiveMin() )
    {
    return NumericTraits<OutputImagePixelType>::NonpositiveMin();
    }
  else if (NumericTraits<OutputImagePixelType>::is_integer)
    {
    return Math::Round<OutputImagePixelType>(value);
    }
  else
    {
    return static_cast<OutputImagePixelType>(value);
    }
}

template <class TInputImage, class TOutputImage>
void
NoiseBaseImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Seed: "
    << static_cast<typename NumericTraits<uint32_t>::PrintType>( m_Seed ) << std::endl;
}
} // end namespace itk

#endif
