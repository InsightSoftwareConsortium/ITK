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
#ifndef itkShotNoiseImageFilter_hxx
#define itkShotNoiseImageFilter_hxx

#include "itkShotNoiseImageFilter.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"
#include "itkNormalVariateGenerator.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
ShotNoiseImageFilter<TInputImage, TOutputImage>
::ShotNoiseImageFilter() :
  m_Scale( 1.0 )
{
}

template <class TInputImage, class TOutputImage>
void
ShotNoiseImageFilter<TInputImage, TOutputImage>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread, ThreadIdType threadId)
{
  const InputImageType* inputPtr = this->GetInput();
  OutputImageType*      outputPtr = this->GetOutput(0);

  // Create a random generator per thread
  typename Statistics::MersenneTwisterRandomVariateGenerator::Pointer rand =
    Statistics::MersenneTwisterRandomVariateGenerator::New();
  const uint32_t seed = Self::Hash( this->GetSeed(), threadId );
  rand->Initialize(seed);
  typename Statistics::NormalVariateGenerator::Pointer randn = Statistics::NormalVariateGenerator::New();
  randn->Initialize(*reinterpret_cast<const int32_t*>( &seed ));

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;
  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // Define the iterators
  ImageScanlineConstIterator<TInputImage> inputIt(inputPtr, inputRegionForThread);
  ImageScanlineIterator<TOutputImage>     outputIt(outputPtr, outputRegionForThread);

  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels() );

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while ( !inputIt.IsAtEnd() )
    {
    while ( !inputIt.IsAtEndOfLine() )
      {
      const double in = m_Scale * inputIt.Get();

      // The value of >=50, is the lambda value in a Poisson
      // distribution where a Gaussian distribution is a "good"
      // approximation of the Poisson. This could be considered to be
      // exposed as an advanced parameter in the future.
      if( in < 50 )
        {
        const double L = std::exp( -in );
        long         k = 0;
        double       p = 1.0;

        do
          {
          k += 1;
          p *= rand->GetVariate();
          }
        while( p > L );

        // Clip the output to the actual supported range
        outputIt.Set( Self::ClampCast( (k-1)/m_Scale ) );
        }
      else
        {
        const double out = in + std::sqrt( in ) * randn->GetVariate();
        outputIt.Set( Self::ClampCast( out/m_Scale ) );
        }
      ++inputIt;
      ++outputIt;
      }
    inputIt.NextLine();
    outputIt.NextLine();
    progress.CompletedPixel();  // potential exception thrown here
    }
}

template <class TInputImage, class TOutputImage>
void
ShotNoiseImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Scale: "
     << static_cast<typename NumericTraits<double>::PrintType>( m_Scale )
     << std::endl;
}
} // end namespace itk

#endif
