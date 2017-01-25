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

#ifndef itkSpeckleNoiseImageFilter_hxx
#define itkSpeckleNoiseImageFilter_hxx

#include "itkSpeckleNoiseImageFilter.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
SpeckleNoiseImageFilter<TInputImage, TOutputImage>
::SpeckleNoiseImageFilter() :
  m_StandardDeviation( 1.0 )
{
}

template <class TInputImage, class TOutputImage>
void
SpeckleNoiseImageFilter<TInputImage, TOutputImage>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread, ThreadIdType threadId)
{
  const InputImageType* inputPtr = this->GetInput();
  OutputImageType*      outputPtr = this->GetOutput(0);

  // Create a random generator per thread
  typename Statistics::MersenneTwisterRandomVariateGenerator::Pointer rand =
    Statistics::MersenneTwisterRandomVariateGenerator::New();
  const uint32_t seed = Self::Hash( this->GetSeed(), threadId );
  rand->Initialize(seed);

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

  // Choose the value of the gamma distribution so that the mean is 1 and the
  // variance depends on the standard deviation
  const double theta = m_StandardDeviation * m_StandardDeviation;
  const double k = 1 / theta;

  const double floork = Math::Floor<double>( k );
  const double delta = k - floork;
  const double v0 = Math::e / ( Math::e + delta );

  while ( !inputIt.IsAtEnd() )
    {
    while ( !inputIt.IsAtEndOfLine() )
      {
      // First generate the gamma distributed random variable
      // ref http://en.wikipedia.org/wiki/Gamma_distribution#Generating_gamma-distributed_random_variables
      double xi;
      double nu;
      do
        {
        const double v1 = 1.0 - rand->GetVariateWithOpenUpperRange(); // open *lower* range -- (0,1]
        const double v2 = 1.0 - rand->GetVariateWithOpenUpperRange();
        const double v3 = 1.0 - rand->GetVariateWithOpenUpperRange();
        if( v1 <= v0 )
          {
          xi = std::pow( v2, 1 / delta );
          nu = v3 * std::pow( xi, delta - 1.0 );
          }
        else
          {
          xi = 1.0 - std::log( v2 );
          nu = v3 * std::exp( -xi );
          }
        }
      while( nu > std::exp( -xi ) * std::pow( xi, delta - 1.0 ) );
      double gamma = xi;
      for( int i = 0; i < floork; i++ )
        {
        gamma -= std::log( 1.0 - rand->GetVariateWithOpenUpperRange() );
        }
      gamma *= theta;
      // Apply multiplicative noise
      const double out = gamma * inputIt.Get();
      outputIt.Set( Self::ClampCast( out )  );
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
SpeckleNoiseImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "StandardDeviation: "
     << static_cast<typename NumericTraits<double>::PrintType>( m_StandardDeviation )
     << std::endl;
}
} // end namespace itk

#endif
