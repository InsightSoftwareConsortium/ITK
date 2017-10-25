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
#ifndef itkAdditiveGaussianNoiseImageFilter_hxx
#define itkAdditiveGaussianNoiseImageFilter_hxx

#include "itkAdditiveGaussianNoiseImageFilter.h"
#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"
#include "itkNormalVariateGenerator.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
AdditiveGaussianNoiseImageFilter<TInputImage, TOutputImage>
::AdditiveGaussianNoiseImageFilter() :
  m_Mean( 0.0 ),
  m_StandardDeviation( 1.0 )
{
}

template <class TInputImage, class TOutputImage>
void
AdditiveGaussianNoiseImageFilter<TInputImage, TOutputImage>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread,
                        ThreadIdType threadId)
{
  const InputImageType*  inputPtr = this->GetInput();
  OutputImageType*       outputPtr = this->GetOutput(0);

  // Create a random generator per thread
  typename Statistics::NormalVariateGenerator::Pointer randn = Statistics::NormalVariateGenerator::New();
  const uint32_t seed = Self::Hash( this->GetSeed(), threadId );
  // Convert the seed bit for bit to int32
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
      const double out = inputIt.Get() + m_Mean + m_StandardDeviation * randn->GetVariate();
      outputIt.Set( Self::ClampCast(out) );
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
AdditiveGaussianNoiseImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Mean: "
     << static_cast<typename NumericTraits<double>::PrintType>( m_Mean )
     << std::endl;
  os << indent << "StandardDeviation: "
     << static_cast<typename NumericTraits<double>::PrintType>( m_StandardDeviation )
     << std::endl;
}
} // end namespace itk

#endif
