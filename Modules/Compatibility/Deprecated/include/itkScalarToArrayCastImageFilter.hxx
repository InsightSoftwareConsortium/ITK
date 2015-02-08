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
#ifndef itkScalarToArrayCastImageFilter_hxx
#define itkScalarToArrayCastImageFilter_hxx
#if !defined( ITK_LEGACY_REMOVE )

#include "itkScalarToArrayCastImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"
#include "itkPixelTraits.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage  >
ScalarToArrayCastImageFilter< TInputImage, TOutputImage >
::ScalarToArrayCastImageFilter()
{
  this->SetNumberOfRequiredInputs
    (PixelTraits< OutputImagePixelType >::Dimension);
}

/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template< typename TInputImage, typename TOutputImage  >
void
ScalarToArrayCastImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  unsigned int length =
    PixelTraits< OutputImagePixelType >::Dimension;

  std::vector< const TInputImage * >                     inputs;
  std::vector< ImageRegionConstIterator< TInputImage > > i_iters;

  for ( unsigned int i = 0; i < length; i++ )
    {
    inputs.push_back( this->GetInput(i) );
    i_iters.push_back(
      ImageRegionConstIterator< TInputImage >
        (inputs[i], outputRegionForThread) );
    ( i_iters[i] ).GoToBegin();
    }

  typename TOutputImage::Pointer outputPtr = this->GetOutput(0);

  ImageRegionIterator< TOutputImage > outputIt(outputPtr, outputRegionForThread);

  ProgressReporter progress( this,
                             threadId,
                             outputRegionForThread.GetNumberOfPixels() );
  outputIt.GoToBegin();
  typename TOutputImage::PixelType arrayPixel;

  while ( !outputIt.IsAtEnd() )
    {
    for ( unsigned int j = 0; j < length; j++ )
      {
      arrayPixel[j] = ( i_iters[j] ).Get();
      ++( i_iters[j] );
      }
    outputIt.Set(arrayPixel);
    ++outputIt;
    progress.CompletedPixel();
    }
}
} // end namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
