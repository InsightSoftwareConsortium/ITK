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
#ifndef __itkNaryFunctorImageFilter_txx
#define __itkNaryFunctorImageFilter_txx

#include "itkNaryFunctorImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template< class TInputImage, class TOutputImage, class TFunction >
NaryFunctorImageFilter< TInputImage, TOutputImage, TFunction >
::NaryFunctorImageFilter()
{
  // This number will be incremented each time an image
  // is added over the two minimum required
  this->SetNumberOfRequiredInputs(2);
  this->InPlaceOff();
}

/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template< class TInputImage, class TOutputImage, class TFunction >
void
NaryFunctorImageFilter< TInputImage, TOutputImage, TFunction >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  const unsigned int numberOfInputImages =
    static_cast< unsigned int >( this->GetNumberOfInputs() );

  typedef ImageRegionConstIterator< TInputImage > ImageRegionConstIteratorType;
  std::vector< ImageRegionConstIteratorType * > inputItrVector;
  inputItrVector.reserve(numberOfInputImages);

  // support progress methods/callbacks.
  // count the number of inputs that are non-null
  for ( unsigned int i = 0; i < numberOfInputImages; ++i )
    {
    InputImagePointer inputPtr =
      dynamic_cast< TInputImage * >( ProcessObject::GetInput(i) );

    if ( inputPtr )
      {
      inputItrVector.push_back( new ImageRegionConstIteratorType(inputPtr, outputRegionForThread) );
      }
    }
  ProgressReporter progress( this, threadId,
                             outputRegionForThread.GetNumberOfPixels() );

  const unsigned int numberOfValidInputImages = inputItrVector.size();

  if ( numberOfValidInputImages == 0 )
    {
    //No valid regions in the thread
    //(and no region iterators to delete)
    return;
    }

  NaryArrayType naryInputArray(numberOfValidInputImages);

  OutputImagePointer                  outputPtr = this->GetOutput(0);
  ImageRegionIterator< TOutputImage > outputIt(outputPtr, outputRegionForThread);

  typename std::vector< ImageRegionConstIteratorType * >::iterator regionIterators;
  const typename std::vector< ImageRegionConstIteratorType * >::const_iterator regionItEnd =
    inputItrVector.end();

  typename NaryArrayType::iterator arrayIt;

  while ( !outputIt.IsAtEnd() )
    {
    arrayIt = naryInputArray.begin();
    regionIterators = inputItrVector.begin();
    while ( regionIterators != regionItEnd )
      {
      *arrayIt++ = ( *regionIterators )->Get();
      ++( *( *regionIterators ) );
      ++regionIterators;
      }
    outputIt.Set( m_Functor(naryInputArray) );
    ++outputIt;
    progress.CompletedPixel();
    }

  // Free memory
  regionIterators = inputItrVector.begin();
  while ( regionIterators != regionItEnd )
    {
    delete ( *regionIterators++ );
    }
}
} // end namespace itk

#endif
