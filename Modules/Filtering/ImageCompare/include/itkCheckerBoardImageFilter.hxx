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
#ifndef itkCheckerBoardImageFilter_hxx
#define itkCheckerBoardImageFilter_hxx

#include "itkCheckerBoardImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{

template< typename TImage >
CheckerBoardImageFilter< TImage >
::CheckerBoardImageFilter()
{
  m_CheckerPattern.Fill(4);
}

template< typename TImage >
void
CheckerBoardImageFilter< TImage >
::SetInput1(const InputImageType *image1)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput( 0, const_cast< InputImageType * >( image1 ) );
}

template< typename TImage >
void
CheckerBoardImageFilter< TImage >
::SetInput2(const InputImageType *image2)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput( 1, const_cast< InputImageType * >( image2 ) );
}

template< typename TImage >
void
CheckerBoardImageFilter< TImage >
::ThreadedGenerateData(
  const ImageRegionType & outputRegionForThread, ThreadIdType threadId)
{
  // Get the output pointers
  OutputImagePointer     outputPtr = this->GetOutput();
  InputImageConstPointer input1Ptr = this->GetInput(0);
  InputImageConstPointer input2Ptr = this->GetInput(1);

  // Create an iterator that will walk the output region for this thread.
  typedef ImageRegionIteratorWithIndex< OutputImageType >     OutputIterator;
  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputIterator;

  OutputIterator outItr(outputPtr, outputRegionForThread);
  InputIterator  in1Itr(input1Ptr, outputRegionForThread);
  InputIterator  in2Itr(input2Ptr, outputRegionForThread);

  outItr.GoToBegin();
  in1Itr.GoToBegin();
  in2Itr.GoToBegin();

  // Support for progress methods/callbacks
  ProgressReporter progress( this, threadId,
                             outputRegionForThread.GetNumberOfPixels() );

  typename InputImageType::SizeType size =
    input2Ptr->GetLargestPossibleRegion().GetSize();

  PatternArrayType factors;

  for ( unsigned int d = 0; d < ImageDimension; d++ )
    {
    factors[d] = size[d] / m_CheckerPattern[d];
    }

  typedef typename InputImageType::PixelType PixelType;
  typedef typename InputImageType::IndexType IndexType;

  PixelType pixval;

  // Walk the output region
  while ( !outItr.IsAtEnd() )
    {
    IndexType index = outItr.GetIndex();

    unsigned int sum = 0;

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      sum += static_cast< unsigned int >( index[i] / factors[i] );
      }

    if ( sum & 1 )
      {
      pixval = in2Itr.Get();
      }
    else
      {
      pixval = in1Itr.Get();
      }

    outItr.Set(pixval);

    progress.CompletedPixel();

    ++outItr;
    ++in1Itr;
    ++in2Itr;
    }
}

template< typename TImage >
void
CheckerBoardImageFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Checker pattern: " << m_CheckerPattern << std::endl;
}
} // end namespace itk

#endif
