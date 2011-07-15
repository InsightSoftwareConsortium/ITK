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
#ifndef __itkFFTShiftImageFilter_hxx
#define __itkFFTShiftImageFilter_hxx

#include "itkFFTShiftImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
FFTShiftImageFilter< TInputImage, TOutputImage >
::FFTShiftImageFilter()
{
  m_Inverse = false;
}

template< class TInputImage, class TOutputImage >
void
FFTShiftImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( !input )
        { return; }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}

template< class TInputImage, class TOutputImage >
void
FFTShiftImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // setup the progress reporter
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // the index and size of the image needed to compute the shift
  const IndexType oIdx = this->GetOutput()->GetLargestPossibleRegion().GetIndex();
  const SizeType  oSize = this->GetOutput()->GetLargestPossibleRegion().GetSize();
  // the size of the segments for all axes
  SizeType seg1Size;
  SizeType seg2Size;

  // the center pixel is not computed the same way for the inverse shift in
  // case the size is odd, to restore the same image as before the shift
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( oSize[i] % 2 == 1 )
      {
      if ( !m_Inverse )
        {
        seg1Size[i] = oSize[i] / 2 + 1;
        seg2Size[i] = oSize[i] / 2;
        }
      else
        {
        seg1Size[i] = oSize[i] / 2;
        seg2Size[i] = oSize[i] / 2 + 1;
        }
      }
    else
      {
      seg1Size[i] = oSize[i] / 2;
      seg2Size[i] = oSize[i] / 2;
      }
    }

  // now iterate over the pixels of the output region for this thread
  ImageRegionIteratorWithIndex< OutputImageType > oIt(this->GetOutput(), outputRegionForThread);
  for ( oIt.GoToBegin(); !oIt.IsAtEnd(); ++oIt )
    {
    IndexType idx = oIt.GetIndex();

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if ( idx[i] <  (int)( oIdx[i] + seg2Size[i] ) )
        {
        idx[i] = idx[i] + seg1Size[i];
        }
      else
        {
        idx[i] = idx[i] - seg2Size[i];
        }
      }

    oIt.Set( static_cast< OutputImagePixelType >( this->GetInput()->GetPixel(idx) ) );
    progress.CompletedPixel();
    }
}

template< class TInputImage, class TOutputImage >
void
FFTShiftImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Inverse: "  << m_Inverse << std::endl;
}
} // end namespace itk
#endif
