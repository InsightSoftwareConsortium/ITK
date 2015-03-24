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
#ifndef itkCyclicShiftImageFilter_hxx
#define itkCyclicShiftImageFilter_hxx

#include "itkCyclicShiftImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
CyclicShiftImageFilter< TInputImage, TOutputImage >
::CyclicShiftImageFilter()
{
  m_Shift.Fill( NumericTraits< OffsetValueType >::ZeroValue() );
}

template< typename TInputImage, typename TOutputImage >
void
CyclicShiftImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method.
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( !input )
    {
    return;
    }
  input->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
CyclicShiftImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // Setup the progress reporter.
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );


  const InputImageType * inputImage = this->GetInput();

  // The index and size of the image needed to compute the shift
  const IndexType outIdx = this->GetOutput()->GetLargestPossibleRegion().GetIndex();
  const SizeType  outSize = this->GetOutput()->GetLargestPossibleRegion().GetSize();

  // Now iterate over the pixels of the output region for this thread.
  ImageRegionIteratorWithIndex< OutputImageType > outIt(this->GetOutput(), outputRegionForThread);
  for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
    {
    IndexType index = outIt.GetIndex();

    for ( unsigned int i = 0; i < ImageDimension; ++i )
      {
      IndexValueType shiftedIdx = ( index[i] - outIdx[i] - m_Shift[i] )
        % static_cast< OffsetValueType >(outSize[i]);
      if ( shiftedIdx < 0 )
        {
        shiftedIdx += outSize[i];
        }
      index[i] = shiftedIdx + outIdx[i];
      }

    outIt.Set( static_cast< OutputImagePixelType >( inputImage->GetPixel( index ) ) );
    progress.CompletedPixel();
    }
}

template< typename TInputImage, typename TOutputImage >
void
CyclicShiftImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Shift: " << m_Shift << std::endl;
}

} // end namespace itk
#endif
