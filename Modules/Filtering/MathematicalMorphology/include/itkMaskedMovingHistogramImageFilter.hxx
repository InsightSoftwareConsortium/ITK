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
#ifndef itkMaskedMovingHistogramImageFilter_hxx
#define itkMaskedMovingHistogramImageFilter_hxx

#include "itkMaskedMovingHistogramImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"
#include "itkNumericTraits.h"

#include "itkImageRegionIterator.h"
#include "itkImageLinearConstIteratorWithIndex.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 */


namespace itk
{
template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel, typename THistogram >
MaskedMovingHistogramImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel, THistogram >
::MaskedMovingHistogramImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  this->m_FillValue = NumericTraits< OutputPixelType >::ZeroValue();
  this->m_MaskValue = NumericTraits< MaskPixelType >::max();
  this->m_BackgroundMaskValue = NumericTraits< MaskPixelType >::ZeroValue();
  this->m_GenerateOutputMask = true;
  this->SetGenerateOutputMask(false);
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel, typename THistogram >
void
MaskedMovingHistogramImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel, THistogram >
::SetGenerateOutputMask(bool generateOutputMask)
{
  if ( generateOutputMask != this->m_GenerateOutputMask )
    {
    this->m_GenerateOutputMask = generateOutputMask;
    if ( generateOutputMask )
      {
      this->SetNumberOfRequiredOutputs(2);
      typename MaskImageType::Pointer maskout = TMaskImage::New();
      this->SetNthOutput( 1, maskout.GetPointer() );
      }
    else
      {
      this->SetNumberOfRequiredOutputs(1);
      this->SetNthOutput(1, ITK_NULLPTR);
      }
    }
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel, typename THistogram >
void
MaskedMovingHistogramImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel, THistogram >
::AllocateOutputs()
{
  if ( this->m_GenerateOutputMask )
    {
    // Allocate the output image.
    typename TOutputImage::Pointer output = this->GetOutput();
    output->SetBufferedRegion( output->GetRequestedRegion() );
    output->Allocate();
    // Allocate the output mask image.
    typename TMaskImage::Pointer mask = this->GetOutputMask();
    mask->SetBufferedRegion( mask->GetRequestedRegion() );
    mask->Allocate();
    }
  else
    {
    Superclass::AllocateOutputs();
    }
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel, typename THistogram >
DataObject::Pointer
MaskedMovingHistogramImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel, THistogram >
::MakeOutput(DataObjectPointerArraySizeType idx)
{
  DataObject::Pointer output;

  switch ( idx )
    {
    case 0:
      output = ( TOutputImage::New() ).GetPointer();
      break;
    case 1:
      output = ( TMaskImage::New() ).GetPointer();
      break;
    }
  return output.GetPointer();
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel, typename THistogram >
typename MaskedMovingHistogramImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel,
                                           THistogram >::MaskImageType *
MaskedMovingHistogramImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel, THistogram >
::GetOutputMask(void)
{
  typename MaskImageType::Pointer res = dynamic_cast< MaskImageType * >( this->ProcessObject::GetOutput(1) );
  return res;
}

// a modified version that uses line iterators and only moves the
// histogram in one direction. Hopefully it will be a bit simpler and
// faster due to improved memory access and a tighter loop.
template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel, typename THistogram >
void
MaskedMovingHistogramImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel, THistogram >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // instantiate the histogram
  HistogramType histogram;
  this->ConfigureHistogram( histogram );

  OutputImageType *     outputImage = this->GetOutput();
  MaskImageType *       outputMask = this->GetOutputMask();
  const InputImageType *inputImage = this->GetInput();
  const MaskImageType * maskImage = this->GetMaskImage();

  RegionType inputRegion = inputImage->GetRequestedRegion();

  // initialize the histogram
  for ( typename OffsetListType::iterator listIt = this->m_KernelOffsets.begin();
        listIt != this->m_KernelOffsets.end(); listIt++ )
    {
    IndexType idx = outputRegionForThread.GetIndex() + ( *listIt );
    if ( inputRegion.IsInside(idx) && maskImage->GetPixel(idx) == m_MaskValue )
      {
      histogram.AddPixel( inputImage->GetPixel(idx) );
      }
    else
      {
      histogram.AddBoundary();
      }
    }

  // now move the histogram
  FixedArray< short, ImageDimension > direction;
  direction.Fill(1);
  int        axis = ImageDimension - 1;
  OffsetType offset;
  offset.Fill(0);
  RegionType stRegion;
  stRegion.SetSize( this->m_Kernel.GetSize() );
  stRegion.PadByRadius(1);   // must pad the region by one because of the
                             // translation

  OffsetType centerOffset;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    centerOffset[i] = stRegion.GetSize()[i] / 2;
    }

  int BestDirection = this->m_Axes[axis];
  int LineLength = inputRegion.GetSize()[BestDirection];

  // Report progress every line instead of every pixel
  ProgressReporter progress(this, threadId,
                            outputRegionForThread.GetNumberOfPixels() / outputRegionForThread.GetSize()[BestDirection]);
  // init the offset and get the lists for the best axis
  offset[BestDirection] = direction[BestDirection];
  // it's very important for performances to get a pointer and not a copy
  const OffsetListType *addedList = &this->m_AddedOffsets[offset];
  const OffsetListType *removedList = &this->m_RemovedOffsets[offset];

  typedef ImageLinearConstIteratorWithIndex< InputImageType > InputLineIteratorType;
  InputLineIteratorType InLineIt(inputImage, outputRegionForThread);
  InLineIt.SetDirection(BestDirection);
  InLineIt.GoToBegin();
  IndexType LineStart;
  InLineIt.GoToBegin();

  typedef typename std::vector< HistogramType > HistVecType;
  HistVecType HistVec(ImageDimension);
  typedef typename std::vector< IndexType > IndexVecType;
  IndexVecType PrevLineStartVec(ImageDimension);

  // Steps is used to keep track of the order in which the line
  // iterator passes over the various dimensions.
  int *Steps = new int[ImageDimension];

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    HistVec[i] = histogram;
    PrevLineStartVec[i] = InLineIt.GetIndex();
    Steps[i] = 0;
    }

  while ( !InLineIt.IsAtEnd() )
    {
    HistogramType & histRef = HistVec[BestDirection];
    IndexType      PrevLineStart = InLineIt.GetIndex();
    for ( InLineIt.GoToBeginOfLine(); !InLineIt.IsAtEndOfLine(); ++InLineIt )
      {
      // Update the histogram
      IndexType currentIdx = InLineIt.GetIndex();

      if ( maskImage->GetPixel(currentIdx) == m_MaskValue && histRef.IsValid() )
        {
        outputImage->SetPixel( currentIdx,
                               static_cast< OutputPixelType >( histRef.GetValue( inputImage->GetPixel(currentIdx) ) ) );
        if ( this->m_GenerateOutputMask )
          {
          outputMask->SetPixel(currentIdx, m_MaskValue);
          }
        }
      else
        {
        outputImage->SetPixel(currentIdx, m_FillValue);
        if ( this->m_GenerateOutputMask )
          {
          outputMask->SetPixel(currentIdx, m_BackgroundMaskValue);
          }
        }
      stRegion.SetIndex(currentIdx - centerOffset);
      pushHistogram(histRef, addedList, removedList, inputRegion,
                    stRegion, inputImage, maskImage, currentIdx);
      }
    Steps[BestDirection] += LineLength;
    InLineIt.NextLine();
    if ( InLineIt.IsAtEnd() )
      {
      break;
      }
    LineStart = InLineIt.GetIndex();
    // This section updates the histogram for the next line
    // Since we aren't zig zagging we need to figure out which
    // histogram to update and the direction in which to push
    // it. Then we need to copy that histogram to the relevant
    // places
    OffsetType LineOffset, Changes;
    // Figure out which stored histogram to move and in
    // which direction
    int LineDirection = 0;
    // This function deals with changing planes etc
    this->GetDirAndOffset(LineStart, PrevLineStart,
                          LineOffset, Changes, LineDirection);
    ++( Steps[LineDirection] );
    IndexType             PrevLineStartHist = LineStart - LineOffset;
    const OffsetListType *addedListLine = &this->m_AddedOffsets[LineOffset];
    const OffsetListType *removedListLine = &this->m_RemovedOffsets[LineOffset];
    HistogramType &       tmpHist = HistVec[LineDirection];
    stRegion.SetIndex(PrevLineStart - centerOffset);
    // Now move the histogram
    pushHistogram(tmpHist, addedListLine, removedListLine, inputRegion,
                  stRegion, inputImage, maskImage, PrevLineStartHist);

    //PrevLineStartVec[LineDirection] = LineStart;
    // copy the updated histogram and line start entries to the
    // relevant directions. When updating direction 2, for example,
    // new copies of directions 0 and 1 should be made.
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if ( Steps[i] > Steps[LineDirection] )
        {
        // make sure this is the right thing to do
        HistVec[i] = HistVec[LineDirection];
        }
      }
    progress.CompletedPixel();
    }
  delete[] Steps;
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel, typename THistogram >
void
MaskedMovingHistogramImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel, THistogram >
::pushHistogram(HistogramType & histogram,
                const OffsetListType *addedList,
                const OffsetListType *removedList,
                const RegionType & inputRegion,
                const RegionType & kernRegion,
                const InputImageType *inputImage,
                const MaskImageType *maskImage,
                const IndexType currentIdx)
{
  if ( inputRegion.IsInside(kernRegion) )
    {
    // update the histogram
    for ( typename OffsetListType::const_iterator addedIt = addedList->begin();
          addedIt != addedList->end(); addedIt++ )
      {
      typename InputImageType::IndexType idx = currentIdx + ( *addedIt );
      if ( maskImage->GetPixel(idx) == m_MaskValue )
        {
        histogram.AddPixel( inputImage->GetPixel(idx) );
        }
      else
        {
        histogram.AddBoundary();
        }
      }
    for ( typename OffsetListType::const_iterator removedIt = removedList->begin();
          removedIt != removedList->end(); removedIt++ )
      {
      typename InputImageType::IndexType idx = currentIdx + ( *removedIt );
      if ( maskImage->GetPixel(idx) == m_MaskValue )
        {
        histogram.RemovePixel( inputImage->GetPixel(idx) );
        }
      else
        {
        histogram.RemoveBoundary();
        }
      }
    }
  else
    {
    // update the histogram
    for ( typename OffsetListType::const_iterator addedIt = addedList->begin();
          addedIt != addedList->end(); addedIt++ )
      {
      IndexType idx = currentIdx + ( *addedIt );
      if ( inputRegion.IsInside(idx) && maskImage->GetPixel(idx) == m_MaskValue )
        {
        histogram.AddPixel( inputImage->GetPixel(idx) );
        }
      else
        {
        histogram.AddBoundary();
        }
      }
    for ( typename OffsetListType::const_iterator removedIt = removedList->begin();
          removedIt != removedList->end(); removedIt++ )
      {
      IndexType idx = currentIdx + ( *removedIt );
      if ( inputRegion.IsInside(idx) && maskImage->GetPixel(idx) == m_MaskValue )
        {
        histogram.RemovePixel( inputImage->GetPixel(idx) );
        }
      else
        {
        histogram.RemoveBoundary();
        }
      }
    }
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel, typename THistogram >

void
MaskedMovingHistogramImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel, THistogram >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "GenerateOutputMask: "  << m_GenerateOutputMask << std::endl;
  os << indent << "FillValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_FillValue ) << std::endl;
  os << indent << "MaskValue: "  << static_cast< typename NumericTraits< MaskPixelType >::PrintType >( m_MaskValue )
     << std::endl;
  os << indent << "BackgroundMaskValue: "
     << static_cast< typename NumericTraits< MaskPixelType >::PrintType >( m_BackgroundMaskValue ) << std::endl;
}
} // end namespace itk
#endif
