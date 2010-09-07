/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMovingHistogramImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMovingHistogramImageFilter_txx
#define __itkMovingHistogramImageFilter_txx

#include "itkMovingHistogramImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"
#include "itkNumericTraits.h"

#ifndef zigzag

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageLinearConstIteratorWithIndex.h"

#endif

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel, class THistogram >
MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::MovingHistogramImageFilter()
{}

template< class TInputImage, class TOutputImage, class TKernel, class THistogram >
THistogram *
MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::NewHistogram()
{
  return new THistogram();
}

#ifdef zigzag
template< class TInputImage, class TOutputImage, class TKernel, class THistogram >
void
MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, int threadId)
{
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // instanciate the histogram
  THistogram *histogram = this->NewHistogram();

  OutputImageType *     outputImage = this->GetOutput();
  const InputImageType *inputImage = this->GetInput();
  RegionType            inputRegion = inputImage->GetRequestedRegion();

  // initialize the histogram
  for ( typename OffsetListType::iterator listIt = this->m_KernelOffsets.begin();
        listIt != this->m_KernelOffsets.end();
        listIt++ )
    {
    IndexType idx = outputRegionForThread.GetIndex() + ( *listIt );
    if ( inputRegion.IsInside(idx) )
              { histogram->AddPixel( inputImage->GetPixel(idx) ); }
    else
              { histogram->AddBoundary(); }
    }
  // and set the first point of the image
  outputImage->SetPixel( outputRegionForThread.GetIndex(),
                         static_cast< OutputPixelType >( histogram->GetValue( inputImage->GetPixel(
outputRegionForThread.GetIndex() ) ) ) );
  progress.CompletedPixel();

  // now move the histogram
  FixedArray< short, ImageDimension > direction;
  direction.Fill(1);
  IndexType  currentIdx = outputRegionForThread.GetIndex();
  int        axis = ImageDimension - 1;
  OffsetType offset;
  offset.Fill(0);
  RegionType stRegion;
  stRegion.SetSize( this->m_Kernel.GetSize() );
  stRegion.PadByRadius(1);   // must pad the region by one because of the
                             // translation

  OffsetType centerOffset;
  for ( int axis = 0; axis < ImageDimension; axis++ )
    {
    centerOffset[axis] = stRegion.GetSize()[axis] / 2;
    }

  // init the offset and get the lists for the best axis
  offset[this->m_Axes[axis]] = direction[this->m_Axes[axis]];
  // it's very important for performances to get a pointer and not a copy
  const OffsetListType *addedList = &this->m_AddedOffsets[offset];
  const OffsetListType *removedList = &this->m_RemovedOffsets[offset];

  while ( axis >= 0 )
    {
    if ( outputRegionForThread.IsInside(currentIdx + offset) )
      {
      stRegion.SetIndex(currentIdx + offset - centerOffset);
      if ( inputRegion.IsInside(stRegion) )
        {
        // update the histogram
        for ( typename OffsetListType::const_iterator addedIt = addedList->begin();
              addedIt != addedList->end();
              addedIt++ )
          {
          histogram->AddPixel( inputImage->GetPixel( currentIdx + ( *addedIt ) ) );
          }
        for ( typename OffsetListType::const_iterator removedIt = removedList->begin();
              removedIt != removedList->end();
              removedIt++ )
          {
          histogram->RemovePixel( inputImage->GetPixel( currentIdx + ( *removedIt ) ) );
          }
        }
      else
        {
        // update the histogram
        for ( typename OffsetListType::const_iterator addedIt = addedList->begin();
              addedIt != addedList->end();
              addedIt++ )
          {
          IndexType idx = currentIdx + ( *addedIt );
          if ( inputRegion.IsInside(idx) )
                    { histogram->AddPixel( inputImage->GetPixel(idx) ); }
          else
                    { histogram->AddBoundary(); }
          }
        for ( typename OffsetListType::const_iterator removedIt = removedList->begin();
              removedIt != removedList->end();
              removedIt++ )
          {
          IndexType idx = currentIdx + ( *removedIt );
          if ( inputRegion.IsInside(idx) )
                    { histogram->RemovePixel( inputImage->GetPixel(idx) ); }
          else
                    { histogram->RemoveBoundary(); }
          }
        }

      OutputPixelType value = static_cast< OutputPixelType >( histogram->GetValue( inputImage->GetPixel(currentIdx) ) );

      // store the new index
      currentIdx += offset;

      outputImage->SetPixel(currentIdx, value);
      progress.CompletedPixel();

      if ( axis != ImageDimension - 1 )
        {
        offset[this->m_Axes[axis]] = 0;
        // the axis must be the last one
        axis = ImageDimension - 1;
        offset[this->m_Axes[axis]] = direction[this->m_Axes[axis]];
        addedList = &this->m_AddedOffsets[offset];
        removedList = &this->m_RemovedOffsets[offset];
        }
      }
    else
      {
      // the next offset is not in the right region,
      // we need to switch to another axis

      // invert the direction of the current axis
      direction[this->m_Axes[axis]] *= -1;
      // set the offset of the current axis to 0
      // -> offset == [0]*dim
      offset[this->m_Axes[axis]] = 0;
      // and switch to another axis
      axis--;

      if ( axis >= 0 )
        {
        offset[this->m_Axes[axis]] = direction[this->m_Axes[axis]];
        addedList = &this->m_AddedOffsets[offset];
        removedList = &this->m_RemovedOffsets[offset];
        }
      }
    }
}

#else
// a modified version that uses line iterators and only moves the
// histogram in one direction. Hopefully it will be a bit simpler and
// faster due to improved memory access and a tighter loop.
template< class TInputImage, class TOutputImage, class TKernel, class THistogram >
void
MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       int threadId)
{
  // instantiate the histogram
  HistogramType *histogram = this->NewHistogram();

  OutputImageType *     outputImage = this->GetOutput();
  const InputImageType *inputImage = this->GetInput();
  RegionType            inputRegion = inputImage->GetRequestedRegion();

  // initialize the histogram
  for ( typename OffsetListType::iterator listIt = this->m_KernelOffsets.begin();
        listIt != this->m_KernelOffsets.end();
        listIt++ )
    {
    IndexType idx = outputRegionForThread.GetIndex() + ( *listIt );
    if ( inputRegion.IsInside(idx) )
              { histogram->AddPixel( inputImage->GetPixel(idx) ); }
    else
              { histogram->AddBoundary(); }
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

  OffsetType   centerOffset;
  unsigned int i;
  for ( i = 0; i < ImageDimension; i++ )
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

  typedef ImageRegionIterator< OutputImageType > OutputIteratorType;
  //OutputIteratorType oit(outputImage, outputRegionForThread);
  InLineIt.GoToBegin();
  IndexType LineStart;
  //PrevLineStart = InLineIt.GetIndex();
  InLineIt.GoToBegin();

  typedef typename std::vector< HistogramType * > HistVecType;
  HistVecType HistVec(ImageDimension);
  typedef typename std::vector< IndexType > IndexVecType;
  IndexVecType PrevLineStartVec(ImageDimension);

  // Steps is used to keep track of the order in which the line
  // iterator passes over the various dimensions.
  int *Steps = new int[ImageDimension];

  for ( i = 0; i < ImageDimension; i++ )
    {
    HistVec[i] = histogram->Clone();
    PrevLineStartVec[i] = InLineIt.GetIndex();
    Steps[i] = 0;
    }

  while ( !InLineIt.IsAtEnd() )
    {
    HistogramType *histRef = HistVec[BestDirection];
    IndexType      PrevLineStart = InLineIt.GetIndex();
    for ( InLineIt.GoToBeginOfLine(); !InLineIt.IsAtEndOfLine(); ++InLineIt )
      {
      // Update the historgram
      IndexType currentIdx = InLineIt.GetIndex();
      outputImage->SetPixel( currentIdx,
                             static_cast< OutputPixelType >( histRef->GetValue( inputImage->GetPixel(currentIdx) ) ) );
      stRegion.SetIndex(currentIdx - centerOffset);
      PushHistogram(histRef, addedList, removedList, inputRegion,
                    stRegion, inputImage, currentIdx);
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
    GetDirAndOffset(LineStart, PrevLineStart,
                    LineOffset, Changes, LineDirection);
    ++( Steps[LineDirection] );
    IndexType             PrevLineStartHist = LineStart - LineOffset;
    const OffsetListType *addedListLine = &this->m_AddedOffsets[LineOffset];
    const OffsetListType *removedListLine = &this->m_RemovedOffsets[LineOffset];
    HistogramType *       tmpHist = HistVec[LineDirection];
    stRegion.SetIndex(PrevLineStart - centerOffset);
    // Now move the histogram
    PushHistogram(tmpHist, addedListLine, removedListLine, inputRegion,
                  stRegion, inputImage, PrevLineStartHist);

    //PrevLineStartVec[LineDirection] = LineStart;
    // copy the updated histogram and line start entries to the
    // relevant directions. When updating direction 2, for example,
    // new copies of directions 0 and 1 should be made.
    for ( i = 0; i < ImageDimension; i++ )
      {
      if ( Steps[i] > Steps[LineDirection] )
        {
        //PrevLineStartVec[i] = LineStart;
        delete ( HistVec[i] );
        HistVec[i] = HistVec[LineDirection]->Clone();
        }
      }
    progress.CompletedPixel();
    }
  for ( i = 0; i < ImageDimension; i++ )
    {
    delete ( HistVec[i] );
    }
  delete[] Steps;
  delete histogram;
}

template< class TInputImage, class TOutputImage, class TKernel, class THistogram >
void
MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::PushHistogram(HistogramType *histogram,
                const OffsetListType *addedList,
                const OffsetListType *removedList,
                const RegionType & inputRegion,
                const RegionType & kernRegion,
                const InputImageType *inputImage,
                const IndexType currentIdx)
{
  if ( inputRegion.IsInside(kernRegion) )
    {
    // update the histogram
    for ( typename OffsetListType::const_iterator addedIt = addedList->begin(); addedIt != addedList->end(); addedIt++ )
      {
      histogram->AddPixel( inputImage->GetPixel( currentIdx + ( *addedIt ) ) );
      }
    for ( typename OffsetListType::const_iterator removedIt = removedList->begin();
          removedIt != removedList->end();
          removedIt++ )
      {
      histogram->RemovePixel( inputImage->GetPixel( currentIdx + ( *removedIt ) ) );
      }
    }
  else
    {
    // update the histogram
    for ( typename OffsetListType::const_iterator addedIt = addedList->begin(); addedIt != addedList->end(); addedIt++ )
      {
      IndexType idx = currentIdx + ( *addedIt );
      if ( inputRegion.IsInside(idx) )
                { histogram->AddPixel( inputImage->GetPixel(idx) ); }
      else
                { histogram->AddBoundary(); }
      }
    for ( typename OffsetListType::const_iterator removedIt = removedList->begin();
          removedIt != removedList->end();
          removedIt++ )
      {
      IndexType idx = currentIdx + ( *removedIt );
      if ( inputRegion.IsInside(idx) )
                { histogram->RemovePixel( inputImage->GetPixel(idx) ); }
      else
                { histogram->RemoveBoundary(); }
      }
    }
}

template< class TInputImage, class TOutputImage, class TKernel, class THistogram >
void

MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::PrintHistogram(const HistogramType & H)
{
  /*  std::cout << "Hist = " ;
  typename HistogramType::const_iterator mapIt;
  for (mapIt = H.begin(); mapIt != H.end(); mapIt++)
    {
    std::cout << "V= " << int(mapIt->first) << " C= " << int(mapIt->second) << " ";
    }
  std::cout << std::endl;*/
}

#endif
} // end namespace itk
#endif
