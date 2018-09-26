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
#ifndef itkConnectedComponentImageFilter_hxx
#define itkConnectedComponentImageFilter_hxx

#include "itkConnectedComponentImageFilter.h"

#include "itkImageScanlineIterator.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkMaskImageFilter.h"
#include "itkConnectedComponentAlgorithm.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( !input )
    {
    return;
    }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );

  MaskImagePointer mask = const_cast< MaskImageType * >( this->GetMaskImage() );
  if ( mask )
    {
    mask->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::BeforeThreadedGenerateData()
{
  typename TOutputImage::Pointer output = this->GetOutput();
  typename TInputImage::ConstPointer input = this->GetInput();
  typename TMaskImage::ConstPointer mask = this->GetMaskImage();

  this->SetupLineOffsets(false);

  using MaskFilterType =
      MaskImageFilter< TInputImage, TMaskImage, TInputImage >;
  typename MaskFilterType::Pointer maskFilter = MaskFilterType::New();
  if ( mask )
    {
    maskFilter->SetInput(input);
    maskFilter->SetMaskImage(mask);
    maskFilter->Update();
    m_Input = maskFilter->GetOutput();
    }
  else
    {
    m_Input = input;
    }

  ThreadIdType nbOfThreads = this->GetNumberOfWorkUnits();
  if ( MultiThreaderBase::GetGlobalMaximumNumberOfThreads() != 0 )
    {
    nbOfThreads = std::min( this->GetNumberOfWorkUnits(), MultiThreaderBase::GetGlobalMaximumNumberOfThreads() );
    }
  // number of threads can be constrained by the region size, so call the
  // SplitRequestedRegion
  // to get the real number of threads which will be used
  typename TOutputImage::RegionType splitRegion;  // dummy region - just to call
                                                  // the following method
  nbOfThreads = this->SplitRequestedRegion(0, nbOfThreads, splitRegion);

  // set up the vars used in the threads
  m_NumberOfLabels.clear();
  m_NumberOfLabels.resize(nbOfThreads, 0);
  m_Barrier = Barrier::New();
  m_Barrier->Initialize(nbOfThreads);
  const SizeValueType pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  const SizeValueType xsize = output->GetRequestedRegion().GetSize()[0];
  const SizeValueType linecount = pixelcount / xsize;
  m_LineMap.resize(linecount);
  m_FirstLineIdToJoin.resize(nbOfThreads - 1);
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  typename TOutputImage::Pointer output = this->GetOutput();
  typename TMaskImage::ConstPointer mask = this->GetMaskImage();

  const auto nbOfThreads = static_cast<ThreadIdType>( m_NumberOfLabels.size() );

  // create a line iterator
  using InputLineIteratorType = ImageScanlineConstIterator< InputImageType >;
  InputLineIteratorType inLineIt(m_Input, outputRegionForThread);

  // set the progress reporter to deal with the number of lines
  const SizeValueType pixelcountForThread = outputRegionForThread.GetNumberOfPixels();
  const SizeValueType xsizeForThread = outputRegionForThread.GetSize()[0];
  const SizeValueType linecountForThread = pixelcountForThread / xsizeForThread;
  ProgressReporter    progress(this, threadId, linecountForThread * 2);

  // find the split axis
  const IndexType outputRegionIdx = output->GetRequestedRegion().GetIndex();
  const IndexType outputRegionForThreadIdx = outputRegionForThread.GetIndex();
  SizeType  outputRegionSize = output->GetRequestedRegion().GetSize();
  SizeType  outputRegionForThreadSize = outputRegionForThread.GetSize();
  int             splitAxis = 0;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( outputRegionSize[i] != outputRegionForThreadSize[i] )
      {
      splitAxis = i;
      }
    }

  // compute the number of pixels before that threads
  outputRegionSize[splitAxis] = outputRegionForThreadIdx[splitAxis] - outputRegionIdx[splitAxis];
  using LineIdType = SizeValueType;
  LineIdType firstLineIdForThread = RegionType(outputRegionIdx, outputRegionSize).GetNumberOfPixels() / xsizeForThread;
  LineIdType lineId = firstLineIdForThread;

  LineIdType nbOfLabels = 0;
  for ( inLineIt.GoToBegin();
        !inLineIt.IsAtEnd();
        inLineIt.NextLine() )
    {
    LineEncodingType thisLine;
    while ( !inLineIt.IsAtEndOfLine() )
      {
      const InputPixelType PVal = inLineIt.Get();
      //std::cout << inLineIt.GetIndex() << std::endl;
      if ( PVal != NumericTraits< InputPixelType >::ZeroValue( PVal ) )
        {
        // We've hit the start of a run
        const IndexType thisIndex = inLineIt.GetIndex();
        //std::cout << thisIndex << std::endl;
        SizeValueType length = 1;
        ++inLineIt;
        while ( !inLineIt.IsAtEndOfLine() && inLineIt.Get() != NumericTraits< InputPixelType >::ZeroValue( PVal ) )
          {
          ++length;
          ++inLineIt;
          }
        // create the run length object to go in the vector
        RunLength thisRun = { length, thisIndex, 0 };
        thisLine.push_back(thisRun);
        nbOfLabels++;
        }
      else
        {
        ++inLineIt;
        }
      }
    m_LineMap[lineId] = thisLine;
    lineId++;
    progress.CompletedPixel();
    }

  m_NumberOfLabels[threadId] = nbOfLabels;

  // wait for the other threads to complete that part
  this->Wait();

  // compute the total number of labels
  nbOfLabels = 0;
  for ( ThreadIdType i = 0; i < nbOfThreads; i++ )
    {
    nbOfLabels += m_NumberOfLabels[i];
    }

  if ( threadId == 0 )
    {
    // set up the union find structure
    this->InitUnion(nbOfLabels);
    // insert all the labels into the structure -- an extra loop but
    // saves complicating the ones that come later
    auto MapBegin = m_LineMap.begin();
    auto MapEnd = m_LineMap.end();
    auto LineIt = MapBegin;
    SizeValueType label = 1;
    for ( LineIt = MapBegin; LineIt != MapEnd; ++LineIt )
      {
      for ( auto cIt = LineIt->begin(); cIt != LineIt->end(); ++cIt )
        {
        cIt->label = label;
        this->InsertSet(label);
        label++;
        }
      }
    }

  // wait for the other threads to complete that part
  this->Wait();

  // now process the map and make appropriate entries in an equivalence
  // table
  // itkAssertInDebugAndIgnoreInReleaseMacro( linecount == m_LineMap.size() );
  const SizeValueType pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  const SizeValueType xsize = output->GetRequestedRegion().GetSize()[0];
  const SizeValueType linecount = pixelcount / xsize;

  SizeValueType lastLineIdForThread =  linecount;
  SizeValueType nbOfLineIdToJoin = 0;
  if ( threadId != nbOfThreads - 1 )
    {
    outputRegionForThreadSize = outputRegionForThread.GetSize();
    outputRegionForThreadSize[splitAxis] -= 1;
    lastLineIdForThread = firstLineIdForThread
                          + RegionType(outputRegionIdx, outputRegionForThreadSize).GetNumberOfPixels() / xsizeForThread;
    m_FirstLineIdToJoin[threadId] = lastLineIdForThread;
    // found the number of line ids to join
    nbOfLineIdToJoin =
      RegionType( outputRegionIdx, outputRegionForThread.GetSize() ).GetNumberOfPixels() / xsizeForThread
      - RegionType(outputRegionIdx, outputRegionForThreadSize).GetNumberOfPixels() / xsizeForThread;
    }

  for ( SizeValueType thisIdx = firstLineIdForThread; thisIdx < lastLineIdForThread; ++thisIdx )
    {
    if ( !m_LineMap[thisIdx].empty() )
      {
      for ( OffsetVectorConstIterator I = this->m_LineOffsets.begin();
            I != this->m_LineOffsets.end();
            ++I )
        {
        const OffsetValueType neighIdx = ( *I ) + thisIdx;
        // check if the neighbor is in the map
        if ( neighIdx >= 0 && neighIdx < static_cast<OffsetValueType>( linecount ) && !m_LineMap[neighIdx].empty() )
          {
          // Now check whether they are really neighbors
          const bool areNeighbors = this->CheckNeighbors(m_LineMap[thisIdx][0].where, m_LineMap[neighIdx][0].where);
          if ( areNeighbors )
            {
            // Compare the two lines
            this->CompareLines(
              m_LineMap[thisIdx],
              m_LineMap[neighIdx],
              false,
              false,
              m_BackgroundValue,
              [this](
                 const LineEncodingConstIterator& currentRun,
                 const LineEncodingConstIterator& neighborRun,
                 OffsetValueType,
                 OffsetValueType)
              {
                this->LinkLabels(neighborRun->label, currentRun->label);
              });
            }
          }
        }
      }
    }

  // wait for the other threads to complete that part
  this->Wait();

  while ( m_FirstLineIdToJoin.size() != 0 )
    {
    if ( threadId * 2 < static_cast<ThreadIdType>( m_FirstLineIdToJoin.size() ) )
      {
      for ( SizeValueType thisIdx = m_FirstLineIdToJoin[threadId * 2];
            thisIdx < m_FirstLineIdToJoin[threadId * 2] + nbOfLineIdToJoin;
            ++thisIdx )
        {
        if ( !m_LineMap[thisIdx].empty() )
          {
          for ( OffsetVectorConstIterator I = this->m_LineOffsets.begin();
                I != this->m_LineOffsets.end();
                ++I )
            {
            const OffsetValueType neighIdx = ( *I ) + thisIdx;
            // check if the neighbor is in the map
            if ( neighIdx >= 0 && neighIdx < static_cast<OffsetValueType>( linecount ) && !m_LineMap[neighIdx].empty() )
              {
              // Now check whether they are really neighbors
              const bool areNeighbors = this->CheckNeighbors(m_LineMap[thisIdx][0].where, m_LineMap[neighIdx][0].where);
              if ( areNeighbors )
                {
                // Compare the two lines
                  this->CompareLines(
                    m_LineMap[thisIdx],
                    m_LineMap[neighIdx],
                    false,
                    false,
                    m_BackgroundValue,
                    [this](
                       const LineEncodingConstIterator& currentRun,
                       const LineEncodingConstIterator& neighborRun,
                       OffsetValueType,
                       OffsetValueType)
                    {
                      this->LinkLabels(neighborRun->label, currentRun->label);
                    });
                }
              }
            }
          }
        }
      }

    this->Wait();

    if ( threadId == 0 )
      {
      // remove the region already joined
      typename std::vector< SizeValueType > newFirstLineIdToJoin;
      for ( unsigned int i = 1; i < m_FirstLineIdToJoin.size(); i += 2 )
        {
        newFirstLineIdToJoin.push_back(m_FirstLineIdToJoin[i]);
        }
      m_FirstLineIdToJoin = newFirstLineIdToJoin;
      }

    this->Wait();
    }

  if ( threadId == 0 )
    {
    m_ObjectCount = this->CreateConsecutive(m_BackgroundValue);
    }

  this->Wait();

  // check for overflow exception here
  if ( m_ObjectCount > static_cast< SizeValueType >(
         NumericTraits< OutputPixelType >::max() ) )
    {
    if ( threadId == 0 )
      {
      // main thread throw the exception
      itkExceptionMacro(
        << "Number of objects greater than maximum of output pixel type ");
      }
    else
      {
      // other threads just return
      return;
      }
    }

  // create the output
  // A more complex version that is intended to minimize the number of
  // visits to the output image which should improve cache
  // performance on large images. We also want to optimize the
  // performance of the map by being able to iterate through it,
  // rather than do lots of look ups. Don't know whether that will
  // make much difference in practice.
  // Note - this is unnecessary if AllocateOutputs initalizes to zero

  ImageRegionIterator< OutputImageType > oit(output, outputRegionForThread);
  ImageRegionIterator< OutputImageType > fstart = oit;
  fstart.GoToBegin();
  ImageRegionIterator< OutputImageType > fend = oit;
  fend.GoToEnd();

  lastLineIdForThread = firstLineIdForThread
                        + RegionType( outputRegionIdx,
                                      outputRegionForThread.GetSize() ).GetNumberOfPixels() / xsizeForThread;

  for ( SizeValueType thisIdx = firstLineIdForThread; thisIdx < lastLineIdForThread; thisIdx++ )
    {
    // now fill the labelled sections
    for ( LineEncodingConstIterator cIt = m_LineMap[thisIdx].begin(); cIt != m_LineMap[thisIdx].end(); ++cIt )
      {
      const SizeValueType Ilab = this->LookupSet(cIt->label);
      const OutputPixelType lab = this->m_Consecutive[Ilab];
      oit.SetIndex(cIt->where);
      // initialize the non labelled pixels
      for (; fstart != oit; ++fstart )
        {
        fstart.Set(m_BackgroundValue);
        }
      for ( SizeValueType i = 0; i < (SizeValueType) cIt->length; ++i, ++oit )
        {
        oit.Set(lab);
        }
      fstart = oit;
      //++fstart;
      }
    progress.CompletedPixel();
    }
  // fill the rest of the image with background value
  for (; fstart != fend; ++fstart )
    {
    fstart.Set(m_BackgroundValue);
    }
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::AfterThreadedGenerateData()
{
  m_NumberOfLabels.clear();
  m_Barrier = nullptr;
  m_LineMap.clear();
  m_Input = nullptr;
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ObjectCount: "  << m_ObjectCount << std::endl;
}
} // end namespace itk

#endif
