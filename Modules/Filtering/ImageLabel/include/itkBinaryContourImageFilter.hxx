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
#ifndef itkBinaryContourImageFilter_hxx
#define itkBinaryContourImageFilter_hxx

#include "itkBinaryContourImageFilter.h"

// don't think we need the indexed version as we only compute the
// index at the start of each run, but there isn't a choice
#include "itkImageLinearIteratorWithIndex.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkMaskImageFilter.h"
#include "itkConnectedComponentAlgorithm.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
BinaryContourImageFilter< TInputImage, TOutputImage >
::BinaryContourImageFilter()
{
  m_FullyConnected = false;
  m_ForegroundValue = NumericTraits< InputImagePixelType >::max();
  m_BackgroundValue = NumericTraits< OutputImagePixelType >::ZeroValue();
  m_NumberOfThreads = 0;

  this->SetInPlace(false);
}

template< typename TInputImage, typename TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::Wait()
{
  if ( m_NumberOfThreads > 1 )
    {
    m_Barrier->Wait();
    }
}

template< typename TInputImage, typename TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
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
  input->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  OutputImagePointer output = this->GetOutput();
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  OutputImagePointer output = this->GetOutput();
  InputImageConstPointer input = this->GetInput();

  ThreadIdType nbOfThreads = this->GetNumberOfThreads();
  ThreadIdType maxNumberOfThreads = itk::MultiThreader::GetGlobalMaximumNumberOfThreads();
  if ( maxNumberOfThreads != 0 )
    {
    nbOfThreads = std::min( this->GetNumberOfThreads(), maxNumberOfThreads );
    }
  // number of threads can be constrained by the region size, so call the
  // SplitRequestedRegion
  // to get the real number of threads which will be used
  RegionType splitRegion;  // dummy region - just to call
                                                  // the following method
  nbOfThreads = this->SplitRequestedRegion(0, nbOfThreads, splitRegion);

  m_Barrier = Barrier::New();
  m_Barrier->Initialize(nbOfThreads);

  RegionType tempRegion = output->GetRequestedRegion();
  SizeValueType pixelcount = tempRegion.GetNumberOfPixels();

  SizeValueType xsize = tempRegion.GetSize()[0];
  SizeValueType linecount = pixelcount / xsize;

  m_ForegroundLineMap.clear();
  m_ForegroundLineMap.resize(linecount);

  m_BackgroundLineMap.clear();
  m_BackgroundLineMap.resize(linecount);

  m_NumberOfThreads = nbOfThreads;
}

template< typename TInputImage, typename TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  OutputImagePointer      output  = this->GetOutput();
  InputImageConstPointer  input   = this->GetInput();

  // create a line iterator
  typedef itk::ImageLinearConstIteratorWithIndex< InputImageType >
    InputLineIteratorType;

  InputLineIteratorType inLineIt(input, outputRegionForThread);
  inLineIt.SetDirection(0);

  typedef itk::ImageLinearIteratorWithIndex< OutputImageType >
    OutputLineIteratorType;

  OutputLineIteratorType outLineIt(output, outputRegionForThread);
  outLineIt.SetDirection(0);

  // set the progress reporter to deal with the number of lines
  SizeValueType    pixelcountForThread = outputRegionForThread.GetNumberOfPixels();
  SizeValueType    xsizeForThread = outputRegionForThread.GetSize()[0];
  SizeValueType    linecountForThread = pixelcountForThread / xsizeForThread;
  ProgressReporter progress(this, threadId, linecountForThread * 2);

  // find the split axis
  IndexType     outputRegionIdx = output->GetRequestedRegion().GetIndex();
  IndexType     outputRegionForThreadIdx = outputRegionForThread.GetIndex();
  unsigned int  splitAxis = 0;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( outputRegionIdx[i] != outputRegionForThreadIdx[i] )
      {
      splitAxis = i;
      }
    }

  // compute the number of pixels before that thread
  SizeType outputRegionSize = output->GetRequestedRegion().GetSize();
  outputRegionSize[splitAxis] = outputRegionForThreadIdx[splitAxis] - outputRegionIdx[splitAxis];

  SizeValueType firstLineIdForThread =
    RegionType( outputRegionIdx, outputRegionSize ).GetNumberOfPixels() / xsizeForThread;

  SizeValueType lineId = firstLineIdForThread;

  OffsetVec LineOffsets;
  SetupLineOffsets(LineOffsets);

  outLineIt.GoToBegin();
  for ( inLineIt.GoToBegin();
        !inLineIt.IsAtEnd();
        inLineIt.NextLine(), outLineIt.NextLine() )
    {
    inLineIt.GoToBeginOfLine();
    outLineIt.GoToBeginOfLine();

    LineEncodingType fgLine;
    LineEncodingType bgLine;

    while ( !inLineIt.IsAtEndOfLine() )
      {
      InputImagePixelType PVal = inLineIt.Get();

      if ( Math::AlmostEquals(PVal, m_ForegroundValue) )
        {
        // We've hit the start of a run
        SizeValueType length = 0;
        IndexType thisIndex = inLineIt.GetIndex();

        outLineIt.Set(m_BackgroundValue);

        ++length;
        ++inLineIt;
        ++outLineIt;

        while ( !inLineIt.IsAtEndOfLine()
                && Math::AlmostEquals( inLineIt.Get(), m_ForegroundValue ) )
          {
          outLineIt.Set(m_BackgroundValue);
          ++length;
          ++inLineIt;
          ++outLineIt;
          }
        // create the run length object to go in the vector
        fgLine.push_back( runLength( length, thisIndex ) );
        }
      else
        {
        // We've hit the start of a run
        SizeValueType length = 0;
        IndexType thisIndex = inLineIt.GetIndex();

        outLineIt.Set(PVal);
        ++length;
        ++inLineIt;
        ++outLineIt;
        while ( !inLineIt.IsAtEndOfLine()
                && Math::NotAlmostEquals( inLineIt.Get(), m_ForegroundValue ) )
          {
          outLineIt.Set( inLineIt.Get() );
          ++length;
          ++inLineIt;
          ++outLineIt;
          }
        // create the run length object to go in the vector
        bgLine.push_back( runLength( length, thisIndex ) );
        }
      }

    m_ForegroundLineMap[lineId] = fgLine;
    m_BackgroundLineMap[lineId] = bgLine;
    lineId++;
    progress.CompletedPixel();
    }

  // wait for the other threads to complete that part
  this->Wait();

  // now process the map and make appropriate entries in an equivalence table
  SizeValueType pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  SizeValueType xsize = output->GetRequestedRegion().GetSize()[0];
  OffsetValueType linecount = pixelcount / xsize;

  SizeValueType lastLineIdForThread =  linecount;
  if ( threadId != m_NumberOfThreads - 1 )
    {
    lastLineIdForThread = firstLineIdForThread
                          + RegionType( outputRegionIdx,
                                        outputRegionForThread.GetSize() ).GetNumberOfPixels() / xsizeForThread;
    }

  for ( SizeValueType thisIdx = firstLineIdForThread;
        thisIdx < lastLineIdForThread;
        ++thisIdx )
    {
    if ( !m_ForegroundLineMap[thisIdx].empty() )
      {
      for ( typename OffsetVec::const_iterator I = LineOffsets.begin();
            I != LineOffsets.end();
            ++I )
        {
        OffsetValueType NeighIdx = thisIdx + ( *I );

        // check if the neighbor is in the map
        if ( NeighIdx >= 0 && NeighIdx < linecount && !m_BackgroundLineMap[NeighIdx].empty() )
          {
          // Now check whether they are really neighbors
          bool areNeighbors =
            CheckNeighbors(m_ForegroundLineMap[thisIdx][0].m_Where, m_BackgroundLineMap[NeighIdx][0].m_Where);
          if ( areNeighbors )
            {
            // Compare the two lines
            CompareLines(m_ForegroundLineMap[thisIdx], m_BackgroundLineMap[NeighIdx]);
            }
          }
        }
      }
    progress.CompletedPixel();
    }
}

template< typename TInputImage, typename TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::AfterThreadedGenerateData()
{
  m_Barrier = ITK_NULLPTR;
  m_ForegroundLineMap.clear();
  m_BackgroundLineMap.clear();
}

template< typename TInputImage, typename TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::SetupLineOffsets(OffsetVec & LineOffsets)
{
  // Create a neighborhood so that we can generate a table of offsets
  // to "previous" line indexes
  // We are going to mis-use the neighborhood iterators to compute the
  // offset for us. All this messing around produces an array of
  // offsets that will be used to index the map
  OutputImagePointer output = this->GetOutput();

  const unsigned int PretendDimension = ImageDimension - 1;

  typedef Image< OffsetValueType, PretendDimension >          PretendImageType;
  typedef typename PretendImageType::Pointer                  PretendImagePointer;
  typedef typename PretendImageType::RegionType               PretendImageRegionType;
  typedef typename PretendImageType::RegionType::SizeType     PretendSizeType;
  typedef typename PretendImageType::RegionType::IndexType    PretendIndexType;

  PretendImagePointer fakeImage = PretendImageType::New();

  PretendImageRegionType LineRegion;
  //LineRegion = PretendImageType::RegionType::New();

  OutputSizeType OutSize = output->GetRequestedRegion().GetSize();

  PretendSizeType PretendSize;

  // The first dimension has been collapsed
  for ( unsigned int i = 0; i < PretendDimension; i++ )
    {
    PretendSize[i] = OutSize[i + 1];
    }

  LineRegion.SetSize(PretendSize);
  fakeImage->SetRegions(LineRegion);

  PretendSizeType kernelRadius;
  kernelRadius.Fill(1);

  typedef ConstShapedNeighborhoodIterator< PretendImageType > LineNeighborhoodType;
  LineNeighborhoodType lnit(kernelRadius, fakeImage, LineRegion);

  setConnectivity(&lnit, m_FullyConnected);

  typename LineNeighborhoodType::IndexListType ActiveIndexes;
  ActiveIndexes = lnit.GetActiveIndexList();

  typename LineNeighborhoodType::IndexListType::const_iterator LI;

  PretendIndexType idx = LineRegion.GetIndex();
  OffsetValueType  offset = fakeImage->ComputeOffset(idx);

  for ( LI = ActiveIndexes.begin(); LI != ActiveIndexes.end(); ++LI )
    {
    LineOffsets.push_back(
      fakeImage->ComputeOffset( idx + lnit.GetOffset(*LI) ) - offset );
    }

  LineOffsets.push_back(0);
  // LineOffsets is the thing we wanted.
}

template< typename TInputImage, typename TOutputImage >
bool
BinaryContourImageFilter< TInputImage, TOutputImage >
::CheckNeighbors(const OutputIndexType & A,
                 const OutputIndexType & B)
{
  // this checks whether the line encodings are really neighbors. The
  // first dimension gets ignored because the encodings are along that
  // axis
  for ( unsigned int i = 1; i < ImageDimension; i++ )
    {
    if ( itk::Math::abs( A[i] - B[i] ) > 1 )
      {
      return ( false );
      }
    }
  return ( true );
}

template< typename TInputImage, typename TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::CompareLines(LineEncodingType & current, const LineEncodingType & Neighbour)
{
  bool             sameLine = true;
  OutputOffsetType Off = current[0].m_Where - Neighbour[0].m_Where;

  for ( unsigned int i = 1; i < ImageDimension; i++ )
    {
    if ( Off[i] != 0 )
      {
      sameLine = false;
      break;
      }
    }

  OffsetValueType offset = 0;
  if ( m_FullyConnected || sameLine )
    {
    offset = 1;
    }

  OutputImagePointer output = this->GetOutput();

  // out marker iterator
  LineEncodingConstIterator mIt = Neighbour.begin();

  for ( LineEncodingIterator cIt = current.begin();
        cIt != current.end();
        ++cIt )
    {
    // the start x position
    OffsetValueType cStart = cIt->m_Where[0];
    OffsetValueType cLast = cStart + cIt->m_Length - 1;

    bool lineCompleted = false;

    for ( LineEncodingConstIterator nIt = mIt;
          nIt != Neighbour.end() && !lineCompleted;
          ++nIt )
      {
      OffsetValueType nStart = nIt->m_Where[0];
      OffsetValueType nLast = nStart + nIt->m_Length - 1;

      // there are a few ways that neighbouring lines might overlap
      //   neighbor      S------------------E
      //   current    S------------------------E
      //-------------
      //   neighbor      S------------------E
      //   current    S----------------E
      //-------------
      //   neighbor      S------------------E
      //   current             S------------------E
      //-------------
      //   neighbor      S------------------E
      //   current             S-------E
      //-------------
      OffsetValueType ss1 = nStart - offset;
      OffsetValueType ee2 = nLast + offset;

      bool eq = false;

      OffsetValueType oStart = 0;
      OffsetValueType oLast = 0;

      // the logic here can probably be improved a lot
      if ( ( ss1 >= cStart ) && ( ee2 <= cLast ) )
        {
        // case 1
        eq = true;
        oStart = ss1;
        oLast = ee2;
        }
      else if ( ( ss1 <= cStart ) && ( ee2 >= cLast ) )
        {
        // case 4
        eq = true;
        oStart = cStart;
        oLast = cLast;
        }
      else if ( ( ss1 <= cLast ) && ( ee2 >= cLast ) )
        {
        // case 2
        eq = true;
        oStart = ss1;
        oLast = cLast;
        }
      else if ( ( ss1 <= cStart ) && ( ee2 >= cStart ) )
        {
        // case 3
        eq = true;
        oStart = cStart;
        oLast = ee2;
        }

      if ( eq )
        {
        itkAssertOrThrowMacro( ( oStart <= oLast ), "Start and Last out of order" );
        IndexType idx = cIt->m_Where;
        for ( OffsetValueType x = oStart; x <= oLast; x++ )
          {
          idx[0] = x;
          output->SetPixel(idx, m_ForegroundValue);
          }
        if ( oStart == cStart && oLast == cLast )
          {
          lineCompleted = true;
          }
        }
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "ForegroundValue: "
     << static_cast< typename NumericTraits< InputImagePixelType >::PrintType >( m_ForegroundValue ) << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_BackgroundValue ) << std::endl;
}
} // end namespace itk

#endif
