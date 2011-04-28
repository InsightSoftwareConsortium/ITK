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
#ifndef __itkLabelContourImageFilter_txx
#define __itkLabelContourImageFilter_txx

#include "itkLabelContourImageFilter.h"

// don't think we need the indexed version as we only compute the
// index at the start of each run, but there isn't a choice
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkMaskImageFilter.h"
#include "itkConnectedComponentAlgorithm.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
LabelContourImageFilter< TInputImage, TOutputImage >
::LabelContourImageFilter() :
  m_BackgroundValue( NumericTraits< OutputImagePixelType >::Zero ),
  m_NumberOfThreads( 0 ),
  m_FullyConnected( false )
{
  this->SetInPlace(false);
}

// -----------------------------------------------------------------------------
template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
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
}

// -----------------------------------------------------------------------------
template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

// -----------------------------------------------------------------------------
template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  int nbOfThreads = this->GetNumberOfThreads();
  int global_nb_threads = itk::MultiThreader::GetGlobalMaximumNumberOfThreads();

  if ( global_nb_threads != 0 )
    {
    nbOfThreads = vnl_math_min( nbOfThreads, global_nb_threads );
    }

  // number of threads can be constrained by the region size, so call the
  // SplitRequestedRegion
  // to get the real number of threads which will be used
  OutputRegionType splitRegion;

  // dummy region - just to call
  // the following method
  nbOfThreads = this->SplitRequestedRegion(0, nbOfThreads, splitRegion);

//  std::cout << "nbOfThreads: " << nbOfThreads << std::endl;
  m_Barrier = Barrier::New();
  m_Barrier->Initialize(nbOfThreads);

  OutputImagePointer output = this->GetOutput();

  SizeValueType pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  SizeValueType xsize = output->GetRequestedRegion().GetSize()[0];
  SizeValueType linecount = pixelcount / xsize;

  m_LineMap.clear();
  m_LineMap.resize(linecount);
  m_NumberOfThreads = nbOfThreads;
}

// -----------------------------------------------------------------------------
template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputRegionType & outputRegionForThread,
                       int threadId)
{
  OutputImagePointer output = this->GetOutput();
  typename TInputImage::ConstPointer input = this->GetInput();

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
  OutputIndexType outputRegionIdx = output->GetRequestedRegion().GetIndex();
  OutputIndexType outputRegionForThreadIdx = outputRegionForThread.GetIndex();
  int       splitAxis = 0;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( outputRegionIdx[i] != outputRegionForThreadIdx[i] )
      {
      splitAxis = i;
      }
    }

  // compute the number of pixels before that thread
  OutputSizeType outputRegionSize = output->GetRequestedRegion().GetSize();
  outputRegionSize[splitAxis] = outputRegionForThreadIdx[splitAxis] - outputRegionIdx[splitAxis];
  SizeValueType firstLineIdForThread =
    OutputRegionType(outputRegionIdx, outputRegionSize).GetNumberOfPixels() / xsizeForThread;
  SizeValueType lineId = firstLineIdForThread;

  OffsetVectorType LineOffsets;
  SetupLineOffsets(LineOffsets);

  outLineIt.GoToBegin();
  for ( inLineIt.GoToBegin();
        !inLineIt.IsAtEnd();
        inLineIt.NextLine(), outLineIt.NextLine() )
    {
    inLineIt.GoToBeginOfLine();
    outLineIt.GoToBeginOfLine();
    LineEncodingType Line;
    while ( !inLineIt.IsAtEndOfLine() )
      {
      InputPixelType PVal = inLineIt.Get();

      SizeValueType  length = 0;
      InputIndexType thisIndex = inLineIt.GetIndex();
      //std::cout << thisIndex << std::endl;
      outLineIt.Set(m_BackgroundValue);
      ++length;
      ++inLineIt;
      ++outLineIt;
      while ( !inLineIt.IsAtEndOfLine()
              && inLineIt.Get() == PVal )
        {
        outLineIt.Set(m_BackgroundValue);
        ++length;
        ++inLineIt;
        ++outLineIt;
        }
      // create the run length object to go in the vector
      RunLength     thisRun = { length, thisIndex, PVal };

      Line.push_back(thisRun);
      }
//     std::cout << std::endl;
    m_LineMap[lineId] = Line;
    ++lineId;
    progress.CompletedPixel();
    }

  // wait for the other threads to complete that part
  this->Wait();

  // now process the map and make appropriate entries in an equivalence
  // table
  // itkAssertInDebugAndIgnoreInReleaseMacro( linecount == m_ForegroundLineMap.size() );
  SizeValueType   pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  SizeValueType   xsize = output->GetRequestedRegion().GetSize()[0];
  OffsetValueType linecount = pixelcount / xsize;

  SizeValueType lastLineIdForThread =  linecount;
  if ( threadId != m_NumberOfThreads - 1 )
    {
    lastLineIdForThread = firstLineIdForThread
                          + OutputRegionType( outputRegionIdx,
                                              outputRegionForThread.GetSize() ).GetNumberOfPixels() / xsizeForThread;
    }

  for ( SizeValueType ThisIdx = firstLineIdForThread; ThisIdx < lastLineIdForThread; ++ThisIdx )
    {
    if ( !m_LineMap[ThisIdx].empty() )
      {
      for ( OffsetVectorConstIterator I = LineOffsets.begin();
            I != LineOffsets.end();
            ++I )
        {
        OffsetValueType NeighIdx = ThisIdx + ( *I );

        // check if the neighbor is in the map
        if ( NeighIdx >= 0 && NeighIdx < linecount )
          {
          if( !m_LineMap[NeighIdx].empty() )
            {
            // Now check whether they are really neighbors
            bool areNeighbors =
              CheckNeighbors( m_LineMap[ThisIdx][0].where,
                              m_LineMap[NeighIdx][0].where );
            if ( areNeighbors )
              {
              // Compare the two lines
              CompareLines( m_LineMap[ThisIdx],
                            m_LineMap[NeighIdx] );
              }
            }
          }
        }
      }
    progress.CompletedPixel();
    }
}

// -----------------------------------------------------------------------------
template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::AfterThreadedGenerateData()
{
  m_LineMap.clear();
}

// -----------------------------------------------------------------------------
template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::SetupLineOffsets(OffsetVectorType & LineOffsets)
{
  // Create a neighborhood so that we can generate a table of offsets
  // to "previous" line indexes
  // We are going to mis-use the neighborhood iterators to compute the
  // offset for us. All this messing around produces an array of
  // offsets that will be used to index the map
  OutputImagePointer output = this->GetOutput();

  const unsigned int PretendDimension = ImageDimension - 1;

  typedef Image< OffsetValueType, PretendDimension >      PretendImageType;
  typedef typename PretendImageType::Pointer              PretendImagePointer;
  typedef typename PretendImageType::RegionType           PretendRegionType;
  typedef typename PretendRegionType::SizeType            PretendSizeType;
  typedef typename PretendRegionType::IndexType           PretendIndexType;

  PretendImagePointer fakeImage = PretendImageType::New();

  OutputSizeType OutSize = output->GetRequestedRegion().GetSize();

  PretendSizeType PretendSize;
  // The first dimension has been collapsed
  for ( unsigned int i = 0; i < PretendDimension; i++ )
    {
    PretendSize[i] = OutSize[i + 1];
    }

  PretendRegionType LineRegion;
  LineRegion.SetSize(PretendSize);

  fakeImage->SetRegions(LineRegion);

  PretendSizeType kernelRadius;
  kernelRadius.Fill(1);

  typedef ConstShapedNeighborhoodIterator< PretendImageType > LineNeighborhoodType;

  LineNeighborhoodType lnit(kernelRadius, fakeImage, LineRegion);

  setConnectivity(&lnit, m_FullyConnected);

  typedef typename LineNeighborhoodType::IndexListType  LineNeighborhoodIndexListType;
  LineNeighborhoodIndexListType ActiveIndexes = lnit.GetActiveIndexList();

  PretendIndexType idx = LineRegion.GetIndex();
  OffsetValueType  offset = fakeImage->ComputeOffset(idx);

  const typename LineNeighborhoodIndexListType::const_iterator LEnd = ActiveIndexes.end();
  for (typename LineNeighborhoodIndexListType::const_iterator LI = ActiveIndexes.begin();
    LI != LEnd; ++LI )
    {
    LineOffsets.push_back(fakeImage->ComputeOffset( idx + lnit.GetOffset(*LI) ) - offset);
    }

  LineOffsets.push_back(0);
  // LineOffsets is the thing we wanted.
}

// -----------------------------------------------------------------------------
template< class TInputImage, class TOutputImage >
bool
LabelContourImageFilter< TInputImage, TOutputImage >
::CheckNeighbors(const OutputIndexType & A,
                 const OutputIndexType & B) const
{
  // this checks whether the line encodings are really neighbors. The
  // first dimension gets ignored because the encodings are along that
  // axis
  OutputOffsetType Off = A - B;

  for ( unsigned int i = 1; i < ImageDimension; i++ )
    {
    if ( vnl_math_abs(Off[i]) > 1 )
      {
      return ( false );
      }
    }
  return ( true );
}

// -----------------------------------------------------------------------------
template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::CompareLines(LineEncodingType & current, const LineEncodingType & Neighbour)
{
  bool             sameLine = true;
  OutputOffsetType Off = current[0].where - Neighbour[0].where;

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
//   std::cout << "offset: " << offset << std::endl;

  OutputImagePointer output = this->GetOutput();

  LineEncodingIterator cIt = current.begin();
  LineEncodingIterator cEnd = current.end();

  // out marker iterator

  while ( cIt != cEnd )
    {
    if ( cIt->label != m_BackgroundValue )
      {
      //runLength cL = *cIt;
      OffsetValueType cStart = cIt->where[0];  // the start x position
      OffsetValueType cLast = cStart + cIt->length - 1;

      bool lineCompleted = false;
      const LineEncodingConstIterator mEnd = Neighbour.end();
      for(LineEncodingConstIterator mIt = Neighbour.begin();
        mIt != mEnd && !lineCompleted; ++mIt )
        {
        if ( mIt->label != cIt->label )
          {
          //runLength nL = *nIt;
          OffsetValueType nStart = mIt->where[0];
          OffsetValueType nLast = nStart + mIt->length - 1;

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
          // OffsetValueType ss2 = nStart + offset;
          // OffsetValueType ee1 = nLast - offset;
          OffsetValueType ee2 = nLast + offset;

          bool            eq = false;
          OffsetValueType oStart = 0;
          OffsetValueType oLast = 0;

          // the logic here can probably be improved a lot
          if ( ( ss1 >= cStart ) && ( ee2 <= cLast ) )
            {
            // case 1
    //         std::cout << "case 1" << std::endl;
            eq = true;
            oStart = ss1;
            oLast = ee2;
            }
          else if ( ( ss1 <= cStart ) && ( ee2 >= cLast ) )
            {
            // case 4
    //         std::cout << "case 4" << std::endl;
            eq = true;
            oStart = cStart;
            oLast = cLast;
            }
          else if ( ( ss1 <= cLast ) && ( ee2 >= cLast ) )
            {
            // case 2
    //         std::cout << "case 2" << std::endl;
            eq = true;
            oStart = ss1;
            oLast = cLast;
            }
          else if ( ( ss1 <= cStart ) && ( ee2 >= cStart ) )
            {
            // case 3
    //         std::cout << "case 3" << std::endl;
            eq = true;
            oStart = cStart;
            oLast = ee2;
            }

          if ( eq )
            {
    //         std::cout << oStart << " " << oLast << std::endl;
            itkAssertInDebugAndIgnoreInReleaseMacro(oStart <= oLast);
            OutputIndexType idx = cIt->where;
            for ( OffsetValueType x = oStart; x <= oLast; ++x )
              {
    //           std::cout << idx << std::endl;
              idx[0] = x;
              output->SetPixel(idx, cIt->label);
              }

            if ( oStart == cStart && oLast == cLast )
              {
              lineCompleted = true;
              }
            }
          }
        }
      }
    ++cIt;
    }
}

// -----------------------------------------------------------------------------
template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_BackgroundValue ) << std::endl;
}

// -----------------------------------------------------------------------------
template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::Wait()
{
  if ( m_NumberOfThreads > 1 )
    {
    m_Barrier->Wait();
    }
}
} // end namespace itk

#endif
