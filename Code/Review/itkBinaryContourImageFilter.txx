/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryContourImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryContourImageFilter_txx
#define __itkBinaryContourImageFilter_txx

#include "itkBinaryContourImageFilter.h"
#include "itkNumericTraits.h"

// don't think we need the indexed version as we only compute the
// index at the start of each run, but there isn't a choice
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkMaskImageFilter.h"
#include "itkConnectedComponentAlgorithm.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
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
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}

template< class TInputImage, class TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< class TInputImage, class TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  typename TOutputImage::Pointer output = this->GetOutput();
  typename TInputImage::ConstPointer input = this->GetInput();

  long nbOfThreads = this->GetNumberOfThreads();
  if ( itk::MultiThreader::GetGlobalMaximumNumberOfThreads() != 0 )
    {
    nbOfThreads = vnl_math_min( this->GetNumberOfThreads(), itk::MultiThreader::GetGlobalMaximumNumberOfThreads() );
    }
  // number of threads can be constrained by the region size, so call the
  // SplitRequestedRegion
  // to get the real number of threads which will be used
  typename TOutputImage::RegionType splitRegion;  // dummy region - just to call
                                                  // the following method
  nbOfThreads = this->SplitRequestedRegion(0, nbOfThreads, splitRegion);

  m_Barrier = Barrier::New();
  m_Barrier->Initialize(nbOfThreads);
  long pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  long xsize = output->GetRequestedRegion().GetSize()[0];
  long linecount = pixelcount / xsize;
  m_ForegroundLineMap.clear();
  m_ForegroundLineMap.resize(linecount);
  m_BackgroundLineMap.clear();
  m_BackgroundLineMap.resize(linecount);
  m_NumberOfThreads = nbOfThreads;
}

template< class TInputImage, class TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       int threadId)
{
  typename TOutputImage::Pointer output = this->GetOutput();
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
  long             pixelcountForThread = outputRegionForThread.GetNumberOfPixels();
  long             xsizeForThread = outputRegionForThread.GetSize()[0];
  long             linecountForThread = pixelcountForThread / xsizeForThread;
  ProgressReporter progress(this, threadId, linecountForThread * 2);

  // find the split axis
  IndexType outputRegionIdx = output->GetRequestedRegion().GetIndex();
  IndexType outputRegionForThreadIdx = outputRegionForThread.GetIndex();
  int       splitAxis = 0;
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
  long firstLineIdForThread = RegionType(outputRegionIdx, outputRegionSize).GetNumberOfPixels() / xsizeForThread;
  long lineId = firstLineIdForThread;

  OffsetVec LineOffsets;
  SetupLineOffsets(LineOffsets);

  outLineIt.GoToBegin();
  for ( inLineIt.GoToBegin();
        !inLineIt.IsAtEnd();
        inLineIt.NextLine(), outLineIt.NextLine() )
    {
    inLineIt.GoToBeginOfLine();
    outLineIt.GoToBeginOfLine();
    lineEncoding fgLine;
    lineEncoding bgLine;
    while ( !inLineIt.IsAtEndOfLine() )
      {
      InputPixelType PVal = inLineIt.Get();

      if ( PVal == m_ForegroundValue )
        {
        // We've hit the start of a run
        runLength thisRun;
        long      length = 0;
        IndexType thisIndex;
        thisIndex = inLineIt.GetIndex();

        outLineIt.Set(m_BackgroundValue);
        ++length;
        ++inLineIt;
        ++outLineIt;
        while ( !inLineIt.IsAtEndOfLine()
                && inLineIt.Get() == m_ForegroundValue )
          {
          outLineIt.Set(m_BackgroundValue);
          ++length;
          ++inLineIt;
          ++outLineIt;
          }
        // create the run length object to go in the vector
        thisRun.length = length;
        thisRun.where = thisIndex;
        fgLine.push_back(thisRun);
        }
      else
        {
        // We've hit the start of a run
        runLength thisRun;
        long      length = 0;
        IndexType thisIndex;
        thisIndex = inLineIt.GetIndex();

        outLineIt.Set(PVal);
        ++length;
        ++inLineIt;
        ++outLineIt;
        while ( !inLineIt.IsAtEndOfLine()
                && inLineIt.Get() != m_ForegroundValue )
          {
          outLineIt.Set( inLineIt.Get() );
          ++length;
          ++inLineIt;
          ++outLineIt;
          }
        // create the run length object to go in the vector
        thisRun.length = length;
        thisRun.where = thisIndex;
        bgLine.push_back(thisRun);
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
  long pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  long xsize = output->GetRequestedRegion().GetSize()[0];
  long linecount = pixelcount / xsize;

  long lastLineIdForThread =  linecount;
  if ( threadId != m_NumberOfThreads - 1 )
    {
    lastLineIdForThread = firstLineIdForThread
                          + RegionType( outputRegionIdx,
                                        outputRegionForThread.GetSize() ).GetNumberOfPixels() / xsizeForThread;
    }

  for ( long ThisIdx = firstLineIdForThread; ThisIdx < lastLineIdForThread; ++ThisIdx )
    {
    if ( !m_ForegroundLineMap[ThisIdx].empty() )
      {
      for ( OffsetVec::const_iterator I = LineOffsets.begin();
            I != LineOffsets.end(); ++I )
        {
        long NeighIdx = ThisIdx + ( *I );
        // check if the neighbor is in the map
        if ( NeighIdx >= 0 && NeighIdx < linecount && !m_BackgroundLineMap[NeighIdx].empty() )
          {
          // Now check whether they are really neighbors
          bool areNeighbors =
            CheckNeighbors(m_ForegroundLineMap[ThisIdx][0].where, m_BackgroundLineMap[NeighIdx][0].where);
          if ( areNeighbors )
            {
            // Compare the two lines
            CompareLines(m_ForegroundLineMap[ThisIdx], m_BackgroundLineMap[NeighIdx]);
            }
          }
        }
      }
    progress.CompletedPixel();
    }
}

template< class TInputImage, class TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::AfterThreadedGenerateData()
{
  m_Barrier = NULL;
  m_ForegroundLineMap.clear();
  m_BackgroundLineMap.clear();
}

template< class TInputImage, class TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::SetupLineOffsets(OffsetVec & LineOffsets)
{
  // Create a neighborhood so that we can generate a table of offsets
  // to "previous" line indexes
  // We are going to mis-use the neighborhood iterators to compute the
  // offset for us. All this messing around produces an array of
  // offsets that will be used to index the map
  typename TOutputImage::Pointer output = this->GetOutput();
  typedef Image< long, TOutputImage::ImageDimension - 1 >     PretendImageType;
  typedef typename PretendImageType::RegionType::SizeType     PretendSizeType;
  typedef typename PretendImageType::RegionType::IndexType    PretendIndexType;
  typedef ConstShapedNeighborhoodIterator< PretendImageType > LineNeighborhoodType;

  typename PretendImageType::Pointer fakeImage;
  fakeImage = PretendImageType::New();

  typename PretendImageType::RegionType LineRegion;
  //LineRegion = PretendImageType::RegionType::New();

  OutSizeType OutSize = output->GetRequestedRegion().GetSize();

  PretendSizeType PretendSize;
  // The first dimension has been collapsed
  for ( unsigned int i = 0; i < PretendSize.GetSizeDimension(); i++ )
    {
    PretendSize[i] = OutSize[i + 1];
    }

  LineRegion.SetSize(PretendSize);
  fakeImage->SetRegions(LineRegion);
  PretendSizeType kernelRadius;
  kernelRadius.Fill(1);
  LineNeighborhoodType lnit(kernelRadius, fakeImage, LineRegion);

  setConnectivity(&lnit, m_FullyConnected);

  typename LineNeighborhoodType::IndexListType ActiveIndexes;
  ActiveIndexes = lnit.GetActiveIndexList();

  typename LineNeighborhoodType::IndexListType::const_iterator LI;

  PretendIndexType idx = LineRegion.GetIndex();
  long             offset = fakeImage->ComputeOffset(idx);

  for ( LI = ActiveIndexes.begin(); LI != ActiveIndexes.end(); LI++ )
    {
    LineOffsets.push_back(fakeImage->ComputeOffset( idx + lnit.GetOffset(*LI) ) - offset);
    }
  LineOffsets.push_back(0);
  // LineOffsets is the thing we wanted.
}

template< class TInputImage, class TOutputImage >
bool
BinaryContourImageFilter< TInputImage, TOutputImage >
::CheckNeighbors(const OutputIndexType & A,
                 const OutputIndexType & B)
{
  // this checks whether the line encodings are really neighbors. The
  // first dimension gets ignored because the encodings are along that
  // axis
  OutputOffsetType Off = A - B;

  for ( unsigned i = 1; i < OutputImageDimension; i++ )
    {
    if ( abs(Off[i]) > 1 )
      {
      return ( false );
      }
    }
  return ( true );
}

template< class TInputImage, class TOutputImage >
void
BinaryContourImageFilter< TInputImage, TOutputImage >
::CompareLines(lineEncoding & current, const lineEncoding & Neighbour)
{
  bool             sameLine = true;
  OutputOffsetType Off = current[0].where - Neighbour[0].where;

  for ( unsigned i = 1; i < OutputImageDimension; i++ )
    {
    if ( Off[i] != 0 )
      {
      sameLine = false;
      }
    }

  long offset = 0;
  if ( m_FullyConnected || sameLine )
    {
    offset = 1;
    }

  typename TOutputImage::Pointer output = this->GetOutput();

  typename lineEncoding::const_iterator nIt, mIt;
  typename lineEncoding::iterator cIt;

  mIt = Neighbour.begin(); // out marker iterator

  for ( cIt = current.begin(); cIt != current.end(); ++cIt )
    {
    //runLength cL = *cIt;
    long cStart = cIt->where[0];  // the start x position
    long cLast = cStart + cIt->length - 1;
    bool lineCompleted = false;
    for ( nIt = mIt; nIt != Neighbour.end() && !lineCompleted; ++nIt )
      {
      //runLength nL = *nIt;
      long nStart = nIt->where[0];
      long nLast = nStart + nIt->length - 1;
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
      long ss1 = nStart - offset;
      // long ss2 = nStart + offset;
      // long ee1 = nLast - offset;
      long ee2 = nLast + offset;
      bool eq = false;
      long oStart = 0;
      long oLast = 0;
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
        IndexType idx = cIt->where;
        for ( int x = oStart; x <= oLast; x++ )
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

template< class TInputImage, class TOutputImage >
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
