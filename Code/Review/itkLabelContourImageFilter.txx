/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelContourImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelContourImageFilter_txx
#define __itkLabelContourImageFilter_txx

#include "itkLabelContourImageFilter.h"
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

template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  typename TOutputImage::Pointer output = this->GetOutput();
  typename TInputImage::ConstPointer input = this->GetInput();

  unsigned long nbOfThreads = this->GetNumberOfThreads();
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
//  std::cout << "nbOfThreads: " << nbOfThreads << std::endl;

  m_Barrier = Barrier::New();
  m_Barrier->Initialize(nbOfThreads);
  SizeValueType pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  SizeValueType xsize = output->GetRequestedRegion().GetSize()[0];
  SizeValueType linecount = pixelcount / xsize;
  m_LineMap.clear();
  m_LineMap.resize(linecount);
  m_NumberOfThreads = nbOfThreads;
}

template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
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
  SizeValueType    pixelcountForThread = outputRegionForThread.GetNumberOfPixels();
  SizeValueType    xsizeForThread = outputRegionForThread.GetSize()[0];
  SizeValueType    linecountForThread = pixelcountForThread / xsizeForThread;
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
  SizeValueType firstLineIdForThread =
    RegionType(outputRegionIdx, outputRegionSize).GetNumberOfPixels() / xsizeForThread;
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
    lineEncoding Line;
    while ( !inLineIt.IsAtEndOfLine() )
      {
      InputPixelType PVal = inLineIt.Get();

      runLength     thisRun;
      SizeValueType length = 0;
      IndexType     thisIndex;
      thisIndex = inLineIt.GetIndex();
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
      thisRun.length = length;
      thisRun.where = thisIndex;
      thisRun.label = PVal;
      Line.push_back(thisRun);
      }
//     std::cout << std::endl;
    m_LineMap[lineId] = Line;
    lineId++;
    progress.CompletedPixel();
    }

  // wait for the other threads to complete that part
  this->Wait();

  // now process the map and make appropriate entries in an equivalence
  // table
  // assert( linecount == m_ForegroundLineMap.size() );
  SizeValueType   pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  SizeValueType   xsize = output->GetRequestedRegion().GetSize()[0];
  OffsetValueType linecount = pixelcount / xsize;

  SizeValueType lastLineIdForThread =  linecount;
  if ( threadId != m_NumberOfThreads - 1 )
    {
    lastLineIdForThread = firstLineIdForThread
                          + RegionType( outputRegionIdx,
                                        outputRegionForThread.GetSize() ).GetNumberOfPixels() / xsizeForThread;
    }

  for ( SizeValueType ThisIdx = firstLineIdForThread; ThisIdx < lastLineIdForThread; ++ThisIdx )
    {
    if ( !m_LineMap[ThisIdx].empty() )
      {
      for ( OffsetVec::const_iterator I = LineOffsets.begin();
            I != LineOffsets.end(); ++I )
        {
        OffsetValueType NeighIdx = ThisIdx + ( *I );
        // check if the neighbor is in the map
        if ( NeighIdx >= 0 && NeighIdx < linecount && !m_LineMap[NeighIdx].empty() )
          {
          // Now check whether they are really neighbors
          bool areNeighbors =
            CheckNeighbors(m_LineMap[ThisIdx][0].where, m_LineMap[NeighIdx][0].where);
          if ( areNeighbors )
            {
            // Compare the two lines
            CompareLines(m_LineMap[ThisIdx], m_LineMap[NeighIdx]);
            }
          }
        }
      }
    progress.CompletedPixel();
    }
}

template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::AfterThreadedGenerateData()
{
  m_Barrier = NULL;
  m_LineMap.clear();
}

template< class TInputImage, class TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
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
  OffsetValueType  offset = fakeImage->ComputeOffset(idx);

  for ( LI = ActiveIndexes.begin(); LI != ActiveIndexes.end(); LI++ )
    {
    LineOffsets.push_back(fakeImage->ComputeOffset( idx + lnit.GetOffset(*LI) ) - offset);
    }
  LineOffsets.push_back(0);
  // LineOffsets is the thing we wanted.
}

template< class TInputImage, class TOutputImage >
bool
LabelContourImageFilter< TInputImage, TOutputImage >
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
LabelContourImageFilter< TInputImage, TOutputImage >
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

  OffsetValueType offset = 0;
  if ( m_FullyConnected || sameLine )
    {
    offset = 1;
    }
//   std::cout << "offset: " << offset << std::endl;

  typename TOutputImage::Pointer output = this->GetOutput();

  typename lineEncoding::const_iterator nIt, mIt;
  typename lineEncoding::iterator cIt;

  mIt = Neighbour.begin(); // out marker iterator

  for ( cIt = current.begin(); cIt != current.end(); ++cIt )
    {
    if ( cIt->label == m_BackgroundValue )
      {
      continue;
      }
    //runLength cL = *cIt;
    OffsetValueType cStart = cIt->where[0];  // the start x position
    OffsetValueType cLast = cStart + cIt->length - 1;
    bool            lineCompleted = false;
    for ( nIt = mIt; nIt != Neighbour.end() && !lineCompleted; ++nIt )
      {
      if ( nIt->label == cIt->label )
        {
        continue;
        }

      //runLength nL = *nIt;
      OffsetValueType nStart = nIt->where[0];
      OffsetValueType nLast = nStart + nIt->length - 1;
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
        assert(oStart <= oLast);
        IndexType idx = cIt->where;
        for ( int x = oStart; x <= oLast; x++ )
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
} // end namespace itk

#endif
