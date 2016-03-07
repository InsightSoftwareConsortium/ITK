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

// don't think we need the indexed version as we only compute the
// index at the start of each run, but there isn't a choice
#include "itkImageLinearConstIteratorWithIndex.h"
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

  typedef MaskImageFilter< TInputImage, TMaskImage, TInputImage >
  MaskFilterType;
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

  ThreadIdType nbOfThreads = this->GetNumberOfThreads();
  if ( itk::MultiThreader::GetGlobalMaximumNumberOfThreads() != 0 )
    {
    nbOfThreads = std::min( this->GetNumberOfThreads(), itk::MultiThreader::GetGlobalMaximumNumberOfThreads() );
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

  const ThreadIdType nbOfThreads = static_cast<const ThreadIdType>( m_NumberOfLabels.size() );

  // create a line iterator
  typedef itk::ImageLinearConstIteratorWithIndex< InputImageType > InputLineIteratorType;
  InputLineIteratorType inLineIt(m_Input, outputRegionForThread);
  inLineIt.SetDirection(0);

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
  typedef SizeValueType LineIdType;
  LineIdType firstLineIdForThread = RegionType(outputRegionIdx, outputRegionSize).GetNumberOfPixels() / xsizeForThread;
  LineIdType lineId = firstLineIdForThread;

  OffsetVec LineOffsets;
  SetupLineOffsets(LineOffsets);

  LineIdType nbOfLabels = 0;
  for ( inLineIt.GoToBegin();
        !inLineIt.IsAtEnd();
        inLineIt.NextLine() )
    {
    inLineIt.GoToBeginOfLine();
    lineEncoding ThisLine;
    while ( !inLineIt.IsAtEndOfLine() )
      {
      const InputPixelType PVal = inLineIt.Get();
      //std::cout << inLineIt.GetIndex() << std::endl;
      if ( PVal != NumericTraits< InputPixelType >::ZeroValue( PVal ) )
        {
        // We've hit the start of a run
        runLength thisRun;
        const IndexType thisIndex = inLineIt.GetIndex();
        //std::cout << thisIndex << std::endl;
        SizeValueType length = 1;
        ++inLineIt;
        while ( !inLineIt.IsAtEndOfLine()
                && inLineIt.Get() != NumericTraits< InputPixelType >::ZeroValue( PVal ) )
          {
          ++length;
          ++inLineIt;
          }
        // create the run length object to go in the vector
        thisRun.length = length;
        thisRun.label = 0; // will give a real label later
        thisRun.where = thisIndex;
        ThisLine.push_back(thisRun);
        nbOfLabels++;
        }
      else
        {
        ++inLineIt;
        }
      }
    m_LineMap[lineId] = ThisLine;
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
    InitUnion(nbOfLabels);
    // insert all the labels into the structure -- an extra loop but
    // saves complicating the ones that come later
    typename LineMapType::iterator MapBegin = m_LineMap.begin();
    typename LineMapType::iterator MapEnd = m_LineMap.end();
    typename LineMapType::iterator LineIt = MapBegin;
    SizeValueType label = 1;
    for ( LineIt = MapBegin; LineIt != MapEnd; ++LineIt )
      {
      for ( typename lineEncoding::iterator cIt = LineIt->begin(); cIt != LineIt->end(); ++cIt )
        {
        cIt->label = label;
        InsertSet(label);
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

  for ( SizeValueType ThisIdx = firstLineIdForThread; ThisIdx < lastLineIdForThread; ++ThisIdx )
    {
    if ( !m_LineMap[ThisIdx].empty() )
      {
      for ( typename OffsetVec::const_iterator I = LineOffsets.begin();
            I != LineOffsets.end(); ++I )
        {
        const OffsetValueType NeighIdx = ( *I ) + ThisIdx;
        // check if the neighbor is in the map
        if ( NeighIdx >= 0 && NeighIdx < static_cast<OffsetValueType>( linecount ) && !m_LineMap[NeighIdx].empty() )
          {
          // Now check whether they are really neighbors
          const bool areNeighbors =
            CheckNeighbors(m_LineMap[ThisIdx][0].where, m_LineMap[NeighIdx][0].where);
          if ( areNeighbors )
            {
            // Compare the two lines
            CompareLines(m_LineMap[ThisIdx], m_LineMap[NeighIdx]);
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
      for ( SizeValueType ThisIdx = m_FirstLineIdToJoin[threadId * 2];
            ThisIdx < m_FirstLineIdToJoin[threadId * 2] + nbOfLineIdToJoin;
            ++ThisIdx )
        {
        if ( !m_LineMap[ThisIdx].empty() )
          {
          for ( typename OffsetVec::const_iterator I = LineOffsets.begin();
                I != LineOffsets.end(); ++I )
            {
            const OffsetValueType NeighIdx = ( *I ) + ThisIdx;
            // check if the neighbor is in the map
            if ( NeighIdx >= 0 && NeighIdx < static_cast<OffsetValueType>( linecount ) && !m_LineMap[NeighIdx].empty() )
              {
              // Now check whether they are really neighbors
              const bool areNeighbors =
                CheckNeighbors(m_LineMap[ThisIdx][0].where, m_LineMap[NeighIdx][0].where);
              if ( areNeighbors )
                {
                // Compare the two lines
                CompareLines(m_LineMap[ThisIdx], m_LineMap[NeighIdx]);
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
    m_ObjectCount = CreateConsecutive();
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

  for ( SizeValueType ThisIdx = firstLineIdForThread; ThisIdx < lastLineIdForThread; ThisIdx++ )
    {
    // now fill the labelled sections
    for ( typename lineEncoding::const_iterator cIt = m_LineMap[ThisIdx].begin(); cIt != m_LineMap[ThisIdx].end(); ++cIt )
      {
      const SizeValueType   Ilab = LookupSet(cIt->label);
      const OutputPixelType lab = m_Consecutive[Ilab];
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
  m_Barrier = ITK_NULLPTR;
  m_LineMap.clear();
  m_Input = ITK_NULLPTR;
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::SetupLineOffsets(OffsetVec & LineOffsets)
{
  // Create a neighborhood so that we can generate a table of offsets
  // to "previous" line indexes
  // We are going to mis-use the neighborhood iterators to compute the
  // offset for us. All this messing around produces an array of
  // offsets that will be used to index the map
  typename TOutputImage::Pointer output = this->GetOutput();
  typedef Image< OffsetValueType, TOutputImage::ImageDimension - 1 >  PretendImageType;
  typedef typename PretendImageType::RegionType::SizeType             PretendSizeType;
  typedef typename PretendImageType::RegionType::IndexType            PretendIndexType;
  typedef ConstShapedNeighborhoodIterator< PretendImageType >         LineNeighborhoodType;

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

  // only activate the indices that are "previous" to the current
  // pixel and face connected (exclude the center pixel from the
  // neighborhood)
  //
  setConnectivityPrevious(&lnit, m_FullyConnected);

  typename LineNeighborhoodType::IndexListType ActiveIndexes;
  ActiveIndexes = lnit.GetActiveIndexList();

  typename LineNeighborhoodType::IndexListType::const_iterator LI;

  PretendIndexType idx = LineRegion.GetIndex();
  OffsetValueType offset = fakeImage->ComputeOffset(idx);

  for ( LI = ActiveIndexes.begin(); LI != ActiveIndexes.end(); LI++ )
    {
    LineOffsets.push_back(fakeImage->ComputeOffset( idx + lnit.GetOffset(*LI) ) - offset);
    }

  // LineOffsets is the thing we wanted.
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
bool
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::CheckNeighbors(const OutputIndexType & A,
                 const OutputIndexType & B)
{
  // this checks whether the line encodings are really neighbors. The
  // first dimension gets ignored because the encodings are along that
  // axis
  OutputOffsetType Off = A - B;

  for ( unsigned i = 1; i < OutputImageDimension; i++ )
    {
    if ( itk::Math::abs(Off[i]) > 1 )
      {
      return ( false );
      }
    }
  return ( true );
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::CompareLines(lineEncoding & current, const lineEncoding & Neighbour)
{
  long offset = 0;

  if ( m_FullyConnected )
    {
    offset = 1;
    }

  typename lineEncoding::const_iterator nIt, mIt;
  typename lineEncoding::iterator cIt;

  mIt = Neighbour.begin(); // out marker iterator

  for ( cIt = current.begin(); cIt != current.end(); ++cIt )
    {
    //runLength cL = *cIt;
    IndexValueType cStart = cIt->where[0];  // the start x position
    IndexValueType cLast = cStart + cIt->length - 1;

    for ( nIt = mIt; nIt != Neighbour.end(); ++nIt )
      {
      //runLength nL = *nIt;
      IndexValueType nStart = nIt->where[0];
      IndexValueType nLast = nStart + nIt->length - 1;
      // there are a few ways that neighbouring lines might overlap
      //   neighbor      S                  E
      //   current    S                        E
      //------------------------------------------
      //   neighbor      S                  E
      //   current    S                E
      //------------------------------------------
      //   neighbor      S                  E
      //   current             S                  E
      //------------------------------------------
      //   neighbor      S                  E
      //   current             S       E
      //------------------------------------------
      IndexValueType ss1 = nStart - offset;
      // IndexValueType ss2 = nStart + offset;
      IndexValueType ee1 = nLast - offset;
      IndexValueType ee2 = nLast + offset;
      bool eq = false;
      // the logic here can probably be improved a lot
      if ( ( ss1 >= cStart ) && ( ee2 <= cLast ) )
        {
        // case 1
        eq = true;
        }
      else
        {
        if ( ( ss1 <= cLast ) && ( ee2 >= cLast ) )
          {
          // case 2
          eq = true;
          }
        else
          {
          if ( ( ss1 <= cStart ) && ( ee2 >= cStart ) )
            {
            // case 3
            eq = true;
            }
          else
            {
            if ( ( ss1 <= cStart ) && ( ee2 >= cLast ) )
              {
              // case 4
              eq = true;
              }
            }
          }
        }
      if ( eq )
        {
        LinkLabels(nIt->label, cIt->label);
        }

      if ( ee1 >= cLast )
        {
        // No point looking for more overlaps with the current run
        // because the neighbor run is either case 2 or 4
        mIt = nIt;
        break;
        }
      }
    }
}

// union find related functions
template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::InsertSet(const LabelType label)
{
  m_UnionFind[label] = label;
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
SizeValueType
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::CreateConsecutive()
{
  m_Consecutive = UnionFindType( m_UnionFind.size() );

  SizeValueType CLab = 0;
  SizeValueType count = 0;
  for ( SizeValueType I = 1; I < m_UnionFind.size(); I++ )
    {
    SizeValueType L = m_UnionFind[I];
    if ( L == I )
      {
      if ( CLab == static_cast< SizeValueType >( m_BackgroundValue ) )
        {
        ++CLab;
        }
      m_Consecutive[L] = CLab;
      ++CLab;
      ++count;
      }
    }
  return count;
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
SizeValueType
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::LookupSet(const LabelType label)
{
  // recursively set the equivalence if necessary
  if ( label != m_UnionFind[label] )
    {
    m_UnionFind[label] = this->LookupSet(m_UnionFind[label]);
    }
  return ( m_UnionFind[label] );
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::LinkLabels(const LabelType lab1, const LabelType lab2)
{
  SizeValueType E1 = this->LookupSet(lab1);
  SizeValueType E2 = this->LookupSet(lab2);

  if ( E1 < E2 )
    {
    m_UnionFind[E2] = E1;
    }
  else
    {
    m_UnionFind[E1] = E2;
    }
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
ConnectedComponentImageFilter< TInputImage, TOutputImage, TMaskImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "ObjectCount: "  << m_ObjectCount << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_BackgroundValue ) << std::endl;
}
} // end namespace itk

#endif
