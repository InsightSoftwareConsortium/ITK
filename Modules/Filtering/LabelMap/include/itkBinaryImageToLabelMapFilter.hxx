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
#ifndef itkBinaryImageToLabelMapFilter_hxx
#define itkBinaryImageToLabelMapFilter_hxx

#include "itkBinaryImageToLabelMapFilter.h"
#include "itkNumericTraits.h"
#include "itkImageScanlineConstIterator.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkConnectedComponentAlgorithm.h"
#include "itkProgressReporter.h"
#include "itkProgressTransformer.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::BinaryImageToLabelMapFilter()
{
  this->m_FullyConnected = false;
  this->m_NumberOfObjects = 0;
  this->m_OutputBackgroundValue = NumericTraits< OutputPixelType >::NonpositiveMin();
  this->m_InputForegroundValue = NumericTraits< InputPixelType >::max();
}

template< typename TInputImage, typename TOutputImage >
void
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
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

template< typename TInputImage, typename TOutputImage >
void
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  TOutputImage * output = this->GetOutput();
  output->SetRequestedRegion( output->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Call a method that can be overriden by a subclass to allocate
  // memory for the filter's outputs
  this->AllocateOutputs();

  OutputImageType * output = this->GetOutput();
  output->SetBackgroundValue(this->m_OutputBackgroundValue);

  const typename OutputImageType::RegionType & requestedRegion = output->GetRequestedRegion();
  const typename OutputImageType::SizeType & requestedSize = requestedRegion.GetSize();

  // set up the vars used in the threads
  this->m_WorkUnitResults.clear();
  m_LineOffsets.clear();

  const SizeValueType pixelcount = requestedRegion.GetNumberOfPixels();
  const SizeValueType xsize = requestedSize[0];
  const SizeValueType linecount = pixelcount / xsize;
  m_LineMap.resize(linecount);

  this->SetupLineOffsets();

  ProgressTransformer progress1( 0.0f, 0.5f, this );

  MultiThreaderBase* multiThreader = this->GetMultiThreader();
  multiThreader->SetNumberOfWorkUnits( this->GetNumberOfWorkUnits() );
  multiThreader->template ParallelizeImageRegionRestrictDirection< TOutputImage::ImageDimension >(
    0,
    requestedRegion,
    [this]( const RegionType& lambdaRegion )
    {
      this->DynamicThreadedGenerateData( lambdaRegion );
    },
    progress1.GetProcessObject());

  // compute the total number of labels
  SizeValueType nbOfLabels = 0;
  for ( SizeValueType i = 0; i < m_WorkUnitResults.size(); ++i )
    {
    nbOfLabels += m_WorkUnitResults[i].numberOfLabels;
    }

  // set up the union find structure
  m_UnionFind = UnionFindType( nbOfLabels + 1 );
  // insert all the labels into the structure -- an extra loop but
  // saves complicating the ones that come later
  typename LineMapType::iterator MapBegin, MapEnd, LineIt;
  MapBegin = m_LineMap.begin();
  MapEnd = m_LineMap.end();
  LineIt = MapBegin;
  InternalLabelType label = 1;
  for ( LineIt = MapBegin; LineIt != MapEnd; ++LineIt )
    {
    typename lineEncoding::iterator cIt;
    for ( cIt = LineIt->begin(); cIt != LineIt->end(); ++cIt )
      {
      cIt->label = label;
      m_UnionFind[label] = label;
      label++;
      }
    }

  ProgressTransformer progress2( 0.55f, 0.6f, this );
  multiThreader->ParallelizeArray(
    0, m_WorkUnitResults.size(), [this]( SizeValueType index ) { this->ComputeEquivalence( index ); }, progress2.GetProcessObject());

  ProgressTransformer progress3( 0.6f, 0.75f, this );
  multiThreader->ParallelizeArray(
    0, m_WorkUnitResults.size(), [this]( SizeValueType index ) { this->MergeLabels( index ); }, progress3.GetProcessObject());

  // AfterThreadedGenerateData
  typename TInputImage::ConstPointer input = this->GetInput();
  m_NumberOfObjects = CreateConsecutive();
  ProgressReporter progress(this, 0, linecount, 25, 0.75f, 0.25f);
  // check for overflow exception here
  if ( m_NumberOfObjects > static_cast< SizeValueType >( NumericTraits< OutputPixelType >::max() ) )
    {
    itkExceptionMacro(
      << "Number of objects (" << m_NumberOfObjects << ") greater than maximum of output pixel type ("
      << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( NumericTraits< OutputPixelType >::
                                                                                   max() ) << ").");
    }

  for ( SizeValueType thisIdx = 0; thisIdx < linecount; thisIdx++ )
    {
    // now fill the labelled sections
    using LineIterator = typename lineEncoding::const_iterator;

    LineIterator cIt = m_LineMap[thisIdx].begin();
    const LineIterator cEnd = m_LineMap[thisIdx].end();

    while ( cIt != cEnd )
      {
      const InternalLabelType Ilab = LookupSet(cIt->label);
      const OutputPixelType lab = m_Consecutive[Ilab];
      output->SetLine(cIt->where, cIt->length, lab);
      ++cIt;
      }
    progress.CompletedPixel();
    }

  //clear and make sure memory is freed
  std::deque<WorkUnitData>().swap(m_WorkUnitResults);
  OffsetVectorType().swap(m_LineOffsets);
  LineMapType().swap(m_LineMap);
}

template< typename TInputImage, typename TOutputImage >
void
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::DynamicThreadedGenerateData(const RegionType & outputRegionForThread)
{
  TOutputImage * output = this->GetOutput();
  const TInputImage * input = this->GetInput();

  // create a line iterator
  using InputLineIteratorType = itk::ImageScanlineConstIterator< InputImageType >;
  InputLineIteratorType inLineIt(input, outputRegionForThread);

  const SizeValueType xsizeForThread = outputRegionForThread.GetSize()[0];

  // find the split axis
  const IndexType & outputRegionIdx = output->GetRequestedRegion().GetIndex();
  const IndexType & outputRegionForThreadIdx = outputRegionForThread.GetIndex();
  SizeType outputRegionSize = output->GetRequestedRegion().GetSize();
  const SizeType & outputRegionForThreadSize = outputRegionForThread.GetSize();
  int splitAxis = ( TOutputImage::ImageDimension > 1 ) ? 1 : 0;

  WorkUnitData workUnitData = { 0, 0, 0, 0 };

  // compute the number of pixels before that threads
  outputRegionSize[splitAxis] = outputRegionForThreadIdx[splitAxis] - outputRegionIdx[splitAxis];
  workUnitData.firstLineIdForThread =
    RegionType(outputRegionIdx, outputRegionSize).GetNumberOfPixels() / xsizeForThread;
  SizeValueType lineId = workUnitData.firstLineIdForThread;

  SizeType localRegionSize = outputRegionForThreadSize;
  localRegionSize[splitAxis] -= 1;
  SizeValueType lastLineIdForThread = workUnitData.firstLineIdForThread
    + RegionType(outputRegionIdx, localRegionSize).GetNumberOfPixels() / xsizeForThread;
  workUnitData.firstLineIdToJoin = lastLineIdForThread;

  // found the number of line ids to join
  workUnitData.numberOfLineIdsToJoin =
    RegionType( outputRegionIdx, outputRegionForThread.GetSize() ).GetNumberOfPixels() / xsizeForThread
    - RegionType(outputRegionIdx, localRegionSize).GetNumberOfPixels() / xsizeForThread;

  for ( inLineIt.GoToBegin();
        !inLineIt.IsAtEnd();
        inLineIt.NextLine() )
    {
    inLineIt.GoToBeginOfLine();
    lineEncoding thisLine;
    while ( !inLineIt.IsAtEndOfLine() )
      {
      const InputPixelType pixelValue = inLineIt.Get();
      if ( pixelValue == this->m_InputForegroundValue )
        {
        // We've hit the start of a run
        runLength thisRun;
        SizeValueType length = 0;
        IndexType thisIndex;
        thisIndex = inLineIt.GetIndex();
        ++length;
        ++inLineIt;
        while ( !inLineIt.IsAtEndOfLine()
                && inLineIt.Get() == this->m_InputForegroundValue )
          {
          ++length;
          ++inLineIt;
          }
        // create the run length object to go in the vector
        thisRun.length = length;
        thisRun.label = 0; // will give a real label later
        thisRun.where = thisIndex;
        thisLine.push_back(thisRun);
        ++workUnitData.numberOfLabels;
        }
      else
        {
        ++inLineIt;
        }
      }
    m_LineMap[lineId] = thisLine;
    ++lineId;
    }

  std::lock_guard<std::mutex> mutexHolder(m_Mutex);
  this->m_WorkUnitResults.push_back( workUnitData );
}

template< typename TInputImage, typename TOutputImage >
void
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::ComputeEquivalence(const SizeValueType workUnitResultsIndex)
{
  // process the map and make appropriate entries in an equivalence table
  const OffsetValueType linecount = m_LineMap.size();

  for ( SizeValueType thisIdx = m_WorkUnitResults[workUnitResultsIndex].firstLineIdForThread;
        thisIdx < m_WorkUnitResults[workUnitResultsIndex].firstLineIdToJoin;
        ++thisIdx )
    {
    if ( !m_LineMap[thisIdx].empty() )
      {
      typename OffsetVectorType::const_iterator I = m_LineOffsets.begin();
      while ( I != m_LineOffsets.end() )
        {
        OffsetValueType NeighIdx = thisIdx + ( *I );
        // check if the neighbor is in the map
        if ( NeighIdx >= 0 && NeighIdx < linecount && !m_LineMap[NeighIdx].empty() )
          {
          // Now check whether they are really neighbors
          bool areNeighbors = this->CheckNeighbors(m_LineMap[thisIdx][0].where, m_LineMap[NeighIdx][0].where);
          if ( areNeighbors )
            {
            this->CompareLines(m_LineMap[thisIdx], m_LineMap[NeighIdx]);
            }
          }
        ++I;
        }
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::MergeLabels( const SizeValueType workUnitResultsIndex )
{
  const OffsetValueType linecount = m_LineMap.size();
  WorkUnitData wud = m_WorkUnitResults[workUnitResultsIndex];
  for ( SizeValueType thisIdx = wud.firstLineIdToJoin;
        thisIdx < wud.firstLineIdToJoin + wud.numberOfLineIdsToJoin;
        ++thisIdx )
    {
    if ( !m_LineMap[thisIdx].empty() )
      {
      typename OffsetVectorType::const_iterator I = m_LineOffsets.begin();
      while ( I != m_LineOffsets.end() )
        {
        OffsetValueType NeighIdx = thisIdx + ( *I );
        // check if the neighbor is in the map
        if ( NeighIdx >= 0 && NeighIdx < linecount && !m_LineMap[NeighIdx].empty() )
          {
          // Now check whether they are really neighbors
          bool areNeighbors =
            CheckNeighbors(m_LineMap[thisIdx][0].where, m_LineMap[NeighIdx][0].where);
          if ( areNeighbors )
            {
            // Compare the two lines
            CompareLines(m_LineMap[thisIdx], m_LineMap[NeighIdx]);
            }
          }
        ++I;
        }
      }
    }
}


template< typename TInputImage, typename TOutputImage >
void
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::SetupLineOffsets()
{
  // Create a neighborhood so that we can generate a table of offsets
  // to "previous" line indexes
  // We are going to mis-use the neighborhood iterators to compute the
  // offset for us. All this messing around produces an array of
  // offsets that will be used to index the map
  typename TOutputImage::Pointer output = this->GetOutput();
  using PretendImageType = Image< OffsetValueType, TOutputImage::ImageDimension - 1 >;
  using PretendSizeType = typename PretendImageType::RegionType::SizeType;
  using PretendIndexType = typename PretendImageType::RegionType::IndexType;
  using LineNeighborhoodType = ConstShapedNeighborhoodIterator< PretendImageType >;

  typename PretendImageType::Pointer fakeImage;
  fakeImage = PretendImageType::New();

  typename PretendImageType::RegionType LineRegion;
  //LineRegion = PretendImageType::RegionType::New();

  OutSizeType OutSize = output->GetRequestedRegion().GetSize();

  PretendSizeType PretendSize;
  // The first dimension has been collapsed
  for ( SizeValueType i = 0; i < PretendSize.GetSizeDimension(); i++ )
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
  OffsetValueType  offset = fakeImage->ComputeOffset(idx);

  for ( LI = ActiveIndexes.begin(); LI != ActiveIndexes.end(); LI++ )
    {
    m_LineOffsets.push_back(fakeImage->ComputeOffset( idx + lnit.GetOffset(*LI) ) - offset);
    }
}

template< typename TInputImage, typename TOutputImage >
bool
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::CheckNeighbors(const OutputIndexType & A,
                 const OutputIndexType & B)
{
  // this checks whether the line encodings are really neighbors. The
  // first dimension gets ignored because the encodings are along that
  // axis
  for ( unsigned i = 1; i < OutputImageDimension; i++ )
    {
    if ( itk::Math::abs(A[i] - B[i]) > 1 )
      {
      return false;
      }
    }
  return true;
}

template< typename TInputImage, typename TOutputImage >
void
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::CompareLines(lineEncoding & current, const lineEncoding & Neighbour)
{
  OffsetValueType offset = 0;

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
    OffsetValueType cStart = cIt->where[0];  // the start x position
    OffsetValueType cLast = cStart + cIt->length - 1;

    for ( nIt = mIt; nIt != Neighbour.end(); ++nIt )
      {
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
      OffsetValueType ee1 = nLast - offset;
      OffsetValueType ee2 = nLast + offset;
      bool eq = false;
      if ( ( ss1 >= cStart ) && ( ss1 <= cLast ) )
        {
        // case 1 or 2
        eq = true;
        }
      else if ( ( cStart >= ss1 ) && ( cStart <= ee2 ) )
        {
        // case 3 or 4
        eq = true;
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

template< typename TInputImage, typename TOutputImage >
typename BinaryImageToLabelMapFilter< TInputImage, TOutputImage >::SizeValueType
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::CreateConsecutive()
{
  const size_t N = m_UnionFind.size();

  m_Consecutive = ConsecutiveVectorType( N );
  m_Consecutive[ 0 ] = this->m_OutputBackgroundValue;

  OutputPixelType consecutiveLabel = 0;
  SizeValueType count = 0;

  for ( size_t i = 1; i < N; i++ )
    {
    const auto label = static_cast< size_t >( m_UnionFind[i] );
    if ( label == i )
      {
      m_Consecutive[label] = ++consecutiveLabel;
      ++count;
      }
    }
  return count;
}

template< typename TInputImage, typename TOutputImage >
typename BinaryImageToLabelMapFilter< TInputImage, TOutputImage >::InternalLabelType
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::LookupSet(const InternalLabelType label)
{
  InternalLabelType l = label;
  while (l != m_UnionFind[l])
    {
    l = m_UnionFind[l]; //transitively sets equivalence
    }
  return l;
}

template< typename TInputImage, typename TOutputImage >
void
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::LinkLabels(const InternalLabelType lab1, const InternalLabelType lab2)
{
  std::lock_guard<std::mutex> mutexHolder(m_Mutex);
  InternalLabelType E1 = this->LookupSet(lab1);
  InternalLabelType E2 = this->LookupSet(lab2);

  if ( E1 < E2 )
    {
    m_UnionFind[E2] = E1;
    }
  else
    {
    m_UnionFind[E1] = E2;
    }
}

template< typename TInputImage, typename TOutputImage >
void
BinaryImageToLabelMapFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "InputForegroundValue: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( this->m_InputForegroundValue ) << std::endl;
  os << indent << "OutputBackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( this->m_OutputBackgroundValue )
     << std::endl;
  os << indent << "Number of Objects: " << this->m_NumberOfObjects << std::endl;
}
} // end namespace itk

#endif
