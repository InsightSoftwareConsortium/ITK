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
#ifndef itkLabelContourImageFilter_hxx
#define itkLabelContourImageFilter_hxx

#include "itkLabelContourImageFilter.h"

// don't think we need the indexed version as we only compute the
// index at the start of each run, but there isn't a choice
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkConnectedComponentAlgorithm.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
LabelContourImageFilter< TInputImage, TOutputImage >
::LabelContourImageFilter() :
  m_BackgroundValue( NumericTraits< OutputImagePixelType >::ZeroValue() ),
  m_NumberOfWorkUnits( 0 ),
  m_FullyConnected( false )
{
  this->SetInPlace(false);
  this->DynamicMultiThreadingOn();
}

// -----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
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
template< typename TInputImage, typename TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template<typename TInputImage, typename TOutputImage>
void
LabelContourImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  this->UpdateProgress(0.0f);
  this->AllocateOutputs();
  this->BeforeThreadedGenerateData();
  this->UpdateProgress(0.05f);

  OutputRegionType reqRegion = this->GetOutput()->GetRequestedRegion();

  this->GetMultiThreader()->SetNumberOfWorkUnits( this->GetNumberOfWorkUnits() );
  //parallelize in a way which does not split the region along X axis
  //to accomplish this, we parallelize a region with lower dimension
  //which we extend with full scanlines along X
  this->GetMultiThreader()->ParallelizeImageRegion(
      ImageDimension - 1,
      &reqRegion.GetIndex()[1],
      &reqRegion.GetSize()[1],
      [&](const IndexValueType index[], const SizeValueType size[])
      {
        OutputRegionType r;
        r.SetIndex(0, reqRegion.GetIndex(0));
        r.SetSize(0, reqRegion.GetSize(0));
        for (unsigned d = 1; d < ImageDimension; d++)
          {
          r.SetIndex(d, index[d - 1]);
          r.SetSize(d, size[d - 1]);
      }
      this->DynamicThreadedGenerateData(r);
      },
      nullptr);
  this->UpdateProgress(0.5f);

  //avoid splitting the region along X
  this->GetMultiThreader()->ParallelizeImageRegion(
      ImageDimension - 1,
      &reqRegion.GetIndex()[1],
      &reqRegion.GetSize()[1],
      [&](const IndexValueType index[], const SizeValueType size[])
      {
      OutputRegionType r;
      r.SetIndex(0, reqRegion.GetIndex(0));
      r.SetSize(0, reqRegion.GetSize(0));
      for (unsigned d = 1; d < ImageDimension; d++)
        {
        r.SetIndex(d, index[d - 1]);
        r.SetSize(d, size[d - 1]);
        }
      this->ThreadedIntegrateData(r);
      },
      nullptr);
  this->UpdateProgress(0.99f);

  this->AfterThreadedGenerateData();
  this->UpdateProgress(1.0f);
}

// -----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  OutputImageType* output = this->GetOutput();

  SizeValueType pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  SizeValueType xsize = output->GetRequestedRegion().GetSize()[0];
  SizeValueType linecount = pixelcount / xsize;

  m_LineMap.clear();
  m_LineMap.resize(linecount);
}

template<typename TInputImage, typename TOutputImage>
SizeValueType
LabelContourImageFilter<TInputImage, TOutputImage>
::IndexToLinearIndex(OutputIndexType index)
{
  SizeValueType li = 0;
  SizeValueType stride = 1;
  OutputRegionType r = this->GetOutput()->GetRequestedRegion();
  //ignore x axis, which is always full size
  for (unsigned d = 1; d < ImageDimension; d++)
    {
    itkAssertOrThrowMacro(r.GetIndex(d) <= index[d],
        "Index must be within requested region!");
    li += (index[d] - r.GetIndex(d))*stride;
    stride *= r.GetSize(d);
    }
  return li;
}

// -----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::DynamicThreadedGenerateData(const OutputRegionType & outputRegionForThread)
{
  OutputImageType   *output = this->GetOutput();
  const InputImageType *input = this->GetInput();

  using InputLineIteratorType = itk::ImageLinearConstIteratorWithIndex<InputImageType>;
  InputLineIteratorType inLineIt(input, outputRegionForThread);
  inLineIt.SetDirection(0);

  using OutputLineIteratorType = itk::ImageLinearIteratorWithIndex<OutputImageType>;
  OutputLineIteratorType outLineIt(output, outputRegionForThread);
  outLineIt.SetDirection(0);

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
    SizeValueType lineId = IndexToLinearIndex(inLineIt.GetIndex());
    m_LineMap[lineId] = Line;
    }

}

template< typename TInputImage, typename TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::ThreadedIntegrateData(const OutputRegionType & outputRegionForThread)
{
  OutputImageType *output = this->GetOutput();

  using OutputLineIteratorType = itk::ImageLinearIteratorWithIndex<OutputImageType>;
  OutputLineIteratorType outLineIt(output, outputRegionForThread);
  outLineIt.SetDirection(0);

  OffsetVectorType LineOffsets;
  SetupLineOffsets(LineOffsets);

  SizeValueType   pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  SizeValueType   xsize = output->GetRequestedRegion().GetSize()[0];
  OffsetValueType linecount = pixelcount / xsize;
  itkAssertInDebugAndIgnoreInReleaseMacro(SizeValueType(linecount) == m_LineMap.size());

  for (outLineIt.GoToBegin(); !outLineIt.IsAtEnd(); outLineIt.NextLine())
    {
    SizeValueType ThisIdx = this->IndexToLinearIndex(outLineIt.GetIndex());
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
              CompareLines( output,
                            m_LineMap[ThisIdx],
                            m_LineMap[NeighIdx] );
              }
            }
          }
        }
      }
    }
}

// -----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::AfterThreadedGenerateData()
{
  m_LineMap.clear();
}

// -----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::SetupLineOffsets(OffsetVectorType & LineOffsets)
{
  // Create a neighborhood so that we can generate a table of offsets
  // to "previous" line indexes
  // We are going to mis-use the neighborhood iterators to compute the
  // offset for us. All this messing around produces an array of
  // offsets that will be used to index the map
  OutputImageType* output = this->GetOutput();

  const unsigned int PretendDimension = ImageDimension - 1;

  using PretendImageType = Image< OffsetValueType, PretendDimension >;
  using PretendImagePointer = typename PretendImageType::Pointer;
  using PretendRegionType = typename PretendImageType::RegionType;
  using PretendSizeType = typename PretendRegionType::SizeType;
  using PretendIndexType = typename PretendRegionType::IndexType;

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

  using LineNeighborhoodType = ConstShapedNeighborhoodIterator< PretendImageType >;

  LineNeighborhoodType lnit(kernelRadius, fakeImage, LineRegion);

  setConnectivity(&lnit, m_FullyConnected);

  using LineNeighborhoodIndexListType = typename LineNeighborhoodType::IndexListType;
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
template< typename TInputImage, typename TOutputImage >
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
    if ( itk::Math::abs(Off[i]) > 1 )
      {
      return ( false );
      }
    }
  return ( true );
}

// -----------------------------------------------------------------------------
template< typename TInputImage, typename TOutputImage >
void
LabelContourImageFilter< TInputImage, TOutputImage >
::CompareLines(TOutputImage *output, LineEncodingType & current, const LineEncodingType & Neighbour)
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

  auto cIt = current.begin();
  auto cEnd = current.end();

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
      for(auto mIt = Neighbour.begin();
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
            itkAssertInDebugAndIgnoreInReleaseMacro(oStart <= oLast);
            OutputIndexType idx = cIt->where;
            for ( OffsetValueType x = oStart; x <= oLast; ++x )
              {
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
template< typename TInputImage, typename TOutputImage >
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
