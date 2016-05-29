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
#ifndef itkLabelStatisticsImageFilter_hxx
#define itkLabelStatisticsImageFilter_hxx
#include "itkLabelStatisticsImageFilter.h"

#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkImageScanlineConstIterator.h"
#include "itkProgressReporter.h"

namespace itk
{
template< typename TInputImage, typename TLabelImage >
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::LabelStatisticsImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  m_UseHistograms = false;
  m_NumBins.SetSize(1);
  m_NumBins[0] = 20;
  m_LowerBound = static_cast< RealType >( NumericTraits< PixelType >::NonpositiveMin() );
  m_UpperBound = static_cast< RealType >( NumericTraits< PixelType >::max() );
  m_ValidLabelValues.clear();
}

template< typename TInputImage, typename TLabelImage >
void
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TLabelImage >
void
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::AllocateOutputs()
{
  // Pass the input through as the output
  InputImagePointer image =
    const_cast< TInputImage * >( this->GetInput() );

  this->GraftOutput(image);

  // Nothing that needs to be allocated for the remaining outputs
}

template< typename TInputImage, typename TLabelImage >
void
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::SetHistogramParameters(const int numBins, RealType lowerBound, RealType upperBound)
{
  m_NumBins[0] = numBins;
  m_LowerBound = lowerBound;
  m_UpperBound = upperBound;
  m_UseHistograms = true;
}

template< typename TInputImage, typename TLabelImage >
void
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::BeforeThreadedGenerateData()
{
  ThreadIdType numberOfThreads = this->GetNumberOfThreads();

  // Resize the thread temporaries
  m_LabelStatisticsPerThread.resize(numberOfThreads);

  // Initialize the temporaries
  for ( ThreadIdType i = 0; i < numberOfThreads; ++i )
    {
    m_LabelStatisticsPerThread[i].clear();
    }

  // Initialize the final map
  m_LabelStatistics.clear();
}

template< typename TInputImage, typename TLabelImage >
void
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::AfterThreadedGenerateData()
{
  MapIterator      mapIt;
  MapConstIterator threadIt;
  ThreadIdType     i;
  ThreadIdType     numberOfThreads = this->GetNumberOfThreads();

  // Run through the map for each thread and accumulate the count,
  // sum, and sumofsquares
  for ( i = 0; i < numberOfThreads; i++ )
    {
    // iterate over the map for this thread
    for ( threadIt = m_LabelStatisticsPerThread[i].begin();
          threadIt != m_LabelStatisticsPerThread[i].end();
          ++threadIt )
      {
      // does this label exist in the cumulative structure yet?
      mapIt = m_LabelStatistics.find( ( *threadIt ).first );
      if ( mapIt == m_LabelStatistics.end() )
        {
        // create a new entry
        typedef typename MapType::value_type MapValueType;
        if ( m_UseHistograms )
          {
          mapIt = m_LabelStatistics.insert( MapValueType( ( *threadIt ).first,
                                                          LabelStatistics(m_NumBins[0], m_LowerBound,
                                                                          m_UpperBound) ) ).first;
          }
        else
          {
          mapIt = m_LabelStatistics.insert( MapValueType( ( *threadIt ).first,
                                                          LabelStatistics() ) ).first;
          }
        }


      typename MapType::mapped_type &labelStats = ( *mapIt ).second;

      // accumulate the information from this thread
      labelStats.m_Count += ( *threadIt ).second.m_Count;
      labelStats.m_Sum += ( *threadIt ).second.m_Sum;
      labelStats.m_SumOfSquares += ( *threadIt ).second.m_SumOfSquares;

      if ( labelStats.m_Minimum > ( *threadIt ).second.m_Minimum )
        {
        labelStats.m_Minimum = ( *threadIt ).second.m_Minimum;
        }
      if ( labelStats.m_Maximum < ( *threadIt ).second.m_Maximum )
        {
        labelStats.m_Maximum = ( *threadIt ).second.m_Maximum;
        }

      //bounding box is min,max pairs
      for ( unsigned int ii = 0; ii < ( ImageDimension * 2 ); ii += 2 )
        {
        if ( labelStats.m_BoundingBox[ii] > ( *threadIt ).second.m_BoundingBox[ii] )
          {
          labelStats.m_BoundingBox[ii] = ( *threadIt ).second.m_BoundingBox[ii];
          }
        if ( labelStats.m_BoundingBox[ii + 1] < ( *threadIt ).second.m_BoundingBox[ii + 1] )
          {
          labelStats.m_BoundingBox[ii + 1] = ( *threadIt ).second.m_BoundingBox[ii + 1];
          }
        }

      // if enabled, update the histogram for this label
      if ( m_UseHistograms )
        {
        typename HistogramType::IndexType index;
        index.SetSize(1);
        for ( unsigned int bin = 0; bin < m_NumBins[0]; bin++ )
          {
          index[0] = bin;
          labelStats.m_Histogram->IncreaseFrequency( bin, ( *threadIt ).second.m_Histogram->GetFrequency(bin) );
          }
        }
      } // end of thread map iterator loop
    }   // end of thread loop

  // compute the remainder of the statistics
  for ( mapIt = m_LabelStatistics.begin();
        mapIt != m_LabelStatistics.end();
        ++mapIt )
    {
    typename MapType::mapped_type &labelStats = ( *mapIt ).second;

    // mean
    labelStats.m_Mean = labelStats.m_Sum
                               / static_cast< RealType >( labelStats.m_Count );

    // variance
    if ( labelStats.m_Count > 1 )
      {
      // unbiased estimate of variance
      LabelStatistics & ls = mapIt->second;
      const RealType    sumSquared  = ls.m_Sum * ls.m_Sum;
      const RealType    count       = static_cast< RealType >( ls.m_Count );

      ls.m_Variance = ( ls.m_SumOfSquares - sumSquared / count ) / ( count - 1.0 );
      }
    else
      {
      labelStats.m_Variance = NumericTraits< RealType >::ZeroValue();
      }

    // sigma
    labelStats.m_Sigma = std::sqrt( labelStats.m_Variance );
    }

    {
    //Now update the cached vector of valid labels.
    m_ValidLabelValues.resize(0);
    m_ValidLabelValues.reserve(m_LabelStatistics.size());
    for ( mapIt = m_LabelStatistics.begin();
      mapIt != m_LabelStatistics.end();
      ++mapIt )
      {
      m_ValidLabelValues.push_back(mapIt->first);
      }
    }
}

template< typename TInputImage, typename TLabelImage >
void
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::ThreadedGenerateData(const RegionType & outputRegionForThread,
                       ThreadIdType threadId)
{

  typename HistogramType::IndexType histogramIndex(1);
  typename HistogramType::MeasurementVectorType histogramMeasurement(1);

  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if( size0 == 0)
    {
    return;
    }

  ImageLinearConstIteratorWithIndex< TInputImage > it (this->GetInput(),
                                                       outputRegionForThread);

  ImageScanlineConstIterator< TLabelImage > labelIt (this->GetLabelInput(),
                                                     outputRegionForThread);

  MapIterator mapIt;

  // support progress methods/callbacks
  const size_t numberOfLinesToProcess = outputRegionForThread.GetNumberOfPixels() / size0;
  ProgressReporter progress( this, threadId, static_cast<SizeValueType>( numberOfLinesToProcess ) );

  // do the work
  while ( !it.IsAtEnd() )
    {
    while ( !it.IsAtEndOfLine() )
      {
      const RealType & value = static_cast< RealType >( it.Get() );

      const LabelPixelType & label = labelIt.Get();

      // is the label already in this thread?
      mapIt = m_LabelStatisticsPerThread[threadId].find(label);
      if ( mapIt == m_LabelStatisticsPerThread[threadId].end() )
        {
        // create a new statistics object
        typedef typename MapType::value_type MapValueType;
        if ( m_UseHistograms )
          {
          mapIt = m_LabelStatisticsPerThread[threadId].insert( MapValueType( label,
                                                                             LabelStatistics(m_NumBins[0], m_LowerBound,
                                                                                             m_UpperBound) ) ).first;
          }
        else
          {
          mapIt = m_LabelStatisticsPerThread[threadId].insert( MapValueType( label,
                                                                             LabelStatistics() ) ).first;
          }
        }

      typename MapType::mapped_type &labelStats = ( *mapIt ).second;

      // update the values for this label and this thread
      if ( value < labelStats.m_Minimum )
        {
        labelStats.m_Minimum = value;
        }
      if ( value > labelStats.m_Maximum )
        {
        labelStats.m_Maximum = value;
        }

      // bounding box is min,max pairs
      for ( unsigned int i = 0; i < ( 2 * TInputImage::ImageDimension ); i += 2 )
        {
        const IndexType & index = it.GetIndex();
        if ( labelStats.m_BoundingBox[i] > index[i / 2] )
          {
          labelStats.m_BoundingBox[i] = index[i / 2];
          }
        if ( labelStats.m_BoundingBox[i + 1] < index[i / 2] )
          {
          labelStats.m_BoundingBox[i + 1] = index[i / 2];
          }
        }

      labelStats.m_Sum += value;
      labelStats.m_SumOfSquares += ( value * value );
      labelStats.m_Count++;

      // if enabled, update the histogram for this label
      if ( m_UseHistograms )
        {
        histogramMeasurement[0] = value;
        labelStats.m_Histogram->GetIndex(histogramMeasurement, histogramIndex);
        labelStats.m_Histogram->IncreaseFrequencyOfIndex(histogramIndex, 1);
        }


      ++labelIt;
      ++it;
      }
    labelIt.NextLine();
    it.NextLine();
    progress.CompletedPixel();
    }

}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::RealType
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetMinimum(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< PixelType >::max();
    }
  else
    {
    return ( *mapIt ).second.m_Minimum;
    }
}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::RealType
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetMaximum(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< PixelType >::NonpositiveMin();
    }
  else
    {
    return ( *mapIt ).second.m_Maximum;
    }
}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::RealType
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetMean(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< PixelType >::ZeroValue();
    }
  else
    {
    return ( *mapIt ).second.m_Mean;
    }
}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::RealType
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetSum(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< PixelType >::ZeroValue();
    }
  else
    {
    return ( *mapIt ).second.m_Sum;
    }
}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::RealType
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetSigma(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< PixelType >::ZeroValue();
    }
  else
    {
    return ( *mapIt ).second.m_Sigma;
    }
}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::RealType
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetVariance(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits< PixelType >::ZeroValue();
    }
  else
    {
    return ( *mapIt ).second.m_Variance;
    }
}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::BoundingBoxType
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetBoundingBox(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);
  if ( mapIt == m_LabelStatistics.end() )
    {
    BoundingBoxType emptyBox;
    // label does not exist, return a default value
    return emptyBox;
    }
  else
    {
    return ( *mapIt ).second.m_BoundingBox;
    }
}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::RegionType
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetRegion(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);

  if ( mapIt == m_LabelStatistics.end() )
    {
    RegionType emptyRegion;
    // label does not exist, return a default value
    return emptyRegion;
    }
  else
    {
    BoundingBoxType bbox = this->GetBoundingBox(label);
    IndexType       index;
    SizeType        size;

    for ( unsigned int i = 0; i < ImageDimension; ++i )
      {
      index[i] = bbox[2 * i];
      size[i] = bbox[2 * i + 1] - bbox[2 * i] + 1;
      }
    RegionType region;
    region.SetSize(size);
    region.SetIndex(index);

    return region;
    }
}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::MapSizeType
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetCount(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return 0;
    }
  else
    {
    return ( *mapIt ).second.m_Count;
    }
}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::RealType
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetMedian(LabelPixelType label) const
{
  RealType         median = 0.0;
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);
  if ( mapIt == m_LabelStatistics.end() || !m_UseHistograms )
    {
    // label does not exist OR histograms not enabled, return a default value
    return median;
    }
  else
    {
    typename HistogramType::SizeValueType bin = 0;

    typename HistogramType::IndexType index;
    index.SetSize(1);
    RealType total = 0;

    // count bins until just over half the distribution is counted
    while ( total <= ( ( *mapIt ).second.m_Count / 2 ) && ( bin < m_NumBins[0] ) )
      {
      index[0] = bin;
      total += ( *mapIt ).second.m_Histogram->GetFrequency(index);
      bin++;
      }
    bin--;
    index[0] = bin;

    // return center of bin range
    RealType lowRange = ( *mapIt ).second.m_Histogram->GetBinMin(0, bin);
    RealType highRange  = ( *mapIt ).second.m_Histogram->GetBinMax(0, bin);
    median = lowRange + ( highRange - lowRange ) / 2;
    return median;
    }
}

template< typename TInputImage, typename TLabelImage >
typename LabelStatisticsImageFilter< TInputImage, TLabelImage >::HistogramPointer
LabelStatisticsImageFilter< TInputImage, TLabelImage >
::GetHistogram(LabelPixelType label) const
{
  MapConstIterator mapIt;

  mapIt = m_LabelStatistics.find(label);
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return ITK_NULLPTR;
    }
  else
    {
    // this will be zero if histograms have not been enabled
    return ( *mapIt ).second.m_Histogram;
    }
}

template< typename TImage, typename TLabelImage >
void
LabelStatisticsImageFilter< TImage, TLabelImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of labels: " << m_LabelStatistics.size()
     << std::endl;
  os << indent << "Use Histograms: " << m_UseHistograms
     << std::endl;
  os << indent << "Histogram Lower Bound: " << m_LowerBound
     << std::endl;
  os << indent << "Histogram Upper Bound: " << m_UpperBound
     << std::endl;
}
} // end namespace itk
#endif
