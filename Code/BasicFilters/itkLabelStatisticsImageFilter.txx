/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkLabelStatisticsImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLabelStatisticsImageFilter_txx
#define _itkLabelStatisticsImageFilter_txx
#include "itkLabelStatisticsImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"

namespace itk {
#if defined(__GNUC__) && (__GNUC__ <= 2) //NOTE: This class needs a mutex for gnu 2.95
/** Used for mutex locking */
#define LOCK_HASHMAP this->m_Mutex.Lock()
#define UNLOCK_HASHMAP this->m_Mutex.Unlock()
#else
#define LOCK_HASHMAP
#define UNLOCK_HASHMAP
#endif

template<class TInputImage, class TLabelImage>
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::LabelStatisticsImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  m_UseHistograms = false;
  m_NumBins[0] = 20;
  m_LowerBound = static_cast<RealType>( NumericTraits<PixelType>::NonpositiveMin() );
  m_UpperBound = static_cast<RealType>( NumericTraits<PixelType>::max() );
}



template<class TInputImage, class TLabelImage>
void
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image =
      const_cast< typename Superclass::InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
  if ( this->GetLabelInput() )
    {
    LabelImagePointer label =
      const_cast< TLabelImage * >( this->GetLabelInput() );
    label->SetRequestedRegionToLargestPossibleRegion();
    }
}

template<class TInputImage, class TLabelImage>
void
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}


template<class TInputImage, class TLabelImage>
void
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::AllocateOutputs()
{
  // Pass the input through as the output
  InputImagePointer image =
    const_cast< TInputImage * >( this->GetInput() );
  this->GraftOutput( image );

  // Nothing that needs to be allocated for the remaining outputs
}

template<class TInputImage, class TLabelImage>
void
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::SetHistogramParameters(const int numBins, RealType lowerBound, RealType upperBound)
{
  m_NumBins[0] = numBins;
  m_LowerBound = lowerBound;
  m_UpperBound = upperBound;
  m_UseHistograms = true;
}

template<class TInputImage, class TLabelImage>
void
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::BeforeThreadedGenerateData()
{
  int numberOfThreads = this->GetNumberOfThreads();

  // Resize the thread temporaries
  m_LabelStatisticsPerThread.resize(numberOfThreads);
  
  // Initialize the temporaries
  for (int i=0; i < numberOfThreads; ++i)
    {
    m_LabelStatisticsPerThread[i].clear();
    }

  // Initialize the final map
  m_LabelStatistics.clear();
}

template<class TInputImage, class TLabelImage>
void
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::AfterThreadedGenerateData()
{
  MapIterator mapIt;
  MapConstIterator threadIt;
  int i;
  int numberOfThreads = this->GetNumberOfThreads();

  // Run through the map for each thread and accumulate the count,
  // sum, and sumofsquares
  for (i = 0; i < numberOfThreads; i++)
    {
    // iterate over the map for this thread
    for (threadIt = m_LabelStatisticsPerThread[i].begin();
      threadIt != m_LabelStatisticsPerThread[i].end();
      ++threadIt)
      {
      // does this label exist in the cumulative stucture yet?
      mapIt = m_LabelStatistics.find( (*threadIt).first );
      if (mapIt == m_LabelStatistics.end())
        {
        // create a new entry
        typedef typename MapType::value_type MapValueType;
        if (m_UseHistograms)
          {
          mapIt = m_LabelStatistics.insert( MapValueType((*threadIt).first, 
             LabelStatistics(m_NumBins[0], m_LowerBound, m_UpperBound)) ).first;
          }
        else
          {
          mapIt = m_LabelStatistics.insert( MapValueType((*threadIt).first, 
              LabelStatistics()) ).first;
          }
        }

      // accumulate the information from this thread
      (*mapIt).second.m_Count += (*threadIt).second.m_Count;
      (*mapIt).second.m_Sum += (*threadIt).second.m_Sum;
      (*mapIt).second.m_SumOfSquares += (*threadIt).second.m_SumOfSquares;

      if ((*mapIt).second.m_Minimum > (*threadIt).second.m_Minimum)
        {
        (*mapIt).second.m_Minimum = (*threadIt).second.m_Minimum;
        }
      if ((*mapIt).second.m_Maximum < (*threadIt).second.m_Maximum)
        {
        (*mapIt).second.m_Maximum = (*threadIt).second.m_Maximum;
        }

      //bounding box is min,max pairs
      int dimension = (*mapIt).second.m_BoundingBox.size() / 2;
      for (int i = 0; i < (dimension * 2) ; i += 2 ) 
         {
         if ((*mapIt).second.m_BoundingBox[i] > (*threadIt).second.m_BoundingBox[i])
           {
           (*mapIt).second.m_BoundingBox[i] = (*threadIt).second.m_BoundingBox[i];
           }
         if ((*mapIt).second.m_BoundingBox[i + 1] < (*threadIt).second.m_BoundingBox[i + 1])
           {
           (*mapIt).second.m_BoundingBox[i + 1] = (*threadIt).second.m_BoundingBox[i + 1];
           }
         }

      // if enabled, update the histogram for this label
      if (m_UseHistograms)
        {
        typename HistogramType::IndexType index;
        for (unsigned int bin=0; bin<m_NumBins[0]; bin++)
          {
          index[0] = bin;
          (*mapIt).second.m_Histogram->IncreaseFrequency(bin, (*threadIt).second.m_Histogram->GetFrequency(bin));
          }
        }
      } // end of thread map iterator loop
    } // end of thread loop
  
  // compute the remainder of the statistics 
  for (mapIt = m_LabelStatistics.begin();
       mapIt != m_LabelStatistics.end();
       ++mapIt)
    {
    // mean
    (*mapIt).second.m_Mean = (*mapIt).second.m_Sum /
      static_cast<RealType>( (*mapIt).second.m_Count );


    // variance
    if ((*mapIt).second.m_Count > 1)
      {
      // unbiased estimate of variance
      (*mapIt).second.m_Variance
        = ((*mapIt).second.m_SumOfSquares
           - ((*mapIt).second.m_Sum*(*mapIt).second.m_Sum
              / static_cast<RealType>((*mapIt).second.m_Count)))
        / (static_cast<RealType>((*mapIt).second.m_Count) - 1);
      }
    else
      {
      (*mapIt).second.m_Variance = NumericTraits<RealType>::Zero;
      }
    
    // sigma
    (*mapIt).second.m_Sigma = sqrt((*mapIt).second.m_Variance);
    }
  
}

template<class TInputImage, class TLabelImage>
void
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::ThreadedGenerateData(const RegionType& outputRegionForThread,
         int threadId) 
{
  RealType value;
  LabelPixelType label;
  ImageRegionConstIteratorWithIndex<TInputImage> it (this->GetInput(),
         outputRegionForThread);
  ImageRegionConstIterator<TLabelImage> labelIt (this->GetLabelInput(),
       outputRegionForThread);
  MapIterator mapIt;
  
  // support progress methods/callbacks
  ProgressReporter progress(this, threadId,
       outputRegionForThread.GetNumberOfPixels());

  // do the work
  while (!it.IsAtEnd())
    {
    value = static_cast<RealType>(it.Get());
    label = labelIt.Get();

    // is the label already in this thread?
    mapIt = m_LabelStatisticsPerThread[threadId].find( label );
    if (mapIt == m_LabelStatisticsPerThread[threadId].end())
      {
      // create a new statistics object
      typedef typename MapType::value_type MapValueType;
      LOCK_HASHMAP;
      if (m_UseHistograms)
        {
        mapIt = m_LabelStatisticsPerThread[threadId].insert( MapValueType(label, 
               LabelStatistics(m_NumBins[0], m_LowerBound, m_UpperBound)) ).first;
        }
      else
        {
        mapIt = m_LabelStatisticsPerThread[threadId].insert( MapValueType(label, 
               LabelStatistics()) ).first;
        }
      UNLOCK_HASHMAP;
      }

    // update the values for this label and this thread
    if (value < (*mapIt).second.m_Minimum)
      {
      (*mapIt).second.m_Minimum = value;
      }
    if (value > (*mapIt).second.m_Maximum)
      {
      (*mapIt).second.m_Maximum = value;
      }

    // bounding box is min,max pairs
    for (unsigned int i = 0; i < ( 2 * it.GetImageDimension()) ; i+=2 ) 
      {
      typename ImageRegionConstIteratorWithIndex<TInputImage>::IndexType index = it.GetIndex();
      if ((*mapIt).second.m_BoundingBox[i] > index[i/2])
        {
        (*mapIt).second.m_BoundingBox[i] = index[i/2];
        }
      if ((*mapIt).second.m_BoundingBox[i + 1] < index[i/2])
        {
        (*mapIt).second.m_BoundingBox[i + 1] = index[i/2];
        }
      }

    (*mapIt).second.m_Sum += value;
    (*mapIt).second.m_SumOfSquares += (value * value);
    (*mapIt).second.m_Count++;

    // if enabled, update the histogram for this label
    if (m_UseHistograms)
      {
      typename HistogramType::MeasurementVectorType meas;
      meas[0] = value;
      (*mapIt).second.m_Histogram->IncreaseFrequency(meas, 1.0F);
      }

    ++it;
    ++labelIt;
    progress.CompletedPixel();
    }
}

template<class TInputImage, class TLabelImage>
typename LabelStatisticsImageFilter<TInputImage, TLabelImage>::RealType
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetMinimum(LabelPixelType label) const
{
  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits<PixelType>::max();
    }
  else
    {
    return (*mapIt).second.m_Minimum;
    }
}

template<class TInputImage, class TLabelImage>
typename LabelStatisticsImageFilter<TInputImage, TLabelImage>::RealType
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetMaximum(LabelPixelType label) const
{
  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits<PixelType>::NonpositiveMin();
    }
  else
    {
    return (*mapIt).second.m_Maximum;
    }
}

template<class TInputImage, class TLabelImage>
typename LabelStatisticsImageFilter<TInputImage, TLabelImage>::RealType
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetMean(LabelPixelType label) const
{
  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits<PixelType>::Zero;
    }
  else
    {
    return (*mapIt).second.m_Mean;
    }
}

template<class TInputImage, class TLabelImage>
typename LabelStatisticsImageFilter<TInputImage, TLabelImage>::RealType
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetSum(LabelPixelType label) const
{
  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits<PixelType>::Zero;
    }
  else
    {
    return (*mapIt).second.m_Sum;
    }
}

template<class TInputImage, class TLabelImage>
typename LabelStatisticsImageFilter<TInputImage, TLabelImage>::RealType
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetSigma(LabelPixelType label) const
{
  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits<PixelType>::Zero;
    }
  else
    {
    return (*mapIt).second.m_Sigma;
    }
}

template<class TInputImage, class TLabelImage>
typename LabelStatisticsImageFilter<TInputImage, TLabelImage>::RealType
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetVariance(LabelPixelType label) const
{
  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return NumericTraits<PixelType>::Zero;
    }
  else
    {
    return (*mapIt).second.m_Variance;
    }
}

template<class TInputImage, class TLabelImage>
typename LabelStatisticsImageFilter<TInputImage, TLabelImage>::BoundingBoxType
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetBoundingBox(LabelPixelType label) const
{

  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );
  if ( mapIt == m_LabelStatistics.end() )
    {
    BoundingBoxType emptyBox;
    // label does not exist, return a default value
    return emptyBox;
    }
  else
    {
    return (*mapIt).second.m_BoundingBox;
    }
}

template<class TInputImage, class TLabelImage>
typename LabelStatisticsImageFilter<TInputImage, TLabelImage>::RegionType
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetRegion(LabelPixelType label) const
{
  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );

  if ( mapIt == m_LabelStatistics.end() )
    {
    RegionType emptyRegion;
    // label does not exist, return a default value
    return emptyRegion;
    }
  else
    {
    BoundingBoxType bbox = this->GetBoundingBox( label );
    IndexType index;
    SizeType size;

    unsigned int dimension = bbox.size() / 2;

    for (unsigned int i = 0; i < dimension; i++)
      {
      index[i] = bbox[2*i];
      size[i] = bbox[2*i+1] - bbox[2*i] + 1;
      }
    RegionType region;
    region.SetSize(size);
    region.SetIndex(index);
    
    return region;
    }
}

template<class TInputImage, class TLabelImage>
unsigned long
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetCount(LabelPixelType label) const
{
  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );
  if ( mapIt == m_LabelStatistics.end() )
    {
    // label does not exist, return a default value
    return 0;
    }
  else
    {
    return (*mapIt).second.m_Count;
    }
}

template<class TInputImage, class TLabelImage>
typename LabelStatisticsImageFilter<TInputImage, TLabelImage>::RealType
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetMedian(LabelPixelType label) const
{
  RealType median = 0.0;
  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );
  if ( mapIt == m_LabelStatistics.end() || !m_UseHistograms)
    {
    // label does not exist OR histograms not enabled, return a default value
    return median;
    }
  else
    {
    typename HistogramType::SizeType::SizeValueType  bin = 0;
    typename HistogramType::IndexType index;
    RealType total = 0;

    // count bins until just over half the distribution is counted
    while (total <= ((*mapIt).second.m_Count/ 2) && (bin < m_NumBins[0])) 
      {
      index[0] = bin;
      total += (*mapIt).second.m_Histogram->GetFrequency(index);
      bin++;
      }
    bin--;
    index[0] = bin;

    // return center of bin range
    RealType lowRange = (*mapIt).second.m_Histogram->GetBinMin(0, bin);
    RealType highRange  = (*mapIt).second.m_Histogram->GetBinMax(0, bin);
    median = lowRange + (highRange - lowRange) / 2;
    return median;
    }
}

template<class TInputImage, class TLabelImage>
typename LabelStatisticsImageFilter<TInputImage, TLabelImage>::HistogramPointer
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::GetHistogram(LabelPixelType label) const
{
  MapConstIterator mapIt;
  mapIt = m_LabelStatistics.find( label );
  if ( mapIt == m_LabelStatistics.end())
    {
    // label does not exist, return a default value
    return 0;
    }
  else
    {
    // this will be zero if histograms have not been enabled
    return (*mapIt).second.m_Histogram;
    }
}

template <class TImage, class TLabelImage>
void 
LabelStatisticsImageFilter<TImage, TLabelImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Number of labels: " << m_LabelStatistics.size()
     << std::endl;
  os << indent << "Use Histograms: " << m_UseHistograms
     << std::endl;
  os << indent << "Histogram Lower Bound: " << m_LowerBound
     << std::endl;
  os << indent << "Histogram Upper Bound: " << m_UpperBound
     << std::endl;
}


}// end namespace itk
#endif
