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
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"

namespace itk {

template<class TInputImage, class TLabelImage>
LabelStatisticsImageFilter<TInputImage, TLabelImage>
::LabelStatisticsImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
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
      const_cast< typename TLabelImage * >( this->GetLabelInput() );
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
        mapIt
          = m_LabelStatistics.insert(MapType::value_type((*threadIt).first,
                                                   LabelStatistics())).first;
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
  ImageRegionConstIterator<TInputImage> it (this->GetInput(),
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
      mapIt = m_LabelStatisticsPerThread[threadId].insert( MapType::value_type(label, LabelStatistics()) ).first;
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

    (*mapIt).second.m_Sum += value;
    (*mapIt).second.m_SumOfSquares += (value * value);
    (*mapIt).second.m_Count++;
    ++it;
    ++labelIt;
    progress.CompletedPixel();
    }
}

template<class TInputImage, class TLabelImage>
typename NumericTraits<typename TInputImage::PixelType>::RealType
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
typename NumericTraits<typename TInputImage::PixelType>::RealType
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
typename NumericTraits<typename TInputImage::PixelType>::RealType
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
typename NumericTraits<typename TInputImage::PixelType>::RealType
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
typename NumericTraits<typename TInputImage::PixelType>::RealType
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
typename NumericTraits<typename TInputImage::PixelType>::RealType
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

template <class TImage, class TLabelImage>
void 
LabelStatisticsImageFilter<TImage, TLabelImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Number of labels: " << m_LabelStatistics.size()
     << std::endl;
}


}// end namespace itk
#endif
