/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOtsuMultipleThresholdsCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkOtsuMultipleThresholdsCalculator_txx
#define _itkOtsuMultipleThresholdsCalculator_txx

#include "itkOtsuMultipleThresholdsCalculator.h"

namespace itk{
  namespace Statistics{

template<class TInputHistogram>
OtsuMultipleThresholdsCalculator<TInputHistogram>
::OtsuMultipleThresholdsCalculator()
{
  m_NumberOfThresholds = 1;
  m_Output.resize(m_NumberOfThresholds);
  std::fill(m_Output.begin(),m_Output.end(),NumericTraits<MeasurementType>::Zero);
}
                                                    
template<class TInputHistogram>
const typename OtsuMultipleThresholdsCalculator<TInputHistogram>::OutputType&
OtsuMultipleThresholdsCalculator< TInputHistogram >
::GetOutput()
{
  return m_Output ;
}

/*
 * Increment the thresholds of one position along the histogram
 */
template<class TInputHistogram>
bool 
OtsuMultipleThresholdsCalculator<TInputHistogram>
::IncrementThresholds(InstanceIdentifierVectorType& thresholdIndexes, MeasurementType globalMean, MeanVectorType& classMean, FrequencyVectorType& classFrequency)
{
  typename TInputHistogram::ConstPointer histogram = this->GetInputHistogram();

  unsigned long numberOfHistogramBins = histogram->Size();
  unsigned long numberOfClasses = classMean.size();

  double meanOld, freqOld;

  unsigned int k;
  int j;

  // from the upper threshold down
  for(j=static_cast<int>(m_NumberOfThresholds-1); j>=0; j--)
    {
    // if this threshold can be incremented (i.e. we're not at the end of the histogram)
    if (thresholdIndexes[j] < numberOfHistogramBins - 2 - (m_NumberOfThresholds-1 - j) )
      {
      // increment it and update mean and frequency of the class bounded by the threshold
      ++thresholdIndexes[j];

      meanOld = classMean[j];
      freqOld = classFrequency[j];
      
      classFrequency[j] += histogram->GetFrequency(thresholdIndexes[j]);
      
      if (classFrequency[j] != 0.0)
        {
        classMean[j] = (meanOld * freqOld + histogram->GetMeasurementVector(thresholdIndexes[j])[0] * histogram->GetFrequency(thresholdIndexes[j])) / classFrequency[j];
        }
      else
        {
        classMean[j] = 0.0;
        }
      
      // set higher thresholds adjacent to their previous ones, and update mean and frequency of the respective classes
      for (k=j+1; k<m_NumberOfThresholds; k++)
        {
        thresholdIndexes[k] = thresholdIndexes[k-1] + 1;
        classFrequency[k] = histogram->GetFrequency(thresholdIndexes[k]);
        classMean[k] = histogram->GetMeasurementVector(thresholdIndexes[k])[0] * classFrequency[k];
        }
      
      // update mean and frequency of the highest class
      classFrequency[numberOfClasses-1] = histogram->GetTotalFrequency();
      classMean[numberOfClasses-1] = globalMean * histogram->GetTotalFrequency();

      for(k=0; k<numberOfClasses-1; k++)
        {
        classFrequency[numberOfClasses-1] -= classFrequency[k];
        classMean[numberOfClasses-1] -= classMean[k] * classFrequency[k];
        }

      if (classFrequency[numberOfClasses-1] != 0.0)
        {
        classMean[numberOfClasses-1] /= classFrequency[numberOfClasses-1];
        }
      else
        {
        classMean[numberOfClasses-1] = 0.0;
        }

      // exit the for loop if a threshold has been incremented
      break;
      }
    else  // if this threshold can't be incremented
      {
      // if it's the lowest threshold
      if (j==0)
        {
        // we couldn't increment because we're done
        return false;
        }
      }
    }
  // we incremented
  return true;
}

/*
 * Compute Otsu's thresholds
 */                    
template<class TInputHistogram>
void
OtsuMultipleThresholdsCalculator<TInputHistogram>
::GenerateData()
{
  typename TInputHistogram::ConstPointer histogram = this->GetInputHistogram();

  // TODO: as an improvement, the class could accept multi-dimensional histograms
  // and the user could specify the dimension to apply the algorithm to.
  if (histogram->GetSize().GetSizeDimension() != 1)
    {
    itkExceptionMacro(<<"Histogram must be 1-dimensional.");
    }

  // compute global mean
  typename TInputHistogram::ConstIterator iter = histogram->Begin() ;
  typename TInputHistogram::ConstIterator end = histogram->End() ;
  double globalMean = 0.0;
  double globalFrequency = static_cast<double>(histogram->GetTotalFrequency());
  while (iter != end)
    {
    globalMean += iter.GetMeasurementVector()[0] * iter.GetFrequency();
    ++iter ;
    }
  globalMean /= globalFrequency ;

  unsigned long numberOfClasses = m_NumberOfThresholds + 1;

  // initialize thresholds
  InstanceIdentifierVectorType thresholdIndexes(m_NumberOfThresholds);

  unsigned long j;
  for(j=0; j<m_NumberOfThresholds; j++)
    {
    thresholdIndexes[j] = j+1;
    }

  InstanceIdentifierVectorType maxVarThresholdIndexes = thresholdIndexes;

  // compute frequency and mean of initial classes
  double sum = 0.0;
  FrequencyVectorType classFrequency(numberOfClasses);
  for (j=0; j<numberOfClasses-1; j++)
    {
    classFrequency[j] = histogram->GetFrequency(thresholdIndexes[j]);
    sum += classFrequency[j];
    }
  classFrequency[numberOfClasses-1] = globalFrequency - sum;
  
  sum = 0.0;
  MeanVectorType classMean(numberOfClasses);
  for (j=0; j < numberOfClasses-1; j++)
    {
    classMean[j] = histogram->GetMeasurementVector(j)[0] * classFrequency[j];
    sum += classMean[j] * classFrequency[j];
    }

  if (classFrequency[numberOfClasses-1] != 0.0)
    {
    classMean[numberOfClasses-1] = (globalMean * globalFrequency - sum) / classFrequency[numberOfClasses-1];
    }
  else
    {
    classMean[numberOfClasses-1] = 0.0;
    }
  
  double maxVarBetween = 0.0;
  for (j=0; j<numberOfClasses; j++)
    {
    maxVarBetween += classFrequency[j] * (globalMean - classMean[j]) * (globalMean - classMean[j]);
    }

  // explore all possible threshold configurations and choose the one that yields maximum between-class variance
  while (Self::IncrementThresholds(thresholdIndexes, globalMean, classMean, classFrequency))
    {
    double varBetween = 0.0;
    for (j=0; j<numberOfClasses; j++)
      {
      varBetween += classFrequency[j] * (globalMean - classMean[j]) * (globalMean - classMean[j]);
      }

    if (varBetween > maxVarBetween)
      {
      maxVarBetween = varBetween;
      maxVarThresholdIndexes = thresholdIndexes;
      }
    }

  // copy corresponding bin max to threshold vector
  m_Output.resize(m_NumberOfThresholds);

  for (j=0; j<m_NumberOfThresholds; j++)
    {
    m_Output[j] = histogram->GetBinMax(0,maxVarThresholdIndexes[j]);
    }
}

template<class TInputHistogram>
void
OtsuMultipleThresholdsCalculator<TInputHistogram>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "NumberOfThresholds: " << m_NumberOfThresholds;

  os << indent << "Output: ";
  for (unsigned long j=0; j<m_NumberOfThresholds; j++)
    {
    os << m_Output[j] << " ";
    }
  os << std::endl;
}

} // end namespace Statistics
} // end namespace itk

#endif
