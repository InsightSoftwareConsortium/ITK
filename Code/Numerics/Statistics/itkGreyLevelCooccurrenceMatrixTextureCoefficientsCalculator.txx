/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculator_txx
#define _itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculator_txx

#include "itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculator.h"

#include "itkNumericTraits.h"
#include "vnl/vnl_math.h"

namespace itk {
  namespace Statistics {
    
    template< class THistogram >
    void
    GreyLevelCooccurrenceMatrixTextureCoefficientsCalculator< THistogram >::
    Compute( void )
      {
      typedef typename HistogramType::Iterator HistogramIterator;
      
      // First, normalize the histogram if it doesn't look normalized.
      // This is one pass through the histogram.
      FrequencyType totalFrequency = m_Histogram->GetTotalFrequency();
      if ( (totalFrequency - NumericTraits<MeasurementType>::One) > 0.0001 )
        {
        // Doesn't look normalized:
        this->NormalizeHistogram();
        }
      
      // Now get the various means and variances. This is takes two passes
      // through the histogram.
      double pixelMean, marginalMean, marginalDevSquared, pixelVariance;
      this->ComputeMeansAndVariances(pixelMean, marginalMean, marginalDevSquared,
                                     pixelVariance);
      
            
      // Finally compute the texture features. Another one pass.
      m_Energy = m_Entropy = m_Correlation = m_InverseDifferenceMoment =
        m_Inertia = m_ClusterShade = m_ClusterProminence = m_HaralickCorrelation = 0;
      
      double pixelVarianceSquared = pixelVariance * pixelVariance;
      double log2 = vcl_log(2.);
      for (HistogramIterator hit = m_Histogram->Begin();
           hit != m_Histogram->End(); ++hit)
        {
        MeasurementType frequency = hit.GetFrequency();
        if (frequency == 0)
          {
          continue; // no use doing these calculations if we're just multiplying by zero.
          }
        
        IndexType index = m_Histogram->GetIndex(hit.GetInstanceIdentifier());
        m_Energy += frequency * frequency;
        m_Entropy -= (frequency > 0.0001) ? frequency * vcl_log(frequency) / log2 : 0;
        m_Correlation += ( (index[0] - pixelMean) * (index[1] - pixelMean) * frequency)
          / pixelVarianceSquared;
        m_InverseDifferenceMoment += frequency /
          (1.0 + (index[0] - index[1]) * (index[0] - index[1]) );
        m_Inertia += (index[0] - index[1]) * (index[0] - index[1]) * frequency;
        m_ClusterShade += vcl_pow((index[0] - pixelMean) + (index[1] - pixelMean), 3) *
          frequency;
        m_ClusterProminence += vcl_pow((index[0] - pixelMean) + (index[1] - pixelMean), 4) *
          frequency;
        m_HaralickCorrelation += index[0] * index[1] * frequency;
        }
      
      m_HaralickCorrelation = (m_HaralickCorrelation - marginalMean * marginalMean) /
        marginalDevSquared;
      }
  

    template< class THistogram >
    void
    GreyLevelCooccurrenceMatrixTextureCoefficientsCalculator< THistogram >::
    NormalizeHistogram( void )
      {
        typename HistogramType::Iterator hit;
        typename HistogramType::FrequencyType totalFrequency = 
          m_Histogram->GetTotalFrequency();
        
        for (hit = m_Histogram->Begin(); hit != m_Histogram->End(); ++hit)
          {
          hit.SetFrequency(hit.GetFrequency() / totalFrequency);
          }
      }
      
    template< class THistogram >
    void
    GreyLevelCooccurrenceMatrixTextureCoefficientsCalculator< THistogram >::
    ComputeMeansAndVariances( double &pixelMean, double &marginalMean, 
                              double &marginalDevSquared, double &pixelVariance )
      {
      // This function takes two passes through the histogram and two passes through
      // an array of the same length as a histogram axis. This could probably be
      // cleverly compressed to one pass, but it's not clear that that's necessary.
      
      typedef typename HistogramType::Iterator HistogramIterator;
      
      // Initialize everything
      typename HistogramType::SizeValueType binsPerAxis = m_Histogram->GetSize(0);
      double *marginalSums = new double[binsPerAxis];
      for (double *ms_It = marginalSums; 
           ms_It < marginalSums + binsPerAxis; ms_It++)
        {
        *ms_It = 0;
        }
      pixelMean = 0;
      
      // Ok, now do the first pass through the histogram to get the marginal sums
      // and compute the pixel mean
      HistogramIterator hit;
      for (hit = m_Histogram->Begin(); hit != m_Histogram->End(); ++hit)
        {
        MeasurementType frequency = hit.GetFrequency();
        IndexType index = m_Histogram->GetIndex(hit.GetInstanceIdentifier());
        pixelMean += index[0] * frequency;
        marginalSums[index[0]] += frequency;
        }
      
      /*  Now get the mean and deviaton of the marginal sums.
          Compute incremental mean and SD, a la Knuth, "The  Art of Computer 
          Programming, Volume 2: Seminumerical Algorithms",  section 4.2.2. 
          Compute mean and standard deviation using the recurrence relation:
          M(1) = x(1), M(k) = M(k-1) + (x(k) - M(k-1) ) / k
          S(1) = 0, S(k) = S(k-1) + (x(k) - M(k-1)) * (x(k) - M(k))
          for 2 <= k <= n, then
          sigma = vcl_sqrt(S(n) / n) (or divide by n-1 for sample SD instead of
          population SD).
      */
      marginalMean = marginalSums[0];
      marginalDevSquared = 0;
      for (unsigned int arrayIndex = 1 ; arrayIndex < binsPerAxis; arrayIndex++)
        {
        int k = arrayIndex + 1;
        double M_k_minus_1 = marginalMean;
        double S_k_minus_1 = marginalDevSquared;
        double x_k = marginalSums[arrayIndex];
        
        double M_k = M_k_minus_1 + (x_k - M_k_minus_1) / k;
        double S_k = S_k_minus_1 + (x_k - M_k_minus_1) * (x_k - M_k);
        
        marginalMean = M_k;
        marginalDevSquared = S_k;
        }
      marginalDevSquared = marginalDevSquared / binsPerAxis;
      
      // OK, now compute the pixel variances.
      pixelVariance = 0;
      for (hit = m_Histogram->Begin(); hit != m_Histogram->End(); ++hit)
        {
        MeasurementType frequency = hit.GetFrequency();
        IndexType index = m_Histogram->GetIndex(hit.GetInstanceIdentifier());
        pixelVariance += (index[0] - pixelMean) * (index[0] - pixelMean) * frequency;
        }      
      delete [] marginalSums;
      }
    
    template< class THistogram >
    double
    GreyLevelCooccurrenceMatrixTextureCoefficientsCalculator< THistogram >::
    GetFeature(TextureFeatureName feature)
      {
      switch(feature)
        {
        case Energy:
          return this->GetEnergy();
        case Entropy:
          return this->GetEntropy();
        case Correlation:
          return this->GetCorrelation();
        case InverseDifferenceMoment:
          return this->GetInverseDifferenceMoment();
        case Inertia:
          return this->GetInertia();
        case ClusterShade:
          return this->GetClusterShade();
        case ClusterProminence:
          return this->GetClusterProminence();
        case HaralickCorrelation:
          return this->GetHaralickCorrelation();
        default:
          return 0;
        }
      }
      
    template< class THistogram >
    void
    GreyLevelCooccurrenceMatrixTextureCoefficientsCalculator< THistogram >::    
    PrintSelf(std::ostream& os, Indent indent) const
      {
      Superclass::PrintSelf(os,indent);
      }
    
  } // end of namespace Statistics 
} // end of namespace itk 


#endif
