/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageToGreyLevelCooccurrenceMatrixGenerator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkScalarImageToGreyLevelCooccurrenceMatrixGenerator_txx
#define _itkScalarImageToGreyLevelCooccurrenceMatrixGenerator_txx

#include "itkScalarImageToGreyLevelCooccurrenceMatrixGenerator.h"

#include "itkConstNeighborhoodIterator.h"
#include "vnl/vnl_math.h"


namespace itk {
  namespace Statistics {
    
    template< class TImageType, class THistogramFrequencyContainer >
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator() : 
    m_NumberOfBinsPerAxis(itkGetStaticConstMacro(DefaultBinsPerAxis)), m_Normalize(false)
      {
      m_LowerBound.Fill(NumericTraits<PixelType>::min());
      m_UpperBound.Fill(NumericTraits<PixelType>::max() + 1);
      m_Min = NumericTraits<PixelType>::min();
      m_Max = NumericTraits<PixelType>::max();
      }
    
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    Compute( void )
      {
      // First, create an appropriate histogram with the right number of bins
      // and mins and maxes correct for the image type.
      m_Output = HistogramType::New();
      typename HistogramType::SizeType size;
      size.Fill(m_NumberOfBinsPerAxis);
      m_Output->Initialize(size, m_LowerBound, m_UpperBound);
      
      // Next, find the minimum radius that encloses all the offsets.
      unsigned int minRadius = 0;
      typename OffsetVector::ConstIterator offsets;
      for(offsets = m_Offsets->Begin(); offsets != m_Offsets->End(); offsets++)
        {
        for(unsigned int i = 0; i < offsets.Value().GetOffsetDimension(); i++)
          {
          unsigned int distance = vnl_math_abs(offsets.Value()[i]);
          if(distance > minRadius)
            {
            minRadius = distance;
            }          
          }
        }
      
      RadiusType radius;
      radius.Fill(minRadius);
      
      // Now fill in the histogram
      this->FillHistogram(radius, m_Input->GetRequestedRegion());
      
      // Normalizse the histogram if requested
      if(m_Normalize)
        {
        this->NormalizeHistogram();
        }
      
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    FillHistogram(RadiusType radius, RegionType region)
      {
      // Iterate over all of those pixels and offsets, adding each 
      // co-occurrence pair to the histogram
      
      typedef ConstNeighborhoodIterator<ImageType> NeighborhoodIteratorType;
      NeighborhoodIteratorType neighborIt;
      neighborIt = NeighborhoodIteratorType(radius, m_Input, region);

      for (neighborIt.GoToBegin(); !neighborIt.IsAtEnd(); ++neighborIt) 
        {
        const PixelType centerPixelIntensity = neighborIt.GetCenterPixel();
        if (centerPixelIntensity < m_Min || 
            centerPixelIntensity > m_Max)
          {
          continue; // don't put a pixel in the histogram if the value
                    // is out-of-bounds.
          }
        
        typename OffsetVector::ConstIterator offsets;
        for(offsets = m_Offsets->Begin(); offsets != m_Offsets->End(); offsets++)
          {
          bool pixelInBounds;
          const PixelType pixelIntensity = 
            neighborIt.GetPixel(offsets.Value(), pixelInBounds);
          
          if (!pixelInBounds)
            {
            continue; // don't put a pixel in the histogram if it's out-of-bounds.
            }
          
          if (pixelIntensity < m_Min || 
              pixelIntensity > m_Max)
            {
            continue; // don't put a pixel in the histogram if the value
                      // is out-of-bounds.
            }
          
          // Now make both possible co-occurrence combinations and increment the
          // histogram with them.
          MeasurementVectorType cooccur;
          cooccur[0] = centerPixelIntensity;
          cooccur[1] = pixelIntensity;
          m_Output->IncreaseFrequency(cooccur, 1);
          cooccur[1] = centerPixelIntensity;
          cooccur[0] = pixelIntensity;
          m_Output->IncreaseFrequency(cooccur, 1);
          }
        }
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    NormalizeHistogram( void )
      {
      typename HistogramType::Iterator hit;
      typename HistogramType::FrequencyType totalFrequency = 
        m_Output->GetTotalFrequency();
      
      for (hit = m_Output->Begin(); hit != m_Output->End(); ++hit)
        {
        hit.SetFrequency(hit.GetFrequency() / totalFrequency);
        }
      }
        
    template< class TImageType, class THistogramFrequencyContainer >
    void
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    SetPixelValueMinMax( PixelType min, PixelType max )
      {
      itkDebugMacro("setting Min to " << min << "and Max to " << max);
      m_Min = min;
      m_Max = max;
      m_LowerBound.Fill(min);
      m_UpperBound.Fill(max + 1);
      this->Modified();
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    PrintSelf(std::ostream& os, Indent indent) const
      {
      Superclass::PrintSelf(os,indent);
      }
    
  } // end of namespace Statistics 
} // end of namespace itk 


#endif
