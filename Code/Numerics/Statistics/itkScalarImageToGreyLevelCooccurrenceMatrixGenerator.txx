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

#include "itkImageRegionConstIterator.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "vnl/vnl_math.h"


namespace itk {
  namespace Statistics {
    
    template< class TImageType, class THistogramFrequencyContainer >
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator() : 
    m_BinsPerAxis(itkGetStaticConstMacro(DefaultBinsPerAxis)), m_Normalize(false)
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
      m_Histogram = HistogramType::New();
      typename HistogramType::SizeType size;
      size.Fill(m_BinsPerAxis);
      m_Histogram->Initialize(size, m_LowerBound, m_UpperBound);
      
      // Next, find the minimum radius that encloses all the offsets.
      int minRadius = 0;
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
      
      // Next, find all the pixels that are not within one radius of the
      // edge of the buffered region.
      typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<ImageType>
        FaceCalculatorType;
     
      RadiusType radius;
      radius.Fill(minRadius);
      FaceCalculatorType faceCalculator;
      typename FaceCalculatorType::FaceListType faceList;
      faceList = faceCalculator(m_Image, m_Image->GetRequestedRegion(),
                                 radius);
      RegionType nonBoundaryRegion = faceList.front();
      
      // Now fill in the histogram
      this->FillHistogram(radius, nonBoundaryRegion);
      
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
      typedef ImageRegionConstIterator<ImageType>
      RegionIteratorType;
      
      typedef ConstShapedNeighborhoodIterator<ImageType>
        ShapedNeighborhoodIteratorType;
      
      ShapedNeighborhoodIteratorType shapedIt;
      RegionIteratorType regionIt;
      
      shapedIt = ShapedNeighborhoodIteratorType(radius, m_Image, region);
      regionIt = RegionIteratorType(m_Image, region);
      
      MeasurementVectorType cooccur;
      typename OffsetVector::ConstIterator offsets;
      for(offsets = m_Offsets->Begin(); offsets != m_Offsets->End(); offsets++)
        {
        shapedIt.ActivateOffset(offsets.Value());
        }
      
      for (shapedIt.GoToBegin(), regionIt.GoToBegin(); !shapedIt.IsAtEnd(); 
           ++shapedIt, ++regionIt) 
        {
        const PixelType center_pixel_intensity = regionIt.Get();
        if (center_pixel_intensity < m_Min || 
            center_pixel_intensity > m_Max)
          {
          continue; // don't put a pixel in the histogram if it's out-of-bounds.
          }
        
        typename ShapedNeighborhoodIteratorType::ConstIterator neighborhoodIt;
        for (neighborhoodIt = shapedIt.Begin(); !neighborhoodIt.IsAtEnd(); 
             ++neighborhoodIt)
          {
          const PixelType pixel_intensity = neighborhoodIt.Get();
          if (pixel_intensity < m_Min || 
              pixel_intensity > m_Max)
            {
            continue; // don't put a pixel in the histogram if it's out-of-bounds.
            }
          
          // Now make both possible co-occurrence combinations and increment the
          // histogram with them.
          cooccur[0] = center_pixel_intensity;
          cooccur[1] = pixel_intensity;
          m_Histogram->IncreaseFrequency(cooccur, 1);
          cooccur[1] = center_pixel_intensity;
          cooccur[0] = pixel_intensity;
          m_Histogram->IncreaseFrequency(cooccur, 1);
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
        m_Histogram->GetTotalFrequency();
      
      for (hit = m_Histogram->Begin(); hit != m_Histogram->End(); ++hit)
        {
        hit.SetFrequency(hit.GetFrequency() / totalFrequency);
        }
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
      SetInput( const ImagePointer inputImage )
      {
        m_Image = inputImage;
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    SetOffset( const OffsetType offset )
      {
      OffsetVectorPointer offsetVector = OffsetVector::New();
      offsetVector->push_back(offset);
      this->SetOffsets(offsetVector);
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    SetOffsets( const OffsetVectorPointer offsetsVector )
      {
      m_Offsets = offsetsVector;
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    typename ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    HistogramPointer
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    GetOutput(void) const
      {
      return m_Histogram;
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    SetNumberOfBinsPerAxis( unsigned int numberOfBins )
      {
      m_BinsPerAxis = numberOfBins;
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    ScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    SetPixelValueMinMax( PixelType min, PixelType max )
      {
      m_Min = min;
      m_Max = max;
      m_LowerBound.Fill(min);
      m_UpperBound.Fill(max + 1);
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
