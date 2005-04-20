/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator_txx
#define _itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator_txx

#include "itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator.h"

#include "itkNumericTraits.h"
#include "itkImageRegionConstIterator.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk {
  namespace Statistics {
    
    template< class TImageType, class THistogramFrequencyContainer >
    MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator() : 
    m_ImageMask(NULL), m_InsidePixelValue(NumericTraits<PixelType>::One)
      {
      // Empty
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    FillHistogram(RadiusType radius, RegionType region)
      {
      if (m_ImageMask.IsNull())
        {
        // If there's no mask set, just use the (faster) superclass method
        Superclass::FillHistogram(radius, region);
        return;
        }
      
      // Iterate over all of those pixels and offsets, adding each 
      // co-occurrence pair to the histogram
      
      typedef ConstNeighborhoodIterator<ImageType> NeighborhoodIteratorType;
      NeighborhoodIteratorType neighborIt, maskNeighborIt;
      neighborIt = NeighborhoodIteratorType(radius, this->GetInput(), region);
      maskNeighborIt = NeighborhoodIteratorType(radius, m_ImageMask, region);
      
      for (neighborIt.GoToBegin(), maskNeighborIt.GoToBegin();
           !neighborIt.IsAtEnd(); ++neighborIt, ++maskNeighborIt) 
        {
        
        if (maskNeighborIt.GetCenterPixel() != m_InsidePixelValue)
          {
          continue; // Go to the next loop if we're not in the mask
          }
        
        const PixelType centerPixelIntensity = neighborIt.GetCenterPixel();
        if (centerPixelIntensity < this->GetMin() || 
            centerPixelIntensity > this->GetMax())
          {
          continue; // don't put a pixel in the histogram if the value
                    // is out-of-bounds.
          }
        
        typename OffsetVector::ConstIterator offsets;
        for(offsets = this->GetOffsets()->Begin(); offsets != this->GetOffsets()->End(); offsets++)
          {
          
          if (maskNeighborIt.GetPixel(offsets.Value()) != m_InsidePixelValue)
            {
            continue; // Go to the next loop if we're not in the mask
            }
          
          bool pixelInBounds;
          const PixelType pixelIntensity = 
            neighborIt.GetPixel(offsets.Value(), pixelInBounds);
          
          if (!pixelInBounds)
            {
            continue; // don't put a pixel in the histogram if it's out-of-bounds.
            }
          
          if (pixelIntensity < this->GetMin() || 
              pixelIntensity > this->GetMax())
            {
            continue; // don't put a pixel in the histogram if the value
                      // is out-of-bounds.
            }
          
          // Now make both possible co-occurrence combinations and increment the
          // histogram with them.
          MeasurementVectorType cooccur;
          cooccur[0] = centerPixelIntensity;
          cooccur[1] = pixelIntensity;
          this->GetOutput()->IncreaseFrequency(cooccur, 1);
          cooccur[1] = centerPixelIntensity;
          cooccur[0] = pixelIntensity;
          this->GetOutput()->IncreaseFrequency(cooccur, 1);
          }
        }
      }
    
    template< class TImageType, class THistogramFrequencyContainer >
    void
    MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    PrintSelf(std::ostream& os, Indent indent) const
      {
      Superclass::PrintSelf(os,indent);
      }
    
  } // end of namespace Statistics 
} // end of namespace itk 


#endif
