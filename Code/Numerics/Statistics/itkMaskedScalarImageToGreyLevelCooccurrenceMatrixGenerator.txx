#ifndef _itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator_txx
#define _itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator_txx

#include "itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator.h"

#include "itkNumericTraits.h"
#include "itkImageRegionConstIterator.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"

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
      typedef ImageRegionConstIterator<ImageType>
      RegionIteratorType;
      
      typedef ConstShapedNeighborhoodIterator<ImageType>
        ShapedNeighborhoodIteratorType;
      
      
      ShapedNeighborhoodIteratorType shapedIt, shapedMaskIt;
      RegionIteratorType regionIt, regionMaskIt;
      
      shapedIt = ShapedNeighborhoodIteratorType(radius, m_Image, region);
      regionIt = RegionIteratorType(m_Image, region);
      
      shapedMaskIt = ShapedNeighborhoodIteratorType(radius, m_ImageMask, region);
      regionMaskIt = RegionIteratorType(m_ImageMask, region);
      
      MeasurementVectorType cooccur;
      typename OffsetVector::ConstIterator offsets;
      for(offsets = m_Offsets->Begin(); offsets != m_Offsets->End(); offsets++)
        {
        shapedIt.ActivateOffset(offsets.Value());
        shapedMaskIt.ActivateOffset(offsets.Value());
        }
      
      for (shapedIt.GoToBegin(), regionIt.GoToBegin(), shapedMaskIt.GoToBegin(), 
           regionMaskIt.GoToBegin(); !shapedIt.IsAtEnd(); 
           ++shapedIt, ++regionIt, ++shapedMaskIt, ++regionMaskIt) 
        {

        if (regionMaskIt.Get() != m_InsidePixelValue)
          {
          continue; // Go to the next loop if we're not in the mask
          }
        const PixelType center_pixel_intensity = regionIt.Get();
        
        typename ShapedNeighborhoodIteratorType::ConstIterator neighborhoodIt,
          neighborhoodMaskIt;
        for (neighborhoodIt = shapedIt.Begin(), neighborhoodMaskIt = shapedMaskIt.Begin();
             !neighborhoodIt.IsAtEnd(); ++neighborhoodIt, ++neighborhoodMaskIt)
          {

          if (neighborhoodMaskIt.Get() != m_InsidePixelValue)
            {
            continue; // Go to the next loop if we're not in the mask.
            }
          const PixelType pixel_intensity = neighborhoodIt.Get();
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
    MaskedScalarImageToGreyLevelCooccurrenceMatrixGenerator< TImageType,
    THistogramFrequencyContainer >::
    PrintSelf(std::ostream& os, Indent indent) const
      {
      Superclass::PrintSelf(os,indent);
      }
    
  } // end of namespace Statistics 
} // end of namespace itk 


#endif
