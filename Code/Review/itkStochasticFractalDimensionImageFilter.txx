/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStochasticFractalDimensionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStochasticFractalDimensionImageFilter_txx
#define __itkStochasticFractalDimensionImageFilter_txx

#include "itkStochasticFractalDimensionImageFilter.h"

#include "itkNeighborhoodAlgorithm.h"
#include "itkProgressReporter.h"

#include <vector>

namespace itk
{
template< class TInputImage, class TMaskImage, class TOutputImage >
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::StochasticFractalDimensionImageFilter()
{
  this->m_NeighborhoodRadius.Fill(2);

  this->m_MaskImage = NULL;
}

template< class TInputImage, class TMaskImage, class TOutputImage >
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::~StochasticFractalDimensionImageFilter()
{}

template< class TInputImage, class TMaskImage, class TOutputImage >
void
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::SetMaskImage(const MaskImageType *mask)
{
  this->SetNthInput( 1, const_cast< MaskImageType * >( mask ) );
}

template< class TInputImage, class TMaskImage, class TOutputImage >
const typename StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >::MaskImageType *
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::GetMaskImage() const
{
  const MaskImageType *maskImage =
    dynamic_cast< const MaskImageType * >( this->ProcessObject::GetInput(1) );

  return maskImage;
}

template< class TInputImage, class TMaskImage, class TOutputImage >
void
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::GenerateData()
{
  this->AllocateOutputs();

  typedef typename InputImageType::PixelType  InputPixelType;
  typedef typename OutputImageType::PixelType OutputPixelType;
  typedef typename InputImageType::PointType  PointType;

  const InputImageType *inputImage = this->GetInput();

  typename InputImageType::RegionType region = inputImage->GetRequestedRegion();

  ProgressReporter progress(this, 0, region.GetNumberOfPixels(), 100);

  typedef typename NeighborhoodAlgorithm
  ::ImageBoundaryFacesCalculator< InputImageType > FaceCalculatorType;
  FaceCalculatorType faceCalculator;

  typename FaceCalculatorType::FaceListType faceList =
    faceCalculator(inputImage, region, this->m_NeighborhoodRadius);

  typename FaceCalculatorType::FaceListType::iterator fit;

  typename InputImageType::SpacingType spacing = inputImage->GetSpacing();

  RealType minSpacing = spacing[0];

  for ( unsigned int d = 0; d < ImageDimension; d++ )
    {
    if ( spacing[d] < minSpacing )
      {
      minSpacing = spacing[d];
      }
    }

  std::vector< RealType > distances;
  std::vector< RealType > distancesFrequency;
  std::vector< RealType > averageAbsoluteIntensityDifference;

  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    ConstNeighborhoodIteratorType It(
      this->m_NeighborhoodRadius, this->GetInput(), *fit);

    NeighborhoodIterator< OutputImageType > ItO(
      this->m_NeighborhoodRadius, this->GetOutput(), *fit);

    for ( It.GoToBegin(), ItO.GoToBegin(); !It.IsAtEnd(); ++It, ++ItO )
      {
      if ( this->m_MaskImage && !this->m_MaskImage->GetPixel( It.GetIndex() ) )
        {
        ItO.SetCenterPixel(NumericTraits< OutputPixelType >::Zero);
        progress.CompletedPixel();
        continue;
        }

      distances.clear();
      distancesFrequency.clear();
      averageAbsoluteIntensityDifference.clear();

      for ( unsigned int i = 0; i < It.GetNeighborhood().Size(); i++ )
        {
        bool           IsInBounds1;
        InputPixelType pixel1 = It.GetPixel(i, IsInBounds1);

        if ( !IsInBounds1 )
          {
          continue;
          }

        if ( !this->m_MaskImage || this->m_MaskImage->GetPixel( It.GetIndex(i) ) )
          {
          PointType point1;
          this->GetInput()->TransformIndexToPhysicalPoint(It.GetIndex(i), point1);

          for ( unsigned int j = 0; j < It.GetNeighborhood().Size(); j++ )
            {
            if ( i == j )
              {
              continue;
              }

            bool           IsInBounds2;
            InputPixelType pixel2 = It.GetPixel(j, IsInBounds2);

            if ( !IsInBounds2 )
              {
              continue;
              }

            if ( !this->m_MaskImage || this->m_MaskImage->GetPixel( It.GetIndex(j) ) )
              {
              PointType point2;
              this->GetInput()->TransformIndexToPhysicalPoint(It.GetIndex(j), point2);

              const RealType distance = point1.SquaredEuclideanDistanceTo(point2);

              bool distanceFound = false;
              for ( unsigned int k = 0; k < distances.size(); k++ )
                {
                if ( vnl_math_abs(distances[k] - distance) < 0.5 * minSpacing )
                  {
                  distancesFrequency[k]++;
                  averageAbsoluteIntensityDifference[k] += vnl_math_abs(pixel1 - pixel2);
                  distanceFound = true;
                  break;
                  }
                }

              if ( !distanceFound )
                {
                distances.push_back(distance);
                distancesFrequency.push_back(1);
                averageAbsoluteIntensityDifference.push_back( vnl_math_abs(pixel1 - pixel2) );
                }
              }
            }
          }
        }

      RealType sumY = 0.0;
      RealType sumX = 0.0;
      RealType sumXY = 0.0;
      RealType sumXX = 0.0;

      for ( unsigned int k = 0; k < distances.size(); k++ )
        {
        if ( distancesFrequency[k] == 0 )
          {
          continue;
          }

        averageAbsoluteIntensityDifference[k] /= static_cast< RealType >( distancesFrequency[k] );
        averageAbsoluteIntensityDifference[k] = vcl_log(averageAbsoluteIntensityDifference[k]);

        const RealType distance = vcl_log( vcl_sqrt(distances[k]) );

        sumY += averageAbsoluteIntensityDifference[k];
        sumX += distance;
        sumXX += ( distance * distance );
        sumXY += ( averageAbsoluteIntensityDifference[k] * distance );
        }

      const RealType N = static_cast< RealType >( distances.size() );

      const RealType slope = ( N * sumXY - sumX * sumY ) / ( N * sumXX - sumX * sumX );

      ItO.SetCenterPixel( static_cast< OutputPixelType >( 3.0 - slope ) );

      progress.CompletedPixel();
      }
    }
}

template< class TInputImage, class TMaskImage, class TOutputImage >
void
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Neighborhood radius: " << this->m_NeighborhoodRadius << std::endl;
}
} // end namespace itk

#endif
