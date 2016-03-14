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
#ifndef itkStochasticFractalDimensionImageFilter_hxx
#define itkStochasticFractalDimensionImageFilter_hxx

#include "itkStochasticFractalDimensionImageFilter.h"

#include "itkNeighborhoodAlgorithm.h"
#include "itkProgressReporter.h"

#include <vector>

namespace itk
{
template< typename TInputImage, typename TMaskImage, typename TOutputImage >
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::StochasticFractalDimensionImageFilter()
{
  this->m_NeighborhoodRadius.Fill(2);

  this->m_MaskImage = ITK_NULLPTR;
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage >
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::~StochasticFractalDimensionImageFilter()
{}

template< typename TInputImage, typename TMaskImage, typename TOutputImage >
void
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::SetMaskImage(const MaskImageType *mask)
{
  this->SetNthInput( 1, const_cast< MaskImageType * >( mask ) );
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage >
const typename StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >::MaskImageType *
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::GetMaskImage() const
{
  const MaskImageType *maskImage =
    dynamic_cast< const MaskImageType * >( this->ProcessObject::GetInput(1) );

  return maskImage;
}

template< typename TInputImage, typename TMaskImage, typename TOutputImage >
void
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::GenerateData()
{
  this->AllocateOutputs();

  typedef typename InputImageType::PixelType  InputPixelType;
  typedef typename OutputImageType::PixelType OutputPixelType;
  typedef typename InputImageType::PointType  PointType;

  const InputImageType *inputImage = this->GetInput();
  OutputImageType      *outputImage = this->GetOutput();
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
      this->m_NeighborhoodRadius, inputImage, *fit);

    NeighborhoodIterator< OutputImageType > ItO(
      this->m_NeighborhoodRadius, outputImage, *fit);

    for ( It.GoToBegin(), ItO.GoToBegin(); !It.IsAtEnd(); ++It, ++ItO )
      {
      if ( this->m_MaskImage && !this->m_MaskImage->GetPixel( It.GetIndex() ) )
        {
        ItO.SetCenterPixel(NumericTraits< OutputPixelType >::ZeroValue());
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
          inputImage->TransformIndexToPhysicalPoint(It.GetIndex(i), point1);

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
              inputImage->TransformIndexToPhysicalPoint(It.GetIndex(j), point2);

              const RealType distance = point1.SquaredEuclideanDistanceTo(point2);

              bool distanceFound = false;
              for ( unsigned int k = 0; k < distances.size(); k++ )
                {
                if ( itk::Math::abs(distances[k] - distance) < 0.5 * minSpacing )
                  {
                  distancesFrequency[k]++;
                  averageAbsoluteIntensityDifference[k] += itk::Math::abs(pixel1 - pixel2);
                  distanceFound = true;
                  break;
                  }
                }

              if ( !distanceFound )
                {
                distances.push_back(distance);
                distancesFrequency.push_back(1);
                averageAbsoluteIntensityDifference.push_back( itk::Math::abs(pixel1 - pixel2) );
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
        averageAbsoluteIntensityDifference[k] = std::log(averageAbsoluteIntensityDifference[k]);

        const RealType distance = std::log( std::sqrt(distances[k]) );

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

template< typename TInputImage, typename TMaskImage, typename TOutputImage >
void
StochasticFractalDimensionImageFilter< TInputImage, TMaskImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Neighborhood radius: " << this->m_NeighborhoodRadius << std::endl;
}
} // end namespace itk

#endif
