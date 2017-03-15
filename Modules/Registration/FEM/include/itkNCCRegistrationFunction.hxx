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
#ifndef itkNCCRegistrationFunction_hxx
#define itkNCCRegistrationFunction_hxx

#include "itkNCCRegistrationFunction.h"
#include "itkMacro.h"
#include "itkNeighborhoodIterator.h"
#include "itkMath.h"

namespace itk
{

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
NCCRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::NCCRegistrationFunction() :
  m_TimeStep( 1.0 ),
  m_FixedImageGradientCalculator( GradientCalculatorType::New() ),
  m_DenominatorThreshold( 1e-9 ),
  m_IntensityDifferenceThreshold( 0.001 ),
  m_MetricTotal( 0.0 )
{
  m_FixedImageSpacing.Fill( 1 );

  RadiusType r;

  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    r[j] = 1;
    }
  this->SetRadius(r);

  this->SetMovingImage(ITK_NULLPTR);
  this->SetFixedImage(ITK_NULLPTR);

  m_FixedImageSpacing.Fill(1.0);

  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = static_cast< InterpolatorType * >(
    interp.GetPointer() );
}

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
NCCRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::InitializeIteration()
{
  if( !this->m_MovingImage || !this->m_FixedImage || !m_MovingImageInterpolator )
    {
    itkExceptionMacro(<< "MovingImage, FixedImage and/or Interpolator not set");
    }

  // Cache fixed image information
  m_FixedImageSpacing = this->m_FixedImage->GetSpacing();

  // Set up gradient calculator
  m_FixedImageGradientCalculator->SetInputImage(this->m_FixedImage);

  // Set up moving image interpolator
  m_MovingImageInterpolator->SetInputImage(this->m_MovingImage);

  std::cout << " Total metric: " << m_MetricTotal << "; field size: "
            << this->GetDisplacementField()->GetLargestPossibleRegion().GetSize() << "; image size: "
            << this->m_FixedImage->GetLargestPossibleRegion().GetSize() << std::endl;
  m_MetricTotal = 0.0;
}

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
typename NCCRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PixelType
NCCRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::ComputeUpdate( const NeighborhoodType & it, void *itkNotUsed(globalData),
                 const FloatOffsetType & itkNotUsed(offset) )
{
  const IndexType oindex = it.GetIndex();

  const typename FixedImageType::SizeType hradius = it.GetRadius();
  FixedImageType *img = const_cast< FixedImageType * >( this->m_FixedImage.GetPointer() );
  const typename FixedImageType::SizeType imagesize = img->GetLargestPossibleRegion().GetSize();

  NeighborhoodIterator< FixedImageType >
  hoodIt( hradius, img, img->GetRequestedRegion() );
  hoodIt.SetLocation(oindex);

  double sff = 0.0;
  double smm = 0.0;
  double sfm = 0.0;

  double derivativeF[ImageDimension];
  double derivativeM[ImageDimension];
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    derivativeF[j] = 0;
    derivativeM[j] = 0;
    }

  unsigned int hoodlen = hoodIt.Size();
  for( unsigned int indct = 0; indct < hoodlen - 1; indct++ )
    {
    const IndexType index = hoodIt.GetIndex(indct);
    bool inimage = true;
    for( unsigned int dd = 0; dd < ImageDimension; dd++ )
      {
      if( index[dd] < 0 || index[dd] >
           static_cast< typename IndexType::IndexValueType >( imagesize[dd] - 1 ) )
        {
        inimage = false;
        }
      }
    if( inimage )
      {
      // Get fixed image related information.
      // Note: no need to check the index is within fixed image buffer.
      // This is done by the external filter.
      const double fixedValue = (double)this->m_FixedImage->GetPixel(index);
      const CovariantVectorType fixedGradient =
        m_FixedImageGradientCalculator->EvaluateAtIndex(index);
      double fixedGradientSquaredMagnitude = 0;
      for( unsigned int j = 0; j < ImageDimension; j++ )
        {
        fixedGradientSquaredMagnitude += itk::Math::sqr(fixedGradient[j]) * m_FixedImageSpacing[j];
        }

      // Get moving image related information
      typedef typename TDisplacementField::PixelType DeformationPixelType;
      const DeformationPixelType vec = this->GetDisplacementField()->GetPixel(index);
      PointType mappedPoint;
      this->GetFixedImage()->TransformIndexToPhysicalPoint(index, mappedPoint);
      for( unsigned int j = 0; j < ImageDimension; j++ )
        {
        mappedPoint[j] += vec[j];
        }
      double movingValue = 0.0;
      if( m_MovingImageInterpolator->IsInsideBuffer(mappedPoint) )
        {
        movingValue = m_MovingImageInterpolator->Evaluate(mappedPoint);
        }

      sff += fixedValue * fixedValue;
      smm += movingValue * movingValue;
      sfm += fixedValue * movingValue;

      for( unsigned int dim = 0; dim < ImageDimension; dim++ )
        {
        const double differential = fixedGradient[dim];
        derivativeF[dim] += fixedValue  * differential;
        derivativeM[dim] += movingValue * differential;
        }
      }
    }

  PixelType update;
  update.Fill(0.0);
  double updatenorm = 0.0;
  if( ( sff * smm ) != 0.0 )
    {
    const double factor = 1.0 / std::sqrt(sff * smm);
    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      update[i] = factor * ( derivativeF[i] - ( sfm / smm ) * derivativeM[i] );
      updatenorm += ( update[i] * update[i] );
      }
    updatenorm = std::sqrt(updatenorm);
    m_MetricTotal += sfm * factor;
    this->m_Energy += sfm * factor;
    }
  else
    {
    update.Fill(0.0);
    updatenorm = 1.0;
    }

  if( this->GetNormalizeGradient() && updatenorm != 0.0 )
    {
    update /= updatenorm;
    }
  return update * this->m_GradientStep;
}

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
NCCRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "TimeStep: " << m_TimeStep << std::endl;
  os << indent << "FixedImageSpacing: "
    << static_cast< typename itk::NumericTraits< SpacingType >::PrintType >( m_FixedImageSpacing )
    << std::endl;

  itkPrintSelfObjectMacro( FixedImageGradientCalculator );
  itkPrintSelfObjectMacro( MovingImageInterpolator );

  os << indent << "DenominatorThreshold: " << m_DenominatorThreshold << std::endl;
  os << indent << "IntensityDifferenceThreshold: " << m_IntensityDifferenceThreshold
    << std::endl;
  os << indent << "MetricTotal: " << m_MetricTotal << std::endl;
}
} // end namespace itk

#endif
