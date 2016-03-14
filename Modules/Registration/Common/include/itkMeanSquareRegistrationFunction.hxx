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
#ifndef itkMeanSquareRegistrationFunction_hxx
#define itkMeanSquareRegistrationFunction_hxx

#include "itkMeanSquareRegistrationFunction.h"
#include "itkMacro.h"
#include "itkMath.h"
#include "itkMath.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
MeanSquareRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::MeanSquareRegistrationFunction()
{
  RadiusType   r;
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 0;
    }
  this->SetRadius(r);

  this->SetEnergy(0.0);
  m_TimeStep = 1.0;
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  this->SetMovingImage(ITK_NULLPTR);
  this->SetFixedImage(ITK_NULLPTR);
  m_FixedImageGradientCalculator = GradientCalculatorType::New();

  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = itkDynamicCastInDebugMode< InterpolatorType * >(interp.GetPointer() );
}

/*
 * Standard "PrintSelf" method.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
MeanSquareRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
/*
  os << indent << "MovingImageIterpolator: ";
  os << m_MovingImageInterpolator.GetPointer() << std::endl;
  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "DenominatorThreshold: ";
  os << m_DenominatorThreshold << std::endl;
  os << indent << "IntensityDifferenceThreshold: ";
  os << m_IntensityDifferenceThreshold << std::endl;
*/
}

/*
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
MeanSquareRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::InitializeIteration()
{
  if ( !this->GetMovingImage() || !this->GetFixedImage() || !m_MovingImageInterpolator )
    {
    itkExceptionMacro(<< "MovingImage, FixedImage and/or Interpolator not set");
    }

  // cache fixed image information
  m_FixedImageSpacing    = this->GetFixedImage()->GetSpacing();

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage( this->GetFixedImage() );

  // setup moving image interpolator
  m_MovingImageInterpolator->SetInputImage( this->GetMovingImage() );

  this->SetEnergy(0.0);
}

/**
 * Compute update at a non boundary neighbourhood
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
typename MeanSquareRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PixelType
MeanSquareRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::ComputeUpdate( const NeighborhoodType & it, void *itkNotUsed(globalData),
                 const FloatOffsetType & itkNotUsed(offset) )
{
  // Get fixed image related information
  // Note: no need to check the index is within
  // fixed image buffer. This is done by the external filter.
  const IndexType           index = it.GetIndex();
  const double              fixedValue = (double)this->GetFixedImage()->GetPixel(index);
  const CovariantVectorType fixedGradient = m_FixedImageGradientCalculator->EvaluateAtIndex(index);
  double                    fixedGradientSquaredMagnitude = 0;

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    fixedGradientSquaredMagnitude += itk::Math::sqr(fixedGradient[j]) * m_FixedImageSpacing[j];
    }

  // Get moving image related information
  const DisplacementFieldPixelType itvec = this->GetDisplacementField()->GetPixel(index);
  PointType                       mappedPoint;
  this->GetFixedImage()->TransformIndexToPhysicalPoint(index, mappedPoint);
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    mappedPoint[j] += itvec[j];
    }
  double movingValue = 0.0;
  if ( m_MovingImageInterpolator->IsInsideBuffer(mappedPoint) )
    {
    movingValue = m_MovingImageInterpolator->Evaluate(mappedPoint);
    }

  // Compute update
  const double speedValue = fixedValue - movingValue;
  this->m_Energy += speedValue * speedValue;

  const bool normalizemetric = this->GetNormalizeGradient();
  double     denominator = 1.0;
  if ( normalizemetric )
    {
    denominator = speedValue * speedValue * fixedGradientSquaredMagnitude;
    denominator = std::sqrt(denominator);
    }
  if ( Math::AlmostEquals( denominator, 0.0 ) )
    {
    denominator = 1.0;
    }
  PixelType update;
  if ( itk::Math::abs(speedValue) < m_IntensityDifferenceThreshold
       || denominator < m_DenominatorThreshold )
    {
    update.Fill(0.0);
    return update;
    }

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    update[j] = speedValue * fixedGradient[j] * itk::Math::sqr(m_FixedImageSpacing[j])
                / denominator * this->m_GradientStep;
    }
  return update;
}
} // end namespace itk

#endif
