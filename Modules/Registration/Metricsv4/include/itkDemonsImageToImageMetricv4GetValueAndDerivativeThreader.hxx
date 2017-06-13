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
#ifndef itkDemonsImageToImageMetricv4GetValueAndDerivativeThreader_hxx
#define itkDemonsImageToImageMetricv4GetValueAndDerivativeThreader_hxx

#include "itkDemonsImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{
template< typename TDomainPartitioner, typename TImageToImageMetric, typename TDemonsMetric >
void
DemonsImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TDemonsMetric >
::BeforeThreadedExecution()
{
  Superclass::BeforeThreadedExecution();

  /* Store the casted pointer to avoid dynamic casting in tight loops. */
  this->m_DemonsAssociate = dynamic_cast<TDemonsMetric*>( this->m_Associate );
  if( this->m_DemonsAssociate == ITK_NULLPTR )
    {
    itkExceptionMacro("Dynamic casting of associate pointer failed.");
    }
}

template< typename TDomainPartitioner, typename TImageToImageMetric, typename TDemonsMetric >
bool
DemonsImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TDemonsMetric >
::ProcessPoint( const VirtualIndexType &,
                const VirtualPointType &,
                const FixedImagePointType &,
                const FixedImagePixelType &        fixedImageValue,
                const FixedImageGradientType &     fixedImageGradient,
                const MovingImagePointType &,
                const MovingImagePixelType &       movingImageValue,
                const MovingImageGradientType &    movingImageGradient,
                MeasureType &                      metricValueReturn,
                DerivativeType &                   localDerivativeReturn,
                const ThreadIdType ) const
{
  /* Metric value */
  const InternalComputationValueType speedValue = fixedImageValue - movingImageValue;
  const InternalComputationValueType sqr_speedValue = itk::Math::sqr( speedValue );
  metricValueReturn = sqr_speedValue;

  if( ! this->GetComputeDerivative() )
    {
    return true;
    }

  /* Derivative */
  InternalComputationValueType  gradientSquaredMagnitude = 0;
  const FixedImageGradientType* gradient;
  SizeValueType                 numberOfDimensions;

  if( this->m_DemonsAssociate->GetGradientSourceIncludesFixed() )
    {
    gradient = &fixedImageGradient;
    numberOfDimensions = ImageToImageMetricv4Type::FixedImageDimension;
    }
  else
    {
    gradient = &movingImageGradient;
    numberOfDimensions = ImageToImageMetricv4Type::MovingImageDimension;
    }

  for ( ImageDimensionType j = 0; j < numberOfDimensions; j++ )
    {
    gradientSquaredMagnitude += itk::Math::sqr( (*gradient)[j] );
    }

  /*
   * In the original equation the denominator is defined as (g-f)^2 + grad_mag^2.
   * However there is a mismatch in units between the two terms.
   * The units for the second term is intensity^2/mm^2 while the
   * units for the first term is intensity^2. This mismatch is particularly
   * problematic when the fixed image does not have unit spacing.
   * In this implementation, we normalize the first term by a factor K,
   * such that denominator = (g-f)^2/K + grad_mag^2
   * where K = mean square spacing to compensate for the mismatch in units.
   */
  const InternalComputationValueType denominator = sqr_speedValue / this->m_DemonsAssociate->m_Normalizer + gradientSquaredMagnitude;

  if ( itk::Math::abs(speedValue) < this->m_DemonsAssociate->GetIntensityDifferenceThreshold() ||
       denominator < this->m_DemonsAssociate->GetDenominatorThreshold() )
    {
    localDerivativeReturn.Fill( NumericTraits<DerivativeValueType>::ZeroValue() );
    return true;
    }

  for ( SizeValueType p = 0; p < this->GetCachedNumberOfLocalParameters(); p++ )
    {
    localDerivativeReturn[p] = speedValue * (*gradient)[p] / denominator;
    }

  return true;
}

} // end namespace itk

#endif
