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
#ifndef itkDemonsImageToImageMetricv4_hxx
#define itkDemonsImageToImageMetricv4_hxx

#include "itkDemonsImageToImageMetricv4.h"

namespace itk
{

template < typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits >
DemonsImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage, TInternalComputationValueType, TMetricTraits>
::DemonsImageToImageMetricv4()
{
  // We have our own GetValueAndDerivativeThreader's that we want
  // ImageToImageMetricv4 to use.
  this->m_DenseGetValueAndDerivativeThreader  = DemonsDenseGetValueAndDerivativeThreaderType::New();
  this->m_SparseGetValueAndDerivativeThreader = DemonsSparseGetValueAndDerivativeThreaderType::New();

  // Unlike most other metrics, this defaults to using fixed image gradients
  this->SetGradientSource( this->GRADIENT_SOURCE_FIXED );

  this->m_Normalizer = NumericTraits<TInternalComputationValueType>::OneValue();
  this->m_DenominatorThreshold = static_cast<TInternalComputationValueType>(1e-9);
  this->m_IntensityDifferenceThreshold = static_cast<TInternalComputationValueType>(0.001);

}

template < typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits >
DemonsImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage, TInternalComputationValueType, TMetricTraits>
::~DemonsImageToImageMetricv4()
{
}

template < typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits >
void
DemonsImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage, TInternalComputationValueType, TMetricTraits>
::Initialize(void)
{
  // Make sure user has not set to use both moving and fixed image
  // gradients
  if( this->GetGradientSource() == this->GRADIENT_SOURCE_BOTH )
    {
    itkExceptionMacro("GradientSource has been set to GRADIENT_SOURCE_BOTH. "
                      "You must choose either GRADIENT_SOURCE_MOVING or "
                      "GRADIENT_SOURCE_FIXED." );
    }

  // Verify that the transform has local support, and its number of local
  // parameters equals the dimensionality of the image gradient source.
  if( this->m_MovingTransform->GetTransformCategory() != MovingTransformType::DisplacementField )
    {
    itkExceptionMacro( "The moving transform must be a displacement field transform" );
    }

  // compute the normalizer
  ImageDimensionType dimension;
  typename TFixedImage::SpacingType imageSpacing;
  if( this->GetGradientSource() == this->GRADIENT_SOURCE_FIXED )
    {
    imageSpacing = this->m_FixedImage->GetSpacing();
    dimension = FixedImageDimension;
    }
  else
    {
    imageSpacing = this->m_MovingImage->GetSpacing();
    dimension = MovingImageDimension;
    }

  this->m_Normalizer = NumericTraits<TInternalComputationValueType>::ZeroValue();
  for ( ImageDimensionType k = 0; k < dimension; k++ )
    {
    this->m_Normalizer += imageSpacing[k] * imageSpacing[k];
    }
  this->m_Normalizer /= static_cast<TInternalComputationValueType>( dimension );

  Superclass::Initialize();
}

template < typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits >
void
DemonsImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage, TInternalComputationValueType, TMetricTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "IntensityDifferenceThreshold: "
               << this->GetIntensityDifferenceThreshold() << std::endl
     << indent << "DenominatorThreshold: " << this->GetDenominatorThreshold()
               << std::endl
     << indent << "Normalizer: " << this->GetNormalizer() << std::endl;

}

} // end namespace itk


#endif
