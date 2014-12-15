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
#ifndef itkGPUDemonsRegistrationFilter_hxx
#define itkGPUDemonsRegistrationFilter_hxx
#include "itkGPUDemonsRegistrationFilter.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter >
GPUDemonsRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter >
::GPUDemonsRegistrationFilter()
{
  typename GPUDemonsRegistrationFunctionType::Pointer drfp;

  drfp = GPUDemonsRegistrationFunctionType::New();

  this->SetDifferenceFunction( drfp );

  m_UseMovingImageGradient = false;
}

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter >
void
GPUDemonsRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter >
::PrintSelf(std::ostream & os, Indent indent) const
{
  GPUSuperclass::PrintSelf(os, indent);
  os << indent << "UseMovingImageGradient: ";
  os << m_UseMovingImageGradient << std::endl;
  os << indent << "Intensity difference threshold: "
     << this->GetIntensityDifferenceThreshold() << std::endl;
}

/*
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter >
void
GPUDemonsRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter >
::InitializeIteration()
{
  // call the GPUSuperclass  implementation
  GPUSuperclass::InitializeIteration();

  // set the gradient selection flag
  GPUDemonsRegistrationFunctionType *drfp =
    dynamic_cast< GPUDemonsRegistrationFunctionType * >
      ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to DemonsRegistrationFunction");
    }

  drfp->SetUseMovingImageGradient(m_UseMovingImageGradient);

  /**
   * Smooth the deformation field
   */
  if ( this->GetSmoothDisplacementField() )
    {
      this->SmoothDisplacementField();
    }
}

/**
 * Get the metric value from the difference function
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter >
double
GPUDemonsRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter >
::GetMetric() const
{
  GPUDemonsRegistrationFunctionType *drfp =
    dynamic_cast< GPUDemonsRegistrationFunctionType * >
      ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to DemonsRegistrationFunction");
    }

  return drfp->GetMetric();
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter >
double
GPUDemonsRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter >
::GetIntensityDifferenceThreshold() const
{
  GPUDemonsRegistrationFunctionType *drfp =
    dynamic_cast< GPUDemonsRegistrationFunctionType * >
      ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to DemonsRegistrationFunction");
    }

  return drfp->GetIntensityDifferenceThreshold();
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter >
void
GPUDemonsRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter >
::SetIntensityDifferenceThreshold(double threshold)
{
  GPUDemonsRegistrationFunctionType *drfp =
    dynamic_cast< GPUDemonsRegistrationFunctionType * >
      ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to DemonsRegistrationFunction");
    }

  drfp->SetIntensityDifferenceThreshold(threshold);
}

/**
 * Get the metric value from the difference function
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField, typename TParentImageFilter >
void
GPUDemonsRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField, TParentImageFilter >
::ApplyUpdate(const TimeStepType& dt)
{
  // If we smooth the update buffer before applying it, then the are
  // approximating a viscuous problem as opposed to an elastic problem
  if ( this->GetSmoothUpdateField() )
    {
    this->SmoothUpdateField();
    }

  this->m_ApplyUpdateTime.Start();
  this->GPUSuperclass::ApplyUpdate(dt);
  this->m_ApplyUpdateTime.Stop();

  GPUDemonsRegistrationFunctionType *drfp =
    dynamic_cast< GPUDemonsRegistrationFunctionType * >
      ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to DemonsRegistrationFunction");
    }

  this->SetRMSChange( drfp->GetRMSChange() );
}

} // end namespace itk

#endif
