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
#ifndef itkLevelSetMotionRegistrationFilter_hxx
#define itkLevelSetMotionRegistrationFilter_hxx
#include "itkLevelSetMotionRegistrationFilter.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::LevelSetMotionRegistrationFilter()
{
  typename LevelSetMotionFunctionType::Pointer drfp;
  drfp = LevelSetMotionFunctionType::New();

  this->SetDifferenceFunction( static_cast< FiniteDifferenceFunctionType * >(
                                 drfp.GetPointer() ) );

  // By default, no regularization of the deformation field is
  // performed in LevelSetMotionRegistration
  this->SmoothDisplacementFieldOff();
  this->SmoothUpdateFieldOff();
}

template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Intensity difference threshold: "
     << this->GetIntensityDifferenceThreshold() << std::endl;
  os << indent << "Gradient magnitude threshold: "
     << this->GetGradientMagnitudeThreshold() << std::endl;
  os << indent << "Gradient smoothing standard deviations: "
     << this->GetGradientSmoothingStandardDeviations() << std::endl;
}

/*
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::InitializeIteration()
{
  // call the superclass  implementation
  Superclass::InitializeIteration();

  // set the gradient selection flag
  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  drfp->SetUseImageSpacing( this->GetUseImageSpacing() );

  //
  // Smooth the deformation field
  //
  if ( this->GetSmoothDisplacementField() )
    {
    this->SmoothDisplacementField();
    }
}

/**
 * Get the metric value from the difference function
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
bool
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::Halt()
{
  // call the superclass' version
  bool halt = Superclass::Halt();

  if ( ( this->m_RMSChange == 0.0 ) && ( this->GetElapsedIterations() != 0 ) )
    {
    halt = true;
    }

  return halt;
}

/**
 * Get the metric value from the difference function
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
double
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::GetMetric() const
{
  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  return drfp->GetMetric();
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
double
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::GetAlpha() const
{
  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  return drfp->GetAlpha();
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::SetAlpha(double alpha)
{
  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  drfp->SetAlpha(alpha);
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
double
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::GetIntensityDifferenceThreshold() const
{
  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  return drfp->GetIntensityDifferenceThreshold();
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::SetIntensityDifferenceThreshold(double threshold)
{
  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  drfp->SetIntensityDifferenceThreshold(threshold);
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
double
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::GetGradientMagnitudeThreshold() const
{
  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  return drfp->GetGradientMagnitudeThreshold();
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::SetGradientMagnitudeThreshold(double threshold)
{
  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  drfp->SetGradientMagnitudeThreshold(threshold);
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
double
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::GetGradientSmoothingStandardDeviations() const
{
  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  return drfp->GetGradientSmoothingStandardDeviations();
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::SetGradientSmoothingStandardDeviations(double sigma)
{
  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  drfp->SetGradientSmoothingStandardDeviations(sigma);
}

/**
 * Get the metric value from the difference function
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDisplacementField >
::ApplyUpdate(const TimeStepType& dt)
{
  // If we smooth the update buffer before applying it, then the are
  // approximating a viscuous problem as opposed to an elastic problem
  if ( this->GetSmoothUpdateField() )
    {
    this->SmoothUpdateField();
    }

  this->Superclass::ApplyUpdate(dt);

  LevelSetMotionFunctionType *drfp =
    dynamic_cast< LevelSetMotionFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to LevelSetMotionRegistrationFunction");
    }

  this->SetRMSChange( drfp->GetRMSChange() );
}
} // end namespace itk

#endif
