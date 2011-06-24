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
#ifndef __itkDemonsRegistrationFilter_txx
#define __itkDemonsRegistrationFilter_txx
#include "itkDemonsRegistrationFilter.h"

namespace itk
{
/**
 * Default constructor
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
DemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::DemonsRegistrationFilter()
{
  typename DemonsRegistrationFunctionType::Pointer drfp;
  drfp = DemonsRegistrationFunctionType::New();

  this->SetDifferenceFunction( static_cast< FiniteDifferenceFunctionType * >(
                                 drfp.GetPointer() ) );

  m_UseMovingImageGradient = false;
}

template< class TFixedImage, class TMovingImage, class TDeformationField >
void
DemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "UseMovingImageGradient: ";
  os << m_UseMovingImageGradient << std::endl;
  os << indent << "Intensity difference threshold: "
     << this->GetIntensityDifferenceThreshold() << std::endl;
}

/*
 * Set the function state values before each iteration
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
DemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::InitializeIteration()
{
  // call the superclass  implementation
  Superclass::InitializeIteration();

  // set the gradient selection flag
  DemonsRegistrationFunctionType *drfp =
    dynamic_cast< DemonsRegistrationFunctionType * >
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
  if ( this->GetSmoothDeformationField() )
    {
    this->SmoothDeformationField();
    }
}

/**
 * Get the metric value from the difference function
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
DemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::GetMetric() const
{
  DemonsRegistrationFunctionType *drfp =
    dynamic_cast< DemonsRegistrationFunctionType * >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
DemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::GetIntensityDifferenceThreshold() const
{
  DemonsRegistrationFunctionType *drfp =
    dynamic_cast< DemonsRegistrationFunctionType * >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
DemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::SetIntensityDifferenceThreshold(double threshold)
{
  DemonsRegistrationFunctionType *drfp =
    dynamic_cast< DemonsRegistrationFunctionType * >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
DemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::ApplyUpdate(const TimeStepType& dt)
{
  // If we smooth the update buffer before applying it, then the are
  // approximating a viscuous problem as opposed to an elastic problem
  if ( this->GetSmoothUpdateField() )
    {
    this->SmoothUpdateField();
    }

  this->Superclass::ApplyUpdate(dt);

  DemonsRegistrationFunctionType *drfp =
    dynamic_cast< DemonsRegistrationFunctionType * >
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
