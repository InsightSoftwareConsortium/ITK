/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetMotionRegistrationFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevelSetMotionRegistrationFilter_txx
#define __itkLevelSetMotionRegistrationFilter_txx
#include "itkLevelSetMotionRegistrationFilter.h"

namespace itk
{
/**
 * Default constructor
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::LevelSetMotionRegistrationFilter()
{
  typename LevelSetMotionFunctionType::Pointer drfp;
  drfp = LevelSetMotionFunctionType::New();

  this->SetDifferenceFunction( static_cast< FiniteDifferenceFunctionType * >(
                                 drfp.GetPointer() ) );

  // By default, no regularization of the deformation field is
  // performed in LevelSetMotionRegistration
  this->SmoothDeformationFieldOff();
  this->SmoothUpdateFieldOff();
}

template< class TFixedImage, class TMovingImage, class TDeformationField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
  if ( this->GetSmoothDeformationField() )
    {
    this->SmoothDeformationField();
    }
}

/**
 * Get the metric value from the difference function
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
bool
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
LevelSetMotionRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::ApplyUpdate(TimeStepType dt)
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
