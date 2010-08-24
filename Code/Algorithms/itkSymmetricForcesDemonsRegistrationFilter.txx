/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricForcesDemonsRegistrationFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSymmetricForcesDemonsRegistrationFilter_txx
#define __itkSymmetricForcesDemonsRegistrationFilter_txx
#include "itkSymmetricForcesDemonsRegistrationFilter.h"

namespace itk
{
/**
 * Default constructor
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
SymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::SymmetricForcesDemonsRegistrationFilter()
{
  typename DemonsRegistrationFunctionType::Pointer drfp;
  drfp = DemonsRegistrationFunctionType::New();

  this->SetDifferenceFunction( static_cast< FiniteDifferenceFunctionType * >(
                                 drfp.GetPointer() ) );
}

/*
 * Set the function state values before each iteration
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
SymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::InitializeIteration()
{
  // update variables in the equation object
  DemonsRegistrationFunctionType *f =
    dynamic_cast< DemonsRegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !f )
    {
    itkExceptionMacro(<< "FiniteDifferenceFunction not of type DemonsRegistrationFunctionType");
    }

  f->SetDeformationField( this->GetDeformationField() );

  // call the superclass  implementation
  Superclass::InitializeIteration();

  /*
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
SymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::GetMetric() const
{
  DemonsRegistrationFunctionType *drfp =
    dynamic_cast< DemonsRegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to SymmetricForcesDemonsRegistrationFunction");
    }

  return drfp->GetMetric();
}

/*
 *
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
SymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
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

/*
 *
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
SymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::SetIntensityDifferenceThreshold(double threshold)
{
  DemonsRegistrationFunctionType *drfp =
    dynamic_cast< DemonsRegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to SymmetricDemonsRegistrationFunction");
    }

  drfp->SetIntensityDifferenceThreshold(threshold);
}

/*
 * Get the metric value from the difference function
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
const double &
SymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::GetRMSChange() const
{
  DemonsRegistrationFunctionType *drfp =
    dynamic_cast< DemonsRegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to SymmetricForcesDemonsRegistrationFunction");
    }

  return drfp->GetRMSChange();
}

/*
 * Get the metric value from the difference function
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
SymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::ApplyUpdate(TimeStepType dt)
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

template< class TFixedImage, class TMovingImage, class TDeformationField >
void
SymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Intensity difference threshold: "
     << this->GetIntensityDifferenceThreshold() << std::endl;
}
} // end namespace itk

#endif
