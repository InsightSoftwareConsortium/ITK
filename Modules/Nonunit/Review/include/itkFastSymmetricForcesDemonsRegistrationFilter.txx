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
#ifndef __itkFastSymmetricForcesDemonsRegistrationFilter_txx
#define __itkFastSymmetricForcesDemonsRegistrationFilter_txx

#include "itkFastSymmetricForcesDemonsRegistrationFilter.h"

namespace itk
{
/**
 * Default constructor
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::FastSymmetricForcesDemonsRegistrationFilter()
{
  typename DemonsRegistrationFunctionType::Pointer drfp;
  drfp = DemonsRegistrationFunctionType::New();

  this->SetDifferenceFunction( static_cast< FiniteDifferenceFunctionType * >(
                                 drfp.GetPointer() ) );

  m_Multiplier = MultiplyByConstantType::New();
  m_Multiplier->InPlaceOn();

  m_Adder = AdderType::New();
  m_Adder->InPlaceOn();
}

/*
 * Set the function state values before each iteration
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::InitializeIteration()
{
  // update variables in the equation object
  DemonsRegistrationFunctionType *f = this->DownCastDifferenceFunctionType();

  f->SetDeformationField( this->GetDeformationField() );

  // call the superclass  implementation ( initializes f )
  Superclass::InitializeIteration();
}

/*
 * Get the metric value from the difference function
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::GetMetric() const
{
  const DemonsRegistrationFunctionType *drfp = this->DownCastDifferenceFunctionType();

  return drfp->GetMetric();
}

/**
 * Return intensity difference threshold
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::GetIntensityDifferenceThreshold() const
{
  const DemonsRegistrationFunctionType *drfp = this->DownCastDifferenceFunctionType();

  return drfp->GetIntensityDifferenceThreshold();
}

/**
 * Sets the intensity difference threshold
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::SetIntensityDifferenceThreshold(double threshold)
{
  DemonsRegistrationFunctionType *drfp = this->DownCastDifferenceFunctionType();

  drfp->SetIntensityDifferenceThreshold(threshold);
}

/**
 * Get the maximum update step length
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
double
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::GetMaximumUpdateStepLength() const
{
  const DemonsRegistrationFunctionType *drfp = this->DownCastDifferenceFunctionType();

  return drfp->GetMaximumUpdateStepLength();
}

/**
 * Set the maximum update step length
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::SetMaximumUpdateStepLength(double threshold)
{
  DemonsRegistrationFunctionType *drfp = this->DownCastDifferenceFunctionType();

  drfp->SetMaximumUpdateStepLength(threshold);
}

/**
 * Get the metric value from the difference function
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
const double &
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::GetRMSChange() const
{
  const DemonsRegistrationFunctionType *drfp = this->DownCastDifferenceFunctionType();

  return drfp->GetRMSChange();
}

/**
 *
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
typename FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::GradientType
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::GetUseGradientType() const
{
  const DemonsRegistrationFunctionType *drfp = this->DownCastDifferenceFunctionType();

  return drfp->GetUseGradientType();
}

/**
 *
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::SetUseGradientType(GradientType gtype)
{
  DemonsRegistrationFunctionType *drfp = this->DownCastDifferenceFunctionType();

  drfp->SetUseGradientType(gtype);
}

/**
 * Checks whether the DifferenceFunction is of type DemonsRegistrationFunction.
 * It throws and exception, if it is not.
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
typename FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage,
                                                      TDeformationField >::DemonsRegistrationFunctionType *
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::DownCastDifferenceFunctionType()
{
  DemonsRegistrationFunctionType *drfp =
    dynamic_cast< DemonsRegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to SymmetricDemonsRegistrationFunction");
    }

  return drfp;
}

/**
 * Checks whether the DifferenceFunction is of type DemonsRegistrationFunction.
 * It throws and exception, if it is not.
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
const typename FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage,
                                                            TDeformationField >::DemonsRegistrationFunctionType *
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::DownCastDifferenceFunctionType() const
{
  const DemonsRegistrationFunctionType *drfp =
    dynamic_cast< const DemonsRegistrationFunctionType * >
    ( this->GetDifferenceFunction().GetPointer() );

  if ( !drfp )
    {
    itkExceptionMacro(
      << "Could not cast difference function to SymmetricDemonsRegistrationFunction");
    }

  return drfp;
}

template< class TFixedImage, class TMovingImage, class TDeformationField >
void
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::AllocateUpdateBuffer()
{
  // The update buffer looks just like the output.
  DeformationFieldPointer output = this->GetOutput();
  DeformationFieldPointer upbuf = this->GetUpdateBuffer();

  upbuf->SetLargestPossibleRegion( output->GetLargestPossibleRegion() );
  upbuf->SetRequestedRegion( output->GetRequestedRegion() );
  upbuf->SetBufferedRegion( output->GetBufferedRegion() );
  upbuf->SetOrigin( output->GetOrigin() );
  upbuf->SetSpacing( output->GetSpacing() );
  upbuf->SetDirection( output->GetDirection() );
  upbuf->Allocate();
}

/**
 * Get the metric value from the difference function
 */
template< class TFixedImage, class TMovingImage, class TDeformationField >
void
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::ApplyUpdate(const TimeStepType& dt)
{
  // If we smooth the update buffer before applying it, then the are
  // approximating a viscuous problem as opposed to an elastic problem
  if ( this->GetSmoothUpdateField() )
    {
    this->SmoothUpdateField();
    }

  // use time step if necessary
  if ( vcl_fabs(dt - 1.0) > 1.0e-4 )
    {
    itkDebugMacro("Using timestep: " << dt);
    m_Multiplier->SetInput2(dt);
    m_Multiplier->SetInput( this->GetUpdateBuffer() );
    m_Multiplier->GraftOutput( this->GetUpdateBuffer() );
    // in place update
    m_Multiplier->Update();
    // graft output back to this->GetUpdateBuffer()
    this->GetUpdateBuffer()->Graft( m_Multiplier->GetOutput() );
    }

  m_Adder->SetInput1( this->GetOutput() );
  m_Adder->SetInput2( this->GetUpdateBuffer() );

  m_Adder->GetOutput()->SetRequestedRegion( this->GetOutput()->GetRequestedRegion() );
  m_Adder->Update();

  // Region passing stuff
  this->GraftOutput( m_Adder->GetOutput() );

  DemonsRegistrationFunctionType *drfp = this->DownCastDifferenceFunctionType();

  this->SetRMSChange( drfp->GetRMSChange() );

  /*
   * Smooth the deformation field
   */
  if ( this->GetSmoothDeformationField() )
    {
    this->SmoothDeformationField();
    }
}

template< class TFixedImage, class TMovingImage, class TDeformationField >
void
FastSymmetricForcesDemonsRegistrationFilter< TFixedImage, TMovingImage, TDeformationField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Intensity difference threshold: "
     << this->GetIntensityDifferenceThreshold() << std::endl;
}
} // end namespace itk

#endif
