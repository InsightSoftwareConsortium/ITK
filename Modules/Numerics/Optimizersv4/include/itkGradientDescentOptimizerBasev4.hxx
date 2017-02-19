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
#ifndef itkGradientDescentOptimizerBasev4_hxx
#define itkGradientDescentOptimizerBasev4_hxx

#include "itkGradientDescentOptimizerBasev4.h"
#include "itkGradientDescentOptimizerBasev4ModifyGradientByScalesThreader.h"
#include "itkGradientDescentOptimizerBasev4ModifyGradientByLearningRateThreader.h"

namespace itk
{

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::GradientDescentOptimizerBasev4Template():
  m_Stop(false)
{
  /** Threader for apply scales to gradient */
  typename GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate<TInternalComputationValueType>::Pointer modifyGradientByScalesThreader =
    GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate<TInternalComputationValueType>::New();
  this->m_ModifyGradientByScalesThreader = modifyGradientByScalesThreader;

  /** Threader for apply the learning rate to gradient */
  typename GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate<TInternalComputationValueType>::Pointer modifyGradientByLearningRateThreader =
    GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate<TInternalComputationValueType>::New();
  this->m_ModifyGradientByLearningRateThreader = modifyGradientByLearningRateThreader;

  this->m_StopCondition      = MAXIMUM_NUMBER_OF_ITERATIONS;
  this->m_StopConditionDescription << this->GetNameOfClass() << ": ";

  this->m_MaximumStepSizeInPhysicalUnits = NumericTraits<TInternalComputationValueType>::ZeroValue();

  this->m_UseConvergenceMonitoring = true;
  this->m_ConvergenceWindowSize = 50;

  this->m_DoEstimateLearningRateAtEachIteration = false;
  this->m_DoEstimateLearningRateOnce = true;
}

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::~GradientDescentOptimizerBasev4Template()
{}

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
void
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "DoEstimateLearningRateAtEachIteration: "
    << ( this->m_DoEstimateLearningRateAtEachIteration ? "On" : "Off" ) << std::endl;
  os << indent << "DoEstimateLearningRateOnce: "
    << ( this->m_DoEstimateLearningRateOnce ? "On" : "Off" ) << std::endl;
  os << indent << "MaximumStepSizeInPhysicalUnits: "
    << static_cast< typename NumericTraits< TInternalComputationValueType >::PrintType >( this->m_MaximumStepSizeInPhysicalUnits )
    << std::endl;
  os << indent << "UseConvergenceMonitoring: "
    << ( this->m_UseConvergenceMonitoring ? "On" : "Off" ) << std::endl;
  os << indent << "ConvergenceWindowSize: "
    << static_cast< typename NumericTraits< SizeValueType >::PrintType >( this->m_ConvergenceWindowSize )
    << std::endl;

  itkPrintSelfObjectMacro( ConvergenceMonitoring );
  itkPrintSelfObjectMacro( ModifyGradientByScalesThreader );
  itkPrintSelfObjectMacro( ModifyGradientByLearningRateThreader );

  os << indent << "Stop: "
    << ( this->m_Stop ? "On" : "Off" ) << std::endl;
  os << indent << "StopCondition: "
    << static_cast< typename NumericTraits< StopConditionType >::PrintType >( this->m_StopCondition )
    << std::endl;
  os << indent << "StopConditionDescription: "
    << this->m_StopConditionDescription.str() << std::endl;
  os << indent << "Gradient: "
    << static_cast< typename NumericTraits< DerivativeType >::PrintType >( this->m_Gradient )
    << std::endl;
}


//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
const typename GradientDescentOptimizerBasev4Template<TInternalComputationValueType>::StopConditionReturnStringType
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::GetStopConditionDescription() const
{
  return this->m_StopConditionDescription.str();
}

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
void
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::StopOptimization(void)
{
  itkDebugMacro( "StopOptimization called with a description - "
    << this->GetStopConditionDescription() );
  this->m_Stop = true;
  this->InvokeEvent( EndEvent() );
}

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
void
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::ModifyGradientByScales()
{
  if ( (this->GetScalesAreIdentity() && this->GetWeightsAreIdentity())
       || this->m_Gradient.GetSize() == 0 )
    {
    return;
    }

  IndexRangeType fullrange;
  fullrange[0] = 0;
  fullrange[1] = this->m_Gradient.GetSize()-1; //range is inclusive
  /* Perform the modification either with or without threading */

  if( this->m_Metric->HasLocalSupport() )
    {
    // Inheriting classes should instantiate and assign m_ModifyGradientByScalesThreader
    // in their constructor.
    itkAssertInDebugAndIgnoreInReleaseMacro( !m_ModifyGradientByScalesThreader.IsNull() );

    this->m_ModifyGradientByScalesThreader->Execute( this, fullrange );
    }
  else
    {
    /* Global transforms are small, so update without threading. */
    this->ModifyGradientByScalesOverSubRange( fullrange );
    }
}

template<typename TInternalComputationValueType>
void
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::StartOptimization( bool doOnlyInitialization )
{
  itkDebugMacro("StartOptimization");

  /* Validate some settings */
  if ( this->m_ScalesEstimator.IsNotNull() &&
      this->m_DoEstimateLearningRateOnce &&
      this->m_DoEstimateLearningRateAtEachIteration )
    {
    itkExceptionMacro("Both m_DoEstimateLearningRateOnce and "
                      "m_DoEstimateLearningRateAtEachIteration "
                      "are enabled. Not allowed. ");
    }

  /* Estimate the parameter scales if requested. */
  if ( this->m_ScalesEstimator.IsNotNull() && this->m_DoEstimateScales )
    {
    this->m_ScalesEstimator->EstimateScales(this->m_Scales);
    itkDebugMacro( "Estimated scales = " << this->m_Scales );

    /* If user hasn't set this, assign the default. */
    if ( this->m_MaximumStepSizeInPhysicalUnits <= NumericTraits<TInternalComputationValueType>::epsilon())
      {
      this->m_MaximumStepSizeInPhysicalUnits = this->m_ScalesEstimator->EstimateMaximumStepSize();
      }
    }

  if ( this->m_UseConvergenceMonitoring )
    {
    // Initialize the convergence checker
    this->m_ConvergenceMonitoring = ConvergenceMonitoringType::New();
    this->m_ConvergenceMonitoring->SetWindowSize( this->m_ConvergenceWindowSize );
    }

  /* Must call the superclass version for basic validation and setup */
  Superclass::StartOptimization( doOnlyInitialization );
}

//-------------------------------------------------------------------
template<typename TInternalComputationValueType>
void
GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
::ModifyGradientByLearningRate()
{
  if ( this->m_Gradient.GetSize() == 0 )
    {
    return;
    }

  IndexRangeType fullrange;
  fullrange[0] = 0;
  fullrange[1] = this->m_Gradient.GetSize()-1; //range is inclusive

  /* Perform the modification either with or without threading */
  if( this->m_Metric->HasLocalSupport() )
    {
    // Inheriting classes should instantiate and assign m_ModifyGradientByLearningRateThreader
    // in their constructor.
    itkAssertInDebugAndIgnoreInReleaseMacro( !m_ModifyGradientByLearningRateThreader.IsNull() );
    /* Add a check for m_LearningRateIsIdentity?
       But m_LearningRate is not assessible here.
       Should we declare it in a base class as m_Scales ? */

    this->m_ModifyGradientByLearningRateThreader->Execute( this, fullrange );
    }
  else
    {
    /* Global transforms are small, so update without threading. */
    this->ModifyGradientByLearningRateOverSubRange( fullrange );
    }
}

} //namespace itk

#endif
