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
#ifndef __itkObjectToObjectOptimizerBase_hxx
#define __itkObjectToObjectOptimizerBase_hxx

#include "itkObjectToObjectOptimizerBase.h"
#include "itkMultiThreader.h"

namespace itk
{

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
::ObjectToObjectOptimizerBaseTemplate()
{
  this->m_Metric = NULL;
  this->m_CurrentMetricValue = 0;
  // Initialize, but w/out calling SetNumberOfThreads, to avoid
  // valgrind warning.
  this->m_NumberOfThreads = MultiThreader::GetGlobalDefaultNumberOfThreads();
  this->m_ScalesAreIdentity = false;
  this->m_WeightsAreIdentity = true;
}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
::~ObjectToObjectOptimizerBaseTemplate()
{}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
void
ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of threads: " << this->m_NumberOfThreads << std::endl;
  os << indent << "Number of scales:  " << this->m_Scales.Size() << std::endl;
  if( this->m_Scales.Size() > 0 )
    {
    os << indent << "m_Scales: " << this->m_Scales << std::endl;
    }
  else
    {
    os << indent << "m_Scales is unset." << std::endl;
    }
  os << indent << "m_ScalesAreIdentity: " << this->GetScalesAreIdentity() << std::endl;
  if( this->m_Weights.Size() > 0 )
    {
    os << indent << "m_Weights: " << this->m_Weights << std::endl;
    }
  else
    {
    os << indent << "m_Weights is unset. Treated as identity." << std::endl;
    }
  os << indent << "m_WeightsAreIdentity: " << this->GetWeightsAreIdentity() << std::endl;
  os << indent << "Metric: " << std::endl;
  m_Metric->Print( os, indent.GetNextIndent() );
}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
void
ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
::SetNumberOfThreads( ThreadIdType number )
{
  if( number < 1 )
    {
    itkExceptionMacro("Number of threads must be > 0");
    }
  if( number != this->m_NumberOfThreads )
    {
    this->m_NumberOfThreads = number;
    this->Modified();
    }
}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
void
ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
::StartOptimization( bool itkNotUsed(doOnlyInitialization) )
{
  /* Validate some settings */
  if( this->m_Metric.IsNull() )
    {
    itkExceptionMacro("m_Metric must be set.");
    return;
    }

  /* Verify m_Scales. If m_Scales hasn't been set, initialize to all 1's. */
  typedef typename ScalesType::ValueType     SValueType;
  if( this->m_Scales.Size() > 0 )
    {
    if( this->m_Scales.Size() != this->m_Metric->GetNumberOfLocalParameters() )
      {
      itkExceptionMacro("Size of scales (" << this->m_Scales.Size()
                        << ") must equal number of local parameters (" <<
                        this->m_Metric->GetNumberOfLocalParameters() << ").");
      }
    /* Check that all values in m_Scales are > machine epsilon, to avoid
     * division by zero/epsilon.
     * Also check if scales are identity. */
    typedef typename ScalesType::size_type     SizeType;
    this->m_ScalesAreIdentity = true;
    for( SizeType i=0; i < this->m_Scales.Size(); i++ )
      {
      if( this->m_Scales[i] <= NumericTraits<SValueType>::epsilon() )
        {
        itkExceptionMacro("m_Scales values must be > epsilon.");
        }
      /* Check if the scales are identity. Consider to be identity if
       * within a tolerance, to allow for automatically estimated scales
       * that may not be exactly 1.0 when in priciniple they should be. */
      SValueType difference = vcl_fabs( NumericTraits<SValueType>::OneValue() - this->m_Scales[i] );
      SValueType tolerance = static_cast<SValueType>( 0.01 );
      if( difference > tolerance  )
        {
        this->m_ScalesAreIdentity = false;
        break;
        }
      }
    }
  else
    {
    //Initialize scales to identity
    m_Scales.SetSize( this->m_Metric->GetNumberOfLocalParameters() );
    m_Scales.Fill( NumericTraits<SValueType>::OneValue() );
    this->m_ScalesAreIdentity = true;
    }

  /* Verify m_Weights. */
  typedef typename ScalesType::ValueType     SValueType;
  if( this->m_Weights.Size() > 0 )
    {
    if( this->m_Weights.Size() != this->m_Metric->GetNumberOfLocalParameters() )
      {
      itkExceptionMacro("Size of weights (" << this->m_Weights.Size()
                        << ") must equal number of local parameters (" << this->m_Metric->GetNumberOfLocalParameters() << ").");
      }
    /* Check if they are identity within tolerance. */
    typedef typename ScalesType::size_type     SizeType;
    this->m_WeightsAreIdentity = true;
    for( SizeType i=0; i < this->m_Weights.Size(); i++ )
      {
      SValueType difference = vcl_fabs( NumericTraits<SValueType>::OneValue() - this->m_Weights[i] );
      SValueType tolerance = static_cast<SValueType>( 1e-4 );
      if( difference > tolerance  )
        {
        this->m_WeightsAreIdentity = false;
        break;
        }
      }
    }
  else
    {
    // Set weights to identity. But leave the array empty.
    this->m_WeightsAreIdentity = true;
    }
}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
const typename ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>::ParametersType &
ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
::GetCurrentPosition() const
{
  if( this->m_Metric.IsNull() )
    {
    itkExceptionMacro("m_Metric has not been assigned. Cannot get parameters.");
    }
  return this->m_Metric->GetParameters();
}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
const typename ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>::MeasureType &
ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
::GetValue() const
{
  return this->GetCurrentMetricValue();
}

}//namespace itk

#endif
