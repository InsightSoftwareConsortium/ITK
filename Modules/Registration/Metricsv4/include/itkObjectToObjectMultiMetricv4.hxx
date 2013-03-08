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
#ifndef __itkObjectToObjectMultiMetricv4_hxx
#define __itkObjectToObjectMultiMetricv4_hxx

#include "itkObjectToObjectMultiMetricv4.h"
#include "itkCompositeTransform.h"

namespace itk
{

/** Constructor */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::ObjectToObjectMultiMetricv4()
{
  this->m_MetricQueue.clear();

  //We want the moving transform to be NULL by default
  this->m_MovingTransform = NULL;
}

/** Destructor */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::~ObjectToObjectMultiMetricv4()
{
}

/** Add a metric to the queue. */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
void
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::AddMetric (MetricType* metric)
{
  this->m_MetricQueue.push_back(metric);
}

/** Clear the queue */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
void
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::ClearMetricQueue()
{
  this->m_MetricQueue.clear();
}

/** Get the number of metrics */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
itk::SizeValueType
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::GetNumberOfMetrics() const
{
  return this->m_MetricQueue.size();
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
void
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::SetMovingTransform( MovingTransformType * transform )
{
  if( this->GetNumberOfMetrics() == 0 )
    {
    itkExceptionMacro("No metrics are assigned. Cannot assign transform.");
    }
  Superclass::SetMovingTransform( transform );
  for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
    {
    this->m_MetricQueue[j]->SetMovingTransform( transform );
    }
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
void
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::SetFixedTransform( FixedTransformType * transform )
{
  if( this->GetNumberOfMetrics() == 0 )
    {
    itkExceptionMacro("No metrics are assigned. Cannot assign transform.");
    }
  Superclass::SetFixedTransform( transform );
  for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
    {
    this->m_MetricQueue[j]->SetFixedTransform( transform );
    }
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
void
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::Initialize() throw ( ExceptionObject )
{
  if( this->GetNumberOfMetrics() == 0 )
    {
    itkExceptionMacro("No metrics are assigned. Cannot evaluate.");
    }

  /* Verify derivative weights and initialize if appropriate */
  if( this->m_MetricWeights.Size() > 0 )
    {
    if( this->m_MetricWeights.Size() != this->GetNumberOfMetrics() )
      {
      itkExceptionMacro("The derivative weights are not of the proper size. "
                        "Number of metrics: " << this->GetNumberOfMetrics() << ", "
                        "Number of weights: " << this->m_MetricWeights.Size() );
      }
    /* normalize the weights */
    WeightValueType sum = NumericTraits<WeightValueType>::Zero;
    for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
      {
      sum += this->m_MetricWeights[j];
      }
    if( sum <= NumericTraits<WeightValueType>::epsilon() )
      {
      itkExceptionMacro("The derivative weights are too small: " << this->m_MetricWeights );
      }
    for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
      {
      this->m_MetricWeights[j] = this->m_MetricWeights[j] / sum;
      }
    }
  else
    {
    /* Initialize to defaults */
    this->m_MetricWeights.SetSize( this->GetNumberOfMetrics() );
    this->m_MetricWeights.Fill( NumericTraits<WeightValueType>::One / static_cast<WeightValueType>(this->GetNumberOfMetrics()) );
    }

  /* resize */
  this->m_MetricValueArray.SetSize( this->GetNumberOfMetrics() );

  /* Verify the same transform is in all metrics. */
  const MovingTransformType * firstTransform = NULL;
  for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
    {
    const MovingTransformType * transform = this->m_MetricQueue[j]->GetMovingTransform();
    //Check if it's a composite. If so, there must be only one transform set to be
    // optimized, and it must be the same as in other metrics.
    typedef CompositeTransform<typename MovingTransformType::ScalarType, TFixedDimension> CompositeType;
    const CompositeType * composite = dynamic_cast<const CompositeType*>(transform);
    if( composite != NULL )
      {
      SizeValueType count = 0;
      for( size_t n = 0; n < composite->GetNumberOfTransforms(); n++ )
        {
        if( composite->GetNthTransformToOptimize( n ) )
          {
          count++;
          transform = composite->GetNthTransform( n );
          }
        }
      if( count != 1 )
        {
        itkExceptionMacro("Expected exactly one transform set to be optimized within the composite transform. Error with metric " << j << ".");
        }
      }

    if( j == 0 )
      {
      firstTransform = transform;
      }
    else
      {
      if( transform != firstTransform )
        {
        itkExceptionMacro("One or more component metrics have different active transforms. "
                          "Each metric must be using the same transform object. For CompositeTransform, "
                          "there must be only one transform set to optimize, and it must be the same "
                          "as other metric transforms." );
        }
      }
    }

  /* Assign local pointers to common transforms */
  if( this->m_MovingTransform.IsNull() )
    {
    Superclass::SetMovingTransform( const_cast<MovingTransformType*>(this->m_MetricQueue[0]->GetMovingTransform()) );
    }
  if( this->m_FixedTransform.IsNull() )
    {
    Superclass::SetFixedTransform(  const_cast<MovingTransformType*>(this->m_MetricQueue[0]->GetFixedTransform()) );
    }

  /* Initialize individual metrics. */
  for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
    {
    try
      {
      this->m_MetricQueue[j]->Initialize();
      }
    catch(ExceptionObject exc)
      {
      std::string msg("Caught exception initializing metric: \n");
      msg += exc.what();
      ExceptionObject err(__FILE__, __LINE__, msg);
      throw err;
      }
    }

  /* Get the first valid virtual domain and assign
   * it to this metric as a common virtual domain,
   * for direct use by calling classes. */
  for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
    {
    if( this->m_MetricQueue[j]->GetVirtualImage() )
      {
      this->SetVirtualDomainFromImage( this->m_MetricQueue[j]->GetVirtualImage() );
      break;
      }
    }

  /* Do this after we've setup local copy of virtual domain */
  Superclass::Initialize();
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
typename ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>::MeasureType
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::GetValue() const
{
  for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
    {
    this->m_MetricValueArray[j] = this->m_MetricQueue[j]->GetValue();
    }

  MeasureType firstValue = this->m_MetricValueArray[0];
  this->m_Value = firstValue;
  return firstValue;
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
void
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::GetDerivative(DerivativeType & derivativeResult) const
{
  MeasureType firstValue;
  this->GetValueAndDerivative( firstValue, derivativeResult );
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
void
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::GetValueAndDerivative(MeasureType & firstValue, DerivativeType & derivativeResult) const
{
  if ( derivativeResult.GetSize() != this->GetNumberOfParameters() )
    {
    derivativeResult.SetSize( this->GetNumberOfParameters() );
    }
  derivativeResult.Fill( NumericTraits<DerivativeValueType>::Zero );

  DerivativeType  metricDerivative;
  MeasureType     metricValue = NumericTraits<MeasureType>::Zero;

  // Loop over metrics
  DerivativeValueType totalMagnitude = NumericTraits<DerivativeValueType>::Zero;
  for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
    {
    this->m_MetricQueue[j]->GetValueAndDerivative( metricValue, metricDerivative);
    this->m_MetricValueArray[j] = metricValue;

    DerivativeValueType magnitude = metricDerivative.magnitude();
    DerivativeValueType weightOverMagnitude = NumericTraits<DerivativeValueType>::Zero;
    totalMagnitude += magnitude;

    if( magnitude > NumericTraits<DerivativeValueType>::epsilon() )
      {
      weightOverMagnitude = this->m_MetricWeights[j] / magnitude;
      }
    // derivative = \sum_j w_j * (dM_j / ||dM_j||)
    for( NumberOfParametersType p = 0; p < this->GetNumberOfParameters(); p++ )
      {
      // roll our own loop to avoid temporary variable that could be large when using displacement fields.
      derivativeResult[p] += ( metricDerivative[p] * weightOverMagnitude );
      }
    }

  // Scale by totalMagnitude to prevent what amounts to implicit step estimation from magnitude scaling.
  // This keeps the behavior of this metric the same as a regular metric, with respect to derivative
  // magnitudes.
  totalMagnitude /= this->GetNumberOfMetrics();
  for( NumberOfParametersType p = 0; p < this->GetNumberOfParameters(); p++ )
    {
    derivativeResult[p] *= totalMagnitude;
    }

  firstValue = this->m_MetricValueArray[0];
  this->m_Value = firstValue;
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
typename ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>::MetricValueArrayType
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::GetValueArray() const
{
  return this->m_MetricValueArray;
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
typename ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>::MeasureType
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::GetWeightedValue() const
{
  MeasureType value = NumericTraits<MeasureType>::Zero;

  for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
    {
    // value = sum_j w_j * M_j
    value += this->m_MetricValueArray[j] * this->m_MetricWeights[j];
    }
  return value;
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
const typename ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>::MetricQueueType &
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::GetMetricQueue() const
{
  return this->m_MetricQueue;
}


template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
bool
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::SupportsArbitraryVirtualDomainSamples( void ) const
{
  for (SizeValueType j = 0; j < this->GetNumberOfMetrics(); j++)
    {
    if( ! this->m_MetricQueue[j]->SupportsArbitraryVirtualDomainSamples() )
      {
      return false;
      }
    }
  return true;
}

template<unsigned int TFixedDimension, unsigned int TMovingDimension, class TVirtualImage>
void
ObjectToObjectMultiMetricv4<TFixedDimension, TMovingDimension, TVirtualImage>
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Weights of metric derivatives: " << this->m_MetricWeights << std::endl;
  os << indent << "The multivariate contains the following metrics: " << std::endl << std::endl;
  for (SizeValueType i = 0; i < this->GetNumberOfMetrics(); i++)
    {
    os << indent << "~~~ Metric " << i << " ~~~" << std::endl;
    this->m_MetricQueue[i]->Print(os, indent );
    }
}

} // end namespace itk

#endif //__itkObjectToObjectMultiMetricv4_hxx
