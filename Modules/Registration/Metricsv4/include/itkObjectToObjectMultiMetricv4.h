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

#ifndef itkObjectToObjectMultiMetricv4_h
#define itkObjectToObjectMultiMetricv4_h

#include "itkObjectToObjectMetric.h"
#include "itkArray.h"
#include <deque>

namespace itk
{
/** \class ObjectToObjectMultiMetricv4
  * \brief This class takes one ore more ObjectToObject metrics and assigns weights to their derivatives
  * to compute a single result.
  *
  * This class takes N ObjectToObject-derived component metrics and assigns a weight to each of the metrics'
  * derivatives. It then computes a weighted measure of the metric. The GetValue() and
  * GetValueAndDerivative() methods compute the measure and derivative using the following calculation:
  *
  * metric value = Sum_j ( w_j * M_j ) (see important note below)
  *
  * and the GetDerivative() method computes the derivative by computing:
  *
  * derivative = Sum_j ( w_j * dM_j / ||dM_j|| ) * ( Sum_j( ||dM_j|| ) / J )
  *
  * \note The metric value is unit-less, and thus it is difficult to compute a combined metric.
  * This metric returns the metric value in three ways:
  *  1) GetValue() returns the computed value of only the *first* component metric. This result
  *     is stored in m_Value and also returned by GetCurrentValue().
  *  2) GetValueArray() returns an itkArray of metric values, one for each component metric. It
  *     only has meaning after a call to GetValue(), GetDerivative() or GetValueAndDerivative().
  *  3) GetWeightedValue() returns a combined metric value of all component metrics, using the
  *     assigned weights. It only has meaning after a call to GetValue(), GetDerivative()
  *     or GetValueAndDerivative().
  *
  * The assigned weights are normalized internally to sum to one before use, and the weights
  * default to 1/N, where N is the number of component metrics.
  *
  * \note Each component metric must use the same transform parameters object. That is, each metric
  * must be evaluating the same parameters by evaluating the same transform. Except, if a component
  * transform is a CompositeTransform, in which case it must be set to optimize a single transform,
  * and that transform must be the same as the transform in other component metrics.
  *
  * \note Each component metric must be setup independently, except for the metric transforms
  * which can optionally be set from this class. That is, each component's images or point sets,
  * fixed transforms and options must be set independently. The only methods in this metric for setting
  * up the component metrics is SetMovingTransform(). The corresponding
  * Set accesor is also available. When Set{Fixed/Moving}Transform() is not used
  * this metric's m_{Fixed/Moving}Transform member is assigned to the
  * fixed/moving transform assigned to the first component metric.
  *
  * Each component will be initialized by this metric in the call to Initialize().
  *
  * \note When used with an itkRegistrationParameterScalesEstimator estimator, and the multi-metric
  * holds one or more point-set metrics, the user must assign a virtual domain point set for sampling
  * to ensure proper sampling within the point set metrics. In order to generate valid shift estimations,
  * such a virtual domain point set must include mapped points from the fixed point set.
  * See RegistrationParameterScalesEstimator::SetVirtualDomainPointSet() and
  * PointSetToPointSetMetricv4::GetVirtualTransformedPointSet(). If there are two different point sets,
  * then the virtual domain point set should be a union of the two for completeness.
  *
  * \note If the user does not explicitly assign a virtual domain, then the first valid virtual
  * domain found in the component metrics will be used a virtual domain for this multi-metric,
  * which will be queried by classes such as registration parameter estimators.
  * Each componenet metric will still use its own virtual domain for internal calculations when
  * evaluated, so it is possible to use different virtual domains for each metric if desired.
  * If no component metric has a virtual domain defined, then by default the virtual domain is
  * unbounded.
  * When the transform is high dimensional (e.g. DisplacementFieldTransform) then there must
  * be a virtual domain that matches the space of the transform field. Note that when used
  * with a DisplacementFieldTransform, both Image and PointSet metrics will automatically
  * create a matching virtual domain during initialization if one has not been assigned by the user.
  *
  * \ingroup ITKMetricsv4
  */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, typename TVirtualImage = Image<double, TFixedDimension>, class TInternalComputationValueType = double >
class ITK_TEMPLATE_EXPORT ObjectToObjectMultiMetricv4:
  public ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualImage, TInternalComputationValueType>
{
public:
  /** Standard class typedefs */
  typedef ObjectToObjectMultiMetricv4                                                                      Self;
  typedef ObjectToObjectMetric<TFixedDimension, TMovingDimension, TVirtualImage, TInternalComputationValueType>  Superclass;
  typedef SmartPointer<Self>                                                                               Pointer;
  typedef SmartPointer<const Self>                                                                         ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectToObjectMultiMetricv4, ObjectToObjectMetric);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Types inherited from Superclass. */
  typedef typename Superclass::MeasureType                  MeasureType;
  typedef typename Superclass::DerivativeType               DerivativeType;
  typedef typename Superclass::DerivativeValueType          DerivativeValueType;
  typedef typename Superclass::ParametersType               ParametersType;
  typedef typename Superclass::ParametersValueType          ParametersValueType;
  typedef typename Superclass::NumberOfParametersType       NumberOfParametersType;
  typedef typename Superclass::CoordinateRepresentationType CoordinateRepresentationType;
  typedef typename Superclass::MovingTransformType          MovingTransformType;
  typedef typename Superclass::FixedTransformType           FixedTransformType;

  /** typedefs related to the metric queue */
  typedef Superclass                               MetricType;
  typedef typename MetricType::Pointer             MetricBasePointer;
  typedef typename MetricType::ConstPointer        MetricBaseConstPointer;
  typedef std::deque<MetricBasePointer>            MetricQueueType;

  typedef typename Superclass::ObjectType          ObjectType;

  typedef typename DerivativeType::ValueType       WeightValueType;
  typedef Array<WeightValueType>                   WeightsArrayType;
  typedef Array<MeasureType>                       MetricValueArrayType;

  itkSetMacro(MetricWeights,WeightsArrayType);
  itkGetMacro(MetricWeights,WeightsArrayType);

  /** Add a metric to the queue */
  void AddMetric( MetricType* metric );

  /** Clear the metric queue */
  void ClearMetricQueue();

  /** Get the number of metrics */
  SizeValueType GetNumberOfMetrics() const;

  void Initialize(void) ITK_OVERRIDE;

  /** Set fixed object (image, point set, etc.)*/
  virtual void SetFixedObject( const ObjectType *itkNotUsed( object ) ) ITK_OVERRIDE
    {
    itkExceptionMacro( "A single object should not be specified for the multi metric.");
    }

  /** Set moving object (image, point set, etc.)*/
  virtual void SetMovingObject( const ObjectType *itkNotUsed( object ) ) ITK_OVERRIDE
    {
    itkExceptionMacro( "A single object should not be specified for the multi metric.");
    }

  /** Set each of the component metrics to use this moving transform. */
  virtual void SetMovingTransform( MovingTransformType * ) ITK_OVERRIDE;

  /** Set each of the component metrics to use this fixed transform. */
  virtual void SetFixedTransform( FixedTransformType * ) ITK_OVERRIDE;

  /** Evaluate the metrics and return the value of only the *first* metric.
   * \sa GetValueArray
   * \sa GetWeightedValue
   */
  MeasureType GetValue() const ITK_OVERRIDE;

  virtual void GetDerivative( DerivativeType & ) const ITK_OVERRIDE;

  /** Evaluate the metric value and derivative.
   * \note \param value will contain the value of only the *first* metric on return.
   * \param derivative holds the combined derivative on return.
   *
   * \sa GetValueArray
   * \sa GetWeightedValue */
  void GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const ITK_OVERRIDE;

  /** Returns an itkArray of metric values, one for each component metric. It
   *  only has meaning after a call to GetValue(), GetDerivative() or GetValueAndDerivative(). */
  MetricValueArrayType GetValueArray() const;

  /**  Returns a combined metric value of all component metrics, using the
   *   assigned weights. It only has meaning after a call to GetValue(), GetDerivative() or GetValueAndDerivative(). */
  MeasureType GetWeightedValue() const;

  /** Get the metrics queue */
  const MetricQueueType & GetMetricQueue() const;

  virtual bool SupportsArbitraryVirtualDomainSamples( void ) const ITK_OVERRIDE;

  typedef typename Superclass::MetricCategoryType   MetricCategoryType;

  /** Get metric category */
  virtual MetricCategoryType GetMetricCategory() const ITK_OVERRIDE
    {
    return Superclass::MULTI_METRIC;
    }

protected:

  ObjectToObjectMultiMetricv4();
  virtual ~ObjectToObjectMultiMetricv4() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ObjectToObjectMultiMetricv4);

  MetricQueueType               m_MetricQueue;
  WeightsArrayType              m_MetricWeights;
  mutable MetricValueArrayType  m_MetricValueArray;
};

} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkObjectToObjectMultiMetricv4.hxx"
#endif

#endif
