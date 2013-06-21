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
#ifndef __itkObjectToObjectOptimizerBase_h
#define __itkObjectToObjectOptimizerBase_h

#include "itkOptimizerParameters.h"
#include "itkObjectToObjectMetricBase.h"
#include "itkIntTypes.h"

namespace itk
{
/** \class ObjectToObjectOptimizerBaseTemplate
 * \brief Abstract base for object-to-object optimizers.
 *
 * The goal of this optimizer hierarchy is to work with metrics
 * of any type, i.e. working with any kind of object, such as
 * image or point-set.
 *
 * Transform parameters are not manipulated directly. Instead,
 * the optimizer retrieves the metric derivative from the metric,
 * modifies the derivative as required, then passes it back to
 * the metric as an update. The metric then processes it as
 * appropriate, typically by passing it to its transform that is
 * being optimized.
 *
 * SetScales() allows setting of a per-local-parameter scaling array.
 * The gradient of each local parameter is divided by the corresponding
 * scale. If unset, the \c m_Scales array will be initialized to all 1's.
 * Note that when used with transforms with local support, these scales
 * correspond to each _local_ parameter, and not to each parameter. For
 * example, in a DisplacementFieldTransform of dimensionality N, the Scales
 * is size N, with each element corresponding to a dimension within the
 * transform's displacement field, and is applied to each vector in the
 * displacement field.
 *
 * SetWeights() allows setting of a per-local-parameter weighting array.
 * If unset, the weights are treated as identity. Weights are multiplied
 * by the gradient at the same time scaling is applied. Weights are
 * different than scales, and may be used, for example, to easily mask out a
 * particular parameter during optimzation to hold it constant. Or they
 * may be used to apply another kind of prior knowledge.
 *
 * Threading of some optimizer operations may be handled within
 * derived classes, for example in GradientDescentOptimizer.
 *
 * \note Derived classes must override StartOptimization, and then call
 * this base class version to perform common initializations.
 *
 * \ingroup ITKOptimizersv4
 */
template< class TInternalComputationValueType>
class ITK_EXPORT ObjectToObjectOptimizerBaseTemplate : public Object
{
public:
  /** Standard class typedefs. */
  typedef ObjectToObjectOptimizerBaseTemplate         Self;
  typedef Object                                      Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectToObjectOptimizerBaseTemplate, Object);

  /**  Scale type. */
  typedef OptimizerParameters< TInternalComputationValueType >          ScalesType;

  /**  Parameters type. */
  typedef OptimizerParameters< TInternalComputationValueType >          ParametersType;

  /** Metric function type */
  typedef ObjectToObjectMetricBaseTemplate< TInternalComputationValueType >  MetricType;
  typedef typename MetricType::Pointer                                      MetricTypePointer;

  /** Number of parameters type */
  typedef typename MetricType::NumberOfParametersType        NumberOfParametersType;

  /** Measure type */
  typedef typename MetricType::MeasureType                   MeasureType;

  /** Accessors for Metric */
  itkSetObjectMacro( Metric, MetricType );
  itkGetModifiableObjectMacro(Metric, MetricType );

  /** Accessor for metric value. Returns the value
   *  stored in m_CurrentMetricValue from the most recent
   *  call to evaluate the metric. */
  itkGetConstReferenceMacro( CurrentMetricValue, MeasureType );

  /** Deprecated accessor for currently stored metric value for use
   *  by classes that support both v4 and v3 optimizers.
   *
   *  \sa GetCurrentMetricValue()
   */
  const MeasureType & GetValue();

  /** Set current parameters scaling. */
  itkSetMacro( Scales, ScalesType );

  /** Get current parameters scaling. */
  itkGetConstReferenceMacro( Scales, ScalesType );

  /** Get whether scales are identity. Cannot be set */
  itkGetConstReferenceMacro( ScalesAreIdentity, bool );

  /** Set current parameters weights. */
  itkSetMacro( Weights, ScalesType );

  /** Get current parameters weights. This will return an
   * empty array when weights have not been set by user. */
  itkGetConstReferenceMacro( Weights, ScalesType );

  /** Get whether weights are identity. Cannot be set */
  itkGetConstReferenceMacro( WeightsAreIdentity, bool );

  /** Set the number of threads to use when threading.
   * The default is the global default number of threads
   * returned from itkMultiThreader. */
  virtual void SetNumberOfThreads( ThreadIdType number );

  /** Get the number of threads set to be used. */
  itkGetConstReferenceMacro( NumberOfThreads, ThreadIdType );

  /** Get a reference to the current position of the optimization.
   * This returns the parameters from the assigned metric, since the optimizer
   * itself does not store a position. */
  const ParametersType & GetCurrentPosition();

  /** Run the optimization.
   * \param doOnlyInitialization This is false by default. It should only be
   * set to true for special cases when the class should be initialized to
   * perform optimization, but no optimization should be run. For example,
   * itkMultiGradientOptimizerv4 needs to do this.
   * \note Derived classes must override and call this superclass method, then
   * perform any additional initialization before performing optimization. */
  virtual void StartOptimization( bool doOnlyInitialization = false );

protected:

  /** Default constructor */
  ObjectToObjectOptimizerBaseTemplate();
  virtual ~ObjectToObjectOptimizerBaseTemplate();

  MetricTypePointer             m_Metric;
  ThreadIdType                  m_NumberOfThreads;

  /** Metric measure value at a given iteration, as most recently evaluated. */
  MeasureType                   m_CurrentMetricValue;

  /** Scales. Size is expected to be == metric->GetNumberOfLocalParameters().
   * See the main documentation for more details. */
  ScalesType                    m_Scales;

  /** Parameter weights. These are applied to local parameters, at the same time
   * as scales. See main documentation.
   * If not set by user, the array remains empty and treated as identity to simplify
   * the reuse of an optimizer with transforms with different numbers of parameters. */
  ScalesType                    m_Weights;

  /** Flag to avoid unnecessary arithmetic when scales are identity. */
  bool                          m_ScalesAreIdentity;

  /** Flag to avoid unnecessary arithmetic when weights are identity. */
  bool                          m_WeightsAreIdentity;

  virtual void PrintSelf(std::ostream & os, Indent indent) const;

private:

  //purposely not implemented
  ObjectToObjectOptimizerBaseTemplate( const Self & );
  //purposely not implemented
  void operator=( const Self& );

};

/** This helps to meet backward compatibility */
typedef ObjectToObjectOptimizerBaseTemplate<double> ObjectToObjectOptimizerBase;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkObjectToObjectOptimizerBase.hxx"
#endif

#endif
