/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkPointSetToPointSetRegistrationMethod_h
#define itkPointSetToPointSetRegistrationMethod_h

#include "itkProcessObject.h"
#include "itkPointSetToPointSetMetric.h"
#include "itkMultipleValuedNonLinearOptimizer.h"
#include "itkDataObjectDecorator.h"

namespace itk
{
/** \class PointSetToPointSetRegistrationMethod
 * \brief Base class for PointSet to PointSet Registration Methods.
 *
 * This Class define the generic interface for a registration method.
 *
 * This class is templated over the type of the PointSet and the PointSet to be
 * registered. A generic Transform is used by this class. That allows to select
 * at run time the particular type of transformation that is to be applied for
 * registering the PointSets.
 *
 * This class uses a generic Metric in order to compare the PointSet
 * and the PointSet. The final goal of the registration method is to find the
 * set of parameters of the Transformation that optimizes the metric.
 *
 * The registration method also supports a generic optimizer that can be
 * selected at run-time. The only restriction for the optimizer is that it
 * should be able to operate in single-valued cost functions given that the
 * metrics used to compare PointSet with PointSets provide a single value as
 * output.
 *
 * The terms FixedPointSet and MovingPointSet are used in this class to indicate
 * that the PointSet is being mapped by the transform.
 *
 * This class uses the coordinate system of the Fixed PointSet as a reference
 * and searches for a Transform that will map points from the space of the Fixed
 * PointSet to the space of the Moving PointSet.
 *
 * For doing so, a Metric will be continuously applied to compare the Fixed
 * PointSet with the Transformed Moving PointSet. This process also requires to
 * interpolate values from the Moving PointSet.
 *
 * This class requires the Transform, the Metric, and the Optimizer to be
 * explicitly set.
 *
 * \ingroup RegistrationFilters
 * \ingroup ITKRegistrationCommon
 */
template <typename TFixedPointSet, typename TMovingPointSet>
class ITK_TEMPLATE_EXPORT PointSetToPointSetRegistrationMethod : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointSetToPointSetRegistrationMethod);

  /** Standard class type aliases. */
  using Self = PointSetToPointSetRegistrationMethod;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToPointSetRegistrationMethod, ProcessObject);

  /**  Type of the Fixed PointSet. */
  using FixedPointSetType = TFixedPointSet;
  using FixedPointSetConstPointer = typename FixedPointSetType::ConstPointer;

  /**  Type of the Moving PointSet. */
  using MovingPointSetType = TMovingPointSet;
  using MovingPointSetConstPointer = typename MovingPointSetType::ConstPointer;

  /**  Type of the Metric. */
  using MetricType = PointSetToPointSetMetric<FixedPointSetType, MovingPointSetType>;
  using MetricPointer = typename MetricType::Pointer;

  /**  Type of the Transform . */
  using TransformType = typename MetricType::TransformType;
  using TransformPointer = typename TransformType::Pointer;

  /** Type for the output: Using Decorator pattern for enabling
   *  the Transform to be passed in the data pipeline. */
  using TransformOutputType = DataObjectDecorator<TransformType>;
  using TransformOutputPointer = typename TransformOutputType::Pointer;
  using TransformOutputConstPointer = typename TransformOutputType::ConstPointer;

  /**  Type of the Optimizer. */
  using OptimizerType = MultipleValuedNonLinearOptimizer;

  /** Type of the Transformation parameters. This is the same type used to
   *  represent the search space of the optimization algorithm. */
  using ParametersType = typename MetricType::TransformParametersType;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Set/Get the Fixed PointSet. */
  itkSetConstObjectMacro(FixedPointSet, FixedPointSetType);
  itkGetConstObjectMacro(FixedPointSet, FixedPointSetType);

  /** Set/Get the Moving PointSet. */
  itkSetConstObjectMacro(MovingPointSet, MovingPointSetType);
  itkGetConstObjectMacro(MovingPointSet, MovingPointSetType);

  /** Set/Get the Optimizer. */
  itkSetObjectMacro(Optimizer, OptimizerType);
  itkGetModifiableObjectMacro(Optimizer, OptimizerType);

  /** Set/Get the Metric. */
  itkSetObjectMacro(Metric, MetricType);
  itkGetModifiableObjectMacro(Metric, MetricType);

  /** Set/Get the Transfrom. */
  itkSetObjectMacro(Transform, TransformType);
  itkGetModifiableObjectMacro(Transform, TransformType);

  /** Set/Get the initial transformation parameters. */
  virtual void
  SetInitialTransformParameters(const ParametersType & param);

  itkGetConstReferenceMacro(InitialTransformParameters, ParametersType);

  /** Get the last transformation parameters visited by
   * the optimizer. */
  itkGetConstReferenceMacro(LastTransformParameters, ParametersType);

  /** Initialize by setting the interconnects between the components. */
  void
  Initialize();

  /** Returns the transform resulting from the registration process  */
  const TransformOutputType *
  GetOutput() const;

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

  ModifiedTimeType
  GetMTime() const override;

protected:
  PointSetToPointSetRegistrationMethod();
  ~PointSetToPointSetRegistrationMethod() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  MetricPointer          m_Metric;
  OptimizerType::Pointer m_Optimizer;

  MovingPointSetConstPointer m_MovingPointSet;
  FixedPointSetConstPointer  m_FixedPointSet;

  TransformPointer m_Transform;

  ParametersType m_InitialTransformParameters;
  ParametersType m_LastTransformParameters;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointSetToPointSetRegistrationMethod.hxx"
#endif

#endif
