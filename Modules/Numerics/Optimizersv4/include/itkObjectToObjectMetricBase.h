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
#ifndef itkObjectToObjectMetricBase_h
#define itkObjectToObjectMetricBase_h

#include "itkTransformBase.h"
#include "itkSingleValuedCostFunctionv4.h"
#include "ITKOptimizersv4Export.h"

namespace itk
{
/**\class ObjectToObjectMetricBaseTemplateEnums
 *\brief This class contains all the enum classes used by the ObjectToObjectMetricBaseTemplate class.
 * \ingroup ITKOptimizersv4
 */
class ObjectToObjectMetricBaseTemplateEnums
{
public:
  /**
   *\class GradientSource
   * \ingroup ITKOptimizersv4
   * Source of the gradient(s) used by the metric
   * (e.g. image gradients, in the case of
   * image to image metrics). Defaults to Moving. */
  enum class GradientSource : uint8_t
  {
    GRADIENT_SOURCE_FIXED = 0,
    GRADIENT_SOURCE_MOVING,
    GRADIENT_SOURCE_BOTH
  };

  /**
   *\class MetricCategory
   * \ingroup ITKOptimizersv4
   */
  enum class MetricCategory : uint8_t
  {
    UNKNOWN_METRIC = 0,
    OBJECT_METRIC = 1,
    IMAGE_METRIC = 2,
    POINT_SET_METRIC = 3,
    MULTI_METRIC = 4
  };
};
// Define how to print enumeration
extern ITKOptimizersv4_EXPORT std::ostream &
                              operator<<(std::ostream & out, const ObjectToObjectMetricBaseTemplateEnums::GradientSource value);
extern ITKOptimizersv4_EXPORT std::ostream &
                              operator<<(std::ostream & out, const ObjectToObjectMetricBaseTemplateEnums::MetricCategory value);
/**
 *\class ObjectToObjectMetricBaseTemplate
 * \brief Base class for all object-to-object similarity metrics added in ITKv4.
 *
 * This is the abstract base class for a hierarchy of similarity metrics
 * that may, in derived classes, operate on meshes, images, etc.
 * This class computes a value that measures the similarity between the two
 * objects.
 *
 * Derived classes must provide implementations for:
 *  GetValue
 *  GetDerivative
 *  GetValueAndDerivative
 *  Initialize
 *  GetNumberOfParameters
 *  GetNumberOfLocalParameters
 *  GetParameters
 *  SetParameters
 *  HasLocalSupport
 *  UpdateTransformParameters
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TInternalComputationValueType = double>
class ITK_TEMPLATE_EXPORT ObjectToObjectMetricBaseTemplate
  : public SingleValuedCostFunctionv4Template<TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ObjectToObjectMetricBaseTemplate);

  /** Standard class type aliases. */
  using Self = ObjectToObjectMetricBaseTemplate;
  using Superclass = SingleValuedCostFunctionv4Template<TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectToObjectMetricBaseTemplate, SingleValuedCostFunctionv4Template);

  /** Type used for representing object components  */
  using CoordinateRepresentationType = TInternalComputationValueType;

  /**  Type of the measure. */
  using MeasureType = typename Superclass::MeasureType;

  /**  Type of the derivative. */
  using DerivativeType = typename Superclass::DerivativeType;
  using DerivativeValueType = typename DerivativeType::ValueType;

  /**  Type of the parameters. */
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = TInternalComputationValueType;

  /**  Type of object. */
  using ObjectType = Object;
  using ObjectConstPointer = typename ObjectType::ConstPointer;

  /** Get/Set the Fixed Object.  */
  itkSetConstObjectMacro(FixedObject, ObjectType);
  itkGetConstObjectMacro(FixedObject, ObjectType);

  /** Get/Set the Moving Object.  */
  itkSetConstObjectMacro(MovingObject, ObjectType);
  itkGetConstObjectMacro(MovingObject, ObjectType);

  using GradientSourceEnum = itk::ObjectToObjectMetricBaseTemplateEnums::GradientSource;
#if !defined(ITK_LEGACY_REMOVE)
  /** Enables backwards compatibility for enum values */
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr GradientSourceEnum GRADIENT_SOURCE_FIXED = GradientSourceEnum::GRADIENT_SOURCE_FIXED;
  static constexpr GradientSourceEnum GRADIENT_SOURCE_MOVING = GradientSourceEnum::GRADIENT_SOURCE_MOVING;
  static constexpr GradientSourceEnum GRADIENT_SOURCE_BOTH = GradientSourceEnum::GRADIENT_SOURCE_BOTH;
#endif

  /**
   * Set source of gradient.  This variable allows the user to switch
   * between calculating the gradient with respect to the fixed
   * object or moving object.
   * \sa GradientSourceEnum
   */
  itkSetMacro(GradientSource, GradientSourceEnum);

  /**
   * Get gradient source.
   * See \c GetGradientSourceIncludesFixed and \c GetGradientSourceIncludesMoving
   * for convenience methods. */
  itkGetConstMacro(GradientSource, GradientSourceEnum);

  /** Return true of \c m_GradientSource is either \c GRADIENT_SOURCE_FIXED or
   * \c GRADIENT_SOURCE_BOTH. Convenience method. */
  bool
  GetGradientSourceIncludesFixed() const;

  /** Return true of \c m_GradientSource is either \c GRADIENT_SOURCE_MOVING or
   * \c GRADIENT_SOURCE_BOTH. Convenience method. */
  bool
  GetGradientSourceIncludesMoving() const;

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly, and initializing
   *  internal variables as required. This is for one-time initialization,
   *  e.g. before starting an optimization process. */
  virtual void
  Initialize() = 0;

  /** Type to represent the number of parameters that are being optimized at
   * any given iteration of the optimizer. */
  using NumberOfParametersType = unsigned int;

  /** Calculate and return the value for the metric based on the current
   * transformation(s). The result is both returned, and stored in the
   * m_Value member variable. */
  MeasureType
  GetValue() const override = 0;

  /**
   * This method returns the derivative based on the current
   * transformation(s). */
  virtual void
  GetDerivative(DerivativeType &) const = 0;

  /** This method returns the derivative and value based on the current
   * transformation(s). */
  void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const override = 0;

  /** Methods for working with the metric's 'active' transform, e.g. the
   * transform being optimized in the case of registration. Some of these are
   * used in non-metric classes, e.g. optimizers. */
  NumberOfParametersType
  GetNumberOfParameters() const override = 0;
  virtual NumberOfParametersType
  GetNumberOfLocalParameters() const = 0;

  /** Set the active transform's parameters by value*/
  virtual void
  SetParameters(ParametersType & params) = 0;

  /** Get a const reference to the active transform's parameters */
  virtual const ParametersType &
  GetParameters() const = 0;

  /** Return whether the metric's active transform has local support,
   * e.g. whether it is dense/high-dimensional. */
  virtual bool
  HasLocalSupport() const = 0;

  /** Update the parameters of the metric's active transform.
   * Typically this call is passed through directly to the transform.
   * \c factor is a scalar multiplier for each value in update, and
   * defaults to 1.0 .
   * \c derivative must be the proper size, as retrieved
   * from GetNumberOfParameters. */
  virtual void
  UpdateTransformParameters(const DerivativeType & derivative,
                            ParametersValueType    factor = NumericTraits<ParametersValueType>::OneValue()) = 0;

  /** Get the current metric value stored in m_Value. This is only
   * meaningful after a call to GetValue() or GetValueAndDerivative().
   * Note that this would normally be called GetValue, but that name is
   * used for historical reasons by GetValue() to compute the current
   * metric value and store it in m_Value. */
  MeasureType
  GetCurrentValue() const;

  using MetricCategoryEnum = itk::ObjectToObjectMetricBaseTemplateEnums::MetricCategory;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr MetricCategoryEnum UNKNOWN_METRIC = MetricCategoryEnum::UNKNOWN_METRIC;
  static constexpr MetricCategoryEnum OBJECT_METRIC = MetricCategoryEnum::OBJECT_METRIC;
  static constexpr MetricCategoryEnum IMAGE_METRIC = MetricCategoryEnum::IMAGE_METRIC;
  static constexpr MetricCategoryEnum POINT_SET_METRIC = MetricCategoryEnum::POINT_SET_METRIC;
  static constexpr MetricCategoryEnum MULTI_METRIC = MetricCategoryEnum::MULTI_METRIC;
#endif

  /** Get metric category */
  virtual MetricCategoryEnum
  GetMetricCategory() const
  {
    return MetricCategoryEnum::UNKNOWN_METRIC;
  }

protected:
  ObjectToObjectMetricBaseTemplate();
  ~ObjectToObjectMetricBaseTemplate() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Fixed and Moving Objects */
  ObjectConstPointer m_FixedObject;
  ObjectConstPointer m_MovingObject;

  GradientSourceEnum m_GradientSource;

  /** Metric value, stored after evaluating */
  mutable MeasureType m_Value;
};

/** This helps to meet backward compatibility */
using ObjectToObjectMetricBase = ObjectToObjectMetricBaseTemplate<double>;
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkObjectToObjectMetricBase.hxx"
#endif

#endif
