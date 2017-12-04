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
#ifndef itkObjectToObjectMetricBase_h
#define itkObjectToObjectMetricBase_h

#include "itkTransformBase.h"
#include "itkSingleValuedCostFunctionv4.h"


namespace itk
{
  /** \class ObjectToObjectMetricBaseTemplate
   * \brief Base class for all object-to-object similarlity metrics added in ITKv4.
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
template<typename TInternalComputationValueType=double>
class ITK_TEMPLATE_EXPORT ObjectToObjectMetricBaseTemplate:
  public SingleValuedCostFunctionv4Template<TInternalComputationValueType>
{
public:
  /** Standard class typedefs. */
  typedef ObjectToObjectMetricBaseTemplate                                   Self;
  typedef SingleValuedCostFunctionv4Template<TInternalComputationValueType>  Superclass;
  typedef SmartPointer< Self >                                               Pointer;
  typedef SmartPointer< const Self >                                         ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectToObjectMetricBaseTemplate, SingleValuedCostFunctionv4Template);

  /** Type used for representing object components  */
  typedef TInternalComputationValueType           CoordinateRepresentationType;

  /**  Type of the measure. */
  typedef typename Superclass::MeasureType        MeasureType;

  /**  Type of the derivative. */
  typedef typename Superclass::DerivativeType     DerivativeType;
  typedef typename DerivativeType::ValueType      DerivativeValueType;

  /**  Type of the parameters. */
  typedef typename Superclass::ParametersType     ParametersType;
  typedef TInternalComputationValueType           ParametersValueType;

  /**  Type of object. */
  typedef Object                                  ObjectType;
  typedef typename ObjectType::ConstPointer       ObjectConstPointer;

  /** Get/Set the Fixed Object.  */
  itkSetConstObjectMacro( FixedObject, ObjectType );
  itkGetConstObjectMacro( FixedObject, ObjectType );

  /** Get/Set the Moving Object.  */
  itkSetConstObjectMacro( MovingObject, ObjectType );
  itkGetConstObjectMacro( MovingObject, ObjectType );

  /** Source of the gradient(s) used by the metric
   * (e.g. image gradients, in the case of
   * image to image metrics). Defaults to Moving. */
  typedef enum  { GRADIENT_SOURCE_FIXED=0,
                  GRADIENT_SOURCE_MOVING,
                  GRADIENT_SOURCE_BOTH } GradientSourceType;

  /**
   * Set source of gradient.  This variable allows the user to switch
   * between calculating the gradient with respect to the fixed
   * object or moving object.
   * \sa GradientSourceType
   */
  itkSetMacro( GradientSource, GradientSourceType );

  /**
   * Get gradient source.
   * See \c GetGradientSourceIncludesFixed and \c GetGradientSourceIncludesMoving
   * for convenience methods. */
  itkGetConstMacro( GradientSource, GradientSourceType );

  /** Return true of \c m_GradientSource is either \c GRADIENT_SOURCE_FIXED or
   * \c GRADIENT_SOURCE_BOTH. Convenience method. */
  bool GetGradientSourceIncludesFixed() const;

  /** Return true of \c m_GradientSource is either \c GRADIENT_SOURCE_MOVING or
   * \c GRADIENT_SOURCE_BOTH. Convenience method. */
  bool GetGradientSourceIncludesMoving() const;

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly, and initializing
   *  internal variables as required. This is for one-time initialization,
   *  e.g. before starting an optimization process. */
  virtual void Initialize(void) = 0;

  /** Type to represent the number of parameters that are being optimized at
   * any given iteration of the optimizer. */
  typedef unsigned int NumberOfParametersType;

  /** Calculate and return the value for the metric based on the current
   * transformation(s). The result is both returned, and stored in the
   * m_Value member variable. */
  virtual MeasureType GetValue() const ITK_OVERRIDE = 0;

  /**
   * This method returns the derivative based on the current
   * transformation(s). */
  virtual void GetDerivative( DerivativeType & ) const = 0;

  /** This method returns the derivative and value based on the current
   * transformation(s). */
  virtual void GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const ITK_OVERRIDE = 0;

  /** Methods for working with the metric's 'active' transform, e.g. the
   * transform being optimized in the case of registration. Some of these are
   * used in non-metric classes, e.g. optimizers. */
  virtual NumberOfParametersType GetNumberOfParameters() const ITK_OVERRIDE = 0;
  virtual NumberOfParametersType GetNumberOfLocalParameters() const = 0;

  /** Set the active transform's parameters by value*/
  virtual void SetParameters( ParametersType & params ) = 0;

  /** Get a const reference to the active transform's parameters */
  virtual const ParametersType & GetParameters() const = 0;

  /** Return whether the metric's active transform has local support,
   * e.g. whether it is dense/high-dimensional. */
  virtual bool HasLocalSupport() const = 0;

  /** Update the parameters of the metric's active transform.
   * Typically this call is passed through directly to the transform.
   * \c factor is a scalar multiplier for each value in update, and
   * defaults to 1.0 .
   * \c derivative must be the proper size, as retrieved
   * from GetNumberOfParameters. */
  virtual void UpdateTransformParameters( const DerivativeType & derivative,
                                         ParametersValueType factor = NumericTraits<ParametersValueType>::OneValue()) = 0;

  /** Get the current metric value stored in m_Value. This is only
   * meaningful after a call to GetValue() or GetValueAndDerivative().
   * Note that this would normally be called GetValue, but that name is
   * used for historical reasons by GetValue() to compute the current
   * metric value and store it in m_Value. */
  MeasureType GetCurrentValue() const;

  typedef enum {
    UNKNOWN_METRIC = 0,
    OBJECT_METRIC = 1,
    IMAGE_METRIC = 2,
    POINT_SET_METRIC = 3,
    MULTI_METRIC = 4
    } MetricCategoryType;

  /** Get metric category */
  virtual MetricCategoryType GetMetricCategory() const
    {
    return UNKNOWN_METRIC;
    }

protected:
  ObjectToObjectMetricBaseTemplate();
  virtual ~ObjectToObjectMetricBaseTemplate() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Fixed and Moving Objects */
  ObjectConstPointer      m_FixedObject;
  ObjectConstPointer      m_MovingObject;

  GradientSourceType              m_GradientSource;

  /** Metric value, stored after evaluating */
  mutable MeasureType             m_Value;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ObjectToObjectMetricBaseTemplate);
};

/** This helps to meet backward compatibility */
typedef ObjectToObjectMetricBaseTemplate<double> ObjectToObjectMetricBase;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkObjectToObjectMetricBase.hxx"
#endif

#endif
