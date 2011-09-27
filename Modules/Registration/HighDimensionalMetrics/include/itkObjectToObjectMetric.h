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
#ifndef __itkObjectToObjectMetric_h
#define __itkObjectToObjectMetric_h

#include "itkSingleValuedCostFunction.h"

namespace itk
{

/** \class ObjectToObjectMetric
 * \brief Computes similarity between regions of two objects.
 *
 * This class is templated over the type of the two input objects.
 * This is the abstract base class for a hierarchy of similarity metrics
 * that may, in derived classes, operate on meshes, images, etc.
 * This class computes a value that measures the similarity between the two
 * objects.
 *
 * Derived classes must provide implementations for:
 *  GetValue
 *  GetValueAndDerivative
 *  Initialize
 *  GetNumberOfParameters
 *  GetNumberOfLocalParameters
 *  GetParameters
 *  HasLocalSupport
 *  UpdateTransformParameters
 *
 * \note Transform Optimization
 * This hierarchy currently assumes only the moving transform is 'active',
 * i.e. only the moving transform is being optimized when used in an optimizer.
 * The eventual goal however is to allow for either moving, fixed or both
 * transforms to be optimized within a single metric.
 *
 * \ingroup ITKHighDimensionalMetrics
 */

class ITK_EXPORT ObjectToObjectMetric:
  public SingleValuedCostFunction
{
public:
  /** Standard class typedefs. */
  typedef ObjectToObjectMetric       Self;
  typedef SingleValuedCostFunction   Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectToObjectMetric, SingleValuedCostFunction);

  /** Type used for representing object components  */
  typedef Superclass::ParametersValueType CoordinateRepresentationType;

  /** Type for internal computations */
  typedef double                          InternalComputationValueType;

  /**  Type of the measure. */
  typedef  Superclass::MeasureType        MeasureType;

  /**  Type of the derivative. */
  typedef  Superclass::DerivativeType     DerivativeType;

  /**  Type of the parameters. */
  typedef  Superclass::ParametersType       ParametersType;
  typedef  Superclass::ParametersValueType  ParametersValueType;

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
   * Get source of the gradient.
   * \sa GradientSourceType
   */
  itkGetConstMacro( GradientSource, GradientSourceType );

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly, and initializing
   *  internal variables as required. This is for one-time initialization,
   *  e.g. before starting an optimization process. */
  virtual void Initialize(void) throw ( ExceptionObject ) = 0;

  /** This method returns the value of the cost function */
  using Superclass::GetValue;
  virtual MeasureType GetValue() = 0;

  /** This method returns the derivative of the cost function.
   * \c derivative will be sized and allocated as needed by metric.
   * If it's already allocated at proper size, no new allocation is done. */
  using Superclass::GetDerivative;
  virtual void GetDerivative(DerivativeType & derivative);

  /** This method returns the value and derivative of the cost function.
   * \c derivative will be sized and allocated as needed by metric.
   * If it's already proper size, no new allocation is done. */
  using Superclass::GetValueAndDerivative;
  virtual void GetValueAndDerivative(MeasureType & value,
                                     DerivativeType & derivative) = 0;

  /** Methods for working with the metric's 'active' transform, e.g. the
   * transform being optimized in the case of registration. Some of these are
   * used in non-metric classes, e.g. optimizers. */
  virtual unsigned int GetNumberOfParameters() const = 0;
  virtual unsigned int GetNumberOfLocalParameters() const = 0;

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
  virtual void UpdateTransformParameters( DerivativeType & derivative,
                                          ParametersValueType factor = 1.0) = 0;

  /** Provide these three methods to satisfy pure virtuals within
   * SingleValuedCostFunction. This is a sign that we probalby shouldn't
   * be deriving this class from SingleValuedCostFunction. */
  MeasureType GetValue( const ParametersType& ) const;

  /** This method returns the derivative of the cost function */
  void GetDerivative( const ParametersType &, DerivativeType &) const;

  void GetValueAndDerivative (const ParametersType &,
                              MeasureType &,
                              DerivativeType &) const;
protected:
  ObjectToObjectMetric();
  virtual ~ObjectToObjectMetric();

  void PrintSelf(std::ostream & os, Indent indent) const;

  GradientSourceType       m_GradientSource;

private:
  ObjectToObjectMetric(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

};
} // end namespace itk

#endif
