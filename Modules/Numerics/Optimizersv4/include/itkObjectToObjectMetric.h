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

#include "itkTransform.h"
#include "itkObjectToObjectMetricBase.h"

namespace itk
{

/** \class ObjectToObjectMetric
 * \brief Computes similarity between regions of two objects.
 *
 * This class is templated over the dimensionality of the two input objects.
 * This is the abstract templated base class for a hierarchy of similarity metrics
 * that may, in derived classes, operate on meshes, images, etc.
 * This class computes a value that measures the similarity between the two
 * objects.
 *
 * Derived classes must provide implementations for:
 *  GetValue
 *  GetDerivative
 *  GetValueAndDerivative
 *
 * \note Transform Optimization
 * This hierarchy currently assumes only the moving transform is 'active',
 * i.e. only the moving transform is being optimized when used in an optimizer.
 * The eventual goal however is to allow for either moving, fixed or both
 * transforms to be optimized within a single metric.
 *
 * \ingroup ITKOptimizersv4
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, unsigned int TVirtualDimension = TFixedDimension>
class ITK_EXPORT ObjectToObjectMetric:
  public ObjectToObjectMetricBase
{
public:
  /** Standard class typedefs. */
  typedef ObjectToObjectMetric         Self;
  typedef ObjectToObjectMetricBase     Superclass;
  typedef SmartPointer< Self >         Pointer;
  typedef SmartPointer< const Self >   ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectToObjectMetric, ObjectToObjectMetricBase);

  /** Type used for representing object components  */
  typedef typename Superclass::ParametersValueType CoordinateRepresentationType;

  /** Type for internal computations */
  typedef typename Superclass::InternalComputationValueType    InternalComputationValueType;

  /**  Type of the measure. */
  typedef typename Superclass::MeasureType            MeasureType;

  /**  Type of the derivative. */
  typedef typename Superclass::DerivativeType         DerivativeType;
  typedef typename Superclass::DerivativeValueType    DerivativeValueType;

  /**  Type of the parameters. */
  typedef typename Superclass::ParametersType         ParametersType;
  typedef typename Superclass::ParametersValueType    ParametersValueType;
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  /** Dimension type */
  typedef SizeValueType                       DimensionType;

  /** Object dimension accessors */
  itkStaticConstMacro(FixedDimension, DimensionType, TFixedDimension);
  itkStaticConstMacro(MovingDimension, DimensionType, TMovingDimension);
  itkStaticConstMacro(VirtualDimension, DimensionType, TVirtualDimension);

  /**  Type of the Transform Base classes */
  typedef Transform<ParametersValueType, TMovingDimension, TVirtualDimension> MovingTransformType;
  typedef Transform<ParametersValueType, TFixedDimension, TVirtualDimension>  FixedTransformType;

  typedef typename FixedTransformType::Pointer         FixedTransformPointer;
  typedef typename FixedTransformType::InputPointType  FixedInputPointType;
  typedef typename FixedTransformType::OutputPointType FixedOutputPointType;
  typedef typename FixedTransformType::ParametersType  FixedTransformParametersType;

  typedef typename MovingTransformType::Pointer         MovingTransformPointer;
  typedef typename MovingTransformType::InputPointType  MovingInputPointType;
  typedef typename MovingTransformType::OutputPointType MovingOutputPointType;
  typedef typename MovingTransformType::ParametersType  MovingTransformParametersType;

  /** Jacobian type. This is the same for all transforms */
  typedef typename FixedTransformType::JacobianType     JacobianType;
  typedef typename FixedTransformType::JacobianType     FixedTransformJacobianType;
  typedef typename MovingTransformType::JacobianType    MovingTransformJacobianType;

  virtual void Initialize(void) throw ( ExceptionObject );

  virtual NumberOfParametersType GetNumberOfParameters() const;
  virtual NumberOfParametersType GetNumberOfLocalParameters() const;
  virtual void SetParameters( ParametersType & params );
  virtual const ParametersType & GetParameters() const;
  virtual bool HasLocalSupport() const;
  virtual void UpdateTransformParameters( DerivativeType & derivative, ParametersValueType factor);

  /** Connect the fixed transform. */
  itkSetObjectMacro(FixedTransform, FixedTransformType);

  /** Get a pointer to the fixed transform.  */
  itkGetConstObjectMacro(FixedTransform, FixedTransformType);

  /** Connect the moving transform. */
  itkSetObjectMacro(MovingTransform, MovingTransformType);

  /** Get a pointer to the moving transform.  */
  itkGetConstObjectMacro(MovingTransform, MovingTransformType);

  /** Connect the moving transform using a backwards-compatible name.
   * This assigns the input transform to the moving transform. */
  void SetTransform( MovingTransformType* transform );

  /** Get the moving transform using a backwards-compatible name */
  const MovingTransformType * GetTransform();

protected:
  ObjectToObjectMetric();
  virtual ~ObjectToObjectMetric();

  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Transforms */
  FixedTransformPointer   m_FixedTransform;
  MovingTransformPointer  m_MovingTransform;

private:
  ObjectToObjectMetric(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkObjectToObjectMetric.hxx"
#endif

#endif
