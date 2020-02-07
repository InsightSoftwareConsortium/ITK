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
#ifndef itkPointSetToPointSetMetric_h
#define itkPointSetToPointSetMetric_h

#include "itkImageBase.h"
#include "itkTransform.h"
#include "itkMultipleValuedCostFunction.h"
#include "itkMacro.h"
#include "itkGradientRecursiveGaussianImageFilter.h"

namespace itk
{
/** \class PointSetToPointSetMetric
 * \brief Computes similarity between two point sets.
 *
 * This Class is templated over the type of the two point-sets.  It
 * expects a Transform to be plugged in.  This particular
 * class is the base class for a hierarchy of point-set to point-set metrics.
 *
 * This class computes a value that measures the similarity between the fixed point-set
 * and the transformed moving point-set.
 *
 * \ingroup RegistrationMetrics
 *
 * \ingroup ITKRegistrationCommon
 */

template <typename TFixedPointSet, typename TMovingPointSet>
class ITK_TEMPLATE_EXPORT PointSetToPointSetMetric : public MultipleValuedCostFunction
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointSetToPointSetMetric);

  /** Standard class type aliases. */
  using Self = PointSetToPointSetMetric;
  using Superclass = MultipleValuedCostFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Type used for representing point components  */
  using CoordinateRepresentationType = Superclass::ParametersValueType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToPointSetMetric, MultipleValuedCostFunction);

  /**  Type of the moving Pointset. */
  using MovingPointSetType = TMovingPointSet;
  using MovingPointSetPixelType = typename TMovingPointSet::PixelType;
  using MovingPointSetConstPointer = typename MovingPointSetType::ConstPointer;

  /**  Type of the fixed Pointset. */
  using FixedPointSetType = TFixedPointSet;
  using FixedPointSetConstPointer = typename FixedPointSetType::ConstPointer;

  /** Constants for the pointset dimensions */
  static constexpr unsigned int MovingPointSetDimension = TMovingPointSet::PointDimension;
  static constexpr unsigned int FixedPointSetDimension = TFixedPointSet::PointDimension;

  using FixedPointIterator = typename FixedPointSetType::PointsContainer::ConstIterator;
  using FixedPointDataIterator = typename FixedPointSetType::PointDataContainer::ConstIterator;

  using MovingPointIterator = typename MovingPointSetType::PointsContainer::ConstIterator;
  using MovingPointDataIterator = typename MovingPointSetType::PointDataContainer::ConstIterator;

  /**  Type of the Transform Base class */
  using TransformType =
    Transform<CoordinateRepresentationType, Self::MovingPointSetDimension, Self::FixedPointSetDimension>;

  using TransformPointer = typename TransformType::Pointer;
  using InputPointType = typename TransformType::InputPointType;
  using OutputPointType = typename TransformType::OutputPointType;
  using TransformParametersType = typename TransformType::ParametersType;
  using TransformJacobianType = typename TransformType::JacobianType;

  /**  Type of the measure. */
  using MeasureType = Superclass::MeasureType;

  /**  Type of the derivative. */
  using DerivativeType = Superclass::DerivativeType;

  /**  Type of the parameters. */
  using ParametersType = Superclass::ParametersType;

  /** Get/Set the Fixed Pointset.  */
  itkSetConstObjectMacro(FixedPointSet, FixedPointSetType);
  itkGetConstObjectMacro(FixedPointSet, FixedPointSetType);

  /** Get/Set the Moving Pointset.  */
  itkSetConstObjectMacro(MovingPointSet, MovingPointSetType);
  itkGetConstObjectMacro(MovingPointSet, MovingPointSetType);

  /** Connect the Transform. */
  itkSetObjectMacro(Transform, TransformType);

  /** Get a pointer to the Transform.  */
  itkGetModifiableObjectMacro(Transform, TransformType);

  /** Set the parameters defining the Transform. */
  void
  SetTransformParameters(const ParametersType & parameters) const;

  /** Return the number of parameters required by the Transform */
  unsigned int
  GetNumberOfParameters() const override
  {
    return m_Transform->GetNumberOfParameters();
  }

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  virtual void
  Initialize();

protected:
  PointSetToPointSetMetric();
  ~PointSetToPointSetMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  FixedPointSetConstPointer m_FixedPointSet;

  MovingPointSetConstPointer m_MovingPointSet;

  mutable TransformPointer m_Transform;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointSetToPointSetMetric.hxx"
#endif

#endif
