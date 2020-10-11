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
#ifndef itkBSplineExponentialDiffeomorphicTransformParametersAdaptor_h
#define itkBSplineExponentialDiffeomorphicTransformParametersAdaptor_h

#include "itkConstantVelocityFieldTransformParametersAdaptor.h"

namespace itk
{
/** \class BSplineExponentialDiffeomorphicTransformParametersAdaptor
 * \brief Helper class for multiresolution image registration
 *
 * \author Nick Tustison
 *
 * \ingroup ITKRegistrationCommon
 */
template <typename TTransform>
class ITK_TEMPLATE_EXPORT BSplineExponentialDiffeomorphicTransformParametersAdaptor
  : public ConstantVelocityFieldTransformParametersAdaptor<TTransform>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineExponentialDiffeomorphicTransformParametersAdaptor);

  /** Standard class type aliases. */
  using Self = BSplineExponentialDiffeomorphicTransformParametersAdaptor;
  using Superclass = ConstantVelocityFieldTransformParametersAdaptor<TTransform>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineExponentialDiffeomorphicTransformParametersAdaptor,
               BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor);

  using TransformType = TTransform;
  using ScalarType = typename TransformType::ScalarType;
  using SplineOrderType = typename TransformType::SplineOrderType;
  using ArrayType = typename TransformType::ArrayType;

  /** Dimension of parameters. */
  static constexpr unsigned int SpaceDimension = TransformType::Dimension;

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * velocity field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  virtual void
  SetNumberOfControlPointsForTheConstantVelocityField(const ArrayType &);

  /**
   * Get the control point grid size defining the B-spline estimate of the
   * velocity field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkGetConstMacro(NumberOfControlPointsForTheConstantVelocityField, ArrayType);

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * update field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  virtual void
  SetNumberOfControlPointsForTheUpdateField(const ArrayType &);

  /**
   * Get the control point grid size defining the B-spline estimate of the
   * update field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkGetConstMacro(NumberOfControlPointsForTheUpdateField, ArrayType);

  /**
   * Set the velocity field mesh size which is used to specify the control point
   * grid size.  The mesh size in each dimension is calculated as the
   * difference between the control point grid size and the spline order, i.e.
   * meshSize = controlPointGridSize - SplineOrder.
   */
  void
  SetMeshSizeForTheConstantVelocityField(const ArrayType &);

  /**
   * Set the update field mesh size which is used to specify the control point
   * grid size.  The mesh size in each dimension is calculated as the
   * difference between the control point grid size and the spline order, i.e.
   * meshSize = controlPointGridSize - SplineOrder.
   */
  void
  SetMeshSizeForTheUpdateField(const ArrayType &);

  /**
   * Change the displacement field fixed parameters
   */
  void
  AdaptTransformParameters() override;

protected:
  BSplineExponentialDiffeomorphicTransformParametersAdaptor();
  ~BSplineExponentialDiffeomorphicTransformParametersAdaptor() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ArrayType        m_NumberOfControlPointsForTheConstantVelocityField;
  ModifiedTimeType m_NumberOfControlPointsForTheConstantVelocityFieldSetTime;

  ArrayType        m_NumberOfControlPointsForTheUpdateField;
  ModifiedTimeType m_NumberOfControlPointsForTheUpdateFieldSetTime;

}; // class BSplineExponentialDiffeomorphicTransformParametersAdaptor
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineExponentialDiffeomorphicTransformParametersAdaptor.hxx"
#endif

#endif /* itkBSplineExponentialDiffeomorphicTransformParametersAdaptor_h */
