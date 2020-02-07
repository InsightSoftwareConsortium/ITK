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
#ifndef itkBSplineSmoothingOnUpdateDisplacementFieldTransform_h
#define itkBSplineSmoothingOnUpdateDisplacementFieldTransform_h

#include "itkDisplacementFieldTransform.h"

#include "itkDisplacementFieldToBSplineImageFilter.h"
#include "itkPointSet.h"

namespace itk
{

/** \class BSplineSmoothingOnUpdateDisplacementFieldTransform
 * \brief Representation of a smooth deformation field  with B-splines.
 *
 * Although there already exists a B-spline transform in ITK which can be used
 * for processes such as image registration, if these processes involve a dense
 * sampling of an image a significant computational speed-up can be achieved
 * by densely sampling the B-spline transform prior to invoking transformations.
 *
 * This class takes as input a displacement field, smooths it on demand using
 * the specified B-spline parameters.  This represents an alternative approach
 * to B-spline (FFD) registration and is explained more in detail in the
 * reference given below.
 *
 * \author Nicholas J. Tustison
 *
 * \par REFERENCE
 * NJ Tustison, BB Avants, JC Gee, "Directly Manipulated Free-Form Deformation
 * Image Registration", IEEE Transactions on Image Processing, 18(3):624-635,
 * 2009.
 *
 * \ingroup ITKDisplacementField
 */
template <typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT BSplineSmoothingOnUpdateDisplacementFieldTransform
  : public DisplacementFieldTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineSmoothingOnUpdateDisplacementFieldTransform);

  /** Standard class type aliases. */
  using Self = BSplineSmoothingOnUpdateDisplacementFieldTransform;
  using Superclass = DisplacementFieldTransform<TParametersValueType, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineSmoothingOnUpdateDisplacementFieldTransform, DisplacementFieldTransform);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Dimension of the domain spaces. */
  static constexpr unsigned int Dimension = NDimensions;

  /** Types from superclass */
  using ScalarType = typename Superclass::ScalarType;
  using DerivativeType = typename Superclass::DerivativeType;
  using DerivativeValueType = typename DerivativeType::ValueType;
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename Superclass::DisplacementFieldPointer;
  using DisplacementFieldConstPointer = typename Superclass::DisplacementFieldConstPointer;

  using TransformPointer = typename Transform<TParametersValueType, NDimensions, NDimensions>::Pointer;

  /**
   * type alias for projecting the input displacement field onto a
   * B-spline field.
   */
  using DisplacementVectorType = typename DisplacementFieldType::PixelType;
  using PointSetType = PointSet<DisplacementVectorType, Dimension>;
  using SplineOrderType = unsigned int;
  using BSplineFilterType = DisplacementFieldToBSplineImageFilter<DisplacementFieldType>;
  using WeightsContainerType = typename BSplineFilterType::WeightsContainerType;
  using DisplacementFieldControlPointLatticeType = DisplacementFieldType;
  using ArrayType = typename BSplineFilterType::ArrayType;
  using ArrayValueType = typename ArrayType::ValueType;

  /**
   * Update the transform's parameters by the values in \c update.  We
   * assume \c update is of the same length as Parameters. Throw exception
   * otherwise. The update process performs an smoothing on the displacement
   * field by using BSplines.
   * \c factor is a scalar multiplier for each value in update.
   * \c BSplineSmoothDisplacementField is called after the update is
   * added to the field.
   * See base class for more details.
   */
  void
  UpdateTransformParameters(const DerivativeType & update, ScalarType factor = 1.0) override;

  /**
   * Set the spline order defining the bias field estimate.  Default = 3.
   */
  itkSetMacro(SplineOrder, SplineOrderType);

  /**
   * Get the spline order defining the displacement field estimate.  Default = 3.
   */
  itkGetConstMacro(SplineOrder, SplineOrderType);

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * update field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkSetMacro(NumberOfControlPointsForTheUpdateField, ArrayType);

  /**
   * Get the control point grid size defining the B-spline estimate of the
   * update field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkGetConstMacro(NumberOfControlPointsForTheUpdateField, ArrayType);

  /**
   * Set the update field mesh size which is used to specify the control point
   * grid size.  The mesh size in each dimension is calculated as the
   * difference between the control point grid size and the spline order, i.e.
   * meshSize = controlPointGridSize - SplineOrder.
   */
  void
  SetMeshSizeForTheUpdateField(const ArrayType &);

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * total field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkSetMacro(NumberOfControlPointsForTheTotalField, ArrayType);

  /**
   * Get the control point grid size defining the B-spline estimate of the
   * scalar bias field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkGetConstMacro(NumberOfControlPointsForTheTotalField, ArrayType);

  /**
   * Set the total field mesh size which is used to specify the control point
   * grid size.  The mesh size in each dimension is calculated as the
   * difference between the control point grid size and the spline order, i.e.
   * meshSize = controlPointGridSize - SplineOrder.
   */
  void
  SetMeshSizeForTheTotalField(const ArrayType &);

  /**
   * Enforce stationary boundaries.  Important for diffeomorphic transforms.
   */
  itkBooleanMacro(EnforceStationaryBoundary);
  itkSetMacro(EnforceStationaryBoundary, bool);
  itkGetConstMacro(EnforceStationaryBoundary, bool);

protected:
  BSplineSmoothingOnUpdateDisplacementFieldTransform();
  ~BSplineSmoothingOnUpdateDisplacementFieldTransform() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Clone the current transform */
  typename LightObject::Pointer
  InternalClone() const override;

  /**
   * Smooth the displacement field using B-splines.
   */
  DisplacementFieldPointer
  BSplineSmoothDisplacementField(const DisplacementFieldType *, const ArrayType &);

private:
  SplineOrderType m_SplineOrder{ 3 };
  bool            m_EnforceStationaryBoundary{ true };
  ArrayType       m_NumberOfControlPointsForTheUpdateField;
  ArrayType       m_NumberOfControlPointsForTheTotalField;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineSmoothingOnUpdateDisplacementFieldTransform.hxx"
#endif

#endif // itkBSplineSmoothingOnUpdateDisplacementFieldTransform_h
