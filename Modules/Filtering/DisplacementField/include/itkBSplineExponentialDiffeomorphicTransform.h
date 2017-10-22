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
#ifndef itkBSplineExponentialDiffeomorphicTransform_h
#define itkBSplineExponentialDiffeomorphicTransform_h

#include "itkConstantVelocityFieldTransform.h"
#include "itkDisplacementFieldToBSplineImageFilter.h"
#include "itkPointSet.h"

namespace itk
{

/** \class BSplineExponentialDiffeomorphicTransform
 * \brief Exponential transform using B-splines as the smoothing kernel.
 *
 * Exponential transform inspired by the work of J. Ashburner (see reference
 * below).  Assuming a constant velocity field, the transform takes as input
 * the update field at time point t = 1, \f$u\f$ and smooths it using a B-spline
 * smoothing (i.e. fitting) operation, \f$S_{update}\f$ defined by \c SplineOrder and
 * \c NumberOfControlPointsForTheUpdateField.  We add that the current estimate
 * of the velocity field and then perform a second smoothing step such that
 * the new velocity field is
 *
 * \f{eqnarray*}{
 *   v_{new} = S_{velocity}( v_{old} + S_{update}( u ) ).
 * \f}
 *
 * We then exponentiate \f$v_{new}\f$ using the class
 * \c ExponentialDisplacementImageFilter to yield both the forward and inverse
 * displacement fields.
 *
 * \li J. Ashburner. A Fast Diffeomorphic Image Registration Algorithm.
 * NeuroImage, 38(1):95-113, 2007.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKDisplacementField
 */
template<typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT BSplineExponentialDiffeomorphicTransform :
  public ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef BSplineExponentialDiffeomorphicTransform                          Self;
  typedef ConstantVelocityFieldTransform<TParametersValueType, NDimensions> Superclass;
  typedef SmartPointer<Self>                                                Pointer;
  typedef SmartPointer<const Self>                                          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( BSplineExponentialDiffeomorphicTransform, ConstantVelocityFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Dimension of the velocity field . */
  itkStaticConstMacro( ConstantVelocityFieldDimension, unsigned int, NDimensions );

  /** Dimension of the vector spaces. */
  itkStaticConstMacro( Dimension, unsigned int, NDimensions );

  /** Types from superclass */
  typedef typename Superclass::ScalarType               ScalarType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename DerivativeType::ValueType            DerivativeValueType;

  typedef typename Superclass::ParametersType            ParametersType;
  typedef typename Superclass::ParametersValueType       ParametersValueType;
  typedef typename Superclass::FixedParametersType       FixedParametersType;
  typedef typename Superclass::FixedParametersValueType  FixedParametersValueType;

  typedef typename Superclass::DisplacementFieldType        DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldPointer     DisplacementFieldPointer;
  typedef typename Superclass::ConstantVelocityFieldType    ConstantVelocityFieldType;
  typedef typename Superclass::ConstantVelocityFieldPointer ConstantVelocityFieldPointer;

  typedef typename DisplacementFieldType::PixelType     DisplacementVectorType;

  /**
   * typedefs for projecting the input displacement field onto a
   * B-spline field.
   */
  typedef PointSet<ConstantVelocityFieldType, Dimension>                            PointSetType;
  typedef unsigned int                                                              SplineOrderType;
  typedef DisplacementFieldToBSplineImageFilter<ConstantVelocityFieldType>          BSplineFilterType;
  typedef typename BSplineFilterType::WeightsContainerType                          WeightsContainerType;
  typedef typename BSplineFilterType::ArrayType                                     ArrayType;
  typedef typename ArrayType::ValueType                                             ArrayValueType;

  /**
   * Update the transform's parameters by the values in \c update. We overwrite the
   * base class implementation as we might want to smooth the update field before
   * adding it to the velocity field
   */
  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType factor = 1.0 ) ITK_OVERRIDE;

  /**
   * Smooth the constant velocity field in-place.
   */
  virtual ConstantVelocityFieldPointer BSplineSmoothConstantVelocityField( const ConstantVelocityFieldType *, const ArrayType & );

  /**
   * Set/Get the spline order.
   */
  itkSetMacro( SplineOrder, SplineOrderType );
  itkGetConstMacro( SplineOrder, SplineOrderType );

  /**
   * Set/Get the control point grid size defining the B-spline estimate of the
   * smoothed velocity field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkSetMacro( NumberOfControlPointsForTheConstantVelocityField, ArrayType );
  itkGetConstMacro( NumberOfControlPointsForTheConstantVelocityField, ArrayType );

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * smoothed update field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkSetMacro( NumberOfControlPointsForTheUpdateField, ArrayType );
  itkGetConstMacro( NumberOfControlPointsForTheUpdateField, ArrayType );

  /**
   * Set the update field mesh size which is used to specify the control point
   * grid size.  The mesh size in each dimension is calculated as the
   * difference between the control point grid size and the spline order, i.e.
   * meshSize = controlPointGridSize - SplineOrder.
   */
  void SetMeshSizeForTheConstantVelocityField( const ArrayType & );

  /**
   * Set the velocity field mesh size which is used to specify the control point
   * grid size.  The mesh size in each dimension is calculated as the
   * difference between the control point grid size and the spline order, i.e.
   * meshSize = controlPointGridSize - SplineOrder.
   */
  void SetMeshSizeForTheUpdateField( const ArrayType & );

protected:
  BSplineExponentialDiffeomorphicTransform();
  virtual ~BSplineExponentialDiffeomorphicTransform() ITK_OVERRIDE;

  void PrintSelf( std::ostream &, Indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineExponentialDiffeomorphicTransform);

  ArrayType                               m_NumberOfControlPointsForTheConstantVelocityField;
  ArrayType                               m_NumberOfControlPointsForTheUpdateField;

  SplineOrderType                         m_SplineOrder;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
# include "itkBSplineExponentialDiffeomorphicTransform.hxx"
#endif

#endif // itkBSplineExponentialDiffeomorphicTransform_h
