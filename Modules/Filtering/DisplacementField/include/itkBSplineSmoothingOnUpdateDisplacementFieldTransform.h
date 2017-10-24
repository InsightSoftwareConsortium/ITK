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
template<typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT BSplineSmoothingOnUpdateDisplacementFieldTransform :
  public DisplacementFieldTransform<TParametersValueType, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef BSplineSmoothingOnUpdateDisplacementFieldTransform            Self;
  typedef DisplacementFieldTransform<TParametersValueType, NDimensions> Superclass;
  typedef SmartPointer<Self>                                            Pointer;
  typedef SmartPointer<const Self>                                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( BSplineSmoothingOnUpdateDisplacementFieldTransform, DisplacementFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Dimension of the domain spaces. */
  itkStaticConstMacro( Dimension, unsigned int, NDimensions );

  /** Types from superclass */
  typedef typename Superclass::ScalarType                    ScalarType;
  typedef typename Superclass::DerivativeType                DerivativeType;
  typedef typename DerivativeType::ValueType                 DerivativeValueType;
  typedef typename Superclass::DisplacementFieldType         DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldPointer      DisplacementFieldPointer;
  typedef typename Superclass::DisplacementFieldConstPointer DisplacementFieldConstPointer;

  typedef typename Transform<TParametersValueType,NDimensions, NDimensions>::Pointer
             TransformPointer;

  /**
   * typedefs for projecting the input displacement field onto a
   * B-spline field.
   */
  typedef typename DisplacementFieldType::PixelType                            DisplacementVectorType;
  typedef PointSet<DisplacementVectorType, Dimension>                          PointSetType;
  typedef unsigned int                                                         SplineOrderType;
  typedef DisplacementFieldToBSplineImageFilter<DisplacementFieldType>         BSplineFilterType;
  typedef typename BSplineFilterType::WeightsContainerType                     WeightsContainerType;
  typedef DisplacementFieldType                                                DisplacementFieldControlPointLatticeType;
  typedef typename BSplineFilterType::ArrayType                                ArrayType;
  typedef typename ArrayType::ValueType                                        ArrayValueType;

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
  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType factor = 1.0 ) ITK_OVERRIDE;

  /**
   * Set the spline order defining the bias field estimate.  Default = 3.
   */
  itkSetMacro( SplineOrder, SplineOrderType );

  /**
   * Get the spline order defining the displacement field estimate.  Default = 3.
   */
  itkGetConstMacro( SplineOrder, SplineOrderType );

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * update field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkSetMacro( NumberOfControlPointsForTheUpdateField, ArrayType );

  /**
   * Get the control point grid size defining the B-spline estimate of the
   * update field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkGetConstMacro( NumberOfControlPointsForTheUpdateField, ArrayType );

  /**
   * Set the update field mesh size which is used to specify the control point
   * grid size.  The mesh size in each dimension is calculated as the
   * difference between the control point grid size and the spline order, i.e.
   * meshSize = controlPointGridSize - SplineOrder.
   */
  void SetMeshSizeForTheUpdateField( const ArrayType & );

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * total field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkSetMacro( NumberOfControlPointsForTheTotalField, ArrayType );

  /**
   * Get the control point grid size defining the B-spline estimate of the
   * scalar bias field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkGetConstMacro( NumberOfControlPointsForTheTotalField, ArrayType );

  /**
   * Set the total field mesh size which is used to specify the control point
   * grid size.  The mesh size in each dimension is calculated as the
   * difference between the control point grid size and the spline order, i.e.
   * meshSize = controlPointGridSize - SplineOrder.
   */
  void SetMeshSizeForTheTotalField( const ArrayType & );

  /**
   * Enforce stationary boundaries.  Important for diffeomorphic transforms.
   */
  itkBooleanMacro( EnforceStationaryBoundary );
  itkSetMacro( EnforceStationaryBoundary, bool );
  itkGetConstMacro( EnforceStationaryBoundary, bool );

protected:
  BSplineSmoothingOnUpdateDisplacementFieldTransform();
  virtual ~BSplineSmoothingOnUpdateDisplacementFieldTransform() ITK_OVERRIDE;

  void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  /** Clone the current transform */
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

  /**
   * Smooth the displacement field using B-splines.
   */
   DisplacementFieldPointer BSplineSmoothDisplacementField( const DisplacementFieldType *, const ArrayType & );

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineSmoothingOnUpdateDisplacementFieldTransform);

  SplineOrderType             m_SplineOrder;
  bool                        m_EnforceStationaryBoundary;
  ArrayType                   m_NumberOfControlPointsForTheUpdateField;
  ArrayType                   m_NumberOfControlPointsForTheTotalField;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
# include "itkBSplineSmoothingOnUpdateDisplacementFieldTransform.hxx"
#endif

#endif // itkBSplineSmoothingOnUpdateDisplacementFieldTransform_h
