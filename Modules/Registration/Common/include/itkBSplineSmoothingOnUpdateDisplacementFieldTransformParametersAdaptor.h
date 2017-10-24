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
#ifndef itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_h
#define itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_h

#include "itkDisplacementFieldTransformParametersAdaptor.h"

namespace itk
{
/** \class BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor
 * \brief Helper class for multiresolution image registration
 *
 * \author Nick Tustison
 *
 * \ingroup ITKRegistrationCommon
 */
template<typename TTransform>
class ITK_TEMPLATE_EXPORT BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor
: public DisplacementFieldTransformParametersAdaptor<TTransform>
{
public:

  /** Standard class typedefs. */
  typedef BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor          Self;
  typedef DisplacementFieldTransformParametersAdaptor<TTransform>                      Superclass;
  typedef SmartPointer<Self>                                                           Pointer;
  typedef SmartPointer<const Self>                                                     ConstPointer;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor,
    DisplacementFieldTransformParametersAdaptor );

  typedef TTransform                               TransformType;
  typedef typename TransformType::ScalarType       ScalarType;
  typedef typename TransformType::SplineOrderType  SplineOrderType;
  typedef typename TransformType::ArrayType        ArrayType;

  /** Dimension of parameters. */
  itkStaticConstMacro( SpaceDimension, unsigned int, TransformType::Dimension );

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * update field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  virtual void SetNumberOfControlPointsForTheUpdateField( const ArrayType & );

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
  virtual void SetNumberOfControlPointsForTheTotalField( const ArrayType & );

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
   * Change the displacement field fixed parameters
   */
  virtual void AdaptTransformParameters() ITK_OVERRIDE;

protected:
  BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor();
  ~BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor() ITK_OVERRIDE;

  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor);

  ArrayType                   m_NumberOfControlPointsForTheUpdateField;
  ArrayType                   m_NumberOfControlPointsForTheTotalField;
  ModifiedTimeType            m_NumberOfControlPointsForTheUpdateFieldSetTime;
  ModifiedTimeType            m_NumberOfControlPointsForTheTotalFieldSetTime;


}; //class BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.hxx"
#endif

#endif /* itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_h */
