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
#ifndef __itkBSplineDisplacementFieldTransform_h
#define __itkBSplineDisplacementFieldTransform_h

#include "itkDisplacementFieldTransform.h"

#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkPointSet.h"

namespace itk
{

/** \class BSplineDisplacementFieldTransform
 *
 * \ingroup ITKTransform
 */
template
  <class TScalar, unsigned int NDimensions>
class ITK_EXPORT BSplineDisplacementFieldTransform :
  public DisplacementFieldTransform<TScalar, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef BSplineDisplacementFieldTransform                 Self;
  typedef DisplacementFieldTransform<TScalar, NDimensions>  Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( BSplineDisplacementFieldTransform, Transform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Dimension of the domain spaces. */
  itkStaticConstMacro( Dimension, unsigned int, NDimensions );

  /** Use the displacement field type */
  typedef typename Superclass::DisplacementFieldType DisplacementFieldType;

  /**
   * typedefs for projecting the input displacement field onto a
   * B-spline field.
   */
  typedef typename DisplacementFieldType::PixelType DisplacementVectorType;
  typedef DisplacementFieldType                     ControlPointLatticeType;
  typedef PointSet<DisplacementVectorType,
    itkGetStaticConstMacro( Dimension )>           PointSetType;
  typedef BSplineScatteredDataPointSetToImageFilter
    <PointSetType, DisplacementFieldType>           BSplineFilterType;
  typedef typename BSplineFilterType::PointDataImageType
                                                   DisplacementFieldControlPointLatticeType;
  typedef typename BSplineFilterType::ArrayType    ArrayType;

  /** Get/Set the displacement field. */
  virtual void SetDisplacementField( DisplacementFieldType * );

  /**
   * Set the spline order defining the bias field estimate.  Default = 3.
   */
  itkSetMacro( SplineOrder, unsigned int );

  /**
   * Get the spline order defining the bias field estimate.  Default = 3.
   */
  itkGetConstMacro( SplineOrder, unsigned int );

  /**
   * Set the control point grid size definining the B-spline estimate of the
   * scalar bias field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkSetMacro( NumberOfControlPoints, ArrayType );

  /**
   * Get the control point grid size definining the B-spline estimate of the
   * scalar bias field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkGetConstMacro( NumberOfControlPoints, ArrayType );

  /**
   * Set the mesh size which is another way of specifying the control point
   * grid size.  The mesh size in each dimension is calculated as the
   * difference
   * between the control point grid size and the spline order, i.e.
   * meshSize = controlPointGridSize - SplineOrder.
   */
  void SetMeshSize( const ArrayType meshSize )
    {
    ArrayType numberOfControlPoints;
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      numberOfControlPoints[d] = meshSize[d] + this->m_SplineOrder;
      }
    this->SetNumberOfControlPoints( numberOfControlPoints );
    }

  /**
   * Set the number of fitting levels.  One of the contributions of N4 is the
   * introduction of a multi-scale approach to fitting. This allows one to
   * specify a B-spline mesh size for initial fitting followed by a doubling of
   * the mesh resolution for each subsequent fitting level.  Default = 3 levels.
   */
  itkSetMacro( NumberOfFittingLevels, ArrayType );

  /**
   * Set the number of fitting levels.  One of the contributions of N4 is the
   * introduction of a multi-scale approach to fitting. This allows one to
   * specify a B-spline mesh size for initial fitting followed by a doubling of
   * the mesh resolution for each subsequent fitting level.  Default = 3 levels.
   */
  void SetNumberOfFittingLevels( const unsigned int n )
    {
    ArrayType nlevels;

    nlevels.Fill( n );
    this->SetNumberOfFittingLevels( nlevels );
    }

  /**
   * Get the number of fitting levels.  One of the contributions is the
   * introduction of a multi-scale approach to fitting. This allows one to
   * specify a B-spline mesh size for initial fitting followed by a doubling of
   * the mesh resolution for each subsequent fitting level.
   * Default = 3 levels.
   */
  itkGetConstMacro( NumberOfFittingLevels, ArrayType );


  /**
   * Boolean value to calculate an approximate inverse
   * of the displacement field.
   */
  itkSetMacro( CalculateApproximateInverseDisplacementField, bool );

  /**
   * Boolean value to calculate an approximate inverse of the
   * displacement field.
   */
  itkGetConstMacro( CalculateApproximateInverseDisplacementField, bool );

  /**
   * Boolean value to calculate an approximate inverse of the
   * displacement field.
   */
  itkBooleanMacro( CalculateApproximateInverseDisplacementField );

protected:
  BSplineDisplacementFieldTransform();
  virtual ~BSplineDisplacementFieldTransform();
  void PrintSelf( std::ostream& os, Indent indent ) const;

  bool                   m_CalculateApproximateInverseDisplacementField;

  typename DisplacementFieldControlPointLatticeType::Pointer
                                        m_DisplacementFieldControlPointLattice;
  typename DisplacementFieldControlPointLatticeType::Pointer
                                 m_InverseDisplacementFieldControlPointLattice;

  unsigned int                                m_SplineOrder;
  ArrayType                                   m_NumberOfControlPoints;
  ArrayType                                   m_NumberOfFittingLevels;

private:
  BSplineDisplacementFieldTransform( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

} // end namespace itk

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkBSplineDisplacementFieldTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkBSplineDisplacementFieldTransform.hxx"
#endif

#endif // __itkBSplineDisplacementFieldTransform_h
