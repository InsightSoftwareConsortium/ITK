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
#ifndef __itkBSplineExponentialDiffeomorphicTransform_h
#define __itkBSplineExponentialDiffeomorphicTransform_h

#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransform.h"

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
 * \ref J. Ashburner. A Fast Diffeomorphic Image Registration Algorithm.
 * NeuroImage, 38(1):95-113, 2007.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKDisplacementField
 */
template<class TScalar, unsigned int NDimensions>
class ITK_EXPORT BSplineExponentialDiffeomorphicTransform :
  public  BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef BSplineExponentialDiffeomorphicTransform                                   Self;
  typedef BSplineSmoothingOnUpdateDisplacementFieldTransform<TScalar, NDimensions>   Superclass;
  typedef DisplacementFieldTransform<TScalar, NDimensions>                           SuperSuperclass;
  typedef SmartPointer<Self>                                                         Pointer;
  typedef SmartPointer<const Self>                                                   ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( BSplineExponentialDiffeomorphicTransform, BSplineSmoothingOnUpdateDisplacementFieldTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Dimension of the domain spaces. */
  itkStaticConstMacro( Dimension, unsigned int, NDimensions );

  /** Types from superclass */
  typedef typename Superclass::ScalarType               ScalarType;
  typedef typename Superclass::ArrayType                ArrayType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename DerivativeType::ValueType            DerivativeValueType;
  typedef typename Superclass::DisplacementFieldType    DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldPointer DisplacementFieldPointer;
  typedef typename Superclass::DisplacementFieldType    ConstantVelocityFieldType;
  typedef typename Superclass::DisplacementFieldPointer ConstantVelocityFieldPointer;
  typedef typename Superclass::DisplacementVectorType   DisplacementVectorType;

  typedef typename Transform<TScalar,NDimensions,NDimensions>::Pointer TransformPointer;

  /**
   * Update the transform's parameters by the values in \c update.  We
   * assume \c update is of the same length as Parameters. Throw exception
   * otherwise. The update process performs an smoothing on the velocity field
   * field by using BSplines.
   * \c factor is a scalar multiplier for each value in update.
   * \c BSplineSmoothDisplacementField is called after the update is
   * added to the field.
   * See base class for more details.
   */
  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType factor = 1.0 );

  /**
   * Smooth the constant velocity field in-place.
   */
  virtual ConstantVelocityFieldPointer BSplineSmoothConstantVelocityField( ConstantVelocityFieldType * field, ScalarType scalar )
    {
    return this->BSplineSmoothDisplacementField( field, scalar );
    }

  /**
   * Set the control point grid size defining the B-spline estimate of the
   * smoothed velocity field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkSetMacro( NumberOfControlPointsForTheVelocityField, ArrayType );

  /**
   * Get the control point grid size defining the B-spline estimate of the smoothing
   * of the velocity field.  In each dimension, the B-spline mesh size is equal
   * to the number of control points in that dimension minus the spline order.
   * Default = 4 control points in each dimension for a mesh size of 1 in each
   * dimension.
   */
  itkGetConstMacro( NumberOfControlPointsForTheVelocityField, ArrayType );

  // Set/get compute inverse
  itkSetMacro( ComputeInverse, bool );
  itkGetConstMacro( ComputeInverse, bool );
  itkBooleanMacro( ComputeInverse );

  // Set/get compute number of exp. iterations automatically
  itkSetMacro( CalculateNumberOfIntegrationStepsAutomatically, bool );
  itkGetConstMacro( CalculateNumberOfIntegrationStepsAutomatically, bool );
  itkBooleanMacro( CalculateNumberOfIntegrationStepsAutomatically );

  // Set/get the number of iterations (valid only if CalculateNumberOfIterationsAutomatically = false)
  itkSetMacro( NumberOfIntegrationSteps, unsigned int );
  itkGetConstMacro( NumberOfIntegrationSteps, unsigned int );

protected:
  BSplineExponentialDiffeomorphicTransform();
  virtual ~BSplineExponentialDiffeomorphicTransform();

  void PrintSelf( std::ostream &, Indent ) const;

private:
  BSplineExponentialDiffeomorphicTransform( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  bool                                    m_CalculateNumberOfIntegrationStepsAutomatically;
  unsigned int                            m_NumberOfIntegrationSteps;

  bool                                    m_ComputeInverse;

  ConstantVelocityFieldPointer            m_ConstantVelocityField;

  ArrayType                               m_NumberOfControlPointsForTheVelocityField;
};

} // end namespace itk

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkBSplineExponentialDiffeomorphicTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkBSplineExponentialDiffeomorphicTransform.hxx"
#endif

#endif // __itkBSplineExponentialDiffeomorphicTransform_h
