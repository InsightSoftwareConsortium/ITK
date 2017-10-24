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
#ifndef itkGaussianExponentialDiffeomorphicTransform_h
#define itkGaussianExponentialDiffeomorphicTransform_h

#include "itkConstantVelocityFieldTransform.h"

#include "itkGaussianOperator.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

namespace itk
{

/** \class GaussianExponentialDiffeomorphicTransform
 * \brief Exponential transform using a Gaussian smoothing kernel.
 *
 * Exponential transform inspired by the work of J. Ashburner (see reference
 * below).  Assuming a constant velocity field, the transform takes as input
 * the update field at time point t = 1, \f$u\f$ and smooths it using Gaussian
 * smoothing, \f$S_{update}\f$ defined by \c GaussianSmoothingVarianceForTheUpdateField
 * We add that the current estimate of the velocity field and then perform a
 * second smoothing step such that the new velocity field is
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
class ITK_TEMPLATE_EXPORT GaussianExponentialDiffeomorphicTransform :
  public ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef GaussianExponentialDiffeomorphicTransform                         Self;
  typedef ConstantVelocityFieldTransform<TParametersValueType, NDimensions> Superclass;
  typedef SmartPointer<Self>                                                Pointer;
  typedef SmartPointer<const Self>                                          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( GaussianExponentialDiffeomorphicTransform, ConstantVelocityFieldTransform );

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

  typedef typename Superclass::DisplacementFieldType        DisplacementFieldType;
  typedef typename Superclass::DisplacementFieldPointer     DisplacementFieldPointer;
  typedef typename Superclass::ConstantVelocityFieldType    ConstantVelocityFieldType;
  typedef typename Superclass::ConstantVelocityFieldPointer ConstantVelocityFieldPointer;

  typedef typename DisplacementFieldType::PixelType     DisplacementVectorType;

  /**
   * Update the transform's parameters by the values in \c update. We overwrite the
   * base class implementation as we might want to smooth the update field before
   * adding it to the velocity field
   */
  virtual void UpdateTransformParameters( const DerivativeType & update, ScalarType factor = 1.0 ) ITK_OVERRIDE;

  /** Smooth the velocity field in-place.
   * \warning Not thread safe. Does its own threading.
   */
  virtual ConstantVelocityFieldPointer GaussianSmoothConstantVelocityField( ConstantVelocityFieldType *, ScalarType );

  /**
   * Set/Get Gaussian smoothing parameter for the smoothed velocity field.
   */
  itkSetMacro( GaussianSmoothingVarianceForTheConstantVelocityField, ScalarType );
  itkGetConstMacro( GaussianSmoothingVarianceForTheConstantVelocityField, ScalarType );

  /**
   * Set/Get Gaussian smoothing parameter for the smoothed update field.
   */
  itkSetMacro( GaussianSmoothingVarianceForTheUpdateField, ScalarType );
  itkGetConstMacro( GaussianSmoothingVarianceForTheUpdateField, ScalarType );

protected:
  GaussianExponentialDiffeomorphicTransform();
  virtual ~GaussianExponentialDiffeomorphicTransform() ITK_OVERRIDE;

  /** Type of Gaussian Operator used during smoothing. Define here
   * so we can use a member var during the operation. */
  typedef GaussianOperator<ScalarType, NDimensions> GaussianSmoothingOperatorType;

  typedef VectorNeighborhoodOperatorImageFilter
    <ConstantVelocityFieldType, ConstantVelocityFieldType>
                                                  GaussianSmoothingSmootherType;

  GaussianSmoothingOperatorType                   m_GaussianSmoothingOperator;

  void PrintSelf( std::ostream &, Indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianExponentialDiffeomorphicTransform);

  ScalarType                              m_GaussianSmoothingVarianceForTheUpdateField;
  ScalarType                              m_GaussianSmoothingVarianceForTheConstantVelocityField;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
# include "itkGaussianExponentialDiffeomorphicTransform.hxx"
#endif

#endif // itkGaussianExponentialDiffeomorphicTransform_h
