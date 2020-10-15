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
template <typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT GaussianExponentialDiffeomorphicTransform
  : public ConstantVelocityFieldTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GaussianExponentialDiffeomorphicTransform);

  /** Standard class type aliases. */
  using Self = GaussianExponentialDiffeomorphicTransform;
  using Superclass = ConstantVelocityFieldTransform<TParametersValueType, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianExponentialDiffeomorphicTransform, ConstantVelocityFieldTransform);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Dimension of the velocity field . */
  static constexpr unsigned int ConstantVelocityFieldDimension = NDimensions;

  /** Dimension of the vector spaces. */
  static constexpr unsigned int Dimension = NDimensions;

  /** Types from superclass */
  using ScalarType = typename Superclass::ScalarType;
  using DerivativeType = typename Superclass::DerivativeType;
  using DerivativeValueType = typename DerivativeType::ValueType;

  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename Superclass::DisplacementFieldPointer;
  using ConstantVelocityFieldType = typename Superclass::ConstantVelocityFieldType;
  using ConstantVelocityFieldPointer = typename Superclass::ConstantVelocityFieldPointer;

  using DisplacementVectorType = typename DisplacementFieldType::PixelType;

  /**
   * Update the transform's parameters by the values in \c update. We overwrite the
   * base class implementation as we might want to smooth the update field before
   * adding it to the velocity field
   */
  void
  UpdateTransformParameters(const DerivativeType & update, ScalarType factor = 1.0) override;

  /** Smooth the velocity field in-place.
   * \warning Not thread safe. Does its own threading.
   */
  virtual ConstantVelocityFieldPointer
  GaussianSmoothConstantVelocityField(ConstantVelocityFieldType *, ScalarType);

  /**
   * Set/Get Gaussian smoothing parameter for the smoothed velocity field.
   */
  itkSetMacro(GaussianSmoothingVarianceForTheConstantVelocityField, ScalarType);
  itkGetConstMacro(GaussianSmoothingVarianceForTheConstantVelocityField, ScalarType);

  /**
   * Set/Get Gaussian smoothing parameter for the smoothed update field.
   */
  itkSetMacro(GaussianSmoothingVarianceForTheUpdateField, ScalarType);
  itkGetConstMacro(GaussianSmoothingVarianceForTheUpdateField, ScalarType);

protected:
  GaussianExponentialDiffeomorphicTransform();
  ~GaussianExponentialDiffeomorphicTransform() override = default;

  /** Type of Gaussian Operator used during smoothing. Define here
   * so we can use a member var during the operation. */
  using GaussianSmoothingOperatorType = GaussianOperator<ScalarType, NDimensions>;

  using GaussianSmoothingSmootherType =
    VectorNeighborhoodOperatorImageFilter<ConstantVelocityFieldType, ConstantVelocityFieldType>;

  GaussianSmoothingOperatorType m_GaussianSmoothingOperator;

  void
  PrintSelf(std::ostream &, Indent) const override;

private:
  ScalarType m_GaussianSmoothingVarianceForTheUpdateField;
  ScalarType m_GaussianSmoothingVarianceForTheConstantVelocityField;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianExponentialDiffeomorphicTransform.hxx"
#endif

#endif // itkGaussianExponentialDiffeomorphicTransform_h
