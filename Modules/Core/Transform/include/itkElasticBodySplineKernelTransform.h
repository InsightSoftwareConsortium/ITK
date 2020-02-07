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
#ifndef itkElasticBodySplineKernelTransform_h
#define itkElasticBodySplineKernelTransform_h

#include "itkKernelTransform.h"

namespace itk
{
/** \class ElasticBodySplineKernelTransform
 * \brief This class defines the elastic body spline (EBS) transformation.
 *
 * This class defines the elastic body spline (EBS) transformation.
 * It is implemented in as straightforward a manner as possible from
 * the IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 * Taken from the paper:
 * The EBS "is based on a physical model of a homogeneous, isotropic,
 * three-dimensional elastic body. The model can approximate the way
 * that some physical objects deform".
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double, unsigned int NDimensions = 3>
class ITK_TEMPLATE_EXPORT ElasticBodySplineKernelTransform : public KernelTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ElasticBodySplineKernelTransform);

  /** Standard class type aliases. */
  using Self = ElasticBodySplineKernelTransform;
  using Superclass = KernelTransform<TParametersValueType, NDimensions>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ElasticBodySplineKernelTransform, KernelTransform);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Scalar type. */
  using ScalarType = typename Superclass::ScalarType;

  /** Parameters type. */
  using ParametersType = typename Superclass::ParametersType;
  using FixedParametersType = typename Superclass::FixedParametersType;

  /** Jacobian type. */
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;

  /** Dimension of the domain space. */
  static constexpr unsigned int SpaceDimension = Superclass::SpaceDimension;

  /** Set alpha.  Alpha is related to Poisson's Ratio (\f$\nu\f$) as
   * \f$\alpha = 12 ( 1 - \nu ) - 1\f$
   */
  itkSetMacro(Alpha, TParametersValueType);

  /** Get alpha */
  itkGetConstMacro(Alpha, TParametersValueType);

  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;

protected:
  ElasticBodySplineKernelTransform();
  ~ElasticBodySplineKernelTransform() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using GMatrixType = typename Superclass::GMatrixType;
  /** Compute G(x)
   * For the elastic body spline, this is:
   * \f$ G(x) = [alpha*r(x)^2*I - 3*x*x']*r(x) \f$
   * \f$ G(x) = [\alpha*r(x)^2*I - 3*x*x']*r(x) \f$
   * where
   * \f$\alpha = 12 ( 1 - \nu ) - 1\f$
   * \f$\nu\f$ is Poisson's Ratio
   * \f$ r(x) = Euclidean norm = sqrt[x1^2 + x2^2 + x3^2] \f$
   * \f[ r(x) = \sqrt{ x_1^2 + x_2^2 + x_3^2 }  \f]
   * I = identity matrix
   */
  void
  ComputeG(const InputVectorType & landmarkVector, GMatrixType & gmatrix) const override;

  /** alpha,  Alpha is related to Poisson's Ratio (\f$\nu\f$) as
   * \f$ \alpha = 12 ( 1 - \nu ) - 1\f$
   */
  TParametersValueType m_Alpha;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkElasticBodySplineKernelTransform.hxx"
#endif

#endif // itkElasticBodySplineKernelTransform_h
