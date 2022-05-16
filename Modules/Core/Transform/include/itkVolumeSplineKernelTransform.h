/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkVolumeSplineKernelTransform_h
#define itkVolumeSplineKernelTransform_h

#include "itkKernelTransform.h"

namespace itk
{
/** \class VolumeSplineKernelTransform
 * This class defines the thin plate spline (TPS) transformation.
 * It is implemented in as straightforward a manner as possible from
 * the IEEE TMI paper by Davis, Khotanzad, Flamig, and Harms,
 * Vol. 16 No. 3 June 1997
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType, unsigned int VDimension = 3>
// Number of dimensions
class ITK_TEMPLATE_EXPORT VolumeSplineKernelTransform : public KernelTransform<TParametersValueType, VDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VolumeSplineKernelTransform);

  /** Standard class type aliases. */
  using Self = VolumeSplineKernelTransform;
  using Superclass = KernelTransform<TParametersValueType, VDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VolumeSplineKernelTransform, KernelTransform);

  /** Scalar type. */
  using typename Superclass::ScalarType;

  /** Parameters type. */
  using typename Superclass::ParametersType;
  using typename Superclass::FixedParametersType;

  /** Jacobian Type */
  using typename Superclass::JacobianType;
  using typename Superclass::JacobianPositionType;
  using typename Superclass::InverseJacobianPositionType;

  /** Dimension of the domain space. */
  static constexpr unsigned int SpaceDimension = Superclass::SpaceDimension;

  /** These (rather redundant) type alias are needed because type alias are not inherited */
  using typename Superclass::InputPointType;
  using typename Superclass::OutputPointType;
  using typename Superclass::InputVectorType;
  using typename Superclass::OutputVectorType;
  using typename Superclass::InputCovariantVectorType;
  using typename Superclass::OutputCovariantVectorType;
  using typename Superclass::PointsIterator;

protected:
  VolumeSplineKernelTransform() = default;
  ~VolumeSplineKernelTransform() override = default;

  /** These (rather redundant) type alias are needed because on type alias are not inherited. */
  using typename Superclass::GMatrixType;

  /** Compute G(x)
   * For the volume plate spline, this is:
   * G(x) = r(x)^3*I
   * \f$ G(x) = r(x)^3*I \f$
   * where
   * r(x) = Euclidean norm = sqrt[x1^2 + x2^2 + x3^2]
   * \f[ r(x) = \sqrt{ x_1^2 + x_2^2 + x_3^2 }  \f]
   * I = identity matrix. */
  void
  ComputeG(const InputVectorType & x, GMatrixType & gmatrix) const override;

  /** Compute the contribution of the landmarks weighted by the kernel
   *  function to the global deformation of the space  */
  void
  ComputeDeformationContribution(const InputPointType & thisPoint, OutputPointType & result) const override;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVolumeSplineKernelTransform.hxx"
#endif

#endif // itkVolumeSplineKernelTransform_h
