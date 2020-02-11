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
#ifndef itkEuler2DTransform_h
#define itkEuler2DTransform_h

#include <iostream>
#include "itkRigid2DTransform.h"

namespace itk
{
/** \class Euler2DTransform
 *
 * \brief Euler2DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rigid transformation is 2D space.
 * The transform is specified as a rotation around arbitrary center
 * and is followed by a translation.
 *
 * This transform is basically is a synonym for Rigid2DTransform.
 *
 * \sa Rigid2DTransform
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT Euler2DTransform : public Rigid2DTransform<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(Euler2DTransform);

  /** Standard class type aliases. */
  using Self = Euler2DTransform;
  using Superclass = Rigid2DTransform<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Euler2DTransform, Rigid2DTransform);

  /** Dimension of parameters. */
  static constexpr unsigned int SpaceDimension = 2;
  static constexpr unsigned int ParametersDimension = 3;


  /** Parameters type. */
  using ScalarType = typename Superclass::ScalarType;
  using ParametersType = typename Superclass::ParametersType;
  using FixedParametersType = typename Superclass::FixedParametersType;

  /** Jacobian type. */
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;

  /** Point type. */
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;

  /** Vector type. */
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;

  /** CovariantVector type. */
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;

  /** VnlVector type. */
  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;
  using MatrixType = typename Superclass::MatrixType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost.*/
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  /**
   * This method creates and returns a new Euler2DTransform object
   * which is the inverse of self.
   */
  void
  CloneInverseTo(Pointer & newinverse) const;

  /** Get an inverse of this transform. */
  bool
  GetInverse(Self * inverse) const;

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override;

  /**
   * This method creates and returns a new Euler2DTransform object
   * which has the same parameters as self.
   */
  void
  CloneTo(Pointer & clone) const;

  /**
   * Update the angle from the underlying matrix. This method
   * is old and is retained for backward compatibility.
   */
  void
  ComputeAngleFromMatrix()
  {
    this->ComputeMatrixParameters();
  }

protected:
  Euler2DTransform(unsigned int parametersDimension);
  Euler2DTransform();
  ~Euler2DTransform() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
}; // class Euler2DTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkEuler2DTransform.hxx"
#endif

#endif /* itkEuler2DTransform_h */
