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
#ifndef itkv3Rigid3DTransform_h
#define itkv3Rigid3DTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"
#include "itkVersor.h"

namespace itk
{
namespace v3
{
/** \class Rigid3DTransform
 * \brief ITK3.x compatible Rigid3DTransform of a vector space (e.g. space coordinates)
 *
 * NOTE: In ITK4, the itkNewMacro() was removed from
 * itk::Rigid3DTransform. This class, itkv3::Rigid3DTransform provides
 * the ITK3.x functionality. The purpose of the class is provide ITKv3
 * functionality with backward compatibility after ITK 5.0 removal of
 * ITKV3_COMPATIBILITY support.
 *
 * Even though the name Rigid3DTransform is conceptually closer to
 * what a user may expect, the VersorRigid3DTransform is often a
 * much better transform to use during optimization procedures from
 * both a speed perspective (lower dimensional parameter space), and
 * stability standpoint (versors do not suffer from rotational gimble
 * lock).
 *
 * This transform applies a rotation and translation in 3D space.
 * The transform is specified as a rotation matrix around a arbitrary center
 * and is followed by a translation.
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 12 elements.
 * The first 9 parameters represents the rotation matrix in row-major order
 * (where the column index varies the fastest). The last 3 parameters defines
 * the translation in each dimension.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation in each dimension.
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double>
class ITK_TEMPLATE_EXPORT Rigid3DTransform : public itk::Rigid3DTransform<TParametersValueType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(Rigid3DTransform);

  /** Standard class type aliases. */
  using Self = Rigid3DTransform;
  using Superclass = itk::Rigid3DTransform<TParametersValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;


  /** Run-time type information (and related methods). */
  itkTypeMacro(Rigid3DTransform, Rigid3DTransform);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the space. */
  static constexpr unsigned int SpaceDimension = 3;
  static constexpr unsigned int InputSpaceDimension = 3;
  static constexpr unsigned int OutputSpaceDimension = 3;
  static constexpr unsigned int ParametersDimension = 12;

  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename Superclass::ParametersValueType;
  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename Superclass::FixedParametersValueType;
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;
  using ScalarType = typename Superclass::ScalarType;
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using OutputVectorValueType = typename Superclass::OutputVectorValueType;
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;
  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;
  using MatrixType = typename Superclass::MatrixType;
  using InverseMatrixType = typename Superclass::InverseMatrixType;
  using MatrixValueType = typename Superclass::MatrixValueType;
  using CenterType = typename Superclass::CenterType;
  using TranslationType = typename Superclass::TranslationType;
  using OffsetType = typename Superclass::OffsetType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  /** Get an inverse of this transform. */
  bool
  GetInverse(Self * inverse) const
  {
    return this->Superclass::GetInverse(inverse);
  }

  /** Return an inverse of this transform. */
  InverseTransformBasePointer
  GetInverseTransform() const override
  {
    Pointer inv = New();
    return this->GetInverse(inv) ? inv.GetPointer() : nullptr;
  }

protected:
  Rigid3DTransform() = default;
}; // class Rigid3DTransform
} // namespace v3
} // namespace itk

#if !defined(ITK_LEGACY_REMOVE)
#  define itkv3 itk::v3
#endif

#endif /* itkv3Rigid3DTransform_h */
