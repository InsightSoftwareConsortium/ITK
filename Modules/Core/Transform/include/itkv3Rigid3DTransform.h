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
#ifndef itkv3Rigid3DTransform_h
#define itkv3Rigid3DTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"
#include "itkVersor.h"

namespace itkv3
{
/** \class Rigid3DTransform
 * \brief ITK3.x compatible Rigid3DTransform of a vector space (e.g. space coordinates)
 *
 * NOTE: In ITK4, the itkNewMacro() was removed from
 * itk::Rigid3DTransform. This class, itkv3::Rigid3DTransform provides
 * the ITK3.x functionality. The purpose of the class is provide ITKv3
 * functionality while allowing the user to turn off
 * ITKV3_COMPATIBILITY.
 *
 * Even though the name Rigid3DTransform is conceptually closer to
 * what a user may expect, the VersorRigid3DTransform is often a
 * much better transform to use during optimization proceedures from
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
template<typename TParametersValueType=double>
class Rigid3DTransform:
    public itk::Rigid3DTransform<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef Rigid3DTransform                            Self;
  typedef itk::Rigid3DTransform<TParametersValueType> Superclass;
  typedef itk::SmartPointer<Self>                     Pointer;
  typedef itk::SmartPointer<const Self>               ConstPointer;


  /** Run-time type information (and related methods). */
  itkTypeMacro(Rigid3DTransform, itk::Rigid3DTransform);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Dimension of the space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 12);

  typedef typename Superclass::ParametersType            ParametersType;
  typedef typename Superclass::ParametersValueType       ParametersValueType;
  typedef typename Superclass::FixedParametersType       FixedParametersType;
  typedef typename Superclass::FixedParametersValueType  FixedParametersValueType;
  typedef typename Superclass::JacobianType              JacobianType;
  typedef typename Superclass::ScalarType                ScalarType;
  typedef typename Superclass::InputVectorType           InputVectorType;
  typedef typename Superclass::OutputVectorType          OutputVectorType;
  typedef typename Superclass::OutputVectorValueType     OutputVectorValueType;
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;
  typedef typename Superclass::InputVnlVectorType        InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType       OutputVnlVectorType;
  typedef typename Superclass::InputPointType            InputPointType;
  typedef typename Superclass::OutputPointType           OutputPointType;
  typedef typename Superclass::MatrixType                MatrixType;
  typedef typename Superclass::InverseMatrixType         InverseMatrixType;
  typedef typename Superclass::MatrixValueType           MatrixValueType;
  typedef typename Superclass::CenterType                CenterType;
  typedef typename Superclass::TranslationType           TranslationType;
  typedef typename Superclass::OffsetType                OffsetType;

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  typedef typename Superclass::InverseTransformBaseType InverseTransformBaseType;
  typedef typename InverseTransformBaseType::Pointer    InverseTransformBasePointer;

/** Get an inverse of this transform. */
  bool GetInverse(Self *inverse) const
  {
  return this->Superclass::GetInverse(inverse);
  }

/** Return an inverse of this transform. */
virtual InverseTransformBasePointer GetInverseTransform() const ITK_OVERRIDE
  {
  Pointer inv = New();
  return this->GetInverse(inv) ? inv.GetPointer() : ITK_NULLPTR;
  }

protected:
  Rigid3DTransform()
  {
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Rigid3DTransform);
};                                //class Rigid3DTransform
}  // namespace itkv3
#endif /* itkv3Rigid3DTransform_h */
