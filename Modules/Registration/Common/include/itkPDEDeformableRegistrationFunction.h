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
#ifndef itkPDEDeformableRegistrationFunction_h
#define itkPDEDeformableRegistrationFunction_h

#include "itkFiniteDifferenceFunction.h"

namespace itk
{
/** \class PDEDeformableRegistrationFunction
 *
 * This is an abstract base class for all PDE functions which drives a
 * deformable registration algorithm. It is used by
 * PDEDeformationRegistrationFilter subclasses to compute the
 * output deformation field which will map a moving image onto
 * a fixed image.
 *
 * This class is templated over the fixed image type, moving image type
 * and the deformation field type.
 *
 * \sa PDEDeformableRegistrationFilter
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKRegistrationCommon
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class PDEDeformableRegistrationFunction : public FiniteDifferenceFunction<TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PDEDeformableRegistrationFunction);

  /** Standard class type aliases. */
  using Self = PDEDeformableRegistrationFunction;
  using Superclass = FiniteDifferenceFunction<TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(PDEDeformableRegistrationFunction, FiniteDifferenceFunction);

  /** MovingImage image type. */
  using MovingImageType = TMovingImage;
  using MovingImagePointer = typename MovingImageType::ConstPointer;

  /** FixedImage image type. */
  using FixedImageType = TFixedImage;
  using FixedImagePointer = typename FixedImageType::ConstPointer;

  /** Deformation field type. */
  using DisplacementFieldType = TDisplacementField;
  using DisplacementFieldTypePointer = typename DisplacementFieldType::Pointer;

  /** Set the moving image.  */
  void
  SetMovingImage(const MovingImageType * ptr)
  {
    m_MovingImage = ptr;
  }

  /** Get the moving image. */
  const MovingImageType *
  GetMovingImage() const
  {
    return m_MovingImage;
  }

  /** Set the fixed image. */
  void
  SetFixedImage(const FixedImageType * ptr)
  {
    m_FixedImage = ptr;
  }

  /** Get the fixed image. */
  const FixedImageType *
  GetFixedImage() const
  {
    return m_FixedImage;
  }

  /** Set the deformation field image. */
  void
  SetDisplacementField(DisplacementFieldTypePointer ptr)
  {
    m_DisplacementField = ptr;
  }

  /** Get the deformation field. This function should have been
   *  declared const. It is not for backward compatibility reasons. */
  DisplacementFieldType *
  GetDisplacementField()
  {
    return m_DisplacementField;
  }

  void
  SetEnergy(double e)
  {
    m_Energy = e;
  }
  double
  GetEnergy() const
  {
    return m_Energy;
  }
  void
  SetGradientStep(double e)
  {
    m_GradientStep = e;
  }
  double
  GetGradientStep() const
  {
    return m_GradientStep;
  }
  void
  SetNormalizeGradient(bool e)
  {
    m_NormalizeGradient = e;
  }
  bool
  GetNormalizeGradient() const
  {
    return m_NormalizeGradient;
  }

protected:
  PDEDeformableRegistrationFunction()
  {
    m_MovingImage = nullptr;
    m_FixedImage = nullptr;
    m_DisplacementField = nullptr;
    m_Energy = 0.0;
    m_NormalizeGradient = true;
    m_GradientStep = 1.0;
  }

  ~PDEDeformableRegistrationFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "MovingImage: ";
    os << m_MovingImage.GetPointer() << std::endl;
    os << indent << "FixedImage: ";
    os << m_FixedImage.GetPointer() << std::endl;
  }

  /** The moving image. */
  MovingImagePointer m_MovingImage;

  /** The fixed image. */
  FixedImagePointer m_FixedImage;

  /** The deformation field. */
  DisplacementFieldTypePointer m_DisplacementField;

  mutable double m_Energy;

  bool m_NormalizeGradient;

  mutable double m_GradientStep;
};
} // end namespace itk

#endif
