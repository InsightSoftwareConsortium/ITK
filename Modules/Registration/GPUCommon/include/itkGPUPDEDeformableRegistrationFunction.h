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
#ifndef itkGPUPDEDeformableRegistrationFunction_h
#define itkGPUPDEDeformableRegistrationFunction_h

#include "itkGPUFiniteDifferenceFunction.h"

namespace itk
{
/** \class GPUPDEDeformableRegistrationFunction
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
 * \ingroup ITKGPURegistrationCommon
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
class GPUPDEDeformableRegistrationFunction :
  public GPUFiniteDifferenceFunction<TDisplacementField>
{
public:
  /** Standard class typedefs. */
  typedef GPUPDEDeformableRegistrationFunction            Self;
  typedef GPUFiniteDifferenceFunction<TDisplacementField> Superclass;
  typedef SmartPointer<Self>                              Pointer;
  typedef SmartPointer<const Self>                        ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(GPUPDEDeformableRegistrationFunction,
               GPUFiniteDifferenceFunction);

  /** MovingImage image type. */
  typedef TMovingImage                           MovingImageType;
  typedef typename MovingImageType::ConstPointer MovingImagePointer;

  /** FixedImage image type. */
  typedef TFixedImage                           FixedImageType;
  typedef typename FixedImageType::ConstPointer FixedImagePointer;

  /** Deformation field type. */
  typedef TDisplacementField                      DisplacementFieldType;
  typedef typename DisplacementFieldType::Pointer DisplacementFieldTypePointer;

  /** Set the moving image.  */
  void SetMovingImage(const MovingImageType *ptr)
  {
    m_MovingImage = ptr;
  }

  /** Get the moving image. */
  const MovingImageType * GetMovingImage(void) const
  {
    return m_MovingImage;
  }

  /** Set the fixed image. */
  void SetFixedImage(const FixedImageType *ptr)
  {
    m_FixedImage = ptr;
  }

  /** Get the fixed image. */
  const FixedImageType * GetFixedImage(void) const
  {
    return m_FixedImage;
  }

  /** Set the deformation field image. */
  void SetDisplacementField(DisplacementFieldTypePointer ptr)
  {
    m_DisplacementField = ptr;
  }

  /** Get the deformation field. This function should have been
   *  declared const. It is not for backward compatibility reasons. */
  DisplacementFieldType * GetDisplacementField(void)
  {
    return m_DisplacementField;
  }

  void SetEnergy(double e)
  {
    m_Energy = e;
  }

  double GetEnergy() const
  {
    return m_Energy;
  }

  void SetGradientStep(double e)
  {
    m_GradientStep = e;
  }

  double GetGradientStep() const
  {
    return m_GradientStep;
  }

  void SetNormalizeGradient(bool e)
  {
    m_NormalizeGradient = e;
  }

  bool GetNormalizeGradient() const
  {
    return m_NormalizeGradient;
  }

protected:
  GPUPDEDeformableRegistrationFunction()
  {
    m_MovingImage = ITK_NULLPTR;
    m_FixedImage = ITK_NULLPTR;
    m_DisplacementField = ITK_NULLPTR;
    m_Energy = 0.0;
    m_NormalizeGradient = true;
    m_GradientStep = 1.0;
  }

  ~GPUPDEDeformableRegistrationFunction() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
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

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUPDEDeformableRegistrationFunction);

};
} // end namespace itk

#endif
