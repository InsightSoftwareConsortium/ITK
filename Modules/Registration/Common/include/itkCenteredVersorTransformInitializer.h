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
#ifndef itkCenteredVersorTransformInitializer_h
#define itkCenteredVersorTransformInitializer_h

#include "itkCenteredTransformInitializer.h"
#include "itkVersorRigid3DTransform.h"

namespace itk
{
/** \class CenteredVersorTransformInitializer
 * \brief CenteredVersorTransformInitializer is a helper class intended to
 * initialize the center of rotation, versor, and translation of the
 * VersorRigid3DTransform.
 *
 * This class derived from the CenteredTransformInitializer and uses it in
 * a more constrained context. It always uses the Moments mode, and also
 * takes advantage of the second order moments in order to initialize the
 * Versor representing rotation.
 *
 * \ingroup ITKRegistrationCommon
 * \ingroup ITKTransform
 */
template <typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT CenteredVersorTransformInitializer
  : public CenteredTransformInitializer<VersorRigid3DTransform<double>, TFixedImage, TMovingImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CenteredVersorTransformInitializer);

  /** Standard class type aliases. */
  using Self = CenteredVersorTransformInitializer;
  using Superclass = CenteredTransformInitializer<VersorRigid3DTransform<double>, TFixedImage, TMovingImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CenteredVersorTransformInitializer, Object);

  /** Type of the transform to initialize */
  using TransformType = typename Superclass::TransformType;
  using TransformPointer = typename Superclass::TransformPointer;

  /** Dimension of parameters. */
  static constexpr unsigned int InputSpaceDimension = Superclass::InputSpaceDimension;
  static constexpr unsigned int OutputSpaceDimension = Superclass::OutputSpaceDimension;

  /** Image Types to use in the initialization of the transform */
  using FixedImageType = typename Superclass::FixedImageType;
  using MovingImageType = typename Superclass::MovingImageType;

  using FixedImagePointer = typename Superclass::FixedImagePointer;
  using MovingImagePointer = typename Superclass::MovingImagePointer;

  /** Offset type. */
  using OffsetType = typename Superclass::OffsetType;

  /** Point type. */
  using InputPointType = typename Superclass::InputPointType;

  /** Vector type. */
  using OutputVectorType = typename Superclass::OutputVectorType;

  /** Initialize the transform using data from the images */
  void
  InitializeTransform() override;

  /** Enable the use of the principal axes of each image to compute an
   * initial rotation that will align them. */
  itkSetMacro(ComputeRotation, bool);
  itkGetMacro(ComputeRotation, bool);
  itkBooleanMacro(ComputeRotation);

protected:
  CenteredVersorTransformInitializer();
  ~CenteredVersorTransformInitializer() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  bool m_ComputeRotation;
}; // class CenteredVersorTransformInitializer
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCenteredVersorTransformInitializer.hxx"
#endif

#endif /* itkCenteredVersorTransformInitializer_h */
