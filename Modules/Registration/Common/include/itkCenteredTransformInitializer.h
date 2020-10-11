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
#ifndef itkCenteredTransformInitializer_h
#define itkCenteredTransformInitializer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkImageMomentsCalculator.h"

#include <iostream>

namespace itk
{
/** \class CenteredTransformInitializer
 * \brief CenteredTransformInitializer is a helper class intended to
 * initialize the center of rotation and the translation of Transforms having
 * the center of rotation among their parameters.
 *
 * This class is connected to the fixed image, moving image and transform
 * involved in the registration. Two modes of operation are possible:
 *
 * - Geometrical,
 * - Center of mass
 *
 * In the first mode, the geometrical center of the moving image is passed as
 * initial center of rotation to the transform and the vector from the center
 * of the  fixed image to the center of the moving image is passed as the
 * initial translation. This mode basically assumes that the anatomical objects
 * to be registered are centered in their respective images. Hence the best
 * initial guess for the registration is the one that superimposes those two
 * centers.
 *
 * In the second mode, the moments of gray level values are computed
 * for both images. The center of mass of the moving image is then
 * used as center of rotation. The vector between the two centers of
 * mass is passes as the initial translation to the transform. This
 * second approach assumes that the moments of the anatomical objects
 * are similar for both images and hence the best initial guess for
 * registration is to superimpose both mass centers.  Note that this
 * assumption will probably not hold in multi-modality registration.
 *
 * \ingroup ITKRegistrationCommon
 * \ingroup ITKTransform
 */
template <typename TTransform, typename TFixedImage, typename TMovingImage>
class ITK_TEMPLATE_EXPORT CenteredTransformInitializer : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CenteredTransformInitializer);

  /** Standard class type aliases. */
  using Self = CenteredTransformInitializer;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CenteredTransformInitializer, Object);

  /** Type of the transform to initialize */
  using TransformType = TTransform;
  using TransformPointer = typename TransformType::Pointer;

  /** Dimension of parameters. */
  static constexpr unsigned int InputSpaceDimension = TransformType::InputSpaceDimension;
  static constexpr unsigned int OutputSpaceDimension = TransformType::OutputSpaceDimension;

  /** Image Types to use in the initialization of the transform */
  using FixedImageType = TFixedImage;
  using MovingImageType = TMovingImage;

  using FixedImagePointer = typename FixedImageType::ConstPointer;
  using MovingImagePointer = typename MovingImageType::ConstPointer;

  /** Moment calculators */
  using FixedImageCalculatorType = ImageMomentsCalculator<FixedImageType>;
  using MovingImageCalculatorType = ImageMomentsCalculator<MovingImageType>;

  using FixedImageCalculatorPointer = typename FixedImageCalculatorType::Pointer;
  using MovingImageCalculatorPointer = typename MovingImageCalculatorType::Pointer;

  /** Offset type. */
  using OffsetType = typename TransformType::OffsetType;

  /** Point type. */
  using InputPointType = typename TransformType::InputPointType;

  /** Vector type. */
  using OutputVectorType = typename TransformType::OutputVectorType;

  /** Set the transform to be initialized */
  itkSetObjectMacro(Transform, TransformType);

  /** Set the fixed image used in the registration process */
  itkSetConstObjectMacro(FixedImage, FixedImageType);

  /** Set the moving image used in the registration process */
  itkSetConstObjectMacro(MovingImage, MovingImageType);

  /** Initialize the transform using data from the images */
  virtual void
  InitializeTransform();

  /** Select between using the geometrical center of the images or
      using the center of mass given by the image intensities. */
  void
  GeometryOn()
  {
    m_UseMoments = false;
  }
  void
  MomentsOn()
  {
    m_UseMoments = true;
  }

  /** Get() access to the moments calculators */
  itkGetModifiableObjectMacro(FixedCalculator, FixedImageCalculatorType);
  itkGetModifiableObjectMacro(MovingCalculator, MovingImageCalculatorType);

protected:
  CenteredTransformInitializer();
  ~CenteredTransformInitializer() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  itkGetModifiableObjectMacro(Transform, TransformType);

private:
  TransformPointer m_Transform;

  FixedImagePointer m_FixedImage;

  MovingImagePointer m_MovingImage;

  bool m_UseMoments;

  FixedImageCalculatorPointer  m_FixedCalculator;
  MovingImageCalculatorPointer m_MovingCalculator;
}; // class CenteredTransformInitializer
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCenteredTransformInitializer.hxx"
#endif

#endif /* itkCenteredTransformInitializer_h */
