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

#ifndef itkResampleInPlaceImageFilter_h
#define itkResampleInPlaceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVersorRigid3DTransform.h"

namespace itk
{
/** \class ResampleInPlaceImageFilter
 * \brief Resample an image in place.
 *
 * The current ITK resample image filter will generate a physical memory-
 * modified version of the input image if the input transform is not identity.
 * The abuse use of the resample filter can be a source of problems.
 * It can exhaust memory if the image is very large.
 * It WILL reduce image quality when there are lots of transforms to be
 * superimposed for the input image. We usually don't even care
 * about those intermediate transformed images!
 *
 * If all the transforms are rigid, there is a far superior way to achieve a similar result.
 * Updating image metadata in-place removes the accumulated resampling errors
 * as well as eliminating the expense of accessing the physical memory of the image.
 * We need to compose all the transforms beforehand to make use of this filter.
 *
 * \param RigidTransform -- Currently must be a \class VersorRigid3D
 * \param InputImage -- The image to be duplicated and modified to incorporate the
 * rigid transform
 * \return -- An image with the same voxels values as the input, but with differnt
 * physical space representation affected by the rigid transform.
 *
 * The purpose of this code is to generate the new origin and direction
 * that will remove the need for using the transform.
 *
 * Given a set of PhysicalFixedImagePoints (i.e. from the fixedImage space)
 * those points are converted to PhysicalMovingImagePoints = TfmF2M( PhysicalFixedImagePoints )
 * and then MovingContinuousIndexPoints = movingImage->TransformPhysicalPointToContinuousIndex(
 * PhysicalMovingImagePoints ) to get image values.
 *
 * We desire to change the moving image DirectionCosine [DC] and Origin [O] such that
 * we can compute the:
 * MovingContinuousIndexPoints = newMovingImage->TransformPhysicalPointToContinuousIndex( PhysicalFixedImagePoints )
 *
 * Image Notations:
 *   \f$\mathbf{D}\f$: Direction cosine matrix
 *   \f$\mathbf{o}\f$: Origin vector
 *   \f$\mathbf{S}\f$: Spacing
 *   \f$\mathbf{ci}\f$: Continouos index
 *   \f$\mathbf{D}^{'}\f$: New direction cosine matrix
 *   \f$\mathbf{o}^{'}\f$: New origin vector
 *
 * Rigid Transform Notations:
 *   \f$\mathbf{R}\f$: Rotation matrix
 *   \f$\mathbf{c}\f$: Center of rotation vector
 *   \f$\mathbf{t}\f$: Translation vector
 *
 *   \f$\mathbf{f}_{p}\f$: fixed image points in physical space
 *   \f$\mathbf{m}_{p}\f$: moving image points in physical space
 *
 * TransformPhysicalPointToContinuousIndex:
 * \f[
 *   \mathbf{ci} = \mathbf{S}^{-1}\mathbf{D}^{-1}( \mathbf{m}_{p} - \mathbf{o} ) \\
 *   \mathbf{m}_{p} = \mathbf{R}(\mathbf{f}_{p} - \mathbf{c}) + \mathbf{c} + \mathbf{T}
 * \f]
 *
 * After substitutions:
 *
 * \f[
 * \mathbf{m}_{c} = \underbrace{\mathbf{R}^{-1}\mathbf{D}}_\text{new cosine}\mathbf{S} * \mathbf{i} +
 * \underbrace{\mathbf{R}^{-1} * \mathbf{o} - \mathbf{R}^{-1} * \mathbf{c} - \mathbf{R}^{-1}*T}_\text{new origin} +
 * \mathbf{c} \\
 * \\
 * \mathbf{D}^{'} = \mathbf{R}^{-1}\mathbf{D} \\
 * \mathbf{o}^{'} = \mathbf{R}^{-1} * ( \mathbf{o} - \mathbf{c} - \mathbf{t} ) + \mathbf{c}
 * \f]
 *
 * \ingroup ITKTransform
 */
template <typename TInputImage, typename TOutputImage>
class ResampleInPlaceImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ResampleInPlaceImageFilter);

  /** Standard class type alias */
  using Self = ResampleInPlaceImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ResampleInPlaceImageFilter, ImageToImageFilter);

  /** input/output image type alias */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputImagePointType = typename InputImageType::PointType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputImagePixelType, OutputImagePixelType>));
#endif

  /** Transform type alias */
  using RigidTransformType = VersorRigid3DTransform<double>;
  using RigidTransformConstPointer = typename RigidTransformType::ConstPointer;

  /** Set/Get rigid transform. The default is an identity transform */
  itkSetConstObjectMacro(RigidTransform, RigidTransformType);
  itkGetConstObjectMacro(RigidTransform, RigidTransformType);

  /** Set/Get required input image. (A wrapper to this->Set/GetInput()) */
  void
  SetInputImage(const InputImageType * image);

  const InputImageType *
  GetInputImage() const;

protected:
  ResampleInPlaceImageFilter() = default;
  ~ResampleInPlaceImageFilter() override = default;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  OutputImagePointer         m_OutputImage;
  RigidTransformConstPointer m_RigidTransform;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkResampleInPlaceImageFilter.hxx"
#endif

#endif
