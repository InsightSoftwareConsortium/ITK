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
 * The ResampleImageFilter will generate a physical memory-modified version of
 * the input image if the input transform is not identity. Its neglectful use
 * can be a source of problems: e.g. it can exhaust the memory if the image is
 * very large, and it WILL reduce the image quality when there are lots of
 * transforms to be superimposed for the input image. Often times, we are not
 * interested in the intermediate transformed images.
 *
 * If all the transforms are rigid, there is a far superior way to achieve a similar result.
 * Updating image metadata in-place removes the accumulated resampling errors
 * as well as eliminating the expense of accessing the physical memory of the image.
 * We need to compose all the transforms beforehand to make use of this filter.
 *
 * \param \c RigidTransform Currently must be a VersorRigid3DTransform.
 * \param \c InputImage The image to be duplicated and modified to incorporate the
 * rigid transform.
 * \return An image with the same voxel values as the input, but with different
 * physical space representation affected by the rigid transform.
 *
 * The purpose of this class is to generate the new origin and direction
 * that will remove the need for using the transform.
 *
 * Let us call the transform operation from the fixed image to moving image <tt>TfmF2M</tt>.
 * Given a set of points from the fixed image in physical space (i.e. <tt>physicalFixedImagePoints</tt>),
 * the aim is to convert those points into the moving image physical space as
 * <tt>physicalMovingImagePoints = TfmF2M( physicalFixedImagePoints )</tt>, and
 * then be able to get the image values as
 * <tt>movingContinuousIndexPoints = movingImage->TransformPhysicalPointToContinuousIndex( physicalMovingImagePoints
 * )</tt>.
 *
 * We desire to change the moving image direction cosine \f$\mathbf{D}\f$ and origin \f$\mathbf{o}\f$
 * such that we can compute the moving image points as
 * <tt>movingContinuousIndexPoints = newMovingImage->TransformPhysicalPointToContinuousIndex( physicalFixedImagePoints
 * )</tt>
 *
 * Let us introduce the notation that will be used to formalize the transformation:
 *  - Image-related parameters:
 *    - \f$\mathbf{D}\f$: direction cosine matrix
 *    - \f$\mathbf{o}\f$: origin vector
 *    - \f$\mathbf{S}\f$: spacing
 *    - \f$\mathbf{ci}\f$: continouos index
 *    - \f$\mathbf{D}^{'}\f$: new direction cosine matrix
 *    - \f$\mathbf{o}^{'}\f$: new origin vector
 *
 *  - Image content in corresponding space:
 *    - \f$\mathbf{f}_{p}\f$: fixed image points in physical space
 *    - \f$\mathbf{m}_{p}\f$: moving image points in physical space
 *
 *  - Rigid transform-related parameters:
 *     - \f$\mathbf{R}\f$: rotation matrix
 *     - \f$\mathbf{c}\f$: center of rotation vector
 *     - \f$\mathbf{t}\f$: translation vector
 *
 * The <tt>TransformPhysicalPointToContinuousIndex</tt> method performs then:
 * \f{eqnarray*}{
 *   \mathbf{ci} &= \mathbf{S}^{-1}\mathbf{D}^{-1}( \mathbf{m}_{p} - \mathbf{o} ) \\
 *   \mathbf{m}_{p} &= \mathbf{R}(\mathbf{f}_{p} - \mathbf{c}) + \mathbf{c} + \mathbf{t}
 * \f}
 *
 * After substitution:
 *
 * \f{eqnarray*}{
 *   \mathbf{m}_{c} &= \underbrace{\mathbf{R}^{-1}\mathbf{D}}_\text{new cosine}\mathbf{S} * \mathbf{i} +
 *   \underbrace{\mathbf{R}^{-1} * \mathbf{o} - \mathbf{R}^{-1} * \mathbf{c} - \mathbf{R}^{-1}*t}_\text{new origin} +
 *   \mathbf{c} \\
 *   \\
 *   \mathbf{D}^{'} &= \mathbf{R}^{-1}\mathbf{D} \\
 *   \mathbf{o}^{'} &= \mathbf{R}^{-1} * ( \mathbf{o} - \mathbf{c} - \mathbf{t} ) + \mathbf{c}
 * \f}
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
