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

#ifndef itkTransformGeometryImageFilter_h
#define itkTransformGeometryImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkVersorRigid3DTransform.h"
#include "itkDataObjectDecorator.h"

namespace itk
{
/** \class TransformGeometryImageFilter
 * \brief Modify an image's geometric meta-data, changing its "physical" extent.
 *
 * The TransformGeometryImageFilter "physically" changes the image in
 * space using the given transformation. The resulting image is an image with
 * the same voxel values as the input, but with different physical space
 * representation as affected by the transform.
 *
 * The specific transformation type can be any type derived from the
 * MatrixOffsetTransformBase and the TranslationTransform.
 * The modification of the geometric meta-data is an alternative to
 * resampling the moving image onto the fixed image grid, after registration. The advantages of using
 * this approach over resampling are two-fold, it does not introduce artifacts
 * into the result because the original intensity information is not
 * modified, and it is computationally more efficient.
 *
 * When the filter is used with a rigid or translation transformation
 * the resulting image can be saved in any desired format. When the
 * filter is used with an affine transformation the resulting image
 * should be saved in a format that supports a non ortho-normal
 * direction cosine matrix (e.g. nrrd).
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
 *    - \f$\mathbf{ci}\f$: continuous index
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
class ITK_TEMPLATE_EXPORT TransformGeometryImageFilter : public InPlaceImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TransformGeometryImageFilter);

  /** Standard class type alias */
  using Self = TransformGeometryImageFilter;
  using Superclass = InPlaceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(TransformGeometryImageFilter, InPlaceImageFilter);

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
  using TransformType = Transform<double, InputImageDimension, OutputImageDimension>;
  using TransformConstPointer = typename TransformType::ConstPointer;

  /** Set/Get required rigid transform. */
  itkSetGetDecoratedObjectInputMacro(Transform, TransformType);

  /** Set/Get required input image. */
  itkSetInputMacro(InputImage, InputImageType);
  itkGetInputMacro(InputImage, InputImageType);

protected:
  TransformGeometryImageFilter();
  ~TransformGeometryImageFilter() override = default;

  void
  GenerateOutputInformation() override;

  void
  VerifyPreconditions() ITKv5_CONST override;

  void
  GenerateData() override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTransformGeometryImageFilter.hxx"
#endif

#endif
