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
#ifndef itkGridForwardWarpImageFilter_h
#define itkGridForwardWarpImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class GridForwardWarpImageFilter
 * \brief Warps a grid using an input deformation field.
 *
 * GridForwardWarpImageFilter warps a grid with respect to
 * a given deformation field.
 *
 * A displacement field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the input image. The vector type must support element access via operator
 * [].
 *
 * The output image is produced by forward mapping.
 *
 * Each vector in the displacement field represent the distance between
 * a geometric point in the input space and a point in the output space such
 * that:
 *
 * \f[ p_{in} = p_{out} + d \f]
 *
 * Typically the mapped position does not correspond to an integer pixel
 * position in the output image. We round it.
 *
 * Warning: the functionality provided by this class is currently implemented
 * with more options and multi-threaded in itk::GridImageSource by setting up
 * an appropriate itk::ResampleImageFilter instance to such an object.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/510
 *
 * \ingroup ITKReview
 */
template <typename TDisplacementField, typename TOutputImage>
class ITK_TEMPLATE_EXPORT GridForwardWarpImageFilter : public ImageToImageFilter<TDisplacementField, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GridForwardWarpImageFilter);

  /** Standard class type aliases. */
  using Self = GridForwardWarpImageFilter;
  using Superclass = ImageToImageFilter<TDisplacementField, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(GridForwardWarpImageFilter, ImageToImageFilter);

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Inherit some types from the superclass. */
  using typename Superclass::OutputImageType;
  using typename Superclass::OutputImagePointer;
  using IndexType = typename OutputImageType::IndexType;
  using SizeType = typename OutputImageType::SizeType;
  using PixelType = typename OutputImageType::PixelType;
  using SpacingType = typename OutputImageType::SpacingType;

  /** Determine the image dimension. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int DisplacementFieldDimension = TDisplacementField::ImageDimension;

  /** Deformation field type alias support */
  using DisplacementFieldType = TDisplacementField;
  using DisplacementFieldConstPointer = typename DisplacementFieldType::ConstPointer;
  using DisplacementType = typename DisplacementFieldType::PixelType;

  /** Set the background value */
  itkSetMacro(BackgroundValue, PixelType);

  /** Get the background value */
  itkGetConstMacro(BackgroundValue, PixelType);

  /** Set the foreground value */
  itkSetMacro(ForegroundValue, PixelType);

  /** Get the foreground value */
  itkGetConstMacro(ForegroundValue, PixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, DisplacementFieldDimension>));
  itkConceptMacro(DisplacementFieldHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<typename TDisplacementField::PixelType::ValueType>));
  // End concept checking
#endif

protected:
  GridForwardWarpImageFilter();
  ~GridForwardWarpImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  PixelType    m_BackgroundValue;
  PixelType    m_ForegroundValue;
  unsigned int m_GridPixSpacing{ 5 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGridForwardWarpImageFilter.hxx"
#endif

#endif
