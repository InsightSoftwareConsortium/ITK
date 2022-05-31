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
#ifndef itkMedialThicknessImageFilter3D_h
#define itkMedialThicknessImageFilter3D_h

#include "itkImageToImageFilter.h"
#include "itkBinaryThinningImageFilter3D.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkMaskImageFilter.h"
#include "itkMultiplyImageFilter.h"

namespace itk
{

/** \class MedialThicknessImageFilter3D
 *
 * \brief This filter computes the medial thickness of a 3D input image.
 *
 * The input is assumed to be a binary image. All non-zero valued voxels
 * are set to 1 internally to simplify the computation. The filter will
 * extract a one-pixel-wide skeleton of the object and compute the distance
 * to the object edge. This distance multiplied by 2 is considered the
 * local thickness of the object (thickness = center-to-boundary x 2).
 *
 * \ingroup MathematicalMorphologyImageFilters Thickness3D
 */
template <typename TInputImage, typename TOutputImage>
class MedialThicknessImageFilter3D : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MedialThicknessImageFilter3D);

  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;

  /** Standard class typedefs. */
  using Self = MedialThicknessImageFilter3D<InputImageType, OutputImageType>;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information. */
  itkTypeMacro(MedialThicknessImageFilter3D, ImageToImageFilter);

  /** Standard New macro. */
  itkNewMacro(Self);

protected:
  /** Composite sub-filters typedefs. */
  using DistanceImageFilterType = SignedMaurerDistanceMapImageFilter<TInputImage, TOutputImage>;
  using ThinningImageFilterType = BinaryThinningImageFilter3D<TInputImage, TInputImage>;
  using MaskImageFilterType = MaskImageFilter<TOutputImage, TInputImage, TOutputImage>;
  using MultiplyImageFilterType = MultiplyImageFilter<TOutputImage, TOutputImage, TOutputImage>;

  /** Composite sub-filters members. */
  typename DistanceImageFilterType::Pointer m_DistanceFilter;
  typename ThinningImageFilterType::Pointer m_ThinningFilter;
  typename MaskImageFilterType::Pointer     m_MaskFilter;
  typename MultiplyImageFilterType::Pointer m_MultiplyFilter;

  MedialThicknessImageFilter3D();
  ~MedialThicknessImageFilter3D() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using OutputRegionType = typename OutputImageType::RegionType;

  void
  GenerateData() override;

private:
#ifdef ITK_USE_CONCEPT_CHECKING
  // Add concept checking such as
  // itkConceptMacro( FloatingPointPixel, ( itk::Concept::IsFloatingPoint< typename InputImageType::PixelType > ) );
#endif
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMedialThicknessImageFilter3D.hxx"
#endif

#endif // itkMedialThicknessImageFilter3D
