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
#ifndef itkMedialThicknessImageFilter3D_h
#define itkMedialThicknessImageFilter3D_h

#include "itkImageToImageFilter.h"
#include "itkBinaryThinningImageFilter3D.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkMaskImageFilter.h"

namespace itk
{

/** \class MedialThicknessImageFilter3D
 *
 * \brief Filters a image by iterating over its pixels.
 *
 * Filters a image by iterating over its pixels in a multi-threaded way
 * and {to be completed by the developer}.
 *
 * \ingroup Thickness3D
 *
 */
template <typename TInputImage, typename TOutputImage>
class MedialThicknessImageFilter3D : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MedialThicknessImageFilter3D);

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
  using DistanceType = SignedMaurerDistanceMapImageFilter<TInputImage, TOutputImage>;
  using SkeletonType = BinaryThinningImageFilter3D<TInputImage, TInputImage>;
  using MaskType = MaskImageFilter<TOutputImage, TInputImage, TOutputImage>;

  /** Composite sub-filters members. */
  typename DistanceType::Pointer m_DistanceFilter;
  typename SkeletonType::Pointer m_SkeletonFilter;
  typename MaskType::Pointer     m_MaskFilter;

  MedialThicknessImageFilter3D();
  virtual ~MedialThicknessImageFilter3D() override {}

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typedef typename OutputImageType::RegionType OutputRegionType;

  virtual void
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
