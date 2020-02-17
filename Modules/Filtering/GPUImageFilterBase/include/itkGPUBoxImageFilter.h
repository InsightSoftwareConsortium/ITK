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
#ifndef itkGPUBoxImageFilter_h
#define itkGPUBoxImageFilter_h

#include "itkGPUImageToImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkBoxImageFilter.h"

namespace itk
{
/**
 * \class GPUBoxImageFilter
 * \brief A base class for all the GPU filters working on a box neighborhood
 *
 * This filter provides the code to store the radius information about the
 * neighborhood used in the subclasses.
 * It reuses the GenerateInputRequestedRegion() defined in BoxImageFilter class.
 *
 * \author Won-Ki Jeong
 * \ingroup ITKGPUImageFilterBase
 */

template <typename TInputImage,
          typename TOutputImage,
          typename TParentImageFilter = BoxImageFilter<TInputImage, TOutputImage>>
class GPUBoxImageFilter : public GPUImageToImageFilter<TInputImage, TOutputImage, TParentImageFilter>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUBoxImageFilter);

  /** Standard class type aliases. */
  using Self = GPUBoxImageFilter;
  using GPUSuperclass = GPUImageToImageFilter<TInputImage, TOutputImage, TParentImageFilter>;
  using CPUSuperclass = TParentImageFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GPUBoxImageFilter, GPUImageToImageFilter);

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using RegionType = typename CPUSuperclass::RegionType;
  using SizeType = typename CPUSuperclass::SizeType;
  using IndexType = typename CPUSuperclass::IndexType;
  using OffsetType = typename CPUSuperclass::OffsetType;
  using InputPixelType = typename TInputImage::PixelType;

  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;

  /** n-dimensional Kernel radius. */
  using RadiusType = typename CPUSuperclass::SizeType;
  using RadiusValueType = typename InputImageType::SizeValueType;

protected:
  GPUBoxImageFilter() = default;
  ~GPUBoxImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    GPUSuperclass::PrintSelf(os, indent);
  }
};
} // namespace itk

#endif
