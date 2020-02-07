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
#ifndef itkKernelImageFilter_h
#define itkKernelImageFilter_h

#include "itkBoxImageFilter.h"

namespace itk
{

template <unsigned int VDimension>
class FlatStructuringElement;
/**
 * \class KernelImageFilter
 * \brief A base class for all the filters working on an arbitrary shaped neighborhood
 *
 * This filter provides the code to store the radius information about the
 * neighborhood used in the subclasses.
 *
 * \author Gaetan Lehmann
 * \ingroup ITKImageFilterBase
 */

template< typename TInputImage, typename TOutputImage, typename TKernel /*=Neighborhood<bool,
                                                                 TInputImage::ImageDimension>*/                     >
class ITK_TEMPLATE_EXPORT KernelImageFilter:
  public BoxImageFilter< TInputImage, TOutputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(KernelImageFilter);

  /** Standard class type aliases. */
  using Self = KernelImageFilter;
  using Superclass = BoxImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(KernelImageFilter, BoxImageFilter);

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using OffsetType = typename TInputImage::OffsetType;

  using InputPixelType = typename TInputImage::PixelType;

  using OutputImageType = TOutputImage;
  using OutputPixelType = typename TOutputImage::PixelType;

  using KernelType = TKernel;

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  /** Kernel type used to create box kernel, in SetRadius() method */
  using FlatKernelType = FlatStructuringElement<(Self::ImageDimension)>;
  /** n-dimensional Kernel radius. */
  using RadiusType = typename TInputImage::SizeType;

  /** Set kernel (structuring element). */
  virtual void
  SetKernel(const KernelType & kernel);

  itkGetConstReferenceMacro(Kernel, KernelType);

  /** Set the kernel to a box kernel of given radius. */
  void
  SetRadius(const RadiusType & radius) override;

  void
  SetRadius(const SizeValueType & radius) override
  {
    // needed because of the overloading of the method
    Superclass::SetRadius(radius);
  }

protected:
  KernelImageFilter();
  ~KernelImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** kernel or structuring element to use. */
  KernelType m_Kernel;

private:
  template <typename T>
  void
  MakeKernel(const RadiusType & radius, T & kernel)
  {
    kernel.SetRadius(radius);
    for (typename T::Iterator kit = kernel.Begin(); kit != kernel.End(); kit++)
    {
      *kit = 1;
    }
  }

  void
  MakeKernel(const RadiusType & radius, FlatKernelType & kernel)
  {
    // set up a decomposable box structuring element which is
    // much efficient with van Herk / Gil Werman filters
    kernel = FlatKernelType::Box(radius);
    assert(kernel.GetDecomposable());
  }
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkKernelImageFilter.hxx"
#endif

#endif
