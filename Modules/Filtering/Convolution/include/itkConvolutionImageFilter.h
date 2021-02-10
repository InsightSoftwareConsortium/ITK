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
#ifndef itkConvolutionImageFilter_h
#define itkConvolutionImageFilter_h

#include "itkConvolutionImageFilterBase.h"

#include "itkProgressAccumulator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{
/**
 *\class ConvolutionImageFilter
 * \brief Convolve a given image with an arbitrary image kernel.
 *
 * This filter operates by centering the flipped kernel at each pixel
 * in the image and computing the inner product between pixel values
 * in the image and pixel values in the kernel. The center of the
 * kernel is defined as \f$ \lfloor (2*i+s-1)/2 \rfloor \f$ where
 * \f$i\f$ is the index and \f$s\f$ is the size of the largest
 * possible region of the kernel image. For kernels with odd sizes in
 * all dimensions, this corresponds to the center pixel. If a
 * dimension of the kernel image has an even size, then the center
 * index of the kernel in that dimension will be the largest integral
 * index that is less than the continuous index of the image center.
 *
 * The kernel can optionally be normalized to sum to 1 using
 * NormalizeOn(). Normalization is off by default.
 *
 * \warning This filter ignores the spacing, origin, and orientation
 * of the kernel image and treats them as identical to those in the
 * input image.
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Image Kernel Convolution"
 * by Tustison N., Gee J.
 * http://insight-journal.org/browse/publication/208
 *
 * \author Nicholas J. Tustison
 * \author James C. Gee
 * \ingroup ITKConvolution
 *
 * \sphinx
 * \sphinxexample{Filtering/Convolution/ConvolveImageWithKernel,Convolve Image With Kernel}
 * \endsphinx
 */
template <typename TInputImage, typename TKernelImage = TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT ConvolutionImageFilter
  : public ConvolutionImageFilterBase<TInputImage, TKernelImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConvolutionImageFilter);

  using Self = ConvolutionImageFilter;
  using Superclass = ConvolutionImageFilterBase<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information ( and related methods ) */
  itkTypeMacro(ConvolutionImageFilter, ConvolutionImageFilterBase);

  /** Dimensionality of input and output data is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using KernelImageType = TKernelImage;
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using KernelPixelType = typename KernelImageType::PixelType;
  using InputIndexType = typename InputImageType::IndexType;
  using OutputIndexType = typename OutputImageType::IndexType;
  using KernelIndexType = typename KernelImageType::IndexType;
  using InputSizeType = typename InputImageType::SizeType;
  using OutputSizeType = typename OutputImageType::SizeType;
  using KernelSizeType = typename KernelImageType::SizeType;
  using InputRegionType = typename InputImageType::RegionType;
  using OutputRegionType = typename OutputImageType::RegionType;
  using KernelRegionType = typename KernelImageType::RegionType;

protected:
  ConvolutionImageFilter() = default;
  ~ConvolutionImageFilter() override = default;

  /** ConvolutionImageFilter needs the entire image kernel, which in
   * general is going to be a different size then the output requested
   * region. As such, this filter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()  */
  void
  GenerateInputRequestedRegion() override;

  /** This filter uses a minipipeline to compute the output. */
  void
  GenerateData() override;

  /** The kernel needs padding if any of the sizes of its dimensions is
   * even. This method checks for this condition. */
  bool
  GetKernelNeedsPadding() const;

  /** Calculates the padding width needed to make each dimension odd. */
  KernelSizeType
  GetKernelPadSize() const;

  /** Calculates the radius of the kernel. */
  template <typename TImage>
  KernelSizeType
  GetKernelRadius(const TImage * kernelImage) const;

private:
  template <typename TImage>
  void
  ComputeConvolution(const TImage * kernelImage, ProgressAccumulator * progress);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConvolutionImageFilter.hxx"
#endif

#endif
