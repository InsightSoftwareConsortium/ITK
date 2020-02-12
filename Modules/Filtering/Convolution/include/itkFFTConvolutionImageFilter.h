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
#ifndef itkFFTConvolutionImageFilter_h
#define itkFFTConvolutionImageFilter_h

#include "itkConvolutionImageFilterBase.h"

#include "itkProgressAccumulator.h"
#include "itkHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{
/**
 *\class FFTConvolutionImageFilter
 * \brief Convolve a given image with an arbitrary image kernel using
 * multiplication in the Fourier domain.
 *
 * This filter produces output equivalent to the output of the
 * ConvolutionImageFilter.  However, it takes advantage of the
 * convolution theorem to accelerate the convolution computation when
 * the kernel is large.
 *
 * \warning This filter ignores the spacing, origin, and orientation
 * of the kernel image and treats them as identical to those in the
 * input image.
 *
 * This code was adapted from the Insight Journal contribution:
 *
 * "FFT Based Convolution"
 * by Gaetan Lehmann
 * https://hdl.handle.net/10380/3154
 *
 * \ingroup ITKConvolution
 * \sa ConvolutionImageFilter
 *
 */
template <typename TInputImage,
          typename TKernelImage = TInputImage,
          typename TOutputImage = TInputImage,
          typename TInternalPrecision = double>
class ITK_TEMPLATE_EXPORT FFTConvolutionImageFilter
  : public ConvolutionImageFilterBase<TInputImage, TKernelImage, TOutputImage>

{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FFTConvolutionImageFilter);

  using Self = FFTConvolutionImageFilter;
  using Superclass = ConvolutionImageFilterBase<TInputImage, TKernelImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information ( and related methods ) */
  itkTypeMacro(FFTConvolutionImageFilter, ConvolutionImageFilterBase);

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
  using SizeValueType = typename InputSizeType::SizeValueType;
  using InputRegionType = typename InputImageType::RegionType;
  using OutputRegionType = typename OutputImageType::RegionType;
  using KernelRegionType = typename KernelImageType::RegionType;

  /** Internal types used by the FFT filters. */
  using InternalImageType = Image<TInternalPrecision, TInputImage::ImageDimension>;
  using InternalImagePointerType = typename InternalImageType::Pointer;
  using InternalComplexType = std::complex<TInternalPrecision>;
  using InternalComplexImageType = Image<InternalComplexType, TInputImage::ImageDimension>;
  using InternalComplexImagePointerType = typename InternalComplexImageType::Pointer;

  /** Typedef to describe the boundary condition. */
  using BoundaryConditionType = typename Superclass::BoundaryConditionType;
  using BoundaryConditionPointerType = typename Superclass::BoundaryConditionPointerType;

  itkSetMacro(SizeGreatestPrimeFactor, SizeValueType);
  itkGetMacro(SizeGreatestPrimeFactor, SizeValueType);

protected:
  FFTConvolutionImageFilter();
  ~FFTConvolutionImageFilter() override = default;

  /** Because the inputs are real, we can use the specialized filters
   * for real-to-complex Fourier transforms. */
  using FFTFilterType = RealToHalfHermitianForwardFFTImageFilter<InternalImageType, InternalComplexImageType>;
  using IFFTFilterType = HalfHermitianToRealInverseFFTImageFilter<InternalComplexImageType, InternalImageType>;

  /** FFTConvolutionImageFilter needs the entire image kernel, which in
   * general is going to be a different size than the output requested
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

  /** Prepare the input images for operations in the Fourier
   * domain. This includes resizing the input and kernel images,
   * normalizing the kernel if requested, shifting the kernel, and
   * taking the Fourier transform of the padded inputs. */
  void
  PrepareInputs(const InputImageType *            input,
                const KernelImageType *           kernel,
                InternalComplexImagePointerType & preparedInput,
                InternalComplexImagePointerType & preparedKernel,
                ProgressAccumulator *             progress,
                float                             progressWeight);

  /** Prepare the input image. This includes padding the image and
   * taking the Fourier transform of the padded image. */
  void
  PrepareInput(const InputImageType *            input,
               InternalComplexImagePointerType & preparedInput,
               ProgressAccumulator *             progress,
               float                             progressWeight);

  /** Pad the input image. */
  void
  PadInput(const InputImageType *     input,
           InternalImagePointerType & paddedInput,
           ProgressAccumulator *      progress,
           float                      progressWeight);

  /** Take the Fourier transform of the padded input. */
  void
  TransformPaddedInput(const InternalImageType *         paddedInput,
                       InternalComplexImagePointerType & transformedInput,
                       ProgressAccumulator *             progress,
                       float                             progressWeight);

  /** Prepare the kernel. This includes resizing the input and kernel
   * images, normalizing the kernel if requested, shifting the kernel,
   * and taking the Fourier transform of the padded kernel. */
  void
  PrepareKernel(const KernelImageType *           kernel,
                InternalComplexImagePointerType & preparedKernel,
                ProgressAccumulator *             progress,
                float                             progressWeight);

  /** Produce output from the final Fourier domain image. */
  void
  ProduceOutput(InternalComplexImageType * paddedOutput, ProgressAccumulator * progress, float progressWeight);

  /** Crop the padded version of the output. */
  void
  CropOutput(InternalImageType * paddedOutput, ProgressAccumulator * progress, float progressWeight);

  /** Get the lower bound for the padding of both the kernel and input
   * images. Assuming that the regions of the kernel and input are the
   * same, then this lower bound can be used to move the index of the
   * padded kernel and padded input so that they are the same. This
   * is important to avoid exceptions in filters that operate on these
   * images. */
  InputSizeType
  GetPadLowerBound() const;

  /** Get the pad size. */
  InputSizeType
  GetPadSize() const;

  /** Get whether the X dimension has an odd size. */
  bool
  GetXDimensionIsOdd() const;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  SizeValueType m_SizeGreatestPrimeFactor;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFFTConvolutionImageFilter.hxx"
#endif

#endif
