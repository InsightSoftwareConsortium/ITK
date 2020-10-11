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
#ifndef itkConvolutionImageFilterBase_h
#define itkConvolutionImageFilterBase_h

#include "itkImageToImageFilter.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "ITKConvolutionExport.h"

namespace itk
{
/**\class ConvolutionImageFilterBaseEnums
 * \brief Contains all enum classes used by ConvolutionImageFilterBase class.
 * \ingroup ITKConvolution
 */
class ConvolutionImageFilterBaseEnums
{
public:
  /**
   *\class ConvolutionImageFilterOutputRegion
   * \ingroup ITKConvolution
   * Output region mode type enumeration
   */
  enum class ConvolutionImageFilterOutputRegion : uint8_t
  {
    SAME = 0,
    VALID
  };
};
/** Define how to print enumerations */
extern ITKConvolution_EXPORT std::ostream &
                             operator<<(std::ostream & out, const ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion value);

/**
 *\class ConvolutionImageFilterBase
 * \brief Abstract base class for the convolution image filters.
 *
 * \ingroup ITKConvolution
 * \sa ConvolutionImageFilter FFTConvolutionImageFilter
 */
template <typename TInputImage, typename TKernelImage = TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT ConvolutionImageFilterBase : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConvolutionImageFilterBase);

  using Self = ConvolutionImageFilterBase;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information ( and related methods ) */
  itkTypeMacro(ConvolutionImageFilterBase, ImageToImageFilter);

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

  /** Typedef to describe the boundary condition. */
  using BoundaryConditionType = ImageBoundaryCondition<TInputImage>;
  using BoundaryConditionPointerType = BoundaryConditionType *;
  using DefaultBoundaryConditionType = ZeroFluxNeumannBoundaryCondition<TInputImage>;

  /** Set/get the boundary condition. */
  itkSetMacro(BoundaryCondition, BoundaryConditionPointerType);
  itkGetConstMacro(BoundaryCondition, BoundaryConditionPointerType);

  /** Set/get the image kernel. */
  itkSetInputMacro(KernelImage, KernelImageType);
  itkGetInputMacro(KernelImage, KernelImageType);

  /** Normalize the output image by the sum of the kernel
   * components. Defaults to off. */
  itkSetMacro(Normalize, bool);
  itkGetConstMacro(Normalize, bool);
  itkBooleanMacro(Normalize);

  /** Reverse compatibility for enumerations */
  using OutputRegionModeEnum = ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion;
#if !defined(ITK_LEGACY_REMOVE)
  using OutputRegionModeType = ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion;
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr OutputRegionModeEnum SAME = OutputRegionModeEnum::SAME;
  static constexpr OutputRegionModeEnum VALID = OutputRegionModeEnum::VALID;
#endif

  /** Sets the output region mode. If set to SAME, the output region
   * will be the same as the input region, and regions of the image
   * near the boundaries will contain contributions from outside the
   * input image as determined by the boundary condition set in
   * SetBoundaryCondition(). If set to VALID, the output region
   * consists of pixels computed only from pixels in the input image
   * (no extrapolated contributions from the boundary condition are
   * needed). The output is therefore smaller than the input
   * region. Default output region mode is SAME. */
  itkSetEnumMacro(OutputRegionMode, OutputRegionModeEnum);
  itkGetEnumMacro(OutputRegionMode, OutputRegionModeEnum);
  virtual void
  SetOutputRegionModeToSame();
  virtual void
  SetOutputRegionModeToValid();

protected:
  ConvolutionImageFilterBase();
  ~ConvolutionImageFilterBase() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** The largest possible output region may differ from the largest
   * possible input region. */
  void
  GenerateOutputInformation() override;

  /** Get the valid region of the convolution. */
  OutputRegionType
  GetValidRegion() const;

  /** Default superclass implementation ensures that input images
   * occupy same physical space. This is not needed for this filter. */
  void
  VerifyInputInformation() ITKv5_CONST override{};

private:
  bool m_Normalize{ false };

  DefaultBoundaryConditionType m_DefaultBoundaryCondition;
  BoundaryConditionPointerType m_BoundaryCondition;

  OutputRegionModeEnum m_OutputRegionMode{ ConvolutionImageFilterBaseEnums::ConvolutionImageFilterOutputRegion::SAME };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConvolutionImageFilterBase.hxx"
#endif

#endif
