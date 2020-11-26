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

#ifndef itkGPUSmoothingRecursiveYvvGaussianImageFilter_h
#define itkGPUSmoothingRecursiveYvvGaussianImageFilter_h

#ifdef GPU

#  include "itkImage.h"
#  include "itkPixelTraits.h"
#  include "itkCommand.h"
#  include "itkFixedArray.h"
#  include "itkSmoothingRecursiveYvvGaussianImageFilter.h"
#  include "itkOpenCLUtil.h"
#  include "itkGPUImageToImageFilter.h"

namespace itk
{
/**
 * \class GPUSmoothingRecursiveYvvGaussianImageFilter
 * \brief Recursive Gaussian blur based on Young-Van Vliet's algorithm and
 *  implemented for GPU.
 *
 *  The GPU implementation is more efficient than the CPU
 *  the bigger the image is; use the benchmark tests to establish the size for which
 *  the GPU performs better for your particular hardware configuration.
 *
 *  More information in the Insight Journal publication:
 *  http://hdl.handle.net/10380/3425
 *
 * \ingroup SmoothingRecursiveYvvGaussianFilter
 */

itkGPUKernelClassMacro(GPUSmoothingRecursiveYvvGaussianImageFilterKernel);

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_EXPORT GPUSmoothingRecursiveYvvGaussianImageFilter
  : public GPUImageToImageFilter<TInputImage,
                                 TOutputImage,
                                 SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUSmoothingRecursiveYvvGaussianImageFilter);

  /** Standard class type alias. */
  using Self = GPUSmoothingRecursiveYvvGaussianImageFilter;
  // typedef SmoothingRecursiveYvvGaussianImageFilter<TInputImage,TOutputImage>
  //   CPUSuperclass;
  using Superclass = GPUImageToImageFilter<TInputImage,
                                           TOutputImage,
                                           SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>>;
  using GPUSuperclass = GPUImageToImageFilter<TInputImage,
                                              TOutputImage,
                                              SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Pixel Type of the input image */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using PixelType = typename TInputImage::PixelType;
#  ifdef WITH_DOUBLE
  using RealType = typename NumericTraits<PixelType>::RealType;
  using ScalarRealType = typename NumericTraits<PixelType>::ScalarRealType;
#  else
  using RealType = typename NumericTraits<PixelType>::FloatType;
  using ScalarRealType = typename NumericTraits<PixelType>::FloatType;
#  endif

  using GPUInputImage = typename itk::GPUTraits<TInputImage>::Type;
  using GPUOutputImage = typename itk::GPUTraits<TOutputImage>::Type;

  /** Runtime information support. */
  itkTypeMacro(GPUSmoothingRecursiveYvvGaussianImageFilter, GPUImageToImageFilter);

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Define the type for the sigma array */
  using SigmaArrayType = FixedArray<ScalarRealType, itkGetStaticConstMacro(ImageDimension)>;

  /** Define the image type for internal computations
   RealType is usually 'double' in NumericTraits.
   Here we prefer float in order to save memory.  */

  using InternalRealType = typename NumericTraits<PixelType>::FloatType;
  using RealImageType = GPUImage<InternalRealType, itkGetStaticConstMacro(ImageDimension)>;

  /**  Pointer to the Output Image */
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set Sigma value. Sigma is measured in the units of image spacing. You
   may use the method SetSigma to set the same value across each axis or
   use the method SetSigmaArray if you need different values along each
   axis. */
  void
  SetSigmaArray(const SigmaArrayType & sigmas);

  void
  SetSigma(ScalarRealType sigma);

  SigmaArrayType
  GetSigmaArray() const;

  ScalarRealType
  GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian */
  void
  SetNormalizeAcrossScale(bool normalizeInScaleSpace);

  itkGetConstMacro(NormalizeAcrossScale, bool);

  virtual void
  SetUp(ScalarRealType spacing);

#  ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  /** End concept checking */
#  endif
  /** Get OpenCL Kernel source as a string, creates a GetOpenCLSource method */
  itkGetOpenCLSourceFromKernelMacro(GPUSmoothingRecursiveYvvGaussianImageFilterKernel);
  void
  SetInput(const TInputImage * input) override;
  using Superclass::SetInput;

protected:
  GPUSmoothingRecursiveYvvGaussianImageFilter();
  ~GPUSmoothingRecursiveYvvGaussianImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Data */
  void
  GPUGenerateData() override;

  /** GPUSmoothingRecursiveYvvGaussianImageFilter needs all of the input to produce an
   * output. Therefore, GPUSmoothingRecursiveYvvGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() ITK_NOEXCEPT override;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  void
  AllocateGPUCoefficients();

  std::ostringstream defines;

  ScalarRealType   m_B1;
  ScalarRealType   m_B2;
  ScalarRealType   m_B3;
  ScalarRealType   m_B;
  ScalarRealType * m_Bvalues;

  // Initialization matrix for anti-causal pass
  ScalarRealType * m_CPUMatrix;

  using GPUDataPointer = GPUDataManager::Pointer;

  GPUDataPointer m_GPUMMatrixDataManager;
  GPUDataPointer m_GPUBCoefficientsDataManager;
  GPUDataPointer m_GPULocalDataManager;

private:
  void
  BuildKernel();

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;

  int                               m_FilterGPUKernelHandle;
  typename GPUInputImage::Pointer   inPtr;
  typename GPUOutputImage::Pointer  otPtr;
  typename GPUOutputImage::SizeType m_requestedSize;
  /** Standard deviation of the gaussian used for smoothing */
  SigmaArrayType m_Sigma;
};
} // end namespace itk

#  ifndef ITK_MANUAL_INSTANTIATION
#    include "itkGPUSmoothingRecursiveYvvGaussianImageFilter.hxx"
#  endif

#endif // GPU

#endif // itkGPUSmoothingRecursiveYvvGaussianImageFilter_h
