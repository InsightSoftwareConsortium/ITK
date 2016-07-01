/*=========================================================================
 *
 * Copyright Insight Software Consortium
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 *=========================================================================*/

#ifdef GPU

#  ifndef _ITK_GPU_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_H_
#    define _ITK_GPU_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_H_

#    include "itkImage.h"
#    include "itkPixelTraits.h"
#    include "itkCommand.h"
#    include "itkFixedArray.h"
#    include "itkSmoothingRecursiveYvvGaussianImageFilter.h"
#    include "itkOpenCLUtil.h"
#    include "itkGPUImageToImageFilter.h"

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
  /** Standard class typedefs. */
  typedef GPUSmoothingRecursiveYvvGaussianImageFilter Self;
  // typedef SmoothingRecursiveYvvGaussianImageFilter<TInputImage,TOutputImage>
  //    CPUSuperclass;
  typedef GPUImageToImageFilter<TInputImage,
                                TOutputImage,
                                SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>>
    Superclass;
  typedef GPUImageToImageFilter<TInputImage,
                                TOutputImage,
                                SmoothingRecursiveYvvGaussianImageFilter<TInputImage, TOutputImage>>
                                   GPUSuperclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Pixel Type of the input image */
  typedef TInputImage                     InputImageType;
  typedef TOutputImage                    OutputImageType;
  typedef typename TInputImage::PixelType PixelType;
#    ifdef WITH_DOUBLE
  typedef typename NumericTraits<PixelType>::RealType       RealType;
  typedef typename NumericTraits<PixelType>::ScalarRealType ScalarRealType;
#    else
  typedef typename NumericTraits<PixelType>::FloatType RealType;
  typedef typename NumericTraits<PixelType>::FloatType ScalarRealType;
#    endif

  typedef typename itk::GPUTraits<TInputImage>::Type  GPUInputImage;
  typedef typename itk::GPUTraits<TOutputImage>::Type GPUOutputImage;

  /** Runtime information support. */
  itkTypeMacro(GPUSmoothingRecursiveYvvGaussianImageFilter, GPUImageToImageFilter);

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Define the type for the sigma array */
  typedef FixedArray<ScalarRealType, itkGetStaticConstMacro(ImageDimension)> SigmaArrayType;

  /** Define the image type for internal computations
   RealType is usually 'double' in NumericTraits.
   Here we prefer float in order to save memory.  */

  typedef typename NumericTraits<PixelType>::FloatType                       InternalRealType;
  typedef GPUImage<InternalRealType, itkGetStaticConstMacro(ImageDimension)> RealImageType;

  /**  Pointer to the Output Image */
  typedef typename OutputImageType::Pointer OutputImagePointer;

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

#    ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  /** End concept checking */
#    endif
  /** Get OpenCL Kernel source as a string, creates a GetOpenCLSource method */
  itkGetOpenCLSourceFromKernelMacro(GPUSmoothingRecursiveYvvGaussianImageFilterKernel);
  void
  SetInput(const TInputImage * input);
  using Superclass::SetInput;

protected:
  GPUSmoothingRecursiveYvvGaussianImageFilter();
  virtual ~GPUSmoothingRecursiveYvvGaussianImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** Generate Data */
  void
  GPUGenerateData(void);

  /** GPUSmoothingRecursiveYvvGaussianImageFilter needs all of the input to produce an
   * output. Therefore, GPUSmoothingRecursiveYvvGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void
  GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output);

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

  typedef GPUDataManager::Pointer GPUDataPointer;

  GPUDataPointer m_GPUMMatrixDataManager;
  GPUDataPointer m_GPUBCoefficientsDataManager;
  GPUDataPointer m_GPULocalDataManager;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUSmoothingecursiveYvvGaussianImageFilter);

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
  int            telltale; // TODO: REMOVE
};
} // end namespace itk

#    ifndef ITK_MANUAL_INSTANTIATION
#      include "itkGPUSmoothingRecursiveYvvGaussianImageFilter.hxx"
#    endif

#  endif //_ITK_GPU_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_H_
#endif   // GPU
