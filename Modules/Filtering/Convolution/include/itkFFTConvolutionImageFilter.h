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
#ifndef __itkFFTConvolutionImageFilter_h
#define __itkFFTConvolutionImageFilter_h

#include "itkConvolutionImageFilterBase.h"

#include "itkProgressAccumulator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{
/** \class FFTConvolutionImageFilter
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
 * http://hdl.handle.net/10380/3154
 *
 * \ingroup ITKConvolution
 * \sa ConvolutionImageFilter
 *
 */
template< class TInputImage, class TKernelImage = TInputImage, class TOutputImage = TInputImage, class TInternalPrecision=double >
class ITK_EXPORT FFTConvolutionImageFilter :
  public ConvolutionImageFilterBase< TInputImage,
                                     TKernelImage,
                                     TOutputImage,
                                     Image< TInternalPrecision, TInputImage::ImageDimension > >
{
public:
  /** Internal types used by the FFT filters. */
  typedef Image< TInternalPrecision, TInputImage::ImageDimension >
    InternalImageType;
  typedef typename InternalImageType::Pointer
    InternalImagePointerType;
  typedef std::complex< TInternalPrecision >
    InternalComplexType;
  typedef Image< InternalComplexType, TInputImage::ImageDimension >
    InternalComplexImageType;
  typedef typename InternalComplexImageType::Pointer
    InternalComplexImagePointerType;

  /** Standard types. */
  typedef FFTConvolutionImageFilter                       Self;
  typedef ConvolutionImageFilterBase< TInputImage,
                                      TKernelImage,
                                      TOutputImage,
                                      InternalImageType >
                                                          Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information ( and related methods ) */
  itkTypeMacro(FFTConvolutionImageFilter, ConvolutionImageFilterBase);

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  typedef TInputImage                           InputImageType;
  typedef TOutputImage                          OutputImageType;
  typedef TKernelImage                          KernelImageType;
  typedef typename InputImageType::PixelType    InputPixelType;
  typedef typename OutputImageType::PixelType   OutputPixelType;
  typedef typename KernelImageType::PixelType   KernelPixelType;
  typedef typename InputImageType::IndexType    InputIndexType;
  typedef typename OutputImageType::IndexType   OutputIndexType;
  typedef typename KernelImageType::IndexType   KernelIndexType;
  typedef typename InputImageType::SizeType     InputSizeType;
  typedef typename OutputImageType::SizeType    OutputSizeType;
  typedef typename KernelImageType::SizeType    KernelSizeType;
  typedef typename InputSizeType::SizeValueType SizeValueType;
  typedef typename InputImageType::RegionType   InputRegionType;
  typedef typename OutputImageType::RegionType  OutputRegionType;
  typedef typename KernelImageType::RegionType  KernelRegionType;

  /** Typedef to describe the boundary condition. */
  typedef typename Superclass::BoundaryConditionType        BoundaryConditionType;
  typedef typename Superclass::BoundaryConditionPointerType BoundaryConditionPointerType;

protected:
  FFTConvolutionImageFilter();
  ~FFTConvolutionImageFilter() {}

  /** FFTConvolutionImageFilter needs the entire image kernel, which in
   * general is going to be a different size than the output requested
   * region. As such, this filter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()  */
  void GenerateInputRequestedRegion();

  /** This filter uses a minipipeline to compute the output. */
  void GenerateData();

  /** Prepare the input images for operations in the Fourier
   * domain. This includes resizing the input and kernel images,
   * normalizing the kernel if requested, shifting the kernel and
   * taking the Fourier transform of the padded inputs. */
  void PrepareInputs(InternalComplexImagePointerType & preparedInput,
                     InternalComplexImagePointerType & preparedKernel,
                     ProgressAccumulator * progress,
                     float progressWeight);

  /** Produce output from the final Fourier domain image. */
  void ProduceOutput(InternalComplexImageType * paddedOutput,
                     ProgressAccumulator * progress,
                     float progressWeight);

  /** Get the pad size. */
  InputSizeType GetPadSize() const;

  /** Get whether the X dimension has an odd size. */
  bool GetXDimensionIsOdd() const;

private:
  FFTConvolutionImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTConvolutionImageFilter.hxx"
#endif

#endif
