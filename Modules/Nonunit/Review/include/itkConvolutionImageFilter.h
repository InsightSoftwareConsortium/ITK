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
#ifndef __itkConvolutionImageFilter_h
#define __itkConvolutionImageFilter_h

#include "itkImageToImageFilter.h"

#include "itkProgressAccumulator.h"

namespace itk
{
/** \class ConvolutionImageFilter
 * \brief Convolve a given image with an arbitrary image kernel.
 *
 * This filter operates by centering the kernel at each pixel in the
 * image and computing the inner product between pixel values in the
 * image and pixel values in the kernel. The center of the kernel is
 * defined as \f$ \lfloor (2*i+s-1)/2 \rfloor \f$ where \f$i\f$ is the
 * index and \f$s\f$ is the size of the largest possible region of the
 * kernel image. For kernels with odd sizes in all dimensions, this
 * corresponds to the center pixel. If a dimension of the kernel image
 * has an even size, then the center index of the kernel in that
 * dimension will be the largest integral index that is less than the
 * continuous index of the image center.
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
 * http://hdl.handle.net/1926/1323
 * http://www.insight-journal.org/browse/publication/208
 *
 * \author Nicholas J. Tustison
 * \author James C. Gee
 * \ingroup ITK-Review
 *
 * \wiki
 * \wikiexample{ImageProcessing/ConvolutionImageFilter,Convolve an image with a kernel}
 * \endwiki
 */
template< class TInputImage, class TOutputImage = TInputImage >
class ITK_EXPORT ConvolutionImageFilter :
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  typedef ConvolutionImageFilter                          Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information ( and related methods ) */
  itkTypeMacro(ConvolutionImageFilter, ImageToImageFilter);

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  typedef TInputImage                          InputImageType;
  typedef TOutputImage                         OutputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef typename OutputImageType::PixelType  OutputPixelType;
  typedef typename InputImageType::SizeType    InputSizeType;
  typedef typename OutputImageType::SizeType   OutputSizeType;
  typedef typename InputImageType::RegionType  InputRegionType;
  typedef typename OutputImageType::RegionType OutputRegionType;

  itkSetInputMacro(ImageKernel, InputImageType, 1);
  itkGetInputMacro(ImageKernel, InputImageType, 1);

  /** Normalize the output image by the sum of the kernel
   * components. Defaults to off. */
  itkSetMacro(Normalize, bool);
  itkGetConstMacro(Normalize, bool);
  itkBooleanMacro(Normalize);

  /** ConvolutionImageFilter needs a smaller 2nd input (the image kernel)
   * requested region than output requested region.  As such, this filter
   * needs to provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion()  */
  virtual void GenerateInputRequestedRegion();

protected:
  ConvolutionImageFilter();
  ~ConvolutionImageFilter();

  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateData();

  /** The kernel needs padding if any of the sizes of its dimensions is
   * even. This method checks for this condition. */
  bool GetKernelNeedsPadding() const;

  /** Calculates the padding width needed to make each dimension odd. */
  InputSizeType GetKernelPadSize() const;

private:
  ConvolutionImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented

  template< class TKernelImage >
  void ComputeConvolution( const TKernelImage *kernelImage,
                           ProgressAccumulator *progress );

  bool m_Normalize;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConvolutionImageFilter.txx"
#endif

#endif
