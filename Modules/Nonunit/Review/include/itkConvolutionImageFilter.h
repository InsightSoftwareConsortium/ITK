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

namespace itk
{
/** \class ConvolutionImageFilter
 * \brief Convolve a given image with an arbitrary image kernel
 *
 * http://hdl.handle.net/1926/1323
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
class ITK_EXPORT ConvolutionImageFilter:
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
  typedef typename OutputImageType::RegionType OutputRegionType;

  itkSetInputMacro(ImageKernel, InputImageType, 1);
  itkGetInputMacro(ImageKernel, InputImageType, 1);

  /**
   * Normalize the output image by the sum of the kernel components
   */
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
  /** de/constructor */
  ConvolutionImageFilter();
  ~ConvolutionImageFilter();

  void PrintSelf(std::ostream & os, Indent indent) const;

  void ThreadedGenerateData(const OutputRegionType & outputRegionForThread, int threadId);

private:
  ConvolutionImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented

private:
  bool m_Normalize;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConvolutionImageFilter.txx"
#endif

#endif
