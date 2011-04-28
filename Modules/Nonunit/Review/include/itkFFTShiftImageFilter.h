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
#ifndef __itkFFTShiftImageFilter_h
#define __itkFFTShiftImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class FFTShiftImageFilter
 * \brief Shift the zero-frequency components to center of the image
 *
 * The fourier transform produce an image where the zero frequency components are in the corner
 * of the image, making it difficult to understand. This filter shift the component to the center
 * of the image.
 * Note that with images with odd size, applying this filter twice will not produce the same image
 * than the original one without using SetInverse(true) on one (and only one) of the two filters.
 *
 * http://hdl.handle.net/1926/321
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa FFTRealToComplexConjugateImageFilter, FFTComplexConjugateToRealImageFilter, Log10ImageFilter, RescaleIntensityImageFilter
 *
 * \ingroup FourierTransform
 * \ingroup ITK-Review
 */
template< class TInputImage, class TOutputImage >
class ITK_EXPORT FFTShiftImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef FFTShiftImageFilter                             Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                            InputImageType;
  typedef TOutputImage                           OutputImageType;
  typedef typename InputImageType::Pointer       InputImagePointer;
  typedef typename InputImageType::ConstPointer  InputImageConstPointer;
  typedef typename InputImageType::RegionType    InputImageRegionType;
  typedef typename InputImageType::PixelType     InputImagePixelType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;
  typedef typename OutputImageType::IndexType    IndexType;
  typedef typename OutputImageType::SizeType     SizeType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(FFTShiftImageFilter,
               ImageToImageFilter);

  /**
   * Set/Get whether the filter must perform an inverse transform or not.
   * This option has no effect if none of the size of the input image is odd,
   * but is required to be able to restore the original image if at least one
   * of the size is odd.
   */
  itkSetMacro(Inverse, bool);
  itkGetConstReferenceMacro(Inverse, bool);
  itkBooleanMacro(Inverse);
protected:
  FFTShiftImageFilter();
  ~FFTShiftImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** FFTShiftImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const OutputImageRegionType &
                             outputRegionForThread,
                             int threadId);

private:
  FFTShiftImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  bool m_Inverse;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTShiftImageFilter.txx"
#endif

#endif
