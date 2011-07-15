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
#ifndef __itkFFTRealToComplexConjugateImageFilter_h
#define __itkFFTRealToComplexConjugateImageFilter_h

#include "itkImageToImageFilter.h"
#include <complex>

namespace itk
{
/** \class FFTRealToComplexConjugateImageFilter
 *
 * \brief Base class for "Forward" Fast Fourier Transform.
 *
 * This is a base class for the "forward" or "direct" discrete Fourier
 * Transform.  This is an abstract base class: the actual implementation is
 * provided by the best child available on the the system when the object is
 * created via the object factory system.
 *
 * This class transforms a real input image into its complex Fourier Transform.
 * The transform of a real input image has complex conjugate symmetry.  That is,
 * values in the second half of the transform are the complex conjugates of
 * values in the first half.  Some implementations, e.g. FFTW, may take
 * advantage of this property and reduce the size of the output in one direction
 * to N/2+1, where N is the size of the input.  If this occurs, FullMatrix()
 * returns 'false'.
 *
 * \ingroup FourierTransform
 *
 * \sa FFTComplexConjugateToRealImageFilter, FFTComplexToComplexImageFilter
 * \ingroup ITKFFT
 */
template< class TInputImage, class TOutputImage=Image< std::complex<typename TInputImage::PixelType>, TInputImage::ImageDimension> >
class ITK_EXPORT FFTRealToComplexConjugateImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef TInputImage                          InputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::PixelType  OutputPixelType;

  typedef FFTRealToComplexConjugateImageFilter                    Self;
  typedef ImageToImageFilter< InputImageType, OutputImageType >   Superclass;
  typedef SmartPointer< Self >                                    Pointer;
  typedef SmartPointer< const Self >                              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTRealToComplexConjugateImageFilter, ImageToImageFilter);

  /** Customized object creation methods that support configuration-based
    * selection of FFT implementation.
    *
    * Default implementation is VnlFFT.
    */
  static Pointer New(void);

  /** Image type typedef support. */
  typedef InputImageType                    ImageType;
  typedef typename InputImageType::SizeType ImageSizeType;
protected:
  FFTRealToComplexConjugateImageFilter() {}
  virtual ~FFTRealToComplexConjugateImageFilter(){}

  /** The output may be a different size from the input if complex conjugate
   * symmetry is implicit. */
  virtual void GenerateOutputInformation(); // figure out allocation for output
                                            // image


  /** This class requires the entire input. */
  virtual void GenerateInputRequestedRegion();

  /** This class produces the entire output. */
  virtual void EnlargeOutputRequestedRegion(DataObject *output);

  /** Returns true if the outputs size is the same size as the input, i.e.
   * we do not take advantage of complex conjugate symmetry. */
  virtual bool FullMatrix() = 0; // must be implemented in child

private:
  FFTRealToComplexConjugateImageFilter(const Self &); //purposely not
                                                      // implemented
  void operator=(const Self &);                       //purposely not
                                                      // implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#ifndef __itkVnlFFTRealToComplexConjugateImageFilter_h
#ifndef __itkVnlFFTRealToComplexConjugateImageFilter_hxx
#ifndef __itkFFTWRealToComplexConjugateImageFilter_h
#ifndef __itkFFTWRealToComplexConjugateImageFilter_hxx
#include "itkFFTRealToComplexConjugateImageFilter.hxx"
#endif
#endif
#endif
#endif
#endif

#endif
