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
#ifndef __itkFFTComplexConjugateToRealImageFilter_h
#define __itkFFTComplexConjugateToRealImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include <complex>

namespace itk
{
/** \class FFTComplexConjugateToRealImageFilter
 *
 * \brief Base class for "Inverse" Fast Fourier Transform.
 *
 * This is a base class for the "inverse" or "reverse" discrete Fourier
 * Transform.  This is an abstract base class: the actual implementation is
 * provided by the best child available on the the system when the object is
 * created via the object factory system.
 *
 * This class transforms a complex conjugate symmetric image into its real
 * spatial domain representation.  If the input in not complex conjugate symmetric, the
 * imaginary component is discarded.  The transform of a real input image has
 * complex conjugate symmetry.  That is, values in the second half of the
 * transform are the complex conjugates of values in the first half.  Some
 * implementations, e.g. FFTW, may take advantage of this property and reduce
 * the size of the output in one direction during the forward transform  to
 * N/2+1, where N is the size of the input.  If this occurs, FullMatrix()
 * returns 'false'.  If this was the case, the size of the inverse output image
 * will be larger than the input.
 *
 * \ingroup FourierTransform
 *
 * \sa FFTRealToComplexConjugateImageFilter, FFTComplexConjugateToRealImageFilter
 * \ingroup ITK-FFT
 */
template< class TInputImage, class TOutputImage=Image< typename TInputImage::PixelType::value_type, TInputImage::ImageDimension> >
class ITK_EXPORT FFTComplexConjugateToRealImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >

{
public:
  /** Standard class typedefs. */
  typedef TInputImage                          InputImageType;
  typedef typename InputImageType::PixelType   InputPixelType;
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::PixelType  OutputPixelType;

  typedef FFTComplexConjugateToRealImageFilter                    Self;
  typedef ImageToImageFilter< InputImageType, OutputImageType >   Superclass;
  typedef SmartPointer< Self >                                    Pointer;
  typedef SmartPointer< const Self >                              ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTComplexConjugateToRealImageFilter, ImageToImageFilter);

  /** Customized object creation methods that support configuration-based
  * selection of FFT implementation.
  *
  * Default implementation is VnlFFT.
  */
  static Pointer New(void);

  typedef typename InputImageType::SizeType ImageSizeType;

  /** The output may be a different size from the input if complex conjugate
   * symmetry is implicit. */
  virtual void GenerateOutputInformation(); // figure out allocation for output
                                            // image

  /** This class requires the entire input. */
  virtual void GenerateInputRequestedRegion();

  /** Returns true if the outputs size is the same size as the input, i.e.
   * we do not take advantage of complex conjugate symmetry. */
  virtual bool FullMatrix() = 0; // must be implemented in child

  /** Was the original truncated dimension size odd? */
  void SetActualXDimensionIsOdd(bool isodd)
  {
    m_ActualXDimensionIsOdd = isodd;
  }

  void SetActualXDimensionIsOddOn()
  {
    this->SetActualXDimensionIsOdd(true);
  }

  void SetActualXDimensionIsOddOff()
  {
    this->SetActualXDimensionIsOdd(false);
  }

  bool ActualXDimensionIsOdd()
  {
    return m_ActualXDimensionIsOdd;
  }

protected:
  FFTComplexConjugateToRealImageFilter():m_ActualXDimensionIsOdd(false) {}
  virtual ~FFTComplexConjugateToRealImageFilter(){}
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) );
private:
  bool m_ActualXDimensionIsOdd;
  FFTComplexConjugateToRealImageFilter(const Self &); //purposely not
                                                      // implemented
  void operator=(const Self &);                       //purposely not
                                                      // implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#ifndef __itkVnlFFTComplexConjugateToRealImageFilter_h
#ifndef __itkVnlFFTComplexConjugateToRealImageFilter_txx
#ifndef __itkFFTWComplexConjugateToRealImageFilter_h
#ifndef __itkFFTWComplexConjugateToRealImageFilter_txx
#include "itkFFTComplexConjugateToRealImageFilter.txx"
#endif
#endif
#endif
#endif
#endif

#endif
