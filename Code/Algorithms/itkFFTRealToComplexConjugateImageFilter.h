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
 * \brief TODO
 *
 * \ingroup FourierTransform
 */
template< class TPixel, unsigned int VDimension = 3 >
class ITK_EXPORT FFTRealToComplexConjugateImageFilter:
  public ImageToImageFilter< Image< TPixel, VDimension >,
                             Image< std::complex< TPixel >, VDimension > >
{
public:
  /** Standard class typedefs. */
  typedef Image< TPixel, VDimension >                 TInputImageType;
  typedef Image< std::complex< TPixel >, VDimension > TOutputImageType;

  typedef FFTRealToComplexConjugateImageFilter                    Self;
  typedef ImageToImageFilter< TInputImageType, TOutputImageType > Superclass;
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
  typedef TInputImageType              ImageType;
  typedef typename ImageType::SizeType ImageSizeType;
protected:
  FFTRealToComplexConjugateImageFilter() {}
  virtual ~FFTRealToComplexConjugateImageFilter(){}

  virtual void GenerateOutputInformation(); // figure out allocation for output
                                            // image

  virtual void GenerateInputRequestedRegion();

  virtual void EnlargeOutputRequestedRegion(DataObject *output);

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
#ifndef __itkVnlFFTRealToComplexConjugateImageFilter_txx
#ifndef __itkFFTWRealToComplexConjugateImageFilter_h
#ifndef __itkFFTWRealToComplexConjugateImageFilter_txx
#include "itkFFTRealToComplexConjugateImageFilter.txx"
#endif
#endif
#endif
#endif
#endif

#endif
