/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTRealToComplexConjugateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFFTRealToComplexConjugateImageFilter_h
#define __itkFFTRealToComplexConjugateImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkImage.h>
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
