/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTRealToComplexConjugateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
/** /class FFTRealToComplexConjugateImageFilter
 * /brief 
 *
 * \ingroup 
 */
template <class TPixel,unsigned int Dimension = 3>
class ITK_EXPORT FFTRealToComplexConjugateImageFilter :
    public ImageToImageFilter< Image< TPixel , Dimension >,
                               Image< std::complex< TPixel > , Dimension > >
{
public:
  /** Standard class typedefs.*/ 
  typedef Image<TPixel,Dimension> TInputImageType;
  typedef Image< std::complex< TPixel > , Dimension> TOutputImageType;

  typedef FFTRealToComplexConjugateImageFilter Self;
  typedef ImageToImageFilter< TInputImageType, TOutputImageType > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> constPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTRealToComplexConjugateImageFilter, ImageToImageFilter);

  /** Customized object creation methods that support configuration-based 
    * selection of FFT implementation.
    *
    * Default implementation is VnlFFT.
    */
  static Pointer New(void);

  /** Image type typedef support. */
  typedef TInputImageType ImageType;
  typedef typename ImageType::SizeType ImageSizeType;
  virtual void GenerateOutputInformation(); // figure out allocation for output image
  virtual void GenerateInputRequestedRegion(); 
  virtual void EnlargeOutputRequestedRegion(DataObject *output); 
  virtual bool FullMatrix() = 0; // must be implemented in child
protected:
  FFTRealToComplexConjugateImageFilter() {}
  virtual ~FFTRealToComplexConjugateImageFilter(){}

private:
  FFTRealToComplexConjugateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTRealToComplexConjugateImageFilter.txx"
#endif

#endif
