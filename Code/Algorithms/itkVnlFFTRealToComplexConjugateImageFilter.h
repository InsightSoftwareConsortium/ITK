/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVnlFFTRealToComplexConjugateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVnlFFTRealToComplexConjugateImageFilter_h
#define __itkVnlFFTRealToComplexConjugateImageFilter_h
#include "itkFFTRealToComplexConjugateImageFilter.h"
#include <complex>
namespace itk
{
/** \class VnlFFTRealToComplexConjugateImageFilter
 *
 * \brief TODO
 */
template< class TPixel, unsigned int VDimension = 3 >
class VnlFFTRealToComplexConjugateImageFilter:
  public FFTRealToComplexConjugateImageFilter< TPixel, VDimension >
{
public:
  /** Standard class typedefs. */
  typedef VnlFFTRealToComplexConjugateImageFilter                    Self;
  typedef FFTRealToComplexConjugateImageFilter< TPixel, VDimension > Superclass;
  typedef SmartPointer< Self >                                       Pointer;
  typedef SmartPointer< const Self >                                 ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlFFTRealToComplexConjugateImageFilter,
               FFTRealToComplexConjugateImageFilter);

  //
  // these should be defined in every FFT filter class
  virtual void GenerateData();  // generates output from input

  virtual bool FullMatrix();

protected:
  VnlFFTRealToComplexConjugateImageFilter() {}
  ~VnlFFTRealToComplexConjugateImageFilter() {}
  ///** Method to check if an array dimension is legal for PFA FFT */
  bool Legaldim(int n);

private:
  inline std::complex< TPixel > myConj(const std::complex< TPixel > & __z)
  {
    return std::complex< TPixel >( __z.real(), -__z.imag() );
  }

  VnlFFTRealToComplexConjugateImageFilter(const Self &); //purposely not
                                                         // implemented
  void operator=(const Self &);                          //purposely not
                                                         // implemented
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVnlFFTRealToComplexConjugateImageFilter.txx"
#endif

#endif
