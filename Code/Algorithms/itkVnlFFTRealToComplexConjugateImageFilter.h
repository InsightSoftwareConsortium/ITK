/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVnlFFTRealToComplexConjugateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVnlFFTRealToComplexConjugateImageFilter_h
#define __itkVnlFFTRealToComplexConjugateImageFilter_h
#include "itkFFTRealToComplexConjugateImageFilter.h"

namespace itk
{
template <class TPixel, unsigned int Dimension = 3>
class VnlFFTRealToComplexConjugateImageFilter :
    public FFTRealToComplexConjugateImageFilter<TPixel,Dimension>
{
public:
  /** Standard class typedefs.*/ 
  typedef VnlFFTRealToComplexConjugateImageFilter Self;
  typedef FFTRealToComplexConjugateImageFilter<TPixel,Dimension> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> constPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlFFTRealToComplexConjugateImageFilter,
               FFTRealToComplexConjugateImageFilter);

  //
  // these should be defined in every FFT filter class
  virtual void GenerateData();  // generates output from input

protected:
  VnlFFTRealToComplexConjugateImageFilter() { }
  ~VnlFFTRealToComplexConjugateImageFilter() { }

private:
  VnlFFTRealToComplexConjugateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVnlFFTRealToComplexConjugateImageFilter.txx"
#endif

#endif
