/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSCSLRealToComplexConjugateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSCSLRealToComplexConjugateImageFilter_h
#define __itkSCSLRealToComplexConjugateImageFilter_h
#include "itkFFTRealToComplexConjugateImageFilter.h"
#ifdef USE_SCSL

#include <complex.h>

#include <scsl_fft.h>

namespace itk
{
template <class TPixel, unsigned int VDimension = 3>
class SCSLRealToComplexConjugateImageFilter :
    public FFTRealToComplexConjugateImageFilter<TPixel,VDimension>
{
public:
  /** Standard class typedefs. */ 
  typedef SCSLRealToComplexConjugateImageFilter                   Self;
  typedef FFTRealToComplexConjugateImageFilter<TPixel,VDimension> Superclass;
  typedef SmartPointer<Self>                                      Pointer;
  typedef SmartPointer<const Self>                                ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SCSLRealToComplexConjugateImageFilter,
               FFTRealToComplexConjugateImageFilter);

  //
  // these should be defined in every FFT filter class
  virtual void GenerateData();  // generates output from input
  void PrintSelf(std::ostream& os,Indent indent);
  virtual bool FullMatrix(); // must be implemented in child


protected:
  SCSLRealToComplexConjugateImageFilter() {}
  ~SCSLRealToComplexConjugateImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SCSLRealToComplexConjugateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSCSLRealToComplexConjugateImageFilter.txx"
#endif
#endif // USE_SCSL
#endif
