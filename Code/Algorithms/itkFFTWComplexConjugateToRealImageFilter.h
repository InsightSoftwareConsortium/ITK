/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTWComplexConjugateToRealImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFFTWComplexConjugateToRealImageFilter_h
#define __itkFFTWComplexConjugateToRealImageFilter_h
#include "itkFFTComplexConjugateToRealImageFilter.h"
#ifdef USE_FFTW
#include "fftw3.h"

namespace itk
{

template <typename TPixel, unsigned int Dimension = 3>
class FFTWComplexConjugateToRealImageFilter :
    public FFTComplexConjugateToRealImageFilter<TPixel,Dimension>
{
public:

  typedef FFTWComplexConjugateToRealImageFilter Self;
  typedef FFTComplexConjugateToRealImageFilter<TPixel,Dimension> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> constPointer;

  /** Standard class typedefs.*/ 
  typedef typename Superclass::TInputImageType TInputImageType;
  typedef typename Superclass::TOutputImageType TOutputImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWComplexConjugateToRealImageFilter,
               FFTComplexConjugateToRealImageFilter);

  /** Image type typedef support. */
  typedef TInputImageType ImageType;
  typedef typename ImageType::SizeType ImageSizeType;

  //
  // these should be defined in every FFT filter class
  virtual void GenerateData();  // generates output from input
  virtual bool FullMatrix();
protected:
  FFTWComplexConjugateToRealImageFilter()  
  { 
    M_PlanComputed = false; 
    M_PlanComputedf= false; 
    
  }
  virtual ~FFTWComplexConjugateToRealImageFilter(){
    if(M_PlanComputed)
      {
      fftw_destroy_plan(M_plan);
      }
    if(M_PlanComputedf)
      {
      fftwf_destroy_plan(M_planf);
      }
  }
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  FFTWComplexConjugateToRealImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  bool M_PlanComputed;
  bool M_PlanComputedf;
  fftw_plan M_plan;
  fftwf_plan M_planf;
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTWComplexConjugateToRealImageFilter.txx"
#endif
#endif // USE_FFTW
#endif
