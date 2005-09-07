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
#if defined(USE_FFTWF) || defined(USE_FFTWD)
#include "itkFFTComplexConjugateToRealImageFilter.h"
#include "fftw3.h"

namespace itk
{
template <typename TPixel,unsigned int Dimension = 3>
class FFTWComplexConjugateToRealImageFilter:
    public FFTComplexConjugateToRealImageFilter<TPixel,Dimension>
{
//#error Invalid Type Listed for TPixel
};

template <unsigned int Dimension>
class FFTWComplexConjugateToRealImageFilter<float,Dimension>:
    public FFTComplexConjugateToRealImageFilter<float,Dimension>
{
/** TODO:  There should be compile time type checks so that
           if only USE_FFTWF is defined, then only floats are valid.
           and if USE_FFTWD is defined, then only doubles are valid.
*/
public:
  typedef float TPixel;
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
    M_PlanComputed= false;
  }
  virtual ~FFTWComplexConjugateToRealImageFilter(){
    if(M_PlanComputed)
      {
      fftwf_destroy_plan(M_plan);
      }
  }
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  FFTWComplexConjugateToRealImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  bool M_PlanComputed;
  fftwf_plan M_plan;
};

template <unsigned int Dimension>
class FFTWComplexConjugateToRealImageFilter<double,Dimension>:
    public FFTComplexConjugateToRealImageFilter<double,Dimension>
{
/** TODO:  There should be compile time type checks so that
           if only USE_FFTWF is defined, then only floats are valid.
           and if USE_FFTWD is defined, then only doubles are valid.
*/
public:
  typedef double TPixel;
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
    M_PlanComputed= false;
  }
  virtual ~FFTWComplexConjugateToRealImageFilter(){
    if(M_PlanComputed)
      {
      fftw_destroy_plan(M_plan);
      }
  }
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  FFTWComplexConjugateToRealImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  bool M_PlanComputed;
  fftw_plan M_plan;
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTWComplexConjugateToRealImageFilter.txx"
#endif
#endif // defined(USE_FFTWF) || defined(USE_FFTWD)
#endif
