/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTWRealToComplexConjugateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFFTWRealToComplexConjugateImageFilter_h
#define __itkFFTWRealToComplexConjugateImageFilter_h
#if defined(USE_FFTWF) || defined(USE_FFTWD)
#include "itkFFTRealToComplexConjugateImageFilter.h"
#include "fftw3.h"



namespace itk
{
/** TODO:  There should be compile time type checks so that
           if only USE_FFTWF is defined, then only floats are valid.
           and if USE_FFTWD is defined, then only doubles are valid.
*/
/** /class FFTWRealToComplexConjugateImageFilter
 * /brief
 *
 * \ingroup
 */
template <class TPixel, unsigned int Dimension = 3>
class ITK_EXPORT FFTWRealToComplexConjugateImageFilter :
    public FFTRealToComplexConjugateImageFilter<TPixel,Dimension>
{
    //not a real working class.
};

template <unsigned int Dimension>
class ITK_EXPORT FFTWRealToComplexConjugateImageFilter<float,Dimension> :
    public FFTRealToComplexConjugateImageFilter<float,Dimension>
{
public:
  typedef float TPixel;
  typedef FFTWRealToComplexConjugateImageFilter Self;
  typedef FFTRealToComplexConjugateImageFilter<TPixel,Dimension> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> constPointer;

  /** Standard class typedefs.*/
  typedef typename Superclass::TInputImageType TInputImageType;
  typedef typename Superclass::TOutputImageType TOutputImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWRealToComplexConjugateImageFilter,
               FFTRealToComplexConjugateImageFilter);

  //
  // these should be defined in every FFT filter class
  virtual void GenerateData();  // generates output from input
  void PrintSelf(std::ostream& os,Indent indent);

protected:
  FFTWRealToComplexConjugateImageFilter() {
    M_PlanComputed = false;
    M_PlanComputed = false;
  }
  ~FFTWRealToComplexConjugateImageFilter()
  {
    if(M_PlanComputed)
      {
      fftwf_destroy_plan(M_plan);
      }
  }
  void PrintSelf(std::ostream& os, Indent indent) const;
  virtual bool FullMatrix();
private:
  FFTWRealToComplexConjugateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  bool M_PlanComputed;
  fftwf_plan M_plan;
};

template <unsigned int Dimension>
class ITK_EXPORT FFTWRealToComplexConjugateImageFilter<double,Dimension> :
    public FFTRealToComplexConjugateImageFilter<double,Dimension>
{
public:
  typedef double TPixel;
  typedef FFTWRealToComplexConjugateImageFilter Self;
  typedef FFTRealToComplexConjugateImageFilter<TPixel,Dimension> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> constPointer;

  /** Standard class typedefs.*/
  typedef typename Superclass::TInputImageType TInputImageType;
  typedef typename Superclass::TOutputImageType TOutputImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWRealToComplexConjugateImageFilter,
               FFTRealToComplexConjugateImageFilter);

  //
  // these should be defined in every FFT filter class
  virtual void GenerateData();  // generates output from input
  void PrintSelf(std::ostream& os,Indent indent);


protected:
  FFTWRealToComplexConjugateImageFilter() {
    M_PlanComputed = false;
  }
  ~FFTWRealToComplexConjugateImageFilter()
  {
    if(M_PlanComputed)
      {
      fftw_destroy_plan(M_plan);
      }
  }
  void PrintSelf(std::ostream& os, Indent indent) const;
  virtual bool FullMatrix();
private:
  FFTWRealToComplexConjugateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  bool M_PlanComputed;
  fftw_plan M_plan;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTWRealToComplexConjugateImageFilter.txx"
#endif
#endif // defined(USE_FFTWF) || defined(USE_FFTWD)
#endif //__itkFFTWRealToComplexConjugateImageFilter_h
