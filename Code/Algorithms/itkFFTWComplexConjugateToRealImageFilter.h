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

//
// FFTWCommon defines proxy classes based on data types
#include "itkFFTWCommon.h"


namespace itk
{

template <typename TPixel,unsigned int Dimension>
class FFTWComplexConjugateToRealImageFilter :
    public FFTComplexConjugateToRealImageFilter<TPixel,Dimension>
{
public:
  typedef FFTWComplexConjugateToRealImageFilter Self;
  typedef FFTComplexConjugateToRealImageFilter<TPixel,Dimension> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> constPointer;
  //
  // the proxy type is a wrapper for the fftw API
  // since the proxy is only defined over double and float,
  // trying to use any other pixel type is inoperative, as
  // is trying to use double if only the float FFTW version is 
  // configured in, or float if only double is configured.
  //
  typedef typename fftw::Proxy<TPixel> FFTWProxyType;

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
  FFTWComplexConjugateToRealImageFilter() : m_PlanComputed(false),
                                            m_LastImageSize(0),
                                            m_InputBuffer(0),
                                            m_OutputBuffer(0)
  {
  }
  virtual ~FFTWComplexConjugateToRealImageFilter(){
    if(m_PlanComputed)
      {
      fftwf_destroy_plan(m_Plan);
      delete [] m_InputBuffer;
      delete [] m_OutputBuffer;
      }
  }
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  FFTWComplexConjugateToRealImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  bool m_PlanComputed;
  typename FFTWProxyType::PlanType m_Plan;
  unsigned int m_LastImageSize;
  // local storage needed to keep fftw from scribbling on
  typename FFTWProxyType::ComplexType *m_InputBuffer;
  TPixel *m_OutputBuffer;
};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTWComplexConjugateToRealImageFilter.txx"
#endif

#endif //__itkFFTWComplexConjugateToRealImageFilter_h
