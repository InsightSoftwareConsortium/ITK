/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTWComplexToComplexImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFFTWComplexToComplexImageFilter_h
#define __itkFFTWComplexToComplexImageFilter_h

#if defined( USE_FFTWF ) || defined( USE_FFTWD )

#include "itkFFTComplexToComplexImageFilter.h"
#include "fftw3.h"

namespace itk
{
/** \class FFTWComplexToComplexImageFilter
 *  \brief Implements an API to enable the Fourier transform or the inverse
 *  Fourier transform of images with complex valued voxels to be computed using
 *  either FFTW from MIT or the FFTW interface in Intel MKL.
 *
 * \ingroup FourierTransform
 *
 * \author Simon K. Warfield simon.warfield@childrens.harvard.edu
 *
 * \note Attribution Notice. This research work was made possible by
 * Grant Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from
 * the National Center for Research Resources (NCRR), a component of the
 * National Institutes of Health (NIH).  Its contents are solely the
 * responsibility of the authors and do not necessarily represent the
 * official view of NCRR or NIH.
 *
 */

template< typename TPixel, unsigned int NDimension = 3 >
class FFTWComplexToComplexImageFilter:
  public FFTComplexToComplexImageFilter< TPixel, NDimension >
{
//#error Invalid Type Listed for TPixel
};

template< unsigned int NDimension >
class FFTWComplexToComplexImageFilter< float, NDimension > :
  public FFTComplexToComplexImageFilter< float, NDimension >
{
// TODO:  There should be compile time type checks so that
//        if only USE_FFTWF is defined, then only floats are valid.
//        and if USE_FFTWD is defined, then only doubles are valid.
public:
  typedef float                                                TPixel;
  typedef FFTWComplexToComplexImageFilter                      Self;
  typedef FFTComplexToComplexImageFilter< TPixel, NDimension > Superclass;
  typedef SmartPointer< Self >                                 Pointer;
  typedef SmartPointer< const Self >                           ConstPointer;

  /** Standard class typedefs. */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWComplexToComplexImageFilter,
               FFTComplexToComplexImageFilter);

  /** Image type typedef support. */
  typedef InputImageType               ImageType;
  typedef typename ImageType::SizeType ImageSizeType;
protected:

  FFTWComplexToComplexImageFilter()
  {
    this->m_PlanComputed = false;
  }

  virtual ~FFTWComplexToComplexImageFilter()
  {
    if ( m_PlanComputed )
      {
      fftwf_destroy_plan(m_Plan);
      }
  }

  /**
   * these methods should be defined in every FFT filter class
   */
  virtual void GenerateData();  // generates output from input

  virtual bool FullMatrix();

private:
  FFTWComplexToComplexImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                  //purposely not implemented

  bool       m_PlanComputed;
  fftwf_plan m_Plan;
};

template< unsigned int NDimension >
class FFTWComplexToComplexImageFilter< double, NDimension > :
  public FFTComplexToComplexImageFilter< double, NDimension >
{
// TODO:  There should be compile time type checks so that
//        if only USE_FFTWF is defined, then only floats are valid.
//        and if USE_FFTWD is defined, then only doubles are valid.
public:
  typedef double                                               TPixel;
  typedef FFTWComplexToComplexImageFilter                      Self;
  typedef FFTComplexToComplexImageFilter< TPixel, NDimension > Superclass;
  typedef SmartPointer< Self >                                 Pointer;
  typedef SmartPointer< const Self >                           ConstPointer;

  /** Standard class typedefs. */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWComplexToComplexImageFilter,
               FFTComplexToComplexImageFilter);

  /** Image type typedef support. */
  typedef InputImageType               ImageType;
  typedef typename ImageType::SizeType ImageSizeType;
protected:

  FFTWComplexToComplexImageFilter()
  {
    m_PlanComputed = false;
  }

  virtual ~FFTWComplexToComplexImageFilter()
  {
    if ( this->m_PlanComputed )
      {
      fftw_destroy_plan(this->m_Plan);
      }
  }

  //
  // these should be defined in every FFT filter class
  virtual void GenerateData();  // generates output from input

  virtual bool FullMatrix();

private:
  FFTWComplexToComplexImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                  //purposely not implemented

  bool      m_PlanComputed;
  fftw_plan m_Plan;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTWComplexToComplexImageFilter.txx"
#endif

#endif // defined(USE_FFTWF) || defined(USE_FFTWD)

#endif //__itkFFTWComplexToComplexImageFilter_h
