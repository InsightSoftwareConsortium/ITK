/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSigmoidImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSigmoidImageFilter_h
#define __itkSigmoidImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
/** \class SigmoidImageFilter
 * \brief Computes the sigmoid function pixel-wise
 * 
 * A linear transformation is applied first on the argument of 
 * the sigmoid fuction. The resulting total transfrom is given by
 *
 * \f[
 * f(x) = (Max-Min) \cdot \frac{1}{\left(1+e^{-(\alpha \cdot x + \beta)}\right)} + Min
 * \f]
 *
 * Every output pixel is equal to f(x). Where x is the intensity of the
 * homologous input pixel, and alpha and beta are user-provided constants.
 * 
 * \ingroup IntensityImageFilters  Multithreaded
 *
 */

namespace Function {  
  template< class TInput, class TOutput>
  class Sigmoid
  {
  public:
    Sigmoid() { 
      m_Alpha = 1.0; 
      m_Beta =  0.0; 
      m_OutputMinimum = NumericTraits< TOutput >::min();
      m_OutputMaximum = NumericTraits< TOutput >::max();
    }
    ~Sigmoid() {};
    inline TOutput operator()( const TInput & A )
    {
      const double x = m_Alpha * static_cast<double>(A) + m_Beta;
      const double e = 1.0 / ( 1.0 + exp( - x ) );
      const double v = 
        (m_OutputMaximum - m_OutputMinimum ) * e + m_OutputMinimum;
      return static_cast<TOutput>( v );
    }
  void SetAlpha( double alpha ) {
    m_Alpha = alpha;
    }
  void SetBeta( double beta ) {
    m_Beta = beta;
    }
  double GetAlpha() const {
    return m_Alpha;
    }
  double GetBeta() const {
    return m_Beta;
    }
  void SetOutputMinimum( TOutput min ) {
    m_OutputMinimum = min;
    }
  void SetOutputMaximum( TOutput max ) {
    m_OutputMaximum = max;
    }
  double GetOutputMinimum() const {
    return m_OutputMinimum;
    }
  double GetOutputMaximum() const {
    return m_OutputMaximum;
    }

  private:
    double  m_Alpha;
    double  m_Beta;
    TOutput m_OutputMinimum;
    TOutput m_OutputMaximum;
  }; 
}


template <class TInputImage, class TOutputImage>
class ITK_EXPORT SigmoidImageFilter :
    public
    UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Function::Sigmoid< 
              typename TInputImage::PixelType, 
              typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef SigmoidImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
    Function::Sigmoid< typename TInputImage::PixelType, 
                   typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename TOutputImage::PixelType OutputPixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  void SetAlpha( double alpha )
    {
    if( alpha == this->GetFunctor().GetAlpha() ) 
      {
      return;
      }
    this->GetFunctor().SetAlpha( alpha );
    this->Modified();
    }

  void SetBeta( double beta )
    {
    if( beta == this->GetFunctor().GetBeta() ) 
      {
      return;
      }
    this->GetFunctor().SetBeta( beta );
    this->Modified();
    }

  void SetOutputMinimum( OutputPixelType min )
    {
    if( min == this->GetFunctor().GetOutputMinimum() ) 
      {
      return;
      }
    this->GetFunctor().SetOutputMinimum( min );
    this->Modified();
    }

  void SetOutputMaximum( OutputPixelType max )
    {
    if( max == this->GetFunctor().GetOutputMaximum() ) 
      {
      return;
      }
    this->GetFunctor().SetOutputMaximum( max );
    this->Modified();
    }

protected:
  SigmoidImageFilter() {}
  virtual ~SigmoidImageFilter() {}

private:
  SigmoidImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
