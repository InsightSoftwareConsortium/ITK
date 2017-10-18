/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSigmoidImageFilter_h
#define itkSigmoidImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkMath.h"

namespace itk
{
/** \class SigmoidImageFilter
 * \brief Computes the sigmoid function pixel-wise
 *
 * A linear transformation is applied first on the argument of
 * the sigmoid function. The resulting total transform is given by
 *
 * \f[
 * f(x) = (Max-Min) \cdot \frac{1}{\left(1+e^{- \frac{ x - \beta }{\alpha}}\right)} + Min
 * \f]
 *
 * Every output pixel is equal to f(x). Where x is the intensity of the
 * homologous input pixel, and alpha and beta are user-provided constants.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 *
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{ImageProcessing/SigmoidImageFilter,Pass image pixels through a sigmoid function}
 * \endwiki
 */

namespace Functor
{
template< typename TInput, typename TOutput >
class Sigmoid
{
public:
  Sigmoid()
  {
    m_Alpha = 1.0;
    m_Beta =  0.0;
    m_OutputMinimum = NumericTraits< TOutput >::min();
    m_OutputMaximum = NumericTraits< TOutput >::max();
  }

  ~Sigmoid() {}
  bool operator!=(const Sigmoid & other) const
  {
    if ( Math::NotExactlyEquals(m_Alpha, other.m_Alpha)
         || Math::NotExactlyEquals(m_Beta, other.m_Beta)
         || Math::NotExactlyEquals(m_OutputMaximum, other.m_OutputMaximum)
         || Math::NotExactlyEquals(m_OutputMinimum, other.m_OutputMinimum)  )
      {
      return true;
      }
    return false;
  }

  bool operator==(const Sigmoid & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    const double x = ( static_cast< double >( A ) - m_Beta ) / m_Alpha;
    const double e = 1.0 / ( 1.0 + std::exp(-x) );
    const double v =
      ( m_OutputMaximum - m_OutputMinimum ) * e + m_OutputMinimum;

    return static_cast< TOutput >( v );
  }

  void SetAlpha(double alpha)
  {
    m_Alpha = alpha;
  }

  void SetBeta(double beta)
  {
    m_Beta = beta;
  }

  double GetAlpha() const
  {
    return m_Alpha;
  }

  double GetBeta() const
  {
    return m_Beta;
  }

  void SetOutputMinimum(TOutput min)
  {
    m_OutputMinimum = min;
  }

  void SetOutputMaximum(TOutput max)
  {
    m_OutputMaximum = max;
  }

  TOutput GetOutputMinimum() const
  {
    return m_OutputMinimum;
  }

  TOutput GetOutputMaximum() const
  {
    return m_OutputMaximum;
  }

private:
  double  m_Alpha;
  double  m_Beta;
  TOutput m_OutputMinimum;
  TOutput m_OutputMaximum;
};
}

template< typename TInputImage, typename TOutputImage >
class SigmoidImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::Sigmoid<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef SigmoidImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::Sigmoid< typename TInputImage::PixelType,
                       typename TOutputImage::PixelType > >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename TOutputImage::PixelType OutputPixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Macro that provides the GetNameOfClass() method */
  itkTypeMacro(SigmoidImageFilter, UnaryFunctorImageFilter);

  void SetAlpha(double alpha)
  {
    if ( Math::ExactlyEquals(alpha, this->GetFunctor().GetAlpha()) )
      {
      return;
      }
    this->GetFunctor().SetAlpha(alpha);
    this->Modified();
  }

  double GetAlpha() const
  {
    return this->GetFunctor().GetAlpha();
  }

  void SetBeta(double beta)
  {
    if ( Math::ExactlyEquals(beta, this->GetFunctor().GetBeta()) )
      {
      return;
      }
    this->GetFunctor().SetBeta(beta);
    this->Modified();
  }

  double GetBeta() const
  {
    return this->GetFunctor().GetBeta();
  }

  void SetOutputMinimum(OutputPixelType min)
  {
    if ( Math::ExactlyEquals(min, this->GetFunctor().GetOutputMinimum()) )
      {
      return;
      }
    this->GetFunctor().SetOutputMinimum(min);
    this->Modified();
  }

  OutputPixelType GetOutputMinimum() const
  {
    return this->GetFunctor().GetOutputMinimum();
  }

  void SetOutputMaximum(OutputPixelType max)
  {
    if ( Math::ExactlyEquals(max, this->GetFunctor().GetOutputMaximum()) )
      {
      return;
      }
    this->GetFunctor().SetOutputMaximum(max);
    this->Modified();
  }

  OutputPixelType GetOutputMaximum() const
  {
    return this->GetFunctor().GetOutputMaximum();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputConvertibleToDoubleCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType, double > ) );
  itkConceptMacro( OutputAdditiveOperatorsCheck,
                   ( Concept::AdditiveOperators< OutputPixelType > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, OutputPixelType > ) );
  itkConceptMacro( OutputTimesDoubleCheck,
                   ( Concept::MultiplyOperator< OutputPixelType, double > ) );
  itkConceptMacro( OutputDoubleAdditiveOperatorsCheck,
                   ( Concept::AdditiveOperators< OutputPixelType, OutputPixelType, double > ) );
  // End concept checking
#endif

protected:
  SigmoidImageFilter() {}
  virtual ~SigmoidImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SigmoidImageFilter);
};
} // end namespace itk

#endif
