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
#ifndef itkExpNegativeImageFilter_h
#define itkExpNegativeImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkMath.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class ExpNegative
 * \brief
 * \ingroup ITKImageIntensity
 */
template< typename TInput, typename TOutput >
class ExpNegative
{
public:
  ExpNegative() { m_Factor = 1.0; }
  ~ExpNegative() {}

  bool operator!=(const ExpNegative & other) const
  {
    if ( Math::NotExactlyEquals(m_Factor, other.m_Factor) )
      {
      return true;
      }
    return false;
  }

  bool operator==(const ExpNegative & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast< TOutput >( std::exp( -m_Factor * static_cast< double >( A ) ) );
  }

  /** Sets the value 'K' used in the function evaluation exp(-K.x). */
  void SetFactor(double factor)
  {
    m_Factor = factor;
  }

  double GetFactor() const
  {
    return m_Factor;
  }

private:
  double m_Factor;
};
}
/** \class ExpNegativeImageFilter
 * \brief Computes the function exp(-K.x) for each input pixel.
 *
 * Every output pixel is equal to std::exp(-K.x ). where x is the
 * intensity of the homologous input pixel, and K is a user-provided
 * constant.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 *
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage, typename TOutputImage >
class ExpNegativeImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::ExpNegative<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef ExpNegativeImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::ExpNegative< typename TInputImage::PixelType,
                          typename TOutputImage::PixelType > >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ExpNegativeImageFilter,
               UnaryFunctorImageFilter);

  void SetFactor(double factor)
  {
    if ( factor == this->GetFunctor().GetFactor() )
      {
      return;
      }
    this->GetFunctor().SetFactor(factor);
    this->Modified();
  }
  double GetFactor() const
  {
    return this->GetFunctor().GetFactor();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputConvertibleToDoubleCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType, double > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  ExpNegativeImageFilter() {}
  virtual ~ExpNegativeImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExpNegativeImageFilter);
};
} // end namespace itk

#endif
