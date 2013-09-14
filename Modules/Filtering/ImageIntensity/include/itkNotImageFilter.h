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
#ifndef __itkNotImageFilter_h
#define __itkNotImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
namespace Functor
{
/**
 * \class NOT
 * \brief Unary logical NOT functor
 * \ingroup ITKImageIntensity
 */
template< typename TInput, typename TOutput = TInput >
class NOT
{
public:
  NOT() {}
  ~NOT() {}
  bool operator!=(const NOT &) const
  {
    return false;
  }

  bool operator==(const NOT & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast< TOutput >( !A );
  }
};
}
/** \class NotImageFilter
 * \brief Implements the NOT logical operator pixel-wise on an image.
 *
 * This class is templated over the types of an
 * input image and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * Since the logical NOT operation is operates only on boolean types,
 * the input type must be implicitly convertible to bool, which is
 * only defined in C++ for integer types, the images passed to this
 * filter must comply with the requirement of using integer pixel type.
 *
 * The total operation over one pixel will be
 *
 * \code
 *  output_pixel = static_cast<OutputPixelType>( !input_pixel )
 * \endcode
 *
 * Where "!" is the unary Logical NOT operator in C++.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage, typename TOutputImage >
class NotImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::NOT<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >

{
public:
  /** Standard class typedefs. */
  typedef NotImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::NOT<
      typename TInputImage::PixelType,
      typename TOutputImage::PixelType >
    >                               Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(NotImageFilter,
               UnaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType,
                                           bool > ) );
  itkConceptMacro( OutputConvertibleToOutputCheck,
                   ( Concept::Convertible< bool,
                                           typename TOutputImage::PixelType > ) );
  itkConceptMacro( InputNotOperatorCheck,
                   ( Concept::NotOperator< typename TInputImage::PixelType > ) );
  /** End concept checking */
#endif

protected:
  NotImageFilter() {}
  virtual ~NotImageFilter() {}

private:
  NotImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented
};
} // end namespace itk

#endif
