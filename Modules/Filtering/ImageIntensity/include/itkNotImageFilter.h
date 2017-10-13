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
#ifndef itkNotImageFilter_h
#define itkNotImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkLogicOpsFunctors.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class NotImageFilter
 * \brief Implements the NOT logical operator pixel-wise on an image.
 *
 * This class is templated over the type of an
 * input image and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * Since the logical NOT operation operates only on boolean types,
 * the input type must be implicitly convertible to bool, which is
 * only defined in C++ for integer types, the images passed to this
 * filter must comply with the requirement of using integer pixel type.
 *
 * The total operation over one pixel will be
 *
 * \code
 *   if( !A )
 *     {
 *     return this->m_ForegroundValue;
 *     }
 *   return this->m_BackgroundValue;
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

  /** Set/Get the value used to mark the false pixels of the result of
    * the operator. Defaults to 0 */
  void SetBackgroundValue(const typename TOutputImage::PixelType & backgroundValue)
  {
    if ( Math::NotExactlyEquals(this->GetBackgroundValue(), backgroundValue) )
      {
      this->Modified();
      this->GetFunctor().SetBackgroundValue(backgroundValue);
      }
  }
  typename TOutputImage::PixelType GetBackgroundValue() const
  {
    return this->GetFunctor().GetBackgroundValue();
  }


  /** Set/Get the value used to mark the false pixels of the result of
    * the operator. Defaults to 1 */
  void SetForegroundValue(const typename TOutputImage::PixelType & foregroundValue)
  {
    std::cout << "this->GetForegroundValue(): " << this->GetForegroundValue()
              <<  "  foregroundValue: " <<  foregroundValue << std::endl;
    if ( Math::NotExactlyEquals(this->GetForegroundValue(), foregroundValue) )
      {
      this->Modified();
      this->GetFunctor().SetForegroundValue(foregroundValue);
      }
  }
  typename TOutputImage::PixelType GetForegroundValue() const
  {
    return this->GetFunctor().GetForegroundValue();
  }


#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType,
                                           bool > ) );
  itkConceptMacro( OutputConvertibleToOutputCheck,
                   ( Concept::Convertible< bool,
                                           typename TOutputImage::PixelType > ) );
  itkConceptMacro( InputNotOperatorCheck,
                   ( Concept::NotOperator< typename TInputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  NotImageFilter()
  {
    this->GetFunctor().SetForegroundValue(true);
    this->GetFunctor().SetBackgroundValue(false);
  }
  virtual ~NotImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NotImageFilter);
};
} // end namespace itk

#endif
