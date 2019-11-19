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

#ifndef itkMaximumAbsoluteValueImageFilter_h
#define itkMaximumAbsoluteValueImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkMath.h"

namespace itk {
namespace Functor {
/** \class MaximumAbsoluteValue
 * \brief Compute the maximum (of the absolute value) between two images.
 *
 * This class takes two images as arguments and returns the maximum
 * of the absolute value pixel wise. For instance, two pixels with
 * values 2 and -3 would return -3, since the absolute value of -3
 * is larger than 2.
 * 
 * \sa MultiScaleHessianEnhancementImageFilter
 * 
 * \author: Thomas Fitze
 * \ingroup BoneEnhancement
 */
template<typename TInputPixel1, typename TInputPixel2 = TInputPixel1, typename TOutputPixel = TInputPixel1>
class MaximumAbsoluteValue {
public:
  MaximumAbsoluteValue() {
  }

  ~MaximumAbsoluteValue() {
  }

  bool operator!=(const MaximumAbsoluteValue &) const
  {
    return false;
  }

  bool operator==(const MaximumAbsoluteValue & other) const
  {
    return !( *this != other );
  }

  inline TOutputPixel operator()(const TInputPixel1 A, const TInputPixel2 B) {
    return static_cast<TOutputPixel>(Math::abs(A) > Math::abs(B) ? A : B);
  }
}; // end of class
} // namespace functor

/** \class MaximumAbsoluteValueImageFilter
 * \brief Compute the maximum (of the absolute value) between two images.
 *
 * This class takes two images as arguments and returns the maximum
 * of the absolute value pixel wise. For instance, two pixels with
 * values 2 and -3 would return -3, since the absolute value of -3
 * is larger than 2.
 * 
 * \sa MultiScaleHessianEnhancementImageFilter
 * 
 * \author: Thomas Fitze
 * \ingroup BoneEnhancement
 */
template<typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1>
class ITK_TEMPLATE_EXPORT MaximumAbsoluteValueImageFilter
  : public BinaryFunctorImageFilter<TInputImage1, TInputImage2, TOutputImage,
    Functor::MaximumAbsoluteValue<typename TInputImage1::PixelType, typename TInputImage2::PixelType,typename TOutputImage::PixelType> > 
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaximumAbsoluteValueImageFilter);

  /** Standard Self type alias */
  using Self            = MaximumAbsoluteValueImageFilter;
  using Superclass      = BinaryFunctorImageFilter<TInputImage1, TInputImage2, TOutputImage,
                          Functor::MaximumAbsoluteValue< typename TInputImage1::PixelType, typename TInputImage2::PixelType,
                            typename TOutputImage::PixelType > >;
  using Pointer         = SmartPointer<Self>;
  using ConstPointer    = SmartPointer<const Self>;
  using Input1PixelType = typename TInputImage1::PixelType;
  using Input2PixelType = typename TInputImage2::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MaximumAbsoluteValueImageFilter, BinaryFunctorImageFilter);
#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( Input1ConvertableToOutputCheck,
                   ( Concept::Convertible< Input1PixelType, OutputPixelType > ) );
  itkConceptMacro( Input2ConvertableToOutputCheck,
                   ( Concept::Convertible< Input2PixelType, OutputPixelType > ) );
  itkConceptMacro( Input1GreaterThanInput2Check,
                   ( Concept::GreaterThanComparable< Input1PixelType, Input2PixelType > ) );
  // End concept checking
#endif
protected:
    MaximumAbsoluteValueImageFilter() {
    };

    virtual ~MaximumAbsoluteValueImageFilter() {
    }
}; // end of class
} // end namespace itk

#endif // itkMaximumAbsoluteValueImageFilter_h
