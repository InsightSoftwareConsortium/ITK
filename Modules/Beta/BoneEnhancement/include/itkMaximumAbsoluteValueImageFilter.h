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
template<typename TInputPixel1, typename TInputPixel2 = TInputPixel1, typename TOutputPixel = TInputPixel1>
class MaximumAbsoluteValue {
public:
    MaximumAbsoluteValue() {
    }

    ~MaximumAbsoluteValue() {
    }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking TODO
  itkConceptMacro( Input1ConvertableToOutputCheck,
                   ( Concept::Convertible< TInputPixel1, TOutputPixel >) );
  itkConceptMacro( Input2ConvertableToOutputCheck,
                   ( Concept::Convertible< TInputPixel2, TOutputPixel >) );
  itkConceptMacro( Input1GreaterThanInput2Check,
                   ( Concept::GreaterThanComparable< TInputPixel1, TInputPixel2 >) );
  // End concept checking
#endif

    inline TOutputPixel operator()(const TInputPixel1 A, const TInputPixel2 B) {
        return static_cast<TOutputPixel>(Math::abs(A) > Math::abs(B) ? A : B);
    }
}; // end of class
} // namespace functor

/** \class MaximumAbsoluteValueImageFilter
 * \brief Compute the maximum (of the absolute value) between two images.
 *
 * This class takes two images as arguments and takes 
 * 
 * \sa MultiScaleHessianEnhancementImageFilter
 * 
 * \author: Thomas Fitze
 * \ingroup BoneEnhancement
 */
template<typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1>
class MaximumAbsoluteValueImageFilter :
public BinaryFunctorImageFilter<TInputImage1, TInputImage2, TOutputImage,
Functor::MaximumAbsoluteValue<typename TInputImage1::PixelType, typename TInputImage2::PixelType,typename TOutputImage::PixelType> > 
{
public:
  /** Standard Self typedef */
  typedef MaximumAbsoluteValueImageFilter Self;
  typedef BinaryFunctorImageFilter<TInputImage1, TInputImage2, TOutputImage,
          Functor::MaximumAbsoluteValue<typename TInputImage1::PixelType, typename TInputImage2::PixelType,
                  typename TOutputImage::PixelType> > Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MaximumAbsoluteValueImageFilter, BinaryFunctorImageFilter);

protected:
    MaximumAbsoluteValueImageFilter() {
    };

    virtual ~MaximumAbsoluteValueImageFilter() {
    }

private:
    ITK_DISALLOW_COPY_AND_ASSIGN(MaximumAbsoluteValueImageFilter);
}; // end of class
} // end namespace itk

#endif // itkMaximumAbsoluteValueImageFilter_h
