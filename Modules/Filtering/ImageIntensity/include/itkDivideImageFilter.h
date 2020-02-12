/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkDivideImageFilter_h
#define itkDivideImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"
#include "itkArithmeticOpsFunctors.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

namespace itk
{

/**
 *\class DivideImageFilter
 * \brief Pixel-wise division of two images.
 *
 * This class is templated over the types of the two
 * input images and the type of the output image. When the divisor is zero,
 * the division result is set to the maximum number that can be
 * represented  by default to avoid exception. Numeric conversions
 * (castings) are done by the C++ defaults.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/PixelDivisionOfTwoImages,Pixel Division Of Two Images}
 * \endsphinx
 */
template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
class ITK_TEMPLATE_EXPORT DivideImageFilter
  : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DivideImageFilter);

  /**
   * Standard "Self" type alias.
   */
  using Self = DivideImageFilter;

  /**
   * Standard "Superclass" type alias.
   */
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;

  /**
   * Smart pointer type alias support
   */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType =
    Functor::Div<typename TInputImage1::PixelType, typename TInputImage2::PixelType, typename TOutputImage::PixelType>;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DivideImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(IntConvertibleToInput2Check, (Concept::Convertible<int, typename TInputImage2::PixelType>));
  itkConceptMacro(Input1Input2OutputDivisionOperatorsCheck,
                  (Concept::DivisionOperators<typename TInputImage1::PixelType,
                                              typename TInputImage2::PixelType,
                                              typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  DivideImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~DivideImageFilter() override = default;

  void
  VerifyPreconditions() ITKv5_CONST override
  {
    Superclass::VerifyPreconditions();

    const auto * input =
      dynamic_cast<const typename Superclass::DecoratedInput2ImagePixelType *>(this->ProcessObject::GetInput(1));
    if (input != nullptr &&
        itk::Math::AlmostEquals(input->Get(), itk::NumericTraits<typename TInputImage2::PixelType>::ZeroValue()))
    {
      itkGenericExceptionMacro(<< "The constant value used as denominator should not be set to zero");
    }
  }
};

} // end namespace itk

#endif
