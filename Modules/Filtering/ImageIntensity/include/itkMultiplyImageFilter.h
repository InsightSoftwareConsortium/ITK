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
#ifndef itkMultiplyImageFilter_h
#define itkMultiplyImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"
#include "itkArithmeticOpsFunctors.h"

namespace itk
{
/** \class MultiplyImageFilter
 * \brief Pixel-wise multiplication of two images.
 *
 * This class is templated over the types of the two
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/MultiplyTwoImages,Multiply Two Images}
 * \sphinxexample{Filtering/ImageIntensity/MultiplyImageByScalar,Multiply Image By Scalar}
 * \endsphinx
 */
template <typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1>
class ITK_TEMPLATE_EXPORT MultiplyImageFilter
  : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiplyImageFilter);

  /** Standard class type aliases. */
  using Self = MultiplyImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;

  using FunctorType =
    Functor::Mult<typename TInputImage1::PixelType, typename TInputImage2::PixelType, typename TOutputImage::PixelType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MultiplyImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1Input2OutputMultiplyOperatorCheck,
                  (Concept::MultiplyOperator<typename TInputImage1::PixelType,
                                             typename TInputImage2::PixelType,
                                             typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  MultiplyImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }
  ~MultiplyImageFilter() override = default;
};
} // end namespace itk

#endif
