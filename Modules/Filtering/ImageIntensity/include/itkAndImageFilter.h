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
#ifndef itkAndImageFilter_h
#define itkAndImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"
#include "itkBitwiseOpsFunctors.h"
#include "itkNumericTraits.h"

namespace itk
{

/**
 *\class AndImageFilter
 * \brief Implements the AND bitwise operator pixel-wise between two images.
 *
 * This class is templated over the types of the two
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * Since the bitwise AND operation is only defined in C++ for integer
 * types, the images passed to this filter must comply with the requirement
 * of using integer pixel type.
 *
 * The total operation over one pixel will be
   \code
    output_pixel = static_cast<OutputPixelType>( input1_pixel & input2_pixel )
   \endcode
 * Where "&" is the bitwise AND operator in C++.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/BinaryANDTwoImages,Binary AND Two Images}
 * \endsphinx
 */
template <typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1>
class AndImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AndImageFilter);

  /** Standard class type aliases. */
  using Self = AndImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using FunctorType =
    Functor::AND<typename TInputImage1::PixelType, typename TInputImage2::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AndImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1Input2OutputBitwiseOperatorsCheck,
                  (Concept::BitwiseOperators<typename TInputImage1::PixelType,
                                             typename TInputImage2::PixelType,
                                             typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  AndImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }
  ~AndImageFilter() override = default;
};
} // end namespace itk

#endif
