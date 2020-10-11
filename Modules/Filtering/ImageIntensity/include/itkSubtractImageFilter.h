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
#ifndef itkSubtractImageFilter_h
#define itkSubtractImageFilter_h

#include "itkBinaryGeneratorImageFilter.h"
#include "itkArithmeticOpsFunctors.h"

namespace itk
{
/**
 *\class SubtractImageFilter
 * \brief Pixel-wise subtraction of two images.
 *
 * Subtract each pixel from image2 from its corresponding pixel in
 * image1:
 *
   \code
   Output = Input1 - Input2.
   \endcode
 *
 * This is done using
 *
   \code
   SetInput1( image1 );
   SetInput2( image2 );
   \endcode
 *
 * This class is templated over the types of the two
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * Additionally, a constant can be subtracted from every pixel in an image using:
 *
   \code
   SetInput1( image1 );
   SetConstant2( constant );
   \endcode
 *
 * \note The result of AddImageFilter with a negative constant is not
 * necessarily the same as SubtractImageFilter. This would be the case when
 * the PixelType defines an operator-() that is not the inverse of operator+()
 *
 * \ingroup IntensityImageFilters MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/SubtractTwoImages,Subtract Two Images}
 * \sphinxexample{Filtering/ImageIntensity/SubtractConstantFromEveryPixel,Subtract Constant From Every Pixel}
 * \endsphinx
 */
template <typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1>
class SubtractImageFilter : public BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SubtractImageFilter);

  /** Standard class type aliases. */
  using Self = SubtractImageFilter;
  using Superclass = BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FunctorType =
    Functor::Sub2<typename TInputImage1::PixelType, typename TInputImage2::PixelType, typename TOutputImage::PixelType>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(SubtractImageFilter, BinaryGeneratorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(Input1Input2OutputAdditiveOperatorsCheck,
                  (Concept::AdditiveOperators<typename TInputImage1::PixelType,
                                              typename TInputImage2::PixelType,
                                              typename TOutputImage::PixelType>));
  // End concept checking
#endif

protected:
  SubtractImageFilter()
  {
#if !defined(ITK_WRAPPING_PARSER)
    Superclass::SetFunctor(FunctorType());
#endif
  }

  ~SubtractImageFilter() override = default;
};
} // end namespace itk

#endif
