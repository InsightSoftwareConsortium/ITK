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
#ifndef itkGrayscaleFunctionDilateImageFilter_h
#define itkGrayscaleFunctionDilateImageFilter_h

#include "itkMorphologyImageFilter.h"

namespace itk
{
/** \class GrayscaleFunctionDilateImageFilter
 * \brief Grayscale function dilation of an image.
 *
 * Dilate an image using functional grayscale morphology. Function
 * dilation takes the maximum of all the pixels identified by the
 * structuring element plus the structuring element value.
 *
 * The structuring element can be composed of arbitrary nonnegative
 * values (not restricted to zero or one). Element values greater than
 * zero indicate pixels that will be considered during the dilation.
 * The function dilation operation is defined as the maxixum over the
 * element of the image value PLUS the structuring element value.
 *
 * For the each input image pixel,
 *   - NeighborhoodIterator gives neighbors of the pixel.
 *   - Evaluate() member function returns the maximum value among
 *     the image neighbors plus the kernel value where the kernel has
 *     elements > 0.
 *   - Replace the original value with the max value.
 *
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT GrayscaleFunctionDilateImageFilter
  : public MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GrayscaleFunctionDilateImageFilter);

  /** Standard class type aliases. */
  using Self = GrayscaleFunctionDilateImageFilter;
  using Superclass = MorphologyImageFilter<TInputImage, TOutputImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GrayscaleFunctionDilateImageFilter, MorphologyImageFilter);

  /** Declaration of pixel type. */
  using PixelType = typename Superclass::PixelType;

  /** Kernel (structuring element) iterator. */
  using KernelIteratorType = typename Superclass::KernelIteratorType;

  /** Neighborhood iterator type. */
  using NeighborhoodIteratorType = typename Superclass::NeighborhoodIteratorType;

  /** Kernel type alias. */
  using KernelType = typename Superclass::KernelType;

  /** Default boundary condition type */
  using DefaultBoundaryConditionType = typename Superclass::DefaultBoundaryConditionType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int KernelDimension = TKernel::NeighborhoodDimension;

  /** Type of the pixels in the Kernel. */
  using KernelPixelType = typename TKernel::PixelType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck1, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(SameDimensionCheck2, (Concept::SameDimension<InputImageDimension, KernelDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(KernelConvertibleToInputCheck, (Concept::Convertible<KernelPixelType, PixelType>));
  itkConceptMacro(InputAdditiveOperatorsCheck, (Concept::AdditiveOperators<PixelType>));
  itkConceptMacro(InputGreaterThanComparableCheck, (Concept::GreaterThanComparable<PixelType>));
  itkConceptMacro(KernelGreaterThanComparableCheck, (Concept::GreaterThanComparable<KernelPixelType>));
  // End concept checking
#endif

protected:
  GrayscaleFunctionDilateImageFilter();
  ~GrayscaleFunctionDilateImageFilter() override = default;

  /** Evaluate image neighborhood with kernel to find the new value
   * for the center pixel value
   *
   * It will return the maximum value of the image pixels plus the
   * structuring element values whose corresponding element in the
   * structuring element is positive. This version of Evaluate is used
   * for non-boundary pixels. */
  PixelType
  Evaluate(const NeighborhoodIteratorType & nit,
           const KernelIteratorType         kernelBegin,
           const KernelIteratorType         kernelEnd) override;

private:
  // Default boundary condition for dilation filter, defaults to
  // NumericTraits<PixelType>::NonpositiveMin()
  DefaultBoundaryConditionType m_DilateBoundaryCondition;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGrayscaleFunctionDilateImageFilter.hxx"
#endif

#endif
