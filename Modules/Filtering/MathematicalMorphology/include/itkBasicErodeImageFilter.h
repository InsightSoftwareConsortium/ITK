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
#ifndef itkBasicErodeImageFilter_h
#define itkBasicErodeImageFilter_h

#include "itkMorphologyImageFilter.h"

namespace itk
{
/** \class BasicErodeImageFilter
 * \brief Grayscale erosion of an image.
 *
 * Erode an image using grayscale morphology. Erosion takes the
 * minimum of all the pixels identified by the structuring element.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * For the each input image pixel,
 *   - NeighborhoodIterator gives neighbors of the pixel.
 *   - Evaluate() member function returns the minimum value among
 *     the image neighbors where the kernel has elements > 0
 *   - Replace the original value with the min value
 *
 * \sa MorphologyImageFilter, GrayscaleFunctionErodeImageFilter, BinaryErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT BasicErodeImageFilter : public MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BasicErodeImageFilter);

  /** Standard class type aliases. */
  using Self = BasicErodeImageFilter;
  using Superclass = MorphologyImageFilter<TInputImage, TOutputImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BasicErodeImageFilter, MorphologyImageFilter);

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
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(SameDimensionCheck1, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(SameDimensionCheck2, (Concept::SameDimension<InputImageDimension, KernelDimension>));
  itkConceptMacro(InputLessThanComparableCheck, (Concept::LessThanComparable<PixelType>));
  itkConceptMacro(KernelGreaterThanComparableCheck, (Concept::GreaterThanComparable<KernelPixelType>));
  // End concept checking
#endif

protected:
  BasicErodeImageFilter();
  ~BasicErodeImageFilter() override = default;

  /** Evaluate image neighborhood with kernel to find the new value
   * for the center pixel value.
   *
   * It will return the minimum value of the image pixels whose corresponding
   * element in the structuring element is positive. This version of
   * Evaluate is used for non-boundary pixels. */
  PixelType
  Evaluate(const NeighborhoodIteratorType & nit,
           const KernelIteratorType         kernelBegin,
           const KernelIteratorType         kernelEnd) override;

private:
  // Default boundary condition for erosion filter, defaults to
  // NumericTraits<PixelType>::max()
  DefaultBoundaryConditionType m_ErodeBoundaryCondition;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBasicErodeImageFilter.hxx"
#endif

#endif
