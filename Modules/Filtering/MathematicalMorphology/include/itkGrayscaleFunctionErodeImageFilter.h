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
#ifndef itkGrayscaleFunctionErodeImageFilter_h
#define itkGrayscaleFunctionErodeImageFilter_h

#include "itkMorphologyImageFilter.h"

namespace itk
{
/** \class GrayscaleFunctionErodeImageFilter
 * \brief Grayscale function erosion of an image.
 *
 * Erode an image using functional grayscale morphology. Function
 * erosion takes the minimum of all the pixels identified by the
 * structuring element minus the structuring element value.
 *
 * The structuring element can be composed of arbitrary nonnegative
 * values (not restricted to zero or one). Element values greater than
 * zero indicate pixels that will be considered during the dilation.
 * The function erosion operation is defined as the minimum over the
 * elements of the image value MINUS the structuring element value.
 *
 * For the each input image pixel,
 *   - NeighborhoodIterator gives neighbors of the pixel.
 *   - Evaluate() member function returns the minimum value among
 *     the image neighbors minus the kernel value where the kernel has
 *     elements > 0.
 *   - Replace the original value with the min value
 *
 * \sa MorphologyImageFilter, GrayscaleFunctionErodeImageFilter, BinaryErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT GrayscaleFunctionErodeImageFilter
  : public MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GrayscaleFunctionErodeImageFilter);

  /** Standard class type aliases. */
  using Self = GrayscaleFunctionErodeImageFilter;
  using Superclass = MorphologyImageFilter<TInputImage, TOutputImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GrayscaleFunctionErodeImageFilter, MorphologyImageFilter);

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
  itkConceptMacro(InputLessThanComparableCheck, (Concept::LessThanComparable<PixelType>));
  itkConceptMacro(KernelGreaterThanComparableCheck, (Concept::GreaterThanComparable<KernelPixelType>));
  // End concept checking
#endif

protected:
  GrayscaleFunctionErodeImageFilter();
  ~GrayscaleFunctionErodeImageFilter() override = default;

  /** Evaluate image neighborhood with kernel to find the new value
   * for the center pixel value
   *
   * It will return the minimum value of the image pixels minus the
   * structuring element values whose corresponding element in the
   * structuring element is positive. This version of Evaluate is used
   * for non-boundary pixels. */
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
#  include "itkGrayscaleFunctionErodeImageFilter.hxx"
#endif

#endif
