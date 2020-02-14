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
#ifndef itkDilateObjectMorphologyImageFilter_h
#define itkDilateObjectMorphologyImageFilter_h

#include "itkObjectMorphologyImageFilter.h"

namespace itk
{
/**
 *\class DilateObjectMorphologyImageFilter
 * \brief dilation of an object in an image
 *
 * Dilate an image using binary morphology.
 * Pixel values matching the object value are considered the
 * "foreground" and all other pixels are "background". This is useful
 * in processing mask images containing only one object.
 *
 * If a pixel's value is equal to the object
 * value and the pixel is adjacent to a non-object valued pixel, then
 * the kernel is centered on the object-value pixel and neighboring
 * pixels covered by the kernel are assigned the object value.
 * The structuring element is assumed to be composed of binary values
 * (zero or one).
 *
 * \sa ObjectMorphologyImageFilter, ErodeObjectMorphologyImageFilter
 * \sa BinaryDilateImageFilter
 * \ingroup ImageEnhancement MathematicalMorphologyImageFilters
 * \ingroup ITKBinaryMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT DilateObjectMorphologyImageFilter
  : public ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DilateObjectMorphologyImageFilter);

  /** Standard class type aliases. */
  using Self = DilateObjectMorphologyImageFilter;
  using Superclass = ObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method */
  itkNewMacro(Self);

  /** Runtime information support */
  itkTypeMacro(DilateObjectMorphologyImageFilter, ObjectMorphologyImageFilter);

  /** duplicates from base class to avoid compiler warnings */
  using PixelType = typename Superclass::PixelType;

  /** duplicates from base class to avoid compiler warnings */
  using KernelType = TKernel;

  /** duplicates from base class to avoid compiler warnings */
  using KernelIteratorType = typename KernelType::ConstIterator;

  /** duplicates from base class to avoid compiler warnings */
  using OutputNeighborhoodIteratorType = NeighborhoodIterator<TOutputImage>;

  using DefaultBoundaryConditionType = typename Superclass::DefaultBoundaryConditionType;

  /** Type of the pixels in the Kernel. */
  using KernelPixelType = typename TKernel::PixelType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(KernelGreaterThanComparableCheck, (Concept::GreaterThanComparable<KernelPixelType>));
  // End concept checking
#endif

protected:
  DilateObjectMorphologyImageFilter();
  ~DilateObjectMorphologyImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Apply the kernel to the neighborhood given.
   *
   * All values in neighborhood covered by the kernel will be set to the
   * object value.  */
  void
  Evaluate(OutputNeighborhoodIteratorType & nit, const KernelType & kernel) override;

private:
  // Default boundary condition for dilation filter, defaults to
  // NumericTraits<PixelType>::NonpositiveMin()
  DefaultBoundaryConditionType m_DilateBoundaryCondition;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDilateObjectMorphologyImageFilter.hxx"
#endif

#endif
