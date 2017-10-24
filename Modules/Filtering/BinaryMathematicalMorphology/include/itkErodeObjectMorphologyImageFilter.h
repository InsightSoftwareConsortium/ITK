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
#ifndef itkErodeObjectMorphologyImageFilter_h
#define itkErodeObjectMorphologyImageFilter_h

#include "itkObjectMorphologyImageFilter.h"

namespace itk
{
/** \class ErodeObjectMorphologyImageFilter
 * \brief Erosion of an object in an image
 *
 * Erosion of an image using binary morphology.
 * Pixel values matching the object value are considered the
 * "object" and all other pixels are "background". This is useful
 * in processing mask images containing only one object.
 *
 * If the pixel covered by the center of the kernel has the pixel value
 * ObjectValue and the pixel is adjacent to a non-object valued pixel, then
 * the kernel is centered on the object-value pixel and neighboring
 * pixels covered by the kernel are assigned the background value.
 * The structuring element is assumed to be composed of binary values
 * (zero or one).
 *
 * \sa ObjectMorphologyImageFilter, BinaryFunctionErodeImageFilter
 * \sa BinaryErodeImageFilter
 * \ingroup ImageEnhancement MathematicalMorphologyImageFilters
 * \ingroup ITKBinaryMathematicalMorphology
 */
template< typename TInputImage, typename TOutputImage, typename TKernel >
class ITK_TEMPLATE_EXPORT ErodeObjectMorphologyImageFilter:
  public ObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
{
public:
  /** Standard class typedefs. */
  typedef ErodeObjectMorphologyImageFilter                                  Self;
  typedef ObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel > Superclass;
  typedef SmartPointer< Self >                                              Pointer;
  typedef SmartPointer< const Self >                                        ConstPointer;

  /** Standard New method */
  itkNewMacro(Self);

  /** Runtime information support */
  itkTypeMacro(ErodeObjectMorphologyImageFilter, ObjectMorphologyImageFilter);

  /** Declaration of Pixel Type */
  typedef typename Superclass::PixelType PixelType;

  /** Kernel typedef */
  typedef TKernel KernelType;

  /** Kernel (structuring element) iterator */
  typedef typename KernelType::ConstIterator KernelIteratorType;

  typedef NeighborhoodIterator< TOutputImage > OutputNeighborhoodIteratorType;

  /** Default boundary condition type */
  typedef typename Superclass::DefaultBoundaryConditionType
  DefaultBoundaryConditionType;

  /** Set the object's value. Added for API consistency with itkBinaryErode
    filter */
  void SetErodeValue(PixelType objectValue)
  {
    this->SetObjectValue(objectValue);
  }

  /** Set the object's value. Added for API consistency with itkBinaryErode
    filter */
  PixelType GetErodeValue(void)
  {
    return this->GetObjectValue();
  }

  /** Set the value to be assigned to eroded pixels */
  itkSetMacro(BackgroundValue, PixelType);

  /** Get the value to be assigned to eroded pixels */
  itkGetConstMacro(BackgroundValue, PixelType);

  /** Type of the pixels in the Kernel. */
  typedef typename TKernel::PixelType KernelPixelType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( KernelGreaterThanComparableCheck,
                   ( Concept::GreaterThanComparable< KernelPixelType > ) );
  // End concept checking
#endif

protected:
  ErodeObjectMorphologyImageFilter();
  ~ErodeObjectMorphologyImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Apply the kernel to the neighborhood given.
   *
   * All values in neighborhood covered by the kernel will be set to the
   * background value.  */
  void Evaluate(OutputNeighborhoodIteratorType & nit,
                const KernelType & kernel) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ErodeObjectMorphologyImageFilter);

  PixelType m_BackgroundValue;

  // Default boundary condition for erosion filter, defaults to
  // NumericTraits<PixelType>::max()
  DefaultBoundaryConditionType m_ErodeBoundaryCondition;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkErodeObjectMorphologyImageFilter.hxx"
#endif

#endif
